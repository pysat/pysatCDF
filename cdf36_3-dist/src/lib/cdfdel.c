/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                        CDF `delete' operations.
*
*  Version 1.4b, 4-Mar-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  20-May-92, J Love     Original version (was part of `cdflib.c').
*   V1.1  21-Aug-92, J Love     CDF V2.3 (shareable/NeXT,zVar).
*   V1.2  30-Nov-93, J Love     CDF V2.4.  Readonly mode, deleting V1 CDFs on
*                               all machines.
*   V1.3   5-Dec-94, J Love     CDF V2.5.
*   V1.3a  6-Jan-95, J Love     More cache-residency.
*   V1.3b 24-Feb-95, J Love     Solaris 2.3 IDL i/f.
*   V1.4   5-Sep-96, J Love     CDF V2.6.
*   V1.4a 21-Feb-97, J Love	Removed RICE.
*   V1.4b  4-Mar-97, J Love	Windows NT for MS Visual C/C++ on an IBM PC.
*   V2.0  08-Apr-04, M Liu      Reset the current variable offset when that
*   V2.1  08-Jan-09, M Liu      Update the attrnum in AEDR when an attribute
*                               is deleted.
*   V2.2  08-May-10, M Liu      Reset firstRecInVVR/lastRecInVVR when a
*                               record(s) gets deleted.
*   V2.3  09-May-14, M Liu      Renumber the sparse records agter delete.
*
******************************************************************************/

#include "cdflib.h"

/******************************************************************************
* Local function prototypes.
******************************************************************************/

static CDFstatus WasteTree_r PROTOARGs((
  struct CDFstruct *CDF, Int32 vxrOffset
));
static CDFstatus DeleteVarRecords PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 firstRec, Int32 lastRec,
  Logical reNumber
));
static CDFstatus DeleteRecords PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 firstRec, Int32 lastRec,
  Int32 *deletedTo
));
static CDFstatus DeleteRecords_r PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 firstVXRoffset,
  Int32 firstRec, Int32 lastRec, Int32 *deletedTo, Logical *total
));
static CDFstatus DeleteFromFront PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 last,
  Int32 vxrOffset, struct VXRstruct *VXR, int entryN, Int32 irSize
));
static CDFstatus DeleteFromMiddle PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 first, Int32 last,
  Int32 vxrOffset, struct VXRstruct *VXR, int entryN, Int32 irSize
));
static CDFstatus DeleteFromEnd PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 first,
  Int32 vxrOffset, struct VXRstruct *VXR, int entryN, Int32 irSize
));
static CDFstatus DeleteVXRentry PROTOARGs((
  struct CDFstruct *CDF, Int32 firstVXRoffset, Int32 delVXRoffset,
  int delEntryN, Logical *total
));
static CDFstatus InsertIndexEntry PROTOARGs((
  struct CDFstruct *CDF, Int32 vxrOffset, int entryN, Logical after,
  Int32 first, Int32 last, Int32 offset
));
static CDFstatus UpdateIndexEntries_r PROTOARGs((
  vFILE *fp, Int32 vxrOffset, Int32 aboveRecord, Int32 recordCount
));
static CDFstatus WriteCVVRorVVR PROTOARGs((
  struct CDFstruct *CDF, Int32 cSize, Int32 stageOffset, Int32 uSize,
  Int32 *newOffset
));

/******************************************************************************
* CDFdel.
******************************************************************************/

STATICforIDL CDFstatus CDFdel (Va, Cur)
struct VAstruct *Va;
struct CurStruct *Cur;
{
CDFstatus tStatus, pStatus = CDF_OK;
switch (Va->item) {
  /****************************************************************************
  * CDF_, delete an open CDF.
  ****************************************************************************/
  case CDF_: {
    struct CDFstruct *CDF;
    SelectCDF (Cur->cdf, CDF)
    if (!WriteAccess(CDF,TRUE,&pStatus)) return pStatus;
    sX (DeleteCDFfiles(CDF), &pStatus);
    if (CDF->uDotFp != NULL) {
      if (!DELETEv(CDF->uDotFp,NULL)) sX (SCRATCH_DELETE_ERROR, &pStatus);
      CDF->uDotFp = NULL;
    }
    if (CDF->stage.fp != NULL) {
      if (!DELETEv(CDF->stage.fp,NULL)) sX (SCRATCH_DELETE_ERROR, &pStatus);
      CDF->stage.fp = NULL;
    }
    if (CDF->compressFp != NULL) {
      if (!DELETEv(CDF->compressFp,NULL)) sX (SCRATCH_DELETE_ERROR, &pStatus);
      CDF->compressFp = NULL;
    }
    FreeCDFid (CDF, FALSE);
    Cur->cdf = NULL;
    break;
  }
  /****************************************************************************
  * zVAR_/rVAR_
  ****************************************************************************/
  case zVAR_:
  case rVAR_: {
    Logical zOp = (Va->item == zVAR_), zVar;
    struct CDFstruct *CDF; struct VDRstruct VDR, VDRt;
    Int32 vOffset, tOffset, rMaxRec = NO_RECORD, aOffset, eOffset;
    Int32 scope, entryN, VDRhead, nVars; int varN;
    /**************************************************************************
    * Get pointer to current CDF and locate the current r/zVariable.
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    if (!CDF->singleFile) return UNSUPPORTED_OPERATION;
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
/*
    if (!sX(LocateCurrentVar(CDF,zOp,&vOffset,&zVar,NULL),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
*/

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) vOffset = CDF->CURzVarOffset;
    else vOffset = CDF->CURrVarOffset;

    /**************************************************************************
    * Read/get GDR field(s).
    **************************************************************************/
    if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		    BOO(zVar,GDR_zVDRHEAD,GDR_rVDRHEAD),&VDRhead,
		    GDR_NULL),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    nVars = BOO(zVar,CDF->NzVars,CDF->NrVars);
    /**************************************************************************
    * Read the VDR.
    **************************************************************************/
    if (!sX(ReadVDR(CDF,CDF->fp,vOffset,zVar,
		    VDR_RECORD,&VDR,NULL,
		    VDR_NULL),&pStatus)){
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Switch to write access.
    **************************************************************************/
    if (!WriteAccess(CDF,FALSE,&pStatus)) return pStatus;
    /**************************************************************************
    * Remove the VDR from the list of r/zVariables and decrement the number
    * of r/zVariables.  If this is a true rVariable, check for the maximum
    * record number written for the rVariables up to the one being deleted.
    **************************************************************************/
    if (VDRhead == vOffset)
      VDRhead = VDR.VDRnext;
    else {
      tOffset = VDRhead;
      while (tOffset != 0) {
	if (!sX(ReadVDR(CDF,CDF->fp,tOffset,zVar,
			VDR_RECORD,&VDRt,NULL,
			VDR_NULL),&pStatus)){
	  AbortAccess (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
	if (!zVar) rMaxRec = MAXIMUM (rMaxRec, VDRt.MaxRec);
	if (VDRt.VDRnext == vOffset) {
	  VDRt.VDRnext = VDR.VDRnext;
	  if (!sX(WriteVDR(CDF,CDF->fp,tOffset,zVar,
			   VDR_RECORD,&VDRt,NULL,
			   VDR_NULL),&pStatus)) {
	    AbortAccess (CDF, UPDATE, noDELETE);
	    return pStatus;
	  }
	  break;
	}
	tOffset = VDRt.VDRnext;
      }
      if (tOffset == 0) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return CORRUPTED_V2_CDF;
      }
    }
    nVars--;
    /**************************************************************************
    * Renumber following VDRs.  If this is a true rVariable, continue checking
    * for the maximum record number written.
    **************************************************************************/
    tOffset = VDR.VDRnext;
    while (tOffset != 0) {
      if (!sX(ReadVDR(CDF,CDF->fp,tOffset,zVar,
		      VDR_RECORD,&VDRt,NULL,
		      VDR_NULL),&pStatus)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (!zVar) rMaxRec = MAXIMUM (rMaxRec, VDRt.MaxRec);
      VDRt.Num--;
      if (!sX(WriteVDR(CDF,CDF->fp,tOffset,zVar,
		       VDR_RECORD,&VDRt,NULL,
		       VDR_NULL),&pStatus)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      tOffset = VDRt.VDRnext;
    }
    /**************************************************************************
    * Write/store the GDR fields that may have changed.
    **************************************************************************/
    if (!zVar) {
      CDF->rMaxRec = rMaxRec;
      if (!sX(WriteGDR(CDF->fp,CDF->GDRoffset,
		       GDR_rMAXREC,&rMaxRec,
		       GDR_NULL),&pStatus)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    if (!sX(WriteGDR(CDF->fp,CDF->GDRoffset,
		     BOO(zVar,GDR_zVDRHEAD,GDR_rVDRHEAD),&VDRhead,
		     BOO(zVar,GDR_NzVARS,GDR_NrVARS),&nVars,
		     GDR_NULL),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (zVar)
      CDF->NzVars = nVars;
    else
      CDF->NrVars = nVars;
    /**************************************************************************
    * Waste VDR.
    **************************************************************************/
    if (!sX(WasteIR(CDF,vOffset,VDR.RecordSize),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Waste CPR/SPR (if one exists).
    **************************************************************************/
    if (VDR.CPRorSPRoffset != NO_OFFSET) {
      Int32 irSize;
      if (!sX(ReadIrSize(CDF->fp,
			 VDR.CPRorSPRoffset,
			 &irSize),&pStatus)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (!sX(WasteIR(CDF,VDR.CPRorSPRoffset,irSize),&pStatus)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    /**************************************************************************
    * Waste VXRs and VVRs/CVVRs.
    **************************************************************************/
    if (!sX(WasteTree_r(CDF,VDR.VXRhead),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Free/adjust variable data structures kept in memory.  Note that the
    * number of z/rVariables has already been decremented.
    **************************************************************************/
    if (zVar) {
      if (CDF->zVars[(int)VDR.Num] != NULL) {
	cdf_FreeMemory (CDF->zVars[(int)VDR.Num], NULL);
      }
      for (varN = (int) VDR.Num; varN < nVars; varN++) {
	 CDF->zVars[varN] = CDF->zVars[varN+1];
	 if (CDF->zVars[varN] != NULL) CDF->zVars[varN]->varN = varN;
      }
      CDF->zVars[varN] = NULL;
    }
    else {
      if (CDF->rVars[(int)VDR.Num] != NULL) {
	cdf_FreeMemory (CDF->rVars[(int)VDR.Num], NULL);
      }
      for (varN = (int) VDR.Num; varN < nVars; varN++) {
	 CDF->rVars[varN] = CDF->rVars[varN+1];
	 if (CDF->rVars[varN] != NULL) CDF->rVars[varN]->varN = varN;
      }
      CDF->rVars[varN] = NULL;
    }
    /**************************************************************************
    * Reset the current r/zVariable number.
    **************************************************************************/
    if (zOp) {
      CDF->CURzVarNum = RESERVED_VARNUM;
      CDF->CURzVarOffset = 0;
    } else {
      CDF->CURrVarNum = RESERVED_VARNUM;
      CDF->CURrVarOffset = 0;
    }
    /**************************************************************************
    * Delete the associated attribute entries and renumber the entries that
    * are associated with the variables that followed the variable that was
    * deleted.
    **************************************************************************/
    if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		    GDR_ADRHEAD,&aOffset,
		    GDR_NULL),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    while (aOffset != 0) {
      /************************************************************************
      * Read the scope of the attribute.
      ************************************************************************/
      if (!sX(ReadADR(CDF->fp,aOffset,
		      ADR_SCOPE,&scope,
		      ADR_NULL),&pStatus)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (VARIABLEscope(scope)) {
	Int32 maxEntry = NO_ENTRY;
	/**********************************************************************
	* Delete associated entry.
	**********************************************************************/
	tStatus = FindEntryByNumber (CDF, aOffset, zVar, VDR.Num, &eOffset);
	switch (tStatus) {
	  case CDF_OK:
	    if (!sX(DeleteEntry(CDF,aOffset,eOffset),&pStatus)) {
	      AbortAccess (CDF, UPDATE, noDELETE);
	      return pStatus;
	    }
	    break;
	  case NO_SUCH_ENTRY:
	    break;
	  default:
	    AbortAccess (CDF, UPDATE, noDELETE);
	    return tStatus;
	}
	/**********************************************************************
	* Renumber entries.  Note that the entry numbers are not necessarily
	* increasing.  (The entries may have been written in any order.)  Also
	* determine the new maximum entry number.
	**********************************************************************/
	if (!sX(ReadADR(CDF->fp,aOffset,
			BOO(zVar,ADR_AzEDRHEAD,ADR_AgrEDRHEAD),&eOffset,
			ADR_NULL),&pStatus)) {
	  AbortAccess (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
	while (eOffset != 0) {
	  if (!sX(ReadAEDR(CDF->fp,eOffset,
			   AEDR_NUM,&entryN,
			   AEDR_NULL),&pStatus)) {
	    AbortAccess (CDF, UPDATE, noDELETE);
	    return pStatus;
	  }
	  if (entryN > VDR.Num) {
	    entryN--;
	    if (!sX(WriteAEDR(CDF,CDF->fp,eOffset,
			      AEDR_NUM,&entryN,
			      AEDR_NULL),&pStatus)) {
	      AbortAccess (CDF, UPDATE, noDELETE);
	      return pStatus;
	    }
	  }
	  maxEntry = MAXIMUM (maxEntry, entryN);
	  if (!sX(ReadAEDR(CDF->fp,eOffset,
			   AEDR_AEDRNEXT,&eOffset,
			   AEDR_NULL),&pStatus)) {
	    AbortAccess (CDF, UPDATE, noDELETE);
	    return pStatus;
	  }
	}
	/**********************************************************************
	* Write new maximum entry number to the ADR.
	**********************************************************************/
	if (!sX(WriteADR(CDF->fp,aOffset,
			 BOO(zVar,ADR_MAXzENTRY,ADR_MAXgrENTRY),&maxEntry,
			 ADR_NULL),&pStatus)) {
	  AbortAccess (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
      }
      /************************************************************************
      * Read offset of next ADR.
      ************************************************************************/
      if (!sX(ReadADR(CDF->fp,aOffset,
		      ADR_ADRNEXT,&aOffset,
		      ADR_NULL),&pStatus)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    /**************************************************************************
    * Reset the current entry offset (in case it was affected).
    **************************************************************************/
    if (!sX(BOO(zOp,SetCURzEntry(CDF,FALSE,CDF->CURzEntryNum),
		    SetCURgrEntry(CDF,FALSE,CDF->CURgrEntryNum)),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    break;
  }
  /****************************************************************************
  * rVAR_RECORDS_/zVAR_RECORDS_/rVAR_RECORDS_RENUMBER_/zVAR_RECORDS__RENUMBER_
  ****************************************************************************/
  case rVAR_RECORDS_:
  case zVAR_RECORDS_:
  case rVAR_RECORDS_RENUMBER_:
  case zVAR_RECORDS_RENUMBER_: {
    Logical zOp = (Va->item == zVAR_RECORDS_ ||
                   Va->item == zVAR_RECORDS_RENUMBER_);
    struct CDFstruct *CDF; struct VarStruct *Var;
    Int32 firstRec = (Int32) va_arg (Va->ap, long);
    Int32 lastRec = (Int32) va_arg (Va->ap, long);
    Logical reNumber;
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    if (!CDF->singleFile) {
      sX (MULTI_FILE_FORMAT, &pStatus);
      break;
    }
    if (firstRec < 0) return BAD_REC_NUM;
    if (lastRec < 0) return BAD_REC_NUM;
    if (lastRec < firstRec) return BAD_REC_NUM;
    if (!WriteAccess(CDF,FALSE,&pStatus)) return pStatus;
    if (!sX(InitCurrentVar(CDF,zOp,&Var),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (Va->item == rVAR_RECORDS_RENUMBER_ ||
        Va->item == zVAR_RECORDS_RENUMBER_) reNumber = TRUE;
    else reNumber = FALSE;
    if (!sX(DeleteVarRecords(CDF,Var,firstRec,lastRec,reNumber),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    break;
  }
  /****************************************************************************
  * ATTR_
  ****************************************************************************/
  case ATTR_: {
    struct CDFstruct *CDF;
    Int32 tOffset;
    struct GDRstruct GDR;
    struct ADRstruct ADR, ADRt;
    struct AEDRstruct AEDRt;
    Int32 eOffset, eNext;
    /**************************************************************************
    * Get pointer to current CDF and locate the current attribute.
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTattrSELECTED(CDF)) return NO_ATTR_SELECTED;
    /**************************************************************************
    * Read the GDR and ADR.
    **************************************************************************/
    if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		    GDR_RECORD,&GDR,
		    GDR_NULL),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (!sX(ReadADR(CDF->fp,CDF->CURattrOffset,
		    ADR_RECORD,&ADR,
		    ADR_NULL),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Switch to write access.
    **************************************************************************/
    if (!WriteAccess(CDF,FALSE,&pStatus)) return pStatus;
    /**************************************************************************
    * Remove the ADR from the list of attributes and decrement the number of
    * attributes.
    **************************************************************************/
    if (GDR.ADRhead == CDF->CURattrOffset)
      GDR.ADRhead = ADR.ADRnext;
    else {
      tOffset = GDR.ADRhead;
      while (tOffset != 0) {
	if (!sX(ReadADR(CDF->fp,tOffset,
			ADR_RECORD,&ADRt,
			ADR_NULL),&pStatus)) {
	  AbortAccess (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
	if (ADRt.ADRnext == CDF->CURattrOffset) break;
	tOffset = ADRt.ADRnext;
      }
      if (tOffset == 0) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return CORRUPTED_V2_CDF;
      }
      ADRt.ADRnext = ADR.ADRnext;
      if (!sX(WriteADR(CDF->fp,tOffset,
		       ADR_RECORD,&ADRt,
		       ADR_NULL),&pStatus)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    GDR.NumAttr--;
    if (!sX(WriteGDR(CDF->fp,CDF->GDRoffset,
		     GDR_RECORD,&GDR,
		     GDR_NULL),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Renumber each of the following attributes.
    **************************************************************************/
    tOffset = ADR.ADRnext;
    while (tOffset != 0) {
      if (!sX(ReadADR(CDF->fp,tOffset,
		      ADR_RECORD,&ADRt,
		      ADR_NULL),&pStatus)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      ADRt.Num--;
      if (!sX(WriteADR(CDF->fp,tOffset,
		       ADR_RECORD,&ADRt,
		       ADR_NULL),&pStatus)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      eOffset = ADRt.AgrEDRhead;
      while (eOffset != 0) {
        if (!sX(ReadAEDR(CDF->fp,eOffset,
                         AEDR_AEDRNEXT,&eNext,
                         AEDR_NULL),&pStatus)) {
          AbortAccess (CDF, UPDATE, noDELETE);
          return pStatus;
        }
        if (!sX(WriteAEDR(CDF,CDF->fp,eOffset,
                          AEDR_ATTRNUM,&ADRt.Num,
                          AEDR_NULL),&pStatus)) {
          AbortAccess (CDF, UPDATE, noDELETE);
          return pStatus;
        }
        eOffset = eNext;
      }
      eOffset = ADRt.AzEDRhead;
      while (eOffset != 0) {
        if (!sX(ReadAEDR(CDF->fp,eOffset,
                         AEDR_AEDRNEXT,&eNext,
                         AEDR_NULL),&pStatus)) {
          AbortAccess (CDF, UPDATE, noDELETE);
          return pStatus;
        }
        if (!sX(WriteAEDR(CDF,CDF->fp,eOffset,
                          AEDR_ATTRNUM,&ADRt.Num,
                          AEDR_NULL),&pStatus)) {
          AbortAccess (CDF, UPDATE, noDELETE);
          return pStatus;
        }
        eOffset = eNext;
      }
      tOffset = ADRt.ADRnext;
    }
    /**************************************************************************
    * Waste the ADR and each of the AgrEDR and AzEDR's for the attribute.
    **************************************************************************/
    if (!sX(WasteIR(CDF,CDF->CURattrOffset,ADR.RecordSize),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    tOffset = ADR.AgrEDRhead;
    while (tOffset != 0) {
      if (!sX(ReadAEDR(CDF->fp,tOffset,
		       AEDR_RECORD,&AEDRt,NULL,
		       AEDR_NULL),&pStatus)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (!sX(WasteIR(CDF,tOffset,AEDRt.RecordSize),&pStatus)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      tOffset = AEDRt.AEDRnext;
    }
    tOffset = ADR.AzEDRhead;
    while (tOffset != 0) {
      if (!sX(ReadAEDR(CDF->fp,tOffset,
		       AEDR_RECORD,&AEDRt,NULL,
		       AEDR_NULL),&pStatus)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (!sX(WasteIR(CDF,tOffset,AEDRt.RecordSize),&pStatus)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      tOffset = AEDRt.AEDRnext;
    }
    /**************************************************************************
    * Reset the current attribute offset and current entry offsets.
    **************************************************************************/
    CDF->CURattrOffset = RESERVED_ATTROFFSET;
    CDF->CURgrEntryOffset = RESERVED_ENTRYOFFSET;
    CDF->CURzEntryOffset = RESERVED_ENTRYOFFSET;
    break;
  }
  /****************************************************************************
  * gENTRY_/zENTRY_/rENTRY_
  ****************************************************************************/
  case gENTRY_:
  case zENTRY_:
  case rENTRY_: {
    int entryType = E3p(Va->item,gENTRY_,rENTRY_,zENTRY_);
    struct CDFstruct *CDF;
    Int32 eOffset;
    /**************************************************************************
    * Get pointer to current CDF, locate the current attribute, and verify
    * that a current entry number has been selected.
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTattrSELECTED(CDF)) return NO_ATTR_SELECTED;
    if (E3(entryType,
	   CDF->CURgrEntryNum,
	   CDF->CURgrEntryNum,
	   CDF->CURzEntryNum) == RESERVED_ENTRYNUM) return NO_ENTRY_SELECTED;
    /**************************************************************************
    * Verify that the operation being performed is legal for the attribute's
    * scope.
    **************************************************************************/
    if (!sX(CheckEntryOp(CDF,entryType),&pStatus)) return pStatus;
    /**************************************************************************
    * Switch to write access.
    **************************************************************************/
    if (!WriteAccess(CDF,FALSE,&pStatus)) return pStatus;
    /**************************************************************************
    * Locate the entry to be deleted.
    **************************************************************************/
    eOffset = E3(entryType,CDF->CURgrEntryOffset,
			   CDF->CURgrEntryOffset,
			   CDF->CURzEntryOffset);
    if (eOffset == RESERVED_ENTRYOFFSET) return NO_SUCH_ENTRY;
    /**************************************************************************
    * Delete the entry.
    **************************************************************************/
    if (!sX(DeleteEntry(CDF,CDF->CURattrOffset,eOffset),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Reset the current entry offset (to indicate the entry no longer exists).
    **************************************************************************/
    switch (entryType) {
      case gENTRYt:
      case rENTRYt:
	CDF->CURgrEntryOffset = RESERVED_ENTRYOFFSET;
	break;
      case zENTRYt:
	CDF->CURzEntryOffset = RESERVED_ENTRYOFFSET;
	break;
    }
    break;
  }
  /****************************************************************************
  * Unknown item, must be the next function.
  ****************************************************************************/
  default: {
    Va->fnc = Va->item;
    break;
  }
}
return pStatus;
}

/******************************************************************************
* WasteTree_r.
******************************************************************************/

static CDFstatus WasteTree_r (CDF, vxrOffset)
struct CDFstruct *CDF;
Int32 vxrOffset;
{
  CDFstatus pStatus = CDF_OK;
  int e; struct VXRstruct VXR; Int32 irType, irSize;
  while (vxrOffset != ZERO_OFFSET) {
    if (!sX(ReadVXR(CDF->fp,vxrOffset,
		    VXR_RECORD,&VXR,
		    VXR_NULL),&pStatus)) return pStatus;
    if (!sX(WasteIR(CDF,vxrOffset,VXR.RecordSize),&pStatus)) return pStatus;
    for (e = 0; e < VXR.NusedEntries; e++) {
       if (!sX(ReadIrType(CDF->fp,
			  VXR.Offset[e],
			  &irType),&pStatus)) return pStatus;
       switch (irType) {
	 case VXR_:
	   if (!sX(WasteTree_r(CDF,VXR.Offset[e]),&pStatus)) return pStatus;
	   break;
	 case VVR_:
	 case CVVR_:
	   if (!sX(ReadIrSize(CDF->fp,
			      VXR.Offset[e],
			      &irSize),&pStatus)) return pStatus;
	   if (!sX(WasteIR(CDF,
			   VXR.Offset[e],
			   irSize),&pStatus)) return pStatus;
	   break;
	 default:
	   return CORRUPTED_V2_CDF;
       }
    }
    vxrOffset = VXR.VXRnext;
  }
  return pStatus;
}

/******************************************************************************
* DeleteVarRecords.
******************************************************************************/

static CDFstatus DeleteVarRecords (CDF, Var, firstRec, lastRec, reNumber)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 firstRec;
Int32 lastRec;
Logical reNumber;
{
  CDFstatus pStatus = CDF_OK; Int32 deletedTo, recNum; Logical found;
  /****************************************************************************
  * Base on the variable type...
  ****************************************************************************/
  switch (Var->vType) {
    /**************************************************************************
    * Standard variable in a single-file CDF.
    **************************************************************************/
    case STANDARD_: {
      Int32 vxrHead, aboveRecord, deleteCount;
      /************************************************************************
      * Verify/adjust the records to be deleted.
      ************************************************************************/
      if (firstRec > Var->maxAllocated) return pStatus;
      lastRec = MINIMUM(lastRec,Var->maxAllocated);
      /************************************************************************
      * Delete the records.
      ************************************************************************/
      for (recNum = firstRec; recNum <= lastRec; recNum = deletedTo + 1) {
	 if (!sX(DeleteRecords(CDF,Var,recNum,
			       lastRec,&deletedTo),&pStatus)) return pStatus;
      }
      deleteCount = lastRec - firstRec + 1;
      /************************************************************************
      * Update the index entries.
      ************************************************************************/
      aboveRecord = firstRec - 1;
      if (!sX(ReadVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
		      VDR_VXRHEAD,&vxrHead,
		      VDR_NULL),&pStatus)) return pStatus;
      if (!sX(UpdateIndexEntries_r(CDF->fp,vxrHead,aboveRecord,deleteCount),
				   &pStatus))
	return pStatus;
      /************************************************************************
      * Update the control information.
      ************************************************************************/
      if (firstRec <= Var->maxWritten) {
	if (lastRec > Var->maxWritten)
	  Var->maxWritten = firstRec - 1;
	else
	  Var->maxWritten -= deleteCount;
      }
      if (firstRec <= Var->maxAllocated) {
	if (lastRec > Var->maxAllocated)
	  Var->maxAllocated = firstRec - 1;
	else
	  Var->maxAllocated -= deleteCount;
      }
      Var->firstRecInVVR = NO_RECORD;
      Var->lastRecInVVR = NO_RECORD;
      Var->offsetOfVVR = NO_OFFSET;
      /************************************************************************
      * Update the maximum record field in the VDR.
      ************************************************************************/
      if (firstRec <= Var->maxRec) {
	if (lastRec > Var->maxRec)
	  Var->maxRec = firstRec - 1;
	else
	  Var->maxRec -= deleteCount;
	if (!sX(WriteVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
			 VDR_MAXREC,&(Var->maxRec),
			 VDR_NULL),&pStatus)) return pStatus;
      }
      break;
    }
    /**************************************************************************
    * Sparse records.
    **************************************************************************/
    case SPARSE_RECORDS_: {
      Int32 aboveRecord, deleteCount;
      Int32 vxrHead; Int32 maxRec;
      /************************************************************************
      * Flush staging area first.  This will automatically clear the staging
      * area as well.
      ************************************************************************/
      if (!sX(FlushStage(CDF,Var),&pStatus)) return pStatus;
      /************************************************************************
      * Verify/adjust the records to be deleted.
      ************************************************************************/
      if (firstRec > Var->maxAllocated) return pStatus;
      lastRec = MINIMUM(lastRec,Var->maxAllocated);
      /************************************************************************
      * Delete the records.
      ************************************************************************/
      for (recNum = firstRec; recNum <= lastRec; recNum = deletedTo + 1) {
	 if (!sX(DeleteRecords(CDF,Var,recNum,
			       lastRec,&deletedTo),&pStatus)) return pStatus;
      }
      if (reNumber) {
        deleteCount = lastRec - firstRec + 1;
        /**********************************************************************
        * Update the index entries.
        **********************************************************************/
        aboveRecord = firstRec - 1;
        if (!sX(ReadVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
                        VDR_VXRHEAD,&vxrHead,
                        VDR_NULL),&pStatus)) return pStatus;
        if (!sX(UpdateIndexEntries_r(CDF->fp,vxrHead,aboveRecord,deleteCount),
                                     &pStatus))
          return pStatus;
        /**********************************************************************
        * Update the control information.
        **********************************************************************/
        if (firstRec <= Var->maxWritten) {
          if (lastRec > Var->maxWritten)
            Var->maxWritten = firstRec - 1;
          else
            Var->maxWritten -= deleteCount;
        }
        if (firstRec <= Var->maxAllocated) {
          if (lastRec > Var->maxAllocated)
            Var->maxAllocated = firstRec - 1;
          else
            Var->maxAllocated -= deleteCount;
        }
        /**********************************************************************
        * Update the maximum record field in the VDR.
        **********************************************************************/
        maxRec = Var->maxRec;
        if (firstRec <= Var->maxRec) {
          if (lastRec > Var->maxRec)
            maxRec = firstRec - 1;
          else
            maxRec -= deleteCount;
          if (!sX(WriteVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
                           VDR_MAXREC,&(maxRec),
                           VDR_NULL),&pStatus)) return pStatus;
        }
        if (INCLUSIVE(firstRec,Var->maxRec,lastRec)) {
          if (!sX(PrevRecord(CDF,Var->VDRoffset,Var->zVar,
                             (Int32)(firstRec - 1),
                             &(Var->maxRec),&found),&pStatus)) return pStatus;
          if (!found) Var->maxRec = NO_RECORD;
          if (!sX(WriteVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
                           VDR_MAXREC,&(Var->maxRec),
                           VDR_NULL),&pStatus)) return pStatus;
        } else
          Var->maxRec = maxRec;
      } else {
        /**********************************************************************
        * Update the control information.
        **********************************************************************/
        if (INCLUSIVE(firstRec,Var->maxWritten,lastRec)) {
          if (!sX(PrevRecord(CDF,Var->VDRoffset,Var->zVar,
                             (Int32)(firstRec - 1),
                             &(Var->maxWritten),&found),&pStatus)) return
                                                                   pStatus;
          if (!found) Var->maxWritten = NO_RECORD;
        }
        if (INCLUSIVE(firstRec,Var->maxAllocated,lastRec)) {
          if (!sX(PrevRecord(CDF,Var->VDRoffset,Var->zVar,
                             (Int32)(firstRec - 1),
                             &(Var->maxAllocated),&found),&pStatus)) return
                                                                     pStatus;
          if (!found) Var->maxAllocated = NO_RECORD;
        }
        /**********************************************************************
        * Update the maximum record field in the VDR.
        **********************************************************************/
        if (INCLUSIVE(firstRec,Var->maxRec,lastRec)) {
          if (!sX(PrevRecord(CDF,Var->VDRoffset,Var->zVar,
                             (Int32)(firstRec - 1),
                             &(Var->maxRec),&found),&pStatus)) return pStatus;
          if (!found) Var->maxRec = NO_RECORD;
          if (!sX(WriteVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
                           VDR_MAXREC,&(Var->maxRec),
                           VDR_NULL),&pStatus)) return pStatus;
        }
      }
      Var->firstRecInVVR = NO_RECORD;
      Var->lastRecInVVR = NO_RECORD;
      Var->offsetOfVVR = NO_OFFSET;
      break;
    }
    /**************************************************************************
    * Compressed records.
    **************************************************************************/
    case COMPRESSED_: {
      Int32 vxrHead, aboveRecord, deleteCount;
      /************************************************************************
      * Flush/clear staging area first.
      ************************************************************************/
      if (!sX(FlushStage(CDF,Var),&pStatus)) return pStatus;
      Var->stage.firstRec = NO_RECORD;
      Var->stage.lastRec = NO_RECORD;
      Var->stage.dotOffset = NO_OFFSET;
      Var->firstRecInVVR = NO_RECORD;
      Var->lastRecInVVR = NO_RECORD;
      Var->offsetOfVVR = NO_OFFSET;
      /************************************************************************
      * Verify/adjust the records to be deleted.
      ************************************************************************/
      if (firstRec > Var->maxRec) return pStatus;
      lastRec = MINIMUM(lastRec,Var->maxRec);
      /************************************************************************
      * Delete the records.
      ************************************************************************/
      for (recNum = firstRec; recNum <= lastRec; recNum = deletedTo + 1) {
	 if (!sX(DeleteRecords(CDF,Var,recNum,
			       lastRec,&deletedTo),&pStatus)) return pStatus;
      }
      deleteCount = lastRec - firstRec + 1;
      /************************************************************************
      * Update the index entries.
      ************************************************************************/
      aboveRecord = firstRec - 1;
      if (!sX(ReadVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
		      VDR_VXRHEAD,&vxrHead,
		      VDR_NULL),&pStatus)) return pStatus;
      if (!sX(UpdateIndexEntries_r(CDF->fp,vxrHead,aboveRecord,deleteCount),
				   &pStatus))
	return pStatus;
      /************************************************************************
      * Update the maximum record field in the VDR.
      ************************************************************************/
      if (firstRec <= Var->maxRec) {
	if (lastRec > Var->maxRec)
	  Var->maxRec = firstRec - 1;
	else
	  Var->maxRec -= deleteCount;
	if (!sX(WriteVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
			 VDR_MAXREC,&(Var->maxRec),
			 VDR_NULL),&pStatus)) return pStatus;
      }
      break;
    }
    /**************************************************************************
    * Sparse compressed records.
    **************************************************************************/
    case SPARSE_COMPRESSED_RECORDS_:
      /************************************************************************
      * Flush/clear staging area first.
      ************************************************************************/
      if (!sX(FlushStage(CDF,Var),&pStatus)) return pStatus;
      Var->stage.firstRec = NO_RECORD;
      Var->stage.lastRec = NO_RECORD;
      Var->stage.dotOffset = NO_OFFSET;
      Var->firstRecInVVR = NO_RECORD;
      Var->lastRecInVVR = NO_RECORD;
      Var->offsetOfVVR = NO_OFFSET;
      /************************************************************************
      * Verify/adjust the records to be deleted.
      ************************************************************************/
      if (firstRec > Var->maxRec) return pStatus;
      lastRec = MINIMUM(lastRec,Var->maxRec);
      /************************************************************************
      * Delete the records.
      ************************************************************************/
      for (recNum = firstRec; recNum <= lastRec; recNum = deletedTo + 1) {
	 if (!sX(DeleteRecords(CDF,Var,recNum,
			       lastRec,&deletedTo),&pStatus)) return pStatus;
      }
      /************************************************************************
      * Update the maximum record field in the VDR.
      ************************************************************************/
      if (INCLUSIVE(firstRec,Var->maxRec,lastRec)) {
	if (!sX(PrevRecord(CDF,Var->VDRoffset,Var->zVar,
			   (Int32)(firstRec - 1),
			   &(Var->maxRec),&found),&pStatus)) return pStatus;
	if (!found) Var->maxRec = NO_RECORD;
	if (!sX(WriteVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
			 VDR_MAXREC,&(Var->maxRec),
			 VDR_NULL),&pStatus)) return pStatus;
      }
      break;
    /**************************************************************************
    * Oops.
    **************************************************************************/
    default:
      return CDF_INTERNAL_ERROR;
  }
  /****************************************************************************
  * Update the VXR tail field in the VDR.
  ****************************************************************************/
  if (!sX(UpdateVXRtailInVDR(CDF,Var),&pStatus)) return pStatus;
  /****************************************************************************
  * Update the maximum record field in the GDR if an rVariable.  This is
  * done in case this rVariable had the maximum record number.
  ****************************************************************************/
  if (!Var->zVar) {
    Int32 maxRec, vdrOffset; Logical zVar = FALSE;
    CDF->rMaxRec = NO_RECORD;
    if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		    GDR_rVDRHEAD,&vdrOffset,
		    GDR_NULL),&pStatus)) return pStatus;
    while (vdrOffset != ZERO_OFFSET) {
      if (!sX(ReadVDR(CDF,CDF->fp,vdrOffset,zVar,
		      VDR_MAXREC,&maxRec,
		      VDR_VDRNEXT,&vdrOffset,
		      VDR_NULL),&pStatus)) return pStatus;
      CDF->rMaxRec = MAXIMUM(CDF->rMaxRec,maxRec);
    }
    if (!sX(WriteGDR(CDF->fp,CDF->GDRoffset,
		     GDR_rMAXREC,&(CDF->rMaxRec),
		     GDR_NULL),&pStatus)) return pStatus;
  }
  return pStatus;
}

/******************************************************************************
* DeleteRecords.
******************************************************************************/

static CDFstatus DeleteRecords (CDF, Var, firstRec, lastRec, deletedTo)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 firstRec;
Int32 lastRec;
Int32 *deletedTo;
{
  CDFstatus pStatus = CDF_OK; Logical total = FALSE; Int32 vxrOffset;
  /****************************************************************************
  * Read offset of the first VXR...
  ****************************************************************************/
  if (!sX(ReadVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
		  VDR_VXRHEAD,&vxrOffset,
		  VDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * ...and start there.
  ****************************************************************************/
  if (!sX(DeleteRecords_r(CDF,Var,vxrOffset,firstRec,
			  lastRec,deletedTo,&total),&pStatus)) return pStatus;
  /****************************************************************************
  * If the top level of VXR(s) was totally deleted, set the VXR head/tail to
  * ZERO_OFFSET.
  ****************************************************************************/
  if (total) {
    Int32 zeroOffset = ZERO_OFFSET;
    if (!sX(WriteVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
		     VDR_VXRHEAD,&zeroOffset,
		     VDR_VXRTAIL,&zeroOffset,
		     VDR_NULL),&pStatus)) return pStatus;
  }
  return pStatus;
}

/******************************************************************************
* DeleteRecords_r.
******************************************************************************/

static CDFstatus DeleteRecords_r (CDF, Var, firstVXRoffset, firstRec, lastRec,
				  deletedTo, total)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 firstVXRoffset;
Int32 firstRec;
Int32 lastRec;
Int32 *deletedTo;
Logical *total;
{
  CDFstatus pStatus = CDF_OK; Int32 vxrOffset = firstVXRoffset;
  struct VXRstruct VXR; int entryN;
  /****************************************************************************
  * While another VXR...
  ****************************************************************************/
  while (vxrOffset != ZERO_OFFSET) {
    /**************************************************************************
    * Read the VXR.
    **************************************************************************/
    if (!sX(ReadVXR(CDF->fp,vxrOffset,
		    VXR_RECORD,&VXR,
		    VXR_NULL),&pStatus)) return pStatus;
    /**************************************************************************
    * For each VXR entry...
    **************************************************************************/
    for (entryN = 0; entryN < VXR.NusedEntries; entryN++) {
       /***********************************************************************
       * If some of the records to be deleted are within this entry...
       ***********************************************************************/
       if (!(lastRec < VXR.First[entryN] || firstRec > VXR.Last[entryN])) {
	 Int32 first = MAXIMUM(firstRec,VXR.First[entryN]);
	 Int32 last = MINIMUM(lastRec,VXR.Last[entryN]);
	 Int32 irType;
	 /*********************************************************************
	 * Determine the type of internal record pointed to by this entry.
	 *********************************************************************/
	 if (!sX(ReadIrType(CDF->fp,
			    VXR.Offset[entryN],
			    &irType),&pStatus)) return pStatus;
	 /*********************************************************************
	 * Based on the type of internal record...
	 *********************************************************************/
	 switch (irType) {
	   /*******************************************************************
	   * Another VXR (in a branch at a lower level)...
	   *******************************************************************/
	   case VXR_: {
	     Logical totalBelow = FALSE;
	     /*****************************************************************
	     * Recursively call this routine...
	     *****************************************************************/
	     if (!sX(DeleteRecords_r(CDF,Var,VXR.Offset[entryN],
				     first,last,deletedTo,
				     &totalBelow),&pStatus)) return pStatus;
	     /*****************************************************************
	     * If the branch of VXRs below has not been totally deleted, update
	     * this entry for the records that remain and return.
	     *****************************************************************/
	     if (!totalBelow) {
	       struct VXRstruct VXRbelow;
	       if (!sX(ReadVXR(CDF->fp,VXR.Offset[entryN],
			       VXR_RECORD,&VXRbelow,
			       VXR_NULL),&pStatus)) return pStatus;
	       VXR.First[entryN] = VXRbelow.First[0];
	       while (VXRbelow.VXRnext != ZERO_OFFSET) {
		 if (!sX(ReadVXR(CDF->fp,VXRbelow.VXRnext,
				 VXR_RECORD,&VXRbelow,
				 VXR_NULL),&pStatus)) return pStatus;
	       }
	       VXR.Last[entryN] = VXRbelow.Last[(int)VXRbelow.NusedEntries-1];
	       if (!sX(WriteVXR(CDF->fp,vxrOffset,
				VXR_RECORD,&VXR,
				VXR_NULL),&pStatus)) return pStatus;
	       return pStatus;
	     }
	     /*****************************************************************
	     * The branch of VXRs below has been totally deleted.  Delete this
	     * entry by moving the following entries up.  If this is the last
	     * entry, indicate to the caller that this branch has been totally
	     * deleted.
	     *****************************************************************/
	     if (!sX(DeleteVXRentry(CDF,firstVXRoffset,
				    vxrOffset,entryN,total),&pStatus)) return
								       pStatus;
	     return pStatus;
	   }
	   /*******************************************************************
	   * A VVR or CVVR...
	   *******************************************************************/
	   case VVR_:
	   case CVVR_: {
	     Int32 irSize;
	     /*****************************************************************
	     * Read the size of the CVVR/VVR.
	     *****************************************************************/
	     if (!sX(ReadIrSize(CDF->fp,
				VXR.Offset[entryN],
				&irSize),&pStatus)) return pStatus;
	     /*****************************************************************
	     * Based on which records in the CVVR/VVR are being deleted...
	     *****************************************************************/
	     if (first > VXR.First[entryN]) {
	       if (last < VXR.Last[entryN]) {
		 if (!sX(DeleteFromMiddle(CDF,Var,first,last,
					  vxrOffset,&VXR,
					  entryN,irSize),&pStatus)) return
								    pStatus;
	       }
	       else {
		 if (!sX(DeleteFromEnd(CDF,Var,first,
				       vxrOffset,&VXR,
				       entryN,irSize),&pStatus)) return
								 pStatus;
	       }
	     }
	     else {
	       if (last < VXR.Last[entryN]) {
		 if (!sX(DeleteFromFront(CDF,Var,last,
					 vxrOffset,&VXR,
					 entryN,irSize),&pStatus)) return
								   pStatus;
	       }
	       else {
		 /*************************************************************
		 * The entire block of records (ie. the entire CVVR/VVR) is
		 * being deleted.
		 *************************************************************/
		 if (!sX(WasteIR(CDF,VXR.Offset[entryN],
				 irSize),&pStatus)) return pStatus;
		 if (!sX(DeleteVXRentry(CDF,firstVXRoffset,
					vxrOffset,
					entryN,total),&pStatus)) return
								 pStatus;
	       }
	     }
	     *deletedTo = last;
	     return pStatus;
	   }
	   /*******************************************************************
	   * Something else - this is bad.
	   *******************************************************************/
	   default:
	     return CORRUPTED_V2_CDF;
	 }
       }
    }
    /**************************************************************************
    * On to the next VXR...
    **************************************************************************/
    vxrOffset = VXR.VXRnext;
  }
  /****************************************************************************
  * The records to be deleted do not exist...
  ****************************************************************************/
  *deletedTo = lastRec;
  return pStatus;
}

/******************************************************************************
* DeleteFromFront.
******************************************************************************/

static CDFstatus DeleteFromFront (CDF, Var, last, vxrOffset, VXR, entryN,
				  irSize)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 last;
Int32 vxrOffset;
struct VXRstruct *VXR;
int entryN;
Int32 irSize;
{
  CDFstatus pStatus = CDF_OK;
  switch (Var->vType) {
    case STANDARD_:
    case SPARSE_RECORDS_: {
      Int32 deleteOffset, nDeleteRecords, nRemainingBytes;
      Int32 nDeleteBytes, nRecords2, offset2, nBytes2, recordSize;
      /************************************************************************
      * The records being deleted are at the beginning of the VVR.  First
      * calculate needed offsets and sizes.
      ************************************************************************/
      deleteOffset = VXR->Offset[entryN] + VVR_BASE_SIZE;
      nDeleteRecords = last - VXR->First[entryN] + 1;
      nDeleteBytes = nDeleteRecords * Var->NphyRecBytes;
      nRecords2 = VXR->Last[entryN] - last;
      nBytes2 = nRecords2 * Var->NphyRecBytes;
      offset2 = VXR->Offset[entryN] + VVR_BASE_SIZE + nDeleteBytes;
      nRemainingBytes = irSize - VVR_BASE_SIZE - nBytes2;
      /************************************************************************
      * Slide the remaining records to the beginning of the VVR.
      ************************************************************************/
      if (!sX(CopyBytes(CDF->fp,offset2,
			CDF_READ_ERROR,
			nBytes2,CDF->fp,
			deleteOffset,
			CDF_WRITE_ERROR),&pStatus)) return pStatus;
      /************************************************************************
      * Update the VXR entry.
      ************************************************************************/
      VXR->First[entryN] = last + 1;
      if (!sX(WriteVXR(CDF->fp,vxrOffset,
		       VXR_RECORD,VXR,
		       VXR_NULL),&pStatus)) return pStatus;
      /************************************************************************
      * If possible, waste the unused bytes that are now at the end of the
      * VVR (and update the record size field).  Otherwise, the record size
      * field of the VVR remains unchanged.
      ************************************************************************/
      if (nRemainingBytes >= UIR_BASE_SIZE) {
	Int32 offset = deleteOffset + nBytes2;
	if (!sX(WasteIR(CDF,offset,
			nRemainingBytes),&pStatus)) return pStatus;
	recordSize = VVR_BASE_SIZE + nBytes2;
	if (!sX(WriteVVR(CDF->fp,VXR->Offset[entryN],
			 VVR_RECORDSIZE,&recordSize,
			 VVR_NULL),&pStatus)) return pStatus;
      }
      break;
    }
    case COMPRESSED_:
    case SPARSE_COMPRESSED_RECORDS_: {
      Int32 nRecords = VXR->Last[entryN] - VXR->First[entryN] + 1;
      Int32 uSize = nRecords * Var->NphyRecBytes;
      Int32 firstRecord2, nRecords2, offset2, nBytes2, cSize, newOffset;
      /************************************************************************
      * Initialize staging area and scratch space.
      ************************************************************************/
      if (Var->stage.areaOffset == NO_OFFSET) {
	Int32 nBytes = Var->blockingFactor * Var->NphyRecBytes;
	if (!sX(InitVarStage(CDF,Var,nBytes),&pStatus)) return pStatus;
      }
      if (!sX(InitScratch(ScratchDirectory(CDF),
			  &(CDF->compressFp),
			  CDF->compressCacheSize),&pStatus)) return pStatus;
      /************************************************************************
      * Bring the CVVR/VVR to the staging area.
      ************************************************************************/
      if (!sX(DecompressToStage(CDF,Var,
				VXR->Offset[entryN],
				uSize),&pStatus)) return pStatus;
      /************************************************************************
      * Waste the CVVR/VVR.
      ************************************************************************/
      if (!sX(WasteIR(CDF,VXR->Offset[entryN],irSize),&pStatus)) return
								 pStatus;
      /************************************************************************
      * Calculate sizes/offsets for the block of remaining records.
      ************************************************************************/
      firstRecord2 = last + 1;
      nRecords2 = VXR->Last[entryN] - last;
      if (nRecords2 <= 0) break;
      offset2 = Var->stage.areaOffset +
		(Var->NphyRecBytes * (firstRecord2 - VXR->First[entryN]));
      nBytes2 = nRecords2 * Var->NphyRecBytes;
      /************************************************************************
      * Compress the block of remaining records...
      ************************************************************************/
      if (!sX(Compress(CDF->stage.fp,offset2,
		       nBytes2,SCRATCH_READ_ERROR,
		       Var->cType,Var->cParms,
		       CDF->compressFp,ZERO_OFFSET,
		       &cSize,SCRATCH_WRITE_ERROR),&pStatus)) return pStatus;
      /************************************************************************
      * ...and allocate/write the CVVR/VVR.
      ************************************************************************/
      if (!sX(WriteCVVRorVVR(CDF,cSize,offset2,
			     nBytes2,&newOffset),&pStatus)) return pStatus;
      /************************************************************************
      * Update the index entry for the block of remaining records.
      ************************************************************************/
      VXR->First[entryN] = firstRecord2;
      VXR->Offset[entryN] = newOffset;
      if (!sX(WriteVXR(CDF->fp,vxrOffset,
		       VXR_RECORD,VXR,
		       VXR_NULL),&pStatus)) return pStatus;
      break;
    }
    default:
      return CDF_INTERNAL_ERROR;
  }
  return pStatus;
}

/******************************************************************************
* DeleteFromMiddle.
******************************************************************************/

static CDFstatus DeleteFromMiddle (CDF, Var, first, last, vxrOffset, VXR,
				   entryN, irSize)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 first;
Int32 last;
Int32 vxrOffset;
struct VXRstruct *VXR;
int entryN;
Int32 irSize;
{
  CDFstatus pStatus = CDF_OK;
  switch (Var->vType) {
    case STANDARD_:
    case SPARSE_RECORDS_: {
      Int32 nRecords1, nBytes1, deleteOffset, nDeleteRecords;
      Int32 nDeleteBytes, firstRecord2, lastRecord2, nRecords2, offset2;
      Int32 nBytes2, nExtraBytes, recordSize, vvrOffset, vvr_ = VVR_;
      Int32 nRemainingBytes;
      /************************************************************************
      * A block of records will remain at the beginning and at the end.  First
      * calculate needed offsets and sizes.  Note that the number of bytes in
      * the second remaining block of records takes into account any extra
      * bytes that may exist at the end of the VVR.
      ************************************************************************/
      nRecords1 = first - VXR->First[entryN];
      nBytes1 = nRecords1 * Var->NphyRecBytes;
      deleteOffset = VXR->Offset[entryN] + VVR_BASE_SIZE + nBytes1;
      nDeleteRecords = last - first + 1;
      nDeleteBytes = nDeleteRecords * Var->NphyRecBytes;
      firstRecord2 = last + 1;
      lastRecord2 = VXR->Last[entryN];
      nRecords2 = lastRecord2 - firstRecord2 + 1;
      offset2 = deleteOffset + nDeleteBytes;
      nBytes2 = nRecords2 * Var->NphyRecBytes;
      nExtraBytes = irSize - VVR_BASE_SIZE - nBytes1 - nDeleteBytes - nBytes2;
      /************************************************************************
      * If the second block of remaining records cannot remain in the same
      * place...
      ************************************************************************/
      if (nDeleteBytes < VVR_BASE_SIZE) {
	/**********************************************************************
	* Move the second block of remaining records to a new VVR.
	**********************************************************************/
	recordSize = VVR_BASE_SIZE + nBytes2;
	if (!sX(AllocateIR(CDF,recordSize,&vvrOffset),&pStatus)) return
								 pStatus;
	if (!sX(WriteVVR(CDF->fp,vvrOffset,
			 VVR_RECORDSIZE,&recordSize,
			 VVR_RECORDTYPE,&vvr_,
			 VVR_NULL),&pStatus)) return pStatus;
	if (!sX(CopyBytes(CDF->fp,offset2,
			  CDF_READ_ERROR,
			  nBytes2,CDF->fp,
			  (Int32)(vvrOffset + VVR_BASE_SIZE),
			  CDF_WRITE_ERROR),&pStatus)) return pStatus;
	/**********************************************************************
	* If the remaining bytes can be changed to a UIR...
	**********************************************************************/
	nRemainingBytes = nDeleteBytes + nBytes2 + nExtraBytes;
	if (nRemainingBytes >= UIR_BASE_SIZE) {
	  recordSize = VVR_BASE_SIZE + nBytes1;
	  if (!sX(WriteVVR(CDF->fp,VXR->Offset[entryN],
			   VVR_RECORDSIZE,&recordSize,
			   VVR_NULL),&pStatus)) return pStatus;
	  if (!sX(WasteIR(CDF,deleteOffset,
			  nRemainingBytes),&pStatus)) return pStatus;
	}
	/**********************************************************************
	* Update the VXR entry for the first block of remaining records.
	**********************************************************************/
	VXR->Last[entryN] = first - 1;
	if (!sX(WriteVXR(CDF->fp,vxrOffset,
			 VXR_RECORD,VXR,
			 VXR_NULL),&pStatus)) return pStatus;
	/**********************************************************************
	* Insert a VXR entry for the new VVR created for the second block of
	* remaining records.
	**********************************************************************/
	if (!sX(InsertIndexEntry(CDF,vxrOffset,
				 entryN,(Logical)TRUE,
				 firstRecord2,lastRecord2,
				 vvrOffset),&pStatus)) return pStatus;
      }
      else {
	/**********************************************************************
	* Update record size field of VVR for the first block of remaining
	* records and create a UIR for the bytes being "deleted" that are not
	* going to be used for the new VVR containing the second block of
	* remaining records.
	**********************************************************************/
	nRemainingBytes = nDeleteBytes - VVR_BASE_SIZE;
	if (nRemainingBytes < UIR_BASE_SIZE)
	  recordSize = VVR_BASE_SIZE + nBytes1 + nRemainingBytes;
	else {
	  recordSize = VVR_BASE_SIZE + nBytes1;
	  if (!sX(WasteIR(CDF,deleteOffset,
			  nRemainingBytes),&pStatus)) return pStatus;
	}
	if (!sX(WriteVVR(CDF->fp,VXR->Offset[entryN],
			 VVR_RECORDSIZE,&recordSize,
			 VVR_NULL),&pStatus)) return pStatus;
	/**********************************************************************
	* Update the VXR entry for the first block of remaining records.
	**********************************************************************/
	VXR->Last[entryN] = first - 1;
	if (!sX(WriteVXR(CDF->fp,vxrOffset,
			 VXR_RECORD,VXR,
			 VXR_NULL),&pStatus)) return pStatus;
	/**********************************************************************
	* Write the record size and type fields of the new VVR for the second
	* block of remaining records.
	**********************************************************************/
	vvrOffset = offset2 - VVR_BASE_SIZE;
	recordSize = VVR_BASE_SIZE + nBytes2 + nExtraBytes;
	if (!sX(WriteVVR(CDF->fp,vvrOffset,
			 VVR_RECORDSIZE,&recordSize,
			 VVR_RECORDTYPE,&vvr_,
			 VVR_NULL),&pStatus)) return pStatus;
	/**********************************************************************
	* Insert a VXR entry for the new VVR created for the second block of
	* remaining records.
	**********************************************************************/
	if (!sX(InsertIndexEntry(CDF,vxrOffset,
				 entryN,(Logical)TRUE,
				 firstRecord2,lastRecord2,
				 vvrOffset),&pStatus)) return pStatus;
      }
      break;
    }
    case COMPRESSED_:
    case SPARSE_COMPRESSED_RECORDS_: {
      Int32 nRecords = VXR->Last[entryN] - VXR->First[entryN] + 1;
      Int32 uSize = nRecords * Var->NphyRecBytes;
      Int32 nRecords1, nBytes1, firstRecord2, lastRecord2, nRecords2;
      Int32 offset2, nBytes2, cSize, newOffset;
      /************************************************************************
      * Initialize staging area and scratch space.
      ************************************************************************/
      if (Var->stage.areaOffset == NO_OFFSET) {
	Int32 nBytes = Var->blockingFactor * Var->NphyRecBytes;
	if (!sX(InitVarStage(CDF,Var,nBytes),&pStatus)) return pStatus;
      }
      if (!sX(InitScratch(ScratchDirectory(CDF),
			  &(CDF->compressFp),
			  CDF->compressCacheSize),&pStatus)) return pStatus;
      /************************************************************************
      * Bring the CVVR/VVR to the staging area.
      ************************************************************************/
      if (!sX(DecompressToStage(CDF,Var,
				VXR->Offset[entryN],
				uSize),&pStatus)) return pStatus;
      /************************************************************************
      * Waste the CVVR/VVR.
      ************************************************************************/
      if (!sX(WasteIR(CDF,VXR->Offset[entryN],irSize),&pStatus)) return
								 pStatus;
      /************************************************************************
      * Calculate sizes/offsets for the first and second blocks of remaining
      * records.
      ************************************************************************/
      nRecords1 = first - VXR->First[entryN];
      nBytes1 = nRecords1 * Var->NphyRecBytes;
      firstRecord2 = last + 1;
      lastRecord2 = VXR->Last[entryN];
      nRecords2 = lastRecord2 - firstRecord2 + 1;
      offset2 = Var->stage.areaOffset +
		(Var->NphyRecBytes * (firstRecord2 - VXR->First[entryN]));
      nBytes2 = nRecords2 * Var->NphyRecBytes;
      /************************************************************************
      * Compress the first block of remaining records...
      ************************************************************************/
      if (!sX(Compress(CDF->stage.fp,
		       Var->stage.areaOffset,
		       nBytes1,SCRATCH_READ_ERROR,
		       Var->cType,Var->cParms,
		       CDF->compressFp,ZERO_OFFSET,
		       &cSize,SCRATCH_WRITE_ERROR),&pStatus)) return pStatus;
      /************************************************************************
      * ...and allocate/write the CVVR/VVR.
      ************************************************************************/
      if (!sX(WriteCVVRorVVR(CDF,cSize,
			     Var->stage.areaOffset,
			     nBytes1,&newOffset),&pStatus)) return pStatus;
      /************************************************************************
      * Update the index entry for the first block of remaining records.
      ************************************************************************/
      VXR->Last[entryN] = first - 1;
      VXR->Offset[entryN] = newOffset;
      if (!sX(WriteVXR(CDF->fp,vxrOffset,
		       VXR_RECORD,VXR,
		       VXR_NULL),&pStatus)) return pStatus;
      /************************************************************************
      * Compress the second block of remaining records...
      ************************************************************************/
      if (!sX(Compress(CDF->stage.fp,offset2,
		       nBytes2,SCRATCH_READ_ERROR,
		       Var->cType,Var->cParms,
		       CDF->compressFp,ZERO_OFFSET,
		       &cSize,SCRATCH_WRITE_ERROR),&pStatus)) return pStatus;
      /************************************************************************
      * ...and allocate/write the CVVR/VVR.
      ************************************************************************/
      if (!sX(WriteCVVRorVVR(CDF,cSize,offset2,
			     nBytes2,&newOffset),&pStatus)) return pStatus;
      /************************************************************************
      * Insert a new index entry for the second block of remaining records.
      ************************************************************************/
      if (!sX(InsertIndexEntry(CDF,vxrOffset,
			       entryN,(Logical)TRUE,
			       firstRecord2,lastRecord2,
			       newOffset),&pStatus)) return pStatus;
      break;
    }
    default:
      return CDF_INTERNAL_ERROR;
  }
  return pStatus;
}

/******************************************************************************
* DeleteFromEnd.
******************************************************************************/

static CDFstatus DeleteFromEnd (CDF, Var, first, vxrOffset, VXR, entryN,
				irSize)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 first;
Int32 vxrOffset;
struct VXRstruct *VXR;
int entryN;
Int32 irSize;
{
  CDFstatus pStatus = CDF_OK;
  switch (Var->vType) {
    case STANDARD_:
    case SPARSE_RECORDS_: {
      Int32 nRecords1, nBytes1, deleteOffset, recordSize, nRemainingBytes;
      /************************************************************************
      * First calculate needed offsets and sizes.
      ************************************************************************/
      nRecords1 = first - VXR->First[entryN];
      nBytes1 = nRecords1 * Var->NphyRecBytes;
      deleteOffset = VXR->Offset[entryN] + VVR_BASE_SIZE + nBytes1;
      nRemainingBytes = irSize - VVR_BASE_SIZE - nBytes1;
      /************************************************************************
      * Update the VXR entry.
      ************************************************************************/
      VXR->Last[entryN] = first - 1;
      if (!sX(WriteVXR(CDF->fp,vxrOffset,
		       VXR_RECORD,VXR,
		       VXR_NULL),&pStatus)) return pStatus;
      /************************************************************************
      * If enough bytes are being deleted for a UIR, waste them and update
      * the record size field of the VVR.
      ************************************************************************/
      if (nRemainingBytes >= UIR_BASE_SIZE) {
	if (!sX(WasteIR(CDF,deleteOffset,
			nRemainingBytes),&pStatus)) return pStatus;
	recordSize = VVR_BASE_SIZE + nBytes1;
	if (!sX(WriteVVR(CDF->fp,VXR->Offset[entryN],
			 VVR_RECORDSIZE,&recordSize,
			 VVR_NULL),&pStatus)) return pStatus;
      }
      break;
    }
    case COMPRESSED_:
    case SPARSE_COMPRESSED_RECORDS_: {
      Int32 nRecords = VXR->Last[entryN] - VXR->First[entryN] + 1;
      Int32 uSize = nRecords * Var->NphyRecBytes;
      Int32 nRecords1, nBytes1, cSize, newOffset;
      /************************************************************************
      * Initialize staging area and scratch space.
      ************************************************************************/
      if (Var->stage.areaOffset == NO_OFFSET) {
	Int32 nBytes = Var->blockingFactor * Var->NphyRecBytes;
	if (!sX(InitVarStage(CDF,Var,nBytes),&pStatus)) return pStatus;
      }
      if (!sX(InitScratch(ScratchDirectory(CDF),
			  &(CDF->compressFp),
			  CDF->compressCacheSize),&pStatus)) return pStatus;
      /************************************************************************
      * Bring the CVVR/VVR to the staging area.
      ************************************************************************/
      if (!sX(DecompressToStage(CDF,Var,
				VXR->Offset[entryN],
				uSize),&pStatus)) return pStatus;
      /************************************************************************
      * Waste the CVVR/VVR.
      ************************************************************************/
      if (!sX(WasteIR(CDF,VXR->Offset[entryN],irSize),&pStatus)) return
								 pStatus;
      /************************************************************************
      * Calculate sizes/offsets for the block of remaining records.
      ************************************************************************/
      nRecords1 = first - VXR->First[entryN];
      nBytes1 = nRecords1 * Var->NphyRecBytes;
      /************************************************************************
      * Compress the block of remaining records...
      ************************************************************************/
      if (!sX(Compress(CDF->stage.fp,
		       Var->stage.areaOffset,
		       nBytes1,SCRATCH_READ_ERROR,
		       Var->cType,Var->cParms,
		       CDF->compressFp,ZERO_OFFSET,
		       &cSize,SCRATCH_WRITE_ERROR),&pStatus)) return pStatus;
      /************************************************************************
      * ...and allocate/write the CVVR/VVR.
      ************************************************************************/
      if (!sX(WriteCVVRorVVR(CDF,cSize,
			     Var->stage.areaOffset,
			     nBytes1,&newOffset),&pStatus)) return pStatus;
      /************************************************************************
      * Update the index entry for the block of remaining records.
      ************************************************************************/
      VXR->Last[entryN] = first - 1;
      VXR->Offset[entryN] = newOffset;
      if (!sX(WriteVXR(CDF->fp,vxrOffset,
		       VXR_RECORD,VXR,
		       VXR_NULL),&pStatus)) return pStatus;
      break;
    }
    default:
      return CDF_INTERNAL_ERROR;
  }
  return pStatus;
}

/******************************************************************************
* DeleteVXRentry.
******************************************************************************/

static CDFstatus DeleteVXRentry (CDF, firstVXRoffset, delVXRoffset, delEntryN,
				 total)
struct CDFstruct *CDF;
Int32 firstVXRoffset;
Int32 delVXRoffset;
int delEntryN;
Logical *total;
{
  CDFstatus pStatus = CDF_OK;
  Int32 vxrOffset = firstVXRoffset, prevVXRoffset = ZERO_OFFSET;
  struct VXRstruct VXR, nextVXR; int entryN, lastEntryN;
  /****************************************************************************
  * Find the VXR in which an entry is being deleted while keeping track of the
  * previous VXR.
  ****************************************************************************/
  if (!sX(ReadVXR(CDF->fp,vxrOffset,
		  VXR_RECORD,&VXR,
		  VXR_NULL),&pStatus)) return pStatus;
  while (vxrOffset != delVXRoffset) {
    prevVXRoffset = vxrOffset;
    vxrOffset = VXR.VXRnext;
    if (!sX(ReadVXR(CDF->fp,vxrOffset,
		    VXR_RECORD,&VXR,
		    VXR_NULL),&pStatus)) return pStatus;
  }
  /****************************************************************************
  * Until done...
  ****************************************************************************/
  for (;;) {
     /*************************************************************************
     * Move the following entries up.  Nothing will happen if there is only
     * one entry.
     *************************************************************************/
     for (entryN = delEntryN + 1; entryN < VXR.NusedEntries; entryN++) {
	VXR.First[entryN-1] = VXR.First[entryN];
	VXR.Last[entryN-1] = VXR.Last[entryN];
	VXR.Offset[entryN-1] = VXR.Offset[entryN];
     }
     lastEntryN = (int) (VXR.NusedEntries - 1);
     /*************************************************************************
     * If a VXR does not follow, erase the last entry if there was more than
     * one or waste the VXR is there was only one (entry).
     *************************************************************************/
     if (VXR.VXRnext == ZERO_OFFSET) {
       if (VXR.NusedEntries > 1) {
	 VXR.First[lastEntryN] = NO_RECORD;
	 VXR.Last[lastEntryN] = NO_RECORD;
	 VXR.Offset[lastEntryN] = NO_OFFSET;
	 VXR.NusedEntries--;
	 if (!sX(WriteVXR(CDF->fp,vxrOffset,
			  VXR_RECORD,&VXR,
			  VXR_NULL),&pStatus)) return pStatus;
       }
       else {
	 if (!sX(WasteIR(CDF,vxrOffset,
			 VXR.RecordSize),&pStatus)) return pStatus;
	 if (prevVXRoffset != ZERO_OFFSET) {
	   Int32 zeroOffset = ZERO_OFFSET;
	   if (!sX(WriteVXR(CDF->fp,prevVXRoffset,
			    VXR_VXRNEXT,&zeroOffset,
			    VXR_NULL),&pStatus)) return pStatus;
	 }
	 else
	   *total = TRUE;
       }
       return pStatus;
     }
     /*************************************************************************
     * Otherwise, read the next VXR, set the current VXR's last entry to the
     * first entry of the next VXR, rewrite the current VXR, and then set the
     * current VXR to the next VXR and set the first entry to be deleted and
     * continue...
     *************************************************************************/
     if (!sX(ReadVXR(CDF->fp,VXR.VXRnext,
		     VXR_RECORD,&nextVXR,
		     VXR_NULL),&pStatus)) return pStatus;
     VXR.First[lastEntryN] = nextVXR.First[0];
     VXR.Last[lastEntryN] = nextVXR.Last[0];
     VXR.Offset[lastEntryN] = nextVXR.Offset[0];
     if (!sX(WriteVXR(CDF->fp,vxrOffset,
		      VXR_RECORD,&VXR,
		      VXR_NULL),&pStatus)) return pStatus;
     prevVXRoffset = vxrOffset;
     vxrOffset = VXR.VXRnext;
     VXR = nextVXR;
     delEntryN = 0;
  }
}

/******************************************************************************
* InsertIndexEntry.
******************************************************************************/

static CDFstatus InsertIndexEntry (CDF, vxrOffset, entryN, after, first, last,
				   offset)
struct CDFstruct *CDF;
Int32 vxrOffset;
int entryN;
Logical after;          /* TRUE: Insert the new entry after `entryN'.
			   FALSE: Insert the new entry at `entryN'. */
Int32 first;
Int32 last;
Int32 offset;
{
  CDFstatus pStatus = CDF_OK; struct VXRstruct VXR; int eN, lastEntryN;
  Int32 pushedFirst, pushedLast, pushedOffset, newVXRoffset;
  /****************************************************************************
  * Read the VXR.
  ****************************************************************************/
  if (!sX(ReadVXR(CDF->fp,vxrOffset,
		  VXR_RECORD,&VXR,
		  VXR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * If there is room in this VXR for another entry, simply insert it.
  ****************************************************************************/
  if (VXR.NusedEntries < VXR.Nentries) {
    if (after) {
      for (eN = (int) VXR.NusedEntries; eN > entryN + 1; eN--) {
	 VXR.First[eN] = VXR.First[eN-1];
	 VXR.Last[eN] = VXR.Last[eN-1];
	 VXR.Offset[eN] = VXR.Offset[eN-1];
      }
      VXR.First[entryN+1] = first;
      VXR.Last[entryN+1] = last;
      VXR.Offset[entryN+1] = offset;
    }
    else {
      for (eN = (int) VXR.NusedEntries; eN > entryN; eN--) {
	 VXR.First[eN] = VXR.First[eN-1];
	 VXR.Last[eN] = VXR.Last[eN-1];
	 VXR.Offset[eN] = VXR.Offset[eN-1];
      }
      VXR.First[entryN] = first;
      VXR.Last[entryN] = last;
      VXR.Offset[entryN] = offset;
    }
    VXR.NusedEntries++;
    if (!sX(WriteVXR(CDF->fp,vxrOffset,
		     VXR_RECORD,&VXR,
		     VXR_NULL),&pStatus)) return pStatus;
    return pStatus;
  }
  /****************************************************************************
  * No room for another entry...
  ****************************************************************************/
  lastEntryN = (int) (VXR.NusedEntries - 1);
  if (after) {
    if (entryN == lastEntryN) {
      pushedFirst = first;
      pushedLast = last;
      pushedOffset = offset;
    }
    else {
      pushedFirst = VXR.First[lastEntryN];
      pushedLast = VXR.Last[lastEntryN];
      pushedOffset = VXR.Offset[lastEntryN];
      for (eN = lastEntryN; eN > entryN + 1; eN--) {
	 VXR.First[eN] = VXR.First[eN-1];
	 VXR.Last[eN] = VXR.Last[eN-1];
	 VXR.Offset[eN] = VXR.Offset[eN-1];
      }
      VXR.First[entryN+1] = first;
      VXR.Last[entryN+1] = last;
      VXR.Offset[entryN+1] = offset;
    }
  }
  else {
    pushedFirst = VXR.First[lastEntryN];
    pushedLast = VXR.Last[lastEntryN];
    pushedOffset = VXR.Offset[lastEntryN];
    for (eN = lastEntryN; eN > entryN; eN--) {
       VXR.First[eN] = VXR.First[eN-1];
       VXR.Last[eN] = VXR.Last[eN-1];
       VXR.Offset[eN] = VXR.Offset[eN-1];
    }
    VXR.First[entryN] = first;
    VXR.Last[entryN] = last;
    VXR.Offset[entryN] = offset;
  }
  /****************************************************************************
  * If this is the last VXR on the linked list, allocate a new VXR, update
  * this VXR with the new VXR's offset, and write the new VXR.
  ****************************************************************************/
  if (VXR.VXRnext == ZERO_OFFSET) {
    if (!sX(AllocateIR(CDF,(Int32)VXR_BASE_SIZE,
		       &newVXRoffset),&pStatus)) return pStatus;
    VXR.VXRnext = newVXRoffset;
    if (!sX(WriteVXR(CDF->fp,vxrOffset,
		     VXR_RECORD,&VXR,
		     VXR_NULL),&pStatus)) return pStatus;
    InitNewVXR (&VXR, pushedFirst, pushedLast, pushedOffset);
    if (!sX(WriteVXR(CDF->fp,newVXRoffset,
		     VXR_RECORD,&VXR,
		     VXR_NULL),&pStatus)) return pStatus;
    return pStatus;
  }
  /****************************************************************************
  * There is another VXR on the linked list - update this VXR and recursively
  * call this routine to insert the entry that was pushed out in the first
  * entry position.
  ****************************************************************************/
  if (!sX(WriteVXR(CDF->fp,vxrOffset,
		   VXR_RECORD,&VXR,
		   VXR_NULL),&pStatus)) return pStatus;
  if (!sX(InsertIndexEntry(CDF,VXR.VXRnext,
			   0,(Logical)FALSE,
			   pushedFirst,pushedLast,
			   pushedOffset),&pStatus)) return pStatus;
  return pStatus;
}

/******************************************************************************
* UpdateIndexEntries_r.
******************************************************************************/

static CDFstatus UpdateIndexEntries_r (fp, vxrOffset, aboveRecord, recordCount)
vFILE *fp;
Int32 vxrOffset;
Int32 aboveRecord;
Int32 recordCount;
{
  CDFstatus pStatus = CDF_OK; struct VXRstruct VXR; int entryN; Int32 irType;
  while (vxrOffset != ZERO_OFFSET) {
    Logical modified = FALSE;
    if (!sX(ReadVXR(fp,vxrOffset,
		    VXR_RECORD,&VXR,
		    VXR_NULL),&pStatus)) return pStatus;
    for (entryN = 0; entryN < VXR.NusedEntries; entryN++) {
       if (VXR.Last[entryN] > aboveRecord) {
	 if (!sX(ReadIrType(fp,VXR.Offset[entryN],
			    &irType),&pStatus)) return pStatus;
	 switch (irType) {
	   case VXR_:
	     if (!sX(UpdateIndexEntries_r(fp,VXR.Offset[entryN],aboveRecord,
					  recordCount),&pStatus))
	 	return pStatus;
	     break;
	   case VVR_:
	   case CVVR_:
	     break;
	   default:
	     return CORRUPTED_V2_CDF;
	 }
	 if (VXR.First[entryN] > aboveRecord) VXR.First[entryN] -= recordCount;
	 VXR.Last[entryN] -= recordCount;
	 modified = TRUE;
       }
    }
    if (modified) {
      if (!sX(WriteVXR(fp,vxrOffset,
		       VXR_RECORD,&VXR,
		       VXR_NULL),&pStatus)) return pStatus;
    }
    vxrOffset = VXR.VXRnext;
  }
  return pStatus;
}

/******************************************************************************
* WriteCVVRorVVR.
******************************************************************************/

static CDFstatus WriteCVVRorVVR (CDF, cSize, stageOffset, uSize, newOffset)
struct CDFstruct *CDF;
Int32 cSize;
Int32 stageOffset;
Int32 uSize;
Int32 *newOffset;
{
  CDFstatus pStatus = CDF_OK; Int32 recordSize, tOffset;
  if (CVVR_BASE_SIZE + cSize < VVR_BASE_SIZE + uSize) {
    struct CVVRstruct CVVR;
    recordSize = CVVR_BASE_SIZE + cSize;
    LoadCVVRx (CVVR, recordSize, cSize)
    if (!sX(AllocateIR(CDF,recordSize,newOffset),&pStatus)) return pStatus;
    if (!sX(WriteCVVR(CDF->fp,*newOffset,
		      CVVR_RECORDx,&CVVR,
		      CVVR_NULL),&pStatus)) return pStatus;
    tOffset = *newOffset + CVVR_BASE_SIZE;
    if (!sX(CopyBytes(CDF->compressFp,ZERO_OFFSET,
		      SCRATCH_READ_ERROR,cSize,CDF->fp,
		      tOffset,CDF_WRITE_ERROR),&pStatus)) return pStatus;
  }
  else {
    struct VVRstruct VVR;
    recordSize = VVR_BASE_SIZE + uSize;
    LoadVVRx (VVR, recordSize)
    if (!sX(AllocateIR(CDF,recordSize,newOffset),&pStatus)) return pStatus;
    if (!sX(WriteVVR(CDF->fp,*newOffset,
		     VVR_RECORDx,&VVR,
		     VVR_NULL),&pStatus)) return pStatus;
    tOffset = *newOffset + VVR_BASE_SIZE;
    if (!sX(CopyBytes(CDF->stage.fp,stageOffset,
		      SCRATCH_READ_ERROR,uSize,CDF->fp,
		      tOffset,CDF_WRITE_ERROR),&pStatus)) return pStatus;
  }
  return pStatus;
}
