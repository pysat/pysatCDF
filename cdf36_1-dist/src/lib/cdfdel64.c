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
*                               variable is deleted.
*   V2.1  29-Jun-04, M Liu      Added LFS (Large File Support > 2G).
*   V2.2  08-Jan-09, M Liu      Update the attrnum in AEDR when an attribute
*                               is deleted.
*   V2.3  08-May-10, M Liu      Reset firstRecInVVR/lastRecInVVR when a 
*                               record(s) gets deleted.
*   V2.4  24-Jan-11, M Liu      Modified UpdateIndexEntries_r_64 to acquire
*                               physical record size to set VXR's last record.
*   V2.5  09-May-12, M Liu      Remodified UpdateIndexEntries_r_64.
*   V2.6  27-May-14, M Liu      Added renumbering the sparse records after
*                               deletion.
*
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"

/******************************************************************************
* Local function prototypes.
******************************************************************************/

static CDFstatus WasteTree_r_64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T vxrOffset
));
static CDFstatus DeleteVarRecords64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 firstRec, Int32 lastRec,
  Logical reNumber
));
static CDFstatus DeleteRecords64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 firstRec, Int32 lastRec,
  Int32 *deletedTo
));
static CDFstatus DeleteRecords_r_64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, OFF_T firstVXRoffset,
  Int32 firstRec, Int32 lastRec, Int32 *deletedTo, Logical *total
));
static CDFstatus DeleteFromFront64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 last,
  OFF_T vxrOffset, struct VXRstruct64 *VXR, int entryN, OFF_T irSize
));
static CDFstatus DeleteFromMiddle64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 first, Int32 last,
  OFF_T vxrOffset, struct VXRstruct64 *VXR, int entryN, OFF_T irSize
));
static CDFstatus DeleteFromEnd64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 first,
  OFF_T vxrOffset, struct VXRstruct64 *VXR, int entryN, OFF_T irSize
));
static CDFstatus DeleteVXRentry64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T firstVXRoffset, OFF_T delVXRoffset,
  int delEntryN, Logical *total
));
static CDFstatus InsertIndexEntry64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T vxrOffset, int entryN, Logical after,
  Int32 first, Int32 last, OFF_T offset
));
static CDFstatus UpdateIndexEntries_r_64 PROTOARGs((
  vFILE *fp, OFF_T vxrOffset, Int32 aboveRecord, Int32 recordCount
));
static CDFstatus WriteCVVRorVVR64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T cSize, OFF_T stageOffset, OFF_T uSize,
  OFF_T *newOffset
));

/******************************************************************************
* CDFdel64.
******************************************************************************/

STATICforIDL CDFstatus CDFdel64 (Va, Cur)
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
    if (!WriteAccess64(CDF,TRUE,&pStatus)) return pStatus;
    sX (DeleteCDFfiles64(CDF), &pStatus);
    if (CDF->uDotFp != NULL) {
      if (!DELETEv64(CDF->uDotFp,NULL)) sX (SCRATCH_DELETE_ERROR, &pStatus);
      CDF->uDotFp = NULL;
    }
    if (CDF->stage.fp != NULL) {
      if (!DELETEv64(CDF->stage.fp,NULL)) sX (SCRATCH_DELETE_ERROR, &pStatus);
      CDF->stage.fp = NULL;
    }
    if (CDF->compressFp != NULL) {
      if (!DELETEv64(CDF->compressFp,NULL)) sX (SCRATCH_DELETE_ERROR, &pStatus);
      CDF->compressFp = NULL;
    }
    FreeCDFid (CDF, FALSE);
    Cur->cdf = NULL;
    ReduceOpenCDFsCount();
    break;
  }
  /****************************************************************************
  * zVAR_/rVAR_
  ****************************************************************************/
  case zVAR_:
  case rVAR_: {
    Logical zOp = (Va->item == zVAR_), zVar;
    struct CDFstruct *CDF; struct VDRstruct64 VDR, VDRt;
    Int32 rMaxRec = NO_RECORD;
    Int32 scope, entryN, nVars; int varN;
    OFF_T vOffset, tOffset, aOffset, eOffset, VDRhead;
    /**************************************************************************
    * Get pointer to current CDF and locate the current r/zVariable.
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    if (!CDF->singleFile) return UNSUPPORTED_OPERATION;
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    /**************************************************************************
    * Switch to write access.
    **************************************************************************/
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;

/*
    if (!sX(LocateCurrentVar64(CDF,zOp,&vOffset,&zVar,NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
*/

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) vOffset = CDF->CURzVarOffset64;
    else vOffset = CDF->CURrVarOffset64;

    /**************************************************************************
    * Read/get GDR field(s).
    **************************************************************************/
    if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
		    BOO(zVar,GDR_zVDRHEAD,GDR_rVDRHEAD),&VDRhead,
		    GDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    nVars = BOO(zVar,CDF->NzVars,CDF->NrVars);
    /**************************************************************************
    * Read the VDR.
    **************************************************************************/
    if (!sX(ReadVDR64(CDF,CDF->fp,vOffset,zVar,
		    VDR_RECORD,&VDR,NULL,
		    VDR_NULL),&pStatus)){
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
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
	if (!sX(ReadVDR64(CDF,CDF->fp,tOffset,zVar,
			  VDR_RECORD,&VDRt,NULL,
			  VDR_NULL),&pStatus)){
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
	if (!zVar) rMaxRec = MAXIMUM (rMaxRec, VDRt.MaxRec);
	if (VDRt.VDRnext == vOffset) {
	  VDRt.VDRnext = VDR.VDRnext;
	  if (!sX(WriteVDR64(CDF,CDF->fp,tOffset,zVar,
			     VDR_RECORD,&VDRt,NULL,
			     VDR_NULL),&pStatus)) {
	    AbortAccess64 (CDF, UPDATE, noDELETE);
	    return pStatus;
	  }
	  break;
	}
	tOffset = VDRt.VDRnext;
      }
      if (tOffset == 0) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return CORRUPTED_V3_CDF;
      }
    }
    nVars--;
    /**************************************************************************
    * Renumber following VDRs.  If this is a true rVariable, continue checking
    * for the maximum record number written.
    **************************************************************************/
    tOffset = VDR.VDRnext;
    while (tOffset != 0) {
      if (!sX(ReadVDR64(CDF,CDF->fp,tOffset,zVar,
		        VDR_RECORD,&VDRt,NULL,
		        VDR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (!zVar) rMaxRec = MAXIMUM (rMaxRec, VDRt.MaxRec);
      VDRt.Num--;
      if (!sX(WriteVDR64(CDF,CDF->fp,tOffset,zVar,
		         VDR_RECORD,&VDRt,NULL,
		         VDR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      tOffset = VDRt.VDRnext;
    }
    /**************************************************************************
    * Write/store the GDR fields that may have changed.
    **************************************************************************/
    if (!zVar) {
      CDF->rMaxRec = rMaxRec;
      if (!sX(WriteGDR64(CDF->fp,CDF->GDRoffset64,
		         GDR_rMAXREC,&rMaxRec,
		         GDR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    if (!sX(WriteGDR64(CDF->fp,CDF->GDRoffset64,
		       BOO(zVar,GDR_zVDRHEAD,GDR_rVDRHEAD),&VDRhead,
		       BOO(zVar,GDR_NzVARS,GDR_NrVARS),&nVars,
		       GDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (zVar)
      CDF->NzVars = nVars;
    else
      CDF->NrVars = nVars;
    /**************************************************************************
    * Waste VDR.
    **************************************************************************/
    if (!sX(WasteIR64(CDF,vOffset,VDR.RecordSize),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Waste CPR/SPR (if one exists).
    **************************************************************************/
    if (VDR.CPRorSPRoffset != (OFF_T) NO_OFFSET64) {
      OFF_T irSize;
      if (!sX(ReadIrSize64(CDF->fp,
			   VDR.CPRorSPRoffset,
			   &irSize),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (!sX(WasteIR64(CDF,VDR.CPRorSPRoffset,irSize),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    /**************************************************************************
    * Waste VXRs and VVRs/CVVRs.
    **************************************************************************/
    if (!sX(WasteTree_r_64(CDF,VDR.VXRhead),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
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
      CDF->CURzVarOffset64 = (OFF_T) 0;
    } else {
      CDF->CURrVarNum = RESERVED_VARNUM;
      CDF->CURrVarOffset64 = (OFF_T) 0;
    }
    /**************************************************************************
    * Delete the associated attribute entries and renumber the entries that
    * are associated with the variables that followed the variable that was
    * deleted.
    **************************************************************************/
    if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
		      GDR_ADRHEAD,&aOffset,
		      GDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    while (aOffset != 0) {
      /************************************************************************
      * Read the scope of the attribute.
      ************************************************************************/
      if (!sX(ReadADR64(CDF->fp,aOffset,
		        ADR_SCOPE,&scope,
		        ADR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (VARIABLEscope(scope)) {
	Int32 maxEntry = NO_ENTRY;
	/**********************************************************************
	* Delete associated entry.
	**********************************************************************/
	tStatus = FindEntryByNumber64 (CDF, aOffset, zVar, VDR.Num, &eOffset);
	switch (tStatus) {
	  case CDF_OK:
	    if (!sX(DeleteEntry64(CDF,aOffset,eOffset),&pStatus)) {
	      AbortAccess64 (CDF, UPDATE, noDELETE);
	      return pStatus;
	    }
	    break;
	  case NO_SUCH_ENTRY:
	    break;
	  default:
	    AbortAccess64 (CDF, UPDATE, noDELETE);
	    return tStatus;
	}
	/**********************************************************************
	* Renumber entries.  Note that the entry numbers are not necessarily
	* increasing.  (The entries may have been written in any order.)  Also
	* determine the new maximum entry number.
	**********************************************************************/
	if (!sX(ReadADR64(CDF->fp,aOffset,
			  BOO(zVar,ADR_AzEDRHEAD,ADR_AgrEDRHEAD),&eOffset,
			  ADR_NULL),&pStatus)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
	while (eOffset != 0) {
	  if (!sX(ReadAEDR64(CDF->fp,eOffset,
			     AEDR_NUM,&entryN,
			     AEDR_NULL),&pStatus)) {
	    AbortAccess64 (CDF, UPDATE, noDELETE);
	    return pStatus;
	  }
	  if (entryN > VDR.Num) {
	    entryN--;
	    if (!sX(WriteAEDR64(CDF,CDF->fp,eOffset,
			        AEDR_NUM,&entryN,
			        AEDR_NULL),&pStatus)) {
	      AbortAccess64 (CDF, UPDATE, noDELETE);
	      return pStatus;
	    }
	  }
	  maxEntry = MAXIMUM (maxEntry, entryN);
	  if (!sX(ReadAEDR64(CDF->fp,eOffset,
			     AEDR_AEDRNEXT,&eOffset,
			     AEDR_NULL),&pStatus)) {
	    AbortAccess64 (CDF, UPDATE, noDELETE);
	    return pStatus;
	  }
	}
	/**********************************************************************
	* Write new maximum entry number to the ADR.
	**********************************************************************/
	if (!sX(WriteADR64(CDF->fp,aOffset,
			   BOO(zVar,ADR_MAXzENTRY,ADR_MAXgrENTRY),&maxEntry,
			   ADR_NULL),&pStatus)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
      }
      /************************************************************************
      * Read offset of next ADR.
      ************************************************************************/
      if (!sX(ReadADR64(CDF->fp,aOffset,
		        ADR_ADRNEXT,&aOffset,
		        ADR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    /**************************************************************************
    * Reset the current entry offset (in case it was affected).
    **************************************************************************/
    if (!sX(BOO(zOp,SetCURzEntry64(CDF,FALSE,CDF->CURzEntryNum),
		    SetCURgrEntry64(CDF,FALSE,CDF->CURgrEntryNum)),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    break;
  }
  /****************************************************************************
  * rVAR_RECORDS_/zVAR_RECORDS_/rVAR_RECORDS_RENUMBER_/zVAR_RECORDS_RENUMBER_
  ****************************************************************************/
  case rVAR_RECORDS_:
  case zVAR_RECORDS_:
  case rVAR_RECORDS_RENUMBER_:
  case zVAR_RECORDS_RENUMBER_: {
    Logical zOp = (Va->item == zVAR_RECORDS_ ||
                   Va->item == zVAR_RECORDS_RENUMBER_);
    Logical reNumber;
    struct CDFstruct *CDF; struct VarStruct *Var;
    Int32 firstRec = (Int32) va_arg (Va->ap, long);
    Int32 lastRec = (Int32) va_arg (Va->ap, long);
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
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    if (!sX(InitCurrentVar64(CDF,zOp,&Var),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (Va->item == rVAR_RECORDS_RENUMBER_ ||
        Va->item == zVAR_RECORDS_RENUMBER_)
      reNumber = TRUE;
    else
      reNumber = FALSE;
    if (!sX(DeleteVarRecords64(CDF,Var,firstRec,lastRec,reNumber),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    break;
  }
  /****************************************************************************
  * ATTR_
  ****************************************************************************/
  case ATTR_: {
    struct CDFstruct *CDF;
    OFF_T tOffset;
    struct GDRstruct64 GDR;
    struct ADRstruct64 ADR, ADRt;
    struct AEDRstruct64 AEDRt;
    OFF_T eOffset, eNext;
    /**************************************************************************
    * Get pointer to current CDF and locate the current attribute.
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTattrSELECTED64(CDF)) return NO_ATTR_SELECTED;
    /**************************************************************************
    * Read the GDR and ADR.
    **************************************************************************/
    if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
		      GDR_RECORD,&GDR,
		      GDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (!sX(ReadADR64(CDF->fp,CDF->CURattrOffset64,
		      ADR_RECORD,&ADR,
		      ADR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Switch to write access.
    **************************************************************************/
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    /**************************************************************************
    * Remove the ADR from the list of attributes and decrement the number of
    * attributes.
    **************************************************************************/
    if (GDR.ADRhead == CDF->CURattrOffset64)
      GDR.ADRhead = ADR.ADRnext;
    else {
      tOffset = GDR.ADRhead;
      while (tOffset != 0) {
	if (!sX(ReadADR64(CDF->fp,tOffset,
			  ADR_RECORD,&ADRt,
			  ADR_NULL),&pStatus)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
	if (ADRt.ADRnext == CDF->CURattrOffset64) break;
	tOffset = ADRt.ADRnext;
      }
      if (tOffset == 0) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return CORRUPTED_V3_CDF;
      }
      ADRt.ADRnext = ADR.ADRnext;
      if (!sX(WriteADR64(CDF->fp,tOffset,
		         ADR_RECORD,&ADRt,
		         ADR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    GDR.NumAttr--;
    if (!sX(WriteGDR64(CDF->fp,CDF->GDRoffset64,
		       GDR_RECORD,&GDR,
		       GDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Renumber each of the following attributes.
    **************************************************************************/
    tOffset = ADR.ADRnext;
    while (tOffset != 0) {
      if (!sX(ReadADR64(CDF->fp,tOffset,
		        ADR_RECORD,&ADRt,
		        ADR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      ADRt.Num--;
      if (!sX(WriteADR64(CDF->fp,tOffset,
		         ADR_RECORD,&ADRt,
		         ADR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      eOffset = ADRt.AgrEDRhead;
      while (eOffset != 0) {
        if (!sX(ReadAEDR64(CDF->fp,eOffset,
                           AEDR_AEDRNEXT,&eNext,
                           AEDR_NULL),&pStatus)) {
          AbortAccess64 (CDF, UPDATE, noDELETE);
          return pStatus;
        }
        if (!sX(WriteAEDR64(CDF,CDF->fp,eOffset,
                            AEDR_ATTRNUM,&ADRt.Num,
                            AEDR_NULL),&pStatus)) {
          AbortAccess64 (CDF, UPDATE, noDELETE);
          return pStatus;
        }
        eOffset = eNext;
      }
      eOffset = ADRt.AzEDRhead;
      while (eOffset != 0) {
        if (!sX(ReadAEDR64(CDF->fp,eOffset,
                           AEDR_AEDRNEXT,&eNext,
                           AEDR_NULL),&pStatus)) {
          AbortAccess64 (CDF, UPDATE, noDELETE);
          return pStatus;
        }
        if (!sX(WriteAEDR64(CDF,CDF->fp,eOffset,
                            AEDR_ATTRNUM,&ADRt.Num,
                            AEDR_NULL),&pStatus)) {
          AbortAccess64 (CDF, UPDATE, noDELETE);
          return pStatus;
        }
        eOffset = eNext;
      }
      tOffset = ADRt.ADRnext;
    }
    /**************************************************************************
    * Waste the ADR and each of the AgrEDR and AzEDR's for the attribute.
    **************************************************************************/
    if (!sX(WasteIR64(CDF,CDF->CURattrOffset64,ADR.RecordSize),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    tOffset = ADR.AgrEDRhead;
    while (tOffset != 0) {
      if (!sX(ReadAEDR64(CDF->fp,tOffset,
		         AEDR_RECORD,&AEDRt,NULL,
		         AEDR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (!sX(WasteIR64(CDF,tOffset,AEDRt.RecordSize),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      tOffset = AEDRt.AEDRnext;
    }
    tOffset = ADR.AzEDRhead;
    while (tOffset != 0) {
      if (!sX(ReadAEDR64(CDF->fp,tOffset,
		         AEDR_RECORD,&AEDRt,NULL,
		         AEDR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (!sX(WasteIR64(CDF,tOffset,AEDRt.RecordSize),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      tOffset = AEDRt.AEDRnext;
    }
    /**************************************************************************
    * Reset the current attribute offset and current entry offsets.
    **************************************************************************/
    CDF->CURattrOffset64 = (OFF_T) RESERVED_ATTROFFSET64;
    CDF->CURgrEntryOffset64 = (OFF_T) RESERVED_ENTRYOFFSET64;
    CDF->CURzEntryOffset64 = (OFF_T) RESERVED_ENTRYOFFSET64;
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
    OFF_T eOffset;
    /**************************************************************************
    * Get pointer to current CDF, locate the current attribute, and verify
    * that a current entry number has been selected.
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTattrSELECTED64(CDF)) return NO_ATTR_SELECTED;
    if (E3(entryType,
	   CDF->CURgrEntryNum,
	   CDF->CURgrEntryNum,
	   CDF->CURzEntryNum) == RESERVED_ENTRYNUM) return NO_ENTRY_SELECTED;
    /**************************************************************************
    * Verify that the operation being performed is legal for the attribute's
    * scope.
    **************************************************************************/
    if (!sX(CheckEntryOp64(CDF,entryType),&pStatus)) return pStatus;
    /**************************************************************************
    * Switch to write access.
    **************************************************************************/
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    /**************************************************************************
    * Locate the entry to be deleted.
    **************************************************************************/
    eOffset = E3(entryType,CDF->CURgrEntryOffset64,
			   CDF->CURgrEntryOffset64,
			   CDF->CURzEntryOffset64);
    if (eOffset == (OFF_T) RESERVED_ENTRYOFFSET64) return NO_SUCH_ENTRY;
    /**************************************************************************
    * Delete the entry.
    **************************************************************************/
    if (!sX(DeleteEntry64(CDF,CDF->CURattrOffset64,eOffset),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Reset the current entry offset (to indicate the entry no longer exists).
    **************************************************************************/
    switch (entryType) {
      case gENTRYt:
      case rENTRYt:
	CDF->CURgrEntryOffset64 = (OFF_T) RESERVED_ENTRYOFFSET64;
	break;
      case zENTRYt:
	CDF->CURzEntryOffset64 = (OFF_T) RESERVED_ENTRYOFFSET64;
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
* WasteTree_r_64.
******************************************************************************/

static CDFstatus WasteTree_r_64 (CDF, vxrOffset)
struct CDFstruct *CDF;
OFF_T vxrOffset;
{
  CDFstatus pStatus = CDF_OK;
  int e; struct VXRstruct64 VXR; Int32 irType; OFF_T irSize;
  while (vxrOffset != (OFF_T) ZERO_OFFSET64) {
    if (!sX(ReadVXR64(CDF->fp,vxrOffset,
		      VXR_RECORD,&VXR,
		      VXR_NULL),&pStatus)) return pStatus;
    if (!sX(WasteIR64(CDF,vxrOffset,VXR.RecordSize),&pStatus)) return pStatus;
    for (e = 0; e < VXR.NusedEntries; e++) {
       if (!sX(ReadIrType64(CDF->fp,
			    VXR.Offset[e],
			    &irType),&pStatus)) return pStatus;
       switch (irType) {
	 case VXR_:
	   if (!sX(WasteTree_r_64(CDF,VXR.Offset[e]),&pStatus)) return pStatus;
	   break;
	 case VVR_:
	 case CVVR_:
	   if (!sX(ReadIrSize64(CDF->fp,
			        VXR.Offset[e],
			        &irSize),&pStatus)) return pStatus;
	   if (!sX(WasteIR64(CDF,
			     VXR.Offset[e],
			     irSize),&pStatus)) return pStatus;
	   break;
	 default:
	   return CORRUPTED_V3_CDF;
       }
    }
    vxrOffset = VXR.VXRnext;
  }
  return pStatus;
}

/******************************************************************************
* DeleteVarRecords64.
******************************************************************************/

static CDFstatus DeleteVarRecords64 (CDF, Var, firstRec, lastRec, reNumber)
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
      Int32 aboveRecord, deleteCount;
      OFF_T vxrHead;
      /************************************************************************
      * Verify/adjust the records to be deleted.
      ************************************************************************/
      if (firstRec > Var->maxAllocated) return pStatus;
      lastRec = MINIMUM(lastRec,Var->maxAllocated);
      /************************************************************************
      * Delete the records.
      ************************************************************************/
      for (recNum = firstRec; recNum <= lastRec; recNum = deletedTo + 1) {
	 if (!sX(DeleteRecords64(CDF,Var,recNum,
			         lastRec,&deletedTo),&pStatus)) return pStatus;
      }
      deleteCount = lastRec - firstRec + 1;
      /************************************************************************
      * Update the index entries.
      ************************************************************************/
      aboveRecord = firstRec - 1;
      if (!sX(ReadVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
		        VDR_VXRHEAD,&vxrHead,
		        VDR_NULL),&pStatus)) return pStatus;
      if (!sX(UpdateIndexEntries_r_64(CDF->fp,vxrHead,aboveRecord,deleteCount),
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
      Var->offsetOfVVR64 = (OFF_T) NO_OFFSET64;
      /************************************************************************
      * Update the maximum record field in the VDR.
      ************************************************************************/
      if (firstRec <= Var->maxRec) {
	if (lastRec > Var->maxRec)
	  Var->maxRec = firstRec - 1;
	else
	  Var->maxRec -= deleteCount;
	if (!sX(WriteVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
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
      OFF_T vxrHead; long maxRec;
      /************************************************************************
      * Flush staging area first.  This will automatically clear the staging
      * area as well.
      ************************************************************************/
      if (!sX(FlushStage64(CDF,Var),&pStatus)) return pStatus;
      /************************************************************************
      * Verify/adjust the records to be deleted.
      ************************************************************************/
      if (firstRec > Var->maxAllocated) return pStatus;
      lastRec = MINIMUM(lastRec,Var->maxAllocated);
      /************************************************************************
      * Delete the records.
      ************************************************************************/
      for (recNum = firstRec; recNum <= lastRec; recNum = deletedTo + 1) {
	 if (!sX(DeleteRecords64(CDF,Var,recNum,
			         lastRec,&deletedTo),&pStatus)) return pStatus;
      }
      if (reNumber) {
        deleteCount = lastRec - firstRec + 1;
        /*********************************************************************
        * Update the index entries.
        *********************************************************************/
        aboveRecord = firstRec - 1;
        if (!sX(ReadVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
                          VDR_VXRHEAD,&vxrHead,
                          VDR_NULL),&pStatus)) return pStatus;
        if (!sX(UpdateIndexEntries_r_64(CDF->fp,vxrHead,aboveRecord,
                                        deleteCount),&pStatus))
          return pStatus;
        /*********************************************************************
        * Update the control information.
        *********************************************************************/
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
        /*********************************************************************
        * Update the maximum record field in the VDR.
        *********************************************************************/
        maxRec = Var->maxRec;
        if (firstRec <= Var->maxRec) {
          if (lastRec > Var->maxRec)
            maxRec = firstRec - 1;
          else
            maxRec -= deleteCount;
          if (!sX(WriteVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
                             VDR_MAXREC,&(maxRec),
                             VDR_NULL),&pStatus)) return pStatus;
        }
        if (INCLUSIVE(firstRec,Var->maxRec,lastRec)) {
  	  if (!sX(PrevRecord64(CDF,Var->VDRoffset64,Var->zVar,
			       (Int32)(firstRec - 1),
			       &(Var->maxRec),&found),&pStatus)) return
                                                                     pStatus;
    	  if (!found) Var->maxRec = NO_RECORD;
	  if (!sX(WriteVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
			     VDR_MAXREC,&(Var->maxRec),
			     VDR_NULL),&pStatus)) return pStatus;
        } else
          Var->maxRec = maxRec;
      } else {
        /*********************************************************************
        * Update the control information.
        *********************************************************************/
        if (INCLUSIVE(firstRec,Var->maxWritten,lastRec)) {
          if (!sX(PrevRecord64(CDF,Var->VDRoffset64,Var->zVar,
                               (Int32)(firstRec - 1),
                               &(Var->maxWritten),&found),&pStatus)) return
                                                                     pStatus;
          if (!found) Var->maxWritten = NO_RECORD;
        }
        if (INCLUSIVE(firstRec,Var->maxAllocated,lastRec)) {
          if (!sX(PrevRecord64(CDF,Var->VDRoffset64,Var->zVar,
                               (Int32)(firstRec - 1),
                               &(Var->maxAllocated),&found),&pStatus)) return
                                                                     pStatus;
          if (!found) Var->maxAllocated = NO_RECORD;
        }
        /*********************************************************************
        * Update the maximum record field in the VDR.
        *********************************************************************/
        if (INCLUSIVE(firstRec,Var->maxRec,lastRec)) {
          if (!sX(PrevRecord64(CDF,Var->VDRoffset64,Var->zVar,
                               (Int32)(firstRec - 1),
                               &(Var->maxRec),&found),&pStatus)) return
                                                                     pStatus;
          if (!found) Var->maxRec = NO_RECORD;
          if (!sX(WriteVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
                             VDR_MAXREC,&(Var->maxRec),
                             VDR_NULL),&pStatus)) return pStatus;
        }
      }
      Var->firstRecInVVR = NO_RECORD;
      Var->lastRecInVVR = NO_RECORD;
      Var->offsetOfVVR64 = (OFF_T) NO_OFFSET64;
      break;
    }
    /**************************************************************************
    * Compressed records.
    **************************************************************************/
    case COMPRESSED_: {
      Int32 aboveRecord, deleteCount;
      OFF_T vxrHead;
      /************************************************************************
      * Flush/clear staging area first.
      ************************************************************************/
      if (!sX(FlushStage64(CDF,Var),&pStatus)) return pStatus;
      Var->stage.firstRec = NO_RECORD;
      Var->stage.lastRec = NO_RECORD;
      Var->stage.dotOffset64 = (OFF_T) NO_OFFSET64;
      Var->firstRecInVVR = NO_RECORD;
      Var->lastRecInVVR = NO_RECORD;
      Var->offsetOfVVR64 = (OFF_T) NO_OFFSET64;
      /************************************************************************
      * Verify/adjust the records to be deleted.
      ************************************************************************/
      if (firstRec > Var->maxRec) return pStatus;
      lastRec = MINIMUM(lastRec,Var->maxRec);
      /************************************************************************
      * Delete the records.
      ************************************************************************/
      for (recNum = firstRec; recNum <= lastRec; recNum = deletedTo + 1) {
	 if (!sX(DeleteRecords64(CDF,Var,recNum,
			         lastRec,&deletedTo),&pStatus)) return pStatus;
      }
      deleteCount = lastRec - firstRec + 1;
      /************************************************************************
      * Update the index entries.
      ************************************************************************/
      aboveRecord = firstRec - 1;
      if (!sX(ReadVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
		        VDR_VXRHEAD,&vxrHead,
		        VDR_NULL),&pStatus)) return pStatus;
      if (!sX(UpdateIndexEntries_r_64(CDF->fp,vxrHead,aboveRecord,deleteCount),
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
	if (!sX(WriteVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
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
      if (!sX(FlushStage64(CDF,Var),&pStatus)) return pStatus;
      Var->stage.firstRec = NO_RECORD;
      Var->stage.lastRec = NO_RECORD;
      Var->stage.dotOffset64 = (OFF_T) NO_OFFSET64;
      Var->firstRecInVVR = NO_RECORD;
      Var->lastRecInVVR = NO_RECORD;
      Var->offsetOfVVR64 = (OFF_T) NO_OFFSET64;
      /************************************************************************
      * Verify/adjust the records to be deleted.
      ************************************************************************/
      if (firstRec > Var->maxRec) return pStatus;
      lastRec = MINIMUM(lastRec,Var->maxRec);
      /************************************************************************
      * Delete the records.
      ************************************************************************/
      for (recNum = firstRec; recNum <= lastRec; recNum = deletedTo + 1) {
	 if (!sX(DeleteRecords64(CDF,Var,recNum,
			         lastRec,&deletedTo),&pStatus)) return pStatus;
      }
      /************************************************************************
      * Update the maximum record field in the VDR.
      ************************************************************************/
      if (INCLUSIVE(firstRec,Var->maxRec,lastRec)) {
	if (!sX(PrevRecord64(CDF,Var->VDRoffset64,Var->zVar,
			     (Int32)(firstRec - 1),
			     &(Var->maxRec),&found),&pStatus)) return pStatus;
	if (!found) Var->maxRec = NO_RECORD;
	if (!sX(WriteVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
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
  if (!sX(UpdateVXRtailInVDR64(CDF,Var),&pStatus)) return pStatus;
  /****************************************************************************
  * Update the maximum record field in the GDR if an rVariable.  This is
  * done in case this rVariable had the maximum record number.
  ****************************************************************************/
  if (!Var->zVar) {
    Int32 maxRec; Logical zVar = FALSE;
    OFF_T vdrOffset;
    CDF->rMaxRec = NO_RECORD;
    if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
		      GDR_rVDRHEAD,&vdrOffset,
		      GDR_NULL),&pStatus)) return pStatus;
    while (vdrOffset != (OFF_T) ZERO_OFFSET64) {
      if (!sX(ReadVDR64(CDF,CDF->fp,vdrOffset,zVar,
		        VDR_MAXREC,&maxRec,
		        VDR_VDRNEXT,&vdrOffset,
		        VDR_NULL),&pStatus)) return pStatus;
      CDF->rMaxRec = MAXIMUM(CDF->rMaxRec,maxRec);
    }
    if (!sX(WriteGDR64(CDF->fp,CDF->GDRoffset64,
		       GDR_rMAXREC,&(CDF->rMaxRec),
		       GDR_NULL),&pStatus)) return pStatus;
  }
  return pStatus;
}

/******************************************************************************
* DeleteRecords64.
******************************************************************************/

static CDFstatus DeleteRecords64 (CDF, Var, firstRec, lastRec, deletedTo)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 firstRec;
Int32 lastRec;
Int32 *deletedTo;
{
  CDFstatus pStatus = CDF_OK; Logical total = FALSE; OFF_T vxrOffset;
  /****************************************************************************
  * Read offset of the first VXR...
  ****************************************************************************/
  if (!sX(ReadVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
		    VDR_VXRHEAD,&vxrOffset,
		    VDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * ...and start there.
  ****************************************************************************/
  if (!sX(DeleteRecords_r_64(CDF,Var,vxrOffset,firstRec,
			     lastRec,deletedTo,&total),&pStatus)) return pStatus;
  /****************************************************************************
  * If the top level of VXR(s) was totally deleted, set the VXR head/tail to
  * ZERO_OFFSET.
  ****************************************************************************/
  if (total) {
    OFF_T zeroOffset = (OFF_T) ZERO_OFFSET64;
    if (!sX(WriteVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
		       VDR_VXRHEAD,&zeroOffset,
		       VDR_VXRTAIL,&zeroOffset,
		       VDR_NULL),&pStatus)) return pStatus;
  }
  return pStatus;
}

/******************************************************************************
* DeleteRecords_r_64.
******************************************************************************/

static CDFstatus DeleteRecords_r_64 (CDF, Var, firstVXRoffset, firstRec, lastRec,
				     deletedTo, total)
struct CDFstruct *CDF;
struct VarStruct *Var;
OFF_T firstVXRoffset;
Int32 firstRec;
Int32 lastRec;
Int32 *deletedTo;
Logical *total;
{
  CDFstatus pStatus = CDF_OK; OFF_T vxrOffset = firstVXRoffset;
  struct VXRstruct64 VXR; int entryN;
  /****************************************************************************
  * While another VXR...
  ****************************************************************************/
  while (vxrOffset != (OFF_T) ZERO_OFFSET64) {
    /**************************************************************************
    * Read the VXR.
    **************************************************************************/
    if (!sX(ReadVXR64(CDF->fp,vxrOffset,
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
	 if (!sX(ReadIrType64(CDF->fp,
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
	     if (!sX(DeleteRecords_r_64(CDF,Var,VXR.Offset[entryN],
				        first,last,deletedTo,
				        &totalBelow),&pStatus)) return pStatus;
	     /*****************************************************************
	     * If the branch of VXRs below has not been totally deleted, update
	     * this entry for the records that remain and return.
	     *****************************************************************/
	     if (!totalBelow) {
	       struct VXRstruct64 VXRbelow;
	       if (!sX(ReadVXR64(CDF->fp,VXR.Offset[entryN],
			         VXR_RECORD,&VXRbelow,
			         VXR_NULL),&pStatus)) return pStatus;
	       VXR.First[entryN] = VXRbelow.First[0];
	       while (VXRbelow.VXRnext != (OFF_T) ZERO_OFFSET64) {
		 if (!sX(ReadVXR64(CDF->fp,VXRbelow.VXRnext,
				   VXR_RECORD,&VXRbelow,
				   VXR_NULL),&pStatus)) return pStatus;
	       }
	       VXR.Last[entryN] = VXRbelow.Last[(int)VXRbelow.NusedEntries-1];
	       if (!sX(WriteVXR64(CDF->fp,vxrOffset,
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
	     if (!sX(DeleteVXRentry64(CDF,firstVXRoffset,
				      vxrOffset,entryN,total),&pStatus)) return
								       pStatus;
	     return pStatus;
	   }
	   /*******************************************************************
	   * A VVR or CVVR...
	   *******************************************************************/
	   case VVR_:
	   case CVVR_: {
	     OFF_T irSize;
	     /*****************************************************************
	     * Read the size of the CVVR/VVR.
	     *****************************************************************/
	     if (!sX(ReadIrSize64(CDF->fp,
				  VXR.Offset[entryN],
				  &irSize),&pStatus)) return pStatus;
	     /*****************************************************************
	     * Based on which records in the CVVR/VVR are being deleted...
	     *****************************************************************/
	     if (first > VXR.First[entryN]) {
	       if (last < VXR.Last[entryN]) {
		 if (!sX(DeleteFromMiddle64(CDF,Var,first,last,
					    vxrOffset,&VXR,
					    entryN,irSize),&pStatus)) return
								    pStatus;
	       }
	       else {
		 if (!sX(DeleteFromEnd64(CDF,Var,first,
				         vxrOffset,&VXR,
				         entryN,irSize),&pStatus)) return
								 pStatus;
	       }
	     }
	     else {
	       if (last < VXR.Last[entryN]) {
		 if (!sX(DeleteFromFront64(CDF,Var,last,
					   vxrOffset,&VXR,
					   entryN,irSize),&pStatus)) return
								   pStatus;
	       }
	       else {
		 /*************************************************************
		 * The entire block of records (ie. the entire CVVR/VVR) is
		 * being deleted.
		 *************************************************************/
		 if (!sX(WasteIR64(CDF,VXR.Offset[entryN],
				   irSize),&pStatus)) return pStatus;
		 if (!sX(DeleteVXRentry64(CDF,firstVXRoffset,
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
	     return CORRUPTED_V3_CDF;
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

static CDFstatus DeleteFromFront64 (CDF, Var, last, vxrOffset, VXR, entryN,
				    irSize)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 last;
OFF_T vxrOffset;
struct VXRstruct64 *VXR;
int entryN;
OFF_T irSize;
{
  CDFstatus pStatus = CDF_OK;
  switch (Var->vType) {
    case STANDARD_:
    case SPARSE_RECORDS_: {
      Int32 nDeleteRecords; OFF_T nRemainingBytes;
      Int32 nDeleteBytes, nRecords2, nBytes2;
      OFF_T deleteOffset, offset2, recordSize;
      /************************************************************************
      * The records being deleted are at the beginning of the VVR.  First
      * calculate needed offsets and sizes.
      ************************************************************************/
      deleteOffset = VXR->Offset[entryN] + VVR_BASE_SIZE64;
      nDeleteRecords = last - VXR->First[entryN] + 1;
      nDeleteBytes = nDeleteRecords * Var->NphyRecBytes;
      nRecords2 = VXR->Last[entryN] - last;
      nBytes2 = nRecords2 * Var->NphyRecBytes;
      offset2 = VXR->Offset[entryN] + VVR_BASE_SIZE64 + nDeleteBytes;
      nRemainingBytes = irSize - VVR_BASE_SIZE64 - nBytes2;
      /************************************************************************
      * Slide the remaining records to the beginning of the VVR.
      ************************************************************************/
      if (!sX(CopyBytes64(CDF->fp,offset2,
			  CDF_READ_ERROR,
			  nBytes2,CDF->fp,
			  deleteOffset,
			  CDF_WRITE_ERROR),&pStatus)) return pStatus;
      /************************************************************************
      * Update the VXR entry.
      ************************************************************************/
      VXR->First[entryN] = last + 1;
      if (!sX(WriteVXR64(CDF->fp,vxrOffset,
		         VXR_RECORD,VXR,
		         VXR_NULL),&pStatus)) return pStatus;
      /************************************************************************
      * If possible, waste the unused bytes that are now at the end of the
      * VVR (and update the record size field).  Otherwise, the record size
      * field of the VVR remains unchanged.
      ************************************************************************/
      if (nRemainingBytes >= UIR_BASE_SIZE64) {
	OFF_T offset = deleteOffset + nBytes2;
	if (!sX(WasteIR64(CDF,offset,
			  nRemainingBytes),&pStatus)) return pStatus;
	recordSize = VVR_BASE_SIZE64 + nBytes2;
	if (!sX(WriteVVR64(CDF->fp,VXR->Offset[entryN],
			   VVR_RECORDSIZE,&recordSize,
			   VVR_NULL),&pStatus)) return pStatus;
      }
      break;
    }
    case COMPRESSED_:
    case SPARSE_COMPRESSED_RECORDS_: {
      Int32 nRecords = VXR->Last[entryN] - VXR->First[entryN] + 1;
      OFF_T uSize = nRecords * Var->NphyRecBytes;
      Int32 firstRecord2, nRecords2; OFF_T cSize;
      OFF_T offset2, newOffset, nBytes2;
      /************************************************************************
      * Initialize staging area and scratch space.
      ************************************************************************/
      if (Var->stage.areaOffset64 == (OFF_T) NO_OFFSET64) {
	Int32 nBytes = Var->blockingFactor * Var->NphyRecBytes;
	if (!sX(InitVarStage64(CDF,Var,nBytes),&pStatus)) return pStatus;
      }
      if (!sX(InitScratch64(ScratchDirectory(CDF),
			    &(CDF->compressFp),
			    CDF->compressCacheSize),&pStatus)) return pStatus;
      /************************************************************************
      * Bring the CVVR/VVR to the staging area.
      ************************************************************************/
      if (!sX(DecompressToStage64(CDF,Var,
				  VXR->Offset[entryN],
				  uSize),&pStatus)) return pStatus;
      /************************************************************************
      * Waste the CVVR/VVR.
      ************************************************************************/
      if (!sX(WasteIR64(CDF,VXR->Offset[entryN],irSize),&pStatus)) return
								 pStatus;
      /************************************************************************
      * Calculate sizes/offsets for the block of remaining records.
      ************************************************************************/
      firstRecord2 = last + 1;
      nRecords2 = VXR->Last[entryN] - last;
      if (nRecords2 <= 0) break;
      offset2 = Var->stage.areaOffset64 +
		(Var->NphyRecBytes * (firstRecord2 - VXR->First[entryN]));
      nBytes2 = nRecords2 * Var->NphyRecBytes;
      /************************************************************************
      * Compress the block of remaining records...
      ************************************************************************/
      if (!sX(Compress64(CDF->stage.fp,offset2,
		         nBytes2,SCRATCH_READ_ERROR,
		         Var->cType,Var->cParms,
		         CDF->compressFp,(OFF_T) ZERO_OFFSET64,
		         &cSize,SCRATCH_WRITE_ERROR),&pStatus)) return pStatus;
      /************************************************************************
      * ...and allocate/write the CVVR/VVR.
      ************************************************************************/
      if (!sX(WriteCVVRorVVR64(CDF,cSize,offset2,
			       nBytes2,&newOffset),&pStatus)) return pStatus;
      /************************************************************************
      * Update the index entry for the block of remaining records.
      ************************************************************************/
      VXR->First[entryN] = firstRecord2;
      VXR->Offset[entryN] = newOffset;
      if (!sX(WriteVXR64(CDF->fp,vxrOffset,
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
* DeleteFromMiddle64.
******************************************************************************/

static CDFstatus DeleteFromMiddle64 (CDF, Var, first, last, vxrOffset, VXR,
				     entryN, irSize)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 first;
Int32 last;
OFF_T vxrOffset;
struct VXRstruct64 *VXR;
int entryN;
OFF_T irSize;
{
  CDFstatus pStatus = CDF_OK;
  switch (Var->vType) {
    case STANDARD_:
    case SPARSE_RECORDS_: {
      Int32 nRecords1, nBytes1, nDeleteRecords;
      Int32 nDeleteBytes, firstRecord2, lastRecord2, nRecords2;
      Int32 nBytes2, nExtraBytes, vvr_ = VVR_;
      OFF_T nRemainingBytes;
      OFF_T deleteOffset, offset2, vvrOffset, recordSize;
      /************************************************************************
      * A block of records will remain at the beginning and at the end.  First
      * calculate needed offsets and sizes.  Note that the number of bytes in
      * the second remaining block of records takes into account any extra
      * bytes that may exist at the end of the VVR.
      ************************************************************************/
      nRecords1 = first - VXR->First[entryN];
      nBytes1 = nRecords1 * Var->NphyRecBytes;
      deleteOffset = VXR->Offset[entryN] + VVR_BASE_SIZE64 + nBytes1;
      nDeleteRecords = last - first + 1;
      nDeleteBytes = nDeleteRecords * Var->NphyRecBytes;
      firstRecord2 = last + 1;
      lastRecord2 = VXR->Last[entryN];
      nRecords2 = lastRecord2 - firstRecord2 + 1;
      offset2 = deleteOffset + nDeleteBytes;
      nBytes2 = nRecords2 * Var->NphyRecBytes;
      nExtraBytes = (Int32) (irSize - VVR_BASE_SIZE64 - nBytes1 - nDeleteBytes 
		             - nBytes2);
      /************************************************************************
      * If the second block of remaining records cannot remain in the same
      * place...
      ************************************************************************/
      if (nDeleteBytes < VVR_BASE_SIZE64) {
	/**********************************************************************
	* Move the second block of remaining records to a new VVR.
	**********************************************************************/
	recordSize = VVR_BASE_SIZE64 + nBytes2;
	if (!sX(AllocateIR64(CDF,recordSize,&vvrOffset),&pStatus)) return
								 pStatus;
	if (!sX(WriteVVR64(CDF->fp,vvrOffset,
			   VVR_RECORDSIZE,&recordSize,
			   VVR_RECORDTYPE,&vvr_,
			   VVR_NULL),&pStatus)) return pStatus;
	if (!sX(CopyBytes64(CDF->fp,offset2,
			    CDF_READ_ERROR,
			    nBytes2,CDF->fp,
			    (vvrOffset + VVR_BASE_SIZE64),
			    CDF_WRITE_ERROR),&pStatus)) return pStatus;
	/**********************************************************************
	* If the remaining bytes can be changed to a UIR...
	**********************************************************************/
	nRemainingBytes = (OFF_T) nDeleteBytes + nBytes2 + nExtraBytes;
	if (nRemainingBytes >= UIR_BASE_SIZE64) {
	  recordSize = VVR_BASE_SIZE64 + nBytes1;
	  if (!sX(WriteVVR64(CDF->fp,VXR->Offset[entryN],
			     VVR_RECORDSIZE,&recordSize,
			     VVR_NULL),&pStatus)) return pStatus;
	  if (!sX(WasteIR64(CDF,deleteOffset,
			    nRemainingBytes),&pStatus)) return pStatus;
	}
	/**********************************************************************
	* Update the VXR entry for the first block of remaining records.
	**********************************************************************/
	VXR->Last[entryN] = first - 1;
	if (!sX(WriteVXR64(CDF->fp,vxrOffset,
			   VXR_RECORD,VXR,
			   VXR_NULL),&pStatus)) return pStatus;
	/**********************************************************************
	* Insert a VXR entry for the new VVR created for the second block of
	* remaining records.
	**********************************************************************/
	if (!sX(InsertIndexEntry64(CDF,vxrOffset,
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
	nRemainingBytes = (OFF_T) nDeleteBytes - VVR_BASE_SIZE64;
	if (nRemainingBytes < UIR_BASE_SIZE64)
	  recordSize = VVR_BASE_SIZE64 + nBytes1 + nRemainingBytes;
	else {
	  recordSize = VVR_BASE_SIZE64 + nBytes1;
	  if (!sX(WasteIR64(CDF,deleteOffset,
			    nRemainingBytes),&pStatus)) return pStatus;
	}
	if (!sX(WriteVVR64(CDF->fp,VXR->Offset[entryN],
			   VVR_RECORDSIZE,&recordSize,
			   VVR_NULL),&pStatus)) return pStatus;
	/**********************************************************************
	* Update the VXR entry for the first block of remaining records.
	**********************************************************************/
	VXR->Last[entryN] = first - 1;
	if (!sX(WriteVXR64(CDF->fp,vxrOffset,
			   VXR_RECORD,VXR,
			   VXR_NULL),&pStatus)) return pStatus;
	/**********************************************************************
	* Write the record size and type fields of the new VVR for the second
	* block of remaining records.
	**********************************************************************/
	vvrOffset = offset2 - VVR_BASE_SIZE64;
	recordSize = VVR_BASE_SIZE64 + nBytes2 + nExtraBytes;
	if (!sX(WriteVVR64(CDF->fp,vvrOffset,
			   VVR_RECORDSIZE,&recordSize,
			   VVR_RECORDTYPE,&vvr_,
			   VVR_NULL),&pStatus)) return pStatus;
	/**********************************************************************
	* Insert a VXR entry for the new VVR created for the second block of
	* remaining records.
	**********************************************************************/
	if (!sX(InsertIndexEntry64(CDF,vxrOffset,
				   entryN,(Logical)TRUE,
				   firstRecord2,lastRecord2,
				   vvrOffset),&pStatus)) return pStatus;
      }
      break;
    }
    case COMPRESSED_:
    case SPARSE_COMPRESSED_RECORDS_: {
      Int32 nRecords = VXR->Last[entryN] - VXR->First[entryN] + 1;
      OFF_T uSize = nRecords * Var->NphyRecBytes;
      Int32 nRecords1, firstRecord2, lastRecord2, nRecords2;
      OFF_T offset2, nBytes1, nBytes2, cSize, newOffset;
      /************************************************************************
      * Initialize staging area and scratch space.
      ************************************************************************/
      if (Var->stage.areaOffset64 == (OFF_T) NO_OFFSET64) {
	Int32 nBytes = Var->blockingFactor * Var->NphyRecBytes;
	if (!sX(InitVarStage64(CDF,Var,nBytes),&pStatus)) return pStatus;
      }
      if (!sX(InitScratch64(ScratchDirectory(CDF),
			    &(CDF->compressFp),
			    CDF->compressCacheSize),&pStatus)) return pStatus;
      /************************************************************************
      * Bring the CVVR/VVR to the staging area.
      ************************************************************************/
      if (!sX(DecompressToStage64(CDF,Var,
				  VXR->Offset[entryN],
				  uSize),&pStatus)) return pStatus;
      /************************************************************************
      * Waste the CVVR/VVR.
      ************************************************************************/
      if (!sX(WasteIR64(CDF,VXR->Offset[entryN],irSize),&pStatus)) return
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
      offset2 = Var->stage.areaOffset64 +
		(Var->NphyRecBytes * (firstRecord2 - VXR->First[entryN]));
      nBytes2 = nRecords2 * Var->NphyRecBytes;
      /************************************************************************
      * Compress the first block of remaining records...
      ************************************************************************/
      if (!sX(Compress64(CDF->stage.fp,
		         Var->stage.areaOffset64,
		         nBytes1,SCRATCH_READ_ERROR,
		         Var->cType,Var->cParms,
		         CDF->compressFp,(OFF_T) ZERO_OFFSET64,
		         &cSize,SCRATCH_WRITE_ERROR),&pStatus)) return pStatus;
      /************************************************************************
      * ...and allocate/write the CVVR/VVR.
      ************************************************************************/
      if (!sX(WriteCVVRorVVR64(CDF,cSize,
		  	       Var->stage.areaOffset64,
			       nBytes1,&newOffset),&pStatus)) return pStatus;
      /************************************************************************
      * Update the index entry for the first block of remaining records.
      ************************************************************************/
      VXR->Last[entryN] = first - 1;
      VXR->Offset[entryN] = newOffset;
      if (!sX(WriteVXR64(CDF->fp,vxrOffset,
		         VXR_RECORD,VXR,
		         VXR_NULL),&pStatus)) return pStatus;
      /************************************************************************
      * Compress the second block of remaining records...
      ************************************************************************/
      if (!sX(Compress64(CDF->stage.fp,offset2,
		         nBytes2,SCRATCH_READ_ERROR,
		         Var->cType,Var->cParms,
		         CDF->compressFp,(OFF_T) ZERO_OFFSET64,
		         &cSize,SCRATCH_WRITE_ERROR),&pStatus)) return pStatus;
      /************************************************************************
      * ...and allocate/write the CVVR/VVR.
      ************************************************************************/
      if (!sX(WriteCVVRorVVR64(CDF,cSize,offset2,
			       nBytes2,&newOffset),&pStatus)) return pStatus;
      /************************************************************************
      * Insert a new index entry for the second block of remaining records.
      ************************************************************************/
      if (!sX(InsertIndexEntry64(CDF,vxrOffset,
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
* DeleteFromEnd64.
******************************************************************************/

static CDFstatus DeleteFromEnd64 (CDF, Var, first, vxrOffset, VXR, entryN,
				  irSize)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 first;
OFF_T vxrOffset;
struct VXRstruct64 *VXR;
int entryN;
OFF_T irSize;
{
  CDFstatus pStatus = CDF_OK;
  switch (Var->vType) {
    case STANDARD_:
    case SPARSE_RECORDS_: {
      Int32 nRecords1, nBytes1;
      OFF_T nRemainingBytes, deleteOffset, recordSize;
      /************************************************************************
      * First calculate needed offsets and sizes.
      ************************************************************************/
      nRecords1 = first - VXR->First[entryN];
      nBytes1 = nRecords1 * Var->NphyRecBytes;
      deleteOffset = VXR->Offset[entryN] + VVR_BASE_SIZE64 + nBytes1;
      nRemainingBytes = irSize - VVR_BASE_SIZE64 - nBytes1;
      /************************************************************************
      * Update the VXR entry.
      ************************************************************************/
      VXR->Last[entryN] = first - 1;
      if (!sX(WriteVXR64(CDF->fp,vxrOffset,
		         VXR_RECORD,VXR,
		         VXR_NULL),&pStatus)) return pStatus;
      /************************************************************************
      * If enough bytes are being deleted for a UIR, waste them and update
      * the record size field of the VVR.
      ************************************************************************/
      if (nRemainingBytes >= UIR_BASE_SIZE64) {
	if (!sX(WasteIR64(CDF,deleteOffset,
			  nRemainingBytes),&pStatus)) return pStatus;
	recordSize = VVR_BASE_SIZE64 + nBytes1;
	if (!sX(WriteVVR64(CDF->fp,VXR->Offset[entryN],
			   VVR_RECORDSIZE,&recordSize,
			   VVR_NULL),&pStatus)) return pStatus;
      }
      break;
    }
    case COMPRESSED_:
    case SPARSE_COMPRESSED_RECORDS_: {
      Int32 nRecords = VXR->Last[entryN] - VXR->First[entryN] + 1;
      OFF_T uSize = nRecords * Var->NphyRecBytes;
      Int32 nRecords1; OFF_T nBytes1, cSize;
      OFF_T newOffset;
      /************************************************************************
      * Initialize staging area and scratch space.
      ************************************************************************/
      if (Var->stage.areaOffset64 == (OFF_T) NO_OFFSET64) {
	Int32 nBytes = Var->blockingFactor * Var->NphyRecBytes;
	if (!sX(InitVarStage64(CDF,Var,nBytes),&pStatus)) return pStatus;
      }
      if (!sX(InitScratch64(ScratchDirectory(CDF),
			    &(CDF->compressFp),
			    CDF->compressCacheSize),&pStatus)) return pStatus;
      /************************************************************************
      * Bring the CVVR/VVR to the staging area.
      ************************************************************************/
      if (!sX(DecompressToStage64(CDF,Var,
				  VXR->Offset[entryN],
				  uSize),&pStatus)) return pStatus;
      /************************************************************************
      * Waste the CVVR/VVR.
      ************************************************************************/
      if (!sX(WasteIR64(CDF,VXR->Offset[entryN],irSize),&pStatus)) return
								 pStatus;
      /************************************************************************
      * Calculate sizes/offsets for the block of remaining records.
      ************************************************************************/
      nRecords1 = first - VXR->First[entryN];
      nBytes1 = nRecords1 * Var->NphyRecBytes;
      /************************************************************************
      * Compress the block of remaining records...
      ************************************************************************/
      if (!sX(Compress64(CDF->stage.fp,
		         Var->stage.areaOffset64,
		         nBytes1,SCRATCH_READ_ERROR,
		         Var->cType,Var->cParms,
		         CDF->compressFp,(OFF_T) ZERO_OFFSET64,
		         &cSize,SCRATCH_WRITE_ERROR),&pStatus)) return pStatus;
      /************************************************************************
      * ...and allocate/write the CVVR/VVR.
      ************************************************************************/
      if (!sX(WriteCVVRorVVR64(CDF,cSize,
			       Var->stage.areaOffset64,
			       nBytes1,&newOffset),&pStatus)) return pStatus;
      /************************************************************************
      * Update the index entry for the block of remaining records.
      ************************************************************************/
      VXR->Last[entryN] = first - 1;
      VXR->Offset[entryN] = newOffset;
      if (!sX(WriteVXR64(CDF->fp,vxrOffset,
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
* DeleteVXRentry64.
******************************************************************************/

static CDFstatus DeleteVXRentry64 (CDF, firstVXRoffset, delVXRoffset, delEntryN,
				   total)
struct CDFstruct *CDF;
OFF_T firstVXRoffset;
OFF_T delVXRoffset;
int delEntryN;
Logical *total;
{
  CDFstatus pStatus = CDF_OK;
  OFF_T vxrOffset = firstVXRoffset, prevVXRoffset = (OFF_T) ZERO_OFFSET64;
  struct VXRstruct64 VXR, nextVXR; int entryN, lastEntryN;
  /****************************************************************************
  * Find the VXR in which an entry is being deleted while keeping track of the
  * previous VXR.
  ****************************************************************************/
  if (!sX(ReadVXR64(CDF->fp,vxrOffset,
		    VXR_RECORD,&VXR,
		    VXR_NULL),&pStatus)) return pStatus;
  while (vxrOffset != delVXRoffset) {
    prevVXRoffset = vxrOffset;
    vxrOffset = VXR.VXRnext;
    if (!sX(ReadVXR64(CDF->fp,vxrOffset,
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
     if (VXR.VXRnext == (OFF_T) ZERO_OFFSET64) {
       if (VXR.NusedEntries > 1) {
	 VXR.First[lastEntryN] = NO_RECORD;
	 VXR.Last[lastEntryN] = NO_RECORD;
	 VXR.Offset[lastEntryN] = (OFF_T) NO_OFFSET64;
	 VXR.NusedEntries--;
	 if (!sX(WriteVXR64(CDF->fp,vxrOffset,
			    VXR_RECORD,&VXR,
			    VXR_NULL),&pStatus)) return pStatus;
       }
       else {
	 if (!sX(WasteIR64(CDF,vxrOffset,
			   VXR.RecordSize),&pStatus)) return pStatus;
	 if (prevVXRoffset != (OFF_T) ZERO_OFFSET64) {
	   OFF_T zeroOffset = (OFF_T) ZERO_OFFSET64;
	   if (!sX(WriteVXR64(CDF->fp,prevVXRoffset,
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
     if (!sX(ReadVXR64(CDF->fp,VXR.VXRnext,
		       VXR_RECORD,&nextVXR,
		       VXR_NULL),&pStatus)) return pStatus;
     VXR.First[lastEntryN] = nextVXR.First[0];
     VXR.Last[lastEntryN] = nextVXR.Last[0];
     VXR.Offset[lastEntryN] = nextVXR.Offset[0];
     if (!sX(WriteVXR64(CDF->fp,vxrOffset,
		        VXR_RECORD,&VXR,
		        VXR_NULL),&pStatus)) return pStatus;
     prevVXRoffset = vxrOffset;
     vxrOffset = VXR.VXRnext;
     VXR = nextVXR;
     delEntryN = 0;
  }
}

/******************************************************************************
* InsertIndexEntry64.
******************************************************************************/

static CDFstatus InsertIndexEntry64 (CDF, vxrOffset, entryN, after, first, last,
				     offset)
struct CDFstruct *CDF;
OFF_T vxrOffset;
int entryN;
Logical after;          /* TRUE: Insert the new entry after `entryN'.
			   FALSE: Insert the new entry at `entryN'. */
Int32 first;
Int32 last;
OFF_T offset;
{
  CDFstatus pStatus = CDF_OK; struct VXRstruct64 VXR; int eN, lastEntryN;
  Int32 pushedFirst, pushedLast;
  OFF_T pushedOffset, newVXRoffset;
  /****************************************************************************
  * Read the VXR.
  ****************************************************************************/
  if (!sX(ReadVXR64(CDF->fp,vxrOffset,
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
    if (!sX(WriteVXR64(CDF->fp,vxrOffset,
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
  if (VXR.VXRnext == (OFF_T) ZERO_OFFSET64) {
    if (!sX(AllocateIR64(CDF,(OFF_T) VXR_BASE_SIZE64,
		         &newVXRoffset),&pStatus)) return pStatus;
    VXR.VXRnext = newVXRoffset;
    if (!sX(WriteVXR64(CDF->fp,vxrOffset,
		       VXR_RECORD,&VXR,
		       VXR_NULL),&pStatus)) return pStatus;
    InitNewVXR64 (&VXR, pushedFirst, pushedLast, pushedOffset);
    if (!sX(WriteVXR64(CDF->fp,newVXRoffset,
		       VXR_RECORD,&VXR,
		       VXR_NULL),&pStatus)) return pStatus;
    return pStatus;
  }
  /****************************************************************************
  * There is another VXR on the linked list - update this VXR and recursively
  * call this routine to insert the entry that was pushed out in the first
  * entry position.
  ****************************************************************************/
  if (!sX(WriteVXR64(CDF->fp,vxrOffset,
		     VXR_RECORD,&VXR,
		     VXR_NULL),&pStatus)) return pStatus;
  if (!sX(InsertIndexEntry64(CDF,VXR.VXRnext,
			     0,(Logical)FALSE,
			     pushedFirst,pushedLast,
			     pushedOffset),&pStatus)) return pStatus;
  return pStatus;
}

/******************************************************************************
* UpdateIndexEntries_r_64.
******************************************************************************/

static CDFstatus UpdateIndexEntries_r_64 (fp, vxrOffset, aboveRecord, recordCount)
vFILE *fp;
OFF_T vxrOffset;
Int32 aboveRecord;
Int32 recordCount;
{
  CDFstatus pStatus = CDF_OK; struct VXRstruct64 VXR; int entryN; Int32 irType;
  while (vxrOffset != (OFF_T) ZERO_OFFSET64) {
    Logical modified = FALSE;
    if (!sX(ReadVXR64(fp,vxrOffset,
		      VXR_RECORD,&VXR,
		      VXR_NULL),&pStatus)) return pStatus;
    for (entryN = 0; entryN < VXR.NusedEntries; entryN++) {
       if (VXR.Last[entryN] > aboveRecord) {
	 if (!sX(ReadIrType64(fp,VXR.Offset[entryN],
			      &irType),&pStatus)) return pStatus;
	 switch (irType) {
	   case VXR_:
	     if (!sX(UpdateIndexEntries_r_64(fp,VXR.Offset[entryN],aboveRecord,
					     recordCount),&pStatus))
		return pStatus;
	     break;
	   case VVR_:
	   case CVVR_:
	     break;
	   default:
	     return CORRUPTED_V3_CDF;
	 }
	 if (VXR.First[entryN] > aboveRecord) VXR.First[entryN] -= recordCount;
         VXR.Last[entryN] -= recordCount;
	 modified = TRUE;
       }
    }
    if (modified) {
      if (!sX(WriteVXR64(fp,vxrOffset,
		         VXR_RECORD,&VXR,
		         VXR_NULL),&pStatus)) return pStatus;
    }
    vxrOffset = VXR.VXRnext;
  }
  return pStatus;
}

/******************************************************************************
* WriteCVVRorVVR64.
******************************************************************************/

static CDFstatus WriteCVVRorVVR64 (CDF, cSize, stageOffset, uSize, newOffset)
struct CDFstruct *CDF;
OFF_T cSize;
OFF_T stageOffset;
OFF_T uSize;
OFF_T *newOffset;
{
  CDFstatus pStatus = CDF_OK; OFF_T recordSize; OFF_T tOffset;
  if (CVVR_BASE_SIZE64 + cSize < VVR_BASE_SIZE64 + uSize) {
    struct CVVRstruct64 CVVR;
    recordSize = CVVR_BASE_SIZE64 + cSize;
    LoadCVVRx64 (CVVR, recordSize, cSize)
    if (!sX(AllocateIR64(CDF,recordSize,newOffset),&pStatus)) return pStatus;
    if (!sX(WriteCVVR64(CDF->fp,*newOffset,
		        CVVR_RECORDx,&CVVR,
		        CVVR_NULL),&pStatus)) return pStatus;
    tOffset = *newOffset + CVVR_BASE_SIZE64;
    if (!sX(CopyBytes64(CDF->compressFp,(OFF_T) ZERO_OFFSET64,
		        SCRATCH_READ_ERROR,cSize,CDF->fp,
		        tOffset,CDF_WRITE_ERROR),&pStatus)) return pStatus;
  }
  else {
    struct VVRstruct64 VVR;
    recordSize = VVR_BASE_SIZE64 + uSize;
    LoadVVRx64 (VVR, recordSize)
    if (!sX(AllocateIR64(CDF,recordSize,newOffset),&pStatus)) return pStatus;
    if (!sX(WriteVVR64(CDF->fp,*newOffset,
		       VVR_RECORDx,&VVR,
		       VVR_NULL),&pStatus)) return pStatus;
    tOffset = *newOffset + VVR_BASE_SIZE64;
    if (!sX(CopyBytes64(CDF->stage.fp,stageOffset,
		        SCRATCH_READ_ERROR,uSize,CDF->fp,
		        tOffset,CDF_WRITE_ERROR),&pStatus)) return pStatus;
  }
  return pStatus;
}
