/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                     CDF library miscellaneous functions, part 2.
*
*  Version 1.3b, 4-Mar-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  15-Dec-94, J Love     Original version.
*   V1.1   6-Jan-95, J Love	Encode/decode changes.  More cache-residency.
*   V1.1a 20-Jan-95, J Love	IRIX 6.0 (64-bit).
*   V1.1b 15-Mar-95, J Love	Solaris 2.3 IDL i/f.  Fixed `recNum' parameter
*				of `LastAllocatedRecord'.  Gnu C on OSF/1.
*   V1.2  21-Mar-95, J Love	POSIX.
*   V1.2a 18-Apr-95, J Love	More POSIX.
*   V1.2b 13-Jun-95, J Love	Linux.
*   V1.2c  7-Sep-95, J Love	CDFexport-related changes.  Fixed possible
*				memory leak.
*   V1.3   5-Sep-96, J Love	CDF V2.6.
*   V1.3a 21-Feb-97, J Love	Removed RICE.
*   V1.3b  4-Mar-97, J Love	Windows NT for MS Visual C/C++ on an IBM PC.
*   V2.0  08-Apr-04, M Liu      Modified LocateCurrentVar function to save
*                               the current selected variable's offset.
*                               Modified FindVarByNumber and FindVarByName
*                               functions to start searching for the variable
*                               from the current selected variable, if it's
*                               cwselected, instead of always from the 
*                               beginning. Remove the calls to FindVarByNumber 
*                               if a variable is already selected.
*   V2.1  04-May-04, M Liu      Corrected Int32ToCDFid and CDFidToInt32 to 
*                               handle 64-bit for Solaris/sparc.
*   V2.2  21-Jun-05, M Liu      Corrected Int32ToCDFid and CDFidToInt32 to
*                               handle 64-bit Intel/AMD running Linux.
*   V3.2  20-Jun-07, D Berger   Modified the "FindAttr...", "FindEntry..."
*                               routines, and the "SetCUR..." routines and 
*                               added ResetReadOnlyState to handle READONLYon 
*                               mode.
*   V3.5  09-Aug-13, M Liu      Modified CDFidToInt32 and Int32ToCDFid so
*                               8-byte CDFid can be preserved and restored
*                               from the 4-byte id in Fortran.
*
******************************************************************************/

#include "cdflib.h"

/******************************************************************************
* sX.
*   Determine what should be done with a status code.  For each call to
* `CDFlib', status is returned as follows...
*
*  1. The first ERROR encountered terminates the call and is returned.  This
*     routine will not overwrite an existing error code.
*  2. The last WARNING encountered is returned.  INFOs encountered after a
*     WARNING are ignored.
*  3. In the absence of any WARNINGs, the last INFO is returned.
*  4. In the absence of any WARNINGs or INFOs, CDF_OK is returned.
*
* This routine returns FALSE if the pending status code is an ERROR; otherwise
* it returns TRUE.
*
******************************************************************************/

VISIBLE_PREFIX Logical sX (cStatus, pStatus)
CDFstatus cStatus;      /* Status code to be checked. */
CDFstatus *pStatus;     /* Pending status code. */
{
  if (cStatus == CDF_OK) return TRUE;           /* Ok, do nothing. */
  if (cStatus < CDF_WARN) {                     /* Error. */
    if (*pStatus > CDF_WARN) *pStatus = cStatus;
    return FALSE;
  }
  if (cStatus > CDF_OK) {                       /* Info. */
    if (CDF_OK <= *pStatus) *pStatus = cStatus;
    return TRUE;
  }
  *pStatus = cStatus;                           /* Warning. */
  return TRUE;
}

/******************************************************************************
* LocateCurrentVar.
******************************************************************************/

STATICforIDL CDFstatus LocateCurrentVar (CDF, zOp, offset, zVar, Var)
struct CDFstruct *CDF;          /* In: Pointer to CDF. */
Logical zOp;                    /* In: TRUE if current zVariable is to be
				   accessed; FALSE if current rVariable.  N/A
				   if zMode is on (since the current zVariable
				   number will always be used). */
Int32 *offset;                  /* Out: Offset of the current variable's VDR.
				   This may be a NULL pointer. */
Logical *zVar;                  /* Out: TRUE if a true zVariable; FALSE if
				   a true rVariable.  This may be a NULL
				   pointer. */
struct VarStruct **Var;         /* Out: Pointer to variable.  This will be NULL
				   if the variable has yet to be initialized.
				   This may be a NULL pointer. */
{
  CDFstatus tStatus;
  Int32 tOffset;
  /****************************************************************************
  * Pass back the offset of the VDR.
  ****************************************************************************/
  if (zModeON(CDF)) {
    if (CDF->CURzVarNum < CDF->NrVars) {
      ASSIGNnotNULL(zVar, FALSE)
      tStatus = FindVarByNumber (CDF, CDF->CURzVarNum, FALSE, &tOffset);
      if (StatusOK(tStatus)) {
	ASSIGNnotNULL (Var, CDF->rVars[(int)CDF->CURzVarNum])
	ASSIGNnotNULL (offset, tOffset)
	CDF->CURzVarOffset = tOffset;
      }
    }
    else {
      ASSIGNnotNULL(zVar, TRUE)
      tStatus = FindVarByNumber (CDF, CDF->CURzVarNum - CDF->NrVars, TRUE,
				 &tOffset);
      if (StatusOK(tStatus)) {
	ASSIGNnotNULL (Var, CDF->zVars[(int)(CDF->CURzVarNum - CDF->NrVars)])
	ASSIGNnotNULL (offset, tOffset)
	CDF->CURzVarOffset = tOffset;
      }
    }
  }
  else {
    ASSIGNnotNULL (zVar, zOp)
    tStatus = FindVarByNumber (CDF,BOO(zOp,CDF->CURzVarNum,
					   CDF->CURrVarNum),zOp,&tOffset);
    if (StatusOK(tStatus)) {
      ASSIGNnotNULL (Var, BOO(zOp,CDF->zVars[(int)CDF->CURzVarNum],
				  CDF->rVars[(int)CDF->CURrVarNum]))
      ASSIGNnotNULL (offset, tOffset)
      if (zOp) 
	CDF->CURzVarOffset = tOffset;
      else 
	CDF->CURrVarOffset = tOffset;
    }
  }
  return tStatus;
}

/******************************************************************************
* CurrentVarMode.
******************************************************************************/

STATICforIDL Logical CurrentVarMode (CDF, zOp)
struct CDFstruct *CDF;          /* In: Pointer to CDF. */
Logical zOp;                    /* In: TRUE if current zVariable is to be
				   accessed; FALSE if current rVariable.  N/A
				   if zMode is on (since the current zVariable
				   number will always be used). */
{

  if (zModeON(CDF)) {
    if (CDF->CURzVarNum < CDF->NrVars) 
      return FALSE;
    else 
      return TRUE;
  }
  else 
    return  zOp;
}

/******************************************************************************
* InitCurrentVar.
******************************************************************************/

STATICforIDL CDFstatus InitCurrentVar (CDF, zOp, Var)
struct CDFstruct *CDF;          /* In: Pointer to CDF. */
Logical zOp;                    /* In: TRUE if current zVariable is to be
				   accessed; FALSE if current rVariable.  N/A
				   if zMode is on (since the current zVariable
				   number will always be used [even when a real
				   rVariable is being accessed]). */
struct VarStruct **Var;         /* Out: Pointer to variable. */
{
  CDFstatus tStatus;
  /****************************************************************************
  * Pass back the pointer to its variable structure.  The variable will
  * be initialized for access if necessary.
  ****************************************************************************/
  if (zModeON(CDF))
    if (CDF->CURzVarNum < CDF->NrVars)
      tStatus = InitVar (CDF, CDF->CURzVarNum, FALSE, Var);
    else
      tStatus = InitVar (CDF, CDF->CURzVarNum - CDF->NrVars, TRUE, Var);
  else
    tStatus = InitVar (CDF,BOO(zOp,CDF->CURzVarNum,CDF->CURrVarNum),zOp,Var);
  return tStatus;
}

/******************************************************************************
* InitVar.
******************************************************************************/

STATICforIDL CDFstatus InitVar (CDF, varN, zVar, VarP)
struct CDFstruct *CDF;          /* In: Pointer to CDF. */
Int32 varN;                     /* In: Real variable number (ignoring the
				   zMode). */
Logical zVar;                   /* In: TRUE if a real zVariable (ignoring
				   the zMode). */
struct VarStruct **VarP;        /* Out: Pointer to variable. */
{
  CDFstatus pStatus = CDF_OK;
  struct VarStruct *Var = BOO(zVar,CDF->zVars[(int)varN],
				   CDF->rVars[(int)varN]);
  /****************************************************************************
  * Check if the variable has already been initialized.  If not, allocate and
  * initialize its variable structure.
  ****************************************************************************/
  if (Var == NULL) {
    /**************************************************************************
    * Allocate a variable structure.
    **************************************************************************/
    Var = (struct VarStruct *) cdf_AllocateMemory ((size_t)sizeof(struct VarStruct), NULL);
    if (Var == NULL) return BAD_MALLOC;
    /**************************************************************************
    * Determine offset of the VDR in the dotCDF file.
    **************************************************************************/
    if (!sX(FindVarByNumber(CDF,varN,zVar,&(Var->VDRoffset)),&pStatus)) {
      cdf_FreeMemory (Var, NULL);
      return pStatus;
    }

    /**************************************************************************
    * Initialize miscellaneous fields of the variable structure.
    **************************************************************************/
    Var->zVar = zVar; 
    Var->varN = varN;
    Var->fp = NULL;
    Var->varCacheSize = NUMcacheVAR;		/* N/A if single-file CDF. */
    Var->accessed_at = CDF->pseudo_clock++;
    Var->firstRecInVVR = NO_RECORD;
    Var->lastRecInVVR = NO_RECORD;
    Var->offsetOfVVR = NO_OFFSET;
    /**************************************************************************
    * Read fields to be held in memory for efficiency.
    **************************************************************************/
    if (!sX(ReadVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
		    VDR_MAXREC,&(Var->maxRec),
		    VDR_NULL),&pStatus)) {
      cdf_FreeMemory (Var, NULL);
      return pStatus;
    }
    /**************************************************************************
    * More initialization.
    **************************************************************************/
    if (!sX(InitVar2(CDF,Var),&pStatus)) {
      cdf_FreeMemory (Var, NULL);
      return pStatus;
    }
    /**************************************************************************
    * Store pointer to variable structure.
    **************************************************************************/
    if (zVar)
      CDF->zVars[(int)varN] = Var;
    else
      CDF->rVars[(int)varN] = Var;
  }
  /****************************************************************************
  * Pass back pointer to variable.
  ****************************************************************************/
  ASSIGNnotNULL (VarP, Var)
  return pStatus;
}

/******************************************************************************
* InitVar2.
******************************************************************************/

STATICforIDL CDFstatus InitVar2 (CDF, Var)
struct CDFstruct *CDF;
struct VarStruct *Var;
{
  int dimN; CDFstatus pStatus = CDF_OK; struct CPRstruct CPR; int i;
  Int32 flags, dataType, numElems, CPRoffset, sRecords;
  /****************************************************************************
  * Read necessary fields from the VDR.
  ****************************************************************************/
  if (!sX(ReadVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
		  VDR_FLAGS,&flags,
		  VDR_DATATYPE,&dataType,
		  VDR_NUMELEMS,&numElems,
		  VDR_sRECORDS,&sRecords,
		  VDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * Determine if the variable has a known data type.
  ****************************************************************************/
  if (!ValidDataType(dataType) || InValidDataType(dataType))
    return BAD_DATA_TYPE;
  /****************************************************************************
  * Calculate the dimensionality and variances of the variable based on the
  * current zMode.
  ****************************************************************************/
  if (!sX(CalcDimParms(CDF,Var->VDRoffset,
		       Var->zVar,&(Var->numDims),
		       Var->dimSizes,Var->dimVarys),&pStatus)) return pStatus;
  Var->recVary = BOO(RECvaryBITset(flags),VARY,NOVARY);
  /****************************************************************************
  * Calculate the number of values for each dimension.
  ****************************************************************************/
  CalcNumDimValues (CDF, Var);
  /****************************************************************************
  * Calculate the element and value sizes.
  ****************************************************************************/
  Var->NvalueElems = numElems;
  Var->NelemBytes = (Int32) CDFelemSize (dataType);
  Var->NvalueBytes = Var->NvalueElems * Var->NelemBytes;
  /****************************************************************************
  * Calculate the number of physical and virtual values per record.
  ****************************************************************************/
  CalcRecValues (Var);
  /****************************************************************************
  * Calculate the number of elements and the size (in bytes) of physical and
  * conceptual records.
  ****************************************************************************/
  Var->NphyRecElems = Var->NphyRecValues * Var->NvalueElems;
  Var->NvirtRecElems = Var->NvirtRecValues * Var->NvalueElems;
  Var->NphyRecBytes = Var->NphyRecValues * Var->NvalueBytes;
  Var->NvirtRecBytes = Var->NvirtRecValues * Var->NvalueBytes;
  /**************************************************************************
  * Initialize current positioning.
  **************************************************************************/
  Var->seqValueOffset = 0;
  Var->zRD.recNumber = 0;
  Var->zRD.recCount = 1;
  Var->zRD.recInterval = 1;
  for (dimN = 0; dimN < Var->numDims; dimN++) {
     Var->zRD.dimIndices[dimN] = 0;
     Var->zRD.dimCounts[dimN] = Var->dimSizes[dimN];
     Var->zRD.dimIntervals[dimN] = 1;
  }
  /****************************************************************************
  * Determine variable type.
  ****************************************************************************/
  if (!sX(VariableType(CDF,Var->VDRoffset,
		       Var->zVar,&(Var->vType)),&pStatus)) return pStatus;
  /**************************************************************************
  * What to do if a record is missing.
  **************************************************************************/
  Var->prevIfMissing = (sRecords == PREV_SPARSERECORDS);
  /**************************************************************************
  * Based on the variable type...
  **************************************************************************/
  switch (Var->vType) {
    case STANDARD_:
    case SPARSE_RECORDS_:
      if (!sX(LastRecord(CDF,Var->VDRoffset,Var->zVar,
			 &(Var->maxAllocated)),&pStatus)) return pStatus;
      Var->maxWritten = Var->maxAllocated;
      break;
    case COMPRESSED_:
    case SPARSE_COMPRESSED_RECORDS_:
      if (!sX(ReadVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
		      VDR_CPRorSPR,&CPRoffset,
		      VDR_NULL),&pStatus)) return pStatus;
      if (!sX(ReadCPR(CDF->fp,CPRoffset,
		      CPR_RECORD,&CPR,
		      CPR_NULL),&pStatus)) return pStatus;
      Var->cType = CPR.cType;
      for (i = 0; i < CPR.pCount; i++) Var->cParms[i] = CPR.cParms[i];
      Var->reservePct = 0;
      break;
    case SPARSE_ARRAYS_:
    case SPARSE_RECORDS_AND_ARRAYS_:
      return UNKNOWN_SPARSENESS;
    case IN_MULTI_:
      break;
    default:
      return CDF_INTERNAL_ERROR;
  }
  /**************************************************************************
  * Initialize staging area (although n/a for most variable types).
  **************************************************************************/
  Var->stage.areaOffset = NO_OFFSET;
  Var->stage.firstRec = NO_RECORD;
  Var->stage.lastRec = NO_RECORD;
  Var->stage.dotOffset = NO_OFFSET;
  Var->stage.modified = FALSE;
  /****************************************************************************
  * Calculate the blocking factor based on variable type...
  ****************************************************************************/
  if (!sX(CalcBF(CDF,Var),&pStatus)) return pStatus;
  /****************************************************************************
  * Determine the encoding and decoding functions to be used.
  ****************************************************************************/
  if (!sX(ConversionFunction(dataType,HostEncoding(),
			     CDF->encoding,CDF->negToPosFp0,
			     &(Var->EncodeFunction)),&pStatus)) return pStatus;
  if (!sX(ConversionFunction(dataType,CDF->encoding,
			     CDF->decoding,CDF->negToPosFp0,
			     &(Var->DecodeFunction)),&pStatus)) return pStatus;
  return pStatus;
}

/******************************************************************************
* CalcRecValues.
******************************************************************************/

STATICforIDL void CalcRecValues (Var)
struct VarStruct *Var;
{
  int dimN;
  Var->NphyRecValues = 1;
  Var->NvirtRecValues = 1;
  for (dimN = 0; dimN < Var->numDims; dimN++) {
     Var->NvirtRecValues *= Var->dimSizes[dimN];
     if (Var->dimVarys[dimN]) Var->NphyRecValues *= Var->dimSizes[dimN];
  }
  return;
}

/******************************************************************************
* CalcNumDimValues.
******************************************************************************/

STATICforIDL void CalcNumDimValues (CDF, Var)
struct CDFstruct *CDF;
struct VarStruct *Var;
{
  int dimN, dimNx;
  if (!CDF->rowMajor) {
     for (dimN = 0; dimN < Var->numDims; dimN++) {
	Var->nPhyDimValues[dimN] = 1;
	for (dimNx = 0; dimNx < dimN; dimNx++) {
	   if (Var->dimVarys[dimNx]) {
	     Var->nPhyDimValues[dimN] *= Var->dimSizes[dimNx];
	   }
	}
     }
  }
  else {
     for (dimN = 0; dimN < Var->numDims; dimN++) {
	Var->nPhyDimValues[dimN] = 1;
	for (dimNx = dimN + 1; dimNx < Var->numDims; dimNx++) {
	   if (Var->dimVarys[dimNx]) {
	     Var->nPhyDimValues[dimN] *= Var->dimSizes[dimNx];
	   }
	}
     }
  }
  return;
}

/******************************************************************************
* CalcBF.
******************************************************************************/

STATICforIDL CDFstatus CalcBF (CDF, Var)
struct CDFstruct *CDF;
struct VarStruct *Var;
{
  CDFstatus pStatus = CDF_OK; Int32 bF;
  if (!sX(ReadVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
		  VDR_BLOCKING,&bF,
		  VDR_NULL),&pStatus)) return pStatus;
  switch (Var->vType) {
    case STANDARD_:
      if (Var->recVary)
	if (bF == 0) {
	  Int32 min = ((MIN_BLOCKING_BYTES_standard-1)/Var->NphyRecBytes)+1;
	  Var->blockingFactor = MAXIMUM(min,MIN_BLOCKING_RECS_standard);
	}
	else
	  Var->blockingFactor = bF;
      else
	Var->blockingFactor = 1;
      break;
    case SPARSE_RECORDS_:
      if (Var->recVary)
	if (bF == 0) {
	  Int32 min = ((MIN_BLOCKING_BYTES_sparse-1)/Var->NphyRecBytes)+1;
	  Var->blockingFactor = MAXIMUM(min,MIN_BLOCKING_RECS_sparse);
	}
	else
	  Var->blockingFactor = bF;
      else
	Var->blockingFactor = 1;
      break;
    case COMPRESSED_:
    case SPARSE_COMPRESSED_RECORDS_:
      Var->blockingFactor = bF;
      break;
    case SPARSE_ARRAYS_:
    case SPARSE_RECORDS_AND_ARRAYS_:
      return UNKNOWN_SPARSENESS;
    case IN_MULTI_:
      Var->blockingFactor = 1;
      break;
    default:
      return CDF_INTERNAL_ERROR;
  }
  return pStatus;
}

/******************************************************************************
* UpdateConversions.
******************************************************************************/

STATICforIDL CDFstatus UpdateConversions (CDF)
struct CDFstruct *CDF;
{
  CDFstatus pStatus = CDF_OK; Logical zVar;
  for (zVar = 0; zVar <= 1; zVar++) {
     int varN; Int32 nVars = BOO(zVar,CDF->NzVars,CDF->NrVars);
     for (varN = 0; varN < nVars; varN++) {
	struct VarStruct *Var = BOO(zVar,CDF->zVars[varN],CDF->rVars[varN]);
        if (Var != NULL) {
	  Int32 dataType;
	  if (!sX(ReadVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
			  VDR_DATATYPE,&dataType,
			  VDR_NULL),&pStatus)) return pStatus;
	  if (!sX(ConversionFunction(dataType,HostEncoding(),
				     CDF->encoding,CDF->negToPosFp0,
				     &(Var->EncodeFunction)),&pStatus)) return
								       pStatus;
	  if (!sX(ConversionFunction(dataType,CDF->encoding,
				     CDF->decoding,CDF->negToPosFp0,
				     &(Var->DecodeFunction)),&pStatus)) return
								       pStatus;
        }
     }
  }
  return pStatus;
}

/******************************************************************************
* UpdateNEWzMode.
******************************************************************************/

STATICforIDL CDFstatus UpdateNEWzMode (CDF)
struct CDFstruct *CDF;
{
  CDFstatus pStatus = CDF_OK; Logical zVar;
  for (zVar = 0; zVar <= 1; zVar++) {
     int varN; Int32 nVars = BOO(zVar,CDF->NzVars,CDF->NrVars);
     for (varN = 0; varN < nVars; varN++) {
	struct VarStruct *Var = BOO(zVar,CDF->zVars[varN],CDF->rVars[varN]);
        if (Var != NULL) {
	  if (!sX(CalcDimParms(CDF,Var->VDRoffset,
			       Var->zVar,&(Var->numDims),
			       Var->dimSizes,
			       Var->dimVarys),&pStatus)) return pStatus;
	  CalcNumDimValues (CDF, Var);
	  CalcRecValues (Var);
	  Var->NvirtRecElems = Var->NvirtRecValues * Var->NvalueElems;
	  Var->NvirtRecBytes = Var->NvirtRecValues * Var->NvalueBytes;
        }
     }
  }
  return pStatus;
}

/******************************************************************************
* VarIdentity.
*    Returns information about the real identity of a variable.
******************************************************************************/

STATICforIDL CDFstatus VarIdentity (CDF, varN, zOp, varNt, zVar, Var)
struct CDFstruct *CDF;          /* In: Pointer to CDF. */
Int32 varN;                      /* In: Variable number - zModed. */
Logical zOp;                    /* In: TRUE if zVariable; FALSE if rVariable.
				       This is also zModed. */
Int32 *varNt;                    /* Out: Real variable number. */
Logical *zVar;                  /* Out: Real variable type: TRUE if a
					zVariable; FALSE if an rVariable. */
struct VarStruct **Var;         /* Out: Pointer to variable. */
{
  if (zModeON(CDF))
    if (0 <= varN && varN < CDF->NrVars) {
      ASSIGNnotNULL (varNt, varN)
      ASSIGNnotNULL (zVar, FALSE)
      ASSIGNnotNULL (Var, CDF->rVars[(int)varN])
    }
    else
      if (varN < CDF->NrVars + CDF->NzVars) {
	ASSIGNnotNULL (varNt, varN - CDF->NrVars)
	ASSIGNnotNULL (zVar, TRUE)
	ASSIGNnotNULL (Var, CDF->zVars[(int)varN-CDF->NrVars])
      }
      else
	return NO_SUCH_VAR;
  else
    if (0 <= varN && varN < BOO(zOp,CDF->NzVars,CDF->NrVars)) {
      ASSIGNnotNULL (varNt, varN)
      ASSIGNnotNULL (zVar, zOp)
      ASSIGNnotNULL (Var, BOO(zOp,CDF->zVars[(int)varN],
				  CDF->rVars[(int)varN]))
    }
    else
      return NO_SUCH_VAR;
  return CDF_OK;
}

/******************************************************************************
* OpenVar.
* Open a variable for read and/or write access (if this is a multi-file CDF
* and the variable file is closed).  It is assumed that the variable has been
* initialized for access and that the variable is closed (and in a multi-file
* CDF).
******************************************************************************/

STATICforIDL CDFstatus OpenVar (CDF, Var)
struct CDFstruct *CDF;
struct VarStruct *Var;
{
  CDFstatus pStatus = CDF_OK;
  char a_mode[MAX_aMODE_LEN+1], pathname[DU_MAX_PATH_LEN+1];
  /****************************************************************************
  * Try to open the variable file.
  ****************************************************************************/
  BuildFilePath (BOO(Var->zVar,Zt,Vt), CDF->CDFname, CDF->no_append,
		 CDF->upper_case_ext, CDF->version_numbers, Var->varN,
		 pathname);
  if (CDF->status == READ_WRITE)
    strcpyX (a_mode, READ_PLUS_a_mode, MAX_aMODE_LEN);
  else
    strcpyX (a_mode, READ_ONLY_a_mode, MAX_aMODE_LEN);
  Var->fp = V_open (pathname, a_mode);
  /****************************************************************************
  * If the open failed, close a variable file and try.  If that attempt fails
  * return an error.
  ****************************************************************************/
  if (Var->fp == NULL) {
    if (!sX(CloseLRUvar(CDF),&pStatus)) return pStatus;
    Var->fp = V_open (pathname, a_mode);
    if (Var->fp == NULL) return VAR_OPEN_ERROR;
  }
  /****************************************************************************
  * The open was successful - try to set the proper cache size.
  ****************************************************************************/
  if (!CACHEv(Var->fp,Var->varCacheSize)) {
    V_close (Var->fp, NULL, NULL);
    Var->fp = NULL;
    return BAD_CACHE_SIZE;
  }
  return pStatus;
}

/******************************************************************************
* LastRecord.
******************************************************************************/

STATICforIDL CDFstatus LastRecord (CDF, VDRoffset, zVar, recNum)
struct CDFstruct *CDF;  /* In: Pointer to CDF. */
Int32 VDRoffset;        /* In: Offset of VDR. */
Logical zVar;           /* In: TRUE if a real zVariable; FALSE if rVariable. */
Int32 *recNum;          /* Out: Last record allocated. */
{
  CDFstatus pStatus = CDF_OK;
  Int32 VXRoffset, nUsedEntries, lastRecs[MAX_VXR_ENTRIES];
  if (!sX(ReadVDR(CDF,CDF->fp,VDRoffset,zVar,
		  VDR_VXRTAIL,&VXRoffset,
		  VDR_NULL),&pStatus)) return pStatus;
  if (VXRoffset == 0)
    *recNum = NO_RECORD;
  else {
    if (!sX(ReadVXR(CDF->fp,VXRoffset,
		    VXR_NUSEDENTRIES,&nUsedEntries,
		    VXR_LASTREC,lastRecs,
		    VXR_NULL),&pStatus)) return pStatus;
    if ((int)nUsedEntries > MAX_VXR_ENTRIES) return CORRUPTED_V2_CDF; 
    *recNum = lastRecs[(int)(nUsedEntries-1)];
  }
  return pStatus;
}

/******************************************************************************
* IndicesValueOffset.
******************************************************************************/

STATICforIDL Int32 IndicesValueOffset (numDims, dimIndices, dimVarys,
				      nPhyDimValues)
Int32 numDims;
Int32 *dimIndices;
Int32 *dimVarys;
Int32 *nPhyDimValues;
{
  Int32 offset = 0; int dimN;
  for (dimN = 0; dimN < numDims; dimN++) {
     if (dimVarys[dimN]) offset += (dimIndices[dimN] * nPhyDimValues[dimN]);
  }
  return offset;
}

/******************************************************************************
* ValueOffsetIndices.
******************************************************************************/

STATICforIDL void ValueOffsetIndices (offset, rowMajor, numDims, dimVarys,
				      nPhyDimValues, indices)
Int32 offset;
Logical rowMajor;
Int32 numDims;
Int32 *dimVarys;
Int32 *nPhyDimValues;
Int32 *indices;
{
  int dimN, i;
  for (i = 0, dimN = BOO(rowMajor,0,(int)(numDims-1));
       i < numDims; i++, BOO(rowMajor,dimN++,dimN--)) {
     if (dimVarys[dimN]) {
       indices[dimN] = offset / nPhyDimValues[dimN];
       offset = offset % nPhyDimValues[dimN];
     }
     else
       indices[dimN] = 0;
  }
  return;
}

/******************************************************************************
* RecordByteOffset.
******************************************************************************/

STATICforIDL CDFstatus RecordByteOffset (CDF, Var, phyRecN, offsetP)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 phyRecN;
Int32 *offsetP;
{
  CDFstatus pStatus = CDF_OK;
  Int32 firstRec=-1, lastRec=-1, offset=-1; 
  switch (Var->vType) {
    case STANDARD_:
    case SPARSE_RECORDS_: {
      if (Var->firstRecInVVR <= phyRecN && phyRecN <= Var->lastRecInVVR) {
        *offsetP = (Int32) (Var->offsetOfVVR + VVR_BASE_SIZE +
                           (Var->NphyRecBytes * (phyRecN - Var->firstRecInVVR)));
	break;
      }
      else {
        if (!sX(SearchForRecord(CDF,Var->VDRoffset,Var->zVar,
	  		        phyRecN,&firstRec,&lastRec,
			        &offset,NULL),&pStatus)) return pStatus;
        *offsetP = (Int32) (offset + VVR_BASE_SIZE +
	 	           (Var->NphyRecBytes * (phyRecN - firstRec)));
	Var->firstRecInVVR = firstRec;
	Var->lastRecInVVR = lastRec;
	Var->offsetOfVVR = offset;
        break;
      }
    }
    case COMPRESSED_:
    case SPARSE_COMPRESSED_RECORDS_:
    case SPARSE_ARRAYS_:
    case SPARSE_RECORDS_AND_ARRAYS_:
      return CDF_INTERNAL_ERROR;
    case IN_MULTI_:
      *offsetP = (Int32) (phyRecN * Var->NphyRecBytes);
      break;
    default:
      return CDF_INTERNAL_ERROR;
  }
  return pStatus;
}

/******************************************************************************
* ConfigureNEWzMode.
******************************************************************************/

STATICforIDL CDFstatus ConfigureNEWzMode (CDF)
struct CDFstruct *CDF;
{
  CDFstatus pStatus = CDF_OK;
  if (!sX(UpdateNEWzMode(CDF),&pStatus)) return pStatus;
  InitCURobjectsStates (CDF);
  return pStatus;
}

/******************************************************************************
* InitCURobjectsStates.
*   Note that initializing is done regardless of the current zMode.
******************************************************************************/

STATICforIDL void InitCURobjectsStates (CDF)
struct CDFstruct *CDF;
{
  struct VarStruct *Var; int dimN, varNum;
  /****************************************************************************
  * Initialize current attributes/entries/variables.
  ****************************************************************************/
  CDF->CURattrOffset = RESERVED_ATTROFFSET;
  CDF->CURattrOffset64 = (OFF_T) RESERVED_ATTROFFSET;
  CDF->CURgrEntryNum = RESERVED_ENTRYNUM;
  CDF->CURgrEntryOffset = RESERVED_ENTRYOFFSET;
  CDF->CURgrEntryOffset64 = (OFF_T) RESERVED_ENTRYOFFSET;
  CDF->CURzEntryNum = RESERVED_ENTRYNUM;
  CDF->CURzEntryOffset = RESERVED_ENTRYOFFSET;
  CDF->CURzEntryOffset64 = (OFF_T) RESERVED_ENTRYOFFSET;
  CDF->CURrVarNum = RESERVED_VARNUM;
  CDF->CURzVarNum = RESERVED_VARNUM;

  if (CDF->fp != NULL)
  {
      CDF->fp->CurADRIndex = RESERVED_ENTRYNUM;
      CDF->fp->CurAEDRIndex = RESERVED_ENTRYNUM;
  };

  /****************************************************************************
  * Initialize current positioning for EACH rVariable.
  ****************************************************************************/
  for (varNum = 0; varNum < CDF->NrVars; varNum++) {
     Var = CDF->rVars[varNum];
     if (Var != NULL) {
       Var->seqValueOffset = 0;
       Var->seqValueOffset64 = (OFF_T) 0;
       Var->zRD.recNumber = 0;
       Var->zRD.recCount = 1;
       Var->zRD.recInterval = 1;
       for (dimN = 0; dimN < Var->numDims; dimN++) {
	  Var->zRD.dimIndices[dimN] = 0;
	  Var->zRD.dimCounts[dimN] = Var->dimSizes[dimN];
	  Var->zRD.dimIntervals[dimN] = 1;
       }
     }
  }
  /****************************************************************************
  * Initialize current positioning for EACH zVariable.
  ****************************************************************************/
  for (varNum = 0; varNum < CDF->NzVars; varNum++) {
     Var = CDF->zVars[varNum];
     if (Var != NULL) {
       Var->seqValueOffset = 0;
       Var->seqValueOffset64 = (OFF_T) 0;
       Var->zRD.recNumber = 0;
       Var->zRD.recCount = 1;
       Var->zRD.recInterval = 1;
       for (dimN = 0; dimN < Var->numDims; dimN++) {
	  Var->zRD.dimIndices[dimN] = 0;
	  Var->zRD.dimCounts[dimN] = Var->dimSizes[dimN];
	  Var->zRD.dimIntervals[dimN] = 1;
       }
     }
  }
  /****************************************************************************
  * Initialize current positioning for ALL rVariables.
  ****************************************************************************/
  CDF->rRD.recNumber = 0;
  CDF->rRD.recCount = 1;
  CDF->rRD.recInterval = 1;
  for (dimN = 0; dimN < CDF->rNumDims; dimN++) {
     CDF->rRD.dimIndices[dimN] = 0;
     CDF->rRD.dimCounts[dimN] = CDF->rDimSizes[dimN];
     CDF->rRD.dimIntervals[dimN] = 1;
  }
  return;
}

/******************************************************************************
* FindAttrByName.
******************************************************************************/

STATICforIDL CDFstatus FindAttrByName (CDF, searchName, offset)
struct CDFstruct *CDF;          /* In: Pointer to CDF. */
char *searchName;               /* In: Attribute name to find. */
Int32 *offset;                  /* Out: Offset of ADR that was found. */
{
  Int32 numAttrs, tOffset, nextADR, fstADR;
  char attrName[CDF_ATTR_NAME_LEN+1];
  CDFstatus pStatus = CDF_OK;
  int attrN;
  long read_only_mode;

  pStatus = CDFlib(CONFIRM_, CDF_READONLY_MODE_, &read_only_mode, NULL_);
  if (pStatus != CDF_OK) return pStatus;

  /**************************************************************************
  * If in READONLYon mode, set CurADRIndex to the referenced attribute number.
  ***************************************************************************/
  if (read_only_mode == READONLYon)
  {
      for (attrN = 0; attrN < CDF->fp->GDR->NumAttr; attrN++)
      {
         if (!strcmpITB(CDF->fp->ADRList[attrN]->Name,searchName))
         {
             CDF->fp->CurADRIndex = attrN;
             ASSIGNnotNULL (offset, DUMMY_ENTRYOFFSET);
             return CDF_OK;
         };
      };
  }
  else
  {
      /************************************************************************
      * Read number of attributes and the offset of the first ADR from the GDR.
      ************************************************************************/
      if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
                      GDR_NUMATTR,&numAttrs,
                      GDR_ADRHEAD,&fstADR,
                      GDR_NULL),&pStatus)) return pStatus;
      /************************************************************************
      * Read from ADRs until a matching attribute name is found.
      *   Note that if this is a V2.0 CDF, the last ADR will not have an
      * offset of zero for the next ADR.  For that reason, we will loop
      * through the number of attributes read from the GDR (and then stop).
      ************************************************************************/
      if (CDF->CURattrOffset != RESERVED_ATTROFFSET)                   
        tOffset = CDF->CURattrOffset;                                  
      else                                                             
        tOffset = fstADR;                                              

      for (attrN = 0; attrN < numAttrs; attrN++) {
         if (!sX(ReadADR(CDF->fp,tOffset,
	                 ADR_NAME,attrName,
                         ADR_ADRNEXT,&nextADR,
                         ADR_NULL),&pStatus)) return pStatus;
         if (!strcmpITB(attrName,searchName)) {
           ASSIGNnotNULL (offset, tOffset)
           return CDF_OK;
         }
         if (nextADR != 0)
           tOffset = nextADR;
         else
           tOffset = fstADR;
      }
  };
  /****************************************************************************
  * Attribute name not found, return error.
  ****************************************************************************/
  return NO_SUCH_ATTR;
}

/******************************************************************************
* FindAttrByNumber.
******************************************************************************/

STATICforIDL CDFstatus FindAttrByNumber (CDF, searchNum, offset)
struct CDFstruct *CDF;          /* In: Pointer to the CDF. */
Int32 searchNum;                /* In: The attribute number to be found. */
Int32 *offset;                  /* Out: The offset of the located ADR. */
{
  Int32 numAttrs, tOffset, attrNum, nextADR, fstADR;
  CDFstatus pStatus = CDF_OK;
  Int32 attrN;
  long read_only_mode;
  /****************************************************************************
  * Validate.
  ****************************************************************************/
  if (searchNum < 0) return BAD_ATTR_NUM;
  /****************************************************************************
  * First check if the next attribute is the one being searched for.  For this
  * to be the case, an attribute must currently be selected, the next attribute
  * must exist, and the next attribute's number must be the attribute number
  * being searched for.  But don't try this if a V2.0 CDF because of the bad
  * terminating offset of the ADR linked list in those CDFs.
  ****************************************************************************/
/*
  if (!CDF->badTerminatingOffsets) {
    if (CDF->CURattrOffset != RESERVED_ATTROFFSET) {
      Int32 nextOffset;
      if (!sX(ReadADR(CDF->fp,CDF->CURattrOffset,
		      ADR_ADRNEXT,&nextOffset,
		      ADR_NULL),&pStatus)) return pStatus;
      if (nextOffset != 0) {
	Int32 nextNum;
	if (!sX(ReadADR(CDF->fp,nextOffset,
			ADR_NUM,&nextNum,
			ADR_NULL),&pStatus)) return pStatus;
	if (nextNum == searchNum) {
	  *offset = nextOffset;
	  return pStatus;
	}
      }
    }
  }
*/
  pStatus = CDFlib(CONFIRM_, CDF_READONLY_MODE_, &read_only_mode, NULL_);
  if (pStatus != CDF_OK) return pStatus;

  /***************************************************************************
  * If in read only mode, set CurADRIndex to the searched for atribute number.
  ***************************************************************************/
  if (read_only_mode == READONLYon)
  {
      if (CDF->fp->GDR->NumAttr <= searchNum) return NO_SUCH_ATTR;
      if (searchNum < 0 || searchNum >= CDF->fp->GDR->NumAttr) 
                                                            return NO_SUCH_ATTR;
      CDF->fp->CurADRIndex = searchNum;
      ASSIGNnotNULL (offset, DUMMY_ENTRYOFFSET);
      return CDF_OK;
  }
  else
  {
      /************************************************************************
      * The next attribute isn't the one being searched for.  First read the
      * number of attributes and the offset of the first ADR from the GDR.
      ************************************************************************/
      if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
                      GDR_NUMATTR,&numAttrs,
                      GDR_ADRHEAD,&fstADR,
                      GDR_NULL),&pStatus)) return pStatus;
      if (numAttrs <= searchNum) return NO_SUCH_ATTR;

      if (CDF->CURattrOffset != RESERVED_ATTROFFSET)
        tOffset = CDF->CURattrOffset;
      else
        tOffset = fstADR;
      /************************************************************************
      * Read from ADRs until a matching attribute number is found.
      *   Note that if this is a V2.0 CDF, the last ADR will not have an
      * offset of zero for the next ADR.  For that reason, we will loop
      * through the number of attributes read from the GDR (and then stop).
      ************************************************************************/
      for (attrN = 0; attrN < numAttrs; attrN++) {
         if (!sX(ReadADR(CDF->fp,tOffset,
                         ADR_NUM,&attrNum,
                         ADR_ADRNEXT,&nextADR,
                         ADR_NULL),&pStatus)) return pStatus;
         if (attrNum == searchNum) {
           ASSIGNnotNULL (offset, tOffset)
           return CDF_OK;
         }
         if (nextADR != 0)
           tOffset = nextADR;
         else
           tOffset = fstADR;
      }   
      /************************************************************************
      * Attribute number not found, internal error or corrupted CDF.
      ************************************************************************/
      return CORRUPTED_V2_CDF;
  }
}

/******************************************************************************
* FindEntryByNumber.
******************************************************************************/

STATICforIDL CDFstatus FindEntryByNumber (CDF, ADRoffset, zEntry, entryN,
					  offset)
struct CDFstruct *CDF;          /* In: Pointer to the CDF. */
Int32 ADRoffset;                /* In: Offset of attribute's ADR. */
Logical zEntry;			/* In: TRUE if AzEDR list to be searched.
				       FALSE if AgrEDR list to be searched. */
Int32 entryN;                    /* In: The entry number being searched for. */
Int32 *offset;                  /* Out: The offset of the located AEDR. */
{
  Int32 numEntries, tOffset, entryNum, nextADR;
  CDFstatus pStatus = CDF_OK;
  Int32 entryX;
  long read_only_mode;
  /****************************************************************************
  * Read number of entries and the offset of the first AEDR from the ADR.
  ****************************************************************************/
  if (!sX(ReadADR(CDF->fp,ADRoffset,
		  BOO(zEntry,ADR_NzENTRIES,ADR_NgrENTRIES),&numEntries,
		  BOO(zEntry,ADR_AzEDRHEAD,ADR_AgrEDRHEAD),&tOffset,
		  ADR_NULL),&pStatus)) return pStatus;
  pStatus = CDFlib(CONFIRM_, CDF_READONLY_MODE_, &read_only_mode, NULL_);
  if (pStatus != CDF_OK) return pStatus;
  if (read_only_mode == READONLYon)
  {
      /***********************************************************************
      * If the requested entry exists, set CurAEDRIndex to the entry number and 
      * set CURzEntrySel to reflect if the entry is in the z list or the gr 
      * list.
      ************************************************************************/
      if (zEntry && entryN <= CDF->fp->ADRList[CDF->fp->CurADRIndex]->MAXzEntry)
      {
          if (CDF->fp->ADRList[CDF->fp->CurADRIndex]->zAEDRList[entryN] != NULL)
          {
              CDF->fp->CURzEntrySel = TRUE;
              CDF->fp->CurAEDRIndex = entryN;
              ASSIGNnotNULL (offset, DUMMY_ENTRYOFFSET);
              return pStatus;
          }
          else
          {
              return NO_SUCH_ENTRY;
          }
      }
      else if (!zEntry && entryN <= CDF->fp->ADRList[CDF->fp->CurADRIndex]->
               MAXgrEntry)
      {
          if (CDF->fp->ADRList[CDF->fp->CurADRIndex]->grAEDRList[entryN] 
              != NULL)
          {
              CDF->fp->CURzEntrySel = FALSE;
              CDF->fp->CurAEDRIndex = entryN;
              ASSIGNnotNULL (offset, DUMMY_ENTRYOFFSET);
              return pStatus;
          }
          else
          {
              return NO_SUCH_ENTRY;
          };
      }

      else
      {
          return NO_SUCH_ENTRY;
      };
  };
  /****************************************************************************
  * Read from AEDRs until a matching entry number is found.
  *   Note that if this is a V2.0 CDF, the last AEDR will not have an
  * offset of zero for the next AEDR.  For that reason, we will loop
  * through the number of entries read from the ADR (and then stop).
  ****************************************************************************/
  for (entryX = 0; entryX < numEntries; entryX++) {
     if (!sX(ReadAEDR(CDF->fp,tOffset,
		      AEDR_NUM,&entryNum,
		      AEDR_AEDRNEXT,&nextADR,
		      AEDR_NULL),&pStatus)) return pStatus;
     if (entryNum == entryN) {
       ASSIGNnotNULL (offset, tOffset)
       return CDF_OK;
     }
     tOffset = nextADR;
  }
  /****************************************************************************
  * Entry number not found.
  ****************************************************************************/
  return NO_SUCH_ENTRY;
}

/******************************************************************************
* FindLastAttr.
******************************************************************************/

STATICforIDL CDFstatus FindLastAttr (CDF, lastOffset)
struct CDFstruct *CDF;          /* In: Pointer to the CDF. */
Int32 *lastOffset;              /* Out: Offset of last attribute's ADR. */
{
  CDFstatus pStatus = CDF_OK;
  Int32 nAttrs, offset;
  Int32 attrN;
  long read_only_mode;

  pStatus = CDFlib(CONFIRM_, CDF_READONLY_MODE_, &read_only_mode, NULL_);
  if (pStatus != CDF_OK) return pStatus;

  /*************************************************************************
  * If in READONLYon mode, set CurADRIndex to the index of the last
  * attribute in menmory.
  *************************************************************************/
  if (read_only_mode == READONLYon)
  {
      *lastOffset = DUMMY_ATTROFFSET;
      CDF->fp->CurADRIndex = CDF->fp->GDR->NumAttr - 1;
  }
  else
  {
      /************************************************************************
      * Read number of attributes and the offset of the first ADR.  If there are
      * no attributes, return an offset of zero.
      ************************************************************************/
      if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
                      GDR_NUMATTR,&nAttrs,
                      GDR_NULL),&pStatus)) return pStatus;
      if (nAttrs == 0) {
        *lastOffset = 0;
        return pStatus;
      }
      /************************************************************************
      * There is at least one attribute.
      * Note that if this is a V2.0 CDF, the last ADR will not have an offset of
      * zero for the next ADR.  For that reason, we will loop through the number
      * of attributes read from the GDR (and then stop).
      ************************************************************************/
      if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
                      GDR_ADRHEAD,&offset,
                      GDR_NULL),&pStatus)) return pStatus;
      for (attrN = 0; attrN < nAttrs - 1; attrN++) {
         if (!sX(ReadADR(CDF->fp,offset,
                         ADR_ADRNEXT,&offset,
                         ADR_NULL),&pStatus)) return pStatus;
      }
      *lastOffset = offset;
  };
  return pStatus;
}

/******************************************************************************
* FindLastEntry.
******************************************************************************/

STATICforIDL CDFstatus FindLastEntry (CDF, ADRoffset, zEntry, lastOffset)
struct CDFstruct *CDF;          /* In: Pointer to the CDF. */
Int32 ADRoffset;                /* In: Offset of attribute's ADR. */
Logical zEntry;                 /* In: TRUE if (real) zEntry is being searched
				   for; FALSE if gEntry/rEntry. */
Int32 *lastOffset;              /* Out: The offset of the last AEDR. */
{
  CDFstatus pStatus = CDF_OK;
  Int32 offset, nEntries;
  Int32 entryX;
  long read_only_mode;

  pStatus = CDFlib(CONFIRM_, CDF_READONLY_MODE_, &read_only_mode, NULL_);
  if (pStatus != CDF_OK) return pStatus;

  /***********************************************************************
  * If in READONLYon mode, set CurAEDRIndex to MAXzEntry or MAXgrEntry, as
  * appropriate.
  ***********************************************************************/
  if (read_only_mode == READONLYon)
  {
      *lastOffset = DUMMY_ENTRYOFFSET;
      if (zEntry == TRUE)
      {
          CDF->fp->CurAEDRIndex =
                           CDF->fp->ADRList[CDF->fp->CurADRIndex]->MAXzEntry;
      }
      else
      {
          CDF->fp->CurAEDRIndex =
                           CDF->fp->ADRList[CDF->fp->CurADRIndex]->MAXgrEntry;      };
  }
  else
  {
      /************************************************************************
      * Read offset of first AEDR and determine if there are any entries (of the
      * specified type).  If there are none, pass back an offset of zero.
      ************************************************************************/
      if (!sX(ReadADR(CDF->fp,ADRoffset,
                      BOO(zEntry,ADR_AzEDRHEAD,ADR_AgrEDRHEAD),
                      &offset,ADR_NULL),&pStatus)) return pStatus;
      if (offset == 0) {
        *lastOffset = 0;
        return pStatus;
      }
      /************************************************************************
      * There is at least one entry.  Read the actual number of entries and then
      * scan through to the last one.
      *   Note that if this is a V2.0 CDF, the last AEDR will not have an
      * offset of zero for the next AEDR.  For that reason, we will loop
      * through the number of entries (minus one) read from the ADR (and then
      * stop).
      ************************************************************************/
      if (!sX(ReadADR(CDF->fp,ADRoffset,
                      BOO(zEntry,ADR_NzENTRIES,ADR_NgrENTRIES),&nEntries,
                      ADR_NULL),&pStatus)) return pStatus;
      for (entryX = 0; entryX < nEntries - 1; entryX++) {
         if (!sX(ReadAEDR(CDF->fp,offset,
                          AEDR_AEDRNEXT,&offset,
                          AEDR_NULL),&pStatus)) return pStatus;
      }
      *lastOffset = offset;
  }
  return pStatus;
}

/******************************************************************************
* FindPrevEntry.
******************************************************************************/

STATICforIDL CDFstatus FindPrevEntry (CDF, ADRoffset, searchOffset, zEntry,
				      prevOffset)
struct CDFstruct *CDF;          /* Pointer to the CDF. */
Int32 ADRoffset;                /* Offset of attribute's ADR. */
Int32 searchOffset;             /* The entry offset being searched for. */
Logical zEntry;                 /* TRUE if (real) zEntry is being searched for;
				   FALSE if gEntry/rEntry. */
Int32 *prevOffset;              /* The offset of the previous AEDR. */
{
  CDFstatus pStatus = CDF_OK;
  Int32 offset, nEntries, nextOffset;
  Int32 entryX;
  /****************************************************************************
  * Read the offset of the first AEDR.  If that offset is the same as the
  * search offset, return an offset of zero.
  ****************************************************************************/
  if (!sX(ReadADR(CDF->fp,ADRoffset,
		  BOO(zEntry,ADR_AzEDRHEAD,ADR_AgrEDRHEAD),&offset,
		  ADR_NULL),&pStatus)) return pStatus;
  if (offset == searchOffset) {
    *prevOffset = 0;
    return pStatus;
  }
  /****************************************************************************
  * The first AEDR is not at the search offset.  Read the actual number of
  * entries and then scan through them looking for the AEDR offset being
  * searched for.  If the search offset is not found, then either the CDF is
  * corrupted or an internal logic error has occurred.
  *   Note that if this is a V2.0 CDF, the last AEDR will not have an
  * offset of zero for the next AEDR.  For that reason, we will loop
  * through the number of entries read from the ADR (and then stop).
  ****************************************************************************/
  if (!sX(ReadADR(CDF->fp,ADRoffset,
		  BOO(zEntry,ADR_NzENTRIES,ADR_NgrENTRIES),&nEntries,
		  ADR_NULL),&pStatus)) return pStatus;
  for (entryX = 0; entryX < nEntries; entryX++) {
     if (!sX(ReadAEDR(CDF->fp,offset,
		      AEDR_AEDRNEXT,&nextOffset,
		      AEDR_NULL),&pStatus)) return pStatus;
     if (nextOffset == searchOffset) {
       *prevOffset = offset;
       return pStatus;
     }
     offset = nextOffset;
  }
  return CORRUPTED_V2_CDF;
}

/******************************************************************************
* FindVarByName.
*    Both the rVariable and zVariable lists are searched (since variable names
* are unique in a CDF).
******************************************************************************/

STATICforIDL CDFstatus FindVarByName (CDF, searchName, offset, zVar, Var)
struct CDFstruct *CDF;  /* In: Pointer to the CDF. */
char *searchName;       /* In: The variable name being searched for. */
Int32 *offset;          /* Out: Offset of the zVDR/rVDR. */
Logical *zVar;          /* Out: TRUE if a zVariable. */
struct VarStruct **Var; /* Out: Pointer to variable structure. */
{
  int varN;
  char varName[CDF_VAR_NAME_LEN+1];  
  CDFstatus pStatus = CDF_OK;
  Int32 tOffset, nextVDR;
  Int32 headOffset;
  /****************************************************************************
  * Read from rVDRs until a matching variable name is found.
  *   Note that if this is a V2.0 CDF, the last rVDR will not have an
  * offset of zero for the next rVDR.  For that reason, we will loop
  * through the number of rVariables (and then stop).
  ****************************************************************************/
  if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		  GDR_rVDRHEAD,&headOffset,
		  GDR_NULL),&pStatus)) return pStatus;

  if (CDF->CURrVarNum != RESERVED_VARNUM)
    tOffset = CDF->CURrVarOffset;
  else
    tOffset = headOffset;

  for (varN = 0; varN < CDF->NrVars; varN++) {
     if (!sX(ReadVDR(CDF,CDF->fp,tOffset,FALSE,
		     VDR_NAME,varName,
		     VDR_VDRNEXT,&nextVDR,
		     VDR_NULL),&pStatus)) return pStatus;
     if (!strcmpITB(varName,searchName)) {
       ASSIGNnotNULL (offset, tOffset)
       ASSIGNnotNULL (zVar, FALSE)
       ASSIGNnotNULL (Var, CDF->rVars[varN])
       return CDF_OK;
     }
     if (nextVDR == 0) tOffset = headOffset;
     else tOffset = nextVDR;
  }
  /****************************************************************************
  * Read from zVDRs until a matching variable name is found.
  ****************************************************************************/
  if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		  GDR_zVDRHEAD,&headOffset,
		  GDR_NULL),&pStatus)) return pStatus;
/*
  if (CDF->CURzVarNum != RESERVED_VARNUM)
    tOffset = CDF->CURzVarOffset;
  else
*/
  tOffset = headOffset;

  for (varN = 0; varN < CDF->NzVars; varN++) {
     if (!sX(ReadVDR(CDF,CDF->fp,tOffset,TRUE,
		     VDR_NAME,varName,
		     VDR_VDRNEXT,&nextVDR,
		     VDR_NULL),&pStatus)) return pStatus;
     if (!strcmpITB(varName,searchName)) {
       ASSIGNnotNULL (offset, tOffset)
       ASSIGNnotNULL (zVar, TRUE)
       ASSIGNnotNULL (Var, CDF->zVars[varN])
       return CDF_OK;
     }
     if (nextVDR == 0) tOffset = headOffset;
     else tOffset = nextVDR;
  }
  /****************************************************************************
  * Variable name not found, return error.
  ****************************************************************************/
  return NO_SUCH_VAR;
}

/******************************************************************************
* FindVarByNumber.
******************************************************************************/

STATICforIDL CDFstatus FindVarByNumber (CDF, searchNum, zVar, offset)
struct CDFstruct *CDF;  /* In: Pointer to CDF. */
Int32 searchNum;        /* In: Variable number to be searched for. */
Logical zVar;           /* In: TRUE if a (real) zVariable number should be
			   found. */
Int32 *offset;          /* Out: offset of the VDR. */
{
  CDFstatus pStatus = CDF_OK;
  Int32 nVars = BOO(zVar,CDF->NzVars,CDF->NrVars);
  Int32 tOffset, varNum, nextVDR, fstOffset;
  int varN;
  /****************************************************************************
  * Read offset of first VDR.
  ****************************************************************************/
  if (searchNum < 0) return BAD_VAR_NUM;
  if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		  BOO(zVar,GDR_zVDRHEAD,GDR_rVDRHEAD),&fstOffset,
		  GDR_NULL),&pStatus)) return pStatus;
  if (nVars <= searchNum) return NO_SUCH_VAR;
  /****************************************************************************
  * Read from VDRs until a matching variable number is found.
  *   Note that if this is a V2.0 CDF, the last VDR will not have an
  * offset of zero for the next VDR.  For that reason, we will loop
  * through the number of variables (and then stop).
  ****************************************************************************/

  if (zModeON(CDF)) {
    tOffset = fstOffset;
    if (CDF->CURzVarNum != RESERVED_VARNUM) {
      long numT = BOO(zVar, CDF->CURzVarNum-CDF->NrVars, CDF->CURzVarNum);
      if (numT > -1 && numT <= searchNum) tOffset = CDF->CURzVarOffset;
    }
  }
  else {
    if (zVar) {
      if (CDF->CURzVarNum != RESERVED_VARNUM && CDF->CURzVarNum < searchNum)
        tOffset = CDF->CURzVarOffset;
      else
        tOffset = fstOffset;
    } else {
      if (CDF->CURrVarNum != RESERVED_VARNUM && CDF->CURrVarNum < searchNum)
        tOffset = CDF->CURrVarOffset;
      else
        tOffset = fstOffset;
    }
  }

  /****************************************************************************
  * Search for the variable from the current selected variabale, instead of
  * the top of the variable list. It will speed up the search process if the 
  * variables are accessed in a orderly sequence.
  ****************************************************************************/   
  for (varN = 0; varN < nVars; varN++) {
     if (!sX(ReadVDR(CDF,CDF->fp,tOffset,zVar,
		     VDR_NUM,&varNum,
		     VDR_VDRNEXT, &nextVDR, 
		     VDR_NULL),&pStatus)) return pStatus;
     if (varNum == searchNum) {
       ASSIGNnotNULL (offset, tOffset)
       return CDF_OK;
     }
     if (nextVDR != 0) 			/* Reaching the end of the list? */
       tOffset = nextVDR;		/* No.... Continue.              */
     else 
       tOffset = fstOffset;		/* Yes... Go back to the top.    */ 
  }
  /****************************************************************************
  * Variable number not found, return error.
  ****************************************************************************/
  return CORRUPTED_V2_CDF;
}

/******************************************************************************
* VerifyNoRecordsWritten.
*    Verifies that no records have been written.  Both the rVariable and
* zVariable lists are searched.
******************************************************************************/

STATICforIDL CDFstatus VerifyNoRecordsWritten (CDF, no)
struct CDFstruct *CDF;          /* In: Pointer to the CDF. */
Logical *no;                    /* Out: If TRUE, no records written. */
{
  CDFstatus pStatus = CDF_OK; Int32 tOffset, maxRec; int varN; Logical zVar;
  /****************************************************************************
  * Read from r/zVDRs until a maximum record greater than NO_RECORD is found.
  *   Note that if this is a V2.0 CDF, the last rVDR will not have an
  * offset of zero for the next rVDR.  For that reason, we will loop
  * through the number of variables (and then stop).
  ****************************************************************************/
  for (zVar = 0; zVar <= 1; zVar++) {
     if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
                     BOO(zVar,GDR_zVDRHEAD,GDR_rVDRHEAD),&tOffset,
                     GDR_NULL),&pStatus)) return pStatus;
     for (varN = 0; varN < BOO(zVar,CDF->NzVars,CDF->NrVars); varN++) {
        if (!sX(ReadVDR(CDF,CDF->fp,tOffset,zVar,
                        VDR_MAXREC,&maxRec,
                        VDR_VDRNEXT,&tOffset,
                        VDR_NULL),&pStatus)) return pStatus;
        if (maxRec > NO_RECORD) {
          *no = FALSE;
          return pStatus;
        }
     }
  }
  /****************************************************************************
  * No records written.
  ****************************************************************************/
  *no = TRUE;
  return pStatus;
}

/******************************************************************************
* VerifyNoPadsSpecified.
*    Verifies that no pad values have been specified.  Both the rVariable
* and zVariable lists are searched.
******************************************************************************/

STATICforIDL CDFstatus VerifyNoPadsSpecified (CDF, no)
struct CDFstruct *CDF;          /* In: Pointer to the CDF. */
Logical *no;                    /* Out: If TRUE, no pad values written. */
{
  CDFstatus pStatus = CDF_OK;
  Int32 tOffset, flags32;
  int varN;
  /****************************************************************************
  * Read from rVDRs until a pad value is found.
  *   Note that if this is a V2.0 CDF, the last rVDR will not have an
  * offset of zero for the next rVDR.  For that reason, we will loop
  * through the number of rVariables (and then stop).
  ****************************************************************************/
  if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
                  GDR_rVDRHEAD,&tOffset,
                  GDR_NULL),&pStatus)) return pStatus;
  for (varN = 0; varN < CDF->NrVars; varN++) {
     if (!sX(ReadVDR(CDF,CDF->fp,tOffset,FALSE,
                     VDR_FLAGS,&flags32,
                     VDR_NULL),&pStatus)) return pStatus;
     if (PADvalueBITset(flags32)) {
       *no = FALSE;
       return pStatus;
     }
     if (!sX(ReadVDR(CDF,CDF->fp,tOffset,FALSE,
                     VDR_VDRNEXT,&tOffset,
                     VDR_NULL),&pStatus)) return pStatus;
  }
  /****************************************************************************
  * Read from zVDRs until a pad value is found.
  ****************************************************************************/
  if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
                  GDR_zVDRHEAD,&tOffset,
                  GDR_NULL),&pStatus)) return pStatus;
  for (varN = 0; varN < CDF->NzVars; varN++) {
     if (!sX(ReadVDR(CDF,CDF->fp,tOffset,TRUE,
                     VDR_FLAGS,&flags32,
                     VDR_NULL),&pStatus)) return pStatus;
     if (PADvalueBITset(flags32)) {
       *no = FALSE;
       return pStatus;          
     }
     if (!sX(ReadVDR(CDF,CDF->fp,tOffset,TRUE,
                     VDR_VDRNEXT,&tOffset,
                     VDR_NULL),&pStatus)) return pStatus;
  }
  /****************************************************************************
  * No pad values specified.
  ****************************************************************************/
  *no = TRUE; 
  return pStatus;
} 
  
/******************************************************************************
* VerifyNoEntriesWritten.
******************************************************************************/
     
STATICforIDL CDFstatus VerifyNoEntriesWritten (CDF, no)
struct CDFstruct *CDF;
Logical *no;
{      
  CDFstatus pStatus = CDF_OK;
  Int32 numAttrs, tOffset, nEntries;
  int attrN;
  /****************************************************************************
  * Read number of attributes and the offset of the first ADR from the GDR.
  ****************************************************************************/
  if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
                  GDR_NUMATTR,&numAttrs, 
                  GDR_ADRHEAD,&tOffset,
                  GDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * Read from ADRs until an entry is found.
  *   Note that if this is a V2.0 CDF, the last ADR will not have an
  * offset of zero for the next ADR.  For that reason, we will loop
  * through the number of attributes read from the GDR (and then stop).
  ****************************************************************************/
  for (attrN = 0; attrN < numAttrs; attrN++) {
     if (!sX(ReadADR(CDF->fp,tOffset,
                     ADR_NgrENTRIES,&nEntries,
                     ADR_NULL),&pStatus)) return pStatus;
     if (nEntries > 0) {
       *no = FALSE;
       return pStatus;
     }
     if (!sX(ReadADR(CDF->fp,tOffset,
                     ADR_NzENTRIES,&nEntries,
                     ADR_NULL),&pStatus)) return pStatus;
     if (nEntries > 0) {
       *no = FALSE;
       return pStatus;
     }
     if (!sX(ReadADR(CDF->fp,tOffset,
                     ADR_ADRNEXT,&tOffset,
                     ADR_NULL),&pStatus)) return pStatus;
  }
  /****************************************************************************
  * No entries detected.
  ****************************************************************************/
  *no = TRUE;
  return pStatus;
}

/******************************************************************************
* DefaultPadValue.
*    `memmove' is used rather than an assignment statement (for the data
* types greater in size than one byte) because there is no guarantee that the
* address which receives the pad value is aligned properly.  If it isn't, a
* bus error could occur on some computers.
******************************************************************************/

STATICforIDL void DefaultPadValue (dataType, numElems, padValue)
Int32 dataType;
Int32 numElems;
void *padValue;
{
  size_t nBytes = (size_t) CDFelemSize (dataType);
  Byte1 *ptr = padValue;
  double padE[2];                  /* The "largest" data type element. */
  int i;
  switch (dataType) {
    case CDF_BYTE:
      *((sChar *) &padE) = (sChar) DEFAULT_BYTE_PADVALUE;
      break;
    case CDF_INT1:
      *((sChar *) &padE) = (sChar) DEFAULT_INT1_PADVALUE;
      break;
    case CDF_UINT1:
      *((uChar *) &padE) = (uChar) DEFAULT_UINT1_PADVALUE;
      break;
    case CDF_INT2:
      *((Int16 *) &padE) = (Int16) DEFAULT_INT2_PADVALUE;
      break;
    case CDF_UINT2:
      *((uInt16 *) &padE) = (uInt16) DEFAULT_UINT2_PADVALUE;
      break;
    case CDF_INT4:
      *((Int32 *) &padE) = (Int32) DEFAULT_INT4_PADVALUE;
      break;
    case CDF_UINT4:
      *((uInt32 *) &padE) = (uInt32) DEFAULT_UINT4_PADVALUE;
      break;
    case CDF_INT8:
      *((Int64 *) &padE) = (Int64) DEFAULT_INT8_PADVALUE;
      break;
    case CDF_REAL4:
      *((float *) &padE) = (float) DEFAULT_REAL4_PADVALUE;
      break;
    case CDF_FLOAT:
      *((float *) &padE) = (float) DEFAULT_FLOAT_PADVALUE;
      break;
    case CDF_REAL8:
      *((double *) &padE) = (double) DEFAULT_REAL8_PADVALUE;
      break;
    case CDF_DOUBLE:
      *((double *) &padE) = (double) DEFAULT_DOUBLE_PADVALUE;
      break;
    case CDF_EPOCH:
      *((double *) &padE) = (double) DEFAULT_EPOCH_PADVALUE;
      break;
    case CDF_EPOCH16:                                             
      *((double *) &padE) = (double) DEFAULT_EPOCH16_PADVALUE;    
      *(((double *) &padE) + 1) = (double) DEFAULT_EPOCH16_PADVALUE;
      break;
    case CDF_TIME_TT2000:
      *((long long *) &padE) = (long long) DEFAULT_TT2000_PADVALUE;
      break;
    case CDF_CHAR:
      *((sChar *) &padE) = (sChar) DEFAULT_CHAR_PADVALUE;
      break;
    case CDF_UCHAR:
      *((uChar *) &padE) = (uChar) DEFAULT_UCHAR_PADVALUE;
      break;
  }
  for (i = 0; i < numElems; i++, ptr += nBytes) memmove (ptr, &padE, nBytes);
  return;
}

/******************************************************************************
* DefaultPadValuePre350.
*    Reset the pad value to its pre-3.5.0 value for pre-3.5.0 file.
******************************************************************************/
STATICforIDL void DefaultPadValuePre350 (dataType, numElems, padValue)

Int32 dataType;
Int32 numElems;
void *padValue;
{
  size_t nBytes = (size_t) CDFelemSize (dataType);
  Byte1 *ptr = padValue;
  double padE[2];                  /* The "largest" data type element. */
  int i;
  switch (dataType) {
    case CDF_BYTE:
      *((sChar *) &padE) = (sChar) 0;
      break;
    case CDF_INT1:
      *((sChar *) &padE) = (sChar) 0;
      break;
    case CDF_UINT1:
      *((uChar *) &padE) = (uChar) 0;
      break;
    case CDF_INT2:
      *((Int16 *) &padE) = (Int16) 0;
      break;
    case CDF_UINT2:
      *((uInt16 *) &padE) = (uInt16) 0;
      break;
    case CDF_INT4:
      *((Int32 *) &padE) = (Int32) 0;
      break;
    case CDF_UINT4:
      *((uInt32 *) &padE) = (uInt32) 0;
      break;
    case CDF_INT8:
      *((Int64 *) &padE) = (Int64) -9223372036854775807LL-1;
      break;
    case CDF_REAL4:
      *((float *) &padE) = (float) 0.0;
      break;
    case CDF_FLOAT:
      *((float *) &padE) = (float) 0.0;
      break;
    case CDF_REAL8:
      *((double *) &padE) = (double) 0.0;
      break;
    case CDF_DOUBLE:
      *((double *) &padE) = (double) 0.0;
      break;
    case CDF_EPOCH:
      *((double *) &padE) = (double) 0.0;
      break;
    case CDF_EPOCH16:                                             
      *((double *) &padE) = (double) 0.0;    
      *(((double *) &padE) + 1) = (double) 0.0;
      break;
    case CDF_TIME_TT2000:
      *((long long *) &padE) = (long long) -9223372036854775807LL;
      break;
    case CDF_CHAR:
      *((sChar *) &padE) = (sChar) ' ';
      break;
    case CDF_UCHAR:
      *((uChar *) &padE) = (uChar) ' ';
      break;
  }
  for (i = 0; i < numElems; i++, ptr += nBytes) memmove (ptr, &padE, nBytes);
  return;
}

/******************************************************************************
* DefaultPadBuffer.
******************************************************************************/

STATICforIDL CDFstatus DefaultPadBuffer (CDF, Var, nValues, buffer)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 nValues;
void *buffer;
{
  CDFstatus pStatus = CDF_OK;
  Byte1 *tBuffer = buffer; Int32 i; Int32 dataType, numElems;
  if (!sX(ReadVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
		  VDR_DATATYPE,&dataType,
		  VDR_NUMELEMS,&numElems,
		  VDR_NULL),&pStatus)) return pStatus;
  for (i = 0; i < nValues; i++, tBuffer += (int) Var->NvalueBytes) {
     DefaultPadValuePre350 (dataType, numElems, tBuffer);
  }
  return pStatus;
}

/******************************************************************************
* PadBuffer.
******************************************************************************/

STATICforIDL CDFstatus PadBuffer (CDF, Var, nValues, buffer)
struct CDFstruct *CDF;          /* Pointer to CDF. */
struct VarStruct *Var;          /* Pointer to variable. */
Int32 nValues;                   /* Number of values in buffer. */
void *buffer;                   /* Buffer to pad. */
{
  CDFstatus pStatus = CDF_OK;
  Int32 flags, dataType, numElems;
  /****************************************************************************
  * Read the flags, data type, and number of elements fields of the VDR.
  ****************************************************************************/
  if (!sX(ReadVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
		  VDR_FLAGS,&flags,
		  VDR_DATATYPE,&dataType,
		  VDR_NUMELEMS,&numElems,
		  VDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * If a pad value has been specified for the variable, read the pad value
  * from the VDR and duplicate for the desired number of values.  Otherwise,
  * copy the desired number of default pad values into the buffer.  Then
  * convert the padded buffer into the desired decoding.
  ****************************************************************************/
  if (PADvalueBITset(flags)) {
    Byte1 *tBuffer = buffer; Int32 valueN;
    if (!sX(ReadVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
		    VDR_PADVALUE,tBuffer,
		    VDR_NULL),&pStatus)) return pStatus;
    for (valueN = 1; valueN < nValues; valueN++) {
       memmove (tBuffer + ((size_t) Var->NvalueBytes), tBuffer,
		(size_t) Var->NvalueBytes);
       tBuffer += (size_t) Var->NvalueBytes;
    }
    if (!sX(ConvertBuffer(CDF->encoding,CDF->decoding,
			  CDF->negToPosFp0,dataType,
			  (nValues * numElems),
			  buffer,buffer),&pStatus)) return pStatus;
  }
  else {
    if (!sX(DefaultPadBuffer(CDF,Var,nValues,buffer),&pStatus)) return pStatus;
    if (!sX(ConvertBuffer(HostEncoding(),CDF->decoding,
			  CDF->negToPosFp0,dataType,
			  (nValues * numElems),
			  buffer,buffer),&pStatus)) return pStatus;
  }
  return pStatus;
}

/******************************************************************************
* HostEncoding.
*    Returns encoding type of host machine.
******************************************************************************/

STATICforIDL Int32 HostEncoding ()
{
#if defined(sun)
  return ((Int32) SUN_ENCODING);
#endif
#if defined(vax)
  return ((Int32) VAX_ENCODING);
#endif
#if defined(MIPSEL)
  return ((Int32) DECSTATION_ENCODING);
#endif
#if defined(MIPSEB)
  return ((Int32) SGi_ENCODING);
#endif
#if defined(IBMPC) || defined(macosXintel)
  return ((Int32) IBMPC_ENCODING);
#endif
#if defined(IBMRS)
  return ((Int32) IBMRS_ENCODING);
#endif
#if defined(HP)
  return ((Int32) HP_ENCODING);
#endif
#if defined(NeXT)
  return ((Int32) NeXT_ENCODING);
#endif
#if defined(alphaosf)
  return ((Int32) ALPHAOSF1_ENCODING);
#endif
#if defined(alphavmsD) || defined(posixSHELLalphaD)
  return ((Int32) ALPHAVMSd_ENCODING);
#endif
#if defined(alphavmsG) || defined(posixSHELLalphaG)
  return ((Int32) ALPHAVMSg_ENCODING);
#endif
#if defined(alphavmsI) || defined(posixSHELLalphaI)
  return ((Int32) ALPHAVMSi_ENCODING);
#endif
#if defined(mac) || defined(POWERPC) || defined(macosXppc)
  return ((Int32) PPC_ENCODING);
#endif
}

/******************************************************************************
* HostDecoding.
*    Returns decoding type of host machine.
******************************************************************************/

STATICforIDL Int32 HostDecoding ()
{
#if defined(sun)
  return ((Int32) SUN_DECODING);
#endif
#if defined(vax)
  return ((Int32) VAX_DECODING);
#endif
#if defined(MIPSEL)
  return ((Int32) DECSTATION_DECODING);
#endif
#if defined(MIPSEB)
  return ((Int32) SGi_DECODING);
#endif
#if defined(IBMPC) || defined(macosXintel)
  return ((Int32) IBMPC_DECODING);
#endif
#if defined(IBMRS)
  return ((Int32) IBMRS_DECODING);
#endif
#if defined(HP)
  return ((Int32) HP_DECODING);
#endif
#if defined(NeXT)
  return ((Int32) NeXT_DECODING);
#endif
#if defined(alphaosf)
  return ((Int32) ALPHAOSF1_DECODING);
#endif
#if defined(alphavmsD) || defined(posixSHELLalphaD)
  return ((Int32) ALPHAVMSd_DECODING);
#endif
#if defined(alphavmsG) || defined(posixSHELLalphaG)
  return ((Int32) ALPHAVMSg_DECODING);
#endif
#if defined(alphavmsI) 
  return ((Int32) ALPHAVMSi_DECODING);
#endif
#if defined(mac) || defined(POWERPC) || defined(macosXppc)
  return ((Int32) PPC_DECODING);
#endif
}

/******************************************************************************
* IntegerOrder.
*    Returns integer order based on encoding (decoding).
******************************************************************************/

STATICforIDL int IntegerOrder (ed)
Int32 ed;                /* Encoding/decoding. */
{
  switch (ed) {
    case NETWORK_ENCODING:
    case SUN_ENCODING:
    case SGi_ENCODING:
    case IBMRS_ENCODING:
    case HP_ENCODING:
    case NeXT_ENCODING:
    case PPC_ENCODING:
      return BIGendianORDER;
    case DECSTATION_ENCODING:
    case ALPHAOSF1_ENCODING:
    case ALPHAVMSd_ENCODING:
    case ALPHAVMSg_ENCODING:
    case ALPHAVMSi_ENCODING:
    case IBMPC_ENCODING:
    case VAX_ENCODING:
      return LITTLEendianORDER;
    default:
      return 0;                 /* Internal error. */
  }
}

/******************************************************************************
* FpType.
*    Returns floating-point type based on encoding (decoding).
******************************************************************************/

STATICforIDL int FpType (ed)
Int32 ed;                /* Encoding/decoding. */
{
  switch (ed) {
    case NETWORK_ENCODING:
    case SUN_ENCODING:
    case SGi_ENCODING:
    case IBMRS_ENCODING:
    case HP_ENCODING:
    case NeXT_ENCODING:
    case PPC_ENCODING:
      return FP_1;
    case DECSTATION_ENCODING:
    case ALPHAOSF1_ENCODING:
    case IBMPC_ENCODING:
    case ALPHAVMSi_ENCODING:
      return FP_2;
    case VAX_ENCODING:
    case ALPHAVMSd_ENCODING:
      return FP_3;
    case ALPHAVMSg_ENCODING:
      return FP_4;
    default:
      return 0;
  }
}

/******************************************************************************
* CDFelemSize.
******************************************************************************/

STATICforIDL int CDFelemSize (dataType)
long dataType;
{
  switch (dataType) {
    case CDF_BYTE: return 1;
    case CDF_INT1: return 1;
    case CDF_INT2: return 2;
    case CDF_INT4: return 4;
    case CDF_INT8: return 8;
    case CDF_UINT1: return 1;
    case CDF_UINT2: return 2;
    case CDF_UINT4: return 4;
    case CDF_REAL4: return 4;
    case CDF_REAL8: return 8;
    case CDF_FLOAT: return 4;
    case CDF_DOUBLE: return 8;
    case CDF_EPOCH: return 8;
    case CDF_EPOCH16: return 16;  
    case CDF_TIME_TT2000: return 8;  
    case CDF_CHAR: return 1;
    case CDF_UCHAR: return 1;
  }
  return 0;
}

/******************************************************************************
* EquivDataTypes.
******************************************************************************/

STATICforIDL Logical EquivDataTypes (dataType1, dataType2)
Int32 dataType1;
Int32 dataType2;
{
  switch (dataType1) {
    case CDF_BYTE:
    case CDF_INT1:
    case CDF_UINT1:
    case CDF_CHAR:
    case CDF_UCHAR:
      switch (dataType2) {
        case CDF_BYTE:
        case CDF_INT1:
        case CDF_UINT1:
        case CDF_CHAR:
        case CDF_UCHAR:
	  return TRUE;
        default:
	  return FALSE;
      }
    case CDF_INT2:
    case CDF_UINT2:
      switch (dataType2) {
        case CDF_INT2:
        case CDF_UINT2:
	  return TRUE;
        default:
	  return FALSE;
      }
    case CDF_INT4:
    case CDF_UINT4:
      switch (dataType2) {
        case CDF_INT4:
        case CDF_UINT4:
	  return TRUE;
        default:
	  return FALSE;
      }
    case CDF_INT8:
    case CDF_TIME_TT2000:
      switch (dataType2) {
        case CDF_INT8:
        case CDF_TIME_TT2000:
	  return TRUE;
        default:
	  return FALSE;
      }
    case CDF_REAL4:
    case CDF_FLOAT:
      switch (dataType2) {
        case CDF_REAL4:
        case CDF_FLOAT:
	  return TRUE;
        default:
	  return FALSE;
      }
    case CDF_REAL8:
    case CDF_DOUBLE:
    case CDF_EPOCH:
      switch (dataType2) {
        case CDF_REAL8:
        case CDF_DOUBLE:
        case CDF_EPOCH:
	  return TRUE;
        default:
	  return FALSE;
      }
    case CDF_EPOCH16:
      switch (dataType2) {
        case CDF_EPOCH16:
          return TRUE;
        default: 
          return FALSE;
      }
  }
  return FALSE;			/* CDF_INTERNAL_ERROR or CORRUPTED_V2_CDF? */
}

/******************************************************************************
* strcmpITB.  Do a STRing CoMPare Ignoring any Trailing Blanks.
******************************************************************************/

STATICforIDL int strcmpITB (string1, string2)
char *string1;
char *string2;
{
  size_t len1 = strlen(string1);
  size_t len2 = strlen(string2);
  while (len1 > 0 && string1[len1-1] == ' ') len1--;
  while (len2 > 0 && string2[len2-1] == ' ') len2--;
  if (len1 == len2)
    return strncmp (string1, string2, len1);
  else
    return strcmp (string1, string2);
}


/******************************************************************************
* FreeCDFid.
* Free a CDF's dynamically allocated memory.
******************************************************************************/

STATICforIDL void FreeCDFid (CDF, aborting)
struct CDFstruct *CDF;
Logical aborting;
{
  /****************************************************************************
  * Free file names/paths.
  ****************************************************************************/
  if (CDF->CDFname != NULL) cdf_FreeMemory (CDF->CDFname, NULL);
  if (CDF->scratchDir != NULL) cdf_FreeMemory (CDF->scratchDir, NULL);
  /****************************************************************************
  * Free rVariable structures.
  ****************************************************************************/
  if (CDF->rVars != NULL) {
    int varNum;
    for (varNum = 0; varNum < CDF->NrVars; varNum++) {
       if (CDF->rVars[varNum] != NULL) cdf_FreeMemory (CDF->rVars[varNum], NULL);
    }
    cdf_FreeMemory (CDF->rVars, NULL);
  }
  /****************************************************************************
  * Free zVariable structures.
  ****************************************************************************/
  if (CDF->zVars != NULL) {
    int varNum;
    for (varNum = 0; varNum < CDF->NzVars; varNum++) {
       if (CDF->zVars[varNum] != NULL) cdf_FreeMemory (CDF->zVars[varNum], NULL);
    }
    cdf_FreeMemory (CDF->zVars, NULL);
  }
  /****************************************************************************
  * Free CDF structure.  The CDF structure's magic number is "killed" just in
  * case the application tries to use the CDFid again.
  ****************************************************************************/
  if (sizeof(CDF) == 8) cdfid_FreeMemory (CDF, NULL);
  if (aborting)
    CDF->magic = ABORTEDid_MAGIC_NUMBER;
  else {
    CDF->magic = KILLEDid_MAGIC_NUMBER;
    cdf_FreeMemory (CDF, NULL);
  }
  return;
}

/******************************************************************************
* CloseLRUvar.
* Close a variable/zVariable file to free a file pointer.
******************************************************************************/

STATICforIDL CDFstatus CloseLRUvar (CDF)
struct CDFstruct *CDF;
{
  struct VarStruct *Var, *oldestVar = NULL;
  uLongx oldest_access = CDF->pseudo_clock; int varNum;
  /****************************************************************************
  * Scan through rVariables looking for oldest access.
  ****************************************************************************/
  for (varNum = 0; varNum < CDF->NrVars; varNum++) {
    Var = CDF->rVars[varNum];
    if (Var != NULL) {
      if (Var->fp != NULL) {
	if (Var->accessed_at < oldest_access) {
	  oldest_access = Var->accessed_at;
	  oldestVar = Var;
	}
      }
    }
  }
  /****************************************************************************
  * Scan through zVariables looking for oldest access.
  ****************************************************************************/
  for (varNum = 0; varNum < CDF->NzVars; varNum++) {
    Var = CDF->zVars[varNum];
    if (Var != NULL) {
      if (Var->fp != NULL) {
	if (Var->accessed_at < oldest_access) {
	  oldest_access = Var->accessed_at;
	  oldestVar = Var;
	}
      }
    }
  }
  /****************************************************************************
  * If a variable was found, close the associated file.
  ****************************************************************************/
  if (oldestVar != NULL) {
    if (!CLOSEv(oldestVar->fp,NULL,NULL)) {
      oldestVar->fp = NULL;
      return VAR_CLOSE_ERROR;
    }
    oldestVar->fp = NULL;
  }
  return CDF_OK;
}

/******************************************************************************
* CheckEntryOp.
******************************************************************************/

STATICforIDL CDFstatus CheckEntryOp (CDF, entryType)
struct CDFstruct *CDF;
int entryType;
{
  Int32 scope; CDFstatus pStatus = CDF_OK;
  if (!sX(ReadADR(CDF->fp,CDF->CURattrOffset,
                  ADR_SCOPE,&scope,
                  ADR_NULL),&pStatus)) {
    AbortAccess (CDF, UPDATE, noDELETE);
    return pStatus;
  }
  if (GLOBALscope(scope)) {
    if (entryType != gENTRYt) return ILLEGAL_FOR_SCOPE;
  }
  else {
    if (entryType == gENTRYt) return ILLEGAL_FOR_SCOPE;
    if (BADzOP(CDF,entryType == rENTRYt)) return ILLEGAL_IN_zMODE;
  }
  return pStatus;
}

/******************************************************************************
* SetCURgrEntry.
******************************************************************************/

STATICforIDL CDFstatus SetCURgrEntry (CDF, useCurrent, entryNum)
struct CDFstruct *CDF;
Logical useCurrent;	/* TRUE if current g/rEntry offset can be used to speed
			   up the search. */
Int32 entryNum;		/* The new g/rEntry number. */
{
  CDFstatus pStatus = CDF_OK, tStatus;
  Int32 scope, offset, attrNum, attrNumX, entryNumX, nextOffset;
  long read_only_mode;
  /****************************************************************************
  * Check if the new g/rEntry number is the reserved entry number.
  ****************************************************************************/
  if (entryNum == RESERVED_ENTRYNUM) {
    CDF->CURgrEntryNum = RESERVED_ENTRYNUM;
    CDF->fp->CurAEDRIndex = RESERVED_ENTRYNUM;
    CDF->CURgrEntryOffset = RESERVED_ENTRYOFFSET;
    return pStatus;
  }
  /****************************************************************************
  * Check that a current attribute is selected.
  ****************************************************************************/
  if (CDF->CURattrOffset == RESERVED_ATTROFFSET) {
    CDF->CURgrEntryNum = entryNum;
    CDF->fp->CurAEDRIndex = RESERVED_ENTRYNUM;
    CDF->CURgrEntryOffset = RESERVED_ENTRYOFFSET;
    return pStatus;
  }
  /****************************************************************************
  * Get the scope and number of the current attribute. If READONLYon, the 
  * scope and number are already in th mteadata structures.
  ****************************************************************************/
  pStatus = CDFlib(CONFIRM_, CDF_READONLY_MODE_, &read_only_mode, NULL_);
  if (pStatus != CDF_OK) return pStatus;
  if (read_only_mode == READONLYon)
  {
      scope = CDF->fp->ADRList[CDF->fp->CurADRIndex]->Scope;
      attrNum = CDF->fp->CurADRIndex;
  }
  else
  {
      if (!sX(ReadADR(CDF->fp,CDF->CURattrOffset,
                      ADR_SCOPE,&scope,
                      ADR_NUM,&attrNum,
                      ADR_NULL),&pStatus)) return pStatus;
  };
  /****************************************************************************
  * If the current attribute is variable-scoped and zMode is on, then the
  * current g/rEntry offset is n/a.
  ****************************************************************************/
  if (VARIABLEscope(scope) && zModeON(CDF)) {
    CDF->CURgrEntryNum = entryNum;
    CDF->fp->CurAEDRIndex = RESERVED_ENTRYNUM;
    CDF->CURgrEntryOffset = RESERVED_ENTRYOFFSET;
    return pStatus;
  }
  /****************************************************************************
  * IF READONLYoff, check if the next entry is the one being searched for.
  * For this to be the case, an entry must currently be selected and must be
  * associated with the current attribute, the next entry must exist, and the
  * next entry's number must be the entry number being searched for.  But
  * don't try this if a V2.0 CDF because of the bad terminating offset of the
  * AEDR linked lists in those CDFs.
  ****************************************************************************/
  if (read_only_mode == READONLYoff && useCurrent && 
      !CDF->badTerminatingOffsets) {
    if (CDF->CURgrEntryOffset != RESERVED_ENTRYOFFSET) {
      if (!sX(ReadAEDR(CDF->fp,CDF->CURgrEntryOffset,
		       AEDR_ATTRNUM,&attrNumX,
		       AEDR_AEDRNEXT,&nextOffset,
		       AEDR_NULL),&pStatus)) return pStatus;
      if (attrNumX == attrNum && nextOffset != 0) {
	if (!sX(ReadAEDR(CDF->fp,nextOffset,
			 AEDR_NUM,&entryNumX,
			 AEDR_NULL),&pStatus)) return pStatus;
	if (entryNumX == entryNum) {
	  CDF->CURgrEntryNum = entryNum;
	  CDF->CURgrEntryOffset = nextOffset;
	  return pStatus;
	}
      }
    }
  }
  /****************************************************************************
  * Search the list of AEDRs for the entry.
  ****************************************************************************/
  tStatus = FindEntryByNumber (CDF, CDF->CURattrOffset, FALSE, entryNum,
			       &offset);
  switch (tStatus) {
    case CDF_OK:
      break;
    case NO_SUCH_ENTRY:
      offset = RESERVED_ENTRYOFFSET;
      break;
    default:
      return tStatus;
  }
  CDF->CURgrEntryNum = entryNum;
  CDF->CURgrEntryOffset = offset;
  return pStatus;
}

/******************************************************************************
* SetCURzEntry.
******************************************************************************/

STATICforIDL CDFstatus SetCURzEntry (CDF, useCurrent, entryNum)
struct CDFstruct *CDF;
Logical useCurrent;	/* TRUE if current zEntry offset can be used to speed
			   up the search. */
Int32 entryNum;		/* The new zEntry number. */
{
  CDFstatus pStatus = CDF_OK, tStatus;
  Int32 scope, offset, attrNum, attrNumX, entryNumX, nextOffset;
  Logical zEntry;
  Int32 entryN;
  long read_only_mode;
  /****************************************************************************
  * Check if the new zEntry number is the reserved entry number.
  ****************************************************************************/
  if (entryNum == RESERVED_ENTRYNUM) {
    CDF->CURzEntryNum = RESERVED_ENTRYNUM;
    CDF->fp->CurAEDRIndex = RESERVED_ENTRYNUM;
    CDF->CURzEntryOffset = RESERVED_ENTRYOFFSET;
    return pStatus;
  }
  /****************************************************************************
  * Check that a current attribute is selected.
  ****************************************************************************/
  if (CDF->CURattrOffset == RESERVED_ATTROFFSET) {
    CDF->CURzEntryNum = entryNum;
    CDF->fp->CurAEDRIndex = RESERVED_ENTRYNUM;
    CDF->CURzEntryOffset = RESERVED_ENTRYOFFSET;
    return pStatus;
  }
  /****************************************************************************
  * Read the scope and number of the current attribute. If READONLYon, they
  * are already in memory.
  ****************************************************************************/
  pStatus = CDFlib(CONFIRM_, CDF_READONLY_MODE_, &read_only_mode, NULL_);
  if (pStatus != CDF_OK) return pStatus;
  if (read_only_mode == READONLYon)
  {
      scope = CDF->fp->ADRList[CDF->fp->CurADRIndex]->Scope;
      attrNum = CDF->fp->CurADRIndex;
  }
  else
  {
      if (!sX(ReadADR(CDF->fp,CDF->CURattrOffset,
                      ADR_SCOPE,&scope,
                      ADR_NUM,&attrNum,
                      ADR_NULL),&pStatus)) return pStatus;
  };
  /****************************************************************************
  * If the current attribute is global-scoped, then the current zEntry offset
  * is n/a.
  ****************************************************************************/
  if (GLOBALscope(scope)) {
    CDF->CURzEntryNum = entryNum;
    CDF->fp->CurAEDRIndex = RESERVED_ENTRYNUM;
    CDF->CURzEntryOffset = RESERVED_ENTRYOFFSET;
    return pStatus;
  }
  /****************************************************************************
  * Determine if a AgrEDR or AzEDR and the true entry number.
  ****************************************************************************/
  if (zModeON(CDF)) {
    if (entryNum < CDF->NrVars) {
      zEntry = FALSE;
      entryN = entryNum;
    }
    else {
      zEntry = TRUE;
      entryN = entryNum - CDF->NrVars;
    }
  }
  else {
    zEntry = TRUE;
    entryN = entryNum;
  }
  /****************************************************************************
  * IF READONLYoff, check if the next entry is the one being searched for.
  * For this to be the case, an entry must currently be selected and must be
  * associated with the current attribute, the next entry must exist, and the
  * next entry's number must be the entry number being searched for.  But don't
  * try this if a V2.0 CDF because of the bad terminating offset of the AEDR
  * linked lists in those CDFs.
  ****************************************************************************/
  if (read_only_mode == READONLYoff && useCurrent && 
      !CDF->badTerminatingOffsets) {
    if (CDF->CURzEntryOffset != RESERVED_ENTRYOFFSET) {
      if (!sX(ReadAEDR(CDF->fp,CDF->CURzEntryOffset,
		       AEDR_ATTRNUM,&attrNumX,
		       AEDR_AEDRNEXT,&nextOffset,
		       AEDR_NULL),&pStatus)) return pStatus;
      if (attrNumX == attrNum && nextOffset != 0) {
	if (!sX(ReadAEDR(CDF->fp,nextOffset,
			 AEDR_NUM,&entryNumX,
			 AEDR_NULL),&pStatus)) return pStatus;
	if (entryNumX == entryN) {
	  CDF->CURzEntryNum = entryNum;
	  CDF->CURzEntryOffset = nextOffset;
	  return pStatus;
	}
      }
    }
  }
  /****************************************************************************
  * Search the list of AEDRs for the entry.
  ****************************************************************************/
  tStatus = FindEntryByNumber (CDF, CDF->CURattrOffset, zEntry, entryN,
			       &offset);
  switch (tStatus) {
    case CDF_OK:
      break;
    case NO_SUCH_ENTRY:
      offset = RESERVED_ENTRYOFFSET;
      break;
    default:
      return tStatus;
  }
  CDF->CURzEntryNum = entryNum;
  CDF->CURzEntryOffset = offset;
  return pStatus;
}

/******************************************************************************
* Int32ToCDFid.
******************************************************************************/

STATICforIDL CDFid Int32ToCDFid (id)
Int32 id;
{
#if defined(alphaosf) || defined(IRIX64bit) || defined(_LP64) || \
    defined(__amd64) || defined(__x86_64__) || defined(__ia64__) || \
    defined(__PPC64__) || defined(__ppc64__) || defined(_M_X64)
  return cdfid_getCDFid (id, NULL);
#else
  return ((CDFid) id);
#endif
}

/******************************************************************************
* CDFidToInt32.
*    On 64-bit machines (OSF/1, IRIX 6.x and Solaris in 64-bit mode), 
*    truncation may occur when the pointer (CDFid) is assigned to a 32-bit 
*    integer.
******************************************************************************/

STATICforIDL Int32 CDFidToInt32 (id)
CDFid id;
{
#if defined(alphaosf) || defined(IRIX64bit) || defined(_LP64) || \
    defined(__amd64) || defined(__x86_64__) || defined(__ia64__) || \
    defined(__PPC64__) || defined(__ppc64__) || defined(_M_X64)
  union {
    CDFid id;
    Int32 i[2];
  } u;
  u.id = id;
  cdfid_AllocateMemory (id, NULL);
# if defined(alphaosf) || defined(__amd64) || defined(__x86_64__) || \
     defined(__ia64__) || defined(_M_X64)
    return u.i[0];
# endif
# if defined(IRIX64bit) || (defined(_LP64) && defined(_BIG_ENDIAN)) || \
     defined(__PPC64__) || defined(__ppc64__)
    return u.i[1];
# endif
#else
  return ((Int32) id);
#endif
}

/******************************************************************************
* VariableType.
******************************************************************************/

STATICforIDL CDFstatus VariableType (CDF, vdrOffset, zVar, vType)
struct CDFstruct *CDF;
Int32 vdrOffset;
Logical zVar;
int *vType;
{
  CDFstatus pStatus = CDF_OK; Int32 flags, sRecords;
  if (!sX(ReadVDR(CDF,CDF->fp,vdrOffset,zVar,
		  VDR_FLAGS,&flags,
		  VDR_sRECORDS,&sRecords,
		  VDR_NULL),&pStatus)) return pStatus;
  if (CDF->singleFile) {
    if (VARcompressionBITset(flags) && SPARSEarraysBITset(flags)) {
      return CORRUPTED_V2_CDF;
    }
    if (sRecords == NO_SPARSERECORDS) {
      *vType = STANDARD_;
      if (VARcompressionBITset(flags)) *vType = COMPRESSED_;
      if (SPARSEarraysBITset(flags)) *vType = SPARSE_ARRAYS_;
    }
    else {
      *vType = SPARSE_RECORDS_;
      if (VARcompressionBITset(flags)) *vType = SPARSE_COMPRESSED_RECORDS_;
      if (SPARSEarraysBITset(flags)) *vType = SPARSE_RECORDS_AND_ARRAYS_;
    }
  }
  else {
    *vType = IN_MULTI_;
    if (VARcompressionBITset(flags)) return CORRUPTED_V2_CDF;
    if (SPARSEarraysBITset(flags)) return CORRUPTED_V2_CDF;
    if (sRecords != NO_SPARSERECORDS) return CORRUPTED_V2_CDF;
  }
  return pStatus;
}

/******************************************************************************
* CompressionParmsCount.
******************************************************************************/

STATICforIDL int CompressionParmsCount (cType)
Int32 cType;
{
  switch (cType) {
    case NO_COMPRESSION: return 0;
    case RLE_COMPRESSION: return NUM_RLE_PARMS;
    case HUFF_COMPRESSION: return NUM_HUFF_PARMS;
    case AHUFF_COMPRESSION: return NUM_AHUFF_PARMS;
    case GZIP_COMPRESSION: return NUM_GZIP_PARMS;
  }
  return 0;	/* CORRUPTED_V2_CDF or CDF_INTERNAL_ERROR? */
}

/******************************************************************************
* SparsenessParmsCount.
******************************************************************************/

STATICforIDL int SparsenessParmsCount (sArraysType)
Int32 sArraysType;
{
  switch (sArraysType) {
    case NO_SPARSEARRAYS:
      return 0;
  }
  return 0;	/* CORRUPTED_V2_CDF or CDF_INTERNAL_ERROR? */
}

/******************************************************************************
* Compress.
******************************************************************************/

STATICforIDL CDFstatus Compress (iFp, iOffset, iSize, iError, cType, cParms,
				 oFp, oOffset, oSize, oError)
vFILE *iFp;
Int32 iOffset;
Int32 iSize;
CDFstatus iError;
Int32 cType;
Int32 cParms[];
vFILE *oFp;
Int32 oOffset;
Int32 *oSize;
CDFstatus oError;
{
  CDFstatus pStatus = CDF_OK;
  switch (cType) {
    case RLE_COMPRESSION: {
      if (cParms[0] != RLE_OF_ZEROs) return UNKNOWN_COMPRESSION;
      if (!sX(CompressRLE0(iFp,iOffset,iSize,iError,
			   oFp,oOffset,oSize,oError),&pStatus)) return pStatus;
      break;
    }
    case HUFF_COMPRESSION:
      if (cParms[0] != OPTIMAL_ENCODING_TREES) return UNKNOWN_COMPRESSION;
      if (!sX(CompressHUFF0(iFp,iOffset,
			    iSize,iError,oFp,
			    oOffset,oSize,oError),&pStatus)) return pStatus;
      break;
    case AHUFF_COMPRESSION:
      if (cParms[0] != OPTIMAL_ENCODING_TREES) return UNKNOWN_COMPRESSION;
      if (!sX(CompressAHUFF0(iFp,iOffset,
			     iSize,iError,oFp,
			     oOffset,oSize,oError),&pStatus)) return pStatus;
      break;
    case GZIP_COMPRESSION:
      if (!INCLUSIVE(1,cParms[0],9)) return UNKNOWN_COMPRESSION;
      if (!sX(CompressGZIP(iFp,iOffset,iSize,iError,
			   oFp,oOffset,oSize,
			   oError,cParms[0]),&pStatus)) return pStatus;
      break;
    default:
      return UNKNOWN_COMPRESSION;
  }
  return pStatus;
}

/******************************************************************************
* Decompress.
******************************************************************************/

STATICforIDL CDFstatus Decompress (iFp, iOffset, iSize, iError, cType, cParms,
				   oFp, oOffset, oError)
vFILE *iFp;
Int32 iOffset;
Int32 iSize;
CDFstatus iError;
Int32 cType;
Int32 cParms[];
vFILE *oFp;
Int32 oOffset;
CDFstatus oError;
{
  CDFstatus pStatus = CDF_OK;
  switch (cType) {
    case RLE_COMPRESSION: {
      if (cParms[0] != RLE_OF_ZEROs) return UNKNOWN_COMPRESSION;
      if (!sX(DecompressRLE0(iFp,iOffset,iSize,iError,
			     oFp,oOffset,oError),&pStatus)) return pStatus;
      break;
    }
    case HUFF_COMPRESSION:
      if (cParms[0] != OPTIMAL_ENCODING_TREES) return UNKNOWN_COMPRESSION;
      if (!sX(DecompressHUFF0(iFp,iOffset,iError,
			      oFp,oOffset,oError),&pStatus)) return pStatus;
      break;
    case AHUFF_COMPRESSION:
      if (cParms[0] != OPTIMAL_ENCODING_TREES) return UNKNOWN_COMPRESSION;
      if (!sX(DecompressAHUFF0(iFp,iOffset,iError,
			       oFp,oOffset,oError),&pStatus)) return pStatus;
      break;
    case GZIP_COMPRESSION:
      if (!INCLUSIVE(1,cParms[0],9)) return UNKNOWN_COMPRESSION;
      if (!sX(DecompressGZIP(iFp,iOffset,iSize,iError,
			     oFp,oOffset,oError),&pStatus)) return pStatus;
      break;
    default:
      return UNKNOWN_COMPRESSION;
  }
  return pStatus;
}

/******************************************************************************
* DecompressToStage.
******************************************************************************/

STATICforIDL CDFstatus DecompressToStage (CDF, Var, offset, uSize)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 offset;
Int32 uSize;
{
  CDFstatus pStatus = CDF_OK; Int32 irType, tOffset;
  if (!sX(ReadIrType(CDF->fp,offset,&irType),&pStatus)) return pStatus;
  switch (irType) {
    case VVR_: {
      tOffset = offset + VVR_BASE_SIZE;
      if (!sX(CopyBytes(CDF->fp,tOffset,
			CDF_READ_ERROR,
			uSize,CDF->stage.fp,
			Var->stage.areaOffset,
			SCRATCH_WRITE_ERROR),&pStatus)) return pStatus;
      break;
    }
    case CVVR_: {
      struct CVVRstruct CVVR;
      if (!sX(ReadCVVR(CDF->fp,offset,
		       CVVR_RECORDx,&CVVR,
		       CVVR_NULL),&pStatus)) return pStatus;
      tOffset = offset + CVVR_BASE_SIZE;
      if (!sX(Decompress(CDF->fp,tOffset,
			 CVVR.cSize,CDF_READ_ERROR,
			 Var->cType,Var->cParms,CDF->stage.fp,
			 Var->stage.areaOffset,
			 SCRATCH_WRITE_ERROR),&pStatus)) return pStatus;
      break;
    }
    default:
      return CORRUPTED_V2_CDF;
  }
  return pStatus;
}

/******************************************************************************
* ResetReadOnlyState.
******************************************************************************/

STATICforIDL void ResetReadOnlyState (CDF)
struct CDFstruct *CDF;
{
    Int32 i, j;
    /*************************************************************************
    * Free all allocated memory associated with the metadata READONLYon data
    * structures and clear all the state variables.
    *************************************************************************/
    if (CDF->fp != NULL && CDF->fp->GDR != NULL)
    {
        for (i = 0; i < CDF->fp->GDR->NumAttr; i++)
        {
            if (CDF->fp->ADRList[i] != NULL)
            {
                for (j = 0; j <= CDF->fp->ADRList[i]->MAXgrEntry; j++)
                {
                    if (CDF->fp->ADRList[i]->grAEDRList[j] != NULL)
                    {
                        cdf_FreeMemory(
                           CDF->fp->ADRList[i]->grAEDRList[j]->Value, NULL);
                        CDF->fp->ADRList[i]->grAEDRList[j]->Value = NULL;
                        cdf_FreeMemory(CDF->fp->ADRList[i]->grAEDRList[j],
                                       NULL);
                        CDF->fp->ADRList[i]->grAEDRList[j] = NULL;
                    };
                };
                if (CDF->fp->ADRList[i]->grAEDRList != NULL)
                  cdf_FreeMemory(CDF->fp->ADRList[i]->grAEDRList, NULL);

                for (j = 0; j <= CDF->fp->ADRList[i]->MAXzEntry; j++)
                {
                    if (CDF->fp->ADRList[i]->zAEDRList[j] != NULL)
                    {
                        cdf_FreeMemory(
                           CDF->fp->ADRList[i]->zAEDRList[j]->Value, NULL);
                        CDF->fp->ADRList[i]->zAEDRList[j]->Value = NULL;
                        cdf_FreeMemory(CDF->fp->ADRList[i]->zAEDRList[j], NULL);
                        CDF->fp->ADRList[i]->zAEDRList[j] = NULL;
                    };
                };
                if (CDF->fp->ADRList[i]->zAEDRList != NULL)
                  cdf_FreeMemory(CDF->fp->ADRList[i]->zAEDRList, NULL);
            };
            cdf_FreeMemory(CDF->fp->ADRList[i], NULL);
            CDF->fp->ADRList[i] = NULL;
        };
        if (CDF->fp->ADRList != NULL)
        {
            cdf_FreeMemory(CDF->fp->ADRList, NULL);
            CDF->fp->ADRList = NULL;
        };
        cdf_FreeMemory(CDF->fp->GDR, NULL);
        CDF->fp->GDR = NULL;
        CDF->fp->CurADRIndex = RESERVED_ENTRYNUM;
        CDF->fp->CurAEDRIndex = RESERVED_ENTRYNUM;
        CDF->CURattrOffset = RESERVED_ATTROFFSET;
        CDF->CURgrEntryOffset = RESERVED_ENTRYOFFSET;
        CDF->CURzEntryOffset = RESERVED_ENTRYOFFSET;
    };
}
