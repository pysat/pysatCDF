/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                     CDF `select' operations.
*
*  Version 1.4a, 21-Feb-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  20-May-92, J Love     Original version (was part of `cdflib.c').
*   V1.1  21-Aug-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V1.2   4-Jan-94, J Love     CDF V2.4.
*   V1.3  15-Dec-94, J Love     CDF V2.5.
*   V1.3a  4-Jan-95, J Love     Encode/decode changes.
*   V1.3b 19-Jan-95, J Love     IRIX 6.0 (64-bit).
*   V1.3c 24-Feb-95, J Love     Solaris 2.3 IDL i/f.
*   V1.4   5-Sep-96, J Love     CDF V2.6.
*   V1.4a 21-Feb-97, J Love	Removed RICE.
*   V2.0  08-Apr-04, M Liu      Modified selection for r/zVAR_ to see if
*                               its the same as the current. if not, get and
*                               save its offset from the file. This offset
*                               is used for any succeeding reference to this 
*                               variable for GET/PUT/DELETE/CLOSE operation.
*   V3.2  20-Jun-07, D Berger   Added clearing of READONLY mode metadata state
*                               info when READONLYon selected.
*   V3.2a 29-AUG-07, D Berger   Removed clearing of READONLY mode metadata state
*                               info when zMODE selected.
*
******************************************************************************/

#include "cdflib.h"

/******************************************************************************
* CDFsel.
******************************************************************************/

STATICforIDL CDFstatus CDFsel (Va, Cur)
struct VAstruct *Va;
struct CurStruct *Cur;
{
CDFstatus tStatus, pStatus = CDF_OK;
switch (Va->item) {
  /****************************************************************************
  * CDF_, select the current CDF.
  ****************************************************************************/
  case CDF_: {
    CDFid id = va_arg (Va->ap, CDFid);
    /**************************************************************************
    * Check if the reserved CDFid (used by IDL interface and Windows DLL).
    **************************************************************************/
    if (id == RESERVED_CDFID) {
      Cur->cdf = (struct CDFstruct *) RESERVED_CDFID;
      break;
    }
    /**************************************************************************
    * Check what the CDFid points to.
    **************************************************************************/
    switch (((struct CDFstruct *)id)->magic) {
      case VALIDid_MAGIC_NUMBER:
      case ABORTEDid_MAGIC_NUMBER:
	break;
      case KILLEDid_MAGIC_NUMBER:
	return BAD_CDF_ID;
      default:
	return BAD_CDF_ID;
    }
    /**************************************************************************
    * Set the current CDF.
    **************************************************************************/
    Cur->cdf = (struct CDFstruct *) id;
    break;
  }
  /****************************************************************************
  * CDF_STATUS_, select the current CDFstatus.
  ****************************************************************************/
  case CDF_STATUS_: {
    Cur->status = va_arg (Va->ap, CDFstatus);
    break;
  }
  /****************************************************************************
  * CDF_READONLY_MODE_, select a readonly mode for the current CDF.
  ****************************************************************************/
  case CDF_READONLY_MODE_: {
    struct CDFstruct *CDF;
    Logical Change = FALSE;
    long mode = va_arg (Va->ap, long);
    SelectCDF (Cur->cdf, CDF)
    switch (mode) {
      case READONLYon: 
        if (CDF->status == READ_ONLY) 
        {
            if (CDF->readOnly != TRUE) Change = TRUE;
            CDF->readOnly = TRUE;
        };
	break;
      case READONLYoff:
        if (CDF->readOnly != FALSE) Change = TRUE;
	CDF->readOnly = FALSE;
	break;
      default:
	return BAD_READONLY_MODE;
    }
    /*************************************************************************
    * Clear the READONLY metadata state info.
    *************************************************************************/
    if (Change)
    {
        ResetReadOnlyState(CDF);
        if (CDF->readOnly == TRUE && CDF->fp != NULL)
        {
            if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
                              GDR_NULL),&pStatus)) {
                AbortAccess (CDF, UPDATE, noDELETE);
                return pStatus;
            }
        };
    };
    break;
  }
  /****************************************************************************
  * CDF_zMODE_, select a zMode for the current CDF.  Changing the zMode causes
  *             all "current" objects/states to be reset (sort of like
  *             reopening the CDF).
  ****************************************************************************/
  case CDF_zMODE_: {
    long mode;
    struct CDFstruct *CDF;
    mode = va_arg (Va->ap, long);
    SelectCDF (Cur->cdf, CDF)
    switch (mode) {
      /************************************************************************
      * zMODEoff, turn off zMode.
      ************************************************************************/
      case zMODEoff:
	CDF->zMode = (int) zMODEoff;
	break;
      /************************************************************************
      * zMODEon1, turn on zMode/1.  The rVariables become a zGroup (ie. they
      * all have the same dimensionality [that of the rVariables] with their
      * original record/dimension variances).
      ************************************************************************/
      case zMODEon1:
	CDF->zMode = (int) zMODEon1;
	break;
      /************************************************************************
      * zMODEon2, turn on zMode/2.  The dimensionality of each rVariable is
      * determined based on their dimension variances (dimensions with a
      * NOVARY variance are eliminated).
      ************************************************************************/
      case zMODEon2:
	CDF->zMode = (int) zMODEon2;
	break;
      /************************************************************************
      * Unknown zMode.
      ************************************************************************/
      default:
	return BAD_zMODE;
    };
    if (!sX(ConfigureNEWzMode(CDF),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    };
    break;
  }
  /****************************************************************************
  * CDF_DECODING_, selects the decoding for attribute entry and variable data
  * values read from the current CDF.
  ****************************************************************************/
  case CDF_DECODING_: {
    struct CDFstruct *CDF;
    long decoding = va_arg (Va->ap, long);
    SelectCDF (Cur->cdf, CDF)
    if (!ValidDecoding((Int32)decoding)) return BAD_DECODING;
    CDF->decoding = decoding;
    if (!sX(UpdateConversions(CDF),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    break;
  }
  /****************************************************************************
  * CDF_NEGtoPOSfp0_MODE_, select a negative to positive floating point zero
  * mode for the current CDF.
  ****************************************************************************/
  case CDF_NEGtoPOSfp0_MODE_: {
    struct CDFstruct *CDF;
    long mode = va_arg (Va->ap, long);
    SelectCDF (Cur->cdf, CDF)
    switch (mode) {
      case NEGtoPOSfp0on:
	CDF->negToPosFp0 = TRUE;
	break;
      case NEGtoPOSfp0off:
	CDF->negToPosFp0 = FALSE;
	break;
      default:
	return BAD_NEGtoPOSfp0_MODE;
    }
    if (!sX(UpdateConversions(CDF),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    break;
  }
  /****************************************************************************
  * CDF_SCRATCHDIR_
  ****************************************************************************/
  case CDF_SCRATCHDIR_: {
    struct CDFstruct *CDF; size_t length;
    char *scratchDir = va_arg (Va->ap, char *);
    SelectCDF (Cur->cdf, CDF)
    if (CDF->scratchDir != NULL) {
      cdf_FreeMemory (CDF->scratchDir, NULL);
      CDF->scratchDir = NULL;
    }
    if (scratchDir == NULL) return BAD_SCRATCH_DIR;
    length = strlen (scratchDir);
    if (length > DU_MAX_DIR_LEN) return BAD_SCRATCH_DIR;
    CDF->scratchDir = (char *) cdf_AllocateMemory ((size_t) (length + 1), NULL);
    if (CDF->scratchDir == NULL) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return BAD_MALLOC;
    }
    strcpyX (CDF->scratchDir, scratchDir, 0);
    break;
  }
  /****************************************************************************
  * CDF_CACHESIZE_
  * Select a new cache size for the "working" dotCDF file.  A cache size of
  * zero causes a reset to the default.
  ****************************************************************************/
  case CDF_CACHESIZE_: {
    struct CDFstruct *CDF;
    int nBuffers = (int) va_arg (Va->ap, long);
    if (nBuffers < 0) return BAD_CACHE_SIZE;
    SelectCDF (Cur->cdf, CDF)
    CDF->workingCacheSize = BOO(nBuffers > 0,nBuffers,BOO(CDF->singleFile,
							  NUMcacheSINGLE,
							  NUMcacheMULTI));
    if (!CACHEv(CDF->fp,CDF->workingCacheSize)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return BAD_CACHE_SIZE;
    }
    break;
  }
  /****************************************************************************
  * STAGE_CACHESIZE_
  * Select a new cache size for the staging file.  A cache size of zero causes
  * a reset to the default.
  ****************************************************************************/
  case STAGE_CACHESIZE_: {
    struct CDFstruct *CDF;
    int nBuffers = (int) va_arg (Va->ap, long);
    if (nBuffers < 0) return BAD_CACHE_SIZE;
    SelectCDF (Cur->cdf, CDF)
    CDF->stage.cacheSize = BOO(nBuffers > 0,nBuffers,NUMcacheSTAGE);
    if (CDF->stage.fp != NULL) {
      if (!CACHEv(CDF->stage.fp,CDF->stage.cacheSize)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return BAD_CACHE_SIZE;
      }
    }
    break;
  }
  /****************************************************************************
  * COMPRESS_CACHESIZE_
  * Select a new cache size for the compression scratch file.  A cache size of
  * zero causes a reset to the default.  Note that the vFILE is first cleared
  * so that cache buffers are not written to disk.  This assumes that the
  * compression scratch file is always cleared before being used.
  ****************************************************************************/
  case COMPRESS_CACHESIZE_: {
    struct CDFstruct *CDF;
    int nBuffers = (int) va_arg (Va->ap, long);
    if (nBuffers < 0) return BAD_CACHE_SIZE;
    SelectCDF (Cur->cdf, CDF)
    CDF->compressCacheSize = BOO(nBuffers > 0,nBuffers,NUMcacheCOMPRESS);
    if (CDF->compressFp != NULL) {
      if (!CLEARv(CDF->compressFp)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return BAD_CACHE_SIZE;
      }
      if (!CACHEv(CDF->compressFp,CDF->compressCacheSize)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return BAD_CACHE_SIZE;
      }
    }
    break;
  }
  /****************************************************************************
  * rVARs_CACHESIZE_/zVARs_CACHESIZE_
  * Selects a new cache size for all of the r/zVariable files.  N/A if a
  * single-file CDF.  A cache size of zero causes a reset to the default.
  ****************************************************************************/
  case rVARs_CACHESIZE_:
  case zVARs_CACHESIZE_: {
    Logical zOp = (Va->item == zVARs_CACHESIZE_);
    struct CDFstruct *CDF; struct VarStruct *Var;
    int nBuffers = (int) va_arg (Va->ap, long), varN;
    if (nBuffers < 0) return BAD_CACHE_SIZE;
    if (nBuffers < 1) nBuffers = NUMcacheVAR;
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (CDF->singleFile) {
      sX (SINGLE_FILE_FORMAT, &pStatus);
      break;
    }
    if (zModeON(CDF)) {
      for (varN = 0; varN < CDF->NrVars; varN++) {
	 if (!sX(InitVar(CDF,varN,FALSE,&Var),&pStatus)) {
	   AbortAccess (CDF, UPDATE, noDELETE);
	   return pStatus;
	 }
	 Var->varCacheSize = nBuffers;
	 if (Var->fp != NULL) {
	   if (!CACHEv(Var->fp,Var->varCacheSize)) {
	     AbortAccess (CDF, UPDATE, noDELETE);
	     return BAD_CACHE_SIZE;
	   }
	 }
      }
      for (varN = 0; varN < CDF->NzVars; varN++) {
	 if (!sX(InitVar(CDF,varN,TRUE,&Var),&pStatus)) {
	   AbortAccess (CDF, UPDATE, noDELETE);
	   return pStatus;
	 }
	 Var->varCacheSize = nBuffers;
	 if (Var->fp != NULL) {
	   if (!CACHEv(Var->fp,Var->varCacheSize)) {
	     AbortAccess (CDF, UPDATE, noDELETE);
	     return BAD_CACHE_SIZE;
	   }
	 }
      }
    }
    else {
      long nVars = BOO(zOp,CDF->NzVars,CDF->NrVars);
      for (varN = 0; varN < nVars; varN++) {
	 if (!sX(InitVar(CDF,varN,zOp,&Var),&pStatus)) {
	   AbortAccess (CDF, UPDATE, noDELETE);
	   return pStatus;
	 }
	 Var->varCacheSize = nBuffers;
	 if (Var->fp != NULL) {
	   if (!CACHEv(Var->fp,Var->varCacheSize)) {
	     AbortAccess (CDF, UPDATE, noDELETE);
	     return BAD_CACHE_SIZE;
	   }
	 }
      }
    }
    break;
  }
  /****************************************************************************
  * rVAR_CACHESIZE_/zVAR_CACHESIZE_, selects a new cache size for the current
  * r/zVariable's file.  N/a if a single-file CDF.  A cache size of zero causes
  * a reset to the default.
  ****************************************************************************/
  case rVAR_CACHESIZE_:
  case zVAR_CACHESIZE_: {
    Logical zOp = (Va->item == zVAR_CACHESIZE_);
    struct CDFstruct *CDF; struct VarStruct *Var;
    int nBuffers = (int) va_arg (Va->ap, long);
    if (nBuffers < 0) return BAD_CACHE_SIZE;
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (CDF->singleFile) {
      sX (SINGLE_FILE_FORMAT, &pStatus);
      break;
    }
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    if (!sX(InitCurrentVar(CDF,zOp,&Var),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    Var->varCacheSize = BOO(nBuffers < 1,NUMcacheVAR,nBuffers);
    if (Var->fp != NULL) {
      if (!CACHEv(Var->fp,Var->varCacheSize)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return BAD_CACHE_SIZE;
      }
    }
    break;
  }
  /****************************************************************************
  * rVAR_/zVAR_
  *    Select (by number) the current rVariable/zVariable for the current CDF.
  ****************************************************************************/
  case rVAR_:
  case zVAR_: {
    Logical zOp = (Va->item == zVAR_);
    struct CDFstruct *CDF;
    Int32 offset;
    Int32 varNum = (Int32) va_arg (Va->ap, long);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (varNum < 0) return BAD_VAR_NUM;
    if (zModeON(CDF))
      if (varNum < CDF->NrVars) {
	if (CDF->CURzVarNum == varNum) {
	  offset = CDF->CURzVarOffset;
	  return CDF_OK;
	} else {
          if (!sX(FindVarByNumber(CDF,varNum,FALSE,&offset),&pStatus)) {
	    AbortAccess (CDF, UPDATE, noDELETE);
	    return pStatus;
	  }
	  CDF->CURzVarNum = varNum;
	  CDF->CURzVarOffset = offset;
	  return pStatus;
	}
      }	    
      else {
	long varNumT = varNum - CDF->NrVars;
	if (varNumT < CDF->NzVars) {
	  if (CDF->CURzVarNum == varNum) {
	    offset = CDF->CURzVarOffset;
	    return CDF_OK;
          } else {
            if (!sX(FindVarByNumber(CDF,varNumT,TRUE,&offset),&pStatus)) {
	      AbortAccess (CDF, UPDATE, noDELETE);
	      return pStatus;
	    }
	    CDF->CURzVarNum = varNum;
	    CDF->CURzVarOffset = offset;
	    return pStatus;
	  }
	}
	else
	  return NO_SUCH_VAR;
      }
    else
      if (varNum < BOO(zOp,CDF->NzVars,CDF->NrVars)) {
	if (zOp) {
	  if (CDF->CURzVarNum == varNum) {
	    offset = CDF->CURzVarOffset;
	    return CDF_OK;
	  } else {
            if (!sX(FindVarByNumber(CDF,varNum,TRUE,&offset),&pStatus)) {
	      AbortAccess (CDF, UPDATE, noDELETE);
	      return pStatus;
	    }
	    CDF->CURzVarNum = varNum;
	    CDF->CURzVarOffset = offset;
	    return pStatus;
	  }
	}
	else {
	  if (CDF->CURrVarNum == varNum) {
	    offset = CDF->CURrVarOffset;
	    return CDF_OK;
	  } else {
            if (!sX(FindVarByNumber(CDF,varNum,FALSE,&offset),&pStatus)) {
	      AbortAccess (CDF, UPDATE, noDELETE);
	      return pStatus;
	    }
	    CDF->CURrVarNum = varNum; 
	    CDF->CURrVarOffset = offset;
	    return pStatus;
	  }
	}
      } else
	return NO_SUCH_VAR;
    break;
  }
  /****************************************************************************
  * rVAR_NAME_/zVAR_NAME_
  *    Select (by name) the current rVariable/zVariable for the current CDF.
  ****************************************************************************/
  case rVAR_NAME_:
  case zVAR_NAME_: {
    Logical zOp = (Va->item == zVAR_NAME_), zVar;
    struct CDFstruct *CDF;
    char *varName = va_arg (Va->ap, char *);
    Int32 varN, offset;
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    tStatus = FindVarByName (CDF, varName, &offset, &zVar, NULL);
    switch (tStatus) {
      case CDF_OK:
	break;
      case NO_SUCH_VAR:
	return tStatus;
      default:
	AbortAccess (CDF, UPDATE, noDELETE);
	return tStatus;
    }
    if (!sX(ReadVDR(CDF,CDF->fp,offset,zVar,
		    VDR_NUM,&varN,
		    VDR_NULL),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (zModeON(CDF)) {
      CDF->CURzVarNum = (zVar ? CDF->NrVars + varN : varN);
      CDF->CURzVarOffset = offset;
    } else {
      if (zOp)
	if (zVar) {
	  CDF->CURzVarNum = varN;
	  CDF->CURzVarOffset = offset;
	} else
	  return NO_SUCH_VAR;                   /* Wrong type of variable. */
      else
	if (zVar)
	  return NO_SUCH_VAR;                   /* Wrong type of variable. */
	else {
	  CDF->CURrVarNum = varN;
	  CDF->CURrVarOffset = offset;
	}
    }
    break;
  }
  /****************************************************************************
  * rVARs_RECNUMBER_/rVARs_RECCOUNT_/rVARs_RECINTERVAL_
  ****************************************************************************/
  case rVARs_RECNUMBER_:
  case rVARs_RECCOUNT_:
  case rVARs_RECINTERVAL_: {
    struct CDFstruct *CDF;
    Int32 value = (Int32) va_arg (Va->ap, long);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,TRUE)) return ILLEGAL_IN_zMODE;
    if (CDF->NrVars == 0) {
      if (!sX(NO_VARS_IN_CDF,&pStatus)) return pStatus;
    }
    switch (Va->item) {
      case rVARs_RECNUMBER_:
	if (value < 0) return BAD_REC_NUM;
	CDF->rRD.recNumber = value;
	break;
      case rVARs_RECCOUNT_:
	if (value < 0) return BAD_REC_COUNT;
	CDF->rRD.recCount = value;
	break;
      case rVARs_RECINTERVAL_:
	if (value < 1) return BAD_REC_INTERVAL;
	CDF->rRD.recInterval = value;
	break;
    }
    break;
  }
  /****************************************************************************
  * zVAR_RECNUMBER_/zVAR_RECCOUNT_/zVAR_RECINTERVAL_
  ****************************************************************************/
  case zVAR_RECNUMBER_:
  case zVAR_RECCOUNT_:
  case zVAR_RECINTERVAL_: {
    struct CDFstruct *CDF;
    struct VarStruct *Var;
    Int32 value = (Int32) va_arg (Va->ap, long);
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTvarSELECTED(CDF,TRUE)) return NO_VAR_SELECTED;
    if (!sX(InitCurrentVar(CDF,TRUE,&Var),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    switch (Va->item) {
      case zVAR_RECNUMBER_:
	if (value < 0) return BAD_REC_NUM;
	Var->zRD.recNumber = value;
	break;
      case zVAR_RECCOUNT_:
	if (value < 1) return BAD_REC_COUNT;
	Var->zRD.recCount = value;
	break;
      case zVAR_RECINTERVAL_:
	if (value < 1) return BAD_REC_INTERVAL;
	Var->zRD.recInterval = value;
	break;
    }
    break;
  }
  /****************************************************************************
  * zVARs_RECNUMBER_
  ****************************************************************************/
  case zVARs_RECNUMBER_: {
    struct CDFstruct *CDF;
    struct VarStruct *Var;
    Int32 recNumber = (Int32) va_arg (Va->ap, long); Int32 varN;
    SelectCDF (Cur->cdf, CDF)
    if (zModeON(CDF)) {
      if (CDF->NrVars + CDF->NzVars == 0) {
	if (!sX(NO_VARS_IN_CDF,&pStatus)) return pStatus;
      }
      for (varN = 0; varN < CDF->NrVars; varN++) {
	 if (!sX(InitVar(CDF,varN,FALSE,&Var),&pStatus)) {
	   AbortAccess (CDF, UPDATE, noDELETE);
	   return pStatus;
	 }
	 Var->zRD.recNumber = recNumber;
      }
      for (varN = 0; varN < CDF->NzVars; varN++) {
	 if (!sX(InitVar(CDF,varN,TRUE,&Var),&pStatus)) {
	   AbortAccess (CDF, UPDATE, noDELETE);
	   return pStatus;
	 }
	 Var->zRD.recNumber = recNumber;
      }
    }
    else {
      if (CDF->NzVars == 0) {
	if (!sX(NO_VARS_IN_CDF,&pStatus)) return pStatus;
      }
      for (varN = 0; varN < CDF->NzVars; varN++) {
	 if (!sX(InitVar(CDF,varN,TRUE,&Var),&pStatus)) {
	   AbortAccess (CDF, UPDATE, noDELETE);
	   return pStatus;
	 }
	 Var->zRD.recNumber = recNumber;
      }
    }
    break;
  }
  /****************************************************************************
  * rVARs_DIMINDICES_/rVARs_DIMCOUNTS_/rVARs_DIMINTERVALS_
  ****************************************************************************/
  case rVARs_DIMINDICES_:
  case rVARs_DIMCOUNTS_:
  case rVARs_DIMINTERVALS_: {
    struct CDFstruct *CDF;
    long *values = va_arg (Va->ap, long *);
    int dimN;
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,TRUE)) return ILLEGAL_IN_zMODE;
    if (CDF->NrVars == 0) {
      if (!sX(NO_VARS_IN_CDF,&pStatus)) return pStatus;
    }
    switch (Va->item) {
      case rVARs_DIMINDICES_: {
	for (dimN = 0; dimN < CDF->rNumDims; dimN++)
	   if (values[dimN] < 0 || CDF->rDimSizes[dimN] <= values[dimN])
	     return BAD_DIM_INDEX;
	   else
	     CDF->rRD.dimIndices[dimN] = (Int32) values[dimN];
	break;
      }
      case rVARs_DIMCOUNTS_:
	for (dimN = 0; dimN < CDF->rNumDims; dimN++)
	   if (values[dimN] < 1)
	     return BAD_DIM_COUNT;
	   else
	     CDF->rRD.dimCounts[dimN] = (Int32) values[dimN];
	break;
      case rVARs_DIMINTERVALS_:
	for (dimN = 0; dimN < CDF->rNumDims; dimN++)
	   if (values[dimN] < 1)
	     return BAD_DIM_INTERVAL;
	   else
	     CDF->rRD.dimIntervals[dimN] = (Int32) values[dimN];
	break;
    }
    break;
  }
  /****************************************************************************
  * zVAR_DIMINDICES_/zVAR_DIMCOUNTS_/zVAR_DIMINTERVALS_
  ****************************************************************************/
  case zVAR_DIMINDICES_:
  case zVAR_DIMCOUNTS_:
  case zVAR_DIMINTERVALS_: {
    struct CDFstruct *CDF;
    struct VarStruct *Var;
    long *values = va_arg (Va->ap, long *);
    int dimN;
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTvarSELECTED(CDF,TRUE)) return NO_VAR_SELECTED;
    if (!sX(InitCurrentVar(CDF,TRUE,&Var),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    switch (Va->item) {
      case zVAR_DIMINDICES_:
	for (dimN = 0; dimN < Var->numDims; dimN++)
	   if (values[dimN] < 0 || Var->dimSizes[dimN] <= values[dimN])
	     return BAD_DIM_INDEX;
	   else
	     Var->zRD.dimIndices[dimN] = (Int32) values[dimN];
	break;
      case zVAR_DIMCOUNTS_:
	for (dimN = 0; dimN < Var->numDims; dimN++)
	   if (values[dimN] < 1)
	     return BAD_DIM_COUNT;
	   else
	     Var->zRD.dimCounts[dimN] = (Int32) values[dimN];
	break;
      case zVAR_DIMINTERVALS_:
	for (dimN = 0; dimN < Var->numDims; dimN++)
	   if (values[dimN] < 1)
	     return BAD_DIM_INTERVAL;
	   else
	     Var->zRD.dimIntervals[dimN] = (Int32) values[dimN];
	break;
    }
    break;
  }
  /****************************************************************************
  * rVAR_SEQPOS_/zVAR_SEQPOS_, 
  ****************************************************************************/
  case rVAR_SEQPOS_:
  case zVAR_SEQPOS_: {
    Logical zOp = (Va->item == zVAR_SEQPOS_);
    struct CDFstruct *CDF; struct VarStruct *Var;
    int dimN; Int32 indices[CDF_MAX_DIMS];
    Int32 recNumber = (Int32) va_arg (Va->ap, long);
    long *dimIndices = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (recNumber < 0) return BAD_REC_NUM;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    if (!sX(InitCurrentVar(CDF,zOp,&Var),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    for (dimN = 0; dimN < Var->numDims; dimN++) {
       if (dimIndices[dimN] < 0 || dimIndices[dimN] >= Var->dimSizes[dimN])
	 return BAD_DIM_INDEX;
       else
	 indices[dimN] = (Int32) dimIndices[dimN];
    }
    Var->seqValueOffset = BOO(Var->recVary,recNumber*Var->NphyRecValues,0);
    Var->seqValueOffset += IndicesValueOffset (Var->numDims, indices,
					       Var->dimVarys,
					       Var->nPhyDimValues);
    break;
  }
  /****************************************************************************
  * rVAR_RESERVEPERCENT_/zVAR_RESERVEPERCENT_
  ****************************************************************************/
  case rVAR_RESERVEPERCENT_:
  case zVAR_RESERVEPERCENT_: {
    Logical zOp = (Va->item == zVAR_RESERVEPERCENT_);
    struct CDFstruct *CDF; struct VarStruct *Var;
    int pct = (int) va_arg (Va->ap, long);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    if (pct < 0) return BAD_COMPRESSION_PARM;
    if (!sX(InitCurrentVar(CDF,zOp,&Var),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    switch (Var->vType) {
      case COMPRESSED_:
      case SPARSE_COMPRESSED_RECORDS_:
	Var->reservePct = pct;
	break;
      case SPARSE_ARRAYS_:
      case SPARSE_RECORDS_AND_ARRAYS_:
	return CDF_INTERNAL_ERROR;
      case STANDARD_:
      case SPARSE_RECORDS_:
	sX (NA_FOR_VARIABLE, &pStatus);
	break;
      case IN_MULTI_:
	sX (MULTI_FILE_FORMAT, &pStatus);
	break;
    }
    break;
  }
  /****************************************************************************
  * ATTR_/ATTR_NAME_
  *   Select the current attribute by number/name.
  ****************************************************************************/
  case ATTR_:
  case ATTR_NAME_: {
    Logical nameOp = (Va->item == ATTR_NAME_);
    long attrNum; char *attrName; struct CDFstruct *CDF; Int32 offset;
    if (nameOp)
      attrName = va_arg (Va->ap, char *);
    else {
      attrNum = va_arg (Va->ap, long);
      if (attrNum < 0) return BAD_ATTR_NUM;
    }
    /**************************************************************************
    * Determine current attribute offset.
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    tStatus = BOO(nameOp,FindAttrByName(CDF,attrName,&offset),
			 FindAttrByNumber(CDF,(Int32)attrNum,&offset));
    switch (tStatus) {
      case CDF_OK:
	CDF->CURattrOffset = offset;
	break;
      case NO_SUCH_ATTR:
	return tStatus;
      default:
	AbortAccess (CDF, UPDATE, noDELETE);
	return tStatus;
    }
    /**************************************************************************
    * Reset current entry offsets.
    **************************************************************************/
    if (!sX(SetCURgrEntry(CDF,FALSE,CDF->CURgrEntryNum),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (!sX(SetCURzEntry(CDF,FALSE,CDF->CURzEntryNum),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    break;
  }
  /****************************************************************************
  * gENTRY_/rENTRY_/zENTRY_, select the current g/r/zEntry by number.
  ****************************************************************************/
  case gENTRY_:
  case rENTRY_:
  case zENTRY_: {
    struct CDFstruct *CDF; Logical zOp = (Va->item == zENTRY_);
    long entryNum = va_arg (Va->ap, long);
    /**************************************************************************
    * Setup/validate operation.
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,Va->item==rENTRY_)) return ILLEGAL_IN_zMODE;
    if (entryNum < 0) return BAD_ENTRY_NUM;
    /**************************************************************************
    * Set current entry number and offset.
    **************************************************************************/
    if (!sX(BOO(zOp,SetCURzEntry(CDF,TRUE,(Int32)entryNum),
		    SetCURgrEntry(CDF,TRUE,(Int32)entryNum)),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    break;
  }
  /****************************************************************************
  * rENTRY_NAME_/zENTRY_NAME_, select the current r/zEntry number by variable
  * name.
  ****************************************************************************/
  case rENTRY_NAME_:
  case zENTRY_NAME_: {
    void *varName = va_arg (Va->ap, void *);
    Logical zOp = (Va->item == zENTRY_NAME_), zVar;
    struct CDFstruct *CDF;
    Int32 offset, varN;
    long entryNum;
    /**************************************************************************
    * Setup/validate operation.
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,Va->item == rENTRY_NAME_)) return ILLEGAL_IN_zMODE;
    /**************************************************************************
    * Locate the VDR.
    **************************************************************************/
    tStatus = FindVarByName (CDF, varName, &offset, &zVar, NULL);
    switch (tStatus) {
      case CDF_OK:
	break;
      case NO_SUCH_VAR:
	return tStatus;
      default:
	AbortAccess (CDF, UPDATE, noDELETE);
	return tStatus;
    }
    /**************************************************************************
    * Read the variable number.
    **************************************************************************/
    if (!sX(ReadVDR(CDF,CDF->fp,offset,zVar,
		    VDR_NUM,&varN,
		    VDR_NULL),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Determine the new entry number.
    **************************************************************************/
    if (zModeON(CDF))
      if (zVar)
	entryNum = CDF->NrVars + varN;
      else
	entryNum = varN;
    else
      if (zVar)
	if (zOp)
	  entryNum = varN;
	else
	  return NO_SUCH_VAR;                   /* Wrong type of variable. */
      else
	if (zOp)
	  return NO_SUCH_VAR;                   /* Wrong type of variable. */
	else
	  entryNum = varN;
    /**************************************************************************
    * Set the current entry number and offset.
    **************************************************************************/
    if (!sX(BOO(zOp,SetCURzEntry(CDF,FALSE,(Int32)entryNum),
		    SetCURgrEntry(CDF,FALSE,(Int32)entryNum)),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
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
