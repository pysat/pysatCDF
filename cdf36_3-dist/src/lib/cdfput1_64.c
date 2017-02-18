/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                    CDF `put' operations, part 1.
*
*  Version 1.5, 9-Sep-96, Hughes STX.
*
*  Modification history:
*
*   V1.0  20-May-92, J Love     Original version (was part of `cdflib.c').
*   V1.1  16-Sep-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V1.1  16-Sep-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V1.2  10-Dec-93, J Love     CDF V2.4.  Added readonly mode and zMode.
*   V1.3  15-Dec-94, J Love     CDF V2.5.
*   V1.3a  6-Jan-95, J Love     Encode/decode changes.  More cache-residency.
*   V1.3b 15-Mar-95, J Love     Solaris 2.3 IDL i/f.  Fixed `recNum' argument
*                               to `LastAllocatedRecord'.
*   V1.4  30-May-95, J Love     Fixed bug in <PUT_,r/zVAR_PADVALUE_> involving
*                               use of old VDR offset after the VDR was moved.
*   V1.4a  4-Aug-95, J Love     CDFexport-related changes.
*   V1.5   9-Sep-96, J Love     CDF V2.6.
*   V2.0  08-Apr-04, M Liu      Removed calls to LocateCurrentVar function as
*                               its offset becomes available when it is
*                               selected/created. 
*   V2.1  29-Jun-04, M Liu      Added LFS (Large File Support > 2G).
*   V2.2  16-Nov-05, M Liu      Changed code for data spec modification. -99
*                               is passed as a special indicator for the 
*                               number of elements as it can't be change. 
*
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"

/******************************************************************************
* CDFput1_64.
******************************************************************************/

STATICforIDL CDFstatus CDFput1_64 (Va, Cur)
struct VAstruct *Va;
struct CurStruct *Cur;
{
CDFstatus tStatus, pStatus = CDF_OK;
switch (Va->item) {
  /****************************************************************************
  * rVAR_NAME_/zVAR_NAME_, rename the current variable.  A variable with the
  * same name must not already exist in the CDF.
  ****************************************************************************/
  case rVAR_NAME_:
  case zVAR_NAME_: {
    Logical zOp = (Va->item == zVAR_NAME_), zVarCur;
    struct CDFstruct *CDF;
    char *varName = va_arg (Va->ap, char *), tmpName[CDF_VAR_NAME_LEN256+1];
    OFF_T offsetCur, offsetFound;
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;

    zVarCur = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVarCur) offsetCur = CDF->CURzVarOffset64;
    else offsetCur = CDF->CURrVarOffset64;

    if (strlen(varName) > (size_t) CDF_VAR_NAME_LEN256) {
      if (!sX(VAR_NAME_TRUNC,&pStatus)) return pStatus;
    }
    strcpyX (tmpName, varName, CDF_VAR_NAME_LEN256);
    if (!ValidVarName(tmpName)) return BAD_VAR_NAME;
    /**************************************************************************
    * Check that the new variable name is not already in use.  Don't flag as
    * an error if the new name is the same as the old name (ignoring trailing
    * blanks).  Trailing blanks may be eliminated.
    **************************************************************************/
    tStatus = FindVarByName64 (CDF, tmpName, &offsetFound, NULL, NULL);
    switch (tStatus) {
      case CDF_OK:
	if (offsetFound != offsetCur) return VAR_EXISTS;
	break;
      case NO_SUCH_VAR:
	break;
      default:
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return tStatus;
    }
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    NulPad (tmpName, CDF_VAR_NAME_LEN256);
    if (!sX(WriteVDR64(CDF,CDF->fp,offsetCur,zVarCur,
		       VDR_NAME,tmpName,
		       VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    break;
  }

  /****************************************************************************
  * rVAR_DATASPEC_/zVAR_DATASPEC_, 
  *   If the data types are not equivalent or the number of elements are
  *   different, then check for the following:
  *     1) if any records have been written
  *     2) if a pad value has been specified
  *   If either is true, then the data specification cannot be changed.
  ****************************************************************************/
  case rVAR_DATASPEC_:
  case zVAR_DATASPEC_: {
    Logical zOp = (Va->item == zVAR_DATASPEC_), zVar;
    struct CDFstruct *CDF; struct VarStruct *Var;
    Int32 newDataType = (Int32) va_arg (Va->ap, long);
    Int32 newNumElems = (Int32) va_arg (Va->ap, long);
    Int32 flags, dataType, numElems, maxRec, maxAllocated; OFF_T offset;
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) offset = CDF->CURzVarOffset64;
    else offset = CDF->CURrVarOffset64;

    if (zModeON(CDF))
      if (CDF->CURzVarNum < CDF->NrVars)
        Var = CDF->rVars[(int)CDF->CURzVarNum];
      else
        Var = CDF->zVars[(int)(CDF->CURzVarNum - CDF->NrVars)];
    else
      Var = BOO(zOp,CDF->zVars[(int)CDF->CURzVarNum],
                    CDF->rVars[(int)CDF->CURrVarNum]);

    if (!ValidDataType(newDataType)) return BAD_DATA_TYPE;
    if (newNumElems != -99 && newNumElems < 1) return BAD_NUM_ELEMS;
    if (!STRINGdataType(newDataType)) {
      if (newNumElems != -99 && newNumElems != 1) return BAD_NUM_ELEMS;
    }
    if (!sX(ReadVDR64(CDF,CDF->fp,offset,zVar,
		      VDR_FLAGS,&flags,
		      VDR_DATATYPE,&dataType,
		      VDR_NUMELEMS,&numElems,
		      VDR_MAXREC,&maxRec,
		      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * If the data specifications are not equivalent, check if any records have
    * been written or allocated or if a pad value has been specified.
    **************************************************************************/
    if (!EquivDataTypes(newDataType,dataType) || 
        (newNumElems != -99 && newNumElems != numElems)) {
      if (maxRec > NO_RECORD) return CANNOT_CHANGE;
      if (!sX(LastRecord64(CDF,offset,zVar,&maxAllocated),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (maxAllocated > NO_RECORD) return CANNOT_CHANGE;
      if (PADvalueBITset(flags)) return CANNOT_CHANGE;
    }
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    if (newNumElems == -99) newNumElems = numElems;
    /**************************************************************************
    * Update the VDR with the new data specification.
    **************************************************************************/
    if (!sX(WriteVDR64(CDF,CDF->fp,offset,zVar,
		       VDR_DATATYPE,&newDataType,
		       VDR_NUMELEMS,&newNumElems,
		       VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * If this variable has been initialized, recalculate the affected
    * parameters.
    **************************************************************************/
    if (Var != NULL) {
      Var->NvalueElems = newNumElems;
      Var->NelemBytes = (Int32) CDFelemSize ((long)newDataType);
      Var->NvalueBytes = Var->NvalueElems * Var->NelemBytes;
      Var->NphyRecElems = Var->NphyRecValues * Var->NvalueElems;
      Var->NvirtRecElems = Var->NvirtRecValues * Var->NvalueElems;
      Var->NphyRecBytes = Var->NphyRecValues * Var->NvalueBytes;
      Var->NvirtRecBytes = Var->NvirtRecValues * Var->NvalueBytes;
      if (!sX(ConversionFunction(newDataType,HostEncoding(),
				 CDF->encoding,CDF->negToPosFp0,
				 &(Var->EncodeFunction)),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (!sX(ConversionFunction(newDataType,CDF->encoding,
				 CDF->decoding,CDF->negToPosFp0,
				 &(Var->DecodeFunction)),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    break;
  }

  /****************************************************************************
  * rVAR_RECVARY_/zVAR_RECVARY_, 
  *   Can't change if any records have been written or allocated.
  ****************************************************************************/
  case rVAR_RECVARY_:
  case zVAR_RECVARY_: {
    Logical zOp = (Va->item == zVAR_RECVARY_), zVar;
    Int32 maxAllocated; OFF_T offset;
    struct CDFstruct *CDF; struct VarStruct *Var; struct VDRstruct64 VDR;
    long recVariance = va_arg (Va->ap, long);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) offset = CDF->CURzVarOffset64;
    else offset = CDF->CURrVarOffset64;

    if (zModeON(CDF))
      if (CDF->CURzVarNum < CDF->NrVars)
        Var = CDF->rVars[(int)CDF->CURzVarNum];
      else
        Var = CDF->zVars[(int)(CDF->CURzVarNum - CDF->NrVars)];
    else
      Var = BOO(zOp,CDF->zVars[(int)CDF->CURzVarNum],
                    CDF->rVars[(int)CDF->CURrVarNum]);

    if (!sX(ReadVDR64(CDF,CDF->fp,offset,zVar,
		      VDR_RECORD,&VDR,NULL,
		      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (VDR.MaxRec > NO_RECORD) return CANNOT_CHANGE;
    if (!sX(LastRecord64(CDF,offset,zVar,&maxAllocated),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (maxAllocated > NO_RECORD) return CANNOT_CHANGE;
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    if (recVariance)
      SetBit32 (&VDR.Flags,VDR_RECVARY_BIT);
    else {
      ClearBit32 (&VDR.Flags,VDR_RECVARY_BIT);
      if (VDR.blockingFactor > 1) VDR.blockingFactor = 1;
    }
    if (!sX(WriteVDR64(CDF,CDF->fp,offset,zVar,
		       VDR_RECORD,&VDR,NULL,
		       VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * If this variable has been initialized, recalculate the affected
    * parameters.
    **************************************************************************/
    if (Var != NULL) {
      Var->recVary = BOO(recVariance,VARY,NOVARY);
      if (!sX(CalcBF64(CDF,Var),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    break;
  }

  /****************************************************************************
  * rVAR_DIMVARYS_/zVAR_DIMVARYS_
  *     Respecify dimension variances for current rVariable/zVariable.  Can't
  * change if any records have been written or if zMode/2 and really an
  * rVariable.
  ****************************************************************************/
  case rVAR_DIMVARYS_:
  case zVAR_DIMVARYS_: {
    Logical zOp = (Va->item == zVAR_DIMVARYS_), zVar;
    long *dimVarys = va_arg (Va->ap, long *);
    struct CDFstruct *CDF; struct VarStruct *Var; struct VDRstruct64 VDR;
    Int32 numDims, maxAllocated; int dimN; OFF_T offset;
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) offset = CDF->CURzVarOffset64;
    else offset = CDF->CURrVarOffset64;

    if (zModeON(CDF))
      if (CDF->CURzVarNum < CDF->NrVars)
        Var = CDF->rVars[(int)CDF->CURzVarNum];
      else
        Var = CDF->zVars[(int)(CDF->CURzVarNum - CDF->NrVars)];
    else
      Var = BOO(zOp,CDF->zVars[(int)CDF->CURzVarNum],
                    CDF->rVars[(int)CDF->CURrVarNum]);

    if (CDF->zMode == zMODEon2 && !zVar) return CANNOT_CHANGE;
    if (!sX(ReadVDR64(CDF,CDF->fp,offset,zVar,
		      VDR_RECORD,&VDR,NULL,
		      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (VDR.MaxRec > NO_RECORD) return CANNOT_CHANGE;
    if (!sX(LastRecord64(CDF,offset,zVar,&maxAllocated),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (maxAllocated > NO_RECORD) return CANNOT_CHANGE;
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    numDims = BOO(zVar,VDR.zNumDims,CDF->rNumDims);
    for (dimN = 0; dimN < numDims; dimN++) {
       VDR.DimVarys[dimN] = BOO(dimVarys[dimN],VARY,NOVARY);
    }
    if (!sX(WriteVDR64(CDF,CDF->fp,offset,zVar,
		       VDR_RECORD,&VDR,NULL,
		       VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * If this variable has been initialized, recalculate its parameters.
    *************************************************************************/
    if (Var != NULL) {
      if (!sX(CalcDimParms64(CDF,Var->VDRoffset64,
			     Var->zVar,&(Var->numDims),
			     Var->dimSizes,Var->dimVarys),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      CalcNumDimValues (CDF, Var);
      CalcRecValues (Var);
      Var->NphyRecElems = Var->NphyRecValues * Var->NvalueElems;
      Var->NphyRecBytes = Var->NphyRecValues * Var->NvalueBytes;
      if (!sX(CalcBF64(CDF,Var),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    break;
  }

  /****************************************************************************
  * rVAR_ALLOCATERECS_/zVAR_ALLOCATERECS_
  * rVAR_ALLOCATEBLOCK_/zVAR_ALLOCATEBLOCK_
  ****************************************************************************/
  case rVAR_ALLOCATERECS_:
  case zVAR_ALLOCATERECS_:
  case rVAR_ALLOCATEBLOCK_:
  case zVAR_ALLOCATEBLOCK_: {
    Logical zOp = (Va->item == zVAR_ALLOCATERECS_ ||
		   Va->item == zVAR_ALLOCATEBLOCK_);
    Logical blockOp = (Va->item == rVAR_ALLOCATEBLOCK_ ||
		       Va->item == zVAR_ALLOCATEBLOCK_);
    struct CDFstruct *CDF; struct VarStruct *Var; struct AllocStruct alloc;
    long firstRecN, lastRecN;
    if (blockOp) {
      firstRecN = va_arg (Va->ap, long);
      lastRecN = va_arg (Va->ap, long);
    }
    else {
      firstRecN = 0;
      lastRecN = va_arg(Va->ap,long) - 1;
    }
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    if (firstRecN < 0) return BAD_ALLOCATE_RECS;
    if (lastRecN < firstRecN) return BAD_ALLOCATE_RECS;
    if (!sX(InitCurrentVar64(CDF,zOp,&Var),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (!Var->recVary && (firstRecN != 0 || lastRecN != 0)) {
      return BAD_ALLOCATE_RECS;
    }
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    /**************************************************************************
    * Depending on variable type...
    **************************************************************************/
    switch (Var->vType) {
      case STANDARD_:
	if (firstRecN > Var->maxAllocated + 1) {
	  firstRecN = Var->maxAllocated + 1;
	  sX (PRECEEDING_RECORDS_ALLOCATED, &pStatus);
	}
	/* No `break' is deliberate. */
      case SPARSE_RECORDS_:
	LoadAllocVVR64 (alloc, firstRecN, lastRecN, FALSE)
	if (!sX(AllocateRecords64(CDF,Var,alloc),&pStatus)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
	Var->maxAllocated = MAXIMUM(lastRecN,Var->maxAllocated);
	break;
      case COMPRESSED_:
      case SPARSE_COMPRESSED_RECORDS_:
	return CANNOT_ALLOCATE_RECORDS;
      case SPARSE_ARRAYS_:
      case SPARSE_RECORDS_AND_ARRAYS_:
	return CANNOT_ALLOCATE_RECORDS;
      case IN_MULTI_:
	sX (MULTI_FILE_FORMAT, &pStatus);
	break;
      default:
	return CDF_INTERNAL_ERROR;
    }
    break;
  }

  /****************************************************************************
  * rVAR_INITIALRECS_/zVAR_INITIALRECS_,
  *    Specify (write) an initial number of records for the current variable.
  * Can't set if any records have been written already.
  ****************************************************************************/
  case rVAR_INITIALRECS_:
  case zVAR_INITIALRECS_: {
    Logical zOp = (Va->item == zVAR_INITIALRECS_);
    struct CDFstruct *CDF; struct VarStruct *Var;
    long recNum, valueN; int how; void *buffer; long offset;
    long nInitialRecs = va_arg (Va->ap, long); OFF_T nValues;
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    if (!sX(InitCurrentVar64(CDF,zOp,&Var),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (Var->maxRec > NO_RECORD) return CANNOT_CHANGE;
    if (nInitialRecs < 1) return BAD_INITIAL_RECS;
    if (!Var->recVary && nInitialRecs > 1) return BAD_INITIAL_RECS;
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    if (!CDF->singleFile && Var->fp == NULL) {
      if (!sX(OpenVar64(CDF,Var),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    if (!sX(BuildPadBuffer64(CDF,Var,(Int32)nInitialRecs,
			     &how,&buffer,FALSE),&pStatus)) return pStatus;
    switch (how) {
      case ALLrecordsATonce:
	nValues = (OFF_T) nInitialRecs * Var->NphyRecValues;
	if (!sX(WriteVarValues64(CDF,Var,INT32_ZERO,INT32_ZERO,
			         nValues,buffer),&pStatus)) {
	  cdf_FreeMemory (buffer, NULL);
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
	break;
      case ONErecordATaTIME:
	for (recNum = 0; recNum < nInitialRecs; recNum++) {
	   if (!sX(WriteVarValues64(CDF,Var,(Int32)recNum,INT32_ZERO,
				    (OFF_T) Var->NphyRecValues,buffer),&pStatus)) {
	     cdf_FreeMemory (buffer, NULL);
	     AbortAccess64 (CDF, UPDATE, noDELETE);
	     return pStatus;
	   }
	}
	break;
      case ONEvalueATaTIME:
	for (recNum = 0; recNum < nInitialRecs; recNum++) {
	   for (valueN = 0; valueN < Var->NphyRecValues; valueN++) {
	      offset = valueN * Var->NvalueBytes;
	      if (!sX(WriteVarValues64(CDF,Var,(Int32)recNum,
				       (Int32)offset,
				       (OFF_T) 1,buffer),&pStatus)) {
		cdf_FreeMemory (buffer, NULL);
		AbortAccess64 (CDF, UPDATE, noDELETE);
		return pStatus;
	      }
	   }
	}
	break;
    }
    cdf_FreeMemory (buffer, NULL);
    break;
  }

  /****************************************************************************
  * rVAR_BLOCKINGFACTOR_/zVAR_BLOCKINGFACTOR_
  ****************************************************************************/
  case rVAR_BLOCKINGFACTOR_:
  case zVAR_BLOCKINGFACTOR_: {
    Logical zOp = (Va->item == zVAR_BLOCKINGFACTOR_);
    struct CDFstruct *CDF; struct VarStruct *Var;
    Int32 blocking = (Int32) va_arg (Va->ap, long), flags;
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    if (!sX(InitCurrentVar64(CDF,zOp,&Var),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Validate blocking factor.  Zero (0) is valid meaning that the default
    * is to be used.
    **************************************************************************/
    switch (Var->vType) {
      case STANDARD_:
	break;
      case SPARSE_RECORDS_:
	if (Var->stage.areaOffset64 != (OFF_T) NO_OFFSET64) return CANNOT_CHANGE;
	break;
      case COMPRESSED_:
      case SPARSE_COMPRESSED_RECORDS_:
	if (Var->maxRec > NO_RECORD) return CANNOT_CHANGE;
	break;
      case SPARSE_ARRAYS_:
      case SPARSE_RECORDS_AND_ARRAYS_:
	return UNKNOWN_SPARSENESS;
      case IN_MULTI_:
	break;
      default:
	return CDF_INTERNAL_ERROR;
    }
    if (blocking < 0) return BAD_BLOCKING_FACTOR;
    /**************************************************************************
    * Get write access.
    **************************************************************************/
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    /**************************************************************************
    * Update VDR with new blocking factor.
    **************************************************************************/
    if (!sX(ReadVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
		      VDR_FLAGS,&flags,
		      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (!RECvaryBITset(flags) && blocking > 1) {
      blocking = 1;
      sX (FORCED_PARAMETER, &pStatus);
    }
    if (!sX(WriteVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
		       VDR_BLOCKING,&blocking,
		       VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Recalculate the variable parameters.
    *************************************************************************/
    if (!sX(CalcBF64(CDF,Var),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    break;
  }

  /****************************************************************************
  * rVAR_PADVALUE_/zVAR_PADVALUE_
  ****************************************************************************/
  case rVAR_PADVALUE_:
  case zVAR_PADVALUE_: {
    Logical zOp = (Va->item == zVAR_PADVALUE_);
    struct CDFstruct *CDF; struct VarStruct *Var; struct VDRstruct64 VDR;
    void *padValue = va_arg (Va->ap, void *);
    OFF_T newOffset;    /* Offset of VDR after "possibly" being moved. */
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    /**************************************************************************
    * Initialize variable.
    **************************************************************************/
    if (!sX(InitCurrentVar64(CDF,zOp,&Var),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Get write access.
    **************************************************************************/
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    if (Var->dataType == CDF_TIME_TT2000) {
      if (!UpdateTT2000header(CDF,&pStatus)) return pStatus;
    }
    /**************************************************************************
    * Read the variable's VDR.
    **************************************************************************/
    if (!sX(ReadVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
		      VDR_RECORD,&VDR,NULL,
		      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (PADvalueBITset(VDR.Flags)) {
      /************************************************************************
      * A pad value has already been specified - simply overwrite the existing
      * pad value.
      ************************************************************************/
      if (!sX(WriteVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
		         VDR_PADVALUE,padValue,
		         VDR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    else {
      /************************************************************************
      * A pad value has not yet been specified.
      ************************************************************************/
      int nBytes = (int) (CDFelemSize(VDR.DataType) * VDR.NumElems);
      SetBit32 (&VDR.Flags,VDR_PADVALUE_BIT);
      if (!sX(ResizeIR64(CDF,Var->VDRoffset64,
		         VDR.RecordSize+nBytes,
		         &newOffset,TRUE,NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      VDR.RecordSize += nBytes;
      if (!sX(WriteVDR64(CDF,CDF->fp,newOffset,Var->zVar,
		         VDR_RECORD,&VDR,padValue,
		         VDR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (newOffset != Var->VDRoffset64) {
	if (VDR.Num > 0) {
	  OFF_T prevOffset;
	  if (!sX(FindVarByNumber64(CDF,(VDR.Num-1),
				    Var->zVar,&prevOffset),&pStatus)) {
	    AbortAccess64 (CDF, UPDATE, noDELETE);
	    return pStatus;
	  }
	  if (!sX(WriteVDR64(CDF,CDF->fp,prevOffset,Var->zVar,
			     VDR_VDRNEXT,&newOffset,
			     VDR_NULL),&pStatus)) {
	    AbortAccess64 (CDF, UPDATE, noDELETE);
	    return pStatus;
	  }
	}
	else {
	  if (!sX(WriteGDR64(CDF->fp,CDF->GDRoffset64,
			     BOO(Var->zVar,GDR_zVDRHEAD,GDR_rVDRHEAD),&newOffset,
			     GDR_NULL),&pStatus)) {
	    AbortAccess64 (CDF, UPDATE, noDELETE);
	    return pStatus;
	  }
	}
	Var->VDRoffset64 = newOffset;

        if (zModeON(CDF)) {
          CDF->CURzVarOffset64 = newOffset;
        }
        else {
          if (zOp) {
            CDF->CURzVarOffset64 = newOffset;
          } else {
            CDF->CURrVarOffset64 = newOffset;
          }
        }

      }
    }
    /**************************************************************************
    * Indicate that allocated records may have to be padded when the CDF is
    * closed (if the proper type variable).
    **************************************************************************/
    switch (Var->vType) {
      case STANDARD_:
      case SPARSE_RECORDS_:
	if (Var->maxAllocated > Var->maxRec) Var->maxWritten = Var->maxRec;
	break;
      case COMPRESSED_:
      case SPARSE_COMPRESSED_RECORDS_:
      case SPARSE_ARRAYS_:
      case SPARSE_RECORDS_AND_ARRAYS_:
      case IN_MULTI_:
	break;
      default:
	return CDF_INTERNAL_ERROR;
    }
    break;
  }

  /****************************************************************************
  * rVAR_DATA_/zVAR_DATA_, 
  ****************************************************************************/
  case rVAR_DATA_:
  case zVAR_DATA_: {
    /**************************************************************************
    * Local variables.
    **************************************************************************/
    Logical zOp = (Va->item == zVAR_DATA_);
    struct CDFstruct *CDF; struct VarStruct *Var;
    Int32 phyRecNum; struct rdSTRUCT *rd; Int32 offset;
    /**************************************************************************
    * Get arguments.
    **************************************************************************/
    void *value = va_arg (Va->ap, char *);
    /**************************************************************************
    * Validate current CDF/variable.
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    /**************************************************************************
    * Initialize variable.
    **************************************************************************/
    if (!sX(InitCurrentVar64(CDF,zOp,&Var),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Get write access.
    **************************************************************************/
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    if (Var->dataType == CDF_TIME_TT2000) {
      if (!UpdateTT2000header(CDF,&pStatus)) return pStatus;
    }
    /**************************************************************************
    * If a multi-file CDF, open variable file (if closed).
    **************************************************************************/
    if (!CDF->singleFile && Var->fp == NULL) {
      if (!sX(OpenVar64(CDF,Var),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    /**************************************************************************
    * Write value.
    **************************************************************************/
    rd = BOO(zModeON(CDF),&(Var->zRD),BOO(zOp,&(Var->zRD),&(CDF->rRD)));
    phyRecNum = (Var->recVary ? rd->recNumber : 0);
    offset = IndicesValueOffset(Var->numDims,
				rd->dimIndices,
				Var->dimVarys,
				Var->nPhyDimValues) * Var->NvalueBytes;
    if (!sX(WriteVarValues64(CDF,Var,phyRecNum,
			     offset,(OFF_T) 1,value),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Update last access time.
    **************************************************************************/
    Var->accessed_at = CDF->pseudo_clock++;
    if (!sX(SetLeapSecondLastUpdate (CDF, Var, zOp), &pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    break;
  }

  /****************************************************************************
  * rVAR_HYPERDATA_/zVAR_HYPERDATA_, 
  ****************************************************************************/
  case rVAR_HYPERDATA_:
  case zVAR_HYPERDATA_: {
    /**************************************************************************
    * Local variables.
    **************************************************************************/
    Logical zOp = (Va->item == zVAR_HYPERDATA_);
    int dimN; struct CDFstruct *CDF; struct VarStruct *Var;
    struct rdSTRUCT *rd;
#if LIMITof64K
    long Nvalues, Nbytes;
#endif
    /**************************************************************************
    * Get arguments.
    **************************************************************************/
    void *buffer = va_arg (Va->ap, char *);
    /**************************************************************************
    * Validate current CDF/variable.
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    /**************************************************************************
    * Initialize variable.
    **************************************************************************/
    if (!sX(InitCurrentVar64(CDF,zOp,&Var),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Validate hyper parameters.
    **************************************************************************/
    rd = BOO(zModeON(CDF),&(Var->zRD),BOO(zOp,&(Var->zRD),&(CDF->rRD)));
    for (dimN = 0; dimN < Var->numDims; dimN++) {
       long maxIndex = rd->dimIndices[dimN] +
		       ((rd->dimCounts[dimN] - 1) * rd->dimIntervals[dimN]);
       if (maxIndex >= Var->dimSizes[dimN]) return BAD_DIM_INDEX;
    }
#if LIMITof64K
    Nvalues = rd->recCount;
    for (dimN = 0; dimN < Var->numDims; dimN++) Nvalues *= rd->dimCounts[dimN];
    Nbytes = Nvalues * Var->NvalueBytes;
    if (TOObigIBMpc(Nbytes)) return IBM_PC_OVERFLOW;
#endif
    /**************************************************************************
    * Get write access.
    **************************************************************************/
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    if (Var->dataType == CDF_TIME_TT2000) {
      if (!UpdateTT2000header(CDF,&pStatus)) return pStatus;
    }
    /**************************************************************************
    * If a multi-file CDF, open variable file (if closed).
    **************************************************************************/
    if (!CDF->singleFile && Var->fp == NULL) {
      if (!sX(OpenVar64(CDF,Var),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    /**************************************************************************
    * Write values.
    **************************************************************************/
    if (!sX(HyperWrite64(CDF,Var,rd,buffer),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Update last access time.
    **************************************************************************/
    Var->accessed_at = Cur->cdf->pseudo_clock++;
    if (!sX(SetLeapSecondLastUpdate (CDF, Var, zOp), &pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    break;
  }

  /****************************************************************************
  * rVAR_SEQDATA_/zVAR_SEQDATA_, 
  ****************************************************************************/
  case rVAR_SEQDATA_:
  case zVAR_SEQDATA_: {
    /**************************************************************************
    * Local variables.
    **************************************************************************/
    Logical zOp = (Va->item == zVAR_SEQDATA_);
    struct VarStruct *Var; struct CDFstruct *CDF; Int32 phyRecNum; 
    Int32 offset;
    /**************************************************************************
    * Get arguments.
    **************************************************************************/
    void *value = va_arg (Va->ap, char *);
    /**************************************************************************
    * Validate current CDF/variable.
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    /**************************************************************************
    * Initialize variable.
    **************************************************************************/
    if (!sX(InitCurrentVar64(CDF,zOp,&Var),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Compute physical record number and determine if this is a legal write.
    **************************************************************************/
    phyRecNum = (Int32) (Var->seqValueOffset64 / (OFF_T) Var->NphyRecValues);
    if ((!Var->recVary) && (phyRecNum > 0)) return END_OF_VAR;
    /**************************************************************************
    * Get write access.
    **************************************************************************/
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    if (Var->dataType == CDF_TIME_TT2000) {
      if (!UpdateTT2000header(CDF,&pStatus)) return pStatus;
    }
    /**************************************************************************
    * If a multi-file CDF, open variable file (if closed).
    **************************************************************************/
    if (!CDF->singleFile && Var->fp == NULL) {
      if (!sX(OpenVar64(CDF,Var),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    /**************************************************************************
    * Write value.
    **************************************************************************/
    offset = (Int32) (Var->seqValueOffset64 % (OFF_T) Var->NphyRecValues) * 
		     Var->NvalueBytes;
    if (!sX(WriteVarValues64(CDF,Var,phyRecNum,
			     offset,(OFF_T) 1,value),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Update last access time and current sequential position.
    **************************************************************************/
    Var->seqValueOffset64++;
    Var->accessed_at = Cur->cdf->pseudo_clock++;
    if (!sX(SetLeapSecondLastUpdate (CDF, Var, zOp), &pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    break;
  }

  /****************************************************************************
  * rVARs_RECDATA_/zVARs_RECDATA_, write physical data records for up to all
  * of the rVariables/zVariables.
  ****************************************************************************/
  case rVARs_RECDATA_:
  case zVARs_RECDATA_: {
    /**************************************************************************
    * Local variables.
    **************************************************************************/
    Logical zOp = (Va->item == zVARs_RECDATA_), zVar;
    struct VarStruct *Var; struct CDFstruct *CDF;
    Int32 recNum, varNt; Byte1 *tBuffer; int varX;
#if LIMITof64K
    long nBytes;
#endif
    /**************************************************************************
    * Get arguments.
    **************************************************************************/
    long nVars = va_arg (Va->ap, long);
    long *varNs = va_arg (Va->ap, long *);
    void *buffer = va_arg (Va->ap, char *);
    /**************************************************************************
    * Validate...
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (nVars < 1) return BAD_NUM_VARS;
    /**************************************************************************
    * Get write access.
    **************************************************************************/
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    /**************************************************************************
    * Initialize variables and check that each can be accessed.
    **************************************************************************/
    for (varX = 0; varX < nVars; varX++) {
       if (!sX(VarIdentity(CDF,(Int32)varNs[varX],
			   zOp,&varNt,&zVar,NULL),&pStatus)) return pStatus;
       if (!sX(InitVar64(CDF,varNt,zVar,&Var),&pStatus)) {
	 AbortAccess64 (CDF, UPDATE, noDELETE);
	 return pStatus;
       }
    }
    /**************************************************************************
    * Check for too large a write under DOS (64K limit).
    **************************************************************************/
#if LIMITof64K
    for (varX = 0, nBytes = 0; varX < nVars; varX++) {
       if (!sX(VarIdentity(CDF,(Int32)varNs[varX],
			   zOp,NULL,NULL,&Var),&pStatus)) return pStatus;
       nBytes += Var->NphyRecBytes;
    }
    if (TOObigIBMpc(nBytes)) return IBM_PC_OVERFLOW;
#endif
    /**************************************************************************
    * Write to each selected variable...
    **************************************************************************/
    for (varX = 0, tBuffer = buffer; varX < nVars; varX++) {
       /***********************************************************************
       * Get variable...
       ***********************************************************************/
       if (!sX(VarIdentity(CDF,(Int32)varNs[varX],
			   zOp,NULL,NULL,&Var),&pStatus)) return pStatus;
       /***********************************************************************
       * If a multi-file CDF, open variable file (if closed).
       ***********************************************************************/
       if (!CDF->singleFile && Var->fp == NULL) {
	 if (!sX(OpenVar64(CDF,Var),&pStatus)) {
	   AbortAccess64 (CDF, UPDATE, noDELETE);
	   return pStatus;
	 }
       }
       /***********************************************************************
       * Write variable value(s).
       ***********************************************************************/
       recNum = BOO(Var->recVary,BOO(zOp,Var->zRD.recNumber,
					 CDF->rRD.recNumber),0);
       if (!sX(WriteVarValues64(CDF,Var,recNum,INT32_ZERO,
			        (OFF_T) Var->NphyRecValues,tBuffer),&pStatus)) {
	 AbortAccess64 (CDF, UPDATE, noDELETE);
	 return pStatus;
       }
       /***********************************************************************
       * Increment buffer pointer and update time of last access for variable.
       ***********************************************************************/
       tBuffer += (size_t) Var->NphyRecBytes;
       Var->accessed_at = CDF->pseudo_clock++;
    }
    if (!sX(SetLeapSecondLastUpdate (CDF, Var, zOp), &pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    break;
  }
  /****************************************************************************
  * Look in `cdfput2.c' for these items.
  ****************************************************************************/
  case CDF_ENCODING_:
  case CDF_MAJORITY_:
  case CDF_FORMAT_:
  case CDF_COMPRESSION_:
  case ATTR_NAME_:
  case ATTR_SCOPE_:
  case gENTRY_DATA_:
  case rENTRY_DATA_:
  case zENTRY_DATA_:
  case gENTRY_DATASPEC_:
  case rENTRY_DATASPEC_:
  case zENTRY_DATASPEC_:
  case rVAR_COMPRESSION_:
  case zVAR_COMPRESSION_:
  case rVAR_SPARSERECORDS_:
  case zVAR_SPARSERECORDS_:
  case rVAR_SPARSEARRAYS_:
  case zVAR_SPARSEARRAYS_:
  case CDF_CHECKSUM_:
  case CDF_LEAPSECONDLASTUPDATED_:
    return CDFput2_64 (Va, Cur);
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
