/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                        CDF `confirm' operations.
*
*  Version 1.5a, 21-Feb-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  20-May-92, J Love     Original version (was part of `cdflib.c').
*   V1.1  21-Aug-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V1.2  21-Dec-93, J Love     CDF V2.4.
*   V1.2a 22-Feb-94, J Love     Spelling lesson.
*   V1.3  19-Dec-94, J Love     CDF V2.5.
*   V1.3a 24-Feb-95, J Love	Solaris 2.3 IDL i/f.
*   V1.4  15-May-95, J Love	Added <CONFIRM_,CDF_>.
*   V1.4a  8-Jun-95, J Love	Added <CONFIRM_,r/zVAR_PADVALUE_>.
*   V1.4b  4-Aug-95, J Love	CDFexport-related changes.
*   V1.5   5-Sep-96, J Love	CDF V2.6.
*   V1.5a 21-Feb-97, J Love	Removed RICE.
*
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"

/******************************************************************************
* CDFcon.
******************************************************************************/

STATICforIDL CDFstatus CDFcon (Va, Cur)
struct VAstruct *Va;
struct CurStruct *Cur;
{
CDFstatus tStatus, pStatus = CDF_OK;

switch (Va->item) {
  /****************************************************************************
  * CDF_
  ****************************************************************************/
  case CDF_: {
    CDFid *id = va_arg (Va->ap, CDFid *);
    *id = (CDFid) Cur->cdf;
    break;
  }
  /****************************************************************************
  * CDF_STATUS_
  ****************************************************************************/
  case CDF_STATUS_: {
    CDFstatus *status = va_arg (Va->ap, CDFstatus *);
    *status = Cur->status;
    break;
  }
  /****************************************************************************
  * CDF_NAME_
  ****************************************************************************/
  case CDF_NAME_: {
    struct CDFstruct *CDF;
    char *CDFname = va_arg (Va->ap, char *);
    SelectCDF (Cur->cdf, CDF)
    strcpyX (CDFname, CDF->CDFname, CDF_PATHNAME_LEN);
    break;
  }
  /****************************************************************************
  * CDF_ACCESS_
  * The `SelectCDF' macro returns NO_MORE_ACCESS if that is the case.
  ****************************************************************************/
  case CDF_ACCESS_: {
    if (Cur->cdf == NULL) return NO_CDF_SELECTED;
    if (Cur->cdf->magic == ABORTEDid_MAGIC_NUMBER) return NO_MORE_ACCESS;
    break;
  }
  /****************************************************************************
  * CDF_READONLY_MODE_
  ****************************************************************************/
  case CDF_READONLY_MODE_: {
    struct CDFstruct *CDF;
    long *mode = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    *mode = BOO(CDF->readOnly,READONLYon,READONLYoff);
    break;
  }
  /****************************************************************************
  * CDF_zMODE_
  ****************************************************************************/
  case CDF_zMODE_: {
    struct CDFstruct *CDF;
    long *mode = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    *mode = CDF->zMode;
    break;
  }
  /****************************************************************************
  * CDF_NEGtoPOSfp0_MODE_
  ****************************************************************************/
  case CDF_NEGtoPOSfp0_MODE_: {
    struct CDFstruct *CDF;
    long *mode = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    *mode = BOO(CDF->negToPosFp0,NEGtoPOSfp0on,NEGtoPOSfp0off);
    break;
  }
  /****************************************************************************
  * CDF_DECODING_
  ****************************************************************************/
  case CDF_DECODING_: {
    struct CDFstruct *CDF;
    long *decoding = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    *decoding = CDF->decoding;
    break;
  }
  /****************************************************************************
  * CDF_CACHESIZE_
  ****************************************************************************/
  case CDF_CACHESIZE_: {
    struct CDFstruct *CDF;
    long *nBuffers = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    *nBuffers = (long) CDF->workingCacheSize;
    break;
  }
  /****************************************************************************
  * STAGE_CACHESIZE_
  ****************************************************************************/
  case STAGE_CACHESIZE_: {
    struct CDFstruct *CDF;
    long *nBuffers = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    *nBuffers = (long) CDF->stage.cacheSize;
    break;
  }
  /****************************************************************************
  * COMPRESS_CACHESIZE_
  ****************************************************************************/
  case COMPRESS_CACHESIZE_: {
    struct CDFstruct *CDF;
    long *nBuffers = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    *nBuffers = (long) CDF->compressCacheSize;
    break;
  }
  /****************************************************************************
  * rVAR_CACHESIZE_/zVAR_CACHESIZE_
  ****************************************************************************/
  case rVAR_CACHESIZE_:
  case zVAR_CACHESIZE_: {
    Logical zOp = (Va->item == zVAR_CACHESIZE_);
    struct CDFstruct *CDF;
    struct VarStruct *Var;
    long *nBuffers = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    if (!CDF->singleFile) {
      if (!sX(InitCurrentVar(CDF,zOp,&Var),&pStatus)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      *nBuffers = (long) Var->varCacheSize;
    }
    else
      sX (SINGLE_FILE_FORMAT, &pStatus);
    break;
  }
  /****************************************************************************
  * rVAR_/zVAR_
  ****************************************************************************/
  case rVAR_:
  case zVAR_: {
    Logical zOp = (Va->item == zVAR_);
    struct CDFstruct *CDF;
    long *varNum = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    *varNum = BOO(zOp,CDF->CURzVarNum,CDF->CURrVarNum);
    break;
  }
  /****************************************************************************
  * rVAR_PADVALUE_/zVAR_PADVALUE_
  ****************************************************************************/
  case rVAR_PADVALUE_:
  case zVAR_PADVALUE_: {
    Logical zOp = (Va->item == zVAR_PADVALUE_), zVar;
    struct CDFstruct *CDF;
    Int32 offset, flags;
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
/*
    if (!sX(LocateCurrentVar(CDF,zOp,&offset,&zVar,NULL),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
*/

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) offset = CDF->CURzVarOffset;
    else offset = CDF->CURrVarOffset;

    if (!sX(ReadVDR(CDF,CDF->fp,offset,zVar,
		    VDR_FLAGS,&flags,
		    VDR_NULL),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (!PADvalueBITset(flags)) sX (NO_PADVALUE_SPECIFIED, &pStatus);
    break;
  }
  /****************************************************************************
  * rVAR_EXISTENCE_/zVAR_EXISTENCE_, confirm the existence of a named variable.
  ****************************************************************************/
  case rVAR_EXISTENCE_:
  case zVAR_EXISTENCE_: {
    Logical zOp = (Va->item == zVAR_EXISTENCE_), zVar;
    struct CDFstruct *CDF;
    char *varName = va_arg (Va->ap, char *);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    tStatus = FindVarByName (CDF, varName, NULL, &zVar, NULL);
    switch (tStatus) {
      case CDF_OK:
	break;
      case NO_SUCH_VAR:
	return tStatus;
      default:
	AbortAccess (CDF, UPDATE, noDELETE);
	return tStatus;
    }
    if (!zModeON(CDF)) {
      if ((zVar && !zOp) || (!zVar && zOp)) return NO_SUCH_VAR; /*Wrong type*/
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
    long *value = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,TRUE)) return ILLEGAL_IN_zMODE;
    if (CDF->NrVars == 0) {
      if (!sX(NO_VARS_IN_CDF,&pStatus)) return pStatus;
    }
    switch (Va->item) {
      case rVARs_RECNUMBER_:
	*value = CDF->rRD.recNumber;
	break;
      case rVARs_RECCOUNT_:
	*value = CDF->rRD.recCount;
	break;
      case rVARs_RECINTERVAL_:
	*value = CDF->rRD.recInterval;
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
    long *value = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTvarSELECTED(CDF,TRUE)) return NO_VAR_SELECTED;
    if (!sX(InitCurrentVar(CDF,TRUE,&Var),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    switch (Va->item) {
      case zVAR_RECNUMBER_:
	*value = Var->zRD.recNumber;
	break;
      case zVAR_RECCOUNT_:
	*value = Var->zRD.recCount;
	break;
      case zVAR_RECINTERVAL_:
	*value = Var->zRD.recInterval;
	break;
    }
    break;
  }
  /****************************************************************************
  * rVARs_DIMINDICES_/rVARs_DIMCOUNTS_/rVARs_DIMINTERVALS_
  ****************************************************************************/
  case rVARs_DIMINDICES_:
  case rVARs_DIMCOUNTS_:
  case rVARs_DIMINTERVALS_: {
    struct CDFstruct *CDF; int dimN;
    long *values = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,TRUE)) return ILLEGAL_IN_zMODE;
    if (CDF->NrVars == 0) {
      if (!sX(NO_VARS_IN_CDF,&pStatus)) return pStatus;
    }
    switch (Va->item) {
      case rVARs_DIMINDICES_:
	for (dimN = 0; dimN < CDF->rNumDims; dimN++)
	   values[dimN] = CDF->rRD.dimIndices[dimN];
	break;
      case rVARs_DIMCOUNTS_:
	for (dimN = 0; dimN < CDF->rNumDims; dimN++)
	   values[dimN] = CDF->rRD.dimCounts[dimN];
	break;
      case rVARs_DIMINTERVALS_:
	for (dimN = 0; dimN < CDF->rNumDims; dimN++)
	   values[dimN] = CDF->rRD.dimIntervals[dimN];
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
    int dimN;
    long *values = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTvarSELECTED(CDF,TRUE)) return NO_VAR_SELECTED;
    if (!sX(InitCurrentVar(CDF,TRUE,&Var),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    switch (Va->item) {
      case zVAR_DIMINDICES_:
	for (dimN = 0; dimN < Var->numDims; dimN++)
	   values[dimN] = Var->zRD.dimIndices[dimN];
	break;
      case zVAR_DIMCOUNTS_:
	for (dimN = 0; dimN < Var->numDims; dimN++)
	   values[dimN] = Var->zRD.dimCounts[dimN];
	break;
      case zVAR_DIMINTERVALS_:
	for (dimN = 0; dimN < Var->numDims; dimN++)
	   values[dimN] = Var->zRD.dimIntervals[dimN];
	break;
    }
    break;
  }
  /****************************************************************************
  * rVAR_SEQPOS_/zVAR_SEQPOS_
  ****************************************************************************/
  case rVAR_SEQPOS_:
  case zVAR_SEQPOS_: {
    Logical zOp = (Va->item == zVAR_SEQPOS_);
    struct CDFstruct *CDF; struct VarStruct *Var;
    Int32 indices[CDF_MAX_DIMS]; int dimN;
    long *recNumber = va_arg (Va->ap, long *);
    long *dimIndices = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    if (!sX(InitCurrentVar(CDF,zOp,&Var),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    *recNumber = (long) (Var->seqValueOffset / Var->NphyRecValues);
    ValueOffsetIndices (Var->seqValueOffset % Var->NphyRecValues,
			CDF->rowMajor, Var->numDims, Var->dimVarys,
			Var->nPhyDimValues, indices);
    for (dimN = 0; dimN < Var->numDims; dimN++) {
       dimIndices[dimN] = (long) indices[dimN];
    }
    break;
  }
  /****************************************************************************
  * rVAR_RESERVEPERCENT_/zVAR_RESERVEPERCENT_
  ****************************************************************************/
  case rVAR_RESERVEPERCENT_:
  case zVAR_RESERVEPERCENT_: {
    Logical zOp = (Va->item == zVAR_RESERVEPERCENT_);
    struct CDFstruct *CDF; struct VarStruct *Var;
    long *pct = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    if (!sX(InitCurrentVar(CDF,zOp,&Var),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    switch (Var->vType) {
      case COMPRESSED_:
      case SPARSE_COMPRESSED_RECORDS_:
	*pct = (long) Var->reservePct;
	break;
      case SPARSE_ARRAYS_:
      case SPARSE_RECORDS_AND_ARRAYS_:
	return CDF_INTERNAL_ERROR;
      case STANDARD_:
      case SPARSE_RECORDS_:
	*pct = 0;
	sX (NA_FOR_VARIABLE, &pStatus);
	break;
      case IN_MULTI_:
	*pct = 0;
	sX (MULTI_FILE_FORMAT, &pStatus);
	break;
    }
    break;
  }
  /****************************************************************************
  * ATTR_
  ****************************************************************************/
  case ATTR_: {
    struct CDFstruct *CDF; Int32 tAttrNum;
    long *attrNum = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTattrSELECTED(CDF)) return NO_ATTR_SELECTED;
    if (!sX(ReadADR(CDF->fp,CDF->CURattrOffset,
		    ADR_NUM,&tAttrNum,
		    ADR_NULL),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    *attrNum = (long) tAttrNum;
    break;
  }
  /****************************************************************************
  * ATTR_EXISTENCE_, confirms the existence of a named attribute.
  ****************************************************************************/
  case ATTR_EXISTENCE_: {
    struct CDFstruct *CDF;
    char *attrName = va_arg (Va->ap, char *);
    SelectCDF (Cur->cdf, CDF)
    tStatus = FindAttrByName (CDF, attrName, NULL);
    switch (tStatus) {
      case CDF_OK:
	break;
      case NO_SUCH_ATTR:
	return tStatus;;
      default:
	AbortAccess (CDF, UPDATE, noDELETE);
	return tStatus;
    }
    break;
  }
  /****************************************************************************
  * gENTRY_/rENTRY_/zENTRY_
  ****************************************************************************/
  case rENTRY_:
  case gENTRY_:
  case zENTRY_: {
    Logical zOp = (Va->item == zENTRY_);
    struct CDFstruct *CDF;
    long *entryNum = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,Va->item==rENTRY_)) return ILLEGAL_IN_zMODE;
    if (BOO(zOp,CDF->CURzEntryNum,CDF->CURgrEntryNum) == RESERVED_ENTRYNUM)
      return NO_ENTRY_SELECTED;
    *entryNum = BOO(zOp,CDF->CURzEntryNum,CDF->CURgrEntryNum);
    break;
  }
  /****************************************************************************
  * gENTRY_EXISTENCE_/rENTRY_EXISTENCE_/zENTRY_EXISTENCE_, confirms the
  * existence of a numbered entry for the current attribute.
  ****************************************************************************/
  case gENTRY_EXISTENCE_:
  case rENTRY_EXISTENCE_:
  case zENTRY_EXISTENCE_: {
    int entryType = E3p(Va->item,gENTRY_EXISTENCE_,
				 rENTRY_EXISTENCE_,
				 zENTRY_EXISTENCE_);
    struct CDFstruct *CDF; Logical zEntry;
    long entryNum = va_arg (Va->ap, long), entryN;
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTattrSELECTED(CDF)) return NO_ATTR_SELECTED;
    if (!sX(CheckEntryOp(CDF,entryType),&pStatus)) return pStatus;
    if (entryNum < 0) return BAD_ENTRY_NUM;
    switch (entryType) {
      case gENTRYt:
      case rENTRYt:
	zEntry = FALSE;
	entryN = entryNum;
	break;
      case zENTRYt:
	zEntry = BOO(zModeON(CDF),BOO(entryNum < CDF->NrVars,FALSE,TRUE),TRUE);
	entryN = BOO(zModeON(CDF),BOO(entryNum < CDF->NrVars,entryNum,
				      entryNum - CDF->NrVars),entryNum);
	break;
    }
    tStatus = FindEntryByNumber(CDF,CDF->CURattrOffset,
				zEntry,(Int32)entryN,NULL);
    switch (tStatus) {
      case CDF_OK:
	break;
      case NO_SUCH_ENTRY:
	return tStatus;
      default:
	AbortAccess (CDF, UPDATE, noDELETE);
	return tStatus;
    }
    break;
  }
  /****************************************************************************
  * CURgENTRY_EXISTENCE_/CURrENTRY_EXISTENCE_/CURzENTRY_EXISTENCE_, confirms
  * the existence of the current entry number for the current attribute.
  ****************************************************************************/
  case CURgENTRY_EXISTENCE_:
  case CURrENTRY_EXISTENCE_:
  case CURzENTRY_EXISTENCE_: {
    int entryType = E3p(Va->item,CURgENTRY_EXISTENCE_,
				 CURrENTRY_EXISTENCE_,
				 CURzENTRY_EXISTENCE_);
    struct CDFstruct *CDF; Int32 offset;
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTattrSELECTED(CDF)) return NO_ATTR_SELECTED;
    if (E3(entryType,
	   CDF->CURgrEntryNum,
	   CDF->CURgrEntryNum,
	   CDF->CURzEntryNum) == RESERVED_ENTRYNUM) return NO_ENTRY_SELECTED;
    if (!sX(CheckEntryOp(CDF,entryType),&pStatus)) return pStatus;
    offset = E3(entryType,CDF->CURgrEntryOffset,
			  CDF->CURgrEntryOffset,
			  CDF->CURzEntryOffset);
    if (offset == RESERVED_ENTRYOFFSET) return NO_SUCH_ENTRY;
    break;
  }
  /****************************************************************************
  * CDF_CHECKSUM_
  ****************************************************************************/
  case CDF_CHECKSUM_: {
    struct CDFstruct *CDF;
    SelectCDF (Cur->cdf, CDF)
    if (!sX(CDFVerifyChecksum(CDF),&pStatus)) return pStatus;
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
