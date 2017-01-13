/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                            Calculate statistics.
*
*  Version 2.6a, 18-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  29-Aug-91, J Love     Original version (for CDF V2.1).
*   V2.0  16-Mar-92, J Love     IBM PC port.  Added fill value filtering.
*   V2.1  21-Aug-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V2.2  10-Dec-93, J Love     CDF V2.4.
*   V2.3   6-Dec-94, J Love     CDF V2.5.
*   V2.4  15-May-95, J Love	Moved `ASSIGNx', `EQx', etc. to `toolbox3.c'
*				(and some calling sequences changed).
*   V2.5   1-Sep-95, J Love	Hyper groups.
*   V2.5a 19-Sep-95, J Love	CHECKforABORTso.
*   V2.5b 29-Sep-95, J Love	Less CHECKforABORTso.
*   V2.6  22-Jul-96, J Love	CDF V2.6.
*   V2.6a 18-Nov-97, J Love	Windows NT/Visual C++.
*
******************************************************************************/

#include "cdfstats.h"

/******************************************************************************
* CALCstat.  Reads each record of a variable and calculates the statistics.
******************************************************************************/

Logical CALCstat (Var, virtual)
struct VarStruct *Var;
int virtual;
{
  CDFstatus status;
  long attrN;
  long varyCount;
  int dimN;
  long dimSizes[CDF_MAX_DIMS];
  size_t nValueBytes[1];
  Byte1 **handles[1];
  long nHypers, hyperN;
  long nValues, valueN;
  struct GroupStruct groups;
  struct HyperStruct hyper;

  /****************************************************************************
  * Initialize for this variable.
  ****************************************************************************/
  Var->low = 0;
  Var->high = 0;
  Var->fills = 0;
  Var->oneINrange = FALSE;
  Var->minmaxInited = FALSE;
  Var->monoInited = FALSE;
  varyCount = BOO(Var->recVary,1,0);
  for (dimN = 0; dimN < Var->numDims; dimN++) {
     if (Var->dimVarys[dimN]) {
       dimSizes[dimN] = Var->dimSizes[dimN];
       varyCount++;
     }
     else
       dimSizes[dimN] = 1;
  }
  if (varyCount == 1)
    Var->checkMonotonicVar = TRUE;
  else
    Var->checkMonotonicVar = FALSE;
  Var->nValueBytes = Var->numElemsV * CDFelemSize(Var->dataTypeV);
  if (epochMonotonic) {
    if (!virtual && !CDFepochDataType(Var->dataTypeV)) return TRUE;
  }
  /****************************************************************************
  * Allocate memory.
  ****************************************************************************/
  handles[0] = &(Var->buffer);
  nValueBytes[0] = (size_t) Var->nValueBytes;
  AllocateBuffers (Var->varMaxRec + 1, Var->numDims, dimSizes, &groups, 0, 1,
		   handles, nValueBytes, ROWmajor(majority), MINnHYPERS,
		   FatalError);
  Var->min = cdf_AllocateMemory ((size_t) Var->nValueBytes,
			     FatalError);
  Var->max = cdf_AllocateMemory ((size_t) Var->nValueBytes,
			     FatalError);
  Var->last = cdf_AllocateMemory ((size_t) Var->nValueBytes,
			      FatalError);
  if (Var->rangeCheckVar) {
    Var->minINrange = cdf_AllocateMemory ((size_t) Var->nValueBytes,
				      FatalError);
    Var->maxINrange = cdf_AllocateMemory ((size_t) Var->nValueBytes,
				      FatalError);
  }
  /****************************************************************************
  * Read each value...
  ****************************************************************************/
  status = CDFlib (SELECT_, VAR(Var->Z), Var->varN,
		   NULL_);
  if (!StatusHandlerStats(status)) return FALSE;
  InitHyperParms (&hyper, &groups, Var->numDims, &nHypers, &nValues);
  for (hyperN = 0; hyperN < nHypers; hyperN++) {
     status = CDFlib (SELECT_, BOO(Var->Z,zVAR_RECNUMBER_,
					  rVARs_RECNUMBER_), hyper.recNumber,
			       BOO(Var->Z,zVAR_RECCOUNT_,
					  rVARs_RECCOUNT_), hyper.recCount,
			       BOO(Var->Z, zVAR_RECINTERVAL_,
					   rVARs_RECINTERVAL_), hyper.recInterval,
			       BOO(Var->Z,zVAR_DIMINDICES_,
					  rVARs_DIMINDICES_), hyper.dimIndices,
			       BOO(Var->Z,zVAR_DIMCOUNTS_,
					  rVARs_DIMCOUNTS_), hyper.dimCounts,
			       BOO(Var->Z, zVAR_DIMINTERVALS_,
					   rVARs_DIMINTERVALS_), hyper.dimIntervals,
		      GET_, BOO(Var->Z,zVAR_HYPERDATA_,
				       rVAR_HYPERDATA_), Var->buffer,
		      NULL_);
     if (!StatusHandlerStats(status)) return FALSE;
     for (valueN = 0, Var->value = Var->buffer; valueN < nValues;
	  valueN++, Var->value += (size_t) Var->nValueBytes) {
	if (!epochMonotonic) {
	  if (Var->minmaxInited)
	    MinMaxCheck (Var);
	  else
	    MinMaxInit (Var);
	}
	if (Var->checkMonotonicVar) Monotonic (Var);
     }
     IncrHyperParms (&hyper, &groups, Var->numDims, ROWmajor(majority),
		     &nValues);
     CHECKforABORTso
  }
  /****************************************************************************
  * Display statistics.
  ****************************************************************************/
  DISPstat (Var, virtual);
  /****************************************************************************
  * Update attribute(s) if requested.  (NOTE: macros/functions could be used).
  ****************************************************************************/
  if (updateValids && Var->minmaxInited) {
    status = CDFlib (SELECT_, CDF_READONLY_MODE_, READONLYoff,
		     NULL_);
    if (!StatusHandlerStats(status)) return FALSE;
    status = CDFlib (GET_, ATTR_NUMBER_, "VALIDMIN", &attrN,
		     NULL_);
    if (status == NO_SUCH_ATTR) {
      status = CDFlib (CREATE_, ATTR_, "VALIDMIN", VARIABLE_SCOPE, &attrN,
		       NULL_);
      if (!StatusHandlerStats(status)) return FALSE;
    }
    else
      if (!StatusHandlerStats(status)) return FALSE;
    status = CDFlib (SELECT_, ATTR_, attrN,
			      BOO(Var->Z,zENTRY_,rENTRY_), Var->varN,
		     PUT_, BOO(Var->Z,zENTRY_DATA_,
				      rENTRY_DATA_), Var->dataTypeV,
						     Var->numElemsV, Var->min,
		     NULL_);
    if (!StatusHandlerStats(status)) return FALSE;
    status = CDFlib (GET_, ATTR_NUMBER_, "VALIDMAX", &attrN,
		     NULL_);
    if (status == NO_SUCH_ATTR) {
      status = CDFlib (CREATE_, ATTR_, "VALIDMAX", VARIABLE_SCOPE, &attrN,
		       NULL_);
      if (!StatusHandlerStats(status)) return FALSE;
    }
    else
      if (!StatusHandlerStats(status)) return FALSE;
    status = CDFlib (SELECT_, ATTR_, attrN,
			      BOO(Var->Z,zENTRY_,rENTRY_), Var->varN,
		     PUT_, BOO(Var->Z,zENTRY_DATA_,
				      rENTRY_DATA_), Var->dataTypeV,
						     Var->numElemsV, Var->max,
		     NULL_);
    if (!StatusHandlerStats(status)) return FALSE;
  }
  if (updateScales && Var->minmaxInited) {
    status = CDFlib (SELECT_, CDF_READONLY_MODE_, READONLYoff,
		     NULL_);
    if (!StatusHandlerStats(status)) return FALSE;
    status = CDFlib (GET_, ATTR_NUMBER_, "SCALEMIN", &attrN,
		     NULL_);
    if (status == NO_SUCH_ATTR) {
      status = CDFlib (CREATE_, ATTR_, "SCALEMIN", VARIABLE_SCOPE, &attrN,
		       NULL_);
      if (!StatusHandlerStats(status)) return FALSE;
    }
    else
      if (!StatusHandlerStats(status)) return FALSE;
    status = CDFlib (SELECT_, ATTR_, attrN,
			      BOO(Var->Z,zENTRY_,rENTRY_), Var->varN,
		     PUT_, BOO(Var->Z,zENTRY_DATA_,
				      rENTRY_DATA_), Var->dataTypeV,
						     Var->numElemsV, Var->min,
		     NULL_);
    if (!StatusHandlerStats(status)) return FALSE;
    status = CDFlib (GET_, ATTR_NUMBER_, "SCALEMAX", &attrN,
		     NULL_);
    if (status == NO_SUCH_ATTR) {
      status = CDFlib (CREATE_, ATTR_, "SCALEMAX", VARIABLE_SCOPE, &attrN,
		       NULL_);
      if (!StatusHandlerStats(status)) return FALSE;
    }
    else
      if (!StatusHandlerStats(status)) return FALSE;
    status = CDFlib (SELECT_, ATTR_, attrN,
			      BOO(Var->Z,zENTRY_,rENTRY_), Var->varN,
		     PUT_, BOO(Var->Z,zENTRY_DATA_,
				      rENTRY_DATA_), Var->dataTypeV,
						     Var->numElemsV, Var->max,
		     NULL_);
    if (!StatusHandlerStats(status)) return FALSE;
  }
  if (updateMonotonic && Var->monoInited) {
    char mono[MONOTON_RESULT_LEN+1];

    status = CDFlib (SELECT_, CDF_READONLY_MODE_, READONLYoff,
		     NULL_);
    if (!StatusHandlerStats(status)) return FALSE;
    status = CDFlib (GET_, ATTR_NUMBER_, "MONOTON", &attrN,
		     NULL_);
    if (status == NO_SUCH_ATTR) {
      status = CDFlib (CREATE_, ATTR_, "MONOTON", VARIABLE_SCOPE, &attrN,
		       NULL_);
      if (!StatusHandlerStats(status)) return FALSE;
    }
    else
      if (!StatusHandlerStats(status)) return FALSE;
    switch (Var->monoState) {
      case _Increase:
	strcpyX (mono, "INCREASE", MONOTON_RESULT_LEN);
	break;
      case _Decrease:
	strcpyX (mono, "DECREASE", MONOTON_RESULT_LEN);
	break;
      default:
	strcpyX (mono, "FALSE   ", MONOTON_RESULT_LEN);
    }
    status = CDFlib (SELECT_, ATTR_, attrN,
			      BOO(Var->Z,zENTRY_,rENTRY_), Var->varN,
		     PUT_, BOO(Var->Z,zENTRY_DATA_,
				      rENTRY_DATA_), CDF_CHAR, 8L, mono,
		     NULL_);
    if (!StatusHandlerStats(status)) return FALSE;
  }
  /****************************************************************************
  * Return - note that memory allocated will be freed by the caller (or the
  * caller's caller).
  ****************************************************************************/
  return TRUE;
}

/******************************************************************************
* MinMaxInit.
******************************************************************************/

void MinMaxInit (Var)
struct VarStruct *Var;
{
  Logical use;
  if (Var->ignoreFillsVar)
    if (NEx(Var->value,Var->fillval,Var->dataTypeV,Var->numElemsV))
      use = TRUE;
    else {
      use = FALSE;
      Var->fills++;
    }
  else
    use = TRUE;
  if (use) {
    ASSIGNx (Var->min, Var->value, Var->dataTypeV, Var->numElemsV);
    ASSIGNx (Var->max, Var->value, Var->dataTypeV, Var->numElemsV);
    if (Var->rangeCheckVar) {
      if (LTx(Var->value,Var->validmin,Var->dataTypeV,Var->numElemsV))
        Var->low++;
      else
        if (GTx(Var->value,Var->validmax,Var->dataTypeV,Var->numElemsV))
	  Var->high++;
        else {
	  Var->oneINrange = TRUE;
	  ASSIGNx (Var->minINrange, Var->value, Var->dataTypeV,
		   Var->numElemsV);
	  ASSIGNx (Var->maxINrange, Var->value, Var->dataTypeV,
		   Var->numElemsV);
        }
    }
    Var->minmaxInited = TRUE;
  }
  return;
}

void MinMaxCheck (Var)
struct VarStruct *Var;
{
  Logical use;
  if (Var->ignoreFillsVar)
    if (NEx(Var->value,Var->fillval,Var->dataTypeV,Var->numElemsV))
      use = TRUE;
    else {
      use = FALSE;
      Var->fills++;
    }
  else
    use = TRUE;
  if (use) {
    if (LTx(Var->value,Var->min,Var->dataTypeV,Var->numElemsV))
      ASSIGNx (Var->min, Var->value, Var->dataTypeV, Var->numElemsV);
    else {
      if (GTx(Var->value,Var->max,Var->dataTypeV,Var->numElemsV)) {
        ASSIGNx (Var->max, Var->value, Var->dataTypeV, Var->numElemsV);
      }
    }
    if (Var->rangeCheckVar) {
      if (LTx(Var->value,Var->validmin,Var->dataTypeV,Var->numElemsV))
        Var->low++;
      else
        if (GTx(Var->value,Var->validmax,Var->dataTypeV,Var->numElemsV))
	  Var->high++;
        else
	  if (Var->oneINrange) {
	    if (LTx(Var->value,Var->minINrange,Var->dataTypeV,Var->numElemsV))
	      ASSIGNx (Var->minINrange, Var->value, Var->dataTypeV,
		       Var->numElemsV);
	    else
	      if (GTx(Var->value,Var->maxINrange,
		      Var->dataTypeV,Var->numElemsV)) {
	        ASSIGNx (Var->maxINrange, Var->value, Var->dataTypeV,
		         Var->numElemsV);
	      }
	  }
	  else {
	    Var->oneINrange = TRUE;
	    ASSIGNx (Var->minINrange, Var->value, Var->dataTypeV,
		     Var->numElemsV);
	    ASSIGNx (Var->maxINrange, Var->value, Var->dataTypeV,
		     Var->numElemsV);
	  }
    }
  }
  return;
}

/******************************************************************************
* Monotonic.
******************************************************************************/

void Monotonic (Var)
struct VarStruct *Var;
{
  Logical use;

  if (Var->ignoreFillsVar)
    if (NEx(Var->value,Var->fillval,Var->dataTypeV,Var->numElemsV))
      use = TRUE;
    else
      use = FALSE;
  else
    use = TRUE;
  if (use) {
    if (!Var->monoInited) {
      ASSIGNx (Var->last, Var->value, Var->dataTypeV, Var->numElemsV);
      Var->monoState = _Init;
      Var->monoInited = TRUE;
    }
    else {
      switch (Var->monoState) {
        case _Init:
	  if (EQx(Var->value,Var->last,Var->dataTypeV,Var->numElemsV)) {
	    Var->monoState = _Steady;
	    break;
	  }
	  if (LTx(Var->value,Var->last,Var->dataTypeV,Var->numElemsV)) {
	    Var->monoState = _Decrease;
	    break;
	  }
	  if (GTx(Var->value,Var->last,Var->dataTypeV,Var->numElemsV)) {
	    Var->monoState = _Increase;
	    break;
	  }
	  break;
        case _Steady:
	  if (LTx(Var->value,Var->last,Var->dataTypeV,Var->numElemsV)) {
	    Var->monoState = _noIncrease;
	    break;
	  }
	  if (GTx(Var->value,Var->last,Var->dataTypeV,Var->numElemsV)) {
	    Var->monoState = _noDecrease;
	    break;
	  }
	  break;
        case _Increase:
	  if (LTx(Var->value,Var->last,Var->dataTypeV,Var->numElemsV)) {
	    Var->monoState = _False;
	    break;
	  }
	  if (EQx(Var->value,Var->last,Var->dataTypeV,Var->numElemsV)) {
	    Var->monoState = _noDecrease;
	    break;
	  }
	  break;
        case _Decrease:
	  if (GTx(Var->value,Var->last,Var->dataTypeV,Var->numElemsV)) {
	    Var->monoState = _False;
	    break;
	  }
	  if (EQx(Var->value,Var->last,Var->dataTypeV,Var->numElemsV)) {
	    Var->monoState = _noIncrease;
	    break;
	  }
	  break;
        case _noIncrease:
	  if (GTx(Var->value,Var->last,Var->dataTypeV,Var->numElemsV)) {
	    Var->monoState = _False;
	    break;
	  }
	  break;
        case _noDecrease:
	  if (LTx(Var->value,Var->last,Var->dataTypeV,Var->numElemsV)) {
	    Var->monoState = _False;
	    break;
	  }
	case _False:
	  /* Do nothing? */
	  break;
      }
      ASSIGNx (Var->last, Var->value, Var->dataTypeV, Var->numElemsV);
    }
  }
  return;
}

/******************************************************************************
* HandleVirtual.
******************************************************************************/

Logical HandleVirtual (Var)
struct VarStruct *Var;
{
  CDFstatus status;
  struct VarStruct VirtualVar;
  long numElemsE;                 /* number of elements for attribute entry */
  void *temp;

  /****************************************************************************
  * Initialize pointers for memory to be allocated.
  ****************************************************************************/
  VirtualVar.buffer = NULL;
  /****************************************************************************
  * Find out the real variable has the epoch time values. 
  ****************************************************************************/
  status = CDFlib (SELECT_, ATTR_NAME_, "COMPONENT_0",
			    BOO(Var->Z,zENTRY_,rENTRY_), Var->varN,
		   GET_, BOO(Var->Z,zENTRY_NUMELEMS_,
				    rENTRY_NUMELEMS_), &numElemsE,
		         BOO(Var->Z,zENTRY_DATA_,
				    rENTRY_DATA_), VirtualVar.varName,
		   NULL_);
  if (status == NO_SUCH_ENTRY) return TRUE;
  VirtualVar.varName[(int)numElemsE] = '\0';
  status = CDFlib (GET_, BOO(Var->Z,zVAR_NUMBER_,
				    rVAR_NUMBER_), VirtualVar.varName,
						   &VirtualVar.varN,
		   NULL_);
  if (status != CDF_OK) return FALSE;
  status = CDFlib (SELECT_, BOO(Var->Z,zVAR_,rVAR_), VirtualVar.varN,
		   GET_, BOO(Var->Z,zVAR_MAXREC_,
				    rVAR_MAXREC_), &VirtualVar.varMaxRec,
		   NULL_);
  if (status != CDF_OK) return FALSE;
  if (VirtualVar.varMaxRec <= 0) {
    status = CDFlib (SELECT_, ATTR_NAME_, "COMPONENT_1",
			      BOO(Var->Z,zENTRY_,rENTRY_), Var->varN,
		     GET_, BOO(Var->Z,zENTRY_NUMELEMS_,
				      rENTRY_NUMELEMS_), &numElemsE,
		           BOO(Var->Z,zENTRY_DATA_,
				      rENTRY_DATA_), VirtualVar.varName,
		     NULL_);
    if (status != CDF_OK) return FALSE;
    VirtualVar.varName[(int)numElemsE] = '\0';
    status = CDFlib (GET_, BOO(Var->Z,zVAR_NUMBER_,
				      rVAR_NUMBER_), VirtualVar.varName,
						     &VirtualVar.varN,
		     NULL_);
    if (status != CDF_OK) return FALSE;
    status = CDFlib (SELECT_, BOO(Var->Z,zVAR_,rVAR_), VirtualVar.varN,
		     GET_, BOO(Var->Z,zVAR_MAXREC_,
				      rVAR_MAXREC_), &VirtualVar.varMaxRec,
		     NULL_);
    if (status != CDF_OK) return FALSE;
  }
  VirtualVar.validmin = NULL;
  VirtualVar.validmax = NULL;
  VirtualVar.fillval = NULL;
  VirtualVar.format = NULL;
  VirtualVar.min = NULL;
  VirtualVar.max = NULL;
  VirtualVar.last = NULL;
  VirtualVar.minINrange = NULL;
  VirtualVar.maxINrange = NULL;
  VirtualVar.buffer = NULL;
  VirtualVar.Z = Var->Z;
  status = CDFlib (GET_, BOO(Var->Z,zVAR_NUMBER_,
				    rVAR_NUMBER_), VirtualVar.varName,
						   &VirtualVar.varN,
		   NULL_);
  if (status != CDF_OK) return FALSE;
  status = CDFlib (SELECT_, BOO(Var->Z,zVAR_,rVAR_), VirtualVar.varN,
		   GET_, BOO(Var->Z,zVAR_DATATYPE_,
				    rVAR_DATATYPE_), &VirtualVar.dataTypeV,
		         BOO(Var->Z,zVAR_NUMELEMS_,
				    rVAR_NUMELEMS_), &VirtualVar.numElemsV,
		         BOO(Var->Z,zVAR_NUMDIMS_,
				    rVARs_NUMDIMS_), &VirtualVar.numDims,
		         BOO(Var->Z,zVAR_DIMSIZES_,
				    rVARs_DIMSIZES_), VirtualVar.dimSizes,
		         BOO(Var->Z,zVAR_RECVARY_,
				    rVAR_RECVARY_), &VirtualVar.recVary,
		         BOO(Var->Z,zVAR_DIMVARYS_,
				    rVAR_DIMVARYS_), VirtualVar.dimVarys,
		         BOO(Var->Z,zVAR_MAXREC_,
				    rVAR_MAXREC_), &VirtualVar.varMaxRec,
		   NULL_);
  if (status != CDF_OK) return FALSE;
  /****************************************************************************
  * If fill values are to be ignored, get the fill value.
  ****************************************************************************/
  if (ignoreFills) {
    long dataTypeE, numElemsE;
    status = CDFlib (SELECT_, ATTR_NAME_, "FILLVAL",
                              BOO(Var->Z,zENTRY_,rENTRY_), VirtualVar.varN,
                     GET_, BOO(Var->Z,zENTRY_DATATYPE_,
                                      rENTRY_DATATYPE_), &dataTypeE,
                           BOO(Var->Z,zENTRY_NUMELEMS_,
                                      rENTRY_NUMELEMS_), &numElemsE,
                     NULL_);
    if (status == NO_SUCH_ENTRY)
      VirtualVar.ignoreFillsVar = FALSE;
    else {
      if (!StatusHandlerStats(status)) return FALSE;
      temp = cdf_AllocateMemory ((size_t) (CDFelemSize(dataTypeE) * numElemsE),
                             FatalError);
      status = CDFlib (GET_, BOO(VirtualVar.Z,zENTRY_DATA_,rENTRY_DATA_), temp,
                       NULL_);
      if (!StatusHandlerStats(status)) {
        cdf_FreeMemory (temp, FatalError);
        return FALSE;
      }
      VirtualVar.fillval = cdf_AllocateMemory ((size_t) (CDFelemSize(VirtualVar.dataTypeV) *
                                               VirtualVar.numElemsV),
                                     FatalError);
      ConvertDataType (dataTypeE, numElemsE, temp, VirtualVar.dataTypeV,
                       VirtualVar.numElemsV, VirtualVar.fillval);
      cdf_FreeMemory (temp, FatalError);
      VirtualVar.ignoreFillsVar = TRUE;
    }
  }
  else
    VirtualVar.ignoreFillsVar = FALSE;
  /****************************************************************************
  * Read each record and calculate statistics.
  ****************************************************************************/
  if (!CALCstat(&VirtualVar, TRUE)) return FALSE;
  FreeVarMem(&VirtualVar);
  /****************************************************************************
  * Return - note that memory allocated will be freed by caller.
  ****************************************************************************/
  return TRUE;
}

