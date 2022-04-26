struct VarStruct {
  Logical Z;
  long varN;
  char varName[CDF_VAR_NAME_LEN256+1];
  long dataTypeV;
  long numElemsV;
  long numDims;
  long dimSizes[CDF_MAX_DIMS];
  long recVary;
  long dimVarys[CDF_MAX_DIMS];
  long varMaxRec;
  Byte1 *value;
  Byte1 *buffer;
  Logical hyper;
  long nValueBytes;
};
 size_t nValueBytes[1], nValueBytes2[1];
 long numElms, dataType;
 
    /**************************************************************************
    * Check if this a virtual variable. If so, sets it.
    **************************************************************************/
    virtual = 0;
    status = CDFlib (SELECT_, ATTR_NAME_, "VIRTUAL",
                              BOO(Var->Z,zENTRY_,rENTRY_), varN,
                     GET_, BOO(Var->Z,zENTRY_NUMELEMS_,
                                      rENTRY_NUMELEMS_), &numElms,
                     NULL_);
    if (status == CDF_OK) virtual = 1;
    if (virtual == 1) {
      status = CDFlib (SELECT_, ATTR_NAME_, "COMPONENT_1",
                                BOO(Var->Z,zENTRY_,rENTRY_), varN,
                       GET_, BOO(Var->Z,zENTRY_NUMELEMS_,
                                        rENTRY_NUMELEMS_), &numElms,
                             BOO(Var->Z,zENTRY_DATATYPE_,
                                        rENTRY_DATATYPE_), &dataType,
                             BOO(Var->Z,zENTRY_DATA_,
                                        rENTRY_DATA_), Var.varName,
                       NULL_);
      if (status != CDF_OK) return FALSE;
  Var.varName[numElms] = '\0';
  status = CDFlib (GET_, BOO(Var->Z,zVAR_NUMBER_,
                                    rVAR_NUMBER_), Var.varName,
                                                   &Var.varN,
                   NULL_);
  if (status != CDF_OK) return FALSE;
  status = CDFlib (SELECT_, BOO(Var->Z,zVAR_,rVAR_), Var.varN,
                   GET_, BOO(Var->Z,zVAR_DATATYPE_,
                                    rVAR_DATATYPE_), &Var.dataTypeV,
                         BOO(Var->Z,zVAR_NUMELEMS_,
                                    rVAR_NUMELEMS_), &Var.numElemsV,
                         BOO(Var->Z,zVAR_NUMDIMS_,
                                    rVARs_NUMDIMS_), &Var.numDims,
                         BOO(Var->Z,zVAR_DIMSIZES_,
                                    rVARs_DIMSIZES_), Var.dimSizes,
                         BOO(Var->Z,zVAR_RECVARY_,
                                    rVAR_RECVARY_), &Var.recVary,
                         BOO(Var->Z,zVAR_DIMVARYS_,
                                    rVAR_DIMVARYS_), Var.dimVarys,
                         BOO(Var->Z,zVAR_MAXREC_,
                                    rVAR_MAXREC_), &Var.varMaxRec,
                   NULL_);
  if (status != CDF_OK) return FALSE;
  Var->nValueBytes = Var->numElemsV * CDFelemSize(Var->dataTypeV);
  nValueBytes[0] = (size_t) Var->nValueBytes;
  Var->nValueBytes = Var->numElemsV * CDFelemSize(Var->dataTypeV);
  nValueBytes2[0] = (size_t) Var->nValueBytes;

  /****************************************************************************
  * Allocate memory.
  ****************************************************************************/
  handles[0] = &(Var->buffer);
  nValueBytes[0] = (size_t) Var->nValueBytes;
  AllocateBuffers (Var->varMaxRec + 1, Var->numDims, dimSizes, &groups, 0, 1,
                   handles, nValueBytes, ROWmajor(majority), MINnHYPERS,
                   FatalError);
  AllocateBuffers (Var->varMaxRec + 1, Var->numDims, dimSizes, &groups, 0, 1,
                   handles, nValueBytes2, ROWmajor(majority), MINnHYPERS,
                   FatalError);
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
                               BOO(Var->Z,
                                   zVAR_RECINTERVAL_,
                                   rVARs_RECINTERVAL_), hyper.recInterval,
                               BOO(Var->Z,zVAR_DIMINDICES_,
                                          rVARs_DIMINDICES_), hyper.dimIndices,
                               BOO(Var->Z,zVAR_DIMCOUNTS_,
                                          rVARs_DIMCOUNTS_), hyper.dimCounts,
                               BOO(Var->Z,
                                   zVAR_DIMINTERVALS_,
                                           rVARs_DIMINTERVALS_), hyper.dimIntervals,
                      GET_, BOO(Var->Z,zVAR_HYPERDATA_,
                                       rVAR_HYPERDATA_), Var->buffer,
                      NULL_);
     if (!StatusHandlerStats(status)) return FALSE;
     for (valueN = 0, Var->value = Var->buffer, Var2->value = Var2->buffer;
          valueN < nValues;
          valueN++, Var->value += (size_t) Var->nValueBytes,
          Var2->value += (size_t) Var2->nValueBytes) {
        *(Var2->value) = (xxx) (*(Var->value) / xxx);
     }
     status = CDFlib (SELECT_, BOO(Var->Z,zVAR__,
                                          rVAR_), Var2.varN,
                      PUT_, BOO(Var->Z,zVAR_HYPERDATA_,
                                       rVAR_HYPERDATA_), Var2->buffer,
                      NULL_);
     if (!StatusHandlerStats(status)) return FALSE;
     IncrHyperParms (&hyper, &groups, Var->numDims, ROWmajor(majority),
                     &nValues);
     CHECKforABORTso
  }

