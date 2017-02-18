/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                     CDF library miscellaneous functions, part 1.
*
*  Version 1.3e, 18-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  19-Dec-94, J Love     Original version.
*   V1.0a 29-Dec-94, J Love     WriteBuffer: increment buffer pointer in the
*                               case where the memory allocation failed.
*   V1.1  13-Jan-95, J Love     Encode/decode changes.  More cache-residency.
*                               Allow all possible extensions on all machines.
*   V1.1a 19-Jan-95, J Love     IRIX 6.x (64-bit).
*   V1.1b 24-Feb-95, J Love     Solaris 2.3 IDL i/f.
*   V1.2  21-Mar-95, J Love     POSIX.
*   V1.2a 18-Apr-95, J Love     More POSIX.  MEMLOG_.
*   V1.2b 19-Apr-95, J Love     Memory functions moved to `cdfmem.c'.
*   V1.2c  7-Sep-95, J Love     Corrected status codes being returned.  Try
*                               progressively smaller temporary buffers in
*                               `WriteVarElems'.
*   V1.3  10-Sep-96, J Love     CDF V2.6.
*   V1.3a 21-Feb-97, J Love	Removed RICE.
*   V1.3b 28-Feb-97, J Love	Windows NT for MS Visual C/C++ on an IBM PC.
*   V1.3c 11-Sep-97, J Love	Magic numbers are now uInt32.
*   V1.3d 20-Oct-97, J Love	Properly cast the uInt32 magic numbers.  More
*				Windows NT.
*   V1.3e 18-Nov-97, J Love	Even more Windows NT.
*   V2.0  08/Apr-04, M liu      Replaced VSTREAM.STATS with VSTREAM_STATS.
*   V2.1  29-Jun-04, M Liu      Added LFS (Large File Support > 2G).
*   V2.2  26-Oct-12, M Liu      Change CDF version/release/increment and
*                               copyright only when a newer version apps 
*                               is applying the modification.
*
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"
#include "cdfrev.h"

/******************************************************************************
* Max/MinLongLong.
******************************************************************************/

VISIBLE_PREFIX OFF_T MaxLongLong (a, b)
OFF_T a;
OFF_T b;
{
  return MAXIMUM64(a,b);
}

VISIBLE_PREFIX OFF_T MinLongLong (a, b)
OFF_T a; 
OFF_T b; 
{
  return MINIMUM64(a,b);
}

/******************************************************************************
* UpdateDotCDF64.
* If this routine is called when aborting a CDF, we cannot assume that
* the CDF structure is complete - it may have been only partially initialized
* when the CDF was aborted. If it is called to save the CDF without closing 
* it, the data will be properly preserved.
******************************************************************************/

VISIBLE_PREFIX CDFstatus UpdateDotCDF64 (CDF)
struct CDFstruct *CDF; 
{
  CDFstatus pStatus = CDF_OK; int varN; struct VarStruct *Var; Logical zVar;
  /**************************************************************************
  * Update r/zVariables depending on the variable type...
  **************************************************************************/
  for (zVar = 0; zVar <= 1; zVar++) {
    if (BOO(zVar,CDF->zVars,CDF->rVars) != NULL) {
      Int32 nVars = BOO(zVar,CDF->NzVars,CDF->NrVars);
      for (varN = 0; varN < nVars; varN++) {
         Var = BOO(zVar,CDF->zVars[varN],CDF->rVars[varN]);
         if (Var != NULL) {
           switch (Var->vType) {
             case SPARSE_RECORDS_: {
               if (!sX(FlushStage64(CDF,Var),&pStatus)) break;
               /* No `break' is intentional. */
             case STANDARD_:
               if (Var->maxWritten < Var->maxAllocated) {
                 Int32 padFrom = Var->maxWritten + 1;
                 if (!sX(PadUnRecords64(CDF,Var,padFrom,
                                        Var->maxAllocated),&pStatus)) break;
                 Var->maxWritten = Var->maxAllocated;
               }
               break;
             }
             case COMPRESSED_:
             case SPARSE_COMPRESSED_RECORDS_:
               if (!sX(FlushStage64(CDF,Var),&pStatus)) break;
               break;
             case SPARSE_ARRAYS_:
             case SPARSE_RECORDS_AND_ARRAYS_:
               sX (UNKNOWN_SPARSENESS, &pStatus);
               break;
             case IN_MULTI_:
               break;
             default:
               sX (CDF_INTERNAL_ERROR, &pStatus);
               break;
           }
         }
      }
    }
  }
  return pStatus;
}     
      
/******************************************************************************
* CloseVarFiles64.
*
* Close the open variable files of the specified CDF.  This routine closes all
* of the open variable files regardless of the number of errors detected.
*
* Because this routine is called when aborting a CDF, we cannot assume
* that the CDF structure is complete.  Eg., it may have been only partially
* initialized when the CDF was aborted.
******************************************************************************/

STATICforIDL CDFstatus CloseVarFiles64 (CDF)
struct CDFstruct *CDF;
{
  CDFstatus pStatus = CDF_OK; struct VarStruct *Var; int varN;
  /****************************************************************************
  * If a multi-file CDF, close the variable files.
  ****************************************************************************/
  if (!CDF->singleFile) {
    /**************************************************************************
    * Close rVariable files.  If the pointer to the rVariable is NULL, then
    * the rVariable has yet to be initialized (and is obviously closed).
    **************************************************************************/
    if (CDF->rVars != NULL) {
      for (varN = 0; varN < CDF->NrVars; varN++) {
         Var = CDF->rVars[varN];
         if (Var != NULL) {
           if (Var->fp != NULL) {
             if (!CLOSEv64(Var->fp,NULL,NULL)) sX (VAR_CLOSE_ERROR, &pStatus);
             Var->fp = NULL;
           }
         }
      }
    }
    /**************************************************************************
    * Close zVariable files.  If the pointer to the zVariable is NULL, then
    * the zVariable has yet to be initialized (and is obviously closed).
    **************************************************************************/
    if (CDF->zVars != NULL) {
      for (varN = 0; varN < CDF->NzVars; varN++) {
         Var = CDF->zVars[varN];
         if (Var != NULL) {
           if (Var->fp != NULL) {
             if (!CLOSEv64(Var->fp,NULL,NULL)) sX (VAR_CLOSE_ERROR, &pStatus);
             Var->fp = NULL;
           }
         }
      }
    }
  }
  return pStatus;
}

/******************************************************************************
* WriteAccess64.
* Close and then reopen a CDF for read/write access (it was opened with
* read-only access initially).  If the CDF is earlier than CDF V2.5, then
* some of the fields will have to be fixed and the CDR will be truncated for
* a shorter copyright length (unless the CDF is being deleted in which case
* it would be a waste of time to do these things).
******************************************************************************/

STATICforIDL Logical WriteAccess64 (CDF, forDelete, pStatus)
struct CDFstruct *CDF;
Logical forDelete;      /* Is the write access is needed to delete the CDF? */
CDFstatus *pStatus;     /* Returned status. */
{
#if BUILD_READ_ONLY_DISTRIBUTION
   *pStatus = READ_ONLY_DISTRIBUTION;
   return FALSE;
#else
   char pathName[DU_MAX_PATH_LEN+1]; vSTATS vStats;
   /***************************************************************************
   * Check if write access already.
   ***************************************************************************/
   if (CDF->status == READ_WRITE) return TRUE;
   /***************************************************************************
   * Check if this CDF is in read-only mode.
   ***************************************************************************/
   if (CDF->readOnly) {
     *pStatus = READ_ONLY_MODE;
     return FALSE;
   }
   /***************************************************************************
   * Close (the possibly compressed) dotCDF file and the variable files (if
   * a multi-file CDF).  An uncompressed dotCDF file and any scratch files
   * stay open.
   ***************************************************************************/
   if (!CLOSEv64(CDF->dotFp,NULL,&vStats)) {
     CDF->dotFp = NULL;
     AbortAccess64 (CDF, noUPDATE, noDELETE);
     return FALSE;
   }
   CDF->dotFp = NULL;
   AddTOvStats (&CDF->dotCDFvStats, &vStats);
#if defined(DEBUG)
   DisplayVs (getenv("VSTREAM_STATS"), "DotCDF..", &vStats);
#endif
   if (!sX(CloseVarFiles64(CDF),pStatus)) {
     AbortAccess64 (CDF, noUPDATE, noDELETE);
     return FALSE;
   }
   /***************************************************************************
   * Open dotCDF file with read-write access.  If read-write access not is
   * allowed, try to return to read-only access.  If reopening with read-only
   * access fails, free CDF structures as if CDF had been closed.
   ***************************************************************************/
   BuildFilePath (CDFt, CDF->CDFname, CDF->no_append, CDF->upper_case_ext,
                  CDF->version_numbers, 0L, pathName);
   CDF->dotFp = V_open64 (pathName, READ_PLUS_a_mode);
   if (CDF->dotFp == NULL) {
     CDF->dotFp = V_open64 (pathName, READ_ONLY_a_mode);
     if (CDF->dotFp == NULL) {
       AbortAccess64 (CDF, noUPDATE, noDELETE);
       *pStatus = CDF_OPEN_ERROR;
       return FALSE;
     }
     else {
       CDF->status = READ_ONLY;
       *pStatus = NO_WRITE_ACCESS;                      /* Don't return yet. */
     }
   }
   else
     CDF->status = READ_WRITE;
   /***************************************************************************
   * If the CDF is not compressed, reassign the "working" file pointer and
   * reset the cache size (unless deleting).  If the CDF is compressed, the
   * cache size of the "working" file pointer does not have to be reset.
   ***************************************************************************/
   if (CDF->uDotFp == NULL) {
     CDF->fp = CDF->dotFp;
     if (!forDelete) {
       if (!CACHEv64(CDF->fp,CDF->workingCacheSize)) {
         *pStatus = BAD_CACHE_SIZE;
         AbortAccess64 (CDF, noUPDATE, noDELETE);
         return FALSE;
       }
     }
   }
   /***************************************************************************
   * Fix various fields (if write access was obtained) unless write access
   * was needed to delete the CDF.
   ***************************************************************************/
   if (CDF->status == READ_WRITE && !forDelete) {
     Int32 versionNew = CDF_LIBRARY_VERSION,
           releaseNew = CDF_LIBRARY_RELEASE,
           incrementNew = CDF_LIBRARY_INCREMENT;
     Int32 versionCur, releaseCur, incrementCur;
     uInt32 magicNumber1 = V3magicNUMBER_1,
            magicNumber2 = V3magicNUMBER_2u;
     char copyRight[CDF_COPYRIGHT_LEN+1];
     /*************************************************************************
     * Update magic numbers.
     *************************************************************************/
     if (!SEEKv64(CDF->fp,(OFF_T)V3_MAGIC_OFFSET_1,vSEEK_SET)) {
       AbortAccess64 (CDF, noUPDATE, noDELETE);
       *pStatus = CDF_WRITE_ERROR;
       return FALSE;
     }
     if (!Write32_64(CDF->fp,(Int32 *)&magicNumber1)) {
       AbortAccess64 (CDF, noUPDATE, noDELETE);
       *pStatus = CDF_WRITE_ERROR;
       return FALSE;
     }
     if (!Write32_64(CDF->fp,(Int32 *)&magicNumber2)) {
       AbortAccess64 (CDF, noUPDATE, noDELETE);
       *pStatus = CDF_WRITE_ERROR;
       return FALSE;
     }
     /*************************************************************************
     * If a V2.0 CDF, correct the EOF field.
     *************************************************************************/
     if (CDF->badEOF) {
       if (!sX(CorrectV20eof(CDF),pStatus)) {
         AbortAccess64 (CDF, noUPDATE, noDELETE);
         return FALSE;
       }
       CDF->badEOF = FALSE;
     }
     /*************************************************************************
     * If a V2.0 CDF, correct the terminating offset fields.  NOTE: Fix these
     * fields before the other "fixing" routines (which may depend on these
     * fields).
     *************************************************************************/
     if (CDF->badTerminatingOffsets) {
       if (!sX(CorrectV20offsets(CDF),pStatus)) {
         AbortAccess64 (CDF, noUPDATE, noDELETE);
         return FALSE;
       }
       CDF->badTerminatingOffsets = FALSE;
     }
     /*************************************************************************
     * If prior to CDF V2.1.1, then change the data type associated with the
     * "EPOCH" rVariable/rEntries to CDF_EPOCH.
     *************************************************************************/
     if (CDF->fakeEPOCH) {
       if (!sX(CorrectEPOCH(CDF),pStatus)) {
         AbortAccess64 (CDF, noUPDATE, noDELETE);
         return FALSE;
       }
       CDF->fakeEPOCH = FALSE;
     }
     /*************************************************************************
     * If prior to CDF V2.5, then truncate the CDR for a shorter copyright
     * field and shorten each VDR to reclaim the wasted space.
     *************************************************************************/
     if (CDF->wastedSpace) {
       if (!sX(ShortenCDR(CDF),pStatus)) {
         AbortAccess64 (CDF, noUPDATE, noDELETE);
         return FALSE;
       }
       if (!sX(ShortenVDRs(CDF),pStatus)) {
         AbortAccess64 (CDF, noUPDATE, noDELETE);
         return FALSE;
       }
       CDF->wastedSpace = FALSE;
     }
     /*************************************************************************
     * If prior to CDF V2.5, then convert all assumed scopes to definite
     * scopes.
     *************************************************************************/
     if (CDF->assumedScopes) {
       if (!sX(CorrectScopes(CDF),pStatus)) {
         AbortAccess64 (CDF, noUPDATE, noDELETE);
         return FALSE;
       }
       CDF->assumedScopes = FALSE;
     }
     /*************************************************************************
     * Fix blocking factors for variables having a recVary of NOVARY.
     *************************************************************************/
     if (!sX(CorrectBlockingFactors64(CDF),pStatus)) {
       AbortAccess64 (CDF, noUPDATE, noDELETE);
       return FALSE;
     }
     if (isLFS(CDF)) {
       /*********************************************************************
       * Update version/release/increment and copywright only if a newer
       * library version is modifying the CDF.
       *********************************************************************/
       if (!sX(ReadCDR64(CDF->fp,V3_CDR_OFFSET64,
                         CDR_VERSION,&versionCur,
                         CDR_RELEASE,&releaseCur,
                         CDR_INCREMENT,&incrementCur,
                         CDR_NULL),pStatus)) {
         AbortAccess64 (CDF, noUPDATE, noDELETE);
         return FALSE;
       }
       if ((versionCur * 10000 + releaseCur * 100 + incrementCur) < 
           (versionNew * 10000 + releaseNew * 100 + incrementNew)) {
         if (!sX(WriteCDR64(CDF->fp,V3_CDR_OFFSET64,
                            CDR_VERSION,&versionNew,
                            CDR_RELEASE,&releaseNew,
                            CDR_INCREMENT,&incrementNew,
                            CDR_NULL),pStatus)) {
           AbortAccess64 (CDF, noUPDATE, noDELETE);
           return FALSE;
         }
         CDFcopyRight (copyRight);
         NulPad (copyRight, CDF_COPYRIGHT_LEN);
         if (!sX(WriteCDR64(CDF->fp,V3_CDR_OFFSET64,
                            CDR_COPYRIGHT,copyRight,
                            CDR_NULL),pStatus)) {
           AbortAccess64 (CDF, noUPDATE, noDELETE);
           return FALSE;
         }
       }
     }
   }
   /***************************************************************************
   * Return based on whether or not write access was obtained.
   ***************************************************************************/
   return (CDF->status == READ_WRITE);
#endif
}

/******************************************************************************
* UpdateTT2000header.
* The leap second last updated header is to be updated from the leap second
* table if it is an earlier day (not zero).
******************************************************************************/

STATICforIDL Logical UpdateTT2000header (CDF, pStatus)
struct CDFstruct *CDF;
CDFstatus *pStatus;
{
   long year, month, day;
   Int32 tLastUpdated, tLastUpdated2;
   /***************************************************************************
   * Check if updated already.
   ***************************************************************************/
   if (CDF->tt2000Updated == 1) return TRUE;
   if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
                     GDR_LEAPSECONDLASTUPDATED,&tLastUpdated,
                     GDR_NULL),pStatus)) {
     AbortAccess64 (CDF, noUPDATE, noDELETE);
     return FALSE;
   }
   if (tLastUpdated != 0) {
#if defined(vms)
     CDFgetLastDateinLeapSecondsTBL (&year, &month, &day);
#else
     CDFgetLastDateinLeapSecondsTable (&year, &month, &day);
#endif
     tLastUpdated2 = 10000*(Int32)year + 100*(Int32)month + (Int32)day;
     if (tLastUpdated2 > tLastUpdated) { 
       if (!sX(WriteGDR64(CDF->fp,CDF->GDRoffset64,
                          GDR_LEAPSECONDLASTUPDATED,&tLastUpdated2,
                          GDR_NULL),pStatus)) {
         AbortAccess64 (CDF, noUPDATE, noDELETE);
         return FALSE;
       }
     }
   }
   CDF->tt2000Updated = 1;
   return TRUE;
}

/******************************************************************************
* WriteBuffer64.
*    Write occurs at current offset (assumed to have been set before this
* routine is called).  On IBM PCs, it is assumed that `nBytes' will not
* exceed 65535.
******************************************************************************/

STATICforIDL CDFstatus WriteBuffer64 (CDF, fp, dataType, numElems, buffer)
struct CDFstruct *CDF;
vFILE *fp;
Int32 dataType;
Int32 numElems;
void *buffer;
{
  CDFstatus pStatus = CDF_OK;
  size_t nElemBytes = (size_t) CDFelemSize(dataType);
  size_t nBytes = (size_t) (numElems * nElemBytes);
  double eValue; Int32 elemN; Byte1 *ptr; void *tBuffer;
  /****************************************************************************
  * Try to encode/write entire buffer.
  ****************************************************************************/
  tBuffer = cdf_AllocateMemory (nBytes, NULL);
  if (tBuffer != NULL) {
    if (!sX(ConvertBuffer(HostEncoding(),CDF->encoding,CDF->negToPosFp0,
                          dataType,numElems,buffer,tBuffer),&pStatus)) {
      cdf_FreeMemory (tBuffer, NULL);
      return pStatus;
    }
    if (!WRITEv64(tBuffer,1,nBytes,fp)) {
      cdf_FreeMemory (tBuffer, NULL);
      return CDF_WRITE_ERROR;
    }
    cdf_FreeMemory (tBuffer, NULL);
    return pStatus;
  }
  /****************************************************************************
  * If that failed, encode/write one element at a time.
  ****************************************************************************/
  for (elemN = 0, ptr = buffer; elemN < numElems; elemN++, ptr += nElemBytes) {
     if (!sX(ConvertBuffer(HostEncoding(),CDF->encoding,
                           CDF->negToPosFp0,dataType,1L,ptr,
                           &eValue),&pStatus)) return pStatus;
     if (!WRITEv64(&eValue,1,nElemBytes,fp)) return CDF_WRITE_ERROR;
  }
  return pStatus;
}

/******************************************************************************
* UpdateMaxRec64.
******************************************************************************/

STATICforIDL CDFstatus UpdateMaxRec64 (CDF, Var, recNum)
struct CDFstruct *CDF;          /* In: Pointer to CDF. */
struct VarStruct *Var;          /* In: Pointer to variable. */
Int32 recNum;                   /* In: Possible new maximum record number. */
{
  CDFstatus pStatus = CDF_OK;
  if (recNum > Var->maxRec) {
    Var->maxRec = recNum;
    if (!sX(WriteVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
                       VDR_MAXREC,&recNum,
                       VDR_NULL),&pStatus)) return pStatus;
  }
  if (!Var->zVar) {
    if (recNum > CDF->rMaxRec) {
      CDF->rMaxRec = recNum;
      if (!sX(WriteGDR64(CDF->fp,CDF->GDRoffset64,
                         GDR_rMAXREC,&recNum,
                         GDR_NULL),&pStatus)) return pStatus;
    }
  }
  return pStatus;
}

/******************************************************************************
* CalcDimParms64.
*    Calculates a variable's number of dimensions, dimension sizes, and
* dimension variances depending on the current zMode.
******************************************************************************/

STATICforIDL CDFstatus CalcDimParms64 (CDF, offset, zVar, numDimsP, dimSizesP,
				     dimVarysP)
struct CDFstruct *CDF;          /* In: Pointer to CDF. */
OFF_T offset;                   /* In: Offset of VDR. */
Logical zVar;                   /* In: TRUE if a true zVariable.  FALSE if a
				       true rVariable. */
Int32 *numDimsP;		/* Out: Number of dimensions. */
Int32 dimSizesP[];              /* Out: Dimension sizes. */
Int32 dimVarysP[];		/* Out: Dimension variances. */
{
  CDFstatus pStatus = CDF_OK; int dN;
  Int32 tNumDims, tDimSizes[CDF_MAX_DIMS], tDimVarys[CDF_MAX_DIMS];
  Int32 numDims, dimSizes[CDF_MAX_DIMS], dimVarys[CDF_MAX_DIMS];
  /****************************************************************************
  * Determine `true' parameters.
  ****************************************************************************/
  if (zVar) {
    if (!sX(ReadVDR64(CDF,CDF->fp,offset,TRUE,
		      VDR_zNUMDIMS,&tNumDims,
	  	      VDR_zDIMSIZES,tDimSizes,
		      VDR_DIMVARYS,tDimVarys,
		      VDR_NULL),&pStatus)) return pStatus;
  }
  else {
    if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
		      GDR_rNUMDIMS,&tNumDims,
		      GDR_rDIMSIZES,tDimSizes,
		      GDR_NULL),&pStatus)) {
         return pStatus;
    }
    if (!sX(ReadVDR64(CDF,CDF->fp,offset,FALSE,
		      VDR_DIMVARYS,tDimVarys,
		      VDR_NULL),&pStatus)) return pStatus;
  }
  /****************************************************************************
  * Determine parameters based on zMode and if r/zVariable.
  ****************************************************************************/
  if (CDF->zMode == zMODEon2 && !zVar) {
    for (dN = 0, numDims = 0; dN < tNumDims; dN++) {
       if (tDimVarys[dN]) {
	 dimSizes[(int)numDims] = tDimSizes[dN];
	 dimVarys[(int)numDims] = VARY;
	 numDims++;
       }
    }
  }
  else {
    numDims = tNumDims;
    for (dN = 0; dN < tNumDims; dN++) {
       dimSizes[dN] = tDimSizes[dN];
       dimVarys[dN] = tDimVarys[dN];
    }
  }
  /****************************************************************************
  * Assign those values requested.
  ****************************************************************************/
  ASSIGNnotNULL (numDimsP, numDims)
  ASSIGNnotNULLarray (dimSizesP, numDims, dimSizes)
  ASSIGNnotNULLarray (dimVarysP, numDims, dimVarys)
  return pStatus;
}

/******************************************************************************
* WasteIR64.
******************************************************************************/

STATICforIDL CDFstatus WasteIR64 (CDF, wasteOffset, size)
struct CDFstruct *CDF;
OFF_T wasteOffset;
OFF_T size;
{
  CDFstatus pStatus = CDF_OK;
  struct UIRstruct64 newUIR, firstUIR, tUIR, nextUIR;
  OFF_T tOffset, nextOffset, UIRhead;
  /****************************************************************************
  * Begin initializing UIR.
  ****************************************************************************/
  newUIR.RecordSize = size;
  newUIR.RecordType = UIR_;
  /****************************************************************************
  * Check that the internal record being wasted is big enough for the `next'
  * and `previous' fields.  If not, mark it as wasted but don't place it in
  * the linked list of UIRs.  Note that there will always be enough room for
  * the `size' and `type' fields.  If not, an internal logic error has occured.
  ****************************************************************************/
  if (size < UIR_BASE_SIZE64) {
    if (size < UUIR_BASE_SIZE64) return CDF_INTERNAL_ERROR;
    if (!sX(WriteUIR64(CDF->fp,wasteOffset,
	  	       UIR_RECORDSIZE,&(newUIR.RecordSize),
		       UIR_RECORDTYPE,&(newUIR.RecordType),
		       UIR_NULL),&pStatus)) return pStatus;
    return pStatus;
  }
  /****************************************************************************
  * Read offset of first UIR.
  ****************************************************************************/
  if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
		    GDR_UIRHEAD,&UIRhead,
		    GDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * Check if no UIRs exist yet.
  ****************************************************************************/
  if (UIRhead == 0) {
    newUIR.NextUIR = 0;
    newUIR.PrevUIR = 0;
    if (!sX(WriteUIR64(CDF->fp,wasteOffset,
		       UIR_RECORD,&newUIR,
		       UIR_NULL),&pStatus)) return pStatus;
    UIRhead = wasteOffset;
    if (!sX(WriteGDR64(CDF->fp,CDF->GDRoffset64,
		       GDR_UIRHEAD,&UIRhead,
		       GDR_NULL),&pStatus)) return pStatus;
    return pStatus;
  }
  /****************************************************************************
  * At least one UIR exists, check if the new UIR is before the first UIR.
  ****************************************************************************/
  if (wasteOffset < UIRhead) {
    if (!sX(ReadUIR64(CDF->fp,UIRhead,
		      UIR_RECORD,&firstUIR,
		      UIR_NULL),&pStatus)) return pStatus;
    newUIR.NextUIR = UIRhead;
    newUIR.PrevUIR = 0;
    if (!sX(WriteUIR64(CDF->fp,wasteOffset,
		       UIR_RECORD,&newUIR,
		       UIR_NULL),&pStatus)) return pStatus;
    firstUIR.PrevUIR = wasteOffset;
    if (!sX(WriteUIR64(CDF->fp,UIRhead,
		       UIR_RECORD,&firstUIR,
		       UIR_NULL),&pStatus)) return pStatus;
    UIRhead = wasteOffset;
    if (!sX(WriteGDR64(CDF->fp,CDF->GDRoffset64,
		       GDR_UIRHEAD,&UIRhead,
		       GDR_NULL),&pStatus)) return pStatus;
    return pStatus;
  }
  /****************************************************************************
  * The new UIR is not before the first UIR.  Scan the UIRs to find the point
  * at which it should be inserted.
  ****************************************************************************/
  tOffset = UIRhead;
  if (!sX(ReadUIR64(CDF->fp,tOffset,
		    UIR_RECORD,&tUIR,
		    UIR_NULL),&pStatus)) return pStatus;
  while (tUIR.NextUIR != 0) {
    if (wasteOffset < tUIR.NextUIR) {
      nextOffset = tUIR.NextUIR;
      if (!sX(ReadUIR64(CDF->fp,nextOffset,
		        UIR_RECORD,&nextUIR,
		        UIR_NULL),&pStatus)) return pStatus;
      newUIR.NextUIR = tUIR.NextUIR;
      newUIR.PrevUIR = tOffset;
      if (!sX(WriteUIR64(CDF->fp,wasteOffset,
		         UIR_RECORD,&newUIR,
		         UIR_NULL),&pStatus)) return pStatus;
      tUIR.NextUIR = wasteOffset;
      if (!sX(WriteUIR64(CDF->fp,tOffset,
		         UIR_RECORD,&tUIR,
		         UIR_NULL),&pStatus)) return pStatus;
      nextUIR.PrevUIR = wasteOffset;
      if (!sX(WriteUIR64(CDF->fp,nextOffset,
		         UIR_RECORD,&nextUIR,
		         UIR_NULL),&pStatus)) return pStatus;
      return pStatus;
    }
    tOffset = tUIR.NextUIR;
    if (!sX(ReadUIR64(CDF->fp,tOffset,
		      UIR_RECORD,&tUIR,
		      UIR_NULL),&pStatus)) return pStatus;
  }
  /****************************************************************************
  * The new UIR is going to be the last UIR.
  ****************************************************************************/
  newUIR.NextUIR = 0;
  newUIR.PrevUIR = tOffset;
  if (!sX(WriteUIR64(CDF->fp,wasteOffset,
		     UIR_RECORD,&newUIR,
		     UIR_NULL),&pStatus)) return pStatus;
  tUIR.NextUIR = wasteOffset;
  if (!sX(WriteUIR64(CDF->fp,tOffset,
		     UIR_RECORD,&tUIR,
		     UIR_NULL),&pStatus)) return pStatus;
  return pStatus;
}

/******************************************************************************
* AllocateIR64.
******************************************************************************/

STATICforIDL CDFstatus AllocateIR64 (CDF, size, offset)
struct CDFstruct *CDF;          /* In: Pointer to CDF. */
OFF_T size;                     /* In: Size of internal record (bytes). */
OFF_T *offset;                  /* Out: Offset of allocated internal record. */
{
  CDFstatus pStatus = CDF_OK;
  Int32 uir_ = UIR_;
  OFF_T tSize, sOffset, eOffset, UIRhead, eof;
  struct UIRstruct64 sUIR, eUIR;
  /****************************************************************************
  * Read EOF and offset of first UIR from GDR.
  ****************************************************************************/
  if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
		    GDR_UIRHEAD,&UIRhead,
		    GDR_EOF,&eof,
		    GDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * If UIRs exist, try to use one or more of them (if contiguous) for the new
  * internal record.
  ****************************************************************************/
  if (UIRhead != 0) {
    sOffset = UIRhead;
    if (!sX(ReadUIR64(CDF->fp,sOffset,
		      UIR_RECORD,&sUIR,
		      UIR_NULL),&pStatus)) return pStatus;
    eOffset = sOffset;
    eUIR = sUIR;
    tSize = sUIR.RecordSize;
    for (;;) {
       /***********************************************************************
       * Check if the starting to ending UIRs are the exact size needed.
       ***********************************************************************/
       if (size == tSize) {
	 if (!sX(RemoveUIRs64(CDF,sOffset,eOffset),&pStatus)) return pStatus;
	 if (!sX(WriteIrSize64(CDF->fp,sOffset,&size),&pStatus)) return pStatus;
	 if (!sX(WriteIrType64(CDF->fp,sOffset,&uir_),&pStatus)) return pStatus;
	 *offset = sOffset;
	 return pStatus;
       }
       /***********************************************************************
       * Check if the starting to ending UIRs are big enough for the new
       * internal record and for a new UIR to fill the remaining space.
       ***********************************************************************/
       if (size + UIR_BASE_SIZE64 <= tSize) {
	 if (!sX(RemoveUIRs64(CDF,sOffset,eOffset),&pStatus)) return pStatus;
	 if (!sX(WasteIR64(CDF,sOffset+size,tSize-size),&pStatus)) {
	   return pStatus;
	 }
	 if (!sX(WriteIrSize64(CDF->fp,sOffset,&size),&pStatus)) return pStatus;
	 if (!sX(WriteIrType64(CDF->fp,sOffset,&uir_),&pStatus)) return pStatus;
	 *offset = sOffset;
	 return pStatus;
       }
       /***********************************************************************
       * Check if the end of the UIRs has been reached.  If so, check if the
       * ending UIR is the last IR in the dotCDF file.
       ***********************************************************************/
       if (eUIR.NextUIR == 0) {
	 if (eOffset + (OFF_T) eUIR.RecordSize == eof) {
	   /*******************************************************************
	   * The ending UIR is the last internal record in the CDF.  Check to
	   * see if after allocating the new internal record less than
	   * UIR_BASE_SIZE64 bytes will remain before the EOF.  If so, waste an
	   * internal record at the location of those bytes so that a UIR is
	   * at the end (rather than stranded bytes).
	   *******************************************************************/
	   if (!sX(RemoveUIRs64(CDF,sOffset,eOffset),&pStatus)) return pStatus;
	   if (size < tSize) {
	     if (!sX(WasteIR64(CDF,sOffset+size,
			       UIR_BASE_SIZE64),&pStatus)) return pStatus;
	     eof = sOffset + (OFF_T) (size + UIR_BASE_SIZE64);
	   }
	   else
	     eof = sOffset + (OFF_T) size;
	   if (!sX(WriteGDR64(CDF->fp,CDF->GDRoffset64,
			      GDR_EOF,&eof,
			      GDR_NULL),&pStatus)) return pStatus;
	   if (!sX(WriteIrSize64(CDF->fp,
			         sOffset,
			         &size),&pStatus)) return pStatus;
	   if (!sX(WriteIrType64(CDF->fp,
			         sOffset,
			         &uir_),&pStatus)) return pStatus;
	   *offset = sOffset;
	   return pStatus;
	 }
	 else {
	   /*******************************************************************
	   * Non-UIRs follow the ending UIR.  The new internal record will
	   * have to be allocated at the EOF.
	   *******************************************************************/
	   *offset = eof;
	   if (!sX(WriteIrSize64(CDF->fp,eof,&size),&pStatus)) return pStatus;
	   if (!sX(WriteIrType64(CDF->fp,eof,&uir_),&pStatus)) return pStatus;
	   eof += (OFF_T) size;
	   if (!sX(WriteGDR64(CDF->fp,CDF->GDRoffset64,
			      GDR_EOF,&eof,
			      GDR_NULL),&pStatus)) return pStatus;
	   return pStatus;
	 }
       }
       /***********************************************************************
       * If the next UIR is contiguous with the ending UIR, make it the ending
       * UIR.  Otherwise, make the next UIR the starting and ending UIRs.
       ***********************************************************************/
       if (eOffset + eUIR.RecordSize == eUIR.NextUIR) {
	 eOffset = eUIR.NextUIR;
	 if (!sX(ReadUIR64(CDF->fp,eOffset,
			   UIR_RECORD,&eUIR,
			   UIR_NULL),&pStatus)) return pStatus;
	 tSize += eUIR.RecordSize;
       }
       else {
	 sOffset = eUIR.NextUIR;
	 if (!sX(ReadUIR64(CDF->fp,sOffset,
			   UIR_RECORD,&sUIR,
			   UIR_NULL),&pStatus)) return pStatus;
	 eOffset = sOffset;
	 eUIR = sUIR;
	 tSize = sUIR.RecordSize;
       }
    }
  }
  /****************************************************************************
  * No UIRs exist.  The new internal record will have to be allocated at the
  * EOF.
  ****************************************************************************/
  *offset = eof;
  if (!sX(WriteIrSize64(CDF->fp,eof,&size),&pStatus)) return pStatus;
  if (!sX(WriteIrType64(CDF->fp,eof,&uir_),&pStatus)) return pStatus;
  eof += size;
  if (!sX(WriteGDR64(CDF->fp,CDF->GDRoffset64,
		     GDR_EOF,&eof,
		     GDR_NULL),&pStatus)) return pStatus;
  return pStatus;
}

/******************************************************************************
* ResizeIR64.
******************************************************************************/

STATICforIDL CDFstatus ResizeIR64 (CDF, curOffset, newSize, newOffset, move,
				 success)
struct CDFstruct *CDF;
OFF_T curOffset;        /* In: Current offset of internal record. */
OFF_T newSize;          /* In: New size of internal record.  This may be
			   smaller or larger than the current size. */
OFF_T *newOffset;       /* Out: New offset of internal record.  This variable
			   is not modified if an error occurs or the internal
			   record cannot be extended (when `move' is FALSE). */
Logical move;           /* In: TRUE if the internal record can be moved if
			   necessary. */
Logical *success;       /* Out: TRUE if the internal record was successfully
			   extended (whether or not it had to be moved). */
{
  CDFstatus pStatus = CDF_OK; OFF_T curSize; 
  OFF_T eof;
  /****************************************************************************
  * Determine current size of internal record.
  ****************************************************************************/
  if (!sX(ReadIrSize64(CDF->fp,curOffset,&curSize),&pStatus)) return pStatus;
  /****************************************************************************
  * Check sizes...
  ****************************************************************************/
  if (newSize > curSize) {
    /**************************************************************************
    * The internal record is growing.  First check if it is the last one in
    * the CDF.
    **************************************************************************/
    if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
		      GDR_EOF,&eof,
		      GDR_NULL),&pStatus)) return pStatus;
    if (curOffset + (OFF_T) curSize == eof) {
      /************************************************************************
      * Last internal record.  Simply extend the CDF.
      ************************************************************************/
      ASSIGNnotNULL (newOffset, curOffset)
      eof += (OFF_T) (newSize - curSize);
      if (!sX(WriteGDR64(CDF->fp,CDF->GDRoffset64,
		         GDR_EOF,&eof,
		         GDR_NULL),&pStatus)) return pStatus;
      if (!sX(WriteIrSize64(CDF->fp,
			    curOffset,
			    &newSize),&pStatus)) return pStatus;
      ASSIGNnotNULL (success, TRUE)
      return pStatus;
    }
    else {
      /************************************************************************
      * Not the last internal record.  If the internal record may be moved,
      * first mark it as unused and then allocate a new internal record.
      * Marking it unused first allows the possibility that if will be used
      * as part of the allocated internal record.  If the internal record can
      * not be moved, check if unused records immediately follow.
      ************************************************************************/
      if (move) {
	if (!sX(WasteIR64(CDF,curOffset,curSize),&pStatus)) return pStatus;
	if (!sX(AllocateIR64(CDF,newSize,newOffset),&pStatus)) return pStatus;
	ASSIGNnotNULL (success, TRUE)
	return pStatus;
      }
      else {
	Int32 irType;
	OFF_T sOffset, eOffset, tSize, UIRhead;
	struct UIRstruct64 sUIR, eUIR;
	/**********************************************************************
	* First check if there are any UIRs in the CDF.  This is done because
	* CDF V2.5.0* (alpha/beta) created UIRs without the next and previous
	* UIR fields and didn't use the `UIRhead' field in the GDR.  Because
	* we don't want to use UIRs in those CDFs (because they are not the
	* same as the current UIRs), this will keep us from doing so (because
	* the `UIRhead' fields will always contain zero if a V2.5.0* CDF).
	**********************************************************************/
	if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
			  GDR_UIRHEAD,&UIRhead,
			  GDR_NULL),&pStatus)) return pStatus;
	if (UIRhead == 0) {
	  ASSIGNnotNULL (success, FALSE)
	  return pStatus;
	}
	/**********************************************************************
	* Read the internal record which immediately follows the internal
	* record being resized.  If it is a UIR make it the starting UIR.
	* ---------------------------- DANGER ---------------------------------
	* Don't try to read an entire UIR.  First read only the record type
	* field and check if it is UIR_.  Then read the entire UIR.  This is
	* because the next internal record could be smaller than a UIR (or
	* larger but not entirely written yet [eg. a VVR]).
	**********************************************************************/
	sOffset = curOffset + curSize;
	if (!sX(ReadIrType64(CDF->fp,sOffset,&irType),&pStatus)) return pStatus;
	if (irType != UIR_) {
	  ASSIGNnotNULL (success, FALSE)
	  return pStatus;
	}
	if (!sX(ReadUIR64(CDF->fp,sOffset,
			  UIR_RECORD,&sUIR,
			  UIR_NULL),&pStatus)) return pStatus;
	tSize = curSize + sUIR.RecordSize;
	eOffset = sOffset;
	eUIR = sUIR;
	for (;;) {
	   /*******************************************************************
	   * Check if the exact amount of available space has been found.
	   *******************************************************************/
	   if (newSize == tSize) {
	     if (!sX(RemoveUIRs64(CDF,sOffset,eOffset),&pStatus)) return pStatus;
	     if (!sX(WriteIrSize64(CDF->fp,
				   curOffset,
				   &newSize),&pStatus)) return pStatus;
	     ASSIGNnotNULL (newOffset, curOffset)
	     ASSIGNnotNULL (success, TRUE)
	     return pStatus;
	   }
	   /*******************************************************************
	   * Check if enough available space has been found to increase the
	   * internal record and then create a new UIR in the remaining space.
	   *******************************************************************/
	   if (newSize + UIR_BASE_SIZE64 <= tSize) {
	     if (!sX(RemoveUIRs64(CDF,sOffset,eOffset),&pStatus)) return pStatus;
	     if (!sX(WasteIR64(CDF,curOffset+newSize,tSize-newSize),&pStatus)) {
	       return pStatus;
	     }
	     if (!sX(WriteIrSize64(CDF->fp,
				   curOffset,
				   &newSize),&pStatus)) return pStatus;
	     ASSIGNnotNULL (newOffset, curOffset)
	     ASSIGNnotNULL (success, TRUE)
	     return pStatus;
	   }
	   /*******************************************************************
	   * Check if the end of the UIRs has been reached.
	   *******************************************************************/
	   if (eUIR.NextUIR == 0) {
	     /*****************************************************************
	     * If the ending UIR is at the EOF, then the internal record can
	     * be extended beyond the EOF or up to it with the creation of a
	     * new UIR at the very end.
	     *****************************************************************/
	     if (eOffset + (OFF_T) eUIR.RecordSize == eof) {
	       if (!sX(RemoveUIRs64(CDF,sOffset,eOffset),&pStatus)) return
								  pStatus;
	       if (newSize < tSize) {
		 if (!sX(WasteIR64(CDF,curOffset+newSize,
				   UIR_BASE_SIZE64),&pStatus)) return pStatus;
		 eof = curOffset + (OFF_T) (newSize + UIR_BASE_SIZE64);
	       }
	       else
		 eof = curOffset + (OFF_T) newSize;
	       if (!sX(WriteGDR64(CDF->fp,CDF->GDRoffset64,
				  GDR_EOF,&eof,
				  GDR_NULL),&pStatus)) return pStatus;
	       if (!sX(WriteIrSize64(CDF->fp,
				     curOffset,
				     &newSize),&pStatus)) return pStatus;
	       ASSIGNnotNULL (newOffset, curOffset)
	       ASSIGNnotNULL (success, TRUE)
	       return pStatus;
	     }
	     else {
	       ASSIGNnotNULL (success, FALSE)
	       return pStatus;
	     }
	   }
	   /*******************************************************************
	   * If the next UIR is contiguous with the ending UIR, make it the
	   * ending UIR.
	   *******************************************************************/
	   if (eOffset + eUIR.RecordSize == eUIR.NextUIR) {
	     eOffset = eUIR.NextUIR;
	     if (!sX(ReadUIR64(CDF->fp,eOffset,
			       UIR_RECORD,&eUIR,
			       UIR_NULL),&pStatus)) return pStatus;
	     tSize += eUIR.RecordSize;
	   }
	   else {
	     ASSIGNnotNULL (success, FALSE)
	     return pStatus;
	   }
	}
      }
    }
  }
  else {
    /**************************************************************************
    * The internal record is shrinking.  Check if it can be shrunk in place
    * and a UIR created to occupy the extra space.  If not, waste it and then
    * allocate a new internal record (if moving it is allowed).
    **************************************************************************/
    if (newSize <= (curSize - UIR_BASE_SIZE64)) {
      if (!sX(WasteIR64(CDF,curOffset + newSize,
		        curSize - newSize),&pStatus)) return pStatus;
      if (!sX(WriteIrSize64(CDF->fp,
			    curOffset,
			    &newSize),&pStatus)) return pStatus;
      ASSIGNnotNULL (newOffset, curOffset)
      ASSIGNnotNULL (success, TRUE)
    }
    else {
      if (move) {
	if (!sX(WasteIR64(CDF,curOffset,curSize),&pStatus)) return pStatus;
	if (!sX(AllocateIR64(CDF,newSize,newOffset),&pStatus)) return pStatus;
	ASSIGNnotNULL (success, TRUE)
      }
      else {
	ASSIGNnotNULL (success, FALSE)
      }      
    }
    return pStatus;
  }
}

/******************************************************************************
* RemoveUIRs64.
******************************************************************************/

STATICforIDL CDFstatus RemoveUIRs64 (CDF, sOffset, eOffset)
struct CDFstruct *CDF;          /* In: Pointer to CDF. */
OFF_T sOffset;                  /* In: Offset of starting UIR. */
OFF_T eOffset;                  /* In: Offset of ending UIR. */
{
  CDFstatus pStatus = CDF_OK;
  struct UIRstruct64 sUIR, eUIR;
  OFF_T UIRhead;
  if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
		    GDR_UIRHEAD,&UIRhead,
		    GDR_NULL),&pStatus)) return pStatus;
  if (!sX(ReadUIR64(CDF->fp,sOffset,
		    UIR_RECORD,&sUIR,
		    UIR_NULL),&pStatus)) return pStatus;
  if (!sX(ReadUIR64(CDF->fp,eOffset,
		    UIR_RECORD,&eUIR,
		    UIR_NULL),&pStatus)) return pStatus;
  if (UIRhead == sOffset) {
    UIRhead = eUIR.NextUIR;
    if (!sX(WriteGDR64(CDF->fp,CDF->GDRoffset64,
		       GDR_UIRHEAD,&UIRhead,
		       GDR_NULL),&pStatus)) return pStatus;
  }
  else {
    struct UIRstruct64 prevUIR;
    if (!sX(ReadUIR64(CDF->fp,sUIR.PrevUIR,
		      UIR_RECORD,&prevUIR,
		      UIR_NULL),&pStatus)) return pStatus;
    prevUIR.NextUIR = eUIR.NextUIR;
    if (!sX(WriteUIR64(CDF->fp,sUIR.PrevUIR,
		       UIR_RECORD,&prevUIR,
		       UIR_NULL),&pStatus)) return pStatus;
  }
  if (eUIR.NextUIR != 0) {
    struct UIRstruct64 nextUIR;
    if (!sX(ReadUIR64(CDF->fp,eUIR.NextUIR,
		      UIR_RECORD,&nextUIR,
		      UIR_NULL),&pStatus)) return pStatus;
    nextUIR.PrevUIR = sUIR.PrevUIR;
    if (!sX(WriteUIR64(CDF->fp,eUIR.NextUIR,
		       UIR_RECORD,&nextUIR,
		       UIR_NULL),&pStatus)) return pStatus;
  }
  return pStatus;
}

/******************************************************************************
* CorrectBlockingFactors64.
*    It is assumed that the last rVDR offset has already been fixed.
******************************************************************************/

STATICforIDL CDFstatus CorrectBlockingFactors64 (CDF)
struct CDFstruct *CDF;
{
  CDFstatus pStatus = CDF_OK; Int32 nVars; OFF_T vdrOffset; int varN;
  Logical zVar; struct VDRstruct64 VDR; struct VarStruct **Vars, *Var;
  for (zVar = 0; zVar <= 1; zVar++) {
     Vars = BOO(zVar,CDF->zVars,CDF->rVars);
     nVars = BOO(zVar,CDF->NzVars,CDF->NrVars);
     for (varN = 0; varN < nVars; varN++) {
        Var = Vars[varN];
        if (Var == NULL) {
          if (!sX(FindVarByNumber64(CDF,(Int32)varN,
                                    zVar,&vdrOffset),&pStatus)) return pStatus;
        }
        else
          vdrOffset = Var->VDRoffset64;
        if (!sX(ReadVDR64(CDF,CDF->fp,vdrOffset,zVar,
                          VDR_RECORD,&VDR,NULL,
                          VDR_NULL),&pStatus)) return pStatus;
        if (!RECvaryBITset(VDR.Flags) && VDR.blockingFactor > 1) {
          VDR.blockingFactor = 1;
          if (!sX(WriteVDR64(CDF,CDF->fp,vdrOffset,zVar,
                             VDR_RECORD,&VDR,NULL,
                             VDR_NULL),&pStatus)) return pStatus;
          if (Var != NULL) {
            if (!sX(CalcBF64(CDF,Var),&pStatus)) return pStatus;
          }
        }
     }
  }
  return pStatus;
}

/******************************************************************************
* AbortAccess64.
******************************************************************************/

STATICforIDL void AbortAccess64 (CDF, updateCDF, deleteCDF)
struct CDFstruct *CDF;
Logical updateCDF;      /* Update "working" dotCDF file (if read/write
                           access has been obtained)? */
Logical deleteCDF;      /* Delete CDF file(s)? */
{
  /****************************************************************************
  * If the CDF is to be deleted do that first and then skip the updating and/or
  * closing of the CDF files.
  ****************************************************************************/
  if (deleteCDF) {
    DeleteCDFfiles64 (CDF);
    if (CDF->uDotFp != NULL) V_delete64 (CDF->uDotFp, NULL);
  }
  else {
    /**************************************************************************
    * Update the dotCDF file if requested, if the current access is read/write,
    * and if the "working" dotCDF file has not already been closed or deleted.
    **************************************************************************/
    if (CDF->status == READ_WRITE && updateCDF &&
        (CDF->fp == CDF->dotFp || CDF->fp == CDF->uDotFp)) UpdateDotCDF64 (CDF);
    /**************************************************************************
    * Close all of the CDF files if they are not already closed (or deleted).
    **************************************************************************/
    if (CDF->dotFp != NULL) V_close64 (CDF->dotFp,
				       ((updateCDF==UPDATE)?CDF:NULL), NULL);
    if (CDF->uDotFp != NULL) V_close64 (CDF->uDotFp,
				        ((updateCDF==UPDATE)?CDF:NULL), NULL);
    CloseVarFiles64 (CDF);
  }               
  /****************************************************************************
  * Delete the scratch files that still exist.
  ****************************************************************************/
  if (CDF->stage.fp != NULL) V_delete64 (CDF->stage.fp, NULL);
  if (CDF->compressFp != NULL) V_delete64 (CDF->compressFp, NULL);
  /****************************************************************************
  * Free the memory used by the CDF.
  ****************************************************************************/
  FreeCDFid (CDF, TRUE);
  return;
}

/******************************************************************************
* DeleteCDFfiles64.
* The files may be open or closed.  This routine does not delete the
* uncompressed dotCDF file (if one exists).
******************************************************************************/

STATICforIDL CDFstatus DeleteCDFfiles64 (CDF)
struct CDFstruct *CDF;
{
  char tmpFile[DU_MAX_PATH_LEN+1]; CDFstatus pStatus = CDF_OK;
  /**************************************************************************
  * Delete dotCDF file.
  **************************************************************************/
  if (CDF->dotFp == NULL) {
    BuildFilePath (CDFt, CDF->CDFname, CDF->no_append, CDF->upper_case_ext,
                   CDF->version_numbers, INT32_ZERO, tmpFile);
    if (!CDFdeleteFile(tmpFile)) sX (CDF_DELETE_ERROR, &pStatus);
  }
  else {
    if (!DELETEv64(CDF->dotFp,NULL)) sX (CDF_DELETE_ERROR, &pStatus);
    CDF->dotFp = NULL;
  }
  /**************************************************************************
  * Delete the variable files (if multi-file).  Both rVariable and zVariable
  * files are deleted.
  **************************************************************************/
  if (!CDF->singleFile) {
    int varN;
    for (varN = 0; varN < CDF->NrVars; varN++) {
       struct VarStruct *Var = CDF->rVars[varN];
       if (Var != NULL) {
         if (Var->fp != NULL) {
           if (!DELETEv64(Var->fp,NULL)) sX (VAR_DELETE_ERROR, &pStatus);
           Var->fp = NULL;
           continue;
         }
       }
       BuildFilePath (Vt, CDF->CDFname, CDF->no_append, CDF->upper_case_ext,
                      CDF->version_numbers, varN, tmpFile);
       if (!CDFdeleteFile(tmpFile)) sX (VAR_DELETE_ERROR, &pStatus);
    }
    for (varN = 0; varN < CDF->NzVars; varN++) {
       struct VarStruct *Var = CDF->zVars[varN];
       if (Var != NULL) {
         if (Var->fp != NULL) {
           if (!DELETEv64(Var->fp,NULL)) sX (VAR_DELETE_ERROR, &pStatus);
           Var->fp = NULL;
           continue;
         }
       }
       BuildFilePath (Zt, CDF->CDFname, CDF->no_append, CDF->upper_case_ext,
                      CDF->version_numbers, varN, tmpFile);
       if (!CDFdeleteFile(tmpFile)) sX (VAR_DELETE_ERROR, &pStatus);
    }
  }
  return pStatus;
}

/******************************************************************************
* DeleteEntry64.
******************************************************************************/

STATICforIDL CDFstatus DeleteEntry64 (CDF, aOffset, eOffset)
struct CDFstruct *CDF;
OFF_T aOffset;
OFF_T eOffset;
{
  CDFstatus pStatus = CDF_OK;
  struct ADRstruct64 ADR;
  struct AEDRstruct64 AEDR, AEDRt;
  OFF_T prevEntryOffset;
  Logical zEntry;
  /****************************************************************************
  * Read the ADR and the AEDR being deleted.
  ****************************************************************************/
  if (!sX(ReadADR64(CDF->fp,aOffset,
                    ADR_RECORD,&ADR,
                    ADR_NULL),&pStatus)) return pStatus;
  if (!sX(ReadAEDR64(CDF->fp,eOffset,
                     AEDR_RECORD,&AEDR,NULL,
                     AEDR_NULL),&pStatus)) return pStatus;
  zEntry = (AEDR.RecordType == AzEDR_);
  /****************************************************************************
  * Remove the AEDR from the list of entries.
  ****************************************************************************/
  if (!sX(FindPrevEntry64(CDF,aOffset,eOffset,
                          zEntry,&prevEntryOffset),&pStatus)) return pStatus;
  if (prevEntryOffset == 0) {
    /**************************************************************************
    * The first entry on the linked list is being deleted.  Point the ADR to
    * the entry being pointed to by the entry being deleted.
    **************************************************************************/
    if (zEntry)
      ADR.AzEDRhead = AEDR.AEDRnext;
    else
      ADR.AgrEDRhead = AEDR.AEDRnext;
  }
  else {
    /**************************************************************************
    * The entry being deleted is not the first entry on the linked list.  Point
    * the previous entry to the entry pointed to by the entry being deleted.
    **************************************************************************/
    if (!sX(ReadAEDR64(CDF->fp,prevEntryOffset,
                       AEDR_RECORD,&AEDRt,NULL,
                       AEDR_NULL),&pStatus)) return pStatus;
    AEDRt.AEDRnext = AEDR.AEDRnext;
    if (!sX(WriteAEDR64(CDF,CDF->fp,prevEntryOffset,
                        AEDR_RECORD,&AEDRt,NULL,
                        AEDR_NULL),&pStatus)) return pStatus;
  }
  /****************************************************************************
  * Decrement the number of entries and recalculate the maximum entry (if
  * necessary).
  ****************************************************************************/
  if (zEntry)
    ADR.NzEntries--;
  else
    ADR.NgrEntries--;
  if (BOO(zEntry,ADR.MAXzEntry,ADR.MAXgrEntry) == AEDR.Num) {
    Int32 maxEntry = NO_ENTRY;
    OFF_T tOffset = BOO(zEntry,ADR.AzEDRhead,ADR.AgrEDRhead);
    while (tOffset != 0) {
      if (!sX(ReadAEDR64(CDF->fp,tOffset,
                         AEDR_RECORD,&AEDRt,NULL,
                         AEDR_NULL),&pStatus)) return pStatus;
      maxEntry = MaxInt32 (maxEntry, AEDRt.Num);
      tOffset = AEDRt.AEDRnext;
    }
    if (zEntry)
      ADR.MAXzEntry = maxEntry;
    else
      ADR.MAXgrEntry = maxEntry;
  }
  /****************************************************************************
  * Rewrite the ADR and waste the AEDR (of the entry being deleted).
  ****************************************************************************/
  if (!sX(WriteADR64(CDF->fp,aOffset,
                     ADR_RECORD,&ADR,
                     ADR_NULL),&pStatus)) return pStatus; 
  if (!sX(WasteIR64(CDF,eOffset,AEDR.RecordSize),&pStatus)) return pStatus;
  return pStatus;
}                    
                     
/******************************************************************************
* PadUnRecords64.
*   Pad all allocated records within the first/last record range.  Note that
* in the case of sparse records some of the records may not be allocated (and
* should not be padded of course).
******************************************************************************/
                   
STATICforIDL CDFstatus PadUnRecords64 (CDF, Var, firstRec, lastRec)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 firstRec;
Int32 lastRec;
{
  CDFstatus pStatus = CDF_OK; OFF_T offset; int how; void *buffer;
  Int32 recNum, nRecords, toRec, lastRecInVVR; Logical found;
  /****************************************************************************
  * Pad the records.
  * NOTE: Padding in a single-file CDF could be made more efficient by not
  * allocating/freeing the pad buffer over and over...
  ****************************************************************************/
  if (CDF->singleFile) {
    /**************************************************************************
    * Single-file...first determine the first allocated record to be padded.
    **************************************************************************/
    if (!sX(NextRecord64(CDF,Var->VDRoffset64,Var->zVar,
                         firstRec,&recNum,&found),&pStatus)) return pStatus;
    if (!found) return pStatus;
    /**************************************************************************
    * While the first record to be padded is within the range...
    **************************************************************************/
    while (recNum <= lastRec) {
     if (!sX(RecordByteOffset64(CDF,Var,recNum,
                                &offset),&pStatus)) return pStatus;
     if (!sX(SearchForRecord64(CDF,Var->VDRoffset64,
                               Var->zVar,recNum,
                               NULL,&lastRecInVVR,
                               NULL,NULL),&pStatus)) return pStatus;
     toRec = MINIMUM(lastRec,lastRecInVVR);
     nRecords = toRec - recNum + 1;
     if (!sX(BuildPadBuffer64(CDF,Var,nRecords,
                              &how,&buffer,TRUE),&pStatus)) return pStatus;
     if (!sX(WritePadValues64(Var,CDF->fp,offset,
                              nRecords,how,buffer),&pStatus)) {
       cdf_FreeMemory (buffer, NULL);
       return pStatus;
     }
     cdf_FreeMemory (buffer, NULL);
     recNum += nRecords;
     /************************************************************************
     * Determine the next `first' record to be padded.
     ************************************************************************/
     if (!sX(NextRecord64(CDF,Var->VDRoffset64,Var->zVar,
                          recNum,&recNum,&found),&pStatus)) return pStatus;
     if (!found) return pStatus;
    }
  }
  else {
    /**************************************************************************
    * Multi-file...
    **************************************************************************/
    if (!sX(RecordByteOffset64(CDF,Var,
                               firstRec,
                               &offset),&pStatus)) return pStatus;
    nRecords = lastRec - firstRec + 1;
    if (!sX(BuildPadBuffer64(CDF,Var,nRecords,
                             &how,&buffer,TRUE),&pStatus)) return pStatus;
    if (!sX(WritePadValues64(Var,Var->fp,offset,nRecords,how,buffer),&pStatus)) {
      cdf_FreeMemory (buffer, NULL);
      return pStatus;
    }
    cdf_FreeMemory (buffer, NULL);
  }
  return pStatus;
}

/******************************************************************************
* WritePadValues64.
*   NOTE: It is assumed that the records are contiguous (in a single-file CDF)
* and that on a DOS machine the buffer does not exceed 64K bytes.  It is also
* assumed that the values have already been encoded for the CDF.
******************************************************************************/

STATICforIDL CDFstatus WritePadValues64 (Var, fp, offset, nRecords, how, buffer)
struct VarStruct *Var;
vFILE *fp;
OFF_T offset;
Int32 nRecords;
int how;
void *buffer;
{
  Int32 nBytes, nValues, recX, valueX;
  /****************************************************************************
  * Seek to desired offset.
  ****************************************************************************/
  if (!SEEKv64(fp,offset,vSEEK_SET)) return VAR_WRITE_ERROR;
  /****************************************************************************
  * Write records...
  ****************************************************************************/
  switch (how) {
    case ALLrecordsATonce:
      nBytes = nRecords * Var->NphyRecBytes;
      if (!WRITEv64(buffer,(size_t)nBytes,1,fp)) return VAR_WRITE_ERROR;
      break;
    case ONErecordATaTIME:
      for (recX = 0; recX < nRecords; recX++) {
	 if (!WRITEv64(buffer,(size_t)Var->NphyRecBytes,1,fp)) {
	   return VAR_WRITE_ERROR;
	 }
      }
      break;
    case ONEvalueATaTIME:
      nValues = nRecords * Var->NphyRecValues;
      for (valueX = 0; valueX < nValues; valueX++) {
	 if (!WRITEv64(buffer,(size_t)Var->NvalueBytes,1,fp)) {
	   return VAR_WRITE_ERROR;
	 }
      }
      break;
  }
  return CDF_OK;
}

/******************************************************************************
* SetLeapSecondLastUpdate.
* Set the last updated date for CDF that has TT2000 variable.
******************************************************************************/

STATICforIDL CDFstatus SetLeapSecondLastUpdate (CDF, Var, zVar)
struct CDFstruct *CDF;
struct VarStruct *Var;      /* the variable */
Logical zVar;
{
  Int32 dataType, LeapSecondLastUpdated, version, release, increment;
  long year, month, day;
   CDFstatus pStatus = CDF_OK;
  if (CDF->leapSecondUpdated == 1) return CDF_OK;
  if (!sX(ReadVDR64(CDF,CDF->fp,Var->VDRoffset64,zVar,
                    VDR_DATATYPE,&dataType,
                    VDR_NULL),&pStatus)) {
    AbortAccess64 (CDF, noUPDATE, noDELETE);
    return pStatus;
  }
  if (dataType != CDF_TIME_TT2000) return CDF_OK;
  if (!sX(ReadCDR64(CDF->fp,V3_CDR_OFFSET64,
                    CDR_VERSION,&version,
                    CDR_RELEASE,&release,
                    CDR_INCREMENT,&increment,
                    CDR_NULL),&pStatus)) {
    AbortAccess64 (CDF, noUPDATE, noDELETE);
    return pStatus;
  }
  /*********************************************************************
  * Update version/release/increment and copywright only if a newer
  * library version is modifying the CDF.
  *********************************************************************/
  if (!PriorTo("3.6.0",version,release,increment)) {
#if defined(vms)
    CDFgetLastDateinLeapSecondsTBL (&year, &month, &day);
#else
    CDFgetLastDateinLeapSecondsTable (&year, &month, &day);
#endif
    LeapSecondLastUpdated = (Int32) (10000*year + 100*month + day);
    if (!sX(WriteGDR64(CDF->fp,CDF->GDRoffset64,
                       GDR_LEAPSECONDLASTUPDATED,&LeapSecondLastUpdated,
                       GDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, noUPDATE, CDFDELETE);
      cdf_FreeMemory (CDF, NULL);
      return pStatus;
    }
    CDF->leapSecondUpdated = 1;
  }
  return CDF_OK;
}
