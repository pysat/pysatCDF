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
*   V2.1  13-Oct-06, M Liu      Changed to allow upper and lower case CDF  
*                               name to be used on win32.
*   V2.2  12-Aug-10, M Liu      Ensured the filled file path name not overrun
*                               the defined string in BuildFilePath.
*   V2.3  22-Aug-10, M Liu      Modified NulPad to use memset to speed up the
*                               padding process.
*
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"
#include "cdfrev.h"

/******************************************************************************
* CorrectV20eof.
******************************************************************************/

STATICforIDL CDFstatus CorrectV20eof (CDF)
struct CDFstruct *CDF;
{
  CDFstatus pStatus = CDF_OK;
  Int32 eof = 0, size, vOffset, aOffset, eOffset, nAttrs, nEntries;
  int varX, attrX, entryX;
  /****************************************************************************
  * Check if CDR is last internal record.
  ****************************************************************************/
  if (!sX(ReadCDR(CDF->fp,CDF->CDRoffset,
		  CDR_RECORDSIZE,&size,
		  CDR_NULL),&pStatus)) return pStatus;
  eof = MaxInt32 (eof, CDF->CDRoffset + size);
  /****************************************************************************
  * Check if GDR is last internal record.
  ****************************************************************************/
  if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		  GDR_RECORDSIZE,&size,
		  GDR_NULL),&pStatus)) return pStatus;
  eof = MaxInt32 (eof, CDF->GDRoffset + size);
  /****************************************************************************
  * Scan through rVDRs checking if each is the last internal record.  Note
  * that V2.0 CDFs won't have zVDRs, VXRs, or VVRs.
  ****************************************************************************/
  if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		  GDR_rVDRHEAD,&vOffset,
		  GDR_NULL),&pStatus)) return pStatus;
  for (varX = 0; varX < CDF->NrVars; varX++) {
     if (!sX(ReadVDR(CDF,CDF->fp,vOffset,FALSE,
		     VDR_RECORDSIZE,&size,
		     VDR_NULL),&pStatus)) return pStatus;
     eof = MaxInt32 (eof, vOffset + size);
     if (!sX(ReadVDR(CDF,CDF->fp,vOffset,FALSE,
		     VDR_VDRNEXT,&vOffset,
		     VDR_NULL),&pStatus)) return pStatus;
  }
  /****************************************************************************
  * Scan through the ADRs checking if each is the last internal record.
  ****************************************************************************/
  if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		  GDR_NUMATTR,&nAttrs,
		  GDR_ADRHEAD,&aOffset,
		  GDR_NULL),&pStatus)) return pStatus;
  for (attrX = 0; attrX < nAttrs; attrX++) {
     if (!sX(ReadADR(CDF->fp,aOffset,
		     ADR_RECORDSIZE,&size,
		     ADR_NULL),&pStatus)) return pStatus;
     eof = MaxInt32 (eof, aOffset + size);
     /*************************************************************************
     * Scan through the ArEDRs checking if each is the last internal record.
     * Note that V2.0 CDFs won't have AzEDRs.
     *************************************************************************/
     if (!sX(ReadADR(CDF->fp,aOffset,
		     ADR_AgrEDRHEAD,&eOffset,
		     ADR_NgrENTRIES,&nEntries,
		     ADR_NULL),&pStatus)) return pStatus;
     for (entryX = 0; entryX < nEntries; entryX++) {
	if (!sX(ReadAEDR(CDF->fp,eOffset,
			 AEDR_RECORDSIZE,&size,
			 AEDR_NULL),&pStatus)) return pStatus;
	eof = MaxInt32 (eof, eOffset + size);
	if (!sX(ReadAEDR(CDF->fp,eOffset,
			 AEDR_AEDRNEXT,&eOffset,
			 AEDR_NULL),&pStatus)) return pStatus;
     }
     if (!sX(ReadADR(CDF->fp,aOffset,
		     ADR_ADRNEXT,&aOffset,
		     ADR_NULL),&pStatus)) return pStatus;
  }
  /****************************************************************************
  * Save correct EOF and return.
  ****************************************************************************/
  if (!sX(WriteGDR(CDF->fp,CDF->GDRoffset,
		   GDR_EOF,&eof,
		   GDR_NULL),&pStatus)) return pStatus;
  return pStatus;
}

/******************************************************************************
* CorrectV20offsets.
******************************************************************************/

STATICforIDL CDFstatus CorrectV20offsets (CDF)
struct CDFstruct *CDF;
{
  CDFstatus pStatus = CDF_OK;
  Int32 zero = 0, size, vOffset, aOffset, eOffset, nAttrs, nEntries;
  int varX, attrX, entryX;
  /****************************************************************************
  * Scan through rVDRs fixing the next VDR field of the last one (setting it
  * to an offset of zero).  Note that V2.0 CDFs won't have zVDRs, VXRs, or
  * VVRs.
  ****************************************************************************/
  if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		  GDR_rVDRHEAD,&vOffset,
		  GDR_NULL),&pStatus)) return pStatus;
  for (varX = 0; varX < CDF->NrVars; varX++) {
     if (!sX(ReadVDR(CDF,CDF->fp,vOffset,FALSE,
		     VDR_RECORDSIZE,&size,
		     VDR_NULL),&pStatus)) return pStatus;
     if (varX == CDF->NrVars - 1) {
       if (!sX(WriteVDR(CDF,CDF->fp,vOffset,FALSE,
			VDR_VDRNEXT,&zero,
			VDR_NULL),&pStatus)) return pStatus;
     }
     else {
       if (!sX(ReadVDR(CDF,CDF->fp,vOffset,FALSE,
		       VDR_VDRNEXT,&vOffset,
		       VDR_NULL),&pStatus)) return pStatus;
     }
  }
  /****************************************************************************
  * Scan through the ADRs fixing the next ADR field of the last one (setting
  * it to an offset of zero).
  ****************************************************************************/
  if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		  GDR_NUMATTR,&nAttrs,
		  GDR_ADRHEAD,&aOffset,
		  GDR_NULL),&pStatus)) return pStatus;
  for (attrX = 0; attrX < nAttrs; attrX++) {
     if (!sX(ReadADR(CDF->fp,aOffset,
		     ADR_RECORDSIZE,&size,
		     ADR_NULL),&pStatus)) return pStatus;
     /*************************************************************************
     * Scan through the ArEDRs fixing the next ArEDR field of the last one
     * (setting it to an offset of zero).  Note that V2.0 CDFs won't have
     * AzEDRs.
     *************************************************************************/
     if (!sX(ReadADR(CDF->fp,aOffset,
		     ADR_AgrEDRHEAD,&eOffset,
		     ADR_NgrENTRIES,&nEntries,
		     ADR_NULL),&pStatus)) return pStatus;
     for (entryX = 0; entryX < nEntries; entryX++) {
	if (!sX(ReadAEDR(CDF->fp,eOffset,
			 AEDR_RECORDSIZE,&size,
			 AEDR_NULL),&pStatus)) return pStatus;
	if (entryX == nEntries - 1) {
	  if (!sX(WriteAEDR(CDF,CDF->fp,eOffset,
			    AEDR_AEDRNEXT,&zero,
			    AEDR_NULL),&pStatus)) return pStatus;
	}
	else {
	  if (!sX(ReadAEDR(CDF->fp,eOffset,
			   AEDR_AEDRNEXT,&eOffset,
			   AEDR_NULL),&pStatus)) return pStatus;
	}
     }
     if (attrX == nAttrs - 1) {
       if (!sX(WriteADR(CDF->fp,aOffset,
			ADR_ADRNEXT,&zero,
			ADR_NULL),&pStatus)) return pStatus;
     }
     else {
       if (!sX(ReadADR(CDF->fp,aOffset,
		       ADR_ADRNEXT,&aOffset,
		       ADR_NULL),&pStatus)) return pStatus;
     }
  }
  return pStatus;
}

/******************************************************************************
* UpdateDotCDF.
* If this routine is called when aborting a CDF, we cannot assume that
* the CDF structure is complete - it may have been only partially initialized
* when the CDF was aborted. If it is called to save the CDF without closing 
* it, the data will be properly preserved.
******************************************************************************/

VISIBLE_PREFIX CDFstatus UpdateDotCDF (CDF)
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
	       if (!sX(FlushStage(CDF,Var),&pStatus)) break;
	       /* No `break' is intentional. */
	     case STANDARD_:
	       if (Var->maxWritten < Var->maxAllocated) {
		 Int32 padFrom = Var->maxWritten + 1;
		 if (!sX(PadUnRecords(CDF,Var,padFrom,
				      Var->maxAllocated),&pStatus)) break;
		 Var->maxWritten = Var->maxAllocated;
	       }
	       break;
	     }
	     case COMPRESSED_:
	     case SPARSE_COMPRESSED_RECORDS_:
	       if (!sX(FlushStage(CDF,Var),&pStatus)) break;
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
* CloseVarFiles.
*
* Close the open variable files of the specified CDF.  This routine closes all
* of the open variable files regardless of the number of errors detected.
*
* Because this routine is called when aborting a CDF, we cannot assume
* that the CDF structure is complete.  Eg., it may have been only partially
* initialized when the CDF was aborted.
******************************************************************************/

STATICforIDL CDFstatus CloseVarFiles (CDF)
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
	     if (!CLOSEv(Var->fp,NULL,NULL)) sX (VAR_CLOSE_ERROR, &pStatus);
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
	     if (!CLOSEv(Var->fp,NULL,NULL)) sX (VAR_CLOSE_ERROR, &pStatus);
	     Var->fp = NULL;
	   }
	 }
      }
    }
  }
  return pStatus;
}

/******************************************************************************
* WriteAccess.
* Close and then reopen a CDF for read/write access (it was opened with
* read-only access initially).  If the CDF is earlier than CDF V2.5, then
* some of the fields will have to be fixed and the CDR will be truncated for
* a shorter copyright length (unless the CDF is being deleted in which case
* it would be a waste of time to do these things).
******************************************************************************/

STATICforIDL Logical WriteAccess (CDF, forDelete, pStatus)
struct CDFstruct *CDF;
Logical forDelete;      /* Is the write access is needed to delete the CDF? */
CDFstatus *pStatus;	/* Returned status. */
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
   if (!CLOSEv(CDF->dotFp,NULL,&vStats)) {
     CDF->dotFp = NULL;
     AbortAccess (CDF, noUPDATE, noDELETE);
     return FALSE;
   }
   CDF->dotFp = NULL;
   AddTOvStats (&CDF->dotCDFvStats, &vStats);
#if defined(DEBUG)
   DisplayVs (getenv("VSTREAM_STATS"), "DotCDF..", &vStats);
#endif
   if (!sX(CloseVarFiles(CDF),pStatus)) {
     AbortAccess (CDF, noUPDATE, noDELETE);
     return FALSE;
   }
   /***************************************************************************
   * Open dotCDF file with read-write access.  If read-write access is not
   * allowed, try to return to read-only access.  If reopening with read-only
   * access fails, free CDF structures as if CDF had been closed.
   ***************************************************************************/
   BuildFilePath (CDFt, CDF->CDFname, CDF->no_append, CDF->upper_case_ext,
		  CDF->version_numbers, 0L, pathName);
   CDF->dotFp = V_open (pathName, READ_PLUS_a_mode);
   if (CDF->dotFp == NULL) {
     CDF->dotFp = V_open (pathName, READ_ONLY_a_mode);
     if (CDF->dotFp == NULL) {
       AbortAccess (CDF, noUPDATE, noDELETE);
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
       if (!CACHEv(CDF->fp,CDF->workingCacheSize)) {
         *pStatus = BAD_CACHE_SIZE;
         AbortAccess (CDF, noUPDATE, noDELETE);
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
     uInt32 magicNumber1 = V2magicNUMBER_1,
	    magicNumber2 = V2magicNUMBER_2u;
     char copyRight[CDF_COPYRIGHT_LEN+1];
     /*************************************************************************
     * Update magic numbers.
     *************************************************************************/
     if (!SEEKv(CDF->fp,(long)V2_MAGIC_OFFSET_1,vSEEK_SET)) {
       AbortAccess (CDF, noUPDATE, noDELETE);
       *pStatus = CDF_WRITE_ERROR;
       return FALSE;
     }
     if (!Write32(CDF->fp,(Int32 *)&magicNumber1)) {
       AbortAccess (CDF, noUPDATE, noDELETE);
       *pStatus = CDF_WRITE_ERROR;
       return FALSE;
     }
     if (!Write32(CDF->fp,(Int32 *)&magicNumber2)) {
       AbortAccess (CDF, noUPDATE, noDELETE);
       *pStatus = CDF_WRITE_ERROR;
       return FALSE;
     }
     /*************************************************************************
     * If a V2.0 CDF, correct the EOF field.
     *************************************************************************/
     if (CDF->badEOF) {
       if (!sX(CorrectV20eof(CDF),pStatus)) {
	 AbortAccess (CDF, noUPDATE, noDELETE);
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
	 AbortAccess (CDF, noUPDATE, noDELETE);
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
	 AbortAccess (CDF, noUPDATE, noDELETE);
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
	 AbortAccess (CDF, noUPDATE, noDELETE);
	 return FALSE;
       }
       if (!sX(ShortenVDRs(CDF),pStatus)) {
	 AbortAccess (CDF, noUPDATE, noDELETE);
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
	 AbortAccess (CDF, noUPDATE, noDELETE);
	 return FALSE;
       }
       CDF->assumedScopes = FALSE;
     }
     /*************************************************************************
     * Fix blocking factors for variables having a recVary of NOVARY.
     *************************************************************************/
     if (!sX(CorrectBlockingFactors(CDF),pStatus)) {
       AbortAccess (CDF, noUPDATE, noDELETE);
       return FALSE;
     }
     /*************************************************************************
     * Update version/release/increment - should never happen as for older
     * versioned (V2.7 and older) CDFs, we want to keep its original data
     * structure.
     *************************************************************************/
     if (isLFS(CDF)) { /* It should be false all the time. */
       if (!sX(WriteCDR(CDF->fp,V2_CDR_OFFSET,
		        CDR_VERSION,&versionNew,
		        CDR_RELEASE,&releaseNew,
		        CDR_INCREMENT,&incrementNew,
		        CDR_NULL),pStatus)) {
         AbortAccess (CDF, noUPDATE, noDELETE);
         return FALSE;
       }
     }
     /*************************************************************************
     * Update copyright.
     *************************************************************************/
     CDFcopyRight (copyRight);
     NulPad (copyRight, CDF_COPYRIGHT_LEN);
     if (!sX(WriteCDR(CDF->fp,V2_CDR_OFFSET,
		      CDR_COPYRIGHT,copyRight,
		      CDR_NULL),pStatus)) {
       AbortAccess (CDF, noUPDATE, noDELETE);
       return FALSE;
     }
   }
   /***************************************************************************
   * Return based on whether or not write access was obtained.
   ***************************************************************************/
   return (CDF->status == READ_WRITE);
#endif
}

/******************************************************************************
* WriteBuffer.
*    Write occurs at current offset (assumed to have been set before this
* routine is called).  On IBM PCs, it is assumed that `nBytes' will not
* exceed 65535.
******************************************************************************/

STATICforIDL CDFstatus WriteBuffer (CDF, fp, dataType, numElems, buffer)
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
    if (!WRITEv(tBuffer,1,nBytes,fp)) {
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
     if (!WRITEv(&eValue,1,nElemBytes,fp)) return CDF_WRITE_ERROR;
  }
  return pStatus;
}

/******************************************************************************
* NegativeZeroReal4.
* Checks for -0.0 (on any type computer).  Assumed to be in host encoding.
******************************************************************************/

VISIBLE_PREFIX Logical NegativeZeroReal4 (value)
float *value;
{
#if defined(FP1cpu) || defined(FP2cpu)
  return (*((uInt32 *) value) == (uInt32) 0x80000000);
#endif
#if defined(FP3cpu) || defined(FP4cpu)
  /****************************************************************************
  * On VAXes and DEC Alphas running OpenVMS/POSIXshell we're only interested
  * in the sign bit and exponent.
  ****************************************************************************/
  return ((*((uInt32 *) value) & (uInt32) 0x0000FF80) == (uInt32) 0x00008000);
#endif
}

/******************************************************************************
* NegativeZeroReal8.
* Checks for -0.0 (on any type computer).  Assumed to be in host encoding.
******************************************************************************/

VISIBLE_PREFIX Logical NegativeZeroReal8 (value)
double *value;
{
#if defined(FP1cpu)
  return ((*((uInt32 *) value) == (uInt32) 0x80000000) &&
	  (*((uInt32 *) value+1) == (uInt32) 0x00000000));
#endif
#if defined(FP2cpu)
  return ((*((uInt32 *) value) == (uInt32) 0x00000000) &&
	  (*((uInt32 *) value+1) == (uInt32) 0x80000000));
#endif
#if defined(FP3cpu)
  /****************************************************************************
  * On VAXes and DEC Alphas running OpenVMS/POSIXshell in D_FLOAT mode we're
  * only interested in the sign bit and exponent (which are in the first
  * longword [32-bit]).
  ****************************************************************************/
  return ((*((uInt32 *) value) & (uInt32) 0x0000FF80) == (uInt32) 0x00008000);
#endif
#if defined(FP4cpu)
  /****************************************************************************
  * On DEC Alphas running OpenVMS/POSIXshell in G_FLOAT mode we're only
  * interested in the sign bit and exponent (which are in the first longword
  * [32-bit]).
  ****************************************************************************/
  return ((*((uInt32 *) value) & (uInt32) 0x0000FFF0) == (uInt32) 0x00008000);
#endif
}

/******************************************************************************
* StripTrailingBlanks.
******************************************************************************/

STATICforIDL void StripTrailingBlanks (string)
char *string;
{
  int i;
  for (i = strlen(string) - 1; i >= 0 && string[i] == ' '; i--) {
     string[i] = NUL;
  }
  return;
}

/******************************************************************************
* MakeUpperString.
* Convert string to upper-case.
******************************************************************************/

VISIBLE_PREFIX void MakeUpperString (string)
char *string;
{
  int i;
  for (i = 0; string[i] != NUL; i++) {
     string[i] = (char) MakeUpper((int)string[i]);
  }
  return;
}

/******************************************************************************
* MakeLowerString.
* Convert string to lower-case.
******************************************************************************/

VISIBLE_PREFIX void MakeLowerString (string)
char *string;
{
  int i;
  for (i = 0; string[i] != NUL; i++) {
     string[i] = (char) MakeLower((int)string[i]);
  }
  return;
}

/******************************************************************************
* SetBit32.
******************************************************************************/

STATICforIDL void SetBit32 (value, bit)
Int32 *value;
int bit;
{
  *value = *value | (1 << bit);
  return;
}

/******************************************************************************
* ClearBit32.
******************************************************************************/

STATICforIDL void ClearBit32 (value, bit)
Int32 *value;
int bit;
{
  *value = *value & ~(1 << bit);
  return;
}

/******************************************************************************
* FindCDF.
*    Tries various extensions on the specified CDF path to see if the CDF
* exists.  The extensions tried are those which should be present on the
* various platforms plus the extensions which might be generated by a CD-ROM
* driver.  Finally, the pathname is tried without an extension being added
* in case the CDF had been renamed with a different extension or no extension.
******************************************************************************/

STATICforIDL CDFstatus FindCDF (path, no_append, upper, version)
char *path;             /* Base pathname. */
Logical *no_append;     /* Should extensions/version numbers be appended? */
Logical *upper;         /* Should extensions be upper case? */
Logical *version;       /* Should a version number of `;1' be appended? */
{
  char pathT[DU_MAX_PATH_LEN+1];
  size_t pathLen;

  pathLen = strlen (path);

  strcpyX (pathT, path, DU_MAX_PATH_LEN);
  if (pathLen < 4 || strcmp(pathT+(pathLen-4), ".cdf") != 0)
    strcatX (pathT, ".cdf", DU_MAX_PATH_LEN);
  if (IsReg(pathT)) {
    *no_append = FALSE;
    *upper = FALSE;
    *version = FALSE;
    return CDF_OK;
  }

  strcpyX (pathT, path, DU_MAX_PATH_LEN);
  if (pathLen < 4 || strcmp(pathT+(pathLen-4), ".CDF") != 0)
    strcatX (pathT, ".CDF", DU_MAX_PATH_LEN);
  if (IsReg(pathT)) {
    *no_append = FALSE;
    *upper = TRUE;
    *version = FALSE;
    return CDF_OK;
  }

  strcpyX (pathT, path, DU_MAX_PATH_LEN);
  if (pathLen < 6 || !StrStrIgCaseX(pathT+(pathLen-6), ".cdf;1"))
    strcatX (pathT, ".cdf;1", DU_MAX_PATH_LEN);
  if (IsReg(pathT)) {
    *no_append = FALSE;
    *upper = FALSE;
    *version = TRUE;
    return CDF_OK;
  }

  strcpyX (pathT, path, DU_MAX_PATH_LEN);
  if (pathLen < 6 || !StrStrIgCaseX(pathT+(pathLen-6), ".CDF;1"))
    strcatX (pathT, ".CDF;1", DU_MAX_PATH_LEN);
  if (IsReg(pathT)) {
    *no_append = FALSE;
    *upper = TRUE;
    *version = TRUE;
    return CDF_OK;
  }

/*
#if defined(unix) || defined(dos) || defined(win32) 
*/
#if defined(win32) || defined(dos) 
  strcpyX (pathT, path, DU_MAX_PATH_LEN);
  MakeUpperString (pathT);
  if (pathLen < 4 || !StrStrIgCaseX(pathT+(pathLen-4), ".CDF"))
    strcatX (pathT, ".CDF", DU_MAX_PATH_LEN);
  if (IsReg(pathT)) {
    *no_append = FALSE;
    *upper = TRUE;
    *version = FALSE;
    return CDF_OK;
  }
/*
  strcpyX (pathT, path, DU_MAX_PATH_LEN);
  MakeUpperString (pathT);
  if (IsReg(pathT)) {
    *no_append = TRUE;
    *upper = TRUE;
    *version = FALSE;
    return CDF_OK;
  }
*/
#endif

  if (IsReg(path)) {
    *no_append = TRUE;
    *upper = FALSE;
    *version = FALSE;
    return CDF_OK;
  }

  return NO_SUCH_CDF;
}

/******************************************************************************
* BuildFilePath.
******************************************************************************/

STATICforIDL void BuildFilePath (fileType, pathBase, noAppend, upperCase,
				 versionNumber, varN, pathX)
int fileType;           /* Type of file. */
char *pathBase;         /* Base pathname. */
Logical noAppend;       /* Should extensions/version numbers be appended? */
Logical upperCase;      /* Should uppercase extensions be appended? */
Logical versionNumber;  /* Should a version number of `;1' be appended? */
Int32 varN;             /* Variable number.  N/a if a `cdf' file. */
char pathX[DU_MAX_PATH_LEN+1];
			/* The expanded path w/ extensions/version numbers. */
{
  ExpandPath (pathBase, pathX);
  if (!noAppend) {
    switch (fileType) {
      case CDFt:
	strcatX (pathX, (upperCase ? ".CDF" : ".cdf"), DU_MAX_PATH_LEN);
	break;
      case Vt:
	strcatX (pathX, (upperCase ? ".V" : ".v"), DU_MAX_PATH_LEN);
  	snprintf (EofS(pathX), (size_t) DU_MAX_PATH_LEN+1-strlen(pathX), 
                  "%d", (int) varN);
	break;
      case Zt:
	strcatX (pathX, (upperCase ? ".Z" : ".z"), DU_MAX_PATH_LEN);
	snprintf (EofS(pathX), (size_t) DU_MAX_PATH_LEN+1-strlen(pathX),
                  "%d", (int) varN);
	break;
    }
    strcatX (pathX, (versionNumber ? ";1" : ""), DU_MAX_PATH_LEN);
  }
  return;
}

/******************************************************************************
* NulPad.
*    Pads with NUL characters to the length specified.  Also NUL-terminates
* the string.
******************************************************************************/

STATICforIDL void NulPad (string, length)
char *string;
int length;
{
  int i;
  int j;
/*
  for (i = (int) strlen(string); i < length; i++) string[i] = NUL;
  string[length] = NUL;
*/
  j = (int) strlen(string);
  i = length - j;
  if (i > 0) 
    memset(string+j, '\0', i+1);
  return;
}

/******************************************************************************
* UpdateMaxRec.
******************************************************************************/

STATICforIDL CDFstatus UpdateMaxRec (CDF, Var, recNum)
struct CDFstruct *CDF;          /* In: Pointer to CDF. */
struct VarStruct *Var;          /* In: Pointer to variable. */
Int32 recNum;                    /* In: Possible new maximum record number. */
{
  CDFstatus pStatus = CDF_OK;
  if (recNum > Var->maxRec) {
    Var->maxRec = recNum;
    if (!sX(WriteVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
		     VDR_MAXREC,&recNum,
		     VDR_NULL),&pStatus)) return pStatus;
  }
  if (!Var->zVar) {
    if (recNum > CDF->rMaxRec) {
      CDF->rMaxRec = recNum;
      if (!sX(WriteGDR(CDF->fp,CDF->GDRoffset,
		       GDR_rMAXREC,&recNum,
		       GDR_NULL),&pStatus)) return pStatus;
    }
  }
  return pStatus;
}

/******************************************************************************
* CalcDimParms.
*    Calculates a variable's number of dimensions, dimension sizes, and
* dimension variances depending on the current zMode.
******************************************************************************/

STATICforIDL CDFstatus CalcDimParms (CDF, offset, zVar, numDimsP, dimSizesP,
				     dimVarysP)
struct CDFstruct *CDF;          /* In: Pointer to CDF. */
Int32 offset;                   /* In: Offset of VDR. */
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
    if (!sX(ReadVDR(CDF,CDF->fp,offset,TRUE,
		    VDR_zNUMDIMS,&tNumDims,
		    VDR_zDIMSIZES,tDimSizes,
		    VDR_DIMVARYS,tDimVarys,
		    VDR_NULL),&pStatus)) return pStatus;
  }
  else {
    if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		    GDR_rNUMDIMS,&tNumDims,
		    GDR_rDIMSIZES,tDimSizes,
		    GDR_NULL),&pStatus)) return pStatus;
    if (!sX(ReadVDR(CDF,CDF->fp,offset,FALSE,
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
* NULterminateMAX.
*    NUL-terminate a string but only if a NUL is not found before the maximum
* length is reached.
******************************************************************************/

VISIBLE_PREFIX void NULterminateMAX (string, maxLen)
char *string;
size_t maxLen;
{
  int i;
  for (i = 0; i < (int) maxLen; i++)
     if (string[i] == NUL) return;
  string[maxLen] = NUL;
  return;
}

/******************************************************************************
* ClearBytes.
******************************************************************************/

VISIBLE_PREFIX void ClearBytes (buffer, firstByte, lastByte)
void *buffer;
int firstByte;
int lastByte;
{
  memset ((Byte1 *)buffer+firstByte, 0, (size_t) (lastByte - firstByte + 1));
  return;
}

/******************************************************************************
* WasteIR.
******************************************************************************/

STATICforIDL CDFstatus WasteIR (CDF, wasteOffset, size)
struct CDFstruct *CDF;
Int32 wasteOffset;
Int32 size;
{
  CDFstatus pStatus = CDF_OK;
  struct UIRstruct newUIR, firstUIR, tUIR, nextUIR;
  Int32 tOffset, nextOffset, UIRhead;

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
  if (size < UIR_BASE_SIZE) {
    if (size < UUIR_BASE_SIZE) return CDF_INTERNAL_ERROR;
    if (!sX(WriteUIR(CDF->fp,wasteOffset,
		     UIR_RECORDSIZE,&(newUIR.RecordSize),
		     UIR_RECORDTYPE,&(newUIR.RecordType),
		     UIR_NULL),&pStatus)) return pStatus;
    return pStatus;
  }
  /****************************************************************************
  * Read offset of first UIR.
  ****************************************************************************/
  if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		  GDR_UIRHEAD,&UIRhead,
		  GDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * Check if no UIRs exist yet.
  ****************************************************************************/
  if (UIRhead == 0) {
    newUIR.NextUIR = 0;
    newUIR.PrevUIR = 0;
    if (!sX(WriteUIR(CDF->fp,wasteOffset,
		     UIR_RECORD,&newUIR,
		     UIR_NULL),&pStatus)) return pStatus;
    UIRhead = wasteOffset;
    if (!sX(WriteGDR(CDF->fp,CDF->GDRoffset,
		     GDR_UIRHEAD,&UIRhead,
		     GDR_NULL),&pStatus)) return pStatus;
    return pStatus;
  }
  /****************************************************************************
  * At least one UIR exists, check if the new UIR is before the first UIR.
  ****************************************************************************/
  if (wasteOffset < UIRhead) {
    if (!sX(ReadUIR(CDF->fp,UIRhead,
		    UIR_RECORD,&firstUIR,
		    UIR_NULL),&pStatus)) return pStatus;
    newUIR.NextUIR = UIRhead;
    newUIR.PrevUIR = 0;
    if (!sX(WriteUIR(CDF->fp,wasteOffset,
		     UIR_RECORD,&newUIR,
		     UIR_NULL),&pStatus)) return pStatus;
    firstUIR.PrevUIR = wasteOffset;
    if (!sX(WriteUIR(CDF->fp,UIRhead,
		     UIR_RECORD,&firstUIR,
		     UIR_NULL),&pStatus)) return pStatus;
    UIRhead = wasteOffset;
    if (!sX(WriteGDR(CDF->fp,CDF->GDRoffset,
		     GDR_UIRHEAD,&UIRhead,
		     GDR_NULL),&pStatus)) return pStatus;
    return pStatus;
  }
  /****************************************************************************
  * The new UIR is not before the first UIR.  Scan the UIRs to find the point
  * at which it should be inserted.
  ****************************************************************************/
  tOffset = UIRhead;
  if (!sX(ReadUIR(CDF->fp,tOffset,
		  UIR_RECORD,&tUIR,
		  UIR_NULL),&pStatus)) return pStatus;
  while (tUIR.NextUIR != 0) {
    if (wasteOffset < tUIR.NextUIR) {
      nextOffset = tUIR.NextUIR;
      if (!sX(ReadUIR(CDF->fp,nextOffset,
		      UIR_RECORD,&nextUIR,
		      UIR_NULL),&pStatus)) return pStatus;
      newUIR.NextUIR = tUIR.NextUIR;
      newUIR.PrevUIR = tOffset;
      if (!sX(WriteUIR(CDF->fp,wasteOffset,
		       UIR_RECORD,&newUIR,
		       UIR_NULL),&pStatus)) return pStatus;
      tUIR.NextUIR = wasteOffset;
      if (!sX(WriteUIR(CDF->fp,tOffset,
		       UIR_RECORD,&tUIR,
		       UIR_NULL),&pStatus)) return pStatus;
      nextUIR.PrevUIR = wasteOffset;
      if (!sX(WriteUIR(CDF->fp,nextOffset,
		       UIR_RECORD,&nextUIR,
		       UIR_NULL),&pStatus)) return pStatus;
      return pStatus;
    }
    tOffset = tUIR.NextUIR;
    if (!sX(ReadUIR(CDF->fp,tOffset,
		    UIR_RECORD,&tUIR,
		    UIR_NULL),&pStatus)) return pStatus;
  }
  /****************************************************************************
  * The new UIR is going to be the last UIR.
  ****************************************************************************/
  newUIR.NextUIR = 0;
  newUIR.PrevUIR = tOffset;
  if (!sX(WriteUIR(CDF->fp,wasteOffset,
		   UIR_RECORD,&newUIR,
		   UIR_NULL),&pStatus)) return pStatus;
  tUIR.NextUIR = wasteOffset;
  if (!sX(WriteUIR(CDF->fp,tOffset,
		   UIR_RECORD,&tUIR,
		   UIR_NULL),&pStatus)) return pStatus;
  return pStatus;
}

/******************************************************************************
* AllocateIR.
******************************************************************************/

STATICforIDL CDFstatus AllocateIR (CDF, size, offset)
struct CDFstruct *CDF;          /* In: Pointer to CDF. */
Int32 size;                     /* In: Size of internal record (bytes). */
Int32 *offset;                  /* Out: Offset of allocated internal record. */
{
  CDFstatus pStatus = CDF_OK;
  Int32 sOffset, eOffset, tSize, UIRhead, eof, uir_ = UIR_;
  struct UIRstruct sUIR, eUIR;
  /****************************************************************************
  * Read EOF and offset of first UIR from GDR.
  ****************************************************************************/
  if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		  GDR_UIRHEAD,&UIRhead,
		  GDR_EOF,&eof,
		  GDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * If UIRs exist, try to use one or more of them (if contiguous) for the new
  * internal record.
  ****************************************************************************/
  if (UIRhead != 0) {
    sOffset = UIRhead;
    if (!sX(ReadUIR(CDF->fp,sOffset,
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
	 if (!sX(RemoveUIRs(CDF,sOffset,eOffset),&pStatus)) return pStatus;
	 if (!sX(WriteIrSize(CDF->fp,sOffset,&size),&pStatus)) return pStatus;
	 if (!sX(WriteIrType(CDF->fp,sOffset,&uir_),&pStatus)) return pStatus;
	 *offset = sOffset;
	 return pStatus;
       }
       /***********************************************************************
       * Check if the starting to ending UIRs are big enough for the new
       * internal record and for a new UIR to fill the remaining space.
       ***********************************************************************/
       if (size + UIR_BASE_SIZE <= tSize) {
	 if (!sX(RemoveUIRs(CDF,sOffset,eOffset),&pStatus)) return pStatus;
	 if (!sX(WasteIR(CDF,sOffset+size,tSize-size),&pStatus)) {
	   return pStatus;
	 }
	 if (!sX(WriteIrSize(CDF->fp,sOffset,&size),&pStatus)) return pStatus;
	 if (!sX(WriteIrType(CDF->fp,sOffset,&uir_),&pStatus)) return pStatus;
	 *offset = sOffset;
	 return pStatus;
       }
       /***********************************************************************
       * Check if the end of the UIRs has been reached.  If so, check if the
       * ending UIR is the last IR in the dotCDF file.
       ***********************************************************************/
       if (eUIR.NextUIR == 0) {
	 if (eOffset + eUIR.RecordSize == eof) {
	   /*******************************************************************
	   * The ending UIR is the last internal record in the CDF.  Check to
	   * see if after allocating the new internal record less than
	   * UIR_BASE_SIZE bytes will remain before the EOF.  If so, waste an
	   * internal record at the location of those bytes so that a UIR is
	   * at the end (rather than stranded bytes).
	   *******************************************************************/
	   if (!sX(RemoveUIRs(CDF,sOffset,eOffset),&pStatus)) return pStatus;
	   if (size < tSize) {
	     if (!sX(WasteIR(CDF,sOffset+size,
			     UIR_BASE_SIZE),&pStatus)) return pStatus;
	     eof = sOffset + size + UIR_BASE_SIZE;
	   }
	   else
	     eof = sOffset + size;
	   if (!sX(WriteGDR(CDF->fp,CDF->GDRoffset,
			    GDR_EOF,&eof,
			    GDR_NULL),&pStatus)) return pStatus;
	   if (!sX(WriteIrSize(CDF->fp,
			       sOffset,
			       &size),&pStatus)) return pStatus;
	   if (!sX(WriteIrType(CDF->fp,
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
	   if (!sX(WriteIrSize(CDF->fp,eof,&size),&pStatus)) return pStatus;
	   if (!sX(WriteIrType(CDF->fp,eof,&uir_),&pStatus)) return pStatus;
	   eof += size;
	   if (!sX(WriteGDR(CDF->fp,CDF->GDRoffset,
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
	 if (!sX(ReadUIR(CDF->fp,eOffset,
			 UIR_RECORD,&eUIR,
			 UIR_NULL),&pStatus)) return pStatus;
	 tSize += eUIR.RecordSize;
       }
       else {
	 sOffset = eUIR.NextUIR;
	 if (!sX(ReadUIR(CDF->fp,sOffset,
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
  if (!sX(WriteIrSize(CDF->fp,eof,&size),&pStatus)) return pStatus;
  if (!sX(WriteIrType(CDF->fp,eof,&uir_),&pStatus)) return pStatus;
  eof += size;
  if (!sX(WriteGDR(CDF->fp,CDF->GDRoffset,
		   GDR_EOF,&eof,
		   GDR_NULL),&pStatus)) return pStatus;
  return pStatus;
}

/******************************************************************************
* ResizeIR.
******************************************************************************/

STATICforIDL CDFstatus ResizeIR (CDF, curOffset, newSize, newOffset, move,
				 success)
struct CDFstruct *CDF;
Int32 curOffset;        /* In: Current offset of internal record. */
Int32 newSize;          /* In: New size of internal record.  This may be
			   smaller or larger than the current size. */
Int32 *newOffset;       /* Out: New offset of internal record.  This variable
			   is not modified if an error occurs or the internal
			   record cannot be extended (when `move' is FALSE). */
Logical move;           /* In: TRUE if the internal record can be moved if
			   necessary. */
Logical *success;       /* Out: TRUE if the internal record was successfully
			   extended (whether or not it had to be moved). */
{
  CDFstatus pStatus = CDF_OK; Int32 curSize; Int32 eof;
  /****************************************************************************
  * Determine current size of internal record.
  ****************************************************************************/
  if (!sX(ReadIrSize(CDF->fp,curOffset,&curSize),&pStatus)) return pStatus;
  /****************************************************************************
  * Check sizes...
  ****************************************************************************/
  if (newSize > curSize) {
    /**************************************************************************
    * The internal record is growing.  First check if it is the last one in
    * the CDF.
    **************************************************************************/
    if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		    GDR_EOF,&eof,
		    GDR_NULL),&pStatus)) return pStatus;
    if (curOffset + curSize == eof) {
      /************************************************************************
      * Last internal record.  Simply extend the CDF.
      ************************************************************************/
      ASSIGNnotNULL (newOffset, curOffset)
      eof += (newSize - curSize);
      if (!sX(WriteGDR(CDF->fp,CDF->GDRoffset,
		       GDR_EOF,&eof,
		       GDR_NULL),&pStatus)) return pStatus;
      if (!sX(WriteIrSize(CDF->fp,
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
	if (!sX(WasteIR(CDF,curOffset,curSize),&pStatus)) return pStatus;
	if (!sX(AllocateIR(CDF,newSize,newOffset),&pStatus)) return pStatus;
	ASSIGNnotNULL (success, TRUE)
	return pStatus;
      }
      else {
	Int32 sOffset, eOffset, tSize, UIRhead, irType;
	struct UIRstruct sUIR, eUIR;
	/**********************************************************************
	* First check if there are any UIRs in the CDF.  This is done because
	* CDF V2.5.0* (alpha/beta) created UIRs without the next and previous
	* UIR fields and didn't use the `UIRhead' field in the GDR.  Because
	* we don't want to use UIRs in those CDFs (because they are not the
	* same as the current UIRs), this will keep us from doing so (because
	* the `UIRhead' fields will always contain zero if a V2.5.0* CDF).
	**********************************************************************/
	if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
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
	if (!sX(ReadIrType(CDF->fp,sOffset,&irType),&pStatus)) return pStatus;
	if (irType != UIR_) {
	  ASSIGNnotNULL (success, FALSE)
	  return pStatus;
	}
	if (!sX(ReadUIR(CDF->fp,sOffset,
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
	     if (!sX(RemoveUIRs(CDF,sOffset,eOffset),&pStatus)) return pStatus;
	     if (!sX(WriteIrSize(CDF->fp,
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
	   if (newSize + UIR_BASE_SIZE <= tSize) {
	     if (!sX(RemoveUIRs(CDF,sOffset,eOffset),&pStatus)) return pStatus;
	     if (!sX(WasteIR(CDF,curOffset+newSize,tSize-newSize),&pStatus)) {
	       return pStatus;
	     }
	     if (!sX(WriteIrSize(CDF->fp,
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
	     if (eOffset + eUIR.RecordSize == eof) {
	       if (!sX(RemoveUIRs(CDF,sOffset,eOffset),&pStatus)) return
								  pStatus;
	       if (newSize < tSize) {
		 if (!sX(WasteIR(CDF,curOffset+newSize,
				 UIR_BASE_SIZE),&pStatus)) return pStatus;
		 eof = curOffset + newSize + UIR_BASE_SIZE;
	       }
	       else
		 eof = curOffset + newSize;
	       if (!sX(WriteGDR(CDF->fp,CDF->GDRoffset,
				GDR_EOF,&eof,
				GDR_NULL),&pStatus)) return pStatus;
	       if (!sX(WriteIrSize(CDF->fp,
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
	     if (!sX(ReadUIR(CDF->fp,eOffset,
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
    if (newSize <= (curSize - UIR_BASE_SIZE)) {
      if (!sX(WasteIR(CDF,curOffset + newSize,
		      curSize - newSize),&pStatus)) return pStatus;
      if (!sX(WriteIrSize(CDF->fp,
			  curOffset,
			  &newSize),&pStatus)) return pStatus;
      ASSIGNnotNULL (newOffset, curOffset)
      ASSIGNnotNULL (success, TRUE)
    }
    else {
      if (move) {
	if (!sX(WasteIR(CDF,curOffset,curSize),&pStatus)) return pStatus;
	if (!sX(AllocateIR(CDF,newSize,newOffset),&pStatus)) return pStatus;
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
* RemoveUIRs.
******************************************************************************/

STATICforIDL CDFstatus RemoveUIRs (CDF, sOffset, eOffset)
struct CDFstruct *CDF;          /* In: Pointer to CDF. */
Int32 sOffset;                  /* In: Offset of starting UIR. */
Int32 eOffset;                  /* In: Offset of ending UIR. */
{
  CDFstatus pStatus = CDF_OK;
  struct UIRstruct sUIR, eUIR;
  Int32 UIRhead;
  if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		  GDR_UIRHEAD,&UIRhead,
		  GDR_NULL),&pStatus)) return pStatus;
  if (!sX(ReadUIR(CDF->fp,sOffset,
		  UIR_RECORD,&sUIR,
		  UIR_NULL),&pStatus)) return pStatus;
  if (!sX(ReadUIR(CDF->fp,eOffset,
		  UIR_RECORD,&eUIR,
		  UIR_NULL),&pStatus)) return pStatus;
  if (UIRhead == sOffset) {
    UIRhead = eUIR.NextUIR;
    if (!sX(WriteGDR(CDF->fp,CDF->GDRoffset,
		     GDR_UIRHEAD,&UIRhead,
		     GDR_NULL),&pStatus)) return pStatus;
  }
  else {
    struct UIRstruct prevUIR;
    if (!sX(ReadUIR(CDF->fp,sUIR.PrevUIR,
		    UIR_RECORD,&prevUIR,
		    UIR_NULL),&pStatus)) return pStatus;
    prevUIR.NextUIR = eUIR.NextUIR;
    if (!sX(WriteUIR(CDF->fp,sUIR.PrevUIR,
		     UIR_RECORD,&prevUIR,
		     UIR_NULL),&pStatus)) return pStatus;
  }
  if (eUIR.NextUIR != 0) {
    struct UIRstruct nextUIR;
    if (!sX(ReadUIR(CDF->fp,eUIR.NextUIR,
		    UIR_RECORD,&nextUIR,
		    UIR_NULL),&pStatus)) return pStatus;
    nextUIR.PrevUIR = sUIR.PrevUIR;
    if (!sX(WriteUIR(CDF->fp,eUIR.NextUIR,
		     UIR_RECORD,&nextUIR,
		     UIR_NULL),&pStatus)) return pStatus;
  }
  return pStatus;
}

/******************************************************************************
* PriorTo.
******************************************************************************/

VISIBLE_PREFIX Logical PriorTo (spec, version, release, increment)
char *spec;
Int32 version;
Int32 release;
Int32 increment;
{
  int ver, rel, incr;
  switch (sscanf(spec,"%d.%d.%d",&ver,&rel,&incr)) {
    case 1:
      if (version < ver) return TRUE;
      break;
    case 2:
      if (version < ver) return TRUE;
      if (version == ver && release < rel) return TRUE;
      break;
    case 3:
      if (version < ver) return TRUE;
      if (version == ver && release < rel) return TRUE;
      if (version == ver && release == rel && increment < incr) return TRUE;
      break;
  }
  return FALSE;
}

/******************************************************************************
* ShortenCDR.
******************************************************************************/

STATICforIDL CDFstatus ShortenCDR (CDF)
struct CDFstruct *CDF;
{
  CDFstatus pStatus = CDF_OK;
  Int32 offset, oldRecordSize, newRecordSize, nBytes;
  if (!sX(ReadCDR(CDF->fp,CDF->CDRoffset,
		  CDR_RECORDSIZE,&oldRecordSize,
		  CDR_NULL),&pStatus))
    return pStatus;
  newRecordSize = CDR_BASE_SIZE + CDF_COPYRIGHT_LEN;
  if (!sX(WriteCDR(CDF->fp,V2_CDR_OFFSET,
		   CDR_RECORDSIZE,&newRecordSize,
		   CDR_NULL),&pStatus)) return pStatus;
  offset = CDF->CDRoffset + newRecordSize;
  nBytes = oldRecordSize - newRecordSize;
  if (!sX(WasteIR(CDF,offset,nBytes),&pStatus)) return pStatus;
  return pStatus;
}

/******************************************************************************
* CorrectScopes.
*    It is assumed that the last ADR offset has already been fixed.
******************************************************************************/

STATICforIDL CDFstatus CorrectScopes (CDF)
struct CDFstruct *CDF;
{
  Int32 tOffset, attrScope;
  CDFstatus pStatus = CDF_OK;
  /****************************************************************************
  * Read the offset of the first ADR from the GDR.
  ****************************************************************************/
  if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		  GDR_ADRHEAD,&tOffset,
		  GDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * Read the ADRs correcting each assumed scope.
  ****************************************************************************/
  while (tOffset != 0) {
     if (!sX(ReadADR(CDF->fp,tOffset,
		     ADR_SCOPE,&attrScope,
		     ADR_NULL),&pStatus)) return pStatus;
     switch (attrScope) {
       case GLOBALscopeASSUMED:
	 attrScope = GLOBAL_SCOPE;
	 if (!sX(WriteADR(CDF->fp,tOffset,
			  ADR_SCOPE,&attrScope,
			  ADR_NULL),&pStatus)) return pStatus;
	 break;
       case VARIABLEscopeASSUMED:
	 attrScope = VARIABLE_SCOPE;
	 if (!sX(WriteADR(CDF->fp,tOffset,
			  ADR_SCOPE,&attrScope,
			  ADR_NULL),&pStatus)) return pStatus;
	 break;
     }
     if (!sX(ReadADR(CDF->fp,tOffset,
		     ADR_ADRNEXT,&tOffset,
		     ADR_NULL),&pStatus)) return pStatus;
  }
  return pStatus;
}

/******************************************************************************
* ShortenVDRs.
*    It is assumed that the last rVDR offset has already been fixed.
******************************************************************************/

STATICforIDL CDFstatus ShortenVDRs (CDF)
struct CDFstruct *CDF;
{
  Int32 vOffset, nextOffset, recordSize, nTailBytes;
  void *tBuffer;
  Int32 tOffset;
  CDFstatus pStatus = CDF_OK;
  int i;
  for (i = 0; i < 2; i++) {
     Logical zVar = (i == 0);
     /*************************************************************************
     * Read the offset of the first rVDR from the GDR.
     *************************************************************************/
     if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		     BOO(zVar,GDR_zVDRHEAD,GDR_rVDRHEAD),&vOffset,
		     GDR_NULL),&pStatus)) return pStatus;
     /*************************************************************************
     * Read the rVDRs shortening each.
     *************************************************************************/
     while (vOffset != ZERO_OFFSET) {
       if (!sX(ReadVDR(CDF,CDF->fp,vOffset,zVar,
		       VDR_VDRNEXT,&nextOffset,
		       VDR_RECORDSIZE,&recordSize,
		       VDR_NULL),&pStatus)) return pStatus;
       nTailBytes = recordSize - VDR_WASTED_OFFSET - VDR_WASTED_SIZE;
       recordSize -= VDR_WASTED_SIZE;
       if (!sX(WriteVDR(CDF,CDF->fp,vOffset,zVar,
			VDR_RECORDSIZE,&recordSize,
			VDR_NULL),&pStatus)) return pStatus;
       tBuffer = cdf_AllocateMemory ((size_t) nTailBytes, NULL);
       if (tBuffer != NULL) {
	 tOffset = vOffset + VDR_WASTED_OFFSET + VDR_WASTED_SIZE;
	 if (!SEEKv(CDF->fp,tOffset,vSEEK_SET)) return CDF_READ_ERROR;
	 if (!READv(tBuffer,(size_t)nTailBytes,1,CDF->fp)) return
							   CDF_READ_ERROR;
	 tOffset = vOffset + VDR_WASTED_OFFSET;
	 if (!SEEKv(CDF->fp,tOffset,vSEEK_SET)) return CDF_WRITE_ERROR;
	 if (!WRITEv(tBuffer,(size_t)nTailBytes,1,CDF->fp)) return
							    CDF_WRITE_ERROR;
	 cdf_FreeMemory (tBuffer, NULL);
       }
       else {
	 Int32 oldOffset = vOffset + VDR_WASTED_OFFSET + VDR_WASTED_SIZE,
	       newOffset = vOffset + VDR_WASTED_OFFSET, byteX;
	 Byte1 tByte;
	 for (byteX = 0; byteX < nTailBytes; byteX++) {
	    if (!SEEKv(CDF->fp,oldOffset,vSEEK_SET)) return CDF_READ_ERROR;
	    if (!READv(&tByte,1,1,CDF->fp)) return CDF_READ_ERROR;
	    if (!SEEKv(CDF->fp,newOffset,vSEEK_SET)) return CDF_WRITE_ERROR;
	    if (!WRITEv(&tByte,1,1,CDF->fp)) return CDF_WRITE_ERROR;
	    oldOffset++;
	    newOffset++;
	 }
       }
       if (!sX(WasteIR(CDF,vOffset+recordSize,VDR_WASTED_SIZE),&pStatus)) {
	 return pStatus;
       }
       vOffset = nextOffset;
     }
  }
  return pStatus;
}

/******************************************************************************
* CorrectBlockingFactors.
*    It is assumed that the last rVDR offset has already been fixed.
******************************************************************************/

STATICforIDL CDFstatus CorrectBlockingFactors (CDF)
struct CDFstruct *CDF;
{
  CDFstatus pStatus = CDF_OK; Int32 nVars, vdrOffset; int varN;
  Logical zVar; struct VDRstruct VDR; struct VarStruct **Vars, *Var;
  for (zVar = 0; zVar <= 1; zVar++) {
     Vars = BOO(zVar,CDF->zVars,CDF->rVars);
     nVars = BOO(zVar,CDF->NzVars,CDF->NrVars);
     for (varN = 0; varN < nVars; varN++) {
	Var = Vars[varN];
	if (Var == NULL) {
	  if (!sX(FindVarByNumber(CDF,(Int32)varN,
				  zVar,&vdrOffset),&pStatus)) return pStatus;
	}
	else
	  vdrOffset = Var->VDRoffset;
	if (!sX(ReadVDR(CDF,CDF->fp,vdrOffset,zVar,
			VDR_RECORD,&VDR,NULL,
			VDR_NULL),&pStatus)) return pStatus;
	if (!RECvaryBITset(VDR.Flags) && VDR.blockingFactor > 1) {
	  VDR.blockingFactor = 1;
	  if (!sX(WriteVDR(CDF,CDF->fp,vdrOffset,zVar,
			   VDR_RECORD,&VDR,NULL,
			   VDR_NULL),&pStatus)) return pStatus;
	  if (Var != NULL) {
	    if (!sX(CalcBF(CDF,Var),&pStatus)) return pStatus;
	  }
	}
     }
  }
  return pStatus;
}

/******************************************************************************
* CorrectEPOCH.
******************************************************************************/

STATICforIDL CDFstatus CorrectEPOCH (CDF)
struct CDFstruct *CDF;
{
  CDFstatus tStatus, pStatus = CDF_OK;
  Int32 dataType, vNum, vOffset, aOffset, eOffset;
  Logical zVar;
  int i;
  /****************************************************************************
  * Search for EPOCH rVariable.
  ****************************************************************************/
  tStatus = FindVarByName (CDF, "EPOCH", &vOffset, &zVar, NULL);
  switch (tStatus) {
    case CDF_OK:
      if (!sX(ReadVDR(CDF,CDF->fp,vOffset,zVar,
		      VDR_NUM,&vNum,
		      VDR_DATATYPE,&dataType,
		      VDR_NULL),&pStatus)) return pStatus;
      if (FLOAT8dataType(dataType)) dataType = CDF_EPOCH;
      if (!sX(WriteVDR(CDF,CDF->fp,vOffset,zVar,
		       VDR_DATATYPE,&dataType,
		       VDR_NULL),&pStatus)) return pStatus;
      /************************************************************************
      * Search for associated VALIDMIN/VALIDMAX/SCALEMIN/SCALEMAX rEntries.
      ************************************************************************/
      for (i = 0; i < 4; i++) {
	 char aName[8+1];
	 switch (i) {
	   case 0: strcpy (aName, "VALIDMIN"); break;
	   case 1: strcpy (aName, "VALIDMAX"); break;
	   case 2: strcpy (aName, "SCALEMIN"); break;
	   case 3: strcpy (aName, "SCALEMAX"); break;
	 }
	 tStatus = FindAttrByName (CDF,aName,&aOffset);
	 switch (tStatus) {
	   case CDF_OK:
	     tStatus = FindEntryByNumber (CDF, aOffset, zVar, vNum,
					  &eOffset);    /* We can do this since
							   only rVariables will
							   exist. */
	     switch (tStatus) {
	       case CDF_OK:
		 if (!sX(ReadAEDR(CDF->fp,eOffset,
				  AEDR_DATATYPE,&dataType,
				  AEDR_NULL),&pStatus)) return pStatus;
		 if (FLOAT8dataType(dataType)) dataType = CDF_EPOCH;
		 if (!sX(WriteAEDR(CDF,CDF->fp,eOffset,
				   AEDR_DATATYPE,&dataType,
				   AEDR_NULL),&pStatus)) return pStatus;
		 break;
	       case NO_SUCH_ENTRY:
		 break;
	       default:
		 return tStatus;
	     }
	     break;
	   case NO_SUCH_ATTR:
	     break;
	   default:
	     return tStatus;
	 }
      }
      break;
    case NO_SUCH_VAR:
      break;
    default:
      return tStatus;
  }
  return pStatus;
}

/******************************************************************************
* CDFdeleteFile.
******************************************************************************/

STATICforIDL Logical CDFdeleteFile (path)
char *path;
{
#if defined(vms)
  char tPath[DU_MAX_PATH_LEN+1];
  strcpyX (tPath, path, DU_MAX_PATH_LEN);
  strcatX (tPath, ";0", DU_MAX_PATH_LEN);    /* Only most recent is deleted. */
  return (delete(tPath) == 0);
#endif
#if defined(unix) || defined(dos)
  return (unlink(path) == 0);
#endif
#if defined(mac) || defined(posixSHELL) || defined(win32)
  return (remove(path) == 0);
#endif
}

/******************************************************************************
* KillAbortedCDF.
******************************************************************************/

STATICforIDL void KillAbortedCDF (CDF, Cur)
struct CDFstruct *CDF;
struct CurStruct *Cur;
{
  CDF->magic = KILLEDid_MAGIC_NUMBER;
  cdf_FreeMemory (CDF, NULL);
  Cur->cdf = NULL;
  return;
}

/******************************************************************************
* AbortAccess.
******************************************************************************/

STATICforIDL void AbortAccess (CDF, updateCDF, deleteCDF)
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
    DeleteCDFfiles (CDF);
    if (CDF->uDotFp != NULL) V_delete (CDF->uDotFp, NULL);
  }
  else {
    /**************************************************************************
    * Update the dotCDF file if requested, if the current access is read/write,
    * and if the "working" dotCDF file has not already been closed or deleted.
    **************************************************************************/
    if (CDF->status == READ_WRITE && updateCDF &&
	(CDF->fp == CDF->dotFp || CDF->fp == CDF->uDotFp)) UpdateDotCDF (CDF);
    /**************************************************************************
    * Close all of the CDF files if they are not already closed (or deleted).
    **************************************************************************/
    if (CDF->dotFp != NULL) V_close (CDF->dotFp,
				     ((updateCDF==UPDATE)?CDF:NULL), NULL);
    if (CDF->uDotFp != NULL) V_close (CDF->uDotFp,
				      ((updateCDF==UPDATE)?CDF:NULL), NULL);
    CloseVarFiles (CDF);
  }
  /****************************************************************************
  * Delete the scratch files that still exist.
  ****************************************************************************/
  if (CDF->stage.fp != NULL) V_delete (CDF->stage.fp, NULL);
  if (CDF->compressFp != NULL) V_delete (CDF->compressFp, NULL);
  /****************************************************************************
  * Free the memory used by the CDF.
  ****************************************************************************/
  FreeCDFid (CDF, TRUE);
  return;
}

/******************************************************************************
* DeleteCDFfiles.
* The files may be open or closed.  This routine does not delete the
* uncompressed dotCDF file (if one exists).
******************************************************************************/

STATICforIDL CDFstatus DeleteCDFfiles (CDF)
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
    if (!DELETEv(CDF->dotFp,NULL)) sX (CDF_DELETE_ERROR, &pStatus);
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
	   if (!DELETEv(Var->fp,NULL)) sX (VAR_DELETE_ERROR, &pStatus);
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
	   if (!DELETEv(Var->fp,NULL)) sX (VAR_DELETE_ERROR, &pStatus);
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
* DeleteEntry.
******************************************************************************/

STATICforIDL CDFstatus DeleteEntry (CDF, aOffset, eOffset)
struct CDFstruct *CDF;
Int32 aOffset;
Int32 eOffset;
{
  CDFstatus pStatus = CDF_OK;
  struct ADRstruct ADR;
  struct AEDRstruct AEDR, AEDRt;
  Int32 prevEntryOffset;
  Logical zEntry;
  /****************************************************************************
  * Read the ADR and the AEDR being deleted.
  ****************************************************************************/
  if (!sX(ReadADR(CDF->fp,aOffset,
		  ADR_RECORD,&ADR,
		  ADR_NULL),&pStatus)) return pStatus;
  if (!sX(ReadAEDR(CDF->fp,eOffset,
		   AEDR_RECORD,&AEDR,NULL,
		   AEDR_NULL),&pStatus)) return pStatus;
  zEntry = (AEDR.RecordType == AzEDR_);
  /****************************************************************************
  * Remove the AEDR from the list of entries.
  ****************************************************************************/
  if (!sX(FindPrevEntry(CDF,aOffset,eOffset,
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
    if (!sX(ReadAEDR(CDF->fp,prevEntryOffset,
		     AEDR_RECORD,&AEDRt,NULL,
		     AEDR_NULL),&pStatus)) return pStatus;
    AEDRt.AEDRnext = AEDR.AEDRnext;
    if (!sX(WriteAEDR(CDF,CDF->fp,prevEntryOffset,
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
    Int32 tOffset = BOO(zEntry,ADR.AzEDRhead,ADR.AgrEDRhead);
    while (tOffset != 0) {
      if (!sX(ReadAEDR(CDF->fp,tOffset,
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
  if (!sX(WriteADR(CDF->fp,aOffset,
		   ADR_RECORD,&ADR,
		   ADR_NULL),&pStatus)) return pStatus;
  if (!sX(WasteIR(CDF,eOffset,AEDR.RecordSize),&pStatus)) return pStatus;
  return pStatus;
}

/******************************************************************************
* CtoPstr/PtoCstr.
******************************************************************************/

#if defined(MPW_C)
STATICforIDL uChar *CtoPstr (string)
char *string;
{
  size_t length = MINIMUM(strlen(string),255);
  if (length > 0) memmove (&string[1], string, length);
  string[0] = (char) length;
  return ((uChar *) string);
}
#endif

#if defined(MPW_C)
STATICforIDL char *PtoCstr (string)
uChar *string;
{
  size_t length = (size_t) string[0];
  if (length > 0) memmove (string, &string[1], length);
  string[length] = (uChar) NUL;
  return ((char *) string);
}
#endif

/******************************************************************************
* PadUnRecords.
*   Pad all allocated records within the first/last record range.  Note that
* in the case of sparse records some of the records may not be allocated (and
* should not be padded of course).
******************************************************************************/

STATICforIDL CDFstatus PadUnRecords (CDF, Var, firstRec, lastRec)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 firstRec;
Int32 lastRec;
{
  CDFstatus pStatus = CDF_OK; Int32 offset; int how; void *buffer;
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
    if (!sX(NextRecord(CDF,Var->VDRoffset,Var->zVar,
		       firstRec,&recNum,&found),&pStatus)) return pStatus;
    if (!found) return pStatus;
    /**************************************************************************
    * While the first record to be padded is within the range...
    **************************************************************************/
    while (recNum <= lastRec) {
      if (!sX(RecordByteOffset(CDF,Var,recNum,
			       &offset),&pStatus)) return pStatus;
      if (!sX(SearchForRecord(CDF,Var->VDRoffset,
			      Var->zVar,recNum,
			      NULL,&lastRecInVVR,
			      NULL,NULL),&pStatus)) return pStatus;
      toRec = MINIMUM(lastRec,lastRecInVVR);
      nRecords = toRec - recNum + 1;
      if (!sX(BuildPadBuffer(CDF,Var,nRecords,
			     &how,&buffer,TRUE),&pStatus)) return pStatus;
      if (!sX(WritePadValues(Var,CDF->fp,offset,
			     nRecords,how,buffer),&pStatus)) {
	cdf_FreeMemory (buffer, NULL);
	return pStatus;
      }
      cdf_FreeMemory (buffer, NULL);
      recNum += nRecords;
      /************************************************************************
      * Determine the next `first' record to be padded.
      ************************************************************************/
      if (!sX(NextRecord(CDF,Var->VDRoffset,Var->zVar,
			 recNum,&recNum,&found),&pStatus)) return pStatus;
      if (!found) return pStatus;
    }
  }
  else {
    /**************************************************************************
    * Multi-file...
    **************************************************************************/
    if (!sX(RecordByteOffset(CDF,Var,
			     firstRec,
			     &offset),&pStatus)) return pStatus;
    nRecords = lastRec - firstRec + 1;
    if (!sX(BuildPadBuffer(CDF,Var,nRecords,
			   &how,&buffer,TRUE),&pStatus)) return pStatus;
    if (!sX(WritePadValues(Var,Var->fp,offset,nRecords,how,buffer),&pStatus)) {
      cdf_FreeMemory (buffer, NULL);
      return pStatus;
    }
    cdf_FreeMemory (buffer, NULL);
  }
  return pStatus;
}

/******************************************************************************
* WritePadValues.
*   NOTE: It is assumed that the records are contiguous (in a single-file CDF)
* and that on a DOS machine the buffer does not exceed 64K bytes.  It is also
* assumed that the values have already been encoded for the CDF.
******************************************************************************/

STATICforIDL CDFstatus WritePadValues (Var, fp, offset, nRecords, how, buffer)
struct VarStruct *Var;
vFILE *fp;
Int32 offset;
Int32 nRecords;
int how;
void *buffer;
{
  Int32 nBytes, nValues, recX, valueX;
  /****************************************************************************
  * Seek to desired offset.
  ****************************************************************************/
  if (!SEEKv(fp,(long)offset,vSEEK_SET)) return VAR_WRITE_ERROR;
  /****************************************************************************
  * Write records...
  ****************************************************************************/
  switch (how) {
    case ALLrecordsATonce:
      nBytes = nRecords * Var->NphyRecBytes;
      if (!WRITEv(buffer,(size_t)nBytes,1,fp)) return VAR_WRITE_ERROR;
      break;
    case ONErecordATaTIME:
      for (recX = 0; recX < nRecords; recX++) {
	 if (!WRITEv(buffer,(size_t)Var->NphyRecBytes,1,fp)) {
	   return VAR_WRITE_ERROR;
	 }
      }
      break;
    case ONEvalueATaTIME:
      nValues = nRecords * Var->NphyRecValues;
      for (valueX = 0; valueX < nValues; valueX++) {
	 if (!WRITEv(buffer,(size_t)Var->NvalueBytes,1,fp)) {
	   return VAR_WRITE_ERROR;
	 }
      }
      break;
  }
  return CDF_OK;
}

/******************************************************************************
* MinInt/Int32/Long.
* These are used to avoid double evaluation in macro expansions (eg. MINIMUM).
******************************************************************************/

VISIBLE_PREFIX int MinInt (a, b)
int a;
int b;
{
  return MINIMUM(a,b);
}

VISIBLE_PREFIX Int32 MinInt32 (a, b)
Int32 a;
Int32 b;
{
  return MINIMUM(a,b);
}

VISIBLE_PREFIX long MinLong (a, b)
long a;
long b;
{
  return MINIMUM(a,b);
}

/******************************************************************************
* MaxInt/Int32/Long.
* These are used to avoid double evaluation in macro expansions (eg. MAXIMUM).
******************************************************************************/

VISIBLE_PREFIX int MaxInt (a, b)
int a;
int b;
{
  return MAXIMUM(a,b);
}

VISIBLE_PREFIX Int32 MaxInt32 (a, b)
Int32 a;
Int32 b;
{
  return MAXIMUM(a,b);
}

VISIBLE_PREFIX long MaxLong (a, b)
long a;
long b;
{
  return MAXIMUM(a,b);
}

/******************************************************************************
* AddTOvStats.
******************************************************************************/

STATICforIDL void AddTOvStats (vStatsTO, vStats)
vSTATS *vStatsTO;	/* If NULL, do nothing. */
vSTATS *vStats;		/* If NULL, initialize (to zero). */
{
  if (vStatsTO != NULL) {
    if (vStats == NULL) {
      vStatsTO->nBuffers = 0;
      vStatsTO->maxBuffers = 0;
      vStatsTO->nV_reads = 0;
      vStatsTO->nV_writes = 0;
      vStatsTO->nPageIns = 0;
      vStatsTO->nPageOuts = 0;
      vStatsTO->nBlockReads = 0;
      vStatsTO->nBlockWrites = 0;
    }
    else {
      vStatsTO->nBuffers = MAXIMUM(vStatsTO->nBuffers,vStats->nBuffers);
      vStatsTO->maxBuffers = MAXIMUM(vStatsTO->maxBuffers,vStats->maxBuffers);
      vStatsTO->nV_reads += vStats->nV_reads;
      vStatsTO->nV_writes += vStats->nV_writes;
      vStatsTO->nPageIns += vStats->nPageIns;
      vStatsTO->nPageOuts += vStats->nPageOuts;
      vStatsTO->nBlockReads += vStats->nBlockReads;
      vStatsTO->nBlockWrites += vStats->nBlockWrites;
    }
  }
  return;
}

/******************************************************************************
* FillSpacesToString.
* This function scans for Nul in a string buffer. If found, it fills space(s)
* until the end of the number of elements. This happens when a string is
* entered with a length shorter than the defined number of elements. The
* buffer's size is a number of numElms, which could contain multiple strings.
******************************************************************************/
STATICforIDL void FillSpacesToString (buffer, totalBytes, numElems)
char  *buffer;
int   totalBytes;
int   numElems;
{
  int i, j, k, l;
  if ((totalBytes == numElems) && ((int)strlen(buffer) == totalBytes)) return; 
  l = totalBytes / numElems;
  for (i = 0; i < l; ++i) {
    if ((int)strlen(buffer+i*numElems) >= numElems) continue;
    for (j = 0; j < numElems; ++j) {
      if (*(buffer+i*numElems+j) == '\0') {
        for (k = j; k < numElems; ++k)
          *(buffer+i*numElems+k) = ' ';
        break;
      }
    }
  }
  return;
}
/******************************************************************************
* DisplayVs.
******************************************************************************/

#if defined(DEBUG)
STATICforIDL void DisplayVs (toWhere, label, vStats)
char *toWhere;
char *label;
vSTATS *vStats;
{
  if (toWhere != NULL) {
    FILE *fp = BOO(strcmp(toWhere,"STDOUT"),FOPEN(toWhere,"a"),stdout);
    if (fp == NULL) return;
    fprintf (fp, "%s", label);
    fprintf (fp, "..BUF:%03ldu/%03ldx", vStats->nBuffers, vStats->maxBuffers);
    fprintf (fp, " V_:%06ldr/%06ldw", vStats->nV_reads, vStats->nV_writes);
    fprintf (fp, " PAGE:%05ldi/%05ldo", vStats->nPageIns, vStats->nPageOuts);
    fprintf (fp, " BLK:%05ldr/%05ldw",
	     vStats->nBlockReads, vStats->nBlockWrites);
    fprintf (fp, "\n");
    if (fp != stdout) fclose (fp);
  }
  return;
}
#endif
