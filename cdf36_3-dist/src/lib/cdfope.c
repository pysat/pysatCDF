/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                        CDF `open' operations.
*
*  Version 1.4a, 21-Feb-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  20-May-92, J Love     Original version (was part of `cdflib.c').
*   V1.1  29-Sep-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V1.2  25-Jan-94, J Love     CDF V2.4.
*   V1.3  15-Dec-94, J Love     CDF V2.5.
*   V1.3a  9-Jan-95, J Love	Encode/decode changes, etc.
*   V1.3b 24-Feb-95, J Love	Solaris 2.3 IDL i/f.
*   V1.3c  8-May-95, J Love	Only check version/release for FUTURE_CDF.
*   V1.3d  7-Sep-95, J Love	CDFexport-related changes.  Fixed clean up
*				when a CDF is aborted.
*   V1.4  21-Aug-96, J Love	CDF V2.6.
*   V1.4a 21-Feb-97, J Love	Removed RICE.
*   V1.5  21-Jun-04, M Liu      Modified the error message for NOT_A_CDF.
*   V2.0  29-Jun-04, M Liu      Added support for LFS (Large File System >2G).
*   V1.7  13-Oct-06, M Liu      Changed to allow upper and lower case CDF  
*                               name to be used on win32.
*   V3.2  17-Jun-07, D Berger   Added setting of Cur->cdf and id prior to calls
*                               to ReadGDR in support of READONLY perfomance
*                               improvement changes.
*   V3.3  16-Jul-08, M Liu      Added ValidateCDF/ValidateCDF64 function calls
*                               after GDR is read.
*
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"
#include "cdfrev.h"

static CDFstatus ValidateCDFTT2000 (CDFid id, int zVars, int rVars);

/******************************************************************************
* CDFope.
******************************************************************************/

STATICforIDL CDFstatus CDFope (Va, Cur)
struct VAstruct *Va;
struct CurStruct *Cur;
{
CDFstatus pStatus = CDF_OK;

switch (Va->item) {
  /****************************************************************************
  * CDF_, open an existing CDF.
  ****************************************************************************/
  case CDF_: {
    char CDFnameT[CDF_PATHNAME_LEN+1], CDFnameX[DU_MAX_PATH_LEN+1];
    char CDFnameTx[CDF_PATHNAME_LEN+1];
    char *CDFnameP; struct CDFstruct *CDF;
    vFILE *dotFp, *uDotFp;
    size_t nBytes; int varN; CDFid *id;
    Logical upper_case_ext, version_numbers, no_append;
    Int32 magicNumber1, magicNumber2, CDRflags, version, release, increment;
    Logical largeFile;
    /**************************************************************************
    * Get arguments for this operation/item.
    **************************************************************************/
    CDFnameP = va_arg (Va->ap, char *);
    id = va_arg (Va->ap, CDFid *);
    *id = (CDFid) NULL;
    /**************************************************************************
    * Validate arguments.
    **************************************************************************/
    if (strlen(CDFnameP) > (size_t) CDF_PATHNAME_LEN) {
      if (!sX(CDF_NAME_TRUNC,&pStatus)) return pStatus;
    }
    strcpyX (CDFnameT, CDFnameP, CDF_PATHNAME_LEN);
#if STRIP_TRAILING_BLANKS_FROM_CDFPATH
    StripTrailingBlanks (CDFnameT);
#endif
#if defined(vms) || defined(dos)
    MakeUpperString (CDFnameT);
#endif
    RemoveCDFFileExtension(CDFnameT, CDFnameTx);
/*  if (!ValidCDFname(CDFnameTx)) return BAD_CDF_NAME; */
    if (!sX(FindCDF(CDFnameTx,&no_append,
		    &upper_case_ext,
		    &version_numbers),&pStatus)) return pStatus;
    /**************************************************************************
    * Open CDF file.
    **************************************************************************/
    BuildFilePath (CDFt, CDFnameTx, no_append, upper_case_ext, version_numbers,
		   INT32_ZERO, CDFnameX);
    if (!sX(CheckLFS(CDFnameX, &largeFile, NULL),&pStatus)) return pStatus; 
    if (!largeFile)
      dotFp = V_open (CDFnameX, READ_ONLY_a_mode);
    else
      dotFp = V_open64 (CDFnameX, READ_ONLY_a_mode);
    if (dotFp == NULL) return CDF_OPEN_ERROR;
    /**************************************************************************
    * Read the magic numbers.
    **************************************************************************/
    if (!largeFile) { /* 32-bit offset file.... */
      if (!Read32(dotFp,&magicNumber1)) {
        V_close (dotFp, NULL, NULL);
        return CDF_READ_ERROR;
      }
      if (!Read32(dotFp,&magicNumber2)) {
        V_close (dotFp, NULL, NULL);
        return CDF_READ_ERROR;
      }
    } else { /* 64-bit offset file.... */
      if (!Read32_64(dotFp,&magicNumber1)) {
        V_close64 (dotFp, NULL, NULL);
        return CDF_READ_ERROR;
      }     
      if (!Read32_64(dotFp,&magicNumber2)) {
        V_close64 (dotFp, NULL, NULL);
        return CDF_READ_ERROR;
      }       
    }
    /**************************************************************************
    * Determine what version CDF this is, if it is compressed (and if this is
    * actually a CDF).
    **************************************************************************/
    switch (magicNumber1) {
      case V1magicNUMBER_flip:
	if (!largeFile)
          V_close (dotFp, NULL, NULL);
        else
          V_close64 (dotFp, NULL, NULL);
	return ILLEGAL_ON_V1_CDF;
      case V2magicNUMBER_1pre:
	uDotFp = NULL;
	break;
      case V2magicNUMBER_1:
      case V3magicNUMBER_1:
	switch (magicNumber2) {
	  case V2magicNUMBER_2u: /* Same as V3magicNUMBER_2u */
	    if (magicNumber1 == V2magicNUMBER_1) { /* 32-offset */
	      if (!CACHEv(dotFp,NUMcacheUNKNOWN)) {
	        V_close (dotFp, NULL, NULL);
	        return BAD_CACHE_SIZE;
	      }
	    } else { /* 64-bit */
	      if (!CACHEv64(dotFp,NUMcacheUNKNOWN)) {
		V_close64 (dotFp, NULL, NULL);
		return BAD_CACHE_SIZE;
	      }
	    }
	    uDotFp = NULL;
	    break;
	  case V2magicNUMBER_2c:
	    uDotFp = V_scratch (ScratchDirectory(NULL), "cdf");
	    if (uDotFp == NULL) {
	      V_close (dotFp, NULL, NULL);
	      return CDF_CREATE_ERROR;
	    }
	    if (magicNumber1 == V2magicNUMBER_1) {
	      if (!CACHEv(uDotFp,NUMcacheUNKNOWN)) {
	        V_close (dotFp, NULL, NULL);
	        V_delete (uDotFp, NULL);
	        return BAD_CACHE_SIZE;
	      } 
              if (!sX(DecompressCDF(dotFp,uDotFp),&pStatus)) {
                V_close (dotFp, NULL, NULL);
                V_delete (uDotFp, NULL);
                return pStatus;
              }
	    } else {
              if (!CACHEv64(uDotFp,NUMcacheUNKNOWN)) {
                V_close64 (dotFp, NULL, NULL);
                V_delete64 (uDotFp, NULL);
                return BAD_CACHE_SIZE;
              }
	      if (!sX(DecompressCDF64(dotFp,uDotFp),&pStatus)) {
	        V_close64 (dotFp, NULL, NULL);
	        V_delete64 (uDotFp, NULL);
	        return pStatus;
	      }
	    }
	    break;
	  default:
	    if (!largeFile)
	        V_close (dotFp, NULL, NULL);
	    else
	        V_close64 (dotFp, NULL, NULL);
	    return NOT_A_CDF;
	}
	break;
      default:
	if (!largeFile)
          V_close (dotFp, NULL, NULL);
        else
          V_close64 (dotFp, NULL, NULL);
        if (magicNumber1 == netCDFmagicNUMBER1 || 
            magicNumber1 == netCDFmagicNUMBER2)
          return IS_A_NETCDF;
        else
	  return NOT_A_CDF_OR_NOT_SUPPORTED;
    }
    /**************************************************************************
    * Allocate and begin initializing CDF structure.
    **************************************************************************/
    CDF = (struct CDFstruct *) cdf_AllocateMemory (
					(size_t)sizeof(struct CDFstruct), NULL);
    if (CDF == NULL) {
      if (!largeFile) {
        V_close (dotFp, NULL, NULL);
	if (uDotFp != NULL) V_delete (uDotFp, NULL);
      } else {
        V_close64 (dotFp, NULL, NULL);
	if (uDotFp != NULL) V_delete64 (uDotFp, NULL);
      } 
      return BAD_MALLOC;
    }
    CDF->CDFname = (char *) cdf_AllocateMemory (
					(size_t)strlen(CDFnameTx) + 1, NULL);
    if (CDF->CDFname == NULL) {
      if (!largeFile) {
        V_close (dotFp, NULL, NULL);
	if (uDotFp != NULL) V_delete (uDotFp, NULL);
      } else {
        V_close64 (dotFp, NULL, NULL);
	if (uDotFp != NULL) V_delete64 (uDotFp, NULL);
      } 
      cdf_FreeMemory (CDF, NULL);
      return BAD_MALLOC;
    }
    else
      strcpyX (CDF->CDFname, CDFnameTx, 0);
    CDF->magic = VALIDid_MAGIC_NUMBER;
    CDF->largeFile = largeFile;
    CDF->dotFp = dotFp;
    CDF->uDotFp = uDotFp;
    CDF->fp = BOO(uDotFp == NULL,dotFp,uDotFp);
    CDF->no_append = no_append;
    CDF->upper_case_ext = upper_case_ext;
    CDF->version_numbers = version_numbers;
    CDF->decoding = HOST_DECODING;
    CDF->readOnly = FALSE;
    CDF->zMode = zMODEoff;
    CDF->negToPosFp0 = FALSE;
    CDF->status = READ_ONLY;
CDF->tt2000Updated = 0;
    CDF->pseudo_clock = 0;
    CDF->stage.fp = NULL;
    CDF->stage.mark = ZERO_OFFSET;
    CDF->stage.mark64 = (OFF_T) ZERO_OFFSET64;
    CDF->stage.cacheSize = NUMcacheSTAGE;
    CDF->compressCacheSize = NUMcacheCOMPRESS;
    CDF->compressFp = NULL;
    CDF->scratchDir = NULL;
    CDF->NrVars = 0;		/* Updated below if the read is OK. */
    CDF->NzVars = 0;		/* Updated below if the read is OK. */
    CDF->rVars = NULL;
    CDF->zVars = NULL;
    CDF->CURzVarNum = RESERVED_VARNUM;
    CDF->CURrVarNum = RESERVED_VARNUM;
    CDF->CURzVarOffset = 0;
    CDF->CURzVarOffset64 = (OFF_T) 0;
    CDF->CURrVarOffset = 0;
    CDF->CURrVarOffset64 = (OFF_T) 0;
    /**************************************************************************
    * Read necessary fields from the CDR and GDR.
    **************************************************************************/
    if (!largeFile) {
      CDF->CDRoffset = V2_CDR_OFFSET;
      if (!sX(ReadCDR(CDF->fp,CDF->CDRoffset,
		      CDR_GDROFFSET,&(CDF->GDRoffset),
		      CDR_ENCODING,&(CDF->encoding),
		      CDR_FLAGS,&CDRflags,
		      CDR_VERSION,&version,
		      CDR_RELEASE,&release,
		      CDR_INCREMENT,&increment,
		      CDR_NULL),&pStatus)) {
        AbortAccess (CDF, noUPDATE, noDELETE);
        cdf_FreeMemory (CDF, NULL);
        return pStatus;
      }
      /************************************************************************
       * Set state info for use in ReadGDR - required by changes to support
       * READONLY performance enhancement.
       ***********************************************************************/
      Cur->cdf = CDF;
      *id = CDF;
      if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		      GDR_EOF,&(CDF->eof),
		      GDR_NrVARS,&(CDF->NrVars),
		      GDR_NzVARS,&(CDF->NzVars),
		      GDR_rMAXREC,&(CDF->rMaxRec),
		      GDR_NULL),&pStatus)) {
        AbortAccess (CDF, noUPDATE, noDELETE);
        cdf_FreeMemory (CDF, NULL);
        Cur->cdf = NULL;
        *id = (CDFid) NULL;
        return pStatus;
      }
    } else {
      /************************************************************************
       * Set state info for use in ReadGDR - required by changes to support
       * READONLY performance enhancement.
       ***********************************************************************/
      Cur->cdf = CDF;
      *id = CDF;
      CDF->CDRoffset64 = (OFF_T) V3_CDR_OFFSET64;
      if (!sX(ReadCDR64(CDF->fp,CDF->CDRoffset64,
                        CDR_GDROFFSET,&(CDF->GDRoffset64),
                        CDR_ENCODING,&(CDF->encoding),
                        CDR_FLAGS,&CDRflags,
                        CDR_VERSION,&version,
	                CDR_RELEASE,&release,
	                CDR_INCREMENT,&increment,
	                CDR_NULL),&pStatus)) {
        AbortAccess64 (CDF, noUPDATE, noDELETE);
        cdf_FreeMemory (CDF, NULL);
        Cur->cdf = NULL;
        *id = (CDFid) NULL;
        return pStatus;
      }

      /************************************************************************
       * Set state info for use in ReadGDR - required by changes to support
       * READONLY performance enhancement.
       ***********************************************************************/
      Cur->cdf = CDF;
      *id = CDF;
      if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
			GDR_EOF,&(CDF->eof64),
                        GDR_NrVARS,&(CDF->NrVars),
                        GDR_NzVARS,&(CDF->NzVars),
                        GDR_rMAXREC,&(CDF->rMaxRec),
                        GDR_NULL),&pStatus)) {
        AbortAccess64 (CDF, noUPDATE, noDELETE);
        cdf_FreeMemory (CDF, NULL);
        Cur->cdf = NULL;
        *id = (CDFid) NULL;
        return pStatus;
      }
    }
    /**************************************************************************
    * Continue initializing CDF structure.
    **************************************************************************/
    CDF->singleFile = SINGLEfileBITset (CDRflags);
    CDF->rowMajor = ROWmajorBITset (CDRflags);
    CDF->checksum = (PriorTo("3.0.0",version,release,increment) ?
                     NO_CHECKSUM : CDFChecksumMethod (CDRflags));
    CDF->fakeEPOCH = PriorTo("2.1.1",version,release,increment);
    CDF->wastedSpace = PriorTo("2.5",version,release,increment);
    CDF->badEOF = PriorTo("2.1",version,release,increment);
    CDF->badTerminatingOffsets = PriorTo("2.1",version,release,increment);
    CDF->assumedScopes = PriorTo("2.5",version,release,increment);
    CDF->workingCacheSize = BOO(CDF->singleFile,NUMcacheSINGLE,NUMcacheMULTI);
    /************************************************************************
     * Validate the CDF to make sure that certain data fields are normal by
     * doing some sanity checks.
     ***********************************************************************/
    if (CDFgetValidate()) {
      Logical debug;
      if (CDFgetValidateDebug())
        debug = TRUE;
      else
        debug = FALSE;
      if (!largeFile) {
        pStatus = ValidateCDF(CDF,CDF->fp,CDF->CDRoffset,CDF->eof, debug);
        if (pStatus != CDF_OK) {
          AbortAccess (CDF, noUPDATE, noDELETE);
          cdf_FreeMemory (CDF, NULL);
          Cur->cdf = NULL;
          *id = (CDFid) NULL;
          return pStatus;
        }   
      } else {
        pStatus = ValidateCDF64(CDF,CDF->fp,CDF->CDRoffset64,CDF->eof64, debug);
        if (pStatus != CDF_OK) {
          AbortAccess64 (CDF, noUPDATE, noDELETE);
          cdf_FreeMemory (CDF, NULL);
          Cur->cdf = NULL;
          *id = (CDFid) NULL;
          if (pStatus == CORRUPTED_V3_CDF) {
            char vri[6];
            long libver, librel, libinc;
            CDFstatus status;
            snprintf (vri, (size_t) 6, "%1.1d.%1.1d.%1.1d", (int) version,
                      (int) release, (int) increment);
            status = CDFlib (GET_, LIB_VERSION_, &libver,
                                   LIB_RELEASE_, &librel,
                                   LIB_INCREMENT_, &libinc,
                             NULL_);
            if (PriorTo(vri, (Int32)libver, (Int32)librel, (Int32)libinc) &&
                PriorTo("3.5.0", (Int32)libver, (Int32)librel, (Int32)libinc))
              return UNABLE_TO_PROCESS_CDF;
            else
              return pStatus;
          } else
            return pStatus;
        }
      }
    }
    /**************************************************************************
    * Acquire some more info from the CDF.
    **************************************************************************/
    if (!largeFile) {
      CDF->CDRoffset = V2_CDR_OFFSET;
      if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
                      GDR_rNUMDIMS,&(CDF->rNumDims),
                      GDR_rDIMSIZES,CDF->rDimSizes,
                      GDR_NULL),&pStatus)) {
        AbortAccess (CDF, noUPDATE, noDELETE);
        cdf_FreeMemory (CDF, NULL);
        Cur->cdf = NULL;
        *id = (CDFid) NULL;
        return pStatus;
      }
    } else {
      CDF->CDRoffset64 = (OFF_T) V3_CDR_OFFSET64;
      if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
                        GDR_rNUMDIMS,&(CDF->rNumDims),
                        GDR_rDIMSIZES,CDF->rDimSizes,
                        GDR_NULL),&pStatus)) {
        AbortAccess64 (CDF, noUPDATE, noDELETE);
        cdf_FreeMemory (CDF, NULL);
        Cur->cdf = NULL;
        *id = (CDFid) NULL;
        return pStatus;
      }
    }
    /**************************************************************************
    * Set the cache size for the "working" dotCDF file based on the format
    * of the CDF.
    **************************************************************************/
    if (!largeFile) {
      if (!CACHEv(CDF->fp,CDF->workingCacheSize)) {
        AbortAccess (CDF, noUPDATE, noDELETE);
        cdf_FreeMemory (CDF, NULL);
        return BAD_CACHE_SIZE;
      }
    } else {
      if (!CACHEv64(CDF->fp,CDF->workingCacheSize)) {
        AbortAccess64 (CDF, noUPDATE, noDELETE);
        cdf_FreeMemory (CDF, NULL);
        return BAD_CACHE_SIZE;
      }
    }
    /**************************************************************************
    * Allocate and initialize variable data structures kept in memory.
    **************************************************************************/
    if (CDF->NrVars > 0) {
      nBytes = (size_t) (CDF->NrVars * sizeof(struct VarStruct *));
      CDF->rVars = (struct VarStruct **) cdf_AllocateMemory (nBytes, NULL);
      if (CDF->rVars == NULL) {
        if (!largeFile) 	
	  AbortAccess (CDF, noUPDATE, noDELETE);
        else
          AbortAccess64 (CDF, noUPDATE, noDELETE);
        cdf_FreeMemory (CDF, NULL);
	return BAD_MALLOC;
      }
      for (varN = 0; varN < CDF->NrVars; varN++) CDF->rVars[varN] = NULL;
      CDF->MAXrVars = (int) CDF->NrVars;
    }
    else {
      CDF->rVars = NULL;
      CDF->MAXrVars = 0;
    }
    if (CDF->NzVars > 0) {
      nBytes = (size_t) (CDF->NzVars * sizeof(struct VarStruct *));
      CDF->zVars = (struct VarStruct **) cdf_AllocateMemory (nBytes, NULL);
      if (CDF->zVars == NULL) {
        if (!largeFile)
	  AbortAccess (CDF, noUPDATE, noDELETE);
        else
          AbortAccess64 (CDF, noUPDATE, noDELETE);
        cdf_FreeMemory (CDF, NULL);
	return BAD_MALLOC;
      }
      for (varN = 0; varN < CDF->NzVars; varN++) CDF->zVars[varN] = NULL;
      CDF->MAXzVars = (int) CDF->NzVars;
    }
    else {
      CDF->zVars = NULL;
      CDF->MAXzVars = 0;
    }
    /**************************************************************************
    * If this is a multi-file CDF and a CDF pathname that doesn't require file
    * extensions to be appended was specified (ie. a weird naming convention),
    * close/free the CDF and return an error.  This is because the extensions
    * for the variable files would be known only to the user.  We'd rather not
    * guess.
    **************************************************************************/
    if (!CDF->singleFile && CDF->no_append) {
      if (!largeFile)
        AbortAccess (CDF, noUPDATE, noDELETE);
      else
        AbortAccess64 (CDF, noUPDATE, noDELETE);
      cdf_FreeMemory (CDF, NULL);
      return BAD_CDF_EXTENSION;
    }
    /**************************************************************************
    * Initialize the current objects/states and the Vstream statistics.
    **************************************************************************/
    InitCURobjectsStates (CDF);
    AddTOvStats (&CDF->dotCDFvStats, NULL);
    AddTOvStats (&CDF->uDotCDFvStats, NULL);
    /**************************************************************************
    * Select the current CDF and pass back the CDF identifier.
    **************************************************************************/
    Cur->cdf = CDF;
    *id = CDF;
    if (CDF->singleFile && CDF->checksum != NO_CHECKSUM) {
      if (!sX(CDFconfirmChecksum(*id),&pStatus)) {
        if (!largeFile) 
          AbortAccess (CDF, noUPDATE, noDELETE);
	else
	  AbortAccess64 (CDF, noUPDATE, noDELETE);
        cdf_FreeMemory (CDF, NULL);
        Cur->cdf = NULL;
        *id = (CDFid) NULL;
        return pStatus;
      }
    }
    if (CDFgetValidate() && (!PriorTo ("3.5.0", version, release, increment))) {
        if (!sX(ValidateCDFTT2000 (*id, (int) CDF->NzVars, (int) CDF->NrVars),
                &pStatus)) {
          AbortAccess (CDF, noUPDATE, noDELETE);
          cdf_FreeMemory (CDF, NULL);
          Cur->cdf = NULL;
          *id = (CDFid) NULL;
          return pStatus;
        }
    }
    AddOpenCDFsCount();
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
* ValidateCDFTT2000.
******************************************************************************/

static CDFstatus ValidateCDFTT2000 (CDFid id, int zVars, int rVars)
{
  long maxRec, dataType;
  CDFstatus status;
  int i, j, count, tt2000Sts;
  long long tt2000 = DEFAULT_TT2000_PADVALUE;
  void *buffer;
  long year, month, day, tt2000LastUpdated, numDims, dimSizes[CDF_MAX_DIMS];
  double yy, mm, dd, hh, mn, ss, ml, ms, ns;
  int once;

  status = CDFgetLeapSecondLastUpdated (id, &tt2000LastUpdated);
  if (status != CDF_OK) return status;
#if defined(vms)
  CDFgetLastDateinLeapSecondsTBL (&year, &month, &day);
#else
  CDFgetLastDateinLeapSecondsTable (&year, &month, &day);
#endif
  if (tt2000LastUpdated == 0 ||
      tt2000LastUpdated == (10000*year+100*month+day)) return CDF_OK;
  once = 0;
  for (i = 0; i < zVars; ++i) {
    status = CDFlib(SELECT_, zVAR_, (long) i,
                    GET_, zVAR_DATATYPE_, &dataType,
                    NULL_);
    if (status != CDF_OK) return status;
    if (dataType != CDF_TIME_TT2000) continue;
    status = CDFlib(GET_, zVAR_MAXREC_, &maxRec,
                    NULL_);
    if (status != CDF_OK) return status;
    if (maxRec < 0) continue;
    /* If the file's last updated leap second date is later than table's, 
       then, the table is out-dated. */
    if ((once == 0) && (tt2000LastUpdated > (10000*year+100*month+day)))
      return TT2000_USED_OUTDATED_TABLE;
    else once = 1;
    status = CDFlib(GET_, zVAR_NUMDIMS_, &numDims,
                          zVAR_DIMSIZES_, dimSizes,
                    NULL_);
    if (status != CDF_OK) return status;
    count = (int) (maxRec + 1);
    if (numDims > 0)
      for (j = 0; j < (int) numDims; ++j) count *= (int) dimSizes[j];
    buffer = (void *) malloc ((size_t) (count * sizeof(long long)));
    if (buffer == NULL) return BAD_MALLOC;
    status =  CDFgetVarAllRecordsByVarID (id, 1, (long) i, buffer);
    if (status < CDF_OK) {
      free (buffer);
      return status;
    } 
    for (j = 0; j < count; ++j) {
      if (tt2000 < *(((long long *)buffer)+j))
        tt2000 = *(((long long *)buffer)+j);
    }
    free (buffer);
  }
  status = CDFlib(GET_, rVARs_NUMDIMS_, &numDims,
                        rVARs_DIMSIZES_, dimSizes,
                  NULL_);
  if (status != CDF_OK) return status;
  for (i = 0; i < rVars; ++i) {
    status = CDFlib(SELECT_, rVAR_, (long) i,
                    GET_, rVAR_DATATYPE_, &dataType,
                    NULL_);
    if (status != CDF_OK) return status;
    if (dataType != CDF_TIME_TT2000) continue;
    status = CDFlib(GET_, rVAR_MAXREC_, &maxRec,
                    NULL_);
    if (status != CDF_OK) return status;
    if (maxRec < 0) continue;
    if ((once == 0) && (tt2000LastUpdated > (10000*year+100*month+day)))
      return TT2000_USED_OUTDATED_TABLE;
    else once = 1;
    count = (int) (maxRec + 1);
    if (numDims > 0) 
      for (j = 0; j < (int) numDims; ++j) count *= (int) dimSizes[j];
    buffer = (void *) malloc ((size_t) (count * sizeof(long long)));
    if (buffer == NULL) return BAD_MALLOC;
    status =  CDFgetVarAllRecordsByVarID (id, 0, (long) i, buffer);
    if (status < CDF_OK) {
      free (buffer);
      return status;
    }
    for (j = 0; j < count; ++j) {
      if (tt2000 < *(((long long *)buffer)+j))
        tt2000 = *(((long long *)buffer)+j);
    }
    free (buffer);
  }
  if (tt2000 != DEFAULT_TT2000_PADVALUE) {
    breakdownTT2000 (tt2000, &yy, &mm, &dd, &hh, &mn, &ss, &ml, &ms, &ns);
    tt2000Sts = ValidateTT2000((int) (10000*yy+100*mm+dd),
                               (int)tt2000LastUpdated);
    if (tt2000Sts == -1) return TT2000_USED_OUTDATED_TABLE;
    else if (tt2000Sts == 0) return TT2000_CDF_MAYNEEDUPDATE;
  }
  return CDF_OK;
}


