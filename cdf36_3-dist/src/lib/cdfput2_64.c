/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                    CDF `put' operations, part 2.
*
*  Version 1.4a, 21-Feb-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  20-May-92, J Love     Original version (was part of `cdflib.c').
*   V1.1  16-Sep-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V1.2  24-Jan-94, J Love     CDF V2.4.
*   V1.3   5-Dec-94, J Love     CDF V2.5.
*   V1.3a  6-Jan-95, J Love     Encode/decode changes.  More cache-residency.
*   V1.3b 24-Feb-95, J Love     Solaris 2.3 IDL i/f.
*   V1.3c  4-Aug-95, J Love     CDFexport-related changes.
*   V1.4   3-Oct-96, J Love     CDF V2.6.
*   V1.4a 21-Feb-97, J Love	Removed RICE.
*   V2.0  08-Apr-04, M Liu      Removed calls to LocateCurrentVar function as
*                               its offset becomes available when it is
*                               selected/created. 
*   V2.1  29-Jun-04, M Liu      Added LFS (Large File Support > 2G).
*   V2.2  16-Nov-05, M Liu      Changed code for data spec modification. -99
*                               is passed as a special indicator for the 
*                               number of elements as it can't be changed.
*   V2.3  17-Oct-12, M Liu      Not allowed to set SPARSEREC if a variable
*                               is non-record varying.
*
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"

/******************************************************************************
* CDFput2_64.
******************************************************************************/

STATICforIDL CDFstatus CDFput2_64 (Va, Cur)
struct VAstruct *Va;
struct CurStruct *Cur;
{
CDFstatus tStatus, pStatus = CDF_OK;

switch (Va->item) {
  /****************************************************************************
  * CDF_ENCODING_, 
  *   Can't change if any variables have been written to (including pad
  *   values).  Can't change if any attribute entries have been written.
  ****************************************************************************/
  case CDF_ENCODING_: {
    struct CDFstruct *CDF;
    Int32 actualEncoding;
    Int32 encoding = (Int32) va_arg (Va->ap, long);
    Logical no;
    SelectCDF (Cur->cdf, CDF)
    if (!ValidEncoding(encoding,&actualEncoding)) return BAD_ENCODING;
    if (!sX(VerifyNoRecordsWritten64(CDF,&no),&pStatus)) return pStatus;
    if (!no) return CANNOT_CHANGE;
    if (!sX(VerifyNoPadsSpecified64(CDF,&no),&pStatus)) return pStatus;
    if (!no) return CANNOT_CHANGE;
    if (!sX(VerifyNoEntriesWritten64(CDF,&no),&pStatus)) return pStatus;
    if (!no) return CANNOT_CHANGE;
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    CDF->encoding = actualEncoding;
    /**************************************************************************
    * Update the `encoding' field in the CDR.
    **************************************************************************/
    if (!sX(WriteCDR64(CDF->fp,V3_CDR_OFFSET64,
		       CDR_ENCODING,&(CDF->encoding),
		       CDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Update the initialized variables for the new encoding.
    **************************************************************************/
    if (!sX(UpdateConversions64(CDF),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    break;
  }
  /****************************************************************************
  * CDF_MAJORITY_, 
  *   Can't change if any variable values have been written.
  ****************************************************************************/
  case CDF_MAJORITY_: {
    struct CDFstruct *CDF;
    long majority = va_arg (Va->ap,long);
    Logical no, zVar; Int32 CDRflags;
    SelectCDF (Cur->cdf, CDF)
    if (majority != ROW_MAJOR && majority != COLUMN_MAJOR) return BAD_MAJORITY;
    if (!sX(VerifyNoRecordsWritten64(CDF,&no),&pStatus)) return pStatus;
    if (!no) return CANNOT_CHANGE;
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    CDF->rowMajor = ROWmajor(majority);
    /**************************************************************************
    * Update the `flags' field in the CDR.
    **************************************************************************/
    if (!sX(ReadCDR64(CDF->fp,CDF->CDRoffset64,
		      CDR_FLAGS,&CDRflags,
		      CDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (CDF->rowMajor)
      SetBit32 (&CDRflags, CDR_MAJORITY_BIT);
    else
      ClearBit32 (&CDRflags, CDR_MAJORITY_BIT);
    if (!sX(WriteCDR64(CDF->fp,V3_CDR_OFFSET64,
		       CDR_FLAGS,&CDRflags,
		       CDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Update the initialized variables for the new majority.
    **************************************************************************/
    for (zVar = 0; zVar <= 1; zVar++) {
       int varN; Int32 nVars = BOO(zVar,CDF->NzVars,CDF->NrVars);
       for (varN = 0; varN < nVars; varN++) {
	  struct VarStruct *Var = BOO(zVar,CDF->zVars[varN],CDF->rVars[varN]);
          if (Var != NULL) CalcNumDimValues (CDF, Var);
       }
    }
    break;
  }
  /****************************************************************************
  * CDF_FORMAT_, 
  * Can't change if any variables have been created or if a compressed
  * single-file CDF.
  ****************************************************************************/
  case CDF_FORMAT_: {
    long format = va_arg (Va->ap,long);
    struct CDFstruct *CDF; Int32 CDRflags;
    SelectCDF (Cur->cdf, CDF)
    if (format != SINGLE_FILE && format != MULTI_FILE) return BAD_FORMAT;
    if (CDF->NrVars > 0) return CANNOT_CHANGE;
    if (CDF->NzVars > 0) return CANNOT_CHANGE;
    if (CDF->uDotFp != NULL) return CANNOT_CHANGE;
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    if (format == MULTI_FILE && CDF->checksum != NO_CHECKSUM)
      return CANNOT_CHANGE;
    CDF->singleFile = (format == SINGLE_FILE);
    /**************************************************************************
    * Update the `flags' field in the CDR.
    **************************************************************************/
    if (!sX(ReadCDR64(CDF->fp,CDF->CDRoffset64,
		      CDR_FLAGS,&CDRflags,
		      CDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (CDF->singleFile)
      SetBit32 (&CDRflags, CDR_FORMAT_BIT);
    else
      ClearBit32 (&CDRflags, CDR_FORMAT_BIT);
    if (!sX(WriteCDR64(CDF->fp,V3_CDR_OFFSET64,
		       CDR_FLAGS,&CDRflags,
		       CDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Reset cache size for "working" dotCDF file to the default for the CDF's
    * format.
    **************************************************************************/
    CDF->workingCacheSize = BOO(CDF->singleFile,NUMcacheSINGLE,NUMcacheMULTI);
    if (!CACHEv64(CDF->fp,CDF->workingCacheSize)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return BAD_CACHE_SIZE;
    }
    break;
  }
  /****************************************************************************
  * CDF_COMPRESSION_
  ****************************************************************************/
  case CDF_COMPRESSION_: {
    long cType = va_arg (Va->ap, long);         /* Compression type. */
    long *cParms = va_arg (Va->ap, long *);     /* Compression parameters. */
    struct CDFstruct *CDF; int parmN; vSTATS vStats;
    SelectCDF (Cur->cdf, CDF)
    /**************************************************************************
    * Validate compression type/parameters.
    **************************************************************************/
    if (!sX(ValidateCompression64(cType,cParms),&pStatus)) return pStatus;
    /**************************************************************************
    * Verify that compression is not being attempted on a multi-file CDF.  (It
    * is allowed to specify no compression.)
    **************************************************************************/
    if (!CDF->singleFile) {
      if (cType != NO_COMPRESSION) return CANNOT_COMPRESS;
      break;
    }
    /**************************************************************************
    * Depending on the type of compression...
    **************************************************************************/
    switch (cType) {
      /************************************************************************
      * Turning off compression.
      ************************************************************************/
      case NO_COMPRESSION:
	/**********************************************************************
	* If the CDF is currently compressed...
	**********************************************************************/
	if (CDF->uDotFp != NULL) {
	  char pathName[DU_MAX_PATH_LEN+1];
	  /********************************************************************
	  * First get write access.
	  ********************************************************************/
	  if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
	  /********************************************************************
	  * Delete the compressed dotCDF file.
	  ********************************************************************/
	  if (!DELETEv64(CDF->dotFp,&vStats)) {
	    CDF->dotFp = NULL;
	    AbortAccess64 (CDF, UPDATE, noDELETE);
	    return CDF_CLOSE_ERROR;
	  }
	  CDF->dotFp = NULL;
	  AddTOvStats (&CDF->dotCDFvStats, &vStats);
#if defined(DEBUG)
	  DisplayVs (getenv("VSTREAM_STATS"), "DotCDF..", &vStats);
#endif
	  /********************************************************************
	  * Recreate the dotCDF file (now uncompressed) and set its cache size
	  * to that of the "working" dotCDF file (which it now has become).
	  ********************************************************************/
	  BuildFilePath (CDFt, CDF->CDFname, CDF->no_append,
			 CDF->upper_case_ext, CDF->version_numbers,
			 INT32_ZERO, pathName);
	  CDF->dotFp = V_open64 (pathName, WRITE_PLUS_a_mode);
	  if (CDF->dotFp == NULL) {
	    AbortAccess64 (CDF, UPDATE, noDELETE);
	    return CDF_CREATE_ERROR;
	  }
	  if (!CACHEv64(CDF->dotFp,CDF->workingCacheSize)) {
	    AbortAccess64 (CDF, UPDATE, noDELETE);
	    return BAD_CACHE_SIZE;
	  }
	  /********************************************************************
	  * Copy the uncompressed dotCDF file to the recreated dotCDF file.
	  ********************************************************************/
	  if (!sX(CopyCDF64(CDF->uDotFp,CDF->dotFp),&pStatus)) {
	    AbortAccess64 (CDF, UPDATE, noDELETE);
	    return pStatus;
	  }
	  /********************************************************************
	  * Flush the recreated dotCDF file.
	  ********************************************************************/
	  if (!FLUSHv64(CDF->dotFp)) {
	    AbortAccess64 (CDF, UPDATE, noDELETE);
	    return CDF_WRITE_ERROR;
	  }
	  /********************************************************************
	  * Delete the uncompressed dotCDF file.
	  ********************************************************************/
	  if (!DELETEv64(CDF->uDotFp,&vStats)) {
	    CDF->uDotFp = NULL;
	    AbortAccess64 (CDF, UPDATE, noDELETE);
	    return SCRATCH_DELETE_ERROR;
	  }
	  CDF->uDotFp = NULL;
	  AddTOvStats (&CDF->uDotCDFvStats, &vStats);
#if defined(DEBUG)
	  DisplayVs (getenv("VSTREAM_STATS"), "uDotCDF.", &vStats);
#endif
	  /********************************************************************
	  * Reset the "working" dotCDF file.
	  ********************************************************************/
	  CDF->fp = CDF->dotFp;
	}
	break;
      /************************************************************************
      * Turning on/changing compression.
      ************************************************************************/
      case RLE_COMPRESSION:
      case HUFF_COMPRESSION:
      case AHUFF_COMPRESSION:
      case GZIP_COMPRESSION: {
	char pathName[DU_MAX_PATH_LEN+1]; struct CPRstruct64 CPR; Int32 pCount;
	/**********************************************************************
	* Get read/write access.
	**********************************************************************/
	if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
	/**********************************************************************
	* If the CDF is not currently compressed, create an uncompressed dotCDF
	* file and copy the current contents into it.
	**********************************************************************/
	if (CDF->uDotFp == NULL) {
	  CDF->uDotFp = V_scratch (ScratchDirectory(CDF), "cdf");
	  if (CDF->uDotFp == NULL) {
	    AbortAccess64 (CDF, UPDATE, noDELETE);
	    return CDF_CREATE_ERROR;
	  }
	  if (!CACHEv64(CDF->uDotFp,CDF->workingCacheSize)) {
	    AbortAccess64 (CDF, UPDATE, noDELETE);
	    return BAD_CACHE_SIZE;
	  }
	  if (!sX(CopyCDF64(CDF->dotFp,CDF->uDotFp),&pStatus)) {
	    AbortAccess64 (CDF, UPDATE, noDELETE);
	    return pStatus;
	  }
	  if (!FLUSHv64(CDF->uDotFp)) {
	    AbortAccess64 (CDF, UPDATE, noDELETE);
	    return CDF_WRITE_ERROR;
	  }
	  CDF->fp = CDF->uDotFp;
	}
	/**********************************************************************
	* Delete/recreate the dotCDF file and write a CCR and CPR.
	**********************************************************************/
	if (!DELETEv64(CDF->dotFp,&vStats)) {
	  CDF->dotFp = NULL;
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return CDF_DELETE_ERROR;
	}
	CDF->dotFp = NULL;
	AddTOvStats (&CDF->dotCDFvStats, &vStats);
#if defined(DEBUG)
	DisplayVs (getenv("VSTREAM_STATS"), "DotCDF..", &vStats);
#endif
	BuildFilePath (CDFt, CDF->CDFname, CDF->no_append,
		       CDF->upper_case_ext, CDF->version_numbers,
		       INT32_ZERO, pathName);
	CDF->dotFp = V_open64 (pathName, WRITE_PLUS_a_mode);
	if (CDF->dotFp == NULL) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return CDF_CREATE_ERROR;
	}
	pCount = CompressionParmsCount ((Int32) cType);
	CPR.RecordSize = CPR_BASE_SIZE64 + (pCount * sizeof(Int32));
	CPR.RecordType = CPR_;
	CPR.rfuA = 0;
	CPR.cType = (Int32) cType;
	CPR.pCount = pCount;
	for (parmN = 0; parmN < pCount; parmN++) {
	   CPR.cParms[parmN] = (Int32) cParms[parmN];
	}
	if (!sX(WriteCompressedCDF64(CDF,&CPR,EMPTY),&pStatus)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
	if (!FLUSHv64(CDF->dotFp)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return CDF_WRITE_ERROR;
	}
	break;
      }
    }
    break;
  }
  /****************************************************************************
  * r/zVAR_COMPRESSION_
  ****************************************************************************/
  case rVAR_COMPRESSION_:
  case zVAR_COMPRESSION_: {
    Logical zOp = (Va->item == zVAR_COMPRESSION_), zVar;
    Int32 maxAllocated, pCount; int p; OFF_T VDRoffset;
    struct CDFstruct *CDF; struct VarStruct *Var;
    struct VDRstruct64 VDR; struct CPRstruct64 CPR;
    long cType = va_arg (Va->ap, long);         /* Compression type. */
    long *cParms = va_arg (Va->ap, long *);     /* Compression parameters. */
    /**************************************************************************
    * Verify the current CDF and variable and that this operation is legal for
    * the current zMode.
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    /**************************************************************************
    * Validate compression type/parameters.
    **************************************************************************/
    if (!sX(ValidateCompression64(cType,cParms),&pStatus)) return pStatus;
    /**************************************************************************
    * Get write access.
    **************************************************************************/
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    /**************************************************************************
    * Verify that compression isn't being attempted on a multi-file CDF.  (It
    * is allowed to specify no compression.)  After this point a single-file
    * CDF can be assumed.
    **************************************************************************/
    if (!CDF->singleFile) {
      if (cType != NO_COMPRESSION) return CANNOT_COMPRESS;
      break;
    }
    /**************************************************************************
    * Locate the current variable.
    **************************************************************************/

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) VDRoffset = CDF->CURzVarOffset64;
    else VDRoffset = CDF->CURrVarOffset64;

    if (zModeON(CDF))
      if (CDF->CURzVarNum < CDF->NrVars)
        Var = CDF->rVars[(int)CDF->CURzVarNum];
      else
        Var = CDF->zVars[(int)(CDF->CURzVarNum - CDF->NrVars)];
    else
        Var = BOO(zOp,CDF->zVars[(int)CDF->CURzVarNum],
                      CDF->rVars[(int)CDF->CURrVarNum]);

    /**************************************************************************
    * Read VDR.
    **************************************************************************/
    if (!sX(ReadVDR64(CDF,CDF->fp,VDRoffset,zVar,
		      VDR_RECORD,&VDR,NULL,
		      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Verify that no records have been written or allocated.  If so, the
    * compression cannot be changed.
    **************************************************************************/
    if (!sX(LastRecord64(CDF,VDRoffset,zVar,&maxAllocated),&pStatus)){
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (maxAllocated > NO_RECORD) return CANNOT_COMPRESS;
    if (VDR.MaxRec > NO_RECORD) return CANNOT_COMPRESS;
    /**************************************************************************
    * If array sparseness is selected, then the variable cannot be compressed.
    **************************************************************************/
    if (SPARSEarraysBITset(VDR.Flags)) {
      if (cType != NO_COMPRESSION) return CANNOT_COMPRESS;
    }
    /**************************************************************************
    * Disable current compression (if any).
    **************************************************************************/
    if (VARcompressionBITset(VDR.Flags)) {
      ClearBit32 (&VDR.Flags, VDR_COMPRESSION_BIT);
      if (!sX(ReadCPR64(CDF->fp,VDR.CPRorSPRoffset,
		        CPR_RECORD,&CPR,
		        CPR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }      
      if (!sX(WasteIR64(CDF,VDR.CPRorSPRoffset,CPR.RecordSize),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      VDR.CPRorSPRoffset = (OFF_T) NO_OFFSET64;
    }
    /**************************************************************************
    * Set up new compression.
    **************************************************************************/
    switch (cType) {
      case NO_COMPRESSION:
	break;
      case RLE_COMPRESSION:
      case HUFF_COMPRESSION:
      case AHUFF_COMPRESSION:
      case GZIP_COMPRESSION:
	pCount = CompressionParmsCount ((Int32) cType);
	SetBit32 (&VDR.Flags, VDR_COMPRESSION_BIT);
	CPR.RecordSize = CPR_BASE_SIZE64 + (pCount * sizeof(Int32));
	CPR.RecordType = CPR_;
	CPR.cType = (Int32) cType;
	CPR.rfuA = 0;
	CPR.pCount = pCount;
	for (p = 0; p < pCount; p++) CPR.cParms[p] = (Int32) cParms[p];
	if (!sX(AllocateIR64(CDF,CPR.RecordSize,&VDR.CPRorSPRoffset),&pStatus)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
	if (!sX(WriteCPR64(CDF->fp,VDR.CPRorSPRoffset,
			   CPR_RECORD,&CPR,
			   CPR_NULL),&pStatus)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
	break;
    }
    /**************************************************************************
    * Update VDR.
    **************************************************************************/
    if (!sX(WriteVDR64(CDF,CDF->fp,VDRoffset,zVar,
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
      if (!sX(VariableType64(CDF,VDRoffset,zVar,&(Var->vType)),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (cType != NO_COMPRESSION) {
        Var->cType = (Int32) cType;
        for (p = 0; p < pCount; p++) Var->cParms[p] = (Int32) cParms[p];
	Var->reservePct = 0;
      }
      if (!sX(CalcBF64(CDF,Var),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    break;
  }
  /****************************************************************************
  * CDF_CHECKSUM_,
  ****************************************************************************/
  case CDF_CHECKSUM_: {
    long checksumValue = va_arg (Va->ap, long);
    Int32 version, release;
    struct CDFstruct *CDF; Int32 CDRflags;
    SelectCDF (Cur->cdf, CDF)
    if (!CDF->singleFile) return CANNOT_CHANGE;
    if (checksumValue != NONE_CHECKSUM &&
        checksumValue != MD5_CHECKSUM &&
        checksumValue != OTHER_CHECKSUM) return BAD_CHECKSUM;
    /**************************************************************************
    * Update the `flags' field in the CDR.
    **************************************************************************/
    if (!sX(ReadCDR64(CDF->fp,CDF->CDRoffset64,
                      CDR_VERSION,&version,
                      CDR_RELEASE,&release,
                      CDR_FLAGS,&CDRflags,
                      CDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Only apply to Version 2.6 and later CDFs.
    **************************************************************************/
    if ((version*100+release) < 206) return CHECKSUM_NOT_ALLOWED;

    /**************************************************************************
    * Get write access.
    **************************************************************************/
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    if (checksumValue == NONE_CHECKSUM) {
      ClearBit32 (&CDRflags, CDR_CHECKSUM_BIT);
      ClearBit32 (&CDRflags, CDR_CHECKSUM_MD5_BIT);
      ClearBit32 (&CDRflags, CDR_CHECKSUM_OTHER_BIT);
    } else {
      SetBit32 (&CDRflags, CDR_CHECKSUM_BIT);
      if (checksumValue == MD5_CHECKSUM) {
        SetBit32 (&CDRflags, CDR_CHECKSUM_MD5_BIT);
        ClearBit32 (&CDRflags, CDR_CHECKSUM_OTHER_BIT);
      } else {
        ClearBit32 (&CDRflags, CDR_CHECKSUM_MD5_BIT);
        SetBit32 (&CDRflags, CDR_CHECKSUM_OTHER_BIT);
      }
    }
    if (!sX(WriteCDR64(CDF->fp,CDF->CDRoffset64,
                       CDR_FLAGS,&CDRflags,
                       CDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    CDF->checksum = checksumValue;
    break;
  }
  /****************************************************************************
  * r/zVAR_SPARSERECORDS_
  ****************************************************************************/
  case rVAR_SPARSERECORDS_:
  case zVAR_SPARSERECORDS_: {
    Logical zOp = (Va->item == zVAR_SPARSERECORDS_), zVar;
    Int32 maxAllocated, maxRec, flags; OFF_T VDRoffset;
    struct CDFstruct *CDF; struct VarStruct *Var;
    Int32 sRecordsType = (Int32) va_arg (Va->ap, long);
    /**************************************************************************
    * Verify the current CDF and variable and that this operation is legal for
    * the current zMode.
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    /**************************************************************************
    * Get write access.
    **************************************************************************/
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    /**************************************************************************
    * Verify that sparseness isn't being attempted on a multi-file CDF.  (It
    * is allowed to specify sRecords.NO.)  After this point a single-file CDF
    * can be assumed.
    **************************************************************************/
    if (!CDF->singleFile) {
      if (sRecordsType != NO_SPARSERECORDS) return CANNOT_SPARSERECORDS;
      break;
    }
    /**************************************************************************
    * Locate the current variable.
    **************************************************************************/
    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) VDRoffset = CDF->CURzVarOffset64;
    else VDRoffset = CDF->CURrVarOffset64;

    if (zModeON(CDF))
      if (CDF->CURzVarNum < CDF->NrVars)
        Var = CDF->rVars[(int)CDF->CURzVarNum];
      else
        Var = CDF->zVars[(int)(CDF->CURzVarNum - CDF->NrVars)];
    else
      Var = BOO(zOp,CDF->zVars[(int)CDF->CURzVarNum],
                    CDF->rVars[(int)CDF->CURrVarNum]);
    /**************************************************************************
    * Verify that no records have been written or allocated.  If so, the
    * sparseness cannot be changed.
    **************************************************************************/
    if (!sX(ReadVDR64(CDF,CDF->fp,VDRoffset,zVar,
		      VDR_MAXREC,&maxRec,
		      VDR_FLAGS,&flags,
		      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (maxRec > NO_RECORD) return CANNOT_SPARSERECORDS;
    if (!sX(LastRecord64(CDF,VDRoffset,zVar,&maxAllocated),&pStatus)){
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (maxAllocated > NO_RECORD) return CANNOT_SPARSERECORDS;
    if (!RECvaryBITset(flags) && sRecordsType != NO_SPARSERECORDS)
      return CANNOT_SPARSERECORDS;
    /**************************************************************************
    * Based on type of sparseness...
    **************************************************************************/
    switch (sRecordsType) {
      case NO_SPARSERECORDS:
      case PAD_SPARSERECORDS:
      case PREV_SPARSERECORDS:
	if (!sX(WriteVDR64(CDF,CDF->fp,VDRoffset,zVar,
			   VDR_sRECORDS,&sRecordsType,
			   VDR_NULL),&pStatus)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
	break;
      default:
	return UNKNOWN_SPARSENESS;
    }
    /**************************************************************************
    * If this variable has been initialized, recalculate the affected
    * parameters.
    **************************************************************************/
    if (Var != NULL) {
      if (!sX(VariableType64(CDF,VDRoffset,zVar,&(Var->vType)),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      Var->prevIfMissing = (sRecordsType == PREV_SPARSERECORDS);
    }
    break;
  }
  /****************************************************************************
  * r/zVAR_SPARSEARRAYS_
  ****************************************************************************/
  case rVAR_SPARSEARRAYS_:
  case zVAR_SPARSEARRAYS_: {
    Logical zOp = (Va->item == zVAR_SPARSEARRAYS_);
    struct CDFstruct *CDF;
    long sArraysType = va_arg (Va->ap, long);	/* Sparseness type. */
    (void) va_arg (Va->ap, long *);		/* Doing it this way quiets
						   the Borland C compiler. */
    /**************************************************************************
    * Verify the current CDF and variable and that this operation is legal for
    * the current zMode.
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    /**************************************************************************
    * Verify that sparse arrays aren't being attempted on a multi-file CDF.
    * (It is allowed to specify no sparse arrays.)
    **************************************************************************/
    if (!CDF->singleFile) {
      if (sArraysType != NO_SPARSEARRAYS) return CANNOT_SPARSEARRAYS;
      break;
    }
    /**************************************************************************
    * Sparse arrays are not yet supported.
    **************************************************************************/
    switch (sArraysType) {
      case NO_SPARSEARRAYS:
	/* Nothing needs to be done until sparse arrays are supported. */
	break;
      default:
	return UNKNOWN_SPARSENESS;
    }
    break;
  }
  /****************************************************************************
  * ATTR_NAME_, 
  ****************************************************************************/
  case ATTR_NAME_: {
    struct CDFstruct *CDF;
    char tmpname[CDF_ATTR_NAME_LEN256+1];
    char *attrname = va_arg (Va->ap, char *);
    OFF_T offsetFound;
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTattrSELECTED64(CDF)) return NO_ATTR_SELECTED;
    if (strlen(attrname) > (size_t) CDF_ATTR_NAME_LEN256) {
      if (!sX(ATTR_NAME_TRUNC,&pStatus)) return pStatus;
    }
    strcpyX (tmpname, attrname, CDF_ATTR_NAME_LEN256);
    if (!ValidAttrName(tmpname)) return BAD_ATTR_NAME;
    /**************************************************************************
    * Check that the new attribute name is not already in use.  Don't flag as
    * an error if the new name is the same as the old name (ignoring trailing
    * blanks).  Trailing blanks may be being eliminated.
    **************************************************************************/
    tStatus = FindAttrByName64 (CDF, tmpname, &offsetFound);
    switch (tStatus) {
      case CDF_OK:
	if (offsetFound != CDF->CURattrOffset64) return ATTR_EXISTS;
	break;
      case NO_SUCH_ATTR:
	break;
      default:
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return tStatus;
    }
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    NulPad (tmpname, CDF_ATTR_NAME_LEN256);
    if (!sX(WriteADR64(CDF->fp,CDF->CURattrOffset64,
		       ADR_NAME,tmpname,
		       ADR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    break;
  }
  /****************************************************************************
  * ATTR_SCOPE_
  ****************************************************************************/
  case ATTR_SCOPE_: {
    struct CDFstruct *CDF;
    Int32 scope = (Int32) va_arg (Va->ap, long);
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTattrSELECTED64(CDF)) return NO_ATTR_SELECTED;
    if (!ValidAttrScope(scope)) return BAD_SCOPE;
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    if (!sX(WriteADR64(CDF->fp,CDF->CURattrOffset64,
		       ADR_SCOPE,&scope,
		       ADR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Reset current entry offsets.
    **************************************************************************/
    if (!sX(SetCURgrEntry64(CDF,FALSE,CDF->CURgrEntryNum),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (!sX(SetCURzEntry64(CDF,FALSE,CDF->CURzEntryNum),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    break;
  }
  /****************************************************************************
  * CDF_LEAPSECONDLASTUPDATED_, 
  ****************************************************************************/
  case CDF_LEAPSECONDLASTUPDATED_: {
    struct CDFstruct *CDF;
    long lastUpdated = va_arg (Va->ap, long);
    Int32 lastUpdatedT;
    SelectCDF (Cur->cdf, CDF)
    if (lastUpdated != 0) {
      if (ValidateLeapSecondLastUpdated((int)lastUpdated) != 1)
      return BADDATE_LEAPSECOND_UPDATED;
    }
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    lastUpdatedT = (Int32) lastUpdated;
    if (!sX(WriteGDR64(CDF->fp,CDF->GDRoffset64,
                       GDR_LEAPSECONDLASTUPDATED,&lastUpdatedT,
                       GDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    break;
  }
  /****************************************************************************
  * gENTRY_DATASPEC_/rENTRY_DATASPEC_/zENTRY_DATASPEC_, change the data
  * specification of an existing entry.  Currently, only the data type can
  * be changed (and must be equivalent).  The number of elements must remain
  * the same.
  ****************************************************************************/
  case rENTRY_DATASPEC_:
  case gENTRY_DATASPEC_:
  case zENTRY_DATASPEC_: {
    int entryType = E3p(Va->item,gENTRY_DATASPEC_,
				 rENTRY_DATASPEC_,
				 zENTRY_DATASPEC_);
    struct CDFstruct *CDF;
    Int32 newDataType = (Int32) va_arg (Va->ap, long);
    Int32 newNumElems = (Int32) va_arg (Va->ap, long);
    Int32 oldDataType, oldNumElems; OFF_T eOffset;
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTattrSELECTED64(CDF)) return NO_ATTR_SELECTED;
    if (E3(entryType,
	   CDF->CURgrEntryNum,
	   CDF->CURgrEntryNum,
	   CDF->CURzEntryNum) == RESERVED_ENTRYNUM) return NO_ENTRY_SELECTED;
    if (!ValidDataType(newDataType)) return BAD_DATA_TYPE;
    if (newNumElems != -99 && newNumElems < 1) return BAD_NUM_ELEMS;
    if (!sX(CheckEntryOp64(CDF,entryType),&pStatus)) return pStatus;
    eOffset = E3(entryType,CDF->CURgrEntryOffset64,
			   CDF->CURgrEntryOffset64,
			   CDF->CURzEntryOffset64);
    if (eOffset == (OFF_T) RESERVED_ENTRYOFFSET64) return NO_SUCH_ENTRY;
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    if (!sX(ReadAEDR64(CDF->fp,eOffset,
		       AEDR_DATATYPE,&oldDataType,
		       AEDR_NUMELEMS,&oldNumElems,
		       AEDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if ((!EquivDataTypes(newDataType,oldDataType)) ||
	(newNumElems != -99 && newNumElems != oldNumElems)) 
        return CANNOT_CHANGE;
    if (!sX(WriteAEDR64(CDF,CDF->fp,eOffset,
		        AEDR_DATATYPE,&newDataType,
		        AEDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    break;
  }
  /****************************************************************************
  * gENTRY_DATA_/rENTRY_DATA_/zENTRY_DATA_, 
  ****************************************************************************/
  case rENTRY_DATA_:
  case gENTRY_DATA_:
  case zENTRY_DATA_: {
    int entryType = E3p(Va->item,gENTRY_DATA_,rENTRY_DATA_,zENTRY_DATA_);
    struct CDFstruct *CDF;
    struct ADRstruct64 ADR;
    long entryN;                /* True entry number. */
    Logical zEntry;             /* If true, a AzEDR.  If FALSE, a AgrEDR. */
    int nBytesNew;              /* Size of new entry value. */
    OFF_T eOffset;              /* Offset of AEDR. */
    long dataType = va_arg (Va->ap, long);
    long numElems = va_arg (Va->ap, long);
    void *value = va_arg (Va->ap, void *);
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTattrSELECTED64(CDF)) return NO_ATTR_SELECTED;
    if (E3(entryType,
	   CDF->CURgrEntryNum,
	   CDF->CURgrEntryNum,
	   CDF->CURzEntryNum) == RESERVED_ENTRYNUM) return NO_ENTRY_SELECTED;
    if (!ValidDataType((Int32)dataType)) return BAD_DATA_TYPE;
    if (numElems < 1L) return BAD_NUM_ELEMS;
    if (!sX(ReadADR64(CDF->fp,CDF->CURattrOffset64,
		      ADR_RECORD,&ADR,
		      ADR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (!sX(CheckEntryOp64(CDF,entryType),&pStatus)) return pStatus;
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    if (dataType == CDF_TIME_TT2000) {
      if (!UpdateTT2000header(CDF,&pStatus)) return pStatus;
    }
    nBytesNew = (int) (CDFelemSize(dataType) * numElems);
    eOffset = E3(entryType,CDF->CURgrEntryOffset64,
			   CDF->CURgrEntryOffset64,
			   CDF->CURzEntryOffset64);
    if (eOffset != (OFF_T) RESERVED_ENTRYOFFSET64) {
      /************************************************************************
      * The entry already exists.
      ************************************************************************/
      struct AEDRstruct64 AEDR; int nBytesCur;
      if (!sX(ReadAEDR64(CDF->fp,eOffset,
		         AEDR_RECORD,&AEDR,NULL,
		         AEDR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      nBytesCur = (int) (CDFelemSize(AEDR.DataType) * AEDR.NumElems);
      zEntry = (AEDR.RecordType == AzEDR_);
      if (nBytesNew != nBytesCur) {
	/********************************************************************
	* The size of the new entry value is different than the size of the
	* old entry value.  The AEDR is changing size.
	********************************************************************/
	OFF_T prevOffset, newOffset;
	if (!sX(FindPrevEntry64(CDF,CDF->CURattrOffset64,eOffset,
			        zEntry,&prevOffset),&pStatus)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
	if (!sX(ResizeIR64(CDF,eOffset,
			   AEDR_BASE_SIZE64+nBytesNew,
			   &newOffset,TRUE,NULL),&pStatus)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
	AEDR.RecordSize = AEDR_BASE_SIZE64 + nBytesNew;
	AEDR.DataType = dataType;
	AEDR.NumElems = numElems;
	if (!sX(WriteAEDR64(CDF,CDF->fp,newOffset,
			    AEDR_RECORD,&AEDR,value,
			    AEDR_NULL),&pStatus)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
	switch (entryType) {
	  case gENTRYt:
	  case rENTRYt:
	    CDF->CURgrEntryOffset64 = newOffset;
	    break;
	  case zENTRYt:
	    CDF->CURzEntryOffset64 = newOffset;
	    break;
	}
	if (prevOffset == 0) {
	  if (!sX(WriteADR64(CDF->fp,CDF->CURattrOffset64,
			     BOO(zEntry,ADR_AzEDRHEAD,ADR_AgrEDRHEAD),&newOffset,
			     ADR_NULL),&pStatus)) {
	    AbortAccess64 (CDF, UPDATE, noDELETE);
	    return pStatus;
	  }
	}
	else {
	  if (!sX(WriteAEDR64(CDF,CDF->fp,prevOffset,
			      AEDR_AEDRNEXT,&newOffset,
			      AEDR_NULL),&pStatus)) {
	    AbortAccess64 (CDF, UPDATE, noDELETE);
	    return pStatus;
	  }
	}
      }
      else {
	/********************************************************************
	* The AEDR is not changing size.
	********************************************************************/
	AEDR.DataType = dataType;
	AEDR.NumElems = numElems;
	if (!sX(WriteAEDR64(CDF,CDF->fp,eOffset,
			    AEDR_RECORD,&AEDR,value,
			    AEDR_NULL),&pStatus)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
      }
    }
    else {
      /************************************************************************
      * The entry does not exist.
      ************************************************************************/
      struct AEDRstruct64 AEDR;
      OFF_T lastOffset;
      zEntry = E3(entryType,FALSE,FALSE,
		  BOO(zModeON(CDF),
		      BOO(CDF->CURzEntryNum < CDF->NrVars,FALSE,TRUE),TRUE));
      entryN = E3(entryType,CDF->CURgrEntryNum,CDF->CURgrEntryNum,
		  BOO(zModeON(CDF),
		      BOO(CDF->CURzEntryNum < CDF->NrVars,
			  CDF->CURzEntryNum,CDF->CURzEntryNum - CDF->NrVars),
		      CDF->CURzEntryNum));
      AEDR.RecordSize = AEDR_BASE_SIZE64 + nBytesNew;
      AEDR.RecordType = BOO(zEntry,AzEDR_,AgrEDR_);
      AEDR.AEDRnext = (OFF_T) 0;
      AEDR.AttrNum = ADR.Num;
      AEDR.DataType = dataType;
      AEDR.Num = entryN;
      AEDR.NumElems = numElems;
      AEDR.rfuA = 0;
      AEDR.rfuB = 0;
      AEDR.rfuC = 0;
      AEDR.rfuD = -1;
      AEDR.rfuE = -1;
      if (!sX(AllocateIR64(CDF,AEDR.RecordSize,&eOffset),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (!sX(WriteAEDR64(CDF,CDF->fp,eOffset,
			  AEDR_RECORD,&AEDR,value,
			  AEDR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      switch (entryType) {
	case gENTRYt:
	case rENTRYt:
	  CDF->CURgrEntryOffset64 = eOffset;
	  break;
	case zENTRYt:
	  CDF->CURzEntryOffset64 = eOffset;
	  break;
      }
      if (!sX(FindLastEntry64(CDF,CDF->CURattrOffset64,
			      zEntry,&lastOffset),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (lastOffset == (OFF_T) 0) {
	/********************************************************************
	* This is the first entry (of this type).  The ADR will point to
	* this entry.
	********************************************************************/
	if (zEntry) {
	  ADR.NzEntries = 1;
	  ADR.MAXzEntry = entryN;
	  ADR.AzEDRhead = eOffset;
	}
	else {
	  ADR.NgrEntries = 1;
	  ADR.MAXgrEntry = entryN;
	  ADR.AgrEDRhead = eOffset;
	}
	if (!sX(WriteADR64(CDF->fp,CDF->CURattrOffset64,
			   ADR_RECORD,&ADR,
			   ADR_NULL),&pStatus)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
      }
      else {
	/********************************************************************
	* Entries already exist (of this type).  The currently last entry
	* will point to this entry.
	********************************************************************/
	if (zEntry) {
	  ADR.NzEntries++;
	  ADR.MAXzEntry = MAXIMUM(ADR.MAXzEntry,entryN);
	}
	else {
	  ADR.NgrEntries++;
	  ADR.MAXgrEntry = MAXIMUM(ADR.MAXgrEntry,entryN);
	}
	if (!sX(WriteADR64(CDF->fp,CDF->CURattrOffset64,
			   ADR_RECORD,&ADR,
			   ADR_NULL),&pStatus)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
	if (!sX(WriteAEDR64(CDF,CDF->fp,lastOffset,
			    AEDR_AEDRNEXT,&eOffset,
			    AEDR_NULL),&pStatus)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
      }
    }
    break;
  }
}
return pStatus;
}
