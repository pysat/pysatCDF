/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                        CDF `create' operations.
*
*  Version 1.5b, 20-Oct-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  20-May-92, J Love     Original version (was part of `cdflib.c').
*   V1.1  29-Sep-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V1.2  24-Jan-94, J Love     CDF V2.4.  Added readonly mode.
*   V1.3  15-Dec-94, J Love     CDF V2.5.
*   V1.3a  9-Jan-95, J Love	Encode/decode changes.  More cache-residency.
*   V1.3b 24-Feb-95, J Love	Solaris 2.3 IDL i/f.
*   V1.4  21-Mar-95, J Love	POSIX.
*   V1.4a  7-Sep-95, J Love	CDFexport-related changes.  Fixed cleanup when
*				a CDF is aborted.
*   V1.5  15-Aug-96, J Love	CDF V2.6.
*   V1.5a 21-Feb-97, J Love	Removed RICE.
*   V1.5b 11-Sep-97, J Love	Magic numbers are now uInt32.
*   V1.5c 20-Oct-97, J Love	Properly cast the uInt32 magic numbers.
*   V1.6   8-Apr-04, M Liu      Save the currently created variable's offset. 
*   V2.0  29-Jun-04, M Liu      Added LFS (Large File Support > 2G).
*   V2.1  13-Oct-06, M Liu      Changed to allow upper and lower case CDF  
*                               name to be used on win32.
*   V3.5  23-Jul-13, M Liu      Changed to alternative encoding to IBMPC.
*
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"
#include "cdfrev.h"

/******************************************************************************
* CDFcre64.
******************************************************************************/

STATICforIDL CDFstatus CDFcre64 (Va, Cur)
struct VAstruct *Va;
struct CurStruct *Cur;
{
CDFstatus tStatus, pStatus = CDF_OK;

switch (Va->item) {
  /****************************************************************************
  * CDF_, create a new CDF.
  ****************************************************************************/
  case CDF_: {
    long numDims, *dimSizes;
    char CDFnameT[CDF_PATHNAME_LEN+1], CDFnameX[DU_MAX_PATH_LEN+1], 
         *CDFnameP;
    char copyRight[CDF_COPYRIGHT_LEN+1];
    CDFid *id;
    struct CDFstruct *CDF;
    struct CDRstruct64 CDR;
    struct GDRstruct64 GDR;
    vFILE *dotFp;
    int dimN;
#if defined(vms) || defined(dos)
    Logical upper_case_ext = TRUE;
#else /* Unix, POSIX, win32 & Macintosh */
    Logical upper_case_ext = FALSE;
#endif
    Logical version_numbers = FALSE;
    Logical no_append = FALSE;
    uInt32 magicNumber1 = V3magicNUMBER_1;
    uInt32 magicNumber2u = V3magicNUMBER_2u;
    char CDFnamePx[CDF_PATHNAME_LEN+1];
    char *del = NULL;
    long year, month, day;
    /**************************************************************************
    * Get arguments for this operation/item.
    **************************************************************************/
    CDFnameP = va_arg (Va->ap, char *);
    numDims = va_arg (Va->ap, long);
    dimSizes = va_arg (Va->ap, long *);
    id = va_arg (Va->ap, CDFid *);
    *id = (CDFid) NULL;
    /**************************************************************************
    * Validate arguments.
    **************************************************************************/
#if BUILD_READ_ONLY_DISTRIBUTION
    if (!sX(READ_ONLY_DISTRIBUTION,&pStatus)) return pStatus;
#endif
    if (numDims < 0 || numDims > CDF_MAX_DIMS) return BAD_NUM_DIMS;
    for (dimN = 0; dimN < numDims; dimN++) {
       if (dimSizes[dimN] < 1) return BAD_DIM_SIZE;
    }
    if (strlen(CDFnameP) > (size_t) CDF_PATHNAME_LEN) {
      if (!sX(CDF_NAME_TRUNC,&pStatus)) return pStatus;
    }
    RemoveCDFFileExtension(CDFnameP, CDFnamePx);
    strcpyX (CDFnameT, CDFnamePx, CDF_PATHNAME_LEN);
#if STRIP_TRAILING_BLANKS_FROM_CDFPATH
    StripTrailingBlanks (CDFnameT);
#endif
#if defined(vms) || defined(dos)
    MakeUpperString (CDFnameT);
#endif
#if defined(unix) || defined(win32) || defined(dos)
# if defined(unix)
    del = strrchr(CDFnameT, '/');
# else
    del = strrchr(CDFnameT, '\\');
    if (del == NULL) {
#     if defined(win32)
        del = strrchr(CDFnameT, '/');
#     endif
    }
# endif
    if (del == NULL) {
      if (!ValidCDFname(CDFnameT)) return BAD_CDF_NAME;
    } else {
      if (!ValidCDFname(del+1)) return BAD_CDF_NAME;
    }
#else
    if (!ValidCDFname(CDFnameT)) return BAD_CDF_NAME;
#endif
    BuildFilePath (CDFt, CDFnameT, no_append, upper_case_ext, version_numbers,
		   INT32_ZERO, CDFnameX);
    if (IsReg(CDFnameX)) return CDF_EXISTS;
    /**************************************************************************
    * Create CDF file.
    **************************************************************************/
    dotFp = V_open64 (CDFnameX, WRITE_PLUS_a_mode);
    if (dotFp == NULL) return CDF_CREATE_ERROR;
    /**************************************************************************
    * Allocate/initialize CDF structure.
    **************************************************************************/
    CDF = (struct CDFstruct *) cdf_AllocateMemory ((size_t)sizeof(struct CDFstruct), NULL);
    if (CDF == NULL) {
      V_close64 (dotFp, NULL, NULL);
      CDFdeleteFile (CDFnameX);
      return BAD_MALLOC;
    }
    CDF->CDFname = (char *) cdf_AllocateMemory ((size_t)strlen(CDFnameT) + 1, NULL);
    if (CDF->CDFname == NULL) {
      V_close64 (dotFp, NULL, NULL);
      CDFdeleteFile (CDFnameX);
      cdf_FreeMemory (CDF, NULL);
      return BAD_MALLOC;
    }
    else
      strcpyX (CDF->CDFname, CDFnameT, 0);
    CDF->magic = VALIDid_MAGIC_NUMBER;
    CDF->largeFile = TRUE;
    CDF->CDRoffset64 = (OFF_T) V3_CDR_OFFSET64;
    CDF->GDRoffset64 = (OFF_T) NO_OFFSET64; /* Reset below when GDR is written. */
    CDF->readOnly = FALSE;
    CDF->zMode = zMODEoff;
    CDF->decoding = HOST_DECODING;
    CDF->negToPosFp0 = FALSE;
    CDF->status = READ_WRITE;
    CDF->dotFp = dotFp;
    CDF->uDotFp = NULL;
    CDF->fp = dotFp;
    CDF->pseudo_clock = 0;
    CDF->upper_case_ext = upper_case_ext;
    CDF->version_numbers = version_numbers;
    CDF->no_append = no_append;
    CDF->fakeEPOCH = FALSE;
    CDF->wastedSpace = FALSE;
    CDF->badEOF = FALSE;
    CDF->badTerminatingOffsets = FALSE;
    CDF->assumedScopes = FALSE;
    CDF->NrVars = 0;
    CDF->NzVars = 0;
    CDF->MAXrVars = 0;
    CDF->MAXzVars = 0;
    CDF->rVars = NULL;
    CDF->zVars = NULL;
    CDF->encoding = BOO(DEFAULT_TO_HOST_ENCODING,HostEncoding(),
			IBMPC_ENCODING);
    CDF->rowMajor = DEFAULT_TO_ROW_MAJOR;
    CDF->singleFile = DEFAULT_TO_SINGLE_FILE;
    CDF->rMaxRec = NO_RECORD;
    CDF->rNumDims = numDims;
    for (dimN = 0; dimN < numDims; dimN++) {
       CDF->rDimSizes[dimN] = dimSizes[dimN];
    }
    CDF->stage.fp = NULL;
    CDF->stage.mark64 = (OFF_T) ZERO_OFFSET64;
    CDF->stage.cacheSize = NUMcacheSTAGE;
    CDF->compressFp = NULL;
    CDF->scratchDir = NULL;
    CDF->workingCacheSize = BOO(CDF->singleFile,NUMcacheSINGLE,NUMcacheMULTI);
    CDF->compressCacheSize = NUMcacheCOMPRESS;
    CDF->checksum = BOO(CDFgetChecksumEnvVar()<=0,NONE_CHECKSUM,MD5_CHECKSUM);
    CDF->leapSecondUpdated = 1;
    InitCURobjectsStates (CDF);
    AddTOvStats (&CDF->dotCDFvStats, NULL);
    AddTOvStats (&CDF->uDotCDFvStats, NULL);
    /**************************************************************************
    * Set number of cache buffers based on the CDF's format.
    **************************************************************************/
    if (!CACHEv64(CDF->fp,CDF->workingCacheSize)) {
      AbortAccess64 (CDF, noUPDATE, CDFDELETE);
      cdf_FreeMemory (CDF, NULL);
      return BAD_CACHE_SIZE;
    }
    /**************************************************************************
    * Write magic numbers.
    **************************************************************************/
    if (!Write32_64(CDF->fp,(Int32 *)&magicNumber1)) {
      AbortAccess64 (CDF, noUPDATE, CDFDELETE);
      cdf_FreeMemory (CDF, NULL);
      return CDF_WRITE_ERROR;
    }
    if (!Write32_64(CDF->fp,(Int32 *)&magicNumber2u)) {
      AbortAccess64 (CDF, noUPDATE, CDFDELETE);
      cdf_FreeMemory (CDF, NULL);
      return CDF_WRITE_ERROR;
    }
    /**************************************************************************
    * Write CDR.
    **************************************************************************/
    CDR.RecordSize = (OFF_T) (CDR_BASE_SIZE64 + CDF_COPYRIGHT_LEN);
    CDR.RecordType = CDR_;
    CDR.GDRoffset = CDF->CDRoffset64 + CDR.RecordSize;
    CDR.Version = CDF_LIBRARY_VERSION;
    CDR.Release = CDF_LIBRARY_RELEASE;
    CDR.Encoding = CDF->encoding;
    CDR.Flags = 0;
    if (CDF->rowMajor) SetBit32 (&(CDR.Flags), CDR_MAJORITY_BIT);
    if (CDF->singleFile) SetBit32 (&(CDR.Flags), CDR_FORMAT_BIT);
    CDR.rfuA = 0;
    CDR.rfuB = 0;
    CDR.Increment = CDF_LIBRARY_INCREMENT;
    CDR.rfuD = -1;
    CDR.rfuE = -1;
    CDFcopyRight (copyRight);
    NulPad (copyRight, CDF_COPYRIGHT_LEN);
    if (!sX(WriteCDR64(CDF->fp,V3_CDR_OFFSET64,
		       CDR_RECORD,&CDR,copyRight,
		       CDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, noUPDATE, CDFDELETE);
      cdf_FreeMemory (CDF, NULL);
      return pStatus;
    }
    /**************************************************************************
    * Write GDR.
    **************************************************************************/
    CDF->GDRoffset64 = CDR.GDRoffset;
    GDR.RecordSize = GDR_BASE_SIZE64 + (OFF_T) ((numDims * sizeof(Int32)));
    GDR.RecordType = GDR_;
    GDR.rVDRhead = (OFF_T) 0;
    GDR.zVDRhead = (OFF_T) 0;
    GDR.ADRhead = (OFF_T) 0;
    GDR.eof = (OFF_T) (CDR.GDRoffset + GDR.RecordSize);
    GDR.NrVars = CDF->NrVars;
    GDR.NumAttr = 0;
    GDR.rMaxRec = CDF->rMaxRec;
    GDR.rNumDims = CDF->rNumDims;
    GDR.NzVars = CDF->NzVars;
    GDR.UIRhead = (OFF_T) 0;
    GDR.rfuC = 0;
#if defined(vms)
    CDFgetLastDateinLeapSecondsTBL (&year, &month, &day);
#else
    CDFgetLastDateinLeapSecondsTable (&year, &month, &day);
#endif
    GDR.LeapSecondLastUpdated = (Int32) (10000*year + 100*month + day);
    GDR.rfuE = -1;
    for (dimN = 0; dimN < CDF->rNumDims; dimN++) {
       GDR.rDimSizes[dimN] = CDF->rDimSizes[dimN];
    }
    if (!sX(WriteGDR64(CDF->fp,CDF->GDRoffset64,
		       GDR_RECORD,&GDR,
		       GDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, noUPDATE, CDFDELETE);
      cdf_FreeMemory (CDF, NULL);
      return pStatus;
    }
    /**************************************************************************
    * Select current CDF and pass back CDFid.
    **************************************************************************/
    Cur->cdf = CDF;
    *id = (CDFid) CDF;
    if (CDF->checksum > 0) {
        if (!sX(CDFsetChecksum(*id, CDF->checksum),&pStatus)) return pStatus;
    }
    AddOpenCDFsCount();
    break;
  }

  /****************************************************************************
  * rVAR_/zVAR_, create a new variable.
  ****************************************************************************/
  case rVAR_:
  case zVAR_: {
    Logical zOp = (Va->item == zVAR_);
    long *numOut, zNumDims, *zDimSizes, recVariance,
	 *dimVariances, newVarNum, numDims;
    char *name, Tname[CDF_VAR_NAME_LEN256+1], pathnameX[DU_MAX_PATH_LEN+1];
    struct CDFstruct *CDF;
    struct VDRstruct64 VDR;
    struct VarStruct ***vars;
    vFILE *varFp;
    Int32 nVars;
    Int32 dataType, numElements;
    int dimN, *max;
    OFF_T offset, ntlOffset, VDRhead;
    int ttmmpp;
    Int32 padSize, toPad;
    void *pad;
    name = va_arg (Va->ap, char *);
    dataType = (Int32) va_arg (Va->ap, long);
    numElements = (Int32) va_arg (Va->ap, long);
    if (zOp) {
      zNumDims = va_arg (Va->ap, long);
      zDimSizes = va_arg (Va->ap, long *);
    }
    recVariance = va_arg (Va->ap, long);
    dimVariances = va_arg (Va->ap, long *);
    numOut = va_arg (Va->ap, long *);
    /**************************************************************************
    * Get current CDF.
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    /**************************************************************************
    * Read GDR fields.
    **************************************************************************/
    if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
		      BOO(zOp,GDR_zVDRHEAD,GDR_rVDRHEAD),&VDRhead,
		      BOO(zOp,GDR_NzVARS,GDR_NrVARS),&nVars,
		      GDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Validate arguments.
    **************************************************************************/
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!ValidDataType(dataType)) return BAD_DATA_TYPE;
    if (numElements < 1) return BAD_NUM_ELEMS;
    if (!STRINGdataType(dataType) && numElements != 1) return BAD_NUM_ELEMS;
    if (zOp) {
      if (zNumDims < 0 || zNumDims > CDF_MAX_DIMS) return BAD_NUM_DIMS;
      for (dimN = 0; dimN < zNumDims; dimN++)
	 if (zDimSizes[dimN] < 1) return BAD_DIM_SIZE;
    }
    if (strlen(name) > (size_t) CDF_VAR_NAME_LEN256) {
      if (!sX(VAR_NAME_TRUNC,&pStatus)) return pStatus;
    }
    strcpyX (Tname, name, CDF_VAR_NAME_LEN256);
#if LIMITof64K
    if (TOObigIBMpc(CDFelemSize(dataType)*numElements)) return IBM_PC_OVERFLOW;
#endif
    if (!ValidVarName(Tname)) return BAD_VAR_NAME;
    tStatus = FindVarByName64 (CDF, Tname, NULL, NULL, NULL);
    switch (tStatus) {
      case NO_SUCH_VAR:
	break;
      case CDF_OK:
	return VAR_EXISTS;
      default:
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return tStatus;
    }
    newVarNum = nVars;
    nVars++;
#if defined(dos)
    if (!CDF->singleFile && newVarNum > 99) return TOO_MANY_VARS;
#endif
    /**************************************************************************
    * Switch to read-write access if necessary.
    **************************************************************************/
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    if (dataType == CDF_TIME_TT2000) {
      if (!UpdateTT2000header(CDF,&pStatus)) return pStatus;
    }
    /**************************************************************************
    * Allocate initial/additional pointers to variable data structures.
    **************************************************************************/
    max = BOO(zOp,&(CDF->MAXzVars),&(CDF->MAXrVars));
    vars = BOO(zOp,&(CDF->zVars),&(CDF->rVars));
    if (newVarNum >= *max) {
      int newMaxVars = (int) (VARs_INCREMENT * ((newVarNum/VARs_INCREMENT)+1));
      int varN;
      size_t nBytes = newMaxVars * sizeof(struct VarStruct *);
      void *ptr;
      if (*max > 0)
	ptr = (struct VarStruct **) cdf_ReallocateMemory (*vars, nBytes, NULL);
      else
	ptr = (struct VarStruct **) cdf_AllocateMemory ((size_t)nBytes, NULL);
      if (ptr == NULL) return BAD_MALLOC;
      *vars = ptr;
      for (varN = *max; varN < newMaxVars; varN++) (*vars)[varN] = NULL;
      *max = newMaxVars;
    }
    /**************************************************************************
    * Create variable file (if multi-file CDF).
    **************************************************************************/
    if (!CDF->singleFile) {
      BuildFilePath (BOO(zOp,Zt,Vt), CDF->CDFname, CDF->no_append,
		     CDF->upper_case_ext, CDF->version_numbers,
		     (Int32) newVarNum, pathnameX);
      varFp = V_open64 (pathnameX, WRITE_PLUS_a_mode);
      if (varFp == NULL) {
	if (!sX(CloseLRUvar(CDF),&pStatus)) return pStatus;
	varFp = V_open64 (pathnameX, WRITE_PLUS_a_mode);
	if (varFp == NULL) return VAR_CREATE_ERROR;
      }
      if (!CLOSEv64(varFp,NULL,NULL)) {
	CDFdeleteFile (pathnameX);
	return VAR_CREATE_ERROR;
      }
    }
    /**************************************************************************
    * Write rVDR/zVDR.
    **************************************************************************/
    if (zOp) ttmmpp = zNumDims * sizeof(Int32);
    else ttmmpp = 0;
    VDR.RecordSize = BOO(zOp,zVDR_BASE_SIZE64,rVDR_BASE_SIZE64) +
		     ttmmpp +                                     /*DimSizes.*/
		     BOO(zOp,zNumDims,
			     CDF->rNumDims)*sizeof(Int32);	  /*DimVarys.*/
    VDR.RecordType = BOO(zOp,zVDR_,rVDR_);
    VDR.VDRnext = (OFF_T) 0;
    VDR.DataType = dataType;
    VDR.MaxRec = NO_RECORD;
    VDR.VXRhead = (OFF_T) 0;
    VDR.VXRtail = (OFF_T) 0;
    VDR.Flags = 0;
    if (recVariance) SetBit32 (&(VDR.Flags), VDR_RECVARY_BIT);
    VDR.sRecords = NO_SPARSERECORDS;
    VDR.rfuB = 0;
    VDR.rfuC = -1;
    VDR.rfuF = -1;
    VDR.NumElems = numElements;
    VDR.Num = newVarNum;
    VDR.CPRorSPRoffset = (OFF_T) NO_OFFSET64;
    VDR.blockingFactor = 0;
    strcpyX (VDR.Name, Tname, CDF_VAR_NAME_LEN256);
    NulPad (VDR.Name, CDF_VAR_NAME_LEN256);
    if (zOp) {
      VDR.zNumDims = zNumDims;
      for (dimN = 0; dimN < zNumDims; dimN++)
	 VDR.zDimSizes[dimN] = zDimSizes[dimN];
    }
    for (dimN = 0, numDims = BOO(zOp,zNumDims,CDF->rNumDims);
	 dimN < numDims; dimN++) {
       VDR.DimVarys[dimN] = BOO(dimVariances[dimN],VARY,NOVARY);
    }
    padSize = (int) (CDFelemSize(dataType) * numElements);
    pad = (void *) cdf_AllocateMemory((size_t)padSize, NULL);
    if (!sX(AllocateIR64(CDF,VDR.RecordSize+padSize,&offset),&pStatus)) {
      CDFdeleteFile (pathnameX);
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    toPad = VDR.RecordSize;
    DefaultPadValue(dataType, numElements, pad);
    VDR.RecordSize += padSize;
    SetBit32 (&VDR.Flags,VDR_PADVALUE_BIT);
    if (!sX(WriteVDR64(CDF,CDF->fp,offset,zOp,
		       VDR_RECORD,&VDR,pad,
		       VDR_NULL),&pStatus)) {
      CDFdeleteFile (pathnameX);
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    cdf_FreeMemory (pad, NULL);
    /**************************************************************************
    * Point next-to-last rVDR/zVDR (or GDR) to this rVDR/zVDR.
    **************************************************************************/
    if (newVarNum != 0) {
      if (!sX(FindVarByNumber64(CDF,(Int32)(newVarNum-1),
			        zOp,&ntlOffset),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (!sX(WriteVDR64(CDF,CDF->fp,ntlOffset,zOp,
		         VDR_VDRNEXT,&offset,
		         VDR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    else
      VDRhead = offset;
    /**************************************************************************
    * Update GDR fields that may have been modified.
    **************************************************************************/
    if (!sX(WriteGDR64(CDF->fp,CDF->GDRoffset64,
		       BOO(zOp,GDR_zVDRHEAD,GDR_rVDRHEAD),&VDRhead,
		       BOO(zOp,GDR_NzVARS,GDR_NrVARS),&nVars,
		       GDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Update the appropriate variable count held in memory for efficiency.
    **************************************************************************/
    if (zOp)
      CDF->NzVars = nVars;
    else
      CDF->NrVars = nVars;
    /**************************************************************************
    * Select current variable and determine variable number to be passed back
    * (based on zMode).
    **************************************************************************/
    if (zModeON(CDF)) {
      long varN = CDF->NrVars + newVarNum;
      CDF->CURzVarNum = varN;
      CDF->CURzVarOffset64 = offset;
      *numOut = varN;
    }
    else {
      if (zOp) {
	CDF->CURzVarNum = newVarNum;
        CDF->CURzVarOffset64 = offset;
      } else {
	CDF->CURrVarNum = newVarNum;
        CDF->CURrVarOffset64 = offset;
      }
      *numOut = newVarNum;
    }
    break;
  }
  /****************************************************************************
  * ATTR_, 
  ****************************************************************************/
  case ATTR_: {
    long *attrNumOut, scope;
    char *attrName, truncName[CDF_ATTR_NAME_LEN256+1];
    struct CDFstruct *CDF;
    struct ADRstruct64 ADR;
    Int32 numAttr;
    OFF_T offset, ADRhead;
    /**************************************************************************
    * Get arguments for this operation/item.
    **************************************************************************/
    attrName = va_arg (Va->ap, char *);
    scope = va_arg (Va->ap, long);
    attrNumOut = va_arg (Va->ap, long *);
    /**************************************************************************
    * Select current CDF.
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    /**************************************************************************
    * Validate arguments.
    **************************************************************************/
    if (!ValidAttrScope((Int32)scope)) return BAD_SCOPE;
    if (strlen(attrName) > (size_t) CDF_ATTR_NAME_LEN256) {
      if (!sX(ATTR_NAME_TRUNC,&pStatus)) return pStatus;
    }
    strcpyX (truncName, attrName, CDF_ATTR_NAME_LEN256);
    if (!ValidAttrName(truncName)) return BAD_ATTR_NAME;
    tStatus = FindAttrByName64 (CDF, truncName, NULL);
    switch (tStatus) {
      case NO_SUCH_ATTR:
	break;
      case CDF_OK:
	return ATTR_EXISTS;
      default:
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return tStatus;
    }
    /**************************************************************************
    * Switch to read-write access if necessary.
    **************************************************************************/
    if (!WriteAccess64(CDF,FALSE,&pStatus)) return pStatus;
    /**************************************************************************
    * Read GDR fields.
    **************************************************************************/
    if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
		      GDR_NUMATTR,&numAttr,
		      GDR_ADRHEAD,&ADRhead,
		      GDR_NULL),&pStatus)) return pStatus;
    /**************************************************************************
    * Write ADR.
    **************************************************************************/
    ADR.RecordSize = ADR_BASE_SIZE64;
    ADR.RecordType = ADR_;
    ADR.ADRnext = (OFF_T) 0;
    ADR.AgrEDRhead = (OFF_T) 0;
    ADR.Scope = scope;
    ADR.Num = numAttr;
    ADR.NgrEntries = 0;
    ADR.MAXgrEntry = NO_ENTRY;
    ADR.rfuA = 0;
    ADR.AzEDRhead = (OFF_T) 0;
    ADR.NzEntries = 0;
    ADR.MAXzEntry = NO_ENTRY;
    ADR.rfuE = -1;
    strcpyX (ADR.Name, truncName, CDF_ATTR_NAME_LEN256);
    NulPad (ADR.Name, CDF_ATTR_NAME_LEN256);
    if (!sX(AllocateIR64(CDF,ADR.RecordSize,&offset),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (!sX(WriteADR64(CDF->fp,offset,
		       ADR_RECORD,&ADR,
		       ADR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Point last ADR (or GDR) to this ADR.
    **************************************************************************/
    if (numAttr == 0)
      ADRhead = offset;
    else {
      OFF_T lastOffset;
      if (!sX(FindLastAttr64(CDF,&lastOffset),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (!sX(WriteADR64(CDF->fp,lastOffset,
		         ADR_ADRNEXT,&offset,
		         ADR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    /**************************************************************************
    * Update GDR fields.
    **************************************************************************/
    numAttr++;
    if (!sX(WriteGDR64(CDF->fp,CDF->GDRoffset64,
		       GDR_NUMATTR,&numAttr,
		       GDR_ADRHEAD,&ADRhead,
		       GDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Select current attribute and entry offsets and pass back attribute
    * number.
    **************************************************************************/
    CDF->CURattrOffset64 = offset;
    CDF->CURgrEntryOffset64 = (OFF_T) RESERVED_ENTRYOFFSET64;
    CDF->CURzEntryOffset64 = (OFF_T) RESERVED_ENTRYOFFSET64;
    *attrNumOut = ADR.Num;
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
