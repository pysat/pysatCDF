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
*   V1.7  13-Oct-06, M Liu      Changed to allow upper and lower case CDF  
*                               name to be used on win32.
*   V3.5  23-Jul-13, M Liu      Changed the alternative encoding to IBMPC.
*
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"
#include "cdfrev.h"

/******************************************************************************
* CDFcre.
******************************************************************************/

STATICforIDL CDFstatus CDFcre (Va, Cur)
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
    char CDFnameT[CDF_PATHNAME_LEN+1], CDFnameX[DU_MAX_PATH_LEN+1], *CDFnameP;
    char CDFnameTx[CDF_PATHNAME_LEN+1];
    char copyRight[CDF_COPYRIGHT_LEN+1];
    CDFid *id;
    struct CDFstruct *CDF;
    struct CDRstruct CDR;
    struct GDRstruct GDR;
    vFILE *dotFp;
    int dimN;
#if defined(vms) || defined(dos)
    Logical upper_case_ext = TRUE;
#else /* Unix, POSIX, & Macintosh */
    Logical upper_case_ext = FALSE;
#endif
    Logical version_numbers = FALSE;
    Logical no_append = FALSE;
    uInt32 magicNumber1 = V2magicNUMBER_1;
    uInt32 magicNumber2u = V2magicNUMBER_2u;
    char *del = NULL;
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
    strcpyX (CDFnameT, CDFnameP, CDF_PATHNAME_LEN);
#if STRIP_TRAILING_BLANKS_FROM_CDFPATH
    StripTrailingBlanks (CDFnameT);
#endif
#if defined(vms) || defined(dos)
    MakeUpperString (CDFnameT);
#endif
    RemoveCDFFileExtension(CDFnameT, CDFnameTx);
#if defined(unix) || defined(win32) || defined(dos)
# if defined(unix)
    del = strrchr(CDFnameTx, '/');
# else
    del = strrchr(CDFnameTx, '\\');
    if (del == NULL) {
#     if defined(win32)
        del = strrchr(CDFnameTx, '/');
#     endif
    }
# endif
    if (del == NULL) {
      if (!ValidCDFname(CDFnameTx)) return BAD_CDF_NAME;
    } else {
      if (!ValidCDFname(del+1)) return BAD_CDF_NAME;
    }
#else
    if (!ValidCDFname(CDFnameTx)) return BAD_CDF_NAME;
#endif
    BuildFilePath (CDFt, CDFnameTx, no_append, upper_case_ext, version_numbers,
		   INT32_ZERO, CDFnameX);
    if (IsReg(CDFnameX)) return CDF_EXISTS;
    /**************************************************************************
    * Create CDF file.
    **************************************************************************/
    dotFp = V_open (CDFnameX, WRITE_PLUS_a_mode);
    if (dotFp == NULL) return CDF_CREATE_ERROR;
    /**************************************************************************
    * Allocate/initialize CDF structure.
    **************************************************************************/
    CDF = (struct CDFstruct *) cdf_AllocateMemory ((size_t)sizeof(struct CDFstruct), NULL);
    if (CDF == NULL) {
      V_close (dotFp, NULL, NULL);
      CDFdeleteFile (CDFnameX);
      return BAD_MALLOC;
    }
    CDF->CDFname = (char *) cdf_AllocateMemory ((size_t)strlen(CDFnameTx) + 1, NULL);
    if (CDF->CDFname == NULL) {
      V_close (dotFp, NULL, NULL);
      CDFdeleteFile (CDFnameX);
      cdf_FreeMemory (CDF, NULL);
      return BAD_MALLOC;
    }
    else
      strcpyX (CDF->CDFname, CDFnameTx, 0);
    CDF->magic = VALIDid_MAGIC_NUMBER;
    CDF->largeFile = FALSE;
    CDF->CDRoffset = V2_CDR_OFFSET;
    CDF->GDRoffset = NO_OFFSET;		/* Reset below when GDR is written. */
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
    CDF->encoding = BOO(DEFAULT_TO_HOST_ENCODING,
			HostEncoding(),IBMPC_ENCODING);
    CDF->rowMajor = DEFAULT_TO_ROW_MAJOR;
    CDF->singleFile = DEFAULT_TO_SINGLE_FILE;
    CDF->rMaxRec = NO_RECORD;
    CDF->rNumDims = numDims;
    for (dimN = 0; dimN < numDims; dimN++) {
       CDF->rDimSizes[dimN] = dimSizes[dimN];
    }
    CDF->stage.fp = NULL;
    CDF->stage.mark = ZERO_OFFSET;
    CDF->stage.cacheSize = NUMcacheSTAGE;
    CDF->compressFp = NULL;
    CDF->scratchDir = NULL;
    CDF->workingCacheSize = BOO(CDF->singleFile,NUMcacheSINGLE,NUMcacheMULTI);
    CDF->compressCacheSize = NUMcacheCOMPRESS;
    CDF->checksum = BOO(CDFgetChecksumEnvVar()<=0,NONE_CHECKSUM,MD5_CHECKSUM);
    InitCURobjectsStates (CDF);
    AddTOvStats (&CDF->dotCDFvStats, NULL);
    AddTOvStats (&CDF->uDotCDFvStats, NULL);
    /**************************************************************************
    * Set number of cache buffers based on the CDF's format.
    **************************************************************************/
    if (!CACHEv(CDF->fp,CDF->workingCacheSize)) {
      AbortAccess (CDF, noUPDATE, CDFDELETE);
      cdf_FreeMemory (CDF, NULL);
      return BAD_CACHE_SIZE;
    }
    /**************************************************************************
    * Write magic numbers.
    **************************************************************************/
    if (!Write32(CDF->fp,(Int32 *)&magicNumber1)) {
      AbortAccess (CDF, noUPDATE, CDFDELETE);
      cdf_FreeMemory (CDF, NULL);
      return CDF_WRITE_ERROR;
    }
    if (!Write32(CDF->fp,(Int32 *)&magicNumber2u)) {
      AbortAccess (CDF, noUPDATE, CDFDELETE);
      cdf_FreeMemory (CDF, NULL);
      return CDF_WRITE_ERROR;
    }
    /**************************************************************************
    * Write CDR.
    **************************************************************************/
    CDR.RecordSize = CDR_BASE_SIZE + CDF_COPYRIGHT_LEN;
    CDR.RecordType = CDR_;
    CDR.GDRoffset = CDF->CDRoffset + CDR.RecordSize;
    if (CDFgetFileBackward()) {
      CDR.Version = 2;
      CDR.Release = 7;
      CDR.Increment = 2;
    } else {
      CDR.Version = CDF_LIBRARY_VERSION;
      CDR.Release = CDF_LIBRARY_RELEASE;
      CDR.Increment = CDF_LIBRARY_INCREMENT;
    }
    CDR.Encoding = CDF->encoding;
    CDR.Flags = 0;
    if (CDF->rowMajor) SetBit32 (&(CDR.Flags), CDR_MAJORITY_BIT);
    if (CDF->singleFile) SetBit32 (&(CDR.Flags), CDR_FORMAT_BIT);
    CDR.rfuA = 0;
    CDR.rfuB = 0;
    CDR.rfuD = -1;
    CDR.rfuE = -1;
    CDFcopyRight (copyRight);
    NulPad (copyRight, CDF_COPYRIGHT_LEN);
    if (!sX(WriteCDR(CDF->fp,V2_CDR_OFFSET,
		     CDR_RECORD,&CDR,copyRight,
		     CDR_NULL),&pStatus)) {
      AbortAccess (CDF, noUPDATE, CDFDELETE);
      cdf_FreeMemory (CDF, NULL);
      return pStatus;
    }
    /**************************************************************************
    * Write GDR.
    **************************************************************************/
    CDF->GDRoffset = CDR.GDRoffset;
    GDR.RecordSize = GDR_BASE_SIZE + (numDims * sizeof(Int32));
    GDR.RecordType = GDR_;
    GDR.rVDRhead = 0;
    GDR.zVDRhead = 0;
    GDR.ADRhead = 0;
    GDR.eof = CDR.GDRoffset + GDR.RecordSize;
    GDR.NrVars = CDF->NrVars;
    GDR.NumAttr = 0;
    GDR.rMaxRec = CDF->rMaxRec;
    GDR.rNumDims = CDF->rNumDims;
    GDR.NzVars = CDF->NzVars;
    GDR.UIRhead = 0;
    GDR.rfuC = 0;
    GDR.rfuD = -1;
    GDR.rfuE = -1;
    for (dimN = 0; dimN < CDF->rNumDims; dimN++) {
       GDR.rDimSizes[dimN] = CDF->rDimSizes[dimN];
    }
    if (!sX(WriteGDR(CDF->fp,CDF->GDRoffset,
		     GDR_RECORD,&GDR,
		     GDR_NULL),&pStatus)) {
      AbortAccess (CDF, noUPDATE, CDFDELETE);
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
    char *name, Tname[CDF_VAR_NAME_LEN+1], pathnameX[DU_MAX_PATH_LEN+1];
    struct CDFstruct *CDF;
    struct VDRstruct VDR;
    struct VarStruct ***vars;
    vFILE *varFp;
    Int32 offset, ntlOffset, VDRhead, nVars;
    Int32 dataType, numElements;
    int dimN, *max;
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
    if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		    BOO(zOp,GDR_zVDRHEAD,GDR_rVDRHEAD),&VDRhead,
		    BOO(zOp,GDR_NzVARS,GDR_NrVARS),&nVars,
		    GDR_NULL),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Validate arguments.
    **************************************************************************/
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!ValidDataType(dataType) || InValidDataType(dataType))
      return BAD_DATA_TYPE;
    if (numElements < 1) return BAD_NUM_ELEMS;
    if (!STRINGdataType(dataType) && numElements != 1) return BAD_NUM_ELEMS;
    if (zOp) {
      if (zNumDims < 0 || zNumDims > CDF_MAX_DIMS) return BAD_NUM_DIMS;
      for (dimN = 0; dimN < zNumDims; dimN++)
	 if (zDimSizes[dimN] < 1) return BAD_DIM_SIZE;
    }
    if (strlen(name) > (size_t) CDF_VAR_NAME_LEN) {
      if (!sX(VAR_NAME_TRUNC,&pStatus)) return pStatus;
    }
    strcpyX (Tname, name, CDF_VAR_NAME_LEN);
#if LIMITof64K
    if (TOObigIBMpc(CDFelemSize(dataType)*numElements)) return IBM_PC_OVERFLOW;
#endif
    if (!ValidVarName(Tname)) return BAD_VAR_NAME;
    tStatus = FindVarByName (CDF, Tname, NULL, NULL, NULL);
    switch (tStatus) {
      case NO_SUCH_VAR:
	break;
      case CDF_OK:
	return VAR_EXISTS;
      default:
	AbortAccess (CDF, UPDATE, noDELETE);
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
    if (!WriteAccess(CDF,FALSE,&pStatus)) return pStatus;
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
      varFp = V_open (pathnameX, WRITE_PLUS_a_mode);
      if (varFp == NULL) {
	if (!sX(CloseLRUvar(CDF),&pStatus)) return pStatus;
	varFp = V_open (pathnameX, WRITE_PLUS_a_mode);
	if (varFp == NULL) return VAR_CREATE_ERROR;
      }
      if (!CLOSEv(varFp,NULL, NULL)) {
	CDFdeleteFile (pathnameX);
	return VAR_CREATE_ERROR;
      }
    }
    /**************************************************************************
    * Write rVDR/zVDR.
    **************************************************************************/
    VDR.RecordSize = BOO(zOp,zVDR_BASE_SIZE,rVDR_BASE_SIZE) +
		     BOO(zOp,zNumDims,0)*sizeof(Int32) +          /*DimSizes.*/
		     BOO(zOp,zNumDims,
			     CDF->rNumDims)*sizeof(Int32);	  /*DimVarys.*/
    VDR.RecordType = BOO(zOp,zVDR_,rVDR_);
    VDR.VDRnext = 0;
    VDR.DataType = dataType;
    VDR.MaxRec = NO_RECORD;
    VDR.VXRhead = 0;
    VDR.VXRtail = 0;
    VDR.Flags = 0;
    if (recVariance) SetBit32 (&(VDR.Flags), VDR_RECVARY_BIT);
    VDR.sRecords = NO_SPARSERECORDS;
    VDR.rfuB = 0;
    VDR.rfuC = -1;
    VDR.rfuF = -1;
    VDR.NumElems = numElements;
    VDR.Num = newVarNum;
    VDR.CPRorSPRoffset = NO_OFFSET;
    VDR.blockingFactor = 0;
    strcpyX (VDR.Name, Tname, CDF_VAR_NAME_LEN);
    NulPad (VDR.Name, CDF_VAR_NAME_LEN);
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
    if (!sX(AllocateIR(CDF,VDR.RecordSize+padSize,&offset),&pStatus)) {
      CDFdeleteFile (pathnameX);
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    toPad = VDR.RecordSize;
    DefaultPadValuePre350(dataType, numElements, pad);
    VDR.RecordSize += padSize;
    SetBit32 (&VDR.Flags,VDR_PADVALUE_BIT);
    if (!sX(WriteVDR(CDF,CDF->fp,offset,zOp,
		     VDR_RECORD,&VDR,pad,
		     VDR_NULL),&pStatus)) {
      CDFdeleteFile (pathnameX);
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    cdf_FreeMemory (pad, NULL);
    /**************************************************************************
    * Point next-to-last rVDR/zVDR (or GDR) to this rVDR/zVDR.
    **************************************************************************/
    if (newVarNum != 0) {
      if (!sX(FindVarByNumber(CDF,(Int32)(newVarNum-1),
			      zOp,&ntlOffset),&pStatus)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (!sX(WriteVDR(CDF,CDF->fp,ntlOffset,zOp,
		       VDR_VDRNEXT,&offset,
		       VDR_NULL),&pStatus)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    else
      VDRhead = offset;
    /**************************************************************************
    * Update GDR fields that may have been modified.
    **************************************************************************/
    if (!sX(WriteGDR(CDF->fp,CDF->GDRoffset,
		     BOO(zOp,GDR_zVDRHEAD,GDR_rVDRHEAD),&VDRhead,
		     BOO(zOp,GDR_NzVARS,GDR_NrVARS),&nVars,
		     GDR_NULL),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
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
      CDF->CURzVarOffset = offset;
      *numOut = varN;
    }
    else {
      if (zOp) {
	CDF->CURzVarNum = newVarNum;
        CDF->CURzVarOffset = offset;
      } else {
	CDF->CURrVarNum = newVarNum;
        CDF->CURrVarOffset = offset;
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
    char *attrName, truncName[CDF_ATTR_NAME_LEN+1];
    struct CDFstruct *CDF;
    struct ADRstruct ADR;
    Int32 offset, numAttr, ADRhead;
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
    if (strlen(attrName) > (size_t) CDF_ATTR_NAME_LEN) {
      if (!sX(ATTR_NAME_TRUNC,&pStatus)) return pStatus;
    }
    strcpyX (truncName, attrName, CDF_ATTR_NAME_LEN);
    if (!ValidAttrName(truncName)) return BAD_ATTR_NAME;
    tStatus = FindAttrByName (CDF, truncName, NULL);
    switch (tStatus) {
      case NO_SUCH_ATTR:
	break;
      case CDF_OK:
	return ATTR_EXISTS;
      default:
	AbortAccess (CDF, UPDATE, noDELETE);
	return tStatus;
    }
    /**************************************************************************
    * Switch to read-write access if necessary.
    **************************************************************************/
    if (!WriteAccess(CDF,FALSE,&pStatus)) return pStatus;
    /**************************************************************************
    * Read GDR fields.
    **************************************************************************/
    if (!sX(ReadGDR(CDF->fp,CDF->GDRoffset,
		    GDR_NUMATTR,&numAttr,
		    GDR_ADRHEAD,&ADRhead,
		    GDR_NULL),&pStatus)) return pStatus;
    /**************************************************************************
    * Write ADR.
    **************************************************************************/
    ADR.RecordSize = ADR_BASE_SIZE;
    ADR.RecordType = ADR_;
    ADR.ADRnext = 0;
    ADR.AgrEDRhead = 0;
    ADR.Scope = scope;
    ADR.Num = numAttr;
    ADR.NgrEntries = 0;
    ADR.MAXgrEntry = NO_ENTRY;
    ADR.rfuA = 0;
    ADR.AzEDRhead = 0;
    ADR.NzEntries = 0;
    ADR.MAXzEntry = NO_ENTRY;
    ADR.rfuE = -1;
    strcpyX (ADR.Name, truncName, CDF_ATTR_NAME_LEN);
    NulPad (ADR.Name, CDF_ATTR_NAME_LEN);
    if (!sX(AllocateIR(CDF,ADR.RecordSize,&offset),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (!sX(WriteADR(CDF->fp,offset,
		     ADR_RECORD,&ADR,
		     ADR_NULL),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Point last ADR (or GDR) to this ADR.
    **************************************************************************/
    if (numAttr == 0)
      ADRhead = offset;
    else {
      Int32 lastOffset;
      if (!sX(FindLastAttr(CDF,&lastOffset),&pStatus)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (!sX(WriteADR(CDF->fp,lastOffset,
		       ADR_ADRNEXT,&offset,
		       ADR_NULL),&pStatus)) {
	AbortAccess (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    /**************************************************************************
    * Update GDR fields.
    **************************************************************************/
    numAttr++;
    if (!sX(WriteGDR(CDF->fp,CDF->GDRoffset,
		     GDR_NUMATTR,&numAttr,
		     GDR_ADRHEAD,&ADRhead,
		     GDR_NULL),&pStatus)) {
      AbortAccess (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Select current attribute and entry offsets and pass back attribute
    * number.
    **************************************************************************/
    CDF->CURattrOffset = offset;
    CDF->CURgrEntryOffset = RESERVED_ENTRYOFFSET;
    CDF->CURzEntryOffset = RESERVED_ENTRYOFFSET;
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
