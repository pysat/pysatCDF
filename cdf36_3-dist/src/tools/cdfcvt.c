/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                             CDFconvert.
*
*  Version 2.5c, 14-Dec-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  24-Jan-91, H Leckner  Original version (for CDF V2.0).
*   V2.0  17-May-92, J Love     IBM PC port (major changes).
*   V2.1  29-Sep-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V2.1a  6-Oct-92, J Love     Fixed freeing of `bufferX' if 1-dimensional.
*   V2.2  25-Jan-94, J Love     CDF V2.4.
*   V2.3   7-Dec-94, J Love     CDF V2.5.
*   V2.3a 10-Jan-95, J Love     Uppercase file extensions on the Macintosh.
*   V2.3b  6-Apr-95, J Love     POSIX.
*   V2.4   6-Sep-95, J Love     CDFexport-related changes.  Hyper groups.
*   V2.4a 19-Sep-95, J Love     CHECKforABORTso.
*   V2.4b 29-Sep-95, J Love     Macintosh dialog filtering.  Outline default
*                               button.  Less CHECKforABORTso.
*   V2.5   9-Sep-96, J Love     CDF V2.6.
*   V2.5a 21-Feb-97, J Love     Removed RICE.
*   V2.5b 17-Nov-97, J Love     Windows NT/Visual C++.
*   V2.5c 14-Dec-97, J Love     Added ALPHAVMSi encoding.
*   V2.6  17-Apr-01, M Liu      Added checking for variable names entered in
*                               the compression option
*   V2.7  03-May-06, M Liu      Added checksum option for the converted files.
*   V2.8  13-Oct-06, M Liu      Changed to allow upper and lower case CDF  
*                               name to be used on win32.
*   V3.3  10-Apr-09, M Liu      Changed MAC_ENCODING to PPC_ENCODING.
*   V3.3a  7-Jan-10, M Liu      Allowed 'none' compression option to have
*                               a blocking factor.
*   V3.32 20-Apr-11, M Liu      Added "epoch2tt2000", "tt20002epoch" or 
*                               "tt20002epoch16" option to convert epoch from
*                               CDF_EPOCH/CDF_EPOCH16 to CDF_TIME_TT2000 or 
*                               from CDF_TIME_TT2000 to CDF_EPOCH/CDF_EPOCH16.
*   V3.50 20-Sep-13, M Liu      For NRV variables, no compression is applied 
*                               if the record size is < 1K.
*   V3.51 11-Apr-14, M Liu      Added sort and blockingfactor option. 
*   V3.52 11-Aug-15, M Liu      Added "compressnonepoch" switch.
*
******************************************************************************/
#include "cdfcvt.h"
#include "cdflib.h"
#include "cdflib64.h"

/******************************************************************************
* Increased stack size for Borland C on IBM PC.
******************************************************************************/

#if defined(BORLANDC)
extern unsigned _stklen = BORLANDC_STACK_SIZE;
#endif

/******************************************************************************
* Local function prototypes.
******************************************************************************/

static void ConvertEpoch (long, int, long, double *, long long *);
static void PrepareSort (char *);
static CDFstatus ShuffleRecords (CDFid, Logical, struct HyperStruct, char *, long recSize);
static void AdjustTT2000Records (long count, char *, long recSize, int adjustLS);
static int SearchNeighbor (long, void *, void *, int *, int, int);

/******************************************************************************
* Global variables.
******************************************************************************/

Logical useSkeletonCDF;
Logical mLog, pctLog;
Logical deleteIFexists;
Logical negToPosFp0;
long dstFormat;
long dstMajority;
long dstEncoding;
long srcFormat, srcMajority, srcEncoding, srcVersion, srcRelease, srcIncrement;
long srcLastUpdated;
CDFid srcId, dstId, sktId;
long zMode;
Logical report[3];
Logical backward = FALSE;
long epochvsTT2000 = 0;
int checksumFlag;
long checksum, srcChecksum = -999;
long workingCache, stageCache, compressCache;
Logical dumpStatistics;
struct CompressionStruct *compression;
struct SparseRecordsStruct *sparseRecords;
Logical pctOn;
char sortVar[CDF_VAR_NAME_LEN256+1];
long sortMax = 0;
int  *sortIdx, *fillIdx;
long blockingFactorA;
int maxBF = 0;
int adjustLS = -1;
int lastUpdated = -1;
int compressNonEpoch = 0;

/******************************************************************************
* Main.
******************************************************************************/

#if !defined(win32) || (defined(win32) && defined(ALONE))
MAIN {
  Logical success = TRUE;
  strcpyX (pgmName, "CDFconvert", MAX_PROGRAM_NAME_LEN);
#if defined(mac)
  MacExecuteSO (ConvertCDFs, ConvertQOPs);
#else
  success = ConvertCDFs (argc, argv);
#endif
#if defined(DEBUG)
  if (cdf_FreeMemory(NULL,FatalError) > 0) DisplayWarning ("Abandoned buffers.");
#else
  cdf_FreeMemory (NULL, FatalError);
#endif
  return BOO(success,EXIT_SUCCESS_,EXIT_FAILURE_);
}
#endif

/******************************************************************************
* ConvertCDFs.
******************************************************************************/

Logical ConvertCDFs (argC, argV)
int argC;
char *argV[];
{
  QOP *qop;
  static char *validQuals[] = {
    "single", "multi", "row", "column", "host", "network", "skeleton", "log",
    "nolog", "percent", "nopercent", "delete", "nodelete", "zmode",
    "neg2posfp0", "noneg2posfp0", "report", "page", "nopage", "cache",
    "statistics", "nostatistics", "encoding", "compression", "sparseness",
    "about", "backward", "checksum", "epoch2tt2000", "tt20002epoch",
    "tt20002depoch", "sort", "blockingfactor", "adjusttt2000", 
    "leapsecondlastupdated", "compressnonepoch", NULL };
  static int optRequired[] = {
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE,
    TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE,
    TRUE, TRUE, FALSE, 0 };
  static char *reportTokens[] = { "errors", "warnings", "informationals" };
  CDFstatus status;
  Logical status2;
  char srcSpec[DU_MAX_PATH_LEN+1];
  char dstSpec[DU_MAX_PATH_LEN+1];
  char srcPath[DU_MAX_PATH_LEN+1];
  char dstPath[DU_MAX_PATH_LEN+1];
  char sktPath[DU_MAX_PATH_LEN+1];
  char CDFname[DU_MAX_NAME_LEN+1];
  long numCDFs;
  int i;
  char **directories = NULL;
  char **CDFnames = NULL;
  Logical qopError = FALSE;
  /****************************************************************************
  * Set global variables.
  ****************************************************************************/
  dstFormat = SOURCEformat;
  dstMajority = SOURCEmajority;
  dstEncoding = SOURCEencoding;
  /****************************************************************************
  * Get qualifiers/options/parameters.
  ****************************************************************************/
  switch (argC) {
    case 1:
      PageOLH ("cdfcvt.olh", argV[0]);
      return TRUE;
    case 2:
      if (strcmp(argV[1],"-java") == 0) {
        pagingOn = FALSE;
        PageOLH ("cdfcvtj.olh", argV[0]);
        return TRUE;
      }
    default:
      qop = Qop (argC, argV, validQuals, optRequired);
      if (qop == NULL) return FALSE;
      /************************************************************************
      * Check for `about' qualifier.
      ************************************************************************/
      if (qop->qualEntered[ABOUTqual]) {
	DisplayIdentification (pgmName);
	cdf_FreeMemory (qop, FatalError);
	return TRUE;
      }
      /************************************************************************
      * Get CDFs/paths to convert.
      ************************************************************************/
      switch (qop->Nparms) {
	case 0:
	  DisplayError ("Missing source and destination CDF specifications.");
	  qopError = TRUE;
	  break;
	case 1:
	  DisplayError ("Missing source or destination CDF specification.");
	  qopError = TRUE;
	  break;
	case 2:
	  strcpyX (srcSpec, qop->parms[CDFSPEC1parm], DU_MAX_PATH_LEN);
	  strcpyX (dstSpec, qop->parms[CDFSPEC2parm], DU_MAX_PATH_LEN);
#if defined(vms) || defined(dos)
	  MakeUpperString (srcSpec);
	  MakeUpperString (dstSpec);
#endif
	  break;
	default:
	  DisplayError ("Too many parameters.");
	  qopError = TRUE;
	  break;
      }
      if (IsWild(dstSpec)) {
	DisplayError ("Destination cannot be a wildcard specification.");
	qopError = TRUE;
      }
      if (IsDir(srcSpec) || IsWild(srcSpec)) {
	if (!IsDir(dstSpec)) {
	  DisplayError ("Destination must be a directory.");
	  qopError = TRUE;
	}
      }
      /************************************************************************
      * Check for skeleton CDF qualifier.
      ************************************************************************/
      if (qop->qualEntered[SKELqual])
	strcpyX (sktPath, qop->qualOpt[SKELqual], DU_MAX_PATH_LEN);
      else
	strcpyX (sktPath, "", DU_MAX_PATH_LEN);
      /************************************************************************
      * Check for sort qualifier.
      ************************************************************************/
      if (qop->qualEntered[SORTqual])
	strcpyX (sortVar, qop->qualOpt[SORTqual], CDF_VAR_NAME_LEN256);
      else
	strcpyX (sortVar, "", DU_MAX_PATH_LEN);
      /************************************************************************
      * Check for blockingfactor qualifier.
      ************************************************************************/
      if (qop->qualEntered[BLOCKINGFACTORqual]) {
        char *bf = qop->qualOpt[BLOCKINGFACTORqual];
	if (sscanf(bf, "%ld", &blockingFactorA) != 1) {
          if (strncmpIgCase (bf, "max", 3) != 0) {
            maxBF = 1;
          } else if (strncmpIgCase (bf, "optimal", 7) != 0) {
	    maxBF = 2;
	  } else {
            DisplayError ("Invalid blockingfactor string.");
            qopError = TRUE;
          }
        } else {
          if (blockingFactorA < 1) {
            DisplayError ("Invalid blockingfactor value.");
            qopError = TRUE;
          }
        }
      } else
	blockingFactorA = -1;
      /************************************************************************
      * Check for adjusttt2000 qualifier.
      ************************************************************************/
      if (qop->qualEntered[ADJUSTTT2000qual]) {
        char *tt2000ls = qop->qualOpt[ADJUSTTT2000qual];
	if (sscanf(tt2000ls, "%d", &adjustLS) != 1) {
            DisplayError ("Invalid adjusttt2000 value.");
            qopError = TRUE;
        }
        if (ValidateLeapSecondLastUpdated (adjustLS) == 0) {
            DisplayError ("Invalid adjusttt2000 value (not in leap second table).");
            qopError = TRUE;
        }
      }
      /************************************************************************
      * Check for leapsecondlastupdated qualifier.
      ************************************************************************/
      if (qop->qualEntered[LEAPSECONDLASTUPDATEDqual]) {
        char *newfield = qop->qualOpt[LEAPSECONDLASTUPDATEDqual];
	if (sscanf(newfield, "%d", &lastUpdated) != 1) {
            DisplayError ("Invalid leapsecondlastupdated value.");
            qopError = TRUE;
        }
        if (lastUpdated != 0 &&
            ValidateLeapSecondLastUpdated (lastUpdated) == 0) {
            DisplayError ("Invalid leapsecondlastupdated value (not in leap second table).");
            qopError = TRUE;
        }
      }
      /************************************************************************
      * Check for compressnonepoch qualifier.
      ************************************************************************/
      if (qop->qualEntered[COMPRESSNONEPOCHqual]) {
        compressNonEpoch = 1;
      }
      /************************************************************************
      * Check for an overriding format, majority, or encoding qualifier.
      ************************************************************************/
      qopError = qopError | !S2qualifierLong(qop,&dstFormat,SINGLEqual,
					     SINGLE_FILE,MULTIqual,MULTI_FILE,
					     dstFormat,"format");
      qopError = qopError | !S2qualifierLong(qop,&dstMajority,ROWqual,
					     ROW_MAJOR,COLqual,COLUMN_MAJOR,
					     dstMajority,"majority");
      qopError = qopError | !S2qualifierLong(qop,&dstEncoding,HOSTqual,
					     HOST_ENCODING,NETqual,
					     NETWORK_ENCODING,dstEncoding,
					     "encoding");
      if (qop->qualEntered[ENCODINGqual]) {
	if (dstEncoding != SOURCEencoding) {
	  DisplayError ("Conflicting qualifiers (encoding/host/network).");
	  qopError = TRUE;
	}
	else {
	  static char *encodingStrings[] = {
	    "host", "network", "sun", "vax", "decstation", "sgi",
	    "ibmpc", "ibmrs", "mac", "hp", "next", "alphaosf1",
	    "alphavmsd", "alphavmsg", "alphavmsi", NULL
	  };
	  static long encodings[] = {
	    HOST_ENCODING, NETWORK_ENCODING, SUN_ENCODING, VAX_ENCODING,
	    DECSTATION_ENCODING, SGi_ENCODING, IBMPC_ENCODING, IBMRS_ENCODING,
	    PPC_ENCODING, HP_ENCODING, NeXT_ENCODING, ALPHAOSF1_ENCODING,
	    ALPHAVMSd_ENCODING, ALPHAVMSg_ENCODING, ALPHAVMSi_ENCODING
	  };
	  int match = FindUniqueMatch (qop->qualOpt[ENCODINGqual],
				       encodingStrings);
	  switch (match) {
	    case NOMATCH:
	      DisplayError ("Unknown encoding.");
	      qopError = TRUE;
	      break;
	    case MATCHES:
	      DisplayError ("Ambiguous encoding.");
	      qopError = TRUE;
	      break;
	    default:
	      dstEncoding = encodings[match];
	      break;
	  }
	}
      }
      /************************************************************************
      * Check for /ZMODE,-zmode qualifier.
      ************************************************************************/
      if (qop->qualEntered[ZMODEqual]) {
	switch (qop->qualOpt[ZMODEqual][0]) {
	  case '0': zMode = zMODEoff; break;
	  case '1': zMode = zMODEon1; break;
	  case '2': zMode = zMODEon2; break;
	  default: {
	    DisplayError ("Illegal zMode.");
	    qopError = TRUE;
	  }
	}
      }
      else
	zMode = DEFAULTzModeCVT;
      /*********************************************************************
      * Check for `cache' qualifier.
      *********************************************************************/
      if (qop->qualEntered[CACHEqual]) {
	if (!ParseCacheSizes(qop->qualOpt[CACHEqual],
			     &workingCache,&stageCache,&compressCache)) {
	  DisplayError ("Illegal cache size/type.");
	  qopError = TRUE;
	}
      }
      else {
	workingCache = useDEFAULTcacheSIZE;
	stageCache = useDEFAULTcacheSIZE;
	compressCache = useDEFAULTcacheSIZE;
      }
      /************************************************************************
      * Check for `report' qualifier.  If absent, use defaults.
      ************************************************************************/
      if (qop->qualEntered[REPORTqual]) {
	if (!ParseOptionList(3,reportTokens,qop->qualOpt[REPORTqual],report)) {
	  DisplayError ("Illegal list of `report' options.");
	  qopError = TRUE;
	}
      }
      else {
	report[ERRORs] = REPORTerrorsDEFAULT;
	report[WARNs] = REPORTwarningsDEFAULT;
	report[INFOs] = REPORTinfosDEFAULT;
      }
      /************************************************************************
      * Check for `compression' qualifier.
      ************************************************************************/
      if (qop->qualEntered[COMPRESSqual]) {
	compression = ParseCompressionOptions (qop->qualOpt[COMPRESSqual]);
	if (compression == NULL) {
	  DisplayError ("Illegal `compression' options.");
	  qopError = TRUE;
	}
      }
      else
	compression = NULL;
      /************************************************************************
      * Check for `sparseness' qualifier.
      *
      * NOTE: When sparse arrays are added the call to `ParseSparsenessOptions'
      * will have to be changed to return both `sparseRecords' and
      * `sparseArrays' (or something like that).
      ************************************************************************/
      if (qop->qualEntered[SPARSEqual]) {
	sparseRecords = ParseSparsenessOptions (qop->qualOpt[SPARSEqual]);
	if (sparseRecords == NULL) {
	  DisplayError ("Illegal `sparseness' options.");
	  qopError = TRUE;
	}
      }
      else
	sparseRecords = NULL;
      /************************************************************************
      * Check for `backward' qualifier.
      ************************************************************************/
      if (qop->qualEntered[BACKWARDqual]) {
        CDFsetFileBackward(BACKWARDFILEon);
        backward = TRUE;
      }
      /************************************************************************
      * Check for `epoch2tt2000', `tt20002epoch' or `tt20002epoch16' qualifier.
      ************************************************************************/
      qopError = qopError | !S3qualifierLong(qop,&epochvsTT2000,
                                             EPOCH2TT2000qual,EPOCH2TT2000_,
                                             TT20002EPOCHqual,TT20002EPOCH_,
                                             TT20002EPOCH16qual,TT20002EPOCH16_,
                                             0L,"epoch conversion");
      /************************************************************************
      * Check for `checksum' qualifier.
      ************************************************************************/
      if (qop->qualEntered[CHECKSUMqual]) {
        char *cks = qop->qualOpt[CHECKSUMqual];
	if (StrStrIgCaseX(cks, "no") || StrStrIgCaseX(cks, "none")) {
          checksumFlag = 0;
	  checksum = NO_CHECKSUM;
	} else if (StrStrIgCaseX(cks, "md5")) {
           checksumFlag = 1;
           checksum = MD5_CHECKSUM;
        } else if (StrStrIgCaseX(cks, "source")) {
           checksumFlag = 2;
        } else {
            DisplayError ("Illegal checksum method.");
            qopError = TRUE;
        }
      }
      else
        checksumFlag = -1;

      /************************************************************************
      * Check for `page', `log', `percent', `delete', `neg2posfp0', and
      * `statistics' qualifiers.
      ************************************************************************/
      qopError = qopError | !TFqualifier (qop,&pagingOn,PAGEqual,NOPAGEqual,
					  DEFAULTpageCVT,"page");
      qopError = qopError | !TFqualifier (qop,&mLog,LOGqual,NOLOGqual,
					  DEFAULTlogCVT,"log");
      qopError = qopError | !TFqualifier (qop,&pctLog,PCTqual,NOPCTqual,
					  DEFAULTpctCVT,"percent");
      qopError = qopError | !TFqualifier (qop,&deleteIFexists,DELqual,
					  NODELqual,DEFAULTdelCVT,"delete");
      qopError = qopError | !TFqualifier (qop,&negToPosFp0,NEG2POSqual,
					  NONEG2POSqual,DEFAULT_NEGtoPOSfp0,
					  "neg2posfp0");
      qopError = qopError | !TFqualifier (qop,&dumpStatistics,STATSqual,
					  NOSTATSqual,DEFAULTstatsCVT,
					  "neg2posfp0");
      /************************************************************************
      * Check for qualifier compatibility.
      ************************************************************************/
      if (pctLog && (!mLog)) {
	DisplayError ("`log' must be used with `percentage'.");
	qopError = TRUE;
      }
      /************************************************************************
      * Free QOP memory and check for an error.
      ************************************************************************/
      cdf_FreeMemory (qop, FatalError);
      if (qopError) {
	if (compression != NULL) FreeCompression (compression);
	if (sparseRecords != NULL) FreeSparseness (sparseRecords);
	return FALSE;
      }
      break;
  }

  /****************************************************************************
  * If a skeleton CDF was specified, open it and set the destination format,
  * majority, and encoding (unless they were explicitly specified).
  ****************************************************************************/
  if (!NULstring(sktPath)) {
    long sktFormat, sktMajority, sktEncoding;
#if defined(vms) || defined(dos)
    MakeUpperString (sktPath);
#endif
    WriteOut (stdout, "Opening Skeleton CDF: ");
    WriteOut (stdout, sktPath);
    WriteOut (stdout, "\n");
    status = CDFlib (OPEN_, CDF_, sktPath, &sktId,
		     GET_, CDF_FORMAT_, &sktFormat,
			   CDF_MAJORITY_, &sktMajority,
			   CDF_ENCODING_, &sktEncoding,
			   CDF_VERSION_, &srcVersion,
                           CDF_RELEASE_, &srcRelease,
                           CDF_INCREMENT_, &srcIncrement,
		     NULL_);
    if (StatusBAD(status)) {
       StatusHandlerCvt ("SKT", status);
       useSkeletonCDF = FALSE;
    }
    if (!PriorTo ("3.2.0", srcVersion, srcRelease, srcIncrement)) {
      status = CDFlib (GET_, CDF_CHECKSUM_, &srcChecksum,
                       NULL_);
      if (StatusBAD(status)) {
         StatusHandlerCvt ("SKT", status);
         useSkeletonCDF = FALSE;
      }
    }
    else {
      useSkeletonCDF = TRUE;
      if (dstFormat == SOURCEformat) dstFormat = sktFormat;
      if (dstMajority == SOURCEmajority) dstMajority = sktMajority;
      if (dstEncoding == SOURCEencoding) dstEncoding = sktEncoding;
    }
  }
  else
    useSkeletonCDF = FALSE;
  /****************************************************************************
  * Convert CDFs.
  ****************************************************************************/
  if (IsDir(srcSpec) || IsWild(srcSpec)) {
    numCDFs = CDFdirList (srcSpec, &directories, &CDFnames);
    if (numCDFs < 1) {
      WriteOut (stdout, "No CDFs found in source directory.\n");
    }
    else {
      for (i = 0; i < numCDFs; i++) {
	 strcpyX (srcPath, directories[i], DU_MAX_PATH_LEN);
	 AppendToDir (srcPath, CDFnames[i]);
	 strcpyX (dstPath, dstSpec, DU_MAX_PATH_LEN);
	 AppendToDir (dstPath, CDFnames[i]);
	 status2 = ConvertCDF (srcPath, dstPath);
      }
    }
  }
  else {
    if (IsDir(dstSpec)) {
      ParsePath (srcSpec, NULL, CDFname);
      strcpyX (dstPath, dstSpec, DU_MAX_PATH_LEN);
      AppendToDir (dstPath, CDFname);
      status2 = ConvertCDF (srcSpec, dstPath);
    }
    else {
      status2 = ConvertCDF (srcSpec, dstSpec);
    }
  }
  if (directories != NULL) cdf_FreeMemory (directories, FatalError);
  if (CDFnames != NULL) cdf_FreeMemory (CDFnames, FatalError);
  if (compression != NULL) FreeCompression (compression);
  if (sparseRecords != NULL) FreeSparseness (sparseRecords);
  /****************************************************************************
  * Close skeleton CDF if one was used.
  ****************************************************************************/
  if (useSkeletonCDF) {
    status = CDFclose (sktId);
    StatusHandlerCvt ("SKT", status);	/* Ignore return. */
  }
  return status2;
}

/******************************************************************************
* ConvertCDF.
******************************************************************************/

Logical ConvertCDF (srcPath, dstPath)
char *srcPath;
char *dstPath;
{
  return ConvertCDFx (srcPath, dstPath);
}

/******************************************************************************
* ConvertCDFx.
******************************************************************************/

Logical ConvertCDFx (srcPath, dstPath)
char *srcPath;
char *dstPath;
{
  CDFstatus status; int pN;
  long format, majority, encoding, version, release, increment;
  long numDims, dimSizes[CDF_MAX_DIMS];
  long SRCcType, SRCcParms[CDF_MAX_PARMS], SRCcPct;
  long cType, cParms[CDF_MAX_PARMS];
  long numVars, numZvars;
  char text[MAX_OUTPUT_TEXT_LEN+1];
  /***************************************************************************
  * Display converting message.
  ***************************************************************************/
  WriteOut (stdout, "Converting \"");
  WriteOut (stdout, srcPath);
  if (!EndsWithIgCase(srcPath, ".cdf"))
    WriteOut (stdout, ".cdf");
  WriteOut (stdout, "\" to \"");
  WriteOut (stdout, dstPath);
  if (!EndsWithIgCase(dstPath, ".cdf"))
    WriteOut (stdout, ".cdf");
  WriteOut (stdout, "\"\n");
  
  /***************************************************************************
  * Open source CDF.  If an error occurs, skip to next CDF.
  ***************************************************************************/
  if (mLog) WriteOut (stdout, "  Opening source CDF...\n");
  status = CDFlib (OPEN_, CDF_, srcPath, &srcId,
                   GET_, CDF_NUMrVARS_, &numVars,                     
		         CDF_NUMzVARS_, &numZvars,
	           NULL_);
  if (!StatusHandlerCvt("SRC",status)) return FALSE;
  status = CDFlib (SELECT_, CDF_, srcId,
		   SELECT_, CDF_READONLY_MODE_, READONLYon,
			    CDF_zMODE_, zMode,
			    CDF_NEGtoPOSfp0_MODE_, BOO(negToPosFp0,
						       NEGtoPOSfp0on,
						       NEGtoPOSfp0off),
			    CDF_CACHESIZE_, workingCache,
			    STAGE_CACHESIZE_, stageCache,
			    COMPRESS_CACHESIZE_, compressCache,
		   GET_, CDF_FORMAT_, &srcFormat,
                         CDF_VERSION_, &version,
                         CDF_RELEASE_, &release,
                         CDF_INCREMENT_, &increment,
		         CDF_NUMrVARS_, &numVars,
		         CDF_NUMzVARS_, &numZvars,
			 CDF_MAJORITY_, &srcMajority,
			 CDF_ENCODING_, &srcEncoding,
			 CDF_COMPRESSION_, &SRCcType, SRCcParms, &SRCcPct,
			 rVARs_NUMDIMS_, &numDims,
			 rVARs_DIMSIZES_, dimSizes,
		   NULL_);
  if (!StatusHandlerCvt("SRC",status)) return FALSE;
  if (!PriorTo ("3.2.0", (Int32) version, (Int32) release, (Int32) increment)) {
    status = CDFlib (GET_, CDF_CHECKSUM_, &srcChecksum,
                     NULL_);
    if (!StatusHandlerCvt("SRC",status)) return FALSE;
  }
  CHECKforABORTso
  if (!PriorTo ("3.5.0", version, release, increment)) {
    status = CDFlib (GET_, CDF_LEAPSECONDLASTUPDATED_, &srcLastUpdated,
                     NULL_);
    if (!StatusHandlerCvt("SRC",status)) return FALSE;
  }
  else {
    srcLastUpdated = -1;
  }
  if (CDFgetFileBackward()) {
    /*************************************************************************
    * Check the size of source CDF.  Give errors if it's over 2G when
    * converting it to V2.7 version. It's only applicable to V3.* files.
    *************************************************************************/
    if (version >= 3) {
      OFF_T cSize, uSize;
      status = CDFlib (GET_, CDF_INFO_, srcPath, &SRCcType, SRCcParms,
                                        &cSize, &uSize,
                       NULL_);
      if (!StatusHandlerCvt("SRC",status)) return FALSE;
#if defined(win32)
      if ((cSize >= (OFF_T) ((1i64 << 31) - 1)) ||
          (uSize >= (OFF_T) ((1i64 << 31) - 1))) {
#else
      if ((cSize >= (OFF_T) ((1LL << 31) - 1)) ||
          (uSize >= (OFF_T) ((1LL << 31) - 1))) {
#endif
        WriteOut (stdout, "Soruce file size: ");
#if defined(win32) || defined(__MINGW32__)
        snprintf (text, (size_t) sizeof(text),
		  "cSize(%I64d) uSize(%I64d)", (long long) cSize, (long long) uSize);
#else
        snprintf (text, (size_t) sizeof(text),
		  "cSize(%lld) uSize(%lld)", (long long) cSize, (long long) uSize);
#endif
        WriteOut (stdout, text);
        WriteOut (stdout, " -- is too big to be converted to a V2.7 file. \n");
	status = CDFlib (CLOSE_, CDF_,
		         NULL_);
        if (sortMax > 0) {
          free (sortIdx);
          sortMax = 0;
        }
        return FALSE;
      }
    }
  }

  /***************************************************************************
  * Determine format, majority, encoding, and compression of destination CDF.
  ***************************************************************************/
  format = BOO(dstFormat == SOURCEformat,srcFormat,dstFormat);
  majority = BOO(dstMajority == SOURCEmajority,srcMajority,dstMajority);
  encoding = BOO(dstEncoding == SOURCEencoding,srcEncoding,dstEncoding);
  cType = SRCcType;
  for (pN = 0; pN < CDF_MAX_PARMS; pN++) cParms[pN] = SRCcParms[pN];
  if (compression != NULL) {
    if (compression->CDF.cType != SOURCEcompression) {
      cType = compression->CDF.cType;
      for (pN = 0; pN < CDF_MAX_PARMS; pN++) {
	 cParms[pN] = compression->CDF.cParms[pN];
      }
    }
  }
  /***************************************************************************
  * Create destination CDF.  Note that the cache size is selected after the
  * CDF's format is specified.  This is because changing a CDF's format will
  * also cause it's cache size to be changed.
  ***************************************************************************/
  if (mLog) WriteOut (stdout, "  Creating destination CDF...\n");
  status = CDFlib (CREATE_, CDF_, dstPath, numDims, dimSizes, &dstId,
		   NULL_);
  if (status == CDF_EXISTS && deleteIFexists) {
    status = CDFlib (OPEN_, CDF_, dstPath, &dstId,
		     DELETE_, CDF_,
		     NULL_);
    if (!StatusHandlerCvt("DST",status)) {
      WriteOut (stdout, "  Unabled to delete existing destination CDF.\n");
      if (mLog) WriteOut (stdout, "  Closing source CDF...\n");
      StatusHandlerCvt ("SRC", CDFclose(srcId));
      return FALSE;
    }
    status = CDFlib (CREATE_, CDF_, dstPath, numDims, dimSizes, &dstId,
		     NULL_);
  }
  if (!StatusHandlerCvt("DST",status)) {
    if (mLog) WriteOut (stdout, "  Closing source CDF...\n");
    StatusHandlerCvt ("SRC", CDFclose(srcId));
    return FALSE;
  }
  CHECKforABORTso
  status = CDFlib (SELECT_, CDF_, dstId,
		   PUT_, CDF_FORMAT_, format,
			 CDF_MAJORITY_, majority,
			 CDF_ENCODING_, encoding,
/*			 CDF_COMPRESSION_, cType, cParms, */
		   SELECT_, CDF_CACHESIZE_, workingCache,
			    STAGE_CACHESIZE_, stageCache,
/*			    COMPRESS_CACHESIZE_, compressCache, */
		   NULL_);
  if (!StatusHandlerCvt("DST",status)) {
    if (mLog) WriteOut (stdout, "  Closing destination CDF...\n");
    StatusHandlerCvt ("DST", CDFclose(dstId));
    if (mLog) WriteOut (stdout, "  Closing source CDF...\n");
    StatusHandlerCvt ("SRC", CDFclose(srcId));
    return FALSE;
  }
  CHECKforABORTso

  if (compressNonEpoch == 0) {
    status = CDFlib (SELECT_, CDF_, dstId,
		     PUT_, CDF_COMPRESSION_, cType, cParms,
		     SELECT_, COMPRESS_CACHESIZE_, compressCache,
		     NULL_);
    if (!StatusHandlerCvt("DST",status)) {
      if (mLog) WriteOut (stdout, "  Closing destination CDF...\n");
      StatusHandlerCvt ("DST", CDFclose(dstId));
      if (mLog) WriteOut (stdout, "  Closing source CDF...\n");
      StatusHandlerCvt ("SRC", CDFclose(srcId));
      return FALSE;
    }
  }

  if (!backward) {
    if (lastUpdated >= 0) {
      status = CDFlib (SELECT_, CDF_, dstId,
                       PUT_, CDF_LEAPSECONDLASTUPDATED_, (long) lastUpdated,
                       NULL_);
      if (!StatusHandlerCvt("DST",status)) {
        if (mLog) WriteOut (stdout, "  Closing destination CDF...\n");
        StatusHandlerCvt ("DST", CDFclose(dstId));
        if (mLog) WriteOut (stdout, "  Closing source CDF...\n");
        StatusHandlerCvt ("SRC", CDFclose(srcId));
        return FALSE;
      }
    }
    if (adjustLS > -1) {
      status = CDFlib (SELECT_, CDF_, dstId,
                       PUT_, CDF_LEAPSECONDLASTUPDATED_, (long) adjustLS,
                       NULL_);
      if (!StatusHandlerCvt("DST",status)) {
        if (mLog) WriteOut (stdout, "  Closing destination CDF...\n");
        StatusHandlerCvt ("DST", CDFclose(dstId));
        if (mLog) WriteOut (stdout, "  Closing source CDF...\n");
        StatusHandlerCvt ("SRC", CDFclose(srcId));
        return FALSE;
      }
    }
    if (checksumFlag == 0 || checksumFlag == 1) { 	/* no or md5 */
      status = CDFlib (SELECT_, CDF_, dstId,
                       PUT_, CDF_CHECKSUM_, checksum,
                       NULL_);
    } else if (checksumFlag == 2) { 			/* source */
      if (srcChecksum != -999) {
        status = CDFlib (SELECT_, CDF_, dstId,
                         PUT_, CDF_CHECKSUM_, srcChecksum,
                         NULL_);
      }
    } else {					/* not specified */
      int env = CDFgetChecksumEnvVar();
      if (env == 0) {  			/* env var not set ior set to no */
        if (srcChecksum != -999)
          status = CDFlib (SELECT_, CDF_, dstId,
                           PUT_, CDF_CHECKSUM_, srcChecksum,
                           NULL_);
      } else					/* env var set to md5 */
        status = CDFlib (SELECT_, CDF_, dstId,
                         PUT_, CDF_CHECKSUM_, MD5_CHECKSUM,
                         NULL_);
    }
  }
  if (!StatusHandlerCvt("DST",status)) {
    if (mLog) WriteOut (stdout, "  Closing destination CDF...\n");
    StatusHandlerCvt ("DST", CDFclose(dstId));
    if (mLog) WriteOut (stdout, "  Closing source CDF...\n");
    StatusHandlerCvt ("SRC", CDFclose(srcId));
    return FALSE;
  }
  CHECKforABORTso
  /***************************************************************************
  * Convert attributes.  If that is successful, convert variables.
  ***************************************************************************/
  if (!ConvertAttributes()) {
    if (mLog) WriteOut (stdout, "  Closing source CDF...\n");
    StatusHandlerCvt ("SRC", CDFclose(srcId));
    if (mLog) WriteOut (stdout, "  Closing destination CDF...\n");
    StatusHandlerCvt ("DST", CDFclose(dstId));
    return FALSE;
  }
  /***************************************************************************
  * Handle sort variable.
  ***************************************************************************/
  if (!NULstring (sortVar)) {
    PrepareSort (sortVar);
  }
  /***************************************************************************
  * Convert variables.
  ***************************************************************************/
  if (!ConvertVariables (srcMajority, majority)) {
    if (mLog) WriteOut (stdout, "  Closing source CDF...\n");
    StatusHandlerCvt ("SRC", CDFclose(srcId));
    if (mLog) WriteOut (stdout, "  Closing destination CDF...\n");
    StatusHandlerCvt ("DST", CDFclose(dstId));
    return FALSE;
  }
  /***************************************************************************
  * Close source and destination CDFs.
  ***************************************************************************/
  if (srcLastUpdated != -1 && lastUpdated == -1 && adjustLS == -1) {
    status = CDFlib (SELECT_, CDF_, dstId,
                     PUT_, CDF_LEAPSECONDLASTUPDATED_, (long) srcLastUpdated,
                     NULL_);
    StatusHandlerCvt ("DST", status);
  }
  if (dumpStatistics) {
    vSTATS vStatsDotCDF, vStatsStage, vStatsCompress;
    if (mLog) WriteOut (stdout, "  Closing source CDF...\n");
    status = CDFlib (SELECT_, CDF_, srcId,
		     CLOSE_, CDFwithSTATS_, &vStatsDotCDF,
					    &vStatsStage,
					    &vStatsCompress,
		     NULL_);
    StatusHandlerCvt ("SRC", status);
    CHECKforABORTso
    DisplayStatistics ("source", &vStatsDotCDF, &vStatsStage, &vStatsCompress);
    if (mLog) WriteOut (stdout, "  Closing destination CDF...\n");
    status = CDFlib (SELECT_, CDF_, dstId,
		     CLOSE_, CDFwithSTATS_, &vStatsDotCDF,
					    &vStatsStage,
					    &vStatsCompress,
		     NULL_);
    StatusHandlerCvt ("DST", status);
    DisplayStatistics ("destination", &vStatsDotCDF, &vStatsStage,
		       &vStatsCompress);
  }
  else {
    if (mLog) WriteOut (stdout, "  Closing source CDF...\n");
    status = CDFlib (SELECT_, CDF_, srcId,
		     CLOSE_, CDF_,
		     NULL_);
    StatusHandlerCvt ("SRC", status);
    CHECKforABORTso
    if (mLog) WriteOut (stdout, "  Closing destination CDF...\n");
    status = CDFlib (SELECT_, CDF_, dstId,
		     CLOSE_, CDF_,
		     NULL_);
    StatusHandlerCvt ("DST", status);
  }
  if (sortMax > 0) free (sortIdx);
  return TRUE;
}

/******************************************************************************
* ConvertAttributes.
******************************************************************************/

Logical ConvertAttributes ()
{
  CDFstatus status;
  long attrNum, entryNum, ignoredNum;
  long numAttrs, maxEntry, maxZentry;
  long scope;
  char attrName[CDF_ATTR_NAME_LEN256 + 1];

  /****************************************************************************
  * Inquire number of attributes to be converted.
  ****************************************************************************/

  if (mLog) WriteOut (stdout, "  Converting attributes...\n");

  status = CDFlib (SELECT_, CDF_, srcId,
		   GET_, CDF_NUMATTRS_, &numAttrs,
		   NULL_);
  if (!StatusHandlerCvt("SRC",status)) return FALSE;

  for (attrNum = 0; attrNum < numAttrs; attrNum++) {
     /*************************************************************************
     * Inquire attribute in source CDF.
     *************************************************************************/
  
     status = CDFlib (SELECT_, CDF_, srcId,
			       ATTR_, attrNum,
		      GET_, ATTR_NAME_, attrName,
			    ATTR_SCOPE_, &scope,
		      NULL_);
     if (!StatusHandlerCvt("SRC",status)) return FALSE;

     if (mLog) {
       WriteOut (stdout, "    Converting ");
       WriteOut (stdout, BOO(scope == GLOBAL_SCOPE,"g","v"));
       WriteOut (stdout, "Attribute \"");
       WriteOut (stdout, attrName);
       WriteOut (stdout, "\"\n");
     }

     /*************************************************************************
     * Create new attribute in destination CDF.
     *************************************************************************/

     status = CDFlib (SELECT_, CDF_, dstId,
		      CREATE_, ATTR_, attrName, scope, &ignoredNum,
		      NULL_);
     if (!StatusHandlerCvt("DST",status)) return FALSE;

     /*************************************************************************
     * Copy entries from source CDF to destination CDF.
     *************************************************************************/

     if (scope == GLOBAL_SCOPE) {
       status = CDFlib (SELECT_, CDF_, srcId,
			GET_, ATTR_MAXgENTRY_, &maxEntry,
			NULL_);
       if (!StatusHandlerCvt("SRC",status)) return FALSE;
       if (mLog) {
	 WriteOut (stdout, "      Converting entries...\n");
       }
       for (entryNum = 0; entryNum <= maxEntry; entryNum++) {
	  if (!ConvertEntry(attrName,entryNum,gENTRYt)) return FALSE;
       }
     }
     else {
       status = CDFlib (SELECT_, CDF_, srcId,
			GET_, ATTR_MAXrENTRY_, &maxEntry,
			      ATTR_MAXzENTRY_, &maxZentry,
			NULL_);
       if (!StatusHandlerCvt("SRC",status)) return FALSE;
       if (mLog) {
	 WriteOut (stdout, "      Converting entries...\n");
       }
       for (entryNum = 0; entryNum <= maxEntry; entryNum++) {
	  if (!ConvertEntry(attrName,entryNum,rENTRYt)) return FALSE;
       }
       for (entryNum = 0; entryNum <= maxZentry; entryNum++) {
	  if (!ConvertEntry(attrName,entryNum,zENTRYt)) return FALSE;
       }
     }
     CHECKforABORTso
  }

  return TRUE;
}

/******************************************************************************
* ConvertEntry.
******************************************************************************/

Logical ConvertEntry (attrName, entryNum, entryType)
char *attrName;
long entryNum;
int entryType;
{
  CDFstatus status;
  long dataType;
  long numElements;
  void *value;
  int rzEntry;
  char varName[CDF_VAR_NAME_LEN256+1];

  if (entryType == gENTRYt)
    rzEntry = 0;
  else 
    rzEntry = 1;
  status = CDFlib (SELECT_, CDF_, srcId,
			    ENTRY(entryType), entryNum,
		   GET_, ENTRY_DATATYPE(entryType), &dataType,
			 ENTRY_NUMELEMS(entryType), &numElements,
                   NULL_);
  if (status != NO_SUCH_ENTRY) {
    if (rzEntry == 1) {
      status = CDFlib (SELECT_, (entryType == rENTRYt ? rVAR_ : zVAR_), entryNum,
                       GET_, (entryType == rENTRYt ? rVAR_NAME_ : zVAR_NAME_),
                             varName,
                       NULL_);
    }
    if (!StatusHandlerCvt("SRC",status)) return FALSE;
    value = cdf_AllocateMemory ((size_t) (CDFelemSize(dataType) * numElements),
			    FatalError);
    status = CDFlib (SELECT_, CDF_, srcId,
		     GET_, ENTRY_DATA(entryType), value,
		     NULL_);
    if (!StatusHandlerCvt("SRC",status)) return FALSE;
    if (epochvsTT2000 == 0 || ((dataType != CDF_EPOCH &&
                                dataType != CDF_EPOCH16 &&
                                dataType != CDF_TIME_TT2000) &&
                               (!StrStrIgCaseX(attrName, "units") &&
                                !StrStrIgCaseX(attrName, "fillval") &&
                                !StrStrIgCaseX(attrName, "validmin") &&
                                !StrStrIgCaseX(attrName, "validmax")))) {
      status = CDFlib (SELECT_, CDF_, dstId,
  			        ENTRY(entryType), entryNum,
		       PUT_, ENTRY_DATA(entryType), dataType, numElements,
			     value,
		       NULL_);
      if (!StatusHandlerCvt("DST",status)) return FALSE;
    } else {
      if (epochvsTT2000 == 1 && ((dataType == CDF_EPOCH ||
                                  dataType == CDF_EPOCH16) ||
                                 (StrStrIgCaseX(varName, "epoch") &&
                                  (StrStrIgCaseX(attrName, "units") ||
                                   StrStrIgCaseX(attrName, "fillval") ||
                                   StrStrIgCaseX(attrName, "validmin") ||
                                   StrStrIgCaseX(attrName, "validmax"))))) {
        if ((dataType == CDF_EPOCH || dataType == CDF_EPOCH16) ||
            (!StrStrIgCaseX(attrName, "units"))) {
          long long *valueX;
          valueX = (long long *) cdf_AllocateMemory ((size_t)numElements * 8, 
                                                   FatalError);
          if (valueX == NULL) return FALSE;
          ConvertEpoch (dataType, (int)numElements, 1L, (double *)value,
                        valueX);
          status = CDFlib (SELECT_, CDF_, dstId,
                                    ENTRY(entryType), entryNum,
                           PUT_, ENTRY_DATA(entryType), CDF_TIME_TT2000,
	    	    	         numElements, valueX,
                           NULL_);
          cdf_FreeMemory (valueX, FatalError);
          if (!StatusHandlerCvt("DST",status)) return FALSE;
        } else {
          status = CDFlib (SELECT_, CDF_, dstId,
                                    ENTRY(entryType), entryNum,
                           PUT_, ENTRY_DATA(entryType), CDF_CHAR,
                                 2L, "ns",
                           NULL_);
          if (!StatusHandlerCvt("DST",status)) return FALSE;
        }
      } else if ((epochvsTT2000 == 2 || epochvsTT2000 == 3) &&
                 dataType == CDF_TIME_TT2000) {
        double *valueX;
        int step = (epochvsTT2000 == 2 ? 1 : 2);
        valueX = (double *) cdf_AllocateMemory ((size_t)numElements * 8 * step,
                                                FatalError);
        if (valueX == NULL) return FALSE;
        ConvertEpoch (dataType, (int)numElements, epochvsTT2000,
                      valueX, (long long *)value);
        status = CDFlib (SELECT_, CDF_, dstId,
                                  ENTRY(entryType), entryNum,
                         PUT_, ENTRY_DATA(entryType),
                               (epochvsTT2000 == 2 ? CDF_EPOCH : CDF_EPOCH16),
                               numElements, valueX,
                         NULL_);
        cdf_FreeMemory (valueX, FatalError);
        if (!StatusHandlerCvt("DST",status)) return FALSE;
      } else {
        status = CDFlib (SELECT_, CDF_, dstId,
                                  ENTRY(entryType), entryNum,
                         PUT_, ENTRY_DATA(entryType), dataType, numElements,
                               value,
                         NULL_);
        if (!StatusHandlerCvt("DST",status)) return FALSE;
      }
      cdf_FreeMemory (value, FatalError);
    }
  }
  CHECKforABORTso
  return TRUE;
}

/******************************************************************************
* ConvertVariables.
******************************************************************************/

Logical ConvertVariables (SRCmajority, DSTmajority)
long SRCmajority;
long DSTmajority;
{
  CDFstatus status;
  long numVars, numZvars;
  long varNum;

  if (mLog) WriteOut (stdout, "  Converting variables...\n");

  /****************************************************************************
  * Inquire number of variables to be converted.
  ****************************************************************************/

  status = CDFlib (SELECT_, CDF_, srcId,
		   GET_, CDF_NUMrVARS_, &numVars,
			 CDF_NUMzVARS_, &numZvars,
		   NULL_);
  if (!StatusHandlerCvt("SRC",status)) return FALSE;

  if (compression != NULL) CheckEnteredVarNames();
  for (varNum = 0; varNum < numVars; varNum++) {
     if (!ConvertVariable(varNum,FALSE,SRCmajority,DSTmajority)) return FALSE;
  }

  for (varNum = 0; varNum < numZvars; varNum++) {
     if (!ConvertVariable(varNum,TRUE,SRCmajority,DSTmajority)) return FALSE;
  }

  return TRUE;
}

/******************************************************************************
* CheckEnteredVarNames.
******************************************************************************/

void CheckEnteredVarNames ()
{
  CDFstatus status; 
  struct cVarStruct *Var;
  long numVars, numZvars;
  long varNum;
  Logical found;
  char varName[CDF_VAR_NAME_LEN256 + 1];

  /****************************************************************************
  * Inquire number of variables to be checked.
  ****************************************************************************/

  status = CDFlib (SELECT_, CDF_, srcId,
                   GET_, CDF_NUMrVARS_, &numVars,
                         CDF_NUMzVARS_, &numZvars,
                   NULL_);
  if (!StatusHandlerCvt("SRC",status)) return;

  for (Var = compression->VARhead; Var != NULL; Var = Var->next) {
     found = FALSE;
     for (varNum = 0; varNum < numVars; varNum++) {
       status = CDFlib (SELECT_, CDF_, srcId,
                                 rVAR_, varNum,
                        GET_, rVAR_NAME_, varName,
                        NULL_);
       if (!StatusHandlerCvt("SRC",status)) return;
       if (!strcmpITB(Var->name,varName)) {
         found = TRUE;
         break;
       }
     }
     if (found) continue;
     for (varNum = 0; varNum < numZvars; varNum++) {
       status = CDFlib (SELECT_, CDF_, srcId,
                                 zVAR_, varNum,
                        GET_, zVAR_NAME_, varName,
                        NULL_);
       if (!StatusHandlerCvt("SRC",status)) return;
       if (!strcmpITB(Var->name,varName)) {
         found = TRUE;
         break;
       }
     }
     if (found) continue;
     WriteOut (stdout, "Warning: variable name: ");
     WriteOut (stdout, Var->name);
     WriteOut (stdout, " not found in the CDF...\n");
  }
}

/******************************************************************************
* ConvertVariable.
******************************************************************************/

Logical ConvertVariable (varNum, Z, SRCmajority, DSTmajority)
long varNum;
Logical Z;
long SRCmajority;
long DSTmajority;
{
   CDFstatus status; long cPct, sArraysPct;
   long ignoredNum, dataType, numElements, recVary, dimVarys[CDF_MAX_DIMS];
   long numDims, dimSizes[CDF_MAX_DIMS], NvalueBytes, maxRec, blockingFactor;
   long cType, cParms[CDF_MAX_PARMS], sArraysType, sArraysParms[CDF_MAX_PARMS],
	sRecordsType, reservePct;
   char varName[CDF_VAR_NAME_LEN256 + 1]; long recNum, fromRec, toRec;
   int dimN; void *padValue; long nValuesPerRecord, nRecords;
   Byte1 **handles[2], *buffer1, *buffer2;
   size_t nValueBytes[2]; long nHypers, nValues, hyperN;
   struct GroupStruct groups; struct HyperStruct hyper;
   Logical srcRowMajor = ROWmajor(SRCmajority), switchMajority;
   static char cvtMsg[] = "      converting variable values...    ";
   long dataTypeX;
   int once = 0;
   /***************************************************************************
   * Inquire existing variable in source CDF.
   ***************************************************************************/
   status = CDFlib (SELECT_, CDF_, srcId,
			     VAR(Z), varNum,
		    GET_, VAR_NAME(Z), varName,
			  VAR_DATATYPE(Z), &dataType,
			  VAR_NUMELEMS(Z), &numElements,
			  VAR_RECVARY(Z), &recVary,
			  VAR_DIMVARYS(Z), dimVarys,
			  VAR_MAXREC(Z), &maxRec,
			  VAR_BLOCKINGFACTOR(Z), &blockingFactor,
			  VAR_SPARSERECORDS(Z), &sRecordsType,
/*			  VAR_SPARSEARRAYS(Z), &sArraysType, */
/*					       sArraysParms, */
/*					       &sArraysPct, */
			  VAR_COMPRESSION(Z), &cType, cParms, &cPct,
		    NULL_);
   if (!StatusHandlerCvt("SRC",status)) return FALSE;
   if (cType != NO_COMPRESSION) {
     status = CDFlib (CONFIRM_, VAR_RESERVEPERCENT(Z), &reservePct,
		      NULL_);
     if (!StatusHandlerCvt("SRC",status)) return FALSE;
   }
   if (epochvsTT2000 == 0)
     dataTypeX = dataType;
   else {
     if (epochvsTT2000 == 1 && (dataType == CDF_EPOCH ||
                                dataType == CDF_EPOCH16))
       dataTypeX = CDF_TIME_TT2000;
     else if (epochvsTT2000 == 2 && dataType == CDF_TIME_TT2000)
       dataTypeX = CDF_EPOCH;
     else if (epochvsTT2000 == 3 && dataType == CDF_TIME_TT2000)
       dataTypeX = CDF_EPOCH16;
     else 
       dataTypeX = dataType;
   }
   if (Z) {
     status = CDFlib (SELECT_, CDF_, srcId,
		      GET_, zVAR_NUMDIMS_, &numDims,
			    zVAR_DIMSIZES_, dimSizes,
		      NULL_);
     if (!StatusHandlerCvt("SRC",status)) return FALSE;
   }
   else {
     status = CDFlib (SELECT_, CDF_, srcId,
		      GET_, rVARs_NUMDIMS_, &numDims,
			    rVARs_DIMSIZES_, dimSizes,
		      NULL_);
     if (!StatusHandlerCvt("SRC",status)) return FALSE;
   }
   /***************************************************************************
   * Creating new variable in destination CDF.
   ***************************************************************************/
   if (mLog) {
     WriteOut (stdout, "    Converting ");
     WriteOut (stdout, BOO(Z,"zV","rV"));
     WriteOut (stdout, "ariable \"");
     WriteOut (stdout, varName);
     WriteOut (stdout, "\"\n");
   }
   if (Z) {
     status = CDFlib (SELECT_, CDF_, dstId,
		      CREATE_, zVAR_, varName, dataTypeX, numElements, numDims,
				      dimSizes, recVary, dimVarys, &ignoredNum,
		      NULL_);
     if (!StatusHandlerCvt("DST",status)) return FALSE;
   }
   else {
     status = CDFlib (SELECT_, CDF_, dstId,
		      CREATE_, rVAR_, varName, dataTypeX, numElements, recVary,
				      dimVarys, &ignoredNum,
		      NULL_);
     if (!StatusHandlerCvt("DST",status)) return FALSE;
   }
   /***************************************************************************
   * Specify pad value for new variable (if it exists for old variable).
   ***************************************************************************/
   NvalueBytes = numElements * CDFelemSize(dataType);
   padValue = cdf_AllocateMemory ((size_t) NvalueBytes, FatalError);
   status = CDFlib (SELECT_, CDF_, srcId,
		    GET_, VAR_PADVALUE(Z), padValue,
		    NULL_);
   if (status != NO_PADVALUE_SPECIFIED) {
     if (!StatusHandlerCvt("SRC",status)) return FALSE;
     if (epochvsTT2000 == 0 || (dataType != CDF_EPOCH &&
                                dataType != CDF_EPOCH16 &&
                                dataType != CDF_TIME_TT2000)) {
       status = CDFlib (SELECT_, CDF_, dstId,
		        PUT_, VAR_PADVALUE(Z), padValue,
		        NULL_);
       if (!StatusHandlerCvt("DST",status)) return FALSE;
     } else {
       if (epochvsTT2000 == 1 && (dataType == CDF_EPOCH ||
                                  dataType == CDF_EPOCH16)) {
         long long *epoch = cdf_AllocateMemory ((size_t)numElements * 8,
	  				        FatalError);
         if (epoch == NULL) return FALSE; 
         ConvertEpoch (dataType, (int) numElements, 1L,
                       (double *)padValue, epoch);
         status = CDFlib (SELECT_, CDF_, dstId,
                          PUT_, VAR_PADVALUE(Z), epoch,
                          NULL_);
         cdf_FreeMemory (epoch, FatalError);
         if (!StatusHandlerCvt("DST",status)) return FALSE;
       } else if ((epochvsTT2000 == 2 || epochvsTT2000 == 3) &&
                  dataType == CDF_TIME_TT2000) {
         double *epoch;
         int step = (epochvsTT2000 == 2 ? 1 : 2);
         epoch = cdf_AllocateMemory ((size_t)numElements * 8 * step,
                                     FatalError);
         if (epoch == NULL) return FALSE;
         ConvertEpoch (dataType, (int) numElements, epochvsTT2000,
                       epoch, (long long *)padValue);
         status = CDFlib (SELECT_, CDF_, dstId,
                          PUT_, VAR_PADVALUE(Z), epoch,
                          NULL_);
         cdf_FreeMemory (epoch, FatalError);
         if (!StatusHandlerCvt("DST",status)) return FALSE;
       } else {
         status = CDFlib (SELECT_, CDF_, dstId,
                          PUT_, VAR_PADVALUE(Z), padValue,
                          NULL_);
         if (!StatusHandlerCvt("DST",status)) return FALSE;
       }
     }
   }
   cdf_FreeMemory (padValue, FatalError);
   /***************************************************************************
   * Determine sparseness/compression/blocking factor.
   ***************************************************************************/
   if (sparseRecords != NULL) {
     struct sRecordsVarStruct *Var;
     if (sparseRecords->VARs.sRecordsType != SOURCEsparseRECORDS) {
       sRecordsType = sparseRecords->VARs.sRecordsType;
     }
     for (Var = sparseRecords->VARhead; Var != NULL; Var = Var->next) {
	if (!strcmpITB(Var->name,varName)) {
	  sRecordsType = Var->sRecordsType;
	  break;
	}
     }
   }
   if (compression != NULL) {
     struct cVarStruct *Var; int i;
     if (compression->VARs.cType != SOURCEcompression) {
       cType = compression->VARs.cType;
       for (i = 0; i < CDF_MAX_PARMS; i++) {
	  cParms[i] = compression->VARs.cParms[i];
       }
       blockingFactor = compression->VARs.bf;
       reservePct = compression->VARs.reserve;
     }
     for (Var = compression->VARhead; Var != NULL; Var = Var->next) {
	if (!strcmpITB(Var->name,varName)) {
	  cType = Var->cType;
	  for (i = 0; i < CDF_MAX_PARMS; i++) cParms[i] = Var->cParms[i];
	  blockingFactor = Var->bf;
	  reservePct = Var->reserve;
	  break;
	}
     }
   }
   if (maxBF == 1) {
       if (once == 0) {
         long maxRec;
         status = CDFlib (SELECT_, CDF_, srcId,
                          GET_, VARs_MAXREC(Z), &maxRec,
                          NULL_);
         if (!StatusHandlerCvt("DST",status)) return FALSE;
         once = 1;
         blockingFactorA = maxRec + 1;
       }
   } else if (maxBF == 2) {
       if (recVary) {
	 long recSize = 1;
	 int jx;
         for (jx = 0; jx < (int)numDims; jx++)
	   if (dimVarys[jx]) recSize *= dimSizes[jx];
	 recSize *= CDFelemSize(dataTypeX);
	 recSize *= numElements;
         blockingFactorA = (long) (8192 * NUMcacheSTAGE) / recSize;
	 if (blockingFactorA > (maxRec+1)) blockingFactorA = maxRec + 1;
         if (blockingFactorA < 1) blockingFactorA = 1;
       }
   }
   if (blockingFactorA != -1) blockingFactor = blockingFactorA;
/*   if (!recVary) blockingFactor = MINIMUM(blockingFactor,1); */
   /***************************************************************************
   * Put compression/sparseness/blocking factor.
   ***************************************************************************/
   status = CDFlib (SELECT_, CDF_, dstId,
		    PUT_, VAR_BLOCKINGFACTOR(Z), (recVary?blockingFactor:1),
			  VAR_SPARSERECORDS(Z), sRecordsType,
/*			  VAR_SPARSEARRAYS(Z), sArraysType, sArraysParms, */
/*			  VAR_COMPRESSION(Z), cType, cParms, */
		    NULL_);
   if (!StatusHandlerCvt("DST",status)) return FALSE;
   if (!recVary) {
     long values = 1, size;
     for (dimN = 0; dimN < numDims; dimN++) values *= dimSizes[dimN];
     status = CDFgetDataTypeSize (dataType, &size);
     if ((size*values) >= 1024) { /* Allow compression if NRV record >= 1K */
       if (cType != NO_COMPRESSION && compressNonEpoch == 0)
         status = CDFlib (PUT_, VAR_COMPRESSION(Z), cType, cParms,
			  SELECT_, VAR_RESERVEPERCENT(Z), reservePct,
		          NULL_);
       else if (compressNonEpoch == 1 && !CDFepochDataType(dataTypeX)) {
           if (cType == NO_COMPRESSION) {
	     cParms[0] = 6L;
	     cType = GZIP_COMPRESSION;
           }
           status = CDFlib (PUT_, VAR_COMPRESSION(Z), cType, cParms,
		            NULL_);
       } else
	 cType = NO_COMPRESSION;
     } else
       cType = NO_COMPRESSION;
   } else {
     if (cType != NO_COMPRESSION && compressNonEpoch == 0)
       status = CDFlib (PUT_, VAR_COMPRESSION(Z), cType, cParms,
			SELECT_, VAR_RESERVEPERCENT(Z), reservePct,
		        NULL_);
     else if (compressNonEpoch == 1 && !CDFepochDataType(dataTypeX)) {
       if (cType == NO_COMPRESSION) {
         cParms[0] = 6L;
         cType = GZIP_COMPRESSION;
       }
       status = CDFlib (PUT_, VAR_COMPRESSION(Z), cType, cParms,
		        NULL_);
     } else
       cType = NO_COMPRESSION;
   }
   if (!StatusHandlerCvt("DST",status)) return FALSE;
   CHECKforABORTso
   /***************************************************************************
   * Return (success) if no records exist.
   ***************************************************************************/
   if (maxRec == NO_RECORD) return TRUE;
   /***************************************************************************
   * Read/write values using hyper groups...
   ***************************************************************************/
   for (dimN = 0, nValuesPerRecord = 1; dimN < numDims; dimN++) {
      if (dimVarys[dimN])
	nValuesPerRecord *= dimSizes[dimN];
      else
	dimSizes[dimN] = 1;
   }
   handles[0] = &buffer1;
   handles[1] = &buffer2;
   nValueBytes[0] = (size_t) NvalueBytes;
   nValueBytes[1] = (size_t) NvalueBytes;
   if (mLog) {
     WriteOut (stdout, cvtMsg);
     if (pctLog)
       pctOn = TRUE;
     else
       WriteOut (stdout, "\n");
   }
   switchMajority = (numDims > 1 && SRCmajority != DSTmajority);
   for (recNum = 0; recNum <= maxRec; recNum = toRec + 1) {
      /************************************************************************
      * Determine the next allocated record.
      ************************************************************************/
      status = CDFlib (SELECT_, CDF_, srcId,
		       GET_, VAR_ALLOCATEDFROM(Z), recNum, &fromRec,
		       NULL_);
      if (!StatusHandlerCvt("SRC",status)) return FALSE;
      /************************************************************************
      * Determine the last allocated record (before the next unallocated one).
      * Do not let this exceed the maximum record written to the variable.
      ************************************************************************/
      status = CDFlib (SELECT_, CDF_, srcId,
		       GET_, VAR_ALLOCATEDTO(Z), fromRec, &toRec,
		       NULL_);
      if (!StatusHandlerCvt("SRC",status)) return FALSE;
      toRec = MINIMUM(toRec,maxRec);
      /************************************************************************
      * Allocate the records unless the variable is compressed or has sparse
      * arrays.
      ************************************************************************/
/*    if (cType == NO_COMPRESSION && sArraysType == NO_SPARSEARRAYS) { */
      if (cType == NO_COMPRESSION) {
	status = CDFlib (SELECT_, CDF_, dstId,
			 PUT_, VAR_ALLOCATEBLOCK(Z), fromRec, toRec,
			 NULL_);
	if (!StatusHandlerCvt("DST",status)) return FALSE;
      }
      /************************************************************************
      * Calculate the number of records in this group.
      ************************************************************************/
      nRecords = toRec - fromRec + 1;
      /************************************************************************
      * If the majority is being switched...
      ************************************************************************/
      if (switchMajority) {
	AllocateBuffers (nRecords, numDims, dimSizes, &groups, 0, 2, handles,
			 nValueBytes, srcRowMajor, MINnHYPERS, FatalError);
	if (HyperFullRecord(&groups,numDims)) {
	  long nBytesPerRecord = nValuesPerRecord * NvalueBytes, recX;
	  InitHyperParms (&hyper, &groups, numDims, &nHypers, &nValues);
	  hyper.recNumber = fromRec;
	  for (hyperN = 0; hyperN < nHypers; hyperN++) {
	     status = HYPER_READ (srcId, Z, hyper, buffer1);
	     if (!StatusHandlerCvt("SRC",status)) return FALSE;
	     if (pctLog) {
	       if (!pctOn) WriteOut (stdout, cvtMsg);
	       WriteOutPct (PCTx(PCT(hyperN,nHypers,1,3),toRec,maxRec));
	       pctOn = TRUE;
	     }
	     CHECKforABORTso
	     for (recX = 0; recX < hyper.recCount; recX++) {
	        size_t offset = (size_t) (recX * nBytesPerRecord);
	        if (srcRowMajor)
	          ROWtoCOL (buffer1 + offset, buffer2 + offset, numDims,
		            dimSizes, NvalueBytes);
	        else
	          COLtoROW (buffer1 + offset, buffer2 + offset, numDims,
		            dimSizes, NvalueBytes);
	     }
	     if (pctLog) {
	       if (!pctOn) WriteOut (stdout, cvtMsg);
	       WriteOutPct (PCTx(PCT(hyperN,nHypers,2,3),toRec,maxRec));
	       pctOn = TRUE;
	     }
	     CHECKforABORTso
             if (epochvsTT2000 == 0 || (dataType != CDF_EPOCH &&
                                        dataType != CDF_EPOCH16 &&
                                        dataType != CDF_TIME_TT2000)) {
               if (dataType == CDF_TIME_TT2000 && adjustLS > -1)
                 AdjustTT2000Records (hyper.recCount, (char *) buffer2,
                                      NvalueBytes * nValuesPerRecord, adjustLS);
               if (sortMax > 1 && sortMax == (maxRec+1) && recVary &&
                   sRecordsType == NO_SPARSERECORDS) {
                 status = ShuffleRecords (dstId, Z, hyper, (char *) buffer2,
                                          NvalueBytes * nValuesPerRecord); 
               } else 
	         status = HYPER_WRITE (dstId, Z, hyper, buffer2);
	       if (!StatusHandlerCvt("DST",status)) return FALSE;
             } else {
               if (epochvsTT2000 == 1 && (dataType == CDF_EPOCH ||
                                          dataType == CDF_EPOCH16)) {
                 long long *bufferx;
  	         int ix, values = 1;
	         for (ix = 0; ix < numDims; ++ix) values *= (int) dimSizes[ix];
                 values *= numElements;
	         bufferx = (long long *) cdf_AllocateMemory ((size_t) hyper.recCount *
							   values * 8, FatalError);
	         if (bufferx != NULL) {
                   ConvertEpoch (dataType, hyper.recCount * values, 1L,
		                 (double *) buffer2, bufferx);
                   if (sortMax > 1 && sortMax == (maxRec+1) && recVary &&
                       sRecordsType == NO_SPARSERECORDS)
                     status = ShuffleRecords (dstId, Z, hyper, (char *) bufferx,
                                              values * 8);
                   else
                     status = HYPER_WRITE (dstId, Z, hyper, bufferx);
                   if (!StatusHandlerCvt("DST",status)) return FALSE;
	         } else { /* One at a time */
		   long count;
		   int step = (dataType == CDF_EPOCH ? 1 : 2);
		   bufferx = (long long *) cdf_AllocateMemory ((size_t)values*8,
                                                               FatalError);
		   if (bufferx == NULL) return FALSE;
		   count = hyper.recCount;
		   hyper.recCount = 1;
		   for (ix = 0; ix < (int) count; ++ix) {
		     hyper.recNumber += ix;
                     ConvertEpoch (dataType, values, 1L,
		  		   ((double *)buffer2)+step*ix*values, bufferx);
                     if (sortMax > 1 && sortMax == (maxRec+1) && recVary &&
                         sRecordsType == NO_SPARSERECORDS)
                       status = ShuffleRecords (dstId, Z, hyper, (char *) bufferx,
                                                values * 8);
                     else
                       status = HYPER_WRITE (dstId, Z, hyper, bufferx);
                     if (!StatusHandlerCvt("DST",status)) return FALSE;
		   }
                 }
                 cdf_FreeMemory (bufferx, NULL);
               } else if ((epochvsTT2000 == 2 || epochvsTT2000 == 3) &&
                          dataType == CDF_TIME_TT2000) {
                 double *bufferx;
                 int ix, values = 1;
                 int step = (epochvsTT2000 == 2 ? 1 : 2);
                 if (dataType == CDF_TIME_TT2000 && adjustLS > -1)
                   AdjustTT2000Records (hyper.recCount, (char *) buffer2,
                                        NvalueBytes * nValuesPerRecord, adjustLS);
                 for (ix = 0; ix < numDims; ++ix) values *= (int) dimSizes[ix];
                 values *= numElements;
                 bufferx = (double *) cdf_AllocateMemory ((size_t) hyper.recCount *
                                                           values * 8 * step,
                                                           FatalError);
                 if (bufferx != NULL) {
                   ConvertEpoch (dataType, hyper.recCount * values,
                                 epochvsTT2000, bufferx, (long long *) buffer2);
                   if (sortMax > 1 && sortMax == (maxRec+1) && recVary &&
                       sRecordsType == NO_SPARSERECORDS)
                     status = ShuffleRecords (dstId, Z, hyper, (char *) bufferx,
                                              values * 8 * step);
                   else
                     status = HYPER_WRITE (dstId, Z, hyper, bufferx);
                   cdf_FreeMemory (bufferx, NULL);
                   if (!StatusHandlerCvt("DST",status)) return FALSE;
                 } else {                   long count;
                   bufferx = (double *) cdf_AllocateMemory ((size_t)values*8*step,
                                                            FatalError);
                   if (bufferx == NULL) return FALSE;
                   count = hyper.recCount;
                   hyper.recCount = 1;
                   for (ix = 0; ix < (int) count; ++ix) {
                     hyper.recNumber += ix;
                     ConvertEpoch (dataType, values, epochvsTT2000,
                                   bufferx+step*ix*values,
                                   (long long *) buffer2);
                     if (sortMax > 1 && sortMax == (maxRec+1) && recVary &&
                         sRecordsType == NO_SPARSERECORDS)
                       status = ShuffleRecords (dstId, Z, hyper, (char *) bufferx,
                                                values * 8 * step);
                     else
                       status = HYPER_WRITE (dstId, Z, hyper, bufferx);
                     if (!StatusHandlerCvt("DST",status)) return FALSE;
                   }
                 }
                 cdf_FreeMemory (bufferx, NULL);
               } else {
                 if (sortMax > 1 && sortMax == (maxRec+1) && recVary &&
                     sRecordsType == NO_SPARSERECORDS)
                   status = ShuffleRecords (dstId, Z, hyper, (char *) buffer2,
                                            NvalueBytes * nValuesPerRecord);
                 else
                   status = HYPER_WRITE (dstId, Z, hyper, buffer2);
                 if (!StatusHandlerCvt("DST",status)) return FALSE;
               }
             }
	     if (pctLog) {
	       if (!pctOn) WriteOut (stdout, cvtMsg);
	       WriteOutPct (PCTx(PCT(hyperN,nHypers,3,3),toRec,maxRec));
	       pctOn = TRUE;
	     }
	     CHECKforABORTso
	     IncrHyperParms (&hyper, &groups, numDims, srcRowMajor, &nValues);
          }
          cdf_FreeMemory (buffer1, FatalError);
          cdf_FreeMemory (buffer2, FatalError);
        }
	else {
          cdf_FreeMemory (buffer2, FatalError);
          InitHyperParms (&hyper, &groups, numDims, &nHypers, &nValues);
	  hyper.recNumber = fromRec;
          for (hyperN = 0; hyperN < nHypers; hyperN++) {
             long indices[CDF_MAX_DIMS]; Byte1 *value = buffer1; long valueN;
             status = HYPER_READ (srcId, Z, hyper, buffer1);
             if (!StatusHandlerCvt("SRC",status)) return FALSE;
             CHECKforABORTso
             if (pctLog) {
	       if (!pctOn) WriteOut (stdout, cvtMsg);
	       WriteOutPct (PCTx(PCT(hyperN,nHypers,1,2),toRec,maxRec));
	       pctOn = TRUE;
             }
             status = CDFlib (SELECT_,CDF_,dstId,
				      BOO(Z,zVAR_RECNUMBER_,
				            rVARs_RECNUMBER_),hyper.recNumber,
		              NULL_);
             if (!StatusHandlerCvt("DST",status)) return FALSE;
             for (dimN = 0; dimN < numDims; dimN++) {
	        indices[dimN] = hyper.dimIndices[dimN];
             }
             for (valueN = 0; valueN < nValues; valueN++) {
                if (epochvsTT2000 == 0 || (dataType != CDF_EPOCH &&
                                           dataType != CDF_EPOCH16 &&
                                           dataType != CDF_TIME_TT2000)) {
                  if (dataType == CDF_TIME_TT2000 && adjustLS > -1)
                    AdjustTT2000Records (1, (char *) value, 8, adjustLS);
	          status = CDFlib (SELECT_, BOO(Z,zVAR_DIMINDICES_,
		  			          rVARs_DIMINDICES_), indices,
			           PUT_, VAR_DATA(Z), value,
			           NULL_);
                } else {
                  if (epochvsTT2000 == 1 && (dataType == CDF_EPOCH ||
                                             dataType == CDF_EPOCH16)) {
		    long long epoch;
                    ConvertEpoch (dataType, 1, 1L, (double *)value, &epoch);
                    status = CDFlib (SELECT_, BOO(Z,zVAR_DIMINDICES_,
                                                    rVARs_DIMINDICES_), indices,
                                     PUT_, VAR_DATA(Z), &epoch,
                                     NULL_);
		  } else if ((epochvsTT2000 == 2 || epochvsTT2000 == 3) &&
                             dataType == CDF_TIME_TT2000) {
                    double epoch[2];
                    ConvertEpoch (dataType, 1, epochvsTT2000,
                                  epoch, (long long *)value);
                    status = CDFlib (SELECT_, BOO(Z,zVAR_DIMINDICES_,
                                                    rVARs_DIMINDICES_), indices,
                                     PUT_, VAR_DATA(Z), epoch,
                                     NULL_);
                  } else {
                    status = CDFlib (SELECT_, BOO(Z,zVAR_DIMINDICES_,
                                                    rVARs_DIMINDICES_), indices,
                                     PUT_, VAR_DATA(Z), value,
                                     NULL_);
                  }
                }
                if (!StatusHandlerCvt("DST",status)) return FALSE;
	        if (srcRowMajor)
	          INCRindicesROW (numDims, dimSizes, indices);
	        else
	          INCRindicesCOL (numDims, dimSizes, indices);
	        value += (size_t) NvalueBytes;
             }
             if (pctLog) {
	       if (!pctOn) WriteOut (stdout, cvtMsg);
	       WriteOutPct (PCTx(PCT(hyperN,nHypers,2,2),toRec,maxRec));
	       pctOn = TRUE;
             }
             IncrHyperParms (&hyper, &groups, numDims, srcRowMajor, &nValues);
             CHECKforABORTso
          }
          cdf_FreeMemory (buffer1, FatalError);
	}
      }
      else {
	AllocateBuffers (nRecords, numDims, dimSizes, &groups, 0, 1, handles,
			 nValueBytes, srcRowMajor, MINnHYPERS, FatalError);
	InitHyperParms (&hyper, &groups, numDims, &nHypers, &nValues);
	hyper.recNumber = fromRec;
	for (hyperN = 0; hyperN < nHypers; hyperN++) {
	   status = HYPER_READ (srcId, Z, hyper, buffer1);
	   if (!StatusHandlerCvt("SRC",status)) return FALSE;
	   if (pctLog) {
	     if (!pctOn) WriteOut (stdout, cvtMsg);
	     WriteOutPct (PCTx(PCT(hyperN,nHypers,1,2),toRec,maxRec));
	     pctOn = TRUE;
	   }
	   CHECKforABORTso
	   if (epochvsTT2000 == 0 || (dataType != CDF_EPOCH &&
			              dataType != CDF_EPOCH16 &&
			              dataType != CDF_TIME_TT2000)) {
             if (dataType == CDF_TIME_TT2000 && adjustLS > -1)
               AdjustTT2000Records (hyper.recCount, (char *) buffer1,
                                    NvalueBytes * nValuesPerRecord, adjustLS);
             if (sortMax > 1 && sortMax == (maxRec+1) && recVary &&
                 sRecordsType == NO_SPARSERECORDS)
               status = ShuffleRecords (dstId, Z, hyper, (char *) buffer1,
                                        NvalueBytes * nValuesPerRecord);
             else
	       status = HYPER_WRITE (dstId, Z, hyper, buffer1);
	     if (!StatusHandlerCvt("DST",status)) return FALSE;
	   } else {
             if (epochvsTT2000 == 1 && (dataType == CDF_EPOCH ||
                                        dataType == CDF_EPOCH16)) {
  	       long long *bufferx;
	       int ix, values = 1;
	       int step = (dataType == CDF_EPOCH ? 1 : 2);
	       for (ix = 0; ix < numDims; ++ix) values *= dimSizes[ix];
               values *= numElements;
	       bufferx = (long long *) cdf_AllocateMemory ((size_t)hyper.recCount *
							   values * 8,
                                                           FatalError);
	       if (bufferx != NULL) {
	         ConvertEpoch (dataType, hyper.recCount * values,
		  	       1L, (double *)buffer1, bufferx);
                 if (sortMax > 1 && sortMax == (maxRec+1) && recVary &&
                     sRecordsType == NO_SPARSERECORDS)
                   status = ShuffleRecords (dstId, Z, hyper, (char *) bufferx,
                                            values * 8);
                 else
                   status = HYPER_WRITE (dstId, Z, hyper, bufferx);
                 if (!StatusHandlerCvt("DST",status)) return FALSE;
	       } else {
	         long count = hyper.recCount;
	         bufferx = (long long *) cdf_AllocateMemory ((size_t)values * 8,
                                                             FatalError);
	         if (bufferx == NULL) return FALSE;
	         hyper.recCount = 1;
	         for (ix = 0; ix < (int) count; ++ix) {
                   ConvertEpoch (dataType, values, 1L,
		  	         ((double *) buffer1)+step*values*ix, bufferx);
		   hyper.recNumber += ix;
                   if (sortMax > 1 && sortMax == (maxRec+1) && recVary &&
                       sRecordsType == NO_SPARSERECORDS)
                     status = ShuffleRecords (dstId, Z, hyper, (char *) bufferx,
                                              values * 8);
                   else
                     status = HYPER_WRITE (dstId, Z, hyper, bufferx);
                   if (!StatusHandlerCvt("DST",status)) return FALSE;
	         }
               }
               cdf_FreeMemory (bufferx, FatalError);
	     } else if ((epochvsTT2000 == 2 || epochvsTT2000 == 3) &&
                        dataType == CDF_TIME_TT2000) {
               double *bufferx;
               int ix, values = 1;
               int step = (epochvsTT2000 == 2 ? 1 : 2);
               if (adjustLS > -1)
                 AdjustTT2000Records (hyper.recCount, (char *) buffer1,
                                      NvalueBytes * nValuesPerRecord, adjustLS);
               for (ix = 0; ix < numDims; ++ix) values *= dimSizes[ix];
               bufferx = (double *) cdf_AllocateMemory ((size_t)hyper.recCount *
                                                         values * 8 * step,
                                                         FatalError);
               if (bufferx != NULL) {
                 ConvertEpoch (dataType, hyper.recCount * values,
                               epochvsTT2000, bufferx, (long long *)buffer1);
                 if (sortMax > 1 && sortMax == (maxRec+1) && recVary &&
                     sRecordsType == NO_SPARSERECORDS)
                   status = ShuffleRecords (dstId, Z, hyper, (char *) bufferx,
                                            values * 8 * step);
                 else
                   status = HYPER_WRITE (dstId, Z, hyper, bufferx);
                 cdf_FreeMemory (bufferx, FatalError);
                 if (!StatusHandlerCvt("DST",status)) return FALSE;
               } else {
                 long count = hyper.recCount;
                 bufferx = (double *) cdf_AllocateMemory ((size_t)values*8*step,
                                                          FatalError);
                 if (bufferx == NULL) return FALSE;
                 hyper.recCount = 1;
                 for (ix = 0; ix < (int) count; ++ix) {
                   ConvertEpoch (dataType, values, epochvsTT2000,
                                 bufferx, (long long *) buffer1+values*ix);
                   hyper.recNumber += ix;
                   if (sortMax > 1 && sortMax == (maxRec+1) && recVary &&
                       sRecordsType == NO_SPARSERECORDS)
                     status = ShuffleRecords (dstId, Z, hyper, (char *) bufferx,
                                              values * 8 * step);
                   else
                     status = HYPER_WRITE (dstId, Z, hyper, bufferx);
                   if (!StatusHandlerCvt("DST",status)) return FALSE;
                 }
                 cdf_FreeMemory (bufferx, FatalError);
               }
             } else {
               if (sortMax > 1 && sortMax == (maxRec+1) && recVary &&
                   sRecordsType == NO_SPARSERECORDS)
                 status = ShuffleRecords (dstId, Z, hyper, (char *) buffer1,
                                          NvalueBytes * nValuesPerRecord);
               else
                 status = HYPER_WRITE (dstId, Z, hyper, buffer1);
               if (!StatusHandlerCvt("DST",status)) return FALSE;
             }
	   }
	   if (pctLog) {
	     if (!pctOn) WriteOut (stdout, cvtMsg);
	     WriteOutPct (PCTx(PCT(hyperN,nHypers,2,2),toRec,maxRec));
	     pctOn = TRUE;
	   }
	   CHECKforABORTso
	   IncrHyperParms (&hyper, &groups, numDims, srcRowMajor, &nValues);
	}
	cdf_FreeMemory (buffer1, FatalError);
      }
   }
   if (pctLog) {
     WriteOut (stdout, "\n");
     pctOn = FALSE;
   }
   return TRUE;
}

/******************************************************************************
* ParseSparsenessOptions.
******************************************************************************/

#define BADs(s) FreeSparseness(s); return NULL;

struct SparseRecordsStruct *ParseSparsenessOptions (options)
char *options;
{
  struct SparseRecordsStruct *S; size_t nBytes, len; Logical done;
  char *p1, *p2; struct sRecordsVarStruct *Var;
  /****************************************************************************
  * Allocate/initialize sparseness structure.
  ****************************************************************************/
  nBytes = sizeof(struct SparseRecordsStruct);
  S = (struct SparseRecordsStruct *) cdf_AllocateMemory (nBytes, FatalError);
  S->VARs.sRecordsType = SOURCEsparseRECORDS;
  S->VARhead = NULL;
  /****************************************************************************
  * Scan options...
  * NOTE: This duplication (see `ParseCompressionOptions') could probably be
  * eliminated.
  ****************************************************************************/
  switch (*options) {
    case '(':           /* VMS-style. */
      p1 = options + 1;
      len = strlen(p1);
      if (len == 0) { BADs(S) }
      if (p1[len-1] != ')') { BADs(S) }
      p1[len-1] = NUL;
      break;
    case '"':		/* UNIX-style on a Mac (double quotes not stripped). */
      p1 = options + 1;
      len = strlen(p1);
      if (len == 0) { BADs(S) }
      if (p1[len-1] != '"') { BADs(S) }
      p1[len-1] = NUL;
      break;
    default:            /* UNIX-style on a UNIX machine or IBM PC. */
      p1 = options;
      break;
  }
  for (;;) {
     p2 = strchr (p1, ':');
     if (p2 == NULL) { BADs(S) }
     *p2 = NUL;
     if (strcmpIgCase(p1,"vars") == 1) {
       p1 = p2 + 1;
       if (!ParseSparsenessToken(&p1,&p2,&(S->VARs.sRecordsType),&done)) {
	 BADs(S)
       }
       if (done) return S;
       p1 = p2 + 1;
       continue;
     }
     if (strcmpIgCase(p1,"var") == 1) {
       nBytes = sizeof(struct sRecordsVarStruct);
       Var = (struct sRecordsVarStruct *) cdf_AllocateMemory (nBytes, FatalError);
       Var->name = NULL;
       Var->next = S->VARhead;
       S->VARhead = Var;
       p1 = p2 + 1;
       if (*p1 == NUL) { BADs(S) }
       p2 = strchr (p1 + 1, *p1);
       if (p2 == NULL) { BADs(S) }
       p1 += 1;
       *p2 = NUL;
       len = strlen (p1);
       if (len < 1) { BADs(S) }
       Var->name = (char *) cdf_AllocateMemory (len+1, FatalError);
       strcpyX (Var->name, p1, len);
       p2 += 1;
       if (*p2 != ':') { BADs(S) }
       p1 = p2 + 1;
       if (!ParseSparsenessToken(&p1,&p2,&(Var->sRecordsType),&done)) {
	 BADs(S)
       }
       if (done) return S;
       p1 = p2 + 1;
       continue;
     }
     BADs(S)
  }
}

/******************************************************************************
* ParseSparsenessToken.
******************************************************************************/

Logical ParseSparsenessToken (p1, p2, sRecordsType, done)
char **p1;
char **p2;
long *sRecordsType;
Logical *done;
{
  char *sToken;
  if (**p1 == NUL) return FALSE;
  *p2 = strchr (*p1, ',');
  if (*p2 == NULL) {
    *done = TRUE;
  }
  else {
    *done = FALSE;
    **p2 = NUL;
  }
  sToken = *p1;
  if (strcmpIgCase(sToken,"sRecords.PAD") == 1) {
    *sRecordsType = PAD_SPARSERECORDS;
    return TRUE;
  }
  if (strcmpIgCase(sToken,"sRecords.PREV") == 1) {
    *sRecordsType = PREV_SPARSERECORDS;
    return TRUE;
  }
  if (strcmpIgCase(sToken,"sRecords.NO") == 1) {
    *sRecordsType = NO_SPARSERECORDS;
    return TRUE;
  }
  return FALSE;
}

/******************************************************************************
* ParseCompressionOptions.
******************************************************************************/

#define BADc(c) FreeCompression(c); return NULL;

struct CompressionStruct *ParseCompressionOptions (options)
char *options;
{
  struct CompressionStruct *C; size_t nBytes, len;
  Logical done; char *p1, *p2; struct cVarStruct *Var;
  /****************************************************************************
  * Allocate/initialize compression structure.
  ****************************************************************************/
  nBytes = sizeof(struct CompressionStruct);
  C = (struct CompressionStruct *) cdf_AllocateMemory (nBytes, FatalError);
  C->CDF.cType = SOURCEcompression;
  C->VARs.cType = SOURCEcompression;
  C->VARhead = NULL;
  /****************************************************************************
  * Scan options...
  ****************************************************************************/
  switch (*options) {
    case '(':           /* VMS-style. */
      p1 = options + 1;
      len = strlen(p1);
      if (len == 0) { BADc(C) }
      if (p1[len-1] != ')') { BADc(C) }
      p1[len-1] = NUL;
      break;
    case '"':		/* UNIX-style on a Mac (double quotes not stripped). */
      p1 = options + 1;
      len = strlen(p1);
      if (len == 0) { BADc(C) }
      if (p1[len-1] != '"') { BADc(C) }
      p1[len-1] = NUL;
      break;
    default:            /* UNIX-style on a UNIX machine or IBM PC. */
      p1 = options;
      break;
  }
  for (;;) {
     p2 = strchr (p1, ':');
     if (p2 == NULL) { BADc(C) }
     *p2 = NUL;
     if (strcmpIgCase(p1,"cdf") == 1) {
       p1 = p2 + 1;
       if (!ParseCompressionTokenAndBF(&p1,&p2,
				       &(C->CDF.cType),
				       C->CDF.cParms,
				       NULL,NULL,&done)) { BADc(C) }
       if (done) return C;
       p1 = p2 + 1;
       continue;
     }
     if (strcmpIgCase(p1,"vars") == 1) {
       p1 = p2 + 1;
       if (!ParseCompressionTokenAndBF(&p1,&p2,
				       &(C->VARs.cType),
				       C->VARs.cParms,
				       &(C->VARs.bf),
				       &(C->VARs.reserve),
				       &done)) { BADc(C) }
       if (done) return C;
       p1 = p2 + 1;
       continue;
     }
     if (strcmpIgCase(p1,"var") == 1) {
       nBytes = sizeof(struct cVarStruct);
       Var = (struct cVarStruct *) cdf_AllocateMemory (nBytes, FatalError);
       Var->name = NULL;
       Var->next = C->VARhead;
       C->VARhead = Var;
       p1 = p2 + 1;
       if (*p1 == NUL) { BADc(C) }
       p2 = strchr (p1 + 1, *p1);
       if (p2 == NULL) { BADc(C) }
       p1 += 1;
       *p2 = NUL;
       len = strlen (p1);
       if (len < 1) { BADc(C) }
       Var->name = (char *) cdf_AllocateMemory ((size_t)len+1, FatalError);
       strcpyX (Var->name, p1, len);
       p2 += 1;
       if (*p2 != ':') { BADc(C) }
       p1 = p2 + 1;
       if (!ParseCompressionTokenAndBF(&p1,&p2,
				       &(Var->cType),
				       Var->cParms,
				       &(Var->bf),
				       &(Var->reserve),
				       &done)) { BADc(C) }
       if (done) return C;
       p1 = p2 + 1;
       continue;
     }
     BADc(C)
  }
}

/******************************************************************************
* ParseCompressionTokenAndBF.
******************************************************************************/

Logical ParseCompressionTokenAndBF (p1, p2, cType, cParms, bf, reserve, done)
char **p1;
char **p2;
long *cType;
long cParms[CDF_MAX_PARMS];
long *bf;
long *reserve;
Logical *done;
{
  int count, level; long v1, v2; char *cToken, *p;
  if (**p1 == NUL) return FALSE;
  *p2 = strchr (*p1, ',');
  if (*p2 == NULL) {
    *done = TRUE;
  }
  else {
    *done = FALSE;
    **p2 = NUL;
  }
  cToken = *p1;
  p = strchr (*p1, ':');
  if (p == NULL)
    count = 0;
  else {
    *p = NUL;
    count = sscanf (p+1, "%ld:%ld", &v1, &v2);
    if (count < 1) return FALSE;
  }
  if (strcmpIgCase(cToken,"none") == 1) {
/*
    if (count > 0) return FALSE;
    *cType = NO_COMPRESSION;
    ASSIGNnotNULL (bf, 0)
    ASSIGNnotNULL (reserve, 0)
*/
    *cType = NO_COMPRESSION;
    ASSIGNnotNULL (bf, BOO(count > 0,v1,0))
    ASSIGNnotNULL (reserve, 0)
    return TRUE;
  }
  if (strcmpIgCase(cToken,"rle.0") == 1) {
    *cType = RLE_COMPRESSION;
    cParms[0] = RLE_OF_ZEROs;
    ASSIGNnotNULL (bf, BOO(count > 0,v1,0))
    ASSIGNnotNULL (reserve, BOO(count > 1,v2,0))
    return TRUE;
  }
  if (strcmpIgCase(cToken,"huff.0") == 1) {
    *cType = HUFF_COMPRESSION;
    cParms[0] = OPTIMAL_ENCODING_TREES;
    ASSIGNnotNULL (bf, BOO(count > 0,v1,0))
    ASSIGNnotNULL (reserve, BOO(count > 1,v2,0))
    return TRUE;
  }
  if (strcmpIgCase(cToken,"ahuff.0") == 1) {
    *cType = AHUFF_COMPRESSION;
    cParms[0] = OPTIMAL_ENCODING_TREES;
    ASSIGNnotNULL (bf, BOO(count > 0,v1,0))
    ASSIGNnotNULL (reserve, BOO(count > 1,v2,0))
    return TRUE;
  }
  for (level = 1; level <= 9; level++) {
     char gzipToken[MAX_GZIP_TOKEN_LEN+1];
     snprintf (gzipToken, (size_t) sizeof(gzipToken), "gzip.%d", level);
     if (strcmpIgCase(cToken,gzipToken) == 1) {
       *cType = GZIP_COMPRESSION;
       cParms[0] = level;
       ASSIGNnotNULL (bf, BOO(count > 0,v1,0))
       ASSIGNnotNULL (reserve, BOO(count > 1,v2,0))
       return TRUE;
     }
  }
  return FALSE;
}

/******************************************************************************
* FreeSparseness.
******************************************************************************/

void FreeSparseness (sparseRecords)
struct SparseRecordsStruct *sparseRecords;
{
  struct sRecordsVarStruct *Var = sparseRecords->VARhead;
  while (Var != NULL) {
    struct sRecordsVarStruct *VARnext = Var->next;
    if (Var->name != NULL) cdf_FreeMemory (Var->name, FatalError);
    cdf_FreeMemory (Var, FatalError);
    Var = VARnext;
  }
  cdf_FreeMemory (sparseRecords, FatalError);
}

/******************************************************************************
* FreeCompression.
******************************************************************************/

void FreeCompression (compression)
struct CompressionStruct *compression;
{
  struct cVarStruct *Var = compression->VARhead;
  while (Var != NULL) {
    struct cVarStruct *VARnext = Var->next;
    if (Var->name != NULL) cdf_FreeMemory (Var->name, FatalError);
    cdf_FreeMemory (Var, FatalError);
    Var = VARnext;
  }
  cdf_FreeMemory (compression, FatalError);
}

/******************************************************************************
* ConvertEpoch.
******************************************************************************/

void ConvertEpoch (dataType, numValues, conversionType, value1, value2)
long dataType;
int numValues;
long conversionType;
double *value1;
long long *value2;
{
  int ix;
  CDFstatus status = CDF_OK;
  if (conversionType == 1) { /* CDF_EPOCH/CDF_EPOCH16 to TT2000 */
    if (dataType == CDF_EPOCH) {
      for (ix = 0; ix < numValues; ++ix) {
        *(value2+ix) = CDF_TT2000_from_UTC_EPOCH (*(value1+ix));
      }
    } else {
      for (ix = 0; ix < numValues; ++ix) {
        *(value2+ix) = CDF_TT2000_from_UTC_EPOCH16 (value1+ix*2);
      }
    }
  } else if (conversionType == 2) { /* TT2000 to CDF_EPOCH */
    for (ix = 0; ix < numValues; ++ix) {
      *(value1+ix) = CDF_TT2000_to_UTC_EPOCH (*(value2+ix));
    }
  } else if (conversionType == 3) { /* TT2000 to CDF_EPOCH16 */
    double tmp;
    for (ix = 0; ix < numValues; ++ix) {
      tmp = CDF_TT2000_to_UTC_EPOCH16 (*(value2+ix), value1+2*ix);
    }
  }
}

/******************************************************************************
* StatusHandlerCvt.
******************************************************************************/

Logical StatusHandlerCvt (which, status)
char *which;
CDFstatus status;
{
  char text[CDF_STATUSTEXT_LEN+1];            /* Explanation text. */
  char oText[MAX_OUTPUT_TEXT_LEN+1];          /* Output text. */
  if (StatusERROR(status)) {
    if (report[ERRORs]) {
      if (pctOn) {
	WriteOut (stdout, "\n");	      /* Move to start of next line. */
	pctOn = FALSE;
      }
      CDFlib (SELECT_, CDF_STATUS_, status,
	      GET_, STATUS_TEXT_, text,
	      NULL_);
      snprintf (oText, (size_t) sizeof(oText), "ERROR,%s> %s", which,
	        text);
      OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 0);
    }
    return FALSE;
  }
  if (StatusWARN(status)) {
    if (report[WARNs]) {
      if (pctOn) {
	WriteOut (stdout, "\n");	      /* Move to start of next line. */
	pctOn = FALSE;
      }
      CDFlib (SELECT_, CDF_STATUS_, status,
	      GET_, STATUS_TEXT_, text,
	      NULL_);
      snprintf (oText, (size_t) sizeof(oText), "WARNING,%s> %s", which, 
	        text);
      OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 0);
    }
    return TRUE;
  }
  if (StatusINFO(status)) {
    if (report[INFOs]) {
      if (pctOn) {
	WriteOut (stdout, "\n");	      /* Move to start of next line. */
	pctOn = FALSE;
      }
      CDFlib (SELECT_, CDF_STATUS_, status,
	      GET_, STATUS_TEXT_, text,
	      NULL_);
      snprintf (oText, (size_t) sizeof(oText), "INFO,%s> %s", which, 
	        text);
      OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 0);
    }
    return TRUE;
  }
  return TRUE;          /* CDF_OK */
}

/******************************************************************************
* PrepareSort.
* Sort the variable and build the indexes for each of the original values in
* the sorted values.
******************************************************************************/

static void PrepareSort (varName)
char *varName;
{
   CDFstatus status; int i, j;
   long dataType, numElements, recVary, dimVarys[CDF_MAX_DIMS];
   long numDims, dimSizes[CDF_MAX_DIMS], indices[CDF_MAX_DIMS],
        intervals[CDF_MAX_DIMS], nValues, typeSize, maxRec;
   char *buffer1, *buffer2, *tmp;
   Logical Z; int badIdx;
   /***************************************************************************
   * Inquire existing variable in source CDF.
   ***************************************************************************/
   status = CDFlib (SELECT_, CDF_, srcId,
                             zVAR_NAME_, varName,
                    NULL_);
   if (status == CDF_OK) Z = TRUE;
   else {
     status = CDFlib (SELECT_, CDF_, srcId,
                               rVAR_NAME_, varName,
                      NULL_);
     if (status == CDF_OK) Z = FALSE;
     else return;
   } 
   status = CDFlib (GET_, VAR_DATATYPE(Z), &dataType,
                          VAR_NUMELEMS(Z), &numElements,
                          VAR_RECVARY(Z), &recVary,
                          VAR_DIMVARYS(Z), dimVarys,
                          VAR_MAXREC(Z), &maxRec,
                    NULL_);
   if (!StatusHandlerCvt("SRC",status)) return;
   if (Z)
     status = CDFlib (GET_, zVAR_NUMDIMS_, &numDims,
                            zVAR_DIMSIZES_, dimSizes,
                      NULL_);
   else
     status = CDFlib (GET_, rVARs_NUMDIMS_, &numDims,
                            rVARs_DIMSIZES_, dimSizes,
                      NULL_);
   if (!StatusHandlerCvt("SRC",status)) return;
   if (recVary != VARY || maxRec < 1) return;
   status = CDFgetDataTypeSize (dataType, &typeSize);
   nValues = typeSize; 
   for (i = 0; i < (int) numDims; ++i) {
     if (dimVarys[i]) nValues *= dimSizes[i];
     indices[i] = 0L;
     intervals[i] = 1L;
   }
   nValues *= numElements;
   nValues *= (maxRec + 1);
   buffer1 = (char *) malloc ((size_t)nValues);
   if (buffer1 == NULL) return;
   buffer2 = (char *) malloc ((size_t)nValues);
   if (buffer2 == NULL) return;
   tmp = (char *) malloc ((size_t)nValues);
   if (tmp == NULL) return;
   status = CDFlib (SELECT_, VAR_RECNUMBER(Z), 0L,
                             VAR_RECCOUNT(Z), maxRec+1L,
                             VAR_RECINTERVAL(Z), 1L,
                    NULL_);
   if (!StatusHandlerCvt("SRC",status)) return;
   if (Z)
     status = CDFlib (SELECT_, zVAR_DIMINDICES_, indices,
                               zVAR_DIMCOUNTS_, dimSizes,
                               zVAR_DIMINTERVALS_, intervals,
                      GET_, zVAR_HYPERDATA_, buffer1,
                      NULL_);
   else
     status = CDFlib (SELECT_, rVARs_DIMINDICES_, indices,
                               rVARs_DIMCOUNTS_, dimSizes,
                               rVARs_DIMINTERVALS_, intervals,
                      GET_, rVAR_HYPERDATA_, buffer1,
                      NULL_);
   if (!StatusHandlerCvt("SRC",status)) return;
   memcpy (buffer2, buffer1, (size_t) nValues);
   MergeSort (dataType, (void *) buffer2, (void *) tmp, 0, maxRec);
   sortIdx = malloc (sizeof(int) * (maxRec+1));
   fillIdx = malloc (sizeof(int) * (maxRec+1));
   badIdx = 0;
   for (i = 0; i <= (int) maxRec; ++i) {
     j = BinarySearch (dataType, (void *) buffer2, 
                       (void *) (buffer1+i*typeSize), 0, maxRec); 
     if (j < 0) { /* not found */
       free (sortIdx);
       free (fillIdx);
       badIdx = 1;
       break;
     }
     if (fillIdx[j] == 0) { /* not filled... use it */
	sortIdx[i] = j;
	fillIdx[j] = 1;
     } else { /* already filled... duplicate. Find the available neighbor. */
	j =  SearchNeighbor (dataType, (void *) buffer2,
			     (void *) (buffer1+i*typeSize), fillIdx,
			     (int)(maxRec+1), j);
	if (j < 0) { /* not found */
          free (sortIdx);
          free (fillIdx);
          badIdx = 1;
          break;
	}
	sortIdx[i] = j;
	fillIdx[j] = 1;;
     }
   }
   if (badIdx == 0) sortMax = maxRec + 1;
   free (buffer1);
   free (buffer2);
   free (tmp);
   free (fillIdx);
}

/******************************************************************************
* ShuffleRecords.
******************************************************************************/

static CDFstatus ShuffleRecords (id, zVar, hyper, buffer, recSize)
CDFid id;
Logical zVar;
struct HyperStruct hyper;
char *buffer;
long recSize;
{
   CDFstatus status; int i;

   status = CDFlib (SELECT_, CDF_, id,
                             BOO(zVar,zVAR_RECCOUNT_,
                                      rVARs_RECCOUNT_), 1L,
                             BOO(zVar,zVAR_RECINTERVAL_,
                                      rVARs_RECINTERVAL_), 1L,
                             BOO(zVar,zVAR_DIMINDICES_,
                                      rVARs_DIMINDICES_), hyper.dimIndices,
                             BOO(zVar,zVAR_DIMCOUNTS_,
                                      rVARs_DIMCOUNTS_), hyper.dimCounts,
                             BOO(zVar,zVAR_DIMINTERVALS_,
                                      rVARs_DIMINTERVALS_), hyper.dimIntervals,
                    NULL_);
   if (!StatusHandlerCvt("DST",status)) return status;
   for (i = 0; i < (int) hyper.recCount; ++i) {
      status = CDFlib (SELECT_, BOO(zVar,zVAR_RECNUMBER_,
                                         rVARs_RECNUMBER_), 
                                  (long) sortIdx[(int)hyper.recNumber+i],
                       PUT_, BOO(zVar,zVAR_HYPERDATA_,rVAR_HYPERDATA_),
                                 buffer+i*recSize,
                       NULL_);
      if (!StatusHandlerCvt("DST",status)) return status;
   }
   return status;
}

/******************************************************************************
* AdjustTT2000Records.
******************************************************************************/

static void AdjustTT2000Records (count, buffer, recSize, ymd)
long count;
char *buffer;
long recSize;
int  ymd;
{
   int i, found;
   double yy, mm, dd, hh, nn, ss, ms, is, ns;
   long long tt2000, mydiff;
   int y, m, d;

   y = ymd/10000;
   m = (ymd-10000*y)/100;
   d = ymd - 10000*y - 100*m;
   tt2000 = computeTT2000((double)y, (double)m, (double)d, 0.0, 0.0, 0.0,
                          0.0, 0.0, 0.0);
   found = 0;
   for (i = (int) count-1; i >= 0; --i) {
     mydiff = *(long long*)(buffer+i*recSize) - tt2000;
     if (mydiff >= -1000000000LL) {
       *(long long*)(buffer+i*recSize) += 1000000000LL;
     } else {
       if (i > 1) {
         breakdownTT2000(*(long long*)(buffer+(i-1)*recSize), &yy, &mm, &dd,
                         &hh, &nn, &ss, &ms, &is, &ns);
         if (*(long long*)(buffer+i*recSize) <
             *(long long*)(buffer+(i-1)*recSize) && (ss == 60.0)) {
           *(long long*)(buffer+i*recSize) += 1000000000LL;
           found = 1;
         } else if (ss == 60.0 && found == 1) {
           *(long long*)(buffer+i*recSize) += 1000000000LL;
         }
       }
     }
   }  
}

/******************************************************************************
 * SearchNeighbor.
 * Find the open neighbohood of the current index that can be used, which has
 * the same key value (due to duplicate key).
 ******************************************************************************/

static int SearchNeighbor (long dataType, void *values, void *value, int *idx,
                           int max, int id)
{
      int i;
      if (dataType == CDF_EPOCH || dataType == CDF_DOUBLE ||
          dataType == CDF_REAL8) {
	for (i = (id-1); i > -1; i--) {
	  if (idx[i] == 1) continue;
          if (((double *)values)[i] < *(double *) value) break;
	  return i;
	}
        for (i = (id+1); i < max; i++) {
          if (idx[i] == 1) continue;
          if (((double *)values)[i] > *(double *) value) break;
          return i;
        }
	return -1;
      } else if (dataType == CDF_EPOCH16) {
        for (i = (id-1); i > -1; i--) {
          if (idx[i] == 1) continue;
          if (((double *)values)[2*i] < *(double *) value) break;
          if ((((double *)values)[2*i] == *(double *) value) &&
              (((double *)values)[2*i+1] < *(((double *) value)+1))) break;
          return i;
        }
        for (i = (id+1); i < max; i++) {
          if (idx[i] == 1) continue;
          if (((double *)values)[2*i] > *(double *) value) break;
          if ((((double *)values)[2*i] == *(double *) value) &&
              (((double *)values)[2*i+1] > *(((double *) value)+1))) break;
          return i;
        }
        return -1;
      } else if (dataType == CDF_TIME_TT2000 || dataType == CDF_INT8) {
        for (i = (id-1); i > -1; i--) {
          if (idx[i] == 1) continue;
          if (((long long *)values)[i] < *(long long *) value) break;
          return i;
        }
        for (i = (id+1); i < max; i++) {
          if (idx[i] == 1) continue;
          if (((long long *)values)[i] > *(long long *) value) break;
          return i;
        }
        return -1;
      } else if (dataType == CDF_INT1 || dataType == CDF_BYTE) {
        for (i = (id-1); i > -1; i--) {
          if (idx[i] == 1) continue;
          if (((sChar *)values)[i] < *(sChar *) value) break;
          return i;
        }
        for (i = (id+1); i < max; i++) {
          if (idx[i] == 1) continue;
          if (((sChar *)values)[i] > *(sChar *) value) break;
          return i;
        }
        return -1;
      } else if (dataType == CDF_UINT1) {
        for (i = (id-1); i > -1; i--) {
          if (idx[i] == 1) continue;
          if (((uChar *)values)[i] < *(uChar *) value) break;
          return i;
        }
        for (i = (id+1); i < max; i++) {
          if (idx[i] == 1) continue;
          if (((uChar *)values)[i] > *(uChar *) value) break;
          return i;
        }
        return -1;
      } else if (dataType == CDF_INT2) {
        for (i = (id-1); i > -1; i--) {
          if (idx[i] == 1) continue;
          if (((short *)values)[i] < *(short *) value) break;
          return i;
        }
        for (i = (id+1); i < max; i++) {
          if (idx[i] == 1) continue;
          if (((short *)values)[i] > *(short *) value) break;
          return i;
        }
        return -1;
      } else if (dataType == CDF_UINT2) {
        for (i = (id-1); i > -1; i--) {
          if (idx[i] == 1) continue;
          if (((unsigned short *)values)[i] < *(unsigned short *) value) break;
          return i;
        }
        for (i = (id+1); i < max; i++) {
          if (idx[i] == 1) continue;
          if (((unsigned short *)values)[i] > *(unsigned short *) value) break;
          return i;
        }
        return -1;
      } else if (dataType == CDF_INT4) {
        for (i = (id-1); i > -1; i--) {
          if (idx[i] == 1) continue;
          if (((int *)values)[i] < *(int *) value) break;
          return i;
        }
        for (i = (id+1); i < max; i++) {
          if (idx[i] == 1) continue;
          if (((int *)values)[i] > *(int *) value) break;
          return i;
        }
        return -1;
      } else if (dataType == CDF_UINT4) {
        for (i = (id-1); i > -1; i--) {
          if (idx[i] == 1) continue;
          if (((unsigned int *)values)[i] < *(unsigned int *) value) break;
          return i;
        }
        for (i = (id+1); i < max; i++) {
          if (idx[i] == 1) continue;
          if (((unsigned int *)values)[i] > *(unsigned int *) value) break;
          return i;
        }
        return -1;
      } else if (dataType == CDF_REAL4 || dataType == CDF_FLOAT) {
        for (i = (id-1); i > -1; i--) {
          if (idx[i] == 1) continue;
          if (((float *)values)[i] < *(float *) value) break;
          return i;
        }
        for (i = (id+1); i < max; i++) {
          if (idx[i] == 1) continue;
          if (((float *)values)[i] > *(float *) value) break;
          return i;
        }
        return -1;
      }
      return -1;
}

/******************************************************************************
* ConvertQOPs.
*    Returns TRUE if execution should continue.
******************************************************************************/

#if defined(mac)
Logical ConvertQOPs (argC, argV)
int *argC;
char **argV[];
{
  DialogPtr dialogP;
  DialogRecord dRecord;
  WindowPtr behind = (WindowPtr) -1;
  ControlHandle controlHs[MAXIMUMin+1];
  Rect iRect;
#ifdef __MWERKS__
  ModalFilterUPP FilterDialogQOPsoUPP = NULL;
  FileFilterUPP FilterForCDFsUPP = NULL;
  UserItemUPP OutlineDefaultButtonUPP;
#endif
  short itemN, iType, i;
  static Logical first = TRUE;
  char cancelTitle[] = "Cancel";

  static Logical logMsg = DEFAULTlogCVT;
  static Logical dispPct = DEFAULTpctCVT;
  static Logical deleteDst = DEFAULTdelCVT;
  static Logical pageOutput = DEFAULTpageCVT;
  static Logical reportInfos = REPORTinfosDEFAULT;
  static Logical reportWarns = REPORTwarningsDEFAULT;
  static Logical reportErrors = REPORTerrorsDEFAULT;
  static Logical negToPos = DEFAULT_NEGtoPOSfp0;
  static Logical dispStats = DEFAULTstatsCVT;
  static int dstFormat = 0;
  static int dstMajority = 0;
  static int zModeSrc = DEFAULTzModeCVT;
  static int dstEncoding = 0;
  static Str255 cacheText = "\p";
  static Str255 srcText = "\p";
  static Str255 dstText = "\p";
  static Str255 sktText = "\p";
  static Str255 sparseText = "\p";
  static Str255 compressText = "\p";

  /****************************************************************************
  * Create the dialog and get the control handles.
  ****************************************************************************/

  dialogP = GetNewDialog (QOPri, &dRecord, behind);
  
  for (itemN = 1; itemN <= MAXIMUMin; itemN++) {
     GetDItem (dialogP, itemN, &iType, (Handle *) &controlHs[itemN], &iRect);
  }

  /****************************************************************************
  * Set the control values.
  ****************************************************************************/

  SetIText ((Handle) controlHs[SRCTEXTin], srcText);
  SetIText ((Handle) controlHs[DSTTEXTin], dstText);
  SetIText ((Handle) controlHs[SKTTEXTin], sktText);
  SetIText ((Handle) controlHs[CACHEin], cacheText);
  SetIText ((Handle) controlHs[SPARSEin], sparseText);
  SetIText ((Handle) controlHs[COMPRESSin], compressText);

  if (logMsg) SetCtlValue (controlHs[LOGin], 1);
  if (dispPct) SetCtlValue (controlHs[PCTin], 1);
  if (deleteDst) SetCtlValue (controlHs[DELETEin], 1);
  if (pageOutput) SetCtlValue (controlHs[PAGEin], 1);
  if (reportInfos) SetCtlValue (controlHs[INFOin], 1);
  if (reportWarns) SetCtlValue (controlHs[WARNin], 1);
  if (reportErrors) SetCtlValue (controlHs[ERRORin], 1);
  if (negToPos) SetCtlValue (controlHs[NEGZin], 1);
  if (dispStats) SetCtlValue (controlHs[STATSin], 1);

  SetCtlValue (controlHs[DSTFORMATinBASE+dstFormat], 1);
  SetCtlValue (controlHs[DSTMAJORITYinBASE+dstMajority], 1);
  SetCtlValue (controlHs[SRCzMODEinBASE+zModeSrc], 1);
  SetCtlValue (controlHs[DSTENCODINGinBASE+dstEncoding], 1);

#ifndef __MWERKS__
  SetDItem (dialogP, (short) ODBin, (short) userItem,
	    (Handle) OutlineDefaultButton, &iRect);
#else
  OutlineDefaultButtonUPP = NewUserItemProc (OutlineDefaultButton);
  SetDItem (dialogP, (short) ODBin, (short) userItem,
	    (Handle) OutlineDefaultButtonUPP, &iRect);
#endif
  /****************************************************************************
  * Change the "Quit" button to a "Cancel" button after the first time.
  ****************************************************************************/

  if (first)
    first = FALSE;
  else
    SetCTitle (controlHs[CANCELin], CtoPstr(cancelTitle));

  /****************************************************************************
  * Display the dialog and wait for user actions.
  ****************************************************************************/
    
  ShowWindow ((WindowPtr) dialogP);
  SetCursor (ARROW_CURSOR);
#ifdef __MWERKS__
  FilterDialogQOPsoUPP = NewModalFilterProc((ProcPtr) FilterDialogQOPso);
#endif

  for (;;) {
#ifndef __MWERKS__
    ModalDialog (FilterDialogQOPso, &itemN);
#else
    ModalDialog (FilterDialogQOPsoUPP, &itemN);
#endif
    switch (itemN) {
      /************************************************************************
      * Ok.
      ************************************************************************/
      case OKin: {
	char *formatOptions[] = { NULL, "-single", "-multi" };
	char *majorityOptions[] = { NULL, "-row", "-column" };
	char *encodingSymbols[] = { NULL, "host", "sun", "vax", "sgi",
				    "hp", "ibmpc", "ibmrs", "network",
				    "mac", "decstation", "next", "alphavmsd",
				    "alphavmsg", "alphaosf1", "alphavmsi" };
	int n;
	char tempS1[1+1];

	/**********************************************************************
	* Get the value of each control.
	**********************************************************************/

	GetIText ((Handle) controlHs[SRCTEXTin], srcText);
	GetIText ((Handle) controlHs[DSTTEXTin], dstText);
	GetIText ((Handle) controlHs[SKTTEXTin], sktText);
	GetIText ((Handle) controlHs[CACHEin], cacheText);
	GetIText ((Handle) controlHs[SPARSEin], sparseText);
	GetIText ((Handle) controlHs[COMPRESSin], compressText);

	logMsg = GetCtlValue (controlHs[LOGin]);
	dispPct = GetCtlValue (controlHs[PCTin]);
	deleteDst = GetCtlValue (controlHs[DELETEin]);
	pageOutput = GetCtlValue (controlHs[PAGEin]);
	reportInfos = GetCtlValue (controlHs[INFOin]);
	reportWarns = GetCtlValue (controlHs[WARNin]);
	reportErrors = GetCtlValue (controlHs[ERRORin]);
	negToPos = GetCtlValue (controlHs[NEGZin]);
	dispStats = GetCtlValue (controlHs[STATSin]);
	
	for (dstFormat = 0; dstFormat < 3; dstFormat++) {
	   if (GetCtlValue(controlHs[DSTFORMATinBASE+dstFormat])) break;
	}
	for (dstMajority = 0; dstMajority < 3; dstMajority++) {
	   if (GetCtlValue(controlHs[DSTMAJORITYinBASE+dstMajority])) break;
	}
	for (dstEncoding = 0; dstEncoding < 16; dstEncoding++) {
	   if (GetCtlValue(controlHs[DSTENCODINGinBASE+dstEncoding])) break;
	}
	for (zModeSrc = 0; zModeSrc < 3; zModeSrc++) {
	   if (GetCtlValue(controlHs[SRCzMODEinBASE+zModeSrc])) break;
	}
	
	/**********************************************************************
	* Build argc/argv.
	**********************************************************************/

	*argC = 11 + BOO(NULpString(srcText),0,1) +
			     BOO(NULpString(dstText),0,1) +
			     BOO(NULpString(sktText),0,2) +
			     BOO(NULpString(cacheText),0,2) +
			     BOO(NULpString(sparseText),0,2) +
			     BOO(NULpString(compressText),0,2) +
			     BOO(dstFormat != 0, 1, 0) +
			     BOO(dstMajority != 0, 1, 0) +
			     BOO(dstEncoding != 0, 2, 0);
	*argV = (char **) cdf_AllocateMemory ((size_t)*argC * sizeof(char *),
							  FatalError);
	
	n = 0;
	MAKEstrARGv (argV, n, pgmName)

	if (!NULpString(srcText)) {
	  PtoCstr (srcText);
	  MAKEstrARGv (argV, n, (char *) srcText)
	  CtoPstr ((char *) srcText);
	}

	if (!NULpString(dstText)) {
	  PtoCstr (dstText);
	  MAKEstrARGv (argV, n, (char *) dstText)
	  CtoPstr ((char *) dstText);
	}

	MAKEbooARGv (argV, n, logMsg, "-log", "-nolog")
	MAKEbooARGv (argV, n, dispPct, "-percent", "-nopercent")
	MAKEbooARGv (argV, n, deleteDst, "-delete", "-nodelete")
	MAKEbooARGv (argV, n, pageOutput, "-page", "-nopage")
	MAKEbooARGv (argV, n, negToPos, "-neg2posfp0", "-noneg2posfp0")
	MAKEbooARGv (argV, n, dispStats, "-statistics", "-nostatistics")
	
	MAKEstrARGv (argV, n, "-zmode")
	snprintf (tempS1, (size_t) sizeof(tempS1), "%d", zModeSrc);
	MAKEstrARGv (argV, n, tempS1)

	MAKEstrARGv (argV, n, "-report")
	MAKEstrARGv (argV, n, StatusCodeReportOptions(reportErrors,
						      reportWarns,
						      reportInfos))

	if (dstFormat != 0) {
	  MAKEstrARGv (argV, n, formatOptions[dstFormat])
	}

	if (dstMajority != 0) {
	  MAKEstrARGv (argV, n, majorityOptions[dstMajority])
	}

	if (dstEncoding != 0) {
	  MAKEstrARGv (argV, n, "-encoding")
	  MAKEstrARGv (argV, n, encodingSymbols[dstEncoding])
	}

	if (!NULpString(sktText)) {
	  MAKEstrARGv (argV, n, "-skeleton")
	  PtoCstr (sktText);
	  MAKEstrARGv (argV, n, (char *) sktText)
	  CtoPstr ((char *) sktText);
	}

	if (!NULpString(cacheText)) {
	  MAKEstrARGv (argV, n, "-cache")
	  PtoCstr (cacheText);
	  MAKEstrARGv (argV, n, (char *) cacheText)
	  CtoPstr ((char *) cacheText);
	}

	if (!NULpString(sparseText)) {
	  MAKEstrARGv (argV, n, "-sparseness")
	  PtoCstr (sparseText);
	  MAKEstrARGv (argV, n, (char *) sparseText)
	  CtoPstr ((char *) sparseText);
	}

	if (!NULpString(compressText)) {
	  MAKEstrARGv (argV, n, "-compression")
	  PtoCstr (compressText);
	  MAKEstrARGv (argV, n, (char *) compressText)
	  CtoPstr ((char *) compressText);
	}

	/**********************************************************************
	* Close the dialog and return.
	**********************************************************************/
#ifdef __MWERKS__
        DisposeRoutineDescriptor (FilterDialogQOPsoUPP);
	DisposeRoutineDescriptor (OutlineDefaultButtonUPP);
#endif
	CloseDialog (dialogP);
	return TRUE;
      }
      /************************************************************************
      * Help.
      ************************************************************************/
      case HELPin: {
	int n;
	*argC = 1;
	*argV = (char **) cdf_AllocateMemory ((size_t)*argC * sizeof(char *),
								  FatalError);
	n = 0;
	MAKEstrARGv (argV, n, pgmName)
#ifdef __MWERKS__
        DisposeRoutineDescriptor (FilterDialogQOPsoUPP);
	DisposeRoutineDescriptor (OutlineDefaultButtonUPP);
#endif
	CloseDialog (dialogP);
	return TRUE;
      }
      /************************************************************************
      * Cancel.
      ************************************************************************/
      case CANCELin:
#ifdef __MWERKS__
        DisposeRoutineDescriptor (FilterDialogQOPsoUPP);
	DisposeRoutineDescriptor (OutlineDefaultButtonUPP);
#endif
	CloseDialog (dialogP);
	return FALSE;
      /************************************************************************
      * Select source CDF specification.
      ************************************************************************/
      case SRCSELECTin: {
	StandardFileReply srcReply;
	char srcPath[DU_MAX_PATH_LEN+1];
#ifndef __MWERKS__
	StandardGetFile (FilterForCDFs, -1, NULL, &srcReply);
#else
	FilterForCDFsUPP = NewFileFilterProc((ProcPtr) FilterForCDFs);
	StandardGetFile (FilterForCDFsUPP, -1, NULL, &srcReply);
        DisposeRoutineDescriptor (FilterForCDFsUPP);
#endif
	if (srcReply.sfGood && !srcReply.sfIsFolder && !srcReply.sfIsVolume) {
	  BuildMacPath (&srcReply.sfFile, srcPath, TRUE);
	  srcText[0] = strlen (srcPath);
	  strcpyX ((char *) &srcText[1], srcPath, 255);
	  SetIText ((Handle) controlHs[SRCTEXTin], srcText);
	}
	break;
      }
      /************************************************************************
      * Select destination CDF specification.
      *     The cursor is set because `StandardPutFile' leaves the cursor as
      * an iBeam (instead of returning it to what it was).
      ************************************************************************/
      case DSTSELECTin: {
	StandardFileReply dstReply;
	char dstPath[DU_MAX_PATH_LEN+1], prompt[] = "Enter specification:";
	StandardPutFile (CtoPstr(prompt), CtoPstr(""), &dstReply);
	if (dstReply.sfGood && !dstReply.sfIsFolder && !dstReply.sfIsVolume) {
	  BuildMacPath (&dstReply.sfFile, dstPath, TRUE);
	  dstText[0] = strlen (dstPath);
	  strcpyX ((char *) &dstText[1], dstPath, 255);
	  SetIText ((Handle) controlHs[DSTTEXTin], dstText);
	}
	SetCursor (&(qd.arrow));
	break;
      }
      /************************************************************************
      * Select skeleton CDF.
      ************************************************************************/
      case SKTSELECTin: {
	StandardFileReply sktReply;
	char sktPath[DU_MAX_PATH_LEN+1];
#ifndef __MWERKS__
	StandardGetFile (FilterForCDFs, -1, NULL, &sktReply);
#else
	FilterForCDFsUPP = NewFileFilterProc((ProcPtr) FilterForCDFs);
	StandardGetFile (FilterForCDFsUPP, -1, NULL, &sktReply);
        DisposeRoutineDescriptor (FilterForCDFsUPP);
#endif
	if (sktReply.sfGood && !sktReply.sfIsFolder && !sktReply.sfIsVolume) {
	  BuildMacPath (&sktReply.sfFile, sktPath, TRUE);
	  sktText[0] = strlen (sktPath);
	  strcpyX ((char *) &sktText[1], sktPath, 255);
	  SetIText ((Handle) controlHs[SKTTEXTin], sktText);
	}
	break;
      }
      /************************************************************************
      * Check boxes.
      ************************************************************************/
      case LOGin:
	if (GetCtlValue(controlHs[LOGin])) {
	  SetCtlValue (controlHs[LOGin], 0);
	  SetCtlValue (controlHs[PCTin], 0);
	}
	else
	  SetCtlValue (controlHs[LOGin], 1);
	break;
      case PCTin:
	if (GetCtlValue(controlHs[PCTin]))
	  SetCtlValue (controlHs[PCTin], 0);
	else {
	  SetCtlValue (controlHs[PCTin], 1);
	  SetCtlValue (controlHs[LOGin], 1);
	}
	break;
      case DELETEin:
      case PAGEin:
      case INFOin:
      case WARNin:
      case ERRORin:
      case NEGZin:
      case STATSin:
	SetCtlValue (controlHs[itemN], BOO(GetCtlValue(controlHs[itemN]),0,1));
	break;
      /************************************************************************
      * Radio buttons.
      ************************************************************************/
      case DSTFORMATinBASE+0:
      case DSTFORMATinBASE+1:
      case DSTFORMATinBASE+2:
	for (i = 0; i < 3; i++) SetCtlValue (controlHs[DSTFORMATinBASE+i], 0);
	SetCtlValue (controlHs[itemN], 1);
	break;
      case DSTMAJORITYinBASE+0:
      case DSTMAJORITYinBASE+1:
      case DSTMAJORITYinBASE+2:
	for (i = 0; i < 3; i++) SetCtlValue (controlHs[DSTMAJORITYinBASE+i],0);
	SetCtlValue (controlHs[itemN], 1);
	break;
      case DSTENCODINGinBASE+0:
      case DSTENCODINGinBASE+1:
      case DSTENCODINGinBASE+2:
      case DSTENCODINGinBASE+3:
      case DSTENCODINGinBASE+4:
      case DSTENCODINGinBASE+5:
      case DSTENCODINGinBASE+6:
      case DSTENCODINGinBASE+7:
      case DSTENCODINGinBASE+8:
      case DSTENCODINGinBASE+9:
      case DSTENCODINGinBASE+10:
      case DSTENCODINGinBASE+11:
      case DSTENCODINGinBASE+12:
      case DSTENCODINGinBASE+13:
      case DSTENCODINGinBASE+14:
      case DSTENCODINGinBASE+15:
	for (i = 0; i < 16; i++) SetCtlValue(controlHs[DSTENCODINGinBASE+i],0);
	SetCtlValue (controlHs[itemN], 1);
	break;
      case SRCzMODEinBASE+0:
      case SRCzMODEinBASE+1:
      case SRCzMODEinBASE+2:
	for (i = 0; i < 3; i++) SetCtlValue (controlHs[SRCzMODEinBASE+i], 0);
	SetCtlValue (controlHs[itemN], 1);
	break;
    }
  }
}
#endif
