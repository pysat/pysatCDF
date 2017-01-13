/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                 CDFirsdump - dump all internal records found in 
*                                         dotCDF file.
*
*  Version 2.6b, 21-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  29-May-91, J Love     Original version.
*   V1.1  25-Jun-91, J Love     Moved increment.  Display fill value.  Display
*                               extend records.  CDF_EPOCH added as a data
*                               type.  Added QOP.  Added PageInst.
*   V1.2  26-Jul-91, J Love     Added SEEK_CUR.  Added XDR decoding.
*                               Use 'Exit'/'ExitBAD'.
*   V1.3   7-Oct-91, J Love     Modified for IBM-PC port (and IBM-RS6000).
*   V2.0  21-Apr-92, J Love     IBM PC port/CDF V2.2.
*   V2.1  29-Sep-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V2.2  22-Oct-92, J Love     Newline after pad values.
*   V2.3  24-Jan-94, J Love     CDF V2.4.
*   V2.3a  8-Feb-94, J Love     DEC Alpha/OpenVMS port.
*   V2.4  21-Dec-94, J Love     CDF V2.5.
*   V2.4a 13-Jan-95, J Love     Uppercase file extensions.
*   V2.4b 26-Jan-95, J Love     Display meaning of `flags'.
*   V2.4c 22-Mar-95, J Love     Check for all NULs following copyright.
*   V2.4d  6-Apr-95, J Love     POSIX.
*   V2.5  25-May-95, J Love     EPOCH styles.  Fixed `all NULs' message.
*   V2.5a 19-Sep-95, J Love     CHECKforABORTso.
*   V2.5b 28-Sep-95, J Love     Macintosh dialog filtering.  Outline default
*                               button.
*   V2.6   3-Oct-96, J Love     CDF V2.6.
*   V2.6a  3-Sep-97, J Love	Fixed ScanCDF.
*   V2.6b 21-Nov-97, J Love	Windows NT/Visual C++.
*   V3.0   7-Jan-05, M Liu      CDF V3.0. Also, remove the data read if the
*                               it does not require a full dump.
*   V3.1   9-Feb-06, M Liu      Changed not to go any further after eof in GDR
*                               is reached for a uncompressed CDF or after CPR
*                               is read for a compressed CDF.
*   V3.2   9-Jul-06, M Liu      Renamed from cdfdump to cdfirsdump.
*   V3.3   4-Apr-11, M Liu      Modified to handle TT2000 style epoch.
*
*   Note: This program may not work properly if the CDF file doesn't have the 
*         CDR, GDR record in sequence at the top of the file. Since it reads 
*         data sequentially from the beginning, information from GDR is needed 
*         for VDR (at least for rVDR). If a VDR is read ahead of GDR, problems
*         will occur. Use cdfconvert to convert/re-organize the original CDF if
*         there is a read problem. 
******************************************************************************/

#include "cdfirsdump.h"

/******************************************************************************
* Increased stack size for Borland C on IBM PC.
******************************************************************************/

#if defined(BORLANDC)
extern unsigned _stklen = BORLANDC_STACK_SIZE;
#endif

/******************************************************************************
* Global variables & macro constants.
******************************************************************************/

long CCRcount, CDRcount, GDRcount, ADRcount, AgrEDRcount, AzEDRcount,
     rVDRcount, zVDRcount, VXRcount, VVRcount, CPRcount, SPRcount,
     CVVRcount, UIRcount, gADRcount, vADRcount;
long CCRbytes, CDRbytes, GDRbytes, ADRbytes, AgrEDRbytes, AzEDRbytes,
     rVDRbytes, zVDRbytes, VXRbytes, VVRbytes, CPRbytes, SPRbytes,
     CVVRbytes, UIRbytes;
OFF_T CCRbytes64, CDRbytes64, GDRbytes64, ADRbytes64, AgrEDRbytes64, 
      AzEDRbytes64, rVDRbytes64, zVDRbytes64, VXRbytes64, VVRbytes64, 
      CPRbytes64, SPRbytes64, CVVRbytes64, UIRbytes64;
long usedBytes, wastedBytes;
long fileSize = -1;
int checksum = 1;
OFF_T fileSize64 = -1;
OFF_T usedBytes64, wastedBytes64;
FILE *OUTfp;
Logical EOFok;		/* When TRUE, an EOF is expected. */
long radix;		/* 10 (decimal) or 16 (hexadecimal). */
Logical isLFS;

/******************************************************************************
* Local function prototypes.
******************************************************************************/

static void DisplaySummary PROTOARGs((long));
static void DisplaySummary64 PROTOARGs((long));
static void CheckSizes PROTOARGs((
  vFILE *CDFfp, long startingOffset
));
static void CheckSizes64 PROTOARGs((
  vFILE *CDFfp, OFF_T startingOffset
));
static void ScanCDF PROTOARGs((
  vFILE *CDFfp, Logical showIndexing, long level, long startingOffset,
  Logical nodata
));
static void ScanCDF64 PROTOARGs((
  vFILE *CDFfp, Logical showIndexing, long level, OFF_T startingOffset,
  Logical nodata
));
static void DumpVXR PROTOARGs((
  vFILE *CDFfp, Int32 vxrOffset, int indent
));
static void DumpVXR64 PROTOARGs((
  vFILE *CDFfp, OFF_T vxrOffset, int indent
));
static char *FileOffset PROTOARGs((Int32 offset));
static char *FileOffset64 PROTOARGs((OFF_T offset));
static char *Hex32 PROTOARGs((Int32 value));
static char *Deci64 PROTOARGs((OFF_T value));
static char *Hex64 PROTOARGs((OFF_T value));
static char *Deci32 PROTOARGs((Int32 value));
static Logical SeekTo PROTOARGs((vFILE *fp, long offset, int how));
static Logical SeekTo64 PROTOARGs((vFILE *fp, OFF_T offset, int how));
static Logical Read4 PROTOARGs((vFILE *fp, Int32 *value));
static Logical Read4_64 PROTOARGs((vFILE *fp, Int32 *value));
static Logical Read4s PROTOARGs((vFILE *fp, Int32 *values, int count));
static Logical Read4s_64 PROTOARGs((vFILE *fp, Int32 *values, int count));
static Logical Read8 PROTOARGs((vFILE *fp, OFF_T *value));
static Logical Read8s PROTOARGs((vFILE *fp, OFF_T *values, int count));
static Logical ReadBytes PROTOARGs((vFILE *fp, void *bytes, Int32 count));
static Logical ReadBytes64 PROTOARGs((vFILE *fp, void *bytes, Int32 count));
static void DisplayReadFailure PROTOARGs((vFILE *fp));

/******************************************************************************
* Main.
******************************************************************************/

#if !defined(win32) || (defined(win32) && defined(ALONE))
MAIN {
  Logical success = TRUE;
  strcpyX (pgmName, "CDFirsdump", MAX_PROGRAM_NAME_LEN);
#if defined(mac)
  MacExecuteSO (DumpCDFIRs, DumpQOPs);
#else
  success = DumpCDFIRs (argc, argv);
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
* DumpCDFIRs.
*    Returns FALSE if an error occurred.
******************************************************************************/

Logical DumpCDFIRs (argC, argV)
int argC;
char *argV[];
{
  vFILE *CDFfp;
  Logical checkSizes, showIndexing, showSummary;
  long level, startingOffset; OFF_T startingOffset64;
  char CDFpath[DU_MAX_PATH_LEN+1], tCDFpath[DU_MAX_PATH_LEN+1];
  char oSpec[DU_MAX_PATH_LEN+1], oDir[DU_MAX_DIR_LEN+1];
  char oName[DU_MAX_NAME_LEN+1];
  Logical nodata;
  QOP *qop; Logical qopError = FALSE;
  static char *validQuals[] = {
    "output", "brief", "most", "full", "page", "nopage", "sizes", "nosizes",
    "offset", "indexing", "noindexing", "summary", "nosummary", "about", "10",
    "16", "data", NULL
  };
  static int optRequired[] = {
    TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 0
  };
  /****************************************************************************
  * Determine qualifiers/options/parameters.
  ****************************************************************************/
  switch (argC) {
    case 1:
      PageOLH ("cdfirsdump.olh", argV[0]);
      return TRUE;
    case 2:
      if (strcmp(argV[1],"-java") == 0) {
        pagingOn = FALSE;
        PageOLH ("cdfirsdumpj.olh", argV[0]);
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
      * Get CDF path.
      ************************************************************************/
      if (qop->Nparms < 1) {
        DisplayError ("Missing parameter.");
        qopError = TRUE;
      }
      else
        strcpyX (CDFpath, qop->parms[CDFPATHparm], DU_MAX_PATH_LEN);
      /************************************************************************
      * Check for `page' qualifier (this must be done before the `output'
      * qualifier is checked).
      ************************************************************************/
      qopError = qopError | !TFqualifier(qop,&pagingOn,PAGEqual,NOPAGEqual,
				         DEFAULTpageIRsDUMP,"page");
      /************************************************************************
      * Check of offset qualifier.
      ************************************************************************/
      if (qop->qualEntered[OFFSETqual]) {
        if (sscanf(qop->qualOpt[OFFSETqual],"%ld",&startingOffset) != 1) {
	  DisplayError ("Illegal starting offset.");
	  qopError = TRUE;
        }
        if (startingOffset < 0) {
	  DisplayError ("Illegal starting offset.");
	  qopError = TRUE;
        }
        startingOffset64 = (OFF_T) startingOffset;
      }
      else {
        startingOffset = NO_OFFSET;
	startingOffset64 = (OFF_T)NO_OFFSET64;
      }
      /************************************************************************
      * Check for output file qualifier.
      ************************************************************************/
      if (qop->qualEntered[OUTPUTqual]) {
        if (pagingOn) {
	  DisplayError ("Conflicting qualifiers (`output' and `page').");
	  qopError = TRUE;
        }
        else
	  strcpyX (oSpec, qop->qualOpt[OUTPUTqual], DU_MAX_PATH_LEN);
      }
      else
        strcpyX (oSpec, "", DU_MAX_PATH_LEN);
      /************************************************************************
      * Check for level qualifier.
      ************************************************************************/
      qopError = qopError | !S3qualifierLong(qop,&level,BRIEFqual,BRIEF_,
					     MOSTqual,MOST_,FULLqual,FULL_,
					     (long)DEFAULTlevelIRsDUMP,"level");
      /************************************************************************
      * Check for `sizes' qualifier.
      ************************************************************************/
      qopError = qopError | !TFqualifier(qop,&checkSizes,SIZESqual,NOSIZESqual,
				         DEFAULTsizesIRsDUMP,"sizes");
      /************************************************************************
      * Check for `indexing' qualifier.
      ************************************************************************/
      qopError = qopError | !TFqualifier(qop,&showIndexing,INDEXINGqual,
				         NOINDEXINGqual,DEFAULTindexingIRsDUMP,
				         "indexing");
      /************************************************************************
      * Check for `summary' qualifier.
      ************************************************************************/
      qopError = qopError | !TFqualifier(qop,&showSummary,SUMMARYqual,
				         NOSUMMARYqual,DEFAULTsummaryIRsDUMP,
				         "summary");
      /************************************************************************
      * Check for file offset radix qualifiers.
      ************************************************************************/
      qopError = qopError | !S2qualifierLong(qop,&radix,DECIqual,10L,HEXAqual,
					     16L,(long)DEFAULTradixIRsDUMP,
					     "radix [10|16]");
      /************************************************************************
      * Check for `data' qualifier.
      ************************************************************************/
      if (qop->qualEntered[DATAqual]) {
        nodata = FALSE;
      }
      else
        nodata = !DEFAULTdataIRsDUMP;
      /************************************************************************
      * Free QOP memory and check for an error.
      ************************************************************************/
      cdf_FreeMemory (qop, FatalError);
      if (qopError) return FALSE;
      break;
  }
  if (!NULstring(oSpec)) {
    ParsePath (oSpec, oDir, oName);
    if (strchr(oName,'.') == NULL) strcatX (oSpec, ".dmp", DU_MAX_PATH_LEN);
  }
  /***************************************************************************
  * Display dumping message.
  ***************************************************************************/
  WriteOut (stdout, "Dumping \"");
  WriteOut (stdout, CDFpath);
  if (!EndsWithIgCase(CDFpath, ".cdf"))
    WriteOut (stdout, ".cdf");
  if (!NULstring(oSpec)) {
    WriteOut (stdout, "\" to \"");
    WriteOut (stdout, oSpec);
  }
  WriteOut (stdout, "\"\n");
  /****************************************************************************
  * Open CDF.
  ****************************************************************************/
  CDFfp = NULL;
  if (CheckLFS(CDFpath, &isLFS, tCDFpath) == CDF_OK) {
    if (!isLFS)
      CDFfp = V_open (tCDFpath, READ_ONLY_a_mode);
    else
      CDFfp = V_open64 (tCDFpath, READ_ONLY_a_mode);
  }
  if (CDFfp == NULL) {
    WriteOut (stdout, "Error opening CDF.\n");
    return FALSE;
  }
  /****************************************************************************
  * Open output file (if specified).
  ****************************************************************************/
  if (NULstring(oSpec))
    OUTfp = stdout;
  else {
    OUTfp = fopen (oSpec, "w");
    if (OUTfp == NULL) {
      WriteOut (stdout, "Unable to open output file.\n");
      if (!isLFS)
        V_close (CDFfp, NULL, NULL);
      else
	V_close64 (CDFfp, NULL, NULL);
      return FALSE;
    }
  }
  /****************************************************************************
  * Initialize global variables.
  ****************************************************************************/
  EOFok = FALSE;
  usedBytes = 0;	usedBytes64 = 0;
  wastedBytes = 0;	wastedBytes64 = 0;
  CCRcount = 0;
  CDRcount = 0;
  GDRcount = 0;
  ADRcount = 0;		gADRcount = vADRcount = 0;
  AgrEDRcount = 0;
  AzEDRcount = 0;
  rVDRcount = 0;
  zVDRcount = 0;
  VXRcount = 0;
  VVRcount = 0;
  CVVRcount = 0;
  CPRcount = 0;
  SPRcount = 0;
  UIRcount = 0;
  CCRbytes = 0;		CCRbytes64 = 0;
  CDRbytes = 0;		CDRbytes64 = 0;
  GDRbytes = 0;		GDRbytes64 = 0;
  ADRbytes = 0;		ADRbytes64 = 0;
  AgrEDRbytes = 0;	AgrEDRbytes64 = 0;
  AzEDRbytes = 0;	AzEDRbytes64 = 0;
  rVDRbytes = 0;	rVDRbytes64 = 0;
  zVDRbytes = 0;	zVDRbytes64 = 0;
  VXRbytes = 0;		VXRbytes64 = 0;
  VVRbytes = 0;		VVRbytes64 = 0;
  CVVRbytes = 0;	CVVRbytes64 = 0;
  CPRbytes = 0;		CPRbytes64 = 0;
  SPRbytes = 0;		SPRbytes64 = 0;
  UIRbytes = 0;		UIRbytes64 = 0;
  /****************************************************************************
  * Check record sizes (if requested).
  ****************************************************************************/
  if (!isLFS) {
    if (checkSizes) CheckSizes (CDFfp, startingOffset);
  } else
    if (checkSizes) CheckSizes64 (CDFfp, startingOffset64);
  /****************************************************************************
  * Scan internal records.
  ****************************************************************************/
  if (!isLFS)
    ScanCDF (CDFfp, showIndexing, level, startingOffset, nodata);
  else
    ScanCDF64 (CDFfp, showIndexing, level, startingOffset64, nodata);
  /****************************************************************************
  * Display summary.
  ****************************************************************************/
  if (showSummary) {
    if (!isLFS) DisplaySummary (level);
    else        DisplaySummary64 (level);
  }
  if (!isLFS) V_close (CDFfp, NULL, NULL);
  else        V_close64 (CDFfp, NULL, NULL);
  if (OUTfp != stdout) fclose (OUTfp);
  return TRUE;
}

/******************************************************************************
* CheckSizes.
******************************************************************************/

static void CheckSizes (CDFfp, startingOffset)
vFILE *CDFfp;
long startingOffset;
{
  long offset; Int32 recordType;
  Int32 recordSize;
  char text[MAX_SCREENLINE_LEN+1];
  WriteOut (OUTfp, "\nChecking record sizes...\n\n");
  offset = BOO(startingOffset != NO_OFFSET,startingOffset,V2_CDR_OFFSET);
  if (!SeekTo(CDFfp,offset,vSEEK_SET)) return;
  for (;;) {
     EOFok = TRUE;
     if (offset == fileSize) return;
     if (!Read4(CDFfp,&recordSize)) return;
     EOFok = FALSE;
     if (recordSize < 1) {
       snprintf (text, (size_t) sizeof(text),
		 "\nIllegal record size detected (%ld) (@%s).\n",
		 (long) recordSize, FileOffset(offset));
       WriteOut (OUTfp, text);
       return;
     }
     if (!Read4(CDFfp,&recordType)) return;
     switch (recordType) {
       case CCR_: strcpyX (text, "CCR", MAX_SCREENLINE_LEN); break;
       case CDR_: strcpyX (text, "CDR", MAX_SCREENLINE_LEN); break;
       case GDR_: strcpyX (text, "GDR", MAX_SCREENLINE_LEN); break;
       case ADR_: strcpyX (text, "ADR", MAX_SCREENLINE_LEN); break;
       case AgrEDR_: strcpyX (text, "AgrEDR", MAX_SCREENLINE_LEN); break;
       case AzEDR_: strcpyX (text, "AzEDR", MAX_SCREENLINE_LEN); break;
       case rVDR_: strcpyX (text, "rVDR", MAX_SCREENLINE_LEN); break;
       case zVDR_: strcpyX (text, "zVDR", MAX_SCREENLINE_LEN); break;
       case VXR_: strcpyX (text, "VXR", MAX_SCREENLINE_LEN); break;
       case VVR_: strcpyX (text, "VVR", MAX_SCREENLINE_LEN); break;
       case CVVR_: strcpyX (text, "CVVR", MAX_SCREENLINE_LEN); break;
       case CPR_: strcpyX (text, "CPR", MAX_SCREENLINE_LEN); break;
       case SPR_: strcpyX (text, "SPR", MAX_SCREENLINE_LEN); break;
       case UIR_: strcpyX (text, "UIR", MAX_SCREENLINE_LEN); break;
       default: {
	 snprintf (text, (size_t) sizeof(text),
		   "\nUnknown record type detected (%ld).\n",
		   (long) recordType);
	 WriteOut (OUTfp, text);
	 return;
       } 
     }
     snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	       ", %ld bytes (offset: %s)\n",
	       (long) recordSize, FileOffset(offset));
     WriteOut (OUTfp, text);
     offset += recordSize;
     if (!SeekTo(CDFfp,offset,vSEEK_SET)) return;
     CHECKforABORTso
  }
}

/******************************************************************************
* CheckSizes64.
******************************************************************************/

static void CheckSizes64 (CDFfp, startingOffset)
vFILE *CDFfp;
OFF_T startingOffset;
{
  OFF_T offset, recordSize; Int32 recordType;
  char text[MAX_SCREENLINE_LEN+1];
  WriteOut (OUTfp, "\nChecking record sizes...\n\n");
  offset = BOO(startingOffset != (OFF_T)NO_OFFSET64,startingOffset,
                                                    V3_CDR_OFFSET64);
  if (!SeekTo64(CDFfp,startingOffset,vSEEK_SET)) return;
  for (;;) {
     EOFok = TRUE;
     if (!Read8(CDFfp,&recordSize)) return;
     EOFok = FALSE;
     if (recordSize < (OFF_T) 1) {
#if !defined(win32) && !defined(__MINGW32__)
       snprintf (text, (size_t) sizeof(text),
		 "\nIllegal record size detected (%lld) (%s).\n",
                 (long long) recordSize, FileOffset64(offset));
#else
       snprintf (text, (size_t) sizeof(text),
		 "\nIllegal record size detected (%I64d) (%s).\n",
                 (long long) recordSize, FileOffset64(offset));
#endif
       WriteOut (OUTfp, text);
       snprintf (text, (size_t) sizeof(text),
		 "     (May have reached the end of the valid data.)\n");
       WriteOut (OUTfp, text);
       return;
     }
     if (!Read4_64(CDFfp,&recordType)) return;
     switch (recordType) {
       case CCR_: strcpyX (text, "CCR", MAX_SCREENLINE_LEN); break;
       case CDR_: strcpyX (text, "CDR", MAX_SCREENLINE_LEN); break;
       case GDR_: strcpyX (text, "GDR", MAX_SCREENLINE_LEN); break;
       case ADR_: strcpyX (text, "ADR", MAX_SCREENLINE_LEN); break;
       case AgrEDR_: strcpyX (text, "AgrEDR", MAX_SCREENLINE_LEN); break;
       case AzEDR_: strcpyX (text, "AzEDR", MAX_SCREENLINE_LEN); break;
       case rVDR_: strcpyX (text, "rVDR", MAX_SCREENLINE_LEN); break;
       case zVDR_: strcpyX (text, "zVDR", MAX_SCREENLINE_LEN); break;
       case VXR_: strcpyX (text, "VXR", MAX_SCREENLINE_LEN); break;
       case VVR_: strcpyX (text, "VVR", MAX_SCREENLINE_LEN); break;
       case CVVR_: strcpyX (text, "CVVR", MAX_SCREENLINE_LEN); break;
       case CPR_: strcpyX (text, "CPR", MAX_SCREENLINE_LEN); break;
       case SPR_: strcpyX (text, "SPR", MAX_SCREENLINE_LEN); break;
       case UIR_: strcpyX (text, "UIR", MAX_SCREENLINE_LEN); break;
       default: {
         snprintf (text, (size_t) sizeof(text),
		   "\nUnknown record type detected (%ld).\n",
                   (long) recordType);
         WriteOut (OUTfp, text);
         return;
       }
     }
#if !defined(win32) && !defined(__MINGW32__)
     snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	       ", %lld bytes (offset: %s)\n",
               (long long) recordSize, FileOffset64(offset));
#else
     snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	       ", %I64d bytes (offset: %s)\n",
               (long long) recordSize, FileOffset64(offset));
#endif
     WriteOut (OUTfp, text);
     offset += recordSize;
     if (!SeekTo64(CDFfp,offset,vSEEK_SET)) return;
     CHECKforABORTso
  }
}

/******************************************************************************
* ScanCDF.
******************************************************************************/

static void ScanCDF (CDFfp, showIndexing, level, startingOffset, nodata)
vFILE *CDFfp;
Logical showIndexing;
long level;
long startingOffset;
Logical nodata;
{
  Int32 recordSize, recordType, encoding, nBytes, rNumDims;
  long offset; int dimN;
  Logical VDRwastedSpace;
  Int32 int32; char text[MAX_SCREENLINE_LEN+1];
  /****************************************************************************
  * Dump magic number(s).
  ****************************************************************************/
  WriteOut (OUTfp, "\nScanning records...\n\n");
  if (startingOffset == NO_OFFSET) {
    if (!SeekTo(CDFfp,0,vSEEK_SET)) return;
    if (!Read4(CDFfp,&int32)) return;
    if (MOST(level)) {
      snprintf (text, (size_t) sizeof(text),
		"Magic number (1): %s\n", Hex32(int32));
      WriteOut (OUTfp, text);
    }
    if (!Read4(CDFfp,&int32)) return;
    if (MOST(level)) {
      snprintf (text, (size_t) sizeof(text),
		"Magic number (2): %s\n", Hex32(int32));
      WriteOut (OUTfp, text);
    }
    usedBytes += 8;
  }
  else {
    if (!SeekTo(CDFfp,startingOffset,vSEEK_SET)) return;
  }
  /****************************************************************************
  * Read and dump records until EOF (or illegal record) reached.
  ****************************************************************************/
  for (;;) {
     /*************************************************************************
     * Read record size.
     *************************************************************************/
     offset = V_tell (CDFfp);
     EOFok = TRUE;
     if (offset == fileSize) return;
     if (!Read4(CDFfp,&recordSize)) return;
     EOFok = FALSE;
     if (recordSize < 1) {
       snprintf (text, (size_t) sizeof(text),
		 "\nIllegal record size detected (%ld) (@%s)\n", 
		 (long) recordSize, FileOffset(offset));
       WriteOut (OUTfp, text);
       return;
     }
     if (MOST(level)) {
       snprintf (text, (size_t) sizeof(text),
		 "\nRecordSize: %ld (@ %s)\n",
                 (long) recordSize, FileOffset((Int32)offset));
       WriteOut (OUTfp, text);
     }
     /*************************************************************************
     * Read record type.
     *************************************************************************/
     if (!Read4(CDFfp,&recordType)) return;
     if (MOST(level)) {
       snprintf (text, (size_t) sizeof(text),
		 "RecordType: %ld ", (long) recordType);
       switch (recordType) {
         case CCR_: strcatX (text, "(CCR)\n", MAX_SCREENLINE_LEN); break;
         case CDR_: strcatX (text, "(CDR)\n", MAX_SCREENLINE_LEN); break;
         case GDR_: strcatX (text, "(GDR)\n", MAX_SCREENLINE_LEN); break;
         case ADR_: strcatX (text, "(ADR)\n", MAX_SCREENLINE_LEN); break;
         case AgrEDR_: strcatX (text, "(AgrEDR)\n", MAX_SCREENLINE_LEN); break;
         case AzEDR_: strcatX (text, "(AzEDR)\n", MAX_SCREENLINE_LEN); break;
         case rVDR_: strcatX (text, "(rVDR)\n", MAX_SCREENLINE_LEN); break;
         case zVDR_: strcatX (text, "(zVDR)\n", MAX_SCREENLINE_LEN); break;
         case VXR_: strcatX (text, "(VXR)\n", MAX_SCREENLINE_LEN); break;
         case VVR_: strcatX (text, "(VVR)\n", MAX_SCREENLINE_LEN); break;
         case CVVR_: strcatX (text, "(CVVR)\n", MAX_SCREENLINE_LEN); break;
         case CPR_: strcatX (text, "(CPR)\n", MAX_SCREENLINE_LEN); break;
         case SPR_: strcatX (text, "(SPR)\n", MAX_SCREENLINE_LEN); break;
         case UIR_: strcatX (text, "(UIR)\n", MAX_SCREENLINE_LEN); break;
         default: {
           if (offset == fileSize)
             snprintf (text, (size_t) sizeof(text),
		       "EOF encountered.\n");
           else
             snprintf (text, (size_t) sizeof(text),
		       "Illegal record type detected (%ld)\n",
                       (long) recordType);
           WriteOut (OUTfp, text);
           return;
         }
       }
       WriteOut (OUTfp, text);
     } 
     /*************************************************************************
     * Based on the record type...
     *************************************************************************/
     switch (recordType) { 
        /**********************************************************************
        * Compressed CDF Record (CCR).
        **********************************************************************/
        case CCR_: {
          Byte1 byte; Int32 i;
          CCRcount++;
          CCRbytes += recordSize;
          fileSize = recordSize+24+8;
	  if (MOST(level)) {
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "CPRoffset: %s\n", FileOffset(int32));
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "uSize: %ld\n", (long) int32);
            WriteOut (OUTfp, text); 
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rfuA: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            nBytes = recordSize - CCR_BASE_SIZE;
            WriteOut (OUTfp, "Skipping compressed IRs...\n");
            for (i = 0; i < nBytes; i++) {
              if (!ReadBytes(CDFfp,&byte,(Int32)1)) return;
            }
	  }
          offset = offset + recordSize;
          if (!SeekTo(CDFfp,offset,vSEEK_SET)) return;
          usedBytes += recordSize;
          break;
        }
        /**********************************************************************
        * CDF Descriptor Record (CDR).
        **********************************************************************/
        case CDR_: {
          Int32 version, release, increment, myflag, lastUpdated;
          char *docText;
          CDRcount++;
          CDRbytes += recordSize;
	  if (MOST(level)) {
	    Logical allNULs = TRUE; int i;
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "GDRoffset: %s\n", FileOffset(int32));
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&version)) return;
            snprintf (text, (size_t) sizeof(text),
		      "Version: %ld\n", (long) version);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&release)) return;
            snprintf (text, (size_t) sizeof(text),
		      "Release: %ld\n", (long) release);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&encoding)) return;
            snprintf (text, (size_t) sizeof(text),
		      "Encoding: %ld\n", (long) encoding);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "Flags: 0x%lX (", (long) int32);
            myflag = int32;
            snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	              "%s,",
                      BOO(BITSET(int32,CDR_MAJORITY_BIT),"Row","Column"));
            snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	              "%s)\n",
                      BOO(BITSET(int32,CDR_FORMAT_BIT),"Single","Multi"));
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rfuA: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rfuB: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&increment)) return;
            snprintf (text, (size_t) sizeof(text),
		      "Increment: %ld\n", (long) increment);
            WriteOut (OUTfp, text);
            VDRwastedSpace = PriorTo ("2.5", (Int32) version, (Int32) release,
                                             (Int32) increment);
            if (!PriorTo ("3.2.0", (Int32) version, (Int32) release,
                                   (Int32) increment)) {
              if (BITSET(myflag,CDR_CHECKSUM_BIT)) {
                if (BITSET(myflag,CDR_CHECKSUM_MD5_BIT))
                  snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	                    "%s,", "MD5");
                else if (BITSET(myflag,CDR_CHECKSUM_OTHER_BIT))
                  snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	                    "%s,", "OTHER");
              } else
		checksum = 0;
            } else
	      checksum = 0;
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rfuD: %ld\n", (long) int32);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rfuE: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            nBytes = recordSize - CDR_BASE_SIZE;
            docText = (char *) cdf_AllocateMemory ((size_t)(nBytes + 1), FatalError);
            if (!ReadBytes(CDFfp,docText,nBytes)) {
              cdf_FreeMemory (docText, NULL);
              return;
            }
            docText[(int)nBytes] = NUL;
            WriteOut (OUTfp, "copyright...\n");
            WriteOut (OUTfp, docText);
            for (i = strlen(docText); i < nBytes; i++) {
               if (docText[i] != NUL) allNULs = FALSE;
            }
            if (allNULs) WriteOut (OUTfp, "...followed by all NULs.\n");
            cdf_FreeMemory (docText, FatalError);
	  }
          offset = offset + recordSize;
          if (!SeekTo(CDFfp,offset,vSEEK_SET)) return;
          usedBytes += recordSize;
          break;
        }
        /**********************************************************************
        * Global Descriptor Record (GDR).
        **********************************************************************/
        case GDR_: {
	  long offsetT = offset;
          GDRcount++;
          GDRbytes += recordSize;
	  if (MOST(level)) {
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rVDRhead: %s\n", FileOffset(int32));
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "zVDRhead: %s\n", FileOffset(int32));
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "ADRhead: %s\n", FileOffset(int32));
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            fileSize = int32;
            snprintf (text, (size_t) sizeof(text),
		      "eof: %s\n", FileOffset(int32));
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "NumVar: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "NumAttr: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rMaxRec: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&rNumDims)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rNumDims: %ld\n", (long) rNumDims);
            WriteOut (OUTfp, text); 
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "NumZvars: %ld\n", (long) int32);
            WriteOut (OUTfp, text); 
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "UIRhead: %s\n", FileOffset(int32));
            WriteOut (OUTfp, text); 
            if (!Read4(CDFfp,&int32)) return; 
            snprintf (text, (size_t) sizeof(text),
		      "rfuC: %ld\n", (long) int32);
            WriteOut (OUTfp, text); 
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rfuD: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rfuE: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            for (dimN = 0; dimN < rNumDims; dimN++) {
              if (!Read4(CDFfp,&int32)) return;
              snprintf (text, (size_t) sizeof(text),
		        " rDimSizes[%d]: %ld\n", dimN, (long) int32);
              WriteOut (OUTfp, text);
	    }
          } else {
            if (!SeekTo(CDFfp,offset+20,vSEEK_SET)) return;
            if (!Read4(CDFfp,&int32)) return;
            fileSize = int32;
	  }
          offset = offset + recordSize;
          if (!SeekTo(CDFfp,offset,vSEEK_SET)) return;
          usedBytes += recordSize;
          break;
        }
        /**********************************************************************
        * Attribute Descriptor Record (ADR).
        **********************************************************************/
        case ADR_: {
          char attrName[CDF_ATTR_NAME_LEN+1];
          ADRcount++;
          ADRbytes += recordSize;
	  if (MOST(level)) {
	    char delim;
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "ADRnext: %s\n", FileOffset(int32));
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "AgrEDRhead: %s\n", FileOffset(int32));
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "Scope: %ld\n", (long) int32);
            if ((long)int32 == GLOBAL_SCOPE) ++gADRcount;
            else ++vADRcount;
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "Num: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "NumRentries: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "MaxRentry: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rfuA: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "AzEDRhead: %s\n", FileOffset(int32));
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "NumZentries: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "MaxZentry: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rfuE: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!ReadBytes(CDFfp,attrName,(Int32)CDF_ATTR_NAME_LEN)) return;
            attrName[CDF_ATTR_NAME_LEN] = NUL;
            delim = PickDelimiter (attrName, strlen(attrName));
            snprintf (text, (size_t) sizeof(text),
		      "Name: %c%s%c\n", delim, attrName, delim);
            WriteOut (OUTfp, text);
	  }
          offset = offset + recordSize;
          if (!SeekTo(CDFfp,offset,vSEEK_SET)) return;
          usedBytes += recordSize;
          break;
        }
        /**********************************************************************
        * Attribute Entry Descriptor Record (AgrEDR or AzEDR).
        **********************************************************************/
        case AgrEDR_:
        case AzEDR_: {
          Logical zEntry = (recordType == AzEDR_);
          Int32 dataType, numElems; void *valuePtr;
          int style;
          if (zEntry) {
            AzEDRcount++;
            AzEDRbytes += recordSize;
          }
          else {
            AgrEDRcount++;
            AgrEDRbytes += recordSize;
          }
	  if (MOST(level)) {
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "A%sEDRnext: %s\n",
                      BOO(zEntry,"z","gr"), FileOffset(int32));
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "AttrNum: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&dataType)) return;
            snprintf (text, (size_t) sizeof(text),
		      "DataType: %ld (%s)\n",
                      (long) dataType, DataTypeToken(dataType));
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "Num: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&numElems)) return;
            snprintf (text, (size_t) sizeof(text),
		      "NumElems: %ld\n", (long) numElems);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rfuA: %ld\n", (long) int32);
            WriteOut (OUTfp, text); 
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rfuB: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rfuC: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rfuD: %ld\n", (long) int32);
            WriteOut (OUTfp, text); 
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rfuE: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            nBytes = CDFelemSize((long)dataType) * numElems;
            valuePtr = cdf_AllocateMemory ((size_t) nBytes, FatalError);
            if (!ReadBytes(CDFfp,valuePtr,nBytes)) {
              cdf_FreeMemory (valuePtr, FatalError);
              return;
            }
            ConvertBuffer ((long) encoding, HOST_DECODING, NEGtoPOSfp0off,
                           (long) dataType, (long) numElems, valuePtr,
                           valuePtr);
            strcpyX (text, "Value: ", MAX_SCREENLINE_LEN);
            if (TT2000dataType((long)dataType)) style = TT2000_3_STYLE;
            else style = EPOCH0_STYLE;
            EncodeValuesFormat ((long) dataType, (long) numElems, valuePtr,
                                EofS(text), NULL, 0,
                                MAX_SCREENLINE_LEN - strlen(text) - 1,
                                style,
                                (size_t) sizeof(text)-strlen(text));
            strcatX (text, "\n", MAX_SCREENLINE_LEN);
            WriteOut (OUTfp, text);
            cdf_FreeMemory (valuePtr, FatalError);
	  }
          offset = offset + recordSize;
          if (!SeekTo(CDFfp,offset,vSEEK_SET)) return;
          usedBytes += recordSize;
          break;
        }
        /**********************************************************************
        * Variable Descriptor Record (rVDR or zVDR).
        **********************************************************************/
        case rVDR_:
        case zVDR_: {
          Logical zVar = (recordType == zVDR_);
          Int32 dataType, varFlags, numElems, numDims;
          char varName[CDF_VAR_NAME_LEN+1]; void *padValue;
          if (zVar) {
            zVDRcount++;
            zVDRbytes += recordSize;
          }
          else {
            rVDRcount++;
            rVDRbytes += recordSize;
          }
	  if (MOST(level)) {
	    char delim;
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "%sVDRnext: %s\n", BOO(zVar,"z","r"),
                     FileOffset(int32));
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&dataType)) return;
            snprintf (text, (size_t) sizeof(text),
		      "DataType: %ld (%s)\n",
                     (long) dataType, DataTypeToken(dataType));
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "MaxRec: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "VXRhead: %s\n", FileOffset(int32));
            WriteOut (OUTfp, text);
            if (showIndexing && int32 != ZERO_OFFSET) {
              long savedOffset = V_tell (CDFfp);
              WriteOut (OUTfp, "\n");
              DumpVXR (CDFfp, int32, 5);
              WriteOut (OUTfp, "\n");
              if (!SeekTo(CDFfp,savedOffset,vSEEK_SET)) return;
	    }
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "VXRtail: %s\n", FileOffset(int32));
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&varFlags)) return;
            snprintf (text, (size_t) sizeof(text),
		      "Flags: 0x%lX (", (long) varFlags);
            snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	              "%sVARY,",
                      BOO(RECvaryBITset(varFlags),"","NO"));
            snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	              "%sPadValue,",
                      BOO(PADvalueBITset(varFlags),"","No"));
            snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	              "%sSparseArrays,",
                      BOO(SPARSEarraysBITset(varFlags),"","No"));
            snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	              "%sCompression)\n",
                      BOO(VARcompressionBITset(varFlags),"","No"));
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "sRecords: %ld (%s)\n", (long) int32,
                      BOO(int32==0, "No-Sparse Record",
                                   BOO(int32==1, "sRecords.PAD",
                                                 "sRecords.PREV")));
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rfuB: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rfuC: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rfuF: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (VDRwastedSpace) {
              Byte1 wastedSpace[VDR_WASTED_SIZE];
              if (!ReadBytes(CDFfp,wastedSpace,(Int32)VDR_WASTED_SIZE)) return;
              WriteOut (OUTfp, "Skipping over wasted space...\n");
	    }
            if (!Read4(CDFfp,&numElems)) return;
            snprintf (text, (size_t) sizeof(text),
		      "NumElems: %ld\n", (long) numElems);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "Num: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "CPRorSPRoffset: %s\n", FileOffset(int32));
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "BlockingFactor: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!ReadBytes(CDFfp,varName,(Int32)CDF_VAR_NAME_LEN)) return;
            varName[CDF_VAR_NAME_LEN] = NUL;
            delim = PickDelimiter (varName, strlen(varName));
            snprintf (text, (size_t) sizeof(text),
		      "Name: %c%s%c\n", delim, varName, delim);
            WriteOut (OUTfp, text);
            if (zVar) {
              if (!Read4(CDFfp,&numDims)) return;
              snprintf (text, (size_t) sizeof(text),
		        "zNumDims: %ld\n", (long) numDims);
              WriteOut (OUTfp, text);
              for (dimN = 0; dimN < numDims; dimN++) {
                if (!Read4(CDFfp,&int32)) return;
                snprintf (text, (size_t) sizeof(text),
		          " zDimSizes[%d]: %ld\n", dimN, (long) int32);
                WriteOut (OUTfp, text);
              }
            }
            else
              numDims = rNumDims;
            for (dimN = 0; dimN < numDims; dimN++) {
              if (!Read4(CDFfp,&int32)) return;
              snprintf (text, (size_t) sizeof(text),
		        " DimVarys[%d]: %ld (%s)\n", dimN, (long) int32,
                              BOO(int32==0, "F", "T"));
              WriteOut (OUTfp, text);
            }
            if (BITSET(varFlags,VDR_PADVALUE_BIT)) {
	      int i; int style;
              nBytes = CDFelemSize((long)dataType) * numElems;
              padValue = cdf_AllocateMemory ((size_t) nBytes, FatalError);
              if (!ReadBytes(CDFfp,padValue,nBytes)) {
                cdf_FreeMemory (padValue, FatalError);
                return;
              }
              strcpyX (text, "PadValue: ", MAX_SCREENLINE_LEN);
              strcatX (text, " (0x", MAX_SCREENLINE_LEN);
              for (i = (int) (nBytes - 1); i >= 0; i--) {
                 snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
			   "%02X", (int) ((Byte1 *)padValue)[i]);
              }
              strcatX (text, ") ", MAX_SCREENLINE_LEN);
              ConvertBuffer ((long) encoding, HOST_DECODING, NEGtoPOSfp0off,
                             (long) dataType, (long) numElems, padValue,
                             padValue);
              if (TT2000dataType((long)dataType)) style = TT2000_3_STYLE;
              else style = EPOCH0_STYLE;
              EncodeValuesFormat ((long) dataType, (long) numElems,
                                  padValue, EofS(text), NULL, 0,
                                  MAX_SCREENLINE_LEN - strlen(text) - 1,
                                  style,
                                  (size_t) sizeof(text)-strlen(text));
              strcatX (text, "\n", MAX_SCREENLINE_LEN);
              WriteOut (OUTfp, text);
              cdf_FreeMemory (padValue, FatalError);
	    }
          }
          offset = offset + recordSize;
          if (!SeekTo(CDFfp,offset,vSEEK_SET)) return;
          usedBytes += recordSize;
          break;
        }
        /**********************************************************************
        * Variable indeX Record (VXR).
        **********************************************************************/
        case VXR_: {
          Int32 nEntries, firstRec[MAX_VXR_ENTRIES]; int entryN;
          Int32 lastRec[MAX_VXR_ENTRIES], VVRoffset[MAX_VXR_ENTRIES];
          VXRcount++;
          VXRbytes += recordSize;
	  if (MOST(level)) {
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "VXRnext: %s\n", FileOffset(int32));
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&nEntries)) return;
            snprintf (text, (size_t) sizeof(text),
		      "Nentries: %ld\n", (long) nEntries);
            WriteOut (OUTfp, text);
            if (nEntries > MAX_VXR_ENTRIES) {
              WriteOut (OUTfp, "MAX_VXR_ENTRIES exceeded.");
              nEntries = MAX_VXR_ENTRIES;
            }      
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "NusedEntries: %ld\n", (long) int32);
            WriteOut (OUTfp, text); 
            if (!Read4s(CDFfp,firstRec,(int)nEntries)) return;
            if (!Read4s(CDFfp,lastRec,(int)nEntries)) return;
            if (!Read4s(CDFfp,VVRoffset,(int)nEntries)) return;
            WriteOut (OUTfp, "\n  Entry  FirstRec  LastRec      Offset\n");
            for (entryN = 0; entryN < nEntries; entryN++) {
               snprintf (text, (size_t) sizeof(text),
			 "     %2d   %7ld  %7ld  %s\n", entryN,
                         (long) firstRec[entryN], (long) lastRec[entryN],
                         FileOffset(VVRoffset[entryN]));
               WriteOut (OUTfp, text);
            }
	  }
          offset = offset + recordSize;
          if (!SeekTo(CDFfp,offset,vSEEK_SET)) return;
          usedBytes += recordSize;
          break;
        }
        /**********************************************************************
        * Variable Values Record (VVR).
        **********************************************************************/
        case VVR_: {
	  Int32 j;
          uByte value; 
          VVRcount++;
          VVRbytes += recordSize;
          nBytes = recordSize - VVR_BASE_SIZE;
          if (FULL(level) && !nodata) {
            for (j = 0; j < nBytes; j++) {
               if (j % BYTESperLINE == 0) WriteOut (OUTfp, "\n  ");
               if (!ReadBytes(CDFfp,&value,(Int32)1)) return;
               snprintf (text, (size_t) sizeof(text),
		         "%02X", (uInt4) value);
               WriteOut (OUTfp, text);
            }
            WriteOut (OUTfp, "\n");
          }
          else {
            offset = offset + recordSize;
            if (!SeekTo(CDFfp,offset,vSEEK_SET)) return;
          }
          usedBytes += recordSize;
          break;
        }
        /**********************************************************************
        * Compressed Variable Values Record (CVVR).
        **********************************************************************/
        case CVVR_: {
          uByte value; Int32 j;
          CVVRcount++;
          CVVRbytes += recordSize;
	  if (MOST(level)) {
            if (!Read4(CDFfp,&int32)) return;
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "cSize: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            nBytes = recordSize - CVVR_BASE_SIZE;
            if (FULL(level) && !nodata) { 
              for (j = 0; j < nBytes; j++) {
                if (j % BYTESperLINE == 0) WriteOut (OUTfp, "\n  ");
                if (!ReadBytes(CDFfp,&value,(Int32)1)) return;
                snprintf (text, (size_t) sizeof(text),
		          "%02X", (uInt4) value);
                WriteOut (OUTfp, text);
              }  
              WriteOut (OUTfp, "\n");
            }
          }    
          offset = offset + recordSize;
          if (!SeekTo(CDFfp,offset,vSEEK_SET)) return;
          usedBytes += recordSize;
          break;
        }   
        /**********************************************************************
        * Compression Parameters Record (CPR).
        **********************************************************************/
        case CPR_: {
          Int32 pCount; int parmN;
          CPRcount++;
          CPRbytes += recordSize;
	  if (MOST(level)) {
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "cType: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rfuA: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&pCount)) return;
            snprintf (text, (size_t) sizeof(text),
		      "pCount: %ld\n", (long) pCount);
            WriteOut (OUTfp, text);
            if (pCount > CDF_MAX_PARMS) {
              WriteOut (OUTfp, "\nCDF_MAX_PARMS exceeded.\n");
              pCount = CDF_MAX_PARMS;
            }
            for (parmN = 0; parmN < pCount; parmN++) {
              if (!Read4(CDFfp,&int32)) return;
              snprintf (text, (size_t) sizeof(text),
		        "  cParms[%d]: %ld\n", parmN, (long) int32);
              WriteOut (OUTfp, text);
	    }
          }
          offset = offset + recordSize;
          if (!SeekTo(CDFfp,offset,vSEEK_SET)) return;
          usedBytes += recordSize;
          break;
        }
        /**********************************************************************
        * Sparseness Parameters Record (SPR).
        **********************************************************************/
        case SPR_: {
          Int32 pCount; int parmN;
          SPRcount++;
          SPRbytes += recordSize;
	  if (MOST(level)) {
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "sArraysType: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&int32)) return;
            snprintf (text, (size_t) sizeof(text),
		      "rfuA: %ld\n", (long) int32);
            WriteOut (OUTfp, text);
            if (!Read4(CDFfp,&pCount)) return;
            snprintf (text, (size_t) sizeof(text),
		      "pCount: %ld\n", (long) pCount);
            WriteOut (OUTfp, text);
            if (pCount > CDF_MAX_PARMS) {
              WriteOut (OUTfp, "\nCDF_MAX_PARMS exceeded.\n");
              pCount = CDF_MAX_PARMS;
            }
            for (parmN = 0; parmN < pCount; parmN++) {
              if (!Read4(CDFfp,&int32)) return;
              snprintf (text, (size_t) sizeof(text),
		        "  sArraysParms[%d]: %ld\n",
                        parmN, (long) int32);
              WriteOut (OUTfp, text);
            } 
	  }   
          offset = offset + recordSize;
          if (!SeekTo(CDFfp,offset,vSEEK_SET)) return;
          usedBytes += recordSize;
          break;
        } 
        /**********************************************************************
        * Unused internal record
        **********************************************************************/
        case UIR_: { 
          UIRcount++;
          UIRbytes += recordSize;
          if (recordSize >= 16) {         /* Assumed a post-beta V2.5 CDF. */
	    if (MOST(level)) {
              if (!Read4(CDFfp,&int32)) return;
              snprintf (text, (size_t) sizeof(text),
		        "Next: %s\n", FileOffset(int32));
              WriteOut (OUTfp, text);
              if (!Read4(CDFfp,&int32)) return;
              snprintf (text, (size_t) sizeof(text),
		        "Prev: %s\n", FileOffset(int32));
              WriteOut (OUTfp, text);
              nBytes = recordSize - 16;
              if (nBytes > 0) {
                Byte1 byte; Int32 k;
                for (k = 0; k < nBytes; k++) {
                  if (!ReadBytes(CDFfp,&byte,(Int32)1)) return;
                }
              }
	    }
          }
          else {                /* Must be a beta V2.5 CDF (or earlies). */
            nBytes = recordSize - 8;
            if (nBytes > 0) {
              Byte1 byte; Int32 k;
              for (k = 0; k < nBytes; k++) {
                 if (!ReadBytes(CDFfp,&byte,(Int32)1)) return;
              }
            }
          }
          offset = offset + recordSize;
          if (!SeekTo(CDFfp,offset,vSEEK_SET)) return;
          wastedBytes += recordSize;
          break;
        }
        /**********************************************************************
        * Illegal record type.
        **********************************************************************/
        default: {
          if (fileSize == offset)
            snprintf (text, (size_t) sizeof(text),
		      "EOF encountered.\n");
          else
            snprintf (text, (size_t) sizeof(text),
		      "Illegal record type detected (%ld)\n",
                     (long) recordType);
          WriteOut (OUTfp, text);
          return;
        }
     }
     CHECKforABORTso
  }
}

/******************************************************************************
* ScanCDF64.
******************************************************************************/

static void ScanCDF64 (CDFfp, showIndexing, level, startingOffset, nodata)
vFILE *CDFfp;
Logical showIndexing;
long level;
OFF_T startingOffset;
Logical nodata;
{
  Int32 recordType, encoding, rNumDims;
  int dimN;
  Logical VDRwastedSpace;
  Int32 int32; char text[MAX_SCREENLINE_LEN+1];
  OFF_T int64;
  OFF_T recordSize, nBytes, offset;
  int style;
  Int32 version, release, increment;
  /****************************************************************************
  * Dump magic number(s).
  ****************************************************************************/
  WriteOut (OUTfp, "\nScanning records...\n\n");
  if (startingOffset == (OFF_T)NO_OFFSET64) {
    if (!SeekTo64(CDFfp,(OFF_T)0,vSEEK_SET)) return;
    if (!Read4_64(CDFfp,&int32)) return;
    if (MOST(level)) {
      snprintf (text, (size_t) sizeof(text),
		"Magic number (1): %s\n", Hex32(int32));
      WriteOut (OUTfp, text);
    }
    if (!Read4_64(CDFfp,&int32)) return;
    if (MOST(level)) {
      snprintf (text, (size_t) sizeof(text),
		"Magic number (2): %s\n", Hex32(int32));
      WriteOut (OUTfp, text);
    }
    usedBytes64 += 8;
  }
  else {
    if (!SeekTo64(CDFfp,startingOffset,vSEEK_SET)) return;
  }
  /****************************************************************************
  * Read and dump records until EOF (or illegal record) reached.
  ****************************************************************************/
  for (;;) {
     /*************************************************************************
     * Read record size.
     *************************************************************************/
     offset = V_tell64 (CDFfp);
     EOFok = TRUE;
     if (offset == fileSize64) return;
     if (!Read8(CDFfp,&recordSize)) return;
     EOFok = FALSE;
     if (recordSize < (OFF_T) 1) {
#if !defined(win32) && !defined(__MINGW32__)
       snprintf (text, (size_t) sizeof(text),
		 "\nIllegal record size detected (%lld) (@%s)\n", 
		 (long long) recordSize, FileOffset64(offset));
#else
       snprintf (text, (size_t) sizeof(text),
		 "\nIllegal record size detected (%I64d) (@%s)\n",
                 (long long) recordSize, FileOffset64(offset));
#endif
       WriteOut (OUTfp, text);
       snprintf (text, (size_t) sizeof(text),
		 "     (May have reached the end of the valid data.)\n");
       WriteOut (OUTfp, text);
       return;
     }
     if (MOST(level)) {
#if !defined(win32) && !defined(__MINGW32__)
       snprintf (text, (size_t) sizeof(text),
		 "\nRecordSize: %lld (@ %s)\n",
		 (long long) recordSize, FileOffset64(offset));
#else
       snprintf (text, (size_t) sizeof(text),
		 "\nRecordSize: %I64d (@ %s)\n",
                 (long long) recordSize, FileOffset64(offset));
#endif
       WriteOut (OUTfp, text);
     }
     /*************************************************************************
     * Read record type.
     *************************************************************************/
     if (!Read4_64(CDFfp,&recordType)) return;
     if (MOST(level)) {
       snprintf (text, (size_t) sizeof(text),
		 "RecordType: %ld ", (long) recordType);
       switch (recordType) {
         case CCR_: strcatX (text, "(CCR)\n", MAX_SCREENLINE_LEN); break;
         case CDR_: strcatX (text, "(CDR)\n", MAX_SCREENLINE_LEN); break;
         case GDR_: strcatX (text, "(GDR)\n", MAX_SCREENLINE_LEN); break;
         case ADR_: strcatX (text, "(ADR)\n", MAX_SCREENLINE_LEN); break;
         case AgrEDR_: strcatX (text, "(AgrEDR)\n", MAX_SCREENLINE_LEN); break;
         case AzEDR_: strcatX (text, "(AzEDR)\n", MAX_SCREENLINE_LEN); break;
         case rVDR_: strcatX (text, "(rVDR)\n", MAX_SCREENLINE_LEN); break;
         case zVDR_: strcatX (text, "(zVDR)\n", MAX_SCREENLINE_LEN); break;
         case VXR_: strcatX (text, "(VXR)\n", MAX_SCREENLINE_LEN); break;
         case VVR_: strcatX (text, "(VVR)\n", MAX_SCREENLINE_LEN); break;
         case CVVR_: strcatX (text, "(CVVR)\n", MAX_SCREENLINE_LEN); break;
         case CPR_: strcatX (text, "(CPR)\n", MAX_SCREENLINE_LEN); break;
         case SPR_: strcatX (text, "(SPR)\n", MAX_SCREENLINE_LEN); break;
         case UIR_: strcatX (text, "(UIR)\n", MAX_SCREENLINE_LEN); break;
         default: {
	   if (offset == fileSize64) 
	     snprintf (text, (size_t) sizeof(text),
		       "EOF encountered.\n");
	   else {
	     snprintf (text, (size_t) sizeof(text),
		       "Illegal record type detected (%ld) (@ %s)\n",
		       (long) recordType, FileOffset64(offset));
	     WriteOut (OUTfp, text);
             snprintf (text, (size_t) sizeof(text),
                       "     (May have reached the end of the valid data.)\n");
           }  
	   WriteOut (OUTfp, text);
	   return;
         }
       }
       WriteOut (OUTfp, text);
     }
     /*************************************************************************
     * Based on the record type...
     *************************************************************************/
     switch (recordType) {
        /**********************************************************************
        * Compressed CDF Record (CCR).
        **********************************************************************/
        case CCR_: {
	  CCRcount++;
	  CCRbytes64 += recordSize;
          fileSize64 = recordSize+28+8;
	  if (MOST(level)) {
	    if (!Read8(CDFfp,&int64)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "CPRoffset: %s\n", FileOffset64(int64));
	    WriteOut (OUTfp, text);
	    if (!Read8(CDFfp,&int64)) return;
#if !defined(win32) && !defined(__MINGW32__)
	    snprintf (text, (size_t) sizeof(text),
		      "uSize: %lld\n", (long long) int64);
#else
            snprintf (text, (size_t) sizeof(text),
		      "uSize: %I64d\n", (long long) int64);
#endif
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rfuA: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    nBytes = recordSize - CCR_BASE_SIZE64;
	    WriteOut (OUTfp, "Skipping compressed IRs...\n");
	  }
	  usedBytes64 += recordSize;
          offset = offset + recordSize;
          if (!SeekTo64(CDFfp,offset,vSEEK_SET)) return;
	  break;
        }
        /**********************************************************************
        * CDF Descriptor Record (CDR).
        **********************************************************************/
        case CDR_: {
	  char *docText;
	  CDRcount++;
	  CDRbytes64 += recordSize;
	  if (MOST(level)) {
	    Logical allNULs = TRUE; int i;
	    if (!Read8(CDFfp,&int64)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "GDRoffset: %s\n", FileOffset64(int64));
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&version)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "Version: %ld\n", (long) version);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&release)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "Release: %ld\n", (long) release);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&encoding)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "Encoding: %ld\n", (long) encoding);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "Flags: 0x%lX (", (long) int32);
	    /* increment is not yet available.... but doesn't matter. */
	    increment = (Int32) 0;
	    if (!PriorTo ("3.2.0", (Int32) version, (Int32) release,
                                   (Int32) increment)) {
	      if (BITSET(int32,CDR_CHECKSUM_BIT)) {
	        if (BITSET(int32,CDR_CHECKSUM_MD5_BIT))
	          snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
		            "%s,", "MD5");
	        else if (BITSET(int32,CDR_CHECKSUM_OTHER_BIT))
	          snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
			    "%s,", "OTHER");
	      } else
		checksum = 0;
	    } else
	      checksum = 0;
	    snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
		      "%s,",
		      BOO(BITSET(int32,CDR_MAJORITY_BIT),"Row","Column"));
	    snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	              "%s)\n",
		      BOO(BITSET(int32,CDR_FORMAT_BIT),"Single","Multi"));
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rfuA: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rfuB: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&increment)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "Increment: %ld\n", (long) increment);
	    WriteOut (OUTfp, text);
	    VDRwastedSpace = PriorTo ("2.5", version, release, increment);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rfuD: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rfuE: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    nBytes = recordSize - CDR_BASE_SIZE64;
	    docText = (char *) cdf_AllocateMemory ((size_t)(nBytes + 1), FatalError);
	    if (!ReadBytes64(CDFfp,docText,(Int32)nBytes)) {
	      cdf_FreeMemory (docText, NULL);
	      return;
	    }
	    docText[(int)nBytes] = NUL;
	    WriteOut (OUTfp, "copyright...\n");
	    WriteOut (OUTfp, docText);
	    for (i = strlen(docText); i < nBytes; i++) {
	       if (docText[i] != NUL) allNULs = FALSE;
	    }
	    if (allNULs) WriteOut (OUTfp, "...followed by all NULs.\n");
	    cdf_FreeMemory (docText, FatalError);
	  }
          offset = offset + recordSize;
          if (!SeekTo64(CDFfp,offset,vSEEK_SET)) return;
	  usedBytes64 += recordSize;
	  break;
        }
        /**********************************************************************
        * Global Descriptor Record (GDR).
        **********************************************************************/
        case GDR_: {
	  GDRcount++;
	  GDRbytes64 += recordSize;
	  if (MOST(level)) {
	    if (!Read8(CDFfp,&int64)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rVDRhead: %s\n", FileOffset64(int64));
	    WriteOut (OUTfp, text);
	    if (!Read8(CDFfp,&int64)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "zVDRhead: %s\n", FileOffset64(int64));
	    WriteOut (OUTfp, text);
	    if (!Read8(CDFfp,&int64)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "ADRhead: %s\n", FileOffset64(int64));
	    WriteOut (OUTfp, text);
	    if (!Read8(CDFfp,&int64)) return;
	    fileSize64 = int64;
	    snprintf (text, (size_t) sizeof(text),
		      "eof: %s\n", FileOffset64(int64));
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "NumVar: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "NumAttr: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rMaxRec: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&rNumDims)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rNumDims: %ld\n", (long) rNumDims);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "NumZvars: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read8(CDFfp,&int64)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "UIRhead: %s\n", FileOffset64(int64));
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rfuC: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
/*
            if (!PriorTo("3.6.0", (Int32) version, (Int32) release,
                         (Int32) increment))
*/
              snprintf (text, (size_t) sizeof(text),
                        "LeapSecondLastUpdated: %ld\n", (long) int32);
/*
            else
              snprintf (text, (size_t) sizeof(text),
                        "rfuD: %ld\n", (long) int32);
*/
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rfuE: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    for (dimN = 0; dimN < rNumDims; dimN++) {
	      if (!Read4_64(CDFfp,&int32)) return;
	      snprintf (text, (size_t) sizeof(text),
		        " rDimSizes[%d]: %ld\n", dimN, (long) int32);
	      WriteOut (OUTfp, text);
	    }
	  } else {
            if (!SeekTo64(CDFfp,offset+36,vSEEK_SET)) return;
            if (!Read8(CDFfp,&int64)) return;
            fileSize64 = int64;
	  }
          offset = offset + recordSize;
          if (!SeekTo64(CDFfp,offset,vSEEK_SET)) return;
	  usedBytes64 += recordSize;
	  break;
        }
        /**********************************************************************
        * Attribute Descriptor Record (ADR).
        **********************************************************************/
        case ADR_: {
	  char attrName[CDF_ATTR_NAME_LEN256+1];
	  ADRcount++;
	  ADRbytes64 += recordSize;
	  if (MOST(level)) {
	    char delim;
	    if (!Read8(CDFfp,&int64)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "ADRnext: %s\n", FileOffset64(int64));
	    WriteOut (OUTfp, text);
	    if (!Read8(CDFfp,&int64)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "AgrEDRhead: %s\n", FileOffset64(int64));
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "Scope: %ld\n", (long) int32);
            if ((long)int32 == GLOBAL_SCOPE) ++gADRcount;
            else ++vADRcount;
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "Num: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "NumRentries: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "MaxRentry: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rfuA: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read8(CDFfp,&int64)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "AzEDRhead: %s\n", FileOffset64(int64));
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "NumZentries: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "MaxZentry: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rfuE: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!ReadBytes64(CDFfp,attrName,(Int32)CDF_ATTR_NAME_LEN256)) return;
	    attrName[CDF_ATTR_NAME_LEN256] = NUL;
	    delim = PickDelimiter (attrName, strlen(attrName));
	    snprintf (text, (size_t) sizeof(text),
		      "Name: %c%s%c\n", delim, attrName, delim);
	    WriteOut (OUTfp, text);
	  }
          offset = offset + recordSize;
          if (!SeekTo64(CDFfp,offset,vSEEK_SET)) return;
	  usedBytes64 += recordSize;
	  break;
        }
        /**********************************************************************
        * Attribute Entry Descriptor Record (AgrEDR or AzEDR).
        **********************************************************************/
        case AgrEDR_:
        case AzEDR_: {
	  Logical zEntry = (recordType == AzEDR_);
	  Int32 dataType, numElems; void *valuePtr;
	  if (zEntry) {
	    AzEDRcount++;
	    AzEDRbytes64 += recordSize;
	  }
	  else {
	    AgrEDRcount++;
	    AgrEDRbytes64 += recordSize;
	  }
	  if (MOST(level)) {
	    if (!Read8(CDFfp,&int64)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "A%sEDRnext: %s\n",
		      BOO(zEntry,"z","gr"), FileOffset64(int64));
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "AttrNum: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&dataType)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "DataType: %ld (%s)\n",
		      (long) dataType, DataTypeToken(dataType));
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "Num: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&numElems)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "NumElems: %ld\n", (long) numElems);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rfuA: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rfuB: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rfuC: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rfuD: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rfuE: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    nBytes = CDFelemSize((long)dataType) * numElems;
	    valuePtr = cdf_AllocateMemory ((size_t) nBytes, FatalError);
	    if (!ReadBytes64(CDFfp,valuePtr,(Int32)nBytes)) {
	      cdf_FreeMemory (valuePtr, FatalError);
	      return;
	    }
	    ConvertBuffer ((long) encoding, HOST_DECODING, NEGtoPOSfp0off,
			   (long) dataType, (long) numElems, valuePtr,
			   valuePtr);
	    strcpyX (text, "Value: ", MAX_SCREENLINE_LEN);
            if (TT2000dataType((long)dataType)) style = TT2000_3_STYLE;
            else style = EPOCH0_STYLE;
	    EncodeValuesFormat ((long) dataType, (long) numElems, valuePtr,
			        EofS(text), NULL, 0,
			        MAX_SCREENLINE_LEN - strlen(text) - 1,
				style,
                                (size_t) sizeof(text)-strlen(text));
	    strcatX (text, "\n", MAX_SCREENLINE_LEN);
	    WriteOut (OUTfp, text);
	    cdf_FreeMemory (valuePtr, FatalError);
	  }
          offset = offset + recordSize;
          if (!SeekTo64(CDFfp,offset,vSEEK_SET)) return;
	  usedBytes64 += recordSize;
	  break;
        }
        /**********************************************************************
        * Variable Descriptor Record (rVDR or zVDR).
        **********************************************************************/
        case rVDR_:
        case zVDR_: {
	  Logical zVar = (recordType == zVDR_);
	  Int32 dataType, varFlags, numElems, numDims;
	  char varName[CDF_VAR_NAME_LEN256+1]; void *padValue;
	  if (zVar) {
	    zVDRcount++;
	    zVDRbytes64 += recordSize;
	  }
	  else {
	    rVDRcount++;
	    rVDRbytes64 += recordSize;
	  }
	  if (MOST(level)) {
	    char delim;
	    if (!Read8(CDFfp,&int64)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "%sVDRnext: %s\n", BOO(zVar,"z","r"),
		      FileOffset64(int64));
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&dataType)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "DataType: %ld (%s)\n",
		      (long) dataType, DataTypeToken(dataType));
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "MaxRec: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read8(CDFfp,&int64)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "VXRhead: %s\n", FileOffset64(int64));
	    WriteOut (OUTfp, text);
	    if (showIndexing && int32 != ZERO_OFFSET) {
	      long savedOffset = V_tell (CDFfp);
	      WriteOut (OUTfp, "\n");
	      DumpVXR (CDFfp, int32, 5);
	      WriteOut (OUTfp, "\n");
	      if (!SeekTo(CDFfp,savedOffset,vSEEK_SET)) return;
	    }
	    if (!Read8(CDFfp,&int64)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "VXRtail: %s\n", FileOffset64(int64));
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&varFlags)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "Flags: 0x%lX (", (long) varFlags);
	    snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	              "%sVARY,",
		      BOO(RECvaryBITset(varFlags),"","NO"));
	    snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	              "%sPadValue,",
		      BOO(PADvalueBITset(varFlags),"","No"));
	    snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	              "%sSparseArrays,",
		      BOO(SPARSEarraysBITset(varFlags),"","No"));
	    snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	              "%sCompression)\n",
		      BOO(VARcompressionBITset(varFlags),"","No"));
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "sRecords: %ld (%s)\n", (long) int32,
	              BOO(int32==0, "No-Sparse Record",
	                           BOO(int32==1, "sRecords.PAD",
	                                         "sRecords.PREV")));
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rfuB: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rfuC: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rfuF: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (VDRwastedSpace) {
	      Byte1 wastedSpace[VDR_WASTED_SIZE64];
	      if (!ReadBytes64(CDFfp,wastedSpace,(Int32)VDR_WASTED_SIZE64)) return;
	      WriteOut (OUTfp, "Skipping over wasted space...\n");
	    }
	    if (!Read4_64(CDFfp,&numElems)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "NumElems: %ld\n", (long) numElems);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "Num: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read8(CDFfp,&int64)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "CPRorSPRoffset: %s\n", FileOffset64(int64));
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "BlockingFactor: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!ReadBytes64(CDFfp,varName,(Int32)CDF_VAR_NAME_LEN256)) return;
	    varName[CDF_VAR_NAME_LEN256] = NUL;
	    delim = PickDelimiter (varName, strlen(varName));
	    snprintf (text, (size_t) sizeof(text),
		      "Name: %c%s%c\n", delim, varName, delim);
	    WriteOut (OUTfp, text);
	    if (zVar) {
	      if (!Read4_64(CDFfp,&numDims)) return;
	      snprintf (text, (size_t) sizeof(text),
		        "zNumDims: %ld\n", (long) numDims);
	      WriteOut (OUTfp, text);
	      for (dimN = 0; dimN < numDims; dimN++) {
	        if (!Read4_64(CDFfp,&int32)) return;
	        snprintf (text, (size_t) sizeof(text),
		          " zDimSizes[%d]: %ld\n", dimN, (long) int32);
	        WriteOut (OUTfp, text);
	      }
	    }
	    else
	      numDims = rNumDims;
	    for (dimN = 0; dimN < numDims; dimN++) {
	      if (!Read4_64(CDFfp,&int32)) return;
	      snprintf (text, (size_t) sizeof(text),
		        " DimVarys[%d]: %ld (%s)\n", dimN, (long) int32,
	                      BOO(int32==0, "F", "T"));
	      WriteOut (OUTfp, text);
	    }
	    if (BITSET(varFlags,VDR_PADVALUE_BIT)) {
	      int i;
	      nBytes = CDFelemSize((long)dataType) * numElems;
	      padValue = cdf_AllocateMemory ((size_t) nBytes, FatalError);
	      if (!ReadBytes64(CDFfp,padValue,(Int32)nBytes)) {
	        cdf_FreeMemory (padValue, FatalError);
	        return;
	      }
	      strcpyX (text, "PadValue: ", MAX_SCREENLINE_LEN);
	      if (dataType != CDF_EPOCH16) {
	        strcatX (text, " (0x", MAX_SCREENLINE_LEN);
	        for (i = (int) (nBytes - 1); i >= 0; i--) {
	          snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
		  	    "%02X", (int) ((Byte1 *)padValue)[i]);
	        }
	        strcatX (text, ") ", MAX_SCREENLINE_LEN);
	      }
	      ConvertBuffer ((long) encoding, HOST_DECODING, NEGtoPOSfp0off,
			     (long) dataType, (long) numElems, padValue,
			     padValue);
              if (TT2000dataType((long)dataType)) style = TT2000_3_STYLE;
              else style = EPOCH0_STYLE;
	      EncodeValuesFormat ((long) dataType, (long) numElems,
				  padValue, EofS(text), NULL, 0,
				  MAX_SCREENLINE_LEN - strlen(text) - 1,
				  style,
                                  (size_t) sizeof(text)-strlen(text));
	      strcatX (text, "\n", MAX_SCREENLINE_LEN);
	      WriteOut (OUTfp, text);
	      cdf_FreeMemory (padValue, FatalError);
	    }
	  }
          offset = offset + recordSize;
          if (!SeekTo64(CDFfp,offset,vSEEK_SET)) return;
	  usedBytes64 += recordSize;
	  break;
        }
        /**********************************************************************
        * Variable indeX Record (VXR).
        **********************************************************************/
        case VXR_: {
	  Int32 nEntries, firstRec[MAX_VXR_ENTRIES]; int entryN;
	  Int32 lastRec[MAX_VXR_ENTRIES]; OFF_T VVRoffset[MAX_VXR_ENTRIES];
	  VXRcount++;
	  VXRbytes64 += recordSize;
	  if (MOST(level)) {
	    if (!Read8(CDFfp,&int64)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "VXRnext: %s\n", FileOffset64(int64));
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&nEntries)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "Nentries: %ld\n", (long) nEntries);
	    WriteOut (OUTfp, text);
	    if (nEntries > MAX_VXR_ENTRIES) {
	      WriteOut (OUTfp, "MAX_VXR_ENTRIES exceeded.");
	      nEntries = MAX_VXR_ENTRIES;
	    }
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "NusedEntries: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4s_64(CDFfp,firstRec,(int)nEntries)) return;
	    if (!Read4s_64(CDFfp,lastRec,(int)nEntries)) return;
	    if (!Read8s(CDFfp,VVRoffset,(int)nEntries)) return;
	    WriteOut (OUTfp, "\n  Entry  FirstRec  LastRec           Offset\n");
	    for (entryN = 0; entryN < nEntries; entryN++) {
	       snprintf (text, (size_t) sizeof(text),
		         "     %2d   %7ld  %7ld      %s\n", entryN,
		         (long) firstRec[entryN], (long) lastRec[entryN],
		         FileOffset64(VVRoffset[entryN]));
	       WriteOut (OUTfp, text);
	    }
	  }
          offset = offset + recordSize;
          if (!SeekTo64(CDFfp,offset,vSEEK_SET)) return;
	  usedBytes64 += recordSize;
	  break;
        }
        /**********************************************************************
        * Variable Values Record (VVR).
        **********************************************************************/
        case VVR_: {
	  uByte value; Int32 j;
	  VVRcount++;
	  VVRbytes64 += recordSize;
	  nBytes = recordSize - VVR_BASE_SIZE64;
	  if (FULL(level) && !nodata) {
	    for (j = 0; j < nBytes; j++) {
	       if (j % BYTESperLINE == 0) WriteOut (OUTfp, "\n  ");
	       if (!ReadBytes64(CDFfp,&value,(Int32)1)) return;
	       snprintf (text, (size_t) sizeof(text),
		         "%02X", (uInt4) value);
	       WriteOut (OUTfp, text);
	    }
	    WriteOut (OUTfp, "\n");
	  }
          offset = offset + recordSize;
          if (!SeekTo64(CDFfp,offset,vSEEK_SET)) return;
	  usedBytes64 += recordSize;
	  break;
        }
        /**********************************************************************
        * Compressed Variable Values Record (CVVR).
        **********************************************************************/
        case CVVR_: {
	  uByte value; Int32 j;
	  CVVRcount++;
	  CVVRbytes64 += recordSize;
	  if (MOST(level)) {
	    if (!Read4_64(CDFfp,&int32)) return;
	    if (!Read8(CDFfp,&int64)) return;
#if !defined(win32) && !defined(__MINGW32__)
              snprintf (text, (size_t) sizeof(text),
                        "cSize: %lld\n", (long long) int64);
#else
              snprintf (text, (size_t) sizeof(text),
                       "cSize: %I64d\n", (long long) int64);
#endif
	    WriteOut (OUTfp, text);
	    nBytes = recordSize - CVVR_BASE_SIZE64;
	    if (FULL(level) && !nodata) {
	      OFF_T tmp = nBytes;
	      j = 0;
	      while (tmp > 0) {
	        if (j % BYTESperLINE == 0) WriteOut (OUTfp, "\n  ");
	        if (!ReadBytes64(CDFfp,&value,(Int32)1)) return;
	        snprintf (text, (size_t) sizeof(text),
		          "%02X", (uInt4) value);
	        WriteOut (OUTfp, text);
	        tmp--;
	        j++;
	      }
	    }
	    WriteOut (OUTfp, "\n");
	  }
          offset = offset + recordSize;
          if (!SeekTo64(CDFfp,offset,vSEEK_SET)) return;
	  usedBytes64 += recordSize;
	  break;
        }
        /**********************************************************************
        * Compression Parameters Record (CPR).
        **********************************************************************/
        case CPR_: {
	  Int32 pCount; int parmN;
	  CPRcount++;
	  CPRbytes64 += recordSize;
	  if (MOST(level)) {
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "cType: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rfuA: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&pCount)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "pCount: %ld\n", (long) pCount);
	    WriteOut (OUTfp, text);
	    if (pCount > CDF_MAX_PARMS) {
	      WriteOut (OUTfp, "\nCDF_MAX_PARMS exceeded.\n");
	      pCount = CDF_MAX_PARMS;
	    }
	    for (parmN = 0; parmN < pCount; parmN++) {
	      if (!Read4_64(CDFfp,&int32)) return;
	      snprintf (text, (size_t) sizeof(text),
		        "  cParms[%d]: %ld\n", parmN, (long) int32);
	      WriteOut (OUTfp, text);
	    }
	  }
          offset = offset + recordSize;
          if (!SeekTo64(CDFfp,offset,vSEEK_SET)) return;
	  usedBytes64 += recordSize;
	  break;
        }
        /**********************************************************************
        * Sparseness Parameters Record (SPR).
        **********************************************************************/
        case SPR_: {
	  Int32 pCount; int parmN;
	  SPRcount++;
	  SPRbytes64 += recordSize;
	  if (MOST(level)) {
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "sArraysType: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&int32)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "rfuA: %ld\n", (long) int32);
	    WriteOut (OUTfp, text);
	    if (!Read4_64(CDFfp,&pCount)) return;
	    snprintf (text, (size_t) sizeof(text),
		      "pCount: %ld\n", (long) pCount);
	    WriteOut (OUTfp, text);
	    if (pCount > CDF_MAX_PARMS) {
	      WriteOut (OUTfp, "\nCDF_MAX_PARMS exceeded.\n");
	      pCount = CDF_MAX_PARMS;
	    }
	    for (parmN = 0; parmN < pCount; parmN++) {
	      if (!Read4_64(CDFfp,&int32)) return;
	      snprintf (text, (size_t) sizeof(text),
		        "  sArraysParms[%d]: %ld\n",
		        parmN, (long) int32);
	      WriteOut (OUTfp, text);
	    }
	  }
          offset = offset + recordSize;
          if (!SeekTo64(CDFfp,offset,vSEEK_SET)) return;
	  usedBytes64 += recordSize;
	  break;
        }
        /**********************************************************************
        * Unused internal record
        **********************************************************************/
        case UIR_: {
	  UIRcount++;
	  UIRbytes64 += recordSize;
	  if (recordSize >= 28) {         /* Assumed a post-beta V2.5 CDF. */
	    if (MOST(level)) {
	      if (!Read8(CDFfp,&int64)) return;
	      snprintf (text, (size_t) sizeof(text),
		        "Next: %s\n", FileOffset64(int64));
	      WriteOut (OUTfp, text);
	      if (!Read8(CDFfp,&int64)) return;
	      snprintf (text, (size_t) sizeof(text),
		        "Prev: %s\n", FileOffset64(int64));
	      WriteOut (OUTfp, text);
	    }
	    nBytes = recordSize - 28;
	    if (nBytes > 0) {
	      Byte1 byte; 
	      OFF_T tmp = nBytes;
	      while (tmp > 0) {
		 if (!ReadBytes64(CDFfp,&byte,(Int32)1)) return;
		 tmp--;
	      }
	    }
	  }
          offset = offset + recordSize;
          if (!SeekTo64(CDFfp,offset,vSEEK_SET)) return;
	  wastedBytes64 += recordSize;
	  break;
        }
        /**********************************************************************
        * Illegal record type.
        **********************************************************************/
        default: {
	  if (fileSize64 == offset) 
	    snprintf (text, (size_t) sizeof(text),
		      "EOF encountered.\n");
	  else {
	    snprintf (text, (size_t) sizeof(text),
		      "Illegal record type detected (%ld) (@ %s)\n",
		     (long) recordType, FileOffset64(offset));
	    WriteOut (OUTfp, text);
	    snprintf (text, (size_t) sizeof(text),
                      "     (May have reached the end of the valid data.)\n");
          }
	  WriteOut (OUTfp, text);
	  return;
        }
     }
     CHECKforABORTso
  }
}

/******************************************************************************
* DisplaySummary.
******************************************************************************/

static void DisplaySummary (long level) {
  char text[132+1]; int width1, width2; long largestBytes;
  long IRcount = CCRcount + CDRcount + GDRcount + ADRcount + AgrEDRcount +
		 AzEDRcount + rVDRcount + zVDRcount + VXRcount + VVRcount +
		 CVVRcount + CPRcount + SPRcount + UIRcount;
  long totalBytes = usedBytes + wastedBytes;
  double pctUsed = 100.0 * (((double) usedBytes) / ((double) totalBytes));
  double pctWasted = 100.0 * (((double) wastedBytes) / ((double) totalBytes));
  double pctCCR = 100.0 * (((double) CCRbytes) / ((double) totalBytes));
  double pctCDR = 100.0 * (((double) CDRbytes) / ((double) totalBytes));
  double pctGDR = 100.0 * (((double) GDRbytes) / ((double) totalBytes));
  double pctADR = 100.0 * (((double) ADRbytes) / ((double) totalBytes));
  double pctAgrEDR = 100.0 * (((double) AgrEDRbytes) / ((double) totalBytes));
  double pctAzEDR = 100.0 * (((double) AzEDRbytes) / ((double) totalBytes));
  double pct_rVDR = 100.0 * (((double) rVDRbytes) / ((double) totalBytes));
  double pct_zVDR = 100.0 * (((double) zVDRbytes) / ((double) totalBytes));
  double pctVXR = 100.0 * (((double) VXRbytes) / ((double) totalBytes));
  double pctVVR = 100.0 * (((double) VVRbytes) / ((double) totalBytes));
  double pctCVVR = 100.0 * (((double) CVVRbytes) / ((double) totalBytes));
  double pctCPR = 100.0 * (((double) CPRbytes) / ((double) totalBytes));
  double pctSPR = 100.0 * (((double) SPRbytes) / ((double) totalBytes));
  double pctUIR = 100.0 * (((double) UIRbytes) / ((double) totalBytes));
  width1 = IntWidth ((int)totalBytes);
  largestBytes = 0;
  largestBytes = MAXIMUM(largestBytes,CCRbytes);
  largestBytes = MAXIMUM(largestBytes,CDRbytes);
  largestBytes = MAXIMUM(largestBytes,GDRbytes);
  largestBytes = MAXIMUM(largestBytes,ADRbytes);
  largestBytes = MAXIMUM(largestBytes,AgrEDRbytes);
  largestBytes = MAXIMUM(largestBytes,AzEDRbytes);
  largestBytes = MAXIMUM(largestBytes,rVDRbytes);
  largestBytes = MAXIMUM(largestBytes,zVDRbytes);
  largestBytes = MAXIMUM(largestBytes,VXRbytes);
  largestBytes = MAXIMUM(largestBytes,VVRbytes);
  largestBytes = MAXIMUM(largestBytes,CVVRbytes);
  largestBytes = MAXIMUM(largestBytes,CPRbytes);
  largestBytes = MAXIMUM(largestBytes,SPRbytes);
  largestBytes = MAXIMUM(largestBytes,UIRbytes);
  width2 = IntWidth ((int)largestBytes);
  WriteOut (OUTfp, "\n\nSummary...");
  snprintf (text, (size_t) sizeof(text),
	    "\n\n  Total bytes: %*ld", width1, totalBytes);
#if defined(vms)
  snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	    ", %ld blocks\n", (long) (((totalBytes-1)/512)+1));
#else
  strcatX (text, "\n", 132);
#endif
  WriteOut (OUTfp, text);
  snprintf (text, (size_t) sizeof(text),
	    "   Used bytes: %*ld, %7.3f%%\n",
	    width1, usedBytes, pctUsed);
  WriteOut (OUTfp, text);
  snprintf (text, (size_t) sizeof(text),
	    " Unused bytes: %*ld, %7.3f%%\n\n",
	    width1, wastedBytes, pctWasted);
  WriteOut (OUTfp, text);
  snprintf (text, (size_t) sizeof(text),
	    "     IR count: %*ld\n\n",
	    width1, IRcount);
  WriteOut (OUTfp, text);
  snprintf (text, (size_t) sizeof(text),
	    "    CCR count: %*ld, %*ld bytes,  %6.3f%%\n",
	    width1, CCRcount, width2, CCRbytes, pctCCR);
  WriteOut (OUTfp, text);
  snprintf (text, (size_t) sizeof(text),
	    "    CDR count: %*ld, %*ld bytes,  %6.3f%%\n",
	    width1, CDRcount, width2, CDRbytes, pctCDR);
  WriteOut (OUTfp, text);
  snprintf (text, (size_t) sizeof(text),
	    "    GDR count: %*ld, %*ld bytes,  %6.3f%%\n",
	    width1, GDRcount, width2, GDRbytes, pctGDR);
  WriteOut (OUTfp, text);
  if (!MOST(level))
    snprintf (text, (size_t) sizeof(text),
 	      "    ADR count: %*ld, %*ld bytes,  %6.3f%%\n",
	      width1, ADRcount, width2, ADRbytes, pctADR);
  else
    snprintf (text, (size_t) sizeof(text),
 	      "    ADR count: %*ld, %*ld bytes,  %6.3f%% (G:%ld V:%ld)\n",
	      width1, ADRcount, width2, ADRbytes, pctADR, gADRcount, vADRcount);
  WriteOut (OUTfp, text);
  snprintf (text, (size_t) sizeof(text),
	    " AgrEDR count: %*ld, %*ld bytes,  %6.3f%%\n",
	    width1, AgrEDRcount, width2, AgrEDRbytes, pctAgrEDR);
  WriteOut (OUTfp, text);
  snprintf (text, (size_t) sizeof(text),
	    "  AzEDR count: %*ld, %*ld bytes,  %6.3f%%\n",
	    width1, AzEDRcount, width2, AzEDRbytes, pctAzEDR);
  WriteOut (OUTfp, text);
  snprintf (text, (size_t) sizeof(text),
	    "   rVDR count: %*ld, %*ld bytes,  %6.3f%%\n",
	    width1, rVDRcount, width2, rVDRbytes, pct_rVDR);
  WriteOut (OUTfp, text);
  snprintf (text, (size_t) sizeof(text),
	    "   zVDR count: %*ld, %*ld bytes,  %6.3f%%\n",
	    width1, zVDRcount, width2, zVDRbytes, pct_zVDR);
  WriteOut (OUTfp, text);
  snprintf (text, (size_t) sizeof(text),
	    "    VXR count: %*ld, %*ld bytes,  %6.3f%%\n",
	    width1, VXRcount, width2, VXRbytes, pctVXR);
  WriteOut (OUTfp, text);
  snprintf (text, (size_t) sizeof(text),
	    "    VVR count: %*ld, %*ld bytes,  %6.3f%%\n",
	    width1, VVRcount, width2, VVRbytes, pctVVR);
  WriteOut (OUTfp, text);
  snprintf (text, (size_t) sizeof(text),
	    "   CVVR count: %*ld, %*ld bytes,  %6.3f%%\n",
	    width1, CVVRcount, width2, CVVRbytes, pctCVVR);
  WriteOut (OUTfp, text);
  snprintf (text, (size_t) sizeof(text),
	    "    CPR count: %*ld, %*ld bytes,  %6.3f%%\n",
	    width1, CPRcount, width2, CPRbytes, pctCPR);
  WriteOut (OUTfp, text);
  snprintf (text, (size_t) sizeof(text),
	    "    SPR count: %*ld, %*ld bytes,  %6.3f%%\n",
	    width1, SPRcount, width2, SPRbytes, pctSPR);
  WriteOut (OUTfp, text);
  snprintf (text, (size_t) sizeof(text),
	    "    UIR count: %*ld, %*ld bytes,  %6.3f%%\n\n\n",
	    width1, UIRcount, width2, UIRbytes, pctUIR);
  WriteOut (OUTfp, text);
  return;
}

/******************************************************************************
* DisplaySummary64.
******************************************************************************/

static void DisplaySummary64 (long level) {
  char text[132+1]; int width1, width2; OFF_T largestBytes;
  long IRcount = CCRcount + CDRcount + GDRcount + ADRcount + AgrEDRcount +
		 AzEDRcount + rVDRcount + zVDRcount + VXRcount + VVRcount +
		 CVVRcount + CPRcount + SPRcount + UIRcount;
  OFF_T totalBytes64 = usedBytes64 + wastedBytes64;
  double pctUsed = 100.0 * (((double) usedBytes64) / ((double) totalBytes64));
  double pctWasted = 100.0 * (((double) wastedBytes64) / ((double) totalBytes64));
  double pctCCR = 100.0 * (((double) CCRbytes64) / ((double) totalBytes64));
  double pctCDR = 100.0 * (((double) CDRbytes64) / ((double) totalBytes64));
  double pctGDR = 100.0 * (((double) GDRbytes64) / ((double) totalBytes64));
  double pctADR = 100.0 * (((double) ADRbytes64) / ((double) totalBytes64));
  double pctAgrEDR = 100.0 * (((double) AgrEDRbytes64) / ((double) totalBytes64));
  double pctAzEDR = 100.0 * (((double) AzEDRbytes64) / ((double) totalBytes64));
  double pct_rVDR = 100.0 * (((double) rVDRbytes64) / ((double) totalBytes64));
  double pct_zVDR = 100.0 * (((double) zVDRbytes64) / ((double) totalBytes64));
  double pctVXR = 100.0 * (((double) VXRbytes64) / ((double) totalBytes64));
  double pctVVR = 100.0 * (((double) VVRbytes64) / ((double) totalBytes64));
  double pctCVVR = 100.0 * (((double) CVVRbytes64) / ((double) totalBytes64));
  double pctCPR = 100.0 * (((double) CPRbytes64) / ((double) totalBytes64));
  double pctSPR = 100.0 * (((double) SPRbytes64) / ((double) totalBytes64));
  double pctUIR = 100.0 * (((double) UIRbytes64) / ((double) totalBytes64));
  width1 = Long64Width (totalBytes64);
  largestBytes = 0;
  largestBytes = MAXIMUM(largestBytes,CCRbytes64);
  largestBytes = MAXIMUM(largestBytes,CDRbytes64);
  largestBytes = MAXIMUM(largestBytes,GDRbytes64);
  largestBytes = MAXIMUM(largestBytes,ADRbytes64);
  largestBytes = MAXIMUM(largestBytes,AgrEDRbytes64);
  largestBytes = MAXIMUM(largestBytes,AzEDRbytes64);
  largestBytes = MAXIMUM(largestBytes,rVDRbytes64);
  largestBytes = MAXIMUM(largestBytes,zVDRbytes64);
  largestBytes = MAXIMUM(largestBytes,VXRbytes64);
  largestBytes = MAXIMUM(largestBytes,VVRbytes64);
  largestBytes = MAXIMUM(largestBytes,CVVRbytes64);
  largestBytes = MAXIMUM(largestBytes,CPRbytes64);
  largestBytes = MAXIMUM(largestBytes,SPRbytes64);
  largestBytes = MAXIMUM(largestBytes,UIRbytes64);
  width2 = Long64Width (largestBytes);
  WriteOut (OUTfp, "\n\nSummary...");
#if !defined(win32) && !defined(__MINGW32__)
  if (checksum == 1)
    snprintf (text, (size_t) sizeof(text),
	      "\n\n  Total bytes: %*lld (+16 if with checksum)", width1,
	      (long long) totalBytes64);
  else
    snprintf (text, (size_t) sizeof(text),
	      "\n\n  Total bytes: %*lld", width1, (long long) totalBytes64);
#else
  if (checksum == 1)
    snprintf (text, (size_t) sizeof(text),
	      "\n\n  Total bytes: %*I64d (+16 if with checksum)", width1,
	      (long long) totalBytes64);
  else
    snprintf (text, (size_t) sizeof(text),
	      "\n\n  Total bytes: %*I64d", width1, (long long) totalBytes64);
#endif
#if defined(vms)
  snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	    ", %ld blocks\n", (long) (((totalBytes64-1)/512)+1));
#else
  strcatX (text, "\n", 132);
#endif
  WriteOut (OUTfp, text);
#if !defined(win32) && !defined(__MINGW32__)
  snprintf (text, (size_t) sizeof(text),
	    "   Used bytes: %*lld, %7.3f%%\n",
	    width1, (long long) usedBytes64, pctUsed);
#else
  snprintf (text, (size_t) sizeof(text),
	    "   Used bytes: %*I64d, %7.3f%%\n",
            width1, (long long) usedBytes64, pctUsed);
#endif
  WriteOut (OUTfp, text);
#if !defined(win32) && !defined(__MINGW32__)
  snprintf (text, (size_t) sizeof(text),
	    " Unused bytes: %*lld, %7.3f%%\n\n",
	    width1, (long long) wastedBytes64, pctWasted);
#else
  snprintf (text, (size_t) sizeof(text),
	    " Unused bytes: %*I64d, %7.3f%%\n\n",
            width1, (long long) wastedBytes64, pctWasted);
#endif
  WriteOut (OUTfp, text);
  snprintf (text, (size_t) sizeof(text),
	    "     IR count: %*ld\n\n",
	    width1, IRcount);
  WriteOut (OUTfp, text);
#if !defined(win32) && !defined(__MINGW32__)
  snprintf (text, (size_t) sizeof(text),
	    "    CCR count: %*ld, %*lld bytes, %6.3f%%\n",
	    width1, CCRcount, width2, (long long) CCRbytes64, pctCCR);
#else
  snprintf (text, (size_t) sizeof(text),
	    "    CCR count: %*ld, %*I64d bytes, %6.3f%%\n",
            width1, CCRcount, width2, (long long) CCRbytes64, pctCCR);
#endif
  WriteOut (OUTfp, text);
#if !defined(win32) && !defined(__MINGW32__)
  snprintf (text, (size_t) sizeof(text),
	    "    CDR count: %*ld, %*lld bytes, %6.3f%%\n",
	    width1, CDRcount, width2, (long long) CDRbytes64, pctCDR);
#else
  snprintf (text, (size_t) sizeof(text),
	    "    CDR count: %*ld, %*I64d bytes, %6.3f%%\n",
            width1, CDRcount, width2, (long long) CDRbytes64, pctCDR);
#endif
  WriteOut (OUTfp, text);
#if !defined(win32) && !defined(__MINGW32__)
  snprintf (text, (size_t) sizeof(text),
	    "    GDR count: %*ld, %*lld bytes, %6.3f%%\n",
	    width1, GDRcount, width2, (long long) GDRbytes64, pctGDR);
#else
  snprintf (text, (size_t) sizeof(text),
	    "    GDR count: %*ld, %*I64d bytes, %6.3f%%\n",
            width1, GDRcount, width2, GDRbytes64, pctGDR);
#endif
  WriteOut (OUTfp, text);
#if !defined(win32) && !defined(__MINGW32__)
  if (!MOST(level))
    snprintf (text, (size_t) sizeof(text),
	      "    ADR count: %*ld, %*lld bytes, %6.3f%%\n",
	      width1, ADRcount, width2, (long long) ADRbytes64, pctADR);
  else
    snprintf (text, (size_t) sizeof(text),
	      "    ADR count: %*ld, %*lld bytes, %6.3f%% (G:%ld V:%ld)\n",
	      width1, ADRcount, width2, (long long) ADRbytes64, pctADR,
	      gADRcount, vADRcount);
#else
  if (!MOST(level))
    snprintf (text, (size_t) sizeof(text),
	      "    ADR count: %*ld, %*I64d bytes, %6.3f%%\n",
              width1, ADRcount, width2, (long long) ADRbytes64, pctADR);
  else
    snprintf (text, (size_t) sizeof(text),
	      "    ADR count: %*ld, %*I64d bytes, %6.3f%% (G:%ld V:%ld)\n",
              width1, ADRcount, width2, (long long) ADRbytes64, pctADR,
	      gADRcount, vADRcount);
#endif
  WriteOut (OUTfp, text);
#if !defined(win32) && !defined(__MINGW32__)
  snprintf (text, (size_t) sizeof(text),
	    " AgrEDR count: %*ld, %*lld bytes, %6.3f%%\n",
	    width1, AgrEDRcount, width2, (long long) AgrEDRbytes64, pctAgrEDR);
#else
  snprintf (text, (size_t) sizeof(text),
	    " AgrEDR count: %*ld, %*I64d bytes, %6.3f%%\n",
            width1, AgrEDRcount, width2, (long long) AgrEDRbytes64, pctAgrEDR);
#endif
  WriteOut (OUTfp, text);
#if !defined(win32) && !defined(__MINGW32__)
  snprintf (text, (size_t) sizeof(text),
	    "  AzEDR count: %*ld, %*lld bytes, %6.3f%%\n",
	    width1, AzEDRcount, width2, (long long) AzEDRbytes64, pctAzEDR);
#else
  snprintf (text, (size_t) sizeof(text),
	    "  AzEDR count: %*ld, %*I64d bytes, %6.3f%%\n",
            width1, AzEDRcount, width2, AzEDRbytes64, pctAzEDR);
#endif
  WriteOut (OUTfp, text);
#if !defined(win32) && !defined(__MINGW32__)
  snprintf (text, (size_t) sizeof(text),
	    "   rVDR count: %*ld, %*lld bytes, %6.3f%%\n",
	    width1, rVDRcount, width2, (long long) rVDRbytes64, pct_rVDR);
#else
  snprintf (text, (size_t) sizeof(text),
	    "   rVDR count: %*ld, %*I64d bytes, %6.3f%%\n",
            width1, rVDRcount, width2, (long long) rVDRbytes64, pct_rVDR);
#endif
  WriteOut (OUTfp, text);
#if !defined(win32) && !defined(__MINGW32__)
  snprintf (text, (size_t) sizeof(text),
	    "   zVDR count: %*ld, %*lld bytes, %6.3f%%\n",
	    width1, zVDRcount, width2, (long long) zVDRbytes64, pct_zVDR);
#else
  snprintf (text, (size_t) sizeof(text),
	    "   zVDR count: %*ld, %*I64d bytes, %6.3f%%\n",
            width1, zVDRcount, width2, (long long) zVDRbytes64, pct_zVDR);
#endif
  WriteOut (OUTfp, text);
#if !defined(win32) && !defined(__MINGW32__)
  snprintf (text, (size_t) sizeof(text),
	    "    VXR count: %*ld, %*lld bytes, %6.3f%%\n",
	    width1, VXRcount, width2, (long long) VXRbytes64, pctVXR);
#else
  snprintf (text, (size_t) sizeof(text),
	    "    VXR count: %*ld, %*I64d bytes, %6.3f%%\n",
            width1, VXRcount, width2, (long long) VXRbytes64, pctVXR);
#endif
  WriteOut (OUTfp, text);
#if !defined(win32) && !defined(__MINGW32__)
  snprintf (text, (size_t) sizeof(text),
	    "    VVR count: %*ld, %*lld bytes, %6.3f%%\n",
	    width1, VVRcount, width2, (long long) VVRbytes64, pctVVR);
#else
  snprintf (text, (size_t) sizeof(text),
	    "    VVR count: %*ld, %*I64d bytes, %6.3f%%\n",
            width1, VVRcount, width2, (long long) VVRbytes64, pctVVR);
#endif
  WriteOut (OUTfp, text);
#if !defined(win32) && !defined(__MINGW32__)
  snprintf (text, (size_t) sizeof(text),
	    "   CVVR count: %*ld, %*lld bytes, %6.3f%%\n",
	    width1, CVVRcount, width2, (long long) CVVRbytes64, pctCVVR);
#else
  snprintf (text, (size_t) sizeof(text),
	    "   CVVR count: %*ld, %*I64d bytes, %6.3f%%\n",
            width1, CVVRcount, width2, (long long) CVVRbytes64, pctCVVR);
#endif
  WriteOut (OUTfp, text);
#if !defined(win32) && !defined(__MINGW32__)
  snprintf (text, (size_t) sizeof(text),
	    "    CPR count: %*ld, %*lld bytes, %6.3f%%\n",
	    width1, CPRcount, width2, (long long) CPRbytes64, pctCPR);
#else
  snprintf (text, (size_t) sizeof(text),
	    "    CPR count: %*ld, %*I64d bytes, %6.3f%%\n",
            width1, CPRcount, width2, (long long) CPRbytes64, pctCPR);
#endif
  WriteOut (OUTfp, text);
#if !defined(win32) && !defined(__MINGW32__)
  snprintf (text, (size_t) sizeof(text),
	    "    SPR count: %*ld, %*lld bytes, %6.3f%%\n",
	    width1, SPRcount, width2, (long long) SPRbytes64, pctSPR);
#else
  snprintf (text, (size_t) sizeof(text),
	    "    SPR count: %*ld, %*I64d bytes, %6.3f%%\n",
            width1, SPRcount, width2, (long long) SPRbytes64, pctSPR);
#endif
  WriteOut (OUTfp, text);
#if !defined(win32) && !defined(__MINGW32__)
  snprintf (text, (size_t) sizeof(text),
	    "    UIR count: %*ld, %*lld bytes, %6.3f%%\n\n\n",
	    width1, UIRcount, width2, (long long) UIRbytes64, pctUIR);
#else
  snprintf (text, (size_t) sizeof(text),
	    "    UIR count: %*ld, %*I64d bytes, %6.3f%%\n\n\n",
            width1, UIRcount, width2, (long long) UIRbytes64, pctUIR);
#endif
  WriteOut (OUTfp, text);
  return;
}

/******************************************************************************
* DumpVXR.
******************************************************************************/

static void DumpVXR (CDFfp, vxrOffset, indent)
vFILE *CDFfp;
Int32 vxrOffset;
int indent;
{
  char text[MAX_SCREENLINE_LEN+1];
  struct VXRstruct VXR; int entryN;
  Int32 irType;
  ReadVXR (CDFfp, vxrOffset,
           VXR_RECORD, &VXR,
           VXR_NULL);
  CpyNcharacters (text, indent, ' ');
  snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	    "VXR @ %s (%d/%d)\n",
            FileOffset(vxrOffset), (int) VXR.NusedEntries, (int) VXR.Nentries);
  WriteOut (OUTfp, text);
  for (entryN = 0; entryN < VXR.Nentries; entryN++) {
     CpyNcharacters (text, indent, ' ');
     if (VXR.First[entryN] == NO_RECORD)
       strcatX (text, "-------- ", MAX_SCREENLINE_LEN);
     else
       snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	         "%08ld ", (long) VXR.First[entryN]);
     if (VXR.Last[entryN] == NO_RECORD)
       strcatX (text, "-------- ", MAX_SCREENLINE_LEN);
     else
       snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	         "%08ld ", (long) VXR.Last[entryN]);
     if (VXR.Offset[entryN] == NO_OFFSET)
       strcatX (text, "----------", MAX_SCREENLINE_LEN);
     else
       strcatX (text, FileOffset(VXR.Offset[entryN]), MAX_SCREENLINE_LEN);
     strcatX (text, "\n", MAX_SCREENLINE_LEN);
     WriteOut (OUTfp, text);
     if (VXR.Offset[entryN] != NO_OFFSET) {
       ReadIrType (CDFfp, VXR.Offset[entryN], &irType);
       switch (irType) {
         case VVR_:
         case CVVR_:
           break;
         case VXR_:
           DumpVXR (CDFfp, VXR.Offset[entryN], indent + 5);
           break;
         default:
           break;
       }
     } 
  }
  if (VXR.VXRnext != ZERO_OFFSET) DumpVXR (CDFfp, VXR.VXRnext, indent);
  return;
}            
             
/******************************************************************************
* DumpVXR64.
******************************************************************************/

static void DumpVXR64 (CDFfp, vxrOffset, indent)
vFILE *CDFfp;
OFF_T vxrOffset;
int indent;
{
  char text[MAX_SCREENLINE_LEN+1];
  struct VXRstruct64 VXR; int entryN;
  Int32 irType;
  ReadVXR64 (CDFfp, vxrOffset,
	     VXR_RECORD, &VXR,
	     VXR_NULL);
  CpyNcharacters (text, indent, ' ');
  snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	    "VXR @ %s (%d/%d)\n",
	    FileOffset64(vxrOffset), (int) VXR.NusedEntries, (int) VXR.Nentries);
  WriteOut (OUTfp, text);
  for (entryN = 0; entryN < VXR.Nentries; entryN++) {
     CpyNcharacters (text, indent, ' ');
     if (VXR.First[entryN] == NO_RECORD)
       strcatX (text, "-------- ", MAX_SCREENLINE_LEN);
       snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	         "%08ld ", (long) VXR.First[entryN]);
     if (VXR.Last[entryN] == NO_RECORD)
       strcatX (text, "-------- ", MAX_SCREENLINE_LEN);
     else
       snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	         "%08ld ", (long) VXR.Last[entryN]);
     if (VXR.Offset[entryN] == (OFF_T)NO_OFFSET64)
       strcatX (text, "----------", MAX_SCREENLINE_LEN);
     else
       strcatX (text, FileOffset64(VXR.Offset[entryN]), MAX_SCREENLINE_LEN);
     strcatX (text, "\n", MAX_SCREENLINE_LEN);
     WriteOut (OUTfp, text);
     if (VXR.Offset[entryN] != (OFF_T)NO_OFFSET64) {
       ReadIrType64 (CDFfp, VXR.Offset[entryN], &irType);
       switch (irType) {
	 case VVR_:
	 case CVVR_:
	   break;
	 case VXR_:
	   DumpVXR64 (CDFfp, VXR.Offset[entryN], indent + 5);
	   break;
	 default:
	   break;
       }
     }
  }
  if (VXR.VXRnext != ZERO_OFFSET64) DumpVXR64 (CDFfp, VXR.VXRnext, indent);
  return;
}

/******************************************************************************
* FileOffset.
******************************************************************************/

static char *FileOffset (offset)
Int32 offset;
{
  switch (radix) {
    case 10: return Deci32(offset);
    case 16: return Hex32(offset);
    default: return "?";
  }
}

/******************************************************************************
* FileOffset64.
******************************************************************************/

static char *FileOffset64 (offset)
OFF_T offset;
{
  switch (radix) {
    case 10: return Deci64(offset);
    case 16: return Hex64(offset);
    default: return "?";
  }
} 
    
/******************************************************************************
* Deci32.
******************************************************************************/

static char *Deci32 (value)
Int32 value;
{
  static char text[3][10+1];
  static int which = -1;
  which = (which + 1) % 3;
  snprintf (text[which], (size_t) 11, "%010ld", (long) value);
  return text[which];
}

/******************************************************************************
* Deci64.
******************************************************************************/

static char *Deci64 (value)
OFF_T value;
{
  static char text[3][20+1];
  static int which = -1;
  which = (which + 1) % 3;
#if !defined(win32) && !defined(__MINGW32__)
  snprintf (text[which], (size_t) 21, "%020lld", (long long) value);
#else
  snprintf (text[which], (size_t) 21, "%020I64d", (long long) value);
#endif
  return text[which];
}

/******************************************************************************
* Hex64.
******************************************************************************/

static char *Hex64 (value)
OFF_T value;
{
  static char text[3][20+1];
  static int which = -1;
  which = (which + 1) % 3;
  snprintf (text[which], (size_t) 21, Int64hexFORMAT, (long long) value);
  return text[which];
}

/******************************************************************************
* Hex32.
******************************************************************************/

static char *Hex32 (value)
Int32 value;
{
  static char text[3][10+1];
  static int which = -1;
  which = (which + 1) % 3;
  snprintf (text[which], (size_t) 11, Int32hexFORMAT, value);
  return text[which];
}

/******************************************************************************
* Hex32_64.
******************************************************************************/

static char *Hex32_64 (value)
Int32 value;
{
  static char text[3][10+1];
  static int which = -1;
  which = (which + 1) % 3;
  snprintf (text[which], (size_t) 11, Int32hexFORMAT, value);
  return text[which];
}

/******************************************************************************
* SeekTo.
******************************************************************************/

static Logical SeekTo (fp, offset, how)
vFILE *fp;
long offset;
int how;
{
  if (!SEEKv(fp,offset,how)) {
    WriteOut (OUTfp, "\nSeek error.\n");
    return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* SeekTo64.
******************************************************************************/

static Logical SeekTo64 (fp, offset, how)
vFILE *fp;
OFF_T offset;
int how;
{
  if (!SEEKv64(fp,offset,how)) {
    WriteOut (OUTfp, "\nSeek error.\n");
    return FALSE;
  } 
  return TRUE;
} 

/******************************************************************************
* Read4.
******************************************************************************/

static Logical Read4 (fp, value)
vFILE *fp;
Int32 *value;
{
  if (!Read32(fp,value)) {
    DisplayReadFailure (fp);
    return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* Read4_64.
******************************************************************************/

static Logical Read4_64 (fp, value)
vFILE *fp;
Int32 *value;
{
  if (!Read32_64(fp,value)) {
    DisplayReadFailure (fp);
    return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* Read8.
******************************************************************************/

static Logical Read8 (fp, value)
vFILE *fp;
OFF_T *value;
{
  if (!Read64_64(fp,value)) {
    DisplayReadFailure (fp);
    return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* Read4s.
******************************************************************************/

static Logical Read4s (fp, values, count)
vFILE *fp;
Int32 *values;
int count;
{
  int i;
  for (i = 0; i < count; i++) {
     if (!Read32(fp,&values[i])) return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* Read4s_64.
******************************************************************************/

static Logical Read4s_64 (fp, values, count)
vFILE *fp;
Int32 *values;
int count;
{
  int i;
  for (i = 0; i < count; i++) {
     if (!Read32_64(fp,&values[i])) return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* Read8s.
******************************************************************************/

static Logical Read8s (fp, values, count)
vFILE *fp;
OFF_T *values;
int count;
{
  int i;
  for (i = 0; i < count; i++) {
     if (!Read64_64(fp,&values[i])) return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* ReadBytes.
******************************************************************************/

static Logical ReadBytes (fp, bytes, count)
vFILE *fp;
void *bytes;
Int32 count;
{
  if (!READv(bytes,(size_t)1,(size_t)count,fp)) {
    DisplayReadFailure (fp);
    return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* ReadBytes64.
******************************************************************************/

static Logical ReadBytes64 (fp, bytes, count)
vFILE *fp;
void *bytes;
Int32 count;
{
  if (!READv64(bytes,(size_t)1,(size_t)count,fp)) {
    DisplayReadFailure (fp);
    return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* DisplayReadFailure.
******************************************************************************/

static void DisplayReadFailure (fp)
vFILE *fp;
{
  if (V_error(fp)) {
    WriteOut (OUTfp, "\nRead failed - error from file system.\n");
    return;
  }
  if (V_eof(fp)) {
    WriteOut (OUTfp, BOO(EOFok,"\nEOF encountered.\n",
			       "\nUnexpected EOF encountered.\n"));
    return;
  }
  WriteOut (OUTfp, "\nUnknown read error - contact CDFsupport.\n");
  return;
}

/******************************************************************************
* DumpQOPs.
*    Returns TRUE if execution should continue.
******************************************************************************/

#if defined(mac)
Logical DumpQOPs (argC, argV)
int *argC;
char **argV[];
{
  DialogPtr dialogP;
  DialogRecord dRecord;
  WindowPtr behind = (WindowPtr) -1;
  ControlHandle controlHs[MAXIMUMin+1];
  Rect iRect;
#ifdef __MWERKS__
  ModalFilterUPP FilterDialogQOPsoUPP;
  FileFilterUPP FilterForCDFsUPP;
  UserItemUPP OutlineDefaultButtonUPP;
#endif
  short itemN, iType;
  static Logical first = TRUE;
  char cancelTitle[] = "Cancel";
  static int dumpLevel = DEFAULTlevelIRsDUMP;
  static Logical pageOutput = DEFAULTpageCVT;
  static Logical checkSizes = DEFAULTsizesIRsDUMP;
  static Logical showSummary = DEFAULTsummaryIRsDUMP;
  static Logical showIndexing = DEFAULTindexingIRsDUMP;
  static Str255 CDFtext = "\p";
  static Str255 outText = "\p";
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
  SetIText ((Handle) controlHs[CDFTEXTin], CDFtext);
  SetIText ((Handle) controlHs[OUTTEXTin], outText);
  switch (dumpLevel) {
    case BRIEF_: SetCtlValue (controlHs[BRIEFin], 1); break;
    case MOST_: SetCtlValue (controlHs[MOSTin], 1); break;
    case FULL_: SetCtlValue (controlHs[FULLin], 1); break;
  }
  if (pageOutput) SetCtlValue (controlHs[PAGEin], 1);
  if (checkSizes) SetCtlValue (controlHs[SIZESin], 1);
  if (showSummary) SetCtlValue (controlHs[SUMMARYin], 1);
  if (showIndexing) SetCtlValue (controlHs[INDEXINGin], 1);
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
  FilterDialogQOPsoUPP = NewModalFilterProc (FilterDialogQOPso);
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
		int n;
		/**********************************************************************
		* Get the value of each control.
		**********************************************************************/
		GetIText ((Handle) controlHs[CDFTEXTin], CDFtext);
		GetIText ((Handle) controlHs[OUTTEXTin], outText);
		if (GetCtlValue(controlHs[BRIEFin])) dumpLevel = BRIEF_;
		if (GetCtlValue(controlHs[MOSTin])) dumpLevel = MOST_;
		if (GetCtlValue(controlHs[FULLin])) dumpLevel = FULL_;
		pageOutput = GetCtlValue (controlHs[PAGEin]);
		checkSizes = GetCtlValue (controlHs[SIZESin]);
		showSummary = GetCtlValue (controlHs[SUMMARYin]);
		showIndexing = GetCtlValue (controlHs[INDEXINGin]);
		/**********************************************************************
		* Build argc/argv.
		**********************************************************************/
		*argC = 6 + BOO(NULpString(CDFtext),0,1) +
				    BOO(NULpString(outText),0,2);
		*argV = (char **) cdf_AllocateMemory ((size_t)*argC * sizeof(char *),
					  FatalError);
		n = 0;
		MAKEstrARGv (argV, n, pgmName)
		if (!NULpString(CDFtext)) {
		  PtoCstr (CDFtext);
		  MAKEstrARGv (argV, n, (char *) CDFtext)
		  CtoPstr ((char *) CDFtext);
		}
		switch (dumpLevel) {
		  case BRIEF_:
		    MAKEstrARGv (argV, n, "-brief")
		    break;
		  case MOST_:
		    MAKEstrARGv (argV, n, "-most")
		    break;
		  case FULL_:
		    MAKEstrARGv (argV, n, "-full")
		    break;
		}
		MAKEbooARGv (argV, n, pageOutput, "-page", "-nopage")
		MAKEbooARGv (argV, n, checkSizes, "-sizes", "-nosizes")
		MAKEbooARGv (argV, n, showSummary, "-summary", "-nosummary")
		MAKEbooARGv (argV, n, showIndexing, "-indexing", "-noindexing")
		if (!NULpString(outText)) {
		  MAKEstrARGv (argV, n, "-output")
		  PtoCstr (outText);
		  MAKEstrARGv (argV, n, (char *) outText)
		  CtoPstr ((char *) outText);
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
      * Select CDF specification.
      ************************************************************************/
      case CDFSELECTin: {
		StandardFileReply CDFreply;
		char CDFpath[DU_MAX_PATH_LEN+1];
#ifndef __MWERKS__
		StandardGetFile (FilterForCDFs, -1, NULL, &CDFreply);
#else
		FilterForCDFsUPP = NewFileFilterProc((ProcPtr) FilterForCDFs);
		StandardGetFile (FilterForCDFsUPP, -1, NULL, &CDFreply);
		DisposeRoutineDescriptor (FilterForCDFsUPP);
#endif
		if (CDFreply.sfGood && !CDFreply.sfIsFolder && !CDFreply.sfIsVolume) {
		  BuildMacPath (&CDFreply.sfFile, CDFpath, TRUE);
		  CDFtext[0] = strlen (CDFpath);
		  strcpyX ((char *) &CDFtext[1], CDFpath, 255);
		  SetIText ((Handle) controlHs[CDFTEXTin], CDFtext);
		}
		break;
      }
      /************************************************************************
      * Select output file specification.
      *     The cursor is set because `StandardPutFile' leaves the cursor as
      * an iBeam (instead of returning it to what it was).
      ************************************************************************/
      case OUTSELECTin: {
	StandardFileReply outReply;
	char outPath[DU_MAX_PATH_LEN+1], prompt[] = "Enter output file:";
	StandardPutFile (CtoPstr(prompt), CtoPstr(""), &outReply);
	if (outReply.sfGood && !outReply.sfIsFolder && !outReply.sfIsVolume) {
	  BuildMacPath (&outReply.sfFile, outPath, TRUE);
	  outText[0] = strlen (outPath);
	  strcpyX ((char *) &outText[1], outPath, 255);
	  SetIText ((Handle) controlHs[OUTTEXTin], outText);
	}
	SetCursor (&(qd.arrow));
	break;
      }
      /************************************************************************
      * Check boxes.
      ************************************************************************/
      case SIZESin:
      case PAGEin:
      case SUMMARYin:
      case INDEXINGin:
	SetCtlValue (controlHs[itemN], BOO(GetCtlValue(controlHs[itemN]),0,1));
	break;
      /************************************************************************
      * Radio buttons.
      ************************************************************************/
      case BRIEFin:
	SetCtlValue (controlHs[BRIEFin], 1);
	SetCtlValue (controlHs[MOSTin], 0);
	SetCtlValue (controlHs[FULLin], 0);
	break;
      case MOSTin:
	SetCtlValue (controlHs[BRIEFin], 0);
	SetCtlValue (controlHs[MOSTin], 1);
	SetCtlValue (controlHs[FULLin], 0);
	break;
      case FULLin:
	SetCtlValue (controlHs[BRIEFin], 0);
	SetCtlValue (controlHs[MOSTin], 0);
	SetCtlValue (controlHs[FULLin], 1);
	break;
    }
  }
}
#endif
