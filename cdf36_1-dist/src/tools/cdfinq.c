/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                                    CDFinquire.
*
*  Version 1.6f, 16-Dec-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  23-Mar-92, J Love     Original version.
*   V1.1  16-Sep-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V1.2   2-Nov-92, J Love     Added variable values display.
*   V1.3  26-Jan-94, J Love     CDF V2.4.
*   V1.4  27-Oct-94, J Love	CDF V2.5.
*   V1.4a  6-Apr-95, J Love	POSIX.
*   V1.5  13-Sep-95, J Love	Added CDFexport defaults.
*   V1.5a 28-Sep-95, J Love	Macintosh dialog filtering.  Outline default
*				button.
*   V1.6   9-Sep-96, J Love	CDF V2.6.
*   V1.6a 15-Nov-96, J Love	Added `simple' mode to CDFexport.
*   V1.6b  4-Feb-97, J Love	Show defaults for CDFexport `simple' mode
*				correctly and added `prompt'.
*   V1.6c 21-Feb-97, J Love	Removed RICE.
*   V1.6d  4-Mar-97, J Love	Windows NT for MS Visual C/C++ on an IBM PC.
*   V1.6e  7-Dec-97, J Love	More Windows NT.
*   V1.6f 16-Dec-97, J Love	Added Alpha/OpenVMS floating-point defaults.
*   V1.6g 24-Feb-03, M Liu      Changed EPOCH default value from double to an 
*                               encoded date string.
*   V1.7  22-Jan-06, M Liu      Removed CDFwalk and CDFlist parts.
*   V3.3  10-Jan-09, M Liu      Validate a file before it is used.
*   V3.4  04-Apr-11, M Liu      Modified to show the default INT8/TT2000 
*                               values.
*
******************************************************************************/

#include "cdfinq.h"

/******************************************************************************
* Increased stack size for Borland C on IBM PC.
******************************************************************************/

#if defined(BORLANDC)
extern unsigned _stklen = BORLANDC_STACK_SIZE;
#endif

/******************************************************************************
* Local function prototypes.
******************************************************************************/

static char *epochToken PROTOARGs((int zMode));
static char *majorityToken PROTOARGs((int zMode));
static char *zModeToken PROTOARGs((int zMode));
static char *No PROTOARGs((Logical state));

#if !defined(mac)
static void CDFexportVMS PROTOARGs((void));
static void CDFexportInitialVMS PROTOARGs((void));
static void CDFsimpleInitialVMS PROTOARGs((void));
static void CDFexportNotVMS PROTOARGs((void));
static void CDFexportInitialNotVMS PROTOARGs((void));
static void CDFsimpleInitialNotVMS PROTOARGs((void));
#endif

/******************************************************************************
* Main.
******************************************************************************/

#if !defined(win32) || (defined(win32) && defined(ALONE))
MAIN {
  Logical success = TRUE;
  strcpyX (pgmName, "CDFinquire", MAX_PROGRAM_NAME_LEN);
#if defined(mac)
  MacExecuteSO (InquireCDF, InquireQOPs);
#else
  success = InquireCDF (argc, argv);
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
* InquireCDF.
******************************************************************************/

Logical InquireCDF (argC, argV)
int argC;
char *argV[];
{
  QOP *qop;
  static char *validQuals[] = { "id", "page", "nopage", "about", NULL };
  static int optRequired[] = { FALSE, FALSE, FALSE, FALSE, 0 };
  Logical qopError = FALSE;

/*  CDFsetValidate (VALIDATEFILEon); */
  /****************************************************************************
  * Based on the number of command line arguments entered...
  ****************************************************************************/
  switch (argC) {
    case 1:
      PageOLH ("cdfinq.olh", argV[0]);
      return TRUE;
    case 2:
      if (strcmp(argV[1],"-java") == 0) {
        pagingOn = FALSE;
        PageOLH ("cdfinqj.olh", argV[0]);
        return TRUE;
      }
    default:
      qop = Qop (argC, argV, validQuals, optRequired);
      if (qop == NULL) return FALSE;
      /***********************************************************************
      * Check for `about' qualifier.
      ***********************************************************************/
      if (qop->qualEntered[ABOUTqual]) {
	DisplayIdentification (pgmName);
	cdf_FreeMemory (qop, FatalError);
	return TRUE;
      }
      /***********************************************************************
      * Check for `id' qualifier.
      ***********************************************************************/
      if (!qop->qualEntered[IDqual]) {
	DisplayError ("Missing qualifier (`id').");
	qopError = TRUE;
      }
      /***********************************************************************
      * Check for `page' qualifier.
      ***********************************************************************/
      qopError = qopError | !TFqualifier(qop,&pagingOn,PAGEqual,NOPAGEqual,
					 DEFAULTpageINQ,"page");
      /************************************************************************
      * Free QOP memory and check for an error.
      ************************************************************************/
      cdf_FreeMemory (qop, FatalError);
      if (qopError) return FALSE;
      break;
  }
  /****************************************************************************
  * Display...
  ****************************************************************************/
  DisplayVerRelInc ();
  DisplayCacheDefaults ();
  DisplayBlockingDefaults ();
  DisplayPadValueDefaults ();
  DisplayMiscDefaults ();
  DisplayToolkitDefaults ();
  return TRUE;
}

/******************************************************************************
* DisplayVerRelInc.
******************************************************************************/

void DisplayVerRelInc () {
  long version, release, increment; char subincrement;
  CDFlib (GET_, LIB_VERSION_, &version,
                LIB_RELEASE_, &release,
                LIB_INCREMENT_, &increment,
                LIB_subINCREMENT_, &subincrement,
          NULL_);
  if (subincrement != ' ')
    WriteOutSO ("\nCDF distribution V%ld.%ld.%ld_%c\n",
                version, release, increment, subincrement);
  else
    WriteOutSO ("\nCDF distribution V%ld.%ld.%ld\n",
                version, release, increment);
  return;
}

/******************************************************************************
* DisplayCacheDefaults.
******************************************************************************/

void DisplayCacheDefaults () {
  char text[MAX_SCREENLINE_LEN+1];
  WriteOutSO ("\nCache size defaults:\n\n");
  snprintf (text, (size_t) sizeof(text),
	    "  Single-file CDF, dotCDF file: %d\n", NUMcacheSINGLE);
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "  Single-file CDF, staging scratch file: %d\n", NUMcacheSTAGE);
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "  Single-file CDF, compression scratch file: %d\n",
	   NUMcacheCOMPRESS);
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "  Multi-file CDF, dotCDF file: %d\n", NUMcacheMULTI);
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "  Multi-file CDF, variable file: %d\n", NUMcacheVAR);
  WriteOutSO (text);
  return;
}  

/******************************************************************************
* DisplayBlockingDefaults.
******************************************************************************/

void DisplayBlockingDefaults () {
  char text[MAX_SCREENLINE_LEN+1];
  WriteOutSO ("\nBlocking factor defaults (minimum bytes/block):\n\n");
  snprintf (text, (size_t) sizeof(text),
	    "  Standard: %d\n", MIN_BLOCKING_BYTES_standard);
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "  SparseRecords: %d\n", MIN_BLOCKING_BYTES_sparse);
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "  Compressed: %d\n", MIN_BLOCKING_BYTES_compressed);
  WriteOutSO (text);
  return;
}  

/******************************************************************************
* DisplayPadValueDefaults.
******************************************************************************/

void DisplayPadValueDefaults () {
  char text[MAX_SCREENLINE_LEN+1];
  char epString[EPOCH_STRING_LEN+1];
  char epString16[EPOCH16_STRING_LEN+1];
  char tt2000[TT2000_3_STRING_LEN+1];
  double epoch16[2];
  WriteOutSO ("\nDefault pad values:\n\n");
  snprintf (text, (size_t) sizeof(text),
	    "  CDF_BYTE: %d\n", (int) DEFAULT_BYTE_PADVALUE);
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "  CDF_INT1: %d\n", (int) DEFAULT_INT1_PADVALUE);
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "  CDF_INT2: %d\n", (int) DEFAULT_INT2_PADVALUE);
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "  CDF_INT4: ");
  snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	    Int32FORMAT, (Int32) DEFAULT_INT4_PADVALUE);
  snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	    "\n");
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
            "  CDF_INT8: ");
  snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
            Int64FORMAT, (long long) DEFAULT_INT8_PADVALUE);
  snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
            "\n");
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "  CDF_UINT1: %u\n", (uInt4) DEFAULT_UINT1_PADVALUE);
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "  CDF_UINT2: %u\n", (uInt4) DEFAULT_UINT2_PADVALUE);
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "  CDF_UINT4: ");
  snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	    Int32uFORMAT, (uInt32) DEFAULT_INT4_PADVALUE);
  snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	    "\n");
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "  CDF_REAL4: %g\n", (float) DEFAULT_REAL4_PADVALUE);
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "  CDF_FLOAT: %g\n", (float) DEFAULT_FLOAT_PADVALUE);
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "  CDF_REAL8: %g\n", (double) DEFAULT_REAL8_PADVALUE);
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "  CDF_DOUBLE: %g\n", (double) DEFAULT_DOUBLE_PADVALUE);
  WriteOutSO (text);
  encodeEPOCH((double) DEFAULT_EPOCH_PADVALUE, epString);
  snprintf (text, (size_t) sizeof(text),
	    "  CDF_EPOCH: %s\n", epString);
  WriteOutSO (text);
  epoch16[0] = epoch16[1] = DEFAULT_EPOCH16_PADVALUE;
  encodeEPOCH16(epoch16, epString16);
  snprintf (text, (size_t) sizeof(text),
	    "  CDF_EPOCH16: %s\n", epString16);
  WriteOutSO (text);
  CDF_TT2000_to_UTC_string((Int64) DEFAULT_TT2000_PADVALUE, tt2000,
                           TT2000_3_STYLE);
  snprintf (text, (size_t) sizeof(text),
            "  CDF_TIME_TT2000: %s\n", tt2000);
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "  CDF_CHAR: \"%c\"\n", (sChar) DEFAULT_CHAR_PADVALUE);
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "  CDF_UCHAR: \"%c\"\n", (uChar) DEFAULT_UCHAR_PADVALUE);
  WriteOutSO (text);
  return;
}  

/******************************************************************************
* DisplayMiscDefaults.
******************************************************************************/

void DisplayMiscDefaults () {
  char text[MAX_SCREENLINE_LEN+1];
  WriteOutSO ("\n");
  snprintf (text, (size_t) sizeof(text),
	    "RLE compression/decompression %s supported.\n",
	   BOO((Logical)SUPPORT_RLE,"is","is not"));
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "HUFF compression/decompression %s supported.\n",
	   BOO((Logical)SUPPORT_HUFF,"is","is not"));
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "AHUFF compression/decompression %s supported.\n",
	   BOO((Logical)SUPPORT_AHUFF,"is","is not"));
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "GZIP compression/decompression %s supported.\n",
	   BOO((Logical)SUPPORT_GZIP,"is","is not"));
  WriteOutSO (text);
  WriteOutSO ("\n");
  snprintf (text, (size_t) sizeof(text),
	    "MD5 checksum %s supported.\n",
           BOO((Logical)SUPPORT_MD5,"is","is not"));
  WriteOutSO (text);
  WriteOutSO ("\n");
  snprintf (text, (size_t) sizeof(text),
	    "Trailing blanks %s stripped from CDF paths.\n",
	   BOO((Logical)STRIP_TRAILING_BLANKS_FROM_CDFPATH,"are","are not"));
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "CDF library allows %s access to CDFs.\n",
	   BOO((Logical)BUILD_READ_ONLY_DISTRIBUTION,"read/only",
						     "read/write"));
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "Status code explanations %s available from CDF library.\n",
	   BOO((Logical)INCLUDE_STATUS_TEXT,"are","are not"));
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "-0.0 %s converted to 0.0 by default.\n",
	   BOO((Logical)DEFAULT_NEGtoPOSfp0,"is","is not"));
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "\nDefault encoding: %s\n",
	   BOO((Logical)DEFAULT_TO_HOST_ENCODING,"HOST_ENCODING",
						 "IBMPC_ENCODING"));
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "Default majority: %s\n",
	   BOO((Logical)DEFAULT_TO_ROW_MAJOR,"ROW_MAJOR","COLUMN_MAJOR"));
  WriteOutSO (text);
  snprintf (text, (size_t) sizeof(text),
	    "Default format: %s\n",
	   BOO((Logical)DEFAULT_TO_SINGLE_FILE,"SINGLE_FILE","MULTI_FILE"));
  WriteOutSO (text);
#if defined(alphavmsD)
  WriteOutSO ("Default single-precision floating-point: F_FLOAT\n");
  WriteOutSO ("Default double-precision floating-point: D_FLOAT\n");
#endif
#if defined(alphavmsG)
  WriteOutSO ("Default single-precision floating-point: F_FLOAT\n");
  WriteOutSO ("Default double-precision floating-point: G_FLOAT\n");
#endif
#if defined(alphavmsI)
  WriteOutSO ("Default single-precision floating-point: IEEE_FLOAT\n");
  WriteOutSO ("Default double-precision floating-point: IEEE_FLOAT\n");
#endif
  return;
}  

/******************************************************************************
* DisplayToolkitDefaults.
******************************************************************************/

void DisplayToolkitDefaults () {
#if !defined(mac) && !defined(win32)
  char text[MAX_SCREENLINE_LEN+1];
  static char *valuesTokens[] = { "none", "nrv", "rv", "all", "named" };
#endif
  WriteOut (stdout, "\nToolkit defaults:\n");
#if defined(mac) || defined(win32)
#if defined(mac)
  WriteOut (stdout, "    On a Macintosh the defaults for a\n");
#else
  WriteOut (stdout, "    On a Windows system the defaults for a\n");
#endif
  WriteOut (stdout, "    toolkit program are shown in the initial\n");
  WriteOut (stdout, "    parameters/qualifiers dialog box.\n");
#else
  /****************************************************************************
  * CDFconvert.
  ****************************************************************************/
  WriteOut (stdout, "\n");
  WriteOut (stdout, "  CDFconvert\n");
#if defined(vms)
  snprintf (text, (size_t) sizeof(text),
	    "    /%sLOG\n", No(DEFAULTlogCVT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sPERCENT\n", No(DEFAULTpctCVT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sDELETE\n", No(DEFAULTdelCVT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sNEG2POSFP0\n", No(DEFAULT_NEGtoPOSfp0));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sPAGE\n", No(DEFAULTpageCVT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /ZMODE=%s\n", zModeToken(DEFAULTzModeCVT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /REPORT=%s\n",
	   StatusCodeReportOptions(REPORTerrorsDEFAULT,
				   REPORTwarningsDEFAULT,
				   REPORTinfosDEFAULT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sSTATISTICS\n", No(DEFAULTstatsCVT));
  WriteOut (stdout, text);
#else
  snprintf (text, (size_t) sizeof(text),
	    "    -%slog\n", No(DEFAULTlogCVT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%spercent\n", No(DEFAULTpctCVT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sdelete\n", No(DEFAULTdelCVT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sneg2posfp0\n", No(DEFAULT_NEGtoPOSfp0));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%spage\n", No(DEFAULTpageCVT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -zmode %s\n", zModeToken(DEFAULTzModeCVT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -report %s\n",
	   StatusCodeReportOptions(REPORTerrorsDEFAULT,
				   REPORTwarningsDEFAULT,
				   REPORTinfosDEFAULT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sstatistics\n", No(DEFAULTstatsCVT));
  WriteOut (stdout, text);
#endif
  /****************************************************************************
  * CDFcompare.
  ****************************************************************************/
  WriteOut (stdout, "\n");
  WriteOut (stdout, "  CDFcompare\n");
#if defined(vms)
  snprintf (text, (size_t) sizeof(text),
	    "    /%sLOG\n", No(DEFAULTlogCMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sATTR\n", No(DEFAULTattrCMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sVAR\n", No(DEFAULTvarCMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sNUMBER\n", No(DEFAULTnumberCMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sPERCENT\n", No(DEFAULTpctCMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sETC\n", No(DEFAULTetcCMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sPAGE\n", No(DEFAULTpageCMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sNEG2POSFP0\n", No(DEFAULT_NEGtoPOSfp0));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /ZMODES=(%s,%s)\n",
	   zModeToken(DEFAULTzModeCMP), zModeToken(DEFAULTzModeCMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sLOCATION\n", No(DEFAULTlocationCMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sVALUE\n", No(DEFAULTvalueCMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /REPORT=%s\n",
	   StatusCodeReportOptions(REPORTerrorsDEFAULT,
				   REPORTwarningsDEFAULT,
				   REPORTinfosDEFAULT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sSTATISTICS\n", No(DEFAULTstatsCMP));
  WriteOut (stdout, text);
#else
  snprintf (text, (size_t) sizeof(text),
	    "    -%slog\n", No(DEFAULTlogCMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sattr\n", No(DEFAULTattrCMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%svar\n", No(DEFAULTvarCMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%snumber\n", No(DEFAULTnumberCMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%spercent\n", No(DEFAULTpctCMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%setc\n", No(DEFAULTetcCMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%spage\n", No(DEFAULTpageCMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sneg2posfp0\n", No(DEFAULT_NEGtoPOSfp0));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -zmodes \"%s,%s\"\n",
	   zModeToken(DEFAULTzModeCMP), zModeToken(DEFAULTzModeCMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%slocation\n", No(DEFAULTlocationCMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%svalue\n", No(DEFAULTvalueCMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -report %s\n",
	   StatusCodeReportOptions(REPORTerrorsDEFAULT,
				   REPORTwarningsDEFAULT,
				   REPORTinfosDEFAULT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sstatistics\n", No(DEFAULTstatsCMP));
  WriteOut (stdout, text);
#endif
  /****************************************************************************
  * CDFstats.
  ****************************************************************************/
  WriteOut (stdout, "\n");
  WriteOut (stdout, "  CDFstats\n");
#if defined(vms)
  snprintf (text, (size_t) sizeof(text),
	    "    /%sRANGE\n", No(DEFAULTrangeSTATS));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sFILL\n", No(DEFAULTfillSTATS));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sFORMAT\n", No(DEFAULTformatSTATS));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sPAGE\n", No(DEFAULTpageSTATS));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sNEG2POSFP0\n", No(DEFAULT_NEGtoPOSfp0));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /ZMODE=%s\n", zModeToken(DEFAULTzModeSTATS));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /REPORT=%s\n",
	   StatusCodeReportOptions(REPORTerrorsDEFAULT,
	   			   REPORTwarningsDEFAULT,
		  		   REPORTinfosDEFAULT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sUPDATE_VALIDS\n", No(DEFAULTupValidsSTATS));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sUPDATE_SCALES\n", No(DEFAULTupScalesSTATS));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sUPDATE_MONOTONIC\n", No(DEFAULTupMonoSTATS));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sSTATISTICS\n", No(DEFAULTstatsSTATS));
  WriteOut (stdout, text);
#else
  snprintf (text, (size_t) sizeof(text),
	    "    -%srange\n", No(DEFAULTrangeSTATS));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sfill\n", No(DEFAULTfillSTATS));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sformat\n", No(DEFAULTformatSTATS));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%spage\n", No(DEFAULTpageSTATS));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sneg2posfp0\n", No(DEFAULT_NEGtoPOSfp0));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -zmode %s\n", zModeToken(DEFAULTzModeSTATS));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -report %s\n",
	   StatusCodeReportOptions(REPORTerrorsDEFAULT,
	   			   REPORTwarningsDEFAULT,
		       		   REPORTinfosDEFAULT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%supdate_valids\n", No(DEFAULTupValidsSTATS));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%supdate_scales\n", No(DEFAULTupScalesSTATS));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%supdate_monotonic\n", No(DEFAULTupMonoSTATS));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sstatistics\n", No(DEFAULTstatsSTATS));
  WriteOut (stdout, text);
#endif
  /****************************************************************************
  * CDFinquire.
  ****************************************************************************/
  WriteOut (stdout, "\n");
  WriteOut (stdout, "  CDFinquire\n");
#if defined(vms)
  WriteOut (stdout, BOO(DEFAULTidINQ,"    /ID\n",""));
  snprintf (text, (size_t) sizeof(text),
	    "    /%sPAGE\n", No(DEFAULTpageINQ));
  WriteOut (stdout, text);
#else
  WriteOut (stdout, BOO(DEFAULTidINQ,"    -id\n",""));
  snprintf (text, (size_t) sizeof(text),
	    "    -%spage\n", No(DEFAULTpageINQ));
  WriteOut (stdout, text);
#endif
  /****************************************************************************
  * CDFdump.
  ****************************************************************************/
  WriteOut (stdout, "\n");
  WriteOut (stdout, "  CDFdump\n");
#if defined(vms)
  snprintf (text, (size_t) sizeof(text),
	    "    /%sFORMAT\n", No(DEFAULTformatDUMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /DUMP=ALL\n");
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sHEADER\n", No(DEFAULTheaderDUMP));
  WriteOut (stdout, text);
#else
  snprintf (text, (size_t) sizeof(text),
	    "    -%sformat\n", No(DEFAULTformatDUMP));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -dump all\n");
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sheader\n", No(DEFAULTheaderDUMP));
  WriteOut (stdout, text);
#endif
  /****************************************************************************
  * SkeletonCDF.
  ****************************************************************************/
  WriteOut (stdout, "\n");
  WriteOut (stdout, "  SkeletonCDF\n");
#if defined(vms)
  snprintf (text, (size_t) sizeof(text),
	    "    /%sLOG\n", No(DEFAULTlogSKT2CDF));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sDELETE\n", No(DEFAULTdeleteSKT2CDF));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sFILLVAL\n", No(DEFAULTfillvalSKT2CDF));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sNEG2POSFP0\n", No(DEFAULT_NEGtoPOSfp0));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /ZMODE=%s\n", zModeToken(DEFAULTzModeCDF2SKT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sSTATISTICS\n", No(DEFAULTstatsSTATS));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /REPORT=%s\n",
	   StatusCodeReportOptions(REPORTerrorsDEFAULT,
				   REPORTwarningsDEFAULT,
				   REPORTinfosDEFAULT));
  WriteOut (stdout, text);
#else
  snprintf (text, (size_t) sizeof(text),
	    "    -%slog\n", No(DEFAULTlogSKT2CDF));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sdelete\n", No(DEFAULTdeleteSKT2CDF));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sfillval\n", No(DEFAULTfillvalSKT2CDF));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sneg2posfp0\n", No(DEFAULT_NEGtoPOSfp0));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -zmode %s\n", zModeToken(DEFAULTzModeCDF2SKT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sstatistics\n", No(DEFAULTstatsSTATS));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -report %s\n",
	   StatusCodeReportOptions(REPORTerrorsDEFAULT,
				   REPORTwarningsDEFAULT,
				   REPORTinfosDEFAULT));
  WriteOut (stdout, text);
#endif
  /****************************************************************************
  * SkeletonTable.
  ****************************************************************************/
  WriteOut (stdout, "\n");
  WriteOut (stdout, "  SkeletonTable\n");
#if defined(vms)
  snprintf (text, (size_t) sizeof(text),
	    "    /%sLOG\n", No(DEFAULTlogCDF2SKT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sFORMAT\n", No(DEFAULTformatCDF2SKT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /VALUES=%s\n", valuesTokens[DEFAULTvaluesCDF2SKT]);
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sNEG2POSFP0\n", No(DEFAULT_NEGtoPOSfp0));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sSCREEN\n", No(DEFAULTscreenCDF2SKT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sPAGE\n", No(DEFAULTpageCDF2SKT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /ZMODE=%s\n", zModeToken(DEFAULTzModeCDF2SKT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sSTATISTICS\n", No(DEFAULTstatsSTATS));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /REPORT=%s\n",
	   StatusCodeReportOptions(REPORTerrorsDEFAULT,
	   			   REPORTwarningsDEFAULT,
		  		   REPORTinfosDEFAULT));
  WriteOut (stdout, text);
#else
  snprintf (text, (size_t) sizeof(text),
	    "    -%slog\n", No(DEFAULTlogCDF2SKT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sformat\n", No(DEFAULTformatCDF2SKT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -values %s\n", valuesTokens[DEFAULTvaluesCDF2SKT]);
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sneg2posfp0\n", No(DEFAULT_NEGtoPOSfp0));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sscreen\n", No(DEFAULTscreenCDF2SKT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%spage\n", No(DEFAULTpageCDF2SKT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -zmode %s\n", zModeToken(DEFAULTzModeCDF2SKT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sstatistics\n", No(DEFAULTstatsSTATS));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -report %s\n",
	   StatusCodeReportOptions(REPORTerrorsDEFAULT,
	   			   REPORTwarningsDEFAULT,
		  		   REPORTinfosDEFAULT));
  WriteOut (stdout, text);
#endif
  /****************************************************************************
  * CDFedit.
  ****************************************************************************/
  WriteOut (stdout, "\n");
  WriteOut (stdout, "  CDFedit\n");
#if defined(vms)
  snprintf (text, (size_t) sizeof(text),
	    "    /%sBROWSE\n", No(DEFAULTbrowseEDIT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /ZMODE=%s\n", zModeToken(DEFAULTzModeEDIT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sFORMAT\n", No(DEFAULTformatEDIT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sPROMPT\n", No(DEFAULTpromptEDIT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sNEG2POSFP0\n", No(DEFAULT_NEGtoPOSfp0));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /REPORT=%s\n",
	   StatusCodeReportOptions(REPORTerrorsDEFAULT,
				   REPORTwarningsDEFAULT,
				   REPORTinfosDEFAULT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sSTATISTICS\n", No(DEFAULTstatsEDIT));
  WriteOut (stdout, text);
#else
  snprintf (text, (size_t) sizeof(text),
	    "    -%sbrowse\n", No(DEFAULTbrowseEDIT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -zmode %s\n", zModeToken(DEFAULTzModeEDIT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sformat\n", No(DEFAULTformatEDIT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sprompt\n", No(DEFAULTpromptEDIT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sneg2posfp0\n", No(DEFAULT_NEGtoPOSfp0));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -report %s\n",
	   StatusCodeReportOptions(REPORTerrorsDEFAULT,
				   REPORTwarningsDEFAULT,
				   REPORTinfosDEFAULT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sstatistics\n", No(DEFAULTstatsEDIT));
  WriteOut (stdout, text);
#endif
  /****************************************************************************
  * CDFexport.
  ****************************************************************************/
  WriteOut (stdout, "\n");
  WriteOut (stdout, "  CDFexport\n");
#if defined(vms)
  CDFexportVMS ();
#else
  CDFexportNotVMS ();
#endif

#endif
 return;

}

/******************************************************************************
* CDFexportVMS.
******************************************************************************/

#if !defined(mac) && !defined(win32)
static void CDFexportVMS () {
  char text[MAX_SCREENLINE_LEN+1];
  snprintf (text, (size_t) sizeof(text),
	    "    /%sSIMPLE\n", No(DEFAULTsimpleEXPORT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sPROMPT\n",
	   No(BOO(DEFAULTsimpleEXPORT,DEFAULTpromptSIMPLE,
				      DEFAULTpromptEXPORT)));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /ZMODE=%s\n",
	   zModeToken(BOO(DEFAULTsimpleEXPORT,DEFAULTzModeSIMPLE,
					      DEFAULTzModeEXPORT)));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sNEG2POSFP0\n", No(DEFAULT_NEGtoPOSfp0));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /REPORT=%s\n",
	   StatusCodeReportOptions(REPORTerrorsDEFAULT,
				   REPORTwarningsDEFAULT,
				   REPORTinfosDEFAULT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    /%sSTATISTICS\n", No(DEFAULTstatsEXPORT));
  WriteOut (stdout, text);
#if DEFAULTsimpleEXPORT
  CDFsimpleInitialVMS ();
#else
  CDFexportInitialVMS ();
#endif
  return;
}
#endif

/******************************************************************************
* CDFexportInitialVMS.
******************************************************************************/

#if !defined(mac) && !defined(win32)
static void CDFexportInitialVMS () {
  char text[MAX_SCREENLINE_LEN+1];
  snprintf (text, (size_t) sizeof(text),
	    "    /INITIAL=(%sFILTER,%sFILLS,%sFORMAT,%sFILLVAL,%sVALIDMIN,",
	    No(DEFAULTeachFilterEXPORT), No(DEFAULTfillsEXPORT),
	    No(DEFAULTformatEXPORT), No(DEFAULTfillvalEXPORT),
	    No(DEFAULTvalidminEXPORT));
  snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	    "%sPREALLOCATE,\n", No(DEFAULTpreAllocateEXPORT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "              %sVALIDMAX,%sMONOTON,%sRECORD,%sINDICES,",
	    No(DEFAULTvalidmaxEXPORT), No(DEFAULTmonotonEXPORT),
	    No(DEFAULTrecordEXPORT), No(DEFAULTindicesEXPORT));
  snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	    "%sOUTPUT,%sDELETE,\n",
	    No(DEFAULToutputEXPORT), No(DEFAULTdeleteEXPORT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "              %sEXCLUSIVE,%s,%s,%s,%s,%s%sHEADING)\n",
	    No(DEFAULTexclusiveEXPORT), epochToken(DEFAULTepochEXPORT),
	    BOO(DEFAULTsingleEXPORT,"SINGLE","MULTI"),
	    BOO(DEFAULTnetworkEXPORT,"NETWORK","HOST"),
	    BOO(DEFAULThorizontalEXPORT,"HORIZONTAL","VERTICAL"),
	    majorityToken(DEFAULTmajorityEXPORT), No(DEFAULTheadingEXPORT));
  WriteOut (stdout, text);
  return;
}
#endif

/******************************************************************************
* CDFsimpleInitialVMS.
******************************************************************************/

#if !defined(mac) && !defined(win32)
static void CDFsimpleInitialVMS () {
  char text[MAX_SCREENLINE_LEN+1];
  snprintf (text, (size_t) sizeof(text),
	    "    /INITIAL=(%sFORMAT,%sRECORD,%sINDICES,%sOUTPUT,\n",
	    No(DEFAULTformatSIMPLE), No(DEFAULTrecordSIMPLE),
	    No(DEFAULTindicesSIMPLE), No(DEFAULToutputSIMPLE));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "              %s,%s,%s%sHEADING)\n",
	    epochToken(DEFAULTepochSIMPLE),
	    BOO(DEFAULThorizontalSIMPLE,"HORIZONTAL","VERTICAL"),
	    majorityToken(DEFAULTmajoritySIMPLE), No(DEFAULTheadingSIMPLE));
  WriteOut (stdout, text);
  return;
}
#endif

/******************************************************************************
* CDFexportNotVMS.
******************************************************************************/

#if !defined(mac) && !defined(win32)
static void CDFexportNotVMS () {
  char text[MAX_SCREENLINE_LEN+1];
  snprintf (text, (size_t) sizeof(text),
	    "    -%ssimple\n", No(DEFAULTsimpleEXPORT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sprompt\n",
	   No(BOO(DEFAULTsimpleEXPORT,DEFAULTpromptSIMPLE,
				      DEFAULTpromptEXPORT)));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -zmode %s\n",
	   zModeToken(BOO(DEFAULTsimpleEXPORT,DEFAULTzModeSIMPLE,
					      DEFAULTzModeEXPORT)));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sneg2posfp0\n", No(DEFAULT_NEGtoPOSfp0));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -report %s\n",
	   StatusCodeReportOptions(REPORTerrorsDEFAULT,
				   REPORTwarningsDEFAULT,
				   REPORTinfosDEFAULT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "    -%sstatistics\n", No(DEFAULTstatsEXPORT));
  WriteOut (stdout, text);
#if DEFAULTsimpleEXPORT
  CDFsimpleInitialNotVMS ();
#else
  CDFexportInitialNotVMS ();
#endif
  return;
}
#endif

/******************************************************************************
* CDFexportInitialNotVMS.
******************************************************************************/

#if !defined(mac) && !defined(win32)
static void CDFexportInitialNotVMS () {
  char text[MAX_SCREENLINE_LEN+1];
  snprintf (text, (size_t) sizeof(text),
	    "    -initial \"%sfilter,%sfills,%sformat,%sfillval,%svalidmin,",
	    No(DEFAULTeachFilterEXPORT), No(DEFAULTfillsEXPORT),
	    No(DEFAULTformatEXPORT), No(DEFAULTfillvalEXPORT),
	    No(DEFAULTvalidminEXPORT));
  snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	    "%spreallocate,\n", No(DEFAULTpreAllocateEXPORT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "              %svalidmax,%smonoton,%srecord,%sindices,",
	    No(DEFAULTvalidmaxEXPORT), No(DEFAULTmonotonEXPORT),
	    No(DEFAULTrecordEXPORT), No(DEFAULTindicesEXPORT));
  snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	    "%soutput,%sdelete,\n",
	    No(DEFAULToutputEXPORT), No(DEFAULTdeleteEXPORT));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "              %sexclusive,%s,%s,%s,%s,%s%sheading\"\n",
	    No(DEFAULTexclusiveEXPORT), epochToken(DEFAULTepochEXPORT),
	    BOO(DEFAULTsingleEXPORT,"single","multi"),
	    BOO(DEFAULTnetworkEXPORT,"network","host"),
	    BOO(DEFAULThorizontalEXPORT,"horizontal","vertical"),
	    majorityToken(DEFAULTmajorityEXPORT), No(DEFAULTheadingEXPORT));
  WriteOut (stdout, text);
  return;
}
#endif

/******************************************************************************
* CDFsimpleInitialNotVMS.
******************************************************************************/

#if !defined(mac) && !defined(win32)
static void CDFsimpleInitialNotVMS () {
  char text[MAX_SCREENLINE_LEN+1];
  snprintf (text, (size_t) sizeof(text),
	    "    -initial \"%sformat,%srecord,%sindices,%soutput,\n",
	    No(DEFAULTformatSIMPLE), No(DEFAULTrecordSIMPLE),
	    No(DEFAULTindicesSIMPLE), No(DEFAULToutputSIMPLE));
  WriteOut (stdout, text);
  snprintf (text, (size_t) sizeof(text),
	    "              %s,%s,%s%sheading\"\n",
	    epochToken(DEFAULTepochSIMPLE),
	    BOO(DEFAULThorizontalSIMPLE,"horizontal","vertical"),
	    majorityToken(DEFAULTmajoritySIMPLE), No(DEFAULTheadingSIMPLE));
  WriteOut (stdout, text);
  return;
}
#endif

/******************************************************************************
* zModeToken.
******************************************************************************/

static char *zModeToken (zMode)
int zMode;
{
  switch (zMode) {
    case zMODEoff: return "0";
    case zMODEon1: return "1";
    case zMODEon2: return "2";
  }
  return "?";
}

/******************************************************************************
* majorityToken.
******************************************************************************/

static char *majorityToken (majority)
int majority;
{
  switch (majority) {
    case INPUT_MAJOR: return "";
    case ROW_MAJOR:
#if defined(vms)
      return "ROW,";
#else
      return "row,";
#endif
    case COLUMN_MAJOR:
#if defined(vms)
      return "COLUMN,";
#else
      return "column,";
#endif
  }
  return "?";
}

/******************************************************************************
* epochToken.
******************************************************************************/

static char *epochToken (epochStyle)
int epochStyle;
{
  switch (epochStyle) {
    case EPOCH0_STYLE:
#if defined(vms)
      return "EPOCH";
#else
      return "epoch";
#endif
    case EPOCH1_STYLE:
#if defined(vms)
      return "EPOCH1";
#else
      return "epoch1";
#endif
    case EPOCH2_STYLE:
#if defined(vms)
      return "EPOCH2";
#else
      return "epoch2";
#endif
    case EPOCH3_STYLE:
#if defined(vms)
      return "EPOCH3";
#else
      return "epoch3";
#endif
    case EPOCHf_STYLE:
#if defined(vms)
      return "EPOCHf";
#else
      return "epochf";
#endif
    case EPOCHx_STYLE:
#if defined(vms)
      return "EPOCHx";
#else
      return "epochx";
#endif
  }
  return "?";
}

/******************************************************************************
* No.
******************************************************************************/

static char *No (state)
Logical state;
{
  switch (state) {
    case TRUE: return "";
#if defined(vms)
    case FALSE: return "NO";
#else
    case FALSE: return "no";
#endif
  }
  return "?";
}

/******************************************************************************
* InquireQOPs.
*    Returns TRUE if execution should continue.
******************************************************************************/

#if defined(mac)
Logical InquireQOPs (argC, argV)
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
  UserItemUPP OutlineDefaultButtonUPP;
#endif
  short itemN, iType;
  static Logical first = TRUE;
  char cancelTitle[] = "Cancel";
  static Logical pageOutput = DEFAULTpageINQ;
  static Logical showId = DEFAULTidINQ;
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
  if (pageOutput) SetCtlValue (controlHs[PAGEin], 1);
  if (showId) SetCtlValue (controlHs[IDin], 1);
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
		int n;
		/**********************************************************************
		* Get the value of each control.
		**********************************************************************/
		pageOutput = GetCtlValue (controlHs[PAGEin]);
		/**********************************************************************
		* Build argc/argv.
		**********************************************************************/
        *argC = 3;
        *argV = (char **) cdf_AllocateMemory ((size_t)*argC * sizeof(char *),
					  FatalError);
		n = 0;
		MAKEstrARGv (argV, n, pgmName)
		MAKEstrARGv (argV, n, "-id")
		MAKEbooARGv (argV, n, pageOutput, "-page", "-nopage")
		/**********************************************************************
		* Close the dialog and return.
		**********************************************************************/
        CloseDialog (dialogP);
#ifdef __MWERKS__
        DisposeRoutineDescriptor (FilterDialogQOPsoUPP);
		DisposeRoutineDescriptor (OutlineDefaultButtonUPP);
#endif
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
		CloseDialog (dialogP);
#ifdef __MWERKS__
        DisposeRoutineDescriptor (FilterDialogQOPsoUPP);
		DisposeRoutineDescriptor (OutlineDefaultButtonUPP);
#endif
        return TRUE;
      }
      /************************************************************************
      * Cancel.
      ************************************************************************/
      case CANCELin:
        CloseDialog (dialogP);
#ifdef __MWERKS__
        DisposeRoutineDescriptor (FilterDialogQOPsoUPP);
		DisposeRoutineDescriptor (OutlineDefaultButtonUPP);
#endif
        return FALSE;
      /************************************************************************
      * Check boxes.
      ************************************************************************/
      case PAGEin:
        SetCtlValue (controlHs[itemN], BOO(GetCtlValue(controlHs[itemN]),0,1));
        break;
    }
  }
}
#endif
