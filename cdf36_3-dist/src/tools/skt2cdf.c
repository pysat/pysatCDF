/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF							SkeletonCDF.
*
*  Version 1.5c, 18-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  18-May-92, J Love     Original version.
*   V1.1  18-Sep-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V1.2  16-Nov-92, J Love     Check for strings split across two lines.
*   V1.3  24-Jan-94, J Love     CDF V2.4.
*   V1.3a  6-Feb-94, J Love     DEC Alpha/OpenVMS port.
*   V1.4  13-Dec-94, J Love     CDF V2.5.
*   V1.4a 11-Jan-95, J Love	Uppercase file extensions.
*   V1.4b 28-Feb-95, J Love	Pass `char' as `int'.
*   V1.4c  6-Apr-95, J Love	POSIX.
*   V1.4d 11-May-95, J Love	EPOCH styles.
*   V1.4e  9-Jun-95, J Love	catchrX.
*   V1.4f 19-Sep-95, J Love	CHECKforABORTso.
*   V1.4g 28-Sep-95, J Love	Macintosh dialog filtering.  Outline default
*				button.
*   V1.5  27-Sep-96, J Love	CDF V2.6.
*   V1.5a 21-Feb-97, J Love	Removed RICE.
*   V1.5b  3-Sep-97, J Love	Fixed zMode/2.
*   V1.5c 18-Nov-97, J Love	Windows NT/Visual C++.
*   V1.5d 22-Jul-98, M Liu 	Delete an invalid existing CDF.
*   V1.5e 26-Jue-01, M Liu      Handle -delete option properly if no existing 
*                               CDF.
*   V2.0  19-May-05, M Liu      Allow space(s) in the CDF file name path.
*   V3.3  04-Apr-11, M Liu      Modified to hanlde TT2000 epoch.
******************************************************************************/

#include "skt2cdf.h"

/******************************************************************************
* Increased stack size for Borland C on IBM PC.
******************************************************************************/

#if defined(BORLANDC)
extern unsigned _stklen = BORLANDC_STACK_SIZE;
#endif

/******************************************************************************
* Local function prototypes.
******************************************************************************/

static Logical CloseTheCDF PROTOARGs((void));

/******************************************************************************
* Global variables.
******************************************************************************/

FILE *SKTfp;
FILE *NRVfp;
Logical done;                   /* Set TRUE when `#end' encountered. */
Logical deleteIFexists;
Logical useFillval;
Logical negToPosFp0;
Logical mLog;
Logical warning;
char line[MAX_SKTLINE_LEN+1+1];         /* + NL + NUL */
int lineN;                              /* Line number in skeleton table. */
char CDFpath[DU_MAX_PATH_LEN+1];
char *stream, *token, *string, *eValue, *location;
size_t streamZ, tokenZ, stringZ, eValueZ, locationZ;
char epoch[EPOCH_STRING_LEN+1];
char epoch2[EPOCH16_STRING_LEN+1];
char tt2000[TT2000_3_STRING_LEN+1];
char charStr[MAX_SKTLINE_LEN+1+1];
char charDelimiter;
int  delimiterNum, numDelimiters;
Logical inString;
char stringDelim;
int stringLineN;
Logical hyphenORendBraceNext;
Logical startBraceFound;
Logical locationStarted;
int epochCount, charCount;
int dimCount;
int varyCount;
enum searchForENUM searchFor;
enum nextItemENUM nextItem;
Logical zSection;		/* In the zVariable section? */
Logical report[3];
Logical displayStats;
long zMode;
long workingCache, stageCache, compressCache;
static char creCDF[] = "creating the CDF";
static char creG[] = "creating a gAttribute";
static char creV[] = "creating a vAttribute";
static char creZ[] = "creating a zVariable";
static char creR[] = "creating an rVariable";
static char delCDF[] = "deleting the CDF";
static char padZ[] = "reading/writing zVariable pad value";
static char padR[] = "reading/writing rVariable pad value";
static char selZ[] = "selecting the current zEntry number";
static char selR[] = "selecting the current rEntry number";
static char selV[] = "selecting the current vAttribute";
static char wriG[] = "writing a gAttribute gEntry";
static char wriVZ[] = "writing a vAttribute zEntry";
static char wriVR[] = "writing a vAttribute rEntry";
static char selRIZ[] = "selecting the current record/indices for a zVariable";
static char selRIR[] = "selecting the current record/indices for rVariables";
static char wriZ[] = "writing a zVariable value";
static char wriR[] = "writing an rVariable value";
static char cloCDF[] = "closing the CDF";
static char cmpCDF[] = "compressing the CDF";
static char cksCDF[] = "checksuming the CDF";
static char libCDF[] = "reading the CDF library version";
static char cmpVAR[] = "compressing the Variable";
static char spaVAR[] = "setting the record sparseness for the Variable";
static char blkVAR[] = "setting the blocking factor for the Variable";
static char padVAR[] = "setting the pad value for the Variable";

/******************************************************************************
* Main.
******************************************************************************/

#if !defined(win32) || (defined(win32) && defined(ALONE))
MAIN {
  Logical success = TRUE;
  strcpyX (pgmName, "SkeletonCDF", MAX_PROGRAM_NAME_LEN);
#if defined(mac)
  MacExecuteSO (CreateSkeletonCDF, CreateSkeletonQOPs);
#else
  success = CreateSkeletonCDF (argc, argv);
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
* CreateSkeletonCDF.
******************************************************************************/

Logical CreateSkeletonCDF (argC, argV)
int argC;
char *argV[];
{
  char SKTpath[DU_MAX_PATH_LEN+1];
  char SKTpathX[DU_MAX_PATH_LEN+1];
  char NRVpathX[DU_MAX_PATH_LEN+1];
  Logical comment;
  int i;
  QOP *qop;
  Logical qopError = FALSE;
  static char *validQuals[] = {
    "cdf", "log", "nolog", "delete", "nodelete", "fillval", "nofillval",
    "report", "neg2posfp0", "noneg2posfp0", "about", "zmode", "statistics",
    "nostatistics", "cache", "warning", "nowarning", "backward", NULL
  };
  static int optRequired[] = {
    TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
    TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, 0
  };
  static char *reportTokens[] = { "errors", "warnings", "informationals" };

  /****************************************************************************
  * Parse qualifiers/options/parameters.
  ****************************************************************************/
  switch (argC) {
    case 1:
      PageOLH ("skt2cdf.olh", argV[0]);
      return TRUE;
    case 2:
      if (strcmp(argV[1],"-java") == 0) {
        pagingOn = FALSE;
        PageOLH ("skt2cdfj.olh", argV[0]);
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
      * Skeleton table pathname.
      ************************************************************************/
      if (qop->Nparms < 1) {
		DisplayError ("Missing parameter.");
		qopError = TRUE;
      }
      else
		strcpyX (SKTpath, qop->parms[SKTPATHparm], DU_MAX_PATH_LEN);
      /************************************************************************
      * Check for CDF pathname qualifier.
      ************************************************************************/
      if (qop->qualEntered[CDFPATHqual]) {
	if (!ValidateQual(qop->qualOpt[CDFPATHqual], validQuals))
		strcpyX (CDFpath, qop->qualOpt[CDFPATHqual], DU_MAX_PATH_LEN);
	else {
		DisplayError ("Invalid \"cdf\" qualifier.");
		qopError = TRUE;
	}
       } else
		CDFpath[0] = NUL;
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
      * Check for `log', `delete', `fillval', `neg2posfp0', and `statistics'
      * qualifiers.
      ************************************************************************/
      qopError = qopError | !TFqualifier(qop,&mLog,LOGqual,NOLOGqual,
					 DEFAULTlogSKT2CDF,"log");
      qopError = qopError | !TFqualifier(qop,&deleteIFexists,DELqual,NODELqual,
					 DEFAULTdeleteSKT2CDF,"delete");
      qopError = qopError | !TFqualifier(qop,&useFillval,FILLVALqual,
					 NOFILLVALqual,DEFAULTfillvalSKT2CDF,
					 "fillval");
      qopError = qopError | !TFqualifier(qop,&negToPosFp0,NEG2POSqual,
					 NONEG2POSqual,DEFAULT_NEGtoPOSfp0,
					 "neg2posfp0");
      qopError = qopError | !TFqualifier(qop,&displayStats,STATSqual,
					 NOSTATSqual,DEFAULTstatsSKT2CDF,
					 "statistics");
      qopError = qopError | !TFqualifier(qop,&warning,WARNINGqual,
					 NOWARNINGqual,DEFAULTwarningSKT2CDF,
					 "warning");
      /***********************************************************************
      * Check for `cache' qualifier.
      ***********************************************************************/
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
      * Check for zMode qualifier.
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
        zMode = DEFAULTzModeSKT2CDF;
      /************************************************************************
      * Check for `backward' qualifier.
      ************************************************************************/
      if (qop->qualEntered[BACKWARDqual]) {
        CDFsetFileBackward(BACKWARDFILEon);
      }
      /************************************************************************
      * Free QOP memory and check for an error.
      ************************************************************************/
      cdf_FreeMemory (qop, FatalError);
      if (qopError) return FALSE;
      break;
  }
  /****************************************************************************
  * Initialize.
  ****************************************************************************/
  done = FALSE;
  lineN = 0;
  inString = FALSE;
  /****************************************************************************
  * Open skeleton table. Try several combinations for the skeleton table path.
  * If the table file contains the ".skt" or ".SKT" extension, then uses it as
  * is. Otherwise, appends ".skt", ".skeleton_table", ".SKT", ".skt;1" and
  * ".SKT;1" to the table file in sequence to see if it exists. The last try
  * is to use the table file name without extension after all preceeding
  * combinations fail.
  ****************************************************************************/
  ExpandPath (SKTpath, SKTpathX);
  if (EndsWithIgCase(SKTpathX, ".skt") ||
      EndsWithIgCase(SKTpathX, ".skt;1")) {
    SKTfp = fopen (SKTpathX, "r");
    if (SKTfp == NULL) {
      WriteOut (stdout, "Unable to open skeleton table \"");
      WriteOut (stdout, SKTpath);
      WriteOut (stdout, "\".\n");
      return FALSE;
    }  
  } else {
    strcatX (SKTpathX, ".skt", DU_MAX_PATH_LEN);
    SKTfp = fopen (SKTpathX, "r");
    if (SKTfp == NULL) {
      ExpandPath (SKTpath, SKTpathX);
      strcatX (SKTpathX, ".skeleton_table", DU_MAX_PATH_LEN);
      SKTfp = fopen (SKTpathX, "r");
      if (SKTfp == NULL) {
        ExpandPath (SKTpath, SKTpathX);
        strcatX (SKTpathX, ".SKT", DU_MAX_PATH_LEN);
        SKTfp = fopen (SKTpathX, "r");
        if (SKTfp == NULL) {
          ExpandPath (SKTpath, SKTpathX);
          strcatX (SKTpathX, ".skt;1", DU_MAX_PATH_LEN);
          SKTfp = fopen (SKTpathX, "r");
	  if (SKTfp == NULL) {
	    ExpandPath (SKTpath, SKTpathX);
	    strcatX (SKTpathX, ".SKT;1", DU_MAX_PATH_LEN);
	    SKTfp = fopen (SKTpathX, "r");
	    if (SKTfp == NULL) {
	      ExpandPath (SKTpath, SKTpathX);
	      SKTfp = fopen (SKTpathX, "r");
	      if (SKTfp == NULL) {
	        WriteOut (stdout, "Unable to open skeleton table \"");
	        WriteOut (stdout, SKTpath);
	        WriteOut (stdout, "\".\n");
	        return FALSE;
              }
            }
	  }
	}
      }
    }
  }
  WriteOut (stdout, "Making a cdf from skeleton table: \"");
  WriteOut (stdout, SKTpathX);
  WriteOut (stdout, "\"");
  /****************************************************************************
  * Open NRV file (may not exist).
  ****************************************************************************/
  ExpandPath (SKTpath, NRVpathX);
  strcatX (NRVpathX, ".nrv", DU_MAX_PATH_LEN);
  NRVfp = fopen (NRVpathX, "r");
  if (NRVfp == NULL) {
    ExpandPath (SKTpath, NRVpathX);
    strcatX (NRVpathX, ".nrv_file", DU_MAX_PATH_LEN);
    NRVfp = fopen (NRVpathX, "r");
    if (NRVfp == NULL) {
      ExpandPath (SKTpath, NRVpathX);
      strcatX (NRVpathX, ".NRV", DU_MAX_PATH_LEN);
      NRVfp = fopen (NRVpathX, "r");
      if (NRVfp == NULL) {
		ExpandPath (SKTpath, NRVpathX);
		strcatX (NRVpathX, ".nrv;1", DU_MAX_PATH_LEN);
		NRVfp = fopen (NRVpathX, "r");
	if (NRVfp == NULL) {
	  ExpandPath (SKTpath, NRVpathX);
	  strcatX (NRVpathX, ".NRV;1", DU_MAX_PATH_LEN);
	  NRVfp = fopen (NRVpathX, "r");
	}
      }
    }
  }
  if (NRVfp != NULL) {
    WriteOut (stdout, "\n");
    WriteOut (stdout,
	      "***********************************************************\n");
    WriteOut (stdout,
	      "* NRV files are no longer supported (no one seemed to use *\n");
    WriteOut (stdout,
	      "* them).  If this is a problem, contact CDFSUPPORT (we'll *\n");
    WriteOut (stdout,
	      "* work out something).  The NRV file is being ignored.    *\n");
    WriteOut (stdout,
	      "***********************************************************\n");
    WriteOut (stdout, "\n");
    fclose (NRVfp);
  }
  /****************************************************************************
  * Initialize character strings which may increase in size.
  ****************************************************************************/
  AllocateGrowingStrings ();
  /****************************************************************************
  * Scan skeleton table (creating the specified CDF).
  ****************************************************************************/
  NextSearchItem (TOKEN, HEADERmark);
  while (fgets(line,MAX_SKTLINE_LEN,SKTfp) != NULL) {
    lineN++;
    for (i = 0, comment = FALSE; line[i] != NUL && !comment; i++) {
       switch (line[i]) {
	 /*********************************************************************
	 * Newline - change to a blank.
	 *********************************************************************/
	 case Nl:
           if (nextItem != CDFPATHfield) {
	     if (!CharStream((int)' ')) {
	       FreeGrowingStrings ();
	       return done;
	     }
	   } else {
	     if (!CharStream((int)'\n')) {
	       FreeGrowingStrings ();
	       return done;
	     }
	   }
	   break;
	 /*********************************************************************
	 * Exclamation point (start of a comment unless inside a string).
	 *********************************************************************/
	 case '!':
	   if (inString) {
	     if (!CharStream((int)line[i])) {
	       FreeGrowingStrings ();
	       return done;
	     }
	   }
	   else {
	     if (strstrIgCase(line, "cdf_compression") != NULL) {
		NextSearchItem (TOKEN, CDFCOMPRESSIONmark);
             } else if (strstrIgCase(line, "cdf_checksum") != NULL) {
		NextSearchItem (TOKEN, CDFCHECKSUMmark);
             } else if (strstrIgCase(line, "cdf_leapsecondlastupdated") != NULL) {
		NextSearchItem (TOKEN, CDFLEAPSECONDLASTUPDATEDmark);
             } else if (strstrIgCase(line, "var_compression") != NULL) {
		NextSearchItem (TOKEN, VARCOMPRESSIONmark);
             } else if (strstrIgCase(line, "var_sparserecords") != NULL) {
		NextSearchItem (TOKEN, VARSPARSEmark);
             } else if (strstrIgCase(line, "var_blockingfactor") != NULL) {
		NextSearchItem (TOKEN, VARBLOCKINGmark);
             } else if (strstrIgCase(line, "var_padvalue") != NULL) {
		NextSearchItem (TOKEN, VARPADVALUEmark);
             } else { 
	       if (!CharStream((int)' ')) {
	         FreeGrowingStrings ();
	         return done;
	       }
	       comment = TRUE;
	     }
	   }
	   break;
	 /*********************************************************************
	 * Something else - pass it through.
	 *********************************************************************/
	 default:
	   if (!CharStream((int)line[i])) {
	     FreeGrowingStrings ();
	     return done;
	   }
	   break;
       }
    }
    CHECKforABORTso
  }
  /****************************************************************************
  * Call `CharStream' with a blank just in case the last line of the skeleton
  * table doesn't have a terminating newline character.  This will cause the
  * `#end' token to be processed.
  ****************************************************************************/
  if (!CharStream((int)' ')) {
    FreeGrowingStrings ();
    return done;
  }
  /****************************************************************************
  * Check if an unexpected EOF occurred.
  ****************************************************************************/
  if (!done) ParseError ("Unexpected end-of-file.");
  FreeGrowingStrings ();
  return done;
}

/******************************************************************************
* CharStream.
* Returns TRUE if the scanning of the skeleton table should continue.  Returns
* FALSE if scanning is complete (either because of an error detected or the
* end of the skeleton table has been reached).
******************************************************************************/

Logical CharStream (chr)
int chr;
{
  CATchr (&stream, chr, &streamZ, BASE_STREAM_SIZE);
  switch (searchFor) {
    /**************************************************************************
    *
    **************************************************************************/
    case TOKEN:
      if ((nextItem != CDFPATHfield && Spacing(chr)) || 
          (nextItem == CDFPATHfield && chr == (int)'\n')) {
	if (token[0] != NUL)
	  if (!ItemStream(token)) return FALSE;
      }
      else
	CATchr (&token, chr, &tokenZ, BASE_TOKEN_SIZE);
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case DELIMSTRING:
      if (!Spacing(chr))
	if (!inString) {
	  stringDelim = (char) chr;
	  stringLineN = lineN;
	  inString = TRUE;
	}
	else {
	  if (lineN != stringLineN) {
	    ParseError ("String/substring split across lines.");
	    return FALSE;
	  }
	  if ((char) chr == stringDelim) {
	    inString = FALSE;
	    if (!ItemStream(string)) return FALSE;
	  }
	  else
	    CATchr (&string, chr, &stringZ, BASE_STRING_SIZE);
	}
      else
	if (inString) {
	  if (lineN != stringLineN) {
	    ParseError ("String/substring split across lines.");
	    return FALSE;
	  }
	  CATchr (&string, chr, &stringZ, BASE_STRING_SIZE);
	}
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case BRACEDnonChar:
      if (!Spacing(chr))
	if (startBraceFound)
	  if ((char) chr == '}') {
	    if (!ItemStream(eValue)) return FALSE;
	  }
	  else
	    CATchr (&eValue, chr, &eValueZ, BASE_EVALUE_SIZE);
	else
	  if ((char) chr == '{')
	    startBraceFound = TRUE;
	  else {
	    TEMPcharSTRING (chr, str)
	    ItemError1 ("{", str);
	    return FALSE;
	  }
      else
	if (startBraceFound) CATchr (&eValue, chr, &eValueZ, BASE_EVALUE_SIZE);
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case BRACEDchar:
      if (!Spacing(chr))
	if (!startBraceFound)
	  if ((char) chr == '{')
	    startBraceFound = TRUE;
	  else {
	    TEMPcharSTRING (chr, str)
	    ItemError1 ("{", str);
	    return FALSE;
	  }
	else
	  if (inString) {
	    if (lineN != stringLineN) {
	      ParseError ("String/substring split across lines.");
	      return FALSE;
	    }
	    if ((char) chr == stringDelim) {
	      inString = FALSE;
	      hyphenORendBraceNext = TRUE;
	    }
	    else
	      CATchr (&eValue, chr, &eValueZ, BASE_EVALUE_SIZE);
	  }
	  else
	    switch ((char)chr) {
	      case '}':
		if (!hyphenORendBraceNext) {
		  ParseError ("Unexpected closing brace (\"}\").");
		  return FALSE;
		}
		if (!ItemStream(eValue)) return FALSE;
		break;
	      case '-':
		if (hyphenORendBraceNext)
		  hyphenORendBraceNext = FALSE;
		else {
		  ItemError1 ("<delimiter>", "-");
		  return FALSE;
		}
		break;
	      default:
		if (hyphenORendBraceNext) {
		  TEMPcharSTRING (chr, str)
		  ItemError2 ("-", "}", str);
		  return FALSE;
		}
		stringDelim = (char) chr;
		stringLineN = lineN;
		inString = TRUE;
		break;
	    }
      else
	if (startBraceFound)
	  if (inString) {
	    if (lineN != stringLineN) {
	      ParseError ("String/substring split across lines.");
	      return FALSE;
	    }
	    CATchr (&eValue, chr, &eValueZ, BASE_EVALUE_SIZE);
	  }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VALLOCATION:
      if (!Spacing(chr))
	if (locationStarted) {
	  CATchr (&location, chr, &locationZ, BASE_LOCATION_SIZE);
	  if ((char) chr == ']') {
	    if (!ItemStream(location)) return FALSE;
	  }
	}
	else
	  if ((char) chr == '[' || Decimal(chr)) {
	    CATchr (&location, chr, &locationZ, BASE_LOCATION_SIZE);
	    locationStarted = TRUE;
	  }
	  else {
	    TEMPcharSTRING (chr, str)
	    ItemError2 ("[", "<record-number", str);
	    return FALSE;
	  }
      else
	if (locationStarted) CATchr (&location, chr, &locationZ,
				       BASE_LOCATION_SIZE);
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VAREPOCH:
      if (epochCount == 0) {
	if (!Spacing(chr)) {
	  catchrX (epoch, chr, EPOCH_STRING_LEN);
	  epochCount = 1;
	}
      }
      else {
	catchrX (epoch, chr, EPOCH_STRING_LEN);
	epochCount++;
	if (epochCount == EPOCH_STRING_LEN)
	  if (!ItemStream(epoch)) return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VAREPOCH16:
      if (epochCount == 0) {
        if (!Spacing(chr)) {
          catchrX (epoch2, chr, EPOCH16_STRING_LEN);
          epochCount = 1;
        }
      }
      else {
        catchrX (epoch2, chr, EPOCH16_STRING_LEN);
        epochCount++;
        if (epochCount == EPOCH16_STRING_LEN)
          if (!ItemStream(epoch2)) return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VARTT2000:
      if (epochCount == 0) {
        if (!Spacing(chr)) {
          catchrX (tt2000, chr, TT2000_3_STRING_LEN);
          epochCount = 1;
        }
      }
      else {
        catchrX (tt2000, chr, TT2000_3_STRING_LEN);
        epochCount++;
        if (epochCount == TT2000_3_STRING_LEN)
          if (!ItemStream(tt2000)) return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VARCHAR:
      if (charCount == 0) {
        if (!Spacing(chr)) {
          int ii;
          catchrX (charStr, chr, MAX_SKTLINE_LEN);
          charCount = 1;
          charDelimiter = chr;
          numDelimiters = 0;
          delimiterNum = 1;
          for (ii = 0; ii < MAX_SKTLINE_LEN; ++ii) {
            if (*(line+ii) == NUL) break;
            if (*(line+ii) == chr) ++numDelimiters;
          }
        }
      }
      else {
        catchrX (charStr, chr, MAX_SKTLINE_LEN);
        charCount++;
        if (chr == charDelimiter) ++delimiterNum;
        if (charCount == MAX_SKTLINE_LEN ||
            ((chr == charDelimiter) && (numDelimiters == delimiterNum)))
          if (!ItemStream(charStr)) return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    default:
      ParseError ("** Internal error, contact CDFSUPPORT **");
      return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* ItemStream.
*    Returns TRUE if the scanning of the skeleton table should continue.
* Returns FALSE if scanning is complete (either because of an error detected
* or the end of the skeleton table has been reached).
******************************************************************************/

Logical ItemStream (item)
char *item;
{
  CDFstatus status; Logical zVar;
  static long format, majority, encoding, zNumDims, rNumDims, entryNum,
	      vDataType, eDataType, vNumElems, recVary, dimVarys[CDF_MAX_DIMS],
	      zDimSizes[CDF_MAX_DIMS], rDimSizes[CDF_MAX_DIMS];
  static char gAttrName[CDF_ATTR_NAME_LEN256+1], vAttrName[CDF_ATTR_NAME_LEN256+1],
	      varName[CDF_VAR_NAME_LEN256+1];
  static long VARYdimVarys[CDF_MAX_DIMS] = {
    VARY, VARY, VARY, VARY, VARY, VARY, VARY, VARY, VARY, VARY
  };
  long numDims, varNum, dimSizes[CDF_MAX_DIMS];
  long *dimVarysP;	/* Points to the `dimVarys' array to be used. */
  int dN, dNt;		/* Dimension number. */
  void *padValue;
  size_t nBytes;
  /****************************************************************************
  * Based on what should be next...
  ****************************************************************************/
  switch (nextItem) {
    /**************************************************************************
    *
    **************************************************************************/
    case HEADERmark:
      if (!strcmp(item,"#header")) {
	if (mLog) WriteOut (stdout, "Reading header section...\n");
	NextSearchItem (TOKEN, CDForOUTPUTmark);
      }
      else {
	ItemError1 ("#header", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case CDForOUTPUTmark:
      if (!strcmp(item,"CDF"))
	NextSearchItem (TOKEN, NAMEmark);
      else
	if (!strcmp(item,"OUTPUT"))
	  NextSearchItem (TOKEN, FILEmark);
	else {
	  ItemError2 ("CDF", "OUTPUT", item);
	  return FALSE;
	}
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case NAMEmark:
      if (!strcmp(item,"NAME:"))
	NextSearchItem (TOKEN, CDFPATHfield);
      else
	if (!strcmp(item,"NAME"))
	  NextSearchItem (TOKEN, NAMECOLONmark);
	else {
	  ItemError2 ("NAME:", "NAME", item);
	  return FALSE;
	}
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case NAMECOLONmark:
      if (!strcmp(item,":"))
	NextSearchItem (TOKEN, CDFPATHfield);
      else {
	ItemError1 (":", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case FILEmark:
      if (!strcmp(item,"FILE"))
	NextSearchItem (TOKEN, NAMEmark);
      else {
	ItemError1 ("FILE", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case CDFPATHfield:
      if (CDFpath[0] == NUL) strcpyX (CDFpath, item, DU_MAX_PATH_LEN);
      NextSearchItem (TOKEN, DATAmark);
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case DATAmark:
      if (!strcmp(item,"DATA"))
	NextSearchItem (TOKEN, ENCODINGmark);
      else {
	ItemError1 ("DATA", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case ENCODINGmark:
      if (!strcmp(item,"ENCODING:"))
	NextSearchItem (TOKEN, ENCODINGfield);
      else
	if (!strcmp(item,"ENCODING"))
	  NextSearchItem (TOKEN, ENCODINGCOLONmark);
	else {
	  ItemError2 ("ENCODING:", "ENCODING", item);
	  return FALSE;
	}
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case ENCODINGCOLONmark:
      if (!strcmp(item,":"))
	NextSearchItem (TOKEN, ENCODINGfield);
      else {
	ItemError1 (":", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case ENCODINGfield:
      encoding = WhichEncoding (item);
      if (encoding == -1) {
	ItemError1 ("<encoding>", item);
	return FALSE;
      }
      else
	NextSearchItem (TOKEN, MAJORITYmark);
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case MAJORITYmark:
      if (!strcmp(item,"MAJORITY:"))
	NextSearchItem (TOKEN, MAJORITYfield);
      else
	if (!strcmp(item,"MAJORITY"))
	  NextSearchItem (TOKEN, MAJORITYCOLONmark);
	else {
	  ItemError2 ("MAJORITY:", "MAJORITY", item);
	  return FALSE;
	}
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case MAJORITYCOLONmark:
      if (!strcmp(item,":"))
	NextSearchItem (TOKEN, MAJORITYfield);
      else {
	ItemError1 (":", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case MAJORITYfield:
      majority = WhichMajority (item);
      if (majority == -1) {
	ItemError1 ("<majority>", item);
	return FALSE;
      }
      else
	NextSearchItem (TOKEN, FORMATmarkOrNUMVARSfield);
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case FORMATmarkOrNUMVARSfield:
      if (!strcmp(item,"FORMAT:"))
	NextSearchItem (TOKEN, FORMATfield);
      else
	if (!strcmp(item,"FORMAT"))
	  NextSearchItem (TOKEN, FORMATCOLONmark);
	else {
#if DEFAULT_TO_SINGLE_FILE
	  format = SINGLE_FILE;
#else
	  format = MULTI_FILE;
#endif
	  NextSearchItem (TOKEN, NUMGATTRSfield);  /* Assumed to be number of
						      variables/zVariables. */
	}
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case FORMATCOLONmark:
      if (!strcmp(item,":"))
	NextSearchItem (TOKEN, FORMATfield);
      else {
	ItemError1 (":", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case FORMATfield:
      format = WhichFormat (item);
      if (format == -1) {
	ItemError1 ("<format>", item);
	return FALSE;
      }
      else
	NextSearchItem (TOKEN, NUMVARSfield);
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case NUMVARSfield:
      NextSearchItem (TOKEN, NUMGATTRSfield);
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case NUMGATTRSfield:
      NextSearchItem (TOKEN, NUMVATTRSfield);
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case NUMVATTRSfield:
      NextSearchItem (TOKEN, NUMRECSfield);
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case NUMRECSfield:
      NextSearchItem (TOKEN, NUMDIMSfield);
      break;
    /**************************************************************************
    * Number of rVariable dimensions.
    **************************************************************************/
    case NUMDIMSfield:
      if (sscanf(item,"%ld",&rNumDims) == 1) {
	if (rNumDims < 0 || rNumDims > CDF_MAX_DIMS) {
	  ItemError1 ("<num-dims>", item);
	  return FALSE;
	}
	else
	  if (rNumDims > 0) {
	    dimCount = 0;
	    NextSearchItem (TOKEN, DIMSIZEfield);
	  }
	  else {
	    CDFid id;
	    if (deleteIFexists) {
		status = CDFlib (OPEN_, CDF_, CDFpath, &id,
				 DELETE_, CDF_,
				 NULL_);
		if (status != NO_SUCH_CDF )
		  if (!StatusHandlerS2C(status,delCDF)) return FALSE;
	    }
	    WriteOut (stdout, "  to  \"");
	    WriteOut (stdout, CDFpath);
	    if (!EndsWithIgCase(CDFpath, ".cdf"))
	      WriteOut (stdout, ".cdf");
	    WriteOut (stdout, "\"...\n");
	    status = CDFlib (CREATE_, CDF_, CDFpath, rNumDims,
					    rDimSizes, &id,
			     PUT_, CDF_FORMAT_, format,
				   CDF_MAJORITY_, majority,
				   CDF_ENCODING_, encoding,
			     SELECT_, CDF_NEGtoPOSfp0_MODE_, BOO(negToPosFp0,
							     NEGtoPOSfp0on,
							     NEGtoPOSfp0off),
				      CDF_CACHESIZE_, workingCache,
				      STAGE_CACHESIZE_, stageCache,
				      COMPRESS_CACHESIZE_, compressCache,
			     NULL_);
	    if (!StatusHandlerS2C(status,creCDF)) return FALSE;
	    NextSearchItem (TOKEN, GATTRmark);
	  }
      }
      else {
	ItemError1 ("<num-dims>", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    * rVariable dimension size(s).
    **************************************************************************/
    case DIMSIZEfield:
      if (sscanf(item,"%ld",&rDimSizes[dimCount]) == 1) {
	if (++dimCount == rNumDims) {
	  CDFid id;
	  if (deleteIFexists) {
		status = CDFlib (OPEN_, CDF_, CDFpath, &id,
				 DELETE_, CDF_,
				 NULL_);
		if (status != NO_SUCH_CDF)
		  if (!StatusHandlerS2C(status,delCDF)) return FALSE;
	  }
	  WriteOut (stdout, "  to  \"");
          WriteOut (stdout, CDFpath);
          if (!EndsWithIgCase(CDFpath, ".cdf"))
            WriteOut (stdout, ".cdf");
          WriteOut (stdout, "\"...\n");
	  status = CDFlib (CREATE_, CDF_, CDFpath, rNumDims,
					  rDimSizes, &id,
			   PUT_, CDF_FORMAT_, format,
				 CDF_MAJORITY_, majority,
				 CDF_ENCODING_, encoding,
			   SELECT_, CDF_NEGtoPOSfp0_MODE_, BOO(negToPosFp0,
							       NEGtoPOSfp0on,
							       NEGtoPOSfp0off),
				    CDF_CACHESIZE_, workingCache,
				    STAGE_CACHESIZE_, stageCache,
				    COMPRESS_CACHESIZE_, compressCache,
			   NULL_);
	  if (!StatusHandlerS2C(status,creCDF)) return FALSE;
	  NextSearchItem (TOKEN, GATTRmark);
	}
	else
	  NextSearchItem (TOKEN, DIMSIZEfield);
      }
      else {
	ItemError1 ("<dim-size>", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case CDFCOMPRESSIONmark:
      if (!strcmp(item,"CDF_COMPRESSION:"))
        NextSearchItem (TOKEN, CDFCOMPRESSIONfield);
      else
        if (!strcmp(item,"CDF_COMPRESSION"))
          NextSearchItem (TOKEN, CDFCOMPRESSIONCOLONmark);
        else {
          ItemError2 ("CDF_COMPRESSION:", "CDF_COMPRESSION", item);
          return FALSE;
        }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case CDFCOMPRESSIONCOLONmark:
      if (!strcmp(item,":"))
	NextSearchItem (TOKEN, CDFCOMPRESSIONfield);
      else {
	ItemError1 (":", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case CDFCOMPRESSIONfield: {
      long compression, compressionParms[CDF_MAX_PARMS];
      long version, release, increment;
      char cmpStr[MAX_SKTLINE_LEN+1+1];
      strncpy(cmpStr, item, strlen(item));
      cmpStr[strlen(item)] = (char) '\0';
      compression = WhichCompression (cmpStr, &compression, compressionParms);
      if (compression == -1) {
	ItemError1 ("cdf<compression>", item);
	return FALSE;
      }
      else
	NextSearchItem (TOKEN, GATTRmark);
      status = CDFlib (GET_, LIB_VERSION_, &version,
                             LIB_RELEASE_, &release,
                             LIB_INCREMENT_, &increment,
                       NULL_);
      if (!StatusHandlerS2C(status,libCDF)) return FALSE;
      if (!PriorTo ("2.6.0", (Int32) version, (Int32) release,
                    (Int32) increment)) {
        status = CDFlib (PUT_, CDF_COMPRESSION_, compression, compressionParms,
                         NULL_);
        if (!StatusHandlerS2C(status,cmpCDF)) return FALSE; 
      }
      break;
    }
    /**************************************************************************
    *
    **************************************************************************/
    case CDFCHECKSUMmark:
      if (!strcmp(item,"CDF_CHECKSUM:"))
        NextSearchItem (TOKEN, CDFCHECKSUMfield);
      else
        if (!strcmp(item,"CDF_CHECKSUM"))
          NextSearchItem (TOKEN, CDFCHECKSUMCOLONmark);
        else {
          ItemError2 ("CDF_CHECKSUM:", "CDF_CHECKSUM", item);
          return FALSE;
        }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case CDFCHECKSUMCOLONmark:
      if (!strcmp(item,":"))
	NextSearchItem (TOKEN, CDFCHECKSUMfield);
      else {
	ItemError1 (":", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case CDFCHECKSUMfield: {
      long checksum, version, release, increment;
      checksum = WhichChecksum (item);
      if (checksum == -1) {
	ItemError1 ("<checksum>", item);
	return FALSE;
      }
      else
	NextSearchItem (TOKEN, GATTRmark);
      status = CDFlib (GET_, LIB_VERSION_, &version,
                             LIB_RELEASE_, &release,
                             LIB_INCREMENT_, &increment,
                       NULL_);
      if (!StatusHandlerS2C(status,libCDF)) return FALSE;
      if (!PriorTo ("3.2.0", (Int32) version, (Int32) release,
                    (Int32) increment)) {  
        status = CDFlib (PUT_, CDF_CHECKSUM_, checksum,
                         NULL_);
        if (!StatusHandlerS2C(status,cksCDF)) return FALSE;
      }
      break;
    }
    /**************************************************************************
    *
    **************************************************************************/
    case CDFLEAPSECONDLASTUPDATEDmark:
      if (!strcmp(item,"CDF_LEAPSECONDLASTUPDATED:"))
        NextSearchItem (TOKEN, CDFLEAPSECONDLASTUPDATEDfield);
      else
        if (!strcmp(item,"CDF_LEAPSECONDLASTUPDATED"))
          NextSearchItem (TOKEN, CDFLEAPSECONDLASTUPDATEDCOLONmark);
        else {
          ItemError2 ("CDF_LEAPSECONDLASTUPDATED:", "CDF_LEAPSECONDLASTUPDATED", item);
          return FALSE;
        }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case CDFLEAPSECONDLASTUPDATEDCOLONmark:
      if (!strcmp(item,":"))
	NextSearchItem (TOKEN, CDFLEAPSECONDLASTUPDATEDfield);
      else {
	ItemError1 (":", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case CDFLEAPSECONDLASTUPDATEDfield: {
      long lastUpdated;
      if (sscanf(item,"%ld",&lastUpdated) != 1) {
	ItemError1 ("<leapsecondlastupdated>", item);
	return FALSE;
      } else
	NextSearchItem (TOKEN, GATTRmark);
      status = CDFlib (PUT_, CDF_LEAPSECONDLASTUPDATED_, lastUpdated,
                       NULL_);
      if (!StatusHandlerS2C(status,cksCDF)) return FALSE;
      break;
    }
    /**************************************************************************
    *
    **************************************************************************/
    case GATTRmark:
      if (!strcmp(item,"#GLOBALattributes")) {
	if (mLog)
	  WriteOut (stdout, "Reading global scope attributes section...\n");
	NextSearchItem (TOKEN, GaNAMEfieldOrVATTRmark);
      }
      else {
	ItemError1 ("#GLOBALattributes", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case GaNAMEfieldOrVATTRmark:
      if (!strcmp(item,"#VARIABLEattributes")) {
	if (mLog)
	  WriteOut (stdout,
		    "Reading variable scope attributes section...\n");
	NextSearchItem (TOKEN, VaNAMEfieldOrVARmark);
      }
      else
	if (!RecurCharStream(stream,DELIMSTRING,GaNAMEfield)) return FALSE;
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case GaNAMEfield: {
      long gAttrNum;
      strcpyX (gAttrName, item, CDF_ATTR_NAME_LEN256);
      if (!ISTPname (gAttrName) && warning)
        WriteOutFP (stdout, "Warning: Attribute name: \"%s\" not ISTP compliant... not recommended\n",
                    gAttrName);
      status = CDFlib (CREATE_, ATTR_, gAttrName, GLOBAL_SCOPE, &gAttrNum,
		       NULL_);
      if (!StatusHandlerS2C(status,creG)) return FALSE;
      NextSearchItem (TOKEN, PERIODmarkOrGa1stENTRYNUMfield);
      break;
    }
    /**************************************************************************
    *
    **************************************************************************/
    case PERIODmarkOrGa1stENTRYNUMfield:
      if (!strcmp(item,"."))
	NextSearchItem (TOKEN, GaNAMEfieldOrVATTRmark);
      else
	if (item[strlen(item)-1] != ':') {
	  ItemError1 ("<entry-num>:", item);
	  return FALSE;
	}
	else
	  if (sscanf(item,"%ld",&entryNum) == 1) {
	    entryNum--;                         /* Reference from zero (0). */
	    NextSearchItem (TOKEN, GaDATATYPEfield);
	  }
	  else {
	    ItemError1 ("<entry-num>:", item);
	    return FALSE;
	  }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case GaDATATYPEfield:
      eDataType = WhichDataType(item);
      if (eDataType == -1) {
	ItemError1 ("<data-type>", item);
	return FALSE;
      }
      else {
	if (STRINGdataType(eDataType))
	  NextSearchItem (BRACEDchar, GaVALUEfield);
	else
	  NextSearchItem (BRACEDnonChar, GaVALUEfield);
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case GaVALUEfield: {
      void *valuesB;
      long eNumElems;
      int style;
      if (!STRINGdataType(eDataType)) {
        if (TT2000dataType(eDataType)) style = TT2000_3_STYLE;
        else  style = EPOCH0_STYLE;
	if (!DecodeValues(item,eDataType,&eNumElems,&valuesB,style)) {
	  ParseError ("Illegal entry value(s).");
	  return FALSE;
	}
      }
      else {
	eNumElems = strlen (item);
	valuesB = item;
      }
      status = CDFlib (SELECT_, gENTRY_, entryNum,
		       PUT_, gENTRY_DATA_, eDataType, eNumElems, valuesB,
		       NULL_);
      if (!StatusHandlerS2C(status,wriG)) return FALSE;
      if (valuesB != item) cdf_FreeMemory (valuesB, FatalError);
      NextSearchItem (TOKEN, PERIODmarkOrGaENTRYNUMfield);
      break;
    }
    /**************************************************************************
    *
    **************************************************************************/
    case PERIODmarkOrGaENTRYNUMfield:
      if (!strcmp(item,"."))
	NextSearchItem (TOKEN, GaNAMEfieldOrVATTRmark);
      else
	if (!RecurCharStream(stream,TOKEN,GaENTRYNUMfield)) return FALSE;
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case GaENTRYNUMfield:
      if (item[strlen(item)-1] != ':') {
	ItemError1 ("<entry-num>:", item);
	return FALSE;
      }
      else
	if (sscanf(item,"%ld",&entryNum) == 1) {
	  entryNum--;                           /* Reference from zero (0). */
	  NextSearchItem (TOKEN, GaDATATYPEfieldOrBRACEmark);
	}
	else {
	  ItemError1 ("<entry-num>:", item);
	  return FALSE;
	}
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case GaDATATYPEfieldOrBRACEmark:
      if (item[0] == '{')
	if (STRINGdataType(eDataType)) {
	  if (!RecurCharStream(stream,BRACEDchar,GaVALUEfield)) return FALSE;
	}
	else {
	  if (!RecurCharStream(stream,BRACEDnonChar,GaVALUEfield)) return
								   FALSE;
	}
      else
	if (!RecurCharStream(stream,TOKEN,GaDATATYPEfield)) return FALSE;
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VaNAMEfieldOrVARmark:
      if (!strcmp(item,"#variables")) {
	if (mLog) WriteOut (stdout, "Reading rVariables section...\n");
	NextSearchItem (TOKEN, VARNAMEfieldORzVARmarkOrENDmark);
	zSection = FALSE;
      }
      else
	if (!RecurCharStream(stream,DELIMSTRING,VaNAMEfield)) return FALSE;
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VaNAMEfield: {
      long vAttrNum;
      strcpyX (vAttrName, item, CDF_ATTR_NAME_LEN256);
      if (!ISTPname (vAttrName) && warning)
        WriteOutFP (stdout, "Warning: Attribute name: \"%s\" not ISTP compliant... not recommended\n",
                    vAttrName);
      status = CDFlib (CREATE_, ATTR_, vAttrName, VARIABLE_SCOPE, &vAttrNum,
		       NULL_);
      if (!StatusHandlerS2C(status,creV)) return FALSE;
      NextSearchItem (TOKEN, VaNAMEfieldOrVARmark);
      break;
    }
    /**************************************************************************
    *
    **************************************************************************/
    case VARNAMEfieldORzVARmarkOrENDmark:
      if (!strcmp(item,"#end")) {
	if (mLog) WriteOut (stdout, "Reading end section...");
	fclose (SKTfp);
	if (!CloseTheCDF()) return FALSE;
	if (mLog) WriteOut (stdout, "success.\n");
	done = TRUE;
	return FALSE;
      }
      else
	if (!strcmp(item,"#zVariables")) {
	  if (mLog) WriteOut (stdout, "Reading zVariables section...\n");
	  NextSearchItem (TOKEN, VARNAMEfieldOrENDmark);
	  zSection = TRUE;
	}
	else
	  if (!RecurCharStream(stream,DELIMSTRING,VARNAMEfield)) return FALSE;
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VARNAMEfieldOrENDmark:
      if (!strcmp(item,"#end")) {
	if (mLog) WriteOut (stdout, "Reading end section...");
	fclose (SKTfp);
	if (!CloseTheCDF()) return FALSE;
	if (mLog) WriteOut (stdout, "success.\n");
	done = TRUE;
	return FALSE;
      }
      else
	if (!RecurCharStream(stream,DELIMSTRING,VARNAMEfield)) return FALSE;
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VARNAMEfield:
      strcpyX (varName, item, CDF_VAR_NAME_LEN256);
      NextSearchItem (TOKEN, VARDATATYPEfield);
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VARDATATYPEfield:
      vDataType = WhichDataType(item);
      if (vDataType == -1) {
	ItemError1 ("<data-type>", item);
	return FALSE;
      }
      else {
	NextSearchItem (TOKEN, VARNUMELEMSfield);
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VARNUMELEMSfield:
      if (sscanf(item,"%ld",&vNumElems) == 1)
	if (zSection)
	  NextSearchItem (TOKEN, zVARNUMDIMSfield);
	else
	  NextSearchItem (TOKEN, VARRECVARYfield);
      else {
	ItemError1 ("<num-elems>", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case zVARNUMDIMSfield:
      if (sscanf(item,"%ld",&zNumDims) == 1)
	if (zNumDims < 0 || zNumDims > CDF_MAX_DIMS) {
	  ItemError1 ("<num-dims>", item);
	  return FALSE;
	}
	else
	  if (zNumDims > 0) {
	    dimCount = 0;
	    NextSearchItem (TOKEN, zVARDIMSIZEfield);
	  }
	  else
	    NextSearchItem (TOKEN, VARRECVARYfield);
      else {
	ItemError1 ("<num-dims>", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case zVARDIMSIZEfield:
      if (sscanf(item,"%ld",&zDimSizes[dimCount]) == 1) {
	if (++dimCount == zNumDims)
	  NextSearchItem (TOKEN, VARRECVARYfield);
	else
	  NextSearchItem (TOKEN, zVARDIMSIZEfield);
      }
      else {
	ItemError1 ("<dim-size>", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VARRECVARYfield:
      if (strlen(item) > (size_t) 1) {
	char msg[MAX_MSG_LEN+1];
	snprintf (msg, (size_t) sizeof(msg),
		  "Extra character(s) after record variance (`%s').", &item[1]);
	ParseError (msg);
	return FALSE;
      }
      else {
	switch (MakeUpper((int)item[0])) {
	  case 'T':
	    recVary = VARY;
	    break;
	  case 'F':
	    recVary = NOVARY;
	    break;
	  default:
	    ItemError1 ("<rec-vary>", item);
	    return FALSE;
	}
	if (BOO(zSection,zNumDims,rNumDims) > 0) {
	  varyCount = 0;
	  NextSearchItem (TOKEN, VARDIMVARYfield);
	}
	else {
	  zVar = BOO(zSection,TRUE,(zMode > zMODEoff));
          if (!ISTPname (varName) && warning)
            WriteOutFP (stdout, "Warning: Variable name: \"%s\" not ISTP compliant... not recommended\n",
                        varName);
	  if (zVar)
	    status = CDFlib (CREATE_, zVAR_, varName, vDataType, vNumElems,
					     0L, NULL, recVary, NULL, &varNum,
			     NULL_);
	  else
	    status = CDFlib (CREATE_, rVAR_, varName, vDataType, vNumElems,
					     recVary, NULL, &varNum,
			     NULL_);
	  if (!StatusHandlerS2C(status,BOO(zVar,creZ,creR))) return FALSE;
	  /********************************************************************
	  * Check if a pad value may possibly be written with the value of the
	  * FILLVAL attribute.  If so, we want to allocate space for it now so
	  * that the dotCDF file is not fragmented when the FILLVAL attribute
	  * entry is encountered.  [There will have been intervening attribute
	  * entries written which would force this rVDR/zVDR to be wasted and a
	  * new one created when the pad value is written.]  To allocated space
	  * we simply inquired the default pad value and then immediately write
	  * it back for the variable.
	  ********************************************************************/
	  if (useFillval) {
	    nBytes = (size_t) (vNumElems * CDFelemSize(vDataType));
	    padValue = cdf_AllocateMemory (nBytes, FatalError);
	    status = CDFlib (GET_, VAR_PADVALUE(zVar), padValue,
			     PUT_, VAR_PADVALUE(zVar), padValue,
			     NULL_);
	    if (!StatusHandlerS2C(status,BOO(zVar,padZ,padR))) {
	      cdf_FreeMemory (padValue, FatalError);
	      return FALSE;
	    }
	    cdf_FreeMemory (padValue, FatalError);
	  }
	  status = CDFlib (SELECT_, BOO(zVar,zENTRY_,rENTRY_), varNum,
			   NULL_);
	  if (!StatusHandlerS2C(status,BOO(zVar,selZ,selR))) return FALSE;
	  NextSearchItem (TOKEN, VaNAMEfieldOrPERIODmark);
	}
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VARDIMVARYfield:
      if (strlen(item) > (size_t) 1) {
	char msg[MAX_MSG_LEN+1];
	snprintf (msg, (size_t) sizeof(msg),
		  "Extra character(s) after dimension variance (`%s').",
		  &item[1]);
	ParseError (msg);
	return FALSE;
      }
      else {
	switch (MakeUpper((int)item[0])) {
	  case 'T':
	    dimVarys[varyCount] = VARY;
	    break;
	  case 'F':
	    dimVarys[varyCount] = NOVARY;
	    break;
	  default:
	    ItemError1 ("<dim-vary>", item);
	    return FALSE;
	}
	if (++varyCount == BOO(zSection,zNumDims,rNumDims)) {
	  if (zSection) {
	    zVar = TRUE;
	    numDims = zNumDims;
	    for (dN = 0; dN < zNumDims; dN++) dimSizes[dN] = zDimSizes[dN];
	    dimVarysP = dimVarys;
	  }
	  else {
	    switch (zMode) {
	      case zMODEoff:
		zVar = FALSE;
		break;
	      case zMODEon1:
		zVar = TRUE;
		numDims = rNumDims;
		for (dN = 0; dN < rNumDims; dN++) dimSizes[dN] = rDimSizes[dN];
		dimVarysP = dimVarys;
		break;
	      case zMODEon2:
		zVar = TRUE;
		for (dN = 0, numDims = 0; dN < rNumDims; dN++) {
		   if (dimVarys[dN]) dimSizes[(int)numDims++] = rDimSizes[dN];
		}
		dimVarysP = VARYdimVarys;
		break;
	    }
	  }
	  if (zVar)
	    status = CDFlib (CREATE_, zVAR_, varName, vDataType, vNumElems,
					     numDims, dimSizes, recVary,
					     dimVarysP, &varNum,
			     NULL_);
	  else
	    status = CDFlib (CREATE_, rVAR_, varName, vDataType, vNumElems,
					     recVary, dimVarys, &varNum,
			     NULL_);
	  if (!StatusHandlerS2C(status,BOO(zVar,creZ,creR))) return FALSE;
	  /********************************************************************
	  * Check if a pad value may possibly be written with the value of the
	  * FILLVAL attribute.  If so, we want to allocate space for it now so
	  * that the `.cdf' file is not fragmented when the FILLVAL attribute
	  * entry is encountered.  [There will have been intervening attribute
	  * entries written which would force this rVDR/zVDR to be wasted and a
	  * new one created when the pad value is written.]  To allocated space
	  * we simply inquired the default pad value and then immediately write
	  * it back for the variable.
	  ********************************************************************/
	  if (useFillval) {
	    nBytes = (size_t) (vNumElems * CDFelemSize(vDataType));
	    padValue = cdf_AllocateMemory (nBytes, FatalError);
	    status = CDFlib (GET_, VAR_PADVALUE(zVar), padValue,
			     PUT_, VAR_PADVALUE(zVar), padValue,
			     NULL_);
	    if (!StatusHandlerS2C(status,BOO(zVar,padZ,padR))) {
	      cdf_FreeMemory (padValue, FatalError);
	      return FALSE;
	    }
	    cdf_FreeMemory (padValue, FatalError);
	  }
	  status = CDFlib (SELECT_, BOO(zVar,zENTRY_,rENTRY_), varNum,
			   NULL_);
	  if (!StatusHandlerS2C(status,BOO(zVar,selZ,selR))) return FALSE;
	  NextSearchItem (TOKEN, VaNAMEfieldOrPERIODmark);
	}
	else
	  NextSearchItem (TOKEN, VARDIMVARYfield);
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VaNAMEfieldOrPERIODmark:
      if (!strcmp(item,"."))
	if (zSection)
	  NextSearchItem (TOKEN, LOCfieldOrVARNAMEfieldOrENDmark);
	else
	  NextSearchItem (TOKEN, LOCfieldOrVARNAMEfieldORzVARmarkOrENDmark);
      else
	if (!RecurCharStream(stream,DELIMSTRING,VaeNAMEfield)) return FALSE;
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VaeNAMEfield:
      strcpyX (vAttrName, item, CDF_ATTR_NAME_LEN256);
      status = CDFlib (SELECT_, ATTR_NAME_, vAttrName,
		       NULL_);
      if (!StatusHandlerS2C(status,selV)) return FALSE;
      NextSearchItem (TOKEN, VaeDATATYPEfield);
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VaeDATATYPEfield:
      eDataType = WhichDataType(item);
      if (eDataType == -1) {
	ItemError1 ("<data-type>", item);
	return FALSE;
      }
      else {
	if (STRINGdataType(eDataType))
	  NextSearchItem (BRACEDchar, VaeVALUEfield);
	else
	  NextSearchItem (BRACEDnonChar, VaeVALUEfield);
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VaeVALUEfield: {
      void *eValueB;
      long eNumElems;
      int style;
      if (!STRINGdataType(eDataType)) {
        if (TT2000dataType(eDataType)) style = TT2000_3_STYLE;
        else style = EPOCH0_STYLE;
	if (!DecodeValues(item,eDataType,&eNumElems,&eValueB,style)) {
	  ParseError ("Illegal entry value(s).");
	  return FALSE;
	}
      }
      else {
	eNumElems = strlen (item);
	eValueB = item;
      }
      zVar = BOO(zSection,TRUE,(zMode > zMODEoff));
      status = CDFlib (PUT_, BOO(zVar,zENTRY_DATA_,rENTRY_DATA_), eDataType,
								  eNumElems,
								  eValueB,
		       NULL_);
      if (!StatusHandlerS2C(status,BOO(zVar,wriVZ,wriVR))) return FALSE;
      if (useFillval) {
	if (!strcmpITB(vAttrName,"FILLVAL")) {
	  nBytes = (size_t) (vNumElems * CDFelemSize(vDataType));
	  padValue = cdf_AllocateMemory (nBytes, FatalError);
	  ConvertDataType (eDataType, eNumElems, eValueB,
			   vDataType, vNumElems, padValue);
	  status = CDFlib (PUT_, VAR_PADVALUE(zVar), padValue,
			   NULL_);
	  if (!StatusHandlerS2C(status,BOO(zVar,padZ,padR))) return FALSE;
	  cdf_FreeMemory (padValue, FatalError);
	}
      }
      if (eValueB != item) cdf_FreeMemory (eValueB, FatalError);
      NextSearchItem (TOKEN, VaNAMEfieldOrPERIODmark);
      break;
    }
    /**************************************************************************
    *
    **************************************************************************/
    case LOCfieldOrVARNAMEfieldORzVARmarkOrENDmark:
      if (!strcmp(item,"#end")) {
	fclose (SKTfp);
	if (!CloseTheCDF()) return FALSE;
	done = TRUE;
	return FALSE;
      }
      else
	if (!strcmp(item,"#zVariables")) {
	  if (mLog) WriteOut (stdout, "Reading zVariables section...\n");
	  NextSearchItem (TOKEN, VARNAMEfieldOrENDmark);
	  zSection = TRUE;
	}
	else
	  if (item[0] == '[' || Decimal(item[0])) {
	    if (!RecurCharStream(stream,VALLOCATION,LOCfield)) return FALSE;
	  }
	  else {
	    if (!RecurCharStream(stream,DELIMSTRING,VARNAMEfield)) return
								     FALSE;
	  }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case LOCfieldOrVARNAMEfieldOrENDmark:
      if (!strcmp(item,"#end")) {
	if (mLog) WriteOut (stdout, "Reading end section...");
	fclose (SKTfp);
	if (!CloseTheCDF()) return FALSE;
	if (mLog) WriteOut (stdout, "success.\n");
	done = TRUE;
	return FALSE;
      }
      else
	if (item[0] == '[' || Decimal(item[0])) {
	  if (!RecurCharStream(stream,VALLOCATION,LOCfield)) return FALSE;
	}
	else {
	  if (!RecurCharStream(stream,DELIMSTRING,VARNAMEfield)) return FALSE;
	}
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case LOCfield: {
      long indices[CDF_MAX_DIMS], nIndices, recNum;
      if (!DecodeRecordAndIndices(item,&recNum,&nIndices,indices)) {
	ParseError ("Illegal record number and/or indices.");
	return FALSE;
      }
      if (nIndices != BOO(zSection,zNumDims,rNumDims)) {
	ParseError ("Wrong number of indices.");
	return FALSE;
      }
      if (zSection) {
        zVar = TRUE;
      }
      else {
        switch (zMode) {
          case zMODEoff:
	    zVar = FALSE;
	    break;
          case zMODEon1:
	    zVar = TRUE;
	    break;
	  /********************************************************************
	  * If zMode/2, only those indices corresponding to a VARY dimension
	  * should remain.
	  ********************************************************************/
          case zMODEon2:
	    zVar = TRUE;
            for (dN = 0, dNt = 0; dN < rNumDims; dN++) { 
              if (dimVarys[dN]) indices[dNt++] = indices[dN];
            }
	    break;
        }
      }
      status = CDFlib (SELECT_, BOO(zVar,zVAR_RECNUMBER_,
					 rVARs_RECNUMBER_), recNum,
				BOO(zVar,zVAR_DIMINDICES_,
					 rVARs_DIMINDICES_), indices,
		       NULL_);
      if (!StatusHandlerS2C(status,BOO(zVar,selRIZ,selRIR))) return FALSE;
      NextSearchItem (TOKEN, EQUALmark);
     break;
    }
    /**************************************************************************
    *
    **************************************************************************/
    case EQUALmark:
      if (!strcmp(item,"="))
	if (STRINGdataType(vDataType))
	  NextSearchItem (BRACEDchar, VARVALUEfield);
	else
	  if (vDataType == CDF_EPOCH)
	    NextSearchItem (VAREPOCH, VARVALUEfield);
	  else if (vDataType == CDF_EPOCH16)
	    NextSearchItem (VAREPOCH16, VARVALUEfield);
	  else if (vDataType == CDF_TIME_TT2000)
	    NextSearchItem (VARTT2000, VARVALUEfield);
	  else
	    NextSearchItem (TOKEN, VARVALUEfield);
      else {
	ItemError1 ("=", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VARCOMPRESSIONmark:
      if (!strcmp(item,"VAR_COMPRESSION:"))
        NextSearchItem (TOKEN, VARCOMPRESSIONfield);
      else
        if (!strcmp(item,"VAR_COMPRESSION"))
          NextSearchItem (TOKEN, VARCOMPRESSIONCOLONmark);
        else {
          ItemError2 ("VAR_COMPRESSION:", "VAR_COMPRESSION", item);
          return FALSE;
        }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VARCOMPRESSIONCOLONmark:
      if (!strcmp(item,":"))
	NextSearchItem (TOKEN, VARCOMPRESSIONfield);
      else {
	ItemError1 (":", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VARCOMPRESSIONfield: {
      long compression, compressionParms[CDF_MAX_PARMS];
      long version, release, increment;
      char cmpStr[MAX_SKTLINE_LEN+1+1];
      strncpy(cmpStr, item, strlen(item));
      cmpStr[strlen(item)] = (char) '\0';
      compression = WhichCompression (cmpStr, &compression, compressionParms);
      if (compression == -1) {
	ItemError1 ("<varcompression>", item);
	return FALSE;
      }
      else
	NextSearchItem (TOKEN, VaNAMEfieldOrPERIODmark);
      status = CDFlib (GET_, LIB_VERSION_, &version,
                             LIB_RELEASE_, &release,
                             LIB_INCREMENT_, &increment,
                       NULL_);
      if (!StatusHandlerS2C(status,libCDF)) return FALSE;
      if (!PriorTo ("2.6.0", (Int32) version, (Int32) release,
                    (Int32) increment)) {
        zVar = BOO(zSection,TRUE,(zMode > zMODEoff));
	status = CDFlib (PUT_, VAR_COMPRESSION(zVar), compression,
                                                      compressionParms,
                         NULL_);
        if (!StatusHandlerS2C(status,cmpVAR)) return FALSE; 
      }
      break;
    }
    /**************************************************************************
    *
    **************************************************************************/
    case VARSPARSEmark:
      if (!strcmp(item,"VAR_SPARSERECORDS:"))
        NextSearchItem (TOKEN, VARSPARSEfield);
      else
        if (!strcmp(item,"VAR_SPARSERECORDS"))
          NextSearchItem (TOKEN, VARSPARSECOLONmark);
        else {
          ItemError2 ("VAR_SPARSERECORDS:", "VAR_SPARSERECORDS", item);
          return FALSE;
        }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VARSPARSECOLONmark:
      if (!strcmp(item,":"))
	NextSearchItem (TOKEN, VARSPARSEfield);
      else {
	ItemError1 (":", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VARSPARSEfield: {
      long sparse;
      sparse = WhichRecSparseness (item);
      if (sparse == -1) {
	ItemError1 ("<var_sparserecords>", item);
	return FALSE;
      }
      else
	NextSearchItem (TOKEN, VaNAMEfieldOrPERIODmark);
      zVar = BOO(zSection,TRUE,(zMode > zMODEoff));
      if (recVary == NOVARY) break;
      status = CDFlib (PUT_, VAR_SPARSERECORDS(zVar), sparse,
                       NULL_);
      if (!StatusHandlerS2C(status,spaVAR)) return FALSE; 
      break;
    }
    /**************************************************************************
    *
    **************************************************************************/
    case VARBLOCKINGmark:
      if (!strcmp(item,"VAR_BLOCKINGFACTOR:"))
        NextSearchItem (TOKEN, VARBLOCKINGfield);
      else
        if (!strcmp(item,"VAR_BLOCKINGFACTOR"))
          NextSearchItem (TOKEN, VARBLOCKINGCOLONmark);
        else {
          ItemError2 ("VAR_BLOCKINGFACTOR:", "VAR_BLOCKINGFACTOR", item);
          return FALSE;
        }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VARBLOCKINGCOLONmark:
      if (!strcmp(item,":"))
	NextSearchItem (TOKEN, VARBLOCKINGfield);
      else {
	ItemError1 (":", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VARBLOCKINGfield: {
      long bf;
      if (sscanf(item, "%ld", &bf) != 1) {
	ItemError1 ("<var_blockingfactor>", item);
	return FALSE;
      }
      else
	NextSearchItem (TOKEN, VaNAMEfieldOrPERIODmark);
      zVar = BOO(zSection,TRUE,(zMode > zMODEoff));
      status = CDFlib (PUT_, VAR_BLOCKINGFACTOR(zVar), bf,
                       NULL_);
      if (!StatusHandlerS2C(status,spaVAR)) return FALSE; 
      break;
    }
    /**************************************************************************
    *
    **************************************************************************/
    case VARPADVALUEmark:
      if (!strcmp(item,"VAR_PADVALUE:")) {
        if (vDataType == CDF_EPOCH)
	  NextSearchItem (VAREPOCH, VARPADVALUEfield);
        else if (vDataType == CDF_EPOCH16)
          NextSearchItem (VAREPOCH16, VARPADVALUEfield);
        else if (vDataType == CDF_TIME_TT2000)
          NextSearchItem (VARTT2000, VARPADVALUEfield);
        else if (vDataType == CDF_CHAR || vDataType == CDF_UCHAR)
          NextSearchItem (VARCHAR, VARPADVALUEfield);
        else
          NextSearchItem (TOKEN, VARPADVALUEfield);
      } else {
        if (!strcmp(item,"VAR_PADVALUE")) {
          if (vDataType == CDF_EPOCH)
            NextSearchItem (VAREPOCH, VARPADVALUEfield);
          else if (vDataType == CDF_EPOCH16)
            NextSearchItem (VAREPOCH16, VARPADVALUEfield);
          else if (vDataType == CDF_TIME_TT2000)
            NextSearchItem (VARTT2000, VARPADVALUEfield);
          else if (vDataType == CDF_CHAR || vDataType == CDF_UCHAR)
            NextSearchItem (VARCHAR, VARPADVALUEfield);
          else
            NextSearchItem (TOKEN, VARPADVALUEmark);
        } else {
          ItemError2 ("VAR_PADVALUE:", "VAR_PADVALUE", item);
          return FALSE;
        }
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VARPADVALUECOLONmark:
      if (!strcmp(item,":"))
	NextSearchItem (TOKEN, VARPADVALUEfield);
      else {
	ItemError1 (":", item);
	return FALSE;
      }
      break;
    /**************************************************************************
    *
    **************************************************************************/
    case VARPADVALUEfield: {
      void *valuesB;
      long pNumElems;
      int style;
      if (!STRINGdataType(vDataType)) {
        if (TT2000dataType(vDataType)) style = TT2000_3_STYLE;
        else  style = EPOCH0_STYLE;
        if (!DecodeValues(item,vDataType,&pNumElems,&valuesB,style)) {
          ParseError ("Illegal pad value.");
          return FALSE;
        }
        if (pNumElems != 1) {
          ParseError ("Illegal number of elements for a pad value.");
          cdf_FreeMemory (valuesB, FatalError);
          return FALSE;
        }
        zVar = BOO(zSection,TRUE,(zMode > zMODEoff));
        status = CDFlib (PUT_, VAR_PADVALUE(zVar), valuesB,
                         NULL_);
      }
      else {
        int len;
        char *tmp;
        valuesB = item + 1;
        tmp = cdf_AllocateMemory (vNumElems + 1, FatalError);
        strncpy (tmp, valuesB, (size_t) vNumElems);
        *(tmp+vNumElems) = '\0';
        len = (int) strlen (valuesB);
        if ((len-1) < vNumElems) {
          int ix;
          for (ix = (len-1); ix < vNumElems; ++ix) *(tmp+ix) = (char) ' ';
        }
        zVar = BOO(zSection,TRUE,(zMode > zMODEoff));
        status = CDFlib (PUT_, VAR_PADVALUE(zVar), tmp,
                         NULL_);
        if (strchr(tmp, '"') != NULL) inString = FALSE;
        cdf_FreeMemory (tmp, FatalError);
      }
      if (!StatusHandlerS2C(status,padVAR)) return FALSE;
      if (!STRINGdataType(vDataType)) cdf_FreeMemory (valuesB, FatalError);
      NextSearchItem (TOKEN, VaNAMEfieldOrPERIODmark);
      break;
    }
    /**************************************************************************
    *
    **************************************************************************/
    case VARVALUEfield: {
      void *value;
      long numElems;
      int style;
      if (!STRINGdataType(vDataType)) {
        if (TT2000dataType(vDataType)) style = TT2000_3_STYLE;
        else style = EPOCH0_STYLE;
	if (!DecodeValues(item,vDataType,&numElems,&value,style)) {
	  ParseError ("Illegal variable value.\n");
	  return FALSE;
	}
	if (numElems != 1) {
	  ParseError ("Illegal number of elements for a variable value.");
	  cdf_FreeMemory (value, FatalError);
	  return FALSE;
	}
      }
      else {
	int len;
	len = strlen (item);
	if (len > vNumElems) {
	  char tempS[MAX_MSG_LEN+1];
	  WriteOut (stdout, "ERROR: Truncating value for variable `");
	  WriteOut (stdout, varName);
	  WriteOut (stdout, "' at line\n");
	  snprintf (tempS, (size_t) sizeof(tempS),
		    "       number %d (this is a non-fatal error).\n", lineN);
	  WriteOut (stdout, tempS);
	  value = item;
	}
	else
	  if (len < vNumElems) {
	    if (mLog) {
	      char tempS[MAX_MSG_LEN+1];
	      WriteOut (stdout,
			"WARNING: Blank padding value for variable `");
	      WriteOut (stdout, varName);
	      WriteOut (stdout, "'\n");
	      snprintf (tempS, (size_t) sizeof(tempS),
			"         at line number %d.\n", lineN);
	      WriteOut (stdout, tempS);
	    }
	    nBytes = (size_t) (vNumElems + 1);
	    value = cdf_AllocateMemory (nBytes, FatalError);
	    strcpyX (value, item, (size_t) vNumElems);
	    BlankPadRight (value, (int) (vNumElems - len));
	  }
	  else
	    value = item;
      }
      zVar = BOO(zSection,TRUE,(zMode > zMODEoff));
      status = CDFlib (PUT_, VAR_DATA(zVar), value,
		       NULL_);
      if (!StatusHandlerS2C(status,BOO(zVar,wriZ,wriR))) return FALSE;
      if (value != item) cdf_FreeMemory (value, FatalError);
      if (zSection)
	NextSearchItem (TOKEN, LOCfieldOrVARNAMEfieldOrENDmark);
      else
	NextSearchItem (TOKEN, LOCfieldOrVARNAMEfieldORzVARmarkOrENDmark);
      break;
    }
    /**************************************************************************
    *
    **************************************************************************/
    default:
      ParseError ("** Internal error, contact CDFSUPPORT **");
      return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* NextSearchItem.
******************************************************************************/

void NextSearchItem (type, item)
enum searchForENUM type;
enum nextItemENUM item;
{
  searchFor = type;
  switch (type) {
    case TOKEN:
      token[0] = NUL;
      break;
    case DELIMSTRING:
      stringDelim = NUL;
      stringLineN = -1;
      string[0] = NUL;
      break;
    case BRACEDchar:
      startBraceFound = FALSE;
      stringDelim = NUL;
      stringLineN = -1;
      hyphenORendBraceNext = FALSE;
      eValue[0] = NUL;
      break;
    case BRACEDnonChar:
      startBraceFound = FALSE;
      eValue[0] = NUL;
      break;
    case VALLOCATION:
      locationStarted = FALSE;
      location[0] = NUL;
      break;
    case VAREPOCH:
      epochCount = 0;
      epoch[0] = NUL;
      break;
    case VAREPOCH16:
      epochCount = 0;
      epoch2[0] = NUL;
      break;
    case VARTT2000:
      epochCount = 0;
      tt2000[0] = NUL;
      break;
    case VARCHAR:
      charCount = 0;
      charStr[0] = NUL;
      break;
  }
  nextItem = item;
  stream[0] = NUL;
  return;
}

/******************************************************************************
* ItemError1.  (A specific item was expected).
******************************************************************************/

void ItemError1 (expected, found)
char *expected;
char *found;
{
  char msg[MAX_MSG_LEN+1];
  snprintf (msg, (size_t) sizeof(msg),
	    "\nExpecting `%s', found `%s' instead.", expected, found);
  ParseError (msg);
  return;
}

/******************************************************************************
* ItemError2.  (One of two items were expected).
******************************************************************************/

void ItemError2 (expected1, expected2, found)
char *expected1;
char *expected2;
char *found;
{
  char msg[MAX_MSG_LEN+1];
  snprintf (msg, (size_t) sizeof(msg),
	    "Expecting `%s' or `%s', found `%s' instead.",
	    expected1, expected2, found);
  ParseError (msg);
  return;
}

/******************************************************************************
* ItemError3.  (One of three items were expected).
******************************************************************************/

void ItemError3 (expected1, expected2, expected3, found)
char *expected1;
char *expected2;
char *expected3;
char *found;
{
  char msg[MAX_MSG_LEN+1];
  snprintf (msg, (size_t) sizeof(msg),
	    "Expecting `%s', `%s', or `%s', found `%s' instead.",
	    expected1, expected2, expected3, found);
  ParseError (msg);
  return;
}

/******************************************************************************
* ParseError.
******************************************************************************/

void ParseError (msg)
char *msg;
{
  char tempS[MAX_MSG_LEN+1];
  WriteOut (stdout, msg);
  WriteOut (stdout, "\n\n");
  snprintf (tempS, (size_t) sizeof(tempS),
	    "Error at (or before) line number %d which follows...\n\n",
	    lineN);
  WriteOut (stdout, tempS);
  WriteOut (stdout, line);
  WriteOut (stdout, "\n");      /* `line' has a NL at the end already. */
  CDFlib (DELETE_, CDF_,
	  NULL_);
  fclose (SKTfp);
}

/******************************************************************************
* CATchr.
* Concatenate a character to a string.  If the string is already at its
* maximum length, reallocate the string with a larger size.
******************************************************************************/

void CATchr (str, chr, size, base)
char **str;
int chr;
size_t *size;
int base;
{
  size_t nChars = (size_t) (*size + base);
  if (strlen(*str) == *size) {
    *str = (char *) cdf_ReallocateMemory (*str, (size_t) (nChars + 1), FatalError);
    *size += base;
  }
  catchrX (*str, chr, nChars);
  return;
}

/******************************************************************************
* RecurCharStream.
******************************************************************************/

Logical RecurCharStream (stream, type, next)
char *stream;
enum searchForENUM type;
enum nextItemENUM next;
{
  char *streamT; int i;
  size_t nChars = strlen (stream);
  streamT = (char *) cdf_AllocateMemory ((size_t) (nChars + 1), FatalError);
  strcpyX (streamT, stream, nChars);
  NextSearchItem (type, next);
  for (i = 0; streamT[i] != NUL; i++) {
     if (!CharStream(streamT[i])) {
       cdf_FreeMemory (streamT, FatalError);
       return FALSE;
     }
  }
  cdf_FreeMemory (streamT, FatalError);
  return TRUE;
}

/******************************************************************************
* AllocateGrowingStrings.
******************************************************************************/

void AllocateGrowingStrings () {
  streamZ = BASE_STREAM_SIZE;
  stream = (char *) cdf_AllocateMemory ((size_t) (streamZ + 1), FatalError);
  stringZ = BASE_STRING_SIZE;
  string = (char *) cdf_AllocateMemory ((size_t) (stringZ + 1), FatalError);
  tokenZ = BASE_TOKEN_SIZE;
  token = (char *) cdf_AllocateMemory ((size_t) (tokenZ + 1), FatalError);
  eValueZ = BASE_EVALUE_SIZE;
  eValue = (char *) cdf_AllocateMemory ((size_t) (eValueZ + 1), FatalError);
  locationZ = BASE_LOCATION_SIZE;
  location = (char *) cdf_AllocateMemory ((size_t) (locationZ + 1), FatalError);
  return;
}

/******************************************************************************
* FreeGrowingStrings.
******************************************************************************/

void FreeGrowingStrings () {
  cdf_FreeMemory (stream, FatalError);
  cdf_FreeMemory (string, FatalError);
  cdf_FreeMemory (token, FatalError);
  cdf_FreeMemory (eValue, FatalError);
  cdf_FreeMemory (location, FatalError);
  return;
}

/******************************************************************************
* CloseTheCDF.
* Returns TRUE if the CDF was successfully closed.
******************************************************************************/

static Logical CloseTheCDF () {
  CDFstatus status;
  if (displayStats) {
    vSTATS vStatsDotCDF, vStatsStage, vStatsCompress;
    status = CDFlib (CLOSE_, CDFwithSTATS_, &vStatsDotCDF,
					    &vStatsStage,
					    &vStatsCompress,
		     NULL_);
    if (!StatusHandlerS2C(status,cloCDF)) return FALSE;
    DisplayStatistics ("CDF", &vStatsDotCDF, &vStatsStage, &vStatsCompress);
  }
  else {
    status = CDFlib (CLOSE_, CDF_,
		     NULL_);
    if (!StatusHandlerS2C(status,cloCDF)) return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* StatusHandlerS2C.
******************************************************************************/

Logical StatusHandlerS2C (status, msg)
CDFstatus status;
char *msg;
{
  char text[CDF_STATUSTEXT_LEN+1];
  static char infoT[] = {
    "The following informational condition was encountered while"
  };
  static char warnT[] = {
    "The following warning condition was encountered while"
  };
  static char errorT[] = {
    "The following ERROR condition was encountered while"
  };
  char tempS[2*(MAX_SCREENLINE_LEN+1)];
  if (StatusINFO(status) && report[INFOs]) {
    CDFlib (SELECT_, CDF_STATUS_, status,
	    GET_, STATUS_TEXT_, text,
	    NULL_);
    snprintf (tempS, (size_t) sizeof(tempS), "\n%s%s%s.\n\n%s\n", infoT,
	      ((strlen(infoT) + strlen(msg) + 2 >
	       (size_t) MAX_SCREENLINE_LEN) ? "\n" : " "), msg, text);
    WriteOut (stdout, tempS);
    return TRUE;
  }
  if (StatusWARN(status) && report[WARNs]) {
    CDFlib (SELECT_, CDF_STATUS_, status,
	    GET_, STATUS_TEXT_, text,
	    NULL_);
    snprintf (tempS, (size_t) sizeof(tempS), "\n%s%s%s.\n\n%s\n", warnT,
	      ((strlen(warnT) + strlen(msg) + 2 >
	       (size_t) MAX_SCREENLINE_LEN) ? "\n" : " "), msg, text);
    WriteOut (stdout, tempS);
    return TRUE;
  }
  if (StatusERROR(status)) {
    if (report[ERRORs]) {
      CDFlib (SELECT_, CDF_STATUS_, status,
	      GET_, STATUS_TEXT_, text,
	      NULL_);
      snprintf (tempS, (size_t) sizeof(tempS), "\n%s%s%s.\n\n%s\n", errorT,
	        ((strlen(errorT) + strlen(msg) + 2 >
		 (size_t) MAX_SCREENLINE_LEN) ? "\n" : " "), msg, text);
      WriteOut (stdout, tempS);
    }
    ParseError ("");
    return FALSE;
  }
  return TRUE;  /* CDF_OK */
}

/******************************************************************************
* CreateSkeletonQOPs.
*    Returns TRUE if execution should continue.
******************************************************************************/

#if defined(mac)
Logical CreateSkeletonQOPs (argC, argV)
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
  FileFilterUPP FilterForSKTsUPP;
  UserItemUPP OutlineDefaultButtonUPP;
#endif
  short itemN, iType, i;
  static Logical first = TRUE;
  char cancelTitle[] = "Cancel";
  static Logical logMsg = DEFAULTlogSKT2CDF;
  static Logical deleteCDF = DEFAULTdeleteSKT2CDF;
  static Logical reportInfos = REPORTinfosDEFAULT;
  static Logical reportWarns = REPORTwarningsDEFAULT;
  static Logical reportErrors = REPORTerrorsDEFAULT;
  static Logical negToPos = DEFAULT_NEGtoPOSfp0;
  static Logical useFillval = DEFAULTfillvalSKT2CDF;
  static Logical dispStats = DEFAULTstatsEDIT;
  static int zMode = DEFAULTzModeSKT2CDF;
  static Str255 CDFtext = "\p";
  static Str255 sktText = "\p";
  static Str255 cacheText = "\p";
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
  SetIText ((Handle) controlHs[SKTTEXTin], sktText);
  SetIText ((Handle) controlHs[CACHEin], cacheText);
  if (logMsg) SetCtlValue (controlHs[LOGin], 1);
  if (deleteCDF) SetCtlValue (controlHs[DELETEin], 1);
  if (reportInfos) SetCtlValue (controlHs[INFOin], 1);
  if (reportWarns) SetCtlValue (controlHs[WARNin], 1);
  if (reportErrors) SetCtlValue (controlHs[ERRORin], 1);
  if (negToPos) SetCtlValue (controlHs[NEGZin], 1);
  if (useFillval) SetCtlValue (controlHs[FILLin], 1);
  if (dispStats) SetCtlValue (controlHs[STATSin], 1);
  SetCtlValue (controlHs[ZMODEinBASE+zMode], 1);

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
  for (;;) {
#ifndef __MWERKS__
    ModalDialog (FilterDialogQOPso, &itemN);
#else
	FilterDialogQOPsoUPP = NewModalFilterProc((ProcPtr) FilterDialogQOPso);
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
		GetIText ((Handle) controlHs[SKTTEXTin], sktText);
		GetIText ((Handle) controlHs[CACHEin], cacheText);
		logMsg = GetCtlValue (controlHs[LOGin]);
		deleteCDF = GetCtlValue (controlHs[DELETEin]);
		reportInfos = GetCtlValue (controlHs[INFOin]);
		reportWarns = GetCtlValue (controlHs[WARNin]);
		reportErrors = GetCtlValue (controlHs[ERRORin]);
		negToPos = GetCtlValue (controlHs[NEGZin]);
		useFillval = GetCtlValue (controlHs[FILLin]);
		dispStats = GetCtlValue (controlHs[STATSin]);
		for (zMode = 0; zMode < 3; zMode++) {
		   if (GetCtlValue(controlHs[ZMODEinBASE+zMode])) break;
		}
		/**********************************************************************
		* Build argc/argv.
		**********************************************************************/
		*argC = 10 + BOO(NULpString(CDFtext),0,2) +
				     BOO(NULpString(sktText),0,1) +
				     BOO(NULpString(cacheText),0,2);
		*argV = (char **) cdf_AllocateMemory ((size_t) (*argC * sizeof(char *)),
					  FatalError);
		n = 0;
		MAKEstrARGv (argV, n, pgmName)
		if (!NULpString(CDFtext)) {
		  MAKEstrARGv (argV, n, "-cdf")
		  PtoCstr (CDFtext);
		  MAKEstrARGv (argV, n, (char *) CDFtext)
		  CtoPstr ((char *) CDFtext);
		}
		MAKEbooARGv (argV, n, logMsg, "-log", "-nolog")
		MAKEbooARGv (argV, n, deleteCDF, "-delete", "-nodelete")
		MAKEbooARGv (argV, n, negToPos, "-neg2posfp0", "-noneg2posfp0")
		MAKEbooARGv (argV, n, useFillval, "-fillval", "-nofillval")
		MAKEbooARGv (argV, n, dispStats, "-statistics", "-nostatistics")
		MAKEstrARGv (argV, n, "-report")
		MAKEstrARGv (argV, n, StatusCodeReportOptions(reportErrors,
						      reportWarns,
						      reportInfos))
		MAKEstrARGv (argV, n, "-zmode")
		switch (zMode) {
		  case zMODEoff:
		    MAKEstrARGv (argV, n, "0")
		    break;
		  case zMODEon1:
		    MAKEstrARGv (argV, n, "1")
		    break;
		  case zMODEon2:
		    MAKEstrARGv (argV, n, "2")
		    break;
		}
		if (!NULpString(cacheText)) {
		  MAKEstrARGv (argV, n, "-cache")
		  PtoCstr (cacheText);
		  MAKEstrARGv (argV, n, (char *) cacheText)
		  CtoPstr ((char *) cacheText);
		}
		if (!NULpString(sktText)) {
		  PtoCstr (sktText);
		  MAKEstrARGv (argV, n, (char *) sktText)
		  CtoPstr ((char *) sktText);
		}
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
		*argV = (char **) cdf_AllocateMemory ((size_t) (*argC * sizeof(char *)),
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
      * Select skeleton CDF specification.
      *     The cursor is set because `StandardPutFile' leaves the cursor as
      * an iBeam (instead of returning it to what it was).
      ************************************************************************/
      case CDFSELECTin: {
		StandardFileReply CDFreply;
		char CDFpath[DU_MAX_PATH_LEN+1], prompt[] = "Enter CDF name:";
		StandardPutFile (CtoPstr(prompt), CtoPstr(""), &CDFreply);
		if (CDFreply.sfGood && !CDFreply.sfIsFolder && !CDFreply.sfIsVolume) {
		  BuildMacPath (&CDFreply.sfFile, CDFpath, TRUE);
		  CDFtext[0] = strlen (CDFpath);
		  strcpyX ((char *) &CDFtext[1], CDFpath, 255);
		  SetIText ((Handle) controlHs[CDFTEXTin], CDFtext);
		}
		SetCursor (&(qd.arrow));
		break;
      }
      /************************************************************************
      * Select skeleton table.
      ************************************************************************/
      case SKTSELECTin: {
		StandardFileReply sktReply;
		char sktPath[DU_MAX_PATH_LEN+1];
#ifndef __MWERKS__
		StandardGetFile (FilterForSKTs, -1, NULL, &sktReply);
#else
		FilterForSKTsUPP = NewFileFilterProc((ProcPtr) FilterForSKTs);
		StandardGetFile (FilterForSKTsUPP, -1, NULL, &sktReply);
		DisposeRoutineDescriptor (FilterForSKTsUPP);
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
      case DELETEin:
      case INFOin:
      case WARNin:
      case ERRORin:
      case NEGZin:
      case FILLin:
      case STATSin:
		SetCtlValue (controlHs[itemN], BOO(GetCtlValue(controlHs[itemN]),0,1));
		break;
      /************************************************************************
      * Radio buttons.
      ************************************************************************/
      case ZMODEinBASE+0:
      case ZMODEinBASE+1:
      case ZMODEinBASE+2:
		for (i = 0; i < 3; i++) SetCtlValue (controlHs[ZMODEinBASE+i], 0);
		SetCtlValue (controlHs[itemN], 1);
		break;
      }
  }
}
#endif
