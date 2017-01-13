/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                        CDFcompare (differences).
*
*  Version 2.7b, 21-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0   1-Jun-91, J Love     Original version (for CDF V2.1).
*   V1.1  25-Jun-91, J Love     CDF_EPOCH added as a data type.  Added QOP.
*                               Added PageInst.
*   V1.2   6-Aug-91, J Love     TRUE/FALSE.  Use 'Exit'/'ExitBAD'.  Use
*                               'CDFlib'.  Display message if no differences
*                               found.
*   V1.3  10-Aug-91, J Love     Optimized comparison of variable values.
*   V2.0  17-May-92, J Love     IBM PC & HP port.
*   V2.1  17-Sep-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V2.1a  6-Oct-92, J Love     Fixed freeing of `bufferX' when 1-dimensional.
*   V2.2  25-Jan-94, J Love     CDF V2.4.
*   V2.2a 22-Feb-94, J Love     Spelling lesson.
*   V2.3   9-Mar-94, J Love     Made parentheses enclosing zModes options (on
*                               all operating systems).
*   V2.4   6-Dec-94, J Love     CDF V2.5.
*   V2.4a 10-Jan-95, J Love     Uppercase file extensions on the Macintosh.
*   V2.4b 16-Mar-95, J Love     Display value(s) when a difference detected.
*   V2.4c  6-Apr-95, J Love     POSIX.
*   V2.5  25-May-95, J Love     EPOCH styles.  Fixed rVariables vs. zVariable
*                               comparison.
*   V2.5a 20-Jun-95, J Love     Fixed variable number matching (in zMode).
*   V2.6   1-Sep-95, J Love     CDFexport-related changes.  Hyper groups.
*   V2.6a 19-Sep-95, J Love	CHECKforABORTso.
*   V2.6b 29-Sep-95, J Love	Macintosh dialog filtering.  Outline default
*				button.  Less CHECKforABORTso.
*   V2.7  15-Aug-96, J Love	CDF V2.6.
*   V2.7a 21-Feb-97, J Love	Removed RICE.
*   V2.7b 21-Nov-97, J Love	Windows NT/Visual C++.
*   V2.8  26-Jan-02, M Liu 	Correct dimension sizes for zVariables.
*   V2.9  17-Jun-02, M Liu      Added tolerance option for float/double values
*                               comparison. 
*   V2.10 13-Oct-06, M Liu      Changed to allow upper and lower case CDF  
*                               name to be used on win32.
*   V3.3  10-Jan-09, M Liu      Validate a file before it is used.
*   V3.3a 04-Apr-11, M Liu      Modified to handle TT2000 epoch style.
*   V3.6  10-Jul-16, M Liu      Added "-noformat" option to show the data.
*
******************************************************************************/

#include "cdfcmp.h"

/******************************************************************************
* Increased stack size for Borland C on IBM PC.
******************************************************************************/

#if defined(BORLANDC)
extern unsigned _stklen = BORLANDC_STACK_SIZE;
#endif

/******************************************************************************
* Global variables.
******************************************************************************/

CDFid id1, id2;
long CDFcType1, CDFcParms1[CDF_MAX_PARMS], CDFcPct1;
long CDFcType2, CDFcParms2[CDF_MAX_PARMS], CDFcPct2;
long rNumDims1, rNumDims2;
long rDimSizes1[CDF_MAX_DIMS], rDimSizes2[CDF_MAX_DIMS];
long zNumDims1, zNumDims2;
long zDimSizes1[CDF_MAX_DIMS], zDimSizes2[CDF_MAX_DIMS];
long encoding1, encoding2;
long majority1, majority2;
long format1, format2;
long checksum1 = -999, checksum2 = -999;
char copyright1[CDF_COPYRIGHT_LEN+1], copyright2[CDF_COPYRIGHT_LEN+1];
long rNumVars1, rNumVars2;
long zNumVars1, zNumVars2;
long numAttrs1, numAttrs2;
long CDFmaxRec1, CDFmaxRec2;
long version1, version2;
long release1, release2;
long increment1, increment2;
long tt2000updated1 = -1L, tt2000updated2 = -1L;
long attrScope1, attrScope2;
long attrMAXgrEntry1, attrMAXgrEntry2;
long attrNUMgrEntries1, attrNUMgrEntries2;
long attrMAXzEntry1, attrMAXzEntry2;
long attrNUMzEntries1, attrNUMzEntries2;
long varMaxRec1, varMaxRec2;
long varDataType1, varDataType2;
long varNumElems1, varNumElems2;
long varRecVary1, varRecVary2;
long varDimVarys1[CDF_MAX_DIMS], varDimVarys2[CDF_MAX_DIMS];
long varBlocking1, varBlocking2;
Logical mLog, pctLog;
Logical cmpEtc;
Logical cmpNumbers;
Logical cmpVars;
Logical cmpAttrs;
Logical negToPosFp0;
Logical locations;
Logical format;
long *attrNumMatches1, *attrNumMatches2;
long *rVarNumMatches1, *rVarNumMatches2;
long *zVarNumMatches1, *zVarNumMatches2;
Logical diffFound;
long zMode_1, zMode_2;
static char rVariable[] = "rVariable";
static char zVariable[] = "zVariable";
static char gEntry[] = "gEntry";
static char rEntry[] = "rEntry";
static char zEntry[] = "zEntry";
char oText[MAX_oTEXT_LEN+1];
Logical report[3];
Logical dumpStatistics;
Logical displayValue;
long workingCache, stageCache, compressCache;
Logical floatSet = FALSE, doubleSet = FALSE;
float floatTolerance = (float) 0.0;
double doubleTolerance = (double) 0.0;

/******************************************************************************
* Main.
******************************************************************************/

#if !defined(win32) || (defined(win32) && defined(ALONE))
MAIN {
  Logical success = TRUE;
  strcpyX (pgmName, "CDFcompare", MAX_PROGRAM_NAME_LEN);
#if defined(mac)
  MacExecuteSO (CompareCDFs, CompareQOPs);
#else
  success = CompareCDFs (argc, argv);
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
* CompareCDFs.
******************************************************************************/

Logical CompareCDFs (argC, argV)
int argC;
char *argV[];
{
  char CDFspec1[DU_MAX_PATH_LEN + 1], CDFspec2[DU_MAX_PATH_LEN + 1];
  char **dirs1 = NULL, **dirs2 = NULL;
  char **CDFs1 = NULL, **CDFs2 = NULL;
  int numCDFs1, numCDFs2;
  char CDFpath1[DU_MAX_PATH_LEN + 1], CDFpath2[DU_MAX_PATH_LEN + 1];
  char dir1[DU_MAX_DIR_LEN + 1], dir2[DU_MAX_DIR_LEN + 1];
  char CDF1[DU_MAX_NAME_LEN + 1], CDF2[DU_MAX_NAME_LEN + 1];
  int i, j;
  Logical qopError = FALSE;
  Logical status2;
  QOP *qop;
  static char *validQuals[] = {
    "log", "nolog", "attr", "noattr", "var", "novar", "number", "nonumber",
    "percent", "nopercent", "zmodes", "etc", "noetc", "neg2posfp0",
    "noneg2posfp0", "location", "nolocation", "report", "page", "nopage",
    "cache", "statistics", "nostatistics", "value", "novalue", "about", 
    "tolerance", "format", "noformat", NULL
  };
  static int optRequired[] = {
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
    FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, 
    FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, 
    TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, 
    TRUE, FALSE, FALSE, 0
  };
  static char *reportTokens[] = { "errors", "warnings", "informationals" };
  static char logPctMsg[] = {
    "Message logging must be specified with percentage logging."
  };
  static char no2ndMsg[] = "No CDF(s) found for 2nd specification.";
  static char no1stMsg[] = "No CDF(s) found for 1st specification.";
  static char noEitherMsg[] = "No CDFs found for either specification.";
  static char noMatch1Msg[] = "Matching CDF not found in 1st specification.";
  static char noMatch2Msg[] = "Matching CDF not found in 2nd specification.";
int ii;
  /****************************************************************************
  * Get qualifiers/options/parameters.
  ****************************************************************************/
  switch (argC) {
    case 1:
      PageOLH ("cdfcmp.olh", argV[0]);
      return TRUE;
    case 2:
      if (strcmp(argV[1],"-java") == 0) {
        pagingOn = FALSE;
        PageOLH ("cdfcmpj.olh", argV[0]);
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
      /************************************************************************
      * Get CDF/paths to compare.
      ************************************************************************/
      switch (qop->Nparms) {
	case 0:
	case 1:
	  DisplayError ("Missing parameter(s).");
	  qopError = TRUE;
	  break;
	case 2:
	  strcpyX (CDFspec1, qop->parms[CDFSPEC1parm], DU_MAX_PATH_LEN);
	  strcpyX (CDFspec2, qop->parms[CDFSPEC2parm], DU_MAX_PATH_LEN);
#if defined(vms) || defined(dos)
	  MakeUpperString (CDFspec1);
	  MakeUpperString (CDFspec2);
#endif
	  break;
	default:
	  DisplayError ("Too many parameters.");
	  qopError = TRUE;
	  break;
      }
      /************************************************************************
      * Check for zMode qualifier.
      ************************************************************************/
      if (qop->qualEntered[ZMODESqual]) {
	char *modes = qop->qualOpt[ZMODESqual];
	if (modes[0] == '(' && modes[strlen(modes)-1] == ')') modes++;
	else if (modes[0] == '"' && modes[strlen(modes)-1] == '"') modes++;
	switch (modes[0]) {
	  case '0': zMode_1 = zMODEoff; break;
	  case '1': zMode_1 = zMODEon1; break;
	  case '2': zMode_1 = zMODEon2; break;
	  default: {
	    DisplayError ("Illegal zModes.");
	    qopError = TRUE;
	    break;
	  }
	}
	switch (modes[2]) {
	  case '0': zMode_2 = zMODEoff; break;
	  case '1': zMode_2 = zMODEon1; break;
	  case '2': zMode_2 = zMODEon2; break;
	  default: {
	    DisplayError ("Illegal zModes.");
	    qopError = TRUE;
	    break;
	  }
	}
      }
      else {
	zMode_1 = DEFAULTzModeCMP;
	zMode_2 = DEFAULTzModeCMP;
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
      * Check for `page', `log', `attr', `var', `number', `percent', `etc',
      * `neg2posfp0', `location', `value', `statistics', and `format' 
      * qualifiers.
      ************************************************************************/
      qopError = qopError | !TFqualifier(qop,&pagingOn,PAGEqual,NOPAGEqual,
					 DEFAULTpageCMP,"page");
      qopError = qopError | !TFqualifier (qop,&mLog,LOGqual,NOLOGqual,
					  DEFAULTlogCMP,"log");
      qopError = qopError | !TFqualifier (qop,&cmpAttrs,ATTRSqual,NOATTRSqual,
					  DEFAULTattrCMP,"attr");
      qopError = qopError | !TFqualifier (qop,&cmpVars,VARSqual,NOVARSqual,
					  DEFAULTvarCMP,"var");
      qopError = qopError | !TFqualifier (qop,&cmpNumbers,NUMBERSqual,
					  NONUMBERSqual,DEFAULTnumberCMP,
					  "number");
      qopError = qopError | !TFqualifier (qop,&pctLog,PCTqual,NOPCTqual,
					  DEFAULTpctCMP,"percent");
      qopError = qopError | !TFqualifier (qop,&cmpEtc,ETCqual,NOETCqual,
					  DEFAULTetcCMP,"etc");
      qopError = qopError | !TFqualifier (qop,&negToPosFp0,NEG2POSqual,
					  NONEG2POSqual,DEFAULT_NEGtoPOSfp0,
					  "neg2posfp0");
      qopError = qopError | !TFqualifier (qop,&locations,LOCSqual,NOLOCSqual,
					  DEFAULTlocationCMP,"location");
      qopError = qopError | !TFqualifier (qop,&displayValue,VALUESqual,
					  NOVALUESqual,DEFAULTvalueCMP,
					  "value");
      qopError = qopError | !TFqualifier (qop,&dumpStatistics,STATSqual,
					  NOSTATSqual,DEFAULTstatsCMP,
					  "statistics");
      qopError = qopError | !TFqualifier (qop,&format,FORMATqual,
					  NOFORMATqual,DEFAULTformatCMP,
					  "format");
      /************************************************************************
      * Check for `cache' qualifier.
      ************************************************************************/
      if (qop->qualEntered[CACHEqual]) {
	if (!ParseCacheSizes(qop->qualOpt[CACHEqual],&workingCache,
			     &stageCache,&compressCache)) {
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
      * Check for `tolerance' qualifier.
      ************************************************************************/
      if (qop->qualEntered[TOLERANCEqual]) {
        if (!ParseTolerances(qop->qualOpt[TOLERANCEqual], &floatTolerance,
                             &floatSet, &doubleTolerance, &doubleSet)) {
          DisplayError ("Illegal tolerance values.");
          qopError = TRUE;
        }
      }
      /************************************************************************
      * Check for conflicting qualifiers.
      ************************************************************************/
      if (pctLog && (!mLog)) {
	DisplayError (logPctMsg);
	qopError = TRUE;
      }
      /************************************************************************
      * Free QOP memory and check for an error.
      ************************************************************************/
      cdf_FreeMemory (qop, FatalError);
      if (qopError) return FALSE;
      break;
  }
/*  CDFsetValidate (VALIDATEFILEon);*/
  /****************************************************************************
  * Compare the CDFs.
  ****************************************************************************/
  if (!IsCDF(CDFspec1)) {
    if (!IsCDF(CDFspec2)) {
      /************************************************************************
      * Both specifications are directories/wildcards.
      ************************************************************************/
      numCDFs1 = CDFdirList (CDFspec1, &dirs1, &CDFs1);
      numCDFs2 = CDFdirList (CDFspec2, &dirs2, &CDFs2);
      if (numCDFs1 > 0)
	if (numCDFs2 > 0) {
	  for (i = 0; i < numCDFs1; i++) {
	     strcpyX (CDFpath1, dirs1[i], DU_MAX_PATH_LEN);
	     AppendToDir (CDFpath1, CDFs1[i]);
	     for (j = 0; j < numCDFs2; j++) {
		if (!strcmp(CDFs1[i],CDFs2[j])) {
		  strcpyX (CDFpath2, dirs2[j], DU_MAX_PATH_LEN);
		  AppendToDir (CDFpath2, CDFs2[j]);
		  status2 = CompareTwoCDFs (CDFpath1, CDFpath2);
		  break;
		}
	     }
	     if (j == numCDFs2) {
	       char tempS[47+DU_MAX_PATH_LEN+1];
	       snprintf (tempS, (size_t) sizeof(tempS),
			 "No matching CDF for \"%s\"", CDFpath1);
	       snprintf (EofS(tempS), (size_t) sizeof(tempS)-strlen(tempS),
			 " in second specification.");
	       OutputWithMargin (stdout, tempS, MAX_SCREENLINE_LEN, 0);
	     }
	  }
	  for (j = 0; j < numCDFs2; j++) {
	     strcpyX (CDFpath2, dirs2[j], DU_MAX_PATH_LEN);
	     AppendToDir (CDFpath2, CDFs2[j]);
	     for (i = 0; i < numCDFs1; i++) {
		if (!strcmp(CDFs2[j],CDFs1[i])) break;
	     }
	     if (i == numCDFs1) {
	       char tempS[47+DU_MAX_PATH_LEN+1];
	       snprintf (tempS, (size_t) sizeof(tempS),
			 "No matching CDF for \"%s\"", CDFpath2);
	       snprintf (EofS(tempS), (size_t) sizeof(tempS)-strlen(tempS),
			 " in first specification.");
	       OutputWithMargin (stdout, tempS, MAX_SCREENLINE_LEN, 0);
	     }
	  }
	}
	else
	  OutputWithMargin (stdout, no2ndMsg, MAX_SCREENLINE_LEN, 0);
      else
	if (numCDFs2 > 0)
	  OutputWithMargin (stdout, no1stMsg, MAX_SCREENLINE_LEN, 0);
	else
	  OutputWithMargin (stdout, noEitherMsg, MAX_SCREENLINE_LEN, 0);
    }
    else {
      /************************************************************************
      * First specification is a directory/wildcard, second is a CDF path.
      ************************************************************************/
      numCDFs1 = CDFdirList (CDFspec1, &dirs1, &CDFs1);
      if (numCDFs1 > 0) {
	ParsePath (CDFspec2, dir2, CDF2);
	for (i = 0; i < numCDFs1; i++) {
	   if (!strcmp(CDFs1[i],CDF2)) {
	     strcpyX (CDFpath1, dirs1[i], DU_MAX_PATH_LEN);
	     AppendToDir (CDFpath1, CDFs1[i]);
	     status2 = CompareTwoCDFs (CDFpath1, CDFspec2);
	     break;
	   }
	}
	if (i == numCDFs1) OutputWithMargin (stdout, noMatch1Msg,
					     MAX_SCREENLINE_LEN, 0);
      }
      else
	OutputWithMargin (stdout, no1stMsg, MAX_SCREENLINE_LEN, 0);
    }
  }
  else {
    if (!IsCDF(CDFspec2)) {
      /************************************************************************
      * First specification is a CDF path, second is a directory/wildcard.
      ************************************************************************/
      numCDFs2 = CDFdirList (CDFspec2, &dirs2, &CDFs2);
      if (numCDFs2 > 0) {
	ParsePath (CDFspec1, dir1, CDF1);
	for (i = 0; i < numCDFs2; i++)
	   if (!strcmp(CDF1,CDFs2[i])) {
	     strcpyX (CDFpath2, dirs2[i], DU_MAX_PATH_LEN);
	     AppendToDir (CDFpath2, CDFs2[i]);
	     status2 = CompareTwoCDFs (CDFspec1, CDFpath2);
	     break;
	   }
	if (i == numCDFs2) OutputWithMargin (stdout, noMatch2Msg,
					     MAX_SCREENLINE_LEN, 0);
      }
      else
	OutputWithMargin (stdout, no2ndMsg, MAX_SCREENLINE_LEN, 0);
    }
    else {
      /************************************************************************
      * Both specifications are CDF paths.
      ************************************************************************/
      status2 = CompareTwoCDFs (CDFspec1, CDFspec2);
    }
  }
  if (dirs1 != NULL) cdf_FreeMemory (dirs1, FatalError);
  if (dirs2 != NULL) cdf_FreeMemory (dirs2, FatalError);
  if (CDFs1 != NULL) cdf_FreeMemory (CDFs1, FatalError);
  if (CDFs2 != NULL) cdf_FreeMemory (CDFs2, FatalError);
  return status2;
}

/******************************************************************************
* CompareTwoCDFs.
******************************************************************************/

Logical CompareTwoCDFs (CDFpath1, CDFpath2)
char *CDFpath1;
char *CDFpath2;
{
  return CompareTwoCDFx (CDFpath1, CDFpath2);
}

/******************************************************************************
* CompareTwoCDFx.
******************************************************************************/

Logical CompareTwoCDFx (CDFpath1, CDFpath2)
char *CDFpath1;
char *CDFpath2;
{
  CDFstatus status;
  static char noDiffMsg[] = "No differences detected.";
  /****************************************************************************
  * Display `comparing' message.
  ****************************************************************************/
  strcpyX (oText, "Comparing \"", 0);
  strcatX (oText, CDFpath1, 0);
  if (!EndsWithIgCase(CDFpath1, ".cdf"))
    strcatX (oText, ".cdf", 0);
  strcatX (oText, "\" with \"", 0);
  strcatX (oText, CDFpath2, 0);
  if (!EndsWithIgCase(CDFpath2, ".cdf"))
    strcatX (oText, ".cdf", 0);
  strcatX (oText, "\"\n", 0);
  OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 0);

  diffFound = FALSE;
  /****************************************************************************
  * Open CDFs.
  ****************************************************************************/
  CHECKforABORTso
  if (mLog) OutputWithMargin (stdout, "Opening CDF1", MAX_SCREENLINE_LEN, 2);
  status = CDFlib (OPEN_, CDF_, CDFpath1, &id1,
		   NULL_);
  if (!StatusHandlerCmp("CDF1",status)) return FALSE;
  CHECKforABORTso
  if (mLog) OutputWithMargin (stdout, "Opening CDF2", MAX_SCREENLINE_LEN, 2);
  status = CDFlib (OPEN_, CDF_, CDFpath2, &id2,
		   NULL_);
  if (!StatusHandlerCmp("CDF2",status)) {
    CDFclose (id1);
    return FALSE;
  }
  CHECKforABORTso
  /****************************************************************************
  * Select CDF modes.
  ****************************************************************************/
  status = CDFlib (SELECT_, CDF_, id1,
			    CDF_READONLY_MODE_, READONLYon,
			    CDF_zMODE_, zMode_1,
			    CDF_NEGtoPOSfp0_MODE_, BOO(negToPosFp0,
						       NEGtoPOSfp0on,
						       NEGtoPOSfp0off),
			    CDF_CACHESIZE_, workingCache,
			    STAGE_CACHESIZE_, stageCache,
			    COMPRESS_CACHESIZE_, compressCache,
		   NULL_);
  if (!StatusHandlerCmp("CDF1",status)) {
    CDFclose (id1);
    CDFclose (id2);
    return FALSE;
  }
  status = CDFlib (SELECT_, CDF_, id2,
			    CDF_READONLY_MODE_, READONLYon,
			    CDF_zMODE_, zMode_2,
			    CDF_NEGtoPOSfp0_MODE_,BOO(negToPosFp0,
						      NEGtoPOSfp0on,
						      NEGtoPOSfp0off),
			    CDF_CACHESIZE_, workingCache,
			    STAGE_CACHESIZE_, stageCache,
			    COMPRESS_CACHESIZE_, compressCache,
		   NULL_);
  if (!StatusHandlerCmp("CDF2",status)) {
    CDFclose (id1);
    CDFclose (id2);
    return FALSE;
  }
  CHECKforABORTso
  /****************************************************************************
  * Inquire general CDF contents.
  ****************************************************************************/
  status = CDFlib (SELECT_, CDF_, id1,
		   GET_, CDF_COMPRESSION_, &CDFcType1, CDFcParms1, &CDFcPct1,
			 CDF_ENCODING_, &encoding1,
			 CDF_MAJORITY_, &majority1,
			 CDF_FORMAT_, &format1,
			 CDF_COPYRIGHT_, copyright1,
			 CDF_NUMrVARS_, &rNumVars1,
			 CDF_NUMzVARS_, &zNumVars1,
			 CDF_NUMATTRS_, &numAttrs1,
			 CDF_VERSION_, &version1,
			 CDF_RELEASE_, &release1,
			 CDF_INCREMENT_, &increment1,
		   NULL_);
  if (!StatusHandlerCmp("CDF1",status)) {
    CDFclose (id1);
    CDFclose (id2);
    return FALSE;
  }
  if (!PriorTo ("3.5.0", version1, release1, increment1)) {
    status = CDFlib (GET_, CDF_LEAPSECONDLASTUPDATED_, &tt2000updated1,
		     NULL_);
    if (!StatusHandlerCmp("CDF1",status)) {
      CDFclose (id1);
      CDFclose (id2);
      return FALSE;
    }
  }
  status = CDFlib (SELECT_, CDF_, id1,
		   GET_, rVARs_NUMDIMS_, &rNumDims1,
			 rVARs_DIMSIZES_, rDimSizes1,
			 rVARs_MAXREC_, &CDFmaxRec1,
		   NULL_);
  if (!StatusHandlerCmp("CDF1",status)) {
    CDFclose (id1);
    CDFclose (id2);
    return FALSE;
  }
  if (!PriorTo ("3.2.0", (Int32) version1, (Int32) release1, (Int32) release1)) {
    status = CDFlib (SELECT_, CDF_, id1,
                     GET_, CDF_CHECKSUM_, &checksum1,
                     NULL_);
    if (!StatusHandlerCmp("CDF1",status)) {
      CDFclose (id1);
      CDFclose (id2);
      return FALSE;
    }
  }
  status = CDFlib (SELECT_, CDF_, id2,
		   GET_, CDF_COMPRESSION_, &CDFcType2, CDFcParms2, &CDFcPct2,
			 CDF_ENCODING_, &encoding2,
			 CDF_MAJORITY_, &majority2,
			 CDF_FORMAT_, &format2,
			 CDF_COPYRIGHT_, copyright2,
			 CDF_NUMrVARS_, &rNumVars2,
			 CDF_NUMzVARS_, &zNumVars2,
			 CDF_NUMATTRS_, &numAttrs2,
			 CDF_VERSION_, &version2,
			 CDF_RELEASE_, &release2,
			 CDF_INCREMENT_, &increment2, 
		   NULL_);
  if (!StatusHandlerCmp("CDF2",status)) {
    CDFclose (id1);
    CDFclose (id2);
    return FALSE;
  }
  if (!PriorTo ("3.5.0", version2, release2, increment2)) {
    status = CDFlib (GET_, CDF_LEAPSECONDLASTUPDATED_, &tt2000updated2,
		     NULL_);
    if (!StatusHandlerCmp("CDF2",status)) {
      CDFclose (id1);
      CDFclose (id2);
      return FALSE;
    }
  }
  status = CDFlib (SELECT_, CDF_, id2,
		   GET_, rVARs_NUMDIMS_, &rNumDims2,
			 rVARs_DIMSIZES_, rDimSizes2,
			 rVARs_MAXREC_, &CDFmaxRec2,
		   NULL_);
  if (!StatusHandlerCmp("CDF2",status)) {
    CDFclose (id1);
    CDFclose (id2);
    return FALSE;
  }
  if (!PriorTo ("3.2.0", (Int32) version2, (Int32) release2, (Int32) release2)) {
    status = CDFlib (SELECT_, CDF_, id2,
                     GET_, CDF_CHECKSUM_, &checksum2,
                     NULL_);
    if (!StatusHandlerCmp("CDF2",status)) {
      CDFclose (id1);
      CDFclose (id2);    
      return FALSE;      
    }                    
  }                      
  CHECKforABORTso
  /****************************************************************************
  * Compare numbering.
  ****************************************************************************/
  if (!AttrNumberMatches()) {
    CDFclose (id1);
    CDFclose (id2);
    return FALSE;
  }
  if (!VarNumberMatches(FALSE)) {
    CDFclose (id1);
    CDFclose (id2);
    return FALSE;
  }
  if (!VarNumberMatches(TRUE)) {
    CDFclose (id1);
    CDFclose (id2);
    return FALSE;
  }
  CHECKforABORTso
  /****************************************************************************
  * Compare CDFs.
  ****************************************************************************/
  if (!CompareGeneral()) {
    CDFclose (id1);
    CDFclose (id2);
    return FALSE;
  }
  CHECKforABORTso
  if (cmpAttrs) {
    if (!CompareAttributes()) {
      CDFclose (id1);
      CDFclose (id2);
      return FALSE;
    }
  }
  if (cmpVars) {
    if (!CompareZvsR()) {
      CDFclose (id1);
      CDFclose (id2);
      return FALSE;
    }
    if (!CompareVariables(FALSE)) {
      CDFclose (id1);
      CDFclose (id2);
      return FALSE;
    }
    if (!CompareVariables(TRUE)) {
      CDFclose (id1);
      CDFclose (id2);
      return FALSE;
    }
  }
  /****************************************************************************
  * Close CDFs and set their id's to NULL.
  ****************************************************************************/
  if (mLog) OutputWithMargin (stdout, "Closing CDFs", MAX_SCREENLINE_LEN, 2);
  if (dumpStatistics) {
    vSTATS vStatsDotCDF, vStatsStage, vStatsCompress;
    status = CDFlib (SELECT_, CDF_, id1,
		     CLOSE_, CDFwithSTATS_, &vStatsDotCDF,
					    &vStatsStage,
					    &vStatsCompress,
		     NULL_);
    if (!StatusHandlerCmp("CDF1",status)) {
      CDFclose (id2);
      return FALSE;
    }
    DisplayStatistics ("CDF1", &vStatsDotCDF, &vStatsStage, &vStatsCompress);
    status = CDFlib (SELECT_, CDF_, id2,
		     CLOSE_, CDFwithSTATS_, &vStatsDotCDF,
					    &vStatsStage,
					    &vStatsCompress,
		     NULL_);
    if (!StatusHandlerCmp("CDF2",status)) return FALSE;
    DisplayStatistics ("CDF2", &vStatsDotCDF, &vStatsStage, &vStatsCompress);
  }
  else {
    status = CDFlib (SELECT_, CDF_, id1,
		     CLOSE_, CDF_,
		     NULL_);
    if (!StatusHandlerCmp("CDF1",status)) {
      CDFclose (id2);
      return FALSE;
    }
    status = CDFlib (SELECT_, CDF_, id2,
		     CLOSE_, CDF_,
		     NULL_);
    if (!StatusHandlerCmp("CDF2",status)) return FALSE;
  }
  /****************************************************************************
  * If no differences, display message and return.
  ****************************************************************************/
  if ((!diffFound) && mLog) {
    OutputWithMargin (stdout, noDiffMsg, MAX_SCREENLINE_LEN, 2);
  }
  /****************************************************************************
  * Free memory used for number matching.
  ****************************************************************************/
  if (attrNumMatches1 != NULL) cdf_FreeMemory (attrNumMatches1, FatalError);
  if (attrNumMatches2 != NULL) cdf_FreeMemory (attrNumMatches2, FatalError);
  if (rVarNumMatches1 != NULL) cdf_FreeMemory (rVarNumMatches1, FatalError);
  if (rVarNumMatches2 != NULL) cdf_FreeMemory (rVarNumMatches2, FatalError);
  if (zVarNumMatches1 != NULL) cdf_FreeMemory (zVarNumMatches1, FatalError);
  if (zVarNumMatches2 != NULL) cdf_FreeMemory (zVarNumMatches2, FatalError);
  return TRUE;
}

/******************************************************************************
* CompareGeneral.
******************************************************************************/

Logical CompareGeneral ()
{
  int i, j;
  /****************************************************************************
  * Compare compressions.
  ****************************************************************************/
  if (cmpEtc && (!SameCompressions(CDFcType1,CDFcParms1,
				   CDFcType2,CDFcParms2))) {
    diffFound = TRUE;
    snprintf (oText, (size_t) sizeof(oText),
	      "DIFFERENT CDF compressions (%s vs. ",
	      CompressionToken(CDFcType1,CDFcParms1));
    snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
	      "%s)", CompressionToken(CDFcType2,CDFcParms2));
    OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
  }
  if (cmpEtc && (CDFcPct1 != CDFcPct2)) {
    diffFound = TRUE;
    snprintf (oText, (size_t) sizeof(oText),
	      "DIFFERENT CDF compression %%s (compressed/uncompressed) (%ld%% vs. %ld%%)",
	      CDFcPct1, CDFcPct2);
    OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
  }
  /****************************************************************************
  * Compare rVariable dimensionalities.
  ****************************************************************************/
  if (rNumDims1 != rNumDims2) {
    diffFound = TRUE;
    snprintf (oText, (size_t) sizeof(oText),
	      "DIFFERENT number of rVariable dimensions (%ld vs. %ld)",
	      rNumDims1, rNumDims2);
    OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
    cmpVars = FALSE;
  }
  else {
    if (rNumDims1 > 0)
      for (i = 0; i < rNumDims1; i++)
	 if (rDimSizes1[i] != rDimSizes2[i]) {
	   diffFound = TRUE;
	   snprintf (oText, (size_t) sizeof(oText),
		     "DIFFERENT rVariable dimension sizes (");
	   for (j = 0; j < rNumDims1; j++) {
	      snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
			"%ld ", rDimSizes1[j]);
	   }
	   strcatX (oText, "vs.", MAX_oTEXT_LEN);
	   for (j = 0; j < rNumDims2; j++) {
	      snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
			" %ld", rDimSizes2[j]);
	   }
	   strcatX (oText, ")", MAX_oTEXT_LEN);
	   OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
	   cmpVars = FALSE;
	   break;
	 }
  }
  /****************************************************************************
  * Compare encodings.
  ****************************************************************************/
  if (cmpEtc && (encoding1 != encoding2)) {
    diffFound = TRUE;
    snprintf (oText, (size_t) sizeof(oText),
	      "DIFFERENT encodings (%s vs. %s)",
	      EncodingToken(encoding1), EncodingToken(encoding2));
    OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
  }
  /****************************************************************************
  * Compare variable majorities.
  ****************************************************************************/
  if (cmpEtc && (majority1 != majority2)) {
    diffFound = TRUE;
    snprintf (oText, (size_t) sizeof(oText),
	      "DIFFERENT majorities (%s vs. %s)",
	      MajorityToken(majority1), MajorityToken(majority2));
    OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
  }
  /****************************************************************************
  * Compare formats.
  ****************************************************************************/
  if (cmpEtc && (format1 != format2)) {
    diffFound = TRUE;
    snprintf (oText, (size_t) sizeof(oText),
	      "DIFFERENT formats (%s vs. %s)",
	      FormatToken(format1), FormatToken(format2));
    OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
  }
  /****************************************************************************
  * Compare checksums if applicable.
  ****************************************************************************/
  if (checksum1 != -999 || checksum2 != -999) {
    if (cmpEtc && (checksum1 != checksum2)) {
      diffFound = TRUE;
      if (checksum1 == -999) checksum1 = 0;
      if (checksum2 == -999) checksum2 = 0;
      if (checksum1 != checksum2) {
        snprintf (oText, (size_t) sizeof(oText),
		  "DIFFERENT checksums (%s vs. %s)",
                  ChecksumToken(checksum1), ChecksumToken(checksum2));
        OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
      }
    }
  }
  /****************************************************************************
  * Compare rVariable counts.
  ****************************************************************************/
  if (rNumVars1 != rNumVars2) {
    diffFound = TRUE;
    snprintf (oText, (size_t) sizeof(oText),
	      "DIFFERENT number of rVariables (%ld vs. %ld)",
	      rNumVars1, rNumVars2);
    OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
  }
  /****************************************************************************
  * Compare zVariable counts.
  ****************************************************************************/
  if (zNumVars1 != zNumVars2) {
    diffFound = TRUE;
    snprintf (oText, (size_t) sizeof(oText),
	      "DIFFERENT number of zVariables (%ld vs. %ld)",
	      zNumVars1, zNumVars2);
    OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
  }
  /****************************************************************************
  * Compare attribute counts.
  ****************************************************************************/
  if (numAttrs1 != numAttrs2) {
    diffFound = TRUE;
    snprintf (oText, (size_t) sizeof(oText),
	      "DIFFERENT number of attributes (%ld vs. %ld)",
	      numAttrs1, numAttrs2);
    OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
  }
  /****************************************************************************
  * Compare rVariable record counts.
  ****************************************************************************/
  if (CDFmaxRec1 != CDFmaxRec2) {
    diffFound = TRUE;
    snprintf (oText, (size_t) sizeof(oText),
	      "DIFFERENT maximum rVariable record (%ld vs. %ld)",
	      CDFmaxRec1 + 1, CDFmaxRec2 + 1);
    OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
  }
  /****************************************************************************
  * Compare version/release/increments.
  ****************************************************************************/
  if (cmpEtc && (version1 != version2 ||
		 release1 != release2 ||
		 increment1 != increment2)) {
    diffFound = TRUE;
    strcpyX (oText, "DIFFERENT creation libraries", MAX_oTEXT_LEN);
    snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
	      " (V%ld.%ld.%ld vs. V%ld.%ld.%ld)",
	      version1, release1, increment1, version2, release2, increment2);
    OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
  }
  /****************************************************************************
  * Compare leap last updated.
  ****************************************************************************/
  if (cmpEtc && (tt2000updated1 != tt2000updated2)) {
    diffFound = TRUE;
    strcpyX (oText, "DIFFERENT leap second table used", MAX_oTEXT_LEN);
    snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
	      " (%ld vs. %ld)", tt2000updated1, tt2000updated2);
    OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
  }
  return TRUE;
}

/******************************************************************************
* AttrNumberMatches.  Determine attribute number matches.
******************************************************************************/

Logical AttrNumberMatches () {
  CDFstatus status;
  long attrN1, attrN2;
  char attrName[CDF_ATTR_NAME_LEN256+1];

  if (numAttrs1 > 0)
    attrNumMatches1 = (long *) cdf_AllocateMemory ((size_t) (numAttrs1 *
							 sizeof(long)),
					       FatalError);
  else
    attrNumMatches1 = (long *) NULL;

  for (attrN1 = 0; attrN1 < numAttrs1; attrN1++) {
     status = CDFlib (SELECT_, CDF_, id1,
			       ATTR_, attrN1,
		      GET_, ATTR_NAME_, attrName,
		      NULL_);
     if (!StatusHandlerCmp("CDF1",status)) return FALSE;

     status = CDFlib (SELECT_, CDF_, id2,
		      GET_, ATTR_NUMBER_, attrName, &attrN2,
		      NULL_);
     if (status == NO_SUCH_ATTR)
       attrNumMatches1[(int)attrN1] = -1;
     else {
       if (!StatusHandlerCmp("CDF2",status)) return FALSE;
       attrNumMatches1[(int)attrN1] = attrN2;
     }
  }

  if (numAttrs2 > 0)
    attrNumMatches2 = (long *) cdf_AllocateMemory ((size_t) (numAttrs2 *
							 sizeof(long)),
					       FatalError);
  else
    attrNumMatches2 = (long *) NULL;

  for (attrN2 = 0; attrN2 < numAttrs2; attrN2++) {
     status = CDFlib (SELECT_, CDF_, id2,
			       ATTR_, attrN2,
		      GET_, ATTR_NAME_, attrName,
		      NULL_);
     if (!StatusHandlerCmp("CDF2",status)) return FALSE;

     status = CDFlib (SELECT_, CDF_, id1,
		      GET_, ATTR_NUMBER_, attrName, &attrN1,
		      NULL_);
     if (status == NO_SUCH_ATTR)
       attrNumMatches2[(int)attrN2] = -1;
     else {
       if (!StatusHandlerCmp("CDF1",status)) return FALSE;
       attrNumMatches2[(int)attrN2] = attrN1;
     }
  }

  return TRUE;
}

/******************************************************************************
* VarNumberMatches.  Determine variable number matches.
******************************************************************************/

Logical VarNumberMatches (Z)
Logical Z;
{
  CDFstatus status;
  long varN1, varN2;
  char varName[CDF_VAR_NAME_LEN256+1];
  /****************************************************************************
  * Allocate matching number array for CDF1.
  ****************************************************************************/
  if (BOO(Z,zNumVars1,rNumVars1) > 0) {
    if (Z)
      zVarNumMatches1 = (long *) cdf_AllocateMemory ((size_t) (zNumVars1 *
							   sizeof(long)),
						 FatalError);
    else
      rVarNumMatches1 = (long *) cdf_AllocateMemory ((size_t) (rNumVars1 *
							  sizeof(long)),
						FatalError);
  }
  else
    if (Z)
      zVarNumMatches1 = NULL;
    else
      rVarNumMatches1 = NULL;
  /****************************************************************************
  * Find matching variable numbers for CDF1.
  ****************************************************************************/
  for (varN1 = 0; varN1 < BOO(Z,zNumVars1,rNumVars1); varN1++) {
     if (BOO(Z,zNumVars2,rNumVars2) > 0) {
       status = CDFlib (SELECT_, CDF_, id1,
				 VAR(Z), varN1,
			GET_, VAR_NAME(Z), varName,
			NULL_);
       if (!StatusHandlerCmp("CDF1",status)) return FALSE;
       status = CDFlib (SELECT_, CDF_, id2,
			GET_, VAR_NUMBER(Z), varName, &varN2,
			NULL_);
       if (status == NO_SUCH_VAR)
	 if (Z)
	   zVarNumMatches1[(int)varN1] = -1;
	 else
	   rVarNumMatches1[(int)varN1] = -1;
       else {
	 if (!StatusHandlerCmp("CDF2",status)) return FALSE;
	 if (Z)
	   zVarNumMatches1[(int)varN1] = varN2;
	 else
	   rVarNumMatches1[(int)varN1] = varN2;
       }
     }
     else
       if (Z)
	 zVarNumMatches1[(int)varN1] = -1;
       else
	 rVarNumMatches1[(int)varN1] = -1;
  }
  /****************************************************************************
  * Allocate matching number array for CDF2.
  ****************************************************************************/
  if (BOO(Z,zNumVars2,rNumVars2) > 0) {
    if (Z)
      zVarNumMatches2 = (long *) cdf_AllocateMemory ((size_t) (zNumVars2 *
							   sizeof(long)),
						 FatalError);
    else
      rVarNumMatches2 = (long *) cdf_AllocateMemory ((size_t) (rNumVars2 *
							  sizeof(long)),
						FatalError);
  }
  else
    if (Z)
      zVarNumMatches2 = NULL;
    else
      rVarNumMatches2 = NULL;
  /****************************************************************************
  * Find matching variable numbers for CDF2.
  ****************************************************************************/
  for (varN2 = 0; varN2 < BOO(Z,zNumVars2,rNumVars2); varN2++) {
     if (BOO(Z,zNumVars1,rNumVars1) > 0) {
       status = CDFlib (SELECT_, CDF_, id2,
				 VAR(Z), varN2,
			GET_, VAR_NAME(Z), varName,
			NULL_);
       if (!StatusHandlerCmp("CDF2",status)) return FALSE;
       status = CDFlib (SELECT_, CDF_, id1,
			GET_, VAR_NUMBER(Z), varName, &varN1,
			NULL_);
       if (status == NO_SUCH_VAR)
	 if (Z)
	   zVarNumMatches2[(int)varN2] = -1;
	 else
	   rVarNumMatches2[(int)varN2] = -1;
       else {
	 if (!StatusHandlerCmp("CDF1",status)) return FALSE;
	 if (Z)
	   zVarNumMatches2[(int)varN2] = varN1;
	 else
	   rVarNumMatches2[(int)varN2] = varN1;
       }
     }
     else
       if (Z)
	 zVarNumMatches2[(int)varN2] = -1;
       else
	 rVarNumMatches2[(int)varN2] = -1;
  }
  return TRUE;
}

/******************************************************************************
* CompareAttributes.
******************************************************************************/

Logical CompareAttributes ()
{
  CDFstatus status;
  long attrN1, attrN2;
  char attrName[CDF_ATTR_NAME_LEN256+1];
  Logical cmpEntries;

  for (attrN1 = 0; attrN1 < numAttrs1; attrN1++) {
     if (attrNumMatches1[(int)attrN1] == -1) {
       status = CDFlib (SELECT_, CDF_, id1,
				 ATTR_, attrN1,
			GET_, ATTR_NAME_, attrName,
			NULL_);
       if (!StatusHandlerCmp("CDF1",status)) return FALSE;
       diffFound = TRUE;
       snprintf (oText, (size_t) sizeof(oText),
		 "Attribute \"%s\" does not exist in CDF2", attrName);
       OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 2);
     }
     else {
       status = CDFlib (SELECT_, CDF_, id1,
				 ATTR_, attrN1,
			GET_, ATTR_NAME_, attrName,
			      ATTR_SCOPE_, &attrScope1,
			NULL_);
       if (!StatusHandlerCmp("CDF1",status)) return FALSE;

       if (attrScope1 == GLOBAL_SCOPE) {
	 attrMAXzEntry1 = -1;
	 attrNUMzEntries1 = 0;
	 status = CDFlib (SELECT_, CDF_, id1,
			  GET_, ATTR_MAXgENTRY_, &attrMAXgrEntry1,
				ATTR_NUMgENTRIES_, &attrNUMgrEntries1,
			  NULL_);
       }
       else
	 status = CDFlib (SELECT_, CDF_, id1,
			  GET_, ATTR_MAXrENTRY_, &attrMAXgrEntry1,
				ATTR_NUMrENTRIES_, &attrNUMgrEntries1,
				ATTR_MAXzENTRY_, &attrMAXzEntry1,
				ATTR_NUMzENTRIES_, &attrNUMzEntries1,
			  NULL_);
       if (!StatusHandlerCmp("CDF1",status)) return FALSE;

       status = CDFlib (SELECT_, CDF_, id2,
				 ATTR_, attrNumMatches1[(int)attrN1],
			GET_, ATTR_SCOPE_, &attrScope2,
			NULL_);
       if (!StatusHandlerCmp("CDF2",status)) return FALSE;

       if (attrScope2 == GLOBAL_SCOPE) {
	 attrMAXzEntry2 = -1;
	 attrNUMzEntries2 = 0;
	 status = CDFlib (SELECT_, CDF_, id2,
			  GET_, ATTR_MAXgENTRY_, &attrMAXgrEntry2,
				ATTR_NUMgENTRIES_, &attrNUMgrEntries2,
			  NULL_);
       }
       else
	 status = CDFlib (SELECT_, CDF_, id2,
			  GET_, ATTR_MAXrENTRY_, &attrMAXgrEntry2,
				ATTR_NUMrENTRIES_, &attrNUMgrEntries2,
				ATTR_MAXzENTRY_, &attrMAXzEntry2,
				ATTR_NUMzENTRIES_, &attrNUMzEntries2,
			  NULL_);
       if (!StatusHandlerCmp("CDF2",status)) return FALSE;

       if (mLog) {
	 snprintf (oText, (size_t) sizeof(oText),
		   "Comparing attribute \"%s\"", attrName);
	 OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 2);
       }

       cmpEntries = TRUE;

       if (cmpNumbers && (attrNumMatches1[(int)attrN1] != attrN1)) {
	 diffFound = TRUE;
	 strcpyX (oText, "DIFFERENT numbers for attribute", MAX_oTEXT_LEN);
	 snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
		   " \"%s\" (%ld vs. %ld)",
		   attrName, attrN1 + 1, attrNumMatches1[(int)attrN1] + 1);
	 OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
       }

       if (attrScope1 != attrScope2) {
	 diffFound = TRUE;
	 snprintf (oText, (size_t) sizeof(oText),
		   "DIFFERENT scopes for attribute \"%s\" (%s vs. %s)",
		   attrName, ScopeToken(attrScope1), ScopeToken(attrScope2));
	 OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
	 /* MAY STILL BE ABLE TO COMPARE ENTRIES. */
       }

       if (attrMAXgrEntry1 != attrMAXgrEntry2) {
	 diffFound = TRUE;
	 strcpyX (oText, "DIFFERENT maximum entry for attribute",
		  MAX_oTEXT_LEN);
	 snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
		   " \"%s\" (%ld vs. %ld)",
		   attrName, attrMAXgrEntry1 + 1, attrMAXgrEntry2 + 1);
	 OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
       }

       if (attrNUMgrEntries1 != attrNUMgrEntries2) {
	 diffFound = TRUE;
	 strcpyX (oText, "DIFFERENT number of entries for attribute",
		  MAX_oTEXT_LEN);
	 snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
		   " \"%s\" (%ld vs. %ld)",
		   attrName, attrNUMgrEntries1, attrNUMgrEntries2);
	 OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
       }

       if (attrMAXzEntry1 != attrMAXzEntry2) {
	 diffFound = TRUE;
	 strcpyX (oText, "DIFFERENT maximum zEntry for attribute",
		  MAX_oTEXT_LEN);
	 snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
		   " \"%s\" (%ld vs. %ld)",
		   attrName, attrMAXzEntry1 + 1, attrMAXzEntry2 + 1);
	 OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
       }

       if (attrNUMzEntries1 != attrNUMzEntries2) {
	 diffFound = TRUE;
	 strcpyX (oText, "DIFFERENT number of zEntries for attribute",
		  MAX_oTEXT_LEN);
	 snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
		   " \"%s\" (%ld vs. %ld)",
		   attrName, attrNUMzEntries1, attrNUMzEntries2);
	 OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
       }

       if (cmpEntries) CompareAttributeEntries (attrN1,
						attrNumMatches1[(int)attrN1],
						attrName);
     }
     CHECKforABORTso
  }

  for (attrN2 = 0; attrN2 < numAttrs2; attrN2++) {
     if (attrNumMatches2[(int)attrN2] == -1) { 
       status = CDFlib (SELECT_, CDF_, id2,
				 ATTR_, attrN2,
			GET_, ATTR_NAME_, attrName,
			NULL_);
       if (!StatusHandlerCmp("CDF2",status)) return FALSE;
       diffFound = TRUE;
       snprintf (oText, (size_t) sizeof(oText),
		 "Attribute \"%s\" does not exist in CDF1", attrName);
       OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 2);
     }
  }

  return TRUE;
}

/******************************************************************************
* CompareAttributeEntries.
******************************************************************************/

Logical CompareAttributeEntries (attrN1, attrN2, attrName)
long attrN1;
long attrN2;
char *attrName;
{
  CDFstatus status;
  long entryN;

  if (mLog) OutputWithMargin (stdout, "Comparing entries...",
			      MAX_SCREENLINE_LEN, 4);

  status = CDFlib (SELECT_, CDF_, id1,
			    ATTR_, attrN1,
		   NULL_);
  if (!StatusHandlerCmp("CDF1",status)) return FALSE;

  status = CDFlib (SELECT_, CDF_, id2,
			    ATTR_, attrN2,
		   NULL_);
  if (!StatusHandlerCmp("CDF2",status)) return FALSE;

  if (attrScope1 == GLOBAL_SCOPE) {
    for (entryN = 0;
	 entryN <= MAXIMUM(attrMAXgrEntry1,attrMAXgrEntry2);
	 entryN++) CompareEntry (attrName, entryN, entryN, NULL, gENTRYt);
  }
  else {
    CompareVscopeEntries (attrName, FALSE);
    CompareVscopeEntries (attrName, TRUE);
  }

  return TRUE;
}

/******************************************************************************
* CompareVscopeEntries.
******************************************************************************/

Logical CompareVscopeEntries (attrName, Z)
char *attrName;
Logical Z;
{
  CDFstatus status;
  long entryN1;
  long entryN2;
  char varName[CDF_VAR_NAME_LEN256+1];
  char *v = BOO(Z,zVariable,rVariable);
  char *e = BOO(Z,zEntry,rEntry);

  /****************************************************************************
  * Check each entry in CDF1 up to the maximum entry for CDF1.
  ****************************************************************************/
  for (entryN1 = 0;
       entryN1 <= BOO(Z,attrMAXzEntry1,attrMAXgrEntry1);
       entryN1++) {
     if (entryN1 < BOO(Z,zNumVars1,rNumVars1)) {
       if (BOO(Z,zVarNumMatches1[(int)entryN1],
		 rVarNumMatches1[(int)entryN1]) == -1) {
	 diffFound = TRUE;
	 snprintf (oText, (size_t) sizeof(oText),
		   "No corresponding %s in CDF2 for %s number", v, e);
	 snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
		   " %ld of attribute \"%s\" in CDF1",
		   entryN1 + 1, attrName);
	 OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
       }
       else {
	 status = CDFlib (SELECT_, CDF_, id1,
				   VAR(Z), entryN1,
			  GET_, VAR_NAME(Z), varName,
			  NULL_);
	 if (!StatusHandlerCmp("CDF1",status)) return FALSE;
	 CompareEntry (attrName, entryN1, BOO(Z,zVarNumMatches1[(int)entryN1],
					        rVarNumMatches1[(int)entryN1]),
		       varName, BOO(Z,zENTRYt,rENTRYt));
       }
     }
     else {
       CompareEntry (attrName, entryN1, entryN1, NULL, BOO(Z,zENTRYt,rENTRYt));
     }
  }

  for (entryN2 = 0;
       entryN2 <= BOO(Z,attrMAXzEntry2,attrMAXgrEntry2);
       entryN2++) {
     if (entryN2 < BOO(Z,zNumVars2,rNumVars2)) {
       if (BOO(Z,zVarNumMatches2[(int)entryN2],
		 rVarNumMatches2[(int)entryN2]) == -1) {
	 diffFound = TRUE;
	 snprintf (oText, (size_t) sizeof(oText),
		   "No corresponding %s in CDF1 for %s number", v, e);
	 snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
		   " %ld of attribute \"%s\" in CDF2",
		   entryN2 + 1, attrName);
	 OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
       }
     }
     else
       CompareEntry (attrName, entryN2, entryN2, NULL, BOO(Z,zENTRYt,rENTRYt));
  }
  return TRUE;
}

/******************************************************************************
* CompareEntry.    It is assumed that the attribute has already been selected
*                  in the two CDFs.
******************************************************************************/

Logical CompareEntry (attrName, entryN1, entryN2, varName, entryType)
char *attrName;
long entryN1;
long entryN2;
char *varName;          /* If != NULL, then a variable name associated with
			   the entries. */
int entryType;
{
  CDFstatus status, status1, status2;
  long entryDataType1, entryDataType2;
  long entryNumElems1, entryNumElems2;
  void *entryValue1, *entryValue2;
  char varNameField[VAR_NAME_FIELD_LEN];
  char entryNumField[ENTRY_NUM_FIELD_LEN];
  long Nbytes;
Logical comp;
  char *v = E3(entryType,NULL,rVariable,zVariable);
  char *e = E3(entryType,gEntry,rEntry,zEntry);
  Logical badStatus1 = FALSE, badStatus2 = FALSE, noSuch = FALSE;

  if (varName == NULL) {
    varNameField[0] = '\0';
    snprintf (entryNumField, (size_t) sizeof(entryNumField),
	      "%ld", entryN1 + 1);
  }
  else {
    strcpyX (varNameField, " (", VAR_NAME_FIELD_LEN);
    strcatX (varNameField, v, VAR_NAME_FIELD_LEN);
    strcatX (varNameField, " \"", VAR_NAME_FIELD_LEN);
    strcatX (varNameField, varName, VAR_NAME_FIELD_LEN);
    strcatX (varNameField, "\")", VAR_NAME_FIELD_LEN);
    if (entryN1 == entryN2)
      snprintf (entryNumField, (size_t) sizeof(entryNumField),
		"%ld", entryN1 + 1);
    else
      snprintf (entryNumField, (size_t) sizeof(entryNumField),
		"%ld/%ld", entryN1 + 1, entryN2 + 1);
  }

  status1 = CDFlib (SELECT_, CDF_, id1,
		    CONFIRM_, ENTRY_EXISTENCE(entryType), entryN1,
		    SELECT_, ENTRY(entryType), entryN1,
		    GET_, ENTRY_DATATYPE(entryType), &entryDataType1,
			  ENTRY_NUMELEMS(entryType), &entryNumElems1,
		    NULL_);

  status2 = CDFlib (SELECT_, CDF_, id2,
		    CONFIRM_, ENTRY_EXISTENCE(entryType), entryN2,
		    SELECT_, ENTRY(entryType), entryN2,
		    GET_, ENTRY_DATATYPE(entryType), &entryDataType2,
			  ENTRY_NUMELEMS(entryType), &entryNumElems2,
		    NULL_);

  if (status1 == NO_SUCH_ENTRY && status2 == NO_SUCH_ENTRY) return TRUE;

  if (status1 == NO_SUCH_ENTRY) {
    diffFound = TRUE;
    snprintf (oText, (size_t) sizeof(oText),
	      "No %s number %ld%s for attribute \"%s\" in CDF1",
	      e, entryN1 + 1, varNameField, attrName);
    OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
    noSuch = TRUE;
  }
  else
    badStatus1 = !StatusHandlerCmp("CDF1",status1);

  if (status2 == NO_SUCH_ENTRY) {
    diffFound = TRUE;
    snprintf (oText, (size_t) sizeof(oText),
	      "No %s number %ld%s for attribute \"%s\" in CDF2",
	      e, entryN2 + 1, varNameField, attrName);
    OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
    noSuch = TRUE;
  }
  else
    badStatus2 = !StatusHandlerCmp("CDF2",status2);

  if (noSuch) return TRUE;
  if (badStatus1 || badStatus2) return FALSE;

  if (cmpEtc && (entryDataType1 != entryDataType2)) {
    diffFound = TRUE;
    strcpyX (oText, "DIFFERENT data types for", MAX_oTEXT_LEN);
    snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
	      " %s number %s%s of attribute \"%s\" ",
	      e, entryNumField, varNameField, attrName);
    snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
	      "(CDF_%s vs. CDF_%s)",
	      DataTypeToken(entryDataType1), DataTypeToken(entryDataType2));
    OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
    /* DON'T RETURN IF THIS DIFFERENCE FOUND - COMPARE STILL POSSIBLE. */
  }

  if (!EquivalentDataTypes(entryDataType1,entryDataType2)) {
    diffFound = TRUE;
    strcpyX (oText, "NON-EQUIVALENT data types for", MAX_oTEXT_LEN);
    snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
	      " %s number %s%s of attribute \"%s\" ",
	      e, entryNumField, varNameField, attrName);
    snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
	      "(CDF_%s vs. CDF_%s)",
	      DataTypeToken(entryDataType1), DataTypeToken(entryDataType2));
    OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
    return TRUE;
  }

  if (entryNumElems1 != entryNumElems2) {
    diffFound = TRUE;
    snprintf (oText, (size_t) sizeof(oText),
	      "DIFFERENT number of elements for %s number %s",
	      e, entryNumField);
    snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
	      "%s of attribute \"%s\" (%ld vs. %ld)",
	      varNameField, attrName, entryNumElems1, entryNumElems2);
    OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
    return TRUE;
  }

  Nbytes = entryNumElems1 * CDFelemSize(entryDataType1);

  entryValue1 = cdf_AllocateMemory ((size_t) Nbytes, FatalError);
  entryValue2 = cdf_AllocateMemory ((size_t) Nbytes, FatalError);

  status = CDFlib (SELECT_, CDF_, id1,
		   GET_, ENTRY_DATA(entryType), entryValue1,
		   NULL_);
  if (!StatusHandlerCmp("CDF1",status)) return FALSE;

  status = CDFlib (SELECT_, CDF_, id2,
		   GET_, ENTRY_DATA(entryType), entryValue2,
		   NULL_);
  if (!StatusHandlerCmp("CDF2",status)) return FALSE;

  if (memcmp(entryValue1,entryValue2,(size_t)Nbytes) != 0) {
    comp = FALSE;
    if ((entryDataType1 == CDF_INT1) || (entryDataType1 == CDF_INT2) ||
	(entryDataType1 == CDF_INT4) || (entryDataType1 == CDF_UINT1) ||
	(entryDataType1 == CDF_UINT2) || (entryDataType1 == CDF_UINT4) ||
	(entryDataType1 == CDF_INT8) || (entryDataType1 == CDF_TIME_TT2000)) {
      comp = TRUE;	
    } else if (STRINGdataType(entryDataType1)) {
       comp = CompareStrings((char *)entryValue1, (char *)entryValue2,
			     entryNumElems1, Nbytes);
    } else {
      if ((!floatSet && ((entryDataType1 == CDF_FLOAT) ||
			 (entryDataType1 == CDF_REAL4))
		     && (CheckNaN(entryValue1, entryValue2, entryDataType1) > 1)) ||
          (!doubleSet && ((entryDataType1 == CDF_REAL8) ||
			  (entryDataType1 == CDF_DOUBLE))
		      && (CheckNaN(entryValue1, entryValue2, entryDataType1) > 1)) ||
          (!doubleSet && ((entryDataType1 == CDF_EPOCH) ||
			  (entryDataType1 == CDF_EPOCH16))) ||
          (floatSet && ((entryDataType1 == CDF_FLOAT) ||
			(entryDataType1 == CDF_REAL4))) ||
          (doubleSet && ((entryDataType1 == CDF_REAL8) ||
			 (entryDataType1 == CDF_DOUBLE) ||
		 	 (entryDataType1 == CDF_EPOCH) ||
		 	 (entryDataType1 == CDF_EPOCH16)))) {
	comp = ValuesCmp(entryValue1, entryValue2, entryDataType1, 
			 (double) floatTolerance, doubleTolerance);
      }
    }
    if (comp) {
      diffFound = TRUE;
      snprintf (oText, (size_t) sizeof(oText),
	  	"DIFFERENT value(s) for %s number %s%s of attribute \"%s\"",
	        e, entryNumField, varNameField, attrName);
      OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
      if (displayValue) {
        WriteOut (stdout, "      CDF1 value(s)> ");
        WriteEntryValue (stdout, entryDataType1, entryNumElems1, entryValue1,
		         21, MAX_SCREENLINE_LEN-1);
        WriteOut (stdout, "\n");
        WriteOut (stdout, "      CDF2 value(s)> ");
        WriteEntryValue (stdout, entryDataType2, entryNumElems2, entryValue2,
		         21, MAX_SCREENLINE_LEN-1);
        WriteOut (stdout, "\n");
      }
    }
  }

  cdf_FreeMemory (entryValue1, FatalError);
  cdf_FreeMemory (entryValue2, FatalError);

  CHECKforABORTso
  return TRUE;
}

/******************************************************************************
* CompareZvsR.
******************************************************************************/

Logical CompareZvsR () {
  CDFstatus status;
  long varN1, varN2;
  char varName1[CDF_VAR_NAME_LEN256+1];
  /****************************************************************************
  * If there are rVariables in CDF1 and zVariables in CDF2, see if any of the
  * rVariables in CDF1 have the same name as a zVariable in CDF2.
  ****************************************************************************/
  if (rNumVars1 > 0 && zNumVars2 > 0) {
    for (varN1 = 0; varN1 < rNumVars1; varN1++) {
       status = CDFlib (SELECT_, CDF_, id1,
				 rVAR_, varN1,
			GET_, rVAR_NAME_, varName1,
			NULL_);
       if (!StatusHandlerCmp("CDF1",status)) return FALSE;
       status = CDFlib (SELECT_, CDF_, id2,
			GET_, zVAR_NUMBER_, varName1, &varN2,
			NULL_);
       switch (status) {
	 case CDF_OK:
	   snprintf (oText, (size_t) sizeof(oText),
		     "zVariable \"%s\" in CDF2 is an rVariable in CDF1",
		     varName1);
	   OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 2);
	   break;
	 case NO_SUCH_VAR:
	   break;
	 default:
	   if (!StatusHandlerCmp("CDF2",status)) return FALSE;
	   break;
       }
       CHECKforABORTso
    }
  }
  /****************************************************************************
  * If there are zVariables in CDF1 and rVariables in CDF2, see if any of the
  * zVariables in CDF1 have the same name as an rVariable in CDF2.
  ****************************************************************************/
  if (zNumVars1 > 0 && rNumVars2 > 0) {
    for (varN1 = 0; varN1 < zNumVars1; varN1++) {
       status = CDFlib (SELECT_, CDF_, id1,
				 zVAR_, varN1,
			GET_, zVAR_NAME_, varName1,
			NULL_);
       if (!(StatusHandlerCmp("CDF1",status))) return FALSE;
       status = CDFlib (SELECT_, CDF_, id2,
			GET_, rVAR_NUMBER_, varName1, &varN2,
			NULL_);
       switch (status) {
	 case CDF_OK:
	   snprintf (oText, (size_t) sizeof(oText),
		     "zVariable \"%s\" in CDF1 is an rVariable in CDF2",
		     varName1);
	   OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 2);
	   break;
	 case NO_SUCH_VAR:
	   break;
	 default:
	   if (!StatusHandlerCmp("CDF2",status)) return FALSE;
	   break;
       }
       CHECKforABORTso
    }
  }
  return TRUE;
}

/******************************************************************************
* CompareVariables.
******************************************************************************/

Logical CompareVariables (Z)
Logical Z;
{
  CDFstatus status; long varN1, varN2; char varName[CDF_VAR_NAME_LEN256 + 1];
  Logical cmpVarValues; int i, j; char *v = BOO(Z,zVariable,rVariable);
  long VARcType1, VARcParms1[CDF_MAX_PARMS], VARcPct1;
  long VARcType2, VARcParms2[CDF_MAX_PARMS], VARcPct2;
  long VARsArraysType1, VARsArraysParms1[CDF_MAX_PARMS];
  long VARsArraysPct1, VARsRecordsType1;
  long VARsArraysType2, VARsArraysParms2[CDF_MAX_PARMS];
  long VARsArraysPct2, VARsRecordsType2;

  for (varN1 = 0; varN1 < BOO(Z,zNumVars1,rNumVars1); varN1++) {
     if (BOO(Z,zVarNumMatches1[(int)varN1],
	       rVarNumMatches1[(int)varN1]) == -1) {
       status = CDFlib (SELECT_, CDF_, id1,
				 VAR(Z), varN1,
			GET_, VAR_NAME(Z), varName,
			NULL_);
       if (!StatusHandlerCmp("CDF1",status)) return FALSE;
       diffFound = TRUE;
       snprintf (oText, (size_t) sizeof(oText),
		 "%s \"%s\" does not exist in CDF2", v, varName);
       OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 2);
     }
     else {
       status = CDFlib (SELECT_, CDF_, id1,
				 VAR(Z), varN1,
			GET_, VAR_NAME(Z), varName,
			      VAR_DATATYPE(Z), &varDataType1,
			      VAR_NUMELEMS(Z), &varNumElems1,
			      VAR_RECVARY(Z), &varRecVary1,
			      VAR_DIMVARYS(Z), varDimVarys1,
			      VAR_MAXREC(Z), &varMaxRec1,
			      VAR_COMPRESSION(Z), &VARcType1,
						  VARcParms1,
						  &VARcPct1,
			      VAR_SPARSEARRAYS(Z), &VARsArraysType1,
						   VARsArraysParms1,
						   &VARsArraysPct1,
			      VAR_SPARSERECORDS(Z), &VARsRecordsType1,
			      VAR_BLOCKINGFACTOR(Z), &varBlocking1,
			NULL_);
       if (!StatusHandlerCmp("CDF1",status)) return FALSE;

       if (Z) {
	 status = CDFlib (SELECT_, CDF_, id1,
			  GET_, zVAR_NUMDIMS_, &zNumDims1,
				zVAR_DIMSIZES_, zDimSizes1,
			  NULL_);
	 if (!StatusHandlerCmp("CDF1",status)) return FALSE;
       }

       status = CDFlib (SELECT_, CDF_, id2,
				 VAR(Z), BOO(Z,zVarNumMatches1[(int)varN1],
					       rVarNumMatches1[(int)varN1]),
			GET_, VAR_DATATYPE(Z), &varDataType2,
			      VAR_NUMELEMS(Z), &varNumElems2,
			      VAR_RECVARY(Z), &varRecVary2,
			      VAR_DIMVARYS(Z),varDimVarys2,
			      VAR_MAXREC(Z), &varMaxRec2,
			      VAR_COMPRESSION(Z), &VARcType2,
						  VARcParms2,
						  &VARcPct2,
			      VAR_SPARSEARRAYS(Z), &VARsArraysType2,
						   VARsArraysParms2,
						   &VARsArraysPct2,
			      VAR_SPARSERECORDS(Z), &VARsRecordsType2,
			      VAR_BLOCKINGFACTOR(Z), &varBlocking2,
			NULL_);
       if (!StatusHandlerCmp("CDF2",status)) return FALSE;

       if (Z) {
	 status = CDFlib (SELECT_, CDF_, id2,
			  GET_, zVAR_NUMDIMS_, &zNumDims2,
				zVAR_DIMSIZES_, zDimSizes2,
			  NULL_);
	 if (!StatusHandlerCmp("CDF2",status)) return FALSE;
       }

       if (mLog) {
	 snprintf (oText, (size_t) sizeof(oText),
		   "Comparing %s \"%s\"", v, varName);
	 OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 2);
       }

       cmpVarValues = TRUE;

       if (cmpNumbers)
	 if (BOO(Z,zVarNumMatches1[(int)varN1],
		   rVarNumMatches1[(int)varN1]) != varN1) {
	   diffFound = TRUE;
	   snprintf (oText, (size_t) sizeof(oText),
		     "DIFFERENT numbers for %s \"%s\" (%ld vs. %ld)",
		     v, varName, varN1+1, BOO(Z,zVarNumMatches1[(int)varN1],
					       rVarNumMatches1[(int)varN1])+1);
	   OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
	 }

       if (cmpEtc && (!SameCompressions(VARcType1,VARcParms1,
					VARcType2,VARcParms2))) {
	 diffFound = TRUE;
	 strcpyX (oText, "DIFFERENT compressions for", MAX_oTEXT_LEN);
	 snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
		   " %s \"%s\" (%s vs. ",
		   v, varName, CompressionToken(VARcType1,VARcParms1));
	 snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
		   "%s)", CompressionToken(VARcType2,VARcParms2));
	 OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
       }

       if (cmpEtc && (VARcPct1 != VARcPct2)) {
	 diffFound = TRUE;
	 strcpyX (oText, "DIFFERENT compression %s (compressed/uncompressed) for",
		  MAX_oTEXT_LEN);
	 snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
		   " %s \"%s\" (%ld%% vs. %ld%%)",
		   v, varName, VARcPct1, VARcPct2);
	 OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
       }

       if (cmpEtc && (!SameSparsenesses(VARsRecordsType1,VARsArraysType1,
					VARsArraysParms1,VARsRecordsType2,
					VARsArraysType2,VARsArraysParms2))) {
	 diffFound = TRUE;
	 strcpyX (oText, "DIFFERENT sparseness for", MAX_oTEXT_LEN);
	 snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
		   " %s \"%s\" (%s vs. ", v, varName,
		   SparsenessToken(VARsRecordsType1,
				   VARsArraysType1,
				   VARsArraysParms1));
	 snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
		   "%s)", SparsenessToken(VARsRecordsType2,
				          VARsArraysType2,
				          VARsArraysParms2));
	 OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
       }

       if (cmpEtc && (VARsArraysPct1 != VARsArraysPct2)) {
	 diffFound = TRUE;
	 strcpyX (oText, "DIFFERENT sparse array percentages for",
		  MAX_oTEXT_LEN);
	 snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
		   " %s \"%s\" (%ld vs. %ld)",
		   v, varName, VARsArraysPct1, VARsArraysPct2);
	 OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
       }

       if (cmpEtc && (varDataType1 != varDataType2)) {
	 diffFound = TRUE;
	 strcpyX (oText, "DIFFERENT data types for", MAX_oTEXT_LEN);
	 snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
		   " %s \"%s\" (CDF_%s vs. CDF_%s)",
		   v, varName, DataTypeToken(varDataType1),
		   DataTypeToken(varDataType2));
	 OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
       }

       if (!EquivalentDataTypes(varDataType1,varDataType2)) {
	 diffFound = TRUE;
	 strcpyX (oText, "Non-equivalent data types for", MAX_oTEXT_LEN);
	 snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
		   " %s \"%s\" (CDF_%s vs. CDF_%s)",
		   v, varName, DataTypeToken(varDataType1),
		   DataTypeToken(varDataType2));
	 OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
	 cmpVarValues = FALSE;
       }

       if (varNumElems1 != varNumElems2) {
	 diffFound = TRUE;
	 strcpyX (oText, "DIFFERENT number of elements for", MAX_oTEXT_LEN);
	 snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
		   " %s \"%s\" (%ld vs. %ld)",
		   v, varName, varNumElems1, varNumElems2);
	 OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
	 cmpVarValues = FALSE;
       }

       if (Z) {
	 if (zNumDims1 != zNumDims2) {
	   diffFound = TRUE;
	   snprintf (oText, (size_t) sizeof(oText),
		     "DIFFERENT number of dimensions (%ld vs. %ld)",
		     zNumDims1, zNumDims2);
	   snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
		     " for zVariable \"%s\"", varName);
	   OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
	   cmpVarValues = FALSE;
	 }
	 else {
	   if (zNumDims1 > 0) {
	     for (i = 0; i < zNumDims1; i++) {
		if (zDimSizes1[i] != zDimSizes2[i]) {
		  diffFound = TRUE;
		  strcpyX (oText,"DIFFERENT dimension sizes (",MAX_oTEXT_LEN);
		  for (j = 0; j < zNumDims1; j++) {
		     snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
			       "%ld ", zDimSizes1[j]);
		  }
		  strcatX (oText, "vs.", MAX_oTEXT_LEN);
		  for (j = 0; j < zNumDims2; j++) {
		     snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
			       " %ld", zDimSizes2[j]);
		  }
		  snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
			    ") for zVariable \"%s\"", varName);
		  OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
		  cmpVarValues = FALSE;
		  break;
		}
	     }
	   }
	 }
       }

       if (varRecVary1 != varRecVary2) {
	 diffFound = TRUE;
	 strcpyX (oText, "DIFFERENT record variances for", MAX_oTEXT_LEN);
	 snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
		   " %s \"%s\" (%s vs. %s)",
		   v, varName, TFvarianceToken(varRecVary1),
		   TFvarianceToken(varRecVary2));
	 OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
       }

       if (Z) {
	 if (zNumDims1 == zNumDims2)
	   for (i = 0; i < zNumDims1; i++)
	     if (varDimVarys1[i] != varDimVarys2[i]) {
	       diffFound = TRUE;
	       strcpyX (oText,"DIFFERENT dimension variances for zVariable \"",
			MAX_oTEXT_LEN);
	       strcatX (oText, varName, MAX_oTEXT_LEN);
	       strcatX (oText, "\" (", MAX_oTEXT_LEN);
	       for (j = 0; j < zNumDims1; j++)
		  snprintf (EofS(oText),(size_t) sizeof(oText)-strlen(oText),
			    "%s",TFvarianceToken(varDimVarys1[j]));
	       strcatX (oText, " vs. ", MAX_oTEXT_LEN);
	       for (j = 0; j < zNumDims2; j++)
		  snprintf (EofS(oText),(size_t) sizeof(oText)-strlen(oText),
			    "%s",TFvarianceToken(varDimVarys2[j]));
	       strcatX (oText, ")", MAX_oTEXT_LEN);
	       OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
	       break;
	     }
       }
       else {
	 for (i = 0; i < rNumDims1; i++)
	   if (varDimVarys1[i] != varDimVarys2[i]) {
	     diffFound = TRUE;
	     strcpyX (oText, "DIFFERENT dimension variances for rVariable \"",
		      MAX_oTEXT_LEN);
	     strcatX (oText, varName, MAX_oTEXT_LEN);
	     strcatX (oText, "\" (", MAX_oTEXT_LEN);
	     for (j = 0; j < rNumDims1; j++) {
		snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
			  "%s", TFvarianceToken(varDimVarys1[j]));
	     }
	     strcatX (oText, " vs. ", MAX_oTEXT_LEN);
	     for (j = 0; j < rNumDims2; j++) {
		snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
			  "%s", TFvarianceToken(varDimVarys2[j]));
	     }
	     strcatX (oText, ")", MAX_oTEXT_LEN);
	     OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
	     break;
	   }
       }

       if (varMaxRec1 != varMaxRec2) {
	 diffFound = TRUE;
	 snprintf (oText,(size_t) sizeof(oText),
		   "DIFFERENT maximum record for %s \"%s\" (%ld vs. %ld)",
		   v, varName, varMaxRec1 + 1, varMaxRec2 + 1);
	 OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
       }

       if (cmpEtc && (varBlocking1 != varBlocking2)) {
	 diffFound = TRUE;
	 snprintf(oText,(size_t) sizeof(oText),
		  "DIFFERENT blocking factor for %s \"%s\" (%ld vs. %ld)",
		  v, varName, varBlocking1, varBlocking2);
	 OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
       }

       if (cmpVarValues) {
	 if (!CompareVariableValues(varN1,
				    BOO(Z,zVarNumMatches1[(int)varN1],
					  rVarNumMatches1[(int)varN1]),
				    varName,Z)) return FALSE;
       }
     }
     CHECKforABORTso
  }

  for (varN2 = 0; varN2 < BOO(Z,zNumVars2,rNumVars2); varN2++)
     if (BOO(Z,zVarNumMatches2[(int)varN2],
	       rVarNumMatches2[(int)varN2]) == -1) {
       status = CDFlib (SELECT_, CDF_, id2,
				 VAR(Z), varN2,
			GET_, VAR_NAME(Z), varName,
			NULL_);
       if (!StatusHandlerCmp("CDF2",status)) return FALSE;
       diffFound = TRUE;
       snprintf (oText, (size_t) sizeof(oText),
		 "%s \"%s\" does not exist in CDF1", v, varName);
       OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 2);
     }

  return TRUE;
}

/******************************************************************************
* CompareVariableValues.
******************************************************************************/

Logical CompareVariableValues (varN1, varN2, varName, Z)
long varN1;
long varN2;
char *varName;
Logical Z;
{
  long numDims, dimSizes[CDF_MAX_DIMS], recVary, dimVarys[CDF_MAX_DIMS];
  long nHypers, hyperN, nValuesPerRec, nValuesPerDim[CDF_MAX_DIMS];
  char *v = BOO(Z,zVariable,rVariable);
  void *padValue1, *padValue2;
  CDFstatus status1, status2;
  long maxRec; size_t nValueBytes[2];
  Logical rowMajor1 = ROWmajor(majority1);
  long recNumber, dimIndices[CDF_MAX_DIMS], valueN, NvalueBytes;
  long valueDiffCount, recordDiffCount, lastRecordDiff, nValues;
  struct GroupStruct groups; struct HyperStruct hyper;
  Byte1 *value1, *value2, *buffer1, *buffer2, **handles[2];
  int dimN, dimNt;
  char *format1 = NULL, *format2 = NULL;
  long dataType, numElems;
  Logical comp;
  static char hyperMsg[] = {
     "      Comparing values with hyper reads..."
  };
  static char hyperSwitchMsg[] = {
     "      Comparing values with hyper reads/switching..."
  };
  static char hyperSingleMsg[] = {
     "      Comparing values with hyper/single reads..."
  };
  /****************************************************************************
  * Select variables.
  ****************************************************************************/
  status1 = CDFlib (SELECT_, CDF_, id1,
			     VAR(Z), varN1,
		    NULL_);
  if (!StatusHandlerCmp("CDF1",status1)) return FALSE;
  status1 = CDFlib (SELECT_, ATTR_NAME_, "FORMAT",
                             BOO(Z,zENTRY_,rENTRY_), varN1,
                    GET_, BOO(Z,zENTRY_DATATYPE_,rENTRY_DATATYPE_), &dataType,
                          BOO(Z,zENTRY_NUMELEMS_,rENTRY_NUMELEMS_), &numElems,
                    NULL_);
  if (status1 == CDF_OK) {
    if (STRINGdataType(dataType)) {
      size_t iix; 
      format1 = (char *) cdf_AllocateMemory ((size_t) numElems + 1,
                                            FatalError);
      status1 = CDFlib (GET_, BOO(Z,zENTRY_DATA_,rENTRY_DATA_), format1,
                        NULL_);
      if (!StatusHandlerCmp("CDF1",status1)) return FALSE;
      iix = strlen((char *)format1);
                if (iix < (size_t)numElems)
                  ((char *)format1)[(int)iix] = '\0';
                else
                  ((char *)format1)[(int)numElems] = '\0';
/*
      if (iix < (size_t)numElems) {
        int iiy;
        for (iiy = iix; iiy < (int) numElems; ++iiy)
          *(format1+iiy) = ' ';
      }
      *(format1+(int)numElems) = '\0';
*/
    }
  }
  status2 = CDFlib (SELECT_, CDF_, id2,
			     VAR(Z), varN2,
		    NULL_);
  if (!StatusHandlerCmp("CDF2",status2)) return FALSE;
  status2 = CDFlib (SELECT_, ATTR_NAME_, "FORMAT",
                             BOO(Z,zENTRY_,rENTRY_), varN2,
                    GET_, BOO(Z,zENTRY_DATATYPE_,rENTRY_DATATYPE_), &dataType,
                          BOO(Z,zENTRY_NUMELEMS_,rENTRY_NUMELEMS_), &numElems,
                    NULL_);
  if (status2 == CDF_OK) {
    if (STRINGdataType(dataType)) {
      size_t iix;
      format2 = (char *) cdf_AllocateMemory ((size_t) numElems + 1,
                                            FatalError);
      status2 = CDFlib (GET_, BOO(Z,zENTRY_DATA_,rENTRY_DATA_), format2,
                        NULL_);
      if (!StatusHandlerCmp("CDF1",status2)) return FALSE;
      iix = strlen((char *)format2);
                if (iix < (size_t)numElems)
                  ((char *)format2)[(int)iix] = '\0';
                else
                  ((char *)format2)[(int)numElems] = '\0';
/*
      if (iix < (size_t)numElems) {
        int iiy;
        for (iiy = iix; iiy < (int) numElems; ++iiy)
          *(format2+iiy) = ' ';
      }
      *(format2+(int)numElems) = '\0';
*/
    }
  }
  /****************************************************************************
  * Compare pad values.
  ****************************************************************************/
  NvalueBytes = varNumElems1 * CDFelemSize(varDataType1);
  padValue1 = cdf_AllocateMemory ((size_t) NvalueBytes, FatalError);
  padValue2 = cdf_AllocateMemory ((size_t) NvalueBytes, FatalError);
  status1 = CDFlib (SELECT_, CDF_, id1,
		    GET_, VAR_PADVALUE(Z), padValue1,
		    NULL_);
  status2 = CDFlib (SELECT_, CDF_, id2,
		    GET_, VAR_PADVALUE(Z), padValue2,
		    NULL_);
  if (status1 != NO_PADVALUE_SPECIFIED) {
    if (!StatusHandlerCmp("CDF1",status1)) return FALSE;
    if (status2 != NO_PADVALUE_SPECIFIED) {
      if (!StatusHandlerCmp("CDF2",status2)) return FALSE;
      if (memcmp(padValue1,padValue2,(size_t)NvalueBytes) != 0) {
	comp = FALSE;
	if ((varDataType1 == CDF_INT1) || (varDataType1 == CDF_INT2) ||
	    (varDataType1 == CDF_INT4) || (varDataType1 == CDF_UINT1) ||
	    (varDataType1 == CDF_UINT2) || (varDataType1 == CDF_UINT4) ||
	    (varDataType1 == CDF_INT8) || (varDataType1 == CDF_TIME_TT2000)) {
	  comp = TRUE;	
	} else if (STRINGdataType(varDataType1)) {
	  comp = CompareStrings((char *)padValue1, (char *)padValue2,
				varNumElems1, NvalueBytes);
	} else {
	  if ((!floatSet && ((varDataType1 == CDF_FLOAT) ||
			     (varDataType1 == CDF_REAL4))
			 && (CheckNaN(padValue1, padValue2, varDataType1) > 1)) ||
	      (!doubleSet && ((varDataType1 == CDF_REAL8) ||
			      (varDataType1 == CDF_DOUBLE))
			  && (CheckNaN(padValue1, padValue2, varDataType1) > 1)) ||
	      (!doubleSet && ((varDataType1 == CDF_EPOCH) ||
			(varDataType1 == CDF_EPOCH16))) ||
	      (floatSet && ((varDataType1 == CDF_FLOAT) ||
			      (varDataType1 == CDF_REAL4))) ||
	      (doubleSet && ((varDataType1 == CDF_REAL8) ||
			     (varDataType1 == CDF_DOUBLE) ||
			     (varDataType1 == CDF_EPOCH) ||
			     (varDataType1 == CDF_EPOCH16)))) {
	    comp = ValuesCmp(padValue1, padValue2, varDataType1, 
			     (double) floatTolerance, doubleTolerance);
	  }
	}
	if (comp) {
	  diffFound = TRUE;
	  snprintf (oText, (size_t) sizeof(oText),
		  "DIFFERENT pad values for %s \"%s\"", v, varName);
	  OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
	  if (displayValue) {
            int style;
	    WriteOut (stdout, "      CDF1 value> ");
            if (TT2000dataType(varDataType1)) style = TT2000_3_STYLE;
            else style = EPOCH0_STYLE;
	    EncodeValuesFormat (varDataType1, varNumElems1, padValue1, oText,
	    		        NULL, 0, MAX_SCREENLINE_LEN - 19, style,
				(size_t) sizeof(oText));
	    WriteOut (stdout, oText);
	    WriteOut (stdout, "\n");
	    WriteOut (stdout, "      CDF2 value> ");
            if (TT2000dataType(varDataType2)) style = TT2000_3_STYLE;
            else style = EPOCH0_STYLE;
	    EncodeValuesFormat (varDataType2, varNumElems2, padValue2, oText,
				NULL, 0, MAX_SCREENLINE_LEN - 19, style,
			  	(size_t) sizeof(oText));
	    WriteOut (stdout, oText);
	    WriteOut (stdout, "\n");
          }
        }
      }
    }
    else {
      diffFound = TRUE;
      snprintf (oText, (size_t) sizeof(oText),
	        "No pad value for %s \"%s\" in CDF2", v, varName);
      OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
    }
  }
  else {
    if (status2 != NO_PADVALUE_SPECIFIED) {
      if (!StatusHandlerCmp("CDF2",status2)) return FALSE;
      diffFound = TRUE;
      snprintf (oText, (size_t) sizeof(oText),
	        "No pad value for %s \"%s\" in CDF1", v, varName);
      OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
    }
  }
  cdf_FreeMemory (padValue1, FatalError);
  cdf_FreeMemory (padValue2, FatalError);
  CHECKforABORTso
  /****************************************************************************
  * Hyper groups...
  ****************************************************************************/
  maxRec = MAXIMUM(varMaxRec1,varMaxRec2);
  recVary = (varRecVary1 || varRecVary2);
  numDims = BOO(Z,zNumDims1,rNumDims1);
  for (dimN = 0; dimN < numDims; dimN++) {
     dimVarys[dimN] = (varDimVarys1[dimN] || varDimVarys2[dimN]);
     if (dimVarys[dimN])
       dimSizes[dimN] = BOO(Z,zDimSizes1[dimN],rDimSizes1[dimN]);
     else
       dimSizes[dimN] = 1;
  }
  for (dimN = 0, nValuesPerRec = 1; dimN < numDims; dimN++) {
     if (dimVarys[dimN]) nValuesPerRec *= dimSizes[dimN];
  }
  for (dimN = 0; dimN < numDims; dimN++) {
     nValuesPerDim[dimN] = 1;
     if (rowMajor1) {
       for (dimNt = dimN + 1; dimNt < numDims; dimNt++) {
	  nValuesPerDim[dimN] *= dimSizes[dimNt];
       }
     }
     else {
       for (dimNt = dimN - 1; dimNt >= 0; dimNt--) {
	  nValuesPerDim[dimN] *= dimSizes[dimNt];
       }
     }
  }
  handles[0] = &buffer1;
  handles[1] = &buffer2;
  nValueBytes[0] = (size_t) NvalueBytes;
  nValueBytes[1] = (size_t) NvalueBytes;
  valueDiffCount = 0;
  recordDiffCount = 0;
  lastRecordDiff = -1;
  /****************************************************************************
  * Check for zero or one dimensions or the same majorities (if two or more
  * dimensions).
  ****************************************************************************/
  if (numDims < 2 || majority1 == majority2) {
    if (mLog) {
      WriteOut (stdout, hyperMsg);
      WriteOut (stdout, BOO(pctLog,"..0%","\n"));
    }
    AllocateBuffers (maxRec + 1, numDims, dimSizes, &groups, 0, 2, handles,
		     nValueBytes, rowMajor1, MINnHYPERS, FatalError);
    InitHyperParms (&hyper, &groups, numDims, &nHypers, &nValues);
    for (hyperN = 0; hyperN < nHypers; hyperN++) {
       status1 = HYPER_READ (id1, Z, hyper, buffer1);
       if (!StatusHandlerCmp("CDF1",status1)) return FALSE;
       if (pctLog) WriteOutPct (PCT(hyperN,nHypers,1,3));
       CHECKforABORTso
       status2 = HYPER_READ (id2, Z, hyper, buffer2);
       if (!StatusHandlerCmp("CDF2",status2)) return FALSE;
       if (pctLog) WriteOutPct (PCT(hyperN,nHypers,2,3));
       if (status1 != status2) {
         if (status1 == VIRTUAL_RECORD_DATA) {
           snprintf (oText, (size_t) sizeof(oText),
                     "Virtual vs Real in record range: %ld to %ld for \"%s\". ",
                     hyper.recNumber,hyper.recNumber+hyper.recCount-1,varName);
           OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
         }
         if (status2 == VIRTUAL_RECORD_DATA) {
           snprintf (oText, (size_t) sizeof(oText),
                     "Real vs Virtual in record reane: %ld to %ld for \"%s\". ",
                     hyper.recNumber,hyper.recNumber+hyper.recCount-1,varName);
           OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
         }
       }
       CHECKforABORTso
       if (memcmp(buffer1,buffer2,(size_t)(nValues*NvalueBytes))) {
	 for (valueN = 0, value1 = buffer1, value2 = buffer2;
	      valueN < nValues; valueN++,
	      value1 += (size_t)NvalueBytes, value2 += (size_t)NvalueBytes) {
	   if (memcmp(value1,value2,(size_t)NvalueBytes)) {
	     comp = FALSE;
	     if ((varDataType1 == CDF_INT1) || (varDataType1 == CDF_INT2) ||
		 (varDataType1 == CDF_INT4) || (varDataType1 == CDF_UINT1) ||
		 (varDataType1 == CDF_UINT2) || (varDataType1 == CDF_UINT4) ||
		 (varDataType1 == CDF_INT8) || (varDataType1 == CDF_TIME_TT2000)) {
	       comp = TRUE;
	     } else if (STRINGdataType(varDataType1)) {
	       comp = CompareStrings((char *)value1, (char *)value2,
				     varNumElems1, NvalueBytes);
             } else {
	       if ((!floatSet && ((varDataType1 == CDF_FLOAT) ||
				  (varDataType1 == CDF_REAL4))
			      && (CheckNaN(value1, value2, varDataType1) > 1)) ||
	           (!doubleSet && ((varDataType1 == CDF_REAL8) ||
				   (varDataType1 == CDF_DOUBLE))
			       && (CheckNaN(value1, value2, varDataType1) > 1)) ||
	           (!doubleSet && ((varDataType1 == CDF_EPOCH) ||
				   (varDataType1 == CDF_EPOCH16))) ||
	           (floatSet && ((varDataType1 == CDF_FLOAT) ||
				 (varDataType1 == CDF_REAL4))) ||
	           (doubleSet && ((varDataType1 == CDF_REAL8) ||
				  (varDataType1 == CDF_DOUBLE) ||
				  (varDataType1 == CDF_EPOCH) ||
				  (varDataType1 == CDF_EPOCH16)))) {
		 comp = ValuesCmp(value1, value2, varDataType1, 
				  (double) floatTolerance, doubleTolerance);
	       }
	     }
	     if (comp) {
	       AtRecordIndices (&hyper, valueN, rowMajor1, numDims,
			        nValuesPerRec, nValuesPerDim, &recNumber,
			        dimIndices);
	       valueDiffCount++;
	       if (recNumber != lastRecordDiff) {
		 recordDiffCount++;
		 lastRecordDiff = recNumber;
	       }
	       ReportValueDifference (recNumber, recVary, numDims, dimIndices,
				      dimVarys, v, varName, value1, value2,
				      hyperMsg, PCT(hyperN,nHypers,2,3),
				      (format==TRUE?format1:NULL),
				      (format==TRUE?format2:NULL));
	     }
	   }
	 }
       }
       if (pctLog) WriteOutPct (PCT(hyperN,nHypers,3,3));
       IncrHyperParms (&hyper, &groups, numDims, rowMajor1, &nValues);
       CHECKforABORTso
    }
    cdf_FreeMemory (buffer1, FatalError);
    cdf_FreeMemory (buffer2, FatalError);
    if (pctLog) WriteOut (stdout, "\n");
    ReportValueDifferenceTotals (recordDiffCount, valueDiffCount, varName, v);
    return TRUE;
  }
  /****************************************************************************
  * There must be two or more dimensions with different majorities.  Attempt
  * to allocate two buffers.
  ****************************************************************************/
  AllocateBuffers (maxRec + 1, numDims, dimSizes, &groups, 0, 2, handles,
		   nValueBytes, rowMajor1, MINnHYPERS, FatalError);
  /****************************************************************************
  * Check if full records are being read.  If so, switch the majority in one
  * of the buffers before the comparison.
  ****************************************************************************/
  if (HyperFullRecord(&groups,numDims)) {
    long nBytesPerRecord = nValuesPerRec * NvalueBytes, recX;
    if (mLog) {
      WriteOut (stdout, hyperSwitchMsg);
      WriteOut (stdout, BOO(pctLog,"..0%","\n"));
    }
    InitHyperParms (&hyper, &groups, numDims, &nHypers, &nValues);
    for (hyperN = 0; hyperN < nHypers; hyperN++) {
       status2 = HYPER_READ (id2, Z, hyper, buffer1);
       if (!StatusHandlerCmp("CDF2",status2)) return FALSE;
       if (pctLog) WriteOutPct (PCT(hyperN,nHypers,1,4));
       CHECKforABORTso
       for (recX = 0; recX < hyper.recCount; recX++) {
	  size_t offset = (size_t) (recX * nBytesPerRecord);
	  if (rowMajor1)
	    COLtoROW (buffer1 + offset, buffer2 + offset, numDims,
		      dimSizes, NvalueBytes);
	  else
	    ROWtoCOL (buffer1 + offset, buffer2 + offset, numDims,
		      dimSizes, NvalueBytes);
       }
       if (pctLog) WriteOutPct (PCT(hyperN,nHypers,2,4));
       CHECKforABORTso
       status1 = HYPER_READ (id1, Z, hyper, buffer1);
       if (!StatusHandlerCmp("CDF1",status1)) return FALSE;
       if (pctLog) WriteOutPct (PCT(hyperN,nHypers,3,4));
       CHECKforABORTso
       if (status1 != status2) {
         if (status1 == VIRTUAL_RECORD_DATA) {
           snprintf (oText, (size_t) sizeof(oText),
                     "Virtual vs Real at record(s): %ld to %ld for \"%s\". ",
                     hyper.recNumber,(hyper.recNumber+hyper.recCount-1),varName);
           OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
         }
         if (status2 == VIRTUAL_RECORD_DATA) {
           snprintf (oText, (size_t) sizeof(oText),
                     "Real vs Virtual at record(s): %ld to %ld for \"%s\". ",
                     hyper.recNumber,(hyper.recNumber+hyper.recCount-1),varName);
           OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
         }
       }
       if (memcmp(buffer1,buffer2,(size_t)(nValues*NvalueBytes))) {
	 for (valueN = 0, value1 = buffer1, value2 = buffer2;
	      valueN < nValues; valueN++,
	      value1 += (size_t)NvalueBytes, value2 += (size_t)NvalueBytes) {
	   if (memcmp(value1,value2,(size_t)NvalueBytes)) {
	     comp = FALSE;
	     if ((varDataType1 == CDF_INT1) || (varDataType1 == CDF_INT2) ||
		 (varDataType1 == CDF_INT4) || (varDataType1 == CDF_UINT1) ||
		 (varDataType1 == CDF_UINT2) || (varDataType1 == CDF_UINT4) ||
		 (varDataType1 == CDF_INT8) || (varDataType1 == CDF_TIME_TT2000)) {
	       comp = TRUE;	
             } else if (STRINGdataType(varDataType1)) {
	       comp = CompareStrings((char *)value1, (char *)value2,
				     varNumElems1, NvalueBytes);
	     } else {
	       if ((!floatSet && ((varDataType1 == CDF_FLOAT) ||
				  (varDataType1 == CDF_REAL4))
			      && (CheckNaN(value1, value2, varDataType1) > 1)) ||
	           (!doubleSet && ((varDataType1 == CDF_REAL8) ||
				   (varDataType1 == CDF_DOUBLE))
			       && (CheckNaN(value1, value2, varDataType1) > 1)) ||
	           (!doubleSet && ((varDataType1 == CDF_EPOCH) ||
				   (varDataType1 == CDF_EPOCH16))) ||
	           (floatSet && ((varDataType1 == CDF_FLOAT) ||
				 (varDataType1 == CDF_REAL4))) ||
	           (doubleSet && ((varDataType1 == CDF_REAL8) ||
				  (varDataType1 == CDF_DOUBLE) ||
				  (varDataType1 == CDF_EPOCH) ||
				  (varDataType1 == CDF_EPOCH16)))) {
		 comp = ValuesCmp(value1, value2, varDataType1, 
				  (double) floatTolerance, doubleTolerance);
	       }
	     }
	     if (comp) {
	       AtRecordIndices (&hyper, valueN, rowMajor1, numDims,
	    		        nValuesPerRec, nValuesPerDim, &recNumber,
			        dimIndices);
	       valueDiffCount++;
	       if (recNumber != lastRecordDiff) {
    	    	 recordDiffCount++;
		 lastRecordDiff = recNumber;
	       }
	       ReportValueDifference (recNumber, recVary, numDims, dimIndices,
				      dimVarys, v, varName, value1, value2,
				      hyperSwitchMsg, PCT(hyperN,nHypers,3,4),
				      (format==TRUE?format1:NULL),
				      (format==TRUE?format2:NULL));
	     }
	   }
	 }
       }
       if (pctLog) WriteOutPct (PCT(hyperN,nHypers,4,4));
       IncrHyperParms (&hyper, &groups, numDims, rowMajor1, &nValues);
       CHECKforABORTso
    }
    cdf_FreeMemory (buffer1, FatalError);
    cdf_FreeMemory (buffer2, FatalError);
    if (pctLog) WriteOut (stdout, "\n");
    ReportValueDifferenceTotals (recordDiffCount, valueDiffCount, varName, v);
    return TRUE;
  }
  /****************************************************************************
  * Less than full records are being read/written.  Hyper read into the
  * buffer but then use single writes to reverse the majority.
  ****************************************************************************/
  cdf_FreeMemory (buffer2, FatalError);
  value2 = cdf_AllocateMemory ((size_t) NvalueBytes, FatalError);
  if (mLog) {
    WriteOut (stdout, hyperSingleMsg);
    WriteOut (stdout, BOO(pctLog,"..0%","\n"));
  }
  InitHyperParms (&hyper, &groups, numDims, &nHypers, &nValues);
  for (hyperN = 0; hyperN < nHypers; hyperN++) {
     status1 = HYPER_READ (id1, Z, hyper, buffer1);
     if (!StatusHandlerCmp("CDF1",status1)) return FALSE;
     if (pctLog) WriteOutPct (PCT(hyperN,nHypers,1,2));
     CHECKforABORTso
     status2 = CDFlib (SELECT_, CDF_, id2,
				BOO(Z,zVAR_RECNUMBER_,
				      rVARs_RECNUMBER_), hyper.recNumber,
		       NULL_);
     if (!StatusHandlerCmp("CDF2",status2)) return FALSE;
     for (dimN = 0; dimN < numDims; dimN++) {
	dimIndices[dimN] = hyper.dimIndices[dimN];
     }
     for (valueN = 0, value1 = buffer1; valueN < nValues;
	  valueN++, value1 += (size_t) NvalueBytes) {
	status2 = CDFlib (SELECT_, BOO(Z,zVAR_DIMINDICES_,
					 rVARs_DIMINDICES_), dimIndices,
			  GET_, VAR_DATA(Z), value2,
			  NULL_);
	if (!StatusHandlerCmp("CDF2",status2)) return FALSE;
	if (memcmp(value1,value2,(size_t)NvalueBytes)) {
	  comp = FALSE;
	  if ((varDataType1 == CDF_INT1) || (varDataType1 == CDF_INT2) ||
	      (varDataType1 == CDF_INT4) || (varDataType1 == CDF_UINT1) ||
	      (varDataType1 == CDF_UINT2) || (varDataType1 == CDF_UINT4) ||
	      (varDataType1 == CDF_INT8) || (varDataType1 == CDF_TIME_TT2000)) {
	    comp = TRUE;	
	  } else if (STRINGdataType(varDataType1)) {
	    comp = CompareStrings((char *)value1, (char *)value2,
				  varNumElems1, NvalueBytes);
	  } else {
	    if ((!floatSet && ((varDataType1 == CDF_FLOAT) ||
			       (varDataType1 == CDF_REAL4))
			   && (CheckNaN(value1, value2, varDataType1) > 1)) ||
	        (!doubleSet && ((varDataType1 == CDF_REAL8) ||
				(varDataType1 == CDF_DOUBLE))
			    && (CheckNaN(value1, value2, varDataType1) > 1)) ||
	        (!doubleSet && ((varDataType1 == CDF_EPOCH) ||
				(varDataType1 == CDF_EPOCH16))) ||
	        (floatSet && ((varDataType1 == CDF_FLOAT) ||
			      (varDataType1 == CDF_REAL4))) ||
	        (doubleSet && ((varDataType1 == CDF_REAL8) ||
			       (varDataType1 == CDF_DOUBLE) ||
			       (varDataType1 == CDF_EPOCH) ||
			       (varDataType1 == CDF_EPOCH16)))) {
	      comp = ValuesCmp(value1, value2, varDataType1, 
			       (double) floatTolerance, doubleTolerance);
	    }
	  }
	  if (comp) {
	    valueDiffCount++;
	    if (hyper.recNumber != lastRecordDiff) {
	      recordDiffCount++;
	      lastRecordDiff = hyper.recNumber;
	    }
	    ReportValueDifference (hyper.recNumber, recVary, numDims,
	  			   dimIndices, dimVarys, v, varName,
				   value1, value2, hyperSingleMsg,
				   PCT(hyperN,nHypers,1,2),
				   (format==TRUE?format1:NULL),
				   (format==TRUE?format2:NULL));
	  }
	}
	if (rowMajor1)
	  INCRindicesROW (numDims, dimSizes, dimIndices);
	else
	  INCRindicesCOL (numDims, dimSizes, dimIndices);
     }
     if (pctLog) WriteOutPct (PCT(hyperN,nHypers,2,2));
     IncrHyperParms (&hyper, &groups, numDims, rowMajor1, &nValues);
     CHECKforABORTso
  }
  cdf_FreeMemory (buffer1, FatalError);
  cdf_FreeMemory (value2, FatalError);
  if (format1 != NULL) cdf_FreeMemory (format1, FatalError);
  if (format2 != NULL) cdf_FreeMemory (format2, FatalError);
  if (pctLog) WriteOut (stdout, "\n");
  ReportValueDifferenceTotals (recordDiffCount, valueDiffCount, varName, v);
  return TRUE;
}

/******************************************************************************
* SameVarys.
******************************************************************************/

Logical SameVarys (numDims, recVary1, recVary2, dimVarys1, dimVarys2)
long numDims;
long recVary1;
long recVary2;
long dimVarys1[];
long dimVarys2[];
{
  int dimNum;
  if (recVary1 != recVary2) return FALSE;
  for (dimNum = 0; dimNum < numDims; dimNum++) {
     if (dimVarys1[dimNum] != dimVarys2[dimNum]) return FALSE;
  }
  return TRUE;
}


/******************************************************************************
* EquivalentDataTypes.
******************************************************************************/

Logical EquivalentDataTypes (dataType1, dataType2)
long dataType1;
long dataType2;
{
  static long realDataType[] = {
    0,1,2,0,3,0,0,0,4,0,
    0,5,6,0,7,0,0,0,0,0,
    0,8,9,0,0,0,0,0,0,0,
    0,9,0,4,0,0,0,0,0,0,
    0,1,0,0,8,9,0,0,0,0,
    0,10,10,0,0,0,0,0,0,0
  };
  return (realDataType[(int)dataType1] == realDataType[(int)dataType2]);
}

/******************************************************************************
* CalcIndicesFromOffset.
******************************************************************************/

void CalcIndicesFromOffset (vOffset, rowMajor, numDims, dimSizes, dimVarys,
			    dimIndices)
long vOffset;           /* Value offset into array (not byte offset). */
Logical rowMajor;
long numDims;
long dimSizes[];
long dimVarys[];
long dimIndices[];
{
  long products[CDF_MAX_DIMS];
  int dimN, dimNt;
  if (rowMajor) {
    for (dimN = 0; dimN < numDims; dimN++) {
       products[dimN] = 1;
       for (dimNt = dimN + 1; dimNt < numDims; dimNt++)
	  if (dimVarys[dimNt]) products[dimN] *= dimSizes[dimNt];
    }
    for (dimN = 0; dimN < numDims; dimN++)
       if (dimVarys[dimN]) {
	 dimIndices[dimN] = vOffset / products[dimN];
	 vOffset %= products[dimN];
       }
       else
	 dimIndices[dimN] = 0;
  }
  else {
    for (dimN = (int) (numDims - 1); dimN >= 0; dimN--) {
       products[dimN] = 1;
       for (dimNt = dimN - 1; dimNt >= 0; dimNt--)
	  if (dimVarys[dimNt]) products[dimN] *= dimSizes[dimNt];
    }
    for (dimN = (int) (numDims - 1); dimN >= 0; dimN--)
       if (dimVarys[dimN]) {
	 dimIndices[dimN] = vOffset / products[dimN];
	 vOffset %= products[dimN];
       }
       else
	 dimIndices[dimN] = 0;
  }
  return;
}

/******************************************************************************
* StatusHandlerCmp.
******************************************************************************/

Logical StatusHandlerCmp (which, status)
char *which;
CDFstatus status;
{
  char text[CDF_STATUSTEXT_LEN+1];            /* Explanation text. */

  if (StatusERROR(status)) {
    if (report[ERRORs]) {
      if (pctLog) WriteOut (stdout, "\n");
      CDFlib (SELECT_, CDF_STATUS_, status,
	      GET_, STATUS_TEXT_, text,
	      NULL_);
      snprintf (oText, (size_t) sizeof(oText),
		"ERROR,%s> %s", which, text);
      OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 0);
    }
    return FALSE;
  }

  if (StatusWARN(status)) {
    if (report[WARNs]) {
      if (pctLog) WriteOut (stdout, "\n");
      CDFlib (SELECT_, CDF_STATUS_, status,
	      GET_, STATUS_TEXT_, text,
	      NULL_);
      snprintf (oText, (size_t) sizeof(oText),
		"WARNING,%s> %s", which, text);
      OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 0);
    }
    return TRUE;
  }

  if (StatusINFO(status)) {
    if (report[INFOs]) {
      if (pctLog) WriteOut (stdout, "\n");
      CDFlib (SELECT_, CDF_STATUS_, status,
	      GET_, STATUS_TEXT_, text,
	      NULL_);
      snprintf (oText, (size_t) sizeof(oText),
		"INFO,%s> %s", which, text);
      OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 0);
    }
    return TRUE;
  }

  return TRUE;          /* CDF_OK */
}

/******************************************************************************
* AtRecordIndices.
******************************************************************************/

void AtRecordIndices (hyper, valueN, rowMajor, numDims, nValuesPerRec,
		      nValuesPerDim, recNumber, dimIndices)
struct HyperStruct *hyper;
long valueN;
Logical rowMajor;
long numDims;
long nValuesPerRec;
long nValuesPerDim[];
long *recNumber;
long dimIndices[];
{
  int dimN;
  *recNumber = (valueN / nValuesPerRec) + hyper->recNumber;
  valueN %= nValuesPerRec;
  if (rowMajor) {
    for (dimN = 0; dimN < numDims; dimN++) {
       dimIndices[dimN] = hyper->dimIndices[dimN];
       dimIndices[dimN] += (valueN / nValuesPerDim[dimN]);
       valueN %= nValuesPerDim[dimN];
    }
  }
  else {
    for (dimN = (int) (numDims - 1); dimN >= 0; dimN--) {
       dimIndices[dimN] = hyper->dimIndices[dimN];
       dimIndices[dimN] += (valueN / nValuesPerDim[dimN]);
       valueN %= nValuesPerDim[dimN];
    }
  }
  return;
}

/******************************************************************************
* ReportValueDifference.
******************************************************************************/

void ReportValueDifference (recNumber, recVary, numDims, dimIndices, dimVarys,
			    v, varName, value1, value2, typeMsg, lastPct,
			    format1, format2)
long recNumber;
long recVary;
long numDims;
long dimIndices[];
long dimVarys[];
char *v;
char *varName;
Byte1 *value1;
Byte1 *value2;
char *typeMsg;
int lastPct;
char *format1;
char *format2;
{
  if (locations) {
    char where[MAX_RECORD_INDICES_LEN+1];
    int style;
    EncodeRecordIndices (where, recNumber, recVary, numDims, dimIndices,
			 dimVarys, (size_t) sizeof(where));
    snprintf (oText, (size_t) sizeof(oText),
	      "DIFFERENT value for %s \"%s\" at %s", v, varName, where);
    if (pctLog) WriteOut (stdout, "\n");
    OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 6);
    if (displayValue) {
      WriteOut (stdout, "        CDF1 value> ");
      if (TT2000dataType(varDataType1)) style = TT2000_3_STYLE;
      else style = EPOCH0_STYLE;
      if ((varDataType1 == CDF_FLOAT || varDataType1 == CDF_REAL4) &&
          *(float *)value1 <= DEFAULT_FLOAT_PADVALUE)
        EncodeValuesFormat (varDataType1, varNumElems1, value1, oText, NULL, 0,
			    MAX_SCREENLINE_LEN - 21, style,
			    (size_t) sizeof(oText));
      else if ((varDataType1 == CDF_DOUBLE || varDataType1 == CDF_REAL8) &&
               *(double *)value1 <= DEFAULT_DOUBLE_PADVALUE)
        EncodeValuesFormat (varDataType1, varNumElems1, value1, oText, NULL, 0,
			    MAX_SCREENLINE_LEN - 21, style,
			    (size_t) sizeof(oText));
      else
        EncodeValuesFormat (varDataType1, varNumElems1, value1, oText, format1, 0,
			    MAX_SCREENLINE_LEN - 21, style,
			    (size_t) sizeof(oText));
      WriteOut (stdout, oText);
      WriteOut (stdout, "\n");
      WriteOut (stdout, "        CDF2 value> ");
      if (TT2000dataType(varDataType2)) style = TT2000_3_STYLE;
      else style = EPOCH0_STYLE;
      if ((varDataType2 == CDF_FLOAT || varDataType2 == CDF_REAL4) &&
          *(float *)value2 <= DEFAULT_FLOAT_PADVALUE)
        EncodeValuesFormat (varDataType2, varNumElems2, value2, oText, NULL, 0,
			    MAX_SCREENLINE_LEN - 21, style,
			    (size_t) sizeof(oText));
      else if ((varDataType2 == CDF_DOUBLE || varDataType2 == CDF_REAL8) &&
               *(double *)value2 <= DEFAULT_DOUBLE_PADVALUE)
        EncodeValuesFormat (varDataType2, varNumElems2, value2, oText, NULL, 0,
			    MAX_SCREENLINE_LEN - 21, style,
			    (size_t) sizeof(oText));
      else        
        EncodeValuesFormat (varDataType2, varNumElems2, value2, oText, format2, 0,
			    MAX_SCREENLINE_LEN - 21, style,
			    (size_t) sizeof(oText));
      WriteOut (stdout, oText);
      WriteOut (stdout, "\n");
    }
    if (pctLog) {
      WriteOut (stdout, typeMsg);
      WriteOut (stdout, "    ");
      WriteOutPct (lastPct);
    }
  }
  return;
}

/******************************************************************************
* ReportValueDifferenceTotals.
******************************************************************************/

void ReportValueDifferenceTotals (recordDiffCount, valueDiffCount, varName, v)
long recordDiffCount;
long valueDiffCount;
char *varName;
char *v;
{
  if (recordDiffCount > 0) {
    diffFound = TRUE;
    snprintf (oText, (size_t) sizeof(oText),
	      "DIFFERENCES in %ld record%s for %s \"%s\"",
	      recordDiffCount, (recordDiffCount > 1 ? "s" : ""), v, varName);
    snprintf (EofS(oText), (size_t) sizeof(oText)-strlen(oText),
	      " (%ld value%s total)", valueDiffCount,
	      (valueDiffCount > 1 ? "s" : ""));
    OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 4);
  }
  return;
}

/******************************************************************************
* ParseTolerances.
* It can be assumed that there is at least one character in `values' or QOP
* would have failed.
******************************************************************************/

Logical ParseTolerances (values, floatValueAddr, floatSet, doubleValueAddr, 
                         doubleSet)
char *values;
float *floatValueAddr;
Logical *floatSet;
double *doubleValueAddr;
Logical *doubleSet;
{
  char string[MAX_TOLERANCES_LEN+1];
  int i, Ncommas;
  char *ptr;
  /****************************************************************************
  * Strip leading and trailing white space from 1st string.
  ****************************************************************************/
  ptr = values;
  while (*ptr != NUL && Spacing(*ptr)) ptr++;
  if (*ptr == NUL) return FALSE;                 /* Null-string. */
  /****************************************************************************
  * Move tolerance values to a temporary buffer and remove all spacing.
  ****************************************************************************/
  strcpyX (string, ptr, MAX_TOLERANCES_LEN);
  /****************************************************************************
  * Check for (and skip) parenthesis on VMS systems.
  ****************************************************************************/
#if defined(vms)
  if (*ptr == '(') {
    char *lastChar = ptr + strlen(ptr) - 1;
    if (*lastChar != ')') return FALSE;
    ptr++;
    *lastChar = NUL;
    strcpyX (string, ptr, MAX_TOLERANCES_LEN);
  }
#endif
  /****************************************************************************
  * Comma separated values.
  ****************************************************************************/
  for (Ncommas = 0, i = 0; string[i] != NUL; i++)       /* Count commas. */
     if (string[i] == ',') Ncommas++;
  ptr = string;
  for (i = 0; i <= Ncommas; i++) {
     if (*ptr == 'f' || *ptr == 'F') {
	ptr = ptr + 2;
	if ((strncmpIgCasePattern(ptr, "default", 7) == 0) || 
	    (strncmpIgCasePattern(ptr, "def", 3) == 0)) {
	  *floatSet = TRUE;
	  *floatValueAddr = (float) DEFAULT_FLOAT_TOLERANCE;
	} else {
	  if (sscanf(ptr,"%f",(float *) floatValueAddr) != 1)
	    return FALSE;
	  else
	    *floatSet = TRUE;
	}
     } else if (*ptr == 'd' || *ptr == 'D') {
	 ptr = ptr + 2;
	 if ((strncmpIgCasePattern(ptr, "default", 7) == 0) ||
	     (strncmpIgCasePattern(ptr, "def", 3) == 0)) {
           *doubleSet = TRUE;
           *doubleValueAddr = (double) DEFAULT_DOUBLE_TOLERANCE;
         } else {
           if (sscanf(ptr,"%lf",(double *) doubleValueAddr) != 1)
             return FALSE;
           else
             *doubleSet = TRUE;
	  }
     } else
       return FALSE;	
     ptr = strchr (ptr, ',') + 1;      /* Garbage last time, doesn't matter. */
  }
  return TRUE;
}

/******************************************************************************
* ValuesCmp.
* Compare the difference between two values against the tolerance.
* It returns TRUE if outside of the tolerance, FALSE otherwise.
******************************************************************************/

Logical ValuesCmp (value1, value2, dataType, floatValue, doubleValue)
void *value1;
void *value2;
long dataType;
double floatValue;
double doubleValue;
{
  int nanID;
  if (dataType == CDF_REAL4 || dataType == CDF_FLOAT) {
     /*************************************************************************
     * Check the sign of tolerance.
     * Use the tolerance as is to compare the difference if the tolerance is
     * positive. Othwise, use the relative tolerance to compare as
     * abs(value1-value2) > abs(tolerance)*max(abs(value1),abs(value2))
     *************************************************************************/
     nanID = CheckNaN(value1, value2, dataType);
     if (nanID == 1) return FALSE;
     if (nanID == 2) return TRUE;
     if (floatValue >= 0.0) {
       if (fabs(*((float *)value1) - *((float *)value2)) > floatValue) 
	 return TRUE;
       else return FALSE;
     } else {
       if (fabs(*((float *)value1) - *((float *)value2)) >
           fabs(floatValue)*MAXIMUM(fabs(*((float *)value1)),
                                    fabs(*((float *)value2))))
         return TRUE;
       else
         return FALSE;
     }
  } else if (dataType == CDF_REAL8 || dataType == CDF_DOUBLE ||
	     dataType == CDF_EPOCH) { 
     nanID = CheckNaN(value1, value2, dataType);
     if (nanID == 1) return FALSE;
     if (nanID == 2) return TRUE;
     if (doubleValue >= 0.0) {
       if (fabs(*((double *)value1) - *((double *)value2)) > doubleValue)
         return TRUE;
       else return FALSE;
     } else {
       if (fabs(*((double *)value1) - *((double *)value2)) >
           fabs(doubleValue)*MAXIMUM(fabs(*((double *)value1)),
                                     fabs(*((double *)value2))))
         return TRUE;
       else
         return FALSE;
     }
  } else if (dataType == CDF_EPOCH16) {
      double epoch16_a[2], epoch16_b[2];
      epoch16_a[0] = *((double *) value1);
      epoch16_a[1] = *(((double *) value1)+1);
      epoch16_b[0] = *((double *) value2);
      epoch16_b[1] = *(((double *) value2)+1);
     if (doubleValue >= 0.0) {
       if (fabs(epoch16_a[0] - epoch16_b[0]) > doubleValue || 
	   fabs(epoch16_a[1] - epoch16_b[1]) > doubleValue)
         return TRUE;
       else return FALSE;
     } else {
       if ((fabs(epoch16_a[0] - epoch16_b[0]) >
            fabs(doubleValue)*MAXIMUM(fabs(epoch16_a[0]),
                                      fabs(epoch16_b[0]))) ||
	   (fabs(epoch16_a[1] - epoch16_b[1]) >
	    fabs(doubleValue)*MAXIMUM(fabs(epoch16_a[1]),
                                      fabs(epoch16_b[1]))))
         return TRUE;
       else
         return FALSE;
     }
  }
  return TRUE;
}

/******************************************************************************
* CheckNaN.
* Check if there is any NaN(s) for the floating valuess.
* Returns: 1 if both are NaNs
*          2 if only one of them is NaN
*          3 if none of them is NaN
******************************************************************************/

int CheckNaN (value1, value2, dataType)
void *value1;
void *value2;
long dataType;
{
  if (dataType == CDF_REAL4 || dataType == CDF_FLOAT) {
     if ((isnan((double)*((float *) value1))) &&
         (isnan((double)*((float *) value2)))) return 1;
     if ((isnan((double)*((float *) value1))) ||
         (isnan((double)*((float *) value2)))) return 2;
  } else if (dataType == CDF_REAL8 || dataType == CDF_DOUBLE) { 
     if ((isnan(*((double *) value1))) &&
         (isnan(*((double *) value2)))) return 1;
     if ((isnan(*((double *) value1))) ||
         (isnan(*((double *) value2)))) return 2;
  }
  return 3;
}

/******************************************************************************
* CompareStrings.
* Compare 2 strings. If there is NUL ('\0') in a string, it is treated as 
* a space ' '. So, no difference is considered for this case.
* Returns: TRUE if both are NOT the same.
*          FALSE otherwise
******************************************************************************/

Logical CompareStrings (string1, string2, numElms, totalBytes)
char *string1;
char *string2;
long numElms;
long totalBytes;
{
  int i, k, l;
  int ix, iy;

  k = (int) (totalBytes / numElms);
  for (l = 0; l < k; ++l) {
    ix = iy = (int) numElms;
    for (i = 0; i < (int)numElms; ++i) {
      if (*(string1+l*(int)numElms+i) == NUL) {
        ix = i;
        break;
      }
    }
    for (i = 0; i < (int)numElms; ++i) {
      if (*(string2+l*(int)numElms+i) == NUL) {
        iy = i;
        break;
      }
    }
    if (ix != numElms && iy != numElms && ix != iy) return TRUE;
    if (ix == iy) {
        for (i = 0; i < ix; ++i) {
          if (*(string1+l*(int)numElms+i) != *(string2+l*(int)numElms+i))
            return TRUE;
        }
    } else {
      for (i = 0; i < MINIMUM(ix, iy); ++i) {
        if (*(string1+l*(int)numElms+i) != *(string2+l*(int)numElms+i))
          return TRUE;
      }
      if (ix > iy) {
        for (i = iy; i < ix; ++i) { 
          if (*(string1+l*(int)numElms+i) != ' ') return TRUE;
        }
      } else {
        for (i = ix; i < iy; ++i) {            
          if (*(string2+l*(int)numElms+i) != ' ') return TRUE;
        }
      }
    }
  }
  return FALSE;
}

/******************************************************************************
* ConvertQOPs.
*    Returns TRUE if execution should continue.
******************************************************************************/

#if defined(mac)
Logical CompareQOPs (argC, argV)
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
  UserItemUPP  OutlineDefaultButtonUPP;
#endif
  short itemN, iType, i;
  static Logical first = TRUE;
  char cancelTitle[] = "Cancel";

  static Logical pageOutput = DEFAULTpageCVT;
  static Logical compareAttrs = DEFAULTattrCMP;
  static Logical compareVars = DEFAULTvarCMP;
  static Logical compareNums = DEFAULTnumberCMP;
  static Logical compareEtc = DEFAULTetcCMP;
  static Logical negToPos = DEFAULT_NEGtoPOSfp0;
  static Logical reportInfos = REPORTinfosDEFAULT;
  static Logical reportWarns = REPORTwarningsDEFAULT;
  static Logical reportErrors = REPORTerrorsDEFAULT;
  static Logical logMsg = DEFAULTlogCMP;
  static Logical dispPct = DEFAULTpctCMP;
  static Logical dispLocs = DEFAULTlocationCMP;
  static Logical dispStats = DEFAULTstatsCMP;
  static Logical dispValues = DEFAULTvalueCMP;
  static int zMode1 = DEFAULTzModeCMP;
  static int zMode2 = DEFAULTzModeCMP;
  static Str255 CDF1text = "\p";
  static Str255 CDF2text = "\p";
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

  SetIText ((Handle) controlHs[CDF1TEXTin], CDF1text);
  SetIText ((Handle) controlHs[CDF2TEXTin], CDF2text);
  SetIText ((Handle) controlHs[CACHEin], cacheText);

  if (pageOutput) SetCtlValue (controlHs[PAGEin], 1);
  if (compareAttrs) SetCtlValue (controlHs[ATTRin], 1);
  if (compareVars) SetCtlValue (controlHs[VARin], 1);
  if (compareNums) SetCtlValue (controlHs[NUMin], 1);
  if (compareEtc) SetCtlValue (controlHs[ETCin], 1);
  if (negToPos) SetCtlValue (controlHs[NEGZin], 1);
  if (reportInfos) SetCtlValue (controlHs[INFOin], 1);
  if (reportWarns) SetCtlValue (controlHs[WARNin], 1);
  if (reportErrors) SetCtlValue (controlHs[ERRORin], 1);
  if (logMsg) SetCtlValue (controlHs[LOGin], 1);
  if (dispPct) SetCtlValue (controlHs[PCTin], 1);
  if (dispLocs) SetCtlValue (controlHs[LOCin], 1);
  if (dispStats) SetCtlValue (controlHs[STATSin], 1);
  if (dispValues) SetCtlValue (controlHs[VALUEin], 1);

  SetCtlValue (controlHs[ZMODE1inBASE+zMode1], 1);
  SetCtlValue (controlHs[ZMODE2inBASE+zMode2], 1);

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
	char tempS3[3+1];

	/**********************************************************************
	* Get the value of each control.
	**********************************************************************/

	GetIText ((Handle) controlHs[CDF1TEXTin], CDF1text);
	GetIText ((Handle) controlHs[CDF2TEXTin], CDF2text);
	GetIText ((Handle) controlHs[CACHEin], cacheText);

	pageOutput = GetCtlValue (controlHs[PAGEin]);
	compareAttrs = GetCtlValue (controlHs[ATTRin]);
	compareVars = GetCtlValue (controlHs[VARin]);
	compareNums = GetCtlValue (controlHs[NUMin]);
	compareEtc = GetCtlValue (controlHs[ETCin]);
	negToPos = GetCtlValue (controlHs[NEGZin]);
	reportInfos = GetCtlValue (controlHs[INFOin]);
	reportWarns = GetCtlValue (controlHs[WARNin]);
	reportErrors = GetCtlValue (controlHs[ERRORin]);
	logMsg = GetCtlValue (controlHs[LOGin]);
	dispPct = GetCtlValue (controlHs[PCTin]);
	dispLocs = GetCtlValue (controlHs[LOCin]);
	dispStats = GetCtlValue (controlHs[STATSin]);
	dispValues = GetCtlValue (controlHs[VALUEin]);
	
	for (zMode1 = 0; zMode1 < 3; zMode1++) {
	   if (GetCtlValue(controlHs[ZMODE1inBASE+zMode1])) break;
	}
	for (zMode2 = 0; zMode2 < 3; zMode2++) {
	   if (GetCtlValue(controlHs[ZMODE2inBASE+zMode2])) break;
	}
	
	/**********************************************************************
	* Build argc/argv.
	**********************************************************************/

	*argC = 16 + BOO(NULpString(CDF1text),0,1) +
			     BOO(NULpString(CDF2text),0,1) +
			     BOO(NULpString(cacheText),0,2);
	*argV = (char **) cdf_AllocateMemory ((size_t)*argC * sizeof(char *), FatalError);
	
	n = 0;
	MAKEstrARGv (argV, n, pgmName)

	if (!NULpString(CDF1text)) {
	  PtoCstr (CDF1text);
	  MAKEstrARGv (argV, n, (char *) CDF1text)
	  CtoPstr ((char *) CDF1text);
	}

	if (!NULpString(CDF2text)) {
	  PtoCstr (CDF2text);
	  MAKEstrARGv (argV, n, (char *) CDF2text)
	  CtoPstr ((char *) CDF2text);
	}

	MAKEbooARGv (argV, n, pageOutput, "-page", "-nopage")
	MAKEbooARGv (argV, n, compareAttrs, "-attr", "-noattr")
	MAKEbooARGv (argV, n, compareVars, "-var", "-novar")
	MAKEbooARGv (argV, n, compareNums, "-number", "-nonumber")
	MAKEbooARGv (argV, n, compareEtc, "-etc", "-noetc")
	MAKEbooARGv (argV, n, negToPos, "-neg2posfp0", "-noneg2posfp0")
	MAKEbooARGv (argV, n, logMsg, "-log", "-nolog")
	MAKEbooARGv (argV, n, dispPct, "-percent", "-nopercent")
	MAKEbooARGv (argV, n, dispLocs, "-location", "-nolocation")
	MAKEbooARGv (argV, n, dispStats, "-statistics", "-nostatistics")
	MAKEbooARGv (argV, n, dispValues, "-value", "-novalue")
	
	MAKEstrARGv (argV, n, "-zmodes")
	snprintf (tempS3, (size_t) sizeof(tempS3),
	          "%d,%d", zMode1, zMode2);
	MAKEstrARGv (argV, n, tempS3)

	MAKEstrARGv (argV, n, "-report")
	MAKEstrARGv (argV, n, StatusCodeReportOptions(reportErrors,
						      reportWarns,
						      reportInfos))

	if (!NULpString(cacheText)) {
	  MAKEstrARGv (argV, n, "-cache")
	  PtoCstr (cacheText);
	  MAKEstrARGv (argV, n, (char *) cacheText)
	  CtoPstr ((char *) cacheText);
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
	*argV = (char **) cdf_AllocateMemory ((size_t)*argC * sizeof(char *), FatalError);
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
      * Select CDF/1 specification.
      ************************************************************************/
      case CDF1SELECTin: {
	StandardFileReply reply;
	char path[DU_MAX_PATH_LEN+1];
#ifndef __MWERKS__
	StandardGetFile (FilterForCDFs, -1, NULL, &reply);
#else
	FilterForCDFsUPP = NewFileFilterProc((ProcPtr) FilterForCDFs);
	StandardGetFile (FilterForCDFsUPP, -1, NULL, &reply);
        DisposeRoutineDescriptor (FilterForCDFsUPP);
#endif
	if (reply.sfGood && !reply.sfIsFolder && !reply.sfIsVolume) {
	  BuildMacPath (&reply.sfFile, path, TRUE);
	  CDF1text[0] = strlen (path);
	  strcpyX ((char *) &CDF1text[1], path, 255);
	  SetIText ((Handle) controlHs[CDF1TEXTin], CDF1text);
	}
	break;
      }
      /************************************************************************
      * Select CDF/2 specification.
      ************************************************************************/
      case CDF2SELECTin: {
	StandardFileReply reply;
	char path[DU_MAX_PATH_LEN+1];
#ifndef __MWERKS__
	StandardGetFile (FilterForCDFs, -1, NULL, &reply);
#else
	FilterForCDFsUPP = NewFileFilterProc((ProcPtr) FilterForCDFs);
	StandardGetFile (FilterForCDFsUPP, -1, NULL, &reply);
        DisposeRoutineDescriptor (FilterForCDFsUPP);
#endif
	if (reply.sfGood && !reply.sfIsFolder && !reply.sfIsVolume) {
	  BuildMacPath (&reply.sfFile, path, TRUE);
	  CDF2text[0] = strlen (path);
	  strcpyX ((char *) &CDF2text[1], path, 255);
	  SetIText ((Handle) controlHs[CDF2TEXTin], CDF2text);
	}
	break;
      }
      /************************************************************************
      * Check boxes.
      ************************************************************************/
      case PAGEin:
      case ATTRin:
      case VARin:
      case NUMin:
      case ETCin:
      case NEGZin:
      case INFOin:
      case WARNin:
      case ERRORin:
      case LOGin:
      case PCTin:
      case LOCin:
      case STATSin:
      case VALUEin:
	SetCtlValue (controlHs[itemN], BOO(GetCtlValue(controlHs[itemN]),0,1));
		break;
      /************************************************************************
      * Radio buttons.
      ************************************************************************/
      case ZMODE1inBASE+0:
      case ZMODE1inBASE+1:
      case ZMODE1inBASE+2:
	for (i = 0; i < 3; i++) SetCtlValue (controlHs[ZMODE1inBASE+i], 0);
	SetCtlValue (controlHs[itemN], 1);
	break;
      case ZMODE2inBASE+0:
      case ZMODE2inBASE+1:
      case ZMODE2inBASE+2:
	for (i = 0; i < 3; i++) SetCtlValue (controlHs[ZMODE2inBASE+i], 0);
	SetCtlValue (controlHs[itemN], 1);
	break;
      }
  }
}
#endif
