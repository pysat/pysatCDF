/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF						      SkeletonTable.
*
*  Version 1.4b, 18-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  18-May-92, J Love	Original version.
*   V1.1   2-Sep-92, J Love	CDF V2.3 (shareable/NeXT/zVar).
*   V1.2  25-Jan-94, J Love	CDF V2.4.
*   V1.3  13-Dec-94, J Love	CDF V2.5.
*   V1.3a 10-Jan-95, J Love	Uppercase file extensions on the Macintosh.
*   V1.3b 28-Feb-95, J Love	Pass `char' as `int'.
*   V1.3c  6-Apr-95, J Love	POSIX.
*   V1.3d  9-May-95, J Love	EPOCH styles.
*   V1.3e  4-Aug-95, J Love	CDFexport-related changes.
*   V1.3f 19-Sep-95, J Love	CHECKforABORTso.
*   V1.3g 28-Sep-95, J Love	Macintosh dialog filtering.  Outline default
*				button.
*   V1.4  26-Aug-96, J Love	CDF V2.6.
*   V1.4a 24-Feb-97, J Love	Removed RICE.  Fixed `values' qualifier.
*   V1.4b 18-Nov-97, J Love	Windows NT/Visual C++.
*   V1.5  13-Oct-06, M Liu      Changed to allow upper and lower case CDF  
*                               name to be used on win32.
*   V3.3  10-Jan-09, M Liu      Validate a file before it is used.
*   V3.3a 20-Jul-09, M Liu      Added validation for input parameters.
*   V3.3b 04-Apr-11, M Liu      Modified to handle TT2000 epoch.
*
******************************************************************************/

#include "cdf2skt.h"

/******************************************************************************
* Increased stack size for Borland C on IBM PC.
******************************************************************************/

#if defined(BORLANDC)
extern unsigned _stklen = BORLANDC_STACK_SIZE;
#endif

/******************************************************************************
* Global variables.
******************************************************************************/

Logical mLog;
FILE *SKTfp;
Logical useFormat;
Logical report[3];
Logical displayStats;

/******************************************************************************
* Main.
******************************************************************************/

#if !defined(win32) || (defined(win32) && defined(ALONE))
MAIN {
  Logical success = TRUE;
  strcpyX (pgmName, "SkeletonTable", MAX_PROGRAM_NAME_LEN);
#if defined(mac)
  MacExecuteSO (CreateSkeletonTable, SkeletonTableQOPs);
#else
  success = CreateSkeletonTable (argc, argv);
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
* CreateSkeletonTable.
******************************************************************************/

Logical CreateSkeletonTable (argC, argV)
int argC;
char *argV[];
{
CDFstatus status; CDFid id;
char CDFpath[DU_MAX_PATH_LEN+1], CDFpathX[DU_MAX_PATH_LEN+1];
char CDFname[DU_MAX_NAME_LEN+1], dir[DU_MAX_DIR_LEN+1];
char SKTpath[DU_MAX_PATH_LEN+1], SKTpathX[DU_MAX_PATH_LEN+1];
char creationStamp[TIME_STAMP_LEN+1];
int count, varValues; long zMode; Logical negToPosFp0, outToScreen;
long workingCache, stageCache, compressCache;
char *variables = NULL;
Logical qopError = FALSE;
QOP *qop;
static char *validQuals[] = {
  "skeleton", "nonrv", "nrvfile", "nrvtable", "log", "nolog", "zmode",
  "neg2posfp0", "noneg2posfp0", "format", "noformat", "report", "screen",
  "noscreen", "cache", "values", "page", "nopage", "about", "statistics",
  "nostatistics", NULL };
static int optRequired[] = {
  TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
  TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, 0
};
static char *reportTokens[] = { "errors", "warnings", "informationals" };

/******************************************************************************
* Parse qualifiers/options/parameters (QOP).
******************************************************************************/

switch (argC) {
  case 1:
    PageOLH ("cdf2skt.olh", argV[0]);
    return TRUE;
  case 2:
    if (strcmp(argV[1],"-java") == 0) {
      pagingOn = FALSE;
      PageOLH ("cdf2sktj.olh", argV[0]);
      return TRUE;
    }
  default:
    qop = Qop (argC, argV, validQuals, optRequired);
    if (qop == NULL) return FALSE;
    /**************************************************************************
    * Check for `about' qualifier.
    **************************************************************************/
    if (qop->qualEntered[ABOUTqual]) {
      DisplayIdentification (pgmName);
      cdf_FreeMemory (qop, FatalError);
      return TRUE;
    }
    /**************************************************************************
    * Get CDF pathname.
    **************************************************************************/
    if (qop->Nparms < 1) {
      DisplayError ("Missing parameter.");
      qopError = TRUE;
    }
    else {
      strcpyX (CDFpath, qop->parms[CDFPATHparm], DU_MAX_PATH_LEN);
      ParsePath (CDFpath, dir, CDFname);
    }
    /**************************************************************************
    * Check if a skeleton table pathname was specified.
    **************************************************************************/
    if (qop->qualEntered[SKTqual]) {
      if (!ValidateQual(qop->qualOpt[SKTqual], validQuals))
        strcpyX (SKTpath, qop->qualOpt[SKTqual], DU_MAX_PATH_LEN);
      else {
        DisplayError ("Invalid \"skeleton\" qualifier.");
        qopError = TRUE;
      }
    } else {
      if (EndsWithIgCase(CDFname, ".cdf"))
        RemoveCDFFileExtension (CDFname, SKTpath);
      else
        strcpyX (SKTpath, CDFname, DU_MAX_PATH_LEN);
    }
    /**************************************************************************
    * Check if which variable values should be displayed was specified.
    **************************************************************************/
    count = 0;
    if (qop->qualEntered[NONRVqual]) count++;
    if (qop->qualEntered[NRVFILEqual]) count++;
    if (qop->qualEntered[NRVTABLEqual]) count++;
    if (qop->qualEntered[VALUESqual]) count++;
    switch (count) {
      case 0:
	varValues = DEFAULTvaluesCDF2SKT;
	break;
      case 1:
	if (qop->qualEntered[NONRVqual]) {
	  varValues = NOvalues;
	  break;
	}
	if (qop->qualEntered[NRVFILEqual]) {
	  DisplayWarning ("NRV files are no longer supported.");
	  varValues = NRVvalues;
	  break;
	}
	if (qop->qualEntered[NRVTABLEqual]) {
	  varValues = NRVvalues;
	  break;
	}
	if (qop->qualEntered[VALUESqual]) {
          if (ValidateQual(qop->qualOpt[VALUESqual], validQuals)) {
            DisplayError ("Invalid \"values\" qualifier.");
            qopError = TRUE;
          } else {
	    size_t len = strlen(qop->qualOpt[VALUESqual]);
	    if (strcmpIgCase(qop->qualOpt[VALUESqual],"none") == 1)
	      varValues = NOvalues;
	    else if (strcmpIgCase(qop->qualOpt[VALUESqual],"nrv") == 1)
	      varValues = NRVvalues;
	    else if (strcmpIgCase(qop->qualOpt[VALUESqual],"rv") == 1)
	      varValues = RVvalues;
	    else if (strcmpIgCase(qop->qualOpt[VALUESqual],"all") == 1)
	      varValues = ALLvalues;
	    else {
	      varValues = NAMEDvalues;
 	      variables = cdf_AllocateMemory ((size_t)len + 1, FatalError);
 	      strcpyX (variables, qop->qualOpt[VALUESqual], len);
	    }
	  }
	}
	break;
      default:
	DisplayError ("Specify only one variable values qualifier.");
	qopError = TRUE;
    }
    /**************************************************************************
    * Check for /ZMODE,-zmode qualifier.
    **************************************************************************/
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
      zMode = DEFAULTzModeCDF2SKT;
    /**************************************************************************
    * Check for `report' qualifier.  If absent, use defaults.
    **************************************************************************/
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
    /**************************************************************************
    * Check for `statistics', `log', `neg2posfp0', `screen', `page', and
    * `format' qualifiers.
    **************************************************************************/
    qopError = qopError | !TFqualifier (qop,&displayStats,STATSqual,
					NOSTATSqual,DEFAULTstatsCDF2SKT,
					"statistics");
    qopError = qopError | !TFqualifier(qop,&outToScreen,SCREENqual,
				       NOSCREENqual,DEFAULTscreenCDF2SKT,
				       "screen");
    qopError = qopError | !TFqualifier(qop,&pagingOn,PAGEqual,NOPAGEqual,
				       DEFAULTpageCDF2SKT,"page");
    qopError = qopError | !TFqualifier(qop,&mLog,LOGqual,NOLOGqual,
				       DEFAULTlogCDF2SKT,"log");
    qopError = qopError | !TFqualifier(qop,&negToPosFp0,NEG2POSqual,
				       NONEG2POSqual,DEFAULT_NEGtoPOSfp0,
				       "neg2posfp0");
    qopError = qopError | !TFqualifier(qop,&useFormat,FORMATqual,NOFORMATqual,
				       DEFAULTformatCDF2SKT,"format");
    /**************************************************************************
    * Check for conflicting/missing qualifiers.
    **************************************************************************/
    if (qop->qualEntered[SKTqual] && qop->qualEntered[SCREENqual]) {
      DisplayError ("Conflicting qualifiers (`skeleton' and `screen').");
      qopError = TRUE;
    }
    if (!qop->qualEntered[SCREENqual] && qop->qualEntered[PAGEqual]) {
      DisplayError ("Conflicting qualifiers (`noscreen' and `page').");
      qopError = TRUE;
    }
    /**************************************************************************
    * Check for overridding qualifiers.
    **************************************************************************/
    if (qop->qualEntered[SCREENqual]) mLog = FALSE;
    /**************************************************************************
    * Free QOP memory and check for an error.
    **************************************************************************/
    cdf_FreeMemory (qop, FatalError);
    if (qopError) return FALSE;
    break;
}

/* CDFsetValidate (VALIDATEFILEon); */
/******************************************************************************
* Create skeleton table.
******************************************************************************/

if (mLog) {
  WriteOut (stdout, "Name of CDF: ");
  WriteOut (stdout, CDFname);
  WriteOut (stdout, "\n");
}

TimeStamp (creationStamp);

if (outToScreen)
  SKTfp = stdout;
else {
  if (!EndsWithIgCase(SKTpath, ".skt"))
    strcatX (SKTpath, ".skt", DU_MAX_PATH_LEN);
  ExpandPath (SKTpath, SKTpathX);

/******************************************************************************
* Display information.
******************************************************************************/

  WriteOut (stdout, "Making  a skeleton table from \"");
  WriteOut (stdout, CDFpath);
  if (!EndsWithIgCase(CDFpath, ".cdf"))
    WriteOut (stdout, ".cdf");
  WriteOut (stdout, "\"  to  \"");
  WriteOut (stdout, SKTpath);
  WriteOut (stdout, "\"...\n");

  SKTfp = fopen (SKTpathX, "w");
  if (SKTfp == NULL) {
    WriteOut (stdout, "Error creating skeleton table (");
    WriteOut (stdout, SKTpath);
    WriteOut (stdout, ").\n");
    return FALSE;
  }
}

WriteOut (SKTfp, "! Skeleton table for the \"");
WriteOut (SKTfp, CDFname);
WriteOut (SKTfp, "\" CDF.");

WriteOut (SKTfp, "\n! Generated: ");
WriteOut (SKTfp, creationStamp);

/******************************************************************************
* Open CDF.
******************************************************************************/

ExpandPath (CDFpath, CDFpathX);
status = CDFlib (OPEN_, CDF_, CDFpathX, &id,
		 SELECT_, CDF_READONLY_MODE_, READONLYon,
			  CDF_zMODE_, zMode,
			  CDF_NEGtoPOSfp0_MODE_, BOO(negToPosFp0,
			  			     NEGtoPOSfp0on,
			  			     NEGtoPOSfp0off),
			  CDF_CACHESIZE_, workingCache,
			  STAGE_CACHESIZE_, stageCache,
			  COMPRESS_CACHESIZE_, compressCache,
		 NULL_);
if (!StatusHandlerC2S(status)) return FALSE;

/******************************************************************************
* Write header section.
******************************************************************************/

CHECKforABORTso
if (mLog) WriteOut (stdout, "Printing Header Information...\n");
if (!WriteHeader(CDFname, varValues, variables)) return FALSE;

/******************************************************************************
* Write global scope attributes section.
******************************************************************************/

if (mLog) WriteOut (stdout, "Printing Attribute Information...\n");
if (!WriteGlobalAttr()) return FALSE;

/******************************************************************************
* Write variable scope attributes section.
******************************************************************************/

if (!WriteVarAttr()) return FALSE;

/******************************************************************************
* Write variables section.
******************************************************************************/

if (mLog) WriteOut (stdout, "Printing Variable Information...\n");
if (!WriteVars(varValues,variables)) return FALSE;

/******************************************************************************
* End section.
******************************************************************************/

if (!WriteEnd()) return FALSE;

/******************************************************************************
* Clean up.
******************************************************************************/

WriteOut (SKTfp, "\n");
if (SKTfp != stdout) fclose (SKTfp);

if (displayStats) {
  vSTATS vStatsDotCDF, vStatsStage, vStatsCompress;
  status = CDFlib (CLOSE_, CDFwithSTATS_, &vStatsDotCDF,
					  &vStatsStage,
					  &vStatsCompress,
		   NULL_);
  if (!StatusHandlerC2S(status)) return FALSE;
  DisplayStatistics ("CDF", &vStatsDotCDF, &vStatsStage, &vStatsCompress);
}
else {
  status = CDFlib (CLOSE_, CDF_,
		   NULL_);
  if (!StatusHandlerC2S(status)) return FALSE;
}

if (variables != NULL) cdf_FreeMemory (variables, FatalError);

return TRUE;
}


/******************************************************************************
* WriteHeader.
******************************************************************************/

Logical WriteHeader (CDFname, varValues, variables)
char *CDFname;
int varValues;
char *variables;
{
CDFstatus status;
long encoding, majority, format;
long numDims;
long dimSizes[CDF_MAX_DIMS];
long numVars, numZvars, maxRec;
long numVattrs, numGattrs;
int  dimN;
long lastUpdated = -1L, version, release, increment;
char sizes[MAX_SIZES_LEN+1], subIncrement;
char text[MAX_TEXT_LEN+1], tempS[MAX_LINE_LEN+1];
long compress, compParms[CDF_MAX_DIMS], compPct, checkSum;

status = CDFlib (GET_, CDF_VERSION_, &version,
		       CDF_RELEASE_, &release,
		       CDF_INCREMENT_, &increment,
		 NULL_);
if (!StatusHandlerC2S(status)) return FALSE;

if (version > 2) {
  status = CDFlib (GET_, CDF_LEAPSECONDLASTUPDATED_, &lastUpdated,
 		   NULL_);
  if (!StatusHandlerC2S(status)) return FALSE;
} 

WriteOutFP (SKTfp, "\n! CDF created/modified by CDF V%ld.%ld.%ld",
	    version, release, increment);

status = CDFlib (GET_, LIB_VERSION_, &version,
		       LIB_RELEASE_, &release,
		       LIB_INCREMENT_, &increment,
		       LIB_subINCREMENT_, &subIncrement,
		 NULL_);
if (!StatusHandlerC2S(status)) return FALSE;

if (subIncrement != ' ')
  WriteOutFP (SKTfp, "\n! Skeleton table created by CDF V%ld.%ld.%ld_%c",
	      version, release, increment, subIncrement);
else
  WriteOutFP (SKTfp, "\n! Skeleton table created by CDF V%ld.%ld.%ld",
              version, release, increment);

WriteOut (SKTfp, "\n");
WriteOut (SKTfp, "\n#header");

status = CDFlib (GET_, CDF_ENCODING_, &encoding,
		       CDF_MAJORITY_, &majority,
		       CDF_FORMAT_, &format,
		       rVARs_NUMDIMS_, &numDims,
		       rVARs_DIMSIZES_, dimSizes,
		       CDF_NUMrVARS_, &numVars,
		       CDF_NUMzVARS_, &numZvars,
		       CDF_NUMgATTRS_, &numGattrs,
		       CDF_NUMvATTRS_, &numVattrs,
		       rVARs_MAXREC_, &maxRec,
		 NULL_);
if (!StatusHandlerC2S(status)) return FALSE;

WriteOut (SKTfp, "\n");
WriteOut (SKTfp, "\n                       CDF NAME: ");
WriteOut (SKTfp, CDFname);

WriteOut (SKTfp, "\n                  DATA ENCODING: ");
WriteOut (SKTfp, EncodingToken(encoding));

WriteOut (SKTfp, "\n                       MAJORITY: ");
WriteOut (SKTfp, MajorityToken(majority));

WriteOut (SKTfp, "\n                         FORMAT: ");
WriteOut (SKTfp, FormatToken(format));

if (numDims > 0) {
  sizes[0] = NUL;
  for (dimN = 0; dimN < numDims; dimN++) {
     snprintf (&sizes[strlen(sizes)], (size_t) sizeof(sizes)-strlen(sizes),
	       "%ld ", dimSizes[dimN]);
  }
  sizes[strlen(sizes)-1] = NUL;			/* Wipe out trailing blank. */
}
else
  strcpyX (sizes, "", MAX_SIZES_LEN);

WriteOut (SKTfp, "\n");
WriteOut (SKTfp,
	  "\n! Variables  G.Attributes  V.Attributes  Records  Dims  ");

Ncharacters (SKTfp, FRONTpadLABEL("Sizes",sizes), (int) ' ');
WriteOut (SKTfp, "Sizes");

WriteOut (SKTfp,
	  "\n! ---------  ------------  ------------  -------  ----  ");

Ncharacters (SKTfp, MaxInt((int)strlen("Sizes"),(int)strlen(sizes)), (int)'-');

WriteOut (SKTfp, "\n");
Ncharacters (SKTfp, 2, (int) ' ');
snprintf (text, (size_t) sizeof(text), "%ld/%ld", numVars, numZvars);
Ncharacters (SKTfp, MaxInt((int)(6 - strlen(text)),0), (int) ' ');
snprintf (tempS, (size_t) sizeof(tempS),
	  "%s     %7ld       %7ld     %5ld/z    %3ld",
	  text, numGattrs, numVattrs, maxRec + 1, numDims);
WriteOut (SKTfp, tempS);

if (numDims > 0) {
  WriteOut (SKTfp, "   ");
  Ncharacters (SKTfp, FRONTpadVALUE("Sizes",sizes), (int) ' ');
  WriteOut (SKTfp, sizes);
}

status = CDFlib (GET_, CDF_COMPRESSION_, &compress, compParms, &compPct,
                 NULL_);
if (!StatusHandlerC2S(status)) return FALSE;

/*
if (compress != NO_COMPRESSION || compress != NO_CHECKSUM)
  WriteOutFP (SKTfp, "\n");
*/

/* if (compress != NO_COMPRESSION) */
  WriteOutFP (SKTfp, "\n! CDF_COMPRESSION: %s",
              CompressionToken(compress, compParms));
  WriteOutFP (SKTfp, "\n! (Valid compression: None, GZIP.1-9, RLE.0, HUFF.0, AHUFF.0)");

status = CDFlib (GET_, CDF_CHECKSUM_, &checkSum,
                 NULL_);
if (!StatusHandlerC2S(status)) return FALSE;

/* if (checkSum != NO_CHECKSUM) */
WriteOutFP (SKTfp, "\n! CDF_CHECKSUM: %s", ChecksumToken(checkSum)); 
WriteOutFP (SKTfp, "\n! (Valid checksum: None, MD5)");

if (!PriorTo ("3.5.0", version, release, increment)) {
  if (lastUpdated > -1 || WriteTT2000VarData(varValues, variables)) {
    if (lastUpdated > -1)
      WriteOutFP (SKTfp, "\n! CDF_LEAPSECONDLASTUPDATED: %ld", lastUpdated); 
    else {
      long year, month, day;
#if defined(vms)
      CDFgetLastDateinLeapSecondsTBL (&year, &month, &day);
#else
      CDFgetLastDateinLeapSecondsTable (&year, &month, &day);
#endif
      WriteOutFP (SKTfp, "\n! CDF_LEAPSECONDLASTUPDATED: %ld", 10000*year+
                                                               100*month+day);
    }
  }
}
return TRUE;
}


/******************************************************************************
* WriteGlobalAttr.
******************************************************************************/

Logical WriteGlobalAttr ()
{
CDFstatus status;
char delim;			     /* Delimeter for attribute name. */
long attrN, numAttrs, scope, maxEntry;
long dataType, numElements, entryN;
int ccc;			     /* Current Cursor Column (base is 0). */
void *value;
char attrName[CDF_ATTR_NAME_LEN256+1],
     tempS[MAX_LINE_LEN+1];
int GattrCount = 0;		     /* Number of global scope attributes. */
int entryCount;

WriteOut (SKTfp, "\n\n");
WriteOut (SKTfp, "\n#GLOBALattributes");

status = CDFlib (GET_, CDF_NUMATTRS_, &numAttrs,
		 NULL_);
if (!StatusHandlerC2S(status)) return FALSE;

if (numAttrs > 0) {
  for (attrN = 0; attrN < numAttrs; attrN++) {
     status = CDFlib (SELECT_, ATTR_, attrN,
		      GET_, ATTR_NAME_, attrName,
			    ATTR_SCOPE_, &scope,
		      NULL_);
     if (!StatusHandlerC2S(status)) return FALSE;
     if (scope == GLOBAL_SCOPE) {
       /***********************************************************************
       * Global scope attribute...
       ***********************************************************************/
       GattrCount++;

       status = CDFlib (GET_, ATTR_MAXgENTRY_, &maxEntry,
		        NULL_);
       if (!StatusHandlerC2S(status)) return FALSE;

       /***********************************************************************
       * If first global scope attribute found, print field headings.
       ***********************************************************************/

       if (GattrCount == 1) {
	 WriteOut (SKTfp, "\n\n");
	 ccc = WriteOut (SKTfp, "! Attribute");
	 Ncharacters (SKTfp, ENTRY_NUM_COL - ccc, (int) ' ');
	 ccc += ENTRY_NUM_COL - ccc;
	 ccc += WriteOut (SKTfp, "Entry       Data");

	 WriteOut (SKTfp, "\n");
	 ccc = WriteOut (SKTfp, "! Name     ");
	 Ncharacters (SKTfp, ENTRY_NUM_COL - ccc, (int) ' ');
	 ccc += ENTRY_NUM_COL - ccc;
	 ccc += WriteOut (SKTfp, "Number      Type       Value");

	 WriteOut (SKTfp, "\n");
	 ccc = WriteOut (SKTfp, "! ---------");
	 Ncharacters (SKTfp, ENTRY_NUM_COL - ccc, (int) ' ');
	 ccc += ENTRY_NUM_COL - ccc;
	 ccc += WriteOut (SKTfp, "------      ----       -----");
       }

       /***********************************************************************
       * Write name.
       ***********************************************************************/

       delim = PickDelimiter (attrName, strlen(attrName));
       WriteOut (SKTfp, "\n\n");
       snprintf (tempS, (size_t) sizeof(tempS),
	         "  %c%s%c", delim, attrName, delim);
       ccc = WriteOut (SKTfp, tempS);

       /***********************************************************************
       * Write each entry.
       ***********************************************************************/

       entryCount = 0;

       for (entryN = 0; entryN <= maxEntry; entryN++) {
	  status = CDFlib (SELECT_, gENTRY_, entryN,
			   GET_, gENTRY_DATATYPE_, &dataType,
				 gENTRY_NUMELEMS_, &numElements,
			   NULL_);
	  if (status != NO_SUCH_ENTRY) {
	    if (!StatusHandlerC2S(status)) return FALSE;
	    entryCount++;

	    value = cdf_AllocateMemory ((size_t) (CDFelemSize(dataType) *
					numElements), FatalError);

	    status = CDFlib (GET_, gENTRY_DATA_, value,
			     NULL_);
	    if (!StatusHandlerC2S(status)) return FALSE;

	    if (entryCount == 1) {
	      if (ccc > ENTRY_NUM_COL - 1) {
		WriteOut (SKTfp, "\n");
		ccc = 0;
	      }
	    }
	    else {
	      WriteOut (SKTfp, "\n");
	      ccc = 0;
	    }

	    Ncharacters (SKTfp, ENTRY_NUM_COL - ccc, (int) ' ');
	    ccc += ENTRY_NUM_COL - ccc;

	    snprintf (tempS, (size_t) sizeof(tempS),
		      "%*ld:    %s   ", ENTRY_NUM_WIDTH, entryN + 1,
		      DataTypePaddedString(dataType));
	    ccc += WriteOut (SKTfp, tempS);

	    if (STRINGdataType (dataType)) { /* not allow LF and CR */
	      int ii;
	      for (ii = 0; ii < (int) numElements; ++ii) { 
	        if (((int)*(((char *)value)+ii) == (int) '\n') ||
	            ((int)*(((char *)value)+ii) == (int) '\r'))
		  memcpy (((char *)value)+ii, " ", 1);
	      }
	    }

	    ccc += WriteOut (SKTfp, "{ ");
	    WriteEntryValue (SKTfp, dataType, numElements, value, ccc,
			     MAX_COL_TO_USE - 4);
						/* -4 for possible " } ." */
	    ccc += WriteOut (SKTfp, " }");

	    cdf_FreeMemory (value, FatalError);	    
	  }
	  CHECKforABORTso
       }
       /***********************************************************************
       * Write '.' after last entry.
       ***********************************************************************/
       WriteOut (SKTfp, " .");
       if (!ISTPname(attrName)) {
         WriteOut (SKTfp, "\n");
         WriteOutFP (SKTfp,
                     "!  Warning: \"%s\": Not ISTP compliant... not recommended",
                     attrName);
       }
     }
     CHECKforABORTso
  }
  if (GattrCount == 0) WriteOut (SKTfp, "\n\n! No global scope attributes.");
}
else
  WriteOut (SKTfp, "\n\n! No global scope attributes.");

return TRUE;
}


/******************************************************************************
* WriteVarAttr.
******************************************************************************/

Logical WriteVarAttr ()
{
CDFstatus status;
char delim,			     /* Delimeter for attribute name. */
     tempS[MAX_LINE_LEN+1];
long attrN, numAttrs, scope;
char attrName[CDF_ATTR_NAME_LEN256+1];
int VattrCount = 0;		     /* Number of variable scope attributes. */

WriteOut (SKTfp, "\n\n");
WriteOut (SKTfp, "\n#VARIABLEattributes");
WriteOut (SKTfp, "\n");

status = CDFlib (GET_, CDF_NUMATTRS_, &numAttrs,
		 NULL_);
if (!StatusHandlerC2S(status)) return FALSE;

if (numAttrs > 0) {
  for (attrN = 0; attrN < numAttrs; attrN++) {
     status = CDFlib (SELECT_, ATTR_, attrN,
		      GET_, ATTR_NAME_, attrName,
			    ATTR_SCOPE_, &scope,
		      NULL_);
     if (!StatusHandlerC2S(status)) return FALSE;
     if (scope == VARIABLE_SCOPE) {
       /***********************************************************************
       * Variable scope attribute...
       ***********************************************************************/
       VattrCount++;
       delim = PickDelimiter (attrName, strlen(attrName));
       snprintf (tempS, (size_t) sizeof(tempS),
		 "\n  %c%s%c", delim, attrName, delim);
       WriteOut (SKTfp, tempS);
       if (!ISTPname(attrName)) {
         WriteOut (SKTfp, "\n");
         WriteOutFP (SKTfp,
                     "!  Warning: \"%s\": Not ISTP compliant... not recommended",
                     attrName);
       }
     }
     CHECKforABORTso
  }
  if (VattrCount == 0) WriteOut (SKTfp, "\n! No variable scope attributes.");
}
else
  WriteOut (SKTfp, "\n! No variable scope attributes.");

return TRUE;
}


/******************************************************************************
* WriteVars.
******************************************************************************/

Logical WriteVars (varValues, variables)
int varValues;
char *variables;
{
CDFstatus status;
long numVars;
long numZvars;
long varN;

status = CDFlib (GET_, CDF_NUMrVARS_, &numVars,
		       CDF_NUMzVARS_, &numZvars,
		 NULL_);
if (!StatusHandlerC2S(status)) return FALSE;

WriteOut (SKTfp, "\n\n");
WriteOut (SKTfp, "\n#variables");

if (numVars == 0)
  WriteOut (SKTfp, "\n\n! No rVariables.");
else
  for (varN = 0; varN < numVars; varN++) {
     if (!WriteVar(varN,varValues,variables,FALSE)) return FALSE;
  }

WriteOut (SKTfp, "\n\n");
WriteOut (SKTfp, "\n#zVariables");

if (numZvars == 0)
  WriteOut (SKTfp, "\n\n! No zVariables.");
else
  for (varN = 0; varN < numZvars; varN++) {
     if (!WriteVar(varN,varValues,variables,TRUE)) return FALSE;
  }

return TRUE;
}

/******************************************************************************
* WriteVar.
******************************************************************************/

Logical WriteVar (varN, varValues, variables, zVar)
long varN;
int varValues;
char *variables;
Logical zVar;
{
CDFstatus status;
long numDims, dimSizes[CDF_MAX_DIMS];
long numAttrs;
long maxRec;
long varDataType, varNumElements;
long entryDataType, entryNumElements;
long recVary, dimVarys[CDF_MAX_DIMS];
char varName[CDF_VAR_NAME_LEN256+1];
char attrName[CDF_ATTR_NAME_LEN256+1];
char delim;
long attrN;
long scope;
int dimN;
int ccc;			      /* Current Cursor Column (base is 0). */
int ddd;
void *value;
int VattrCount;
char varys[MAX_VARYS_LEN+1];
char sizes[MAX_SIZES_LEN+1];
char tempS[MAX_LINE_LEN+1];
long cType, cParms[CDF_MAX_DIMS], cPct, sparse, bf;
void *pad = NULL;
int padValue;
long sarray[CDF_MAX_PARMS];

sarray[0] = 0;
status = CDFlib (SELECT_, VAR(zVar), varN,
		 GET_, VAR_NAME(zVar), varName,
		       VAR_DATATYPE(zVar), &varDataType,
		       VAR_NUMELEMS(zVar), &varNumElements,
		       VAR_RECVARY(zVar), &recVary,
		       VAR_DIMVARYS(zVar), dimVarys,
		       VAR_MAXREC(zVar), &maxRec,
		       VAR_COMPRESSION(zVar), &cType, cParms, &cPct,
		       VAR_SPARSERECORDS(zVar), &sparse,
		       VAR_BLOCKINGFACTOR(zVar), &bf,
		 NULL_);
if (!StatusHandlerC2S(status)) return FALSE;

if (zVar)
  status = CDFlib (SELECT_, zVAR_, varN,
		   GET_, zVAR_NUMDIMS_, &numDims,
			 zVAR_DIMSIZES_, dimSizes,
		   NULL_);
else
  status = CDFlib (GET_, rVARs_NUMDIMS_, &numDims,
			 rVARs_DIMSIZES_, dimSizes,
		   NULL_);
if (!StatusHandlerC2S(status)) return FALSE;

pad = cdf_AllocateMemory ((size_t) ((varNumElements + 
                                     (STRINGdataType(varDataType)? 1 : 0)) *
                                    CDFelemSize(varDataType)),
                          FatalError);
status = CDFlib (GET_, VAR_PADVALUE(zVar), pad,
                 NULL_);
if (!StatusHandlerC2S(status)) return FALSE;
if (status != NO_PADVALUE_SPECIFIED)
  padValue = 1;
else
  padValue = 0;

/***************************************************************************
* If zVariable, build dimension sizes string.
***************************************************************************/

if (numDims > 0) {
  sizes[0] = NUL;
  for (dimN = 0; dimN < numDims; dimN++) {
     snprintf (&sizes[strlen(sizes)], (size_t) sizeof(sizes)-strlen(sizes),
	       "%ld ", dimSizes[dimN]);
  }
  sizes[strlen(sizes)-1] = NUL;			/* Wipe out trailing blank. */
}
else
  strcpyX (sizes, "", MAX_SIZES_LEN);

/***************************************************************************
* Build dimension variance string.
***************************************************************************/

if (numDims > 0) {
  strcpyX (varys, "", MAX_VARYS_LEN);
  for (dimN = 0; dimN < numDims; dimN++) {
     strcatX (varys, TFvarianceToken(dimVarys[dimN]), MAX_VARYS_LEN);
     strcatX (varys, " ", MAX_VARYS_LEN);
  }
  varys[strlen(varys)-1] = NUL;		/* Wipe out trailing blank. */
}
else
  strcpyX (varys, "", MAX_VARYS_LEN);

/***************************************************************************
* Write headings.
***************************************************************************/

if (varN != 0) WriteOut (SKTfp, "\n");		/* Skip 2 lines after first
						   variable. */
WriteOut (SKTfp, "\n\n");
ccc = WriteOut (SKTfp, "! Variable");
Ncharacters (SKTfp, VAR_DATATYPE_COL - ccc, (int) ' ');
WriteOut (SKTfp, "  Data      Number   ");

if (zVar) {
  Ncharacters (SKTfp, 4, (int) ' ');
  Ncharacters (SKTfp, 2, (int) ' ');
  Ncharacters (SKTfp, MaxInt((int)strlen(sizes),
			     (int)strlen("Sizes")), (int) ' ');
  Ncharacters (SKTfp, 2, (int) ' ');
}

WriteOut (SKTfp, " Record   ");
Ncharacters (SKTfp, FRONTpadLABEL("Dimension",varys), (int) ' ');
WriteOut (SKTfp, "Dimension");

WriteOut (SKTfp, "\n");
ccc = WriteOut (SKTfp, "! Name     ");
Ncharacters (SKTfp, VAR_DATATYPE_COL - ccc, (int) ' ');
WriteOut (SKTfp, "  Type     Elements  ");

if (zVar) {
  WriteOut (SKTfp, "Dims");
  Ncharacters (SKTfp, 2, (int) ' ');
  Ncharacters (SKTfp, FRONTpadLABEL("Sizes",sizes), (int) ' ');
  WriteOut (SKTfp, "Sizes");
  Ncharacters (SKTfp, BACKpadLABEL("Sizes",sizes), (int) ' ');
  Ncharacters (SKTfp, 2, (int) ' ');
}

WriteOut (SKTfp, "Variance  ");

Ncharacters (SKTfp, FRONTpadLABEL("Variances",varys), (int) ' ');
WriteOut (SKTfp, "Variances");

WriteOut (SKTfp, "\n");
ccc = WriteOut (SKTfp, "! --------");
Ncharacters (SKTfp, VAR_DATATYPE_COL - ccc, (int) ' ');
WriteOut (SKTfp, "  ----     --------  ");

if (zVar) {
  Ncharacters (SKTfp, 4, (int) '-');
  Ncharacters (SKTfp, 2, (int) ' ');
  Ncharacters (SKTfp, MaxInt((int)strlen("Sizes"),
			     (int)strlen(sizes)), (int) '-');
  Ncharacters (SKTfp, 2, (int) ' ');
}

WriteOut (SKTfp, "--------  ");

Ncharacters (SKTfp, MaxInt((int)strlen("Variances"),
			   (int)strlen(varys)), (int) '-');

/***************************************************************************
* Write variable definition line.
***************************************************************************/

WriteOut (SKTfp, "\n");

delim = PickDelimiter (varName, strlen(varName));
WriteOut (SKTfp, "\n");
snprintf (tempS, (size_t) sizeof(tempS), "  %c%s%c", delim, varName, delim);
ccc = WriteOut (SKTfp, tempS);

if (varDataType != CDF_TIME_TT2000) ddd = 0;
else ddd = 4;

if (ccc > (VAR_DATATYPE_COL - 3 - ddd)) {
  WriteOut (SKTfp, "\n");
  ccc = 0;
}

Ncharacters (SKTfp, VAR_DATATYPE_COL - ccc - ddd, (int) ' ');

snprintf (tempS, (size_t) sizeof(tempS),
	  "%s %*ld   ", DataTypePaddedString(varDataType),
	  VAR_NUMELEMS_WIDTH, varNumElements);
WriteOut (SKTfp, tempS);

Ncharacters (SKTfp, 2, (int) ' ');

if (zVar) {
  snprintf (tempS, (size_t) sizeof(tempS),
	    " %*ld ", zVAR_NUMDIMS_WIDTH, numDims);
  WriteOut (SKTfp, tempS);
  Ncharacters (SKTfp, 2, (int) ' ');
  Ncharacters (SKTfp, FRONTpadVALUE("Sizes",sizes), (int) ' ');
  WriteOut (SKTfp, sizes);
  Ncharacters (SKTfp, BACKpadVALUE("Sizes",sizes), (int) ' ');
  Ncharacters (SKTfp, 2, (int) ' ');
}

WriteOut (SKTfp, "    ");
WriteOut (SKTfp, TFvarianceToken(recVary));

if (numDims > 0) {
  Ncharacters (SKTfp, 3, (int) ' ');
  Ncharacters (SKTfp, 2, (int) ' ');
  Ncharacters (SKTfp, FRONTpadVALUE("Variances",varys), (int) ' ');
  WriteOut (SKTfp, varys);
}

WriteOutFP (SKTfp, "\n");

if (!ISTPname(varName)) {
  WriteOutFP (SKTfp, "!  Warning: \"%s\": Not ISTP compliant... not recommended",
              varName);
}

/* if (cType != NO_COMPRESSION) */
  WriteOutFP (SKTfp, "\n  ! VAR_COMPRESSION: %s",
              CompressionToken(cType, cParms));
  WriteOutFP (SKTfp, "\n  ! (Valid compression: None, GZIP.1-9, RLE.0, HUFF.0, AHUFF.0)");

/* if (recVary == VARY) { */
  WriteOutFP (SKTfp, "\n  ! VAR_SPARSERECORDS: %s",
            SparsenessToken(sparse, NO_SPARSEARRAYS, sarray));
  WriteOutFP (SKTfp, "\n  ! (Valid sparserecords: None, sRecords.PAD, sRecords.PREV)");
/* } */

if (bf != 0) {
  WriteOutFP (SKTfp, "\n  ! VAR_BLOCKINGFACTOR: %ld", bf);
}

if (STRINGdataType(varDataType)) {
  int ix, iy;
  if (padValue == 0) { /* Only output a single blank */
    *(((char *) pad)+0) = (char) DEFAULT_CHAR_PADVALUE;
    *(((char *) pad)+1) = (char) '\0';
  } else {
    long numBytes = (long) UTF8StrLength ((unsigned char *)pad);
    iy = -1;
    if (numBytes == varNumElements) {
      for (ix = 0; ix < (int) varNumElements; ++ix)
        if (!Printable(*(((char *) pad)+ix))) *(((char *) pad)+ix) = '.';
    }
    for (ix = (varNumElements-1); ix >= 0; --ix) {
      if (*(((char *) pad)+ix) != ' ') {
        iy = ix;
        break;
      }
    }
    if (iy == -1) /* All blanks... output a single blank */
      *(((char *) pad)+1) = (char) '\0';
    else
      *(((char *) pad)+iy+1) = (char) '\0';
  }
  if (strchr(pad, '"') == NULL) 
    WriteOutFP (SKTfp, "\n  ! VAR_PADVALUE: \"%s\"", pad);
  else if (strchr(pad, '\'') == NULL)
    WriteOutFP (SKTfp, "\n  ! VAR_PADVALUE: '%s'", pad);
  else
    WriteOutFP (SKTfp, "\n  ! VAR_PADVALUE: $%s$", pad);
} else {
  char string[101], *format;
  int  style;
  if (TT2000dataType(varDataType)) style = TT2000_3_STYLE;
  else style = EPOCH0_STYLE;
  status = GetFormatEntry (zVar, varN, &format);
  if (padValue == 1 && useFormat && format != NULL)
    EncodeValue (varDataType, pad, string, style, 100);
  else
    EncodeValue (varDataType, pad, string, style, 100);
    WriteOutFP (SKTfp, "\n  ! VAR_PADVALUE: %s", string);
  if (format != NULL) cdf_FreeMemory (format, FatalError);
}
cdf_FreeMemory (pad, FatalError);

CHECKforABORTso

/***************************************************************************
* Write corresponding attribute entries.
***************************************************************************/

status = CDFlib (GET_, CDF_NUMATTRS_, &numAttrs,
		 NULL_);
if (!StatusHandlerC2S(status)) return FALSE;

VattrCount = 0;

WriteOut (SKTfp, "\n");

for (attrN = 0; attrN < numAttrs; attrN++) {
   status = CDFlib (SELECT_, ATTR_, attrN,
		    GET_, ATTR_SCOPE_, &scope,
			  ATTR_NAME_, attrName,
		    NULL_);
   if (!StatusHandlerC2S(status)) return FALSE;

   if (scope == VARIABLE_SCOPE) {
     status = CDFlib (SELECT_, BOO(zVar,zENTRY_,rENTRY_), varN,
		      GET_, BOO(zVar,zENTRY_DATATYPE_,
				     rENTRY_DATATYPE_), &entryDataType,
			    BOO(zVar,zENTRY_NUMELEMS_,
				     rENTRY_NUMELEMS_), &entryNumElements,
		      NULL_);
     if (status != NO_SUCH_ENTRY) {
       if (!StatusHandlerC2S(status)) return FALSE;
       VattrCount++;

       if (VattrCount == 1) {
	 WriteOut (SKTfp, "\n");
	 ccc = WriteOut (SKTfp, "  ! Attribute");
	 Ncharacters (SKTfp, ENTRY_DATATYPE_COL - ccc, (int) ' ');
	 ccc += ENTRY_DATATYPE_COL - ccc;
	 ccc += WriteOut (SKTfp, "  Data");

	 WriteOut (SKTfp, "\n");
	 ccc = WriteOut (SKTfp, "  ! Name     ");
	 Ncharacters (SKTfp, ENTRY_DATATYPE_COL - ccc, (int) ' ');
	 ccc += ENTRY_DATATYPE_COL - ccc;
	 ccc += WriteOut (SKTfp, "  Type       Value");

	 WriteOut (SKTfp, "\n");
	 ccc = WriteOut (SKTfp, "  ! --------");
	 Ncharacters (SKTfp, ENTRY_DATATYPE_COL - ccc, (int) ' ');
	 ccc += ENTRY_DATATYPE_COL - ccc;
	 ccc += WriteOut (SKTfp, "  ----       -----");

	 WriteOut (SKTfp, "\n");
       }

       value = cdf_AllocateMemory ((size_t) (entryNumElements *
				         CDFelemSize(entryDataType)),
			       FatalError);

       status = CDFlib (GET_, BOO(zVar,zENTRY_DATA_,rENTRY_DATA_), value,
		        NULL_);
       if (!StatusHandlerC2S(status)) return FALSE;

       delim = PickDelimiter (attrName, strlen(attrName));
       WriteOut (SKTfp, "\n");
       snprintf (tempS, (size_t) sizeof(tempS),
		 "    %c%s%c", delim, attrName, delim);
       ccc = WriteOut (SKTfp, tempS);

       if (entryDataType != CDF_TIME_TT2000) ddd = 0;
       else ddd = 4;

       if (ccc > (ENTRY_DATATYPE_COL - 2 - ddd)) {
	 WriteOut (SKTfp, "\n");
	 ccc = 0;
       }

       Ncharacters (SKTfp, ENTRY_DATATYPE_COL - ccc - ddd, (int) ' ');
       ccc += ENTRY_DATATYPE_COL - ccc;

       ccc += WriteOut (SKTfp, DataTypePaddedString(entryDataType));
       ccc += WriteOut (SKTfp, "   ");

       if (STRINGdataType (entryDataType)) { /* not allow LF and CR */
         int ii;
         for (ii = 0; ii < (int) entryNumElements; ++ii) {
           if (((int)*(((char *)value)+ii) == (int) '\n') ||
               ((int)*(((char *)value)+ii) == (int) '\r'))
             memcpy (((char *)value)+ii, " ", 1);
         }
       }

       ccc += WriteOut (SKTfp, "{ ");
       WriteEntryValue (SKTfp, entryDataType, entryNumElements,
			value, ccc, MAX_COL_TO_USE - 4);
						/* -4 for possible " } ." */
       ccc += WriteOut (SKTfp, " }");

       cdf_FreeMemory (value, FatalError);
     }
   }
   CHECKforABORTso
}

if (VattrCount == 0) {
  WriteOut (SKTfp,
	   "\n  ! No variable scope attribute entries for this variable.");
}

/***************************************************************************
* Write terminating period.
***************************************************************************/

if (VattrCount == 0) {
  WriteOut (SKTfp, "\n\n  .");
  WriteOut (SKTfp,
	    "                         ! Terminating period required.");
}
else
  WriteOut (SKTfp, " .");

/***************************************************************************
* Write data values (if requested).
***************************************************************************/

switch (varValues) {
  case NOvalues:
    WriteOut (SKTfp, "\n\n  ! Values were not requested.");
    break;
  case NRVvalues:
    if (recVary)
      WriteOut (SKTfp, "\n\n  ! RV values were not requested.");
    else
      if (maxRec == 0) {
	WriteOut (SKTfp, "\n\n  ! NRV values follow...\n");
	if (!WriteVariableData(SKTfp,zVar,varN,varDataType,
			       varNumElements,numDims,dimSizes,
			       recVary,dimVarys,maxRec)) return FALSE;
      }
      else
        WriteOut (SKTfp, "\n\n  ! No values (no records for this variable).");
    break;
  case RVvalues:
    if (!recVary)
      WriteOut (SKTfp, "\n\n  ! NRV values were not requested.");
    else
      if (maxRec > -1) {
	WriteOut (SKTfp, "\n\n  ! RV values follow...\n");
	if (!WriteVariableData(SKTfp,zVar,varN,varDataType,
			       varNumElements,numDims,dimSizes,
			       recVary,dimVarys,maxRec)) return FALSE;
      }
      else
        WriteOut (SKTfp, "\n\n  ! No values (no records for this variable).");
    break;
  case ALLvalues:
    if (maxRec > -1) {
      WriteOut (SKTfp, "\n\n  ! Values follow...\n");
      if (!WriteVariableData(SKTfp,zVar,varN,varDataType,
			     varNumElements,numDims,dimSizes,
			     recVary,dimVarys,maxRec)) return FALSE;
    }
    else
      WriteOut (SKTfp, "\n\n  ! No values (no records for this variable).");
    break;
  case NAMEDvalues:
    if (VariableSelected(varName,variables))
      if (maxRec > -1) {
        WriteOut (SKTfp, "\n\n  ! Values follow...\n");
	if (!WriteVariableData(SKTfp,zVar,varN,varDataType,
			       varNumElements,numDims,dimSizes,
			       recVary,dimVarys,maxRec)) return FALSE;
      }
      else
        WriteOut (SKTfp, "\n\n  ! No values (no records for this variable).");
    else
      WriteOut (SKTfp, "\n\n  ! Values were not requested.");
    break;
}

return TRUE;
}

/******************************************************************************
* WriteEnd.
******************************************************************************/

Logical WriteEnd ()
{
  WriteOut (SKTfp, "\n\n");
  WriteOut (SKTfp, "\n#end");
  return TRUE;
}

/******************************************************************************
* WriteVariableData.
*     It is assumed that there is at least one record in the CDF.
******************************************************************************/

Logical WriteVariableData (fp, zVar, varN, dataType, numElements, numDims,
			   dimSizes, recVary, dimVarys, maxRec)
FILE *fp;
Logical zVar;
long varN;
long dataType;
long numElements;
long numDims;
long dimSizes[];
long recVary;
long dimVarys[];
long maxRec;
{
  CDFstatus status;
  long indices[CDF_MAX_DIMS];
  long counts[CDF_MAX_DIMS];
  void *value;
  long majority;
  int ccc;
  long nRecValues, i, recN;
  int dimN;
  char Evalue[80+1], *format, tempS[MAX_LINE_LEN+1];
  int style;
  if (TT2000dataType(dataType)) style = TT2000_3_STYLE;
  else style = EPOCH0_STYLE;
  status = CDFlib (SELECT_, VAR(zVar), varN,
		   GET_, CDF_MAJORITY_, &majority,
		   NULL_);
  if (!StatusHandlerC2S(status)) return FALSE;
  nRecValues = 1;
  if (numDims > 0) {
    for (dimN = 0; dimN < numDims; dimN++) {
       indices[dimN] = 0;
       if (dimVarys[dimN]) {
         nRecValues *= dimSizes[dimN];
         counts[dimN] = dimSizes[dimN];
       }
       else
         counts[dimN] = 1;
    }
  }
  value = cdf_AllocateMemory ((size_t) (numElements * CDFelemSize(dataType)),
			      FatalError);
  if (useFormat) {
    status = GetFormatEntry (zVar, varN, &format);
    if (!StatusHandlerC2S(status)) {
      cdf_FreeMemory (value, FatalError);
      return FALSE;
    }
  }
  else
    format = NULL;
  for (recN = 0; recN <= maxRec; recN++) {
     status = CDFlib (SELECT_, BOO(zVar,zVAR_RECNUMBER_,
					rVARs_RECNUMBER_), recN,
		      NULL_);
     if (!StatusHandlerC2S(status)) {
       cdf_FreeMemory (value, FatalError);
       if (format != NULL) cdf_FreeMemory (format, FatalError);
       return FALSE;
     }
     if (status == VIRTUAL_RECORD_DATA) continue;
     for (i = 0; i < nRecValues; i++) {
        status = CDFlib (SELECT_, BOO(zVar,zVAR_DIMINDICES_,
				           rVARs_DIMINDICES_), indices,
		         GET_, VAR_DATA(zVar), value,
		         NULL_);
        if (!StatusHandlerC2S(status)) {
	  cdf_FreeMemory (value, FatalError);
	  if (format != NULL) cdf_FreeMemory (format, FatalError);
	  return FALSE;
	}
        if (status == VIRTUAL_RECORD_DATA) break;
        ccc = WriteOut (fp, "\n    ");
	if (recVary) {
	  snprintf (tempS, (size_t) sizeof(tempS), "%ld:[", recN + 1);	  
	}
	else
	  strcpyX (tempS, "[", MAX_LINE_LEN);
        if (numDims > 0) {
          for (dimN = 0; dimN < numDims; dimN++) {
             snprintf (EofS(tempS), (size_t) sizeof(tempS)-strlen(tempS),
		       "%ld", indices[dimN] + 1);
	     if (dimN != numDims-1) strcatX (tempS, ",", MAX_LINE_LEN);
          }
        }
        strcatX (tempS, "] = ",MAX_LINE_LEN);
	ccc += WriteOut (SKTfp, tempS);
        if (STRINGdataType(dataType)) {
          ccc += WriteOut (fp, "{ ");
          WriteStringValue (fp, numElements, value, ccc,
		            MAX_COL_TO_USE - 2);    /* -2 for " }" */
          WriteOut (fp, " }");
        }
        else {
          if (useFormat && format != NULL) {
            if ((dataType == CDF_FLOAT) || (dataType == CDF_REAL4)) {
              if (!isnan((double)*(float *)value) &&
                  *(float *)value <= DEFAULT_FLOAT_PADVALUE) {
                EncodeValueFormat (dataType, value, Evalue, NULL, 0, 80,
	 	                   style, (size_t) sizeof(Evalue));
              } else 
                EncodeValue (dataType, value, Evalue, style,
                             (size_t) sizeof(Evalue));
            } else if ((dataType == CDF_DOUBLE) || (dataType == CDF_REAL8)) {
              if (!isnan(*(double *)value) &&
                  *(double *)value <= DEFAULT_DOUBLE_PADVALUE) {
                EncodeValueFormat (dataType, value, Evalue, NULL, 0, 80,
	 	                   style, (size_t) sizeof(Evalue));
              } else
                EncodeValue (dataType, value, Evalue, style,
                             (size_t) sizeof(Evalue));
            } else
              EncodeValueFormat (dataType, value, Evalue, format, 0, 80,
	 	               style, (size_t) sizeof(Evalue));
          } else
            EncodeValue (dataType, value, Evalue, style,
                         (size_t) sizeof(Evalue));
						/* Don't care about width. */
          WriteOut (fp, Evalue);
        }
        if (ROWmajor(majority))
          INCRindicesROW (numDims, counts, indices);
        else
          INCRindicesCOL (numDims, counts, indices);
	CHECKforABORTso
     }
  }
  cdf_FreeMemory (value, FatalError);
  if (format != NULL) cdf_FreeMemory (format, FatalError);
  return TRUE;
}

/******************************************************************************
* VariableSelected.
*      The assumed format of the list of delimited variable names is...
* <delim><chars><delim>,<delim><chars><delim>,...,<delim><chars><delim>
******************************************************************************/

Logical VariableSelected (varName, variables)
char *varName;
char *variables;
{
/*
  char *d1, *d2, varNameT[CDF_VAR_NAME_LEN256+1];
  size_t len;
  d1 = variables;
  for (;;) {
    while (*d1 != NUL && Spacing(*d1)) d1++;
    if (*d1 == NUL) return FALSE;
    d2 = d1 + 1;
    while (*d2 != NUL && *d2 != *d1) d2++;
    if (*d2 == NUL) return FALSE;
    len = (size_t) (d2 - d1 - 1);
    strcpyX (varNameT, d1 + 1, MINIMUM(len,CDF_VAR_NAME_LEN256));
    if (!strcmpITB(varName,varNameT)) return TRUE;
    d1 = d2 + 1;
    while (*d1 != NUL && *d1 != ',') d1++;
    if (*d1 == NUL) return FALSE;
    d1++;
  }
*/
  if (variables == NULL) return FALSE;
  if (strstr((const char *)variables, (const char *)varName) != NULL)
    return TRUE;
  else
    return FALSE;
}

/******************************************************************************
* DataTypePaddedString.
*    Strings are blank-padded on the end to all be the same length.
******************************************************************************/

char *DataTypePaddedString (dataType)
long dataType;
{
switch (dataType) {
  case CDF_BYTE:   return "CDF_BYTE  ";
  case CDF_INT1:   return "CDF_INT1  ";
  case CDF_INT2:   return "CDF_INT2  ";
  case CDF_INT4:   return "CDF_INT4  ";
  case CDF_INT8:   return "CDF_INT8  ";
  case CDF_UINT1:  return "CDF_UINT1 ";
  case CDF_UINT2:  return "CDF_UINT2 ";
  case CDF_UINT4:  return "CDF_UINT4 ";
  case CDF_REAL4:  return "CDF_REAL4 ";
  case CDF_REAL8:  return "CDF_REAL8 ";
  case CDF_FLOAT:  return "CDF_FLOAT ";
  case CDF_DOUBLE: return "CDF_DOUBLE";
  case CDF_EPOCH:  return "CDF_EPOCH ";
  case CDF_CHAR:   return "CDF_CHAR  ";
  case CDF_UCHAR:  return "CDF_UCHAR ";
  case CDF_EPOCH16:  return "CDF_EPOCH16";
  case CDF_TIME_TT2000:  return "CDF_TIME_TT2000";
}
return "??????????";
}

/******************************************************************************
* StatusHandlerC2S.
******************************************************************************/

Logical StatusHandlerC2S (status)
CDFstatus status;
{
  char text[CDF_STATUSTEXT_LEN + 1];

  if (StatusERROR(status)) {
    if (report[ERRORs]) {
      CDFlib (SELECT_, CDF_STATUS_, status,
	      GET_, STATUS_TEXT_, text,
	      NULL_);
      WriteOut (stdout, "\nERROR> ");
      WriteOut (stdout, text);
      WriteOut (stdout, "\n");
    }
    CDFlib (CLOSE_, CDF_,
	    NULL_);
    WriteOut (stdout, "**** THE SKELETON TABLE IS INCOMPLETE ****\n");
    if (SKTfp != stdout) {
      WriteOut (SKTfp, "\n");
      WriteOut (SKTfp, "\n**** THIS SKELETON TABLE IS NOT COMPLETE ****");
      WriteOut (SKTfp, "\n");
      fclose (SKTfp);
    }
    return FALSE;
  }

  if (StatusWARN(status) && report[WARNs]) {
    CDFlib (SELECT_, CDF_STATUS_, status,
	    GET_, STATUS_TEXT_, text,
	    NULL_);
    WriteOut (stdout, "\nWARNING> ");
    WriteOut (stdout, text);
    WriteOut (stdout, "\n");
    return TRUE;
  }

  if (StatusINFO(status) && report[INFOs]) {
    CDFlib (SELECT_, CDF_STATUS_, status,
	    GET_, STATUS_TEXT_, text,
	    NULL_);
    WriteOut (stdout, "\nINFO> ");
    WriteOut (stdout, text);
    WriteOut (stdout, "\n");
    return TRUE;
  }

  return TRUE;		/* CDF_OK */
}

/******************************************************************************
* WriteTT2000VarData.
******************************************************************************/

Logical WriteTT2000VarData (varValues, variables)
int varValues;
char *variables;
{
CDFstatus status;
long numVars, numZvars;
long varN, recVary, varDataType, maxRec;
char varName[CDF_VAR_NAME_LEN256+1];

if (varValues == NOvalues) return FALSE;
status = CDFlib (GET_, CDF_NUMrVARS_, &numVars,
		       CDF_NUMzVARS_, &numZvars,
		 NULL_);
if (!StatusHandlerC2S(status)) return FALSE;
if (numVars > 0) {
  for (varN = 0; varN < numVars; varN++) {
    status = CDFlib (SELECT_, rVAR_, varN,
                     GET_, rVAR_NAME_, varName,
                           rVAR_DATATYPE_, &varDataType,
                           rVAR_RECVARY_, &recVary,
                           rVAR_MAXREC_, &maxRec,
                     NULL_);
    if (!StatusHandlerC2S(status)) return FALSE;
    if (varDataType == CDF_TIME_TT2000) {
      switch (varValues) {
        case NRVvalues:
          if (!recVary && maxRec == 0) return TRUE;
          break;
        case RVvalues:
          if (!recVary && maxRec > -1) return TRUE;
          break;
        case ALLvalues:
          if (maxRec > -1) return TRUE;
          break;
        case NAMEDvalues:
          if (VariableSelected(varName,variables) && maxRec > -1) return TRUE;
          break;
        default:
          break; 
      }
    }
  }
}
if (numZvars > 0) {
  for (varN = 0; varN < numZvars; varN++) {
    status = CDFlib (SELECT_, zVAR_, varN,
                     GET_, zVAR_NAME_, varName,
                           zVAR_DATATYPE_, &varDataType,
                           zVAR_RECVARY_, &recVary,
                           zVAR_MAXREC_, &maxRec,
                     NULL_);
    if (!StatusHandlerC2S(status)) return FALSE;
    if (varDataType == CDF_TIME_TT2000) {
      switch (varValues) {
        case NRVvalues:
          if (!recVary && maxRec == 0) return TRUE;
          break;
        case RVvalues:
          if (!recVary && maxRec > -1) return TRUE;
          break;
        case ALLvalues:
          if (maxRec > -1) return TRUE;
          break;
        case NAMEDvalues:
          if (VariableSelected(varName,variables) && maxRec > -1) return TRUE;
          break;
        default:
          break; 
      }
    }
  }
}
return FALSE;
}

/******************************************************************************
* SkeletonTableQOPs.
*    Returns TRUE if execution should continue.
******************************************************************************/

#if defined(mac)
Logical SkeletonTableQOPs (argC, argV)
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
  short itemN, iType, i;
  static Logical first = TRUE;
  char cancelTitle[] = "Cancel";
  static Logical outToScreen = DEFAULTscreenCDF2SKT;
  static Logical useFormat = DEFAULTformatCDF2SKT;
  static Logical negToPos = DEFAULT_NEGtoPOSfp0;
  static Logical pageOutput = DEFAULTpageCDF2SKT;
  static Logical reportInfos = REPORTinfosDEFAULT;
  static Logical reportWarns = REPORTwarningsDEFAULT;
  static Logical reportErrors = REPORTerrorsDEFAULT;
  static Logical logMsg = DEFAULTlogCDF2SKT;
  static Logical dispStats = DEFAULTstatsEDIT;
  static int zModeSrc = DEFAULTzModeCDF2SKT;
  static int varValues = DEFAULTvaluesCDF2SKT;
  static Str255 CDFtext = "\p";
  static Str255 sktText = "\p";
  static Str255 cacheText = "\p";
  static Str255 varsText = "\p";
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
  SetIText ((Handle) controlHs[VARSin], varsText);
  if (outToScreen) SetCtlValue (controlHs[SCREENin], 1);
  if (useFormat) SetCtlValue (controlHs[FORMATin], 1);
  if (negToPos) SetCtlValue (controlHs[NEGZin], 1);
  if (pageOutput) SetCtlValue (controlHs[PAGEin], 1);
  if (reportInfos) SetCtlValue (controlHs[INFOin], 1);
  if (reportWarns) SetCtlValue (controlHs[WARNin], 1);
  if (reportErrors) SetCtlValue (controlHs[ERRORin], 1);
  if (dispStats) SetCtlValue (controlHs[STATSin], 1);
  if (logMsg) SetCtlValue (controlHs[LOGin], 1);
  SetCtlValue (controlHs[ZMODEinBASE+zModeSrc], 1);
  SetCtlValue (controlHs[VALUESinBASE+varValues], 1);

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
	char *valuesOptions[5] = { "off", "nrv", "rv", "all", NULL };
	int n;
	char tempS1[1+1];
	/**********************************************************************
	* Get the value of each control.
	**********************************************************************/
	GetIText ((Handle) controlHs[CDFTEXTin], CDFtext);
	GetIText ((Handle) controlHs[SKTTEXTin], sktText);
	GetIText ((Handle) controlHs[CACHEin], cacheText);
	GetIText ((Handle) controlHs[VARSin], varsText);
	outToScreen = GetCtlValue (controlHs[SCREENin]);
	useFormat = GetCtlValue (controlHs[FORMATin]);
	negToPos = GetCtlValue (controlHs[NEGZin]);
	pageOutput = GetCtlValue (controlHs[PAGEin]);
	reportInfos = GetCtlValue (controlHs[INFOin]);
	reportWarns = GetCtlValue (controlHs[WARNin]);
	reportErrors = GetCtlValue (controlHs[ERRORin]);
	dispStats = GetCtlValue (controlHs[STATSin]);
	logMsg = GetCtlValue (controlHs[LOGin]);
	for (varValues = 0; varValues < 5; varValues++) {
	   if (GetCtlValue(controlHs[VALUESinBASE+varValues])) break;
	}
	for (zModeSrc = 0; zModeSrc < 3; zModeSrc++) {
	   if (GetCtlValue(controlHs[ZMODEinBASE+zModeSrc])) break;
	}
	/**********************************************************************
	* Build argc/argv.
	**********************************************************************/
	*argC = 13 + BOO(NULpString(CDFtext),0,1) +
		     	 BOO(NULpString(sktText),0,2) +
		     	 BOO(NULpString(cacheText),0,2);
    	*argV = (char **) cdf_AllocateMemory ((size_t)*argC * sizeof(char *),
						  FatalError);
	n = 0;
	MAKEstrARGv (argV, n, pgmName)
	MAKEbooARGv (argV, n, outToScreen, "-screen", "-noscreen")
	MAKEbooARGv (argV, n, useFormat, "-format", "-noformat")
	MAKEbooARGv (argV, n, negToPos, "-neg2posfp0", "-noneg2posfp0")
	MAKEbooARGv (argV, n, pageOutput, "-page", "-nopage")
	MAKEbooARGv (argV, n, dispStats, "-statistics", "-nostatistics")
	MAKEbooARGv (argV, n, logMsg, "-log", "-nolog")
	MAKEstrARGv (argV, n, "-zmode")
	snprintf (tempS1, (size_t) sizeof(tempS1), "%d", zModeSrc);
	MAKEstrARGv (argV, n, tempS1)
	MAKEstrARGv (argV, n, "-report")
	MAKEstrARGv (argV, n, StatusCodeReportOptions(reportErrors,
						      reportWarns,
						      reportInfos))
	MAKEstrARGv (argV, n, "-values")
	switch (varValues) {
	  case NAMEDvalues:
	    PtoCstr (varsText);
	    MAKEstrARGv (argV, n, (char *) varsText)
	    CtoPstr ((char *) varsText);
	    break;
	  default:
	    MAKEstrARGv (argV, n, valuesOptions[varValues])
	    break;
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
	if (!NULpString(CDFtext)) {
	  PtoCstr (CDFtext);
	  MAKEstrARGv (argV, n, (char *) CDFtext)
	  CtoPstr ((char *) CDFtext);
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
      * Select skeleton CDF.
      * The cursor is set because `StandardPutFile' leaves the cursor as an
      * iBeam (instead of returning it to what it was).
      ************************************************************************/
      case SKTSELECTin: {
	StandardFileReply sktReply;
	char sktPath[DU_MAX_PATH_LEN+1], prompt[] = "Enter skeleton table:";
	StandardPutFile (CtoPstr(prompt), CtoPstr(""), &sktReply);
	if (sktReply.sfGood && !sktReply.sfIsFolder && !sktReply.sfIsVolume) {
	  BuildMacPath (&sktReply.sfFile, sktPath, TRUE);
	  sktText[0] = strlen (sktPath);
	  strcpyX ((char *) &sktText[1], sktPath, 255);
	  SetIText ((Handle) controlHs[SKTTEXTin], sktText);
	}
	SetCursor (&(qd.arrow));
	break;
      }
      /************************************************************************
      * Check boxes.
      ************************************************************************/
      case SCREENin:
      case FORMATin:
      case NEGZin:
      case PAGEin:
      case INFOin:
      case WARNin:
      case ERRORin:
      case STATSin:
      case LOGin:
        SetCtlValue (controlHs[itemN], BOO(GetCtlValue(controlHs[itemN]),0,1));
        break;
      /************************************************************************
      * Radio buttons.
      ************************************************************************/
      case VALUESinBASE+0:
      case VALUESinBASE+1:
      case VALUESinBASE+2:
      case VALUESinBASE+3:
      case VALUESinBASE+4:
        for (i = 0; i < 5; i++) SetCtlValue (controlHs[VALUESinBASE+i], 0);
        SetCtlValue (controlHs[itemN], 1);
        break;
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
