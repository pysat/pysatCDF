/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                            CDFstats (statistics).
*
*  Version 2.5b, 18-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  10-Apr-91, J Love     Original version.
*   V1.1  25-Jun-91, J Love     CDF_EPOCH added as a data type.  Added QOP.
*                               Added PageInst.
*   V1.2   1-Aug-91, J Love     TRUE/FALSE.  Added range checking.  Added
*                               EPOCH display.  Use 'Exit'/'ExitBAD'.  Use
*                               'CDFlib'.  Added min/max checking within the
*                               valid range.  Added output option.
*   V2.0  18-Apr-92, J Love     Fixed online instructions.  Broke into smaller
*                               pieces for IBM-PC port.  Changes for IBM-RS6000
*                               port.  Added option to filter out fill values.
*                               Added option to use FORMAT attribute.
*   V2.1  30-Sep-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V2.2  25-Jan-94, J Love     CDF V2.4.
*   V2.3  30-Nov-94, J Love     CDF V2.5.
*   V2.3a 10-Jan-95, J Love	Uppercase file extensions on the Macintosh.
*   V2.3b  6-Apr-95, J Love	POSIX.
*   V2.4  27-Jul-95, J Love	Hyper groups.
*   V2.4a 28-Sep-95, J Love	Macintosh dialog filtering.  Outline default
*				button.
*   V2.5   8-Aug-96, J Love	CDF V2.6.
*   V2.5a 21-Feb-97, J Love	Removed RICE.
*   V2.5b 18-Nov-97, J Love	Windows NT/Visual C++.
*   V3.3  10-Jan-09, M Liu      Validate a file before it is used.
*
******************************************************************************/

#define CDFSTATS
#include "cdfstats.h"

/******************************************************************************
* Increased stack size for Borland C on IBM PC.
******************************************************************************/

#if defined(BORLANDC)
extern unsigned _stklen = BORLANDC_STACK_SIZE;
#endif

/******************************************************************************
* Global variables local to this source file.
******************************************************************************/

Logical report[3];

/******************************************************************************
* Main.
******************************************************************************/

#if !defined(win32) || (defined(win32) && defined(ALONE))
MAIN {
  Logical success = TRUE;
  strcpyX (pgmName, "CDFstats", MAX_PROGRAM_NAME_LEN);
#if defined(mac)
  MacExecuteSO (CalculateStatistics,StatisticsQOPs);
#else
  success = CalculateStatistics (argc, argv);
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
* CalculateStatistics.
******************************************************************************/

Logical CalculateStatistics (argC, argV)
int argC;
char *argV[];
{
  CDFstatus status; CDFid id;
  char oSpec[DU_MAX_PATH_LEN+1], oDir[DU_MAX_DIR_LEN+1];
  char oName[DU_MAX_NAME_LEN+1], CDFpath[DU_MAX_PATH_LEN+1];
  char CDFdir[DU_MAX_DIR_LEN+1], CDFname[DU_MAX_NAME_LEN+1];
  long numVars, numZvars, varN, zMode;
  long workingCache, stageCache, compressCache;
  Logical qopError = FALSE;
  struct VarStruct Var;
  Logical negToPosFp0, displayStats;
  QOP *qop;
  static char *validQuals[] = {
    "range", "norange", "output", "fill", "nofill", "format", "noformat",
    "page", "nopage", "update_valids", "noupdate_valids", "update_scales",
    "noupdate_scales", "zmode", "neg2posfp0", "noneg2posfp0",
    "update_monotonic", "noupdate_monotonic", "report", "cache", "about",
    "statistics", "nostatistics", "epoch_monotonic", NULL
  };
  static int optRequired[] = {
    FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, 0
  };
  static char *reportTokens[] = { "errors", "warnings", "informationals" };

/*  CDFsetValidate (VALIDATEFILEon); */
  /****************************************************************************
  * Get qualifiers/options/parameters.
  ****************************************************************************/
  switch (argC) {
    case 1:
      PageOLH ("cdfstats.olh", argV[0]);
      return TRUE;
    case 2:
      if (strcmp(argV[1],"-java") == 0) {
        pagingOn = FALSE;
        PageOLH ("cdfstatsj.olh", argV[0]);
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
      * Get CDF path parameter.
      ************************************************************************/
      if (qop->Nparms < 1) {
	DisplayError ("Missing parameters.");
	qopError = TRUE;
      }
      else
	strcpyX (CDFpath, qop->parms[CDFPATHparm], DU_MAX_PATH_LEN);

      /***********************************************************************
      * Check for `page' qualifier (this must be done before the `output'
      * qualifier is checked).
      ***********************************************************************/
      qopError = qopError | !TFqualifier(qop,&pagingOn,PAGEqual,NOPAGEqual,
					 DEFAULTpageSTATS,"page");
      /************************************************************************
      * Check for `output' qualifier.
      ************************************************************************/
      if (qop->qualEntered[OUTPUTqual]) {
	if (pagingOn) {
	  DisplayError ("Conflicting qualifiers (`output' and `page').");
	  qopError = TRUE;
	}
	strcpyX (oSpec, qop->qualOpt[OUTPUTqual], DU_MAX_PATH_LEN);
      }
      else
	strcpyX (oSpec, "", DU_MAX_PATH_LEN);
      /************************************************************************
      * Check for `range', `fill', `format', `update_valids', `update_scales',
      * `update_monotonic', `neg2posfp0', and `statistics' qualifiers.
      ************************************************************************/
      qopError = qopError | !TFqualifier(qop,&rangeCheck,RANGEqual,NORANGEqual,
					 DEFAULTrangeSTATS,"range");
      qopError = qopError | !TFqualifier(qop,&ignoreFills,FILLqual,NOFILLqual,
					 DEFAULTfillSTATS,"fill");
      qopError = qopError | !TFqualifier(qop,&useFormat,FORMATqual,
					 NOFORMATqual,DEFAULTfillSTATS,
					 "format");
      qopError = qopError | !TFqualifier(qop,&updateValids,UPDATE_VALIDSqual,
					 NOUPDATE_VALIDSqual,
					 DEFAULTupValidsSTATS,"update_valids");
      qopError = qopError | !TFqualifier(qop,&updateScales,UPDATE_SCALESqual,
					 NOUPDATE_SCALESqual,
					 DEFAULTupScalesSTATS,"update_scales");
      qopError = qopError | !TFqualifier(qop,&updateMonotonic,
					 UPDATE_MONOTONICqual,
					 NOUPDATE_MONOTONICqual,
					 DEFAULTupMonoSTATS,
					 "update_monotonic");
      qopError = qopError | !TFqualifier(qop,&negToPosFp0,NEG2POSFP0qual,
					 NONEG2POSFP0qual,DEFAULT_NEGtoPOSfp0,
					 "neg2posfp0");
      qopError = qopError | !TFqualifier (qop,&displayStats,STATSqual,
					  NOSTATSqual,DEFAULTstatsSTATS,
					  "statistics");
      /************************************************************************
      * Check for /ZMODE,-zmode qualifier.
      ************************************************************************/
      if (qop->qualEntered[ZMODEqual]) {
	if (!strcmp(qop->qualOpt[ZMODEqual],"0"))
	  zMode = zMODEoff;
	else if (!strcmp(qop->qualOpt[ZMODEqual],"1"))
	  zMode = zMODEon1;
	else if (!strcmp(qop->qualOpt[ZMODEqual],"2"))
	  zMode = zMODEon2;
	else {
	  DisplayError ("Illegal zMode.");
	  qopError = TRUE;
	}
      }
      else
	zMode = DEFAULTzModeSTATS;
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
      if (qop == NULL) return FALSE;
      /***********************************************************************
      * Check for `epoch_monotonic' qualifier.
      ***********************************************************************/
      if (qop->qualEntered[EPOCH_MONOTONICqual]) {
        epochMonotonic = TRUE;
      } else
        epochMonotonic = FALSE;
      /************************************************************************
      * Free QOP memory and check for an error.
      ************************************************************************/
      cdf_FreeMemory (qop, FatalError);
      if (qopError) return FALSE;
      break;
  }
  /****************************************************************************
  * Open output file (if specified).
  ****************************************************************************/
  if (NULstring(oSpec))
    OUTfp = stdout;
  else {
    ParsePath (oSpec, oDir, oName);
    if (strchr(oName,'.') == NULL) strcatX (oSpec, ".sts", DU_MAX_PATH_LEN);
    OUTfp = fopen (oSpec, "w");
    if (OUTfp == NULL) {
      DisplayError ("Unable to open output file.");
      return FALSE;
    }
  }
  /****************************************************************************
  * Display information.
  ****************************************************************************/
  ParsePath (CDFpath, CDFdir, CDFname);
  if (!epochMonotonic) {
    WriteOut (OUTfp, "Statistics for \"");
    if (!NULstring(oSpec)) WriteOut (stdout, "Statistics for \"");
  } else {
    WriteOut (OUTfp, "Statistics (for epoch monotonic) for \"");
    if (!NULstring(oSpec)) WriteOut (stdout, "Statistics (for epoch monotonic) for \"");
  }
  WriteOut (OUTfp, CDFname);
  if (!NULstring(oSpec)) WriteOut (stdout, CDFname);
  if (!EndsWithIgCase(CDFname, ".cdf"))
    WriteOut (OUTfp, ".cdf");
    if (!NULstring(oSpec)) WriteOut (stdout, ".cdf");
  if (!NULstring(oSpec)) {
    WriteOut (stdout, "\"  to  \"");
    WriteOut (stdout, oSpec);
  }
  WriteOut (OUTfp, "\"...\n");
  if (!NULstring(oSpec)) WriteOut (stdout, "\"...\n");
  /****************************************************************************
  * Open/inquire CDF.
  ****************************************************************************/
  status = CDFlib (OPEN_, CDF_, CDFpath, &id,
		   SELECT_, CDF_READONLY_MODE_, READONLYon,
			    CDF_zMODE_, zMode,
			    CDF_NEGtoPOSfp0_MODE_, BOO(negToPosFp0,
						       NEGtoPOSfp0on,
						       NEGtoPOSfp0off),
			    CDF_CACHESIZE_, workingCache,
			    STAGE_CACHESIZE_, stageCache,
			    COMPRESS_CACHESIZE_, compressCache,
		   GET_, CDF_NUMrVARS_, &numVars,
			 CDF_NUMzVARS_, &numZvars,
			 CDF_MAJORITY_, &majority,
		   NULL_);
  if (!StatusHandlerStats(status)) return FALSE;
  if (rangeCheck) {
    status = CDFlib (GET_, ATTR_NUMBER_, "VALIDMIN", &validminAttrN,
			   ATTR_NUMBER_, "VALIDMAX", &validmaxAttrN,
		     NULL_);
    if (status == NO_SUCH_ATTR)
      rangeCheck = FALSE;
    else
      if (!StatusHandlerStats(status)) return FALSE;
  }
  if (ignoreFills) {
    status = CDFlib (GET_, ATTR_NUMBER_, "FILLVAL", &fillvalAttrN,
		     NULL_);
    if (status == NO_SUCH_ATTR)
      ignoreFills = FALSE;
    else
      if (!StatusHandlerStats(status)) return FALSE;
  }
  if (useFormat) {
    status = CDFlib (GET_, ATTR_NUMBER_, "FORMAT", &formatAttrN,
		     NULL_);
    if (status == NO_SUCH_ATTR)
      useFormat = FALSE;
    else
      if (!StatusHandlerStats(status)) return FALSE;
  }
  /****************************************************************************
  * Determine statistics for each rVariable.
  ****************************************************************************/
  if (numVars > 0) {
    WriteOut (OUTfp, "\n");
    WriteOut (OUTfp, "rVariables\n");
    WriteOut (OUTfp, "----------\n");
    for (varN = 0; varN < numVars; varN++) {
       Var.Z = FALSE;
       Var.varN = varN;
       status = CDFlib (SELECT_, rVAR_, Var.varN,
			GET_, rVAR_NAME_, Var.varName,
			      rVAR_DATATYPE_, &Var.dataTypeV,
			      rVAR_NUMELEMS_, &Var.numElemsV,
			      rVARs_NUMDIMS_, &Var.numDims,
			      rVARs_DIMSIZES_, Var.dimSizes,
			      rVAR_RECVARY_, &Var.recVary,
			      rVAR_DIMVARYS_, Var.dimVarys,
			      rVAR_MAXREC_, &Var.varMaxRec,
			NULL_);
       if (!StatusHandlerStats(status)) return FALSE;
       if (epochMonotonic && !CDFepochDataType(Var.dataTypeV)) continue;
       if (!SetupVar(&Var)) {
	 FreeVarMem (&Var);
	 return FALSE;
       }
       FreeVarMem (&Var);
    }
  }
  /****************************************************************************
  * Determine statistics for each zVariable.
  ****************************************************************************/
  if (numZvars > 0) {
    WriteOut (OUTfp, "\n");
    WriteOut (OUTfp, "zVariables\n");
    WriteOut (OUTfp, "----------\n");
    for (varN = 0; varN < numZvars; varN++) {
       Var.Z = TRUE;
       Var.varN = varN;
       status = CDFlib (SELECT_, zVAR_, Var.varN,
			GET_, zVAR_NAME_, Var.varName,
			      zVAR_DATATYPE_, &Var.dataTypeV,
			      zVAR_NUMELEMS_, &Var.numElemsV,
			      zVAR_NUMDIMS_, &Var.numDims,
			      zVAR_DIMSIZES_, Var.dimSizes,
			      zVAR_RECVARY_, &Var.recVary,
			      zVAR_DIMVARYS_, Var.dimVarys,
			      zVAR_MAXREC_, &Var.varMaxRec,
			NULL_);
       if (!StatusHandlerStats(status)) return FALSE;
       if (epochMonotonic && !CDFepochDataType(Var.dataTypeV)) continue;
       if (!SetupVar(&Var)) {
	 FreeVarMem (&Var);
	 return FALSE;
       }
       FreeVarMem (&Var);
    }
  }
  WriteOut (OUTfp, "\n");
  /****************************************************************************
  * Close CDF and output file (if specified).
  ****************************************************************************/
  if (displayStats) {
    vSTATS vStatsDotCDF, vStatsStage, vStatsCompress;
    status = CDFlib (CLOSE_, CDFwithSTATS_, &vStatsDotCDF,
					    &vStatsStage,
					    &vStatsCompress,
		     NULL_);
    if (!StatusHandlerStats(status)) return FALSE;
    DisplayStatistics ("CDF", &vStatsDotCDF, &vStatsStage, &vStatsCompress);
  }
  else {
    status = CDFlib (CLOSE_, CDF_,
		     NULL_);
    if (!StatusHandlerStats(status)) return FALSE;
  }
  if (OUTfp != stdout) fclose (OUTfp);
  return TRUE;
}

/******************************************************************************
* SetupVar.
******************************************************************************/

Logical SetupVar (Var)
struct VarStruct *Var;
{
  CDFstatus status;
  char line[MAX_SCREENLINE_LEN+1];
  char delim;
  long dataTypeE;                 /* data type for attribute entry */
  long numElemsE;                 /* number of elements for attribute entry */
  void *temp;
  int dimN;
  int virtual;
  long numElms, dataType;

  /****************************************************************************
  * Initialize pointers for memory to be allocated.
  ****************************************************************************/
  Var->validmin = NULL;
  Var->validmax = NULL;
  Var->fillval = NULL;
  Var->format = NULL;
  Var->min = NULL;
  Var->max = NULL;
  Var->last = NULL;
  Var->minINrange = NULL;
  Var->maxINrange = NULL;
  Var->buffer = NULL;
  virtual = 0;
  if (epochMonotonic) {
    /**************************************************************************
    * Check if this a virtual variable. If so, sets it.
    **************************************************************************/
    status = CDFlib (SELECT_, ATTR_NAME_, "VIRTUAL",
			      BOO(Var->Z,zENTRY_,rENTRY_), Var->varN,
		     GET_, BOO(Var->Z,zENTRY_NUMELEMS_,
				      rENTRY_NUMELEMS_), &numElms,
		     NULL_);
    if (status == CDF_OK) virtual = 1;
  }
  /****************************************************************************
  * Display heading line for this variable.
  ****************************************************************************/
  strcpyX (line, "\n", MAX_SCREENLINE_LEN);
  delim = PickDelimiter (Var->varName, strlen(Var->varName));
  snprintf (EofS(line), (size_t) sizeof(line)-strlen(line),
  	    "%3ld.  %c%s%c", Var->varN + 1, delim, Var->varName, delim);
  snprintf (EofS(line), (size_t) sizeof(line)-strlen(line),
  	    "%s", (virtual==1?" (V)":" "));
  if (Var->Z) {
    snprintf (EofS(line), (size_t) sizeof(line)-strlen(line),
	      "   %ld:[", Var->numDims);
    if (Var->numDims > 0) {
      for (dimN = 0; dimN < Var->numDims; dimN++) {
    	 if (dimN > 0) strcatX (line, ",", MAX_SCREENLINE_LEN);
	 snprintf (EofS(line), (size_t) sizeof(line)-strlen(line),
		   "%ld", Var->dimSizes[dimN]);
      }
    }
    strcatX (line, "]", MAX_SCREENLINE_LEN);
  }
  snprintf (EofS(line), (size_t) sizeof(line)-strlen(line),
	    "   %s/", TFvarianceToken(Var->recVary));
  for (dimN = 0; dimN < Var->numDims; dimN++) {
     snprintf (EofS(line), (size_t) sizeof(line)-strlen(line),
	       "%s", TFvarianceToken(Var->dimVarys[dimN]));
  }
  snprintf (EofS(line), (size_t) sizeof(line)-strlen(line),
	    "   (CDF_%s/%ld)", DataTypeToken(Var->dataTypeV), Var->numElemsV);
  strcatX (line, "\n\n", MAX_SCREENLINE_LEN);
  WriteOut (OUTfp, line);
  /****************************************************************************
  * Return early if no records written.
  ****************************************************************************/
  if (Var->varMaxRec == -1 && epochMonotonic) {
    /**************************************************************************
    * Check if this a virtual variable. If so, handle it.
    **************************************************************************/
    if (virtual == 1) {
      status = CDFlib (SELECT_, ATTR_NAME_, "COMPONENT_1",
				BOO(Var->Z,zENTRY_,rENTRY_), Var->varN,
		       GET_, BOO(Var->Z,zENTRY_NUMELEMS_,
				  	rENTRY_NUMELEMS_), &numElms,
			     BOO(Var->Z,zENTRY_DATATYPE_,
				        rENTRY_DATATYPE_), &dataType,
		       NULL_);
      if (status != CDF_OK) return FALSE;
      if (!HandleVirtual(Var)) return FALSE;
        return TRUE;
    } else {
      WriteOut (OUTfp, "          No records.\n");
      return TRUE;
    }
  }
  /****************************************************************************
  * If range checking requested, get the valid min/max.
  ****************************************************************************/
  if (rangeCheck) {
    status = CDFlib (SELECT_, ATTR_, validminAttrN,
			      BOO(Var->Z,zENTRY_,rENTRY_), Var->varN,
		     GET_, BOO(Var->Z,zENTRY_DATATYPE_,
				      rENTRY_DATATYPE_), &dataTypeE,
			   BOO(Var->Z,zENTRY_NUMELEMS_,
				      rENTRY_NUMELEMS_), &numElemsE,
		     NULL_);
    if (status == NO_SUCH_ENTRY)
      Var->rangeCheckVar = FALSE;
    else {
      if (!StatusHandlerStats(status)) return FALSE;
      temp = cdf_AllocateMemory ((size_t) (CDFelemSize(dataTypeE) * numElemsE),
			         FatalError);
      status = CDFlib (GET_, BOO(Var->Z,zENTRY_DATA_,rENTRY_DATA_), temp,
		       NULL_);
      if (!StatusHandlerStats(status)) {
	cdf_FreeMemory (temp, FatalError);
	return FALSE;
      }
      Var->validmin = cdf_AllocateMemory ((size_t) (CDFelemSize(Var->dataTypeV) *
						    Var->numElemsV),
					  FatalError);
      ConvertDataType (dataTypeE, numElemsE, temp, Var->dataTypeV,
		       Var->numElemsV, Var->validmin);
      cdf_FreeMemory (temp, FatalError);
      status = CDFlib (SELECT_, ATTR_, validmaxAttrN,
				BOO(Var->Z,zENTRY_,rENTRY_), Var->varN,
		       GET_, BOO(Var->Z,zENTRY_DATATYPE_,
				        rENTRY_DATATYPE_), &dataTypeE,
			     BOO(Var->Z,zENTRY_NUMELEMS_,
				        rENTRY_NUMELEMS_), &numElemsE,
		       NULL_);
      if (status == NO_SUCH_ENTRY) {
	Var->rangeCheckVar = FALSE;
	cdf_FreeMemory (Var->validmin, FatalError);
	Var->validmin = NULL;
      }
      else {
	if (!StatusHandlerStats(status)) return FALSE;
	temp = cdf_AllocateMemory ((size_t) (CDFelemSize(dataTypeE) * numElemsE),
				   FatalError);
	status = CDFlib (GET_, BOO(Var->Z,zENTRY_DATA_,rENTRY_DATA_), temp,
			 NULL_);
	if (!StatusHandlerStats(status)) {
	  cdf_FreeMemory (temp, FatalError);
	  return FALSE;
	}
	Var->validmax = cdf_AllocateMemory ((size_t) (CDFelemSize(Var->dataTypeV) *
						      Var->numElemsV),
					    FatalError);
	ConvertDataType (dataTypeE, numElemsE, temp, Var->dataTypeV,
			 Var->numElemsV, Var->validmax);
	cdf_FreeMemory (temp, FatalError);
	Var->rangeCheckVar = TRUE;
     }
    }
  }
  else
    Var->rangeCheckVar = FALSE;
  /****************************************************************************
  * If fill values are to be ignored, get the fill value.
  ****************************************************************************/
  if (ignoreFills) {
    status = CDFlib (SELECT_, ATTR_, fillvalAttrN,
			      BOO(Var->Z,zENTRY_,rENTRY_), Var->varN,
		     GET_, BOO(Var->Z,zENTRY_DATATYPE_,
				      rENTRY_DATATYPE_), &dataTypeE,
			   BOO(Var->Z,zENTRY_NUMELEMS_,
				      rENTRY_NUMELEMS_), &numElemsE,
		     NULL_);
    if (status == NO_SUCH_ENTRY)
      Var->ignoreFillsVar = FALSE;
    else {
      if (!StatusHandlerStats(status)) return FALSE;
      temp = cdf_AllocateMemory ((size_t) (CDFelemSize(dataTypeE) * numElemsE),
			     FatalError);
      status = CDFlib (GET_, BOO(Var->Z,zENTRY_DATA_,rENTRY_DATA_), temp,
		       NULL_);
      if (!StatusHandlerStats(status)) {
	cdf_FreeMemory (temp, FatalError);
	return FALSE;
      }
      Var->fillval = cdf_AllocateMemory ((size_t) (CDFelemSize(Var->dataTypeV) *
					       Var->numElemsV),
				     FatalError);
      ConvertDataType (dataTypeE, numElemsE, temp, Var->dataTypeV,
		       Var->numElemsV, Var->fillval);
      cdf_FreeMemory (temp, FatalError);
      Var->ignoreFillsVar = TRUE;
    }
  }
  else
    Var->ignoreFillsVar = FALSE;
  /****************************************************************************
  * If the FORMAT attribute is to be used, get the entry for this variable.
  ****************************************************************************/
  if (useFormat) {
    status = CDFlib (SELECT_, ATTR_, formatAttrN,
			      BOO(Var->Z,zENTRY_,rENTRY_), Var->varN,
		     GET_, BOO(Var->Z,zENTRY_DATATYPE_,
				      rENTRY_DATATYPE_), &dataTypeE,
			   BOO(Var->Z,zENTRY_NUMELEMS_,
				      rENTRY_NUMELEMS_), &numElemsE,
		     NULL_);
    if (status != NO_SUCH_ENTRY) {
      if (!StatusHandlerStats(status)) return FALSE;
      if (STRINGdataType(dataTypeE)) {
	Var->format = cdf_AllocateMemory ((size_t) (numElemsE + 1),
				      FatalError);
	status = CDFlib (GET_, BOO(Var->Z,zENTRY_DATA_,
					  rENTRY_DATA_), Var->format,
			 NULL_);
	if (!StatusHandlerStats(status)) return FALSE;
	Var->format[(int)numElemsE] = NUL;
      }
    }
  }
  /****************************************************************************
  * Read each record and calculate statistics.
  ****************************************************************************/
  if (!CALCstat(Var, FALSE)) return FALSE;
  /****************************************************************************
  * Return - note that memory allocated will be freed by caller.
  ****************************************************************************/
  return TRUE;
}

/******************************************************************************
* FreeVarMem.
******************************************************************************/

void FreeVarMem (Var)
struct VarStruct *Var;
{
  if (Var->validmin != NULL) cdf_FreeMemory (Var->validmin,
					     FatalError);
  if (Var->validmax != NULL) cdf_FreeMemory (Var->validmax,
					     FatalError);
  if (Var->fillval != NULL) cdf_FreeMemory (Var->fillval, FatalError);
  if (Var->format != NULL) cdf_FreeMemory (Var->format, FatalError);
  if (Var->min != NULL) cdf_FreeMemory (Var->min, FatalError);
  if (Var->max != NULL) cdf_FreeMemory (Var->max, FatalError);
  if (Var->last != NULL) cdf_FreeMemory (Var->last, FatalError);
  if (Var->minINrange != NULL) cdf_FreeMemory (Var->minINrange,
					       FatalError);
  if (Var->maxINrange != NULL) cdf_FreeMemory (Var->maxINrange,
					       FatalError);
  if (Var->buffer != NULL) cdf_FreeMemory (Var->buffer, FatalError);
  return;
}

/******************************************************************************
* StatusHandlerStats.
******************************************************************************/

Logical StatusHandlerStats (status)
CDFstatus status;
{
  char text[CDF_STATUSTEXT_LEN + 1];

  if (StatusERROR(status)) {
    if (report[ERRORs]) {
      CDFlib (SELECT_, CDF_STATUS_, status,
	      GET_, STATUS_TEXT_, text,
	      NULL_);
      WriteOut (OUTfp, "ERROR> ");
      WriteOut (OUTfp, text);
      WriteOut (OUTfp, "\n");
    }
    CDFlib (CLOSE_, CDF_,
		NULL_);
    if (OUTfp != stdout) fclose (OUTfp);
    return FALSE;
  }

  if (StatusWARN(status) && report[WARNs]) {
    CDFlib (SELECT_, CDF_STATUS_, status,
	    GET_, STATUS_TEXT_, text,
	    NULL_);
    WriteOut (OUTfp, "WARNING> ");
    WriteOut (OUTfp, text);
    WriteOut (OUTfp, "\n");
    return TRUE;
  }

  if (StatusINFO(status) && report[INFOs]) {
    CDFlib (SELECT_, CDF_STATUS_, status,
	    GET_, STATUS_TEXT_, text,
	    NULL_);
    WriteOut (OUTfp, "INFO> ");
    WriteOut (OUTfp, text);
    WriteOut (OUTfp, "\n");
    return TRUE;
  }

  return TRUE;  /* CDF_OK */
}

/******************************************************************************
* StatisticsQOPs.
*    Returns TRUE if execution should continue.
******************************************************************************/

#if defined(mac)
Logical StatisticsQOPs (argC, argV)
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
  static Logical pageOutput = DEFAULTpageSTATS;
  static Logical useFormat = DEFAULTformatSTATS;
  static Logical rangeCheck = DEFAULTrangeSTATS;
  static Logical useFillval = DEFAULTfillSTATS;
  static Logical negToPos = DEFAULT_NEGtoPOSfp0;
  static Logical reportInfos = REPORTinfosDEFAULT;
  static Logical reportWarns = REPORTwarningsDEFAULT;
  static Logical reportErrors = REPORTerrorsDEFAULT;
  static Logical updateValids = DEFAULTupValidsSTATS;
  static Logical updateScales = DEFAULTupScalesSTATS;
  static Logical updateMono = DEFAULTupMonoSTATS;
  static Logical dispStats = DEFAULTstatsSTATS;
  static int zMode = DEFAULTzModeSTATS;
  static Str255 CDFtext = "\p";
  static Str255 outText = "\p";
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
  SetIText ((Handle) controlHs[OUTTEXTin], outText);
  SetIText ((Handle) controlHs[CACHEin], cacheText);
  if (pageOutput) SetCtlValue (controlHs[PAGEin], 1);
  if (useFormat) SetCtlValue (controlHs[FORMATin], 1);
  if (rangeCheck) SetCtlValue (controlHs[RANGEin], 1);
  if (useFillval) SetCtlValue (controlHs[FILLin], 1);
  if (negToPos) SetCtlValue (controlHs[NEGZin], 1);
  if (reportInfos) SetCtlValue (controlHs[INFOin], 1);
  if (reportWarns) SetCtlValue (controlHs[WARNin], 1);
  if (reportErrors) SetCtlValue (controlHs[ERRORin], 1);
  if (updateValids) SetCtlValue (controlHs[VALIDin], 1);
  if (updateScales) SetCtlValue (controlHs[SCALEin], 1);
  if (updateMono) SetCtlValue (controlHs[MONOin], 1);
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
	char tempS1[1+1];
	/**********************************************************************
	* Get the value of each control.
	**********************************************************************/
	GetIText ((Handle) controlHs[CDFTEXTin], CDFtext);
	GetIText ((Handle) controlHs[OUTTEXTin], outText);
	GetIText ((Handle) controlHs[CACHEin], cacheText);
	pageOutput = GetCtlValue (controlHs[PAGEin]);
	useFormat = GetCtlValue (controlHs[FORMATin]);
	rangeCheck = GetCtlValue (controlHs[RANGEin]);
	useFillval = GetCtlValue (controlHs[FILLin]);
	negToPos = GetCtlValue (controlHs[NEGZin]);
	reportInfos = GetCtlValue (controlHs[INFOin]);
	reportWarns = GetCtlValue (controlHs[WARNin]);
	reportErrors = GetCtlValue (controlHs[ERRORin]);
	updateValids = GetCtlValue (controlHs[VALIDin]);
	updateScales = GetCtlValue (controlHs[SCALEin]);
	updateMono = GetCtlValue (controlHs[MONOin]);
	dispStats = GetCtlValue (controlHs[STATSin]);
	for (zMode = 0; zMode < 3; zMode++) {
	   if (GetCtlValue(controlHs[ZMODEinBASE+zMode])) break;
	}
	/**********************************************************************
	* Build argc/argv.
	**********************************************************************/
        *argC = 14 + BOO(NULpString(CDFtext),0,1) +
		     BOO(NULpString(outText),0,2) +
		     BOO(NULpString(cacheText),0,2);
        *argV = (char **) cdf_AllocateMemory ((size_t)*argC * sizeof(char *),
					  FatalError);
	n = 0;
	MAKEstrARGv (argV, n, pgmName)
	if (!NULpString(CDFtext)) {
	  PtoCstr (CDFtext);
	  MAKEstrARGv (argV, n, (char *) CDFtext)
	  CtoPstr ((char *) CDFtext);
	}
	MAKEbooARGv (argV, n, pageOutput, "-page", "-nopage")
	MAKEbooARGv (argV, n, useFormat, "-format", "-noformat")
	MAKEbooARGv (argV, n, rangeCheck, "-range", "-norange")
	MAKEbooARGv (argV, n, useFillval, "-fill", "-nofill")
	MAKEbooARGv (argV, n, negToPos, "-neg2posfp0", "-noneg2posfp0")
	MAKEbooARGv (argV, n, updateValids, "-update_valids",
			     "-noupdate_valids")
	MAKEbooARGv (argV, n, updateScales, "-update_scales",
			     "-noupdate_scales")
	MAKEbooARGv (argV, n, updateMono, "-update_monotonic",
			     "-noupdate_monotonic")
	MAKEbooARGv (argV, n, dispStats, "-statistics", "-nostatistics")
	MAKEstrARGv (argV, n, "-zmode")
	snprintf (tempS1, (size_t) sizeof(tempS1), "%d", zMode);
	MAKEstrARGv (argV, n, tempS1)
	MAKEstrARGv (argV, n, "-report")
	MAKEstrARGv (argV, n, StatusCodeReportOptions(reportErrors,
					      reportWarns,
					      reportInfos))
	if (!NULpString(outText)) {
	  MAKEstrARGv (argV, n, "-output")
	  PtoCstr (outText);
	  MAKEstrARGv (argV, n, (char *) outText)
	  CtoPstr ((char *) outText);
	}
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
      * Select output file.
      * The cursor is set because `StandardPutFile' leaves the cursor as
      * an iBeam (instead of returning it to what it was).
      ************************************************************************/
      case OUTSELECTin: {
	StandardFileReply outReply;
	char outPath[DU_MAX_PATH_LEN+1], prompt[] = "Enter skeleton table:";
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
      case PAGEin:
      case FORMATin:
      case RANGEin:
      case FILLin:
      case NEGZin:
      case INFOin:
      case WARNin:
      case ERRORin:
      case VALIDin:
      case SCALEin:
      case MONOin:
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
