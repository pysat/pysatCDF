/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                                       CDFexport.
*
*  Version 1.2e, 14-Dec-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  13-Sep-95, J Love     Original version.
*   V1.0a 28-Sep-95, J Love     Macintosh dialog filtering.  Outline default
*                               button.
*   V1.1   9-Sep-96, J Love     CDF V2.6.
*   V1.2  15-Nov-96, J Love     Added `simple' environment and batch mode.
*   V1.2a 15-Jan-97, J Love     Added prompts for settings file (when saving
*                               and restoring).  Added `help' qualifier.
*   V1.2b 21-Feb-97, J Love     Removed RICE.
*   V1.2c 30-Mar-97, J Love     Allow FieldWindow fields to exceed their
*                               on-screen width.
*   V1.2d 16-Nov-97, J Love	Windows NT/Visual C++.
*   V1.2e 14-Dec-97, J Love	Added ALPHAVMSi encoding.
*   V1.3  03-May-06, M Liu      Added checksum option for the files.
*   V1.4  13-Oct-06, M Liu      Changed to allow upper and lower case CDF  
*                               name to be used on win32.
*   V3.3  10-Jan-09, M Liu      Validate a file before it is used. Use "PPC"
*                               instead of "MAC" for encoding.
*   V3.3a  1-Aug-09, M Liu      Added new qualifiers of 
*                                 - include | exclude
*                                 - epochrange | recordrange
*                                 - controlsettings
*                               to the batch run. 
*   V3.3b 10-Dec-10, M Liu      Added a new option "ios8601" for epoch output.
*   V3.3c 20-Dec-10, M Liu      Added another option "poundedheading" for
*                               heading.
*   V3.3d 04-Apr-11, M Liu      Modified to handle TT2000 epoch style string.
*
******************************************************************************/

#define CDFXP
#include "cdfxp.h"

void FreeTemps();

/******************************************************************************
* Increased stack size and overlay buffer for Borland C on IBM PC.
******************************************************************************/

#if defined(BORLANDC)
extern unsigned _stklen = BORLANDC_STACK_SIZE;
extern unsigned _ovrbuffer = BORLANDC_OVERLAY_SIZE;
#endif

/******************************************************************************
* Main.
******************************************************************************/

#if !defined(win32)
MAIN {
  Logical success = TRUE;
  strcpyX (pgmName, "CDFexport", MAX_PROGRAM_NAME_LEN);
#if defined(mac)
  MacExecuteFSI (ExportCDFs, ExportQOPs);
#else
  success = ExportCDFs (argc, argv);
  FreeTemps ();
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
* ExportCDFs.
******************************************************************************/

Logical ExportCDFs (argC, argV)
int argC;
char *argV[];
{
  static char CDFspec[DU_MAX_PATH_LEN+1] = CURRENTDIRECTORY;
  long zMode = BOO(DEFAULTsimpleEXPORT,DEFAULTzModeSIMPLE,DEFAULTzModeEXPORT);
  Logical negToPosFp0 = DEFAULT_NEGtoPOSfp0;
  Logical prompt = BOO(DEFAULTsimpleEXPORT,DEFAULTpromptSIMPLE,
					   DEFAULTpromptEXPORT);
  /****************************************************************************
  * Set defaults based on simple mode.
  ****************************************************************************/
  OptDefaults ();
  /****************************************************************************
  * Scan parameters/options/qualifiers.
  ****************************************************************************/
  switch (argC) {
    case 1:
      if (!prompt) {
	create_pasteboard ();
	set_cursor_mode (CURSORoff);
	OnlineHelpWindow ("cdfxp.ilh", OLHhelpID);
	delete_pasteboard (ERASE);
	set_cursor_mode (CURSORon);
	return TRUE;
      }
      break;
    default: {
      QOP *qop;
      static char *validQuals[] = {
	"initial", "prompt", "noprompt", "cache", "zmode", "neg2posfp0",
	"noneg2posfp0", "report", "statistics", "nostatistics", "about",
	"simple", "nosimple", "cdf", "text", "settings", "batch", "help",
	"include", "exclude", "sequencetime", "controlsettings", 
        "epochrange", "recordrange", NULL
      };
      static int optRequired[] = {
	TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE,
	FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE,
        FALSE, TRUE, TRUE, 0
      };
      static char *reportTokens[] = { "errors", "warnings", "informationals" };
      Logical qopError = FALSE;
      /************************************************************************
      * Parse qualifiers, options, and parameters (QOPs).
      ************************************************************************/
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
      * Check for `help' qualifier.
      ************************************************************************/
      if (qop->qualEntered[HELPqual]) {
	create_pasteboard ();
	set_cursor_mode (CURSORoff);
	OnlineHelpWindow ("cdfxp.ilh", OLHhelpID);
	delete_pasteboard (ERASE);
	set_cursor_mode (CURSORon);
	cdf_FreeMemory (qop, FatalError);
	return TRUE;
      }
      /************************************************************************
      * Check for `simple' qualifier.
      ************************************************************************/
      qopError = qopError | !TFqualifier(qop,&simpleMode,SIMPLEqual,
					 NOSIMPLEqual,DEFAULTsimpleEXPORT,
					 "simple");
      /************************************************************************
      * Set some defaults based on simple mode.  The other defaults will be
      * set as the remaining qualifiers are checked.
      ************************************************************************/
      strcpyX (settingsFile, BOO(simpleMode,"simple.set",
					    "export.set"), DU_MAX_PATH_LEN);
      zMode = BOO(simpleMode,DEFAULTzModeSIMPLE,DEFAULTzModeEXPORT);
      OptDefaults ();
      /************************************************************************
      * Check for the `prompt' qualifier.
      ************************************************************************/
      qopError = qopError | !TFqualifier(qop,&prompt,PROMPTqual,NOPROMPTqual,
					 BOO(simpleMode,
					     DEFAULTpromptSIMPLE,
					     DEFAULTpromptEXPORT),"prompt");
      /************************************************************************
      * Check for `batch' qualifier.
      * Note that the `batch' qualifier will negate the `prompt' qualifier.
      ************************************************************************/
      if (qop->qualEntered[BATCHqual]) {
	static char *batchStrings[] = { "cdf", "text", NULL };
	static int batches[] = { BATCHcdf, BATCHtext };
	int match = FindUniqueMatch (qop->qualOpt[BATCHqual],batchStrings);
	switch (match) {
	  case NOMATCH:
	    DisplayError ("Unknown batch mode.");
	    qopError = TRUE;
	    break;
	  case MATCHES:
	    DisplayError ("Ambiguous batch mode.");
	    qopError = TRUE;
	    break;
	  default:
	    batchMode = batches[match];
	    prompt = FALSE;
	    break;
	}
      }
      else
	batchMode = noBATCH;
      /************************************************************************
      * Check for `neg2posfp0' and `statistics' qualifiers.
      ************************************************************************/
      qopError = qopError | !TFqualifier(qop,&negToPosFp0,NEG2POSFP0qual,
					 NONEG2POSFP0qual,DEFAULT_NEGtoPOSfp0,
					 "neg2posfp0");
      qopError = qopError | !TFqualifier(qop,&dumpStats,STATSqual,NOSTATSqual,
					 DEFAULTstatsEXPORT,"statistics");
      /************************************************************************
      * Check for `cdf', `text', and `settings' qualifiers.
      ************************************************************************/
      if (qop->qualEntered[CDFqual]) {
        if (!ValidateQual(qop->qualOpt[CDFqual], validQuals))
          strcpyX (outputCDF, qop->qualOpt[CDFqual], CDF_PATHNAME_LEN);
        else {
          DisplayError ("Invalid \"cdf\" qualifier.");
          qopError = TRUE;
        }
      }
      if (qop->qualEntered[TEXTqual]) {
        if (!ValidateQual(qop->qualOpt[TEXTqual], validQuals))
          strcpyX (outputText, qop->qualOpt[TEXTqual], DU_MAX_PATH_LEN);
        else {
          DisplayError ("Invalid \"text\" qualifier.");
          qopError = TRUE;
        }
      }
      if (qop->qualEntered[SETTINGSqual]) {
        if (!ValidateQual(qop->qualOpt[SETTINGSqual], validQuals)) {
          strcpyX (settingsFile, qop->qualOpt[SETTINGSqual], DU_MAX_PATH_LEN);
          numVars = 0;
        } else {
          DisplayError ("Invalid \"settings\" qualifier.");
          qopError = TRUE;
        }
      }
      /************************************************************************
      * Check for `cache' qualifier.
      ************************************************************************/
      if (qop->qualEntered[CACHEqual]) {
	if (!ParseCacheSizes(qop->qualOpt[CACHEqual],
			     &workingCache,&stageCache,&compressCache)) {
	  DisplayError ("Illegal cache size/type.");
	  qopError = TRUE;
	}
      }
      /***********************************************************************
      * Check for `epochrange' or 'recordrange' qualifier.
      ***********************************************************************/
      if (!S2qualifierLong(qop,&range,EPOCHRANGEqual,
                           1L,RECORDRANGEqual,2L,
                           0L,"epochrange and recordrange")) {
        qopError = TRUE;
      } else {
        if (range  == 1L) {
          char *epochStrings[2];
          epochStrings[0] = (char *) cdf_AllocateMemory((size_t)36+1, NULL);
          epochStrings[1] = (char *) cdf_AllocateMemory((size_t)36+1, NULL);
          ParseStringForTokens(',', qop->qualOpt[EPOCHRANGEqual],
                               epochStrings);
          if (!ParseEpochRange(epochStrings)) {
            qopError = TRUE;
          }
          cdf_FreeMemory (epochStrings[0], FatalError);
          cdf_FreeMemory (epochStrings[1], FatalError);
        } else if (range == 2L) {
          if (!ParseRecordRange(qop->qualOpt[RECORDRANGEqual], &recordStart,
                                &recordEnd)) {
            DisplayError ("Illegal record range.");
            qopError = TRUE;
          }
          --recordStart;
          --recordEnd;
        }
      }
      /***********************************************************************
      * Check for `report' qualifier.
      ***********************************************************************/
      if (qop->qualEntered[REPORTqual]) {
	if (!ParseOptionList(3,reportTokens,
			     qop->qualOpt[REPORTqual],report)) {
	  DisplayError ("Illegal list of `report' options.");
	  qopError = TRUE;
	}
      }
      /***********************************************************************
      * Check for `include' or 'exclude' qualifier.
      ***********************************************************************/
      if (!S2qualifierLong(qop,&includeVars,INCLUDEqual,
                           1L,EXCLUDEqual,2L,
                           0L,"include and exclude")) {
        qopError = TRUE;
      } else {
        char *tmp;
        int len;
        if (includeVars > 0 && qop->qualEntered[SETTINGSqual]) {
          DisplayError ("include/exclude and settings file are mutually exclusive.");
          qopError = TRUE;
        } else {
          if (includeVars == 1L) {
            len = strlen(qop->qualOpt[INCLUDEqual]);
            tmp = (char *) cdf_AllocateMemory ((size_t)len+1, NULL);
            strcpyX (tmp, qop->qualOpt[INCLUDEqual], len);
          } else if (includeVars == 2L) {
            len = strlen(qop->qualOpt[EXCLUDEqual]);
            tmp = (char *) cdf_AllocateMemory ((size_t)len+1, NULL);
            strcpyX (tmp, qop->qualOpt[EXCLUDEqual], len);
          }
          if (includeVars > 0) {
            if ((strstrIgCase(tmp, "varsfile=") != NULL) ||
                (strstrIgCase(tmp, "varsfile:") != NULL)) {
              if (!LoadVarsTextFile(tmp+9)) {
                DisplayError ("`include/exclude' file does not exist or is bad.");
                qopError = TRUE;
              }
            } else {
              strcpyX (varsList, tmp, len);
              numVars = GetNumTokens (',', varsList);
              if (numVars > 0) {
                int i;
                vars = (char **) cdf_AllocateMemory ((size_t)sizeof(char *) * numVars, 
                                                     FatalError);
                for (i = 0; i < numVars; ++i)
                  vars[i] = (char *) cdf_AllocateMemory ((size_t)CDF_VAR_NAME_LEN256+1,
                                                         FatalError);
                ParseStringForTokens (',', varsList, vars);  
              } else {
                DisplayError ("Illegal list of `include' or 'exclude' option.");
                qopError = TRUE;
              }
            }
            cdf_FreeMemory (tmp, FatalError);
          }
        }
      }
      /************************************************************************
      * Check for 'controlsettings' qualifier.
      ************************************************************************/
      if (qop->qualEntered[SETTINGSSEQUENCEqual]) {
        if (!qop->qualEntered[SETTINGSqual]) {
          FILE *fp = fopen (settingsFile, "r");
          if (fp == NULL) {
            DisplayError ("Missing settings file. ");
            qopError = TRUE;
          } else {
            fclose(fp);
            outputVars = 2L;
          }
        } else
          outputVars = 2L;
      } else
        outputVars = 0L;
      /************************************************************************
      * Check for `zMode' qualifier.
      ************************************************************************/
      if (qop->qualEntered[ZMODEqual]) {
	if (!strcmp(qop->qualOpt[ZMODEqual],"0"))
	  zMode = zMODEoff;
	else
	  if (!strcmp(qop->qualOpt[ZMODEqual],"1"))
	    zMode = zMODEon1;
	  else
	    if (!strcmp(qop->qualOpt[ZMODEqual],"2"))
	      zMode = zMODEon2;
	    else {
	      DisplayError ("Illegal zMode.");
	      qopError = TRUE;
	    }
      } else
        if (cdaweb) zMode = zMODEon2;
      /************************************************************************
      * Check for `initial' qualifier.
      ************************************************************************/
      if (qop->qualEntered[INITIALqual]) {
	char *option, *parenPtr, *commaPtr, *nextPtr; int which;
	static char *validOptions[] = {
	  "format", "noformat", "filter", "nofilter", "fillval", "nofillval",
	  "single", "multi", "host", "network", "epoch", "epoch1", "epoch2",
	  "epoch3", "epochf", "epochx", "horizontal", "vertical", "record",
	  "norecord", "indices", "noindices", "row", "column", "validmin",
	  "novalidmin", "monoton", "nomonoton", "validmax", "novalidmax",
	  "fills", "nofills", "exclusive", "noexclusive", "output", "nooutput",
	  "delete", "nodelete", "preallocate", "nopreallocate", "heading",
	  "noheading", "iso8601", "poundedheading", NULL
	};
	/**********************************************************************
	* If VMS-style QOPs, "remove" beginning and ending parenthesis.  Also,
	* remove any white space in the list of options.
	**********************************************************************/
	if (qop->qualOpt[INITIALqual][0] == '(') {
	  option = &(qop->qualOpt[INITIALqual][1]);
	  parenPtr = strchr (option, ')');
	  if (parenPtr != NULL) *parenPtr = NUL;
	}
	else
	  option = qop->qualOpt[INITIALqual];
	RemoveWhiteSpace (option);
	/**********************************************************************
	* Scan comma-separated list of options.
	**********************************************************************/
	while (*option != NUL) {
	  /********************************************************************
	  * Location the end of the current option.  This will be either a
	  * comma or a NUL (if the last option).  Also set the pointer to the
	  * next option.
	  ********************************************************************/
	  commaPtr = strchr (option, ',');
	  if (commaPtr != NULL) {
	    *commaPtr = NUL;
	    nextPtr = commaPtr + 1;
	  }
	  else
	    nextPtr = option + strlen(option);
	  /********************************************************************
	  * Validate/set the current option.
	  ********************************************************************/
	  which = MatchOption (option, validOptions);
	  switch (which) {
	    case UNKNOWNopt: {
	      char msg[MAX_MESSAGE_TEXT_LEN+1];
	      snprintf (msg, (size_t) sizeof(msg),
		        "Unknown `initial' option: \"%s\"", option);
	      DisplayError (msg);
	      qopError = TRUE;
	      break;
	    }
	    case AMBIGUOUSopt: {
	      char msg[MAX_MESSAGE_TEXT_LEN+1];
	      snprintf (msg, (size_t) sizeof(msg),
		        "Ambiguous `initial' option: \"%s\"", option);
	      DisplayError (msg);
	      qopError = TRUE;
	      break;
	    }
	    case FILTERopt:
	    case NOFILTERopt:
	    case FILLSopt:
	    case NOFILLSopt:
	    case FILLVALopt:
	    case NOFILLVALopt:
	    case VALIDMINopt:
	    case NOVALIDMINopt:
	    case VALIDMAXopt:
	    case NOVALIDMAXopt:
	    case MONOTONopt:
	    case NOMONOTONopt:
	    case SINGLEopt:
	    case MULTIopt:
	    case HOSTopt:
	    case NETWORKopt:
	    case EXCLUSIVEopt:
	    case NOEXCLUSIVEopt:
	    case DELETEopt:
	    case NODELETEopt:
	    case PREALLOCATEopt:
	    case NOPREALLOCATEopt:
	      if (simpleMode) {
		char msg[MAX_MESSAGE_TEXT_LEN+1];
		snprintf (msg, (size_t) sizeof(msg),
		         "Illegal `initial' option: \"%s\"", option);
		DisplayError (msg);
		qopError = TRUE;
	      }
	      else {
		switch (which) {
		    case FILTERopt: opt.eachFilter = TRUE; break;
		    case NOFILTERopt: opt.eachFilter = FALSE; break;
		    case FILLSopt: opt.useFills = TRUE; break;
		    case NOFILLSopt: opt.useFills = FALSE; break;
		    case FILLVALopt: opt.useFILLVAL = TRUE; break;
		    case NOFILLVALopt: opt.useFILLVAL = FALSE; break;
		    case VALIDMINopt: opt.useVALIDMIN = TRUE; break;
		    case NOVALIDMINopt: opt.useVALIDMIN = FALSE; break;
		    case VALIDMAXopt: opt.useVALIDMAX = TRUE; break;
		    case NOVALIDMAXopt: opt.useVALIDMAX = FALSE; break;
		    case MONOTONopt: opt.useMONOTON = TRUE; break;
		    case NOMONOTONopt: opt.useMONOTON = FALSE; break;
		    case SINGLEopt: opt.singleFile = TRUE; break;
		    case MULTIopt: opt.singleFile = FALSE; break;
		    case HOSTopt: opt.encoding = HOST_ENCODING; break;
		    case NETWORKopt: opt.encoding = NETWORK_ENCODING; break;
		    case EXCLUSIVEopt: opt.exclusive = TRUE; break;
		    case NOEXCLUSIVEopt: opt.exclusive = FALSE; break;
		    case DELETEopt: opt.deleteExisting = TRUE; break;
		    case NODELETEopt: opt.deleteExisting = FALSE; break;
		    case PREALLOCATEopt: opt.preAllocate = TRUE; break;
		    case NOPREALLOCATEopt: opt.preAllocate = FALSE; break;
		}
	      }
	      break;
	    case FORMATopt: opt.useFORMAT = TRUE; break;
	    case NOFORMATopt: opt.useFORMAT = FALSE; break;
	    case EPOCHopt: opt.epochStyle = EPOCH0_STYLE; break;
	    case EPOCH1opt: opt.epochStyle = EPOCH1_STYLE; break;
	    case EPOCH2opt: opt.epochStyle = EPOCH2_STYLE; break;
	    case EPOCH3opt: opt.epochStyle = EPOCH3_STYLE; break;
	    case ISO8601opt: opt.epochStyle = EPOCH4_STYLE; break;
	    case EPOCHFopt: opt.epochStyle = EPOCHf_STYLE; break;
	    case EPOCHXopt: opt.epochStyle = EPOCHx_STYLE; break;
	    case HORIZONTALopt: opt.horizontalMode = TRUE; break;
	    case VERTICALopt: opt.horizontalMode = FALSE; break;
	    case RECORDopt: opt.showRecord = TRUE; break;
	    case NORECORDopt: opt.showRecord = FALSE; break;
	    case INDICESopt: opt.showIndices = TRUE; break;
	    case NOINDICESopt: opt.showIndices = FALSE; break;
	    case ROWopt: opt.majority = ROW_MAJOR; break;
	    case COLUMNopt: opt.majority = COLUMN_MAJOR; break;
	    case OUTPUTopt: opt.outputItem = TRUE; break;
	    case NOOUTPUTopt: opt.outputItem = FALSE; break;
	    case HEADINGopt: opt.textHeading = TRUE; poundedheading = 0; break;
	    case NOHEADINGopt: opt.textHeading = FALSE; break;
            case POUNDEDHEADINGopt: opt.textHeading = TRUE; poundedheading = 1;
                                    break;
	  }
	  /********************************************************************
	  * Move to the next option.
	  ********************************************************************/
	  option = nextPtr;
	}
      }
      /************************************************************************
      * Check for a parameter (the CDF[s]).
      ************************************************************************/
      switch (qop->Nparms) {
	case 0:
	  if (!prompt) {
	    DisplayError ("Missing parameter [CDF(s) specification].");
	    qopError = TRUE;
	  }
	  break;
	case 1:
	  strcpyX (CDFspec, qop->parms[CDFparm], DU_MAX_PATH_LEN);
	  break;
	default:
	  DisplayError ("Too many parameters.");
	  qopError = TRUE;
      }
      /************************************************************************
      * Free QOP memory and return if an error was detected.
      ************************************************************************/
      cdf_FreeMemory (qop, FatalError);
      if (qopError) return FALSE;
      break;
    }
  }

/*   CDFsetValidate (VALIDATEFILEon); */
  /****************************************************************************
  * Depending on batch mode...
  ****************************************************************************/
  if (!BATCH(batchMode)) {
    /**************************************************************************
    * Initialize screen.
    **************************************************************************/
    create_pasteboard ();
    set_cursor_mode (CURSORoff);
    /**************************************************************************
    * Export from CDFs until the cows come home.
    **************************************************************************/
    CDFexportMenu (CDFspec, prompt, zMode, negToPosFp0);
    /**************************************************************************
    * Cleanup screen and return.
    **************************************************************************/
    delete_pasteboard (ERASE);
    set_cursor_mode (CURSORon);
  }
  else {
    /**************************************************************************
    * Batch mode...
    **************************************************************************/
    CDFid id; CDFstatus status; Logical noMoreAccess = FALSE;
    /**************************************************************************
    * Validate.
    **************************************************************************/
    if (IsDir(CDFspec) || IsWild(CDFspec)) {
      DisplayError ("A single CDF must be specified in batch mode.");
      return FALSE;
    }
    /**************************************************************************
    * Open the CDF.
    **************************************************************************/
    status = CDFlib (OPEN_, CDF_, CDFspec, &id,
		     NULL_);
    DisplayStatus (status, "opening CDF");
    if (StatusBAD(status)) return FALSE;
    /**************************************************************************
    * Set CDF modes and cache sizes.
    **************************************************************************/
    status = CDFlib (SELECT_, CDF_READONLY_MODE_, READONLYon,
			      CDF_zMODE_, zMode,
			      CDF_CACHESIZE_, workingCache,
			      STAGE_CACHESIZE_, stageCache,
			      COMPRESS_CACHESIZE_, compressCache,
			      CDF_NEGtoPOSfp0_MODE_, BOO(negToPosFp0,
							 NEGtoPOSfp0on,
							 NEGtoPOSfp0off),
		     NULL_);
    DisplayStatus (status, "setting CDF modes");
    if (StatusBAD(status)) {
      CDFclose (id);
      return FALSE;
    }
    /**************************************************************************
    * Load the CDF.
    **************************************************************************/
    if (!LoadCDF()) {
      CDFclose (id);
      FreeItems ();
      return FALSE;
    }
    /**************************************************************************
    * Load settings file.
    **************************************************************************/
    if (includeVars == 0L) RestoreSettings ();
    /**************************************************************************
    * Generate CDF/listing.
    **************************************************************************/
    switch (batchMode) {
      case BATCHcdf:
	if (!ToCDF(id)) noMoreAccess = TRUE;
	break;
      case BATCHtext:
	if (!cdaweb) {
	  if (!BOO(opt.horizontalMode,ToFileHori(),
				      ToFileVert())) noMoreAccess = TRUE;
	} else
	  if (!ToFileCDAweb()) noMoreAccess = TRUE;
	break;
    }
    /**************************************************************************
    * Close CDF.
    **************************************************************************/
    if (dumpStats && !noMoreAccess) {
      vSTATS vStatsDotCDF, vStatsStage, vStatsCompress;
      char temp1[MAX_SCREENLINE_LEN+1],
	   temp2[MAX_SCREENLINE_LEN+1],
	   temp3[MAX_SCREENLINE_LEN+1];
      status = CDFlib (CLOSE_, CDFwithSTATS_, &vStatsDotCDF,
					      &vStatsStage,
					      &vStatsCompress,
		       NULL_);
      DisplayStatus (status, "closing CDF");
      if (vStatsDotCDF.maxBuffers > 0) {
	BuildStatistics ("input CDF DotCDF file", &vStatsDotCDF,
			 temp1, temp2, temp3);
	printf ("%s\n%s\n%s\n", temp1, temp2, temp3);
      }
      if (vStatsStage.maxBuffers > 0) {
	BuildStatistics ("input CDF staging file", &vStatsStage,
			 temp1, temp2, temp3);
	printf ("%s\n%s\n%s\n", temp1, temp2, temp3);
      }
      if (vStatsCompress.maxBuffers > 0) {
	BuildStatistics ("input CDF compression scratch file", &vStatsCompress,
			 temp1, temp2, temp3);
	printf ("%s\n%s\n%s\n", temp1, temp2, temp3);
      }
    }
    else{
      status = CDFlib (CLOSE_, CDF_,
		       NULL_);
      DisplayStatus (status, "closing CDF");
    }
    /**************************************************************************
    * Free memory.
    **************************************************************************/
    FreeItems ();
  }
  return TRUE;
}

/******************************************************************************
* CDFexportMenu.
******************************************************************************/

void CDFexportMenu (iniSpec, prompt, zMode, negToPosFp0)
char *iniSpec;          /* Initial specification. */
Logical prompt;         /* Was the `prompt' qualifier specified? */
long zMode;             /* zMode to use. */
Logical negToPosFp0;    /* TRUE if -0.0 should be converted to 0.0. */
{
  char path[CDFfieldMAX+1]; static Logical first = TRUE;
  /****************************************************************************
  * CDFwindow definitions.
  ****************************************************************************/
  static char *cdfLabel[2] = { " CDFexport ", " CDFexport (SimpleMode) "};
  AOSs2A (cdfLines,
    "CDF(s):                                                                       ",
    BLANKs78)
  AOSs1 (cdfFields, BLANKs255)
  static int cdfLineNs[] = { CDFfieldLINEn };
  static int cdfCols[] = { CDFfieldCOLn };
  static int cdfLens[] = { CDFfieldLEN };
  static int cdfMaxs[] = { CDFfieldMAX };
  static int cdfExits[] = { ENTERkey_FSI, HELPkey_FSI, EXITkey_FSI, NUL };
  static struct FieldWindowStruct FWcdf_ = {
    0, 0, SCREEN_WIDTH, NULL, 0, NULL, 2, cdfLines, cdfFields, 1, cdfLineNs,
    cdfCols, cdfLens, cdfMaxs, 2, 0, NULL, cdfExits, REFRESHkey_FSI, FALSE,
    NUL, NUL, INSERTorOVERkey_FSI
  };
  /****************************************************************************
  * SelectionWindow definitions.
  ****************************************************************************/
  static char *varLabel[2] = {
    " SelectionWindow, part 0 ",
    " SelectionWindow "
  };
  static char *varHeader[] = { "" };
  static struct ItemWindowStruct IWsel_ = {
    4, 0, SCREEN_WIDTH, NULL, 1, varHeader, 0, NULL, 0, NULL, NULL, NULL,
    9, 0, NULL, NULL, REFRESHkey_FSI, TRUE, NSkey_FSI, PSkey_FSI
  };
  /****************************************************************************
  * KeyWindow definitions.
  ****************************************************************************/
  static char keyDefsBlank[] = "\n\n";
  static char keyDefsPrompt[] = {
    "Open: ________    Help: ________    Exit: ________\n\n"
  };
  static struct EditWindowStruct EWkey_ = {
    " KeyDefinitions ", 17, 0, SCREEN_WIDTH, 0, NULL, keyDefsBlank, 2,
    0, NULL, FALSE, TRUE, NULL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL,
    NUL
  };
  /****************************************************************************
  * MessageWindow definitions.
  ****************************************************************************/
  static char msgLines[] = BLANKs78;
  static int msgExits[] = { NUL };
  static struct EditWindowStruct EWmsg_ = {
    " MessageBuffer ", 21, 0, SCREEN_WIDTH, 0, NULL, msgLines, 1, 0, NULL,
    FALSE, TRUE, msgExits, REFRESHkey_FSI, NUL, NUL, NUL, NUL, NUL, NUL, NUL,
    NUL
  };
  /****************************************************************************
  * Initialize global variables.
  ****************************************************************************/
  itemHead = NULL;
  FWcdf = &FWcdf_;
  IWsel = &IWsel_;
  EWkey = &EWkey_;
  EWmsg = &EWmsg_;
  /****************************************************************************
  * Initialize CDFwindow and encode key definitions.
  ****************************************************************************/
  strcpyX (FWcdf->fields[0], iniSpec, CDFfieldMAX);
  if (first) {
    char *p1 = keyDefsPrompt;
    EncodeKeyDefinitions (1, &p1, ENTERkey_FSI, HELPkey_FSI, EXITkey_FSI);
    FWcdf->label = cdfLabel[BOO(simpleMode,1,0)];
    IWsel->label = varLabel[BOO(simpleMode,1,0)];
    first = FALSE;
  }
  /****************************************************************************
  * Display menu widgets.
  ****************************************************************************/
  FieldWindow (NEWfw, FWcdf, 0, LogicalTRUE);
  ItemWindow (NEWiw, IWsel, 0);
  EditWindow (NEWew, EWkey, LogicalTRUE);
  EditWindow (NEWew, EWmsg, LogicalTRUE);
  /****************************************************************************
  * Determine/export CDF.
  ****************************************************************************/
  if (prompt) {
    NEWkeyDEFS (EWkey, keyDefsPrompt, batchMode)
    FieldWindow (READfw, FWcdf);
  }
  else
    FWcdf->key = ENTERkey_FSI;
  for (;;) {
     switch (FWcdf->key) {
       case ENTERkey_FSI:
	 strcpyX (path, FWcdf->fields[0], CDFfieldMAX);
	 if (IsCDF(path)) {
	   ExportCDF (path, zMode, negToPosFp0);
	   MakeNUL (FWcdf->fLines[CDFvarsRecsLINEn]);
	   FieldWindow (UPDATEfw, FWcdf, 0, LogicalTRUE);
	   IWsel->label = varLabel[BOO(simpleMode,1,0)];
	   IWsel->hLines = varHeader;
	   ItemWindow (UPDATEiw, IWsel, 0);
	 }
	 else
	   if (IsDir(path) || IsWild(path))
	     ExportCDFsSpec (path, zMode, negToPosFp0,
			     varLabel[BOO(simpleMode,1,0)], varHeader);
	   else
	     DisplayMessage ("No CDFs found for specification.", BEEPWAIT1);
	 break;
       case HELPkey_FSI:
	 OnlineHelpWindow ("cdfxp.ilh", CDFhelpID);
	 break;
       case EXITkey_FSI:
	 FieldWindow (DELETEfw, FWcdf);
	 ItemWindow (DELETEiw, IWsel);
	 EditWindow (DELETEew, EWkey);
	 EditWindow (DELETEew, EWmsg);
	 return;
     }
     NEWkeyDEFS (EWkey, keyDefsPrompt, batchMode)
     FieldWindow (READfw, FWcdf);
  }
}

/******************************************************************************
* ExportCDFsSpec.
******************************************************************************/

void ExportCDFsSpec (path, zMode, negToPosFp0, varLabel0, varHeader0)
char *path;
long zMode;
Logical negToPosFp0;
char *varLabel0;
char *varHeader0[];
{
  Logical first = TRUE;
  int nCDFs, CDFn;
  char **dirS, **CDFs;
  static int exitCDFs[] = { ENTERkey_FSI, HELPkey_FSI, EXITkey_FSI, NUL };
  static struct ItemWindowStruct IWcdfs = {
    1, 79 - CDFnameLEN - 2, CDFnameLEN + 2, " Select a CDF ", 0, NULL, 0,
    NULL, 0, NULL, NULL, NULL, 13, 0, NULL, exitCDFs, REFRESHkey_FSI,
    TRUE, NUL, NUL
  };
  static char keyDefs[] = {
    "Open: ________    Help: ________    Exit: ________\n\n"
  };
  /****************************************************************************
  * Encode key definitions first time.
  ****************************************************************************/
  if (first) {
    char *p1 = keyDefs;
    EncodeKeyDefinitions (1, &p1, ENTERkey_FSI, HELPkey_FSI, EXITkey_FSI);
    first = FALSE;
  }
  /****************************************************************************
  * Get directory listing of CDFs.
  ****************************************************************************/
  nCDFs = CDFdirList (path, &dirS, &CDFs);
  if (nCDFs < 1) {
    DisplayMessage ("No CDFs found for specification.", BEEPWAIT1);
    return;
  }
  /****************************************************************************
  * Allocate/load CDFs.
  ****************************************************************************/
  AllocIW (&IWcdfs, nCDFs, nCDFs, IWcdfs.nColsTotal - 2, FatalError);
  for (CDFn = 0; CDFn < nCDFs; CDFn++) {
     strcpyX (IWcdfs.iLines[CDFn], CDFs[CDFn], IWcdfs.nColsTotal - 2);
     IWcdfs.iLineNs[CDFn] = CDFn;
     IWcdfs.iCols[CDFn] = 0;
     IWcdfs.iLens[CDFn] = CDFnameLEN;
  }
  /****************************************************************************
  * Select a CDF.
  ****************************************************************************/
  ItemWindow (NEWiw, &IWcdfs, 0);
  NEWkeyDEFS (EWkey, keyDefs, batchMode)
  for (;;) {
     ItemWindow (READiw, &IWcdfs);
     switch (IWcdfs.key) {
       case ENTERkey_FSI: {
	 char pathX[CDF_PATHNAME_LEN+1];
	 strcpyX (pathX, dirS[IWcdfs.itemN], CDF_PATHNAME_LEN);
	 AppendToDir (pathX, CDFs[IWcdfs.itemN]);
	 ItemWindow (UNDISPLAYiw, &IWcdfs);
	 ExportCDF (pathX, zMode, negToPosFp0);
	 strcpyX (FWcdf->fields[0], path, CDFfieldMAX);
	 MakeNUL (FWcdf->fLines[CDFvarsRecsLINEn]);
	 FieldWindow (UPDATEfw, FWcdf, 0, LogicalTRUE);
	 IWsel->label = varLabel0;
	 IWsel->hLines = varHeader0;
	 ItemWindow (UPDATEiw, IWsel, 0);
	 NEWkeyDEFS (EWkey, keyDefs, batchMode)
	 ItemWindow (REDISPLAYiw, &IWcdfs);
	 break;
       }
       case HELPkey_FSI:
	 OnlineHelpWindow ("cdfxp.ilh", CDFShelpID);
	 break;
       case EXITkey_FSI:
	 ItemWindow (DELETEiw, &IWcdfs);
	 FreeIW (&IWcdfs, FatalError);
	 cdf_FreeMemory (CDFs, FatalError);
	 cdf_FreeMemory (dirS, FatalError);
	 return;
     }
  }
}

/******************************************************************************
* ExportCDF.
******************************************************************************/

void ExportCDF (path, zMode, negToPosFp0)
char *path;
long zMode;
Logical negToPosFp0;
{
  CDFid id; CDFstatus status; Logical noMoreAccess = FALSE;
  static char keyDefsBlank[] = "\n\n";
  /****************************************************************************
  * Erase key definitions while loading.
  ****************************************************************************/
  NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
  /****************************************************************************
  * Open CDF.
  ****************************************************************************/
  DisplayMessage ("Opening CDF...", NOWAIT);
  status = CDFlib (OPEN_, CDF_, path, &id,
		   NULL_);
  DisplayStatus (status, "opening CDF");
  if (StatusBAD(status)) return;
  /****************************************************************************
  * Select CDF modes, etc.
  ****************************************************************************/
  status = CDFlib (SELECT_, CDF_READONLY_MODE_, READONLYon,
			    CDF_zMODE_, zMode,
			    CDF_CACHESIZE_, workingCache,
			    STAGE_CACHESIZE_, stageCache,
			    COMPRESS_CACHESIZE_, compressCache,
			    CDF_NEGtoPOSfp0_MODE_, BOO(negToPosFp0,
						       NEGtoPOSfp0on,
						       NEGtoPOSfp0off),
		   NULL_);
  DisplayStatus (status, "setting CDF modes");
  if (StatusBAD(status)) {
    CDFclose (id);
    return;
  }
  /****************************************************************************
  * Inquire CDF and load/update widgets.
  ****************************************************************************/
  DisplayMessage ("Loading CDF...", NOWAIT);
  if (!LoadCDF()) {
    CDFclose (id);
    FreeItems ();
    return;
  }
  LoadCDFwindow (path);
  FieldWindow (UPDATEfw, FWcdf, 0, LogicalTRUE);
  /****************************************************************************
  * Process keystrokes.
  ****************************************************************************/
  DisplayMessage ("", NOWAIT);
  if (simpleMode)
    SelectionWindow (&noMoreAccess);
  else {
    for (;;) {
       if (!SelectionWindow1(&noMoreAccess)) break;
       if (!SelectionWindow2(&noMoreAccess)) break;
       if (!SelectionWindow3(&noMoreAccess)) break;
       if (!SelectionWindow4(&noMoreAccess)) break;
    }
  }
  if (noMoreAccess) {
    NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
    InfoWindow ("Access to this CDF has been aborted.",
		NULL, NULL, TRUE, TRUE, 1);
  }
  /****************************************************************************
  * Exit this CDF.
  ****************************************************************************/
  FreeItems ();
  FreeIW (IWsel, FatalError);
  if (dumpStats && !noMoreAccess) {
    vSTATS vStatsDotCDF, vStatsStage, vStatsCompress;
    char temp1[MAX_SCREENLINE_LEN+1],
	 temp2[MAX_SCREENLINE_LEN+1],
	 temp3[MAX_SCREENLINE_LEN+1];
    status = CDFlib (CLOSE_, CDFwithSTATS_, &vStatsDotCDF,
					    &vStatsStage,
					    &vStatsCompress,
		     NULL_);
    DisplayStatus (status, "closing CDF");
    if (vStatsDotCDF.maxBuffers > 0) {
      BuildStatistics ("input CDF DotCDF file", &vStatsDotCDF,
		       temp1, temp2, temp3);
      InfoWindow (temp1, temp2, temp3, FALSE, FALSE, 0);
    }
    if (vStatsStage.maxBuffers > 0) {
      BuildStatistics ("input CDF staging file", &vStatsStage,
		       temp1, temp2, temp3);
      InfoWindow (temp1, temp2, temp3, FALSE, FALSE, 0);
    }
    if (vStatsCompress.maxBuffers > 0) {
      BuildStatistics ("input CDF compression scratch file", &vStatsCompress,
		       temp1, temp2, temp3);
      InfoWindow (temp1, temp2, temp3, FALSE, FALSE, 0);
    }
  }
  else {
    status = CDFlib (CLOSE_, CDF_,
		     NULL_);
    DisplayStatus (status, "closing CDF");
  }
  return;
}

/******************************************************************************
* LoadCDFwindow.
******************************************************************************/

void LoadCDFwindow (path)
char *path;
{
  if (path != NULL) strcpyX (FWcdf->fields[0], path, CDFfieldMAX);
  if (simpleMode)
    snprintf (FWcdf->fLines[CDFvarsRecsLINEn], (size_t) sizeof(BLANKs78),
	      "Variables: %ld, Records: %ld", (long) (NrVars + NzVars),
	      (long) (MAXIMUM(rMaxRec,zMaxRec) + 1));
  else
    snprintf (FWcdf->fLines[CDFvarsRecsLINEn], (size_t) sizeof(BLANKs78),
	      "Variables: %ldr/%ldz, Records: %ldr/%ldz, Compression: %s, Checksum: %s",
	      NrVars, NzVars, (long) (rMaxRec + 1), (long) (zMaxRec + 1),
	      CompressionToken(CDFcType,CDFcParms), ChecksumToken(CDFchecksum));
  return;
}

/******************************************************************************
* LoadCDF.
*
* Some of the values loaded aren't really necessary if `simple' mode has been
* selected.
******************************************************************************/

Logical LoadCDF () {
  CDFstatus status; char varName[CDF_VAR_NAME_LEN256+1];
  long eDataType, eNumElems, cPct, sArraysPct;
  int varX, lineL = SCREEN_WIDTH - 2, nVars, dimN;
  struct ItemStruct *Item, *itemTail = NULL;
  static char readingCDF[] = "reading input CDF";
  int ix;

  /****************************************************************************
  * Initialize.
  ****************************************************************************/
  nItems = 0;
  numEpochs = 0;
  /****************************************************************************
  * Inquire/check number of variables.
  ****************************************************************************/
  status = CDFlib (GET_, CDF_NUMrVARS_, &NrVars,
			 CDF_NUMzVARS_, &NzVars,
		   NULL_);
  DisplayStatus (status, readingCDF);
  if (StatusBAD(status)) return FALSE;
  nVars = (int) (NrVars + NzVars);
  if (includeVars == 2L) numVarsO = nVars;
  if (nVars == 0) {
    DisplayMessage ("Error loading CDF.", BEEPWAIT1);
    DisplayMessage ("CDF contains no variables.", BEEPWAIT1);
    return FALSE;
  }
  if (outputVars == 2L) {
    vars = (char **) cdf_AllocateMemory ((size_t)sizeof(char *) * nVars, FatalError);
    for (ix = 0; ix < nVars; ++ix) vars[ix] = NULL;
    numVars = 0;
  }
  /****************************************************************************
  * Inquire/check number of records.
  ****************************************************************************/
  status = CDFlib (GET_, rVARs_MAXREC_, &rMaxRec,
			 zVARs_MAXREC_, &zMaxRec,
		   NULL_);
  DisplayStatus (status, readingCDF);
  if (StatusBAD(status)) return FALSE;
  if (MAXIMUM(rMaxRec,zMaxRec) < 0) {
    DisplayMessage ("Error loading CDF.", BEEPWAIT1);
    DisplayMessage ("CDF contains no records.", BEEPWAIT1);
    return FALSE;
  }
  /****************************************************************************
  * Load CDF compression.
  ****************************************************************************/
  status = CDFlib (GET_, CDF_COMPRESSION_, &CDFcType, CDFcParms, &cPct,
		   NULL_);
  DisplayStatus (status, readingCDF);
  if (StatusBAD(status)) return FALSE;
  /****************************************************************************
  * Load CDF checksum.
  ****************************************************************************/
  status = CDFlib (GET_, CDF_CHECKSUM_, &CDFchecksum, 
                   NULL_);
  DisplayStatus (status, readingCDF);
  if (StatusBAD(status)) return FALSE;
  /****************************************************************************
  * Load records/indices.
  ****************************************************************************/
  if (opt.showRecord) {
    Item = (struct ItemStruct *)
	    cdf_AllocateMemory ((size_t)sizeof(struct ItemStruct), FatalError);
    Item->type = RECORDt;
    Item->width = MIN_RECORD_WIDTH;
    Item->outputSetting = opt.outputItem;
    Item->filterSetting = opt.eachFilter;
    Item->inclusive = TRUE;
    Item->nextItem = NULL;
    Item->Record = (struct RecordStruct *)
		   cdf_AllocateMemory ((size_t)sizeof(struct RecordStruct), FatalError);
    Item->Record->min = NOminMax;
    Item->Record->max = NOminMax;
    itemHead = Item;
    itemTail = Item;
    nItems++;
  }
  if (opt.showIndices) {
    Item = (struct ItemStruct *)
	   cdf_AllocateMemory ((size_t)sizeof(struct ItemStruct), FatalError);
    Item->type = INDICESt;
    Item->width = MIN_INDICES_WIDTH;
    Item->outputSetting = opt.outputItem;
    Item->filterSetting = opt.eachFilter;
    Item->inclusive = TRUE;
    Item->nextItem = NULL;
    Item->Indices = (struct IndicesStruct *)
		    cdf_AllocateMemory ((size_t)sizeof(struct IndicesStruct), FatalError);
    Item->Indices->minNumDims = NOminMax;
    Item->Indices->maxNumDims = NOminMax;
    if (itemHead == NULL)
      itemHead = Item;
    else
      itemTail->nextItem = Item;
    itemTail = Item;
    nItems++;
  }
  /****************************************************************************
  * Load variables.
  ****************************************************************************/
  for (varX = 0; varX < nVars; varX++) {
     /*************************************************************************
     * Allocate a variable structure and an Item structure and place on linked
     * list.
     *************************************************************************/
     Item = (struct ItemStruct *)
	    cdf_AllocateMemory ((size_t)sizeof(struct ItemStruct), FatalError);
     Item->type = VARIABLEt;
     Item->outputSetting = opt.outputItem;
     Item->filterSetting = FALSE;               /* Reset below... */
     Item->inclusive = TRUE;
     Item->Var = (struct VarStruct *) cdf_AllocateMemory ((size_t)sizeof(struct VarStruct),
						      FatalError);
     Item->nextItem = NULL;
     if (itemHead == NULL)
       itemHead = Item;
     else
       itemTail->nextItem = Item;
     itemTail = Item;
     /*************************************************************************
     * Initialize variable structure.
     *************************************************************************/
     Item->Var->zVar = (varX >= NrVars);
     Item->Var->varN = BOO(varX >= NrVars,varX - NrVars,varX);
     Item->Var->min = NULL;
     Item->Var->max = NULL;
     Item->Var->format = NULL;
     Item->Var->fill = NULL;
     Item->Var->pad = NULL;
     Item->Var->value = NULL;
     /*************************************************************************
     * Load variable dimensionality, variances, data type/number of elements,
     * maximum record written, sparseness, compression, and blocking factor.
     *************************************************************************/
     status = CDFlib (SELECT_, BOO(Item->Var->zVar,
				   zVAR_,rVAR_), Item->Var->varN,
                      GET_, BOO(Item->Var->zVar,
				zVAR_NUMDIMS_,
				rVARs_NUMDIMS_), &(Item->Var->numDims),
			    BOO(Item->Var->zVar,
				zVAR_DIMSIZES_,
				rVARs_DIMSIZES_), Item->Var->dimSizes,
			    BOO(Item->Var->zVar,
				zVAR_DATATYPE_,
				rVAR_DATATYPE_), &(Item->Var->dataType),
			    BOO(Item->Var->zVar,
				zVAR_NUMELEMS_,
				rVAR_NUMELEMS_), &(Item->Var->numElems),
			    BOO(Item->Var->zVar,
				zVAR_MAXREC_,
				rVAR_MAXREC_), &(Item->Var->maxRec),
			    BOO(Item->Var->zVar,
				zVAR_RECVARY_,
				rVAR_RECVARY_), &(Item->Var->recVary),
			    BOO(Item->Var->zVar,
				zVAR_DIMVARYS_,
				rVAR_DIMVARYS_), Item->Var->dimVarys,
			    BOO(Item->Var->zVar,
				zVAR_SPARSERECORDS_,
				rVAR_SPARSERECORDS_),
						&(Item->Var->sRecordsType),
			    BOO(Item->Var->zVar,
				zVAR_SPARSEARRAYS_,
				rVAR_SPARSEARRAYS_), &(Item->Var->sArraysType),
						     Item->Var->sArraysParms,
						     &sArraysPct,
			    BOO(Item->Var->zVar,
				zVAR_COMPRESSION_,
				rVAR_COMPRESSION_), &(Item->Var->cType),
						    Item->Var->cParms, &cPct,
			    BOO(Item->Var->zVar,
				zVAR_BLOCKINGFACTOR_,
				rVAR_BLOCKINGFACTOR_), &(Item->Var->blocking),
		      NULL_);
     DisplayStatus (status, readingCDF);
     if (StatusBAD(status)) return FALSE;
     /*************************************************************************
     * Load reserve percentage.
     *************************************************************************/
     if (Item->Var->cType == NO_COMPRESSION)
       Item->Var->reserve = NA_RESERVE;
     else {
       status = CDFlib (CONFIRM_, BOO(Item->Var->zVar,
				      zVAR_RESERVEPERCENT_,
				      rVAR_RESERVEPERCENT_),
							&(Item->Var->reserve),
			NULL_);
       DisplayStatus (status, readingCDF);
       if (StatusBAD(status)) return FALSE;
     }
     /*************************************************************************
     * Calculate the number of values per variable record (conceptual) and
     * the number of bytes per value.
     *************************************************************************/
     Item->Var->nRecordValues = 1;
     for (dimN = 0; dimN < Item->Var->numDims; dimN++) {
	Item->Var->nRecordValues *= Item->Var->dimSizes[dimN];
     }
     Item->Var->nValueBytes =
	    (size_t) (CDFelemSize(Item->Var->dataType) * Item->Var->numElems);
     /*************************************************************************
     * Load variable name.
     *************************************************************************/
     status = CDFlib (GET_, BOO(Item->Var->zVar,
				zVAR_NAME_,rVAR_NAME_), varName,
		      NULL_);
     DisplayStatus (status, readingCDF);
     if (StatusBAD(status)) return FALSE;
     RemoveTrailingBlanks (varName);
     Item->Var->name = (char *) cdf_AllocateMemory ((size_t)strlen(varName) + 1,
						FatalError);
     strcpyX (Item->Var->name, varName, 0);
     if (Item->Var->dataType == CDF_EPOCH ||
         Item->Var->dataType == CDF_EPOCH16 ||
         Item->Var->dataType == CDF_TIME_TT2000) {
       if (strcmpIgCase(Item->Var->name, "epoch") == 1 ||
           strcmpIgCase(Item->Var->name, "range_epoch") == 1) {
           if (IsaVirtualVariable(Item->Var->zVar, Item->Var->name)) 
             ++numEpochs;
       }
     }
     /*************************************************************************
     * Load VALIDMIN entry.
     *************************************************************************/
     if (opt.useVALIDMIN) {
       status = CDFlib (SELECT_, ATTR_NAME_, "VALIDMIN",
				 BOO(Item->Var->zVar,
				     zENTRY_,rENTRY_), Item->Var->varN,
			GET_, BOO(Item->Var->zVar,
				  zENTRY_DATATYPE_,
				  rENTRY_DATATYPE_), &eDataType,
			      BOO(Item->Var->zVar,
				  zENTRY_NUMELEMS_,
				  rENTRY_NUMELEMS_), &eNumElems,
			NULL_);
       switch (status) {
	 case CDF_OK: {
	   size_t nBytesE = (size_t) (CDFelemSize(eDataType) * eNumElems);
	   void *buffer = cdf_AllocateMemory (nBytesE, FatalError);
	   status = CDFlib (GET_, BOO(Item->Var->zVar,
				      zENTRY_DATA_,rENTRY_DATA_), buffer,
			    NULL_);
	   DisplayStatus (status, readingCDF);
	   if (StatusBAD(status)) {
	     cdf_FreeMemory (buffer, FatalError);
	     return FALSE;
	   }
	   Item->Var->min = cdf_AllocateMemory ((size_t)Item->Var->nValueBytes,
					    FatalError);
	   ConvertDataType (eDataType, eNumElems, buffer, Item->Var->dataType,
			    Item->Var->numElems, Item->Var->min);
	   cdf_FreeMemory (buffer, FatalError);
	   Item->filterSetting = opt.eachFilter;
	   break;
	 }
	 case NO_SUCH_ATTR:
	 case NO_SUCH_ENTRY:
	   break;
	 default:
	   DisplayStatus (status, readingCDF);
	   return FALSE;
       }
     }
     /*************************************************************************
     * Load VALIDMAX.
     *************************************************************************/
     if (opt.useVALIDMAX) {
       status = CDFlib (SELECT_, ATTR_NAME_, "VALIDMAX",
				 BOO(Item->Var->zVar,
				     zENTRY_,rENTRY_), Item->Var->varN,
			GET_, BOO(Item->Var->zVar,
				  zENTRY_DATATYPE_,
				  rENTRY_DATATYPE_), &eDataType,
			      BOO(Item->Var->zVar,
				  zENTRY_NUMELEMS_,
				  rENTRY_NUMELEMS_), &eNumElems,
			NULL_);
       switch (status) {
	 case CDF_OK: {
	   size_t nBytesE = (size_t) (CDFelemSize(eDataType) * eNumElems);
	   void *buffer = cdf_AllocateMemory (nBytesE, FatalError);
	   status = CDFlib (GET_, BOO(Item->Var->zVar,
				      zENTRY_DATA_,rENTRY_DATA_), buffer,
			    NULL_);
	   DisplayStatus (status, readingCDF);
	   if (StatusBAD(status)) {
	     cdf_FreeMemory (buffer, FatalError);
	     return FALSE;
	   }
	   Item->Var->max = cdf_AllocateMemory ((size_t)Item->Var->nValueBytes,
					    FatalError);
	   ConvertDataType (eDataType, eNumElems, buffer, Item->Var->dataType,
			    Item->Var->numElems, Item->Var->max);
	   cdf_FreeMemory (buffer, FatalError);
	   Item->filterSetting = opt.eachFilter;
	   break;
	 }
	 case NO_SUCH_ATTR:
	 case NO_SUCH_ENTRY:
	   break;
	 default:
	   DisplayStatus (status, readingCDF);
	   return FALSE;
       }
     }
     /*************************************************************************
     * Validate VALIDMIN/VALIDMAX entries.
     *************************************************************************/
     if (Item->Var->min != NULL && Item->Var->max != NULL) {
       if (GTx(Item->Var->min,Item->Var->max,
	       Item->Var->dataType,Item->Var->numElems)) {
	 static char msg[SCREEN_WIDTH+CDF_VAR_NAME_LEN256+1];
	 snprintf (msg, (size_t) sizeof(msg),
		   "VALIDMIN > VALIDMAX for variable `%s' (both ignored).",
		   Item->Var->name);
	 DisplayMessage (msg, NOBEEPWAIT1);
	 cdf_FreeMemory (Item->Var->min, FatalError);
	 cdf_FreeMemory (Item->Var->max, FatalError);
	 Item->Var->min = NULL;
	 Item->Var->max = NULL;
	 Item->filterSetting = FALSE;
       }
     }
     /*************************************************************************
     * Load FILLVAL.
     *************************************************************************/
     if (opt.useFILLVAL) {
       status = CDFlib (SELECT_, ATTR_NAME_, "FILLVAL",
				 BOO(Item->Var->zVar,
				     zENTRY_,rENTRY_), Item->Var->varN,
			GET_, BOO(Item->Var->zVar,
				  zENTRY_DATATYPE_,
				  rENTRY_DATATYPE_), &eDataType,
			      BOO(Item->Var->zVar,
				  zENTRY_NUMELEMS_,
				  rENTRY_NUMELEMS_), &eNumElems,
			NULL_);
       switch (status) {
	 case CDF_OK: {
	   size_t nBytesE = (size_t) (CDFelemSize(eDataType) * eNumElems);
	   void *buffer = cdf_AllocateMemory (nBytesE, FatalError);
	   status = CDFlib (GET_, BOO(Item->Var->zVar,
				      zENTRY_DATA_,rENTRY_DATA_), buffer,
			    NULL_);
	   DisplayStatus (status, readingCDF);
	   if (StatusBAD(status)) {
	     cdf_FreeMemory (buffer, FatalError);
	     return FALSE;
	   }
	   Item->Var->fill = cdf_AllocateMemory ((size_t)Item->Var->nValueBytes,
					     FatalError);
	   ConvertDataType (eDataType, eNumElems, buffer, Item->Var->dataType,
			    Item->Var->numElems, Item->Var->fill);
	   cdf_FreeMemory (buffer, FatalError);
	   break;
	 }
	 case NO_SUCH_ATTR:
	 case NO_SUCH_ENTRY:
	   break;
	 default:
	   DisplayStatus (status, readingCDF);
	   return FALSE;
       }
     }
     /*************************************************************************
     * Load FORMAT entry.
     *************************************************************************/
     if (opt.useFORMAT) {
       status = CDFlib (SELECT_, ATTR_NAME_, "FORMAT",
				 BOO(Item->Var->zVar,
				     zENTRY_,rENTRY_), Item->Var->varN,
			GET_, BOO(Item->Var->zVar,
				  zENTRY_DATATYPE_,
				  rENTRY_DATATYPE_), &eDataType,
			      BOO(Item->Var->zVar,
				  zENTRY_NUMELEMS_,
				  rENTRY_NUMELEMS_), &eNumElems,
			NULL_);
       switch (status) {
	 case CDF_OK: {
	   if (STRINGdataType(eDataType)) {
	     Item->Var->format = (char *)
				 cdf_AllocateMemory ((size_t) eNumElems + 1,
						 FatalError);
	     status = CDFlib (GET_, BOO(Item->Var->zVar,
					zENTRY_DATA_,
					rENTRY_DATA_), Item->Var->format,
			      NULL_);
	     DisplayStatus (status, readingCDF);
	     if (StatusBAD(status)) return FALSE;
             ((char *)Item->Var->format)[(int)eNumElems] = NUL;
	     RemoveWhiteSpace (Item->Var->format);
	   }
	   break;
	 }
	 case NO_SUCH_ATTR:
	 case NO_SUCH_ENTRY:
	   break;
	 default:
	   DisplayStatus (status, readingCDF);
	   return FALSE;
       }
     }
     Item->width = VariableWidth(Item->Var);
     /*************************************************************************
     * Load monotonicity.
     *************************************************************************/
     if (OneDimensionVaries(Item->Var)) {
       Item->Var->monotonic = UNKNOWNmono;
       if (opt.useMONOTON) {
	 status = CDFlib (SELECT_, ATTR_NAME_, "MONOTON",
				   BOO(Item->Var->zVar,
				       zENTRY_,rENTRY_), Item->Var->varN,
			  GET_, BOO(Item->Var->zVar,
				    zENTRY_DATATYPE_,
				    rENTRY_DATATYPE_), &eDataType,
				BOO(Item->Var->zVar,
				    zENTRY_NUMELEMS_,
				    rENTRY_NUMELEMS_), &eNumElems,
			  NULL_);
	 switch (status) {
	   case CDF_OK: {
	     size_t nBytes = (size_t) (CDFelemSize(eDataType) * eNumElems);
	     char *buffer = (char *) cdf_AllocateMemory (nBytes + 1, FatalError);
	     status = CDFlib (GET_, BOO(Item->Var->zVar,
					zENTRY_DATA_,rENTRY_DATA_), buffer,
			      NULL_);
	     DisplayStatus (status, readingCDF);
	     if (StatusBAD(status)) {
	       cdf_FreeMemory (buffer, FatalError);
	       return FALSE;
	     }
	     if (STRINGdataType(eDataType)) {
	       ((char *)buffer)[(int)eNumElems] = NUL;
	       if (!strcmpITB(buffer,"INCREASE"))
		 Item->Var->monotonic = INCREASEmono;
	       else
		 if (!strcmpITB(buffer,"DECREASE"))
		   Item->Var->monotonic = DECREASEmono;
		 else
		   if (!strcmpITB(buffer,"FALSE")) {
		     Item->Var->monotonic=FALSEmono;
		   }
	     }
	     cdf_FreeMemory (buffer, FatalError);
	     break;
	   }
	   case NO_SUCH_ATTR:
	   case NO_SUCH_ENTRY:
	     break;
	   default:
	     DisplayStatus (status, readingCDF);
	     return FALSE;
	 }
       }
     }
     else
       Item->Var->monotonic = NAmono;
     /*************************************************************************
     * Load pad value.
     *************************************************************************/
     Item->Var->pad = cdf_AllocateMemory ((size_t)Item->Var->nValueBytes,
				      FatalError);
     status = CDFlib (GET_, BOO(Item->Var->zVar,
				zVAR_PADVALUE_,
				rVAR_PADVALUE_), Item->Var->pad,
		      NULL_);
     if (status != NO_PADVALUE_SPECIFIED) DisplayStatus (status, readingCDF);
     if (StatusBAD(status)) return FALSE;
     /*************************************************************************
     * Allocate buffer for value(s) and set miscellaneous flags.
     *************************************************************************/
     Item->Var->value = cdf_AllocateMemory ((size_t)Item->Var->nValueBytes,
					FatalError);
     Item->Var->scalar = ScalarVariable (Item->Var);
     /*************************************************************************
     * Tally another line.
     *************************************************************************/
     nItems++;
  }
  if (includeVars > 0L) {
    Logical found;
    for (varX = 0; varX < numVars; ++varX) {
      found = FALSE;
      for (Item = itemHead; Item != NULL; Item = Item->nextItem) {
        if (Item->type != VARIABLEt) continue;
        if (strcmp(Item->Var->name, vars[varX]) == 0) {
          found = TRUE;
          break;
        }
      }
      if (found == FALSE) {
        char msg[80];
        snprintf(msg, (size_t) sizeof(msg),
		 "Variable: %s not found -- ignored", vars[varX]);
        DisplayMessage (msg, NOBEEPWAIT1);
      } else {
        if (includeVars == 1L) ++numVarsO;
        else --numVarsO;
      }
    }
  }
  if (numEpochs > 0) {
    epochs = (char **) malloc(sizeof(char **)*numEpochs);
    epochTypes = (int *) calloc(1, sizeof(int)*numEpochs);
    for (ix = 0; ix < numEpochs; ++ix)
      epochs[ix] = (char *) malloc(CDF_VAR_NAME_LEN256+1);
    ix = 0;
    for (Item = itemHead; Item != NULL; Item = Item->nextItem) {
      if (Item->type != VARIABLEt) continue;
      if (Item->Var->dataType == CDF_EPOCH || 
          Item->Var->dataType == CDF_EPOCH16 ||
          Item->Var->dataType == CDF_TIME_TT2000) {
        if (strcmpIgCase(Item->Var->name, "epoch") == 1 ||
            strcmpIgCase(Item->Var->name, "range_epoch") == 1) {
          if (IsaVirtualVariable(Item->Var->zVar, Item->Var->name)) {
            strcpyX (epochs[ix], Item->Var->name, 0);
            if (Item->Var->dataType == CDF_EPOCH) 
              epochTypes[ix] = 1;
            else if (Item->Var->dataType == CDF_EPOCH16)
              epochTypes[ix] = 2;
            else 
              epochTypes[ix] = 3;
            ++ix;
          }
        }
      }
    }
  }

  /****************************************************************************
  * Inquire majority.
  ****************************************************************************/
  status = CDFlib (GET_, CDF_MAJORITY_, &inMajority,
		   NULL_);
  DisplayStatus (status, "inquiring CDF majority");
  if (StatusBAD(status)) return FALSE;
  /****************************************************************************
  * Allocate SelectionWindow unless in batch mode.
  ****************************************************************************/
  if (!BATCH(batchMode)) {
    AllocIW (IWsel, nSELWINDOWcolsMAX * nItems, nItems, lineL, FatalError);
  }
  return TRUE;
}

/******************************************************************************
* LoadSelectionWindow.
******************************************************************************/

Logical LoadSelectionWindow (part)
int part;
{
  int itemN, lineN;
  char temp[MAXitemFieldLEN+1];
  struct ItemStruct *Item;
  int style;
  /****************************************************************************
  * Load the requested part.
  ****************************************************************************/
  switch (part) {
    /**************************************************************************
    * `Simple' mode.  Name & output.
    **************************************************************************/
    case PART_:
      for (Item = itemHead, lineN = 0;
	   Item != NULL; Item = Item->nextItem, lineN++) {
	 /*********************************************************************
	 * Load item/variable name.
	 *********************************************************************/
	 switch (Item->type) {
	   case RECORDt:
	     MakeNUL (IWsel->iLines[lineN]);
	     CatToString (IWsel->iLines[lineN], "<Record>", NAMEitemLENsimple,
			  LEFT_JUSTIFY, dots);
	     break;
	   case INDICESt:
	     MakeNUL (IWsel->iLines[lineN]);
	     CatToString (IWsel->iLines[lineN], "<Indices>", NAMEitemLENsimple,
			  LEFT_JUSTIFY, dots);
	     break;
	   case VARIABLEt:
	     EncodeString ((long) strlen(Item->Var->name), Item->Var->name,
			   IWsel->iLines[lineN], -NAMEitemLENsimple,
			   NAMEitemLENsimple);
	     break;
	 }
	 itemN = nSELWINDOWcols_ * lineN;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = NAMEitemCOLnSIMPLE;
	 IWsel->iLens[itemN] = NAMEitemLENsimple;
	 /*********************************************************************
	 * Load output.
	 *********************************************************************/
	 strcatX (IWsel->iLines[lineN], " ", 0);
	 CatToString (IWsel->iLines[lineN],
		      BOO(Item->outputSetting,"YES","no"),
		      OUTPUTitemLEN, LEFT_JUSTIFY, dots);
	 itemN++;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = OUTPUTitemCOLnSIMPLE;
	 IWsel->iLens[itemN] = OUTPUTitemLENsimple;
      }
      IWsel->nItems = itemN + 1;
      break;
    /**************************************************************************
    * Part 1.  Name, data specification, dimensionality, variances, output.
    **************************************************************************/
    case PART1:
      for (Item = itemHead, lineN = 0;
	   Item != NULL; Item = Item->nextItem, lineN++) {
	 /*********************************************************************
	 * Load item/variable name.
	 *********************************************************************/
	 switch (Item->type) {
	   case RECORDt:
	     MakeNUL (IWsel->iLines[lineN]);
	     CatToString (IWsel->iLines[lineN], "<Record>", NAMEitemLEN,
			  LEFT_JUSTIFY, dots);
	     break;
	   case INDICESt:
	     MakeNUL (IWsel->iLines[lineN]);
	     CatToString (IWsel->iLines[lineN], "<Indices>", NAMEitemLEN,
			  LEFT_JUSTIFY, dots);
	     break;
	   case VARIABLEt:
	     EncodeString ((long) strlen(Item->Var->name), Item->Var->name,
			   IWsel->iLines[lineN], -NAMEitemLEN, NAMEitemLEN);
	     break;
	 }
	 itemN = nSELWINDOWcols1 * lineN;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = NAMEitemCOLn;
	 IWsel->iLens[itemN] = NAMEitemLEN;
	 /*********************************************************************
	 * Load data specification.
	 *********************************************************************/
	 strcatX (IWsel->iLines[lineN], " ", 0);
	 switch (Item->type) {
	   case RECORDt:
	   case INDICESt:
	     CatToString (IWsel->iLines[lineN], na, SPECitemLEN, LEFT_JUSTIFY,
			  dots);
	     break;
	   case VARIABLEt:
	     snprintf (temp, (size_t) sizeof(temp),
		       "%s/%ld", DataTypeToken(Item->Var->dataType),
		       Item->Var->numElems);
	     CatToString (IWsel->iLines[lineN], temp, SPECitemLEN,
			  LEFT_JUSTIFY, dots);
	     break;
	 }
	 itemN++;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = SPECitemCOLn;
	 IWsel->iLens[itemN] = SPECitemLEN;
	 /*********************************************************************
	 * Load dimensionality.
	 *********************************************************************/
	 strcatX (IWsel->iLines[lineN], " ", 0);
	 switch (Item->type) {
	   case RECORDt:
	   case INDICESt:
	     CatToString (IWsel->iLines[lineN], na, DIMENitemLEN, LEFT_JUSTIFY,
			  dots);
	     break;
	   case VARIABLEt:
	     EncodeDimensionality (temp, Item->Var->numDims,
				   Item->Var->dimSizes, (size_t) sizeof(temp));
	     CatToString (IWsel->iLines[lineN], temp, DIMENitemLEN,
			  LEFT_JUSTIFY, dots);
	     break;
	 }
	 itemN++;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = DIMENitemCOLn;
	 IWsel->iLens[itemN] = DIMENitemLEN;
	 /*********************************************************************
	 * Load variances.
	 *********************************************************************/
	 strcatX (IWsel->iLines[lineN], " ", 0);
	 switch (Item->type) {
	   case RECORDt:
	   case INDICESt:
	     CatToString (IWsel->iLines[lineN], na, VARYSitemLEN, LEFT_JUSTIFY,
			  dots);
	     break;
	   case VARIABLEt:
	     EncodeVariances (temp, Item->Var->recVary, Item->Var->numDims,
			      Item->Var->dimVarys);
	     CatToString (IWsel->iLines[lineN], temp, VARYSitemLEN,
			  LEFT_JUSTIFY, dots);
	     break;
	 }
	 itemN++;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = VARYSitemCOLn;
	 IWsel->iLens[itemN] = VARYSitemLEN;
	 /*********************************************************************
	 * Load output.
	 *********************************************************************/
	 strcatX (IWsel->iLines[lineN], " ", 0);
	 CatToString (IWsel->iLines[lineN],
		      BOO(Item->outputSetting,"YES","no"),
		      OUTPUTitemLEN, LEFT_JUSTIFY, dots);
	 itemN++;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = OUTPUTitemCOLn;
	 IWsel->iLens[itemN] = OUTPUTitemLEN;
      }
      IWsel->nItems = itemN + 1;
      break;
    /**************************************************************************
    * Part 2.  Name, minimum, maximum, filter.
    **************************************************************************/
    case PART2:
      for (Item = itemHead, lineN = 0;
	   Item != NULL; Item = Item->nextItem, lineN++) {
	 /*********************************************************************
	 * Load item/variable name.
	 *********************************************************************/
	 switch (Item->type) {
	   case RECORDt:
	     MakeNUL (IWsel->iLines[lineN]);
	     CatToString (IWsel->iLines[lineN], "<Record>", NAMEitemLEN,
			  LEFT_JUSTIFY, dots);
	     break;
	   case INDICESt:
	     MakeNUL (IWsel->iLines[lineN]);
	     CatToString (IWsel->iLines[lineN], "<Indices>", NAMEitemLEN,
			  LEFT_JUSTIFY, dots);
	     break;
	   case VARIABLEt:
	     EncodeString ((long) strlen(Item->Var->name), Item->Var->name,
			   IWsel->iLines[lineN], -NAMEitemLEN, NAMEitemLEN);
	     break;
	 }
	 itemN = nSELWINDOWcols2 * lineN;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = NAMEitemCOLn;
	 IWsel->iLens[itemN] = NAMEitemLEN;
	 /*********************************************************************
	 * Load minimum value.
	 *********************************************************************/
	 strcatX (IWsel->iLines[lineN], " ", 0);
	 switch (Item->type) {
	   case RECORDt:
	     if (Item->Record->min == NOminMax)
	       MakeNUL (temp);
	     else
	       snprintf (temp, (size_t) sizeof(temp),
		         "%ld", Item->Record->min + 1);
	     CatToString (IWsel->iLines[lineN], temp, MINitemLEN,
			  LEFT_JUSTIFY, dots);
	     break;
	   case INDICESt:
	     if (Item->Indices->minNumDims == NOminMax)
	       CatNcharacters (IWsel->iLines[lineN], MINitemLEN, ' ');
	     else {
	       EncodeIndicesJustify (temp, Item->Indices->minNumDims,
				     Item->Indices->minIndices, 0,
				     (size_t) sizeof(temp));
	       CatToString (IWsel->iLines[lineN], temp, MINitemLEN,
			    LEFT_JUSTIFY, dots);
	     }
	     break;
	   case VARIABLEt:
             if (TT2000dataType(Item->Var->dataType)) style = TT2000_3_STYLE;
             else style = opt.epochStyle;
	     if (Item->Var->min != NULL)
	       EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems,
				   Item->Var->min, EofS(IWsel->iLines[lineN]),
				   NULL, -MINitemLEN, MINitemLEN,
				   style,
				   (size_t) SCREEN_WIDTH-2-strlen(IWsel->iLines[lineN]));
	     else
	       CatNcharacters (IWsel->iLines[lineN], MINitemLEN, ' ');
	     break;
	 }
	 itemN++;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = MINitemCOLn;
	 IWsel->iLens[itemN] = MINitemLEN;
	 /*********************************************************************
	 * Load maximum value.
	 *********************************************************************/
	 strcatX (IWsel->iLines[lineN], " ", 0);
	 switch (Item->type) {
	   case RECORDt:
	     if (Item->Record->max == NOminMax)
	       MakeNUL (temp);
	     else
	       snprintf (temp, (size_t) sizeof(temp),
		         "%ld", Item->Record->max + 1);
	     CatToString (IWsel->iLines[lineN], temp, MAXitemLEN,
			  LEFT_JUSTIFY, dots);
	     break;
	   case INDICESt:
	     if (Item->Indices->maxNumDims == NOminMax)
	       CatNcharacters (IWsel->iLines[lineN], MAXitemLEN, ' ');
	     else {
	       EncodeIndicesJustify (temp, Item->Indices->maxNumDims,
				     Item->Indices->maxIndices, 0,
				     (size_t) sizeof(temp));
	       CatToString (IWsel->iLines[lineN], temp, MAXitemLEN,
			    LEFT_JUSTIFY, dots);
	     }
	     break;
	   case VARIABLEt:
             if (TT2000dataType(Item->Var->dataType)) style = TT2000_3_STYLE;
             else style = opt.epochStyle;
	     if (Item->Var->max != NULL)
	       EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems,
				   Item->Var->max, EofS(IWsel->iLines[lineN]),
				   NULL, -MAXitemLEN, MAXitemLEN,
				   style,
				   (size_t) SCREEN_WIDTH-2-strlen(IWsel->iLines[lineN]));
	     else
	       CatNcharacters (IWsel->iLines[lineN], MAXitemLEN, ' ');
	     break;
	 }
	 itemN++;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = MAXitemCOLn;
	 IWsel->iLens[itemN] = MAXitemLEN;
	 /*********************************************************************
	 * Load filter.
	 *********************************************************************/
	 strcatX (IWsel->iLines[lineN], " ", 0);
	 CatToString (IWsel->iLines[lineN],
		      BOO(Item->filterSetting,
			  BOO(opt.overallFilter,
			      BOO(opt.exclusive,
				  BOO(Item->inclusive,"YeS","yEs"),"YES"),
			      BOO(opt.exclusive,
				  BOO(Item->inclusive,
				      "(YeS)","(yEs)"),"(YES)")),
			  BOO(opt.overallFilter,"no","(no)")),
		      FILTERitemLEN, LEFT_JUSTIFY, dots);
	 itemN++;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = FILTERitemCOLn;
	 IWsel->iLens[itemN] = FILTERitemLEN;
      }
      IWsel->nItems = itemN + 1;
      break;
    /**************************************************************************
    * Part 3.  Name, fill value, monotonicity, format, width.
    **************************************************************************/
    case PART3:
      for (Item = itemHead, lineN = 0;
	   Item != NULL; Item = Item->nextItem, lineN++) {
	 /*********************************************************************
	 * Load item/variable name.
	 *********************************************************************/
	 switch (Item->type) {
	   case RECORDt:
	     MakeNUL (IWsel->iLines[lineN]);
	     CatToString (IWsel->iLines[lineN], "<Record>", NAMEitemLEN,
			  LEFT_JUSTIFY, dots);
	     break;
	   case INDICESt:
	     MakeNUL (IWsel->iLines[lineN]);
	     CatToString (IWsel->iLines[lineN], "<Indices>", NAMEitemLEN,
			  LEFT_JUSTIFY, dots);
	     break;
	   case VARIABLEt:
	     EncodeString ((long) strlen(Item->Var->name), Item->Var->name,
			   IWsel->iLines[lineN], -NAMEitemLEN, NAMEitemLEN);
	     break;
	 }
	 itemN = nSELWINDOWcols3 * lineN;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = NAMEitemCOLn;
	 IWsel->iLens[itemN] = NAMEitemLEN;
	 /*********************************************************************
	 * Load fill value.
	 *********************************************************************/
	 strcatX (IWsel->iLines[lineN], " ", 0);
	 switch (Item->type) {
	   case RECORDt:
	   case INDICESt:
	     CatToString (IWsel->iLines[lineN], na, FILLitemLEN, LEFT_JUSTIFY,
			  dots);
	     break;
	   case VARIABLEt:
             if (TT2000dataType(Item->Var->dataType)) style = TT2000_3_STYLE;
             else style = opt.epochStyle;
	     if (Item->Var->fill != NULL) {
	       char temp[FILLitemLEN+1];
	       MakeNUL (temp);
	       if (!opt.useFills) strcatX (temp, "(", 0);
	       EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems,
				   Item->Var->fill, EofS(temp), NULL, 0,
				   FILLitemLEN - 2, style,
				   (size_t)sizeof(temp)-strlen(temp));
	       if (!opt.useFills) strcatX (temp, ")", 0);
	       CatToString (IWsel->iLines[lineN], temp, FILLitemLEN,
			    LEFT_JUSTIFY, dots);
	     }
	     else
	       CatNcharacters (IWsel->iLines[lineN], FILLitemLEN, ' ');
	     break;
	 }
	 itemN++;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = FILLitemCOLn;
	 IWsel->iLens[itemN] = FILLitemLEN;
	 /*********************************************************************
	 * Load monotonicity.
	 *********************************************************************/
	 strcatX (IWsel->iLines[lineN], " ", 0);
	 switch (Item->type) {
	   case RECORDt:
	   case INDICESt:
	     CatToString (IWsel->iLines[lineN], na, MONOitemLEN, LEFT_JUSTIFY,
			  dots);
	     break;
	   case VARIABLEt:
	     CatToString (IWsel->iLines[lineN], monos[Item->Var->monotonic+1],
			  MONOitemLEN, LEFT_JUSTIFY, dots);
	     break;
	 }
	 itemN++;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = MONOitemCOLn;
	 IWsel->iLens[itemN] = MONOitemLEN;
	 /*********************************************************************
	 * Load format.
	 *********************************************************************/
	 strcatX (IWsel->iLines[lineN], " ", 0);
	 switch (Item->type) {
	   case RECORDt:
	   case INDICESt:
	     CatToString (IWsel->iLines[lineN], na, FORMATitemLEN,
			  LEFT_JUSTIFY, dots);
	     break;
	   case VARIABLEt:
	     if (Item->Var->format != NULL)
	       if (((EPOCHdataType(Item->Var->dataType) ||
                     EPOCH16dataType(Item->Var->dataType) ||
                     TT2000dataType(Item->Var->dataType)) &&
		    (opt.epochStyle == EPOCH0_STYLE ||
		     opt.epochStyle == EPOCH1_STYLE ||
		     opt.epochStyle == EPOCH2_STYLE ||
		     opt.epochStyle == EPOCH3_STYLE ||
		     opt.epochStyle == EPOCH4_STYLE))
		   || STRINGdataType(Item->Var->dataType)) {
		 size_t nBytes = 1 + strlen(Item->Var->format) + 1;
		 char *temp = (char *) cdf_AllocateMemory (nBytes + 1, FatalError);
		 strcpyX (temp, "(", 0);
		 strcatX (temp, Item->Var->format, 0);
		 strcatX (temp, ")", 0);
		 CatToString (IWsel->iLines[lineN], temp, FORMATitemLEN,
			      LEFT_JUSTIFY, dots);
		 cdf_FreeMemory (temp, FatalError);
	       }
	       else
		 CatToString (IWsel->iLines[lineN], Item->Var->format,
			      FORMATitemLEN, LEFT_JUSTIFY, dots);
	     else
	       CatNcharacters (IWsel->iLines[lineN], FORMATitemLEN, ' ');
	     break;
	 }
	 itemN++;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = FORMATitemCOLn;
	 IWsel->iLens[itemN] = FORMATitemLEN;
	 /*********************************************************************
	 * Load width.
	 *********************************************************************/
	 strcatX (IWsel->iLines[lineN], " ", 0);
	 snprintf (temp, (size_t) sizeof(temp), "%d", Item->width);
	 CatToString (IWsel->iLines[lineN], temp, WIDTHitemLEN, LEFT_JUSTIFY,
		      dots);
	 itemN++;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = WIDTHitemCOLn;
	 IWsel->iLens[itemN] = WIDTHitemLEN;
      }
      IWsel->nItems = itemN + 1;
      break;
    /**************************************************************************
    * Part 4.  Name, sparseness, compression, blocking factor.
    **************************************************************************/
    case PART4:
      for (Item = itemHead, lineN = 0;
	   Item != NULL; Item = Item->nextItem, lineN++) {
	 /*********************************************************************
	 * Load item/variable name.
	 *********************************************************************/
	 switch (Item->type) {
	   case RECORDt:
	     MakeNUL (IWsel->iLines[lineN]);
	     CatToString (IWsel->iLines[lineN], "<Record>", NAMEitemLEN,
			  LEFT_JUSTIFY, dots);
	     break;
	   case INDICESt:
	     MakeNUL (IWsel->iLines[lineN]);
	     CatToString (IWsel->iLines[lineN], "<Indices>", NAMEitemLEN,
			  LEFT_JUSTIFY, dots);
	     break;
	   case VARIABLEt:
	     EncodeString ((long) strlen(Item->Var->name), Item->Var->name,
			   IWsel->iLines[lineN], -NAMEitemLEN, NAMEitemLEN);
	     break;
	 }
	 itemN = nSELWINDOWcols4 * lineN;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = NAMEitemCOLn;
	 IWsel->iLens[itemN] = NAMEitemLEN;
	 /*********************************************************************
	 * Load sparseness.
	 *********************************************************************/
	 strcatX (IWsel->iLines[lineN], " ", 0);
	 switch (Item->type) {
	   case RECORDt:
	   case INDICESt:
	     CatToString (IWsel->iLines[lineN], na, SPARSEitemLEN,
			  LEFT_JUSTIFY, dots);
	     break;
	   case VARIABLEt:
	     CatToString (IWsel->iLines[lineN],
			  SparsenessToken(Item->Var->sRecordsType,
					  Item->Var->sArraysType,
					  Item->Var->sArraysParms),
			  SPARSEitemLEN, LEFT_JUSTIFY, dots);
	     break;
	 }
	 itemN++;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = SPARSEitemCOLn;
	 IWsel->iLens[itemN] = SPARSEitemLEN;
	 /*********************************************************************
	 * Load compression.
	 *********************************************************************/
	 strcatX (IWsel->iLines[lineN], " ", 0);
	 switch (Item->type) {
	   case RECORDt:
	   case INDICESt:
	     CatToString (IWsel->iLines[lineN], na, COMPRESSitemLEN,
			  LEFT_JUSTIFY, dots);
	     break;
	   case VARIABLEt:
	     CatToString (IWsel->iLines[lineN],
			  CompressionToken(Item->Var->cType,Item->Var->cParms),
			  COMPRESSitemLEN, LEFT_JUSTIFY, dots);
	     break;
	 }
	 itemN++;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = COMPRESSitemCOLn;
	 IWsel->iLens[itemN] = COMPRESSitemLEN;
	 /*********************************************************************
	 * Load reserve percentage.
	 *********************************************************************/
	 strcatX (IWsel->iLines[lineN], " ", 0);
	 switch (Item->type) {
	   case RECORDt:
	   case INDICESt:
	     CatToString (IWsel->iLines[lineN], na, RESERVEitemLEN,
			  LEFT_JUSTIFY, dots);
	     break;
	   case VARIABLEt:
	     if (Item->Var->reserve == NA_RESERVE)
	       CatToString (IWsel->iLines[lineN], na, RESERVEitemLEN,
			    LEFT_JUSTIFY, dots);
	     else {
	       snprintf (temp, (size_t) sizeof(temp),
		         "%ld", Item->Var->reserve);
	       CatToString (IWsel->iLines[lineN], temp, RESERVEitemLEN,
			    LEFT_JUSTIFY, dots);
	     }
	     break;
	 }
	 itemN++;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = RESERVEitemCOLn;
	 IWsel->iLens[itemN] = RESERVEitemLEN;
	 /*********************************************************************
	 * Load blocking factor.
	 *********************************************************************/
	 strcatX (IWsel->iLines[lineN], " ", 0);
	 switch (Item->type) {
	   case RECORDt:
	   case INDICESt:
	     CatToString (IWsel->iLines[lineN], na, BLOCKINGitemLEN,
			  LEFT_JUSTIFY, dots);
	     break;
	   case VARIABLEt:
	     snprintf (temp, (size_t) sizeof(temp),
		       "%ld", Item->Var->blocking);
	     CatToString (IWsel->iLines[lineN], temp, BLOCKINGitemLEN,
			  LEFT_JUSTIFY, dots);
	     break;
	 }
	 itemN++;
	 IWsel->iLineNs[itemN] = lineN;
	 IWsel->iCols[itemN] = BLOCKINGitemCOLn;
	 IWsel->iLens[itemN] = BLOCKINGitemLEN;
      }
      IWsel->nItems = itemN + 1;
      break;
  }
  /****************************************************************************
  * Success, return.
  ****************************************************************************/
  return TRUE;
}

/******************************************************************************
* SelectionWindow.
* This is used when `simple' mode has been selected.
******************************************************************************/

void SelectionWindow (noMoreAccess)
Logical *noMoreAccess;  /* Assumed initialized to FALSE by caller. */
{
  static Logical first = TRUE;
  int rX, cX, rXt; struct ItemStruct *Item;
  static char label[] = " SelectionWindow ";
  static char *header[] = {
    "Variable                                                                Output"
  };
  static int exits[] = {
    ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, SCREENkey_SIMPLE, FILEkey_SIMPLE,
    SAVEkey_SIMPLE, RESTOREkey_SIMPLE, NUL
  };
  static char keyDefs[] = "ToScreen: ________    SaveSettings: ________ Help: ________ YES/no:  ________\n  ToFile: ________ RestoreSettings: ________ Exit: ________";
  /****************************************************************************
  * Encode key definitions first time.
  ****************************************************************************/
  if (first) {
    char *p1 = keyDefs;
    EncodeKeyDefinitions (1, &p1, SCREENkey_SIMPLE, SAVEkey_SIMPLE,
			  HELPkey_FSI, ENTERkey_FSI, FILEkey_SIMPLE,
			  RESTOREkey_SIMPLE, EXITkey_FSI);
    first = FALSE;
  }
  /****************************************************************************
  * Load/update SelectionWindow.
  ****************************************************************************/
  IWsel->label = label;
  IWsel->hLines = header;
  IWsel->exitChars = exits;
  LoadSelectionWindow (PART_);
  ItemWindow (UPDATEiw, IWsel, 1);
  /****************************************************************************
  * Update/load KeyWindow.
  ****************************************************************************/
  NEWkeyDEFS (EWkey, keyDefs, batchMode)
  /****************************************************************************
  * Process keystrokes.
  ****************************************************************************/
  for (;;) {
     ItemWindow (READiw, IWsel);
     rX = IWsel->itemN / nSELWINDOWcols_;
     cX = IWsel->itemN % nSELWINDOWcols_;
     for (Item = itemHead, rXt = 0; rXt < rX; rXt++) Item = Item->nextItem;
     switch (IWsel->key) {
       case ENTERkey_FSI:
	 switch (cX) {
	   case NAMEcxSIMPLE:
	     ItemWindow (BEEPiw, IWsel);
	     break;
	   case OUTPUTcxSIMPLE:
	     Item->outputSetting = BOO(Item->outputSetting,FALSE,TRUE);
	     LoadSelectionWindow (PART_);
	     ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
	     break;
	 }
	 break;
       case SCREENkey_SIMPLE:
	 if (!BOO(opt.horizontalMode,ToScreenHori(),ToScreenVert())) {
	   *noMoreAccess = TRUE;
	   return;
	 }
	 break;
       case FILEkey_SIMPLE:
	 if (!BOO(opt.horizontalMode,ToFileHori(),ToFileVert())) {
	   *noMoreAccess = TRUE;
	   return;
	 }
	 break;
       case SAVEkey_SIMPLE:
	 if (PromptFor(settingsFile,DU_MAX_PATH_LEN,strlen(settingsFile),
		       "Enter path for settings file...",SETFILEhelpID)) {
	   DisplayMessage ("Saving settings...", NOWAIT);
	   SaveSettings ();
	   DisplayMessage ("", NOWAIT);
	 }
	 break;
       case RESTOREkey_SIMPLE:
	 if (PromptFor(settingsFile,DU_MAX_PATH_LEN,strlen(settingsFile),
		       "Enter path for settings file...",SETFILEhelpID)) {
	   DisplayMessage ("Restoring settings...", NOWAIT);
	   RestoreSettings ();
	   LoadSelectionWindow (PART_);
	   ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
	   DisplayMessage ("", NOWAIT);
	 }
	 break;
       case HELPkey_FSI:
	 OnlineHelpWindow ("cdfxp.ilh", VARhelpID);
	 break;
       case EXITkey_FSI:
	 if (ConfirmExit()) return;
	 break;
     }
     /*************************************************************************
     * Update key definitions in case they were changed.
     *************************************************************************/
     NEWkeyDEFS (EWkey, keyDefs, batchMode)
  }
}

/******************************************************************************
* SelectionWindow1.
* Returns FALSE when the CDF is to be exited (either by choice or because of
* a fatal CDF error).
******************************************************************************/

Logical SelectionWindow1 (noMoreAccess)
Logical *noMoreAccess;  /* Assumed initialized to FALSE by caller. */
{
  static Logical first = TRUE;
  int rX, cX, rXt, itemN; struct ItemStruct *Item;
  static char label[] = " SelectionWindow, part 1 ";
  static char *header[] = {
    "Item/Variable         DataSpec  Dimensionality              Variances   Output"
  };
  static int exits[] = {
    PART2key_EXPORT, ENTERkey_FSI, EXITkey_FSI, ACTIONSkey_EXPORT,
    FLIPkey_EXPORT, DELETEkey_FSI, HELPkey_FSI, OPTIONSkey_EXPORT, NUL
  };
  static char keyDefs[] = "Modify: ________    Actions: ________     Reverse: ________     Help: ________\nPart 2: ________    Options: ________     Delete:  ________     Exit: ________\n";
  /****************************************************************************
  * Encode key definitions first time.
  ****************************************************************************/
  if (first) {
    char *p1 = keyDefs;
    EncodeKeyDefinitions (1, &p1, ENTERkey_FSI, ACTIONSkey_EXPORT,
			  FLIPkey_EXPORT, HELPkey_FSI, PART2key_EXPORT,
			  OPTIONSkey_EXPORT, DELETEkey_FSI, EXITkey_FSI);
    first = FALSE;
  }
  /****************************************************************************
  * Load/update SelectionWindow.
  ****************************************************************************/
  IWsel->label = label;
  IWsel->hLines = header;
  IWsel->exitChars = exits;
  LoadSelectionWindow (PART1);
  itemN = (IWsel->itemN / nSELWINDOWcols4) * nSELWINDOWcols1;
  switch (IWsel->itemN % nSELWINDOWcols4) {
    case NAMEcx: break;
    case SPARSEcx: itemN += SPECcx; break;
    case COMPRESScx: itemN += VARYScx; break;
    case BLOCKINGcx: itemN += OUTPUTcx; break;
  }
  ItemWindow (UPDATEiw, IWsel, itemN);
  /****************************************************************************
  * Update/load KeyWindow.
  ****************************************************************************/
  NEWkeyDEFS (EWkey, keyDefs, batchMode)
  /****************************************************************************
  * Process keystrokes.
  ****************************************************************************/
  for (;;) {
     ItemWindow (READiw, IWsel);
     rX = IWsel->itemN / nSELWINDOWcols1;
     cX = IWsel->itemN % nSELWINDOWcols1;
     for (Item = itemHead, rXt = 0; rXt < rX; rXt++) Item = Item->nextItem;
     switch (IWsel->key) {
       case ENTERkey_FSI:
	 switch (cX) {
	   case NAMEcx:
	   case SPECcx:
	   case DIMENcx:
	   case VARYScx:
	     ItemWindow (BEEPiw, IWsel);
	     break;
	   case OUTPUTcx:
	     Item->outputSetting = BOO(Item->outputSetting,FALSE,TRUE);
	     LoadSelectionWindow (PART1);
	     ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
	     break;
	 }
	 break;
       case DELETEkey_FSI:
	 switch (cX) {
	   case NAMEcx:
	     ItemWindow (BEEPiw, IWsel);
	     break;
	   case SPECcx:
	   case DIMENcx:
	   case VARYScx:
	   case OUTPUTcx:
	     ItemWindow (BEEPiw, IWsel);
	     break;
	 }
	 break;
       case FLIPkey_EXPORT:
	 if (FlipItems(rX)) {
	   int itemN = (IWsel->itemN + nSELWINDOWcols1) % IWsel->nItems;
	   LoadSelectionWindow (PART1);
	   ItemWindow (UPDATEiw, IWsel, itemN);
	 }
	 else
	   ItemWindow (BEEPiw, IWsel);
	 break;
       case ACTIONSkey_EXPORT:
	 if (!ActionMenu(PART1)) {
	   *noMoreAccess = TRUE;
	   return FALSE;
	 }
	 break;
       case OPTIONSkey_EXPORT:
	 OptionMenu ();
	 LoadSelectionWindow (PART1);
	 ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
	 break;
       case HELPkey_FSI:
	 OnlineHelpWindow ("cdfxp.ilh", VAR1helpID);
	 break;
       case PART2key_EXPORT:
	 return TRUE;
       case EXITkey_FSI:
	 if (ConfirmExit()) return FALSE;
	 break;
     }
     /*************************************************************************
     * Update key definitions in case they were changed.
     *************************************************************************/
     NEWkeyDEFS (EWkey, keyDefs, batchMode)
  }
}

/******************************************************************************
* SelectionWindow2.
* Returns FALSE when the CDF is to be exited (either by choice or because of
* a fatal CDF error).
******************************************************************************/

Logical SelectionWindow2 (noMoreAccess)
Logical *noMoreAccess;
{
  static Logical first = TRUE;
  int rX, cX, rXt, itemN; struct ItemStruct *Item;
  static char label[] = " SelectionWindow, part 2 ";
  static char *header[] = {
    "Item/Variable         Minimum                  Maximum                  Filter"
  };
  static int exits[] = {
    PART3key_EXPORT, ENTERkey_FSI, EXITkey_FSI, ACTIONSkey_EXPORT,
    FLIPkey_EXPORT, DELETEkey_FSI, HELPkey_FSI, OPTIONSkey_EXPORT, NUL
  };
  static char keyDefs[] = "Modify: ________    Actions: ________     Reverse: ________     Help: ________\nPart 3: ________    Options: ________     Delete:  ________     Exit: ________\n";
  /****************************************************************************
  * Encode key definitions first time.
  ****************************************************************************/
  if (first) {
    char *p1 = keyDefs;
    EncodeKeyDefinitions (1, &p1, ENTERkey_FSI, ACTIONSkey_EXPORT,
			  FLIPkey_EXPORT, HELPkey_FSI, PART2key_EXPORT,
			  OPTIONSkey_EXPORT, DELETEkey_FSI, EXITkey_FSI);
    first = FALSE;
  }
  /****************************************************************************
  * Load/update SelectionWindow.
  ****************************************************************************/
  IWsel->label = label;
  IWsel->hLines = header;
  IWsel->exitChars = exits;
  LoadSelectionWindow (PART2);
  itemN = (IWsel->itemN / nSELWINDOWcols1) * nSELWINDOWcols2;
  switch (IWsel->itemN % nSELWINDOWcols1) {
    case NAMEcx: break;
    case SPECcx: itemN += MINcx; break;
    case DIMENcx: itemN += MINcx; break;
    case VARYScx: itemN += MAXcx; break;
    case OUTPUTcx: itemN += FILTERcx; break;
  }
  ItemWindow (UPDATEiw, IWsel, itemN);
  /****************************************************************************
  * Load/update KeyWindow.
  ****************************************************************************/
  NEWkeyDEFS (EWkey, keyDefs, batchMode)
  /****************************************************************************
  * Process keystrokes.
  ****************************************************************************/
  for (;;) {
     ItemWindow (READiw, IWsel);
     rX = IWsel->itemN / nSELWINDOWcols2;
     cX = IWsel->itemN % nSELWINDOWcols2;
     for (Item = itemHead, rXt = 0; rXt < rX; rXt++) Item = Item->nextItem;
     switch (IWsel->key) {
       case ENTERkey_FSI:
	 switch (cX) {
	   case NAMEcx:
	     ItemWindow (BEEPiw, IWsel);
	     break;
	   case MINcx:
	   case MAXcx: {
	     switch (Item->type) {
	       case RECORDt:
	       case INDICESt:
	       case VARIABLEt:
		 if (PromptForMinMax(Item,(cX == MINcx))) {
		   LoadSelectionWindow (PART2);
		   ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
		 }
		 break;
	     }
	     break;
	   case FILTERcx:
	     if (MINorMAXexists(Item)) {
	       if (opt.exclusive) {
		 if (Item->filterSetting) {
		   if (Item->inclusive)
		     Item->inclusive = FALSE;
		   else
		     Item->filterSetting = FALSE;
		 }
		 else {
		   Item->filterSetting = TRUE;
		   Item->inclusive = TRUE;
		 }
	       }
	       else
		 Item->filterSetting = BOO(Item->filterSetting,FALSE,TRUE);
	       opt.overallFilter = TRUE;
	       LoadSelectionWindow (PART2);
	       ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
	     }
	     else
	       ItemWindow (BEEPiw, IWsel);
	     break;
	   }
	 }
	 break;
       case DELETEkey_FSI:
	 switch (cX) {
	   case NAMEcx:
	     ItemWindow (BEEPiw, IWsel);
	     break;
	   case MINcx:
	     switch (Item->type) {
	       case RECORDt:
		 if (Item->Record->min != NOminMax) {
		   Item->Record->min = NOminMax;
		   if (Item->Record->max == NOminMax) {
		     Item->filterSetting = FALSE;
		   }
		   LoadSelectionWindow (PART2);
		   ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
		 }
		 else
		   ItemWindow (BEEPiw, IWsel);
		 break;
	       case INDICESt:
		 if (Item->Indices->minNumDims != NOminMax) {
		   Item->Indices->minNumDims = NOminMax;
		   if (Item->Indices->maxNumDims == NOminMax) {
		     Item->filterSetting = FALSE;
		   }
		   LoadSelectionWindow (PART2);
		   ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
		 }
		 else
		   ItemWindow (BEEPiw, IWsel);
		 break;
	       case VARIABLEt:
		 if (Item->Var->min != NULL) {
		   cdf_FreeMemory (Item->Var->min, FatalError);
		   Item->Var->min = NULL;
		   if (Item->Var->max == NULL) Item->filterSetting = FALSE;
		   LoadSelectionWindow (PART2);
		   ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
		 }
		 else
		   ItemWindow (BEEPiw, IWsel);
		 break;
	     }
	     break;
	   case MAXcx:
	     switch (Item->type) {
	       case RECORDt:
		 if (Item->Record->max != NOminMax) {
		   Item->Record->max = NOminMax;
		   if (Item->Record->min == NOminMax) {
		     Item->filterSetting = FALSE;
		   }
		   LoadSelectionWindow (PART2);
		   ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
		 }
		 else
		   ItemWindow (BEEPiw, IWsel);
		 break;
	       case INDICESt:
		 if (Item->Indices->maxNumDims != NOminMax) {
		   Item->Indices->maxNumDims = NOminMax;
		   if (Item->Indices->minNumDims == NOminMax) {
		     Item->filterSetting = FALSE;
		   }
		   LoadSelectionWindow (PART2);
		   ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
		 }
		 else
		   ItemWindow (BEEPiw, IWsel);
		 break;
	       case VARIABLEt:
		 if (Item->Var->max != NULL) {
		   cdf_FreeMemory (Item->Var->max, FatalError);
		   Item->Var->max = NULL;
		   if (Item->Var->min == NULL) Item->filterSetting = FALSE;
		   LoadSelectionWindow (PART2);
		   ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
		 }
		 else
		   ItemWindow (BEEPiw, IWsel);
		 break;
	     }
	     break;
	   case FILTERcx:
	     ItemWindow (BEEPiw, IWsel);
	     break;
	 }
	 break;
       case FLIPkey_EXPORT:
	 if (FlipItems(rX)) {
	   int itemN = (IWsel->itemN + nSELWINDOWcols2) % IWsel->nItems;
	   LoadSelectionWindow (PART2);
	   ItemWindow (UPDATEiw, IWsel, itemN);
	 }
	 else
	   ItemWindow (BEEPiw, IWsel);
	 break;
       case ACTIONSkey_EXPORT:
	 if (!ActionMenu(PART2)) {
	   *noMoreAccess = TRUE;
	   return FALSE;
	 }
	 break;
       case OPTIONSkey_EXPORT:
	 OptionMenu ();
	 LoadSelectionWindow (PART2);
	 ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
	 break;
       case HELPkey_FSI:
	 OnlineHelpWindow ("cdfxp.ilh", VAR2helpID);
	 break;
       case PART3key_EXPORT:
	 return TRUE;
       case EXITkey_FSI:
	 if (ConfirmExit()) return FALSE;
	 break;
     }
     /*************************************************************************
     * Update key definitions in case they were changed.
     *************************************************************************/
     NEWkeyDEFS (EWkey, keyDefs, batchMode)
  }
}

/******************************************************************************
* SelectionWindow3.
* Returns FALSE when the CDF is to be exited (either by choice or because of
* a fatal CDF error).
******************************************************************************/

Logical SelectionWindow3 (noMoreAccess)
Logical *noMoreAccess;
{
  static Logical first = TRUE;
  int rX, cX, rXt, itemN; struct ItemStruct *Item;
  static char label[] = " SelectionWindow, part 3 ";
  static char *header[] = {
    "Item/Variable         FillValue                  Monotonicity  Format    Width"
  };
  static int exits[] = {
    PART4key_EXPORT, ENTERkey_FSI, EXITkey_FSI, ACTIONSkey_EXPORT,
    FLIPkey_EXPORT, DELETEkey_FSI, HELPkey_FSI, OPTIONSkey_EXPORT, NUL
  };
  static char keyDefs[] = "Modify: ________    Actions: ________     Reverse: ________     Help: ________\nPart 4: ________    Options: ________     Delete:  ________     Exit: ________\n";
  /****************************************************************************
  * Encode key definitions first time.
  ****************************************************************************/
  if (first) {
    char *p1 = keyDefs;
    EncodeKeyDefinitions (1, &p1, ENTERkey_FSI, ACTIONSkey_EXPORT,
			  FLIPkey_EXPORT, HELPkey_FSI, PART4key_EXPORT,
			  OPTIONSkey_EXPORT, DELETEkey_FSI, EXITkey_FSI);
    first = FALSE;
  }
  /****************************************************************************
  * Load/update SelectionWindow.
  ****************************************************************************/
  IWsel->label = label;
  IWsel->hLines = header;
  IWsel->exitChars = exits;
  LoadSelectionWindow (PART3);
  itemN = (IWsel->itemN / nSELWINDOWcols2) * nSELWINDOWcols3;
  switch (IWsel->itemN % nSELWINDOWcols2) {
    case NAMEcx: break;
    case MINcx: itemN += FILLcx; break;
    case MAXcx: itemN += MONOcx; break;
    case FILTERcx: itemN += WIDTHcx; break;
  }
  ItemWindow (UPDATEiw, IWsel, itemN);
  /****************************************************************************
  * Load/update KeyWindow.
  ****************************************************************************/
  NEWkeyDEFS (EWkey, keyDefs, batchMode)
  /****************************************************************************
  * Process keystrokes.
  ****************************************************************************/
  for (;;) {
     ItemWindow (READiw, IWsel);
     rX = IWsel->itemN / nSELWINDOWcols3;
     cX = IWsel->itemN % nSELWINDOWcols3;
     for (Item = itemHead, rXt = 0; rXt < rX; rXt++) Item = Item->nextItem;
     switch (IWsel->key) {
       case ENTERkey_FSI:
	 switch (cX) {
	   case NAMEcx:
	     ItemWindow (BEEPiw, IWsel);
	     break;
	   case FILLcx:
	     switch (Item->type) {
	       case RECORDt:
	       case INDICESt:
		 ItemWindow (BEEPiw, IWsel);
		 break;
	       case VARIABLEt:
		 if (PromptForFill(Item->Var)) {
		   LoadSelectionWindow (PART3);
		   ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
		 }
		 break;
	     }
	     break;
	   case WIDTHcx: {
	     if (PromptForWidth(Item)) {
	       LoadSelectionWindow (PART3);
	       ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
	     }
	     break;
	   }
	   case MONOcx:
	     switch (Item->type) {
	       case RECORDt:
	       case INDICESt:
		 ItemWindow (BEEPiw, IWsel);
		 break;
	       case VARIABLEt:
		 if (Item->Var->monotonic != NAmono) {
		   Item->Var->monotonic = (Item->Var->monotonic + 1) % 4;
		   LoadSelectionWindow (PART3);
		   ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
		 }
		 else
		   ItemWindow (BEEPiw, IWsel);
		 break;
	     }
	     break;
	   case FORMATcx:
	     switch (Item->type) {
	       case RECORDt:
	       case INDICESt:
		 ItemWindow (BEEPiw, IWsel);
		 break;
	       case VARIABLEt:
		 if (PromptForFormat(Item)) {
		   LoadSelectionWindow (PART3);
		   ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
		 }
		 break;
	     }
	     break;
	 }
	 break;
       case DELETEkey_FSI:
	 switch (cX) {
	   case NAMEcx:
	     ItemWindow (BEEPiw, IWsel);
	     break;
	   case FILLcx:
	     switch (Item->type) {
	       case RECORDt:
	       case INDICESt:
		 ItemWindow (BEEPiw, IWsel);
		 break;
	       case VARIABLEt:
		 if (Item->Var->fill != NULL) {
		   cdf_FreeMemory (Item->Var->fill, FatalError);
		   Item->Var->fill = NULL;
		   LoadSelectionWindow (PART3);
		   ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
		 }
		 else
		   ItemWindow (BEEPiw, IWsel);
		 break;
	     }
	     break;
	   case WIDTHcx:
	   case MONOcx:
	     ItemWindow (BEEPiw, IWsel);
	     break;
	   case FORMATcx:
	     switch (Item->type) {
	       case RECORDt:
	       case INDICESt:
		 ItemWindow (BEEPiw, IWsel);
		 break;
	       case VARIABLEt:
		 if (Item->Var->format != NULL) {
		   cdf_FreeMemory (Item->Var->format, FatalError);
		   Item->Var->format = NULL;
		   LoadSelectionWindow (PART3);
		   ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
		 }
		 else
		   ItemWindow (BEEPiw, IWsel);
		 break;
	     }
	     break;
	 }
	 break;
       case FLIPkey_EXPORT:
	 if (FlipItems(rX)) {
	   int itemN = (IWsel->itemN + nSELWINDOWcols3) % IWsel->nItems;
	   LoadSelectionWindow (PART3);
	   ItemWindow (UPDATEiw, IWsel, itemN);
	 }
	 else
	   ItemWindow (BEEPiw, IWsel);
	 break;
       case ACTIONSkey_EXPORT:
	 if (!ActionMenu(PART3)) {
	   *noMoreAccess = TRUE;
	   return FALSE;
	 }
	 break;
       case OPTIONSkey_EXPORT:
	 OptionMenu ();
	 LoadSelectionWindow (PART3);
	 ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
	 break;
       case HELPkey_FSI:
	 OnlineHelpWindow ("cdfxp.ilh", VAR3helpID);
	 break;
       case PART4key_EXPORT:
	 return TRUE;
       case EXITkey_FSI:
	 if (ConfirmExit()) return FALSE;
	 break;
     }
     /*************************************************************************
     * Update key definitions in case they were changed.
     *************************************************************************/
     NEWkeyDEFS (EWkey, keyDefs, batchMode)
  }
}

/******************************************************************************
* SelectionWindow4.
* Returns FALSE when the CDF is to be exited (either by choice or because of
* a fatal CDF error).
******************************************************************************/

Logical SelectionWindow4 (noMoreAccess)
Logical *noMoreAccess;
{
  static Logical first = TRUE;
  int rX, cX, rXt, itemN; struct ItemStruct *Item;
  static char label[] = " SelectionWindow, part 4 ";
  static char *header[] = {
    "Item/Variable         Sparseness          Compression         Reserve Blocking"
  };
  static int exits[] = {
    PART1key_EXPORT, ENTERkey_FSI, EXITkey_FSI, ACTIONSkey_EXPORT,
    FLIPkey_EXPORT, DELETEkey_FSI, HELPkey_FSI, OPTIONSkey_EXPORT, NUL
  };
  static char keyDefs[] = "Modify: ________    Actions: ________     Reverse: ________     Help: ________\nPart 1: ________    Options: ________     Delete:  ________     Exit: ________\n";
  /****************************************************************************
  * Encode key definitions first time.
  ****************************************************************************/
  if (first) {
    char *p1 = keyDefs;
    EncodeKeyDefinitions (1, &p1, ENTERkey_FSI, ACTIONSkey_EXPORT,
			  FLIPkey_EXPORT, HELPkey_FSI, PART1key_EXPORT,
			  OPTIONSkey_EXPORT, DELETEkey_FSI, EXITkey_FSI);
    first = FALSE;
  }
  /****************************************************************************
  * Load/update SelectionWindow.
  ****************************************************************************/
  IWsel->label = label;
  IWsel->hLines = header;
  IWsel->exitChars = exits;
  LoadSelectionWindow (PART4);
  itemN = (IWsel->itemN / nSELWINDOWcols3) * nSELWINDOWcols4;
  switch (IWsel->itemN % nSELWINDOWcols3) {
    case NAMEcx: break;
    case FILLcx: itemN += SPARSEcx; break;
    case MONOcx: itemN += COMPRESScx; break;
    case FORMATcx: itemN += RESERVEcx; break;
    case WIDTHcx: itemN += BLOCKINGcx; break;
  }
  ItemWindow (UPDATEiw, IWsel, itemN);
  /****************************************************************************
  * Load/update KeyWindow.
  ****************************************************************************/
  NEWkeyDEFS (EWkey, keyDefs, batchMode)
  /****************************************************************************
  * Process keystrokes.
  ****************************************************************************/
  for (;;) {
     ItemWindow (READiw, IWsel);
     rX = IWsel->itemN / nSELWINDOWcols4;
     cX = IWsel->itemN % nSELWINDOWcols4;
     for (Item = itemHead, rXt = 0; rXt < rX; rXt++) Item = Item->nextItem;
     switch (IWsel->key) {
       case ENTERkey_FSI:
	 switch (cX) {
	   case NAMEcx:
	     ItemWindow (BEEPiw, IWsel);
	     break;
	   case SPARSEcx:
	     switch (Item->type) {
	       case RECORDt:
	       case INDICESt:
		 ItemWindow (BEEPiw, IWsel);
		 break;
	       case VARIABLEt:
		 if (PromptForSparseness(Item->Var)) {
		   LoadSelectionWindow (PART4);
		   ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
		 }
		 break;
	     }
	     break;
	   case COMPRESScx:
	     switch (Item->type) {
	       case RECORDt:
	       case INDICESt:
		 ItemWindow (BEEPiw, IWsel);
		 break;
	       case VARIABLEt: {
		 long cType, cParms[CDF_MAX_PARMS]; int parmN;
		 if (PromptForCompression(&cType,cParms)) {
		   Item->Var->cType = cType;
		   for (parmN = 0; parmN < CDF_MAX_PARMS; parmN++) {
		      Item->Var->cParms[parmN] = cParms[parmN];
		   }
		   Item->Var->reserve = BOO(Item->Var->cType == NO_COMPRESSION,
					    NA_RESERVE,0);
		   LoadSelectionWindow (PART4);
		   ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
		 }
		 break;
	       }
	     }
	     break;
	   case RESERVEcx:
	     switch (Item->type) {
	       case RECORDt:
	       case INDICESt:
		 ItemWindow (BEEPiw, IWsel);
		 break;
	       case VARIABLEt:
		 if (Item->Var->reserve != NA_RESERVE) {
		   if (PromptForReserve(Item->Var)) {
		     LoadSelectionWindow (PART4);
		     ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
		   }
		 }
		 else
		   ItemWindow (BEEPiw, IWsel);
		 break;
	     }
	     break;
	   case BLOCKINGcx:
	     switch (Item->type) {
	       case RECORDt:
	       case INDICESt:
		 ItemWindow (BEEPiw, IWsel);
		 break;
	       case VARIABLEt:
		 if (PromptForBlocking(Item->Var)) {
		   LoadSelectionWindow (PART4);
		   ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
		 }
		 break;
	     }
	     break;
	 }
	 break;
       case DELETEkey_FSI:
	 switch (cX) {
	   case NAMEcx:
	   case SPARSEcx:
	   case COMPRESScx:
	   case RESERVEcx:
	   case BLOCKINGcx:
	     ItemWindow (BEEPiw, IWsel);
	     break;
	 }
	 break;
       case FLIPkey_EXPORT:
	 if (FlipItems(rX)) {
	   int itemN = (IWsel->itemN + nSELWINDOWcols4) % IWsel->nItems;
	   LoadSelectionWindow (PART4);
	   ItemWindow (UPDATEiw, IWsel, itemN);
	 }
	 else
	   ItemWindow (BEEPiw, IWsel);
	 break;
       case ACTIONSkey_EXPORT:
	 if (!ActionMenu(PART4)) {
	   *noMoreAccess = TRUE;
	   return FALSE;
	 }
	 break;
       case OPTIONSkey_EXPORT:
	 OptionMenu ();
	 LoadSelectionWindow (PART4);
	 ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
	 break;
       case HELPkey_FSI:
	 OnlineHelpWindow ("cdfxp.ilh", VAR4helpID);
	 break;
       case PART1key_EXPORT:
	 return TRUE;
       case EXITkey_FSI:
	 if (ConfirmExit()) return FALSE;
	 break;
     }
     /*************************************************************************
     * Update key definitions in case they were changed.
     *************************************************************************/
     NEWkeyDEFS (EWkey, keyDefs, batchMode)
  }
}

/******************************************************************************
* ActionMenu.
* Returns FALSE if no more access to the CDF.
******************************************************************************/

Logical ActionMenu (part)
int part;
{
  static Logical first = TRUE;
  static int actExits[] = { ENTERkey_FSI, HELPkey_FSI, EXITkey_FSI, NUL };
  static char *actLines1[] = {
    "List to screen",
    "List to file...",
    "Output to CDF(V2.7)...",
    "Walk on screen",
    "Output all items/variables",
    "Output no items/variables",
    "Filter all items/variables",
    "Filter no items/variables",
    "Set CDF's compression...",
    "Set variable compressions...",
    "Save settings...",
    "Restore settings...",
    "Set monotonicities",
    "Set CDF's checksum..."
  };
  static char *actLines2[] = {
    "List to screen",
    "List to file...",
    "Output to CDF(V3)...",
    "Output to CDF(V2.7)...",
    "Walk on screen",
    "Output all items/variables",
    "Output no items/variables",
    "Filter all items/variables",
    "Filter no items/variables",
    "Set CDF's compression",
    "Set variable compressions...",
    "Save settings...",
    "Restore settings...",
    "Set monotonicities",
    "Set CDF's checksum..."
  };

  static int actLineNs1[] = { 0,1,2,3,4,5,6,7,8,9,10,11,12,13 };
  static int actCols1[] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0 };
  static int actLens1[] = { 33,33,33,33,33,33,33,33,33,33,33,33,33,33 };
  static int actLineNs2[] = { 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14 };
  static int actCols2[] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };
  static int actLens2[] = { 33,33,33,33,33,33,33,33,33,33,33,33,33,33,33 };

  static struct ItemWindowStruct IWact1 = {
    1, 44, 35, " ActionMenu ", 0, NULL, 14, actLines1, 14, actLineNs1,
    actCols1, actLens1, 14, 0, NULL, actExits, REFRESHkey_FSI, FALSE, NUL, NUL
  };
  static struct ItemWindowStruct IWact2 = {
    1, 44, 35, " ActionMenu ", 0, NULL, 15, actLines2, 15, actLineNs2,
    actCols2, actLens2, 15, 0, NULL, actExits, REFRESHkey_FSI, FALSE, NUL, NUL
  };
  static struct ItemWindowStruct IWact;
  struct ItemStruct *Item;
  static char keyDefs[] = {
    "Select: ________    Help: ________    Exit: ________\n\n"
  };
  int ix = 0, iy;
  /****************************************************************************
  * Create ActionMenu and process keystrokes.
  ****************************************************************************/
  if (first) {
    char *p1 = keyDefs;
    EncodeKeyDefinitions (1, &p1, ENTERkey_FSI, HELPkey_FSI, EXITkey_FSI);
    first = FALSE;
  }
  if (CDFgetFileBackwardEnvVar() == 1) 
    IWact = IWact1;
  else {
    IWact = IWact2;
    ix = 1;
  }
  ItemWindow (NEWiw, &IWact, 0);
  for (;;) {
     NEWkeyDEFS (EWkey, keyDefs, batchMode)
     ItemWindow (READiw, &IWact);
     switch (IWact.key) {
       case ENTERkey_FSI: {
	 iy = IWact.itemN;
	 if (ix == 0) {
	   if (iy == TOCDFact) iy = TOCDFv2act; /* TOCDFv2act */
	 } else {
	   if (iy == (TOCDFact + 1)) iy = TOCDFv2act;
	   else if ((iy > (TOCDFact + 1)) && (iy != SETcdfCHECKSUMact)) iy--;
	 }
	 switch (iy) {
	   case TOSCREENact:
	     ItemWindow (DELETEiw, &IWact);
	     return BOO(opt.horizontalMode,ToScreenHori(),ToScreenVert());
	   case TOFILEact:
	     ItemWindow (DELETEiw, &IWact);
	     return BOO(opt.horizontalMode,ToFileHori(),ToFileVert());
	   case TOCDFact: 
	   case TOCDFv2act: {
	     CDFstatus status; CDFid inID;
	     ItemWindow (DELETEiw, &IWact);
	     status = CDFlib (CONFIRM_, CDF_, &inID,
			      NULL_);
	     DisplayStatus (status, "confirming input CDF");
	     if (StatusBAD(status)) return FALSE;
	     CDFsetFileBackward(BACKWARDFILEoff);
	     if (iy == TOCDFv2act) CDFsetFileBackward(BACKWARDFILEon);
	     return ToCDF(inID);
	   }
	   case WALKact: {
	     ItemWindow (DELETEiw, &IWact);
	     return ToWalk();
	   }
	   case OUTPUTallITEMSact:
	   case OUTPUTnoITEMSact: {
	     Logical state = (iy == OUTPUTallITEMSact);
	     ItemWindow (DELETEiw, &IWact);
	     DisplayMessage (BOO(state,"Setting `outputs' to `YES'.",
					"Setting `outputs' to `no'"), NOWAIT);
	     for (Item = itemHead; Item != NULL; Item = Item->nextItem) {
		Item->outputSetting = state;
	     }
	     LoadSelectionWindow (part);
	     ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
	     DisplayMessage ("", NOWAIT);
	     return TRUE;
	   }
	   case FILTERallITEMSact:
	     ItemWindow (DELETEiw, &IWact);
	     DisplayMessage ("Setting `filters' to `YES'.", NOWAIT);
	     for (Item = itemHead; Item != NULL; Item = Item->nextItem) {
		switch (Item->type) {
		  case RECORDt:
		    if (Item->Record->min != NOminMax ||
			Item->Record->max != NOminMax) {
		      Item->filterSetting = TRUE;
		    }
		    break;
		  case INDICESt:
		    if (Item->Indices->minNumDims != NOminMax ||
			Item->Indices->maxNumDims != NOminMax) {
		      Item->filterSetting = TRUE;
		    }
		    break;
		  case VARIABLEt:
		    if (Item->Var->min != NULL ||
			Item->Var->max != NULL) Item->filterSetting = TRUE;
		    break;
		}
	     }
	     LoadSelectionWindow (part);
	     ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
	     DisplayMessage ("", NOWAIT);
	     return TRUE;
	   case FILTERnoITEMSact:
	     ItemWindow (DELETEiw, &IWact);
	     DisplayMessage ("Setting `filters' to `no'.", NOWAIT);
	     for (Item = itemHead; Item != NULL; Item = Item->nextItem) {
		Item->filterSetting = FALSE;
	     }
	     LoadSelectionWindow (part);
	     ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
	     DisplayMessage ("", NOWAIT);
	     return TRUE;
	   case SETcdfCOMPRESSact: {
	     long cType, cParms[CDF_MAX_PARMS]; int parmN;
	     ItemWindow (DELETEiw, &IWact);
	     if (PromptForCompression(&cType,cParms)) {
	       CDFcType = cType;
	       for (parmN = 0; parmN < CDF_MAX_PARMS; parmN++) {
		  CDFcParms[parmN] = cParms[parmN];
	       }
	       LoadCDFwindow (NULL);
	       FieldWindow (UPDATEfw, FWcdf, 0, LogicalTRUE);
	     }
	     return TRUE;
	   }
	   case SETvarCOMPRESSact: {
	     long cType, cParms[CDF_MAX_PARMS]; int parmN;
	     ItemWindow (DELETEiw, &IWact);
	     if (PromptForCompression(&cType,cParms)) {
	       for (Item = itemHead; Item != NULL; Item = Item->nextItem) {
		  switch (Item->type) {
		    case RECORDt:
		    case INDICESt:
		      break;
		    case VARIABLEt:
		      Item->Var->cType = cType;
		      for (parmN = 0; parmN < CDF_MAX_PARMS; parmN++) {
			 Item->Var->cParms[parmN] = cParms[parmN];
		      }
		      break;
		  }
	       }
	       LoadSelectionWindow (part);
	       ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
	     }
	     return TRUE;
	   }
	   case SAVEact:
	     ItemWindow (DELETEiw, &IWact);
	     if (PromptFor(settingsFile,DU_MAX_PATH_LEN,strlen(settingsFile),
			   "Enter path for settings file...",SETFILEhelpID)) {
	       DisplayMessage ("Saving settings...", NOWAIT);
	       SaveSettings ();
	       DisplayMessage ("", NOWAIT);
	     }
	     return TRUE;
	   case RESTOREact:
	     ItemWindow (DELETEiw, &IWact);
	     if (PromptFor(settingsFile,DU_MAX_PATH_LEN,strlen(settingsFile),
			   "Enter path for settings file...",SETFILEhelpID)) {
	       DisplayMessage ("Restoring settings...", NOWAIT);
	       RestoreSettings ();
	       LoadSelectionWindow (part);
	       ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
	       DisplayMessage ("", NOWAIT);
	     }
	     return TRUE;
	   case SETMONOact:
	     ItemWindow (DELETEiw, &IWact);
	     if (!SetItemMonotonicities()) return FALSE;
	     LoadSelectionWindow (part);
	     ItemWindow (UPDATEiw, IWsel, IWsel->itemN);
	     return TRUE;
           case SETcdfCHECKSUMact: {
             long checksum; 
             ItemWindow (DELETEiw, &IWact);
             if (PromptForChecksum(&checksum)) {
               CDFchecksum = checksum;
               LoadCDFwindow (NULL);
               FieldWindow (UPDATEfw, FWcdf, 0, LogicalTRUE);
             }
             return TRUE;
           }
	 }
	 break;
      }
      case HELPkey_FSI:
	if (CDFgetFileBackwardEnvVar() == 1)
	  OnlineHelpWindow ("cdfxp.ilh", ACTIONhelpID);
	else
	  OnlineHelpWindow ("cdfxp.ilh", ACTIONv32helpID);
	break;
      case EXITkey_FSI:
	ItemWindow (DELETEiw, &IWact);
	return TRUE;
    }
  }
}

/******************************************************************************
* OptionMenu.
******************************************************************************/

void OptionMenu () {
  Logical first = TRUE; struct OptionStruct optSave;
  static long nextEncoding[] = {
    0L, SUN_ENCODING, VAX_ENCODING, DECSTATION_ENCODING, SGi_ENCODING,
    IBMPC_ENCODING, IBMRS_ENCODING, HOST_ENCODING, PPC_ENCODING, HP_ENCODING,
    0L, NeXT_ENCODING, ALPHAOSF1_ENCODING, ALPHAVMSd_ENCODING,
    ALPHAVMSg_ENCODING, ALPHAVMSi_ENCODING, NETWORK_ENCODING
  };
  static long prevEncoding[] = {
    0L, ALPHAVMSi_ENCODING, NETWORK_ENCODING, SUN_ENCODING,
	VAX_ENCODING, DECSTATION_ENCODING, SGi_ENCODING, IBMPC_ENCODING,
	IBMRS_ENCODING, HOST_ENCODING, 0L, PPC_ENCODING, HP_ENCODING,
	NeXT_ENCODING, ALPHAOSF1_ENCODING, ALPHAVMSd_ENCODING, ALPHAVMSg_ENCODING
  };
  static int optExits[] = { ENTERkey_FSI, HELPkey_FSI, EXITkey_FSI,
			    NEXTFIELDkey_EXPORT, PREVFIELDkey_EXPORT, NUL };
  AOSs15 (optLines, BLANKs40, BLANKs40, BLANKs40, BLANKs40, BLANKs40,
	  BLANKs40, BLANKs40, BLANKs40, BLANKs40, BLANKs40, BLANKs40,
	  BLANKs40, BLANKs40, BLANKs40, BLANKs40)
  static int optLineNs[] = { 0,1,2,3,4,5,6,7,8,9,10,11 };
  static int optCols[] = { 0,0,0,0,0,0,0,0,0,0,0,0 };
  static int optLens[] = { 33,33,33,33,33,33,33,33,33,33,33,33 };
  static struct ItemWindowStruct IWopt = {
    1, 44, 35, " OptionMenu ", 0, NULL, 12, optLines, 12, optLineNs, optCols,
    optLens, 13, 0, NULL, optExits, REFRESHkey_FSI, FALSE, NUL, NUL
  };
  static char keyDefs[] = "Enter: ________    Increment: ____________    Help: ________\nExit:  ________    Decrement: ____________\n";
  /****************************************************************************
  * Display OptionMenu and process keystrokes until exit requested.
  ****************************************************************************/
  for (;;) {
     /*************************************************************************
     * Initialize/update OptionMenu lines.
     *************************************************************************/
     size_t ilen = (size_t) sizeof(BLANKs40);
     snprintf (IWopt.iLines[FILTERrx], ilen, "Use filters: %s",
	       BOO(opt.overallFilter,"YES","no"));
     snprintf (IWopt.iLines[FILLSrx], ilen, "Use fill values: %s",
	       BOO(opt.useFills,"YES","no"));
     snprintf (IWopt.iLines[CDFFORMATrx], ilen, "Create %s-file CDFs",
	       BOO(opt.singleFile,"single","multi"));
     snprintf (IWopt.iLines[ENCODINGrx], ilen, "Create %s-encoded CDFs",
	       encodings[(int)opt.encoding]);
     snprintf (IWopt.iLines[EPOCHrx], ilen, "EPOCH style: %s",
	       epochStyles[opt.epochStyle]);
     snprintf (IWopt.iLines[hvMODErx], ilen, "%s listings",
	       BOO(opt.horizontalMode,"Horizontal","Vertical"));
     snprintf (IWopt.iLines[MAJORrx], ilen, "Majority: %s",
	       majorities[(int)opt.majority]);
     snprintf (IWopt.iLines[SHOWrx], ilen, "Show filtered lines: %s",
	       BOO(opt.showFiltered,"YES","no"));
     snprintf (IWopt.iLines[SPACINGrx], ilen, "Item/variable spacing: %d",
	       opt.spacing);
     snprintf (IWopt.iLines[DELETErx], ilen, "Delete existing CDF: %s",
	       BOO(opt.deleteExisting,"YES, beware!","no"));
     snprintf (IWopt.iLines[PREALLOCrx], ilen, "Preallocate records: %s",
	       BOO(opt.preAllocate,"YES","no"));
     snprintf (IWopt.iLines[HEADINGrx], ilen, "Display heading: %s",
	       BOO(opt.textHeading,BOO(poundedheading==1,"POUNDEDHEADING",
                                       "YES"),"no"));
     /*************************************************************************
     * Create OptionMenu and encode key definitions first time.  Just update
     * the OptionMenu all the following times.
     *************************************************************************/
     if (first) {
       char *p1 = keyDefs;
       EncodeKeyDefinitions (1, &p1, ENTERkey_FSI, NEXTFIELDkey_EXPORT,
			     HELPkey_FSI, EXITkey_FSI, PREVFIELDkey_EXPORT);
       ItemWindow (NEWiw, &IWopt, 0);
       memmove (&optSave, &opt, sizeof(struct OptionStruct));
       first = FALSE;
     }
     else
       ItemWindow (UPDATEiw, &IWopt, IWopt.itemN);
     /*************************************************************************
     * Update key definitions (in case there were changed).
     *************************************************************************/
     NEWkeyDEFS (EWkey, keyDefs, batchMode)
     /*************************************************************************
     * Process keystroke.
     *************************************************************************/
     ItemWindow (READiw, &IWopt);
     switch (IWopt.key) {
       case ENTERkey_FSI:
	 ItemWindow (DELETEiw, &IWopt);
	 return;
       case NEXTFIELDkey_EXPORT:
	 switch (IWopt.itemN) {
	   case FILTERrx:
	     opt.overallFilter = BOO(opt.overallFilter,FALSE,TRUE);
	     break;
	   case FILLSrx:
	     opt.useFills = BOO(opt.useFills,FALSE,TRUE);
	     break;
	   case CDFFORMATrx:
	     opt.singleFile = BOO(opt.singleFile,FALSE,TRUE);
	     break;
	   case hvMODErx:
	     opt.horizontalMode = BOO(opt.horizontalMode,FALSE,TRUE);
	     break;
	   case SHOWrx:
	     opt.showFiltered = BOO(opt.showFiltered,FALSE,TRUE);
	     break;
	   case DELETErx:
	     opt.deleteExisting = BOO(opt.deleteExisting,FALSE,TRUE);
	     break;
	   case PREALLOCrx:
	     opt.preAllocate = BOO(opt.preAllocate,FALSE,TRUE);
	     break;
	   case HEADINGrx:
             if (opt.textHeading == FALSE) {
               opt.textHeading = TRUE;
               poundedheading = 0;
             } else {
               if (poundedheading == 0)
                 poundedheading = 1;
               else {
                 poundedheading = 0;
                 opt.textHeading = FALSE;
               }
             }
	     break;
	   case MAJORrx:
	     opt.majority = BOO(opt.majority == COLUMN_MAJOR,
				INPUT_MAJOR,opt.majority + 1);
	     break;
	   case ENCODINGrx:
	     opt.encoding = nextEncoding[(int)opt.encoding];
	     break;
	   case EPOCHrx:
	     opt.epochStyle = BOO(opt.epochStyle == EPOCH4_STYLE,
				  EPOCH0_STYLE,opt.epochStyle + 1);
	     break;
	   case SPACINGrx:
	     opt.spacing++;
	     break;
	 }
	 break;
       case PREVFIELDkey_EXPORT:
	 switch (IWopt.itemN) {
	   case FILTERrx:
	     opt.overallFilter = BOO(opt.overallFilter,FALSE,TRUE);
	     break;
	   case FILLSrx:
	     opt.useFills = BOO(opt.useFills,FALSE,TRUE);
	     break;
	   case CDFFORMATrx:
	     opt.singleFile = BOO(opt.singleFile,FALSE,TRUE);
	     break;
	   case hvMODErx:
	     opt.horizontalMode = BOO(opt.horizontalMode,FALSE,TRUE);
	     break;
	   case SHOWrx:
	     opt.showFiltered = BOO(opt.showFiltered,FALSE,TRUE);
	     break;
	   case DELETErx:
	     opt.deleteExisting = BOO(opt.deleteExisting,FALSE,TRUE);
	     break;
	   case PREALLOCrx:
	     opt.preAllocate = BOO(opt.preAllocate,FALSE,TRUE);
	     break;
	   case HEADINGrx:
             if (opt.textHeading == FALSE) {
               opt.textHeading = TRUE;
               poundedheading = 1;
             } else {
               if (poundedheading == 1)
                 poundedheading = 0;
               else {
                 poundedheading = 0;
                 opt.textHeading = FALSE;
               }
             }
	     break;
	   case MAJORrx:
	     opt.majority = BOO(opt.majority == INPUT_MAJOR,
				COLUMN_MAJOR,opt.majority - 1);
	     break;
	   case ENCODINGrx:
	     opt.encoding = prevEncoding[(int)opt.encoding];
	     break;
	   case EPOCHrx:
	     opt.epochStyle = BOO(opt.epochStyle == EPOCH0_STYLE,
				  EPOCH4_STYLE,opt.epochStyle - 1);
	     break;
	   case SPACINGrx:
	     if (opt.spacing > 0)
	       opt.spacing--;
	     else
	       ItemWindow (BEEPiw, &IWopt);
	     break;
	 }
	 break;
       case HELPkey_FSI:
	 OnlineHelpWindow ("cdfxp.ilh", OPTIONhelpID);
	 break;
       case EXITkey_FSI:
	 ItemWindow (DELETEiw, &IWopt);
	 memmove (&opt, &optSave, sizeof(struct OptionStruct));
	 return;
     }
  }
}

/******************************************************************************
* ConfirmExit.
* Returns TRUE if exit should proceed.  FALSE if exit should be aborted.
******************************************************************************/

Logical ConfirmExit () {
  return TRUE;
}

/******************************************************************************
* DisplayStatus.
******************************************************************************/

void DisplayStatus (status, text)
CDFstatus status;
char *text;
{
  if (status == CDF_OK) return;
  if (StatusINFO(status)) {
    if (report[INFOs]) {
      char explanation[CDF_STATUSTEXT_LEN+1], msg[MAX_MESSAGE_TEXT_LEN+1];
      CDFlib (SELECT_, CDF_STATUS_, status,
	      GET_, STATUS_TEXT_, explanation,
	      NULL_);
      strcpyX (msg, "Informational...while ", MAX_MESSAGE_TEXT_LEN);
      strcatX (msg, text, MAX_MESSAGE_TEXT_LEN);
      strcatX (msg, ".", MAX_MESSAGE_TEXT_LEN);
      DisplayMessage (msg, NOBEEPWAIT1);
      DisplayMessage (explanation, NOBEEPWAIT1);
    }
    return;
  }
  if (StatusWARN(status)) {
    if (report[WARNs]) {
      char explanation[CDF_STATUSTEXT_LEN+1], msg[MAX_MESSAGE_TEXT_LEN+1];
      CDFlib (SELECT_, CDF_STATUS_, status,
	      GET_, STATUS_TEXT_, explanation,
	      NULL_);
      strcpyX (msg, "Warning...while ", MAX_MESSAGE_TEXT_LEN);
      strcatX (msg, text, MAX_MESSAGE_TEXT_LEN);
      strcatX (msg, ".", MAX_MESSAGE_TEXT_LEN);
      DisplayMessage (msg, BEEPWAIT1);
      DisplayMessage (explanation, BEEPWAIT1);
    }
    return;
  }
  if (report[ERRORs]) {
    char explanation[CDF_STATUSTEXT_LEN+1], msg[MAX_MESSAGE_TEXT_LEN+1];
    CDFlib (SELECT_, CDF_STATUS_, status,
	    GET_, STATUS_TEXT_, explanation,
	    NULL_);
    strcpyX (msg, "ERROR...while ", MAX_MESSAGE_TEXT_LEN);
    strcatX (msg, text, MAX_MESSAGE_TEXT_LEN);
    strcatX (msg, ".", MAX_MESSAGE_TEXT_LEN);
    DisplayMessage (msg, BEEPWAIT1);
    DisplayMessage (explanation, BEEPWAIT);
  }
  return;
}

/******************************************************************************
* DisplayMessage.
*   wait = NOWAIT.      Return immediately, do not clear message.  NUL is
*                       returned.
*   wait = NOBEEPWAIT.  Wait for user input then clear message.  The key
*                       entered is returned.
*   wait = NOBEEPWAIT1. Wait one second then clear message.  NUL is returned.
*   wait = NOBEEPWAIT2. Wait two seconds then clear message.  NUL is returned.
*   wait = BEEPWAIT.    Beep, wait for user input then clear message.  The key
*                       entered is returned.
*   wait = BEEPWAIT1.   Beep, wait one second then clear message.  NUL is
*                       returned.
*   wait = BEEPWAIT2.   Beep, wait two seconds then clear message.  NUL is
*                       returned.
******************************************************************************/

int DisplayMessage (message, wait)
char *message;
int wait;
{
  /****************************************************************************
  * If batch mode, just print the message to the standard output.
  ****************************************************************************/
  if (BATCH(batchMode)) {
    printf ("%s\n", message);
    return 0;
  }
  /****************************************************************************
  * Otherwise...
  ****************************************************************************/
  strcpyX (EWmsg->eText, message, EWmsg->nColsTotal - 2);
  switch (wait) {
    case NOWAIT:
      EditWindow (UPDATEew, EWmsg, LogicalTRUE);
      return 0;
    case NOBEEPWAIT:
    case BEEPWAIT: {
      static char newLabel[] = { " Enter any key to acknowledge. " };
      char *saveLabel = EWmsg->label;
      EWmsg->label = newLabel;
      EditWindow (UPDATEew, EWmsg, LogicalTRUE);
      if (wait == BEEPWAIT) EditWindow (BEEPew, EWmsg);
      EditWindow (READew, EWmsg);
      MakeNUL (EWmsg->eText);
      EWmsg->label = saveLabel;
      EditWindow (UPDATEew, EWmsg, LogicalTRUE);
      return EWmsg->key;
    }
    case NOBEEPWAIT1:
    case NOBEEPWAIT2:
    case BEEPWAIT1:
    case BEEPWAIT2: {
      double seconds = BOO((wait == NOBEEPWAIT1 || wait == BEEPWAIT1),1.0,2.0);
      EditWindow (UPDATEew, EWmsg, LogicalTRUE);
      if (wait == BEEPWAIT1 || wait == BEEPWAIT2) EditWindow (BEEPew, EWmsg);
      zzzzz (seconds);
      MakeNUL (EWmsg->eText);
      EditWindow (UPDATEew, EWmsg, LogicalTRUE);
      return 0;
    }
  }
  return 0;
}

/******************************************************************************
* MatchOption.
*   Returns `AMBIGUOUSopt' if ambiguous, `UNKNOWNopt' if not found.
******************************************************************************/

int MatchOption (option, validOptions)
char *option;
char *validOptions[];
{
  int i, j;
  /****************************************************************************
  * First try for an exact match.
  ****************************************************************************/
  for (i = 0; validOptions[i] != NULL; i++) {
     if (strcmpIgCase(option,validOptions[i]) == 1) return i;
  }
  /****************************************************************************
  * Check if the option matches the first part of one of the valid options
  * (ie. it has been abbreviated by truncation).  If so, make sure that it
  * cannot be any of the other valid options (ie. ambiguous).
  ****************************************************************************/
  for (i = 0; validOptions[i] != NULL; i++) {
     if (strncmpIgCasePattern(option,validOptions[i],strlen(option)) == 0) {
       for (j = i + 1; validOptions[j] != NULL; j++) {
	  if (strncmpIgCasePattern(option,validOptions[j],strlen(option)) == 0) 
	    return AMBIGUOUSopt;
       }
       return i;
     }
  }
  /****************************************************************************
  * The option was not found.
  ****************************************************************************/
  return UNKNOWNopt;
}

/******************************************************************************
* FreeItems.
******************************************************************************/

void FreeItems () {
  struct ItemStruct *Item = itemHead, *tItem;
  while (Item != NULL) {
    tItem = Item->nextItem;
    switch (Item->type) {
      case RECORDt:
	cdf_FreeMemory (Item->Record, FatalError);
	break;
      case INDICESt:
	cdf_FreeMemory (Item->Indices, FatalError);
	break;
      case VARIABLEt:
	if (Item->Var->name != NULL) cdf_FreeMemory (Item->Var->name,
						 FatalError);
	if (Item->Var->min != NULL) cdf_FreeMemory (Item->Var->min,
						FatalError);
	if (Item->Var->max != NULL) cdf_FreeMemory (Item->Var->max,
						FatalError);
	if (Item->Var->format != NULL) cdf_FreeMemory (Item->Var->format,
						   FatalError);
	if (Item->Var->fill != NULL) cdf_FreeMemory (Item->Var->fill,
						 FatalError);
	if (Item->Var->pad != NULL) cdf_FreeMemory (Item->Var->pad,
						FatalError);
	if (Item->Var->value != NULL) cdf_FreeMemory (Item->Var->value,
						  FatalError);
	cdf_FreeMemory (Item->Var, FatalError);
	break;
    }
    cdf_FreeMemory (Item, FatalError);
    Item = tItem;
  }
  itemHead = NULL;
  return;
}
/******************************************************************************
* LongValueWidth.
******************************************************************************/

int LongValueWidth (value)
long value;
{
  int width = 1;
  long tmp = value / 10;
  while (tmp != 0) {
    width++;
    tmp = tmp / 10;
  }
  return width;
}

/******************************************************************************
* RecordIndicesWidth.
******************************************************************************/

int RecordIndicesWidth (lastRec, numDims, dimSizes, type)
long lastRec;
long numDims;
long dimSizes[];
int type;
{
  switch (type) {
    case RECORDt:
      return MaxInt(LongValueWidth(lastRec),MIN_RECORD_WIDTH);
    case INDICESt: {
      int n, indicesW = (int) (1 + BOO(numDims > 1,numDims - 1,0) + 1);
      for (n = 0; n < numDims; n++) indicesW += LongValueWidth(dimSizes[n]);
      return MAXIMUM(indicesW,MIN_INDICES_WIDTH);
    }
  }
  return 0;
}

/******************************************************************************
* FreeToScreen.
******************************************************************************/

void FreeToScreen (header, scrLines, trailer)
char *header[];
char *scrLines[];
char *trailer[];
{
  int lineN;
  cdf_FreeMemory (header[0], FatalError);
  for (lineN = 0; lineN < NUMscreenLINES; lineN++) {
     cdf_FreeMemory (scrLines[lineN], FatalError);
  }
  cdf_FreeMemory (trailer[0], FatalError);
  return;
}

/******************************************************************************
* PromptFor.
******************************************************************************/

Logical PromptFor (value, valueL, cursorAt, label, helpID)
char *value;
int valueL;
int cursorAt;
char *label;
int helpID;
{
  static Logical first = TRUE;
  static int exits[] = { ENTERkey_FSI, HELPkey_FSI, EXITkey_FSI, NUL };
  static struct PromptWindowStruct PW = {
    NULL, 21, 0, SCREEN_WIDTH, 0, NULL, 0, NULL, 0, NULL, exits,
    REFRESHkey_FSI, SOLkey_FSI, EOLkey_FSI, INSERTorOVERkey_FSI
  };
  static char keyDefsBlank[] = "\n\n";
  static char keyDefsPrompt[] = {
    "Enter: ________    Help: ________    Exit: ________\n\n"
  };
  if (first) {
    char *p1 = keyDefsPrompt;
    EncodeKeyDefinitions (1, &p1, ENTERkey_FSI, HELPkey_FSI, EXITkey_FSI);
    first = FALSE;
  }
  PW.label = label;
  PW.maxChars = valueL;
  PW.value = value;
  PromptWindow (NEWpw, &PW, cursorAt, LogicalTRUE);
  NEWkeyDEFS (EWkey, keyDefsPrompt, batchMode)
  for (;;) {
     PromptWindow (READpw, &PW);
     switch (PW.key) {
       case ENTERkey_FSI:
	 PromptWindow (DELETEpw, &PW);
	 NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
	 return TRUE;
       case HELPkey_FSI:
	 OnlineHelpWindow ("cdfxp.ilh", helpID);
	 break;
       case EXITkey_FSI:
	 PromptWindow (DELETEpw, &PW);
	 NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
	 return FALSE;
     }
  }
}

/******************************************************************************
* PromptForMinMax.
******************************************************************************/

Logical PromptForMinMax (Item, min)
struct ItemStruct *Item;
Logical min;
{
  static char value[MAXminMaxLEN+1];
  static char minGTmaxMsg[] = {
    "That minimum would be greater than the current maximum."
  };
  static char maxLTminMsg[] = {
    "That maximum would be less than the current minimum."
  };
  switch (Item->type) {
    /**************************************************************************
    * Record.
    **************************************************************************/
    case RECORDt: {
      long *mmPtr = BOO(min,&(Item->Record->min),&(Item->Record->max));
      long *mmPtrX = BOO(min,&(Item->Record->max),&(Item->Record->min));
      long newValue;
      /************************************************************************
      * Set initial value.
      ************************************************************************/
      if (*mmPtr == NOminMax)
	MakeNUL (value);
      else
	snprintf (value, (size_t) sizeof(value), "%ld", *mmPtr + 1);
      /************************************************************************
      * Prompt for new value.
      ************************************************************************/
      if (!PromptFor(value,MAXminMaxLEN,strlen(value),
		     BOO(min,"Enter new minimum record...",
			     "Enter new maximum record..."),
		     MINmaxRECORDhelpID)) {
	return FALSE;
      }
      /************************************************************************
      * Entering nothing erases the current value.
      ************************************************************************/
      if (NULstring(value)) {
	if (*mmPtr != NOminMax) {
	  *mmPtr = NOminMax;
	  if (*mmPtrX == NOminMax) Item->filterSetting = FALSE;
	  return TRUE;
	}
	else
	  return FALSE;
      }
      /************************************************************************
      * Decode the value entered.
      ************************************************************************/
      if (sscanf(value,"%ld",&newValue) != 1) {
	DisplayMessage ("Error decoding value.", BEEPWAIT1);
	return FALSE;
      }
      else
	newValue--;
      /************************************************************************
      * Validate the value entered.
      ************************************************************************/
      if (newValue < 0) {
	DisplayMessage ("Illegal value.", BEEPWAIT1);
	return FALSE;
      }
      /************************************************************************
      * Make sure the new minimum is not greater than the current maximum.
      ************************************************************************/
      if (min && Item->Record->max != NOminMax) {
	if (newValue > Item->Record->max) {
	  DisplayMessage (minGTmaxMsg, BEEPWAIT1);
	  return FALSE;
	}
      }
      /************************************************************************
      * Make sure the new maximum is not less than the current minimum.
      ************************************************************************/
      if (!min && Item->Record->min != NOminMax) {
	if (newValue < Item->Record->min) {
	  DisplayMessage (maxLTminMsg, BEEPWAIT1);
	  return FALSE;
	}
      }
      /************************************************************************
      * Everything's OK, save the new value entered.
      ************************************************************************/
      *mmPtr = newValue;
      Item->filterSetting = TRUE;
      opt.overallFilter = TRUE;
      break;
    }
    /**************************************************************************
    * Indices.
    **************************************************************************/
    case INDICESt: {
      long *mmNumDims = BOO(min,&(Item->Indices->minNumDims),
				&(Item->Indices->maxNumDims)),
	   *mmNumDimsX = BOO(min,&(Item->Indices->maxNumDims),
				 &(Item->Indices->minNumDims)),
	   *mmIndices = BOO(min,Item->Indices->minIndices,
				Item->Indices->maxIndices),
	   newNumDims, newIndices[CDF_MAX_DIMS];
      int dimN;
      /************************************************************************
      * Set initial indices.
      ************************************************************************/
      if (*mmNumDims == NOminMax)
	strcpyX (value, "[]", MAXminMaxLEN);
      else
	EncodeIndicesJustify (value, *mmNumDims, mmIndices, 0,
			      (size_t) sizeof(value));
      /************************************************************************
      * Prompt for new indices.
      ************************************************************************/
      if (!PromptFor(value,MAXminMaxLEN,strlen(value)-1,
		     BOO(min,"Enter new minimum indices...",
			     "Enter new maximum indices..."),
		     MINmaxINDICEShelpID)) {
	return FALSE;
      }
      /************************************************************************
      * Entering nothing erases the current indices.
      ************************************************************************/
      if (NULstring(value)) {
	if (*mmNumDims != NOminMax) {
	  *mmNumDims = NOminMax;
	  if (*mmNumDimsX == NOminMax) Item->filterSetting = FALSE;
	  return TRUE;
	}
	else
	  return FALSE;
      }
      /************************************************************************
      * Decode the indices entered.
      ************************************************************************/
      if (!DecodeRecordAndIndices(value,NULL,&newNumDims,newIndices)) {
	DisplayMessage ("Error decoding indices.", BEEPWAIT1);
	return FALSE;
      }
      /************************************************************************
      * Validate the indices entered.
      ************************************************************************/
      if (newNumDims == 0) {
	DisplayMessage ("Zero dimensions not allowed.", BEEPWAIT1);
	return FALSE;
      }
      for (dimN = 0; dimN < newNumDims; dimN++) {
	 if (newIndices[dimN] < 0) {
	   DisplayMessage ("Illegal indices.", BEEPWAIT1);
	   return FALSE;
	 }
      }
      /************************************************************************
      * Everything's OK, save the new value entered.
      ************************************************************************/
      *mmNumDims = newNumDims;
      for (dimN = 0; dimN < newNumDims; dimN++) {
	 mmIndices[dimN] = newIndices[dimN];
      }
      Item->filterSetting = TRUE;
      opt.overallFilter = TRUE;
      break;
    }
    /**************************************************************************
    * Variable.
    **************************************************************************/
    case VARIABLEt: {
      void *newValue; long numElems;
      void **mmPtr = BOO(min,&(Item->Var->min),&(Item->Var->max));
      void **mmPtrX = BOO(min,&(Item->Var->max),&(Item->Var->min));
      int style;
      if (TT2000dataType(Item->Var->dataType)) style = TT2000_3_STYLE;
      else style = opt.epochStyle;
      /************************************************************************
      * Set initial value.
      ************************************************************************/
      if (*mmPtr == NULL)
	strcpyX (value, BOO(STRINGdataType(Item->Var->dataType),"\"\"",""), 0);
      else
	EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems, *mmPtr,
			    value, NULL, 0, MAXminMaxLEN, style,
			    (size_t) sizeof(value));
      /************************************************************************
      * Prompt for new value.
      ************************************************************************/
      if (!PromptFor(value,MAXminMaxLEN,
		     strlen(value) -
		      BOO(STRINGdataType(Item->Var->dataType),1,0),
		     BOO(min,"Enter new minimum value...",
			     "Enter new maximum value..."),
		     MINmaxVALUEhelpID)) {
	return FALSE;
      }
      /************************************************************************
      * Entering nothing erases the current value.
      ************************************************************************/
      if (NULstring(value)) {
	if (*mmPtr != NULL) {
	  cdf_FreeMemory (*mmPtr, FatalError);
	  *mmPtr = NULL;
	  if (*mmPtrX == NULL) Item->filterSetting = FALSE;
	  return TRUE;
	}
	else
	  return FALSE;
      }
      /************************************************************************
      * Decode/validate the value entered.
      ************************************************************************/
      if (!DecodeValues(value,Item->Var->dataType,
			&numElems,&newValue,opt.epochStyle)) {
	DisplayMessage ("Error decoding value.", BEEPWAIT1);
	return FALSE;
      }
      if (numElems != Item->Var->numElems) {
	DisplayMessage ("Wrong number of elements.", BEEPWAIT1);
	cdf_FreeMemory (newValue, FatalError);
	return FALSE;
      }
      /************************************************************************
      * Make sure the new minimum is not greater than the current maximum.
      ************************************************************************/
      if (min && Item->Var->max != NULL) {
	if (GTx(newValue,Item->Var->max,
		Item->Var->dataType,Item->Var->numElems)) {
	  DisplayMessage (minGTmaxMsg, BEEPWAIT1);
	  cdf_FreeMemory (newValue, FatalError);
	  return FALSE;
	}
      }
      /************************************************************************
      * Make sure the new maximum is not less than the current minimum.
      ************************************************************************/
      if (!min && Item->Var->min != NULL) {
	if (LTx(newValue,Item->Var->min,
		Item->Var->dataType,Item->Var->numElems)) {
	  DisplayMessage (maxLTminMsg, BEEPWAIT1);
	  cdf_FreeMemory (newValue, FatalError);
	  return FALSE;
	}
      }
      /************************************************************************
      * Everything's OK, save the new value entered.
      ************************************************************************/
      if (*mmPtr == NULL) *mmPtr = cdf_AllocateMemory ((size_t)Item->Var->nValueBytes,
						   FatalError);
      memmove (*mmPtr, newValue, Item->Var->nValueBytes);
      cdf_FreeMemory (newValue, FatalError);
      Item->filterSetting = TRUE;
      opt.overallFilter = TRUE;
      break;
    }
  }
  return TRUE;
}

/******************************************************************************
* PromptForFill.
******************************************************************************/

Logical PromptForFill (Var)
struct VarStruct *Var;
{
  static char value[MAXfillLEN+1];
  void *newValue; long numElems; int style;
  if (TT2000dataType(Var->dataType)) style = TT2000_3_STYLE;
  else style = opt.epochStyle;
  /****************************************************************************
  * Set initial value.
  ****************************************************************************/
  if (Var->fill == NULL)
    strcpyX (value, BOO(STRINGdataType(Var->dataType),"\"\"",""), 0);
  else
    EncodeValuesFormat (Var->dataType, Var->numElems, Var->fill, value, NULL,
			0, MAXfillLEN, style, (size_t) sizeof(value));
  /****************************************************************************
  * Prompt for new value.
  ****************************************************************************/
  if (!PromptFor(value,MAXfillLEN,
		 strlen(value) - BOO(STRINGdataType(Var->dataType),1,0),
		 "Enter new fill value...",FILLhelpID)) {
    return FALSE;
  }
  /****************************************************************************
  * Check if nothing entered.
  ****************************************************************************/
  if (NULstring(value)) {
    if (Var->fill != NULL) {
      cdf_FreeMemory (Var->fill, FatalError);
      Var->fill = NULL;
      return TRUE;
    }
    else
      return FALSE;
  }
  /****************************************************************************
  * Decode/validate value.
  ****************************************************************************/
  if (!DecodeValues(value,Var->dataType,
		    &numElems,&newValue,opt.epochStyle)) {
    DisplayMessage ("Error decoding value.", BEEPWAIT1);
    return FALSE;
  }
  if (numElems != Var->numElems) {
    DisplayMessage ("Illegal number of elements.", BEEPWAIT1);
    cdf_FreeMemory (newValue, FatalError);
    return FALSE;
  }
  /****************************************************************************
  * Save new value.
  ****************************************************************************/
  if (Var->fill == NULL) {
    Var->fill = cdf_AllocateMemory ((size_t)Var->nValueBytes, FatalError);
  }
  memmove (Var->fill, newValue, Var->nValueBytes);
  cdf_FreeMemory (newValue, FatalError);
  opt.useFills = TRUE;
  return TRUE;
}

/******************************************************************************
* PromptForWidth.
******************************************************************************/

Logical PromptForWidth (Item)
struct ItemStruct *Item;
{
  static char value[MAXwidthLEN+1]; int newWidth;
  /****************************************************************************
  * Set initial value.
  ****************************************************************************/
  snprintf (value, (size_t) sizeof(value), "%d", Item->width);
  /****************************************************************************
  * Prompt for new value.
  ****************************************************************************/
  if (!PromptFor(value,MAXwidthLEN,strlen(value),
		 "Enter new width...",WIDTHhelpID)) return FALSE;
  /****************************************************************************
  * Check if nothing entered.
  ****************************************************************************/
  if (NULstring(value)) return FALSE;
  /****************************************************************************
  * Decode/validate value.
  ****************************************************************************/
  if (sscanf(value,"%d",&newWidth) != 1) {
    DisplayMessage ("Error decoding width.", BEEPWAIT1);
    return FALSE;
  }
  if (newWidth < 0) {
    DisplayMessage ("Illegal width.", BEEPWAIT1);
    return FALSE;
  }
  /****************************************************************************
  * Save new width.
  ****************************************************************************/
  Item->width = newWidth;
  return TRUE;
}

/******************************************************************************
* PromptForReserve.
******************************************************************************/

Logical PromptForReserve (Var)
struct VarStruct *Var;
{
  static char value[MAXreserveLEN+1]; long newReserve;
  /****************************************************************************
  * Set initial value.
  ****************************************************************************/
  snprintf (value, (size_t) sizeof(value), "%ld", Var->reserve);
  /****************************************************************************
  * Prompt for new value.
  ****************************************************************************/
  if (!PromptFor(value,MAXreserveLEN,strlen(value),
		 "Enter new reserve percentage...",
		 RESERVEhelpID)) return FALSE;
  /****************************************************************************
  * Check if nothing entered.
  ****************************************************************************/
  if (NULstring(value)) return FALSE;
  /****************************************************************************
  * Decode/validate value.
  ****************************************************************************/
  if (sscanf(value,"%ld",&newReserve) != 1) {
    DisplayMessage ("Error decoding reserve percentage.", BEEPWAIT1);
    return FALSE;
  }
  if (newReserve < 0) {
    DisplayMessage ("Illegal reserve percentage.", BEEPWAIT1);
    return FALSE;
  }
  /****************************************************************************
  * Save new reserve.
  ****************************************************************************/
  Var->reserve = newReserve;
  return TRUE;
}

/******************************************************************************
* PromptForBlocking.
******************************************************************************/

Logical PromptForBlocking (Var)
struct VarStruct *Var;
{
  static char value[MAXblockingLEN+1]; long newBlocking;
  /****************************************************************************
  * Set initial value.
  ****************************************************************************/
  snprintf (value, (size_t) sizeof(value), "%ld", Var->blocking);
  /****************************************************************************
  * Prompt for new value.
  ****************************************************************************/
  if (!PromptFor(value,MAXblockingLEN,strlen(value),
		 "Enter new blocking factor...",BLOCKINGhelpID)) return FALSE;
  /****************************************************************************
  * Check if nothing entered.
  ****************************************************************************/
  if (NULstring(value)) return FALSE;
  /****************************************************************************
  * Decode/validate value.
  ****************************************************************************/
  if (sscanf(value,"%ld",&newBlocking) != 1) {
    DisplayMessage ("Error decoding blocking factor.", BEEPWAIT1);
    return FALSE;
  }
  if (newBlocking < 0) {
    DisplayMessage ("Illegal blocking factor.", BEEPWAIT1);
    return FALSE;
  }
  /****************************************************************************
  * Save new blocking factor.
  ****************************************************************************/
  Var->blocking = newBlocking;
  return TRUE;
}

/******************************************************************************
* PromptForFormat.
******************************************************************************/

Logical PromptForFormat (Item)
struct ItemStruct *Item;
{
  char format[MAXitemFieldLEN+1];
  /****************************************************************************
  * Set initial format.
  ****************************************************************************/
  strcpyX (format, BOO(Item->Var->format == NULL,"",Item->Var->format),
	   MAXitemFieldLEN);
  /****************************************************************************
  * Prompt for new format.
  ****************************************************************************/
  if (!PromptFor(format,MAXitemFieldLEN,strlen(format),
		 "Enter new format...",FORMAThelpID)) return FALSE;
  /****************************************************************************
  * Check if nothing entered.
  ****************************************************************************/
  if (NULstring(format)) {
    if (Item->Var->format != NULL) {
      cdf_FreeMemory (Item->Var->format, FatalError);
      Item->Var->format = NULL;
      return TRUE;
    }
    else
      return FALSE;
  }
  /****************************************************************************
  * Decode/validate new format?
  ****************************************************************************/
  if (!ValidFormat(format)) {
    DisplayMessage ("Illegal format specification.", BEEPWAIT1);
    return FALSE;
  }
  /****************************************************************************
  * Save new format.
  ****************************************************************************/
  if (Item->Var->format != NULL) cdf_FreeMemory (Item->Var->format,
					     FatalError);
  Item->Var->format = (char *) cdf_AllocateMemory ((size_t)strlen(format) + 1,
					       FatalError);
  strcpyX (Item->Var->format, format, 0);
  /****************************************************************************
  * Possibly increase width based on new format.
  ****************************************************************************/
  Item->width = MaxInt(Item->width,VariableWidth(Item->Var));
  /****************************************************************************
  * Return success.
  ****************************************************************************/
  return TRUE;
}

/******************************************************************************
* PromptForCompression.
******************************************************************************/

Logical PromptForCompression (cType, cParms)
long *cType;
long cParms[CDF_MAX_PARMS];
{
  static Logical first = TRUE;
  static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL };
  AOSs1 (iLines, "None   RLE.0   HUFF.0   AHUFF.0   GZIP...")
  static int iLineNs[] = { 0,0,0,0,0 };
  static int iCols[] = { 0,7,15,24,34 };
  static int iLens[] = { 4,5,6,7,7 };
  static struct ItemWindowStruct IW = {
    21, 0, 80, " Select new compression... ", 0, NULL, 1, iLines, 5, iLineNs,
    iCols, iLens, 1, 0, NULL, exitChars, REFRESHkey_FSI, FALSE, NUL, NUL
  };
  static char keyDefs[] = {
    "Enter: ________    Help: ________    Exit: ________\n\n"
  };
  static char keyDefsBlank[] = "";
  if (first) {
    char *p1 = keyDefs;
    EncodeKeyDefinitions (1, &p1, ENTERkey_FSI, HELPkey_FSI, EXITkey_FSI);
    first = FALSE;
  }
  NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
  ItemWindow (NEWiw, &IW, 0);
  NEWkeyDEFS (EWkey, keyDefs, batchMode)
  for (;;) {
     ItemWindow (READiw, &IW);
     switch (IW.key) {
       case ENTERkey_FSI:
	 switch (IW.itemN) {
	   case NONEin:
	     *cType = NO_COMPRESSION;
	     NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
	     ItemWindow (DELETEiw, &IW);
	     return TRUE;
	   case RLE0in:
	     *cType = RLE_COMPRESSION;
	     cParms[0] = RLE_OF_ZEROs;
	     NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
	     ItemWindow (DELETEiw, &IW);
	     return TRUE;
	   case HUFF0in:
	     *cType = HUFF_COMPRESSION;
	     cParms[0] = OPTIMAL_ENCODING_TREES;
	     NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
	     ItemWindow (DELETEiw, &IW);
	     return TRUE;
	   case AHUFF0in:
	     *cType = AHUFF_COMPRESSION;
	     cParms[0] = OPTIMAL_ENCODING_TREES;
	     NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
	     ItemWindow (DELETEiw, &IW);
	     return TRUE;
	   case GZIPin: {
	     Logical done = FALSE;
	     static char token[MAX_GZIP_TOKEN_LEN+1] = "GZIP.";
	     static int gzipExit[] = {
	       ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL
	     };
	     static struct PromptWindowStruct PW = {
	       "Enter GnuZIP compression parameters...", 21, 0, 80, 0, NULL,
	       MAX_GZIP_TOKEN_LEN, token, 0, NULL, gzipExit, REFRESHkey_FSI,
	       SOLkey_FSI, EOLkey_FSI, INSERTorOVERkey_FSI
	     };
	     ItemWindow (UNDISPLAYiw, &IW);
	     PromptWindow (NEWpw, &PW, (int) strlen(PW.value), LogicalTRUE);
	     while (!done) {
		PromptWindow (READpw, &PW);
		switch (PW.key) {
		  case ENTERkey_FSI: {
		    int level;
		    if (sscanf(PW.value,"GZIP.%d",&level) == 1) {
		      if (INCLUSIVE(1,level,9)) {
			*cType = GZIP_COMPRESSION;
			cParms[0] = level;
			NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
			PromptWindow (DELETEpw, &PW);
			ItemWindow (DELETEiw, &IW);
			return TRUE;
		      }
		    }
		    PromptWindow (UNDISPLAYpw, &PW);
		    DisplayMessage ("Illegal GZIP parameters.", BEEPWAIT1);
		    PromptWindow (REDISPLAYpw, &PW);
		    break;
		  }
		  case HELPkey_FSI:
		    OnlineHelpWindow ("cdfxp.ilh", GZIPhelpID);
		    break;
		  case EXITkey_FSI:
		    PromptWindow (DELETEpw, &PW);
		    ItemWindow (REDISPLAYiw, &IW);
		    done = TRUE;
		    break;
		}
	     }
	     break;
	   }
	 }
	 break;
       case HELPkey_FSI:
	 OnlineHelpWindow ("cdfxp.ilh", COMPRESShelpID);
	 break;
       case EXITkey_FSI:
	 NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
	 ItemWindow (DELETEiw, &IW);
	 return FALSE;
     }
  }
}

/******************************************************************************
* PromptForChecksum.
******************************************************************************/

Logical PromptForChecksum (checksum)
long *checksum;
{
  static Logical first = TRUE;
  static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL };
  AOSs1 (iLines, "None   MD5")
  static int iLineNs[] = { 0,0 };
  static int iCols[] = { 0,7 };
  static int iLens[] = { 4,3 };
  static struct ItemWindowStruct IW = {
    21, 0, 80, " Select new checksum... ", 0, NULL, 1, iLines, 2, iLineNs,
    iCols, iLens, 1, 0, NULL, exitChars, REFRESHkey_FSI, FALSE, NUL, NUL
  };
  static char keyDefs[] = {
    "Enter: ________    Help: ________    Exit: ________\n\n"
  };
  static char keyDefsBlank[] = "";
  if (first) {
    char *p1 = keyDefs;
    EncodeKeyDefinitions (1, &p1, ENTERkey_FSI, HELPkey_FSI, EXITkey_FSI);
    first = FALSE;
  }
  NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
  ItemWindow (NEWiw, &IW, 0);
  NEWkeyDEFS (EWkey, keyDefs, batchMode)
  for (;;) {
     ItemWindow (READiw, &IW);
     switch (IW.key) {
       case ENTERkey_FSI:
	 switch (IW.itemN) {
	   case NONEin:
	     *checksum = NO_CHECKSUM;
	     NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
	     ItemWindow (DELETEiw, &IW);
	     return TRUE;
	   case MD5in:
	     *checksum = MD5_CHECKSUM;
	     NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
	     ItemWindow (DELETEiw, &IW);
	     return TRUE;
	 }
	 break;
       case HELPkey_FSI:
	 OnlineHelpWindow ("cdfxp.ilh", CHECKSUMhelpID);
	 break;
       case EXITkey_FSI:
	 NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
	 ItemWindow (DELETEiw, &IW);
	 return FALSE;
     }
  }
}

/******************************************************************************
* PromptForSparseness.
******************************************************************************/

Logical PromptForSparseness (Var)
struct VarStruct *Var;
{
  static Logical first = TRUE;
  static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL };
  AOSs1 (iLines, "sRecords.NO     sRecords.PAD     sRecords.PREV")
  static int iLineNs[] = { 0,0,0 };
  static int iCols[] = { 0,16,33 };
  static int iLens[] = { 11,12,13 };
  static struct ItemWindowStruct IW = {
    21, 0, 80, " Select new sparseness... ", 0, NULL, 1, iLines, 3, iLineNs,
    iCols, iLens, 1, 0, NULL, exitChars, REFRESHkey_FSI, FALSE, NUL, NUL
  };
  static char keyDefsPrompt[] = {
    "Enter: ________    Help: ________    Exit: ________\n\n"
  };
  static char keyDefsBlank[] = "";
  if (first) {
    char *p1 = keyDefsPrompt;
    EncodeKeyDefinitions (1, &p1, ENTERkey_FSI, HELPkey_FSI, EXITkey_FSI);
    first = FALSE;
  }
  ItemWindow (NEWiw, &IW, 0);
  NEWkeyDEFS (EWkey, keyDefsPrompt, batchMode)
  for (;;) {
     ItemWindow (READiw, &IW);
     switch (IW.key) {
       case ENTERkey_FSI:
	 switch (IW.itemN) {
	   case NOsRECORDSin: Var->sRecordsType = NO_SPARSERECORDS; break;
	   case PADsRECORDSin: Var->sRecordsType = PAD_SPARSERECORDS; break;
	   case PREVsRECORDSin: Var->sRecordsType = PREV_SPARSERECORDS; break;
	 }
	 ItemWindow (DELETEiw, &IW);
	 NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
	 return TRUE;
       case HELPkey_FSI:
	 OnlineHelpWindow ("cdfxp.ilh", SPARSEhelpID);
	 break;
       case EXITkey_FSI:
	 ItemWindow (DELETEiw, &IW);
	 NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
	 return FALSE;
     }
  }
}

/******************************************************************************
* FlipItems.
******************************************************************************/

Logical FlipItems (itemX)
int itemX;
{
  struct ItemStruct *Item = itemHead, *prevItem = NULL, *tItem; int itemXt;
  /****************************************************************************
  * Locate Item (and previous Item) to be flipped.
  ****************************************************************************/
  for (itemXt = 0; itemXt < itemX; itemXt++) {
     prevItem = Item;
     Item = Item->nextItem;
  }
  /****************************************************************************
  * Check if only one Item.
  ****************************************************************************/
  if (Item == itemHead && Item->nextItem == NULL) return FALSE;
  /****************************************************************************
  * Check if last Item.
  ****************************************************************************/
  if (Item->nextItem == NULL) {
    prevItem->nextItem = NULL;
    Item->nextItem = itemHead;
    itemHead = Item;
    return TRUE;
  }
  /****************************************************************************
  * Check if first Item.
  ****************************************************************************/
  if (Item == itemHead) {
    tItem = Item->nextItem;
    Item->nextItem = tItem->nextItem;
    tItem->nextItem = Item;
    itemHead = tItem;
    return TRUE;
  }
  /****************************************************************************
  * In the middle of the Item's.
  ****************************************************************************/
  tItem = Item->nextItem;
  Item->nextItem = tItem->nextItem;
  tItem->nextItem = Item;
  prevItem->nextItem = tItem;
  return TRUE;
}

/******************************************************************************
* VariableWidth.
******************************************************************************/

int VariableWidth (Var)
struct VarStruct *Var;
{
  int width;
  /****************************************************************************
  * Check for a string data type.  In this case the format is ignored.
  ****************************************************************************/
  if (STRINGdataType(Var->dataType)) {
    return MaxInt(MIN_VARIABLE_WIDTH,(int)(Var->numElems+2));
  }
  /****************************************************************************
  * Check for an EPOCH data type.  Depending on the style the format may be
  * ignored.
  ****************************************************************************/
  if (EPOCHdataType(Var->dataType)) {
    switch (opt.epochStyle) {
      case EPOCH0_STYLE:
	return MAXIMUM(EPOCH_STRING_LEN,MIN_VARIABLE_WIDTH);
      case EPOCH1_STYLE:
	return MAXIMUM(EPOCH1_STRING_LEN,MIN_VARIABLE_WIDTH);
      case EPOCH2_STYLE:
	return MAXIMUM(EPOCH2_STRING_LEN,MIN_VARIABLE_WIDTH);
      case EPOCH3_STYLE:
	return MAXIMUM(EPOCH3_STRING_LEN,MIN_VARIABLE_WIDTH);
      case EPOCH4_STYLE:
	return MAXIMUM(EPOCH4_STRING_LEN,MIN_VARIABLE_WIDTH);
      case EPOCHf_STYLE:
	break;                  /* Continue on... */
      case EPOCHx_STYLE:
	return MAXIMUM(EPOCHx_STRING_MAX,MIN_VARIABLE_WIDTH);
    }
  }
  /****************************************************************************
  * Check for an EPOCH16 data type.  Depending on the style the format may be
  * ignored.
  ****************************************************************************/
  else if (EPOCH16dataType(Var->dataType)) {
    switch (opt.epochStyle) {
      case EPOCH0_STYLE:
        return MAXIMUM(EPOCH16_STRING_LEN,MIN_VARIABLE_WIDTH);
      case EPOCH1_STYLE:
        return MAXIMUM(EPOCH16_1_STRING_LEN,MIN_VARIABLE_WIDTH);
      case EPOCH2_STYLE:
        return MAXIMUM(EPOCH16_2_STRING_LEN,MIN_VARIABLE_WIDTH);
      case EPOCH3_STYLE:
        return MAXIMUM(EPOCH16_3_STRING_LEN,MIN_VARIABLE_WIDTH);
      case EPOCH4_STYLE:
        return MAXIMUM(EPOCH16_4_STRING_LEN,MIN_VARIABLE_WIDTH);
      case EPOCHf_STYLE:
        break;                  /* Continue on... */
      case EPOCHx_STYLE:
        return MAXIMUM(EPOCHx_STRING_MAX,MIN_VARIABLE_WIDTH);
    }
  }
  /****************************************************************************
  * Check for a TT2000 data type.  Always use ISO 8601 form.
  * ignored.
  ****************************************************************************/
  else if (TT2000dataType(Var->dataType)) {
    switch (opt.epochStyle) {
      case EPOCH0_STYLE:
      case EPOCH1_STYLE:
      case EPOCH2_STYLE:
      case EPOCH3_STYLE:
      case EPOCH4_STYLE:
        return MAXIMUM(TT2000_3_STRING_LEN,MIN_VARIABLE_WIDTH);
    }
  }
  /****************************************************************************
  * Check if a format exists.
  ****************************************************************************/
  if (Var->format != NULL) {
    width = FormatWidth(Var->format);
    if (width != 0) return MAXIMUM(MIN_VARIABLE_WIDTH,width);
  }
  /****************************************************************************
  * Either a format doesn't exist or is illegal.  Use the default width for
  * the data type.
  ****************************************************************************/
  switch (Var->dataType) {
    case CDF_BYTE:
    case CDF_INT1:
      width = 4;
      break;
    case CDF_UINT1:
      width = 3;
      break;
    case CDF_INT2:
      width = 6;
      break;
    case CDF_UINT2:
      width = 5;
      break;
    case CDF_INT4:
      width = 11;
      break;
    case CDF_INT8:
      width = 20;
      break;
    case CDF_UINT4:
      width = 10;
      break;
    case CDF_REAL4:
    case CDF_FLOAT:
    case CDF_REAL8:
    case CDF_DOUBLE:
      width = 12;
      break;
    case CDF_EPOCH:     /* Format (C/Fortran) style. */
      width = 12;
      break;
    case CDF_EPOCH16:	/* Format (C/Fortran) style. */
      width = 16;
      break;
    case CDF_TIME_TT2000:	/* Format (C/Fortran) style. */
      width = 29;
      break;
    default:            /* Unknown data type. */
      width = 0;
  }
  return MAXIMUM(MIN_VARIABLE_WIDTH,width);
}

/******************************************************************************
* OptDefaults.
******************************************************************************/

void OptDefaults () {
  if (simpleMode) {
    opt.eachFilter = FORCEeachFilterSIMPLE;
    opt.overallFilter = FORCEoverallFilterSIMPLE;
    opt.useFills = FORCEfillsSIMPLE;
    opt.useFORMAT = DEFAULTformatSIMPLE;
    opt.useFILLVAL = FORCEfillvalSIMPLE;
    opt.useVALIDMIN = FORCEvalidminSIMPLE;
    opt.useVALIDMAX = FORCEvalidmaxSIMPLE;
    opt.useMONOTON = FORCEmonotonSIMPLE;
    opt.horizontalMode = DEFAULThorizontalSIMPLE;
    opt.showRecord = DEFAULTrecordSIMPLE;
    opt.showIndices = DEFAULTindicesSIMPLE;
    opt.outputItem = DEFAULToutputSIMPLE;
    opt.epochStyle = DEFAULTepochSIMPLE;
    opt.majority = DEFAULTmajoritySIMPLE;
    opt.spacing = DEFAULTspacingSIMPLE;
    opt.singleFile = NAsingleSIMPLE;
    opt.encoding = BOO(NAnetworkSIMPLE,NETWORK_ENCODING,HOST_ENCODING);
    opt.showFiltered = NAshowSIMPLE;
    opt.exclusive = NAexclusiveSIMPLE;
    opt.deleteExisting = NAdeleteSIMPLE;
    opt.preAllocate = NApreAllocateSIMPLE;
    opt.textHeading = DEFAULTheadingSIMPLE;
    poundedheading = 0;
  }
  else {
    opt.eachFilter = DEFAULTeachFilterEXPORT;
    opt.overallFilter = DEFAULToverallFilterEXPORT;
    opt.useFills = DEFAULTfillsEXPORT;
    opt.useFORMAT = DEFAULTformatEXPORT;
    opt.useFILLVAL = DEFAULTfillvalEXPORT;
    opt.useVALIDMIN = DEFAULTvalidminEXPORT;
    opt.useVALIDMAX = DEFAULTvalidmaxEXPORT;
    opt.useMONOTON = DEFAULTmonotonEXPORT;
    opt.horizontalMode = DEFAULThorizontalEXPORT;
    opt.showRecord = DEFAULTrecordEXPORT;
    opt.showIndices = DEFAULTindicesEXPORT;
    opt.outputItem = DEFAULToutputEXPORT;
    opt.epochStyle = DEFAULTepochEXPORT;
    opt.majority = DEFAULTmajorityEXPORT;
    opt.spacing = DEFAULTspacingEXPORT;
    opt.singleFile = DEFAULTsingleEXPORT;
    opt.encoding = BOO(DEFAULTnetworkEXPORT,NETWORK_ENCODING,HOST_ENCODING);
    opt.showFiltered = DEFAULTshowEXPORT;
    opt.exclusive = DEFAULTexclusiveEXPORT;
    opt.deleteExisting = DEFAULTdeleteEXPORT;
    opt.preAllocate = DEFAULTpreAllocateEXPORT;
    opt.textHeading = DEFAULTheadingEXPORT;
    poundedheading = 0;
  }
  return;
}

/******************************************************************************
* LoadVarsTextFile.
******************************************************************************/

Logical LoadVarsTextFile (varsFile)
char *varsFile;
{
  FILE *varsfp;
  int  i, j;
  char line[CDF_VAR_NAME_LEN256+1], item[CDF_VAR_NAME_LEN256+1];
  Logical status;

  varsfp = fopen (varsFile, "r");
  if (varsfp == NULL) return FALSE;
  numVars = 0;
  numVarsO = 0;
  while (fgets(line,CDF_VAR_NAME_LEN256,varsfp) != NULL) {
    if (sscanf(line, "%s\n", item) != 1) continue;
    for (i = 0; line[i] != NUL; i++)
       if (line[i] == Nl) line[i] = '\0';
    RemoveLeadingBlanks (line);
    RemoveTrailingBlanks (line);
    if (line[0] == '\0' || strlen(line) == 0) continue;
    ++numVars;
  }
  if (numVars > 0) {
    vars = (char **) cdf_AllocateMemory ((size_t)sizeof(char *)*numVars, FatalError);
    for (i = 0; i < numVars; i++)
      vars[i] = (char *) cdf_AllocateMemory ((size_t)CDF_VAR_NAME_LEN256+1, 
                                             FatalError);
    j = 0;
    rewind(varsfp);
    while (fgets(line,CDF_VAR_NAME_LEN256,varsfp) != NULL) {
      if (sscanf(line, "%s\n", item) != 1) continue;
      for (i = 0; line[i] != NUL; i++)
        if (line[i] == Nl) line[i] = '\0';    
      RemoveLeadingBlanks (line);
      RemoveTrailingBlanks (line);
      if (line[0] == '\0' || strlen(line) == 0) continue;
      strcpyX (vars[j], line, CDF_VAR_NAME_LEN256);
      ++j;
    }
    status = TRUE;
  } else
    status = FALSE;

  fclose (varsfp);
  return status;
}

/******************************************************************************
* ParseEpochRange.
******************************************************************************/

Logical ParseEpochRange (epochStrings)
char **epochStrings;
{
  double epochTemp;
  int len1, len2;

  len1 = (int) strlen(epochStrings[0]);
  len2 = (int) strlen(epochStrings[1]);
  if (len1 != len2) {
    DisplayError ("Starting and Ending epochs don't match.");
    return FALSE;
  }
  if (len1 != 24 && len1 != 29 && len1 != 36) {
    DisplayError ("Illegal/invalid format for Starting/ending Epoch.");
    return FALSE;
  }
  if (len1 == 24) {
    epochStart = parseEPOCH (epochStrings[0]);
    epochEnd = parseEPOCH (epochStrings[1]);
  } else if (len1 == 36) {
    epochTemp = parseEPOCH16 (epochStrings[0], epoch16Start);
    epochTemp = parseEPOCH16 (epochStrings[1], epoch16End);
  } else {
    tt2000Start = parseTT2000 (epochStrings[0]);
    tt2000End = parseTT2000 (epochStrings[1]);
  }  
  return TRUE;
}


/*****************************************************************************
* FreeTemps.
******************************************************************************/

void FreeTemps ()
{
  int i;
  if (numVars > 0) {
    for (i = 0; i < numVars; ++i)
      if (vars[i] != NULL) cdf_FreeMemory  (vars[i], FatalError);
    cdf_FreeMemory (vars, FatalError);
  }
  if (numEpochs > 0) {
    for (i = 0; i < numEpochs; ++i)
      if (epochs[i] != NULL) free (epochs[i]);
    free (epochs);
    free (epochTypes);
  }
  if (numGroups > 0) {
    int j;
    for (i = 0; i < numGroups; ++i) {
      for (j = 0; j < numVarsInGroup[i]; ++j) {
        if (varsInGroup[i][j] != NULL) free (varsInGroup[i][j]);
        if (varPtrsInGroup[i][j] != NULL) free (varPtrsInGroup[i][j]);
      }
    }
    free (varsInGroup);
    free (varPtrsInGroup);
    free (numVarsInGroup);
  }
}


