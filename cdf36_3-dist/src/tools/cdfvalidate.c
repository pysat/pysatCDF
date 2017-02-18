/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                  Validate CDF internal records.
*
*  Version 1.0, 15-Jul-08, Peort Systems.
*
*  Modification history:
*
*   V1.0  15-Jul-08, M Liu      Original version.
*   V2.0  11-Mar-13, M Liu      Added "-quiet" option to show nothing if a
*                               validation is successful.
*
******************************************************************************/

#define CDFVALIDATE
#include "cdfvalidate.h"

/******************************************************************************
* Local macro definitions.
******************************************************************************/

/******************************************************************************
* Main. 
******************************************************************************/

#if !defined(win32) || (defined(win32) && defined(ALONE))
MAIN {
  Logical success = TRUE;
  strcpyX (pgmName, "CDFvalidate", MAX_PROGRAM_NAME_LEN);
  success = ValidateCDFs (argc, argv);
  return BOO(success,EXIT_SUCCESS_,EXIT_FAILURE_);
}
#endif 

/******************************************************************************
* ValidateCDFs.
*    Returns FALSE if an error occurred.
******************************************************************************/

Logical ValidateCDFs (argC, argV)
int argC;
char *argV[];
{
  int numParms;
  int count;
  char **CDFspec;
  QOP *qop; Logical qopError = FALSE;
  static char *validQuals[] = {
    "debug", "about", "validate", "novalidate", "quiet", NULL
  };
  static int optRequired[] = {
    FALSE, FALSE, FALSE, FALSE, FALSE, 0
  };
  char oText[MAX_oTEXT_LEN+1];
  char text[CDF_STATUSTEXT_LEN+1];
  CDFid id;
  CDFstatus status;
  long numZvars, numRecs, dataType, numElms, numDims, recVary,
       dimSizes[CDF_MAX_DIMS], dimVarys[CDF_MAX_DIMS];
  int  i, j, k, toStop, toStop2;
  CDFdata data;

  /****************************************************************************
  * Determine qualifiers/options/parameters.
  ****************************************************************************/
  switch (argC) {
    case 1:
      PageOLH ("cdfvalidate.olh", argV[0]);
      return TRUE;
    case 2:
      if (strcmp(argV[1],"-java") == 0) {
        pagingOn = FALSE;
        PageOLH ("cdfvalidatej.olh", argV[0]);
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
      * Check for the `validate' qualifier.
      ************************************************************************/
      qopError = qopError |! TFqualifier(qop,&useValidate,VALIDATEqual,
                                         NOVALIDATEqual,
                                         DEFAULTvalidateValidate, "validate");
      /************************************************************************
      * Check if which debug/display option was specified.
      ************************************************************************/
      count = 0;
      if (qop->qualEntered[DEBUGqual]) count++;
      if (qop->qualEntered[QUIETqual]) count++;
      switch (count) {
        case 0:
            debug = FALSE;
            quiet = FALSE;
          break;
        case 1:
          if (qop->qualEntered[DEBUGqual]) {
            debug = TRUE;
            quiet = FALSE;
            break;
          }
          if (qop->qualEntered[QUIETqual]) {
            quiet = TRUE;
            debug = FALSE;
            break;
          }
        default:
          DisplayError ("Specify only one debug or quiet qualifier.");
          qopError = TRUE;
      }
      /************************************************************************
      * Check for `debug' qualifier.
      ************************************************************************/
/*
      qopError = qopError |! TFqualifier(qop,&debug,DEBUGqual,
                                         NODEBUGqual,
                                         DEFAULTdebugValidate, "debug");
*/
      /************************************************************************
      * Check for `quiet' qualifier.
      ************************************************************************/
/*
      qopError = qopError |! TFqualifier(qop,&quiet,QUIETqual,
                                         NOQUIETqual,
                                         DEFAULTquietValidate, "quiet");
*/
      /************************************************************************
      * Get CDF path.
      ************************************************************************/
      if (qop->Nparms < 1) {
        DisplayError ("Missing parameter.");
        qopError = TRUE;
      }
      else {
	numParms = qop->Nparms;
	CDFspec = (char **) malloc(sizeof(char **) * qop->Nparms);
	for (i = 0; i < qop->Nparms; ++i) {
          CDFspec[i] = (char *) malloc(DU_MAX_PATH_LEN + 1);
          strcpyX (CDFspec[i], qop->parms[i], DU_MAX_PATH_LEN);
        }
      }
      /************************************************************************
      * Free QOP memory and check for an error.
      ************************************************************************/
      cdf_FreeMemory (qop, FatalError);
      if (qopError) return FALSE;
      break;
  }
  if (useValidate)
    CDFsetValidate (VALIDATEFILEon);
  else
    CDFsetValidate (VALIDATEFILEoff);
#if !defined(vms)
#if !defined(sun) && (!defined(WIN32) && !defined(__MINGW32__))
  if (debug) setenv ("CDF_VALIDATE_DEBUG", "yes", 1);
#else
#  if !defined(WIN32)
     if (debug) putenv ("CDF_VALIDATE_DEBUG=yes");
#  else
     if (debug) _putenv ("CDF_VALIDATE_DEBUG=yes");
#  endif
#endif
#else
  if (debug) setenv ("CDF$VALIDATE$DEBUG", "yes", 1);
#endif
  for (i = 0; i < numParms; ++i) {
    toStop = 0;
    /**************************************************************************
    * Display `validating' message.
    **************************************************************************/
    strcpyX (oText, "Validating \"", 0);
    strcatX (oText, CDFspec[i], 0);
    if (!EndsWithIgCase(CDFspec[i], ".cdf"))
      strcatX (oText, ".cdf", 0);
    strcatX (oText, "\"...", 0);
    if (!quiet) OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 0);
    /**************************************************************************
    * Open CDF file, which will validate the file.
    **************************************************************************/
    status = CDFlib (OPEN_, CDF_, CDFspec[i], &id,
                     NULL_);
    if (status < CDF_OK) {
      CDFlib (SELECT_, CDF_STATUS_, status,
              GET_, STATUS_TEXT_, text,
              NULL_);
      if (quiet) OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 0);
      printf ("%s\n", text);
      toStop = 1;
    }
    if (toStop == 0) {
      status = CDFlib (SELECT_, CDF_zMODE_, zMODEon2,
                       GET_, CDF_NUMzVARS_, &numZvars,
                       NULL_);
      k = 0;
      toStop2 = 0;
      while ((k < (int) numZvars) && (toStop2 == 0)) {
	status = CDFreadzVarAllByVarID (id, (long) k, &numRecs, &dataType,
			                &numElms, &numDims, dimSizes,
			                &recVary, dimVarys, &data);
        CDFdataFree (data);
        if (status < CDF_OK) {
          CDFlib (SELECT_, CDF_STATUS_, status,
                  GET_, STATUS_TEXT_, text,
                  NULL_);
          if (quiet) OutputWithMargin (stdout, oText, MAX_SCREENLINE_LEN, 0);
          printf ("%s\n", text);
          toStop2 = 1;
        }
        ++k;
      }
    }
    if (toStop == 0) {
      status = CDFlib (CLOSE_, CDF_,
                       NULL_);
      if (status < CDF_OK) {
        CDFlib (SELECT_, CDF_STATUS_, status,
                GET_, STATUS_TEXT_, text,
                NULL_);
        printf ("%s\n", text);
      }
      free (CDFspec[i]);
    }
  }
  free (CDFspec);
  if (numParms == 1) {
    if (status < CDF_OK) return FALSE;
    else return TRUE;
  } else
    return TRUE; 
}

