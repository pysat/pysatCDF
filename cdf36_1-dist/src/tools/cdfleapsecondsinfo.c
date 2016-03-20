/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF           CDFleapsecondsinfo - show CDF's leap seconds table info.
*
*  Version 1.0, 10-Apr-11, ADNET Systems.
*
*  Modification history:
*
*   V1.0  10-Apr-11, M Liu      Original version.
*
******************************************************************************/

#define CDFLEAPSECONDSINFO
#include "cdfleapsecondsinfo.h"

/******************************************************************************
* Global variables.
******************************************************************************/

/******************************************************************************
* Main.
******************************************************************************/

#if !defined(win32) || (defined(win32) && defined(ALONE))
MAIN {
  Logical success = TRUE;
  strcpyX (pgmName, "CDFleaptableinfo", MAX_PROGRAM_NAME_LEN);
  success = LeapTableInfo (argc, argv);
#if defined(DEBUG)
  if (cdf_FreeMemory(NULL,FatalError) > 0) DisplayWarning ("Abandoned buffers.");
#else
  cdf_FreeMemory (NULL, FatalError);
#endif
  return BOO(success,EXIT_SUCCESS_,EXIT_FAILURE_);
}
#endif

/******************************************************************************
* LeapTableInfo.
******************************************************************************/

Logical LeapTableInfo (argC, argV)
int argC;
char *argV[];
{
Logical qopError = FALSE;
char *table;
double **lt = NULL;
QOP *qop;
int fileStatus;
long year, month, day;
int ix, rows;
static char *validQuals[] = {
  "dump", "nodump", "about", NULL 
};
static int optRequired[] = {
  FALSE, FALSE, FALSE,
  0
};

/******************************************************************************
* Parse qualifiers/options/parameters (QOP).
******************************************************************************/

switch (argC) {
  case 1:
    PageOLH ("cdfleaptableinfo.olh", argV[0]);
    return TRUE;
  case 2:
    if (strcmp(argV[1],"-java") == 0) {
      pagingOn = FALSE;
      PageOLH ("cdfleaptableinfo.olh", argV[0]);
      return TRUE;
    }
  default:
    pagingOn = FALSE;
    qop = Qop (argC, argV, validQuals, optRequired);
    if (qop == NULL) return FALSE;
    /************************************************************************
    * Check for the `dump' qualifier.
    ************************************************************************/
    qopError = !TFqualifier(qop,&dump,DUMPqual,NODUMPqual,
                            DEFAULTdumpLEAPTABLE,"dump");
    /**************************************************************************
    * Check for `about' qualifier.
    **************************************************************************/
    if (qop->qualEntered[ABOUTqual]) {
      DisplayIdentification (pgmName);
      cdf_FreeMemory (qop, FatalError);
      return TRUE;
    }
    /**************************************************************************
    * Free QOP memory and check for an error.
    **************************************************************************/
    cdf_FreeMemory (qop, FatalError);
    if (qopError) return FALSE;
    break;
}

  /****************************************************************************
  * Display information.
  ****************************************************************************/
  WriteOutSO ("Info for CDF leap second table...\n");
  table = CDFgetLeapSecondsTableEnvVar();
  fileStatus = CDFgetLeapSecondsTableStatus();
  if (table == NULL) {
#if defined(vms)
    WriteOutSO ("Environment Variable: CDF$LEAPSECONDSTABLE is NOT defined....\n");
#else
    WriteOutSO ("Environment Variable: CDF_LEAPSECONDSTABLE is NOT defined....\n");
#endif
    WriteOutSO ("Thus, the hard-coded table is used.\n");
  } else {
    if (fileStatus == 0) {
#if defined(vms)
      WriteOutSO ("Environment Variable: CDF$LEAPSECONDSTABLE is defined as: %s\n", table);
      WriteOutSO ("                      but the file is invalid....\n");
#else
      WriteOutSO ("Environment Variable: CDF_LEAPSECONDSTABLE is defined as: %s\n", table);
      WriteOutSO ("                      but the file is invalid....\n"); 
#endif
      WriteOutSO ("Thus, the hard-coded table is used.\n");
    } else
      WriteOutSO ("CDF's leap seconds table is based on the file: %s\n", table);
  }
#if defined(vms)
  CDFgetLastDateinLeapSecondsTBL (&year, &month, &day);
#else
  CDFgetLastDateinLeapSecondsTable (&year, &month, &day);
#endif
  WriteOutSO ("The last date a leap second was added to the table is: %2.2ld-%2.2ld-%2.2ld\n",
              year, month, day);
  if (dump == TRUE) {
    rows = CDFgetRowsinLeapSecondsTable();
    lt = (double **) malloc (sizeof(double *) * rows);
    if (lt == NULL) {
      WriteOutSO (" Error space allocation...\n");
      exit (1);
    }
    for (ix = 0; ix < rows; ++ix) {
      lt[ix] = (double *) malloc (sizeof(double) * 6);
      if (lt[ix] == NULL) {
        WriteOutSO (" Error space allocation...\n");
        free (lt);
        exit (1);
      }
    }
    CDFfillLeapSecondsTable(lt);
    WriteOutSO ("    \t     \t     \t  Leap \t\t Drift\t\t Drift\n");
    WriteOutSO ("Year\tMonth\t  Day\tSeconds\t\t    1 \t\t    2\n");
    for (ix = 0; ix < rows; ++ix) {
      WriteOutSO ("%lg\t  %lg\t   %lg\t%lg\t\t %lg\t\t %lg\n",lt[ix][0],
                  lt[ix][1], lt[ix][2], lt[ix][3], lt[ix][4], lt[ix][5]);
    }
  }
  CDFClearLeapSecondsTable ();
  if (lt != NULL) {
    for (ix = 0; ix < rows; ++ix) free (lt[ix]);
    free (lt);
  }
  exit(1);

}

