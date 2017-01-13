/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/*****************************************************************************e
*
*  NSSDC/CDF                            Quick Start Test Program (C interface).
*
*  Version 1.11, 10-Sep-96, Hughes STX.
*
*  Modification history:
*
*   V1.0   24-Jan-91, J Love     Original version (for CDF V2.0).
*   V1.1    7-Mar-91, J Love     Modified output display.
*   V1.2   27-May-91, J Love     Changed for CDF V2.1 enhancements.
*   V1.3   25-Jun-91, J Love     Renamed CDF for portability.
*   V1.4    2-Aug-91, J Love     Added MIPSEB encoding.  Use 'Exit'/'ExitBAD'.
*   V1.5   12-Sep-91, J Love     Modified for IBM-PC port.
*   V1.6   21-Apr-92, J Love     CDF V2.2.
*   V1.7   13-Nov-92, J Love     Borland C 3.0 (partial bracketing).
*   V1.8    9-Dec-93, J Love     CDF V2.4.  Only output if an error occurs
*                                (except for testing/success message).
*   V1.9   20-Dec-94, J Love     CDF V2.5.
*   V1.9a   5-Jan-95, J Love    Indentation on UNIX machines.
*   V1.9b  24-Jan-95, J Love    Better EPOCH test values.
*   V1.10   6-Apr-95, J Love    POSIX.
*   V1.10a 18-Apr-95, J Love    More POSIX.
*   V1.11  10-Sep-96, J Love    CDF V2.6.
*
******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cdf.h"

#if defined(vms)
#include <ssdef>
#define EXIT_SUCCESS_   SS$_NORMAL
#define EXIT_FAILURE_   SS$_ABORT
#else
#define EXIT_SUCCESS_   0
#define EXIT_FAILURE_   1
#endif

/******************************************************************************
* Increased stack size for Borland C on IBM PC.
******************************************************************************/

#if defined(BORLANDC)
extern unsigned _stklen = 12000u;
#endif

/******************************************************************************
* Macros/prototypes.
******************************************************************************/

#define N_DIMS          2
#define DIM_0_SIZE      2
#define DIM_1_SIZE      3

void QuitCDF PROTOARGs((CDFstatus status, char *where, CDFid id));
void QuitEPOCH PROTOARGs((char *where));

/******************************************************************************
* Main.
******************************************************************************/

int main () {
CDFid           id;
CDFstatus       status;
int             dim_n;
static long     encoding = IBMPC_ENCODING;
static long     actual_encoding = IBMPC_ENCODING;
static long     majority = ROW_MAJOR;
static long     checkSum = MD5_CHECKSUM, checkSum_out;
static long     numDims = N_DIMS;
static long     dimSizes[N_DIMS] = { DIM_0_SIZE, DIM_1_SIZE };
static long     varDataType = { CDF_INT2 };
long            varDataType_out;
static long     varNumElements = { 1 };
long            varNumElements_out;
long            varNum_out;
static short    varValues[DIM_0_SIZE][DIM_1_SIZE] = {{1,2,3},{4,5,6}};
long            indices[N_DIMS];
static long     recNum = { 0 };
short           varValue_out;
static long     recStart = { 0 };
static long     recCount = { 1 };
static long     recInterval = { 1 };
static long     counts[N_DIMS] = { DIM_0_SIZE, DIM_1_SIZE };
static long     intervals[N_DIMS] = { 1, 1 };
short           varBuffer_out[DIM_0_SIZE][DIM_1_SIZE];
long            attrNum_out;
static long     entryNum = { 2 };
long            maxEntry_out;
static long     attrScope = { GLOBAL_SCOPE };
long            attrScope_out;
static long     attrDataType = { CDF_INT2 };
long            attrDataType_out;
static long     attrNumElements = { 1 };
long            attrNumElements_out;
static short    attrValue = { 1 };
short           attrValue_out;
long            encoding_out;
long            majority_out;
long            numDims_out;
long            dimSizes_out[N_DIMS];
long            maxRec_out;
long            numVars_out;
long            numAttrs_out;
long            version_out;
long            release_out;

int             x0, x1, x;

static long     varRecVariance = { VARY };
long            varRecVariance_out;
static long     varDimVariances[N_DIMS] = { VARY, VARY };
long            varDimVariances_out[N_DIMS];

static char     varName[] = "VAR1";
static char     new_varName[] = "VAR2";
char            varName_out[CDF_VAR_NAME_LEN256+1];
static char     attrName[] = "ATTR1";
static char     new_attrName[] = "ATTR2";
char            attrName_out[CDF_ATTR_NAME_LEN256];
char            CopyrightText[CDF_COPYRIGHT_LEN+1];
char            errorText[CDF_ERRTEXT_LEN+1];

long            year = 2000;
long            month = 10;
long            day = 13;
long            hour = 12;
long            minute = 0;
long            second = 0;
long            msec = 0;
long            yearOut, monthOut, dayOut,
		hourOut, minuteOut, secondOut, msecOut;
double          epoch, epochOut;
char            epString[EPOCH_STRING_LEN+1];
char            epString1[EPOCH1_STRING_LEN+1];
char            epString2[EPOCH2_STRING_LEN+1];
char            epString3[EPOCH3_STRING_LEN+1];
char            epString4[EPOCH4_STRING_LEN+1];
static char     epStringTrue[EPOCH_STRING_LEN+1] = "13-Oct-2000 12:00:00.000";
static char     epString1True[EPOCH1_STRING_LEN+1] = "20001013.5000000";
static char     epString2True[EPOCH2_STRING_LEN+1] = "20001013120000";
static char     epString3True[EPOCH3_STRING_LEN+1]="2000-10-13T12:00:00.000Z";
static char     epString4True[EPOCH4_STRING_LEN+1]="2000-10-13T12:00:00.000";

/******************************************************************************
* Display title.
******************************************************************************/

printf ("Testing Standard (original)/C interface...\n");

/******************************************************************************
* Create CDF.
******************************************************************************/

status = CDFcreate ("TEST", numDims, dimSizes, encoding, majority, &id);
if (status < CDF_OK) {
  if (status == CDF_EXISTS) {
     status = CDFopen ("TEST", &id);
     if (status < CDF_OK) QuitCDF (status, "1.0", id);
     status = CDFdelete (id);
     if (status < CDF_OK) QuitCDF (status, "1.1", id);
     status = CDFcreate ("TEST", numDims, dimSizes, encoding, majority, &id);
     if (status < CDF_OK) QuitCDF (status, "1.2", id);
  }
  else
     QuitCDF (status, "1.3", id);
}

/******************************************************************************
* Create variable.
******************************************************************************/

status = CDFvarCreate (id, varName, varDataType, varNumElements,
		       varRecVariance, varDimVariances, &varNum_out);
if (status < CDF_OK) QuitCDF (status, "2.0", id);

/******************************************************************************
* Close CDF.
******************************************************************************/

status = CDFclose (id);
if (status < CDF_OK) QuitCDF (status, "3.0", id);

/******************************************************************************
* Reopen CDF.
******************************************************************************/

status = CDFopen ("TEST", &id);
if (status < CDF_OK) QuitCDF (status, "4.0", id);

/******************************************************************************
* Delete CDF.
******************************************************************************/

status = CDFdelete (id);
if (status < CDF_OK) QuitCDF (status, "5.0", id);

/******************************************************************************
* Create CDF again (previous delete will allow this).
******************************************************************************/

status = CDFcreate ("TEST", numDims, dimSizes, encoding, majority, &id);
if (status < CDF_OK) QuitCDF (status, "6.0", id);

/******************************************************************************
* Set checksum (to MD5).
******************************************************************************/

status = CDFsetChecksum (id, checkSum);
if (status < CDF_OK) QuitCDF (status, "6.5", id);

/******************************************************************************
* Create variable.
******************************************************************************/

status = CDFvarCreate (id, varName, varDataType, varNumElements,
		       varRecVariance, varDimVariances, &varNum_out);
if (status < CDF_OK) QuitCDF (status, "7.0", id);

/******************************************************************************
* PUT to variable.
******************************************************************************/

for (x0 = 0; x0 < DIM_0_SIZE; x0++)
   for (x1 = 0; x1 < DIM_1_SIZE; x1++) {
      indices[0] = x0;
      indices[1] = x1;
      status = CDFvarPut (id, CDFvarNum(id,varName), recNum, indices,
			  &varValues[x0][x1]);
      if (status < CDF_OK) QuitCDF (status, "8.0", id);
   }

/******************************************************************************
* GET from the variable.
******************************************************************************/

for (x0 = 0; x0 < DIM_0_SIZE; x0++)
   for (x1 = 0; x1 < DIM_1_SIZE; x1++) {
      indices[0] = x0;
      indices[1] = x1;
      status = CDFvarGet (id, CDFvarNum(id,varName), recNum, indices,
			  &varValue_out);
      if (status < CDF_OK) QuitCDF (status, "9.0", id);
      if (varValue_out != varValues[x0][x1]) QuitCDF (status, "9.1", id);
   }

/******************************************************************************
* HyperPUT to the variable.
******************************************************************************/

for (x0 = 0; x0 < DIM_0_SIZE; x0++)
   for (x1 = 0; x1 < DIM_1_SIZE; x1++)
      varValues[x0][x1] = -varValues[x0][x1];

indices[0] = 0;
indices[1] = 0;

status = CDFvarHyperPut (id, CDFvarNum(id,varName), recStart, recCount,
			 recInterval, indices, counts, intervals, varValues);
if (status < CDF_OK) QuitCDF (status, "10.0", id);

/******************************************************************************
* HyperGET from variable.
******************************************************************************/

status = CDFvarHyperGet (id, CDFvarNum(id,varName), recStart, recCount,
			 recInterval, indices, counts, intervals,
		       varBuffer_out);
if (status < CDF_OK) QuitCDF (status, "11.0", id);

for (x0 = 0; x0 < DIM_0_SIZE; x0++)
   for (x1 = 0; x1 < DIM_1_SIZE; x1++)
      if (varBuffer_out[x0][x1] != varValues[x0][x1]) QuitCDF (status,"11.1",id);

/******************************************************************************
* Create attribue.
******************************************************************************/

status = CDFattrCreate (id, attrName, attrScope, &attrNum_out);
if (status < CDF_OK) QuitCDF (status, "12.0", id);

/******************************************************************************
* PUT to attribute.
******************************************************************************/

status = CDFattrPut (id, CDFattrNum(id,attrName), entryNum, attrDataType,
		     attrNumElements, &attrValue);
if (status < CDF_OK) QuitCDF (status, "13.0", id);

/******************************************************************************
* GET from attribute.
******************************************************************************/

status = CDFattrGet (id, CDFattrNum(id,attrName), entryNum, &attrValue_out);
if (status < CDF_OK) QuitCDF (status, "14.0", id);

/******************************************************************************
* Get CDF documentation.
******************************************************************************/

status = CDFdoc (id, &version_out, &release_out, CopyrightText);
if (status < CDF_OK) QuitCDF (status, "15.0", id);

/******************************************************************************
* Inquire CDF.
******************************************************************************/

status = CDFinquire (id, &numDims_out, dimSizes_out, &encoding_out,
		     &majority_out, &maxRec_out, &numVars_out, &numAttrs_out);
if (status < CDF_OK) QuitCDF (status, "16.0", id);

if (numDims_out != numDims) QuitCDF (status, "16.1", id);
for (x = 0; x < N_DIMS; x++)
   if (dimSizes_out[x] != dimSizes[x]) QuitCDF (status, "16.2", id);
if (encoding_out != actual_encoding) QuitCDF (status, "16.3", id);
if (majority_out != majority) QuitCDF (status, "16.4", id);
if (maxRec_out != 0) QuitCDF (status, "16.5", id);
if (numVars_out != 1) QuitCDF (status, "16.6", id);
if (numAttrs_out != 1) QuitCDF (status, "16.7", id);

status = CDFgetChecksum (id, &checkSum_out);
if (status < CDF_OK) QuitCDF (status, "16.8", id);

if (checkSum_out != checkSum) QuitCDF (status, "16.9", id);

/******************************************************************************
* Rename variable.
******************************************************************************/

status = CDFvarRename (id, CDFvarNum(id,varName), new_varName);
if (status < CDF_OK) QuitCDF (status, "17.0", id);

/******************************************************************************
* Inquire variable.
******************************************************************************/

status = CDFvarInquire (id, CDFvarNum(id,new_varName), varName_out,
			&varDataType_out, &varNumElements_out,
			&varRecVariance_out, varDimVariances_out);
if (status < CDF_OK) QuitCDF (status, "18.0", id);

if (strcmp(varName_out,new_varName) != 0) QuitCDF (status, "18.1", id);
if (varDataType_out != varDataType) QuitCDF (status, "18.2", id);
if (varNumElements_out != varNumElements) QuitCDF (status, "18.3", id);
if (varRecVariance_out != varRecVariance) QuitCDF (status, "18.4", id);

for (dim_n = 0; dim_n < numDims; dim_n++)
   if (varDimVariances_out[dim_n] != varDimVariances[dim_n])
     QuitCDF (status, "18.4", id);

/******************************************************************************
* Rename attribute.
******************************************************************************/

status = CDFattrRename (id, CDFattrNum(id,attrName), new_attrName);
if (status < CDF_OK) QuitCDF (status, "20.0", id);

/******************************************************************************
* Inquire attribute.
******************************************************************************/

status = CDFattrInquire (id, CDFattrNum(id,new_attrName), attrName_out,
			 &attrScope_out, &maxEntry_out);
if (status < CDF_OK) QuitCDF (status, "22.0", id);

if (strcmp(attrName_out,new_attrName) != 0) QuitCDF (status, "22.1", id);
if (attrScope_out != attrScope) QuitCDF (status, "22.2", id);
if (maxEntry_out != entryNum) QuitCDF (status, "22.3", id);

/******************************************************************************
* Inquire attribute entry.
******************************************************************************/

status = CDFattrEntryInquire (id, CDFattrNum(id,new_attrName), entryNum,
			     &attrDataType_out, &attrNumElements_out);
if (status < CDF_OK) QuitCDF (status, "23.0", id);

if (attrDataType_out != attrDataType) QuitCDF (status, "23.1", id);
if (attrNumElements_out != attrNumElements) QuitCDF (status, "23.1", id);

/******************************************************************************
* Get error text.
******************************************************************************/

CDFerror (CDF_OK, errorText);

/******************************************************************************
* Close CDF.
******************************************************************************/

status = CDFclose (id);
if (status < CDF_OK) QuitCDF (status, "24.0", id);

/******************************************************************************
* Test EPOCH routines.
******************************************************************************/

epoch = computeEPOCH (year, month, day, hour, minute, second, msec);

encodeEPOCH (epoch, epString);
if (strcmp(epString,epStringTrue)) QuitEPOCH ("30.0");

epochOut = parseEPOCH (epString);
if (epochOut != epoch) QuitEPOCH ("31.1");

encodeEPOCH1 (epoch, epString1);
if (strcmp(epString1,epString1True)) QuitEPOCH ("30.2");

epochOut = parseEPOCH1 (epString1);
if (epochOut != epoch) QuitEPOCH ("31.3");

encodeEPOCH2 (epoch, epString2);
if (strcmp(epString2,epString2True)) QuitEPOCH ("30.4");

epochOut = parseEPOCH2 (epString2);
if (epochOut != epoch) QuitEPOCH ("31.5");

encodeEPOCH3 (epoch, epString3);
if (strcmp(epString3,epString3True)) QuitEPOCH ("30.6");

epochOut = parseEPOCH3 (epString3);
if (epochOut != epoch) QuitEPOCH ("31.7");

encodeEPOCH4 (epoch, epString4);
if (strcmp(epString4,epString4True)) QuitEPOCH ("30.8");

epochOut = parseEPOCH4 (epString4);
if (epochOut != epoch) QuitEPOCH ("31.9");

EPOCHbreakdown (epoch, &yearOut, &monthOut, &dayOut, &hourOut, &minuteOut,
		&secondOut, &msecOut);
if (yearOut != year) QuitEPOCH ("32.1");
if (monthOut != month) QuitEPOCH ("32.2");
if (dayOut != day) QuitEPOCH ("32.3");
if (hourOut != hour) QuitEPOCH ("32.4");
if (minuteOut != minute) QuitEPOCH ("32.5");
if (secondOut != second) QuitEPOCH ("32.6");
if (msecOut != msec) QuitEPOCH ("32.7");

/******************************************************************************
* Successful completion.
******************************************************************************/

return EXIT_SUCCESS_;
}

/******************************************************************************
* QuitCDF.
******************************************************************************/

void QuitCDF (status, where, id)
CDFstatus status;
char *where;
CDFid id;
{
  char text[CDF_STATUSTEXT_LEN+1];
  printf ("Aborting at %s...\n", where);
  if (status < CDF_OK) {
    CDFerror (status, text);
    printf ("%s\n", text);
  }
  CDFclose (id);
  printf ("...test aborted.\n");
  exit (EXIT_FAILURE_);
}

/******************************************************************************
* QuitEPOCH.
******************************************************************************/

void QuitEPOCH (where)
char *where;
{
  printf ("Aborting at %s...test aborted.\n", where);
  exit (EXIT_FAILURE_);
}

