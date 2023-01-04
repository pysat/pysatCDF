/******************************************************************************
*
*  NSSDC/CDF           Reads the CDF file created by example1.c using the 
*                      New Standard Interface that was introduced in CDF 3.1.  
*                      Or, other CDF file if passed in as a sole parameter.
*
*  History:
*
*   V1.0  17-Jan-2006, D. Han              Original version
*   V1.1  27-Jun-2008, M. Liu              Modified to use dynamic buffer
*                                          allocation
*
******************************************************************************/

/******************************************************************************
* Necessary include files.
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include "cdf.h"

/******************************************************************************
* Function prototypes.
******************************************************************************/

void StatusHandler (long);
void DynamicErrorHandle (CDFid);

/******************************************************************************
* MAIN.
******************************************************************************/
int main (int argc, char **argv) {

CDFid id;            /* CDF identifier. */
CDFstatus status;    /* CDF completion status. */

void  *buffer = NULL, *units, *lat;
void  *time, *image, *pixelValue;
char  varName[CDF_VAR_NAME_LEN256+1],
      attrName[CDF_ATTR_NAME_LEN256+1], subincrement,
      copyright[CDF_COPYRIGHT_LEN+1];
int   i, j;
long  TimeVarNum, ImageVarNum, LatVarNum, attrNum, recNum, numDims, recVary,
      dimIndices[CDF_MAX_DIMS], dimCounts[CDF_MAX_DIMS], 
      dimInterval[CDF_MAX_DIMS], dimSizes[CDF_MAX_DIMS], dimVarys[CDF_MAX_DIMS];
long  datatype, numElements, numRecs, arraySize, attrScope, maxgEntry,
      maxrEntry, maxzEntry;
long  encoding, majority, maxrRec, numrVars, maxzRec, numzVars, numAttrs;
long  version, release, increment;
long  ilen;

/******************************************************************************
* Get the current CDF library version number.
******************************************************************************/
status = CDFgetLibraryVersion (&version, &release, &increment, &subincrement);
if (status != CDF_OK) StatusHandler (status);
printf ("Current CDF library version: CDF %d.%d.%d\n\n",
        version, release, increment);

status = CDFgetLibraryCopyright (copyright);
if (status != CDF_OK) StatusHandler (status);
printf ("CDF library copyright:\n\t%s\n\n", copyright);

/******************************************************************************
* Open the CDF.
******************************************************************************/
if (argc > 1)
  status = CDFopenCDF (argv[1], &id);
else
  status = CDFopenCDF ("example1", &id);
if (status != CDF_OK) StatusHandler (status);

status = CDFgetCopyright (id, copyright);
if (status != CDF_OK) StatusHandler (status);
printf ("CDF copyright:\n\t%s\n\n", copyright);

/* Get the CDF version that was used to create this CDF file */
status = CDFgetVersion (id, &version, &release, &increment);
if (status != CDF_OK) StatusHandler (status);

status = CDFinquireCDF (id, 
                        &numDims, dimSizes,        /* only good for rVars */
                        &encoding, &majority, 
                        &maxrRec, &numrVars,       /* only good for rVars */
                        &maxzRec, &numzVars, &numAttrs);
if (status != CDF_OK) StatusHandler (status);

printf ("CDF file name: example1.cdf\n");
printf ("\tVersion: CDF %d.%d.%d\n", version, release, increment);
printf ("\tNumber of variables:  %d rVars, %d zVars\n", numrVars, numzVars);
printf ("\tNumber of attributes (global & variable): %d\n", numAttrs);
printf ("\tMax record number for zVariables: %d\n\n", maxzRec);

/******************************************************************************
* Read the value of the global attribute named TITLE.
******************************************************************************/
attrNum = CDFgetAttrNum (id, "TITLE");
if (attrNum < CDF_OK) StatusHandler (status);

status = CDFinquireAttr (id, attrNum, attrName, &attrScope, 
                         &maxgEntry, 
                         &maxrEntry,    /* only applicable for variable attr */
                         &maxzEntry);   /* only applicable for variable attr */
if (status != CDF_OK) StatusHandler (status);

if (attrScope == GLOBAL_SCOPE) {
    printf ("Global attribute TITLE:\n");
    for (i=0; i <= maxgEntry; i++) {
        status = CDFinquireAttrgEntry (id, attrNum, (long) i, &datatype, 
                                       &numElements);
        if (status != CDF_OK) StatusHandler (status);
	
	status = CDFgetDataTypeSize (datatype, &ilen);	
	ilen = ilen * numElements;
	if (datatype == CDF_CHAR || datatype == CDF_UCHAR) ++ilen;
	buffer = (void *) malloc (ilen);	
	if (buffer == NULL) DynamicErrorHandle (id);

        status = CDFgetAttrgEntry (id, attrNum, (long) i, buffer);
        if (status != CDF_OK) StatusHandler (status);
        ((char *) buffer)[numElements] = '\0';
        printf ("    entry #%d: %s\n", i, buffer);
	free (buffer);
    }
}

/******************************************************************************
* Read the value of the zVariable attribute named UNITS and FIELDNAM that
* are associated with the Time variable 
******************************************************************************/
TimeVarNum = CDFgetVarNum (id, "Time");
if (TimeVarNum < CDF_OK) StatusHandler (TimeVarNum);

attrNum = CDFgetAttrNum (id, "FIELDNAM");
if (attrNum < CDF_OK) StatusHandler (attrNum);

status = CDFinquireAttrzEntry (id, attrNum, TimeVarNum, 
                               &datatype, &numElements);
if (status != CDF_OK) StatusHandler (status);

if (datatype == CDF_CHAR) { 
    buffer = (char *) malloc (sizeof(char) * numElements + 1);
    if (buffer == NULL) DynamicErrorHandle (id);
    status = CDFgetAttrzEntry (id, attrNum, TimeVarNum, buffer);
    ((char *) buffer)[numElements] = '\0';
    printf ("\nTime:\n    FIELDNAME = %s\n", buffer);
    free (buffer);
} 
    
attrNum = CDFgetAttrNum (id, "UNITS");
if (attrNum < CDF_OK) StatusHandler (attrNum);

status = CDFinquireAttrzEntry (id, attrNum, TimeVarNum,
                               &datatype, &numElements);
if (status != CDF_OK) StatusHandler (status);

status = CDFgetDataTypeSize (datatype, &ilen);
ilen = ilen * numElements;
if (datatype == CDF_CHAR || datatype == CDF_UCHAR) ++ilen;
units = (void *) malloc (ilen);
if (buffer == NULL) DynamicErrorHandle (id);
status = CDFgetAttrzEntry (id, attrNum, TimeVarNum, units);
if (status != CDF_OK) StatusHandler (status);
((char *)units)[numElements] = '\0';
printf ("    UNITS = %s\n", units);
free (units);

/******************************************************************************
* Read data for 'Time' zVariable.
******************************************************************************/
recNum = 0L;
status = CDFgetDataTypeSize (datatype, &ilen);
ilen = ilen * numElements;
if (datatype == CDF_CHAR || datatype == CDF_UCHAR) ++ilen;
time = (void *) malloc (ilen);
if (time == NULL) DynamicErrorHandle (id);
status = CDFgetzVarRecordData (id, TimeVarNum, recNum, time);
if (status != CDF_OK) StatusHandler (status);

printf ("Time:\n    Record #1: %d\n", *(int *) time);
recNum = 1L;
status = CDFgetzVarRecordData (id, TimeVarNum, recNum, time);
if (status != CDF_OK) StatusHandler (status);
printf ("    Record #2: %d\n", *(int *)time);
free (time);

/******************************************************************************
* Delete the second record from 'Time' zVariable.  Record number starts at 0.
******************************************************************************/
status = CDFdeletezVarRecords (id, TimeVarNum,
                               1L,     /* start record number */
                               1L);    /* end record number */ 
if (status != CDF_OK) StatusHandler (status);

/******************************************************************************
* Read data for 'Latitude' zVariable.
******************************************************************************/
dimIndices[0] = 0;
dimIndices[1] = 0;
recNum = 0;

LatVarNum = CDFgetVarNum(id,"Latitude");
if (LatVarNum < CDF_OK) StatusHandler (LatVarNum);

status = CDFsetzVarSeqPos (id, LatVarNum, recNum, dimIndices);
if (status != CDF_OK) StatusHandler (status);

printf ("\nLatitude:\n    Record #1:\n");

/* Read one value at a time - inefficient way of reading data if the whole */
/* array needs to be read.                                                 */
status = CDFgetDataTypeSize (datatype, &ilen);
ilen = ilen * numElements;
if (datatype == CDF_CHAR || datatype == CDF_UCHAR) ++ilen;
lat = (void *) malloc (ilen);
if (lat == NULL) DynamicErrorHandle (id);
for (i=0; i < 181; i++) {
     status = CDFgetzVarSeqData (id, LatVarNum, lat);
     if (status != CDF_OK) StatusHandler (status);
     printf ("%d ", *(short *)lat);
}
printf ("\n");
free (lat);

/******************************************************************************
* Read data for 'Image' zVariable (10 x 20 array).
******************************************************************************/
dimIndices[0] = 0;
dimIndices[1] = 0;
dimInterval[0] = 1;
dimInterval[1] = 1;

ImageVarNum = CDFgetVarNum (id, "Image");
if (ImageVarNum < CDF_OK) StatusHandler (ImageVarNum);

status = CDFinquirezVar (id, ImageVarNum, varName, &datatype,
                         &numElements, &numDims, dimSizes, &recVary, dimVarys);
if (status != CDF_OK) StatusHandler (status);

status = CDFgetzVarNumRecsWritten (id, ImageVarNum, &numRecs);
if (status != CDF_OK) StatusHandler (status);

status = CDFgetDataTypeSize (datatype, &ilen);
ilen = ilen * numElements;
if (buffer == NULL) DynamicErrorHandle (id);

arraySize = 1;
for (i=0; i < numDims; i++) {
     arraySize *= dimSizes[i]; 
     dimCounts[i] = dimSizes[i];
}
image = (void *) malloc (ilen * arraySize * numRecs);

/* read 3 records in one scoop */
status = CDFhyperGetzVarData (id, ImageVarNum,
                              0L,           /* record start */
                              3L,           /* # of records to read */
                              1L,           /* record interval */
                              dimIndices,   /* dimension dimIndices */
                              dimCounts,    /* dimension counts */
                              dimInterval,  /* dimension interval */
                              image);
if (status != CDF_OK) StatusHandler (status);
printf ("\nImage: \n");
for (i=0; i < numRecs; i++) {
    printf ("Record # %d:\n", i);
    for (j=0; j < arraySize; j++)
        printf ("%d ", *(((int *)image)+i*arraySize+j));
    printf ("\n\n"); 
}
free (image);

/* Read a single value - image[1,1] of the second record */
recNum = 1;             /* record number starts at 0 */
dimIndices[0] = 1;
dimIndices[1] = 1;

pixelValue = (void *) malloc (ilen);
if (pixelValue == NULL) DynamicErrorHandle (id);
status = CDFgetzVarData (id, ImageVarNum, recNum, dimIndices, pixelValue);
if (status != CDF_OK) StatusHandler (status);
printf ("Image record #2:[1,1] = %d\n", *(int *)pixelValue);
free (pixelValue);

/******************************************************************************
* Close CDF.
******************************************************************************/
status = CDFcloseCDF (id);
if (status != CDF_OK) StatusHandler (status);

return 0;
}

/******************************************************************************
* Status handler.
******************************************************************************/
void StatusHandler (status)
CDFstatus status;
{
char message[CDF_STATUSTEXT_LEN+1];

if (status < CDF_WARN) {
  printf ("An error has occurred, halting...\n");
  CDFgetStatusText (status, message);
  printf ("%s\n", message);
  exit (1);
}
else
  if (status < CDF_OK) {
    printf ("Warning, function may not have completed as expected...\n");
    CDFgetStatusText (status, message);
    printf ("%s\n", message);
  }
  else
    if (status > CDF_OK) {
      printf ("Function completed successfully, but be advised that...\n");
      CDFgetStatusText (status, message);
      printf ("%s\n", message);
    }       
return;
}

/******************************************************************************
* Dynamic allocation error handler.
******************************************************************************/
void DynamicErrorHandle (id)
CDFid id;
{
  CDFstatus status;
  printf ("An error has occurred while doing malloc, halting...\n");
  status = CDFcloseCDF (id);
  if (status != CDF_OK) StatusHandler (status);
  return;
}
