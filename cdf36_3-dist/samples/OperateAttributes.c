/******************************************************************************
*
*  SPDF/CDF          Sample program for operating attributes/entries in a CDF.
*
*  Version 1.0, 09-Dec-05
*
*  Modification history:
*
*   V1.0  09-Dec-05, M Liu      Original version (for CDF V3.1).
*                               A simple program to add and delete 
*                               attributes/entries to and from a CDF. 
*                               The global attributes apply to the CDF as a 
*                               whole, variable attributes
*                               apply to variables, zVariables in this sample
*                               program. It also shows how to respecify and
*                               verify the specifications of the entries.
******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cdf.h"

#define EXIT_SUCCESS_   0
#define EXIT_FAILURE_   1

/******************************************************************************
* Macros/prototypes.
******************************************************************************/

void QuitCDF PROTOARGs((char *where, CDFstatus status));

/******************************************************************************
* Main.
******************************************************************************/

int main () {
CDFid id;
CDFstatus status;
static long dimSizes[1] = { 2 };
static long dimSizes1[2] = { 2, 3 };
static long dimSizes2[1] = { 5 };
static long var1DataType = { CDF_INT2 };
static long var2DataType = { CDF_CHAR };
static long var1NumElements = { 1 };
static long var2NumElements = { 8 };
long var1Num_out, var2Num_out;
static long var1RecVariance = { VARY };
static long var2RecVariance = { VARY };
static long var1DimVariances[2] = { VARY, VARY };
static long var2DimVariances[1] = { VARY };
static char var1Name[] = "VAR1a"; 
static char var2Name[] = "zVARa1"; 
long attrNum_out;
static long entryNum = { 2 };
long maxEntry_out;
static long attrScope = { GLOBAL_SCOPE };
static long attrScope2 = { VARIABLE_SCOPE };
static long attrScope3 = { VARIABLE_SCOPE };
long attrScope_out;
static long entryDataType = { CDF_INT2 };
static long entryDataTypeNew = { CDF_UINT2 };
long entryDataType_out;
static long entryNumElems = { 1 };
long entryNumElems_out;
static short entryValue = { 1 };
short entryValue_out;
long numAttrs_out;
int i;
static char attrName[] = "ATTR1";
static char attrName2[] = "ATTR2";
static char attrName3[] = "ATTR3";
static char new_attrName[] = "ATTR1a";
char attrName_out[CDF_ATTR_NAME_LEN256];
static char zEntryValue1 = { 4 };
char zEntryValueOut1;
static double zEntryValue2 = { 4.0 };
double zEntryValueOut2;
long maxGentry, numGentries, 
     maxZentry, numZentries, numGattrs, numVattrs;
long entryNumOut1, entryNumOut2, entryNumOut3;

/******************************************************************************
* Create CDF.
******************************************************************************/
/* CDFsetFileBackwardFlag(BACKWARDFILEon); */
status = CDFlib (CREATE_, CDF_, "TEST", 0L, dimSizes, &id,
                 NULL_);

if (status < CDF_OK) {
  if (status == CDF_EXISTS) {
    status = CDFlib (OPEN_, CDF_, "TEST", &id,
                     NULL_);
    if (status < CDF_OK) QuitCDF ("1.0", status);

     status = CDFlib (DELETE_, CDF_,
                      NULL_);
     if (status < CDF_OK) QuitCDF ("1.1", status);
   
     status = CDFlib (CREATE_, CDF_, "TEST", 0L, dimSizes, &id,
                      NULL_);
     if (status < CDF_OK) QuitCDF ("1.2", status);
  }
  else
     QuitCDF ("1.3", status);
} 

/******************************************************************************
* Create variables.
******************************************************************************/

status = CDFlib (CREATE_, zVAR_, var1Name, var1DataType, var1NumElements,
                                 2L, dimSizes1,
                                 var1RecVariance, var1DimVariances,
                                 &var1Num_out,
                 NULL_);
if (status < CDF_OK) QuitCDF ("2.0", status);

status = CDFlib (CREATE_, zVAR_, var2Name, var2DataType, var2NumElements,
                                 1L, dimSizes2,
                                 var2RecVariance, var2DimVariances,
                                 &var2Num_out,
                 NULL_);
if (status < CDF_OK) QuitCDF ("2.1", status);

/******************************************************************************
* Create attributes.
******************************************************************************/

status = CDFlib (CREATE_, ATTR_, attrName, attrScope, &attrNum_out,
			  ATTR_, attrName2, attrScope2, &attrNum_out,
                          ATTR_, attrName3, attrScope3, &attrNum_out,
		 NULL_);
if (status < CDF_OK) QuitCDF ("3.0", status);

/******************************************************************************
* Write to attributes.
******************************************************************************/

status = CDFlib (SELECT_, ATTR_, 0L,
                          gENTRY_, entryNum,
		 PUT_, gENTRY_DATA_, entryDataType, entryNumElems,
				     &entryValue,
		 SELECT_, ATTR_, 1L,
			  zENTRY_NAME_, var1Name,
		 PUT_, zENTRY_DATA_, CDF_BYTE, 1L, &zEntryValue1,
		 SELECT_, ATTR_, 2L,
			  zENTRY_NAME_, var2Name,
		 PUT_, zENTRY_DATA_, CDF_REAL8, 1L, &zEntryValue2,
		 NULL_);
if (status < CDF_OK) QuitCDF ("4.0", status);

/******************************************************************************
* Confirm entry numbers.
******************************************************************************/

status = CDFlib (SELECT_, ATTR_, 0L,
		 CONFIRM_, gENTRY_, &entryNumOut1,
		 SELECT_, ATTR_, 1L,
      		 CONFIRM_, zENTRY_, &entryNumOut2,
		 SELECT_, ATTR_, 2L,
      		 CONFIRM_, zENTRY_, &entryNumOut3,
		 NULL_);
if (status < CDF_OK) QuitCDF ("5.0", status);

if (entryNumOut1 != 2) QuitCDF ("5.1", status);
if (entryNumOut2 != 1) QuitCDF ("5.2", status);
if (entryNumOut3 != 1) QuitCDF ("5.3", status);

/******************************************************************************
* Read from attributes.
******************************************************************************/

status = CDFlib (SELECT_, ATTR_, 0L,
			  gENTRY_, entryNum,
		 CONFIRM_, CURgENTRY_EXISTENCE_,
		 GET_, gENTRY_DATA_, &entryValue_out,
		 SELECT_, ATTR_, 1L,
			  zENTRY_, 0L,
		 CONFIRM_, CURzENTRY_EXISTENCE_,
		 GET_, zENTRY_DATA_, &zEntryValueOut1,
		 SELECT_, ATTR_, 2L,
			  zENTRY_, 1L,
		 CONFIRM_, CURzENTRY_EXISTENCE_,
		 GET_, zENTRY_DATA_, &zEntryValueOut2,
		 NULL_);
if (status < CDF_OK) QuitCDF ("6.0", status);

if (entryValue_out != entryValue) QuitCDF ("6.1", status);
if (zEntryValue1 != zEntryValueOut1) QuitCDF ("6.2", status);
if (zEntryValue2 != zEntryValueOut2) QuitCDF ("6.3", status);

/******************************************************************************
* Confirm existence of attributes/entries.
******************************************************************************/

status = CDFlib (CONFIRM_, ATTR_EXISTENCE_, attrName3,
		 NULL_);
if (status < CDF_OK) QuitCDF ("7.0", status);

status = CDFlib (SELECT_, ATTR_, 0L,
		 CONFIRM_, gENTRY_EXISTENCE_, entryNum,
		 SELECT_, ATTR_, 1L,
		 CONFIRM_, zENTRY_EXISTENCE_, 0L,
		 SELECT_, ATTR_, 2L,
		 CONFIRM_, zENTRY_EXISTENCE_, 1L,
		 NULL_);
if (status < CDF_OK) QuitCDF ("7.1", status);

/******************************************************************************
* Inquire CDF.
******************************************************************************/

status = CDFlib (GET_, CDF_NUMATTRS_, &numAttrs_out,
		 NULL_);
if (status < CDF_OK) QuitCDF ("8.0", status);

if (numAttrs_out != 3) QuitCDF ("8.1", status);

/******************************************************************************
* Rename attribute.
******************************************************************************/

status = CDFlib (SELECT_, ATTR_NAME_, attrName,
		 PUT_, ATTR_NAME_, new_attrName,
		 NULL_);
if (status < CDF_OK) QuitCDF ("9.0", status);

/******************************************************************************
* Inquire attribute.
******************************************************************************/

status = CDFlib (GET_, ATTR_NAME_, attrName_out,
		       ATTR_SCOPE_, &attrScope_out,
		       ATTR_MAXgENTRY_, &maxEntry_out,
		 CONFIRM_, ATTR_, &attrNum_out,
		 NULL_);
if (status < CDF_OK) QuitCDF ("10.0", status);

if (strcmp(attrName_out,new_attrName) != 0) QuitCDF ("10.1", status);
if (attrScope_out != attrScope) QuitCDF ("10.2", status);
if (maxEntry_out != entryNum) QuitCDF ("10.3", status);
if (attrNum_out != 0L) QuitCDF ("10.4", status);

/******************************************************************************
* Inquire attribute entries.
******************************************************************************/

status = CDFlib (SELECT_, ATTR_, 0L,
			  gENTRY_, entryNum,
		 GET_, gENTRY_DATATYPE_, &entryDataType_out,
		       gENTRY_NUMELEMS_, &entryNumElems_out,
		 NULL_);
if (status < CDF_OK) QuitCDF ("11.0", status);

if (entryDataType_out != entryDataType) QuitCDF ("11.1", status);
if (entryNumElems_out != entryNumElems) QuitCDF ("11.2", status);

status = CDFlib (SELECT_, ATTR_, 1L,
			  zENTRY_, 0L,
		 GET_, zENTRY_DATATYPE_, &entryDataType_out,
		       zENTRY_NUMELEMS_, &entryNumElems_out,
		 NULL_);
if (status < CDF_OK) QuitCDF ("11.3", status);

if (entryDataType_out != CDF_BYTE) QuitCDF ("11.4", status);
if (entryNumElems_out != 1L) QuitCDF ("11.5", status);

status = CDFlib (SELECT_, ATTR_, 2L,
			  zENTRY_, 1L,
		 GET_, zENTRY_DATATYPE_, &entryDataType_out,
		       zENTRY_NUMELEMS_, &entryNumElems_out,
		 NULL_);
if (status < CDF_OK) QuitCDF ("11.6", status);

if (entryDataType_out != CDF_REAL8) QuitCDF ("11.7", status);
if (entryNumElems_out != 1L) QuitCDF ("11.8", status);

/******************************************************************************
* Inquire CDF.
******************************************************************************/

status = CDFlib (SELECT_, ATTR_, 0L,
		 GET_, CDF_NUMgATTRS_, &numGattrs,
		       CDF_NUMvATTRS_, &numVattrs,
		       ATTR_MAXgENTRY_, &maxGentry,
		       ATTR_NUMgENTRIES_, &numGentries,
		 SELECT_, ATTR_, 1L,
		 GET_, ATTR_MAXzENTRY_, &maxZentry,
		       ATTR_NUMzENTRIES_, &numZentries,
		 NULL_);
if (status < CDF_OK) QuitCDF ("12.0", status);

if (numGattrs != 1) QuitCDF ("12.1", status);
if (numVattrs != 2) QuitCDF ("12.2", status);
if (maxGentry != entryNum) QuitCDF ("12.3", status);
if (numGentries != 1) QuitCDF ("12.4", status);
if (maxZentry != 0) QuitCDF ("12.5", status);
if (numZentries != 1) QuitCDF ("12.6", status);

/******************************************************************************
* Modify entries/attribute.
******************************************************************************/

status = CDFlib (SELECT_, ATTR_, 0L,
			  gENTRY_, entryNum,
		 PUT_, gENTRY_DATASPEC_, entryDataTypeNew, entryNumElems,
		 SELECT_, ATTR_, 1L,
			  zENTRY_, 0L,
		 PUT_, zENTRY_DATASPEC_, CDF_UINT1, 1L,
		 SELECT_, ATTR_, 2L,
			  zENTRY_, 1L,
		 PUT_, zENTRY_DATASPEC_, CDF_EPOCH, 1L,
		 SELECT_, ATTR_, 0L,
		 PUT_, ATTR_SCOPE_, VARIABLE_SCOPE,
		       ATTR_SCOPE_, GLOBAL_SCOPE,
		 NULL_);
if (status < CDF_OK) QuitCDF ("13.0", status);

/******************************************************************************
* Delete entries/attribute/variables.
******************************************************************************/

status = CDFlib (SELECT_, ATTR_, 0L,
			  gENTRY_, entryNum,
		 DELETE_, gENTRY_, 
		 SELECT_, ATTR_, 1L,
			  zENTRY_, 0L,
		 DELETE_, zENTRY_, 
		 SELECT_, ATTR_, 2L,
			  zENTRY_, 1L,
		 DELETE_, zENTRY_, 
		 SELECT_, ATTR_, 0L,
		 DELETE_, ATTR_, 
		 NULL_);
if (status < CDF_OK) QuitCDF ("14.0", status);

/******************************************************************************
* Close CDF.
******************************************************************************/

status = CDFlib (CLOSE_, CDF_,
		 NULL_);
if (status < CDF_OK) QuitCDF ("15.0", status);

/******************************************************************************
* Successful completion.
******************************************************************************/

return EXIT_SUCCESS_;
}


/******************************************************************************
* QuitCDF.
******************************************************************************/

void QuitCDF (where, status)
char *where;
CDFstatus status;
{
  char text[CDF_STATUSTEXT_LEN+1];
  printf ("Aborting at %s...\n", where);
  if (status < CDF_OK) {
    CDFlib (SELECT_, CDF_STATUS_, status,
	    GET_, STATUS_TEXT_, text,
	    NULL_);
    printf ("%s\n", text);
  }
  CDFlib (CLOSE_, CDF_,
	  NULL_);
  printf ("...test aborted.\n");
  exit (EXIT_FAILURE_);
}
