/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  SPDF/CDF                 Quick Start Test Program (INTERNAL interface/C).
*
*  Version 1.0, 15-Mar-11, ADNET Systems.
*
*  Modification history:
*
*   V1.0  15-Mar-11, M Liu      Original version (for CDF V3.3.2).
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
* Macros/prototypes.
******************************************************************************/

void QuitCDF PROTOARGs((char *where, CDFstatus status));

/******************************************************************************
* Main.
******************************************************************************/

int main () {
CDFid id;
CDFstatus status;
long varnum;
long variance = { VARY };
long dimSizes[1] = {0}, dimVary[1] = {VARY};
long indices[1] = {0}, counts[1]= {1}, intervals[1] = {1};

long long tt2000[8], mm[8];
double year1=2016, month1=12, day1=31, hour1=23, minute1=59, second1=57, 
       msec1=100, usec1=200, nsec1=300;
double year=2016, month=12, day=31, hour=23, minute=59, second=57, 
       msec=100, usec=200, nsec=300;
double yearOut, monthOut, dayOut, hourOut, minuteOut, secondOut, msecOut, 
       usecOut, nsecOut;
int  ix;
char string[TT2000_3_STRING_LEN+1];

long long int8[3][2], int8o[3][2], nn;

/******************************************************************************
* Display title.
******************************************************************************/

printf ("Testing Internal/C interface for CDF_TIME_TT2000 ...\n");

tt2000[0] = computeTT2000 (2000., 1., 1., 12., 0., 0., TT2000END);
breakdownTT2000 (tt2000[0], &yearOut, &monthOut, &dayOut, &hourOut, &minuteOut,
                 &secondOut, &msecOut, &usecOut, &nsecOut);

printf ("Base: 2000-01-01T12:00.00.000 => %lld nanoseconds\n", tt2000[0]);
encodeTT2000 (tt2000[0], string, 3);
printf ("      %lld => %s\n", tt2000[0], string);

/******************************************************************************
* Create CDF.
******************************************************************************/
     
status = CDFlib (CREATE_, CDF_, "TEST", 0, dimSizes, &id,
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
    if (status < CDF_OK) QuitCDF ("6.0", status);
  }
}

/******************************************************************************
* Create variables.
******************************************************************************/

status = CDFlib (CREATE_, zVAR_, "myTT2000", CDF_TIME_TT2000, 1L,
				 0L, dimSizes, variance,
				 dimVary, &varnum,
		 NULL_);
if (status < CDF_OK) QuitCDF ("7.0", status);

dimSizes[0] = 2;

status = CDFlib (CREATE_, zVAR_, "myINT8", CDF_INT8, 1L,
                                 1L, dimSizes, variance,
                                 dimVary, &varnum,
                 NULL_);
if (status < CDF_OK) QuitCDF ("7.1", status);

/******************************************************************************
* PUT to variables.
******************************************************************************/

tt2000[0] = computeTT2000 (year, month, day, hour, minute, second,
                           msec, usec, nsec);
if (tt2000[0] == ILLEGAL_TT2000_VALUE) QuitCDF ("9.0", TT2000_TIME_ERROR);
for (ix = 1; ix < 8; ++ix) 
  tt2000[ix] = tt2000[0] + (long long) ix*1000000000;

varnum = CDFgetVarNum (id, "myTT2000");
status = CDFlib (SELECT_, zVAR_, varnum,
			  zVAR_RECNUMBER_, 0L,
			  zVAR_RECCOUNT_, 8L,
			  zVAR_RECINTERVAL_, 1L,
			  zVAR_DIMINDICES_, indices,
			  zVAR_DIMCOUNTS_, counts,
			  zVAR_DIMINTERVALS_, intervals,
		 PUT_, zVAR_HYPERDATA_, tt2000,
		 NULL_);
if (status < CDF_OK) QuitCDF ("10.0", status);

/******************************************************************************
* HyperGET from variables.
******************************************************************************/

status = CDFlib (GET_, zVAR_HYPERDATA_, mm,
		 NULL_);
if (status < CDF_OK) QuitCDF ("11.0", status);

for (ix= 0; ix < 8; ix++) {
  breakdownTT2000 (mm[ix], &yearOut, &monthOut, &dayOut,
                   &hourOut, &minuteOut, &secondOut, &msecOut,
                   &usecOut, &nsecOut);
  if (year != yearOut) QuitCDF("11.1", status);
  if (month != monthOut) QuitCDF("11.2", status);
  if (day != dayOut) QuitCDF("11.3", status);
  if (hour != hourOut) QuitCDF("11.4", status);
  if (minute != minuteOut) QuitCDF("11.5", status);
  if (second != secondOut) QuitCDF("11.6", status);
  if (msec != msecOut) QuitCDF("11.7", status);
  if (usec != usecOut) QuitCDF("11.8", status);
  if (nsec != nsecOut) QuitCDF("11.9", status);
  ++second;
  if (second > 60) {
    year = year + 1;     
    month = 1; 
    day = 1;
    hour = 0;
    minute = 0;
    second = 0;
  }
}

varnum = CDFgetVarNum (id, "myINT8");
nn = 88888888888LL;
for (ix = 0; ix < 3; ++ix) {
  int8[ix][0] = nn;
  int8[ix][1] = -nn;
  ++nn;
}

counts[0] = 2;
status = CDFlib (SELECT_, zVAR_, varnum,
			  zVAR_RECNUMBER_, 0L,
			  zVAR_RECCOUNT_, 3L,
			  zVAR_RECINTERVAL_, 1L,
			  zVAR_DIMINDICES_, indices,
			  zVAR_DIMCOUNTS_, counts,
			  zVAR_DIMINTERVALS_, intervals,
		 PUT_, zVAR_HYPERDATA_, int8,
		 NULL_);
if (status < CDF_OK) QuitCDF ("12.0", status);

status = CDFlib (GET_, zVAR_HYPERDATA_, int8o,
                 NULL_);
if (status < CDF_OK) QuitCDF ("13.0", status);

nn = 88888888888LL;
for (ix = 0; ix < 3; ++ix) {
  if (int8o[ix][0] != nn) QuitCDF ("14.0", status);;
  if (int8o[ix][1] != -nn) QuitCDF ("14.1", status);; 
  ++nn; 
}                         
                          
/******************************************************************************
* Close CDF.
******************************************************************************/

status = CDFlib (CLOSE_, CDF_,
		 NULL_);
if (status < CDF_OK) QuitCDF ("26.2", status);

/******************************************************************************
* Re-open the CDF.
******************************************************************************/

status = CDFlib (OPEN_, CDF_, "TEST", &id,
                 NULL_);

if (status < CDF_OK) QuitCDF ("30.1", status);
varnum = CDFvarNum (id, "myTT2000");
if (varnum < CDF_OK) QuitCDF ("31.1", status);
/******************************************************************************
* HyperGET from variables.
******************************************************************************/

status = CDFlib (SELECT_, zVAR_, varnum,
			  zVAR_RECNUMBER_, 0L,
			  zVAR_RECCOUNT_, 8L,
			  zVAR_RECINTERVAL_, 1L,
			  zVAR_DIMINDICES_, indices,
			  zVAR_DIMCOUNTS_, counts,
			  zVAR_DIMINTERVALS_, intervals,
		 GET_, zVAR_HYPERDATA_, mm,
		 NULL_);
if (status < CDF_OK) QuitCDF ("32.1", status);

for (ix= 0; ix < 8; ix++) {
  breakdownTT2000 (mm[ix], &yearOut, &monthOut, &dayOut,
                   &hourOut, &minuteOut, &secondOut, &msecOut,
                   &usecOut, &nsecOut);
  encodeTT2000 (mm[ix], string, 3);
  printf ("%s\n",string);
  if (year1 != yearOut) QuitCDF("33.1", status);
  if (month1 != monthOut) QuitCDF("33.2", status);
  if (day1 != dayOut) QuitCDF("33.3", status);
  if (hour1 != hourOut) QuitCDF("33.4", status);
  if (minute1 != minuteOut) QuitCDF("33.5", status);
  if (second1 != secondOut) QuitCDF("33.6", status);
  if (msec1 != msecOut) QuitCDF("33.7", status);
  if (usec1 != usecOut) QuitCDF("33.8", status);
  if (nsec1 != nsecOut) QuitCDF("33.9", status);
  ++second1;
  if (second1 > 60) {
    year1 = year1 + 1;     
    month1 = 1; 
    day1 = 1;
    hour1 = 0;
    minute1 = 0;
    second1 = 0;
  }
}

/******************************************************************************
* Close CDF.
******************************************************************************/

status = CDFlib (CLOSE_, CDF_,
		 NULL_);
if (status < CDF_OK) QuitCDF ("26.2", status);

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
