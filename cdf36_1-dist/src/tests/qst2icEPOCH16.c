/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                  Quick Start Test Program (INTERNAL interface/C).
*
*  Version 1.11, 13-Jun-96, Hughes STX.
*
*  Modification history:
*
*   V1.0  27-May-91, J Love     Original version (for CDF V2.1).
*   V1.1  25-Jun-91, J Love     Renamed CDF for portability.
*   V1.2   2-Aug-91, J Love     Use 'Exit'/'ExitBAD'.  Use 'CDFlib'.
*   V1.3  24-Oct-91, J Love     Modified for IBM-PC port.
*   V1.4  21-Apr-92, J Love     CDF V2.2.
*   V1.5   2-Sep-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V1.6  13-Nov-92, J Love     Borland C 3.0 (partial bracketing).
*   V1.7  14-Jan-94, J Love     CDF V2.4.
*   V1.8  20-Dec-94, J Love     CDF V2.5.
*   V1.8a  9-Jan-95, J Love     Indentation on UNIX machines.
*   V1.8b  8-Mar-95, J Love     Fixed wrong data type for rEntry.
*   V1.9   6-Apr-95, J Love     POSIX.
*   V1.9a 18-Apr-95, J Love     More POSIX.
*   V1.10  2-Jan-95, J Love     Shortened argument lists for Think C.
*   V1.11 13-Jun-96, J Love     CDF V2.6.
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

#define zN_DIMSa        1
#define zDIM_0_SIZEa    5
#define zNUM_ELEMSa     8

void QuitCDF PROTOARGs((char *where, CDFstatus status));

/******************************************************************************
* Main.
******************************************************************************/

int main () {
CDFid id;
CDFstatus status;
static long encoding = IBMPC_ENCODING;
static long majority = ROW_MAJOR;
static long dimSizes[N_DIMS] = { 0, 0 };
long zIndicesA[zN_DIMSa];
static long zCounts[zN_DIMSa] = { zDIM_0_SIZEa };
static long zIntervals[N_DIMS] = { 1 };
int x0;
static long zVarAnum_out;
static long zVarArecVariance = { VARY };
static long year=2001, month=10, day=1, hour=12, minute=10, second=20, 
	    msec=100, usec=200, nsec=300, psec=400;
static long yearOut, monthOut, dayOut, hourOut, minuteOut, secondOut, msecOut, 
	    usecOut, nsecOut, psecOut;
static double epoch16[2], epoch16Save[2], epoch16Out[2], mmm;
static double epoch16Hyper[4][2], epoch16HyperOut[4][2];
static long yearHyper[4], monthHyper[4], dayHyper[4], hourHyper[4],
            minuteHyper[4], secondHyper[4], msecHyper[4],
            usecHyper[4], nsecHyper[4], psecHyper[4];
static long yearHyperOut[4], monthHyperOut[4], dayHyperOut[4], hourHyperOut[4],
            minuteHyperOut[4], secondHyperOut[4], msecHyperOut[4],
            usecHyperOut[4], nsecHyperOut[4], psecHyperOut[4];
static char encodeText[EPOCHx_STRING_MAX];

/******************************************************************************
* Display title.
******************************************************************************/

printf ("Testing Internal/C interface for CDF_EPOCH16 ...\n");

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

/*                     PUT_, CDF_ENCODING_, encoding,  */
		     PUT_, CDF_MAJORITY_, majority,
		           CDF_FORMAT_, SINGLE_FILE,

		     SELECT_, CDF_DECODING_, HOST_DECODING,
		     NULL_);
    if (status < CDF_OK) QuitCDF ("6.0", status);
  }
}

/******************************************************************************
* Create variables.
******************************************************************************/

status = CDFlib (CREATE_, zVAR_, "myEPOCH", CDF_EPOCH16, 1L,
				 0L, dimSizes, zVarArecVariance,
				 0L, &zVarAnum_out,
                 PUT_, zVAR_ALLOCATERECS_, 8L,
		 NULL_);
if (status < CDF_OK) QuitCDF ("7.0d", status);
/******************************************************************************
* PUT to variables.
******************************************************************************/

status = CDFlib (SELECT_, zVAR_, zVarAnum_out,
			  zVAR_RECNUMBER_, 0L,
		 NULL_);
if (status < CDF_OK) QuitCDF ("8.0z", status);

mmm = computeEPOCH16 (year, month, day, hour, minute, second, msec, usec,
		      nsec,psec, epoch16);
if (mmm == ILLEGAL_EPOCH_VALUE) printf("***** error 0\n");

epoch16Save[0] = epoch16[0];
epoch16Save[1] = epoch16[1];

encodeEPOCH16_x (epoch16, "<dom.02>-<month>-<year> <hour>:<min>:<sec>.<fos>",
                 encodeText);
printf("encodeEPOCH16_x: %s\n",encodeText);
encodeEPOCH16_x (epoch16, 
                 "<dom.02>-<month>-<year> <hour>:<min>:<sec>.<msc>:<usc>:<nsc>:<psc>",
                 encodeText);
printf("encodeEPOCH16_x: %s\n",encodeText);
encodeEPOCH16_1 (epoch16, encodeText);
printf("1. encodeEPOCH16_1: %s\n",encodeText);
mmm = parseEPOCH16_1 (encodeText, epoch16);
if (mmm == ILLEGAL_EPOCH_VALUE) printf("***** error 1\n");
encodeEPOCH16_1 (epoch16, encodeText);
printf("2. parseEPOCH16_1:  %s\n",encodeText);

epoch16[0] = epoch16Save[0];
epoch16[1] = epoch16Save[1];
encodeEPOCH16_2 (epoch16, encodeText);
printf("3. encodeEPOCH16_2: %s\n",encodeText);
mmm = parseEPOCH16_2 (encodeText, epoch16);
if (mmm == ILLEGAL_EPOCH_VALUE) printf("***** error 2\n");
encodeEPOCH16_2 (epoch16, encodeText);
printf("4. parseEPOCH16_2:  %s\n",encodeText);

epoch16[0] = epoch16Save[0];
epoch16[1] = epoch16Save[1];
encodeEPOCH16_3 (epoch16, encodeText);
printf("5. encodeEPOCH16_3: %s\n",encodeText);
mmm = parseEPOCH16_3 (encodeText, epoch16);
if (mmm == ILLEGAL_EPOCH_VALUE) printf("***** error 3\n");
encodeEPOCH16_3 (epoch16, encodeText);
printf("6. parseEPOCH16_3:  %s\n",encodeText);

epoch16[0] = epoch16Save[0];
epoch16[1] = epoch16Save[1];
encodeEPOCH16_4 (epoch16, encodeText);
printf("7. encodeEPOCH16_4: %s\n",encodeText);
mmm = parseEPOCH16_4 (encodeText, epoch16);
if (mmm == ILLEGAL_EPOCH_VALUE) printf("***** error 4\n");
encodeEPOCH16_4 (epoch16, encodeText);
printf("8. parseEPOCH16_4:  %s\n",encodeText);

epoch16[0] = epoch16Save[0];
epoch16[1] = epoch16Save[1];
   status = CDFlib (PUT_, zVAR_DATA_, epoch16,
		    NULL_);
   if (status < CDF_OK) QuitCDF ("8.1z", status);

/******************************************************************************
* GET from the variables.
******************************************************************************/

   status = CDFlib (GET_, zVAR_DATA_, epoch16Out,
		    NULL_);
   if (status < CDF_OK) QuitCDF ("9.1z", status);

   EPOCH16breakdown (epoch16, &yearOut, &monthOut, &dayOut, &hourOut, 
		     &minuteOut, &secondOut, &msecOut, &usecOut,&nsecOut,
			 &psecOut);
   if (year != yearOut) QuitCDF("9.2a", status);
   if (month != monthOut) QuitCDF("9.3a", status);
   if (day != dayOut) QuitCDF("9.4a", status);
   if (hour != hourOut) QuitCDF("9.5a", status);
   if (minute != minuteOut) QuitCDF("9.6a", status);
   if (second != secondOut) QuitCDF("9.7a", status);
   if (msec != msecOut) QuitCDF("9.8a", status);
   if (usec != usecOut) QuitCDF("9.9a", status);
   if (nsec != nsecOut) QuitCDF("9.10a", status);
   if (psec != psecOut) QuitCDF("9.11a", status);

/******************************************************************************
* HyperPUT to the variables.
******************************************************************************/

zIndicesA[0] = 0;
for (x0= 0; x0 < 4; x0++) {
  year = year + 1;
  hour = hour + 1;
  second = second +1;
  nsec = nsec + 100;
  yearHyper[x0] = year;
  monthHyper[x0] = month;
  dayHyper[x0] = day;
  hourHyper[x0] = hour;
  minuteHyper[x0] = minute;
  secondHyper[x0] = second;
  msecHyper[x0] = msec;
  usecHyper[x0] = usec;
  nsecHyper[x0] = nsec;
  psecHyper[x0] = psec;

  mmm = computeEPOCH16 (yearHyper[x0], monthHyper[x0], dayHyper[x0], 
			hourHyper[x0], minuteHyper[x0], secondHyper[x0], 
			msecHyper[x0], usecHyper[x0], nsecHyper[x0], 
			psecHyper[x0], epoch16); 
  if (mmm == ILLEGAL_EPOCH_VALUE) printf("***** error 2\n");
  epoch16Hyper[x0][0] = epoch16[0];
  epoch16Hyper[x0][1] = epoch16[1];

}
status = CDFlib (SELECT_, zVAR_RECNUMBER_, 1L,
			  zVAR_RECCOUNT_, 4L,
			  zVAR_RECINTERVAL_, 1L,
			  zVAR_DIMINDICES_, zIndicesA,
			  zVAR_DIMCOUNTS_, zCounts,
			  zVAR_DIMINTERVALS_, zIntervals,
		 PUT_, zVAR_HYPERDATA_, epoch16Hyper,
		 NULL_);
if (status < CDF_OK) QuitCDF ("10.0z", status);

/******************************************************************************
* HyperGET from variables.
******************************************************************************/

status = CDFlib (GET_, zVAR_HYPERDATA_, epoch16HyperOut,
		 NULL_);
if (status < CDF_OK) QuitCDF ("11.0z", status);

for (x0= 0; x0 < 4; x0++) {
/*
   i=1+x0;
   status = CDFlib (SELECT_, zVAR_, zVarAnum_out,
                             zVAR_RECNUMBER_, (long)i,
                    NULL_);
   if (status < CDF_OK) QuitCDF ("8.0z", status);

   status = CDFlib (GET_, zVAR_DATA_, epoch16Out,
                    NULL_);
*/
  EPOCH16breakdown (epoch16HyperOut[x0], &yearOut, &monthOut, &dayOut, &hourOut, 
		    &minuteOut, &secondOut, &msecOut, &usecOut, &nsecOut,
			&psecOut);
  yearHyperOut[x0] = yearOut;
  monthHyperOut[x0] = monthOut;
  dayHyperOut[x0] = dayOut;
  hourHyperOut[x0] = hourOut;
  minuteHyperOut[x0] = minuteOut;
  secondHyperOut[x0] = secondOut;
  msecHyperOut[x0] = msecOut;
  usecHyperOut[x0] = usecOut;
  nsecHyperOut[x0] = nsecOut;
  psecHyperOut[x0] = psecOut;
}

for (x0= 0; x0 < 4; x0++) {

   if (yearHyper[x0] != yearHyperOut[x0]) QuitCDF("11.2a", status);
   if (monthHyper[x0] != monthHyperOut[x0]) QuitCDF("11.3a", status);
   if (dayHyper[x0] != dayHyperOut[x0]) QuitCDF("11.4a", status);
   if (hourHyper[x0] != hourHyperOut[x0]) QuitCDF("11.5a", status);
   if (minuteHyper[x0] != minuteHyperOut[x0]) QuitCDF("11.6a", status);
   if (secondHyper[x0] != secondHyperOut[x0]) QuitCDF("11.7a", status);
   if (msecHyper[x0] != msecHyperOut[x0]) QuitCDF("11.8a", status);
   if (usecHyper[x0] != usecHyperOut[x0]) QuitCDF("11.9a", status);
   if (nsecHyper[x0] != nsecHyperOut[x0]) QuitCDF("11.10a", status);
   if (psecHyper[x0] != psecHyperOut[x0]) QuitCDF("11.11a", status);
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
