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
*   V1.0  21-Feb-16, M Liu      Original version
*
******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cdf.h"

#define EXIT_SUCCESS_   0
#define EXIT_FAILURE_   1

void QuitCDF PROTOARGs((char *where, CDFstatus status));

/******************************************************************************
* Main.
******************************************************************************/

int main () {
CDFid id;
CDFstatus status;
long version, release, increment;
char subIncrement;

status = CDFlib (GET_, LIB_VERSION_, &version,
                       LIB_RELEASE_, &release,
                       LIB_INCREMENT_, &increment,
                       LIB_subINCREMENT_, &subIncrement,
		 NULL_);

if (status < CDF_OK) {
    QuitCDF ("1.3", status);
}
printf("CDF Library version:%ld release:%ld increment:%ld subIncrement:%c\n\n",
        version, release, increment, subIncrement);
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
  printf ("Program failed at %s...\n", where);
  if (status < CDF_OK) {
    CDFlib (SELECT_, CDF_STATUS_, status,
	    GET_, STATUS_TEXT_, text,
	    NULL_);
    printf ("%s\n", text);
  }
  exit (EXIT_FAILURE_);
}
