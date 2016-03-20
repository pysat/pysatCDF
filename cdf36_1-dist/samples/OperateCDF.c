/******************************************************************************
*
*  SPDF/CDF		Sample program for operating a CDF.
*
*  Version 1.0, 09-Dec-05
*
*  Modification history:
*
*   V1.0  09-Dec-05, M Liu      Original version (for CDF V3.1).
*                               A simple program to create/delete a CDF and
*                               modify the CDF specification from the
*                               defaults. The CDF is created expecting 
*                               zVariable(s), the preferred variable, to be 
*                               added. It also shows how to acquire the 
*                               information from the library being operated 
*                               upon.
*
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
static long encoding = NETWORK_ENCODING;
static long majority = ROW_MAJOR;
static long dimSizes[1] = { 0 };
static long cType = GZIP_COMPRESSION;
static long cParms[1] = { 5 };
long version_out;
long release_out;
long increment_out;
char subincrement_out;
long formatOut;
long encoding_out;
long majority_out;
char CopyRightText[CDF_COPYRIGHT_LEN+1];
static long actual_encoding = NETWORK_ENCODING;

/******************************************************************************
 * Get library information.
 ******************************************************************************/

status = CDFlib (GET_, LIB_VERSION_, &version_out,
                       LIB_RELEASE_, &release_out,
                       LIB_INCREMENT_, &increment_out,
                       LIB_subINCREMENT_, &subincrement_out,
                       LIB_COPYRIGHT_, CopyRightText,
                 NULL_);
if (status < CDF_OK) QuitCDF ("1.0", status);

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
    if (status < CDF_OK) QuitCDF ("2.0", status);

    status = CDFlib (DELETE_, CDF_,
		     NULL_);
    if (status < CDF_OK) QuitCDF ("2.1", status);

    status = CDFlib (CREATE_, CDF_, "TEST", 0L, dimSizes, &id,
		     NULL_);
    if (status < CDF_OK) QuitCDF ("2.2", status);
  }
  else
    QuitCDF ("1.3", status);
}

/******************************************************************************
* Respecify the encoding and majority for the CDF (from its default).
******************************************************************************/

status = CDFlib (PUT_, CDF_ENCODING_, encoding,
		       CDF_MAJORITY_, majority,
		 NULL_);

if (status < CDF_OK) QuitCDF ("3.0", status);

/******************************************************************************
* Inquire CDF.   
******************************************************************************/
                 
status = CDFlib (GET_, CDF_FORMAT_, &formatOut,
		       CDF_ENCODING_, &encoding_out,
		       CDF_MAJORITY_, &majority_out,
		 NULL_);
if (status < CDF_OK) QuitCDF ("4.0", status);
                       
if (formatOut != SINGLE_FILE) QuitCDF ("4.1", status);
if (encoding_out != actual_encoding) QuitCDF ("4.2", status);
if (majority_out != majority) QuitCDF ("4.3", status);
   
/******************************************************************************
* Specify the compression for the CDF.
******************************************************************************/

status = CDFlib (PUT_, CDF_COMPRESSION_, cType, cParms,
		 NULL_);

if (status < CDF_OK) QuitCDF ("5.0", status);

/******************************************************************************
* Close CDF.
******************************************************************************/

status = CDFlib (CLOSE_, CDF_,
		 NULL_);
if (status < CDF_OK) QuitCDF ("6.0", status);

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
