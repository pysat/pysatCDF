/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/****************************************************************************** 
*
*  NSSDC/CDF                       EPOCH utility routines for C applications.
*
*  Version 2.5b, 29-Oct-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  24-Jan-91, D Grogan   Original version (for CDF V2.0).
*   V1.1  25-Mar-91, J Love     Added support for Silicon Graphics (MIPSEB
*                               encoding, IRIX - UNIX).
*   V1.2  26-Mar-91, J Love     Added "types.h" include for SunOS 4.0.3
*                               systems (UNIX).  Added "ctypes.h" include and
*                               removed definitions of toupper & tolower.  Use
*                               toupper the safe way.  Added definition for
*                               toupper if SunOS 4.0.3.
*   V1.3  19-Jun-91, J Love     Changed epochParse to return FALSE if illegal
*                               date/time string (and added more error
*                               checking) and to set 'tSince0'.
*   V1.4  29-Jul-91, J Love     TRUE/FALSE.  Don't display error messages (the
*                               caller will do that).
*   V1.5  23-Sep-91, J Love     Modified for IBM-PC port.
*   V2.0   1-Apr-92, J Love     Added to CDF library.  Added additional ways
*                    A Warnock  to display an EPOCH date/time.
*   V2.1  30-Jun-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V2.2  24-Jan-94, J Love     CDF V2.4.  Handle negative EPOCHs.
*   V2.3  13-Dec-94, J Love     CDF V2.5.
*   V2.3a 18-Jan-95, J Love	Made `computeEPOCH' more flexible.
*   V2.3b 24-Jan-95, J Love	Changed `parseEPOCH' for Salford C.  Consider
*				milliseconds in `encodeEPOCH1'.
*   V2.3c 24-Feb-95, J Love	Solaris 2.3 IDL i/f.
*   V2.4   9-May-95, J Love	Added parseEPOCH1, parseEPOCH2, & parseEPOCH3.
*   V2.4a 13-Jun-95, J Love	EPOCH custom format.
*   V2.5   3-Oct-96, J Love	CDF V2.6 (and the OSF/1 bug in `sprintf').
*   V2.5a  8-Mar-97, J Love	Windows NT for MS Visual C/C++ on an IBM PC.
*   V2.5b 29-Oct-97, J Love	More Windows NT.
*   V2.6  29-Jan-04, M Liu      Added a new set of CDF_EPOCH16 functions for 
*                               handling fraction of a second up to picosecond.
*   V2.7  25-Feb-10, M Liu      Modified computeEPOCH and computeEPOCH16 to
*                               allow day/time fields out of the previously 
*                               defined range, e.g., 0-23 for hour. 
*   V2.8  12-Aug-10, M Liu      Limited width of the fraction so it will not
*                               overrun the string in AppendFractionPart. 
*   V2.9  10-Dec-10, M Liu      Added encodeEPOCH4, encodeEPOCH16_4,
*                               parseEPOCH4, parseEPOCH16_4 to handle epochs
*                               conforming to ISO 8601.
******************************************************************************/

#include "cdflib.h"

/******************************************************************************
* Local macro definitions.
******************************************************************************/

#define MAX_PART_LEN		10
#define MAX_MOD_LEN		10
#define MAX_ePART_LEN		25

/******************************************************************************
* Local function prototypes.
******************************************************************************/

static long JulianDay PROTOARGs((long, long, long));
static char *MonthToken PROTOARGs((long));
static char *FullDayToken PROTOARGs((char *));
static Logical AppendFractionPart PROTOARGs((
  char *encoded, double fraction, int defaultWidth, char *modifier
));
static Logical AppendIntegerPart PROTOARGs((
  char *encoded, long integer, int defaultWidth, Logical defaultLeading0,
  char *modifier
));
static Logical AppendPart PROTOARGs((
  char *encoded, char *ePart, int width, Logical leading0
));
void encodeEPOCH16x2 PROTOARGs((
  double epoch, char *encoded
));
void encodeEPOCH16x3 PROTOARGs((
  double epoch[], char *encoded, size_t width
));
void encodeEPOCH16x4 PROTOARGs((
  double epoch, char *encoded
));
static double computeEpoch PROTOARGs((
  long year, long month, long day, long hour, long minute, long second,
  long msec
));
static double computeEpoch16 PROTOARGs((
  long year, long month, long day, long hour, long minute, long second,
  long msec, long usec, long nsec, long psec, double epoch[]
));

/******************************************************************************
* parseEPOCH.
* This function parses an input date/time string and returns an EPOCH
* value.  The format must be exactly as shown below.  Month abbreviations may
* be in any case and are always the first three letters of the month.
*
* Format:       dd-mmm-yyyy hh:mm:ss.mmm
* Examples:      1-Apr-1990 03:05:02.000
*               10-Oct-1993 23:45:49.999
*
* The expected format is the same as that produced by encodeEPOCH.
******************************************************************************/

VISIBLE_PREFIX double parseEPOCH (inString)
char *inString;
{
  char moString[4];
  long year, month, day, hour, minute, second, msec;
  int monthX;
  if (sscanf(inString,"%ld-%c%c%c-%ld %ld:%ld:%ld.%ld",
             &day, &(moString[0]), &(moString[1]), &(moString[2]), &year,
             &hour, &minute, &second, &msec) != 9) return ILLEGAL_EPOCH_VALUE; 
  moString[0] = (char) MakeUpper((int)moString[0]);   /* J */
  moString[1] = (char) MakeLower((int)moString[1]);   /* a */
  moString[2] = (char) MakeLower((int)moString[2]);   /* n */
  moString[3] = NUL;
  for (monthX = 1, month = 0; monthX <= 12; monthX++) {
    if (!strcmp(moString,MonthToken(monthX))) {
      month = monthX;
      break;
    }
  }
  if (month == 0) return ILLEGAL_EPOCH_VALUE;
  return computeEPOCH (year, month, day, hour, minute, second, msec);
}  

/******************************************************************************
* parseEPOCH16.
* This function is an extension of parseEPOCH. It is used to handle the time
* that may contain as small as picoseconds (one trillionth of a second). 
* A section of micro-, nano- and pico-second is added. 
*
* Format:	dd-mmm-yyyy hh:mm:ss.mmm.uuu.nnn.ppp
* Examples:	 1-Apr-1990 03:05:02.000.000.000.000
*		10-Oct-1993 23:45:49.999.999.999.999
*
* The expected format is the same as that produced by encodeEPOCH16.
******************************************************************************/

VISIBLE_PREFIX double parseEPOCH16 (inString, epoch)
char *inString;
double epoch[2];
{
  char moString[4];
  long year, month, day, hour, minute, second, msec, usec, nsec, psec;
  double mmm;
  int monthX;
  if (sscanf(inString,"%ld-%c%c%c-%ld %ld:%ld:%ld.%ld.%ld.%ld.%ld",
	     &day, &(moString[0]), &(moString[1]), &(moString[2]), &year,
	     &hour, &minute, &second, &msec, &usec, &nsec, &psec) != 12) 
        return ILLEGAL_EPOCH_VALUE;
  moString[0] = (char) MakeUpper((int)moString[0]);   /* J */
  moString[1] = (char) MakeLower((int)moString[1]);   /* a */
  moString[2] = (char) MakeLower((int)moString[2]);   /* n */
  moString[3] = NUL;
  for (monthX = 1, month = 0; monthX <= 12; monthX++) {
    if (!strcmp(moString,MonthToken(monthX))) {
      month = monthX;
      break;
    }
  }
  if (month == 0) return ILLEGAL_EPOCH_VALUE;
  mmm = computeEPOCH (year, month, day, hour, minute, second, 0L);
  if (mmm == ILLEGAL_EPOCH_VALUE) return ILLEGAL_EPOCH_VALUE;
  if (msec < 0 || msec > 999) return ILLEGAL_EPOCH_VALUE;
  if (usec < 0 || usec > 999) return ILLEGAL_EPOCH_VALUE;
  if (nsec < 0 || nsec > 999) return ILLEGAL_EPOCH_VALUE;
  if (psec < 0 || psec > 999) return ILLEGAL_EPOCH_VALUE;
  if (year == 9999 && month == 12 && day == 31 && hour == 23 && minute == 59 &&
      second == 59 && msec == 999 && usec == 999 && nsec == 999 && psec == 999) {
    epoch[0] = -1.0E31;
    epoch[1] = -1.0E31;
    return 0.0;
  }
  epoch[0] = mmm / (double) 1000.0;
  epoch[1] = msec * pow(10.0, 9.0) + usec * pow(10.0, 6.0) + nsec * pow(10.0, 3.0) + 
	     psec;
  return (double) 0.0;
}

/******************************************************************************
* parseEPOCH1.
* This function parses an input date/time string and returns an EPOCH
* value.  The format must be exactly as shown below.  Note that if there are
* less than 8 digits after the decimal point, zeros (0's) are assumed for the
* missing digits.
*
* Format:	yyyymmdd.ttttttt
* Examples:	19950508.0000000
*		19671231.58      (== 19671213.5800000)
*
* The expected format is the same as that produced by encodeEPOCH1.
******************************************************************************/

VISIBLE_PREFIX double parseEPOCH1 (inString)
char *inString;
{
  char temp[EPOCH1_STRING_LEN+1]; double fraction; int i;
  long year, month, day, hour, minute, second, msec, fractionL;
  strcpyX (temp, inString, EPOCH1_STRING_LEN);
  for (i = strlen(temp); i < EPOCH1_STRING_LEN; i++) temp[i] = '0';
  temp[i] = NUL;
  if (sscanf(temp,"%4ld%2ld%2ld.%ld",
	     &year, &month, &day, &fractionL) != 4) return ILLEGAL_EPOCH_VALUE;
  fraction = ((double) fractionL) / 10000000.0;
  hour = (long) (fraction * 24.0);
  fraction -= (double) (hour / 24.0);
  minute = (long) (fraction * 1440.0);
  fraction -= (double) (minute / 1440.0);
  second = (long) (fraction * 86400.0);
  fraction -= (double) (second / 86400.0);
  msec = (long) (fraction * 86400000.0);
  return computeEPOCH (year, month, day, hour, minute, second, msec);
}

/******************************************************************************
* parseEPOCH16_1.     
* This function is an extension of parseEPOCH1. It is used to handle the time
* that may contain as small as picoseconds.
* The ttttttt... portion (at least 7-digit or more) represents a fraction of a 
* day, 
*
* Format:       yyyymmdd.ttttttttttttttt
* Examples:     19950508.000000000000000
*               19671231.58      (== 19671213.580000000000000)
*
* The expected format is the same as that produced by encodeEPOCH16_1.
******************************************************************************/

VISIBLE_PREFIX double parseEPOCH16_1 (inString, epoch)
char *inString;
double epoch[2];
{
  char temp[EPOCH16_1_STRING_LEN+1]; double fraction; int i;    
  long year, month, day, hour, minute, second, fractionL1, fractionL2;
  double mmm;

  if (!strcmp (inString, "99991231.999999999999999")) {
    epoch[0] = -1.0E31;
    epoch[1] = -1.0E31;
    return 0.0;
  }

  strcpyX (temp, inString, EPOCH16_1_STRING_LEN);
  for (i = strlen(temp); i < EPOCH16_1_STRING_LEN; i++) temp[i] = '0';
  temp[i] = NUL;
  if (sscanf(temp,"%4ld%2ld%2ld.%7ld%8ld",
             &year, &month, &day, &fractionL1, &fractionL2) != 5) 
     return ILLEGAL_EPOCH_VALUE;
  fraction = ((double) fractionL1 * pow(10.0, 8.0) + (double) fractionL2) * 
	     pow(10.0, -15.0);
  hour = (long) (fraction * 24.0);
  fraction -= (double) (hour / 24.0);
  minute = (long) (fraction * 1440.0);        
  fraction -= (double) (minute / 1440.0);
  second = (long) (fraction * 86400.0);
  fraction -= (double) (second / 86400.0);
  mmm =  computeEPOCH (year, month, day, hour, minute, second, 0L);
  if (mmm == ILLEGAL_EPOCH_VALUE) return ILLEGAL_EPOCH_VALUE;
  epoch[0] = mmm / (double) 1000.0;
  epoch[1] = fraction * 86400.0 * pow(10.0, 12.0);
  return (double) 0.0;
}

/******************************************************************************
* parseEPOCH2.
* This function parses an input date/time string and returns an EPOCH
* value.  The format must be exactly as shown below.
*
* Format:	yyyymmddhhmmss
* Examples:	19950508000000
*		19671231235959
*
* The expected format is the same as that produced by encodeEPOCH2.
******************************************************************************/

VISIBLE_PREFIX double parseEPOCH2 (inString)
char *inString;
{
  long year, month, day, hour, minute, second;
  if (sscanf(inString,"%4ld%2ld%2ld%2ld%2ld%2ld",
	     &year,&month,&day,&hour,&minute,&second) != 6) {
    return ILLEGAL_EPOCH_VALUE;
  }
  return computeEPOCH (year, month, day, hour, minute, second, 0L);
}

/******************************************************************************
* parseEPOCH16_2.
* This function is an extension of parseEPOCH2. It is used to handle the time
* that may contain as small as picoseconds.
*
* Format:       yyyymmddhhmmss
* Examples:     19950508000000
*               19671231235959
*
* The expected format is the same as that produced by encodeEPOCH16_2.
******************************************************************************/

VISIBLE_PREFIX double parseEPOCH16_2 (inString, epoch)
char *inString;
double epoch[2];
{
  long year, month, day, hour, minute, second;
  double mmm;
  if (sscanf(inString,"%4ld%2ld%2ld%2ld%2ld%2ld",
             &year,&month,&day,&hour,&minute,&second) != 6) {
    return ILLEGAL_EPOCH_VALUE;
  }
  mmm = computeEPOCH (year, month, day, hour, minute, second, 0L); 
  if (mmm == ILLEGAL_EPOCH_VALUE) return ILLEGAL_EPOCH_VALUE;
  epoch[0] = mmm / (double) 1000.0;
  epoch[1] = 0.0;
  return (double) 0.0;
}

/******************************************************************************
* parseEPOCH3.
* This function parses an input date/time string and returns an EPOCH value.
* The format must be exactly as shown below.
*
* Format:	yyyy-mm-ddThh:mm:ss.cccZ
* Examples:	1990-04-01T03:05:02.000Z
*		1993-10-10T23:45:49.999Z
*
* The expected format is the same as that produced by encodeEPOCH3.
******************************************************************************/

VISIBLE_PREFIX double parseEPOCH3 (inString)
char *inString;
{
  long year, month, day, hour, minute, second, msec;
  if (sscanf(inString,"%ld-%ld-%ldT%ld:%ld:%ld.%ldZ",
	     &year,&month,&day,&hour,&minute,&second,&msec) != 7) {
    return ILLEGAL_EPOCH_VALUE;
  }
  return computeEPOCH (year, month, day, hour, minute, second, msec);
}

/******************************************************************************
* parseEPOCH16_3.
* This function is an extension of parseEPOCH3. It is used to handle the time
* that may contain as small as picoseconds.
*
* Format:       yyyy-mm-ddThh:mm:ss.ccc.mmm.nnn.pppZ
* Examples:     1990-04-01T03:05:02.000.000.000.000Z
*               1993-10-10T23:45:49.999.999.999.999Z
*
* The expected format is the same as that produced by encodeEPOCH16_3.
******************************************************************************/

VISIBLE_PREFIX double parseEPOCH16_3 (inString, epoch)
char *inString;
double epoch[2];
{
  long year, month, day, hour, minute, second, msec, usec, nsec, psec;
  int len;
  double mmm;
  len = strlen(inString);
  if (len < EPOCH16_3_STRING_LEN) return ILLEGAL_EPOCH_VALUE;
  if (sscanf(inString,"%ld-%ld-%ldT%ld:%ld:%ld.%ld.%ld.%ld.%ldZ",
             &year,&month,&day,&hour,&minute,&second,&msec,&usec,&nsec,&psec) 
      != 10) {
    return ILLEGAL_EPOCH_VALUE;
  }
  if (year == 9999 && month == 12 && day == 31 && hour == 23 && minute == 59 &&
      second == 59 && msec == 999 && usec == 999 && nsec == 999 && psec == 999) {
    epoch[0] = -1.0E31;
    epoch[1] = -1.0E31;
    return (double) 0.0;
  }

  mmm = computeEPOCH (year, month, day, hour, minute, second, 0L);
  if (mmm == ILLEGAL_EPOCH_VALUE) return ILLEGAL_EPOCH_VALUE;
  epoch[0] = mmm / (double) 1000.0;
  epoch[1] = (double) psec + (double) nsec * pow(10.0, 3.0) + 
	     (double) usec * pow(10.0, 6.0) + (double) msec * pow(10.0, 9.0);
  return (double) 0.0;
}

/******************************************************************************
* parseEPOCH4.
* This function parses an input date/time string and returns an EPOCH value.
* The format must conform to ISO 8601 as the following shows:
*
* Format:	yyyy-mm-ddThh:mm:ss.ccc
* Examples:	1990-04-01T03:05:02.000
*		1993-10-10T23:45:49.999
*
* The expected format is the same as that produced by encodeEPOCH4.
******************************************************************************/

VISIBLE_PREFIX double parseEPOCH4 (inString)
char *inString;
{
  long year, month, day, hour, minute, second, msec;
  if (sscanf(inString,"%ld-%ld-%ldT%ld:%ld:%ld.%ld",
	     &year,&month,&day,&hour,&minute,&second,&msec) != 7) {
    return ILLEGAL_EPOCH_VALUE;
  }
  return computeEPOCH (year, month, day, hour, minute, second, msec);
}

/******************************************************************************
* parseEPOCH16_4.
* This function is an extension of parseEPOCH4. It is used to handle the time
* that may contain as small as picoseconds. The string conforms to ISO 8601.
*
* Format:       yyyy-mm-ddThh:mm:ss.cccmmmnnnppp
* Examples:     1990-04-01T03:05:02.000000000000
*               1993-10-10T23:45:49.999999999999
*
* The expected format is the same as that produced by encodeEPOCH16_4.
******************************************************************************/

VISIBLE_PREFIX double parseEPOCH16_4 (inString, epoch)
char *inString;
double epoch[2];
{
  long year, month, day, hour, minute, second, msec, usec, nsec, psec;
  int len;
  double mmm;
  len = strlen(inString);
  if (len < EPOCH16_4_STRING_LEN) return ILLEGAL_EPOCH_VALUE;
  if (sscanf(inString,"%ld-%ld-%ldT%ld:%ld:%ld.%3ld%3ld%3ld%3ld",
             &year,&month,&day,&hour,&minute,&second,&msec,&usec,&nsec,&psec) 
      != 10) {
    return ILLEGAL_EPOCH_VALUE;
  }
  if (year == 9999 && month == 12 && day == 31 && hour == 23 && minute == 59 &&
      second == 59 && msec == 999 && usec == 999 && nsec == 999 && psec == 999) {
    epoch[0] = -1.0E31;
    epoch[1] = -1.0E31;
    return (double) 0.0;
  }

  mmm = computeEPOCH (year, month, day, hour, minute, second, 0L);
  if (mmm == ILLEGAL_EPOCH_VALUE) return ILLEGAL_EPOCH_VALUE;
  epoch[0] = mmm / (double) 1000.0;
  epoch[1] = (double) psec + (double) nsec * pow(10.0, 3.0) + 
	     (double) usec * pow(10.0, 6.0) + (double) msec * pow(10.0, 9.0);
  return (double) 0.0;
}

/******************************************************************************
* encodeEPOCH.
* Converts an EPOCH value into a readable date/time string.
*
* Format:	dd-mmm-yyyy hh:mm:ss.ccc
* Examples:	01-Apr-1990 03:05:02.000
*		10-Oct-1993 23:45:49.999
*
* This format is the same as that expected by parseEPOCH.
******************************************************************************/

VISIBLE_PREFIX void encodeEPOCH (epoch, epString)
double epoch;
char epString[EPOCH_STRING_LEN+1];
{
  if (epoch == -1.0E31) {
    strcpyX (epString, "31-Dec-9999 23:59:59.999", 0);
    return;
  }

  encodeEPOCHx (epoch, "<dom.02>-<month>-<year> <hour>:<min>:<sec>.<fos>",
		epString);
  return;
}

/******************************************************************************
* encodeEPOCH16.
* This function is an extension of parseEPOCH. It is used to handle the time
* that may contain as small as picoseconds (10**-12 second). 
*
*
* Format:       dd-mmm-yyyy hh:mm:ss.ccc.uuu.nnn.ppp
* Examples:     01-Apr-1990 03:05:02.000.000.000.000
*               10-Oct-1993 23:45:49.999.999.999.999
*               012345678901234567890123456789012345
* This format is the same as that expected by parseEPOCH16.
******************************************************************************/

VISIBLE_PREFIX void encodeEPOCH16 (epoch, epString)
double epoch[2];
char epString[EPOCH16_STRING_LEN+1];
{
  char tmp[EPOCH1_STRING_LEN+1];

  if (epoch[0] == -1.0E31 && epoch[1] == -1.0E31) {
    strcpyX (epString, "31-Dec-9999 23:59:59.999.999.999.999", 0);
    return;
  }

  encodeEPOCHx (epoch[0]*1000.0, 
		"<dom.02>-<month>-<year> <hour>:<min>:<sec>.<fos>",
                epString);
  encodeEPOCH16x2 (epoch[1], tmp);
  strcpyX (epString+21, tmp, 15); 
  epString[EPOCH16_STRING_LEN] = NUL; 
  return;
}

/******************************************************************************
* encodeEPOCH1.
* Converts an EPOCH value into a readable date/time string.
*
* Format:	yyyymmdd.ttttttt
* Examples:	19900401.3658893
*		19611231.0000000
*
* This format is the same as that expected by parseEPOCH1.
******************************************************************************/

VISIBLE_PREFIX void encodeEPOCH1 (epoch, epString)
double epoch;
char epString[EPOCH1_STRING_LEN+1];
{
  if (epoch == -1.0E31) {
    strcpyX (epString, "99991231.9999999", 0);
    return;
  }

  encodeEPOCHx (epoch, "<year><mm.02><dom.02>.<fod.7>",
		epString);
  return;
}

/******************************************************************************   
* encodeEPOCH16_1.
* This function is an extension of encodeEPOCH1. It is used to handle the time
* that may contain as small as picoseconds. 
* 
* Converts an EPOCH value into a readable date/time string.
*
* Format:       yyyymmdd.ttttttttttttttt
* Examples:     19900401.365889324567890
*               19611231.000000000000000
*
* This format is the same as that expected by parseEPOCH16_1.
******************************************************************************/

VISIBLE_PREFIX void encodeEPOCH16_1 (epoch, epString)
double epoch[2];
char epString[EPOCH16_1_STRING_LEN+1];
{
  char tmp[15+1];

  if (epoch[0] == -1.0E31 && epoch[1] == -1.0E31) {
    strcpyX (epString, "99991231.999999999999999", 0);
    return;
  }

  encodeEPOCHx (epoch[0]*1000.0, "<year><mm.02><dom.02>.<fod.7>",
                epString);
  encodeEPOCH16x3 (epoch, tmp, (size_t) sizeof(tmp)-1);
  strcpyX (epString+9, tmp, 15);
  epString[EPOCH16_1_STRING_LEN] = NUL; 
  return ;
}

/******************************************************************************
* encodeEPOCH2.
* Converts an EPOCH value into a readable date/time string.
*
* Format:	yyyymmddhhmmss
* Examples:	19900401235959
*		19611231000000
*
* This format is the same as that expected by parseEPOCH2.
******************************************************************************/

VISIBLE_PREFIX void encodeEPOCH2 (epoch, epString)
double epoch;
char epString[EPOCH2_STRING_LEN+1];
{
  if (epoch == -1.0E31) {
    strcpyX (epString, "99991231235959", 0);
    return;
  }

  encodeEPOCHx (epoch, "<year><mm.02><dom.02><hour><min><sec>",
		epString);
  return;
}

/******************************************************************************   
* encodeEPOCH16_2.
* This function is an extension of encodeEPOCH2. It is used to handle the time
* that may contain as small as picoseconds. 
*
* Format:       yyyymmddhhmmss
* Examples:     19900401235959
*               19611231000000
*
* This format is the same as that expected by parseEPOCH16_2.
******************************************************************************/

VISIBLE_PREFIX void encodeEPOCH16_2 (epoch, epString)
double epoch[2];
char epString[EPOCH16_2_STRING_LEN+1];
{
  if (epoch[0] == -1.0E31 && epoch[1] == -1.0E31) {
    strcpyX (epString, "99991231235959", 0);
    return;
  }

  encodeEPOCHx (epoch[0]*1000.0, "<year><mm.02><dom.02><hour><min><sec>",
                epString);
  return;
}

/******************************************************************************
* encodeEPOCH3.
* Converts an EPOCH value into a readable date/time string.
*
* Format:	yyyy-mm-ddThh:mm:ss.cccZ
* Examples:	1990-04-01T03:05:02.000Z
*		1993-10-10T23:45:49.999Z
*
* This format is the same as that expected by parseEPOCH3.
******************************************************************************/

VISIBLE_PREFIX void encodeEPOCH3 (epoch, epString)
double epoch;
char epString[EPOCH3_STRING_LEN+1];
{
  if (epoch == -1.0E31) {
    strcpyX (epString, "9999-12-31T23:59:59.999Z", 0);
    return;
  }

  encodeEPOCHx (epoch, "<year>-<mm.02>-<dom.02>T<hour>:<min>:<sec>.<fos>Z",
		epString);
  return;
}

/******************************************************************************
* encodeEPOCH4.
* Converts an EPOCH value into a readable date/time, ISO 8601 string.
*
* Format:       yyyy-mm-ddThh:mm:ss.ccc
* Examples:     1990-04-01T03:05:02.000
*               1993-10-10T23:45:49.999
*
* This format is the same as that expected by parseEPOCH4.
******************************************************************************/

VISIBLE_PREFIX void encodeEPOCH4 (epoch, epString)
double epoch;
char epString[EPOCH4_STRING_LEN+1];
{
  if (epoch == -1.0E31) {
    strcpyX (epString, "9999-12-31T23:59:59.999", 0);
    return;
  }

  encodeEPOCHx (epoch, "<year>-<mm.02>-<dom.02>T<hour>:<min>:<sec>.<fos>",
                epString);
  return;
}

/****************************************************************************** 
* encodeEPOCH16_3.
* This function is an extension of encodeEPOCH3. It is used to handle the time
* that may contain as small as picoseconds. 
*
* Format:       yyyy-mm-ddThh:mm:ss.mmm.uuu.nnn.pppZ
* Examples:     1990-04-01T03:05:02.000.000.000.000Z
*               1993-10-10T23:45:49.999.999.999.999Z
*               012345678901234567890123456789012345
* This format is the same as that expected by parseEPOCH16_3.
******************************************************************************/

VISIBLE_PREFIX void encodeEPOCH16_3 (epoch, epString)
double epoch[2];
char epString[EPOCH16_3_STRING_LEN+1];
{
  char tmp[EPOCH16_3_STRING_LEN+1];

  if (epoch[0] == -1.0E31 && epoch[1] == -1.0E31) {
    strcpyX (epString, "9999-12-31T23:59:59.999.999.999.999Z", 0);
    return;
  }
 
  encodeEPOCHx (epoch[0]*1000.0, 
		"<year>-<mm.02>-<dom.02>T<hour>:<min>:<sec>.", tmp);
  strcpyX (epString, tmp, 20);
  encodeEPOCH16x2 (epoch[1], tmp);
  strcpyX (epString+20, tmp, EPOCH16_3_STRING_LEN-20-1);
  epString[EPOCH16_3_STRING_LEN-1] = 'Z';
  epString[EPOCH16_3_STRING_LEN] = NUL;
  return;
}

/******************************************************************************
* encodeEPOCH16_4.
* This function is an extension of encodeEPOCH4. It is used to handle the time
* that may contain as small as picoseconds and make the string an ISO 8601 
* format.
*
* Format:       yyyy-mm-ddThh:mm:ss.mmmuuunnnppp
* Examples:     1990-04-01T03:05:02.000000000000
*               1993-10-10T23:45:49.999999999999
*               01234567890123456789012345678901
* This format is the same as that expected by parseEPOCH16_4.
******************************************************************************/

VISIBLE_PREFIX void encodeEPOCH16_4 (epoch, epString)
double epoch[2];
char epString[EPOCH16_4_STRING_LEN+1];
{
  char tmp[EPOCH16_4_STRING_LEN+1];

  if (epoch[0] == -1.0E31 && epoch[1] == -1.0E31) {
    strcpyX (epString, "9999-12-31T23:59:59.999999999999", 0);
    return;
  }

  encodeEPOCHx (epoch[0]*1000.0,
                "<year>-<mm.02>-<dom.02>T<hour>:<min>:<sec>.", tmp);
  strcpyX (epString, tmp, 20);
  encodeEPOCH16x4 (epoch[1], tmp);
  strcpyX (epString+20, tmp, EPOCH16_4_STRING_LEN-20);
  epString[EPOCH16_4_STRING_LEN] = NUL;
  return;
}

/******************************************************************************
* encodeEPOCHx.
******************************************************************************/

VISIBLE_PREFIX void encodeEPOCHx (epoch, format, encoded)
double epoch;
char format[EPOCHx_FORMAT_MAX];
char encoded[EPOCHx_STRING_MAX+1];
{
  char *ptr = format;		/* Current position in format string. */
  char *ptrD;			/* Pointer to decimal point. */
  char *ptrE;			/* Pointer to ending right angle bracket. */
  char *p;			/* Temporary pointer. */
  char part[MAX_PART_LEN+1];	/* Part being encoded. */
  char mod[MAX_MOD_LEN+1];	/* Part modifier. */
  long year, month, day, hour,
       minute, second, msec;	/* EPOCH components. */
  /****************************************************************************
  * Break EPOCH down into its components, validate the format specification,
  * and initialize the encoded string.
  ****************************************************************************/
  if (format == NULL || NULstring(format)) {
    encodeEPOCH (epoch, encoded);
    return;
  }
  EPOCHbreakdown (epoch, &year, &month, &day, &hour, &minute, &second, &msec);
  MakeNUL (encoded);
  /****************************************************************************
  * Scan format string.
  ****************************************************************************/
  for (;;) {
     switch (*ptr) {
       /***********************************************************************
       * End of format string.
       ***********************************************************************/
       case NUL:
	 return;
       /***********************************************************************
       * Start of part to be encoded.
       ***********************************************************************/
       case '<':
	 /*********************************************************************
	 * If next character is also a `<' (character stuffing), then append
	 * a `<' and move on.
	 *********************************************************************/
	 if (*(ptr+1) == '<') {
	   strcatX (encoded, "<", EPOCHx_STRING_MAX);
	   ptr += 2;
	   break;
	 }
	 /*********************************************************************
	 * Find ending right angle bracket.
	 *********************************************************************/
	 ptrE = strchr (ptr + 1, '>');
	 if (ptrE == NULL) {
	   strcatX (encoded, "?", EPOCHx_STRING_MAX);
	   return;
	 }
	 /*********************************************************************
	 * Check for a part modifier.
	 *********************************************************************/
	 ptrD = strchr (ptr + 1, '.');
	 if (ptrD != NULL && ptrD < ptrE) {
	   MakeNUL (part);
	   for (p = ptr+1; p != ptrD; p++) catchrX (part, (int) *p,
						    MAX_PART_LEN);
	   MakeNUL (mod);
	   for (p = ptrD+1; p != ptrE; p++) catchrX (mod, (int) *p,
						     MAX_MOD_LEN);
	 }
	 else {
	   MakeNUL (part);
	   for (p = ptr+1; p != ptrE; p++) catchrX (part, (int) *p,
						    MAX_PART_LEN);
	   MakeNUL (mod);
	 }
	 ptr = ptrE + 1;
	 /*********************************************************************
	 * Day (of month), <dom>.
	 *********************************************************************/
	 if (!strcmp(part,"dom")) {
	   if (!AppendIntegerPart(encoded,day,0,FALSE,mod)) return;
	   break;
	 }
	 /*********************************************************************
	 * Day of year, <doy>.
	 *********************************************************************/
	 if (!strcmp(part,"doy")) {
	   long doy = JulianDay(year,month,day) - JulianDay(year,1L,1L) + 1;
	   if (!AppendIntegerPart(encoded,doy,3,TRUE,mod)) return;
	   break;
	 }
	 /*********************************************************************
	 * Month (3-character), <month>.
	 *********************************************************************/
	 if (!strcmp(part,"month")) {
	   strcatX (encoded, MonthToken(month), EPOCHx_STRING_MAX);
	   break;
	 }
	 /*********************************************************************
	 * Month (digits), <mm>.
	 *********************************************************************/
	 if (!strcmp(part,"mm")) {
	   if (!AppendIntegerPart(encoded,month,0,FALSE,mod)) return;
	   break;
	 }
	 /*********************************************************************
	 * Year (full), <year>.
	 *********************************************************************/
	 if (!strcmp(part,"year")) {
	   if (!AppendIntegerPart(encoded,year,4,TRUE,mod)) return;
	   break;
	 }
	 /*********************************************************************
	 * Year (2-digit), <yr>.
	 *********************************************************************/
	 if (!strcmp(part,"yr")) {
	   long yr = year % 100L;
	   if (!AppendIntegerPart(encoded,yr,2,TRUE,mod)) return;
	   break;
	 }
	 /*********************************************************************
	 * Hour, <hour>.
	 *********************************************************************/
	 if (!strcmp(part,"hour")) {
	   if (!AppendIntegerPart(encoded,hour,2,TRUE,mod)) return;
	   break;
	 }
	 /*********************************************************************
	 * Minute, <min>.
	 *********************************************************************/
	 if (!strcmp(part,"min")) {
	   if (!AppendIntegerPart(encoded,minute,2,TRUE,mod)) return;
	   break;
	 }
	 /*********************************************************************
	 * Second, <sec>.
	 *********************************************************************/
	 if (!strcmp(part,"sec")) {
	   if (!AppendIntegerPart(encoded,second,2,TRUE,mod)) return;
	   break;
	 }
	 /*********************************************************************
	 * Fraction of second, <fos>.
	 *********************************************************************/
	 if (!strcmp(part,"fos")) {
	   double fos = ((double) msec) / 1000.0;
	   if (!AppendFractionPart(encoded,fos,3,mod)) return;
	   break;
	 }
	 /*********************************************************************
	 * Fraction of day, <fod>.
	 *********************************************************************/
	 if (!strcmp(part,"fod")) {
	   double fod = ((double) hour / 24.0) +
			((double) minute / 1440.0) +
			((double) second / 86400.0) +
			((double) msec / 86400000.0);
	   if (!AppendFractionPart(encoded,fod,8,mod)) return;
	   break;
	 }
	 /*********************************************************************
	 * Unknown/unsupported part.
	 *********************************************************************/
	 strcatX (encoded, "?", EPOCHx_STRING_MAX);
	 return;
       /***********************************************************************
       * Character to be copied.
       ***********************************************************************/
       default:
	 catchrX (encoded, (int) *ptr, EPOCHx_STRING_MAX);
	 ptr++;
	 break;
     }
  }
}

/******************************************************************************
* encodeEPOCH16_x.
******************************************************************************/

VISIBLE_PREFIX void encodeEPOCH16_x (epoch, format, encoded)
double epoch[2];
char format[EPOCHx_FORMAT_MAX];
char encoded[EPOCHx_STRING_MAX+1];
{
  char *ptr = format;		/* Current position in format string. */
  char *ptrD;			/* Pointer to decimal point. */
  char *ptrE;			/* Pointer to ending right angle bracket. */
  char *p;			/* Temporary pointer. */
  char part[MAX_PART_LEN+1];	/* Part being encoded. */
  char mod[MAX_MOD_LEN+1];	/* Part modifier. */
  long year, month, day, hour,
       minute, second; 
  long msec, usec, nsec, psec;	/* EPOCH components. */
  /****************************************************************************
  * Break EPOCH down into its components, validate the format specification,
  * and initialize the encoded string.
  ****************************************************************************/
  if (format == NULL || NULstring(format)) {
    encodeEPOCH (epoch[0]*1000.0, encoded);
    /* add epoch[1]... */
    return;
  }
  EPOCH16breakdown (epoch, &year, &month, &day, &hour, &minute, &second, 
		    &msec, &usec, &nsec, &psec);
  MakeNUL (encoded);
  /****************************************************************************
  * Scan format string.
  ****************************************************************************/
  for (;;) {
     switch (*ptr) {
       /***********************************************************************
       * End of format string.
       ***********************************************************************/
       case NUL:
	 return;
       /***********************************************************************
       * Start of part to be encoded.
       ***********************************************************************/
       case '<':
	 /*********************************************************************
	 * If next character is also a `<' (character stuffing), then append
	 * a `<' and move on.
	 *********************************************************************/
	 if (*(ptr+1) == '<') {
	   strcatX (encoded, "<", EPOCHx_STRING_MAX);
	   ptr += 2;
	   break;
	 }
	 /*********************************************************************
	 * Find ending right angle bracket.
	 *********************************************************************/
	 ptrE = strchr (ptr + 1, '>');
	 if (ptrE == NULL) {
	   strcatX (encoded, "?", EPOCHx_STRING_MAX);
	   return;
	 }
	 /*********************************************************************
	 * Check for a part modifier.
	 *********************************************************************/
	 ptrD = strchr (ptr + 1, '.');
	 if (ptrD != NULL && ptrD < ptrE) {
	   MakeNUL (part);
	   for (p = ptr+1; p != ptrD; p++) catchrX (part, (int) *p,
						    MAX_PART_LEN);
	   MakeNUL (mod);
	   for (p = ptrD+1; p != ptrE; p++) catchrX (mod, (int) *p,
						     MAX_MOD_LEN);
	 }
	 else {
	   MakeNUL (part);
	   for (p = ptr+1; p != ptrE; p++) catchrX (part, (int) *p,
						    MAX_PART_LEN);
	   MakeNUL (mod);
	 }
	 ptr = ptrE + 1;
	 /*********************************************************************
	 * Day (of month), <dom>.
	 *********************************************************************/
	 if (!strcmp(part,"dom")) {
	   if (!AppendIntegerPart(encoded,day,0,FALSE,mod)) return;
	   break;
	 }
	 /*********************************************************************
	 * Day of year, <doy>.
	 *********************************************************************/
	 if (!strcmp(part,"doy")) {
	   long doy = JulianDay(year,month,day) - JulianDay(year,1L,1L) + 1;
	   if (!AppendIntegerPart(encoded,doy,3,TRUE,mod)) return;
	   break;
	 }
	 /*********************************************************************
	 * Month (3-character), <month>.
	 *********************************************************************/
	 if (!strcmp(part,"month")) {
	   strcatX (encoded, MonthToken(month), EPOCHx_STRING_MAX);
	   break;
	 }
	 /*********************************************************************
	 * Month (digits), <mm>.
	 *********************************************************************/
	 if (!strcmp(part,"mm")) {
	   if (!AppendIntegerPart(encoded,month,0,FALSE,mod)) return;
	   break;
	 }
	 /*********************************************************************
	 * Year (full), <year>.
	 *********************************************************************/
	 if (!strcmp(part,"year")) {
	   if (!AppendIntegerPart(encoded,year,4,TRUE,mod)) return;
	   break;
	 }
	 /*********************************************************************
	 * Year (2-digit), <yr>.
	 *********************************************************************/
	 if (!strcmp(part,"yr")) {
	   long yr = year % 100L;
	   if (!AppendIntegerPart(encoded,yr,2,TRUE,mod)) return;
	   break;
	 }
	 /*********************************************************************
	 * Hour, <hour>.
	 *********************************************************************/
	 if (!strcmp(part,"hour")) {
	   if (!AppendIntegerPart(encoded,hour,2,TRUE,mod)) return;
	   break;
	 }
	 /*********************************************************************
	 * Minute, <min>.
	 *********************************************************************/
	 if (!strcmp(part,"min")) {
	   if (!AppendIntegerPart(encoded,minute,2,TRUE,mod)) return;
	   break;
	 }
	 /*********************************************************************
	 * Second, <sec>.
	 *********************************************************************/
	 if (!strcmp(part,"sec")) {
	   if (!AppendIntegerPart(encoded,second,2,TRUE,mod)) return;
	   break;
	 }
         /*********************************************************************
         * Fraction of second, <fos>.
         *********************************************************************/
         if (!strcmp(part,"fos")) {
           double fos = (double) msec / pow (10.0, 3.0) + 
			(double) usec / pow (10.0, 6.0) +
			(double) nsec / pow (10.0, 9.0) +
			(double) psec / pow (10.0, 12.0);
           if (!AppendFractionPart(encoded,fos,12,mod)) return;
           break;
         }
	 /*********************************************************************
	 * Millisecond, <msc>.
	 *********************************************************************/
	 if (!strcmp(part,"msc")) {
	   if (!AppendIntegerPart(encoded,msec,3,TRUE,mod)) return;
	   break;
	 }
         /*********************************************************************
         * Microsecond, <usc>.
         *********************************************************************/
         if (!strcmp(part,"usc")) {
           if (!AppendIntegerPart(encoded,usec,3,TRUE,mod)) return;
           break;
         }
         /*********************************************************************
         * Nanosecond, <nec>.
         *********************************************************************/
         if (!strcmp(part,"nsc")) {
           if (!AppendIntegerPart(encoded,nsec,3,TRUE,mod)) return;
           break;
         }
         /*********************************************************************
         * Picosecond, <pec>.
         *********************************************************************/
         if (!strcmp(part,"psc")) {
           if (!AppendIntegerPart(encoded,psec,3,TRUE,mod)) return;
           break;
         }
	 /*********************************************************************
	 * Fraction of day, <fod>.
	 *********************************************************************/
	 if (!strcmp(part,"fod")) {
	   double fod = ((double) hour / 24.0) +
			((double) minute / 1440.0) +
			((double) second / 86400.0) +
			((double) msec / 86400000.0);
	   if (!AppendFractionPart(encoded,fod,15,mod)) return;
	   break;
	 }
	 /*********************************************************************
	 * Unknown/unsupported part.
	 *********************************************************************/
	 strcatX (encoded, "?", EPOCHx_STRING_MAX);
	 return;
       /***********************************************************************
       * Character to be copied.
       ***********************************************************************/
       default:
	 catchrX (encoded, (int) *ptr, EPOCHx_STRING_MAX);
	 ptr++;
	 break;
     }
  }
}

static Logical AppendFractionPart (encoded, fraction, defaultWidth, modifier)
char *encoded;
double fraction;
int defaultWidth;
char *modifier;
{
  char ePart[MAX_ePART_LEN+1]; int width, i;
  if (!NULstring(modifier)) {
    if (sscanf(modifier,"%d",&width) != 1) {
      strcatX (encoded, "?", EPOCHx_STRING_MAX);
      return FALSE;
    }
    if (width < 1) {
      strcatX (encoded, "?", EPOCHx_STRING_MAX);
      return FALSE;
    }
  }
  else
    width = defaultWidth;
  if ((width + 2) > MAX_ePART_LEN) 
    width = MAX_ePART_LEN - 2;
  snprintf (ePart, (size_t) sizeof(ePart), "%*.*f", width + 2, width, 
            fraction);
#if defined(alphaosf)
  /****************************************************************************
  * V3.2 of OSF/1 apparently has a bug involving `sprintf'.  The preceeding
  * call to `sprintf' produces a string containing one too many digits after
  * the decimal.  Eg., if width=7 the encoded string might be 0.12345678
  * rather than 0.1234567 as it should be.  So we'll fix it...
  ****************************************************************************/
  ePart[width+2] = NUL;
#endif
  /****************************************************************************
  * If the encoded value was rounded up to 1.000..., then replace all of the
  * digits after the decimal with `9's before appending.
  ****************************************************************************/
  if (ePart[0] == '1') {
    for (i = 0; i < width; i++) ePart[i+2] = '9';
  }
  return AppendPart(encoded,strchr(ePart,'.')+1,width,FALSE);
}

static Logical AppendIntegerPart (encoded, integer, defaultWidth,
				  defaultLeading0, modifier)
char *encoded;
long integer;
int defaultWidth;
Logical defaultLeading0;
char *modifier;
{
  char ePart[MAX_ePART_LEN+1]; int width; Logical leading0;
  if (!NULstring(modifier)) {
    if (sscanf(modifier,"%d",&width) != 1) {
      strcatX (encoded, "?", EPOCHx_STRING_MAX);
      return FALSE;
    }
    if (width < 0) {
      strcatX (encoded, "?", EPOCHx_STRING_MAX);
      return FALSE;
    }
    leading0 = (modifier[0] == '0');
  }
  else {
    width = defaultWidth;
    leading0 = defaultLeading0;
  }
  snprintf (ePart, (size_t) sizeof(ePart), "%ld", integer);
  return AppendPart (encoded, ePart, width, leading0);
}

static Logical AppendPart (encoded, ePart, width, leading0)
char *encoded;
char *ePart;
int width;
Logical leading0;
{
  int i;
  if (width == 0) {
    strcatX (encoded, ePart, EPOCHx_STRING_MAX);
  }
  else {
    int length = (int) strlen(ePart);
    if (length > width) {
      for (i = 0; i < width; i++) strcatX (encoded, "*", EPOCHx_STRING_MAX);
    }
    else {
      int pad = width - length;
      if (pad > 0) {
        for (i = 0; i < pad; i++) strcatX (encoded, BOO(leading0,"0"," "),
					   EPOCHx_STRING_MAX);
      }
      strcatX (encoded, ePart, EPOCHx_STRING_MAX);
    }
  }
  return TRUE;
}

void encodeEPOCH16x2 (epoch, encoded)
double epoch;
char *encoded;
{
  long msec, usec, nsec, psec;
  double mmm;
  psec = (long) fmod (epoch, (double) 1000.0);
  mmm = epoch / (double) 1000.0;
  nsec = (long) fmod (mmm, (double) 1000.0);
  mmm = mmm / (double) 1000.0;
  usec = (long) fmod (mmm, (double) 1000.0);  
  msec = (long) (mmm / (double) 1000.0); 
  snprintf(encoded, (size_t) 15+1, "%3.3ld.%3.3ld.%3.3ld.%3.3ld", 
           msec, usec, nsec, psec);
  encoded[15] = NUL;
  return;
}

void encodeEPOCH16x3 (epoch, encoded, width)
double epoch[2];
char *encoded;
size_t width;
{
  char tmp[17+1];
  long year, month, day, hour, minute, second, msec, usec, nsec, psec;
  double mmm1, mmm2, mmm3;
  EPOCH16breakdown (epoch, &year, &month, &day, &hour, &minute, &second, 
		    &msec, &usec, &nsec, &psec);
  mmm1 = ((double) hour * 3600.0 + (double) minute * 60.0 + (double) second) /
	  86400.0; 
  mmm2 = ((double) msec * pow(10.0, 9.0) + (double) usec * pow(10.0, 6.0) + 
	  (double) nsec * pow(10.0, 3.0) + (double) psec) / 
	  (86400.0 * pow(10.0, 12.0));
  mmm3 = mmm1 + mmm2;
  if (mmm3 >= 1.0) {
    if (mmm1 > 0.0 || mmm2 > 1.0E9) 
      strcpyX (tmp, "0.999999999999999", 0);
  } else
    snprintf(tmp, (size_t) sizeof(tmp), "%.15f", mmm3);
  strcpyX (encoded, tmp+2, (int) width);
  return;
}

void encodeEPOCH16x4 (epoch, encoded)
double epoch;
char *encoded;
{
  long msec, usec, nsec, psec;
  double mmm;
  psec = (long) fmod (epoch, (double) 1000.0);
  mmm = epoch / (double) 1000.0;
  nsec = (long) fmod (mmm, (double) 1000.0);
  mmm = mmm / (double) 1000.0;
  usec = (long) fmod (mmm, (double) 1000.0);
  msec = (long) (mmm / (double) 1000.0);
  snprintf(encoded, (size_t) 12+1, "%3.3ld%3.3ld%3.3ld%3.3ld",
           msec, usec, nsec, psec);
  encoded[12] = NUL;
  return;
}

/******************************************************************************
* computeEPOCH.
* Computes (and returns) an EPOCH value based on its component parts.
* ILLEGAL_EPOCH_VALUE is returned if an illegal component part is detected.
******************************************************************************/

VISIBLE_PREFIX double computeEPOCH (year, month, day, hour, minute, second,
				    msec)
long year, month, day, hour, minute, second, msec;
{
  long daysSince0AD, msecInDay;

  /****************************************************************************
  * Mark 9999-12-31 23:59:59:999 as an invalid date.
  ****************************************************************************/
  if (year == 9999L && month == 12L && day == 31L && hour == 23L &&
      minute == 59L && second == 59L && msec == 999L) 
    return -1.0*pow(10.0, 31.0);
  if (year < 0L) return ILLEGAL_EPOCH_VALUE;
  if ((year > 9999L) || (month < 0L || month > 12L) ||
      (hour < 0L || hour > 23L) || (minute < 0L || minute > 59L) || 
      (second < 0L || second > 59L) || (msec < 0L || msec > 999L))
    return computeEpoch(year,month,day,hour,minute,second,msec);
  if (month == 0L) {
    if (day < 1L || day > 366L) 
      return computeEpoch(year,month,day,hour,minute,second,msec);
  } else {
    if (day < 1L || day > 31L) 
      return computeEpoch(year,month,day,hour,minute,second,msec);
  }
  if (hour == 0L && minute == 0L && second == 0L) {
    if (msec < 0L || msec > 86399999L)
      return computeEpoch(year,month,day,hour,minute,second,msec);
  }
  /****************************************************************************
  * Calculate the days since 0 A.D (1-Jan-0000).  If a value of zero is passed
  * in for `month', assume that `day' is the day-of-year (DOY) with January 1st
  * being day 1.
  ****************************************************************************/
  if (month == 0L) {
    daysSince0AD = JulianDay(year,1L,1L) + (day-1) - 1721060L;
  }
  else {
    daysSince0AD = JulianDay(year,month,day) - 1721060L;
  }
  /****************************************************************************
  * Calculate the millisecond in the day (with the first millisecond being 0).
  * If values of zero are passed in for `hour', `minute', and `second', assume
  * that `msec' is the millisecond in the day.
  ****************************************************************************/
  if (hour == 0L && minute == 0L && second == 0L) {
    msecInDay = msec;
  }
  else {
    msecInDay = (3600000L * hour) + (60000L * minute) + (1000L * second) + msec;
  }
  /****************************************************************************
  * Return the milliseconds since 0 A.D.
  ****************************************************************************/
  return (86400000.0 * daysSince0AD + (double) msecInDay);
}

/******************************************************************************
* computeEPOCH16.
* This function is an extension of computeEPOCH. It is used to handle the time
* that may contain as small as picoseconds. 
******************************************************************************/

VISIBLE_PREFIX double computeEPOCH16 (year, month, day, hour, minute, 
                                      second, msec, usec, nsec, psec, 
				      epoch)
long year, month, day, hour, minute, second, msec, usec, nsec, psec;
double epoch[2];
{
  long daysSince0AD; 
  /****************************************************************************
  * Mark 9999-12-31 23:59:59:999:999:999:999 as an invalid date.
  ****************************************************************************/
  if (year == 9999L && month == 12L && day == 31L && hour == 23L &&
      minute == 59L && second == 59L && msec == 999L && usec == 999L &&
      nsec == 999L && psec == 999L) {
    epoch[0] = -1.0E31;
    epoch[1] = -1.0E31;
    return 0.0;
  }
  if (year < 0L) return ILLEGAL_EPOCH_VALUE;
  if ((year > 9999L) || (month < 0L || month > 12L) ||
      (hour < 0L || hour > 23L) || (minute < 0L || minute > 59L) ||
      (second < 0L || second > 59L) || (msec < 0L || msec > 999L) ||
      (usec < 0L || usec > 999L) || (nsec < 0L || nsec > 999L) ||
      (psec < 0L || psec > 999L))
    return computeEpoch16(year, month, day, hour, minute, second, msec, usec,
                          nsec, psec, epoch);
  if (month == 0L) {
    if (day < 1L || day > 366L)
      return computeEpoch16(year,month,day,hour,minute,second,msec,usec,
                            nsec, psec, epoch);
  } else {
    if (day < 1L || day > 31L)
      return computeEpoch16(year,month,day,hour,minute,second,msec,usec,
                            nsec, psec, epoch);
  }
  /****************************************************************************
  * Calculate the days since 0 A.D (1-Jan-0000).  If a value of zero is passed
  * in for `month', assume that `day' is the day-of-year (DOY) with January 1st
  * being day 1.
  ****************************************************************************/
  if (month == 0L) {
    daysSince0AD = JulianDay(year,1L,1L) + (day-1) - 1721060L;
  }     
  else {
    daysSince0AD = JulianDay(year,month,day) - 1721060L;
  }
  /****************************************************************************
  * Return the seconds and picoseconds since 0 A.D.
  ****************************************************************************/
  epoch[0] = 86400.0 * daysSince0AD + 3600.0 * hour + 60.0 * minute + 
             (double) second;
  epoch[1] = (double) psec + pow(10.0, 3.0) * nsec + pow(10.0, 6.0) * usec + 
             pow(10.0, 9.0) * msec;
  return (double) 0.0;
}

/******************************************************************************
* EPOCHbreakdown.
* Breaks an EPOCH value down into its component parts.
******************************************************************************/

VISIBLE_PREFIX void EPOCHbreakdown (epoch, year, month, day, hour, minute,
				    second, msec)
double epoch;
long *year, *month, *day, *hour, *minute, *second, *msec;
{
  long jd,i,j,k,l,n;
  double msec_AD, second_AD, minute_AD, hour_AD, day_AD;

  if (epoch == -1.0E31) {
    *year = 9999;
    *month = 12;
    *day = 31;
    *hour = 23;
    *minute = 59;
    *second = 59;
    *msec = 999;
    return;
  }

  if (NegativeZeroReal8(&epoch)) {
    *year = 0;
    *month = 0;
    *day = 0;
    *hour = 0;
    *minute = 0;
    *second = 0;
    *msec = 0;
    return;
  }

  if (epoch < 0.0) epoch = -epoch;
  epoch = MINIMUM ((double)MAX_EPOCH_BINARY, epoch);
  msec_AD = epoch;
  second_AD = msec_AD / 1000.0;
  minute_AD = second_AD / 60.0;
  hour_AD = minute_AD / 60.0;
  day_AD = hour_AD / 24.0;

  jd = (long) (1721060 + day_AD);
  l=jd+68569;
  n=4*l/146097;
  l=l-(146097*n+3)/4;
  i=4000*(l+1)/1461001;
  l=l-1461*i/4+31;
  j=80*l/2447;
  k=l-2447*j/80;
  l=j/11;
  j=j+2-12*l;
  i=100*(n-49)+i+l;

  *year = i;
  *month = j;
  *day = k;

  *hour   = (long) fmod (hour_AD, (double) 24.0);
  *minute = (long) fmod (minute_AD, (double) 60.0);
  *second = (long) fmod (second_AD, (double) 60.0);
  *msec   = (long) fmod (msec_AD, (double) 1000.0);

  return;
}

/******************************************************************************
* EPOCH16breakdown. 
* This function is an extension of EPOCHbreakdown. It is used to handle the 
* time that may contain as small as picoseconds. 
******************************************************************************/

VISIBLE_PREFIX void EPOCH16breakdown (epoch, year, month, day, hour, 
                                      minute, second, msec, usec, nsec, psec)
double epoch[2];
long *year, *month, *day, *hour, *minute, *second;
long *msec, *usec, *nsec, *psec;
{
  long jd,i,j,k,l,n;
  double second_AD, minute_AD, hour_AD, day_AD;
  double psec_SC, nsec_SC, usec_SC;

  if (epoch[0] == -1.0E31 && epoch[1] == -1.0E31) {
    *year = 9999;
    *month = 12;
    *day = 31;
    *hour = 23;
    *minute = 59;
    *second = 59;
    *msec = 999;
    *usec = 999;
    *nsec = 999;
    *psec = 999;
    return;
  }
    
  if (NegativeZeroReal8(&epoch[0])) {
    *year = 0;
    *month = 0;
    *day = 0;
    *hour = 0;
    *minute = 0;
    *second = 0;
  }
  if (NegativeZeroReal8(&epoch[1])) {
    *msec = 0;
    *usec = 0;
    *nsec = 0;
    *psec = 0;
  }

  if (epoch[0] < 0.0) epoch[0] = -epoch[0];
  if (epoch[1] < 0.0) epoch[1] = -epoch[1];
  epoch[0] = MINIMUM ((double)MAX_EPOCH16_1_BINARY, epoch[0]);
  if (epoch[0] == MAX_EPOCH16_1_BINARY)
    epoch[1] = MINIMUM ((double)MAX_EPOCH16_2_BINARY, epoch[1]);
  else
    epoch[1] = MINIMUM ((double)MAX_EPOCH16_2_BINARY+1.0, epoch[1]);

  second_AD = epoch[0];
  minute_AD = second_AD / 60.0;
  hour_AD = minute_AD / 60.0;
  day_AD = hour_AD / 24.0;

  jd = (long) (1721060 + day_AD);
  l=jd+68569;
  n=4*l/146097;
  l=l-(146097*n+3)/4;
  i=4000*(l+1)/1461001;
  l=l-1461*i/4+31;
  j=80*l/2447;
  k=l-2447*j/80;
  l=j/11;
  j=j+2-12*l;
  i=100*(n-49)+i+l;

  *year = i;
  *month = j;
  *day = k;

  *hour   = (long) fmod (hour_AD, (double) 24.0);
  *minute = (long) fmod (minute_AD, (double) 60.0);
  *second = (long) fmod (second_AD, (double) 60.0);
  
  psec_SC = epoch[1];
  *psec = (long) fmod(epoch[1], (double) 1000.0);
  nsec_SC = psec_SC / 1000.0;
  *nsec = (long) fmod(nsec_SC, (double) 1000.0);
  usec_SC = nsec_SC / 1000.0;
  *usec = (long) fmod(usec_SC, (double) 1000.0);
  *msec = (long) (usec_SC / (double) 1000.0);

  return;
}

/******************************************************************************
* JulianDay.
* The year, month, and day are assumed to have already been validated.  This
* is the day since 0 AD/1 BC.  (Julian day may not be the proper term.)
******************************************************************************/

static long JulianDay (y,m,d)
long y, m, d;
{
  return (367*y-7*(y+(m+9)/12)/4-3*((y+(m-9)/7)/100+1)/4+275*m/9+d+1721029);
}

/******************************************************************************
* MonthToken.
******************************************************************************/

static char *MonthToken (month)
long month;
{
  switch (month) {
    case 1: return "Jan";
    case 2: return "Feb";
    case 3: return "Mar";
    case 4: return "Apr";
    case 5: return "May";
    case 6: return "Jun";
    case 7: return "Jul";
    case 8: return "Aug";
    case 9: return "Sep";
    case 10: return "Oct";
    case 11: return "Nov";
    case 12: return "Dec";
  }
  return "???";
}

/******************************************************************************
* FullDayToken.
******************************************************************************/

static char *FullDayToken (day3)
char *day3;
{
  if (!strcmp(day3,"Sun")) return "Sunday";
  if (!strcmp(day3,"Mon")) return "Monday";
  if (!strcmp(day3,"Tue")) return "Tuesday";
  if (!strcmp(day3,"Wed")) return "Wednesday";
  if (!strcmp(day3,"Thu")) return "Thursday";
  if (!strcmp(day3,"Fri")) return "Friday";
  if (!strcmp(day3,"Sat")) return "Saturday";
  return "Someday";
}

/******************************************************************************
* computeEpoch.
******************************************************************************/

static double computeEpoch (year, month, day, hour, minute, second, msec)
long year, month, day, hour, minute, second, msec;
{
  long daysSince0AD;
  double msecFromEpoch, msecInDay;

  if (month == 0L) {
    daysSince0AD = JulianDay(year,1L,1L) + (day-1L) - 1721060L;
  }
  else {
    if (month < 0L) {
      --year;
      month = 13 + month;
    }
    daysSince0AD = JulianDay(year,month,day) - 1721060L;
  }
  if (daysSince0AD < 0L) return ILLEGAL_EPOCH_VALUE;
  msecInDay = 3600000.0 * hour + 60000.0 * minute + 1000.0 * second + 
              (double) msec;
  msecFromEpoch =  86400000.0 * daysSince0AD + msecInDay;
  if (msecFromEpoch < 0.0)
    return ILLEGAL_EPOCH_VALUE;
  else
    return msecFromEpoch;
}

/******************************************************************************
* computeEpoch16.
******************************************************************************/

static double computeEpoch16 (year, month, day, hour, minute, second, msec,
                              usec, nsec, psec, epoch)
long year, month, day, hour, minute, second, msec, usec, nsec, psec;
double epoch[2];
{
  long daysSince0AD;
  if (month == 0L) {
    daysSince0AD = JulianDay(year,1L,1L) + (day-1L) - 1721060L;
  }
  else {
    if (month < 0L) {
      --year;
      month = 13 + month;
    }
    daysSince0AD = JulianDay(year,month,day) - 1721060L;
  }
  if (daysSince0AD < 0L) return ILLEGAL_EPOCH_VALUE;
  epoch[0] = 86400.0 * daysSince0AD + 3600.0 * hour + 60.0 * minute + 
             (double) second;
  epoch[1] = (double) psec + pow(10.0, 3.0) * nsec + pow(10.0, 6.0) * usec + 
             pow(10.0, 9.0) * msec;
  if (epoch[1] < 0.0 || epoch[1] >= pow(10.0, 12.0)) {
    int sec;
    double tmp;
    if (epoch[1] < 0.0) {
      sec = (int) (epoch[1] / pow(10.0, 12.0));
      tmp = epoch[1] - sec * pow(10.0, 12.0);
      if (tmp != 0.0 && tmp != -0.0) {
        epoch[0] = epoch[0] + (double) sec - 1.0;
        epoch[1] = pow(10.0, 12.0) + tmp;
      } else {
        epoch[0] = epoch[0] + (double) sec;
        epoch[1] = 0.0;
      }
    } else {
      sec = (int) (epoch[1] / pow(10.0, 12.0));
      tmp = epoch[1] - sec * pow(10.0, 12.0);
      if (tmp != 0.0 && tmp != -0.0) {
        epoch[1] = tmp;
        epoch[0] = epoch[0] + (double) sec;
      } else {
        epoch[1] = 0.0;
        epoch[0] = epoch[0] + (double) sec;
      }
    }
  }
  if (epoch[0] < 0.0)
    return ILLEGAL_EPOCH_VALUE;
  else
    return 0.0;
}

/******************************************************************************
* TimeStamp.
* Gets the date & time from the system and encodes a string in the following
* form:
*
*     ddddddddd, dd-mmm-yyyy hh:mm:ss
*
* Examples:
*
*     Saturday, 23-Oct-1993 09:37:34
*     Sunday, 2-Jan-1994 10:00:00
*     Wednesday, 27-Oct-1993 23:59:59
*
* Trailing blanks are not appended if the string is shorter than its maximum
* possible length. The passed string buffer has to be at least 32 bytes long.
******************************************************************************/

VISIBLE_PREFIX void TimeStamp (stampStr)
char *stampStr;
{
  time_t bintim;
  char ctimeStr[CTIME_STRING_LEN+1], dayOfWeek3[3+1], dayOfMonth[2+1],
       year[4+1], month[3+1], hourMinuteSecond[8+1];
  time (&bintim);
  strcpyX (ctimeStr, ctime(&bintim), CTIME_STRING_LEN);
  strcpyX (dayOfWeek3, ctimeStr, 3);
  strcpyX (dayOfMonth, &ctimeStr[8], 2);
  if (dayOfMonth[0] == ' ') memmove (dayOfMonth, &dayOfMonth[1], 2);
  strcpyX (year, &ctimeStr[20], 4);
  strcpyX (month, &ctimeStr[4], 3);
  strcpyX (hourMinuteSecond, &ctimeStr[11], 8);
  snprintf (stampStr, (size_t) 31+1, "%s, %s-%s-%s %s",
	    FullDayToken(dayOfWeek3), dayOfMonth, month, year,
	    hourMinuteSecond);
  return;
}
