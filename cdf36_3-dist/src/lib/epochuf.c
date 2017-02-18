/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                 EPOCH utility routines for Fortran applications.
*
*  Version 1.2, 9-Sep-96, Hughes STX.
*
*  Modification history:
*
*   V1.0   7-Nov-94, J Love	Original version.
*   V1.1  13-Jun-95, J Love	EPOCH custom format.  Linux.
*   V1.2   9-Sep-96, J Love	CDF V2.6.
*   V2.6  29-Jan-04, M Liu	Added a new set of CDF_EPOCH16 functions for 
*                               handling fraction of a second up to picosecond.
*
******************************************************************************/

#include "cdflib.h"

/******************************************************************************
* parse_EPOCH (FORTRAN equivalent of parseEPOCH).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(parse_epoch__,parse_epoch_,parse_epoch,PARSE_EPOCH)
(string, epoch Fif_GHOSTARG(string_len))
void *string;
double *epoch;
Fif_GHOSTDEF(string_len)
{
  struct STRINGstruct *ssh = NULL;
#if defined(Fif_DESCR)
  *epoch = parseEPOCH(DESCRtoREFnul(string,EPOCH_STRING_LEN,&ssh));
#endif
#if defined(Fif_GHOSTLEN)
  *epoch = parseEPOCH(NULterminate(string,Fif_GHOSTUSE(string_len),&ssh));
#endif
#if defined(Fif_NOLEN)
  *epoch = parseEPOCH(FindEndNUL(string,EPOCH_STRING_LEN,&ssh));
#endif
  FreeStrings (ssh);
  return;
}

/******************************************************************************
* parse_EPOCH1 (FORTRAN equivalent of parseEPOCH1).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(parse_epoch1__,parse_epoch1_,parse_epoch1,PARSE_EPOCH1)
(string, epoch Fif_GHOSTARG(string_len))
void *string;
double *epoch;
Fif_GHOSTDEF(string_len)
{
  struct STRINGstruct *ssh = NULL;
#if defined(Fif_DESCR)
  *epoch = parseEPOCH1(DESCRtoREFnul(string,EPOCH1_STRING_LEN,&ssh));
#endif
#if defined(Fif_GHOSTLEN)
  *epoch = parseEPOCH1(NULterminate(string,Fif_GHOSTUSE(string_len),&ssh));
#endif
#if defined(Fif_NOLEN)
  *epoch = parseEPOCH1(FindEndNUL(string,EPOCH1_STRING_LEN,&ssh));
#endif
  FreeStrings (ssh);
  return;
}

/******************************************************************************
* parse_EPOCH2 (FORTRAN equivalent of parseEPOCH2).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(parse_epoch2__,parse_epoch2_,parse_epoch2,PARSE_EPOCH2)
(string, epoch Fif_GHOSTARG(string_len))
void *string;
double *epoch;
Fif_GHOSTDEF(string_len)
{
  struct STRINGstruct *ssh = NULL;
#if defined(Fif_DESCR)
  *epoch = parseEPOCH2(DESCRtoREFnul(string,EPOCH2_STRING_LEN,&ssh));
#endif
#if defined(Fif_GHOSTLEN)
  *epoch = parseEPOCH2(NULterminate(string,Fif_GHOSTUSE(string_len),&ssh));
#endif
#if defined(Fif_NOLEN)
  *epoch = parseEPOCH2(FindEndNUL(string,EPOCH2_STRING_LEN,&ssh));
#endif
  FreeStrings (ssh);
  return;
}

/******************************************************************************
* parse_EPOCH3 (FORTRAN equivalent of parseEPOCH3).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(parse_epoch3__,parse_epoch3_,parse_epoch3,PARSE_EPOCH3)
(string, epoch Fif_GHOSTARG(string_len))
void *string;
double *epoch;
Fif_GHOSTDEF(string_len)
{
  struct STRINGstruct *ssh = NULL;
#if defined(Fif_DESCR)
  *epoch = parseEPOCH3(DESCRtoREFnul(string,EPOCH3_STRING_LEN,&ssh));
#endif
#if defined(Fif_GHOSTLEN)
  *epoch = parseEPOCH3(NULterminate(string,Fif_GHOSTUSE(string_len),&ssh));
#endif
#if defined(Fif_NOLEN)
  *epoch = parseEPOCH3(FindEndNUL(string,EPOCH3_STRING_LEN,&ssh));
#endif
  FreeStrings (ssh);
  return;
}

/******************************************************************************
* parse_EPOCH4 (FORTRAN equivalent of parseEPOCH4).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(parse_epoch4__,parse_epoch4_,parse_epoch4,PARSE_EPOCH4)
(string, epoch Fif_GHOSTARG(string_len))
void *string;
double *epoch;
Fif_GHOSTDEF(string_len)
{
  struct STRINGstruct *ssh = NULL;
#if defined(Fif_DESCR)
  *epoch = parseEPOCH4(DESCRtoREFnul(string,EPOCH4_STRING_LEN,&ssh));
#endif
#if defined(Fif_GHOSTLEN)
  *epoch = parseEPOCH4(NULterminate(string,Fif_GHOSTUSE(string_len),&ssh));
#endif
#if defined(Fif_NOLEN)
  *epoch = parseEPOCH4(FindEndNUL(string,EPOCH4_STRING_LEN,&ssh));
#endif
  FreeStrings (ssh);
  return;
}

/******************************************************************************
* encode_EPOCH (FORTRAN equivalent of encodeEPOCH).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(encode_epoch__,encode_epoch_,encode_epoch,ENCODE_EPOCH)
(epoch, string Fif_GHOSTARG(string_len))
double *epoch;
void *string;
Fif_GHOSTDEF(string_len)
{
  char tString[EPOCH_STRING_LEN+1];
  encodeEPOCH (*epoch, tString);
#if defined(Fif_GHOSTLEN)
  CtoFORTstring (tString, string, Fif_GHOSTUSE(string_len));
#else
  CtoFORTstring (tString, string, EPOCH_STRING_LEN);
#endif
  return;
}

/******************************************************************************
* encode_EPOCH1 (FORTRAN equivalent of encodeEPOCH1).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(encode_epoch1__,encode_epoch1_,encode_epoch1,ENCODE_EPOCH1)
(epoch, string Fif_GHOSTARG(string_len))
double *epoch;
void *string;
Fif_GHOSTDEF(string_len)
{
  char tString[EPOCH1_STRING_LEN+1];
  encodeEPOCH1 (*epoch, tString);
#if defined(Fif_GHOSTLEN)
  CtoFORTstring (tString, string, Fif_GHOSTUSE(string_len));
#else
  CtoFORTstring (tString, string, EPOCH1_STRING_LEN);
#endif
  return;
}

/******************************************************************************
* encode_EPOCH2 (FORTRAN equivalent of encodeEPOCH2).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(encode_epoch2__,encode_epoch2_,encode_epoch2,ENCODE_EPOCH2)
(epoch, string Fif_GHOSTARG(string_len))
double *epoch;
void *string;
Fif_GHOSTDEF(string_len)
{
  char tString[EPOCH2_STRING_LEN+1];
  encodeEPOCH2 (*epoch, tString);
#if defined(Fif_GHOSTLEN)
  CtoFORTstring (tString, string, Fif_GHOSTUSE(string_len));
#else
  CtoFORTstring (tString, string, EPOCH2_STRING_LEN);
#endif
  return;
}

/******************************************************************************
* encode_EPOCH3 (FORTRAN equivalent of encodeEPOCH3).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(encode_epoch3__,encode_epoch3_,encode_epoch3,ENCODE_EPOCH3)
(epoch, string Fif_GHOSTARG(string_len))
double *epoch;
void *string;
Fif_GHOSTDEF(string_len)
{
  char tString[EPOCH3_STRING_LEN+1];
  encodeEPOCH3 (*epoch, tString);
#if defined(Fif_GHOSTLEN)
  CtoFORTstring (tString, string, Fif_GHOSTUSE(string_len));
#else
  CtoFORTstring (tString, string, EPOCH3_STRING_LEN);
#endif
  return;
}

/******************************************************************************
* encode_EPOCH4 (FORTRAN equivalent of encodeEPOCH4).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(encode_epoch4__,encode_epoch4_,encode_epoch4,ENCODE_EPOCH4)
(epoch, string Fif_GHOSTARG(string_len))
double *epoch;
void *string;
Fif_GHOSTDEF(string_len)
{
  char tString[EPOCH4_STRING_LEN+1];
  encodeEPOCH4 (*epoch, tString);
#if defined(Fif_GHOSTLEN)
  CtoFORTstring (tString, string, Fif_GHOSTUSE(string_len));
#else
  CtoFORTstring (tString, string, EPOCH4_STRING_LEN);
#endif
  return;
}

/******************************************************************************
* encode_EPOCHx (FORTRAN equivalent of encodeEPOCHx).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(encode_epochx__,encode_epochx_,encode_epochx,ENCODE_EPOCHX)
(epoch, format, string Fif_GHOSTARG(format_len) Fif_GHOSTARG(string_len))
double *epoch;
void *format;
void *string;
Fif_GHOSTDEF(format_len)
Fif_GHOSTDEF(string_len)
{
  struct STRINGstruct *ssh = NULL;
  char tString[EPOCHx_STRING_MAX+1];
#if defined(Fif_DESCR)
  encodeEPOCHx (*epoch, DESCRtoREFnul(format,EPOCHx_FORMAT_MAX,&ssh), tString);
#endif
#if defined(Fif_GHOSTLEN)
  encodeEPOCHx (*epoch, NULterminate(format,Fif_GHOSTUSE(format_len),&ssh),
		tString);
#endif
#if defined(Fif_NOLEN)
  encodeEPOCHx (*epoch, FindEndNUL(format,EPOCHx_FORMAT_MAX,&ssh), tString);
#endif
#if defined(Fif_GHOSTLEN)
  CtoFORTstring (tString, string, Fif_GHOSTUSE(string_len));
#else
  CtoFORTstring (tString, string, EPOCHx_STRING_MAX);
#endif
  return;
}

/******************************************************************************
* EPOCH_breakdown (FORTRAN equivalent of EPOCHbreakdown).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(epoch_breakdown__,
	       epoch_breakdown_,
	       epoch_breakdown,
	       EPOCH_BREAKDOWN)
(epoch, year, month, day, hour, minute, second, msec)
double *epoch;
Int32 *year;
Int32 *month;
Int32 *day;
Int32 *hour;
Int32 *minute;
Int32 *second;
Int32 *msec;
{
  double tEpoch = *epoch;
  long tYear, tMonth, tDay, tHour, tMinute, tSecond, tMsec;
  EPOCHbreakdown (tEpoch, &tYear, &tMonth, &tDay, &tHour, &tMinute, &tSecond,
		  &tMsec);
  *year = (Int32) tYear;
  *month = (Int32) tMonth;
  *day = (Int32) tDay;
  *hour = (Int32) tHour;
  *minute = (Int32) tMinute;
  *second = (Int32) tSecond;
  *msec = (Int32) tMsec;
  return;
}

/******************************************************************************
* compute_EPOCH (FORTRAN equivalent of computeEPOCH).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(compute_epoch__,compute_epoch_,compute_epoch,COMPUTE_EPOCH)
(year, month, day, hour, minute, second, msec, epoch)
Int32 *year;
Int32 *month;
Int32 *day;
Int32 *hour;
Int32 *minute;
Int32 *second;
Int32 *msec;
double *epoch;
{
  *epoch = computeEPOCH ((long) *year, (long) *month, (long) *day,
			 (long) *hour, (long) *minute, (long) *second,
			 (long) *msec);
  return;
}

/******************************************************************************
* parse_EPOCH16 (FORTRAN equivalent of parseEPOCH16).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(parse_epoch16__,parse_epoch16_,parse_epoch16,
               PARSE_EPOCH16)
(string, epoch Fif_GHOSTARG(string_len))
void *string;
double *epoch;
Fif_GHOSTDEF(string_len)
{
  struct STRINGstruct *ssh = NULL;
  double mmm;
#if defined(Fif_DESCR)
  mmm = parseEPOCH16(DESCRtoREFnul(string,EPOCH16_STRING_LEN,&ssh), 
		         epoch);
#endif
#if defined(Fif_GHOSTLEN)
  mmm = parseEPOCH16(NULterminate(string,Fif_GHOSTUSE(string_len),&ssh), 
		         epoch);
#endif
#if defined(Fif_NOLEN)
  mmm = parseEPOCH16(FindEndNUL(string,EPOCH16_STRING_LEN,&ssh), 
		         epoch);
#endif
  FreeStrings (ssh);
  return;
}

/******************************************************************************
* parse_EPOCH16_1 (FORTRAN equivalent of parseEPOCH16_1).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(parse_epoch16_1__,parse_epoch16_1_,parse_epoch16_1,
	       PARSE_EPOCH16_1)
(string, epoch Fif_GHOSTARG(string_len))
void *string;
double *epoch;
Fif_GHOSTDEF(string_len)
{
  struct STRINGstruct *ssh = NULL;
  double mmm;
#if defined(Fif_DESCR)
  mmm = parseEPOCH16_1(DESCRtoREFnul(string,EPOCH16_1_STRING_LEN,&ssh), 
		          epoch);
#endif
#if defined(Fif_GHOSTLEN)
  mmm = parseEPOCH16_1(NULterminate(string,Fif_GHOSTUSE(string_len),&ssh), 
		          epoch);
#endif
#if defined(Fif_NOLEN)
  mmm = parseEPOCH16_1(FindEndNUL(string,EPOCH16_1_STRING_LEN,&ssh), 
		          epoch);
#endif
  FreeStrings (ssh);
  return;
}

/******************************************************************************
* parse_EPOCH16_2 (FORTRAN equivalent of parseEPOCH16_2).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(parse_epoch16_2__,parse_epoch16_2_,parse_epoch16_2,
	       PARSE_EPOCH16_2)
(string, epoch Fif_GHOSTARG(string_len))
void *string;
double *epoch;
Fif_GHOSTDEF(string_len)
{
  struct STRINGstruct *ssh = NULL;
  double mmm;
#if defined(Fif_DESCR)
  mmm = parseEPOCH16_2(DESCRtoREFnul(string,EPOCH16_2_STRING_LEN,&ssh), 
		          epoch);
#endif
#if defined(Fif_GHOSTLEN)
  mmm = parseEPOCH16_2(NULterminate(string,Fif_GHOSTUSE(string_len),&ssh), 
		          epoch);
#endif
#if defined(Fif_NOLEN)
  mmm = parseEPOCH16_2(FindEndNUL(string,EPOCH16_2_STRING_LEN,&ssh), 
		          epoch);
#endif
  FreeStrings (ssh);
  return;
}

/******************************************************************************
* parse_EPOCH16_3 (FORTRAN equivalent of parseEPOCH16_3).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(parse_epoch16_3__,parse_epoch16_3_,parse_epoch16_3,
	       PARSE_EPOCH16_3)
(string, epoch Fif_GHOSTARG(string_len))
void *string;
double *epoch;
Fif_GHOSTDEF(string_len)
{
  struct STRINGstruct *ssh = NULL;
  double mmm;
#if defined(Fif_DESCR)
  mmm = parseEPOCH16_3(DESCRtoREFnul(string,EPOCH16_3_STRING_LEN,&ssh), 
		          epoch);
#endif
#if defined(Fif_GHOSTLEN)
  mmm = parseEPOCH16_3(NULterminate(string,Fif_GHOSTUSE(string_len),&ssh), 
		          epoch);
#endif
#if defined(Fif_NOLEN)
  mmm = parseEPOCH16_3(FindEndNUL(string,EPOCH16_3_STRING_LEN,&ssh), 
		          epoch);
#endif
  FreeStrings (ssh);
  return;
}

/******************************************************************************
* parse_EPOCH16_4 (FORTRAN equivalent of parseEPOCH16_4).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(parse_epoch16_4__,parse_epoch16_4_,parse_epoch16_4,
	       PARSE_EPOCH16_4)
(string, epoch Fif_GHOSTARG(string_len))
void *string;
double *epoch;
Fif_GHOSTDEF(string_len)
{
  struct STRINGstruct *ssh = NULL;
  double mmm;
#if defined(Fif_DESCR)
  mmm = parseEPOCH16_4(DESCRtoREFnul(string,EPOCH16_4_STRING_LEN,&ssh), 
		          epoch);
#endif
#if defined(Fif_GHOSTLEN)
  mmm = parseEPOCH16_4(NULterminate(string,Fif_GHOSTUSE(string_len),&ssh), 
		          epoch);
#endif
#if defined(Fif_NOLEN)
  mmm = parseEPOCH16_4(FindEndNUL(string,EPOCH16_4_STRING_LEN,&ssh), 
		          epoch);
#endif
  FreeStrings (ssh);
  return;
}

/******************************************************************************
* encode_EPOCH16 (FORTRAN equivalent of encodeEPOCH16).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(encode_epoch16__,encode_epoch16_,encode_epoch16,
	       ENCODE_EPOCH16)
(epoch, string Fif_GHOSTARG(string_len))
double *epoch;
void *string;
Fif_GHOSTDEF(string_len)
{
  char tString[EPOCH16_STRING_LEN+1];
  encodeEPOCH16 (epoch, tString);
#if defined(Fif_GHOSTLEN)
  CtoFORTstring (tString, string, Fif_GHOSTUSE(string_len));
#else
  CtoFORTstring (tString, string, EPOCH16_STRING_LEN);
#endif
  return;
}

/******************************************************************************
* encode_EPOCH16_1 (FORTRAN equivalent of encodeEPOCH16_1).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(encode_epoch16_1__,encode_epoch16_1_,encode_epoch16_1,
	       ENCODE_EPOCH16_1)
(epoch, string Fif_GHOSTARG(string_len))
double *epoch;
void *string;
Fif_GHOSTDEF(string_len)
{
  char tString[EPOCH16_1_STRING_LEN+1];
  encodeEPOCH16_1 (epoch, tString);
#if defined(Fif_GHOSTLEN)
  CtoFORTstring (tString, string, Fif_GHOSTUSE(string_len));
#else
  CtoFORTstring (tString, string, EPOCH16_1_STRING_LEN);
#endif
  return;
}

/******************************************************************************
* encode_EPOCH16_2 (FORTRAN equivalent of encodeEPOCH16_2).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(encode_epoch16_2__,encode_epoch16_2_,encode_epoch16_2,
	       ENCODE_EPOCH16_2)
(epoch, string Fif_GHOSTARG(string_len))
double *epoch;
void *string;
Fif_GHOSTDEF(string_len)
{
  char tString[EPOCH16_2_STRING_LEN+1];
  encodeEPOCH16_2 (epoch, tString);
#if defined(Fif_GHOSTLEN)
  CtoFORTstring (tString, string, Fif_GHOSTUSE(string_len));
#else
  CtoFORTstring (tString, string, EPOCH16_2_STRING_LEN);
#endif
  return;
}

/******************************************************************************
* encode_EPOCH16_3 (FORTRAN equivalent of encodeEPOCH16_3).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(encode_epoch16_3__,encode_epoch16_3_,encode_epoch16_3,
	       ENCODE_EPOCH16_3)
(epoch, string Fif_GHOSTARG(string_len))
double *epoch;
void *string;
Fif_GHOSTDEF(string_len)
{
  char tString[EPOCH16_3_STRING_LEN+1];
  encodeEPOCH16_3 (epoch, tString);
#if defined(Fif_GHOSTLEN)
  CtoFORTstring (tString, string, Fif_GHOSTUSE(string_len));
#else
  CtoFORTstring (tString, string, EPOCH16_3_STRING_LEN);
#endif
  return;
}

/******************************************************************************
* encode_EPOCH16_4 (FORTRAN equivalent of encodeEPOCH16_4).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(encode_epoch16_4__,encode_epoch16_4_,encode_epoch16_4,
	       ENCODE_EPOCH16_4)
(epoch, string Fif_GHOSTARG(string_len))
double *epoch;
void *string;
Fif_GHOSTDEF(string_len)
{
  char tString[EPOCH16_4_STRING_LEN+1];
  encodeEPOCH16_4 (epoch, tString);
#if defined(Fif_GHOSTLEN)
  CtoFORTstring (tString, string, Fif_GHOSTUSE(string_len));
#else
  CtoFORTstring (tString, string, EPOCH16_4_STRING_LEN);
#endif
  return;
}

/******************************************************************************
* encode_EPOCH16_x (FORTRAN equivalent of encodeEPOCH16_x).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(encode_epoch16_x__,encode_epoch16_x_,encode_epoch16_x,
	       ENCODE_EPOCH16_X)
(epoch, format, string Fif_GHOSTARG(format_len) Fif_GHOSTARG(string_len))
double *epoch;
void *format;
void *string;
Fif_GHOSTDEF(format_len)
Fif_GHOSTDEF(string_len)
{
  struct STRINGstruct *ssh = NULL;
  char tString[EPOCHx_STRING_MAX+1];
#if defined(Fif_DESCR)
  encodeEPOCH16_x (epoch, DESCRtoREFnul(format,EPOCHx_FORMAT_MAX,&ssh), 
		   tString);
#endif
#if defined(Fif_GHOSTLEN)
  encodeEPOCH16_x (epoch, NULterminate(format,Fif_GHOSTUSE(format_len),&ssh),
		   tString);
#endif
#if defined(Fif_NOLEN)
  encodeEPOCH16_x (epoch, FindEndNUL(format,EPOCHx_FORMAT_MAX,&ssh), 
		   tString);
#endif
#if defined(Fif_GHOSTLEN)
  CtoFORTstring (tString, string, Fif_GHOSTUSE(string_len));
#else
  CtoFORTstring (tString, string, EPOCHx_STRING_MAX);
#endif
  return;
}

/******************************************************************************
* EPOCH16_breakdown (FORTRAN equivalent of EPOCH16breakdown).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(epoch16_breakdown__,
	       epoch16_breakdown_,
	       epoch16_breakdown,
	       EPOCH16_BREAKDOWN)
(epoch, year, month, day, hour, minute, second, msec, usec, nsec, psec)
double *epoch;
Int32 *year;
Int32 *month;
Int32 *day;
Int32 *hour;
Int32 *minute;
Int32 *second;
Int32 *msec;
Int32 *usec;
Int32 *nsec;
Int32 *psec;
{
  double tEpoch[2];
  long tYear, tMonth, tDay, tHour, tMinute, tSecond, tMsec, tUsec, tNsec, tPsec;
  tEpoch[0] = *(double *) epoch;
  tEpoch[1] = *((double *)epoch + 1);
  EPOCH16breakdown (tEpoch, &tYear, &tMonth, &tDay, &tHour, &tMinute, 
		    &tSecond, &tMsec, &tUsec, &tNsec, &tPsec);
  *year = (Int32) tYear;
  *month = (Int32) tMonth;
  *day = (Int32) tDay;
  *hour = (Int32) tHour;
  *minute = (Int32) tMinute;
  *second = (Int32) tSecond;
  *msec = (Int32) tMsec;
  *usec = (Int32) tUsec;
  *nsec = (Int32) tNsec;
  *psec = (Int32) tPsec;
  return;
}

/******************************************************************************
* compute_EPOCH16 (FORTRAN equivalent of computeEPOCH16).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(compute_epoch16__,compute_epoch16_,compute_epoch16,
	       COMPUTE_EPOCH16)
(year, month, day, hour, minute, second, msec, usec, nsec, psec, epoch)
Int32 *year;
Int32 *month;
Int32 *day;
Int32 *hour;
Int32 *minute;
Int32 *second;
Int32 *msec;
Int32 *usec;
Int32 *nsec; 
Int32 *psec;
double *epoch;
{
  double mmm;
  mmm = computeEPOCH16 ((long) *year, (long) *month, (long) *day,
			    (long) *hour, (long) *minute, (long) *second,
			    (long) *msec, (long) *usec, (long) *nsec, 
			    (long) *psec, epoch);
  return;
}
