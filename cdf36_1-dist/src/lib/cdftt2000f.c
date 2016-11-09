/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/****************************************************************************** 
*
*  SPDF/CDF                       CDF_TIME_TT2000 utility routines for C.
*
*  Version 1.0, 15-Mar-11, ADNET systems
*
*  Modification history:
*
*   V1.0  15-Mar-11, M Liu      Initial version (for CDF V3.3.2)
******************************************************************************/

#include "cdflib.h"

/******************************************************************************
* breakdown_TT2000 (FORTRAN equivalent of CDF_TT2000_to_UTC_parts).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(breakdown_tt2000__,
               breakdown_tt2000_,
               breakdown_tt2000,
               BREAKDOWN_TT2000)
(nanoSecSinceJ2000, ly, lm, ld, lh, ln, ls, ll, lu, lx)
long long *nanoSecSinceJ2000;
int *ly;
int *lm;
int *ld;
int *lh;
int *ln;
int *ls;
int *ll;
int *lu;
int *lx;
{
  double ly1, lm1, ld1, lh1, ln1, ls1, ll1, lu1, lx1;
  CDF_TT2000_to_UTC_parts (*nanoSecSinceJ2000, &ly1, &lm1, &ld1,
                           &lh1, &ln1, &ls1, &ll1, &lu1, &lx1);
  *ly = (int) ly1;
  *lm = (int) lm1;
  *ld = (int) ld1;
  *lh = (int) lh1;
  *ln = (int) ln1;
  *ls = (int) ls1;
  *ll = (int) ll1;
  *lu = (int) lu1;
  *lx = (int) lx1;
}

/******************************************************************************
* breakdown_TT2000 (FORTRAN equivalent of CDF_TT2000_to_UTC_parts).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(tt2000_breakdown__,
               tt2000_breakdown_,
               tt2000_breakdown,
               TT2000_BREAKDOWN)
(nanoSecSinceJ2000, ly, lm, ld, lh, ln, ls, ll, lu, lx)
long long *nanoSecSinceJ2000;
int *ly;
int *lm;
int *ld;
int *lh;
int *ln;
int *ls;
int *ll;
int *lu;
int *lx;
{
  double ly1, lm1, ld1, lh1, ln1, ls1, ll1, lu1, lx1;
  CDF_TT2000_to_UTC_parts (*nanoSecSinceJ2000, &ly1, &lm1, &ld1,
                           &lh1, &ln1, &ls1, &ll1, &lu1, &lx1);
  *ly = (int) ly1;
  *lm = (int) lm1;
  *ld = (int) ld1;
  *lh = (int) lh1;
  *ln = (int) ln1;
  *ls = (int) ls1;
  *ll = (int) ll1;
  *lu = (int) lu1;
  *lx = (int) lx1;
}

/******************************************************************************
* CDF_TT2000_to_UTC_parts_f (FORTRAN equivalent of CDF_TT2000_to_UTC_parts).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_tt2000_to_utc_parts_f__,
               cdf_tt2000_to_utc_parts_f_,
               cdf_tt2000_to_utc_parts_f,
               CDF_TT2000_TO_UTC_PARTS_F)
(nanoSecSinceJ2000, ly, lm, ld, lh, ln, ls, ll, lu, lx)
long long *nanoSecSinceJ2000;
int *ly;
int *lm;
int *ld;
int *lh;
int *ln;
int *ls;
int *ll;
int *lu;
int *lx;
{
  double ly1, lm1, ld1, lh1, ln1, ls1, ll1, lu1, lx1;
  CDF_TT2000_to_UTC_parts (*nanoSecSinceJ2000, &ly1, &lm1, &ld1,
                           &lh1, &ln1, &ls1, &ll1, &lu1, &lx1);
  *ly = (int) ly1;
  *lm = (int) lm1;
  *ld = (int) ld1;
  *lh = (int) lh1;
  *ln = (int) ln1;
  *ls = (int) ls1;
  *ll = (int) ll1;
  *lu = (int) lu1;
  *lx = (int) lx1;
}

/******************************************************************************
* compute_TT2000 (FORTRAN equivalent of CDF_TT2000_from_UTC_parts).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(compute_tt2000__,
               compute_tt2000_,
               compute_tt2000,
               COMPUTE_TT2000)
(ly, lm, ld, lh, ln, ls, ll, lu, la, nanoSecSinceJ2000)
int *ly;
int *lm;
int *ld;
int *lh;
int *ln;
int *ls;
int *ll;
int *lu;
int *la;
long long *nanoSecSinceJ2000;
{
  *nanoSecSinceJ2000 = CDF_TT2000_from_UTC_parts ((double)*ly, (double)*lm,
                                                  (double)*ld, (double)*lh,
                                                  (double)*ln, (double)*ls,
                                                  (double)*ll, (double)*lu,
                                                  (double)*la);
}

/******************************************************************************
* CDF_TT2000_from_UTC_parts_f (FORTRAN equivalent of
* CDF_TT2000_from_UTC_parts).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_tt2000_from_utc_parts_f__,
               cdf_tt2000_from_utc_parts_f_,
               cdf_tt2000_from_utc_parts_f,
               CDF_TT2000_FROM_UTC_PARTS_F)
(ly, lm, ld, lh, ln, ls, ll, lu, la, nanoSecSinceJ2000)
int *ly;
int *lm;
int *ld;
int *lh;
int *ln;
int *ls;
int *ll;
int *lu;
int *la;
long long *nanoSecSinceJ2000;
{
  *nanoSecSinceJ2000 = CDF_TT2000_from_UTC_parts ((double)*ly, (double)*lm, 
                                                  (double)*ld, (double)*lh,
                                                  (double)*ln, (double)*ls,
                                                  (double)*ll, (double)*lu,
                                                  (double)*la);
}

/******************************************************************************
* TT2000_to_EPOCH (FORTRAN equivalent of CDF_TT2000_to_UTC_EPOCH).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(tt2000_to_epoch__,
               tt2000_to_epoch_,
               tt2000_to_epoch,
               TT2000_TO_EPOCH)
(nanoSecSinceJ2000, epoch)
long long *nanoSecSinceJ2000;
double *epoch;
{
  *epoch = CDF_TT2000_to_UTC_EPOCH (*nanoSecSinceJ2000);
}

/******************************************************************************
* CDF_TT2000_to_UTC_EPOCH_f (FORTRAN equivalent of CDF_TT2000_to_UTC_EPOCH).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_tt2000_to_utc_epoch_f__,
               cdf_tt2000_to_utc_epoch_f_,
               cdf_tt2000_to_utc_epoch_f,
               CDF_TT2000_TO_UTC_EPOCH_F)
(nanoSecSinceJ2000, epoch)
long long *nanoSecSinceJ2000;
double *epoch;
{
  *epoch = CDF_TT2000_to_UTC_EPOCH (*nanoSecSinceJ2000);
}

/******************************************************************************
* TT2000_from_EPOCH (FORTRAN equivalent of CDF_TT2000_from_UTC_EPOCH).
******************************************************************************/

VISIBLE_PREFIX 
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(tt2000_from_epoch__,
               tt2000_from_epoch_,
               tt2000_from_epoch,
               TT2000_FROM_EPOCH)
(epoch, nanoSecSinceJ2000)
double *epoch;
long long *nanoSecSinceJ2000;
{
  *nanoSecSinceJ2000 = CDF_TT2000_from_UTC_EPOCH (*epoch);
}

/******************************************************************************
* CDF_TT2000_from_UTC_EPOCH_f (FORTRAN equivalent of CDF_TT2000_from_UTC_EPOCH).
******************************************************************************/

VISIBLE_PREFIX 
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_tt2000_from_utc_epoch_f__,
               cdf_tt2000_from_utc_epoch_f_,
               cdf_tt2000_from_utc_epoch_f,
               CDF_TT2000_FROM_UTC_EPOCH_F)
(epoch, nanoSecSinceJ2000)
double *epoch;
long long *nanoSecSinceJ2000;
{
  *nanoSecSinceJ2000 = CDF_TT2000_from_UTC_EPOCH (*epoch);
}

/******************************************************************************
* TT2000_to_EPOCH16 (FORTRAN equivalent of CDF_TT2000_to_UTC_EPOCH16).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(tt2000_to_epoch16__,
               tt2000_to_epoch16_,
               tt2000_to_epoch16,
               TT2000_TO_EPOCH16)
(nanoSecSinceJ2000, epoch16)
long long *nanoSecSinceJ2000;
double *epoch16;
{
  CDF_TT2000_to_UTC_EPOCH16 (*nanoSecSinceJ2000, epoch16);
}

/******************************************************************************
* CDF_TT2000_to_UTC_EPOCH16_f (FORTRAN equivalent of
* CDF_TT2000_to_UTC_EPOCH16).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_tt2000_to_utc_epoch16_f__,
               cdf_tt2000_to_utc_epoch16_f_,
               cdf_tt2000_to_utc_epoch16_f,
               CDF_TT2000_TO_UTC_EPOCH16_F)
(nanoSecSinceJ2000, epoch16)
long long *nanoSecSinceJ2000;
double *epoch16;
{
  CDF_TT2000_to_UTC_EPOCH16 (*nanoSecSinceJ2000, epoch16);
}

/******************************************************************************
* TT2000_from_EPOCH16 (FORTRAN equivalent of CDF_TT2000_from_UTC_EPOCH16).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(tt2000_from_epoch16__,
               tt2000_from_epoch16_,
               tt2000_from_epoch16,
               TT2000_FROM_EPOCH16)
(epoch16, nanoSecSinceJ2000) 
double *epoch16;
long long *nanoSecSinceJ2000;
{
  *nanoSecSinceJ2000 = CDF_TT2000_from_UTC_EPOCH16 (epoch16);
}

/******************************************************************************
* CDF_TT2000_from_UTC_EPOCH16_f (FORTRAN equivalent of
* CDF_TT2000_from_UTC_EPOCH16).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_tt2000_from_utc_epoch16_f__,
               cdf_tt2000_from_utc_epoch16_f_,
               cdf_tt2000_from_utc_epoch16_f,
               CDF_TT2000_FROM_UTC_EPOCH16_F)
(epoch16, nanoSecSinceJ2000) 
double *epoch16;
long long *nanoSecSinceJ2000;
{
  *nanoSecSinceJ2000 = CDF_TT2000_from_UTC_EPOCH16 (epoch16);
}


/******************************************************************************
* encode_TT2000 (FORTRAN equivalent of CDF_TT2000_to_UTC_string).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(encode_tt2000__,
               encode_tt2000_,
               encode_tt2000,
               ENCODE_TT2000)
(nanoSecSinceJ2000, format, string Fif_GHOSTARG(string_len))
long long *nanoSecSinceJ2000;
int *format;
void *string;
Fif_GHOSTDEF(string_len)
{
  char tString[TT2000_0_STRING_LEN+1];
  int len = -1;
  if (*format == 3) len = TT2000_3_STRING_LEN;
  else if (*format == 0) len = TT2000_0_STRING_LEN;
  else if (*format == 1) len = TT2000_1_STRING_LEN;
  else if (*format == 2) len = TT2000_2_STRING_LEN;
  if (len != -1) { 
    CDF_TT2000_to_UTC_string (*nanoSecSinceJ2000, tString, *format);
#if defined(Fif_GHOSTLEN)
    CtoFORTstring (tString, string, Fif_GHOSTUSE(string_len));
#else
    CtoFORTstring (tString, string, len);
#endif
  } else {
#if defined(Fif_GHOSTLEN)
    CtoFORTstring (" ", string, Fif_GHOSTUSE(string_len));
#else
    CtoFORTstring (" ", string, 1);
#endif
  }
}

/******************************************************************************
* CDF_TT2000_to_UTC_string_f (FORTRAN equivalent of CDF_TT2000_to_UTC_string).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_tt2000_to_utc_string_f__,
               cdf_tt2000_to_utc_string_f_,
               cdf_tt2000_to_utc_string_f,
               CDF_TT2000_TO_UTC_STRING_F)
(nanoSecSinceJ2000, format, string Fif_GHOSTARG(string_len))
long long *nanoSecSinceJ2000;
int *format;
void *string;
Fif_GHOSTDEF(string_len)
{
  char tString[TT2000_0_STRING_LEN+1];
  int len = -1;
  if (*format == 3) len = TT2000_3_STRING_LEN;
  else if (*format == 0) len = TT2000_0_STRING_LEN;
  else if (*format == 1) len = TT2000_1_STRING_LEN;
  else if (*format == 2) len = TT2000_2_STRING_LEN;
  if (len != -1) { 
    CDF_TT2000_to_UTC_string (*nanoSecSinceJ2000, tString, *format);
#if defined(Fif_GHOSTLEN)
    CtoFORTstring (tString, string, Fif_GHOSTUSE(string_len));
#else
    CtoFORTstring (tString, string, len);
#endif
  } else {
#if defined(Fif_GHOSTLEN)
    CtoFORTstring (" ", string, Fif_GHOSTUSE(string_len));
#else
    CtoFORTstring (" ", string, 1);
#endif
  }
}

/******************************************************************************
* parse_TT2000 (FORTRAN equivalent of CDF_TT2000_from_UTC_string).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(parse_tt2000__,
               parse_tt2000_,
               parse_tt2000,
               PARSE_TT2000)
(string, tt2000 Fif_GHOSTARG(string_len))
char *string;
long long *tt2000;
Fif_GHOSTDEF(string_len)
{
  struct STRINGstruct *ssh = NULL;
  size_t len = strlen(string)+1;
#if defined(Fif_DESCR)
  *tt2000 = CDF_TT2000_from_UTC_string (DESCRtoREFnul(string,len,&ssh));
#endif
#if defined(Fif_GHOSTLEN)
  *tt2000 = CDF_TT2000_from_UTC_string (NULterminate(string,Fif_GHOSTUSE(string_len),&ssh));
#endif
#if defined(Fif_NOLEN)
  *tt2000 = CDF_TT2000_from_UTC_string (FindEndNUL(string,len,&ssh));
#endif
  FreeStrings (ssh);
}

/******************************************************************************
* CDF_TT2000_from_UTC_string_f (FORTRAN equivalent of
* CDF_TT2000_from_UTC_string).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_tt2000_from_utc_string_f__,
               cdf_tt2000_from_utc_string_f_,
               cdf_tt2000_from_utc_string_f,
               CDF_TT2000_FROM_UTC_STRING_F)
(string, tt2000 Fif_GHOSTARG(string_len))
char *string;
long long *tt2000;
Fif_GHOSTDEF(string_len)
{
  struct STRINGstruct *ssh = NULL;
  size_t len = strlen(string)+1;
#if defined(Fif_DESCR)
  *tt2000 = CDF_TT2000_from_UTC_string (DESCRtoREFnul(string,len,&ssh));
#endif
#if defined(Fif_GHOSTLEN)
  *tt2000 = CDF_TT2000_from_UTC_string (NULterminate(string,Fif_GHOSTUSE(string_len),&ssh));
#endif
#if defined(Fif_NOLEN)
  *tt2000 = CDF_TT2000_from_UTC_string (FindEndNUL(string,len,&ssh));
#endif
  FreeStrings (ssh);
}

/******************************************************************************
* CDF_getLeapSecondsTableEnvVar (FORTRAN equivalent of
* CDFgetLeapSecondsTableEnvVar).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_getleapsecondstableenvvar__,
               cdf_getleapsecondstableenvvar_,
               cdf_getleapsecondstableenvvar,
               CDF_GETLEAPSECONDSTABLEENVVAR)
(string Fif_GHOSTARG(string_len))
char *string;
Fif_GHOSTDEF(string_len)
{
  char *table = CDFgetLeapSecondsTableEnvVar();
  if (table != NULL) {
#if defined(Fif_GHOSTLEN)
    CtoFORTstring (table, string, Fif_GHOSTUSE(string_len));
#else
    CtoFORTstring (table, string, CDF_PATHNAME_LEN);
#endif
  } else
    string[0] = (char) '\0';
}

/******************************************************************************
* CDF_getLastDateinLeapSecondsTable (FORTRAN equivalent of
* CDFgetLastDateinLeapSecondsTable).
******************************************************************************/
#if defined(vms)
VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_getlastdateinleapsectable__,
               cdf_getlastdateinleapsectable_,
               cdf_getlastdateinleapsectable,
               CDF_GETLASTDATEINLEAPSECTABLE)
#else
VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_getlastdateinleapsecondstable__,
               cdf_getlastdateinleapsecondstable_,
               cdf_getlastdateinleapsecondstable,
               CDF_GETLASTDATEINLEAPSECONDSTABLE)
#endif
(year, month, day)
Int32 *year;
Int32 *month;
Int32 *day;
{
  long yy, mm, dd;
#if defined(vms)
  CDFgetLastDateinLeapSecondsTBL (&yy, &mm, &dd);
#else
  CDFgetLastDateinLeapSecondsTable (&yy, &mm, &dd);
#endif
  *year = (Int32) yy;
  *month = (Int32) mm;
  *day = (Int32) dd; 
}

/******************************************************************************
* CDF_getLeapSecondsTableStatus (FORTRAN equivalent of
* CDFgetLeapSecondsTableStatus).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_getleapsecondstablestatus__,
               cdf_getleapsecondstablestatus_,
               cdf_getleapsecondstablestatus,
               CDF_GETLEAPSECONDSTABLESTATUS)
()
{
  return (Int32) CDFgetLeapSecondsTableStatus ();
}

/******************************************************************************
* CDF_getRowsinLeapSecondsTable (FORTRAN equivalent of
* CDFgetRowsinLeapSecondsTable).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_getrowsinleapsecondstable__,
               cdf_getrowsinleapsecondstable_,
               cdf_getrowsinleapsecondstable,
               CDF_GETROWSINLEAPSECONDSTABLE)
()
{
  return (Int32) CDFgetRowsinLeapSecondsTable ();
}

/******************************************************************************
* CDF_fillLeapSecondsTable (FORTRAN equivalent of
* CDFfillLeapSecondsTable).
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_fillleapsecondstable__,
               cdf_fillleapsecondstable_,
               cdf_fillleapsecondstable,
               CDF_FILLLEAPSECONDSTABLE)
(tableo)
double *tableo;
{
  int ix, iy;
  int rows = CDFgetRowsinLeapSecondsTable ();
  double **table;
  table = (double **) cdf_AllocateMemory (sizeof (double *) * rows, NULL);
  for (ix = 0; ix < rows; ++ix)
    table[ix] = cdf_AllocateMemory (sizeof (double) * 6, NULL);
  CDFfillLeapSecondsTable (table);
  for (ix = 0; ix < rows; ++ix) 
    for (iy = 0; iy < 6; ++iy)
      *(((double *)tableo)+6*ix+iy) = table[ix][iy];
  for (ix = 0; ix < rows; ++ix) cdf_FreeMemory (table[ix], NULL);
  cdf_FreeMemory (table, NULL);
}


