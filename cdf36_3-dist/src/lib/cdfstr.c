/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF				      CDF character string handling.
*
*  Version 1.7a, 28-Feb-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  16-May-91, J Love	Original version (for CDF V2.1).  This is a
*				combination of nulterminate.c, isdescr.c,
*				descrtoref.c, and descrtorefnul.c.
*   V1.1  31-Jul-91, J Love	Added 'array' class to checking for a
*				descriptor (DSC$K_CLASS_A).
*   V1.2  23-Sep-91, J Love	Modified for IBM-PC port.
*   V1.3   5-Apr-92, J Love	CDF V2.2.
*   V1.4   7-Jul-92, J Love	CDF V2.3 (shareable/NeXT/zVar).
*   V1.5   1-Mar-93, J Love	CDF V2.4.
*   V1.5a  5-Feb-94, J Love	DEC Alpha/OpenVMS port.
*   V1.6  13-Dec-94, J Love	CDF V2.5.
*   V1.6a 10-Jan-95, J Love	Added `Trailer'.
*   V1.6b  6-Mar-95, J Love	Added `IsGraphChr'.  Solaris 2.3 IDL i/f.
*				`strstr' moved here (for SunOS before 4.1).
*   V1.7  22-Aug-96, J Love	CDF V2.6.
*   V1.7a 28-Feb-97, J Love	Windows NT for MS Visual C/C++ on an IBM PC.
*   V1.8  12-Aug-10, M Liu 	Nothing is performed if negative max length
*                               is passed in in strcpyX, strcatX, catchrX,
*                               PstrcatX and PstrcpyX.
*
******************************************************************************/

/******************************************************************************
*  NOTE:
*   Never assume that a string may be NUL-terminated already.  If it is,
*   it might be accidental and the NUL might go away.
******************************************************************************/

#include "cdflib.h"

#if defined(Fif_DESCR)
#if defined(vms)
#include <descrip.h>
#endif
#if defined(MPW_C)
#define MPW_FORTRAN_STRING	18
struct DESCRstruct {
  void *ptr;
  short length;
  Byte1 filler;
  Byte1 type;
};
#endif
#endif

#define IsASCII(c) (0 <= c && c <= 127)

/******************************************************************************
* NULterminate.  NUL-terminate a character string.
******************************************************************************/

STATICforIDL char *NULterminate (string, length, ssh)
char	*string;			/* String to NUL-terminate. */
size_t	length;				/* Length of string (not including
					   the NUL terminator. */
struct STRINGstruct **ssh;		/* Pointer to head of STRINGstruct
					   linked list. */
{
  struct STRINGstruct *ss;

  if (*ssh == NULL) {	/* First string. */
    *ssh = (struct STRINGstruct *) cdf_AllocateMemory ((size_t)sizeof(struct STRINGstruct),
						   NULL);
    if (*ssh == NULL) return NULL;
    ss = *ssh;
  }
  else {
    ss = *ssh;
    while (ss->next != NULL) ss = ss->next;
    ss->next = (struct STRINGstruct *)
	       cdf_AllocateMemory ((size_t)sizeof(struct STRINGstruct), NULL);
    if (ss->next == NULL) return NULL;
    ss = ss->next;
  }

  ss->next = NULL;

  ss->string = (char *) cdf_AllocateMemory ((size_t)length + 1, NULL);
  if (ss->string == NULL) return NULL;

  memmove (ss->string, string, length);
  ss->string[length] = NUL;

  return ss->string;
}

/******************************************************************************
* FreeStrings.  Free all strings (and control structures) used during the call.
******************************************************************************/

STATICforIDL void FreeStrings (ssh)
struct STRINGstruct *ssh;		/* Head of STRINGstruct chain. */
{
  struct STRINGstruct *ss, *sst;
  ss = ssh;
  while (ss != NULL) {
    cdf_FreeMemory (ss->string, NULL);
    sst = ss;
    ss = ss->next;
    cdf_FreeMemory (sst, NULL);
  }
  return;
}

#if defined(Fif_DESCR)
/******************************************************************************
* isDESCR.  Is a descriptor being pointed to?
*
* NOTE:
*     There is no checking on the length of the string because all lengths
* are valid to VMS and to CDF (for attributes with character data types).  It
* is assumed that all lengths will also be valid on the Macintosh using MPW
* Fortran.
*
******************************************************************************/

STATICforIDL Logical isDESCR (ptr, aptr, len)
void	*ptr;		/* could point to a descriptor or to the
			   string (if already passed by reference) */
char	**aptr;		/* pointer to string */
size_t	*len;		/* length of string */
{
#if defined(vms)
  struct dsc$descriptor *tPtr = (struct dsc$descriptor *) ptr;
  if (tPtr->dsc$b_dtype == DSC$K_DTYPE_T &&
      (tPtr->dsc$b_class == DSC$K_CLASS_S ||
       tPtr->dsc$b_class == DSC$K_CLASS_A ||
       tPtr->dsc$b_class == DSC$K_CLASS_NCA)) {
    *len = tPtr->dsc$w_length;
    *aptr = tPtr->dsc$a_pointer;
    return TRUE;
  }
  else
    return FALSE;
#endif
#if defined(MPW_C)
  struct DESCRstruct *tPtr = (struct DESCRstruct *) ptr;
  if (tPtr->type == MPW_FORTRAN_STRING && 0 <= tPtr->length) {
    *len = (size_t) tPtr->length;
    *aptr = (char *) tPtr->ptr;
    return TRUE;
  }
  else
    return FALSE;
#endif
}
#endif

#if defined(Fif_DESCR)
/******************************************************************************
* DESCRtoREF.  Convert from passing by descriptor to passing by reference.
******************************************************************************/

STATICforIDL char *DESCRtoREF (ptr)
void	*ptr;		/* could point to a descriptor or to the string (if
			   already passed by reference) */
{
  char *aptr;		/* pointer to string */
  size_t len;		/* length of string */

  if (isDESCR (ptr, &aptr, &len))
    return aptr;
  else
    return (char *) ptr;	 	   /* already passed by reference */
}
#endif

#if defined(Fif_DESCR)
/******************************************************************************
* DESCRtoREFnul.  Convert from passing by descriptor to passing by reference
*		  and NUL-terminate.
*  NOTE:
*   If a descriptor is being pointed to, always use a temporary string to pass
*   by reference - never assume that the string may already be NUL-terminated
*   (in case its NUL-terminated by chance and the NUL might go away).
*
******************************************************************************/

STATICforIDL char *DESCRtoREFnul (ptr, maxREFlen, ssh)
void	*ptr;				/* Could point to a descriptor or to
					   the string (if already passed by
					   reference). */
size_t	maxREFlen;			/* Maximum length of the string if
					   passed by reference. */
struct STRINGstruct **ssh;		/* Pointer to head of STRINGstruct
					   linked list. */
{
  char *aptr;		/* pointer to string */
  size_t len;		/* length of string */
  if (isDESCR (ptr, &aptr, &len))
    return NULterminate (aptr, len, ssh);
  else
    return NULterminate (ptr, maxREFlen, ssh);
}
#endif

#if defined(Fif_NOLEN)
/******************************************************************************
* FindEndNUL.  Find the end of the string and NUL-terminate.
*
*  NOTE:
*   Never assume that the string may already be NUL-terminated (in case its
*   NUL-terminated by chance and the NUL might go away).
*
******************************************************************************/

STATICforIDL char *FindEndNUL (ptr, maxREFlen, ssh)
char	*ptr;				/* Character string. */
size_t	maxREFlen;			/* Maximum length of the string. */
struct STRINGstruct **ssh;		/* Pointer to head of STRINGstruct
					   linked list. */
{
  size_t i;
  for (i = 0; i < maxREFlen; i++)
     if (!Printable(ptr[i])) return NULterminate (ptr, i, ssh);
  return NULterminate (ptr, maxREFlen, ssh);
}
#endif

/******************************************************************************
* Trailer.
*   Note that if the trailer is a null-string (length of zero), TRUE will be
* returned.
******************************************************************************/

VISIBLE_PREFIX Logical Trailer (string, trailer)
char *string;
char *trailer;
{
  size_t stringL = strlen (string);
  size_t trailerL = strlen (trailer);
  if (trailerL > stringL) return FALSE;
  if (strcmp(&string[stringL-trailerL],trailer)) return FALSE;
  return TRUE;
}

/******************************************************************************
* strcpyX.
*    Copies from the source to the destination but only up to the maximum
* number of characters specified.  Then NUL-terminates the destination
* either at the actual end of the copied source string or after the maximum
* number of characters.  If the maximum number of characters is zero, then
* a normal `strcpy' is done. If the maximum number of characters is less than
* zero, nothing is copied. 'max', if not zero, could be the destination's
* actual size, or the maximum allowed size for the destination item defined
* in CDF (when the actual size is not available in the calling function), not
* including the terminating NUL character.
******************************************************************************/

VISIBLE_PREFIX char *strcpyX (dst, src, max)
char *dst;
char *src;
size_t max;
{
  if (max > 0 && src[0] != NUL) {
    size_t j;
    j = MINIMUM(strlen(src), max);
    strncpy (dst, src, j);
    dst[(int)j] = NUL;
/*
    int i;
    for (i = 0; src[i] != NUL && i < (int) max; i++) dst[i] = src[i];
    dst[i] = NUL;
*/
  } else if (max == 0) {
    strcpy (dst, src);
  } else if (src[0] == NUL) {
    dst[0] = NUL;
  }
  return dst;
}

/******************************************************************************
* strcatX.
*    Concatenates from the source to the destination but only up to the
* maximum number of characters, 'max', specified. In other words, 'max' is
* used as a mechanism for controlling how many characters are allowed/stored
* in the destination. Then NUL-terminates the destination either at the actual
* end of the concatenated source string or after the maximum number of
* characters.  If the maximum number of characters is zero, then a normal
* `strcat' is done. If the maximum number of characters is less than zero, 
* nothing is concatenated. 'max', if not zero, could be the destination's 
* actual size, or the maximum allowed size for the destination item defined 
* in CDF (when the actual size is not available in the calling function), not
* including the terminating NUL character.
******************************************************************************/

VISIBLE_PREFIX char *strcatX (dst, src, max)
char *dst;
char *src;
size_t max;
{
  if (max > 0 && src[0] != NUL) {
    size_t i, j;
    i = strlen(dst);
    j = strlen(src);
    if (max > i) {
      size_t k = MINIMUM(max - i, j);
      if (k > 0) 
        strncat (dst, src, k);
    }
/*
    for (i = strlen(dst), j = 0; src[j] != NUL && i < (int) max; i++, j++) {
       dst[i] = src[j];

    }
    dst[i] = NUL; 
*/
  } else if (max == 0) {
    strcat (dst, src);
  }
  return dst;
}

/******************************************************************************
* catchrX.
*    Concatenates one character (the source) to the destination string but
* only up to the maximum number of characters specified.  If the maximum
* number of characters is zero, the character is always concatenated.
* If the maximum number of characters is less than zero, nothing is 
* concatenated. 
******************************************************************************/

VISIBLE_PREFIX char *catchrX (dst, src, max)
char *dst;
int src;
size_t max;
{
  size_t l = strlen(dst);
  if (max > 0) {
    if (l < max) {
      dst[l] = (char) src;
      dst[l+1] = NUL;
    }
  } else if (max == 0) {
    dst[l] = (char) src;
    dst[l+1] = NUL;
  }
  return dst;
}

/******************************************************************************
* prependX.
*    Place a string at the beginning of another string but don't let the
* destination string exceed a maximum length.
******************************************************************************/

VISIBLE_PREFIX char *prependX (dst, src, max)
char *dst;
char *src;
size_t max;
{
  size_t lenDst = strlen(dst);
  size_t maxLen = max - lenDst;
  size_t lenSrc = MINIMUM (maxLen, strlen(src));
  memmove (&dst[lenSrc], dst, lenDst + 1);
  memmove (dst, src, lenSrc);
  return dst;
}

/******************************************************************************
* PstrcpyX.
*    Copy a Pascal-style string to a C-style string.
******************************************************************************/

#if defined(mac)
STATICforIDL char *PstrcpyX (dst, src, max)
char *dst;
char *src;
size_t max;
{
  if (max < 0) return dst;
  int len = (int) BOO(max > 0,MINIMUM(max,(int)src[0]),src[0]);
  strncpy (dst, &src[1], len);
  dst[len] = NUL;
  return dst;
}
#endif

/******************************************************************************
* PstrcatX.
*    Concatinate a Pascal-style string to a C-style string.
******************************************************************************/

#if defined(mac)
STATICforIDL char *PstrcatX (dst, src, max)
char *dst;
char *src;
size_t max;
{
  if (max > 0) {
    int i, j;
    for (i = strlen(dst), j = 1; j <= src[0] && i < max; i++, j++) {
       dst[i] = src[j];
    }
    dst[i] = NUL;
  } else if (max == 0) {
     strncat (dst, &src[1], (size_t) src[0]);
  }
  return dst;
}
#endif

/******************************************************************************
* PprependX.
*    Place a Pascal-style string at the beginning of a C-style string but
* don't let the C-style string exceed a maximum length.
******************************************************************************/

#if defined(mac)
STATICforIDL char *PprependX (dst, src, max)
char *dst;
char *src;
size_t max;
{
  size_t lenDst = strlen(dst);
  size_t maxLen = max - lenDst;
  size_t lenSrc = MINIMUM (maxLen, src[0]);
  memmove (&dst[lenSrc], dst, lenDst + 1);
  memmove (dst, &src[1], lenSrc);
  return dst;
}
#endif

/******************************************************************************
* CtoFORTstring.
*   Copies a NUL-terminated string (C-style) to a FORTRAN-style string.
******************************************************************************/

STATICforIDL void CtoFORTstring (Cstring, FORTstring, length)
char *Cstring;
void *FORTstring;
int length;
{
#if defined(Fif_DESCR)
  char *ptr; size_t lenT; int i;
  if (!isDESCR(FORTstring,&ptr,&lenT)) {
    ptr = FORTstring;
    lenT = (size_t) length;
  }
/*memmove (ptr, Cstring, lenT);*/
  strncpy (ptr, Cstring, lenT);
  for (i = strlen(Cstring); i < lenT; i++) ptr[i] = ' ';
  return;
#else
  int i;
/*memmove (FORTstring, Cstring, (size_t) length);*/
  strncpy (FORTstring, Cstring, (size_t) length);
  for (i = strlen(Cstring); i < length; i++) ((char *) FORTstring)[i] = ' ';
  return;
#endif
}

/******************************************************************************
* Printable.
******************************************************************************/

VISIBLE_PREFIX int Printable (c)
int c;
{
  return BOO(IsASCII(c),isprint(c),FALSE);
}

/******************************************************************************
* UpperCase.
******************************************************************************/

VISIBLE_PREFIX int UpperCase (c)
int c;
{
  return BOO(IsASCII(c),isupper(c),FALSE);
}

/******************************************************************************
* LowerCase.
******************************************************************************/

VISIBLE_PREFIX int LowerCase (c)
int c;
{
  return BOO(IsASCII(c),islower(c),FALSE);
}

/******************************************************************************
* Alphabetic.
******************************************************************************/

VISIBLE_PREFIX int Alphabetic (c)
int c;
{
  return BOO(IsASCII(c),isalpha(c),FALSE);
}

/******************************************************************************
* Decimal.
******************************************************************************/

VISIBLE_PREFIX int Decimal (c)
int c;
{
  return BOO(IsASCII(c),isdigit(c),FALSE);
}

/******************************************************************************
* Spacing.
******************************************************************************/

VISIBLE_PREFIX int Spacing (c)
int c;
{
  return BOO(IsASCII(c),isspace(c),FALSE);
}

/******************************************************************************
* Visible.
******************************************************************************/

VISIBLE_PREFIX int Visible (c)
int c;
{
  return BOO(IsASCII(c),isgraph(c),FALSE);
}

/******************************************************************************
* MakeLower.
******************************************************************************/

VISIBLE_PREFIX int MakeLower (c)
int c;
{
  return BOO(UpperCase(c),(c | 0x20),c);
}

/******************************************************************************
* MakeUpper.
******************************************************************************/

VISIBLE_PREFIX int MakeUpper (c)
int c;
{
  return BOO(LowerCase(c),(c & 0x5F),c);
}

/******************************************************************************
* CDFstrcmpIgCase.
* It returns  1 if compared strings are identical with the same length
*             0 if compared strings are not same or there is a NULL string
******************************************************************************/

int CDFstrcmpIgCase (string1, string2)
char *string1;
char *string2;
{
  size_t string1L, string2L;
  int i;

  if (string1 == NULL || string2 == NULL) return 0;
  string1L = strlen(string1);
  string2L = strlen(string2);
  if (string1L != string2L) return 0;
  for (i = 0; i < (int) string1L; i++) {
     if (MakeLower(string1[i]) != MakeLower(string2[i]))
       return 0;
  }
  return 1;
}

/******************************************************************************
* CDFstrstrIgCase.
******************************************************************************/

char *CDFstrstrIgCase (string, substring)
char *string;
char *substring;
{
  size_t stringL, substringL;
  int i, j;
  
  if (string == NULL || substring == NULL) return NULL;
  stringL = strlen(string);
  substringL = strlen(substring);
  if (stringL == 0 || substringL == 0) return NULL;
  if (stringL < substringL) return NULL;
  for (i = 0; i < (int) stringL; i++) {
     for (j = 0; j < (int) substringL; j++) {
        if (MakeLower(substring[j]) != MakeLower(string[i+j])) break;
     }
     if (j == substringL) return &string[i];
  }
  return NULL;
}

/******************************************************************************
* `strstr' (for SunOS before 4.1).  This should no longer be needed.  If it
* is, STATICforIDL may or may not need to be present.
******************************************************************************/

#if defined(SunOSpre41)
/*STATICforIDL*/ char *strstr (string, substring)
char *string;
char *substring;
{
  int stringL = strlen(string);
  int substringL = strlen(substring);
  int i, j;
  if (stringL == 0 || substringL == 0) return NULL;
  if (stringL < substringL) return NULL;
  for (i = 0; i < stringL; i++) {
     for (j = 0; j < substringL; j++)
	if (substring[j] != string[i+j]) break;
     if (j == substringL) return &string[i];
  }
  return NULL;
}
#endif
