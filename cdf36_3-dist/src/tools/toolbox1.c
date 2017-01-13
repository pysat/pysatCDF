/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                   Toolbox of routines for CDF Toolkit (General).
*
*  Version 1.5d, 23-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  25-Jan-94, J Love     Original version.
*   V1.0a  6-Feb-94, J Love     DEC Alpha/OpenVMS port.
*   V1.1  10-Mar-94, J Love     Modified `ParseOptionList' to make the
*                               enclosing parentheses optional (on any
*                               operating system).
*   V1.2  15-Dec-94, J Love     CDF V2.5.
*   V1.2a 13-Jan-95, J Love     Allow mixed-case in tokens.  Changed
*                               `CDFdirList' to allow all possible extensions
*                               on Macintosh machines also.
*   V1.2b  6-Mar-95, J Love     Added macro definition checking to catch
*                               bug in directory listing system calls.  Pass
*                               `char' as `int'.
*   V1.3  21-Mar-95, J Love     POSIX.
*   V1.3a 18-Apr-95, J Love     More POSIX.
*   V1.3b 24-Apr-95, J Love     Fixed `EncodeString' to not overflow the
*                               maximum length if the string won't fit.
*   V1.4  25-May-95, J Love     Added `GetCdataType'.  EPOCH styles.
*                               SOLARISbsdDIRUTILSbug.
*   V1.4a 12-Jun-95, J Love     EPOCH custom format.  catchrX.
*   V1.4b  5-Sep-95, J Love     Allow NULL for output fields not needed.
*                               NUL-terminate decoded string in `DecodeValues'.
*   V1.4c 19-Sep-95, J Love     Macintosh event handling.
*   V1.4d 27-Sep-95, J Love     Reduced TextEdit text size for Think C.
*   V1.5  27-Aug-96, J Love     CDF V2.6.
*   V1.5a  2-Nov-96, J Love	Changed `DisplayIdentification' to not print
*				a `blank' sub-increment.
*   V1.5b 21-Feb-97, J Love	Removed RICE.
*   V1.5c  3-Mar-97, J Love	Windows NT for MS Visual C/C++ on an IBM PC.
*   V1.5d 23-Nov-97, J Love	More Windows NT.
*   V1.6  10-May-01, M Liu      New port for Cygwin
*   V1.7  11-Jul-05, M Liu      Added MingW port for PC.
*   V1.8  13-Oct-06, M Liu      Changed to allow upper and lower case CDF  
*                               name to be used on win32.
*   V1.9  23-Feb-14, M Liu      Handled nan and inf floating values.  
*   V1.10  3-Apr-14, M Liu      Added MergeSort and BinarySearch functions.
*
******************************************************************************/

#define TOOLBOX1
#include "cdftools.h"

#if defined(win32)
#include <Windows.h>
#include <Winbase.h>
#endif

/******************************************************************************
* Local macros.
******************************************************************************/

#define MAX_cFORMAT_LEN 10

/******************************************************************************
* Local enumerators.
******************************************************************************/

#if defined(unix) || defined(mac) || defined(posixSHELL)
enum typeENUM {TBD, STR, WILD1, WILDn};
#endif

/******************************************************************************
* Local structures.
******************************************************************************/

struct FILEstruct {
  char *name;
  struct FILEstruct *next;
};

/******************************************************************************
* Local function prototypes.
******************************************************************************/

static Logical FtoCformat PROTOARGs((long, char *, long *, char *, size_t));
static int UseCformat PROTOARGs((long, long, char *, void *, char *, size_t));
static long GetCdataType PROTOARGs((char *, long *));
static void AddToList PROTOARGs((struct FILEstruct **FILEhead,
				 struct FILEstruct *FILEptr));
static void Merge PROTOARGs((long, void *, void *, int, int, int ));

#if defined(unix) || defined(mac) || defined(posixSHELL)
static int PatternMatch PROTOARGs((char *pattern, char *name));
static int CheckPattern PROTOARGs((int nTypes, enum typeENUM types[],
				   char *strings[], char *name));
#endif
#if defined(win32)
static void RemoveLeading0 PROTOARGs((char *text, size_t width));
#endif
/******************************************************************************
* FatalError.
******************************************************************************/

void FatalError (message)
char *message;
{
#if defined(mac)
  MacMessageDialog ("FATAL ERROR!", message);
#else
#if defined(win32) && !defined(ALONE)
  WinMessageDialog ("FATAL ERROR!", message);
#else
  WriteOut (stdout, "FATAL ERROR!  ");
  WriteOut (stdout, message);
  WriteOut (stdout, "\n");
#endif
#endif
  ExitBAD;
}

/******************************************************************************
* DisplayError.
******************************************************************************/

void DisplayError (message)
char *message;
{
#if defined(mac)
  MacMessageDialog ("Error!", message);
#else
#if defined(win32) && !defined(ALONE)
  WinMessageDialog ("Error!", message);
#else
  WriteOut (stdout, "Error!  ");
  WriteOut (stdout, message);
  WriteOut (stdout, "\n");
#endif
#endif
  return;
}

/******************************************************************************
* DisplayWarning.
*    It is assumed that the message contains at least one character.
******************************************************************************/

void DisplayWarning (message)
char *message;
{
#if defined(mac)
  MacMessageDialog ("Warning...", message);
#else
  WriteOut (stdout, "Warning: ");
  WriteOut (stdout, message);
  WriteOut (stdout, "\n");
#endif
  return;
}

/******************************************************************************
* DisplayInfo.
******************************************************************************/

void DisplayInfo (message)
char *message;
{
#if defined(mac)
  MacMessageDialog ("", message);
#else
  WriteOut (stdout, message);
  WriteOut (stdout, "\n");
#endif
  return;
}

/******************************************************************************
* DecodeVariances.
******************************************************************************/

Logical DecodeVariances (mask, numDims, recVary, dimVarys)
char *mask;
long numDims;
long *recVary;
long dimVarys[];
{
  int dimN, charN;

  switch (mask[0]) {
    case 'T':
    case 't':
      *recVary = VARY;
      break;
    case 'F':
    case 'f':
      *recVary = NOVARY;
      break;
    default:
      return FALSE;
  }

  for (dimN = 0, charN = 2; dimN < numDims; dimN++, charN++) {
     switch (mask[charN]) {
       case 'T':
       case 't':
	 dimVarys[dimN] = VARY;
	 break;
       case 'F':
       case 'f':
	 dimVarys[dimN] = NOVARY;
	 break;
       default:
	 return FALSE;
   }
  }

  return TRUE;
}

/******************************************************************************
* DecodeDelimitedString.
******************************************************************************/

Logical DecodeDelimitedString (dString, pString)
char *dString;          /* Delimited string. */
char *pString;          /* Parsed (decoded) string. */
{
  char *d1 = dString;           /* First delimiter. */
  char *d2;                     /* Second delimiter. */
  size_t len;                   /* Size of decoded string. */

  while (*d1 != NUL && Spacing(*d1)) d1++;
  if (*d1 == NUL) return FALSE;

  d2 = d1 + 1;
  while (*d2 != NUL && *d2 != *d1) d2++;
  if (*d2 == NUL) return FALSE;

  len = (size_t) (d2 - d1 - 1);
  memmove (pString, d1 + 1, len);
  pString[len] = NUL;

  return TRUE;
}

/******************************************************************************
* DecodeDimensionality.
******************************************************************************/

Logical DecodeDimensionality (string, numDims, dimSizes)
char *string;
long *numDims;
long *dimSizes;
{
  char *cp;
  /****************************************************************************
  * The number of dimensions should be first.
  ****************************************************************************/
  cp = NextNonSpace (string);
  if (sscanf(cp,"%ld",numDims) != 1) return FALSE;
  if (*numDims < 0) return FALSE;
  cp = NextNonDigit (cp);
  /****************************************************************************
  * Skip past the `:' and `['.
  ****************************************************************************/
  cp = NextNonSpace (cp);
  if (*cp != ':') return FALSE;
  cp = NextNonSpace (cp+1);
  if (*cp != '[') return FALSE;
  cp = NextNonSpace (cp+1);
  /****************************************************************************
  * Decode each dimension size.
  ****************************************************************************/
  if (*numDims > 0) {
    int dimN, lastDim = (int) (*numDims - 1);
    for (dimN = 0; dimN <= lastDim; dimN++) {
       if (sscanf(cp,"%ld",&dimSizes[dimN]) != 1) return FALSE;
       cp = NextNonDigit (cp);
       cp = NextNonSpace (cp);
       if (dimN != lastDim) {
	 if (*cp != ',') return FALSE;
	 cp = NextNonSpace (cp+1);
       }
    }
  }
  /****************************************************************************
  * Check for a trailing `]'.
  ****************************************************************************/
  if (*cp != ']') return FALSE;
  /****************************************************************************
  * Everything went Ok, return TRUE;
  ****************************************************************************/
  return TRUE;
}

/******************************************************************************
* DecodeRecordAndIndices.
******************************************************************************/

Logical DecodeRecordAndIndices (string, recN, nIndices, indices)
char *string;
long *recN;                     /* Can be NULL if not needed. */
long *nIndices;
long indices[CDF_MAX_DIMS];
{
  char *cp; long recNt;
  /****************************************************************************
  * The record number will be first if it exists;
  ****************************************************************************/
  cp = NextNonSpace (string);
  if (*cp == '[') {
    recNt = 0;
  }
  else {
    if (sscanf(cp,"%ld",&recNt) != 1) return FALSE;
    recNt -= 1;
    if (recNt < 0) return FALSE;
    cp = NextNonDigit (cp);
    cp = NextNonSpace (cp);
    if (*cp != ':') return FALSE;
    cp = NextNonSpace (cp+1);
    if (*cp != '[') return FALSE;
  }
  ASSIGNnotNULL (recN, recNt)
  cp = NextNonSpace (cp+1);
  /****************************************************************************
  * Decode each index while counting how many there are.
  ****************************************************************************/
  *nIndices = 0;
  if (*cp == ']') return TRUE;
  for (;;) {
     if (*nIndices == CDF_MAX_DIMS) return FALSE;       /* Too many indices. */
     if (sscanf(cp,"%ld",&indices[(int)(*nIndices)]) != 1) return FALSE;
     indices[(int)(*nIndices)] -= 1;
     if (indices[(int)(*nIndices)] < 0) return FALSE;
     *nIndices += 1;
     cp = NextNonDigit (cp);    /* Skip to space, comma, or `]'. */
     cp = NextNonSpace (cp);    /* Skip past spaces. */
     switch (*cp) {
       case ']':
	 return TRUE;
       case ',':
	 cp = NextNonSpace (cp + 1);
	 break;
       default:
	 return FALSE;
     }
  }
}

/******************************************************************************
* NextNonSpace.
******************************************************************************/

char *NextNonSpace (cp)
char *cp;
{
  while (Spacing(*cp)) cp++;
  return cp;
}

/******************************************************************************
* NextNonDigit.
******************************************************************************/

char *NextNonDigit (cp)
char *cp;
{
  while (Decimal(*cp)) cp++;
  return cp;
}

/******************************************************************************
* FormatWidth.
*    Returns width of a format specifier [or zero if the width is unknown or
* illegal].
******************************************************************************/

int FormatWidth (format)
char *format;
{
  int width; char *ptr = format;
  /****************************************************************************
  * Skip past Fortran repeat count (eg. the `20' in `20I8' or `20(I8)').
  * Note that this won't skip past a C `%'.
  ****************************************************************************/
  while (*ptr != NUL && (*ptr == '(' ||
			 Spacing(*ptr) ||
			 Decimal(*ptr))) ptr++;
  if (*ptr == NUL) return 0;
  /****************************************************************************
  * Skip past Fortran format type (eg. the `F' in `F4.1') or C `%' and/or
  * flags (one of which might be a `0').
  ****************************************************************************/
  while (*ptr != NUL && (!Decimal(*ptr) || *ptr == '0')) ptr++;
  if (*ptr == NUL) return 0;
  /****************************************************************************
  * Decode format width.
  ****************************************************************************/
  if (sscanf(ptr,"%d",&width) != 1) width = 0;
  return width;
}

/******************************************************************************
* FormatPrecision.
*   Returns precision of a format specifier [or zero if the precision doesn't
* exist [eg. integer format]).
******************************************************************************/

int FormatPrecision (format)
char *format;
{
  int precision;
  char *ptr = strchr (format, '.');
  if (ptr == NULL)
    precision = 0;
  else
    if (sscanf(ptr+1,"%d",&precision) != 1) precision = 0;
  return precision;
}

/******************************************************************************
* EncodeValuesFormat.  Returns width of encoded values.
******************************************************************************/

int EncodeValuesFormat (dataType, numElems, binary, text, format, minWidth,
			maxWidth, style, width)
long dataType;          /* Data type of values. */
long numElems;          /* Number of elements (values). */
void *binary;           /* In: Values in binary. */
char *text;             /* Out: Encoded values. */
char *format;           /* If NULL, use default formats.  Not applicable if
			   a string data type. */
int minWidth;           /* Minimum width (including delimeters if a string
			   data type.  If zero, no minimum width (no blank
			   padding).  If positive, right justified (padding
			   on left side).  If negative, left justified
			   (padding on right side). */
int maxWidth;           /* Maximum width (including delimeters if a string
			   data type.  If zero, no maximum width (no
			   truncating). */
int style;              /* EPOCH style. */
size_t width;
{
  /****************************************************************************
  * Encode value(s).
  ****************************************************************************/
  if (STRINGdataType(dataType))
    EncodeString (numElems, binary, text, minWidth, maxWidth);
  else {
    long elemN;
    char tText[MAX_nonSTRING_VALUE_LEN+1];
    strcpyX (text, "", maxWidth);
    for (elemN = 0; elemN < numElems; elemN++) {
       /***********************************************************************
       * Encode each value (if it fits).
       ***********************************************************************/
       EncodeValueFormat (dataType, (Byte1 *) binary +
				    (size_t) (elemN*CDFelemSize(dataType)),
			  tText, format, 0, 0, style, (size_t) sizeof(tText));
       if (maxWidth > 0) {
	 int n = strlen(tText);
	 if (strlen(text) + (elemN > 0 ? 2 : 0) + n > (size_t) maxWidth) {
	   /*******************************************************************
	   * This value won't fit.  Find a comma after which to put the
	   * truncation symbol (`...').  `i' starts at the NUL just in case
	   * this is the first value (`text' is a null-string).
	   *******************************************************************/
	   int i;
	   for (i = strlen(text); i != 0; i--) {
	      if (text[i] == ',' &&
		  (i + strlen(" ...") < (size_t) maxWidth)) break;
	   }
	   if (i != 0)
	     strcpyX (&text[i+1], " ...", maxWidth);
	   else {
	     switch (elemN) {
	       case 0:
		 CpyNcharacters (text, maxWidth, (int) '*');
		 break;
	       case 1:
		 if (strlen(text) + strlen(", ...") <= (size_t) maxWidth)
		   strcatX (text, ", ...", maxWidth);
		 else {
		   CpyNcharacters (text, maxWidth, (int) '*');
		 }
		 break;
	       default:
		 CpyNcharacters (text, maxWidth, (int) '*');
		 break;
	     }
	   }
	   break;
	 }
	 else
	   snprintf (&text[strlen(text)], 
	             (size_t) maxWidth+1-strlen(text),
	             "%s%s", (elemN > 0 ? ", " : ""), tText);
       }
       else
	 snprintf (&text[strlen(text)], width-strlen(text), "%s%s",
		   (elemN > 0 ? ", " : ""), tText);
    }
    /**************************************************************************
    * Blank pad to `minWidth' if necessary.
    **************************************************************************/
    if (minWidth != 0) {
      int n = (int) strlen(text);
      if (minWidth > 0) {               /* Right justify. */
	if (n < minWidth) {
	  int i, pad = minWidth - n;
	  memmove (&text[pad], text, n + 1);
	  for (i = 0; i < pad; i++) text[i] = ' ';
	}
      }
      else {                    /* Left justify. */
	int min = -minWidth;
	if (n < min) {
	  int pad = min - n;
	  CatNcharacters (text, pad, (int) ' ');
	}
      }
    }
  }
  /****************************************************************************
  * Return length of encoded value(s).
  ****************************************************************************/
  return strlen(text);
}

/******************************************************************************
* EncodeValueFormat.
*    Returns width of encoded value.  Not for use with character string data
* types (CDF_CHAR or CDF_UCHAR).
******************************************************************************/

int EncodeValueFormat (dataType, binary, text, format, minWidth, maxWidth,
		       style, width)
long dataType;          /* Data type of value being encoded. */
void *binary;           /* In: Value in binary. */
char *text;             /* Out: Encoded value. */
char *format;           /* Format string to be used. */
int minWidth;           /* Minimum width (including delimeters if a string
			   data type.  If zero, no minimum width (no blank
			   padding).  If positive, right justified (padding
			   on left side).  If negative, left justified
			   (padding on right side). */
int maxWidth;           /* Maximum width.  If zero, no maximum width (no
			   asteriks if too big). */
int style;              /* EPOCH style. */
size_t width;		/* Size of encoded text. */
{
  long CdataType;
  char Cformat[MAX_cFORMAT_LEN+1];
  char tText[MAX_nonSTRING_VALUE_LEN+1];
  /****************************************************************************
  * Encode value.
  ****************************************************************************/
  if (EPOCHdataType(dataType)) {
    switch (style) {
      case EPOCHf_STYLE:
	return EncodeValueFormat(CDF_DOUBLE,binary,text,
				 format,minWidth,maxWidth,0,width);
      case EPOCHx_STYLE:
	encodeEPOCHx (*((double *) binary), format, tText);
	break;
      default:
	EncodeValue (CDF_EPOCH, binary, tText, style, width);
	break;
    }
  } else if (EPOCH16dataType(dataType)) {
    switch (style) {
      case EPOCHf_STYLE:
        return EncodeValueFormat(CDF_DOUBLE,binary,text,
                                 format,minWidth,maxWidth,0,width);
      case EPOCHx_STYLE:
        encodeEPOCHx (*((double *) binary), format, tText);
        break;
      default:
        EncodeValue (CDF_EPOCH16, binary, tText, style, width);
        break;
    }
  } else if (TT2000dataType(dataType)) {
        if (style > 3) style = 3;
        EncodeValue (dataType, binary, tText, style, width);
  }
  else
    if (format == NULL)
      EncodeValue (dataType, binary, tText, 0, width);
    else
      if (strchr(format,'%') != NULL)
	if (GetCdataType(format,&CdataType))
	  UseCformat (dataType, CdataType, format, binary, tText, width);
	else
	  EncodeValue (dataType, binary, tText, 0, width);
      else
	if (FtoCformat(dataType,format,&CdataType,Cformat,
		       (size_t) sizeof(Cformat)))
	  UseCformat (dataType, CdataType, Cformat, binary, tText, width);
	else
	  EncodeValue (dataType, binary, tText, 0, width);
  /****************************************************************************
  * Fill in asteriks if encoded value is too big.  Otherwise, copy encoded
  * value.
  ****************************************************************************/
  if (maxWidth > 0) {
    int n = (int) strlen(tText);
    if (n > maxWidth)
      CpyNcharacters (text, maxWidth, (int) '*');
    else
      strcpyX (text, tText, maxWidth);
  }
  else
    strcpyX (text, tText, maxWidth);
  /****************************************************************************
  * Blank pad to `minWidth' if necessary.
  ****************************************************************************/
  if (minWidth != 0) {
    int n = (int) strlen(text);
    if (minWidth > 0) {         /* Right justify. */
      if (n < minWidth) {
	int i, pad = minWidth - n;
	memmove (&text[pad], text, n + 1);
	for (i = 0; i < pad; i++) text[i] = ' ';
      }
    }
    else {                      /* Left justify. */
      int min = -minWidth;
      if (n < min) {
	int pad = min - n;
	CatNcharacters (text, pad, (int) ' ');
      }
    }
  }
  /****************************************************************************
  * Return length of encoded value.
  ****************************************************************************/
  return strlen(text);
}

/******************************************************************************
* EncodeValue.
* Returns width of encoded value.
******************************************************************************/

int EncodeValue (dataType, binary, text, style, width)
long dataType;          /* CDF data type. */
void *binary;           /* In: Value to encode. */
char *text;             /* Out: Encoded value. */
int style;              /* In: EPOCH style. */
size_t width;		/* In: size of encoded text. */
{
  MakeNUL (text);
  switch (dataType) {
    case CDF_BYTE:
    case CDF_INT1:
      snprintf (text, width, "%d", (int) *((sChar *) binary));
      break;
    case CDF_INT2:
      snprintf (text, width, "%d", (int) *((Int16 *) binary));
      break;
    case CDF_INT4:
      snprintf (text, width, Int32FORMAT, *((Int32 *) binary));
      break;
    case CDF_INT8:
      snprintf (text, width, Int64FORMAT, (long long) *((Int64 *) binary));
      break;
    case CDF_UINT1:
      snprintf (text, width, "%u", (uInt4) *((uChar *) binary));
      break;
    case CDF_UINT2:
      snprintf (text, width, "%u", (uInt4) *((uInt16 *) binary));
      break;
    case CDF_UINT4:
      snprintf (text, width, Int32uFORMAT, *((uInt32 *) binary));
      break;
    case CDF_REAL4:
    case CDF_FLOAT: {
      if (NegativeZeroReal4((float *) binary))
	strcpyX (text, NEGATIVEzeroSYMBOL, 0);
      else {
	size_t len;
	char *ePtr, tText[80+1];
#if defined(win32) || defined(vms)
        if (isnan((double)*((float *) binary)))
	  snprintf (text, width, "%s", "nan");
        else if (isinf((double)*((float *) binary)))
	  snprintf (text, width, "%s%s",
                    (*((float *) binary)>0.0f?"":"-"),"inf");
        else
	  snprintf (text, width, "%g", *((float *) binary));
#else
        if (isinf((double)*((float *) binary)))
	  snprintf (text, width, "%s%s",
                    (*((float *) binary)>0.0f?"":"-"), "inf");
        else
	  snprintf (text, width, "%g", *((float *) binary));
#endif
	if (((ePtr = strchr(text,'e')) == NULL) &&
            (strstrIgCase(text,"nan") == NULL) &&
            (strstrIgCase(text,"inf") == NULL)) { /* eg., 0 */
	  if (strchr(text,'.') == NULL) strcatX (text,".0", 0);
	}
	else {
	  if ((strchr(text,'.') == NULL) &&
              (strstrIgCase(text, "nan") == NULL) &&
              (strstrIgCase(text, "inf") == NULL)) {  /* eg., 1e+07 */
	    len = (size_t) (ePtr - text);
	    strcpyX (tText, text, MINIMUM(len,80));
	    strcatX (tText, ".0", 0);
	    strcatX (tText, ePtr, 0);
	    strcpyX (text, tText, width-1);
	  }
	}
      }
      break;
    }
    case CDF_REAL8:
    case CDF_DOUBLE: {
      if (NegativeZeroReal8((double *) binary))
	strcpyX (text, NEGATIVEzeroSYMBOL, 0);
      else {
	size_t len;
	char *ePtr, tText[80+1];
#if defined(win32) || defined(vms)
        if (isnan(*((double *) binary)))
	  snprintf (text, width, "%s", "nan");
        else if (isinf(*((double *) binary)))
	  snprintf (text, width, "%s%s",
                    (*((double *) binary)>0.0?"":"-"), "inf");
        else
	  snprintf (text, width, "%g", *((double *) binary));
#else
        if (isinf(*((double *) binary)))
	  snprintf (text, width, "%s%s",
                    (*((double *) binary)>0.0?"":"-"), "inf");
        else
	  snprintf (text, width, "%g", *((double *) binary));
#endif
	if (((ePtr = strchr(text,'e')) == NULL) &&
            (strstrIgCase(text,"nan") == NULL) &&
            (strstrIgCase(text,"inf") == NULL)) { /* eg., 0 */
	  if (strchr(text,'.') == NULL) strcatX (text,".0", 0);
	}
	else {
	  if (strchr(text,'.') == NULL &&
              strstrIgCase(text, "nan") == NULL &&
              strstrIgCase(text, "inf") == NULL) {  /* eg., 1e+07 */
	    len = (size_t) (ePtr - text);
	    strcpyX (tText, text, MINIMUM(len,80));
	    strcatX (tText, ".0", 0);
	    strcatX (tText, ePtr, 0);
	    strcpyX (text, tText, width-1);
	  }
	}
      }
      break;
    }
    case CDF_CHAR:
      snprintf (text, width, "%c", *((sChar *) binary));
      break;
    case CDF_UCHAR:
      snprintf (text, width, "%c", *((uChar *) binary));
      break;
    case CDF_EPOCH:
      switch (style) {
	case EPOCH0_STYLE:
	  encodeEPOCH (*((double *) binary), text);
	  break;
	case EPOCH1_STYLE:
	  encodeEPOCH1 (*((double *) binary), text);
	  break;
	case EPOCH2_STYLE:
	  encodeEPOCH2 (*((double *) binary), text);
	  break;
	case EPOCH3_STYLE:
	  encodeEPOCH3 (*((double *) binary), text);
	  break;
	case EPOCH4_STYLE:
	  encodeEPOCH4 (*((double *) binary), text);
	  break;
	case EPOCHf_STYLE:
	  return EncodeValue (CDF_DOUBLE,binary,text,0,width);
	case EPOCHx_STYLE:
	  /* Unsupported (a format specification is needed). */
	  break;
      }
      break;
    case CDF_EPOCH16:
      switch (style) {
        case EPOCH0_STYLE:
          encodeEPOCH16 ((double *) binary, text);
          break;
        case EPOCH1_STYLE:
          encodeEPOCH16_1 ((double *) binary, text);
          break;
        case EPOCH2_STYLE:
          encodeEPOCH16_2 ((double *) binary, text);
          break;
        case EPOCH3_STYLE:
          encodeEPOCH16_3 ((double *) binary, text);
          break;
        case EPOCH4_STYLE:
          encodeEPOCH16_4 ((double *) binary, text);
          break;
        case EPOCHf_STYLE:
          return EncodeValue (CDF_DOUBLE,binary,text,0,width);
        case EPOCHx_STYLE:
          /* Unsupported (a format specification is needed). */
          break;
      }
      break;
    case CDF_TIME_TT2000:
      /* If converting from a EPOCH data type, the following predefined values
         (along with 0.0) that are outside of valid range for TT2000 will be
         acceped. */
      if ((*((double *) binary) == -1.0E+31) ||
          (*((double *) binary) == -1.0E-31) ||
          (*((long long *) binary) == FILLED_TT2000_VALUE)) {
        *(long long *) binary = FILLED_TT2000_VALUE;
      } else if (NegativeZeroReal8((double *) binary)) {
        *(long long *) binary = ILLEGAL_TT2000_VALUE;
      }
      switch (style) {
        case TT2000_0_STYLE:
          CDF_TT2000_to_UTC_string (*(long long *) binary, text, 0);
          break;
        case TT2000_1_STYLE:
          CDF_TT2000_to_UTC_string (*(long long *) binary, text, 1);
          break;
        case TT2000_2_STYLE:
          CDF_TT2000_to_UTC_string (*(long long *) binary, text, 2);
          break;
        case TT2000_3_STYLE:
          CDF_TT2000_to_UTC_string (*(long long *) binary, text, 3);
          break;
      }
      break;
  }
#if defined(win32)
  if (dataType == CDF_REAL4 || dataType == CDF_FLOAT ||
      dataType == CDF_REAL8 || dataType == CDF_DOUBLE)
    RemoveLeading0 (text, width);
#endif
  return strlen(text);
}

/******************************************************************************
* EncodeString.  Returns width of encoded string (including delimeters).
******************************************************************************/

int EncodeString (nChars, iString, oString, minWidth, maxWidth)
long nChars;            /* Number of characters (elements). */
char *iString;          /* String to encode (input). */
char *oString;          /* Encoded string (output). */
int minWidth;           /* Minimum width (including delimeters if a string
			   data type.  If zero, no minimum width (no blank
			   padding).  If positive, right justified (padding
			   on left side).  If negative, left justified
			   (padding on right side). */
int maxWidth;           /* Maximum width (including delimeters).  If zero,
			   no maximum width (no truncating). */
{
  char delim = PickDelimiter (iString, (size_t) nChars); int i;
  long numBytes;
  /****************************************************************************
  * Encode string.
  ****************************************************************************/
  numBytes = (long) UTF8StrLength ((unsigned char *)iString);

  if ((nChars + 2) > BOO(maxWidth == 0,(int)(nChars + 2),maxWidth)) {
    /**************************************************************************
    * Won't fit.  Should at least encode `"a..."'.  If not enough room, fill
    * in with astericks.
    **************************************************************************/
    if (maxWidth < 6) {
      CpyNcharacters (oString, maxWidth, (int) '*');
    }
    else {
      oString[0] = delim;
      for (i = 0; i < maxWidth - 5; i++) {
	 if (numBytes == nChars) 
	   oString[i+1] = BOO(Printable(iString[i]),iString[i],'.');
	 else
	   if (i < numBytes)
	     oString[i+1] = iString[i];
	   else
	     oString[i+1] = ' ';
      }
      oString[i+1] = '.';
      oString[i+2] = '.';
      oString[i+3] = '.';
      oString[i+4] = delim;
      oString[i+5] = NUL;
    }
  }
  else {
    /**************************************************************************
    * Fits.
    **************************************************************************/
    oString[0] = delim;
    for (i = 0; i < nChars; i++) {
       if (numBytes == nChars)
         oString[i+1] = BOO(Printable(iString[i]),iString[i],'.');
       else
	 if (i < numBytes)
	   oString[i+1] = iString[i];
	 else
	   oString[i+1] = ' ';
    }
    oString[i+1] = delim;
    oString[i+2] = NUL;
  }
  /****************************************************************************
  * Blank pad to `minWidth' if necessary.
  ****************************************************************************/
  if (minWidth != 0) {
    int n = (int) strlen(oString);
    if (minWidth > 0) {         /* Right justify. */
      if (n < minWidth) {
	int i, pad = minWidth - n;
	memmove (&oString[pad], oString, n + 1);
	for (i = 0; i < pad; i++) oString[i] = ' ';
      }
    }
    else {                      /* Left justify. */
      int min = -minWidth;
      if (n < min) {
	int pad = min - n;
	CatNcharacters (oString, pad, (int) ' ');
      }
    }
  }
  /****************************************************************************
  * Return length of encoded string.
  ****************************************************************************/
  return strlen(oString);
}

/******************************************************************************
* EncodeNegativeZero.
******************************************************************************/

void EncodeNegativeZero (string, format, width)
char *string;
char *format;
size_t width;
{
  char *ptr;
  /****************************************************************************
  * Encode 0.0 using format then check if the value was successfully encoded.
  ****************************************************************************/
  string[0] = NUL;
  snprintf(string, width, format, 0.0);
  if (NULstring(string)) {
    strcpyX (string, "-0.0(bad FORMAT)", 0);
    return;
  }
  /****************************************************************************
  * 0.0 was successfully encoded - put a negative sign at the beginning
  * (increasing the length of the string if necessary).
  ****************************************************************************/
  ptr = string;
  while (*ptr == ' ') ptr++;
  if (ptr == string) {
    memmove (&string[1], string, strlen(string) + 1);
    string[0] = '-';
  }
  else
    *(ptr - 1) = '-';
  return;
}

/******************************************************************************
* UseCformat.
* Returns width of encoded value.  Not for use with CDF_EPOCH, CDF_EPOCH16,
* CDF_(U)CHAR, CDF_TIME_TT2000.
******************************************************************************/

static int UseCformat (dataType, CdataType, Cformat, binary, text, width)
long dataType;          /* Value's actual data type.  This should never be
			   CDF_EPOCH. */
long CdataType;         /* Data type corresponding to C format.  This should
			   never be CDF_EPOCH. */
char *Cformat;          /* C format specification. */
void *binary;           /* Value to be encoded. */
char *text;             /* String into which to encode the value. */
size_t width;		/* Size of encoded text. */
{
  switch (dataType) {
    case CDF_BYTE:
    case CDF_INT1:
      switch (CdataType) {
	case CDF_BYTE:
	case CDF_INT1:
	  snprintf (text, width, Cformat, (sChar) *((sChar *) binary));
	  break;
	case CDF_UINT1:
	  snprintf (text, width, Cformat, (uChar) *((sChar *) binary));
	  break;
	case CDF_INT2:
	  snprintf (text, width, Cformat, (Int16) *((sChar *) binary));
	  break;
	case CDF_UINT2:
	  snprintf (text, width, Cformat, (uInt16) *((sChar *) binary));
	  break;
	case CDF_INT4:
	  snprintf (text, width, Cformat, (Int32) *((sChar *) binary));
	  break;
	case CDF_INT8:
	  snprintf (text, width, Cformat, (Int64) *((sChar *) binary));
	  break;
	case CDF_UINT4:
	  snprintf (text, width, Cformat, (uInt32) *((sChar *) binary));
	  break;
	case CDF_REAL4:
	case CDF_FLOAT:
	  snprintf (text, width, Cformat, (float) *((sChar *) binary));
	  break;
	case CDF_REAL8:
	case CDF_DOUBLE:
	  snprintf (text, width, Cformat, (double) *((sChar *) binary));
	  break;
      }
      break;
    case CDF_UINT1:
      switch (CdataType) {
	case CDF_BYTE:
	case CDF_INT1:
	  snprintf (text, width, Cformat, (sChar) *((uChar *) binary));
	  break;
	case CDF_UINT1:
	  snprintf (text, width, Cformat, (uChar) *((uChar *) binary));
	  break;
	case CDF_INT2:
	  snprintf (text, width, Cformat, (Int16) *((uChar *) binary));
	  break;
	case CDF_UINT2:
	  snprintf (text, width, Cformat, (uInt16) *((uChar *) binary));
	  break;
	case CDF_INT4:
	  snprintf (text, width, Cformat, (Int32) *((uChar *) binary));
	  break;
	case CDF_INT8:
	  snprintf (text, width, Cformat, (Int64) *((uChar *) binary));
	  break;
	case CDF_UINT4:
	  snprintf (text, width, Cformat, (uInt32) *((uChar *) binary));
	  break;
	case CDF_REAL4:
	case CDF_FLOAT:
	  snprintf (text, width, Cformat, (float) *((uChar *) binary));
	  break;
	case CDF_REAL8:
	case CDF_DOUBLE:
	  snprintf (text, width, Cformat, (double) *((uChar *) binary));
	  break;
      }
      break;
    case CDF_INT2:
      switch (CdataType) {
	case CDF_BYTE:
	case CDF_INT1:
	  snprintf (text, width, Cformat, (sChar) *((Int16 *) binary));
	  break;
	case CDF_UINT1:
	  snprintf (text, width, Cformat, (uChar) *((Int16 *) binary));
	  break;
	case CDF_INT2:
	  snprintf (text, width, Cformat, (Int16) *((Int16 *) binary));
	  break;
	case CDF_UINT2:
	  snprintf (text, width, Cformat, (uInt16) *((Int16 *) binary));
	  break;
	case CDF_INT4:
	  snprintf (text, width, Cformat, (Int32) *((Int16 *) binary));
	  break;
	case CDF_INT8:
	  snprintf (text, width, Cformat, (Int64) *((Int16 *) binary));
	  break;
	case CDF_UINT4:
	  snprintf (text, width, Cformat, (uInt32) *((Int16 *) binary));
	  break;
	case CDF_REAL4:
	case CDF_FLOAT:
	  snprintf (text, width, Cformat, (float) *((Int16 *) binary));
	  break;
	case CDF_REAL8:
	case CDF_DOUBLE:
	  snprintf (text, width, Cformat, (double) *((Int16 *) binary));
	  break;
      }
      break;
    case CDF_UINT2:
      switch (CdataType) {
	case CDF_BYTE:
	case CDF_INT1:
	  snprintf (text, width, Cformat, (sChar) *((uInt16 *) binary));
	  break;
	case CDF_UINT1:
	  snprintf (text, width, Cformat, (uChar) *((uInt16 *) binary));
	  break;
	case CDF_INT2:
	  snprintf (text, width, Cformat, (Int16) *((uInt16 *) binary));
	  break;
	case CDF_UINT2:
	  snprintf (text, width, Cformat, (uInt16) *((uInt16 *) binary));
	  break;
	case CDF_INT4:
	  snprintf (text, width, Cformat, (Int32) *((uInt16 *) binary));
	  break;
	case CDF_INT8:
	  snprintf (text, width, Cformat, (Int64) *((uInt16 *) binary));
	  break;
	case CDF_UINT4:
	  snprintf (text, width, Cformat, (uInt32) *((uInt16 *) binary));
	  break;
	case CDF_REAL4:
	case CDF_FLOAT:
	  snprintf (text, width, Cformat, (float) *((uInt16 *) binary));
	  break;
	case CDF_REAL8:
	case CDF_DOUBLE:
	  snprintf (text, width, Cformat, (double) *((uInt16 *) binary));
	  break;
      }
      break;
    case CDF_INT4:
      switch (CdataType) {
	case CDF_BYTE:
	case CDF_INT1:
	  snprintf (text, width, Cformat, (sChar) *((Int32 *) binary));
	  break;
	case CDF_UINT1:
	  snprintf (text, width, Cformat, (uChar) *((Int32 *) binary));
	  break;
	case CDF_INT2:
	  snprintf (text, width, Cformat, (Int16) *((Int32 *) binary));
	  break;
	case CDF_UINT2:
	  snprintf (text, width, Cformat, (uInt16) *((Int32 *) binary));
	  break;
	case CDF_INT4:
	  snprintf (text, width, Cformat, (Int32) *((Int32 *) binary));
	  break;
	case CDF_INT8:
	  snprintf (text, width, Cformat, (Int64) *((Int32 *) binary));
	  break;
	case CDF_UINT4:
	  snprintf (text, width, Cformat, (uInt32) *((Int32 *) binary));
	  break;
	case CDF_REAL4:
	case CDF_FLOAT:
	  snprintf (text, width, Cformat, (float) *((Int32 *) binary));
	  break;
	case CDF_REAL8:
	case CDF_DOUBLE:
	  snprintf (text, width, Cformat, (double) *((Int32 *) binary));
	  break;
      }
      break;
    case CDF_INT8:
      switch (CdataType) {
	case CDF_BYTE:
	case CDF_INT1:
	  snprintf (text, width, Cformat, (sChar) *((Int64 *) binary));
	  break;
	case CDF_UINT1:
	  snprintf (text, width, Cformat, (uChar) *((Int64 *) binary));
	  break;
	case CDF_INT2:
	  snprintf (text, width, Cformat, (Int16) *((Int64 *) binary));
	  break;
	case CDF_UINT2:
	  snprintf (text, width, Cformat, (uInt16) *((Int64 *) binary));
	  break;
	case CDF_INT4:
	  snprintf (text, width, Cformat, (Int32) *((Int64 *) binary));
	  break;
	case CDF_INT8:
	  snprintf (text, width, Cformat, (Int64) *((Int64 *) binary));
	  break;
	case CDF_UINT4:
	  snprintf (text, width, Cformat, (uInt32) *((Int64 *) binary));
	  break;
	case CDF_REAL4:
	case CDF_FLOAT:
	  snprintf (text, width, Cformat, (float) *((Int64 *) binary));
	  break;
	case CDF_REAL8:
	case CDF_DOUBLE:
	  snprintf (text, width, Cformat, (double) *((Int64 *) binary));
	  break;
      }
      break;
    case CDF_UINT4:
      switch (CdataType) {
	case CDF_BYTE:
	case CDF_INT1:
	  snprintf (text, width, Cformat, (sChar) *((uInt32 *) binary));
	  break;
	case CDF_UINT1:
	  snprintf (text, width, Cformat, (uChar) *((uInt32 *) binary));
	  break;
	case CDF_INT2:
	  snprintf (text, width, Cformat, (Int16) *((uInt32 *) binary));
	  break;
	case CDF_UINT2:
	  snprintf (text, width, Cformat, (uInt16) *((uInt32 *) binary));
	  break;
	case CDF_INT4:
	  snprintf (text, width, Cformat, (Int32) *((uInt32 *) binary));
	  break;
	case CDF_INT8:
	  snprintf (text, width, Cformat, (Int64) *((uInt32 *) binary));
	  break;
	case CDF_UINT4:
	  snprintf (text, width, Cformat, (uInt32) *((uInt32 *) binary));
	  break;
	case CDF_REAL4:
	case CDF_FLOAT:
	  snprintf (text, width, Cformat, (float) *((uInt32 *) binary));
	  break;
	case CDF_REAL8:
	case CDF_DOUBLE:
	  snprintf (text, width, Cformat, (double) *((uInt32 *) binary));
	  break;
      }
      break;
    case CDF_REAL4:
    case CDF_FLOAT:
      switch (CdataType) {
	case CDF_BYTE:
	case CDF_INT1:
	  snprintf (text, width, Cformat, (sChar) *((float *) binary));
	  break;
	case CDF_UINT1:
	  snprintf (text, width, Cformat, (uChar) *((float *) binary));
	  break;
	case CDF_INT2:
	  snprintf (text, width, Cformat, (Int16) *((float *) binary));
	  break;
	case CDF_UINT2:
	  snprintf (text, width, Cformat, (uInt16) *((float *) binary));
	  break;
	case CDF_INT4:
	  snprintf (text, width, Cformat, (Int32) *((float *) binary));
	  break;
	case CDF_INT8:
	  snprintf (text, width, Cformat, (Int64) *((float *) binary));
	  break;
	case CDF_UINT4:
	  snprintf (text, width, Cformat, (uInt32) *((float *) binary));
	  break;
	case CDF_REAL4:
	case CDF_FLOAT:
	  if (NegativeZeroReal4((float *) binary))
	    EncodeNegativeZero (text, Cformat, width);
	  else {
#if defined(win32) || defined(vms)
	    if (isnan((double)*((float*)binary)))
	      snprintf (text, width, "%s", "nan");
	    else if (isinf((double)*((float*)binary)))
	      snprintf (text, width, "%s", "inf");
	    else
	      snprintf (text, width, Cformat, (float) *((float *) binary));
#else
	    snprintf (text, width, Cformat, (float) *((float *) binary));
#endif
          }
	  break;
	case CDF_REAL8:
	case CDF_DOUBLE:
	  if (NegativeZeroReal4((float *) binary))
	    EncodeNegativeZero (text, Cformat, width);
	  else
	    snprintf (text, width, Cformat, (double) *((float *) binary));
	  break;
      }
      break;
    case CDF_REAL8:
    case CDF_DOUBLE:
      switch (CdataType) {
	case CDF_BYTE:
	case CDF_INT1:
	  snprintf (text, width, Cformat, (sChar) *((double *) binary));
	  break;
	case CDF_UINT1:
	  snprintf (text, width, Cformat, (uChar) *((double *) binary));
	  break;
	case CDF_INT2:
	  snprintf (text, width, Cformat, (Int16) *((double *) binary));
	  break;
	case CDF_UINT2:
	  snprintf (text, width, Cformat, (uInt16) *((double *) binary));
	  break;
	case CDF_INT4:
	  snprintf (text, width, Cformat, (Int32) *((double *) binary));
	  break;
	case CDF_INT8:
	  snprintf (text, width, Cformat, (Int64) *((double *) binary));
	  break;
	case CDF_UINT4:
	  snprintf (text, width, Cformat, (uInt32) *((double *) binary));
	  break;
	case CDF_REAL4:
	case CDF_FLOAT:
	  if (NegativeZeroReal8((double *) binary))
	    EncodeNegativeZero (text, Cformat, width);
	  else
	    snprintf (text, width, Cformat, (float) *((double *) binary));
	  break;
	case CDF_REAL8:
	case CDF_DOUBLE:
	  if (NegativeZeroReal8((double *) binary))
	    EncodeNegativeZero (text, Cformat, width);
	  else {
#if defined(win32) || defined(vms)
	    if (isnan(*((double*)binary)))
	      snprintf (text, width, "%s", "nan");
	    else if (isinf(*((double*)binary)))
	      snprintf (text, width, "%s", "inf");
	    else
	      snprintf (text, width, Cformat, (double) *((double *) binary));
#else
	    if (isinf(*((double*)binary)))
	      snprintf (text, width, "%s", "inf");
            else
	      snprintf (text, width, Cformat, (double) *((double *) binary));
#endif
          }
	  break;
      }
      break;
  }

#if defined(win32)
  if (CdataType == CDF_REAL4 || CdataType == CDF_FLOAT ||
      CdataType == CDF_REAL8 || CdataType == CDF_DOUBLE)
    RemoveLeading0 (text, width);
#endif

  return strlen(text);
}

/******************************************************************************
* FtoCformat.
* Returns TRUE if valid FORTRAN format specification.
******************************************************************************/

static Logical FtoCformat (dataType, Fformat, CdataType, Cformat, width)
long dataType;          /* Value's actual data type.  This should never be
			   CDF_EPOCH. */
char *Fformat;          /* FORTRAN format specification. */
long *CdataType;        /* Data type implied by C format specification. */
char *Cformat;          /* C format specification. */
size_t width;		/* Size of Cformat. */
{
  int nFound;
  int w, m, d;
  /****************************************************************************
  * Change `Fformat' to point to first non-blank, non-digit, non-`('
  * character.  This will skip over a Fortran repeat count (eg. the `20' in
  * `20F8.4' or `20(F8.4)').
  ****************************************************************************/
  while (*Fformat != NUL && (*Fformat == '(' ||
			     Spacing(*Fformat) ||
			     Decimal(*Fformat))) Fformat++;
  if (*Fformat == NUL) return FALSE;
  /****************************************************************************
  * Encode C format specification.
  ****************************************************************************/
  switch (Fformat[0]) {
    /**************************************************************************
    * Integer/decimal.
    **************************************************************************/
    case 'I':
    case 'i':
      nFound = sscanf (&Fformat[1], "%d.%d", &w, &m);
      switch (nFound) {
	case 0:
	  return FALSE;
	case 1:
	  switch (dataType) {
	    case CDF_BYTE:
	    case CDF_INT1:
	      snprintf (Cformat, width, "%%%dd", w);
	      *CdataType = CDF_INT1;
	      return TRUE;
	    case CDF_UINT1:
	      snprintf (Cformat, width, "%%%du", w);
	      *CdataType = CDF_UINT1;
	      return TRUE;
	    case CDF_INT2:
	      snprintf (Cformat, width, "%%%dd", w);
	      *CdataType = CDF_INT2;
	      return TRUE;
	    case CDF_UINT2:
	      snprintf (Cformat, width, "%%%du", w);
	      *CdataType = CDF_UINT2;
	      return TRUE;
	    case CDF_INT4:
	      snprintf (Cformat, width, "%%%d%sd", w, Int32FORMATmod);
	      *CdataType = CDF_INT4;
	      return TRUE;
	    case CDF_INT8:
	      snprintf (Cformat, width, "%%%d%sd", w, Int64FORMATmod);
	      *CdataType = CDF_INT8;
	      return TRUE;
	    case CDF_UINT4:
	      snprintf (Cformat, width, "%%%d%su", w, Int32FORMATmod);
	      *CdataType = CDF_UINT4;
	      return TRUE;
	    case CDF_REAL4:
	    case CDF_FLOAT:
	    case CDF_REAL8:
	    case CDF_DOUBLE:
	      snprintf (Cformat, width, "%%%d%sd", w, Int32FORMATmod);
	      *CdataType = CDF_INT4;
	      return TRUE;
	  }
	  return FALSE;
	case 2:
	  switch (dataType) {
	    case CDF_BYTE:
	    case CDF_INT1:
	      snprintf (Cformat, width, "%%%d.%dd", w, m);
	      *CdataType = CDF_INT1;
	      return TRUE;
	    case CDF_UINT1:
	      snprintf (Cformat, width, "%%%d.%du", w, m);
	      *CdataType = CDF_UINT1;
	      return TRUE;
	    case CDF_INT2:
	      snprintf (Cformat, width, "%%%d.%dd", w, m);
	      *CdataType = CDF_INT2;
	      return TRUE;
	    case CDF_UINT2:
	      snprintf (Cformat, width, "%%%d.%du", w, m);
	      *CdataType = CDF_UINT2;
	      return TRUE;
	    case CDF_INT4:
	      snprintf (Cformat, width, "%%%d.%d%sd", w, m, Int32FORMATmod);
	      *CdataType = CDF_INT4;
	      return TRUE;
	    case CDF_INT8:
	      snprintf (Cformat, width, "%%%d.%d%sd", w, m, Int64FORMATmod);
	      *CdataType = CDF_INT8;
	      return TRUE;
	    case CDF_UINT4:
	      snprintf (Cformat, width, "%%%d.%d%su", w, m, Int32FORMATmod);
	      *CdataType = CDF_UINT4;
	      return TRUE;
	    case CDF_REAL4:
	    case CDF_FLOAT:
	    case CDF_REAL8:
	    case CDF_DOUBLE:
	      snprintf (Cformat, width, "%%%d.%d%sd", w, m, Int32FORMATmod);
	      *CdataType = CDF_INT4;
	      return TRUE;
	  }
	  return FALSE;
      }
      return FALSE;
    /**************************************************************************
    * Integer/octal.
    **************************************************************************/
    case 'O':
    case 'o':
      nFound = sscanf (&Fformat[1], "%d.%d", &w, &m);
      switch (nFound) {
	case 0:
	  return FALSE;
	case 1:
	  switch (dataType) {
	    case CDF_BYTE:
	    case CDF_INT1:
	      snprintf (Cformat, width, "%%%do", w);
	      *CdataType = CDF_UINT1;
	      return TRUE;
	    case CDF_UINT1:
	      snprintf (Cformat, width, "%%%do", w);
	      *CdataType = CDF_UINT1;
	      return TRUE;
	    case CDF_INT2:
	      snprintf (Cformat, width, "%%%do", w);
	      *CdataType = CDF_UINT2;
	      return TRUE;
	    case CDF_UINT2:
	      snprintf (Cformat, width, "%%%do", w);
	      *CdataType = CDF_UINT2;
	      return TRUE;
	    case CDF_INT4:
	      snprintf (Cformat, width, "%%%d%so", w, Int32FORMATmod);
	      *CdataType = CDF_UINT4;
	      return TRUE;
	    case CDF_INT8:
	      snprintf (Cformat, width, "%%%d%so", w, Int64FORMATmod);
	      *CdataType = CDF_UINT4;
	      return TRUE;
	    case CDF_UINT4:
	      snprintf (Cformat, width, "%%%d%so", w, Int32FORMATmod);
	      *CdataType = CDF_UINT4;
	      return TRUE;
	    case CDF_REAL4:
	    case CDF_FLOAT:
	    case CDF_REAL8:
	    case CDF_DOUBLE:
	      snprintf (Cformat, width, "%%%d%so", w, Int32FORMATmod);
	      *CdataType = CDF_UINT4;
	      return TRUE;
	  }
	  return FALSE;
	case 2:
	  switch (dataType) {
	    case CDF_BYTE:
	    case CDF_INT1:
	      snprintf (Cformat, width, "%%%d.%do", w, m);
	      *CdataType = CDF_UINT1;
	      return TRUE;
	    case CDF_UINT1:
	      snprintf (Cformat, width, "%%%d.%do", w, m);
	      *CdataType = CDF_UINT1;
	      return TRUE;
	    case CDF_INT2:
	      snprintf (Cformat, width, "%%%d.%do", w, m);
	      *CdataType = CDF_UINT2;
	      return TRUE;
	    case CDF_UINT2:
	      snprintf (Cformat, width, "%%%d.%do", w, m);
	      *CdataType = CDF_UINT2;
	      return TRUE;
	    case CDF_INT4:
	      snprintf (Cformat, width, "%%%d.%d%so", w, m, Int32FORMATmod);
	      *CdataType = CDF_UINT4;
	      return TRUE;
	    case CDF_INT8:
	      snprintf (Cformat, width, "%%%d.%d%so", w, m, Int64FORMATmod);
	      *CdataType = CDF_UINT4;
	      return TRUE;
	    case CDF_UINT4:
	      snprintf (Cformat, width, "%%%d.%d%so", w, m, Int32FORMATmod);
	      *CdataType = CDF_UINT4;
	      return TRUE;
	    case CDF_REAL4:
	    case CDF_FLOAT:
	    case CDF_REAL8:
	    case CDF_DOUBLE:
	      snprintf (Cformat, width, "%%%d.%d%so", w, m, Int32FORMATmod);
	      *CdataType = CDF_UINT4;
	      return TRUE;
	  }
	  return FALSE;
      }
      return FALSE;
    /**************************************************************************
    * Integer/hexadecimal.
    **************************************************************************/
    case 'Z':
    case 'z':
      nFound = sscanf (&Fformat[1], "%d.%d", &w, &m);
      switch (nFound) {
	case 0:
	  return FALSE;
	case 1:
	  switch (dataType) {
	    case CDF_BYTE:
	    case CDF_INT1:
	      snprintf (Cformat, width, "%%%dX", w);
	      *CdataType = CDF_UINT1;
	      return TRUE;
	    case CDF_UINT1:
	      snprintf (Cformat, width, "%%%dX", w);
	      *CdataType = CDF_UINT1;
	      return TRUE;
	    case CDF_INT2:
	      snprintf (Cformat, width, "%%%dX", w);
	      *CdataType = CDF_UINT2;
	      return TRUE;
	    case CDF_UINT2:
	      snprintf (Cformat, width, "%%%dX", w);
	      *CdataType = CDF_UINT2;
	      return TRUE;
	    case CDF_INT4:
	      snprintf (Cformat, width, "%%%d%sX", w, Int32FORMATmod);
	      *CdataType = CDF_UINT4;
	      return TRUE;
	    case CDF_INT8:
	      snprintf (Cformat, width, "%%%d%sX", w, Int64FORMATmod);
	      *CdataType = CDF_UINT4;
	      return TRUE;
	    case CDF_UINT4:
	      snprintf (Cformat, width, "%%%d%sX", w, Int32FORMATmod);
	      *CdataType = CDF_UINT4;
	      return TRUE;
	    case CDF_REAL4:
	    case CDF_FLOAT:
	    case CDF_REAL8:
	    case CDF_DOUBLE:
	      snprintf (Cformat, width, "%%%d%sX", w, Int32FORMATmod);
	      *CdataType = CDF_UINT4;
	      return TRUE;
	  }
	  return FALSE;
	case 2:
	  switch (dataType) {
	    case CDF_BYTE:
	    case CDF_INT1:
	      snprintf (Cformat, width, "%%%d.%dX", w, m);
	      *CdataType = CDF_UINT1;
	      return TRUE;
	    case CDF_UINT1:
	      snprintf (Cformat, width, "%%%d.%dX", w, m);
	      *CdataType = CDF_UINT1;
	      return TRUE;
	    case CDF_INT2:
	      snprintf (Cformat, width, "%%%d.%dX", w, m);
	      *CdataType = CDF_UINT2;
	      return TRUE;
	    case CDF_UINT2:
	      snprintf (Cformat, width, "%%%d.%dX", w, m);
	      *CdataType = CDF_UINT2;
	      return TRUE;
	    case CDF_INT4:
	      snprintf (Cformat, width, "%%%d.%d%sX", w, m, Int32FORMATmod);
	      *CdataType = CDF_UINT4;
	      return TRUE;
	    case CDF_INT8:
	      snprintf (Cformat, width, "%%%d.%d%sX", w, m, Int64FORMATmod);
	      *CdataType = CDF_UINT4;
	      return TRUE;
	    case CDF_UINT4:
	      snprintf (Cformat, width, "%%%d.%d%sX", w, m, Int32FORMATmod);
	      *CdataType = CDF_UINT4;
	      return TRUE;
	    case CDF_REAL4:
	    case CDF_FLOAT:
	    case CDF_REAL8:
	    case CDF_DOUBLE:
	      snprintf (Cformat, width, "%%%d.%d%sX", w, m, Int32FORMATmod);
	      *CdataType = CDF_UINT4;
	      return TRUE;
	  }
	  return FALSE;
      }
      return FALSE;
    /**************************************************************************
    * Floating-point/non-scientific notation (which is called...
    **************************************************************************/
    case 'F':
    case 'f':
      nFound = sscanf (&Fformat[1], "%d.%d", &w, &d);
      switch (nFound) {
	case 2:
	  snprintf (Cformat, width, "%%%d.%df", w, d);
          if (dataType == CDF_FLOAT || dataType == CDF_REAL4)
            *CdataType = CDF_FLOAT;
          else
	    *CdataType = CDF_DOUBLE;
	  return TRUE;
      }
      return FALSE;
    /**************************************************************************
    * Floating-point/scientific notation.
    **************************************************************************/
    case 'E':
    case 'e':
    case 'D':
    case 'd':
      nFound = sscanf (&Fformat[1], "%d.%d", &w, &d);
      switch (nFound) {
	case 2:
	  snprintf (Cformat, width, "%%%d.%de", w, d);
          if (dataType == CDF_FLOAT || dataType == CDF_REAL4)
            *CdataType = CDF_FLOAT;
          else
	    *CdataType = CDF_DOUBLE;
	  return TRUE;
      }
      return FALSE;
    /**************************************************************************
    * Floating-point/notation depends on value.
    **************************************************************************/
    case 'G':
    case 'g':
      nFound = sscanf (&Fformat[1], "%d.%d", &w, &d);
      switch (nFound) {
	case 2:
	  snprintf (Cformat, width, "%%%d.%dg", w, d);
          if (dataType == CDF_FLOAT || dataType == CDF_REAL4)
            *CdataType = CDF_FLOAT;
          else
	    *CdataType = CDF_DOUBLE;
	  return TRUE;
      }
      return FALSE;
    /**************************************************************************
    * Character.
    **************************************************************************/
    case 'A':
    case 'a':
      nFound = sscanf (&Fformat[1], "%d", &w);
      switch (nFound) {
	case 1:
	  snprintf (Cformat, width, "%%%d.%ds", w, w);
	  *CdataType = CDF_CHAR;
	  return TRUE;
      }
      return FALSE;
  }
  return FALSE;                         /* Unknown FORTRAN format character. */
}

/******************************************************************************
* GetCdataType.
*   Given a C format specification, returns the corresponding C data type.
******************************************************************************/

static long GetCdataType (Cformat, CdataType)
char *Cformat;          /* C format specification. */
long *CdataType;        /* Corresponding C data type. */
{
  char *ptrB;           /* Beginning of specification. */
  char *ptrE;           /* End of specification. */
  int len;
  /****************************************************************************
  * Find beginning of specification.
  ****************************************************************************/
  ptrB = Cformat;
  while (*ptrB != NUL && Spacing(*ptrB)) ptrB++;
  if (*ptrB != '%') return FALSE;
  len = (int) strlen(ptrB);
  /****************************************************************************
  * Find end of specification.
  ****************************************************************************/
  ptrE = Cformat + strlen(Cformat) - 1;
  while (ptrE != ptrB && Spacing(*ptrE)) ptrE--;
  if (ptrE == ptrB) return FALSE;
  /****************************************************************************
  * Choose a data type based on the conversion character and possible modifier.
  ****************************************************************************/
  switch (*ptrE) {
    case 'd':
    case 'i':
      switch (*(ptrE-1)) {
	case 'l':
	  if (len>3 && (*(ptrE-2) == (char) 'l')) 
	    *CdataType = CDF_INT8;
	  else
	    *CdataType = CDF_INT4;
	  break;
	case 'h':
	  *CdataType = CDF_INT2;
	  break;
	default:
#if defined(dos)
	  *CdataType = CDF_INT2;
#else
	  *CdataType = CDF_INT4;
#endif
	  break;
      }
      break;
    case 'x':
    case 'X':
    case 'o':
    case 'u':
      switch (*(ptrE-1)) {
	case 'l':
	  *CdataType = CDF_UINT4;
	  break;
	case 'h':
	  *CdataType = CDF_UINT2;
	  break;
	default:
#if defined(dos)
	  *CdataType = CDF_UINT2;
#else
	  *CdataType = CDF_UINT4;
#endif
	  break;
      }
      break;
    case 'c':
    case 's':
      *CdataType = CDF_CHAR;
      break;
    case 'f':
    case 'e':
    case 'E':
    case 'g':
    case 'G':
      *CdataType = CDF_DOUBLE;
      break;
    default:
      return FALSE;     /* Unknown C format description. */
  }
  return TRUE;
}

/******************************************************************************
* EncodeDimensionality.  Returns width of encoded dimensionality
******************************************************************************/

int EncodeDimensionality (string, numDims, dimSizes, width)
char *string;
long numDims;
long *dimSizes;
size_t width;
{
  int dimN;
  snprintf (string, width, "%ld:[", numDims);
  for (dimN = 0; dimN < numDims; dimN++)
     snprintf (&(string[strlen(string)]), width-strlen(string),
	       "%s%ld", (dimN > 0 ? "," : ""), dimSizes[dimN]);
  strcatX (string, "]", 0);
  return strlen(string);
}

/******************************************************************************
* EncodeVariances.  Returns width of encoded variances.
******************************************************************************/

int EncodeVariances (string, recVary, numDims, dimVarys)
char *string;
long recVary;
long numDims;
long *dimVarys;
{
  int dimN;
  strcpyX (string, TFvarianceToken(recVary), 0);
  strcatX (string, "/", 0);
  for (dimN = 0; dimN < numDims; dimN++)
     strcatX (string, TFvarianceToken(dimVarys[dimN]), 0);
  return strlen(string);
}

/******************************************************************************
* EncodeRecordIndices.
* Uses `*' for record/dimension variances of NOVARY.  Returns width of
* record/indices.
******************************************************************************/

int EncodeRecordIndices (string, recNum, recVary, numDims, indices, dimVarys,
			 width)
char *string;
long recNum;
long recVary;
long numDims;
long indices[];
long dimVarys[];
size_t width;
{
  int dimN;
  if (recVary)
    snprintf (string, width, "%ld", recNum + 1);
  else
    strcpyX (string, "*", 0);
  strcatX (string, ":[", 0);
  for (dimN = 0; dimN < numDims; dimN++) {
     if (dimN != 0) strcatX (string, ",", 0);
     if (dimVarys[dimN])
       snprintf (&string[strlen(string)], width-strlen(string),
	         "%ld", indices[dimN] + 1);
     else
       strcatX (string, "*", 0);
  }
  strcatX (string, "]", 0);
  return strlen(string);
}

/******************************************************************************
* EncodeRecordIndicesVARY.  Force record/dimension variances to VARY.
******************************************************************************/

int EncodeRecordIndicesVARY (string, recNum, numDims, indices, width)
char *string;
long recNum;
long numDims;
long indices[];
size_t width;
{
  static long recVary = VARY;
  static long dimVarys[CDF_MAX_DIMS] = { VARY, VARY, VARY, VARY, VARY,
					 VARY, VARY, VARY, VARY, VARY };
  EncodeRecordIndices (string, recNum, recVary, numDims, indices, dimVarys, 
		       width);
  return strlen(string);
}

/*****************************************************************************
* OnlineHelpFP.
*
*  UNIX: The full pathname of the executable is not passed in via `argv[0]'.
*  The command which was entered on the command line is passed in (aliases
*  are, however, evaluated).
*
******************************************************************************/

FILE *OnlineHelpFP (filename, execpath)
char *filename;                         /* Name of help file. */
char *execpath;                         /* Pathname of executable (argv[0]). */
{
  FILE *fp;
  char pathname[DU_MAX_PATH_LEN+1];
#if !defined(mac)
  char execname[DU_MAX_NAME_LEN+1];
  char *envPtr;
#endif
#if defined(mac)
  execpath;                     /* Quites the MPW C compiler. */
#endif
  char path[DU_MAX_PATH_LEN+1], dest[DU_MAX_PATH_LEN+1];
  int  len, baselen = DU_MAX_PATH_LEN;
  char *lastdel;
#if defined(linux) || defined(sun)
  pid_t pid;
#endif

  /****************************************************************************
  * Try to use logical name (VMS) or environment variable (UNIX/POSIXshell,
  * DOS, & Windows).
  ****************************************************************************/
#if defined(vms)
  strcpyX (pathname, "CDF$HELP:", DU_MAX_PATH_LEN);
  AppendToDir (pathname, filename);
  fp = fopen (pathname, "r");
  if (fp != NULL) return fp;
#endif
#if defined(unix) || defined(dos) || defined(posixSHELL) || defined(win32)
  envPtr = getenv ("CDF_HELP");
  if (envPtr != NULL) {
    strcpyX (pathname, envPtr, DU_MAX_PATH_LEN);
    AppendToDir (pathname, filename);
#if defined(__CYGWIN__)
    fp = fopen (pathname, "rt");
#else
    fp = fopen (pathname, "r");
#endif
    if (fp != NULL) return fp;
  }
#endif
  /***************************************************************************
  * If not on a Macintosh...
  * Try relative paths from location of executable in the following order...
  *  1. From standard executable/binary directory.
  *  2. From source directory.
  *  3. Same directory as that containing the executable.
  ***************************************************************************/
#if !defined(mac)
  if (execpath != NULL) {
    if (execpath[0] != NUL) {
      ParsePath (execpath, pathname, execname);
#if defined(vms)
      pathname[strlen(pathname)-1] = NUL;   /* Wipes out trailing `]'. */
      strcatX (pathname, "-.HELP]", DU_MAX_PATH_LEN);
#endif
#if defined(unix) || defined(posixSHELL)
      AppendToDir (pathname, "../lib/cdf/help");
#endif
#if defined(dos) || defined(win32)
      AppendToDir (pathname, "..\\help");
#endif
      AppendToDir (pathname, filename);
#if defined(__CYGWIN__)
      fp = fopen (pathname, "rt");
#else
      fp = fopen (pathname, "r");
#endif
      if (fp != NULL) return fp;
      ParsePath (execpath, pathname, execname);
#if defined(vms)
      pathname[strlen(pathname)-1] = NUL;   /* Wipes out trailing `]'. */
      strcatX (pathname, "-.-.HELP]", DU_MAX_PATH_LEN);
#endif
#if defined(unix) || defined(posixSHELL)
      AppendToDir (pathname, "../help");
#endif
#if defined(dos) || defined(win32)
      AppendToDir (pathname, "..\\..\\help");
#endif
      AppendToDir (pathname, filename);
#if defined(__CYGWIN__)
      fp = fopen (pathname, "rt");
#else
      fp = fopen (pathname, "r");
#endif
      if (fp != NULL) return fp;
      ParsePath (execpath, pathname, execname);
#if defined(linux) || defined(sun) || defined(win32) || defined(__MACH__)
#  if defined(linux) || defined(sun)
      pid = getpid();
#     if defined(linux) 
        sprintf(path, "/proc/%d/exe", (int) pid);
#     else
        sprintf(path, "/proc/%d/path/a.out", (int) pid);
#     endif
      len = (int) readlink(path, dest, baselen);
      dest[len] = '\0';
#  else
#     if defined(__MACH__)
        len = _NSGetExecutablePath(dest, (uint32_t *) &baselen);
#     else
        len = (int) GetModuleFileName(NULL, dest, baselen);
        dest[len] = '\0';
#     endif
#  endif
      path[0] = '\0';
#  if !defined(win32)
      lastdel = strrchr (dest, '/');
      strncpy (path, dest, (size_t) (lastdel-dest));
      AppendToDir (path, "../help");
#  else
      lastdel = strrchr (dest, '\\');
      strncpy (path, dest, (size_t) (lastdel-dest));
      AppendToDir (path, "..\\help");
#  endif
#endif
      AppendToDir (path, filename);
#if defined(__CYGWIN__)
      fp = fopen (path, "rt");
#else
      fp = fopen (path, "r");
#endif
      if (fp != NULL) return fp;
      ParsePath (execpath, pathname, execname);
      AppendToDir (pathname, filename);
      fp = fopen (pathname, "r");
      if (fp != NULL) return fp;
    }
  }
#endif
  /***************************************************************************
  * If on a Macintosh...try relative (partial) paths to the `help' file.
  ***************************************************************************/
#if defined(mac)
  strcpyX (pathname, "::help", DU_MAX_PATH_LEN);
  AppendToDir (pathname, filename);
  fp = fopen (pathname, "r");
  if (fp != NULL) return fp;
  strcpyX (pathname, ":::help", DU_MAX_PATH_LEN);
  AppendToDir (pathname, filename);
  fp = fopen (pathname, "r");
  if (fp != NULL) return fp;
#endif
  /***************************************************************************
  * If on a UNIX/POSIXshell or DOS/Windows system, assume that the `bin'
  * directory has been added to the `path' and try a relative path from each
  * `path' entry.
  ***************************************************************************/
#if defined(unix) || defined(posixSHELL) || \
    defined(dos) || defined(win32)
#if defined(unix) || defined(posixSHELL)
#define SEP_CHAR ':'
#else
#define SEP_CHAR ';'
#endif
  envPtr = getenv ("PATH");
  if (envPtr != NULL) {
    char *path = (char *) cdf_AllocateMemory ((size_t)strlen(envPtr) + 1, FatalError);
    char *p1 = path;
    strcpyX (path, envPtr, 0);
    for (;;) {
       char *sepPtr = strchr (p1, SEP_CHAR);
       if (sepPtr != NULL) *sepPtr = NUL;
       strcpyX (pathname, p1, DU_MAX_PATH_LEN);
#if defined(unix) || defined(posixSHELL)
       AppendToDir (pathname, "../lib/cdf/help");
#else
       AppendToDir (pathname, "..\\help");
#endif
       AppendToDir (pathname, filename);
#if defined(__CYGWIN__)
       fp = fopen (pathname, "rt");
#else
       fp = fopen (pathname, "r");
#endif
       if (fp != NULL) {
	 cdf_FreeMemory (path, FatalError);
	 return fp;
       }
       if (sepPtr == NULL) break;
       p1 = sepPtr + 1;
    }
    cdf_FreeMemory (path, FatalError);
  }
#endif
  /***************************************************************************
  * Try pathes from the current directory.
  ***************************************************************************/
#if defined(vms)
  strcpyX (pathname, "[-.HELP]", DU_MAX_PATH_LEN);
#endif
#if defined(unix) || defined(posixSHELL)
  strcpyX (pathname, "../help", DU_MAX_PATH_LEN);
#endif
#if defined(dos) || defined(win32)
  strcpyX (pathname, "..\\help", DU_MAX_PATH_LEN);
#endif
  AppendToDir (pathname, filename);
#if defined(__CYGWIN__)
  fp = fopen (pathname, "rt");
#else
  fp = fopen (pathname, "r");
#endif
  if (fp != NULL) return fp;
#if defined(vms)
  strcpyX (pathname, "[-.-.HELP]", DU_MAX_PATH_LEN);
#endif
#if defined(unix) || defined(posixSHELL)
  strcpyX (pathname, "../../help", DU_MAX_PATH_LEN);
#endif
#if defined(dos) || defined(win32)
  strcpyX (pathname, "..\\..\\help", DU_MAX_PATH_LEN);
#endif
  AppendToDir (pathname, filename);
#if defined(__CYGWIN__)
  fp = fopen (pathname, "rt");
#else
  fp = fopen (pathname, "r");
#endif
  if (fp != NULL) return fp;
#if defined(vms)
  strcpyX (pathname, "[-.-.-.HELP]", DU_MAX_PATH_LEN);
#endif
#if defined(unix) || defined(posixSHELL)
  strcpyX (pathname, "../../../help", DU_MAX_PATH_LEN);
#endif
#if defined(dos) || defined(win32)
  strcpyX (pathname, "..\\..\\..\\help", DU_MAX_PATH_LEN);
#endif
  AppendToDir (pathname, filename);
#if defined(__CYGWIN__)
  fp = fopen (pathname, "rt");
#else
  fp = fopen (pathname, "r");
#endif
  if (fp != NULL) return fp;
#if defined(win32)
  strcpyX (pathname, "help", DU_MAX_PATH_LEN);
#endif
  AppendToDir (pathname, filename);
#if defined(__CYGWIN__)
  fp = fopen (pathname, "rt");
#else
  fp = fopen (pathname, "r");
#endif
  if (fp != NULL) return fp;
  /***************************************************************************
  * Try current directory.
  ***************************************************************************/
#if defined(__CYGWIN__)
  fp = fopen (pathname, "rt");
#else
  fp = fopen (pathname, "r");
#endif
  if (fp != NULL) return fp;
  /***************************************************************************
  * Give up.
  ***************************************************************************/
  return NULL;
}

/*****************************************************************************
* PageOLH.
*****************************************************************************/

void PageOLH (olhFile, argv0)
char *olhFile;
char *argv0;
{
  FILE *fp;
  char line[MAX_SCREENLINE_LEN+1+1+1];
  int nestLevel = 0;                    /* Depth into `#ifos's. */
  int osMask = 0;                       /* When 0, display line.  Note that bit
					   0 is not used (eg. nesting depth of
					   1 uses bit 1, etc.). */
#if defined(vms)
  static char thisOS[] = "vms";
#endif
#if defined(unix) || defined(posixSHELL)
  static char thisOS[] = "unix";
#endif
#if defined(dos) || (defined(win32) && defined(ALONE))
  static char thisOS[] = "dos";
#endif
#if defined(mac)
  static char thisOS[] = "mac";
#endif
#if defined(win32) && !defined(ALONE)
  static char thisOS[] = "win";
#endif
  static char *openErrorMsg = {
    "Error opening online help file - contact CDFsupport.\n"
  };
  static char *syntaxErrorMsg = {
    "Syntax error in online help file - contact CDFsupport.\n"
  };
  /****************************************************************************
  * Open online help file.
  ****************************************************************************/
  fp = OnlineHelpFP (olhFile, argv0);
  if (fp == NULL) {
    WriteOut (stdout, openErrorMsg);
    return;
  }
  /****************************************************************************
  * Read online help file - passing appropriate lines through for display.
  ****************************************************************************/
  while (fgets(line,MAX_SCREENLINE_LEN+1+1,fp) != NULL) {
    /**************************************************************************
    * Check if an operating system directive.  If not, display the line if
    * all of the bits in the operating system mask are clear.
    **************************************************************************/
    if (line[0] == '#') {
      line[strlen(line)-1] = NUL;
      /************************************************************************
      * Check for a `#ifos' directive.  If this operating system is not
      * specified, then set the bit in the operating system mask for this
      * nesting level.
      ************************************************************************/
      if (!strncmp(line,"#ifos",5)) {
	nestLevel++;
	if (strstr(line,thisOS) == NULL) SETBIT (osMask, nestLevel);
	continue;
      }
      /************************************************************************
      * Check for a `#else' directive.  Simply flip the bit in the operating
      * system mask for this nesting level.
      ************************************************************************/
      if (!strcmp(line,"#else")) {
	if (nestLevel < 1) {
	  WriteOut (stdout, syntaxErrorMsg);
	  break;
	}
	FLPBIT (osMask, nestLevel);
	continue;
      }
      /************************************************************************
      * Check for a `#endos' directive.  Clear the bit in the operating system
      * mask for this nesting level and decrement the nesting level.
      ************************************************************************/
      if (!strcmp(line,"#endos")) {
	if (nestLevel < 1) {
	  WriteOut (stdout, syntaxErrorMsg);
	  break;
	}
	CLRBIT (osMask, nestLevel);
	nestLevel--;
	continue;
      }
      /************************************************************************
      * An unknown directive has been encountered - assume it to be a comment.
      ************************************************************************/
      continue;
    }
    else {
      if (osMask == 0) WriteOut (stdout, line);
    }
  }
  if (nestLevel != 0) WriteOut (stdout, syntaxErrorMsg);
  fclose (fp);
  return;
}

/******************************************************************************
* OutputWithMargin.
*   Outputs a long text string to multiple lines of a file (or stdout)
* breaking the lines at blanks if possible.
******************************************************************************/

void OutputWithMargin (fp, text, maxLen, marginLen)
FILE *fp;
char *text;
int maxLen;     /* Maximum length of line (including margin). */
int marginLen;  /* Length of margin preceeding text on each line. */
{
  char *ptr = text, *ptrT;
  int maxLenT = maxLen - marginLen;     /* Maximum number of characters per
					   line (excluding the margin). */
  for (;;) {
     /*************************************************************************
     * If the remaining characters will fit on a line, print them and return.
     * This also covers the case where the entire text will fit on one line.
     *************************************************************************/
     if (strlen(ptr) <= (size_t) maxLenT) {
       if (marginLen > 0) Ncharacters (fp, marginLen, (int) ' ');
       WriteOut (fp, ptr);
       WriteOut (fp, "\n");
       return;
     }
     /*************************************************************************
     * Otherwise, find a blank character at which to break the characters.  The
     * blank is not printed on either line.
     *************************************************************************/
     for (ptrT = ptr + maxLenT; ptrT != ptr; ptrT--) {
	if (*ptrT == ' ') {
	  char *string = NULedString (ptr, (int) (ptrT - ptr));
	  if (marginLen > 0) Ncharacters (fp, marginLen, (int) ' ');
	  WriteOut (fp, string);
	  WriteOut (fp, "\n");
	  ptr = ptrT + 1;
	  cdf_FreeMemory (string, FatalError);
	  break;
	}
     }
     /*************************************************************************
     * If a blank could not be found, break the characters at the end of the
     * maximum number per line.
     *************************************************************************/
     if (ptrT == ptr) {
       char *string = NULedString (ptr, maxLenT);
       if (marginLen > 0) Ncharacters (fp, marginLen, (int) ' ');
       WriteOut (fp, string);
       WriteOut (fp, "\n");
       ptr += maxLenT;
       cdf_FreeMemory (string, FatalError);
     }
  }
}

/******************************************************************************
* InitPctLog.
******************************************************************************/

void InitPctLog (lastPct, pctOn)
int *lastPct;
Logical *pctOn;
{
  WriteOut (stdout, "..0%");
  *lastPct = 0;
  *pctOn = TRUE;
  return;
}

/******************************************************************************
* UpdatePctLog.
******************************************************************************/

void UpdatePctLog (pct, lastPct)
int pct;
int *lastPct;
{
  if (pct != *lastPct) {
    char string[10+1];
    snprintf (string, (size_t) sizeof(string), "\b\b\b\b%s%d%%",
	      (pct < 10 ? ".." : (pct < 100 ? "." : "")), pct);
    WriteOut (stdout, string);
    *lastPct = pct;
  }
  return;
}

/******************************************************************************
* RefreshPctLog.
******************************************************************************/

void RefreshPctLog (lastPct)
int lastPct;
{
  char string[6+1];
  snprintf (string, (size_t) sizeof(string), "%s%d%%",
	    (lastPct < 10 ? ".." : (lastPct < 100 ? "." : "")), lastPct);
  WriteOut (stdout, string);
  return;
}

/******************************************************************************
* CleanupPctLog.
*   100% is output in case there were no values (in which case 0% would be
* currently displayed).
******************************************************************************/

void CleanupPctLog (pctOn)
Logical *pctOn;
{
  WriteOut (stdout, "\b\b\b\b100%\n");
  *pctOn = FALSE;
  return;
}

/******************************************************************************
* Ncharacters.
*   Outputs a specified number of a selected character to a file pointer.
******************************************************************************/

void Ncharacters (fp, nChars, chr)
FILE *fp;
int nChars;
int chr;
{
  int i; char string[1+1];
  string[0] = (char) chr;
  string[1] = NUL;
  for (i = 0; i < nChars; i++) WriteOut (fp, string);
  return;
}

/******************************************************************************
* CatNcharacters.
*   Concatenates some number of a specified character to a string.
******************************************************************************/

void CatNcharacters (string, nChars, chr)
char *string;
int nChars;
int chr;
{
  int i;
  for (i = 0; i < nChars; i++) catchrX (string, chr, 0);
  return;
}

/******************************************************************************
* CpyNcharacters.
*   Copies some number of a specified character to a string.
******************************************************************************/

void CpyNcharacters (string, nChars, chr)
char *string;
int nChars;
int chr;
{
  int i;
  for (i = 0; i < nChars; i++) string[i] = (char) chr;
  string[nChars] = NUL;
  return;
}

/******************************************************************************
* TFqualifier.
******************************************************************************/

Logical TFqualifier (qop, variable, Tx, Fx, defaultTF, conflictText)
QOP *qop;
Logical *variable;
int Tx;
int Fx;
Logical defaultTF;
char *conflictText;
{
  int count = 0;
  if (qop->qualEntered[Tx]) count++;
  if (qop->qualEntered[Fx]) count++;
  switch (count) {
    case 0:
      *variable = defaultTF;
      return TRUE;
    case 1:
      *variable = (qop->qualEntered[Tx] ? TRUE : FALSE);
      return TRUE;
    default: {
      char tempS[MAX_MESSAGE_TEXT_LEN+1];
      snprintf (tempS, (size_t) sizeof(tempS),
	        "Conflicting qualifiers (%s & no%s).",
	        conflictText, conflictText);
      DisplayError (tempS);
      return FALSE;
    }
  }
}

/******************************************************************************
* S2qualifierLong.
******************************************************************************/

Logical S2qualifierLong (qop, variable, S1x, S1, S2x, S2, defaultS,
			 conflictText)
QOP *qop;
long *variable;
int S1x;
long S1;
int S2x;
long S2;
long defaultS;
char *conflictText;
{
  int count = 0;
  if (qop->qualEntered[S1x]) count++;
  if (qop->qualEntered[S2x]) count++;
  switch (count) {
    case 0:
      *variable = defaultS;
      return TRUE;
    case 1:
      *variable = (qop->qualEntered[S1x] ? S1 : S2);
      return TRUE;
    default: {
      char tempS[MAX_MESSAGE_TEXT_LEN+1];
      snprintf (tempS, (size_t) sizeof(tempS),
	        "Conflicting qualifiers (%s).", conflictText);
      DisplayError (tempS);
      return FALSE;
    }
  }
}

/******************************************************************************
* S3qualifierLong.
******************************************************************************/

Logical S3qualifierLong (qop, variable, S1x, S1, S2x, S2, S3x, S3, defaultS,
			 conflictText)
QOP *qop;
long *variable;
int S1x;
long S1;
int S2x;
long S2;
int S3x;
long S3;
long defaultS;
char *conflictText;
{
  int count = 0;
  if (qop->qualEntered[S1x]) count++;
  if (qop->qualEntered[S2x]) count++;
  if (qop->qualEntered[S3x]) count++;
  switch (count) {
    case 0:
      *variable = defaultS;
      return TRUE;
    case 1:
      *variable = BOO(qop->qualEntered[S1x],S1,
		      BOO(qop->qualEntered[S2x],S2,S3));
      return TRUE;
    default: {
      char tempS[MAX_MESSAGE_TEXT_LEN+1];
      snprintf (tempS, (size_t) sizeof(tempS),
	        "Conflicting qualifiers (%s).", conflictText);
      DisplayError (tempS);
      return FALSE;
    }
  }
}

/******************************************************************************
* S4qualifierLong.
******************************************************************************/

Logical S4qualifierLong (qop, variable, S1x, S1, S2x, S2, S3x, S3, S4x, S4,
			 defaultS, conflictText)
QOP *qop;
long *variable;
int S1x;
long S1;
int S2x;
long S2;
int S3x;
long S3;
int S4x;
long S4;
long defaultS;
char *conflictText;
{
  int count = 0;
  if (qop->qualEntered[S1x]) count++;
  if (qop->qualEntered[S2x]) count++;
  if (qop->qualEntered[S3x]) count++;
  if (qop->qualEntered[S4x]) count++;
  switch (count) {
    case 0:
      *variable = defaultS;
      return TRUE;
    case 1:
      *variable = BOO(qop->qualEntered[S1x],S1,
		      BOO(qop->qualEntered[S2x],S2,
                          (BOO(qop->qualEntered[S3x],S3,S4))));
      return TRUE;
    default: {
      char tempS[MAX_MESSAGE_TEXT_LEN+1];
      snprintf (tempS, (size_t) sizeof(tempS),
	        "Conflicting qualifiers (%s).", conflictText);
      DisplayError (tempS);
      return FALSE;
    }
  }
}

/******************************************************************************
* S6qualifierLong.
******************************************************************************/

Logical S6qualifierLong (qop, variable, S1x, S1, S2x, S2, S3x, S3, S4x, S4,
			 S5x, S5, S6x, S6, defaultS, conflictText)
QOP *qop;
long *variable;
int S1x;
long S1;
int S2x;
long S2;
int S3x;
long S3;
int S4x;
long S4;
int S5x;
long S5;
int S6x;
long S6;
long defaultS;
char *conflictText;
{
  int count = 0;
  if (qop->qualEntered[S1x]) count++;
  if (qop->qualEntered[S2x]) count++;
  if (qop->qualEntered[S3x]) count++;
  if (qop->qualEntered[S4x]) count++;
  if (qop->qualEntered[S5x]) count++;
  if (qop->qualEntered[S6x]) count++;
  switch (count) {
    case 0:
      *variable = defaultS;
      return TRUE;
    case 1:
      *variable = BOO(qop->qualEntered[S1x],S1,
		      (BOO(qop->qualEntered[S2x],S2,
                           (BOO(qop->qualEntered[S3x],S3,
                                (BOO(qop->qualEntered[S4x],S4,
                                     (BOO(qop->qualEntered[S5x],S5,S6)))))))));
      return TRUE;
    default: {
      char tempS[MAX_MESSAGE_TEXT_LEN+1];
      snprintf (tempS, (size_t) sizeof(tempS),
	        "Conflicting qualifiers (%s).", conflictText);
      DisplayError (tempS);
      return FALSE;
    }
  }
}

/******************************************************************************
* BlankPadRight.
*   Concatenate blanks to the right end of a string.
******************************************************************************/

void BlankPadRight (string, count)
char *string;
int count;
{
  int i;
  for (i = 0; i < count; i++) catchrX (string, (int) ' ', 0);
  return;
}

/******************************************************************************
* GetFormatEntry.
* It is assumed that the current CDF has been selected.
******************************************************************************/

CDFstatus GetFormatEntry (Z, varNum, format)
Logical Z;
long varNum;
char **format;
{
  CDFstatus pStatus = CDF_OK, status; long dataType, numElems;
  status = CDFlib (SELECT_, ATTR_NAME_, "FORMAT",
			    BOO(Z,zENTRY_,rENTRY_), varNum,
		   GET_, BOO(Z,zENTRY_DATATYPE_,rENTRY_DATATYPE_), &dataType,
			 BOO(Z,zENTRY_NUMELEMS_,rENTRY_NUMELEMS_), &numElems,
		   NULL_);
  switch (status) {
    case NO_SUCH_ATTR:
    case NO_SUCH_ENTRY:
      *format = NULL;
      return pStatus;
    default:
      if (!sX(status,&pStatus)) {
	*format = NULL;
	return pStatus;
      }
  }
  if (!STRINGdataType(dataType)) {
    *format = NULL;
    return pStatus;
  }
  *format = (char *) cdf_AllocateMemory ((size_t) (numElems + 1),
				        FatalError);
  status = CDFlib (GET_, BOO(Z,zENTRY_DATA_,rENTRY_DATA_), *format,
		   NULL_);
  if (!sX(status,&pStatus)) {
    cdf_FreeMemory (*format, FatalError);
    *format = NULL;
    return pStatus;
  }
  (*format)[(int)numElems] = NUL;
  return pStatus;
}

/******************************************************************************
* ParseOptionList.
******************************************************************************/

Logical ParseOptionList (nTokens, tokens, list, present)
int nTokens;
char *tokens[];
char *list;
Logical present[];
{
  int tokenN, matchN; size_t tokenLen;
  char token[MAX_TOKEN_LEN+1], *ptr1, *ptr2, terminator;
  /****************************************************************************
  * Assume initially that none of the tokens were found.
  ****************************************************************************/
  for (tokenN = 0; tokenN < nTokens; tokenN++) present[tokenN] = FALSE;
  /****************************************************************************
  * Check that the entered list of tokens is not a NUL string, that there are
  * some possible tokens to match, etc.
  ****************************************************************************/
  if (NULstring(list)) return TRUE;     /* Obviously, no tokens were found. */
  if (nTokens < 1) return FALSE;        /* Probably a logic error. */
  /****************************************************************************
  * Scan the entered list of tokens searching for matches with the list of
  * possible tokens.  First determine the starting character position and the
  * ending delimiter.
  ****************************************************************************/
  switch (list[0]) {
    case '(':           /* VMS-style. */
      terminator = ')';
      ptr1 = list + 1;
      break;
    case '"':		/* UNIX-style on a Mac (double quotes not stripped). */
      terminator = '"';
      ptr1 = list + 1;
      break;
    default:            /* UNIX-style on a UNIX machine or IBM PC. */
      terminator = NUL;
      ptr1 = list;
      break;
  }
  for (;;) {
     /*************************************************************************
     * Find beginning of the next token in list (skipping past any leading
     * blanks).  If the end of the list is reached instead, then there are
     * no more tokens.
     *************************************************************************/
     while (*ptr1 == ' ') ptr1++;
     if (*ptr1 == terminator) return TRUE;
     /*************************************************************************
     * Find the end of the token and copy.  The token is ended by either a
     * blank, comma, or the terminator.
     *************************************************************************/
     ptr2 = ptr1 + 1;
     while (*ptr2 != ' ' && *ptr2 != ',' && *ptr2 != terminator) ptr2++;
     tokenLen = (size_t) (ptr2 - ptr1);
     strcpyX (token, ptr1, MINIMUM(tokenLen,MAX_TOKEN_LEN));
     /*************************************************************************
     * Search possible tokens for a match.  If more than one match is found,
     * return an error.
     *************************************************************************/
     for (matchN = -1, tokenN = 0; tokenN < nTokens; tokenN++)
	if (strncmpIgCasePattern(token,tokens[tokenN],strlen(token)) == 0) {
	  if (matchN == -1) {
	    matchN = tokenN;
	  } else {
	    return FALSE;
	  }
	}
     if (matchN == -1)
       return FALSE;
     else
       present[matchN] = TRUE;
     /*************************************************************************
     * Setup to search for next token in list.
     *************************************************************************/
     switch (*ptr2) {
       case ' ':
	 /*********************************************************************
	 * The last token was ended by a blank.  Check to see if a comma is
	 * the next non-blank character.  If so, the next token begins one
	 * character beyond the comma (leading blanks will be skipped when
	 * the beginning of that token is located).  Otherwise, the next token
	 * begins at the non-blank character.
	 *********************************************************************/
	 ptr1 = ptr2 + 1;
	 while (*ptr1 == ' ') ptr1++;
	 if (*ptr1 == ',') ptr1++;
	 break;
       case ',':
	 /*********************************************************************
	 * The last token was ended by a comma.  The next token begins one
	 * character beyond the comma.
	 *********************************************************************/
	 ptr1 = ptr2 + 1;
	 break;
       default:
	 /*********************************************************************
	 * The terminator must have ended the last token.
	 *********************************************************************/
	 return TRUE;
     }
  }
}

/******************************************************************************
* NULedString.
*    Returns a pointer to a NUL-terminated string that was allocated here but
* not freed.  The caller must free the string when no longer needed.  Also,
* `len'+1 characters will be allocated for the new string (and copied to from
* the existing string) regardless of whether or not a NUL appears in the first
* `len' characters.
******************************************************************************/

char *NULedString (ptr, len)
char *ptr;                      /* Pointer to the string from which to extract
				   the first `len' characters. */
size_t len;                     /* Number of characters to extract.  This does
				   not include the terminating NUL that will
				   be appended to the string being returned. */
{
  char *string;
  string = (char *) cdf_AllocateMemory ((size_t)len + 1, FatalError);
  memmove (string, ptr, len);
  string[len] = NUL;
  return string;
}

/******************************************************************************
* WriteOutSO.
******************************************************************************/

#if defined(STDARG)
int WriteOutSO (char *format, ...)
#else
int WriteOutSO (va_alist)
va_dcl
#endif
{
#if !defined(STDARG)
  char *format;
#endif
  int n;
  char text[MAX_oTEXT_LEN];
  va_list ap;
#if defined(STDARG)
  va_start (ap, format);
#else
  VA_START (ap);
  format = va_arg (ap, char *);
#endif
  vsnprintf (text, (size_t) sizeof(text), format, ap);
  n = WriteOut (stdout, text);
  va_end (ap);
  return n;
}

/******************************************************************************
* WriteOutFP.
******************************************************************************/

#if defined(STDARG)
int WriteOutFP (FILE *fp, char *format, ...)
#else
int WriteOutFP (va_alist)
va_dcl
#endif
{
#if !defined(STDARG)
  FILE *fp;
  char *format;
#endif
  int n;
  char text[MAX_oTEXT_LEN];
  va_list ap;
#if defined(STDARG)
  va_start (ap, format);
#else
  VA_START (ap);
  fp = va_arg (ap, FILE *);
  format = va_arg (ap, char *);
#endif
  vsnprintf (text, (size_t) sizeof(text), format, ap);
  n = WriteOut (fp, text);
  va_end (ap);
  return n;
}

/******************************************************************************
* WriteOut.
* Returns number of characters output.
******************************************************************************/

int WriteOut (fp, text)
FILE *fp;
char *text;
{
  /****************************************************************************
  * If the output is not going to `stdout', then assume it is going to a file
  * (and ignore paging if requested).
  ****************************************************************************/
  if (fp != stdout)
    fprintf (fp, "%s", text);
  else {
    /**************************************************************************
    * The output is going to `stdout'.  Assume that `stdout' has not been
    * redirected away from the user's terminal (to a file).  If `stdout' has
    * been redirected to a file, then hopefully the user had enough sense not
    * to request paging.
    **************************************************************************/
#if defined(mac) || (defined(win32) && !defined(ALONE))
#if defined(mac)
    WriteOutMacSO (text);
#else
#if defined(SO)
    WriteOutWin32 (text);
#endif
#endif
#else
    char *ptr = text, *ptrNL; size_t len;
    /**************************************************************************
    * If paging is on, first prompt for `more...' if the end of the standard
    * output screen has been reached.
    **************************************************************************/
    for (;;) {
       /***********************************************************************
       * If paging on, check if the maximum number of lines have been written.
       ***********************************************************************/
       if (pagingOn) {
	 if (soLineCount == MAX_LINES_WHEN_PAGING) {
	   char key = NUL; static char prompt[] = "\nEnter RETURN for more...";
	   WriteStringStdOut (prompt, strlen(prompt));
	   ReadCharStdIn (&key);
	   if (key != '\n') pagingOn = FALSE;
	   WriteStringStdOut ("\n\n", 2);
	   soLineCount = 0;
	 }
       }
       /***********************************************************************
       * Write the string up to the next newline character.  If there aren't
       * any(more) newline characters, write the rest of the string.
       ***********************************************************************/
       ptrNL = strchr (ptr, Nl);
       if (ptrNL == NULL) {
	 len = strlen (ptr);
	 WriteStringStdOut (ptr, len);
	 break;
       }
       else {
	 len = (int) (ptrNL - ptr + 1);
	 WriteStringStdOut (ptr, len);
	 if (pagingOn) soLineCount++;
	 ptr = ptrNL + 1;
	 if (*ptr == NUL) break;
       }
    }
    fflush (stdout);
#endif
  }
  return strlen(text);
}

/******************************************************************************
* DirList.
* If an error occurs (or no files found), zero (0) is returned.
******************************************************************************/

int DirList (dir, patternC, patternS, dirS, nameS)
char *dir;              /* In: Directory to search. */
int patternC;           /* In: Pattern count. */
char **patternS;        /* In: Patterns to search for. */
char ***dirS;           /* Out: Directories. */
char ***nameS;          /* Out: File names. */
{
int i;
int nFiles = 0;
int tNameChars = 0;
struct FILEstruct *FILEhead = NULL;
struct FILEstruct *FILEptr;
struct FILEstruct *FILEptrT;
char *dirP, *nameP;
size_t nNameChars;

#if defined(BORLANDC)
char pathX[DU_MAX_PATH_LEN + 1];
struct ffblk ffblk;
int done;
#endif

#if defined(MICROSOFTC)
char pathX[DU_MAX_PATH_LEN + 1];
struct find_t fileinfo;
int done;
#endif

#if defined(unix) || defined(posixSHELL)
DIR *dirp;
char dirX[DU_MAX_PATH_LEN + 1];
char pattern[DU_MAX_NAME_LEN+1];
#if defined(Mach)
struct direct *dp;
#else
struct dirent *dp;
#endif
#endif

#if defined(vms)
char pathX[DU_MAX_PATH_LEN + 1];
char pathFound[DU_MAX_PATH_LEN + 1];
struct dsc$descriptor_s pathDESCR;
struct dsc$descriptor_s pathFoundDESCR;
uLongx context = 0;
char name[DU_MAX_NAME_LEN + 1];
uLongx status;
#endif

#if defined(mac)
short vRefNum;
long dirID;
OSErr rCode;
CInfoPBRec cParms;
Str255 tempS;
char *name;
char pattern[DU_MAX_NAME_LEN+1];
#endif

#if defined(win32)
char pathX[DU_MAX_PATH_LEN + 1];
long handle;
struct _finddata_t fdata;
int done;
#endif

#if defined(win32)
  /****************************************************************************
  * Find files on Windows machine.  Note that only one directory is possible
  * per path on Windows systems.
  ****************************************************************************/
  for (i = 0; i < patternC; i++) {
     ExpandPath (dir, pathX);
     AppendToDir (pathX, patternS[i]);
     handle = _findfirst (pathX, &fdata);
     if (handle != -1) {
	done = FALSE;
        while (!done) {
          /***********************************************************************
          * Allocate/setup file structure.
          ***********************************************************************/
          FILEptr = (struct FILEstruct *) cdf_AllocateMemory ((size_t)sizeof(struct FILEstruct), FatalError);
          nNameChars = strlen (fdata.name);
          FILEptr->name = (char *) cdf_AllocateMemory ((size_t)nNameChars + 1, FatalError);
          strcpyX (FILEptr->name, fdata.name, nNameChars);
          tNameChars += nNameChars + 1;
          nFiles++;
          FILEptr->next = NULL;
          AddToList (&FILEhead, FILEptr);
          /***********************************************************************
          * Get next matching file.
          ***********************************************************************/
          done = _findnext (handle, &fdata);
        }
	_findclose (handle);
     }
   }
#endif

#if defined(dos)
  /****************************************************************************
  * Find files on MS-DOS machine.  Note that only one directory is possible
  * per path on MS-DOS systems.
  ****************************************************************************/
  for (i = 0; i < patternC; i++) {
     ExpandPath (dir, pathX);
     AppendToDir (pathX, patternS[i]);
#if defined(BORLANDC)
     done = findfirst (pathX, &ffblk, 0);
#endif
#if defined(MICROSOFTC)
     done = _dos_findfirst (pathX, 0, &fileinfo);
#endif
     while (!done) {
       /***********************************************************************
       * Allocate/setup file structure.
       ***********************************************************************/
       FILEptr = (struct FILEstruct *)
		 cdf_AllocateMemory ((size_t)sizeof(struct FILEstruct),
				 FatalError);
#if defined(BORLANDC)
       nNameChars = strlen (ffblk.ff_name);
       FILEptr->name = (char *) cdf_AllocateMemory ((size_t)nNameChars + 1, FatalError);
       strcpyX (FILEptr->name, ffblk.ff_name, nNameChars);
#endif
#if defined(MICROSOFTC)
       nNameChars = strlen (fileinfo.name);
       FILEptr->name = (char *) cdf_AllocateMemory ((size_t)nNameChars + 1, FatalError);
       strcpyX (FILEptr->name, fileinfo.name, nNameChars);
#endif
       tNameChars += nNameChars + 1;
       nFiles++;
       FILEptr->next = NULL;
       AddToList (&FILEhead, FILEptr);
       /***********************************************************************
       * Get next matching file.
       ***********************************************************************/
#if defined(BORLANDC)
       done = findnext (&ffblk);
#endif
#if defined(MICROSOFTC)
       done = _dos_findnext (&fileinfo);
#endif
     }
   }
#endif

#if defined(unix) || defined(posixSHELL)
  /****************************************************************************
  * Find files on UNIX machine.  Note that only one directory is possible per
  * path on UNIX systems.
  ****************************************************************************/
  ExpandPath (dir, dirX);
  if (NULstring(dirX)) strcpyX (dirX, ".", DU_MAX_PATH_LEN);
						  /* Default to current dir. */
  dirp = opendir (dirX);
  if (dirp == NULL) return 0;
  dp = readdir (dirp);
  while (dp != NULL) {
    char *name = dp->d_name;
#if SOLARISbsdDIRUTILSbug
    name -= 2;
#endif
    for (i = 0; i < patternC; i++) {
       strcpyX (pattern, patternS[i], DU_MAX_NAME_LEN);
       if (NULstring(pattern)) strcpyX (pattern, "*", DU_MAX_NAME_LEN);
						  /* Default to all files. */
       if (PatternMatch (pattern, name)) {
	 /*********************************************************************
	 * Allocate/setup file structure.
	 *********************************************************************/
	 FILEptr = (struct FILEstruct *)
		   cdf_AllocateMemory ((size_t)sizeof(struct FILEstruct),
				   FatalError);
	 nNameChars = strlen (name);
	 FILEptr->name = (char *) cdf_AllocateMemory ((size_t)nNameChars + 1, FatalError);
	 strcpyX (FILEptr->name, name, nNameChars);
	 tNameChars += nNameChars + 1;
	 nFiles++;
	 FILEptr->next = NULL;
	 AddToList (&FILEhead, FILEptr);
	 break;
       }
    }
    /**************************************************************************
    * Get next file.
    **************************************************************************/
    dp = readdir (dirp);
  }
  closedir (dirp);
#endif

#if defined(vms)
  /****************************************************************************
  * Find files on VMS machine.  Note that a logical name defined to be a set of
  * two or more directories may result in files from more than one directory
  * being found.  In that case, the logical name would be returned as the name
  * of the directory (the VMS file system handles this when opening a file).
  * The actual directories are ignored.
  ****************************************************************************/
  pathFoundDESCR.dsc$w_length = sizeof(pathFound) - 1; /* Pass full string. */
  pathFoundDESCR.dsc$b_dtype = DSC$K_DTYPE_T;
  pathFoundDESCR.dsc$b_class = DSC$K_CLASS_S;
  pathFoundDESCR.dsc$a_pointer = pathFound;
  for (i = 0; i < patternC; i++) {
     ExpandPath (dir, pathX);
     AppendToDir (pathX, patternS[i]);
     pathDESCR.dsc$w_length = strlen(pathX);            /* Pass part used. */
     pathDESCR.dsc$b_dtype = DSC$K_DTYPE_T;
     pathDESCR.dsc$b_class = DSC$K_CLASS_S;
     pathDESCR.dsc$a_pointer = pathX;
     while ((status = lib$find_file (&pathDESCR, &pathFoundDESCR, &context,
				     NULL, NULL, NULL, NULL)) == RMS$_NORMAL) {
       /***********************************************************************
       * A file was found, NUL terminate and remove trailing blanks (version
       * number is left on).
       ***********************************************************************/
       pathFound[pathFoundDESCR.dsc$w_length] = NUL;    /* NUL-terminate. */
       for (i = strlen(pathFound) - 1; i >= 0 && pathFound[i] == ' '; i--) {
	  pathFound[i] = NUL;
       }
       ParsePath (pathFound, NULL, name);
       /***********************************************************************
       * Allocate/setup file structure.
       ***********************************************************************/
       FILEptr = (struct FILEstruct *)
		 cdf_AllocateMemory ((size_t)sizeof(struct FILEstruct),
				 FatalError);
       nNameChars = strlen (name);
       FILEptr->name = (char *) cdf_AllocateMemory ((size_t)nNameChars + 1,
						FatalError);
       strcpyX (FILEptr->name, name, nNameChars);
       tNameChars += nNameChars + 1;
       nFiles++;
       FILEptr->next = NULL;
       AddToList (&FILEhead, FILEptr);
     }
     status = lib$find_file_end (&context);
  }
#endif

#if defined(mac)
  /****************************************************************************
  * Find files on a Macintosh.  Note that only one directory is possible per
  * path on Macintosh systems.
  ****************************************************************************/
  if (!MacDirSpecified(dir,&vRefNum,&dirID)) {
    *nameS = NULL;
    *dirS = NULL;
    return 0;
  }
  cParms.hFileInfo.ioNamePtr = tempS;
  cParms.hFileInfo.ioVRefNum = vRefNum;
  cParms.hFileInfo.ioFDirIndex = 1;
  cParms.hFileInfo.ioDirID = dirID;
  rCode = PBGetCatInfo (&cParms, FALSE);
  while (rCode == noErr) {
    /**************************************************************************
    * Check if a file (rather than a directory).
    **************************************************************************/
    if (BITCLR(cParms.hFileInfo.ioFlAttrib,4)) {
      name = PtoCstr (cParms.hFileInfo.ioNamePtr);
      for (i = 0; i < patternC; i++) {
	 strcpyX (pattern, patternS[i], DU_MAX_NAME_LEN);
	 if (NULstring(pattern)) strcpyX (pattern, "*", DU_MAX_NAME_LEN);
	 if (PatternMatch(pattern,name)) {
	   /*******************************************************************
	   * Allocate/setup file structure.
	   *******************************************************************/
	   FILEptr = (struct FILEstruct *)
		     cdf_AllocateMemory ((size_t)sizeof(struct FILEstruct),
				     FatalError);
	   nNameChars = strlen (name);
	   FILEptr->name = (char *) cdf_AllocateMemory ((size_t)nNameChars+1,
						    FatalError);
	   strcpyX (FILEptr->name, name, nNameChars);
	   tNameChars += nNameChars + 1;
	   nFiles++;
	   FILEptr->next = NULL;
	   AddToList (&FILEhead, FILEptr);
	   break;
	 }
      }
    }
    /**************************************************************************
    * Get next file.
    **************************************************************************/
    cParms.hFileInfo.ioDirID = dirID;    /* This must be reset each time. */
    cParms.hFileInfo.ioFDirIndex++;         /* Increment to next file. */
    rCode = PBGetCatInfo (&cParms, FALSE);
  }
#endif

  /****************************************************************************
  * Build directory/file arrays.
  ****************************************************************************/
  if (nFiles > 0) {
    /**************************************************************************
    * Setup to traverse linked list.
    **************************************************************************/
    *dirS = (char **) cdf_AllocateMemory ((size_t)(nFiles*sizeof(char *))+strlen(dir)+1,
				      FatalError);
    *nameS = (char **) cdf_AllocateMemory ((size_t)(nFiles*sizeof(char *)) + tNameChars,
				       FatalError);
    dirP = (char *) (*dirS) + nFiles*sizeof(char *);
    nameP = (char *) (*nameS) + nFiles*sizeof(char *);
    /**************************************************************************
    * Traverse linked list of file structures.  Note that the second through
    * last directory strings actually point to the first directory string.
    **************************************************************************/
    for (FILEptr=FILEhead, i=0; FILEptr != NULL; FILEptr=FILEptr->next, i++) {
       if (i == 0) {
	 strcpyX (dirP, dir, 0);
	 (*dirS)[i] = dirP;
       }
       else
	 (*dirS)[i] = (*dirS)[i-1];
       strcpyX (nameP, FILEptr->name, 0);
       (*nameS)[i] = nameP;
       nameP += strlen(FILEptr->name) + 1;
    }
    /**************************************************************************
    * Free linked list of file structures.
    **************************************************************************/
    FILEptr = FILEhead;
    while (FILEptr != NULL) {
      FILEptrT = FILEptr;
      FILEptr = FILEptr->next;
      cdf_FreeMemory (FILEptrT->name, FatalError);
      cdf_FreeMemory (FILEptrT, FatalError);
    }
  }
  else {
    /**************************************************************************
    * No files found.
    **************************************************************************/
    *nameS = NULL;
    *dirS = NULL;
  }
  return nFiles;
}

/******************************************************************************
* AddToList.
******************************************************************************/

static void AddToList (FILEhead, FILEptr)
struct FILEstruct **FILEhead;
struct FILEstruct *FILEptr;
{
  if (*FILEhead == NULL)
    *FILEhead = FILEptr;
  else
    if (strcmp(FILEptr->name,(*FILEhead)->name) < 0) {
      FILEptr->next = *FILEhead;
      *FILEhead = FILEptr;
    }
    else {
      struct FILEstruct *FILEptrT = *FILEhead;
      for (;;) {
	 if (FILEptrT->next == NULL) {
	   FILEptrT->next = FILEptr;
	   break;
	 }
	 else
	   if (strcmp(FILEptr->name,FILEptrT->next->name) < 0) {
	     FILEptr->next = FILEptrT->next;
	     FILEptrT->next = FILEptr;
	     break;
	   }
	 FILEptrT = FILEptrT->next;
      }
    }
  return;
}

/******************************************************************************
* PatternMatch. (Only used on UNIX/POSIXshell and Macintosh systems).
******************************************************************************/

#if defined(unix) || defined(mac) || defined(posixSHELL)
static int PatternMatch (pattern, name)
char *pattern;
char *name;
{
enum typeENUM types[DU_MAX_PATTERNS];
char *strings[DU_MAX_PATTERNS];
int nTypes = 0;
int len, i, result;

for (i = 0; i < DU_MAX_PATTERNS; i++) types[i] = TBD;

for (i = 0; pattern[i] != NUL; i++)
   switch (pattern[i]) {
     case '?':
       types[nTypes++] = WILD1;
       break;
     case '*':
       if (nTypes == 0 || types[nTypes] != WILDn) types[nTypes++] = WILDn;
       break;
     default:
       if (types[nTypes] == TBD) {
	 types[nTypes] = STR;
	 strings[nTypes] = (char *) cdf_AllocateMemory ((size_t)DU_MAX_NAME_LEN + 1,
						    FatalError);
	 strings[nTypes][0] = pattern[i];
	 strings[nTypes][1] = NUL;
	 nTypes++;
       }
       else {
	 len = strlen (strings[nTypes-1]);
	 strings[nTypes-1][len] = pattern[i];
	 strings[nTypes-1][len+1] = NUL;
       }
   }

result = CheckPattern (nTypes, types, strings, name);

for (i = 0; i < DU_MAX_PATTERNS; i++) {
   if (types[i] == STR) cdf_FreeMemory (strings[i], FatalError);
}

return result;
}
#endif

/******************************************************************************
* CheckPattern. (Only used on UNIX/POSIXshell and Macintosh systems).
******************************************************************************/

#if defined(unix) || defined(mac) || defined(posixSHELL)
static int CheckPattern (nTypes, types, strings, name)
int nTypes;
enum typeENUM types[];
char *strings[];
char *name;
{
int sp = 0;
int i, j;

for (i = 0; i < nTypes; i++) {
   switch (types[i]) {
     case TBD:
       /* Do nothing? */
       break;
     case STR:
       if (strncmp(&name[sp],strings[i],strlen(strings[i])) != 0)
	 return FALSE;
       else
	 sp += strlen(strings[i]);
       break;
     case WILD1:
       if (name[sp] == NUL)
	 return FALSE;
       else
	 sp++;
       break;
     case WILDn:
       if (i == nTypes-1)       /* At last type to check? */
	 return TRUE;
       else {
	 for (j = i; name[j] != NUL; j++)
	    if (CheckPattern (nTypes-i-1, &types[i+1],
			      &strings[i+1], &name[j])) return TRUE;
	 return FALSE;
       }
   }
}

if (name[sp] == NUL)
  return TRUE;
else
  return FALSE;
}
#endif

/******************************************************************************
* CDFdirList.
*    Does a `DirList' using the possible CDF extensions (`.cdf' on all
* platforms and also `.CDF', `.cdf;1', and `.CDF;1' on UNIX/POSIXshell and
* Macintosh platforms because of CD-ROM inconsistencies
******************************************************************************/

int CDFdirList (path, dirS, nameS)
char *path;
char ***dirS;
char ***nameS;
{
#if defined(unix) || defined(mac) || defined(posixSHELL)
  /****************************************************************************
  * UNIX/POSIXshell and Macintosh.
  ****************************************************************************/
  char dir[DU_MAX_DIR_LEN+1];
  char pattern[DU_MAX_NAME_LEN+1];
  static char pattern1[DU_MAX_NAME_LEN+1];
  static char pattern2[DU_MAX_NAME_LEN+1];
  static char pattern3[DU_MAX_NAME_LEN+1];
  static char pattern4[DU_MAX_NAME_LEN+1];
  static char *patternS[4] = { pattern1, pattern2, pattern3, pattern4 };
  int nCDFs;

  ParsePath (path, dir, pattern);

  strcpyX (pattern1, pattern, DU_MAX_NAME_LEN);
  if (NULstring(pattern1)) strcatX (pattern1, "*", DU_MAX_NAME_LEN);
  strcatX (pattern1, ".cdf", DU_MAX_NAME_LEN);

  strcpyX (pattern2, pattern, DU_MAX_NAME_LEN);
  if (NULstring(pattern2)) strcatX (pattern2, "*", DU_MAX_NAME_LEN);
  strcatX (pattern2, ".CDF", DU_MAX_NAME_LEN);

  strcpyX (pattern3, pattern, DU_MAX_NAME_LEN);
  if (NULstring(pattern3)) strcatX (pattern3, "*", DU_MAX_NAME_LEN);
  strcatX (pattern3, ".cdf;1", DU_MAX_NAME_LEN);

  strcpyX (pattern4, pattern, DU_MAX_NAME_LEN);
  if (NULstring(pattern4)) strcatX (pattern4, "*", DU_MAX_NAME_LEN);
  strcatX (pattern4, ".CDF;1", DU_MAX_NAME_LEN);

  nCDFs = DirList (dir, 4, patternS, dirS, nameS);
  RemoveExtensions (nCDFs, *nameS);

  return nCDFs;
#endif
#if defined(vms) || defined(dos) || defined(win32)
  /****************************************************************************
  * VMS, Windows, and MS-DOS.
  ****************************************************************************/
  char dir[DU_MAX_DIR_LEN+1];
  static char pattern[DU_MAX_NAME_LEN+1];
  static char *patternS[1] = { pattern };
  int nCDFs;

  ParsePath (path, dir, pattern);

  if (NULstring(pattern)) strcatX (pattern, "*", DU_MAX_NAME_LEN);
  strcatX (pattern, ".cdf", DU_MAX_NAME_LEN);

  nCDFs = DirList (dir, 1, patternS, dirS, nameS);
  RemoveExtensions (nCDFs, *nameS);

  return nCDFs;
#endif
}

/******************************************************************************
* RemoveExtensions.  Locates the last '.' in a file name and replaces it with
*                    a NUL.  This effectively removes the file extension.  If
*                    the file name does not have an extension and has a '.' in
*                    its name, this could have undesired effects.
******************************************************************************/

void RemoveExtensions (nFiles, files)
int nFiles;
char *files[];
{
  int i;
  char *ptr;
  for (i = 0; i < nFiles; i++)
     if ((ptr = strrchr(files[i],'.')) != NULL) *ptr = NUL;
  return;
}

/******************************************************************************
* WriteStringStdOut.
******************************************************************************/

void WriteStringStdOut (string, length)
char *string;
size_t length;
{
  fprintf (stdout, "%*.*s", (int) length, (int) length, string);
  return;
}

/******************************************************************************
* ReadCharStdIn.
******************************************************************************/

#if !defined(mac)
void ReadCharStdIn (key)
char *key;
{
  *key = (char) fgetc (stdin);
  return;
}
#endif

/******************************************************************************
* DisplayStatistics.
******************************************************************************/

void DisplayStatistics (label, vStatsDotCDF, vStatsStage, vStatsCompress)
char *label;
vSTATS *vStatsDotCDF;
vSTATS *vStatsStage;
vSTATS *vStatsCompress;
{
  char temp1[MAX_SCREENLINE_LEN+1], temp2[MAX_SCREENLINE_LEN+1],
       temp3[MAX_SCREENLINE_LEN+1], labelX[MAX_SCREENLINE_LEN+1];
  if (vStatsDotCDF->maxBuffers > 0) {
    strcpyX (labelX, label, MAX_SCREENLINE_LEN);
    strcatX (labelX, " DotCDF file", MAX_SCREENLINE_LEN);
    BuildStatistics (labelX, vStatsDotCDF, temp1, temp2, temp3);
    WriteOut (stdout, temp1);
    WriteOut (stdout, "\n");
    WriteOut (stdout, temp2);
    WriteOut (stdout, "\n");
    WriteOut (stdout, temp3);
    WriteOut (stdout, "\n");
  }
  if (vStatsStage->maxBuffers > 0) {
    strcpyX (labelX, label, MAX_SCREENLINE_LEN);
    strcatX (labelX, " staging file", MAX_SCREENLINE_LEN);
    BuildStatistics (labelX, vStatsStage, temp1, temp2, temp3);
    WriteOut (stdout, temp1);
    WriteOut (stdout, "\n");
    WriteOut (stdout, temp2);
    WriteOut (stdout, "\n");
    WriteOut (stdout, temp3);
    WriteOut (stdout, "\n");
  }
  if (vStatsCompress->maxBuffers > 0) {
    strcpyX (labelX, label, MAX_SCREENLINE_LEN);
    strcatX (labelX, " compression scratch file", MAX_SCREENLINE_LEN);
    BuildStatistics (labelX, vStatsCompress, temp1, temp2, temp3);
    WriteOut (stdout, temp1);
    WriteOut (stdout, "\n");
    WriteOut (stdout, temp2);
    WriteOut (stdout, "\n");
    WriteOut (stdout, temp3);
    WriteOut (stdout, "\n");
  }
  return;
}

/******************************************************************************
* BuildStatistics.
******************************************************************************/

void BuildStatistics (label, vStats, temp1, temp2, temp3)
char *label;
vSTATS *vStats;
char temp1[MAX_SCREENLINE_LEN+1];
char temp2[MAX_SCREENLINE_LEN+1];
char temp3[MAX_SCREENLINE_LEN+1];
{
  strcpyX (temp1, "  Statistics", MAX_SCREENLINE_LEN);
  if (label != NULL) {
    strcatX (temp1, " for ", MAX_SCREENLINE_LEN);
    strcatX (temp1, label, MAX_SCREENLINE_LEN);
  }
  strcatX (temp1, ":", MAX_SCREENLINE_LEN);
  snprintf (temp2, (size_t) (MAX_SCREENLINE_LEN+1),
	    "    %d buffer%s used of the %d allowed, ", vStats->nBuffers,
	    BOO(vStats->nBuffers <= 1,"","s"), vStats->maxBuffers);
  snprintf (EofS(temp2), (size_t) (MAX_SCREENLINE_LEN+1-strlen(temp2)),
	    "[%ld reads, %ld writes]  ", vStats->nV_reads,
	    vStats->nV_writes);
  snprintf (temp3, (size_t) (MAX_SCREENLINE_LEN+1),
	    "    %ld page in%s, %ld page out%s, ",
	    vStats->nPageIns, BOO(vStats->nPageIns<=1,"","s"),
	    vStats->nPageOuts, BOO(vStats->nPageOuts<=1,"","s"));
  snprintf (EofS(temp3), (size_t) (MAX_SCREENLINE_LEN+1-strlen(temp3)),
	    "%ld block read%s, %ld block write%s  ",
	    vStats->nBlockReads, BOO(vStats->nBlockReads<=1,"","s"),
	    vStats->nBlockWrites, BOO(vStats->nBlockWrites<=1,"","s"));
  return;
}

/******************************************************************************
* StatusCodeReportOptions.
******************************************************************************/

char *StatusCodeReportOptions (e,w,i)
Logical e;
Logical w;
Logical i;
{
  static char mask[MAX_REPORTMASK_LEN+1];
#if defined(vms)
  strcpyX (mask, "(", MAX_REPORTMASK_LEN);
#else
  strcpyX (mask, "\"", MAX_REPORTMASK_LEN);
#endif
  strcatX (mask, (e ? "errors" : ""), MAX_REPORTMASK_LEN);
  strcatX (mask, (w ? (e ? ",warnings" : "warnings") : ""),
	   MAX_REPORTMASK_LEN);
  strcatX (mask, (i ? (e || w ? ",informationals" : "informationals") : ""),
	   MAX_REPORTMASK_LEN);
#if defined(vms)
  strcatX (mask, ")", MAX_REPORTMASK_LEN);
#else
  strcatX (mask, "\"", MAX_REPORTMASK_LEN);
#endif
#if defined(vms) || defined(dos)
  MakeUpperString (mask);
#endif
  return mask;
}

/******************************************************************************
* DisplayIdentification.
******************************************************************************/

void DisplayIdentification (name)
char *name;
{
  long version, release, increment; char subIncrement;
  char text[MAX_MESSAGE_TEXT_LEN+1]; CDFstatus status;
  status = CDFlib (GET_, LIB_VERSION_, &version,
			 LIB_RELEASE_, &release,
			 LIB_INCREMENT_, &increment,
			 LIB_subINCREMENT_, &subIncrement,
		   NULL_);
  if (StatusBAD(status))
    snprintf (text, (size_t) sizeof(text), "Error inquiring CDF library.");
  else {
    snprintf (text, (size_t) sizeof(text), "%s from the CDF V%ld.%ld.%ld",
	      name, version, release, increment);
    if (subIncrement != ' ') 
      snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	        "_%c", subIncrement);
    snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	      " distribution.");
  }
  DisplayInfo (text);
  return;
}

/******************************************************************************
* FindUniqueMatch.
* Returns: NOMATCH if a match was not found,
*	   MATCHES if more than one match,
*	   `index' if only one match.
******************************************************************************/

int FindUniqueMatch (target, strings)
char *target;		/* Searching for this string. */
char *strings[];	/* Terminated by a NULL pointer. */
{
  int match, beyond; size_t targetL = strlen(target);
  for (match = 0; strings[match] != NULL; match++) {
     if (strncmpIgCasePattern(target,strings[match],targetL) == 0) {
       for (beyond = match + 1; strings[beyond] != NULL; beyond++) {
	  if (strncmpIgCasePattern(target,strings[beyond],targetL) == 0)
	    return MATCHES;
       }
       return match;
     }
  }
  return NOMATCH;
}

/******************************************************************************
* RemoveLeading0.
* Change ...e+0.. (3 numbers after +/-) from win32 to ...e+.. (2 numbers
* after +/-) just like other systems
******************************************************************************/

void RemoveLeading0 (text, width)
char *text;		/* String to work on... */
size_t width;		/* Width of the string */
{
  if (strchr(text, 'e') != NULL || strchr(text, 'E') != NULL) {
      int ilen = (int) strlen(text);
      char *ie;
      char *to0;
      ie = strchr(text, 'e');
      if (ie == NULL) ie = strchr(text, 'E');
      to0 = strstr(ie, "+0");
      if (to0 == NULL) to0 = strstr(ie, "-0");
      if (to0 != NULL) {
        if (strlen(ie) == 5) {
          char *newText;
          int itrail;
          newText = (char *) malloc(ilen);
          itrail = (int) strlen(ie);
          memmove(newText, text, ilen-itrail+2);
          memmove(newText+ilen-itrail+2, text+ilen-itrail+3,
                  itrail-3);
          newText[ilen-1] = NUL;
          strcpyX (text, newText, width-1);
          free (newText);
        }
      }
  }
}

/******************************************************************************
* ISTPname.
* Verify a name (attribute or variable) follows ISTP guidelines that it has a
* leading letter and then followed by letter(s), number(s) and underscore(s).
* Returns 1 if it is a valid one, 0 otherwise.
******************************************************************************/

int ISTPname (name)
char *name;		/* String to work on... */
{
  size_t len;
  int i, val;
  if (name == NULL) return 0;
  len = strlen (name);
  for (i = 0; i < (int) len; ++i) {
    val = (char) *(name+i);
    if (i == 0) {
      if ((val >= 65 && val <= 90) || (val >= 97 && val <= 122)) continue;
      else return 0;
    } else
      if ((val >= 48 && val <= 57) || (val >= 65 && val <= 90) ||
          (val >= 97 && val <= 122) || val == 95) continue;
      return 0;
  }
  return 1;
}

/******************************************************************************
* MergeSort.
* Use merge sort algorithm to sort an array of CDF typed data.
******************************************************************************/

void MergeSort (long dataType, void *buf, void *tmp, int left, int right)
{
	if ( left < right )
	{
		int center = (left + right) / 2;
		MergeSort (dataType, buf, tmp, left, center);
		MergeSort (dataType, buf, tmp, center + 1, right);
		Merge (dataType, buf, tmp, left, center + 1, right);
	}
}


static void Merge (long dataType, void *buf, void *tmp, int left, int right,
                   int rightEnd )
{
       int cnt;
       size_t typeSize;
       int leftEnd = right - 1;
       int k = left;
       int num = rightEnd - left + 1;

       typeSize = (size_t) CDFelemSize (dataType);
       while (left <= leftEnd && right <= rightEnd) {
           if (((dataType == CDF_EPOCH || dataType == CDF_REAL8 ||
                 dataType == CDF_DOUBLE) &&
                (*(((double *)buf)+left) <= *(((double *)buf)+right))) ||
               ((dataType == CDF_EPOCH16) &&
                (*(((double *)buf)+2*left) < *(((double *)buf)+2*right) ||
                 (*(((double *)buf)+2*left) == *(((double *)buf)+2*right) &&
                  *(((double *)buf)+2*left+1) <= *(((double *)buf)+2*right+1)))) ||
               ((dataType == CDF_TIME_TT2000 || dataType == CDF_INT8) &&
                (*(((long long *)buf)+left) <=
                 *(((long long *)buf)+right))) ||
               ((dataType == CDF_INT1 || dataType == CDF_BYTE) &&
                (*(((sChar *)buf)+left) <= *(((sChar *)buf)+right))) ||
               ((dataType == CDF_UINT1) && 
                (*(((uChar *)buf)+left) <= *(((uChar *)buf)+right))) ||
               ((dataType == CDF_INT2) && 
                (*(((short *)buf)+left) <= *(((short *)buf)+right))) ||
               ((dataType == CDF_UINT2) &&
                (*(((unsigned short *)buf)+left) <=
                 *(((unsigned short *)buf)+right))) ||
               ((dataType == CDF_INT4) && 
                (*(((int *)buf)+left) <= *(((int *)buf)+right))) ||
               ((dataType == CDF_UINT4) &&
                (*(((unsigned int *)buf)+left) <=
                 *(((unsigned int *)buf)+right))) ||
               ((dataType == CDF_REAL4 || dataType == CDF_FLOAT) &&
                (*(((float *)buf)+left) <= *(((float *)buf)+right)))) {
                memcpy ((char *)tmp+typeSize*k,
                        (char *)buf+typeSize*left, typeSize);
                ++k; ++left;
           } else {
             memcpy ((char *)tmp+typeSize*k,
                     (char *)buf+typeSize*right, typeSize);
             ++k; ++right;
           }
       }
       if (left <= leftEnd) { /* Copy rest of first half */
         cnt = leftEnd - left + 1;
         memcpy ((char *)tmp+typeSize*k, (char *)buf+typeSize*left,
                 typeSize*cnt);
         k += cnt;
       }
       if (right <= rightEnd) { /* Copy rest of right half */
         cnt = rightEnd - right + 1;
         memcpy ((char *)tmp+typeSize*k, (char *)buf+typeSize*right,
                 typeSize*cnt);
         k += cnt;
       }
       /* copy tmp back */
       memcpy ((char *)buf+typeSize*(rightEnd-num+1), 
               (char *)tmp+typeSize*(rightEnd-num+1), typeSize*num);

}

/******************************************************************************
* BinarySearch.
* Use binary search algorithm to search and return the index for the
* given data value in an sorted array of a CDF type.
* It returns the index, or -1 if invalid range.
******************************************************************************/

int BinarySearch (long dataType, void *values, void *value, int min, int max)
{
      int mid;
      mid = (min + max) / 2;
      if (mid < 0) return -1; 
      if (((dataType == CDF_EPOCH || dataType == CDF_DOUBLE ||
            dataType == CDF_REAL8) &&
           (*(((double *)values)+mid) > *(double *) value)) ||
          ((dataType == CDF_EPOCH16) &&
           ((*(((double *)values)+2*mid) > *(double *) value) ||
            ((*(((double *)values)+2*mid) == *(double *) value) &&
             (*(((double *)values)+2*mid+1) > *(((double *) value)+1))))) ||
          ((dataType == CDF_TIME_TT2000 || dataType == CDF_INT8) &&
           (*(((long long *)values)+mid) > *(long long *) value)) ||
          ((dataType == CDF_INT1 || dataType == CDF_BYTE) &&
           (*(((sChar *)values)+mid) > *(sChar *) value)) ||
          ((dataType == CDF_UINT1) &&
           (*(((uChar *)values)+mid) > *(uChar *) value)) ||
          ((dataType == CDF_INT2) &&
           (*(((short *)values)+mid) > *(short *) value)) ||
          ((dataType == CDF_UINT2) &&
           (*(((unsigned short *)values)+mid) > *(unsigned short *) value)) ||
          ((dataType == CDF_INT4) &&
           (*(((int *)values)+mid) > *(int *) value)) ||
          ((dataType == CDF_UINT4) &&
           (*(((unsigned int *)values)+mid) > *(unsigned int *) value)) ||
          ((dataType == CDF_REAL4 || dataType == CDF_FLOAT) &&
           (*(((float *)values)+mid) > *(float *) value)))
          /* value is in lower subset */
          return BinarySearch (dataType, values, value, min, mid-1);
      else 
        if (((dataType == CDF_EPOCH || dataType == CDF_DOUBLE ||
              dataType == CDF_REAL8) &&
             (*(((double *)values)+mid) < *(double *) value)) ||
            ((dataType == CDF_EPOCH16) &&
             ((*(((double *)values)+2*mid) < *(double *) value) ||
              ((*(((double *)values)+2*mid) == *(double *) value) &&
               (*(((double *)values)+2*mid+1) < *(((double *) value)+1))))) ||
            ((dataType == CDF_TIME_TT2000 || dataType == CDF_INT8) &&
             (*(((long long *)values)+mid) < *(long long *) value)) ||
            ((dataType == CDF_INT1 || dataType == CDF_BYTE) &&
             (*(((sChar *)values)+mid) < *(sChar *) value)) ||
            ((dataType == CDF_UINT1) &&
             (*(((uChar *)values)+mid) < *(uChar *) value)) ||
            ((dataType == CDF_INT2) &&
             (*(((short *)values)+mid) < *(short *) value)) ||
            ((dataType == CDF_UINT2) &&
             (*(((unsigned short *)values)+mid) < *(unsigned short *) value)) ||
            ((dataType == CDF_INT4) &&
             (*(((int *)values)+mid) < *(int *) value)) ||
            ((dataType == CDF_UINT4) &&
             (*(((unsigned int *)values)+mid) < *(unsigned int *) value)) ||
            ((dataType == CDF_REAL4 || dataType == CDF_FLOAT) &&
             (*(((float *)values)+mid) < *(float *) value)))
          /* value is in upper subset */
          return BinarySearch (dataType, values, value, mid+1, max);
      else
          /* value has been found */
          return mid;
}

/******************************************************************************
* DisplayCDFdata.
******************************************************************************/

void DisplayCDFdata (dataType, numElems, data)
long dataType;
long numElems;
void *data;
{
 if (CDFstringDataType(dataType)) {
   char *buf;
   buf = malloc(numElems+1);
   memcpy (buf, data, numElems);
   *(buf+numElems) = '\0';
   printf("\"%s\" ",buf);
   free (buf);
 } else if (CDFintDataType(dataType)) {
    if (dataType == CDF_INT1)
        printf("%d ", (int)*(char *)data);
    if (dataType == CDF_UINT1)
        printf("%d ", (int)*(unsigned char *)data);
    if (dataType == CDF_INT2)
        printf("%hd ", *(short *)data);
    if (dataType == CDF_UINT2)
        printf("%hu ", *(unsigned short *)data);
    if (dataType == CDF_INT4)
        printf("%d ", *(int *)data);
    if (dataType == CDF_UINT4)
        printf("%u ", *(unsigned int *)data);
    if (dataType == CDF_INT8)
#if defined(win32)
        printf("%I64d ", *(long long *)data);
#else
        printf("%lld ", *(long long *)data);
#endif
 } else if (CDFepochDataType(dataType)) {
    if (dataType == CDF_EPOCH) {
      char epString[EPOCH_STRING_LEN+1];
      encodeEPOCH(*(double *)data, epString);
      printf("  %s ",epString);
    } else if (dataType == CDF_TIME_TT2000) {
      char tt2000[TT2000_3_STRING_LEN+1];
      encodeTT2000(*(long long *)data, tt2000);
      printf("  %s ",tt2000);
    } else {
      char epString16[EPOCH16_STRING_LEN+1];
      encodeEPOCH16((double *)data, epString16);
      printf("  %s ",epString16);
    }
 } else {
   if (dataType == CDF_REAL4 || dataType == CDF_FLOAT)
     printf("%g ", *(float *)data);
   else
     printf("%g ", *(double *)data);
 }
}

