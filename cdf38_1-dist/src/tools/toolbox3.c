/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF              Toolbox of routines for CDF Toolkit (Miscellaneous).
*
*  Version 1.6b, 3-Mar-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  25-Jan-94, J Love     Original version.
*   V1.0a  6-Feb-94, J Love     DEC Alpha/OpenVMS port.
*   V1.1  10-Mar-94, J Love     Modified `ParseOptionList' to make the
*                               enclosing parentheses optional (on any
*                               operating system).
*   V1.2  13-Dec-94, J Love     CDF V2.5.
*   V1.2a 13-Jan-95, J Love     Changed `IsCDF' to allow all possible
*                               extensions on Macintosh machines also.
*   V1.2b 20-Jan-95, J Love     IRIX 6.x (64-bit).
*   V1.2c  3-Mar-95, J Love     Moved `EncodeKeyDefinitions', etc. to
*                               `windoz.c'.
*   V1.3  21-Mar-95, J Love     POSIX.
*   V1.3a 18-Apr-95, J Love     More POSIX.
*   V1.4  22-May-95, J Love     `ASSIGNx', `EQx', etc. moved here (and some
*                               calling sequences changed).
*   V1.5  14-Sep-95, J Love     Added routines used by CDFexport.  Hyper
*                               groups.
*   V1.5a 19-Sep-95, J Love     Macintosh event handling.
*   V1.6   9-Sep-96, J Love     CDF V2.6.
*   V1.6a 21-Feb-97, J Love	Removed RICE.
*   V1.6b  3-Mar-97, J Love	Windows NT for MS Visual C/C++ on an IBM PC.
*   V1.6c 18-Dec-98, M Liu      Add the missing cases for ALPHAVMSi in
*                               WhichEncoding and EncodingToken.
*   V1.7  05-May-04, M Liu      Changed function AllocateBuffers when the
*                               record count is 1 or more.
*   V1.8  11-Jul-05, M Liu      Added MingW port for PC.
*   V1.9  13-Oct-06, M Liu      Changed to allow upper and lower case CDF  
*                               name to be used on win32.
*   V2.0  21-Oct-08, M Liu      Modified RemoveTrailingBlanks to end process
*                               when the first non-blank is found.
*   V3.3  10-Jan-09, M Liu      Validate a file before it is used.
*   V3.3a 10-Jan-10, M Liu      Added functions: ValidateQual, 
*                               InitHyperParmsFromAnywhere and 
*                               ParseRecordRange.
*   V3.4  24-Feb-14, M Liu      Handled nan and inf floating point values.
*
******************************************************************************/

#define _XOPEN_SOURCE 700
#define _XOPEN_SOURCE_EXTENDED

#include <stdio.h>
#include <wchar.h>
#include <math.h>
#include "cdflib.h"
#include "cdftools.h"

/******************************************************************************
* Macros.
******************************************************************************/

#if defined(dos)
#define MAXnBYTESperALLOCATION  50000L  /* Remember 64Kb limit. */
#else
#if defined(mac)
#define MAXnBYTESperALLOCATION  50000L
#else
#define MAXnBYTESperALLOCATION  8000000L
#endif
#endif

struct interval {
  int first;
  int last;
};

/******************************************************************************
* Local function prototypes.
******************************************************************************/

static Logical EqStringsIgLTws PROTOARGs((char *string1, char *string2));
static Logical DecodeInt32 PROTOARGs((char *, int, Int32 *));
static Logical DecodeInt64 PROTOARGs((char *, int, Int64 *));
static Logical DecodeInt32u PROTOARGs((char *, int, uInt32 *));
static char *strnchr PROTOARGs((char *string, int chr, int n));
static QOP *ParseSlashStyle PROTOARGs((
  int argc, char *argv[], char *validQuals[], int optRequired[]
));
static QOP *ParseHyphenStyle PROTOARGs((
  int argc, char *argv[], char *validQuals[], int optRequired[]
));
static void FreeArgvT PROTOARGs((int lastArgN, char *argvT[]));
static QOP *BuildQOP PROTOARGs((
  int Nparms, char *parms[], char *validQuals[], int *qualEntered,
  char *qualOpt[]
));
static Logical AttemptAllocations PROTOARGs((
  long nScalarValues, int nScalars, long nHyperValues, int nHypers,
  Byte1 **handles[], size_t nValueBytes[]
));
static int bisearch (wchar_t, const struct interval *, int);

/******************************************************************************
* EncodeRecordJustify.
******************************************************************************/

void EncodeRecordJustify (string, recN, minWidth, width)
char *string;
long recN;
int minWidth;   /* If zero, no minimum width.  If positive, right justified.
		   If negative, left justified. */
size_t  width;
{
  snprintf (string, width, "%ld", recN + 1);
  Justify (string, minWidth);
  return;
}

/******************************************************************************
* EncodeIndicesJustify.
******************************************************************************/

void EncodeIndicesJustify (string, numDims, indices, minWidth, width)
char *string;
long numDims;
long indices[];
int minWidth;   /* If zero, no minimum width.  If positive, right justified.
		   If negative, left justified. */
size_t  width;
{
  int n;
  strcpyX (string, "[", 0);
  for (n = 0; n < numDims; n++) {
     if (n > 0) strcatX (string, ",", 0);
     snprintf (EofS(string), width-strlen(string), "%ld",
	       indices[n] + 1);
  }
  strcatX (string, "]", 0);
  Justify (string, minWidth);
  return;
}

/******************************************************************************
* EncodeRecordIndicesJustify.
******************************************************************************/

void EncodeRecordIndicesJustify (string, recN, numDims, indices, minWidth, 
				 width)
char *string;
long recN;
long numDims;
long indices[];
int minWidth;   /* If zero, no minimum width.  If positive, right justified.
		   If negative, left justified. */
size_t  width;
{
  EncodeRecordJustify (string, recN, 0, width);
  EncodeIndicesJustify (EofS(string), numDims, indices, 0,
		        width-strlen(string));
  Justify (string, minWidth);
  return;
}

/******************************************************************************
* Justify.
******************************************************************************/

void Justify (string, minWidth)
char *string;
int minWidth;   /* If zero, no minimum width.  If positive, right justified.
		   If negative, left justified. */
{
  if (minWidth < 0) {
    int pad = -minWidth - strlen(string);
    if (pad > 0) CatNcharacters (string, pad, (int) ' ');
  }
  else {
    if (minWidth > 0) {
      int i, stringL = strlen(string);
      int shift = minWidth - stringL;
      if (shift > 0) {
	memmove (&(string[shift]), string, stringL + 1);
	for (i = 0; i < shift; i++) string[i] = ' ';
      }
    }
  }
  return;
}

/******************************************************************************
* CatString.
******************************************************************************/

void CatToString (string, cat, length, justify, more)
char *string;
char *cat;
int length;
int justify;
char *more;
{
  int catL = (int) strlen (cat);
  int moreL = (int) strlen (more);
  if (catL > length)
    if (moreL >= length)
      CatNcharacters (string, length, '*');
    else {
      strcatX (string, cat, strlen(string) + (length - moreL));
      strcatX (string, more, 0);
    }
  else
    switch (justify) {
      case LEFT_JUSTIFY:
	strcatX (string, cat, 0);
	CatNcharacters (string, length - catL, ' ');
	break;
      case CENTER_JUSTIFY: {
	int after = (length - catL) / 2;
	int before = (length - catL) - after;
	CatNcharacters (string, before, ' ');
	strcatX (string, cat, 0);
	CatNcharacters (string, after, ' ');
	break;
      }
      case RIGHT_JUSTIFY:
	CatNcharacters (string, length - catL, ' ');
	strcatX (string, cat, 0);
	break;
    }
  return;
}

/******************************************************************************
* RemoveLeadingBlanks.
******************************************************************************/

void RemoveLeadingBlanks (string)
char *string;
{
  while (string[0] == ' ') memmove (string, &string[1], strlen(string));
  return;
}

/******************************************************************************
* RemoveTrailingBlanks.
******************************************************************************/

void RemoveTrailingBlanks (string)
char *string;
{
  int i;
  for (i = strlen(string) - 1; i >= 0; i--) {
     if (string[i] != ' ') return;
     string[i] = NUL;
  }
  return;
}

/******************************************************************************
* RemoveWhiteSpace.
******************************************************************************/

void RemoveWhiteSpace (string)
char *string;
{
  char *c = string;
  while (*c != NUL) {
    if (Spacing(*c))
      memmove (c, c + 1, strlen(c+1) + 1);
    else
      c++;
  }
  return;
}

/******************************************************************************
* CopyToField.
*    Copies a string to a field and blank pads to the end of the field.
******************************************************************************/

void CopyToField (string, field, length)
char *string;
char *field;
int length;
{
  strcpyX (field, string, length);
  BlankPadRight (field, length - strlen(string));
  return;
}

/******************************************************************************
* CopyFromField.
*    Copies from a field to a string and removes trailing blanks.  It is
* assumed that the string is large enough.
******************************************************************************/

void CopyFromField (field, length, string)
char *field;
int length;
char *string;
{
  strcpyX (string, field, length);
  RemoveTrailingBlanks (string);
  return;
}

/******************************************************************************
* SystemClock.
******************************************************************************/

double SystemClock () {
  return ((double) time(NULL));
}

/******************************************************************************
* ParsePath.
******************************************************************************/

void ParsePath (path, dir, name)
char *path;
char dir[DU_MAX_DIR_LEN+1];
char name[DU_MAX_NAME_LEN+1];
{
  char *cp;
  size_t len;
  char dirT[DU_MAX_DIR_LEN+1];
  char nameT[DU_MAX_NAME_LEN+1];
#if defined(unix) || defined(posixSHELL)
  if (IsDir(path)) {
    strcpyX (dirT, path, DU_MAX_DIR_LEN);
    nameT[0] = NUL;
  }
  else {
    cp = strrchr (path, '/');
    if (cp == NULL) {
      dirT[0] = NUL;
      strcpyX (nameT, path, DU_MAX_NAME_LEN);
    }
    else {
      len = cp - path;                          /* No trailing '/'. */
      strcpyX (dirT, path, MINIMUM(len,DU_MAX_DIR_LEN));
      strcpyX (nameT, cp + 1, DU_MAX_NAME_LEN);
    }
  }
#endif
#if defined(dos) || defined(win32)
  if (IsDir(path)) {
    strcpyX (dirT, path, DU_MAX_DIR_LEN);
    nameT[0] = NUL;
  }
  else {
    char *cp1 = strrchr (path, ':');
    char *cp2 = strrchr (path, '\\');
    cp = (cp1 > cp2 ? cp1 : cp2);
    if (cp == NULL) {
      dirT[0] = NUL;
      strcpyX (nameT, path, DU_MAX_NAME_LEN);
    }
    else {
      if (cp1 == NULL) {
	len = (size_t) (cp2 - path);            /* No trailing '\'. */
	strcpyX (dirT, path, MINIMUM(len,DU_MAX_DIR_LEN));
      }
      else {
	if (cp2 == NULL) {
	  len = (size_t) (cp1 - path + 1);      /* Trailing ':'. */
	  strcpyX (dirT, path, MINIMUM(len,DU_MAX_DIR_LEN));
	}
	else {
	  if (cp1 + 1 == cp2)
	    len = (size_t) (cp2 - path + 1);    /* Trailing ':'. */
	  else
	    len = (size_t) (cp2 - path);        /* No trailing ':'. */
	  strcpyX (dirT, path, MINIMUM(len,DU_MAX_DIR_LEN));
	}
      }
      strcpyX (nameT, cp + 1, DU_MAX_NAME_LEN);
    }
  }
#endif
#if defined(vms)
  char *cp1 = strrchr (path, ':');
  char *cp2 = strrchr (path, ']');
  cp = (cp1 > cp2 ? cp1 : cp2);
  if (cp == NULL) {
    dirT[0] = NUL;
    strcpyX (nameT, path, DU_MAX_NAME_LEN);
  }
  else {
    len = cp - path + 1;                        /* Trailing ':' or ']' */
    strcpyX (dirT, path, MINIMUM(len,DU_MAX_DIR_LEN));
    strcpyX (nameT, cp + 1, DU_MAX_NAME_LEN);
  }
#endif
#if defined(mac)
  if (IsDir(path)) {
    strcpyX (dirT, path, DU_MAX_DIR_LEN);
    nameT[0] = NUL;
  }
  else {
    cp = strrchr (path, ':');
    if (cp == NULL) {
      dirT[0] = NUL;
      strcpyX (nameT, path, DU_MAX_NAME_LEN);
    }
    else {
      len = cp - path + 1;                      /* Trailing ':'. */
      strcpyX (dirT, path, MINIMUM(len,DU_MAX_DIR_LEN));
      strcpyX (nameT, cp + 1, DU_MAX_NAME_LEN);
    }
  }
#endif
  if (dir != NULL) strcpyX (dir, dirT, DU_MAX_DIR_LEN);
  if (name != NULL) strcpyX (name, nameT, DU_MAX_NAME_LEN);
  return;
}

/******************************************************************************
* IsDir (directory).
* `stat' on MS-DOS systems will indicate a directory if the pathname
* contains a wildcard character.
******************************************************************************/

Logical IsDir (path)
char *path;
{
  char pathX[DU_MAX_PATH_LEN+1];
  size_t len;
#if !defined(unix) && !defined(posixSHELL)
  int lastChar;
#endif
#if defined(unix) || defined(dos) || defined(posixSHELL) || defined(win32)
  struct stat st;
#endif
  ExpandPath (path, pathX);
  len = strlen (pathX);
  if (len == 0) return FALSE;
#if !defined(unix) && !defined(posixSHELL)
  lastChar = len - 1;
#endif
#if defined(unix) || defined(dos) || defined(posixSHELL) || defined(win32)
  /****************************************************************************
  * UNIX/POSIXshell, DOS, and Windows.
  ****************************************************************************/
#if defined(__MINGW32__)
  if (pathX[len-1] == '/') {
    pathX[len-1] = NUL;         /* Strip trailing '/' */
    len--;
  }
#endif
#if defined(dos) || defined(win32)
  /****************************************************************************
  * DOS/Windows: Check for wildcard character in the pathname.
  ****************************************************************************/
  if (strchr(pathX,'*') != NULL) return FALSE;
  if (strchr(pathX,'?') != NULL) return FALSE;
  /****************************************************************************
  * DOS/Windows: Strip trailing backslash (if present).
  ****************************************************************************/
  if (pathX[lastChar] == '\\') {
    pathX[lastChar] = NUL;         /* Strip trailing '\' */
    len--;
  }
  /****************************************************************************
  * DOS/Windows: Check for disk drive specification.
  ****************************************************************************/
  if (len == 2)
    if (Alphabetic(pathX[0]) && pathX[1] == ':') return TRUE;
#endif
#if defined(unix) || defined(posixSHELL)
  /****************************************************************************
  * UNIX: Check for wildcard character in the pathname.
  ****************************************************************************/
  if (strchr(pathX,'*') != NULL) return FALSE;
  if (strchr(pathX,'?') != NULL) return FALSE;
#endif
  /****************************************************************************
  * Call `stat' to determine type of file.
  ****************************************************************************/
  if (stat(pathX, &st) == 0) {
#if defined(SALFORDC)           /* Salford's `stat' is broken. */
    return (st.st_size == 0);
#else
#if defined(S_ISDIR)
    return S_ISDIR(st.st_mode);
#else
    return (st.st_mode & S_IFDIR);
#endif
#endif
  }
  else
    return FALSE;
#endif
#if defined(vms)
  /****************************************************************************
  * VMS.
  ****************************************************************************/
  /****************************************************************************
  * Check for wildcard character in the pathname.
  ****************************************************************************/
  if (strchr(pathX,'*') != NULL) return FALSE;
  if (strchr(pathX,'%') != NULL) return FALSE;
  /****************************************************************************
  * Check for possible logical name defined to be a full file specification
  * (not just a directory).
  ****************************************************************************/
  if (IsReg(pathX))
    return FALSE;
  else
    /**************************************************************************
    * If not, check if the path ends with a ':' (logical name) or ']'
    * (directory).
    **************************************************************************/
    if (pathX[lastChar] == ':' || pathX[lastChar] == ']')
      return TRUE;
    else
      return FALSE;
#endif
#if defined(mac)
  /****************************************************************************
  * MAC: Check to see if specification ends with a `:' (and assume it to be a
  * directory without actually checking).  If specification doesn't end with
  * a `:', check if it is a directory anyway.
  ****************************************************************************/
  if (pathX[lastChar] == ':')
    return TRUE;
  else {
    return MacDirSpecified (pathX, NULL, NULL);
  }
#endif
}

/******************************************************************************
* IsWild (Wildcard specification in name).
******************************************************************************/

Logical IsWild (path)
char *path;
{
  char dir[DU_MAX_DIR_LEN+1];
  char name[DU_MAX_NAME_LEN+1];
  ParsePath (path, dir, name);
  if (strchr(name,'*') != NULL) return TRUE;
#if defined(vms)
  if (strchr(name,'%') != NULL) return TRUE;
#else
  if (strchr(name,'?') != NULL) return TRUE;
#endif
  return FALSE;
}

/******************************************************************************
* IsCDF.
*    Determines if the specified pathname is that of a CDF.  Because of
* inconsistent behavior of CD-ROM drivers on various UNIX/POSIXshell machines
* and the Macintosh, the following extensions are tried: `.cdf', `.CDF',
* `.cdf;1', and `.CDF;1'.
******************************************************************************/

Logical IsCDF (pathname)
char *pathname;
{
  char pathnameX[DU_MAX_PATH_LEN+1];
  ExpandPath (pathname, pathnameX);
  if (IsReg(pathnameX)) return TRUE;
  strcatX (pathnameX, ".cdf", DU_MAX_PATH_LEN);
  if (IsReg(pathnameX)) return TRUE;
#if defined(unix) || defined(mac) || defined(posixSHELL)
  ExpandPath (pathname, pathnameX);
  strcatX (pathnameX, ".cdf;1", DU_MAX_PATH_LEN);
  if (IsReg(pathnameX)) return TRUE;
  ExpandPath (pathname, pathnameX);
  strcatX (pathnameX, ".CDF", DU_MAX_PATH_LEN);
  if (IsReg(pathnameX)) return TRUE;
  ExpandPath (pathname, pathnameX);
  strcatX (pathnameX, ".CDF;1", DU_MAX_PATH_LEN);
  if (IsReg(pathnameX)) return TRUE;
#endif
  return FALSE;
}

/******************************************************************************
* Qop.
******************************************************************************/

QOP *Qop (argc, argv, validQuals, optRequired)
int argc;
char *argv[];
char *validQuals[];
int optRequired[];
{
  /****************************************************************************
  * Check to see if a style definition has been specified.  (A slash or hyphen
  * as the first command line argument.)
  ****************************************************************************/
  if (argc > 1) {
    if (!strcmp(argv[1],"/")) return ParseSlashStyle (argc - 1, &argv[1],
						      validQuals, optRequired);
    if (!strcmp(argv[1],"-")) return ParseHyphenStyle (argc - 1, &argv[1],
						       validQuals,optRequired);
  }
  /****************************************************************************
  * No style definition, parse according to default for operating system
  * being used.
  ****************************************************************************/
#if defined(vms)
  return ParseSlashStyle (argc, argv, validQuals, optRequired);
#else
  return ParseHyphenStyle (argc, argv, validQuals, optRequired);
#endif
}

/******************************************************************************
* ParseSlashStyle.
* Assume qualifiers begin with a slash (/).
******************************************************************************/

static QOP *ParseSlashStyle (argc, argv, validQuals, optRequired)
int argc;
char *argv[];
char *validQuals[];
int optRequired[];
{
  QOP *qop;                     /* Pointer to QOP structure. */
  int argN,                     /* Argument number. */
      qualNv,                   /* Qualifier number (from list of valid
				   qualifiers). */
      qualNe,                   /* Qualifier number (from list of qualifiers
				   entered). */
      Nparms = 0,               /* Number of parameters entered. */
      Nquals = 0,               /* Number of qualifiers entered. */
      matchQualNv,              /* Matching qualifier number (from list of
				   valid qualifiers). */
      qualEntered[QOP_MAX_QUALs];
				/* TRUE if the corresponding qualifier was
				   entered. */
  char *parms[QOP_MAX_PARMs],   /* Pointers to the parameters entered. */
       *qualOpt[QOP_MAX_QUALs], /* Pointers to the options of the qualifiers
				   entered (indexed to correspond to the list
				   of valid qualifiers). */
       *argvT[QOP_MAX_ARGVs],   /* Pointers to temporary `argv' strings. */
       *quals[QOP_MAX_QUALs],   /* Pointers to the qualifiers entered (in the
				   order they were entered). */
       *opts[QOP_MAX_QUALs],    /* Pointers to the options entered (in the
				   order they were entered). */
       *ptr;                    /* Pointer into character string. */
  enum modes { UNKNOWN_,
	       START_OF_PARM_,
	       FIND_PARM_END_,
	       START_OF_QUAL_,
	       FIND_QUAL_END_,
	       START_OF_OPT_,
	       FIND_OPT_END_ } mode;  /* The current mode when scanning the
					 list of command line arguments. */
  /****************************************************************************
  * Determine parameters and qualifiers/options.
  ****************************************************************************/
  for (argN = 1; argN < argc; argN++) {
     char nChars = strlen (argv[argN]);
     argvT[argN] = (char *) cdf_AllocateMemory ((size_t)nChars + 1, FatalError);
     strcpyX (argvT[argN], argv[argN], nChars);
     mode = UNKNOWN_;
     for (ptr = argvT[argN]; *ptr != NUL;) {
	switch (mode) {
	  case UNKNOWN_:
	    if (*ptr == '/') {
	      mode = START_OF_QUAL_;
	      ptr++;
	    }
	    else
	      mode = START_OF_PARM_;
	    break;
	  case START_OF_PARM_:
	    Nparms++;
	    if (Nparms <= QOP_MAX_PARMs)
	      parms[Nparms-1] = ptr;
	    else {
	      DisplayError ("Too many parameters.");
	      FreeArgvT (argN, argvT);
	      return NULL;
	    }
	    mode = FIND_PARM_END_;
	    ptr++;
	    break;
	  case FIND_PARM_END_:
	    if (*ptr == '/') {
	      if (*(ptr+1) == '/') {
		memmove (ptr, ptr+1, strlen(ptr+1) + 1);
	      } else {
		*ptr = NUL;
		mode = START_OF_QUAL_;
	      }
	    }
	    ptr++;
	    break;
	  case START_OF_QUAL_:
	    if (*ptr == '/')
	      mode = START_OF_PARM_;
	    else {
	      Nquals++;
	      if (Nquals <= QOP_MAX_QUALs)
		quals[Nquals-1] = ptr;
	      else {
		DisplayError ("Too many qualifiers.");
		FreeArgvT (argN, argvT);
		return NULL;
	      }
	      mode = FIND_QUAL_END_;
	      ptr++;
	    }
	    break;
	  case FIND_QUAL_END_:
	    switch (*ptr) {
	      case '/':
		*ptr = NUL;
		opts[Nquals-1] = NULL;
		mode = START_OF_QUAL_;
		break;
	      case '=':
		*ptr = NUL;
		mode = START_OF_OPT_;
		break;
	    }
	    ptr++;
	    break;
	  case START_OF_OPT_:
	    if (*ptr == '/')
	      if (*(ptr+1) == '/') {
		opts[Nquals-1] = ptr + 1;
		ptr += 2;
		mode = FIND_OPT_END_;
	      }
	      else {
		char tempS[MAX_MESSAGE_TEXT_LEN+1];
		snprintf (tempS, (size_t) sizeof(tempS),
			  "Missing option for qualifier `/%s'.",
			  quals[Nquals-1]);
		DisplayError (tempS);
		FreeArgvT (argN, argvT);
		return NULL;
	      }
	    else {
	      opts[Nquals-1] = ptr;
	      mode = FIND_OPT_END_;
	      ptr++;
	    }
	    break;
	  case FIND_OPT_END_:
	    if (*ptr == '/') {
	      if (*(ptr+1) == '/') {
		memmove (ptr, ptr+1, strlen(ptr+1) + 1);
	      } else {
		*ptr = NUL;
		mode = START_OF_QUAL_;
	      }
	    }
	    ptr++;
	    break;
	}
     }
     switch (mode) {
       case UNKNOWN_:
	 /* Logic error. */
	 break;
       case START_OF_PARM_:
	 /* Logic error. */
	 break;
       case START_OF_QUAL_:
	 DisplayError ("Missing qualifier body (/).");
	 FreeArgvT (argN, argvT);
	 return NULL;
       case FIND_QUAL_END_:
	 /* Qualifier already NUL-terminated */
	 opts[Nquals-1] = NULL;
	 break;
       case FIND_PARM_END_:
	 /* Parameter already NUL-terminated */
	 break;
       case START_OF_OPT_: {
	 char tempS[MAX_MESSAGE_TEXT_LEN+1];
	 snprintf (tempS, (size_t) sizeof(tempS),
		   "Missing option for qualifier `/%s'.",quals[Nquals-1]);
	 DisplayError (tempS);
	 FreeArgvT (argN, argvT);
	 return NULL;
       }
       case FIND_OPT_END_:
	 /* Option already NUL-terminated */
	 break;
     }
  }
  /****************************************************************************
  * Set up to scan command line arguments (while checking number of valid
  * qualifiers).
  ****************************************************************************/
  for (qualNv = 0; validQuals[qualNv] != NULL; qualNv++) {
     if (qualNv < QOP_MAX_QUALs) {
       qualEntered[qualNv] = FALSE;
       qualOpt[qualNv] = NULL;
     }
     else {
       WriteOut (stdout, "Too many valid qualifiers.\n");
       FreeArgvT (argc - 1, argvT);
       return NULL;
     }
  }
  /****************************************************************************
  * Determine which qualifiers/options were entered.
  ****************************************************************************/
  for (qualNe = 0; qualNe < Nquals; qualNe++) {
     matchQualNv = FindUniqueMatch (quals[qualNe], validQuals);
     switch (matchQualNv) {
       case NOMATCH: {
	 char tempS[MAX_MESSAGE_TEXT_LEN+1];
	 snprintf (tempS, (size_t) sizeof(tempS),
		   "Unknown qualifier (/%s).", quals[qualNe]);
	 DisplayError (tempS);
	 FreeArgvT (argc - 1, argvT);
	 return NULL;
       }
       case MATCHES: {
	 char tempS[MAX_MESSAGE_TEXT_LEN+1];
	 snprintf (tempS, (size_t) sizeof(tempS),
		   "Ambiguous qualifier (/%s).", quals[qualNe]);
	 DisplayError (tempS);
	 FreeArgvT (argc - 1, argvT);
	 return NULL;
       }
       default: {
	 if (optRequired[matchQualNv] && opts[qualNe] == NULL) {
	   char tempS[MAX_MESSAGE_TEXT_LEN+1];
	   snprintf (tempS, (size_t) sizeof(tempS),
	 	     "Option missing for qualifier `/%s'.",
		     validQuals[matchQualNv]);
	   DisplayError (tempS);
	   FreeArgvT (argc - 1, argvT);
	   return NULL;
         }
	 if (!optRequired[matchQualNv] && opts[qualNe] != NULL) {
	   char tempS[MAX_MESSAGE_TEXT_LEN+1];
	   snprintf (tempS, (size_t) sizeof(tempS),
		     "Extraneous option for qualifier `/%s'.",
		     validQuals[matchQualNv]);
	   DisplayError (tempS);
	   FreeArgvT (argc - 1, argvT);
	   return NULL;
	 }
	 if (qualEntered[matchQualNv]) {
	   char tempS[MAX_MESSAGE_TEXT_LEN+1];
	   snprintf (tempS, (size_t) sizeof(tempS),
		     "Redundant qualifier (/%s).",
		    validQuals[matchQualNv]);
	   DisplayError (tempS);
	   FreeArgvT (argc - 1, argvT);
	   return NULL;
	 }
	 qualEntered[matchQualNv] = TRUE;
	 qualOpt[matchQualNv] = opts[qualNe];
	 break;
       }
     }
  }
  /****************************************************************************
  * Build QOP structure.
  ****************************************************************************/
  qop = BuildQOP (Nparms, parms, validQuals, qualEntered, qualOpt);
  /****************************************************************************
  * Free memory used and return QOP structure.
  ****************************************************************************/
  FreeArgvT (argc - 1, argvT);
  return qop;
}

/******************************************************************************
* ParseHyphenStyle.
* Assume qualifiers begin with a hyphen (-).
******************************************************************************/

static QOP *ParseHyphenStyle (argc, argv, validQuals, optRequired)
int argc;
char *argv[];
char *validQuals[];
int optRequired[];
{
  QOP *qop;                     /* Pointer to a QOP structure. */
  int argN,                     /* Command line argument number. */
      qualNv,                   /* Qualifier number from `valids' list. */
      Nparms = 0,               /* Number of parameters entered. */
      matchQualNv,              /* Matching qualifier number (from list of
				   valid qualifiers). */
      qualEntered[QOP_MAX_QUALs];
				/* TRUE if the corresponding qualifier was
				   entered. */
  char *parms[QOP_MAX_PARMs],   /* Pointers to the parameters entered. */
       *qualOpt[QOP_MAX_QUALs]; /* Pointers to the options of the qualifiers
				   entered (indexed to correspond to the list
				   of valid qualifiers). */
  /****************************************************************************
  * Set up to scan command line arguments (while checking number of valid
  * qualifiers).
  ****************************************************************************/
  for (qualNv = 0; validQuals[qualNv] != NULL; qualNv++) {
     if (qualNv < QOP_MAX_QUALs) {
       qualEntered[qualNv] = FALSE;
       qualOpt[qualNv] = NULL;
     }
     else {
       DisplayError ("Too many valid qualifiers.");
       return NULL;
     }
  }
  /****************************************************************************
  * Determine which qualifiers were entered (the rest being parameters).
  ****************************************************************************/
  for (argN = 1; argN < argc; argN++) {
     if (argv[argN][0] == '-' && argv[argN][1] != '-') {
       matchQualNv = FindUniqueMatch (&argv[argN][1], validQuals);
       switch (matchQualNv) {
	 case NOMATCH: {
	   char tempS[MAX_MESSAGE_TEXT_LEN+1];
	   snprintf (tempS, (size_t) sizeof(tempS),
		     "Unknown qualifier (-%s).", &argv[argN][1]);
	   DisplayError (tempS);
	   return NULL;
	 }
	 case MATCHES: {
	   char tempS[MAX_MESSAGE_TEXT_LEN+1];
	   snprintf (tempS, (size_t) sizeof(tempS),
		     "Ambiguous qualifier (-%s).", &argv[argN][1]);
	   DisplayError (tempS);
	   return NULL;
	 }
	 default: {
	   if (qualEntered[matchQualNv]) {
	     char tempS[MAX_MESSAGE_TEXT_LEN+1];
	     snprintf (tempS, (size_t) sizeof(tempS),
		       "Redundant qualifier (-%s).",
		       validQuals[matchQualNv]);
	     DisplayError (tempS);
	     return NULL;
	   }
	   else
	     qualEntered[matchQualNv] = TRUE;
	   if (optRequired[matchQualNv]) {
	     if (argN+1 < argc) {
	       qualOpt[matchQualNv] = argv[argN+1];
	       argN++;
	     }
	     else {
	       char tempS[MAX_MESSAGE_TEXT_LEN+1];
	       snprintf (tempS, (size_t) sizeof(tempS),
		         "Option missing for qualifier `-%s'.",
		 	 validQuals[matchQualNv]);
	       DisplayError (tempS);
	       return NULL;
	     }
	   }
	 }
       }
     }
     else {
       Nparms++;
       if (Nparms <= QOP_MAX_PARMs)
	 parms[Nparms-1] = BOO(strncmp(argv[argN],"--",2),
			       argv[argN],argv[argN] + 1);
       else {
	 DisplayError ("Too many parameters.");
	 return NULL;
       }
     }
  }
  /****************************************************************************
  * Build QOP structure.
  ****************************************************************************/
  qop = BuildQOP (Nparms, parms, validQuals, qualEntered, qualOpt);
  /****************************************************************************
  * Return QOP structure.
  ****************************************************************************/
  return qop;
}

/******************************************************************************
* BuildQOP.
******************************************************************************/

static QOP *BuildQOP (Nparms, parms, validQuals, qualEntered, qualOpt)
int Nparms;
char *parms[];
char *validQuals[];
int *qualEntered;
char *qualOpt[];
{
  QOP *qop;             /* Pointer to a QOP structure. */
  int parmN,            /* Parameter number. */
      qualNv,           /* Qualifier number (in list of valid qualifiers. */
      strCount = 0;     /* Number of characters in the strings to be contained
			   in the QOP structure. */
  char *strOffset;      /* Offset into the QOP structure of a string
			   (parameter/option). */
  for (parmN = 0; parmN < Nparms; parmN++)
     strCount += strlen(parms[parmN]) + 1;
  for (qualNv = 0; validQuals[qualNv] != NULL; qualNv++)
     if (qualEntered[qualNv])
       if (qualOpt[qualNv] != NULL) strCount += strlen(qualOpt[qualNv]) + 1;
  qop = (QOP *) cdf_AllocateMemory ((size_t)sizeof(QOP) + strCount, FatalError);
  strOffset = (char *) qop + sizeof(QOP);
  qop->Nparms = Nparms;
  for (parmN = 0; parmN < Nparms; parmN++) {
     qop->parms[parmN] = strOffset;
     strOffset += strlen(parms[parmN]) + 1;
     strcpyX (qop->parms[parmN], parms[parmN], 0);
  }
  for (qualNv = 0; validQuals[qualNv] != NULL; qualNv++) {
     qop->qualEntered[qualNv] = qualEntered[qualNv];
     if (qualEntered[qualNv]) {
       if (qualOpt[qualNv] != NULL) {
	 qop->qualOpt[qualNv] = strOffset;
	 strOffset += strlen(qualOpt[qualNv]) + 1;
	 strcpyX (qop->qualOpt[qualNv], qualOpt[qualNv], 0);
       }
       else
	 qop->qualOpt[qualNv] = NULL;
     }
  }
  return qop;
}

/******************************************************************************
* ValidateQual.
******************************************************************************/

Logical ValidateQual (qual, validQuals)
char *qual;
char *validQuals[];
{
  int ix = 0;
  while (validQuals[ix] != NULL) {
    if (StrStrIgCaseX(qual+1, validQuals[ix]))
      return TRUE;
    ++ix;
  }
  return FALSE;
}

/******************************************************************************
* FreeArgvT.
******************************************************************************/

static void FreeArgvT (lastArgN, argvT)
int lastArgN;
char *argvT[];
{
  int argN;
  for (argN = 1; argN <= lastArgN; argN++) cdf_FreeMemory (argvT[argN],
						       FatalError);
  return;
}

/******************************************************************************
* ASSIGNx.
******************************************************************************/

void ASSIGNx (dst, src, dataType, numElems)
void *dst;
void *src;
long dataType;
long numElems;
{
  memmove (dst, src, (size_t) (CDFelemSize(dataType) * numElems));
  return;
}

/******************************************************************************
* EQx.
******************************************************************************/

Logical EQx (a, b, dataType, numElems)
void *a;
void *b;
long dataType;
long numElems;
{
  if (dataType != CDF_EPOCH16)
    return BOO(memcmp(a,b,(size_t)(CDFelemSize(dataType)*numElems)),FALSE,TRUE);
  else {
    double epoch16_a[2], epoch16_b[2];
    epoch16_a[0] = *((double *) a);
    epoch16_a[1] = *(((double *) a)+1);
    epoch16_b[0] = *((double *) b);
    epoch16_b[1] = *(((double *) b)+1);
    if (epoch16_a[0] == epoch16_b[0] && epoch16_a[1] == epoch16_b[1])
      return TRUE;
    return FALSE;
  }
}

/******************************************************************************
* NEx.
******************************************************************************/

Logical NEx (a, b, dataType, numElems)
void *a;
void *b;
long dataType;
long numElems;
{
  if (dataType != CDF_EPOCH16)
    return BOO(memcmp(a,b,(size_t)(CDFelemSize(dataType)*numElems)),TRUE,FALSE);
  else {
    double epoch16_a[2], epoch16_b[2];
    epoch16_a[0] = *((double *) a);
    epoch16_a[1] = *(((double *) a)+1);
    epoch16_b[0] = *((double *) b);
    epoch16_b[1] = *(((double *) b)+1);
    if (epoch16_a[0] == epoch16_b[0] && epoch16_a[1] == epoch16_b[1])
      return FALSE;
    return TRUE;
  }
}

/******************************************************************************
* LEx.
******************************************************************************/

Logical LEx (a, b, dataType, numElems)
void *a;
void *b;
long dataType;
long numElems;
{
switch (dataType) {
  case CDF_BYTE:
  case CDF_INT1:
    return (*((sChar *) a) <= *((sChar *) b));
  case CDF_UINT1:
    return (*((uChar *) a) <= *((uChar *) b));
  case CDF_INT2:
    return (*((Int16 *) a) <= *((Int16 *) b));
  case CDF_UINT2:
    return (*((uInt16 *) a) <= *((uInt16 *) b));
  case CDF_INT4:
    return (*((Int32 *) a) <= *((Int32 *) b));
  case CDF_INT8:
  case CDF_TIME_TT2000:
    return (*((Int64 *) a) <= *((Int64 *) b));
  case CDF_UINT4:
    return (*((uInt32 *) a) <= *((uInt32 *) b));
  case CDF_REAL4:
  case CDF_FLOAT:
    return (*((float *) a) <= *((float *) b));
  case CDF_REAL8:
  case CDF_DOUBLE:
  case CDF_EPOCH:
    return (*((double *) a) <= *((double *) b));
  case CDF_EPOCH16: {
    double epoch16_a[2], epoch16_b[2];
    epoch16_a[0] = *((double *) a);
    epoch16_a[1] = *(((double *) a)+1);
    epoch16_b[0] = *((double *) b);
    epoch16_b[1] = *(((double *) b)+1);
    if (epoch16_a[0] > epoch16_b[0]) return FALSE;
    if (epoch16_a[0] == epoch16_b[0] && epoch16_a[1] > epoch16_b[1])
      return FALSE;
    return TRUE;
  }
  case CDF_CHAR:
  case CDF_UCHAR:
    return BOO(memcmp(a,b,(size_t)numElems) <= 0,TRUE,FALSE);
}
return FALSE;
}

/******************************************************************************
* LTx.
******************************************************************************/

Logical LTx (a, b, dataType, numElems)
void *a;
void *b;
long dataType;
long numElems;
{
switch (dataType) {
  case CDF_BYTE:
  case CDF_INT1:
    return (*((sChar *) a) < *((sChar *) b));
  case CDF_UINT1:
    return (*((uChar *) a) < *((uChar *) b));
  case CDF_INT2:
    return (*((Int16 *) a) < *((Int16 *) b));
  case CDF_UINT2:
    return (*((uInt16 *) a) < *((uInt16 *) b));
  case CDF_INT4:
    return (*((Int32 *) a) < *((Int32 *) b));
  case CDF_INT8:
  case CDF_TIME_TT2000:
    return (*((Int64 *) a) < *((Int64 *) b));
  case CDF_UINT4:
    return (*((uInt32 *) a) < *((uInt32 *) b));
  case CDF_REAL4:
  case CDF_FLOAT:
    return (*((float *) a) < *((float *) b));
  case CDF_REAL8:
  case CDF_DOUBLE:
  case CDF_EPOCH:
    return (*((double *) a) < *((double *) b));
  case CDF_EPOCH16: {
    double epoch16_a[2], epoch16_b[2];
    epoch16_a[0] = *((double *) a);
    epoch16_a[1] = *(((double *) a)+1);
    epoch16_b[0] = *((double *) b);
    epoch16_b[1] = *(((double *) b)+1);
    if (epoch16_a[0] > epoch16_b[0]) return FALSE;
    if (epoch16_a[0] == epoch16_b[0] && epoch16_a[1] >= epoch16_b[1])
      return FALSE;
    return TRUE;
  }
  case CDF_CHAR:
  case CDF_UCHAR:
    return BOO(memcmp(a,b,(size_t)numElems) < 0,TRUE,FALSE);
}
return FALSE;
}

/******************************************************************************
* GEx.
******************************************************************************/

Logical GEx (a, b, dataType, numElems)
void *a;
void *b;
long dataType;
long numElems;
{
switch (dataType) {
  case CDF_BYTE:
  case CDF_INT1:
    return (*((sChar *) a) >= *((sChar *) b));
  case CDF_UINT1:
    return (*((uChar *) a) >= *((uChar *) b));
  case CDF_INT2:
    return (*((Int16 *) a) >= *((Int16 *) b));
  case CDF_UINT2:
    return (*((uInt16 *) a) >= *((uInt16 *) b));
  case CDF_INT4:
    return (*((Int32 *) a) >= *((Int32 *) b));
  case CDF_INT8:
  case CDF_TIME_TT2000:
    return (*((Int64 *) a) >= *((Int64 *) b));
  case CDF_UINT4:
    return (*((uInt32 *) a) >= *((uInt32 *) b));
  case CDF_REAL4:
  case CDF_FLOAT:
    return (*((float *) a) >= *((float *) b));
  case CDF_REAL8:
  case CDF_DOUBLE:
  case CDF_EPOCH:
    return (*((double *) a) >= *((double *) b));
  case CDF_EPOCH16: {
    double epoch16_a[2], epoch16_b[2];
    epoch16_a[0] = *((double *) a);
    epoch16_a[1] = *(((double *) a)+1);
    epoch16_b[0] = *((double *) b);
    epoch16_b[1] = *(((double *) b)+1);
    if (epoch16_a[0] < epoch16_b[0]) return FALSE;
    if (epoch16_a[0] == epoch16_b[0] && epoch16_a[1] < epoch16_b[1])
      return FALSE;
    return TRUE;
  }
  case CDF_CHAR:
  case CDF_UCHAR:
    return BOO(memcmp(a,b,(size_t)numElems) >= 0,TRUE,FALSE);
}
return FALSE;
}

/******************************************************************************
* GTx.
******************************************************************************/

Logical GTx (a, b, dataType, numElems)
void *a;
void *b;
long dataType;
long numElems;
{
switch (dataType) {
  case CDF_BYTE:
  case CDF_INT1:
    return (*((sChar *) a) > *((sChar *) b));
  case CDF_UINT1:
    return (*((uChar *) a) > *((uChar *) b));
  case CDF_INT2:
    return (*((Int16 *) a) > *((Int16 *) b));
  case CDF_UINT2:
    return (*((uInt16 *) a) > *((uInt16 *) b));
  case CDF_INT4:
    return (*((Int32 *) a) > *((Int32 *) b));
  case CDF_INT8:
  case CDF_TIME_TT2000:
    return (*((Int64 *) a) > *((Int64 *) b));
  case CDF_UINT4:
    return (*((uInt32 *) a) > *((uInt32 *) b));
  case CDF_REAL4:
  case CDF_FLOAT:
    return (*((float *) a) > *((float *) b));
  case CDF_REAL8:
  case CDF_DOUBLE:
  case CDF_EPOCH:
    return (*((double *) a) > *((double *) b));
  case CDF_EPOCH16: {
    double epoch16_a[2], epoch16_b[2];
    epoch16_a[0] = *((double *) a);
    epoch16_a[1] = *(((double *) a)+1);
    epoch16_b[0] = *((double *) b);
    epoch16_b[1] = *(((double *) b)+1);
    if (epoch16_a[0] < epoch16_b[0]) return FALSE;
    if (epoch16_a[0] == epoch16_b[0] && epoch16_a[1] <= epoch16_b[1])
      return FALSE;
    return TRUE;
  }
  case CDF_CHAR:
  case CDF_UCHAR:
    return BOO(memcmp(a,b,(size_t)numElems) > 0,TRUE,FALSE);
}
return FALSE;
}

/******************************************************************************
* IncrRecordIndicesFirstLast.
* Rolls over to `first' indices if at `last' indices (and increments record).
******************************************************************************/

void IncrRecordIndicesFirstLast (rowMajor, recordN, numDims, first, last,
				 indices)
Logical rowMajor;
long *recordN;
long numDims;
long first[];
long last[];
long indices[];
{
  if (numDims > 0) {
    int firstDim = BOO(rowMajor,(int)(numDims - 1),0);
    int lastDim = BOO(rowMajor,0,(int)(numDims - 1));
    int dimIncr = BOO(rowMajor,-1,1);
    int doneDim = lastDim + dimIncr, dimN;
    for (dimN = firstDim; dimN != doneDim; dimN += dimIncr) {
       if (indices[dimN] == last[dimN]) {
         indices[dimN] = first[dimN];
         if (dimN == lastDim) (*recordN)++;
       }
       else {
         indices[dimN]++;
         break;
       }
    }
  }
  else
    (*recordN)++;
  return;
}

/******************************************************************************
* IncrIndicesFirstLastRow.
* Rolls over to `first' indices if at `last' indices.
******************************************************************************/

void IncrIndicesFirstLastRow (numDims, first, last, indices)
long numDims;
long first[];
long last[];
long indices[];
{
  int dimN;
  for (dimN = (int) (numDims - 1); dimN >= 0; dimN--) {
     if (indices[dimN] == last[dimN])
       indices[dimN] = first[dimN];
     else {
       indices[dimN]++;
       break;
     }
  }
  return;
}

/******************************************************************************
* IncrIndicesFirstLastCol.
* Rolls over to `first' indices if at `last' indices.
******************************************************************************/

void IncrIndicesFirstLastCol (numDims, first, last, indices)
long numDims;
long first[];
long last[];
long indices[];
{
  int dimN;
  for (dimN = 0; dimN < numDims; dimN++) {
     if (indices[dimN] == last[dimN])
       indices[dimN] = first[dimN];
     else {
       indices[dimN]++;
       break;
     }
  }
  return;
}

/******************************************************************************
* AllocateBuffers.
******************************************************************************/

Logical AllocateBuffers (nRecords, numDims, dimSizes, groups, nScalarBuffers,
			 nHyperBuffers, handles, nValueBytes, rowMajor,
			 minNofHypers, fatalFnc)
long nRecords;
long numDims;
long dimSizes[];
struct GroupStruct *groups;
int nScalarBuffers;
int nHyperBuffers;
Byte1 **handles[];
size_t nValueBytes[];
Logical rowMajor;
int minNofHypers;
void (*fatalFnc) PROTOARGs((char *msg));
{
  int lsDimN;           /* Least significant dimension number. */
  int msDimN;           /* Most significant dimension number. */
  int dimStep;          /* How to increment dimension number from most
			   significant to least significant. */
  int dimN, dN;
  long nValuesPerRec, nValuesPerDim[CDF_MAX_DIMS], count, nHypersSoFar;
  /****************************************************************************
  * Setup for majority.
  ****************************************************************************/
  lsDimN = BOO(rowMajor,(int)(numDims-1),0);
  msDimN = BOO(rowMajor,0,(int)(numDims-1));
  dimStep = BOO(rowMajor,1,-1);
  /****************************************************************************
  * Calculate number of values per record/dimension.
  ****************************************************************************/
  if (numDims > 0) {
    nValuesPerDim[lsDimN] = 1;
    for (dimN = lsDimN - dimStep; dimN != msDimN - dimStep; dimN -= dimStep) {
       nValuesPerDim[dimN] = dimSizes[dimN+dimStep] *
			     nValuesPerDim[dimN+dimStep];
    }
    nValuesPerRec = dimSizes[msDimN] * nValuesPerDim[msDimN];
  }
  else
    nValuesPerRec = 1;
  /****************************************************************************
  * Check if full records can be allocated.
  ****************************************************************************/
  count = nRecords;
  while (count >= 1 ) {
     long nRecGroups = nRecords/count + BOO(nRecords % count == 0,0,1);
     if (nRecGroups >= minNofHypers || count == 1) {
       if (AttemptAllocations(count,nScalarBuffers,count*nValuesPerRec,
			      nHyperBuffers,handles,nValueBytes)) {
	 groups->nRecGroups = nRecGroups;
	 groups->firstRecCount = count;
	 groups->lastRecCount = BOO(nRecords % count == 0,
				    count,nRecords % count);
	 groups->recGroupN = 1;
	 for (dimN = msDimN; dimN != lsDimN + dimStep; dimN += dimStep) {
	    groups->nDimGroups[dimN] = 1;
	    groups->firstDimCount[dimN] = dimSizes[dimN];
	    groups->lastDimCount[dimN] = dimSizes[dimN];
	    groups->dimGroupN[dimN] = 1;
	 }
	 return TRUE;
       }
     }
     if (count == 1) break;
     count = (count + BOO(count % 2 == 0,0,1)) / 2;
  }
  groups->nRecGroups = nRecords;
  groups->firstRecCount = 1;
  groups->lastRecCount = 1;
  groups->recGroupN = 1;
  nHypersSoFar = nRecords;
  /****************************************************************************
  * Check how much of each dimension can be allocated.
  ****************************************************************************/
  for (dimN = msDimN; dimN != lsDimN + dimStep; dimN += dimStep) {
     count = dimSizes[dimN];
     while (count > 1) {
	long nDimGroups = (dimSizes[dimN] / count) +
			  BOO(dimSizes[dimN] % count == 0,0,1);
	if (nHypersSoFar * nDimGroups >= minNofHypers) {
	  if (AttemptAllocations(1L,nScalarBuffers,count*nValuesPerDim[dimN],
				 nHyperBuffers,handles,nValueBytes)) {
	    groups->nDimGroups[dimN] = nDimGroups;
	    groups->firstDimCount[dimN] = count;
	    groups->lastDimCount[dimN] = BOO(dimSizes[dimN] % count == 0,
					     count,dimSizes[dimN] % count);
	    groups->dimGroupN[dimN] = 1;
	    for (dN = dimN + dimStep; dN != lsDimN + dimStep; dN += dimStep) {
	       groups->nDimGroups[dN] = 1;
	       groups->firstDimCount[dN] = dimSizes[dN];
	       groups->lastDimCount[dN] = dimSizes[dN];
	       groups->dimGroupN[dN] = 1;
	    }
	    return TRUE;
	  }
	}
	count = (count + 1) / 2;
     }
     groups->nDimGroups[dimN] = dimSizes[dimN];
     groups->firstDimCount[dimN] = 1;
     groups->lastDimCount[dimN] = 1;
     groups->dimGroupN[dimN] = 1;
     nHypersSoFar *= dimSizes[dimN];
  }
  if (AttemptAllocations(1L,nScalarBuffers,1L,nHyperBuffers,
			 handles,nValueBytes)) return TRUE;
  /****************************************************************************
  * Not all of the buffers can be allocated even at only one value per buffer.
  ****************************************************************************/
  if (fatalFnc != NULL) (*fatalFnc)("Unable to allocate memory buffers.");
  return FALSE;
}

/******************************************************************************
* AttemptAllocations.
******************************************************************************/

static Logical AttemptAllocations (nScalarValues, nScalarBuffers, nHyperValues,
				   nHyperBuffers, handles, nValueBytes)
long nScalarValues;
int nScalarBuffers;
long nHyperValues;
int nHyperBuffers;
Byte1 **handles[];
size_t nValueBytes[];
{
  int bN, bNt; uLongx maxBytes = 1, nBytes;
#if defined(mac) || defined(dos)
  void *ptr;
#endif
  /****************************************************************************
  * First allocate scalar buffers.
  ****************************************************************************/
  for (bN = 0; bN < nScalarBuffers; bN++) {
     nBytes = nScalarValues * nValueBytes[bN];
     if (nBytes > MAXnBYTESperALLOCATION)
       *handles[bN] = NULL;
     else
       *handles[bN] = cdf_AllocateMemory ((size_t) nBytes, NULL);
     if (*handles[bN] == NULL) {
       for (bNt = bN - 1; bNt >= 0; bNt--) cdf_FreeMemory (*handles[bNt],
						       NULL);
       return FALSE;
     }
     maxBytes = MAXIMUM(maxBytes,nBytes);
  }
  /****************************************************************************
  * Then allocate hyper buffers.
  ****************************************************************************/
  for (bN = nScalarBuffers; bN < nScalarBuffers + nHyperBuffers; bN++) {
     nBytes = nHyperValues * nValueBytes[bN];
     if (nBytes > MAXnBYTESperALLOCATION)
       *handles[bN] = NULL;
     else
       *handles[bN] = cdf_AllocateMemory ((size_t) nBytes, NULL);
     if (*handles[bN] == NULL) {
       for (bNt = bN - 1; bNt >= 0; bNt--) cdf_FreeMemory (*handles[bNt],
						       NULL);
       return FALSE;
     }
     maxBytes = MAXIMUM(maxBytes,nBytes);
  }
  /****************************************************************************
  * Allocate and then free a buffer twice the size of the largest buffer
  * allocated.  This will save some space on the heap for other uses.
  ****************************************************************************/
#if defined(mac) || defined(dos)
  ptr = cdf_AllocateMemory ((size_t) (2 * maxBytes), NULL);
  if (ptr == NULL) {
    for (bNt = nScalarBuffers + nHyperBuffers - 1; bNt >= 0; bNt--) {
       cdf_FreeMemory (*handles[bNt], NULL);
    }
    return FALSE;
  }
  cdf_FreeMemory (ptr, NULL);
#endif
  /****************************************************************************
  * All allocations succeeded, return TRUE.
  ****************************************************************************/
  return TRUE;
}

/******************************************************************************
* HyperFullRecord.
******************************************************************************/

Logical HyperFullRecord (groups, numDims)
struct GroupStruct *groups;
long numDims;
{
  int dimN;
  for (dimN = 0; dimN < numDims; dimN++) {
     if (groups->nDimGroups[dimN] > 1) return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* HypersPerRecord.
******************************************************************************/

long HypersPerRecord (groups, numDims)
struct GroupStruct *groups;
long numDims;
{
  int dimN; long nHypers = 1;
  for (dimN = 0; dimN < numDims; dimN++) {
     nHypers *= groups->nDimGroups[dimN];
  }
  return nHypers;
}

/******************************************************************************
* HyperStartRecord.
******************************************************************************/

Logical HyperStartOfRecord (hyperSt, numDims)
struct HyperStruct *hyperSt;
long numDims;
{
  int dimN;
  for (dimN = 0; dimN < numDims; dimN++) {
     if (hyperSt->dimIndices[dimN] > 0) return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* InitHyperParms.
******************************************************************************/

void InitHyperParms (hyperSt, groups, numDims, nHypers, nValues)
struct HyperStruct *hyperSt;
struct GroupStruct *groups;
long numDims;
long *nHypers;
long *nValues;
{
  int dimN;
  *nHypers = groups->nRecGroups;
  for (dimN = 0; dimN < numDims; dimN++) *nHypers *= groups->nDimGroups[dimN];
  hyperSt->recNumber = 0;
  hyperSt->recCount = groups->firstRecCount;
  hyperSt->recInterval = 1;
  for (dimN = 0; dimN < numDims; dimN++) {
     hyperSt->dimIndices[dimN] = 0;
     hyperSt->dimCounts[dimN] = groups->firstDimCount[dimN];
     hyperSt->dimIntervals[dimN] = 1;
  }
  *nValues = hyperSt->recCount;
  for (dimN = 0; dimN < numDims; dimN++) *nValues *= hyperSt->dimCounts[dimN];
  return;
}

/******************************************************************************
* InitHyperParmsFromAnywhere.
******************************************************************************/

void InitHyperParmsFromAnywhere (hyperSt, groups, numDims, nHypers, nValues, 
                                 strRec)
struct HyperStruct *hyperSt;
struct GroupStruct *groups;
long numDims;
long *nHypers;
long *nValues;
long strRec;
{
  int dimN;
  *nHypers = groups->nRecGroups;
  for (dimN = 0; dimN < numDims; dimN++) *nHypers *= groups->nDimGroups[dimN];
  hyperSt->recNumber = strRec;
  hyperSt->recCount = groups->firstRecCount;
  hyperSt->recInterval = 1;
  for (dimN = 0; dimN < numDims; dimN++) {
     hyperSt->dimIndices[dimN] = 0;
     hyperSt->dimCounts[dimN] = groups->firstDimCount[dimN];
     hyperSt->dimIntervals[dimN] = 1;
  }
  *nValues = hyperSt->recCount;
  for (dimN = 0; dimN < numDims; dimN++) *nValues *= hyperSt->dimCounts[dimN];
  return;
}

/******************************************************************************
* IncrHyperParms.
******************************************************************************/

void IncrHyperParms (hyperSt, groups, numDims, rowMajor, nValues)
struct HyperStruct *hyperSt;
struct GroupStruct *groups;
long numDims;
Logical rowMajor;
long *nValues;
{
  int lsDimN;           /* Least significant dimension number. */
  int msDimN;           /* Most significant dimension number. */
  int dimStep;          /* How to increment dimension number from most
			   significant to least significant. */
  int dimN;
  /****************************************************************************
  * Setup for majority.
  ****************************************************************************/
  lsDimN = BOO(rowMajor,(int)(numDims-1),0);
  msDimN = BOO(rowMajor,0,(int)(numDims-1));
  dimStep = BOO(rowMajor,1,-1);
  /****************************************************************************
  * Increment indices.
  ****************************************************************************/
  for (dimN = lsDimN; dimN != msDimN - dimStep; dimN -= dimStep) {
     if (groups->dimGroupN[dimN] < groups->nDimGroups[dimN]) {
       groups->dimGroupN[dimN]++;
       if (groups->dimGroupN[dimN] < groups->nDimGroups[dimN]) {
	 hyperSt->dimIndices[dimN] += groups->firstDimCount[dimN];
	 hyperSt->dimCounts[dimN] = groups->firstDimCount[dimN];
       }
       else {
	 hyperSt->dimIndices[dimN] += groups->firstDimCount[dimN];
	 hyperSt->dimCounts[dimN] = groups->lastDimCount[dimN];
       }
       *nValues = hyperSt->recCount;
       for (dimN = 0; dimN < numDims; dimN++) {
	  *nValues *= hyperSt->dimCounts[dimN];
       }
       return;
     }
     else {
       groups->dimGroupN[dimN] = 1;
       hyperSt->dimIndices[dimN] = 0;
       hyperSt->dimCounts[dimN] = groups->firstDimCount[dimN];
     }
  }
  /****************************************************************************
  * Increment record.
  ****************************************************************************/
  groups->recGroupN++;
  hyperSt->recNumber += groups->firstRecCount;
  hyperSt->recCount = BOO(groups->recGroupN < groups->nRecGroups,
			groups->firstRecCount,groups->lastRecCount);
  *nValues = hyperSt->recCount;
  for (dimN = 0; dimN < numDims; dimN++) *nValues *= hyperSt->dimCounts[dimN];
  return;
}

/******************************************************************************
* WriteOutPct.
******************************************************************************/

void WriteOutPct (pct)
int pct;
{
  char text[8+1];
  strcpyX (text, "\b\b\b\b", 8);
  if (pct < 10) strcatX (text, ".", 8);
  if (pct < 100) strcatX (text, ".", 8);
  snprintf (EofS(text), (size_t) sizeof(text)-strlen(text), "%d", pct);
  strcatX (text, "%", 8);
  WriteOut (stdout, text);
  return;
}

/******************************************************************************
* SwitchMajority.
******************************************************************************/

Logical SwitchMajority (buffer, rowMajor, numDims, dimSizes, recCount,
			nValueBytes)
Byte1 *buffer;
Logical rowMajor;
long numDims;
long dimSizes[];
long recCount;
size_t nValueBytes;
{
  int dimN; long nRecordBytes, recX; Byte1 *offset; void *tBuffer;
  /****************************************************************************
  * If less than two dimensions no switching needs to be done.
  ****************************************************************************/
  if (numDims < 2) return TRUE;
  /****************************************************************************
  * Calculate number of bytes per record.
  ****************************************************************************/
  nRecordBytes = nValueBytes;
  for (dimN = 0; dimN < numDims; dimN++) nRecordBytes *= dimSizes[dimN];
  /****************************************************************************
  * Attempt to allocate a temporary buffer for one record.
  ****************************************************************************/
#if defined(dos)
  if (TOObigIBMpc(nRecordBytes))
    tBuffer = NULL;
  else
#endif
    tBuffer = cdf_AllocateMemory ((size_t) nRecordBytes, NULL);
  if (tBuffer == NULL) return FALSE;
  /****************************************************************************
  * Switch majority.
  ****************************************************************************/
  for (recX = 0, offset = buffer; recX < recCount;
       recX++, offset += (size_t) nRecordBytes) {
    if (rowMajor)
      ROWtoCOL (offset, tBuffer, numDims, dimSizes, (long) nValueBytes);
    else
      COLtoROW (offset, tBuffer, numDims, dimSizes, (long) nValueBytes);
    memmove (offset, tBuffer, (size_t) nRecordBytes);
  }
  /****************************************************************************
  * Free temporary buffer and return success.
  ****************************************************************************/
  cdf_FreeMemory (tBuffer, FatalError);
  return TRUE;
}

/******************************************************************************
* CompressedCDF.
******************************************************************************/

Logical CompressedCDF (CDFpath)
char *CDFpath;
{
  CDFstatus status; long cType, cParms[CDF_MAX_PARMS];
  OFF_T cFileSize, uFileSize;
  status = CDFlib (GET_, CDF_INFO_, CDFpath, &cType, cParms,
                                    &cFileSize, &uFileSize,
                   NULL_);
  if (StatusBAD(status)) return FALSE;
  return ((Logical) (cType != NO_COMPRESSION));
}

/******************************************************************************
* SameCompressions.
******************************************************************************/

Logical SameCompressions (cType1, cParms1, cType2, cParms2)
long cType1;
long cParms1[CDF_MAX_PARMS];
long cType2;
long cParms2[CDF_MAX_PARMS];
{
  int parmN, pCount;
  /****************************************************************************
  * Compare compression types.
  ****************************************************************************/
  if (cType1 != cType2) return FALSE;
  /****************************************************************************
  * If same types, compare compression parameters (if any).
  ****************************************************************************/
  pCount = CompressionParmsCount(cType1);
  for (parmN = 0; parmN < pCount; parmN++) {
     if (cParms1[parmN] != cParms2[parmN]) return FALSE;
  }
  /****************************************************************************
  * Compressions must be the same.
  ****************************************************************************/
  return TRUE;
}

/******************************************************************************
* SameSparsenesses.
******************************************************************************/

Logical SameSparsenesses (sRecordsType1, sArraysType1, sArraysParms1,
			  sRecordsType2, sArraysType2, sArraysParms2)
long sRecordsType1;
long sArraysType1;
long sArraysParms1[CDF_MAX_PARMS];
long sRecordsType2;
long sArraysType2;
long sArraysParms2[CDF_MAX_PARMS];
{
  int parmN, pCount;
  /****************************************************************************
  * Compare sparse records.
  ****************************************************************************/
  if (sRecordsType1 != sRecordsType2) return FALSE;
  /****************************************************************************
  * Compare sparse array types.
  ****************************************************************************/
  if (sArraysType1 != sArraysType2) return FALSE;
  /****************************************************************************
  * If same types, compare sparse array parameters (if any).
  ****************************************************************************/
  pCount = SparsenessParmsCount(sArraysType1);
  for (parmN = 0; parmN < pCount; parmN++) {
     if (sArraysParms1[parmN] != sArraysParms2[parmN]) return FALSE;
  }
  /****************************************************************************
  * Sparsenesses must be the same.
  ****************************************************************************/
  return TRUE;
}

/******************************************************************************
* WhichFormat.
******************************************************************************/

long WhichFormat (token)
char *token;
{
  if (strncmpIgCase(token,"SINGLE",6) != 0) return SINGLE_FILE;
  if (strncmpIgCase(token,"MULTI",5) != 0) return MULTI_FILE;
  return -1;
}

/******************************************************************************
* WhichMajority.
******************************************************************************/

long WhichMajority (token)
char *token;
{
  if (strncmpIgCase(token,"ROW",3) != 0) return ROW_MAJOR;
  if ((strncmpIgCase(token,"COL",3) != 0) ||
      (strncmpIgCase(token,"COLUMN",6) != 0)) return COLUMN_MAJOR;
  return -1;
}

/******************************************************************************
* WhichRecSparseness.
******************************************************************************/

long WhichRecSparseness (token)
char *token;
{
  if (strncmpIgCase(token,"None",4) != 0) return NO_SPARSERECORDS;
  if (strncmpIgCase(token,"sRecords.PAD",12) != 0) return PAD_SPARSERECORDS;
  if (strncmpIgCase(token,"sRecords.PREV",13) != 0) return PREV_SPARSERECORDS;
  return -1;
}

/******************************************************************************
* WhichCompression
******************************************************************************/

long WhichCompression (token, compression, cParms)
char *token;
long *compression;
long cParms[CDF_MAX_PARMS];
{
  *compression = -1;
  cParms[0] = 0;
  if (strncmpIgCasePattern(token,"No",2) == 0)
    *compression = NO_COMPRESSION;
  else if (strncmpIgCasePattern(token,"RLE",3) == 0)
    *compression = RLE_COMPRESSION;
  else if (strncmpIgCasePattern(token,"HUFF",4) == 0)
     *compression = HUFF_COMPRESSION;
  else if (strncmpIgCasePattern(token,"AHUFF",5) == 0)
    *compression = AHUFF_COMPRESSION;
  else if (strncmpIgCasePattern(token,"GZIP",4) == 0) {
#if defined(__osf__)
    int clevel;
    if (sscanf(token,"GZIP.%d",&clevel) == 1) {
#else
    long clevel;
    if (sscanf(token,"GZIP.%1ld",&clevel) == 1) {
#endif
        if (INCLUSIVE(1,clevel,9)) {
	  *compression =  GZIP_COMPRESSION;
	  cParms[0] = (long) clevel;
	}
    }
  }
  return (long) *compression;
}

/******************************************************************************
* WhichEncoding.  `MIPSEL' (DECstation) and `MIPSEB' (SGi) are valid synonyms
*                 in order to remain compatible with previous releases of CDF.
******************************************************************************/

long WhichEncoding (token)
char *token;
{
  if (strncmpIgCase(token,"HOST",4) != 0) return HOST_ENCODING;
  if (strncmpIgCase(token,"NETWORK",6) != 0) return NETWORK_ENCODING;
  if (strncmpIgCase(token,"IBMPC",5) != 0) return IBMPC_ENCODING;
  if ((strncmpIgCase(token,"PPC",3) != 0) ||
      (strncmpIgCase(token,"MAC",3) != 0)) return PPC_ENCODING;
  if (strncmpIgCase(token,"SUN",3) != 0) return SUN_ENCODING;
  if (strncmpIgCase(token,"VAX",3) != 0) return VAX_ENCODING;
  if ((strncmpIgCase(token,"SGi",3) != 0) ||
      (strncmpIgCase(token,"MIPSEB",6) != 0)) return SGi_ENCODING;
  if ((strncmpIgCase(token,"DECSTATION",10) != 0) ||
      (strncmpIgCase(token,"MIPSEL",6) != 0)) return DECSTATION_ENCODING;
  if (strncmpIgCase(token,"IBMRS",5) != 0) return IBMRS_ENCODING;
  if (strncmpIgCase(token,"HP",2) != 0) return HP_ENCODING;
  if (strncmpIgCase(token,"NeXT",4) != 0) return NeXT_ENCODING;
  if (strncmpIgCase(token,"ALPHAOSF1",9) != 0) return ALPHAOSF1_ENCODING;
  if (strncmpIgCase(token,"ALPHAVMSd",9) != 0) return ALPHAVMSd_ENCODING;
  if (strncmpIgCase(token,"ALPHAVMSg",9) != 0) return ALPHAVMSg_ENCODING;
  if (strncmpIgCase(token,"ALPHAVMSi",9) != 0) return ALPHAVMSi_ENCODING;
  if (strncmpIgCase(token,"ARM_LITTLE",10) != 0) return ARM_LITTLE_ENCODING;
  if (strncmpIgCase(token,"ARM_BIG",7) != 0) return ARM_BIG_ENCODING;
  if (strncmpIgCase(token,"IA64VMSi",8) != 0) return IA64VMSi_ENCODING;
  if (strncmpIgCase(token,"IA64VMSd",8) != 0) return IA64VMSd_ENCODING;
  if (strncmpIgCase(token,"IA64VMSg",8) != 0) return IA64VMSg_ENCODING;
  return -1;
}

/******************************************************************************
* WhichDataType.
******************************************************************************/

long WhichDataType (token)
char *token;
{
  if (strncmpIgCase(token,"BYTE",4) != 0) return CDF_BYTE;
  if (strncmpIgCase(token,"INT1",4) != 0) return CDF_INT1;
  if (strncmpIgCase(token,"UINT1",5) != 0) return CDF_UINT1;
  if (strncmpIgCase(token,"INT2",4) != 0) return CDF_INT2;
  if (strncmpIgCase(token,"UINT2",5) != 0) return CDF_UINT2;
  if (strncmpIgCase(token,"INT4",4) != 0) return CDF_INT4;
  if (strncmpIgCase(token,"INT8",4) != 0) return CDF_INT8;
  if (strncmpIgCase(token,"UINT4",5) != 0) return CDF_UINT4;
  if (strncmpIgCase(token,"REAL4",5) != 0) return CDF_REAL4;
  if (strncmpIgCase(token,"FLOAT",5) != 0) return CDF_FLOAT;
  if (strncmpIgCase(token,"REAL8",5) != 0) return CDF_REAL8;
  if (strncmpIgCase(token,"DOUBLE",6) != 0) return CDF_DOUBLE;
  if (strncmpIgCase(token,"CHAR",4) != 0) return CDF_CHAR;
  if (strncmpIgCase(token,"UCHAR",5) != 0) return CDF_UCHAR;
  if (strncmpIgCase(token,"EPOCH",5) != 0) return CDF_EPOCH;
  if (strncmpIgCase(token,"EPOCH16",7) != 0) return CDF_EPOCH16;
  if (strncmpIgCase(token,"TIME_TT2000",11) != 0) return CDF_TIME_TT2000;
  if (strncmpIgCase(token,"TT2000",6) != 0) return CDF_TIME_TT2000;
  if (strncmpIgCase(token,"CDF_BYTE",8) != 0) return CDF_BYTE;
  if (strncmpIgCase(token,"CDF_INT1",8) != 0) return CDF_INT1;
  if (strncmpIgCase(token,"CDF_UINT1",9) != 0) return CDF_UINT1;
  if (strncmpIgCase(token,"CDF_INT2",8) != 0) return CDF_INT2;
  if (strncmpIgCase(token,"CDF_UINT2",9) != 0) return CDF_UINT2;
  if (strncmpIgCase(token,"CDF_INT4",8) != 0) return CDF_INT4;
  if (strncmpIgCase(token,"CDF_INT8",8) != 0) return CDF_INT8;
  if (strncmpIgCase(token,"CDF_UINT4",9) != 0) return CDF_UINT4;
  if (strncmpIgCase(token,"CDF_REAL4",9) != 0) return CDF_REAL4;
  if (strncmpIgCase(token,"CDF_FLOAT",9) != 0) return CDF_FLOAT;
  if (strncmpIgCase(token,"CDF_REAL8",9) != 0) return CDF_REAL8;
  if (strncmpIgCase(token,"CDF_DOUBLE",10) != 0) return CDF_DOUBLE;
  if (strncmpIgCase(token,"CDF_CHAR",8) != 0) return CDF_CHAR;
  if (strncmpIgCase(token,"CDF_UCHAR",9) != 0) return CDF_UCHAR;
  if (strncmpIgCase(token,"CDF_EPOCH",9) != 0) return CDF_EPOCH;
  if (strncmpIgCase(token,"CDF_EPOCH16",11) != 0) return CDF_EPOCH16;
  if (strncmpIgCase(token,"CDF_TIME_TT2000",15) != 0) return CDF_TIME_TT2000;
  if (strncmpIgCase(token,"CDF_TT2000",10) != 0) return CDF_TIME_TT2000;
  return -1;
}

/******************************************************************************
* WhichChecksum.
******************************************************************************/

long WhichChecksum (token)
char *token;
{
  if ((strncmpIgCase(token,"NO",2) != 0) ||
      (strncmpIgCase(token,"NONE",4) != 0)) return NO_CHECKSUM;
  if (strncmpIgCase(token,"MD5",3) != 0) return MD5_CHECKSUM;
  if (strncmpIgCase(token,"OTHER",5) != 0) return OTHER_CHECKSUM;
  return -1;
}

/******************************************************************************
* EncodingToken.
*   Returns address of character string for encoding.
******************************************************************************/

char *EncodingToken (encoding)
long encoding;
{
  switch (encoding) {
    case NETWORK_ENCODING: return "NETWORK";
    case VAX_ENCODING: return "VAX";
    case SUN_ENCODING: return "SUN";
    case DECSTATION_ENCODING: return "DECSTATION";
    case SGi_ENCODING: return "SGi";
    case IBMRS_ENCODING: return "IBMRS";
    case IBMPC_ENCODING: return "IBMPC";
    case HP_ENCODING: return "HP";
    case NeXT_ENCODING: return "NeXT";
    case ALPHAOSF1_ENCODING: return "ALPHAOSF1";
    case ALPHAVMSd_ENCODING: return "ALPHAVMSd";
    case ALPHAVMSg_ENCODING: return "ALPHAVMSg";
    case ALPHAVMSi_ENCODING: return "ALPHAVMSi";
    case PPC_ENCODING: return "PPC";
    case ARM_LITTLE_ENCODING: return "ARM_LITTLE";
    case ARM_BIG_ENCODING: return "ARM_BIG";
    case IA64VMSi_ENCODING: return "IA64VMSi";
    case IA64VMSd_ENCODING: return "IA64VMSd";
    case IA64VMSg_ENCODING: return "IA64VMSg";
    default: return "?";
  }
}

/******************************************************************************
* MajorityToken.
*   Returns address of character string for majority.
******************************************************************************/

char *MajorityToken (majority)
long majority;
{
  switch (majority) {
    case ROW_MAJOR: return "ROW";
    case COLUMN_MAJOR: return "COLUMN";
    default: return "?";
  }
}

/******************************************************************************
* FormatToken.
*   Returns address of character string for format.
******************************************************************************/

char *FormatToken (format)
long format;
{
  switch (format) {
    case SINGLE_FILE: return "SINGLE";
    case MULTI_FILE: return "MULTI";
    default: return "?";
  }
}

/******************************************************************************
* ChecksumToken.
*   Returns address of character string for checksum.
******************************************************************************/

char *ChecksumToken (checksum)
long checksum;
{
  switch (checksum) {
    case NO_CHECKSUM: return "None";
    case MD5_CHECKSUM: return "MD5";
    case OTHER_CHECKSUM: return "Other";
    default: return "?";
  }
}

/******************************************************************************
* ScopeToken.
*   Returns address of character string for scope.
******************************************************************************/

char *ScopeToken (scope)
long scope;
{
  switch (scope) {
    case GLOBAL_SCOPE: return "GLOBAL";
    case VARIABLE_SCOPE: return "VARIABLE";
    default: return "?";
  }
}

/******************************************************************************
* VarianceToken.
*   Returns address of character string for variance.
******************************************************************************/

char *VarianceToken (variance)
long variance;
{
  switch (variance) {
    case VARY: return "VARY";
    case NOVARY: return "NOVARY";
    default: return "?";
  }
}

/******************************************************************************
* TFvarianceToken.
*   Returns address of character string for variance (T/F).
******************************************************************************/

char *TFvarianceToken (variance)
long variance;
{
  switch (variance) {
    case VARY: return "T";
    case NOVARY: return "F";
    default: return "?";
  }
}

/******************************************************************************
* DataTypeToken.
*   Returns address of character string for data type.
******************************************************************************/

char *DataTypeToken (dataType)
long dataType;
{
  switch (dataType) {
    case CDF_BYTE: return "BYTE";
    case CDF_INT1: return "INT1";
    case CDF_INT2: return "INT2";
    case CDF_INT4: return "INT4";
    case CDF_INT8: return "INT8";
    case CDF_UINT1: return "UINT1";
    case CDF_UINT2: return "UINT2";
    case CDF_UINT4: return "UINT4";
    case CDF_REAL4: return "REAL4";
    case CDF_REAL8: return "REAL8";
    case CDF_FLOAT: return "FLOAT";
    case CDF_DOUBLE: return "DOUBLE";
    case CDF_EPOCH: return "EPOCH";
    case CDF_EPOCH16: return "EPOCH16";
    case CDF_TIME_TT2000: return "TT2000";
    case CDF_CHAR: return "CHAR";
    case CDF_UCHAR: return "UCHAR";
    default: return "?";
  }
}

/******************************************************************************
* CompressionToken.
*   Returns address of character string for compression type/parameters.
******************************************************************************/

char *CompressionToken (cType, cParms)
long cType;
long cParms[CDF_MAX_PARMS];
{
  switch (cType) {
    case NO_COMPRESSION:
      return "None";
    case RLE_COMPRESSION:
      switch (cParms[0]) {
	case RLE_OF_ZEROs: return "RLE.0";
	default: return "RLE.?";
      }
    case HUFF_COMPRESSION:
      switch (cParms[0]) {
	case OPTIMAL_ENCODING_TREES: return "HUFF.0";
	default: return "HUFF.?";
      }
    case AHUFF_COMPRESSION:
      switch (cParms[0]) {
	case OPTIMAL_ENCODING_TREES: return "AHUFF.0";
	default: return "AHUFF.?";
      }
    case GZIP_COMPRESSION: {
      static char token[MAX_GZIP_TOKEN_LEN+1];
      snprintf (token, (size_t) sizeof(token), "GZIP.%d", (int) cParms[0]);
      return token;
    }
    default: return "?";
  }
}

/******************************************************************************
* SparsenessToken.
*   Returns address of character string for sparseness types/parameters.
* Both record and array sparseness is encoded in the same string.
******************************************************************************/

char *SparsenessToken (sRecordsType, sArraysType, sArraysParms)
long sRecordsType;
long sArraysType;
long sArraysParms[CDF_MAX_PARMS];
{
  if (sArraysParms[0] == 0) { };    /* Quiets some compilers. */
  switch (sRecordsType) {
    case NO_SPARSERECORDS:
      switch (sArraysType) {
	case NO_SPARSEARRAYS:
	  return "None";
	default:
	  return "?";
      }
    case PAD_SPARSERECORDS:
      switch (sArraysType) {
	case NO_SPARSEARRAYS:
	  return "sRecords.PAD";
	default:
	  return "?";
      }
    case PREV_SPARSERECORDS:
      switch (sArraysType) {
	case NO_SPARSEARRAYS:
	  return "sRecords.PREV";
	default:
	  return "?";
      }
    default:
      return "?";
  }
}

/******************************************************************************
* PickDelimiter.
*    Exclamation mark (`!') isn't used because it starts a comment in a
* skeleton table.  Period (`.') isn't used because it ends a list of
* attribute entries (in a skeleton table).  Left bracket (`[') isn't used
* because it starts a set of NRV indices (in a skeleton table).
******************************************************************************/

char PickDelimiter (string, N)
char *string;
size_t N;
{
  static char choices[] = "\"'`^~:-,;|+=@#$%&*()/?<>{}]\\";
  int i;
  for (i = 0; choices[i] != NUL; i++) {
     if (strnchr(string,choices[i],N) == NULL) return choices[i];
  }
  return NUL;   /* Very bad, use character stuffing if this is a problem. */
}

/******************************************************************************
* strnchr.
******************************************************************************/

static char *strnchr (string, chr, n)
char *string;
int chr;
int n;
{
  int i;
  for (i = 0; i < n; i++) if (string[i] == chr) return &string[i];
  return NULL;
}

/******************************************************************************
* sleep.  Only for Microsoft C.
******************************************************************************/

#if defined(MICROSOFTC)
void sleep (seconds)
uInt4 seconds;
{
time_t start_time;
time_t current_time;

time (&start_time);
time (&current_time);

while (current_time - start_time < seconds) time (&current_time);

return;
}
#endif

/******************************************************************************
* strstrIgCase.
******************************************************************************/

char *strstrIgCase (string, substring)
char *string;
char *substring;
{
  int stringL = strlen(string);
  int substringL = strlen(substring);
  int i, j;
  if (stringL == 0 || substringL == 0) return NULL;
  if (stringL < substringL) return NULL;
  for (i = 0; i < stringL; i++) {
     for (j = 0; j < substringL; j++) {
        if (MakeLower(substring[j]) != MakeLower(string[i+j])) break;
     }
     if (j == substringL) return &string[i];
  }
  return NULL;
}

/******************************************************************************
* strcmpIgCase.
* It returns  1 if compared strings are identical with the same length
*             0 if compared (sub)strings are not same
******************************************************************************/

int strcmpIgCase (string1, string2)
char *string1;
char *string2;
{
  int string1L = strlen(string1);
  int string2L = strlen(string2);
  int i;
  if (string1L != string2L) return 0;
  for (i = 0; i < string1L; i++) {
     if (MakeLower(string1[i]) != MakeLower(string2[i]))
       return 0;
  }
  return 1;
}

/******************************************************************************
* strncmpIgCase... Assuming count is the length from one of passed strings.
*                  It compares characters upto the passed count.
* It returns  1 if compared strings are identical with same length
*             0 if compared (sub)strings are not same
******************************************************************************/

int strncmpIgCase (string1, string2, count)
char *string1;
char *string2;
size_t count;
{
  int string1L = (int) strlen(string1);
  int string2L = (int) strlen(string2);
  int maxL, i;
  maxL = MINIMUM(string2L,(int)count);
  maxL = MINIMUM(string1L,maxL);
  for (i = 0; i < maxL; i++) {
     if (i == string1L || i == string2L) break;
     if (MakeLower(string1[i]) != MakeLower(string2[i])) return 0;
  }
  if (string1L == string2L) return 1;
  else return 0;
}

/******************************************************************************
* strncmpIgCasePattern.
* It returns  0 if compared strings are the same for the specified length
*             <>0 if compared (sub)strings do not have the same pattern
******************************************************************************/

int strncmpIgCasePattern (string, pattern, count)
char *string;
char *pattern;
size_t count;
{
  int string1L = (int) strlen(string);
  int string2L = (int) strlen(pattern);
  int maxL, i;
  maxL = MINIMUM(string2L,(int)count);
  if (maxL > string1L) return -1;
  maxL = MINIMUM(maxL,string1L);
  for (i = 0; i < maxL; i++) {
    if (MakeLower(string[i]) != MakeLower(pattern[i])) {
      return ((int) (string[i] - pattern[i]));
    }
  }
  return 0;
}

/******************************************************************************
* strchrIgCase.
******************************************************************************/

char *strchrIgCase (str, chr)
char *str;
int chr;
{
  int strL = strlen(str);
  int i;
  if (strL == 0) return NULL;
  for (i = 0; i < strL; i++) {
     if (MakeLower(chr) == MakeLower(str[i])) return &str[i];
  }
  return NULL;
}

/******************************************************************************
* WriteEntryValue.
******************************************************************************/

void WriteEntryValue (fp, dataType, numElements, value, ccc, MaXcc, encode,
                      format)
FILE *fp;
long dataType;
long numElements;
void *value;
int ccc;                /* Current Cursor Column (at start). */
int MaXcc;              /* Maximum Cursor Column at which to output. */
int encode;             /* ==1 if ISO8601, ==2 if ISO8601 w/Z, ==0 if default.*/
char *format;
{
  if (STRINGdataType(dataType))
    WriteStringValue (fp, numElements, value, ccc, MaXcc);
  else {
    int i, elemN, n, cccBase = ccc;
    char evalue[80+1];
    Logical newLine = TRUE;
    int style;
    if (TT2000dataType(dataType)) {
      if (encode < 2) style = TT2000_3_STYLE;
      else style = TT2000_4_STYLE; 
    } else {        
      if (encode == 1) style = EPOCH4_STYLE;
      else if (encode == 2) style = EPOCH3_STYLE;
      else style = EPOCH0_STYLE;
    }
    for (elemN = 0; elemN < numElements; elemN++) {
       if (format == NULL)
         n = EncodeValue (dataType, (Byte1 *) value + elemN*CDFelemSize(dataType),
			  evalue, style, (size_t) sizeof(evalue));
       else
         n = EncodeValueFormat (dataType,
                                (Byte1 *) value + elemN*CDFelemSize(dataType),
				evalue, format, -1, 0, style,
				(size_t) sizeof(evalue));
       if (ccc + (newLine ? 0 : 1) + n +
	   (elemN != numElements - 1 ? 1 : 0) > MaXcc) {
	 WriteOut (fp, "\n");
	 for (i = 0; i < cccBase; i++) WriteOut (fp, " ");
	 ccc = cccBase;
	 newLine = TRUE;
       }
       if (!newLine) ccc += WriteOut (fp, " ");
       ccc += WriteOut (fp, evalue);
       if (elemN != numElements - 1) ccc += WriteOut (fp, ",");
       newLine = FALSE;
    }
  }
  return;
}

/******************************************************************************
* WriteEntryValueSKT.
******************************************************************************/

void WriteEntryValueSKT (fp, dataType, numElements, value, ccc, MaXcc, encode,
                         format)
FILE *fp;
long dataType;
long numElements;
void *value;
int ccc;                /* Current Cursor Column (at start). */
int MaXcc;              /* Maximum Cursor Column at which to output. */
int encode;             /* ==1 if ISO8601, ==2 if ISO8601 w/Z, ==0 if default.*/
char *format;
{
  if (STRINGdataType(dataType))
    WriteStringValue (fp, numElements, value, ccc, MaXcc);
  else {
    int i, elemN, n, cccBase = ccc;
    char evalue[80+1];
    Logical newLine = TRUE;
    int style;
    if (TT2000dataType(dataType)) {
      if (encode < 2) style = TT2000_3_STYLE;
      else style = TT2000_4_STYLE; 
    } else {        
      if (encode == 1) style = EPOCH4_STYLE;
      else if (encode == 2) style = EPOCH3_STYLE;
      else style = EPOCH0_STYLE;
    }
    for (elemN = 0; elemN < numElements; elemN++) {
       if (format == NULL)
         n = EncodeValueSKT (dataType, (Byte1 *) value +
					 elemN*CDFelemSize(dataType),
			     evalue, style, (size_t) sizeof(evalue));
       else
         n = EncodeValueFormat (dataType,
                                (Byte1 *) value + elemN*CDFelemSize(dataType),
				evalue, format, -1, 0, style,
				(size_t) sizeof(evalue));
       if (ccc + (newLine ? 0 : 1) + n +
	   (elemN != numElements - 1 ? 1 : 0) > MaXcc) {
	 WriteOut (fp, "\n");
	 for (i = 0; i < cccBase; i++) WriteOut (fp, " ");
	 ccc = cccBase;
	 newLine = TRUE;
       }
       if (!newLine) ccc += WriteOut (fp, " ");
       ccc += WriteOut (fp, evalue);
       if (elemN != numElements - 1) ccc += WriteOut (fp, ",");
       newLine = FALSE;
    }
  }
  return;
}

/******************************************************************************
* WriteStringValue.
*    Note that control characters may exist in the string being written (even
* a NUL character).  Control characters are to be replaced with `.'.  For this
* reason `memmove' is used to move around strings/substrings.
******************************************************************************/

void WriteStringValue (fp, numChars, value, ccc, MaXcc)
FILE *fp;
long numChars;
void *value;
int ccc;                /* Current Cursor Column (at start). */
int MaXcc;              /* Maximum Cursor Column at which to output. */
{
  int cccBase = ccc, i, withNul, len;
  char *string = (char *) value;
  char delim;
  size_t textL = MaXcc + 1;             /* MaXcc + 1. */
  char *text = (char *) cdf_AllocateMemory ((size_t) (textL + 1), FatalError);
  long numBytes2;
  withNul = -1;
  numBytes2 = (long) UTF8StrLength((char *)value, numChars);
  if ((ccc + 1 + numChars + 1 - 1) <= MaXcc) {
    delim = PickDelimiter (string, (size_t) numChars);
    text[0] = delim;
    memmove (&text[1], string, (size_t) numChars);
    if (numChars == numBytes2) {
      for (i = 1; i <= numChars; i++) {
         if (!Printable(text[i])) text[i] = '.';
      }
      text[(int)numChars+1] = delim;
      text[(int)numChars+2] = NUL;
    } else {
      text[(int)numBytes2+1] = delim;
      text[(int)numBytes2+2] = NUL;
    }
    WriteOut (fp, text);
  } else {
    int sss = MaXcc - ccc - 4 + 1;      /* 4 for starting `"' and ending `" -'
					   (assuming `"' is the delimeter). */
    char *substring = (char *) cdf_AllocateMemory ((size_t) (sss + 1),
						   FatalError);
    int x = 0, len2;
    while (x + sss < numChars) {
      if (string[x+sss] == ' ') /* First character next line will be a blank.*/
	len2 = sss;
      else {
	if (string[x+sss-1] == ' ') /* Last character on line is a blank. */
	  len2 = sss;
	else {                 /* Look for a blank at which to break line. */
	  for (i = x + sss - 2; i > x; i--) if (string[i] == ' ') break;
	  if (i > x)
	    len2 = i - x + 1;
	  else
	    len2 = sss;
	}
      }
      memmove (substring, &string[x], len2);
      substring[len2] = NUL;
      delim = PickDelimiter (substring, strlen(substring));
      snprintf (text, (size_t) textL+1, "%c%s%c -", delim, substring, delim);
      WriteOut (fp, text);
      WriteOut (fp, "\n");
      for (i = 0; i < cccBase; i++) WriteOut (fp, " ");
      x += len2;
    }
    len = (size_t) (numChars - x);
    memmove (substring, &string[x], len);
    substring[len] = NUL;
    delim = PickDelimiter (substring, strlen(substring));
    snprintf (text, (size_t) textL+1, "%c%s%c", delim, substring, delim);
    WriteOut (fp, text);
    if(substring != NULL) cdf_FreeMemory (substring, FatalError);
  }
  if(text != NULL) cdf_FreeMemory (text, FatalError);
  return;
}

/******************************************************************************
* ConvertDataType.
******************************************************************************/

void ConvertDataType (fromDataType, fromNumElems, fromPtr, toDataType,
		      toNumElems, toPtr)
long fromDataType;
long fromNumElems;              /* Only used if character data type. */
void *fromPtr;
long toDataType;
long toNumElems;                /* Only used if character data type. */
void *toPtr;
{
  switch (fromDataType) {
    /**************************************************************************
    * 1-byte signed integer to...
    **************************************************************************/
    case CDF_BYTE:
    case CDF_INT1:
      switch (toDataType) {
	case CDF_BYTE:
	case CDF_INT1:
	  *((sChar *) toPtr) = (sChar) *((sChar *) fromPtr);
	  break;
	case CDF_UINT1:
	  *((uChar *) toPtr) = (uChar) *((sChar *) fromPtr);
	  break;
	case CDF_INT2:
	  *((Int16 *) toPtr) = (Int16) *((sChar *) fromPtr);
	  break;
	case CDF_UINT2:
	  *((uInt16 *) toPtr) = (uInt16) *((sChar *) fromPtr);
	  break;
	case CDF_INT4:
	  *((Int32 *) toPtr) = (Int32) *((sChar *) fromPtr);
	  break;
	case CDF_INT8:
	case CDF_TIME_TT2000:
	  *((Int64 *) toPtr) = (Int64) *((sChar *) fromPtr);
	  break;
	case CDF_UINT4:
	  *((uInt32 *) toPtr) = (uInt32) *((sChar *) fromPtr);
	  break;
	case CDF_FLOAT:
	case CDF_REAL4:
	  *((float *) toPtr) = (float) *((sChar *) fromPtr);
	  break;
	case CDF_EPOCH:
	case CDF_DOUBLE:
	case CDF_REAL8:
	  *((double *) toPtr) = (double) *((sChar *) fromPtr);
	  break;
        case CDF_EPOCH16:
          *((double *) toPtr) = (double) *((sChar *) fromPtr);
	  *(((double *) toPtr) + 1) = (double) *(((sChar *) fromPtr) + 1);
          break;
	case CDF_CHAR:
	case CDF_UCHAR: {
	  int i, len;
	  char tmp[MAX_nonSTRING_VALUE_LEN+1];
	  snprintf (tmp, (size_t) sizeof(tmp), "%d", *((sChar *) fromPtr));
	  for (i = 0, len = strlen(tmp); i < toNumElems; i++) {
	     *((char *) toPtr + i) = (i < len ? tmp[i] : ' ');
	  }
	  break;
	}
      }
      break;
    /**************************************************************************
    * 1-byte unsigned integer to...
    **************************************************************************/
    case CDF_UINT1:
      switch (toDataType) {
	case CDF_BYTE:
	case CDF_INT1:
	  *((sChar *) toPtr) = (sChar) *((uChar *) fromPtr);
	  break;
	case CDF_UINT1:
	  *((uChar *) toPtr) = (uChar) *((uChar *) fromPtr);
	  break;
	case CDF_INT2:
	  *((Int16 *) toPtr) = (Int16) *((uChar *) fromPtr);
	  break;
	case CDF_UINT2:
	  *((uInt16 *) toPtr) = (uInt16) *((uChar *) fromPtr);
	  break;
	case CDF_INT4:
	  *((Int32 *) toPtr) = (Int32) *((uChar *) fromPtr);
	  break;
	case CDF_INT8:
	case CDF_TIME_TT2000:
	  *((Int64 *) toPtr) = (Int64) *((uChar *) fromPtr);
	  break;
	case CDF_UINT4:
	  *((uInt32 *) toPtr) = (uInt32) *((uChar *) fromPtr);
	  break;
	case CDF_FLOAT:
	case CDF_REAL4:
	  *((float *) toPtr) = (float) *((uChar *) fromPtr);
	  break;
	case CDF_EPOCH:
	case CDF_DOUBLE:
	case CDF_REAL8:
	  *((double *) toPtr) = (double) *((uChar *) fromPtr);
	  break;
        case CDF_EPOCH16:
          *((double *) toPtr) = (double) *((uChar *) fromPtr);
	  *(((double *) toPtr) + 1) = (double) *(((uChar *) fromPtr) + 1);
          break;
	case CDF_CHAR:
	case CDF_UCHAR: {
	  int i, len;
	  char tmp[MAX_nonSTRING_VALUE_LEN+1];
	  snprintf (tmp, (size_t) sizeof(tmp), "%u", *((uChar *) fromPtr));
	  for (i = 0, len = strlen(tmp); i < toNumElems; i++) {
	     *((char *) toPtr + i) = (i < len ? tmp[i] : ' ');
	  }
	  break;
	}
      }
      break;
    /**************************************************************************
    * 2-byte signed integer to...
    **************************************************************************/
    case CDF_INT2:
      switch (toDataType) {
	case CDF_BYTE:
	case CDF_INT1:
	  *((sChar *) toPtr) = (sChar) *((Int16 *) fromPtr);
	  break;
	case CDF_UINT1:
	  *((uChar *) toPtr) = (uChar) *((Int16 *) fromPtr);
	  break;
	case CDF_INT2:
	  *((Int16 *) toPtr) = (Int16) *((Int16 *) fromPtr);
	  break;
	case CDF_UINT2:
	  *((uInt16 *) toPtr) = (uInt16) *((Int16 *) fromPtr);
	  break;
	case CDF_INT4:
	  *((Int32 *) toPtr) = (Int32) *((Int16 *) fromPtr);
	  break;
	case CDF_INT8:
	case CDF_TIME_TT2000:
	  *((Int64 *) toPtr) = (Int64) *((Int16 *) fromPtr);
	  break;
	case CDF_UINT4:
	  *((uInt32 *) toPtr) = (uInt32) *((Int16 *) fromPtr);
	  break;
	case CDF_FLOAT:
	case CDF_REAL4:
	  *((float *) toPtr) = (float) *((Int16 *) fromPtr);
	  break;
	case CDF_EPOCH:
	case CDF_DOUBLE:
	case CDF_REAL8:
	  *((double *) toPtr) = (double) *((Int16 *) fromPtr);
	  break;
        case CDF_EPOCH16:
          *((double *) toPtr) = (double) *((Int16 *) fromPtr);
	  *(((double *) toPtr) + 1) = (double) *(((Int16 *) fromPtr) + 1);
          break;
	case CDF_CHAR:
	case CDF_UCHAR: {
	  int i, len;
	  char tmp[MAX_nonSTRING_VALUE_LEN+1];
	  snprintf (tmp, (size_t) sizeof(tmp),
		    "%d", (int) *((Int16 *) fromPtr));
	  for (i = 0, len = strlen(tmp); i < toNumElems; i++) {
	     *((char *) toPtr + i) = (i < len ? tmp[i] : ' ');
	  }
	  break;
	}
      }
      break;
    /**************************************************************************
    * 2-byte unsigned integer to...
    **************************************************************************/
    case CDF_UINT2:
      switch (toDataType) {
	case CDF_BYTE:
	case CDF_INT1:
	  *((sChar *) toPtr) = (sChar) *((uInt16 *) fromPtr);
	  break;
	case CDF_UINT1:
	  *((uChar *) toPtr) = (uChar) *((uInt16 *) fromPtr);
	  break;
	case CDF_INT2:
	  *((Int16 *) toPtr) = (Int16) *((uInt16 *) fromPtr);
	  break;
	case CDF_UINT2:
	  *((uInt16 *) toPtr) = (uInt16) *((uInt16 *) fromPtr);
	  break;
	case CDF_INT4:
	  *((Int32 *) toPtr) = (Int32) *((uInt16 *) fromPtr);
	  break;
	case CDF_INT8:
	case CDF_TIME_TT2000:
	  *((Int64 *) toPtr) = (Int64) *((uInt16 *) fromPtr);
	  break;
	case CDF_UINT4:
	  *((uInt32 *) toPtr) = (uInt32) *((uInt16 *) fromPtr);
	  break;
	case CDF_FLOAT:
	case CDF_REAL4:
	  *((float *) toPtr) = (float) *((uInt16 *) fromPtr);
	  break;
	case CDF_EPOCH:
	case CDF_DOUBLE:
	case CDF_REAL8:
	  *((double *) toPtr) = (double) *((uInt16 *) fromPtr);
	  break;
        case CDF_EPOCH16:
          *((double *) toPtr) = (double) *((uInt16 *) fromPtr);
	  *(((double *) toPtr) + 1) = (double) *(((uInt16 *) fromPtr) + 1);
          break;
	case CDF_CHAR:
	case CDF_UCHAR: {
	  int i, len;
	  char tmp[MAX_nonSTRING_VALUE_LEN+1];
	  snprintf (tmp, (size_t) sizeof(tmp),
	  	    "%u", (int) *((uInt16 *) fromPtr));
	  for (i = 0, len = strlen(tmp); i < toNumElems; i++) {
	     *((char *) toPtr + i) = (i < len ? tmp[i] : ' ');
	  }
	  break;
	}
      }
      break;
    /**************************************************************************
    * 4-byte signed integer to...
    **************************************************************************/
    case CDF_INT4:
      switch (toDataType) {
	case CDF_BYTE:
	case CDF_INT1:
	  *((sChar *) toPtr) = (sChar) *((Int32 *) fromPtr);
	  break;
	case CDF_UINT1:
	  *((uChar *) toPtr) = (uChar) *((Int32 *) fromPtr);
	  break;
	case CDF_INT2:
	  *((Int16 *) toPtr) = (Int16) *((Int32 *) fromPtr);
	  break;
	case CDF_UINT2:
	  *((uInt16 *) toPtr) = (uInt16) *((Int32 *) fromPtr);
	  break;
	case CDF_INT4:
	  *((Int32 *) toPtr) = (Int32) *((Int32 *) fromPtr);
	  break;
	case CDF_INT8:
	case CDF_TIME_TT2000:
	  *((Int64 *) toPtr) = (Int64) *((Int32 *) fromPtr);
	  break;
	case CDF_UINT4:
	  *((uInt32 *) toPtr) = (uInt32) *((Int32 *) fromPtr);
	  break;
	case CDF_FLOAT:
	case CDF_REAL4:
	  *((float *) toPtr) = (float) *((Int32 *) fromPtr);
	  break;
	case CDF_EPOCH:
	case CDF_DOUBLE:
	case CDF_REAL8:
	  *((double *) toPtr) = (double) *((Int32 *) fromPtr);
	  break;
        case CDF_EPOCH16:
          *((double *) toPtr) = (double) *((Int32 *) fromPtr);
	  *(((double *) toPtr) + 1) = (double) *(((Int32 *) fromPtr) + 1);
          break;
	case CDF_CHAR:
	case CDF_UCHAR: {
	  int i, len;
	  char tmp[MAX_nonSTRING_VALUE_LEN+1];
	  snprintf (tmp, (size_t) sizeof(tmp),
		    "%ld", (long) *((Int32 *) fromPtr));
	  for (i = 0, len = strlen(tmp); i < toNumElems; i++) {
	     *((char *) toPtr + i) = (i < len ? tmp[i] : ' ');
	  }
	  break;
	}
      }
      break;
    /**************************************************************************
    * 4-byte unsigned integer to...
    **************************************************************************/
    case CDF_UINT4:
      switch (toDataType) {
	case CDF_BYTE:
	case CDF_INT1:
	  *((sChar *) toPtr) = (sChar) *((uInt32 *) fromPtr);
	  break;
	case CDF_UINT1:
	  *((uChar *) toPtr) = (uChar) *((uInt32 *) fromPtr);
	  break;
	case CDF_INT2:
	  *((Int16 *) toPtr) = (Int16) *((uInt32 *) fromPtr);
	  break;
	case CDF_UINT2:
	  *((uInt16 *) toPtr) = (uInt16) *((uInt32 *) fromPtr);
	  break;
	case CDF_INT4:
	  *((Int32 *) toPtr) = (Int32) *((uInt32 *) fromPtr);
	  break;
	case CDF_INT8:
	case CDF_TIME_TT2000:
	  *((Int64 *) toPtr) = (Int64) *((uInt32 *) fromPtr);
	  break;
	case CDF_UINT4:
	  *((uInt32 *) toPtr) = (uInt32) *((uInt32 *) fromPtr);
	  break;
	case CDF_FLOAT:
	case CDF_REAL4:
	  *((float *) toPtr) = (float) *((uInt32 *) fromPtr);
	  break;
	case CDF_EPOCH:
	case CDF_DOUBLE:
	case CDF_REAL8:
	  *((double *) toPtr) = (double) *((uInt32 *) fromPtr);
	  break;
        case CDF_EPOCH16:
          *((double *) toPtr) = (double) *((uInt32 *) fromPtr);
	  *(((double *) toPtr) + 1) = (double) *(((uInt32 *) fromPtr) + 1);
          break;
	case CDF_CHAR:
	case CDF_UCHAR: {
	  int i, len;
	  char tmp[MAX_nonSTRING_VALUE_LEN+1];
	  snprintf (tmp, (size_t) sizeof(tmp),
		    "%lu", (uLongx) *((uInt32 *) fromPtr));
	  for (i = 0, len = strlen(tmp); i < toNumElems; i++) {
	     *((char *) toPtr + i) = (i < len ? tmp[i] : ' ');
	  }
	  break;
	}
      }
      break;
    /**************************************************************************
    * 8-byte signed integer to...
    **************************************************************************/
    case CDF_INT8:
    case CDF_TIME_TT2000:
      switch (toDataType) {
	case CDF_BYTE:
	case CDF_INT1:
	  *((sChar *) toPtr) = (sChar) *((Int64 *) fromPtr);
	  break;
	case CDF_UINT1:
	  *((uChar *) toPtr) = (uChar) *((Int64 *) fromPtr);
	  break;
	case CDF_INT2:
	  *((Int16 *) toPtr) = (Int16) *((Int64 *) fromPtr);
	  break;
	case CDF_UINT2:
	  *((uInt16 *) toPtr) = (uInt16) *((Int64 *) fromPtr);
	  break;
	case CDF_INT4:
	  *((Int32 *) toPtr) = (Int32) *((Int64 *) fromPtr);
	  break;
	case CDF_INT8:
	case CDF_TIME_TT2000:
	  *((Int64 *) toPtr) = (Int64) *((Int64 *) fromPtr);
	  break;
	case CDF_UINT4:
	  *((uInt32 *) toPtr) = (uInt32) *((Int64 *) fromPtr);
	  break;
	case CDF_FLOAT:
	case CDF_REAL4:
	  *((float *) toPtr) = (float) *((Int64 *) fromPtr);
	  break;
	case CDF_EPOCH:
	case CDF_DOUBLE:
	case CDF_REAL8:
	  *((double *) toPtr) = (double) *((Int64 *) fromPtr);
	  break;
        case CDF_EPOCH16:
          *((double *) toPtr) = (double) *((Int64 *) fromPtr);
	  *(((double *) toPtr) + 1) = (double) *(((Int64 *) fromPtr) + 1);
          break;
	case CDF_CHAR:
	case CDF_UCHAR: {
	  int i, len;
	  char tmp[MAX_nonSTRING_VALUE_LEN+1];
	  snprintf (tmp, (size_t) sizeof(tmp),
		    "%lld", (long long) *((Int64 *) fromPtr));
	  for (i = 0, len = strlen(tmp); i < toNumElems; i++) {
	     *((char *) toPtr + i) = (i < len ? tmp[i] : ' ');
	  }
	  break;
	}
      }
      break;
    /**************************************************************************
    * 4-byte floating-point to...
    **************************************************************************/
    case CDF_FLOAT:
    case CDF_REAL4:
      switch (toDataType) {
	case CDF_BYTE:
	case CDF_INT1:
	  *((sChar *) toPtr) = (sChar) *((float *) fromPtr);
	  break;
	case CDF_UINT1:
	  *((uChar *) toPtr) = (uChar) *((float *) fromPtr);
	  break;
	case CDF_INT2:
	  *((Int16 *) toPtr) = (Int16) *((float *) fromPtr);
	  break;
	case CDF_UINT2:
	  *((uInt16 *) toPtr) = (uInt16) *((float *) fromPtr);
	  break;
	case CDF_INT4:
	  *((Int32 *) toPtr) = (Int32) *((float *) fromPtr);
	  break;
	case CDF_INT8:
	case CDF_TIME_TT2000:
	  *((Int64 *) toPtr) = (Int64) *((float *) fromPtr);
	  break;
	case CDF_UINT4:
	  *((uInt32 *) toPtr) = (uInt32) *((float *) fromPtr);
	  break;
	case CDF_FLOAT:
	case CDF_REAL4:
	  *((float *) toPtr) = (float) *((float *) fromPtr);
	  break;
	case CDF_EPOCH:
	case CDF_DOUBLE:
	case CDF_REAL8:
	  *((double *) toPtr) = (double) *((float *) fromPtr);
	  break;
        case CDF_EPOCH16:
          *((double *) toPtr) = (double) *((float *) fromPtr);
	  *(((double *) toPtr) + 1) = (double) *(((float *) fromPtr) + 1);
          break;
	case CDF_CHAR:
	case CDF_UCHAR: {
	  int i, len;
	  char tmp[MAX_nonSTRING_VALUE_LEN+1];
	  snprintf (tmp, (size_t) sizeof(tmp), "%f", *((float *) fromPtr));
	  for (i = 0, len = strlen(tmp); i < toNumElems; i++) {
	     *((char *) toPtr + i) = (i < len ? tmp[i] : ' ');
	  }
	  break;
	}
      }
      break;
    /**************************************************************************
    * 8-byte floating-point to...
    **************************************************************************/
    case CDF_EPOCH:
    case CDF_DOUBLE:
    case CDF_REAL8:
      switch (toDataType) {
	case CDF_BYTE:
	case CDF_INT1:
	  *((sChar *) toPtr) = (sChar) *((double *) fromPtr);
	  break;
	case CDF_UINT1:
	  *((uChar *) toPtr) = (uChar) *((double *) fromPtr);
	  break;
	case CDF_INT2:
	  *((Int16 *) toPtr) = (Int16) *((double *) fromPtr);
	  break;
	case CDF_UINT2:
	  *((uInt16 *) toPtr) = (uInt16) *((double *) fromPtr);
	  break;
	case CDF_INT4:
	  *((Int32 *) toPtr) = (Int32) *((double *) fromPtr);
	  break;
	case CDF_INT8:
	case CDF_TIME_TT2000:
	  *((Int64 *) toPtr) = (Int64) *((double *) fromPtr);
	  break;
	case CDF_UINT4:
	  *((uInt32 *) toPtr) = (uInt32) *((double *) fromPtr);
	  break;
	case CDF_FLOAT:
	case CDF_REAL4:
	  *((float *) toPtr) = (float) *((double *) fromPtr);
	  break;
	case CDF_EPOCH:
	case CDF_DOUBLE:
	case CDF_REAL8:
	  *((double *) toPtr) = (double) *((double *) fromPtr);
	  break;
	case CDF_CHAR:
	case CDF_UCHAR: {
	  int i, len;
	  char tmp[MAX_nonSTRING_VALUE_LEN+1];
	  snprintf (tmp, (size_t) sizeof(tmp), "%f", *((double *) fromPtr));
	  for (i = 0, len = strlen(tmp); i < toNumElems; i++) {
	     *((char *) toPtr + i) = (i < len ? tmp[i] : ' ');
	  }
	  break;
	}
      }
      break;
    /**************************************************************************
    * 16-byte floating-point to...
    **************************************************************************/
    case CDF_EPOCH16:
      switch (toDataType) {
        case CDF_BYTE:
        case CDF_INT1:
          *((sChar *) toPtr) = (sChar) *((double *) fromPtr);
	  *(((sChar *) toPtr) + 1) = (sChar) *(((double *) fromPtr) + 1);
          break;
        case CDF_UINT1:
          *((uChar *) toPtr) = (uChar) *((double *) fromPtr);
	  *(((uChar *) toPtr) + 1) = (uChar) *(((double *) fromPtr) + 1);
          break;
        case CDF_INT2:
          *((Int16 *) toPtr) = (Int16) *((double *) fromPtr);
	  *(((Int16 *) toPtr) + 1) = (Int16) *(((double *) fromPtr) + 1);
          break;
        case CDF_UINT2:
          *((uInt16 *) toPtr) = (uInt16) *((double *) fromPtr);
	  *(((uInt16 *) toPtr) + 1) = (uInt16) *(((double *) fromPtr) + 1);
          break;
        case CDF_INT4:
          *((Int32 *) toPtr) = (Int32) *((double *) fromPtr);
	  *(((Int32 *) toPtr) + 1) = (Int32) *(((double *) fromPtr) + 1);
          break;
        case CDF_INT8:
	case CDF_TIME_TT2000:
          *((Int64 *) toPtr) = (Int64) *((double *) fromPtr);
	  *(((Int64 *) toPtr) + 1) = (Int64) *(((double *) fromPtr) + 1);
          break;
        case CDF_UINT4:
          *((uInt32 *) toPtr) = (uInt32) *((double *) fromPtr);
	  *(((uInt32 *) toPtr) + 1) = (uInt32) *(((double *) fromPtr) + 1);
          break;
        case CDF_FLOAT:
        case CDF_REAL4:
          *((float *) toPtr) = (float) *((double *) fromPtr);
	  *(((float *) toPtr) + 1) = (float) *(((double *) fromPtr) + 1);
          break;
        case CDF_EPOCH:
        case CDF_DOUBLE:
        case CDF_REAL8:
          *((double *) toPtr) = (double) *((double *) fromPtr);
	  *(((double *) toPtr) + 1) = (double) *(((double *) fromPtr) + 1);
          break;
        case CDF_EPOCH16:
          *((double *) toPtr) = (double) *((double *) fromPtr);
          *(((double *) toPtr) + 1) = (double) *(((double *) fromPtr) + 1);
          break;
        case CDF_CHAR:
        case CDF_UCHAR: {
          int i, j, len;
          char tmp[MAX_nonSTRING_VALUE_LEN+1];
          snprintf (tmp, (size_t) sizeof(tmp),
		    "%f", *((double *) fromPtr));
          for (i = 0, len = strlen(tmp); i < toNumElems; i++) {
             *((char *) toPtr + i) = (i < len ? tmp[i] : ' ');
          }
          snprintf (tmp, (size_t) sizeof(tmp),
		    "%f", *(((double *) fromPtr) + 1));
	  j = i;
          for (i = 0, len = strlen(tmp); i < toNumElems; i++) {
             *((char *) toPtr + j) = (i < len ? tmp[i] : ' ');
          }
          break;
        }
      }
      break;
    /**************************************************************************
    * character (string) to...
    **************************************************************************/
    case CDF_CHAR:
    case CDF_UCHAR: {
      size_t nBytes = (size_t) (fromNumElems + 1);
      char *str = (char *) cdf_AllocateMemory (nBytes, FatalError);
      memmove (str, fromPtr, (size_t) fromNumElems);
      str[(int)fromNumElems] = NUL;
      switch (toDataType) {
	case CDF_BYTE:
	case CDF_INT1: {
	  int tmp;
	  if (sscanf(str,"%d",&tmp) < 1)
	    *((sChar *) toPtr) = 0;
	  else
	    *((sChar *) toPtr) = (sChar) tmp;
	  break;
	}
	case CDF_UINT1: {
	  uInt4 tmp;
	  if (sscanf(str,"%u",&tmp) < 1)
	    *((uChar *) toPtr) = 0;
	  else
	    *((uChar *) toPtr) = (uChar) tmp;
	  break;
	}
	case CDF_INT2: {
	  int tmp;
	  if (sscanf(str,"%d",&tmp) < 1)
	    *((Int16 *) toPtr) = 0;
	  else
	    *((Int16 *) toPtr) = (Int16) tmp;
	  break;
	}
	case CDF_UINT2: {
	  uInt4 tmp;
	  if (sscanf(str,"%u",&tmp) < 1)
	    *((uInt16 *) toPtr) = 0;
	  else
	    *((uInt16 *) toPtr) = (uInt16) tmp;
	  break;
	}
	case CDF_INT4: {
	  long tmp;
	  if (sscanf(str,"%ld",&tmp) < 1)
	    *((Int32 *) toPtr) = 0;
	  else
	    *((Int32 *) toPtr) = (Int32) tmp;
	  break;
	}
	case CDF_INT8:
	case CDF_TIME_TT2000: {
	  long long tmp;
	  if (sscanf(str,"%lld",&tmp) < 1)
	    *((Int64 *) toPtr) = 0;
	  else
	    *((Int64 *) toPtr) = (Int64) tmp;
	  break;
	}
	case CDF_UINT4: {
	  uLongx tmp;
	  if (sscanf(str,"%lu",&tmp) < 1)
	    *((uInt32 *) toPtr) = 0;
	  else
	    *((uInt32 *) toPtr) = (uInt32) tmp;
	  break;
	}
	case CDF_FLOAT:
	case CDF_REAL4: {
	  float tmp;
	  if (sscanf(str,"%g",&tmp) < 1)
	    *((float *) toPtr) = (float) 0.0;
	  else
	    *((float *) toPtr) = tmp;
	  break;
	}
	case CDF_EPOCH:
	case CDF_DOUBLE:
	case CDF_REAL8: {
	  double tmp;
	  if (sscanf(str,"%lg",&tmp) < 1)
	    *((double *) toPtr) = 0.0;
	  else
	    *((double *) toPtr) = tmp;
	  break;
	}
        case CDF_EPOCH16: {
          double tmp1, tmp2;
          if (sscanf(str,"%lg %lg",&tmp1, &tmp2) < 2) {
            *((double *) toPtr) = 0.0;
	    *(((double *) toPtr) + 1) = 0.0;
          } else {
            *((double *) toPtr) = tmp1;
	    *(((double *) toPtr) + 1) = tmp2;
	  }
          break;
        }
	case CDF_CHAR:
	case CDF_UCHAR: {
	  int i;
	  for (i = 0; i < toNumElems; i++) {
	     *((char *) toPtr + i) = (i < fromNumElems ? str[i] : ' ');
	  }
	  break;
	}
      }
      cdf_FreeMemory (str, FatalError);
      break;
    }
  }
  return;
}

/******************************************************************************
* DecodeValues.
*     Memory for the values is allocated here but is NOT freed (unless an
* error occurred.)
******************************************************************************/

Logical DecodeValues (string, dataType, numElems, values, style)
char *string;
long dataType;
long *numElems;
void **values;
int style;              /* EPOCH style. */
{
if (STRINGdataType(dataType)) {
  /****************************************************************************
  * Delimited character string.
  ****************************************************************************/
  char delim;
  char *firstDelim, *secondDelim;
  char *ptr = string;
  while (*ptr != NUL && Spacing(*ptr)) ptr++;    /* Skip to delimiter. */
  if (*ptr == NUL) return FALSE;                    /* Null-string. */
  delim = *ptr;
  firstDelim = ptr;
  secondDelim = strchr (ptr + 1, delim);
  if (secondDelim == NULL) return FALSE;
  *numElems = secondDelim - firstDelim - 1;
  *values = cdf_AllocateMemory ((size_t) ((size_t)*numElems + 1), FatalError);
  memmove (*values, firstDelim + 1, (size_t) *numElems);
  ((char *) *values)[(int)(*numElems)] = NUL;
}
else {
  /****************************************************************************
  * Comma separated values.
  ****************************************************************************/
  int i, Ncommas; char *ptr; int len, plus, hex;
  for (Ncommas = 0, i = 0; string[i] != NUL; i++)       /* Count commas. */
     if (string[i] == ',') Ncommas++;
  *numElems = Ncommas + 1;
  *values = cdf_AllocateMemory ((size_t) ((size_t)*numElems *
					  CDFelemSize(dataType)),
			        FatalError);
  for (ptr = string, i = 0; i < *numElems; i++) {
     len = (int) strlen(ptr);
     plus = 0;
     hex = 0;
     if (len > 2 && *ptr == '0' && (*(ptr+1) == 'X' || *(ptr+1) == 'x')) {
	plus = 2;
	hex = 1;
     }
     switch (dataType) {
       case CDF_BYTE:
       case CDF_INT1: {
	 int temp;
	 if (sscanf(ptr+plus,(hex==0?"%d":"%X"),&temp) != 1) {
	   cdf_FreeMemory (*values, FatalError);
	   return FALSE;
	 }
	 if (temp < -128 || temp > 127) {
	   cdf_FreeMemory (*values, FatalError);
	   return FALSE;
	 }
	 *((sChar *) *values + i) = (sChar) temp;
	 break;
       }
       case CDF_UINT1: {
	 int temp;
	 if (sscanf(ptr+plus,(hex==0?"%d":"%X"),&temp) != 1) {
	   cdf_FreeMemory (*values, FatalError);
	   return FALSE;
	 }
	 if (temp < 0 || temp > 255) {
	   cdf_FreeMemory (*values, FatalError);
	   return FALSE;
	 }
	 *((uChar *) *values + i) = (uChar) temp;
	 break;
       }
       case CDF_INT2: {
	 int temp;
	 if (sscanf(ptr+plus,(hex==0?"%d":"%X"),&temp) != 1) {
	   cdf_FreeMemory (*values, FatalError);
	   return FALSE;
	 }
	 *((Int16 *) *values + i) = (Int16) temp;
	 break;
       }
       case CDF_UINT2: {
	 int temp;
	 if (sscanf(ptr+plus,(hex==0?"%d":"%X"),&temp) != 1) {
	   cdf_FreeMemory (*values, FatalError);
	   return FALSE;
	 }
	 if (temp < 0 || temp > 65535L) {
	   cdf_FreeMemory (*values, FatalError);
	   return FALSE;
	 }
	 *((uInt16 *) *values + i) = (uInt16) temp;
	 break;
       }
       case CDF_INT4:
	 if (!DecodeInt32(ptr+plus,hex,(Int32 *) *values + i)) {
	   cdf_FreeMemory (*values, FatalError);
	   return FALSE;
	 }
	 break;
       case CDF_INT8:
	 if (!DecodeInt64(ptr+plus,hex,(Int64 *) *values + i)) {
	   cdf_FreeMemory (*values, FatalError);
	   return FALSE;
	 }
	 break;
       case CDF_UINT4:
	 if (!DecodeInt32u(ptr+plus,hex,(uInt32 *) *values + i)) {
	   cdf_FreeMemory (*values, FatalError);
	   return FALSE;
	 }
	 break;
       case CDF_REAL4:
       case CDF_FLOAT:
	 if ((strstrIgCase(ptr+plus, "nan") == NULL) &&
	      (strstrIgCase(ptr+plus, "inf") == NULL)) {
	   int ret;
	   if (hex == 0)
	     ret = sscanf(ptr+plus,"%f",(float *) *values + i);
	   else
	     ret = sscanf(ptr+plus,"%X",(int *) *values + i);
	   if (ret != 1) {
	     cdf_FreeMemory (*values, FatalError);
	     return FALSE;
	   }
	 } else if (strstrIgCase(ptr+plus, "nan") != NULL) {
#if !defined(win32) && !defined(vms)
	   if (sscanf(ptr+plus,"%f",(float *) *values + i) != 1) {
	     cdf_FreeMemory (*values, FatalError);
	     return FALSE;
	   }
#else
           unsigned int nan= 0x7fffffff;
           *((float *) *values + i) = *(float *)(&nan);
#endif
	 } else {
#if !defined(win32) && !defined(vms)
	   if (sscanf(ptr+plus,"%f",(float *) *values + i) != 1) {
	     cdf_FreeMemory (*values, FatalError);
	     return FALSE;
	   }
#else
           float inf = 0.0f;
           if (strstrIgCase(ptr+plus, "-") != NULL)
             *((float *) *values + i) = (float) -1.0f/inf;
           else
             *((float *) *values + i) = (float) 1.0f/inf;
#endif
         }
	 break;
       case CDF_REAL8:
       case CDF_DOUBLE:
	 if ((strstrIgCase(ptr+plus, "nan") == NULL) &&
             (strstrIgCase(ptr+plus, "inf") == NULL)) {
	   int ret;
	   if (hex == 0)
	     ret = sscanf(ptr+plus,"%lf",(double *) *values + i);
	   else
	     ret = sscanf(ptr+plus,"%llX",(long long *) *values + i);
	   if (ret != 1) {
	     cdf_FreeMemory (*values, FatalError);
	     return FALSE;
	   }
	 } else if (strstrIgCase(ptr+plus, "nan") != NULL) {
#if !defined(win32) && !defined(vms)
	   if (sscanf(ptr+plus,"%lf",(double *) *values + i) != 1) {
	     cdf_FreeMemory (*values, FatalError);
	     return FALSE;
	   }
#else
           unsigned int nan[2] = {0xffffffff, 0x7fffffff};
           *((double *) *values + i) = *(double *)nan;
#endif
	 } else {
#if !defined(win32) && !defined(vms)
	   if (sscanf(ptr+plus,"%lf",(double *) *values + i) != 1) {
	     cdf_FreeMemory (*values, FatalError);
	     return FALSE;
	   }
#else
           double inf = 0.0;
           if (strstrIgCase(ptr+plus, "-") != NULL)
             *((double *) *values + i) = (double)-1.0/inf;
           else
             *((double *) *values + i) = (double)1.0/inf;
#endif
         }
	 break;
       case CDF_EPOCH: {
         double value;
         if (strstrIgCase(ptr, "nan") != NULL) {
#if !defined(win32) && !defined(vms)
           value = NAN;
#else
           int zero = 0;
           value = 0.0 / zero;
#endif
         } else {
           value = 0.0;
           if (style <= EPOCH3_STYLE || style == EPOCH4_STYLE) 
             value = toParseEPOCH(ptr);
           else if (style == EPOCHf_STYLE) {
             cdf_FreeMemory (*values, FatalError);
             return DecodeValues(string,CDF_DOUBLE,numElems,values,style);
           } else if (style == EPOCHx_STYLE) {
             value = toParseEPOCH (ptr);  /*Assume EPOCH0_STYLE instead.*/
           }
	 }
	 if (value == ILLEGAL_EPOCH_VALUE) {
	   cdf_FreeMemory (*values, FatalError);
	   return FALSE;
	 }
	 else
	   *((double *) *values + i) = value;
	 break;
       }
       case CDF_EPOCH16: {
         double value[2], dummy = 0.0;
         if (strstrIgCase(ptr, "nan") != NULL) {
#if !defined(win32) && !defined(vms)
           value[0] = NAN;
           value[1] = NAN;
#else
           int zero = 0;
           value[0] = 0.0 / zero;
           value[1] = 0.0 / zero;
#endif
         } else {
           if (style <= EPOCH3_STYLE || style == EPOCH4_STYLE)
             dummy = toParseEPOCH16(ptr, value);
           else if (style == EPOCHf_STYLE) {
             cdf_FreeMemory (*values, FatalError);
             return DecodeValues(string,CDF_DOUBLE,numElems,values,style);
           } else if (style == EPOCHx_STYLE) { /*Assume EPOCH0_STYLE instead.*/
             dummy = toParseEPOCH16 (ptr, value);
           }
         }
         if (dummy == ILLEGAL_EPOCH_VALUE ||
	     value[0] == ILLEGAL_EPOCH_VALUE ||
	     value[1] == ILLEGAL_EPOCH_VALUE) {
           cdf_FreeMemory (*values, FatalError);
           return FALSE;
         }
         else {
	   int j = 2 * i;
           *((double *) *values + j) = value[0];
	   *(((double *) *values + j) + 1) = value[1];
	 }
         break;
       }
       case CDF_TIME_TT2000: {
         long long tt2000;
         tt2000 = CDF_TT2000_from_UTC_string (ptr);
         *((long long *) *values + i) = tt2000;
         break;
       }
     }
     ptr = strchr (ptr+plus, ',') + 1; /* Garbage last time, doesn't matter. */
  }
}
return TRUE;
}

/******************************************************************************
* DecodeInt32.
*    This routine is necessary because DEC Alphas running OpenVMS do not
* decode "-2147483648" properly.
******************************************************************************/

static Logical DecodeInt32 (text, hex, binary)
char *text;
int hex;
Int32 *binary;
{
  if (EqStringsIgLTws(text,"-2147483648"))
#if defined(dos)
    *binary = (Int32) (DEFAULT_INT4_PADVALUE - 1);
#else
    *binary = (Int32) 0x80000000;
#endif
  else
    if (sscanf(text,(hex==0?Int32FORMAT:"%X"),binary) != 1) return FALSE;
  return TRUE;
}

/******************************************************************************
* DecodeInt32u.
*    This routine is necessary because VAX/VMS systems do not properly decode
* unsigned values.
******************************************************************************/

static Logical DecodeInt32u (text, hex, binary)
char *text;
int hex;
uInt32 *binary;
{
   char *start = text;
   char *end, temp[MAX_ENCODED_INT4_LEN+1];
   int len; unsigned int tempB;
   while (*start != NUL && Spacing(*start)) start++;
   if (*start == NUL) return FALSE;
   end = start;
   if (hex == 0) while (*end != NUL && Decimal(*end)) end++;
   len = (int) (end - start);
   if (len > MAX_ENCODED_INT4_LEN) return FALSE;
   strcpyX (temp, start, MINIMUM(len,MAX_ENCODED_INT4_LEN));
   if (len < MAX_ENCODED_INT4_LEN) {
     if (sscanf(temp,(hex==0?"%d":"%X"),&tempB) != 1) return FALSE;
   }
   else {
     if (strcmp(temp,"4294967295") > 0) return FALSE;
     if (sscanf(&temp[1],(hex==0?"%d":"%X"),&tempB) != 1) return FALSE;
#if defined(dos)
     tempB += (1000000000UL * (temp[0] - '0'));
#else
     tempB += ((uLongx) 1000000000 * (temp[0] - '0'));
#endif
   }
   *binary = (uInt32) tempB;
   return TRUE;
}

/******************************************************************************
* DecodeInt64.
*    This routine is necessary because DEC Alphas running OpenVMS do not
* decode "-9223372036854775808" properly (?).
******************************************************************************/

static Logical DecodeInt64 (text, hex, binary)
char *text;
int hex;
Int64 *binary;
{
  if (EqStringsIgLTws(text,"-9223372036854775808"))
#if defined(dos)
    *binary = (Int64) DEFAULT_INT8_PADVALUE - 1;
#else
    *binary = (Int64) 0x8000000000000000LL;
#endif
  else
    if (sscanf(text,(hex==0?Int64FORMAT:Int64hexFORMAT),(long long *)binary)
        != 1) return FALSE;
  return TRUE;
}

/******************************************************************************
* EqStringsIgLTws.
*     Compares two strings ignoring leading and trailing white space.
******************************************************************************/

static Logical EqStringsIgLTws (string1, string2)
char *string1;
char *string2;
{
  char *tString1, *tString2, *ptr;
  size_t nChars;
  Logical returnValue;
  /****************************************************************************
  * Strip leading and trailing white space from 1st string.
  ****************************************************************************/
  ptr = string1;
  while (*ptr != NUL && Spacing(*ptr)) ptr++;
  nChars = strlen (ptr);
  tString1 = (char *) cdf_AllocateMemory ((size_t)nChars + 1, FatalError);
  strcpyX (tString1, ptr, 0);
  ptr = tString1 + nChars;
  while (ptr != tString1 && Spacing(*(ptr-1))) ptr--;
  *ptr = NUL;
  /****************************************************************************
  * Strip leading and trailing white space from 2nd string.
  ****************************************************************************/
  ptr = string2;
  while (*ptr != NUL && Spacing(*ptr)) ptr++;
  nChars = strlen (ptr);
  tString2 = (char *) cdf_AllocateMemory ((size_t)nChars + 1, FatalError);
  strcpyX (tString2, ptr, 0);
  ptr = tString2 + nChars;
  while (ptr != tString2 && Spacing(*(ptr-1))) ptr--;
  *ptr = NUL;
  /****************************************************************************
  * Free temporary strings and return result of comparison.
  ****************************************************************************/
  returnValue = (!strcmp(tString1,tString2) ? TRUE : FALSE);
  cdf_FreeMemory (tString1, FatalError);
  cdf_FreeMemory (tString2, FatalError);
  return returnValue;
}

/******************************************************************************
* IntWidth.
******************************************************************************/

int IntWidth (value)
int value;
{
  int len;
  if (value > 0) 
    len = log10(value) + 1;
  else if (value < 0) {
    if (value == (DEFAULT_INT4_PADVALUE-1)) len = 11;
    else len = log10(-value) + 2;
  } else len = 1;
  return len;
}

/******************************************************************************
* Long64Width.
******************************************************************************/

int Long64Width (value)
OFF_T value;
{
  int len;
  if (value > 0)
    len = log10((long long)value) + 1;
  else if (value < 0) {
    if ((long long)value == (long long)DEFAULT_INT8_PADVALUE-1) len = 20; 
    else len = log10(-value) + 2;
  } else
    len = 1;
  return len;
}

/******************************************************************************
* ParseCacheSizes.
* It can be assumed that there is at least one character in `sizes' or QOP
* would have failed.
******************************************************************************/

Logical  ParseCacheSizes (sizes, workingCache, stageCache, compressCache)
char *sizes;
long *workingCache;
long *stageCache;
long *compressCache;
{
  char *p, *size, tmp[MAX_CACHESIZES_LEN+1];
  /****************************************************************************
  * Initialize all cache sizes in case only some have been specified.
  ****************************************************************************/
  *workingCache = useDEFAULTcacheSIZE;
  *stageCache = useDEFAULTcacheSIZE;
  *compressCache = useDEFAULTcacheSIZE;
  /****************************************************************************
  * Move cache sizes to a temporary buffer and remove all spacing.
  ****************************************************************************/
  strcpyX (tmp, sizes, MAX_CACHESIZES_LEN);
  RemoveWhiteSpace (tmp);
  p = tmp;
  /****************************************************************************
  * Check for (and skip) parenthesis on VMS systems.
  ****************************************************************************/
#if defined(vms)
  if (*p == '(') {
    char *lastChar = p + strlen(p) - 1;
    if (*lastChar != ')') return FALSE;
    p++;
    *lastChar = NUL;
  }
#endif
  /****************************************************************************
  * Scan the list of cache sizes...
  ****************************************************************************/
  for (;;) {
     /*************************************************************************
     * Save the location of the size and increment to the next non-decimal
     * character (ie. past the size).
     *************************************************************************/
     size = p;
     while (Decimal(*p)) p++;
     /*************************************************************************
     * Based on the non-decimal character reached...
     *************************************************************************/
     switch (*p) {
       case 's':
	 if (sscanf(size,"%lds",stageCache) != 1) return FALSE;
	 break;
       case 'c':
	 if (sscanf(size,"%ldc",compressCache) != 1) return FALSE;
	 break;
       case 'd':
	 if (sscanf(size,"%ldd,",workingCache) != 1) return FALSE;
	 break;
       case ',':
	 if (sscanf(size,"%ld,",workingCache) != 1) return FALSE;
	 break;
       case NUL:
	 if (sscanf(size,"%ld",workingCache) != 1) return FALSE;
	 return TRUE;
       default:
	 return FALSE;
     }
     /*************************************************************************
     * Increment to the beginning of the next cache size (or to the end of the
     * sizes).
     *************************************************************************/
     if (*p != ',') p++;
     if (*p == ',') p++;
     if (*p == NUL) return TRUE;
  }
}

/******************************************************************************
* ParseRecordRange.
* It can be assumed that there is at least one character in `sizes' or QOP
* would have failed.
******************************************************************************/

Logical  ParseRecordRange (sizes, startingRec, endingRec)
char *sizes;
long *startingRec;
long *endingRec;
{
  char *p, *size, tmp[MAX_CACHESIZES_LEN+1];
  long number;
  int ix;
  /****************************************************************************
  * Initialize starting/ending record numbers.
  ****************************************************************************/
  *startingRec = useDEFAULTrecordNUM;
  *endingRec = useDEFAULTrecordNUM;
  /****************************************************************************
  * Move record numbers to a temporary buffer and remove all spacing.
  ****************************************************************************/
  strcpyX (tmp, sizes, MAX_CACHESIZES_LEN);
  RemoveWhiteSpace (tmp);
  p = tmp;
  /****************************************************************************
  * Check for (and skip) parenthesis on VMS systems.
  ****************************************************************************/
#if defined(vms)
  if (*p == '(') {
    char *lastChar = p + strlen(p) - 1;
    if (*lastChar != ')') return FALSE;
    p++;
    *lastChar = NUL;
  }
#endif
  if (*p == NUL) return FALSE;
  for (ix = 0; ix < 2; ++ix) {
     /*************************************************************************
     * Save the location of the size and increment to the next non-decimal
     * character (ie. past the size).
     *************************************************************************/
     size = p;
     while (Decimal(*p)) p++;
     /*************************************************************************
     * Based on the non-decimal character reached...
     *************************************************************************/
     switch (*p) {
       case ',':
         if (sscanf(size,"%ld,",&number) != 1) return FALSE;
         break;
       case NUL:
         if (sscanf(size,"%ld",&number) != 1) return FALSE;
         break;
       default:
         return FALSE;
     }
     if (number < 1) return FALSE;
     if (ix == 0) 
       *startingRec = number;
     else {
       *endingRec = number;
       if (*endingRec < *startingRec) return FALSE;
     }
     /*************************************************************************
     * Increment to the beginning of the next number (or to the end of the
     * sizes).
     *************************************************************************/
     if (*p == ',') p++;
     if (*p == NUL) return TRUE;
  }
  return TRUE;
}

/******************************************************************************
*  Remove the ".skt" file extension from the given file name if it's there.
*  It ignores the case.
*  Example:
*      mydata.skt => mydata
******************************************************************************/

void RemoveCDFSktExtension (sktName, sktPath)
char *sktName;         /* CDF skeleton table file name. */
char *sktPath;         /* The string holding file name without extension. */
{
    int ptr = -1;

    strcpyX (sktPath, sktName, CDF_PATHNAME_LEN);
    if (EndsWithIgCase (sktPath, ".skt")) {
      ptr = StrLaststrIgCase(sktPath, ".skt");
      if (ptr != -1) ((char *) sktPath)[ptr] = (char) '\0';
    }
    return;
}

/******************************************************************************
* GetNumTokens.
******************************************************************************/

int GetNumTokens (token, string)
int  token;
char *string;
{
  int count = 0;
  char *temp1, *temp2;
  char *ptr1, terminator;
  /****************************************************************************
  * Check that the entered list of tokens is not a NUL string, that there are
  * some possible tokens to match, etc.
  ****************************************************************************/
  if (NULstring(string)) {
    return 0;     /* Obviously, no tokens were found. */
  }
  /****************************************************************************
  * Scan the entered string of tokens searching for matches with the token.
  * First determine the starting character position and the * ending delimiter.
  ****************************************************************************/
  switch (string[0]) {
    case '(':           /* VMS-style. */
      terminator = ')';
      ptr1 = string + 1;
      break;
    case '"':           /* UNIX-style on a Mac (double quotes not stripped). */
      terminator = '"';
      ptr1 = string + 1;
      break;
    default:            /* UNIX-style on a UNIX machine or IBM PC. */
      terminator = NUL;
      ptr1 = string;
      break;
  }
  temp1 = ptr1;
  temp2 = ptr1 + strlen(ptr1) - 1;
  while (temp1 <= temp2) {
    if ((int) *temp1 == token) count++;
    temp1++;
  }
  count++;
  return count;
}

/******************************************************************************
* ParseStringForTokens.
******************************************************************************/

void ParseStringForTokens (token, string, items)
int   token;
char *string;
char *items[];
{
  int  tokenN; size_t tokenLen;
  char *ptr1, *ptr2, terminator;
  /****************************************************************************
  * Check that the entered list of tokens is not a NUL string, that there are
  * some possible tokens to match, etc.
  ****************************************************************************/
  if (NULstring(string)) {
    return;     /* Obviously, no tokens were found. */
  }
  /****************************************************************************
  * Scan the entered string of tokens searching for matches with the token.
  * First determine the starting character position and the * ending delimiter.
  ****************************************************************************/
  switch (string[0]) {
    case '(':           /* VMS-style. */
      terminator = ')';
      ptr1 = string + 1;
      break;
    case '"':           /* UNIX-style on a Mac (double quotes not stripped). */
      terminator = '"';
      ptr1 = string + 1;
      break;
    default:            /* UNIX-style on a UNIX machine or IBM PC. */
      terminator = NUL;
      ptr1 = string;
      break;
  }
  tokenN = 0;
  for (;;) {
     /*************************************************************************
     * Find beginning of the next token in list (skipping past any leading
     * blanks).  If the end of the list is reached instead, then there are
     * no more tokens.
     *************************************************************************/
     while (*ptr1 == ' ') ptr1++;
     if (*ptr1 == terminator) return;
     /*************************************************************************
     * Find the end of the token and copy.  The token is ended by either a
     * blank, comma, or the terminator.
     *************************************************************************/
     ptr2 = ptr1 + 1;
     while ((int) *ptr2 != token && *ptr2 != terminator) ptr2++;
     tokenLen = (size_t) (ptr2 - ptr1);
     strcpyX (items[tokenN], ptr1, tokenLen);
     tokenN++;
     /*************************************************************************
     * Setup to search for next token in list.
     *************************************************************************/
     if ((int) *ptr2 == token)
       /*********************************************************************
       * The last token was ended by a comma.  The next token begins one
       * character beyond the comma.
       *********************************************************************/
       ptr1 = ptr2 + 1;
     else
       /*********************************************************************
       * The terminator must have ended the last token.
       *********************************************************************/
       return;
  }
}

/******************************************************************************
* IsFloatingValidValue - check if data is a floating point data type and in
* the range of the default pad value (padvalue < value < -padvalue).
******************************************************************************/

int IsFloatingValidValue (dataType, value)
long dataType;
void *value;
{
  if (FLOAT4dataType(dataType)) {
    if (*(float *)value > DEFAULT_FLOAT_PADVALUE &&
        *(float *)value < (0.0f - DEFAULT_FLOAT_PADVALUE)) return 1;
  } else if (DOUBLEdataType(dataType)) {
    if (*(double *)value > DEFAULT_DOUBLE_PADVALUE &&
        *(double *)value < (0.0 - DEFAULT_DOUBLE_PADVALUE)) return 1;
  }
  return 0;
}

/******************************************************************************
* IsHexFormat.
******************************************************************************/

Logical IsHexFormat (format)
char *format;
{
  if (format == NULL) return FALSE;
  if (strstr (format, "Z") != NULL || strstr (format, "z") != NULL ||
      strstr (format, "X") != NULL || strstr (format, "x") != NULL)
    return TRUE;
  else
    return FALSE;
}

/******************************************************************************
 * UTF8CharWidth - find the column width of the given character.
 *****************************************************************************/
int UTF8CharWidth (const char *c)
{
  wchar_t *wc;
  size_t iz; int len, ia;
  char *tmp, locale[201];

  ia = 0;
  iz = (size_t) UTF8CharLength (c);
  tmp = (char *) malloc (iz + 1);
  memcpy (tmp, c, iz);
  *(tmp+iz) = '\0';
  wc = (wchar_t *) malloc (sizeof(wchar_t) * (iz + 1));
  strncpy (locale, setlocale(LC_CTYPE, NULL), 200);
#if !defined(WIN32)
  if (CDFstrstrIgCase (locale, "utf8") == NULL &&
      CDFstrstrIgCase (locale, "utf-8") == NULL) {
    setlocale (LC_CTYPE, "en_US.UTF-8");
    ia = 1;
  }
  mbtowc (wc, tmp, iz);
/*
  len = GetWideCharWidth (*wc);
*/
  len = wcwidth (*wc);
  if (len == -1) len = GetUTF8CharWidth (*wc);
#else
  if (CDFstrstrIgCase (locale, "utf8") == NULL &&
      CDFstrstrIgCase (locale, "utf-8") == NULL) {
    setlocale (LC_CTYPE, ".UTF8");
    ia = 1;
  }
  mbtowc (wc, tmp, iz);
  len = GetUTF8CharWidth (*wc);
#endif
  free (wc);
  wc = NULL;
  free (tmp);
  tmp = NULL;
  if (ia == 1) setlocale (LC_CTYPE, locale);
  return len;
}

/******************************************************************************
 * UTF8CharAtWidth - The given character's width in a null-terminating string
 * when display. If returns 0 if the character number (0-based) is not in the
 * string.
 *****************************************************************************/
int UTF8CharAtWidth (const char *string, int charNum)
{
  int i, ix, iy, space, ia; wchar_t *wchars; size_t iz;
  char locale[201];

  ia = 0;
  ix = UTF8StrLength (string, (int) strlen(string));
  if (charNum >= ix) return 0;
  wchars = (wchar_t *) malloc (sizeof(wchar_t) * (ix + 1));
  strncpy (locale, setlocale(LC_CTYPE, NULL), 200);
#if !defined(WIN32)
  if (CDFstrstrIgCase (locale, "utf8") == NULL &&
      CDFstrstrIgCase (locale, "utf-8") == NULL) {
    setlocale (LC_CTYPE, "en_US.UTF-8");
    ia = 1;
  }
  iy = (int) mbstowcs (wchars, string, ix);
#else
  if (CDFstrstrIgCase (locale, "utf8") == NULL &&
      CDFstrstrIgCase (locale, "utf-8") == NULL) {
    setlocale (LC_CTYPE, ".UTF8");
    ia = 1;
  }
  mbstowcs_s (&iz, wchars, (size_t) ix+1, string, (size_t) ix);
#endif
  space = UTF8WCharWidth (wchars[charNum]);
  free (wchars);
  wchars = NULL;
  if (ia == 1) setlocale (LC_CTYPE, locale);
  return space;
}

/******************************************************************************
 * UTF8CharAtPosition - find the column position of the given character
 * number from beginning of the null-terminating string when it displays.
 *****************************************************************************/
int UTF8CharAtPosition (const char *string, int charNum)
{
  int ix, iy, i, j, pos, ia, len; wchar_t *wchars; size_t iz;
  char locale[201];

  ia = 0;
  strncpy (locale, setlocale(LC_CTYPE, NULL), 200);
  iy = (int) strlen (string);
  ix = UTF8StrLength (string, iy);
  wchars = (wchar_t *) malloc (sizeof(wchar_t) * (ix + 1));
#if !defined(WIN32)
  if (CDFstrstrIgCase (locale, "utf8") == NULL &&
      CDFstrstrIgCase (locale, "utf-8") == NULL) {
    setlocale (LC_CTYPE, "en_US.UTF-8");
    ia = 1;
  }
  iy = mbstowcs (wchars, string, ix+1);
#else
  if (CDFstrstrIgCase (locale, "utf8") == NULL &&
      CDFstrstrIgCase (locale, "utf-8") == NULL) {
    setlocale (LC_CTYPE, ".UTF8");
    ia = 1;
  }
  mbstowcs_s (&iz, wchars, (size_t) ix+1, string, (size_t) ix);
#endif
  pos = 0;
  if (charNum < ix) ix = charNum;
  for (i = 0; i < ix; ++i) {
#if !defined(WIN32)
/*
    pos += GetWideCharWidth (wchars[i]);
*/
    len = wcwidth (wchars[i]);
    if (len == -1) len = GetUTF8CharWidth (wchars[i]);
    pos += len;
#else
    pos += GetUTF8CharWidth (wchars[i]);
#endif
  }
  free (wchars);
  wchars = NULL;
  if (ia == 1) setlocale (LC_CTYPE, locale);
  return pos;
}

/******************************************************************************
 * UTF8FirstCharsWidth - Find the total number of columns (width) from the
 * first characters in a null-terminating string.
 *****************************************************************************/
int UTF8FirstCharsWidth (const char *string, int numChars)
{
  int i, ix, iy, width, ia; wchar_t *wchars; size_t iz;
  char locale[201];

  ia = 0;
  strncpy (locale, setlocale(LC_CTYPE, NULL), 200);
  ix = UTF8StrLength (string, (int) strlen(string));
  wchars = (wchar_t *) malloc (sizeof(wchar_t) * (ix + 1));
#if !defined(WIN32)
  if (CDFstrstrIgCase (locale, "utf8") == NULL &&
      CDFstrstrIgCase (locale, "utf-8") == NULL) {
    setlocale (LC_CTYPE, "en_US.UTF-8");
    ia = 1;
  }
  iy = mbstowcs (wchars, string, ix+1);
#else
  if (CDFstrstrIgCase (locale, "utf8") == NULL &&
      CDFstrstrIgCase (locale, "utf-8") == NULL) {
    setlocale (LC_CTYPE, ".UTF8");
    ia = 1;
  }
  mbstowcs_s (&iz, wchars, (size_t) ix+1, string, (size_t) ix);
#endif
  width = 0;
  if (numChars > ix) numChars = ix;
  width = UTF8WCharsWidth (wchars, numChars);
  free (wchars);
  wchars = NULL;
  if (ia == 1) setlocale (LC_CTYPE, locale);
  return width;
}

/******************************************************************************
 * UTF8LastCharWithinWidth - Find the last character number in the 
 * null-terminating string that has the string width within the given column
 * when display.
 *****************************************************************************/
int UTF8LastCharWithinWidth (const char *string, int width)
{
  int i, ix, iy, w2, ia; wchar_t *wchars; size_t iz;
  char locale[201];

  ia = 0;
  strncpy (locale, setlocale(LC_CTYPE, NULL), 200);
  ix = UTF8StrLength (string, (int) strlen(string));
  wchars = (wchar_t *) malloc (sizeof(wchar_t) * (ix + 1));
#if !defined(WIN32)
  if (CDFstrstrIgCase (locale, "utf8") == NULL &&
      CDFstrstrIgCase (locale, "utf-8") == NULL) {
    setlocale (LC_CTYPE, "en_US.UTF-8");
    ia = 1;
  }
  iy = mbstowcs (wchars, string, ix+1);
#else
  if (CDFstrstrIgCase (locale, "utf8") == NULL &&
      CDFstrstrIgCase (locale, "utf-8") == NULL) {
    setlocale (LC_CTYPE, ".UTF8");
    ia = 1;
  }
  mbstowcs_s (&iz, wchars, (size_t) ix+1, string, (size_t) ix);
#endif
  w2 = 0;
  for (i = 0; i < ix; ++i) {
    w2 += UTF8WCharWidth (wchars[i]);
    if (w2 > width) break; 
  }
  free (wchars);
  wchars = NULL;
  if (ia == 1) setlocale (LC_CTYPE, locale);
  return (i-1);
}

/******************************************************************************
 * UTF8TotalWidth - Counts the total width a UTF-8 null-terminating string
 * occpies when display.
 *****************************************************************************/
int UTF8TotalWidth (const char *string)
{
  int i, ix, iy, width, ia; wchar_t *wchars; size_t iz;
  char locale[201];

  ia = 0;
  strncpy (locale, setlocale(LC_CTYPE, NULL), 200);
  ix = UTF8StrLength (string, (int) strlen(string));
  wchars = (wchar_t *) malloc (sizeof(wchar_t) * (ix + 1));
#if !defined(WIN32)
  if (CDFstrstrIgCase (locale, "utf8") == NULL &&
      CDFstrstrIgCase (locale, "utf-8") == NULL) {
    setlocale (LC_CTYPE, "en_US.UTF-8");
    ia = 1;
  }
  iy = mbstowcs (wchars, string, ix+1);
#else
  if (CDFstrstrIgCase (locale, "utf8") == NULL &&
      CDFstrstrIgCase (locale, "utf-8") == NULL) {
    setlocale (LC_CTYPE, ".UTF8");
    ia = 1;
  }
  mbstowcs_s (&iz, wchars, (size_t) ix+1, string, (size_t) ix);
#endif
  width = 0;
  for (i = 0; i < ix; ++i) {
    width += UTF8WCharWidth (wchars[i]);
  }
  if (ia == 1) setlocale (LC_CTYPE, locale);
  return width;
}

/******************************************************************************
 * UTF8WCharWidth - find the column width of the given wide character.
 *****************************************************************************/
int UTF8WCharWidth (wchar_t wc)
{
#if !defined(WIN32)
/*  return GetWideCharWidth (wc); */
  int len;
  len = wcwidth (wc);
  if (len != -1) return len;
  else return GetUTF8CharWidth (wc);
#else
  return GetUTF8CharWidth (wc);
#endif
}

/******************************************************************************
 * UTF8WCharsWidth - find the total column width of the given number of wide
 * characters in a wide character string.
 *****************************************************************************/
int UTF8WCharsWidth (const wchar_t *wcs, int numChars)
{
  int i, width;
  width = 0;
  for (i = 0; i < numChars; ++i) {
    width += UTF8WCharWidth (wcs[i]);
  }
  return width;
}

/******************************************************************************
 * UTF8HasWiderChar - Checks to see if a UTF8 null-terminating string contains
 * characters taking more than one column when display. It returns the number
 * of extra columns than the normal columns that have a width of one for each
 * character. It returns 0 if there are no wider characters.
 *****************************************************************************/
int UTF8HasWiderChar (const char *string)
{
  int i, ix, iy, iz, ia; wchar_t *wchars; size_t ik;
  char locale[201];

  ia = 0;
  strncpy (locale, setlocale(LC_CTYPE, NULL), 200);
  iy = (int) strlen(string);
  ix = UTF8StrLength (string, iy);
  wchars = (wchar_t *) malloc (sizeof(wchar_t) * (ix + 1));
#if !defined(WIN32)
  if (CDFstrstrIgCase (locale, "utf8") == NULL &&
      CDFstrstrIgCase (locale, "utf-8") == NULL) {
    setlocale (LC_CTYPE, "en_US.UTF-8");
    ia = 1;
  }
  iy = mbstowcs (wchars, string, ix);
#else
  if (CDFstrstrIgCase (locale, "utf8") == NULL &&
      CDFstrstrIgCase (locale, "utf-8") == NULL) {
    setlocale (LC_CTYPE, ".UTF8");
    ia = 1;
  }
  mbstowcs_s (&ik, wchars, (size_t) ix+1, string, (size_t) ix);
#endif
  iz = 0;
  for (i = 0; i < ix; ++i) {
#if !defined(WIN32)
/*
     iy = GetWideCharWidth(wchars[i]);
*/
     iy = wcwidth (wchars[i]);
     if (iy == -1) iy = GetUTF8CharWidth(wchars[i]);
#else
     iy = GetUTF8CharWidth(wchars[i]);
#endif
     if (iy > 1) iz = iz + iy - 1;
  }
  free (wchars);
  wchars = NULL;
  if (ia == 1) setlocale(LC_CTYPE, locale);
  return iz;
}

/* auxiliary function for binary search in interval table */
static int bisearch(wchar_t ucs, const struct interval *table, int max) {
  int min = 0;
  int mid;

  if (ucs < table[0].first || ucs > table[max].last)
    return 0;
  while (max >= min) {
    mid = (min + max) / 2;
    if (ucs > table[mid].last)
      min = mid + 1;
    else if (ucs < table[mid].first)
      max = mid - 1;
    else
      return 1;
  }
  return 0;
}

/******************************************************************************
 * Get the width from a UTF-8 character in wide charatcer type.
 * From http://cl.cam.ac.uk/!mgk25/ucs/wcwidth.c by Markus Kuhn
 * Note: On Windows and 32-bit AIX platforms, wchar_t is a 16-bit type and 
 *       therefore cannot accommodate all Unicode characters. 
 *****************************************************************************/

int GetUTF8CharWidth (wchar_t ucs) {

  /* sorted list of non-overlapping intervals of non-spacing characters */
  /* generated by "uniset +cat=Me +cat=Mn +cat=Cf -00AD +1160-11FF +200B c" */
  static const struct interval combining[] = {
    { 0x0300, 0x036F }, { 0x0483, 0x0486 }, { 0x0488, 0x0489 },
    { 0x0591, 0x05BD }, { 0x05BF, 0x05BF }, { 0x05C1, 0x05C2 },
    { 0x05C4, 0x05C5 }, { 0x05C7, 0x05C7 }, { 0x0600, 0x0603 },
    { 0x0610, 0x0615 }, { 0x064B, 0x065E }, { 0x0670, 0x0670 },
    { 0x06D6, 0x06E4 }, { 0x06E7, 0x06E8 }, { 0x06EA, 0x06ED },
    { 0x070F, 0x070F }, { 0x0711, 0x0711 }, { 0x0730, 0x074A },
    { 0x07A6, 0x07B0 }, { 0x07EB, 0x07F3 }, { 0x0901, 0x0902 },
    { 0x093C, 0x093C }, { 0x0941, 0x0948 }, { 0x094D, 0x094D },
    { 0x0951, 0x0954 }, { 0x0962, 0x0963 }, { 0x0981, 0x0981 },
    { 0x09BC, 0x09BC }, { 0x09C1, 0x09C4 }, { 0x09CD, 0x09CD },
    { 0x09E2, 0x09E3 }, { 0x0A01, 0x0A02 }, { 0x0A3C, 0x0A3C },
    { 0x0A41, 0x0A42 }, { 0x0A47, 0x0A48 }, { 0x0A4B, 0x0A4D },
    { 0x0A70, 0x0A71 }, { 0x0A81, 0x0A82 }, { 0x0ABC, 0x0ABC },
    { 0x0AC1, 0x0AC5 }, { 0x0AC7, 0x0AC8 }, { 0x0ACD, 0x0ACD },
    { 0x0AE2, 0x0AE3 }, { 0x0B01, 0x0B01 }, { 0x0B3C, 0x0B3C },
    { 0x0B3F, 0x0B3F }, { 0x0B41, 0x0B43 }, { 0x0B4D, 0x0B4D },
    { 0x0B56, 0x0B56 }, { 0x0B82, 0x0B82 }, { 0x0BC0, 0x0BC0 },
    { 0x0BCD, 0x0BCD }, { 0x0C3E, 0x0C40 }, { 0x0C46, 0x0C48 },
    { 0x0C4A, 0x0C4D }, { 0x0C55, 0x0C56 }, { 0x0CBC, 0x0CBC },
    { 0x0CBF, 0x0CBF }, { 0x0CC6, 0x0CC6 }, { 0x0CCC, 0x0CCD },
    { 0x0CE2, 0x0CE3 }, { 0x0D41, 0x0D43 }, { 0x0D4D, 0x0D4D },
    { 0x0DCA, 0x0DCA }, { 0x0DD2, 0x0DD4 }, { 0x0DD6, 0x0DD6 },
    { 0x0E31, 0x0E31 }, { 0x0E34, 0x0E3A }, { 0x0E47, 0x0E4E },
    { 0x0EB1, 0x0EB1 }, { 0x0EB4, 0x0EB9 }, { 0x0EBB, 0x0EBC },
    { 0x0EC8, 0x0ECD }, { 0x0F18, 0x0F19 }, { 0x0F35, 0x0F35 },
    { 0x0F37, 0x0F37 }, { 0x0F39, 0x0F39 }, { 0x0F71, 0x0F7E },
    { 0x0F80, 0x0F84 }, { 0x0F86, 0x0F87 }, { 0x0F90, 0x0F97 },
    { 0x0F99, 0x0FBC }, { 0x0FC6, 0x0FC6 }, { 0x102D, 0x1030 },
    { 0x1032, 0x1032 }, { 0x1036, 0x1037 }, { 0x1039, 0x1039 },
    { 0x1058, 0x1059 }, { 0x1160, 0x11FF }, { 0x135F, 0x135F },
    { 0x1712, 0x1714 }, { 0x1732, 0x1734 }, { 0x1752, 0x1753 },
    { 0x1772, 0x1773 }, { 0x17B4, 0x17B5 }, { 0x17B7, 0x17BD },
    { 0x17C6, 0x17C6 }, { 0x17C9, 0x17D3 }, { 0x17DD, 0x17DD },
    { 0x180B, 0x180D }, { 0x18A9, 0x18A9 }, { 0x1920, 0x1922 },
    { 0x1927, 0x1928 }, { 0x1932, 0x1932 }, { 0x1939, 0x193B },
    { 0x1A17, 0x1A18 }, { 0x1B00, 0x1B03 }, { 0x1B34, 0x1B34 },
    { 0x1B36, 0x1B3A }, { 0x1B3C, 0x1B3C }, { 0x1B42, 0x1B42 },
    { 0x1B6B, 0x1B73 }, { 0x1DC0, 0x1DCA }, { 0x1DFE, 0x1DFF },
    { 0x200B, 0x200F }, { 0x202A, 0x202E }, { 0x2060, 0x2063 },
    { 0x206A, 0x206F }, { 0x20D0, 0x20EF }, { 0x302A, 0x302F },
    { 0x3099, 0x309A }, { 0xA806, 0xA806 }, { 0xA80B, 0xA80B },
    { 0xA825, 0xA826 }, { 0xFB1E, 0xFB1E }, { 0xFE00, 0xFE0F },
    { 0xFE20, 0xFE23 }, { 0xFEFF, 0xFEFF }, { 0xFFF9, 0xFFFB },
    { 0x10A01, 0x10A03 }, { 0x10A05, 0x10A06 }, { 0x10A0C, 0x10A0F },
    { 0x10A38, 0x10A3A }, { 0x10A3F, 0x10A3F }, { 0x1D167, 0x1D169 },
    { 0x1D173, 0x1D182 }, { 0x1D185, 0x1D18B }, { 0x1D1AA, 0x1D1AD },
    { 0x1D242, 0x1D244 }, { 0xE0001, 0xE0001 }, { 0xE0020, 0xE007F },
    { 0xE0100, 0xE01EF }
  };

  /* test for 8-bit control characters */
  if (ucs == 0)
    return 0;
  if (ucs < 32 || (ucs >= 0x7f && ucs < 0xa0))
    return -1;

  /* binary search in table of non-spacing characters */
  if (bisearch(ucs, combining,
               sizeof(combining) / sizeof(struct interval) - 1))
    return 0;

  /* if we arrive here, ucs is not a combining or C0/C1 control character */

  return 1 +
    (ucs >= 0x1100 &&
     (ucs <= 0x115f ||                    /* Hangul Jamo init. consonants */
      ucs == 0x2329 || ucs == 0x232a ||
      (ucs >= 0x2e80 && ucs <= 0xa4cf &&
       ucs != 0x303f) ||                  /* CJK ... Yi */
      (ucs >= 0xac00 && ucs <= 0xd7a3) || /* Hangul Syllables */
      (ucs >= 0xf900 && ucs <= 0xfaff) || /* CJK Compatibility Ideographs */
      (ucs >= 0xfe10 && ucs <= 0xfe19) || /* Vertical forms */
      (ucs >= 0xfe30 && ucs <= 0xfe6f) || /* CJK Compatibility Forms */
      (ucs >= 0xff00 && ucs <= 0xff60) || /* Fullwidth Forms */
      (ucs >= 0xffe0 && ucs <= 0xffe6) ||
      (ucs >= 0x20000 && ucs <= 0x2fffd) ||
      (ucs >= 0x30000 && ucs <= 0x3fffd)));
}

