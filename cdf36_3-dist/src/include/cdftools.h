/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF					      Header for CDF toolkit.
*
*  Version 1.3c, 14-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0   9-Dec-94, J Love	Original version.
*   V1.0a 20-Jan-95, J Love	IRIX 6.x (64-bit).
*   V1.0b  3-Mar-95, J Love	Pass `char' as `int'.  `EncodeKeyDefinitions'
*				prototype moved to `windoz.h'.
*   V1.1   6-Apr-95, J Love	POSIX.
*   V1.1a 18-Apr-95, J Love	More POSIX.
*   V1.1b 22-May-95, J Love	EPOCH styles.  Added `MakeNUL'.  Prototypes
*				for `ASSIGNx', `EQx', etc. moved here.
*   V1.1c 13-Jun-95, J Love	EPOCH custom format.
*   V1.2   1-Sep-95, J Love	Added prototypes for routines used by
*				CDFexport.  Hyper groups.
*   V1.2a 19-Sep-95, J Love	Macintosh event handling.
*   V1.2b 29-Sep-95, J Love	Macintosh dialog filtering.  Executing clock
*				for Macintosh.  Macintosh cursors.
*   V1.3  24-Sep-96, J Love	CDF V2.6.
*   V1.3a 21-Feb-97, J Love	Removed RICE.
*   V1.3b 28-Feb-97, J Love	Windows NT for MS Visual C/C++ on an IBM PC.
*   V1.3c 14-Nov-97, J Love	More Windows NT.
*   V3.3  10-Jan-09, M Liu      Added Mac or Linux on PPC running 64-bit mode.
*   V3.3a 10-Dec-10, M Liu      Added a new epoch style for ISO 8601.
*
*****************************************************************************/

#if !defined(CDFTOOLSh_INCLUDEd__)
#define CDFTOOLSh_INCLUDEd__

/*****************************************************************************
* Include files.
*****************************************************************************/

#include "cdfdist.h"

/*****************************************************************************
* Miscellaneous.
*****************************************************************************/

#define INPUT_MAJOR             0
#define useDEFAULTcacheSIZE	0
#define useDEFAULTrecordNUM     0

/*****************************************************************************
* Macintosh-specific...
*****************************************************************************/

#if defined(mac)
#  define RETURN_KEYCODE	36
#  define ENTER_KEYCODE		76
#  define PAGEup_KEYCODE	116
#  define PAGEdown_KEYCODE	121
#  define UParrow_KEYCODE	126
#  define DOWNarrow_KEYCODE	125
#  define PERIOD_KEYCODE	47
#  define E_KEYCODE		14
#  define R_KEYCODE		15
#  define S_KEYCODE		1
#  define Q_KEYCODE		12
#  define O_KEYCODE		31
#  define N_KEYCODE		45
#  define SO_STATUS_LINE_LEN	83
#  define ARROW_CURSOR		(&(qd.arrow))
#  define CDF_CURSOR		(*(GetCursor((short)(CDF_CURSORri))))
#endif

/*****************************************************************************
* Abort keys for CDF Toolkit programs.  On the Macintosh the Command-Period
* causes an abort.
*****************************************************************************/

#if defined(vms)
#  define ABORTkey(key) (key == KB_CTRL_Y || key == KB_CTRL_C)
#endif

#if defined(unix) || defined(posixSHELL) || defined(dos) || defined(win32)
#  define ABORTkey(key) (key == KB_CTRL_C)
#endif

#if defined(mac)
#  define ABORTkeyMAC(event) \
((((event.message & keyCodeMask) >> 8) == PERIOD_KEYCODE) && \
 ((event.modifiers & cmdKey) != 0))
#endif

/*****************************************************************************
* CHECKforABORTso.
*****************************************************************************/

#if defined(mac)
#  define CHECKforABORTso		CheckForAbortSOmac();
#else
#  if (defined(win32) && !defined(__MINGW32__)) && !defined(ALONE)
#    define CHECKforABORTso		CheckForAbortSOwin32();
#  else
#    define CHECKforABORTso \

#  endif
#endif

/*****************************************************************************
* Matching constants.
*****************************************************************************/

#define NOMATCH			(-1)
#define MATCHES			(-2)

/*****************************************************************************
* Justification constants (used by `CatToString').
*****************************************************************************/

#define LEFT_JUSTIFY		1
#define CENTER_JUSTIFY		2
#define RIGHT_JUSTIFY		3

/*****************************************************************************
* Directory constants.
*****************************************************************************/

#ifndef DU_MAX_DIR_LEN 
#define DU_MAX_DIR_LEN		256
#endif
#ifndef DU_MAX_NAME_LEN 
#define DU_MAX_NAME_LEN		256
#endif
#define DU_MAX_PATTERNS		16

/*****************************************************************************
* EPOCH/time constants.
*****************************************************************************/

#define TIME_STAMP_LEN		31

#define EPOCH0_STYLE		0		/* Standard */
#define EPOCH1_STYLE		1
#define EPOCH2_STYLE		2
#define EPOCH3_STYLE		3
#define EPOCH4_STYLE		6
#define EPOCHf_STYLE		4		/* Use C/Fortran format. */
#define EPOCHx_STYLE		5		/* Use custom format. */

/*****************************************************************************
* TT2000/time constants.
*****************************************************************************/

#define TT2000_0_STYLE		0               /* Standard */
#define TT2000_1_STYLE		1
#define TT2000_2_STYLE		2
#define TT2000_3_STYLE		3

/*****************************************************************************
* QOP constants.
*****************************************************************************/

#define QOP_MAX_ARGVs	40
#define QOP_MAX_PARMs	40
#define QOP_MAX_QUALs	40

/*****************************************************************************
* QOP structure.
*****************************************************************************/

typedef struct QOPstruct {
int Nparms;
char *parms[QOP_MAX_PARMs];
int qualEntered[QOP_MAX_QUALs];
char *qualOpt[QOP_MAX_QUALs];
} QOP;

/******************************************************************************
* Reporting constants.
******************************************************************************/

#define ERRORs		0
#define WARNs		1
#define INFOs		2

/******************************************************************************
* Format specifiers.
******************************************************************************/

#if defined(alphaosf) || defined(IRIX64bit) || defined(_LP64) || \
    defined(__amd64) || defined(__x86_64__) || defined(__ia64__) || \
    defined(__PPC64__) || defined(__ppc64__)
#  define Int32FORMAT	"%d"
#  define Int64FORMAT   "%lld"
#  define Int32uFORMAT	"%u"
#  define Int32FORMATmod	""
#  define Int64FORMATmod        "ll"
#  define Int32hexFORMAT	"0x%08X"
#  if defined(alphaosf)
#    define Int64hexFORMAT      "0x%016X"
#  else
#    define Int64hexFORMAT      "0x%016llX"
#  endif
#else
#  define Int32FORMAT	"%ld"
#if defined(win32) || defined(__MINGW32__)
#  define Int64FORMAT   "%I64d"
#else
#  define Int64FORMAT   "%lld"
#endif
#  define Int32uFORMAT	"%lu"
#  define Int32FORMATmod	"l"
#  define Int64FORMATmod	"ll"
#  define Int32hexFORMAT	"0x%08lX"
#if defined(win32) || defined(__MINGW32__)
#  define Int64hexFORMAT        "0x%016I64X"
#else
#  define Int64hexFORMAT        "0x%016llX"
#endif
#endif

/*****************************************************************************
* Possible variable value selections (for SkeletonTable).
*****************************************************************************/

#define NOvalues	0
#define NRVvalues	1
#define RVvalues	2
#define ALLvalues	3
#define NAMEDvalues	4

/******************************************************************************
* Current directory macro.
******************************************************************************/

#if defined(vms)
#  define CURRENTDIRECTORY "[]"
#endif
#if defined(unix) || defined(dos) || defined(posixSHELL) || defined(win32)
#  define CURRENTDIRECTORY "."
#endif
#if defined(mac)
#  define CURRENTDIRECTORY ":"
#endif

/******************************************************************************
* NoMoreAccess.
******************************************************************************/

#define NoMoreAccess(id) \
(StatusBAD(BOO(id == NULL, \
	   CDFlib(CONFIRM_,CDF_ACCESS_,NULL_), \
	   CDFlib(SELECT_,CDF_,id,CONFIRM_,CDF_ACCESS_,NULL_))))

/******************************************************************************
* Hyper groups.
******************************************************************************/

#define HYPER_READ(id,zVar,hyperX,buffer) \
CDFlib (SELECT_, CDF_, id, \
		 BOO(zVar,zVAR_RECNUMBER_, \
			  rVARs_RECNUMBER_), hyperX.recNumber, \
		 BOO(zVar,zVAR_RECCOUNT_, \
			  rVARs_RECCOUNT_), hyperX.recCount, \
		 BOO(zVar,zVAR_RECINTERVAL_, \
			  rVARs_RECINTERVAL_), hyperX.recInterval, \
		 BOO(zVar,zVAR_DIMINDICES_, \
			  rVARs_DIMINDICES_), hyperX.dimIndices, \
		 BOO(zVar,zVAR_DIMCOUNTS_, \
			  rVARs_DIMCOUNTS_), hyperX.dimCounts, \
		 BOO(zVar,zVAR_DIMINTERVALS_, \
			  rVARs_DIMINTERVALS_), hyperX.dimIntervals, \
	GET_, BOO(zVar,zVAR_HYPERDATA_,rVAR_HYPERDATA_), buffer, \
	NULL_)

#define HYPER_WRITE(id,zVar,hyperX,buffer) \
CDFlib (SELECT_, CDF_, id, \
		 BOO(zVar,zVAR_RECNUMBER_, \
			  rVARs_RECNUMBER_), hyperX.recNumber, \
		 BOO(zVar,zVAR_RECCOUNT_, \
			  rVARs_RECCOUNT_), hyperX.recCount, \
		 BOO(zVar,zVAR_RECINTERVAL_, \
			  rVARs_RECINTERVAL_), hyperX.recInterval, \
		 BOO(zVar,zVAR_DIMINDICES_, \
			  rVARs_DIMINDICES_), hyperX.dimIndices, \
		 BOO(zVar,zVAR_DIMCOUNTS_, \
			  rVARs_DIMCOUNTS_), hyperX.dimCounts, \
		 BOO(zVar,zVAR_DIMINTERVALS_, \
			  rVARs_DIMINTERVALS_), hyperX.dimIntervals, \
	PUT_, BOO(zVar,zVAR_HYPERDATA_,rVAR_HYPERDATA_), buffer, \
	NULL_)

struct GroupStruct {
  long nRecGroups;
  long firstRecCount;
  long lastRecCount;
  long nDimGroups[CDF_MAX_DIMS];
  long firstDimCount[CDF_MAX_DIMS];
  long lastDimCount[CDF_MAX_DIMS];
  long recGroupN;			/* Used for incrementing. */
  long dimGroupN[CDF_MAX_DIMS];		/* Used for incrementing. */
};

struct HyperStruct {
  long recNumber;
  long recCount;
  long recInterval;
  long dimIndices[CDF_MAX_DIMS];
  long dimCounts[CDF_MAX_DIMS];
  long dimIntervals[CDF_MAX_DIMS];
};

/******************************************************************************
* PCT.
******************************************************************************/

#define NO_PCT		(-1)
#define PCT(n,N,x,X)	((int)((n*100*X + 100*x)/(N*X)))

/*****************************************************************************
* Sizes/limits.
*****************************************************************************/

#define SCREEN_WIDTH			80
#define SCREEN_HEIGHT               	24
#define MAX_SCREENLINE_LEN          	79
#define MAX_QUALIFIER_NAME_LEN		40
#define MAX_nonSTRING_VALUE_LEN     	320
#define MAX_KEY_TOKEN_LEN           	20
#define MAX_GZIP_TOKEN_LEN           	6
#define MAX_TOKEN_LEN			30
#define MAX_SEVERITY_TEXT_LEN		20
#define MAX_MESSAGE_TEXT_LEN		80
#define MAX_PROGRAM_NAME_LEN		25
#define MAX_ENCODED_INT4_LEN		11
#define MAX_ENCODED_INT8_LEN		20
#define MAX_REPORTMASK_LEN		40
#define MAX_oTEXT_LEN			150
#define MAX_CACHESIZES_LEN		30
#define MAX_TOLERANCES_LEN              30

#if defined(BORLANDC)
#  define BORLANDC_STACK_SIZE		12000u
#  define BORLANDC_OVERLAY_SIZE		0x4000	/* 16-byte paragraphs. */
#endif

#if defined(mac)
#  define MAX_LINES_WHEN_PAGING		24
#else
#  define MAX_LINES_WHEN_PAGING		22
#endif

/******************************************************************************
* BLANKs78 (and BLANKs40).
*
*    This macro is used to initialize a static character string to a size
* large enough to be used for a line in a window.  Normally that would be
* 78 characters because the left-most and right-most columns of a window are
* part of the border.  We'll make it a few more just in case a header line
* runs a little long (because of a value larger than expected).
*
******************************************************************************/

#define BLANKs78 "                                                                                                 "
#define BLANKs40 "                                             "

/*****************************************************************************
* Symbol(s) for displaying illegal/special values.
*****************************************************************************/

#if defined(FP1cpu) || defined(FP2cpu)
#  define NEGATIVEzeroSYMBOL	"-0.0"
#endif

#if defined(FP3cpu) || defined(FP4cpu)
#  define NEGATIVEzeroSYMBOL	"<Illegal,-0.0>"
#endif

/*****************************************************************************
* Exit macros.
*****************************************************************************/

#if defined(vms)
#  define EXIT_SUCCESS_	SS$_NORMAL
#  define EXIT_FAILURE_	SS$_ABORT
#else
#  define EXIT_SUCCESS_	0
#  define EXIT_FAILURE_	1
#endif

#define Exit exit(EXIT_SUCCESS_)
#define ExitBAD exit(EXIT_FAILURE_)

/*****************************************************************************
* Byte/short extraction macros.
*****************************************************************************/

#define HighSHORTinLONG(l) ((short) (l >> 16))
#define LowSHORTinLONG(l) ((short) (l & 0xFFFF))

/******************************************************************************
* `main' macros.
******************************************************************************/

#if defined(mac)
#  define MAIN int main ()
#else
#  define MAIN int main (argc, argv) int argc; char *argv[];
#endif

/******************************************************************************
* NULpString.
*    Macro to check if a Pascal null-string (length of zero).
******************************************************************************/

#define NULpString(string) (string[0] == 0)

/******************************************************************************
* AOSs1/AOSs2/.../AOSsN.
*   These macros are necessary because some compilers (eg. GNU C, the Digital
* C compiler on a DEC Alpha running OpenVMS, etc.) seem to have a problem with
* initialized arrays of arrays of characters (arrays of strings).  If an
* attempt is made to modify one of the strings an access violation will occur.
* In those cases where there are two or more arrays of strings with the same
* number of strings, two separate macros will be needed to keep duplicate
* variable names from occurring (eg. AOSs1 & AOSs1A).
******************************************************************************/

#define AOSs1(arrayofstrings,string0) \
static char _1line0_[] = string0; \
static char *arrayofstrings[] = { _1line0_ };

#define AOSs1A(arrayofstrings,string0) \
static char _1Aline0_[] = string0; \
static char *arrayofstrings[] = { _1Aline0_ };

#define AOSs1B(arrayofstrings,string0) \
static char _1Bline0_[] = string0; \
static char *arrayofstrings[] = { _1Bline0_ };

#define AOSs1C(arrayofstrings,string0) \
static char _1Cline0_[] = string0; \
static char *arrayofstrings[] = { _1Cline0_ };

#define AOSs1D(arrayofstrings,string0) \
static char _1Dline0_[] = string0; \
static char *arrayofstrings[] = { _1Dline0_ };

#define AOSs1E(arrayofstrings,string0) \
static char _1Eline0_[] = string0; \
static char *arrayofstrings[] = { _1Eline0_ };

#define AOSs2(arrayofstrings,string0,string1) \
static char _2line0_[] = string0; \
static char _2line1_[] = string1; \
static char *arrayofstrings[] = { _2line0_, _2line1_ };

#define AOSs2A(arrayofstrings,string0,string1) \
static char _2Aline0_[] = string0; \
static char _2Aline1_[] = string1; \
static char *arrayofstrings[] = { _2Aline0_, _2Aline1_ };

#define AOSs2B(arrayofstrings,string0,string1) \
static char _2Bline0_[] = string0; \
static char _2Bline1_[] = string1; \
static char *arrayofstrings[] = { _2Bline0_, _2Bline1_ };

#define AOSs2C(arrayofstrings,string0,string1) \
static char _2Cline0_[] = string0; \
static char _2Cline1_[] = string1; \
static char *arrayofstrings[] = { _2Cline0_, _2Cline1_ };

#define AOSs3(arrayofstrings,string0,string1,string2) \
static char _3line0_[] = string0; \
static char _3line1_[] = string1; \
static char _3line2_[] = string2; \
static char *arrayofstrings[] = { _3line0_, _3line1_, _3line2_ };

#define AOSs4(arrayofstrings,string0,string1,string2,string3) \
static char _4line0_[] = string0; \
static char _4line1_[] = string1; \
static char _4line2_[] = string2; \
static char _4line3_[] = string3; \
static char *arrayofstrings[] = { _4line0_, _4line1_, _4line2_, _4line3_ };

#define AOSs5(arrayofstrings,string0,string1,string2,string3,string4) \
static char _5line0_[] = string0; \
static char _5line1_[] = string1; \
static char _5line2_[] = string2; \
static char _5line3_[] = string3; \
static char _5line4_[] = string4; \
static char *arrayofstrings[] = { \
  _5line0_, _5line1_, _5line2_, _5line3_, _5line4_ \
};

#define AOSs6(arrayofstrings,string0,string1,string2,string3,string4,string5) \
static char _6line0_[] = string0; \
static char _6line1_[] = string1; \
static char _6line2_[] = string2; \
static char _6line3_[] = string3; \
static char _6line4_[] = string4; \
static char _6line5_[] = string5; \
static char *arrayofstrings[] = { \
  _6line0_, _6line1_, _6line2_, _6line3_, _6line4_, _6line5_ \
};

#define AOSs11(arrayofstrings,string0,string1,string2,string3,string4,string5,string6,string7,string8,string9,string10) \
static char _11line0_[] = string0; \
static char _11line1_[] = string1; \
static char _11line2_[] = string2; \
static char _11line3_[] = string3; \
static char _11line4_[] = string4; \
static char _11line5_[] = string5; \
static char _11line6_[] = string6; \
static char _11line7_[] = string7; \
static char _11line8_[] = string8; \
static char _11line9_[] = string9; \
static char _11line10_[] = string10; \
static char *arrayofstrings[] = { \
  _11line0_, _11line1_, _11line2_, _11line3_, _11line4_, _11line5_, \
  _11line6_, _11line7_, _11line8_, _11line9_, _11line10_ \
};

#define AOSs15(arrayofstrings,string0,string1,string2,string3,string4,string5,string6,string7,string8,string9,string10,string11,string12,string13,string14) \
static char _15line0_[] = string0; \
static char _15line1_[] = string1; \
static char _15line2_[] = string2; \
static char _15line3_[] = string3; \
static char _15line4_[] = string4; \
static char _15line5_[] = string5; \
static char _15line6_[] = string6; \
static char _15line7_[] = string7; \
static char _15line8_[] = string8; \
static char _15line9_[] = string9; \
static char _15line10_[] = string10; \
static char _15line11_[] = string11; \
static char _15line12_[] = string12; \
static char _15line13_[] = string13; \
static char _15line14_[] = string14; \
static char *arrayofstrings[] = { \
  _15line0_, _15line1_, _15line2_, _15line3_, _15line4_, _15line5_, \
  _15line6_, _15line7_, _15line8_, _15line9_, _15line10_, _15line11_, \
  _15line12_, _15line13_, _15line14_ \
};

#define AOSs18(arrayofstrings,string0,string1,string2,string3,string4,string5,string6,string7,string8,string9,string10,string11,string12,string13,string14,string15,string16,string17) \
static char _18line0_[] = string0; \
static char _18line1_[] = string1; \
static char _18line2_[] = string2; \
static char _18line3_[] = string3; \
static char _18line4_[] = string4; \
static char _18line5_[] = string5; \
static char _18line6_[] = string6; \
static char _18line7_[] = string7; \
static char _18line8_[] = string8; \
static char _18line9_[] = string9; \
static char _18line10_[] = string10; \
static char _18line11_[] = string11; \
static char _18line12_[] = string12; \
static char _18line13_[] = string13; \
static char _18line14_[] = string14; \
static char _18line15_[] = string15; \
static char _18line16_[] = string16; \
static char _18line17_[] = string17; \
static char *arrayofstrings[] = { \
  _18line0_, _18line1_, _18line2_, _18line3_, _18line4_, _18line5_, \
  _18line6_, _18line7_, _18line8_, _18line9_, _18line10_, _18line11_, \
  _18line12_, _18line13_, _18line14_, _18line15_, _18line16_, _18line17_ \
};

/******************************************************************************
* MAKEbooARGv/MAKEstrARGv.
* Macros used to build argc/argv on a Macintosh and under Win32.
******************************************************************************/

#if defined(mac) || defined(win32)
#  define MAKEbooARGv(argv,n,boo,t,f) { \
char tS[] = t; \
char fS[] = f; \
size_t len = strlen (BOO(boo,tS,fS)); \
(*argv)[n] = (char *) cdf_AllocateMemory (len + 1, FatalError); \
strcpyX ((*argv)[n++], BOO(boo,tS,fS), 0); \
}
#  define MAKEstrARGv(argv,n,str) { \
char *s = (char *) str; \
(*argv)[n] = (char *) cdf_AllocateMemory (strlen(s) + 1, FatalError);\
strcpyX ((*argv)[n++], s, 0); \
}
#endif

/******************************************************************************
* Global variables.
******************************************************************************/

#if defined(TOOLBOX1)
#  if defined(mac)
Logical pagingOn = FALSE;	/* Online help doesn't need to be paged. */
#  else
Logical pagingOn = TRUE;	/* So online help will be paged. */
#  endif
int soLineCount = 0;
char pgmName[MAX_PROGRAM_NAME_LEN+1] = "";
#else
extern Logical pagingOn;
extern int soLineCount;
extern char pgmName[MAX_PROGRAM_NAME_LEN+1];
#endif

#if defined(mac)
#  if defined(TOOLBOX2)
WindowPtr soWindowP = NULL;	/* If NULL, then a FSI toolkit program. */
TEHandle soTextH;
ControlHandle soVscrollH;
Rect soTextRect;
MenuHandle appleMenuHso, fileMenuHso;
short soDestHeight;
short soViewHeight;
short soLineHeight;
int soNviewLines;
Point soAtPoint;
Rect soStatusRect;
char soStatusLine[SO_STATUS_LINE_LEN+1] = "";
clock_t startClock;
int lastSecond;
#  else
extern WindowPtr soWindowP;
extern TEHandle soTextH;
extern ControlHandle soVscrollH;
extern Rect soTextRect;
extern MenuHandle appleMenuHso, fileMenuHso;
extern short soDestHeight;
extern short soViewHeight;
extern short soLineHeight;
extern int soNviewLines;
extern Point soAtPoint;
extern Rect soStatusRect;
extern char soStatusLine[SO_STATUS_LINE_LEN+1];
extern clock_t startClock;
extern int lastSecond;
#  endif
#endif

/*****************************************************************************
* Function prototypes.
*****************************************************************************/

int UTF8StrLength PROTOARGs((unsigned char *));
void ASSIGNx PROTOARGs((void *, void *, long, long));
Logical EQx PROTOARGs((void *, void *, long, long));
Logical NEx PROTOARGs((void *, void *, long, long));
Logical LEx PROTOARGs((void *, void *, long, long));
Logical LTx PROTOARGs((void *, void *, long, long));
Logical GEx PROTOARGs((void *, void *, long, long));
Logical GTx PROTOARGs((void *, void *, long, long));
Logical IsDir PROTOARGs((char *));
Logical IsWild PROTOARGs((char *));
Logical IsCDF PROTOARGs((char *));
void EncodeRecordJustify PROTOARGs((char *string, long recN, int minWidth,
  size_t width
));
void EncodeIndicesJustify PROTOARGs((
  char *string, long numDims, long indices[], int minWidth, size_t width
));
void EncodeRecordIndicesJustify PROTOARGs((
  char *string, long recN, long numDims, long indices[], int minWidth,
  size_t width
));
void Justify PROTOARGs((char *string, int minWidth));
void RemoveLeadingBlanks PROTOARGs((char *string));
void RemoveLeading0 PROTOARGs((char *string, size_t len));
int ISTPname PROTOARGs((char *name));
void RemoveTrailingBlanks PROTOARGs((char *string));
void RemoveWhiteSpace PROTOARGs((char *string));
void CopyToField PROTOARGs((char *string, char *field, int length));
void CopyFromField PROTOARGs((char *field, int length, char *string));
void CatToString PROTOARGs((
  char *string, char *cat, int length, int justify, char *more
));
double SystemClock PROTOARGs((void));
void ParsePath PROTOARGs((
  char *path, char dir[DU_MAX_DIR_LEN+1], char name[DU_MAX_NAME_LEN+1]
));
QOP *Qop PROTOARGs((int, char *[], char *[], int *));
FILE *OnlineHelpFP PROTOARGs((char *, char *));
void PageOLH PROTOARGs((char *, char *));
int WriteOut PROTOARGs((FILE *, char *));
char PickDelimiter PROTOARGs((char *, size_t));
void ConvertDataType PROTOARGs((long, long, void *, long, long, void *));
int EncodeDimensionality PROTOARGs((char *, long, long *, size_t));
int EncodeVariances PROTOARGs((char *, long, long, long *));
int EncodeRecordIndices PROTOARGs((char *, long, long, long, long *, long *,
  size_t
));
int EncodeRecordIndicesVARY PROTOARGs((char *, long, long, long *, size_t));
int EncodeString PROTOARGs((long, char *, char *, int, int));
int EncodeValue PROTOARGs((long, void *, char *, int, size_t));
int EncodeValueFormat PROTOARGs((
  long dataType, void *binary, char *text, char *format, int minWidth,
  int maxWidth, int style, size_t width
));
int EncodeValuesFormat PROTOARGs((
  long dataType, long numElems, void *binary, char *text, char *format,
  int minWidth, int maxWidth, int style, size_t width
));
void EncodeNegativeZero PROTOARGs((char *string, char *format, size_t width));
void WriteStringValue PROTOARGs((FILE *, long, void *, int, int));
void WriteEntryValue PROTOARGs((FILE *, long, long, void *, int, int));
char *EncodingToken PROTOARGs((long));
char *MajorityToken PROTOARGs((long));
char *FormatToken PROTOARGs((long));
char *ScopeToken PROTOARGs((long));
char *VarianceToken PROTOARGs((long));
char *TFvarianceToken PROTOARGs((long));
char *DataTypeToken PROTOARGs((long));
char *CompressionToken PROTOARGs((long cType, long cParms[CDF_MAX_PARMS]));
char *SparsenessToken PROTOARGs((
  long sRecordsType, long sArraysType, long sArraysParms[CDF_MAX_PARMS]
));
char *ChecksumToken PROTOARGs((long));
long WhichDataType PROTOARGs((char *));
long WhichFormat PROTOARGs((char *));
long WhichMajority PROTOARGs((char *));
long WhichEncoding PROTOARGs((char *));
long WhichCompression PROTOARGs((char *, long *, long *));
long WhichRecSparseness PROTOARGs((char *));
long WhichChecksum PROTOARGs((char *));
Logical DecodeValues PROTOARGs((char *, long, long *, void **, int));
Logical DecodeVariances PROTOARGs((char *, long, long *, long *));
Logical DecodeDelimitedString PROTOARGs((char *, char *));
Logical DecodeDimensionality PROTOARGs((char *, long *, long *));
Logical DecodeRecordAndIndices PROTOARGs((
  char *string, long *recN, long *nIndices, long indices[CDF_MAX_DIMS]
));
char *NextNonSpace PROTOARGs((char *cp));
char *NextNonDigit PROTOARGs((char *cp));
int FormatWidth PROTOARGs((char *));
int FormatPrecision PROTOARGs((char *));
char *strstrIgCase PROTOARGs((char *string, char *substring));
char *strchrIgCase PROTOARGs((char *str, int chr));
int strcmpIgCase PROTOARGs((char *string1, char *string2));
int strncmpIgCase PROTOARGs((char *string1, char *string2, size_t count));
int strncmpIgCasePattern PROTOARGs((char *string, char *pattern, 
  size_t count
));
void OutputWithMargin PROTOARGs((
  FILE *fp, char *text, int maxLen, int marginLen
));
void InitPctLog PROTOARGs((int *lastPct, Logical *pctOn));
void UpdatePctLog PROTOARGs((int pct, int *lastPct));
void RefreshPctLog PROTOARGs((int lastPct));
void CleanupPctLog PROTOARGs((Logical *pctOn));
void Ncharacters PROTOARGs((FILE *fp, int nChars, int chr));
void CatNcharacters PROTOARGs((char *string, int nChars, int chr));
void CpyNcharacters PROTOARGs((char *string, int nChars, int chr));
Logical TFqualifier PROTOARGs((
  QOP *qop, Logical *variable, int Tx, int Fx, Logical defaultTF,
  char *conflictText
));
Logical S2qualifierLong PROTOARGs((
  QOP *qop, long *variable, int S1x, long S1, int S2x, long S2, long defaultS,
  char *conflictText
));
Logical S3qualifierLong PROTOARGs((
  QOP *qop, long *variable, int S1x, long S1, int S2x, long S2, int S3x,
  long S3, long defaultS, char *conflictText
));
Logical S4qualifierLong PROTOARGs((
  QOP *qop, long *variable, int S1x, long S1, int S2x, long S2, int S3x,
  long S3, int S4x, long S4, long defaultS, char *conflictText
));
Logical S6qualifierLong PROTOARGs((
  QOP *qop, long *variable, int S1x, long S1, int S2x, long S2, int S3x,
  long S3, int S4x, long S4, int S5x, long S5, int S6x, long S6, 
  long defaultS, char *conflictText
));
void BlankPadRight PROTOARGs((char *string, int count));
CDFstatus GetFormatEntry PROTOARGs((Logical, long, char **));
Logical ParseOptionList PROTOARGs((
  int nTokens, char *tokens[], char *list, Logical present[]
));
char *NULedString PROTOARGs((char *existingString, size_t nCharacters));
void ReadCharStdIn PROTOARGs((char *key));
void WriteStringStdOut PROTOARGs((char *string, size_t length));
void DisplayError PROTOARGs((char *message));
void DisplayWarning PROTOARGs((char *message));
void DisplayInfo PROTOARGs((char *message));
int DirList PROTOARGs((
  char *dir, int patternC, char **patternS, char ***dirS, char ***nameS
));
int CDFdirList PROTOARGs((char *path, char ***dirS, char ***nameS));
void RemoveExtensions PROTOARGs((int, char **));
char *TempCharStr PROTOARGs((int character));
void DisplayStatistics PROTOARGs((
  char *label, vSTATS *vStatsDotCDF, vSTATS *vStatsStage,
  vSTATS *vStatsCompress
));
void BuildStatistics PROTOARGs((
  char *label, vSTATS *vStats, char temp1[MAX_SCREENLINE_LEN+1],
  char temp2[MAX_SCREENLINE_LEN+1], char temp3[MAX_SCREENLINE_LEN+1]
));
int WriteOutSO VARPROTOARGs((char *format, ...));
int WriteOutFP VARPROTOARGs((FILE *fp, char *format, ...));
void IncrRecordIndicesFirstLast PROTOARGs((
  Logical rowMajor, long *recordN, long numDims, long first[], long last[],
  long indices[]
));
void IncrIndicesFirstLastRow PROTOARGs((
  long numDims, long first[], long last[], long indices[]
));
void IncrIndicesFirstLastCol PROTOARGs((
  long numDims, long first[], long last[], long indices[]
));
Logical AllocateBuffers PROTOARGs((
  long nRecords, long numDims, long dimSizes[], struct GroupStruct *groups,
  int nScalarBuffers, int nHyperBuffers, Byte1 **handles[],
  size_t nValueBytes[], Logical rowMajor, int minNofHypers,
  void (*fatalFnc) PROTOARGs((char *msg))
));
void InitHyperParms PROTOARGs((
  struct HyperStruct *hyperX, struct GroupStruct *groups, long numDims,
  long *nHypers, long *nValues
));
void InitHyperParmsFromAnywhere PROTOARGs((
  struct HyperStruct *hyperX, struct GroupStruct *groups, long numDims,
  long *nHypers, long *nValues, long strRec
));
void IncrHyperParms PROTOARGs((
  struct HyperStruct *hyperX, struct GroupStruct *groups, long numDims,
  Logical rowMajor, long *nValues
));
Logical HyperFullRecord PROTOARGs((struct GroupStruct *groups, long numDims));
long HypersPerRecord PROTOARGs((struct GroupStruct *groups, long numDims));
Logical HyperStartOfRecord PROTOARGs((
  struct HyperStruct *hyperX, long numDims
));
void WriteOutPct PROTOARGs((int pct));
Logical SwitchMajority PROTOARGs((
  Byte1 *buffer, Logical rowMajor, long numDims, long dimSizes[],
  long recCount, size_t nValueBytes
));
Logical CompressedCDF PROTOARGs((char *CDFpath));
Logical SameCompressions PROTOARGs((
  long cType1, long cParms1[CDF_MAX_PARMS],
  long cType2, long cParms2[CDF_MAX_PARMS]
));
Logical SameSparsenesses PROTOARGs((
  long sRecordsType1, long sArraysType1, long sArraysParms1[CDF_MAX_PARMS],
  long sRecordsType2, long sArraysType2, long sArraysParms2[CDF_MAX_PARMS]
));
int IntWidth PROTOARGs((int value));
int Long64Width PROTOARGs((OFF_T value));
void DisplayIdentification PROTOARGs((char *name));
Logical  ParseCacheSizes PROTOARGs((
  char *sizes, long *workingCache, long *stageCache, long *compressCache
));
Logical  ParseRecordRange PROTOARGs((
  char *sizes, long *startingRec, long *endingRec
));
int FindUniqueMatch PROTOARGs((char *target, char *strings[]));
void ParseStringForTokens PROTOARGs((int token, char *string, char *items[]));
int GetNumTokens PROTOARGs((int token, char *string));
Logical ValidateQual PROTOARGs((
  char *qual, char *validQuals[]
));
void DisplayCDFdata PROTOARGs((
  long dataType, long numElems, void *value
));
int BinarySearch PROTOARGs((
  long dataType, void *values, void *value, int min, int max
));
void MergeSort PROTOARGs((
  long dataType, void *values, void *tmp, int left, int right
));
#if defined(mac)
void MacExecuteSO PROTOARGs((
  Logical (*exeFnc)(int argC, char *argV[]),
  Logical (*qopFnc)(int *argC, char **argV[])
));
void MacExecuteFSI PROTOARGs((
  Logical (*exeFnc)(int argC, char *argV[]),
  Logical (*qopFnc)(int *argC, char **argV[])
));
void MacExecuteTimer PROTOARGs((Logical (*exeFnc)(), int argC, char *argV[]));
void MacMessageDialog PROTOARGs((char *severityText, char *messageText));
void InitMacUI PROTOARGs((void));
void FreeMacQOPs PROTOARGs((int argC, char *argV[]));
void CalcBounds PROTOARGs((
  Rect *rect, int width, int height, double topPct, double leftPct
));
void ResetSO PROTOARGs((Logical clear));
void InitMacSO PROTOARGs((void));
void InitMacMenusSO PROTOARGs((void));
void InitMacFSI PROTOARGs((void));
void InitMacMenusFSI PROTOARGs((void));
void UpdateSOscrollBars PROTOARGs((void));
void DrawStatusLine PROTOARGs((char *text));
void MacReadKeySO PROTOARGs((char *key));
Logical SaveSO PROTOARGs((Logical as));
Logical BuildMacPath PROTOARGs((
  FSSpec *spec, char path[DU_MAX_PATH_LEN+1], Logical stripExt
));
void DisplayAbout PROTOARGs((void));
void WriteOutMacSO PROTOARGs((char *text));
void WriteStringSO PROTOARGs((char *string, size_t length));
void MacSOoverFlow PROTOARGs((void));
void CheckForAbortSOmac PROTOARGs((void));
void RemoveCDFSktExtension PROTOARGs((
  char *sktName, char *sktPath
));
pascal Boolean FilterForCDFs PROTOARGs((ParmBlkPtr pb));
pascal Boolean FilterForSKTs PROTOARGs((ParmBlkPtr pb));
#  ifndef __MWERKS__
pascal void OutlineDefaultButton PROTOARGs((DialogPtr window, short itemN));
pascal Boolean FilterDialogQOPfsi PROTOARGs((
  DialogPtr dialog, EventRecord *event, short *itemN
));
pascal Boolean FilterDialogQOPso PROTOARGs((
  DialogPtr dialog, EventRecord *event, short *itemN
));
#  else
pascal void OutlineDefaultButton PROTOARGs((DialogPtr window, SInt16 itemN));
pascal Boolean FilterDialogQOPfsi PROTOARGs((
  DialogPtr dialog, EventRecord *event, SInt16 *itemN
));
pascal Boolean FilterDialogQOPso PROTOARGs((
  DialogPtr dialog, EventRecord *event, SInt16 *itemN
));
#  endif
#endif

#if defined(__cplusplus)
extern "C" {
#endif
void FatalError PROTOARGs((char *message));
char *StatusCodeReportOptions PROTOARGs((Logical e, Logical w, Logical i));
#if defined(win32)
#  if defined(SO)
void CheckForAbortSOwin32 PROTOARGs((void));
void WriteOutWin32 PROTOARGs((char *text));
#  else
void TransferTextAttrs PROTOARGs((char *text, char *attrs));
void SetCursorPosition PROTOARGs((int row, int col));
#  endif
void WinMessageDialog PROTOARGs((char *severityText, char *messageText));
void DebugMessage PROTOARGs((char *text));
#endif
#if defined(__cplusplus)
}
#endif

#if defined(vms)
#define isinf(x) (!finite(x))
#endif

/*****************************************************************************/

#endif
