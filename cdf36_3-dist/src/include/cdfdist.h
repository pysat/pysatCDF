/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                            Header file for CDF distribution.
*
*  Version 2.7e, 14-Dec-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  20-Jul-91, J Love     Original version (for CDF V2.1).
*   V2.0  17-May-92, J Love     IBM PC & HP-UX port.  CDF V2.2.
*   V2.1  15-Sep-92, J Love     CDF V2.3 (shareable/zVar/NeXT).
*   V2.2  30-Oct-92, J Love     Added variable values display to CDFinquire.
*   V2.3  25-Jan-94, J Love     CDF V2.4.
*   V2.3a  8-Feb-94, J Love     DEC Alpha/OpenVMS port.
*   V2.4  13-Dec-94, J Love     CDF V2.5.
*   V2.5  20-Jan-95, J Love     `Trailer' function prototype.
*   V2.5a 19-Jan-95, J Love     IRIX 6.0 (64-bit).
*   V2.5b 28-Feb-95, J Love     `IsGraphChr' function prototype.  Solaris 2.3
*                               IDL i/f.  Pass `char' as `int'.
*   V2.6   6-Apr-95, J Love     POSIX.
*   V2.6a 18-Apr-95, J Love     More POSIX.
*   V2.6b  9-May-95, J Love     Virtual memory under Microsoft C 7.00.  Added
*				`EPOCHdataType'.  EPOCH styles.
*   V2.6c 13-Jun-95, J Love	EPOCH custom format.  Linux.
*   V2.6d 25-Aug-95, J Love	More system include files for UNIX.  Moved
*				`ASSIGNnotNULL' from `cdflib.h'.  CDFexport-
*				related changes.  More virtual memory for
*				Microsoft C 7.00.
*   V2.7   9-Sep-96, J Love	CDF V2.6.
*   V2.7a 26-Nov-96, J Love	Better handling of HP-UX POSIX-compliant
*				compiler (c89).
*   V2.7b 21-Feb-97, J Love	Removed RICE.
*   V2.7c 28-Feb-97, J Love	Windows NT for MS Visual C/C++ on an IBM PC.
*   V2.7d 16-Nov-97, J Love	More Windows NT.
*   V2.7e 14-Dec-97, J Love	Added ALPHAVMSi encoding.
*   V2.7f 27-May-98, M Liu 	SunOS/gnu has stdarg.h while SunOS/bsd doesn't.
*   V2.7g 07-Feb-01, M Liu      Added Solaris running on PC platform.
*   V2.8  02-May-01, M Liu      Added handling for CYGWIN.
*   V2.9  25-Nov-02, M Liu      Added handling for 64-bit Solaris sparcv9.
*   V2.10 08-Apr-04, M Liu      Added a new function for data type CDF_EPOCH16.
*   V2.11 11-Jul-05, M Liu      Added MingW port for PC.
*   V2.12 30-Jan-06, M Liu      Added FreeBSD port.
*   V2.13 09-Jun-06, M Liu      Added HP-UX and AIX for LFS.
*   V2.14 09-Jul-06, M Liu      Added MacOSX on i386 PC (macosXintel).
*   V2.15 18-Apr-08, M Liu      Further modified MacOS X into macosXppc and 
*                               maxosXintel. For PPC, pre-defined macro 
*                               "__ppc__" and "__ppc64__" by gcc for 32/64-
*                               bit mode, respectively on Mac.
*   V2.16 28-Apr-09, M Liu      Added linux-PPC (pre-defined macro "__PPC__"
*                               by gcc for either 32/64-bit).
*   V2.17 16-Jul-12, M Liu      Changed "uInt" to "uInt4", "uLong" to "uLongx"
*                               and "Byte" to "Byte1" so they won't be in
*                               conflict with the same typedef's defined in
*                               zlib.
*   V2.18 09-Aug-13, M Liu      Added cdfid_AllocateMemory, cdfid_getCDFid,
*                               cdfid_FreeMemory.
*
******************************************************************************/

#if !defined(CDFDISTh_INCLUDEd__)
#  define CDFDISTh_INCLUDEd__

/******************************************************************************
* Definitions for misguided compilers, etc.
******************************************************************************/

#if defined(WIN32)
#  define IBMPC
#  define win32
#  if !defined MSVC67
#    if (_MSC_VER == 1300) 
/* VC7, building with pre-VC7 runtime libraries */
#      define MSVC67 
#    endif
#  endif
#  if defined (_MSC_VER) && (_MSC_VER < 1900)
#     define snprintf _snprintf
#     define isnan(x) _isnan(x)
#     define isinf(x) (!_finite(x))
#  endif
#endif

#if defined(MSDOS)              /* Microsoft C. */
#  define dos
#  define MICROSOFTC
#  if _MSC_VER == 600
#    define MICROSOFTC_600
#  endif
#  if _MSC_VER == 700
#    define MICROSOFTC_700
#  endif
#endif

#if defined(__MSDOS__)          /* Borland C. */
#  define dos
#  define BORLANDC
#endif

#if defined(__SALFORD__)        /* Salford C. */
#  define dos 1
#  define SALFORDC 1
#endif

#if defined(dos)
#  define IBMPC
#endif

#if defined(NeXT)
#  define Mach
#endif

#if defined(__ppc__) || (defined(__APPLE__) && !defined(i386) && \
                         !defined(__x86_64__))
#  define POWERPC
#  define unix
#  if defined(__MACH__)  	/* Mac OS X cc (gcc) */
#    define macosXppc
#  endif
#endif

#if defined(__APPLE__) && (defined(i386) || defined(__x86_64__))
#  define unix
#  define IBMPC
#  define macosXintel
#endif

#if defined(AIX)
#  define unix
#endif

#if defined(linux) || defined(__CYGWIN__) || defined(__MINGW32__) || \
    defined(__FreeBSD__)
#  if defined(PPC) || defined(__PPC__) 
#    define POWERPC
#  else
#    if defined(i386) || defined(__amd64) || defined(__x86_64__) || \
        defined(__ia64__)
#      define IBMPC
#    else
#      if defined(__alpha)
#        define alphaosf
#      endif
#    endif
#  endif
#endif

#if defined(__QNX__)
#  define IBMPC
#  define unix
#endif

#if defined(sun) && (defined(i386) || defined(__amd64) || \
    defined(__x86_64__) || defined(__ia64__))
#  define IBMPC
#  undef sun
#endif

#if defined(__alpha)
#  if defined(__osf__)
#    define alphaosf
#  else
#    if defined(vms)
#      define alphavms
#      if __D_FLOAT
#        define alphavmsD
#      endif
#      if __G_FLOAT
#        define alphavmsG
#      endif
#      if __IEEE_FLOAT
#        define alphavmsI
#      endif
#    else           /* "vms" not defined if "-W ansi89" used. */
#      if !defined(linux) && !defined(__FreeBSD__)
#        define posixSHELL
#        define posixSHELLalpha
#        if __D_FLOAT
#          define posixSHELLalphaD
#        endif
#        if __G_FLOAT
#          define posixSHELLalphaG
#        endif
#        if __IEEE_FLOAT
#          define posixSHELLalphaI
#        endif
#      endif
#    endif
#  endif
#endif

#if defined(vax)
#  if defined(vms)
#    define vaxvms
#  else           /* "vms" not defined if "-W ansi89" used. */
#    define posixSHELL
#    define posixSHELLvax
#  endif
#endif

#if defined(macintosh) || defined(__MWERKS__) /* Macintosh MPW or MetroWerks CodeWarrior */
#  define MPW_C
#  define mac

#  if defined(__MWERKS__)			/* MetroWerks CodeWarrior */
#    define STDARG

#    ifdef OLDROUTINENAMES
#      undef OLDROUTINENAMES
#    endif
#    define OLDROUTINENAMES 1

#    ifdef TARGET_OS_MAC
#      undef TARGET_OS_MAC
#    endif
#    define TARGET_OS_MAC 1
#  endif
#endif

#if defined(THINK_C)            /* Macintosh Think C */
#  define mac
#endif

#if defined(HPUXposix)
#  define unix
#  define hpux
#  define HP
#  define _HPUX_SOURCE
#endif

#if defined(HP)
#  if !defined(unix)
#    define unix
#  endif
#  if !defined(hpux)
#    define hpux
#  endif
#endif

#if defined(__MINGW32__)
#  undef win32
#  define unix
#endif

/******************************************************************************
* Determine variable argument list type - `stdarg' or `varargs'.  Currently,
* only SunOS/bsd doesn't have `stdarg.h' (but SunOS/gnu and all Solaris do).
******************************************************************************/

#if defined(sun) && !defined(SOLARIS) && !defined(__GCC_NEW_VARARGS__)
#  define VARARGS
#else
#  define STDARG
#endif

/******************************************************************************
* System include files.
******************************************************************************/

#if defined(vms)
#  include <stdlib.h>
#  include <stdio.h>
#  include <string.h>
#  include <ctype.h>
#  include <time.h>
#  include <math.h>
#  include <unixio.h>
#  include <types.h>
#  include <descrip.h>
#  include <rmsdef.h>
#  include <climsgdef.h>
#  include <smgdef.h>
#  include <ssdef.h>
#  include <stat.h>
#  include <unistd.h>
#  include <lib$routines.h>
#  if defined(VARARGS)
#    include <varargs.h>
#  else
#    include <stdarg.h>
#  endif
#endif

#if (defined(unix) && !defined(__MINGW32__)) || defined(posixSHELL)
#  include <stdlib.h>
#  include <stdio.h>
#  include <string.h>
#  include <ctype.h>
#  include <time.h>
#  include <math.h>
#  include <pwd.h>
#  include <unistd.h>
#  include <sys/types.h>
#  include <sys/stat.h>
#  include <sys/time.h>
#  if defined(VARARGS)
#    include <varargs.h>
#  else
#    include <stdarg.h>
#  endif
#  if defined (AIX) || defined(__QNX__)
#    include <sys/select.h>
#  endif
#  if defined(Mach)
#    include <sys/dir.h>
#  else
#    include <dirent.h>
#  endif
#endif

#if defined(__MACH__)
#  include <mach-o/dyld.h>
#endif

#if defined(dos) || defined(win32) || defined(__MINGW32__)
#  include <stdlib.h>
#  include <stdio.h>
#  include <string.h>
#  include <ctype.h>
#  include <time.h>
#  include <math.h>
#  if defined(win32)
#    include <io.h>
#    include <fcntl.h>
/*#    include <Windows.h> */
#  endif
#  if defined(BORLANDC)
#    include <dir.h>
#  endif
#  if !defined(SALFORDC)
#    include <dos.h>
#  endif
#  if defined(__MINGW32__)
#    include <dirent.h>
#  endif
#  include <conio.h>
#  include <sys\types.h>
#  include <sys\stat.h>
#  if defined(VARARGS)
#    include <varargs.h>
#  else
#    include <stdarg.h>
#  endif
#endif

#if defined(mac)
#  if defined(MPW_C)
#    include <StdLib.h>
#    include <StdIO.h>
#    include <String.h>
#    include <CType.h>
#    include <Time.h>
#    include <Math.h>
#    include <Types.h>
#    include <Fonts.h>
#    include <Windows.h>
#    include <Dialogs.h>
#    include <Textedit.h>
#    include <Menus.h>
#    include <Resources.h>
#    include <StandardFile.h>
#    include <ToolUtils.h>
#    if defined(__MWERKS__)
#      include <stdarg.h>
#      include <Controls.h>
#      include <Devices.h>
#      include <Sound.h>
#    else
#      if defined(STDARG)
#        include <StdArg.h>
#      endif
#    endif
#    include <Files.h>
#    include <Quickdraw.h>
#    include <Events.h>
#    ifndef __MWERKS__
#      include <OSEvents.h>
#      include <Desk.h>
#    endif
#  endif
#  if defined(THINK_C)
#    include <stdlib.h>
#    include <stdio.h>
#    include <string.h>
#    include <ctype.h>
#    include <time.h>
#    include <math.h>
#    include <stat.h>
#    if defined(STDARG)
#      include <stdarg.h>
#    endif
#  endif
#endif

/******************************************************************************
* If using the POSIX-compliant C comiler on an HP, redefine the macros that
* MAY have been undefined by the system include files.
******************************************************************************/

#if defined(HPUXposix)
#  if !defined(unix)
#    define unix
#  endif
#  if !defined(hpux)
#    define hpux
#  endif
#endif

/******************************************************************************
* Typedefs.
******************************************************************************/

typedef int Logical;

typedef unsigned short uShort;
typedef unsigned int uInt4;
typedef unsigned long uLongx;

#if defined(alphaosf) || defined(IRIX64bit) || defined(_LP64) || \
    defined(__amd64) || defined(__x86_64__) || defined(__ia64__) || \
    defined(AIX64) || defined(HP64) || defined(__PPC64__) || \
    defined(__ppc64__)
   /***************************************************************************
   * `long' is 64 bits on a DEC Alpha/OSF1, an SGi/IRIX 6.x using the `-64'
   * option (64-bit objects) and a Solaris sparcv9, AMD64 and EM64T/IA64.
   * They are also 64-bits on AIX, HP-UX and PPC if 64-bit mode is on.
   * `int' is 32 bits in these cases.
   ***************************************************************************/
   typedef int Int32;
   typedef uInt4 uInt32;
   typedef long Int64;
#else
   typedef long Int32;
   typedef uLongx uInt32;
   typedef long long Int64;
#endif

typedef short Int16;
typedef uShort uInt16;

#if defined(AIX) || defined(dos) || defined(sgi) || defined(__QNX__)
   /***************************************************************************
   * `char' is unsigned by default under AIX, MS-DOS, IRIX, and QNX...
   ***************************************************************************/
   typedef signed char sChar;
#else
   /***************************************************************************
   * ...and signed by default everywhere else.
   ***************************************************************************/
   typedef char sChar;
#endif

typedef unsigned char uChar;

#if !defined(mac)
  /****************************************************************************
  * `Byte' is already defined in `types.h' on a Macintosh (MPW C & Think C).
  ****************************************************************************/
  typedef uChar Byte1;
#endif

typedef uChar uByte;
typedef sChar sByte;

/******************************************************************************
* Non-system include files.
******************************************************************************/

#include "cdf.h"
#include "cdfconfig.h"

#define FSEEK fseek
#define FTELL ftell
#if defined(STAT)
#  undef STAT
#endif
/* #define STAT stat */
#define FOPEN fopen
#define FWRITE fwrite
#define FREAD fread
#define FCLOSE fclose
#define FLUSH fflush

#if (_FILE_OFFSET_BITS == 64) || defined(_LARGEFILE_SOURCE) || \
    defined(_LARGEFILE64_SOURCE) || defined(_LARGEFILE)
#  if defined(win32)
     typedef __int64 OFF_T;
#    define FSEEK64 _lseeki64
#    define FTELL64 _telli64
#    define FOPEN64 _open
#    define FWRITE64 _write
#    define FREAD64 _read
#    define FCLOSE64 _close
#    define FLUSH64 _commit
#    define STAT    _stat64
#  else
#    if defined(__MINGW32__) || defined(AIX)
       typedef off64_t OFF_T;
#    else
       typedef off_t OFF_T;
#    endif
#    if defined(sgi)
#      define FSEEK64 fseek64
#      define FTELL64 ftell64
#    else
#      if defined(__MINGW32__)
#        define FSEEK64 fseeko64
#        define FTELL64 ftello64
#      else
#        define FSEEK64 fseeko
#        define FTELL64 ftello
#      endif
#    endif
#    if defined(__osf__) || defined(__CYGWIN__) || \
        defined(vms) || defined(sgi) || defined(__MINGW32__) || \
        defined(__FreeBSD__) || defined(macosXintel) || defined(macosXppc) || \
        defined(__PPC__)
#      define FOPEN64 fopen
#    else
#      define FOPEN64 fopen64
#    endif
#    define FWRITE64 fwrite
#    define FREAD64  fread
#    define FCLOSE64 fclose
#    define FLUSH64  fflush
#    if defined(__CYGWIN__) || defined(vms)
#      define STAT     stat
#    else
#      define STAT     stat64
#    endif
#  endif
#else
   typedef long OFF_T;
#  define STAT  stat
#endif

/******************************************************************************
* Configuration-dependent system include files.
******************************************************************************/

#if defined(MICROSOFTC_700)
#  if INCLUDEvMEMORY
#    include <vmemory.h>
#  endif
#endif

/******************************************************************************
* Debugging.
******************************************************************************/

#define TraceZ(msg) {printf(msg); fflush(stdout); fflush(stderr);}

/******************************************************************************
* Variable argument list function prototyping.
******************************************************************************/

#if defined(STDARG)
#  if defined(PROTOs_)
#    define VARPROTOARGs(args) args
#  else
#    define VARPROTOARGs(args) ()
#  endif
#else
#  if defined(PROTOs_)
#    define VARPROTOARGs(args) ()
#  else
#    define VARPROTOARGs(args) ()
#  endif
#endif

/*****************************************************************************
* Floating-point encodings.
*   1..........Sun, SGi, IBM-RS, HP, NeXT, Macintosh, PowerPC
*   2..........DECstation, IBM-PC, Alpha (OSF/1), Alpha (OpenVMS - IEEE_FLOAT)
*   3..........VAX, Alpha (OpenVMS - D_FLOAT)
*   4..........Alpha (OpenVMS - G_FLOAT)
*****************************************************************************/

#if defined(sun) || defined(MIPSEB) || defined(IBMRS) || defined(HP) || \
    defined(NeXT) || defined(mac) || defined(POWERPC)
#  define FP1cpu
#endif

#if defined(MIPSEL) || defined(IBMPC) || defined(alphaosf) || \
    defined(alphavmsI) || defined(posixSHELLalphaI)
#  define FP2cpu
#endif

#if defined(vax) || defined(alphavmsD) || defined(posixSHELLalphaD)
#  define FP3cpu
#endif

#if defined(alphavmsG) || defined(posixSHELLalphaG)
#  define FP4cpu
#endif

/*****************************************************************************
* VA_START.
*     This is necessary because of some HP9000s which don't like it when
* `va_start' is passed an element of a structure.  It is apparently related
* to the version of HP-UX being used (other HP9000s are happy with the
* structure element being passed).
*****************************************************************************/

#if defined(hpux)
#  define VA_START(ap) { \
va_list _apT_; \
va_start (_apT_); \
ap = _apT_; \
}
#else
#  define VA_START(ap) va_start(ap)
#endif

/*****************************************************************************
* Sizes/limits/constants.
*****************************************************************************/

#define NO_RECORD		(-1)
#define NO_ENTRY		(-1)

#define DU_MAX_DIR_LEN		256
#define DU_MAX_NAME_LEN		256
#define DU_MAX_PATH_LEN		DU_MAX_DIR_LEN + DU_MAX_NAME_LEN

#define MAX_aMODE_LEN		3

#if defined(BORLANDC) || defined(MICROSOFTC)
#  define LIMITof64K      1
#else
#  define LIMITof64K      0
#endif

/*****************************************************************************
* Compression.
*****************************************************************************/

#define NUM_RLE_PARMS		1
#define NUM_HUFF_PARMS		1
#define NUM_AHUFF_PARMS		1
#define NUM_GZIP_PARMS		1

/*****************************************************************************
* ASCII characters.
*   Beware when using NL and CR with MPW C.  Their values are reversed from
* all of the other platforms/compilers.
*****************************************************************************/

#define Nl      '\n'            /* Newline. */
#define Cr      '\r'            /* Carriage return. */
#define Bs      '\b'            /* Backspace. */
#define Ht	'\t'		/* Horizontal tab. */
#define NUL     '\0'            /* Null character. */

/*****************************************************************************
* C Run-time Library redirections.
*****************************************************************************/

#if defined(dos)
#  define cfree free
#endif

#if defined(unix)
#  if (defined(sun) && (!defined(_SUNOS_VTOC_8) && !defined(_SUNOS_VTOC_16))) \
      || defined(sgi)
#    define memmove(dst,src,nbytes) bcopy(src,dst,nbytes)
#  endif
#endif

/*****************************************************************************
* Range macros.
*****************************************************************************/

#define INCLUSIVE(a,b,c) ((a <= b) && (b <= c))
#define EXCLUSIVE(a,b,c) ((a < b) && (b < c))

/******************************************************************************
* CDFstatus checking macros.
* WARNING: Be careful that `status' is not an embedded function call when
* using `StatusWARN' (it will be called twice).
******************************************************************************/

#define StatusOK(status) ((CDFstatus) status > CDF_WARN)
#define StatusBAD(status) ((CDFstatus) status < CDF_WARN)
#define StatusERROR(status) ((CDFstatus) status < CDF_WARN)
#define StatusWARN(status) EXCLUSIVE(CDF_WARN,(CDFstatus) status,CDF_OK)
#define StatusINFO(status) ((CDFstatus) status > CDF_OK)

/*****************************************************************************
* TRUE/FALSE (for where they don't exist).
*****************************************************************************/

#if !defined(TRUE)
#  define TRUE 1
#endif

#if !defined(FALSE)
#  define FALSE 0
#endif

#define LogicalTRUE	((Logical) TRUE)
#define LogicalFALSE	((Logical) FALSE)

/*****************************************************************************
* Bit macros.
*****************************************************************************/

#define BITSET(a,bit) ((a & (1 << bit)) ? TRUE : FALSE)
#define BITCLR(a,bit) ((!(a & (1 << bit))) ? TRUE : FALSE)

#define SETBIT(a,bitN) a = a | (1 << bitN)
#define CLRBIT(a,bitN) a = a & ~(1 << bitN)
#define FLPBIT(a,bitN) a = a ^ (1 << bitN)

/*****************************************************************************
* Minimum/Maximum macros.
*****************************************************************************/

#if !defined(MINIMUM)
#  define MINIMUM(a,b) ((a) < (b) ? (a) : (b))
#endif

#if !defined(MAXIMUM)
#  define MAXIMUM(a,b) ((a) > (b) ? (a) : (b))
#endif

#if !defined(MINIMUM64)
#  define MINIMUM64(a,b) ((OFF_T)(a) < (OFF_T)(b) ? (OFF_T)(a) : (OFF_T)(b))
#endif

#if !defined(MAXIMUM64)
#  define MAXIMUM64(a,b) ((OFF_T)(a) > (OFF_T)(b) ? (OFF_T)(a) : (OFF_T)(b))
#endif

#define MINIMUMof3(a,b,c) (MINIMUM(a,MINIMUM(b,c)))
#define MAXIMUMof3(a,b,c) (MAXIMUM(a,MAXIMUM(b,c)))

#define MINIMUM64of3(a,b,c) (MINIMUM64(a,MINIMUM64(b,c)))
#define MAXIMUM64of3(a,b,c) (MAXIMUM64(a,MAXIMUM64(b,c)))

/*****************************************************************************
* ONEof/PICKfrom macros.
*****************************************************************************/

#define ONEof2(a,b1,b2) ((a == b1) || (a == b2))
#define ONEof3(a,b1,b2,b3) ((a == b1) || (a == b2) || (a == b3))
#define ONEof6(a,b1,b2,b3,b4,b5,b6) \
((a == b1) || (a == b2) || (a == b3) || (a == b4) || (a == b5) || (a == b6))

#define PICKfrom3(a,b1,c1,b2,c2,b3,c3) \
(BOO(a == b1,c1,BOO(a == b2,c2,BOO(a == b3,c3,0))))

/******************************************************************************
* BOO.
*   Choose one of two things based on a boolean value.
******************************************************************************/

#define BOO(b,t,f) (b ? t : f)

/******************************************************************************
* NULstring.
*    Macro to check if a null-string (length of zero).
******************************************************************************/

#define NULstring(string) (string[0] == NUL)

/******************************************************************************
* MakeNUL.
*    Macro which makes a string a null-string (length of 0).
******************************************************************************/

#define MakeNUL(string) strcpy(string,"")

/******************************************************************************
* ROWmajor/COLmajor.
******************************************************************************/

#define ROWmajor(majority) (majority == ROW_MAJOR)
#define COLmajor(majority) (majority == COLUMN_MAJOR)

/******************************************************************************
* CHECKsum.
******************************************************************************/

#define CHECKsum(checksum) (checksum != NO_CHECKSUM)

/******************************************************************************
* STRINGdataType.
*    Macro to check if a character string data type.
******************************************************************************/

#define STRINGdataType(dataType) \
(dataType == CDF_CHAR || dataType == CDF_UCHAR)

/******************************************************************************
* CDFstringDataType.
*    Macro to check if a character string data type.
******************************************************************************/

#define CDFstringDataType(dataType) \
(dataType == CDF_CHAR || dataType == CDF_UCHAR)

/******************************************************************************
* CDFintDataType.
*    Macro to check if one of the CDF integer data types.
******************************************************************************/

#define CDFintDataType(dataType) \
(dataType == CDF_INT1 || dataType == CDF_UINT1 || dataType == CDF_INT2 || dataType == CDF_UINT2 || dataType == CDF_INT4 || dataType == CDF_UINT4 || dataType == CDF_INT8)

/******************************************************************************
* CDFfloatDataType.
*    Macro to check if one of the CDF float data types.
******************************************************************************/

#define CDFfloatDataType(dataType) \
(dataType == CDF_REAL4 || dataType == CDF_REAL8 || dataType == CDF_FLOAT || dataType == CDF_DOUBLE)

/******************************************************************************
* CDFepochDataType.
*    Macro to check if one of the CDF epoch data types.
******************************************************************************/

#define CDFepochDataType(dataType) \
(dataType == CDF_EPOCH || dataType == CDF_EPOCH16 || dataType == CDF_TIME_TT2000)

/******************************************************************************
* EPOCHdataType.
*    Macro to check if an EPOCH data type.
******************************************************************************/

#define EPOCHdataType(dataType) (dataType == CDF_EPOCH)

/******************************************************************************
* EPOCH16dataType.
*    Macro to check if an EPOCH16 data type.
******************************************************************************/

#define EPOCH16dataType(dataType) (dataType == CDF_EPOCH16)

/******************************************************************************
* TT2000dataType.
*    Macro to check if an CDF_TIME_TT2000 data type.
******************************************************************************/

#define TT2000dataType(dataType) (dataType == CDF_TIME_TT2000)

/******************************************************************************
* EofS (End of String).
*   Macro which expands to a pointer to the NUL character at the end of a
* character string.
******************************************************************************/

#define EofS(string) &string[strlen(string)]

/******************************************************************************
* ASSIGNnotNULL & ASSIGNnotNULLarray.
*   These is used in those cases where the caller of a routine has the option
* of passing in a NULL pointer for those parameters for which a returned
* value is not desired.
******************************************************************************/

#define ASSIGNnotNULL(ptr,value) if (ptr != NULL) *ptr = value;

#define ASSIGNnotNULLarray(ptr,count,values) \
if (ptr != NULL) { \
  int _i_; for (_i_ = 0; _i_ < count; _i_++) ptr[_i_] = values[_i_]; \
}

/******************************************************************************
* Entry type constants/macros.
******************************************************************************/

#define gENTRYt         1
#define rENTRYt         2
#define zENTRYt         3

#define E3(type,itemG,itemR,itemZ) \
(BOO(type == gENTRYt, \
     itemG,BOO(type == rENTRYt, \
	       itemR,BOO(type == zENTRYt,itemZ,0))))

#define E3p(E,g,r,z) \
(BOO(E == g,gENTRYt,BOO(E == r,rENTRYt,BOO(E == z,zENTRYt,0))))

/******************************************************************************
* TOObigIBMpc.  Check for overflow on IBM PCs (running MS-DOS) because of 64Kb
* limit on segment sizes.
******************************************************************************/

#define TOObigIBMpc(size) ((long) size > 65535L)

/******************************************************************************
* ARRAYtoVALUE.
******************************************************************************/

#define ARRAYtoVALUE(array,value,count) \
{int _i_; for (_i_ = 0; _i_ < count; _i_++) array[_i_] = value;}

/******************************************************************************
* ARRAYtoARRAY.
******************************************************************************/

#define ARRAYtoARRAY(dst,src,count) \
{int _i_; for (_i_ = 0; _i_ < count; _i_++) dst[_i_] = src[_i_];}

/******************************************************************************
* vSTATS structure.
******************************************************************************/

typedef struct vSTATSstruct {
  int maxBuffers;       /* Maximum number of cache buffers to be used. */
  int nBuffers;         /* Actual number of cache buffers being used. */
  long nV_reads;        /* Number of calls to `V_read'. */
  long nV_writes;       /* Number of calls to `V_write'. */
  long nBlockReads;     /* Number of file blocks which have been read. */
  long nBlockWrites;    /* Number of file blocks which have been written. */
  long nPageIns;        /* Number of blocks paged in from the file. */
  long nPageOuts;       /* Number of blocks paged out to the file. */
} vSTATS;

/******************************************************************************
* Entry items...
******************************************************************************/

#define ENTRY(eT) E3(eT,gENTRY_,rENTRY_,zENTRY_)
#define ENTRY_DATA(eT) E3(eT,gENTRY_DATA_,rENTRY_DATA_,zENTRY_DATA_)
#define ENTRY_NAME(eT) E3(eT,0L,rENTRY_NAME_,zENTRY_NAME_)
#define ENTRY_EXISTENCE(eT) E3(eT,gENTRY_EXISTENCE_, \
				  rENTRY_EXISTENCE_, \
				  zENTRY_EXISTENCE_)
#define ENTRY_DATATYPE(eT) E3(eT,gENTRY_DATATYPE_, \
				 rENTRY_DATATYPE_, \
				 zENTRY_DATATYPE_)
#define ENTRY_NUMELEMS(eT) E3(eT,gENTRY_NUMELEMS_, \
				 rENTRY_NUMELEMS_, \
				 zENTRY_NUMELEMS_)
#define ENTRY_DATASPEC(eT) E3(eT,gENTRY_DATASPEC_, \
				 rENTRY_DATASPEC_, \
				 zENTRY_DATASPEC_)
#define CURENTRY_EXISTENCE(eT) E3(eT,CURgENTRY_EXISTENCE_, \
				     CURrENTRY_EXISTENCE_, \
				     CURzENTRY_EXISTENCE_)

/******************************************************************************
* Variable items...
******************************************************************************/

#define VAR(zOp) BOO(zOp,zVAR_,rVAR_)
#define VAR_NAME(zOp) BOO(zOp,zVAR_NAME_,rVAR_NAME_)
#define VAR_PADVALUE(zOp) BOO(zOp,zVAR_PADVALUE_,rVAR_PADVALUE_)
#define VAR_DATATYPE(zOp) BOO(zOp,zVAR_DATATYPE_,rVAR_DATATYPE_)
#define VAR_NUMELEMS(zOp) BOO(zOp,zVAR_NUMELEMS_,rVAR_NUMELEMS_)
#define VAR_RECVARY(zOp) BOO(zOp,zVAR_RECVARY_,rVAR_RECVARY_)
#define VAR_DIMVARYS(zOp) BOO(zOp,zVAR_DIMVARYS_,rVAR_DIMVARYS_)
#define VAR_DATA(zOp) BOO(zOp,zVAR_DATA_,rVAR_DATA_)
#define VAR_HYPERDATA(zOp) BOO(zOp,zVAR_HYPERDATA_,rVAR_HYPERDATA_)
#define VAR_SEQDATA(zOp) BOO(zOp,zVAR_SEQDATA_,rVAR_SEQDATA_)
#define VAR_SEQPOS(zOp) BOO(zOp,zVAR_SEQPOS_,rVAR_SEQPOS_)
#define VAR_MAXREC(zOp) BOO(zOp,zVAR_MAXREC_,rVAR_MAXREC_)
#define VAR_MAXallocREC(zOp) BOO(zOp,zVAR_MAXallocREC_,rVAR_MAXallocREC_)
#define VAR_DATASPEC(zOp) BOO(zOp,zVAR_DATASPEC_,rVAR_DATASPEC_)
#define VAR_INITIALRECS(zOp) BOO(zOp,zVAR_INITIALRECS_,rVAR_INITIALRECS_)
#define VAR_nINDEXRECORDS(zOp) BOO(zOp,zVAR_nINDEXRECORDS_,rVAR_nINDEXRECORDS_)
#define VAR_nINDEXENTRIES(zOp) BOO(zOp,zVAR_nINDEXENTRIES_,rVAR_nINDEXENTRIES_)
#define VAR_EXISTENCE(zOp) BOO(zOp,zVAR_EXISTENCE_,rVAR_EXISTENCE_)
#define VAR_NUMBER(zOp) BOO(zOp,zVAR_NUMBER_,rVAR_NUMBER_)
#define VAR_CACHESIZE(zOp) BOO(zOp,zVAR_CACHESIZE_,rVAR_CACHESIZE_)
#define VAR_ALLOCATERECS(zOp) BOO(zOp,zVAR_ALLOCATERECS_,rVAR_ALLOCATERECS_)
#define VAR_COMPRESSION(zOp) BOO(zOp,zVAR_COMPRESSION_,rVAR_COMPRESSION_)
#define VAR_SPARSERECORDS(zOp) BOO(zOp,zVAR_SPARSERECORDS_,rVAR_SPARSERECORDS_)
#define VAR_SPARSEARRAYS(zOp) BOO(zOp,zVAR_SPARSEARRAYS_,rVAR_SPARSEARRAYS_)
#define VAR_ALLOCATEBLOCK(zOp) BOO(zOp,zVAR_ALLOCATEBLOCK_,rVAR_ALLOCATEBLOCK_)
#define VAR_NUMRECS(zOp) BOO(zOp,zVAR_NUMRECS_,rVAR_NUMRECS_)
#define VAR_NUMallocRECS(zOp) BOO(zOp,zVAR_NUMallocRECS_,rVAR_NUMallocRECS_)
#define VAR_ALLOCATEDFROM(zOp) BOO(zOp,zVAR_ALLOCATEDFROM_,rVAR_ALLOCATEDFROM_)
#define VAR_ALLOCATEDTO(zOp) BOO(zOp,zVAR_ALLOCATEDTO_,rVAR_ALLOCATEDTO_)
#define VAR_nINDEXLEVELS(zOp) BOO(zOp,zVAR_nINDEXLEVELS_,rVAR_nINDEXLEVELS_)
#define VAR_RECORDS(zOp) BOO(zOp,zVAR_RECORDS_,rVAR_RECORDS_)
#define VAR_RECORDS_RENUMBER(zOp) BOO(zOp,zVAR_RECORDS_RENUMBER_,rVAR_RECORDS_RENUMBER_)
#define VAR_BLOCKINGFACTOR(zOp) BOO(zOp,zVAR_BLOCKINGFACTOR_, \
					rVAR_BLOCKINGFACTOR_)
#define VAR_RESERVEPERCENT(zOp) BOO(zOp,zVAR_RESERVEPERCENT_, \
					rVAR_RESERVEPERCENT_)
#define VAR_RECNUMBER(zOp) BOO(zOp,zVAR_RECNUMBER_,rVARs_RECNUMBER_)
#define VAR_RECCOUNT(zOp) BOO(zOp,zVAR_RECCOUNT_,rVARs_RECCOUNT_)
#define VAR_RECINTERVAL(zOp) BOO(zOp,zVAR_RECINTERVAL_,rVARs_RECINTERVAL_)

#define VARs_MAXREC(zOp) BOO(zOp,zVARs_MAXREC_,rVARs_MAXREC_)
#define VARs_RECDATA(zOp) BOO(zOp,zVARs_RECDATA_,rVARs_RECDATA_)
#define VARs_RECNUMBER(zOp) BOO(zOp,zVARs_RECNUMBER_,rVARs_RECNUMBER_)
#define VARs_CACHESIZE(zOp) BOO(zOp,zVARs_CACHESIZE_,rVARs_CACHESIZE_)

/******************************************************************************
* Function prototypes for routines contained in the CDF library but also used
* by the toolkit/toolbox/IDL.
******************************************************************************/

#if defined(__cplusplus)
  extern "C" {
#endif
VISIBLE_PREFIX void *cdf_AllocateMemory PROTOARGs((
  size_t nBytes, void (*fatalFnc) PROTOARGs((char *msg))
));
VISIBLE_PREFIX void *cdf_ReallocateMemory PROTOARGs((
  void *ptr, size_t nBytes, void (*fatalFnc) PROTOARGs((char *msg))
));
VISIBLE_PREFIX int cdf_FreeMemory PROTOARGs((
  void *ptr, void (*fatalFnc) PROTOARGs((char *msg))
));
VISIBLE_PREFIX int cdf_FreeMemoryX PROTOARGs((
  void *ptr, void (*fatalFnc) PROTOARGs((char *msg)), int loc
));
VISIBLE_PREFIX void *CallocateMemory PROTOARGs((
  size_t nObjects, size_t objSize, void (*fatalFnc) PROTOARGs((char *msg))
));
VISIBLE_PREFIX void cdfid_AllocateMemory PROTOARGs((
  void *ptr, void (*fatalFnc) PROTOARGs((char *msg))
));
VISIBLE_PREFIX int cdfid_FreeMemory PROTOARGs((
  void *ptr, void (*fatalFnc) PROTOARGs((char *msg))
));
VISIBLE_PREFIX CDFid cdfid_getCDFid PROTOARGs((
  Int32 id, void (*fatalFnc) PROTOARGs((char *msg))
));
VISIBLE_PREFIX char *strcpyX PROTOARGs((char *dst, char *src, size_t max));
VISIBLE_PREFIX char *strcatX PROTOARGs((char *dst, char *src, size_t max));
#if defined(__cplusplus)
  }
#endif

VISIBLE_PREFIX void ClearBytes PROTOARGs((
  void *buffer, int firstByte, int lastByte
));
VISIBLE_PREFIX char *catchrX PROTOARGs((char *dst, int src, size_t max));
VISIBLE_PREFIX char *prependX PROTOARGs((char *dst, char *src, size_t max));
VISIBLE_PREFIX Logical IsReg PROTOARGs((char *));
VISIBLE_PREFIX void ExpandPath PROTOARGs((
  char *shortPath, char longPath[DU_MAX_PATH_LEN]
));
VISIBLE_PREFIX void TimeStamp PROTOARGs((char *));
VISIBLE_PREFIX int strcmpITB PROTOARGs((char *, char *));
VISIBLE_PREFIX Logical NegativeZeroReal8 PROTOARGs((double *value));
VISIBLE_PREFIX Logical NegativeZeroReal4 PROTOARGs((float *value));
VISIBLE_PREFIX void MakeUpperString PROTOARGs((char *string));
VISIBLE_PREFIX void MakeLowerString PROTOARGs((char *string));
VISIBLE_PREFIX void INCRindicesROW PROTOARGs((
  long numDims, long *dimSizes, long *indices
));
VISIBLE_PREFIX void INCRindicesCOL PROTOARGs((
  long numDims, long *dimSizes, long *indices
));
VISIBLE_PREFIX Logical Trailer PROTOARGs((char *string, char *trailer));
VISIBLE_PREFIX int MakeLower PROTOARGs((int c));
VISIBLE_PREFIX int MakeUpper PROTOARGs((int c));
VISIBLE_PREFIX int Printable PROTOARGs((int c));
VISIBLE_PREFIX int UpperCase PROTOARGs((int c));
VISIBLE_PREFIX int LowerCase PROTOARGs((int c));
VISIBLE_PREFIX int Alphabetic PROTOARGs((int c));
VISIBLE_PREFIX int Decimal PROTOARGs((int c));
VISIBLE_PREFIX int Spacing PROTOARGs((int c));
VISIBLE_PREFIX int Visible PROTOARGs((int c));
VISIBLE_PREFIX CDFid Int32ToCDFid PROTOARGs((Int32 id));
VISIBLE_PREFIX Int32 CDFidToInt32 PROTOARGs((CDFid id));
VISIBLE_PREFIX Logical sX PROTOARGs((CDFstatus cStatus, CDFstatus *pStatus));
VISIBLE_PREFIX void AppendToDir PROTOARGs((char *, char *));
VISIBLE_PREFIX int CompressionParmsCount PROTOARGs((Int32 cType));
VISIBLE_PREFIX int SparsenessParmsCount PROTOARGs((Int32 sArraysType));
VISIBLE_PREFIX int MinInt PROTOARGs((int a, int b));
VISIBLE_PREFIX Int32 MinInt32 PROTOARGs((Int32 a, Int32 b));
VISIBLE_PREFIX long MinLong PROTOARGs((long a, long b));
VISIBLE_PREFIX OFF_T MinLongLong PROTOARGs((OFF_T a, OFF_T b));
VISIBLE_PREFIX int MaxInt PROTOARGs((int a, int b));
VISIBLE_PREFIX Int32 MaxInt32 PROTOARGs((Int32 a, Int32 b));
VISIBLE_PREFIX long MaxLong PROTOARGs((long a, long b));
VISIBLE_PREFIX Int64 MaxInt64 PROTOARGs((Int64 a, Int64 b));
VISIBLE_PREFIX Int64 MinInt64 PROTOARGs((Int64 a, Int64 b));
VISIBLE_PREFIX OFF_T MaxLongLong PROTOARGs((OFF_T a, OFF_T b));
VISIBLE_PREFIX int EndsWith PROTOARGs((char *s1, char *s2));
VISIBLE_PREFIX int EndsWithIgCase PROTOARGs((char *s1, char *s2));
VISIBLE_PREFIX int StrLaststr PROTOARGs((char *s1, char *s2));
VISIBLE_PREFIX int StrLaststrIgCase PROTOARGs((char *s1, char *s2));
VISIBLE_PREFIX void RemoveCDFFileExtension PROTOARGs((
  char *fileName, char *dstPath
));

#if defined(mac)
   STATICforIDL Logical MacDirSpecified PROTOARGs((
    char *path, short *vRefNum, long *dirID
));
   STATICforIDL char *PstrcpyX PROTOARGs((char *dst, char *src, size_t max));
   STATICforIDL char *PstrcatX PROTOARGs((char *dst, char *src, size_t max));
   STATICforIDL char *PprependX PROTOARGs((char *dst, char *src, size_t max));
#  if defined(MPW_C)
     STATICforIDL uChar *CtoPstr PROTOARGs((char *string));
     STATICforIDL char *PtoCstr PROTOARGs((uChar *string));
#  endif
#endif

/*****************************************************************************/

#endif
