/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                    Header file for CDF library.
*
*  Version 3.7e, 18-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  22-Jan-91, R Kulkarni Original version (for CDF V2.0).
*                    J Love
*
*   V2.0   3-Jun-91, J Love     Renamed (was cdf_internal.h).  Modified for
*                               CDF V2.1, namely the moving of constant macros
*                               to "cdf.h" for the INTERNAL interface and the
*                               single/multi-file option.  Renamed some symbols
*                               for clarity.
*   V2.1  24-Jun-91, J Love     Fixed for SGi port.
*   V2.2   8-Aug-91, J Love     Added 'Eof' (feof).  Added some miscellaneous
*                               macro definitions.  Renamed (was cdflib.h).
*                               Added support for Cray/UNICOS.  Added 'Exit'
*                               and 'ExitBAD'.  Added VIO_FOR_STREAM.
*                               Functions in VIO were renamed to avoid
*                               collisions on SGi/IRIX.
*   V3.0  21-Apr-92, J Love     IBM PC & HP-UX port.  CDF V2.2.
*   V3.1  21-Sep-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V3.2  24-Jan-94, J Love     CDF V2.4.
*   V3.2a  8-Feb-94, J Love     DEC Alpha/OpenVMS port.
*   V3.3  15-Dec-94, J Love     CDF V2.5.
*   V3.4   9-Jan-95, J Love     Encode/decode changes.  More cache-residency.
*   V3.5  19-Jan-95, J Love     IRIX 6.0 (64-bit).
*   V3.5a 15-Mar-95, J Love     Solaris 2.3 IDL i/f.  Fixed `recNum' argument
*                               of `LastAllocatedRecord'.
*   V3.6  28-Mar-95, J Love     POSIX.
*   V3.6a 18-Apr-95, J Love     More POSIX.
*   V3.6b  8-May-95, J Love     Virtual memory under Microsoft C 7.00.
*   V3.6c 13-Jun-95, J Love     Linux.
*   V3.6d  7-Sep-95, J Love     Moved `ASSIGNnotNULL' to `cdfdist.h'.
*                               CDFexport-related changes.  More virtual
*                               memory (Microsoft C 7.00).
*   V3.7   5-Sep-96, J Love     CDF V2.6.
*   V3.7a 20-Nov-96, J Love	Removed definitions of BIG/LITTLE_ENDIAN which
*				were no longer used and caused a redefinition
*				on Linux systems.
*   V3.7b 21-Feb-97, J Love	Removed RICE.
*   V3.7c  8-Mar-97, J Love	Windows NT for MS Visual C/C++ on an IBM PC.
*   V3.7d  2-Sep-97, J Love	Made magic number variables `uInt32'.
*   V3.7e 18-Nov-97, J Love	More Windows NT.
*   V3.7f 25-May-98, M Liu 	Add 3 new fields in VarStruct data structure
*				Add one more parameter in HyperReadDim and
*				HyperWriteDim proto function.
*   V3.8  04-May-01, M Liu      Add Cygwin port.
*   V3.9  08-Apr-04, M Liu      Add CURr/zVarOffset fields to the CDF
*                               structure.
*   V3.10 11-Jul-05, M Liu      Added MingW port for PC.
*   V3.11 30-Jan-06, M Liu      Added FreeBSD port.
*   V3.20 25-Apr-07, D Berger   Changed nCACHE_BUFFER_BYTEs from 512 to 10240.
*   V3.21 04-May-07, D Berger   Added option to define Fif_ENTRYPOINT based on
*                               predefined constants.
*   V3.22 18-Jun-07, D Berger   Added structures in support of metadata 
*                               READONLY performance enhancement.
*   V3.23 10-Jan-09, M Liu      Added maximum sizes for internal records.
*   V3.24 28-Apr-09, M Liu      Added "__PPC__" for Linux on PPC.
*   V3.25 28-Aug-12, M Liu      Changed nCACHE_BUFFER_BYTEs for windows from
*                               512 to 65536 while keeps Unix/Cygwin as 8192.
*
******************************************************************************/

#if !defined(CDFLIBh_INCLUDEd__)
#  define CDFLIBh_INCLUDEd__

/*****************************************************************************
* Include files.
*****************************************************************************/

#if !defined(FORJNI)		/* It is defined to compiler as -D "FORJNI" */
#  define LIBCDF_SOURCE_
#endif
#include "cdfdist.h"

/*****************************************************************************
* V1.x magic number.
*    This is what a V1 magic number looks like when the bytes are flipped.
* The bytes will be flipped on a VAX because the longword was assumed to be
* in network order and then flipped to VAX order.  The bytes will be flipped
* on a Sun, SGi, NeXT, HP, IBM RS, etc. because the longword was assumed to
* already be in network order (the same as those machines) but the byte order
* in memory is reversed from that of a VAX (where all V1 CDFs were created).
*****************************************************************************/

#define V1magicNUMBER_flip      0x0F000000

/*****************************************************************************
* V2.x/V3.x magic numbers.
*****************************************************************************/

#define V2magicNUMBER_1pre      0x0000FFFF   /* Written at offset 0x00000000
						of pre-V2.6 CDFs. */
#define V2magicNUMBER_1         0xCDF26002   /* Written at offset 0x00000000
						of V2.6 CDFs. */
#define V3magicNUMBER_1         0xCDF30001   /* Written at offset 0x00000000
                                                of V3.0 CDFs. */
#define V2magicNUMBER_2u        0x0000FFFF   /* Written at offset 0x00000004
						of uncompressed V2.6 CDFs (as
						well as all pre-V2.6 CDFs). */
#define V3magicNUMBER_2u        0x0000FFFF   /* Written at offset 0x00000004
                                                of uncompressed V3.0 CDFs.  */
#define V2magicNUMBER_2c        0xCCCC0001   /* Written at offset 0x00000004
                                                of compressed V2.6 CDFs. */
#define V3magicNUMBER_2c        0xCCCC0001   /* Written at offset 0x00000004
						of compressed V3.0 CDFs. */
#define VALIDid_MAGIC_NUMBER    0x12345678   /* Placed in `CDF' structure
						(CDFid).  Used to check that
						a valid CDF has been selected
						by an application. */
#define KILLEDid_MAGIC_NUMBER   0x24682468   /* Placed in `CDF' structure
						(CDFid) before freeing the
						structure.  This is done in
						case an application tries to
						select the CDF again (which
						might cause some sort of
						memory violation anyway). */
#define ABORTEDid_MAGIC_NUMBER	0x13571357   /* Placed in `CDF' structure
						(CDFid) when access to that
						CDF has been aborted. */

/*****************************************************************************
* NetCDF file magic number (CDFx).
*****************************************************************************/

#define netCDFmagicNUMBER1      0x43444601   /* Written at offset 0x00000000
                                                of classic netCDF files. */
#define netCDFmagicNUMBER2      0x43444602   /* Written at offset 0x00000000
                                                of 64-bit netCDF files. */

/*****************************************************************************
* Virtual memory (Microsoft C 7.00).
*****************************************************************************/

#if defined(MICROSOFTC_700) && INCLUDEvMEMORY
  typedef _vmhnd_t MemHandle;
# if defined(CDFLIB)
    Logical useVmem = DEFAULT_USEvMEM;
    long vMemSize = DEFAULT_vMEMSIZE;
    uInt4 vMemMask = DEFAULT_vMEMMASK;
# else
    extern Logical useVmem;
    extern long vMemSize;
    extern uInt4 vMemMask;
# endif
#endif

/*****************************************************************************
* Miscellaneous constants.
*****************************************************************************/

#define INT32_ZERO              ((Int32) 0)
#define INT32_ONE               ((Int32) 1)

#define NO_BLOCK                (-1)
#define NO_OFFSET               0xFFFFFFFF
#define NO_OFFSET64             -1
#define ZERO_OFFSET             0x00000000
#define ZERO_OFFSET64           0

#define ALLrecordsATonce        1
#define ONErecordATaTIME        2
#define ONEvalueATaTIME         3

#define EXT_LEN			3	/* Length of file extensions that are
					   created by the CDF library.  This
					   is limited by the 8.3 convention of
					   MS-DOS. */
#define MAX_TMP			99999

#define READ_ONLY		1
#define READ_WRITE		2

#define NUMcacheUNKNOWN		4	/* The number of cache buffers to use
					   for the "working" dotCDF file when
					   the CDF's format is unknown.  This
					   is set large enough to prevent any
					   paging while reading the CDF's
					   format (among other things). */

/*****************************************************************************
* Variable types.
*****************************************************************************/

#define STANDARD_                       1
#define SPARSE_RECORDS_                 2
#define COMPRESSED_                     3
#define SPARSE_COMPRESSED_RECORDS_      4
#define SPARSE_ARRAYS_                  5
#define SPARSE_RECORDS_AND_ARRAYS_      6
#define IN_MULTI_                       7

#define SPARSErecords(Var) \
(Var->vType == SPARSE_RECORDS_ || \
 Var->vType == SPARSE_COMPRESSED_RECORDS_ || \
 Var->vType == SPARSE_RECORDS_AND_ARRAYS_)

#define SPARSEarrays(Var) \
(Var->vType == SPARSE_ARRAYS_ || Var->vType == SPARSE_RECORDS_AND_ARRAYS_)

#define COMPRESSED(Var) \
(Var->vType == COMPRESSED_ || Var->vType == SPARSE_COMPRESSED_RECORDS_)

/*****************************************************************************
* Reserved values.
*****************************************************************************/

#define RESERVED_ATTROFFSET (-1)                /* Indicates that an attribute
						   hasn't been selected yet. */
#define RESERVED_ENTRYNUM   (-1L)               /* Indicates that a g/r/zEntry
						   number hasn't been selected
						   yet. */
#define RESERVED_ENTRYOFFSET (-1)               /* Indicates that the current
						   g/r/zEntry does not exist.*/
#define RESERVED_ATTROFFSET64 (-1)
#define RESERVED_ENTRYOFFSET64 (-1)
#define DUMMY_ATTROFFSET (-2)                   /* Used for CurAttrOffset in 
                                                   READONLYon mode */
#define DUMMY_ENTRYOFFSET (-2)                  /* Used for CurEntryOffset in
                                                   READONLYon mode */
#define RESERVED_VARNUM     (-1L)               /* Indicates that a variable
						   hasn't been selected yet. */

/******************************************************************************
* NETWORKbyteORDERcpu.
*   network (big-endian)..........Sun, SGi, IBM-RS, HP, NeXT, Macintosh,
*				  PowerPC
*   non-network (little-endian)...VAX (VMS, POSIX Shell), DECstation,
*                                 IBM-PC (MS-DOS, Linux, Solaris, Macos X),
*                                 DEC Alpha (OSF/1, OpenVMS, POSIX Shell).
******************************************************************************/

#if defined(sun) || defined(MIPSEB) || defined(IBMRS) || defined(HP) || \
    defined(NeXT) || defined(mac) || defined(PPC) || defined(macosXppc) || \
    defined(__PPC__) || defined(__ppc64__) 
#  define NETWORKbyteORDERcpu
#endif

/******************************************************************************
* XDRishENCODINGcpu.
*   xdr...........Sun, SGi, IBM-RS, HP, NeXT, Macintosh, PowerPC
*   non-xdr.......VAX (VMS, POSIX Shell), DECstation, IBM-PC (MS-DOS, Linux
*                 Solaris, Macos X), DEC Alpha (OSF/1, OpenVMS, POSIX Shell).
******************************************************************************/

#if defined(sun) || defined(MIPSEB) || defined(IBMRS) || defined(HP) || \
    defined(NeXT) || defined(mac) || defined(PPC) || defined(macosXppc) || \
    defined(__PPC__) || defined(__ppc64__) 
#  define XDRishENCODINGcpu
#endif

/******************************************************************************
* Constants for `seeking'.
******************************************************************************/

#define vSEEK_SET       0
#define vSEEK_CUR       1
#define vSEEK_END       2

/*****************************************************************************
* CDF V2.0+ constants.
*****************************************************************************/

#define FIRST_IR_OFFSET                 ((Int32) 0x00000008)
#define V2_MAGIC_OFFSET_1               ((Int32) 0x00000000)
#define V2_MAGIC_OFFSET_2               ((Int32) 0x00000004)
#define V2_CDR_OFFSET                   ((Int32) 0x00000008)
#define V2_CCR_OFFSET                   ((Int32) 0x00000008)

/*****************************************************************************
* CDF V3.0+ constants.
*****************************************************************************/

#define V3_MAGIC_OFFSET_1               ((Int32) 0x00000000)
#define V3_MAGIC_OFFSET_2               ((Int32) 0x00000004)

#define MAGIC_NUMBERS_SIZE              8
#define VDR_WASTED_SIZE                 128
#define VARs_INCREMENT                  20

#define VSTREAM_MAGIC_NUMBER    0x12345678      /* Used to verify that a VFILE
						   structure has been passed
						   to a function. */

#define DEFAULT_nCACHE_BUFFERs  1               /* Default number of buffers
						   in the cache. */
#if defined(win32) && (_FILE_OFFSET_BITS == 64) 
#  define nCACHE_BUFFER_BYTEs     65536	        /* Size (bytes) of each cache
						   buffer. */
#else
#  if defined(linux) || defined(__CYGWIN__)
#    define nCACHE_BUFFER_BYTEs     8192
#  else
#    define nCACHE_BUFFER_BYTEs     10240
#  endif
#endif

#if defined(vms)
#  define VMS_DEFAULT_nALLOCATION_BLOCKS  3       /* Allocate only 3 blocks at
						   a time (override VMS default
						   of 128 for fixed record
						   length file [or 200 when
						   file FTPed to VMS]). */
#endif

#define vMAX_TRYs       5                       /* Maximum number of trys on a
						   read or write operation. */

#define MAX_EPOCH_BINARY                3.15569519999998e14
#define MAX_EPOCH16_1_BINARY            3.15569519999E11
#define MAX_EPOCH16_2_BINARY            9.99999999998E11

#define CTIME_STRING_LEN                25

#define MAX_VXR_ENTRIES                 10      /* Maximum from this and
						   previous CDF releases. */
#define NUM_VXR_ENTRIES                 7       /* For VXRs whose entries
						   point to VVR/CVVRs. */
#define NUM_VXRx_ENTRIES                3       /* For VXRs whose entries
						   point to other VXRs. */

/*****************************************************************************
* CDF versions/releases/increments.
*****************************************************************************/

#define VERSION_1       1
#define VERSION_2       2

#define RELEASE_0       0
#define RELEASE_1       1
#define RELEASE_2       2
#define RELEASE_3       3
#define RELEASE_4       4
#define RELEASE_5       5
#define RELEASE_6       6

#define INCREMENT_1     1

/*****************************************************************************
* File types.
*****************************************************************************/

#define CDFt            1
#define Vt              3
#define Zt              4

/*****************************************************************************
* AbortAccess macros.
*****************************************************************************/

#define UPDATE          TRUE
#define noUPDATE        FALSE
#define CDFDELETE       TRUE
#define noDELETE        FALSE

/*****************************************************************************
* Compression.
*****************************************************************************/

#define EMPTY           TRUE
#define notEMPTY        FALSE

/*****************************************************************************
* Macros for FORTRAN interface(s).
*****************************************************************************/

#if defined(vms) || defined(MPW_C)
#  define Fif_DESCR
#endif

#if (defined(unix) && !defined(Mach)) || defined(SALFORDC) || \
    defined(posixSHELL) || defined(_MSC_VER) 
#  define Fif_GHOSTLEN
#endif

#if (defined(dos) && !defined(SALFORDC)) || defined(Mach)
#  define Fif_NOLEN
#endif

#if defined(double_underscore)
#    define Fif_ENTRYPOINT(ep__,ep_,ep,EP) ep__
#elif defined(single_underscore)
#    define Fif_ENTRYPOINT(ep__,ep_,ep,EP) ep_
#elif defined(no_underscore)
#    define Fif_ENTRYPOINT(ep__,ep_,ep,EP) ep
#elif defined(unix) && !defined(AIX) && !defined(Mach) && !defined(hpux) || \
    defined(__MINGW32__)
#  if (defined(linux) && !defined(__ICC)) || defined(__CYGWIN__) || \
      defined(sun) || defined(__MINGW32__) || defined(__FreeBSD__) || \
      defined(__PPC__)
#    define Fif_ENTRYPOINT(ep__,ep_,ep,EP) ep__
#  else
#    define Fif_ENTRYPOINT(ep__,ep_,ep,EP) ep_
#  endif
#else
#  if defined(SALFORDC)
#    define Fif_ENTRYPOINT(ep__,ep_,ep,EP) EP
#  else
#    define Fif_ENTRYPOINT(ep__,ep_,ep,EP) ep
#  endif
#endif

#if defined(Fif_GHOSTLEN)
#  if defined(SALFORDC)
    typedef int *Fif_GHOSTTYPE;
#    define Fif_GHOSTFETCH(ap) *(va_arg(ap,int *))
#    define Fif_GHOSTUSE(l) *l
#  else
    typedef int Fif_GHOSTTYPE;
#    define Fif_GHOSTFETCH(ap) va_arg(ap,int)
#    define Fif_GHOSTUSE(l) l
#  endif
#  define Fif_GHOSTARG(v) ,v
#  define Fif_GHOSTDEF(v) Fif_GHOSTTYPE v;
#else
#  define Fif_GHOSTARG(v) \

#  define Fif_GHOSTDEF(v) \

#endif

#if defined(MPW_C)
#  define Fif_PREFIXa pascal
#else
#  define Fif_PREFIXa \

#endif

#if defined(MICROSOFTC)
#  if defined(MICROSOFTC_600)
#    define Fif_PREFIXb _cdecl
#  endif
#  if defined(MICROSOFTC_700)
#    define Fif_PREFIXb __cdecl
#  endif
#else
#  define Fif_PREFIXb \

#endif

/******************************************************************************
* Integer orders.
******************************************************************************/

#define LITTLEendianORDER       1
#define BIGendianORDER          2

/******************************************************************************
* Assumed attribute scopes.
******************************************************************************/

#define GLOBALscopeASSUMED      3L
#define VARIABLEscopeASSUMED    4L

/******************************************************************************
* Floating-point types.
******************************************************************************/

#define FP_1                    1
#define FP_2                    2
#define FP_3                    3
#define FP_4                    4

/******************************************************************************
* Floating-point structures.
******************************************************************************/

#if defined(FP1cpu)
  struct fp1struct4 {
    uInt4 s : 1, e1 : 7, e0 : 1, m2 : 7, m1 : 8, m0 : 8;
  };
  struct fp2struct4 {
    uInt4 m0 : 8, m1 : 8, e0 : 1, m2 : 7, s : 1, e1 : 7;
  };
  struct fp34struct4 {
    uInt4 e0 : 1, m2 : 7, s : 1, e1 : 7, m0 : 8, m1 : 8;
  };
  struct fp1struct8 {
    uInt4 s : 1, e1 : 7, e0 : 4, m6 : 4, m5 : 8,
	 m4 : 8, m3 : 8, m2 : 8, m1 : 8, m0 : 8;
  };
  struct fp2struct8 {
    uInt4 m0 : 8, m1 : 8, m2 : 8, m3 : 8, m4 : 8,
	 m5 : 8, e0 : 4, m6 : 4, s : 1, e1 : 7;
  };
  struct fp3struct8 {
    uInt4 e0 : 1, m6 : 7, s : 1, e1 : 7, m4 : 8,
	 m5 : 8, m2 : 8, m3 : 8, m0 : 8, m1 : 8;
  };
  struct fp4struct8 {
    uInt4 e0 : 4, m6 : 4, s : 1, e1 : 7, m4 : 8,
	 m5 : 8, m2 : 8, m3 : 8, m0 : 8, m1 : 8;
  };
#endif

#if defined(FP2cpu)
  struct fp1struct4 {
    uInt4 e1 : 7, s : 1, m2 : 7, e0 : 1, m1 : 8, m0 : 8;
  };
  struct fp2struct4 {
    uInt4 m0 : 8, m1 : 8, m2 : 7, e0 : 1, e1 : 7, s : 1;
  };
  struct fp34struct4 {
    uInt4 m2 : 7, e0 : 1, e1 : 7, s : 1, m0 : 8, m1 : 8;
  };
  struct fp1struct8 {
    uInt4 e1 : 7, s : 1, m6 : 4, e0 : 4, m5 : 8,
	 m4 : 8, m3 : 8, m2 : 8, m1 : 8, m0 : 8;
  };
  struct fp2struct8 {
    uInt4 m0 : 8, m1 : 8, m2 : 8, m3 : 8, m4 : 8,
	 m5 : 8, m6 : 4, e0 : 4, e1 : 7, s : 1;
  };
  struct fp3struct8 {
    uInt4 m6 : 7, e0 : 1, e1 : 7, s : 1, m4 : 8,
	 m5 : 8, m2 : 8, m3 : 8, m0 : 8, m1 : 8;
  };
  struct fp4struct8 {
    uInt4 m6 : 4, e0 : 4, e1 : 7, s : 1, m4 : 8,
	 m5 : 8, m2 : 8, m3 : 8, m0 : 8, m1 : 8;
  };
#endif

#if defined(FP3cpu)
  struct fp1struct4 {
    uInt4 e1 : 7, s : 1, m2 : 7, e0 : 1, m1 : 8, m0 : 8;
  };
  struct fp2struct4 {
    uInt4 m0 : 8, m1 : 8, m2 : 7, e0 : 1, e1 : 7, s : 1;
  };
  struct fp34struct4 {
    uInt4 m2 : 7, e0 : 1, e1 : 7, s : 1, m0 : 8, m1 : 8;
  };
  struct fp1struct8 {
    uInt4 e1 : 7, s : 1, m6 : 4, e0 : 4, m5 : 8,
	 m4 : 8, m3 : 8, m2 : 8, m1 : 8, m0 : 8;
  };
  struct fp2struct8 {
    uInt4 m0 : 8, m1 : 8, m2 : 8, m3 : 8, m4 : 8,
	 m5 : 8, m6 : 4, e0 : 4, e1 : 7, s : 1;
  };
  struct fp3struct8 {
    uInt4 m6 : 7, e0 : 1, e1 : 7, s : 1, m4 : 8,
	 m5 : 8, m2 : 8, m3 : 8, m0 : 8, m1 : 8;
  };
  struct fp4struct8 {
    uInt4 m6 : 4, e0 : 4, e1 : 7, s : 1, m4 : 8,
	 m5 : 8, m2 : 8, m3 : 8, m0 : 8, m1 : 8;
  };
#endif

#if defined(FP4cpu)
  struct fp1struct4 {
    uInt4 e1 : 7, s : 1, m2 : 7, e0 : 1, m1 : 8, m0 : 8;
  };
  struct fp2struct4 {
    uInt4 m0 : 8, m1 : 8, m2 : 7, e0 : 1, e1 : 7, s : 1;
  };
  struct fp34struct4 {
    uInt4 m2 : 7, e0 : 1, e1 : 7, s : 1, m0 : 8, m1 : 8;
  };
  struct fp1struct8 {
    uInt4 e1 : 7, s : 1, m6 : 4, e0 : 4, m5 : 8,
	 m4 : 8, m3 : 8, m2 : 8, m1 : 8, m0 : 8;
  };
  struct fp2struct8 {
    uInt4 m0 : 8, m1 : 8, m2 : 8, m3 : 8, m4 : 8,
	 m5 : 8, m6 : 4, e0 : 4, e1 : 7, s : 1;
  };
  struct fp3struct8 {
    uInt4 m6 : 7, e0 : 1, e1 : 7, s : 1, m4 : 8,
	 m5 : 8, m2 : 8, m3 : 8, m0 : 8, m1 : 8;
  };
  struct fp4struct8 {
    uInt4 m6 : 4, e0 : 4, e1 : 7, s : 1, m4 : 8,
	 m5 : 8, m2 : 8, m3 : 8, m0 : 8, m1 : 8;
  };
#endif

/******************************************************************************
* REVERSE2b.
******************************************************************************/

#define REVERSE2b(buf) { \
Byte1 *_buf_ = (Byte1 *) (buf), _temp_; \
_temp_ = _buf_[0]; \
_buf_[0] = _buf_[1]; \
_buf_[1] = _temp_; \
}

/******************************************************************************
* REVERSE4b.
******************************************************************************/

#define REVERSE4b(buf) { \
Byte1 *_buf_ = (Byte1 *) (buf), _temp_; \
_temp_ = _buf_[0]; \
_buf_[0] = _buf_[3]; \
_buf_[3] = _temp_; \
_temp_ = _buf_[1]; \
_buf_[1] = _buf_[2]; \
_buf_[2] = _temp_; \
}

/******************************************************************************
* REVERSE8b.
******************************************************************************/

#define REVERSE8b(buf) { \
Byte1 *_buf_ = (Byte1 *) (buf), _temp_; \
_temp_ = _buf_[0]; \
_buf_[0] = _buf_[7]; \
_buf_[7] = _temp_; \
_temp_ = _buf_[1]; \
_buf_[1] = _buf_[6]; \
_buf_[6] = _temp_; \
_temp_ = _buf_[2]; \
_buf_[2] = _buf_[5]; \
_buf_[5] = _temp_; \
_temp_ = _buf_[3]; \
_buf_[3] = _buf_[4]; \
_buf_[4] = _temp_; \
}

/******************************************************************************
* REVERSE2bIO.
******************************************************************************/

#define REVERSE2bIO(iPtr,oPtr) \
((Byte1 *) (oPtr))[0] = ((Byte1 *) (iPtr))[1]; \
((Byte1 *) (oPtr))[1] = ((Byte1 *) (iPtr))[0];

/******************************************************************************
* REVERSE4bIO.
******************************************************************************/

#define REVERSE4bIO(iPtr,oPtr) \
((Byte1 *) (oPtr))[0] = ((Byte1 *) (iPtr))[3]; \
((Byte1 *) (oPtr))[1] = ((Byte1 *) (iPtr))[2]; \
((Byte1 *) (oPtr))[2] = ((Byte1 *) (iPtr))[1]; \
((Byte1 *) (oPtr))[3] = ((Byte1 *) (iPtr))[0];

/******************************************************************************
* REVERSE8bIO.
******************************************************************************/

#define REVERSE8bIO(iPtr,oPtr) \
((Byte1 *) (oPtr))[0] = ((Byte1 *) (iPtr))[7]; \
((Byte1 *) (oPtr))[1] = ((Byte1 *) (iPtr))[6]; \
((Byte1 *) (oPtr))[2] = ((Byte1 *) (iPtr))[5]; \
((Byte1 *) (oPtr))[3] = ((Byte1 *) (iPtr))[4]; \
((Byte1 *) (oPtr))[4] = ((Byte1 *) (iPtr))[3]; \
((Byte1 *) (oPtr))[5] = ((Byte1 *) (iPtr))[2]; \
((Byte1 *) (oPtr))[6] = ((Byte1 *) (iPtr))[1]; \
((Byte1 *) (oPtr))[7] = ((Byte1 *) (iPtr))[0];

/******************************************************************************
* LoadCVVRx/LoadVVRx.
******************************************************************************/

#define LoadCVVRx(cvvr,recordsize,csize) \
cvvr.RecordSize = (Int32) (recordsize); \
cvvr.RecordType = CVVR_; \
cvvr.rfuA = (Int32) 0; \
cvvr.cSize = (Int32) (csize);

#define LoadVVRx(vvr,recordsize) \
vvr.RecordSize = (Int32) (recordsize); \
vvr.RecordType = VVR_;

/******************************************************************************
* LoadAllocCVVR/LoadAllocVVR.
******************************************************************************/

#define LoadAllocCVVR(alloc,firstrec,lastrec,csize,xsize) \
alloc.first = (Int32) (firstrec); \
alloc.last = (Int32) (lastrec); \
alloc.type = (int) CVVR_; \
alloc.cvvr.cSize = (Int32) (csize); \
alloc.cvvr.xSize = (Int32) (xsize);

#define LoadAllocVVR(alloc,firstrec,lastrec,newvvr) \
alloc.first = (Int32) (firstrec); \
alloc.last = (Int32) (lastrec); \
alloc.type = (int) VVR_; \
alloc.vvr.newX = (Logical) (newvvr);

/******************************************************************************
* GLOBALscope/VARIABLEscope/DEFINITEscope.
******************************************************************************/

#define GLOBALscope(scope) \
(scope == GLOBAL_SCOPE || scope == GLOBALscopeASSUMED)

#define VARIABLEscope(scope) \
(scope == VARIABLE_SCOPE || scope == VARIABLEscopeASSUMED)

#define DEFINITEscope(scope) \
(BOO(scope == GLOBALscopeASSUMED, \
     GLOBAL_SCOPE,BOO(scope == VARIABLEscopeASSUMED,VARIABLE_SCOPE,scope)))

/******************************************************************************
* FLOAT8dataType.
*    Macro to check for a double-precision floating-point data type.
******************************************************************************/

#define FLOAT8dataType(dataType) \
(dataType == CDF_REAL8 || dataType == CDF_DOUBLE || dataType == CDF_EPOCH)

/******************************************************************************
* FLOAT16dataType.
*    Macro to check for a (sudo) 16-byte (2 8-byte) floating-point data type.
******************************************************************************/

#define FLOAT16dataType(dataType) \
(dataType == CDF_EPOCH16)

/******************************************************************************
* Operating system dependent definitions.
******************************************************************************/

#if defined(vms) || defined(dos) || defined(mac) || defined(win32) || \
    defined(__CYGWIN__) || defined(__MINGW32__)
#  define READ_ONLY_a_mode        "rb"
#  define READ_PLUS_a_mode        "r+b"
#  define WRITE_PLUS_a_mode       "w+b"
#else
#  define READ_ONLY_a_mode        "r"
#  define READ_PLUS_a_mode        "r+"
#  define WRITE_PLUS_a_mode       "w+"
#endif

#if defined(win32)
#define READ_ONLY_a_mode_windows_LFS  _O_RDONLY | _O_BINARY
#define READ_PLUS_a_mode_windows_LFS  _O_RDWR | _O_BINARY
#define WRITE_PLUS_a_mode_windows_LFS _O_RDWR | _O_CREAT | _O_TRUNC | _O_BINARY
#define PMODE _S_IWRITE
#endif

/******************************************************************************
* Miscellaneous macros.
******************************************************************************/

#define SEEKv(fp,offset,origin) (V_seek(fp,offset,origin) == 0)
#define READv(buffer,size,number,fp) (V_read(buffer,size,number,fp) == number)
#define WRITEv(buffer,size,number,fp) (V_write(buffer,size,number,fp)==number)
#define CACHEv(fp,nBuffers) (V_setcache(fp,nBuffers) == 0)
#define CLEARv(fp) (V_clear(fp) == 0)
#define DELETEv(fp,vStats) (V_delete(fp,vStats) == 0)
#define CLOSEv(fp,CDF,vStats) (V_close(fp,CDF,vStats) == 0)
#define FLUSHv(fp) (V_flush(fp) == 0)

#define MEMMOVE(dst,src,nBytes) if (dst != src) memmove (dst, src, nBytes);

#define DECODE(function,buffer,numElems) \
BOO(function == NULL,CDF_OK,function(buffer,numElems))

/******************************************************************************
* CURRENTattrSELECTED.
******************************************************************************/

#define CURRENTattrSELECTED(CDF) (CDF->CURattrOffset != RESERVED_ATTROFFSET)
#define CURRENTattrSELECTED64(CDF) (CDF->CURattrOffset64 != RESERVED_ATTROFFSET64)

/******************************************************************************
* CURRENTvarSELECTED.
******************************************************************************/

#define CURRENTvarSELECTED(CDF,zOp) \
BOO(zModeON(CDF), \
    CDF->CURzVarNum != RESERVED_VARNUM, \
    BOO(zOp,CDF->CURzVarNum,CDF->CURrVarNum) != RESERVED_VARNUM)

/******************************************************************************
* BADzOP.
*    Check if an operation is legal while in zMode.
******************************************************************************/

#define BADzOP(CDF,rVar) (zModeON(CDF) && rVar)

/******************************************************************************
* EXISTSisBAD.
******************************************************************************/

#define EXISTSisBAD(alloc) BOO((alloc)->type == VVR_,(alloc)->vvr.newX,TRUE)

/******************************************************************************
* Allocation structures.
******************************************************************************/

struct AllocStruct {
  Int32 first;          /* First record to allocate. */
  Int32 last;           /* Last record to allocate. */
  int type;             /* Type of record(s): VVR_ or CVVR_. */
  struct {
    Logical newX;       /* TRUE: Don't append to an existing VVR (even if
				 possible).  CDF_INTERNAL_ERROR if any of
				 the records already exist. */
  } vvr;
  struct {
    Int32 cSize;        /* Size of CVVR compressed data. */
    Int32 xSize;        /* Size of CVVR extra space. */
  } cvvr;
  struct {
    OFF_T cSize;        /* Size of CVVR compressed data. */
    OFF_T xSize;        /* Size of CVVR extra space. */
  } cvvr64;
};

/******************************************************************************
* vFILE/vCACHE structures.
******************************************************************************/

typedef struct vCACHEstruct {
  long blockN;		/* File block number in this cache buffer. */
  struct vCACHEstruct *next;
			/* Index of next cache buffer (on linked list). */
  struct vCACHEstruct *prev;
			/* Index of previous cache buffer (on linked list). */
  Logical modified;     /* TRUE if this file block has been modified. */
  void *ptr;            /* Pointer to cache buffer or virtual memory handle
			   (if Microsoft C 7.00). */
} vCACHE;

typedef struct vFILEstruct {
  uInt32 magic_number;  /* Magic number for VFILE structure. */
  FILE *fp;             /* File pointer.  Initially set to NULL if a scratch
			   file (because the scratch file hasn't been created
			   yet).  If not a scratch file, set to the file
			   pointer of the created/opened file. */
  char *path;		/* If a scratch file, initially set to the scratch
			   directory or a NUL-string if the current directory
			   is to be used.  This is set to the pathname of the
			   file if this is not a scratch file or if a scratch
			   file for which the file has been created (because a
			   block had to be paged out). */
  char scratchExt[EXT_LEN+1];
			/* Extension for this scratch file.  */
  Logical scratch;	/* Set TRUE if a scratch file. */
  Logical error;        /* TRUE if the error indicator is set.  No further
			   access to the file is allowed. */
  Logical eof;		/* TRUE if the EOF indicator is set (after a read is
			   attempted at the EOF). */
  vCACHE *cacheHead;    /* Head of linked list of cache structures. */
  vCACHE *cacheTail;    /* Tail of linked list of cache structures. */
  int maxBuffers;       /* Maximum number of cache buffers to be used. */
  int nBuffers;         /* Actual number of cache buffers being used. */
  long nV_reads;        /* Number of calls to `V_read'. */
  long nV_writes;       /* Number of calls to `V_write'. */
  long nBlockReads;     /* Number of file blocks which have been read. */
  long nBlockWrites;    /* Number of file blocks which have been written. */
  long nPageIns;        /* Number of blocks paged in from the file. */
  long nPageOuts;       /* Number of blocks paged out to the file. */
  long length;          /* Length in bytes of the file.  This includes what is
			   in the cache but not necessarily what is physically
			   in the file yet) using 32-bit offset. */
  OFF_T length64;       /* Length in bytes of the file.  This includes what is
                           in the cache but not necessarily what is physically
                           in the file yet) using 64-bit offset. */
  long phyLength;       /* Physical length of the file in bytes (ignoring what
			   might be in the cache) using 32-bit offset. */
  OFF_T phyLength64;    /* Physical length of the file in bytes (ignoring what
                           might be in the cache) using 64-bit offset. */
  long offset;          /* Read/write position (byte offset) in file
			   using 32-bit offset. */
  OFF_T offset64;       /* Read/write position (byte offset) in file
			   using 64-bit offset. */
  int   fh;             /* File handler. Used for Windows in lower-level
                           file i/o in LFS. */
  struct GDRstruct *GDR;
                        /* GDR data used in READONLYon mode */
  struct GDRstruct64 *GDR64;
                        /* GDR64 data used in READONLYon mode */
  struct ADRstruct **ADRList; 
                        /* List of ADRs used in READONLYon mode. Indexed by
                           attribute number. */
  struct ADRstruct64 **ADRList64;
                        /* List of ADR64s used in READONLYon mode. Indexed by
                           attribute number. */
  int CurADRIndex;      /* Index into ADRList/ADRList64 of currently selected
                           attribute. Valid only in READONLYon mode. */
  int CurAEDRIndex;     /* Index into AEDRList/AEDRList64 of currently selected
                           attribute entry. Valid only in READONLYon mode. */
  Logical CURzEntrySel; /* If the CurAEDRIndex refers to a zENRTY or a 
                           grENTRY. Valid only in READONLYon mode. */
} vFILE;

/******************************************************************************
* CDR flag bit positions (bit 0 is Least Significant Bit).
******************************************************************************/

#define CDR_MAJORITY_BIT        0       /* majority: set = row major,
						     clear = column major */
#define CDR_FORMAT_BIT          1       /* format: set = single file,
						   clear = multi file */
#define CDR_CHECKSUM_BIT        2	/* Check sum bit. */
#define CDR_CHECKSUM_MD5_BIT    3       /* Check sum bit for MD5 method. */
#define CDR_CHECKSUM_OTHER_BIT  4       /* Check sum bit for another method. */

/******************************************************************************
* VDR flag bit positions (bit 0 is Least Significant Bit).  These apply to
* both rVDRs and zVDRs.  Note that the compression and sparse arrays bits
* cannot both be set.
******************************************************************************/

#define VDR_RECVARY_BIT         0       /* Record variance...
					     set = VARY,
					     clear = NOVARY. */
#define VDR_PADVALUE_BIT        1       /* Pad value...
					     set = specified,
					     clear = not specified. */
#define VDR_COMPRESSION_BIT     2       /* Variable compression...
					     set = compressed, CPR offset
						   in CPRorSPRoffset field.
					     clear = uncompressed. */
#define VDR_SPARSEARRAYS_BIT    3       /* Sparse arrays...
					     set = sparse, SPR offset
						   in CPRorSPRoffset field.
					     clear = unsparse. */

/******************************************************************************
* Internal record types.
******************************************************************************/

#define UIR_    ((Int32) (-1))  /* Unused Internal Record. */
#define CDR_    ((Int32) 1)     /* CDF Descriptor Record. */
#define GDR_    ((Int32) 2)     /* Global Descriptor Record. */
#define rVDR_   ((Int32) 3)     /* rVariable Descriptor Record. */
#define ADR_    ((Int32) 4)     /* Attribute Descriptor Record. */
#define AgrEDR_ ((Int32) 5)     /* Attribute g/rEntry Descriptor Record. */
#define VXR_    ((Int32) 6)     /* Variable indeX Record. */
#define VVR_    ((Int32) 7)     /* Variable Values Record. */
#define zVDR_   ((Int32) 8)     /* zVariable Descriptor Record. */
#define AzEDR_  ((Int32) 9)     /* Attribute zEntry Descriptor Record. */
#define CCR_    ((Int32) 10)    /* Compressed CDF Record. */
#define CPR_    ((Int32) 11)    /* Compression Parameters Record. */
#define SPR_    ((Int32) 12)    /* Sparseness Parameters Record. */
#define CVVR_   ((Int32) 13)    /* Compressed Variable Values Record. */

/******************************************************************************
* Base record sizes (bytes).
******************************************************************************/

#define UUIR_BASE_SIZE          8
#define UIR_BASE_SIZE           16
#define CDR_BASE_SIZE           48
#define GDR_BASE_SIZE           60
#define zVDR_BASE_SIZE          (68 + CDF_VAR_NAME_LEN)
#define rVDR_BASE_SIZE          (64 + CDF_VAR_NAME_LEN)
#define VXR_BASE_SIZE           (20 + (12 * NUM_VXR_ENTRIES))
#define VXRx_BASE_SIZE          (20 + (12 * NUM_VXRx_ENTRIES))
#define VVR_BASE_SIZE           8
#define ADR_BASE_SIZE           (52 + CDF_ATTR_NAME_LEN)
#define AEDR_BASE_SIZE          48
#define CCR_BASE_SIZE           20
#define CPR_BASE_SIZE           20
#define SPR_BASE_SIZE           20
#define CVVR_BASE_SIZE          16

/******************************************************************************
* Max record sizes (bytes).
******************************************************************************/

#define CDR_MAX_SIZE            (48 + CDF_COPYRIGHT_LEN)
#define GDR_MAX_SIZE            (60 + 4 * CDF_MAX_DIMS)
#define zVDR_MAX_SIZE           (68 + CDF_VAR_NAME_LEN + 8 * CDF_MAX_DIMS)
#define rVDR_MAX_SIZE           (64 + CDF_VAR_NAME_LEN + 4 * CDF_MAX_DIMS)
#define VXR_MAX_SIZE            (20 + (16 * MAX_VXR_ENTRIES))
#define VXRx_MAX_SIZE           (20 + (16 * MAX_VXRx_ENTRIES))
#define ADR_MAX_SIZE            (52 + CDF_ATTR_NAME_LEN)
#define AEDR_MAX_SIZE           48
#define CPR_MAX_SIZE            28
#define SPR_MAX_SIZE            28

/******************************************************************************
* CCR structure/fields/offsets - Compressed CDF Record.
******************************************************************************/

struct CCRstruct {
  Int32 RecordSize;     /* Size of record (bytes) */
  Int32 RecordType;     /* Type of record (ie. CCR) */
  Int32 CPRoffset;      /* File offset to CPR (bytes). */
  Int32 uSize;          /* Size of uncompressed CDF's IRs (bytes).  This byte
			   count does NOT include the magic numbers. */
  Int32 rfuA;           /* Reserved for future use.  Set to zero. */
};

#define CCR_NULL                (-1)
#define CCR_RECORD              0
#define CCR_RECORDSIZE          1
#define CCR_RECORDTYPE          2
#define CCR_CPROFFSET           3
#define CCR_USIZE               4
#define CCR_RFUa                5

#define CCR_RECORDSIZE_OFFSET   0
#define CCR_RECORDTYPE_OFFSET   4
#define CCR_CPROFFSET_OFFSET    8
#define CCR_USIZE_OFFSET        12
#define CCR_RFUa_OFFSET         16

/******************************************************************************
* CDR structure/fields/offsets - CDF Descriptor Record.
******************************************************************************/

struct CDRstruct {
  Int32 RecordSize;     /* Size of record (bytes) */
  Int32 RecordType;     /* Type of record (ie. CDR) */
  Int32 GDRoffset;      /* File offset to GDR (bytes) */
  Int32 Version;        /* CDF Version Number */
  Int32 Release;        /* CDF Release Number */
  Int32 Encoding;       /* Encoding of CDF File */
  Int32 Flags;          /* Flags (see bit definitions). */
  Int32 rfuA;           /* reserved for future use (value = 0) */
  Int32 rfuB;           /* reserved for future use (value = 0) */
  Int32 Increment;      /* CDF Increment Number (Vversion.release.increment),
			   always 0 for V2.0 CDFs */
  Int32 rfuD;           /* reserved for future use (value = -1) */
  Int32 rfuE;           /* reserved for future use (value = -1) */
};

#define CDR_NULL                (-1)
#define CDR_RECORD              0
#define CDR_RECORDSIZE          1
#define CDR_RECORDTYPE          2
#define CDR_GDROFFSET           3
#define CDR_VERSION             4
#define CDR_RELEASE             5
#define CDR_ENCODING            6
#define CDR_FLAGS               7
#define CDR_INCREMENT           8
#define CDR_COPYRIGHT           9

#define CDR_RECORDSIZE_OFFSET   0
#define CDR_RECORDTYPE_OFFSET   4
#define CDR_GDROFFSET_OFFSET    8
#define CDR_VERSION_OFFSET      12
#define CDR_RELEASE_OFFSET      16
#define CDR_ENCODING_OFFSET     20
#define CDR_FLAGS_OFFSET        24
#define CDR_RFUa_OFFSET         28
#define CDR_RFUb_OFFSET         32
#define CDR_INCREMENT_OFFSET    36
#define CDR_RFUd_OFFSET         40
#define CDR_RFUe_OFFSET         44
#define CDR_COPYRIGHT_OFFSET    48

/******************************************************************************
* GDR structure/fields/offsets - Global Descriptor Record.
*
* Note(s):
*   The Eof field was not maintained for V2.0 CDFs (it will contain a random
* value).  When a V2.0 CDF is modified, the end-of-file will be determined and
* this field will be updated.
*   Even though CDF_MAX_DIMS elements are allocated for the `rDimSizes'
* array, only those sizes which actually exist are written to the CDF file.
*
******************************************************************************/

struct GDRstruct {
  Int32 RecordSize;     /* Size of current record (bytes) */
  Int32 RecordType;     /* Type of record (ie. GDR) */
  Int32 rVDRhead;       /* File offset to first rVDR (bytes) */
  Int32 zVDRhead;       /* CDF V2.0, value is undefined (random).
			   CDF V2.1, value = 0.
			   CDF V2.2, File offset to first zVDR (bytes). */
  Int32 ADRhead;        /* File offset to first ADR (bytes) */
  Int32 eof;            /* If V2.0 CDF, value is undefined.
			   If V2.1+ CDF, end-of-file (byte offset). */
  Int32 NrVars;         /* Number of rVariables */
  Int32 NumAttr;        /* Number of Attributes */
  Int32 rMaxRec;        /* Maximum rVariable record number written to. */
  Int32 rNumDims;       /* Number of CDF dimensions (for rVariables). */
  Int32 NzVars;         /* Number of zVariables. */
  Int32 UIRhead;        /* File offset to first UIR (bytes). */
  Int32 rfuC;           /* reserved for future use (value = 0) */
  Int32 rfuD;           /* reserved for future use (value = -1) */
  Int32 rfuE;           /* reserved for future use (value = -1) */
  Int32 rDimSizes[CDF_MAX_DIMS];
			/* Size of each dimension (for rVariables). */
};

#define GDR_NULL                (-1)
#define GDR_RECORD              0
#define GDR_RECORDSIZE          1
#define GDR_RECORDTYPE          2
#define GDR_rVDRHEAD            3
#define GDR_zVDRHEAD            4
#define GDR_ADRHEAD             5
#define GDR_EOF                 6
#define GDR_NrVARS              7
#define GDR_NUMATTR             8
#define GDR_rMAXREC             9
#define GDR_rNUMDIMS            10
#define GDR_NzVARS              11
#define GDR_UIRHEAD             12
#define GDR_rDIMSIZES           13

#define GDR_RECORDSIZE_OFFSET   0
#define GDR_RECORDTYPE_OFFSET   4
#define GDR_rVDRHEAD_OFFSET     8
#define GDR_zVDRHEAD_OFFSET     12
#define GDR_ADRHEAD_OFFSET      16
#define GDR_EOF_OFFSET          20
#define GDR_NrVARS_OFFSET       24
#define GDR_NUMATTR_OFFSET      28
#define GDR_rMAXREC_OFFSET      32
#define GDR_rNUMDIMS_OFFSET     36
#define GDR_NzVARS_OFFSET       40
#define GDR_UIRHEAD_OFFSET      44
#define GDR_RFUc_OFFSET         48
#define GDR_RFUd_OFFSET         52
#define GDR_RFUe_OFFSET         56
#define GDR_rDIMSIZES_OFFSET    60

/******************************************************************************
* rVDR/zVDR structure/fields/offsets - r/zVariable Descriptor Record.
******************************************************************************/

struct VDRstruct {
  Int32 RecordSize;     /* Size of current record (bytes) */
  Int32 RecordType;     /* Type of record (eg. rVDR/zVDR) */
  Int32 VDRnext;        /* File offset to next rVDR/zVDR (bytes).  Note in
			   V2.0 CDFs this field was not set properly for the
			   last variable (rVariable). */
  Int32 DataType;       /* Data type of variable */
  Int32 MaxRec;         /* Maximum record number for variable */
  Int32 VXRhead;        /* File offset to first VXR (bytes).  Value will be
			   0 if MULTI_FILE format */
  Int32 VXRtail;        /* File offset to last VXR (bytes).  Value will be 0
			   if MULTI_FILE format */
  Int32 Flags;          /* Flags (see bit definitions). */
  Int32 sRecords;       /* Type of sparse records.  NO_SPARSERECORDS,
			   PAD_SPARSERECORDS, or PREV_SPARSERECORDS. */
  Int32 rfuB;           /* reserved for future use (value = 0) */
  Int32 rfuC;           /* reserved for future use (value = -1) */
  Int32 rfuF;           /* reserved for future use (value = -1) */
  Int32 NumElems;       /* Number of elements of DataType (should be 1 if not
			   CDF_CHAR or CDF_UCHAR) */
  Int32 Num;            /* Variable number */
  Int32 CPRorSPRoffset; /* CPR/SPR offset depending on bits set in `Flags'.
			   If neither compression nor sparse arrays, set to
			   0xFFFFFFFF. */
  Int32 blockingFactor; /* If uncompressed/unsparse:
			     Number of records to extend when new allocations
			     are necessary (if 0, use default).  For multi-file
			     CDFs, extensions are always one record at a time.
			     For NRV variables this value is N/A (only one
			     record is ever written).
			   If compressed/sparse:
			     Number of (or maximum) records per block... */
  char  Name[CDF_VAR_NAME_LEN+1];
			/* Variable name */
  Int32 zNumDims;       /* Number of dimensions for zVariable.  N/A if an
			   rVariable. */
  Int32 zDimSizes[CDF_MAX_DIMS];
			/* Dimension sizes for zVariable.  N/A if an
			   rVariable. */
  Int32 DimVarys[CDF_MAX_DIMS];
			/* Dimension variances. */
			/* Optional... PadValue with the given DataType. */
};

#define VDR_NULL                (-1)
#define VDR_RECORD              0
#define VDR_RECORDSIZE          1
#define VDR_RECORDTYPE          2
#define VDR_VDRNEXT             3
#define VDR_DATATYPE            4
#define VDR_MAXREC              5
#define VDR_VXRHEAD             6
#define VDR_VXRTAIL             7
#define VDR_FLAGS               8
#define VDR_sRECORDS            9
#define VDR_NUMELEMS            10
#define VDR_NUM                 11
#define VDR_CPRorSPR            12
#define VDR_BLOCKING            13
#define VDR_NAME                14
#define VDR_zNUMDIMS            15
#define VDR_zDIMSIZES           16
#define VDR_DIMVARYS            17
#define VDR_PADVALUE            18

#define VDR_RECORDSIZE_OFFSET   0
#define VDR_RECORDTYPE_OFFSET   4
#define VDR_VDRNEXT_OFFSET      8
#define VDR_DATATYPE_OFFSET     12
#define VDR_MAXREC_OFFSET       16
#define VDR_VXRHEAD_OFFSET      20
#define VDR_VXRTAIL_OFFSET      24
#define VDR_FLAGS_OFFSET        28
#define VDR_sRECORDS_OFFSET     32
#define VDR_RFUb_OFFSET         36
#define VDR_RFUc_OFFSET         40
#define VDR_RFUf_OFFSET         44
#define VDR_NUMELEMS_OFFSET     48
#define VDR_NUM_OFFSET          52
#define VDR_CPRorSPR_OFFSET     56
#define VDR_BLOCKING_OFFSET     60
#define VDR_NAME_OFFSET         64

#define rVDR_DIMVARYS_OFFSET    128
#define rVDR_PADVALUE_OFFSETb   128     /* Offset base (more will be added). */

#define zVDR_zNUMDIMS_OFFSET    128
#define zVDR_zDIMSIZES_OFFSET   132
#define zVDR_DIMVARYS_OFFSETb   132     /* Offset base (more will be added). */
#define zVDR_PADVALUE_OFFSETb   132     /* Offset base (more will be added). */

#define VDR_WASTED_OFFSET       48

/******************************************************************************
* VXR structure/fields/offsets - Variable Index Record.
******************************************************************************/

struct VXRstruct {
  Int32 RecordSize;     /* Size of current record (bytes) */
  Int32 RecordType;     /* Type of record (ie. VXR) */
  Int32 VXRnext;        /* File offset to next VXR (bytes) */
  Int32 Nentries;       /* Number of index entries (entries not being used
			   contain -1 in each field) */
  Int32 NusedEntries;   /* Number of index entries actually used */
  Int32 First[MAX_VXR_ENTRIES];
			/* First record number in VVR */
  Int32 Last[MAX_VXR_ENTRIES];
			/* Last record number in VVR */
  Int32 Offset[MAX_VXR_ENTRIES];
			/* File offset to VXR/VVR (bytes) */
};

#define VXR_NULL                (-1)
#define VXR_RECORD              0
#define VXR_RECORDSIZE          1
#define VXR_RECORDTYPE          2
#define VXR_VXRNEXT             3
#define VXR_NENTRIES            4
#define VXR_NUSEDENTRIES        5
#define VXR_FIRSTREC            6
#define VXR_LASTREC             7
#define VXR_OFFSET              8

#define VXR_RECORDSIZE_OFFSET   0
#define VXR_RECORDTYPE_OFFSET   4
#define VXR_VXRNEXT_OFFSET      8
#define VXR_NENTRIES_OFFSET     12
#define VXR_NUSEDENTRIES_OFFSET 16
#define VXR_FIRSTREC_OFFSET     20

/******************************************************************************
* VVR structure/fields/offsets - Variable Values Record.
******************************************************************************/

struct VVRstruct {
  Int32 RecordSize;     /* Size of current record (bytes). */
  Int32 RecordType;     /* Type of record (ie. VVR). */
			/* Bytes... */
};

#define VVR_NULL                (-1)
#define VVR_RECORDx             0
#define VVR_RECORDSIZE          1
#define VVR_RECORDTYPE          2

#define VVR_RECORDSIZE_OFFSET   0
#define VVR_RECORDTYPE_OFFSET   4

/******************************************************************************
* ADR structure/fields/offsets - Attribute Descriptor Record.
******************************************************************************/

struct ADRstruct {
  Int32 RecordSize;     /* Size of current record (bytes). */
  Int32 RecordType;     /* Type of record (ie. ADR). */
  Int32 ADRnext;        /* File offset to next ADR (bytes).  Note that in
			   V2.0 CDFs this field was not set properly for the
			   last attribute. */
  Int32 AgrEDRhead;     /* File offset to first AgrEDR (bytes). */
  Int32 Scope;          /* Variable or global. */
  Int32 Num;            /* Attribute id number. */
  Int32 NgrEntries;     /* Number of attribute gEntries/rEntries. */
  Int32 MAXgrEntry;     /* Maximum attribute gEntry/rEntry number. */
  Int32 rfuA;           /* reserved for future use (value = 0). */
  Int32 AzEDRhead;      /* File offset to first AzEDR (bytes). */
  Int32 NzEntries;      /* Number of attribute zEntries. */
  Int32 MAXzEntry;      /* Maximum attribute zEntry number. */
  Int32 rfuE;           /* Reserved for future use (value = -1). */
  char  Name[CDF_ATTR_NAME_LEN+1];
			/* Attribute name. */
  struct AEDRstructExt **grAEDRList;
                        /* List of grENTRYs related to an attribute. The list
                           is an array 0-MAXgrEntry, indexed by the entry
                           number. When there is no entry for the attribute, 
                           the corresponding list item is NULL */ 
  struct AEDRstructExt **zAEDRList;
                        /* List of zENTRYs related to an attribute. The list
                           is an array 0-MAXgrEntry, indexed by the entry
                           number. When there is no entry for the attribute,
                           the corresponding list item is NULL */
};

#define ADR_NULL                (-1)
#define ADR_RECORD              0
#define ADR_RECORDSIZE          1
#define ADR_RECORDTYPE          2
#define ADR_ADRNEXT             3
#define ADR_AgrEDRHEAD          4
#define ADR_SCOPE               5
#define ADR_NUM                 6
#define ADR_NgrENTRIES          7
#define ADR_MAXgrENTRY          8
#define ADR_AzEDRHEAD           9
#define ADR_NzENTRIES           10
#define ADR_MAXzENTRY           11
#define ADR_NAME                12

#define ADR_RECORDSIZE_OFFSET   0
#define ADR_RECORDTYPE_OFFSET   4
#define ADR_ADRNEXT_OFFSET      8
#define ADR_AgrEDRHEAD_OFFSET   12
#define ADR_SCOPE_OFFSET        16
#define ADR_NUM_OFFSET          20
#define ADR_NgrENTRIES_OFFSET   24
#define ADR_MAXgrENTRY_OFFSET   28
#define ADR_RFUa_OFFSET         32
#define ADR_AzEDRHEAD_OFFSET    36
#define ADR_NzENTRIES_OFFSET    40
#define ADR_MAXzENTRY_OFFSET    44
#define ADR_RFUe_OFFSET         48
#define ADR_NAME_OFFSET         52

/******************************************************************************
* AgrEDR/AzEDR structure/fields/offsets - Attribute g/r/zEntry Descriptor
* Record.
******************************************************************************/

struct AEDRstruct {
  Int32 RecordSize;     /* Size of current record (bytes) */
  Int32 RecordType;     /* Type of record (ie. AgrEDR/AzEDR) */
  Int32 AEDRnext;       /* File offset to next AgrEDR/AzEDR.  Note that in
			   V2.0 CDFs this field was not set properly for the
			   last entry (rEntry). */
  Int32 AttrNum;        /* Attribute Number */
  Int32 DataType;       /* Entry Data Type */
  Int32 Num;            /* Entry number (rVariable/zVariable number or
			   arbitrary global `entry' [gEntry] number) */
  Int32 NumElems;       /* Number of elements of DataType */
  Int32 rfuA;           /* reserved for future use (value = 0) */
  Int32 rfuB;           /* reserved for future use (value = 0) */
  Int32 rfuC;           /* reserved for future use (value = 0) */
  Int32 rfuD;           /* reserved for future use (value = -1) */
  Int32 rfuE;           /* reserved for future use (value = -1) */
};

/*****************************************************************************
* AEDRstructExt - structure that extends AEDRstruct to hold value data in
* support of the READONLY metadata enhancement.
****************************************************************************/

struct AEDRstructExt {
    struct AEDRstruct AEDR;
                       /* The basic AEDR data. */
    int ValueSize;     /* The size in bytes of the value data. */
    void *Value;       /* The value data. */
};

#define AEDR_NULL               (-1)
#define AEDR_RECORD             0
#define AEDR_RECORDSIZE         1
#define AEDR_RECORDTYPE         2
#define AEDR_AEDRNEXT           3
#define AEDR_ATTRNUM            4
#define AEDR_DATATYPE           5
#define AEDR_NUM                6
#define AEDR_NUMELEMS           7
#define AEDR_VALUE              8

#define AEDR_RECORDSIZE_OFFSET  0
#define AEDR_RECORDTYPE_OFFSET  4
#define AEDR_AEDRNEXT_OFFSET    8
#define AEDR_ATTRNUM_OFFSET     12
#define AEDR_DATATYPE_OFFSET    16
#define AEDR_NUM_OFFSET         20
#define AEDR_NUMELEMS_OFFSET    24
#define AEDR_RFUa_OFFSET        28
#define AEDR_RFUb_OFFSET        32
#define AEDR_RFUc_OFFSET        36
#define AEDR_RFUd_OFFSET        40
#define AEDR_RFUe_OFFSET        44
#define AEDR_VALUE_OFFSET       48

/******************************************************************************
* CPR structure/fields/offsets - Compression Parameters Record.
******************************************************************************/

struct CPRstruct {
  Int32 RecordSize;     /* Size of record (bytes) */
  Int32 RecordType;     /* Type of record (ie. CPR) */
  Int32 cType;          /* Type of compression. */
  Int32 rfuA;           /* Reserved for future use.  Set to zero. */
  Int32 pCount;         /* Parameter count. */
  Int32 cParms[CDF_MAX_PARMS];
			/* Parameters. */
};

#define CPR_NULL                (-1)
#define CPR_RECORD              0
#define CPR_RECORDSIZE          1
#define CPR_RECORDTYPE          2
#define CPR_CTYPE               3
#define CPR_RFUa                4
#define CPR_PCOUNT              5
#define CPR_CPARM1              6
#define CPR_CPARM2              7
#define CPR_CPARM3              8
#define CPR_CPARM4              9
#define CPR_CPARM5              10

#define CPR_RECORDSIZE_OFFSET   0
#define CPR_RECORDTYPE_OFFSET   4
#define CPR_CTYPE_OFFSET        8
#define CPR_RFUa_OFFSET         12
#define CPR_PCOUNT_OFFSET       16
#define CPR_CPARM1_OFFSET       20
#define CPR_CPARM2_OFFSET       24
#define CPR_CPARM3_OFFSET       28
#define CPR_CPARM4_OFFSET       32
#define CPR_CPARM5_OFFSET       36

/******************************************************************************
* SPR structure/fields/offsets - Sparseness Parameters Record.
******************************************************************************/

struct SPRstruct {
  Int32 RecordSize;     /* Size of record (bytes) */
  Int32 RecordType;     /* Type of record (ie. CPR) */
  Int32 sArraysType;    /* Type of array sparseness. */
  Int32 rfuA;           /* Reserved for future use.  Set to zero. */
  Int32 pCount;         /* Parameter count. */
  Int32 sArraysParms[CDF_MAX_PARMS];
			/* Parameters. */
};

#define SPR_NULL                (-1)
#define SPR_RECORD              0
#define SPR_RECORDSIZE          1
#define SPR_RECORDTYPE          2
#define SPR_STYPE               3
#define SPR_RFUa                4
#define SPR_PCOUNT              5
#define SPR_SPARM1              6
#define SPR_SPARM2              7
#define SPR_SPARM3              8
#define SPR_SPARM4              9
#define SPR_SPARM5              10

#define SPR_RECORDSIZE_OFFSET   0
#define SPR_RECORDTYPE_OFFSET   4
#define SPR_STYPE_OFFSET        8
#define SPR_RFUa_OFFSET         12
#define SPR_PCOUNT_OFFSET       16
#define SPR_SPARM1_OFFSET       20
#define SPR_SPARM2_OFFSET       24
#define SPR_SPARM3_OFFSET       28
#define SPR_SPARM4_OFFSET       32
#define SPR_SPARM5_OFFSET       36

/******************************************************************************
* CVVR structure/fields/offsets - Compressed Variable Values Record.
******************************************************************************/

struct CVVRstruct {
  Int32 RecordSize;     /* Size of record (bytes) */
  Int32 RecordType;     /* Type of record (ie. CVVR) */
  Int32 rfuA;           /* Reserved for future use.  Set to zero (0). */
  Int32 cSize;          /* Size in bytes of compressed data. */
			/* Bytes... */
			/* Extra... */
};

#define CVVR_NULL               (-1)
#define CVVR_RECORDx            0
#define CVVR_RECORDSIZE         1
#define CVVR_RECORDTYPE         2
#define CVVR_RFUa               3
#define CVVR_CSIZE              4

#define CVVR_RECORDSIZE_OFFSET  0
#define CVVR_RECORDTYPE_OFFSET  4
#define CVVR_RFUa_OFFSET        8
#define CVVR_CSIZE_OFFSET       12

/******************************************************************************
* UIR structure/fields/offsets - Unused Internal Record.
******************************************************************************/

struct UIRstruct {
  Int32 RecordSize;     /* Size of current record (bytes). */
  Int32 RecordType;     /* Type of record (ie. UIR). */
  Int32 NextUIR;        /* File offset of next UIR (bytes). */
  Int32 PrevUIR;        /* File offset of previous UIR (bytes). */
};

#define UIR_NULL                (-1)
#define UIR_RECORD              0
#define UIR_RECORDSIZE          1
#define UIR_RECORDTYPE          2
#define UIR_NEXTUIR             3
#define UIR_PREVUIR             4

#define UIR_RECORDSIZE_OFFSET   0
#define UIR_RECORDTYPE_OFFSET   4
#define UIR_NEXTUIR_OFFSET      8
#define UIR_PREVUIR_OFFSET      12

/******************************************************************************
* Structure for variable record/dimension states for read/write operations.
******************************************************************************/

struct rdSTRUCT {
  Int32 recNumber;                       /* Current record number. */
  Int32 recCount;                        /* Current record count. */
  Int32 recInterval;                     /* Current record interval. */
  Int32 dimIndices[CDF_MAX_DIMS];        /* Current dimension indices. */
  Int32 dimCounts[CDF_MAX_DIMS];         /* Current dimension counts. */
  Int32 dimIntervals[CDF_MAX_DIMS];      /* Current dimension intervals. */
};

/******************************************************************************
* Structures for rVariable/zVariable related data.
******************************************************************************/

struct VarStageStruct {
  Int32 areaOffset;             /* Offset in staging file of area for this
				   variable using 32-bit offset. */
  OFF_T areaOffset64;           /* Offset in staging file of area for this
                                   variable using 64-bit offset. */
  Int32 firstRec;               /* Number of first record in staging area. */
  Int32 lastRec;                /* Number of last record in staging area. */
  Int32 dotOffset;              /* Associated VVR/CVVR offset in dotCDF file
				   using 32-bit offset.
				   NO_OFFSET if no VVR/CVVR yet. */
  OFF_T dotOffset64;            /* Associated VVR/CVVR offset in dotCDF file
				   using 64-bit offset.
                                   NO_OFFSET if no VVR/CVVR yet. */
  Logical modified;             /* TRUE if record(s) in staging area have been
				   modified. */
};

struct VarStruct {
  Int32 VDRoffset;              /* Byte offset of rVDR/zVDR using 32-bit
				   offset. */
  OFF_T VDRoffset64;            /* Byte offset of rVDR/zVDR using 64-bit
				   offset. */
  Int32 varN;                   /* The variable's number. */
  Int32 dataType;               /* The variable's data type. */
  vFILE *fp;                    /* For a multi-file CDF, this is NULL when
				   the variable file is closed.  For a
				   single-file CDF, this is always NULL. */
  int varCacheSize;             /* Number of cache buffers for the variable
				   file (if a multi-file CDF). */
  uLongx accessed_at;           /* Pseudo-clock time of last access. */
  CDFstatus (*DecodeFunction) PROTOARGs((void *buffer, Int32 numElems));
				/* Function which converts values read from
				   the CDF to the desired decoding. */
  CDFstatus (*EncodeFunction) PROTOARGs((void *buffer, Int32 numElems));
				/* Function which converts values being written
				   to the CDF to the desired encoding. */
  /****************************************************************************
  * The following values are calculated based on the current zMode.  For
  * rVariables they may differ from the values in the VDR.  For zVariables
  * they will be the same as the values in the VDR but are maintained here
  * for consistency.
  ****************************************************************************/
  Int32 numDims;                 /* Number of dimensions, based on zMode. */
  Int32 dimSizes[CDF_MAX_DIMS];  /* Dimension sizes, based on zMode. */
  Int32 recVary;                 /* Record variance, based on zMode (doesn't
				    differ but done for consistency). */
  Int32 dimVarys[CDF_MAX_DIMS];  /* Dimension variances, based on zMode. */
  /****************************************************************************
  * The following values are maintained in memory for efficiency.
  ****************************************************************************/
  Int32 NvalueElems;             /* Number of elements per value. */
  Int32 NelemBytes;              /* Number of bytes per element. */
  Int32 NvalueBytes;             /* Number of bytes per value. */
  Int32 NphyRecValues;           /* Number of physical values per record. */
  Int32 NvirtRecValues;          /* Number of virtual values per record. */
  Int32 NphyRecElems;            /* Number of physical elements per record. */
  Int32 NvirtRecElems;           /* Number of virtual elements per record. */
  Int32 NphyRecBytes;            /* Number of physical bytes per record. */
  Int32 NvirtRecBytes;           /* Number of virtual bytes per record. */
  Int32 nPhyDimValues[CDF_MAX_DIMS];
				 /* Number of physical values `below' each
				    occurance of a dimension. */
  Int32 maxRec;                  /* Maximum record number. */
  /****************************************************************************
  * The following values maintain the current positioning.
  ****************************************************************************/
  Int32 seqValueOffset;         /* Used for sequential access using 32-bit
				   offset.  The current
				   value offset (not the byte offset). */
  OFF_T seqValueOffset64;       /* Used for sequential access using 64-bit
				   offset.  The current
                                   value offset (not the byte offset). */
  struct rdSTRUCT zRD;          /* Current record/dimension states.  These are
				   always used for zVariables.  For rVariables,
				   these are only used while in zMode.  When
				   not in zMode, the current record/dimension
				   states in the `CDF' structure are used (and
				   are the same for all rVariables). */
  /****************************************************************************
  * The following are dependent on the type of variable.
  ****************************************************************************/
  Logical zVar;                 /* TRUE if a zVariable, FALSE if an
				   rVariable.  Independent of zMode. */
  int vType;                    /* Type of variable:
				     STANDARD_, SPARSE_RECORDS, COMPRESSED_,
				     SPARSE_COMPRESSED_RECORDS, SPARSE_ARRAYS,
				     or SPARSE_RECORDS_AND_ARRAYS. */
  Int32 blockingFactor;         /* Standard (in single-file CDF):
				     Number of records by which to extend a
				     variable when necessary.
				   Compressed:
				     Number of records per block (except for
				     the last block which may be smaller).
				   Sparse compressed records:
				     Maximum number of records per block.
				   Sparse records:
				     Number of records in staging area.
				   Sparse records and arrays:
				   Sparse arrays:
				   In multi-file CDF:
				     N/A. */
  Int32 maxAllocated;           /* Maximum record number allocated. */
  Int32 maxWritten;             /* Maximum record number written/padded. */
  Int32 cType;                  /* Compression type. */
  Int32 cParms[CDF_MAX_PARMS];  /* Compression parameters. */
  struct VarStageStruct stage;  /* Staging control. */
  Logical prevIfMissing;        /* TRUE: return previous records' value if a
				   record is missing (sRecords.PREV).
				   FALSE: return pad value if a record is
				   missing (sRecords.PAD or sRecords.NO). */
  int reservePct;               /* Reserve percentage for compressed variables.
				   0            No reserve percentage (the
						default).
				   1..100       Allocate (as a minimum) n% of
						the uncompressed size.
				   101...       Allocate an addition (n-100)%
						of space beyond the original
						compressed size. */
  Int32 firstRecInVVR;          /* First data record in the most recently
                                   accessed VVR. */
  Int32 lastRecInVVR;           /* Last data record in the most recently
                                   accessed VVR. */
  Int32 offsetOfVVR;            /* Offset of the most recently accessed VVR
				   from the cdf using 32-bit offset. */
  OFF_T offsetOfVVR64;          /* Offset of the most recently accessed VVR
				   from the cdf using 64-bit offset. */
};

/******************************************************************************
* Structures for CDF related data.
******************************************************************************/

struct CDFstageStruct {
  vFILE *fp;                            /* Staging scratch file pointer. */
  Int32 mark;                           /* New allocations (for a variable)
					   are at this offset using 32-bit. */
  OFF_T mark64;				/* New allocations (for a variable)
                                           are at this offset using 64-bit. */
  int cacheSize;			/* Number of cache buffers for the
					   staging scratch file. */
};

struct CDFstruct {
  uInt32 magic;				/* Magic number stored in CDF
					   structure in memory.  Used to
					   check that a valid `CDFid' was
					   specified. */
  Logical largeFile;                    /* TRUE: if based on 64-bit offset,
					   FALSE: older 32-bit offset. */
  vFILE *fp;                            /* Working file pointer.  Set to either
					   `dotFp' (if not a compressed CDF)
					   or `uDotFp' (if a compressed CDF).*/
  vFILE *dotFp;                         /* File pointer to dotCDF file (for a
                                           uncompressed CDF). . */
  vFILE *uDotFp;                        /* File pointer to uncompressed dotCDF
					   file (for a compressed CDF). */
  vFILE *compressFp;			/* Compression scratch file pointer.
					   If NULL, the compression scratch
					   file has not been created yet. */
  Int32 CDRoffset;                      /* Byte offset of CDR using 32-bit
					   offset. N/A if a Version 1
					   CDF. */
  OFF_T CDRoffset64;                    /* Byte offset of CDR using 64-bit
					   offset. N/A if a Version 1
					   CDF. */
  Int32 GDRoffset;                      /* Byte offset of GDR using 32-bit.
					   N/A if a Version 1 CDF. */
  OFF_T GDRoffset64;                    /* Byte offset of GDR using 64-bit.
                                           N/A if a Version 1 CDF. */
  char *CDFname;                        /* CDF name as specified by caller. */
  char *scratchDir;                     /* Directory to be used for scratch
					   files. */
  Int32 decoding;                       /* Decoding (encoding) for attribute
					   entry and variable values read from
					   a CDF. */
  Logical readOnly;                     /* TRUE: read-only access,
					   FALSE: read-write access. Applicable 
                                           to opening existing files. */
  int zMode;                            /* zMODEoff: no zMode,
					   zMODEon1: keep NOVARY variances
						     (rVariables --> zGroup),
					   zMODEon2: remove dimensions with
						     NOVARY variance. */
  Logical negToPosFp0;                  /* TRUE: convert -0.0 to 0.0,
					   FALSE: don't convert. */
  int status;                           /* READ_ONLY, or READ_WRITE. */
  int tt2000Updated;                    /* 1: if leap second last updated 
                                           header has been updated from the
                                           table. 0: no update yet. */
  int workingCacheSize;                 /* Number of cache buffers for the
					   "working" dotCDF file. */
  int compressCacheSize;		/* Number of cache buffers for the
					   compression scratch file. */
  uLongx pseudo_clock;                  /* Tick count used to determine which
					   variable was least recently used. */
  Logical singleFile;                   /* TRUE if a single-file CDF. */
  Logical rowMajor;                     /* TRUE if a row major CDF. */
  Int32 encoding;                       /* Encoding of CDF. */
  Int32 rNumDims;                       /* Number of rVariable dimensions. */
  Int32 rDimSizes[CDF_MAX_DIMS];        /* rVariable dimension sizes. */
  Int32 rMaxRec;                        /* Maximum record number of all the
					   rVariables. */
  Int32 NrVars;                         /* Number of rVariables. */
  Int32 NzVars;                         /* Number of zVariables. */
  int MAXrVars;                         /* Current size of `rVars' array. */
  int MAXzVars;                         /* Current size of `zVars' array. */
  struct VarStruct **rVars;             /* Pointer to array of pointers to
					   rVariable data structures. */
  struct VarStruct **zVars;             /* Pointer to array of pointers to
					   zVariable data structures. */
  Int32 CURrVarNum;                     /* Current rVariable number (when not
					   in zMode). */
  Int32 CURrVarOffset;                  /* Current selected rVariable offset
					   using 32-bit offset.*/
  OFF_T CURrVarOffset64;                /* Current selected rVariable offset
					   using 64-bit offset.*/
  Int32 CURzVarNum;                     /* Current zVariable number (used when
					   zMode is on or off). */
  Int32 CURzVarOffset;                  /* Current selected zVariable offset
					   using 32-bit offset.*/
  OFF_T CURzVarOffset64;                /* Current selected zVariable offset
					   using 64-bit offset.*/
  Int32 CURattrOffset;                  /* Offset of current attribute's ADR
					   using 32-bit offset.
					   Set to RESERVED_ATTROFFSET if an
					   attribute is not selected. */
  OFF_T CURattrOffset64;                /* Offset of current attribute's ADR
					   using 64-bit offset.
					   Set to RESERVED_ATTROFFSET if an
					   attribute is not selected. */
  Int32 CURgrEntryNum;                  /* Currently selected gEntry/rEntry
					   number (a corresponding gEntry/
					   rEntry in the current attribute
					   does not have to exist yet).  When
					   in zMode, this is used only for
					   the gEntry number.  Set to
					   RESERVED_ENTRYNUM when a
					   gEntry/rEntry is not selected. */
  Int32 CURzEntryNum;                    /* Currently selected zEntry number (a
					   corresponding zEntry in the current
					   attribute does not have to exist
					   yet).  This is used when zMode is
					   on or off.  When zMode is on, could
					   actually point to an AgrEDR.  Set to
					   RESERVED_ENTRYNUM when a zEntry is
					   not selected. */
  Int32 CURgrEntryOffset;               /* Offset of current AgrEDR using 32-bit
					   offset.  Not set
					   to RESERVED_ENTRYOFFSET when an
					   attribute and gEntry/rEntry have
					   been selected and the entry actually
					   exists. */
  OFF_T CURgrEntryOffset64;             /* Offset of current AgrEDR using 64-bit
					   offset.  Not set
                                           to RESERVED_ENTRYOFFSET when an
                                           attribute and gEntry/rEntry have
                                           been selected and the entry actually
                                           exists. */
  Int32 CURzEntryOffset;                /* Offset of current AzEDR using 32-bit
					   offset.  Not set
					   to RESERVED_ENTRYOFFSET when an
					   attribute and zEntry have been
					   selected and the entry actually
					   exists.  If zMode is on, could point
					   to an AgrEDR. */
  OFF_T CURzEntryOffset64;              /* Offset of current AzEDR using 64-bit
					   offset.  Not set
                                           to RESERVED_ENTRYOFFSET when an
                                           attribute and zEntry have been
                                           selected and the entry actually
                                           exists.  If zMode is on, could point
                                           to an AgrEDR. */
  struct rdSTRUCT rRD;                  /* Current record/dimension states for
					   the rVariables (they are the same
					   for all rVariables). */
  struct CDFstageStruct stage;          /* Staging control. */
  Logical no_append;                    /* TRUE if pathnames should not have
					   extensions or version numbers
					   appended. */
  Logical upper_case_ext;               /* TRUE if uppercase file extensions.*/
  Logical version_numbers;              /* TRUE if a ";1" is appended to all
					   pathnames. */
  Logical fakeEPOCH;                    /* TRUE if a CDF prior to CDF V2.1.1
					   for which the data type CDF_EPOCH
					   should be substituted for the actual
					   data type in some cases involving
					   the rVariable "EPOCH". */
  Logical wastedSpace;                  /* TRUE if a CDF prior to CDF V2.5 in
					   which there is 128 bytes of wasted
					   space in each VDR and 1689 bytes of
					   wasted space in the CDR. */
  Logical badEOF;                       /* TRUE if a CDF prior to CDF V2.1 in
					   which case the EOF of the GDR was
					   not used (ie. contains garbage). */
  Logical badTerminatingOffsets;        /* TRUE if a CDF prior to CDF V2.1 in
					   which the terminating offsets of the
					   ADR and rVDR linked lists are bad.*/
  Logical assumedScopes;                /* TRUE if a CDF prior to CDF V2.5 in
					   which there are assumed attribute
					   scopes. */
  vSTATS dotCDFvStats;			/* Vstream statistics for the dotCDF
					   file (compressed or uncompressed).*/
  vSTATS uDotCDFvStats;			/* Vstream statistics for the
					   uncompressed dotCDF file (of a
					   compressed CDF). */
  long     checksum;                    /* Indicator for check sum. */
  Int32 eof;                            /* End-of-file (EOF) position in the 
                                           dotCDF file (total # of bytes used
					   in the dotCDF file.) for non-LFS*/
  OFF_T eof64;                          /* End-of-file (EOF) position in the 
					   dotCDF file (total # of bytes used
					   in the dotCDF file.) for LFS*/
  int leapSecondUpdated;		/* While writing/updating a CDF, the
					   field indicating the
					   LeapSecondLastUpdated field in GDR
					   had been updated. */
};

/******************************************************************************
* Structure for `string' related data (used by FORTRAN interfaces).
******************************************************************************/

struct STRINGstruct {
  char *string;
  struct STRINGstruct *next;
};

/******************************************************************************
* Structure for `varargs' related data.
******************************************************************************/

struct VAstruct {
  va_list ap;
  long fnc;
  long item;
};

/******************************************************************************
* Structure for `current' data.
******************************************************************************/

struct CurStruct {
  struct CDFstruct *cdf;
  CDFstatus status;
};

/******************************************************************************
* SelectCDF.
******************************************************************************/

#define SelectCDF(cur_,cdf_) \
if (cur_ == NULL) \
  return NO_CDF_SELECTED; \
else \
  if (cur_->magic == ABORTEDid_MAGIC_NUMBER) \
    return NO_MORE_ACCESS; \
  else \
    cdf_ = cur_;

/******************************************************************************
* SINGLEfileBITset.
******************************************************************************/

#define SINGLEfileBITset(flags) (BITSET(flags,CDR_FORMAT_BIT))

/******************************************************************************
* ROWmajorBITset.
******************************************************************************/

#define ROWmajorBITset(flags) (BITSET(flags,CDR_MAJORITY_BIT))

/******************************************************************************
* CHECKsumBITset.
******************************************************************************/

#define CHECKsumBITset(flags) (BITSET(flags,CDR_CHECKSUM_BIT))

/******************************************************************************
* zModeON/zModeOFF.
******************************************************************************/

#define zModeON(CDF) (CDF->zMode == zMODEon1 || CDF->zMode == zMODEon2)
#define zModeOFF(CDF) (CDF->zMode == zMODEoff)

/******************************************************************************
* Checks for set bits in VDR flags.
******************************************************************************/

#define PADvalueBITset(flags) (BITSET(flags,VDR_PADVALUE_BIT))
#define RECvaryBITset(flags) (BITSET(flags,VDR_RECVARY_BIT))
#define SPARSEarraysBITset(flags) (BITSET(flags,VDR_SPARSEARRAYS_BIT))
#define VARcompressionBITset(flags) (BITSET(flags,VDR_COMPRESSION_BIT))

/******************************************************************************
* Used to check for large file (64-bit) offset.
******************************************************************************/

#define isLFS(CDF) (CDF->largeFile)

/******************************************************************************
* Function prototypes.
******************************************************************************/

STATICforIDL struct VarStruct *VarStructPtr PROTOARGs((
  struct CDFstruct *CDF, Logical zOp, long varN
));
STATICforIDL void DefaultPadValue PROTOARGs((
  Int32 dataType, Int32 numElems, void *padValue
));
STATICforIDL void DefaultPadValuePre350 PROTOARGs((
  Int32 dataType, Int32 numElems, void *padValue
));
STATICforIDL CDFstatus DefaultPadBuffer PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 nValues, void *buffer
));
STATICforIDL CDFstatus PadBuffer PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 nValues, void *buffer
));
STATICforIDL CDFstatus WasteIR PROTOARGs((
  struct CDFstruct *CDF, Int32 offset, Int32 size
));
STATICforIDL Int32 IndicesValueOffset PROTOARGs((
  Int32 numDims, Int32 *indices, Int32 *dimVarys, Int32 *nPhyDimValues
));
STATICforIDL void ValueOffsetIndices PROTOARGs((
  Int32 offset, Logical rowMajor, Int32 numDims, Int32 *dimVarys,
  Int32 *nPhyDimValues, Int32 *indices
));
STATICforIDL CDFstatus RecordByteOffset PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 phyRecN, Int32 *offset
));
STATICforIDL CDFstatus ConfigureNEWzMode PROTOARGs((struct CDFstruct *CDF));
STATICforIDL void InitCURobjectsStates PROTOARGs((struct CDFstruct *CDF));
STATICforIDL void ResetReadOnlyState PROTOARGs((struct CDFstruct *CDF));
STATICforIDL Int32 HostEncoding PROTOARGs((void));
STATICforIDL Int32 HostDecoding PROTOARGs((void));
STATICforIDL int IntegerOrder PROTOARGs((Int32 ed));
STATICforIDL int FpType PROTOARGs((Int32 ed));
STATICforIDL Logical EquivDataTypes PROTOARGs((
  Int32 dataType1, Int32 dataType2
));
STATICforIDL void FreeCDFid PROTOARGs((
  struct CDFstruct *CDF, Logical aborting
));
STATICforIDL CDFstatus CloseLRUvar PROTOARGs((struct CDFstruct *CDF));
STATICforIDL CDFstatus VarAccess PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var
));
STATICforIDL CDFstatus AllocateRecords VARPROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, struct AllocStruct alloc
));
STATICforIDL CDFstatus PadUnRecords PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 firstRec, Int32 lastRec
));
STATICforIDL CDFstatus CloseVarFiles PROTOARGs((struct CDFstruct *CDF));
STATICforIDL Logical WriteAccess PROTOARGs((
  struct CDFstruct *CDF, Logical forDelete, CDFstatus *pStatus
));
STATICforIDL CDFstatus WriteVarElems PROTOARGs((
  struct VarStruct *Var, vFILE *fp, Int32 offset, Int32 numElems, void *buffer
));
STATICforIDL CDFstatus WriteBuffer PROTOARGs((
  struct CDFstruct *CDF, vFILE *fp, Int32 dataType, Int32 numElems,
  void *buffer
));
STATICforIDL CDFstatus HyperRead PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, struct rdSTRUCT *rd,
  void *buffer
));
STATICforIDL CDFstatus HyperReadDim PROTOARGs((
  Int32 numDims, Int32 *dimSizes, Int32 *dimVarys, Int32 *indices,
  Int32 *counts, Int32 *intervals, Int32 *nHypDimValues, Int32 *nPhyDimValues,
  Logical *fullPhyDim, int firstDim, int dimIncr, Int32 recNum, Int32 offset,
  void *buffer, void *phyBuffer, struct CDFstruct *CDF, struct VarStruct *Var
));
STATICforIDL CDFstatus HyperWrite PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, struct rdSTRUCT *rd,
  void *buffer
));
STATICforIDL CDFstatus HyperWriteDim PROTOARGs((
  Int32 numDims, Int32 *dimSizes, Int32 *dimVarys, Int32 *indices,
  Int32 *counts, Int32 *intervals, Int32 *nHypDimValues, Int32 *nPhyDimValues,
  Logical *fullPhyDim, int firstDim, int dimIncr, Int32 recNum, Int32 offset,
  void *buffer, void *phyBuffer, struct CDFstruct *CDF, struct VarStruct *Var
));
STATICforIDL CDFstatus CDFcre PROTOARGs((
  struct VAstruct *Va, struct CurStruct *Cur
));
STATICforIDL CDFstatus CDFope PROTOARGs((
  struct VAstruct *Va, struct CurStruct *Cur
));
STATICforIDL CDFstatus CDFdel PROTOARGs((
  struct VAstruct *Va, struct CurStruct *Cur
));
STATICforIDL CDFstatus CDFclo PROTOARGs((
  struct VAstruct *Va, struct CurStruct *Cur
));
STATICforIDL CDFstatus CDFget PROTOARGs((
  struct VAstruct *Va, struct CurStruct *Cur
));
STATICforIDL CDFstatus CDFput1 PROTOARGs((
  struct VAstruct *Va, struct CurStruct *Cur
));
STATICforIDL CDFstatus CDFput2 PROTOARGs((
  struct VAstruct *Va, struct CurStruct *Cur
));
STATICforIDL CDFstatus CDFsel PROTOARGs((
  struct VAstruct *Va, struct CurStruct *Cur
));
STATICforIDL CDFstatus CDFcon PROTOARGs((
  struct VAstruct *Va, struct CurStruct *Cur
));
STATICforIDL CDFstatus CDFsav PROTOARGs((
  struct VAstruct *Va, struct CurStruct *Cur
));
STATICforIDL void CDFcopyRight PROTOARGs((char *copyRight));
STATICforIDL CDFstatus CDFstatusText PROTOARGs((
  CDFstatus status, char *textPtr
));
STATICforIDL CDFstatus Read_V1_header PROTOARGs((struct CDFstruct *CDF));
STATICforIDL CDFstatus Read_V2_header PROTOARGs((struct CDFstruct *CDF));
STATICforIDL CDFstatus Write_V2_header PROTOARGs((struct CDFstruct *CDF));
STATICforIDL char *NULterminate PROTOARGs((
  char *string, size_t length, struct STRINGstruct **ssh
));
STATICforIDL void FreeStrings PROTOARGs((struct STRINGstruct *ssh));
VISIBLE_PREFIX Logical ValidVarName PROTOARGs((char *name));
VISIBLE_PREFIX Logical ValidAttrName PROTOARGs((char *name));
VISIBLE_PREFIX Logical ValidDataType PROTOARGs((Int32 dataType));
VISIBLE_PREFIX Logical InValidDataType PROTOARGs((Int32 dataType));
VISIBLE_PREFIX Logical ValidAttrScope PROTOARGs((Int32 scope));
VISIBLE_PREFIX Logical ValidEncoding PROTOARGs((
  Int32 encoding, Int32 *actualEncoding
));
VISIBLE_PREFIX Logical ValidDecoding PROTOARGs((Int32 decoding));
VISIBLE_PREFIX CDFstatus ValidateCompression PROTOARGs((
  long cType, long *cParms
));
STATICforIDL CDFstatus ConversionFunction PROTOARGs((
  Int32 dataType, Int32 encoding, Int32 decoding, Logical negToPosFp0,
  CDFstatus (**function) PROTOARGs((void *buffer, Int32 numElems))
));
STATICforIDL CDFstatus FP1toFP34single PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP2toFP34single PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP34toFP1single PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP34toFP2single PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP1toFP2singleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP1toFP34singleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP2toFP1singleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP2toFP34singleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP34toFP1singleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP34toFP2singleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP1toFP3double PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP1toFP4double PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP2toFP3double PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP2toFP4double PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP3toFP1double PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP3toFP2double PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP3toFP4double PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP4toFP1double PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP4toFP2double PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP4toFP3double PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP1toFP2doubleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP1toFP3doubleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP1toFP4doubleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP2toFP1doubleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP2toFP3doubleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP2toFP4doubleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP3toFP1doubleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP3toFP2doubleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP3toFP4doubleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP4toFP1doubleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP4toFP2doubleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP4toFP3doubleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP1singleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP2singleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP34singleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP1doubleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP2doubleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP3doubleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP4doubleNEGtoPOS PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus FP3doubleLIMIT PROTOARGs((
  void *buffer, Int32 numElems
));
STATICforIDL CDFstatus Reverse2 PROTOARGs((void *buffer, Int32 numElems));
STATICforIDL CDFstatus Reverse4 PROTOARGs((void *buffer, Int32 numElems));
STATICforIDL CDFstatus Reverse8 PROTOARGs((void *buffer, Int32 numElems));
STATICforIDL CDFstatus Reverse16 PROTOARGs((void *buffer, Int32 numElems));
#if defined(Fif_DESCR)
  STATICforIDL Logical isDESCR PROTOARGs((void *ptr, char **aptr, size_t *len));
  STATICforIDL char *DESCRtoREF PROTOARGs((void *ptr));
  STATICforIDL char *DESCRtoREFnul PROTOARGs((
    void *ptr, size_t maxREFlen, struct STRINGstruct **ssh
));
#endif
#if defined(Fif_NOLEN)
  STATICforIDL char *FindEndNUL PROTOARGs((
    char *ptr, size_t maxREFlen, struct STRINGstruct **ssh
));
#endif
STATICforIDL void SetBit32 PROTOARGs((Int32 *value, int bit));
STATICforIDL void ClearBit32 PROTOARGs((Int32 *value, int bit));
STATICforIDL void CtoFORTstring PROTOARGs((
  char *Cstring, void *FORTstring, int length
));
STATICforIDL void NulPad PROTOARGs((char *string, int length));
STATICforIDL CDFstatus UpdateMaxRec PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 phyRecNum
));
STATICforIDL CDFstatus FindCDF PROTOARGs((
  char *path, Logical *append, Logical *upper, Logical *version
));
STATICforIDL void BuildFilePath PROTOARGs((
  int fileType, char *pathBase, Logical noAppend, Logical upperCase,
  Logical versionNumber, Int32 varN, char pathX[DU_MAX_PATH_LEN+1]
));
STATICforIDL CDFstatus LocateCurrentVar PROTOARGs((
  struct CDFstruct *CDF, Logical zOp, Int32 *offset, Logical *zVar,
  struct VarStruct **Var
));
STATICforIDL Logical CurrentVarMode PROTOARGs((
  struct CDFstruct *CDF, Logical zOp
));
STATICforIDL CDFstatus InitCurrentVar PROTOARGs((
  struct CDFstruct *CDF, Logical zVar, struct VarStruct **Var
));
STATICforIDL CDFstatus InitVar PROTOARGs((
  struct CDFstruct *CDF, Int32 varN, Logical zVar, struct VarStruct **Var
));
STATICforIDL CDFstatus InitVar2 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var
));
STATICforIDL CDFstatus VarIdentity PROTOARGs((
  struct CDFstruct *CDF, Int32 varN, Logical zOp, Int32 *varNt, Logical *zVar,
  struct VarStruct **Var
));
STATICforIDL CDFstatus OpenVar PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var
));
STATICforIDL CDFstatus FindAttrByName PROTOARGs((
  struct CDFstruct *CDF, char *searchName, Int32 *offset
));
STATICforIDL CDFstatus FindAttrByNumber PROTOARGs((
  struct CDFstruct *CDF, Int32 searchNum, Int32 *offset
));
STATICforIDL CDFstatus FindEntryByNumber PROTOARGs((
  struct CDFstruct *CDF, Int32 ADRoffset, Logical zEntry, Int32 entryN,
  Int32 *offset
));
STATICforIDL CDFstatus FindVarByName PROTOARGs((
  struct CDFstruct *CDF, char *searchName, Int32 *offset, Logical *zVar,
  struct VarStruct **Var
));
STATICforIDL CDFstatus FindVarByNumber PROTOARGs((
  struct CDFstruct *CDF, Int32 searchNum, Logical zVar, Int32 *offset
));
STATICforIDL CDFstatus FindLastAttr PROTOARGs((
  struct CDFstruct *CDF, Int32 *lastOffset
));
STATICforIDL CDFstatus FindLastEntry PROTOARGs((
  struct CDFstruct *CDF, Int32 ADRoffset, Logical zEntry, Int32 *lastOffset
));
STATICforIDL CDFstatus FindPrevEntry PROTOARGs((
  struct CDFstruct *CDF, Int32 ADRoffset, Int32 searchOffset, Logical zEntry,
  Int32 *prevOffset
));
STATICforIDL CDFstatus CheckEntryOp PROTOARGs((
  struct CDFstruct *CDF, int entryType
));
STATICforIDL CDFstatus SetCURgrEntry PROTOARGs((
  struct CDFstruct *CDF, Logical useCurrent, Int32 entryNum
));
STATICforIDL CDFstatus SetCURzEntry PROTOARGs((
  struct CDFstruct *CDF, Logical useCurrent, Int32 entryNum
));
STATICforIDL CDFstatus CalcDimParms PROTOARGs((
  struct CDFstruct *CDF, Int32 offset, Logical zVar, Int32 *numDimsP,
  Int32 dimSizesP[], Int32 dimVarysP[]
));
STATICforIDL Int32 SeqValueByteOffset PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var
));
VISIBLE_PREFIX void NULterminateMAX PROTOARGs((char *string, size_t maxLen));
STATICforIDL CDFstatus AllocateIR PROTOARGs((
  struct CDFstruct *CDF, Int32 size, Int32 *offset
));
STATICforIDL CDFstatus ResizeIR PROTOARGs((
  struct CDFstruct *CDF, Int32 curOffset, Int32 newSize, Int32 *newOffset,
  Logical move, Logical *success
));
STATICforIDL CDFstatus RemoveUIRs PROTOARGs((
  struct CDFstruct *CDF, Int32 sOffset, Int32 eOffset
));
STATICforIDL CDFstatus LastRecord PROTOARGs((
  struct CDFstruct *CDF, Int32 offset, Logical zVar, Int32 *recNum
));
STATICforIDL CDFstatus VerifyNoRecordsWritten PROTOARGs((
  struct CDFstruct *CDF, Logical *no
));
STATICforIDL CDFstatus VerifyNoPadsSpecified PROTOARGs((
  struct CDFstruct *CDF, Logical *no
));
STATICforIDL CDFstatus VerifyNoEntriesWritten PROTOARGs((
  struct CDFstruct *CDF, Logical *no
));
STATICforIDL Logical Read32s PROTOARGs((vFILE *fp, Int32 *buffer, int count));
STATICforIDL CDFstatus ReadIrSize PROTOARGs((
  vFILE *fp, Int32 offset, Int32 *irSize
));
STATICforIDL CDFstatus ReadCDR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
STATICforIDL CDFstatus ReadGDR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
STATICforIDL CDFstatus ReadADR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
STATICforIDL CDFstatus ReadADRList PROTOARGs((vFILE *fp));
STATICforIDL CDFstatus ReadAEDRList PROTOARGs((
  vFILE *fp, struct AEDRstructExt ***AEDRList, Int32 AEDRHead,
  Int32 NumEntries, Int32 MaxEntry
));
STATICforIDL CDFstatus ReadAEDR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
STATICforIDL CDFstatus ReadVDR VARPROTOARGs((
  struct CDFstruct *CDF, vFILE *fp, Int32 offset, Logical zVar, ...
));
STATICforIDL CDFstatus ReadVVR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
STATICforIDL CDFstatus ReadCCR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
STATICforIDL CDFstatus ReadCPR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
STATICforIDL CDFstatus ReadSPR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
STATICforIDL CDFstatus ReadCVVR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
STATICforIDL CDFstatus ReadUIR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
STATICforIDL Logical Write32 PROTOARGs((vFILE *fp, Int32 *value));
STATICforIDL Logical Write32s PROTOARGs((vFILE *fp, Int32 *buffer, int count));
STATICforIDL CDFstatus WriteIrSize PROTOARGs((
  vFILE *fp, Int32 offset, Int32 *irSize
));
STATICforIDL CDFstatus WriteIrType PROTOARGs((
  vFILE *fp, Int32 offset, Int32 *irType
));
STATICforIDL CDFstatus WriteCDR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
STATICforIDL CDFstatus WriteGDR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
STATICforIDL CDFstatus WriteADR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
STATICforIDL CDFstatus WriteAEDR VARPROTOARGs((
  struct CDFstruct *CDF, vFILE *fp, Int32 offset, ...
));
STATICforIDL CDFstatus WriteVDR VARPROTOARGs((
  struct CDFstruct *CDF, vFILE *fp, Int32 offset, Logical zVar, ...
));
STATICforIDL CDFstatus WriteVXR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
STATICforIDL CDFstatus WriteVVR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
STATICforIDL CDFstatus WriteCCR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
STATICforIDL CDFstatus WriteCPR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
STATICforIDL CDFstatus WriteSPR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
STATICforIDL CDFstatus WriteCVVR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
STATICforIDL CDFstatus WriteUIR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
STATICforIDL CDFstatus ShortenCDR PROTOARGs((struct CDFstruct *CDF));
STATICforIDL CDFstatus ShortenVDRs PROTOARGs((struct CDFstruct *CDF));
STATICforIDL CDFstatus CorrectEPOCH PROTOARGs((struct CDFstruct *CDF));
STATICforIDL CDFstatus CorrectScopes PROTOARGs((struct CDFstruct *CDF));
STATICforIDL CDFstatus CorrectBlockingFactors PROTOARGs((
  struct CDFstruct *CDF
));
STATICforIDL CDFstatus CorrectV20offsets PROTOARGs((struct CDFstruct *CDF));
STATICforIDL CDFstatus CorrectV20eof PROTOARGs((struct CDFstruct *CDF));
STATICforIDL Logical CDFdeleteFile PROTOARGs((char *path));
STATICforIDL void KillAbortedCDF PROTOARGs((
  struct CDFstruct *CDF, struct CurStruct *Cur
));
STATICforIDL void AbortAccess PROTOARGs((
  struct CDFstruct *CDF, Logical updateCDF, Logical deleteCDF
));
STATICforIDL CDFstatus DeleteCDFfiles PROTOARGs((struct CDFstruct *CDF));
STATICforIDL CDFstatus DeleteEntry PROTOARGs((
  struct CDFstruct *CDF, Int32 aOffset, Int32 eOffset
));
STATICforIDL void StripTrailingBlanks PROTOARGs((char *string));
STATICforIDL CDFstatus DecompressCDF PROTOARGs((vFILE *dotFp, vFILE *uDotFp));
STATICforIDL CDFstatus WriteCompressedCDF PROTOARGs((
  struct CDFstruct *CDF, struct CPRstruct *CPR, Logical empty
));
STATICforIDL CDFstatus CopyCDF PROTOARGs((vFILE *srcFp, vFILE *destFp));
STATICforIDL CDFstatus CompressRLE0 PROTOARGs((
  vFILE *srcFp, Int32 srcOffset, Int32 srcSize, CDFstatus srcError,
  vFILE *destFp, Int32 destOffset, Int32 *destSize, CDFstatus destError
));
STATICforIDL CDFstatus DecompressRLE0 PROTOARGs((
  vFILE *srcFp, Int32 srcOffset, Int32 srcSize, CDFstatus srcError,
  vFILE *destFp, Int32 destOffset, CDFstatus destError
));
STATICforIDL CDFstatus CompressHUFF0 PROTOARGs((
  vFILE *input, Int32 iOffset, Int32 iSize, CDFstatus iError,
  vFILE *oFp, Int32 oOffset, Int32 *oSize, CDFstatus oError
));
STATICforIDL CDFstatus DecompressHUFF0 PROTOARGs((
  vFILE *iFp, Int32 iOffset, CDFstatus iError,
  vFILE *output, Int32 oOffset, CDFstatus oError
));
STATICforIDL CDFstatus CompressAHUFF0 PROTOARGs((
  vFILE *input, Int32 iOffset, Int32 iSize, CDFstatus iError,
  vFILE *oFp, Int32 oOffset, Int32 *oSize, CDFstatus oError
));
STATICforIDL CDFstatus DecompressAHUFF0 PROTOARGs((
  vFILE *iFp, Int32 iOffset, CDFstatus iError,
  vFILE *output, Int32 oOffset, CDFstatus oError
));
STATICforIDL CDFstatus CompressGZIP PROTOARGs((
  vFILE *srcFp, Int32 srcOffset, Int32 srcSize, CDFstatus srcError,
  vFILE *destFp, Int32 destOffset, Int32 *destSize, CDFstatus destError,
  Int32 level
));
STATICforIDL CDFstatus DecompressGZIP PROTOARGs((
  vFILE *srcFp, Int32 srcOffset, Int32 srcSize, CDFstatus srcError,
  vFILE *destFp, Int32 destOffset, CDFstatus destError
));
STATICforIDL CDFstatus WriteVarValues PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 recNum, Int32 offset,
  Int32 nValues, void *buffer
));
STATICforIDL CDFstatus SearchForRecord PROTOARGs((
  struct CDFstruct *CDF, Int32 VDRoffset, Logical zVar, Int32 recNum,
  Int32 *firstRec, Int32 *lastRec, Int32 *offset, Logical *found
));
STATICforIDL CDFstatus IndexingStatistics PROTOARGs((
  struct CDFstruct *CDF, Int32 VDRoffset, Logical zVar, Int32 *nVXRsP,
  Int32 *nEntriesP, Int32 *nAllocP, Int32 *nRecordsP, Int32 *nLevelsP
));
STATICforIDL CDFstatus BuildPadBuffer PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 nRecords, int *how,
  void **buffer, Logical encode
));
STATICforIDL CDFstatus WritePadValues PROTOARGs((
  struct VarStruct *Var, vFILE *fp, Int32 offset, Int32 nRecords, int how,
  void *buffer
));
STATICforIDL CDFstatus ReadVarElems PROTOARGs((
  struct VarStruct *Var, vFILE *fp, Int32 offset, Int32 numElems, void *buffer
));
STATICforIDL CDFstatus ReadVarValues PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 recNum, Int32 offset,
  Int32 nValues, void *buffer
));
STATICforIDL CDFstatus PrevRecord PROTOARGs((
  struct CDFstruct *CDF, Int32 VDRoffset, Logical zVar, Int32 baseRec,
  Int32 *prevRec, Logical *found
));
STATICforIDL CDFstatus NextRecord PROTOARGs((
  struct CDFstruct *CDF, Int32 VDRoffset, Logical zVar, Int32 baseRec,
  Int32 *nextRec, Logical *found
));
STATICforIDL CDFstatus VariableType PROTOARGs((
  struct CDFstruct *CDF, Int32 vdrOffset, Logical zVar, int *vType
));
STATICforIDL CDFstatus InitVarStage PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 nBytes
));
STATICforIDL CDFstatus CopyBytes PROTOARGs((
  vFILE *iFp, Int32 iStart, CDFstatus iError, Int32 nBytes, vFILE *oFp,
  Int32 oStart, CDFstatus oError
));
STATICforIDL CDFstatus ModIndexOffset PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 firstRec, Int32 lastRec,
  Int32 newOffset
));
STATICforIDL CDFstatus InitScratch PROTOARGs((
  char *scratchDir, vFILE **scratchFpH, int cacheSize
));
STATICforIDL CDFstatus CalcBF PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var
));
STATICforIDL CDFstatus CalcCompressionPct PROTOARGs((
  struct CDFstruct *CDF, Int32 vdrOffset, Logical zVar, long *cPct
));
STATICforIDL CDFstatus CalcPhyRecBytes PROTOARGs((
  struct CDFstruct *CDF, Int32 vdrOffset, Logical zVar, Int32 *nPhyRecBytes
));
STATICforIDL void CalcNumDimValues PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var
));
STATICforIDL void CalcRecValues PROTOARGs((struct VarStruct *Var));
STATICforIDL CDFstatus UpdateConversions PROTOARGs((struct CDFstruct *CDF));
STATICforIDL CDFstatus UpdateNEWzMode PROTOARGs((struct CDFstruct *CDF));
STATICforIDL void InitNewVXR PROTOARGs((
  struct VXRstruct *VXR, Int32 firstRec, Int32 lastRec, Int32 offset
));
STATICforIDL CDFstatus UpdateVXRtailInVDR PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var
));
STATICforIDL CDFstatus Compress PROTOARGs((
  vFILE *iFp, Int32 iOffset, Int32 iSize, CDFstatus iError, Int32 cType,
  Int32 cParms[], vFILE *oFp, Int32 oOffset, Int32 *oSize, CDFstatus oError
));
STATICforIDL CDFstatus Decompress PROTOARGs((
  vFILE *iFp, Int32 iOffset, Int32 iSize, CDFstatus iError, Int32 cType,
  Int32 cParms[], vFILE *oFp, Int32 oOffset, CDFstatus oError
));
STATICforIDL CDFstatus DecompressToStage PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 offset, Int32 uSize
));
STATICforIDL CDFstatus FlushStage PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var
));
STATICforIDL char *ScratchDirectory PROTOARGs((struct CDFstruct *CDF));
STATICforIDL void AddTOvStats PROTOARGs((vSTATS *vStatsSum, vSTATS *vStats));
STATICforIDL CDFstatus ValidateCDF PROTOARGs((
  struct CDFstruct *CDF, vFILE *vFp, Int32 offset, Int32 fileSize, Logical debug
));
STATICforIDL void FillSpacesToString PROTOARGs((char *buffer, int totalBytes,
  int numElems
));
#if defined(__cplusplus)
  extern "C" {
#endif
VISIBLE_PREFIX Logical StrStrIgCaseX PROTOARGs((char *string1, char *string2));
VISIBLE_PREFIX Logical ValidCDFname PROTOARGs((char *name));
#if !defined(__CFM68K__) || defined(__USING_STATIC_LIBS__) || !defined(CFM68KDLL)
  VISIBLE_PREFIX CDFstatus UpdateDotCDF PROTOARGs((struct CDFstruct *CDF));
#endif
VISIBLE_PREFIX vFILE *V_open PROTOARGs((char *file_spec, char *a_mode));
VISIBLE_PREFIX vFILE *V_scratch PROTOARGs((char *directory, char *extension));
VISIBLE_PREFIX int V_setcache PROTOARGs((vFILE *vfp, int nCacheBuffers));
VISIBLE_PREFIX int V_seek PROTOARGs((vFILE *vfp, long offset, int direction));
VISIBLE_PREFIX long V_tell PROTOARGs((vFILE *vfp));
VISIBLE_PREFIX int V_eof PROTOARGs((vFILE *vfp));
VISIBLE_PREFIX int V_error PROTOARGs((vFILE *vfp));
VISIBLE_PREFIX size_t V_read PROTOARGs((
  void *buffer, size_t item_size, size_t n_items, vFILE *vfp
));
VISIBLE_PREFIX size_t V_write PROTOARGs((
  void *buffer, size_t item_size, size_t n_items, vFILE *vfp
));
VISIBLE_PREFIX int V_getc PROTOARGs((vFILE *fp));
VISIBLE_PREFIX int V_putc PROTOARGs((int value, vFILE *fp));
VISIBLE_PREFIX Logical FlushCache PROTOARGs((vFILE *vFp, vCACHE *firstCache));
#if !defined(__CFM68K__) || defined(__USING_STATIC_LIBS__) || !defined(CFM68KDLL)
  VISIBLE_PREFIX int V_flush PROTOARGs((vFILE *vfp));
#endif
VISIBLE_PREFIX int V_clear PROTOARGs((vFILE *vfp));
VISIBLE_PREFIX int V_close PROTOARGs((
  vFILE *vfp, struct CDFstruct *CDF, vSTATS *vStats
));
VISIBLE_PREFIX int V_delete PROTOARGs((vFILE *vFp, vSTATS *vStats));
VISIBLE_PREFIX int GetMyPID PROTOARGs((void));
VISIBLE_PREFIX void SetMyPID PROTOARGs((int pid));
VISIBLE_PREFIX CDFstatus ReadVXR VARPROTOARGs((vFILE *fp, Int32 offset, ...));
VISIBLE_PREFIX CDFstatus ReadIrType PROTOARGs((
  vFILE *fp, Int32 offset, Int32 *irType
));
VISIBLE_PREFIX Logical Read32 PROTOARGs((vFILE *fp, Int32 *value));
VISIBLE_PREFIX CDFstatus ConvertBuffer PROTOARGs((
  Int32 srcEncoding, Int32 dstEncoding, Logical negToPosFp0, Int32 dataType,
  Int32 numElems, void *srcbuffer, void *dstBuffer
));
VISIBLE_PREFIX Logical PriorTo PROTOARGs((
  char *spec, Int32 version, Int32 release, Int32 increment
));
#if defined(MSVC67)
VISIBLE_PREFIX long _ftol PROTOARGs((double dblSource));
VISIBLE_PREFIX long _ftol2 PROTOARGs((double dblSource));
#endif
#if defined(__cplusplus)
  }
#endif

#if defined(DEBUG)
  STATICforIDL void DisplayVs PROTOARGs((
    char *toWhere, char *label, vSTATS *vStats
));
#endif

#if defined(MICROSOFTC_700) && INCLUDEvMEMORY
  STATICforIDL MemHandle AllocateVMemory PROTOARGs((size_t nBytes));
  STATICforIDL void *LoadVMemory PROTOARGs((
    MemHandle handle, Logical writeFlag
));
  STATICforIDL int FreeVMemory PROTOARGs((MemHandle handle));
#endif

/*****************************************************************************/

#endif
