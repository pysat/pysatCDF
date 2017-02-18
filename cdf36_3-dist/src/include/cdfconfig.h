/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                          CDF software distribution configuration.
*
*  Version 1.5f, 14-Dec-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  25-Jan-94, J Love     Original version.
*   V1.0a  4-Feb-94, J Love     DEC Alpha/OpenVMS port.
*   V1.1   9-Dec-94, J Love     CDF V2.5.
*   V1.1a 27-Feb-95, J Love     Added `value' qualifier to CDFcompare.
*   V1.2  28-Mar-95, J Love     POSIX.
*   V1.2a 18-Apr-95, J Love     More POSIX.
*   V1.3  25-May-95, J Love     Setting for virtual memory usage under
*                               Microsoft C 7.00.  SOLARISbsdDIRUTILSbug.
*   V1.4  13-Sep-95, J Love     Added CDFexport.  More virtual memory under
*                               Microsoft C 7.00.  Reorganized control keys
*                               for FSI toolkit programs.
*   V1.5   9-Sep-96, J Love     CDF V2.6.
*   V1.5a 20-Dec-96, J Love	Added `simple' and `batch' modes to CDFexport.
*				Turned off support for RICE compression until
*				permission to distribute the source code is
*				received.
*   V1.5b 15-Jan-97, J Love	More changes for simple and batch modes in
*				CDFexport.
*   V1.5c 21-Feb-97, J Love	Removed RICE.
*   V1.5d 28-Feb-97, J Love	Windows NT for MS Visual C/C++ on an IBM PC.
*   V1.5e  2-Sep-97, J Love	Changed DEFAULTsimpleEXPORT to 1/0.
*   V1.5f 14-Dec-97, J Love	Added ALPHAVMSi encoding.
*   V1.6  11-Jul-05, M Liu      Added MingW port for PC.
*   V3.2  26-Apr-07, D Berger   Increased number of cache buffer definitions for
*                               WIN32 to duplicate those defined for UNIX.
*
******************************************************************************/

#if !defined(CONFIGh_INCLUDEd__)
#define CONFIGh_INCLUDEd__

/******************************************************************************
* This file contains the configurable parameters of the CDF distribution.
* Any modifications must be made before the CDF distribution is built.  This
* file is `included' by the CDF library and toolkit when they are compiled
* and linked.  If you are unsure of how to change one of these defaults
* contact CDFsupport.
******************************************************************************/

/******************************************************************************
* Supported compressions.  1 = yes, 0 = no.
******************************************************************************/

#define SUPPORT_RLE		1
#define SUPPORT_HUFF		1
#define SUPPORT_AHUFF		1

#if defined(dos)
#  define SUPPORT_GZIP		0
#else
#  define SUPPORT_GZIP		1
#endif

#define SUPPORT_RLE64           1
#define SUPPORT_HUFF64          1
#define SUPPORT_AHUFF64         1
#define SUPPORT_GZIP64          1

/******************************************************************************
* Supported checksums.  1 = yes, 0 = no.
******************************************************************************/

#define SUPPORT_MD5             1

/*****************************************************************************
* Default number of cache buffers for the various file types.  These default
* sizes are generally optimized for physically sequential access of variable
* data values (whether using single, hyper, or sequential reads/writes).  Use
* caution when raising these values on IBM PC and Macintosh systems where
* memory is limited.
*
* Each value is defined as follows...
*
*   NUMcacheSINGLE
*       The minimum number of cache buffers for the dotCDF file of an
*	uncompressed single-file CDF or for the "working" dotCDF file
*	of a compressed single-file CDF.
*
*   NUMcacheMULTI
*       The number of cache buffers for the dotCDF file in a multi-file CDF.
*
*   NUMcacheVAR
*       The number of cache buffers for each variable file in a multi-file CDF.
*
*   NUMcacheSTAGE
*	The number of cache buffers for the variable staging file.
*
*   NUMcacheCOMPRESS
*	The number of cache buffers for the compression scratch file.
*
* Note that these are only defaults - an application should set the number
* of cache buffers for a file with the Internal Interface to fully optimize
* access to that file.
*
*****************************************************************************/

#if defined(vms) || defined(unix) || defined(posixSHELL)
#  define NUMcacheSINGLE	300
#  define NUMcacheMULTI		40
#  define NUMcacheVAR		1
#  define NUMcacheSTAGE		125
#  define NUMcacheCOMPRESS	80
#endif

#if defined(win32) 
#  define NUMcacheSINGLE	300
#  define NUMcacheMULTI		40
#  define NUMcacheVAR		1
#  define NUMcacheSTAGE		125
#  define NUMcacheCOMPRESS	80
#endif

#if defined(mac)
#  define NUMcacheSINGLE	32
#  define NUMcacheMULTI		12
#  define NUMcacheVAR		1
#  define NUMcacheSTAGE		32
#  define NUMcacheCOMPRESS	20
#endif

#if defined(dos)
#  define NUMcacheSINGLE	16
#  define NUMcacheMULTI		8
#  define NUMcacheVAR		1
#  define NUMcacheSTAGE		16
#  define NUMcacheCOMPRESS	10
#endif

/*****************************************************************************
* Blocking factors.
*****************************************************************************/
#if defined(win32)

#  define MIN_BLOCKING_BYTES_standard     512
#  define MIN_BLOCKING_RECS_standard      1

#  define MIN_BLOCKING_BYTES_sparse       2048
#  define MIN_BLOCKING_RECS_sparse        1

#  define MIN_BLOCKING_BYTES_compressed   65536
#  define MIN_BLOCKING_RECS_compressed    1
#else
#  if defined(linux) || defined(__i386) || defined(__x86_64)
#    define MIN_BLOCKING_BYTES_standard     8192
#    define MIN_BLOCKING_RECS_standard      1

#    define MIN_BLOCKING_BYTES_sparse       16384
#    define MIN_BLOCKING_RECS_sparse        1

#    define MIN_BLOCKING_BYTES_compressed   65536
#    define MIN_BLOCKING_RECS_compressed    1
#  else
#    define MIN_BLOCKING_BYTES_standard     1024
#    define MIN_BLOCKING_RECS_standard      1

#    define MIN_BLOCKING_BYTES_sparse       4096
#    define MIN_BLOCKING_RECS_sparse        1

#    define MIN_BLOCKING_BYTES_compressed   65536
#    define MIN_BLOCKING_RECS_compressed    1
#  endif
#endif
/*****************************************************************************
* Should trailing blanks be automatically stripped from CDF pathnames when
* creating or opening a CDF?  1 = yes, 0 = no.
*****************************************************************************/

#define STRIP_TRAILING_BLANKS_FROM_CDFPATH      1       /* 1 = yes, 0 = no. */

/*****************************************************************************
* Should a read-only distribution be built with which CDFs can only be read,
* not created or modified.
*****************************************************************************/

#define BUILD_READ_ONLY_DISTRIBUTION    0               /* 1 = yes, 0 = no. */

/*****************************************************************************
* Should the explanation text for CDF status codes be built into the CDF
* library (which increases its size).
*****************************************************************************/

#define INCLUDE_STATUS_TEXT             1               /* 1 = yes, 0 = no. */

/*****************************************************************************
* Should the default encoding for CDFs created with the Internal Interface be
* HOST_ENCODING (the alternative is IBMPC_ENCODING)?
*****************************************************************************/

#define DEFAULT_TO_HOST_ENCODING        TRUE     /* TRUE = yes, FALSE = no. */

/*****************************************************************************
* Should the default majority for CDFs created with the Internal Interface be
* ROW_MAJOR (the alternate is COLUMN_MAJOR)?
*****************************************************************************/

#define DEFAULT_TO_ROW_MAJOR            TRUE    /* TRUE = yes, FALSE = no. */

/*****************************************************************************
* Should the default format for CDFs created with either the Internal or
* Standard Interface be SINGLE_FILE (the alternative is MULTI_FILE)?
*****************************************************************************/

#define DEFAULT_TO_SINGLE_FILE          TRUE    /* TRUE = yes, FALSE = no. */

/*****************************************************************************
* Virtual memory defaults under Microsoft 7.00.
*****************************************************************************/

#if defined(MICROSOFTC_700)
#  define INCLUDEvMEMORY          1
#  if INCLUDEvMEMORY
#    define DEFAULT_USEvMEM         FALSE
#    define DEFAULT_vMEMSIZE        2048
#    define DEFAULT_vMEMMASK        (_VM_EMS | _VM_XMS)
#  endif
#endif

/*****************************************************************************
* Does a bug exist in the BSD system library involving the directory
* utilities on a Solaris system?  This bug existed in Solaris 2.2 and 2.3
* but has apparently been fixed in Solaris 2.4.  If the first two characters
* of file names are absent, then this bug exists on your system.
*****************************************************************************/

#define SOLARISbsdDIRUTILSbug           0               /* 1 = yes, 0 = no. */

/*****************************************************************************
* The default pad values for each data type.
*****************************************************************************/

#define DEFAULT_BYTE_PADVALUE           (-127)
#define DEFAULT_INT1_PADVALUE           (-127)
#define DEFAULT_UINT1_PADVALUE          ((unsigned char)254)
#define DEFAULT_INT2_PADVALUE           ((short)-32767)
#define DEFAULT_UINT2_PADVALUE          ((unsigned short)65534)
#define DEFAULT_INT4_PADVALUE           (-2147483647L)
#define DEFAULT_INT8_PADVALUE           ((long long)-9223372036854775807LL)
#define DEFAULT_UINT4_PADVALUE          (4294967294U)
#define DEFAULT_REAL4_PADVALUE          ((float)-1.0E30f)
#define DEFAULT_FLOAT_PADVALUE          ((float)-1.0E30f)
#define DEFAULT_REAL8_PADVALUE          -1.0E30
#define DEFAULT_DOUBLE_PADVALUE         -1.0E30
#define DEFAULT_CHAR_PADVALUE           ' '
#define DEFAULT_UCHAR_PADVALUE          ' '
#define DEFAULT_EPOCH_PADVALUE          0.0
#define DEFAULT_EPOCH16_PADVALUE        0.0
#define DEFAULT_TT2000_PADVALUE         ((long long)-9223372036854775807LL)
                                        /* 0000-01-01T00:00:00.000000000 */
/*****************************************************************************
* By default, convert -0.0 to 0.0 when read from or written to a CDF by the
* CDF library?  (Values already existing in the CDF are not modified.)  TRUE
* = yes, FALSE = no.  This default can be overridden on the applicable toolkit
* command lines.  -0.0 is an illegal floating-point value on VAXes and DEC
* Alphas running OpenVMS (with the exception of IEEE_FLOAT).  Programs will
* crash on those machines if -0.0 is used in arithmetic operations or is
* encoded for display.  -0.0 is legal on UNIX machines, the IBM PC, and the
* Macintosh.
*****************************************************************************/

#if defined(vax) || defined(alphavmsD) || defined(alphavmsG)
#  define DEFAULT_NEGtoPOSfp0             TRUE
#else
#  define DEFAULT_NEGtoPOSfp0             FALSE
#endif

/******************************************************************************
* Status reporting defaults for toolkit programs.  These defaults can be
* overridden on the command line when a toolkit program is executed.  TRUE
* means report, FALSE means do not report.
******************************************************************************/

#define REPORTerrorsDEFAULT     TRUE
#define REPORTwarningsDEFAULT   TRUE
#define REPORTinfosDEFAULT      FALSE

/******************************************************************************
* Command line and menu defaults for CDFexport.
* These defaults can be overridden on the command line when CDFexport is
* executed.  For the [no]xxxxx-style qualifiers TRUE means `xxxxx' and FALSE
* means `noxxxxx'.
******************************************************************************/

#define DEFAULTsimpleEXPORT		0	/* 1 = yes, 0 = no. */

#define DEFAULTpromptEXPORT             FALSE   /* [no]prompt */
#define DEFAULTstatsEXPORT              FALSE   /* [no]statistics */
#define DEFAULTzModeEXPORT              zMODEoff  /* zMODEoff = zMode/off,
						     zMODEon1 = zMode/1,
						     zMODEon2 = zMode/2. */
#define DEFAULTeachFilterEXPORT         FALSE   /* [no]filter */
#define DEFAULTfillsEXPORT              TRUE    /* [no]fills */
#define DEFAULTformatEXPORT             TRUE    /* [no]format */
#define DEFAULTfillvalEXPORT            TRUE    /* [no]fillval */
#define DEFAULTvalidminEXPORT           TRUE    /* [no]validmin */
#define DEFAULTvalidmaxEXPORT           TRUE    /* [no]validmax */
#define DEFAULTmonotonEXPORT            TRUE    /* [no]monoton */
#define DEFAULTrecordEXPORT             TRUE    /* [no]record */
#define DEFAULTindicesEXPORT            TRUE    /* [no]indices */
#define DEFAULTexclusiveEXPORT          FALSE   /* [no]exclusive */
#define DEFAULToutputEXPORT             TRUE    /* [no]output */
#define DEFAULTdeleteEXPORT             FALSE   /* [no]delete */
#define DEFAULTpreAllocateEXPORT        TRUE    /* [no]preallocate */
#define DEFAULTheadingEXPORT		TRUE	/* [no]heading */
#define DEFAULTsingleEXPORT             TRUE    /* TRUE = "single",
						   FALSE = "multi". */
#define DEFAULTnetworkEXPORT            FALSE   /* TRUE = "network",
						   FALSE = "host". */
#define DEFAULTepochEXPORT              0       /* 0 (standard), 1, 2, 3, 4
						   (C/Fortran format), 5
						   (custom format), or 6
                                                   (ISO 8601). */
#define DEFAULThorizontalEXPORT         FALSE   /* TRUE = "horizontal",
						   FALSE = "vertical". */
#define DEFAULTmajorityEXPORT           0       /* ROW_MAJOR, COLUMN_MAJOR, or
						   0 (same as input CDF). */

#define DEFAULTshowEXPORT               FALSE   /* Initially show filtered
						   listing lines? */
#define DEFAULTspacingEXPORT            1       /* Spacing between items in a
						   listing. */
#define DEFAULToverallFilterEXPORT      TRUE    /* Overall filtering initially
						   enabled? */

#define DEFAULTpromptSIMPLE		TRUE	/* Don't change these... */
#define DEFAULTformatSIMPLE		TRUE
#define DEFAULThorizontalSIMPLE		TRUE
#define DEFAULTrecordSIMPLE		FALSE
#define DEFAULTindicesSIMPLE		FALSE
#define DEFAULToutputSIMPLE		FALSE
#define DEFAULTepochSIMPLE		0
#define DEFAULTmajoritySIMPLE		0
#define DEFAULTspacingSIMPLE		1
#define DEFAULTheadingSIMPLE		TRUE
#define DEFAULTzModeSIMPLE		zMODEon2
#define DEFAULTcdaweb			FALSE
#define DEFAULTvirtual                  FALSE

/******************************************************************************
* Command line defaults for CDFedit.
* These defaults can be overridden on the command line when CDFedit is
* executed.  For the [no]xxxxx-style qualifiers TRUE means `xxxxx' and FALSE
* means `noxxxxx'.
******************************************************************************/

#define DEFAULTbrowseEDIT       FALSE           /* [no]browse */
#define DEFAULTformatEDIT       TRUE            /* [no]format */
#define DEFAULTpromptEDIT       FALSE           /* [no]prompt */
#define DEFAULTstatsEDIT        FALSE           /* [no]statistics */
#define DEFAULTzModeEDIT        zMODEoff        /* zMODEoff = zMode/0,
						   zMODEon1 = zMode/1,
						   zMODEon2 = zMode/2 */
#define DEFAULTgWithEDIT        FALSE           /* [no]gwithentries */
#define DEFAULTvWithEDIT        FALSE           /* [no]vwithentries */

/******************************************************************************
* Command line defaults for CDFconvert.
* These defaults can be overridden on the command line when CDFconvert is
* executed.  For the [no]xxxxx-style qualifiers TRUE means `xxxxx' and FALSE
* means `noxxxxx'.
******************************************************************************/

#define DEFAULTlogCVT           FALSE           /* [no]log */
#define DEFAULTpctCVT           FALSE           /* [no]percent */
#define DEFAULTdelCVT           FALSE           /* [no]delete */
#define DEFAULTpageCVT          FALSE           /* [no]page */
#define DEFAULTstatsCVT         FALSE           /* [no]statistics */
#define DEFAULTzModeCVT         zMODEoff        /* zMODEoff = zMode/0,
						   zMODEon1 = zMode/1,
						   zMODEon2 = zMode/2 */
#define DEFAULTchecksumCVT          0           /* no checksum */

/******************************************************************************
* Command line defaults for CDFinquire.
* These defaults can be overridden on the command line when CDFinquire is
* executed.  For the [no]xxxxx-style qualifiers TRUE means `xxxxx' and FALSE
* means `noxxxxx'.
******************************************************************************/

#define DEFAULTpageINQ          FALSE           /* [no]page */
#define DEFAULTidINQ            TRUE            /* TRUE if /ID,-id specified.*/

/******************************************************************************
* Command line defaults for CDFirsdump.
* These defaults can be overridden on the command line when CDFirsdump is
* executed.  For the [no]xxxxx-style qualifiers TRUE means `xxxxx' and FALSE
* means `noxxxxx'.
******************************************************************************/

#define DEFAULTpageIRsDUMP         FALSE        /* [no]page */
#define DEFAULTsizesIRsDUMP        FALSE        /* [no]sizes */
#define DEFAULTindexingIRsDUMP     FALSE        /* [no]indexing */
#define DEFAULTsummaryIRsDUMP      TRUE         /* [no]summary */
#define DEFAULTradixIRsDUMP	   10		/* 10 = decimal (file offsets),
						   16 = hexadecimal. */
#define DEFAULTlevelIRsDUMP        1            /* 1 == "brief",
						   2 == "most",
						   3 == "full" */
#define DEFAULTdataIRsDUMP	   FALSE	/* data. */

/******************************************************************************
* Command line defaults for CDFcompare.
* These defaults can be overridden on the command line when CDFcompare is
* executed.  For the [no]xxxxx-style qualifiers TRUE means `xxxxx' and FALSE
* means `noxxxxx'.
******************************************************************************/

#define DEFAULTpageCMP          FALSE           /* [no]page */
#define DEFAULTlogCMP           FALSE           /* [no]log */
#define DEFAULTattrCMP          TRUE            /* [no]attr */
#define DEFAULTvarCMP           TRUE            /* [no]var */
#define DEFAULTnumberCMP        TRUE            /* [no]number */
#define DEFAULTpctCMP           FALSE           /* [no]percent */
#define DEFAULTetcCMP           TRUE            /* [no]etc */
#define DEFAULTlocationCMP      FALSE           /* [no]location */
#define DEFAULTvalueCMP         FALSE           /* [no]value */
#define DEFAULTformatCMP        TRUE            /* [no]format */
#define DEFAULTstatsCMP         FALSE           /* [no]statistics */
#define DEFAULTzModeCMP         zMODEoff        /* zMODEoff = zMode/0,
						   zMODEon1 = zMode/1,
						   zMODEon2 = zMode/2.
						   This default applies to
						   both CDFs being compared. */

/******************************************************************************
* Command line defaults for CDFstats.
* These defaults can be overridden on the command line when CDFstats is
* executed.  For the [no]xxxxx-style qualifiers TRUE means `xxxxx' and FALSE
* means `noxxxxx'.
******************************************************************************/

#define DEFAULTrangeSTATS       TRUE            /* [no]range */
#define DEFAULTfillSTATS        TRUE            /* [no]fill */
#define DEFAULTupValidsSTATS    FALSE           /* [no]update_valids */
#define DEFAULTupScalesSTATS    FALSE           /* [no]update_scales */
#define DEFAULTupMonoSTATS      FALSE           /* [no]update_monotonic */
#define DEFAULTpageSTATS        FALSE           /* [no]page */
#define DEFAULTformatSTATS      TRUE            /* [no]format */
#define DEFAULTstatsSTATS	FALSE		/* [no]statistics */
#define DEFAULTzModeSTATS       zMODEoff        /* zMODEoff = zMode/0,
						   zMODEon1 = zMode/1,
						   zMODEon2 = zMode/2 */

/******************************************************************************
* Command line defaults for SkeletonTable.
* These defaults can be overridden on the command line when SkeletonTable
* is executed.  For the [no]xxxxx-style qualifiers TRUE means `xxxxx' and FALSE
* means `noxxxxx'.
******************************************************************************/

#define DEFAULTvaluesCDF2SKT    NRVvalues       /* NOvalues = no variables,
						   NRVvalues = NRV variables,
						   RVvalues = RV variables,
						   ALLvalues = all variables,
						   NAMEDvalues = named
								 variables. */
#define DEFAULTlogCDF2SKT       FALSE           /* [no]log */
#define DEFAULTformatCDF2SKT    TRUE            /* [no]format */
#define DEFAULTzModeCDF2SKT     zMODEoff        /* zMODEoff = zMode/0,
						   zMODEon1 = zMode/1,
						   zMODEon2 = zMode/2 */
#define DEFAULTscreenCDF2SKT    FALSE           /* TRUE = to screen, FALSE =
						   to skeleton table file. */
#define DEFAULTpageCDF2SKT      FALSE           /* [no]page */
#define DEFAULTstatsCDF2SKT	FALSE		/* [no]statistics */

/******************************************************************************
* Command line defaults for SkeletonCDF.
* These defaults can be overridden on the command line when SkeletonCDF is
* executed.  For the [no]xxxxx-style qualifiers TRUE means `xxxxx' and FALSE
* means `noxxxxx'.
******************************************************************************/

#define DEFAULTlogSKT2CDF       FALSE           /* [no]log */
#define DEFAULTdeleteSKT2CDF    FALSE           /* [no]delete */
#define DEFAULTfillvalSKT2CDF   FALSE           /* [no]fillval */
#define DEFAULTstatsSKT2CDF	FALSE		/* [no]statistics */
#define DEFAULTwarningSKT2CDF	TRUE		/* [no]warning */
#define DEFAULTzModeSKT2CDF     zMODEoff        /* zMODEoff = zMode/0,
						   zMODEon1 = zMode/1,
						   zMODEon2 = zMode/2 */

/******************************************************************************
* Command line defaults for CDFdump.
* These defaults can be overridden on the command line when CDFdump is
* executed.  For the [no]xxxxx-style qualifiers TRUE means `xxxxx' and FALSE
* means `noxxxxx'.
******************************************************************************/

#define DEFAULTformatDUMP       TRUE            /* [no]format */
#define DEFAULTheaderDUMP       TRUE            /* [no]header */
#define DEFAULTcol2rowDUMP      FALSE           /* col2row    */
#define DEFAULTepochVVDUMP      FALSE           /* epochVV    */

/******************************************************************************
* Command line defaults for CDFmerge.
* These defaults can be overridden on the command line when CDFmerge is
* executed.  For the [no]xxxxx-style qualifiers TRUE means `xxxxx' and FALSE
* means `noxxxxx'.
******************************************************************************/

#define DEFAULTlogMERGE         FALSE           /* [no]log */
#define DEFAULTdataonlyMERGE    FALSE           /* [no]dataonly */
#define DEFAULTcdawebMERGE      FALSE           /* [no]cdaweb */
#define DEFAULTaugmentlabelMERGE   FALSE        /* [no]augment_label */

/******************************************************************************
* Command line defaults for CDFleapsecondsinfo.
* These defaults can be overridden on the command line when CDFleaptableinfo
* is executed.  
******************************************************************************/

#define DEFAULTdumpLEAPTABLE    FALSE           /* TRUE if /DUMP, -dump 
                                                   specified. */

/******************************************************************************
* `Setup' menu defaults for CDFlist.
* These defaults can be overridden on the command line when CDFlist is
* executed.  For the [no]xxxxx-style qualifiers TRUE means `xxxxx' and FALSE
* means `noxxxxx'.
******************************************************************************/

#define DEFAULTformatLIST       TRUE            /* [no]format */
#define DEFAULTfilterLIST       TRUE            /* [no]filter */
#define DEFAULTfillLIST         TRUE            /* [no]fill */
#define DEFAULTeraseLIST        TRUE            /* [no]erase */
#define DEFAULTsingleLIST       TRUE            /* TRUE == "single",
						   FALSE == "multi". */
#define DEFAULTnetworkLIST      FALSE           /* TRUE == "network",
						   FALSE == "host". */
#define DEFAULTepochLIST        TRUE            /* TRUE == "epoch",
						   FALSE == "epoch1". */
#define DEFAULThorizontalLIST   TRUE            /* TRUE == "horizontal",
						   FALSE == "vertical". */
#define DEFAULTzModeLIST        FALSE           /* TRUE == "zmode" present,
						   FALSE == "zmode" absent. */

/******************************************************************************
* `Setup' menu defaults for CDFwalk.
* These defaults can be overridden on the command line when CDFwalk is
* executed.  For the [no]xxxxx-style qualifiers TRUE means `xxxxx' and FALSE
* means `noxxxxx'.
******************************************************************************/

#define DEFAULTformatWALK       TRUE            /* [no]format */

/******************************************************************************
* `Setup' menu defaults for CDFvalidate.
* These defaults can be overridden on the command line when CDFvalidate is
* executed.  For the [no]xxxxx-style qualifiers TRUE means `xxxxx' and FALSE
* means `noxxxxx'.
******************************************************************************/

#define DEFAULTvalidateValidate TRUE            /* [no]validate */
#define DEFAULTdebugValidate    FALSE           /* [no]debug */
#define DEFAULTquietValidate    TRUE            /* [no]quiet */

/******************************************************************************
* Key definitions for the toolkit program using a full-screen interface.
* These are currently CDFedit, CDFexport, CDFlist, and CDFwalk.  The key
* definitions are appended in such a way as to identify the program(s) to
* which they apply...
*
*       _FSI            Applies to all of the programs.
*       _EDIT           Applies to CDFedit.
*       _EXPORT         Applies to CDFexport.
*       _LIST           Applies to CDFlist.
*       _WALK           Applies to CDFwalk.
*
* These values would be changed, for instance, if problems were encountered
* with a terminal server intercepting (and interpreting) control characters.
* These values can be found in the include file `windoz.h'.
*
* The following keys should not be used because of various systems interpreting
* them in special ways (VMS being the most guilty).
*
*   Ctrl-C, Ctrl-Y, Ctrl-Z      Interrupt/abort/exit on various systems.
*   Ctrl-O                      Discards output on VMS systems.
*   Ctrl-Q, Ctrl-S              XON/XOFF protocol on some systems.
*   Ctrl-T                      Displays time/status on VMS systems.
*   Ctrl-X                      Discards input on VMS systems.
*   Ctrl-H                      `Delete' on some systems.
*
******************************************************************************/

#define ENTERkey_FSI                    KB_RETURN
#define EXITkey_FSI                     KB_CTRL_E
#define NSkey_FSI                       KB_CTRL_N
#define PSkey_FSI                       KB_CTRL_P
#define REFRESHkey_FSI                  KB_CTRL_W
#define HELPkey_FSI                     KB_CTRL_K
#define DELETEkey_FSI                   KB_CTRL_D
#define INSERTorOVERkey_FSI             KB_CTRL_A
#define SOLkey_FSI                      KB_CTRL_F
#define EOLkey_FSI                      KB_CTRL_L

#define DELETECDFkey_EDIT               DELETEkey_FSI
#define DELETEATTRorENTRYkey_EDIT       DELETEkey_FSI
#define DELETEVARkey_EDIT               DELETEkey_FSI
#define CREATECDFkey_EDIT               KB_CTRL_A
#define CREATEVARkey_EDIT               KB_CTRL_A
#define CREATEATTRkey_EDIT              KB_CTRL_A
#define CREATEENTRYkey_EDIT             KB_CTRL_B
#define NEXTFIELDkey_EDIT               KB_RIGHTARROW
#define PREVFIELDkey_EDIT               KB_LEFTARROW
#define INCREMENTkey_EDIT               KB_UPARROW
#define DECREMENTkey_EDIT               KB_DOWNARROW
#define TOGGLESCOPEkey_EDIT             KB_CTRL_G
#define rENTRYbyNUMBERkey_EDIT          KB_CTRL_A
#define zENTRYbyNUMBERkey_EDIT          KB_CTRL_B
#define zMODE0key_EDIT                  KB_0
#define zMODE1key_EDIT                  KB_1
#define zMODE2key_EDIT                  KB_2
#define INFOkey_EDIT                    KB_CTRL_F
#define NEXTVARkey_EDIT                 KB_CTRL_J
#define NEXTATTRkey_EDIT                KB_CTRL_J
#define UPDATEkey_EDIT                  KB_CTRL_U
#define MOREHELPkey_EDIT                KB_CTRL_L

#define ABORTkey_EXPORT                 KB_CTRL_E
#define ACTIONSkey_EXPORT               KB_CTRL_A
#define OPTIONSkey_EXPORT               KB_CTRL_B
#define CRUISEkey_EXPORT                KB_CTRL_G
#define FLIPkey_EXPORT                  KB_CTRL_R
#define PART1key_EXPORT                 KB_TAB
#define PART2key_EXPORT                 KB_TAB
#define PART3key_EXPORT                 KB_TAB
#define PART4key_EXPORT                 KB_TAB
#define NEXTFIELDkey_EXPORT             KB_RIGHTARROW
#define PREVFIELDkey_EXPORT             KB_LEFTARROW
#define INCREMENTkey_EXPORT             KB_UPARROW
#define DECREMENTkey_EXPORT             KB_DOWNARROW
#define SWITCHkey_EXPORT		KB_TAB

#define SELECTkey_LIST                  KB_RETURN
#define NEXTFIELDkey_LIST               KB_RETURN
#define PREVFIELDkey_LIST               KB_CTRL_B
#define CONTINUEkey_LIST                KB_RETURN
#define HELPDONEkey_LIST                KB_RETURN
#define ACTIONkey_LIST                  KB_CTRL_F
#define INSERTTOGGLEkey_LIST            KB_CTRL_A
#define SOLkey_LIST                     KB_CTRL_H
#define EOLkey_LIST                     KB_CTRL_E
#define DELENDkey_LIST                  KB_CTRL_D
#define NEXTITEMkey_LIST                KB_DOWNARROW
#define PREVITEMkey_LIST                KB_UPARROW
#define NEXTVARkey_LIST                 KB_DOWNARROW
#define PREVVARkey_LIST                 KB_UPARROW
#define DELETEVARkey_LIST               KB_DELETE
#define POPUPkey_LIST                   KB_CTRL_P
#define QUITkey_LIST                    KB_q
#define PAGEDOWNkey_LIST                KB_d
#define PAGEUPkey_LIST                  KB_u
#define FILTERTOGGLEkey_LIST            KB_f
#define OUTPUTkey_LIST                  KB_y
#define NOOUTPUTkey_LIST                KB_n
#define MONOTONINCkey_LIST              KB_PLUS
#define MONOTONDECkey_LIST              KB_MINUS
#define MONOTONFALSEkey_LIST            KB_f

#define SELECTkey_WALK                  KB_RETURN
#define NEXTFIELDkey_WALK               KB_RETURN
#define PREVFIELDkey_WALK               KB_CTRL_B
#define CONTINUEkey_WALK                KB_RETURN
#define HELPDONEkey_WALK                KB_RETURN
#define ACTIONkey_WALK                  KB_CTRL_F
#define INSERTTOGGLEkey_WALK            KB_CTRL_A
#define MOVEtoBEGINkey_WALK             KB_CTRL_H
#define MOVEtoENDkey_WALK               KB_CTRL_E
#define DELETEtoENDkey_WALK             KB_CTRL_D
#define NEXTITEMkey_WALK                KB_DOWNARROW
#define PREVITEMkey_WALK                KB_UPARROW
#define NEXTRECDIMkey_WALK              KB_DOWNARROW
#define PREVRECDIMkey_WALK              KB_UPARROW
#define NEXTVARkey_WALK                 KB_DOWNARROW
#define PREVVARkey_WALK                 KB_UPARROW
#define NEXTRECORDkey_WALK              KB_RIGHTARROW
#define PREVRECORDkey_WALK              KB_LEFTARROW
#define POPUPkey_WALK                   KB_CTRL_P
#define QUITkey_WALK                    KB_q
#define PAGEDOWNkey_WALK                KB_d
#define PAGEUPkey_WALK                  KB_u
#define NEWVARkey_WALK                  KB_v
#define INPUTkey_WALK                   KB_i

/*****************************************************************************/

#endif
