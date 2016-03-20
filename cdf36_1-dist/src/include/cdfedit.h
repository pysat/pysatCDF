/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF	                                    Header file for CDFedit.
*
*  Version 1.4b, 17-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  24-Jan-94, J Love     Original version.
*   V1.0a 30-Mar-94, J Love     Solaris using Gnu C compiler.
*   V1.1  15-Dec-94, J Love     CDF V2.5.
*   V1.2  23-Jan-95, J Love     IRIX 6.x (64-bit).
*   V1.3   3-Apr-95, J Love     POSIX.
*   V1.3a 20-Jul-95, J Love     CDFexport-related changes.
*   V1.4  30-Sep-96, J Love     CDF V2.6.
*   V1.4a 21-Feb-97, J Love	Removed RICE.
*   V1.4b 17-Nov-97, J Love	Windows NT/Visual C++.
*
******************************************************************************/

#if !defined(CDFEDITh_INCLUDEd__)
#define CDFEDITh_INCLUDEd__

/******************************************************************************
* Include files.
******************************************************************************/

#include "cdflib.h"
#include "widgets.h"

#if defined(mac)
#  include "cdfedit.rh"
#endif

/******************************************************************************
* Settings.
******************************************************************************/

#define MAXREC_LIMIT	0

/******************************************************************************
* Constants.
******************************************************************************/

#define ONLINE_HELP_WIDTH               78      /* Doesn't include borders. */

#define CDFs_NAME_LEN                   53
#define MAIN_MENU_1st_THIRD_LEN         20
#define MAIN_MENU_2nd_THIRD_LEN         47
#define VAR_MORE_MENU_1st_HALF_LEN      34
#define VARNAME_FIELD_LEN               16
#define ATTRNAME_FIELD_LEN              16
#define ATTRNAME_bFIELD_LEN             40	/* Browsing. */
#define ATTRNAME_eFIELD_LEN             44	/* Editing. */
#define DATASPEC_FIELD_LEN              9
#define DIMENSIONS_FIELD_LEN            16
#define VARIANCES_FIELD_LEN             7
#define ENTRYNUM_FIELD_LEN              6
#define gAttrENTRYVALUE_FIELD_LEN       44
#define vAttrENTRYVALUE_FIELD_LEN       34
#define varENTRYVALUE_FIELD_LEN         51

#define VARNAME_FIELD_BLANKS            1
#define DIMENSIONS_FIELD_BLANKS         1
#define VARIANCES_FIELD_BLANKS          1
#define ATTRNAME_FIELD_BLANKS           1
#define ENTRYNUM_FIELD_BLANKS           1
#define DATASPEC_FIELD_BLANKS           1

#define MAX_RECORD_NUMBER_LEN           15
#define MAX_DIMENSION_INDEX_LEN         15
#define MAX_NUMELEMS_LEN                15
#define MAX_ENTRYNUM_LEN                15
#define MAX_DIMENSIONALITY_LEN          50
#define MAX_ENTRYSTRING_LEN             1000
#define MAX_VARIANCES_LEN               (1 + 1 + CDF_MAX_DIMS)

#define DELIMed_ATTR_NAME_LEN           (1 + CDF_ATTR_NAME_LEN256 + 1)
#define DELIMed_VAR_NAME_LEN            (1 + CDF_VAR_NAME_LEN256 + 1)

#define ENTRYNUM_FIELD_LENx             6
#define ENTRYNUM_FIELD_BLANKSx          1
#define VARNAME_FIELD_LENx              16
#define VARNAME_FIELD_BLANKSx           1
#define DATASPEC_FIELD_LENx             9
#define DATASPEC_FIELD_BLANKSx          1
#define gAttrENTRYVALUE_FIELD_LENx      61
#define vAttrENTRYVALUE_FIELD_LENx      51
#define ENTRIES_COLUMNx                 69

#define MANY_VARs                       30
#define MANY_ENTRYs                     30
#define MANY_ATTRs_AND_ENTRYs           150

#define FORCDF                          1
#define FORzVAR                         2
#define FORrVAR                         3

#define NO_DATATYPE                     (-1)
#define nCOMPRESSIONs                   5

#define MAX_ILHFILE_LEN			12	/* 8.3 because of DOS. */

/******************************************************************************
* Item constants.
******************************************************************************/

#define EDITzVarsIN             0
#define EDITrVarsIN             3
#define EDITgAttrsIN            5
#define EDITvAttrsIN            7
#define CHANGEcompressionIN     1
#define CHANGEchecksumIN        2
#define CHANGEencodingIN        4
#define CHANGEmajorityIN        6
#define CHANGEformatIN          8

#define BROWSEinOFFSET          9
#define BROWSEzVarsIN           (0 + BROWSEinOFFSET)
#define BROWSErVarsIN           (1 + BROWSEinOFFSET)
#define BROWSEgAttrsIN          (2 + BROWSEinOFFSET)
#define BROWSEvAttrsIN          (3 + BROWSEinOFFSET)

#define VIEWorMODIFYpadIN       0
#define MODIFYblockingIN        1
#define MODIFYsparsenessIN      2
#define ALLOCATErecordsIN       3
#define INITIALrecordsIN        4
#define MODIFYcompressionIN     5
#define DELETErecords1IN	6
#define DELETErecords2IN	7

#define NOcompressionIN		0
#define RLE0compressionIN	1
#define HUFF0compressionIN	2
#define AHUFF0compressionIN	3
#define GZIPcompressionIN	4

/******************************************************************************
* QOP constants.
******************************************************************************/

#define CDFSPECparm             0

#define BROWSEqual              0
#define NOBROWSEqual            1
#define ZMODEqual               2
#define FORMATqual              3
#define NOFORMATqual            4
#define PROMPTqual              5
#define NOPROMPTqual            6
#define REPORTqual              7
#define NEG2POSFP0qual          8
#define NONEG2POSFP0qual        9
#define CACHEqual               10
#define STATSqual               11
#define NOSTATSqual             12
#define gWITHqual               13
#define NOgWITHqual             14
#define vWITHqual               15
#define NOvWITHqual             16
#define ABOUTqual		17

/******************************************************************************
* Help Identifiers.
******************************************************************************/

#define CDFShelpID              0
#define CDFhelpID               1
#define GATTRShelpID            2
#define VATTRShelpID            3
#define ENTRIEShelpID           4
#define ZVARShelpID             5
#define RVARShelpID             6
#define CDFNAMEhelpID           7
#define RDIMhelpID              8
#define FORMAThelpID            9
#define ENCODINGhelpID          10
#define MAJORITYhelpID          11
#define SPEChelpID              12
#define DELETECDFhelpID         13
#define CREATECDFhelpID         14
#define ATTRRENAMEhelpID        15
#define VARRENAMEhelpID         16
#define ENTRYDATATYPEhelpID     17
#define ENTRYVALUEhelpID        18
#define ENTRYNUMBERhelpID       19
#define VARSELECThelpID         20
#define VARNUMELEMShelpID       21
#define VARVARYShelpID          22
#define RECNUMBERhelpID         23
#define DIMINDEXhelpID          24
#define VARVALUEhelpID          25
#define VARVALUEShelpID         26
#define VARMOREhelpID           27
#define PADVALUEhelpID          28
#define INITIALRECShelpID       29
#define EXTENDRECShelpID        30
#define VARNAMEhelpID           31
#define ATTRNAMEhelpID          32
#define ZDIMhelpID              33
#define VARDATATYPEhelpID       34
#define DELETEATTRhelpID        35
#define DELETEENTRYhelpID       36
#define DELETEVARhelpID         37
#define ALLOCATERECShelpID      38
#define GATTRShelpIDx           39
#define VATTRShelpIDx           40
#define GATTRENTRIEShelpIDx     41
#define VATTRENTRIEShelpIDx     42
#define CDFCOMPRESSIONhelpID    43
#define VARCOMPRESSIONhelpID    44
#define SPARSENESShelpID	45
#define TEXTENTRIEShelpID	46
#define MOREHELPhelpID		47
#define GZIPhelpID		48
#define DELETERECShelpID	49
#define CREATECDFV23helpID      50
#define CHECKSUMhelpID          51
#define OLHhelpID               99

/******************************************************************************
* DataType ItemWindow macros.
******************************************************************************/

#define DTiwSYMBOLS \
"CDF_CHAR    CDF_INT1  CDF_INT2   CDF_INT4   CDF_REAL4  CDF_REAL8   CDF_EPOCH", \
"CDF_UCHAR   CDF_UINT1 CDF_UINT2  CDF_UINT4  CDF_FLOAT  CDF_DOUBLE  CDF_BYTE", \
"CDF_EPOCH16 CDF_INT8  CDF_TIME_TT2000"

#define DTiwLINEnS      0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2
#define DTiwCOLs        0,12,22,33,44,55,67, 0,12,22,33,44,55,67, 0,12,22
#define DTiwLENs        8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9,10, 8,11, 8,15

/******************************************************************************
* Global variables.
******************************************************************************/

#if defined(CDFEDIT)
Logical report[3];              /* TRUE if a particular type of status code
				   (error/warning/info) should be reported. */
Logical dumpStatistics;         /* TRUE if caching statistics should be
				   displayed when the CDF is closed. */
Logical gAttrsAndEntries;       /* TRUE if the entries should be displayed
				   with gAttributes.  FALSE if there should
				   be a separate menu for each gAttribute's
				   entries. */
Logical vAttrsAndEntries;       /* TRUE if the entries should be displayed
				   with vAttributes.  FALSE if there should
				   be a separate menu for each vAttribute's
				   entries. */
char ilhFile[MAX_ILHFILE_LEN+1];/* Name of the inline help file. */
char olhFile[] = "cdfedit.olh"; /* Name of the command line help file. */
Logical browseOnly;             /* TRUE if the CDF is being browsed. */
Logical compressed;             /* TRUE if the currently open CDF is
				   compressed. */
#else
extern Logical report[3];
extern Logical dumpStatistics;
extern Logical gAttrsAndEntries;
extern Logical vAttrsAndEntries;
extern char ilhFile[MAX_ILHFILE_LEN+1];
extern char olhFile[];
extern Logical browseOnly;
extern Logical compressed;
#endif

/******************************************************************************
* Function prototypes.
******************************************************************************/

Logical EditCDFs PROTOARGs((int argC, char *argV[]));
Logical EditCDF PROTOARGs((
  char *CDFname, Logical useFormat, long workingCache, long stageCache,
  long compressCache, long zMode, Logical negToPosFp0
));
Logical EditCDFx PROTOARGs((
  char *CDFname, Logical useFormat, long workingCache, long stageCache,
  long compressCache, long zMode, Logical negToPosFp0, Logical *closed
));
Logical EditCDFsMenu PROTOARGs((
  char *CDFspec, Logical negToPosFp0, Logical useFormat, long zMode,
  long workingCache, long stageCache, long compressCache
));
Logical EditAttrs PROTOARGs((Logical, char *));
Logical EditAttrsX PROTOARGs((Logical, char *));
Logical EditAttrEntriesX PROTOARGs((Logical G, long attrN, long nAttrs));
Logical EditVars PROTOARGs((Logical, char *, Logical));
Logical EditVarValues PROTOARGs((Logical, long, long, Logical, Logical *));
Logical EditVarEntries PROTOARGs((Logical, long, long));
Logical EditVarMore PROTOARGs((Logical, long, Logical, Logical *, long));
Logical EditEntry PROTOARGs((long attrN, long entryN, int entryType,
			     Logical newDataSpec, Logical *changed));
void BuildCDFsMenu PROTOARGs((char *, int *, char ***, char ***,
			      struct ItemWindowStruct *));
Logical BuildCDFmenu PROTOARGs((char *, struct ItemWindowStruct *));
Logical BuildVarMenu PROTOARGs((Logical, char *, struct ItemWindowStruct *));
Logical BuildVarEntryMenu PROTOARGs((Logical, long, long *, long **,
				     Logical **, struct ItemWindowStruct *));
Logical BuildVarMoreMenu PROTOARGs((
  Logical, long, struct ItemWindowStruct *, Logical, Logical
));
Logical BuildAttrMenu PROTOARGs((Logical G, char *CDFname, long *nAttrs,
				 long **attrNs, long **nEntries,
				 long ***entryNs,struct ItemWindowStruct *IW));
Logical BuildAttrMenuX PROTOARGs((Logical G, char *CDFname, long *nAttrs,
				  long *nAttrsOfScope, long **attrNs,
				  struct ItemWindowStruct *IW));
Logical BuildAttrEntriesMenuX PROTOARGs((Logical G, long attrN, long *nEntries,
					 long **entryNs, int **entryTypes,
					 struct ItemWindowStruct *IW));
Logical BuildVarValueLine PROTOARGs((Logical, long, long, long, long *,
				     long *, long, long, void *, char *,
				     char *, int *, int *, int *, int,
				     char *, Logical *, size_t));
Logical BuildTextEditEntriesMenu PROTOARGs((
  long attrN, struct EditWindowStruct *EW, Logical *allCharacter
));
void FreeVarEntryMenu PROTOARGs((long *, Logical *,
				 struct ItemWindowStruct *));
void FreeAttrMenu PROTOARGs((long nAttrs, long *attrNs, long *nEntries,
			     long **entryNs, struct ItemWindowStruct *IW));
void FreeAttrMenuX PROTOARGs((long *attrNs, struct ItemWindowStruct *IW));
void FreeAttrEntriesMenuX PROTOARGs((long *entryNs, int *entryTypes,
				     struct ItemWindowStruct *IW));
void FreeCDFsMenu PROTOARGs((char **, char **, struct ItemWindowStruct *));
Logical CreateCDF PROTOARGs((
  char *CDFdir, char *CDFname, long zMode, Logical useFormat,
  Logical negToPosFp0, long workingCache, long stageCache, long compressCache,
  struct ItemWindowStruct *IWcdfs
));
Logical CreateEntry PROTOARGs((Logical, long, Logical *));
Logical CreateVar PROTOARGs((Logical, Logical *));
Logical EntryExists PROTOARGs((long, long, int, Logical *));
Logical SpecificationPrompt PROTOARGs((
  char CDFspec[DU_MAX_PATH_LEN+1], Logical useFormat, Logical negToPosFp0,
  long zMode, long workingCache, long stageCache, long compressCache
));
Logical TryToEditCDF PROTOARGs((
  char *CDFspec, Logical negToPosFp0, Logical useFormat, long zMode,
  long workingCache, long stageCache, long compressCache
));
Logical PromptForSpec PROTOARGs((char CDFspec[DU_MAX_PATH_LEN+1]));
Logical ReportStatus PROTOARGs((CDFstatus status, Logical center));
void CDFstatusWindow PROTOARGs((CDFstatus status, Logical center));
void ProblemWindow PROTOARGs((char *message, Logical center));
void MessageWindow VARPROTOARGs((char **lineS, ...));
Logical ConfirmWindow PROTOARGs((int, int, char *, char *, Logical, int));
Logical ConfirmWindow2 PROTOARGs((int, int, char *, char *, Logical, int));
void InitializeScreen PROTOARGs((void));
void CleanupScreen PROTOARGs((void));
Logical EnterIW PROTOARGs((struct ItemWindowStruct *IW, int helpId));
Logical EnterPW PROTOARGs((struct PromptWindowStruct *PW, int helpId));
int IndexInList PROTOARGs((char *match, int nItems, char **items));
int DataTypeItemN PROTOARGs((long dataType));
void ShowFullName PROTOARGs((char *name, Logical attr));
Logical SelectFormat PROTOARGs((void));
Logical SelectEncoding PROTOARGs((void));
Logical SelectMajority PROTOARGs((void));
Logical SelectCompression PROTOARGs((int what, int row, Logical *changed));
Logical SelectSparseness PROTOARGs((Logical zVar, Logical *changed));
Logical SelectDataSpec PROTOARGs((
  long *dataType, long *numElems, char *label
));
Logical SelectNumElems PROTOARGs((
  long dataType, long *numElems, char *label
));
Logical SelectChecksum PROTOARGs((void));
Logical UpdateTextEditEntries PROTOARGs((
  long attrN, struct EditWindowStruct *EW
));
Logical TextEditEntries PROTOARGs((long attrN));
CDFstatus CheckLFS PROTOARGs((char *name, Logical *isLFS, char *fullName
));

#if defined(mac)
Logical EditQOPs PROTOARGs((int *argC, char **argV[]));
#endif

/*****************************************************************************/

#endif
