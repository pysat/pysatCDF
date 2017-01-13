/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                            CDFexport header file.
*
*  Version 1.2e, 14-Dec-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  13-Sep-95, J Love     Original version.
*   V1.1  26-Aug-96, J Love     CDF V2.6.
*   V1.2  15-Nov-96, J Love     Added `simple' environment and batch mode.
*   V1.2a 15-Jan-97, J Love     Added prompts for settings file.  Added `help'
*                               qualifier.
*   V1.2b 21-Feb-97, J Love     Removed RICE.
*   V1.2c 31-Mar-97, J Love     Allow FieldWindow fields to exceed their
*                               on-screen width.
*   V1.2d 17-Nov-97, J Love	Windows NT/Visual C++.
*   V1.2e 14-Dec-97, J Love	Added ALPHAVMSi encoding.
*   V3.3  10-Apr-09, M Liu      Added Linux port on PPC.
*   V3.3a 10-Dec-10, M Liu      Added a new epoch style for ISO 8601.
*
******************************************************************************/

#if !defined(___cdfxport_h___)
#define ___cdfxport_h___

/******************************************************************************
* Include files.
******************************************************************************/

#include "widgets.h"

#if defined(mac)
#  include "cdfxp.rh"
#endif

/******************************************************************************
* Parameters/qualifiers/options.
******************************************************************************/

#define CDFparm                 0

#define INITIALqual             0
#define PROMPTqual              1
#define NOPROMPTqual            2
#define CACHEqual               3
#define ZMODEqual               4
#define NEG2POSFP0qual          5
#define NONEG2POSFP0qual        6
#define REPORTqual              7
#define STATSqual               8
#define NOSTATSqual             9
#define ABOUTqual               10
#define SIMPLEqual              11
#define NOSIMPLEqual            12
#define CDFqual                 13
#define TEXTqual                14
#define SETTINGSqual            15
#define BATCHqual               16
#define HELPqual                17
#define INCLUDEqual             18
#define EXCLUDEqual             19
#define TIMEFIRSTSEQUENCEqual   20
#define SETTINGSSEQUENCEqual    21
#define EPOCHRANGEqual          22
#define RECORDRANGEqual         23

#define FORMATopt               0
#define NOFORMATopt             1
#define FILTERopt               2
#define NOFILTERopt             3
#define FILLVALopt              4
#define NOFILLVALopt            5
#define SINGLEopt               6
#define MULTIopt                7
#define HOSTopt                 8
#define NETWORKopt              9
#define EPOCHopt                10
#define EPOCH1opt               11
#define EPOCH2opt               12
#define EPOCH3opt               13
#define EPOCHFopt               14
#define EPOCHXopt               15
#define HORIZONTALopt           16
#define VERTICALopt             17
#define RECORDopt               18
#define NORECORDopt             19
#define INDICESopt              20
#define NOINDICESopt            21
#define ROWopt                  22
#define COLUMNopt               23
#define VALIDMINopt             24
#define NOVALIDMINopt           25
#define MONOTONopt              26
#define NOMONOTONopt            27
#define VALIDMAXopt             28
#define NOVALIDMAXopt           29
#define FILLSopt                30
#define NOFILLSopt              31
#define EXCLUSIVEopt            32
#define NOEXCLUSIVEopt          33
#define OUTPUTopt               34
#define NOOUTPUTopt             35
#define DELETEopt               36
#define NODELETEopt             37
#define PREALLOCATEopt          38
#define NOPREALLOCATEopt        39
#define HEADINGopt              40
#define NOHEADINGopt            41
#define ISO8601opt              42
#define POUNDEDHEADINGopt       43

/******************************************************************************
* Miscellaneous.
******************************************************************************/

#define CDFnameLEN              31
#define NUMscreenLINES          18
#define SCREENtextMAX           (NUMscreenLINES*(SCREEN_WIDTH-2+1))
#define MAXvalueLEN             150
#define MAXminMaxLEN            150
#define MAXfillLEN              150
#define MAXitemFieldLEN         150
#define MAXwidthLEN             5
#define MAXreserveLEN           7
#define MAXblockingLEN          8
#define MAXrecordNumberLEN      11
#define MAXdimensionIndexLEN    11
#define MAXgENTRYencodedLEN     150

#define MAXwalkingVARs          13

#define NOWAIT                  0
#define NOBEEPWAIT              -10
#define NOBEEPWAIT1             -1
#define NOBEEPWAIT2             -2
#define BEEPWAIT                10
#define BEEPWAIT1               1
#define BEEPWAIT2               2

#define AMBIGUOUSopt            -1
#define UNKNOWNopt              -2

#define RECORDt                 0
#define INDICESt                1
#define VARIABLEt               2

#define MIN_RECORD_WIDTH        6
#define MIN_INDICES_WIDTH       11
#define MIN_VARIABLE_WIDTH      8

#define NOminMax                (-1)
#define NA_RESERVE              (-1)

#define PART_                   0       /* `Simple' mode. */
#define PART1                   1
#define PART2                   2
#define PART3                   3
#define PART4                   4

#define MAX_ENCODING            ALPHAVMSi_ENCODING
#define MAX_COMPRESSION         GZIP_COMPRESSION
#define MAX_CHECKSUM            MD5_CHECKSUM
#define MAX_SETTINGS_LEN        200

#define PASSED                  1
#define FAILED                  2
#define FATAL                   3

#define SUCCESS                 1
#define FATALin                 2
#define FATALout                3

#define EQsearch                0
#define LTsearch                1
#define LEsearch                2
#define GTsearch                3
#define GEsearch                4

#define BLANKs255 "\
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
       \
   "

/******************************************************************************
* `Batch' mode.
******************************************************************************/

#define noBATCH                 0
#define BATCHcdf                1
#define BATCHtext               2

#define BATCH(mode) (mode != noBATCH)

/******************************************************************************
* `Simple' mode.
******************************************************************************/

#define FORCEeachFilterSIMPLE           FALSE
#define FORCEoverallFilterSIMPLE        FALSE
#define FORCEfillsSIMPLE                FALSE
#define FORCEfillvalSIMPLE              FALSE
#define FORCEvalidminSIMPLE             FALSE
#define FORCEvalidmaxSIMPLE             FALSE
#define FORCEmonotonSIMPLE              FALSE

#define NAsingleSIMPLE                  FALSE
#define NAnetworkSIMPLE                 FALSE
#define NAshowSIMPLE                    FALSE
#define NAexclusiveSIMPLE               FALSE
#define NAdeleteSIMPLE                  FALSE
#define NApreAllocateSIMPLE             FALSE

#define SCREENkey_SIMPLE                KB_CTRL_A
#define FILEkey_SIMPLE                  KB_CTRL_B
#define SAVEkey_SIMPLE                  KB_CTRL_J
#define RESTOREkey_SIMPLE               KB_CTRL_L

#if defined(alphaosf) || defined(IRIX64bit) || defined(_LP64) || \
    defined(__amd64) || defined(__x86_64__) || defined(__ia64__) || \
    defined(__PPC64__) || defined(__ppc64__)
#  define Int32FORMATstandard     "%11d"
#  define Int32uFORMATstandard    "%10d"
#  define Int64FORMATstandard     "%20lld"
#else
#  define Int32FORMATstandard     "%11ld"
#  define Int32uFORMATstandard    "%10ld"
#  define Int64FORMATstandard     "%20lld"
#endif

#define EPOCHx_FORMAT_STANDARD  "*Unsupported*"

/******************************************************************************
* Field numbers/locations.
******************************************************************************/

#define CDFfn                   0
#define CDFfieldLINEn           0
#define CDFfieldCOLn            8
#define CDFfieldLEN             70
#define CDFfieldMAX             CDF_PATHNAME_LEN
#define CDFvarsRecsLINEn        1

#define NAMEitemCOLnSIMPLE      0
#define NAMEitemLENsimple       71
#define OUTPUTitemCOLnSIMPLE    72
#define OUTPUTitemLENsimple     6

#define NAMEitemCOLn            0
#define NAMEitemLEN             21

#define SPECitemCOLn            22
#define SPECitemLEN             9
#define DIMENitemCOLn           32
#define DIMENitemLEN            27
#define VARYSitemCOLn           60
#define VARYSitemLEN            11
#define OUTPUTitemCOLn          72
#define OUTPUTitemLEN           6

#define MINitemCOLn             22
#define MINitemLEN              24      /* Maximum of the possible styles. */
#define MAXitemCOLn             47
#define MAXitemLEN              24      /* Maximum of the possible styles. */
#define FILTERitemCOLn          72
#define FILTERitemLEN           6

#define FILLitemCOLn            22
#define FILLitemLEN             26
#define MONOitemCOLn            49
#define MONOitemLEN             13
#define FORMATitemCOLn          63
#define FORMATitemLEN           9
#define WIDTHitemCOLn           73
#define WIDTHitemLEN            5

#define SPARSEitemCOLn          22
#define SPARSEitemLEN           19
#define COMPRESSitemCOLn        42
#define COMPRESSitemLEN         19
#define RESERVEitemCOLn         62
#define RESERVEitemLEN          7
#define BLOCKINGitemCOLn        70
#define BLOCKINGitemLEN         8

#define nSELWINDOWcolsMAX       5
#define nSELWINDOWcols1         5
#define nSELWINDOWcols2         4
#define nSELWINDOWcols3         5
#define nSELWINDOWcols4         5
#define nSELWINDOWcols_         2       /* `Simple' mode. */

#define WALKleftLEN             21
#define WALKrightLEN            56

/******************************************************************************
* Column numbers.
******************************************************************************/

#define NAMEcxSIMPLE            0
#define OUTPUTcxSIMPLE          1

#define NAMEcx                  0

#define SPECcx                  1
#define DIMENcx                 2
#define VARYScx                 3
#define OUTPUTcx                4

#define MINcx                   1
#define MAXcx                   2
#define FILTERcx                3

#define FILLcx                  1
#define MONOcx                  2
#define FORMATcx                3
#define WIDTHcx                 4

#define SPARSEcx                1
#define COMPRESScx              2
#define RESERVEcx               3
#define BLOCKINGcx              4

/******************************************************************************
* Row numbers.
******************************************************************************/

#define FILTERrx                0
#define FILLSrx                 1
#define CDFFORMATrx             2
#define ENCODINGrx              3
#define EPOCHrx                 4
#define hvMODErx                5
#define MAJORrx                 6
#define SHOWrx                  7
#define SPACINGrx               8
#define DELETErx                9
#define PREALLOCrx              10
#define HEADINGrx               11

/******************************************************************************
* Item numbers.
******************************************************************************/

#define NONEin                  0
#define RLE0in                  1
#define HUFF0in                 2
#define AHUFF0in                3
#define GZIPin                  4

#define NOsRECORDSin            0
#define PADsRECORDSin           1
#define PREVsRECORDSin          2

#define MD5in                   1
/******************************************************************************
* Actions.
******************************************************************************/

#define TOSCREENact             0
#define TOFILEact               1
#define TOCDFact                2
#define WALKact                 3
#define OUTPUTallITEMSact       4
#define OUTPUTnoITEMSact        5
#define FILTERallITEMSact       6
#define FILTERnoITEMSact        7
#define SETcdfCOMPRESSact       8
#define SETvarCOMPRESSact       9
#define SAVEact                 10
#define RESTOREact              11
#define SETMONOact              12
#define TOCDFv2act              13
#define SETcdfCHECKSUMact       14

/******************************************************************************
* Help identifiers.
******************************************************************************/

#define CDFhelpID               1
#define CDFShelpID              2
#define VAR1helpID              3
#define VAR2helpID              4
#define VAR3helpID              5
#define ACTIONhelpID            6
#define OPTIONhelpID            7
#define MINmaxRECORDhelpID      8
#define MINmaxINDICEShelpID     9
#define MINmaxVALUEhelpID       10
#define FILLhelpID              11
#define FORMAThelpID            12
#define WIDTHhelpID             13
#define SCREENhelpID            14
#define oFILEhelpID             15
#define oCDFhelpID              16
#define VAR4helpID              17
#define BLOCKINGhelpID          18
#define COMPRESShelpID          19
#define SPARSEhelpID            20
#define RESERVEhelpID           21
#define GZIPhelpID              23
#define WALKriHelpID            24
#define WALKvarHelpID           25
#define RECORDhelpID            26
#define INDEXhelpID             27
#define VALUEhelpID             28
#define SEARCHTYPEhelpID        29
#define VARhelpID               30      /* `Simple' mode. */
#define SETFILEhelpID           31
#define ACTIONv32helpID         32
#define CHECKSUMhelpID          33
#define OLHhelpID               99

/******************************************************************************
* Output types.
******************************************************************************/

#define OUTPUTtoSCREENh         1
#define OUTPUTtoSCREENv         2
#define OUTPUTtoFILEh           3
#define OUTPUTtoFILEv           4
#define OUTPUTtoCDF             5
#define OUTPUTtoFILEcdaweb      6

/******************************************************************************
* Monotonicities.
******************************************************************************/

#define NAmono                  (-1)
#define UNKNOWNmono             0
#define INCREASEmono            1
#define DECREASEmono            2
#define FALSEmono               3

/******************************************************************************
* Filter status.
******************************************************************************/

#define PASSes          1
#define SHOWit          2
#define FAILline        3
#define FAILrecord      4

#define SHOWline(status) (status == PASSes || status == SHOWit)

/******************************************************************************
* NEWkeyDEFs.
******************************************************************************/

#define NEWkeyDEFS(EWkey,keyDefs,batchMode) \
if (!BATCH(batchMode)) { \
  EWkey->eText = keyDefs; \
  EditWindow (UPDATEew, EWkey, LogicalTRUE); \
}

/******************************************************************************
* VALUEinBUFFER.
******************************************************************************/

#define VALUEinBUFFER(Var,valueN) \
(Var->buffer + (size_t) (valueN * Var->nValueBytes))

/******************************************************************************
* MAJORITYtoOUT.
******************************************************************************/

#define MAJORITYtoOUT(outMajority,inMajority) \
BOO(outMajority == INPUT_MAJOR,inMajority,outMajority)

/******************************************************************************
* RECORDfailedFILTER, INDICESfailedFILTER, and VARfailedFILTER.
******************************************************************************/

#define RECORDfailedFILTER(Item,recN) \
(Item->filter && \
 (!RecordPassesMin(Item,recN) || !RecordPassesMax(Item,recN)))

#define INDICESfailedFILTER(Item,indices) \
(Item->filter && \
 (!IndicesPassMin(Item,indices) || !IndicesPassMax(Item,indices)))

#define VARfailedFILTER(Item,value) \
(Item->filter && \
 (!VarPassesMin(Item,value) || !VarPassesMax(Item,value)))

/******************************************************************************
* SELECTcdf.
******************************************************************************/

#define SELECTcdf(id) CDFlib(SELECT_,CDF_,id,NULL_);

/******************************************************************************
* MINorMAXexists.
******************************************************************************/

#define MINorMAXexists(Item) \
(Item->type == RECORDt && \
 (Item->Record->min != NOminMax || \
  Item->Record->max != NOminMax)) || \
(Item->type == INDICESt && \
 (Item->Indices->minNumDims != NOminMax || \
  Item->Indices->maxNumDims != NOminMax)) || \
(Item->type == VARIABLEt && \
 (Item->Var->min != NULL || \
  Item->Var->max != NULL))

/******************************************************************************
* FILTER.
******************************************************************************/

#define FILTER(Item,opt) (opt.overallFilter && Item->filter)

/******************************************************************************
* USEFILL.
******************************************************************************/

#define USEFILL(Var,opt) (opt.useFills && Var->fill != NULL)

/******************************************************************************
* WRITEFILL.
******************************************************************************/

#define WRITEFILL(Var,opt) \
(USEFILL(Var,opt) && NEx(Var->fill,Var->pad,Var->dataType,Var->numElems))

/******************************************************************************
* NRVtoOUTPUT.
******************************************************************************/

#define NRVtoOUTPUT(Item) \
(Item->output && !Item->Var->recVary)

/******************************************************************************
* HYPERget/put and SINGLEget/put.
******************************************************************************/

#define SINGLEget(id,varN,zVar,recNumber,dimIndices,buffer) \
CDFlib (SELECT_, CDF_, id, \
		 BOO(zVar,zVAR_,rVAR_), varN, \
		 BOO(zVar,zVAR_RECNUMBER_,rVARs_RECNUMBER_), recNumber, \
		 BOO(zVar,zVAR_DIMINDICES_,rVARs_DIMINDICES_), dimIndices, \
	GET_, BOO(zVar,zVAR_DATA_,rVAR_DATA_), buffer, \
	NULL_)

#define SINGLEput(id,varN,zVar,recNumber,dimIndices,buffer) \
CDFlib (SELECT_, CDF_, id, \
		 BOO(zVar,zVAR_,rVAR_), varN, \
		 BOO(zVar,zVAR_RECNUMBER_,rVARs_RECNUMBER_), recNumber, \
		 BOO(zVar,zVAR_DIMINDICES_,rVARs_DIMINDICES_), dimIndices, \
	PUT_, BOO(zVar,zVAR_DATA_,rVAR_DATA_), buffer, \
	NULL_)

#define HYPERget(id,varN,zVar,recNumber,recCount,dimIndices,dimCounts,buffer) \
CDFlib (SELECT_, CDF_, id, \
		 BOO(zVar,zVAR_,rVAR_), varN, \
		 BOO(zVar,zVAR_RECNUMBER_,rVARs_RECNUMBER_), recNumber, \
		 BOO(zVar,zVAR_RECCOUNT_,rVARs_RECCOUNT_), recCount, \
		 BOO(zVar,zVAR_DIMINDICES_,rVARs_DIMINDICES_), dimIndices, \
		 BOO(zVar,zVAR_DIMCOUNTS_,rVARs_DIMCOUNTS_), dimCounts, \
	GET_, BOO(zVar,zVAR_HYPERDATA_,rVAR_HYPERDATA_), buffer, \
	NULL_)

#define HYPERput(id,varN,zVar,recNumber,recCount,dimIndices,dimCounts,buffer) \
CDFlib (SELECT_, CDF_, id, \
		 BOO(zVar,zVAR_,rVAR_), varN, \
		 BOO(zVar,zVAR_RECNUMBER_,rVARs_RECNUMBER_), recNumber, \
		 BOO(zVar,zVAR_RECCOUNT_,rVARs_RECCOUNT_), recCount, \
		 BOO(zVar,zVAR_DIMINDICES_,rVARs_DIMINDICES_), dimIndices, \
		 BOO(zVar,zVAR_DIMCOUNTS_,rVARs_DIMCOUNTS_), dimCounts, \
	PUT_, BOO(zVar,zVAR_HYPERDATA_,rVAR_HYPERDATA_), buffer, \
	NULL_)

/******************************************************************************
* Record structure.
******************************************************************************/

struct RecordStruct {
  long min;
  long max;
};

/******************************************************************************
* Indices structure.
******************************************************************************/

struct IndicesStruct {
  long numDims;
  long dimSizes[CDF_MAX_DIMS];
  long indices[CDF_MAX_DIMS];
  long minNumDims;
  long minIndices[CDF_MAX_DIMS];
  long maxNumDims;
  long maxIndices[CDF_MAX_DIMS];
};

/******************************************************************************
* Variable structure.
******************************************************************************/

struct VarStruct {
  long varN;            /* Number. */
  long numDims;         /* Number of dimensions. */
  long dimSizes[CDF_MAX_DIMS];
			/* Dimension sizes (conceptual). */
  long nRecordValues;   /* Number of values per record (conceptual). */
  long dataType;        /* Data type. */
  long numElems;        /* Number of elements (of the data type) per value. */
  long recVary;         /* Record variance. */
  long dimVarys[CDF_MAX_DIMS];
			/* Dimension variances. */
  long maxRec;          /* Maximum record written. */
  long blocking;        /* Blocking factor. */
  long reserve;         /* Reserve percentage. */
  long sRecordsType;    /* Sparse records type. */
  long sArraysType;     /* Sparse arrays type. */
  long sArraysParms[CDF_MAX_PARMS];
			/* Sparse arrays parameters. */
  long cType;           /* Compression type. */
  long cParms[CDF_MAX_PARMS];
			/* Compression parameters. */
  long valueN;          /* Value number. */
  long indices[CDF_MAX_DIMS];
			/* Indices for this variable. */
  long varNo;           /* Corresponding variable number in an output CDF. */
  long oRecords;        /* Number of records to be written to the corresponding
			   variable in an output CDF. */
  void *min;            /* Minimum filter value. */
  void *max;            /* Maximum filter value. */
  void *fill;           /* FILLVAL entry converted to variable's data type. */
  void *pad;            /* Pad value. */
  void *value;          /* Buffer for one value. */
  Byte1 *buffer;        /* Buffer for many values. */
  char *name;           /* Variable name. */
  char *format;         /* Format (C or Fortran). */
  size_t nValueBytes;   /* Number of bytes per variable value. */
  int monotonic;        /* `INCREASEmono' if this variable is monotonic
			   increase, `DECREASEmono' if monotonic decrease,
			   `FALSEmono' if not monotonic, or `UNKNOWNmono'
			   if monotonicity is unknown. */
  Logical zVar;         /* TRUE if a zVariable, FALSE if an rVariable. */
  Logical scalar;       /* Scalar variable? */
};

/******************************************************************************
* Item structure.
******************************************************************************/

struct ItemStruct {
  int type;                     /* RECORDSt, INDICESt, or VARIABLEt. */
  int width;
  Logical outputSetting;        /* Setting on the SelectionWindow. */
  Logical filterSetting;        /* Setting on the SelectionWindow. */
  Logical output;               /* If item should be output. */
  Logical filter;               /* If item should be filtered. */
  Logical inclusive;
  struct VarStruct *Var;
  struct RecordStruct *Record;
  struct IndicesStruct *Indices;
  struct ItemStruct *nextItem;
  struct ItemStruct *nextExport;
  struct ItemStruct *nextScalar;
  struct ItemStruct *nextHyper;
};

/******************************************************************************
* Options structure.
******************************************************************************/

struct OptionStruct {
  long encoding;                /* Encoding for output CDFs. */
  long majority;                /* Majority for listings/output CDFs. */
  Logical eachFilter;           /* Initial filter setting for each
				   item/variable (yes/no). */
  Logical overallFilter;        /* Overall use of filters? */
  Logical useFills;             /* Use fill values? */
  Logical useFORMAT;            /* Use FORMAT attribute entries? */
  Logical useFILLVAL;           /* Use FILLVAL attribute entries? */
  Logical useVALIDMIN;          /* Use VALIDMIN attribute entries? */
  Logical useVALIDMAX;          /* Use VALIDMAX attribute entries? */
  Logical useMONOTON;           /* Use MONOTON attribute entries? */
  Logical singleFile;
  Logical horizontalMode;
  Logical showRecord;
  Logical showIndices;
  Logical showFiltered;
  Logical exclusive;
  Logical outputItem;           /* Initial output setting for each
				   item/variable (yes/no). */
  Logical deleteExisting;       /* Delete existing CDFs? */
  Logical preAllocate;          /* Preallocate variable records? */
  Logical textHeading;          /* Display heading line in text files? */
  int epochStyle;
  int spacing;
};

/******************************************************************************
* Global variables.
******************************************************************************/

#if defined(CDFXP)
int batchMode = noBATCH;
Logical simpleMode = DEFAULTsimpleEXPORT;
Logical dumpStats = DEFAULTstatsEXPORT;
Logical report[3] = {
  REPORTerrorsDEFAULT, REPORTwarningsDEFAULT, REPORTinfosDEFAULT
};
long inMajority;
struct OptionStruct opt;
int nItems;
struct ItemStruct *itemHead;
struct FieldWindowStruct *FWcdf;
struct ItemWindowStruct *IWsel;
struct EditWindowStruct *EWkey;
struct EditWindowStruct *EWmsg;
long workingCache = useDEFAULTcacheSIZE;
long stageCache = useDEFAULTcacheSIZE;
long compressCache = useDEFAULTcacheSIZE;
char na[] = "n/a";
char dots[] = "...";
char *monos[] = { "n/a", "Unknown", "Increase", "Decrease", "False" };
char *epochStyles[] = {
  "standard", "alternate/1", "alternate/2", "alternate/3",
  "Format/C_Fortran", "Format/custom", "ISO8601"
};
char *encodings[] = {
  NULL, "NETWORK", "SUN", "VAX", "DECSTATION", "SGi", "IBMPC", "IBMRS",
  "HOST", "PPC", NULL, "HP", "NeXT", "ALPHAOSF1", "ALPHAVMSd", "ALPHAVMSg",
  "ALPHAVMSi", "PowerPC"
};
char *majorities[] = { "input", "row", "column" };
char *compressions[] = { "no", "rle", "huff", "ahuff", "gzip" };
char *checksums[] = {"no", "md5"};
char *headings[] = { "no", "YES", "POUNDEDHEADING" };
char readingCDF[] = "reading input CDF";
char writingCDF[] = "writing output CDF";
long dimIndices_0[CDF_MAX_DIMS] = { 0,0,0,0,0,0,0,0,0,0 };
long dimCounts_1[CDF_MAX_DIMS] = { 1,1,1,1,1,1,1,1,1,1 };
long CDFcType;
long CDFcParms[CDF_MAX_PARMS];
long NrVars, NzVars;
long rMaxRec, zMaxRec;
long CDFchecksum;
#if DEFAULTsimpleEXPORT
char settingsFile[DU_MAX_PATH_LEN+1] = "simple.set";
#else
char settingsFile[DU_MAX_PATH_LEN+1] = "export.set";
#endif
char outputText[DU_MAX_PATH_LEN+1] = "default.lis";
char outputCDF[CDF_PATHNAME_LEN+1] = "default";
long includeVars;
char varsList[50*(CDF_VAR_NAME_LEN256+1)];
long outputVars;
Logical cdaweb;
int  poundedheading;
char **vars;
int  numVars;
int  numVarsO;
long range;
double epochStart, epochEnd;
double epoch16Start[2], epoch16End[2];
long long tt2000Start, tt2000End;
long recordStart, recordEnd;
int numEpochs;
char **epochs;
int *epochIndx;
long *epochMaxs;
int *epochTypes;
int numGroups;
char ***varsInGroup;
struct ItemStruct ***varPtrsInGroup;
int *numVarsInGroup;
#else
extern int batchMode;
extern Logical simpleMode;
extern Logical dumpStats;
extern Logical report[3];
extern long inMajority;
extern struct OptionStruct opt;
extern int nItems;
extern struct ItemStruct *itemHead;
extern struct FieldWindowStruct *FWcdf;
extern struct ItemWindowStruct *IWsel;
extern struct EditWindowStruct *EWkey;
extern struct EditWindowStruct *EWmsg;
extern long workingCache, stageCache, compressCache;
extern char na[];
extern char dots[];
extern char *monos[];
extern char *epochStyles[];
extern char *encodings[];
extern char *majorities[];
extern char *compressions[];
extern char *checksums[];
extern char readingCDF[];
extern char writingCDF[];
extern long dimIndices_0[CDF_MAX_DIMS];
extern long dimCounts_1[CDF_MAX_DIMS];
extern long CDFcType;
extern long CDFcParms[CDF_MAX_PARMS];
extern long NrVars, NzVars;
extern long rMaxRec, zMaxRec;
extern long CDFchecksum;
extern char settingsFile[DU_MAX_PATH_LEN+1];
extern char outputText[DU_MAX_PATH_LEN+1];
extern char outputCDF[CDF_PATHNAME_LEN+1];
extern long includeVars;
extern long outputVars;
extern Logical cdaweb;
extern int  poundedheading;
extern char varsList[50*(CDF_VAR_NAME_LEN256+1)];
extern char **vars;
extern int  numVars;
extern int  numVarsO;
extern Logical range;
extern double epochStart, epochEnd;
extern double epoch16Start[2], epoch16End[2];
extern long long tt2000Start, tt2000End;
extern long recordStart, recordEnd;
extern int numEpochs;
extern char **epochs;
extern int *epochIndx;
extern long *epochMaxs;
extern int *epochTypes;
extern int numGroups;
extern char ***varsInGroup;
extern struct ItemStruct ***varPtrsInGroup;
extern int *numVarsInGroup;
#endif

/******************************************************************************
* Function prototypes.
******************************************************************************/

Logical ExportCDFs PROTOARGs((int argC, char *argV[]));
void CDFexportMenu PROTOARGs((
  char *iniSpec, Logical prompt, long zMode, Logical negToPosFp0
));
void ExportCDFsSpec PROTOARGs((
  char *path, long zMode, Logical negToPosFp0, char *varLabel0,
  char *varHeader0[]
));
void ExportCDF PROTOARGs((char *path, long zMode, Logical negToPosFp0));
Logical LoadCDF PROTOARGs((void));
Logical LoadSelectionWindow PROTOARGs((int part));
void LoadCDFwindow PROTOARGs((char *path));
void SelectionWindow PROTOARGs((Logical *noMoreAccess));
Logical SelectionWindow1 PROTOARGs((Logical *noMoreAccess));
Logical SelectionWindow2 PROTOARGs((Logical *noMoreAccess));
Logical SelectionWindow3 PROTOARGs((Logical *noMoreAccess));
Logical SelectionWindow4 PROTOARGs((Logical *noMoreAccess));
Logical ActionMenu PROTOARGs((int part));
void OptionMenu PROTOARGs((void));
Logical ConfirmExit PROTOARGs((void));
void DisplayStatus PROTOARGs((CDFstatus status, char *text));
int DisplayMessage PROTOARGs((char *message, int wait));
int MatchOption PROTOARGs((char *option, char *validOptions[]));
void FreeItems PROTOARGs((void));
int LongValueWidth PROTOARGs((long value));
int RecordIndicesWidth PROTOARGs((
  long lastRec, long numDims, long dimSizes[], int type
));
void FreeToScreen PROTOARGs((
  char *header[], char *scrLines[], char *trailer[]
));
Logical PromptFor PROTOARGs((
  char *value, int valueL, int cursorAt, char *label, int helpID
));
Logical PromptForMinMax PROTOARGs((struct ItemStruct *Item, Logical min));
Logical PromptForFill PROTOARGs((struct VarStruct *Var));
Logical PromptForReserve PROTOARGs((struct VarStruct *Var));
Logical PromptForBlocking PROTOARGs((struct VarStruct *Var));
Logical PromptForWidth PROTOARGs((struct ItemStruct *Item));
Logical PromptForFormat PROTOARGs((struct ItemStruct *Item));
Logical PromptForCompression PROTOARGs((
  long *cType, long cParms[CDF_MAX_PARMS]
));
Logical PromptForSparseness PROTOARGs((struct VarStruct *Var));
Logical PromptForChecksum PROTOARGs((long *checksum));
Logical FlipItems PROTOARGs((int itemX));
int VariableWidth PROTOARGs((struct VarStruct *Var));
void SaveSettings PROTOARGs((void));
void RestoreSettings PROTOARGs((void));
Logical ToWalk PROTOARGs((void));
Logical ToScreenHori PROTOARGs((void));
Logical ToFileHori PROTOARGs((void));
Logical ToFileCDAweb PROTOARGs((void));
CDFstatus EncodeLineHori PROTOARGs((
  char *line, long recN, int *filterStatus, struct ItemStruct *exportHead,
  Logical standard, size_t width
));
CDFstatus EncodeLineHoriN PROTOARGs((
  char *line, long recN, int *filterStatus, int ix, int ia, int **flags,
  Logical standard, size_t width
));
Logical ToScreenVert PROTOARGs((void));
Logical ToFileVert PROTOARGs((void));
CDFstatus EncodeLineVert PROTOARGs((
  char *line, long recN, long valueN, long numDims,
  long indices[CDF_MAX_DIMS], Logical same, int *filterStatus,
  struct ItemStruct *exportHead, Logical outRowMajor, Logical standard,
  size_t width
));
Logical ListAttributes PROTOARGs((FILE *oFp, Logical *cdfFatal));
Logical ToCDF PROTOARGs((CDFid inID));
int ToCDFsameGt0 PROTOARGs((
  CDFid inID, CDFid outID, long firstRec, long lastRec, long numDims,
  long dimSizes[], long firstIndices[], long outMajority,
  struct ItemStruct *exportHead
));
int ToCDFdiffOrZero PROTOARGs((
  CDFid inID, CDFid outID, long firstRec, long lastRec, long outMajority,
  struct ItemStruct *exportHead
));
int OutputNRVvalues PROTOARGs((
  CDFid inID, CDFid outID, struct ItemStruct *exportHead, Logical same,
  long dimSizes[], long firstIndices[], long outMajority
));
Logical OutputHyperBuffer PROTOARGs((
  CDFid outID, long varNo, Logical zVar, long outMajority, long recNumber,
  long recCount, long dimIndices[], long dimCounts[], long numDims,
  long dimSizes[], Byte1 *buffer, long nValues, Logical fullRecord,
  size_t nValueBytes, long nRecordValues
));
int CopyAttributesANDgEntries PROTOARGs((
  CDFid inID, CDFid outID, long *nAttrs
));
int CopyVariablesANDrzEntries PROTOARGs((
  CDFid inID, CDFid outID, long nAttrs, Logical same, long numDims,
  long dimSizes[], struct ItemStruct *exportHead
));
Logical FirstLastRecord PROTOARGs((
  long *firstRec, long *lastRec, Logical toCDF, struct ItemStruct *exportHead
));
Logical FirstLastIndices PROTOARGs((
  long numDims, long dimSizes[], long firstIndices[], long lastIndices[],
  long *nValues, Logical toCDF, struct ItemStruct *exportHead
));
Logical ScalarVariable PROTOARGs((struct VarStruct *Var));
Logical DimensionalVariable PROTOARGs((struct VarStruct *Var, int *dimN));
Logical OneDimensionVaries PROTOARGs((struct VarStruct *Var));
Logical ReadScalarValue PROTOARGs((struct VarStruct *Var, long recN));
Logical ReadDimensionalValue PROTOARGs((
  struct VarStruct *Var, long indices[]
));
Logical ValidFormat PROTOARGs((char *format));
Logical SameDimensionalities PROTOARGs((
  long *numDims, long dimSizes[], struct ItemStruct *exportHead
));
void ValidateRecordIndices PROTOARGs((
  int type, Logical same, long numDims, struct ItemStruct *exportHead
));
Logical VarPassesMin PROTOARGs((struct ItemStruct *Item, void *value));
Logical VarPassesMax PROTOARGs((struct ItemStruct *Item, void *value));
Logical RecordPassesMin PROTOARGs((struct ItemStruct *Item, long recN));
Logical RecordPassesMax PROTOARGs((struct ItemStruct *Item, long recN));
Logical IndicesPassMin PROTOARGs((struct ItemStruct *Item, long indices[]));
Logical IndicesPassMax PROTOARGs((struct ItemStruct *Item, long indices[]));
void DisplayPctComplete PROTOARGs((int pct, char *msg));
void UpdateToScreen PROTOARGs((
  struct EditWindowStruct *EWscr, char *trailerMsg, long at, long total
));
Logical SetItemMonotonicities PROTOARGs((void));
Logical SetVarMonotonicity PROTOARGs((struct VarStruct *Var));
void BuildExportList PROTOARGs((
  struct ItemStruct **exportHead, Logical walking
));
void RemoveExportItems PROTOARGs((struct ItemStruct **exportHead));
void ReorderExportItems PROTOARGs((struct ItemStruct **exportHead));
void ReorderGroupItems PROTOARGs((struct ItemStruct **exportHead));
void FreeExportBuffers PROTOARGs((struct ItemStruct *exportHead));
void FilterHypers PROTOARGs((struct ItemStruct *hyperHead, long nValues));
long FindFirstRecord PROTOARGs((
  long recX, struct ItemStruct *scalarHead, Logical filteringScalars,
  long recCount
));
long FindLastRecord PROTOARGs((
  long recF, struct ItemStruct *scalarHead, Logical filteringScalars,
  long recCount
));
void FilterBuffer PROTOARGs((
  struct ItemStruct *Item, Byte1 *buffer, long nValues
));
Logical AbortListing PROTOARGs((FILE *oFp, char *line));
Logical AbortCDF PROTOARGs((struct ItemStruct *exportHead));
int PreAllocateRecords PROTOARGs((
  CDFid inID, CDFid outID, struct ItemStruct *scalarHead,
  struct ItemStruct *hyperHead, long *firstRec, long *lastRec
));
void OptDefaults PROTOARGs((void));
Logical ParseEpochRange PROTOARGs((char **epochRanges));
Logical LoadVarsTextFile PROTOARGs((char *varsFile));
Logical IsaVirtualVariable PROTOARGs((Logical zVAR, char *varName));
CDFstatus GetVirtualVariableFunc PROTOARGs((Logical zVAR, char *varName, 
char *funcName));
CDFstatus ReadRealorVirtualVariableEpoch PROTOARGs((Logical zVAR,
char *varName, int ind, void **epochValues));
CDFstatus ReadVirtualVariableApplyQaflag PROTOARGs((Logical zVar, char *varName,
int *qaFlags));
int FindUsedEpochs PROTOARGs((struct ItemStruct **itemHead, int *index));
CDFstatus LoadVirtualEpochs PROTOARGs((void **epochValues));
#if defined(mac)
Logical ExportQOPs PROTOARGs((
  int *argC, char **argV[]
));
#endif

/*****************************************************************************/

#endif /* ___cdfxport_h___ */
