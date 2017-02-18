/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                    Header file for CDF library.
*
*  Version 1.0, 11-Jun-04, Hughes STX.
*
*  Modification history:
*
*   V1.0  11-Jun-04, M Liu      Original version (for CDF V2.8).
*                               Modified from cdflib.h for 64-bit addressing.
*   V3.2 18-Jun-07, D Berger    Added structures in support of metadata
*                               READONLY performance enhancement.
*   V3.3 10-Jan-09, M Liu       Added maximum sizes for internal records.
******************************************************************************/

#if !defined(CDFLIB64h_INCLUDEd__)
#  define CDFLIB64h_INCLUDEd__
#include "cdfdist.h"
/*****************************************************************************
* CDF V3.0+ constants.
*****************************************************************************/

#define V3_CDR_OFFSET64                 ((Int32) 0x00000008)
#define V3_CCR_OFFSET64                 ((Int32) 0x00000008)
#define VDR_WASTED_SIZE64               148

/*****************************************************************************
* Miscellaneous macros.
*****************************************************************************/
#define SEEKv64(fp,offset,origin) (V_seek64(fp,offset,origin) == 0)
#define READv64(buffer,size,number,fp) \
(V_read64(buffer,size,number,fp) == number)
#define WRITEv64(buffer,size,number,fp) \
(V_write64(buffer,size,number,fp) == number)
#define CLOSEv64(fp,CDF,vStats) (V_close64(fp,CDF,vStats) == 0)
#define DELETEv64(fp,vStats) (V_delete64(fp,vStats) == 0)
#define CACHEv64(fp,nBuffers) (V_setcache64(fp,nBuffers) == 0)
#define FLUSHv64(fp) (V_flush64(fp) == 0)

/******************************************************************************
* LoadCVVRx/LoadVVRx.
******************************************************************************/

#define LoadCVVRx64(cvvr,recordsize,csize) \
cvvr.RecordSize = (OFF_T) (recordsize); \
cvvr.RecordType = CVVR_; \
cvvr.rfuA = (Int32) 0; \
cvvr.cSize = (OFF_T) (csize);

#define LoadVVRx64(vvr,recordsize) \
vvr.RecordSize = (OFF_T) (recordsize); \
vvr.RecordType = VVR_;

/******************************************************************************
* LoadAllocCVVR64/LoadAllocVVR64.
******************************************************************************/

#define LoadAllocCVVR64(alloc,firstrec,lastrec,csize,xsize) \
alloc.first = (Int32) (firstrec); \
alloc.last = (Int32) (lastrec); \
alloc.type = (int) CVVR_; \
alloc.cvvr64.cSize = (OFF_T) (csize); \
alloc.cvvr64.xSize = (OFF_T) (xsize);

#define LoadAllocVVR64(alloc,firstrec,lastrec,newvvr) \
alloc.first = (Int32) (firstrec); \
alloc.last = (Int32) (lastrec); \
alloc.type = (int) VVR_; \
alloc.vvr.newX = (Logical) (newvvr);

/******************************************************************************
* Base record sizes (bytes).
******************************************************************************/

#define UUIR_BASE_SIZE64        12
#define UIR_BASE_SIZE64         28
#define CDR_BASE_SIZE64         56
#define GDR_BASE_SIZE64         84
#define zVDR_BASE_SIZE64        (88 + CDF_VAR_NAME_LEN256)
#define rVDR_BASE_SIZE64        (84 + CDF_VAR_NAME_LEN256)
#define VXR_BASE_SIZE64         (28 + (16 * NUM_VXR_ENTRIES))
#define VXRx_BASE_SIZE64        (28 + (16 * NUM_VXRx_ENTRIES))
#define VVR_BASE_SIZE64         12
#define ADR_BASE_SIZE64         (68 + CDF_ATTR_NAME_LEN256)
#define AEDR_BASE_SIZE64        56
#define CCR_BASE_SIZE64         32
#define CPR_BASE_SIZE64         24
#define SPR_BASE_SIZE64         24
#define CVVR_BASE_SIZE64        24

/******************************************************************************
* Max record sizes (bytes).
******************************************************************************/

#define CDR_MAX_SIZE64          (56 + CDF_COPYRIGHT_LEN)
#define GDR_MAX_SIZE64          (84 + 4 * CDF_MAX_DIMS)
#define zVDR_MAX_SIZE64         (88 + CDF_VAR_NAME_LEN256 + 8 * CDF_MAX_DIMS)
#define rVDR_MAX_SIZE64         (84 + CDF_VAR_NAME_LEN256 + 4 * CDF_MAX_DIMS)
#define VXR_MAX_SIZE64          (28 + (16 * NUM_VXR_ENTRIES))
#define VXRx_MAX_SIZE64         (28 + (16 * NUM_VXRx_ENTRIES))
#define ADR_MAX_SIZE64          (68 + CDF_ATTR_NAME_LEN256)
#define AEDR_MAX_SIZE64         56
#define CPR_MAX_SIZE64          28
#define SPR_MAX_SIZE64          28

/******************************************************************************
* CCR structure/fields/offsets - Compressed CDF Record.
******************************************************************************/

struct CCRstruct64 {
  OFF_T RecordSize;     /* Size of record (bytes) */
  Int32 RecordType;     /* Type of record (ie. CCR) */
  OFF_T CPRoffset;      /* File offset to CPR (bytes). */
  OFF_T uSize;          /* Size of uncompressed CDF's IRs (bytes).  This byte
			   count does NOT include the magic numbers. */
  Int32 rfuA;           /* Reserved for future use.  Set to zero. */
};

#define CCR_RECORDSIZE_OFFSET64 0
#define CCR_RECORDTYPE_OFFSET64 8
#define CCR_CPROFFSET_OFFSET64  12
#define CCR_USIZE_OFFSET64      20
#define CCR_RFUa_OFFSET64       28

/******************************************************************************
* CDR structure/fields/offsets - CDF Descriptor Record.
******************************************************************************/

struct CDRstruct64 {
  OFF_T RecordSize;     /* Size of record (bytes) */
  Int32 RecordType;     /* Type of record (ie. CDR) */
  OFF_T GDRoffset;      /* File offset to GDR (bytes) */
  Int32 Version;        /* CDF Version Number */
  Int32 Release;        /* CDF Release Number */
  Int32 Encoding;       /* Encoding of CDF File */
  Int32 Flags;          /* Flags (see bit definitions in cdflib.h). */
  Int32 rfuA;           /* reserved for future use (value = 0) */
  Int32 rfuB;           /* reserved for future use (value = 0) */
  Int32 Increment;      /* CDF Increment Number (Vversion.release.increment),
			   always 0 for V2.0 CDFs */
  Int32 rfuD;           /* reserved for future use (value = -1) */
  Int32 rfuE;           /* reserved for future use (value = -1) */
};

#define CDR_RECORDSIZE_OFFSET64 0
#define CDR_RECORDTYPE_OFFSET64 8
#define CDR_GDROFFSET_OFFSET64  12
#define CDR_VERSION_OFFSET64    20
#define CDR_RELEASE_OFFSET64    24
#define CDR_ENCODING_OFFSET64   28
#define CDR_FLAGS_OFFSET64      32
#define CDR_RFUa_OFFSET64       36
#define CDR_RFUb_OFFSET64       40
#define CDR_INCREMENT_OFFSET64  44
#define CDR_RFUd_OFFSET64       52
#define CDR_RFUe_OFFSET64       52
#define CDR_COPYRIGHT_OFFSET64  56

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

struct GDRstruct64 {
  OFF_T RecordSize;     /* Size of current record (bytes) */
  Int32 RecordType;     /* Type of record (ie. GDR) */
  OFF_T rVDRhead;       /* File offset to first rVDR (bytes) */
  OFF_T zVDRhead;       /* CDF V2.0, value is undefined (random).
			   CDF V2.1, value = 0.
			   CDF V2.2, File offset to first zVDR (bytes). */
  OFF_T ADRhead;        /* File offset to first ADR (bytes) */
  OFF_T eof;            /* If V2.0 CDF, value is undefined.
			   If V2.1+ CDF, end-of-file (byte offset). */
  Int32 NrVars;         /* Number of rVariables */
  Int32 NumAttr;        /* Number of Attributes */
  Int32 rMaxRec;        /* Maximum rVariable record number written to. */
  Int32 rNumDims;       /* Number of CDF dimensions (for rVariables). */
  Int32 NzVars;         /* Number of zVariables. */
  OFF_T UIRhead;        /* File offset to first UIR (bytes). */
  Int32 rfuC;           /* reserved for future use (value = 0) */
  Int32 LeapSecondLastUpdated; /* Last leap second was added to the TT2000
                                  table in YYYYMMDD form. */
  Int32 rfuE;           /* reserved for future use (value = -1) */
  Int32 rDimSizes[CDF_MAX_DIMS];
			/* Size of each dimension (for rVariables). */
};
#define GDR_LEAPSECONDLASTUPDATED   14

#define GDR_RECORDSIZE_OFFSET64 0
#define GDR_RECORDTYPE_OFFSET64 8
#define GDR_rVDRHEAD_OFFSET64   12
#define GDR_zVDRHEAD_OFFSET64   20
#define GDR_ADRHEAD_OFFSET64    28
#define GDR_EOF_OFFSET64        36
#define GDR_NrVARS_OFFSET64     44
#define GDR_NUMATTR_OFFSET64    48
#define GDR_rMAXREC_OFFSET64    52
#define GDR_rNUMDIMS_OFFSET64   56
#define GDR_NzVARS_OFFSET64     60
#define GDR_UIRHEAD_OFFSET64    64
#define GDR_RFUc_OFFSET64       72
#define GDR_LEAPSECONDLASTUPDATED_OFFSET64       76
#define GDR_RFUe_OFFSET64       80
#define GDR_rDIMSIZES_OFFSET64  84

/******************************************************************************
* rVDR/zVDR structure/fields/offsets - r/zVariable Descriptor Record.
******************************************************************************/

struct VDRstruct64 {
  OFF_T RecordSize;     /* Size of current record (bytes) */
  Int32 RecordType;     /* Type of record (eg. rVDR/zVDR) */
  OFF_T VDRnext;        /* File offset to next rVDR/zVDR (bytes).  Note in
			   V2.0 CDFs this field was not set properly for the
			   last variable (rVariable). */
  Int32 DataType;       /* Data type of variable */
  Int32 MaxRec;         /* Maximum record number for variable */
  OFF_T VXRhead;        /* File offset to first VXR (bytes).  Value will be
			   0 if MULTI_FILE format */
  OFF_T VXRtail;        /* File offset to last VXR (bytes).  Value will be 0
			   if MULTI_FILE format */
  Int32 Flags;          /* Flags (see bit definitions in cdflib.h). */
  Int32 sRecords;       /* Type of sparse records.  NO_SPARSERECORDS,
			   PAD_SPARSERECORDS, or PREV_SPARSERECORDS. */
  Int32 rfuB;           /* reserved for future use (value = 0) */
  Int32 rfuC;           /* reserved for future use (value = -1) */
  Int32 rfuF;           /* reserved for future use (value = -1) */
  Int32 NumElems;       /* Number of elements of DataType (should be 1 if not
			   CDF_CHAR or CDF_UCHAR) */
  Int32 Num;            /* Variable number */
  OFF_T CPRorSPRoffset; /* CPR/SPR offset depending on bits set in `Flags'.
			   If neither compression nor sparse arrays, set to
			   0xFFFFFFFF. */
  Int32 blockingFactor; /* If uncompressed/unsparse:
			     Number of records to extend when new allocations
			     are necessary (if 0, use default).  For multi-file
			     CDFs, extensions are always one record at a time.
			     For NRV variables this value is n/a (only one
			     record is ever written).
			   If compressed/sparse:
			     Number of (or maximum) records per block... */
  char  Name[CDF_VAR_NAME_LEN256+1];
			/* Variable name */
  Int32 zNumDims;       /* Number of dimensions for zVariable.  N/a if an
			   rVariable. */
  Int32 zDimSizes[CDF_MAX_DIMS];
			/* Dimension sizes for zVariable.  N/a if an
			   rVariable. */
  Int32 DimVarys[CDF_MAX_DIMS];
			/* Dimension variances. */
			/* Optional... PadValue with the given DataType. */
};

#define VDR_RECORDSIZE_OFFSET64 0
#define VDR_RECORDTYPE_OFFSET64 8
#define VDR_VDRNEXT_OFFSET64    12
#define VDR_DATATYPE_OFFSET64   20
#define VDR_MAXREC_OFFSET64     24
#define VDR_VXRHEAD_OFFSET64    28
#define VDR_VXRTAIL_OFFSET64    36
#define VDR_FLAGS_OFFSET64      44
#define VDR_sRECORDS_OFFSET64   48
#define VDR_RFUb_OFFSET64       52
#define VDR_RFUc_OFFSET64       56
#define VDR_RFUf_OFFSET64       60
#define VDR_NUMELEMS_OFFSET64   64
#define VDR_NUM_OFFSET64        68
#define VDR_CPRorSPR_OFFSET64   72
#define VDR_BLOCKING_OFFSET64   80
#define VDR_NAME_OFFSET64       84

#define rVDR_DIMVARYS_OFFSET64  340
#define rVDR_PADVALUE_OFFSETb64 340     /* Offset base (more will be added). */

#define zVDR_zNUMDIMS_OFFSET64  340
#define zVDR_zDIMSIZES_OFFSET64 344
#define zVDR_DIMVARYS_OFFSETb64 344     /* Offset base (more will be added). */
#define zVDR_PADVALUE_OFFSETb64 344     /* Offset base (more will be added). */

#define VDR_WASTED_OFFSET64     48

/******************************************************************************
* VXR structure/fields/offsets - Variable Index Record.
******************************************************************************/

struct VXRstruct64 {
  OFF_T RecordSize;     /* Size of current record (bytes) */
  Int32 RecordType;     /* Type of record (ie. VXR) */
  OFF_T VXRnext;        /* File offset to next VXR (bytes) */
  Int32 Nentries;       /* Number of index entries (entries not being used
			   contain -1 in each field) */
  Int32 NusedEntries;   /* Number of index entries actually used */
  Int32 First[MAX_VXR_ENTRIES];
			/* First record number in VVR */
  Int32 Last[MAX_VXR_ENTRIES];
			/* Last record number in VVR */
  OFF_T Offset[MAX_VXR_ENTRIES];
			/* File offset to VXR/VVR (bytes) */
};

#define VXR_RECORDSIZE_OFFSET64 0
#define VXR_RECORDTYPE_OFFSET64 8
#define VXR_VXRNEXT_OFFSET64    12
#define VXR_NENTRIES_OFFSET64   20
#define VXR_NUSEDENTRIES_OFFSET64 24
#define VXR_FIRSTREC_OFFSET64   28

/******************************************************************************
* VVR structure/fields/offsets - Variable Values Record.
******************************************************************************/

struct VVRstruct64 {
  OFF_T RecordSize;     /* Size of current record (bytes). */
  Int32 RecordType;     /* Type of record (ie. VVR). */
			/* Bytes... */
};

#define VVR_RECORDSIZE_OFFSET64 0
#define VVR_RECORDTYPE_OFFSET64 8

/******************************************************************************
* ADR structure/fields/offsets - Attribute Descriptor Record.
******************************************************************************/

struct ADRstruct64 {
  OFF_T RecordSize;     /* Size of current record (bytes). */
  Int32 RecordType;     /* Type of record (ie. ADR). */
  OFF_T ADRnext;        /* File offset to next ADR (bytes).  Note that in
			   V2.0 CDFs this field was not set properly for the
			   last attribute. */
  OFF_T AgrEDRhead;     /* File offset to first AgrEDR (bytes). */
  Int32 Scope;          /* Variable or global. */
  Int32 Num;            /* Attribute id number. */
  Int32 NgrEntries;     /* Number of attribute gEntries/rEntries. */
  Int32 MAXgrEntry;     /* Maximum attribute gEntry/rEntry number. */
  Int32 rfuA;           /* reserved for future use (value = 0). */
  OFF_T AzEDRhead;      /* File offset to first AzEDR (bytes). */
  Int32 NzEntries;      /* Number of attribute zEntries. */
  Int32 MAXzEntry;      /* Maximum attribute zEntry number. */
  Int32 rfuE;           /* Reserved for future use (value = -1). */
  char  Name[CDF_ATTR_NAME_LEN256+1];
			/* Attribute name. */
  struct AEDRstructExt64 **grAEDRList64; 
                        /* List of grENTRYs related to an attribute. The list
                           is an array 0-MAXgrEntry, indexed by the entry
                           number. When there is no entry for the attribute,
                           the corresponding list item is NULL */
  struct AEDRstructExt64 **zAEDRList64; 
                        /* List of zENTRYs related to an attribute. The list
                           is an array 0-MAXgrEntry, indexed by the entry
                           number. When there is no entry for the attribute,
                           the corresponding list item is NULL */
};

#define ADR_RECORDSIZE_OFFSET64 0
#define ADR_RECORDTYPE_OFFSET64 8
#define ADR_ADRNEXT_OFFSET64    12
#define ADR_AgrEDRHEAD_OFFSET64 20
#define ADR_SCOPE_OFFSET64      28
#define ADR_NUM_OFFSET64        32
#define ADR_NgrENTRIES_OFFSET64 36
#define ADR_MAXgrENTRY_OFFSET64 40
#define ADR_RFUa_OFFSET64       44
#define ADR_AzEDRHEAD_OFFSET64  48
#define ADR_NzENTRIES_OFFSET64  56
#define ADR_MAXzENTRY_OFFSET64  60
#define ADR_RFUe_OFFSET64       64
#define ADR_NAME_OFFSET64       68

/******************************************************************************
* AgrEDR/AzEDR structure/fields/offsets - Attribute g/r/zEntry Descriptor
* Record.
******************************************************************************/

struct AEDRstruct64 {
  OFF_T RecordSize;     /* Size of current record (bytes) */
  Int32 RecordType;     /* Type of record (ie. AgrEDR/AzEDR) */
  OFF_T AEDRnext;       /* File offset to next AgrEDR/AzEDR.  Note that in
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
* AEDRstructExt64 - structure that extends AEDRstruct to hold value data in
* support of the READONLY metadata enhancement.
****************************************************************************/

struct AEDRstructExt64 {
    struct AEDRstruct64 AEDR;
                        /* The basic AEDR data. */
    int ValueSize;      /* The size in bytes of the value data. */
    void *Value;        /* The value data. */
};

#define AEDR_RECORDSIZE_OFFSET64 0
#define AEDR_RECORDTYPE_OFFSET64 8
#define AEDR_AEDRNEXT_OFFSET64  12
#define AEDR_ATTRNUM_OFFSET64   20
#define AEDR_DATATYPE_OFFSET64  24
#define AEDR_NUM_OFFSET64       28
#define AEDR_NUMELEMS_OFFSET64  32
#define AEDR_RFUa_OFFSET64      36
#define AEDR_RFUb_OFFSET64      40
#define AEDR_RFUc_OFFSET64      44
#define AEDR_RFUd_OFFSET64      48
#define AEDR_RFUe_OFFSET64      52
#define AEDR_VALUE_OFFSET64     56

/******************************************************************************
* CPR structure/fields/offsets - Compression Parameters Record.
******************************************************************************/

struct CPRstruct64 {
  OFF_T RecordSize;     /* Size of record (bytes) */
  Int32 RecordType;     /* Type of record (ie. CPR) */
  Int32 cType;          /* Type of compression. */
  Int32 rfuA;           /* Reserved for future use.  Set to zero. */
  Int32 pCount;         /* Parameter count. */
  Int32 cParms[CDF_MAX_PARMS];
			/* Parameters. */
};

#define CPR_RECORDSIZE_OFFSET64 0
#define CPR_RECORDTYPE_OFFSET64 8
#define CPR_CTYPE_OFFSET64      12
#define CPR_RFUa_OFFSET64       16
#define CPR_PCOUNT_OFFSET64     20
#define CPR_CPARM1_OFFSET64     24
#define CPR_CPARM2_OFFSET64     28
#define CPR_CPARM3_OFFSET64     32
#define CPR_CPARM4_OFFSET64     36
#define CPR_CPARM5_OFFSET64     40

/******************************************************************************
* SPR structure/fields/offsets - Sparseness Parameters Record.
******************************************************************************/

struct SPRstruct64 {
  OFF_T RecordSize;     /* Size of record (bytes) */
  Int32 RecordType;     /* Type of record (ie. CPR) */
  Int32 sArraysType;    /* Type of array sparseness. */
  Int32 rfuA;           /* Reserved for future use.  Set to zero. */
  Int32 pCount;         /* Parameter count. */
  Int32 sArraysParms[CDF_MAX_PARMS];
			/* Parameters. */
};

#define SPR_RECORDSIZE_OFFSET64 0
#define SPR_RECORDTYPE_OFFSET64 8
#define SPR_STYPE_OFFSET64      12
#define SPR_RFUa_OFFSET64       16
#define SPR_PCOUNT_OFFSET64     20
#define SPR_SPARM1_OFFSET64     24
#define SPR_SPARM2_OFFSET64     32
#define SPR_SPARM3_OFFSET64     32
#define SPR_SPARM4_OFFSET64     36
#define SPR_SPARM5_OFFSET64     40

/******************************************************************************
* CVVR structure/fields/offsets - Compressed Variable Values Record.
******************************************************************************/

struct CVVRstruct64 {
  OFF_T RecordSize;     /* Size of record (bytes) */
  Int32 RecordType;     /* Type of record (ie. CVVR) */
  Int32 rfuA;           /* Reserved for future use.  Set to zero (0). */
  OFF_T cSize;          /* Size in bytes of compressed data. */
			/* Bytes... */
			/* Extra... */
};

#define CVVR_RECORDSIZE_OFFSET64 0
#define CVVR_RECORDTYPE_OFFSET64 8
#define CVVR_RFUa_OFFSET64      12
#define CVVR_CSIZE_OFFSET64     16

/******************************************************************************
* UIR structure/fields/offsets - Unused Internal Record.
******************************************************************************/

struct UIRstruct64 {
  OFF_T RecordSize;     /* Size of current record (bytes). */
  Int32 RecordType;     /* Type of record (ie. UIR). */
  OFF_T NextUIR;        /* File offset of next UIR (bytes). */
  OFF_T PrevUIR;        /* File offset of previous UIR (bytes). */
};

#define UIR_RECORDSIZE_OFFSET64 0
#define UIR_RECORDTYPE_OFFSET64 8
#define UIR_NEXTUIR_OFFSET64    12
#define UIR_PREVUIR_OFFSET64    20

/******************************************************************************
* Function prototypes.
******************************************************************************/

VISIBLE_PREFIX CDFstatus ValidateCompression64 PROTOARGs((
  long cType, long *cParms
));
STATICforIDL Logical Read32s_64 PROTOARGs((vFILE *fp, Int32 *buffer, int count));
STATICforIDL Logical Read64s_64 PROTOARGs((vFILE *fp, OFF_T *buffer, int count));
STATICforIDL CDFstatus WasteIR64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T offset, OFF_T size
));
STATICforIDL CDFstatus RecordByteOffset64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 phyRecN, OFF_T *offset
));
STATICforIDL CDFstatus AllocateRecords64 VARPROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, struct AllocStruct alloc
));
STATICforIDL CDFstatus PadUnRecords64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 firstRec, Int32 lastRec
));
STATICforIDL CDFstatus CloseVarFiles64 PROTOARGs((struct CDFstruct *CDF));
STATICforIDL Logical WriteAccess64 PROTOARGs((
  struct CDFstruct *CDF, Logical forDelete, CDFstatus *pStatus
));
STATICforIDL Logical UpdateTT2000header PROTOARGs((
  struct CDFstruct *CDF, CDFstatus *pStatus
));
STATICforIDL CDFstatus WriteBuffer64 PROTOARGs((
  struct CDFstruct *CDF, vFILE *fp, Int32 dataType, Int32 numElems,
  void *buffer
));
STATICforIDL CDFstatus WriteVarElems64 PROTOARGs((
  struct VarStruct *Var, vFILE *fp, OFF_T offset, OFF_T numElems, void *buffer
));
STATICforIDL CDFstatus HyperRead64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, struct rdSTRUCT *rd,
  void *buffer
));
STATICforIDL CDFstatus HyperReadDim64 PROTOARGs((
  Int32 numDims, Int32 *dimSizes, Int32 *dimVarys, Int32 *indices,
  Int32 *counts, Int32 *intervals, Int32 *nHypDimValues, Int32 *nPhyDimValues,
  Logical *fullPhyDim, int firstDim, int dimIncr, Int32 recNum, Int32 offset,
  void *buffer, void *phyBuffer, struct CDFstruct *CDF, struct VarStruct *Var
));
STATICforIDL CDFstatus HyperWrite64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, struct rdSTRUCT *rd,
  void *buffer
));
STATICforIDL CDFstatus HyperWriteDim64 PROTOARGs((
  Int32 numDims, Int32 *dimSizes, Int32 *dimVarys, Int32 *indices,
  Int32 *counts, Int32 *intervals, Int32 *nHypDimValues, Int32 *nPhyDimValues,
  Logical *fullPhyDim, int firstDim, int dimIncr, Int32 recNum, Int32 offset,
  void *buffer, void *phyBuffer, struct CDFstruct *CDF, struct VarStruct *Var
));
STATICforIDL CDFstatus CDFcre64 PROTOARGs((
  struct VAstruct *Va, struct CurStruct *Cur
));
STATICforIDL CDFstatus CDFdel64 PROTOARGs((
  struct VAstruct *Va, struct CurStruct *Cur
));
STATICforIDL CDFstatus CDFclo64 PROTOARGs((
  struct VAstruct *Va, struct CurStruct *Cur
));
STATICforIDL CDFstatus CDFget64 PROTOARGs((
  struct VAstruct *Va, struct CurStruct *Cur
));
STATICforIDL CDFstatus CDFput1_64 PROTOARGs((
  struct VAstruct *Va, struct CurStruct *Cur
));
STATICforIDL CDFstatus CDFput2_64 PROTOARGs((
  struct VAstruct *Va, struct CurStruct *Cur
));
STATICforIDL CDFstatus CDFsel64 PROTOARGs((
  struct VAstruct *Va, struct CurStruct *Cur
));
STATICforIDL CDFstatus CDFcon64 PROTOARGs((
  struct VAstruct *Va, struct CurStruct *Cur
));
STATICforIDL CDFstatus ConfigureNEWzMode64 PROTOARGs((struct CDFstruct *CDF));
STATICforIDL void ResetReadOnlyState64 PROTOARGs((struct CDFstruct *CDF));
STATICforIDL CDFstatus UpdateNEWzMode PROTOARGs((struct CDFstruct *CDF));
STATICforIDL CDFstatus UpdateMaxRec64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 phyRecNum
));
STATICforIDL CDFstatus LocateCurrentVar64 PROTOARGs((
  struct CDFstruct *CDF, Logical zOp, OFF_T *offset, Logical *zVar,
  struct VarStruct **Var
));
STATICforIDL CDFstatus InitCurrentVar64 PROTOARGs((
  struct CDFstruct *CDF, Logical zVar, struct VarStruct **Var
));
STATICforIDL CDFstatus InitVar64 PROTOARGs((
  struct CDFstruct *CDF, Int32 varN, Logical zVar, struct VarStruct **Var
));
STATICforIDL CDFstatus InitVar2_64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var
));
STATICforIDL CDFstatus FindAttrByName64 PROTOARGs((
  struct CDFstruct *CDF, char *searchName, OFF_T *offset
));
STATICforIDL CDFstatus FindAttrByNumber64 PROTOARGs((
  struct CDFstruct *CDF, Int32 searchNum, OFF_T *offset
));
STATICforIDL CDFstatus FindEntryByNumber64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T ADRoffset, Logical zEntry, Int32 entryN,
  OFF_T *offset
));
STATICforIDL CDFstatus OpenVar64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var
));
STATICforIDL CDFstatus FindVarByName64 PROTOARGs((
  struct CDFstruct *CDF, char *searchName, OFF_T *offset, Logical *zVar,
  struct VarStruct **Var
));
STATICforIDL CDFstatus FindVarByNumber64 PROTOARGs((
  struct CDFstruct *CDF, Int32 searchNum, Logical zVar, OFF_T *offset
));
STATICforIDL CDFstatus FindLastAttr64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T *lastOffset
));
STATICforIDL CDFstatus FindLastEntry64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T ADRoffset, Logical zEntry, OFF_T *lastOffset
));
STATICforIDL CDFstatus FindPrevEntry64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T ADRoffset, OFF_T searchOffset, Logical zEntry,
  OFF_T *prevOffset
));
STATICforIDL CDFstatus CheckEntryOp64 PROTOARGs((
  struct CDFstruct *CDF, int entryType
));
STATICforIDL CDFstatus SetCURgrEntry64 PROTOARGs((
  struct CDFstruct *CDF, Logical useCurrent, Int32 entryNum
));
STATICforIDL CDFstatus SetCURzEntry64 PROTOARGs((
  struct CDFstruct *CDF, Logical useCurrent, Int32 entryNum
));
STATICforIDL CDFstatus CalcDimParms64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T offset, Logical zVar, Int32 *numDimsP,
  Int32 dimSizesP[], Int32 dimVarysP[]
));
STATICforIDL CDFstatus AllocateIR64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T size, OFF_T *offset
));
STATICforIDL CDFstatus ResizeIR64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T curOffset, OFF_T newSize, OFF_T *newOffset,
  Logical move, Logical *success
));
STATICforIDL CDFstatus RemoveUIRs64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T sOffset, OFF_T eOffset
));
STATICforIDL CDFstatus LastRecord64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T offset, Logical zVar, Int32 *recNum
));
STATICforIDL CDFstatus VerifyNoRecordsWritten64 PROTOARGs((
  struct CDFstruct *CDF, Logical *no
));
STATICforIDL CDFstatus VerifyNoPadsSpecified64 PROTOARGs((
  struct CDFstruct *CDF, Logical *no
));
STATICforIDL CDFstatus VerifyNoEntriesWritten64 PROTOARGs((
  struct CDFstruct *CDF, Logical *no
));
STATICforIDL CDFstatus ReadIrSize64 PROTOARGs((
  vFILE *fp, OFF_T offset, OFF_T *irSize
));
STATICforIDL CDFstatus ReadCDR64 VARPROTOARGs((vFILE *fp, OFF_T offset, ...));
STATICforIDL CDFstatus ReadGDR64 VARPROTOARGs((vFILE *fp, OFF_T offset, ...));
STATICforIDL CDFstatus ReadADR64 VARPROTOARGs((vFILE *fp, OFF_T offset, ...));
STATICforIDL CDFstatus ReadAEDR64 VARPROTOARGs((vFILE *fp, OFF_T offset, ...));
STATICforIDL CDFstatus ReadADRList64 PROTOARGs((vFILE *fp));
STATICforIDL CDFstatus ReadAEDRList64 PROTOARGs((
  vFILE *fp, struct AEDRstructExt64 ***AEDRList, OFF_T AEDRHead,
  Int32 NumEntries, Int32 MaxEntry
));
STATICforIDL CDFstatus ReadVDR64 VARPROTOARGs((
  struct CDFstruct *CDF, vFILE *fp, OFF_T offset, Logical zVar, ...
));
STATICforIDL CDFstatus ReadVVR64 VARPROTOARGs((vFILE *fp, OFF_T offset, ...));
STATICforIDL CDFstatus ReadCCR64 VARPROTOARGs((vFILE *fp, OFF_T offset, ...));
STATICforIDL CDFstatus ReadCPR64 VARPROTOARGs((vFILE *fp, OFF_T offset, ...));
STATICforIDL CDFstatus ReadSPR64 VARPROTOARGs((vFILE *fp, OFF_T offset, ...));
STATICforIDL CDFstatus ReadCVVR64 VARPROTOARGs((vFILE *fp, OFF_T offset, ...));
STATICforIDL CDFstatus ReadUIR64 VARPROTOARGs((vFILE *fp, OFF_T offset, ...));
STATICforIDL Logical Write32_64 PROTOARGs((vFILE *fp, Int32 *value));
STATICforIDL Logical Write32s_64 PROTOARGs((vFILE *fp, Int32 *buffer, int count));
STATICforIDL Logical Write64 PROTOARGs((vFILE *fp, OFF_T *value));
STATICforIDL Logical Write64s PROTOARGs((vFILE *fp, OFF_T *buffer, int count));
STATICforIDL CDFstatus WriteIrSize64 PROTOARGs((
  vFILE *fp, OFF_T offset, OFF_T *irSize
));
STATICforIDL CDFstatus WriteIrType64 PROTOARGs((
  vFILE  *fp, OFF_T offset, Int32 *irType
));
STATICforIDL CDFstatus WriteCDR64 VARPROTOARGs((vFILE  *fp, OFF_T offset, ...));
STATICforIDL CDFstatus WriteGDR64 VARPROTOARGs((vFILE  *fp, OFF_T offset, ...));
STATICforIDL CDFstatus WriteADR64 VARPROTOARGs((vFILE  *fp, OFF_T offset, ...));
STATICforIDL CDFstatus WriteAEDR64 VARPROTOARGs((
  struct CDFstruct *CDF, vFILE  *fp, OFF_T offset, ...
));
STATICforIDL CDFstatus WriteVDR64 VARPROTOARGs((
  struct CDFstruct *CDF, vFILE  *fp, OFF_T offset, Logical zVar, ...
));
STATICforIDL CDFstatus WriteVXR64 VARPROTOARGs((vFILE  *fp, OFF_T offset, ...));
STATICforIDL CDFstatus WriteVVR64 VARPROTOARGs((vFILE  *fp, OFF_T offset, ...));
STATICforIDL CDFstatus WriteCCR64 VARPROTOARGs((vFILE  *fp, OFF_T offset, ...));
STATICforIDL CDFstatus WriteCPR64 VARPROTOARGs((vFILE  *fp, OFF_T offset, ...));
STATICforIDL CDFstatus WriteSPR64 VARPROTOARGs((vFILE  *fp, OFF_T offset, ...));
STATICforIDL CDFstatus WriteCVVR64 VARPROTOARGs((vFILE  *fp, OFF_T offset, ...));
STATICforIDL CDFstatus WriteUIR64 VARPROTOARGs((vFILE  *fp, OFF_T offset, ...));
STATICforIDL CDFstatus CorrectBlockingFactors64 PROTOARGs((
  struct CDFstruct *CDF
));
STATICforIDL void AbortAccess64 PROTOARGs((
  struct CDFstruct *CDF, Logical updateCDF, Logical deleteCDF
));
STATICforIDL CDFstatus DeleteCDFfiles64 PROTOARGs((struct CDFstruct *CDF));
STATICforIDL CDFstatus DeleteEntry64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T aOffset, OFF_T eOffset
));
STATICforIDL CDFstatus DecompressCDF64 PROTOARGs((vFILE *dotFp, vFILE *uDotFp));
STATICforIDL CDFstatus WriteCompressedCDF64 PROTOARGs((
  struct CDFstruct *CDF, struct CPRstruct64 *CPR, Logical empty
));
STATICforIDL CDFstatus CopyCDF64 PROTOARGs((vFILE *srcFp, vFILE *destFp));
STATICforIDL CDFstatus CompressRLE0_64 PROTOARGs((
  vFILE *srcFp, OFF_T srcOffset, OFF_T srcSize, CDFstatus srcError,
  vFILE *destFp, OFF_T destOffset, OFF_T *destSize, CDFstatus destError
));
STATICforIDL CDFstatus DecompressRLE0_64 PROTOARGs((
  vFILE *srcFp, OFF_T srcOffset, OFF_T srcSize, CDFstatus srcError,
  vFILE *destFp, OFF_T destOffset, CDFstatus destError
));
STATICforIDL CDFstatus CompressHUFF0_64 PROTOARGs((
  vFILE *input, OFF_T iOffset, OFF_T iSize, CDFstatus iError,
  vFILE *oFp, OFF_T oOffset, OFF_T *oSize, CDFstatus oError
));
STATICforIDL CDFstatus DecompressHUFF0_64 PROTOARGs((
  vFILE *iFp, OFF_T iOffset, CDFstatus iError,
  vFILE *output, OFF_T oOffset, CDFstatus oError
));
STATICforIDL CDFstatus CompressAHUFF0_64 PROTOARGs((
  vFILE *input, OFF_T iOffset, OFF_T iSize, CDFstatus iError,
  vFILE *oFp, OFF_T oOffset, OFF_T *oSize, CDFstatus oError
));
STATICforIDL CDFstatus DecompressAHUFF0_64 PROTOARGs((
  vFILE *iFp, OFF_T iOffset, CDFstatus iError,
  vFILE *output, OFF_T oOffset, CDFstatus oError
));
STATICforIDL CDFstatus CompressGZIP_64 PROTOARGs((
  vFILE *srcFp, OFF_T srcOffset, OFF_T srcSize, CDFstatus srcError,
  vFILE *destFp, OFF_T destOffset, OFF_T *destSize, CDFstatus destError,
  Int32 level
));
STATICforIDL CDFstatus DecompressGZIP_64 PROTOARGs((
  vFILE *srcFp, OFF_T srcOffset, OFF_T srcSize, CDFstatus srcError,
  vFILE *destFp, OFF_T destOffset, CDFstatus destError
));
STATICforIDL CDFstatus WriteVarValues64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 recNum, Int32 offset,
  OFF_T nValues, void *buffer
));
STATICforIDL CDFstatus SearchForRecord64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T VDRoffset, Logical zVar, Int32 recNum,
  Int32 *firstRec, Int32 *lastRec, OFF_T *offset, Logical *found
));
STATICforIDL CDFstatus IndexingStatistics64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T VDRoffset, Logical zVar, Int32 *nVXRsP,
  Int32 *nEntriesP, Int32 *nAllocP, Int32 *nRecordsP, Int32 *nLevelsP
));
STATICforIDL CDFstatus DefaultPadBuffer64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, OFF_T nValues, void *buffer
));                   
STATICforIDL CDFstatus PadBuffer64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, OFF_T nValues, void *buffer
));
STATICforIDL CDFstatus BuildPadBuffer64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 nRecords, int *how,
  void **buffer, Logical encode
));
STATICforIDL CDFstatus WritePadValues64 PROTOARGs((
  struct VarStruct *Var, vFILE *fp, OFF_T offset, Int32 nRecords, int how,
  void *buffer
));
STATICforIDL CDFstatus ReadVarElems64 PROTOARGs((
  struct VarStruct *Var, vFILE *fp, OFF_T offset, OFF_T numElems, void *buffer
));
STATICforIDL CDFstatus ReadVarValues64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 recNum, Int32 offset,
  OFF_T nValues, void *buffer
));
STATICforIDL CDFstatus PrevRecord64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T VDRoffset, Logical zVar, Int32 baseRec,
  Int32 *prevRec, Logical *found
));
STATICforIDL CDFstatus NextRecord64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T VDRoffset, Logical zVar, Int32 baseRec,
  Int32 *nextRec, Logical *found
));
STATICforIDL CDFstatus VariableType64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T vdrOffset, Logical zVar, int *vType
));
STATICforIDL CDFstatus InitVarStage64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 nBytes
));
STATICforIDL CDFstatus CopyBytes64 PROTOARGs((
  vFILE *iFp, OFF_T iStart, CDFstatus iError, OFF_T nBytes, vFILE *oFp,
  OFF_T oStart, CDFstatus oError
));
STATICforIDL CDFstatus ModIndexOffset64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 firstRec, Int32 lastRec,
  OFF_T newOffset
));
STATICforIDL CDFstatus InitScratch64 PROTOARGs((
  char *scratchDir, vFILE **scratchFpH, int cacheSize
));
STATICforIDL CDFstatus CalcBF64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var
));
STATICforIDL CDFstatus CalcCompressionPct64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T vdrOffset, Logical zVar, long *cPct
));
STATICforIDL CDFstatus CalcPhyRecBytes64 PROTOARGs((
  struct CDFstruct *CDF, OFF_T vdrOffset, Logical zVar, Int32 *nPhyRecBytes
));
STATICforIDL void InitNewVXR64 PROTOARGs((
  struct VXRstruct64 *VXR, Int32 firstRec, Int32 lastRec, OFF_T offset
));
STATICforIDL CDFstatus UpdateVXRtailInVDR64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var
));
STATICforIDL CDFstatus UpdateConversions64 PROTOARGs((struct CDFstruct *CDF));
STATICforIDL CDFstatus Compress64 PROTOARGs((
  vFILE *iFp, OFF_T iOffset, OFF_T iSize, CDFstatus iError, Int32 cType,
  Int32 cParms[], vFILE *oFp, OFF_T oOffset, OFF_T *oSize, CDFstatus oError
));
STATICforIDL CDFstatus Decompress64 PROTOARGs((
  vFILE *iFp, OFF_T iOffset, OFF_T iSize, CDFstatus iError, Int32 cType,
  Int32 cParms[], vFILE *oFp, OFF_T oOffset, CDFstatus oError
));
STATICforIDL CDFstatus DecompressToStage64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, OFF_T offset, OFF_T uSize
));
STATICforIDL CDFstatus FlushStage64 PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var
));
STATICforIDL CDFstatus SetLeapSecondLastUpdate PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Logical zVar
));
STATICforIDL CDFstatus ValidateCDF64 PROTOARGs((
  struct CDFstruct *CDF, vFILE *vFp, OFF_T offset64, OFF_T fileSize,
  Logical debug
));
#if defined(__cplusplus)
  extern "C" {
#endif
VISIBLE_PREFIX Logical Read32_64 PROTOARGs((vFILE *fp, Int32 *value));
VISIBLE_PREFIX Logical Read64_64 PROTOARGs((vFILE *fp, OFF_T *value));
#if !defined(__CFM68K__) || defined(__USING_STATIC_LIBS__) || !defined(CFM68KDLL)
  VISIBLE_PREFIX CDFstatus UpdateDotCDF64 PROTOARGs((struct CDFstruct *CDF));
#endif
VISIBLE_PREFIX vFILE *V_open64 PROTOARGs((char *file_spec, char *a_mode));
VISIBLE_PREFIX int V_setcache64 PROTOARGs((vFILE *vfp, int nCacheBuffers));
VISIBLE_PREFIX int V_seek64 PROTOARGs((vFILE *vfp, OFF_T offset, int direction));
VISIBLE_PREFIX OFF_T V_tell64 PROTOARGs((vFILE *vfp));
VISIBLE_PREFIX size_t V_read64 PROTOARGs((
  void *buffer, size_t item_size, size_t n_items, vFILE *vfp
));
VISIBLE_PREFIX size_t V_write64 PROTOARGs((
  void *buffer, size_t item_size, size_t n_items, vFILE *vfp
));
VISIBLE_PREFIX int V_getc64 PROTOARGs((vFILE *fp));
VISIBLE_PREFIX int V_putc64 PROTOARGs((int value, vFILE *fp));
VISIBLE_PREFIX int V_flush64 PROTOARGs((vFILE *vfp));
VISIBLE_PREFIX int V_close64 PROTOARGs((vFILE *vfp, struct CDFstruct *CDF, 
  vSTATS *vStats));
VISIBLE_PREFIX int V_delete64 PROTOARGs((vFILE *vFp, vSTATS *vStats));
VISIBLE_PREFIX Logical FlushCache64 PROTOARGs((vFILE *vFp, 
  vCACHE *firstCache));
VISIBLE_PREFIX CDFstatus ReadVXR64 VARPROTOARGs((vFILE *fp, OFF_T offset, ...));
VISIBLE_PREFIX CDFstatus ReadIrType64 PROTOARGs((
  vFILE *fp, OFF_T offset, Int32 *irType
));
VISIBLE_PREFIX CDFstatus CheckLFS VARPROTOARGs((char *name, Logical *isLFS,
  char *fullName
));
VISIBLE_PREFIX CDFstatus CheckLFSx VARPROTOARGs((char *name, Logical *isLFS));
VISIBLE_PREFIX void CDFsetFileBackward2 PROTOARGs((
  int flag
));
VISIBLE_PREFIX long CDFChecksumMethod PROTOARGs((Int32 flags));
VISIBLE_PREFIX int CDFgetChecksumEnvVar PROTOARGs(());
VISIBLE_PREFIX CDFstatus CDFVerifyChecksum PROTOARGs((
  struct CDFstruct *CDF
));
VISIBLE_PREFIX CDFstatus CDFAddChecksum PROTOARGs((
  struct CDFstruct *CDF
));
VISIBLE_PREFIX void AddOpenCDFsCount PROTOARGs(());
VISIBLE_PREFIX void ReduceOpenCDFsCount PROTOARGs(());
VISIBLE_PREFIX int ValidateTT2000 PROTOARGs((
  int tt2000YYYYMMDD, int leapSecondLastUpdated
));
VISIBLE_PREFIX int ValidateLeapSecondLastUpdated PROTOARGs((
  int leapSecondLastUpdated
));

#if defined(__cplusplus)
  }
#endif

/*****************************************************************************/

#endif
