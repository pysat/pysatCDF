/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF					Header file for CDFconvert.
*
*  Version 1.5a, 17-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  17-May-92, J Love	Original version.
*   V1.1  25-Jun-92, J Love	CDF V2.3 (shareable/NeXT/zVar).
*   V1.2  26-Nov-93, J Love	CDF V2.4.
*   V1.3  13-Dec-94, J Love	CDF V2.5.
*   V1.4  28-Mar-95, J Love	POSIX.
*   V1.4a 14-Sep-95, J Love	CDFexport-related changes.
*   V1.5   2-Jul-96, J Love	CDF V2.6.
*   V1.5a 17-Nov-97, J Love	Windows NT/Visual C++.
*
******************************************************************************/

#if !defined(CDFCVTh_INCLUDEd__)
#define CDFCVTh_INCLUDEd__

/******************************************************************************
* Include files.
******************************************************************************/

#include "cdftools.h"
#include "cdflib.h"
#include "cdflib64.h"

#if defined(mac)
#  include "cdfcvt.rh"
#endif

/******************************************************************************
* Parameter/qualifier constants.
******************************************************************************/

#define CDFSPEC1parm	0
#define CDFSPEC2parm	1

#define SINGLEqual	0
#define MULTIqual	1
#define ROWqual		2
#define COLqual		3
#define HOSTqual	4
#define NETqual		5
#define SKELqual	6
#define LOGqual		7
#define NOLOGqual	8
#define PCTqual		9
#define NOPCTqual	10
#define DELqual		11
#define NODELqual	12
#define ZMODEqual	13
#define NEG2POSqual	14
#define NONEG2POSqual	15
#define REPORTqual	16
#define PAGEqual	17
#define NOPAGEqual	18
#define CACHEqual	19
#define STATSqual	20
#define NOSTATSqual	21
#define ENCODINGqual	22
#define COMPRESSqual	23
#define SPARSEqual	24
#define ABOUTqual	25
#define BACKWARDqual    26
#define CHECKSUMqual    27
#define EPOCH2TT2000qual 28
#define TT20002EPOCHqual 29
#define TT20002EPOCH16qual 30
#define SORTqual        31
#define BLOCKINGFACTORqual 32
#define ADJUSTTT2000qual 33
#define LEAPSECONDLASTUPDATEDqual 34
#define COMPRESSNONEPOCHqual 35

/******************************************************************************
* Macro constants.
******************************************************************************/

#define MAX_COL_TO_USE		70
#define INDENT_CDF_STATUS	6
#define MAX_OUTPUT_TEXT_LEN	512
#define MINnHYPERS		1

#define SOURCEformat		(-1L)
#define SOURCEmajority		(-1L)
#define SOURCEencoding		(-1L)
#define SOURCEcompression	(-1L)
#define SOURCEsparseRECORDS	(-1L)
#define SOURCEsparseARRAYS	(-1L)

#define EPOCH2TT2000_		1L
#define TT20002EPOCH_		2L
#define TT20002EPOCH16_		3L

/******************************************************************************
* PCTx.
******************************************************************************/

#define PCTx(pct,toRec,maxRec) ((int)((pct*(toRec+1))/(maxRec+1)))

/******************************************************************************
* Sparseness/compression structures.
******************************************************************************/

struct SparseRecordsStruct {
  struct {
    long sRecordsType;
  } VARs;
  struct sRecordsVarStruct {
    char *name;
    long sRecordsType;
    struct sRecordsVarStruct *next;
  } *VARhead;
};

struct SparseArraysStruct {
  struct {
    long sArraysType;
    long sArraysParms[CDF_MAX_PARMS];
  } VARs;
  struct sArraysVarStruct {
    char *name;
    long sArraysType;
    long sArraysParms[CDF_MAX_PARMS];
    struct sArraysVarStruct *next;
  } *VARhead;
};

struct CompressionStruct {
  struct {
    long cType;
    long cParms[CDF_MAX_PARMS];
  } CDF;
  struct {
    long cType;
    long cParms[CDF_MAX_PARMS];
    long bf;
    long reserve;
  } VARs;
  struct cVarStruct {
    char *name;
    long cType;
    long cParms[CDF_MAX_PARMS];
    long bf;
    long reserve;
    struct cVarStruct *next;
  } *VARhead;
};

/******************************************************************************
* Function Prototypes.
******************************************************************************/

void CheckEnteredVarNames PROTOARGs((void));
Logical ConvertCDFs PROTOARGs((int argC, char *argV[]));
Logical ConvertCDF PROTOARGs((char *, char *));
Logical ConvertCDFx PROTOARGs((char *, char *));
Logical ConvertAttributes PROTOARGs((void));
Logical ConvertEntry PROTOARGs((char *, long, int));
Logical ConvertVariables PROTOARGs((long, long));
Logical ConvertVariable PROTOARGs((long, Logical, long, long));
struct CompressionStruct *ParseCompressionOptions PROTOARGs((char *options));
Logical ParseCompressionTokenAndBF PROTOARGs((
  char **p1, char **p2, long *cType, long cParms[CDF_MAX_PARMS], long *bf,
  long *reserve, Logical *done
));
struct SparseRecordsStruct *ParseSparsenessOptions PROTOARGs((char *options));
Logical ParseSparsenessToken PROTOARGs((
  char **p1, char **p2, long *sRecordsType, Logical *done
));
void FreeSparseness PROTOARGs((struct SparseRecordsStruct *sparseRecords));
void FreeCompression PROTOARGs((struct CompressionStruct *compression));
Logical StatusHandlerCvt PROTOARGs((char *, CDFstatus));

#if defined(mac)
Logical ConvertQOPs PROTOARGs((int *argC, char **argV[]));
#endif

/*****************************************************************************/

#endif
