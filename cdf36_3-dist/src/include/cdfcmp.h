/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                    Header file for CDFcompare.
*
*  Version 1.6a, 17-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  23-Sep-91, J Love     Original version (for CDF V2.1).
*   V1.1  17-May-92, J Love     IBM PC port.
*   V1.2  28-Aug-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V1.3  26-Nov-93, J Love     CDF V2.4.
*   V1.4  27-Oct-94, J Love     CDF V2.5.
*   V1.4a 27-Feb-95, J Love     `value' qualifier.
*   V1.5  28-Mar-95, J Love     POSIX.
*   V1.5a 14-Sep-95, J Love     Hyper groups.
*   V1.6   2-Jul-96, J Love	CDF V2.6.
*   V1.6a 17-Nov-97, J Love	Windows NT/Visual C++.
*   V1.7  19-Jun-02, M Liu      Added the default float/double tolerance
*                               values.
*
******************************************************************************/

#if !defined(CDFCMPh_INCLUDEd__)
#define CDFCMPh_INCLUDEd__

/******************************************************************************
* Include files.
******************************************************************************/

#include "cdftools.h"
#include "cdflib.h"
#include "cdflib64.h"

#if defined(mac)
#  include "cdfcmp.rh"
#endif

/******************************************************************************
* Parameter/qualifier constants.
******************************************************************************/

#define CDFSPEC1parm    0
#define CDFSPEC2parm    1

#define LOGqual         0
#define NOLOGqual       1
#define ATTRSqual       2
#define NOATTRSqual     3
#define VARSqual        4
#define NOVARSqual      5
#define NUMBERSqual     6
#define NONUMBERSqual   7
#define PCTqual         8
#define NOPCTqual       9
#define ZMODESqual      10
#define ETCqual         11
#define NOETCqual       12
#define NEG2POSqual     13
#define NONEG2POSqual   14
#define LOCSqual        15
#define NOLOCSqual      16
#define REPORTqual      17
#define PAGEqual        18
#define NOPAGEqual      19
#define CACHEqual       20
#define STATSqual       21
#define NOSTATSqual     22
#define VALUESqual      23
#define NOVALUESqual    24
#define ABOUTqual	25
#define TOLERANCEqual   26
#define FORMATqual      27
#define NOFORMATqual    28

/******************************************************************************
* Macro constats.
******************************************************************************/

#define INDENT_CDF_STATUS       6
#define MAX_RECORD_INDICES_LEN  40
#define VAR_NAME_FIELD_LEN      (CDF_VAR_NAME_LEN + 1 + 14)
#define ENTRY_NUM_FIELD_LEN     12
#define MINnHYPERS		1
#define DEFAULT_FLOAT_TOLERANCE 1.0E-06
#define DEFAULT_DOUBLE_TOLERANCE 1.0E-09

/******************************************************************************
* Function Prototypes.
******************************************************************************/

Logical CompareCDFs PROTOARGs((int argC, char *argV[]));
Logical CompareQOPs PROTOARGs((int *argC, char **argV[]));
Logical CompareTwoCDFs PROTOARGs((char *, char *));
Logical CompareTwoCDFx PROTOARGs((char *, char *));
Logical CompareGeneral PROTOARGs((void));
Logical CompareVscopeEntries PROTOARGs((char *, Logical));
Logical CompareEntry PROTOARGs((char *, long, long, char *, int));
Logical CompareAttributes PROTOARGs((void));
Logical CompareAttributeEntries PROTOARGs((long, long, char *));
Logical CompareZvsR PROTOARGs((void));
Logical CompareVariables PROTOARGs((Logical));
Logical CompareVariableValues PROTOARGs((long, long, char *, Logical));
Logical AttrNumberMatches PROTOARGs((void));
Logical VarNumberMatches PROTOARGs((Logical));
Logical EquivalentDataTypes PROTOARGs((long, long));
Logical SameVarys PROTOARGs((long, long, long, long *, long *));
void CalcIndicesFromOffset PROTOARGs((long vOffset, Logical rowMajor,
				      long numDims, long *dimSizes,
				      long *dimVarys, long *dimIndices));
Logical StatusHandlerCmp PROTOARGs((char *, CDFstatus));
void AtRecordIndices PROTOARGs((
  struct HyperStruct *hyper, long valueN, Logical rowMajor, long numDims,
  long nValuesPerRec, long nValuesPerDim[], long *recNumber, long dimIndices[]
));
void ReportValueDifference PROTOARGs((
  long recNumber, long recVary, long numDims, long dimIndices[],
  long dimVarys[], char *v, char *varName, Byte1 *value1, Byte1 *value2,
  char *typeMsg, int lastPct, char *format1, char *format2
));
void ReportValueDifferenceTotals PROTOARGs((
  long recordDiffCount, long valueDiffCount, char *varName, char *v
));
Logical ParseTolerances PROTOARGs((
  char *, float *, Logical *, double *, Logical *
));
Logical ValuesCmp PROTOARGs((void *, void *, long, double, double));
int CheckNaN PROTOARGs((void *, void *, long));
Logical CompareStrings PROTOARGs((char *, char *, long, long));

/*****************************************************************************/

#endif
