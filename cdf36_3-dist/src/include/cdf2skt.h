/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF					Header file for SkeletonTable.
*
*  Version 1.5a, 18-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  18-May-92, J Love	Original version.
*   V1.1  25-Jun-92, J Love	CDF V2.3 (shareable/Next/zVar).
*   V1.2  26-Nov-93, J Love	CDF V2.4.
*   V1.3  19-Sep-94, J Love	CDF V2.5.
*   V1.4  28-Mar-95, J Love	POSIX.
*   V1.5   1-Aug-96, J Love	CDF V2.6.
*   V1.5a 18-Nov-97, J Love	Windows NT/Visual C++.
*
******************************************************************************/

#if !defined(CDF2SKTh_INCLUDEd__)
#define CDF2SKTh_INCLUDEd__

/******************************************************************************
* Include files.
******************************************************************************/

#include "cdflib.h"
#include "cdftools.h"

#if defined(mac)
#  include "cdf2skt.rh"
#endif

/******************************************************************************
* Parameter/qualifier constants.
******************************************************************************/

#define CDFPATHparm	0

#define SKTqual		0
#define NONRVqual	1
#define NRVFILEqual	2
#define NRVTABLEqual	3
#define LOGqual		4
#define NOLOGqual	5
#define ZMODEqual	6
#define NEG2POSqual	7
#define NONEG2POSqual	8
#define FORMATqual	9
#define NOFORMATqual	10
#define REPORTqual	11
#define SCREENqual	12
#define NOSCREENqual	13
#define CACHEqual	14
#define VALUESqual	15
#define PAGEqual	16
#define NOPAGEqual	17
#define ABOUTqual	18
#define STATSqual	19
#define NOSTATSqual	20

/******************************************************************************
* Macro constants.
******************************************************************************/

#define MAX_LINE_LEN		79
#define MAX_COL_TO_USE		(MAX_LINE_LEN - 1)
#define ENTRY_NUM_COL		20
#define ENTRY_NUM_WIDTH		5
#define VAR_DATATYPE_COL	18
#define ENTRY_DATATYPE_COL	18
#define VAR_NUMELEMS_WIDTH	5
#define zVAR_NUMDIMS_WIDTH	2
#define MAX_SIZES_LEN		80
#define MAX_VARYS_LEN		80
#define MAX_TEXT_LEN		80

/******************************************************************************
* Padding macros.
******************************************************************************/

#define FRONTpadLABEL(label,value) \
((size_t) strlen(label) < (size_t) strlen(value) ? \
(strlen(value) - strlen(label)) / (size_t) 2 : 0)

#define BACKpadLABEL(label,value) \
((size_t) strlen(label) < (size_t) strlen(value) ? \
(strlen(value) - strlen(label) + (size_t) 1) / (size_t) 2 : 0)

#define FRONTpadVALUE(label,value) \
((size_t) strlen(value) < (size_t) strlen(label) ? \
(strlen(label) - strlen(value) + (size_t) 1) / (size_t) 2 : 0)

#define BACKpadVALUE(label,value) \
((size_t) strlen(value) < (size_t) strlen(label) ? \
(strlen(label) - strlen(value)) / (size_t) 2 : 0)

/******************************************************************************
* Function Prototypes.
******************************************************************************/

#if defined(__cplusplus)
extern "C" {
#endif
Logical CreateSkeletonTable PROTOARGs((int argC, char *argV[]));
#if defined(__cplusplus)
}
#endif

Logical SkeletonTableQOPs PROTOARGs((int *argC, char **argV[]));
Logical WriteHeader PROTOARGs((char *CDFname, int varValues, char *variables));
Logical WriteGlobalAttr PROTOARGs((void));
Logical WriteVarAttr PROTOARGs((void));
Logical WriteVars PROTOARGs((int varValues, char *variables));
Logical WriteVar PROTOARGs((long varN, int varValues, char *variables,
			    Logical zVar));
Logical WriteEnd PROTOARGs((void));
Logical WriteVariableData PROTOARGs((FILE *fp, Logical zVar, long varN,
				     long dataType, long numElements,
				     long numDims, long dimSizes[],
				     long recVary, long dimVarys[],
				     long maxRec));
Logical VariableSelected PROTOARGs((char *varName, char *variables));
char *DataTypePaddedString PROTOARGs((long dataType));
Logical StatusHandlerC2S PROTOARGs((CDFstatus status));

Logical WriteTT2000VarData PROTOARGs((int varValues, char *variables));
/*****************************************************************************/

#endif
