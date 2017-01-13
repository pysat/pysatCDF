/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF					Header file for CDFstats.
*
*  Version 1.7a, 18-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  23-Sep-91, J Love	Original version (for CDF V2.1).
*   V1.1  23-Mar-92, J Love	CDF V2.2.  Added fill value filtering.
*   V1.2  11-Jun-92, J Love	CDF V2.3 (shareable/NeXT).
*   V1.3  26-Nov-93, J Love	CDF V2.4.
*   V1.4  13-Dec-94, J Love	CDF V2.5.
*   V1.5  28-Mar-95, J Love	POSIX.
*   V1.5a 15-May-95, J Love	Prototypes for `ASSIGNx', `EQx', etc. moved
*				to `cdftools.h'.
*   V1.6  14-Sep-95, J Love	Hyper groups.
*   V1.7   1-Aug-96, J Love	CDF V2.6.
*   V1.7a 18-Nov-97, J Love	Windows NT/Visual C++.
*
******************************************************************************/

#if !defined(CDFSTATSh_INCLUDEd__)
#define CDFSTATSh_INCLUDEd__

/******************************************************************************
* Include files.
******************************************************************************/

#include "cdftools.h"

#if defined(mac)
#  include "cdfstats.rh"
#endif

/******************************************************************************
* QOP constants.
******************************************************************************/

#define CDFPATHparm		0

#define RANGEqual		0
#define NORANGEqual		1
#define OUTPUTqual		2
#define FILLqual		3
#define NOFILLqual		4
#define FORMATqual		5
#define NOFORMATqual		6
#define PAGEqual		7
#define NOPAGEqual		8
#define UPDATE_VALIDSqual	9
#define NOUPDATE_VALIDSqual	10
#define UPDATE_SCALESqual	11
#define NOUPDATE_SCALESqual	12
#define ZMODEqual		13
#define NEG2POSFP0qual		14
#define NONEG2POSFP0qual	15
#define UPDATE_MONOTONICqual	16
#define NOUPDATE_MONOTONICqual	17
#define REPORTqual		18
#define CACHEqual		19
#define ABOUTqual		20
#define STATSqual		21
#define NOSTATSqual		22
#define EPOCH_MONOTONICqual	23

/******************************************************************************
* Constants.
******************************************************************************/

#define MONOTON_RESULT_LEN	8
#define MINnHYPERS		1

/******************************************************************************
* Enumerators.
******************************************************************************/

enum monoStatesENUM { _Init, _Steady, _Increase, _Decrease, _noIncrease,
		      _noDecrease, _False };

/******************************************************************************
* Global variables.
******************************************************************************/

#if defined(CDFSTATS)
Logical updateValids;
Logical updateScales;
Logical updateMonotonic;
Logical useFormat;
Logical rangeCheck;
Logical ignoreFills;
Logical epochMonotonic;
FILE *OUTfp;
long validminAttrN;			/* attribute number */
long validmaxAttrN;			/* attribute number */
long fillvalAttrN;
long formatAttrN;
long majority;
#else
extern Logical updateValids;
extern Logical updateScales;
extern Logical updateMonotonic;
extern Logical useFormat;
extern Logical rangeCheck;
extern Logical ignoreFills;
extern Logical epochMonotonic;
extern FILE *OUTfp;
extern long validminAttrN;			/* attribute number */
extern long validmaxAttrN;			/* attribute number */
extern long fillvalAttrN;
extern long formatAttrN;
extern long majority;
#endif

/******************************************************************************
* VarStruct.
******************************************************************************/

struct VarStruct {
  Logical Z;
  long varN;
  char varName[CDF_VAR_NAME_LEN256+1];
  long dataTypeV;
  long numElemsV;
  long numDims;
  long dimSizes[CDF_MAX_DIMS];
  long recVary;
  long dimVarys[CDF_MAX_DIMS];
  long varMaxRec;
  Logical checkMonotonicVar;
  Logical rangeCheckVar;
  Logical ignoreFillsVar;
  char *format;
  void *validmin;
  void *validmax;
  void *fillval;
  Byte1 *value;
  Byte1 *buffer;
  void *min, *max, *last;
  void *minINrange, *maxINrange;
  long low, high;
  long fills;
  Logical oneINrange;
  Logical minmaxInited;
  Logical monoInited;
  Logical hyper;
  enum monoStatesENUM monoState;
  long nValueBytes;
};

/******************************************************************************
* Function Prototypes.
******************************************************************************/

Logical CalculateStatistics PROTOARGs((int argC, char *argV[]));
Logical StatisticsQOPs PROTOARGs((int *argC, char **argV[]));
void MinMaxInit PROTOARGs((struct VarStruct *));
void MinMaxCheck PROTOARGs((struct VarStruct *));
void Monotonic PROTOARGs((struct VarStruct *));
void DisplayMin PROTOARGs((struct VarStruct *));
void DisplayMax PROTOARGs((struct VarStruct *));
void DisplayFill PROTOARGs((struct VarStruct *));
Logical SetupVar PROTOARGs((struct VarStruct *));
Logical CALCstat PROTOARGs((struct VarStruct *, int));
void DISPstat PROTOARGs((struct VarStruct *, int));
void FreeVarMem PROTOARGs((struct VarStruct *Var));
Logical StatusHandlerStats PROTOARGs((CDFstatus));
Logical HandleVirtual PROTOARGs((struct VarStruct *Var));

/*****************************************************************************/

#endif
