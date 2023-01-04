/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                               Header file for CDFleaptableinfo.
*
*  Version 1.0, 11-Apr-11, GSFC/SPDF.
*
*  Modification history:
*
*   V1.0  11-Apr-11, M Liu      Original version (for CDF V3.3.2).
*
******************************************************************************/

#if !defined(CDFLEAPTABLEINFOh_INCLUDEd__)
#define CDFLEAPTABLEINFOh_INCLUDEd__

/******************************************************************************
* Include files.
******************************************************************************/

#include "cdftools.h"

/******************************************************************************
* Parameter/qualifier constants.
******************************************************************************/

#define DUMPqual        0
#define NODUMPqual      1
#define ABOUTqual       2

/******************************************************************************
* Global variables.
******************************************************************************/
#if defined(CDFLEAPSECONDSINFO)
int dump = DEFAULTdumpLEAPTABLE;
#else
extern int dump;
#endif
/******************************************************************************
* Function Prototypes.
******************************************************************************/

Logical LeapTableInfo PROTOARGs((int argC, char *argV[]));

/*****************************************************************************/

#endif
