/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF					Header file for cdfvalidate.
*
*  Version 1.0, 20-Aug-08, Perot System.
*
*  Modification history:
*
*   V1.0  20-Aug-08, M Liu 	Original version.
*
******************************************************************************/

#if !defined(CDFValidateh_INCLUDEd__)
#define CDFValidateh_INCLUDEd__

/******************************************************************************
* Include files.
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"
#include "cdftools.h"

/******************************************************************************
* QOP constants.
******************************************************************************/

#define DEBUGqual		0
#define ABOUTqual		1
#define VALIDATEqual            2
#define NOVALIDATEqual          3
#define QUIETqual               4
#define NOQUIETqual             5

/******************************************************************************
* Global variables.
******************************************************************************/
#if defined(CDFVALIDATE)
Logical useValidate = DEFAULTvalidateValidate;
Logical debug = DEFAULTdebugValidate;
Logical quiet = DEFAULTquietValidate;
#else
extern Logical useValidate;
extern Logical debug;
extern Logical quiet;
#endif

/******************************************************************************
* Function Prototypes.
******************************************************************************/

Logical ValidateCDFs PROTOARGs((int argC, char *argV[]));

/*****************************************************************************/

#endif
