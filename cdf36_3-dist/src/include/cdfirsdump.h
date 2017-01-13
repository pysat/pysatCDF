/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF					Header file for CDFdump.
*
*  Version 1.5, 20-Aug-96, Hughes STX.
*
*  Modification history:
*
*   V1.0  23-Mar-92, J Love	Original version.
*   V1.1  30-Sep-92, J Love	CDF V2.3 (shareable/NeXT/zVar/IDL).
*   V1.2  11-Jan-94, J Love	CDF V2.4.
*   V1.3  21-Dec-94, J Love	CDF V2.5.
*   V1.4  28-Mar-95, J Love	POSIX.
*   V1.5  20-Aug-96, J Love	CDF V2.6.
*
******************************************************************************/

#if !defined(CDFIRsDUMPh_INCLUDEd__)
#define CDFIRsDUMPh_INCLUDEd__

/******************************************************************************
* Include files.
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"
#include "cdftools.h"

#if defined(mac)
#  include "cdfdump.rh"
#endif

/******************************************************************************
* QOP constants.
******************************************************************************/

#define CDFPATHparm		0

#define OUTPUTqual		0
#define BRIEFqual		1
#define MOSTqual		2
#define FULLqual		3
#define PAGEqual		4
#define NOPAGEqual		5
#define SIZESqual		6
#define NOSIZESqual		7
#define OFFSETqual		8
#define INDEXINGqual		9
#define NOINDEXINGqual		10
#define SUMMARYqual		11
#define NOSUMMARYqual		12
#define ABOUTqual		13
#define DECIqual		14
#define HEXAqual		15
#define DATAqual		16

/******************************************************************************
* Other constants/macros.
******************************************************************************/

#define BRIEF_          1L
#define MOST_		2L
#define FULL_           3L

#define BYTESperLINE	38

#define FULL(level) (level >= FULL_)
#define MOST(level) (level >= MOST_)

/******************************************************************************
* Function Prototypes.
******************************************************************************/

Logical DumpCDFIRs PROTOARGs((int argC, char *argV[]));

#if defined(mac)
Logical DumpQOPs PROTOARGs((int *argC, char **argV[]));
#endif

/*****************************************************************************/

#endif
