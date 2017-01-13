/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF					Header file for CDFinquire.
*
*  Version 1.6, 27-Aug-96, Hughes STX.
*
*  Modification history:
*
*   V1.0  23-Mar-92, J Love	Original version.
*   V1.1  22-Jun-92, J Love	CDF V2.3 (shareable/NeXT/zVars).
*   V1.2  29-Oct-92, J Love	Added variable values display.
*   V1.3  26-Nov-93, J Love	CDF V2.4.
*   V1.4  13-Dec-94, J Love	CDF V2.5.
*   V1.5  28-Mar-95, J Love	POSIX.
*   V1.6  27-Aug-96, J Love	CDF V2.6.
*
******************************************************************************/

#if !defined(CDFINQh_INCLUDEd__)
#define CDFINQh_INCLUDEd__

/******************************************************************************
* Include files.
******************************************************************************/

#include "cdftools.h"

#if defined(mac)
#  include "cdfinq.rh"
#endif

/******************************************************************************
* Parameter/qualifier constants.
******************************************************************************/

#define IDqual		0
#define PAGEqual	1
#define NOPAGEqual	2
#define ABOUTqual	3

/******************************************************************************
* Function Prototypes.
******************************************************************************/

Logical InquireCDF PROTOARGs((int argC, char *argV[]));
Logical InquireQOPs PROTOARGs((int *argC, char **argV[]));
void DisplayVerRelInc PROTOARGs((void));
void DisplayToolkitDefaults PROTOARGs((void));
void DisplayCacheDefaults PROTOARGs((void));
void DisplayBlockingDefaults PROTOARGs((void));
void DisplayPadValueDefaults PROTOARGs((void));
void DisplayMiscDefaults PROTOARGs((void));

/*****************************************************************************/

#endif
