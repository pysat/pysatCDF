/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF					`LibMain' for Windows DLL.
*
*  Version 1.1, 30-Aug-96, Hughes STX.
*
*  Modification history:
*
*   V1.0  15-Nov-94, J Love	Original version.
*   V1.1  30-Aug-96, J Love	CDF V2.6.  Include `cdf.h' rather than
*				`cdflib.h'.  Including `cdflib.h' caused
*				the Borland C 3.0 compiler to crash.
*
******************************************************************************/

#include <windows.h>
#include "cdf.h"

int FAR PASCAL LibMain (HANDLE hInstance, WORD wDataSeg,
			WORD cbHeapSize, LPSTR lpCmdLine) {
  CDFlib (SELECT_, CDF_, RESERVED_CDFID,
		   CDF_STATUS_, RESERVED_CDFSTATUS,
	  NULL_);
  /* `UnlockData' not needed - data segment FIXED. */
  return 1;
}
