/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                    INTERNAL interface to CDF.
*
*  Version 2.8b, 8-Mar-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  22-Jan-91, R Kulkarni		Original version (for CDF V2.0).
*		     J Love
*   V2.0   6-Jun-91, J Love             Renamed (was cdfcore.c). Modified for 
*                                       V2.1 style INTERNAL interface (and
*                                       fixes to CDF V2.0 distribution).  Also
*                                       renamed some symbols for clarity.
*   V2.1  24-Jun-91, J Love             Fixed PUT_,VAR_INITIALRECS.  Stripped
*                                       trailing blanks off of CDF names. 
*                                       Allow variable data type to be changed
*                                       if "equivalent".  Added CDF_EPOCH as
*                                       a data type.
*   V2.2   8-Aug-91, J Love             Don't use #elif (for SGi port) and
*                                       added support for Cray/UNICOS.
*                                       INTERNAL i/f function renamed 'CDFlib'.
*                                       Different numbers of CACHE buffers for
*                                       different file types (VIO).  Modified
*                                       hyper access (and fixed part of it).
*                                       Changed attribute entry access.  Check
*                                       for supported encoding when opening a
*                                       CDF.
*   V2.3  20-May-92, J Love             Changed for IBM-PC port (split into
*                                       smaller source files, etc.).  Made
*                                       shareable.
*   V2.4  21-Aug-92, J Love             CDF V2.3 (shareable/NeXT/zVar).
*   V2.5  19-Jan-94, J Love             CDF V2.4.
*   V2.6  15-Dec-94, J Love             CDF V2.5.
*   V2.6a 24-Feb-95, J Love		Solaris 2.3 IDL i/f.
*   V2.7  19-Jul-95, J Love		Virtual memory (Microsoft C 7.00).
*   V2.7a 22-Sep-95, J Love		Changed virtual memory control.
*   V2.8  19-Aug-96, J Love		CDF V2.6.
*   V2.8a 14-Feb-97, J Love		Fixed `va_arg' usage.
*   V2.8b  8-Mar-97, J Love		Windows NT for MS Visual C++ 4 on an
*					IBM PC.
*
******************************************************************************/

#define CDFLIB
#include "cdflib.h"
#include "cdflib64.h"

#if defined(BUILDINGforIDL)
#define CUR Cur
#else
static struct CurStruct Cur = { RESERVED_CDFID, RESERVED_CDFSTATUS };
#define CUR &Cur
#endif

#if defined(MICROSOFTC_700) && INCLUDEvMEMORY
static Logical first = TRUE;
#endif

static Logical second = TRUE;

/******************************************************************************
* CDFlib.
******************************************************************************/

#if defined(STDARG)
#if defined(BUILDINGforIDL)
VISIBLE_PREFIX CDFstatus CDFlib (struct CurStruct *Cur, long requiredFnc, ...)
#else
VISIBLE_PREFIX CDFstatus CDFlib (long requiredFnc, ...)
#endif
#else
VISIBLE_PREFIX CDFstatus CDFlib (va_alist)
va_dcl
#endif
{
  CDFstatus pStatus = CDF_OK;
  struct VAstruct Va;
#if defined(BUILDINGforIDL)
#if defined(VARARGS)
  struct CurStruct *Cur;
#endif
#endif
  /****************************************************************************
  * Process variable length argument list.
  ****************************************************************************/
#if defined(STDARG)
  va_start (Va.ap, requiredFnc);
  Va.fnc = requiredFnc;
#else
  VA_START (Va.ap);
#if defined(BUILDINGforIDL)
  Cur = va_arg (Va.ap, struct CurStruct *);
#endif
  Va.fnc = va_arg (Va.ap, long);
#endif
  /****************************************************************************
  * Initialize virtual memory parameters for Microsoft C 7.00.
  ****************************************************************************/
#if defined(MICROSOFTC_700) && INCLUDEvMEMORY
  if (first) {
    char *vsize = getenv("CDF_VSIZE");
    if (vsize != NULL) {
      long nBytes;
      if (sscanf(vsize,"%ld",&nBytes) == 1) {
        switch (nBytes) {
	  case 0:
	    useVmem = FALSE;
	    break;
	  case DEFAULT_vMEMSIZE:
	    useVmem = TRUE;
	    vMemSize = DEFAULT_vMEMSIZE;
	    break;
	  default:
	    useVmem = FALSE;	/* Until the mystery is solved... */
	    vMemSize = nBytes;
        }
        if (useVmem) {
	  char *vmask = getenv("CDF_VMASK");
	  if (vmask != NULL) {
	    uInt4 tMask = 0;
	    if (strstr(vmask,"EMS") != NULL ||
	        strstr(vmask,"ems") != NULL) tMask |= _VM_EMS;
	    if (strstr(vmask,"XMS") != NULL ||
	        strstr(vmask,"xms") != NULL) tMask |= _VM_XMS;
	    if (strstr(vmask,"DISK") != NULL ||
	        strstr(vmask,"disk") != NULL) tMask |= _VM_DISK;
	    if (tMask != 0) vMemMask = tMask;
          }
        }
      }
    }
    first = FALSE;
  }
#endif

  /****************************************************************************
  * Scan list of arguments.
  ****************************************************************************/
  while (Va.fnc != NULL_) {
    switch (Va.fnc) {
      case CREATE_:
        for (;;) {
	   Va.item = va_arg (Va.ap, long);
	   if (second) {
	     if (CDFgetFileBackwardEnvVar() == 1) {
		 CDFsetFileBackward2 (BACKWARDFILEon);
	     }
	     second = FALSE;
	   }
           if (Va.item == CDF_ && CDFgetFileBackward() == 1) {
             if (!sX(CDFcre(&Va,CUR),&pStatus)) return pStatus;
           } else if (Va.item == CDF_ && CDFgetFileBackward() == 0) {
             if (!sX(CDFcre64(&Va,CUR),&pStatus)) return pStatus;
	   } else {
             if (*CUR.cdf != NULL && !isLFS(*CUR.cdf)) {
               if (!sX(CDFcre(&Va,CUR),&pStatus)) return pStatus;
             } else {
               if (!sX(CDFcre64(&Va,CUR),&pStatus)) return pStatus;
             }
           }
	   if (Va.fnc == Va.item) break;
        }
        break;
      case OPEN_:
        for (;;) {
	   Va.item = va_arg (Va.ap, long);
	   if (!sX(CDFope(&Va,CUR),&pStatus)) return pStatus;
	   if (Va.fnc == Va.item) break;
        }
        break;
      case DELETE_:
        for (;;) {
	   Va.item = va_arg (Va.ap, long);
	   if (*CUR.cdf != NULL && !isLFS(*CUR.cdf)) {
	     if (!sX(CDFdel(&Va,CUR),&pStatus)) return pStatus;
	   } else {
	     if (!sX(CDFdel64(&Va,CUR),&pStatus)) return pStatus;
	   }
	   if (Va.fnc == Va.item) break;
        }
        break;
      case CLOSE_:
        for (;;) {
	   Va.item = va_arg (Va.ap, long);
	   if (*CUR.cdf != NULL && !isLFS(*CUR.cdf)) {
	     if (!sX(CDFclo(&Va,CUR),&pStatus)) return pStatus;
	   } else {
	     if (!sX(CDFclo64(&Va,CUR),&pStatus)) return pStatus;
	   }
	   if (Va.fnc == Va.item) break;
        }
        break;
      case SELECT_:
        for (;;) {
	   Va.item = va_arg (Va.ap, long);
	   if (*CUR.cdf != NULL && !isLFS(*CUR.cdf)) {
	     if (!sX(CDFsel(&Va,CUR),&pStatus)) return pStatus;
	   } else {
	     if (!sX(CDFsel64(&Va,CUR),&pStatus)) return pStatus;
	   }
	   if (Va.fnc == Va.item) break;
        }
        break;
      case CONFIRM_:
        for (;;) {
	   Va.item = va_arg (Va.ap, long);
	   if (*CUR.cdf != NULL && !isLFS(*CUR.cdf)) {
	     if (!sX(CDFcon(&Va,CUR),&pStatus)) return pStatus;
	   } else {
	     if (!sX(CDFcon64(&Va,CUR),&pStatus)) return pStatus;
	   }
	   if (Va.fnc == Va.item) break;
        }
        break;
      case GET_:
        for (;;) {
	   Va.item = va_arg (Va.ap, long);
           if (Va.item == CDF_INFO_) {
             if (!sX(CDFget64(&Va,CUR),&pStatus)) return pStatus;
           } else if (*CUR.cdf != NULL && !isLFS(*CUR.cdf)) {
	     if (!sX(CDFget(&Va,CUR),&pStatus)) return pStatus;
	   } else {
	     if (!sX(CDFget64(&Va,CUR),&pStatus)) return pStatus;
	   }
	   if (Va.fnc == Va.item) break;
        }
        break;
      case PUT_:
        for (;;) {
	   Va.item = va_arg (Va.ap, long);
	   if (*CUR.cdf != NULL && !isLFS(*CUR.cdf)) {
	     if (!sX(CDFput1(&Va,CUR),&pStatus)) return pStatus;
	   } else {
	     if (!sX(CDFput1_64(&Va,CUR),&pStatus)) return pStatus;
	   }
	   if (Va.fnc == Va.item) break;
        }
        break;

      case SAVE_:
        for (;;) {
           Va.item = va_arg (Va.ap, long);
           if (!sX(CDFsav(&Va,CUR),&pStatus)) return pStatus;
           if (Va.fnc == Va.item) break;
        }
        break;

      default: {
        va_end (Va.ap);
        return BAD_FNC_OR_ITEM;
      }
    }
  }
  va_end (Va.ap);
  return pStatus;
}

