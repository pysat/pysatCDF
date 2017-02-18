/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                            Validate CDF things.
*
*  Version 1.9c, 14-Dec-97, Hughes STX.
*
*  Modification history:
*
*   V1.0   3-Jun-91, J Love     Original version (for CDF V2.1).  This is a
*                               combination of validcdfname.c, validvarname.c,
*                               validattrname.c, validdatatype.c,
*                               validattrscope.c, and validencoding.
*   V1.1  10-Jun-91, J Love     Don't allow blanks in VMS CDF names.  Check
*                               all characters in CDF, attr., and var. names.
*   V1.2  28-Jun-91, J Love     Housekeeping.
*   V1.3  15-Sep-91, J Love     Changed for IBM-PC port.
*   V1.4   4-Oct-91, J Love     Changed for IBM-RS6000 port (and IBM-PC).
*   V1.5  23-Mar-92, J Love     HP port/CDF V2.2.
*   V1.6   5-Oct-92, J Love     Added NeXT to `validEncoding' (better late
*                               than never).
*   V1.7  25-Jan-94, J Love     CDF V2.4.
*   V1.7a  4-Feb-94, J Love     DEC Alpha/OpenVMS port.
*   V1.8  13-Dec-94, J Love     CDF V2.5.
*   V1.8a 24-Feb-95, J Love     Solaris 2.3 IDL i/f.
*   V1.9  27-Aug-96, J Love     CDF V2.6.
*   V1.9a 21-Feb-97, J Love	Removed RICE.
*   V1.9b 28-Feb-97, J Love	Windows NT for MS Visual C/C++ on an IBM PC.
*   V1.9c 14-Dec-97, J Love	Added ALPHAVMSi encoding.
*   V1.10 18-Feb-10, M Liu 	Modified ValidAttrScope to allow the assumed
*                               global/variable attribute in the older file.
*   V1.11 10-Feb-12, M Liu 	Added InValidDataType function.
*
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"

/******************************************************************************
* Validate CDF name.
******************************************************************************/

VISIBLE_PREFIX Logical ValidCDFname (name)
char *name;
{
  size_t len = strlen(name);
  int i;
  /****************************************************************************
  * Length must be at least one.
  ****************************************************************************/
  if (len < 1) return FALSE;
  /****************************************************************************
  * All characters must be printable.
  ****************************************************************************/
  for (i = 0; i < (int) len; i++) {
     if (!Printable((int)name[i])) return FALSE;
  }
  /****************************************************************************
  * Passed all tests - return TRUE.
  ****************************************************************************/
  return TRUE;
}

/******************************************************************************
* Validate variable name.
******************************************************************************/

VISIBLE_PREFIX Logical ValidVarName (name)
char *name;
{
  size_t len;     /* length of name */
  size_t i;
  /****************************************************************************
  * Length must be at least one.
  ****************************************************************************/
  len = strlen(name);
  if (len < 1) return FALSE;      /* length must be at least one */
  /****************************************************************************
  * All characters must be printable.
  ****************************************************************************/
  for (i = 0; i < len; i++) {
     if (!Printable((int)name[i])) return FALSE;
  }
  /****************************************************************************
  * Passed all tests - return TRUE.
  ****************************************************************************/
  return TRUE;
}

/******************************************************************************
* Validate attribute name.
******************************************************************************/

VISIBLE_PREFIX Logical ValidAttrName (name)
char *name;
{
  size_t len;     /* length of name */
  size_t i;
  /****************************************************************************
  * Length must be at least one.
  ****************************************************************************/
  len = strlen(name);
  if (len < 1) return FALSE;      /* length must be at least one */
  /****************************************************************************
  * All characters must be printable.
  ****************************************************************************/
  for (i = 0; i < len; i++) {
     if (!Printable((int)name[i])) return FALSE;
  }
  /****************************************************************************
  * Passed all tests - return TRUE.
  ****************************************************************************/
  return TRUE;
}

/******************************************************************************
* Validate data type.
******************************************************************************/

VISIBLE_PREFIX Logical ValidDataType (dataType)
Int32 dataType;
{
  switch (dataType) {
    case CDF_INT1: return TRUE;
    case CDF_INT2: return TRUE;
    case CDF_INT4: return TRUE;
    case CDF_INT8: return TRUE;
    case CDF_UINT1: return TRUE;
    case CDF_UINT2: return TRUE;
    case CDF_UINT4: return TRUE;
    case CDF_REAL4: return TRUE;
    case CDF_REAL8: return TRUE;
    case CDF_CHAR: return TRUE;
    case CDF_UCHAR: return TRUE;
    case CDF_BYTE: return TRUE;
    case CDF_FLOAT: return TRUE;
    case CDF_DOUBLE: return TRUE;
    case CDF_EPOCH: return TRUE;
    case CDF_EPOCH16: return TRUE;
    case CDF_TIME_TT2000: return TRUE;
  }
  return FALSE;
}

/******************************************************************************
* InValidate data type.
* Data types added in V3.* are invalid for V2.* file. 
******************************************************************************/

VISIBLE_PREFIX Logical InValidDataType (dataType)
Int32 dataType;
{
  switch (dataType) {
    case CDF_INT8: return TRUE;
    case CDF_EPOCH16: return TRUE;
    case CDF_TIME_TT2000: return TRUE;
  }
  return FALSE;
}

/******************************************************************************
* Validate attribute scope.
******************************************************************************/

VISIBLE_PREFIX Logical ValidAttrScope (scope)
Int32 scope;
{
  switch (scope) {
    case GLOBAL_SCOPE: return TRUE;
    case VARIABLE_SCOPE: return TRUE;
    case GLOBALscopeASSUMED: return TRUE;
    case VARIABLEscopeASSUMED: return TRUE;
  }
  return FALSE;
}

/******************************************************************************
* Validate encoding.
******************************************************************************/

VISIBLE_PREFIX Logical ValidEncoding (encoding, actualEncoding)
Int32 encoding;
Int32 *actualEncoding;
{
  switch (encoding) {
    case HOST_ENCODING:
      *actualEncoding = HostEncoding();
      break;
    case NETWORK_ENCODING:
    case SUN_ENCODING:
    case VAX_ENCODING:
    case DECSTATION_ENCODING:
    case SGi_ENCODING:
    case IBMPC_ENCODING:
    case IBMRS_ENCODING:
    case HP_ENCODING:
    case NeXT_ENCODING:
    case ALPHAOSF1_ENCODING:
    case ALPHAVMSd_ENCODING:
    case ALPHAVMSg_ENCODING:
    case ALPHAVMSi_ENCODING:
    case PPC_ENCODING:
      *actualEncoding = encoding;
      break;
    default:
      return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* Validate decoding.
******************************************************************************/

VISIBLE_PREFIX Logical ValidDecoding (decoding)
Int32 decoding;
{
  switch (decoding) {
    case HOST_DECODING:
    case NETWORK_DECODING:
    case SUN_DECODING:
    case VAX_DECODING:
    case DECSTATION_DECODING:
    case SGi_DECODING:
    case IBMPC_DECODING:
    case IBMRS_DECODING:
    case HP_DECODING:
    case NeXT_DECODING:
    case ALPHAOSF1_DECODING:
    case ALPHAVMSd_DECODING:
    case ALPHAVMSg_DECODING:
    case ALPHAVMSi_DECODING:
    case PPC_DECODING:
      return TRUE;
    default:
      return FALSE;
  }
}

/******************************************************************************
* ValidateCompression.
******************************************************************************/

#if defined(BORLANDC)
#pragma warn -par
#endif

VISIBLE_PREFIX CDFstatus ValidateCompression (cType, cParms)
long cType;
long *cParms;
{
  switch (cType) {
    case NO_COMPRESSION:
      break;
#if SUPPORT_RLE 
    case RLE_COMPRESSION:
      if (cParms[0] != RLE_OF_ZEROs) return BAD_COMPRESSION_PARM;
      break;
#endif
#if SUPPORT_HUFF 
    case HUFF_COMPRESSION:
      if (cParms[0] != OPTIMAL_ENCODING_TREES) return BAD_COMPRESSION_PARM;
      break;
#endif
#if SUPPORT_AHUFF 
    case AHUFF_COMPRESSION:
      if (cParms[0] != OPTIMAL_ENCODING_TREES) return BAD_COMPRESSION_PARM;
      break;
#endif
#if SUPPORT_GZIP 
    case GZIP_COMPRESSION:
      if (!INCLUSIVE(1,cParms[0],9)) return BAD_COMPRESSION_PARM;
      break;
#endif
    default:
      return UNKNOWN_COMPRESSION;
  }
  return CDF_OK;
}

/******************************************************************************
* ValidateCompression64.
******************************************************************************/
      
#if defined(BORLANDC)
#pragma warn -par
#endif

VISIBLE_PREFIX CDFstatus ValidateCompression64 (cType, cParms)
long cType;
long *cParms;
{
  switch (cType) {
    case NO_COMPRESSION:
      break;
#if SUPPORT_RLE64
    case RLE_COMPRESSION:
      if (cParms[0] != RLE_OF_ZEROs) return BAD_COMPRESSION_PARM;
      break;
#endif
#if SUPPORT_HUFF64
    case HUFF_COMPRESSION:
      if (cParms[0] != OPTIMAL_ENCODING_TREES) return BAD_COMPRESSION_PARM;
      break;
#endif
#if SUPPORT_AHUFF64
    case AHUFF_COMPRESSION:
      if (cParms[0] != OPTIMAL_ENCODING_TREES) return BAD_COMPRESSION_PARM;
      break;
#endif
#if SUPPORT_GZIP64
    case GZIP_COMPRESSION:
      if (!INCLUSIVE(1,cParms[0],9)) return BAD_COMPRESSION_PARM;
      break;
#endif
    default:
      return UNKNOWN_COMPRESSION;
  }
  return CDF_OK;
}


