/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                            CDF encoding/decoding.
*
*  Version 1.3a, 18-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  31-Jan-94, J Love     Original version.
*   V1.0a  8-Feb-94, J Love	DEC Alpha/OpenVMS port.
*   V1.1   9-Nov-94, J Love	CDF V2.5.
*   V1.1a  5-Jan-95, J Love	Encode/decode changes.
*   V1.1b 19-Jan-95, J Love	IRIX 6.0 (64-bit).
*   V1.1c 24-Feb-95, J Love	Solaris 2.3 IDL i/f.
*   V1.2  21-Mar-95, J Love	POSIX.
*   V1.2a 18-Apr-95, J Love	More POSIX.
*   V1.3   3-Oct-96, J Love	CDF V2.6.
*   V1.3a 18-Nov-97, J Love	Windows NT/Visual C++.
*   V3.3  10-Jan-09, M Liu      Modifed to check input argument for numElems 
*                               in ConvertBuffer.
*
******************************************************************************/

#include "cdflib.h"

#if defined(alphavms) || defined(posixSHELLalpha)
#define DYNAMICfpSTRUCTs	1
#define STATICfpSTRUCTs		0
#else
#define STATICfpSTRUCTs		1
#define DYNAMICfpSTRUCTs	0
#endif

#if defined(alphavmsD) || defined(posixSHELLalphaD)
#define LIMITfp3DOUBLEs		1
#else
#define LIMITfp3DOUBLEs		0
#endif

/******************************************************************************
* Minimums, maximums, and zeros.
******************************************************************************/

#if defined(FP1cpu)
#define FP1ZEROsingle 0x0,0x0,0x0,0x0,0x0,0x0
#define FP2ZEROsingle 0x0,0x0,0x0,0x0,0x0,0x0
#define FP34ZEROsingle 0x0,0x0,0x0,0x0,0x0,0x0
#define FP1ZEROsingleNEG 0x1,0x0,0x0,0x0,0x0,0x0
#define FP2ZEROsingleNEG 0x0,0x0,0x0,0x0,0x1,0x0
#define FP34MINsingle 0x1,0x0,0x0,0x0,0x0,0x0
#define FP34MAXsingle 0x1,0x7F,0x0,0x7F,0xFF,0xFF
#define FP1ZEROdouble 0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
#define FP2ZEROdouble 0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
#define FP3ZEROdouble 0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
#define FP4ZEROdouble 0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
#define FP1ZEROdoubleNEG 0x1,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
#define FP2ZEROdoubleNEG 0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x1,0x0
#define FP3MINdouble 0x1,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
#define FP3MAXdouble 0x1,0x7F,0x0,0x7F,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF
#define FP4MINdouble 0x1,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
#define FP4MAXdouble 0xF,0xF,0x0,0x7F,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF
#endif

#if defined(FP2cpu) || defined(FP3cpu) || defined(FP4cpu)
#define FP1ZEROsingle 0x0,0x0,0x0,0x0,0x0,0x0
#define FP2ZEROsingle 0x0,0x0,0x0,0x0,0x0,0x0
#define FP34ZEROsingle 0x0,0x0,0x0,0x0,0x0,0x0
#define FP1ZEROsingleNEG 0x0,0x1,0x0,0x0,0x0,0x0
#define FP2ZEROsingleNEG 0x0,0x0,0x0,0x0,0x0,0x1
#define FP34MINsingle 0x0,0x1,0x0,0x0,0x0,0x0
#define FP34MAXsingle 0x7F,0x1,0x7F,0x0,0xFF,0xFF
#define FP1ZEROdouble 0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
#define FP2ZEROdouble 0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
#define FP3ZEROdouble 0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
#define FP4ZEROdouble 0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
#define FP1ZEROdoubleNEG 0x0,0x1,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
#define FP2ZEROdoubleNEG 0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x1
#define FP3MINdouble 0x0,0x1,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
#define FP3MAXdouble 0x7F,0x1,0x7F,0x0,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF
#define FP4MINdouble 0x0,0x1,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
#define FP4MAXdouble 0xF,0xF,0x7F,0x0,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF
#endif

#if STATICfpSTRUCTs
static struct fp1struct4 FP1zeroSingle = { FP1ZEROsingle };
static struct fp2struct4 FP2zeroSingle = { FP2ZEROsingle };
static struct fp34struct4 FP34zeroSingle = { FP34ZEROsingle };
static struct fp1struct4 FP1zeroSingleNeg = { FP1ZEROsingleNEG };
static struct fp2struct4 FP2zeroSingleNeg = { FP2ZEROsingleNEG };
static struct fp34struct4 FP34minSingle = { FP34MINsingle };
static struct fp34struct4 FP34maxSingle = { FP34MAXsingle };
static struct fp1struct8 FP1zeroDouble = { FP1ZEROdouble };
static struct fp2struct8 FP2zeroDouble = { FP2ZEROdouble };
static struct fp3struct8 FP3zeroDouble = { FP3ZEROdouble };
static struct fp4struct8 FP4zeroDouble = { FP4ZEROdouble };
static struct fp1struct8 FP1zeroDoubleNeg = { FP1ZEROdoubleNEG };
static struct fp2struct8 FP2zeroDoubleNeg = { FP2ZEROdoubleNEG };
static struct fp3struct8 FP3minDouble = { FP3MINdouble };
static struct fp3struct8 FP3maxDouble = { FP3MAXdouble };
static struct fp4struct8 FP4minDouble = { FP4MINdouble };
static struct fp4struct8 FP4maxDouble = { FP4MAXdouble };
#endif

/******************************************************************************
* ConvertBuffer.
******************************************************************************/

VISIBLE_PREFIX CDFstatus ConvertBuffer (srcEncoding, dstEncoding, negToPosFp0,
				      dataType, numElems, srcBuffer, dstBuffer)
Int32 srcEncoding;
Int32 dstEncoding;
Logical negToPosFp0;
Int32 dataType;
Int32 numElems;
void *srcBuffer;
void *dstBuffer;
{
  Int32 tSrcEncoding = BOO(srcEncoding == HOST_ENCODING,
			  HostEncoding(), srcEncoding);
  Int32 tDstEncoding = BOO(dstEncoding == HOST_ENCODING,
			  HostEncoding(), dstEncoding);

  CDFstatus pStatus = CDF_OK;	/* Pending status code.  Note that the status
				   code from a routine is only checked in those
				   cases were NEGATIVE_FP_ZERO may be returned
				   (ie., when converting to a VMSish floating
				   point encoding). */

  if (numElems < 1) return CDF_INTERNAL_ERROR;

  switch (dataType) {
      /************************************************************************
      * Bytes/characters - these are the same on all supported computers so no
      * conversion is necessary.
      ************************************************************************/
      case CDF_BYTE:
      case CDF_INT1:
      case CDF_UINT1:
      case CDF_CHAR:
      case CDF_UCHAR:
	MEMMOVE (dstBuffer, srcBuffer, (size_t) numElems)
	break;
      /************************************************************************
      * 2-byte integers.
      ************************************************************************/
      case CDF_INT2:
      case CDF_UINT2:
	if (IntegerOrder(tSrcEncoding) == IntegerOrder(tDstEncoding)) {
	  MEMMOVE (dstBuffer, srcBuffer, (size_t) (2 * numElems));
	}
	else {
	  MEMMOVE (dstBuffer, srcBuffer, (size_t) (2 * numElems));
	  Reverse2 (dstBuffer, numElems);
	}
	break;
      /************************************************************************
      * 4-byte integers.
      ************************************************************************/
      case CDF_INT4:
      case CDF_UINT4:
	if (IntegerOrder(tSrcEncoding) == IntegerOrder(tDstEncoding)) {
	  MEMMOVE (dstBuffer, srcBuffer, (size_t) (4 * numElems));
	}
	else {
	  MEMMOVE (dstBuffer, srcBuffer, (size_t) (4 * numElems));
	  Reverse4 (dstBuffer, numElems);
	}
	break;
      /************************************************************************
      * 8-byte integers.
      ************************************************************************/
      case CDF_INT8:
      case CDF_TIME_TT2000:
	if (IntegerOrder(tSrcEncoding) == IntegerOrder(tDstEncoding)) {
	  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems));
	}
	else {
	  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems));
	  Reverse8 (dstBuffer, numElems);
	}
	break;
      /************************************************************************
      * 4-byte floating-point values.
      ************************************************************************/
      case CDF_REAL4:
      case CDF_FLOAT: {
	int srcFpType = FpType (tSrcEncoding);
	int dstFpType = FpType (tDstEncoding);
	switch (srcFpType) {
	  /********************************************************************
	  * Floating-point/1 source encoding.
	  ********************************************************************/
	  case FP_1:
	    switch (dstFpType) {
	      /****************************************************************
	      * Floating-point/1 destination encoding desired.
	      ****************************************************************/
	      case FP_1:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (4 * numElems))
		  FP1singleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (4 * numElems))
		}
		break;
	      /****************************************************************
	      * Floating-point/2 destination encoding desired.
	      ****************************************************************/
	      case FP_2:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (4 * numElems))
		  FP1toFP2singleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (4 * numElems))
		  Reverse4 (dstBuffer, numElems);
		}
		break;
	      /****************************************************************
	      * Floating-point/34 destination encoding desired.
	      ****************************************************************/
	      case FP_3:
	      case FP_4:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (4 * numElems))
		  FP1toFP34singleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (4 * numElems))
		  sX (FP1toFP34single(dstBuffer,numElems), &pStatus);
		}
		break;
	    }
	    break;
	  /********************************************************************
	  * Floating-point/2 source encoding.
	  ********************************************************************/
	  case FP_2:
	    switch (dstFpType) {
	      /****************************************************************
	      * Floating-point/1 destination encoding desired.
	      ****************************************************************/
	      case FP_1:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (4 * numElems))
		  FP2toFP1singleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (4 * numElems))
		  Reverse4 (dstBuffer, numElems);
		}
		break;
	      /****************************************************************
	      * Floating-point/2 destination encoding desired.
	      ****************************************************************/
	      case FP_2:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (4 * numElems))
		  FP2singleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (4 * numElems))
		}
		break;
	      /****************************************************************
	      * Floating-point/34 destination encoding desired.
	      ****************************************************************/
	      case FP_3:
	      case FP_4:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (4 * numElems))
		  FP2toFP34singleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (4 * numElems))
		  sX (FP2toFP34single(dstBuffer,numElems), &pStatus);
		}
		break;
	    }
	    break;
	  /********************************************************************
	  * Floating-point/34 source encoding.
	  ********************************************************************/
	  case FP_3:
	  case FP_4:
	    switch (dstFpType) {
	      /****************************************************************
	      * Floating-point/1 destination encoding desired.
	      ****************************************************************/
	      case FP_1:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (4 * numElems))
		  FP34toFP1singleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (4 * numElems))
		  FP34toFP1single (dstBuffer, numElems);
		}
		break;
	      /****************************************************************
	      * Floating-point/2 destination encoding desired.
	      ****************************************************************/
	      case FP_2:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (4 * numElems))
		  FP34toFP2singleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (4 * numElems))
		  FP34toFP2single (dstBuffer, numElems);
		}
		break;
	      /****************************************************************
	      * Floating-point/34 destination encoding desired.
	      ****************************************************************/
	      case FP_3:
	      case FP_4:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (4 * numElems))
		  FP34singleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (4 * numElems))
		}
		break;
	    }
	    break;
	}
	break;
      }
      /************************************************************************
      * 8-byte floating-point values.
      ************************************************************************/
      case CDF_REAL8:
      case CDF_DOUBLE:
      case CDF_EPOCH: {
	int srcFpType = FpType (tSrcEncoding);
	int dstFpType = FpType (tDstEncoding);
	switch (srcFpType) {
	  /********************************************************************
	  * Floating-point/1 source encoding.
	  ********************************************************************/
	  case FP_1:
	    switch (dstFpType) {
	      /****************************************************************
	      * Floating-point/1 destination encoding desired.
	      ****************************************************************/
	      case FP_1:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  FP1doubleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		}
		break;
	      /****************************************************************
	      * Floating-point/2 destination encoding desired.
	      ****************************************************************/
	      case FP_2:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  FP1toFP2doubleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  Reverse8 (dstBuffer, numElems);
		}
		break;
	      /****************************************************************
	      * Floating-point/3 destination encoding desired.
	      ****************************************************************/
	      case FP_3:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  FP1toFP3doubleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  sX (FP1toFP3double(dstBuffer,numElems), &pStatus);
		}
		break;
	      /****************************************************************
	      * Floating-point/4 destination encoding desired.
	      ****************************************************************/
	      case FP_4:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  FP1toFP4doubleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  sX (FP1toFP4double(dstBuffer,numElems), &pStatus);
		}
		break;
	    }
	    break;
	  /********************************************************************
	  * Floating-point/2 source encoding.
	  ********************************************************************/
	  case FP_2:
	    switch (dstFpType) {
	      /****************************************************************
	      * Floating-point/1 destination encoding desired.
	      ****************************************************************/
	      case FP_1:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  FP2toFP1doubleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  Reverse8 (dstBuffer, numElems);
		}
		break;
	      /****************************************************************
	      * Floating-point/2 destination encoding desired.
	      ****************************************************************/
	      case FP_2:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  FP2doubleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		}
		break;
	      /****************************************************************
	      * Floating-point/3 destination encoding desired.
	      ****************************************************************/
	      case FP_3:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  FP2toFP3doubleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  sX (FP2toFP3double(dstBuffer,numElems), &pStatus);
		}
		break;
	      /****************************************************************
	      * Floating-point/4 destination encoding desired.
	      ****************************************************************/
	      case FP_4:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  FP2toFP4doubleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  sX (FP2toFP4double(dstBuffer,numElems), &pStatus);
		}
		break;
	    }
	    break;
	  /********************************************************************
	  * Floating-point/3 source encoding.
	  ********************************************************************/
	  case FP_3:
	    switch (dstFpType) {
	      /****************************************************************
	      * Floating-point/1 destination encoding desired.
	      ****************************************************************/
	      case FP_1:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  FP3toFP1doubleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  FP3toFP1double (dstBuffer, numElems);
		}
		break;
	      /****************************************************************
	      * Floating-point/2 destination encoding desired.
	      ****************************************************************/
	      case FP_2:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  FP3toFP2doubleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  FP3toFP2double (dstBuffer, numElems);
		}
		break;
	      /****************************************************************
	      * Floating-point/3 destination encoding desired.
	      ****************************************************************/
	      case FP_3:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  FP3doubleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
#if LIMITfp3DOUBLEs
		  sX (FP3doubleLIMIT(dstBuffer,numElems), &pStatus);
#endif
		}
		break;
	      /****************************************************************
	      * Floating-point/4 destination encoding desired.
	      ****************************************************************/
	      case FP_4:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  FP3toFP4doubleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  sX (FP3toFP4double(dstBuffer,numElems), &pStatus);
		}
		break;
	    }
	    break;
	  /********************************************************************
	  * Floating-point/4 source encoding.
	  ********************************************************************/
	  case FP_4:
	    switch (dstFpType) {
	      /****************************************************************
	      * Floating-point/1 destination encoding desired.
	      ****************************************************************/
	      case FP_1:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  FP4toFP1doubleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  FP4toFP1double (dstBuffer, numElems);
		}
		break;
	      /****************************************************************
	      * Floating-point/2 destination encoding desired.
	      ****************************************************************/
	      case FP_2:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  FP4toFP2doubleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  FP4toFP2double (dstBuffer, numElems);
		}
		break;
	      /****************************************************************
	      * Floating-point/3 destination encoding desired.
	      ****************************************************************/
	      case FP_3:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  FP4toFP3doubleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  sX (FP4toFP3double(dstBuffer,numElems), &pStatus);
		}
		break;
	      /****************************************************************
	      * Floating-point/4 destination encoding desired.
	      ****************************************************************/
	      case FP_4:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		  FP4doubleNEGtoPOS (dstBuffer, numElems);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems))
		}
		break;
	    }
	    break;
	}
	break;
      }
      /************************************************************************
      * 2 8-byte floating-point values.
      ************************************************************************/
      case CDF_EPOCH16: {
	int srcFpType = FpType (tSrcEncoding);
	int dstFpType = FpType (tDstEncoding);
	switch (srcFpType) {
	  /********************************************************************
	  * Floating-point/1 source encoding.
	  ********************************************************************/
	  case FP_1:
	    switch (dstFpType) {
	      /****************************************************************
	      * Floating-point/1 destination encoding desired.
	      ****************************************************************/
	      case FP_1:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  FP1doubleNEGtoPOS (dstBuffer, numElems * 2);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		}
		break;
	      /****************************************************************
	      * Floating-point/2 destination encoding desired.
	      ****************************************************************/
	      case FP_2:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  FP1toFP2doubleNEGtoPOS (dstBuffer, numElems * 2);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  Reverse8 (dstBuffer, numElems * 2);
		}
		break;
	      /****************************************************************
	      * Floating-point/3 destination encoding desired.
	      ****************************************************************/
	      case FP_3:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  FP1toFP3doubleNEGtoPOS (dstBuffer, numElems * 2);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  sX (FP1toFP3double(dstBuffer,numElems * 2), &pStatus);
		}
		break;
	      /****************************************************************
	      * Floating-point/4 destination encoding desired.
	      ****************************************************************/
	      case FP_4:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  FP1toFP4doubleNEGtoPOS (dstBuffer, numElems * 2);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  sX (FP1toFP4double(dstBuffer,numElems * 2), &pStatus);
		}
		break;
	    }
	    break;
	  /********************************************************************
	  * Floating-point/2 source encoding.
	  ********************************************************************/
	  case FP_2:
	    switch (dstFpType) {
	      /****************************************************************
	      * Floating-point/1 destination encoding desired.
	      ****************************************************************/
	      case FP_1:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  FP2toFP1doubleNEGtoPOS (dstBuffer, numElems * 2);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  Reverse8 (dstBuffer, numElems * 2);
		}
		break;
	      /****************************************************************
	      * Floating-point/2 destination encoding desired.
	      ****************************************************************/
	      case FP_2:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  FP2doubleNEGtoPOS (dstBuffer, numElems * 2);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		}
		break;
	      /****************************************************************
	      * Floating-point/3 destination encoding desired.
	      ****************************************************************/
	      case FP_3:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  FP2toFP3doubleNEGtoPOS (dstBuffer, numElems * 2);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  sX (FP2toFP3double(dstBuffer,numElems * 2), &pStatus);
		}
		break;
	      /****************************************************************
	      * Floating-point/4 destination encoding desired.
	      ****************************************************************/
	      case FP_4:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  FP2toFP4doubleNEGtoPOS (dstBuffer, numElems * 2);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  sX (FP2toFP4double(dstBuffer,numElems * 2), &pStatus);
		}
		break;
	    }
	    break;
	  /********************************************************************
	  * Floating-point/3 source encoding.
	  ********************************************************************/
	  case FP_3:
	    switch (dstFpType) {
	      /****************************************************************
	      * Floating-point/1 destination encoding desired.
	      ****************************************************************/
	      case FP_1:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  FP3toFP1doubleNEGtoPOS (dstBuffer, numElems * 2);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  FP3toFP1double (dstBuffer, numElems * 2);
		}
		break;
	      /****************************************************************
	      * Floating-point/2 destination encoding desired.
	      ****************************************************************/
	      case FP_2:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  FP3toFP2doubleNEGtoPOS (dstBuffer, numElems * 2);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  FP3toFP2double (dstBuffer, numElems * 2);
		}
		break;
	      /****************************************************************
	      * Floating-point/3 destination encoding desired.
	      ****************************************************************/
	      case FP_3:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  FP3doubleNEGtoPOS (dstBuffer, numElems * 2);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
#if LIMITfp3DOUBLEs
		  sX (FP3doubleLIMIT(dstBuffer,numElems * 2), &pStatus);
#endif
		}
		break;
	      /****************************************************************
	      * Floating-point/4 destination encoding desired.
	      ****************************************************************/
	      case FP_4:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  FP3toFP4doubleNEGtoPOS (dstBuffer, numElems * 2);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  sX (FP3toFP4double(dstBuffer,numElems * 2), &pStatus);
		}
		break;
	    }
	    break;
	  /********************************************************************
	  * Floating-point/4 source encoding.
	  ********************************************************************/
	  case FP_4:
	    switch (dstFpType) {
	      /****************************************************************
	      * Floating-point/1 destination encoding desired.
	      ****************************************************************/
	      case FP_1:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  FP4toFP1doubleNEGtoPOS (dstBuffer, numElems * 2);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  FP4toFP1double (dstBuffer, numElems * 2);
		}
		break;
	      /****************************************************************
	      * Floating-point/2 destination encoding desired.
	      ****************************************************************/
	      case FP_2:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  FP4toFP2doubleNEGtoPOS (dstBuffer, numElems * 2);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  FP4toFP2double (dstBuffer, numElems * 2);
		}
		break;
	      /****************************************************************
	      * Floating-point/3 destination encoding desired.
	      ****************************************************************/
	      case FP_3:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  FP4toFP3doubleNEGtoPOS (dstBuffer, numElems * 2);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  sX (FP4toFP3double(dstBuffer,numElems * 2), &pStatus);
		}
		break;
	      /****************************************************************
	      * Floating-point/4 destination encoding desired.
	      ****************************************************************/
	      case FP_4:
		if (negToPosFp0) {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		  FP4doubleNEGtoPOS (dstBuffer, numElems * 2);
		}
		else {
		  MEMMOVE (dstBuffer, srcBuffer, (size_t) (8 * numElems * 2))
		}
		break;
	    }
	    break;
	}
	break;
      }
      /************************************************************************
      * Unknown data type.
      ************************************************************************/
      default:
	return CDF_INTERNAL_ERROR;
    }
  return pStatus;
}

/******************************************************************************
* ConversionFunction.
******************************************************************************/

STATICforIDL CDFstatus ConversionFunction (dataType, encoding, decoding,
					   negToPosFp0, function)
Int32 dataType;
Int32 encoding;
Int32 decoding;
Logical negToPosFp0;
CDFstatus (**function) PROTOARGs((void *buffer, Int32 numElems));
{
  Int32 tDecoding = BOO(decoding == HOST_DECODING,HostDecoding(), decoding);
  switch (dataType) {
    /**************************************************************************
    * Bytes/characters - these are the same on all supported computers so no
    * decoding is necessary.
    **************************************************************************/
    case CDF_BYTE:
    case CDF_INT1:
    case CDF_UINT1:
    case CDF_CHAR:
    case CDF_UCHAR:
      *function = NULL;
      break;
    /**************************************************************************
    * 2-byte integers.
    **************************************************************************/
    case CDF_INT2:
    case CDF_UINT2:
      if (IntegerOrder(encoding) == IntegerOrder(tDecoding))
	*function = NULL;
      else
	*function = Reverse2;
      break;
    /**************************************************************************
    * 4-byte integers.
    **************************************************************************/
    case CDF_INT4:
    case CDF_UINT4:
      if (IntegerOrder(encoding) == IntegerOrder(tDecoding))
	*function = NULL;
      else
	*function = Reverse4;
      break;
    /**************************************************************************
    * 4-byte integers.
    **************************************************************************/
    case CDF_INT8:
    case CDF_TIME_TT2000:
      if (IntegerOrder(encoding) == IntegerOrder(tDecoding))
	*function = NULL;
      else
	*function = Reverse8;
      break;
    /**************************************************************************
    * 4-byte floating-point values.
    **************************************************************************/
    case CDF_REAL4:
    case CDF_FLOAT: {
      int fpEncoding = FpType (encoding);
      int fpDecoding = FpType (tDecoding);
      switch (fpEncoding) {
	/**********************************************************************
	* Floating-point/1 encoding.
	**********************************************************************/
	case FP_1:
	  switch (fpDecoding) {
	    /******************************************************************
	    * Floating-point/1 encoding - floating-point/1 decoding desired.
	    ******************************************************************/
	    case FP_1:
	      if (negToPosFp0)
		*function = FP1singleNEGtoPOS;
	      else
		*function = NULL;
	      break;
	    /******************************************************************
	    * Floating-point/1 encoding - floating-point/2 decoding desired.
	    ******************************************************************/
	    case FP_2:
	      if (negToPosFp0)
		*function = FP1toFP2singleNEGtoPOS;
	      else
		*function = Reverse4;
	      break;
	    /******************************************************************
	    * Floating-point/1 encoding - floating-point/34 decoding desired.
	    ******************************************************************/
	    case FP_3:
	    case FP_4:
	      if (negToPosFp0)
		*function = FP1toFP34singleNEGtoPOS;
	      else
		*function = FP1toFP34single;
	      break;
	  }
	  break;
	/**********************************************************************
	* Floating-point/2 encoding.
	**********************************************************************/
	case FP_2:
	  switch (fpDecoding) {
	    /******************************************************************
	    * Floating-point/2 encoding - floating-point/1 decoding desired.
	    ******************************************************************/
	    case FP_1:
	      if (negToPosFp0)
		*function = FP2toFP1singleNEGtoPOS;
	      else
		*function = Reverse4;
	      break;
	    /******************************************************************
	    * Floating-point/2 encoding - floating-point/2 decoding desired.
	    ******************************************************************/
	    case FP_2:
	      if (negToPosFp0)
		*function = FP2singleNEGtoPOS;
	      else
		*function = NULL;
	      break;
	    /******************************************************************
	    * Floating-point/2 encoding - floating-point/34 decoding desired.
	    ******************************************************************/
	    case FP_3:
	    case FP_4:
	      if (negToPosFp0)
		*function = FP2toFP34singleNEGtoPOS;
	      else
		*function = FP2toFP34single;
	      break;
	  }
	  break;
	/**********************************************************************
	* Floating-point/34 encoding.
	**********************************************************************/
	case FP_3:
	case FP_4:
	  switch (fpDecoding) {
	    /******************************************************************
	    * Floating-point/34 encoding - floating-point/1 decoding desired.
	    ******************************************************************/
	    case FP_1:
	      if (negToPosFp0)
		*function = FP34toFP1singleNEGtoPOS;
	      else
		*function = FP34toFP1single;
	      break;
	    /******************************************************************
	    * Floating-point/34 encoding - floating-point/2 decoding desired.
	    ******************************************************************/
	    case FP_2:
	      if (negToPosFp0)
		*function = FP34toFP2singleNEGtoPOS;
	      else
		*function = FP34toFP2single;
	      break;
	    /******************************************************************
	    * Floating-point/34 encoding - floating-point/34 decoding
	    * desired.
	    ******************************************************************/
	    case FP_3:
	    case FP_4:
	      if (negToPosFp0)
		*function = FP34singleNEGtoPOS;
	      else
		*function = NULL;
	      break;
	  }
	  break;
      }
      break;
    }
    /**************************************************************************
    * 8-byte floating-point values.
    **************************************************************************/
    case CDF_REAL8:
    case CDF_DOUBLE:
    case CDF_EPOCH: {
      int fpEncoding = FpType (encoding);
      int fpDecoding = FpType (tDecoding);
      switch (fpEncoding) {
	/**********************************************************************
	* Floating-point/1 encoding.
	**********************************************************************/
	case FP_1:
	  switch (fpDecoding) {
	    /******************************************************************
	    * Floating-point/1 encoding - floating-point/1 decoding desired.
	    ******************************************************************/
	    case FP_1:
	      if (negToPosFp0)
		*function = FP1doubleNEGtoPOS;
	      else
		*function = NULL;
	      break;
	    /******************************************************************
	    * Floating-point/1 encoding - floating-point/2 decoding desired.
	    ******************************************************************/
	    case FP_2:
	      if (negToPosFp0)
		*function = FP1toFP2doubleNEGtoPOS;
	      else
		*function = Reverse8;
	      break;
	    /******************************************************************
	    * Floating-point/1 encoding - floating-point/3 decoding desired.
	    ******************************************************************/
	    case FP_3:
	      if (negToPosFp0)
		*function = FP1toFP3doubleNEGtoPOS;
	      else
		*function = FP1toFP3double;
	      break;
	    /******************************************************************
	    * Floating-point/1 encoding - floating-point/4 decoding desired.
	    ******************************************************************/
	    case FP_4:
	      if (negToPosFp0)
		*function = FP1toFP4doubleNEGtoPOS;
	      else
		*function = FP1toFP4double;
	      break;
	  }
	  break;
	/**********************************************************************
	* Floating-point/2 encoding.
	**********************************************************************/
	case FP_2:
	  switch (fpDecoding) {
	    /******************************************************************
	    * Floating-point/2 encoding - floating-point/1 decoding desired.
	    ******************************************************************/
	    case FP_1:
	      if (negToPosFp0)
		*function = FP2toFP1doubleNEGtoPOS;
	      else
		*function = Reverse8;
	      break;
	    /******************************************************************
	    * Floating-point/2 encoding - floating-point/2 decoding desired.
	    ******************************************************************/
	    case FP_2:
	      if (negToPosFp0)
		*function = FP2doubleNEGtoPOS;
	      else
		*function = NULL;
	      break;
	    /******************************************************************
	    * Floating-point/2 encoding - floating-point/3 decoding desired.
	    ******************************************************************/
	    case FP_3:
	      if (negToPosFp0)
		*function = FP2toFP3doubleNEGtoPOS;
	      else
		*function = FP2toFP3double;
	      break;
	    /******************************************************************
	    * Floating-point/2 encoding - floating-point/4 decoding desired.
	    ******************************************************************/
	    case FP_4:
	      if (negToPosFp0)
		*function = FP2toFP4doubleNEGtoPOS;
	      else
		*function = FP2toFP4double;
	      break;
	  }
	  break;
	/**********************************************************************
	* Floating-point/3 encoding.
	**********************************************************************/
	case FP_3:
	  switch (fpDecoding) {
	    /******************************************************************
	    * Floating-point/3 encoding - floating-point/1 decoding desired.
	    ******************************************************************/
	    case FP_1:
	      if (negToPosFp0)
		*function = FP3toFP1doubleNEGtoPOS;
	      else
		*function = FP3toFP1double;
	      break;
	    /******************************************************************
	    * Floating-point/3 encoding - floating-point/2 decoding desired.
	    ******************************************************************/
	    case FP_2:
	      if (negToPosFp0)
		*function = FP3toFP2doubleNEGtoPOS;
	      else
		*function = FP3toFP2double;
	      break;
	    /******************************************************************
	    * Floating-point/3 encoding - floating-point/3 decoding desired.
	    ******************************************************************/
	    case FP_3:
	      if (negToPosFp0)
		*function = FP3doubleNEGtoPOS;
	      else
#if LIMITfp3DOUBLEs
		*function = FP3doubleLIMIT;
#else
		*function = NULL;
#endif
	      break;
	    /******************************************************************
	    * Floating-point/3 encoding - floating-point/4 decoding desired.
	    ******************************************************************/
	    case FP_4:
	      if (negToPosFp0)
		*function = FP3toFP4doubleNEGtoPOS;
	      else
		*function = FP3toFP4double;
	      break;
	  }
	  break;
	/**********************************************************************
	* Floating-point/4 encoding.
	**********************************************************************/
	case FP_4:
	  switch (fpDecoding) {
	    /******************************************************************
	    * Floating-point/4 encoding - floating-point/1 decoding desired.
	    ******************************************************************/
	    case FP_1:
	      if (negToPosFp0)
		*function = FP4toFP1doubleNEGtoPOS;
	      else
		*function = FP4toFP1double;
	      break;
	    /******************************************************************
	    * Floating-point/4 encoding - floating-point/2 decoding desired.
	    ******************************************************************/
	    case FP_2:
	      if (negToPosFp0)
		*function = FP4toFP2doubleNEGtoPOS;
	      else
		*function = FP4toFP2double;
	      break;
	    /******************************************************************
	    * Floating-point/4 encoding - floating-point/3 decoding desired.
	    ******************************************************************/
	    case FP_3:
	      if (negToPosFp0)
		*function = FP4toFP3doubleNEGtoPOS;
	      else
		*function = FP4toFP3double;
	      break;
	    /******************************************************************
	    * Floating-point/4 encoding - floating-point/4 decoding desired.
	    ******************************************************************/
	    case FP_4:
	      if (negToPosFp0)
		*function = FP4doubleNEGtoPOS;
	      else
		*function = NULL;
	      break;
	  }
	  break;
      }
      break;
    }
    /**************************************************************************
    * 2 8-byte floating-point values.
    **************************************************************************/
    case CDF_EPOCH16: {
      int fpEncoding = FpType (encoding);
      int fpDecoding = FpType (tDecoding);
      switch (fpEncoding) {
	/**********************************************************************
	* Floating-point/1 encoding.
	**********************************************************************/
	case FP_1:
	  switch (fpDecoding) {
	    /******************************************************************
	    * Floating-point/1 encoding - floating-point/1 decoding desired.
	    ******************************************************************/
	    case FP_1:
	      if (negToPosFp0)
		*function = FP1doubleNEGtoPOS;
	      else
		*function = NULL;
	      break;
	    /******************************************************************
	    * Floating-point/1 encoding - floating-point/2 decoding desired.
	    ******************************************************************/
	    case FP_2:
	      if (negToPosFp0)
		*function = FP1toFP2doubleNEGtoPOS;
	      else
		*function = Reverse16;
	      break;
	    /******************************************************************
	    * Floating-point/1 encoding - floating-point/3 decoding desired.
	    ******************************************************************/
	    case FP_3:
	      if (negToPosFp0)
		*function = FP1toFP3doubleNEGtoPOS;
	      else
		*function = FP1toFP3double;
	      break;
	    /******************************************************************
	    * Floating-point/1 encoding - floating-point/4 decoding desired.
	    ******************************************************************/
	    case FP_4:
	      if (negToPosFp0)
		*function = FP1toFP4doubleNEGtoPOS;
	      else
		*function = FP1toFP4double;
	      break;
	  }
	  break;
	/**********************************************************************
	* Floating-point/2 encoding.
	**********************************************************************/
	case FP_2:
	  switch (fpDecoding) {
	    /******************************************************************
	    * Floating-point/2 encoding - floating-point/1 decoding desired.
	    ******************************************************************/
	    case FP_1:
	      if (negToPosFp0)
		*function = FP2toFP1doubleNEGtoPOS;
	      else
		*function = Reverse16;
	      break;
	    /******************************************************************
	    * Floating-point/2 encoding - floating-point/2 decoding desired.
	    ******************************************************************/
	    case FP_2:
	      if (negToPosFp0)
		*function = FP2doubleNEGtoPOS;
	      else
		*function = NULL;
	      break;
	    /******************************************************************
	    * Floating-point/2 encoding - floating-point/3 decoding desired.
	    ******************************************************************/
	    case FP_3:
	      if (negToPosFp0)
		*function = FP2toFP3doubleNEGtoPOS;
	      else
		*function = FP2toFP3double;
	      break;
	    /******************************************************************
	    * Floating-point/2 encoding - floating-point/4 decoding desired.
	    ******************************************************************/
	    case FP_4:
	      if (negToPosFp0)
		*function = FP2toFP4doubleNEGtoPOS;
	      else
		*function = FP2toFP4double;
	      break;
	  }
	  break;
	/**********************************************************************
	* Floating-point/3 encoding.
	**********************************************************************/
	case FP_3:
	  switch (fpDecoding) {
	    /******************************************************************
	    * Floating-point/3 encoding - floating-point/1 decoding desired.
	    ******************************************************************/
	    case FP_1:
	      if (negToPosFp0)
		*function = FP3toFP1doubleNEGtoPOS;
	      else
		*function = FP3toFP1double;
	      break;
	    /******************************************************************
	    * Floating-point/3 encoding - floating-point/2 decoding desired.
	    ******************************************************************/
	    case FP_2:
	      if (negToPosFp0)
		*function = FP3toFP2doubleNEGtoPOS;
	      else
		*function = FP3toFP2double;
	      break;
	    /******************************************************************
	    * Floating-point/3 encoding - floating-point/3 decoding desired.
	    ******************************************************************/
	    case FP_3:
	      if (negToPosFp0)
		*function = FP3doubleNEGtoPOS;
	      else
#if LIMITfp3DOUBLEs
		*function = FP3doubleLIMIT;
#else
		*function = NULL;
#endif
	      break;
	    /******************************************************************
	    * Floating-point/3 encoding - floating-point/4 decoding desired.
	    ******************************************************************/
	    case FP_4:
	      if (negToPosFp0)
		*function = FP3toFP4doubleNEGtoPOS;
	      else
		*function = FP3toFP4double;
	      break;
	  }
	  break;
	/**********************************************************************
	* Floating-point/4 encoding.
	**********************************************************************/
	case FP_4:
	  switch (fpDecoding) {
	    /******************************************************************
	    * Floating-point/4 encoding - floating-point/1 decoding desired.
	    ******************************************************************/
	    case FP_1:
	      if (negToPosFp0)
		*function = FP4toFP1doubleNEGtoPOS;
	      else
		*function = FP4toFP1double;
	      break;
	    /******************************************************************
	    * Floating-point/4 encoding - floating-point/2 decoding desired.
	    ******************************************************************/
	    case FP_2:
	      if (negToPosFp0)
		*function = FP4toFP2doubleNEGtoPOS;
	      else
		*function = FP4toFP2double;
	      break;
	    /******************************************************************
	    * Floating-point/4 encoding - floating-point/3 decoding desired.
	    ******************************************************************/
	    case FP_3:
	      if (negToPosFp0)
		*function = FP4toFP3doubleNEGtoPOS;
	      else
		*function = FP4toFP3double;
	      break;
	    /******************************************************************
	    * Floating-point/4 encoding - floating-point/4 decoding desired.
	    ******************************************************************/
	    case FP_4:
	      if (negToPosFp0)
		*function = FP4doubleNEGtoPOS;
	      else
		*function = NULL;
	      break;
	  }
	  break;
      }
      break;
    }
    /**************************************************************************
    * Unknown data type.
    **************************************************************************/
    default:
      return CDF_INTERNAL_ERROR;
  }
  return CDF_OK;
}

/******************************************************************************
* FP1toFP34single.
*    Floating-point/1 to floating-point/34, single-precision.
******************************************************************************/

STATICforIDL CDFstatus FP1toFP34single (buffer, numElems)
void *buffer;
Int32 numElems;
{
  uInt16 fp1e, fp34e;
  uInt32 fp1m, fp34m;
  struct fp1struct4 *fp1;
  struct fp34struct4 *fp34, Fp34;
  CDFstatus pStatus = CDF_OK;
  Int32 elemN;
#if DYNAMICfpSTRUCTs
  struct fp34struct4 FP34zeroSingle = { FP34ZEROsingle };
  struct fp34struct4 FP34minSingle = { FP34MINsingle };
  struct fp34struct4 FP34maxSingle = { FP34MAXsingle };
#endif
  for (elemN = 0,
       fp1 = (struct fp1struct4 *) buffer,
       fp34 = (struct fp34struct4 *) buffer;
       elemN < numElems;
       elemN++, fp1++, fp34++) {
     /*************************************************************************
     * Calculate FP1's exponent.
     *************************************************************************/
     fp1e = (fp1->e1 << 1) | fp1->e0;
     switch (fp1e) {
       /***********************************************************************
       * FP1 exponent of zero (0) - what to do depends on the FP1 mantissa.
       ***********************************************************************/
       case 0x0:
	 fp1m = (fp1->m2 << 16) | (fp1->m1 << 8) | fp1->m0;
	 /*********************************************************************
	 * If the FP1 mantissa is zero (0), set FP34 to 0.0 and use the sign
	 * from FP1.  Also check for -0.0.
	 *********************************************************************/
	 if (fp1m == 0x0) {
	   Fp34 = FP34zeroSingle;
	   Fp34.s = fp1->s;
	   if (Fp34.s == 0x1) pStatus = NEGATIVE_FP_ZERO;
	   break;
	 }
	 /*********************************************************************
	 * If the FP1 mantissa (with an FP1 exponent of zero [0]) puts the FP1
	 * value below the smallest magnitude possible for FP34, set FP34 to
	 * its minimum magnitude but with FP1's sign.
	 *********************************************************************/
	 if (fp1m < 0x200000) {
	   Fp34 = FP34minSingle;
	   Fp34.s = fp1->s;
	   break;
	 }
	 /*********************************************************************
	 * If the FP1 mantissa is in the range where it maps 1-to-4 to FP34
	 * mantissas, set the FP34 mantissa to the first (lowest) value which
	 * maps.
	 *********************************************************************/
	 if (fp1m < 0x400000) {
	   fp34e = 0x1;
	   Fp34.e1 = fp34e >> 1;
	   Fp34.e0 = fp34e;
	   fp34m = 4 * (fp1m - 0x200000);
	   Fp34.m2 = (Byte1) (fp34m >> 16);
	   Fp34.m1 = (Byte1) (fp34m >> 8);
	   Fp34.m0 = (Byte1) fp34m;
	   Fp34.s = fp1->s;
	   break;
	 }
	 /*********************************************************************
	 * The FP1 mantissa is in the range where it maps 1-to-2 to FP34
	 * mantissas, set the FP34 mantissa to the first (lowest) value which
	 * maps.
	 *********************************************************************/
	 fp34e = 0x2;
	 Fp34.e1 = fp34e >> 1;
	 Fp34.e0 = fp34e;
	 fp34m = 2 * (fp1m - 0x400000);
	 Fp34.m2 = (Byte1) (fp34m >> 16);
	 Fp34.m1 = (Byte1) (fp34m >> 8);
	 Fp34.m0 = (Byte1) fp34m;
	 Fp34.s = fp1->s;
	 break;
       /***********************************************************************
       * The FP1 exponent puts the FP1 value beyond the largest magnitude
       * possible for FP34 - set FP34 to its maximum magnitude but with FP1's
       * sign.
       ***********************************************************************/
       case 0xFE:
       case 0xFF:
	 Fp34 = FP34maxSingle;
	 Fp34.s = fp1->s;
	 break;
       /***********************************************************************
       * The FP1 value will `fit' into FP34.
       ***********************************************************************/
       default:
	 fp34e = fp1e + 2;
	 Fp34.e1 = fp34e >> 1;
	 Fp34.e0 = fp34e;
	 Fp34.m2 = fp1->m2;
	 Fp34.m1 = fp1->m1;
	 Fp34.m0 = fp1->m0;
	 Fp34.s = fp1->s;
     }
     *fp34 = Fp34;
  }
  return pStatus;
}

/******************************************************************************
* FP1toFP34singleNEGtoPOS.
*    Floating-point/1 to floating-point/34, single-precision.  Convert -0.0
* to 0.0.
******************************************************************************/

STATICforIDL CDFstatus FP1toFP34singleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  CDFstatus status = FP1toFP34single (buffer, numElems);
  if (status == NEGATIVE_FP_ZERO) FP34singleNEGtoPOS (buffer, numElems);
  return CDF_OK;
}

/******************************************************************************
* FP2toFP34single.
*    Floating-point/2 to floating-point/34, single-precision.
* See FP1toFP34single for comments.
******************************************************************************/

STATICforIDL CDFstatus FP2toFP34single (buffer, numElems)
void *buffer;
Int32 numElems;
{
  uInt16 fp2e, fp34e;
  uInt32 fp2m, fp34m;
  struct fp2struct4 *fp2;
  struct fp34struct4 *fp34, Fp34;
  CDFstatus pStatus = CDF_OK;
  Int32 elemN;
#if DYNAMICfpSTRUCTs
  struct fp34struct4 FP34zeroSingle = { FP34ZEROsingle };
  struct fp34struct4 FP34minSingle = { FP34MINsingle };
  struct fp34struct4 FP34maxSingle = { FP34MAXsingle };
#endif
  for (elemN = 0,
       fp2 = (struct fp2struct4 *) buffer,
       fp34 = (struct fp34struct4 *) buffer;
       elemN < numElems;
       elemN++, fp2++, fp34++) {
     fp2e = (fp2->e1 << 1) | fp2->e0;
     switch (fp2e) {
       case 0x0:
	 fp2m = (fp2->m2 << 16) | (fp2->m1 << 8) | fp2->m0;
	 if (fp2m == 0x0) {
	   Fp34 = FP34zeroSingle;
	   Fp34.s = fp2->s;
	   if (Fp34.s == 0x1) pStatus = NEGATIVE_FP_ZERO;
	   break;
	 }
	 if (fp2m < 0x200000) {
	   Fp34 = FP34minSingle;
	   Fp34.s = fp2->s;
	   break;
	 }
	 if (fp2m < 0x400000) {
	   fp34e = 0x1;
	   Fp34.e1 = fp34e >> 1;
	   Fp34.e0 = fp34e;
	   fp34m = 4 * (fp2m - 0x200000);
	   Fp34.m2 = (Byte1) (fp34m >> 16);
	   Fp34.m1 = (Byte1) (fp34m >> 8);
	   Fp34.m0 = (Byte1) fp34m;
	   Fp34.s = fp2->s;
	   break;
	 }
	 fp34e = 0x2;
	 Fp34.e1 = fp34e >> 1;
	 Fp34.e0 = fp34e;
	 fp34m = 2 * (fp2m - 0x400000);
	 Fp34.m2 = (Byte1) (fp34m >> 16);
	 Fp34.m1 = (Byte1) (fp34m >> 8);
	 Fp34.m0 = (Byte1) fp34m;
	 Fp34.s = fp2->s;
	 break;
       case 0xFE:
       case 0xFF:
	 Fp34 = FP34maxSingle;
	 Fp34.s = fp2->s;
	 break;
       default:
	 fp34e = fp2e + 2;
	 Fp34.e1 = fp34e >> 1;
	 Fp34.e0 = fp34e;
	 Fp34.m2 = fp2->m2;
	 Fp34.m1 = fp2->m1;
	 Fp34.m0 = fp2->m0;
	 Fp34.s = fp2->s;
     }
     *fp34 = Fp34;
  }
  return pStatus;
}

/******************************************************************************
* FP2toFP34singleNEGtoPOS.
*    Floating-point/2 to floating-point/34, single-precision.  Convert -0.0
* to 0.0.
******************************************************************************/

STATICforIDL CDFstatus FP2toFP34singleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  CDFstatus status = FP2toFP34single (buffer, numElems);
  if (status == NEGATIVE_FP_ZERO) FP34singleNEGtoPOS (buffer, numElems);
  return CDF_OK;
}

/******************************************************************************
* FP34toFP1single.
*    Floating-point/34 to floating-point/1, single-precision.
******************************************************************************/

STATICforIDL CDFstatus FP34toFP1single (buffer, numElems)
void *buffer;
Int32 numElems;
{
  uInt16 fp34e, fp1e;
  uInt32 fp34m, fp1m;
  struct fp34struct4 *fp34;
  struct fp1struct4 *fp1, Fp1;
  Int32 elemN;
#if DYNAMICfpSTRUCTs
  struct fp1struct4 FP1zeroSingle = { FP1ZEROsingle };
#endif
  for (elemN = 0,
       fp34 = (struct fp34struct4 *) buffer,
       fp1 = (struct fp1struct4 *) buffer;
       elemN < numElems;
       elemN++, fp34++, fp1++) {
     /*************************************************************************
     * Calculate the FP34 exponent.
     *************************************************************************/
     fp34e = (fp34->e1 << 1) | fp34->e0;
     switch (fp34e) {
       /***********************************************************************
       * If the FP34 exponent is zero (0), the value is 0.0 regardless of the
       * FP34 mantissa.  The FP34 sign shouldn't be set either since -0.0
       * isn't valid value on VAXes.  But since it might be set (for whatever
       * reason), set FP1's sign to that of FP34.
       ***********************************************************************/
       case 0x0:
	 Fp1 = FP1zeroSingle;
	 Fp1.s = fp34->s;
	 break;
       /***********************************************************************
       * FP34 exponents of one (1) or two (2) indicate values whose mantissas
       * are not the same between FP34 and FP1.  For an FP34 exponent of one
       * (1), the FP1 exponent is zero (0) and the full range of FP34
       * mantissas (0x0 through 0x7FFFFF) maps into the smaller range of FP1
       * mantissas from 0x200000 to 0x3FFFFF (a 4-to-1 mapping which means
       * that only one out of four FP34 values in this range maps perfectly to
       * FP1).  For an FP34 exponent of two (2), the FP1 exponent is zero (0)
       * and the full range of FP34 mantissas (0x0 through 0x7FFFFF) maps into
       * the smaller range of FP1 mantissas from 0x400000 to 0x7FFFFF (a
       * 2-to-1 mapping which means that only one out of two FP34 values in
       * this range maps perfectly to FP1).
       ***********************************************************************/
       case 0x1:
       case 0x2:
	 fp34m = (fp34->m2 << 16) | (fp34->m1 << 8) | fp34->m0;
	 if (fp34e == 0x1)
	   fp1m = 0x200000 + (fp34m / 4);
	 else
	   fp1m = 0x400000 + (fp34m / 2);
	 Fp1.m2 = (Byte1) (fp1m >> 16);
	 Fp1.m1 = (Byte1) (fp1m >> 8);
	 Fp1.m0 = (Byte1) fp1m;
	 Fp1.e1 = 0x0;
	 Fp1.e0 = 0x0;
	 Fp1.s = fp34->s;
	 break;
       /***********************************************************************
       * All other FP34 exponents are for FP34 values that map perfectly to FP1
       * (the FP1 exponent is just two less than the FP34 exponent).
       ***********************************************************************/
       default:
	 Fp1.m2 = fp34->m2;
	 Fp1.m1 = fp34->m1;
	 Fp1.m0 = fp34->m0;
	 fp1e = fp34e - 2;
	 Fp1.e1 = fp1e >> 1;
	 Fp1.e0 = fp1e;
	 Fp1.s = fp34->s;
     }
     *fp1 = Fp1;
  }
  return CDF_OK;
}

/******************************************************************************
* FP34toFP1singleNEGtoPOS.
*    Floating-point/34 to floating-point/1, single-precision.  Convert -0.0
* to 0.0.
******************************************************************************/

STATICforIDL CDFstatus FP34toFP1singleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  FP34toFP1single (buffer, numElems);
  FP1singleNEGtoPOS (buffer, numElems);
  return CDF_OK;
}

/******************************************************************************
* FP34toFP2single.
*    Floating-point/34 to floating-point/2, single-precision.
* See FP34toFP1single for comments.
******************************************************************************/

STATICforIDL CDFstatus FP34toFP2single (buffer, numElems)
void *buffer;
Int32 numElems;
{
  uInt16 fp34e, fp2e;
  uInt32 fp34m, fp2m;
  struct fp34struct4 *fp34;
  struct fp2struct4 *fp2, Fp2;
  Int32 elemN;
#if DYNAMICfpSTRUCTs
  struct fp2struct4 FP2zeroSingle = { FP2ZEROsingle };
#endif
  for (elemN = 0,
       fp34 = (struct fp34struct4 *) buffer,
       fp2 = (struct fp2struct4 *) buffer;
       elemN < numElems;
       elemN++, fp34++, fp2++) {
     fp34e = (fp34->e1 << 1) | fp34->e0;
     switch (fp34e) {
       case 0x0:
	 Fp2 = FP2zeroSingle;
	 Fp2.s = fp34->s;
	 break;
       case 0x1:
       case 0x2:
	 fp34m = (fp34->m2 << 16) | (fp34->m1 << 8) | fp34->m0;
	 if (fp34e == 0x1)
	   fp2m = 0x200000 + (fp34m / 4);
	 else
	   fp2m = 0x400000 + (fp34m / 2);
	 Fp2.m2 = (Byte1) (fp2m >> 16);
	 Fp2.m1 = (Byte1) (fp2m >> 8);
	 Fp2.m0 = (Byte1) fp2m;
	 Fp2.e1 = 0x0;
	 Fp2.e0 = 0x0;
	 Fp2.s = fp34->s;
	 break;
       default:
	 Fp2.m2 = fp34->m2;
	 Fp2.m1 = fp34->m1;
	 Fp2.m0 = fp34->m0;
	 fp2e = fp34e - 2;
	 Fp2.e1 = fp2e >> 1;
	 Fp2.e0 = fp2e;
	 Fp2.s = fp34->s;
     }
     *fp2 = Fp2;
  }
  return CDF_OK;
}

/******************************************************************************
* FP34toFP2singleNEGtoPOS.
*    Floating-point/34 to floating-point/2, single-precision.  Convert -0.0
* to 0.0.
******************************************************************************/

STATICforIDL CDFstatus FP34toFP2singleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  FP34toFP2single (buffer, numElems);
  FP2singleNEGtoPOS (buffer, numElems);
  return CDF_OK;
}

/******************************************************************************
* FP1toFP3double.
*   Floating-point/1 to floating-point/3, double-precision.
******************************************************************************/

STATICforIDL CDFstatus FP1toFP3double (buffer, numElems)
void *buffer;
Int32 numElems;
{
  uInt16 fp1e, fp3e;
  struct fp1struct8 *fp1;
  struct fp3struct8 *fp3, Fp3;
  Int32 elemN;
  CDFstatus pStatus = CDF_OK;
#if DYNAMICfpSTRUCTs
  struct fp3struct8 FP3zeroDouble = { FP3ZEROdouble };
  struct fp3struct8 FP3minDouble = { FP3MINdouble };
  struct fp3struct8 FP3maxDouble = { FP3MAXdouble };
#endif
  for (elemN = 0,
       fp1 = (struct fp1struct8 *) buffer,
       fp3 = (struct fp3struct8 *) buffer;
       elemN < numElems;
       elemN++, fp1++, fp3++) {
     /*************************************************************************
     * Calculate the FP1 exponent.
     *************************************************************************/
     fp1e = (fp1->e1 << 4) | fp1->e0;
     /*************************************************************************
     * Check if the FP1 exponent is too small for FP3 (but not 0.0).  If so,
     * set the FP3 value to its smallest possible magnitude (but use FP1's
     * sign).  Also check for -0.0.
     *************************************************************************/
     if (fp1e < 0x37F) {
       if (fp1e == 0x0 &&
	   fp1->m0 == 0x0 && fp1->m1 == 0x0 && fp1->m2 == 0x0 &&
	   fp1->m3 == 0x0 && fp1->m4 == 0x0 && fp1->m5 == 0x0 &&
	   fp1->m6 == 0x0) {
	 Fp3 = FP3zeroDouble;
	 Fp3.s = fp1->s;
	 if (Fp3.s == 0x1) pStatus = NEGATIVE_FP_ZERO;
       }
       else {
	 Fp3 = FP3minDouble;
	 Fp3.s = fp1->s;
       }
       *fp3 = Fp3;
       continue;
     }
     /*************************************************************************
     * Check if the FP1 exponent is too large for FP3.  If so, set the FP3
     * value to its largest possible magnitude (but use FP1's sign).
     *************************************************************************/
     if (fp1e > 0x47D) {
       Fp3 = FP3maxDouble;
       Fp3.s = fp1->s;
       *fp3 = Fp3;
       continue;
     }
     /*************************************************************************
     * The FP1 value will `fit' but the three (extra) least significant bits of
     * the FP3 mantissa are set to zero when the FP1 mantissa is copied to the
     * FP3 mantissa.
     *************************************************************************/
     fp3e = fp1e - 0x37E;
     Fp3.e0 = fp3e;
     Fp3.e1 = fp3e >> 1;
     Fp3.m0 = fp1->m0 << 3;
     Fp3.m1 = (fp1->m1 << 3) | (fp1->m0 >> 5);
     Fp3.m2 = (fp1->m2 << 3) | (fp1->m1 >> 5);
     Fp3.m3 = (fp1->m3 << 3) | (fp1->m2 >> 5);
     Fp3.m4 = (fp1->m4 << 3) | (fp1->m3 >> 5);
     Fp3.m5 = (fp1->m5 << 3) | (fp1->m4 >> 5);
     Fp3.m6 = (fp1->m6 << 3) | (fp1->m5 >> 5);
     Fp3.s = fp1->s;
     *fp3 = Fp3;
  }
#if LIMITfp3DOUBLEs
  /****************************************************************************
  * If this is a DEC Alpha (OpenVMS or POSIX Shell) built for a default of
  * D_FLOAT, check that the values do not exceed the maximum.
  ****************************************************************************/
  FP3doubleLIMIT (buffer, numElems);
#endif
  return pStatus;
}

/******************************************************************************
* FP1toFP3doubleNEGtoPOS.
*   Floating-point/1 to floating-point/3, double-precision.  Convert -0.0 to
* 0.0.
******************************************************************************/

STATICforIDL CDFstatus FP1toFP3doubleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  CDFstatus status = FP1toFP3double (buffer, numElems);
  if (status == NEGATIVE_FP_ZERO) FP3doubleNEGtoPOS (buffer, numElems);
  return CDF_OK;
}

/******************************************************************************
* FP1toFP4double.
*    Floating-point/1 to floating-point/4, double-precision.
******************************************************************************/

STATICforIDL CDFstatus FP1toFP4double (buffer, numElems)
void *buffer;
Int32 numElems;
{
  uInt16 fp1e, fp4e;
  uInt32 fp1mH, fp1mL, fp4mH, fp4mL;
  struct fp1struct8 *fp1;
  struct fp4struct8 *fp4, Fp4;
  CDFstatus pStatus = CDF_OK;
  Int32 elemN;
#if DYNAMICfpSTRUCTs
  struct fp4struct8 FP4zeroDouble = { FP4ZEROdouble };
  struct fp4struct8 FP4minDouble = { FP4MINdouble };
  struct fp4struct8 FP4maxDouble = { FP4MAXdouble };
#endif
  for (elemN = 0,
       fp1 = (struct fp1struct8 *) buffer,
       fp4 = (struct fp4struct8 *) buffer;
       elemN < numElems;
       elemN++, fp1++, fp4++) {
     /*************************************************************************
     * Calculate FP1's exponent.
     *************************************************************************/
     fp1e = (fp1->e1 << 4) | fp1->e0;
     switch (fp1e) {
       /***********************************************************************
       * FP1 exponent of zero (0) - what to do depends on the FP1 mantissa.
       ***********************************************************************/
       case 0x0:
	 fp1mH = (fp1->m6 << 16) | (fp1->m5 << 8) | fp1->m4;
	 fp1mL = (fp1->m3 << 24) | (fp1->m2 << 16) | (fp1->m1 << 8) | fp1->m0;
	 /*********************************************************************
	 * If the FP1 mantissa is zero (0), set FP34 to 0.0 and use the sign
	 * from FP1.  Also check for -0.0.
	 *********************************************************************/
	 if (fp1mH == 0x0 && fp1mL == 0x0) {
	   Fp4 = FP4zeroDouble;
	   Fp4.s = fp1->s;
	   if (Fp4.s == 0x1) pStatus = NEGATIVE_FP_ZERO;
	   break;
	 }
	 /*********************************************************************
	 * If the FP1 mantissa (with an FP1 exponent of zero [0]) puts the FP1
	 * value below the smallest magnitude possible for FP4, set FP4 to its
	 * minimum magnitude but with FP1's sign.
	 *********************************************************************/
	 if (fp1mH < 0x40000) {
	   Fp4 = FP4minDouble;
	   Fp4.s = fp1->s;
	   break;
	 }
	 /*********************************************************************
	 * If the FP1 mantissa is in the range where it maps 1-to-4 to FP4
	 * mantissas, set the FP4 mantissa to the first (lowest) value which
	 * maps.
	 *********************************************************************/
	 if (fp1mH < 0x80000) {
	   Fp4.e1 = 0x0;
	   Fp4.e0 = 0x1;
	   fp4mH = ((fp1mH - 0x40000) << 2) | (fp1mL >> 30);
	   fp4mL = fp1mL << 2;
	   Fp4.m6 = (Byte1) (fp4mH >> 16);
	   Fp4.m5 = (Byte1) (fp4mH >> 8);
	   Fp4.m4 = (Byte1) (fp4mH);
	   Fp4.m3 = (Byte1) (fp4mL >> 24);
	   Fp4.m2 = (Byte1) (fp4mL >> 16);
	   Fp4.m1 = (Byte1) (fp4mL >> 8);
	   Fp4.m0 = (Byte1) (fp4mL);
	   Fp4.s = fp1->s;
	   break;
	 }
	 /*********************************************************************
	 * The FP1 mantissa is in the range where it maps 1-to-2 to FP4
	 * mantissas, set the FP4 mantissa to the first (lowest) value which
	 * maps.
	 *********************************************************************/
	 Fp4.e1 = 0x0;
	 Fp4.e0 = 0x2;
	 fp4mH = ((fp1mH - 0x80000) << 1) | (fp1mL >> 31);
	 fp4mL = fp1mL << 1;
	 Fp4.m6 = (Byte1) (fp4mH >> 16);
	 Fp4.m5 = (Byte1) (fp4mH >> 8);
	 Fp4.m4 = (Byte1) (fp4mH);
	 Fp4.m3 = (Byte1) (fp4mL >> 24);
	 Fp4.m2 = (Byte1) (fp4mL >> 16);
	 Fp4.m1 = (Byte1) (fp4mL >> 8);
	 Fp4.m0 = (Byte1) (fp4mL);
	 Fp4.s = fp1->s;
	 break;
       /***********************************************************************
       * The FP1 exponent puts the FP1 value beyond the largest magnitude
       * possible for FP4 - set FP4 to its maximum magnitude but with FP1's
       * sign.
       ***********************************************************************/
       case 0x7FE:
       case 0x7FF:
	 Fp4 = FP4maxDouble;
	 Fp4.s = fp1->s;
	 break;
       /***********************************************************************
       * The FP1 value will `fit' into FP4.
       ***********************************************************************/
       default:
	 fp4e = fp1e + 2;
	 Fp4.e1 = fp4e >> 4;
	 Fp4.e0 = fp4e;
	 Fp4.m6 = fp1->m6;
	 Fp4.m5 = fp1->m5;
	 Fp4.m4 = fp1->m4;
	 Fp4.m3 = fp1->m3;
	 Fp4.m2 = fp1->m2;
	 Fp4.m1 = fp1->m1;
	 Fp4.m0 = fp1->m0;
	 Fp4.s = fp1->s;
     }
     *fp4 = Fp4;
  }
  return pStatus;
}

/******************************************************************************
* FP1toFP4doubleNEGtoPOS.
*   Floating-point/1 to floating-point/4, double-precision.  Convert -0.0 to
* 0.0.
******************************************************************************/

STATICforIDL CDFstatus FP1toFP4doubleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  CDFstatus status = FP1toFP4double (buffer, numElems);
  if (status == NEGATIVE_FP_ZERO) FP4doubleNEGtoPOS (buffer, numElems);
  return CDF_OK;
}

/******************************************************************************
* FP2toFP3double.
*   Floating-point/2 to floating-point/3, double-precision.
* See FP1toFP3double for comments.
******************************************************************************/

STATICforIDL CDFstatus FP2toFP3double (buffer, numElems)
void *buffer;
Int32 numElems;
{
  uInt16 fp2e, fp3e;
  struct fp2struct8 *fp2;
  struct fp3struct8 *fp3, Fp3;
  Int32 elemN;
  CDFstatus pStatus = CDF_OK;
#if DYNAMICfpSTRUCTs
  struct fp3struct8 FP3zeroDouble = { FP3ZEROdouble };
  struct fp3struct8 FP3minDouble = { FP3MINdouble };
  struct fp3struct8 FP3maxDouble = { FP3MAXdouble };
#endif
  for (elemN = 0,
       fp2 = (struct fp2struct8 *) buffer,
       fp3 = (struct fp3struct8 *) buffer;
       elemN < numElems;
       elemN++, fp2++, fp3++) {
     fp2e = (fp2->e1 << 4) | fp2->e0;
     if (fp2e < 0x37F) {
       if (fp2e == 0x0 &&
	   fp2->m0 == 0x0 && fp2->m1 == 0x0 && fp2->m2 == 0x0 &&
	   fp2->m3 == 0x0 && fp2->m4 == 0x0 && fp2->m5 == 0x0 &&
	   fp2->m6 == 0x0) {
	 Fp3 = FP3zeroDouble;
	 Fp3.s = fp2->s;
	 if (Fp3.s == 0x1) pStatus = NEGATIVE_FP_ZERO;
       }
       else {
	 Fp3 = FP3minDouble;
	 Fp3.s = fp2->s;
       }
       *fp3 = Fp3;
       continue;
     }
     if (fp2e > 0x47D) {
       Fp3 = FP3maxDouble;
       Fp3.s = fp2->s;
       *fp3 = Fp3;
       continue;
     }
     fp3e = fp2e - 0x37E;
     Fp3.e0 = fp3e;
     Fp3.e1 = fp3e >> 1;
     Fp3.m0 = fp2->m0 << 3;
     Fp3.m1 = (fp2->m1 << 3) | (fp2->m0 >> 5);
     Fp3.m2 = (fp2->m2 << 3) | (fp2->m1 >> 5);
     Fp3.m3 = (fp2->m3 << 3) | (fp2->m2 >> 5);
     Fp3.m4 = (fp2->m4 << 3) | (fp2->m3 >> 5);
     Fp3.m5 = (fp2->m5 << 3) | (fp2->m4 >> 5);
     Fp3.m6 = (fp2->m6 << 3) | (fp2->m5 >> 5);
     Fp3.s = fp2->s;
     *fp3 = Fp3;
  }
#if LIMITfp3DOUBLEs
  /****************************************************************************
  * If this is a DEC Alpha (OpenVMS or POSIX Shell) built for a default of
  * D_FLOAT, check that the values do not exceed the maximum.
  ****************************************************************************/
  FP3doubleLIMIT (buffer, numElems);
#endif
  return pStatus;
}

/******************************************************************************
* FP2toFP3doubleNEGtoPOS.
*   Floating-point/2 to floating-point/3, double-precision.  Convert -0.0 to
* 0.0.
******************************************************************************/

STATICforIDL CDFstatus FP2toFP3doubleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  CDFstatus status = FP2toFP3double (buffer, numElems);
  if (status == NEGATIVE_FP_ZERO) FP3doubleNEGtoPOS (buffer, numElems);
  return CDF_OK;
}

/******************************************************************************
* FP2toFP4double.
*    Floating-point/2 to floating-point/4, double-precision.
* See FP1toFP4double for comments.
******************************************************************************/

STATICforIDL CDFstatus FP2toFP4double (buffer, numElems)
void *buffer;
Int32 numElems;
{
  uInt16 fp2e, fp4e;
  uInt32 fp2mH, fp2mL, fp4mH, fp4mL;
  struct fp2struct8 *fp2;
  struct fp4struct8 *fp4, Fp4;
  CDFstatus pStatus = CDF_OK;
  Int32 elemN;
#if DYNAMICfpSTRUCTs
  struct fp4struct8 FP4zeroDouble = { FP4ZEROdouble };
  struct fp4struct8 FP4minDouble = { FP4MINdouble };
  struct fp4struct8 FP4maxDouble = { FP4MAXdouble };
#endif
  for (elemN = 0,
       fp2 = (struct fp2struct8 *) buffer,
       fp4 = (struct fp4struct8 *) buffer;
       elemN < numElems;
       elemN++, fp2++, fp4++) {
     fp2e = (fp2->e1 << 4) | fp2->e0;
     switch (fp2e) {
       case 0x0:
	 fp2mH = (fp2->m6 << 16) | (fp2->m5 << 8) | fp2->m4;
	 fp2mL = (fp2->m3 << 24) | (fp2->m2 << 16) | (fp2->m1 << 8) | fp2->m0;
	 if (fp2mH == 0x0 && fp2mL == 0x0) {
	   Fp4 = FP4zeroDouble;
	   Fp4.s = fp2->s;
	   if (Fp4.s == 0x1) pStatus = NEGATIVE_FP_ZERO;
	   break;
	 }
	 if (fp2mH < 0x40000) {
	   Fp4 = FP4minDouble;
	   Fp4.s = fp2->s;
	   break;
	 }
	 if (fp2mH < 0x80000) {
	   Fp4.e1 = 0x0;
	   Fp4.e0 = 0x1;
	   fp4mH = ((fp2mH - 0x40000) << 2) | (fp2mL >> 30);
	   fp4mL = fp2mL << 2;
	   Fp4.m6 = (Byte1) (fp4mH >> 16);
	   Fp4.m5 = (Byte1) (fp4mH >> 8);
	   Fp4.m4 = (Byte1) (fp4mH);
	   Fp4.m3 = (Byte1) (fp4mL >> 24);
	   Fp4.m2 = (Byte1) (fp4mL >> 16);
	   Fp4.m1 = (Byte1) (fp4mL >> 8);
	   Fp4.m0 = (Byte1) (fp4mL);
	   Fp4.s = fp2->s;
	   break;
	 }
	 Fp4.e1 = 0x0;
	 Fp4.e0 = 0x2;
	 fp4mH = ((fp2mH - 0x80000) << 1) | (fp2mL >> 31);
	 fp4mL = fp2mL << 1;
	 Fp4.m6 = (Byte1) (fp4mH >> 16);
	 Fp4.m5 = (Byte1) (fp4mH >> 8);
	 Fp4.m4 = (Byte1) (fp4mH);
	 Fp4.m3 = (Byte1) (fp4mL >> 24);
	 Fp4.m2 = (Byte1) (fp4mL >> 16);
	 Fp4.m1 = (Byte1) (fp4mL >> 8);
	 Fp4.m0 = (Byte1) (fp4mL);
	 Fp4.s = fp2->s;
	 break;
       case 0x7FE:
       case 0x7FF:
	 Fp4 = FP4maxDouble;
	 Fp4.s = fp2->s;
	 break;
       default:
	 fp4e = fp2e + 2;
	 Fp4.e1 = fp4e >> 4;
	 Fp4.e0 = fp4e;
	 Fp4.m6 = fp2->m6;
	 Fp4.m5 = fp2->m5;
	 Fp4.m4 = fp2->m4;
	 Fp4.m3 = fp2->m3;
	 Fp4.m2 = fp2->m2;
	 Fp4.m1 = fp2->m1;
	 Fp4.m0 = fp2->m0;
	 Fp4.s = fp2->s;
     }
     *fp4 = Fp4;
  }
  return pStatus;
}

/******************************************************************************
* FP2toFP4doubleNEGtoPOS.
*   Floating-point/1 to floating-point/4, double-precision.  Convert -0.0 to
* 0.0.
******************************************************************************/

STATICforIDL CDFstatus FP2toFP4doubleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  CDFstatus status = FP2toFP4double (buffer, numElems);
  if (status == NEGATIVE_FP_ZERO) FP4doubleNEGtoPOS (buffer, numElems);
  return CDF_OK;
}

/******************************************************************************
* FP3toFP1double.
*   Floating-point/3 to floating-point/1, double-precision.
******************************************************************************/

STATICforIDL CDFstatus FP3toFP1double (buffer, numElems)
void *buffer;
Int32 numElems;
{
  uInt16 fp3e, fp1e;
  struct fp3struct8 *fp3;
  struct fp1struct8 *fp1, Fp1;
  Int32 elemN;
#if DYNAMICfpSTRUCTs
  struct fp1struct8 FP1zeroDouble = { FP1ZEROdouble };
#endif
  for (elemN = 0,
       fp3 = (struct fp3struct8 *) buffer,
       fp1 = (struct fp1struct8 *) buffer;
       elemN < numElems;
       elemN++, fp3++, fp1++) {
     /*************************************************************************
     * Calculate the FP3 exponent.
     *************************************************************************/
     fp3e = (fp3->e1 << 1) | fp3->e0;
     /*************************************************************************
     * If the FP3 exponent is zero (0), the FP3 value is 0.0 regardless of the
     * mantissa's value.  The FP3 sign bit shouldn't be set either since -0.0
     * is not a valid value on VAXes.  But since it might be set (for whatever
     * reason), set FP1's sign to that of FP3.
     * If the FP3 exponent is not zero (0), the FP1 exponent is just 0x37E more
     * than the FP3 exponent and the three (extra) least signification bits of
     * the FP3 mantissa are lost (truncated) when the FP3 mantissa is copied to
     * the FP1 mantissa.
     *************************************************************************/
     if (fp3e == 0x0) {
       Fp1 = FP1zeroDouble;
       Fp1.s = fp3->s;
     }
     else {
       fp1e = fp3e + 0x37E;
       Fp1.e0 = fp1e;
       Fp1.e1 = fp1e >> 4;
       Fp1.m0 = (fp3->m1 << 5) | (fp3->m0 >> 3);
       Fp1.m1 = (fp3->m2 << 5) | (fp3->m1 >> 3);
       Fp1.m2 = (fp3->m3 << 5) | (fp3->m2 >> 3);
       Fp1.m3 = (fp3->m4 << 5) | (fp3->m3 >> 3);
       Fp1.m4 = (fp3->m5 << 5) | (fp3->m4 >> 3);
       Fp1.m5 = (fp3->m6 << 5) | (fp3->m5 >> 3);
       Fp1.m6 = fp3->m6 >> 3;
       Fp1.s = fp3->s;
     }
     *fp1 = Fp1;
  }
  return CDF_OK;
}

/******************************************************************************
* FP3toFP1doubleNEGtoPOS.
*   Floating-point/3 to floating-point/1, double-precision.  Convert -0.0 to
* 0.0.
******************************************************************************/

STATICforIDL CDFstatus FP3toFP1doubleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  FP3toFP1double (buffer, numElems);
  FP1doubleNEGtoPOS (buffer, numElems);
  return CDF_OK;
}

/******************************************************************************
* FP3toFP2double.
*   Floating-point/3 to floating-point/2, double-precision.
* See FP3toFP1double for comments.
******************************************************************************/

STATICforIDL CDFstatus FP3toFP2double (buffer, numElems)
void *buffer;
Int32 numElems;
{
  uInt16 fp3e, fp2e;
  struct fp3struct8 *fp3;
  struct fp2struct8 *fp2, Fp2;
  Int32 elemN;
#if DYNAMICfpSTRUCTs
  struct fp2struct8 FP2zeroDouble = { FP2ZEROdouble };
#endif
  for (elemN = 0,
       fp3 = (struct fp3struct8 *) buffer,
       fp2 = (struct fp2struct8 *) buffer;
       elemN < numElems;
       elemN++, fp3++, fp2++) {
     fp3e = (fp3->e1 << 1) | fp3->e0;
     if (fp3e == 0x0) {
       Fp2 = FP2zeroDouble;
       Fp2.s = fp3->s;
     }
     else {
       fp2e = fp3e + 0x37E;
       Fp2.e0 = fp2e;
       Fp2.e1 = fp2e >> 4;
       Fp2.m0 = (fp3->m1 << 5) | (fp3->m0 >> 3);
       Fp2.m1 = (fp3->m2 << 5) | (fp3->m1 >> 3);
       Fp2.m2 = (fp3->m3 << 5) | (fp3->m2 >> 3);
       Fp2.m3 = (fp3->m4 << 5) | (fp3->m3 >> 3);
       Fp2.m4 = (fp3->m5 << 5) | (fp3->m4 >> 3);
       Fp2.m5 = (fp3->m6 << 5) | (fp3->m5 >> 3);
       Fp2.m6 = fp3->m6 >> 3;
       Fp2.s = fp3->s;
     }
     *fp2 = Fp2;
  }
  return CDF_OK;
}

/******************************************************************************
* FP3toFP2doubleNEGtoPOS.
*   Floating-point/3 to floating-point/2, double-precision.  Convert -0.0 to
* +0.0.
******************************************************************************/

STATICforIDL CDFstatus FP3toFP2doubleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  FP3toFP2double (buffer, numElems);
  FP2doubleNEGtoPOS (buffer, numElems);
  return CDF_OK;
}

/******************************************************************************
* FP3toFP4double.
*   Floating-point/3 to floating-point/4, double-precision.
******************************************************************************/

STATICforIDL CDFstatus FP3toFP4double (buffer, numElems)
void *buffer;
Int32 numElems;
{
  uInt16 fp3e, fp4e;
  struct fp3struct8 *fp3;
  struct fp4struct8 *fp4, Fp4;
  Int32 elemN;
  CDFstatus pStatus = CDF_OK;
#if DYNAMICfpSTRUCTs
  struct fp4struct8 FP4zeroDouble = { FP4ZEROdouble };
#endif
  for (elemN = 0,
       fp3 = (struct fp3struct8 *) buffer,
       fp4 = (struct fp4struct8 *) buffer;
       elemN < numElems;
       elemN++, fp3++, fp4++) {
     /*************************************************************************
     * Calculate the FP3 exponent.
     *************************************************************************/
     fp3e = (fp3->e1 << 1) | fp3->e0;
     /*************************************************************************
     * If the FP3 exponent is zero (0), the FP3 value is 0.0 regardless of the
     * mantissa's value.  The FP3 sign bit shouldn't be set either since -0.0
     * is not a valid value on VAXes or DEC Alphas.  But since it might be set
     * (for whatever reason), set FP4's sign to that of FP3 [and check for
     * -0.0].
     * If the FP3 exponent is not zero (0), the FP4 exponent is just 0x380 more
     * than the FP3 exponent and the three (extra) least signification bits of
     * the FP3 mantissa are lost (truncated) when the FP3 mantissa is copied to
     * the FP4 mantissa.
     *************************************************************************/
     if (fp3e == 0x0) {
       Fp4 = FP4zeroDouble;
       Fp4.s = fp3->s;
       if (Fp4.s == 0x1) pStatus = NEGATIVE_FP_ZERO;
     }
     else {
       fp4e = fp3e + 0x380;
       Fp4.e0 = fp4e;
       Fp4.e1 = fp4e >> 4;
       Fp4.m0 = (fp3->m1 << 5) | (fp3->m0 >> 3);
       Fp4.m1 = (fp3->m2 << 5) | (fp3->m1 >> 3);
       Fp4.m2 = (fp3->m3 << 5) | (fp3->m2 >> 3);
       Fp4.m3 = (fp3->m4 << 5) | (fp3->m3 >> 3);
       Fp4.m4 = (fp3->m5 << 5) | (fp3->m4 >> 3);
       Fp4.m5 = (fp3->m6 << 5) | (fp3->m5 >> 3);
       Fp4.m6 = fp3->m6 >> 3;
       Fp4.s = fp3->s;
     }
     *fp4 = Fp4;
  }
  return pStatus;
}

/******************************************************************************
* FP3toFP4doubleNEGtoPOS.
*   Floating-point/3 to floating-point/4, double-precision.  Convert -0.0 to
* 0.0.
******************************************************************************/

STATICforIDL CDFstatus FP3toFP4doubleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  CDFstatus status = FP3toFP4double (buffer, numElems);
  if (status == NEGATIVE_FP_ZERO) FP4doubleNEGtoPOS (buffer, numElems);
  return CDF_OK;
}

/******************************************************************************
* FP4toFP1double.
*    Floating-point/4 to floating-point/1, double-precision.
******************************************************************************/

STATICforIDL CDFstatus FP4toFP1double (buffer, numElems)
void *buffer;
Int32 numElems;
{
  uInt16 fp4e, fp1e;
  uInt32 fp4mH, fp4mL, fp1mH, fp1mL;
  struct fp4struct8 *fp4;
  struct fp1struct8 *fp1, Fp1;
  Int32 elemN;
#if DYNAMICfpSTRUCTs
  struct fp1struct8 FP1zeroDouble = { FP1ZEROdouble };
#endif
  for (elemN = 0,
       fp4 = (struct fp4struct8 *) buffer,
       fp1 = (struct fp1struct8 *) buffer;
       elemN < numElems;
       elemN++, fp4++, fp1++) {
     /*************************************************************************
     * Calculate the FP4 exponent.
     *************************************************************************/
     fp4e = (fp4->e1 << 4) | fp4->e0;
     switch (fp4e) {
       /***********************************************************************
       * If the FP4 exponent is zero (0), the value is assumed to be 0.0
       * regardless of the FP4 mantissa (non-zero mantissas are illegal on
       * DEC Alphas).  The FP4 sign shouldn't be set either since -0.0 isn't
       * valid value on DEC Alphas.  But since it might be set (for whatever
       * reason), set FP1's sign to that of FP4.
       ***********************************************************************/
       case 0x0:
	 Fp1 = FP1zeroDouble;
	 Fp1.s = fp4->s;
	 break;
       /***********************************************************************
       * FP4 exponents of one (1) or two (2) indicate values whose mantissas
       * are not the same between FP4 and FP1.  For an FP4 exponent of one
       * (1), the FP1 exponent is zero (0) and the full range of FP4
       * mantissas (0x0 through 0xFFFFFFFFFFFFF) maps into the smaller range
       * of FP1 mantissas from 0x4000000000000 to 0x7FFFFFFFFFFFF (a 4-to-1
       * mapping which means that only one out of four FP4 values in this
       * range maps perfectly to FP1).  For an FP4 exponent of two (2), the
       * FP1 exponent is zero (0) and the full range of FP4 mantissas (0x0
       * through 0xFFFFFFFFFFFFF) maps into the smaller range of FP1 mantissas
       * from 0x8000000000000 to 0xFFFFFFFFFFFFF (a 2-to-1 mapping which means
       * that only one out of two FP4 values in this range maps perfectly to
       * FP1).
       ***********************************************************************/
       case 0x1:
       case 0x2:
	 fp4mH = (fp4->m6 << 16) | (fp4->m5 << 8) | fp4->m4;
	 fp4mL = (fp4->m3 << 24) | (fp4->m2 << 16) | (fp4->m1 << 8) | fp4->m0;
	 if (fp4e == 0x1) {
	   fp1mH = 0x40000 + (fp4mH >> 2);
	   fp1mL = (fp4mH << 30) | (fp4mL >> 2);
	 }
	 else {
	   fp1mH = 0x80000 + (fp4mH >> 1);
	   fp1mL = (fp4mH << 31) | (fp4mL >> 1);
	 }
	 Fp1.m6 = (Byte1) (fp1mH >> 16);
	 Fp1.m5 = (Byte1) (fp1mH >> 8);
	 Fp1.m4 = (Byte1) (fp1mH);
	 Fp1.m3 = (Byte1) (fp1mL >> 24);
	 Fp1.m2 = (Byte1) (fp1mL >> 16);
	 Fp1.m1 = (Byte1) (fp1mL >> 8);
	 Fp1.m0 = (Byte1) (fp1mL);
	 Fp1.e1 = 0x0;
	 Fp1.e0 = 0x0;
	 Fp1.s = fp4->s;
	 break;
       /***********************************************************************
       * All other FP4 exponents are for FP4 values that map perfectly to FP1
       * (the FP1 exponent is just two less than the FP4 exponent).
       ***********************************************************************/
       default:
	 Fp1.m6 = fp4->m6;
	 Fp1.m5 = fp4->m5;
	 Fp1.m4 = fp4->m4;
	 Fp1.m3 = fp4->m3;
	 Fp1.m2 = fp4->m2;
	 Fp1.m1 = fp4->m1;
	 Fp1.m0 = fp4->m0;
	 fp1e = fp4e - 2;
	 Fp1.e1 = fp1e >> 4;
	 Fp1.e0 = fp1e;
	 Fp1.s = fp4->s;
     }
     *fp1 = Fp1;
  }
  return CDF_OK;
}

/******************************************************************************
* FP4toFP1doubleNEGtoPOS.
*   Floating-point/4 to floating-point/1, double-precision.  Convert -0.0 to
* 0.0.
******************************************************************************/

STATICforIDL CDFstatus FP4toFP1doubleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  FP4toFP1double (buffer, numElems);
  FP1doubleNEGtoPOS (buffer, numElems);
  return CDF_OK;
}

/******************************************************************************
* FP4toFP2double.
*    Floating-point/4 to floating-point/1, double-precision.
* See FP4toFP1double for comments.
******************************************************************************/

STATICforIDL CDFstatus FP4toFP2double (buffer, numElems)
void *buffer;
Int32 numElems;
{
  uInt16 fp4e, fp2e;
  uInt32 fp4mH, fp4mL, fp2mH, fp2mL;
  struct fp4struct8 *fp4;
  struct fp2struct8 *fp2, Fp2;
  Int32 elemN;
#if DYNAMICfpSTRUCTs
  struct fp2struct8 FP2zeroDouble = { FP2ZEROdouble };
#endif
  for (elemN = 0,
       fp4 = (struct fp4struct8 *) buffer,
       fp2 = (struct fp2struct8 *) buffer;
       elemN < numElems;
       elemN++, fp4++, fp2++) {
     fp4e = (fp4->e1 << 4) | fp4->e0;
     switch (fp4e) {
       case 0x0:
	 Fp2 = FP2zeroDouble;
	 Fp2.s = fp4->s;
	 break;
       case 0x1:
       case 0x2:
	 fp4mH = (fp4->m6 << 16) | (fp4->m5 << 8) | fp4->m4;
	 fp4mL = (fp4->m3 << 24) | (fp4->m2 << 16) | (fp4->m1 << 8) | fp4->m0;
	 if (fp4e == 0x1) {
	   fp2mH = 0x40000 + (fp4mH >> 2);
	   fp2mL = (fp4mH << 30) | (fp4mL >> 2);
	 }
	 else {
	   fp2mH = 0x80000 + (fp4mH >> 1);
	   fp2mL = (fp4mH << 31) | (fp4mL >> 1);
	 }
	 Fp2.m6 = (Byte1) (fp2mH >> 16);
	 Fp2.m5 = (Byte1) (fp2mH >> 8);
	 Fp2.m4 = (Byte1) (fp2mH);
	 Fp2.m3 = (Byte1) (fp2mL >> 24);
	 Fp2.m2 = (Byte1) (fp2mL >> 16);
	 Fp2.m1 = (Byte1) (fp2mL >> 8);
	 Fp2.m0 = (Byte1) (fp2mL);
	 Fp2.e1 = 0x0;
	 Fp2.e0 = 0x0;
	 Fp2.s = fp4->s;
	 break;
       default:
	 Fp2.m6 = fp4->m6;
	 Fp2.m5 = fp4->m5;
	 Fp2.m4 = fp4->m4;
	 Fp2.m3 = fp4->m3;
	 Fp2.m2 = fp4->m2;
	 Fp2.m1 = fp4->m1;
	 Fp2.m0 = fp4->m0;
	 fp2e = fp4e - 2;
	 Fp2.e1 = fp2e >> 4;
	 Fp2.e0 = fp2e;
	 Fp2.s = fp4->s;
     }
     *fp2 = Fp2;
  }
  return CDF_OK;
}

/******************************************************************************
* FP4toFP2doubleNEGtoPOS.
*   Floating-point/4 to floating-point/2, double-precision.  Convert -0.0 to
* 0.0.
******************************************************************************/

STATICforIDL CDFstatus FP4toFP2doubleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  FP4toFP2double (buffer, numElems);
  FP2doubleNEGtoPOS (buffer, numElems);
  return CDF_OK;
}

/******************************************************************************
* FP4toFP3double.
*   Floating-point/4 to floating-point/3, double-precision.
******************************************************************************/

STATICforIDL CDFstatus FP4toFP3double (buffer, numElems)
void *buffer;
Int32 numElems;
{
  uInt16 fp4e, fp3e;
  struct fp4struct8 *fp4;
  struct fp3struct8 *fp3, Fp3;
  Int32 elemN;
  CDFstatus pStatus = CDF_OK;
#if DYNAMICfpSTRUCTs
  struct fp3struct8 FP3zeroDouble = { FP3ZEROdouble };
  struct fp3struct8 FP3minDouble = { FP3MINdouble };
  struct fp3struct8 FP3maxDouble = { FP3MAXdouble };
#endif
  for (elemN = 0,
       fp4 = (struct fp4struct8 *) buffer,
       fp3 = (struct fp3struct8 *) buffer;
       elemN < numElems;
       elemN++, fp4++, fp3++) {
     /*************************************************************************
     * Calculate the FP4 exponent.
     *************************************************************************/
     fp4e = (fp4->e1 << 4) | fp4->e0;
     /*************************************************************************
     * Check if the FP4 exponent is too small for FP3 (but not 0.0).  If so,
     * set the FP3 value to its smallest possible magnitude (but use FP4's
     * sign).  Also check for -0.0.
     *************************************************************************/
     if (fp4e < 0x381) {
       if (fp4e == 0x0) {
	 Fp3 = FP3zeroDouble;
	 Fp3.s = fp4->s;
	 if (Fp3.s == 0x1) pStatus = NEGATIVE_FP_ZERO;
       }
       else {
	 Fp3 = FP3minDouble;
	 Fp3.s = fp4->s;
       }
       *fp3 = Fp3;
       continue;
     }
     /*************************************************************************
     * Check if the FP4 exponent is too large for FP3.  If so, set the FP3
     * value to its largest possible magnitude (but use FP4's sign).
     *************************************************************************/
     if (fp4e > 0x47F) {
       Fp3 = FP3maxDouble;
       Fp3.s = fp4->s;
       *fp3 = Fp3;
       continue;
     }
     /*************************************************************************
     * The FP4 value will `fit' but the three (extra) least significant bits
     * of the FP3 mantissa are set to zero when the FP4 mantissa is copied to
     * the FP3 mantissa.
     *************************************************************************/
     fp3e = fp4e - 0x380;
     Fp3.e0 = fp3e;
     Fp3.e1 = fp3e >> 1;
     Fp3.m0 = fp4->m0 << 3;
     Fp3.m1 = (fp4->m1 << 3) | (fp4->m0 >> 5);
     Fp3.m2 = (fp4->m2 << 3) | (fp4->m1 >> 5);
     Fp3.m3 = (fp4->m3 << 3) | (fp4->m2 >> 5);
     Fp3.m4 = (fp4->m4 << 3) | (fp4->m3 >> 5);
     Fp3.m5 = (fp4->m5 << 3) | (fp4->m4 >> 5);
     Fp3.m6 = (fp4->m6 << 3) | (fp4->m5 >> 5);
     Fp3.s = fp4->s;
     *fp3 = Fp3;
  }
#if LIMITfp3DOUBLEs
  /****************************************************************************
  * If this is a DEC Alpha (OpenVMS or POSIX Shell) built for a default of
  * D_FLOAT, check that the values do not exceed the maximum.
  ****************************************************************************/
  FP3doubleLIMIT (buffer, numElems);
#endif
  return pStatus;
}

/******************************************************************************
* FP4toFP3doubleNEGtoPOS.
*   Floating-point/4 to floating-point/3, double-precision.  Convert -0.0 to
* 0.0.
******************************************************************************/

STATICforIDL CDFstatus FP4toFP3doubleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  CDFstatus status = FP4toFP3double (buffer, numElems);
  if (status == NEGATIVE_FP_ZERO) FP3doubleNEGtoPOS (buffer, numElems);
  return CDF_OK;
}

/******************************************************************************
* FP1toFP2singleNEGtoPOS.
*    Floating-point/1 to floating-point/2, single-precision.  Convert -0.0 to
* 0.0.
******************************************************************************/

STATICforIDL CDFstatus FP1toFP2singleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  Byte1 *ptr; Int32 elemN;
  for (elemN = 0, ptr = (Byte1 *) buffer;
       elemN < numElems; elemN++, ptr += 4) {
     REVERSE4b (ptr)
  }
  FP2singleNEGtoPOS (buffer, numElems);
  return CDF_OK;
}

/******************************************************************************
* FP2toFP1singleNEGtoPOS.
*    Floating-point/2 to floating-point/1, single-precision.  Convert -0.0 to
* 0.0.
******************************************************************************/

STATICforIDL CDFstatus FP2toFP1singleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  Byte1 *ptr; Int32 elemN;
  for (elemN = 0, ptr = (Byte1 *) buffer;
       elemN < numElems; elemN++, ptr += 4) {
     REVERSE4b (ptr)
  }
  FP1singleNEGtoPOS (buffer, numElems);
  return CDF_OK;
}

/******************************************************************************
* FP1toFP2doubleNEGtoPOS.
*    Floating-point/1 to floating-point/2, single-precision.  Convert -0.0 to
* 0.0.
******************************************************************************/

STATICforIDL CDFstatus FP1toFP2doubleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  Byte1 *ptr; Int32 elemN;
  for (elemN = 0, ptr = (Byte1 *) buffer;
       elemN < numElems; elemN++, ptr += 8) {
     REVERSE8b (ptr)
  }
  FP2doubleNEGtoPOS (buffer, numElems);
  return CDF_OK;
}

/******************************************************************************
* FP2toFP1doubleNEGtoPOS.
*    Floating-point/2 to floating-point/1, single-precision.  Convert -0.0 to
* 0.0.
******************************************************************************/

STATICforIDL CDFstatus FP2toFP1doubleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  Byte1 *ptr; Int32 elemN;
  for (elemN = 0, ptr = (Byte1 *) buffer;
       elemN < numElems; elemN++, ptr += 8) {
     REVERSE8b (ptr)
  }
  FP1doubleNEGtoPOS (buffer, numElems);
  return CDF_OK;
}

/******************************************************************************
* FP1singleNEGtoPOS.
*    Convert -0.0 to 0.0 for floating-point/1 single-precision.
******************************************************************************/

STATICforIDL CDFstatus FP1singleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  Byte1 *ptr;
  Int32 elemN;
#if DYNAMICfpSTRUCTs
  struct fp1struct4 FP1zeroSingle = { FP1ZEROsingle };
  struct fp1struct4 FP1zeroSingleNeg = { FP1ZEROsingleNEG };
#endif
  for (elemN = 0, ptr = (Byte1 *) buffer; elemN < numElems; elemN++, ptr += 4) {
     if (*((uInt32 *) ptr) == *((uInt32 *) &FP1zeroSingleNeg)) {
       *((struct fp1struct4 *) ptr) = FP1zeroSingle;
     }
  }
  return CDF_OK;
}

/******************************************************************************
* FP2singleNEGtoPOS.
*    Convert -0.0 to 0.0 for floating-point/2 single-precision.
******************************************************************************/

STATICforIDL CDFstatus FP2singleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  Byte1 *ptr;
  Int32 elemN;
#if DYNAMICfpSTRUCTs
  struct fp2struct4 FP2zeroSingle = { FP2ZEROsingle };
  struct fp2struct4 FP2zeroSingleNeg = { FP2ZEROsingleNEG };
#endif
  for (elemN = 0, ptr = (Byte1 *) buffer; elemN < numElems; elemN++, ptr += 4) {
     if (*((uInt32 *) ptr) == *((uInt32 *) &FP2zeroSingleNeg)) {
       *((struct fp2struct4 *) ptr) = FP2zeroSingle;
     }
  }
  return CDF_OK;
}

/******************************************************************************
* FP34singleNEGtoPOS.
*    Convert -0.0 to 0.0 for floating-point/34 single-precision.
******************************************************************************/

STATICforIDL CDFstatus FP34singleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  struct fp34struct4 *fp34;
  Int32 elemN;
#if DYNAMICfpSTRUCTs
  struct fp34struct4 FP34zeroSingle = { FP34ZEROsingle };
#endif
  for (elemN = 0, fp34 = (struct fp34struct4 *) buffer;
       elemN < numElems; elemN++, fp34++) {
     if (fp34->e1 == 0x0 && fp34->e0 == 0x0 && fp34->s == 0x1) {
       *fp34 = FP34zeroSingle;
     }
  }
  return CDF_OK;
}

/******************************************************************************
* FP1doubleNEGtoPOS.
*    Convert -0.0 to 0.0 for floating-point/1 double-precision.
******************************************************************************/

STATICforIDL CDFstatus FP1doubleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  Byte1 *ptr;
  Int32 elemN;
#if DYNAMICfpSTRUCTs
  struct fp1struct8 FP1zeroDouble = { FP1ZEROdouble };
  struct fp1struct8 FP1zeroDoubleNeg = { FP1ZEROdoubleNEG };
#endif
  for (elemN = 0, ptr = (Byte1 *) buffer; elemN < numElems; elemN++, ptr += 8) {
     if (*((uInt32 *) ptr) == *((uInt32 *) &FP1zeroDoubleNeg) &&
	 *((uInt32 *) ptr + 1) == *((uInt32 *) &FP1zeroDoubleNeg + 1)) {
       *((struct fp1struct8 *) ptr) = FP1zeroDouble;
     }
  }
  return CDF_OK;
}

/******************************************************************************
* FP2doubleNEGtoPOS.
*    Convert -0.0 to 0.0 for floating-point/2 double-precision.
******************************************************************************/

STATICforIDL CDFstatus FP2doubleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  Byte1 *ptr;
  Int32 elemN;
#if DYNAMICfpSTRUCTs
  struct fp2struct8 FP2zeroDouble = { FP2ZEROdouble };
  struct fp2struct8 FP2zeroDoubleNeg = { FP2ZEROdoubleNEG };
#endif
  for (elemN = 0, ptr = (Byte1 *) buffer; elemN < numElems; elemN++, ptr += 8) {
     if (*((uInt32 *) ptr) == *((uInt32 *) &FP2zeroDoubleNeg) &&
	 *((uInt32 *) ptr + 1) == *((uInt32 *) &FP2zeroDoubleNeg + 1)) {
       *((struct fp2struct8 *) ptr) = FP2zeroDouble;
     }
  }
  return CDF_OK;
}

/******************************************************************************
* FP3doubleNEGtoPOS.
*    Convert -0.0 to 0.0 for floating-point/3 double-precision.
******************************************************************************/

STATICforIDL CDFstatus FP3doubleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  struct fp3struct8 *fp3;
  Int32 elemN;
#if DYNAMICfpSTRUCTs
  struct fp3struct8 FP3zeroDouble = { FP3ZEROdouble };
#endif
  for (elemN = 0, fp3 = (struct fp3struct8 *) buffer;
       elemN < numElems; elemN++, fp3++) {
     if (fp3->e1 == 0x0 && fp3->e0 == 0x0 && fp3->s == 0x1) {
       *fp3 = FP3zeroDouble;
     }
  }
#if LIMITfp3DOUBLEs
  /****************************************************************************
  * If this is a DEC Alpha (OpenVMS or POSIX Shell) built for a default of
  * D_FLOAT, check that the values do not exceed the maximum.
  ****************************************************************************/
  FP3doubleLIMIT (buffer, numElems);
#endif
  return CDF_OK;
}

/******************************************************************************
* FP4doubleNEGtoPOS.
*    Convert -0.0 to 0.0 for floating-point/4 double-precision.
******************************************************************************/

STATICforIDL CDFstatus FP4doubleNEGtoPOS (buffer, numElems)
void *buffer;
Int32 numElems;
{
  struct fp4struct8 *fp4; Int32 elemN;
#if DYNAMICfpSTRUCTs
  struct fp4struct8 FP4zeroDouble = { FP4ZEROdouble };
#endif
  for (elemN = 0, fp4 = (struct fp4struct8 *) buffer;
       elemN < numElems; elemN++, fp4++) {
     if (fp4->e1 == 0x0 && fp4->e0 == 0x0 && fp4->s == 0x1) {
       *fp4 = FP4zeroDouble;
     }
  }
  return CDF_OK;
}

/******************************************************************************
* FP3doubleLIMIT.
*    Check that the maximum double-precision, floating-point/3 value on a
* DEC Alpha (built for D_FLOAT) is not exceeded.  The maximum allowed value
* has an exponent of 0xFF and a mantissa of 0x7FFFFFFFFFFFFB.
******************************************************************************/

STATICforIDL CDFstatus FP3doubleLIMIT (buffer, numElems)
void *buffer;
Int32 numElems;
{
  struct fp3struct8 *fp3; Int32 elemN;
  for (elemN = 0, fp3 = (struct fp3struct8 *) buffer;
       elemN < numElems; elemN++, fp3++) {
     if (fp3->e1 == 0x7F && fp3->e0 == 0x1 && fp3->m6 == 0x7F &&
	 fp3->m5 == 0xFF && fp3->m4 == 0xFF && fp3->m3 == 0xFF &&
	 fp3->m2 == 0xFF && fp3->m1 == 0xFF && fp3->m0 > 0xFB) {
       fp3->m0 = 0xFB;
     }
  }
  return CDF_OK;
}

/******************************************************************************
* Reverse2.
******************************************************************************/

STATICforIDL CDFstatus Reverse2 (buffer, numElems)
void *buffer;
Int32 numElems;
{
  Byte1 *ptr; Int32 elemN;
  for (elemN = 0, ptr = (Byte1 *) buffer; elemN < numElems; elemN++, ptr += 2) {
     REVERSE2b (ptr)
  }
  return CDF_OK;
}

/******************************************************************************
* Reverse4.
******************************************************************************/

STATICforIDL CDFstatus Reverse4 (buffer, numElems)
void *buffer;
Int32 numElems;
{
  Byte1 *ptr; Int32 elemN;
  for (elemN = 0, ptr = (Byte1 *) buffer; elemN < numElems; elemN++, ptr += 4) {
     REVERSE4b (ptr)
  }
  return CDF_OK;
}

/******************************************************************************
* Reverse8.
******************************************************************************/

STATICforIDL CDFstatus Reverse8 (buffer, numElems)
void *buffer;
Int32 numElems;
{
  Byte1 *ptr; Int32 elemN;
  for (elemN = 0, ptr = (Byte1 *) buffer; elemN < numElems; elemN++, ptr += 8) {
     REVERSE8b (ptr)
  }
  return CDF_OK;
}

/******************************************************************************
* Reverse16.
******************************************************************************/

STATICforIDL CDFstatus Reverse16 (buffer, numElems)
void *buffer;
Int32 numElems;
{
  Byte1 *ptr; Int32 elemN;
  for (elemN = 0, ptr = (Byte1 *) buffer; elemN < numElems; elemN++, ptr += 16) {
     REVERSE8b (ptr)
     REVERSE8b (ptr+8)
  }
  return CDF_OK;
}


