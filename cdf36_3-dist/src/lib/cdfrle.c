/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                    Run-length encoding compression/decompression.
*
*  Version 1.0, 15-May-96, Hughes STX.
*
*  Modification history:
*
*   V1.0  15-May-96, J Love     Original version.
*
******************************************************************************/

#include "cdflib.h"

#if !SUPPORT_RLE && defined(BORLANDC)
#pragma warn -par
#endif

/******************************************************************************
* CompressRLE0.
******************************************************************************/

STATICforIDL CDFstatus CompressRLE0 (srcFp, srcOffset, srcSize, srcError,
				     destFp, destOffset, destSize, destError)
vFILE *srcFp;
Int32 srcOffset;
Int32 srcSize;
CDFstatus srcError;
vFILE *destFp;
Int32 destOffset;
Int32 *destSize;
CDFstatus destError;
{
#if SUPPORT_RLE
  CDFstatus pStatus = CDF_OK; Int32 byteN = 0, count;
  uByte byte, zero = 0, max255 = (uByte) 255, zCount;
  /****************************************************************************
  * Seek to starting offsets.
  ****************************************************************************/
  if (!SEEKv(srcFp,(long)srcOffset,vSEEK_SET)) return srcError;
  if (!SEEKv(destFp,(long)destOffset,vSEEK_SET)) return destError;
  /****************************************************************************
  * Initialize destination count and begin...
  ****************************************************************************/
  *destSize = 0;
  for (;;) {
     /*************************************************************************
     * Return if past the last source byte.
     *************************************************************************/
     if (byteN == srcSize) return pStatus;
     /*************************************************************************
     * Read the next source byte.
     *************************************************************************/
     if (!READv(&byte,1,1,srcFp)) return srcError;
     byteN++;
     /*************************************************************************
     * If the source byte is zero, set the counter and begin...
     *************************************************************************/
     if (byte == 0) {
       count = 1;
       for (;;) {
	  /********************************************************************
	  * If past the last source byte, write a zero/count to the
	  * destination file and return.  In this case the final `count'
	  * bytes of the source file are zeroes.
	  ********************************************************************/
	  if (byteN == srcSize) {
	    if (!WRITEv(&zero,1,1,destFp)) return destError;
	    (*destSize)++;
	    zCount = (uByte) (count - 1);
	    if (!WRITEv(&zCount,1,1,destFp)) return destError;
	    (*destSize)++;
	    return pStatus;
	  }
	  /********************************************************************
	  * Read the next source byte.
	  ********************************************************************/
	  if (!READv(&byte,1,1,srcFp)) return srcError;
	  byteN++;
	  /********************************************************************
	  * If the byte is not a zero, write a zero/count and the non-zero
	  * byte to the destination file and break out of this loop.
	  ********************************************************************/
	  if (byte != 0) {
	    if (!WRITEv(&zero,1,1,destFp)) return destError;
	    (*destSize)++;
	    zCount = (Byte1) (count - 1);
	    if (!WRITEv(&zCount,1,1,destFp)) return destError;
	    (*destSize)++;
	    if (!WRITEv(&byte,1,1,destFp)) return destError;
	    (*destSize)++;
	    break;
	  }
	  /********************************************************************
	  * Increment for another zero and check if at the limit.  If so,
	  * write a zero/count to the destination and break out of this loop.
	  ********************************************************************/
	  count++;
	  if (count == 256) {
	    if (!WRITEv(&zero,1,1,destFp)) return destError;
	    (*destSize)++;
	    if (!WRITEv(&max255,1,1,destFp)) return destError;
	    (*destSize)++;
	    break;
	  }
       }
     }
     else {
       /***********************************************************************
       * A non-zero source byte - write it to the destination file.
       ***********************************************************************/
       if (!WRITEv(&byte,1,1,destFp)) return destError;
       (*destSize)++;
     }
  }
#else
  return UNKNOWN_COMPRESSION;
#endif
}

/******************************************************************************
* DecompressRLE0.
******************************************************************************/

STATICforIDL CDFstatus DecompressRLE0 (srcFp, srcOffset, srcSize, srcError,
				       destFp, destOffset, destError)
vFILE *srcFp;
Int32 srcOffset;
Int32 srcSize;
CDFstatus srcError;
vFILE *destFp;
Int32 destOffset;
CDFstatus destError;
{
#if SUPPORT_RLE
  CDFstatus pStatus = CDF_OK; Int32 byteN = 0, count, i;
  uByte byte, zero = 0, zCount;
  /****************************************************************************
  * Seek to starting offsets.
  ****************************************************************************/
  if (!SEEKv(srcFp,(long)srcOffset,vSEEK_SET)) return srcError;
  if (!SEEKv(destFp,(long)destOffset,vSEEK_SET)) return destError;
  /****************************************************************************
  * Begin...
  ****************************************************************************/
  for (;;) {
     /*************************************************************************
     * Return if past the last source byte
     *************************************************************************/
     if (byteN == srcSize) return pStatus;
     /*************************************************************************
     * Read a source byte.  If it is a zero, read the following count byte
     * and write the specified number of zeroes to the destination file.
     * Otherwise, just write the non-zero byte.  Note that the count byte is
     * one less than the number of zeroes.
     *************************************************************************/
     if (!READv(&byte,1,1,srcFp)) return srcError;
     byteN++;
     if (byte == 0) {
       if (byteN == srcSize) return DECOMPRESSION_ERROR;
       if (!READv(&zCount,1,1,srcFp)) return srcError;
       byteN++;
       count = ((Int32) zCount) + 1;
       for (i = 0; i < count; i++) {
	  if (!WRITEv(&zero,1,1,destFp)) return destError;
       }
     }
     else
       if (!WRITEv(&byte,1,1,destFp)) return destError;
  }
#else
  return UNKNOWN_COMPRESSION;
#endif
}
