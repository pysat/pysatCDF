/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                  CDF library hyper functions.
*
*  Version 1.4, 26-Jun-96, Hughes STX.
*
*  Modification history:
*
*   V1.0  20-May-92, J Love     Original version.  These functions were taken
*                               out of `cdflib.c'.
*   V1.1  16-Sep-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V1.2  22-Nov-93, J Love     CDF 2.4.  New hyper algorithms.
*   V1.3   7-Dec-94, J Love     CDF V2.5.
*   V1.3a  6-Jan-95, J Love     Encode/decode changes.  More cache-residency.
*   V1.3b 24-Feb-95, J Love     Solaris 2.3 IDL i/f.
*   V1.4  26-Jun-96, J Love     CDF V2.6.
*   V1.5  26-May-98, M Liu      Add a parameter for HyperReadDim and 
*				HyperWriteDim function calls. For 
*				reading/writing data values of a non-full 
*				physical record, a temp. buffer is used to cut
*				down the physical I/Os.  
*				
******************************************************************************/

#include "cdflib.h"

/******************************************************************************
* HyperRead.
******************************************************************************/

STATICforIDL CDFstatus HyperRead (CDF, Var, rd, buffer)
struct CDFstruct *CDF;
struct VarStruct *Var;
struct rdSTRUCT *rd;
void *buffer;
{
  Int32 nHypRecValues, nHypRecBytes, nHypDimValues[CDF_MAX_DIMS];
  Logical fullPhyRec, fullPhyDim[CDF_MAX_DIMS];
  Int32 firstHypRec, lastHypRec, firstPhyRec;
  Int32 recNum, nValues, nRecords, phyRecN, i; Byte1 *tBuffer, *phyBuffer;
  CDFstatus pStatus = CDF_OK;
  int dimN, dimNt;
  int firstDim;                 /* Based on majority, the "highest" dimension
				   (changes the slowest in memory/on disk). */
  int doneDim;                  /* When cycling through the dimensions, the
				   dimension at which to stop (the dimension
				   beyond the last). */
  int dimIncr;                  /* How to cycle through the dimensions.  Incr-
				   ementing (+1) for row-major, decrementing
				   (-1) for column-major. */
  /****************************************************************************
  * Determine dimension ordering.
  ****************************************************************************/
  if (Var->numDims > 0) {
    firstDim = (int) (CDF->rowMajor ? 0 : Var->numDims - 1);
    doneDim = (int) (CDF->rowMajor ? Var->numDims : -1);
    dimIncr = (CDF->rowMajor ? 1 : -1);
  }
  /****************************************************************************
  * Determine if full physical dimensions can be read.  In order for this to
  * be so for a particular dimension...
  *   1. If the dimension variance is TRUE, then the entire dimension must be
  *      read (the count equal to the size).  If the dimension variance is
  *      FALSE, the count must be equal to one (only reading the single
  *      physical value).
  *   2. For each `lower' dimension having a TRUE variance, the entire
  *      dimension must be read (the count equal to the size).
  *   3. For each `lower' dimension having a FALSE variance, the count must
  *      be equal to one (only reading the single physical dimension).
  * Also determine if full physical records can be read.  If there are one or
  * more dimensions, this depends on whether or not the first dimension can be
  * read with a single physical read.  If there are zero dimensions, then this
  * is always true.
  ****************************************************************************/
  if (Var->numDims > 0) {
    for (dimN = firstDim; dimN != doneDim; dimN += dimIncr) {
       fullPhyDim[dimN] = TRUE;
       for (dimNt = dimN; dimNt != doneDim; dimNt += dimIncr)
	  if ((Var->dimVarys[dimNt] &&
	       rd->dimCounts[dimNt] != Var->dimSizes[dimNt]) ||
	      (!Var->dimVarys[dimNt] && rd->dimCounts[dimNt] > 1)) {
	    fullPhyDim[dimN] = FALSE;
	    break;
	  }
    }
    fullPhyRec = fullPhyDim[firstDim];
  }
  else
    fullPhyRec = TRUE;
  /****************************************************************************
  * Determine if only one read is needed.  In order for this to be so...
  *   1. Full physical records must be being read, and
  *   2a. If the record variance is TRUE, then the record interval must be one
  *       (or a record count of one) so that there is no skipping of records
  *   2b. If the record variance is FALSE.
  ****************************************************************************/
  firstHypRec = rd->recNumber;
  lastHypRec = firstHypRec + (rd->recInterval * (rd->recCount - 1));
  firstPhyRec = (Var->recVary ? firstHypRec : 0);
  if (fullPhyRec && 
      ((Var->recVary && (rd->recInterval == 1 || rd->recCount == 1))
		||
       (!Var->recVary))) {
    nRecords = lastHypRec - firstHypRec + 1;
    nValues = nRecords * Var->NphyRecValues;
    if (!sX(ReadVarValues(CDF,Var,firstPhyRec,
			  INT32_ZERO,nValues,buffer),&pStatus)) return pStatus;
    return pStatus;
  }
  /****************************************************************************
  * A single read was not possible - read one record at a time.
  ****************************************************************************/
  if (Var->numDims > 0) {
    for (dimN = firstDim; dimN != doneDim; dimN += dimIncr) {
       nHypDimValues[dimN] = 1;
       for (dimNt = dimN + dimIncr; dimNt != doneDim; dimNt += dimIncr)
	  nHypDimValues[dimN] *= rd->dimCounts[dimNt];
    }
    nHypRecValues = nHypDimValues[firstDim] * rd->dimCounts[firstDim];
  }
  else
    nHypRecValues = 1;
  nHypRecBytes = nHypRecValues * Var->NvalueBytes;
  for (i = 0, recNum = rd->recNumber, tBuffer = buffer; i < rd->recCount;
       i++, recNum += rd->recInterval, tBuffer += (size_t) nHypRecBytes) {
     phyRecN = BOO(Var->recVary,recNum,0);
     if (fullPhyRec) {
       /*******************************************************************
       * The full physical record is being read, use one physical read.
       *******************************************************************/
       if (!sX(ReadVarValues(CDF,Var,phyRecN,INT32_ZERO,
			     Var->NphyRecValues,
			     tBuffer),&pStatus)) return pStatus;
     }
     else {
       /*******************************************************************
       * Less than the full physical record is to be read. Allocate a 
       * physical record buffer if possible and read in the whole record
       * and extract values from the buffer. Otherwise, get one value at a
       * time from the file
       *******************************************************************/
       phyBuffer = (Byte1 *) cdf_AllocateMemory((size_t) Var->NphyRecBytes,NULL);
       if (phyBuffer != NULL) {
         if (!sX(ReadVarValues(CDF,Var,phyRecN,INT32_ZERO,Var->NphyRecValues,
                               phyBuffer),&pStatus)) {
	   cdf_FreeMemory(phyBuffer,NULL);
	   return pStatus;
	 }
       }	
       if (!sX(HyperReadDim(Var->numDims,Var->dimSizes,
			    Var->dimVarys,rd->dimIndices,
			    rd->dimCounts,rd->dimIntervals,
			    nHypDimValues,Var->nPhyDimValues,
			    fullPhyDim,firstDim,dimIncr,
			    phyRecN,INT32_ZERO,tBuffer,phyBuffer,
			    CDF,Var),&pStatus)) {
         if (phyBuffer != NULL) cdf_FreeMemory(phyBuffer,NULL);
         return pStatus;
       }
       if (phyBuffer != NULL) cdf_FreeMemory(phyBuffer,NULL); 
     }
  }
  return pStatus;
}

/******************************************************************************
* HyperReadDim.  DANGER, DANGER, I'm recursive.
*   NOTE: This routine could be called for a record which is virtual.
******************************************************************************/

STATICforIDL CDFstatus HyperReadDim (numDims, dimSizes, dimVarys, indices,
				     counts, intervals, nHypDimValues,
				     nPhyDimValues, fullPhyDim, firstDim,
				     dimIncr, recNum, offset, buffer, phyBuffer,
				     CDF, Var)
Int32 numDims;
Int32 *dimSizes;
Int32 *dimVarys;
Int32 *indices;
Int32 *counts;
Int32 *intervals;
Int32 *nHypDimValues;
Int32 *nPhyDimValues;
Logical *fullPhyDim;
int firstDim;
int dimIncr;
Int32 recNum;
Int32 offset;           /* Byte offset within record. */
void *buffer;
void *phyBuffer;
struct CDFstruct *CDF;
struct VarStruct *Var;
{
  Int32 tOffset; Int32 nValues, i; Byte1 *tBuffer; CDFstatus pStatus = CDF_OK;
  /****************************************************************************
  * What to do depends on the number of dimensions.  Note that this function
  * should never be called with zero dimensions.
  ****************************************************************************/
  switch (numDims) {
    case 1: {
      /************************************************************************
      * One dimension - read the values along the dimension.
      ************************************************************************/
      if (dimVarys[0]) {
	/**********************************************************************
	* Dimension variance of TRUE, no virtual values to deal with - read
	* physical values.
	**********************************************************************/
	if (intervals[0] == 1) {
	  /********************************************************************
	  * A contiguous strip of values.
	  ********************************************************************/
	  tOffset = offset + (indices[0] * Var->NvalueBytes);
  	  if (phyBuffer == NULL) {
	    if (!sX(ReadVarValues(CDF,Var,recNum,
		 		  tOffset,counts[0],
				  buffer),&pStatus)) return pStatus; 
	  } else {
	    memmove (buffer, (size_t) tOffset + (Byte1 *) phyBuffer, 
		     (size_t) (Var->NvalueBytes * counts[0]));
	  }
	}
	else {
	  /********************************************************************
	  * Not contiguous, read one value at a time skipping over unwanted
	  * values.
	  ********************************************************************/
	  tOffset = offset + (indices[0] * Var->NvalueBytes);
	  tBuffer = buffer; 
	  for (i = 0; i < counts[0]; i++) {
  	     if (phyBuffer == NULL) {
		if (!sX(ReadVarValues(CDF,Var,recNum,
				      tOffset,INT32_ONE,
				      tBuffer),&pStatus)) return pStatus; 
	     } else {
                memmove (tBuffer, (size_t) tOffset+(Byte1 *) phyBuffer, 
			 (size_t) Var->NvalueBytes);
	     }
	     tOffset += (intervals[0] * Var->NvalueBytes);
	     tBuffer += (size_t) Var->NvalueBytes;
	  }
	}
      }
      else {
	/**********************************************************************
	* Dimension variance of FALSE, only one physical value exists.  If the
	* count is greater than one, virtual values will have to be generated.
	**********************************************************************/
  	if (phyBuffer == NULL) {
	  if (!sX(ReadVarValues(CDF,Var,recNum,offset,
			        INT32_ONE,buffer),&pStatus)) return pStatus;
	} else {
	  memmove(buffer, (size_t) offset + (Byte1 *) phyBuffer, 
		  (size_t) Var->NvalueBytes);
	}
	if (counts[0] > 1) {
	  for (i = 1, tBuffer = (Byte1 *) buffer + (size_t) Var->NvalueBytes;
	       i < counts[0]; i++, tBuffer += (size_t) Var->NvalueBytes) {
	     memmove (tBuffer, buffer, (size_t) Var->NvalueBytes);
	  }
	}
      }
      break;
    }
    default: {
      /************************************************************************
      * Two or more dimensions.
      ************************************************************************/
      Int32 nPhyDimBytes = nPhyDimValues[firstDim] * Var->NvalueBytes;
      Int32 nHypDimBytes = nHypDimValues[firstDim] * Var->NvalueBytes;
      int nextDim = firstDim + dimIncr;
      if (dimVarys[firstDim]) {
	/**********************************************************************
	* The first dimension's variance is TRUE.  If the interval is one and
	* only a single read is necessary for the "lower" dimension, use a
	* single read.  Otherwise, cycle through that dimension physically
	* reading each subarray below it (skipping subarrays if necessary).
	**********************************************************************/
	if (intervals[firstDim] == 1 && fullPhyDim[nextDim]) {
	  tOffset = offset + (indices[firstDim] * nPhyDimBytes);
	  nValues = counts[firstDim] * nPhyDimValues[firstDim];
  	  if (phyBuffer == NULL) {
	    if (!sX(ReadVarValues(CDF,Var,recNum,
				  tOffset,nValues,
				  buffer),&pStatus)) return pStatus;
	  } else {
	    memmove(buffer, (size_t) tOffset + (Byte1 *) phyBuffer, 
		    (size_t) (Var->NvalueBytes * nValues));
	  }
	}
	else {
	  tOffset = offset + (indices[firstDim] * nPhyDimBytes);
	  tBuffer = buffer;
	  for (i = 0; i < counts[firstDim]; i++) {
	     if (fullPhyDim[nextDim]) {
  	       if (phyBuffer == NULL) {
		 if (!sX(ReadVarValues(CDF,Var,recNum,
				       tOffset,nPhyDimValues[firstDim],
				       tBuffer),&pStatus)) return pStatus; 
	       } else {
		 memmove(tBuffer, (size_t) tOffset + (Byte1 *) phyBuffer, 
			 (size_t) (Var->NvalueBytes * nPhyDimValues[firstDim]));
	       }
	     }
	     else {
	       int numDimsT = (int) (numDims - 1);
	       int firstDimT = (CDF->rowMajor ? 0 : numDimsT - 1);
	       int passDimT = (CDF->rowMajor ? 1 : 0);
	       if (!sX(HyperReadDim(numDimsT,&dimSizes[passDimT],
				    &dimVarys[passDimT],
				    &indices[passDimT],
				    &counts[passDimT],
				    &intervals[passDimT],
				    &nHypDimValues[passDimT],
				    &nPhyDimValues[passDimT],
				    &fullPhyDim[passDimT],firstDimT,
				    dimIncr,recNum,tOffset,tBuffer,phyBuffer,
				    CDF,Var),&pStatus)) return pStatus;
	     }
	     tOffset += (intervals[firstDim] * nPhyDimBytes);
	     tBuffer += (size_t) nHypDimBytes;
	  }
	}
      }
      else {
	/**********************************************************************
	* The first dimension's variance is FALSE, physically read the only
	* existing subarray and generate virtual subarrays if necessary.
	**********************************************************************/
	if (fullPhyDim[nextDim]) {
  	  if (phyBuffer == NULL) {
	    if (!sX(ReadVarValues(CDF,Var,recNum,offset,
				  nPhyDimValues[firstDim],
				  buffer),&pStatus)) return pStatus;
	  } else {
 
	    memmove(buffer, (size_t) offset + (Byte1 *) phyBuffer, 
		    (size_t) (Var->NvalueBytes * nPhyDimValues[firstDim]));
	  }
	}
	else {
	  int numDimsT = (int) (numDims - 1);
	  int firstDimT = (CDF->rowMajor ? 0 : numDimsT - 1);
	  int passDimT = (CDF->rowMajor ? 1 : 0);
	  if (!sX(HyperReadDim(numDimsT,&dimSizes[passDimT],
			       &dimVarys[passDimT],&indices[passDimT],
			       &counts[passDimT],&intervals[passDimT],
			       &nHypDimValues[passDimT],
			       &nPhyDimValues[passDimT],
			       &fullPhyDim[passDimT],firstDimT,
			       dimIncr,recNum,offset,buffer,phyBuffer,
		 	       CDF,Var),&pStatus)) return pStatus;
	}
	if (counts[firstDim] > 1) {
	  for (i = 1, tBuffer = (Byte1 *) buffer + (size_t) nHypDimBytes;
	       i < counts[firstDim]; i++, tBuffer += (size_t) nHypDimBytes) {
	     memmove (tBuffer, buffer, (size_t) nHypDimBytes);
	  }
	}
      }
      break;
    }
  }
  return pStatus;
}

/******************************************************************************
* HyperWrite.
******************************************************************************/

STATICforIDL CDFstatus HyperWrite (CDF, Var, rd, buffer)
struct CDFstruct *CDF;
struct VarStruct *Var;
struct rdSTRUCT *rd;
void *buffer;
{
  Int32 nHypRecValues, nHypRecBytes, nHypDimValues[CDF_MAX_DIMS];
  Logical fullPhyRec, fullPhyDim[CDF_MAX_DIMS];
  Int32 firstPhyRec, phyRecN, nValues; Byte1 *tBuffer; int dimN; Int32 i;
  CDFstatus pStatus = CDF_OK; Byte1 *phyBuffer;
  int firstDim;                 /* Based on majority, the "highest" dimension
				   (changes the slowest in memory/on disk). */
  int doneDim;                  /* When cycling through the dimensions, the
				   dimension at which to stop (the dimension
				   beyond the last). */
  int dimIncr;                  /* How to cycle through the dimensions.  Incr-
				   ementing (+1) for row-major, decrementing
				   (-1) for column-major. */
  /****************************************************************************
  * Determine dimension ordering.
  ****************************************************************************/
  if (Var->numDims > 0) {
    firstDim = (int) (CDF->rowMajor ? 0 : Var->numDims - 1);
    doneDim = (int) (CDF->rowMajor ? Var->numDims : -1);
    dimIncr = (CDF->rowMajor ? 1 : -1);
  }
  /****************************************************************************
  * Determine if full physical dimensions can be written.  In order for this to
  * be so for a particular dimension...
  *   1. If the dimension variance is TRUE, then the entire dimension must be
  *      written (the count equal to the size).  If the dimension variance is
  *      FALSE, the count must be equal to one (only writing the single
  *      physical value).
  *   2. For each `lower' dimension having a TRUE variance, the entire
  *      dimension must be written (the count equal to the size).
  *   3. For each `lower' dimension having a FALSE variance, the count must
  *      be equal to one (only writing the single physical dimension).
  * Also determine if full physical records can be written.  If there are one
  * or more dimensions, this depends on whether or not the first dimension can
  * be written with a single physical write.  If there are zero dimensions,
  * then this is always true.
  ****************************************************************************/
  if (Var->numDims > 0) {
    for (dimN = firstDim; dimN != doneDim; dimN += dimIncr) {
       int dimNt;
       fullPhyDim[dimN] = TRUE;
       for (dimNt = dimN; dimNt != doneDim; dimNt += dimIncr)
	  if ((Var->dimVarys[dimNt] &&
	       rd->dimCounts[dimNt] != Var->dimSizes[dimNt]) ||
	      (!Var->dimVarys[dimNt] && rd->dimCounts[dimNt] > 1)) {
	    fullPhyDim[dimN] = FALSE;
	    break;
	  }
    }
    fullPhyRec = fullPhyDim[firstDim];
  }
  else
    fullPhyRec = TRUE;
  /****************************************************************************
  * Determine if only one write is needed.  In order for this to be so...
  *   1. Full physical records must be being written.
  *   2. A record variance of FALSE, or if the record variance is TRUE, then
  *      the record interval must be one (or a record count of one) so that
  *      there is no skipping of records.
  ****************************************************************************/
  phyRecN = 0;
  if (fullPhyRec &&
      ((Var->recVary && (rd->recInterval == 1 || rd->recCount == 1)) ||
       (!Var->recVary))) {
    if (Var->recVary) {
      phyRecN = rd->recNumber;
      tBuffer = buffer;
      nValues = rd->recCount * Var->NphyRecValues;
    }
    else {
      tBuffer = (Byte1 *) buffer +
		(size_t) (Var->NphyRecBytes * (rd->recCount-1));
      nValues = Var->NphyRecValues;
    }
    if (!sX(WriteVarValues(CDF,Var,phyRecN,INT32_ZERO,
			   nValues,tBuffer),&pStatus)) return pStatus;
    return pStatus;
  }
  /****************************************************************************
  * Only one write not possible - will have to write one record at a time.
  * First calculate size of hyper records.
  ****************************************************************************/
  if (Var->numDims > 0) {
    for (dimN = firstDim; dimN != doneDim; dimN += dimIncr) {
       int dimNt;
       nHypDimValues[dimN] = 1;
       for (dimNt = dimN + dimIncr; dimNt != doneDim; dimNt += dimIncr)
	  nHypDimValues[dimN] *= rd->dimCounts[dimNt];
    }
    nHypRecValues = nHypDimValues[firstDim] * rd->dimCounts[firstDim];
  }
  else
    nHypRecValues = 1;
  nHypRecBytes = nHypRecValues * Var->NvalueBytes;
  firstPhyRec = BOO(Var->recVary,rd->recNumber,0);
  /****************************************************************************
  * Write each record.
  ****************************************************************************/
  if (Var->recVary) {
    /**************************************************************************
    * A TRUE record variance - write each physical record.
    **************************************************************************/
    for (i = 0, phyRecN = firstPhyRec, tBuffer = buffer;
	 i < rd->recCount;
	 i++, phyRecN += rd->recInterval, tBuffer += (size_t) nHypRecBytes) {
       if (fullPhyRec) {
	 /*********************************************************************
	 * The full physical record is being written, use one physical write.
	 *********************************************************************/
	 if (!sX(WriteVarValues(CDF,Var,phyRecN,INT32_ZERO,
				Var->NphyRecValues,
				tBuffer),&pStatus)) return pStatus;
       }
       else {
	 /*********************************************************************
	 * Less than the full physical record is to be written.
         * physical record buffer if possible and fill in the whole record
         * with values from the buffer. Otherwise, write one value at a
         * time to the file
         *********************************************************************/
         phyBuffer = (Byte1 *) cdf_AllocateMemory((size_t) Var->NphyRecBytes,NULL);
	 if (phyBuffer != NULL) {
	    if (!sX(ReadVarValues(CDF,Var,phyRecN,INT32_ZERO,
                                 Var->NphyRecValues,
                                 phyBuffer),&pStatus)) {
              if (pStatus != VIRTUAL_RECORD_DATA) {
		cdf_FreeMemory(phyBuffer,NULL);
		return pStatus;
	      }
	    }
	 }
	 if (pStatus == VIRTUAL_RECORD_DATA) /* Don't want it to stop. */
	   pStatus = CDF_OK;
	 if (!sX(HyperWriteDim(Var->numDims,Var->dimSizes,
			       Var->dimVarys,rd->dimIndices,
			       rd->dimCounts,rd->dimIntervals,
			       nHypDimValues,Var->nPhyDimValues,
			       fullPhyDim,firstDim,
			       dimIncr,phyRecN,INT32_ZERO,tBuffer,phyBuffer,
			       CDF,Var),&pStatus)) {
           if (phyBuffer != NULL) {
	     cdf_FreeMemory(phyBuffer,NULL);
	     return pStatus;
	   }
         }
         if (phyBuffer != NULL) {
           if (!sX(WriteVarValues(CDF,Var,phyRecN,INT32_ZERO,
                                  Var->NphyRecValues,phyBuffer),&pStatus)) {
	     cdf_FreeMemory(phyBuffer,NULL);
	     return pStatus;
	   }
	   cdf_FreeMemory(phyBuffer,NULL);
	 }
       }
    }
  }
  else {
    /**************************************************************************
    * A FALSE record variance.  Only one physical record to actually be
    * written.  No need to check if a full physical record can be written
    * since that case would have been handled above (because of the FALSE
    * record variance).  If the record count is greater than one, the LAST
    * record in the buffer will be written (as if the records before it had
    * been written but then overwritten by the last one).
    **************************************************************************/
    tBuffer = (Byte1 *) buffer + (size_t) (nHypRecBytes * (rd->recCount - 1));
    phyBuffer = (Byte1 *) cdf_AllocateMemory((size_t) Var->NphyRecBytes,NULL);
    if (phyBuffer != NULL) {
       if (!sX(ReadVarValues(CDF,Var,phyRecN,INT32_ZERO,
                             Var->NphyRecValues, 
                             phyBuffer),&pStatus)) {
	 if (pStatus != VIRTUAL_RECORD_DATA) {
	   cdf_FreeMemory(phyBuffer,NULL);
	   return pStatus;
	 }
       } 
    }
    if (pStatus == VIRTUAL_RECORD_DATA) /* Don't want it to stop */
      pStatus = CDF_OK;
    if (!sX(HyperWriteDim(Var->numDims,Var->dimSizes,Var->dimVarys,
			  rd->dimIndices,rd->dimCounts,
			  rd->dimIntervals,nHypDimValues,
			  Var->nPhyDimValues,fullPhyDim,
			  firstDim,dimIncr,
			  INT32_ZERO,INT32_ZERO,tBuffer,phyBuffer,
			  CDF,Var),&pStatus)) {
      if (phyBuffer != NULL) {
 	cdf_FreeMemory(phyBuffer,NULL);
	return pStatus;
      }
    }
    if (phyBuffer != NULL) {
      if (!sX(WriteVarValues(CDF,Var,phyRecN,INT32_ZERO,
			     Var->NphyRecValues,phyBuffer),&pStatus)) {
	cdf_FreeMemory(phyBuffer,NULL);
        return pStatus;
      }
      cdf_FreeMemory(phyBuffer,NULL);
    }
  }
  return pStatus;
}

/******************************************************************************
* HyperWriteDim.  DANGER, DANGER, I'm recursive.
******************************************************************************/

STATICforIDL CDFstatus HyperWriteDim (numDims, dimSizes, dimVarys, indices,
				      counts, intervals, nHypDimValues,
				      nPhyDimValues, fullPhyDim, firstDim,
				      dimIncr, recNum, offset, buffer, phyBuffer,
				      CDF, Var)
Int32 numDims;
Int32 *dimSizes;
Int32 *dimVarys;
Int32 *indices;
Int32 *counts;
Int32 *intervals;
Int32 *nHypDimValues;
Int32 *nPhyDimValues;
Logical *fullPhyDim;
int firstDim;
int dimIncr;
Int32 recNum;           /* Record number for this write. */
Int32 offset;           /* Byte offset within record at which to begin. */
void *buffer;
void *phyBuffer;
struct CDFstruct *CDF;
struct VarStruct *Var;
{
  Int32 tOffset; Int32 i, nValues; Byte1 *tBuffer;
  CDFstatus pStatus = CDF_OK;
  /****************************************************************************
  * What to do depends on the number of dimensions.  Note that this function
  * should never be called with zero dimensions.
  ****************************************************************************/
  switch (numDims) {
    case 1: {
      /************************************************************************
      * One dimension - write the values along the dimension.
      ************************************************************************/
      if (dimVarys[0]) {
	/**********************************************************************
	* Dimension variance of TRUE, there are no virtual values to deal
	* with (skip) - read physical values.
	**********************************************************************/
	if (intervals[0] == 1) {
	  /********************************************************************
	  * A contiguous strip of values.
	  ********************************************************************/
	  tOffset = offset + (indices[0] * Var->NvalueBytes);
	  if (phyBuffer == NULL) {
	    if (!sX(WriteVarValues(CDF,Var,recNum,
				   tOffset,counts[0],
				   buffer),&pStatus)) return pStatus;
	    } else {
	      memmove ((size_t) tOffset + (Byte1 *) phyBuffer, buffer,
                       (size_t) (Var->NvalueBytes * counts[0]));
          }
	}
	else {
	  /********************************************************************
	  * Not contiguous, write one value at a time skipping over physical
	  * values not being written.
	  ********************************************************************/
	  tOffset = offset + (indices[0] * Var->NvalueBytes);
	  tBuffer = buffer; 
	  for (i = 0; i < counts[0]; i++) {
	     if (phyBuffer == NULL) {
	       if (!sX(WriteVarValues(CDF,Var,recNum,
				      tOffset,INT32_ONE,
				      tBuffer),&pStatus)) return pStatus;
	     } else {
               memmove ((size_t) tOffset+(Byte1 *) phyBuffer, tBuffer,
                        (size_t) Var->NvalueBytes);
             }
	     tOffset += (intervals[0] * Var->NvalueBytes);
	     tBuffer += (size_t) Var->NvalueBytes;
	  }
	}
      }
      else {
	/**********************************************************************
	* Dimension variance of FALSE, only one physical value to be written.
	* If the count is greater than one, skip to the last value in the
	* buffer (this is an unlikely situation).
	**********************************************************************/
	tBuffer = (Byte1 *) buffer +
		  (size_t) (Var->NvalueBytes * (counts[0] - 1));
	if (phyBuffer == NULL) {
	  if (!sX(WriteVarValues(CDF,Var,recNum,offset,
				 INT32_ONE,tBuffer),&pStatus)) return pStatus;
        } else {
          memmove((size_t) offset + (Byte1 *) phyBuffer, tBuffer,
                  (size_t) Var->NvalueBytes);
        }
      }
      break;
    }
    default: {
      /************************************************************************
      * Two or more dimensions.
      ************************************************************************/
      Int32 nPhyDimBytes = nPhyDimValues[firstDim] * Var->NvalueBytes;
      Int32 nHypDimBytes = nHypDimValues[firstDim] * Var->NvalueBytes;
      int nextDim = firstDim + dimIncr;
      if (dimVarys[firstDim]) {
	/**********************************************************************
	* The first dimension's variance is TRUE.  If the interval is one and
	* only a single write is necessary for the "lower" dimension, use a
	* single write.  Otherwise, cycle through that dimension physically
	* writing each subarray below it (skipping subarrays if necessary).
	**********************************************************************/
	if (intervals[firstDim] == 1 && fullPhyDim[nextDim]) {
	  tOffset = offset + (indices[firstDim] * nPhyDimBytes);
	  nValues = counts[firstDim] * nPhyDimValues[firstDim];
	  if (phyBuffer == NULL) {
	    if (!sX(WriteVarValues(CDF,Var,recNum,
				   tOffset,nValues,
				   buffer),&pStatus)) return pStatus;
          } else {
            memmove((size_t) tOffset + (Byte1 *) phyBuffer, buffer, 
                    (size_t) (Var->NvalueBytes * nValues));
          }
	}
	else {
	  tOffset = offset + (indices[firstDim] * nPhyDimBytes);
	  tBuffer = buffer;
	  for (i = 0; i < counts[firstDim]; i++) {
	     if (fullPhyDim[nextDim]) {
	       if (phyBuffer == NULL) {
		 if (!sX(WriteVarValues(CDF,Var,recNum,tOffset,
				        nPhyDimValues[firstDim],
				        tBuffer),&pStatus)) return pStatus;
               } else {
                 memmove((size_t) tOffset + (Byte1 *) phyBuffer, tBuffer,
                         (size_t) (Var->NvalueBytes * nPhyDimValues[firstDim]));
               }
	     }
	     else {
	       int numDimsT = (int) (numDims - 1);
	       int firstDimT = (CDF->rowMajor ? 0 : numDimsT - 1);
	       int passDimT = (CDF->rowMajor ? 1 : 0);
	       if (!sX(HyperWriteDim(numDimsT,&dimSizes[passDimT],
				     &dimVarys[passDimT],
				     &indices[passDimT],
				     &counts[passDimT],
				     &intervals[passDimT],
				     &nHypDimValues[passDimT],
				     &nPhyDimValues[passDimT],
				     &fullPhyDim[passDimT],
				     firstDimT,dimIncr,recNum,
				     tOffset,tBuffer,phyBuffer,
				     CDF,Var),&pStatus)) return pStatus;
	     }
	     tOffset += (intervals[firstDim] * nPhyDimBytes);
	     tBuffer += (size_t) nHypDimBytes;
	  }
	}
      }
      else {
	/**********************************************************************
	* The first dimension's variance is FALSE, skip to the last subarray
	* and write the single physical subarray.
	**********************************************************************/
	tBuffer = (Byte1 *) buffer +
		  (size_t) (nHypDimBytes * (counts[firstDim] - 1));
	if (fullPhyDim[nextDim]) {
	  if (phyBuffer == NULL) {
	    if (!sX(WriteVarValues(CDF,Var,recNum,offset,
				   nPhyDimValues[firstDim],
				   tBuffer),&pStatus)) return pStatus;
	  } else {
            memmove((size_t) offset + (Byte1 *) phyBuffer, tBuffer,
                    (size_t) (Var->NvalueBytes * nPhyDimValues[firstDim]));
          }
	}
	else {
	  int numDimsT = (int) (numDims - 1);
	  int firstDimT = (CDF->rowMajor ? 0 : numDimsT - 1);
	  int passDimT = (CDF->rowMajor ? 1 : 0);
	  if (!sX(HyperWriteDim(numDimsT,&dimSizes[passDimT],
				&dimVarys[passDimT],
				&indices[passDimT],
				&counts[passDimT],
				&intervals[passDimT],
				&nHypDimValues[passDimT],
				&nPhyDimValues[passDimT],
				&fullPhyDim[passDimT],firstDimT,
				dimIncr,recNum,offset,tBuffer,phyBuffer,
				CDF,Var),&pStatus)) return pStatus;
	}
      }
      break;
    }
  }

  return pStatus;
}
