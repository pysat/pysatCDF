/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                     CDF library miscellaneous functions, part 3.
*
*  Version 1.0e, 29-Oct-97, Hughes STX.
*
*  Modification history:
*
*   V1.0   5-Sep-96, J Love     Original version.
*   V1.0a 21-Feb-97, J Love	Removed RICE.
*   V1.0b  4-Mar-97, J Love	Windows NT for MS Visual C/C++ on an IBM PC.
*   V1.0c 11-Sep-97, J Love	Magic number are now uInt32.
*   V1.0d 20-Oct-97, J Love	Properly cast the uInt32 magic numbers.  More
*				Windows NT.
*   V3.2  25-Apr-07, D Berger   Changed COPYblockBYTES from 512.
*   V3.3  24-Nov-10, M Liu      Changed ScratchDirectory to make Windows use
*                               predefined env. variable "TMP" if "CDF_TMP"
*                               is not set. 
*
******************************************************************************/

#include "cdflib.h"

/******************************************************************************
* Macros/function prototypes.
******************************************************************************/

#define COPYblockSIZE nCACHE_BUFFER_BYTEs	

static CDFstatus SearchForRecord_r PROTOARGs((
  vFILE *fp, Int32 vxrOffset, Int32 recNum, Int32 *firstRec, Int32 *lastRec,
  Int32 *offset, Logical *found
));
static CDFstatus IndexingStatistics_r PROTOARGs((
  vFILE *fp, Int32 vxrOffset, Int32 maxRec, int level, Int32 *nLevels,
  Int32 *nVXRs, Int32 *nEntries, Int32 *nAlloc, Int32 *nRecords
));
static CDFstatus PrevRecord_r PROTOARGs((
  vFILE *fp, Int32 vxrOffset, Int32 baseRec, Int32 *prevRec, Logical *found
));
static CDFstatus NextRecord_r PROTOARGs((
  vFILE *fp, Int32 vxrOffset, Int32 baseRec, Int32 *nextRec, Logical *found
));
static CDFstatus CalcCompressionPct_r PROTOARGs((
  vFILE *fp, Int32 vxrOffset, Int32 nPhyRecBytes, Int32 *uTotal, Int32 *cTotal
));
static CDFstatus ModIndexOffset_r PROTOARGs((
  vFILE *fp, Int32 vxrOffset, Int32 firstRec, Int32 lastRec, Int32 newOffset
));
static CDFstatus ReadSparseFull PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 firstRec, Int32 lastRec,
  void *buffer
));
static CDFstatus ReadSparsePartial PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 recNum, Int32 offset,
  Int32 nValues, void *buffer
));
static CDFstatus ReadCompressedFull PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 firstRec, Int32 lastRec,
  void *buffer
));
static CDFstatus ReadCompressedPartial PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 recNum, Int32 offset,
  Int32 nValues, void *buffer
));
static CDFstatus BringToStage PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 recNum, Logical *found
));
static CDFstatus WriteCompressedRecords PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 firstRec, Int32 lastRec,
  void *buffer, Int32 nValues, Int32 offset, Logical fullRecord
));

/******************************************************************************
* DecompressCDF.
******************************************************************************/

STATICforIDL CDFstatus DecompressCDF (dotFp, uDotFp)
vFILE *dotFp;           /* In: File pointer to dotCDF file. */
vFILE *uDotFp;          /* In: Uncompressed CDF file pointer. */
{
  CDFstatus pStatus = CDF_OK; struct CCRstruct CCR; struct CPRstruct CPR;
  uInt32 magicNumber1 = V2magicNUMBER_1, magicNumber2u = V2magicNUMBER_2u;
  Int32 cSize, cOffset;
  /****************************************************************************
  * Read/validate CCR.
  ****************************************************************************/
  if (!sX(ReadCCR(dotFp,V2_CCR_OFFSET,
		  CCR_RECORD,&CCR,
		  CCR_NULL),&pStatus)) return pStatus;
  if (CCR.uSize == 0) return EMPTY_COMPRESSED_CDF;
  /****************************************************************************
  * Read CPR.
  ****************************************************************************/
  if (!sX(ReadCPR(dotFp,CCR.CPRoffset,
		  CPR_RECORD,&CPR,
		  CPR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * Write magic numbers.
  ****************************************************************************/
  if (!SEEKv(uDotFp,V2_MAGIC_OFFSET_1,vSEEK_SET)) return CDF_WRITE_ERROR;
  if (!Write32(uDotFp,(Int32 *)&magicNumber1)) return CDF_WRITE_ERROR;
  if (!Write32(uDotFp,(Int32 *)&magicNumber2u)) return CDF_WRITE_ERROR;
  /****************************************************************************
  * Copy/decompress.
  ****************************************************************************/
  cOffset = V2_CCR_OFFSET + CCR_BASE_SIZE;
  cSize = CCR.RecordSize - CCR_BASE_SIZE;
  if (!sX(Decompress(dotFp,cOffset,
		     cSize,CDF_READ_ERROR,
		     CPR.cType,CPR.cParms,
		     uDotFp,FIRST_IR_OFFSET,
		     CDF_WRITE_ERROR),&pStatus)) return pStatus;
  return pStatus;
}

/******************************************************************************
* WriteCompressedCDF.
******************************************************************************/

STATICforIDL CDFstatus WriteCompressedCDF (CDF, CPR, empty)
struct CDFstruct *CDF;
struct CPRstruct *CPR;
Logical empty;          /* In: If TRUE, write an empty CCR. */
{
  uInt32 magicNumber1 = V2magicNUMBER_1;
  uInt32 magicNumber2c = V2magicNUMBER_2c;
  struct CCRstruct CCR; CDFstatus pStatus = CDF_OK;
  /****************************************************************************
  * Write magic numbers.
  ****************************************************************************/
  if (!SEEKv(CDF->dotFp,V2_MAGIC_OFFSET_1,vSEEK_SET)) return CDF_WRITE_ERROR;
  if (!Write32(CDF->dotFp,(Int32 *)&magicNumber1)) return CDF_WRITE_ERROR;
  if (!Write32(CDF->dotFp,(Int32 *)&magicNumber2c)) return CDF_WRITE_ERROR;
  /****************************************************************************
  * Write CCR.
  ****************************************************************************/
  if (empty) {
    CCR.RecordSize = CCR_BASE_SIZE;
    CCR.RecordType = CCR_;
    CCR.CPRoffset = V2_CCR_OFFSET + CCR.RecordSize;
    CCR.uSize = 0;
    CCR.rfuA = 0;
    if (!sX(WriteCCR(CDF->dotFp,V2_CCR_OFFSET,
		     CCR_RECORD,&CCR,
		     CCR_NULL),&pStatus)) return CDF_WRITE_ERROR;
  }
  else {
    Int32 uSize, eof, cSize, cOffset, GDRoffset;
    if (!sX(ReadCDR(CDF->uDotFp,V2_CDR_OFFSET,
		    CDR_GDROFFSET,&GDRoffset,
		    CDR_NULL),&pStatus)) return pStatus;
    if (!sX(ReadGDR(CDF->uDotFp,GDRoffset,
		    GDR_EOF,&eof,
		    GDR_NULL),&pStatus)) return pStatus;
    uSize = eof - FIRST_IR_OFFSET;
    cOffset = V2_CCR_OFFSET + CCR_BASE_SIZE;
    if (!sX(Compress(CDF->uDotFp,FIRST_IR_OFFSET,
		     uSize,CDF_READ_ERROR,CPR->cType,
		     CPR->cParms,CDF->dotFp,cOffset,
		     &cSize,CDF_WRITE_ERROR),&pStatus)) return pStatus;
    CCR.RecordSize = CCR_BASE_SIZE + cSize;
    CCR.RecordType = CCR_;
    CCR.CPRoffset = V2_CCR_OFFSET + CCR.RecordSize;
    CCR.uSize = uSize;
    CCR.rfuA = 0;
    if (!sX(WriteCCR(CDF->dotFp,V2_CCR_OFFSET,
		     CCR_RECORD,&CCR,
		     CCR_NULL),&pStatus)) return CDF_WRITE_ERROR;
  }
  /****************************************************************************
  * Write CPR.
  ****************************************************************************/
  if (!sX(WriteCPR(CDF->dotFp,CCR.CPRoffset,
		   CPR_RECORD,CPR,
		   CPR_NULL),&pStatus)) return CDF_WRITE_ERROR;
  return pStatus;
}

/******************************************************************************
* CopyCDF.
******************************************************************************/

STATICforIDL CDFstatus CopyCDF (srcFp, destFp)
vFILE *srcFp;
vFILE *destFp;
{
  Int32 nBytes, offset; Byte1 buffer[nCACHE_BUFFER_BYTEs];
  CDFstatus pStatus = CDF_OK;
  if (!SEEKv(srcFp,0L,vSEEK_END)) return CDF_READ_ERROR;
  nBytes = V_tell (srcFp);
  if (nBytes == EOF) return CDF_READ_ERROR;
  if (!SEEKv(srcFp,0L,vSEEK_SET)) return CDF_READ_ERROR;
  if (!SEEKv(destFp,0L,vSEEK_SET)) return CDF_WRITE_ERROR;
  for (offset = 0; offset < nBytes; offset += nCACHE_BUFFER_BYTEs) {
     Int32 nBytesRemaining = nBytes - offset;
     size_t count = (size_t) MINIMUM (nBytesRemaining, nCACHE_BUFFER_BYTEs);
     if (!READv(buffer,count,1,srcFp)) return CDF_READ_ERROR;
     if (!WRITEv(buffer,count,1,destFp)) return CDF_WRITE_ERROR;
  }
  return pStatus;
}

/******************************************************************************
* WriteVarValues.
*    NOTE: If more than one record is being written, full records are assumed
* and the `offset' must be zero.
******************************************************************************/

STATICforIDL CDFstatus WriteVarValues (CDF, Var, startRec, offset, nValues,
				       buffer)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 startRec;          /* Physical record number at which to write. */
Int32 offset;            /* Byte offset within (first) record at which to
			    begin writing. */
Int32 nValues;           /* Number of values to write. */
void *buffer;
{
  Int32 tOffset; Logical fullRecord; Byte1 *tBuffer = buffer;
  Int32 numElems, firstRec, lastRec, lastRecInVVR, nBytes, padTo, recNum;
  Int32 writeTo, recCount; CDFstatus pStatus = CDF_OK;
  /****************************************************************************
  * Determine first/last record(s) being written and if full physical
  * record(s).
  ****************************************************************************/
  firstRec = startRec;
  if (nValues < Var->NphyRecValues) {
    fullRecord = FALSE;
    lastRec = startRec;
  }
  else {
    fullRecord = TRUE;
    lastRec = startRec + ((nValues - 1) / Var->NphyRecValues);
  }
  /****************************************************************************
  * Based on variable type...
  ****************************************************************************/
  switch (Var->vType) {
    /**************************************************************************
    * Standard variable in single-file CDF...
    **************************************************************************/
    case STANDARD_: {
      /************************************************************************
      * Allocate/pad records.
      ************************************************************************/
      if (lastRec > Var->maxAllocated) {
	struct AllocStruct alloc;
	Int32 nNeeded = lastRec - Var->maxAllocated;
	Int32 nRecords = MAXIMUM(nNeeded,Var->blockingFactor);
	LoadAllocVVR (alloc, Var->maxAllocated + 1,
		      Var->maxAllocated + nRecords, FALSE)
	if (!sX(AllocateRecords(CDF,Var,alloc),&pStatus)) return pStatus;
	Var->maxAllocated = alloc.last;
      }
      padTo = BOO(fullRecord,firstRec - 1,firstRec);
      if (padTo > Var->maxWritten) {
	Int32 padFrom = Var->maxWritten + 1;
	if (!sX(PadUnRecords(CDF,Var,padFrom,padTo),&pStatus)) return pStatus;
	Var->maxWritten = padTo;
      }
      /************************************************************************
      * Write value(s).
      ************************************************************************/
      if (fullRecord) {
	recNum = firstRec;
	while (recNum <= lastRec) {
	  if (!sX(SearchForRecord(CDF,Var->VDRoffset,
				  Var->zVar,recNum,
				  NULL,&lastRecInVVR,
				  NULL,NULL),&pStatus)) return pStatus;
	  writeTo = MINIMUM(lastRec,lastRecInVVR);
	  recCount = writeTo - recNum + 1;
	  if (!sX(RecordByteOffset(CDF,Var,
				   recNum,
				   &tOffset),&pStatus)) return pStatus;
	  numElems = recCount * Var->NphyRecElems;
	  if (!sX(WriteVarElems(Var,CDF->fp,tOffset,
				numElems,tBuffer),&pStatus)) return pStatus;
	  recNum += recCount;
	  tBuffer += (size_t) (recCount * Var->NphyRecBytes);
	}
      }
      else {
	if (!sX(RecordByteOffset(CDF,Var,
				 firstRec,
				 &tOffset),&pStatus)) return pStatus;
	tOffset += (Int32) offset;
	numElems = nValues * Var->NvalueElems;
	if (!sX(WriteVarElems(Var,CDF->fp,tOffset,
			      numElems,buffer),&pStatus)) return pStatus;
      }
      /************************************************************************
      * Update the maximum record written.
      ************************************************************************/
      Var->maxWritten = MAXIMUM(Var->maxWritten,lastRec);
      break;
    }
    /**************************************************************************
    * Sparse records...
    **************************************************************************/
    case SPARSE_RECORDS_: {
      Int32 maxRecInStage, recordOffsetInStage, nextRec;
      int how; void *padBuffer; Logical found;
      /************************************************************************
      * Pad records.
      ************************************************************************/
      padTo = BOO(fullRecord,firstRec - 1,firstRec);
      if (padTo > Var->maxWritten) {
	Int32 padFrom = Var->maxWritten + 1;
	if (!sX(PadUnRecords(CDF,Var,padFrom,padTo),&pStatus)) return pStatus;
	Var->maxWritten = padTo;
      }
      /************************************************************************
      * Write value(s).
      ************************************************************************/
      recNum = firstRec;
      while (recNum <= lastRec) {
	/**********************************************************************
	* Check if this record already exists (is allocated).
	**********************************************************************/
	if (!sX(SearchForRecord(CDF,Var->VDRoffset,
				Var->zVar,recNum,
				NULL,&lastRecInVVR,
				NULL,&found),&pStatus)) return pStatus;
	if (found) {
	  writeTo = MINIMUM(lastRec,lastRecInVVR);
	  recCount = writeTo - recNum + 1;
	  if (!sX(RecordByteOffset(CDF,Var,
				   recNum,&tOffset),&pStatus)) return pStatus;
	  if (fullRecord)
	    numElems = recCount * Var->NphyRecElems;
	  else {
	    tOffset += offset;
	    numElems = nValues * Var->NvalueElems;
	  }
	  if (!sX(WriteVarElems(Var,CDF->fp,tOffset,
				numElems,tBuffer),&pStatus)) return pStatus;
	  Var->maxWritten = MAXIMUM(Var->maxWritten,writeTo);
	  recNum += recCount;
	  tBuffer += (size_t) (numElems * Var->NelemBytes);
	  continue;
	}
	/**********************************************************************
	* This record doesn't exist - initialize the staging area.
	**********************************************************************/
	if (Var->stage.areaOffset == NO_OFFSET) {
	  nBytes = Var->blockingFactor * Var->NphyRecBytes;
	  if (!sX(InitVarStage(CDF,Var,nBytes),&pStatus)) return pStatus;
	}
	/**********************************************************************
	* Check if this record is in the staging area.
	**********************************************************************/
	if (INCLUSIVE(Var->stage.firstRec,recNum,Var->stage.lastRec)) {
	  maxRecInStage = Var->stage.firstRec + Var->blockingFactor - 1;
	  writeTo = MINIMUM(lastRec,maxRecInStage);
	  if (!sX(NextRecord(CDF,Var->VDRoffset,Var->zVar,
			     recNum,&nextRec,&found),&pStatus)) return pStatus;
	  if (found) {
	    Int32 prevRecN = nextRec - 1;
	    writeTo = MINIMUM(writeTo,prevRecN);
	  }
	  recCount = writeTo - recNum + 1;
	  recordOffsetInStage = recNum - Var->stage.firstRec;
	  tOffset = Var->stage.areaOffset;
	  tOffset += (recordOffsetInStage * Var->NphyRecBytes);
	  if (fullRecord)
	    numElems = recCount * Var->NphyRecElems;
	  else {
	    tOffset += offset;
	    numElems = nValues * Var->NvalueElems;
	  }
	  if (!sX(WriteVarElems(Var,CDF->stage.fp,tOffset,
				numElems,tBuffer),&pStatus)) return pStatus;
	  Var->stage.lastRec = MAXIMUM(Var->stage.lastRec,writeTo);
	  Var->stage.modified = TRUE;
	  recNum += recCount;
	  tBuffer += (size_t) (numElems * Var->NelemBytes);
	  continue;
	}
	/**********************************************************************
	* This record is not in the staging area.  Check if it can be added.
	**********************************************************************/
	if (Var->stage.firstRec != NO_RECORD) {
	  if (recNum == Var->stage.lastRec + 1) {
	    maxRecInStage = Var->stage.firstRec + Var->blockingFactor - 1;
	    if (recNum <= maxRecInStage) {
	      writeTo = MINIMUM(lastRec,maxRecInStage);
	      if (!sX(NextRecord(CDF,Var->VDRoffset,
				 Var->zVar,recNum,
				 &nextRec,&found),&pStatus)) return pStatus;
	      if (found) {
		Int32 prevRecN = nextRec - 1;
		writeTo = MINIMUM(writeTo,prevRecN);
	      }
	      recCount = writeTo - recNum + 1;
	      recordOffsetInStage = recNum - Var->stage.firstRec;
	      tOffset = Var->stage.areaOffset;
	      tOffset += (recordOffsetInStage * Var->NphyRecBytes);
	      if (fullRecord)
		numElems = recCount * Var->NphyRecElems;
	      else {
		if (!sX(BuildPadBuffer(CDF,Var,recCount,
				       &how,&padBuffer,
				       TRUE),&pStatus)) return pStatus;
		if (!sX(WritePadValues(Var,CDF->stage.fp,tOffset,
				       recCount,how,padBuffer),&pStatus)) {
		  cdf_FreeMemory (padBuffer, NULL);
		  return pStatus;
		}
		cdf_FreeMemory (padBuffer, NULL);
		tOffset += offset;
		numElems = nValues * Var->NvalueElems;
	      }
	      if (!sX(WriteVarElems(Var,CDF->stage.fp,
				    tOffset,numElems,
				    tBuffer),&pStatus)) return pStatus;
	      Var->stage.lastRec = MAXIMUM(Var->stage.lastRec,writeTo);
	      Var->stage.modified = TRUE;
	      recNum += recCount;
	      tBuffer += (size_t) (numElems * Var->NelemBytes);
	      continue;
	    }
	  }
	}
	/**********************************************************************
	* This record cannot be added to the staging area.  First flush the
	* staging area (if necessary)...
	**********************************************************************/
	if (!sX(FlushStage(CDF,Var),&pStatus)) return pStatus;
	/**********************************************************************
	* ...and then start a new staging area with this record.
	**********************************************************************/
	maxRecInStage = recNum + Var->blockingFactor - 1;
	writeTo = MINIMUM(lastRec,maxRecInStage);
	if (!sX(NextRecord(CDF,Var->VDRoffset,Var->zVar,
			   recNum,&nextRec,&found),&pStatus)) return pStatus;
	if (found) {
	  Int32 prevRecN = nextRec - 1;
	  writeTo = MINIMUM(writeTo,prevRecN);
	}
	recCount = writeTo - recNum + 1;
	tOffset = Var->stage.areaOffset;
	if (fullRecord)
	  numElems = recCount * Var->NphyRecElems;
	else {
	  if (!sX(BuildPadBuffer(CDF,Var,recCount,
				 &how,&padBuffer,
				 TRUE),&pStatus)) return pStatus;
	  if (!sX(WritePadValues(Var,CDF->stage.fp,tOffset,
				 recCount,how,padBuffer),&pStatus)) {
	    cdf_FreeMemory (padBuffer, NULL);
	    return pStatus;
	  }
	  cdf_FreeMemory (padBuffer, NULL);
	  tOffset += offset;
	  numElems = nValues * Var->NvalueElems;
	}
	if (!sX(WriteVarElems(Var,CDF->stage.fp,tOffset,
			      numElems,tBuffer),&pStatus)) return pStatus;
	Var->stage.firstRec = recNum;
	Var->stage.lastRec = writeTo;
	Var->stage.modified = TRUE;
	recNum += recCount;
	tBuffer += (size_t) (numElems * Var->NelemBytes);
      }
      break;
    }
    /**************************************************************************
    * Compressed records...
    * Sparse/compressed records...
    **************************************************************************/
    case COMPRESSED_:
    case SPARSE_COMPRESSED_RECORDS_: {
      Logical sparseRecords = (Var->vType == SPARSE_COMPRESSED_RECORDS_);
      /************************************************************************
      * Initialize staging area.
      ************************************************************************/
      if (Var->stage.areaOffset == NO_OFFSET) {
	if (Var->blockingFactor == 0) {
	  if (Var->recVary) {
	    Int32 bf = ((MIN_BLOCKING_BYTES_compressed-1)/Var->NphyRecBytes)+1;
	    Var->blockingFactor = MAXIMUM(bf,MIN_BLOCKING_RECS_compressed);
	  }
	  else
	    Var->blockingFactor = 1;
	  if (!sX(WriteVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
			   VDR_BLOCKING,&(Var->blockingFactor),
			   VDR_NULL),&pStatus)) return pStatus;
	}
	nBytes = Var->blockingFactor * Var->NphyRecBytes;
	if (!sX(InitVarStage(CDF,Var,nBytes),&pStatus)) return pStatus;
      }
      /************************************************************************
      * Pad records (if necessary).
      ************************************************************************/
      if (!sparseRecords) {
	padTo = firstRec - 1;
	if (Var->maxRec < padTo) {
	  Int32 nRecords = padTo + 1, recN, valueN; void *padBuffer; int how;
	  if (!sX(BuildPadBuffer(CDF,Var,
				 nRecords,&how,
				 &padBuffer,FALSE),&pStatus)) return pStatus;
	  switch (how) {
	    case ALLrecordsATonce:
	      if (!sX(WriteCompressedRecords(CDF,Var,Var->maxRec+1,
					     padTo,padBuffer,
					     nRecords*Var->NphyRecValues,
					     ZERO_OFFSET,
					     TRUE),&pStatus)) {
		cdf_FreeMemory (padBuffer, NULL);
		return pStatus;
	      }
	      break;
	    case ONErecordATaTIME:
	      for (recN = Var->maxRec + 1; recN <= padTo; recN++) {
		 if (!sX(WriteCompressedRecords(CDF,Var,recN,recN,
						padBuffer,Var->NphyRecValues,
						ZERO_OFFSET,TRUE),&pStatus)) {
		   cdf_FreeMemory (padBuffer, NULL);
		   return pStatus;
		 }
	      }
	      break;
	    case ONEvalueATaTIME:
	      for (recN = 0; recN < nRecords; recN++) {
		 for (valueN = 0; valueN < Var->NphyRecValues; valueN++) {
		    tOffset = valueN * Var->NvalueBytes;
		    if (!sX(WriteCompressedRecords(CDF,Var,recN,recN,
						   padBuffer,INT32_ONE,
						   tOffset,FALSE),&pStatus)) {
		      cdf_FreeMemory (padBuffer, NULL);
		      return pStatus;
		    }
		 }
	      }
	      break;
	  }
	  cdf_FreeMemory (padBuffer, NULL);
	}
      }
      /************************************************************************
      * Write the record(s).
      ************************************************************************/
      if (!sX(WriteCompressedRecords(CDF,Var,firstRec,lastRec,
				     buffer,nValues,offset,
				     fullRecord),&pStatus)) return pStatus;
      break;
    }
    /**************************************************************************
    * Can't do sparse arrays yet...
    **************************************************************************/
    case SPARSE_ARRAYS_:
    case SPARSE_RECORDS_AND_ARRAYS_:
      return UNKNOWN_SPARSENESS;
    /**************************************************************************
    * Variable in multi-file CDF...
    **************************************************************************/
    case IN_MULTI_: {
      padTo = BOO(fullRecord,firstRec - 1,firstRec);
      /************************************************************************
      * Extend variable (if necessary).
      ************************************************************************/
      if (padTo > Var->maxRec) {
	Int32 padFrom = Var->maxRec + 1;
	if (!sX(PadUnRecords(CDF,Var,padFrom,padTo),&pStatus)) return pStatus;
      }
      /************************************************************************
      * Write value(s).
      ************************************************************************/
      if (!sX(RecordByteOffset(CDF,Var,
			       firstRec,
			       &tOffset),&pStatus)) return pStatus;
      tOffset += (Int32) offset;
      numElems = nValues * Var->NvalueElems;
      if (!sX(WriteVarElems(Var,Var->fp,tOffset,
			    numElems,buffer),&pStatus)) return pStatus;
      break;
    }
    /**************************************************************************
    * Unknown variable type - this should never happen.
    **************************************************************************/
    default:
      return CDF_INTERNAL_ERROR;
  }
  /****************************************************************************
  * Update maximum record numbers.
  ****************************************************************************/
  if (!sX(UpdateMaxRec(CDF,Var,lastRec),&pStatus)) return pStatus;
  return pStatus;
}

/******************************************************************************
* WriteVarElems.
*   NOTE: On IBM PCs, it is assumed that the number of bytes being written
* will not exceed 65535.
******************************************************************************/

STATICforIDL CDFstatus WriteVarElems (Var, fp, offset, numElems, buffer)
struct VarStruct *Var;
vFILE *fp;
Int32 offset;
Int32 numElems;
void *buffer;
{
  CDFstatus pStatus = CDF_OK; Int32 elemCount;
  /****************************************************************************
  * Seek to the desired offset.
  ****************************************************************************/
  if (!SEEKv(fp,(long)offset,vSEEK_SET)) return VAR_WRITE_ERROR;
  if (STRINGdataType(Var->dataType)) {
    FillSpacesToString((char *)buffer, (int) numElems,
                       (int) Var->NvalueElems);
  }
  /****************************************************************************
  * If no encoding is necessary, simply write the buffer and return.
  ****************************************************************************/
  if (Var->EncodeFunction == NULL) {
    Int32 nBytes = numElems * Var->NelemBytes;
    if (!WRITEv(buffer,1,(size_t)nBytes,fp)) return VAR_WRITE_ERROR;
    return pStatus;
  }
  /****************************************************************************
  * Use as large a temporary buffer as possible for the encoding conversion.
  * Start at the full number of elements and then halve that number until an
  * allocation succeeds.
  ****************************************************************************/
  elemCount = numElems;
  for (;;) {
     size_t nBytes = (size_t) (elemCount * Var->NelemBytes);
     void *tBuffer;
     if ((int) nBytes <= 0) return VAR_READ_ERROR;
     tBuffer = cdf_AllocateMemory (nBytes, NULL);
     if (tBuffer != NULL) {
       Int32 elemN = 0; Byte1 *bOffset = buffer;
       while (elemN < numElems) {
	 Int32 thisElemCount = MinInt32 (elemCount, numElems - elemN);
	 size_t thisByteCount = (size_t) (thisElemCount * Var->NelemBytes);
	 memmove (tBuffer, bOffset, thisByteCount);
	 if (!sX(Var->EncodeFunction(tBuffer,thisElemCount),&pStatus)) {
	   cdf_FreeMemory (tBuffer, NULL);
	   return pStatus;
	 }
	 if (!WRITEv(tBuffer,1,thisByteCount,fp)) {
	   cdf_FreeMemory (tBuffer, NULL);
	   return VAR_WRITE_ERROR;
	 }
	 elemN += thisElemCount;
	 bOffset += thisByteCount;
       }
       cdf_FreeMemory (tBuffer, NULL);
       return pStatus;
     }
     if (elemCount == 1) break;
     elemCount = (elemCount + 1) / 2;
  }
  return BAD_MALLOC;
}

/******************************************************************************
* PrevRecord.
*   Determine the last record allocated AT or BEFORE `baseRec'.  This routine
* should only be used for single-file CDFs.
******************************************************************************/

STATICforIDL CDFstatus PrevRecord (CDF, VDRoffset, zVar, baseRec, prevRec,
				   found)
struct CDFstruct *CDF;
Int32 VDRoffset;
Logical zVar;
Int32 baseRec;
Int32 *prevRec;
Logical *found;         /* If NULL, return NO_SUCH_RECORD if the previous
			   record doesn't exist.  Otherwise, set according to
			   whether or not the previous record exists and
			   return the pending status. */
{
  CDFstatus pStatus = CDF_OK; Int32 VXRoffset;
  /****************************************************************************
  * If multi-file...
  ****************************************************************************/
  if (!CDF->singleFile) return CDF_INTERNAL_ERROR;
  /****************************************************************************
  * Single-file...read the offset of the first VXR.
  ****************************************************************************/
  if (!sX(ReadVDR(CDF,CDF->fp,VDRoffset,zVar,
		  VDR_VXRHEAD,&VXRoffset,
		  VDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * ...and start there (unless there are no VXRs).
  ****************************************************************************/
  if (VXRoffset != ZERO_OFFSET) {
    if (!sX(PrevRecord_r(CDF->fp,VXRoffset,
			 baseRec,prevRec,found),&pStatus)) return pStatus;
  }
  else {
    ASSIGNnotNULL (found, FALSE)
    if (found == NULL) pStatus = NO_SUCH_RECORD;
  }
  return pStatus;
}

static CDFstatus PrevRecord_r (fp, vxrOffset, baseRec, prevRec, found)
vFILE *fp;
Int32 vxrOffset;
Int32 baseRec;
Int32 *prevRec;
Logical *found;
{
  CDFstatus pStatus = CDF_OK; int entryN = 0;
  struct VXRstruct VXR, nextVXR; Int32 irType;
  /****************************************************************************
  * Read the first VXR.
  ****************************************************************************/
  if (!sX(ReadVXR(fp,vxrOffset,
		  VXR_RECORD,&VXR,
		  VXR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * Check if there isn't a previous allocated record.  This could only be the
  * case at the top level of VXRs (ie. not when this routine is recursively
  * called).
  ****************************************************************************/
  if (baseRec < VXR.First[0]) {
    ASSIGNnotNULL (found, FALSE)
    return BOO(found == NULL,NO_SUCH_RECORD,pStatus);
  }
  /****************************************************************************
  * Until it's time to return...
  ****************************************************************************/
  for (;;) {
     /*************************************************************************
     * If the record is in the current entry...
     *************************************************************************/
     if (INCLUSIVE(VXR.First[entryN],baseRec,VXR.Last[entryN])) {
       if (!sX(ReadIrType(fp,VXR.Offset[entryN],&irType),&pStatus)) {
	 return pStatus;
       }
       switch (irType) {
	 case VXR_:
	   return PrevRecord_r(fp,VXR.Offset[entryN],baseRec,prevRec,found);
	 case VVR_:
	 case CVVR_:
	   *prevRec = baseRec;
	   ASSIGNnotNULL (found, TRUE)
	   return pStatus;
	 default:
	   return CORRUPTED_V2_CDF;
       }
     }
     /*************************************************************************
     * If this is the last entry in the current VXR...
     *************************************************************************/
     if (entryN == VXR.NusedEntries - 1) {
       if (VXR.VXRnext == ZERO_OFFSET) {
	 *prevRec = VXR.Last[entryN];
	 ASSIGNnotNULL (found, TRUE)
	 return pStatus;
       }
       if (!sX(ReadVXR(fp,VXR.VXRnext,
		       VXR_RECORD,&nextVXR,
		       VXR_NULL),&pStatus)) return pStatus;
       if (baseRec < nextVXR.First[0]) {
	 *prevRec = VXR.Last[entryN];
	 ASSIGNnotNULL (found, TRUE)
	 return pStatus;
       }
       VXR = nextVXR;
       entryN = 0;
     }
     else {
       if (baseRec < VXR.First[entryN+1]) {
	 *prevRec = VXR.Last[entryN];
	 ASSIGNnotNULL (found, TRUE)
	 return pStatus;
       }
       entryN++;
     }
  }
}

/******************************************************************************
* NextRecord.
*   Determine the next allocated record AT or AFTER `baseRec'.  I.e., if
* `baseRec' is allocated, it is returned as `nextRec'.
******************************************************************************/

STATICforIDL CDFstatus NextRecord (CDF, VDRoffset, zVar, baseRec, nextRec,
				   found)
struct CDFstruct *CDF;
Int32 VDRoffset;
Logical zVar;
Int32 baseRec;
Int32 *nextRec;
Logical *found;         /* If NULL, return NO_SUCH_RECORD if the next record
			   doesn't exist.  Otherwise, set according to whether
			   or not the next record exists and return the pending
			   status. */
{
  CDFstatus pStatus = CDF_OK; Int32 VXRoffset, maxRec;
  /****************************************************************************
  * If multi-file...
  ****************************************************************************/
  if (!CDF->singleFile) {
    if (!sX(ReadVDR(CDF,CDF->fp,VDRoffset,zVar,
		    VDR_MAXREC,&maxRec,
		    VDR_NULL),&pStatus)) return pStatus;
    if (baseRec <= maxRec) {
      *nextRec = baseRec;
      ASSIGNnotNULL (found, TRUE)
    }
    else {
      ASSIGNnotNULL (found, FALSE)
      if (found == NULL) pStatus = NO_SUCH_RECORD;
    }
    return pStatus;
  }
  /****************************************************************************
  * ...single-file, read the offset of the first VXR.
  ****************************************************************************/
  if (!sX(ReadVDR(CDF,CDF->fp,VDRoffset,zVar,
		  VDR_VXRHEAD,&VXRoffset,
		  VDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * ...and start there.
  ****************************************************************************/
  if (!sX(NextRecord_r(CDF->fp,VXRoffset,
		       baseRec,nextRec,found),&pStatus)) return pStatus;
  return pStatus;
}

static CDFstatus NextRecord_r (fp, vxrOffset, baseRec, nextRec, found)
vFILE *fp;
Int32 vxrOffset;
Int32 baseRec;
Int32 *nextRec;
Logical *found;
{
  CDFstatus pStatus = CDF_OK;
  int entryN; struct VXRstruct VXR; Int32 irType;
  /****************************************************************************
  * While more VXRs...
  ****************************************************************************/
  while (vxrOffset != ZERO_OFFSET) {
    /**************************************************************************
    * Read the VXR.
    **************************************************************************/
    if (!sX(ReadVXR(fp,vxrOffset,
		    VXR_RECORD,&VXR,
		    VXR_NULL),&pStatus)) return pStatus;
    /**************************************************************************
    * Search entries...
    **************************************************************************/
    if (baseRec <= VXR.Last[(int)(VXR.NusedEntries-1)]) {
      for (entryN = 0; entryN < VXR.NusedEntries; entryN++) {
	 if (baseRec <= VXR.Last[entryN]) {
	   if (!sX(ReadIrType(fp,
			      VXR.Offset[entryN],
			      &irType),&pStatus)) return pStatus;
	   switch (irType) {
	     case VXR_:
	       return NextRecord_r(fp,VXR.Offset[entryN],
				   baseRec,nextRec,found);
	     case VVR_:
	     case CVVR_:
	       *nextRec = BOO(VXR.First[entryN] <= baseRec,
			      baseRec,VXR.First[entryN]);
	       ASSIGNnotNULL (found, TRUE)
	       return pStatus;
	     default:
	       return CORRUPTED_V2_CDF;
	   }
	 }
      }
    }
    vxrOffset = VXR.VXRnext;
  }
  /****************************************************************************
  * No (more) VXRs.  The record number was never found.
  ****************************************************************************/
  ASSIGNnotNULL (found, FALSE)
  return BOO(found == NULL,NO_SUCH_RECORD,pStatus);
}

/******************************************************************************
* SearchForRecord.
******************************************************************************/

STATICforIDL CDFstatus SearchForRecord (CDF, VDRoffset, zVar, recNum,
					firstRec, lastRec, offset, found)
struct CDFstruct *CDF;
Int32 VDRoffset;
Logical zVar;
Int32 recNum;
Int32 *firstRec;
Int32 *lastRec;
Int32 *offset;
Logical *found;         /* If NULL, return NO_SUCH_RECORD if the record is
			   not found.  Otherwise, set according to whether
			   or not the record is found and return the pending
			   status. */
{
  CDFstatus pStatus = CDF_OK; Int32 vxrOffset, maxRec;
  /****************************************************************************
  * If multi-file...
  ****************************************************************************/
  if (!CDF->singleFile) {
    if (!sX(ReadVDR(CDF,CDF->fp,VDRoffset,zVar,
		    VDR_MAXREC,&maxRec,
		    VDR_NULL),&pStatus)) return pStatus;
    if (recNum <= maxRec) {
      ASSIGNnotNULL (firstRec, 0)
      ASSIGNnotNULL (lastRec, maxRec)
      ASSIGNnotNULL (offset, ZERO_OFFSET)
      ASSIGNnotNULL (found, TRUE)
    }
    else {
      ASSIGNnotNULL (found, FALSE)
      if (found == NULL) pStatus = NO_SUCH_RECORD;
    }
    return pStatus;
  }
  /****************************************************************************
  * ...single-file, read the offset of the first VXR.
  ****************************************************************************/
  if (!sX(ReadVDR(CDF,CDF->fp,VDRoffset,zVar,
		  VDR_VXRHEAD,&vxrOffset,
		  VDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * ...and start searching there.
  ****************************************************************************/
  if (!sX(SearchForRecord_r(CDF->fp,vxrOffset,recNum,
			    firstRec,lastRec,
			    offset,found),&pStatus)) return pStatus;
  return pStatus;
}

static CDFstatus SearchForRecord_r (fp, vxrOffset, recNum, firstRec, lastRec,
				    offset, found)
vFILE *fp;
Int32 vxrOffset;
Int32 recNum;
Int32 *firstRec;
Int32 *lastRec;
Int32 *offset;
Logical *found;
{
  CDFstatus pStatus = CDF_OK;
  int entryN; struct VXRstruct VXR; Int32 irType;
  /****************************************************************************
  * While more VXRs...
  ****************************************************************************/
  while (vxrOffset != ZERO_OFFSET) {
    /**************************************************************************
    * Read the VXR.
    **************************************************************************/
    if (!sX(ReadVXR(fp,vxrOffset,
		    VXR_RECORD,&VXR,
		    VXR_NULL),&pStatus)) return pStatus;
    /**************************************************************************
    * Search entries...
    **************************************************************************/
    if (recNum <= VXR.Last[(int)(VXR.NusedEntries-1)]) {
      for (entryN = 0; entryN < VXR.NusedEntries; entryN++) {
	 if (recNum <= VXR.Last[entryN]) {
	   if (VXR.First[entryN] <= recNum) {
	     if (!sX(ReadIrType(fp,
				VXR.Offset[entryN],
				&irType),&pStatus)) return pStatus;
	     switch (irType) {
	       case VXR_:
		 return SearchForRecord_r(fp,VXR.Offset[entryN],recNum,
					  firstRec,lastRec,offset,found);
	       case VVR_:
	       case CVVR_:
		 ASSIGNnotNULL (firstRec, VXR.First[entryN])
		 ASSIGNnotNULL (lastRec, VXR.Last[entryN])
		 ASSIGNnotNULL (offset, VXR.Offset[entryN])
		 ASSIGNnotNULL (found, TRUE)
		 return pStatus;
	       default:
		 return CORRUPTED_V2_CDF;
	     }
	   }
	   else {
	     ASSIGNnotNULL (found, FALSE)
	     return BOO(found == NULL,NO_SUCH_RECORD,pStatus);
	   }
	 }
      }
    }
    vxrOffset = VXR.VXRnext;
  }
  /****************************************************************************
  * No (more) VXRs.  The record number was never found.
  ****************************************************************************/
  ASSIGNnotNULL (found, FALSE)
  return BOO(found == NULL,NO_SUCH_RECORD,pStatus);
}

/******************************************************************************
* IndexingStatistics.
******************************************************************************/

STATICforIDL CDFstatus IndexingStatistics (CDF, VDRoffset, zVar, nVXRsP,
					   nEntriesP, nAllocP, nRecordsP,
					   nLevelsP)
struct CDFstruct *CDF;
Int32 VDRoffset;
Logical zVar;
Int32 *nVXRsP;
Int32 *nEntriesP;
Int32 *nAllocP;
Int32 *nRecordsP;
Int32 *nLevelsP;
{
  CDFstatus pStatus = CDF_OK; Int32 vxrOffset, maxRec; int level = 1;
  Int32 nVXRs = 0, nEntries = 0, nAlloc = 0, nRecords = 0, nLevels = 0;
  /****************************************************************************
  * Read the maximum record and the offset of the first VXR...
  ****************************************************************************/
  if (!sX(ReadVDR(CDF,CDF->fp,VDRoffset,zVar,
		  VDR_VXRHEAD,&vxrOffset,
		  VDR_MAXREC,&maxRec,
		  VDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * ...and start there.
  ****************************************************************************/
  if (vxrOffset != ZERO_OFFSET) {
    if (!sX(IndexingStatistics_r(CDF->fp,vxrOffset,
				 maxRec,level,&nLevels,
				 &nVXRs,&nEntries,
				 &nAlloc,&nRecords),&pStatus)) return pStatus;
  }
  /****************************************************************************
  * Pass back requested statistics.
  ****************************************************************************/
  ASSIGNnotNULL (nVXRsP, nVXRs)
  ASSIGNnotNULL (nEntriesP, nEntries)
  ASSIGNnotNULL (nAllocP, nAlloc)
  ASSIGNnotNULL (nRecordsP, nRecords)
  ASSIGNnotNULL (nLevelsP, nLevels)
  return pStatus;
}

static CDFstatus IndexingStatistics_r (fp, vxrOffset, maxRec, level, nLevels,
				       nVXRs, nEntries, nAlloc, nRecords)
vFILE *fp;
Int32 vxrOffset;
Int32 maxRec;
int level;
Int32 *nLevels;
Int32 *nVXRs;
Int32 *nEntries;
Int32 *nAlloc;
Int32 *nRecords;
{
  CDFstatus pStatus = CDF_OK;
  int e; Int32 irType; struct VXRstruct VXR;
  /****************************************************************************
  * Check if a new level has been reached.
  ****************************************************************************/
  *nLevels = MAXIMUM(*nLevels,level);
  /****************************************************************************
  * While more VXRs...
  ****************************************************************************/
  while (vxrOffset != ZERO_OFFSET) {
    /**************************************************************************
    * Read/tally the VXR.
    **************************************************************************/
    if (!sX(ReadVXR(fp,vxrOffset,
		    VXR_RECORD,&VXR,
		    VXR_NULL),&pStatus)) return pStatus;
    (*nVXRs)++;
    /**************************************************************************
    * Scan/count entries...
    **************************************************************************/
    for (e = 0; e < VXR.NusedEntries; e++) {
       (*nEntries)++;
       if (!sX(ReadIrType(fp,
			  VXR.Offset[e],
			  &irType),&pStatus)) return pStatus;
       switch (irType) {
	 case VXR_:
	   if (!sX(IndexingStatistics_r(fp,VXR.Offset[e],
					maxRec,level+1,
					nLevels,nVXRs,
					nEntries,nAlloc,
					nRecords),&pStatus)) return pStatus;
	   break;
	 case VVR_:
	 case CVVR_:
	   *nAlloc += (VXR.Last[e] - VXR.First[e] + 1);
	   if (VXR.First[e] <= maxRec) {
	     *nRecords += MINIMUM(maxRec,VXR.Last[e]) - VXR.First[e] + 1;
	   }
	   break;
	 default:
	   return CORRUPTED_V2_CDF;
       }
    }
    vxrOffset = VXR.VXRnext;
  }
  return pStatus;
}

/******************************************************************************
* BuildPadBuffer.
******************************************************************************/

STATICforIDL CDFstatus BuildPadBuffer (CDF, Var, nRecords, how, buffer, encode)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 nRecords;
int *how;
void **buffer;
Logical encode;         /* If TRUE, return values in CDF's encoding.  If FALSE,
			   return values in host computer's encoding. */
{
  Byte1 *ptr; Int32 nBytes, nValues, valueN; void *padValue;
  Int32 VDRflags, dataType, numElems; CDFstatus pStatus = CDF_OK;
  /****************************************************************************
  * Determine pad value.
  ****************************************************************************/
  if (!sX(ReadVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
		  VDR_DATATYPE,&dataType,
		  VDR_NUMELEMS,&numElems,
		  VDR_FLAGS,&VDRflags,
		  VDR_NULL),&pStatus)) return pStatus;
  padValue = (void *) cdf_AllocateMemory ((size_t) Var->NvalueBytes, NULL);
  if (padValue == NULL) return BAD_MALLOC;
  if (PADvalueBITset(VDRflags)) {
    if (!sX(ReadVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
		    VDR_PADVALUE,padValue,
		    VDR_NULL),&pStatus)) {
      cdf_FreeMemory (padValue, NULL);
      return pStatus;
    }
    if (!encode) {
      if (!sX(ConvertBuffer(CDF->encoding,HostDecoding(),
			    CDF->negToPosFp0,dataType,
			    numElems,padValue,padValue),&pStatus)) {
	cdf_FreeMemory (padValue, NULL);
	return pStatus;
      }
    }
  }
  else {
    DefaultPadValuePre350 (dataType, numElems, padValue);
    if (encode) {
      if (!sX(ConvertBuffer(HostEncoding(),CDF->encoding,
			    CDF->negToPosFp0,dataType,
			    numElems,padValue,padValue),&pStatus)) {
	cdf_FreeMemory (padValue, NULL);
	return pStatus;
      }
    }
  }
  /****************************************************************************
  * Try to allocate all of the records at once...
  ****************************************************************************/
  nValues = nRecords * Var->NphyRecValues;
  nBytes = nValues * Var->NvalueBytes;
#if LIMITof64K
  if (nBytes < 65536L) {
#endif
    *buffer = (void *) cdf_AllocateMemory ((size_t) nBytes, NULL);
    if (*buffer != NULL) {
      for (valueN = 0, ptr = (Byte1 *) *buffer;
	   valueN < nValues; valueN++, ptr += (size_t) Var->NvalueBytes) {
	 memmove (ptr, padValue, (size_t) Var->NvalueBytes);
      }
      cdf_FreeMemory (padValue, NULL);
      *how = ALLrecordsATonce;
      return pStatus;
    }
#if LIMITof64K
  }
#endif
  /****************************************************************************
  * Not enough memory for that, try allocating one record...
  ****************************************************************************/
#if LIMITof64K
  if (Var->NphyRecBytes < 65536L) {
#endif
    *buffer = (void *) cdf_AllocateMemory ((size_t) Var->NphyRecBytes, NULL);
    if (*buffer != NULL) {
      for (valueN = 0, ptr = (Byte1 *) *buffer;
	   valueN < Var->NphyRecValues;
	   valueN++, ptr += (size_t) Var->NvalueBytes) {
	 memmove (ptr, padValue, (size_t) Var->NvalueBytes);
      }
      cdf_FreeMemory (padValue, NULL);
      *how = ONErecordATaTIME;
      return pStatus;
    }
#if LIMITof64K
  }
#endif
  /****************************************************************************
  * Not enough memory for that either, use the one allocated value...
  ****************************************************************************/
  *buffer = padValue;
  *how = ONEvalueATaTIME;
  return pStatus;
}

/******************************************************************************
* ReadVarValues.
*   NOTE: If more than one record is being read, full records are assumed
* and the `offset' must be zero.
******************************************************************************/

STATICforIDL CDFstatus ReadVarValues (CDF, Var, startRec, offset, nValues,
				      buffer)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 startRec;         /* Physical record number at which to start reading. */
Int32 offset;           /* Byte offset within (first) record at which to
			   begin reading. */
Int32 nValues;          /* Number of values to read. */
void *buffer;
{
  CDFstatus pStatus = CDF_OK; Int32 tOffset; Logical fullRecord;
  Int32 numElems, firstRec, lastRec, lastRecInVVR, nPadValues;
  Int32 readTo, recCount, nBytes;
  /****************************************************************************
  * Determine first/last record(s) being read and if full physical record(s).
  ****************************************************************************/
  firstRec = startRec;
  if (nValues < Var->NphyRecValues) {
    fullRecord = FALSE;
    lastRec = startRec;
  }
  else {
    fullRecord = TRUE;
    lastRec = startRec + ((nValues - 1) / Var->NphyRecValues);
  }
  /****************************************************************************
  * Read value(s).
  ****************************************************************************/
  switch (Var->vType) {
    /**************************************************************************
    * Standard variable in a single-file CDF...
    **************************************************************************/
    case STANDARD_:
      if (fullRecord) {
	/**********************************************************************
	* Full record(s) - read from contiguous groups...
	**********************************************************************/
	Byte1 *tBuffer = buffer; Int32 recNum = firstRec;
	while (recNum <= lastRec) {
	  if (recNum <= Var->maxRec) {
	    if (!sX(SearchForRecord(CDF,Var->VDRoffset,
				    Var->zVar,recNum,
				    NULL,&lastRecInVVR,
				    NULL,NULL),&pStatus)) return pStatus;
	    readTo = MINIMUMof3(Var->maxRec,lastRec,lastRecInVVR);
	    recCount = readTo - recNum + 1;
	    if (!sX(RecordByteOffset(CDF,Var,
				     recNum,
				     &tOffset),&pStatus)) return pStatus;
	    numElems = recCount * Var->NphyRecElems;
	    if (!sX(ReadVarElems(Var,CDF->fp,tOffset,
				 numElems,tBuffer),&pStatus)) return pStatus;
	  }
	  else {
	    recCount = lastRec - recNum + 1;
	    nPadValues = recCount * Var->NphyRecValues;
	    if (!Var->recVary) {
              int ix;
              for (ix = 0; ix < (int) recCount; ++ix)
                memcpy (tBuffer + ix * Var->NphyRecBytes,
                        tBuffer - Var->NphyRecBytes,
                        (size_t) Var->NphyRecBytes);
            } else {
	      if (!sX(PadBuffer(CDF,Var,
			        nPadValues,
			        tBuffer),&pStatus)) return pStatus;
	      sX (VIRTUAL_RECORD_DATA, &pStatus);
	    }
	  }
	  recNum += recCount;
	  tBuffer += (size_t) (recCount * Var->NphyRecBytes);
	}
      }
      else {
	/**********************************************************************
	* Partial record...
	**********************************************************************/
	if (firstRec <= Var->maxRec) {
	  if (!sX(RecordByteOffset(CDF,Var,
				   firstRec,
				   &tOffset),&pStatus)) return pStatus;
	  tOffset += (Int32) offset;
	  numElems = nValues * Var->NvalueElems;
	  if (!sX(ReadVarElems(Var,CDF->fp,tOffset,
			       numElems,buffer),&pStatus)) return pStatus;
	}
	else {
	  if (!sX(PadBuffer(CDF,Var,nValues,buffer),&pStatus)) return pStatus;
	  sX (VIRTUAL_RECORD_DATA, &pStatus);
	}
      }
      break;
    /**************************************************************************
    * Sparse records...
    **************************************************************************/
    case SPARSE_RECORDS_: {
      if (fullRecord) {
	if (!sX(ReadSparseFull(CDF,Var,firstRec,
			       lastRec,buffer),&pStatus)) return pStatus;
      }
      else {
	if (!sX(ReadSparsePartial(CDF,Var,
				  startRec,offset,
				  nValues,buffer),&pStatus)) return pStatus;
      }
      break;
    }
    /**************************************************************************
    * Compressed variable...
    **************************************************************************/
    case COMPRESSED_:
    case SPARSE_COMPRESSED_RECORDS_: {
      /************************************************************************
      * Initialize staging area.  Note that the staging area is only
      * initialized if records have been written.  This prevents the
      * CORRUPTED_V2_CDF error code from being returned if an explicit
      * blocking factor has not been specified for the variable.
      ************************************************************************/
      if (Var->stage.areaOffset == NO_OFFSET && Var->maxRec > NO_RECORD) {
	if (Var->blockingFactor == 0) return CORRUPTED_V2_CDF;
	nBytes = Var->blockingFactor * Var->NphyRecBytes;
	if (!sX(InitVarStage(CDF,Var,nBytes),&pStatus)) return pStatus;
      }
      /************************************************************************
      * If full record(s) or partial record...
      ************************************************************************/
      if (fullRecord) {
	if (!sX(ReadCompressedFull(CDF,Var,firstRec,
				   lastRec,buffer),&pStatus)) return pStatus;
      }
      else {
	if (!sX(ReadCompressedPartial(CDF,Var,startRec,
				      offset,nValues,
				      buffer),&pStatus)) return pStatus;
      }
      break;
    }
    /**************************************************************************
    * Can't do sparse arrays yet...
    **************************************************************************/
    case SPARSE_ARRAYS_:
    case SPARSE_RECORDS_AND_ARRAYS_:
      return UNKNOWN_SPARSENESS;
    case IN_MULTI_:
      if (fullRecord) {
	/********************************************************************
	* Full record(s).
	********************************************************************/
	Byte1 *tBuffer = buffer;
	if (firstRec <= Var->maxRec) {
	  if (!sX(RecordByteOffset(CDF,Var,
				   firstRec,
				   &tOffset),&pStatus)) return pStatus;
	  recCount = MINIMUM(lastRec,Var->maxRec) - firstRec + 1;
	  numElems = recCount * Var->NphyRecElems;
	  if (!sX(ReadVarElems(Var,Var->fp,tOffset,
			       numElems,tBuffer),&pStatus)) return pStatus;
	  tBuffer += (size_t) (recCount * Var->NphyRecBytes);
	}
	if (lastRec > Var->maxRec) {
	  recCount = BOO(Var->maxRec < firstRec,
			 lastRec - firstRec + 1,
			 lastRec - Var->maxRec);
	  nPadValues = recCount * Var->NphyRecValues;
	  if (!sX(PadBuffer(CDF,Var,
			    nPadValues,
			    tBuffer),&pStatus)) return pStatus;
	  sX (VIRTUAL_RECORD_DATA, &pStatus);
	}
      }
      else {
	/********************************************************************
	* Partial record.
	********************************************************************/
	if (firstRec <= Var->maxRec) {
	  if (!sX(RecordByteOffset(CDF,Var,
				   firstRec,
				   &tOffset),&pStatus)) return pStatus;
	  tOffset += (Int32) offset;
	  numElems = nValues * Var->NvalueElems;
	  if (!sX(ReadVarElems(Var,Var->fp,tOffset,
			       numElems,buffer),&pStatus)) return pStatus;
	}
	else {
	  if (!sX(PadBuffer(CDF,Var,nValues,buffer),&pStatus)) return pStatus;
	  sX (VIRTUAL_RECORD_DATA, &pStatus);
	}
      }
      break;
  }
  return pStatus;
}

/******************************************************************************
* ReadVarElems.
*   NOTE: On IBM PCs, it is assumed that the number of bytes being read
* will not exceed 65535.
******************************************************************************/

STATICforIDL CDFstatus ReadVarElems (Var, fp, offset, numElems, buffer)
struct VarStruct *Var;
vFILE *fp;
Int32 offset;
Int32 numElems;
void *buffer;
{
  CDFstatus pStatus = CDF_OK; size_t nBytes;
  /****************************************************************************
  * Seek to the desired offset.
  ****************************************************************************/
  if (!SEEKv(fp,(long)offset,vSEEK_SET)) return VAR_READ_ERROR;
  /****************************************************************************
  * Read the value(s).
  ****************************************************************************/
  nBytes = (size_t) (numElems * Var->NelemBytes);
  if (!READv(buffer,nBytes,1,fp)) return VAR_READ_ERROR;
  /****************************************************************************
  * Decode value(s).
  ****************************************************************************/
  if (!sX(DECODE(Var->DecodeFunction,buffer,numElems),&pStatus)) {
    return pStatus;
  }
  return pStatus;
}

/******************************************************************************
* ROWtoCOL.
******************************************************************************/

VISIBLE_PREFIX void ROWtoCOL (iBuffer, oBuffer, numDims, dimSizes, nValueBytes)
void *iBuffer;
void *oBuffer;
long numDims;
long dimSizes[];
long nValueBytes;
{
  switch (numDims) {
    case 0:
    case 1: {
      long nValues; int dimN;
      for (dimN = 0, nValues = 1; dimN < numDims; dimN++) {
	 nValues *= dimSizes[dimN];
      }
      memmove (oBuffer, iBuffer, (size_t) (nValues * nValueBytes));
      break;
    }
    default: {
      long products[CDF_MAX_DIMS];        /* Products, what each dimension is
					     `worth'. */
      long iBoffset;                      /* Input buffer, byte offset. */
      long oBoffset;                      /* Output buffer, byte offset. */
      long oVoffset;                      /* Output buffer, value offset. */
      int dimN;                           /* Dimension number. */
      for (dimN = 1, products[0] = 1; dimN < numDims; dimN++) {
	 products[dimN] = products[dimN-1] * dimSizes[dimN-1];
      }
      switch (numDims) {
	case 2: {
	  int d0, d1;                    /* Indices... */
	  for (d0 = 0, iBoffset = 0; d0 < dimSizes[0]; d0++) {
	     for (d1 = 0; d1 < dimSizes[1]; d1++) {
		oVoffset = ((long) d0 * products[0]) + ((long) d1 * products[1]);
		oBoffset = oVoffset * nValueBytes;
		memmove ((Byte1 *) oBuffer + (size_t) oBoffset,
			 (Byte1 *) iBuffer + (size_t) iBoffset,
			 (size_t) nValueBytes);
		iBoffset += nValueBytes;
	     }
	  }
	  break;
	}
	case 3: {
	  int d0, d1, d2;                /* Indices... */
	  for (d0 = 0, iBoffset = 0; d0 < dimSizes[0]; d0++) {
	     for (d1 = 0; d1 < dimSizes[1]; d1++) {
		for (d2 = 0; d2 < dimSizes[2]; d2++) {
		   oVoffset = ((long) d0 * products[0]) + ((long) d1 * products[1]) +
			      ((long) d2 * products[2]);
		   oBoffset = oVoffset * nValueBytes;
		   memmove ((Byte1 *) oBuffer + (size_t) oBoffset,
			    (Byte1 *) iBuffer + (size_t) iBoffset,
			    (size_t) nValueBytes);
		   iBoffset += nValueBytes;
		}
	     }
	  }
	  break;
	}
	default: {
	  long indices[CDF_MAX_DIMS]; long nValues; int i;
	  for (dimN = 0; dimN < numDims; dimN++) indices[dimN] = 0;
	  for (dimN = 0, nValues = 1; dimN < numDims; dimN++) {
	     nValues *= dimSizes[dimN];
	  }
	  for (i = 0, iBoffset = 0; i < (int) nValues; i++) {
	     for (oVoffset = 0, dimN = 0; dimN < numDims; dimN++) {
		oVoffset += indices[dimN] * products[dimN];
	     }
	     oBoffset = oVoffset * nValueBytes;
	     memmove ((Byte1 *) oBuffer + (size_t) oBoffset,
		      (Byte1 *) iBuffer + (size_t) iBoffset,
		      (size_t) nValueBytes);
	     iBoffset += nValueBytes;
	     INCRindicesROW (numDims, dimSizes, indices);
	  }
	  break;
	}
      }
      break;
    }
  }
  return;
}

/******************************************************************************
* COLtoROW.
******************************************************************************/

VISIBLE_PREFIX void COLtoROW (iBuffer, oBuffer, numDims, dimSizes, nValueBytes)
void *iBuffer;
void *oBuffer;
long numDims;
long dimSizes[];
long nValueBytes;
{
  switch (numDims) {
    case 0:
    case 1: {
      long nValues; int dimN;
      for (dimN = 0, nValues = 1; dimN < numDims; dimN++) {
	 nValues *= dimSizes[dimN];
      }
      memmove (oBuffer, iBuffer, (size_t) (nValues * nValueBytes));
      break;
    }
    default: {
      long products[CDF_MAX_DIMS];        /* Products, what each dimension is
					     `worth'. */
      long iBoffset;                      /* Input buffer, byte offset. */
      long oBoffset;                      /* Output buffer, byte offset. */
      long oVoffset;                      /* Output buffer, value offset. */
      int dimN;                           /* Dimension number. */
      products[(int)(numDims-1)] = 1;
      for (dimN = (int) (numDims - 2); dimN >= 0; dimN--) {
	 products[dimN] = products[dimN+1] * dimSizes[dimN+1];
      }
      switch (numDims) {
	case 2: {
	  int d0, d1;                    /* Indices... */
	  for (d1 = 0, iBoffset = 0; d1 < dimSizes[1]; d1++) {
	     for (d0 = 0; d0 < dimSizes[0]; d0++) {
		oVoffset = ((long) d0 * products[0]) + ((long) d1 * products[1]);
		oBoffset = oVoffset * nValueBytes;
		memmove ((Byte1 *) oBuffer + (size_t) oBoffset,
			 (Byte1 *) iBuffer + (size_t) iBoffset,
			 (size_t) nValueBytes);
		iBoffset += nValueBytes;
	     }
	  }
	  break;
	}
	case 3: {
	  int d0, d1, d2;                /* Indices... */
	  for (d2 = 0, iBoffset = 0; d2 < dimSizes[2]; d2++) {
	     for (d1 = 0; d1 < dimSizes[1]; d1++) {
		for (d0 = 0; d0 < dimSizes[0]; d0++) {
		   oVoffset = ((long) d0 * products[0]) + ((long) d1 * products[1]) +
			      ((long) d2 * products[2]);
		   oBoffset = oVoffset * nValueBytes;
		   memmove ((Byte1 *) oBuffer + (size_t) oBoffset,
			    (Byte1 *) iBuffer + (size_t) iBoffset,
			    (size_t) nValueBytes);
		   iBoffset += nValueBytes;
		}
	     }
	  }
	  break;
	}
	default: {
	  long indices[CDF_MAX_DIMS]; long nValues; int i;
	  for (dimN = 0; dimN < numDims; dimN++) indices[dimN] = 0;
	  for (dimN = 0, nValues = 1; dimN < numDims; dimN++) {
	     nValues *= dimSizes[dimN];
	  }
	  for (i = 0, iBoffset = 0; i < (int) nValues; i++) {
	     for (oVoffset = 0, dimN = 0; dimN < numDims; dimN++) {
		oVoffset += indices[dimN] * products[dimN];
	     }
	     oBoffset = oVoffset * nValueBytes;
	     memmove ((Byte1 *) oBuffer + (size_t) oBoffset,
		      (Byte1 *) iBuffer + (size_t) iBoffset,
		      (size_t) nValueBytes);
	     iBoffset += nValueBytes;
	     INCRindicesCOL (numDims, dimSizes, indices);
	  }
	  break;
	}
      }
      break;
    }
  }
  return;
}

/******************************************************************************
* INCRindicesROW.
*    Increment to next set of indices, row majority.  When at the last set of
* indices, roll over to 0,0,0,...
******************************************************************************/

VISIBLE_PREFIX void INCRindicesROW (numDims, dimSizes, indices)
long numDims;
long dimSizes[];
long indices[];
{
  int dimN;
  for (dimN = (int) (numDims - 1); dimN >= 0; dimN--)
     if (indices[dimN] == dimSizes[dimN] - 1)
       indices[dimN] = 0;
     else {
       indices[dimN]++;
       break;
     }
  return;
}

/******************************************************************************
* INCRindicesCOL.
*    Increment to next set of indices, column majority.  When at the last set
* of indices, roll over to 0,0,0,...
******************************************************************************/

VISIBLE_PREFIX void INCRindicesCOL (numDims, dimSizes, indices)
long numDims;
long dimSizes[];
long indices[];
{
  int dimN;
  for (dimN = 0; dimN < numDims; dimN++)
     if (indices[dimN] == dimSizes[dimN] - 1)
       indices[dimN] = 0;
     else {
       indices[dimN]++;
       break;
     }
  return;
}

/******************************************************************************
* InitVarStage.
******************************************************************************/

STATICforIDL CDFstatus InitVarStage (CDF, Var, nBytes)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 nBytes;
{
  if (CDF->stage.fp == NULL) {
    CDF->stage.fp = V_scratch (ScratchDirectory(CDF), "stg");
    if (CDF->stage.fp == NULL) return SCRATCH_CREATE_ERROR;
    if (!CACHEv(CDF->stage.fp,CDF->stage.cacheSize)) {
      V_delete (CDF->stage.fp, NULL);
      CDF->stage.fp = NULL;
      return BAD_CACHE_SIZE;
    }
    CDF->stage.mark = ZERO_OFFSET;
  }
  Var->stage.areaOffset = CDF->stage.mark;
  Var->stage.firstRec = NO_RECORD;
  Var->stage.lastRec = NO_RECORD;
  Var->stage.dotOffset = NO_OFFSET;
  Var->stage.modified = FALSE;
  CDF->stage.mark += nBytes;
  return CDF_OK;
}

/******************************************************************************
* InitScratch.
******************************************************************************/

STATICforIDL CDFstatus InitScratch (scratchDir, scratchFpH, cacheSize)
char *scratchDir;	/* Scratch directory to be used. */
vFILE **scratchFpH;	/* Scratch file handle (pointer to pointer). */
int cacheSize;		/* Number of cache buffers to request. */
{
  if (*scratchFpH == NULL) {
    *scratchFpH = V_scratch (scratchDir, NULL);
    if (*scratchFpH == NULL) return SCRATCH_CREATE_ERROR;
    if (!CACHEv(*scratchFpH,cacheSize)) {
      V_delete (*scratchFpH, NULL);
      *scratchFpH = NULL;
      return BAD_CACHE_SIZE;
    }
  }
  else {
    if (V_clear(*scratchFpH) != 0) return SCRATCH_READ_ERROR;
    if (!SEEKv(*scratchFpH,0L,vSEEK_SET)) return SCRATCH_READ_ERROR;
  }
  return CDF_OK;
}

/******************************************************************************
* ScratchDirectory.
******************************************************************************/

STATICforIDL char *ScratchDirectory (CDF)
struct CDFstruct *CDF;	/* Pointer to CDF structure.  This might be NULL if a
			   scratch directory is needed but a CDF has not yet
			   been opened/created. */
{
  char *envTmp = NULL;
  if (CDF != NULL) {
    if (CDF->scratchDir != NULL) return CDF->scratchDir;
  }
#if defined(vms)
   envTmp = getenv ("CDF$TMP");
#else
#  if defined(unix) || defined(dos) || defined(posixSHELL) || defined(win32)
     envTmp = getenv ("CDF_TMP");
#  endif
#endif
#if defined(win32)
   if (envTmp == NULL) envTmp = getenv ("TMP");
#endif
  return envTmp;
}

/******************************************************************************
* FlushStage.
******************************************************************************/

STATICforIDL CDFstatus FlushStage (CDF, Var)
struct CDFstruct *CDF;
struct VarStruct *Var;
{
  CDFstatus pStatus = CDF_OK;
  /****************************************************************************
  * Based on the variable type...
  ****************************************************************************/
  switch (Var->vType) {
    case SPARSE_RECORDS_: {
      Int32 nRecords, nBytes, tOffset; struct AllocStruct alloc;
      /************************************************************************
      * First check if there are records to be flushed in the staging area.
      ************************************************************************/
      if (!Var->stage.modified) return pStatus;
      /************************************************************************
      * Allocate a new VVR.
      ************************************************************************/
      LoadAllocVVR (alloc, Var->stage.firstRec, Var->stage.lastRec, FALSE)
      if (!sX(AllocateRecords(CDF,Var,alloc),&pStatus)) return pStatus;
      Var->maxAllocated = MAXIMUM(Var->maxAllocated,Var->stage.lastRec);
      /************************************************************************
      * Copy the records to the new VVR.
      ************************************************************************/
      if (!sX(RecordByteOffset(CDF,Var,
			       Var->stage.firstRec,
			       &tOffset),&pStatus)) return pStatus;
      nRecords = Var->stage.lastRec - Var->stage.firstRec + 1;
      nBytes = nRecords * Var->NphyRecBytes;
      if (!sX(CopyBytes(CDF->stage.fp,Var->stage.areaOffset,
			SCRATCH_READ_ERROR,nBytes,CDF->fp,
			tOffset,CDF_WRITE_ERROR),&pStatus)) return pStatus;
      Var->maxWritten = MAXIMUM(Var->maxWritten,Var->stage.lastRec);
      /************************************************************************
      * Clear the staging area control.
      ************************************************************************/
      Var->stage.firstRec = NO_RECORD;
      Var->stage.lastRec = NO_RECORD;
      Var->stage.dotOffset = NO_OFFSET;
      Var->stage.modified = FALSE;
      break;
    }
    case COMPRESSED_:
    case SPARSE_COMPRESSED_RECORDS_: {
      Int32 cSize, xSize, irSize, newOffset, nRecords, uSize;
      struct CVVRstruct CVVR; struct VVRstruct VVR; struct AllocStruct alloc;
      /************************************************************************
      * First check if there are records to be flushed in the staging area.
      ************************************************************************/
      if (!Var->stage.modified) return pStatus;
      /************************************************************************
      * Check if the scratch file is created/initialized.
      ************************************************************************/
      if (!sX(InitScratch(ScratchDirectory(CDF),
			  &(CDF->compressFp),
			  CDF->compressCacheSize),&pStatus)) return pStatus;
      /************************************************************************
      * Compress the records in the stage.
      ************************************************************************/
      nRecords = Var->stage.lastRec - Var->stage.firstRec + 1;
      uSize = nRecords * Var->NphyRecBytes;
      if (!sX(Compress(CDF->stage.fp,Var->stage.areaOffset,
		       uSize,SCRATCH_READ_ERROR,Var->cType,
		       Var->cParms,CDF->compressFp,ZERO_OFFSET,
		       &cSize,SCRATCH_WRITE_ERROR),&pStatus)) return pStatus;
      /************************************************************************
      * Does a VVR/CVVR for this block of records already exist?
      ************************************************************************/
      if (Var->stage.dotOffset != NO_OFFSET) {
	/**********************************************************************
	* Read the size of the existing VVR/CVVR.
	**********************************************************************/
	if (!sX(ReadIrSize(CDF->fp,
			   Var->stage.dotOffset,
			   &irSize),&pStatus)) return pStatus;
	/**********************************************************************
	* Will a CVVR fit?  Note that reserve space is only added to new CVVRs.
	**********************************************************************/
	if (CVVR_BASE_SIZE + cSize <= irSize) {
	  LoadCVVRx (CVVR, irSize, cSize)
	  if (!sX(WriteCVVR(CDF->fp,Var->stage.dotOffset,
			    CVVR_RECORDx,&CVVR,
			    CVVR_NULL),&pStatus)) return pStatus;
	  if (!sX(CopyBytes(CDF->compressFp,ZERO_OFFSET,
			    SCRATCH_READ_ERROR,cSize,CDF->fp,
			    Var->stage.dotOffset + CVVR_BASE_SIZE,
			    CDF_WRITE_ERROR),&pStatus)) return pStatus;
	  Var->stage.modified = FALSE;
	  return pStatus;
	}
	/**********************************************************************
	* Will a VVR fit?
	**********************************************************************/
	if (VVR_BASE_SIZE + uSize <= irSize) {
	  LoadVVRx (VVR, irSize)
	  if (!sX(WriteVVR(CDF->fp,Var->stage.dotOffset,
			   VVR_RECORDx,&VVR,
			   VVR_NULL),&pStatus)) return pStatus;
	  if (!sX(CopyBytes(CDF->stage.fp,Var->stage.areaOffset,
			    SCRATCH_READ_ERROR,uSize,CDF->fp,
			    Var->stage.dotOffset + VVR_BASE_SIZE,
			    CDF_WRITE_ERROR),&pStatus)) return pStatus;
	  Var->stage.modified = FALSE;
	  sX (DID_NOT_COMPRESS, &pStatus);
	  return pStatus;
	}
	/**********************************************************************
	* The existing VVR/CVVR will have to be resized.  Will a CVVR be
	* smaller than a VVR?  Note that reserve space is not added to the
	* size of the CVVR because a new one isn't being created.
	**********************************************************************/
	if (CVVR_BASE_SIZE + cSize < VVR_BASE_SIZE + uSize) {
	  if (!sX(ResizeIR(CDF,Var->stage.dotOffset,
			   CVVR_BASE_SIZE + cSize,
			   &newOffset,TRUE,NULL),&pStatus)) return pStatus;
	  LoadCVVRx (CVVR, CVVR_BASE_SIZE + cSize, cSize)
	  if (!sX(WriteCVVR(CDF->fp,newOffset,
			    CVVR_RECORDx,&CVVR,
			    CVVR_NULL),&pStatus)) return pStatus;
	  if (!sX(CopyBytes(CDF->compressFp,ZERO_OFFSET,
			    SCRATCH_READ_ERROR,cSize,
			    CDF->fp,newOffset + CVVR_BASE_SIZE,
			    CDF_WRITE_ERROR),&pStatus)) return pStatus;
	}
	else {
	  if (!sX(ResizeIR(CDF,Var->stage.dotOffset,
			   VVR_BASE_SIZE + uSize,
			   &newOffset,TRUE,NULL),&pStatus)) return pStatus;
	  LoadVVRx (VVR, VVR_BASE_SIZE + uSize)
	  if (!sX(WriteVVR(CDF->fp,newOffset,
			   VVR_RECORDx,&VVR,
			   VVR_NULL),&pStatus)) return pStatus;
	  if (!sX(CopyBytes(CDF->stage.fp,Var->stage.areaOffset,
			    SCRATCH_READ_ERROR,uSize,
			    CDF->fp,newOffset + VVR_BASE_SIZE,
			    CDF_WRITE_ERROR),&pStatus)) return pStatus;
	  sX (DID_NOT_COMPRESS, &pStatus);
	}
	if (!sX(ModIndexOffset(CDF,Var,
			       Var->stage.firstRec,
			       Var->stage.lastRec,
			       newOffset),&pStatus)) return pStatus;
	Var->stage.dotOffset = newOffset;
	Var->stage.modified = FALSE;
	return pStatus;
      }
      /************************************************************************
      * A new VVR/CVVR will have to be created.  First calculate the reserve
      * size.
      ************************************************************************/
      if (Var->reservePct <= 0)
	xSize = 0;
      else {
	if (Var->reservePct <= 100) {
	  Int32 tSize = (Int32) ((uSize * (Var->reservePct/100.0)) + 0.5);
	  xSize = MAXIMUM(cSize,tSize) - cSize;
	}
	else {
	  Int32 tSize = (Int32) ((cSize * (Var->reservePct/100.0)) + 0.5);
	  xSize = tSize - cSize;
	}
      }
      /************************************************************************
      * Will a CVVR be smaller than a VVR?
      ************************************************************************/
      if (CVVR_BASE_SIZE + cSize + xSize < VVR_BASE_SIZE + uSize) {
	LoadAllocCVVR (alloc, Var->stage.firstRec, Var->stage.lastRec,
		       cSize, xSize)
	if (!sX(AllocateRecords(CDF,Var,alloc),&pStatus)) return pStatus;
	if (!sX(SearchForRecord(CDF,Var->VDRoffset,Var->zVar,
				Var->stage.firstRec,NULL,NULL,
				&newOffset,NULL),&pStatus)) return pStatus;
	LoadCVVRx (CVVR, CVVR_BASE_SIZE + cSize + xSize, cSize)
	if (!sX(WriteCVVR(CDF->fp,newOffset,
			  CVVR_RECORDx,&CVVR,
			  CVVR_NULL),&pStatus)) return pStatus;
	if (!sX(CopyBytes(CDF->compressFp,ZERO_OFFSET,
			  SCRATCH_READ_ERROR,cSize,
			  CDF->fp,newOffset + CVVR_BASE_SIZE,
			  CDF_WRITE_ERROR),&pStatus)) return pStatus;
	Var->stage.dotOffset = newOffset;
	Var->stage.modified = FALSE;
	return pStatus;
      }
      /************************************************************************
      * The CVVR will be too big - create a VVR.
      ************************************************************************/
      LoadAllocVVR (alloc, Var->stage.firstRec, Var->stage.lastRec, TRUE)
      if (!sX(AllocateRecords(CDF,Var,alloc),&pStatus)) return pStatus;
      if (!sX(SearchForRecord(CDF,Var->VDRoffset,Var->zVar,
				Var->stage.firstRec,NULL,NULL,
				&newOffset,NULL),&pStatus)) return pStatus;
      LoadVVRx (VVR, VVR_BASE_SIZE + uSize)
      if (!sX(WriteVVR(CDF->fp,newOffset,
		       VVR_RECORDx,&VVR,
		       VVR_NULL),&pStatus)) return pStatus;
      if (!sX(CopyBytes(CDF->stage.fp,Var->stage.areaOffset,
			SCRATCH_READ_ERROR,uSize,
			CDF->fp,newOffset + VVR_BASE_SIZE,
			CDF_WRITE_ERROR),&pStatus)) return pStatus;
      Var->stage.dotOffset = newOffset;
      Var->stage.modified = FALSE;
      sX (DID_NOT_COMPRESS, &pStatus);
      return pStatus;
    }
    default:
      return CDF_INTERNAL_ERROR;
  }
  return pStatus;
}

/******************************************************************************
* BringToStage.
******************************************************************************/

static CDFstatus BringToStage (CDF, Var, recNum, found)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 recNum;
Logical *found;
{
  CDFstatus pStatus = CDF_OK;
  /****************************************************************************
  * First check if record is already in the stage.
  ****************************************************************************/
  if (INCLUSIVE(Var->stage.firstRec,recNum,Var->stage.lastRec)) {
    ASSIGNnotNULL (found, TRUE)
    return pStatus;
  }
  /****************************************************************************
  * Then, based on variable type...
  ****************************************************************************/
  switch (Var->vType) {
    case COMPRESSED_:
    case SPARSE_COMPRESSED_RECORDS_: {
      Int32 firstRec, lastRec, offset, nRecords, uSize; Logical foundX;
      /************************************************************************
      * Determine if the record exists.
      ************************************************************************/
      if (!sX(SearchForRecord(CDF,Var->VDRoffset,Var->zVar,
			      recNum,&firstRec,&lastRec,
			      &offset,&foundX),&pStatus)) return pStatus;
      ASSIGNnotNULL (found, foundX)
      if (!foundX) return BOO(found == NULL,NO_SUCH_RECORD,pStatus);
      /************************************************************************
      * Flush the stage before...
      ************************************************************************/
      if (!sX(FlushStage(CDF,Var),&pStatus)) return pStatus;
      /************************************************************************
      * ...decompressing the CVVR (or VVR if the records did not compress) to
      * the staging area.
      ************************************************************************/
      nRecords = lastRec - firstRec + 1;
      uSize = nRecords * Var->NphyRecBytes;
      if (!sX(DecompressToStage(CDF,Var,
				offset,uSize),&pStatus)) return pStatus;
      /************************************************************************
      * Update staging control.
      ************************************************************************/
      Var->stage.firstRec = firstRec;
      Var->stage.lastRec = lastRec;
      Var->stage.dotOffset = offset;
      Var->stage.modified = FALSE;
      break;
    }
    default:
      return CDF_INTERNAL_ERROR;
  }
  return pStatus;
}

/******************************************************************************
* CopyBytes.
******************************************************************************/

STATICforIDL CDFstatus CopyBytes (iFp, iStart, iError, nBytes, oFp, oStart,
				  oError)
vFILE *iFp;
Int32 iStart;
CDFstatus iError;
Int32 nBytes;
vFILE *oFp;
Int32 oStart;
CDFstatus oError;
{
  Int32 nBlocks = nBytes / COPYblockSIZE;	/* Number of full blocks that
						   can be copied. */
  Int32 lastCount = nBytes % COPYblockSIZE;	/* Number of bytes remaining
						   to be copied after the full
						   blocks have been copied. */
  Int32 i; Byte1 buffer[COPYblockSIZE];
  /****************************************************************************
  * If the copy is within the same file...
  ****************************************************************************/
  if (iFp == oFp) {
    /**************************************************************************
    * If the input group (of bytes) is before the output group start at the
    * end in case they overlap.
    **************************************************************************/
    if (iStart < oStart) {
      if (nBlocks > 0) {
	Int32 iOffset = iStart + nBytes - COPYblockSIZE;
	Int32 oOffset = oStart + nBytes - COPYblockSIZE;
	for (i = 0; i < nBlocks; i++) {
	   if (!SEEKv(iFp,(long)iOffset,vSEEK_SET)) return iError;
	   if (!READv(buffer,(size_t)COPYblockSIZE,(size_t)1,iFp)) return
								   iError;
	   if (!SEEKv(oFp,(long)oOffset,vSEEK_SET)) return oError;
	   if (!WRITEv(buffer,(size_t)COPYblockSIZE,(size_t)1,oFp)) return
								    oError;
	   iOffset -= COPYblockSIZE;
	   oOffset -= COPYblockSIZE;
	}
      }
      if (lastCount > 0) {
	if (!SEEKv(iFp,(long)iStart,vSEEK_SET)) return iError;
	if (!READv(buffer,(size_t)lastCount,(size_t)1,iFp)) return iError;
	if (!SEEKv(oFp,(long)oStart,vSEEK_SET)) return oError;
	if (!WRITEv(buffer,(size_t)lastCount,(size_t)1,oFp)) return oError;
      }
    }
    /**************************************************************************
    * If the input group (of bytes) is after the output group start at the
    * beginning in case they overlap.
    **************************************************************************/
    if (iStart > oStart) {
      Int32 iOffset = iStart;
      Int32 oOffset = oStart;
      if (nBlocks > 0) {
	for (i = 0; i < nBlocks; i++) {
	   if (!SEEKv(iFp,(long)iOffset,vSEEK_SET)) return iError;
	   if (!READv(buffer,(size_t)COPYblockSIZE,(size_t)1,iFp)) return
								   iError;
	   if (!SEEKv(oFp,(long)oOffset,vSEEK_SET)) return oError;
	   if (!WRITEv(buffer,(size_t)COPYblockSIZE,(size_t)1,oFp)) return
								    oError;
	   iOffset += COPYblockSIZE;
	   oOffset += COPYblockSIZE;
	}
      }
      if (lastCount > 0) {
	if (!SEEKv(iFp,(long)iOffset,vSEEK_SET)) return iError;
	if (!READv(buffer,(size_t)lastCount,(size_t)1,iFp)) return iError;
	if (!SEEKv(oFp,(long)oOffset,vSEEK_SET)) return oError;
	if (!WRITEv(buffer,(size_t)lastCount,(size_t)1,oFp)) return oError;
      }
    }
    /**************************************************************************
    * Offsets are the same - do nothing.
    **************************************************************************/
  }
  else {
    /**************************************************************************
    * Different files...
    **************************************************************************/
    if (!SEEKv(iFp,(long)iStart,vSEEK_SET)) return iError;
    if (!SEEKv(oFp,(long)oStart,vSEEK_SET)) return oError;
    for (i = 0; i < nBlocks; i++) {
       if (!READv(buffer,(size_t)COPYblockSIZE,(size_t)1,iFp)) return iError;
       if (!WRITEv(buffer,(size_t)COPYblockSIZE,(size_t)1,oFp)) return oError;
    }
    if (lastCount > 0) {
      if (!READv(buffer,(size_t)lastCount,(size_t)1,iFp)) return iError;
      if (!WRITEv(buffer,(size_t)lastCount,(size_t)1,oFp)) return oError;
    }
  }
  return CDF_OK;
}

/******************************************************************************
* ModIndexOffset.
******************************************************************************/

STATICforIDL CDFstatus ModIndexOffset (CDF, Var, firstRec, lastRec, newOffset)
struct CDFstruct *CDF;  /* Pointer to CDF. */
struct VarStruct *Var;  /* Pointer to variable. */
Int32 firstRec;         /* First record of entry. */
Int32 lastRec;          /* Last record of entry. */
Int32 newOffset;        /* New VVR/CVVR/SVVR offset. */
{
  CDFstatus pStatus = CDF_OK; Int32 vxrOffset;
  /****************************************************************************
  * Read offset of first VXR...
  ****************************************************************************/
  if (!sX(ReadVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
		  VDR_VXRHEAD,&vxrOffset,
		  VDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * ...and start there.
  ****************************************************************************/
  if (!sX(ModIndexOffset_r(CDF->fp,vxrOffset,
			   firstRec,lastRec,
			   newOffset),&pStatus)) return pStatus;
  return pStatus;
}

static CDFstatus ModIndexOffset_r (fp, vxrOffset, firstRec, lastRec, newOffset)
vFILE *fp;              /* File pointer to dotCDF file. */
Int32 vxrOffset;        /* VXR at which to start. */
Int32 firstRec;         /* First record of entry. */
Int32 lastRec;          /* Last record of entry. */
Int32 newOffset;        /* New VVR/CVVR/SVVR offset. */
{
  CDFstatus pStatus = CDF_OK;
  struct VXRstruct VXR; int entryN; Int32 irType;
  /****************************************************************************
  * While more VXRs...
  ****************************************************************************/
  while (vxrOffset != ZERO_OFFSET) {
    /**************************************************************************
    * Read the VXR.
    **************************************************************************/
    if (!sX(ReadVXR(fp,vxrOffset,
		    VXR_RECORD,&VXR,
		    VXR_NULL),&pStatus)) return pStatus;
    /**************************************************************************
    * Search through index entries...
    **************************************************************************/
    for (entryN = 0; entryN < VXR.NusedEntries; entryN++) {
       if (VXR.First[entryN] <= firstRec && lastRec <= VXR.Last[entryN]) {
	 if (!sX(ReadIrType(fp,
			    VXR.Offset[entryN],
			    &irType),&pStatus)) return pStatus;
	 if (irType == VXR_) return ModIndexOffset_r(fp,VXR.Offset[entryN],
						     firstRec,lastRec,
						     newOffset);
	 if (VXR.First[entryN] == firstRec && lastRec == VXR.Last[entryN]) {
	   VXR.Offset[entryN] = newOffset;
	   if (!sX(WriteVXR(fp,vxrOffset,
			    VXR_RECORD,&VXR,
			    VXR_NULL),&pStatus)) return pStatus;
	   return pStatus;
	 }
	 return CDF_INTERNAL_ERROR;             /* or CORRUPTED_V2_CDF? */
       }
    }
    /**************************************************************************
    * Go to next VXR.
    **************************************************************************/
    vxrOffset = VXR.VXRnext;
  }
  /****************************************************************************
  * No more VXRs.
  ****************************************************************************/
  return CDF_INTERNAL_ERROR;
}

/******************************************************************************
* WriteCompressedRecords.
******************************************************************************/

static CDFstatus WriteCompressedRecords (CDF, Var, firstRec, lastRec, buffer,
					 nValues, offset, fullRecord)
struct CDFstruct *CDF;  /* Pointer to CDF. */
struct VarStruct *Var;  /* Pointer to variable. */
Int32 firstRec;         /* First record being written. */
Int32 lastRec;          /* Last record being written. */
void *buffer;           /* Buffer of values. */
Int32 nValues;          /* Number of values being written. */
Int32 offset;           /* If not full record(s), byte offset in record at
			   which to write. */
Logical fullRecord;     /* Full record(s) being written? */
{
  CDFstatus pStatus = CDF_OK; Logical found;
  Int32 recCount, tOffset, recordOffsetInStage, maxRecThisBlock;
  Int32 nextRec, writeTo, numElems; void *padBuffer; int how;
  Int32 recNum = firstRec; Byte1 *tBuffer = (Byte1 *) buffer;
  /****************************************************************************
  * From first to last record...
  ****************************************************************************/
  while (recNum <= lastRec) {
     /*************************************************************************
     * Check if this record is in the staging area.
     *************************************************************************/
     if (INCLUSIVE(Var->stage.firstRec,recNum,Var->stage.lastRec)) {
       recordOffsetInStage = recNum - Var->stage.firstRec;
       tOffset = Var->stage.areaOffset;
       tOffset += (recordOffsetInStage * Var->NphyRecBytes);
       if (fullRecord) {
	 if (Var->stage.dotOffset == NO_OFFSET) {
	   maxRecThisBlock = Var->stage.firstRec + Var->blockingFactor - 1;
	   if (!sX(NextRecord(CDF,Var->VDRoffset,
			      Var->zVar,recNum,
			      &nextRec,&found),&pStatus)) return pStatus;
	   if (!found)
	     writeTo = MINIMUM(maxRecThisBlock,lastRec);
	   else {
	     Int32 prevRecN = nextRec - 1;
	     writeTo = MINIMUMof3(maxRecThisBlock,prevRecN,lastRec);
	   }
	 }
	 else
	   writeTo = MINIMUM(Var->stage.lastRec,lastRec);
	 recCount = writeTo - recNum + 1;
	 numElems = recCount * Var->NphyRecElems;
       }
       else {
	 writeTo = recNum;
	 recCount = 1;
	 numElems = nValues * Var->NvalueElems;
	 tOffset += offset;
       }
       if (!sX(WriteVarElems(Var,CDF->stage.fp,
			     tOffset,numElems,
			     tBuffer),&pStatus)) return pStatus;
       Var->stage.lastRec = MAXIMUM(Var->stage.lastRec,writeTo);
       Var->stage.modified = TRUE;
       tBuffer += (size_t) (numElems * Var->NelemBytes);
       recNum += recCount;
       continue;
     }
     /*************************************************************************
     * Not in the staging area...check if this record is in an existing CVVR.
     *************************************************************************/
     if (!sX(BringToStage(CDF,Var,recNum,&found),&pStatus)) return pStatus;
     if (found) {
       recordOffsetInStage = recNum - Var->stage.firstRec;
       tOffset = Var->stage.areaOffset;
       tOffset += (recordOffsetInStage * Var->NphyRecBytes);
       if (fullRecord) {
	 writeTo = MINIMUM(Var->stage.lastRec,lastRec);
	 recCount = writeTo - recNum + 1;
	 numElems = recCount * Var->NphyRecElems;
       }
       else {
	 writeTo = recNum;
	 recCount = 1;
	 numElems = nValues * Var->NvalueElems;
	 tOffset += offset;
       }
       if (!sX(WriteVarElems(Var,CDF->stage.fp,
			     tOffset,numElems,
			     tBuffer),&pStatus)) return pStatus;
       Var->stage.modified = TRUE;
       tBuffer += (size_t) (numElems * Var->NelemBytes);
       recNum += recCount;
       continue;
     }
     /*************************************************************************
     * Not in an existing CVVR...this record does not exist.  First check
     * if the record(s) can be added to the records in the stage.
     *************************************************************************/
     if (Var->stage.firstRec != NO_RECORD) {
       if (Var->stage.dotOffset == NO_OFFSET) {
	 if (recNum == Var->stage.lastRec + 1) {
	   maxRecThisBlock = Var->stage.firstRec + Var->blockingFactor - 1;
	   if (recNum <= maxRecThisBlock) {
	     recordOffsetInStage = recNum - Var->stage.firstRec;
	     tOffset = Var->stage.areaOffset;
	     tOffset += (recordOffsetInStage * Var->NphyRecBytes);
	     if (fullRecord) {
	       if (!sX(NextRecord(CDF,Var->VDRoffset,
				  Var->zVar,recNum,
				  &nextRec,&found),&pStatus)) return pStatus;
	       if (!found)
		 writeTo = MINIMUM(maxRecThisBlock,lastRec);
	       else {
		 Int32 prevRecN = nextRec - 1;
		 writeTo = MINIMUMof3(maxRecThisBlock,prevRecN,lastRec);
	       }
	       recCount = writeTo - recNum + 1;
	       numElems = recCount * Var->NphyRecElems;
	     }
	     else {
	       if (!sX(BuildPadBuffer(CDF,Var,INT32_ONE,
				      &how,&padBuffer,
				      TRUE),&pStatus)) return pStatus;
	       if (!sX(WritePadValues(Var,CDF->stage.fp,
				      tOffset,INT32_ONE,how,
				      padBuffer),&pStatus)) {
	         cdf_FreeMemory (padBuffer, NULL);
		 return pStatus;
	       }
	       cdf_FreeMemory (padBuffer, NULL);
	       writeTo = recNum;
	       recCount = 1;
	       numElems = nValues * Var->NvalueElems;
	       tOffset += offset;
	     }
	     if (!sX(WriteVarElems(Var,CDF->stage.fp,
				   tOffset,numElems,
				   tBuffer),&pStatus)) return pStatus;
	     Var->stage.lastRec = MAXIMUM(Var->stage.lastRec,writeTo);
	     Var->stage.modified = TRUE;
	     tBuffer += (size_t) (numElems * Var->NelemBytes);
	     recNum += recCount;
	     continue;
	   }
	 }
       }
     }
     /*************************************************************************
     * This record cannot be added to the block of records currently in
     * the staging area.  Start a new block of records.
     *************************************************************************/
     if (!sX(FlushStage(CDF,Var),&pStatus)) return pStatus;
     tOffset = Var->stage.areaOffset;
     if (fullRecord) {
       maxRecThisBlock = recNum + Var->blockingFactor - 1;
       if (!sX(NextRecord(CDF,Var->VDRoffset,Var->zVar,
			  recNum,&nextRec,&found),&pStatus)) return pStatus;
       if (!found)
	 writeTo = MINIMUM(maxRecThisBlock,lastRec);
       else {
	 Int32 prevRecN = nextRec - 1;
	 writeTo = MINIMUMof3(maxRecThisBlock,prevRecN,lastRec);
       }
       recCount = writeTo - recNum + 1;
       numElems = recCount * Var->NphyRecElems;
     }
     else {
       if (!sX(BuildPadBuffer(CDF,Var,INT32_ONE,
			      &how,&padBuffer,
			      TRUE),&pStatus)) return pStatus;
       if (!sX(WritePadValues(Var,CDF->stage.fp,
			      tOffset,INT32_ONE,how,
			      padBuffer),&pStatus)) {
         cdf_FreeMemory (padBuffer, NULL);
	 return pStatus;
       }
       cdf_FreeMemory (padBuffer, NULL);
       writeTo = recNum;
       recCount = 1;
       numElems = nValues * Var->NvalueElems;
       tOffset += offset;
     }
     if (!sX(WriteVarElems(Var,CDF->stage.fp,
			   tOffset,numElems,
			   tBuffer),&pStatus)) return pStatus;
     Var->stage.firstRec = recNum;
     Var->stage.lastRec = writeTo;
     Var->stage.dotOffset = NO_OFFSET;
     Var->stage.modified = TRUE;
     tBuffer += (size_t) (numElems * Var->NelemBytes);
     recNum += recCount;
  }
  return pStatus;
}

/******************************************************************************
* CalcCompressionPct.
******************************************************************************/

STATICforIDL CDFstatus CalcCompressionPct (CDF, vdrOffset, zVar, cPct)
struct CDFstruct *CDF;
Int32 vdrOffset;
Logical zVar;
long *cPct;
{
  CDFstatus pStatus = CDF_OK; Int32 vxrOffset;
  Int32 uTotal = 0, cTotal = 0; Int32 nPhyRecBytes;
  /****************************************************************************
  * Calculate the number of bytes per physical record.
  ****************************************************************************/
  if (!sX(CalcPhyRecBytes(CDF,vdrOffset,
			  zVar,&nPhyRecBytes),&pStatus)) return pStatus;
  /****************************************************************************
  * Read the offset of the first VXR (return 0% if no VXRs)...
  ****************************************************************************/
  if (!sX(ReadVDR(CDF,CDF->fp,vdrOffset,zVar,
		  VDR_VXRHEAD,&vxrOffset,
		  VDR_NULL),&pStatus)) return pStatus;
  if (vxrOffset == ZERO_OFFSET) {
    *cPct = 0;
    return pStatus;
  }
  /****************************************************************************
  * ...and start there.
  ****************************************************************************/
  if (!sX(CalcCompressionPct_r(CDF->fp,vxrOffset,
			       nPhyRecBytes,
			       &uTotal,&cTotal),&pStatus)) return pStatus;
  /****************************************************************************
  * Calculate percentage.
  ****************************************************************************/
  *cPct = (long) (((100.0*cTotal) / uTotal) + 0.5);
  return pStatus;
}

static CDFstatus CalcCompressionPct_r (fp, vxrOffset, nPhyRecBytes, uTotal,
				       cTotal)
vFILE *fp;
Int32 vxrOffset;
Int32 nPhyRecBytes;
Int32 *uTotal;
Int32 *cTotal;
{
  CDFstatus pStatus = CDF_OK; struct VXRstruct VXR; int entryN;
  Int32 nRecords, uSize, irType, irSize;
  /****************************************************************************
  * While more VXRs...
  ****************************************************************************/
  while (vxrOffset != ZERO_OFFSET) {
    /**************************************************************************
    * Read the VXR.
    **************************************************************************/
    if (!sX(ReadVXR(fp,vxrOffset,
		    VXR_RECORD,&VXR,
		    VXR_NULL),&pStatus)) return pStatus;
    /**************************************************************************
    * Scan entries...
    **************************************************************************/
    for (entryN = 0; entryN < VXR.NusedEntries; entryN++) {
       if (!sX(ReadIrType(fp,VXR.Offset[entryN],&irType),&pStatus)) return
								    pStatus;
       switch (irType) {
	 case VXR_:
	   if (!sX(CalcCompressionPct_r(fp,VXR.Offset[entryN],
					nPhyRecBytes,uTotal,
					cTotal),&pStatus)) return pStatus;
	   break;
	 case VVR_:
	 case CVVR_:
	   /*******************************************************************
	   * Accumulate uncompressed size.
	   *******************************************************************/
	   nRecords = VXR.Last[entryN] - VXR.First[entryN] + 1;
	   uSize = nRecords * nPhyRecBytes;
	   *uTotal += uSize;
	   /*******************************************************************
	   * Accumulate compressed size.
	   *******************************************************************/
	   if (!sX(ReadIrSize(fp,VXR.Offset[entryN],&irSize),&pStatus)) return
								       pStatus;
	   *cTotal += irSize - BOO(irType == CVVR_,
				   CVVR_BASE_SIZE,
				   VVR_BASE_SIZE);
	   break;
	 default:
	   return CORRUPTED_V2_CDF;
       }
    }
    vxrOffset = VXR.VXRnext;
  }
  return pStatus;
}

/******************************************************************************
* CalcPhyRecBytes.
******************************************************************************/

STATICforIDL CDFstatus CalcPhyRecBytes (CDF, vdrOffset, zVar, nPhyRecBytes)
struct CDFstruct *CDF;
Int32 vdrOffset;
Logical zVar;
Int32 *nPhyRecBytes;
{
  CDFstatus pStatus = CDF_OK; int dimN; Int32 dataType, numElems;
  Int32 numDims, dimSizes[CDF_MAX_DIMS], dimVarys[CDF_MAX_DIMS];
  if (!sX(CalcDimParms(CDF,vdrOffset,zVar,
		       &numDims,dimSizes,dimVarys),&pStatus)) return pStatus;
  if (!sX(ReadVDR(CDF,CDF->fp,vdrOffset,zVar,
		  VDR_DATATYPE,&dataType,
		  VDR_NUMELEMS,&numElems,
		  VDR_NULL),&pStatus)) return pStatus;
  *nPhyRecBytes = (Int32) CDFelemSize((long)dataType) * numElems;
  for (dimN = 0; dimN < numDims; dimN++) {
     if (dimVarys[dimN]) *nPhyRecBytes *= dimSizes[dimN];
  }
  return pStatus;
}

/******************************************************************************
* ReadSparseFull.
******************************************************************************/

static CDFstatus ReadSparseFull (CDF, Var, firstRec, lastRec, buffer)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 firstRec;
Int32 lastRec;
void *buffer;
{
  CDFstatus pStatus = CDF_OK; Logical found;
  Int32 readTo, recCount, numElems, recX, prevRecN;
  Int32 nextRec, padTo, tOffset, nPadValues;
  Int32 recNum = firstRec;
  Byte1 *tBuffer = (Byte1 *) buffer;
  /****************************************************************************
  * While there are more records to be read/generated...
  ****************************************************************************/
  while (recNum <= lastRec) {
    /**************************************************************************
    * Is the record in the staging area?
    **************************************************************************/
    if (INCLUSIVE(Var->stage.firstRec,recNum,Var->stage.lastRec)) {
      readTo = MINIMUM(Var->stage.lastRec,lastRec);
      recCount = readTo - recNum + 1;
      numElems = recCount * Var->NphyRecElems;
      recX = recNum - Var->stage.firstRec;
      tOffset = Var->stage.areaOffset + (recX * Var->NphyRecBytes);
      if (!sX(ReadVarElems(Var,CDF->stage.fp,
			   tOffset,numElems,tBuffer),&pStatus)) return pStatus;
      tBuffer += (size_t) (recCount * Var->NphyRecBytes);
      recNum += recCount;
      continue;
    }
    /**************************************************************************
    * Does the record exist and has been written to (not just allocated)?
    **************************************************************************/
    if (recNum <= Var->maxRec) {
      Int32 firstRecInVVR, lastRecInVVR;
      if (!sX(SearchForRecord(CDF,Var->VDRoffset,
			      Var->zVar,recNum,
			      &firstRecInVVR,
			      &lastRecInVVR,
			      &tOffset,&found),&pStatus)) return pStatus;
      if (found) {
	readTo = MINIMUMof3(Var->maxRec,lastRec,lastRecInVVR);
	recCount = readTo - recNum + 1;
	numElems = recCount * Var->NphyRecElems;
	recX = recNum - firstRecInVVR;
	tOffset += VVR_BASE_SIZE + (recX * Var->NphyRecBytes);
	if (!sX(ReadVarElems(Var,CDF->fp,tOffset,
			     numElems,tBuffer),&pStatus)) return pStatus;
	tBuffer += (size_t) (recCount * Var->NphyRecBytes);
	recNum += recCount;
	continue;
      }
    }
    /**************************************************************************
    * Determine which records need to be padded.
    **************************************************************************/
    if (recNum <= Var->maxRec) {
      if (!sX(NextRecord(CDF,Var->VDRoffset,Var->zVar,
			 recNum,&nextRec,&found),&pStatus)) return pStatus;
      if (found) {
	if (EXCLUSIVE(recNum,
		      Var->stage.firstRec,
		      nextRec)) nextRec = Var->stage.firstRec;
	prevRecN = nextRec - 1;
	padTo = MINIMUM(prevRecN,lastRec);
      }
      else {
	prevRecN = Var->stage.firstRec - 1;
	padTo = MINIMUM(prevRecN,lastRec);
      }
    }
    else
      padTo = lastRec;
    recCount = padTo - recNum + 1;
    /**************************************************************************
    * If sRecords.PREV...
    **************************************************************************/
    if (Var->prevIfMissing) {
      Int32 prevRec; Byte1 *destBuffer;
      if (!sX(PrevRecord(CDF,Var->VDRoffset,Var->zVar,
			 MINIMUM(recNum,Var->maxRec),
			 &prevRec,&found),&pStatus)) return pStatus;
      if (found) {
	if (EXCLUSIVE(prevRec,Var->stage.lastRec,recNum)) {
	  recX = Var->stage.lastRec - Var->stage.firstRec;
	  tOffset = Var->stage.areaOffset + (recX * Var->NphyRecBytes);
	  if (!sX(ReadVarElems(Var,CDF->stage.fp,
			       tOffset,Var->NphyRecElems,
			       tBuffer),&pStatus)) return pStatus;
	}
	else {
	  if (!sX(RecordByteOffset(CDF,Var,
				   prevRec,
				   &tOffset),&pStatus)) return pStatus;
	  if (!sX(ReadVarElems(Var,CDF->fp,tOffset,
			       Var->NphyRecElems,
			       tBuffer),&pStatus)) return pStatus;
	}
	destBuffer = tBuffer + ((size_t) Var->NphyRecBytes);
	for (recX = 1; recX < recCount; recX++) {
	   memmove (destBuffer, tBuffer, (size_t) Var->NphyRecBytes);
	   destBuffer += (size_t) Var->NphyRecBytes;
	}
	sX (VIRTUAL_RECORD_DATA, &pStatus);
	tBuffer += (size_t) (recCount * Var->NphyRecBytes);
	recNum += recCount;
	continue;
      }
      if (EXCLUSIVE(NO_RECORD,Var->stage.lastRec,recNum)) {
	recX = Var->stage.lastRec - Var->stage.firstRec;
	tOffset = Var->stage.areaOffset + (recX * Var->NphyRecBytes);
	if (!sX(ReadVarElems(Var,CDF->stage.fp,
			     tOffset,Var->NphyRecElems,
			     tBuffer),&pStatus)) return pStatus;
	destBuffer = tBuffer + ((size_t) Var->NphyRecBytes);
	for (recX = 1; recX < recCount; recX++) {
	   memmove (destBuffer, tBuffer, (size_t) Var->NphyRecBytes);
	   destBuffer += (size_t) Var->NphyRecBytes;
	}
	sX (VIRTUAL_RECORD_DATA, &pStatus);
	tBuffer += (size_t) (recCount * Var->NphyRecBytes);
	recNum += recCount;
	continue;
      }
    }
    /**************************************************************************
    * Pad with the variable's pad value.  This occurs if the variable is
    * sRecords.NO, sRecords.PAD, or if sRecords.PREV but a previous record
    * does not exist (in a VVR or the staging area).
    **************************************************************************/
    nPadValues = recCount * Var->NphyRecValues;
    if (!sX(PadBuffer(CDF,Var,nPadValues,tBuffer),&pStatus)) return pStatus;
    sX (VIRTUAL_RECORD_DATA, &pStatus);
    tBuffer += (size_t) (recCount * Var->NphyRecBytes);
    recNum += recCount;
  }
  return pStatus;
}

/******************************************************************************
* ReadSparsePartial.
******************************************************************************/

static CDFstatus ReadSparsePartial (CDF, Var, recNum, offset, nValues, buffer)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 recNum;
Int32 offset;
Int32 nValues;
void *buffer;
{
  CDFstatus pStatus = CDF_OK; Logical found;
  Int32 tOffset, firstRec, prevRec;
  Int32 numElems = nValues * Var->NvalueElems;
  /****************************************************************************
  * If the record is in the staging area...
  ****************************************************************************/
  if (INCLUSIVE(Var->stage.firstRec,recNum,Var->stage.lastRec)) {
    tOffset = Var->stage.areaOffset;
    tOffset += Var->NphyRecBytes * (recNum - Var->stage.firstRec);
    tOffset += offset;
    if (!sX(ReadVarElems(Var,CDF->stage.fp,
			 tOffset,numElems,buffer),&pStatus)) return pStatus;
    return pStatus;
  }
  /****************************************************************************
  * If the record exists and has been written (ie. not just allocated)...
  ****************************************************************************/
  if (recNum <= Var->maxRec) {
    if (!sX(SearchForRecord(CDF,Var->VDRoffset,
			    Var->zVar,recNum,&firstRec,
			    NULL,&tOffset,&found),&pStatus)) return pStatus;
    if (found) {
      tOffset += VVR_BASE_SIZE;
      tOffset += Var->NphyRecBytes * (recNum - firstRec);
      tOffset += offset;
      if (!sX(ReadVarElems(Var,CDF->fp,
			   tOffset,numElems,buffer),&pStatus)) return pStatus;
      return pStatus;
    }
  }
  /****************************************************************************
  * If missing records are to be read from the previous record...
  ****************************************************************************/
  if (Var->prevIfMissing) {
    /**************************************************************************
    * Check if a previous record exists in a VVR.
    **************************************************************************/
    if (!sX(PrevRecord(CDF,Var->VDRoffset,Var->zVar,
		       MINIMUM(recNum,Var->maxRec),
		       &prevRec,&found),&pStatus)) return pStatus;
    /**************************************************************************
    * If so, also make sure that the last record in the staging area isn't
    * really the previous record to use.
    **************************************************************************/
    if (found) {
      if (EXCLUSIVE(prevRec,Var->stage.lastRec,recNum)) {
	Int32 recNumInStage = Var->stage.lastRec - Var->stage.firstRec;
	tOffset = Var->stage.areaOffset;
	tOffset += Var->NphyRecBytes * recNumInStage;
	tOffset += offset;
	if (!sX(ReadVarElems(Var,CDF->stage.fp,tOffset,
			     numElems,buffer),&pStatus)) return pStatus;
      }
      else {
	if (!sX(RecordByteOffset(CDF,Var,
				 prevRec,
				 &tOffset),&pStatus)) return pStatus;
	tOffset += offset;
	if (!sX(ReadVarElems(Var,CDF->fp,tOffset,
			     numElems,buffer),&pStatus)) return pStatus;
      }
      sX (VIRTUAL_RECORD_DATA, &pStatus);
      return pStatus;
    }
    /**************************************************************************
    * A previous record does not exist in a VVR...check if the last record in
    * the staging area is the previous record.
    **************************************************************************/
    if (EXCLUSIVE(NO_RECORD,Var->stage.lastRec,recNum)) {
      Int32 recNumInStage = Var->stage.lastRec - Var->stage.firstRec;
      tOffset = Var->stage.areaOffset;
      tOffset += Var->NphyRecBytes * recNumInStage;
      tOffset += offset;
      if (!sX(ReadVarElems(Var,CDF->stage.fp,
			   tOffset,numElems,buffer),&pStatus)) return pStatus;
      sX (VIRTUAL_RECORD_DATA, &pStatus);
      return pStatus;
    }
  }
  /****************************************************************************
  * Pad the buffer with the variable's pad value.  Note that this is also done
  * if the variable is sRecords.PREV but a previous record does not exist.
  ****************************************************************************/
  if (!sX(PadBuffer(CDF,Var,nValues,buffer),&pStatus)) return pStatus;
  sX (VIRTUAL_RECORD_DATA, &pStatus);
  return pStatus;
}

/******************************************************************************
* ReadCompressedFull.
******************************************************************************/

static CDFstatus ReadCompressedFull (CDF, Var, firstRec, lastRec, buffer)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 firstRec;
Int32 lastRec;
void *buffer;
{
  CDFstatus pStatus = CDF_OK; Logical found;
  Int32 readTo, recCount, numElems, recX, tOffset, nextRec;
  Int32 recNum = firstRec, nPadValues, prevRec, padTo;
  Byte1 *tBuffer = (Byte1 *) buffer, *destBuffer;
  /****************************************************************************
  * While there are more records to be read/generated...
  ****************************************************************************/
  while (recNum <= lastRec) {
    /**************************************************************************
    * Try to bring the record to the staging area.
    **************************************************************************/
    if (!sX(BringToStage(CDF,Var,recNum,&found),&pStatus)) return pStatus;
    if (found) {
      readTo = MINIMUM(Var->stage.lastRec,lastRec);
      recCount = readTo - recNum + 1;
      numElems = recCount * Var->NphyRecElems;
      recX = recNum - Var->stage.firstRec;
      tOffset = Var->stage.areaOffset + (recX * Var->NphyRecBytes);
      if (!sX(ReadVarElems(Var,CDF->stage.fp,
			   tOffset,numElems,tBuffer),&pStatus)) return pStatus;
      tBuffer += (size_t) (recCount * Var->NphyRecBytes);
      recNum += recCount;
      continue;
    }
    /**************************************************************************
    * Determine which records need to be padded.
    **************************************************************************/
    if (!sX(NextRecord(CDF,Var->VDRoffset,Var->zVar,
		       recNum,&nextRec,&found),&pStatus)) return pStatus;
    if (found) {
      if (EXCLUSIVE(recNum,
		    Var->stage.firstRec,
		    nextRec)) nextRec = Var->stage.firstRec;
      prevRec = nextRec - 1;
      padTo = MINIMUM(prevRec,lastRec);
    }
    else
      padTo = BOO(Var->stage.firstRec > recNum,
		  Var->stage.firstRec - 1,lastRec);
    recCount = padTo - recNum + 1;
    /**************************************************************************
    * If sRecords.PREV...
    **************************************************************************/
    if (Var->prevIfMissing) {
      if (!sX(PrevRecord(CDF,Var->VDRoffset,Var->zVar,
			 recNum,&prevRec,&found),&pStatus)) return pStatus;
      if (!found) prevRec = NO_RECORD;
      if (EXCLUSIVE(prevRec,
		    Var->stage.lastRec,
		    recNum)) prevRec = Var->stage.lastRec;
      if (prevRec > NO_RECORD) {
	if (!sX(BringToStage(CDF,Var,prevRec,NULL),&pStatus)) return pStatus;
	recX = prevRec - Var->stage.firstRec;
	tOffset = Var->stage.areaOffset + (recX * Var->NphyRecBytes);
	if (!sX(ReadVarElems(Var,CDF->stage.fp,
			     tOffset,Var->NphyRecElems,
			     tBuffer),&pStatus)) return pStatus;
	destBuffer = tBuffer + ((size_t) Var->NphyRecBytes);
	for (recX = 1; recX < recCount; recX++) {
	   memmove (destBuffer, tBuffer, (size_t) Var->NphyRecBytes);
	   destBuffer += (size_t) Var->NphyRecBytes;
	}
	sX (VIRTUAL_RECORD_DATA, &pStatus);
	tBuffer += (size_t) (recCount * Var->NphyRecBytes);
	recNum += recCount;
	continue;
      }
    }
    /**************************************************************************
    * Pad with the variable's pad value.  This occurs if the variable is
    * sRecords.NO, sRecords.PAD, or if sRecords.PREV but a previous record
    * does not exist (in a CVVR/VVR or the staging area).
    **************************************************************************/
    nPadValues = recCount * Var->NphyRecValues;
    if (!sX(PadBuffer(CDF,Var,nPadValues,tBuffer),&pStatus)) return pStatus;
    sX (VIRTUAL_RECORD_DATA, &pStatus);
    tBuffer += (size_t) (recCount * Var->NphyRecBytes);
    recNum += recCount;
  }
  return pStatus;
}

/******************************************************************************
* ReadCompressedPartial.
******************************************************************************/

static CDFstatus ReadCompressedPartial (CDF, Var, recNum, offset, nValues,
					buffer)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 recNum;
Int32 offset;
Int32 nValues;
void *buffer;
{
  CDFstatus pStatus = CDF_OK;
  Int32 numElems = nValues * Var->NvalueElems;
  Int32 tOffset, prevRec, recX; Logical found;
  /****************************************************************************
  * Try to bring the record to the staging area.
  ****************************************************************************/
  if (!sX(BringToStage(CDF,Var,recNum,&found),&pStatus)) return pStatus;
  if (found) {
    tOffset = Var->stage.areaOffset;
    tOffset += Var->NphyRecBytes * (recNum - Var->stage.firstRec);
    tOffset += offset;
    if (!sX(ReadVarElems(Var,CDF->stage.fp,
			 tOffset,numElems,buffer),&pStatus)) return pStatus;
    return pStatus;
  }
  /****************************************************************************
  * If sRecords.PREV...
  ****************************************************************************/
  if (Var->prevIfMissing) {
    if (!sX(PrevRecord(CDF,Var->VDRoffset,
		       Var->zVar,recNum,
		       &prevRec,&found),&pStatus)) return pStatus;
    if (!found) prevRec = NO_RECORD;
    if (EXCLUSIVE(prevRec,
		  Var->stage.lastRec,
		  recNum)) prevRec = Var->stage.lastRec;
    if (prevRec > NO_RECORD) {
      if (!sX(BringToStage(CDF,Var,prevRec,NULL),&pStatus)) return pStatus;
      recX = Var->stage.lastRec - Var->stage.firstRec;
      tOffset = Var->stage.areaOffset + (Var->NphyRecBytes * recX) + offset;
      if (!sX(ReadVarElems(Var,CDF->stage.fp,
			   tOffset,numElems,buffer),&pStatus)) return pStatus;
      sX (VIRTUAL_RECORD_DATA, &pStatus);
      return pStatus;
    }
  }
  /****************************************************************************
  * Pad the buffer with the variable's pad value.  Note that this is also done
  * if the variable is sRecords.PREV but a previous record does not exist.
  ****************************************************************************/
  if (!sX(PadBuffer(CDF,Var,nValues,buffer),&pStatus)) return pStatus;
  sX (VIRTUAL_RECORD_DATA, &pStatus);
  return pStatus;
}

#if defined(MSVC67)
/******************************************************************************
* Replace _ftol2 with _ftol on Windows as VC 6.0 doesn't have a such function.
******************************************************************************/

VISIBLE_PREFIX long _ftol2(dblSource)
double dblSource;
{
	return (long) _ftol( dblSource );
}
#endif

/******************************************************************************
* UTF8StrLength - Find the actual character number of an UTF-8 string.
******************************************************************************/

VISIBLE_PREFIX int UTF8StrLength (string)
unsigned char *string;
{
  int i = 0;
  int cnt = 0;
  while (string[i]) {
    if ((string[i] & 0xC0) != 0x80) cnt++;
    ++i;
  }
  return cnt;
}


