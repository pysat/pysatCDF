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
*   V2.0  29-Jun-04, M Liu      Added LFS (Large File Support > 2G).
*   V3.2  25-Apr-07, D Berger   Changed COPYblockSIZE from 512.
*   V3.3  10-Jan-09, M Liu      Added CDFsetValidate and CDFgetValidate.
*
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"
#define  REWR_MAX 2147483647
/******************************************************************************
* Macros/function prototypes.
******************************************************************************/

#define COPYblockSIZE nCACHE_BUFFER_BYTEs	

static CDFstatus SearchForRecord_r_64 PROTOARGs((
  vFILE *fp, OFF_T vxrOffset, Int32 recNum, Int32 *firstRec, Int32 *lastRec,
  OFF_T *offset, Logical *found
));
static CDFstatus IndexingStatistics_r_64 PROTOARGs((
  vFILE *fp, OFF_T vxrOffset, Int32 maxRec, int level, Int32 *nLevels,
  Int32 *nVXRs, Int32 *nEntries, Int32 *nAlloc, Int32 *nRecords
));
static CDFstatus PrevRecord_r_64 PROTOARGs((
  vFILE *fp, OFF_T vxrOffset, Int32 baseRec, Int32 *prevRec, Logical *found
));
static CDFstatus NextRecord_r_64 PROTOARGs((
  vFILE *fp, OFF_T vxrOffset, Int32 baseRec, Int32 *nextRec, Logical *found
));
static CDFstatus CalcCompressionPct_r_64 PROTOARGs((
  vFILE *fp, OFF_T vxrOffset, Int32 nPhyRecBytes, OFF_T *uTotal, OFF_T *cTotal
));
static CDFstatus ModIndexOffset_r_64 PROTOARGs((
  vFILE *fp, OFF_T vxrOffset, Int32 firstRec, Int32 lastRec, OFF_T newOffset
));
static CDFstatus ReadSparseFull PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 firstRec, Int32 lastRec,
  void *buffer
));
static CDFstatus ReadSparsePartial PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 recNum, OFF_T offset,
  OFF_T nValues, void *buffer
));
static CDFstatus ReadCompressedFull PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 firstRec, Int32 lastRec,
  void *buffer
));
static CDFstatus ReadCompressedPartial PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 recNum, OFF_T offset,
  OFF_T nValues, void *buffer
));
static CDFstatus BringToStage PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 recNum, Logical *found
));
static CDFstatus WriteCompressedRecords PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 firstRec, Int32 lastRec,
  void *buffer, OFF_T nValues, OFF_T offset, Logical fullRecord
));

/******************************************************************************
* DecompressCDF64.
******************************************************************************/

STATICforIDL CDFstatus DecompressCDF64 (dotFp, uDotFp)
vFILE *dotFp;           /* In: File pointer to dotCDF file. */
vFILE *uDotFp;          /* In: Uncompressed CDF file pointer. */
{
  CDFstatus pStatus = CDF_OK; struct CCRstruct64 CCR; struct CPRstruct64 CPR;
  uInt32 magicNumber1 = V3magicNUMBER_1, magicNumber2u = V2magicNUMBER_2u;
  OFF_T cSize, cOffset;
  /****************************************************************************
  * Read/validate CCR.
  ****************************************************************************/
  if (!sX(ReadCCR64(dotFp,V3_CCR_OFFSET64,
		    CCR_RECORD,&CCR,
		    CCR_NULL),&pStatus)) return pStatus;
  if (CCR.uSize == 0) return EMPTY_COMPRESSED_CDF;
  /****************************************************************************
  * Read CPR.
  ****************************************************************************/
  if (!sX(ReadCPR64(dotFp,CCR.CPRoffset,
		    CPR_RECORD,&CPR,
		    CPR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * Write magic numbers.
  ****************************************************************************/
  if (!SEEKv64(uDotFp,(OFF_T) V3_MAGIC_OFFSET_1,vSEEK_SET)) return CDF_WRITE_ERROR;
  if (!Write32_64(uDotFp,(Int32 *)&magicNumber1)) return CDF_WRITE_ERROR;
  if (!Write32_64(uDotFp,(Int32 *)&magicNumber2u)) return CDF_WRITE_ERROR;
  /****************************************************************************
  * Copy/decompress.
  ****************************************************************************/
  cOffset = (OFF_T) (V3_CCR_OFFSET64 + CCR_BASE_SIZE64);
  cSize = CCR.RecordSize - CCR_BASE_SIZE64;
  if (!sX(Decompress64(dotFp,cOffset,
		       cSize,CDF_READ_ERROR,
		       CPR.cType,CPR.cParms,
		       uDotFp,FIRST_IR_OFFSET,
		       CDF_WRITE_ERROR),&pStatus)) return pStatus;
  return pStatus;
}

/******************************************************************************
* WriteCompressedCDF64.
******************************************************************************/

STATICforIDL CDFstatus WriteCompressedCDF64 (CDF, CPR, empty)
struct CDFstruct *CDF;
struct CPRstruct64 *CPR;
Logical empty;          /* In: If TRUE, write an empty CCR. */
{
  uInt32 magicNumber1 = V3magicNUMBER_1;
  uInt32 magicNumber2c = V2magicNUMBER_2c;
  struct CCRstruct64 CCR; CDFstatus pStatus = CDF_OK;
  /****************************************************************************
  * Write magic numbers.
  ****************************************************************************/
  if (!SEEKv64(CDF->dotFp,(OFF_T) V3_MAGIC_OFFSET_1,vSEEK_SET)) return CDF_WRITE_ERROR;
  if (!Write32_64(CDF->dotFp,(Int32 *)&magicNumber1)) return CDF_WRITE_ERROR;
  if (!Write32_64(CDF->dotFp,(Int32 *)&magicNumber2c)) return CDF_WRITE_ERROR;
  /****************************************************************************
  * Write CCR.
  ****************************************************************************/
  if (empty) {
    CCR.RecordSize = CCR_BASE_SIZE64;
    CCR.RecordType = CCR_;
    CCR.CPRoffset = (OFF_T) V3_CCR_OFFSET64 + CCR.RecordSize;
    CCR.uSize = 0;
    CCR.rfuA = 0;
    if (!sX(WriteCCR64(CDF->dotFp,V3_CCR_OFFSET64,
		       CCR_RECORD,&CCR,
		       CCR_NULL),&pStatus)) return CDF_WRITE_ERROR;
  }
  else {
    OFF_T uSize, eof, cSize, cOffset, GDRoffset;
    if (!sX(ReadCDR64(CDF->uDotFp,V3_CDR_OFFSET64,
		      CDR_GDROFFSET,&GDRoffset,
		      CDR_NULL),&pStatus)) return pStatus;
    if (!sX(ReadGDR64(CDF->uDotFp,GDRoffset,
		      GDR_EOF,&eof,
		      GDR_NULL),&pStatus)) return pStatus;
    uSize = eof - FIRST_IR_OFFSET;
    cOffset = (OFF_T) (V3_CCR_OFFSET64 + CCR_BASE_SIZE64);
    if (!sX(Compress64(CDF->uDotFp,FIRST_IR_OFFSET,
		       uSize,CDF_READ_ERROR,CPR->cType,
		       CPR->cParms,CDF->dotFp,cOffset,
		       &cSize,CDF_WRITE_ERROR),&pStatus)) return pStatus;
    CCR.RecordSize = (OFF_T) (CCR_BASE_SIZE64 + cSize);
    CCR.RecordType = CCR_;
    CCR.CPRoffset = (OFF_T) (V3_CCR_OFFSET64 + CCR.RecordSize);
    CCR.uSize = uSize;
    CCR.rfuA = 0;
    if (!sX(WriteCCR64(CDF->dotFp,V3_CCR_OFFSET64,
		       CCR_RECORD,&CCR,
		       CCR_NULL),&pStatus)) return CDF_WRITE_ERROR;
  }
  /****************************************************************************
  * Write CPR.
  ****************************************************************************/
  if (!sX(WriteCPR64(CDF->dotFp,CCR.CPRoffset,
		     CPR_RECORD,CPR,
		     CPR_NULL),&pStatus)) return CDF_WRITE_ERROR;
  return pStatus;
}

/******************************************************************************
* CopyCDF64.
******************************************************************************/

STATICforIDL CDFstatus CopyCDF64 (srcFp, destFp)
vFILE *srcFp;
vFILE *destFp;
{
  OFF_T nBytes, offset; Byte1 buffer[nCACHE_BUFFER_BYTEs];
  CDFstatus pStatus = CDF_OK;
  if (!SEEKv64(srcFp,(OFF_T)0,vSEEK_END)) return CDF_READ_ERROR;
  nBytes = V_tell64 (srcFp);
  if (nBytes == EOF) return CDF_READ_ERROR;
  if (!SEEKv64(srcFp,(OFF_T)0,vSEEK_SET)) return CDF_READ_ERROR;
  if (!SEEKv64(destFp,(OFF_T)0,vSEEK_SET)) return CDF_WRITE_ERROR;
  for (offset = 0; offset < nBytes; offset += nCACHE_BUFFER_BYTEs) {
     OFF_T nBytesRemaining = nBytes - offset;
     size_t count = (size_t) MINIMUM (nBytesRemaining, nCACHE_BUFFER_BYTEs);
     if (!READv64(buffer,count,1,srcFp)) return CDF_READ_ERROR;
     if (!WRITEv64(buffer,count,1,destFp)) return CDF_WRITE_ERROR;
  }
  return pStatus;
}

/******************************************************************************
* WriteVarValues64.
*    NOTE: If more than one record is being written, full records are assumed
* and the `offset' must be zero.
******************************************************************************/

STATICforIDL CDFstatus WriteVarValues64 (CDF, Var, startRec, offset, nValues,
				         buffer)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 startRec;          /* Physical record number at which to write. */
Int32 offset;            /* Byte offset within (first) record at which to
			    begin writing. */
OFF_T nValues;           /* Number of values to write. */
void *buffer;
{
  OFF_T tOffset; Logical fullRecord; Byte1 *tBuffer = buffer;
  Int32 firstRec, lastRec, lastRecInVVR, nBytes, padTo, recNum;
  Int32 writeTo, recCount; CDFstatus pStatus = CDF_OK;
  OFF_T numElems;
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
	LoadAllocVVR64 (alloc, Var->maxAllocated + 1,
		        Var->maxAllocated + nRecords, FALSE)
	if (!sX(AllocateRecords64(CDF,Var,alloc),&pStatus)) return pStatus;
	Var->maxAllocated = alloc.last;
      }
      padTo = BOO(fullRecord,firstRec - 1,firstRec);
      if (padTo > Var->maxWritten) {
	Int32 padFrom = Var->maxWritten + 1;
	if (!sX(PadUnRecords64(CDF,Var,padFrom,padTo),&pStatus)) return pStatus;
	Var->maxWritten = padTo;
      }
      /************************************************************************
      * Write value(s).
      ************************************************************************/
      if (fullRecord) {
	recNum = firstRec;
	while (recNum <= lastRec) {
	  if (!sX(SearchForRecord64(CDF,Var->VDRoffset64,
				    Var->zVar,recNum,
				    NULL,&lastRecInVVR,
				    NULL,NULL),&pStatus)) return pStatus;
	  writeTo = MINIMUM(lastRec,lastRecInVVR);
	  recCount = writeTo - recNum + 1;
	  if (!sX(RecordByteOffset64(CDF,Var,
				     recNum,
				     &tOffset),&pStatus)) return pStatus;
	  numElems = (OFF_T) recCount * Var->NphyRecElems;
	  if (!sX(WriteVarElems64(Var,CDF->fp,tOffset,
				  numElems,tBuffer),&pStatus)) return pStatus;
	  recNum += recCount;
	  tBuffer += (size_t) (recCount * Var->NphyRecBytes);
	}
      }
      else {
	if (!sX(RecordByteOffset64(CDF,Var,
				   firstRec,
				   &tOffset),&pStatus)) return pStatus;
	tOffset += (OFF_T) offset;
	numElems = nValues * Var->NvalueElems;
	if (!sX(WriteVarElems64(Var,CDF->fp,tOffset,
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
	if (!sX(PadUnRecords64(CDF,Var,padFrom,padTo),&pStatus)) return pStatus;
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
	if (!sX(SearchForRecord64(CDF,Var->VDRoffset64,
				  Var->zVar,recNum,
				  NULL,&lastRecInVVR,
				  NULL,&found),&pStatus)) return pStatus;
	if (found) {
	  writeTo = MINIMUM(lastRec,lastRecInVVR);
	  recCount = writeTo - recNum + 1;
	  if (!sX(RecordByteOffset64(CDF,Var,
				     recNum,&tOffset),&pStatus)) return pStatus;
	  if (fullRecord)
	    numElems = (OFF_T) recCount * Var->NphyRecElems;
	  else {
	    tOffset += (OFF_T) offset;
	    numElems = nValues * Var->NvalueElems;
	  }
	  if (!sX(WriteVarElems64(Var,CDF->fp,tOffset,
				  numElems,tBuffer),&pStatus)) return pStatus;
	  Var->maxWritten = MAXIMUM(Var->maxWritten,writeTo);
	  recNum += recCount;
	  tBuffer += (size_t) (numElems * Var->NelemBytes);
	  continue;
	}
	/**********************************************************************
	* This record doesn't exist - initialize the staging area.
	**********************************************************************/
	if (Var->stage.areaOffset64 == (OFF_T) NO_OFFSET64) {
	  nBytes = Var->blockingFactor * Var->NphyRecBytes;
	  if (!sX(InitVarStage64(CDF,Var,nBytes),&pStatus)) return pStatus;
	}
	/**********************************************************************
	* Check if this record is in the staging area.
	**********************************************************************/
	if (INCLUSIVE(Var->stage.firstRec,recNum,Var->stage.lastRec)) {
	  maxRecInStage = Var->stage.firstRec + Var->blockingFactor - 1;
	  writeTo = MINIMUM(lastRec,maxRecInStage);
	  if (!sX(NextRecord64(CDF,Var->VDRoffset64,Var->zVar,
			       recNum,&nextRec,&found),&pStatus)) return pStatus;
	  if (found) {
	    Int32 prevRecN = nextRec - 1;
	    writeTo = MINIMUM(writeTo,prevRecN);
	  }
	  recCount = writeTo - recNum + 1;
	  recordOffsetInStage = recNum - Var->stage.firstRec;
	  tOffset = Var->stage.areaOffset64;
	  tOffset += (recordOffsetInStage * Var->NphyRecBytes);
	  if (fullRecord)
	    numElems = (OFF_T) recCount * Var->NphyRecElems;
	  else {
	    tOffset += (OFF_T) offset;
	    numElems = nValues * Var->NvalueElems;
	  }
	  if (!sX(WriteVarElems64(Var,CDF->stage.fp,tOffset,
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
	      if (!sX(NextRecord64(CDF,Var->VDRoffset64,
				   Var->zVar,recNum,
				   &nextRec,&found),&pStatus)) return pStatus;
	      if (found) {
		Int32 prevRecN = nextRec - 1;
		writeTo = MINIMUM(writeTo,prevRecN);
	      }
	      recCount = writeTo - recNum + 1;
	      recordOffsetInStage = recNum - Var->stage.firstRec;
	      tOffset = Var->stage.areaOffset64;
	      tOffset += (recordOffsetInStage * Var->NphyRecBytes);
	      if (fullRecord)
		numElems = (OFF_T) recCount * Var->NphyRecElems;
	      else {
		if (!sX(BuildPadBuffer64(CDF,Var,recCount,
				         &how,&padBuffer,
				         TRUE),&pStatus)) return pStatus;
		if (!sX(WritePadValues64(Var,CDF->stage.fp,tOffset,
				         recCount,how,padBuffer),&pStatus)) {
		  cdf_FreeMemory (padBuffer, NULL);
		  return pStatus;
		}
		cdf_FreeMemory (padBuffer, NULL);
		tOffset += (OFF_T) offset;
		numElems = nValues * Var->NvalueElems;
	      }
	      if (!sX(WriteVarElems64(Var,CDF->stage.fp,
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
	if (!sX(FlushStage64(CDF,Var),&pStatus)) return pStatus;
	/**********************************************************************
	* ...and then start a new staging area with this record.
	**********************************************************************/
	maxRecInStage = recNum + Var->blockingFactor - 1;
	writeTo = MINIMUM(lastRec,maxRecInStage);
	if (!sX(NextRecord64(CDF,Var->VDRoffset64,Var->zVar,
			     recNum,&nextRec,&found),&pStatus)) return pStatus;
	if (found) {
	  Int32 prevRecN = nextRec - 1;
	  writeTo = MINIMUM(writeTo,prevRecN);
	}
	recCount = writeTo - recNum + 1;
	tOffset = Var->stage.areaOffset64;
	if (fullRecord)
	  numElems = (OFF_T) recCount * Var->NphyRecElems;
	else {
	  if (!sX(BuildPadBuffer64(CDF,Var,recCount,
				   &how,&padBuffer,
				   TRUE),&pStatus)) return pStatus;
	  if (!sX(WritePadValues64(Var,CDF->stage.fp,tOffset,
				   recCount,how,padBuffer),&pStatus)) {
	    cdf_FreeMemory (padBuffer, NULL);
	    return pStatus;
	  }
	  cdf_FreeMemory (padBuffer, NULL);
	  tOffset += (OFF_T) offset;
	  numElems = nValues * Var->NvalueElems;
	}
	if (!sX(WriteVarElems64(Var,CDF->stage.fp,tOffset,
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
      if (Var->stage.areaOffset64 == (OFF_T) NO_OFFSET64) {
	if (Var->blockingFactor == 0) {
	  if (Var->recVary) {
	    Int32 bf = ((MIN_BLOCKING_BYTES_compressed-1)/Var->NphyRecBytes)+1;
	    Var->blockingFactor = MAXIMUM(bf,MIN_BLOCKING_RECS_compressed);
	  }
	  else
	    Var->blockingFactor = 1;
	  if (!sX(WriteVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
			     VDR_BLOCKING,&(Var->blockingFactor),
			     VDR_NULL),&pStatus)) return pStatus;
	}
	nBytes = Var->blockingFactor * Var->NphyRecBytes;
	if (!sX(InitVarStage64(CDF,Var,nBytes),&pStatus)) return pStatus;
      }
      /************************************************************************
      * Pad records (if necessary).
      ************************************************************************/
      if (!sparseRecords) {
	padTo = firstRec - 1;
	if (Var->maxRec < padTo) {
	  Int32 nRecords = padTo + 1, recN, valueN; void *padBuffer; int how;
	  if (!sX(BuildPadBuffer64(CDF,Var,
				   nRecords,&how,
				   &padBuffer,FALSE),&pStatus)) return pStatus;
	  switch (how) {
	    case ALLrecordsATonce:
	      if (!sX(WriteCompressedRecords(CDF,Var,Var->maxRec+1,
					     padTo,padBuffer,
					     nRecords*Var->NphyRecValues,
					     (OFF_T) ZERO_OFFSET64,
					     TRUE),&pStatus)) {
		cdf_FreeMemory (padBuffer, NULL);
		return pStatus;
	      }
	      break;
	    case ONErecordATaTIME:
	      for (recN = Var->maxRec + 1; recN <= padTo; recN++) {
		 if (!sX(WriteCompressedRecords(CDF,Var,recN,recN,
					        padBuffer,Var->NphyRecValues,
						(OFF_T) ZERO_OFFSET64,TRUE),&pStatus)) {
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
	if (!sX(PadUnRecords64(CDF,Var,padFrom,padTo),&pStatus)) return pStatus;
      }
      /************************************************************************
      * Write value(s).
      ************************************************************************/
      if (!sX(RecordByteOffset64(CDF,Var,
			         firstRec,
			         &tOffset),&pStatus)) return pStatus;
      tOffset += (OFF_T) offset;
      numElems = nValues * Var->NvalueElems;
      if (!sX(WriteVarElems64(Var,Var->fp,tOffset,
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
  if (!sX(UpdateMaxRec64(CDF,Var,lastRec),&pStatus)) return pStatus;
  return pStatus;
}

/******************************************************************************
* WriteVarElems64.
*   NOTE: On IBM PCs, it is assumed that the number of bytes being written
* will not exceed 65535.
******************************************************************************/

STATICforIDL CDFstatus WriteVarElems64 (Var, fp, offset, numElems, buffer)
struct VarStruct *Var;
vFILE *fp;
OFF_T offset;
OFF_T numElems;
void *buffer;
{
  CDFstatus pStatus = CDF_OK; OFF_T elemCount;
  /****************************************************************************
  * Seek to the desired offset.
  ****************************************************************************/
  if (!SEEKv64(fp,offset,vSEEK_SET)) return VAR_WRITE_ERROR;
  /****************************************************************************
  * If no encoding is necessary, simply write the buffer and return.
  ****************************************************************************/
  if (Var->EncodeFunction == NULL) {
    OFF_T  totalBytes = numElems * Var->NelemBytes;
    if (totalBytes <= REWR_MAX) {
      Int32 nBytes = (size_t) totalBytes;
      if (STRINGdataType(Var->dataType)) {
        int len = (int) strlen((char *)buffer);
        if (len < (int) totalBytes) {
          char *newBuffer = (char *) cdf_AllocateMemory (nBytes, NULL);
          memcpy (newBuffer, (char *)buffer, totalBytes);
          FillSpacesToString((char *)newBuffer, (int) totalBytes,
                             (int) Var->NvalueElems);
          if (!WRITEv64(newBuffer,1,(size_t)nBytes,fp)) return VAR_WRITE_ERROR;
          cdf_FreeMemory (newBuffer, NULL);
        } else
          if (!WRITEv64(buffer,1,(size_t)nBytes,fp)) return VAR_WRITE_ERROR;
      } else
        if (!WRITEv64(buffer,1,(size_t)nBytes,fp)) return VAR_WRITE_ERROR;
    } else {
      Byte1 *tBuffer = (Byte1 *) buffer;
      OFF_T remaining = totalBytes;
      int i = 0;
      while (remaining > 0) {
        if (!WRITEv64(tBuffer+i*REWR_MAX,1,
                      (remaining>REWR_MAX?(size_t)REWR_MAX:(size_t)remaining),
                      fp))
          return CDF_WRITE_ERROR;
        remaining -= REWR_MAX;
        ++i;
      }
    }
    return pStatus;
  }
  /****************************************************************************
  * Use as large a temporary buffer as possible for the encoding conversion.
  * Start at the full number of elements and then halve that number until an
  * allocation succeeds.
  ****************************************************************************/
  elemCount = numElems;
  for (;;) {
     OFF_T xBytes = (OFF_T) elemCount * Var->NelemBytes;
#if defined(win32)
     if (xBytes < (OFF_T) ((1i64 << 31) - 1)) {
#else
     if (xBytes < (OFF_T) ((1LL << 31) - 1)) {
#endif
       size_t nBytes = (size_t) (elemCount * Var->NelemBytes);
       void *tBuffer;
       if ((int) nBytes <= 0) return VAR_READ_ERROR;
       tBuffer = cdf_AllocateMemory (nBytes, NULL);
       if (tBuffer != NULL) {
         OFF_T elemN = 0; Byte1 *bOffset = buffer;
	 Int32 thisElemCount; size_t thisByteCount;
         while (elemN < numElems) {
	   if ((numElems - elemN) > elemCount) thisElemCount = elemCount;
           else thisElemCount = (Int32) (numElems - elemN);
	   thisByteCount = (size_t) (thisElemCount * Var->NelemBytes);
	   memmove (tBuffer, bOffset, thisByteCount);
	   if (!sX(Var->EncodeFunction(tBuffer,thisElemCount),&pStatus)) {
	     cdf_FreeMemory (tBuffer, NULL);
	     return pStatus;
	   }
	   if (!WRITEv64(tBuffer,1,thisByteCount,fp)) {
	     cdf_FreeMemory (tBuffer, NULL);
	     return VAR_WRITE_ERROR;
	   }
	   elemN += (OFF_T) thisElemCount;
	   bOffset += (OFF_T) thisByteCount;
         }
         cdf_FreeMemory (tBuffer, NULL);
         return pStatus;
       }
     }
     if (elemCount == 1) break;
     elemCount = (elemCount + 1) / 2;
  }
  return BAD_MALLOC;
}

/******************************************************************************
* PrevRecord64.
*   Determine the last record allocated AT or BEFORE `baseRec'.  This routine
* should only be used for single-file CDFs.
******************************************************************************/

STATICforIDL CDFstatus PrevRecord64 (CDF, VDRoffset, zVar, baseRec, prevRec,
				     found)
struct CDFstruct *CDF;
OFF_T VDRoffset;
Logical zVar;
Int32 baseRec;
Int32 *prevRec;
Logical *found;         /* If NULL, return NO_SUCH_RECORD if the previous
			   record doesn't exist.  Otherwise, set according to
			   whether or not the previous record exists and
			   return the pending status. */
{
  CDFstatus pStatus = CDF_OK; OFF_T VXRoffset;
  /****************************************************************************
  * If multi-file...
  ****************************************************************************/
  if (!CDF->singleFile) return CDF_INTERNAL_ERROR;
  /****************************************************************************
  * Single-file...read the offset of the first VXR.
  ****************************************************************************/
  if (!sX(ReadVDR64(CDF,CDF->fp,VDRoffset,zVar,
		    VDR_VXRHEAD,&VXRoffset,
		    VDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * ...and start there (unless there are no VXRs).
  ****************************************************************************/
  if (VXRoffset != (OFF_T) ZERO_OFFSET64) {
    if (!sX(PrevRecord_r_64(CDF->fp,VXRoffset,
			    baseRec,prevRec,found),&pStatus)) return pStatus;
  }
  else {
    ASSIGNnotNULL (found, FALSE)
    if (found == NULL) pStatus = NO_SUCH_RECORD;
  }
  return pStatus;
}

static CDFstatus PrevRecord_r_64 (fp, vxrOffset, baseRec, prevRec, found)
vFILE *fp;
OFF_T vxrOffset;
Int32 baseRec;
Int32 *prevRec;
Logical *found;
{
  CDFstatus pStatus = CDF_OK; int entryN = 0;
  struct VXRstruct64 VXR, nextVXR; Int32 irType;
  /****************************************************************************
  * Read the first VXR.
  ****************************************************************************/
  if (!sX(ReadVXR64(fp,vxrOffset,
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
       if (!sX(ReadIrType64(fp,VXR.Offset[entryN],&irType),&pStatus)) {
	 return pStatus;
       }
       switch (irType) {
	 case VXR_:
	   return PrevRecord_r_64(fp,VXR.Offset[entryN],baseRec,prevRec,found);
	 case VVR_:
	 case CVVR_:
	   *prevRec = baseRec;
	   ASSIGNnotNULL (found, TRUE)
	   return pStatus;
	 default:
	   return CORRUPTED_V3_CDF;
       }
     }
     /*************************************************************************
     * If this is the last entry in the current VXR...
     *************************************************************************/
     if (entryN == VXR.NusedEntries - 1) {
       if (VXR.VXRnext == (OFF_T) ZERO_OFFSET64) {
	 *prevRec = VXR.Last[entryN];
	 ASSIGNnotNULL (found, TRUE)
	 return pStatus;
       }
       if (!sX(ReadVXR64(fp,VXR.VXRnext,
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
* NextRecord64.
*   Determine the next allocated record AT or AFTER `baseRec'.  I.e., if
* `baseRec' is allocated, it is returned as `nextRec'.
******************************************************************************/

STATICforIDL CDFstatus NextRecord64 (CDF, VDRoffset, zVar, baseRec, nextRec,
				     found)
struct CDFstruct *CDF;
OFF_T VDRoffset;
Logical zVar;
Int32 baseRec;
Int32 *nextRec;
Logical *found;         /* If NULL, return NO_SUCH_RECORD if the next record
			   doesn't exist.  Otherwise, set according to whether
			   or not the next record exists and return the pending
			   status. */
{
  CDFstatus pStatus = CDF_OK; Int32 maxRec;
  OFF_T VXRoffset;
  /****************************************************************************
  * If multi-file...
  ****************************************************************************/
  if (!CDF->singleFile) {
    if (!sX(ReadVDR64(CDF,CDF->fp,VDRoffset,zVar,
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
  if (!sX(ReadVDR64(CDF,CDF->fp,VDRoffset,zVar,
		    VDR_VXRHEAD,&VXRoffset,
		    VDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * ...and start there.
  ****************************************************************************/
  if (!sX(NextRecord_r_64(CDF->fp,VXRoffset,
		          baseRec,nextRec,found),&pStatus)) return pStatus;
  return pStatus;
}

static CDFstatus NextRecord_r_64 (fp, vxrOffset, baseRec, nextRec, found)
vFILE *fp;
OFF_T vxrOffset;
Int32 baseRec;
Int32 *nextRec;
Logical *found;
{
  CDFstatus pStatus = CDF_OK;
  int entryN; struct VXRstruct64 VXR; Int32 irType;
  /****************************************************************************
  * While more VXRs...
  ****************************************************************************/
  while (vxrOffset != (OFF_T) ZERO_OFFSET64) {
    /**************************************************************************
    * Read the VXR.
    **************************************************************************/
    if (!sX(ReadVXR64(fp,vxrOffset,
		      VXR_RECORD,&VXR,
		      VXR_NULL),&pStatus)) return pStatus;
    /**************************************************************************
    * Search entries...
    **************************************************************************/
    if (baseRec <= VXR.Last[(int)(VXR.NusedEntries-1)]) {
      for (entryN = 0; entryN < VXR.NusedEntries; entryN++) {
	 if (baseRec <= VXR.Last[entryN]) {
	   if (!sX(ReadIrType64(fp,
			        VXR.Offset[entryN],
			        &irType),&pStatus)) return pStatus;
	   switch (irType) {
	     case VXR_:
	       return NextRecord_r_64(fp,VXR.Offset[entryN],
				      baseRec,nextRec,found);
	     case VVR_:
	     case CVVR_:
	       *nextRec = BOO(VXR.First[entryN] <= baseRec,
			      baseRec,VXR.First[entryN]);
	       ASSIGNnotNULL (found, TRUE)
	       return pStatus;
	     default:
	       return CORRUPTED_V3_CDF;
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
* SearchForRecord64.
******************************************************************************/

STATICforIDL CDFstatus SearchForRecord64 (CDF, VDRoffset, zVar, recNum,
					  firstRec, lastRec, offset, found)
struct CDFstruct *CDF;
OFF_T VDRoffset;
Logical zVar;
Int32 recNum;
Int32 *firstRec;
Int32 *lastRec;
OFF_T *offset;
Logical *found;         /* If NULL, return NO_SUCH_RECORD if the record is
			   not found.  Otherwise, set according to whether
			   or not the record is found and return the pending
			   status. */
{
  CDFstatus pStatus = CDF_OK; Int32 maxRec;
  OFF_T vxrOffset;
  /****************************************************************************
  * If multi-file...
  ****************************************************************************/
  if (!CDF->singleFile) {
    if (!sX(ReadVDR64(CDF,CDF->fp,VDRoffset,zVar,
		      VDR_MAXREC,&maxRec,
		      VDR_NULL),&pStatus)) return pStatus;
    if (recNum <= maxRec) {
      ASSIGNnotNULL (firstRec, 0)
      ASSIGNnotNULL (lastRec, maxRec)
      ASSIGNnotNULL (offset, (OFF_T) ZERO_OFFSET64)
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
  if (!sX(ReadVDR64(CDF,CDF->fp,VDRoffset,zVar,
		    VDR_VXRHEAD,&vxrOffset,
		    VDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * ...and start searching there.
  ****************************************************************************/
  if (!sX(SearchForRecord_r_64(CDF->fp,vxrOffset,recNum,
			       firstRec,lastRec,
			       offset,found),&pStatus)) return pStatus;
  return pStatus;
}

static CDFstatus SearchForRecord_r_64 (fp, vxrOffset, recNum, firstRec, lastRec,
				       offset, found)
vFILE *fp;
OFF_T vxrOffset;
Int32 recNum;
Int32 *firstRec;
Int32 *lastRec;
OFF_T *offset;
Logical *found;
{
  CDFstatus pStatus = CDF_OK;
  int entryN; struct VXRstruct64 VXR; Int32 irType;
  /****************************************************************************
  * While more VXRs...
  ****************************************************************************/
  while (vxrOffset != (OFF_T) ZERO_OFFSET64) {
    /**************************************************************************
    * Read the VXR.
    **************************************************************************/
    if (!sX(ReadVXR64(fp,vxrOffset,
		      VXR_RECORD,&VXR,
		      VXR_NULL),&pStatus)) return pStatus;
    /**************************************************************************
    * Search entries...
    **************************************************************************/
    if (recNum <= VXR.Last[(int)(VXR.NusedEntries-1)]) {
      for (entryN = 0; entryN < VXR.NusedEntries; entryN++) {
	 if (recNum <= VXR.Last[entryN]) {
	   if (VXR.First[entryN] <= recNum) {
	     if (!sX(ReadIrType64(fp,
				  VXR.Offset[entryN],
				  &irType),&pStatus)) return pStatus;
	     switch (irType) {
	       case VXR_:
		 return SearchForRecord_r_64(fp,VXR.Offset[entryN],recNum,
					     firstRec,lastRec,offset,found);
	       case VVR_:
	       case CVVR_:
		 ASSIGNnotNULL (firstRec, VXR.First[entryN])
		 ASSIGNnotNULL (lastRec, VXR.Last[entryN])
		 ASSIGNnotNULL (offset, VXR.Offset[entryN])
		 ASSIGNnotNULL (found, TRUE)
		 return pStatus;
	       default:
		 return CORRUPTED_V3_CDF;
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
* IndexingStatistics64.
******************************************************************************/

STATICforIDL CDFstatus IndexingStatistics64 (CDF, VDRoffset, zVar, nVXRsP,
					     nEntriesP, nAllocP, nRecordsP,
					     nLevelsP)
struct CDFstruct *CDF;
OFF_T VDRoffset;
Logical zVar;
Int32 *nVXRsP;
Int32 *nEntriesP;
Int32 *nAllocP;
Int32 *nRecordsP;
Int32 *nLevelsP;
{
  CDFstatus pStatus = CDF_OK; Int32 maxRec; int level = 1;
  Int32 nVXRs = 0, nEntries = 0, nAlloc = 0, nRecords = 0, nLevels = 0;
  OFF_T vxrOffset;
  /****************************************************************************
  * Read the maximum record and the offset of the first VXR...
  ****************************************************************************/
  if (!sX(ReadVDR64(CDF,CDF->fp,VDRoffset,zVar,
		    VDR_VXRHEAD,&vxrOffset,
		    VDR_MAXREC,&maxRec,
		    VDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * ...and start there.
  ****************************************************************************/
  if (vxrOffset != (OFF_T) ZERO_OFFSET64) {
    if (!sX(IndexingStatistics_r_64(CDF->fp,vxrOffset,
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

static CDFstatus IndexingStatistics_r_64 (fp, vxrOffset, maxRec, level, nLevels,
				          nVXRs, nEntries, nAlloc, nRecords)
vFILE *fp;
OFF_T vxrOffset;
Int32 maxRec;
int level;
Int32 *nLevels;
Int32 *nVXRs;
Int32 *nEntries;
Int32 *nAlloc;
Int32 *nRecords;
{
  CDFstatus pStatus = CDF_OK;
  int e; Int32 irType; struct VXRstruct64 VXR;
  /****************************************************************************
  * Check if a new level has been reached.
  ****************************************************************************/
  *nLevels = MAXIMUM(*nLevels,level);
  /****************************************************************************
  * While more VXRs...
  ****************************************************************************/
  while (vxrOffset != (OFF_T) ZERO_OFFSET64) {
    /**************************************************************************
    * Read/tally the VXR.
    **************************************************************************/
    if (!sX(ReadVXR64(fp,vxrOffset,
		      VXR_RECORD,&VXR,
		      VXR_NULL),&pStatus)) return pStatus;
    (*nVXRs)++;
    /**************************************************************************
    * Scan/count entries...
    **************************************************************************/
    for (e = 0; e < VXR.NusedEntries; e++) {
       (*nEntries)++;
       if (!sX(ReadIrType64(fp,
			    VXR.Offset[e],
			    &irType),&pStatus)) return pStatus;
       switch (irType) {
	 case VXR_:
	   if (!sX(IndexingStatistics_r_64(fp,VXR.Offset[e],
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
	   return CORRUPTED_V3_CDF;
       }
    }
    vxrOffset = VXR.VXRnext;
  }
  return pStatus;
}

/******************************************************************************
* BuildPadBuffer64.
******************************************************************************/

STATICforIDL CDFstatus BuildPadBuffer64 (CDF, Var, nRecords, how, buffer, encode)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 nRecords;
int *how;
void **buffer;
Logical encode;         /* If TRUE, return values in CDF's encoding.  If FALSE,
			   return values in host computer's encoding. */
{
  Byte1 *ptr; Int32 nBytes, valueN; void *padValue;
  Int32 VDRflags, dataType, numElems; CDFstatus pStatus = CDF_OK;
  OFF_T xBytes, nValues;
  /****************************************************************************
  * Determine pad value.
  ****************************************************************************/
  if (!sX(ReadVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
		    VDR_DATATYPE,&dataType,
		    VDR_NUMELEMS,&numElems,
		    VDR_FLAGS,&VDRflags,
		    VDR_NULL),&pStatus)) return pStatus;
  padValue = (void *) cdf_AllocateMemory ((size_t) Var->NvalueBytes, NULL);
  if (padValue == NULL) return BAD_MALLOC;
  if (PADvalueBITset(VDRflags)) {
    if (!sX(ReadVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
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
    Int32 version, release;
    if (!sX(ReadCDR64(CDF->fp,CDF->CDRoffset64,
                      CDR_VERSION,&version,
                      CDR_RELEASE,&release,
                      CDR_NULL),&pStatus)) return pStatus;
    if (version*100+release < 305)
      DefaultPadValuePre350 (dataType, numElems, padValue);
    else
      DefaultPadValue (dataType, numElems, padValue);
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
  nValues = (OFF_T) nRecords * Var->NphyRecValues;
  xBytes = nValues * Var->NvalueBytes;
#if defined(win32)
  if (xBytes < (OFF_T) ((1i64 << 31) - 1)) {
#else
  if (xBytes < (OFF_T) ((1LL << 31) - 1)) {
#endif
    nBytes = (Int32) xBytes;
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
  }
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
* ReadVarValues64.
*   NOTE: If more than one record is being read, full records are assumed
* and the `offset' must be zero.
******************************************************************************/

STATICforIDL CDFstatus ReadVarValues64 (CDF, Var, startRec, offset, nValues,
				        buffer)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 startRec;         /* Physical record number at which to start reading. */
Int32 offset;           /* Byte offset within (first) record at which to
			   begin reading. */
OFF_T nValues;          /* Number of values to read. */
void *buffer;
{
  CDFstatus pStatus = CDF_OK; OFF_T tOffset; Logical fullRecord;
  Int32 numElems, firstRec, lastRec, lastRecInVVR;
  Int32 readTo, recCount, nBytes; OFF_T nPadValues;
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
	    if (!sX(SearchForRecord64(CDF,Var->VDRoffset64,
				      Var->zVar,recNum,
				      NULL,&lastRecInVVR,
				      NULL,NULL),&pStatus)) return pStatus;
	    readTo = MINIMUMof3(Var->maxRec,lastRec,lastRecInVVR);
	    recCount = readTo - recNum + 1;
	    if (!sX(RecordByteOffset64(CDF,Var,
				       recNum,
				       &tOffset),&pStatus)) return pStatus;
	    numElems = recCount * Var->NphyRecElems;
	    if (!sX(ReadVarElems64(Var,CDF->fp,tOffset,
				   numElems,tBuffer),&pStatus)) return pStatus;
	  }
	  else {
	    recCount = lastRec - recNum + 1;
	    nPadValues = (OFF_T) recCount * Var->NphyRecValues;
	    if (!sX(PadBuffer64(CDF,Var,
			        nPadValues,
			        tBuffer),&pStatus)) return pStatus;
	    sX (VIRTUAL_RECORD_DATA, &pStatus);
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
	  if (!sX(RecordByteOffset64(CDF,Var,
				     firstRec,
				     &tOffset),&pStatus)) return pStatus;
	  tOffset += (OFF_T) offset;
	  numElems = nValues * Var->NvalueElems;
	  if (!sX(ReadVarElems64(Var,CDF->fp,tOffset,
			         numElems,buffer),&pStatus)) return pStatus;
	}
	else {
	  if (!sX(PadBuffer64(CDF,Var,nValues,buffer),&pStatus)) return pStatus;
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
      * CORRUPTED_V3_CDF error code from being returned if an explicit
      * blocking factor has not been specified for the variable.
      ************************************************************************/
      if (Var->stage.areaOffset64 == (OFF_T) NO_OFFSET64 &&
          Var->maxRec > NO_RECORD) {
	if (Var->blockingFactor == 0) return CORRUPTED_V3_CDF;
	nBytes = Var->blockingFactor * Var->NphyRecBytes;
	if (!sX(InitVarStage64(CDF,Var,nBytes),&pStatus)) return pStatus;
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
	  if (!sX(RecordByteOffset64(CDF,Var,
				     firstRec,
				     &tOffset),&pStatus)) return pStatus;
	  recCount = MINIMUM(lastRec,Var->maxRec) - firstRec + 1;
	  numElems = recCount * Var->NphyRecElems;
	  if (!sX(ReadVarElems64(Var,Var->fp,tOffset,
			         numElems,tBuffer),&pStatus)) return pStatus;
	  tBuffer += (size_t) (recCount * Var->NphyRecBytes);
	}
	if (lastRec > Var->maxRec) {
	  recCount = BOO(Var->maxRec < firstRec,
			 lastRec - firstRec + 1,
			 lastRec - Var->maxRec);
	  nPadValues = (OFF_T) recCount * Var->NphyRecValues;
	  if (!sX(PadBuffer64(CDF,Var,
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
	  if (!sX(RecordByteOffset64(CDF,Var,
				     firstRec,
				     &tOffset),&pStatus)) return pStatus;
	  tOffset += (OFF_T) offset;
	  numElems = nValues * Var->NvalueElems;
	  if (!sX(ReadVarElems64(Var,Var->fp,tOffset,
			         numElems,buffer),&pStatus)) return pStatus;
	}
	else {
	  if (!sX(PadBuffer64(CDF,Var,nValues,buffer),&pStatus)) return pStatus;
	  sX (VIRTUAL_RECORD_DATA, &pStatus);
	}
      }
      break;
  }
  return pStatus;
}

/******************************************************************************
* ReadVarElems64.
*   NOTE: On IBM PCs, it is assumed that the number of bytes being read
* will not exceed 65535.
******************************************************************************/

STATICforIDL CDFstatus ReadVarElems64 (Var, fp, offset, numElems, buffer)
struct VarStruct *Var;
vFILE *fp;
OFF_T offset;
OFF_T numElems;
void *buffer;
{
  CDFstatus pStatus = CDF_OK; size_t nBytes;
  OFF_T totalBytes;
  /****************************************************************************
  * Seek to the desired offset.
  ****************************************************************************/
  if (!SEEKv64(fp,offset,vSEEK_SET)) return VAR_READ_ERROR;
  /****************************************************************************
  * Read the value(s).
  ****************************************************************************/
  totalBytes = numElems * Var->NelemBytes;
  if (totalBytes <= REWR_MAX) {
    nBytes = (size_t) totalBytes;
    if (!READv64(buffer,(size_t)totalBytes,1,fp)) return VAR_READ_ERROR;
    if (STRINGdataType(Var->dataType)) {
      FillSpacesToString((char *)buffer, (int) numElems,
                         (int) Var->NvalueElems);
    }
  } else {
    Byte1 *tBuffer = (Byte1 *) buffer;
    OFF_T remaining = totalBytes;
    int i = 0;
    while (remaining > 0) {
      if (!READv64(tBuffer+i*REWR_MAX,
                   (remaining>REWR_MAX?(size_t)REWR_MAX:(size_t)remaining),
                   1,fp))
        return CDF_READ_ERROR;
      remaining -= (OFF_T) REWR_MAX;
      ++i;
    }
  }
  /****************************************************************************
  * Decode value(s).
  ****************************************************************************/
  if (numElems < REWR_MAX) {
    if (!sX(DECODE(Var->DecodeFunction,buffer,(Int32)numElems),&pStatus)) {
      return pStatus;
    }
  } else {
    Byte1 *tBuffer = (Byte1 *) buffer;
    OFF_T remaining = numElems;
    int i = 0;
    while (remaining > 0) {
      if (!sX(DECODE(Var->DecodeFunction,tBuffer+i*REWR_MAX,
                     (remaining>REWR_MAX?(Int32)REWR_MAX:(Int32)remaining)),
              &pStatus))
         return CDF_READ_ERROR;
      remaining -= REWR_MAX;
      ++i;
    }
  }

  return pStatus;
}

/******************************************************************************
* InitVarStage64.
******************************************************************************/

STATICforIDL CDFstatus InitVarStage64 (CDF, Var, nBytes)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 nBytes;
{
  if (CDF->stage.fp == NULL) {
    CDF->stage.fp = V_scratch (ScratchDirectory(CDF), "stg");
    if (CDF->stage.fp == NULL) return SCRATCH_CREATE_ERROR;
    if (!CACHEv64(CDF->stage.fp,CDF->stage.cacheSize)) {
      V_delete64 (CDF->stage.fp, NULL);
      CDF->stage.fp = NULL;
      return BAD_CACHE_SIZE;
    }
    CDF->stage.mark64 = (OFF_T) ZERO_OFFSET64;
  }
  Var->stage.areaOffset64 = CDF->stage.mark64;
  Var->stage.firstRec = NO_RECORD;
  Var->stage.lastRec = NO_RECORD;
  Var->stage.dotOffset64 = (OFF_T) NO_OFFSET64;
  Var->stage.modified = FALSE;
  CDF->stage.mark64 += nBytes;
  return CDF_OK;
}

/******************************************************************************
* InitScratch64.
******************************************************************************/

STATICforIDL CDFstatus InitScratch64 (scratchDir, scratchFpH, cacheSize)
char *scratchDir;	/* Scratch directory to be used. */
vFILE **scratchFpH;	/* Scratch file handle (pointer to pointer). */
int cacheSize;		/* Number of cache buffers to request. */
{
  if (*scratchFpH == NULL) {
    *scratchFpH = V_scratch (scratchDir, NULL);
    if (*scratchFpH == NULL) return SCRATCH_CREATE_ERROR;
    if (!CACHEv64(*scratchFpH,cacheSize)) {
      V_delete64 (*scratchFpH, NULL);
      *scratchFpH = NULL;
      return BAD_CACHE_SIZE;
    }
  }
  else {
    if (V_clear(*scratchFpH) != 0) return SCRATCH_READ_ERROR;
    if (!SEEKv64(*scratchFpH,(OFF_T)0,vSEEK_SET)) return SCRATCH_READ_ERROR;
  }
  return CDF_OK;
}

/******************************************************************************
* FlushStage64.
******************************************************************************/

STATICforIDL CDFstatus FlushStage64 (CDF, Var)
struct CDFstruct *CDF;
struct VarStruct *Var;
{
  CDFstatus pStatus = CDF_OK;
  /****************************************************************************
  * Based on the variable type...
  ****************************************************************************/
  switch (Var->vType) {
    case SPARSE_RECORDS_: {
      Int32 nRecords, nBytes; struct AllocStruct alloc;
      OFF_T tOffset;
      /************************************************************************
      * First check if there are records to be flushed in the staging area.
      ************************************************************************/
      if (!Var->stage.modified) return pStatus;
      /************************************************************************
      * Allocate a new VVR.
      ************************************************************************/
      LoadAllocVVR64 (alloc, Var->stage.firstRec, Var->stage.lastRec, FALSE)
      if (!sX(AllocateRecords64(CDF,Var,alloc),&pStatus)) return pStatus;
      Var->maxAllocated = MAXIMUM(Var->maxAllocated,Var->stage.lastRec);
      /************************************************************************
      * Copy the records to the new VVR.
      ************************************************************************/
      if (!sX(RecordByteOffset64(CDF,Var,
			         Var->stage.firstRec,
			         &tOffset),&pStatus)) return pStatus;
      nRecords = Var->stage.lastRec - Var->stage.firstRec + 1;
      nBytes = nRecords * Var->NphyRecBytes;
      if (!sX(CopyBytes64(CDF->stage.fp,Var->stage.areaOffset64,
		 	  SCRATCH_READ_ERROR,nBytes,CDF->fp,
			  tOffset,CDF_WRITE_ERROR),&pStatus)) return pStatus;
      Var->maxWritten = MAXIMUM(Var->maxWritten,Var->stage.lastRec);
      /************************************************************************
      * Clear the staging area control.
      ************************************************************************/
      Var->stage.firstRec = NO_RECORD;
      Var->stage.lastRec = NO_RECORD;
      Var->stage.dotOffset64 = (OFF_T) NO_OFFSET64;
      Var->stage.modified = FALSE;
      break;
    }
    case COMPRESSED_:
    case SPARSE_COMPRESSED_RECORDS_: {
      Int32 nRecords;
      OFF_T cSize, xSize, irSize, uSize, newOffset;
      struct CVVRstruct64 CVVR; struct VVRstruct64 VVR; struct AllocStruct alloc;
      /************************************************************************
      * First check if there are records to be flushed in the staging area.
      ************************************************************************/
      if (!Var->stage.modified) return pStatus;
      /************************************************************************
      * Check if the scratch file is created/initialized.
      ************************************************************************/
      if (!sX(InitScratch64(ScratchDirectory(CDF),
			    &(CDF->compressFp),
			    CDF->compressCacheSize),&pStatus)) return pStatus;
      /************************************************************************
      * Compress the records in the stage.
      ************************************************************************/
      nRecords = Var->stage.lastRec - Var->stage.firstRec + 1;
      uSize = nRecords * Var->NphyRecBytes;
      if (!sX(Compress64(CDF->stage.fp,Var->stage.areaOffset64,
		         uSize,SCRATCH_READ_ERROR,Var->cType,
		         Var->cParms,CDF->compressFp,(OFF_T) ZERO_OFFSET64,
		         &cSize,SCRATCH_WRITE_ERROR),&pStatus)) return pStatus;
      /************************************************************************
      * Does a VVR/CVVR for this block of records already exist?
      ************************************************************************/
      if (Var->stage.dotOffset64 != (OFF_T) NO_OFFSET64) {
	/**********************************************************************
	* Read the size of the existing VVR/CVVR.
	**********************************************************************/
	if (!sX(ReadIrSize64(CDF->fp,
			     Var->stage.dotOffset64,
			     &irSize),&pStatus)) return pStatus;
	/**********************************************************************
	* Will a CVVR fit?  Note that reserve space is only added to new CVVRs.
	**********************************************************************/
	if (CVVR_BASE_SIZE64 + cSize <= irSize) {
	  LoadCVVRx64 (CVVR, irSize, cSize)
	  if (!sX(WriteCVVR64(CDF->fp,Var->stage.dotOffset64,
			      CVVR_RECORDx,&CVVR,
			      CVVR_NULL),&pStatus)) return pStatus;
	  if (!sX(CopyBytes64(CDF->compressFp,(OFF_T) ZERO_OFFSET64,
			      SCRATCH_READ_ERROR,cSize,CDF->fp,
			      Var->stage.dotOffset64 + CVVR_BASE_SIZE64,
			      CDF_WRITE_ERROR),&pStatus)) return pStatus;
	  Var->stage.modified = FALSE;
	  return pStatus;
	}
	/**********************************************************************
	* Will a VVR fit?
	**********************************************************************/
	if (VVR_BASE_SIZE64 + uSize <= irSize) {
	  LoadVVRx64 (VVR, irSize)
	  if (!sX(WriteVVR64(CDF->fp,Var->stage.dotOffset64,
			     VVR_RECORDx,&VVR,
			     VVR_NULL),&pStatus)) return pStatus;
	  if (!sX(CopyBytes64(CDF->stage.fp,Var->stage.areaOffset64,
			      SCRATCH_READ_ERROR,uSize,CDF->fp,
			      Var->stage.dotOffset64 + VVR_BASE_SIZE64,
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
	if (CVVR_BASE_SIZE64 + cSize < VVR_BASE_SIZE64 + uSize) {
	  if (!sX(ResizeIR64(CDF,Var->stage.dotOffset64,
			     CVVR_BASE_SIZE64 + cSize,
			     &newOffset,TRUE,NULL),&pStatus)) return pStatus;
	  LoadCVVRx64 (CVVR, CVVR_BASE_SIZE64 + cSize, cSize)
	  if (!sX(WriteCVVR64(CDF->fp,newOffset,
			      CVVR_RECORDx,&CVVR,
			      CVVR_NULL),&pStatus)) return pStatus;
	  if (!sX(CopyBytes64(CDF->compressFp,(OFF_T) ZERO_OFFSET64,
			      SCRATCH_READ_ERROR,cSize,
			      CDF->fp,newOffset + CVVR_BASE_SIZE64,
			      CDF_WRITE_ERROR),&pStatus)) return pStatus;
	}
	else {
	  if (!sX(ResizeIR64(CDF,Var->stage.dotOffset64,
			     VVR_BASE_SIZE64 + uSize,
			     &newOffset,TRUE,NULL),&pStatus)) return pStatus;
	  LoadVVRx64 (VVR, VVR_BASE_SIZE64 + uSize)
	  if (!sX(WriteVVR64(CDF->fp,newOffset,
			     VVR_RECORDx,&VVR,
			     VVR_NULL),&pStatus)) return pStatus;
	  if (!sX(CopyBytes64(CDF->stage.fp,Var->stage.areaOffset64,
			      SCRATCH_READ_ERROR,uSize,
			      CDF->fp,newOffset + VVR_BASE_SIZE64,
			      CDF_WRITE_ERROR),&pStatus)) return pStatus;
	  sX (DID_NOT_COMPRESS, &pStatus);
	}
	if (!sX(ModIndexOffset64(CDF,Var,
			         Var->stage.firstRec,
			         Var->stage.lastRec,
			         newOffset),&pStatus)) return pStatus;
	Var->stage.dotOffset64 = newOffset;
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
        Int32 tSize;
	if (Var->reservePct <= 100) {
	  tSize = (Int32) ((uSize * (Var->reservePct/100.0)) + 0.5);
	  xSize = MAXIMUM(cSize,tSize) - cSize;
	}
	else {
	  tSize = (Int32) ((cSize * (Var->reservePct/100.0)) + 0.5);
	  xSize = tSize - cSize;
	}
      }
      /************************************************************************
      * Will a CVVR be smaller than a VVR?
      ************************************************************************/
      if (CVVR_BASE_SIZE64 + cSize + xSize < VVR_BASE_SIZE64 + uSize) {
	LoadAllocCVVR64 (alloc, Var->stage.firstRec, Var->stage.lastRec,
		         cSize, xSize)
	if (!sX(AllocateRecords64(CDF,Var,alloc),&pStatus)) return pStatus;
	if (!sX(SearchForRecord64(CDF,Var->VDRoffset64,Var->zVar,
				  Var->stage.firstRec,NULL,NULL,
				  &newOffset,NULL),&pStatus)) return pStatus;
	LoadCVVRx64 (CVVR, CVVR_BASE_SIZE64 + cSize + xSize, cSize)
	if (!sX(WriteCVVR64(CDF->fp,newOffset,
			    CVVR_RECORDx,&CVVR,
			    CVVR_NULL),&pStatus)) return pStatus;
	if (!sX(CopyBytes64(CDF->compressFp,(OFF_T) ZERO_OFFSET64,
			    SCRATCH_READ_ERROR,cSize,
			    CDF->fp,newOffset + CVVR_BASE_SIZE64,
			    CDF_WRITE_ERROR),&pStatus)) return pStatus;
	Var->stage.dotOffset64 = newOffset;
	Var->stage.modified = FALSE;
	return pStatus;
      }
      /************************************************************************
      * The CVVR will be too big - create a VVR.
      ************************************************************************/
      LoadAllocVVR64 (alloc, Var->stage.firstRec, Var->stage.lastRec, TRUE)
      if (!sX(AllocateRecords64(CDF,Var,alloc),&pStatus)) return pStatus;
      if (!sX(SearchForRecord64(CDF,Var->VDRoffset64,Var->zVar,
				Var->stage.firstRec,NULL,NULL,
				&newOffset,NULL),&pStatus)) return pStatus;
      LoadVVRx64 (VVR, VVR_BASE_SIZE64 + uSize)
      if (!sX(WriteVVR64(CDF->fp,newOffset,
		         VVR_RECORDx,&VVR,
		         VVR_NULL),&pStatus)) return pStatus;
      if (!sX(CopyBytes64(CDF->stage.fp,Var->stage.areaOffset64,
			  SCRATCH_READ_ERROR,uSize,
			  CDF->fp,newOffset + VVR_BASE_SIZE64,
			  CDF_WRITE_ERROR),&pStatus)) return pStatus;
      Var->stage.dotOffset64 = newOffset;
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
      Int32 firstRec, lastRec, nRecords; OFF_T uSize; Logical foundX;
      OFF_T offset;
      /************************************************************************
      * Determine if the record exists.
      ************************************************************************/
      if (!sX(SearchForRecord64(CDF,Var->VDRoffset64,Var->zVar,
			        recNum,&firstRec,&lastRec,
			        &offset,&foundX),&pStatus)) return pStatus;
      ASSIGNnotNULL (found, foundX)
      if (!foundX) return BOO(found == NULL,NO_SUCH_RECORD,pStatus);
      /************************************************************************
      * Flush the stage before...
      ************************************************************************/
      if (!sX(FlushStage64(CDF,Var),&pStatus)) return pStatus;
      /************************************************************************
      * ...decompressing the CVVR (or VVR if the records did not compress) to
      * the staging area.
      ************************************************************************/
      nRecords = lastRec - firstRec + 1;
      uSize = nRecords * Var->NphyRecBytes;
      if (!sX(DecompressToStage64(CDF,Var,
				  offset,uSize),&pStatus)) return pStatus;
      /************************************************************************
      * Update staging control.
      ************************************************************************/
      Var->stage.firstRec = firstRec;
      Var->stage.lastRec = lastRec;
      Var->stage.dotOffset64 = offset;
      Var->stage.modified = FALSE;
      break;
    }
    default:
      return CDF_INTERNAL_ERROR;
  }
  return pStatus;
}

/******************************************************************************
* CopyBytes64.
******************************************************************************/

STATICforIDL CDFstatus CopyBytes64 (iFp, iStart, iError, nBytes, oFp, oStart,
				    oError)
vFILE *iFp;
OFF_T iStart;
CDFstatus iError;
OFF_T nBytes;
vFILE *oFp;
OFF_T oStart;
CDFstatus oError;
{
  Int32 nBlocks = (Int32) (nBytes / COPYblockSIZE);	/* Number of full blocks 
							   that can be copied. */
  Int32 lastCount = (Int32) (nBytes % COPYblockSIZE);	/* Number of bytes 
							   remaining to be copied 
							   after the full blocks 
							   have been copied. */
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
	OFF_T iOffset = iStart + (OFF_T) (nBytes - COPYblockSIZE);
	OFF_T oOffset = oStart + (OFF_T) (nBytes - COPYblockSIZE);
	for (i = 0; i < nBlocks; i++) {
	   if (!SEEKv64(iFp,iOffset,vSEEK_SET)) return iError;
	   if (!READv64(buffer,(size_t)COPYblockSIZE,(size_t)1,iFp)) return
								   iError;
	   if (!SEEKv64(oFp,oOffset,vSEEK_SET)) return oError;
	   if (!WRITEv64(buffer,(size_t)COPYblockSIZE,(size_t)1,oFp)) return
								    oError;
	   iOffset -= (OFF_T) COPYblockSIZE;
	   oOffset -= (OFF_T) COPYblockSIZE;
	}
      }
      if (lastCount > 0) {
	if (!SEEKv64(iFp,iStart,vSEEK_SET)) return iError;
	if (!READv64(buffer,(size_t)lastCount,(size_t)1,iFp)) return iError;
	if (!SEEKv64(oFp,oStart,vSEEK_SET)) return oError;
	if (!WRITEv64(buffer,(size_t)lastCount,(size_t)1,oFp)) return oError;
      }
    }
    /**************************************************************************
    * If the input group (of bytes) is after the output group start at the
    * beginning in case they overlap.
    **************************************************************************/
    if (iStart > oStart) {
      OFF_T iOffset = iStart;
      OFF_T oOffset = oStart;
      if (nBlocks > 0) {
	for (i = 0; i < nBlocks; i++) {
	   if (!SEEKv64(iFp,iOffset,vSEEK_SET)) return iError;
	   if (!READv64(buffer,(size_t)COPYblockSIZE,(size_t)1,iFp)) return
								   iError;
	   if (!SEEKv64(oFp,oOffset,vSEEK_SET)) return oError;
	   if (!WRITEv64(buffer,(size_t)COPYblockSIZE,(size_t)1,oFp)) return
								    oError;
	   iOffset += (OFF_T) COPYblockSIZE;
	   oOffset += (OFF_T) COPYblockSIZE;
	}
      }
      if (lastCount > 0) {
	if (!SEEKv64(iFp,iOffset,vSEEK_SET)) return iError;
	if (!READv64(buffer,(size_t)lastCount,(size_t)1,iFp)) return iError;
	if (!SEEKv64(oFp,oOffset,vSEEK_SET)) return oError;
	if (!WRITEv64(buffer,(size_t)lastCount,(size_t)1,oFp)) return oError;
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
    if (!SEEKv64(iFp,iStart,vSEEK_SET)) return iError;
    if (!SEEKv64(oFp,oStart,vSEEK_SET)) return oError;
    for (i = 0; i < nBlocks; i++) {
       if (!READv64(buffer,(size_t)COPYblockSIZE,(size_t)1,iFp)) return iError;
       if (!WRITEv64(buffer,(size_t)COPYblockSIZE,(size_t)1,oFp)) return oError;
    }
    if (lastCount > 0) {
      if (!READv64(buffer,(size_t)lastCount,(size_t)1,iFp)) return iError;
      if (!WRITEv64(buffer,(size_t)lastCount,(size_t)1,oFp)) return oError;
    }
  }
  return CDF_OK;
}

/******************************************************************************
* ModIndexOffset64.
******************************************************************************/

STATICforIDL CDFstatus ModIndexOffset64 (CDF, Var, firstRec, lastRec, newOffset)
struct CDFstruct *CDF;  /* Pointer to CDF. */
struct VarStruct *Var;  /* Pointer to variable. */
Int32 firstRec;         /* First record of entry. */
Int32 lastRec;          /* Last record of entry. */
OFF_T newOffset;        /* New VVR/CVVR/SVVR offset. */
{
  CDFstatus pStatus = CDF_OK; OFF_T vxrOffset;
  /****************************************************************************
  * Read offset of first VXR...
  ****************************************************************************/
  if (!sX(ReadVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
		    VDR_VXRHEAD,&vxrOffset,
		    VDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * ...and start there.
  ****************************************************************************/
  if (!sX(ModIndexOffset_r_64(CDF->fp,vxrOffset,
			      firstRec,lastRec,
			      newOffset),&pStatus)) return pStatus;
  return pStatus;
}

static CDFstatus ModIndexOffset_r_64 (fp, vxrOffset, firstRec, lastRec, newOffset)
vFILE *fp;              /* File pointer to dotCDF file. */
OFF_T vxrOffset;        /* VXR at which to start. */
Int32 firstRec;         /* First record of entry. */
Int32 lastRec;          /* Last record of entry. */
OFF_T newOffset;        /* New VVR/CVVR/SVVR offset. */
{
  CDFstatus pStatus = CDF_OK;
  struct VXRstruct64 VXR; int entryN; Int32 irType;
  /****************************************************************************
  * While more VXRs...
  ****************************************************************************/
  while (vxrOffset != (OFF_T) ZERO_OFFSET64) {
    /**************************************************************************
    * Read the VXR.
    **************************************************************************/
    if (!sX(ReadVXR64(fp,vxrOffset,
		      VXR_RECORD,&VXR,
		      VXR_NULL),&pStatus)) return pStatus;
    /**************************************************************************
    * Search through index entries...
    **************************************************************************/
    for (entryN = 0; entryN < VXR.NusedEntries; entryN++) {
       if (VXR.First[entryN] <= firstRec && lastRec <= VXR.Last[entryN]) {
	 if (!sX(ReadIrType64(fp,
			      VXR.Offset[entryN],
			      &irType),&pStatus)) return pStatus;
	 if (irType == VXR_) return ModIndexOffset_r_64(fp,VXR.Offset[entryN],
						        firstRec,lastRec,
						        newOffset);
	 if (VXR.First[entryN] == firstRec && lastRec == VXR.Last[entryN]) {
	   VXR.Offset[entryN] = newOffset;
	   if (!sX(WriteVXR64(fp,vxrOffset,
			      VXR_RECORD,&VXR,
			      VXR_NULL),&pStatus)) return pStatus;
	   return pStatus;
	 }
	 return CDF_INTERNAL_ERROR;             /* or CORRUPTED_V3_CDF? */
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
OFF_T nValues;          /* Number of values being written. */
OFF_T offset;           /* If not full record(s), byte offset in record at
			   which to write. */
Logical fullRecord;     /* Full record(s) being written? */
{
  CDFstatus pStatus = CDF_OK; Logical found;
  Int32 recCount, recordOffsetInStage, maxRecThisBlock;
  Int32 nextRec, writeTo; void *padBuffer; int how;
  Int32 recNum = firstRec; Byte1 *tBuffer = (Byte1 *) buffer;
  OFF_T tOffset, numElems;
  /****************************************************************************
  * From first to last record...
  ****************************************************************************/
  while (recNum <= lastRec) {
     /*************************************************************************
     * Check if this record is in the staging area.
     *************************************************************************/
     if (INCLUSIVE(Var->stage.firstRec,recNum,Var->stage.lastRec)) {
       recordOffsetInStage = recNum - Var->stage.firstRec;
       tOffset = Var->stage.areaOffset64;
       tOffset += (recordOffsetInStage * Var->NphyRecBytes);
       if (fullRecord) {
	 if (Var->stage.dotOffset64 == (OFF_T) NO_OFFSET64) {
	   maxRecThisBlock = Var->stage.firstRec + Var->blockingFactor - 1;
	   if (!sX(NextRecord64(CDF,Var->VDRoffset64,
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
	 numElems = (OFF_T) recCount * Var->NphyRecElems;
       }
       else {
	 writeTo = recNum;
	 recCount = 1;
	 numElems = nValues * Var->NvalueElems;
	 tOffset += offset;
       }
       if (!sX(WriteVarElems64(Var,CDF->stage.fp,
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
       tOffset = Var->stage.areaOffset64;
       tOffset += (recordOffsetInStage * Var->NphyRecBytes);
       if (fullRecord) {
	 writeTo = MINIMUM(Var->stage.lastRec,lastRec);
	 recCount = writeTo - recNum + 1;
	 numElems = (OFF_T) recCount * Var->NphyRecElems;
       }
       else {
	 writeTo = recNum;
	 recCount = 1;
	 numElems = nValues * Var->NvalueElems;
	 tOffset += offset;
       }
       if (!sX(WriteVarElems64(Var,CDF->stage.fp,
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
       if (Var->stage.dotOffset64 == (OFF_T) NO_OFFSET64) {
	 if (recNum == Var->stage.lastRec + 1) {
	   maxRecThisBlock = Var->stage.firstRec + Var->blockingFactor - 1;
	   if (recNum <= maxRecThisBlock) {
	     recordOffsetInStage = recNum - Var->stage.firstRec;
	     tOffset = Var->stage.areaOffset64;
	     tOffset += (recordOffsetInStage * Var->NphyRecBytes);
	     if (fullRecord) {
	       if (!sX(NextRecord64(CDF,Var->VDRoffset64,
				    Var->zVar,recNum,
				    &nextRec,&found),&pStatus)) return pStatus;
	       if (!found)
		 writeTo = MINIMUM(maxRecThisBlock,lastRec);
	       else {
		 Int32 prevRecN = nextRec - 1;
		 writeTo = MINIMUMof3(maxRecThisBlock,prevRecN,lastRec);
	       }
	       recCount = writeTo - recNum + 1;
	       numElems = (OFF_T) recCount * Var->NphyRecElems;
	     }
	     else {
	       if (!sX(BuildPadBuffer64(CDF,Var,INT32_ONE,
				        &how,&padBuffer,
				        TRUE),&pStatus)) return pStatus;
	       if (!sX(WritePadValues64(Var,CDF->stage.fp,
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
	     if (!sX(WriteVarElems64(Var,CDF->stage.fp,
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
     if (!sX(FlushStage64(CDF,Var),&pStatus)) return pStatus;
     tOffset = Var->stage.areaOffset64;
     if (fullRecord) {
       maxRecThisBlock = recNum + Var->blockingFactor - 1;
       if (!sX(NextRecord64(CDF,Var->VDRoffset64,Var->zVar,
			    recNum,&nextRec,&found),&pStatus)) return pStatus;
       if (!found)
	 writeTo = MINIMUM(maxRecThisBlock,lastRec);
       else {
	 Int32 prevRecN = nextRec - 1;
	 writeTo = MINIMUMof3(maxRecThisBlock,prevRecN,lastRec);
       }
       recCount = writeTo - recNum + 1;
       numElems = (OFF_T) recCount * Var->NphyRecElems;
     }
     else {
       if (!sX(BuildPadBuffer64(CDF,Var,INT32_ONE,
			        &how,&padBuffer,
			        TRUE),&pStatus)) return pStatus;
       if (!sX(WritePadValues64(Var,CDF->stage.fp,
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
     if (!sX(WriteVarElems64(Var,CDF->stage.fp,
			     tOffset,numElems,
			     tBuffer),&pStatus)) return pStatus;
     Var->stage.firstRec = recNum;
     Var->stage.lastRec = writeTo;
     Var->stage.dotOffset64 = (OFF_T) NO_OFFSET64;
     Var->stage.modified = TRUE;
     tBuffer += (size_t) (numElems * Var->NelemBytes);
     recNum += recCount;
  }
  return pStatus;
}

/******************************************************************************
* CalcCompressionPct64.
******************************************************************************/

STATICforIDL CDFstatus CalcCompressionPct64 (CDF, vdrOffset, zVar, cPct)
struct CDFstruct *CDF;
OFF_T vdrOffset;
Logical zVar;
long *cPct;
{
  CDFstatus pStatus = CDF_OK; OFF_T vxrOffset;
  OFF_T uTotal = 0, cTotal = 0; Int32 nPhyRecBytes;
  /****************************************************************************
  * Calculate the number of bytes per physical record.
  ****************************************************************************/
  if (!sX(CalcPhyRecBytes64(CDF,vdrOffset,
			    zVar,&nPhyRecBytes),&pStatus)) return pStatus;
  /****************************************************************************
  * Read the offset of the first VXR (return 0% if no VXRs)...
  ****************************************************************************/
  if (!sX(ReadVDR64(CDF,CDF->fp,vdrOffset,zVar,
		    VDR_VXRHEAD,&vxrOffset,
		    VDR_NULL),&pStatus)) return pStatus;
  if (vxrOffset == (OFF_T) ZERO_OFFSET64) {
    *cPct = 0;
    return pStatus;
  }
  /****************************************************************************
  * ...and start there.
  ****************************************************************************/
  if (!sX(CalcCompressionPct_r_64(CDF->fp,vxrOffset,
			          nPhyRecBytes,
			          &uTotal,&cTotal),&pStatus)) return pStatus;
  /****************************************************************************
  * Calculate percentage.
  ****************************************************************************/
  *cPct = (long) (((100.0*cTotal) / uTotal) + 0.5);
  return pStatus;
}

static CDFstatus CalcCompressionPct_r_64 (fp, vxrOffset, nPhyRecBytes, uTotal,
				          cTotal)
vFILE *fp;
OFF_T vxrOffset;
Int32 nPhyRecBytes;
OFF_T *uTotal;
OFF_T *cTotal;
{
  CDFstatus pStatus = CDF_OK; struct VXRstruct64 VXR; int entryN;
  Int32 nRecords, irType; OFF_T uSize, irSize;
  /****************************************************************************
  * While more VXRs...
  ****************************************************************************/
  while (vxrOffset != (OFF_T) ZERO_OFFSET64) {
    /**************************************************************************
    * Read the VXR.
    **************************************************************************/
    if (!sX(ReadVXR64(fp,vxrOffset,
		      VXR_RECORD,&VXR,
		      VXR_NULL),&pStatus)) return pStatus;
    /**************************************************************************
    * Scan entries...
    **************************************************************************/
    for (entryN = 0; entryN < VXR.NusedEntries; entryN++) {
       if (!sX(ReadIrType64(fp,VXR.Offset[entryN],&irType),&pStatus)) return
								    pStatus;
       switch (irType) {
	 case VXR_:
	   if (!sX(CalcCompressionPct_r_64(fp,VXR.Offset[entryN],
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
	   if (!sX(ReadIrSize64(fp,VXR.Offset[entryN],&irSize),&pStatus)) 
								return pStatus;
	   *cTotal += irSize - BOO(irType == CVVR_,
				   CVVR_BASE_SIZE64,
				   VVR_BASE_SIZE64);
	   break;
	 default:
	   return CORRUPTED_V3_CDF;
       }
    }
    vxrOffset = VXR.VXRnext;
  }
  return pStatus;
}

/******************************************************************************
* CalcPhyRecBytes64.
******************************************************************************/

STATICforIDL CDFstatus CalcPhyRecBytes64 (CDF, vdrOffset, zVar, nPhyRecBytes)
struct CDFstruct *CDF;
OFF_T vdrOffset;
Logical zVar;
Int32 *nPhyRecBytes;
{
  CDFstatus pStatus = CDF_OK; int dimN; Int32 dataType, numElems;
  Int32 numDims, dimSizes[CDF_MAX_DIMS], dimVarys[CDF_MAX_DIMS];
  if (!sX(CalcDimParms64(CDF,vdrOffset,zVar,
		         &numDims,dimSizes,dimVarys),&pStatus)) return pStatus;
  if (!sX(ReadVDR64(CDF,CDF->fp,vdrOffset,zVar,
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
  Int32 readTo, recCount, recX, prevRecN;
  Int32 nextRec, padTo; OFF_T tOffset, nPadValues, numElems;
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
      numElems = (OFF_T) recCount * Var->NphyRecElems;
      recX = recNum - Var->stage.firstRec;
      tOffset = Var->stage.areaOffset64 + (OFF_T) (recX * Var->NphyRecBytes);
      if (!sX(ReadVarElems64(Var,CDF->stage.fp,
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
      if (!sX(SearchForRecord64(CDF,Var->VDRoffset64,
			        Var->zVar,recNum,
			        &firstRecInVVR,
			        &lastRecInVVR,
			        &tOffset,&found),&pStatus)) return pStatus;
      if (found) {
	readTo = MINIMUMof3(Var->maxRec,lastRec,lastRecInVVR);
	recCount = readTo - recNum + 1;
	numElems = (OFF_T) recCount * Var->NphyRecElems;
	recX = recNum - firstRecInVVR;
	tOffset += VVR_BASE_SIZE64 + (OFF_T) (recX * Var->NphyRecBytes);
	if (!sX(ReadVarElems64(Var,CDF->fp,tOffset,
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
      if (!sX(NextRecord64(CDF,Var->VDRoffset64,Var->zVar,
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
      if (!sX(PrevRecord64(CDF,Var->VDRoffset64,Var->zVar,
			   MINIMUM(recNum,Var->maxRec),
			   &prevRec,&found),&pStatus)) return pStatus;
      if (found) {
	if (EXCLUSIVE(prevRec,Var->stage.lastRec,recNum)) {
	  recX = Var->stage.lastRec - Var->stage.firstRec;
	  tOffset = Var->stage.areaOffset64 + (OFF_T) (recX * Var->NphyRecBytes);
	  if (!sX(ReadVarElems64(Var,CDF->stage.fp,
			         tOffset,Var->NphyRecElems,
			         tBuffer),&pStatus)) return pStatus;
	}
	else {
	  if (!sX(RecordByteOffset64(CDF,Var,
				     prevRec,
				     &tOffset),&pStatus)) return pStatus;
	  if (!sX(ReadVarElems64(Var,CDF->fp,tOffset,
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
	tOffset = Var->stage.areaOffset64 + (OFF_T) (recX * Var->NphyRecBytes);
	if (!sX(ReadVarElems64(Var,CDF->stage.fp,
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
    nPadValues = (OFF_T) recCount * Var->NphyRecValues;
    if (!sX(PadBuffer64(CDF,Var,nPadValues,tBuffer),&pStatus)) return pStatus;
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
OFF_T offset;
OFF_T nValues;
void *buffer;
{
  CDFstatus pStatus = CDF_OK; Logical found;
  Int32 firstRec, prevRec; OFF_T tOffset;
  OFF_T numElems = nValues * Var->NvalueElems;
  /****************************************************************************
  * If the record is in the staging area...
  ****************************************************************************/
  if (INCLUSIVE(Var->stage.firstRec,recNum,Var->stage.lastRec)) {
    tOffset = Var->stage.areaOffset64;
    tOffset += (OFF_T) (Var->NphyRecBytes * (recNum - Var->stage.firstRec));
    tOffset += offset;
    if (!sX(ReadVarElems64(Var,CDF->stage.fp,
			   tOffset,numElems,buffer),&pStatus)) return pStatus;
    return pStatus;
  }
  /****************************************************************************
  * If the record exists and has been written (ie. not just allocated)...
  ****************************************************************************/
  if (recNum <= Var->maxRec) {
    if (!sX(SearchForRecord64(CDF,Var->VDRoffset64,
			      Var->zVar,recNum,&firstRec,
			      NULL,&tOffset,&found),&pStatus)) return pStatus;
    if (found) {
      tOffset += (OFF_T) VVR_BASE_SIZE64;
      tOffset += (OFF_T) Var->NphyRecBytes * (recNum - firstRec);
      tOffset += offset;
      if (!sX(ReadVarElems64(Var,CDF->fp,
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
    if (!sX(PrevRecord64(CDF,Var->VDRoffset64,Var->zVar,
		         MINIMUM(recNum,Var->maxRec),
		         &prevRec,&found),&pStatus)) return pStatus;
    /**************************************************************************
    * If so, also make sure that the last record in the staging area isn't
    * really the previous record to use.
    **************************************************************************/
    if (found) {
      if (EXCLUSIVE(prevRec,Var->stage.lastRec,recNum)) {
	Int32 recNumInStage = Var->stage.lastRec - Var->stage.firstRec;
	tOffset = Var->stage.areaOffset64;
	tOffset += (OFF_T) Var->NphyRecBytes * recNumInStage;
	tOffset += offset;
	if (!sX(ReadVarElems64(Var,CDF->stage.fp,tOffset,
			       numElems,buffer),&pStatus)) return pStatus;
      }
      else {
	if (!sX(RecordByteOffset64(CDF,Var,
				   prevRec,
				   &tOffset),&pStatus)) return pStatus;
	tOffset += offset;
	if (!sX(ReadVarElems64(Var,CDF->fp,tOffset,
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
      tOffset = Var->stage.areaOffset64;
      tOffset += (OFF_T) Var->NphyRecBytes * recNumInStage;
      tOffset += offset;
      if (!sX(ReadVarElems64(Var,CDF->stage.fp,
			     tOffset,numElems,buffer),&pStatus)) return pStatus;
      sX (VIRTUAL_RECORD_DATA, &pStatus);
      return pStatus;
    }
  }
  /****************************************************************************
  * Pad the buffer with the variable's pad value.  Note that this is also done
  * if the variable is sRecords.PREV but a previous record does not exist.
  ****************************************************************************/
  if (!sX(PadBuffer64(CDF,Var,nValues,buffer),&pStatus)) return pStatus;
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
  Int32 readTo, recCount, recX, nextRec;
  Int32 recNum = firstRec, prevRec, padTo;
  Byte1 *tBuffer = (Byte1 *) buffer, *destBuffer;
  OFF_T tOffset, nPadValues, numElems;
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
      numElems = (OFF_T) recCount * Var->NphyRecElems;
      recX = recNum - Var->stage.firstRec;
      tOffset = Var->stage.areaOffset64 + (OFF_T) (recX * Var->NphyRecBytes);
      if (!sX(ReadVarElems64(Var,CDF->stage.fp,
			     tOffset,numElems,tBuffer),&pStatus)) return pStatus;
      tBuffer += (size_t) (recCount * Var->NphyRecBytes);
      recNum += recCount;
      continue;
    }
    /**************************************************************************
    * Determine which records need to be padded.
    **************************************************************************/
    if (!sX(NextRecord64(CDF,Var->VDRoffset64,Var->zVar,
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
      if (!sX(PrevRecord64(CDF,Var->VDRoffset64,Var->zVar,
			   recNum,&prevRec,&found),&pStatus)) return pStatus;
      if (!found) prevRec = NO_RECORD;
      if (EXCLUSIVE(prevRec,
		    Var->stage.lastRec,
		    recNum)) prevRec = Var->stage.lastRec;
      if (prevRec > NO_RECORD) {
	if (!sX(BringToStage(CDF,Var,prevRec,NULL),&pStatus)) return pStatus;
	recX = prevRec - Var->stage.firstRec;
	tOffset = Var->stage.areaOffset64 + (OFF_T) (recX * Var->NphyRecBytes);
	if (!sX(ReadVarElems64(Var,CDF->stage.fp,
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
    nPadValues = (OFF_T) recCount * Var->NphyRecValues;
    if (!sX(PadBuffer64(CDF,Var,nPadValues,tBuffer),&pStatus)) return pStatus;
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
OFF_T offset;
OFF_T nValues;
void *buffer;
{
  CDFstatus pStatus = CDF_OK;
  OFF_T numElems = (OFF_T) nValues * Var->NvalueElems;
  Int32 prevRec, recX; Logical found;
  OFF_T tOffset;
  /****************************************************************************
  * Try to bring the record to the staging area.
  ****************************************************************************/
  if (!sX(BringToStage(CDF,Var,recNum,&found),&pStatus)) return pStatus;
  if (found) {
    tOffset = Var->stage.areaOffset64;
    tOffset += (OFF_T) Var->NphyRecBytes * (recNum - Var->stage.firstRec);
    tOffset += offset;
    if (!sX(ReadVarElems64(Var,CDF->stage.fp,
			   tOffset,numElems,buffer),&pStatus)) return pStatus;
    return pStatus;
  }
  /****************************************************************************
  * If sRecords.PREV...
  ****************************************************************************/
  if (Var->prevIfMissing) {
    if (!sX(PrevRecord64(CDF,Var->VDRoffset64,
		         Var->zVar,recNum,
		         &prevRec,&found),&pStatus)) return pStatus;
    if (!found) prevRec = NO_RECORD;
    if (EXCLUSIVE(prevRec,
		  Var->stage.lastRec,
		  recNum)) prevRec = Var->stage.lastRec;
    if (prevRec > NO_RECORD) {
      if (!sX(BringToStage(CDF,Var,prevRec,NULL),&pStatus)) return pStatus;
      recX = Var->stage.lastRec - Var->stage.firstRec;
      tOffset = Var->stage.areaOffset64 + (OFF_T) (Var->NphyRecBytes * recX) + 
		offset;
      if (!sX(ReadVarElems64(Var,CDF->stage.fp,
			     tOffset,numElems,buffer),&pStatus)) return pStatus;
      sX (VIRTUAL_RECORD_DATA, &pStatus);
      return pStatus;
    }
  }
  /****************************************************************************
  * Pad the buffer with the variable's pad value.  Note that this is also done
  * if the variable is sRecords.PREV but a previous record does not exist.
  ****************************************************************************/
  if (!sX(PadBuffer64(CDF,Var,nValues,buffer),&pStatus)) return pStatus;
  sX (VIRTUAL_RECORD_DATA, &pStatus);
  return pStatus;
}

/******************************************************************************
* CheckLFS.
******************************************************************************/

VISIBLE_PREFIX CDFstatus CheckLFS (CDFname, isLFS, CDFfullName)
char *CDFname;
Logical *isLFS;
char *CDFfullName;
{
  CDFstatus pStatus = CDF_OK;
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
  int fp;
#else
  FILE *fp;
#endif
  Int32 temp, magicNumber1;
  OFF_T length64 = (OFF_T) 0;
  Logical upper_case_ext, version_numbers, no_append;
  char CDFpathX[DU_MAX_PATH_LEN+1], CDFpathT[DU_MAX_PATH_LEN+1];
  size_t pathLen;

  if (CDFfullName != NULL) strcpyX (CDFfullName, CDFname, DU_MAX_PATH_LEN);
  /**************************************************************************
  * Open CDF file.
  **************************************************************************/
  if (strlen(CDFname) > (size_t) CDF_PATHNAME_LEN) {
    if (!sX(CDF_NAME_TRUNC,&pStatus)) return pStatus;
  }
  strcpyX (CDFpathT, CDFname, DU_MAX_PATH_LEN);
#if STRIP_TRAILING_BLANKS_FROM_CDFPATH
  StripTrailingBlanks (CDFpathT);
#endif
#if defined(vms) || defined(dos)
  MakeUpperString (CDFpathT);
#endif
/*  if (!ValidCDFname(CDFpathT)) return BAD_CDF_NAME; */
  if (!sX(FindCDF(CDFpathT,&no_append,
                  &upper_case_ext,
                  &version_numbers),&pStatus)) return pStatus;
  pathLen = strlen(CDFpathT);
  if (((pathLen > 4) && StrStrIgCaseX(CDFpathT+(pathLen-4), ".cdf")) ||
      ((pathLen > 6) && StrStrIgCaseX(CDFpathT+(pathLen-6), ".cdf;1")))
    strcpyX (CDFpathX, CDFpathT, DU_MAX_PATH_LEN);
  else
    BuildFilePath (CDFt, CDFpathT, no_append, upper_case_ext, version_numbers,
                   INT32_ZERO, CDFpathX);
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
  fp = FOPEN64 (CDFpathX, _O_RDONLY);
  if (fp == EOF) return CDF_OPEN_ERROR;
#else
  fp = (FILE *) FOPEN64 (CDFpathX, READ_ONLY_a_mode);
  if (fp == NULL) return CDF_OPEN_ERROR;
#endif
  if (FSEEK64(fp,(OFF_T)0,vSEEK_END) == EOF) {
    FCLOSE64 (fp);
    return CDF_OPEN_ERROR;
  }
  length64 = FTELL64 (fp);
  if (length64 == 0) /* A new one... Must be a LFS. */
    *isLFS = TRUE;
  else { /* Existing one... Check its magic number at the 1st 4-byte. */
    if (FSEEK64(fp,0,vSEEK_SET) == EOF) return CDF_OPEN_ERROR;
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
    if (FREAD64(fp,&magicNumber1,(unsigned int)4) != 4)
#else
    if (FREAD64(&magicNumber1,(size_t)4,(size_t)1,fp) != 1)
#endif
      return CDF_READ_ERROR;
#ifndef NETWORKbyteORDERcpu
    temp = magicNumber1;
    REVERSE4bIO (&temp, &magicNumber1)
#endif
    if (magicNumber1 == V3magicNUMBER_1)
      *isLFS = TRUE;
    else if (magicNumber1 == V2magicNUMBER_1 ||
	     magicNumber1 == V2magicNUMBER_1pre)
      *isLFS = FALSE;
    else {
      *isLFS = TRUE;
      pStatus = NOT_A_CDF_OR_NOT_SUPPORTED;       
    }
  }
  FCLOSE64 (fp);
  if (CDFfullName != NULL) strcpyX (CDFfullName, CDFpathX, DU_MAX_PATH_LEN);
  return pStatus;
}

/******************************************************************************
* StrStrIgCaseX.
******************************************************************************/

VISIBLE_PREFIX Logical StrStrIgCaseX (string, chkstring)
char *string;
char *chkstring;
{
  int stringL = strlen(string);
  int chkstringL = strlen(chkstring);
  int i;
  if (stringL == 0 || chkstringL == 0) return FALSE;
  if (stringL != chkstringL) return FALSE;
  for (i = 0; i < stringL; i++) {
     if (MakeLower(chkstring[i]) != MakeLower(string[i])) return FALSE;
  }
  return TRUE;
}

/******************************************************************************
*  Check if a given string ends with the provided substring, case sensitive.
*  TRUE if s1 ends with s2.  Otherwise, FALSE is returned.
******************************************************************************/

int EndsWith (s1, s2)
char *s1;
char *s2;
{
    int  i;
    char *ps1, *ps2;
    
    if (strlen(s2) > strlen(s1))
        return FALSE;
        
    ps1 = s1 + strlen(s1) - strlen(s2);
    ps2 = s2;

    for (i=0; i < (int) strlen(s2); i++)
        if (*(ps1++) != *(ps2++))
            return FALSE;

    return TRUE;
}

/******************************************************************************
*  Check if a given string ends with the provided substring, ignoring case.
*  TRUE if s1 ends with s2.  Otherwise, FALSE is returned.
******************************************************************************/

int EndsWithIgCase (s1, s2)
char *s1;
char *s2;
{
    int  i;
    char *ps1, *ps2;

    if (strlen(s2) > strlen(s1))
        return FALSE;

    ps1 = s1 + strlen(s1) - strlen(s2);
    ps2 = s2;

    for (i=0; i < (int) strlen(s2); i++) {
        if (MakeLower(*ps1) != MakeLower(*ps2))
            return FALSE;
        ps1++;
        ps2++;
    }
    return TRUE;
}

/******************************************************************************
*  Find the last occurence of a substring within a given string, case
*  sensitive.  If the substring is found, it returns the offset where the
*  substring is at in the original string. If the substring is not found, it
*  returns -1.
******************************************************************************/

VISIBLE_PREFIX int StrLaststr (s1, s2)
char *s1;
char *s2;
{
    char *sc2, *psc1, *ps1;

    if (*s2 == '\0')
        return -1;

    ps1 = s1 + strlen(s1);

    while(ps1 != s1) {
      --ps1;
      for (psc1 = ps1, sc2 = s2; ; )
          if (*(psc1++) != *(sc2++))
            break;
          else if (*sc2 == '\0')
            return (int) (ps1 - s1);
    }
    return -1;
}

/******************************************************************************
*  Find the last occurence of a substring within a given string, ignoring case.
*  If the substring is found, it returns the offset where the substring is at
*  in the original string. If the substring is not found, it returns -1.
******************************************************************************/
        
VISIBLE_PREFIX int StrLaststrIgCase (s1, s2)
char *s1;
char *s2;
{
    char *sc2, *psc1, *ps1;

    if (*s2 == '\0')
        return -1;

    ps1 = s1 + strlen(s1);

    while (ps1 != s1) {
      --ps1;
      for (psc1 = ps1, sc2 = s2; ; ) {
          if (MakeLower(*psc1) != MakeLower(*sc2))
            break;
          else if (*sc2 == '\0')
            return (int) (ps1 - s1);
          psc1++;
          sc2++;
      } 
    }
    return -1;
}     
      
/******************************************************************************
*  Remove the ".cdf" file extension from the given file name if it's there.
*  It ignores the case. 
*  Example: 
*      mydata.cdf => mydata
******************************************************************************/

VISIBLE_PREFIX void RemoveCDFFileExtension (fileName, dstPath)
char *fileName;         /* CDF file name. */
char *dstPath;          /* The string holding file name without extension. */
{
    int ptr = -1;

    strcpyX (dstPath, fileName, CDF_PATHNAME_LEN);
    if (EndsWithIgCase (dstPath, ".cdf")) {
      ptr = StrLaststrIgCase(dstPath, ".cdf");
      if (ptr != -1) ((char *) dstPath)[ptr] = (char) '\0';
    }
    return;
}

/******************************************************************************
* CDFgetFileBackwardEnvVar.
******************************************************************************/

VISIBLE_PREFIX int CDFgetFileBackwardEnvVar ()
{
  char *bk = NULL;
#if defined(vms)
  bk = getenv("CDF$FILEBACKWARD");
#else
  bk = getenv("CDF_FILEBACKWARD");
#endif
  if (bk != NULL) {
    if (strlen(bk) == 0) return 0;
    if (StrStrIgCaseX(bk, "TRUE")) return 1;
  }
  return 0;
}

/******************************************************************************
* CDFgetChecksumEnvVar.
******************************************************************************/

VISIBLE_PREFIX int CDFgetChecksumEnvVar ()
{
  char *bk = NULL;
#if defined(vms)
  bk = getenv("CDF$CHECKSUM");
#else
  bk = getenv("CDF_CHECKSUM");
#endif
  if (bk != NULL) {
    if (strlen(bk) == 0) return 0;
    if (StrStrIgCaseX(bk, "none") || StrStrIgCaseX(bk, "no")) return 0;
    if (StrStrIgCaseX(bk, "md5")) return 1;
  }
  return 0;
}

/******************************************************************************
* CDFChecksumMethod.
******************************************************************************/

VISIBLE_PREFIX long CDFChecksumMethod (Int32 flags)
{
  int md5 = 0;
  int other = 0;

  if (!BITSET(flags,CDR_CHECKSUM_BIT)) return NONE_CHECKSUM;
  md5 = (int) BITSET(flags,CDR_CHECKSUM_MD5_BIT);
  other = (int) BITSET(flags,CDR_CHECKSUM_OTHER_BIT); 
  if (md5 == 1) return MD5_CHECKSUM;
  else if (other == 1) return OTHER_CHECKSUM;
  return NONE_CHECKSUM;
}

/******************************************************************************
* CDFsetValidate.
******************************************************************************/

VISIBLE_PREFIX void CDFsetValidate (long validate)
{
  if (validate == VALIDATEFILEon) {
#if !defined(vms)
#if !defined(sun) && (!defined(WIN32) && !defined(__MINGW32__)) && \
    !defined(sgi)
    setenv ("CDF_VALIDATE", "yes", 1);
#else
#  if !defined(WIN32)
      putenv ("CDF_VALIDATE=yes");
#  else
	  _putenv ("CDF_VALIDATE=yes");
#  endif
#endif
#else 
    setenv ("CDF$VALIDATE", "yes", 1);
#endif 
  } else {
#if !defined(vms)
#if !defined(sun) && (!defined(WIN32) && !defined(__MINGW32__)) && \
    !defined(sgi)
    setenv ("CDF_VALIDATE", "no", 1);
#else
#  if !defined(WIN32)
      putenv ("CDF_VALIDATE=no");
#  else
      _putenv ("CDF_VALIDATE=no");
#  endif
#endif
#else 
    setenv ("CDF$VALIDATE", "no", 1);
#endif 
  }
}

/******************************************************************************
* CDFgetValidate.
* Return: 1 if not set or set to yes (the default)
*         0 if set to no or none
******************************************************************************/

VISIBLE_PREFIX int CDFgetValidate ()
{
  char *vad = NULL;
#if defined(vms)
  vad = getenv("CDF$VALIDATE");
#else
  vad = getenv("CDF_VALIDATE");
#endif
  if (vad != NULL) {
    if (strlen(vad) == 0) return 0;
    if (StrStrIgCaseX(vad, "none") || StrStrIgCaseX(vad, "no")) return 0;
    if (StrStrIgCaseX(vad, "yes")) return 1;
  }
  return 1;
}

/******************************************************************************
* CDFgetValidateDebug.
* Return: 0 if not set or set to no or none (the default)
*         1 if set to yes
******************************************************************************/

VISIBLE_PREFIX int CDFgetValidateDebug ()
{
  char *vad = NULL;
#if defined(vms)
  vad = getenv("CDF$VALIDATE$DEBUG");
#else
  vad = getenv("CDF_VALIDATE_DEBUG");
#endif
  if (vad != NULL) {
    if (strlen(vad) == 0) return 0;
    if (StrStrIgCaseX(vad, "none") || StrStrIgCaseX(vad, "no")) return 0;
    if (StrStrIgCaseX(vad, "yes")) return 1;
  }
  return 0;
}

