/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the 
* Administrator of the National Aeronautics and Space Administration. 
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                CDF library allocation functions.
*
*  Version 1.0, 22-Jul-96, Hughes STX.
*
*  Modification history:
*
*   V1.0  22-Jul-96, J Love     Original version.
*   V2.0   7-May-10, M Liu      Modified to update file's length when a 
*                               VVR/CVVR is allocated, if needed. 
*
******************************************************************************/

#include "cdflib.h"
#include "cdfrev.h"

/******************************************************************************
* Local function prototypes.
******************************************************************************/

static CDFstatus InsertRecords PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, struct AllocStruct alloc,
  Int32 vxrStart, int *count, Int32 *toRec
));
static CDFstatus AppendRecords PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, struct AllocStruct *alloc,
  Int32 vxrStart, int *count
));
static CDFstatus InsertEntry PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, struct AllocStruct *alloc,
  Int32 vxrOffset, int atEntryN, int *count
));
static void InsertAtEntry PROTOARGs((
  Int32 first, Int32 last, Int32 offset, struct VXRstruct *VXR, int atEntryN, 
  Logical *push, Int32 *pushFirst, Int32 *pushLast, Int32 *pushOffset
));
static CDFstatus AppendEntry PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, struct AllocStruct *alloc,
  Int32 vxrOffset, int *count
));
static CDFstatus PadSparseRecords PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 offset, Int32 nRecords
));
static CDFstatus CountVXRs PROTOARGs((
  struct CDFstruct *CDF, Int32 vxrStart, int *count
));
static void InitNewVXRx PROTOARGs((struct VXRstruct *VXRx));
static CDFstatus FirstRecords PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, struct AllocStruct *alloc
));
static CDFstatus ExtendLevel PROTOARGs((
  struct CDFstruct *CDF, Int32 vxrOffset, int count, Int32 *vxrOffsetHead,
  Int32 *lastNew, int *countNew
));
static CDFstatus CreateLevel PROTOARGs((
  struct CDFstruct *CDF, Int32 vxrOffset, int count, Int32 *vxrOffsetHead,
  int *newCount
));
static CDFstatus AllocateVR PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, struct AllocStruct *alloc,
  Int32 *offset
));
static CDFstatus AllocateVVR PROTOARGs((
  struct CDFstruct *CDF, struct VarStruct *Var, Int32 nRecords,
  Int32 *vvrOffset
));
static CDFstatus AllocateCVVR PROTOARGs((
  struct CDFstruct *CDF, Int32 cSize, Int32 xSize, Int32 *cvvrOffset
));

/******************************************************************************
* AllocateRecords.
******************************************************************************/

STATICforIDL CDFstatus AllocateRecords (CDF, Var, alloc)
struct CDFstruct *CDF;
struct VarStruct *Var;
struct AllocStruct alloc;
{
  CDFstatus pStatus = CDF_OK;
  Int32 vxrHead, toRec, vxrHeadNew; int count, newCount;
  /****************************************************************************
  * Read the head of the VXR tree/list.
  ****************************************************************************/
  if (!sX(ReadVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
		  VDR_VXRHEAD,&vxrHead,
		  VDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * If no records allocated yet...
  ****************************************************************************/
  if (vxrHead == ZERO_OFFSET) {
    if (!sX(FirstRecords(CDF,Var,&alloc),&pStatus)) return pStatus;
    return pStatus;
  }
  /****************************************************************************
  * Allocate records starting at the VXRhead until done.
  ****************************************************************************/
  while (alloc.first <= alloc.last) {
    /**************************************************************************
    * Try to insert all of the records.
    **************************************************************************/
    if (!sX(InsertRecords(CDF,Var,
			  alloc,vxrHead,
			  &count,&toRec),&pStatus)) return pStatus;
    /**************************************************************************
    * Check to see if another level of indexing needs to be added.  Multiple
    * levels may be created here because prior to CDF V2.6 a hierarchical
    * indexing scheme was not used.
    **************************************************************************/
    while (count > NUM_VXRx_ENTRIES - 1) {
      if (!sX(CreateLevel(CDF,vxrHead,count,
			  &vxrHeadNew,&newCount),&pStatus)) return pStatus;
      vxrHead = vxrHeadNew;
      if (!sX(WriteVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
		       VDR_VXRHEAD,&vxrHead,
		       VDR_NULL),&pStatus)) return pStatus;
      count = newCount;
    }
    /**************************************************************************
    * Adjust the allocation request based on the record actually allocated to.
    **************************************************************************/
    alloc.first = toRec + 1;
  }
  /****************************************************************************
  * Update the VXR tail in case it changed.  This assumes that the VXR tail
  * field in the VDR is not used by any of the record allocation routines.
  ****************************************************************************/
  if (!sX(UpdateVXRtailInVDR(CDF,Var),&pStatus)) return pStatus;
  return pStatus;
}

/******************************************************************************
* InsertRecords.
******************************************************************************/

static CDFstatus InsertRecords (CDF, Var, alloc, vxrStart, count, toRec)
struct CDFstruct *CDF;
struct VarStruct *Var;
struct AllocStruct alloc;
Int32 vxrStart;
int *count;
Int32 *toRec;
{
  CDFstatus pStatus = CDF_OK; struct VXRstruct VXR;
  Int32 vxrOffset = vxrStart, irType, vxrOffsetNew, lastNew;
  int entryN, belowCount, belowCountNew, toEndCount, countNew;
  /****************************************************************************
  * Scan linked list of VXRs...
  ****************************************************************************/
  *count = 0;
  while (vxrOffset != ZERO_OFFSET) {
    /**************************************************************************
    * Read VXR.
    **************************************************************************/
    if (!sX(ReadVXR(CDF->fp,vxrOffset,
		    VXR_RECORD,&VXR,
		    VXR_NULL),&pStatus)) return pStatus;
    (*count)++;
    /**************************************************************************
    * Scan VXR entries...
    **************************************************************************/
    for (entryN = 0; entryN < VXR.NusedEntries; entryN++) {
       if (alloc.first < VXR.First[entryN]) {
	 if (!sX(ReadIrType(CDF->fp,
			    VXR.Offset[entryN],
			    &irType),&pStatus)) return pStatus;
	 switch (irType) {
	   case VXR_: {
	     Int32 prevRecN = VXR.First[entryN] - 1;
	     alloc.last = MINIMUM(alloc.last,prevRecN);
	     if (!sX(InsertRecords(CDF,Var,alloc,
				   VXR.Offset[entryN],
				   &belowCount,
				   toRec),&pStatus)) return pStatus;
	     VXR.First[entryN] = alloc.first;
	     while (belowCount > NUM_VXRx_ENTRIES - 1) {
	       if (!sX(CreateLevel(CDF,VXR.Offset[entryN],
				   belowCount,&VXR.Offset[entryN],
				   &belowCountNew),&pStatus)) return pStatus;
	       belowCount = belowCountNew;
	     }
	     if (!sX(WriteVXR(CDF->fp,vxrOffset,
			      VXR_RECORD,&VXR,
			      VXR_NULL),&pStatus)) return pStatus;
	     if (!sX(CountVXRs(CDF,VXR.VXRnext,
			       &toEndCount),&pStatus)) return pStatus;
	     *count += toEndCount;
	     return pStatus;
	   }
	   case VVR_:
	   case CVVR_:
	     if (alloc.last > VXR.First[entryN] - 1) {
	       if (EXISTSisBAD(&alloc)) return CDF_INTERNAL_ERROR;
	       alloc.last = VXR.First[entryN] - 1;
	       sX (SOME_ALREADY_ALLOCATED, &pStatus);
	     }
	     if (!sX(InsertEntry(CDF,Var,
				 &alloc,vxrOffset,
				 entryN,&toEndCount),&pStatus)) return pStatus;
	     *count += (toEndCount - 1);
	     *toRec = alloc.last;
	     return pStatus;
	   default:
	     return CORRUPTED_V2_CDF;
	 }
       }
       else {
	 if (alloc.first <= VXR.Last[entryN]) {
	   if (!sX(ReadIrType(CDF->fp,
			      VXR.Offset[entryN],
			      &irType),&pStatus)) return pStatus;
	   switch (irType) {
	     case VXR_:
	       alloc.last = MINIMUM(alloc.last,VXR.Last[entryN]);
	       if (!sX(InsertRecords(CDF,Var,alloc,
				     VXR.Offset[entryN],
				     &belowCount,
				     toRec),&pStatus)) return pStatus;
	       while (belowCount > NUM_VXRx_ENTRIES - 1) {
		 if (!sX(CreateLevel(CDF,VXR.Offset[entryN],
				     belowCount,&VXR.Offset[entryN],
				     &belowCountNew),&pStatus)) return pStatus;
		 if (!sX(WriteVXR(CDF->fp,vxrOffset,
				  VXR_RECORD,&VXR,
				  VXR_NULL),&pStatus)) return pStatus;
		 belowCount = belowCountNew;
	       }
	       if (!sX(CountVXRs(CDF,VXR.VXRnext,
				 &toEndCount),&pStatus)) return pStatus;
	       *count += toEndCount;
	       return pStatus;
	     case VVR_:
	     case CVVR_:
	       if (EXISTSisBAD(&alloc)) return CDF_INTERNAL_ERROR;
	       sX (SOME_ALREADY_ALLOCATED, &pStatus);
	       *toRec = MINIMUM(VXR.Last[entryN],alloc.last);
	       if (!sX(CountVXRs(CDF,
				 VXR.VXRnext,
				 &toEndCount),&pStatus)) return pStatus;
	       *count += toEndCount;
	       return pStatus;
	     default:
	       return CORRUPTED_V2_CDF;
	   }
	 }
       }
    }
    /**************************************************************************
    * No more entries for this VXR.  If there are no more VXRs...
    **************************************************************************/
    if (VXR.VXRnext == ZERO_OFFSET) {
      int lastEntryN = entryN - 1;
      if (!sX(ReadIrType(CDF->fp,
			 VXR.Offset[lastEntryN],
			 &irType),&pStatus)) return pStatus;
      switch (irType) {
	case VXR_:
	  if (!sX(AppendRecords(CDF,Var,&alloc,
				VXR.Offset[lastEntryN],
				&belowCount),&pStatus)) return pStatus;
	  VXR.Last[lastEntryN] = alloc.last;
	  if (belowCount > NUM_VXRx_ENTRIES) {
	    if (!sX(ExtendLevel(CDF,VXR.Offset[lastEntryN],
				belowCount,&vxrOffsetNew,
				&lastNew,&countNew),&pStatus)) return pStatus;
	    VXR.Last[lastEntryN] = lastNew;
	    VXR.VXRnext = vxrOffsetNew;
	    *count += countNew;
	  }
	  if (!sX(WriteVXR(CDF->fp,vxrOffset,
			   VXR_RECORD,&VXR,
			   VXR_NULL),&pStatus)) return pStatus;
	  *toRec = alloc.last;
	  break;
	case VVR_:
	case CVVR_:
	  if (!sX(AppendEntry(CDF,Var,&alloc,
			      vxrOffset,&toEndCount),&pStatus)) return pStatus;
	  *count += (toEndCount - 1);
	  *toRec = alloc.last;
	  break;
	default:
	  return CORRUPTED_V2_CDF;
      }
      return pStatus;
    }
    /**************************************************************************
    * Go on to the next VXR.
    **************************************************************************/
    vxrOffset = VXR.VXRnext;
  }
  return pStatus;
}

/******************************************************************************
* AppendRecords.
******************************************************************************/

static CDFstatus AppendRecords (CDF, Var, alloc, vxrStart, count)
struct CDFstruct *CDF;
struct VarStruct *Var;
struct AllocStruct *alloc;
Int32 vxrStart;
int *count;
{
  CDFstatus pStatus = CDF_OK; struct VXRstruct VXR;
  Int32 vxrOffset = vxrStart, irType, vxrOffsetNew, lastNew;
  int lastEntryN, belowCount, toEndCount, countNew;
  /****************************************************************************
  * Locate last VXR on linked list.  We had better be appending at the last
  * VXR.
  ****************************************************************************/
  if (!sX(ReadVXR(CDF->fp,vxrOffset,
		  VXR_RECORD,&VXR,
		  VXR_NULL),&pStatus)) return pStatus;
  *count = 1;
  while (VXR.VXRnext != ZERO_OFFSET) {
    vxrOffset = VXR.VXRnext;
    if (!sX(ReadVXR(CDF->fp,vxrOffset,
		    VXR_RECORD,&VXR,
		    VXR_NULL),&pStatus)) return pStatus;
    (*count)++;
  }
  /****************************************************************************
  * If the last entry points to another VXR, recursively call this routine
  * with the offset of that VXR.  Otherwise, append the entry.
  ****************************************************************************/
  lastEntryN = (int) (VXR.NusedEntries - 1);
  if (!sX(ReadIrType(CDF->fp,
		     VXR.Offset[lastEntryN],
		     &irType),&pStatus)) return pStatus;
  switch (irType) {
    case VXR_:
      if (!sX(AppendRecords(CDF,Var,alloc,
			    VXR.Offset[lastEntryN],
			    &belowCount),&pStatus)) return pStatus;
      VXR.Last[lastEntryN] = alloc->last;
      if (belowCount > NUM_VXRx_ENTRIES) {
	if (!sX(ExtendLevel(CDF,VXR.Offset[lastEntryN],
			    belowCount,&vxrOffsetNew,
			    &lastNew,&countNew),&pStatus)) return pStatus;
	VXR.Last[lastEntryN] = lastNew;
	VXR.VXRnext = vxrOffsetNew;
	*count += countNew;
      }
      if (!sX(WriteVXR(CDF->fp,vxrOffset,
		       VXR_RECORD,&VXR,
		       VXR_NULL),&pStatus)) return pStatus;
      break;
    case VVR_:
    case CVVR_:
      if (!sX(AppendEntry(CDF,Var,alloc,
			  vxrOffset,&toEndCount),&pStatus)) return pStatus;
      *count += (toEndCount - 1);
      break;
    default:
      return CORRUPTED_V2_CDF;
  }
  return pStatus;
}

/******************************************************************************
* InsertEntry.
******************************************************************************/

static CDFstatus InsertEntry (CDF, Var, alloc, vxrOffset, atEntryN, count)
struct CDFstruct *CDF;
struct VarStruct *Var;
struct AllocStruct *alloc;
Int32 vxrOffset;
int atEntryN;
int *count;
{
  CDFstatus pStatus = CDF_OK;
  Int32 vxrOffsetNew, vrOffset, pushFirst, pushLast, pushOffset;
  struct VXRstruct VXR; Logical push;
  /****************************************************************************
  * Read the VXR, insert the entry while determining if an entry was pushed
  * out of the VXR, and rewrite the VXR.
  ****************************************************************************/
  if (!sX(ReadVXR(CDF->fp,vxrOffset,
		  VXR_RECORD,&VXR,
		  VXR_NULL),&pStatus)) return pStatus;
  if (!sX(AllocateVR(CDF,Var,alloc,&vrOffset),&pStatus)) return pStatus;
  InsertAtEntry (alloc->first, alloc->last, vrOffset, &VXR, atEntryN,
		 &push, &pushFirst, &pushLast, &pushOffset);
  if (!sX(WriteVXR(CDF->fp,vxrOffset,
		   VXR_RECORD,&VXR,
		   VXR_NULL),&pStatus)) return pStatus;
  *count = 1;
  /****************************************************************************
  * While an entry was pushed out...
  ****************************************************************************/
  while (push) {
    if (VXR.VXRnext != ZERO_OFFSET) {
      vxrOffset = VXR.VXRnext;
      if (!sX(ReadVXR(CDF->fp,vxrOffset,
		      VXR_RECORD,&VXR,
		      VXR_NULL),&pStatus)) return pStatus;
      InsertAtEntry (pushFirst, pushLast, pushOffset, &VXR, 0, &push,
		     &pushFirst, &pushLast, &pushOffset);
      if (!sX(WriteVXR(CDF->fp,vxrOffset,
		       VXR_RECORD,&VXR,
		       VXR_NULL),&pStatus)) return pStatus;
    }
    else {
      if (!sX(AllocateIR(CDF,((Int32)VXR_BASE_SIZE),
			 &vxrOffsetNew),&pStatus)) return pStatus;
      if (!sX(WriteVXR(CDF->fp,vxrOffset,
		       VXR_VXRNEXT,&vxrOffsetNew,
		       VXR_NULL),&pStatus)) return pStatus;
      InitNewVXR (&VXR, pushFirst, pushLast, pushOffset);
      if (!sX(WriteVXR(CDF->fp,vxrOffsetNew,
		       VXR_RECORD,&VXR,
		       VXR_NULL),&pStatus)) return pStatus;
      push = FALSE;
    }
    (*count)++;
  }
  return pStatus;
}

/******************************************************************************
* InsertAtEntry.
******************************************************************************/

static void InsertAtEntry (first, last, offset, VXR, atEntryN, push,
			   pushFirst, pushLast, pushOffset)
Int32 first;
Int32 last;
Int32 offset;
struct VXRstruct *VXR;
int atEntryN;
Logical *push;
Int32 *pushFirst;
Int32 *pushLast;
Int32 *pushOffset;
{
  int startEntryN, entryN;
  if (VXR->NusedEntries < VXR->Nentries) {
    *push = FALSE;
    startEntryN = (int) VXR->NusedEntries;
    VXR->NusedEntries++;
  }
  else {
    startEntryN = (int) (VXR->Nentries - 1);
    *pushFirst = VXR->First[startEntryN];
    *pushLast = VXR->Last[startEntryN];
    *pushOffset = VXR->Offset[startEntryN];
    *push = TRUE;
  }
  for (entryN = startEntryN; entryN > atEntryN; entryN--) {
     VXR->First[entryN] = VXR->First[entryN-1];
     VXR->Last[entryN] = VXR->Last[entryN-1];
     VXR->Offset[entryN] = VXR->Offset[entryN-1];
  }
  VXR->First[atEntryN] = first;
  VXR->Last[atEntryN] = last;
  VXR->Offset[atEntryN] = offset;
  return;
}

/******************************************************************************
* AppendEntry.
*   It is assumed that the offset is for a VXR at the end of a linked list.
******************************************************************************/

static CDFstatus AppendEntry (CDF, Var, alloc, vxrOffset, count)
struct CDFstruct *CDF;
struct VarStruct *Var;
struct AllocStruct *alloc;
Int32 vxrOffset;
int *count;
{
  CDFstatus pStatus = CDF_OK; Logical resized;
  struct VXRstruct VXR, newVXR; Int32 offset, vxrOffsetNew;
  /****************************************************************************
  * Read the VXR.
  ****************************************************************************/
  if (!sX(ReadVXR(CDF->fp,vxrOffset,
		  VXR_RECORD,&VXR,
		  VXR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * First check if a VVR_ can be extended.
  ****************************************************************************/
  if (alloc->type == VVR_) {
    if (!alloc->vvr.newX) {
      int lastEntryN = (int) (VXR.NusedEntries - 1);
      if (alloc->first == VXR.Last[lastEntryN] + 1) {
	Int32 newCount = alloc->last - VXR.First[lastEntryN] + 1;
	Int32 newSize = VVR_BASE_SIZE + (newCount * Var->NphyRecBytes);
	if (!sX(ResizeIR(CDF,VXR.Offset[lastEntryN],
			 newSize,NULL,
			 FALSE,&resized),&pStatus)) return pStatus;
	if (resized) {
	  VXR.Last[lastEntryN] = alloc->last;
	  if (!sX(WriteVXR(CDF->fp,vxrOffset,
			   VXR_RECORD,&VXR,
			   VXR_NULL),&pStatus)) return pStatus;
	  if (!sX(WriteVVR(CDF->fp,VXR.Offset[lastEntryN],
			   VVR_RECORDSIZE,&newSize,
			   VVR_NULL),&pStatus)) return pStatus;
	  switch (Var->vType) {
	    case SPARSE_RECORDS_:
	      if (alloc->last < Var->maxWritten) {
		Int32 nRecords = alloc->first - VXR.First[lastEntryN];
		offset = VXR.Offset[lastEntryN] +
			 VVR_BASE_SIZE +
			 (nRecords * Var->NphyRecBytes);
		if (!sX(PadSparseRecords(CDF,Var,
					 offset,
					 nRecords),&pStatus)) return pStatus;
	      }
	      break;
	  }
	  *count = 1;
	  return pStatus;
	}
      }
    }
  }
  /****************************************************************************
  * If there is an unused entry...
  ****************************************************************************/
  if (VXR.NusedEntries < VXR.Nentries) {
    int entryN = (int) VXR.NusedEntries;
    VXR.First[entryN] = alloc->first;
    VXR.Last[entryN] = alloc->last;
    if (!sX(AllocateVR(CDF,Var,alloc,
		       &VXR.Offset[entryN]),&pStatus)) return pStatus;
    VXR.NusedEntries++;
    if (!sX(WriteVXR(CDF->fp,vxrOffset,
		     VXR_RECORD,&VXR,
		     VXR_NULL),&pStatus)) return pStatus;
    *count = 1;
    return pStatus;
  }
  /****************************************************************************
  * No unused entries - create a new VXR.
  ****************************************************************************/
  if (!sX(AllocateIR(CDF,((Int32)VXR_BASE_SIZE),
		     &vxrOffsetNew),&pStatus)) return pStatus;
  if (!sX(AllocateVR(CDF,Var,alloc,&offset),&pStatus)) return pStatus;
  if (!sX(WriteVXR(CDF->fp,vxrOffset,
		   VXR_VXRNEXT,&vxrOffsetNew,
		   VXR_NULL),&pStatus)) return pStatus;
  InitNewVXR (&newVXR, alloc->first, alloc->last, offset);
  if (!sX(WriteVXR(CDF->fp,vxrOffsetNew,
		   VXR_RECORD,&newVXR,
		   VXR_NULL),&pStatus)) return pStatus;
  *count = 2;
  return pStatus;
}

/******************************************************************************
* PadSparseRecords.
******************************************************************************/

static CDFstatus PadSparseRecords (CDF, Var, offset, nRecords)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 offset;
Int32 nRecords;
{
  CDFstatus pStatus = CDF_OK; int how; void *buffer;
  if (!sX(BuildPadBuffer(CDF,Var,nRecords,
			 &how,&buffer,TRUE),&pStatus)) return pStatus;
  if (!sX(WritePadValues(Var,CDF->fp,offset,
			 nRecords,how,buffer),&pStatus)) {
    cdf_FreeMemory (buffer, NULL);
    return pStatus;
  }
  cdf_FreeMemory (buffer, NULL);
  return pStatus;
}

/******************************************************************************
* CountVXRs.
******************************************************************************/

static CDFstatus CountVXRs (CDF, vxrStart, count)
struct CDFstruct *CDF;
Int32 vxrStart;
int *count;
{
  CDFstatus pStatus = CDF_OK; Int32 vxrOffset = vxrStart;
  *count = 0;
  while (vxrOffset != ZERO_OFFSET) {
    if (!sX(ReadVXR(CDF->fp,vxrOffset,
		    VXR_VXRNEXT,&vxrOffset,
		    VXR_NULL),&pStatus)) return pStatus;
    (*count)++;
  }
  return pStatus;
}

/******************************************************************************
* InitNewVXR.
******************************************************************************/

STATICforIDL void InitNewVXR (VXR, firstRec, lastRec, offset)
struct VXRstruct *VXR;
Int32 firstRec;
Int32 lastRec;
Int32 offset;
{
  int entryN;
  VXR->RecordSize = VXR_BASE_SIZE;
  VXR->RecordType = VXR_;
  VXR->VXRnext = ZERO_OFFSET;
  VXR->Nentries = NUM_VXR_ENTRIES;
  VXR->NusedEntries = 1;
  VXR->First[0] = firstRec;
  VXR->Last[0] = lastRec;
  VXR->Offset[0] = offset;
  for (entryN = 1; entryN < NUM_VXR_ENTRIES; entryN++) {
     VXR->First[entryN] = NO_RECORD;
     VXR->Last[entryN] = NO_RECORD;
     VXR->Offset[entryN] = NO_OFFSET;
  }
  return;
}

/******************************************************************************
* InitNewVXRx.
******************************************************************************/

static void InitNewVXRx (VXRx)
struct VXRstruct *VXRx;
{
  int entryN;
  VXRx->RecordSize = VXRx_BASE_SIZE;
  VXRx->RecordType = VXR_;
  VXRx->VXRnext = 0;
  VXRx->Nentries = NUM_VXRx_ENTRIES;
  VXRx->NusedEntries = 0;
  for (entryN = 0; entryN < NUM_VXRx_ENTRIES; entryN++) {
     VXRx->First[entryN] = NO_RECORD;
     VXRx->Last[entryN] = NO_RECORD;
     VXRx->Offset[entryN] = NO_OFFSET;
  }
  return;
}

/******************************************************************************
* FirstRecords.
******************************************************************************/

static CDFstatus FirstRecords (CDF, Var, alloc)
struct CDFstruct *CDF;
struct VarStruct *Var;
struct AllocStruct *alloc;
{
  CDFstatus pStatus = CDF_OK;
  struct VXRstruct VXR; Int32 offset, vxrOffset;
  if (!sX(AllocateIR(CDF,VXR_BASE_SIZE,&vxrOffset),&pStatus)) return pStatus;
  if (!sX(AllocateVR(CDF,Var,alloc,&offset),&pStatus)) return pStatus;
  InitNewVXR (&VXR, alloc->first, alloc->last, offset);
  if (!sX(WriteVXR(CDF->fp,vxrOffset,
		   VXR_RECORD,&VXR,
		   VXR_NULL),&pStatus)) return pStatus;
  if (!sX(WriteVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
		   VDR_VXRHEAD,&vxrOffset,
		   VDR_VXRTAIL,&vxrOffset,
		   VDR_NULL),&pStatus)) return pStatus;
  return pStatus;
}

/******************************************************************************
* ExtendLevel.
******************************************************************************/

static CDFstatus ExtendLevel (CDF, vxrOffset, count, vxrOffsetHead, lastNew,
			      countNew)
struct CDFstruct *CDF;
Int32 vxrOffset;
int count;
Int32 *vxrOffsetHead;
Int32 *lastNew;
int *countNew;
{
  CDFstatus pStatus = CDF_OK; struct VXRstruct VXR;
  if (!sX(ReadVXR(CDF->fp,vxrOffset,
		  VXR_RECORD,&VXR,
		  VXR_NULL),&pStatus)) return pStatus;
  *lastNew = VXR.Last[(int)(VXR.NusedEntries-1)];
  if (!sX(CreateLevel(CDF,VXR.VXRnext,count-1,
		      vxrOffsetHead,countNew),&pStatus)) return pStatus;
  VXR.VXRnext = ZERO_OFFSET;
  if (!sX(WriteVXR(CDF->fp,vxrOffset,
		   VXR_RECORD,&VXR,
		   VXR_NULL),&pStatus)) return pStatus;
  return pStatus;
}

/******************************************************************************
* CreateLevel.
******************************************************************************/

static CDFstatus CreateLevel (CDF, vxrOffset, count, vxrOffsetHead, newCount)
struct CDFstruct *CDF;
Int32 vxrOffset;
int count;
Int32 *vxrOffsetHead;
int *newCount;
{
  CDFstatus pStatus = CDF_OK;
  Int32 sizeOfVXRx = VXRx_BASE_SIZE;
  Int32 vxrOffsetNext, vxrOffsetNew;
  int remaining = count, entryN;
  struct VXRstruct VXR, newVXR;
  /****************************************************************************
  * Create/initialize first VXRx_.
  ****************************************************************************/
  if (!sX(AllocateIR(CDF,sizeOfVXRx,vxrOffsetHead),&pStatus)) return pStatus;
  InitNewVXRx (&newVXR);
  *newCount = 1;
  vxrOffsetNew = *vxrOffsetHead;
  /****************************************************************************
  * Until no more VXRs...
  ****************************************************************************/
  while (vxrOffset != ZERO_OFFSET) {
    /**************************************************************************
    * Read VXR.
    **************************************************************************/
    if (!sX(ReadVXR(CDF->fp,vxrOffset,
		    VXR_RECORD,&VXR,
		    VXR_NULL),&pStatus)) return pStatus;
    /**************************************************************************
    * If the current VXRx_ is full...
    **************************************************************************/
    if (newVXR.NusedEntries == newVXR.Nentries) {
      if (!sX(AllocateIR(CDF,sizeOfVXRx,
			 &(newVXR.VXRnext)),&pStatus)) return pStatus;
      if (!sX(WriteVXR(CDF->fp,vxrOffsetNew,
		       VXR_RECORD,&newVXR,
		       VXR_NULL),&pStatus)) return pStatus;
      vxrOffsetNew = newVXR.VXRnext;
      InitNewVXRx (&newVXR);
      (*newCount)++;
    }
    /**************************************************************************
    * Determine entry to be used and increment number of used entries.
    **************************************************************************/
    entryN = (int) newVXR.NusedEntries;
    newVXR.NusedEntries++;
    /**************************************************************************
    * If this is the last entry in the VXRx_ and there aren't enough VXRs for
    * another full VXRx_...
    **************************************************************************/
    if (entryN == newVXR.Nentries - 1) {
      if (remaining < NUM_VXRx_ENTRIES + 1) {
	newVXR.First[entryN] = VXR.First[0];
	while (VXR.VXRnext != ZERO_OFFSET) {
	  if (!sX(ReadVXR(CDF->fp,VXR.VXRnext,
			  VXR_RECORD,&VXR,
			  VXR_NULL),&pStatus)) return pStatus;
	}
	newVXR.Last[entryN] = VXR.Last[(int)(VXR.NusedEntries-1)];
	newVXR.Offset[entryN] = vxrOffset;
	break;
      }
    }
    /**************************************************************************
    * Add this VXR's indexing to the current VXRx_.
    **************************************************************************/
    newVXR.First[entryN] = VXR.First[0];
    newVXR.Last[entryN] = VXR.Last[(int)(VXR.NusedEntries-1)];
    newVXR.Offset[entryN] = vxrOffset;
    /**************************************************************************
    * Break the VXR's link to the next VXR.
    **************************************************************************/
    vxrOffsetNext = VXR.VXRnext;
    VXR.VXRnext = ZERO_OFFSET;
    if (!sX(WriteVXR(CDF->fp,vxrOffset,
		     VXR_RECORD,&VXR,
		     VXR_NULL),&pStatus)) return pStatus;
    vxrOffset = vxrOffsetNext;
    /**************************************************************************
    * Decrement the number of remaining VXRs.
    **************************************************************************/
    remaining--;
  }
  /****************************************************************************
  * Write the VXRx_ before returning.
  ****************************************************************************/
  if (!sX(WriteVXR(CDF->fp,vxrOffsetNew,
		   VXR_RECORD,&newVXR,
		   VXR_NULL),&pStatus)) return pStatus;
  return pStatus;
}

/******************************************************************************
* AllocateVR.
******************************************************************************/

static CDFstatus AllocateVR (CDF, Var, alloc, offset)
struct CDFstruct *CDF;
struct VarStruct *Var;
struct AllocStruct *alloc;
Int32 *offset;
{
  CDFstatus pStatus = CDF_OK;
  switch (alloc->type) {
    case VVR_: {
      Int32 nRecords = alloc->last - alloc->first + 1;
      if (!sX(AllocateVVR(CDF,Var,
			  nRecords,
			  offset),&pStatus)) return pStatus;
      switch (Var->vType) {
	case SPARSE_RECORDS_:
	  if (alloc->last < Var->maxWritten) {
	    Int32 tOffset = *offset + VVR_BASE_SIZE;
	    if (!sX(PadSparseRecords(CDF,Var,
				     tOffset,
				     nRecords),&pStatus)) return pStatus;
	  }
	  break;
      }
      break;
    }
    case CVVR_:
      if (!sX(AllocateCVVR(CDF,alloc->cvvr.cSize,
			   alloc->cvvr.xSize,
			   offset),&pStatus)) return pStatus;
      break;
    default:
      return CDF_INTERNAL_ERROR;
  }
  return pStatus;
}

/******************************************************************************
* AllocateVVR.
******************************************************************************/

static CDFstatus AllocateVVR (CDF, Var, nRecords, vvrOffset)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 nRecords;
Int32 *vvrOffset;
{
  CDFstatus pStatus = CDF_OK; struct VVRstruct VVR;
  VVR.RecordSize = (Int32) (VVR_BASE_SIZE + (nRecords * Var->NphyRecBytes));
  VVR.RecordType = VVR_;
  if (!sX(AllocateIR(CDF,VVR.RecordSize,vvrOffset),&pStatus)) return pStatus;
  if (!sX(WriteVVR(CDF->fp,*vvrOffset,
		   VVR_RECORDx,&VVR,
		   VVR_NULL),&pStatus)) return pStatus;
  CDF->fp->length = MaxLong (CDF->fp->length, (long)
                                                *vvrOffset + VVR.RecordSize);
  return pStatus;
}

/******************************************************************************
* AllocateCVVR.
******************************************************************************/

static CDFstatus AllocateCVVR (CDF, cSize, xSize, cvvrOffset)
struct CDFstruct *CDF;
Int32 cSize;
Int32 xSize;
Int32 *cvvrOffset;
{
  CDFstatus pStatus = CDF_OK; struct CVVRstruct CVVR;
  CVVR.RecordSize = (Int32) (CVVR_BASE_SIZE + cSize + xSize);
  CVVR.RecordType = CVVR_;
  CVVR.rfuA = (Int32) 0;
  CVVR.cSize = cSize;
  if (!sX(AllocateIR(CDF,CVVR.RecordSize,cvvrOffset),&pStatus)) return pStatus;
  if (!sX(WriteCVVR(CDF->fp,*cvvrOffset,
		    CVVR_RECORDx,&CVVR,
		    CVVR_NULL),&pStatus)) return pStatus;
  CDF->fp->length = MaxLong (CDF->fp->length, (long)
                                               *cvvrOffset + CVVR.RecordSize);
  return pStatus;
}

/******************************************************************************
* UpdateVXRtailInVDR.
******************************************************************************/

STATICforIDL CDFstatus UpdateVXRtailInVDR (CDF, Var)
struct CDFstruct *CDF;
struct VarStruct *Var;
{
  CDFstatus pStatus = CDF_OK; struct VXRstruct VXR;
  Int32 vxrOffset, irType; int lastEntryN;
  /****************************************************************************
  * Read the VXR head.  If zero, set the VXR tail to zero.
  ****************************************************************************/
  if (!sX(ReadVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
		  VDR_VXRHEAD,&vxrOffset,
		  VDR_NULL),&pStatus)) return pStatus;
  if (vxrOffset == ZERO_OFFSET) {
    if (!sX(WriteVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
		     VDR_VXRTAIL,&vxrOffset,
		     VDR_NULL),&pStatus)) return pStatus;
    return pStatus;
  }
  /****************************************************************************
  * Otherwise, read to the VXR tail...
  ****************************************************************************/
  for (;;) {
     if (!sX(ReadVXR(CDF->fp,vxrOffset,
		     VXR_RECORD,&VXR,
		     VXR_NULL),&pStatus)) return pStatus;
     while (VXR.VXRnext != ZERO_OFFSET) {
       vxrOffset = VXR.VXRnext;
       if (!sX(ReadVXR(CDF->fp,vxrOffset,
		       VXR_RECORD,&VXR,
		       VXR_NULL),&pStatus)) return pStatus;
     }
     lastEntryN = (int) (VXR.NusedEntries - 1);
     if (!sX(ReadIrType(CDF->fp,
			VXR.Offset[lastEntryN],
			&irType),&pStatus)) return pStatus;
     switch (irType) {
       case VXR_:
	 vxrOffset = VXR.Offset[lastEntryN];
	 break;
       case VVR_:
       case CVVR_:
	 if (!sX(WriteVDR(CDF,CDF->fp,Var->VDRoffset,Var->zVar,
			  VDR_VXRTAIL,&vxrOffset,
			  VDR_NULL),&pStatus)) return pStatus;
	 return pStatus;
       default:
	 return CORRUPTED_V2_CDF;
     }
  }
}

