/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                     CDF library miscellaneous functions, part 2.
*
*  Version 1.3b, 4-Mar-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  15-Dec-94, J Love     Original version.
*   V1.1   6-Jan-95, J Love	Encode/decode changes.  More cache-residency.
*   V1.1a 20-Jan-95, J Love	IRIX 6.0 (64-bit).
*   V1.1b 15-Mar-95, J Love	Solaris 2.3 IDL i/f.  Fixed `recNum' parameter
*				of `LastAllocatedRecord'.  Gnu C on OSF/1.
*   V1.2  21-Mar-95, J Love	POSIX.
*   V1.2a 18-Apr-95, J Love	More POSIX.
*   V1.2b 13-Jun-95, J Love	Linux.
*   V1.2c  7-Sep-95, J Love	CDFexport-related changes.  Fixed possible
*				memory leak.
*   V1.3   5-Sep-96, J Love	CDF V2.6.
*   V1.3a 21-Feb-97, J Love	Removed RICE.
*   V1.3b  4-Mar-97, J Love	Windows NT for MS Visual C/C++ on an IBM PC.
*   V2.0  08-Apr-04, M Liu      Modified LocateCurrentVar function to save
*                               the current selected variable's offset.
*                               Modified FindVarByNumber and FindVarByName
*                               functions to start searching for the variable
*                               from the current selected variable, if it's
*                               cwselected, instead of always from the 
*                               beginning. Remove the calls to FindVarByNumber 
*                               if a variable is already selected.
*   V2.1  04-May-04, M Liu      Corrected Int32ToCDFid and CDFidToInt32 to 
*                               handle 64-bit on Solaris/sparc.
*   V2.2  29-Jun-04, M Liu      Added LFS (Large File Support > 2G).
*   V3.2  20-Jun-07, D Berger   Modified the "FindAttr...", "FindEntry..."
*                               routines, and the "SetCUR..." routines and
*                               added ResetReadOnlyState to handle READONLYon
*                               mode.
*   V3.3  10-Jan-09, M Liu      Modified to check used entries in LastRecord64.
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"

/******************************************************************************
* LocateCurrentVar64.
******************************************************************************/

STATICforIDL CDFstatus LocateCurrentVar64 (CDF, zOp, offset, zVar, Var)
struct CDFstruct *CDF;          /* In: Pointer to CDF. */
Logical zOp;                    /* In: TRUE if current zVariable is to be
				   accessed; FALSE if current rVariable.  N/A
				   if zMode is on (since the current zVariable
				   number will always be used). */
OFF_T *offset;                  /* Out: Offset of the current variable's VDR.
				   This may be a NULL pointer. */
Logical *zVar;                  /* Out: TRUE if a true zVariable; FALSE if
				   a true rVariable.  This may be a NULL
				   pointer. */
struct VarStruct **Var;         /* Out: Pointer to variable.  This will be NULL
				   if the variable has yet to be initialized.
				   This may be a NULL pointer. */
{
  CDFstatus tStatus;
  OFF_T tOffset;
  /****************************************************************************
  * Pass back the offset of the VDR.
  ****************************************************************************/
  if (zModeON(CDF)) {
    if (CDF->CURzVarNum < CDF->NrVars) {
      ASSIGNnotNULL(zVar, FALSE)
      tStatus = FindVarByNumber64 (CDF, CDF->CURzVarNum, FALSE, &tOffset);
      if (StatusOK(tStatus)) {
	ASSIGNnotNULL (Var, CDF->rVars[(int)CDF->CURzVarNum])
	ASSIGNnotNULL (offset, tOffset)
	CDF->CURzVarOffset64 = tOffset;
      }
    }
    else {
      ASSIGNnotNULL(zVar, TRUE)
      tStatus = FindVarByNumber64 (CDF, CDF->CURzVarNum - CDF->NrVars, TRUE,
				   &tOffset);
      if (StatusOK(tStatus)) {
	ASSIGNnotNULL (Var, CDF->zVars[(int)(CDF->CURzVarNum - CDF->NrVars)])
	ASSIGNnotNULL (offset, tOffset)
	CDF->CURzVarOffset64 = tOffset;
      }
    }
  }
  else {
    ASSIGNnotNULL (zVar, zOp)
    tStatus = FindVarByNumber64 (CDF,BOO(zOp,CDF->CURzVarNum,
					 CDF->CURrVarNum),zOp,&tOffset);
    if (StatusOK(tStatus)) {
      ASSIGNnotNULL (Var, BOO(zOp,CDF->zVars[(int)CDF->CURzVarNum],
				  CDF->rVars[(int)CDF->CURrVarNum]))
      ASSIGNnotNULL (offset, tOffset)
      if (zOp) 
	CDF->CURzVarOffset64 = tOffset;
      else 
	CDF->CURrVarOffset64 = tOffset;
    }
  }
  return tStatus;
}

/******************************************************************************
* InitCurrentVar64.
******************************************************************************/

STATICforIDL CDFstatus InitCurrentVar64 (CDF, zOp, Var)
struct CDFstruct *CDF;          /* In: Pointer to CDF. */
Logical zOp;                    /* In: TRUE if current zVariable is to be
                                   accessed; FALSE if current rVariable.  N/A
                                   if zMode is on (since the current zVariable
                                   number will always be used [even when a real
                                   rVariable is being accessed]). */
struct VarStruct **Var;         /* Out: Pointer to variable. */
{
  CDFstatus tStatus;
  /****************************************************************************
  * Pass back the pointer to its variable structure.  The variable will
  * be initialized for access if necessary.
  ****************************************************************************/
  if (zModeON(CDF))
    if (CDF->CURzVarNum < CDF->NrVars) {
      tStatus = InitVar64 (CDF, CDF->CURzVarNum, FALSE, Var);
    }
    else {
      tStatus = InitVar64 (CDF, CDF->CURzVarNum - CDF->NrVars, TRUE, Var);
  }
  else {
    tStatus = InitVar64 (CDF,BOO(zOp,CDF->CURzVarNum,CDF->CURrVarNum),zOp,Var);
  };
  return tStatus;
}

/******************************************************************************
* InitVar64.
******************************************************************************/

STATICforIDL CDFstatus InitVar64 (CDF, varN, zVar, VarP)
struct CDFstruct *CDF;          /* In: Pointer to CDF. */
Int32 varN;                     /* In: Real variable number (ignoring the
				   zMode). */
Logical zVar;                   /* In: TRUE if a real zVariable (ignoring
				   the zMode). */
struct VarStruct **VarP;        /* Out: Pointer to variable. */
{
  CDFstatus pStatus = CDF_OK;
  struct VarStruct *Var = BOO(zVar,CDF->zVars[(int)varN],
				   CDF->rVars[(int)varN]);
  /****************************************************************************
  * Check if the variable has already been initialized.  If not, allocate and
  * initialize its variable structure.
  ****************************************************************************/
  if (Var == NULL) {
    /**************************************************************************
    * Allocate a variable structure.
    **************************************************************************/
    Var = (struct VarStruct *) cdf_AllocateMemory ((size_t)sizeof(struct VarStruct), NULL);
    if (Var == NULL) return BAD_MALLOC;
    /**************************************************************************
    * Determine offset of the VDR in the dotCDF file.
    **************************************************************************/
    if (!sX(FindVarByNumber64(CDF,varN,zVar,&(Var->VDRoffset64)),&pStatus)) {
      cdf_FreeMemory (Var, NULL);
      return pStatus;
    }

    /**************************************************************************
    * Initialize miscellaneous fields of the variable structure.
    **************************************************************************/
    Var->zVar = zVar; 
    Var->varN = varN;
    Var->fp = NULL;
    Var->varCacheSize = NUMcacheVAR;		/* N/A if single-file CDF. */
    Var->accessed_at = CDF->pseudo_clock++;
    Var->firstRecInVVR = NO_RECORD;
    Var->lastRecInVVR = NO_RECORD;
    Var->offsetOfVVR64 = (OFF_T) NO_OFFSET64;
    /**************************************************************************
    * Read fields to be held in memory for efficiency.
    **************************************************************************/
    if (!sX(ReadVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
		      VDR_DATATYPE,&(Var->dataType),
		      VDR_MAXREC,&(Var->maxRec),
		      VDR_NULL),&pStatus)) {
      cdf_FreeMemory (Var, NULL);
      return pStatus;
    }
    /**************************************************************************
    * More initialization.
    **************************************************************************/
    if (!sX(InitVar2_64(CDF,Var),&pStatus)) {
      cdf_FreeMemory (Var, NULL);
      return pStatus;
    }
    /**************************************************************************
    * Store pointer to variable structure.
    **************************************************************************/
    if (zVar)
      CDF->zVars[(int)varN] = Var;
    else
      CDF->rVars[(int)varN] = Var;
  }
  /****************************************************************************
  * Pass back pointer to variable.
  ****************************************************************************/
  ASSIGNnotNULL (VarP, Var)
  return pStatus;
}

/******************************************************************************
* InitVar2_64.
******************************************************************************/

STATICforIDL CDFstatus InitVar2_64 (CDF, Var)
struct CDFstruct *CDF;
struct VarStruct *Var;
{
  int dimN; CDFstatus pStatus = CDF_OK; struct CPRstruct64 CPR; int i;
  Int32 flags, dataType, numElems, sRecords;
  OFF_T CPRoffset;
  /****************************************************************************
  * Read necessary fields from the VDR.
  ****************************************************************************/
  if (!sX(ReadVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
		    VDR_FLAGS,&flags,
		    VDR_DATATYPE,&dataType,
		    VDR_NUMELEMS,&numElems,
		    VDR_sRECORDS,&sRecords,
		    VDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * Determine if the variable has a known data type.
  ****************************************************************************/
  if (!ValidDataType(dataType)) return BAD_DATA_TYPE;
  /****************************************************************************
  * Calculate the dimensionality and variances of the variable based on the
  * current zMode.
  ****************************************************************************/
  if (!sX(CalcDimParms64(CDF,Var->VDRoffset64,
		         Var->zVar,&(Var->numDims),
		         Var->dimSizes,Var->dimVarys),&pStatus)) return pStatus;
  Var->recVary = BOO(RECvaryBITset(flags),VARY,NOVARY);
  /****************************************************************************
  * Calculate the number of values for each dimension.
  ****************************************************************************/
  CalcNumDimValues (CDF, Var);
  /****************************************************************************
  * Calculate the element and value sizes.
  ****************************************************************************/
  Var->NvalueElems = numElems;
  Var->NelemBytes = (Int32) CDFelemSize (dataType);
  Var->NvalueBytes = Var->NvalueElems * Var->NelemBytes;
  /****************************************************************************
  * Calculate the number of physical and virtual values per record.
  ****************************************************************************/
  CalcRecValues (Var);
  /****************************************************************************
  * Calculate the number of elements and the size (in bytes) of physical and
  * conceptual records.
  ****************************************************************************/
  Var->NphyRecElems = Var->NphyRecValues * Var->NvalueElems;
  Var->NvirtRecElems = Var->NvirtRecValues * Var->NvalueElems;
  Var->NphyRecBytes = Var->NphyRecValues * Var->NvalueBytes;
  Var->NvirtRecBytes = Var->NvirtRecValues * Var->NvalueBytes;
  /**************************************************************************
  * Initialize current positioning.
  **************************************************************************/
  Var->seqValueOffset64 = (OFF_T) 0;
  Var->zRD.recNumber = 0;
  Var->zRD.recCount = 1;
  Var->zRD.recInterval = 1;
  for (dimN = 0; dimN < Var->numDims; dimN++) {
     Var->zRD.dimIndices[dimN] = 0;
     Var->zRD.dimCounts[dimN] = Var->dimSizes[dimN];
     Var->zRD.dimIntervals[dimN] = 1;
  }
  /****************************************************************************
  * Determine variable type.
  ****************************************************************************/
  if (!sX(VariableType64(CDF,Var->VDRoffset64,
		         Var->zVar,&(Var->vType)),&pStatus)) return pStatus;
  /**************************************************************************
  * What to do if a record is missing.
  **************************************************************************/
  Var->prevIfMissing = (sRecords == PREV_SPARSERECORDS);
  /**************************************************************************
  * Based on the variable type...
  **************************************************************************/
  switch (Var->vType) {
    case STANDARD_:
    case SPARSE_RECORDS_:
      if (!sX(LastRecord64(CDF,Var->VDRoffset64,Var->zVar,
			   &(Var->maxAllocated)),&pStatus)) return pStatus;
      Var->maxWritten = Var->maxAllocated;
      break;
    case COMPRESSED_:
    case SPARSE_COMPRESSED_RECORDS_:
      if (!sX(ReadVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
		        VDR_CPRorSPR,&CPRoffset,
		        VDR_NULL),&pStatus)) return pStatus;
      if (!sX(ReadCPR64(CDF->fp,CPRoffset,
		        CPR_RECORD,&CPR,
		        CPR_NULL),&pStatus)) return pStatus;
      Var->cType = CPR.cType;
      for (i = 0; i < CPR.pCount; i++) Var->cParms[i] = CPR.cParms[i];
      Var->reservePct = 0;
      break;
    case SPARSE_ARRAYS_:
    case SPARSE_RECORDS_AND_ARRAYS_:
      return UNKNOWN_SPARSENESS;
    case IN_MULTI_:
      break;
    default:
      return CDF_INTERNAL_ERROR;
  }
  /**************************************************************************
  * Initialize staging area (although n/a for most variable types).
  **************************************************************************/
  Var->stage.areaOffset64 = (OFF_T) NO_OFFSET64;
  Var->stage.firstRec = NO_RECORD;
  Var->stage.lastRec = NO_RECORD;
  Var->stage.dotOffset64 = (OFF_T) NO_OFFSET64;
  Var->stage.modified = FALSE;
  /****************************************************************************
  * Calculate the blocking factor based on variable type...
  ****************************************************************************/
  if (!sX(CalcBF64(CDF,Var),&pStatus)) return pStatus;
  /****************************************************************************
  * Determine the encoding and decoding functions to be used.
  ****************************************************************************/
  if (!sX(ConversionFunction(dataType,HostEncoding(),
			     CDF->encoding,CDF->negToPosFp0,
			     &(Var->EncodeFunction)),&pStatus)) return pStatus;
  if (!sX(ConversionFunction(dataType,CDF->encoding,
			     CDF->decoding,CDF->negToPosFp0,
			     &(Var->DecodeFunction)),&pStatus)) return pStatus;
  return pStatus;
}

/******************************************************************************
* CalcBF64.
******************************************************************************/

STATICforIDL CDFstatus CalcBF64 (CDF, Var)
struct CDFstruct *CDF;
struct VarStruct *Var;
{
  CDFstatus pStatus = CDF_OK; Int32 bF;
  if (!sX(ReadVDR64(CDF,CDF->fp,Var->VDRoffset64,
		    Var->zVar, VDR_BLOCKING,&bF,
		    VDR_NULL),&pStatus)) return pStatus;
  switch (Var->vType) {
    case STANDARD_:
      if (Var->recVary)
	if (bF == 0) {
	  Int32 min = ((MIN_BLOCKING_BYTES_standard-1)/Var->NphyRecBytes)+1;
	  Var->blockingFactor = MAXIMUM(min,MIN_BLOCKING_RECS_standard);
	}
	else
	  Var->blockingFactor = bF;
      else
	Var->blockingFactor = 1;
      break;
    case SPARSE_RECORDS_:
      if (Var->recVary)
	if (bF == 0) {
	  Int32 min = ((MIN_BLOCKING_BYTES_sparse-1)/Var->NphyRecBytes)+1;
	  Var->blockingFactor = MAXIMUM(min,MIN_BLOCKING_RECS_sparse);
	}
	else
	  Var->blockingFactor = bF;
      else
	Var->blockingFactor = 1;
      break;
    case COMPRESSED_:
    case SPARSE_COMPRESSED_RECORDS_:
      Var->blockingFactor = bF;
      break;
    case SPARSE_ARRAYS_:
    case SPARSE_RECORDS_AND_ARRAYS_:
      return UNKNOWN_SPARSENESS;
    case IN_MULTI_:
      Var->blockingFactor = 1;
      break;
    default:
      return CDF_INTERNAL_ERROR;
  }
  return pStatus;
}

/******************************************************************************
* UpdateConversions64.
******************************************************************************/

STATICforIDL CDFstatus UpdateConversions64 (CDF)
struct CDFstruct *CDF;
{
  CDFstatus pStatus = CDF_OK; Logical zVar;
  for (zVar = 0; zVar <= 1; zVar++) {
     int varN; Int32 nVars = BOO(zVar,CDF->NzVars,CDF->NrVars);
     for (varN = 0; varN < nVars; varN++) {
	struct VarStruct *Var = BOO(zVar,CDF->zVars[varN],CDF->rVars[varN]);
        if (Var != NULL) {
	  Int32 dataType;
	  if (!sX(ReadVDR64(CDF,CDF->fp,Var->VDRoffset64,
			    Var->zVar,
			    VDR_DATATYPE,&dataType,
			    VDR_NULL),&pStatus)) return pStatus;
	  if (!sX(ConversionFunction(dataType,HostEncoding(),
				     CDF->encoding,CDF->negToPosFp0,
				     &(Var->EncodeFunction)),&pStatus)) return
								       pStatus;
	  if (!sX(ConversionFunction(dataType,CDF->encoding,
				     CDF->decoding,CDF->negToPosFp0,
				     &(Var->DecodeFunction)),&pStatus)) return
								       pStatus;
        }
     }
  }
  return pStatus;
}

/******************************************************************************
* UpdateNEWzMode64.
******************************************************************************/

STATICforIDL CDFstatus UpdateNEWzMode64 (CDF)
struct CDFstruct *CDF;
{
  CDFstatus pStatus = CDF_OK; Logical zVar;
  for (zVar = 0; zVar <= 1; zVar++) {
     int varN; Int32 nVars = BOO(zVar,CDF->NzVars,CDF->NrVars);
     for (varN = 0; varN < nVars; varN++) {
	struct VarStruct *Var = BOO(zVar,CDF->zVars[varN],CDF->rVars[varN]);
        if (Var != NULL) {
	  if (!sX(CalcDimParms64(CDF,Var->VDRoffset64,
			         Var->zVar,&(Var->numDims),
			         Var->dimSizes,
			         Var->dimVarys),&pStatus)) return pStatus;
	  CalcNumDimValues (CDF, Var);
	  CalcRecValues (Var);
	  Var->NvirtRecElems = Var->NvirtRecValues * Var->NvalueElems;
	  Var->NvirtRecBytes = Var->NvirtRecValues * Var->NvalueBytes;
        }
     }
  }
  return pStatus;
}

/******************************************************************************
* OpenVar64.
* Open a variable for read and/or write access (if this is a multi-file CDF
* and the variable file is closed).  It is assumed that the variable has been
* initialized for access and that the variable is closed (and in a multi-file
* CDF).
******************************************************************************/

STATICforIDL CDFstatus OpenVar64 (CDF, Var)
struct CDFstruct *CDF;
struct VarStruct *Var;
{
  CDFstatus pStatus = CDF_OK;
  char a_mode[MAX_aMODE_LEN+1], pathname[DU_MAX_PATH_LEN+1];
  /****************************************************************************
  * Try to open the variable file.
  ****************************************************************************/
  BuildFilePath (BOO(Var->zVar,Zt,Vt), CDF->CDFname, CDF->no_append,
                 CDF->upper_case_ext, CDF->version_numbers, Var->varN,
                 pathname);
  if (CDF->status == READ_WRITE)
    strcpyX (a_mode, READ_PLUS_a_mode, MAX_aMODE_LEN);
  else
    strcpyX (a_mode, READ_ONLY_a_mode, MAX_aMODE_LEN);
  Var->fp = V_open64 (pathname, a_mode);
  /****************************************************************************
  * If the open failed, close a variable file and try.  If that attempt fails
  * return an error.
  ****************************************************************************/
  if (Var->fp == NULL) {
    if (!sX(CloseLRUvar(CDF),&pStatus)) return pStatus;
    Var->fp = V_open64 (pathname, a_mode);
    if (Var->fp == NULL) return VAR_OPEN_ERROR;
  }
  /****************************************************************************
  * The open was successful - try to set the proper cache size.
  ****************************************************************************/
  if (!CACHEv64(Var->fp,Var->varCacheSize)) {
    V_close64 (Var->fp, NULL, NULL);
    Var->fp = NULL;
    return BAD_CACHE_SIZE;
  }
  return pStatus;
}

/******************************************************************************
* LastRecord64.
******************************************************************************/

STATICforIDL CDFstatus LastRecord64 (CDF, VDRoffset, zVar, recNum)
struct CDFstruct *CDF;  /* In: Pointer to CDF. */
OFF_T VDRoffset;        /* In: Offset of VDR. */
Logical zVar;           /* In: TRUE if a real zVariable; FALSE if rVariable. */
Int32 *recNum;          /* Out: Last record allocated. */
{
  CDFstatus pStatus = CDF_OK;
  Int32 nUsedEntries, lastRecs[MAX_VXR_ENTRIES];
  OFF_T VXRoffset;
  if (!sX(ReadVDR64(CDF,CDF->fp,VDRoffset,zVar,
		    VDR_VXRTAIL,&VXRoffset,
		    VDR_NULL),&pStatus)) return pStatus;
  if (VXRoffset == 0)
    *recNum = NO_RECORD;
  else {
    if (!sX(ReadVXR64(CDF->fp,VXRoffset,
		      VXR_NUSEDENTRIES,&nUsedEntries,
		      VXR_LASTREC,lastRecs,
		      VXR_NULL),&pStatus)) return pStatus;
    if ((int)nUsedEntries > MAX_VXR_ENTRIES) return CORRUPTED_V3_CDF;
    *recNum = lastRecs[(int)(nUsedEntries-1)];
  }
  return pStatus;
}

/******************************************************************************
* RecordByteOffset64.
******************************************************************************/

STATICforIDL CDFstatus RecordByteOffset64 (CDF, Var, phyRecN, offsetP)
struct CDFstruct *CDF;
struct VarStruct *Var;
Int32 phyRecN;
OFF_T *offsetP;
{
  CDFstatus pStatus = CDF_OK;
  Int32 firstRec=-1, lastRec=-1; 
  OFF_T offset = (OFF_T) -1;
  switch (Var->vType) {
    case STANDARD_:
    case SPARSE_RECORDS_: {
      if (Var->firstRecInVVR <= phyRecN && phyRecN <= Var->lastRecInVVR) {
        *offsetP = Var->offsetOfVVR64 + (OFF_T) (VVR_BASE_SIZE64 + ((OFF_T)
                           Var->NphyRecBytes) * (phyRecN - Var->firstRecInVVR));
	break;
      }
      else {
        if (!sX(SearchForRecord64(CDF,Var->VDRoffset64,Var->zVar,
	  		          phyRecN,&firstRec,&lastRec,
			          &offset,NULL),&pStatus)) return pStatus;
        *offsetP = offset + (OFF_T) (VVR_BASE_SIZE64 + ((OFF_T)
	 	           Var->NphyRecBytes) * (phyRecN - firstRec));
	Var->firstRecInVVR = firstRec;
	Var->lastRecInVVR = lastRec;
	Var->offsetOfVVR64 = offset;
        break;
      }
    }
    case COMPRESSED_:
    case SPARSE_COMPRESSED_RECORDS_:
    case SPARSE_ARRAYS_:
    case SPARSE_RECORDS_AND_ARRAYS_:
      return CDF_INTERNAL_ERROR;
    case IN_MULTI_:
      *offsetP = ((OFF_T) phyRecN) * Var->NphyRecBytes;
      break;
    default:
      return CDF_INTERNAL_ERROR;
  }
  return pStatus;
}

/******************************************************************************
* ConfigureNEWzMode64.
******************************************************************************/

STATICforIDL CDFstatus ConfigureNEWzMode64 (CDF)
struct CDFstruct *CDF;
{
  CDFstatus pStatus = CDF_OK;
  if (!sX(UpdateNEWzMode64(CDF),&pStatus)) return pStatus;
  InitCURobjectsStates (CDF);
  return pStatus;
}

/******************************************************************************
* FindAttrByName64.
******************************************************************************/

STATICforIDL CDFstatus FindAttrByName64 (CDF, searchName, offset)
struct CDFstruct *CDF;          /* In: Pointer to CDF. */
char *searchName;               /* In: Attribute name to find. */
OFF_T *offset;                  /* Out: Offset of ADR that was found. */
{
  Int32 numAttrs;
  OFF_T tOffset, nextADR, fstADR;
  char attrName[CDF_ATTR_NAME_LEN256+1];
  CDFstatus pStatus = CDF_OK;
  Int32 attrN;
  long read_only_mode;

  pStatus = CDFlib(CONFIRM_, CDF_READONLY_MODE_, &read_only_mode, NULL_);
  if (pStatus != CDF_OK) return pStatus;
  
  /**************************************************************************
  * If in READONLYon mode, set CurADRIndex to the referenced attribute number.
  ***************************************************************************/
  if (read_only_mode == READONLYon)
  {
      for (attrN = 0; attrN < CDF->fp->GDR64->NumAttr; attrN++) 
      {
         if (!strcmpITB(CDF->fp->ADRList64[attrN]->Name,searchName)) 
         {
             CDF->fp->CurADRIndex = attrN;
             ASSIGNnotNULL (offset, DUMMY_ENTRYOFFSET);
             return CDF_OK;
         };
      };
  }
  else
  {
      /************************************************************************
      * Read number of attributes and the offset of the first ADR from the GDR.
      *************************************************************************/
      if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
                        GDR_NUMATTR,&numAttrs,
                        GDR_ADRHEAD,&fstADR,
                        GDR_NULL),&pStatus)) return pStatus;
      /************************************************************************
      * Read from ADRs until a matching attribute name is found.
      *   Note that if this is a V2.0 CDF, the last ADR will not have an
      * offset of zero for the next ADR.  For that reason, we will loop
      * through the number of attributes read from the GDR (and then stop).
      ************************************************************************/
      if (CDF->CURattrOffset64 != (OFF_T) (OFF_T) RESERVED_ATTROFFSET64)                   
        tOffset = CDF->CURattrOffset64;                                  
      else                                                             
        tOffset = fstADR;                                              

      for (attrN = 0; attrN < numAttrs; attrN++) {
         if (!sX(ReadADR64(CDF->fp,tOffset,
                           ADR_NAME,attrName,
                           ADR_ADRNEXT,&nextADR,
                           ADR_NULL),&pStatus)) return pStatus;
         if (!strcmpITB(attrName,searchName)) {
           ASSIGNnotNULL (offset, tOffset)
           return CDF_OK;
         }
         if (nextADR != 0)
           tOffset = nextADR;
         else
           tOffset = fstADR;
      };
  };
  /****************************************************************************
  * Attribute name not found, return error.
  ****************************************************************************/
  return NO_SUCH_ATTR;
}

/******************************************************************************
* FindAttrByNumber64.
******************************************************************************/

STATICforIDL CDFstatus FindAttrByNumber64 (CDF, searchNum, offset)
struct CDFstruct *CDF;          /* In: Pointer to the CDF. */
Int32 searchNum;                /* In: The attribute number to be found. */
OFF_T *offset;                  /* Out: The offset of the located ADR. */
{
  Int32 numAttrs, attrNum;
  OFF_T tOffset, nextADR, fstADR;
  CDFstatus pStatus = CDF_OK;
  Int32 attrN;
  long read_only_mode;
  /****************************************************************************
  * Validate.
  ****************************************************************************/
  if (searchNum < 0) return BAD_ATTR_NUM;
  /****************************************************************************
  * First check if the next attribute is the one being searched for.  For this
  * to be the case, an attribute must currently be selected, the next attribute
  * must exist, and the next attribute's number must be the attribute number
  * being searched for.  But don't try this if a V2.0 CDF because of the bad
  * terminating offset of the ADR linked list in those CDFs.
  ****************************************************************************/
/*
  if (!CDF->badTerminatingOffsets) {
    if (CDF->CURattrOffset64 != (OFF_T) (OFF_T) RESERVED_ATTROFFSET64) {
      OFF_T nextOffset;
      if (!sX(ReadADR64(CDF->fp,CDF->CURattrOffset64,
		        ADR_ADRNEXT,&nextOffset,
		        ADR_NULL),&pStatus)) return pStatus;
      if (nextOffset != 0) {
	Int32 nextNum;
	if (!sX(ReadADR64(CDF->fp,nextOffset,
			  ADR_NUM,&nextNum,
			  ADR_NULL),&pStatus)) return pStatus;
	if (nextNum == searchNum) {
	  *offset = nextOffset;
	  return pStatus;
	}
      }
    }
  }
*/
  pStatus = CDFlib(CONFIRM_, CDF_READONLY_MODE_, &read_only_mode, NULL_);
  if (pStatus != CDF_OK) return pStatus;
  /***************************************************************************
  * If in read only mode, set CurADRIndex to the searched for atribute number.
  ***************************************************************************/
  if (read_only_mode == READONLYon)
  {
      if (CDF->fp->GDR64->NumAttr <= searchNum) return NO_SUCH_ATTR;
      if (searchNum < 0 || searchNum >= CDF->fp->GDR64->NumAttr) return NO_SUCH_ATTR;
      CDF->fp->CurADRIndex = searchNum;
      ASSIGNnotNULL (offset, DUMMY_ENTRYOFFSET);
      return CDF_OK;
  }
  else
  {
      /************************************************************************
      * The next attribute isn't the one being searched for.  First read the
      * number of attributes and the offset of the first ADR from the GDR.
      ************************************************************************/
      if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
                        GDR_NUMATTR,&numAttrs,
                        GDR_ADRHEAD,&fstADR,
                        GDR_NULL),&pStatus)) return pStatus;
      if (numAttrs <= searchNum) return NO_SUCH_ATTR;
      if (searchNum < 0 || searchNum >= numAttrs) return NO_SUCH_ATTR;
      if (CDF->CURattrOffset64 != (OFF_T) RESERVED_ATTROFFSET64)
        tOffset = CDF->CURattrOffset64;
      else
        tOffset = fstADR;
      /*************************************************************************
      * Read from ADRs until a matching attribute number is found.
      *   Note that if this is a V2.0 CDF, the last ADR will not have an
      * offset of zero for the next ADR.  For that reason, we will loop
      * through the number of attributes read from the GDR (and then stop).
      *************************************************************************/
      for (attrN = 0; attrN < numAttrs; attrN++) {
         if (!sX(ReadADR64(CDF->fp,tOffset,
	                   ADR_NUM,&attrNum,
		           ADR_ADRNEXT,&nextADR,
		           ADR_NULL),&pStatus)) return pStatus;
         if (attrNum == searchNum) {
           ASSIGNnotNULL (offset, tOffset)
           return CDF_OK;
         }
         if (nextADR != 0)
           tOffset = nextADR;
         else
           tOffset = fstADR;
      }
      /*************************************************************************
      * Attribute number not found, internal error or corrupted CDF.
      *************************************************************************/
      return CORRUPTED_V3_CDF;
  };
}

/******************************************************************************
* FindEntryByNumber64.
******************************************************************************/

STATICforIDL CDFstatus FindEntryByNumber64 (CDF, ADRoffset, zEntry, entryN,
					    offset)
struct CDFstruct *CDF;          /* In: Pointer to the CDF. */
OFF_T ADRoffset;                /* In: Offset of attribute's ADR. */
Logical zEntry;			/* In: TRUE if AzEDR list to be searched.
				       FALSE if AgrEDR list to be searched. */
Int32 entryN;                    /* In: The entry number being searched for. */
OFF_T *offset;                  /* Out: The offset of the located AEDR. */
{
  Int32 numEntries, entryNum;
  OFF_T tOffset, nextADR;
  CDFstatus pStatus = CDF_OK;
  Int32 entryX;
  long read_only_mode;
  /****************************************************************************
  * Read number of entries and the offset of the first AEDR from the ADR.
  ****************************************************************************/
  if (!sX(ReadADR64(CDF->fp,ADRoffset,
		    BOO(zEntry,ADR_NzENTRIES,ADR_NgrENTRIES),&numEntries,
		    BOO(zEntry,ADR_AzEDRHEAD,ADR_AgrEDRHEAD),&tOffset,
		    ADR_NULL),&pStatus)) return pStatus;
  pStatus = CDFlib(CONFIRM_, CDF_READONLY_MODE_, &read_only_mode, NULL_);
  if (pStatus != CDF_OK) return pStatus;
  if (read_only_mode == READONLYon)
  {
      /***********************************************************************
      * If the requested entry exists, set CurAEDRIndex to the entry number and
      * set CURzEntrySel to reflect if the entry is in the z list or the gr 
      * list.
      ************************************************************************/
      if (zEntry && entryN <= CDF->fp->ADRList64[CDF->fp->CurADRIndex]->
          MAXzEntry)
      {
          if (CDF->fp->ADRList64[CDF->fp->CurADRIndex]->zAEDRList64[entryN] 
              != NULL)
          {
              CDF->fp->CURzEntrySel = TRUE;
              CDF->fp->CurAEDRIndex = entryN;
              ASSIGNnotNULL (offset, DUMMY_ENTRYOFFSET)
              return pStatus;
          }
          else
          {
              return NO_SUCH_ENTRY;
          }
      }
      else if (!zEntry && entryN <= CDF->fp->ADRList64[CDF->fp->CurADRIndex]->
               MAXgrEntry)
      {
          if (CDF->fp->ADRList64[CDF->fp->CurADRIndex]->grAEDRList64[entryN] 
              != NULL)
          {
              CDF->fp->CURzEntrySel = FALSE;
              CDF->fp->CurAEDRIndex = entryN;
              ASSIGNnotNULL (offset, DUMMY_ENTRYOFFSET)
              return pStatus; 
          }
          else
          {
              return NO_SUCH_ENTRY;
          };
      }
      else
      {
          return NO_SUCH_ENTRY;
      };
  };
  /****************************************************************************
  * Read from AEDRs until a matching entry number is found.
  *   Note that if this is a V2.0 CDF, the last AEDR will not have an
  * offset of zero for the next AEDR.  For that reason, we will loop
  * through the number of entries read from the ADR (and then stop).
  ****************************************************************************/
  for (entryX = 0; entryX < numEntries; entryX++) {
     if (!sX(ReadAEDR64(CDF->fp,tOffset,
		        AEDR_NUM,&entryNum,
		        AEDR_AEDRNEXT,&nextADR,
		        AEDR_NULL),&pStatus)) return pStatus;
     if (entryNum == entryN) {
       ASSIGNnotNULL (offset, tOffset)
       return CDF_OK;
     }
     tOffset = nextADR;
  }
  /****************************************************************************
  * Entry number not found.
  ****************************************************************************/
  return NO_SUCH_ENTRY;
}

/******************************************************************************
* FindLastAttr64.
******************************************************************************/

STATICforIDL CDFstatus FindLastAttr64 (CDF, lastOffset)
struct CDFstruct *CDF;          /* In: Pointer to the CDF. */
OFF_T *lastOffset;              /* Out: Offset of last attribute's ADR. */
{
  CDFstatus pStatus = CDF_OK;
  Int32 nAttrs;
  OFF_T offset;
  Int32 attrN;
  long read_only_mode;
  pStatus = CDFlib(CONFIRM_, CDF_READONLY_MODE_, &read_only_mode, NULL_);
  if (pStatus != CDF_OK) return pStatus;
  
  /*************************************************************************
  * If in READONLYon mode, set CurADRIndex to the index of the last
  * attribute in menmory.
  *************************************************************************/
  if (read_only_mode == READONLYon)
  {
      *lastOffset = DUMMY_ATTROFFSET;
      CDF->fp->CurADRIndex = CDF->fp->GDR64->NumAttr - 1;
  }
  else
  {
      /************************************************************************
      * Read number of attributes and the offset of the first ADR.  If there are
      * no attributes, return an offset of zero.
      ************************************************************************/
      if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
                        GDR_NUMATTR,&nAttrs,
                        GDR_NULL),&pStatus)) return pStatus;
      if (nAttrs == 0) {
        *lastOffset = 0;
        return pStatus;
      }
      /************************************************************************
      * There is at least one attribute.
      * Note that if this is a V2.0 CDF, the last ADR will not have an offset of
      * zero for the next ADR.  For that reason, we will loop through the number
      * of attributes read from the GDR (and then stop).
      ************************************************************************/
      if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
                        GDR_ADRHEAD,&offset,
                        GDR_NULL),&pStatus)) return pStatus;
      for (attrN = 0; attrN < nAttrs - 1; attrN++) {
         if (!sX(ReadADR64(CDF->fp,offset,
                           ADR_ADRNEXT,&offset,
                           ADR_NULL),&pStatus)) return pStatus;
      }
      *lastOffset = offset;
  };
  return pStatus;
}

/******************************************************************************
* FindLastEntry64.
******************************************************************************/

STATICforIDL CDFstatus FindLastEntry64 (CDF, ADRoffset, zEntry, lastOffset)
struct CDFstruct *CDF;          /* In: Pointer to the CDF. */
OFF_T ADRoffset;                /* In: Offset of attribute's ADR. */
Logical zEntry;                 /* In: TRUE if (real) zEntry is being searched
				   for; FALSE if gEntry/rEntry. */
OFF_T *lastOffset;              /* Out: The offset of the last AEDR. */
{
  CDFstatus pStatus = CDF_OK;
  Int32 nEntries;
  OFF_T offset;
  Int32 entryX;
  long read_only_mode;

  pStatus = CDFlib(CONFIRM_, CDF_READONLY_MODE_, &read_only_mode, NULL_);
  if (pStatus != CDF_OK) return pStatus;
  /***********************************************************************
  * If in READONLYon mode, set CurAEDRIndex to MAXzEntry or MAXgrEntry, as
  * appropriate.
  ***********************************************************************/
  if (read_only_mode == READONLYon)
  {
      *lastOffset = DUMMY_ENTRYOFFSET;
      if (zEntry == TRUE)
      {
          CDF->fp->CurAEDRIndex = 
                           CDF->fp->ADRList64[CDF->fp->CurADRIndex]->MAXzEntry;
      }
      else
      {
          CDF->fp->CurAEDRIndex = 
                           CDF->fp->ADRList64[CDF->fp->CurADRIndex]->MAXgrEntry;
      };
  }
  else
  {
      /************************************************************************
      * Read offset of first AEDR and determine if there are any entries (of the
      * specified type).  If there are none, pass back an offset of zero.
      ************************************************************************/
      if (!sX(ReadADR64(CDF->fp,ADRoffset,
                        BOO(zEntry,ADR_AzEDRHEAD,ADR_AgrEDRHEAD),
                        &offset,ADR_NULL),&pStatus)) return pStatus;
      if (offset == 0) {
        *lastOffset = 0;
        return pStatus;
      }
      /************************************************************************
      * There is at least one entry.  Read the actual number of entries and then
      * scan through to the last one.
      *   Note that if this is a V2.0 CDF, the last AEDR will not have an
      * offset of zero for the next AEDR.  For that reason, we will loop
      * through the number of entries (minus one) read from the ADR (and then
      * stop).
      ************************************************************************/
      if (!sX(ReadADR64(CDF->fp,ADRoffset,
                        BOO(zEntry,ADR_NzENTRIES,ADR_NgrENTRIES),&nEntries,
                        ADR_NULL),&pStatus)) return pStatus;
      for (entryX = 0; entryX < nEntries - 1; entryX++) {
         if (!sX(ReadAEDR64(CDF->fp,offset,
                            AEDR_AEDRNEXT,&offset,
                            AEDR_NULL),&pStatus)) return pStatus;
      }
      *lastOffset = offset;
  };
  return pStatus;
}

/******************************************************************************
* FindPrevEntry64.
******************************************************************************/

STATICforIDL CDFstatus FindPrevEntry64 (CDF, ADRoffset, searchOffset, zEntry,
				        prevOffset)
struct CDFstruct *CDF;          /* Pointer to the CDF. */
OFF_T ADRoffset;                /* Offset of attribute's ADR. */
OFF_T searchOffset;             /* The entry offset being searched for. */
Logical zEntry;                 /* TRUE if (real) zEntry is being searched for;
				   FALSE if gEntry/rEntry. */
OFF_T *prevOffset;              /* The offset of the previous AEDR. */
{
  CDFstatus pStatus = CDF_OK;
  Int32 nEntries;
  OFF_T offset, nextOffset;
  int entryX;
  /****************************************************************************
  * Read the offset of the first AEDR.  If that offset is the same as the
  * search offset, return an offset of zero.
  ****************************************************************************/
  if (!sX(ReadADR64(CDF->fp,ADRoffset,
		    BOO(zEntry,ADR_AzEDRHEAD,ADR_AgrEDRHEAD),&offset,
		    ADR_NULL),&pStatus)) return pStatus;
  if (offset == searchOffset) {
    *prevOffset = 0;
    return pStatus;
  }
  /****************************************************************************
  * The first AEDR is not at the search offset.  Read the actual number of
  * entries and then scan through them looking for the AEDR offset being
  * searched for.  If the search offset is not found, then either the CDF is
  * corrupted or an internal logic error has occurred.
  *   Note that if this is a V2.0 CDF, the last AEDR will not have an
  * offset of zero for the next AEDR.  For that reason, we will loop
  * through the number of entries read from the ADR (and then stop).
  ****************************************************************************/
  if (!sX(ReadADR64(CDF->fp,ADRoffset,
		    BOO(zEntry,ADR_NzENTRIES,ADR_NgrENTRIES),&nEntries,
		    ADR_NULL),&pStatus)) return pStatus;
  for (entryX = 0; entryX < nEntries; entryX++) {
     if (!sX(ReadAEDR64(CDF->fp,offset,
		        AEDR_AEDRNEXT,&nextOffset,
		        AEDR_NULL),&pStatus)) return pStatus;
     if (nextOffset == searchOffset) {
       *prevOffset = offset;
       return pStatus;
     }
     offset = nextOffset;
  }
  return CORRUPTED_V3_CDF;
}

/******************************************************************************
* FindVarByName64.
*    Both the rVariable and zVariable lists are searched (since variable names
* are unique in a CDF).
******************************************************************************/

STATICforIDL CDFstatus FindVarByName64 (CDF, searchName, offset, zVar, Var)
struct CDFstruct *CDF;  /* In: Pointer to the CDF. */
char *searchName;       /* In: The variable name being searched for. */
OFF_T *offset;          /* Out: Offset of the zVDR/rVDR. */
Logical *zVar;          /* Out: TRUE if a zVariable. */
struct VarStruct **Var; /* Out: Pointer to variable structure. */
{
  int varN;
  char varName[CDF_VAR_NAME_LEN256+1];  
  CDFstatus pStatus = CDF_OK;
  OFF_T tOffset, nextVDR;
  OFF_T headOffset;
  /****************************************************************************
  * Read from rVDRs until a matching variable name is found.
  *   Note that if this is a V2.0 CDF, the last rVDR will not have an
  * offset of zero for the next rVDR.  For that reason, we will loop
  * through the number of rVariables (and then stop).
  ****************************************************************************/
  if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
		    GDR_rVDRHEAD,&headOffset,
		    GDR_NULL),&pStatus)) return pStatus;

  if (CDF->CURrVarNum != RESERVED_VARNUM)
    tOffset = CDF->CURrVarOffset64;
  else
    tOffset = headOffset;

  for (varN = 0; varN < CDF->NrVars; varN++) {
     if (!sX(ReadVDR64(CDF,CDF->fp,tOffset,FALSE,
		       VDR_NAME,varName,
		       VDR_VDRNEXT,&nextVDR,
		       VDR_NULL),&pStatus)) return pStatus;
     if (!strcmpITB(varName,searchName)) {
       ASSIGNnotNULL (offset, tOffset)
       ASSIGNnotNULL (zVar, FALSE)
       ASSIGNnotNULL (Var, CDF->rVars[varN])
       return CDF_OK;
     }
     if (nextVDR == 0) tOffset = headOffset;
     else tOffset = nextVDR;
  }
  /****************************************************************************
  * Read from zVDRs until a matching variable name is found.
  ****************************************************************************/
  if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
		    GDR_zVDRHEAD,&headOffset,
		    GDR_NULL),&pStatus)) return pStatus;
/*
  if (CDF->CURzVarNum != RESERVED_VARNUM)
    tOffset = CDF->CURzVarOffset64;
  else
*/
  tOffset = headOffset;

  for (varN = 0; varN < CDF->NzVars; varN++) {
     if (!sX(ReadVDR64(CDF,CDF->fp,tOffset,TRUE,
		       VDR_NAME,varName,
		       VDR_VDRNEXT,&nextVDR,
		       VDR_NULL),&pStatus)) return pStatus;
     if (!strcmpITB(varName,searchName)) {
       ASSIGNnotNULL (offset, tOffset)
       ASSIGNnotNULL (zVar, TRUE)
       ASSIGNnotNULL (Var, CDF->zVars[varN])
       return CDF_OK;
     }
     if (nextVDR == 0) tOffset = headOffset;
     else tOffset = nextVDR;
  }
  /****************************************************************************
  * Variable name not found, return error.
  ****************************************************************************/
  return NO_SUCH_VAR;
}

/******************************************************************************
* FindVarByNumber64.
******************************************************************************/

STATICforIDL CDFstatus FindVarByNumber64 (CDF, searchNum, zVar, offset)
struct CDFstruct *CDF;  /* In: Pointer to CDF. */
Int32 searchNum;        /* In: Variable number to be searched for. */
Logical zVar;           /* In: TRUE if a (real) zVariable number should be
			   found. */
OFF_T *offset;          /* Out: offset of the VDR. */
{
  CDFstatus pStatus = CDF_OK;
  Int32 nVars = BOO(zVar,CDF->NzVars,CDF->NrVars);
  Int32 varNum;
  OFF_T tOffset, nextVDR, fstOffset;
  int varN;
  /****************************************************************************
  * Read offset of first VDR.
  ****************************************************************************/
  if (searchNum < 0) return BAD_VAR_NUM;
  if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
		    BOO(zVar,GDR_zVDRHEAD,GDR_rVDRHEAD),&fstOffset,
		    GDR_NULL),&pStatus)) return pStatus;
  if (nVars <= searchNum) return NO_SUCH_VAR;
  /****************************************************************************
  * Read from VDRs until a matching variable number is found.
  *   Note that if this is a V2.0 CDF, the last VDR will not have an
  * offset of zero for the next VDR.  For that reason, we will loop
  * through the number of variables (and then stop).
  ****************************************************************************/

  if (zModeON(CDF)) {
    tOffset = fstOffset;
    if (CDF->CURzVarNum != RESERVED_VARNUM) {
      long numT = BOO(zVar, CDF->CURzVarNum-CDF->NrVars, CDF->CURzVarNum);
      if (numT > -1 && numT <= searchNum) tOffset = CDF->CURzVarOffset64;
    }
  }
  else {
    if (zVar) {
      if (CDF->CURzVarNum != RESERVED_VARNUM && CDF->CURzVarNum < searchNum)
        tOffset = CDF->CURzVarOffset64;
      else
        tOffset = fstOffset;
    } else {
      if (CDF->CURrVarNum != RESERVED_VARNUM && CDF->CURrVarNum < searchNum)
        tOffset = CDF->CURrVarOffset64;
      else
        tOffset = fstOffset;
    }
  }

  /****************************************************************************
  * Search for the variable from the current selected variabale, instead of
  * the top of the variable list. It will speed up the search process if the 
  * variables are accessed in a orderly sequence.
  ****************************************************************************/   
  for (varN = 0; varN < nVars; varN++) {
     if (!sX(ReadVDR64(CDF,CDF->fp,tOffset,zVar,
		       VDR_NUM,&varNum,
		       VDR_VDRNEXT, &nextVDR, 
		       VDR_NULL),&pStatus)) return pStatus;
     if (varNum == searchNum) {
       ASSIGNnotNULL (offset, tOffset)
       return CDF_OK;
     }
     if (nextVDR != 0) 			/* Reaching the end of the list? */
       tOffset = nextVDR;		/* No.... Continue.              */
     else 
       tOffset = fstOffset;		/* Yes... Go back to the top.    */ 
  }
  /****************************************************************************
  * Variable number not found, return error.
  ****************************************************************************/
  return CORRUPTED_V3_CDF;
}

/******************************************************************************
* VerifyNoRecordsWritten64.
*    Verifies that no records have been written.  Both the rVariable and
* zVariable lists are searched.
******************************************************************************/

STATICforIDL CDFstatus VerifyNoRecordsWritten64 (CDF, no)
struct CDFstruct *CDF;          /* In: Pointer to the CDF. */
Logical *no;                    /* Out: If TRUE, no records written. */
{
  CDFstatus pStatus = CDF_OK; Int32 maxRec; int varN; Logical zVar; 
  OFF_T tOffset;
  /****************************************************************************
  * Read from r/zVDRs until a maximum record greater than NO_RECORD is found.
  *   Note that if this is a V2.0 CDF, the last rVDR will not have an
  * offset of zero for the next rVDR.  For that reason, we will loop
  * through the number of variables (and then stop).
  ****************************************************************************/
  for (zVar = 0; zVar <= 1; zVar++) {
     if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
                       BOO(zVar,GDR_zVDRHEAD,GDR_rVDRHEAD),&tOffset,
                       GDR_NULL),&pStatus)) return pStatus;
     for (varN = 0; varN < BOO(zVar,CDF->NzVars,CDF->NrVars); varN++) {
        if (!sX(ReadVDR64(CDF,CDF->fp,tOffset,zVar,
                          VDR_MAXREC,&maxRec,
                          VDR_VDRNEXT,&tOffset,
                          VDR_NULL),&pStatus)) return pStatus;
        if (maxRec > NO_RECORD) {
          *no = FALSE;
          return pStatus;
        }
     }
  }
  /****************************************************************************
  * No records written.
  ****************************************************************************/
  *no = TRUE;
  return pStatus;
}

/******************************************************************************
* VerifyNoPadsSpecified64.
*    Verifies that no pad values have been specified.  Both the rVariable
* and zVariable lists are searched.
******************************************************************************/

STATICforIDL CDFstatus VerifyNoPadsSpecified64 (CDF, no)
struct CDFstruct *CDF;          /* In: Pointer to the CDF. */
Logical *no;                    /* Out: If TRUE, no pad values written. */
{
  CDFstatus pStatus = CDF_OK;
  Int32 flags32; OFF_T tOffset;
  int varN;
  /****************************************************************************
  * Read from rVDRs until a pad value is found.
  *   Note that if this is a V2.0 CDF, the last rVDR will not have an
  * offset of zero for the next rVDR.  For that reason, we will loop
  * through the number of rVariables (and then stop).
  ****************************************************************************/
  if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
                    GDR_rVDRHEAD,&tOffset,
                    GDR_NULL),&pStatus)) return pStatus;
  for (varN = 0; varN < CDF->NrVars; varN++) {
     if (!sX(ReadVDR64(CDF,CDF->fp,tOffset,FALSE,
                       VDR_FLAGS,&flags32,
                       VDR_NULL),&pStatus)) return pStatus;
     if (PADvalueBITset(flags32)) {
       *no = FALSE;
       return pStatus;
     }
     if (!sX(ReadVDR64(CDF,CDF->fp,tOffset,FALSE,
                       VDR_VDRNEXT,&tOffset,
                       VDR_NULL),&pStatus)) return pStatus;
  }
  /****************************************************************************
  * Read from zVDRs until a pad value is found.
  ****************************************************************************/
  if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
                    GDR_zVDRHEAD,&tOffset,
                    GDR_NULL),&pStatus)) return pStatus;
  for (varN = 0; varN < CDF->NzVars; varN++) {
     if (!sX(ReadVDR64(CDF,CDF->fp,tOffset,TRUE,
                       VDR_FLAGS,&flags32,
                       VDR_NULL),&pStatus)) return pStatus;
     if (PADvalueBITset(flags32)) {
       *no = FALSE;
       return pStatus;          
     }
     if (!sX(ReadVDR64(CDF,CDF->fp,tOffset,TRUE,
                       VDR_VDRNEXT,&tOffset,
                       VDR_NULL),&pStatus)) return pStatus;
  }
  /****************************************************************************
  * No pad values specified.
  ****************************************************************************/
  *no = TRUE; 
  return pStatus;
} 
  
/******************************************************************************
* VerifyNoEntriesWritten64.
******************************************************************************/
     
STATICforIDL CDFstatus VerifyNoEntriesWritten64 (CDF, no)
struct CDFstruct *CDF;
Logical *no;
{      
  CDFstatus pStatus = CDF_OK;
  Int32 numAttrs, nEntries; OFF_T tOffset;
  int attrN;
  /****************************************************************************
  * Read number of attributes and the offset of the first ADR from the GDR.
  ****************************************************************************/
  if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
                    GDR_NUMATTR,&numAttrs, 
                    GDR_ADRHEAD,&tOffset,
                    GDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * Read from ADRs until an entry is found.
  *   Note that if this is a V2.0 CDF, the last ADR will not have an
  * offset of zero for the next ADR.  For that reason, we will loop
  * through the number of attributes read from the GDR (and then stop).
  ****************************************************************************/
  for (attrN = 0; attrN < numAttrs; attrN++) {
     if (!sX(ReadADR64(CDF->fp,tOffset,
                       ADR_NgrENTRIES,&nEntries,
                       ADR_NULL),&pStatus)) return pStatus;
     if (nEntries > 0) {
       *no = FALSE;
       return pStatus;
     }
     if (!sX(ReadADR64(CDF->fp,tOffset,
                       ADR_NzENTRIES,&nEntries,
                       ADR_NULL),&pStatus)) return pStatus;
     if (nEntries > 0) {
       *no = FALSE;
       return pStatus;
     }
     if (!sX(ReadADR64(CDF->fp,tOffset,
                       ADR_ADRNEXT,&tOffset,
                       ADR_NULL),&pStatus)) return pStatus;
  }
  /****************************************************************************
  * No entries detected.
  ****************************************************************************/
  *no = TRUE;
  return pStatus;
}

/******************************************************************************
* DefaultPadBuffer64.
******************************************************************************/

STATICforIDL CDFstatus DefaultPadBuffer64 (CDF, Var, nValues, buffer)
struct CDFstruct *CDF;
struct VarStruct *Var;
OFF_T nValues;
void *buffer;
{
  CDFstatus pStatus = CDF_OK;
  Byte1 *tBuffer = buffer; Int32 dataType, numElems;
  Int32 version, release; OFF_T i;
  if (!sX(ReadCDR64(CDF->fp,CDF->CDRoffset64,
                    CDR_VERSION,&version,
                    CDR_RELEASE,&release,
                    CDR_NULL),&pStatus)) return pStatus;
  if (!sX(ReadVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
                    VDR_DATATYPE,&dataType,
                    VDR_NUMELEMS,&numElems,
                    VDR_NULL),&pStatus)) return pStatus;
  for (i = 0; i < nValues; i++, tBuffer += (size_t) Var->NvalueBytes) {
    if (version*100+release < 305)
       DefaultPadValuePre350 (dataType, numElems, tBuffer);
    else
       DefaultPadValue (dataType, numElems, tBuffer);
  }
  return pStatus;
}

/******************************************************************************
* PadBuffer64.
******************************************************************************/

STATICforIDL CDFstatus PadBuffer64 (CDF, Var, nValues, buffer)
struct CDFstruct *CDF;          /* Pointer to CDF. */
struct VarStruct *Var;          /* Pointer to variable. */
OFF_T nValues;                  /* Number of values in buffer. */
void *buffer;                   /* Buffer to pad. */
{
  CDFstatus pStatus = CDF_OK;
  Int32 flags, dataType, numElems;
  /****************************************************************************
  * Read the flags, data type, and number of elements fields of the VDR.
  ****************************************************************************/
  if (!sX(ReadVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
		    VDR_FLAGS,&flags,
		    VDR_DATATYPE,&dataType,
		    VDR_NUMELEMS,&numElems,
		    VDR_NULL),&pStatus)) return pStatus;
  /****************************************************************************
  * If a pad value has been specified for the variable, read the pad value
  * from the VDR and duplicate for the desired number of values.  Otherwise,
  * copy the desired number of default pad values into the buffer.  Then
  * convert the padded buffer into the desired decoding.
  ****************************************************************************/
  if (PADvalueBITset(flags)) {
    Byte1 *tBuffer = buffer; Int32 valueN;
    if (!sX(ReadVDR64(CDF,CDF->fp,Var->VDRoffset64,Var->zVar,
		      VDR_PADVALUE,tBuffer,
		      VDR_NULL),&pStatus)) return pStatus;
    for (valueN = 1; valueN < nValues; valueN++) {
       memmove (tBuffer + ((size_t) Var->NvalueBytes), tBuffer,
		(size_t) Var->NvalueBytes);
       tBuffer += (size_t) Var->NvalueBytes;
    }
    if (!sX(ConvertBuffer(CDF->encoding,CDF->decoding,
			  CDF->negToPosFp0,dataType,
			  (nValues * numElems),
			  buffer,buffer),&pStatus)) return pStatus;
  }
  else {
    if (!sX(DefaultPadBuffer64(CDF,Var,nValues,buffer),&pStatus)) return pStatus;
    if (!sX(ConvertBuffer(HostEncoding(),CDF->decoding,
			  CDF->negToPosFp0,dataType,
			  (nValues * numElems),
			  buffer,buffer),&pStatus)) return pStatus;
  }
  return pStatus;
}

/******************************************************************************
* CheckEntryOp64.
******************************************************************************/

STATICforIDL CDFstatus CheckEntryOp64 (CDF, entryType)
struct CDFstruct *CDF;
int entryType;
{
  Int32 scope; CDFstatus pStatus = CDF_OK;
  if (!sX(ReadADR64(CDF->fp,CDF->CURattrOffset64,
                    ADR_SCOPE,&scope,
                    ADR_NULL),&pStatus)) {
    AbortAccess64 (CDF, UPDATE, noDELETE);
    return pStatus;
  }
  if (GLOBALscope(scope)) {
    if (entryType != gENTRYt) return ILLEGAL_FOR_SCOPE;
  }
  else {
    if (entryType == gENTRYt) return ILLEGAL_FOR_SCOPE;
    if (BADzOP(CDF,entryType == rENTRYt)) return ILLEGAL_IN_zMODE;
  }
  return pStatus;
}

/******************************************************************************
* SetCURgrEntry64.
******************************************************************************/

STATICforIDL CDFstatus SetCURgrEntry64 (CDF, useCurrent, entryNum)
struct CDFstruct *CDF;
Logical useCurrent;	/* TRUE if current g/rEntry offset can be used to speed
			   up the search. */
Int32 entryNum;		/* The new g/rEntry number. */
{
  CDFstatus pStatus = CDF_OK, tStatus;
  Int32 scope, attrNum, attrNumX, entryNumX;
  OFF_T offset, nextOffset;
  long read_only_mode;
  /****************************************************************************
  * Check if the new g/rEntry number is the reserved entry number.
  ****************************************************************************/
  if (entryNum == RESERVED_ENTRYNUM) {
    CDF->CURgrEntryNum = RESERVED_ENTRYNUM;
    CDF->fp->CurAEDRIndex = RESERVED_ENTRYNUM;
    CDF->CURgrEntryOffset64 = (OFF_T) RESERVED_ENTRYOFFSET64;
    return pStatus;
  }
  /****************************************************************************
  * Check that a current attribute is selected.
  ****************************************************************************/
  if (CDF->CURattrOffset64 == (OFF_T) RESERVED_ATTROFFSET64) {
    CDF->CURgrEntryNum = entryNum;
    CDF->fp->CurAEDRIndex = RESERVED_ENTRYNUM;
    CDF->CURgrEntryOffset64 = (OFF_T) RESERVED_ENTRYOFFSET64;
    return pStatus;
  }
  /****************************************************************************
  * Read the scope and number of the current attribute. If READONLYon, they
  * are already in memory.
  ****************************************************************************/
  pStatus = CDFlib(CONFIRM_, CDF_READONLY_MODE_, &read_only_mode, NULL_);
  if (pStatus != CDF_OK) return pStatus;
  if (read_only_mode == READONLYon)
  {
      scope = CDF->fp->ADRList64[CDF->fp->CurADRIndex]->Scope; 
      attrNum = CDF->fp->CurADRIndex;
  }
  else
  {
      if (!sX(ReadADR64(CDF->fp,CDF->CURattrOffset64,
                        ADR_SCOPE,&scope,
                        ADR_NUM,&attrNum,
                        ADR_NULL),&pStatus)) return pStatus;
  };
  /****************************************************************************
  * If the current attribute is variable-scoped and zMode is on, then the
  * current g/rEntry offset is n/a.
  ****************************************************************************/
  if (VARIABLEscope(scope) && zModeON(CDF)) {
    CDF->CURgrEntryNum = entryNum;
    CDF->fp->CurAEDRIndex = RESERVED_ENTRYNUM;
    CDF->CURgrEntryOffset64 = (OFF_T) RESERVED_ENTRYOFFSET64;
    return pStatus;
  }
  /****************************************************************************
  * IF READONLYoff, check if the next entry is the one being searched for.  
  * For this to be the case, an entry must currently be selected and must be 
  * associated with the current attribute, the next entry must exist, and the 
  * next entry's number must be the entry number being searched for.  But 
  * don't try this if a V2.0 CDF because of the bad terminating offset of the
  * AEDR linked lists in those CDFs.
  ****************************************************************************/
  if (read_only_mode == READONLYoff && useCurrent && 
      !CDF->badTerminatingOffsets) {
    if (CDF->CURgrEntryOffset64 != (OFF_T) RESERVED_ENTRYOFFSET64) {
      if (!sX(ReadAEDR64(CDF->fp,CDF->CURgrEntryOffset64,
		         AEDR_ATTRNUM,&attrNumX,
		         AEDR_AEDRNEXT,&nextOffset,
		         AEDR_NULL),&pStatus)) return pStatus;
      if (attrNumX == attrNum && nextOffset != 0) {
	if (!sX(ReadAEDR64(CDF->fp,nextOffset,
			   AEDR_NUM,&entryNumX,
			   AEDR_NULL),&pStatus)) return pStatus;
	if (entryNumX == entryNum) {
	  CDF->CURgrEntryNum = entryNum;
	  CDF->CURgrEntryOffset64 = nextOffset;
	  return pStatus;
	}
      }
    }
  }
  /****************************************************************************
  * Search the list of AEDRs for the entry.
  ****************************************************************************/
  tStatus = FindEntryByNumber64 (CDF, CDF->CURattrOffset64, FALSE, entryNum,
			         &offset);
  switch (tStatus) {
    case CDF_OK:
      break;
    case NO_SUCH_ENTRY:
      offset = (OFF_T) RESERVED_ENTRYOFFSET64;
      break;
    default:
      return tStatus;
  }
  CDF->CURgrEntryNum = entryNum;
  CDF->CURgrEntryOffset64 = offset;
  return pStatus;
}

/******************************************************************************
* SetCURzEntry64.
******************************************************************************/

STATICforIDL CDFstatus SetCURzEntry64 (CDF, useCurrent, entryNum)
struct CDFstruct *CDF;
Logical useCurrent;	/* TRUE if current zEntry offset can be used to speed
			   up the search. */
Int32 entryNum;		/* The new zEntry number. */
{
  CDFstatus pStatus = CDF_OK, tStatus;
  Int32 scope, attrNum, attrNumX, entryNumX;
  OFF_T offset, nextOffset;
  Logical zEntry;
  Int32 entryN;
  long read_only_mode;
  /****************************************************************************
  * Check if the new zEntry number is the reserved entry number.
  ****************************************************************************/
  if (entryNum == RESERVED_ENTRYNUM) {
    CDF->CURzEntryNum = RESERVED_ENTRYNUM;
    CDF->fp->CurAEDRIndex = RESERVED_ENTRYNUM;
    CDF->CURzEntryOffset64 = (OFF_T) RESERVED_ENTRYOFFSET64;
    return pStatus;
  }
  /****************************************************************************
  * Check that a current attribute is selected.
  ****************************************************************************/
  if (CDF->CURattrOffset64 == (OFF_T) RESERVED_ATTROFFSET64) {
    CDF->CURzEntryNum = entryNum;
    CDF->fp->CurAEDRIndex = RESERVED_ENTRYNUM;
    CDF->CURzEntryOffset64 = (OFF_T) RESERVED_ENTRYOFFSET64;
    return pStatus;
  }
  /****************************************************************************
  * Read the scope and number of the current attribute. If READONLYon, they
  * are already in memory.
  ****************************************************************************/
  pStatus = CDFlib(CONFIRM_, CDF_READONLY_MODE_, &read_only_mode, NULL_);
  if (pStatus != CDF_OK) return pStatus;
  if (read_only_mode == READONLYon)
  {
      scope = CDF->fp->ADRList64[CDF->fp->CurADRIndex]->Scope; 
      attrNum = CDF->fp->CurADRIndex;
  }
  else
  {
      if (!sX(ReadADR64(CDF->fp,CDF->CURattrOffset64,
                        ADR_SCOPE,&scope,
                        ADR_NUM,&attrNum,
                        ADR_NULL),&pStatus)) return pStatus;
  };
  /****************************************************************************
  * If the current attribute is global-scoped, then the current zEntry offset
  * is n/a.
  ****************************************************************************/
  if (GLOBALscope(scope)) {
    CDF->CURzEntryNum = entryNum;
    CDF->fp->CurAEDRIndex = RESERVED_ENTRYNUM;
    CDF->CURzEntryOffset64 = (OFF_T) RESERVED_ENTRYOFFSET64;
    return pStatus;
  }
  /****************************************************************************
  * Determine if a AgrEDR or AzEDR and the true entry number.
  ****************************************************************************/
  if (zModeON(CDF)) {
    if (entryNum < CDF->NrVars) {
      zEntry = FALSE;
      entryN = entryNum;
    }
    else {
      zEntry = TRUE;
      entryN = entryNum - CDF->NrVars;
    }
  }
  else {
    zEntry = TRUE;
    entryN = entryNum;
  }
  /****************************************************************************
  * IF READONLYoff, check if the next entry is the one being searched for.  
  * For this to be the case, an entry must currently be selected and must be 
  * associated with the current attribute, the next entry must exist, and the 
  * next entry's number must be the entry number being searched for.  But don't
  * try this if a V2.0 CDF because of the bad terminating offset of the AEDR
  * linked lists in those CDFs.
  ****************************************************************************/
  if (read_only_mode == READONLYoff && useCurrent && 
      !CDF->badTerminatingOffsets) {
    if (CDF->CURzEntryOffset64 != (OFF_T) RESERVED_ENTRYOFFSET64) {
      if (!sX(ReadAEDR64(CDF->fp,CDF->CURzEntryOffset64,
		         AEDR_ATTRNUM,&attrNumX,
		         AEDR_AEDRNEXT,&nextOffset,
		         AEDR_NULL),&pStatus)) return pStatus;
      if (attrNumX == attrNum && nextOffset != 0) {
	if (!sX(ReadAEDR64(CDF->fp,nextOffset,
			   AEDR_NUM,&entryNumX,
			   AEDR_NULL),&pStatus)) return pStatus;
	if (entryNumX == entryN) {
	  CDF->CURzEntryNum = entryNum;
	  CDF->CURzEntryOffset64 = nextOffset;
	  return pStatus;
	}
      }
    }
  }
  /****************************************************************************
  * Search the list of AEDRs for the entry.
  ****************************************************************************/
  tStatus = FindEntryByNumber64 (CDF, CDF->CURattrOffset64, zEntry, entryN,
			         &offset);
  switch (tStatus) {
    case CDF_OK:
      break;
    case NO_SUCH_ENTRY:
      offset = (OFF_T) RESERVED_ENTRYOFFSET64;
      break;
    default:
      return tStatus;
  }
  CDF->CURzEntryNum = entryNum;
  CDF->CURzEntryOffset64 = offset;
  return pStatus;
}

/******************************************************************************
* VariableType64.
******************************************************************************/

STATICforIDL CDFstatus VariableType64 (CDF, vdrOffset, zVar, vType)
struct CDFstruct *CDF;
OFF_T vdrOffset;
Logical zVar;
int *vType;
{
  CDFstatus pStatus = CDF_OK; Int32 flags, sRecords;
  if (!sX(ReadVDR64(CDF,CDF->fp,vdrOffset,zVar,
		    VDR_FLAGS,&flags,
		    VDR_sRECORDS,&sRecords,
		    VDR_NULL),&pStatus)) return pStatus;
  if (CDF->singleFile) {
    if (VARcompressionBITset(flags) && SPARSEarraysBITset(flags)) {
      return CORRUPTED_V3_CDF;
    }
    if (sRecords == NO_SPARSERECORDS) {
      *vType = STANDARD_;
      if (VARcompressionBITset(flags)) *vType = COMPRESSED_;
      if (SPARSEarraysBITset(flags)) *vType = SPARSE_ARRAYS_;
    }
    else {
      *vType = SPARSE_RECORDS_;
      if (VARcompressionBITset(flags)) *vType = SPARSE_COMPRESSED_RECORDS_;
      if (SPARSEarraysBITset(flags)) *vType = SPARSE_RECORDS_AND_ARRAYS_;
    }
  }
  else {
    *vType = IN_MULTI_;
    if (VARcompressionBITset(flags)) return CORRUPTED_V3_CDF;
    if (SPARSEarraysBITset(flags)) return CORRUPTED_V3_CDF;
    if (sRecords != NO_SPARSERECORDS) return CORRUPTED_V3_CDF;
  }
  return pStatus;
}

/******************************************************************************
* Compress64.
******************************************************************************/

STATICforIDL CDFstatus Compress64 (iFp, iOffset, iSize, iError, cType, cParms,
				   oFp, oOffset, oSize, oError)
vFILE *iFp;
OFF_T iOffset;
OFF_T iSize;
CDFstatus iError;
Int32 cType;
Int32 cParms[];
vFILE *oFp;
OFF_T oOffset;
OFF_T *oSize;
CDFstatus oError;
{
  CDFstatus pStatus = CDF_OK;
  switch (cType) {
    case RLE_COMPRESSION: {
      if (cParms[0] != RLE_OF_ZEROs) return UNKNOWN_COMPRESSION;
      if (!sX(CompressRLE0_64(iFp,iOffset,iSize,iError,
			      oFp,oOffset,oSize,oError),&pStatus)) return pStatus;
      break;
    }
    case HUFF_COMPRESSION:
      if (cParms[0] != OPTIMAL_ENCODING_TREES) return UNKNOWN_COMPRESSION;
      if (!sX(CompressHUFF0_64(iFp,iOffset,
			       iSize,iError,oFp,
			       oOffset,oSize,oError),&pStatus)) return pStatus;
      break;
    case AHUFF_COMPRESSION:
      if (cParms[0] != OPTIMAL_ENCODING_TREES) return UNKNOWN_COMPRESSION;
      if (!sX(CompressAHUFF0_64(iFp,iOffset,
			        iSize,iError,oFp,
			        oOffset,oSize,oError),&pStatus)) return pStatus;
      break;
    case GZIP_COMPRESSION:
      if (!INCLUSIVE(1,cParms[0],9)) return UNKNOWN_COMPRESSION;
      if (!sX(CompressGZIP_64(iFp,iOffset,iSize,iError,
			      oFp,oOffset,oSize,
			      oError,cParms[0]),&pStatus)) return pStatus;
      break;
    default:
      return UNKNOWN_COMPRESSION;
  }
  return pStatus;
}

/******************************************************************************
* Decompress64.
******************************************************************************/

STATICforIDL CDFstatus Decompress64 (iFp, iOffset, iSize, iError, cType, cParms,
				     oFp, oOffset, oError)
vFILE *iFp;
OFF_T iOffset;
OFF_T iSize;
CDFstatus iError;
Int32 cType;
Int32 cParms[];
vFILE *oFp;
OFF_T oOffset;
CDFstatus oError;
{
  CDFstatus pStatus = CDF_OK;
  switch (cType) {
    case RLE_COMPRESSION: {
      if (cParms[0] != RLE_OF_ZEROs) return UNKNOWN_COMPRESSION;
      if (!sX(DecompressRLE0_64(iFp,iOffset,iSize,iError,
			        oFp,oOffset,oError),&pStatus)) return pStatus;
      break;
    }
    case HUFF_COMPRESSION:
      if (cParms[0] != OPTIMAL_ENCODING_TREES) return UNKNOWN_COMPRESSION;
      if (!sX(DecompressHUFF0_64(iFp,iOffset,iError,
			         oFp,oOffset,oError),&pStatus)) return pStatus;
      break;
    case AHUFF_COMPRESSION:
      if (cParms[0] != OPTIMAL_ENCODING_TREES) return UNKNOWN_COMPRESSION;
      if (!sX(DecompressAHUFF0_64(iFp,iOffset,iError,
			          oFp,oOffset,oError),&pStatus)) return pStatus;
      break;
    case GZIP_COMPRESSION:
      if (!INCLUSIVE(1,cParms[0],9)) return UNKNOWN_COMPRESSION;
      if (!sX(DecompressGZIP_64(iFp,iOffset,iSize,iError,
			        oFp,oOffset,oError),&pStatus)) return pStatus;
      break;
    default:
      return UNKNOWN_COMPRESSION;
  }
  return pStatus;
}

/******************************************************************************
* DecompressToStage64.
******************************************************************************/

STATICforIDL CDFstatus DecompressToStage64 (CDF, Var, offset, uSize)
struct CDFstruct *CDF;
struct VarStruct *Var;
OFF_T offset;
OFF_T uSize;
{
  CDFstatus pStatus = CDF_OK; Int32 irType;
  OFF_T tOffset;
  if (!sX(ReadIrType64(CDF->fp,offset,&irType),&pStatus)) return pStatus;
  switch (irType) {
    case VVR_: {
      tOffset = offset + VVR_BASE_SIZE64;
      if (!sX(CopyBytes64(CDF->fp,tOffset,
			  CDF_READ_ERROR,
			  uSize,CDF->stage.fp,
			  Var->stage.areaOffset64,
			  SCRATCH_WRITE_ERROR),&pStatus)) return pStatus;
      break;
    }
    case CVVR_: {
      struct CVVRstruct64 CVVR;
      if (!sX(ReadCVVR64(CDF->fp,offset,
		         CVVR_RECORDx,&CVVR,
		         CVVR_NULL),&pStatus)) return pStatus;
      tOffset = offset + CVVR_BASE_SIZE64;
      if (!sX(Decompress64(CDF->fp,tOffset,
			   CVVR.cSize,CDF_READ_ERROR,
			   Var->cType,Var->cParms,CDF->stage.fp,
			   Var->stage.areaOffset64,
			   SCRATCH_WRITE_ERROR),&pStatus)) return pStatus;
      break;
    }
    default:
      return CORRUPTED_V3_CDF;
  }
  return pStatus;
}
/******************************************************************************
* ResetReadOnlyState64.
******************************************************************************/

STATICforIDL void ResetReadOnlyState64 (CDF)
struct CDFstruct *CDF;
{
    int i, j;
    /*************************************************************************
    * Free all allocated memory associated with the metadata READONLYon data
    * structures and clear all the state variables.
    *************************************************************************/
    if (CDF->fp != NULL && CDF->fp->GDR64 != NULL)
    {
        for (i = 0; i < CDF->fp->GDR64->NumAttr; i++)
        {
            if (CDF->fp->ADRList64[i] != NULL)
            {
                for (j = 0; j <= CDF->fp->ADRList64[i]->MAXgrEntry; j++)
                {
                    if (CDF->fp->ADRList64[i]->grAEDRList64[j] != NULL)
                    {
                        if (CDF->fp->ADRList64[i]->grAEDRList64[j]->Value != 
                            NULL)
                        {
                            cdf_FreeMemory(
                               CDF->fp->ADRList64[i]->grAEDRList64[j]->Value, 
                               NULL);
                            CDF->fp->ADRList64[i]->grAEDRList64[j]->Value = 
                                                                           NULL;
                        };
                        cdf_FreeMemory(
                           CDF->fp->ADRList64[i]->grAEDRList64[j], NULL);
                        CDF->fp->ADRList64[i]->grAEDRList64[j] = NULL;
                    };
                };
                if (CDF->fp->ADRList64[i]->grAEDRList64 != NULL)
                  cdf_FreeMemory(CDF->fp->ADRList64[i]->grAEDRList64, NULL);
                for (j = 0; j <= CDF->fp->ADRList64[i]->MAXzEntry; j++)
                {
                    if (CDF->fp->ADRList64[i]->zAEDRList64[j] != NULL)
                    {
                        if (CDF->fp->ADRList64[i]->zAEDRList64[j]->Value != 
                            NULL)
                        {
                            cdf_FreeMemory(
                               CDF->fp->ADRList64[i]->zAEDRList64[j]->Value, 
                               NULL);
                            CDF->fp->ADRList64[i]->zAEDRList64[j]->Value = NULL;
                        };
                        cdf_FreeMemory(CDF->fp->ADRList64[i]->zAEDRList64[j],
                                       NULL);
                        CDF->fp->ADRList64[i]->zAEDRList64[j] = NULL;
                    };
                };
                if (CDF->fp->ADRList64[i]->zAEDRList64 != NULL)
                  cdf_FreeMemory(CDF->fp->ADRList64[i]->zAEDRList64, NULL);
            };
            cdf_FreeMemory(CDF->fp->ADRList64[i], NULL);
            CDF->fp->ADRList64[i] = NULL;
        };
        if (CDF->fp->ADRList64 != NULL)
        {
            cdf_FreeMemory(CDF->fp->ADRList64, NULL);
            CDF->fp->ADRList64 = NULL;
        };
        cdf_FreeMemory(CDF->fp->GDR64, NULL);
        CDF->fp->GDR64 = NULL;
        CDF->fp->CurADRIndex = 0;
        CDF->fp->CurADRIndex = RESERVED_ENTRYNUM;
        CDF->fp->CurAEDRIndex = RESERVED_ENTRYNUM;
        CDF->CURattrOffset64 = RESERVED_ATTROFFSET64;
        CDF->CURgrEntryOffset64 = (OFF_T) RESERVED_ENTRYOFFSET64;
        CDF->CURzEntryOffset64 = (OFF_T) RESERVED_ENTRYOFFSET64;
    };
}
