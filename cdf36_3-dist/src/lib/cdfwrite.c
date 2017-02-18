/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                        Write to internal record.
*
*  Version 1.5a, 28-Feb-97, Hughes STX.
*
*  Modification history:
*
*   V1.0   4-Nov-93, J Love     Original version.
*   V1.1  15-Nov-94, J Love     CDF V2.5.
*   V1.2   5-Jan-95, J Love	Encode/decode changes.
*   V1.2a 30-Jan-95, J Love	`Write32s' now checks count.
*   V1.2b 24-Feb-95, J Love	Solaris 2.3 IDL i/f.
*   V1.3  26-May-95, J Love	CDF V2.4 compatibility mode.  What?
*   V1.4  14-Jun-95, J Love	Use `ReadXYZ' routines.
*   V1.4a  6-Sep-95, J Love	CDFexport-related changes.
*   V1.5   3-Apr-96, J Love	CDF V2.6.
*   V1.5a 28-Feb-97, J Love	Windows NT for MS Visual C/C++ on an IBM PC.
*   V3.2a 11-Apr-08, M Liu      Modified Write32s to eliminate the potential
*                               buffer overflow.
*
******************************************************************************/

#include "cdflib.h"

/******************************************************************************
* Local macro definitions.
******************************************************************************/

#define CWE CDF_WRITE_ERROR

/******************************************************************************
* Write32.
******************************************************************************/

STATICforIDL Logical Write32 (fp, value)
vFILE *fp;
Int32 *value;
{
#if defined(NETWORKbyteORDERcpu)
  if (!WRITEv(value,(size_t)4,(size_t)1,fp)) return FALSE;
#else
  Int32 tValue;
  REVERSE4bIO (value, &tValue)
  if (!WRITEv(&tValue,(size_t)4,(size_t)1,fp)) return FALSE;
#endif
  return TRUE;
}

/******************************************************************************
* Write32s.
******************************************************************************/

STATICforIDL Logical Write32s (fp, buffer, count)
vFILE *fp;
Int32 *buffer;
int count;
{
#define MAX_tBUFFER_SIZE 10             /* This must be set to the maximum
					   value that `count' may ever be.
					   Currently, that is either the
					   maximum number of dimensions or
					   the number of entries in a VXR. */
#if defined(NETWORKbyteORDERcpu)
  if (count == 0) return TRUE;                                          
  if (count < 0 || count > MAX_tBUFFER_SIZE) return FALSE;
  if (!WRITEv(buffer,(size_t)4,(size_t)count,fp)) return FALSE;
#else
  Int32 tBuffer[MAX_tBUFFER_SIZE]; int i;
  if (count == 0) return TRUE;                                          
  if (count < 0 || count > MAX_tBUFFER_SIZE) return FALSE;
  for (i = 0; i < count; i++) {
     REVERSE4bIO (&buffer[i], &tBuffer[i])
  }
  if (!WRITEv(tBuffer,(size_t)4,(size_t)count,fp)) return FALSE;
#endif
  return TRUE;
}

/******************************************************************************
* WriteIrSize.
*   The size is always in the first 4-byte field.
******************************************************************************/

STATICforIDL CDFstatus WriteIrSize (fp, offset, irSize)
vFILE *fp;
Int32 offset;
Int32 *irSize;
{
  if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CWE;
  if (!Write32(fp,irSize)) return CWE;
  return CDF_OK;
}  

/******************************************************************************
* WriteIrType.
*   The type is always in the second 4-byte field.
******************************************************************************/

STATICforIDL CDFstatus WriteIrType (fp, offset, irType)
vFILE *fp;
Int32 offset;
Int32 *irType;
{
  long irTypeOffset = offset + sizeof(Int32);
  if (!SEEKv(fp,irTypeOffset,vSEEK_SET)) return CWE;
  if (!Write32(fp,irType)) return CWE;
  return CDF_OK;
}  

/******************************************************************************
* WriteCDR.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteCDR (vFILE *fp, Int32 offset, ...)
#else
STATICforIDL CDFstatus WriteCDR (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; Int32 offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, Int32);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case CDR_NULL:
	 va_end (ap);
	 return pStatus;
       case CDR_RECORD: {
	 struct CDRstruct *CDR = va_arg (ap, struct CDRstruct *);
	 char *copyRight = va_arg (ap, char *);
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,&(CDR->RecordSize))) return CWE;
	 if (!Write32(fp,&(CDR->RecordType))) return CWE;
	 if (!Write32(fp,&(CDR->GDRoffset))) return CWE;
	 if (!Write32(fp,&(CDR->Version))) return CWE;
	 if (!Write32(fp,&(CDR->Release))) return CWE;
	 if (!Write32(fp,&(CDR->Encoding))) return CWE;
	 if (!Write32(fp,&(CDR->Flags))) return CWE;
	 if (!Write32(fp,&(CDR->rfuA))) return CWE;
	 if (!Write32(fp,&(CDR->rfuB))) return CWE;
	 if (!Write32(fp,&(CDR->Increment))) return CWE;
	 if (!Write32(fp,&(CDR->rfuD))) return CWE;
	 if (!Write32(fp,&(CDR->rfuE))) return CWE;
	 if (copyRight != NULL) {
	   if (!WRITEv(copyRight,CDF_COPYRIGHT_LEN,1,fp)) return CWE;
	 }
	 break;
       }
       case CDR_COPYRIGHT: {
	 char *copyRight = va_arg (ap, char *);
	 long tOffset = offset + CDR_COPYRIGHT_OFFSET;
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!WRITEv(copyRight,CDF_COPYRIGHT_LEN,1,fp)) return CWE;
	 break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *);
	 long tOffset = offset;
	 switch (field) {
	   case CDR_RECORDSIZE: tOffset += CDR_RECORDSIZE_OFFSET; break;
	   case CDR_RECORDTYPE: tOffset += CDR_RECORDTYPE_OFFSET; break;
	   case CDR_GDROFFSET: tOffset += CDR_GDROFFSET_OFFSET; break;
	   case CDR_VERSION: tOffset += CDR_VERSION_OFFSET; break;
	   case CDR_RELEASE: tOffset += CDR_RELEASE_OFFSET; break;
	   case CDR_ENCODING: tOffset += CDR_ENCODING_OFFSET; break;
	   case CDR_FLAGS: tOffset += CDR_FLAGS_OFFSET; break;
	   case CDR_INCREMENT: tOffset += CDR_INCREMENT_OFFSET; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriteGDR.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteGDR (vFILE *fp, Int32 offset, ...)
#else
STATICforIDL CDFstatus WriteGDR (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; Int32 offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, Int32);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case GDR_NULL:
	 va_end (ap);
	 return pStatus;
       case GDR_RECORD: {
	 struct GDRstruct *GDR = va_arg (ap, struct GDRstruct *);
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,&(GDR->RecordSize))) return CWE;
	 if (!Write32(fp,&(GDR->RecordType))) return CWE;
	 if (!Write32(fp,&(GDR->rVDRhead))) return CWE;
	 if (!Write32(fp,&(GDR->zVDRhead))) return CWE;
	 if (!Write32(fp,&(GDR->ADRhead))) return CWE;
	 if (!Write32(fp,&(GDR->eof))) return CWE;
	 if (!Write32(fp,&(GDR->NrVars))) return CWE;
	 if (!Write32(fp,&(GDR->NumAttr))) return CWE;
	 if (!Write32(fp,&(GDR->rMaxRec))) return CWE;
	 if (!Write32(fp,&(GDR->rNumDims))) return CWE;
	 if (!Write32(fp,&(GDR->NzVars))) return CWE;
	 if (!Write32(fp,&(GDR->UIRhead))) return CWE;
	 if (!Write32(fp,&(GDR->rfuC))) return CWE;
	 if (!Write32(fp,&(GDR->rfuD))) return CWE;
	 if (!Write32(fp,&(GDR->rfuE))) return CWE;
	 if (!Write32s(fp,GDR->rDimSizes,(int)GDR->rNumDims)) return CWE;
	 break;
       }
       case GDR_rDIMSIZES: {
	 Int32 *rDimSizes = va_arg (ap, Int32 *), rNumDims; long tOffset;
	 if (!sX(ReadGDR(fp,offset,
			 GDR_rNUMDIMS,&rNumDims,
			 GDR_NULL),&pStatus)) return pStatus;
	 tOffset = offset + GDR_rDIMSIZES_OFFSET;
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32s(fp,rDimSizes,(int)rNumDims)) return CWE;
	 break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *);
	 Int32 tOffset = offset;
	 switch (field) {
	   case GDR_RECORDSIZE: tOffset += GDR_RECORDSIZE_OFFSET; break;
	   case GDR_RECORDTYPE: tOffset += GDR_RECORDTYPE_OFFSET; break;
	   case GDR_rVDRHEAD: tOffset += GDR_rVDRHEAD_OFFSET; break;
	   case GDR_zVDRHEAD: tOffset += GDR_zVDRHEAD_OFFSET; break;
	   case GDR_ADRHEAD: tOffset += GDR_ADRHEAD_OFFSET; break;
	   case GDR_EOF: tOffset += GDR_EOF_OFFSET; break;
	   case GDR_NrVARS: tOffset += GDR_NrVARS_OFFSET; break;
	   case GDR_NUMATTR: tOffset += GDR_NUMATTR_OFFSET; break;
	   case GDR_rMAXREC: tOffset += GDR_rMAXREC_OFFSET; break;
	   case GDR_rNUMDIMS: tOffset += GDR_rNUMDIMS_OFFSET; break;
	   case GDR_NzVARS: tOffset += GDR_NzVARS_OFFSET; break;
	   case GDR_UIRHEAD: tOffset += GDR_UIRHEAD_OFFSET; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriteADR.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteADR (vFILE *fp, Int32 offset, ...)
#else
STATICforIDL CDFstatus WriteADR (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; Int32 offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, Int32);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case ADR_NULL:
	 va_end (ap);
	 return pStatus;
       case ADR_RECORD: {
	 struct ADRstruct *ADR = va_arg (ap, struct ADRstruct *);
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,&(ADR->RecordSize))) return CWE;
	 if (!Write32(fp,&(ADR->RecordType))) return CWE;
	 if (!Write32(fp,&(ADR->ADRnext))) return CWE;
	 if (!Write32(fp,&(ADR->AgrEDRhead))) return CWE;
	 if (!Write32(fp,&(ADR->Scope))) return CWE;
	 if (!Write32(fp,&(ADR->Num))) return CWE;
	 if (!Write32(fp,&(ADR->NgrEntries))) return CWE;
	 if (!Write32(fp,&(ADR->MAXgrEntry))) return CWE;
	 if (!Write32(fp,&(ADR->rfuA))) return CWE;
	 if (!Write32(fp,&(ADR->AzEDRhead))) return CWE;
	 if (!Write32(fp,&(ADR->NzEntries))) return CWE;
	 if (!Write32(fp,&(ADR->MAXzEntry))) return CWE;
	 if (!Write32(fp,&(ADR->rfuE))) return CWE;
	 if (!WRITEv(ADR->Name,CDF_ATTR_NAME_LEN,1,fp)) return CWE;
	 break;
       }
       case ADR_NAME: {
	 char *aName = va_arg (ap, char *);
	 long tOffset = offset + ADR_NAME_OFFSET;
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!WRITEv(aName,CDF_ATTR_NAME_LEN,1,fp)) return CWE;
	 break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); long tOffset = offset;
	 switch (field) {
	   case ADR_RECORDSIZE: tOffset += ADR_RECORDSIZE_OFFSET; break;
	   case ADR_RECORDTYPE: tOffset += ADR_RECORDTYPE_OFFSET; break;
	   case ADR_ADRNEXT: tOffset += ADR_ADRNEXT_OFFSET; break;
	   case ADR_AgrEDRHEAD: tOffset += ADR_AgrEDRHEAD_OFFSET; break;
	   case ADR_SCOPE: tOffset += ADR_SCOPE_OFFSET; break;
	   case ADR_NUM: tOffset += ADR_NUM_OFFSET; break;
	   case ADR_NgrENTRIES: tOffset += ADR_NgrENTRIES_OFFSET; break;
	   case ADR_MAXgrENTRY: tOffset += ADR_MAXgrENTRY_OFFSET; break;
	   case ADR_AzEDRHEAD: tOffset += ADR_AzEDRHEAD_OFFSET; break;
	   case ADR_NzENTRIES: tOffset += ADR_NzENTRIES_OFFSET; break;
	   case ADR_MAXzENTRY: tOffset += ADR_MAXzENTRY_OFFSET; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriteAgrEDR/AzEDR.
*    If the entry value is being written, it is assumed that the value passed
* in is in the host machine's encoding.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteAEDR (struct CDFstruct *CDF, vFILE *fp,
				  Int32 offset, ...)
#else
STATICforIDL CDFstatus WriteAEDR (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  struct CDFstruct *CDF; vFILE *fp; Int32 offset;
  VA_START (ap);
  CDF = va_arg (ap, struct CDFstruct *);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, Int32);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case AEDR_NULL:
	 va_end (ap);
	 return pStatus;
       case AEDR_RECORD: {
	 struct AEDRstruct *AEDR = va_arg (ap, struct AEDRstruct *);
	 void *value = va_arg (ap, void *);
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,&(AEDR->RecordSize))) return CWE;
	 if (!Write32(fp,&(AEDR->RecordType))) return CWE;
	 if (!Write32(fp,&(AEDR->AEDRnext))) return CWE;
	 if (!Write32(fp,&(AEDR->AttrNum))) return CWE;
	 if (!Write32(fp,&(AEDR->DataType))) return CWE;
	 if (!Write32(fp,&(AEDR->Num))) return CWE;
	 if (!Write32(fp,&(AEDR->NumElems))) return CWE;
	 if (!Write32(fp,&(AEDR->rfuA))) return CWE;
	 if (!Write32(fp,&(AEDR->rfuB))) return CWE;
	 if (!Write32(fp,&(AEDR->rfuC))) return CWE;
	 if (!Write32(fp,&(AEDR->rfuD))) return CWE;
	 if (!Write32(fp,&(AEDR->rfuE))) return CWE;
         if (value != NULL) {
	   if (STRINGdataType(AEDR->DataType)) {
	     size_t len; char *newValue = NULL;
	     len = strlen ((char *)value);
	     if ((int)len < (int) AEDR->NumElems) {
	       int ix;
	       newValue = (char *) cdf_AllocateMemory ((size_t) AEDR->NumElems,
						       NULL);
	       memcpy (newValue, (char *)value, len);
	       for (ix = (int) len; ix < (int) AEDR->NumElems; ++ix)
	         *(newValue+ix) = (char) ' ';
	     }
	     if (!sX(WriteBuffer(CDF,fp,AEDR->DataType,AEDR->NumElems,
				 (newValue==NULL?value:(void *)newValue)),
		     &pStatus))
	       return pStatus;
	     if (newValue != NULL) cdf_FreeMemory (newValue, NULL);
	   } else
	     if (!sX(WriteBuffer(CDF,fp,AEDR->DataType,
				 AEDR->NumElems,value),&pStatus)) return pStatus;
	 }
	 break;
       }
       case AEDR_VALUE: {
	 void *value = va_arg (ap, void *);
	 Int32 dataType, numElems; long tOffset;
	 if (!sX(ReadAEDR(fp,offset,
			  AEDR_DATATYPE,&dataType,
			  AEDR_NUMELEMS,&numElems,
			  AEDR_NULL),&pStatus)) return pStatus;
	 tOffset = offset + AEDR_VALUE_OFFSET;
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (STRINGdataType(dataType)) {
	   size_t len; char *newValue = NULL;
	   len = strlen ((char *)value);
	   if ((int)len < (int) numElems) {
	     int ix;
	     newValue = (char *) cdf_AllocateMemory ((size_t) numElems, NULL);
	     memcpy (newValue, (char *)value, len);
	     for (ix = (int) len; ix < (int) numElems; ++ix)
	        *(newValue+ix) = (char) ' ';
	   }
	   if (!sX(WriteBuffer(CDF,fp,dataType,numElems,
			       (newValue==NULL?value:(void *)newValue)),
		   &pStatus))
	     return pStatus;
	   if (newValue != NULL) cdf_FreeMemory (newValue, NULL);
	 } else
	   if (!sX(WriteBuffer(CDF,fp,dataType,
			       numElems,value),&pStatus)) return pStatus;
	 break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); long tOffset = offset;
	 switch (field) {
	   case AEDR_RECORDSIZE: tOffset += AEDR_RECORDSIZE_OFFSET; break;
	   case AEDR_RECORDTYPE: tOffset += AEDR_RECORDTYPE_OFFSET; break;
	   case AEDR_AEDRNEXT: tOffset += AEDR_AEDRNEXT_OFFSET; break;
	   case AEDR_ATTRNUM: tOffset += AEDR_ATTRNUM_OFFSET; break;
	   case AEDR_DATATYPE: tOffset += AEDR_DATATYPE_OFFSET; break;
	   case AEDR_NUM: tOffset += AEDR_NUM_OFFSET; break;
	   case AEDR_NUMELEMS: tOffset += AEDR_NUMELEMS_OFFSET; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriterVDR/zVDR.
*    If the pad value is being written, it is assumed that the value passed
* in is in the host machine's encoding.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteVDR (struct CDFstruct *CDF, vFILE *fp,
				 Int32 offset, Logical zVar, ...)
#else
STATICforIDL CDFstatus WriteVDR (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, zVar);
#else
  struct CDFstruct *CDF; vFILE *fp; Int32 offset; Logical zVar;
  VA_START (ap);
  CDF = va_arg (ap, struct CDFstruct *);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, Int32);
  zVar = va_arg (ap, Logical);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case VDR_NULL:
	 va_end (ap);
	 return pStatus;
       case VDR_RECORD: {
	 struct VDRstruct *VDR = va_arg (ap, struct VDRstruct *);
	 void *padValue = va_arg (ap, void *);
	 Int32 nDims;
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,&(VDR->RecordSize))) return CWE;
	 if (!Write32(fp,&(VDR->RecordType))) return CWE;
	 if (!Write32(fp,&(VDR->VDRnext))) return CWE;
	 if (!Write32(fp,&(VDR->DataType))) return CWE;
	 if (!Write32(fp,&(VDR->MaxRec))) return CWE;
	 if (!Write32(fp,&(VDR->VXRhead))) return CWE;
	 if (!Write32(fp,&(VDR->VXRtail))) return CWE;
	 if (!Write32(fp,&(VDR->Flags))) return CWE;
	 if (!Write32(fp,&(VDR->sRecords))) return CWE;
	 if (!Write32(fp,&(VDR->rfuB))) return CWE;
	 if (!Write32(fp,&(VDR->rfuC))) return CWE;
	 if (!Write32(fp,&(VDR->rfuF))) return CWE;
	 if (!Write32(fp,&(VDR->NumElems))) return CWE;
	 if (!Write32(fp,&(VDR->Num))) return CWE;
	 if (!Write32(fp,&(VDR->CPRorSPRoffset))) return CWE;
	 if (!Write32(fp,&(VDR->blockingFactor))) return CWE;
	 if (!WRITEv(VDR->Name,CDF_VAR_NAME_LEN,1,fp)) return CWE;
	 if (zVar) {
	   if (!Write32(fp,&(VDR->zNumDims))) return CWE;
	   if (!Write32s(fp,VDR->zDimSizes,
			 (int)VDR->zNumDims)) return CWE;
	 }
	 if (zVar)
	   nDims = VDR->zNumDims;
	 else {
	   long tOffset = V_tell (fp); Int32 GDRoffset;
	   if (!sX(ReadCDR(fp,V2_CDR_OFFSET,
			   CDR_GDROFFSET,&GDRoffset,
			   CDR_NULL),&pStatus)) return pStatus;
	   if (!sX(ReadGDR(fp,GDRoffset,
			   GDR_rNUMDIMS,&nDims,
			   GDR_NULL),&pStatus)) return pStatus;
	   if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 }
	 if (!Write32s(fp,VDR->DimVarys,(int)nDims)) return CWE;
	 if (PADvalueBITset(VDR->Flags) && padValue != NULL) {
	   if (STRINGdataType(VDR->DataType)) {
	     size_t len; char *newPad = NULL;
	     len = strlen ((char *)padValue);
	     if ((int)len < (int) VDR->NumElems) {
	       int ix;
	       newPad = (char *) cdf_AllocateMemory ((size_t) VDR->NumElems,
						     NULL);
	       memcpy (newPad, (char *)padValue, len);
	       for (ix = (int) len; ix < (int) VDR->NumElems; ++ix)
		 *(newPad+ix) = (char) ' ';
	     }
	     if (!sX(WriteBuffer(CDF,fp,VDR->DataType,VDR->NumElems,
				 (newPad==NULL?padValue:(void *)newPad)),
		     &pStatus))
	       return pStatus;
	     if (newPad != NULL) cdf_FreeMemory (newPad, NULL);
	   } else
	     if (!sX(WriteBuffer(CDF,fp,VDR->DataType,
			         VDR->NumElems,
			         padValue),&pStatus)) return pStatus;
	 }
	 break;
       }
       case VDR_NAME: {
	 char *vName = va_arg (ap, char *);
	 long tOffset = offset + VDR_NAME_OFFSET;
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!WRITEv(vName,CDF_VAR_NAME_LEN,1,fp)) return CWE;
	 break;
       }
       case VDR_zNUMDIMS: {
	 Int32 *numDims = va_arg (ap, Int32 *);
	 long tOffset = offset + zVDR_zNUMDIMS_OFFSET;
	 if (!zVar) return CDF_INTERNAL_ERROR;
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,numDims)) return CWE;
	 break;
       }
       case VDR_zDIMSIZES: {
	 Int32 *zDimSizes = va_arg (ap, Int32 *), zNumDims;
	 int dimN; long tOffset;
	 if (!zVar) return CDF_INTERNAL_ERROR;
	 if (!sX(ReadVDR(CDF,fp,offset,zVar,
			 VDR_zNUMDIMS,&zNumDims,
			 VDR_NULL),&pStatus)) return pStatus;
	 tOffset = offset + zVDR_zDIMSIZES_OFFSET;
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 for (dimN = 0; dimN < zNumDims; dimN++) {
	    if (!Write32(fp,&(zDimSizes[dimN]))) return CWE;
	 }
	 break;
       }
       case VDR_DIMVARYS: {
	 Int32 *dimVarys = va_arg (ap, Int32 *), nDims; long tOffset;
	 if (zVar) {
	   if (!sX(ReadVDR(CDF,fp,offset,zVar,
			   VDR_zNUMDIMS,&nDims,
			   VDR_NULL),&pStatus)) return pStatus;
	   tOffset = offset + zVDR_DIMVARYS_OFFSETb + (nDims * sizeof(Int32));
	 }
	 else {
	   Int32 GDRoffset;
	   if (!sX(ReadCDR(fp,V2_CDR_OFFSET,
			   CDR_GDROFFSET,&GDRoffset,
			   CDR_NULL),&pStatus)) return pStatus;
	   if (!sX(ReadGDR(fp,GDRoffset,
			   GDR_rNUMDIMS,&nDims,
			   GDR_NULL),&pStatus)) return pStatus;
	   tOffset = offset + rVDR_DIMVARYS_OFFSET;
	 }
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32s(fp,dimVarys,(int)nDims)) return CWE;
	 break;
       }
       case VDR_PADVALUE: {
	 void *padValue = va_arg (ap, void *);
	 Int32 dataType, numElems; long tOffset;
	 if (!sX(ReadVDR(CDF,fp,offset,zVar,
			 VDR_DATATYPE,&dataType,
			 VDR_NUMELEMS,&numElems,
			 VDR_NULL),&pStatus)) return pStatus;
	 if (zVar) {
	   Int32 zNumDims;
	   if (!sX(ReadVDR(CDF,fp,offset,zVar,
			   VDR_zNUMDIMS,&zNumDims,
			   VDR_NULL),&pStatus)) return pStatus;
	   tOffset = offset + zVDR_PADVALUE_OFFSETb +
		     (zNumDims * sizeof(Int32)) + (zNumDims * sizeof(Int32));
	 }
	 else {
	   Int32 rNumDims, GDRoffset;
	   if (!sX(ReadCDR(fp,V2_CDR_OFFSET,
			   CDR_GDROFFSET,&GDRoffset,
			   CDR_NULL),&pStatus)) return pStatus;
	   if (!sX(ReadGDR(fp,GDRoffset,
			   GDR_rNUMDIMS,&rNumDims,
			   GDR_NULL),&pStatus)) return pStatus;
	   tOffset = offset + rVDR_PADVALUE_OFFSETb + (rNumDims*sizeof(Int32));
	 }
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (STRINGdataType(dataType)) {
	   size_t len; char *newPad = NULL;
	   len = strlen ((char *)padValue);
	   if ((int)len < (int) numElems) {
	     int ix;
	     newPad = (char *) cdf_AllocateMemory ((size_t) numElems, NULL);
	     memcpy (newPad, (char *)padValue, len);
	     for (ix = (int) len; ix < (int) numElems; ++ix)
	        *(newPad+ix) = (char) ' ';
	   }
	   if (!sX(WriteBuffer(CDF,fp,dataType,numElems,
			       (newPad==NULL?padValue:(void *)newPad)),
		   &pStatus))
	     return pStatus;
	   if (newPad != NULL) cdf_FreeMemory (newPad, NULL);
	 } else
	   if (!sX(WriteBuffer(CDF,fp,dataType,
			       numElems,padValue),&pStatus)) return pStatus;
	 break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); long tOffset = offset;
	 switch (field) {
	   case VDR_RECORDSIZE: tOffset += VDR_RECORDSIZE_OFFSET; break;
	   case VDR_RECORDTYPE: tOffset += VDR_RECORDTYPE_OFFSET; break;
	   case VDR_VDRNEXT: tOffset += VDR_VDRNEXT_OFFSET; break;
	   case VDR_DATATYPE: tOffset += VDR_DATATYPE_OFFSET; break;
	   case VDR_MAXREC: tOffset += VDR_MAXREC_OFFSET; break;
	   case VDR_VXRHEAD: tOffset += VDR_VXRHEAD_OFFSET; break;
	   case VDR_VXRTAIL: tOffset += VDR_VXRTAIL_OFFSET; break;
	   case VDR_FLAGS: tOffset += VDR_FLAGS_OFFSET; break;
	   case VDR_sRECORDS: tOffset += VDR_sRECORDS_OFFSET; break;
	   case VDR_NUMELEMS: tOffset += VDR_NUMELEMS_OFFSET; break;
	   case VDR_NUM: tOffset += VDR_NUM_OFFSET; break;
	   case VDR_CPRorSPR: tOffset += VDR_CPRorSPR_OFFSET; break;
	   case VDR_BLOCKING: tOffset += VDR_BLOCKING_OFFSET; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriteVXR.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteVXR (vFILE *fp, Int32 offset, ...)
#else
STATICforIDL CDFstatus WriteVXR (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; Int32 offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, Int32);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case VXR_NULL:
	 va_end (ap);
	 return pStatus;
       case VXR_RECORD: {
	 struct VXRstruct *VXR = va_arg (ap, struct VXRstruct *);
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,&(VXR->RecordSize))) return CWE;
	 if (!Write32(fp,&(VXR->RecordType))) return CWE;
	 if (!Write32(fp,&(VXR->VXRnext))) return CWE;
	 if (!Write32(fp,&(VXR->Nentries))) return CWE;
	 if (!Write32(fp,&(VXR->NusedEntries))) return CWE;
	 if (!Write32s(fp,VXR->First,(int)VXR->Nentries)) return CWE;
	 if (!Write32s(fp,VXR->Last,(int)VXR->Nentries)) return CWE;
	 if (!Write32s(fp,VXR->Offset,(int)VXR->Nentries)) return CWE;
	 break;
       }
       case VXR_FIRSTREC:
       case VXR_LASTREC:
       case VXR_OFFSET: {
	 Int32 *buffer = va_arg (ap, Int32 *), nEntries;
	 long tOffset = offset + VXR_FIRSTREC_OFFSET;
	 if (!sX(ReadVXR(fp,offset,
			 VXR_NENTRIES,&nEntries,
			 VXR_NULL),&pStatus)) return pStatus;
	 switch (field) {
	   case VXR_FIRSTREC: break;
	   case VXR_LASTREC: tOffset += nEntries * sizeof(Int32); break;
	   case VXR_OFFSET: tOffset += 2 * nEntries * sizeof(Int32); break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32s(fp,buffer,(int)nEntries)) return CWE;
	 break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); long tOffset = offset;
	 switch (field) {
	   case VXR_RECORDSIZE: tOffset += VXR_RECORDSIZE_OFFSET; break;
	   case VXR_RECORDTYPE: tOffset += VXR_RECORDTYPE_OFFSET; break;
	   case VXR_VXRNEXT: tOffset += VXR_VXRNEXT_OFFSET; break;
	   case VXR_NENTRIES: tOffset += VXR_NENTRIES_OFFSET; break;
	   case VXR_NUSEDENTRIES: tOffset += VXR_NUSEDENTRIES_OFFSET; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriteVVR.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteVVR (vFILE *fp, Int32 offset, ...)
#else
STATICforIDL CDFstatus WriteVVR (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; Int32 offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, Int32);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case VVR_NULL:
	 va_end (ap);
	 return pStatus;
       case VVR_RECORDx: {
	 struct VVRstruct *VVR = va_arg (ap, struct VVRstruct *);
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,&(VVR->RecordSize))) return CWE;
	 if (!Write32(fp,&(VVR->RecordType))) return CWE;
	 break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); long tOffset = offset;
	 switch (field) {
	   case VVR_RECORDSIZE: tOffset += VVR_RECORDSIZE_OFFSET; break;
	   case VVR_RECORDTYPE: tOffset += VVR_RECORDTYPE_OFFSET; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriteUIR.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteUIR (vFILE *fp, Int32 offset, ...)
#else
STATICforIDL CDFstatus WriteUIR (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; Int32 offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, Int32);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case UIR_NULL:
	 va_end (ap);
	 return pStatus;
       case UIR_RECORD: {
	 struct UIRstruct *UIR = va_arg (ap, struct UIRstruct *);
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,&(UIR->RecordSize))) return CWE;
	 if (!Write32(fp,&(UIR->RecordType))) return CWE;
	 if (!Write32(fp,&(UIR->NextUIR))) return CWE;
	 if (!Write32(fp,&(UIR->PrevUIR))) return CWE;
	 break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); long tOffset = offset;
	 switch (field) {
	   case UIR_RECORDSIZE: tOffset += UIR_RECORDSIZE_OFFSET; break;
	   case UIR_RECORDTYPE: tOffset += UIR_RECORDTYPE_OFFSET; break;
	   case UIR_NEXTUIR: tOffset += UIR_NEXTUIR_OFFSET; break;
	   case UIR_PREVUIR: tOffset += UIR_PREVUIR_OFFSET; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriteCCR.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteCCR (vFILE *fp, Int32 offset, ...)
#else
STATICforIDL CDFstatus WriteCCR (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; Int32 offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, Int32);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case CCR_NULL:
	 va_end (ap);
	 return pStatus;
       case CCR_RECORD: {
	 struct CCRstruct *CCR = va_arg (ap, struct CCRstruct *);
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,&(CCR->RecordSize))) return CWE;
	 if (!Write32(fp,&(CCR->RecordType))) return CWE;
	 if (!Write32(fp,&(CCR->CPRoffset))) return CWE;
	 if (!Write32(fp,&(CCR->uSize))) return CWE;
	 if (!Write32(fp,&(CCR->rfuA))) return CWE;
	 break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); long tOffset = offset;
	 switch (field) {
	   case CCR_RECORDSIZE: tOffset += CCR_RECORDSIZE_OFFSET; break;
	   case CCR_RECORDTYPE: tOffset += CCR_RECORDTYPE_OFFSET; break;
	   case CCR_CPROFFSET: tOffset += CCR_CPROFFSET_OFFSET; break;
	   case CCR_USIZE: tOffset += CCR_USIZE_OFFSET; break;
	   case CCR_RFUa: tOffset += CCR_RFUa_OFFSET; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriteCPR.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteCPR (vFILE *fp, Int32 offset, ...)
#else
STATICforIDL CDFstatus WriteCPR (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; Int32 offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, Int32);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case CPR_NULL:
	 va_end (ap);
	 return pStatus;
       case CPR_RECORD: {
	 struct CPRstruct *CPR = va_arg (ap, struct CPRstruct *); int i;
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,&(CPR->RecordSize))) return CWE;
	 if (!Write32(fp,&(CPR->RecordType))) return CWE;
	 if (!Write32(fp,&(CPR->cType))) return CWE;
	 if (!Write32(fp,&(CPR->rfuA))) return CWE;
	 if (!Write32(fp,&(CPR->pCount))) return CWE;
	 for (i = 0; i < CPR->pCount; i++) {
	    if (!Write32(fp,&(CPR->cParms[i]))) return CWE;
	 }
	 break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); long tOffset = offset;
	 switch (field) {
	   case CPR_RECORDSIZE: tOffset += CPR_RECORDSIZE_OFFSET; break;
	   case CPR_RECORDTYPE: tOffset += CPR_RECORDTYPE_OFFSET; break;
	   case CPR_CTYPE: tOffset += CPR_CTYPE_OFFSET; break;
	   case CPR_RFUa: tOffset += CPR_RFUa_OFFSET; break;
	   case CPR_PCOUNT: tOffset += CPR_PCOUNT_OFFSET; break;
	   case CPR_CPARM1: tOffset += CPR_CPARM1_OFFSET; break;
	   case CPR_CPARM2: tOffset += CPR_CPARM2_OFFSET; break;
	   case CPR_CPARM3: tOffset += CPR_CPARM3_OFFSET; break;
	   case CPR_CPARM4: tOffset += CPR_CPARM4_OFFSET; break;
	   case CPR_CPARM5: tOffset += CPR_CPARM5_OFFSET; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriteSPR.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteSPR (vFILE *fp, Int32 offset, ...)
#else
STATICforIDL CDFstatus WriteSPR (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; Int32 offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, Int32);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case SPR_NULL:
	 va_end (ap);
	 return pStatus;
       case SPR_RECORD: {
	 struct SPRstruct *SPR = va_arg (ap, struct SPRstruct *); int i;
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,&(SPR->RecordSize))) return CWE;
	 if (!Write32(fp,&(SPR->RecordType))) return CWE;
	 if (!Write32(fp,&(SPR->sArraysType))) return CWE;
	 if (!Write32(fp,&(SPR->rfuA))) return CWE;
	 if (!Write32(fp,&(SPR->pCount))) return CWE;
	 for (i = 0; i < SPR->pCount; i++) {
	    if (!Write32(fp,&(SPR->sArraysParms[i]))) return CWE;
	 }
	 break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); long tOffset = offset;
	 switch (field) {
	   case SPR_RECORDSIZE: tOffset += SPR_RECORDSIZE_OFFSET; break;
	   case SPR_RECORDTYPE: tOffset += SPR_RECORDTYPE_OFFSET; break;
	   case SPR_STYPE: tOffset += SPR_STYPE_OFFSET; break;
	   case SPR_RFUa: tOffset += SPR_RFUa_OFFSET; break;
	   case SPR_PCOUNT: tOffset += SPR_PCOUNT_OFFSET; break;
	   case SPR_SPARM1: tOffset += SPR_SPARM1_OFFSET; break;
	   case SPR_SPARM2: tOffset += SPR_SPARM2_OFFSET; break;
	   case SPR_SPARM3: tOffset += SPR_SPARM3_OFFSET; break;
	   case SPR_SPARM4: tOffset += SPR_SPARM4_OFFSET; break;
	   case SPR_SPARM5: tOffset += SPR_SPARM5_OFFSET; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriteCVVR.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteCVVR (vFILE *fp, Int32 offset, ...)
#else
STATICforIDL CDFstatus WriteCVVR (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; Int32 offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, Int32);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case CVVR_NULL:
	 va_end (ap);
	 return pStatus;
       case CVVR_RECORDx: {
	 struct CVVRstruct *CVVR = va_arg (ap, struct CVVRstruct *);
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,&(CVVR->RecordSize))) return CWE;
	 if (!Write32(fp,&(CVVR->RecordType))) return CWE;
	 if (!Write32(fp,&(CVVR->rfuA))) return CWE;
	 if (!Write32(fp,&(CVVR->cSize))) return CWE;
	 break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); long tOffset = offset;
	 switch (field) {
	   case CVVR_RECORDSIZE: tOffset += CVVR_RECORDSIZE_OFFSET; break;
	   case CVVR_RECORDTYPE: tOffset += CVVR_RECORDTYPE_OFFSET; break;
	   case CVVR_RFUa: tOffset += CVVR_RFUa_OFFSET; break;
	   case CVVR_CSIZE: tOffset += CVVR_CSIZE_OFFSET; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}
