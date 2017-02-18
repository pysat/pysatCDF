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
*   V2.0  29-Jun-04, M Liu      Added LFS (Large File Support > 2G).
*   V3.2a 11-Apr-08, M Liu      Modified Write32s_64 and Write64s_64 to
*                               eliminate the potential buffer overflow.
*
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"

/******************************************************************************
* Local macro definitions.
******************************************************************************/

#define CWE CDF_WRITE_ERROR

/******************************************************************************
* Write32_64.
******************************************************************************/

STATICforIDL Logical Write32_64 (fp, value)
vFILE *fp;
Int32 *value;
{
#if defined(NETWORKbyteORDERcpu)
  if (!WRITEv64(value,(size_t)4,(size_t)1,fp)) return FALSE;
#else
  Int32 tValue;
  REVERSE4bIO (value, &tValue)
  if (!WRITEv64(&tValue,(size_t)4,(size_t)1,fp)) return FALSE;
#endif
  return TRUE;
} 

/******************************************************************************
* Write32s_64.
******************************************************************************/

STATICforIDL Logical Write32s_64 (fp, buffer, count)
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
  if (!WRITEv64(buffer,(size_t)4,(size_t)count,fp)) return FALSE;
#else
  Int32 tBuffer[MAX_tBUFFER_SIZE]; int i;
  if (count == 0) return TRUE;
  if (count < 0 || count > MAX_tBUFFER_SIZE) return FALSE;
  for (i = 0; i < count; i++) {
     REVERSE4bIO (&buffer[i], &tBuffer[i])
  }
  if (!WRITEv64(tBuffer,(size_t)4,(size_t)count,fp)) return FALSE;
#endif
  return TRUE;
}

/******************************************************************************
* Write64_64.
******************************************************************************/

STATICforIDL Logical Write64_64 (fp, value)
vFILE *fp;
OFF_T *value;
{
#if defined(NETWORKbyteORDERcpu)
  if (!WRITEv64(value,(size_t)8,(size_t)1,fp)) return FALSE;
#else
  OFF_T tValue;
  REVERSE8bIO (value, &tValue)
  if (!WRITEv64(&tValue,(size_t)8,(size_t)1,fp)) return FALSE;
#endif
  return TRUE;
}

/******************************************************************************
* Write64s_64.
******************************************************************************/

STATICforIDL Logical Write64s_64 (fp, buffer, count)
vFILE *fp;
OFF_T *buffer;
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
  if (!WRITEv64(buffer,(size_t)8,(size_t)count,fp)) return FALSE;
#else
  OFF_T tBuffer[MAX_tBUFFER_SIZE]; int i;
  if (count == 0) return TRUE;
  if (count < 0 || count > MAX_tBUFFER_SIZE) return FALSE;
  for (i = 0; i < count; i++) {
     REVERSE8bIO (&buffer[i], &tBuffer[i])
  }
  if (!WRITEv64(tBuffer,(size_t)8,(size_t)count,fp)) return FALSE;
#endif
  return TRUE;
}

/******************************************************************************
* WriteIrSize64.
*   The size is always in the first 8-byte field.
******************************************************************************/

STATICforIDL CDFstatus WriteIrSize64 (fp, offset, irSize)
vFILE *fp;
OFF_T offset;
OFF_T *irSize;
{
  if (!SEEKv64(fp,offset,vSEEK_SET)) return CWE;
  if (!Write64_64(fp,irSize)) return CWE;
  return CDF_OK;
}  

/******************************************************************************
* WriteIrType64.
*   The type is always in the second field (4-byte).
******************************************************************************/

STATICforIDL CDFstatus WriteIrType64 (fp, offset, irType)
vFILE *fp;
OFF_T offset;
Int32 *irType;
{
  OFF_T irTypeOffset = offset + sizeof(OFF_T);
  if (!SEEKv64(fp,irTypeOffset,vSEEK_SET)) return CWE;
  if (!Write32_64(fp,irType)) return CWE;
  return CDF_OK;
}  

/******************************************************************************
* WriteCDR64.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteCDR64 (vFILE *fp, OFF_T offset, ...)
#else
STATICforIDL CDFstatus WriteCDR64 (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; OFF_T offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, OFF_T);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case CDR_NULL:
	 va_end (ap);
	 return pStatus;
       case CDR_RECORD: {
	 struct CDRstruct64 *CDR = va_arg (ap, struct CDRstruct64 *);
	 char *copyRight = va_arg (ap, char *);
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CWE;
	 if (!Write64_64(fp,&(CDR->RecordSize))) return CWE;
	 if (!Write32_64(fp,&(CDR->RecordType))) return CWE;
	 if (!Write64_64(fp,&(CDR->GDRoffset))) return CWE;
         if (!Write32s_64(fp,&(CDR->Version),9)) return CWE;
/*
	 if (!Write32_64(fp,&(CDR->Version))) return CWE;
	 if (!Write32_64(fp,&(CDR->Release))) return CWE;
	 if (!Write32_64(fp,&(CDR->Encoding))) return CWE;
	 if (!Write32_64(fp,&(CDR->Flags))) return CWE;
	 if (!Write32_64(fp,&(CDR->rfuA))) return CWE;
	 if (!Write32_64(fp,&(CDR->rfuB))) return CWE;
	 if (!Write32_64(fp,&(CDR->Increment))) return CWE;
	 if (!Write32_64(fp,&(CDR->rfuD))) return CWE;
	 if (!Write32_64(fp,&(CDR->rfuE))) return CWE;
*/
	 if (copyRight != NULL) {
	   if (!WRITEv64(copyRight,CDF_COPYRIGHT_LEN,1,fp)) return CWE;
	 }
	 break;
       }
       case CDR_COPYRIGHT: {
	 char *copyRight = va_arg (ap, char *);
	 OFF_T tOffset = offset + CDR_COPYRIGHT_OFFSET64;
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!WRITEv64(copyRight,CDF_COPYRIGHT_LEN,1,fp)) return CWE;
	 break;
       }
       case CDR_RECORDSIZE:
       case CDR_GDROFFSET: {
         OFF_T *buffer = va_arg (ap, OFF_T *); OFF_T tOffset = offset;
	 switch (field) {
	   case CDR_RECORDSIZE: tOffset += (OFF_T) CDR_RECORDSIZE_OFFSET64; break;
	   case CDR_GDROFFSET: tOffset += (OFF_T) CDR_GDROFFSET_OFFSET64; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write64_64(fp,buffer)) return CWE;
	 break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
	 switch (field) {
/*	   case CDR_RECORDSIZE: tOffset += (OFF_T) CDR_RECORDSIZE_OFFSET64; break; */
	   case CDR_RECORDTYPE: tOffset += (OFF_T) CDR_RECORDTYPE_OFFSET64; break;
/*	   case CDR_GDROFFSET: tOffset += (OFF_T) CDR_GDROFFSET_OFFSET64; break; */
	   case CDR_VERSION: tOffset += (OFF_T) CDR_VERSION_OFFSET64; break;
	   case CDR_RELEASE: tOffset += (OFF_T) CDR_RELEASE_OFFSET64; break;
	   case CDR_ENCODING: tOffset += (OFF_T) CDR_ENCODING_OFFSET64; break;
	   case CDR_FLAGS: tOffset += (OFF_T) CDR_FLAGS_OFFSET64; break;
	   case CDR_INCREMENT: tOffset += (OFF_T) CDR_INCREMENT_OFFSET64; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32_64(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriteGDR64.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteGDR64 (vFILE *fp, OFF_T offset, ...)
#else
STATICforIDL CDFstatus WriteGDR64 (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; OFF_T offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, OFF_T);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case GDR_NULL:
	 va_end (ap);
	 return pStatus;
       case GDR_RECORD: {
	 struct GDRstruct64 *GDR = va_arg (ap, struct GDRstruct64 *);
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CWE;
	 if (!Write64_64(fp,&(GDR->RecordSize))) return CWE;
	 if (!Write32_64(fp,&(GDR->RecordType))) return CWE;
         if (!Write64s_64(fp,&(GDR->rVDRhead),4)) return CWE;
/*
	 if (!Write64_64(fp,&(GDR->rVDRhead))) return CWE;
	 if (!Write64_64(fp,&(GDR->zVDRhead))) return CWE;
	 if (!Write64_64(fp,&(GDR->ADRhead))) return CWE;
	 if (!Write64_64(fp,&(GDR->eof))) return CWE;
*/
         if (!Write32s_64(fp,&(GDR->NrVars),5)) return CWE;
/*
	 if (!Write32_64(fp,&(GDR->NrVars))) return CWE;
	 if (!Write32_64(fp,&(GDR->NumAttr))) return CWE;
	 if (!Write32_64(fp,&(GDR->rMaxRec))) return CWE;
	 if (!Write32_64(fp,&(GDR->rNumDims))) return CWE;
	 if (!Write32_64(fp,&(GDR->NzVars))) return CWE;
*/
	 if (!Write64_64(fp,&(GDR->UIRhead))) return CWE;
         if (!Write32s_64(fp,&(GDR->rfuC),3)) return CWE;
/*
	 if (!Write32_64(fp,&(GDR->rfuC))) return CWE;
	 if (!Write32_64(fp,&(GDR->LeapSecondLastUpdated))) return CWE;
	 if (!Write32_64(fp,&(GDR->rfuE))) return CWE;
*/
	 if (!Write32s_64(fp,GDR->rDimSizes,(int)GDR->rNumDims)) return CWE;
	 break;
       }
       case GDR_rDIMSIZES: {
	 Int32 *rDimSizes = va_arg (ap, Int32 *), rNumDims; OFF_T tOffset;
	 if (!sX(ReadGDR64(fp,offset,
			   GDR_rNUMDIMS,&rNumDims,
			   GDR_NULL),&pStatus)) return pStatus;
	 tOffset = offset + GDR_rDIMSIZES_OFFSET64;
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32s_64(fp,rDimSizes,(int)rNumDims)) return CWE;
	 break;
       }
       case GDR_RECORDSIZE:
       case GDR_rVDRHEAD:
       case GDR_zVDRHEAD:
       case GDR_ADRHEAD:
       case GDR_EOF:
       case GDR_UIRHEAD: {
         OFF_T *buffer = va_arg (ap, OFF_T *); OFF_T tOffset = offset;
         switch (field) {
           case GDR_RECORDSIZE: tOffset += (OFF_T) GDR_RECORDSIZE_OFFSET64; break;
           case GDR_rVDRHEAD: tOffset += (OFF_T) GDR_rVDRHEAD_OFFSET64; break;
           case GDR_zVDRHEAD: tOffset += (OFF_T) GDR_zVDRHEAD_OFFSET64; break;
           case GDR_ADRHEAD: tOffset += (OFF_T) GDR_ADRHEAD_OFFSET64; break;
           case GDR_EOF: tOffset += (OFF_T) GDR_EOF_OFFSET64; break;
           case GDR_UIRHEAD: tOffset += (OFF_T) GDR_UIRHEAD_OFFSET64; break;
           default: return CDF_INTERNAL_ERROR;
         }
         if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
         if (!Write64_64(fp,buffer)) return CWE;
         break;  
       } 
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
	 switch (field) {
/*	   case GDR_RECORDSIZE: tOffset += (OFF_T) GDR_RECORDSIZE_OFFSET64; break; */
	   case GDR_RECORDTYPE: tOffset += (OFF_T) GDR_RECORDTYPE_OFFSET64; break;
/*	   case GDR_rVDRHEAD: tOffset += (OFF_T) GDR_rVDRHEAD_OFFSET64; break; */
/*	   case GDR_zVDRHEAD: tOffset += (OFF_T) GDR_zVDRHEAD_OFFSET64; break; */
/*	   case GDR_ADRHEAD: tOffset += (OFF_T) GDR_ADRHEAD_OFFSET64; break; */
/*	   case GDR_EOF: tOffset += (OFF_T) GDR_EOF_OFFSET64; break; */
	   case GDR_NrVARS: tOffset += (OFF_T) GDR_NrVARS_OFFSET64; break;
	   case GDR_NUMATTR: tOffset += (OFF_T) GDR_NUMATTR_OFFSET64; break;
	   case GDR_rMAXREC: tOffset += (OFF_T) GDR_rMAXREC_OFFSET64; break;
	   case GDR_rNUMDIMS: tOffset += (OFF_T) GDR_rNUMDIMS_OFFSET64; break;
	   case GDR_NzVARS: tOffset += (OFF_T) GDR_NzVARS_OFFSET64; break;
	   case GDR_LEAPSECONDLASTUPDATED: tOffset += (OFF_T) GDR_LEAPSECONDLASTUPDATED_OFFSET64; break;
/*	   case GDR_UIRHEAD: tOffset += (OFF_T) GDR_UIRHEAD_OFFSET64; break; */
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32_64(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriteADR64.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteADR64 (vFILE *fp, OFF_T offset, ...)
#else
STATICforIDL CDFstatus WriteADR64 (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; OFF_T offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, OFF_T);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case ADR_NULL:
	 va_end (ap);
	 return pStatus;
       case ADR_RECORD: {
	 struct ADRstruct64 *ADR = va_arg (ap, struct ADRstruct64 *);
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CWE;
	 if (!Write64_64(fp,&(ADR->RecordSize))) return CWE;
	 if (!Write32_64(fp,&(ADR->RecordType))) return CWE;
         if (!Write64s_64(fp,&(ADR->ADRnext),2)) return CWE;
/*
	 if (!Write64_64(fp,&(ADR->ADRnext))) return CWE;
	 if (!Write64_64(fp,&(ADR->AgrEDRhead))) return CWE;
*/
         if (!Write32s_64(fp,&(ADR->Scope),5)) return CWE;
/*
	 if (!Write32_64(fp,&(ADR->Scope))) return CWE;
	 if (!Write32_64(fp,&(ADR->Num))) return CWE;
	 if (!Write32_64(fp,&(ADR->NgrEntries))) return CWE;
	 if (!Write32_64(fp,&(ADR->MAXgrEntry))) return CWE;
	 if (!Write32_64(fp,&(ADR->rfuA))) return CWE;
*/
	 if (!Write64_64(fp,&(ADR->AzEDRhead))) return CWE;
         if (!Write32s_64(fp,&(ADR->NzEntries),3)) return CWE;
/*
	 if (!Write32_64(fp,&(ADR->NzEntries))) return CWE;
	 if (!Write32_64(fp,&(ADR->MAXzEntry))) return CWE;
	 if (!Write32_64(fp,&(ADR->rfuE))) return CWE;
*/
	 if (!WRITEv64(ADR->Name,CDF_ATTR_NAME_LEN256,1,fp)) return CWE;
	 break;
       }
       case ADR_NAME: {
	 char *aName = va_arg (ap, char *);
	 OFF_T tOffset = offset + ADR_NAME_OFFSET64;
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!WRITEv64(aName,CDF_ATTR_NAME_LEN256,1,fp)) return CWE;
	 break;
       }
       case ADR_RECORDSIZE:
       case ADR_ADRNEXT:
       case ADR_AgrEDRHEAD:
       case ADR_AzEDRHEAD: {
         OFF_T *buffer = va_arg (ap, OFF_T *); OFF_T tOffset = offset;
         switch (field) {
           case ADR_RECORDSIZE: tOffset += (OFF_T) ADR_RECORDSIZE_OFFSET64; break;
           case ADR_ADRNEXT: tOffset += (OFF_T) ADR_ADRNEXT_OFFSET64; break;
           case ADR_AgrEDRHEAD: tOffset += (OFF_T) ADR_AgrEDRHEAD_OFFSET64; break;
           case ADR_AzEDRHEAD: tOffset += (OFF_T) ADR_AzEDRHEAD_OFFSET64; break;
           default: return CDF_INTERNAL_ERROR;
         }
         if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
         if (!Write64_64(fp,buffer)) return CWE;
         break;
       } 
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
	 switch (field) {
/*	   case ADR_RECORDSIZE: tOffset += (OFF_T) ADR_RECORDSIZE_OFFSET64; break; */
	   case ADR_RECORDTYPE: tOffset += (OFF_T) ADR_RECORDTYPE_OFFSET64; break;
/*	   case ADR_ADRNEXT: tOffset += (OFF_T) ADR_ADRNEXT_OFFSET64; break; */
/*	   case ADR_AgrEDRHEAD: tOffset += (OFF_T) ADR_AgrEDRHEAD_OFFSET64; break; */
	   case ADR_SCOPE: tOffset += (OFF_T) ADR_SCOPE_OFFSET64; break;
	   case ADR_NUM: tOffset += (OFF_T) ADR_NUM_OFFSET64; break;
	   case ADR_NgrENTRIES: tOffset += (OFF_T) ADR_NgrENTRIES_OFFSET64; break;
	   case ADR_MAXgrENTRY: tOffset += (OFF_T) ADR_MAXgrENTRY_OFFSET64; break;
/*	   case ADR_AzEDRHEAD: tOffset += (OFF_T) ADR_AzEDRHEAD_OFFSET64; break; */
	   case ADR_NzENTRIES: tOffset += (OFF_T) ADR_NzENTRIES_OFFSET64; break;
	   case ADR_MAXzENTRY: tOffset += (OFF_T) ADR_MAXzENTRY_OFFSET64; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32_64(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriteAgrEDR64/AzEDR64.
*    If the entry value is being written, it is assumed that the value passed
* in is in the host machine's encoding.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteAEDR64 (struct CDFstruct *CDF, vFILE *fp,
				    OFF_T offset, ...)
#else
STATICforIDL CDFstatus WriteAEDR64 (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  struct CDFstruct *CDF; vFILE *fp; OFF_T offset;
  VA_START (ap);
  CDF = va_arg (ap, struct CDFstruct *);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, OFF_T);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case AEDR_NULL:
	 va_end (ap);
	 return pStatus;
       case AEDR_RECORD: {
	 struct AEDRstruct64 *AEDR = va_arg (ap, struct AEDRstruct64 *);
	 void *value = va_arg (ap, void *);
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CWE;
	 if (!Write64_64(fp,&(AEDR->RecordSize))) return CWE;
	 if (!Write32_64(fp,&(AEDR->RecordType))) return CWE;
	 if (!Write64_64(fp,&(AEDR->AEDRnext))) return CWE;
         if (!Write32s_64(fp,&(AEDR->AttrNum),9)) return CWE;
/*

	 if (!Write32_64(fp,&(AEDR->AttrNum))) return CWE;
	 if (!Write32_64(fp,&(AEDR->DataType))) return CWE;
	 if (!Write32_64(fp,&(AEDR->Num))) return CWE;
	 if (!Write32_64(fp,&(AEDR->NumElems))) return CWE;
	 if (!Write32_64(fp,&(AEDR->rfuA))) return CWE;
	 if (!Write32_64(fp,&(AEDR->rfuB))) return CWE;
	 if (!Write32_64(fp,&(AEDR->rfuC))) return CWE;
	 if (!Write32_64(fp,&(AEDR->rfuD))) return CWE;
	 if (!Write32_64(fp,&(AEDR->rfuE))) return CWE;
*/
	 if (value != NULL) {
           if (STRINGdataType(AEDR->DataType)) {
             size_t len; char *newValue = NULL;
             len = strlen ((char *)value);
             if ((int)len < AEDR->NumElems) {
               int ix;
               newValue = (char *) cdf_AllocateMemory ((size_t) AEDR->NumElems,
						       NULL);
               memcpy (newValue, (char *)value, len);
               for (ix = (int) len; ix < AEDR->NumElems; ++ix)
                  *(newValue+ix) = (char) ' ';
             }
	     if (!sX(WriteBuffer64(CDF,fp,AEDR->DataType,AEDR->NumElems,
				   (newValue==NULL?value:(void *)newValue)),
		     &pStatus))
	       return pStatus;
	     if (newValue != NULL) cdf_FreeMemory (newValue, NULL);
           } else
	     if (!sX(WriteBuffer64(CDF,fp,AEDR->DataType,
				   AEDR->NumElems,value),&pStatus))
	       return pStatus;
	 }
	 break;
       }
       case AEDR_VALUE: {
	 void *value = va_arg (ap, void *);
	 Int32 dataType, numElems; OFF_T tOffset;
	 if (!sX(ReadAEDR64(fp,offset,
			    AEDR_DATATYPE,&dataType,
			    AEDR_NUMELEMS,&numElems,
			    AEDR_NULL),&pStatus)) return pStatus;
	 tOffset = offset + AEDR_VALUE_OFFSET64;
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
         if (STRINGdataType(dataType)) {
           size_t len; char *newValue = NULL;
           len = strlen ((char *)value);
           if ((int)len < (int)numElems) {
             int ix;
             newValue = (char *) cdf_AllocateMemory ((size_t) numElems, NULL);
             memcpy (newValue, (char *)value, len);
             for (ix = (int) len; ix < (int)numElems; ++ix)
                *(newValue+ix) = (char) ' ';
           }
	   if (!sX(WriteBuffer64(CDF,fp,dataType,numElems,
				 (newValue==NULL?value:(void *)newValue)),
		   &pStatus))
	     return pStatus;
	   if (newValue != NULL) cdf_FreeMemory (newValue, NULL);
         } else
	   if (!sX(WriteBuffer64(CDF,fp,dataType,
				 numElems,value),&pStatus)) return pStatus;
	 break;
       }
       case AEDR_RECORDSIZE:
       case AEDR_AEDRNEXT: {
         OFF_T *buffer = va_arg (ap, OFF_T *); OFF_T tOffset = offset;
         switch (field) {
           case AEDR_RECORDSIZE: tOffset += (OFF_T) AEDR_RECORDSIZE_OFFSET64; break;
           case AEDR_AEDRNEXT: tOffset += (OFF_T) AEDR_AEDRNEXT_OFFSET64; break;
           default: return CDF_INTERNAL_ERROR;
         }
         if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
         if (!Write64_64(fp,buffer)) return CWE;
         break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
	 switch (field) {
/*	   case AEDR_RECORDSIZE: tOffset += (OFF_T) AEDR_RECORDSIZE_OFFSET64; break; */
	   case AEDR_RECORDTYPE: tOffset += (OFF_T) AEDR_RECORDTYPE_OFFSET64; break;
/*	   case AEDR_AEDRNEXT: tOffset += (OFF_T) AEDR_AEDRNEXT_OFFSET64; break; */
	   case AEDR_ATTRNUM: tOffset += (OFF_T) AEDR_ATTRNUM_OFFSET64; break;
	   case AEDR_DATATYPE: tOffset += (OFF_T) AEDR_DATATYPE_OFFSET64; break;
	   case AEDR_NUM: tOffset += (OFF_T) AEDR_NUM_OFFSET64; break;
	   case AEDR_NUMELEMS: tOffset += (OFF_T) AEDR_NUMELEMS_OFFSET64; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32_64(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriterVDR64/zVDR64.
*    If the pad value is being written, it is assumed that the value passed
* in is in the host machine's encoding.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteVDR64 (struct CDFstruct *CDF, vFILE *fp,
				   OFF_T offset, Logical zVar, ...)
#else
STATICforIDL CDFstatus WriteVDR64 (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, zVar);
#else
  struct CDFstruct *CDF; vFILE *fp; OFF_T offset; Logical zVar;
  VA_START (ap);
  CDF = va_arg (ap, struct CDFstruct *);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, OFF_T);
  zVar = va_arg (ap, Logical);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case VDR_NULL:
	 va_end (ap);
	 return pStatus;
       case VDR_RECORD: {
	 struct VDRstruct64 *VDR = va_arg (ap, struct VDRstruct64 *);
	 void *padValue = va_arg (ap, void *);
	 Int32 nDims;
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CWE;
	 if (!Write64_64(fp,&(VDR->RecordSize))) return CWE;
	 if (!Write32_64(fp,&(VDR->RecordType))) return CWE;
	 if (!Write64_64(fp,&(VDR->VDRnext))) return CWE;
         if (!Write32s_64(fp,&(VDR->DataType),2)) return CWE;
/*
	 if (!Write32_64(fp,&(VDR->DataType))) return CWE;
	 if (!Write32_64(fp,&(VDR->MaxRec))) return CWE;
*/
         if (!Write64s_64(fp,&(VDR->VXRhead),2)) return CWE;
/*
	 if (!Write64_64(fp,&(VDR->VXRhead))) return CWE;
	 if (!Write64_64(fp,&(VDR->VXRtail))) return CWE;
*/
         if (!Write32s_64(fp,&(VDR->Flags),7)) return CWE;
/*
	 if (!Write32_64(fp,&(VDR->Flags))) return CWE;
	 if (!Write32_64(fp,&(VDR->sRecords))) return CWE;
	 if (!Write32_64(fp,&(VDR->rfuB))) return CWE;
	 if (!Write32_64(fp,&(VDR->rfuC))) return CWE;
	 if (!Write32_64(fp,&(VDR->rfuF))) return CWE;
	 if (!Write32_64(fp,&(VDR->NumElems))) return CWE;
	 if (!Write32_64(fp,&(VDR->Num))) return CWE;
*/
	 if (!Write64_64(fp,&(VDR->CPRorSPRoffset))) return CWE;
	 if (!Write32_64(fp,&(VDR->blockingFactor))) return CWE;
	 if (!WRITEv64(VDR->Name,CDF_VAR_NAME_LEN256,1,fp)) return CWE;
	 if (zVar) {
	   if (!Write32_64(fp,&(VDR->zNumDims))) return CWE;
	   if (!Write32s_64(fp,VDR->zDimSizes,
			    (int)VDR->zNumDims)) return CWE;
	 }
	 if (zVar)
	   nDims = VDR->zNumDims;
	 else {
	   OFF_T tOffset = V_tell64 (fp); OFF_T GDRoffset;
	   if (!sX(ReadCDR64(fp,V3_CDR_OFFSET64,
			     CDR_GDROFFSET,&GDRoffset,
			     CDR_NULL),&pStatus)) return pStatus;
	   if (!sX(ReadGDR64(fp,GDRoffset,
			     GDR_rNUMDIMS,&nDims,
			     GDR_NULL),&pStatus)) return pStatus;
	   if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 }
	 if (!Write32s_64(fp,VDR->DimVarys,(int)nDims)) return CWE;
	 if (PADvalueBITset(VDR->Flags) && padValue != NULL) {
	   if (STRINGdataType(VDR->DataType)) {
	     size_t len; char *newPad = NULL;
	     len = strlen ((char *)padValue);
	     if ((int)len < (int)VDR->NumElems) {
	       int ix;
	       newPad = (char *) cdf_AllocateMemory ((size_t) VDR->NumElems,
						     NULL);
	       memcpy (newPad, (char *)padValue, len);
	       for (ix = (int) len; ix < (int)VDR->NumElems; ++ix)
	          *(newPad+ix) = (char) ' ';
	     }
	     if (!sX(WriteBuffer64(CDF,fp,VDR->DataType,VDR->NumElems,
				   (newPad==NULL?padValue:(void *)newPad)),
	             &pStatus))
	       return pStatus;
	     if (newPad != NULL) cdf_FreeMemory (newPad, NULL);
	   } else
	     if (!sX(WriteBuffer64(CDF,fp,VDR->DataType,
				   VDR->NumElems,
				   padValue),&pStatus)) return pStatus;
	 }
	 break;
       }
       case VDR_NAME: {
	 char *vName = va_arg (ap, char *);
	 OFF_T tOffset = offset + VDR_NAME_OFFSET64;
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!WRITEv64(vName,CDF_VAR_NAME_LEN256,1,fp)) return CWE;
	 break;
       }
       case VDR_zNUMDIMS: {
	 Int32 *numDims = va_arg (ap, Int32 *);
	 OFF_T tOffset = offset + zVDR_zNUMDIMS_OFFSET64;
	 if (!zVar) return CDF_INTERNAL_ERROR;
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32_64(fp,numDims)) return CWE;
	 break;
       }
       case VDR_zDIMSIZES: {
	 Int32 *zDimSizes = va_arg (ap, Int32 *), zNumDims;
	 int dimN; OFF_T tOffset;
	 if (!zVar) return CDF_INTERNAL_ERROR;
	 if (!sX(ReadVDR64(CDF,fp,offset,zVar,
			   VDR_zNUMDIMS,&zNumDims,
			   VDR_NULL),&pStatus)) return pStatus;
	 tOffset = offset + zVDR_zDIMSIZES_OFFSET64;
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 for (dimN = 0; dimN < zNumDims; dimN++) {
	    if (!Write32_64(fp,&(zDimSizes[dimN]))) return CWE;
	 }
	 break;
       }
       case VDR_DIMVARYS: {
	 Int32 *dimVarys = va_arg (ap, Int32 *), nDims; OFF_T tOffset;
	 if (zVar) {
	   if (!sX(ReadVDR64(CDF,fp,offset,zVar,
			     VDR_zNUMDIMS,&nDims,
			     VDR_NULL),&pStatus)) return pStatus;
	   tOffset = offset + (OFF_T) (zVDR_DIMVARYS_OFFSETb64 + 
                                       (nDims * sizeof(Int32)));
	 }
	 else {
	   OFF_T GDRoffset;
	   if (!sX(ReadCDR64(fp,V3_CDR_OFFSET64,
			     CDR_GDROFFSET,&GDRoffset,
			     CDR_NULL),&pStatus)) return pStatus;
	   if (!sX(ReadGDR64(fp,GDRoffset,
			     GDR_rNUMDIMS,&nDims,
			     GDR_NULL),&pStatus)) return pStatus;
	   tOffset = offset + rVDR_DIMVARYS_OFFSET64;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32s_64(fp,dimVarys,(int)nDims)) return CWE;
	 break;
       }
       case VDR_PADVALUE: {
	 void *padValue = va_arg (ap, void *);
	 Int32 dataType, numElems; OFF_T tOffset;
	 if (!sX(ReadVDR64(CDF,fp,offset,zVar,
			   VDR_DATATYPE,&dataType,
			   VDR_NUMELEMS,&numElems,
			   VDR_NULL),&pStatus)) return pStatus;
	 if (zVar) {
	   Int32 zNumDims;
	   if (!sX(ReadVDR64(CDF,fp,offset,zVar,
			     VDR_zNUMDIMS,&zNumDims,
			     VDR_NULL),&pStatus)) return pStatus;
	   tOffset = offset + (OFF_T) (zVDR_PADVALUE_OFFSETb64 +
		                       (zNumDims * sizeof(Int32)) + 
                                       (zNumDims * sizeof(Int32)));
	 }
	 else {
	   Int32 rNumDims; OFF_T GDRoffset;
	   if (!sX(ReadCDR64(fp,V3_CDR_OFFSET64,
			     CDR_GDROFFSET,&GDRoffset,
			     CDR_NULL),&pStatus)) return pStatus;
	   if (!sX(ReadGDR64(fp,GDRoffset,
			     GDR_rNUMDIMS,&rNumDims,
			     GDR_NULL),&pStatus)) return pStatus;
	   tOffset = offset + (OFF_T) (rVDR_PADVALUE_OFFSETb64 + 
                                       (rNumDims*sizeof(Int32)));
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (STRINGdataType(dataType)) {
	   size_t len; char *newPad = NULL;
	   len = strlen ((char *)padValue);
	   if ((int)len < (int)numElems) {
             int ix;
	     newPad = (char *) cdf_AllocateMemory ((size_t) numElems, NULL);
	     memcpy (newPad, (char *)padValue, len);
	     for (ix = (int) len; ix < (int)numElems; ++ix)
		*(newPad+ix) = (char) ' ';
	   }
	   if (!sX(WriteBuffer64(CDF,fp,dataType,numElems,
				 (newPad==NULL?padValue:(void *)newPad)),
		   &pStatus))
	     return pStatus;
	   if (newPad != NULL) cdf_FreeMemory (newPad, NULL);
	 } else
	   if (!sX(WriteBuffer64(CDF,fp,dataType,
				 numElems,padValue),&pStatus)) return pStatus;
	 break;
       }
       case VDR_RECORDSIZE:
       case VDR_VDRNEXT: 
       case VDR_VXRHEAD:
       case VDR_VXRTAIL:
       case VDR_CPRorSPR: {
         OFF_T *buffer = va_arg (ap, OFF_T *); OFF_T tOffset = offset;
         switch (field) {
           case VDR_RECORDSIZE: tOffset += (OFF_T) VDR_RECORDSIZE_OFFSET64; break;
           case VDR_VDRNEXT: tOffset += (OFF_T) VDR_VDRNEXT_OFFSET64; break;
           case VDR_VXRHEAD: tOffset += (OFF_T) VDR_VXRHEAD_OFFSET64; break;
           case VDR_VXRTAIL: tOffset += (OFF_T) VDR_VXRTAIL_OFFSET64; break;
           case VDR_CPRorSPR: tOffset += (OFF_T) VDR_CPRorSPR_OFFSET64; break;
           default: return CDF_INTERNAL_ERROR;
         } 
         if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
         if (!Write64_64(fp,buffer)) return CWE;
         break;      
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
	 switch (field) {
/*	   case VDR_RECORDSIZE: tOffset += (OFF_T) VDR_RECORDSIZE_OFFSET64; break; */
	   case VDR_RECORDTYPE: tOffset += (OFF_T) VDR_RECORDTYPE_OFFSET64; break;
/*	   case VDR_VDRNEXT: tOffset += (OFF_T) VDR_VDRNEXT_OFFSET64; break; */
	   case VDR_DATATYPE: tOffset += (OFF_T) VDR_DATATYPE_OFFSET64; break;
	   case VDR_MAXREC: tOffset += (OFF_T) VDR_MAXREC_OFFSET64; break;
/*	   case VDR_VXRHEAD: tOffset += (OFF_T) VDR_VXRHEAD_OFFSET64; break; */
/*	   case VDR_VXRTAIL: tOffset += (OFF_T) VDR_VXRTAIL_OFFSET64; break; */
	   case VDR_FLAGS: tOffset += (OFF_T) VDR_FLAGS_OFFSET64; break;
	   case VDR_sRECORDS: tOffset += (OFF_T) VDR_sRECORDS_OFFSET64; break;
	   case VDR_NUMELEMS: tOffset += (OFF_T) VDR_NUMELEMS_OFFSET64; break;
	   case VDR_NUM: tOffset += (OFF_T) VDR_NUM_OFFSET64; break;
/*	   case VDR_CPRorSPR: tOffset += (OFF_T) VDR_CPRorSPR_OFFSET64; break; */
	   case VDR_BLOCKING: tOffset += (OFF_T) VDR_BLOCKING_OFFSET64; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32_64(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriteVXR64.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteVXR64 (vFILE *fp, OFF_T offset, ...)
#else
STATICforIDL CDFstatus WriteVXR64 (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; OFF_T offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, OFF_T);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case VXR_NULL:
	 va_end (ap);
	 return pStatus;
       case VXR_RECORD: {
	 struct VXRstruct64 *VXR = va_arg (ap, struct VXRstruct64 *);
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CWE;
	 if (!Write64_64(fp,&(VXR->RecordSize))) return CWE;
	 if (!Write32_64(fp,&(VXR->RecordType))) return CWE;
	 if (!Write64_64(fp,&(VXR->VXRnext))) return CWE;
         if (!Write32s_64(fp,&(VXR->Nentries),2)) return CWE;
/*
	 if (!Write32_64(fp,&(VXR->Nentries))) return CWE;
	 if (!Write32_64(fp,&(VXR->NusedEntries))) return CWE;
*/
	 if (!Write32s_64(fp,VXR->First,(int)VXR->Nentries)) return CWE;
         if (!Write32s_64(fp,VXR->Last,(int)VXR->Nentries)) return CWE;
	 if (!Write64s_64(fp,VXR->Offset,(int)VXR->Nentries)) return CWE;
	 break;
       }
       case VXR_FIRSTREC:
       case VXR_LASTREC: {
	 Int32 *buffer = va_arg (ap, Int32 *), nEntries;
	 OFF_T tOffset = offset + VXR_FIRSTREC_OFFSET64;
	 if (!sX(ReadVXR64(fp,offset,
			   VXR_NENTRIES,&nEntries,
			   VXR_NULL),&pStatus)) return pStatus;
	 switch (field) {
	   case VXR_FIRSTREC: break;
	   case VXR_LASTREC: tOffset += (OFF_T) nEntries * sizeof(Int32); break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32s_64(fp,buffer,(int)nEntries)) return CWE;
	 break;
       }
       case VXR_OFFSET: {
         OFF_T *buffer = va_arg (ap, OFF_T *), nEntries;
         OFF_T tOffset = offset + VXR_FIRSTREC_OFFSET64;
         if (!sX(ReadVXR64(fp,offset,
                           VXR_NENTRIES,&nEntries,
                           VXR_NULL),&pStatus)) return pStatus;
         tOffset += (OFF_T) (2 * nEntries * sizeof(Int32)); 
         if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
         if (!Write64s_64(fp,buffer,(int)nEntries)) return CWE;
         break;
       }
       case VXR_RECORDSIZE:
       case VXR_VXRNEXT: {
         OFF_T *buffer = va_arg (ap, OFF_T *); OFF_T tOffset = offset;
         switch (field) {
           case VXR_RECORDSIZE: tOffset += (OFF_T) VXR_RECORDSIZE_OFFSET64; break; 
           case VXR_VXRNEXT: tOffset += (OFF_T) VXR_VXRNEXT_OFFSET64; break; 
           default: return CDF_INTERNAL_ERROR;
         }
         if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
         if (!Write64_64(fp,buffer)) return CWE;
         break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
	 switch (field) {
/*	   case VXR_RECORDSIZE: tOffset += (OFF_T) VXR_RECORDSIZE_OFFSET64; break; */
	   case VXR_RECORDTYPE: tOffset += (OFF_T) VXR_RECORDTYPE_OFFSET64; break;
/*	   case VXR_VXRNEXT: tOffset += (OFF_T) VXR_VXRNEXT_OFFSET64; break; */
	   case VXR_NENTRIES: tOffset += (OFF_T) VXR_NENTRIES_OFFSET64; break;
	   case VXR_NUSEDENTRIES: tOffset += (OFF_T) VXR_NUSEDENTRIES_OFFSET64; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32_64(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriteVVR64.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteVVR64 (vFILE *fp, OFF_T offset, ...)
#else
STATICforIDL CDFstatus WriteVVR64 (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; OFF_T offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, OFF_T);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case VVR_NULL:
	 va_end (ap);
	 return pStatus;
       case VVR_RECORDx: {
	 struct VVRstruct64 *VVR = va_arg (ap, struct VVRstruct64 *);
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CWE;
	 if (!Write64_64(fp,&(VVR->RecordSize))) return CWE;
	 if (!Write32_64(fp,&(VVR->RecordType))) return CWE;
	 break;
       }
       case VVR_RECORDSIZE: {
         OFF_T *buffer = va_arg (ap, OFF_T *); OFF_T tOffset = offset;
         tOffset += (OFF_T) VVR_RECORDSIZE_OFFSET64;
         if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
         if (!Write64_64(fp,buffer)) return CWE;
         break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
	 switch (field) {
/*	   case VVR_RECORDSIZE: tOffset += (OFF_T) VVR_RECORDSIZE_OFFSET64; break; */
	   case VVR_RECORDTYPE: tOffset += (OFF_T) VVR_RECORDTYPE_OFFSET64; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32_64(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriteUIR64.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteUIR64 (vFILE *fp, OFF_T offset, ...)
#else
STATICforIDL CDFstatus WriteUIR64 (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; OFF_T offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, OFF_T);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case UIR_NULL:
	 va_end (ap);
	 return pStatus;
       case UIR_RECORD: {
	 struct UIRstruct64 *UIR = va_arg (ap, struct UIRstruct64 *);
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CWE;
	 if (!Write64_64(fp,&(UIR->RecordSize))) return CWE;
	 if (!Write32_64(fp,&(UIR->RecordType))) return CWE;
         if (!Write64s_64(fp,&(UIR->NextUIR),2)) return CWE;
/*
	 if (!Write64_64(fp,&(UIR->NextUIR))) return CWE;
	 if (!Write64_64(fp,&(UIR->PrevUIR))) return CWE;
*/
	 break;
       }
       case UIR_RECORDTYPE: {
         Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
         tOffset += (OFF_T) UIR_RECORDTYPE_OFFSET64;
         if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
         if (!Write32_64(fp,buffer)) return CWE;
         break;
       }
       default: {
	 OFF_T *buffer = va_arg (ap, OFF_T *); OFF_T tOffset = offset;
	 switch (field) {
	   case UIR_RECORDSIZE: tOffset += (OFF_T) UIR_RECORDSIZE_OFFSET64; break;
/*	   case UIR_RECORDTYPE: tOffset += (OFF_T) UIR_RECORDTYPE_OFFSET64; break; */
	   case UIR_NEXTUIR: tOffset += (OFF_T) UIR_NEXTUIR_OFFSET64; break;
	   case UIR_PREVUIR: tOffset += (OFF_T) UIR_PREVUIR_OFFSET64; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write64_64(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriteCCR64.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteCCR64 (vFILE *fp, OFF_T offset, ...)
#else
STATICforIDL CDFstatus WriteCCR64 (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; OFF_T offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, OFF_T);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case CCR_NULL:
	 va_end (ap);
	 return pStatus;
       case CCR_RECORD: {
	 struct CCRstruct64 *CCR = va_arg (ap, struct CCRstruct64 *);
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CWE;
	 if (!Write64_64(fp,&(CCR->RecordSize))) return CWE;
	 if (!Write32_64(fp,&(CCR->RecordType))) return CWE;
         if (!Write64s_64(fp,&(CCR->CPRoffset),2)) return CWE;
/*
	 if (!Write64_64(fp,&(CCR->CPRoffset))) return CWE;
	 if (!Write64_64(fp,&(CCR->uSize))) return CWE;
*/
	 if (!Write32_64(fp,&(CCR->rfuA))) return CWE;
	 break;
       }
       case CCR_RECORDTYPE:
       case CCR_RFUa: {
         Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
         switch (field) {
           case CCR_RECORDTYPE: tOffset += (OFF_T) CCR_RECORDTYPE_OFFSET64; break;
           case CCR_RFUa: tOffset += (OFF_T) CCR_RFUa_OFFSET64; break;
           default: return CDF_INTERNAL_ERROR;
         }
         if (!SEEKv64(fp,offset,vSEEK_SET)) return CWE;
         if (!Write32_64(fp,buffer)) return CWE;
         break;
       }
       default: {
	 OFF_T *buffer = va_arg (ap, OFF_T *); OFF_T tOffset = offset;
	 switch (field) {
	   case CCR_RECORDSIZE: tOffset += (OFF_T) CCR_RECORDSIZE_OFFSET64; break;
/*	   case CCR_RECORDTYPE: tOffset += (OFF_T) CCR_RECORDTYPE_OFFSET64; break; */
	   case CCR_CPROFFSET: tOffset += (OFF_T) CCR_CPROFFSET_OFFSET64; break;
	   case CCR_USIZE: tOffset += (OFF_T) CCR_USIZE_OFFSET64; break;
/*	   case CCR_RFUa: tOffset += (OFF_T) CCR_RFUa_OFFSET64; break; */
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write64_64(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriteCPR64.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteCPR64 (vFILE *fp, OFF_T offset, ...)
#else
STATICforIDL CDFstatus WriteCPR64 (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; OFF_T offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, OFF_T);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case CPR_NULL:
	 va_end (ap);
	 return pStatus;
       case CPR_RECORD: {
	 struct CPRstruct64 *CPR = va_arg (ap, struct CPRstruct64 *); int i;
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CWE;
	 if (!Write64_64(fp,&(CPR->RecordSize))) return CWE;
         if (!Write32s_64(fp,&(CPR->RecordType),4)) return CWE;
/*
	 if (!Write32_64(fp,&(CPR->RecordType))) return CWE;
	 if (!Write32_64(fp,&(CPR->cType))) return CWE;
	 if (!Write32_64(fp,&(CPR->rfuA))) return CWE;
	 if (!Write32_64(fp,&(CPR->pCount))) return CWE;
*/
	 for (i = 0; i < CPR->pCount; i++) {
	    if (!Write32_64(fp,&(CPR->cParms[i]))) return CWE;
	 }
	 break;
       }
       case CPR_RECORDSIZE: {
         OFF_T *buffer = va_arg (ap, OFF_T *); OFF_T tOffset = offset;
         tOffset += (OFF_T) CPR_RECORDSIZE_OFFSET64;
         if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
         if (!Write64_64(fp,buffer)) return CWE;
         break; 
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
	 switch (field) {
/*	   case CPR_RECORDSIZE: tOffset += (OFF_T) CPR_RECORDSIZE_OFFSET64; break; */
	   case CPR_RECORDTYPE: tOffset += (OFF_T) CPR_RECORDTYPE_OFFSET64; break;
	   case CPR_CTYPE: tOffset += (OFF_T) CPR_CTYPE_OFFSET64; break;
	   case CPR_RFUa: tOffset += (OFF_T) CPR_RFUa_OFFSET64; break;
	   case CPR_PCOUNT: tOffset += (OFF_T) CPR_PCOUNT_OFFSET64; break;
	   case CPR_CPARM1: tOffset += (OFF_T) CPR_CPARM1_OFFSET64; break;
	   case CPR_CPARM2: tOffset += (OFF_T) CPR_CPARM2_OFFSET64; break;
	   case CPR_CPARM3: tOffset += (OFF_T) CPR_CPARM3_OFFSET64; break;
	   case CPR_CPARM4: tOffset += (OFF_T) CPR_CPARM4_OFFSET64; break;
	   case CPR_CPARM5: tOffset += (OFF_T) CPR_CPARM5_OFFSET64; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32_64(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriteSPR64.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteSPR64 (vFILE *fp, OFF_T offset, ...)
#else
STATICforIDL CDFstatus WriteSPR64 (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; OFF_T offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, OFF_T);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case SPR_NULL:
	 va_end (ap);
	 return pStatus;
       case SPR_RECORD: {
	 struct SPRstruct64 *SPR = va_arg (ap, struct SPRstruct64 *); int i;
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CWE;
	 if (!Write64_64(fp,&(SPR->RecordSize))) return CWE;
         if (!Write32s_64(fp,&(SPR->RecordType),4)) return CWE;
/*
	 if (!Write32_64(fp,&(SPR->RecordType))) return CWE;
	 if (!Write32_64(fp,&(SPR->sArraysType))) return CWE;
	 if (!Write32_64(fp,&(SPR->rfuA))) return CWE;
	 if (!Write32_64(fp,&(SPR->pCount))) return CWE;
*/
	 for (i = 0; i < SPR->pCount; i++) {
	    if (!Write32_64(fp,&(SPR->sArraysParms[i]))) return CWE;
	 }
	 break;
       }
       case SPR_RECORDSIZE: {
         OFF_T *buffer = va_arg (ap, OFF_T *); OFF_T tOffset = offset;
         tOffset += (OFF_T) SPR_RECORDSIZE_OFFSET64;
         if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
         if (!Write64_64(fp,buffer)) return CWE; 
         break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
	 switch (field) {
/*	   case SPR_RECORDSIZE: tOffset += (OFF_T) SPR_RECORDSIZE_OFFSET64; break; */
	   case SPR_RECORDTYPE: tOffset += (OFF_T) SPR_RECORDTYPE_OFFSET64; break;
	   case SPR_STYPE: tOffset += (OFF_T) SPR_STYPE_OFFSET64; break;
	   case SPR_RFUa: tOffset += (OFF_T) SPR_RFUa_OFFSET64; break;
	   case SPR_PCOUNT: tOffset += (OFF_T) SPR_PCOUNT_OFFSET64; break;
	   case SPR_SPARM1: tOffset += (OFF_T) SPR_SPARM1_OFFSET64; break;
	   case SPR_SPARM2: tOffset += (OFF_T) SPR_SPARM2_OFFSET64; break;
	   case SPR_SPARM3: tOffset += (OFF_T) SPR_SPARM3_OFFSET64; break;
	   case SPR_SPARM4: tOffset += (OFF_T) SPR_SPARM4_OFFSET64; break;
	   case SPR_SPARM5: tOffset += (OFF_T) SPR_SPARM5_OFFSET64; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32_64(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}

/******************************************************************************
* WriteCVVR64.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus WriteCVVR64 (vFILE *fp, OFF_T offset, ...)
#else
STATICforIDL CDFstatus WriteCVVR64 (va_alist)
va_dcl
#endif
{
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; OFF_T offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, OFF_T);
#endif
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case CVVR_NULL:
	 va_end (ap);
	 return pStatus;
       case CVVR_RECORDx: {
	 struct CVVRstruct64 *CVVR = va_arg (ap, struct CVVRstruct64 *);
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CWE;
	 if (!Write64_64(fp,&(CVVR->RecordSize))) return CWE;
         if (!Write32s_64(fp,&(CVVR->RecordType),2)) return CWE;
/*
	 if (!Write32_64(fp,&(CVVR->RecordType))) return CWE;
	 if (!Write32_64(fp,&(CVVR->rfuA))) return CWE;
*/
	 if (!Write64_64(fp,&(CVVR->cSize))) return CWE;
	 break;
       }
       case CVVR_RECORDSIZE:
       case CVVR_CSIZE: {
         OFF_T *buffer = va_arg (ap, OFF_T *); OFF_T tOffset = offset;
         switch (field) {
           case CVVR_RECORDSIZE: tOffset += (OFF_T) CVVR_RECORDSIZE_OFFSET64; break;
           case CVVR_CSIZE: tOffset += (OFF_T) CVVR_CSIZE_OFFSET64; break;
           default: return CDF_INTERNAL_ERROR;
         }
         if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
         if (!Write64_64(fp,buffer)) return CWE;
         break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
	 switch (field) {
/*	   case CVVR_RECORDSIZE: tOffset += (OFF_T) CVVR_RECORDSIZE_OFFSET64; break; */
	   case CVVR_RECORDTYPE: tOffset += (OFF_T) CVVR_RECORDTYPE_OFFSET64; break;
	   case CVVR_RFUa: tOffset += (OFF_T) CVVR_RFUa_OFFSET64; break;
/*	   case CVVR_CSIZE: tOffset += (OFF_T) CVVR_CSIZE_OFFSET64; break; */
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CWE;
	 if (!Write32_64(fp,buffer)) return CWE;
	 break;
       }
     }
  }
}
