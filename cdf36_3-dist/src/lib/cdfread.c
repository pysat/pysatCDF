/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                        Read from internal record.
*
*  Version 1.4a, 28-Feb-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  30-Nov-94, J Love     Original version.
*   V1.1  30-Jan-95, J Love	`Read32s' now checks count.
*   V1.1a 24-Feb-95, J Love	Solaris 2.3 IDL i/f.
*   V1.2  26-May-95, J Love	CDF V2.4 compatibility mode.  What?
*   V1.3  14-Jun-95, J Love	Recursion!
*   V1.3a  4-Aug-95, J Love	More efficient `Read32' and `Read32s'.
*				CDFexport-related changes.
*   V1.4   3-Apr-96, J Love	CDF V2.6.
*   V1.4a 28-Feb-97, J Love	Windows NT for MS Visual C/C++ on an IBM PC.
*   V3.2  21-Jun-07, D Berger   Modified ReadGDR, ReadADR, and ReadAEDR, and
*                               added ReadADRList and ReadAEDRList to support
*                               perfomance enhancements for accessing metadata
*                               in READONLYon mode.
*   V3.2a 11-Apr-08, M Liu      Modified Read32s to eliminate the potential 
*                               buffer overflow.
*   V3.3  28-Jul-08, M Liu      Modified to work with CDF file validation.
*   V3.3a 15-Sep-08, M Liu      Modified ReadAEDRList to pass in the number of
*                               entries to set up the list, not just the last
*                               entry number.
*   V3.4  12-Aug-10, M Liu      Replaced strcpy by strcpyX in ReadVDR. 
*
******************************************************************************/

#include "cdflib.h"

/******************************************************************************
* Local macro definitions.
******************************************************************************/

#define CRE CDF_READ_ERROR
#define CV2C CORRUPTED_V2_CDF 

/******************************************************************************
* Read32.
******************************************************************************/

VISIBLE_PREFIX Logical Read32 (fp, value)
vFILE *fp;
Int32 *value;
{
#if defined(NETWORKbyteORDERcpu)
  if (!READv(value,(size_t)4,(size_t)1,fp)) return FALSE;
#else
  Int32 temp;
  if (!READv(&temp,(size_t)4,(size_t)1,fp)) return FALSE;
  REVERSE4bIO (&temp, value)
#endif
  return TRUE;
}

/******************************************************************************
* Read32s.
******************************************************************************/

STATICforIDL Logical Read32s (fp, buffer, count)
vFILE *fp;
Int32 *buffer;
int count;
{
#define MAX_READ32s CDF_MAX_DIMS	/* This must be the maximum of
					   CDF_MAX_DIMS and MAX_VXR_ENTRIES
					   (and for any other uses of
					   `Read32s'). */
#if defined(NETWORKbyteORDERcpu)
  if (count == 0) return TRUE;
  if (count < 0 || count > MAX_READ32s) return FALSE;
  if (!READv(buffer,(size_t)4,(size_t)count,fp)) return FALSE;
#else
  int i; Int32 temp[MAX_READ32s];
  if (count == 0) return TRUE;
  if (count < 0 || count > MAX_READ32s) return FALSE;
  if (!READv(temp,(size_t)4,(size_t)count,fp)) return FALSE;
  for (i = 0; i < count; i++) {
     REVERSE4bIO (&temp[i], &buffer[i])
  }
#endif
  return TRUE;
}

/******************************************************************************
* ReadIrSize.
*   The size is always in the first 4-byte field.
******************************************************************************/

STATICforIDL CDFstatus ReadIrSize (fp, offset, irSize)
vFILE *fp;
Int32 offset;
Int32 *irSize;
{
  if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CRE;
  if (!Read32(fp,irSize)) return CRE;
  return CDF_OK;
}  

/******************************************************************************
* ReadIrType.
*   The type is always in the second 4-byte field.
******************************************************************************/

VISIBLE_PREFIX CDFstatus ReadIrType (fp, offset, irType)
vFILE *fp;
Int32 offset;
Int32 *irType;
{
  long irTypeOffset = offset + sizeof(Int32);
  if (!SEEKv(fp,irTypeOffset,vSEEK_SET)) return CRE;
  if (!Read32(fp,irType)) return CRE;
  return CDF_OK;
}  

/******************************************************************************
* ReadCDR.
*   Note that the length of the CDF copyright was decreased in CDF V2.5 (there
* were way too many characters allowed for).  When reading the copyright, only
* CDF_COPYRIGHT_LEN characters will be read.  This will be less than the
* actual number in CDFs prior to CDF V2.4 but is enough to include all of the
* characters that were used.  (The value of CDF_COPYRIGHT_LEN was decreased
* from CDF V2.4 to CDF V2.5.)
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadCDR (vFILE *fp, Int32 offset, ...)
#else
STATICforIDL CDFstatus ReadCDR (va_alist)
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
	 void *copyRight = va_arg (ap, char *);
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CRE;
	 if (!Read32(fp,&(CDR->RecordSize))) return CRE;
	 if (!Read32(fp,&(CDR->RecordType))) return CRE;
	 if (!Read32(fp,&(CDR->GDRoffset))) return CRE;
	 if (!Read32(fp,&(CDR->Version))) return CRE;
	 if (!Read32(fp,&(CDR->Release))) return CRE;
	 if (!Read32(fp,&(CDR->Encoding))) return CRE;
	 if (!Read32(fp,&(CDR->Flags))) return CRE;
	 if (!Read32(fp,&(CDR->rfuA))) return CRE;
	 if (!Read32(fp,&(CDR->rfuB))) return CRE;
	 if (!Read32(fp,&(CDR->Increment))) return CRE;
	 if (!Read32(fp,&(CDR->rfuD))) return CRE;
	 if (!Read32(fp,&(CDR->rfuE))) return CRE;
	 if (copyRight != NULL) {
	   if (!READv(copyRight,CDF_COPYRIGHT_LEN,1,fp)) return CRE;
	   NulPad (copyRight, CDF_COPYRIGHT_LEN);
	 }
	 break;
       }
       case CDR_COPYRIGHT: {
	 void *copyRight = va_arg (ap, char *);
	 long tOffset = offset + CDR_COPYRIGHT_OFFSET;
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!READv(copyRight,CDF_COPYRIGHT_LEN,1,fp)) return CRE;
	 NulPad (copyRight, CDF_COPYRIGHT_LEN);
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
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read32(fp,buffer)) return CRE;
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadGDR.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadGDR (vFILE *fp, Int32 offset, ...)
#else
STATICforIDL CDFstatus ReadGDR (va_alist)
va_dcl
#endif
{
  long i;
  long read_only_mode;
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; Int32 offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, Int32);
#endif
  /***************************************************************************
  * Check read only mode. If it is invalid, it may have not been initialized -
  * set it to off.
  ***************************************************************************/
  pStatus = CDFlib(CONFIRM_, CDF_READONLY_MODE_, &read_only_mode, NULL_);
  if (pStatus != CDF_OK) return pStatus;
  if (read_only_mode != READONLYon && read_only_mode != READONLYoff)
  {
      read_only_mode = READONLYoff;
  };
  /***************************************************************************
  * If READONLYon, read the GDR into memory for future reference.
  ***************************************************************************/
  if (read_only_mode == READONLYon && fp->GDR == NULL)
  {
      fp->GDR = cdf_AllocateMemory((size_t)sizeof(struct GDRstruct), NULL);
      if (fp->GDR == NULL) return CRE;
      if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CRE;
      if (!Read32(fp,&(fp->GDR->RecordSize))) return CRE;
      if (!Read32(fp,&(fp->GDR->RecordType))) return CRE;
      if (!Read32(fp,&(fp->GDR->rVDRhead))) return CRE;
      if (!Read32(fp,&(fp->GDR->zVDRhead))) return CRE;
      if (!Read32(fp,&(fp->GDR->ADRhead))) return CRE;
      if (!Read32(fp,&(fp->GDR->eof))) return CRE;
      if (!Read32(fp,&(fp->GDR->NrVars))) return CRE;
      if (!Read32(fp,&(fp->GDR->NumAttr))) return CRE;
      if (!Read32(fp,&(fp->GDR->rMaxRec))) return CRE;
      if (!Read32(fp,&(fp->GDR->rNumDims))) return CRE;
      if (!Read32(fp,&(fp->GDR->NzVars))) return CRE;
      if (!Read32(fp,&(fp->GDR->UIRhead))) return CRE;
      if (!Read32(fp,&(fp->GDR->rfuC))) return CRE;
      if (!Read32(fp,&(fp->GDR->rfuD))) return CRE;
      if (!Read32(fp,&(fp->GDR->rfuE))) return CRE;
      if (fp->GDR->rNumDims < 0 || fp->GDR->rNumDims > CDF_MAX_DIMS)
        return CV2C;  
      if (!Read32s(fp,fp->GDR->rDimSizes,
                   (int)fp->GDR->rNumDims)) return CRE;
      pStatus = ReadADRList(fp);
      if (pStatus != CDF_OK) return pStatus;
  };
  /**************************************************************************
  * For whatever data is requested from the GDR, if READONLYon, get the data
  * from the GDR in memory, otherwise read from the file/cache.
  ***************************************************************************/
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case GDR_NULL:
	 va_end (ap);
	 return pStatus;
       case GDR_RECORD: {
	 struct GDRstruct *GDR = va_arg (ap, struct GDRstruct *);
         if (read_only_mode == READONLYoff)
         {
             if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CRE;
             if (!Read32(fp,&(GDR->RecordSize))) return CRE;
             if (!Read32(fp,&(GDR->RecordType))) return CRE;
             if (!Read32(fp,&(GDR->rVDRhead))) return CRE;
             if (!Read32(fp,&(GDR->zVDRhead))) return CRE;
             if (!Read32(fp,&(GDR->ADRhead))) return CRE;
             if (!Read32(fp,&(GDR->eof))) return CRE;
             if (!Read32(fp,&(GDR->NrVars))) return CRE;
             if (!Read32(fp,&(GDR->NumAttr))) return CRE;
             if (!Read32(fp,&(GDR->rMaxRec))) return CRE;
             if (!Read32(fp,&(GDR->rNumDims))) return CRE;
             if (!Read32(fp,&(GDR->NzVars))) return CRE;
             if (!Read32(fp,&(GDR->UIRhead))) return CRE;
             if (!Read32(fp,&(GDR->rfuC))) return CRE;
             if (!Read32(fp,&(GDR->rfuD))) return CRE;
             if (!Read32(fp,&(GDR->rfuE))) return CRE;
             if (GDR->rNumDims < 0 || GDR->rNumDims > CDF_MAX_DIMS)
               break; /* return CV2C;  */
             if (!Read32s(fp,GDR->rDimSizes,(int)GDR->rNumDims)) return CRE;
         }
         else
         {
             GDR->RecordSize = fp->GDR->RecordSize;
             GDR->RecordType = fp->GDR->RecordType;
             GDR->rVDRhead = fp->GDR->rVDRhead;
             GDR->zVDRhead = fp->GDR->zVDRhead;
             GDR->ADRhead = fp->GDR->ADRhead;
             GDR->eof = fp->GDR->eof;
             GDR->NrVars = fp->GDR->NrVars;
             GDR->NumAttr = fp->GDR->NumAttr;
             GDR->rMaxRec = fp->GDR->rMaxRec;
             GDR->rNumDims = fp->GDR->rNumDims;
             GDR->NzVars = fp->GDR->NzVars;
             GDR->UIRhead = fp->GDR->UIRhead;
             GDR->rfuC = fp->GDR->rfuC;
             GDR->rfuD = fp->GDR->rfuD;
             GDR->rfuE = fp->GDR->rfuE;
             for (i = 0; i < GDR->rNumDims; i++)
             {
                 GDR->rDimSizes[i] = fp->GDR->rDimSizes[i];
             };
         }
         break;
       }
       case GDR_rDIMSIZES: {
	 Int32 *rDimSizes = va_arg (ap, Int32 *); Int32 rNumDims; long tOffset;
         if (read_only_mode == READONLYoff)
         {
	     if (!sX(ReadGDR(fp,offset,
                             GDR_rNUMDIMS,&rNumDims,
                             GDR_NULL),&pStatus)) return pStatus;
	     tOffset = offset + GDR_rDIMSIZES_OFFSET;
	     if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
             if (rNumDims < 0 || rNumDims > CDF_MAX_DIMS)
               return CV2C;
	     if (!Read32s(fp,rDimSizes,(int)rNumDims)) return CRE;
         }
         else
         {
             for (i = 0; i < fp->GDR->rNumDims; i++)
             {
                 rDimSizes[i] = fp->GDR->rDimSizes[i];
             };
         }
         break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); long tOffset = offset;
	 switch (field) {
	   case GDR_RECORDSIZE: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += GDR_RECORDSIZE_OFFSET; 
               }
               else
               {
                   *buffer = fp->GDR->RecordSize;
               };
               break;
           }
	   case GDR_RECORDTYPE: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += GDR_RECORDTYPE_OFFSET; 
               }
               else
               {
                   *buffer = fp->GDR->RecordType;
               }
               break;
           }
	   case GDR_rVDRHEAD: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += GDR_rVDRHEAD_OFFSET; 
               }
               else
               {
                   *buffer = fp->GDR->rVDRhead;
               }
               break;
           }
	   case GDR_zVDRHEAD: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += GDR_zVDRHEAD_OFFSET; 
               }
               else
               {
                   *buffer = fp->GDR->zVDRhead;
               }
               break;
           }
	   case GDR_ADRHEAD: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += GDR_ADRHEAD_OFFSET; 
               }
               else
               {
                   *buffer = fp->GDR->ADRhead;
               }
               break;
           }
	   case GDR_EOF: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += GDR_EOF_OFFSET; 
               }
               else
               {
                   *buffer = fp->GDR->eof;
               }
               break;
           }
	   case GDR_NrVARS: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += GDR_NrVARS_OFFSET; 
               }
               else
               {
                   *buffer = fp->GDR->NrVars;
               }
               break;
           }
	   case GDR_NUMATTR: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += GDR_NUMATTR_OFFSET; 
               }
               else
               {
                   *buffer = fp->GDR->NumAttr;
               }
               break;
           }
	   case GDR_rMAXREC: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += GDR_rMAXREC_OFFSET; 
               }
               else
               {
                   *buffer = fp->GDR->rMaxRec;
               }
               break;
           }
	   case GDR_rNUMDIMS: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += GDR_rNUMDIMS_OFFSET; 
               }
               else
               {
                   *buffer = fp->GDR->rNumDims;
               }
               break;
           }
	   case GDR_NzVARS: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += GDR_NzVARS_OFFSET; 
               }
               else
               {
                   *buffer = fp->GDR->NzVars;
               }
               break;
           }
	   case GDR_UIRHEAD: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += GDR_UIRHEAD_OFFSET; 
               }
               else
               {
                   *buffer = fp->GDR->UIRhead;
               }
               break;
           }
	   default: return CDF_INTERNAL_ERROR;
	 }
         if (read_only_mode == READONLYoff)
         {
	     if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
	     if (!Read32(fp,buffer)) return CRE;
         }
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadADR.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadADR (vFILE *fp, Int32 offset, ...)
#else
STATICforIDL CDFstatus ReadADR (va_alist)
va_dcl
#endif
{
  long read_only_mode;
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; Int32 offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, Int32);
#endif
  /***************************************************************************
  * IF READONLYon, and there are no ADRs in memory or no attribute has been
  * selected, return NO_ATTR_SELECTED. Note that reading of ADRs into memory
  * is triggered the 1st time an ADR is selected.
  ***************************************************************************/
  pStatus = CDFlib(CONFIRM_, CDF_READONLY_MODE_, &read_only_mode, NULL_);
  if (pStatus != CDF_OK) return pStatus;
  if (read_only_mode == READONLYon && fp->CurADRIndex == RESERVED_ENTRYNUM)
  {
      return NO_ATTR_SELECTED;
  };
  /***************************************************************************
  * For whatever ADR data is selected, if READONLYon, read the data from memory
  * using CurADRIndex to select the correct ADR from the ADRList. Otherwise,
  * read the data from the file/cache using the file offset.
  ***************************************************************************/ 
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case ADR_NULL:
	 va_end (ap);
	 return pStatus;
       case ADR_RECORD: {
	 struct ADRstruct *ADR = va_arg (ap, struct ADRstruct *);
         if (read_only_mode == READONLYoff)
         {
             if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CRE;
             if (!Read32(fp,&(ADR->RecordSize))) return CRE;
             if (!Read32(fp,&(ADR->RecordType))) return CRE;
             if (!Read32(fp,&(ADR->ADRnext))) return CRE;
             if (!Read32(fp,&(ADR->AgrEDRhead))) return CRE;
             if (!Read32(fp,&(ADR->Scope))) return CRE;
             if (!Read32(fp,&(ADR->Num))) return CRE;
             if (!Read32(fp,&(ADR->NgrEntries))) return CRE;
             if (!Read32(fp,&(ADR->MAXgrEntry))) return CRE;
             if (!Read32(fp,&(ADR->rfuA))) return CRE;
             if (!Read32(fp,&(ADR->AzEDRhead))) return CRE;
             if (!Read32(fp,&(ADR->NzEntries))) return CRE;
             if (!Read32(fp,&(ADR->MAXzEntry))) return CRE;
             if (!Read32(fp,&(ADR->rfuE))) return CRE;
             if (!READv(ADR->Name,CDF_ATTR_NAME_LEN,1,fp)) return CRE;
             NulPad (ADR->Name, CDF_ATTR_NAME_LEN);
         }
         else
         {
             ADR->RecordSize = fp->ADRList[fp->CurADRIndex]->RecordSize;
             ADR->RecordType = fp->ADRList[fp->CurADRIndex]->RecordType;
             ADR->ADRnext = fp->ADRList[fp->CurADRIndex]->ADRnext;
             ADR->AgrEDRhead = fp->ADRList[fp->CurADRIndex]->AgrEDRhead;
             ADR->Scope = fp->ADRList[fp->CurADRIndex]->Scope;
             ADR->Num = fp->ADRList[fp->CurADRIndex]->Num;
             ADR->NgrEntries = fp->ADRList[fp->CurADRIndex]->NgrEntries;
             ADR->MAXgrEntry = fp->ADRList[fp->CurADRIndex]->MAXgrEntry;
             ADR->rfuA = fp->ADRList[fp->CurADRIndex]->rfuA;
             ADR->AzEDRhead = fp->ADRList[fp->CurADRIndex]->AzEDRhead;
             ADR->NzEntries = fp->ADRList[fp->CurADRIndex]->NzEntries;
             ADR->MAXzEntry = fp->ADRList[fp->CurADRIndex]->MAXzEntry;
             ADR->rfuE = fp->ADRList[fp->CurADRIndex]->rfuE;
             strcpyX (ADR->Name, fp->ADRList[fp->CurADRIndex]->Name,
                      CDF_ATTR_NAME_LEN);
         }
         break;
       }
       case ADR_NAME: {
	 char *aName = va_arg (ap, char *);
         if (read_only_mode == READONLYoff) 
         {
	     long tOffset = offset + ADR_NAME_OFFSET;
	     if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
	     if (!READv(aName,CDF_ATTR_NAME_LEN,1,fp)) return CRE;
	     NulPad (aName, CDF_ATTR_NAME_LEN);
         }
         else
         {
             strcpyX (aName, fp->ADRList[fp->CurADRIndex]->Name,
                      CDF_ATTR_NAME_LEN);
         }
	 break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); long tOffset = offset;
	 switch (field) {
	   case ADR_RECORDSIZE: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += ADR_RECORDSIZE_OFFSET;
               }
               else
               {
                   *buffer = fp->ADRList[fp->CurADRIndex]->RecordSize;
               };
               break;
           }
	   case ADR_RECORDTYPE: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += ADR_RECORDTYPE_OFFSET;
               }
               else
               {
                   *buffer = fp->ADRList[fp->CurADRIndex]->RecordType;
               };
               break;
           }
	   case ADR_ADRNEXT: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += ADR_ADRNEXT_OFFSET;
               }
               else
               {
                   *buffer = fp->ADRList[fp->CurADRIndex]->ADRnext;
               };
               break;
           }
	   case ADR_AgrEDRHEAD: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += ADR_AgrEDRHEAD_OFFSET;
               }
               else
               {
                   *buffer = fp->ADRList[fp->CurADRIndex]->AgrEDRhead;
               };
               break;
           }
	   case ADR_SCOPE: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += ADR_SCOPE_OFFSET;
               }
               else
               {
                   *buffer = fp->ADRList[fp->CurADRIndex]->Scope;
               };
               break;
           }
	   case ADR_NUM: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += ADR_NUM_OFFSET;
               }
               else
               {
                   *buffer = fp->ADRList[fp->CurADRIndex]->Num;
               };
               break;
           }
	   case ADR_NgrENTRIES: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += ADR_NgrENTRIES_OFFSET;
               }
               else
               {
                   *buffer = fp->ADRList[fp->CurADRIndex]->NgrEntries;
               };
               break;
           }
	   case ADR_MAXgrENTRY: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += ADR_MAXgrENTRY_OFFSET;
               }
               else
               {
                   *buffer = fp->ADRList[fp->CurADRIndex]->MAXgrEntry;
               };
               break;
           }
	   case ADR_AzEDRHEAD: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += ADR_AzEDRHEAD_OFFSET;
               }
               else
               {
                   *buffer = fp->ADRList[fp->CurADRIndex]->AzEDRhead;
               };
               break;
           }
	   case ADR_NzENTRIES: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += ADR_NzENTRIES_OFFSET;
               }
               else
               {
                   *buffer = fp->ADRList[fp->CurADRIndex]->NzEntries;
               };
               break;
           }
	   case ADR_MAXzENTRY: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += ADR_MAXzENTRY_OFFSET;
               }
               else
               {
                   *buffer = fp->ADRList[fp->CurADRIndex]->MAXzEntry;
               };
               break;
           }
	   default: return CDF_INTERNAL_ERROR;
	 }
         if (read_only_mode == READONLYoff) {
	     if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
	     if (!Read32(fp,buffer)) return CRE;
         };
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadAEDR/AzEDR.
*   If the entry value is being read, it is passed back in the encoding of the
* CDF (no decoding is performed).  The caller must decode the value (if that
* is necessary).
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadAEDR (vFILE *fp, Int32 offset, ...)
#else
STATICforIDL CDFstatus ReadAEDR (va_alist)
va_dcl
#endif
{
  long read_only_mode;
  struct AEDRstructExt *CurAEDR;
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; Int32 offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, Int32);
#endif
  pStatus = CDFlib(CONFIRM_, CDF_READONLY_MODE_, &read_only_mode, NULL_);
  if (pStatus != CDF_OK) return pStatus;
  /***************************************************************************
  * If READONLYon and either an attribute or an entry have not been selected,
  * return the appropriate status. If they have been selected but there is no
  * entry for the selected attribute, return NO_SUCH_ENTRY.  Note that for each
  * attribute, entries are stored in an array which is indexed by the entry's
  * number and if the selected entry number does not exist for the seleceted
  * attribute, the entry list has a NULL pointer for that entry. Note also that
  * the reading of ADRs and AEDRs into memory is triggered by the 1st time an 
  * ADR (or AEDR) is selected.
  ***************************************************************************/
  if (read_only_mode == READONLYon)
  {
      if (fp->CurADRIndex == RESERVED_ENTRYNUM)
      {
          return NO_ATTR_SELECTED;
      }
      else if (fp->CurAEDRIndex == RESERVED_ENTRYNUM)
      {
          return NO_ENTRY_SELECTED;
      }
      else if (fp->CURzEntrySel)
      {
          if (fp->ADRList[fp->CurADRIndex]->zAEDRList[fp->CurAEDRIndex] != NULL)
          {
              CurAEDR = 
                      fp->ADRList[fp->CurADRIndex]->zAEDRList[fp->CurAEDRIndex];
          }
          else
          {
              return NO_SUCH_ENTRY;
          };
      }
      else
      {
          if (fp->ADRList[fp->CurADRIndex]->grAEDRList[fp->CurAEDRIndex] != 
              NULL)
          {
              CurAEDR = 
                     fp->ADRList[fp->CurADRIndex]->grAEDRList[fp->CurAEDRIndex];
          }
          else
          {
              return NO_SUCH_ENTRY;
          };
      };  
  }; 
  /***************************************************************************
  * For whatever AEDR data is selected, if READONLYon, read the data from memory
  * using CurADRIndex and CurAEDRIndex to select the correct AEDR from the 
  * AEDRList. Otherwise, read the data from the file/cache using the file 
  * offset.
  ***************************************************************************/ 
  for (;;) {
     int field = va_arg (ap, int);
     switch (field) {
       case AEDR_NULL:
	 va_end (ap);
	 return pStatus;
       case AEDR_RECORD: {
	 struct AEDRstruct *AEDR = va_arg (ap, struct AEDRstruct *);
	 void *value = va_arg (ap, void *); size_t nBytes;
         if (read_only_mode == READONLYoff)
         {
             if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CRE;
             if (!Read32(fp,&(AEDR->RecordSize))) return CRE;
             if (!Read32(fp,&(AEDR->RecordType))) return CRE;
             if (!Read32(fp,&(AEDR->AEDRnext))) return CRE;
             if (!Read32(fp,&(AEDR->AttrNum))) return CRE;
             if (!Read32(fp,&(AEDR->DataType))) return CRE;
             if (!Read32(fp,&(AEDR->Num))) return CRE;
             if (!Read32(fp,&(AEDR->NumElems))) return CRE;
             if (!Read32(fp,&(AEDR->rfuA))) return CRE;
             if (!Read32(fp,&(AEDR->rfuB))) return CRE;
             if (!Read32(fp,&(AEDR->rfuC))) return CRE;
             if (!Read32(fp,&(AEDR->rfuD))) return CRE;
             if (!Read32(fp,&(AEDR->rfuE))) return CRE;
             if (value != NULL) {
               nBytes = (size_t) (CDFelemSize(AEDR->DataType) *
                                  AEDR->NumElems);
               if (!READv(value,nBytes,1,fp)) return CRE;
               if (STRINGdataType(AEDR->DataType)) {
                 FillSpacesToString((char *)value, (int)AEDR->NumElems,
                                    (int)AEDR->NumElems);
               }

             };
         }
         else
         {
             AEDR->RecordSize = CurAEDR->AEDR.RecordSize;
             AEDR->RecordType = CurAEDR->AEDR.RecordType;
             AEDR->AEDRnext = CurAEDR->AEDR.AEDRnext;
             AEDR->AttrNum = CurAEDR->AEDR.AttrNum;
             AEDR->DataType = CurAEDR->AEDR.DataType;
             AEDR->Num = CurAEDR->AEDR.Num;
             AEDR->NumElems = CurAEDR->AEDR.NumElems;
             AEDR->rfuA = CurAEDR->AEDR.rfuA;
             AEDR->rfuB = CurAEDR->AEDR.rfuB;
             AEDR->rfuC = CurAEDR->AEDR.rfuC;
             AEDR->rfuD = CurAEDR->AEDR.rfuD;
             AEDR->rfuE = CurAEDR->AEDR.rfuE;
             if (value != NULL) {
                 memcpy(value, CurAEDR->Value, CurAEDR->ValueSize);
             };
         };
         break;
       }
       case AEDR_VALUE: {
	 void *value = va_arg (ap, void *);
         if (read_only_mode == READONLYoff)
         {
	     size_t nBytes; Int32 dataType, numElems; long tOffset;
	     if (!sX(ReadAEDR(fp,offset,
                              AEDR_DATATYPE,&dataType,
                              AEDR_NUMELEMS,&numElems,
                              AEDR_NULL),&pStatus)) return pStatus;
	     tOffset = offset + AEDR_VALUE_OFFSET;
	     if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
	     nBytes = (size_t) (CDFelemSize(dataType) * numElems);
	     if (!READv(value,nBytes,1,fp)) return CRE;
             if (STRINGdataType(dataType)) {
               FillSpacesToString((char *)value, (int)numElems, (int)numElems);
             }
         }
         else
         {
             memcpy(value, CurAEDR->Value, CurAEDR->ValueSize);
         };
	 break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); long tOffset = offset;
	 switch (field) {
	   case AEDR_RECORDSIZE: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += AEDR_RECORDSIZE_OFFSET; 
               }
               else
               {
                   *buffer = CurAEDR->AEDR.RecordSize;
               };
               break;
           }
	   case AEDR_RECORDTYPE: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += AEDR_RECORDTYPE_OFFSET; 
               }
               else
               {
                   *buffer = CurAEDR->AEDR.RecordType;
               };
               break;
           }
	   case AEDR_AEDRNEXT: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += AEDR_AEDRNEXT_OFFSET; 
               }
               else
               {
                   *buffer = CurAEDR->AEDR.AEDRnext;
               };
               break;
           }
	   case AEDR_ATTRNUM: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += AEDR_ATTRNUM_OFFSET; 
               }
               else
               {
                   *buffer = CurAEDR->AEDR.AttrNum;
               };
               break;
           }
	   case AEDR_DATATYPE: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += AEDR_DATATYPE_OFFSET; 
               }
               else
               {
                   *buffer = CurAEDR->AEDR.DataType;
               };
               break;
           }
	   case AEDR_NUM: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += AEDR_NUM_OFFSET; 
               }
               else
               {
                   *buffer = CurAEDR->AEDR.Num;
               };
               break;
           }
	   case AEDR_NUMELEMS: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += AEDR_NUMELEMS_OFFSET; 
               }
               else
               {
                   *buffer = CurAEDR->AEDR.NumElems;
               };
               break;
           }
	   default: return CDF_INTERNAL_ERROR;
	 }
         if (read_only_mode == READONLYoff)
         {
	     if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
	     if (!Read32(fp,buffer)) return CRE;
         };
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadVDR/zVDR.
*   If the pad value is being read, it is passed back in the encoding of the
* CDF (no decoding is performed).  The caller must decode the value (if that
* is necessary).
*   If this CDF contains wasted space in its VDRs, note that the offset for
* those fields after the wasted space is adjusted.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadVDR (struct CDFstruct *CDF, vFILE *fp,
				Int32 offset, Logical zVar, ...)
#else
STATICforIDL CDFstatus ReadVDR (va_alist)
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
	 void *padValue = va_arg (ap, void *); Int32 nDims;
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CRE;
	 if (!Read32(fp,&(VDR->RecordSize))) return CRE;
	 if (!Read32(fp,&(VDR->RecordType))) return CRE;
	 if (!Read32(fp,&(VDR->VDRnext))) return CRE;
	 if (!Read32(fp,&(VDR->DataType))) return CRE;
	 if (!Read32(fp,&(VDR->MaxRec))) return CRE;
	 if (!Read32(fp,&(VDR->VXRhead))) return CRE;
	 if (!Read32(fp,&(VDR->VXRtail))) return CRE;
	 if (!Read32(fp,&(VDR->Flags))) return CRE;
	 if (!Read32(fp,&(VDR->sRecords))) return CRE;
	 if (!Read32(fp,&(VDR->rfuB))) return CRE;
	 if (!Read32(fp,&(VDR->rfuC))) return CRE;
	 if (!Read32(fp,&(VDR->rfuF))) return CRE;
	 if (CDF->wastedSpace) {
	   if (!SEEKv(fp,(long)VDR_WASTED_SIZE,vSEEK_CUR)) return CRE;
	 }
	 if (!Read32(fp,&(VDR->NumElems))) return CRE;
	 if (!Read32(fp,&(VDR->Num))) return CRE;
	 if (!Read32(fp,&(VDR->CPRorSPRoffset))) return CRE;
	 if (!Read32(fp,&(VDR->blockingFactor))) return CRE;
	 if (!READv(VDR->Name,CDF_VAR_NAME_LEN,1,fp)) return CRE;
	 NulPad (VDR->Name, CDF_VAR_NAME_LEN);
	 if (zVar) {
	   if (!Read32(fp,&(VDR->zNumDims))) return CRE;
           if (VDR->zNumDims < 0 || VDR->zNumDims > CDF_MAX_DIMS)
             return CV2C;
	   if (!Read32s(fp,VDR->zDimSizes,(int)VDR->zNumDims)) return CRE;
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
	   if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
	 }
         if (nDims < 0 || nDims > CDF_MAX_DIMS)
           return CV2C;
	 if (!Read32s(fp,VDR->DimVarys,(int)nDims)) return CRE;
	 if (PADvalueBITset(VDR->Flags) && padValue != NULL) {
	   size_t nBytes = (size_t) (CDFelemSize(VDR->DataType)*VDR->NumElems);
	   if (!READv(padValue,nBytes,1,fp)) return CRE;
           if (STRINGdataType(VDR->DataType)) {
             FillSpacesToString((char *)padValue, (int)VDR->NumElems,
                                (int)VDR->NumElems);
           }
	 }
	 break;
       }
       case VDR_NAME: {
	 char *vName = va_arg (ap, char *);
	 long tOffset = offset + VDR_NAME_OFFSET;
	 if (CDF->wastedSpace) tOffset += VDR_WASTED_SIZE;
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!READv(vName,CDF_VAR_NAME_LEN,1,fp)) return CRE;
	 NulPad (vName, CDF_VAR_NAME_LEN);
	 break;
       }
       case VDR_zNUMDIMS: {
	 Int32 *numDims = va_arg (ap, Int32 *);
	 long tOffset = offset + zVDR_zNUMDIMS_OFFSET +
			BOO(CDF->wastedSpace,VDR_WASTED_SIZE,0);
	 if (!zVar) return CDF_INTERNAL_ERROR;
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read32(fp,numDims)) return CRE;
	 break;
       }
       case VDR_zDIMSIZES: {
	 Int32 *zDimSizes = va_arg (ap, Int32 *);
	 Int32 zNumDims; long tOffset;
	 if (!zVar) return CDF_INTERNAL_ERROR;
	 if (!sX(ReadVDR(CDF,fp,offset,zVar,
			 VDR_zNUMDIMS,&zNumDims,
			 VDR_NULL),&pStatus)) return pStatus;
	 tOffset = offset + zVDR_zDIMSIZES_OFFSET +
		   BOO(CDF->wastedSpace,VDR_WASTED_SIZE,0);
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
         if (zNumDims < 0 || zNumDims > CDF_MAX_DIMS)
           return CV2C;
	 if (!Read32s(fp,zDimSizes,(int)zNumDims)) return CRE;
	 break;
       }
       case VDR_DIMVARYS: {
	 Int32 *dimVarys = va_arg (ap, Int32 *);
	 Int32 nDims; long tOffset;
	 if (zVar) {
	   if (!sX(ReadVDR(CDF,fp,offset,zVar,
			   VDR_zNUMDIMS,&nDims,
			   VDR_NULL),&pStatus)) return pStatus;
	   tOffset = offset + zVDR_DIMVARYS_OFFSETb + (nDims*sizeof(Int32)) +
		     BOO(CDF->wastedSpace,VDR_WASTED_SIZE,0);
	 }
	 else {
	   Int32 GDRoffset;
	   if (!sX(ReadCDR(fp,V2_CDR_OFFSET,
			   CDR_GDROFFSET,&GDRoffset,
			   CDR_NULL),&pStatus)) return pStatus;
	   if (!sX(ReadGDR(fp,GDRoffset,
			   GDR_rNUMDIMS,&nDims,
			   GDR_NULL),&pStatus)) return pStatus;
	   tOffset = offset + rVDR_DIMVARYS_OFFSET +
		     BOO(CDF->wastedSpace,VDR_WASTED_SIZE,0);
	 }
         if (nDims < 0 || nDims > CDF_MAX_DIMS)
           return CV2C;
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read32s(fp,dimVarys,(int)nDims)) return CRE;
	 break;
       }
       case VDR_PADVALUE: {
	 void *padValue = va_arg (ap, void *);
	 Int32 dataType, numElems; size_t nBytes; long tOffset;
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
		     (zNumDims*sizeof(Int32)) +
		     (zNumDims*sizeof(Int32)) +
		     BOO(CDF->wastedSpace,VDR_WASTED_SIZE,0);
	 }
	 else {
	   Int32 rNumDims, GDRoffset;
	   if (!sX(ReadCDR(fp,V2_CDR_OFFSET,
			   CDR_GDROFFSET,&GDRoffset,
			   CDR_NULL),&pStatus)) return pStatus;
	   if (!sX(ReadGDR(fp,GDRoffset,
			   GDR_rNUMDIMS,&rNumDims,
			   GDR_NULL),&pStatus)) return pStatus;
	   tOffset = offset + rVDR_PADVALUE_OFFSETb +
		     (rNumDims*sizeof(Int32)) +
		     BOO(CDF->wastedSpace,VDR_WASTED_SIZE,0);
	 }
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
	 nBytes = (size_t) (CDFelemSize(dataType) * numElems);
	 if (!READv(padValue,nBytes,1,fp)) return CRE;
         if (STRINGdataType(dataType)) {
           FillSpacesToString((char *)padValue, (int)numElems,
                              (int)numElems);
         }
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
	   case VDR_NUMELEMS:
	     tOffset += (VDR_NUMELEMS_OFFSET +
			 BOO(CDF->wastedSpace,VDR_WASTED_SIZE,0));
	     break;
	   case VDR_NUM:
	     tOffset += (VDR_NUM_OFFSET +
			 BOO(CDF->wastedSpace,VDR_WASTED_SIZE,0));
	     break;
	   case VDR_CPRorSPR:
	     tOffset += (VDR_CPRorSPR_OFFSET +
			 BOO(CDF->wastedSpace,VDR_WASTED_SIZE,0));
	     break;
	   case VDR_BLOCKING:
	     tOffset += (VDR_BLOCKING_OFFSET +
			 BOO(CDF->wastedSpace,VDR_WASTED_SIZE,0));
	     break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read32(fp,buffer)) return CRE;
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadVXR.
******************************************************************************/

#if defined(STDARG)
VISIBLE_PREFIX CDFstatus ReadVXR (vFILE *fp, Int32 offset, ...)
#else
VISIBLE_PREFIX CDFstatus ReadVXR (va_alist)
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
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CRE;
	 if (!Read32(fp,&(VXR->RecordSize))) return CRE;
	 if (!Read32(fp,&(VXR->RecordType))) return CRE;
	 if (!Read32(fp,&(VXR->VXRnext))) return CRE;
	 if (!Read32(fp,&(VXR->Nentries))) return CRE;
	 if (!Read32(fp,&(VXR->NusedEntries))) return CRE;
         if (VXR->Nentries < 0 || VXR->Nentries > MAX_VXR_ENTRIES)
           return CV2C;
	 if (!Read32s(fp,VXR->First,(int)VXR->Nentries)) return CRE;
	 if (!Read32s(fp,VXR->Last,(int)VXR->Nentries)) return CRE;
	 if (!Read32s(fp,VXR->Offset,(int)VXR->Nentries)) return CRE;
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
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
         if (nEntries < 0 || nEntries > MAX_VXR_ENTRIES)
           return CV2C;
	 if (!Read32s(fp,buffer,(int)nEntries)) return CRE;
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
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read32(fp,buffer)) return CRE;
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadVVR.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadVVR (vFILE *fp, Int32 offset, ...)
#else
STATICforIDL CDFstatus ReadVVR (va_alist)
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
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CRE;
	 if (!Read32(fp,&(VVR->RecordSize))) return CRE;
	 if (!Read32(fp,&(VVR->RecordType))) return CRE;
	 break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); long tOffset = offset;
	 switch (field) {
	   case VVR_RECORDSIZE: tOffset += VVR_RECORDSIZE_OFFSET; break;
	   case VVR_RECORDTYPE: tOffset += VVR_RECORDTYPE_OFFSET; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read32(fp,buffer)) return CRE;
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadUIR.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadUIR (vFILE *fp, Int32 offset, ...)
#else
STATICforIDL CDFstatus ReadUIR (va_alist)
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
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CRE;
	 if (!Read32(fp,&(UIR->RecordSize))) return CRE;
	 if (!Read32(fp,&(UIR->RecordType))) return CRE;
	 if (!Read32(fp,&(UIR->NextUIR))) return CRE;
	 if (!Read32(fp,&(UIR->PrevUIR))) return CRE;
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
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read32(fp,buffer)) return CRE;
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadCCR.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadCCR (vFILE *fp, Int32 offset, ...)
#else
STATICforIDL CDFstatus ReadCCR (va_alist)
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
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CRE;
	 if (!Read32(fp,&(CCR->RecordSize))) return CRE;
	 if (!Read32(fp,&(CCR->RecordType))) return CRE;
	 if (!Read32(fp,&(CCR->CPRoffset))) return CRE;
	 if (!Read32(fp,&(CCR->uSize))) return CRE;
	 if (!Read32(fp,&(CCR->rfuA))) return CRE;
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
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read32(fp,buffer)) return CRE;
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadCPR.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadCPR (vFILE *fp, Int32 offset, ...)
#else
STATICforIDL CDFstatus ReadCPR (va_alist)
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
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CRE;
	 if (!Read32(fp,&(CPR->RecordSize))) return CRE;
	 if (!Read32(fp,&(CPR->RecordType))) return CRE;
	 if (!Read32(fp,&(CPR->cType))) return CRE;
	 if (!Read32(fp,&(CPR->rfuA))) return CRE;
	 if (!Read32(fp,&(CPR->pCount))) return CRE;
	 if (CPR->pCount > CDF_MAX_PARMS) return TOO_MANY_PARMS;
	 for (i = 0; i < CPR->pCount; i++) {
	    if (!Read32(fp,&(CPR->cParms[i]))) return CRE;
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
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read32(fp,buffer)) return CRE;
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadSPR.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadSPR (vFILE *fp, Int32 offset, ...)
#else
STATICforIDL CDFstatus ReadSPR (va_alist)
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
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CRE;
	 if (!Read32(fp,&(SPR->RecordSize))) return CRE;
	 if (!Read32(fp,&(SPR->RecordType))) return CRE;
	 if (!Read32(fp,&(SPR->sArraysType))) return CRE;
	 if (!Read32(fp,&(SPR->rfuA))) return CRE;
	 if (!Read32(fp,&(SPR->pCount))) return CRE;
	 if (SPR->pCount > CDF_MAX_PARMS) return TOO_MANY_PARMS;
	 for (i = 0; i < SPR->pCount; i++) {
	    if (!Read32(fp,&(SPR->sArraysParms[i]))) return CRE;
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
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read32(fp,buffer)) return CRE;
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadCVVR.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadCVVR (vFILE *fp, Int32 offset, ...)
#else
STATICforIDL CDFstatus ReadCVVR (va_alist)
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
	 if (!SEEKv(fp,(long)offset,vSEEK_SET)) return CRE;
	 if (!Read32(fp,&(CVVR->RecordSize))) return CRE;
	 if (!Read32(fp,&(CVVR->RecordType))) return CRE;
	 if (!Read32(fp,&(CVVR->rfuA))) return CRE;
	 if (!Read32(fp,&(CVVR->cSize))) return CRE;
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
	 if (!SEEKv(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read32(fp,buffer)) return CRE;
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadADRList.
* Reads metadata into memory for future reference in READONLYon mode.
******************************************************************************/

STATICforIDL CDFstatus ReadADRList (vFILE *fp)
{
  Int32 nxtADR = fp->GDR->ADRhead;
  CDFstatus pStatus = CDF_OK;
  Int32 i;

  /**************************************************************************
  * Allocate memory for the list of ADRs
  ***************************************************************************/
  if (fp->GDR->NumAttr > 0)
  fp->ADRList = cdf_AllocateMemory((size_t)fp->GDR->NumAttr * 
                                   sizeof(struct ADRstruct**), NULL);
  if (fp->GDR->NumAttr > 0 && fp->ADRList == NULL) return BAD_MALLOC;
  for (i = 0; i < fp->GDR->NumAttr; i++)
  {
      fp->ADRList[i] = cdf_AllocateMemory((size_t)sizeof(struct ADRstruct), NULL);
      if (fp->ADRList[i] == NULL) return BAD_MALLOC;
  };
  /***************************************************************************
  * Read the ADRs into the list
  ****************************************************************************/
  for (i = 0; i < fp->GDR->NumAttr; i++)
  {
      if (!SEEKv(fp,nxtADR,vSEEK_SET)) return CRE;
      if (!Read32(fp, &(fp->ADRList[i]->RecordSize))) return CRE;
      if (!Read32(fp,&(fp->ADRList[i]->RecordType))) return CRE;
      if (!Read32(fp,&nxtADR)) return CRE;
      fp->ADRList[i]->ADRnext = 0;
      if (!Read32(fp,&(fp->ADRList[i]->AgrEDRhead))) return CRE;
      if (!Read32(fp,&(fp->ADRList[i]->Scope))) return CRE;
      if (!Read32(fp,&(fp->ADRList[i]->Num))) return CRE;
      if (!Read32(fp,&(fp->ADRList[i]->NgrEntries))) return CRE;
      if (!Read32(fp,&(fp->ADRList[i]->MAXgrEntry))) return CRE;
      if (!Read32(fp,&(fp->ADRList[i]->rfuA))) return CRE;
      if (!Read32(fp,&(fp->ADRList[i]->AzEDRhead))) return CRE;
      if (!Read32(fp,&(fp->ADRList[i]->NzEntries))) return CRE;
      if (!Read32(fp,&(fp->ADRList[i]->MAXzEntry))) return CRE;
      if (!Read32(fp,&(fp->ADRList[i]->rfuE))) return CRE;
      if (!READv(fp->ADRList[i]->Name,CDF_ATTR_NAME_LEN,1,fp))
        return CRE;
      NulPad (fp->ADRList[i]->Name, CDF_ATTR_NAME_LEN);
      /***********************************************************************
      * Allocate memory for the lists of gAEDRs and zAEDRS associated with
      * this ADR and read the AEDRs into the lists.
      ***********************************************************************/
      pStatus = ReadAEDRList(fp, &(fp->ADRList[i]->grAEDRList),
                             fp->ADRList[i]->AgrEDRhead,
                             fp->ADRList[i]->NgrEntries,
                             fp->ADRList[i]->MAXgrEntry);
      if (pStatus != CDF_OK) return pStatus;
      pStatus = ReadAEDRList(fp, &(fp->ADRList[i]->zAEDRList),
                             fp->ADRList[i]->AzEDRhead,
                             fp->ADRList[i]->NzEntries,
                             fp->ADRList[i]->MAXzEntry);
      if (pStatus != CDF_OK) return pStatus;
  };
  return pStatus;
}

/******************************************************************************
* ReadAEDRList.
* Reads the set of entrys (zENTRYs or grENTRYs) associated with an attribute
* into a list indexed by the entry number. If there is no entry for a particular
* index (i.e., there is no entry for the associated variable/attribute
* pair), the list entry is NULL.
******************************************************************************/
STATICforIDL CDFstatus ReadAEDRList (vFILE *fp, 
                                     struct AEDRstructExt ***AEDRList, 
                                     Int32 AEDRHead, 
                                     Int32 NumEntries, 
                                     Int32 MaxEntry)
{
  CDFstatus pStatus = CDF_OK;
  Int32 NxtAEDR = AEDRHead;
  struct AEDRstructExt *TempAEDR;
  int iEntries;
  int i;
  /**************************************************************************
  * Allocate memory for the list and initialize each entry to NULL.
  **************************************************************************/
  if (MaxEntry >= 0) {
      *AEDRList = cdf_AllocateMemory((size_t)(MaxEntry + 1) * 
                                     sizeof(struct AEDRstructExt**), NULL);
      if (*AEDRList == NULL) return BAD_MALLOC;
  }
  else
  {
      *AEDRList = NULL;
      return pStatus;
  };

  for (i = 0; i <= MaxEntry; i++)
  {
      (*AEDRList)[i] = NULL;
  };
  iEntries = 0;
  /***************************************************************************
  * Allocate memory for each AEDR, read each AEDR, and assign them to the list
  * by entry number.
  ***************************************************************************/
  while (NxtAEDR != 0 && iEntries != NumEntries)
  {
      TempAEDR = cdf_AllocateMemory((size_t)sizeof(struct AEDRstructExt), NULL);
      if (TempAEDR == NULL) return BAD_MALLOC;
      if (!SEEKv(fp,NxtAEDR,vSEEK_SET)) return CRE;
      if (!Read32(fp,&(TempAEDR->AEDR.RecordSize))) return CRE;
      if (!Read32(fp,&(TempAEDR->AEDR.RecordType))) return CRE;
      if (!Read32(fp,&NxtAEDR)) return CRE;
      TempAEDR->AEDR.AEDRnext = 0;
      if (!Read32(fp,&(TempAEDR->AEDR.AttrNum))) return CRE;
      if (!Read32(fp,&(TempAEDR->AEDR.DataType))) return CRE;
      if (!Read32(fp,&(TempAEDR->AEDR.Num))) return CRE;
      if (!Read32(fp,&(TempAEDR->AEDR.NumElems))) return CRE;
      if (!Read32(fp,&(TempAEDR->AEDR.rfuA))) return CRE;
      if (!Read32(fp,&(TempAEDR->AEDR.rfuB))) return CRE;
      if (!Read32(fp,&(TempAEDR->AEDR.rfuC))) return CRE;
      if (!Read32(fp,&(TempAEDR->AEDR.rfuD))) return CRE;
      if (!Read32(fp,&(TempAEDR->AEDR.rfuE))) return CRE;
      TempAEDR->ValueSize = (CDFelemSize(TempAEDR->AEDR.DataType) *
                             TempAEDR->AEDR.NumElems);
      if (TempAEDR->ValueSize < 1) return CV2C;
      TempAEDR->Value = cdf_AllocateMemory((size_t)TempAEDR->ValueSize, NULL);
      if (TempAEDR->Value == NULL) return BAD_MALLOC;
      if (!READv(TempAEDR->Value,TempAEDR->ValueSize,1,fp)) return CRE;
      if (STRINGdataType(TempAEDR->AEDR.DataType)) {
        FillSpacesToString((char *)(TempAEDR->Value),
                           (int)TempAEDR->AEDR.NumElems,
                           (int)TempAEDR->AEDR.NumElems);
      }
      ++iEntries;
      if (TempAEDR->AEDR.Num < 0 || TempAEDR->AEDR.Num > MaxEntry)
        return CV2C;
      (*AEDRList)[TempAEDR->AEDR.Num] = TempAEDR;
  };
  return pStatus;
}

