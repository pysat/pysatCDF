/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                           Read from internal records for V3 CDF.
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
*   V2.0  29-Jun-04, M Liu      Added LFS (Large File Support > 2G).
*   V3.2  21-Jun-07, D Berger   Modified ReadGDR64, ReadADR64, and ReadAEDR64, 
*                               and added ReadADRList64 and ReadAEDRList64 to 
*                               support perfomance enhancements for accessing 
*                               metadata in READONLYon mode.
*   V3.2a 11-Apr-08, M Liu      Modified Read32s_64 and Read64s_64 to 
*                               eliminate the potential buffer overflow.
*   V3.3  28-Jul-08, M Liu      Modified to remove checks for wasted space as 
*                               it is handled by cdfread.c for V2 CDF.
*   V3.3a 15-Sep-08, M Liu      Modified ReadAEDRList64 to pass in the number of
*                               entries to set up the list, not just the last
*                               entry number. 
*   V3.4  12-Aug-10, M Liu      Replaced strcpy by strcpyX in ReadVDR. 
*
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"

/******************************************************************************
* Local macro definitions.
******************************************************************************/

#define CRE CDF_READ_ERROR
#define CV3C CORRUPTED_V3_CDF

/******************************************************************************
* Read32_64.
******************************************************************************/

VISIBLE_PREFIX Logical Read32_64 (fp, value)
vFILE *fp;
Int32 *value;
{
#if defined(NETWORKbyteORDERcpu)
  if (!READv64(value,(size_t)4,(size_t)1,fp)) return FALSE;
#else
  Int32 temp;
  if (!READv64(&temp,(size_t)4,(size_t)1,fp)) return FALSE;
  REVERSE4bIO (&temp, value)
#endif
  return TRUE;
}

/******************************************************************************
* Read32s_64.
******************************************************************************/

STATICforIDL Logical Read32s_64 (fp, buffer, count)
vFILE *fp;
Int32 *buffer;
int count;
{
#define MAX_READ32s CDF_MAX_DIMS        /* This must be the maximum of
                                           CDF_MAX_DIMS and MAX_VXR_ENTRIES
                                           (and for any other uses of
                                           `Read32s'). */
#if defined(NETWORKbyteORDERcpu)
  if (count == 0) return TRUE;
  if (count < 0 || count > MAX_READ32s) return FALSE;
  if (!READv64(buffer,(size_t)4,(size_t)count,fp)) return FALSE;
#else
  int i; Int32 temp[MAX_READ32s];
  if (count == 0) return TRUE;
  if (count < 0 || count > MAX_READ32s) return FALSE;
  if (!READv64(temp,(size_t)4,(size_t)count,fp)) return FALSE;
  for (i = 0; i < count; i++) {
     REVERSE4bIO (&temp[i], &buffer[i])
  }
#endif
  return TRUE;
}

/******************************************************************************
* Read64_64.
******************************************************************************/

VISIBLE_PREFIX Logical Read64_64 (fp, value)
vFILE *fp;
OFF_T *value;
{
#if defined(NETWORKbyteORDERcpu)
  if (!READv64(value,(size_t)8,(size_t)1,fp)) return FALSE;
#else
  OFF_T temp;
  if (!READv64(&temp,(size_t)8,(size_t)1,fp)) return FALSE;
  REVERSE8bIO (&temp, value)
#endif
  return TRUE;
}

/******************************************************************************
* Read64s_64.
******************************************************************************/

STATICforIDL Logical Read64s_64 (fp, buffer, count)
vFILE *fp;
OFF_T *buffer;
int count;
{
#define MAX_READ64s CDF_MAX_DIMS        /* This must be the maximum of
                                           CDF_MAX_DIMS and MAX_VXR_ENTRIES
                                           (and for any other uses of
                                           `Read64s'). */
#if defined(NETWORKbyteORDERcpu)
  if (count == 0) return TRUE;
  if (count < 0 || count > MAX_READ64s) return FALSE;
  if (!READv64(buffer,(size_t)8,(size_t)count,fp)) return FALSE;
#else
  int i; OFF_T temp[MAX_READ64s];
  if (count == 0) return TRUE;
  if (count < 0 || count > MAX_READ64s) return FALSE;
  if (!READv64(temp,(size_t)8,(size_t)count,fp)) return FALSE;
  for (i = 0; i < count; i++) {
     REVERSE8bIO (&temp[i], &buffer[i])
  }
#endif
  return TRUE;
}

/******************************************************************************
* ReadIrSize64.
*   The size is always in the first 4-byte field.
******************************************************************************/

STATICforIDL CDFstatus ReadIrSize64 (fp, offset, irSize)
vFILE *fp;
OFF_T offset;
OFF_T *irSize;
{
  if (!SEEKv64(fp,offset,vSEEK_SET)) return CRE;
  if (!Read64_64(fp,irSize)) return CRE;
  return CDF_OK;
}  

/******************************************************************************
* ReadIrType64.
*   The type is always in the second field (4-byte) after the size.
******************************************************************************/

VISIBLE_PREFIX CDFstatus ReadIrType64 (fp, offset, irType)
vFILE *fp;
OFF_T offset;
Int32 *irType;
{
  OFF_T irTypeOffset = offset + sizeof(OFF_T);
  if (!SEEKv64(fp,irTypeOffset,vSEEK_SET)) return CRE;
  if (!Read32_64(fp,irType)) return CRE;
  return CDF_OK;
}  

/******************************************************************************
* ReadCDR64.
*   Note that the length of the CDF copyright was decreased in CDF V2.5 (there
* were way too many characters allowed for).  When reading the copyright, only
* CDF_COPYRIGHT_LEN characters will be read.  This will be less than the
* actual number in CDFs prior to CDF V2.4 but is enough to include all of the
* characters that were used.  (The value of CDF_COPYRIGHT_LEN was decreased
* from CDF V2.4 to CDF V2.5.)
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadCDR64 (vFILE *fp, OFF_T offset, ...)
#else
STATICforIDL CDFstatus ReadCDR64 (va_alist)
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
	 void *copyRight = va_arg (ap, char *);
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CRE;
	 if (!Read64_64(fp,&(CDR->RecordSize))) return CRE;
	 if (!Read32_64(fp,&(CDR->RecordType))) return CRE;
	 if (!Read64_64(fp,&(CDR->GDRoffset))) return CRE;
         /* group read for Version, Release, Encoding, Flags, rfuA, rfuB,
            Increment, rfuD, rfuE -- all 4-byte big-endian. */
         if (!Read32s_64(fp,&(CDR->Version),9)) return CRE;
         /*
  	   if (!Read32_64(fp,&(CDR->Version))) return CRE;
	   if (!Read32_64(fp,&(CDR->Release))) return CRE;
	   if (!Read32_64(fp,&(CDR->Encoding))) return CRE;
	   if (!Read32_64(fp,&(CDR->Flags))) return CRE;
	   if (!Read32_64(fp,&(CDR->rfuA))) return CRE;
  	   if (!Read32_64(fp,&(CDR->rfuB))) return CRE;
	   if (!Read32_64(fp,&(CDR->Increment))) return CRE;
	   if (!Read32_64(fp,&(CDR->rfuD))) return CRE;
	   if (!Read32_64(fp,&(CDR->rfuE))) return CRE;
         */
	 if (copyRight != NULL) {
	   if (!READv64(copyRight,CDF_COPYRIGHT_LEN,1,fp)) return CRE;
	   NulPad (copyRight, CDF_COPYRIGHT_LEN);
	 }
	 break;
       }
       case CDR_COPYRIGHT: {
	 void *copyRight = va_arg (ap, char *);
	 OFF_T tOffset = offset + (OFF_T) CDR_COPYRIGHT_OFFSET64;
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!READv64(copyRight,CDF_COPYRIGHT_LEN,1,fp)) return CRE;
	 NulPad (copyRight, CDF_COPYRIGHT_LEN);
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
         if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
         if (!Read64_64(fp,buffer)) return CRE;
         break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *);
	 OFF_T tOffset = offset;
	 switch (field) {
	   case CDR_RECORDTYPE: tOffset += (OFF_T) CDR_RECORDTYPE_OFFSET64; break;
	   case CDR_VERSION: tOffset += (OFF_T) CDR_VERSION_OFFSET64; break;
	   case CDR_RELEASE: tOffset += (OFF_T) CDR_RELEASE_OFFSET64; break;
	   case CDR_ENCODING: tOffset += (OFF_T) CDR_ENCODING_OFFSET64; break;
	   case CDR_FLAGS: tOffset += (OFF_T) CDR_FLAGS_OFFSET64; break;
	   case CDR_INCREMENT: tOffset += (OFF_T) CDR_INCREMENT_OFFSET64; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read32_64(fp,buffer)) return CRE;
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadGDR64.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadGDR64 (vFILE *fp, OFF_T offset, ...)
#else
STATICforIDL CDFstatus ReadGDR64 (va_alist)
va_dcl
#endif
{
  long read_only_mode;
  int i;
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; OFF_T offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, OFF_T);
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
  if (read_only_mode == READONLYon && fp->GDR64 == NULL)
  {
      fp->GDR64 = cdf_AllocateMemory((size_t)sizeof(struct GDRstruct64), NULL);
      if (fp->GDR64 == NULL) return CRE;
      if (!SEEKv64(fp,offset,vSEEK_SET)) return CRE;
      if (!Read64_64(fp,&(fp->GDR64->RecordSize))) return CRE;
      if (!Read32_64(fp,&(fp->GDR64->RecordType))) return CRE;
      /* group read for rVDRhead, zVDRhead, ADRhead, eof -- all 8-byte
         big-endian */
      if (!Read64s_64(fp,&(fp->GDR64->rVDRhead),4)) return CRE;
      /*
        if (!Read64_64(fp,&(fp->GDR64->rVDRhead))) return CRE;
        if (!Read64_64(fp,&(fp->GDR64->zVDRhead))) return CRE;
        if (!Read64_64(fp,&(fp->GDR64->ADRhead))) return CRE;
        if (!Read64_64(fp,&(fp->GDR64->eof))) return CRE;
      */
      /* group read for NrVars, NumAttr, rMaxRec, rNumDims, NzVars -- all
         4-byte big-endian */
      if (!Read32s_64(fp,&(fp->GDR64->NrVars),5)) return CRE;
      /*
        if (!Read32_64(fp,&(fp->GDR64->NrVars))) return CRE;
        if (!Read32_64(fp,&(fp->GDR64->NumAttr))) return CRE;
        if (!Read32_64(fp,&(fp->GDR64->rMaxRec))) return CRE;
        if (!Read32_64(fp,&(fp->GDR64->rNumDims))) return CRE;
        if (!Read32_64(fp,&(fp->GDR64->NzVars))) return CRE;
      */
      if (!Read64_64(fp,&(fp->GDR64->UIRhead))) return CRE;
      /* group read for rfuC, LeapSecondLastUpdated, rfuE -- all 4-byte
         big-endian */
      if (!Read32s_64(fp,&(fp->GDR64->rfuC),3)) return CRE;
      /*
        if (!Read32_64(fp,&(fp->GDR64->rfuC))) return CRE;
        if (!Read32_64(fp,&(fp->GDR64->LeapSecondLastUpdated))) return CRE;
        if (!Read32_64(fp,&(fp->GDR64->rfuE))) return CRE;
      */
      if (fp->GDR64->rNumDims < 0 ||
          fp->GDR64->rNumDims > CDF_MAX_DIMS) return CV3C; 
      if (!Read32s_64(fp,fp->GDR64->rDimSizes,
                      (int)fp->GDR64->rNumDims)) return CRE;
      pStatus = ReadADRList64(fp);
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
	 struct GDRstruct64 *GDR = va_arg (ap, struct GDRstruct64 *);
         if (read_only_mode == READONLYoff)
         {
	     if (!SEEKv64(fp,offset,vSEEK_SET)) return CRE;
	     if (!Read64_64(fp,&(GDR->RecordSize))) return CRE;
	     if (!Read32_64(fp,&(GDR->RecordType))) return CRE;
             if (!Read64s_64(fp,&(GDR->rVDRhead),4)) return CRE;
             /*
	       if (!Read64_64(fp,&(GDR->rVDRhead))) return CRE;
	       if (!Read64_64(fp,&(GDR->zVDRhead))) return CRE;
	       if (!Read64_64(fp,&(GDR->ADRhead))) return CRE;
	       if (!Read64_64(fp,&(GDR->eof))) return CRE;
             */
             if (!Read32s_64(fp,&(GDR->NrVars),5)) return CRE;
             /*
	       if (!Read32_64(fp,&(GDR->NrVars))) return CRE;
	       if (!Read32_64(fp,&(GDR->NumAttr))) return CRE;
	       if (!Read32_64(fp,&(GDR->rMaxRec))) return CRE;
	       if (!Read32_64(fp,&(GDR->rNumDims))) return CRE;
	       if (!Read32_64(fp,&(GDR->NzVars))) return CRE;
             */
	     if (!Read64_64(fp,&(GDR->UIRhead))) return CRE;
             if (!Read32s_64(fp,&(GDR->rfuC),3)) return CRE;
             /*
	       if (!Read32_64(fp,&(GDR->rfuC))) return CRE;
	       if (!Read32_64(fp,&(GDR->LeapSecondLastUpdated))) return CRE;
	       if (!Read32_64(fp,&(GDR->rfuE))) return CRE;
             */
             if (GDR->rNumDims < 0 ||
                 GDR->rNumDims > CDF_MAX_DIMS) break; /* return CV3C; */
	     if (!Read32s_64(fp,GDR->rDimSizes,(int)GDR->rNumDims)) return CRE;
         }
         else
         {
	     GDR->RecordSize = fp->GDR64->RecordSize;
	     GDR->RecordType = fp->GDR64->RecordType;
	     GDR->rVDRhead = fp->GDR64->rVDRhead;
	     GDR->zVDRhead = fp->GDR64->zVDRhead;
	     GDR->ADRhead = fp->GDR64->ADRhead;
	     GDR->eof = fp->GDR64->eof;
	     GDR->NrVars = fp->GDR64->NrVars;
	     GDR->NumAttr = fp->GDR64->NumAttr;
	     GDR->rMaxRec = fp->GDR64->rMaxRec;
	     GDR->rNumDims = fp->GDR64->rNumDims;
	     GDR->NzVars = fp->GDR64->NzVars;
	     GDR->UIRhead = fp->GDR64->UIRhead;
	     GDR->rfuC = fp->GDR64->rfuC;
	     GDR->LeapSecondLastUpdated = fp->GDR64->LeapSecondLastUpdated;
	     GDR->rfuE = fp->GDR64->rfuE;
             for (i = 0; i < GDR->rNumDims; i++)
             {
                 GDR->rDimSizes[i] = fp->GDR64->rDimSizes[i];
             };
         };
         break;
       }
       case GDR_rDIMSIZES: {
         Int32 *rDimSizes = va_arg (ap, Int32 *); 
         if (read_only_mode == READONLYoff)
         {
             Int32 rNumDims; 
             OFF_T tOffset;
	     if (!sX(ReadGDR64(fp,offset,
	                       GDR_rNUMDIMS,&rNumDims,
			       GDR_NULL),&pStatus)) return pStatus;
	     tOffset = offset + (OFF_T) GDR_rDIMSIZES_OFFSET64;
	     if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
             if (rNumDims < 0 || rNumDims > CDF_MAX_DIMS)
               return CV3C;
	     if (!Read32s_64(fp,rDimSizes,(int)rNumDims)) return CRE;
         }
         else
         {
             for (i = 0; i < fp->GDR64->rNumDims; i++)
             {
                 rDimSizes[i] = fp->GDR64->rDimSizes[i];
             };
         }
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
           case GDR_RECORDSIZE: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += (OFF_T) GDR_RECORDSIZE_OFFSET64; break;
               }
               else
               {
                   *buffer = fp->GDR64->RecordSize; 
               };
               break;
           };
           case GDR_rVDRHEAD: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += (OFF_T) GDR_rVDRHEAD_OFFSET64;
               }
               else
               {
                   *buffer = fp->GDR64->rVDRhead; 
               };
               break;
           };
	   case GDR_zVDRHEAD: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += (OFF_T) GDR_zVDRHEAD_OFFSET64;
               }
               else
               {
                   *buffer = fp->GDR64->zVDRhead; 
               };
               break;
           };
	   case GDR_ADRHEAD: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += (OFF_T) GDR_ADRHEAD_OFFSET64;
               }
               else
               {
                   *buffer = fp->GDR64->ADRhead; 
               };
               break;
           }; 
	   case GDR_EOF: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += (OFF_T) GDR_EOF_OFFSET64;
               }
               else
               {
                   *buffer = fp->GDR64->eof; 
               };
               break;
           };
	   case GDR_UIRHEAD: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += (OFF_T) GDR_UIRHEAD_OFFSET64; break;
               }
               else
               {
                   *buffer = fp->GDR64->UIRhead; 
               };
               break;
           };
	   default: return CDF_INTERNAL_ERROR;
         }
         if (read_only_mode == READONLYoff)
         {
             if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
             if (!Read64_64(fp,buffer)) return CRE;
         }
         break;  
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
	 switch (field) {
	   case GDR_RECORDTYPE: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += (OFF_T) GDR_RECORDTYPE_OFFSET64;
               }
               else
               {
                   *buffer = fp->GDR64->RecordType; 
               };
               break;
           };
	   case GDR_NrVARS: { 
               if (read_only_mode == READONLYoff)
               {
                   tOffset += (OFF_T) GDR_NrVARS_OFFSET64;
               }
               else
               {
                   *buffer = fp->GDR64->NrVars; 
               };
               break;
           };
	   case GDR_NUMATTR: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset +=(OFF_T) GDR_NUMATTR_OFFSET64;
               }
               else
               {
                   *buffer = fp->GDR64->NumAttr; 
               };
               break;
           };
	   case GDR_rMAXREC: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset +=(OFF_T) GDR_rMAXREC_OFFSET64;
               }
               else
               {
                   *buffer = fp->GDR64->rMaxRec;
               };
               break;
           };
	   case GDR_rNUMDIMS: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset +=(OFF_T) GDR_rNUMDIMS_OFFSET64;
               }
               else
               {
                   *buffer = fp->GDR64->rNumDims; 
               };
               break;
           };
	   case GDR_NzVARS: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset +=(OFF_T) GDR_NzVARS_OFFSET64; break;
               }
               else
               {
                   *buffer = fp->GDR64->NzVars; 
               };
               break;
           };
	   case GDR_LEAPSECONDLASTUPDATED: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset +=(OFF_T) GDR_LEAPSECONDLASTUPDATED_OFFSET64; break;
               }
               else
               {
                   *buffer = fp->GDR64->LeapSecondLastUpdated; 
               };
               break;
           };
	   default: return CDF_INTERNAL_ERROR;
	 }
         if (read_only_mode == READONLYoff)
         {
             if (!SEEKv64(fp,tOffset,vSEEK_SET)) {
                 return CRE;
             };
             if (!Read32_64(fp,buffer)) {
                 return CRE;
             };
         }
         break;
       }
     }
  }
}

/******************************************************************************
* ReadADR64.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadADR64 (vFILE *fp, OFF_T offset, ...)
#else
STATICforIDL CDFstatus ReadADR64 (va_alist)
va_dcl
#endif
{
  long read_only_mode;
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; OFF_T offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, OFF_T);
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
         struct ADRstruct64 *ADR = va_arg (ap, struct ADRstruct64 *);
         if (read_only_mode == READONLYoff)
         {
	     if (!SEEKv64(fp,offset,vSEEK_SET)) return CRE;
             if (!Read64_64(fp,&(ADR->RecordSize))) return CRE;
             if (!Read32_64(fp,&(ADR->RecordType))) return CRE;
             if (!Read64s_64(fp,&(ADR->ADRnext),2)) return CRE;
             /*
               if (!Read64_64(fp,&(ADR->ADRnext))) return CRE;
               if (!Read64_64(fp,&(ADR->AgrEDRhead))) return CRE;
             */
             if (!Read32s_64(fp,&(ADR->Scope),5)) return CRE;
             /*
               if (!Read32_64(fp,&(ADR->Scope))) return CRE;
               if (!Read32_64(fp,&(ADR->Num))) return CRE;
               if (!Read32_64(fp,&(ADR->NgrEntries))) return CRE;
               if (!Read32_64(fp,&(ADR->MAXgrEntry))) return CRE;
               if (!Read32_64(fp,&(ADR->rfuA))) return CRE;
             */
             if (!Read64_64(fp,&(ADR->AzEDRhead))) return CRE;
             if (!Read32s_64(fp,&(ADR->NzEntries),3)) return CRE;
             /*
               if (!Read32_64(fp,&(ADR->NzEntries))) return CRE;
               if (!Read32_64(fp,&(ADR->MAXzEntry))) return CRE;
               if (!Read32_64(fp,&(ADR->rfuE))) return CRE;
             */
             if (!READv64(ADR->Name,CDF_ATTR_NAME_LEN256,1,fp)) return CRE;
             NulPad (ADR->Name, CDF_ATTR_NAME_LEN256);
         }
         else
         {
             ADR->RecordSize = fp->ADRList64[fp->CurADRIndex]->RecordSize;
             ADR->RecordType = fp->ADRList64[fp->CurADRIndex]->RecordType;
             ADR->ADRnext = fp->ADRList64[fp->CurADRIndex]->ADRnext;
             ADR->AgrEDRhead = fp->ADRList64[fp->CurADRIndex]->AgrEDRhead;
             ADR->Scope = fp->ADRList64[fp->CurADRIndex]->Scope;
             ADR->Num = fp->ADRList64[fp->CurADRIndex]->Num;
             ADR->NgrEntries = fp->ADRList64[fp->CurADRIndex]->NgrEntries;
             ADR->MAXgrEntry = fp->ADRList64[fp->CurADRIndex]->MAXgrEntry;
             ADR->rfuA = fp->ADRList64[fp->CurADRIndex]->rfuA;
             ADR->AzEDRhead = fp->ADRList64[fp->CurADRIndex]->AzEDRhead;
             ADR->NzEntries = fp->ADRList64[fp->CurADRIndex]->NzEntries;
             ADR->MAXzEntry = fp->ADRList64[fp->CurADRIndex]->MAXzEntry;
             ADR->rfuE = fp->ADRList64[fp->CurADRIndex]->rfuE;
             strcpyX (ADR->Name, fp->ADRList64[fp->CurADRIndex]->Name,
                      CDF_ATTR_NAME_LEN256);
         };
         break;
       };
       case ADR_NAME: {
	 char *aName = va_arg (ap, char *);
         if (read_only_mode == READONLYoff) {
	     OFF_T tOffset = offset + ADR_NAME_OFFSET64;
	     if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
	     if (!READv64(aName,CDF_ATTR_NAME_LEN256,1,fp)) return CRE;
	     NulPad (aName, CDF_ATTR_NAME_LEN256);
         }
         else {
             strcpyX (aName, fp->ADRList64[fp->CurADRIndex]->Name,
                      CDF_ATTR_NAME_LEN256);
         }
	 break;
       }
       case ADR_RECORDSIZE: 
       case ADR_ADRNEXT:
       case ADR_AgrEDRHEAD:
       case ADR_AzEDRHEAD: {
         OFF_T *buffer = va_arg (ap, OFF_T *); OFF_T tOffset = offset;
         switch (field) { 
           case ADR_RECORDSIZE: {
               if (read_only_mode == READONLYoff) {
                   tOffset += (OFF_T) ADR_RECORDSIZE_OFFSET64;
               }
               else {
                   *buffer = fp->ADRList64[fp->CurADRIndex]->RecordSize;
               }
               break;
           }
           case ADR_ADRNEXT: {
               if (read_only_mode == READONLYoff) {
                   tOffset += (OFF_T) ADR_ADRNEXT_OFFSET64;
               }
               else {
                   *buffer = fp->ADRList64[fp->CurADRIndex]->ADRnext;
               }
               break;
           }
           case ADR_AgrEDRHEAD: {
               if (read_only_mode == READONLYoff) {
                   tOffset += (OFF_T) ADR_AgrEDRHEAD_OFFSET64;
               }
               else {
                   *buffer = fp->ADRList64[fp->CurADRIndex]->AgrEDRhead;
               };
               break;
           }
           case ADR_AzEDRHEAD: {
               if (read_only_mode == READONLYoff) {
                   tOffset += (OFF_T) ADR_AzEDRHEAD_OFFSET64;
               }
               else {
                   *buffer = fp->ADRList64[fp->CurADRIndex]->AzEDRhead;
               }
               break;
           }
           default: return CDF_INTERNAL_ERROR;
         }    
         if (read_only_mode == READONLYoff) {
             if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
             if (!Read64_64(fp,buffer)) return CRE;
         };
         break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
	 switch (field) {
	   case ADR_RECORDTYPE: {
               if (read_only_mode == READONLYoff) {
                   tOffset += (OFF_T) ADR_RECORDTYPE_OFFSET64;
               }
               else {
                   *buffer = fp->ADRList64[fp->CurADRIndex]->RecordType;
               }
               break;
           }
	   case ADR_SCOPE: {
               if (read_only_mode == READONLYoff) {
                   tOffset += (OFF_T) ADR_SCOPE_OFFSET64;
               }
               else {
                   *buffer = fp->ADRList64[fp->CurADRIndex]->Scope;
               }
               break;
           }
	   case ADR_NUM: {
               if (read_only_mode == READONLYoff) {
                   tOffset += (OFF_T) ADR_NUM_OFFSET64;
               }
               else {
                   *buffer = fp->ADRList64[fp->CurADRIndex]->Num;
               }
               break;
           }
	   case ADR_NgrENTRIES: {
               if (read_only_mode == READONLYoff) {
                   tOffset += (OFF_T) ADR_NgrENTRIES_OFFSET64;
               }
               else {
                   *buffer = fp->ADRList64[fp->CurADRIndex]->NgrEntries;
               }
               break;
           }    
	   case ADR_MAXgrENTRY: {
               if (read_only_mode == READONLYoff) {
                   tOffset += (OFF_T) ADR_MAXgrENTRY_OFFSET64;
               }
               else {
                   *buffer = fp->ADRList64[fp->CurADRIndex]->MAXgrEntry;
               }
               break;
           }
	   case ADR_NzENTRIES: {
               if (read_only_mode == READONLYoff) {
                   tOffset += (OFF_T) ADR_NzENTRIES_OFFSET64;
               }
               else {
                   *buffer = fp->ADRList64[fp->CurADRIndex]->NzEntries;
               }
               break;
           }
	   case ADR_MAXzENTRY: {
               if (read_only_mode == READONLYoff) {
                   tOffset += (OFF_T) ADR_MAXzENTRY_OFFSET64;
               }
               else {
                   *buffer = fp->ADRList64[fp->CurADRIndex]->MAXzEntry;
               }
               break;
           }
	   default: return CDF_INTERNAL_ERROR;
	 }
         if (read_only_mode == READONLYoff) {
             if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
	     if (!Read32_64(fp,buffer)) return CRE;
         }
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadAEDR64/AzEDR64.
*   If the entry value is being read, it is passed back in the encoding of the
* CDF (no decoding is performed).  The caller must decode the value (if that
* is necessary).
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadAEDR64 (vFILE *fp, OFF_T offset, ...)
#else
STATICforIDL CDFstatus ReadAEDR64 (va_alist)
va_dcl
#endif
{
  long read_only_mode;
  struct AEDRstructExt64 *CurAEDR;
  va_list ap; CDFstatus pStatus = CDF_OK;
#if defined(STDARG)
  va_start (ap, offset);
#else
  vFILE *fp; OFF_T offset;
  VA_START (ap);
  fp = va_arg (ap, vFILE *);
  offset = va_arg (ap, OFF_T);
#endif
  pStatus = CDFlib(CONFIRM_, CDF_READONLY_MODE_, &read_only_mode, NULL_);
  if (pStatus != CDF_OK) return pStatus;
  /***************************************************************************
  * If READONLYon and either an attribute or an entry have not been selected,
  * return the appropriate status. If they have been selected but there is no
  * entry for the selected attribute, return BAD_ENTRY_NUM.  Note that for each
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
          if (fp->ADRList64[fp->CurADRIndex]->zAEDRList64[fp->CurAEDRIndex] 
              != NULL)
          {
              CurAEDR =
                  fp->ADRList64[fp->CurADRIndex]->zAEDRList64[fp->CurAEDRIndex];
          }
          else
          {
              return NO_SUCH_ENTRY;
          };
      }
      else
      {
          if (fp->ADRList64[fp->CurADRIndex]->grAEDRList64[fp->CurAEDRIndex] !=
              NULL)
          {
              CurAEDR =
                 fp->ADRList64[fp->CurADRIndex]->grAEDRList64[fp->CurAEDRIndex];
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
         struct AEDRstruct64 *AEDR = va_arg (ap, struct AEDRstruct64 *);
         void *value = va_arg (ap, void *); size_t nBytes;
         if (read_only_mode == READONLYoff)
         {
             if (!SEEKv64(fp,offset,vSEEK_SET)) return CRE;
             if (!Read64_64(fp,&(AEDR->RecordSize))) return CRE;
             if (!Read32_64(fp,&(AEDR->RecordType))) return CRE;
             if (!Read64_64(fp,&(AEDR->AEDRnext))) return CRE;
             if (!Read32s_64(fp,&(AEDR->AttrNum),9)) return CRE;
             /*
               if (!Read32_64(fp,&(AEDR->AttrNum))) return CRE;
               if (!Read32_64(fp,&(AEDR->DataType))) return CRE;
               if (!Read32_64(fp,&(AEDR->Num))) return CRE;
               if (!Read32_64(fp,&(AEDR->NumElems))) return CRE;
               if (!Read32_64(fp,&(AEDR->rfuA))) return CRE;
               if (!Read32_64(fp,&(AEDR->rfuB))) return CRE;
               if (!Read32_64(fp,&(AEDR->rfuC))) return CRE;
               if (!Read32_64(fp,&(AEDR->rfuD))) return CRE;
               if (!Read32_64(fp,&(AEDR->rfuE))) return CRE;
             */
             if (value != NULL) {
               nBytes = (size_t) (CDFelemSize(AEDR->DataType) *
                                  AEDR->NumElems);
               if (!READv64(value,nBytes,1,fp)) return CRE;
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
	     size_t nBytes; Int32 dataType, numElems; OFF_T tOffset;
             if (!sX(ReadAEDR64(fp,offset,
                                AEDR_DATATYPE,&dataType,
                                AEDR_NUMELEMS,&numElems,
                                AEDR_NULL),&pStatus)) return pStatus;
             tOffset = offset + (OFF_T) AEDR_VALUE_OFFSET64;
	     if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
	     nBytes = (size_t) (CDFelemSize(dataType) * numElems);
	     if (!READv64(value,nBytes,1,fp)) return CRE;
             if (STRINGdataType(dataType)) {
               FillSpacesToString((char *)value, (int)numElems, (int)numElems);
             }
         }
         else
         {
             memcpy(value, CurAEDR->Value, CurAEDR->ValueSize);
         }
	 break;
       }
       case AEDR_RECORDSIZE:
       case AEDR_AEDRNEXT: {
         OFF_T *buffer = va_arg (ap, OFF_T *); OFF_T tOffset = offset;
         switch (field) { 
           case AEDR_RECORDSIZE: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += (OFF_T) AEDR_RECORDSIZE_OFFSET64;
               }
               else
               {
                   *buffer = CurAEDR->AEDR.RecordSize;
               };
               break;
           };
           case AEDR_AEDRNEXT: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += (OFF_T) AEDR_AEDRNEXT_OFFSET64;
               }
               else
               {
                   *buffer = CurAEDR->AEDR.AEDRnext;
               }
               break;
           }
	   default: return CDF_INTERNAL_ERROR;
         }
         if (read_only_mode == READONLYoff)
         {
             if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
             if (!Read64_64(fp,buffer)) return CRE;
         };
         break;  
       } 
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
	 switch (field) {
	   case AEDR_RECORDTYPE: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += (OFF_T) AEDR_RECORDTYPE_OFFSET64;
               }
               else
               {
                   *buffer = CurAEDR->AEDR.RecordType;
               }
               break;
           }
	   case AEDR_ATTRNUM: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += (OFF_T) AEDR_ATTRNUM_OFFSET64;
               }
               else
               {
                   *buffer = CurAEDR->AEDR.AttrNum;
               }
               break;
           }
	   case AEDR_DATATYPE: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += (OFF_T) AEDR_DATATYPE_OFFSET64;
               }
               else
               {
                   *buffer = CurAEDR->AEDR.DataType;
               }
               break;
           }
	   case AEDR_NUM: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += (OFF_T) AEDR_NUM_OFFSET64;
               }
               else
               {
                   *buffer = CurAEDR->AEDR.Num;
               }
               break;
           }
	   case AEDR_NUMELEMS: {
               if (read_only_mode == READONLYoff)
               {
                   tOffset += (OFF_T) AEDR_NUMELEMS_OFFSET64; break;
               }
               else
               {
                   *buffer = CurAEDR->AEDR.NumElems;
               }
               break;
           }
	   default: return CDF_INTERNAL_ERROR;
	 }
         if (read_only_mode == READONLYoff)
         {
             if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
             if (!Read32_64(fp,buffer)) return CRE;
         };
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadVDR/zVDR64.
*   If the pad value is being read, it is passed back in the encoding of the
* CDF (no decoding is performed).  The caller must decode the value (if that
* is necessary).
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadVDR64 (struct CDFstruct *CDF, vFILE *fp,
				  OFF_T offset, Logical zVar, ...)
#else
STATICforIDL CDFstatus ReadVDR64 (va_alist)
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
	 void *padValue = va_arg (ap, void *); Int32 nDims;
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CRE;
	 if (!Read64_64(fp,&(VDR->RecordSize))) return CRE;
	 if (!Read32_64(fp,&(VDR->RecordType))) return CRE;
	 if (!Read64_64(fp,&(VDR->VDRnext))) return CRE;
         if (!Read32s_64(fp,&(VDR->DataType),2)) return CRE;
         /*
	   if (!Read32_64(fp,&(VDR->DataType))) return CRE;
	   if (!Read32_64(fp,&(VDR->MaxRec))) return CRE;
         */
         if (!Read64s_64(fp,&(VDR->VXRhead),2)) return CRE;
         /*
	   if (!Read64_64(fp,&(VDR->VXRhead))) return CRE;
	   if (!Read64_64(fp,&(VDR->VXRtail))) return CRE;
         */
         if (!Read32s_64(fp,&(VDR->Flags),7)) return CRE;
         /*
	   if (!Read32_64(fp,&(VDR->Flags))) return CRE;
	   if (!Read32_64(fp,&(VDR->sRecords))) return CRE;
	   if (!Read32_64(fp,&(VDR->rfuB))) return CRE;
	   if (!Read32_64(fp,&(VDR->rfuC))) return CRE;
	   if (!Read32_64(fp,&(VDR->rfuF))) return CRE;
	   if (!Read32_64(fp,&(VDR->NumElems))) return CRE;
	   if (!Read32_64(fp,&(VDR->Num))) return CRE;
         */
	 if (!Read64_64(fp,&(VDR->CPRorSPRoffset))) return CRE;
	 if (!Read32_64(fp,&(VDR->blockingFactor))) return CRE;
	 if (!READv64(VDR->Name,CDF_VAR_NAME_LEN256,1,fp)) return CRE;
	 NulPad (VDR->Name, CDF_VAR_NAME_LEN256);
	 if (zVar) {
	   if (!Read32_64(fp,&(VDR->zNumDims))) return CRE;
           if (VDR->zNumDims < 0 || VDR->zNumDims > CDF_MAX_DIMS)
             return CV3C;
	   if (!Read32s_64(fp,VDR->zDimSizes,(int)VDR->zNumDims)) return CRE;
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
           if (nDims < 0 || nDims > CDF_MAX_DIMS)
             return CV3C;
	   if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
	 }
	 if (!Read32s_64(fp,VDR->DimVarys,(int)nDims)) return CRE;
	 if (PADvalueBITset(VDR->Flags) && padValue != NULL) {
	   size_t nBytes = (size_t) (CDFelemSize(VDR->DataType)*VDR->NumElems);
	   if (!READv64(padValue,nBytes,1,fp)) return CRE;
           if (STRINGdataType(VDR->DataType)) {
             FillSpacesToString((char *)padValue, (int)VDR->NumElems,
                                (int)VDR->NumElems);
           }
	 }
	 break;
       }
       case VDR_NAME: {
	 char *vName = va_arg (ap, char *);
	 OFF_T tOffset = offset + VDR_NAME_OFFSET64;
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!READv64(vName,CDF_VAR_NAME_LEN256,1,fp)) return CRE;
	 NulPad (vName, CDF_VAR_NAME_LEN256);
	 break;
       }
       case VDR_zNUMDIMS: {
	 Int32 *numDims = va_arg (ap, Int32 *);
	 OFF_T tOffset = offset + zVDR_zNUMDIMS_OFFSET64;
	 if (!zVar) return CDF_INTERNAL_ERROR;
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read32_64(fp,numDims)) return CRE;
	 break;
       }
       case VDR_zDIMSIZES: {
	 Int32 *zDimSizes = va_arg (ap, Int32 *);
	 Int32 zNumDims; OFF_T tOffset;
	 if (!zVar) return CDF_INTERNAL_ERROR;
	 if (!sX(ReadVDR64(CDF,fp,offset,zVar,
			   VDR_zNUMDIMS,&zNumDims,
			   VDR_NULL),&pStatus)) return pStatus;
	 tOffset = offset + (OFF_T) zVDR_zDIMSIZES_OFFSET64;
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
         if (zNumDims < 0 || zNumDims > CDF_MAX_DIMS)
           return CV3C;
	 if (!Read32s_64(fp,zDimSizes,(int)zNumDims)) return CRE;
	 break;
       }
       case VDR_DIMVARYS: {
	 Int32 *dimVarys = va_arg (ap, Int32 *);
	 Int32 nDims; OFF_T tOffset;
	 if (zVar) {
	   if (!sX(ReadVDR64(CDF,fp,offset,zVar,
			    VDR_zNUMDIMS,&nDims,
			    VDR_NULL),&pStatus)) return pStatus;
	   tOffset = offset + (OFF_T) (zVDR_DIMVARYS_OFFSETb64 +
				       (nDims*sizeof(Int32)));
	 }
	 else {
	   OFF_T GDRoffset;
	   if (!sX(ReadCDR64(fp,V3_CDR_OFFSET64,
			     CDR_GDROFFSET,&GDRoffset,
			     CDR_NULL),&pStatus)) return pStatus;
	   if (!sX(ReadGDR64(fp,GDRoffset,
			     GDR_rNUMDIMS,&nDims,
			     GDR_NULL),&pStatus)) return pStatus;
	   tOffset = offset + (OFF_T) rVDR_DIMVARYS_OFFSET64;
	 }
         if (nDims < 0 || nDims > CDF_MAX_DIMS)
           return CV3C;
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read32s_64(fp,dimVarys,(int)nDims)) return CRE;
	 break;
       }
       case VDR_PADVALUE: {
	 void *padValue = va_arg (ap, void *);
	 Int32 dataType, numElems; size_t nBytes; OFF_T tOffset;
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
		     (zNumDims*sizeof(Int32)) +
		     (zNumDims*sizeof(Int32)));
	 }
	 else {
	   Int32 rNumDims;
	   OFF_T GDRoffset;
	   if (!sX(ReadCDR64(fp,V3_CDR_OFFSET64,
			     CDR_GDROFFSET,&GDRoffset,
			     CDR_NULL),&pStatus)) return pStatus;
	   if (!sX(ReadGDR64(fp,GDRoffset,
			     GDR_rNUMDIMS,&rNumDims,
			     GDR_NULL),&pStatus)) return pStatus;
	   tOffset = offset + (OFF_T) (rVDR_PADVALUE_OFFSETb64 +
		     (rNumDims*sizeof(Int32)));
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
	 nBytes = (size_t) (CDFelemSize(dataType) * numElems);
	 if (!READv64(padValue,nBytes,1,fp)) return CRE;
         if (STRINGdataType(dataType)) {
           FillSpacesToString((char *)padValue, (int)numElems,
                              (int)numElems);
         }
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
           case VDR_CPRorSPR: 
		tOffset += (OFF_T) VDR_CPRorSPR_OFFSET64;
		break; 
	   default: return CDF_INTERNAL_ERROR;
         }
         if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
         if (!Read64_64(fp,buffer)) return CRE;
         break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
	 switch (field) {
	   case VDR_RECORDTYPE: tOffset += (OFF_T) VDR_RECORDTYPE_OFFSET64; break;
	   case VDR_DATATYPE: tOffset += (OFF_T) VDR_DATATYPE_OFFSET64; break;
	   case VDR_MAXREC: tOffset += (OFF_T) VDR_MAXREC_OFFSET64; break;
	   case VDR_FLAGS: tOffset += (OFF_T) VDR_FLAGS_OFFSET64; break;
	   case VDR_sRECORDS: tOffset += (OFF_T) VDR_sRECORDS_OFFSET64; break;
	   case VDR_NUMELEMS:
	     tOffset += (OFF_T) VDR_NUMELEMS_OFFSET64;
	     break;
	   case VDR_NUM:
	     tOffset += (OFF_T) VDR_NUM_OFFSET64;
	     break;
	   case VDR_BLOCKING:
	     tOffset += (OFF_T) VDR_BLOCKING_OFFSET64;
	     break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read32_64(fp,buffer)) return CRE;
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadVXR64.
******************************************************************************/

#if defined(STDARG)
VISIBLE_PREFIX CDFstatus ReadVXR64 (vFILE *fp, OFF_T offset, ...)
#else
VISIBLE_PREFIX CDFstatus ReadVXR64 (va_alist)
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
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CRE;
	 if (!Read64_64(fp,&(VXR->RecordSize))) return CRE;
	 if (!Read32_64(fp,&(VXR->RecordType))) return CRE;
	 if (!Read64_64(fp,&(VXR->VXRnext))) return CRE;
         if (!Read32s_64(fp,&(VXR->Nentries),2)) return CRE;
         /*
	   if (!Read32_64(fp,&(VXR->Nentries))) return CRE;
	   if (!Read32_64(fp,&(VXR->NusedEntries))) return CRE;
         */
         if ((int)VXR->Nentries < 0 || (int)VXR->Nentries > MAX_VXR_ENTRIES)
           return CV3C;
	 if (!Read32s_64(fp,VXR->First,(int)VXR->Nentries)) return CRE;
	 if (!Read32s_64(fp,VXR->Last,(int)VXR->Nentries)) return CRE;
	 if (!Read64s_64(fp,VXR->Offset,(int)VXR->Nentries)) return CRE;
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
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
         if ((int)nEntries < 0 || nEntries > MAX_VXR_ENTRIES)
           return CV3C;
	 if (!Read32s_64(fp,buffer,(int)nEntries)) return CRE;
	 break;
       }
       case VXR_OFFSET: {
         OFF_T *buffer = va_arg (ap, OFF_T *); Int32 nEntries;
         OFF_T tOffset = offset + (OFF_T) VXR_FIRSTREC_OFFSET64;
         if (!sX(ReadVXR64(fp,offset,
                           VXR_NENTRIES,&nEntries,
                           VXR_NULL),&pStatus)) return pStatus;
         tOffset += (OFF_T) (2 * nEntries * sizeof(Int32)); 
         if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
         if ((int)nEntries < 0 || nEntries > MAX_VXR_ENTRIES)
           return CV3C;
         if (!Read64s_64(fp,buffer,(int)nEntries)) return CRE;
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
         if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
         if (!Read64_64(fp,buffer)) return CRE;
         break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
	 switch (field) {
	   case VXR_RECORDTYPE: tOffset += (OFF_T) VXR_RECORDTYPE_OFFSET64; break;
	   case VXR_NENTRIES: tOffset += (OFF_T) VXR_NENTRIES_OFFSET64; break;
	   case VXR_NUSEDENTRIES: tOffset += (OFF_T) VXR_NUSEDENTRIES_OFFSET64; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read32_64(fp,buffer)) return CRE;
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadVVR64.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadVVR64 (vFILE *fp, OFF_T offset, ...)
#else
STATICforIDL CDFstatus ReadVVR64 (va_alist)
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
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CRE;
	 if (!Read64_64(fp,&(VVR->RecordSize))) return CRE;
	 if (!Read32_64(fp,&(VVR->RecordType))) return CRE;
	 break;
       }
       case VVR_RECORDSIZE: {
	 OFF_T *buffer = va_arg (ap, OFF_T *); OFF_T tOffset = offset;
         tOffset += (OFF_T) VVR_RECORDSIZE_OFFSET64;
         if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
         if (!Read64_64(fp,buffer)) return CRE;
         break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
	 switch (field) {
	   case VVR_RECORDTYPE: tOffset += (OFF_T) VVR_RECORDTYPE_OFFSET64; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read32_64(fp,buffer)) return CRE;
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadUIR64.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadUIR64 (vFILE *fp, OFF_T offset, ...)
#else
STATICforIDL CDFstatus ReadUIR64 (va_alist)
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
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CRE;
	 if (!Read64_64(fp,&(UIR->RecordSize))) return CRE;
	 if (!Read32_64(fp,&(UIR->RecordType))) return CRE;
         if (!Read64s_64(fp,&(UIR->NextUIR),2)) return CRE;
         /*
	   if (!Read64_64(fp,&(UIR->NextUIR))) return CRE;
	   if (!Read64_64(fp,&(UIR->PrevUIR))) return CRE;
         */
	 break;
       }
       case UIR_RECORDTYPE: {
	 Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
	 tOffset += (OFF_T) UIR_RECORDTYPE_OFFSET64;
         if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
         if (!Read32_64(fp,buffer)) return CRE;
         break;
       }
       default: {
	 OFF_T *buffer = va_arg (ap, OFF_T *); OFF_T tOffset = offset;
	 switch (field) {
	   case UIR_RECORDSIZE: tOffset += (OFF_T) UIR_RECORDSIZE_OFFSET64; break;
	   case UIR_NEXTUIR: tOffset += (OFF_T) UIR_NEXTUIR_OFFSET64; break;
	   case UIR_PREVUIR: tOffset += (OFF_T) UIR_PREVUIR_OFFSET64; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read64_64(fp,buffer)) return CRE;
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadCCR64.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadCCR64 (vFILE *fp, OFF_T offset, ...)
#else
STATICforIDL CDFstatus ReadCCR64 (va_alist)
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
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CRE;
	 if (!Read64_64(fp,&(CCR->RecordSize))) return CRE;
	 if (!Read32_64(fp,&(CCR->RecordType))) return CRE;
         if (!Read64s_64(fp,&(CCR->CPRoffset),2)) return CRE;
         /*
	   if (!Read64_64(fp,&(CCR->CPRoffset))) return CRE;
	   if (!Read64_64(fp,&(CCR->uSize))) return CRE;
         */
	 if (!Read32_64(fp,&(CCR->rfuA))) return CRE;
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
         if (!SEEKv64(fp,offset,vSEEK_SET)) return CRE;
         if (!Read32_64(fp,buffer)) return CRE;
         break;
       }
       default: {
	 OFF_T *buffer = va_arg (ap, OFF_T *); OFF_T tOffset = offset;
	 switch (field) {
	   case CCR_RECORDSIZE: tOffset += (OFF_T) CCR_RECORDSIZE_OFFSET64; break;
	   case CCR_CPROFFSET: tOffset += (OFF_T) CCR_CPROFFSET_OFFSET64; break;
	   case CCR_USIZE: tOffset += (OFF_T) CCR_USIZE_OFFSET64; break;
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read64_64(fp,buffer)) return CRE;
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadCPR64.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadCPR64 (vFILE *fp, OFF_T offset, ...)
#else
STATICforIDL CDFstatus ReadCPR64 (va_alist)
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
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CRE;
	 if (!Read64_64(fp,&(CPR->RecordSize))) return CRE;
         if (!Read32s_64(fp,&(CPR->RecordType),4)) return CRE;
         /*
	   if (!Read32_64(fp,&(CPR->RecordType))) return CRE;
	   if (!Read32_64(fp,&(CPR->cType))) return CRE;
	   if (!Read32_64(fp,&(CPR->rfuA))) return CRE;
	   if (!Read32_64(fp,&(CPR->pCount))) return CRE;
         */
	 if (CPR->pCount > CDF_MAX_PARMS) return TOO_MANY_PARMS;
	 for (i = 0; i < CPR->pCount; i++) {
	    if (!Read32_64(fp,&(CPR->cParms[i]))) return CRE;
	 }
	 break;
       }
       case CPR_RECORDSIZE: {
	 OFF_T *buffer = va_arg (ap, OFF_T *); OFF_T tOffset = offset;
	 tOffset += (OFF_T) CPR_RECORDSIZE_OFFSET64;
         if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
         if (!Read64_64(fp,buffer)) return CRE;
         break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
	 switch (field) {
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
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read32_64(fp,buffer)) return CRE;
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadSPR64.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadSPR64 (vFILE *fp, OFF_T offset, ...)
#else
STATICforIDL CDFstatus ReadSPR64 (va_alist)
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
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CRE;
	 if (!Read64_64(fp,&(SPR->RecordSize))) return CRE;
         if (!Read32s_64(fp,&(SPR->RecordType),4)) return CRE;
         /*
	   if (!Read32_64(fp,&(SPR->RecordType))) return CRE;
	   if (!Read32_64(fp,&(SPR->sArraysType))) return CRE;
	   if (!Read32_64(fp,&(SPR->rfuA))) return CRE;
	   if (!Read32_64(fp,&(SPR->pCount))) return CRE;
         */
	 if (SPR->pCount > CDF_MAX_PARMS) return TOO_MANY_PARMS;
	 for (i = 0; i < SPR->pCount; i++) {
	    if (!Read32_64(fp,&(SPR->sArraysParms[i]))) return CRE;
	 }
	 break;
       }
       case SPR_RECORDSIZE: {
         OFF_T *buffer = va_arg (ap, OFF_T *); OFF_T tOffset = offset;
         tOffset += (OFF_T) SPR_RECORDSIZE_OFFSET64;
         if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
         if (!Read64_64(fp,buffer)) return CRE;
         break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
	 switch (field) {
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
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read32_64(fp,buffer)) return CRE;
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadCVVR64.
******************************************************************************/

#if defined(STDARG)
STATICforIDL CDFstatus ReadCVVR64 (vFILE *fp, OFF_T offset, ...)
#else
STATICforIDL CDFstatus ReadCVVR64 (va_alist)
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
	 if (!SEEKv64(fp,offset,vSEEK_SET)) return CRE;
	 if (!Read64_64(fp,&(CVVR->RecordSize))) return CRE;
         if (!Read32s_64(fp,&(CVVR->RecordType),2)) return CRE;
         /*
	   if (!Read32_64(fp,&(CVVR->RecordType))) return CRE;
	   if (!Read32_64(fp,&(CVVR->rfuA))) return CRE;
         */
	 if (!Read64_64(fp,&(CVVR->cSize))) return CRE;
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
         if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
         if (!Read64_64(fp,buffer)) return CRE;
         break;
       }
       default: {
	 Int32 *buffer = va_arg (ap, Int32 *); OFF_T tOffset = offset;
	 switch (field) {
           /* case CVVR_RECORDSIZE: tOffset += (OFF_T) CVVR_RECORDSIZE_OFFSET64; break; */
	   case CVVR_RECORDTYPE: tOffset += (OFF_T) CVVR_RECORDTYPE_OFFSET64; break;
	   case CVVR_RFUa: tOffset += (OFF_T) CVVR_RFUa_OFFSET64; break;
           /* case CVVR_CSIZE: tOffset += (OFF_T) CVVR_CSIZE_OFFSET64; break; */
	   default: return CDF_INTERNAL_ERROR;
	 }
	 if (!SEEKv64(fp,tOffset,vSEEK_SET)) return CRE;
	 if (!Read32_64(fp,buffer)) return CRE;
	 break;
       }
     }
  }
}

/******************************************************************************
* ReadADRList64.
******************************************************************************/

STATICforIDL CDFstatus ReadADRList64 (vFILE *fp)
{
  OFF_T nxtADR = fp->GDR64->ADRhead;
  CDFstatus pStatus = CDF_OK;
  Int32 i;
  OFF_T temp;

  /**************************************************************************
  * Allocate memory for the list of ADRs 
  ***************************************************************************/
  if (fp->GDR64->NumAttr > 0)
  fp->ADRList64 = cdf_AllocateMemory((size_t)fp->GDR64->NumAttr * 
                                     sizeof(struct ADRstruct64**), NULL);
  if (fp->GDR64->NumAttr > 0 && fp->ADRList64 == NULL) return BAD_MALLOC;
  for (i = 0; i < fp->GDR64->NumAttr; i++)
  {
      fp->ADRList64[i] = cdf_AllocateMemory((size_t)sizeof(struct ADRstruct64), NULL);
      if (fp->ADRList64[i] == NULL) return BAD_MALLOC;
  };
  /***************************************************************************
  * Read the ADRs into the list                                              
  ****************************************************************************/
  for (i = 0; i < fp->GDR64->NumAttr; i++)
  {
      if (!SEEKv64(fp,nxtADR,vSEEK_SET)) return CRE;
      if (!Read64_64(fp, &temp)) return CRE;
      fp->ADRList64[i]->RecordSize = temp;
      if (!Read32_64(fp,&(fp->ADRList64[i]->RecordType))) return CRE;
      if (!Read64_64(fp,&temp)) return CRE;
      nxtADR = temp;
      fp->ADRList64[i]->ADRnext = 0;
      if (!Read64_64(fp,&temp)) return CRE;
      fp->ADRList64[i]->AgrEDRhead = temp;
      if (!Read32s_64(fp,&(fp->ADRList64[i]->Scope),5)) return CRE;
      /*
      if (!Read32_64(fp,&(fp->ADRList64[i]->Scope))) return CRE;
      if (!Read32_64(fp,&(fp->ADRList64[i]->Num))) return CRE;
      if (!Read32_64(fp,&(fp->ADRList64[i]->NgrEntries))) return CRE;
      if (!Read32_64(fp,&(fp->ADRList64[i]->MAXgrEntry))) return CRE;
      if (!Read32_64(fp,&(fp->ADRList64[i]->rfuA))) return CRE;
      */
      if (!Read64_64(fp,&temp)) return CRE;
      fp->ADRList64[i]->AzEDRhead = temp;
      if (!Read32s_64(fp,&(fp->ADRList64[i]->NzEntries),3)) return CRE;
      /*
        if (!Read32_64(fp,&(fp->ADRList64[i]->NzEntries))) return CRE;
        if (!Read32_64(fp,&(fp->ADRList64[i]->MAXzEntry))) return CRE;
        if (!Read32_64(fp,&(fp->ADRList64[i]->rfuE))) return CRE;
      */
      if (!READv64(fp->ADRList64[i]->Name,CDF_ATTR_NAME_LEN256,1,fp))
        return CRE;
      NulPad (fp->ADRList64[i]->Name, CDF_ATTR_NAME_LEN256);
      /***********************************************************************
      * Allocate memory for the lists of gAEDRs and zAEDRS associated with 
      * this ADR and read the AEDRs into the lists. 
      ***********************************************************************/
      pStatus = ReadAEDRList64(fp, &(fp->ADRList64[i]->grAEDRList64),
                               fp->ADRList64[i]->AgrEDRhead, 
                               fp->ADRList64[i]->NgrEntries,
                               fp->ADRList64[i]->MAXgrEntry);
      if (pStatus != CDF_OK) return pStatus;
      pStatus = ReadAEDRList64(fp, &(fp->ADRList64[i]->zAEDRList64),
                               fp->ADRList64[i]->AzEDRhead, 
                               fp->ADRList64[i]->NzEntries,
                               fp->ADRList64[i]->MAXzEntry);
      if (pStatus != CDF_OK) return pStatus;
  };
  return pStatus;
}

/******************************************************************************
* ReadAEDRList64.
* Reads the set of entrys (zENTRYs or grENTRYs) associated with an attribute
* into a list indexed by the entry number. If there is no entry for a particular
* index (i.e., there is no entry for the associated variable/attribute
* pair), the list entry is NULL.
******************************************************************************/
STATICforIDL CDFstatus ReadAEDRList64 (vFILE *fp, 
                                       struct AEDRstructExt64 ***AEDRList, 
                                       OFF_T AEDRHead, 
                                       Int32 NumEntries,
                                       Int32 MaxEntry)
{
  CDFstatus pStatus = CDF_OK;
  OFF_T NxtAEDR = AEDRHead;
  struct AEDRstructExt64 *TempAEDR;
  Int32 iEntries;
  Int32 i;
  OFF_T temp;
  /**************************************************************************
  * Allocate memory for the list and initialize each entry to NULL.
  **************************************************************************/
  if (MaxEntry >= 0) {
      *AEDRList = cdf_AllocateMemory((size_t)(MaxEntry + 1) * 
                                     sizeof(struct AEDRstructExt64**), NULL);
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
      TempAEDR = cdf_AllocateMemory((size_t)sizeof(struct AEDRstructExt64), NULL);
      if (TempAEDR == NULL) return BAD_MALLOC;
      if (!SEEKv64(fp,NxtAEDR,vSEEK_SET)) return CRE;
      if (!Read64_64(fp,&temp)) return CRE;
      TempAEDR->AEDR.RecordSize = temp;
      if (!Read32_64(fp,&(TempAEDR->AEDR.RecordType))) return CRE;
      if (!Read64_64(fp,&temp)) return CRE;
      NxtAEDR = temp;
      TempAEDR->AEDR.AEDRnext = 0;
      if (!Read32s_64(fp,&(TempAEDR->AEDR.AttrNum),9)) return CRE;
      /*
        if (!Read32_64(fp,&(TempAEDR->AEDR.AttrNum))) return CRE;
        if (!Read32_64(fp,&(TempAEDR->AEDR.DataType))) return CRE;
        if (!Read32_64(fp,&(TempAEDR->AEDR.Num))) return CRE;
        if (!Read32_64(fp,&(TempAEDR->AEDR.NumElems))) return CRE;
        if (!Read32_64(fp,&(TempAEDR->AEDR.rfuA))) return CRE;
        if (!Read32_64(fp,&(TempAEDR->AEDR.rfuB))) return CRE;
        if (!Read32_64(fp,&(TempAEDR->AEDR.rfuC))) return CRE;
        if (!Read32_64(fp,&(TempAEDR->AEDR.rfuD))) return CRE;
        if (!Read32_64(fp,&(TempAEDR->AEDR.rfuE))) return CRE;
      */
      TempAEDR->ValueSize = (CDFelemSize(TempAEDR->AEDR.DataType) * 
                             TempAEDR->AEDR.NumElems);
      if (TempAEDR->ValueSize < 1) {
        return CV3C;
      }
      TempAEDR->Value = cdf_AllocateMemory((size_t)TempAEDR->ValueSize, NULL);
      if (TempAEDR->Value == NULL) return BAD_MALLOC;
      if (!READv64(TempAEDR->Value,TempAEDR->ValueSize,1,fp)) return CRE;
      if (STRINGdataType(TempAEDR->AEDR.DataType)) {
        FillSpacesToString((char *)(TempAEDR->Value),
                           (int)TempAEDR->AEDR.NumElems,
                           (int)TempAEDR->AEDR.NumElems);
      }
      ++iEntries;
      if (TempAEDR->AEDR.Num < 0 || TempAEDR->AEDR.Num > MaxEntry) {
        return CV3C;
      }
      (*AEDRList)[TempAEDR->AEDR.Num] = TempAEDR;
  };
  return pStatus;
}

