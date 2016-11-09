/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                        CDF `check sum' operations.
*
*  Version 1.0, 21-Mar-06, Hughes STX.
*
*  Modification history:
*
*   V1.0  21-Mar-06, M Liu      Original version.
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"
#include "cdfmd5.h"

#define BUFFSIZE 16384

/******************************************************************************
* Local function prototypes.
******************************************************************************/
CDFstatus AddChecksumMD5 PROTOARGs((vFILE *vFp, long size,
                                    uChar *signature));
CDFstatus AddChecksumMD5_64 PROTOARGs((vFILE *vFp, OFF_T size,
                                       uChar *signature));
CDFstatus ComputeChecksumMD5 PROTOARGs((vFILE *vFp, long size,
                                        uChar *signature));
CDFstatus ComputeChecksumMD5_64 PROTOARGs((vFILE *vFp, OFF_T size,
                                           uChar *signature));
CDFstatus GetChecksumMD5 PROTOARGs((vFILE *vFp, long size, 
                                    uChar *signature));
CDFstatus GetChecksumMD5_64 PROTOARGs((vFILE *vFp, OFF_T size,
                                       uChar *signature));
CDFstatus AddChecksum PROTOARGs((struct CDFstruct *CDF));
CDFstatus AddChecksum64 PROTOARGs((struct CDFstruct *CDF));
CDFstatus VerifyChecksum PROTOARGs((struct CDFstruct *CDF));
CDFstatus VerifyChecksum64 PROTOARGs((struct CDFstruct *CDF));

/******************************************************************************
* AddChecksumMD5.
******************************************************************************/

CDFstatus AddChecksumMD5 (vFp, size, signature)
vFILE *vFp;
long  size;
uChar signature[16];
{
  
#if defined(vms)
  size_t lastBlk;
  uChar buffer[512];
  int i, j, inBlk, cross= 0;
  inBlk = size % 512;
  lastBlk = (size_t) 512 * (size / 512);
  if (inBlk != 0) {
    if (fseek(vFp->fp,lastBlk,SEEK_SET) == EOF) {
      return FALSE;
    }
    /* for (i = 0; i < 512; i++) buffer[i] = 0; */
    memset (buffer, 0, (size_t) 512);
    if (fread(buffer,512,1,vFp->fp) != 1) {
      return FALSE;
    }
    for (i = 0; i < 16; i++) {
       j = inBlk + i;
       if (j > 511) {
         cross = i;
         break;
       }
       buffer[j] = signature[i];
    }
    if (fseek(vFp->fp,lastBlk,SEEK_SET) == EOF) {
      return FALSE;
    }
    if (fwrite(buffer,512,1,vFp->fp) != 1) {
      return FALSE;
    }

    if (cross > 0) {
      /* for (i = 0; i < 512; i++) buffer[i] = 0; */
      memset (buffer, 0, (size_t) 512); 
      j = 0;
      for (i = cross; i < 16; i++) {
         buffer[j] = signature[i];
         j++; 
      }
      if (fwrite(buffer,512,1,vFp->fp) != 1) {
        return FALSE;
      }
    }
  } else { 
    /* for (i = 16; i < 512; i++) buffer[i] = 0; */
    /* for (i = 0; i < 16; i++) buffer[i] = signature[i]; */
    memcpy (buffer, signature, (size_t) 16);
    memset (buffer+16, 0, (size_t) 512-16);
    if (fseek(vFp->fp,size,SEEK_SET) == EOF) {
      return FALSE;
    }
    if (fwrite(buffer,512,1,vFp->fp) != 1) {
      return FALSE;
    }
  }
#else
  if (fseek(vFp->fp,size,vSEEK_SET) == EOF) return FALSE;
  if (fwrite(signature,1,16,vFp->fp) != 16) return FALSE;
#endif
  if (fflush(vFp->fp) == EOF) {
    vFp->error = TRUE;
    return FALSE;
  }
  return TRUE; 
}
     
/******************************************************************************
* AddChecksumMD5_64.
******************************************************************************/
     
CDFstatus AddChecksumMD5_64 (vFp, size, signature)
vFILE *vFp;
OFF_T  size;
uChar signature[16];
{    

#if defined(win32) && (_FILE_OFFSET_BITS == 64)
  if (vFp->fh == 0) return FALSE;
  if (FSEEK64(vFp->fh,(OFF_T) size,vSEEK_SET) == EOF) return FALSE;
  if (FWRITE64(vFp->fh,signature,16) != 16) return FALSE;
  if (FLUSH64(vFp->fh) == EOF) {
    vFp->error = TRUE;
    return FALSE;
  }
#else
  if (vFp->fp == NULL) return FALSE;
  else {
#   if defined(vms)
      OFF_T lastBlk;
      uChar buffer[512];
      int i, j, inBlk, cross= 0;
      inBlk = size % 512;
      lastBlk = (OFF_T) 512 * (size / 512);
      if (inBlk != 0) {
        if (FSEEK64(vFp->fp,lastBlk,SEEK_SET) == EOF) {
          vFp->error = TRUE;
          return FALSE;
        }
        /* for (i = 0; i < 512; i++) buffer[i] = 0; */
        memset (buffer, 0, (size_t) 512);
        if (FREAD64(buffer,512,1,vFp->fp) != 1) {
          vFp->error = TRUE;
          return FALSE;
        }
        for (i = 0; i < 16; i++) {
          j = inBlk + i;
          if (j > 511) {
            cross = i;
            break;
          }
          buffer[j] = signature[i];
        }
        if (FSEEK64(vFp->fp,lastBlk,SEEK_SET) == EOF) {
          vFp->error = TRUE;
          return FALSE;
        }
        if (FWRITE64(buffer,512,1,vFp->fp) != 1) {
          vFp->error = TRUE;
          return FALSE;
        }

        if (cross > 0) {
          /* for (i = 0; i < 512; i++) buffer[i] = 0; */
          memset (buffer, 0, (size_t) 512);
          j = 0;
          for (i = cross; i < 16; i++) {
            buffer[j] = signature[i];
            j++;
          }
          if (FWRITE64(buffer,512,1,vFp->fp) != 1) {
            vFp->error = TRUE;
            return FALSE;
          }
        }
      } else {
        /* for (i = 16; i < 512; i++) buffer[i] = 0; */
        /* for (i = 0; i < 16; i++) buffer[i] = signature[i]; */
        memcpy (buffer, signature, (size_t) 16);
        memset (buffer+16, 0, (size_t) 512-16);
        if (FSEEK64(vFp->fp,size,SEEK_SET) == EOF) {
          vFp->error = TRUE;
          return FALSE;
        }
        if (FWRITE64(buffer,512,1,vFp->fp) != 1) {
          vFp->error = TRUE;
          return FALSE;
        }
      }
#   else
      if (FSEEK64(vFp->fp,(OFF_T) size,vSEEK_SET) == EOF) return FALSE;
      if (FWRITE64(signature,1,16,vFp->fp) != 16) return FALSE;
#   endif
  }
  if (FLUSH64(vFp->fp) == EOF) {
    vFp->error = TRUE;
    return FALSE;
  }
#endif
  return TRUE;
}

/******************************************************************************
* ComputeChecksumMD5.
******************************************************************************/

CDFstatus ComputeChecksumMD5 (vFp, size, signature)
vFILE *vFp;
long  size;
uChar  signature[16];
{
  uChar buffer[BUFFSIZE];
  struct MD5Context md5c;
  int jj = 0, readBytes;

  MD5Init(&md5c);
  if (fseek(vFp->fp, 0, vSEEK_SET) == EOF) return FALSE;
  while (jj < size) {
     if (jj+BUFFSIZE < size) readBytes = BUFFSIZE;
     else readBytes = size - jj;
     if (fread(buffer,1,readBytes,vFp->fp) != (size_t) readBytes) return FALSE;
     MD5Update(&md5c, buffer, (unsigned) readBytes);
     jj = jj + readBytes;
  }
  MD5FinalZ(signature, &md5c);
  return TRUE;

}

/******************************************************************************
* ComputeChecksumMD5_64.
******************************************************************************/

CDFstatus ComputeChecksumMD5_64 (vFp, size, signature)
vFILE *vFp;
OFF_T  size;
uChar  signature[16];
{
  uChar buffer[BUFFSIZE];
  struct MD5Context md5c;
  OFF_T jj = 0; 
  int readBytes;

  MD5Init(&md5c);
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
     if (FSEEK64(vFp->fh,(OFF_T) 0,vSEEK_SET) == EOF) return FALSE;
#else
     if (FSEEK64(vFp->fp,(OFF_T) 0,vSEEK_SET) == EOF) return FALSE;
#endif
  while (jj < size) {
     if (jj+BUFFSIZE < size) readBytes = BUFFSIZE;
     else readBytes = (int) (size - jj);
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
     if (FREAD64(vFp->fh,buffer,(unsigned int) readBytes) != (int) readBytes)
        return FALSE;
#else
     if (FREAD64(buffer,1,readBytes,vFp->fp) != readBytes) return FALSE;
#endif
     MD5Update(&md5c, buffer, (unsigned) readBytes);
     jj = jj + readBytes;
  }
  MD5FinalZ(signature, &md5c);
  return TRUE;
}

/******************************************************************************
* GetChecksumMD5.
******************************************************************************/

CDFstatus GetChecksumMD5 (vFp, size, signature)
vFILE *vFp;
long  size;
uChar signature[16];
{ 

  if (fseek(vFp->fp,size,vSEEK_SET) == EOF) return FALSE;
  if (fread(signature,1,16,vFp->fp) != 16) return FALSE;
  return TRUE;
}       

/******************************************************************************
* GetChecksumMD5_64.
******************************************************************************/
     
CDFstatus GetChecksumMD5_64 (vFp, size, signature)
vFILE *vFp;
OFF_T  size;
uChar *signature;
{

#if defined(win32) && (_FILE_OFFSET_BITS == 64)
  if (FSEEK64(vFp->fh,(OFF_T) size,vSEEK_SET) == EOF) return FALSE;
  if (FREAD64(vFp->fh,signature,16) != 16) return FALSE;
#else
  if (FSEEK64(vFp->fp,(OFF_T) size,vSEEK_SET) == EOF) return FALSE;
  if (FREAD64(signature,1,16,vFp->fp) != 16) return FALSE;
#endif
  return TRUE;
} 

/******************************************************************************
* CDFVerifyChecksum.
******************************************************************************/

CDFstatus CDFVerifyChecksum (CDF)
struct CDFstruct *CDF;
{
  
  if (!CDF->largeFile)
    return VerifyChecksum (CDF);
  else
    return VerifyChecksum64 (CDF);

}

/******************************************************************************
* VerifyChecksum.
******************************************************************************/

CDFstatus VerifyChecksum (CDF)
struct CDFstruct *CDF;
{
  long GDRoffset, CPRoffset;
  Int32 CDRflags;
  long CCRsize, CPRsize, usedSize; 
  CDFstatus pStatus = CDF_OK;
  uChar signature[16], csig[16];

  if (!sX(ReadCDR(CDF->fp,V2_CDR_OFFSET,
                  CDR_FLAGS,&CDRflags,
                  CDR_GDROFFSET, &GDRoffset,
                  CDR_NULL),&pStatus)) { 
    return pStatus;
  }

  if (!BITSET(CDRflags,CDR_CHECKSUM_BIT)) return pStatus;

  if (CDF->uDotFp == NULL) {
    /**************************************************************************
    * A uncompressed CDF.
    **************************************************************************/
    if (!sX(ReadGDR(CDF->dotFp,GDRoffset,
                    GDR_EOF,&usedSize,
                    GDR_NULL),&pStatus)) {
      return pStatus;
    }
  } else {
    /**************************************************************************
    * A compressed CDF. 
    **************************************************************************/
    if (!sX(ReadCCR(CDF->dotFp,V2_CCR_OFFSET,
                    CCR_RECORDSIZE,&CCRsize,
                    CCR_CPROFFSET, &CPRoffset,
                    CCR_NULL),&pStatus)) {
      return pStatus;
    }
    if (!sX(ReadCPR(CDF->dotFp,CPRoffset,
                    CPR_RECORDSIZE,&CPRsize,
                    CPR_NULL),&pStatus)) {
      return pStatus;
    }
    usedSize = 8 + CCRsize + CPRsize;
  }

  if (BITSET(CDRflags,CDR_CHECKSUM_MD5_BIT)) {
    if (!sX(GetChecksumMD5(CDF->dotFp, usedSize, signature),
        &pStatus)) return pStatus;
    if (!sX(ComputeChecksumMD5(CDF->dotFp, usedSize, csig),
        &pStatus)) return pStatus;
    if (memcmp(signature, csig, 16) == 0) return CDF_OK;
    else return CHECKSUM_ERROR;
  }
  return pStatus;
}

/******************************************************************************
* VerifyChecksum64.
******************************************************************************/

CDFstatus VerifyChecksum64 (CDF)
struct CDFstruct *CDF;
{
  OFF_T GDRoffset, CPRoffset;
  Int32 CDRflags;
  OFF_T CCRsize, CPRsize, usedSize;
  CDFstatus pStatus = CDF_OK;
  uChar signature[16], csig[16];

  if (!sX(ReadCDR64(CDF->fp,V3_CDR_OFFSET64,
                    CDR_FLAGS,&CDRflags,
                    CDR_GDROFFSET, &GDRoffset,
                    CDR_NULL),&pStatus)) {
    return pStatus;
  }
  if (!BITSET(CDRflags,CDR_CHECKSUM_BIT)) return pStatus;
  if (CDF->uDotFp == NULL) {
    /**************************************************************************
    * A uncompressed CDF.
    **************************************************************************/
    if (!sX(ReadGDR64(CDF->dotFp,GDRoffset,
                      GDR_EOF,&usedSize,
                      GDR_NULL),&pStatus)) {
      return pStatus;
    }
  } else {
    /**************************************************************************
    * A compressed CDF.
    **************************************************************************/
    if (!sX(ReadCCR64(CDF->dotFp,V3_CCR_OFFSET64,
                      CCR_RECORDSIZE,&CCRsize,
                      CCR_CPROFFSET, &CPRoffset,
                      CCR_NULL),&pStatus)) {
      return pStatus;
    }
    if (!sX(ReadCPR64(CDF->dotFp,CPRoffset,
                      CPR_RECORDSIZE,&CPRsize,
                      CPR_NULL),&pStatus)) {
      return pStatus;
    }
    usedSize = 8 + CCRsize + CPRsize;
  }

  if (BITSET(CDRflags,CDR_CHECKSUM_MD5_BIT)) {
    if (!sX(GetChecksumMD5_64(CDF->dotFp, usedSize, signature), 
        &pStatus)) return pStatus;
    if (!sX(ComputeChecksumMD5_64(CDF->dotFp, usedSize, csig), 
        &pStatus)) return pStatus;
    if (memcmp(signature, csig, 16) == 0) return CDF_OK;
    else return CHECKSUM_ERROR;
  }
  return pStatus;
}

/******************************************************************************
* CDFAddChecksum.
******************************************************************************/

CDFstatus CDFAddChecksum (CDF)
struct CDFstruct *CDF;
{
  if (!CDF->largeFile)
    return AddChecksum (CDF);
  else
    return AddChecksum64 (CDF);
}
  
/******************************************************************************
* AddChecksum.
******************************************************************************/

CDFstatus AddChecksum (CDF)
struct CDFstruct *CDF;
{
  long GDRoffset, CPRoffset;
  Int32 CDRflags;
  long CCRsize, CPRsize, usedSize; 
  CDFstatus pStatus = CDF_OK;
  uChar signature[16];

  if (!sX(ReadCDR(CDF->fp,V2_CDR_OFFSET,
                  CDR_FLAGS,&CDRflags,
                  CDR_GDROFFSET, &GDRoffset,
                  CDR_NULL),&pStatus)) { 
    return pStatus;
  }

  if (!BITSET(CDRflags,CDR_CHECKSUM_BIT)) return pStatus;

  if (CDF->uDotFp == NULL) {
    /**************************************************************************
    * A uncompressed CDF.
    **************************************************************************/
    if (!sX(ReadGDR(CDF->dotFp,GDRoffset,
                    GDR_EOF,&usedSize,
                    GDR_NULL),&pStatus)) {
      return pStatus;
    }
  } else {
    /**************************************************************************
    * A compressed CDF. 
    **************************************************************************/
    if (!sX(ReadCCR(CDF->dotFp,V2_CCR_OFFSET,
                    CCR_RECORDSIZE,&CCRsize,
                    CCR_CPROFFSET, &CPRoffset,
                    CCR_NULL),&pStatus)) {
      return pStatus;
    }
    if (!sX(ReadCPR(CDF->dotFp,CPRoffset,
                    CPR_RECORDSIZE,&CPRsize,
                    CPR_NULL),&pStatus)) {
      return pStatus;
    }
    usedSize = 8 + CCRsize + CPRsize;
  }

  if (BITSET(CDRflags,CDR_CHECKSUM_MD5_BIT)) {
    if (!sX(ComputeChecksumMD5(CDF->dotFp, usedSize, signature), 
        &pStatus)) return pStatus;
    if (!sX(AddChecksumMD5(CDF->dotFp, usedSize, signature), 
        &pStatus)) return pStatus;
  }
  return pStatus;
}


/******************************************************************************
* AddChecksum64.
******************************************************************************/

CDFstatus AddChecksum64 (CDF)
struct CDFstruct *CDF;
{
  OFF_T GDRoffset, CPRoffset;
  Int32 CDRflags;
  OFF_T CCRsize, CPRsize, usedSize; 
  CDFstatus pStatus = CDF_OK;
  uChar signature[16];

  if (!sX(ReadCDR64(CDF->fp,V3_CDR_OFFSET64,
                    CDR_FLAGS,&CDRflags,
                    CDR_GDROFFSET, &GDRoffset,
                    CDR_NULL),&pStatus)) { 
    return pStatus;
  }

  if (!BITSET(CDRflags,CDR_CHECKSUM_BIT)) return pStatus;

  if (CDF->uDotFp == NULL) {
    /**************************************************************************
    * A uncompressed CDF.
    **************************************************************************/
    if (!sX(ReadGDR64(CDF->dotFp,GDRoffset,
                      GDR_EOF,&usedSize,
                      GDR_NULL),&pStatus)) {
      return pStatus;
    }
  } else {
    /**************************************************************************
    * A compressed CDF. 
    **************************************************************************/
    if (!sX(ReadCCR64(CDF->dotFp,V3_CCR_OFFSET64,
                      CCR_RECORDSIZE,&CCRsize,
                      CCR_CPROFFSET, &CPRoffset,
                      CCR_NULL),&pStatus)) {
      return pStatus;
    }
    if (!sX(ReadCPR64(CDF->dotFp,CPRoffset,
                      CPR_RECORDSIZE,&CPRsize,
                      CPR_NULL),&pStatus)) {
      return pStatus;
    }
    usedSize = 8 + CCRsize + CPRsize;
  }

  if (BITSET(CDRflags,CDR_CHECKSUM_MD5_BIT)) {
    if (!sX(ComputeChecksumMD5_64(CDF->dotFp, usedSize, signature), 
        &pStatus)) return pStatus;
    if (!sX(AddChecksumMD5_64(CDF->dotFp, usedSize, signature), 
        &pStatus)) return pStatus;
  }
  return pStatus;
}

