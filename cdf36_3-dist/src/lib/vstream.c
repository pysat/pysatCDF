/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                            Virtual stream file.
*
*  Version 4.7a, 18-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  22-Jan-91, J Love     Original version (developed for CDF V2.0).
*   V2.0  12-Mar-91, J Love     All fixes to V1.x.  Modified vread and vwrite
*                               to buffer only when necessary.
*   V3.0  14-May-91, J Love     Added caching (for CDF V2.1).
*   V3.1  31-Jul-91, J Love     Added veof.  Added 'memmove' for UNIX.  Added
*                               "deq - default extension quantity" if VMS.
*                               Changed algorithm that looks for bufferN.
*                               Added number of CACHE buffers as a parameter
*                               specified in 'Vopen'.  Renamed functions to
*                               avoid collisions on SGi/IRIX.
*   V3.2  15-Aug-91, J Love     Changed for IBM-PC/MS-DOS port.
*   V4.0  20-May-92, J Love     IBM PC port/CDF V2.2.
*   V4.1  29-Sep-92, J Love     CDF V2.3.  Dealt with EOFs not at 512-byte
*                               boundaries (when FTPed from a UNIX machine).
*   V4.2  21-Dec-93, J Love     CDF V2.4.
*   V4.3  12-Dec-94, J Love     CDF V2.5.
*   V4.3a 19-Jan-95, J Love     IRIX 6.0 (64-bit).
*   V4.3b 24-Feb-95, J Love     Solaris 2.3 IDL i/f.
*   V4.4   7-Jun-95, J Love     Virtual memory under Microsoft C 7.00.
*   V4.5  25-Jul-95, J Love     More virtual memory under Microsoft C 7.00.
*   V4.6  29-Sep-95, J Love     Improved performance...on non-VMS systems don't
*                               extend files a block at a time, don't clear
*                               bytes (anywhere), etc.
*   V4.7  26-Aug-96, J Love     CDF V2.6.
*   V4.7a 18-Nov-97, J Love	Windows NT/Visual C++.
*   V4.8  29-Jun-04, M Liu      Added LFS (Large File Support > 2G).
*   V4.9  25-Apr-07, D Berger   Changed all instances of hardcoded 512 to
*                               nCACHE_BUFFER_BYTEs.
*   V4.10 20-Jun-07, D Berger   Added initialization of variables to support
*                               READONLYon enhancements.
*   V4.11 18-Jun-08, M Liu      Modified the V_read and V_write to make sure
*                               the actual read/write bytes is within the 
*                               buffer size.
*   V4.12 12-Aug-10, M Liu      Fill 0s to each new cache in AllocateBuffer.
*
******************************************************************************/

/******************************************************************************
* Include files.
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"

/******************************************************************************
* Local macros/typedef's.
******************************************************************************/

#define CLEAR_BYTES     0

#if defined(vms) || defined(MPW_C)
#define EXTEND_FILE     1
#else
#define EXTEND_FILE     0
#endif

#define LASTphyBLOCKn(vFp) \
BOO(vFp->phyLength == 0,NO_BLOCK,((vFp->phyLength - 1)/nCACHE_BUFFER_BYTEs))

#if defined(MICROSOFTC_700) && INCLUDEvMEMORY
#define CACHEbufferREADfrom(cache) \
BOO(useVmem,LoadVMemory((MemHandle)cache->ptr,FALSE),cache->ptr)
#define CACHEbufferWRITEto(cache) \
BOO(useVmem,LoadVMemory((MemHandle)cache->ptr,TRUE),cache->ptr)
#else
#define CACHEbufferREADfrom(cache) cache->ptr
#define CACHEbufferWRITEto(cache) cache->ptr
#endif

/******************************************************************************
* Local function prototypes.
******************************************************************************/

static FILE *CDFOpenFile PROTOARGs((char *file_spec, char *a_mode));
static Logical FreeCache PROTOARGs((vCACHE *firstCache));
static vCACHE *FindCache PROTOARGs((vFILE *vFp, long blockN));
static Logical vRead PROTOARGs((
  long offset, void *buffer, size_t nBytes, vFILE *vFp
));
static Logical vWrite PROTOARGs((
  long offset, void *buffer, size_t nBytes, vFILE *vFp
));
static vCACHE *AllocateBuffer PROTOARGs((vFILE *vFp));
static vCACHE *PageIn PROTOARGs((vFILE *vFp, long blockN));
static Logical WriteBlockFromCache PROTOARGs((
  vFILE *vFp, vCACHE *cache, size_t Nbytes
));
static Logical WriteBlockFromBuffer PROTOARGs((
  vFILE *vFp, long blockN, void *buffer, size_t Nbytes
));
#if EXTEND_FILE
static Logical ExtendFile PROTOARGs((vFILE *vFp, long toBlockN));
#endif

/******************************************************************************
* CDFOpenFile.
******************************************************************************/

static FILE *CDFOpenFile (file_spec, a_mode)
char *file_spec;
char *a_mode;
{
#if defined(vms)
  char mrs[10+1];       /* Maximum record size. */
  char deq[10+1];       /* Default allocation quantity. */
  snprintf (mrs, (size_t) sizeof(mrs), "mrs=%d", nCACHE_BUFFER_BYTEs);
  snprintf (deq, (size_t) sizeof(deq), "deq=%d",
	    VMS_DEFAULT_nALLOCATION_BLOCKS);
  return fopen(file_spec,a_mode,"rfm=fix",mrs,deq);
#else
  return FOPEN(file_spec,a_mode);
#endif
}

/******************************************************************************
* FindCache.
******************************************************************************/

static vCACHE *FindCache (vFp, blockN)
vFILE *vFp;
long blockN;
{
  vCACHE *cache = vFp->cacheHead;
  while (cache != NULL) {
    if (cache->blockN == blockN) {
      if (cache != vFp->cacheHead) {
	if (cache == vFp->cacheTail) {
	  cache->prev->next = NULL;
	  vFp->cacheTail = cache->prev;
	}
	else {
	  cache->next->prev = cache->prev;
	  cache->prev->next = cache->next;
	}
	vFp->cacheHead->prev = cache;
	cache->next = vFp->cacheHead;
	vFp->cacheHead = cache;
	cache->prev = NULL;
      }
      return cache;
    }
    cache = cache->next;
  }
  return NULL;
}

/******************************************************************************
* FlushCache.
* Write cache buffers to disk from the specified starting buffer to the last
* buffer.
******************************************************************************/

VISIBLE_PREFIX Logical FlushCache (vFp, firstCache)
vFILE *vFp;             /* Pointer to vFILE structure. */
vCACHE *firstCache;     /* Pointer to the first cache structure to flush. */
{
  vCACHE *cache; long nBytes;
  for (cache = firstCache; cache != NULL; cache = cache->next) {
     if (cache->modified) {
#if defined(vms)
       nBytes = nCACHE_BUFFER_BYTEs;
#else
       nBytes = vFp->length - (cache->blockN * nCACHE_BUFFER_BYTEs);
       nBytes = MINIMUM (nBytes, nCACHE_BUFFER_BYTEs);
#endif
       if (!WriteBlockFromCache(vFp,cache,(size_t)nBytes)) return FALSE;
       cache->modified = FALSE;
     }
  }
  return TRUE;
}

/******************************************************************************
* FreeCache.
******************************************************************************/

static Logical FreeCache (firstCache)
vCACHE *firstCache;     /* Pointer to the first cache structure to free. */
{
  vCACHE *cache = firstCache;
  while (cache != NULL) {
    vCACHE *nextCache = cache->next;
#if defined(MICROSOFTC_700) && INCLUDEvMEMORY
    if (useVmem)
      FreeVMemory ((MemHandle) cache->ptr);
    else
#endif
      cdf_FreeMemory (cache->ptr, NULL);
    cdf_FreeMemory (cache, NULL);
    cache = nextCache;
  }
  return TRUE;
}

/******************************************************************************
* AllocateBuffer.
* Allocate a cache structure to use.  It may be necessary to page out a block
* to the file.  Returns a pointer to the allocated cache structure (or NULL if
* an error occurred).
******************************************************************************/

static vCACHE *AllocateBuffer (vFp)
vFILE *vFp;
{
  vCACHE *cache; long nBytes;
#if !defined(vms)
  long offset;
#endif
  /****************************************************************************
  * Check if a new cache structure can be allocated.  If the allocation(s)
  * fail, process as if the maximum number of cache buffers has already been
  * reached.
  ****************************************************************************/
  if (vFp->nBuffers < vFp->maxBuffers) {
    cache = (vCACHE *) cdf_AllocateMemory ((size_t)sizeof(vCACHE), NULL);
    if (cache != NULL) {
#if defined(MICROSOFTC_700) && INCLUDEvMEMORY
      if (useVmem)
	cache->ptr = (void *) AllocateVMemory (nCACHE_BUFFER_BYTEs);
      else
#endif
	cache->ptr = cdf_AllocateMemory ((size_t)nCACHE_BUFFER_BYTEs, NULL);
      if (cache->ptr != NULL) {
	if (vFp->cacheHead == NULL) {
	  vFp->cacheHead = cache;
	  vFp->cacheTail = cache;
	  cache->next = NULL;
	  cache->prev = NULL;
	}
	else {
	  vFp->cacheHead->prev = cache;
	  cache->next = vFp->cacheHead;
	  vFp->cacheHead = cache;
	  cache->prev = NULL;
	}
	(vFp->nBuffers)++;
	memset(cache->ptr, 0, nCACHE_BUFFER_BYTEs);
	return cache;
      }
      else {
	cdf_FreeMemory (cache, NULL);
	if (vFp->nBuffers == 0) return NULL;
      }
    }
  }
  /****************************************************************************
  * The maximum number of cache buffers have already been created.  Scan the
  * linked list of cache structures searching for the oldest buffer which has
  * not been modified.  If one is found, it is moved to the head of the linked
  * list.
  ****************************************************************************/
  for (cache = vFp->cacheTail; cache != NULL; cache = cache->prev) {
     if (!cache->modified) {
       if (cache != vFp->cacheHead) {
	 if (cache == vFp->cacheTail) {
	   cache->prev->next = NULL;
	   vFp->cacheTail = cache->prev;
	 }
	 else {
	   cache->prev->next = cache->next;
	   cache->next->prev = cache->prev;
	 }
	 vFp->cacheHead->prev = cache;
	 cache->next = vFp->cacheHead;
	 vFp->cacheHead = cache;
	 cache->prev = NULL;
       }
       return cache;
     }
  }
  /****************************************************************************
  * An unmodified buffer was not found.  The last buffer on the linked list
  * will be paged back out to the file and then this cache structure is moved
  * to the head of the linked list.
  ****************************************************************************/
  cache = vFp->cacheTail;
#if defined(vms)
  nBytes = nCACHE_BUFFER_BYTEs;
#else
  offset = nCACHE_BUFFER_BYTEs * cache->blockN;
  nBytes = vFp->length - offset;
  nBytes = MINIMUM (nBytes, nCACHE_BUFFER_BYTEs);
#endif
  if (!WriteBlockFromCache(vFp,cache,(size_t)nBytes)) return NULL;
  if (cache != vFp->cacheHead) {
    cache->prev->next = NULL;
    vFp->cacheTail = cache->prev;
    vFp->cacheHead->prev = cache;
    cache->next = vFp->cacheHead;
    vFp->cacheHead = cache;
    cache->prev = NULL;
  }
  (vFp->nPageOuts)++;
  memset(cache->ptr, 0, nCACHE_BUFFER_BYTEs);
  return cache;
}

/******************************************************************************
* ExtendFile.
* Extend the file to a specified number of blocks.
******************************************************************************/

#if EXTEND_FILE
static Logical ExtendFile (vFp, toBlockN)
vFILE *vFp;
long toBlockN;
{
  vCACHE *cache; long blockN;
  /****************************************************************************
  * First check to see if the physical end-of-file must be extended out to the
  * next multiple of the cache/block size.
  ****************************************************************************/
  if (vFp->phyLength > 0) {
    long lastPhyBlockN = LASTphyBLOCKn (vFp);
    long nBytes = vFp->phyLength - (nCACHE_BUFFER_BYTEs * lastPhyBlockN);
    if (nBytes < nCACHE_BUFFER_BYTEs) {
      cache = FindCache (vFp, lastPhyBlockN);
      if (cache != NULL) {
	void *buffer = CACHEbufferREADfrom (cache);
	if (buffer == NULL) return FALSE;
	if (!vWrite(nCACHE_BUFFER_BYTEs * lastPhyBlockN,
		    buffer,nCACHE_BUFFER_BYTEs,vFp)) return FALSE;
	cache->modified = FALSE;
      }
      else {
	Byte1 buffer[nCACHE_BUFFER_BYTEs];
	if (!vRead(nCACHE_BUFFER_BYTEs * lastPhyBlockN,
		   buffer,(size_t)nBytes,vFp)) return FALSE;
#if CLEAR_BYTES
	ClearBytes (buffer, (int) nBytes, nCACHE_BUFFER_BYTEs - 1);
#endif
	if (!vWrite(nCACHE_BUFFER_BYTEs * lastPhyBlockN,
		    buffer,nCACHE_BUFFER_BYTEs,vFp)) return FALSE;
      }
      vFp->phyLength = nCACHE_BUFFER_BYTEs * (lastPhyBlockN + 1);
    }
  }
  /****************************************************************************
  * Then extend the file the remaining blocks.
  ****************************************************************************/
  for (blockN = LASTphyBLOCKn(vFp) + 1; blockN <= toBlockN; blockN++) {
     cache = FindCache (vFp, blockN);
     if (cache != NULL) {
       void *buffer = CACHEbufferREADfrom (cache);
       if (buffer == NULL) return FALSE;
       if (!vWrite(nCACHE_BUFFER_BYTEs * blockN,
		   buffer,nCACHE_BUFFER_BYTEs,vFp)) return FALSE;
       cache->modified = FALSE;
     }
     else {
       Byte1 buffer[nCACHE_BUFFER_BYTEs];
#if CLEAR_BYTES
       ClearBytes (buffer, 0, nCACHE_BUFFER_BYTEs - 1);
#endif
       if (!vWrite(nCACHE_BUFFER_BYTEs * blockN,
		   buffer,nCACHE_BUFFER_BYTEs,vFp)) return FALSE;
     }
     vFp->phyLength = nCACHE_BUFFER_BYTEs * (blockN + 1);
  }
  return TRUE;
}
#endif

/******************************************************************************
* PageIn.
* Page in a block from the file.  Returns pointer to cache structure used (or
* NULL if an error occurred).
******************************************************************************/

static vCACHE *PageIn (vFp, blockN)
vFILE *vFp;
long blockN;
{
  long offset, nBytes; vCACHE *cache; void *buffer;
  cache = AllocateBuffer (vFp);
  if (cache == NULL) return NULL;
  offset = blockN * nCACHE_BUFFER_BYTEs;
  nBytes = vFp->phyLength - offset;
  nBytes = MINIMUM (nBytes, nCACHE_BUFFER_BYTEs);
  buffer = CACHEbufferWRITEto (cache);
  if (buffer == NULL) return NULL;
  if (!vRead(offset,buffer,(size_t)nBytes,vFp)) return NULL;
#if CLEAR_BYTES
  ClearBytes (buffer, (int) nBytes, nCACHE_BUFFER_BYTEs - 1);
#endif
  cache->blockN = blockN;
  cache->modified = FALSE;
  (vFp->nPageIns)++;
  return cache;
}

/******************************************************************************
* WriteBlockFromCache.
* Write a block out to the file from a cache buffer.  Returns TRUE if
* successful, FALSE if an error occurred.
******************************************************************************/

static Logical WriteBlockFromCache (vFp, cache, nBytes)
vFILE *vFp;
vCACHE *cache;
size_t nBytes;
{
  long offset; void *buffer;
  offset = nCACHE_BUFFER_BYTEs * cache->blockN;
#if EXTEND_FILE
  if (offset > vFp->phyLength) {
    if (!ExtendFile(vFp,cache->blockN-1)) return FALSE;
  }
#endif
  buffer = CACHEbufferREADfrom (cache);
  if (buffer == NULL) return FALSE;
  if (!vWrite(offset,buffer,nBytes,vFp)) return FALSE;
  vFp->phyLength = MaxLong (vFp->phyLength, (long) (offset + nBytes));
  return TRUE;
}

/******************************************************************************
* WriteBlockFromBuffer.
* Write a block out to the file from the caller's buffer.  Returns TRUE if
* successful, FALSE if an error occurred.
******************************************************************************/

static Logical WriteBlockFromBuffer (vFp, blockN, buffer, nBytes)
vFILE *vFp;
long blockN;
void *buffer;
size_t nBytes;
{
  long offset = nCACHE_BUFFER_BYTEs * blockN;
#if EXTEND_FILE
  if (offset > vFp->phyLength) {
    if (!ExtendFile(vFp,blockN-1)) return FALSE;
  }
#endif
  if (!vWrite(offset,buffer,nBytes,vFp)) return FALSE;
  vFp->phyLength = MaxLong (vFp->phyLength, (long) (offset + nBytes));
  return TRUE;
}

/******************************************************************************
* vRead.
******************************************************************************/

static Logical vRead (offset, buffer, nBytes, vFp)
long offset;
void *buffer;
size_t nBytes;
vFILE *vFp;
{
  int tryN;
  /****************************************************************************
  * Does the scratch file exist?  It doesn't make sense for it not to.
  ****************************************************************************/
  if (vFp->fp == NULL) return FALSE;
  /****************************************************************************
  * Tally a block read.
  ****************************************************************************/
  (vFp->nBlockReads)++;
  /****************************************************************************
  * Read the block.  Multiple attempts are made for optical disks.
  ****************************************************************************/
  for (tryN = 1; tryN <= vMAX_TRYs; tryN++) {
     if (fseek(vFp->fp,offset,vSEEK_SET) == EOF) return FALSE;
     if (fread(buffer,nBytes,1,vFp->fp) == 1) return TRUE;
  }
  return FALSE;
}

/******************************************************************************
* vWrite.
******************************************************************************/

static Logical vWrite(offset,buffer,nBytes,vFp)
long offset;
void *buffer;
size_t nBytes;
vFILE *vFp;
{
  int tryN, pid;
  FILE *fp;
#if defined(__MWERKS__)
  int ii;
#endif
#if defined(win32)
  char *temp;
#endif
  /****************************************************************************
  * Create the scratch file if necessary.  If so, the current file path is
  * actually the scratch directory to be used.
  ****************************************************************************/
  if (vFp->fp == NULL) {
    long i; char *tmpPath, *originalPath; size_t pathLength;
    pathLength = strlen(vFp->path) + 1 + 8 + 1 + EXT_LEN;
    if (pathLength < DU_MAX_DIR_LEN) pathLength = DU_MAX_DIR_LEN;
    tmpPath = (char *) cdf_AllocateMemory ((size_t)pathLength + 1, NULL);
    if (tmpPath == NULL) return FALSE;
    originalPath = (char *) cdf_AllocateMemory ((size_t)pathLength + 1, NULL);
    if (originalPath == NULL) return FALSE;
    pid = GetMyPID();
    if (pid == -1) {
#if defined(win32)
      pid = (int) _getpid();
#else
      pid = (int) getpid();
#endif
      SetMyPID (pid);
      srand((unsigned int)pid*time(NULL));
    }
    if (vFp->path != NULL && strlen(vFp->path) != 0) {
      strcpyX (tmpPath, vFp->path, 0);
      AppendToDir (tmpPath, "");
    } else {
      /* Use the known tmp direcotry */
#if defined(linux) || defined(unix)
      strcpyX (tmpPath, "/tmp/", 0);
#else
#  if defined(win32)
      temp = getenv("TEMP");
      if (temp != NULL) {
        strcpyX (tmpPath, temp, 0);
        strcatX (tmpPath, "\\", 0);
      } else {
        temp = getenv("TMP");
        if (temp != NULL) {
          strcpyX (tmpPath, temp, 0);
          strcatX (tmpPath, "\\", 0);
        }
      }
#  else 
#    if defined(vms)
       strcpyX (tmpPath, "SYS$SCRATCH:", 0);
#    else 
       strcpyX (tmpPath, vFp->path, 0);
#    endif
#  endif
#endif       
    }
    strcpyX (originalPath, tmpPath, 0);
    for (i = 1; i <= MAX_TMP; i++) {
       snprintf (EofS(tmpPath), (size_t) pathLength + 1 - strlen(tmpPath),
                 "TMP%05ld.%s", (long)(rand()%100000), vFp->scratchExt);
       if (!IsReg(tmpPath)) {
	 fp = CDFOpenFile (tmpPath, WRITE_PLUS_a_mode);
	 if (fp != NULL) break; 
       }
       strcpyX (tmpPath, originalPath, 0);
    }
    cdf_FreeMemory (originalPath, NULL);
    if (fp == NULL) {
      cdf_FreeMemory (tmpPath, NULL);
      return FALSE;
    } else {
      cdf_FreeMemory (vFp->path, NULL);
      vFp->path = tmpPath;
      vFp->fp = fp;
    }
    cdf_FreeMemory (originalPath, NULL);
  }
  /****************************************************************************
  * Tally a block write.
  ****************************************************************************/
  (vFp->nBlockWrites)++;

#if defined(__MWERKS__)
  ii = fseek(vFp->fp, (long)0, vSEEK_END); 
#endif

  /****************************************************************************
  * Write the block.  Multiple attempts are made for optical disks.
  ****************************************************************************/
  for (tryN = 1; tryN <= vMAX_TRYs; tryN++) {
     if (fseek(vFp->fp,offset,vSEEK_SET) == EOF) return FALSE;
     if (fwrite(buffer,nBytes,1,vFp->fp) == 1) return TRUE;
  }
  return FALSE;
}

/******************************************************************************
* V_open.
* Open the file and setup vFILE structure.
******************************************************************************/

VISIBLE_PREFIX vFILE *V_open (file_spec, a_mode)
char *file_spec;        /* File specification. */
char *a_mode;           /* Access mode. */
{
  FILE *fp;             /* Temporary file pointer. */
  vFILE *vFp;           /* Pointer to vFILE structure. */
#if defined(vms)
  struct STAT st;       /* Status block from `stat'. */
#endif
  /****************************************************************************
  * Open the file.
  ****************************************************************************/
  fp = CDFOpenFile (file_spec, a_mode);
  if (fp == NULL) return NULL;
#if defined(vms)
  /****************************************************************************
  * If the file is being opened in a mode which may require it to be extended
  * (`r+' [read/write] or `a/a+' [append]), check that the EOF offset in the
  * last block is zero (0).  If not, rewrite the last block out to the end.
  * `r' is not checked because it is read only.
  * `w/w+' is not checked because a new file (with EOF == 0) will have been
  * created.
  ****************************************************************************/
  if (strstr(a_mode,"r+") || strchr(a_mode,'a')) {
    long eof; size_t EOFoffsetInBlock;
    if (fseek(fp,0,vSEEK_END) == EOF) {
      fclose (fp);
      return NULL;
    }
    eof = ftell (fp);
    if (eof == EOF) {
      fclose (fp);
      return NULL;
    }
    EOFoffsetInBlock = eof % nCACHE_BUFFER_BYTEs;
    if (EOFoffsetInBlock != 0) {
      long offsetToLastBlock; char buffer[nCACHE_BUFFER_BYTEs]; size_t numitems; int i;
      offsetToLastBlock = nCACHE_BUFFER_BYTEs * (eof / nCACHE_BUFFER_BYTEs);
      if (fseek(fp,offsetToLastBlock,vSEEK_SET) == EOF) {
	fclose (fp);
	return NULL;
      }
      /* for (i = 0; i < nCACHE_BUFFER_BYTEs; i++) buffer[i] = 0; */
      memset (buffer, 0, (size_t) nCACHE_BUFFER_BYTEs);
      if (fread(buffer,EOFoffsetInBlock,1,fp) != 1) {
	fclose (fp);
	return NULL;
      }
      if (fseek(fp,offsetToLastBlock,vSEEK_SET) == EOF) {
	fclose (fp);
	return NULL;
      }
      if (fwrite(buffer,nCACHE_BUFFER_BYTEs,1,fp) != 1) {
	fclose (fp);
	return NULL;
      }
      if (fclose(fp) == EOF) {
	fclose (fp);
	return NULL;
      }
      fp = CDFOpenFile (file_spec, a_mode);
      if (fp == NULL) return NULL;
    }
  }
#endif
  /****************************************************************************
  * Allocate and load vFILE structure.
  ****************************************************************************/
  vFp = (vFILE *) cdf_AllocateMemory ((size_t)sizeof(vFILE), NULL);
  if (vFp == NULL) {
    fclose (fp);
    return NULL;
  }
  vFp->magic_number = VSTREAM_MAGIC_NUMBER;
  vFp->fp = fp;
  vFp->path = (char *) cdf_AllocateMemory ((size_t)strlen(file_spec) + 1, NULL);
  if (vFp->path == NULL) {
    cdf_FreeMemory (vFp, NULL);
    fclose (fp);
    return NULL;
  }
  else
    strcpyX (vFp->path, file_spec, 0);
  vFp->scratch = FALSE;
  vFp->error = FALSE;
  vFp->eof = FALSE;
  vFp->cacheHead = NULL;
  vFp->cacheTail = NULL;
  vFp->maxBuffers = DEFAULT_nCACHE_BUFFERs;
  vFp->nBuffers = 0;
  vFp->nBlockReads = 0;
  vFp->nBlockWrites = 0;
  vFp->nV_reads = 0;
  vFp->nV_writes = 0;
  vFp->nPageIns = 0;
  vFp->nPageOuts = 0;
  vFp->GDR = NULL;
  vFp->GDR64 = NULL;
  vFp->ADRList = NULL;
  vFp->ADRList64 = NULL;
  /****************************************************************************
  * Determine length of file and set current offset.
  ****************************************************************************/
#if defined(vms)
  /****************************************************************************
  * This method is used on VMS systems in case the file is on a CD-ROM.  Some
  * VMS CD-ROM drivers do not correctly handle the EOF marker of a file. 
  * `stat' might fail, however, if the file specification contains a DECnet
  * node.  If `stat' fails, try the other method before giving up.
  ****************************************************************************/
  if (stat(file_spec,&st) == 0) {
    vFp->length = st.st_size;
    vFp->phyLength = st.st_size;
  }
  else {
#endif
    if (fseek(vFp->fp,0,vSEEK_END) == EOF) {
      cdf_FreeMemory (vFp->path, NULL);
      cdf_FreeMemory (vFp, NULL);
      fclose (vFp->fp);
      return NULL;
    }
    vFp->length = ftell (vFp->fp);
    if (vFp->length == EOF) {
      cdf_FreeMemory (vFp->path, NULL);
      cdf_FreeMemory (vFp, NULL);
      fclose (vFp->fp);
      return NULL;
    }
    vFp->phyLength = vFp->length;
#if defined(vms)
  }
#endif
  vFp->offset = BOO(strchr(a_mode,'a') == NULL,0,vFp->length);
  /****************************************************************************
  * Return pointer to vFILE structure.
  ****************************************************************************/
  return vFp;
}

/******************************************************************************
* V_scratch.
* Creates a scratch file.  Note that the file is not actually created until a
* block needs to be paged out.
******************************************************************************/

VISIBLE_PREFIX vFILE *V_scratch (directory, extension)
char *directory;	/* Directory in which to create the scratch file (if
			   necessary).  If NULL, use the current directory. */
char *extension;	/* Extension to use for the scratch file.  If NULL,
			   `.ich' is used. */
{
  vFILE *vFp;           /* Pointer to vFILE structure. */
  /****************************************************************************
  * Allocate and load vFILE structure.
  ****************************************************************************/
  vFp = (vFILE *) cdf_AllocateMemory ((size_t)sizeof(vFILE), NULL);
  if (vFp == NULL) return NULL;
  vFp->magic_number = VSTREAM_MAGIC_NUMBER;
  vFp->fp = NULL;
  vFp->fh = 0;
  vFp->path = (char *) cdf_AllocateMemory ((size_t)BOO(directory == NULL,
					   0,strlen(directory)) + 1, NULL);
  if (vFp->path == NULL) {
    cdf_FreeMemory (vFp, NULL);
    return NULL;
  }
  else
    strcpyX (vFp->path, BOO(directory == NULL,"",directory), 0);
  strcpyX (vFp->scratchExt, BOO(extension == NULL,"ich",extension), EXT_LEN);
  vFp->scratch = TRUE;
  vFp->error = FALSE;
  vFp->eof = FALSE;
  vFp->cacheHead = NULL;
  vFp->cacheTail = NULL;
  vFp->maxBuffers = DEFAULT_nCACHE_BUFFERs;
  vFp->nBuffers = 0;
  vFp->nBlockReads = 0;
  vFp->nBlockWrites = 0;
  vFp->nV_reads = 0;
  vFp->nV_writes = 0;
  vFp->nPageIns = 0;
  vFp->nPageOuts = 0;
  vFp->length = 0;
  vFp->length64 = (OFF_T) 0;
  vFp->phyLength = 0;
  vFp->phyLength64 = (OFF_T) 0;
  vFp->offset = 0;
  vFp->offset64 = (OFF_T) 0;
  vFp->GDR = NULL;
  vFp->GDR64 = NULL;
  vFp->ADRList = NULL;
  vFp->ADRList64 = NULL;
  /****************************************************************************
  * Return pointer to vFILE structure.
  ****************************************************************************/
  return vFp;
}

/******************************************************************************
* V_setcache.
* Set number of cache buffers.  This can be done at any time after the file
* is opened.  Note that in some cases the new cache size may be the same as
* the old cache size (do nothing).
******************************************************************************/

VISIBLE_PREFIX int V_setcache (vFp, maxBuffers)
vFILE *vFp;             /* Pointer to vFILE structure. */
int maxBuffers;         /* New maximum number of cache buffers. */
{
  if (vFp == NULL) return EOF;
  if (vFp->magic_number != VSTREAM_MAGIC_NUMBER) return EOF;
  if (vFp->error) return EOF;
  if (maxBuffers < 1) return EOF;
  if (maxBuffers > vFp->maxBuffers) {
    /**************************************************************************
    * The number of cache buffers is increasing.
    **************************************************************************/
    vFp->maxBuffers = maxBuffers;
  }
  else {
    if (maxBuffers < vFp->maxBuffers) {
      /************************************************************************
      * The number of cache buffers is decreasing - flush to disk and free
      * the buffers which are going away.
      ************************************************************************/
      vCACHE *cache; int count;
      if (vFp->nBuffers > maxBuffers) {
	for (count = 1,
	     cache = vFp->cacheHead;
	     count < maxBuffers; count++) cache = cache->next;
	if (!FlushCache(vFp,cache->next)) {
	  vFp->error = TRUE;
	  return EOF;
	}
	FreeCache (cache->next);
	cache->next = NULL;
	vFp->cacheTail = cache;
	vFp->nBuffers = maxBuffers;
      }
      vFp->maxBuffers = maxBuffers;
    }
  }
  return 0;
}

/******************************************************************************
* V_seek.
* Seek to a position in the file.
******************************************************************************/

VISIBLE_PREFIX int V_seek (vFp, offset, direction)
vFILE *vFp;             /* Pointer to vFILE structure. */
long offset;            /* New current file offset. */
int direction;          /* Reference for offset. */
{
  if (vFp == NULL) return EOF;
  if (vFp->magic_number != VSTREAM_MAGIC_NUMBER) return EOF;
  if (vFp->error) return EOF;
  vFp->eof = FALSE;		/* Cleared before proceeding. */
  switch (direction) {
    case vSEEK_SET:
      if (offset < 0) return EOF;
      vFp->offset = offset;
      break;
    case vSEEK_CUR:
      if (vFp->offset + offset < 0) return EOF;
      vFp->offset += offset;
      break;
    case vSEEK_END:
      vFp->offset = vFp->length;
      break;
    default:
      return EOF;
  }
  return 0;
}

/******************************************************************************
* V_tell.
* Return current offset (position) in file.  This is the byte offset one past
* the last byte that exists.
******************************************************************************/

VISIBLE_PREFIX long V_tell (vFp)
vFILE *vFp;             /* Pointer to vFILE structure. */
{
  if (vFp == NULL) return EOF;
  if (vFp->magic_number != VSTREAM_MAGIC_NUMBER) return EOF;
  if (vFp->error) return EOF;
  return vFp->offset;
}

/******************************************************************************
* V_eof.
* Returns non-zero if EOF indicator is set.  A read at the EOF must occur
* before the EOF indicator will be set (just like `feof').
******************************************************************************/

VISIBLE_PREFIX int V_eof (vFp)
vFILE *vFp;	/* Pointer to vFILE structure. */
{
  if (vFp == NULL) return EOF;
  if (vFp->magic_number != VSTREAM_MAGIC_NUMBER) return EOF;
  if (vFp->eof) return 1;
  return 0;
}

/******************************************************************************
* V_error.
* Returns non-zero if error indicator is set.
******************************************************************************/

VISIBLE_PREFIX int V_error (vFp)
vFILE *vFp;	/* Pointer to vFILE structure. */
{
  if (vFp == NULL) return EOF;
  if (vFp->magic_number != VSTREAM_MAGIC_NUMBER) return EOF;
  if (vFp->error) return 1;
  return 0;
}

/******************************************************************************
* V_read.
******************************************************************************/

VISIBLE_PREFIX size_t V_read (buffer, item_size, n_items, vFp)
void *buffer;           /* Pointer to buffer. */
size_t item_size;       /* Size (in bytes) of each item to read. */
size_t n_items;         /* Number of items to read. */
vFILE *vFp;             /* Pointer to vFILE structure. */
{
  size_t nBytesX;       /* Total number of bytes in buffer. */
  size_t nBytes;        /* Total number of bytes to read. */
  long remainingItems;	/* Number of items remaining after the offset. */
  size_t nItems;	/* Number of items to actually be read. */
  long firstBlockN;     /* First block involved in read. */
  long lastBlockN;      /* Last block involved in read. */
  int bufferOffset;     /* Offset (bytes) into buffer. */
  long fileOffset;      /* Offset (bytes) into file. */
  size_t xBytes;        /* Number of bytes in a transfer. */
  long blockN;          /* Block number in file (from 0). */
  long atBlockN;        /* Block number in file (from 0) at which to read. */
  vCACHE *cache;        /* Pointer to cache structure. */
  Byte1 *cBuffer;       /* Pointer to cache buffer. */
  int remainingBytes;	/* Number of bytes remaining in a block. */
  /****************************************************************************
  * Validate read.
  ****************************************************************************/
#if defined(DEBUG)
  if (getenv("READ.ERROR") != NULL) return 0;
#endif
  if (vFp == NULL) return 0;
  if (vFp->magic_number != VSTREAM_MAGIC_NUMBER) return 0;
  if (vFp->error) return 0;
  if ((int) item_size <= 0 || (int) n_items <= 0) return 0;
  remainingItems = (vFp->length - vFp->offset) / ((long) item_size);
  if (remainingItems < 1) {
    vFp->eof = TRUE;
    vFp->offset = vFp->length;
    return 0;
  }
  if ((long) n_items > remainingItems) {
    nItems = (size_t) remainingItems;
    vFp->eof = TRUE;		/* File offset set to EOF before returning. */
  }
  else
    nItems = n_items;
  nBytes = nItems * item_size;
  nBytesX = n_items * item_size;
  (vFp->nV_reads)++;
  /****************************************************************************
  * Read from first block...
  ****************************************************************************/
  firstBlockN = vFp->offset / nCACHE_BUFFER_BYTEs;
  bufferOffset = (int) (vFp->offset % nCACHE_BUFFER_BYTEs);
  remainingBytes = nCACHE_BUFFER_BYTEs - bufferOffset;
  xBytes = MINIMUM (nBytes, (size_t) remainingBytes);
  if (xBytes > nBytesX) return 0;
  if (bufferOffset > 0 || xBytes < nCACHE_BUFFER_BYTEs) {
    cache = FindCache (vFp, firstBlockN);
    if (cache == NULL) cache = PageIn (vFp, firstBlockN);
    if (cache == NULL) {
      vFp->error = TRUE;
      return 0;
    }
    cBuffer = CACHEbufferREADfrom (cache);
    if (cBuffer == NULL) {
      vFp->error = TRUE;
      return 0;
    }
    memmove (buffer, cBuffer + bufferOffset, xBytes);
    buffer = (Byte1 *) buffer + xBytes;
    atBlockN = firstBlockN + 1;
  }
  else
    atBlockN = firstBlockN;
  /****************************************************************************
  * Read from remaining blocks...
  ****************************************************************************/
  lastBlockN = (vFp->offset + nBytes - 1) / nCACHE_BUFFER_BYTEs;
  for (blockN = atBlockN; blockN <= lastBlockN; blockN++) {
     xBytes = (size_t) (vFp->offset + nBytes - (nCACHE_BUFFER_BYTEs * blockN));
     xBytes = MINIMUM (xBytes, nCACHE_BUFFER_BYTEs);
     if (xBytes > nBytesX) return 0;
     cache = FindCache (vFp, blockN);
     if (cache != NULL) {
       cBuffer = CACHEbufferREADfrom (cache);
       if (cBuffer == NULL) {
	 vFp->error = TRUE;
	 return 0;
       }
       memmove (buffer, cBuffer, xBytes);
     }
     else {
       if (xBytes < nCACHE_BUFFER_BYTEs) {
	 cache = PageIn (vFp, blockN);
	 if (cache == NULL) {
	   vFp->error = TRUE;
	   return 0;
	 }
	 cBuffer = CACHEbufferREADfrom (cache);
	 if (cBuffer == NULL) {
	   vFp->error = TRUE;
	   return 0;
	 }
	 memmove (buffer, cBuffer, xBytes);
       }
       else {
	 fileOffset = nCACHE_BUFFER_BYTEs * blockN;
	 if (!vRead(fileOffset,buffer,nCACHE_BUFFER_BYTEs,vFp)) {
	   vFp->error = TRUE;
	   return 0;
	 }
       }
     }
     buffer = (Byte1 *) buffer + xBytes;
  }
  /****************************************************************************
  * Increment current file offset or set to EOF if the EOF indicator was set.
  ****************************************************************************/
  vFp->offset = BOO(vFp->eof,vFp->length,vFp->offset + nBytes);
  return nItems;
}

/******************************************************************************
* V_write.
******************************************************************************/

VISIBLE_PREFIX size_t V_write (buffer, item_size, n_items, vFp)
void *buffer;           /* Pointer to buffer. */
size_t item_size;       /* Size (in bytes) of each item to write. */
size_t n_items;         /* Number of items to write. */
vFILE *vFp;             /* Pointer to vFILE structure. */
{
  size_t nBytesX;       /* Total number of bytes in buffer. */
  size_t nBytes;        /* Total number of bytes in write. */
  long firstBlockN;     /* First block involved in write. */
  long lastBlockN;      /* Last block involved in write. */
  int bufferOffset;     /* Offset (bytes) into buffer. */
  long blockN;          /* Block number in file (from 0). */
  long atBlockN;        /* Block number in file (from 0) at which to write. */
  size_t xBytes;        /* Number of bytes in a transfer. */
  vCACHE *cache;        /* Pointer to cache structure. */
  Byte1 *cBuffer;       /* Pointer to cache buffer. */
  size_t nBytesInBlock;	/* Number of bytes to the end of the block. */
  /****************************************************************************
  * Validate write.
  ****************************************************************************/
#if defined(DEBUG)
  if (getenv("WRITE.ERROR") != NULL) return 0;
#endif
  if (vFp == NULL) return 0;
  if (vFp->magic_number != VSTREAM_MAGIC_NUMBER) return 0;
  if (vFp->error) return 0;
  if ((int) item_size <= 0 || (int) n_items <= 0) return 0;
  vFp->eof = FALSE;		/* Cleared before proceeding. */
  nBytes = item_size * n_items;
  nBytesX = n_items * item_size;
  if (nBytes < 1) return 0;
  (vFp->nV_writes)++;
  /****************************************************************************
  * Write to first block...
  * Note that if this is a scratch file, the first block is always placed in
  * the cache (even if a full block).
  ****************************************************************************/
  firstBlockN = vFp->offset / nCACHE_BUFFER_BYTEs;
  bufferOffset = (int) (vFp->offset % nCACHE_BUFFER_BYTEs);
  nBytesInBlock = nCACHE_BUFFER_BYTEs - bufferOffset;
  xBytes = MINIMUM (nBytes, nBytesInBlock);
  if (xBytes > nBytesX) return 0;
  if (vFp->scratch || bufferOffset > 0 || xBytes < nCACHE_BUFFER_BYTEs) {
    cache = FindCache (vFp, firstBlockN);
    if (cache == NULL) {
      if (firstBlockN <= LASTphyBLOCKn(vFp)) {
	cache = PageIn (vFp, firstBlockN);
	if (cache == NULL) {
	  vFp->error = TRUE;
	  return 0;
	}
      }
      else {
	cache = AllocateBuffer (vFp);
	if (cache == NULL) {
	  vFp->error = TRUE;
	  return 0;
	}
	cache->blockN = firstBlockN;
#if CLEAR_BYTES
	cBuffer = CACHEbufferWRITEto (cache);
	if (cBuffer == NULL) {
	  vFp->error = TRUE;
	  return 0;
	}
	ClearBytes (cBuffer, 0, bufferOffset - 1);
	ClearBytes (cBuffer, (int) (bufferOffset + xBytes),
		    nCACHE_BUFFER_BYTEs - 1);
#endif
      }
    }
    cBuffer = CACHEbufferWRITEto (cache);
    if (cBuffer == NULL) {
      vFp->error = TRUE;
      return 0;
    }
    memmove (cBuffer + bufferOffset, buffer, xBytes);
    cache->modified = TRUE;
    vFp->length = MaxLong (vFp->length,(long) (vFp->offset + xBytes));
    buffer = (Byte1 *) buffer + xBytes;
    atBlockN = firstBlockN + 1;
  }
  else
    atBlockN = firstBlockN;
  /****************************************************************************
  * Write to remaining blocks...
  ****************************************************************************/
  lastBlockN = (vFp->offset + nBytes - 1) / nCACHE_BUFFER_BYTEs;
  for (blockN = atBlockN; blockN <= lastBlockN; blockN++) {
     xBytes = (size_t) (vFp->offset + nBytes - (nCACHE_BUFFER_BYTEs * blockN));
     xBytes = MINIMUM (xBytes, nCACHE_BUFFER_BYTEs);
     if (xBytes > nBytesX) return 0;
     /*************************************************************************
     * Is this block in the cache?  If so, move the number of bytes to be
     * written at this block to its cache buffer.
     *************************************************************************/
     cache = FindCache (vFp, blockN);
     if (cache != NULL) {
       cBuffer = CACHEbufferWRITEto (cache);
       if (cBuffer == NULL) {
	 vFp->error = TRUE;
	 return 0;
       }
       memmove (cBuffer, buffer, xBytes);
       cache->modified = TRUE;
     }
     else {
       /***********************************************************************
       * This block is not in the cache.  Is a partial block to be written?
       * Note that if this is a scratch file, the block is always placed in
       * the cache (even if not a partial block).
       ***********************************************************************/
       if (vFp->scratch || xBytes < nCACHE_BUFFER_BYTEs) {
	 if (blockN <= LASTphyBLOCKn(vFp)) {
	   cache = PageIn (vFp, blockN);
	   if (cache == NULL) {
	     vFp->error = TRUE;
	     return 0;
	   }
	 }
	 else {
	   cache = AllocateBuffer (vFp);
	   if (cache == NULL) {
	     vFp->error = TRUE;
	     return 0;
	   }
	   cache->blockN = blockN;
#if CLEAR_BYTES
	   cBuffer = CACHEbufferWRITEto (cache);
	   if (cBuffer == NULL) {
	     vFp->error = TRUE;
	     return 0;
	   }
	   ClearBytes (cBuffer, (int) xBytes, nCACHE_BUFFER_BYTEs - 1);
#endif
	 }
	 cBuffer = CACHEbufferWRITEto (cache);
	 if (cBuffer == NULL) {
	   vFp->error = TRUE;
	   return 0;
	 }
	 memmove (cBuffer, buffer, xBytes);
	 cache->modified = TRUE;
       }
       else {
	 /*********************************************************************
         * A full block is to be written.
	 *********************************************************************/
	 if (!WriteBlockFromBuffer(vFp,blockN,buffer,nCACHE_BUFFER_BYTEs)) {
	   vFp->error = TRUE;
	   return 0;
	 }
       }
     }
     vFp->length = MaxLong (vFp->length, 
			    (long) ((nCACHE_BUFFER_BYTEs * blockN) + xBytes));
     buffer = (Byte1 *) buffer + xBytes;
  }
  /****************************************************************************
  * Increment current file offset.
  ****************************************************************************/
  vFp->offset += nBytes;
  return n_items;
}

/******************************************************************************
* V_getc.
******************************************************************************/

VISIBLE_PREFIX int V_getc (fp)
vFILE *fp;
{
  uByte tmp;
  if (V_read(&tmp,1,1,fp) != 1) return EOF;
  return ((int) tmp);
}

/******************************************************************************
* V_putc.
******************************************************************************/

VISIBLE_PREFIX int V_putc (value, fp)
int value;
vFILE *fp;
{
  uByte tmp = (uByte) value;
  if (V_write(&tmp,1,1,fp) != 1) return EOF;
  return value;
}

/******************************************************************************
* V_clear.
* Marks all cache buffers as unmodified.  This is used with scratch files to
* prevent blocks of unwanted data from being paged out to disk.
******************************************************************************/

VISIBLE_PREFIX int V_clear (vFp)
vFILE *vFp;             /* Pointer to vFILE structure. */
{
  vCACHE *cache;
  if (vFp == NULL) return EOF;
  if (vFp->magic_number != VSTREAM_MAGIC_NUMBER) return EOF;
  if (vFp->error) return EOF;
  for (cache = vFp->cacheHead; cache != NULL; cache = cache->next) {
     cache->modified = FALSE;
  }
  return 0;
}

/******************************************************************************
* V_flush.
* Flush the file to disk.
******************************************************************************/

VISIBLE_PREFIX int V_flush (vFp)
vFILE *vFp;             /* Pointer to vFILE structure. */
{
  /****************************************************************************
  * Validate.
  ****************************************************************************/
  if (vFp == NULL) return EOF;
  if (vFp->magic_number != VSTREAM_MAGIC_NUMBER) return EOF;
  if (vFp->error) return EOF;
  /****************************************************************************
  * Flush cache buffers.  If this is a scratch file, this will cause the file
  * to be created if it hasn't been already (unless nothing has been written).
  ****************************************************************************/
  if (!FlushCache(vFp,vFp->cacheHead)) {
    vFp->error = TRUE;
    return EOF;
  }
  /****************************************************************************
  * Flush file.  Note that the file will not be flushed if this is a scratch
  * file to which nothing has been written.
  ****************************************************************************/
  if (vFp->fp != NULL) {
    if (fflush(vFp->fp) == EOF) {
      vFp->error = TRUE;
      return EOF;
    }
  }
  /****************************************************************************
  * Return success.
  ****************************************************************************/
  return 0;
}

/******************************************************************************
* V_close.
* Returns EOF if an error occurred.
******************************************************************************/

VISIBLE_PREFIX int V_close (vFp, CDF, vStats)
vFILE *vFp;             /* Pointer to vFILE structure. */
struct CDFstruct *CDF;  /* Indicator whether to perform check sum operation. */
vSTATS *vStats;         /* Pointer to statistics structure. */
{
  Logical error = FALSE;        /* Has an error occurred? */
  /****************************************************************************
  * Check if a valid pointer to a vFILE structure.
  ****************************************************************************/
  if (vFp == NULL) return EOF;
  if (vFp->magic_number != VSTREAM_MAGIC_NUMBER) return EOF;
  /****************************************************************************
  * Write cache buffers.  If this is a scratch file, this will cause the file
  * to be created if it hasn't been already (unless nothing has been written).
  ****************************************************************************/
  if (!FlushCache(vFp,vFp->cacheHead)) error = TRUE;
  /****************************************************************************
  * Close the file.  Note that the file will not be closed if this is a
  * scratch file to which nothing has been written.
  ****************************************************************************/
  if (vFp->fp != NULL) {
    if (CDF != NULL && (!CDF->readOnly || CDF->status == READ_WRITE) && 
        CDF->singleFile && (CDF->checksum != NONE_CHECKSUM)) {
/*      if (!FLUSHv(vFp)) error = TRUE; */
      if (!CDFAddChecksum(CDF)) error = TRUE;
    }
    if (fclose(vFp->fp) == EOF) error = TRUE;
  }
  /****************************************************************************
  * Pass back statistics (if requested).
  ****************************************************************************/
  if (vStats != NULL) {
    vStats->maxBuffers = vFp->maxBuffers;
    vStats->nBuffers = vFp->nBuffers;
    vStats->nV_reads = vFp->nV_reads;
    vStats->nV_writes = vFp->nV_writes;
    vStats->nBlockReads = vFp->nBlockReads;
    vStats->nBlockWrites = vFp->nBlockWrites;
    vStats->nPageIns = vFp->nPageIns;
    vStats->nPageOuts = vFp->nPageOuts;
  }
  /****************************************************************************
  * Deallocate cache and vFILE structure.
  ****************************************************************************/
  FreeCache (vFp->cacheHead);
  cdf_FreeMemory (vFp->path, NULL);
  cdf_FreeMemory (vFp, NULL);
  /****************************************************************************
  * Return status.
  ****************************************************************************/
  return BOO(error,EOF,0);
}

/******************************************************************************
* V_delete.
* Returns EOF if an error occurred.
******************************************************************************/

VISIBLE_PREFIX int V_delete (vFp, vStats)
vFILE *vFp;             /* Pointer to vFILE structure. */
vSTATS *vStats;         /* Pointer to statistics structure. */
{
  Logical error = FALSE;        /* Has an error occurred? */
  /****************************************************************************
  * Check if a valid pointer to a vFILE structure.
  ****************************************************************************/
  if (vFp == NULL) return EOF;
  if (vFp->magic_number != VSTREAM_MAGIC_NUMBER) return EOF;
  /****************************************************************************
  * Close the file.
  ****************************************************************************/
  if (vFp->fp != NULL) {
    if (fclose(vFp->fp) == EOF) error = TRUE;
  }
  /****************************************************************************
  * Delete the file (unless it was never created).
  ****************************************************************************/
  if (vFp->fp != NULL) {
    if (!CDFdeleteFile(vFp->path)) error = TRUE;
  }
  /****************************************************************************
  * Pass back statistics (if requested).
  ****************************************************************************/
  if (vStats != NULL) {
    vStats->maxBuffers = vFp->maxBuffers;
    vStats->nBuffers = vFp->nBuffers;
    vStats->nV_reads = vFp->nV_reads;
    vStats->nV_writes = vFp->nV_writes;
    vStats->nBlockReads = vFp->nBlockReads;
    vStats->nBlockWrites = vFp->nBlockWrites;
    vStats->nPageIns = vFp->nPageIns;
    vStats->nPageOuts = vFp->nPageOuts;
  }
  /****************************************************************************
  * Deallocate cache and vFILE structure.
  ****************************************************************************/
  FreeCache (vFp->cacheHead);
  cdf_FreeMemory (vFp->path, NULL);
  cdf_FreeMemory (vFp, NULL);
  /****************************************************************************
  * Return status.
  ****************************************************************************/
  return BOO(error,EOF,0);
}
