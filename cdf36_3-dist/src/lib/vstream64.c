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
*   V5.0  17-Apr-06, M Liu      Added checksum function before closing.
*   V5.1  25-Apr-07, D Berger   Changed all instances of hardcoded 512 to
*                               nCACHE_BUFFER_BYTEs.
*   V5.2  20-Jun-07, D Berger   Added initialization of variables to support
*                               READONLYon enhancements.
*   V5.3  18-Jun-08, M Liu      Modified the V_read64 and V_write64 to make
*                               sure the actual read/write bytes is within
*                               the buffer size.
*   V5.4  12-Aug-10, M Liu      Fill 0s to each new cache in AllocateBuffer.
*
******************************************************************************/

/******************************************************************************
* Include files.
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"
#include "errno.h"

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
BOO(vFp->phyLength64 == 0,NO_BLOCK,(long) ((vFp->phyLength64-1)/nCACHE_BUFFER_BYTEs))

#if defined(MICROSOFTC_700) && INCLUDEvMEMORY
#define CACHEbufferREADfrom(cache) \
BOO(useVmem,LoadVMemory((MemHandle)cache->ptr,FALSE),cache->ptr)
#define CACHEbufferWRITEto(cache) \
BOO(useVmem,LoadVMemory((MemHandle)cache->ptr,TRUE),cache->ptr)
#else
#define CACHEbufferREADfrom(cache) cache->ptr
#define CACHEbufferWRITEto(cache) cache->ptr
#endif

int pid = -1;

/******************************************************************************
* Local function prototypes.
******************************************************************************/
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
static int CDFOpenFile PROTOARGs((char *file_spec, char *a_mode));
#else
static FILE *CDFOpenFile PROTOARGs((char *file_spec, char *a_mode));
#endif

static Logical FreeCache PROTOARGs((vCACHE *firstCache));
static vCACHE *FindCache PROTOARGs((vFILE *vFp, long blockN));
static int CountCaches PROTOARGs((vFILE *vFp));
static Logical vRead PROTOARGs((
  OFF_T offset, void *buffer, size_t nBytes, vFILE *vFp
));
static Logical vWrite PROTOARGs((
  OFF_T offset, void *buffer, size_t nBytes, vFILE *vFp
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
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
static int CDFOpenFile (file_spec, a_mode)
#else
static FILE *CDFOpenFile (file_spec, a_mode)
#endif
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
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
  int oflag;
  if (!strcmp(a_mode, READ_ONLY_a_mode))
      oflag = READ_ONLY_a_mode_windows_LFS;
  else if (!strcmp(a_mode, READ_PLUS_a_mode))
      oflag = READ_PLUS_a_mode_windows_LFS;
  else if (!strcmp(a_mode, WRITE_PLUS_a_mode))
      oflag = WRITE_PLUS_a_mode_windows_LFS;
  return FOPEN64 (file_spec,oflag,PMODE);
#else
  return FOPEN64 (file_spec,a_mode);
#endif
#endif
}   

/******************************************************************************
* FindCache. If it is found, then move it to the head.
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
* CountCaches.
******************************************************************************/

static int CountCaches (vFp)
vFILE *vFp;
{
  int ix = 0;
  vCACHE *cache = vFp->cacheHead;
  while (cache != NULL) {
    ++ix;
    cache = cache->next;
  }
  return ix;
}

/******************************************************************************
* FlushCache64.
* Write cache buffers to disk from the specified starting buffer to the last
* buffer.
******************************************************************************/

VISIBLE_PREFIX Logical FlushCache64 (vFp, firstCache)
vFILE *vFp;             /* Pointer to vFILE structure. */
vCACHE *firstCache;     /* Pointer to the first cache structure to flush. */
{
  vCACHE *cache; long nBytes; OFF_T tmpBytes;
  OFF_T tmp; 
  for (cache = firstCache; cache != NULL; cache = cache->next) {
     if (cache->modified) {
#if defined(vms)
       nBytes = nCACHE_BUFFER_BYTEs;
#else
       tmp = ((OFF_T) cache->blockN) * nCACHE_BUFFER_BYTEs;
       tmpBytes = vFp->length64 - tmp;
       if (tmpBytes > nCACHE_BUFFER_BYTEs) 
	 nBytes = nCACHE_BUFFER_BYTEs;
       else
         nBytes = (long) tmpBytes;
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
  vCACHE *cache; long nBytes; OFF_T tmpBytes;
#if !defined(vms)
  OFF_T offset;
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
  offset = ((OFF_T) cache->blockN) * nCACHE_BUFFER_BYTEs;
  tmpBytes = vFp->length64 - offset;
  if (tmpBytes > nCACHE_BUFFER_BYTEs)
    nBytes = nCACHE_BUFFER_BYTEs;
  else
    nBytes = (long) tmpBytes;
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
  if (vFp->phyLength64 > 0) {
    long lastPhyBlockN = LASTphyBLOCKn (vFp);
    long nBytes = (long) (vFp->phyLength64 - (OFF_T) lastPhyBlockN *
					      nCACHE_BUFFER_BYTEs);
    if (nBytes < nCACHE_BUFFER_BYTEs) {
      cache = FindCache (vFp, lastPhyBlockN);
      if (cache != NULL) {
	void *buffer = CACHEbufferREADfrom (cache);
	if (buffer == NULL) return FALSE;
	if (!vWrite((OFF_T) lastPhyBlockN * nCACHE_BUFFER_BYTEs,
		    buffer,nCACHE_BUFFER_BYTEs,vFp)) return FALSE;
	cache->modified = FALSE;
      }
      else {
	Byte1 buffer[nCACHE_BUFFER_BYTEs];
	if (!vRead(((OFF_T)lastPhyBlockN) * nCACHE_BUFFER_BYTEs,
		   buffer,(size_t)nBytes,vFp)) return FALSE;
#if CLEAR_BYTES
	ClearBytes (buffer, (int) nBytes, nCACHE_BUFFER_BYTEs - 1);
#endif
	if (!vWrite((OFF_T) lastPhyBlockN * nCACHE_BUFFER_BYTEs,
		    buffer,nCACHE_BUFFER_BYTEs,vFp)) return FALSE;
      }
      vFp->phyLength64 = ((OFF_T) nCACHE_BUFFER_BYTEs) * (lastPhyBlockN + 1);
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
       if (!vWrite((OFF_T) blockN * nCACHE_BUFFER_BYTEs,
		   buffer,nCACHE_BUFFER_BYTEs,vFp)) return FALSE;
       cache->modified = FALSE;
     }
     else {
       Byte1 buffer[nCACHE_BUFFER_BYTEs];
#if CLEAR_BYTES
       ClearBytes (buffer, 0, nCACHE_BUFFER_BYTEs - 1);
#endif
       if (!vWrite((OFF_T) blockN * nCACHE_BUFFER_BYTEs,
		   buffer,nCACHE_BUFFER_BYTEs,vFp)) return FALSE;
     }
     vFp->phyLength64 = ((OFF_T) blockN + 1) * nCACHE_BUFFER_BYTEs;
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
  long nBytes; vCACHE *cache; void *buffer;
  OFF_T offset, tmpBytes;
  cache = AllocateBuffer (vFp);
  if (cache == NULL) return NULL;
  offset = ((OFF_T) blockN) * nCACHE_BUFFER_BYTEs;
  tmpBytes = vFp->phyLength64 - offset;
  if (tmpBytes > nCACHE_BUFFER_BYTEs)
    nBytes = nCACHE_BUFFER_BYTEs;
  else
    nBytes = (long) tmpBytes;
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
  OFF_T offset; void *buffer;
  offset = ((OFF_T) cache->blockN) * nCACHE_BUFFER_BYTEs;
#if EXTEND_FILE
  if (offset > vFp->phyLength64) {
    if (!ExtendFile(vFp,cache->blockN-1)) return FALSE;
  }
#endif
  buffer = CACHEbufferREADfrom (cache);
  if (buffer == NULL) return FALSE;
  if (!vWrite(offset,buffer,nBytes,vFp)) return FALSE;
  vFp->phyLength64 = MaxLongLong (vFp->phyLength64, offset + (OFF_T) nBytes);
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
  OFF_T offset = ((OFF_T) blockN) * nCACHE_BUFFER_BYTEs;
#if EXTEND_FILE
  if (offset > vFp->phyLength64) {
    if (!ExtendFile(vFp,blockN-1)) return FALSE;
  }
#endif
  if (!vWrite(offset,buffer,nBytes,vFp)) return FALSE;
  vFp->phyLength64 = MaxLongLong (vFp->phyLength64, offset + (OFF_T) nBytes);
  return TRUE;
}

/******************************************************************************
* vRead.
******************************************************************************/

static Logical vRead (offset, buffer, nBytes, vFp)
OFF_T offset;
void *buffer;
size_t nBytes;
vFILE *vFp;
{
  int tryN;
  /****************************************************************************
  * Does the scratch file exist?  It doesn't make sense for it not to.
  ****************************************************************************/
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
  if (vFp->fh == 0) return FALSE;
#else
  if (vFp->fp == NULL) return FALSE;
#endif
  /****************************************************************************
  * Tally a block read.
  ****************************************************************************/
  (vFp->nBlockReads)++;
  /****************************************************************************
  * Read the block.  Multiple attempts are made for optical disks.
  ****************************************************************************/
  for (tryN = 1; tryN <= vMAX_TRYs; tryN++) {
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
     if (FSEEK64(vFp->fh,(OFF_T) offset,vSEEK_SET) == EOF) return FALSE;
     if (FREAD64(vFp->fh,buffer,(unsigned int) nBytes) == (int) nBytes)
        return TRUE;
#else
     if (FSEEK64(vFp->fp,(OFF_T) offset,vSEEK_SET) == EOF) return FALSE;
     if (FREAD64(buffer,nBytes,1,vFp->fp) == 1) return TRUE;
#endif
  }
  return FALSE;
}

/******************************************************************************
* vWrite.
******************************************************************************/

static Logical vWrite (offset,buffer,nBytes,vFp)
OFF_T offset;
void *buffer;
size_t nBytes;
vFILE *vFp;
{
  int tryN;
#if defined(__MWERKS__)
  int ii;
#endif
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
  int fh;
#else
  FILE *fp;
#endif
#if defined(win32)
  char *temp;
#endif
#if defined(unix)
  int    ret, endian = 1;
  long long tmpx;
  union {
    long long tmpTime;
    unsigned int tmp2[2];
  } totmp;
  struct timeval nowtime;
#endif
  /****************************************************************************
  * Create the scratch file if necessary.  If so, the current file path is
  * actually the scratch directory to be used.
  ****************************************************************************/
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
  if (vFp->fh == 0) {
#else
  if (vFp->fp == NULL) {
#endif
    long i; char *tmpPath, *originalPath; size_t pathLength;
    pathLength = strlen(vFp->path) + 1 + 8 + 1 + EXT_LEN;
    if (pathLength < DU_MAX_DIR_LEN) pathLength = DU_MAX_DIR_LEN;
    tmpPath = (char *) cdf_AllocateMemory ((size_t)pathLength + 1, NULL);
    if (tmpPath == NULL) return FALSE;
    originalPath = (char *) cdf_AllocateMemory ((size_t)pathLength + 1, NULL);
    if (originalPath == NULL) return FALSE;
    if (pid == -1) {
#if defined(win32)
      pid = (int) _getpid();
#else
      pid = (int) getpid();
#endif
#if defined(unix)
      ret = gettimeofday(&nowtime, NULL);
      tmpx = (long long) nowtime.tv_sec*1000000LL + (int) nowtime.tv_usec;
      tmpx = tmpx % 10000000000000LL;
      totmp.tmpTime = (long long) pid * tmpx;
      srand((unsigned int)(*(char *)&endian==1)?totmp.tmp2[0]:totmp.tmp2[1]);
#else
      srand((unsigned int)pid*time(NULL));
#endif
    }
    if (vFp->path != NULL && strlen(vFp->path) != 0) { 
      strcpyX (tmpPath, vFp->path, 0);
      AppendToDir (tmpPath, "");
    } else {
      /* Use the known tmp direcotry */
#if defined(linux) || defined(unix) || defined(__CYGWIN__) || \
    defined(__MINGw32__)
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
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
         fh = CDFOpenFile (tmpPath, WRITE_PLUS_a_mode);
         if (fh != -1) break; 
#else
	 fp = CDFOpenFile (tmpPath, WRITE_PLUS_a_mode);
	 if (fp != NULL) break;
#endif
       }
       strcpyX (tmpPath, originalPath, 0);
/*       srand((unsigned int)pid*(time(NULL)+i-1)); */
    }
    cdf_FreeMemory (originalPath, NULL);
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
    if (fh == -1) {
#else
    if (fp == NULL) {
#endif
      cdf_FreeMemory (tmpPath, NULL);
      return FALSE;
    } else {
      cdf_FreeMemory (vFp->path, NULL);
      vFp->path = tmpPath;
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
      vFp->fh = fh;
#else
      vFp->fp = fp;
#endif
    }
  }
  /****************************************************************************
  * Tally a block write.
  ****************************************************************************/
  (vFp->nBlockWrites)++;

#if defined(__MWERKS__)
  ii = FSEEK64(vFp->fp, (long)0, vSEEK_END); 
#endif

  /****************************************************************************
  * Write the block.  Multiple attempts are made for optical disks.
  ****************************************************************************/
  for (tryN = 1; tryN <= vMAX_TRYs; tryN++) {
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
     if (FSEEK64(vFp->fh,(OFF_T) offset,vSEEK_SET) == EOF) return FALSE;
     if (FWRITE64(vFp->fh,buffer,(unsigned int)nBytes) == (int)nBytes)
        return TRUE;
#else
     if (FSEEK64(vFp->fp,(OFF_T) offset,vSEEK_SET) == EOF) return FALSE;
     if (FWRITE64(buffer,nBytes,1,vFp->fp) == 1) return TRUE;
#endif
  }
  return FALSE;
}

/******************************************************************************
* V_seek64.
* Seek to a position in the file.
******************************************************************************/

VISIBLE_PREFIX int V_seek64 (vFp, offset, direction)
vFILE *vFp;             /* Pointer to vFILE structure. */
OFF_T offset;           /* New current file offset. */
int direction;          /* Reference for offset. */
{
  if (vFp == NULL) return EOF;
  if (vFp->magic_number != VSTREAM_MAGIC_NUMBER) return EOF;
  if (vFp->error) return EOF;
  vFp->eof = FALSE;		/* Cleared before proceeding. */
  switch (direction) {
    case vSEEK_SET:
      if (offset < (OFF_T) 0) return EOF;
      vFp->offset64 = offset;
      break;
    case vSEEK_CUR:
      if (vFp->offset64 + offset < (OFF_T) 0) return EOF;
      vFp->offset64 += (OFF_T) offset;
      break;
    case vSEEK_END:
      vFp->offset64 = vFp->length64;
      break;
    default:
      return EOF;
  }
  return 0;
}

/******************************************************************************
* V_tell64.
* Return current offset (position) in file.  This is the byte offset one past
* the last byte that exists.
******************************************************************************/

VISIBLE_PREFIX OFF_T V_tell64 (vFp)
vFILE *vFp;             /* Pointer to vFILE structure. */
{
  if (vFp == NULL) return EOF;
  if (vFp->magic_number != VSTREAM_MAGIC_NUMBER) return EOF;
  if (vFp->error) return EOF;
  return vFp->offset64;
}

/******************************************************************************
* V_read64.
******************************************************************************/

VISIBLE_PREFIX size_t V_read64 (buffer, item_size, n_items, vFp)
void *buffer;           /* Pointer to buffer. */
size_t item_size;       /* Size (in bytes) of each item to read. */
size_t n_items;         /* Number of items to read. */
vFILE *vFp;             /* Pointer to vFILE structure. */
{
  size_t nBytesX;       /* Total number of bytes in buffer. */
  size_t nBytes;	/* Total number of bytes to read. */
  OFF_T  remainingItems;/* Number of items remaining after the offset. */
  size_t nItems;	/* Number of items to actually be read. */
  long firstBlockN;    	/* First block involved in read. */
  long lastBlockN;     	/* Last block involved in read. */
  int bufferOffset;   	/* Offset (bytes) into buffer. */
  OFF_T fileOffset;     /* Offset (bytes) into file. */
  size_t xBytes;       	/* Number of bytes in a transfer. */
  long blockN;         	/* Block number in file (from 0). */
  long atBlockN;       	/* Block number in file (from 0) at which to read. */
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
  remainingItems = (OFF_T) ((vFp->length64 - vFp->offset64) / 
			    (OFF_T) item_size);
  if (remainingItems < 1) {
    vFp->eof = TRUE;
    vFp->offset64 = vFp->length64;
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
  firstBlockN = (long) (vFp->offset64 / (OFF_T) nCACHE_BUFFER_BYTEs);
  bufferOffset = (int) ((OFF_T) vFp->offset64 % nCACHE_BUFFER_BYTEs);
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
    memmove (buffer, cBuffer + bufferOffset, (size_t) xBytes);
    buffer = (Byte1 *) buffer + xBytes;
    atBlockN = firstBlockN + 1;
  }
  else
    atBlockN = firstBlockN;
  /****************************************************************************
  * Read from remaining blocks...
  ****************************************************************************/
  lastBlockN = (long) ((vFp->offset64 + (OFF_T) nBytes - 1) / 
			(OFF_T) nCACHE_BUFFER_BYTEs); 
  for (blockN = atBlockN; blockN <= lastBlockN; blockN++) {
     xBytes = (size_t) (((OFF_T)vFp->offset64) + (OFF_T) nBytes - 
		        ((OFF_T) blockN) * nCACHE_BUFFER_BYTEs);
     xBytes = MINIMUM (xBytes, nCACHE_BUFFER_BYTEs);
     if (xBytes > nBytesX) return 0;
     cache = FindCache (vFp, blockN);
     if (cache != NULL) {
       cBuffer = CACHEbufferREADfrom (cache);
       if (cBuffer == NULL) {
	 vFp->error = TRUE;
	 return 0;
       }
       memmove (buffer, cBuffer, (size_t) xBytes);
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
	 memmove (buffer, cBuffer, (size_t) xBytes);
       }
       else {
	 fileOffset = ((OFF_T) blockN) * nCACHE_BUFFER_BYTEs;
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
  vFp->offset64 = BOO(vFp->eof,vFp->length64,vFp->offset64 + (OFF_T)nBytes);
  return nItems;
}

/******************************************************************************
* V_write64.
******************************************************************************/

VISIBLE_PREFIX size_t V_write64 (buffer, item_size, n_items, vFp)
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
  nBytesX = nBytes;
  if (nBytes < 1) return 0;
  (vFp->nV_writes)++;
  /****************************************************************************
  * Write to first block...
  * Note that if this is a scratch file, the first block is always placed in
  * the cache (even if a full block).
  ****************************************************************************/
  firstBlockN = (long) (vFp->offset64 / (OFF_T) nCACHE_BUFFER_BYTEs); 
  bufferOffset = (int) ((OFF_T) vFp->offset64 % nCACHE_BUFFER_BYTEs);
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
    memmove (cBuffer + bufferOffset, buffer, (size_t) xBytes);
    cache->modified = TRUE;
    vFp->length64 = MaxLongLong (vFp->length64, vFp->offset64 + 
				  (OFF_T) xBytes);
    buffer = (Byte1 *) buffer + xBytes;
    atBlockN = firstBlockN + 1;
  }
  else
    atBlockN = firstBlockN;
  /****************************************************************************
  * Write to remaining blocks...
  ****************************************************************************/
  lastBlockN = (long) ((vFp->offset64 + (OFF_T) nBytes - 1) / 
			(OFF_T) nCACHE_BUFFER_BYTEs); 
  for (blockN = atBlockN; blockN <= lastBlockN; blockN++) {
     xBytes = (size_t) (((OFF_T)vFp->offset64) + (OFF_T) nBytes - 
		        ((OFF_T) blockN) * nCACHE_BUFFER_BYTEs);
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
       memmove (cBuffer, buffer, (size_t) xBytes);
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
	 memmove (cBuffer, buffer, (size_t) xBytes);
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
     vFp->length64 = MaxLongLong (vFp->length64, ((OFF_T) blockN) *
			          nCACHE_BUFFER_BYTEs + (OFF_T) xBytes);
     buffer = (Byte1 *) buffer + xBytes;
  }
  /****************************************************************************
  * Increment current file offset.
  ****************************************************************************/
  vFp->offset64 += (OFF_T) nBytes;
  return n_items;
}

/******************************************************************************
* V_open64.
* Open the file and setup vFILE structure.
******************************************************************************/

VISIBLE_PREFIX vFILE *V_open64 (file_spec, a_mode)
char *file_spec;        /* File specification. */
char *a_mode;           /* Access mode. */
{
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
  int fp;
#else
  FILE *fp;             /* Temporary file pointer. */
#endif
  vFILE *vFp;           /* Pointer to vFILE structure. */
#if defined(vms)
  struct STAT st;       /* Status block from `stat'. */
#endif
  /****************************************************************************
  * Open the file.
  ****************************************************************************/
  fp = CDFOpenFile (file_spec, a_mode);
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
  if (fp == -1) return NULL;
#else
  if (fp == NULL) return NULL;
#endif
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
    OFF_T eof, EOFoffsetInBlock;
    if (FSEEK64(fp,0,vSEEK_END) == EOF) {
      FCLOSE64 (fp);
      return NULL;
    }
    eof = FTELL64 (fp);
    if (eof == EOF) {
      FCLOSE64 (fp);
      return NULL;
    }
    EOFoffsetInBlock = eof % nCACHE_BUFFER_BYTEs;
    if (EOFoffsetInBlock != 0) {
      OFF_T offsetToLastBlock; char buffer[nCACHE_BUFFER_BYTEs]; int i;
      offsetToLastBlock = (OFF_T) nCACHE_BUFFER_BYTEs * 
                          (eof / nCACHE_BUFFER_BYTEs);
      if (FSEEK64(fp,offsetToLastBlock,vSEEK_SET) == EOF) {
	FCLOSE64 (fp);
	return NULL;
      }
      /* for (i = 0; i < nCACHE_BUFFER_BYTEs; i++) buffer[i] = 0; */
      memset (buffer, 0, (size_t) nCACHE_BUFFER_BYTEs);
      if (FREAD64(buffer,EOFoffsetInBlock,1,fp) != 1) {
	FCLOSE64 (fp);
	return NULL;
      }
      if (FSEEK64(fp,offsetToLastBlock,vSEEK_SET) == EOF) {
	FCLOSE64 (fp);
	return NULL;
      }
      if (FWRITE64(buffer,nCACHE_BUFFER_BYTEs,1,fp) != 1) {
	FCLOSE64 (fp);
	return NULL;
      }
      if (FCLOSE64(fp) == EOF) {
	FCLOSE64 (fp);
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
    FCLOSE64 (fp);
    return NULL;
  }
  vFp->magic_number = VSTREAM_MAGIC_NUMBER;
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
  vFp->fh = fp;
#else
  vFp->fp = fp;
#endif
  vFp->path = (char *) cdf_AllocateMemory ((size_t)strlen(file_spec) + 1, NULL);
  if (vFp->path == NULL) {
    cdf_FreeMemory (vFp, NULL);
    FCLOSE64 (fp);
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
    vFp->length64 = st.st_size;
    vFp->phyLength64 = st.st_size;
  }
  else {
#endif
    /**************************************************************************
    * Check for LFS based on its current size and magic numbers.
    **************************************************************************/
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
    if (FSEEK64(vFp->fh,(OFF_T)0,vSEEK_END) == EOF) {
#else
    if (FSEEK64(vFp->fp,(OFF_T)0,vSEEK_END) == EOF) {
#endif
      cdf_FreeMemory (vFp->path, NULL);
      cdf_FreeMemory (vFp, NULL);
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
      FCLOSE64 (vFp->fh);
#else
      FCLOSE64 (vFp->fp);
#endif
      return NULL;
    }
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
    vFp->length64 = FTELL64(vFp->fh);
#else
    vFp->length64 = FTELL64(vFp->fp);
#endif
    if (vFp->length64 == (OFF_T) -1) {
      cdf_FreeMemory (vFp->path, NULL);
      cdf_FreeMemory (vFp, NULL);
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
      FCLOSE64 (vFp->fh);
#else
      FCLOSE64 (vFp->fp);
#endif
      return NULL;
    }
    vFp->phyLength64 = vFp->length64;
    vFp->length = vFp->phyLength = 0;
#if defined(vms)
  }
#endif
  vFp->offset = BOO(strchr(a_mode,'a') == NULL,0,vFp->length);
  vFp->offset64 = BOO(strchr(a_mode,'a') == NULL,(OFF_T) 0,vFp->length64);
  /****************************************************************************
  * Return pointer to vFILE structure.
  ****************************************************************************/
  return vFp;
}

/******************************************************************************
* V_setcache64.
* Set number of cache buffers.  This can be done at any time after the file
* is opened.  Note that in some cases the new cache size may be the same as
* the old cache size (do nothing).
******************************************************************************/

VISIBLE_PREFIX int V_setcache64 (vFp, maxBuffers)
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
        if (!FlushCache64(vFp,cache->next)) {
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
* V_flush64.
* Flush the file to disk.
******************************************************************************/

VISIBLE_PREFIX int V_flush64 (vFp)
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
  if (!FlushCache64(vFp,vFp->cacheHead)) {
    vFp->error = TRUE;
    return EOF;
  }
  /****************************************************************************
  * Flush file.  Note that the file will not be flushed if this is a scratch
  * file to which nothing has been written.
  ****************************************************************************/
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
  if (vFp->fh != 0) {
    if (FLUSH64(vFp->fh) == EOF) {
      vFp->error = TRUE;
      return EOF;
    }
  }
#else
  if (vFp->fp != NULL) {
    if (FLUSH64(vFp->fp) == EOF) {
      vFp->error = TRUE;
      return EOF;
    }
  }
#endif
  /****************************************************************************
  * Return success.
  ****************************************************************************/
  return 0;
}

/******************************************************************************
* V_close64.
* Returns EOF if an error occurred.
******************************************************************************/
  
VISIBLE_PREFIX int V_close64 (vFp, CDF, vStats)
vFILE *vFp;             /* Pointer to vFILE structure. */
struct CDFstruct *CDF;  /* Check sum operation indicator if non-NULL. */
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
  if (!FlushCache64(vFp,vFp->cacheHead)) error = TRUE;
  /****************************************************************************
  * Close the file.  Note that the file will not be closed if this is a
  * scratch file to which nothing has been written.
  ****************************************************************************/
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
  if (vFp->fh != 0) {
    if (CDF != NULL && (!CDF->readOnly || CDF->status == READ_WRITE) &&
        CDF->singleFile && (CDF->checksum != NONE_CHECKSUM)) {
/*      if (!FLUSHv64(vFp)) error = TRUE; */
      if (!CDFAddChecksum(CDF)) error = TRUE;
    }
    if (FCLOSE64 (vFp->fh) == EOF) error = TRUE;
  }
#else
  if (vFp->fp != NULL) {
    if (CDF != NULL && (!CDF->readOnly || CDF->status == READ_WRITE) && 
        CDF->singleFile && (CDF->checksum != NONE_CHECKSUM)) {
/*      if (!FLUSHv64(vFp)) error = TRUE; */
      if (!CDFAddChecksum(CDF)) error = TRUE;
    }
    if (FCLOSE64 (vFp->fp) == EOF) error = TRUE;
  }
#endif
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
* V_getc64.
******************************************************************************/

VISIBLE_PREFIX int V_getc64 (fp)
vFILE *fp;
{ 
  uByte tmp;
  if (V_read64(&tmp,1,1,fp) != 1) return EOF;
  return ((int) tmp);
}

/******************************************************************************
* V_putc64.
******************************************************************************/

VISIBLE_PREFIX int V_putc64 (value, fp)
int value;
vFILE *fp;
{ 
  uByte tmp = (uByte) value;
  if (V_write64(&tmp,1,1,fp) != 1) return EOF;
  return value;
}

/******************************************************************************
* V_delete64.
* Returns EOF if an error occurred.
******************************************************************************/

VISIBLE_PREFIX int V_delete64 (vFp, vStats)
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
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
  if (vFp->fh != 0) {
    if (FCLOSE64(vFp->fh) == EOF) error = TRUE;
  }
#else
  if (vFp->fp != NULL) {
    if (FCLOSE64(vFp->fp) == EOF) error = TRUE;
  }
#endif
  /****************************************************************************
  * Delete the file (unless it was never created).
  ****************************************************************************/
#if defined(win32) && (_FILE_OFFSET_BITS == 64)
  if (vFp->fh != 0) {
#else
  if (vFp->fp != NULL) {
#endif
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


/******************************************************************************
* GetMyPID.
* Returns the PID of the session. If -1 then it is not set.
******************************************************************************/

VISIBLE_PREFIX int GetMyPID ()
{
  return pid;
}

/******************************************************************************
* SetMyPID.
* Sets the PID of the session.
******************************************************************************/

VISIBLE_PREFIX void SetMyPID (int npid)
{
  pid = npid;
}

