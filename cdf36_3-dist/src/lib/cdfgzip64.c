/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                GZIP compression/decompression
*                                           based on ZLIB.
*
*  Version 1.0, 18-Sep-12.
*
*  Modification history:
*
*   V1.0  18-Sep-12, M Liu      Initial version.
*                               Compression/decompression is done by the
*                               open source ZLIB written by Jean-loup Gailly
*                               (compression) and Mark Adler (decompression).
*                               This version is based on their 1.2.7 release.
*                               Refer to: zlib.net for information. 
*   V1.1  18-Oct-13, M Liu      Use zlib V1.2.8 with -DZ_PREFIX mode.
*
******************************************************************************/

#include "zlib/zconf.h"
#include "zlib/zlib.h"
#include "cdflib.h"
#include "cdflib64.h"

#define CHUNK MIN_BLOCKING_BYTES_compressed

/****************************************************************************
 * Local functions.
 ****************************************************************************/
static int defgzip(vFILE *src, vFILE *dst, OFF_T iSize, OFF_T *osize, int level);
static int infgzip(vFILE *src, OFF_T iSize, vFILE *dst);

/* Compress from file source to file dest until EOF on source.
   defgzip() returns CDF_OK on success, or ZLIB_COMPRESS_ERROR for any error,
   .e.g., if memory could not be allocated for processing, an invalid
   compression level is supplied, the version of zlib.h and the
   version of the library linked do not match, or there is
   an error reading or writing the files. */

static int defgzip(vFILE *source, vFILE *dest, OFF_T iSize, OFF_T *osize,
                   int level)
{
    int ret, flush;
    unsigned have;
    z_stream strm;
    uChar in[CHUNK];
    uChar out[CHUNK];
    OFF_T remaining;
    unsigned mySize;

    *osize = 0;
    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;
    ret = deflateInit2(&strm, level, Z_DEFLATED, MAX_WBITS+16, 8, 0);
    if (ret != Z_OK) return ZLIB_COMPRESS_ERROR;
    remaining = iSize;
    do {
        if (remaining > CHUNK) mySize = CHUNK;
        else mySize = (unsigned) remaining;
        strm.avail_in = (unsigned) V_read64(in, (size_t) 1, (size_t) mySize,
                                            source);
        flush = (remaining == (OFF_T) mySize) ? Z_FINISH : Z_NO_FLUSH;
        strm.next_in = in;
        do {
            strm.avail_out = CHUNK;
            strm.next_out = out;
            ret = deflate(&strm, flush);
            if (ret == Z_STREAM_ERROR)
              return ZLIB_COMPRESS_ERROR;
            have = CHUNK - strm.avail_out;
            if (have > 0) {
              if (V_write64(out, 1, (size_t) have, dest) != (size_t) have) {
                (void) deflateEnd(&strm);
                return ZLIB_COMPRESS_ERROR;
              }
              *osize += have;
            }
        } while (strm.avail_out == 0);
        if (strm.avail_in != 0)
          return ZLIB_COMPRESS_ERROR;
        remaining -= (OFF_T) mySize;
    } while (flush != Z_FINISH);
    if (ret != Z_STREAM_END)
      return ZLIB_COMPRESS_ERROR;
    (void) deflateEnd(&strm);
    return CDF_OK;

}

/* Decompress from file source to file dest until stream ends or EOF.
   inflate will set the status to end when a full block of compressed data is
   decompressed. infgzip() returns CDF_OK on success, or ZLIB_UNCOMPRESS_ERROR
   for any error, e.g., if memory could not be allocated for processing, 
   the deflate data is invalid or incomplete, the version of zlib.h and
   the version of the library linked do not match, or there
   is an error reading or writing the files. */

static int infgzip(vFILE *source, OFF_T iSize, vFILE *dest)
{
    int ret;
    unsigned int have;
    z_stream strm;
    uChar in[CHUNK];
    uChar out[CHUNK];
    
    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;
    strm.avail_in = 0;
    strm.next_in = Z_NULL;
    ret = inflateInit2(&strm, 15+16);
    if (ret != Z_OK) return ZLIB_UNCOMPRESS_ERROR;
    do {
        strm.avail_in = (unsigned) V_read64(in, (size_t) 1, (size_t) CHUNK,
                                            source);
        ret = source->eof ? Z_STREAM_END : Z_NO_FLUSH;
        if (strm.avail_in == 0) break;
        strm.next_in = in;
        do {
            strm.avail_out = CHUNK;
            strm.next_out = out;
            ret = inflate(&strm, Z_NO_FLUSH);
            if (ret == Z_STREAM_ERROR)
              return ZLIB_UNCOMPRESS_ERROR;
            switch (ret) {
              case Z_NEED_DICT:
              case Z_DATA_ERROR:
              case Z_MEM_ERROR:
                (void) inflateEnd(&strm);
                return ZLIB_UNCOMPRESS_ERROR;
            }
            have = CHUNK - strm.avail_out;
            if (have > 0) {
               if (V_write64(out, 1, (size_t) have, dest) != (size_t) have) {
                 (void) inflateEnd(&strm);
                 return ZLIB_UNCOMPRESS_ERROR;
               }
            }
        } while (strm.avail_out == 0);
    } while (ret != Z_STREAM_END);
    (void) inflateEnd(&strm);
    return ret == Z_STREAM_END ? CDF_OK : ZLIB_UNCOMPRESS_ERROR;

}

/******************************************************************************
* CompressGZIP_64.
******************************************************************************/

STATICforIDL CDFstatus CompressGZIP_64 (srcFp, srcOffset, srcSize, srcError,
                                        destFp, destOffset, destSize, destError,
                                        level)
vFILE *srcFp;
OFF_T srcOffset;
OFF_T srcSize;
CDFstatus srcError;
vFILE *destFp;
OFF_T destOffset;
OFF_T *destSize;
CDFstatus destError;
Int32 level;
{
#if SUPPORT_GZIP64
  CDFstatus pStatus = CDF_OK;
  if (!SEEKv64(srcFp,srcOffset,vSEEK_SET)) return srcError;
  if (!SEEKv64(destFp,destOffset,vSEEK_SET)) return destError;
  if (!sX(defgzip(srcFp,destFp,srcSize,destSize,level),&pStatus))
    return pStatus;
  return pStatus;
#else
  return UNKNOWN_COMPRESSION;
#endif
}

/******************************************************************************
* DecompressGZIP_64.
******************************************************************************/

STATICforIDL CDFstatus DecompressGZIP_64 (srcFp, srcOffset, iSize, srcError,
                                          destFp, destOffset, destError)
vFILE *srcFp;
OFF_T srcOffset;
OFF_T iSize;
CDFstatus srcError;
vFILE *destFp;
OFF_T destOffset;
CDFstatus destError;
{
#if SUPPORT_GZIP64
  CDFstatus pStatus = CDF_OK;
  if (!SEEKv64(srcFp,srcOffset,vSEEK_SET)) return srcError;
  if (!SEEKv64(destFp,destOffset,vSEEK_SET)) return destError;
  if (!sX(infgzip(srcFp,iSize,destFp),&pStatus)) return pStatus;
  return pStatus;
#else
  return UNKNOWN_COMPRESSION;
#endif
}
