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
*  Version 1.0, 27-Mar-12.
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

#define CHUNK MIN_BLOCKING_BYTES_compressed

/****************************************************************************
 * Local functions.
 ****************************************************************************/
static int defgzip(vFILE *src, vFILE *dst, Int32 iSize, Int32 *osize, int level);
static int infgzip(vFILE *src, Int32 iSize, vFILE *dst);

/* Compress from file source to file dest until EOF on source.
   defgzip() returns CDF_OK on success, or ZLIB_COMPRESS_ERROR for any error,
   .e.g., if memory could not be allocated for processing, if an invalid
   compression level is supplied, if the version of zlib.h and the
   version of the library linked do not match, or if there is
   an error reading or writing the files. */

static int defgzip(vFILE *source, vFILE *dest, Int32 iSize, Int32 *osize,
                   int level)
{
    int ret, flush;
    unsigned have;
    z_stream strm;
    uChar in[CHUNK];
    uChar out[CHUNK];
    Int32 remaining; 
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
        strm.avail_in = (unsigned) V_read(in, (size_t) 1, (size_t) mySize,
                                          source);
        flush = (remaining == (Int32) mySize) ? Z_FINISH : Z_NO_FLUSH;
        strm.next_in = in;
        do {
            strm.avail_out = CHUNK;
            strm.next_out = out;
            ret = deflate(&strm, flush);
            if (ret == Z_STREAM_ERROR)
              return ZLIB_COMPRESS_ERROR;
            have = CHUNK - strm.avail_out;
            if (have > 0) {
              if (V_write(out, 1, (size_t) have, dest) != (size_t) have) {
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
   infgzip() returns CDF_OK on success, or ZLIB_UNCOMPRESS_ERROR for any error,
   e.g., if memory could not be allocated for processing, if the deflate data
   is invalid or incomplete, if the version of zlib.h and
   the version of the library linked do not match, or if there
   is an error reading or writing the files. */

static int infgzip(vFILE *source, Int32 iSize, vFILE *dest)
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
        strm.avail_in = (unsigned) V_read(in, (size_t) 1, (size_t) CHUNK,
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
              if (V_write(out, 1, (size_t) have, dest) != (size_t) have) {
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
* CompressGZIP.
******************************************************************************/

STATICforIDL CDFstatus CompressGZIP (srcFp, srcOffset, srcSize, srcError,
                                     destFp, destOffset, destSize, destError,
                                     level)
vFILE *srcFp;
Int32 srcOffset;
Int32 srcSize;
CDFstatus srcError;
vFILE *destFp;
Int32 destOffset;
Int32 *destSize;
CDFstatus destError;
Int32 level;
{
#if SUPPORT_GZIP
  CDFstatus pStatus = CDF_OK;
  if (!SEEKv(srcFp,srcOffset,vSEEK_SET)) return srcError;
  if (!SEEKv(destFp,destOffset,vSEEK_SET)) return destError;
  if (!sX(defgzip(srcFp,destFp,srcSize,destSize,level),&pStatus))
    return pStatus;
  return pStatus;
#else
  return UNKNOWN_COMPRESSION;
#endif
}

/******************************************************************************
* DecompressGZIP.
******************************************************************************/

STATICforIDL CDFstatus DecompressGZIP (srcFp, srcOffset, iSize, srcError,
                                       destFp, destOffset, destError)
vFILE *srcFp;
Int32 srcOffset;
Int32 iSize;
CDFstatus srcError;
vFILE *destFp;
Int32 destOffset;
CDFstatus destError;
{
#if SUPPORT_GZIP
  CDFstatus pStatus = CDF_OK;
  if (!SEEKv(srcFp,srcOffset,vSEEK_SET)) return srcError;
  if (!SEEKv(destFp,destOffset,vSEEK_SET)) return destError;
  if (!sX(infgzip(srcFp,iSize,destFp),&pStatus)) return pStatus;
  return pStatus;
#else
  return UNKNOWN_COMPRESSION;
#endif
}
