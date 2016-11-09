/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF				CDF Standard Interface (II) /FORTRAN.
*
*  Version 3.1, 26-May-05, Raytheon ITSS.
*
*  Modification history:
*
*   V1.0  26-May-05, M Liu   	Original version (for CDF V3.1).
*   V1.1  03-Jan-12, M Liu   	Added cdf_get_zvar_allrecords_varid,
*                               cdf_get_zvar_rangerecords_varid,
*                               cdf_get_var_allrecords_varname,
*                               cdf_get_var_rangerecords_name functions, and
*                               a set of similar functions for put operations.
*
******************************************************************************/

/******************************************************************************
*    This is the second part of the extended FORTRAN interfaces.
******************************************************************************/

#include "cdflib.h"

/******************************************************************************
*  CDF_delete_attr.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_delete_attr__,
	       cdf_delete_attr_,
	       cdf_delete_attr,
	       CDF_DELETE_ATTR)
(id, attr_num, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	   	/* In: Attribute number. */
Int32	  *status;		/* Out: CDF status code. */
{

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     ATTR_, (long) (*attr_num - 1),
			    DELETE_, ATTR_,
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_get_cachesize.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_cachesize__,
               cdf_get_cachesize_,
               cdf_get_cachesize,
               CDF_GET_CACHESIZE)
(id, cache_size, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *cache_size;          /* Out: CDF cache size. */
Int32     *status;              /* Out: CDF status code. */
{
  long cacheSize;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                            CONFIRM_, CDF_CACHESIZE_, &cacheSize,
                            NULL_);
  if (StatusOK(*status)) *cache_size = (Int32) cacheSize;
  return;
}

/******************************************************************************
*  CDF_get_decoding.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_decoding__,
               cdf_get_decoding_,
               cdf_get_decoding,
               CDF_GET_DECODING)
(id, decoding, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *decoding;            /* Out: CDF decoding. */
Int32     *status;              /* Out: CDF status code. */
{
  long decodingT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                            CONFIRM_, CDF_DECODING_, &decodingT,
                            NULL_);
  if (StatusOK(*status)) *decoding = (Int32) decodingT;
  return;
}

/******************************************************************************
*  CDF_get_negtoposfp0_mode.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_negtoposfp0_mode__,
               cdf_get_negtoposfp0_mode_,
               cdf_get_negtoposfp0_mode,
               CDF_GET_NEGTOPOSFP0_MODE)
(id, negtoposfp0, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *negtoposfp0;         /* Out: CDF NEGtoPOSfp0 mode. */
Int32     *status;              /* Out: CDF status code. */
{
  long negtoposfp0T;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                            CONFIRM_, CDF_NEGtoPOSfp0_MODE_, &negtoposfp0T,
                            NULL_);
  if (StatusOK(*status)) *negtoposfp0 = (Int32) negtoposfp0T;
  return;
}

/******************************************************************************
*  CDF_get_readonly_mode.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_readonly_mode__,
               cdf_get_readonly_mode_,
               cdf_get_readonly_mode,
               CDF_GET_READONLY_MODE)
(id, readonlymode, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *readonlymode;        /* Out: CDF read only mode. */
Int32     *status;              /* Out: CDF status code. */
{
  long readOnlyModeT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                            CONFIRM_, CDF_READONLY_MODE_, &readOnlyModeT,
                            NULL_);
  if (StatusOK(*status)) *readonlymode = (Int32) readOnlyModeT;
  return;
}

/******************************************************************************
*  CDF_get_zmode.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zmode__,
               cdf_get_zmode_,
               cdf_get_zmode,
               CDF_GET_ZMODE)
(id, zmode, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *zmode;               /* Out: CDF zMode. */
Int32     *status;              /* Out: CDF status code. */
{
  long zModeT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                            CONFIRM_, CDF_zMODE_, &zModeT,
                            NULL_);
  if (StatusOK(*status)) *zmode = (Int32) zModeT;
  return;
}

/******************************************************************************
*  CDF_get_compress_cachesize.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_compress_cachesize__,
               cdf_get_compress_cachesize_,
               cdf_get_compress_cachesize,
               CDF_GET_COMPRESS_CACHESIZE)
(id, num_buffers, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *num_buffers;         /* Out: CDF compression cache size. */
Int32     *status;              /* Out: CDF status code. */
{
  long numBuffers;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                            CONFIRM_, COMPRESS_CACHESIZE_, &numBuffers,
                            NULL_);
  if (StatusOK(*status)) *num_buffers = (Int32) numBuffers;
  return;
}

/******************************************************************************
*  CDF_get_stage_cachesize.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_stage_cachesize__,
               cdf_get_stage_cachesize_,
               cdf_get_stage_cachesize,
               CDF_GET_STAGE_CACHESIZE)
(id, num_buffers, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *num_buffers;         /* Out: CDF stage cache size. */
Int32     *status;              /* Out: CDF status code. */
{
  long numBuffers;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                            CONFIRM_, STAGE_CACHESIZE_, &numBuffers,
                            NULL_);
  if (StatusOK(*status)) *num_buffers = (Int32) numBuffers;
  return;
}

/******************************************************************************
*  CDF_get_zvar_cachesize.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_cachesize__,
               cdf_get_zvar_cachesize_,
               cdf_get_zvar_cachesize,
               CDF_GET_ZVAR_CACHESIZE)
(id, var_num, cache_size, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *var_num;             /* Out: zVariable number. */
Int32     *cache_size;          /* Out: zVariable's cache size. */
Int32     *status;              /* Out: CDF status code. */
{
  long cacheSize;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num - 1),
                            CONFIRM_, zVAR_CACHESIZE_, &cacheSize,
                            NULL_);
  if (StatusOK(*status)) *cache_size = (Int32) cacheSize;
  return;
}

/******************************************************************************
*  CDF_get_zvar_reservepercent.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_reservepercent__,
               cdf_get_zvar_reservepercent_,
               cdf_get_zvar_reservepercent,
               CDF_GET_ZVAR_RESERVEPERCENT)
(id, var_num, reserve_percent, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *var_num;             /* In: zVariable number. */
Int32     *reserve_percent;     /* Out: zVariable's reserve percentage. */
Int32     *status;              /* Out: CDF status code. */
{
  long reservePercent;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num - 1),
                            CONFIRM_, zVAR_RESERVEPERCENT_, &reservePercent,
                            NULL_);
  if (StatusOK(*status)) *reserve_percent = (Int32) reservePercent;
  return;
}

/******************************************************************************
*  CDF_get_zvar_seqpos.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_seqpos__,
               cdf_get_zvar_seqpos_,
               cdf_get_zvar_seqpos,
               CDF_GET_ZVAR_SEQPOS)
(id, var_num, rec_num, indices, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *var_num;             /* In: zVariable number. */
Int32     *rec_num;             /* out: zVariable's record number. */
Int32     indices[];            /* Out: zVariable's record indices. */
Int32     *status;              /* Out: CDF status code. */
{
  long numDims, recNum, indicesT[CDF_MAX_DIMS];
  int ix;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num - 1),
                            GET_, zVAR_NUMDIMS_, &numDims,
                            NULL_);
  if (StatusBAD(*status)) return;

  *status = (Int32) CDFlib (CONFIRM_, zVAR_SEQPOS_, &recNum, indicesT,
                            NULL_);
  if (StatusOK(*status)) {
    *rec_num = (Int32) (recNum + 1);
    for (ix = 0; ix < (int) numDims; ix++)
       indices[ix] = (Int32) (indicesT[ix] + 1);
  }
  return;
}

/******************************************************************************
*  CDF_get_attr_name.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_attr_name__,
               cdf_get_attr_name_,
               cdf_get_attr_name,
               CDF_GET_ATTR_NAME)
(id, attr_num, attr_name, status Fif_GHOSTARG(len))
Int32     *id;          /* In: CDF identifier. */
Int32     *attr_num;    /* In: CDF attribute number. */
void      *attr_name;   /* Out: CDF attribute name. */
Int32     *status;      /* Out: CDF status code. */
Fif_GHOSTDEF(len)       /* Invisible length of "attr_name"
                           (generated by FORTRAN compiler). */
{
  char attrName[CDF_ATTR_NAME_LEN256];

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     ATTR_, (long) (*attr_num - 1),
                            GET_, ATTR_NAME_, attrName,
                            NULL_);
  if (StatusOK(*status)) {
#if defined(Fif_GHOSTLEN)
    CtoFORTstring (attrName, attr_name, Fif_GHOSTUSE(len));
#else
    CtoFORTstring (attrName, attr_name, CDF_ATTR_NAME_LEN256);
#endif
  }

  return;
}

/******************************************************************************
*  CDF_get_attr_scope.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_attr_scope__,
               cdf_get_attr_scope_,
               cdf_get_attr_scope,
               CDF_GET_ATTR_SCOPE)
(id, attr_num, attr_scope, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *attr_num;            /* In: Attribute number. */
Int32     *attr_scope;          /* Out: Attribute scope. */
Int32     *status;              /* Out: CDF status code. */
{
  long scope;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     ATTR_, (long) (*attr_num - 1),
                            GET_, ATTR_SCOPE_, &scope,
                            NULL_);
  if (StatusBAD(*status)) return;
  *attr_scope = (Int32) scope;

  return;
}

/******************************************************************************
*  CDF_delete_attr_gentry.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_delete_attr_gentry__,
	       cdf_delete_attr_gentry_,
	       cdf_delete_attr_gentry,
	       CDF_DELETE_ATTR_GENTRY)
(id, attr_num, entry_num, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	   	/* In: Attribute number. */
Int32     *entry_num;           /* In: gEntry number. */
Int32	  *status;		/* Out: CDF status code. */
{

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     ATTR_, (long) (*attr_num - 1),
                                     gENTRY_, (long) (*entry_num - 1),
			    DELETE_, gENTRY_,
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_delete_attr_rentry.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_delete_attr_rentry__,
	       cdf_delete_attr_rentry_,
	       cdf_delete_attr_rentry,
	       CDF_DELETE_ATTR_RENTRY)
(id, attr_num, entry_num, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	   	/* In: Attribute number. */
Int32     *entry_num;           /* In: rEntry number. */
Int32	  *status;		/* Out: CDF status code. */
{

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     ATTR_, (long) (*attr_num - 1),
                                     rENTRY_, (long) (*entry_num - 1),
			    DELETE_, rENTRY_,
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_delete_attr_zentry.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_delete_attr_zentry__,
	       cdf_delete_attr_zentry_,
	       cdf_delete_attr_zentry,
	       CDF_DELETE_ATTR_ZENTRY)
(id, attr_num, entry_num, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	   	/* In: Attribute number. */
Int32     *entry_num;           /* In: zEntry number. */
Int32	  *status;		/* Out: CDF status code. */
{

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     ATTR_, (long) (*attr_num - 1),
                                     zENTRY_, (long) (*entry_num - 1),
			    DELETE_, zENTRY_,
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_get_attr_num_gentries.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_attr_num_gentries__,
	       cdf_get_attr_num_gentries_,
	       cdf_get_attr_num_gentries,
	       CDF_GET_ATTR_NUM_GENTRIES)
(id, attr_num, entries, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	   	/* In: Attribute number. */
Int32     *entries;             /* Out: Number of gEntries. */
Int32	  *status;		/* Out: CDF status code. */
{

  long scope;
  long entriesT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     ATTR_, (long) (*attr_num - 1),
                            GET_, ATTR_SCOPE_, &scope,
                            NULL_);
  if (StatusBAD(*status)) return;

  if (GLOBALscope(scope)) {
    *status = (Int32) CDFlib (GET_, ATTR_NUMgENTRIES_, &entriesT,
                              NULL_);
    if (StatusOK(*status))
      *entries = (Int32) entriesT;
    else
      *entries = (Int32) 0;
  } else {
    *entries = (Int32) 0;
    *status = (Int32) ILLEGAL_FOR_SCOPE;
  }
  return;
}

/******************************************************************************
*  CDF_get_attr_num_rentries.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_attr_num_rentries__,
	       cdf_get_attr_num_rentries_,
	       cdf_get_attr_num_rentries,
	       CDF_GET_ATTR_NUM_RENTRIES)
(id, attr_num, entries, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	   	/* In: Attribute number. */
Int32     *entries;             /* Out: Number of rEntries. */
Int32	  *status;		/* Out: CDF status code. */
{

  long scope;
  long entriesT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     ATTR_, (long) (*attr_num - 1),
                            GET_, ATTR_SCOPE_, &scope,
                            NULL_);
  if (StatusBAD(*status)) return;

  if (GLOBALscope(scope)) {
    *entries = (Int32) 0;
    *status = (Int32) ILLEGAL_FOR_SCOPE;
  } else {
    *status = (Int32) CDFlib (GET_, ATTR_NUMrENTRIES_, &entriesT,
                              NULL_);
    if (StatusOK(*status))
      *entries = (Int32) entriesT;
    else
      *entries = (Int32) 0;
  }
  return;
}

/******************************************************************************
*  CDF_get_attr_num_zentries.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_attr_num_zentries__,
	       cdf_get_attr_num_zentries_,
	       cdf_get_attr_num_zentries,
	       CDF_GET_ATTR_NUM_ZENTRIES)
(id, attr_num, entries, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	   	/* In: Attribute number. */
Int32     *entries;             /* Out: Number of zEntries. */
Int32	  *status;		/* Out: CDF status code. */
{

  long scope;
  long entriesT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                      ATTR_, (long) (*attr_num - 1),
                            GET_, ATTR_SCOPE_, &scope,
                            NULL_);
  if (StatusBAD(*status)) return;
  if (GLOBALscope(scope)) {          
    *entries = (Int32) 0;
    *status = (Int32) ILLEGAL_FOR_SCOPE;
  } else {
    *status = (Int32) CDFlib (GET_, ATTR_NUMzENTRIES_, &entriesT,
                              NULL_);
    if (StatusOK(*status))
      *entries = (Int32) entriesT;
    else
      *entries = (Int32) 0;
  }
  return;
}

/******************************************************************************
*  CDF_get_attr_max_gentry.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_attr_max_gentry__,
	       cdf_get_attr_max_gentry_,
	       cdf_get_attr_max_gentry,
	       CDF_GET_ATTR_MAX_GENTRY)
(id, attr_num, entry, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	   	/* In: Attribute number. */
Int32     *entry;               /* Out: Max number of gEntry. */
Int32	  *status;		/* Out: CDF status code. */
{

  long scope;
  long entryT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     ATTR_, (long) (*attr_num - 1), 
                            GET_, ATTR_SCOPE_, &scope,
                            NULL_);
  if (StatusBAD(*status)) return;

  if (GLOBALscope(scope)) {
    *status = (Int32) CDFlib (GET_, ATTR_MAXgENTRY_, &entryT,
			      NULL_);
    if (StatusOK(*status))
      *entry = (Int32) (entryT + 1);
    else
      *entry = (Int32) 0;
  } else {
    *entry = (Int32) 0;
    *status = (Int32) ILLEGAL_FOR_SCOPE;
  }
  return;
}

/******************************************************************************
*  CDF_get_attr_max_rentry.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_attr_max_rentry__,
	       cdf_get_attr_max_rentry_,
	       cdf_get_attr_max_rentry,
	       CDF_GET_ATTR_MAX_RENTRY)
(id, attr_num, entry, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	   	/* In: Attribute number. */
Int32     *entry;               /* Out: Max number of rEntry. */
Int32	  *status;		/* Out: CDF status code. */
{

  long scope;
  long entryT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     ATTR_, (long) (*attr_num - 1),
                            GET_, ATTR_SCOPE_, &scope,
                            NULL_);  

  if (StatusBAD(*status)) return;

  if (GLOBALscope(scope)) {
    *entry = (Int32) 0;
    *status = (Int32) ILLEGAL_FOR_SCOPE;
  } else {
    *status = (Int32) CDFlib (GET_, ATTR_MAXrENTRY_, &entryT,
                              NULL_);
    if (StatusOK(*status))
      *entry = (Int32) (entryT + 1);
    else
      *entry = (Int32) 0;
  }
  return;
}

/******************************************************************************
*  CDF_get_attr_max_zentry.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_attr_max_zentry__,
	       cdf_get_attr_max_zentry_,
	       cdf_get_attr_max_zentry,
	       CDF_GET_ATTR_MAX_ZENTRY)
(id, attr_num, entry, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	   	/* In: Attribute number. */
Int32     *entry;               /* Out: Max number of zEntry. */
Int32	  *status;		/* Out: CDF status code. */
{

  long scope;
  long entryT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),                                                              ATTR_, (long) (*attr_num - 1),                                               GET_, ATTR_SCOPE_, &scope,                                                        NULL_);  

  if (StatusBAD(*status)) return;

  if (GLOBALscope(scope)) {
    *entry = (Int32) 0;
    *status = (Int32) ILLEGAL_FOR_SCOPE;
  } else {
    *status = (Int32) CDFlib (GET_, ATTR_MAXzENTRY_, &entryT,
                              NULL_);
    if (StatusOK(*status))
      *entry = (Int32) (entryT + 1);
    else
      *entry = (Int32) 0;
  }
  return;
}

/******************************************************************************
*  CDF_get_attr_gentry_datatype.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_attr_gentry_datatype__,
	       cdf_get_attr_gentry_datatype_,
	       cdf_get_attr_gentry_datatype,
	       CDF_GET_ATTR_GENTRY_DATATYPE)
(id, attr_num, entry_num, data_type, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	   	/* In: Attribute number. */
Int32     *entry_num;           /* In: gEntry number. */
Int32     *data_type;           /* Out: Data type. */
Int32	  *status;		/* Out: CDF status code. */
{

  long dataTypeT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     ATTR_, (long) (*attr_num - 1),
                                     gENTRY_, (long) (*entry_num - 1),
                            GET_,    gENTRY_DATATYPE_, &dataTypeT,
			    NULL_);
  if (StatusOK(*status)) *data_type = (Int32) dataTypeT;
  return;
}

/******************************************************************************
*  CDF_get_attr_rentry_datatype.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_attr_rentry_datatype__,
	       cdf_get_attr_rentry_datatype_,
	       cdf_get_attr_rentry_datatype,
	       CDF_GET_ATTR_RENTRY_DATATYPE)
(id, attr_num, entry_num, data_type, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	   	/* In: Attribute number. */
Int32     *entry_num;           /* In: rEntry number. */
Int32     *data_type;           /* Out: Data type. */
Int32	  *status;		/* Out: CDF status code. */
{

  long dataTypeT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     ATTR_, (long) (*attr_num - 1),
                                     rENTRY_, (long) (*entry_num - 1),
                            GET_,    rENTRY_DATATYPE_, &dataTypeT,
			    NULL_);
  if (StatusOK(*status)) *data_type = (Int32) dataTypeT;
  return;
}

/******************************************************************************
*  CDF_get_attr_zentry_datatype.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_attr_zentry_datatype__,
	       cdf_get_attr_zentry_datatype_,
	       cdf_get_attr_zentry_datatype,
	       CDF_GET_ATTR_ZENTRY_DATATYPE)
(id, attr_num, entry_num, data_type, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	   	/* In: Attribute number. */
Int32     *entry_num;           /* In: zEntry number. */
Int32     *data_type;           /* Out: Data type. */
Int32	  *status;		/* Out: CDF status code. */
{

  long dataTypeT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     ATTR_, (long) (*attr_num - 1),
                                     zENTRY_, (long) (*entry_num - 1),
                            GET_,    zENTRY_DATATYPE_, &dataTypeT,
			    NULL_);
  if (StatusOK(*status)) *data_type = (Int32) dataTypeT;
  return;
}

/******************************************************************************
*  CDF_get_attr_gentry_numelems.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_attr_gentry_numelems__,
	       cdf_get_attr_gentry_numelems_,
	       cdf_get_attr_gentry_numelems,
	       CDF_GET_ATTR_GENTRY_NUMELEMS)
(id, attr_num, entry_num, num_elems, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	   	/* In: Attribute number. */
Int32     *entry_num;           /* In: gEntry number. */
Int32     *num_elems;           /* Out: Number of elements. */
Int32	  *status;		/* Out: CDF status code. */
{

  long numElemsT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     ATTR_, (long) (*attr_num - 1),
                                     gENTRY_, (long) (*entry_num - 1),
                            GET_,    gENTRY_NUMELEMS_, &numElemsT,
			    NULL_);
  if (StatusOK(*status)) *num_elems = (Int32) numElemsT;
  return;
}

/******************************************************************************
*  CDF_get_attr_rentry_numelems.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_attr_rentry_numelems__,
	       cdf_get_attr_rentry_numelems_,
	       cdf_get_attr_rentry_numelems,
	       CDF_GET_ATTR_RENTRY_NUMELEMS)
(id, attr_num, entry_num, num_elems, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	   	/* In: Attribute number. */
Int32     *entry_num;           /* In: rEntry number. */
Int32     *num_elems;           /* Out: Number of elements. */
Int32	  *status;		/* Out: CDF status code. */
{

  long numElemsT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     ATTR_, (long) (*attr_num - 1),
                                     rENTRY_, (long) (*entry_num - 1),
                            GET_,    rENTRY_NUMELEMS_, &numElemsT,
			    NULL_);
  if (StatusOK(*status)) *num_elems = (Int32) numElemsT;
  return;
}

/******************************************************************************
*  CDF_get_attr_zentry_numelems.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_attr_zentry_numelems__,
	       cdf_get_attr_zentry_numelems_,
	       cdf_get_attr_zentry_numelems,
	       CDF_GET_ATTR_ZENTRY_NUMELEMS)
(id, attr_num, entry_num, num_elems, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	   	/* In: Attribute number. */
Int32     *entry_num;           /* In: zEntry number. */
Int32     *num_elems;           /* Out: Number of elements. */
Int32	  *status;		/* Out: CDF status code. */
{

  long numElemsT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     ATTR_, (long) (*attr_num - 1),
                                     zENTRY_, (long) (*entry_num - 1),
                            GET_,    zENTRY_NUMELEMS_, &numElemsT,
			    NULL_);
  if (StatusOK(*status)) *num_elems = (Int32) numElemsT;
  return;
}

/******************************************************************************
*  CDF_delete_zvar.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_delete_zvar__,
               cdf_delete_zvar_,
               cdf_delete_zvar,
               CDF_DELETE_ZVAR)
(id, var_num, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32     *var_num;	   	/* In: zVariable number. */
Int32     *status;              /* Out: CDF status code. */
{

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
		                     zVAR_, (long) (*var_num -1),
		            DELETE_, zVAR_,
		            NULL_);
  return;
}

/******************************************************************************
* CDF_delete_zvar_recs.
******************************************************************************/
VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_delete_zvar_recs__,
               cdf_delete_zvar_recs_,
               cdf_delete_zvar_recs,
               CDF_DELETE_ZVAR_RECS)
(id, var_num, start_rec, end_rec, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *var_num;             /* In: zVariable number. */
Int32     *start_rec;           /* In: zVariable's start record number. */
Int32     *end_rec;             /* In: zVariable's end record number. */
Int32     *status;              /* Out: CDF status code. */
{
  long startRec = (long) (*start_rec - 1);
  long endRec = (long) (*end_rec - 1);
  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num -1),
                            DELETE_, zVAR_RECORDS_, startRec, endRec,
                            NULL_);
  return;
}

/******************************************************************************
* CDF_delete_zvar_recs_renumber.
******************************************************************************/
VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_delete_zvar_recs_renumber__,
               cdf_delete_zvar_recs_renumber_,
               cdf_delete_zvar_recs_renumber,
               CDF_DELETE_ZVAR_RECS_RENUMBER)
(id, var_num, start_rec, end_rec, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *var_num;             /* In: zVariable number. */
Int32     *start_rec;           /* In: zVariable's start record number. */
Int32     *end_rec;             /* In: zVariable's end record number. */
Int32     *status;              /* Out: CDF status code. */
{
  long startRec = (long) (*start_rec - 1);
  long endRec = (long) (*end_rec - 1);
  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num -1),
                            DELETE_, zVAR_RECORDS_RENUMBER_, startRec, endRec,
                            NULL_);
  return;
}

/******************************************************************************
* CDF_insert_zvar_recs.
******************************************************************************/
VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_insert_zvar_recs__,
               cdf_insert_zvar_recs_,
               cdf_insert_zvar_recs,
               CDF_INSERT_ZVAR_RECS)
(id, var_num, start_rec, num_recs, buffer, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *var_num;             /* In: zVariable number. */
Int32     *start_rec;           /* In: zVariable's start record number. */
Int32     *num_recs;            /* In: Number of records to insert. */
void      *buffer;              /* In: Buffer holding the inserted data. */
Int32     *status;              /* Out: CDF status code. */
{
  long startRec = (long) (*start_rec - 1);
  *status = (Int32) CDFinsertVarRecordsByVarID (Int32ToCDFid(*id), 1,
                                                (long) (*var_num -1),
                                                startRec, (long) *num_recs,
                                                buffer);
  return;
}

/******************************************************************************
*  CDF_get_num_rvars.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_num_rvars__,
               cdf_get_num_rvars_,
               cdf_get_num_rvars,
               CDF_GET_NUM_RVARS)
(id, num_vars, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32     *num_vars;	   	/* Out: Number of rVariables. */
Int32     *status;              /* Out: CDF status code. */
{
  long numVars;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
		            GET_, CDF_NUMrVARS_, &numVars,
		            NULL_);
  if (StatusOK(*status))
    *num_vars = (Int32) numVars;
  return;
}

/******************************************************************************
*  CDF_get_num_zvars.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_num_zvars__,
               cdf_get_num_zvars_,
               cdf_get_num_zvars,
               CDF_GET_NUM_ZVARS)
(id, num_vars, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32     *num_vars;	   	/* Out: Number of zVariables. */
Int32     *status;              /* Out: CDF status code. */
{
  long numVars;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
		            GET_, CDF_NUMzVARS_, &numVars,
		            NULL_);
  if (StatusOK(*status))
    *num_vars = (Int32) numVars;
  return;
}

/******************************************************************************
*  CDF_get_compression.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_compression__,
               cdf_get_compression_,
               cdf_get_compression,
               CDF_GET_COMPRESSION)
(id, compression_type, compression_parms, compression_percent, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32     *compression_type;	/* Out: CDF compression type. */
Int32     compression_parms[];  /* Out: CDF compression parameters. */
Int32     *compression_percent; /* Out: CDF compression percentage. */
Int32     *status;              /* Out: CDF status code. */
{
  long cType, cPct, cParms[CDF_MAX_PARMS];
  int ix;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
		            GET_, CDF_COMPRESSION_, &cType, cParms, &cPct,
		            NULL_);
  if (StatusOK(*status)) {
    *compression_type = (Int32) cType;
    *compression_percent = (Int32) cPct;
    for (ix = 0; ix < CDF_MAX_PARMS; ix++)
       compression_parms[ix] = (Int32) cParms[ix];
  }
  return;
}

/******************************************************************************
*  CDF_get_copyright.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_copyright__,
               cdf_get_copyright_,
               cdf_get_copyright,
               CDF_GET_COPYRIGHT)
(id, copy_right, status Fif_GHOSTARG(len))
Int32	  *id;		   	/* In: CDF identifier. */
void      *copy_right;	        /* Out: Copyright. */
Int32     *status;              /* Out: CDF status code. */
Fif_GHOSTDEF(len)               /* Invisible length of "copy_right"
                                   (generated by FORTRAN compiler). */
{
  char copyRightT[CDF_COPYRIGHT_LEN+1];

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
		            GET_, CDF_COPYRIGHT_, copyRightT,
		            NULL_);
  if (StatusBAD(*status)) {
    return;
  }

#if defined(Fif_GHOSTLEN)
  CtoFORTstring (copyRightT, copy_right, Fif_GHOSTUSE(len));
#else
  CtoFORTstring (copyRightT, copy_right, CDF_COPYRIGHT_LEN);
#endif
  return;
}

/******************************************************************************
*  CDF_get_encoding.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_encoding__,
               cdf_get_encoding_,
               cdf_get_encoding,
               CDF_GET_ENCODING)
(id, encoding, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32     *encoding;		/* Out: CDF encoding. */
Int32     *status;              /* Out: CDF status code. */
{
  long encodingT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
		            GET_, CDF_ENCODING_, &encodingT,
		            NULL_);
  if (StatusOK(*status)) *encoding = (Int32) encodingT;

  return;
}

/******************************************************************************
*  CDF_get_format.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_format__,
               cdf_get_format_,
               cdf_get_format,
               CDF_GET_FORMAT)
(id, format, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32     *format;		/* Out: CDF format. */
Int32     *status;              /* Out: CDF status code. */
{
  long formatT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
		            GET_, CDF_FORMAT_, &formatT,
		            NULL_);
  if (StatusOK(*status)) *format = (Int32) formatT;

  return;
}

/******************************************************************************
*  CDF_get_majority.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_majority__,
               cdf_get_majority_,
               cdf_get_majority,
               CDF_GET_MAJORITY)
(id, majority, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32     *majority;		/* Out: CDF majority. */
Int32     *status;              /* Out: CDF status code. */
{
  long majorityT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
		            GET_, CDF_MAJORITY_, &majorityT,
		            NULL_);
  if (StatusOK(*status)) *majority = (Int32) majorityT;

  return;
}

/******************************************************************************
*  CDF_get_compress_info.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_compression_info__,
               cdf_get_compression_info_,
               cdf_get_compression_info,
               CDF_GET_COMPRESSION_INFO)
(name, compress_type, compress_parms, compress_size, decompress_size,
status Fif_GHOSTARG(len))
void      *name;                /* In: CDF name. */
Int32     *compress_type;       /* Out: CDF compression type. */
Int32     compress_parms[];     /* Out: CDF compression parameters. */
void      *compress_size;       /* Out: CDF compressed size. */
void      *decompress_size;     /* Out: CDF decompressed size. */
Int32     *status;              /* Out: CDF status code. */
Fif_GHOSTDEF(len) 	        /* Invisible length of "name"
                                   (generated by FORTRAN compiler). */
{
  struct STRINGstruct *ssh = NULL;      /* Head of STRINGstruct linked list. */
  long cType, cParms[CDF_MAX_PARMS];
  int ix;

  *status = (Int32) CDFlib (GET_, CDF_INFO_,
#if defined(Fif_DESCR)
                                           DESCRtoREFnul(name,
                                                         CDF_PATHNAME_LEN,
                                                         &ssh),
#endif
#if defined(Fif_GHOSTLEN)
                                           NULterminate(name,
                                                        Fif_GHOSTUSE(len),
                                                        &ssh),
#endif
#if defined(Fif_NOLEN)
                                           FindEndNUL(name,
                                                      CDF_PATHNAME_LEN,&ssh),
#endif
                                             &cType, cParms, compress_size,
                                             decompress_size,
		            NULL_);
  if (StatusOK(*status)) {
    *compress_type = (Int32) cType;
    for (ix = 0; ix < CDF_MAX_PARMS; ix++)
      compress_parms[ix] = (Int32) cParms[ix];
  }
  return;
}

/******************************************************************************
*  CDF_get_num_attrs.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_num_attrs__,
               cdf_get_num_attrs_,
               cdf_get_num_attrs,
               CDF_GET_NUM_ATTRS)
(id, num_attrs, status)
Int32	  *id;		/* In: CDF identifier. */
Int32     *num_attrs;  	/* Out: CDF number of attributes. */
Int32     *status;      /* Out: CDF status code. */
{
  long numAttrs;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
		            GET_, CDF_NUMATTRS_, &numAttrs,
		            NULL_);
  if (StatusOK(*status)) *num_attrs = (Int32) numAttrs;

  return;
}

/******************************************************************************
*  CDF_get_num_gattrs.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_num_gattrs__,
               cdf_get_num_gattrs_,
               cdf_get_num_gattrs,
               CDF_GET_NUM_GATTRS)
(id, num_attrs, status)
Int32	  *id;		/* In: CDF identifier. */
Int32     *num_attrs;  	/* Out: CDF number of gAttributes. */
Int32     *status;      /* Out: CDF status code. */
{
  long numAttrs;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
		            GET_, CDF_NUMgATTRS_, &numAttrs,
		            NULL_);
  if (StatusOK(*status)) *num_attrs = (Int32) numAttrs;

  return;
}

/******************************************************************************
*  CDF_get_num_vattrs.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_num_vattrs__,
               cdf_get_num_vattrs_,
               cdf_get_num_vattrs,
               CDF_GET_NUM_VATTRS)
(id, num_attrs, status)
Int32	  *id;		/* In: CDF identifier. */
Int32     *num_attrs;  	/* Out: CDF number of vAttributes. */
Int32     *status;      /* Out: CDF status code. */
{
  long numAttrs;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
		            GET_, CDF_NUMvATTRS_, &numAttrs,
		            NULL_);
  if (StatusOK(*status)) *num_attrs = (Int32) numAttrs;

  return;
}

/******************************************************************************
*  CDF_get_datatype_size.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_datatype_size__,
               cdf_get_datatype_size_,
               cdf_get_datatype_size,
               CDF_GET_DATATYPE_SIZE)
(datatype, datatype_size, status)
Int32     *datatype;            /* In: CDF data type. */
Int32     *datatype_size;  	/* Out: CDF data type size. */
Int32     *status;      	/* Out: CDF status code. */
{
  long dtSize;

  *status = (Int32) CDFlib (GET_, DATATYPE_SIZE_, (long) *datatype, &dtSize,
		            NULL_);
  if (StatusOK(*status)) *datatype_size = (Int32) dtSize;

  return;
}

/******************************************************************************
*  CDF_get_lib_copyright.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_lib_copyright__,
               cdf_get_lib_copyright_,
               cdf_get_lib_copyright,
               CDF_GET_LIB_COPYRIGHT)
(copy_right, status Fif_GHOSTARG(len))
void      *copy_right;  /* Out: CDF library copy right. */
Int32     *status;      /* Out: CDF status code. */
Fif_GHOSTDEF(len)       /* Invisible length of "copy_right"
                           (generated by FORTRAN compiler). */
{
  char copyRight[CDF_COPYRIGHT_LEN+1];

  *status = (Int32) CDFlib (GET_, LIB_COPYRIGHT_, copyRight,
		            NULL_);
  if (StatusOK(*status)) {
#if defined(Fif_GHOSTLEN)
    CtoFORTstring (copyRight, copy_right, Fif_GHOSTUSE(len));
#else
    CtoFORTstring (copyRight, copy_right, CDF_COPYRIGHT_LEN);
#endif
  }

  return;
}

/******************************************************************************
*  CDF_get_lib_version.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_lib_version__,
               cdf_get_lib_version_,
               cdf_get_lib_version,
               CDF_GET_LIB_VERSION)
(version, release, increment, subincrement, status Fif_GHOSTARG(len))
Int32     *version;  	/* Out: CDF library version. */
Int32     *release;     /* Out: CDF library release. */
Int32     *increment;   /* Out: CDF library increment. */
void      *subincrement;/* Out: CDF library sub_increment. */
Int32     *status;     	/* Out: CDF status code. */
Fif_GHOSTDEF(len)       /* Invisible length of "subincrement"
                           (generated by FORTRAN compiler). */
{
  long versionT, releaseT, incrementT; 
  char subIncrementT;

  *status = (Int32) CDFlib (GET_, LIB_VERSION_, &versionT,
                                  LIB_RELEASE_, &releaseT,
                                  LIB_INCREMENT_, &incrementT,
                                  LIB_subINCREMENT_, &subIncrementT,
		            NULL_);
  if (StatusOK(*status)) {
    *version = (Int32) versionT;
    *release = (Int32) releaseT;
    *increment = (Int32) incrementT;
  }
#if defined(Fif_GHOSTLEN)
  CtoFORTstring (&subIncrementT, subincrement, Fif_GHOSTUSE(len));
#else
  CtoFORTstring (&subIncrementT, subincrement, 1);
#endif

  return;
}

/******************************************************************************
*  CDF_get_version.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_version__,
               cdf_get_version_,
               cdf_get_version,
               CDF_GET_VERSION)
(id, version, release, increment, status)
Int32     *id;          /* In: CDF identifier. */
Int32     *version;     /* Out: CDF version. */
Int32     *release;     /* Out: CDF release. */
Int32     *increment;   /* Out: CDF increment. */
Int32     *status;      /* Out: CDF status code. */
{
  long versionT, releaseT, incrementT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                            GET_, CDF_VERSION_, &versionT,
                                  CDF_RELEASE_, &releaseT,
                                  CDF_INCREMENT_, &incrementT,
                            NULL_);
  if (StatusOK(*status)) {
    *version = (Int32) versionT;
    *release = (Int32) releaseT;
    *increment = (Int32) incrementT;
  }

  return;
}

/******************************************************************************
*  CDF_get_name.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_name__,
               cdf_get_name_,
               cdf_get_name,
               CDF_GET_NAME)
(id, cdf_name, status Fif_GHOSTARG(len))
Int32     *id;     	/* In: CDF identifier. */
void      *cdf_name;    /* Out: CDF name. */
Int32     *status;      /* Out: CDF status code. */
Fif_GHOSTDEF(len)       /* Invisible length of "cdf_name"
                           (generated by FORTRAN compiler). */
{
  char cdfName[CDF_PATHNAME_LEN+1];

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                            CONFIRM_, CDF_NAME_, cdfName,
                            NULL_);
  if (StatusOK(*status)) {
#if defined(Fif_GHOSTLEN)
    CtoFORTstring (cdfName, cdf_name, Fif_GHOSTUSE(len));
#else
    CtoFORTstring (cdfName, cdf_name, CDF_PATHNAME_LEN);
#endif
  }

  return;
}

/******************************************************************************
*  CDF_get_zvar_name.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_name__,
               cdf_get_zvar_name_,
               cdf_get_zvar_name,
               CDF_GET_ZVAR_NAME)
(id, var_num, var_name, status Fif_GHOSTARG(len))
Int32     *id;          /* In: CDF identifier. */
Int32     *var_num;     /* In: CDF zVariable number. */
void      *var_name;    /* Out: CDF zVariable name. */
Int32     *status;      /* Out: CDF status code. */
Fif_GHOSTDEF(len)       /* Invisible length of "var_name"
                           (generated by FORTRAN compiler). */
{
  char varName[CDF_VAR_NAME_LEN256];

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num - 1),
                            GET_, zVAR_NAME_, varName,
                            NULL_);
  if (StatusOK(*status)) {
#if defined(Fif_GHOSTLEN)
    CtoFORTstring (varName, var_name, Fif_GHOSTUSE(len));
#else
    CtoFORTstring (varName, var_name, CDF_VAR_NAME_LEN256);
#endif
  }

  return;
}

/******************************************************************************
*  CDF_get_zvar_maxwrittenrecnum.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_maxwrittenrecnum__,
               cdf_get_zvar_maxwrittenrecnum_,
               cdf_get_zvar_maxwrittenrecnum,
               CDF_GET_ZVAR_MAXWRITTENRECNUM)
(id, var_num, rec_num, status)
Int32     *id;          /* In: CDF identifier. */
Int32     *var_num;     /* In: CDF zVariable number. */
Int32     *rec_num;     /* Out: zVariable's maximum written record number. */
Int32     *status;      /* Out: CDF status code. */
{
  long recNum;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num - 1),
                            GET_, zVAR_MAXREC_, &recNum,
                            NULL_);
  if (StatusOK(*status)) *rec_num = (Int32) (recNum + 1);

  return;
}

/******************************************************************************
*  CDF_get_zvar_maxallocrecnum.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_maxallocrecnum__,
               cdf_get_zvar_maxallocrecnum_,
               cdf_get_zvar_maxallocrecnum,
               CDF_GET_ZVAR_MAXALLOCRECNUM)
(id, var_num, rec_num, status)
Int32     *id;          /* In: CDF identifier. */
Int32     *var_num;     /* In: CDF zVariable number. */
Int32     *rec_num;     /* Out: zVariables's maximum allocated record number.*/
Int32     *status;      /* Out: CDF status code. */
{
  long recNum;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num - 1),
                            GET_, zVAR_MAXallocREC_, &recNum,
                            NULL_);
  if (StatusOK(*status)) *rec_num = (Int32) (recNum + 1);

  return;
}

/******************************************************************************
*  CDF_get_zvars_maxwrittenrecnum.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvars_maxwrittenrecnum__,
               cdf_get_zvars_maxwrittenrecnum_,
               cdf_get_zvars_maxwrittenrecnum,
               CDF_GET_ZVARS_MAXWRITTENRECNUM)
(id, rec_num, status)
Int32     *id;          /* In: CDF identifier. */
Int32     *rec_num;     /* Out: zVariables's maximum written record number. */
Int32     *status;      /* Out: CDF status code. */
{
  long recNum;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                            GET_, zVARs_MAXREC_, &recNum,
                            NULL_);
  if (StatusOK(*status)) *rec_num = (Int32) (recNum + 1);

  return;
}

/******************************************************************************
*  CDF_get_vars_maxwrittenrecnums.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_vars_maxwrittenrecnums__,
               cdf_get_vars_maxwrittenrecnums_,
               cdf_get_vars_maxwrittenrecnums,
               CDF_GET_VARS_MAXWRITTENRECNUMS)
(id, rec_num1, rec_num2, status)
Int32     *id;          /* In: CDF identifier. */
Int32     *rec_num1;    /* Out: rVariables's maximum written record number. */
Int32     *rec_num2;    /* Out: zVariables's maximum written record number. */
Int32     *status;      /* Out: CDF status code. */
{
  long recNum1, recNum2;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                            GET_, rVARs_MAXREC_, &recNum1,
                                  zVARs_MAXREC_, &recNum2,
                            NULL_);
  if (StatusOK(*status)) {
    *rec_num1 = (Int32) (recNum1 + 1);
    *rec_num2 = (Int32) (recNum2 + 1);
  }

  return;
}

/******************************************************************************
*  CDF_get_zvar_allocrecs.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_allocrecs__,
               cdf_get_zvar_allocrecs_,
               cdf_get_zvar_allocrecs,
               CDF_GET_ZVAR_ALLOCRECS)
(id, var_num, num_recs, status)
Int32     *id;          /* In: CDF identifier. */
Int32     *var_num;     /* zVariable number. */
Int32     *num_recs;    /* Out: zVariable's number of allocated records. */
Int32     *status;      /* Out: CDF status code. */
{
  long numRecs;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num - 1),
                            GET_, zVAR_NUMallocRECS_, &numRecs,
                            NULL_);
  if (StatusOK(*status)) *num_recs = (Int32) numRecs;

  return;
}

/******************************************************************************
*  CDF_get_zvar_datatype.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_datatype__,
               cdf_get_zvar_datatype_,
               cdf_get_zvar_datatype,
               CDF_GET_ZVAR_DATATYPE)
(id, var_num, data_type, status)
Int32     *id;          /* In: CDF identifier. */
Int32     *var_num;     /* In: CDF zVariable number. */
Int32     *data_type;   /* Out: zVariable's data type. */
Int32     *status;      /* Out: CDF status code. */
{
  long dataType;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num - 1),
                            GET_, zVAR_DATATYPE_, &dataType,
                            NULL_);
  if (StatusOK(*status)) *data_type = (Int32) dataType;

  return;
}

/******************************************************************************
*  CDF_get_zvar_numelems.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_numelems__,
               cdf_get_zvar_numelems_,
               cdf_get_zvar_numelems,
               CDF_GET_ZVAR_NUMELEMS)
(id, var_num, num_elems, status)
Int32     *id;          /* In: CDF identifier. */
Int32     *var_num;     /* In: CDF zVariable number. */
Int32     *num_elems;   /* Out: zVariable's number of elements. */
Int32     *status;      /* Out: CDF status code. */
{
  long numElems;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num - 1),
                            GET_, zVAR_NUMELEMS_, &numElems,
                            NULL_);
  if (StatusOK(*status)) *num_elems = (Int32) numElems;

  return;
}

/******************************************************************************
*  CDF_get_zvar_numdims.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_numdims__,
               cdf_get_zvar_numdims_,
               cdf_get_zvar_numdims,
               CDF_GET_ZVAR_NUMDIMS)
(id, var_num, num_dims, status)
Int32     *id;          /* In: CDF identifier. */
Int32     *var_num;     /* In: CDF zVariable number. */
Int32     *num_dims;    /* Out: zVariable's number of dimensions. */
Int32     *status;      /* Out: CDF status code. */
{
  long numDims;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num - 1),
                            GET_, zVAR_NUMDIMS_, &numDims,
                            NULL_);
  if (StatusOK(*status)) *num_dims = (Int32) numDims;

  return;
}

/******************************************************************************
*  CDF_get_zvar_dimsizes.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_dimsizes__,
               cdf_get_zvar_dimsizes_,
               cdf_get_zvar_dimsizes,
               CDF_GET_ZVAR_DIMSIZES)
(id, var_num, dim_sizes, status)
Int32     *id;          /* In: CDF identifier. */
Int32     *var_num;     /* In: CDF zVariable number. */
Int32     dim_sizes[];  /* Out: zVariable's dimensional sizes. */
Int32     *status;      /* Out: CDF status code. */
{
  long numDims, dimSizes[CDF_MAX_DIMS];
  int ix;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num - 1),
                            GET_, zVAR_DIMSIZES_, dimSizes,
                                  zVAR_NUMDIMS_, &numDims,
                            NULL_);
  if (StatusOK(*status)) {
    if (numDims > 0) {
      for (ix = 0; ix < (int) numDims; ix++)
        dim_sizes[ix] = (Int32) dimSizes[ix];
    } else {
      dim_sizes[0] = 0;
    }
  }
  return;
}

/******************************************************************************
*  CDF_get_zvar_recvariance.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_recvariance__,
               cdf_get_zvar_recvariance_,
               cdf_get_zvar_recvariance,
               CDF_GET_ZVAR_RECVARIANCE)
(id, var_num, rec_vary, status)
Int32     *id;          /* In: CDF identifier. */
Int32     *var_num;     /* In: CDF zVariable number. */
Int32     *rec_vary;    /* Out: zVariable's record variance. */
Int32     *status;      /* Out: CDF status code. */
{
  long recVary;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num - 1),
                            GET_, zVAR_RECVARY_, &recVary,
                            NULL_);
  if (StatusOK(*status)) *rec_vary = (Int32) recVary;

  return;
}

/******************************************************************************
*  CDF_get_zvar_dimvariances.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_dimvariances__,
               cdf_get_zvar_dimvariances_,
               cdf_get_zvar_dimvariances,
               CDF_GET_ZVAR_DIMVARIANCES)
(id, var_num, dim_varys, status)
Int32     *id;          /* In: CDF identifier. */
Int32     *var_num;     /* In: CDF zVariable number. */
Int32     dim_varys[];  /* Out: zVariable's dimensional variances. */
Int32     *status;      /* Out: CDF status code. */
{
  long numDims, dimVarys[CDF_MAX_DIMS];
  int ix;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num - 1),
                            GET_, zVAR_DIMVARYS_, dimVarys,
                                  zVAR_NUMDIMS_, &numDims,
                            NULL_);
  if (StatusOK(*status)) {
    if (numDims > 0) {
      for (ix = 0; ix < (int) numDims; ix++)
        dim_varys[ix] = (Int32) dimVarys[ix];
    } else {
      dim_varys[0] = 0;
    }
  }
  return;
}

/******************************************************************************
*  CDF_get_zvar_blockingfactor.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_blockingfactor__,
               cdf_get_zvar_blockingfactor_,
               cdf_get_zvar_blockingfactor,
               CDF_GET_ZVAR_BLOCKINGFACTOR)
(id, var_num, blocking_factor, status)
Int32     *id;          /* In: CDF identifier. */
Int32     *var_num;     /* In: CDF zVariable number. */
Int32     *blocking_factor; /* Out: zVariable's blocking factor. */
Int32     *status;      /* Out: CDF status code. */
{
  long blockingFactor;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num - 1),
                            GET_, zVAR_BLOCKINGFACTOR_, &blockingFactor,
                            NULL_);
  if (StatusOK(*status)) *blocking_factor = (Int32) blockingFactor;

  return;
}

/******************************************************************************
*  CDF_get_zvar_compression.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_compression__,
               cdf_get_zvar_compression_,
               cdf_get_zvar_compression,
               CDF_GET_ZVAR_COMPRESSION)
(id, var_num, compress_type, compress_parms, compress_percent, status)
Int32     *id;          /* In: CDF identifier. */
Int32     *var_num;     /* In: CDF zVariable number. */
Int32     *compress_type;   /* Out: zVariable's compression type. */
Int32     compress_parms[]; /* Out: zVariable's compression parameters. */
Int32     *compress_percent;/* Out: zVariable's compression percentage. */
Int32     *status;      /* Out: CDF status code. */
{
  long cType, cPct, cParms[CDF_MAX_PARMS];
  int ix;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num - 1),
                            GET_, zVAR_COMPRESSION_, &cType, cParms, &cPct,
                            NULL_);
  if (StatusOK(*status)) {
    *compress_type = (Int32) cType;
    *compress_percent = (Int32) cPct;
    for (ix = 0; ix < CDF_MAX_PARMS; ix++)
      compress_parms[ix] = (Int32) cParms[ix];
  }
  return;
}

/******************************************************************************
*  CDF_get_zvar_padvalue.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_padvalue__,
               cdf_get_zvar_padvalue_,
               cdf_get_zvar_padvalue,
               CDF_GET_ZVAR_PADVALUE)
(id, var_num, pad_value, status)
Int32     *id;          /* In: CDF identifier. */
Int32     *var_num;     /* In: CDF zVariable number. */
void      *pad_value;   /* Out: zVariable's pad value. */
Int32     *status;      /* Out: CDF status code. */
{
  long dataType;
  
  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num - 1),
                            GET_, zVAR_DATATYPE_, &dataType,
                            NULL_);

  if (StatusBAD(*status)) return;

  *status = (Int32) CDFlib (GET_, zVAR_PADVALUE_,
#if defined(Fif_DESCR)
                                          BOO(STRINGdataType(dataType),
                                              DESCRtoREF(pad_value),pad_value),
#else
                                          pad_value,
#endif
                            NULL_);
  return;
}

/******************************************************************************
*  CDF_get_zvar_sparserecords.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_sparserecords__,
               cdf_get_zvar_sparserecords_,
               cdf_get_zvar_sparserecords,
               CDF_GET_ZVAR_SPARSERECORDS)
(id, var_num, sparse_records, status)
Int32     *id;          /* In: CDF identifier. */
Int32     *var_num;     /* In: CDF zVariable number. */
Int32     *sparse_records; /* Out: zVariable's sparse records. */
Int32     *status;      /* Out: CDF status code. */
{
  long sparseRecords;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num - 1),
                            GET_, zVAR_SPARSERECORDS_, &sparseRecords,
                            NULL_);
  if (StatusOK(*status)) *sparse_records = (Int32) sparseRecords;

  return;
}

/******************************************************************************
*  CDF_get_zvar_numrecs_written.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_numrecs_written__,
               cdf_get_zvar_numrecs_written_,
               cdf_get_zvar_numrecs_written,
               CDF_GET_ZVAR_NUMRECS_WRITTEN)
(id, var_num, num_records, status)
Int32     *id;          /* In: CDF identifier. */
Int32     *var_num;     /* In: CDF zVariable number. */
Int32     *num_records; /* Out: zVariable's written records. */
Int32     *status;      /* Out: CDF status code. */
{
  long numRecords;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num - 1),
                            GET_, zVAR_NUMRECS_, &numRecords,
                            NULL_);
  if (StatusOK(*status)) *num_records = (Int32) numRecords;

  return;
}

/******************************************************************************
*  CDF_get_zvar_seqdata.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_seqdata__,
               cdf_get_zvar_seqdata_,
               cdf_get_zvar_seqdata,
               CDF_GET_ZVAR_SEQDATA)
(id, var_num, data, status)
Int32     *id;          /* In: CDF identifier. */
Int32     *var_num;     /* In: CDF zVariable number. */
void      *data;        /* Out: CDF zVariable's data. */
Int32     *status;      /* Out: CDF status code. */
{
  long dataType;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, (long) (*var_num - 1),
                            GET_, zVAR_DATATYPE_, &dataType,
                            NULL_);

  if (StatusBAD(*status)) return;

  *status = (Int32) CDFlib (GET_, zVAR_SEQDATA_,
#if defined(Fif_DESCR)
                                              BOO(STRINGdataType(dataType),
                                                  DESCRtoREF(data),data),
#else
                                              data,
#endif
                            NULL_);

  return;
}

/******************************************************************************
*  CDF_get_zvar_recorddata.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_recorddata__,
               cdf_get_zvar_recorddata_,
               cdf_get_zvar_recorddata,
               CDF_GET_ZVAR_RECORDDATA)
(id, var_num, rec_num, buffer, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *var_num;             /* In: CDF zVariable number. */
Int32     *rec_num;             /* In: CDF zVariable record number. */
void      *buffer;              /* Out: zVariable's full record data. */
Int32     *status;              /* Out: CDF status code. */
{
  long numVars, varNums[1];

  numVars = 1;
  varNums[0] = (long) (*var_num - 1);

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVARs_RECNUMBER_, (long) (*rec_num - 1),
                            GET_, zVARs_RECDATA_, numVars, varNums, buffer,
                            NULL_);
  return;
}

/******************************************************************************
*  CDF_get_zvar_allrecords_varid.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_allrecords_varid__,
               cdf_get_zvar_allrecords_varid_,
               cdf_get_zvar_allrecords_varid,
               CDF_GET_ZVAR_ALLRECORDS_VARID)
(id, var_num, buffer, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *var_num;             /* In: CDF zVariable number. */
void      *buffer;              /* Out: zVariable's full record data. */
Int32     *status;              /* Out: CDF status code. */
{

  *status = (Int32) CDFgetVarAllRecordsByVarID (Int32ToCDFid(*id), 1,
                                                (long) (*var_num - 1),
                                                buffer);
  return;
}

/******************************************************************************
*  CDF_get_zvar_rangerecords_varid.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_rangerecords_varid__,
               cdf_get_zvar_rangerecords_varid_,
               cdf_get_zvar_rangerecords_varid,
               CDF_GET_ZVAR_RANGERECORDS_VARID)
(id, var_num, start_rec, stop_rec, buffer, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *var_num;             /* In: CDF zVariable number. */
Int32     *start_rec;           /* In: Starting record number. */
Int32     *stop_rec;            /* In: Stopping record number. */
void      *buffer;              /* Out: zVariable's full record data. */
Int32     *status;              /* Out: CDF status code. */
{

  *status = (Int32) CDFgetVarRangeRecordsByVarID (Int32ToCDFid(*id), 1,
                                                  (long) (*var_num - 1),
                                                  (long) (*start_rec - 1),
                                                   (long) (*stop_rec - 1),
                                                   buffer);
  return;
}

/******************************************************************************
*  CDF_get_var_allrecords_varname.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_var_allrecords_varname__,
               cdf_get_var_allrecords_varname_,
               cdf_get_var_allrecords_varname,
               CDF_GET_VAR_ALLRECORDS_VARNAME)
(id, var_name, buffer, status Fif_GHOSTARG(len))
Int32     *id;                  /* In: CDF identifier. */
void      *var_name;            /* In: CDF zVariable name. */
void      *buffer;              /* Out: zVariable's all records' data. */
Int32     *status;              /* Out: CDF status code. */
Fif_GHOSTDEF(len)               /* Invisible length of "var_name"
                                   (generated by FORTRAN compiler). */
{
  int LFS = TRUE;
  struct CDFstruct *CDF;
  struct STRINGstruct *ssh = NULL;
  *status = (Int32) CDFgetVarAllRecordsByVarName (Int32ToCDFid(*id),
#if defined(Fif_DESCR)
                                                  DESCRtoREFnul(var_name,
                                                     (LFS ?
                                                      CDF_VAR_NAME_LEN256 :
                                                      CDF_VAR_NAME_LEN),
                                                     &ssh),
#endif
#if defined(Fif_GHOSTLEN)
                                                  NULterminate(var_name,
                                                     Fif_GHOSTUSE(len),
                                                     &ssh),
#endif
#if defined(Fif_NOLEN)
                                                  FindEndNUL(var_name,
                                                     (LFS ?
                                                      CDF_VAR_NAME_LEN256 :
                                                      CDF_VAR_NAME_LEN),
                                                     &ssh),
#endif
                                                  buffer);
  return;
}

/******************************************************************************
*  CDF_get_var_rangerecords_name.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_var_rangerecords_name__,
               cdf_get_var_rangerecords_name_,
               cdf_get_var_rangerecords_name,
               CDF_GET_VAR_RANGERECORDS_NAME)
(id, var_name, start_rec, stop_rec, buffer, status Fif_GHOSTARG(len))
Int32     *id;                  /* In: CDF identifier. */
void      *var_name;            /* In: CDF zVariable name. */
Int32     *start_rec;           /* In: zVariable's starting record number. */
Int32     *stop_rec;            /* In: zVariable's stopping record number. */
void      *buffer;              /* Out: zVariable's records' data. */
Int32     *status;              /* Out: CDF status code. */
Fif_GHOSTDEF(len)               /* Invisible length of "var_name"
                                   (generated by FORTRAN compiler). */
{
  int LFS = TRUE;
  struct CDFstruct *CDF;
  struct STRINGstruct *ssh = NULL;
  *status = (Int32) CDFgetVarRangeRecordsByVarName (Int32ToCDFid(*id),
#if defined(Fif_DESCR)
                                                    DESCRtoREFnul(var_name,
                                                       (LFS ?
                                                        CDF_VAR_NAME_LEN256 :
                                                        CDF_VAR_NAME_LEN),
                                                       &ssh),
#endif
#if defined(Fif_GHOSTLEN)
                                                    NULterminate(var_name,
                                                       Fif_GHOSTUSE(len),
                                                       &ssh),
#endif
#if defined(Fif_NOLEN)
                                                    FindEndNUL(var_name,
                                                       (LFS ?
                                                        CDF_VAR_NAME_LEN256 :
                                                        CDF_VAR_NAME_LEN),
                                                       &ssh),
#endif
                                                    (long) (*start_rec - 1),
                                                    (long) (*stop_rec - 1),
                                                    buffer);
  return;
}

/******************************************************************************
*  CDF_put_zvar_allrecords_varid.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_put_zvar_allrecords_varid__,
               cdf_put_zvar_allrecords_varid_,
               cdf_put_zvar_allrecords_varid,
               CDF_PUT_ZVAR_ALLRECORDS_VARID)
(id, var_num, num_recs, buffer, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *var_num;             /* In: CDF zVariable number. */
Int32     *num_recs;            /* In: Number of zVariable records to write. */
void      *buffer;              /* Out: zVariable's full record data. */
Int32     *status;              /* Out: CDF status code. */
{

  *status = (Int32) CDFputVarAllRecordsByVarID (Int32ToCDFid(*id), 1,
                                                (long) (*var_num - 1),
                                                (long) *num_recs,
                                                buffer);
  return;
}

/******************************************************************************
*  CDF_put_zvar_rangerecords_varid.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_put_zvar_rangerecords_varid__,
               cdf_put_zvar_rangerecords_varid_,
               cdf_put_zvar_rangerecords_varid,
               CDF_PUT_ZVAR_RANGERECORDS_VARID)
(id, var_num, start_rec, stop_rec, buffer, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *var_num;             /* In: CDF zVariable number. */
Int32     *start_rec;           /* In: Starting record number. */
Int32     *stop_rec;            /* In: Stopping record number. */
void      *buffer;              /* Out: zVariable's full record data. */
Int32     *status;              /* Out: CDF status code. */
{

  *status = (Int32) CDFputVarRangeRecordsByVarID (Int32ToCDFid(*id), 1,
                                                  (long) (*var_num - 1),
                                                  (long) (*start_rec - 1),
                                                  (long) (*stop_rec - 1),
                                                  buffer);
  return;
}

/******************************************************************************
*  CDF_put_var_allrecords_varname.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_put_var_allrecords_varname__,
               cdf_put_var_allrecords_varname_,
               cdf_put_var_allrecords_varname,
               CDF_PUT_VAR_ALLRECORDS_VARNAME)
(id, var_name, num_recs, buffer, status Fif_GHOSTARG(len))
Int32     *id;                  /* In: CDF identifier. */
void      *var_name;            /* In: CDF zVariable name. */
Int32     *num_recs;            /* In: Number of records to write. */
void      *buffer;              /* Out: zVariable's all records' data. */
Int32     *status;              /* Out: CDF status code. */
Fif_GHOSTDEF(len)               /* Invisible length of "var_name"
                                   (generated by FORTRAN compiler). */
{
  int LFS = TRUE;
  struct CDFstruct *CDF;
  struct STRINGstruct *ssh = NULL;
  *status = (Int32) CDFputVarAllRecordsByVarName (Int32ToCDFid(*id),
#if defined(Fif_DESCR)
                                                  DESCRtoREFnul(var_name,
                                                     (LFS ?
                                                      CDF_VAR_NAME_LEN256 :
                                                      CDF_VAR_NAME_LEN),
                                                     &ssh),
#endif
#if defined(Fif_GHOSTLEN)
                                                  NULterminate(var_name,
                                                     Fif_GHOSTUSE(len),
                                                     &ssh),
#endif
#if defined(Fif_NOLEN)
                                                  FindEndNUL(var_name,
                                                     (LFS ?
                                                      CDF_VAR_NAME_LEN256 :
                                                      CDF_VAR_NAME_LEN),
                                                     &ssh),
#endif
                                                  (long) *num_recs,
                                                  buffer);
  return;
}

/******************************************************************************
*  CDF_put_var_rangerecords_name.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_put_var_rangerecords_name__,
               cdf_put_var_rangerecords_name_,
               cdf_put_var_rangerecords_name,
               CDF_PUT_VAR_RANGERECORDS_NAME)
(id, var_name, start_rec, stop_rec, buffer, status Fif_GHOSTARG(len))
Int32     *id;                  /* In: CDF identifier. */
void      *var_name;            /* In: CDF zVariable name. */
Int32     *start_rec;           /* In: zVariable's starting record number. */
Int32     *stop_rec;            /* In: zVariable's stopping record number. */
void      *buffer;              /* Out: zVariable's records' data. */
Int32     *status;              /* Out: CDF status code. */
Fif_GHOSTDEF(len)               /* Invisible length of "var_name"
                                   (generated by FORTRAN compiler). */
{
  int LFS = TRUE;
  struct CDFstruct *CDF;
  struct STRINGstruct *ssh = NULL;
  *status = (Int32) CDFputVarRangeRecordsByVarName (Int32ToCDFid(*id),
#if defined(Fif_DESCR)
                                                    DESCRtoREFnul(var_name,
                                                       (LFS ?
                                                        CDF_VAR_NAME_LEN256 :
                                                        CDF_VAR_NAME_LEN),
                                                       &ssh),
#endif
#if defined(Fif_GHOSTLEN)
                                                    NULterminate(var_name,
                                                       Fif_GHOSTUSE(len),
                                                       &ssh),
#endif
#if defined(Fif_NOLEN)
                                                    FindEndNUL(var_name,
                                                       (LFS ?
                                                        CDF_VAR_NAME_LEN256 :
                                                        CDF_VAR_NAME_LEN),
                                                       &ssh),
#endif
                                                    (long) (*start_rec - 1),
                                                    (long) (*stop_rec - 1),
                                                    buffer);
  return;
}

/******************************************************************************
*  CDF_get_checksum.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_checksum__,
               cdf_get_checksum_,
               cdf_get_checksum,
               CDF_GET_CHECKSUM)
(id, checksum, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *checksum;            /* Out: CDF checksum. */
Int32     *status;              /* Out: CDF status code. */
{
  long checksumT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                            GET_, CDF_CHECKSUM_, &checksumT,
                            NULL_);
  if (StatusOK(*status)) *checksum = (Int32) checksumT;

  return;
}


