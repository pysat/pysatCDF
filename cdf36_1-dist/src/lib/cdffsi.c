/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF				 CDF Standard Interface (I) /FORTRAN.
*
*  Version 2.7, 14-Feb-96, Hughes STX.
*
*  Modification history:
*
*   V1.0  22-Jan-91, J Love	Original version (for CDF V2.0).
*   V1.1  11-Feb-91, J Love	Fixed max_rec in CDF_inquire.
*   V2.0   1-Jun-91, J Love	Renamed (was CDF_V2_FORTRAN_IF.C).  Changed
*				for new CDF V2.1 internal structures.  Also
*				calls INTERNAL i/f directly rather than using
*				C i/f.
*   V2.1  30-Jul-91, J Love	Use 'CDFlib'.  If variable data type is CHAR
*				or UCHAR, check for %DESCR (if VMS).
*   V2.2  20-May-92, J Love	Modified for IBM-PC port (Microsoft C/FORTRAN
*				necessary).  CDF V2.2.
*   V2.3   2-Sep-92, J Love	CDF V2.3 (shareable/NeXT).
*   V2.4  18-Oct-93, J Love	CDF V2.4 (DEC Alpha ports [`int' != `long']).
*   V2.4a  2-Mar-94, J Love	Fixed `CDF_create' (dimensionality checking).
*   V2.5   9-Nov-94, J Love	CDF V2.5.
*   V2.6  19-Jan-95, J Love	IRIX 6.0 (64-bit).
*   V2.6a 13-Jun-95, J Love	Linux.
*   V2.7  14-Feb-96, J Love	CDF V2.6 (renamed - previously `cdf_f_if.c').
*   V3.0  28-Aug-01, M Liu      Add CDF_getrVarsRecordData,
*                               CDF_getzVarsRecordData, CDF_putrVarsRecordData,
*                               CDF_putzVarsRecordData.
*   V3.1  26-May-05, M Liu      Initial version (for CDF V3.1).
*   V3.2  23-Oct-08, M Liu      Modified CDF_getrVarsRecordData,
*                               CDF_getzVarsRecordData, CDF_putrVarsRecordData,
*                               CDF_putzVarsRecordData to use the number of
*                               passed variables to allocate buffers.
*   V3.3  10-Jan-09, M Liu      Added CDF_set_Validate and CDF_get_Validate.
*
******************************************************************************/

/******************************************************************************
*
* Notes:
*
*    For the FORTRAN interfaces everything is indexed from one (1) while for
*  the C interfaces everything is indexed from zero (0) [eg. variable and
*  attribute numbers, record numbers, indices, entry numbers].  For this
*  reason one (1) is subtracted from arguments passed in while one (1) is
*  added to arguments passed out (where appropriate).
*
*    The FORTRAN interfaces use INTEGER*4 as the data type for all numerical
*  values passed in/out while the C interfaces use `long'.  These are the same
*  on all supported platforms except the DEC Alpha (where `long' is 8 bytes).
*  For this reason temporary variables/arrays of size `long' are used in the
*  calls to the CDF library (the direct C interface).  This causes a small
*  performance loss on the other platforms mainly when calling CDF_var_get or
*  CDF_var_put (but then CDF_var_hyper_get and CDF_var_hyper_put should have
*  been used instead).
*
******************************************************************************/

/******************************************************************************
*
*  Notes for VMS version:
*
*	To make the user's life a little easier, all names and attribute
*  values (for attributes of data types CDF_CHAR and CDF_UCHAR) may be passed
*  in and out by either reference or descriptor.  The default passing mode
*  for an embedded character string (e.g., CALL subr (..., 'string', ...)) or
*  a CHARACTER variable symbol (e.g., CALL subr (..., ATTR_NAME, ...) where
*  ATTR_NAME is defined as CHARACTER*8) is by descriptor when passing from
*  FORTRAN to C in VMS.
*
*	An embedded character string could be enclosed in %REF() to force
*  passing by reference since the FORTRAN compiler puts a NUL character at
*  the end of these strings as expected by the CDF V2.0 library (written in
*  C).  Enclosing a CHARACTER variable symbol in %REF() will result in
*  an error, however, because a NUL character is not placed at the ends of
*  these type strings by the FORTRAN compiler.  The user would have to
*  supply the terminating NUL.  By letting the passing mode default to
*  by-descriptor, this interface will supply the terminating NUL.
*
*	The main difference here from CDF Version 1 is that the %REF() is
*  not needed when passing out names and attribute values.  Also, variable
*  values for character variables need not be enclosed in %REF().
*
******************************************************************************/

/******************************************************************************
*
*  Notes for UNIX version:
*
*    All passing between FORTRAN and C on UNIX systems is done by reference.
*  When character strings are passed between FORTRAN and C, extra arguments
*  are added to the argument list containing the lengths of those character
*  strings (EXCEPT on NeXT machines - FORTRAN applications must NUL-terminate
*  passed character strings).
*
*    Entry points have been made lowercase because the FORTRAN compiler
*  converts all uppercase characters to lowercase in entry points.  This way
*  the linker will find everything.  Unix FORTRAN compilers and linkers also
*  seem to like trailing '_'s (except on the IBM-RS6000/AIX, HP9000/HP-UX,
*  and NeXT/Mach).
*
******************************************************************************/

/******************************************************************************
*
*  Notes for IBM PC version:
*
*    The Standard Interface/FORTRAN is supported for the Microsoft FORTRAN
*  compiler.  Microsoft C must be used to compile/link the CDF library
*  (including this file).
*
******************************************************************************/

/******************************************************************************
*
*  Notes for Macintosh version (MPW Fortran):
*
*    The Standard Interface/FORTRAN is supported for the MPW FORTRAN compiler.
*  MPW C must be used to compile/link the CDF library (including this file).
*
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"

/******************************************************************************
*  CDF_create.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_create__,cdf_create_,cdf_create,CDF_CREATE)
(CDF_name, num_dims, dim_sizes, encoding, majority, id, status
 Fif_GHOSTARG(len))
char	  *CDF_name;		/* In: CDF name. */
Int32	  *num_dims;		/* In: Number of dimensions. */
Int32	  dim_sizes[];		/* In: Dimension sizes. */
Int32	  *encoding;		/* In: Host or network. */
Int32	  *majority;		/* In: Row or column major. */
Int32	  *id;			/* Out: CDF identifier. */
Int32	  *status;		/* Out: CDF status code. */
Fif_GHOSTDEF(len)		/* Invisible length of "CDF_name"
				   (generated by FORTRAN compiler). */
{
  struct STRINGstruct *ssh = NULL;	/* Head of STRINGstruct linked list. */
  int dimN;
  long dimSizesT[CDF_MAX_DIMS];
  CDFid idT;
  long num_dimsT = (long) *num_dims;
  long encodingT = (long) *encoding;
  long majorityT = (long) *majority;

  if (num_dimsT < 0 || num_dimsT > CDF_MAX_DIMS) {
    *status = (Int32) BAD_NUM_DIMS;
    return;
  }

  for (dimN = 0; dimN < num_dimsT; dimN++) {
     dimSizesT[dimN] = (long) dim_sizes[dimN];
  }

  *status = (Int32) CDFlib (CREATE_, CDF_,
#if defined(Fif_DESCR)
					   DESCRtoREFnul(CDF_name,
							 CDF_PATHNAME_LEN,
							 &ssh),
#endif
#if defined(Fif_GHOSTLEN)
					   NULterminate(CDF_name,
							Fif_GHOSTUSE(len),
							&ssh),
#endif
#if defined(Fif_NOLEN)
					   FindEndNUL(CDF_name,
						      CDF_PATHNAME_LEN,&ssh),
#endif
					   num_dimsT, dimSizesT, &idT,
			    NULL_);
  FreeStrings (ssh);
  if (StatusBAD(*status)) return;

  *status = (Int32) CDFlib (PUT_, CDF_ENCODING_, encodingT,
				  CDF_MAJORITY_, majorityT,
			    NULL_);
  if (StatusBAD(*status)) {
    CDFlib (DELETE_, CDF_,
	    NULL_);
    return;
  }

  *id = CDFidToInt32 (idT);
  return;
}

/******************************************************************************
*  CDF_create_cdf.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_create_cdf__,
               cdf_create_cdf_,
               cdf_create_cdf,
               CDF_CREATE_CDF)
(CDF_name, id, status
 Fif_GHOSTARG(len))
char	  *CDF_name;		/* In: CDF name. */
Int32	  *id;			/* Out: CDF identifier. */
Int32	  *status;		/* Out: CDF status code. */
Fif_GHOSTDEF(len)		/* Invisible length of "CDF_name"
				   (generated by FORTRAN compiler). */
{
  struct STRINGstruct *ssh = NULL;	/* Head of STRINGstruct linked list. */
  long dimSizesT[1] = {0};
  CDFid idT;

  *status = (Int32) CDFlib (CREATE_, CDF_,
#if defined(Fif_DESCR)
					   DESCRtoREFnul(CDF_name,
							 CDF_PATHNAME_LEN,
							 &ssh),
#endif
#if defined(Fif_GHOSTLEN)
					   NULterminate(CDF_name,
							Fif_GHOSTUSE(len),
							&ssh),
#endif
#if defined(Fif_NOLEN)
					   FindEndNUL(CDF_name,
						      CDF_PATHNAME_LEN,&ssh),
#endif
					   0L, dimSizesT, &idT,
			    NULL_);

  FreeStrings (ssh);
  if (StatusBAD(*status)) return;

  *id = CDFidToInt32 (idT);
  return;
}

/******************************************************************************
*  CDF_open.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_open__,cdf_open_,cdf_open,CDF_OPEN)
(CDF_name, id, status Fif_GHOSTARG(len))
void	  *CDF_name;		/* In: CDF name. */
Int32	  *id;			/* Out: CDF identifier. */
Int32     *status;		/* Out: CDF status code. */
Fif_GHOSTDEF(len)		/* Invisible length of "CDF_name"
				   (generated by FORTRAN compiler). */
{
  struct STRINGstruct *ssh = NULL;	/* Head of STRINGstruct linked list. */
  CDFid idT;

  *status = (Int32) CDFlib (OPEN_, CDF_,
#if defined(Fif_DESCR)
					 DESCRtoREFnul(CDF_name,
						       CDF_PATHNAME_LEN,&ssh),
#endif
#if defined(Fif_GHOSTLEN)
					 NULterminate(CDF_name,
						      Fif_GHOSTUSE(len),&ssh),
#endif
#if defined(Fif_NOLEN)
					 FindEndNUL(CDF_name,CDF_PATHNAME_LEN,
						    &ssh),
#endif
					 &idT,
			    NULL_);
  FreeStrings (ssh);
  if (StatusBAD(*status)) return;

  *id = CDFidToInt32 (idT);
  return;
}

/******************************************************************************
*  CDF_open_cdf.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_open_cdf__,cdf_open_cdf_,cdf_open_cdf,CDF_OPEN_CDF)
(CDF_name, id, status Fif_GHOSTARG(len))
void	  *CDF_name;		/* In: CDF name. */
Int32	  *id;			/* Out: CDF identifier. */
Int32     *status;		/* Out: CDF status code. */
Fif_GHOSTDEF(len)		/* Invisible length of "CDF_name"
				   (generated by FORTRAN compiler). */
{
  struct STRINGstruct *ssh = NULL;	/* Head of STRINGstruct linked list. */
  CDFid idT;

  *status = (Int32) CDFlib (OPEN_, CDF_,
#if defined(Fif_DESCR)
					 DESCRtoREFnul(CDF_name,
						       CDF_PATHNAME_LEN,&ssh),
#endif
#if defined(Fif_GHOSTLEN)
					 NULterminate(CDF_name,
						      Fif_GHOSTUSE(len),&ssh),
#endif
#if defined(Fif_NOLEN)
					 FindEndNUL(CDF_name,CDF_PATHNAME_LEN,
						    &ssh),
#endif
					 &idT,
			    NULL_);
  FreeStrings (ssh);
  if (StatusBAD(*status)) return;

  *id = CDFidToInt32 (idT);
  return;
}

/******************************************************************************
*  CDF_doc.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_doc__,cdf_doc_,cdf_doc,CDF_DOC)
(id, version, release, text, status Fif_GHOSTARG(text_len))
Int32	  *id;			/* In: CDF identifier. */
Int32	  *version;		/* Out: CDF version number (creating library
					version number). */
Int32	  *release;		/* Out: CDF release number (creating library
					release number). */
void	  *text;		/* Out: Copyright text. */
Int32	  *status;		/* Out: CDF status code. */
Fif_GHOSTDEF(text_len)		/* Invisible length of "text"
				   (generated by FORTRAN compiler). */
{
  char copyRightTextT[CDF_COPYRIGHT_LEN+1];
  long versionT, releaseT;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
			    GET_, CDF_VERSION_, &versionT,
				  CDF_RELEASE_, &releaseT,
				  CDF_COPYRIGHT_, copyRightTextT,
			    NULL_);
  if (StatusBAD(*status)) return;

  *version = (Int32) versionT;
  *release = (Int32) releaseT;
#if defined(Fif_GHOSTLEN)
  CtoFORTstring (copyRightTextT, text, Fif_GHOSTUSE(text_len));
#else
  CtoFORTstring (copyRightTextT, text, CDF_COPYRIGHT_LEN);
#endif
  return;
}

/******************************************************************************
*  CDF_inquire.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_inquire__,cdf_inquire_,cdf_inquire,CDF_INQUIRE)
(id, num_dims, dim_sizes, encoding, majority, max_rec, num_vars, num_attrs,
status)
Int32	  *id;			/* In: CDF identifier. */
Int32	  *num_dims;		/* Out: Number of rDimensions. */
Int32	  dim_sizes[];		/* Out: rDimension sizes. */
Int32	  *encoding;		/* Out: Host or network. */
Int32	  *majority;		/* Out: Row or column major. */
Int32	  *max_rec;		/* Out: Maximum rRecord number. */
Int32	  *num_vars;		/* Out: Number of rVariables. */
Int32	  *num_attrs;		/* Out: Number of attributes. */
Int32	  *status;		/* Out: CDF status code. */
{
  long maxRecT, numDimsT, dimSizesT[CDF_MAX_DIMS], encodingT, majorityT,
       numVarsT, numAttrsT;
  int dimN;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
			    GET_, rVARs_NUMDIMS_, &numDimsT,
				  rVARs_DIMSIZES_, dimSizesT,
			          CDF_ENCODING_, &encodingT,
			      	  CDF_MAJORITY_, &majorityT,
			      	  rVARs_MAXREC_, &maxRecT,
			      	  CDF_NUMrVARS_, &numVarsT,
			      	  CDF_NUMATTRS_, &numAttrsT,
			    NULL_);
  if (StatusBAD(*status)) return;

  *num_dims = (Int32) numDimsT;
  for (dimN = 0; dimN < (int) numDimsT; dimN++) {
     dim_sizes[dimN] = (Int32) dimSizesT[dimN];
  }
  *encoding = (Int32) encodingT;
  *majority = (Int32) majorityT;
  *max_rec = (Int32) (maxRecT + 1);
  *num_vars = (Int32) numVarsT;
  *num_attrs = (Int32) numAttrsT;
  return;
}

/******************************************************************************
*  CDF_inquire_cdf.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_inquire_cdf__,cdf_inquire_cdf_,cdf_inquire_cdf,
               CDF_INQUIRE_CDF)
(id, num_dims, dim_sizes, encoding, majority, max_rrec, num_rvars, 
 max_zrec, num_zvars, num_attrs,
status)
Int32	  *id;			/* In: CDF identifier. */
Int32	  *num_dims;		/* Out: Number of rDimensions. */
Int32	  dim_sizes[];		/* Out: rDimension sizes. */
Int32	  *encoding;		/* Out: Host or network. */
Int32	  *majority;		/* Out: Row or column major. */
Int32	  *max_rrec;		/* Out: Maximum rRecord number. */
Int32	  *num_rvars;		/* Out: Number of rVariables. */
Int32     *max_zrec;		/* Out: Maximum zRecord number. */
Int32     *num_zvars;		/* Out: Number of zVariables. */
Int32	  *num_attrs;		/* Out: Number of attributes. */
Int32	  *status;		/* Out: CDF status code. */
{
  long maxrRecT, numDimsT, dimSizesT[CDF_MAX_DIMS], encodingT, majorityT,
       numrVarsT, numAttrsT, numzVarsT, maxzRecT;
  int dimN;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
			    GET_, rVARs_NUMDIMS_, &numDimsT,
				  rVARs_DIMSIZES_, dimSizesT,
			          CDF_ENCODING_, &encodingT,
			      	  CDF_MAJORITY_, &majorityT,
			      	  rVARs_MAXREC_, &maxrRecT,
			      	  CDF_NUMrVARS_, &numrVarsT,
				  zVARs_MAXREC_, &maxzRecT,
				  CDF_NUMzVARS_, &numzVarsT,
			      	  CDF_NUMATTRS_, &numAttrsT,
			    NULL_);
  if (StatusBAD(*status)) return;

  *num_dims = (Int32) numDimsT;
  for (dimN = 0; dimN < (int) numDimsT; dimN++) {
     dim_sizes[dimN] = (Int32) dimSizesT[dimN];
  }
  *encoding = (Int32) encodingT;
  *majority = (Int32) majorityT;
  *max_rrec = (Int32) (maxrRecT + 1);
  *num_rvars = (Int32) numrVarsT;
  *num_attrs = (Int32) numAttrsT;
  *max_zrec = (Int32) (maxzRecT + 1);
  *num_zvars = (Int32) numzVarsT;
  return;
}

/******************************************************************************
*  CDF_close.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_close__,cdf_close_,cdf_close,CDF_CLOSE)
(id, status)
Int32	  *id;			/* In: CDF identifier. */
Int32	  *status;		/* Out: CDF status code. */
{
  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
			    CLOSE_, CDF_,
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_close_cdf.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_close_cdf__,cdf_close_cdf_,cdf_close_cdf,CDF_CLOSE_CDF)
(id, status)
Int32	  *id;			/* In: CDF identifier. */
Int32	  *status;		/* Out: CDF status code. */
{
  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
			    CLOSE_, CDF_,
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_delete.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_delete__,cdf_delete_,cdf_delete,CDF_DELETE)
(id, status)
Int32	  *id;			/* In: CDF identifier. */
Int32	  *status;		/* Out: CDF status code. */
{
  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
			    DELETE_, CDF_,
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_delete_cdf.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_delete_cdf__,cdf_delete_cdf_,cdf_delete_cdf,CDF_DELETE_CDF)
(id, status)
Int32	  *id;			/* In: CDF identifier. */
Int32	  *status;		/* Out: CDF status code. */
{
  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
			    DELETE_, CDF_,
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_attr_create.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_attr_create__,
	       cdf_attr_create_,
	       cdf_attr_create,
	       CDF_ATTR_CREATE)
(id, attr_name, attr_scope, attr_num, status Fif_GHOSTARG(len))
Int32	  *id;		   	/* In: CDF identifier. */
void	  *attr_name;	   	/* In: Attribute name. */
Int32	  *attr_scope;		/* In: Attribute scope. */
Int32	  *attr_num;	   	/* Out: Attribute number. */
Int32	  *status;		/* Out: CDF status code. */
Fif_GHOSTDEF(len)		/* Invisible length of "attr_name"
				   (generated by FORTRAN compiler). */
{
  struct STRINGstruct *ssh = NULL;	/* Head of STRINGstruct linked list. */
  long attrNumT;
  long scopeT = (long) *attr_scope;
  int LFS = FALSE;
  struct CDFstruct *CDF;

  CDF  = (struct CDFstruct *)Int32ToCDFid(*id);
  if (isLFS(CDF)) LFS = TRUE;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
			    CREATE_, ATTR_,
#if defined(Fif_DESCR)
					    DESCRtoREFnul(attr_name,
							  (LFS ?
							CDF_ATTR_NAME_LEN256 :
							CDF_ATTR_NAME_LEN),
							  &ssh),
#endif
#if defined(Fif_GHOSTLEN)
					    NULterminate(attr_name,
							 Fif_GHOSTUSE(len),
							 &ssh),
#endif
#if defined(Fif_NOLEN)
					    FindEndNUL(attr_name,
                                                          (LFS ?
                                                        CDF_ATTR_NAME_LEN256 :
                                                        CDF_ATTR_NAME_LEN),
						       &ssh),
#endif
					    scopeT, &attrNumT,
			    NULL_);
  FreeStrings (ssh);
  if (StatusBAD(*status)) return;

  *attr_num = (Int32) (attrNumT + 1);
  return;
}

/******************************************************************************
*  CDF_create_attr.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_create_attr__,
	       cdf_create_attr_,
	       cdf_create_attr,
	       CDF_CREATE_ATTR)
(id, attr_name, attr_scope, attr_num, status Fif_GHOSTARG(len))
Int32	  *id;		   	/* In: CDF identifier. */
void	  *attr_name;	   	/* In: Attribute name. */
Int32	  *attr_scope;		/* In: Attribute scope. */
Int32	  *attr_num;	   	/* Out: Attribute number. */
Int32	  *status;		/* Out: CDF status code. */
Fif_GHOSTDEF(len)		/* Invisible length of "attr_name"
				   (generated by FORTRAN compiler). */
{
  struct STRINGstruct *ssh = NULL;	/* Head of STRINGstruct linked list. */
  long attrNumT;
  long scopeT = (long) *attr_scope;
  int LFS = FALSE;
  struct CDFstruct *CDF;

  CDF  = (struct CDFstruct *)Int32ToCDFid(*id);
  if (isLFS(CDF)) LFS = TRUE;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
			    CREATE_, ATTR_,
#if defined(Fif_DESCR)
					    DESCRtoREFnul(attr_name,
							  (LFS ?
							CDF_ATTR_NAME_LEN256 :
							CDF_ATTR_NAME_LEN),
							  &ssh),
#endif
#if defined(Fif_GHOSTLEN)
					    NULterminate(attr_name,
							 Fif_GHOSTUSE(len),
							 &ssh),
#endif
#if defined(Fif_NOLEN)
					    FindEndNUL(attr_name,
                                                          (LFS ?
                                                        CDF_ATTR_NAME_LEN256 :
                                                        CDF_ATTR_NAME_LEN),
						       &ssh),
#endif
					    scopeT, &attrNumT,
			    NULL_);
  FreeStrings (ssh);
  if (StatusBAD(*status)) return;

  *attr_num = (Int32) (attrNumT + 1);
  return;
}

/******************************************************************************
*  CDF_attr_num.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_attr_num__,cdf_attr_num_,cdf_attr_num,CDF_ATTR_NUM)
(id, attr_name Fif_GHOSTARG(len))
Int32     *id;		   	/* In: CDF identifier. */
void	  *attr_name;	   	/* In: Attribute name. */
Fif_GHOSTDEF(len)		/* Invisible length of "attr_name"
				   (generated by FORTRAN compiler). */
{
  struct STRINGstruct *ssh = NULL;	/* Head of STRINGstruct linked list. */
  CDFstatus status;
  long attrNumT;
  int LFS = FALSE;
  struct CDFstruct *CDF;

  CDF  = (struct CDFstruct *)Int32ToCDFid(*id);
  if (isLFS(CDF)) LFS = TRUE;

  status = CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
		   GET_, ATTR_NUMBER_,
#if defined(Fif_DESCR)
				       DESCRtoREFnul(attr_name,
                                                     (LFS ?
                                                      CDF_ATTR_NAME_LEN256 :
                                                      CDF_ATTR_NAME_LEN),
						     &ssh),
#endif
#if defined(Fif_GHOSTLEN)
				       NULterminate(attr_name,
						    Fif_GHOSTUSE(len),&ssh),
#endif
#if defined(Fif_NOLEN)
				       FindEndNUL(attr_name,
                                                  (LFS ?
                                                   CDF_ATTR_NAME_LEN256 :
                                                   CDF_ATTR_NAME_LEN),
						  &ssh),
#endif
				       &attrNumT,
		   NULL_);
  FreeStrings (ssh);

  if (StatusOK(status))
    return ((Int32) (attrNumT + 1));
  else
    return ((Int32) status);
}

/******************************************************************************
*  CDF_get_attr_num.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_attr_num__,cdf_get_attr_num_,cdf_get_attr_num,
               CDF_GET_ATTR_NUM)
(id, attr_name Fif_GHOSTARG(len))
Int32     *id;		   	/* In: CDF identifier. */
void	  *attr_name;	   	/* In: Attribute name. */
Fif_GHOSTDEF(len)		/* Invisible length of "attr_name"
				   (generated by FORTRAN compiler). */
{
  struct STRINGstruct *ssh = NULL;	/* Head of STRINGstruct linked list. */
  CDFstatus status;
  long attrNumT;
  int LFS = FALSE;
  struct CDFstruct *CDF;

  CDF  = (struct CDFstruct *)Int32ToCDFid(*id);
  if (isLFS(CDF)) LFS = TRUE;

  status = CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
		   GET_, ATTR_NUMBER_,
#if defined(Fif_DESCR)
				       DESCRtoREFnul(attr_name,
                                                     (LFS ?
                                                      CDF_ATTR_NAME_LEN256 :
                                                      CDF_ATTR_NAME_LEN),
						     &ssh),
#endif
#if defined(Fif_GHOSTLEN)
				       NULterminate(attr_name,
						    Fif_GHOSTUSE(len),&ssh),
#endif
#if defined(Fif_NOLEN)
				       FindEndNUL(attr_name,
                                                  (LFS ?
                                                   CDF_ATTR_NAME_LEN256 :
                                                   CDF_ATTR_NAME_LEN),
						  &ssh),
#endif
				       &attrNumT,
		   NULL_);
  FreeStrings (ssh);

  if (StatusOK(status))
    return ((Int32) (attrNumT + 1));
  else
    return ((Int32) status);
}

/******************************************************************************
*  CDF_attr_rename.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_attr_rename__,
	       cdf_attr_rename_,
	       cdf_attr_rename,
	       CDF_ATTR_RENAME)
(id, attr_num, attr_name, status Fif_GHOSTARG(len))
Int32     *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	  	/* In: Attribute number. */
void	  *attr_name;	   	/* In: New attribute name. */
Int32	  *status;		/* Out: CDF status code. */
Fif_GHOSTDEF(len)		/* Invisible length of "attr_name"
				   (generated by FORTRAN compiler). */
{
  struct STRINGstruct *ssh = NULL;	/* Head of STRINGstruct linked list. */
  long attrNumT = (long) (*attr_num - 1);
  int LFS = FALSE;
  struct CDFstruct *CDF;

  CDF  = (struct CDFstruct *)Int32ToCDFid(*id);
  if (isLFS(CDF)) LFS = TRUE;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     ATTR_, attrNumT,
			    PUT_, ATTR_NAME_,
#if defined(Fif_DESCR)
					      DESCRtoREFnul(attr_name,
                                                            (LFS ?
                                                        CDF_ATTR_NAME_LEN256 :
                                                        CDF_ATTR_NAME_LEN),
							    &ssh),
#endif
#if defined(Fif_GHOSTLEN)
					      NULterminate(attr_name,
							   Fif_GHOSTUSE(len),
							   &ssh),
#endif
#if defined(Fif_NOLEN)
					      FindEndNUL(attr_name,
                                                         (LFS ?
                                                        CDF_ATTR_NAME_LEN256 :
                                                        CDF_ATTR_NAME_LEN),
							 &ssh),
#endif
			    NULL_);
  FreeStrings (ssh);
  return;
}

/******************************************************************************
*  CDF_rename_attr.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_rename_attr__,
	       cdf_rename_attr_,
	       cdf_rename_attr,
	       CDF_RENAME_ATTR)
(id, attr_num, attr_name, status Fif_GHOSTARG(len))
Int32     *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	  	/* In: Attribute number. */
void	  *attr_name;	   	/* In: New attribute name. */
Int32	  *status;		/* Out: CDF status code. */
Fif_GHOSTDEF(len)		/* Invisible length of "attr_name"
				   (generated by FORTRAN compiler). */
{
  struct STRINGstruct *ssh = NULL;	/* Head of STRINGstruct linked list. */
  long attrNumT = (long) (*attr_num - 1);
  int LFS = FALSE;
  struct CDFstruct *CDF;

  CDF  = (struct CDFstruct *)Int32ToCDFid(*id);
  if (isLFS(CDF)) LFS = TRUE;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     ATTR_, attrNumT,
			    PUT_, ATTR_NAME_,
#if defined(Fif_DESCR)
					      DESCRtoREFnul(attr_name,
                                                            (LFS ?
                                                        CDF_ATTR_NAME_LEN256 :
                                                        CDF_ATTR_NAME_LEN),
							    &ssh),
#endif
#if defined(Fif_GHOSTLEN)
					      NULterminate(attr_name,
							   Fif_GHOSTUSE(len),
							   &ssh),
#endif
#if defined(Fif_NOLEN)
					      FindEndNUL(attr_name,
                                                         (LFS ?
                                                        CDF_ATTR_NAME_LEN256 :
                                                        CDF_ATTR_NAME_LEN),
							 &ssh),
#endif
			    NULL_);
  FreeStrings (ssh);
  return;
}

/******************************************************************************
*  CDF_attr_inquire.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_attr_inquire__,
	       cdf_attr_inquire_,
	       cdf_attr_inquire,
	       CDF_ATTR_INQUIRE)
(id, attr_num, attr_name, attr_scope, max_entry, status Fif_GHOSTARG(len))
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	  	/* In: Attribute number. */
void	  *attr_name;      	/* Out: Attribute name. */
Int32	  *attr_scope;	   	/* Out: Attribute scope. */
Int32	  *max_entry;	   	/* Out: Maximum gEntry/rEntry number used. */
Int32	  *status;		/* Out: CDF status code. */
Fif_GHOSTDEF(len)		/* Invisible length of "attr_name"
				   (generated by FORTRAN compiler). */
{
  char attrNameT[CDF_ATTR_NAME_LEN256];
  long maxEntryT, attrScopeT;
  long attrNumT = (long) (*attr_num - 1);

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     ATTR_, attrNumT,
			    GET_, ATTR_NAME_, attrNameT,
				  ATTR_SCOPE_, &attrScopeT,
			    NULL_);
  if (StatusBAD(*status)) return;

  *status = (Int32) CDFlib (GET_, BOO(GLOBALscope(attrScopeT),
				      ATTR_MAXgENTRY_,
				      ATTR_MAXrENTRY_), &maxEntryT,
			    NULL_);
  if (StatusBAD(*status)) return;

#if defined(Fif_GHOSTLEN)
  CtoFORTstring (attrNameT, attr_name, Fif_GHOSTUSE(len));
#else
  CtoFORTstring (attrNameT, attr_name, CDF_ATTR_NAME_LEN256);
#endif
  *attr_scope = (Int32) attrScopeT;
  *max_entry = (Int32) (maxEntryT + 1);
  return;
}

/******************************************************************************
*  CDF_inquire_attr.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_inquire_attr__,
	       cdf_inquire_attr_,
	       cdf_inquire_attr,
	       CDF_INQUIRE_ATTR)
(id, attr_num, attr_name, attr_scope, max_gentry, max_rentry, max_zentry, 
 status Fif_GHOSTARG(len))
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	  	/* In: Attribute number. */
void	  *attr_name;      	/* Out: Attribute name. */
Int32	  *attr_scope;	   	/* Out: Attribute scope. */
Int32     *max_gentry;          /* Out: Maximum gEntry number if global attribute. */
Int32     *max_rentry;          /* Out: Maximum rEntry number if variable attribute. */
Int32	  *max_zentry;	   	/* Out: Maximum zEntry number if variable attribute. */
Int32	  *status;		/* Out: CDF status code. */
Fif_GHOSTDEF(len)		/* Invisible length of "attr_name"
				   (generated by FORTRAN compiler). */
{
  char attrNameT[CDF_ATTR_NAME_LEN256];
  long maxgEntryT = -1, maxrEntryT = -1, maxzEntryT = -1, attrScopeT;
  long attrNumT = (long) (*attr_num - 1);

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     ATTR_, attrNumT,
			    GET_, ATTR_NAME_, attrNameT,
				  ATTR_SCOPE_, &attrScopeT,
			    NULL_);
  if (StatusBAD(*status)) return;

  if (GLOBALscope(attrScopeT)) {
    *status = (Int32) CDFlib (GET_, ATTR_MAXgENTRY_, &maxgEntryT,
                              NULL_);
    if (StatusBAD(*status)) return;
  } else {
    *status = (Int32) CDFlib (GET_, ATTR_MAXrENTRY_, &maxrEntryT,
                                    ATTR_MAXzENTRY_, &maxzEntryT,
                              NULL_);
    if (StatusBAD(*status)) return;
  }

#if defined(Fif_GHOSTLEN)
  CtoFORTstring (attrNameT, attr_name, Fif_GHOSTUSE(len));
#else
  CtoFORTstring (attrNameT, attr_name, CDF_ATTR_NAME_LEN256);
#endif
  *attr_scope = (Int32) attrScopeT;
  *max_gentry = (Int32) (maxgEntryT + 1);
  *max_rentry = (Int32) (maxrEntryT + 1);
  *max_zentry = (Int32) (maxzEntryT + 1);
  return;
}

/******************************************************************************
*  CDF_attr_entry_inquire.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_attr_entry_inquire__,
	       cdf_attr_entry_inquire_,
	       cdf_attr_entry_inquire,
	       CDF_ATTR_ENTRY_INQUIRE)
(id, attr_num, entry_num, data_type, num_elements, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	  	/* In: Attribute number. */
Int32	  *entry_num;	 	/* In: gEntry/rEntry number. */
Int32	  *data_type;	   	/* Out: Data type. */
Int32	  *num_elements;   	/* Out: Number of elements. */
Int32	  *status;		/* Out: CDF status code. */
{
  long dataTypeT, numElementsT, scope;
  long attrNumT = (long) (*attr_num - 1);
  long entryNumT = (long) (*entry_num - 1);

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     ATTR_, attrNumT,
			    GET_, ATTR_SCOPE_, &scope,
			    NULL_);
  if (StatusBAD(*status)) return;

  *status = (Int32) CDFlib (SELECT_, BOO(GLOBALscope(scope),
					 gENTRY_,rENTRY_), entryNumT,
			    GET_, BOO(GLOBALscope(scope),
				      gENTRY_DATATYPE_,
				      rENTRY_DATATYPE_), &dataTypeT,
				  BOO(GLOBALscope(scope),
				      gENTRY_NUMELEMS_,
				      rENTRY_NUMELEMS_), &numElementsT,
			    NULL_);
  if (StatusBAD(*status)) return;

  *data_type = (Int32) dataTypeT;
  *num_elements = (Int32) numElementsT;
  return;
}

/******************************************************************************
*  CDF_inquire_attr_gentry.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_inquire_attr_gentry__,
	       cdf_inquire_attr_gentry_,
	       cdf_inquire_attr_gentry,
	       CDF_INQUIRE_ATTR_GENTRY)
(id, attr_num, entry_num, data_type, num_elements, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	  	/* In: Attribute number. */
Int32	  *entry_num;	 	/* In: gEntry number. */
Int32	  *data_type;	   	/* Out: Data type. */
Int32	  *num_elements;   	/* Out: Number of elements. */
Int32	  *status;		/* Out: CDF status code. */
{
  long dataTypeT, numElementsT, scope;
  long attrNumT = (long) (*attr_num - 1);
  long entryNumT = (long) (*entry_num - 1);

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     ATTR_, attrNumT,
			    GET_, ATTR_SCOPE_, &scope,
			    NULL_);
  if (StatusBAD(*status)) return;
  if (!GLOBALscope(scope)) {
    *status = (Int32) ILLEGAL_FOR_SCOPE;
    return;
  }

  *status = (Int32) CDFlib (SELECT_, gENTRY_, entryNumT,
			    GET_, gENTRY_DATATYPE_, &dataTypeT,
				  gENTRY_NUMELEMS_, &numElementsT,
			    NULL_);
  if (StatusBAD(*status)) return;

  *data_type = (Int32) dataTypeT;
  *num_elements = (Int32) numElementsT;
  return;
}

/******************************************************************************
*  CDF_inquire_attr_rentry.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_inquire_attr_rentry__,
	       cdf_inquire_attr_rentry_,
	       cdf_inquire_attr_rentry,
	       CDF_INQUIRE_ATTR_RENTRY)
(id, attr_num, entry_num, data_type, num_elements, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	  	/* In: Attribute number. */
Int32	  *entry_num;	 	/* In: rEntry number. */
Int32	  *data_type;	   	/* Out: Data type. */
Int32	  *num_elements;   	/* Out: Number of elements. */
Int32	  *status;		/* Out: CDF status code. */
{
  long dataTypeT, numElementsT, scope;
  long attrNumT = (long) (*attr_num - 1);
  long entryNumT = (long) (*entry_num - 1);

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     ATTR_, attrNumT,
			    GET_, ATTR_SCOPE_, &scope,
			    NULL_);
  if (StatusBAD(*status)) return;
  if (GLOBALscope(scope)) {
    *status = (Int32) ILLEGAL_FOR_SCOPE;
    return;
  }

  *status = (Int32) CDFlib (SELECT_, rENTRY_, entryNumT,
			    GET_, rENTRY_DATATYPE_, &dataTypeT,
				  rENTRY_NUMELEMS_, &numElementsT,
			    NULL_);
  if (StatusBAD(*status)) return;

  *data_type = (Int32) dataTypeT;
  *num_elements = (Int32) numElementsT;
  return;
}

/******************************************************************************
*  CDF_inquire_attr_zentry.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_inquire_attr_zentry__,
	       cdf_inquire_attr_zentry_,
	       cdf_inquire_attr_zentry,
	       CDF_INQUIRE_ATTR_ZENTRY)
(id, attr_num, entry_num, data_type, num_elements, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	  	/* In: Attribute number. */
Int32	  *entry_num;	 	/* In: zEntry number. */
Int32	  *data_type;	   	/* Out: Data type. */
Int32	  *num_elements;   	/* Out: Number of elements. */
Int32	  *status;		/* Out: CDF status code. */
{
  long dataTypeT, numElementsT, scope;
  long attrNumT = (long) (*attr_num - 1);
  long entryNumT = (long) (*entry_num - 1);

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     ATTR_, attrNumT,
			    GET_, ATTR_SCOPE_, &scope,
			    NULL_);
  if (StatusBAD(*status)) return;
  if (GLOBALscope(scope)) {
    *status = (Int32) ILLEGAL_FOR_SCOPE;
    return;
  }

  *status = (Int32) CDFlib (SELECT_, zENTRY_, entryNumT,
			    GET_, zENTRY_DATATYPE_, &dataTypeT,
				  zENTRY_NUMELEMS_, &numElementsT,
			    NULL_);
  if (StatusBAD(*status)) return;

  *data_type = (Int32) dataTypeT;
  *num_elements = (Int32) numElementsT;
  return;
}

/******************************************************************************
*  CDF_attr_put.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_attr_put__,cdf_attr_put_,cdf_attr_put,CDF_ATTR_PUT)
(id, attr_num, entry_num, data_type, num_elems, value, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	  	/* In: Attribute number. */
Int32	  *entry_num; 		/* In: gEntry/rEntry number. */
Int32	  *data_type; 		/* In: Data type. */
Int32	  *num_elems;		/* In: Number of elements. */
void	  *value;	   	/* In: Value.
					VMS: If character data type, could
					     be passed by reference or
					     descriptor. */
Int32	   *status;		/* Out: CDF status code. */
{
  long scope;
  long attrNumT = (long) (*attr_num - 1);
  long entryNumT = (long) (*entry_num - 1);
  long dataTypeT = (long) *data_type;
  long numElemsT = (long) *num_elems;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     ATTR_, attrNumT,
			    GET_, ATTR_SCOPE_, &scope,
			    NULL_);
  if (StatusBAD(*status)) return;

  *status = (Int32) CDFlib (SELECT_, BOO(GLOBALscope(scope),
					 gENTRY_,rENTRY_), entryNumT,
			    PUT_, BOO(GLOBALscope(scope),
				      gENTRY_DATA_,rENTRY_DATA_),
						dataTypeT, numElemsT,
#if defined(Fif_DESCR)
						BOO(STRINGdataType(dataTypeT),
						    DESCRtoREF(value),value),
#else
						value,
#endif
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_put_attr_gentry.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_put_attr_gentry__,
               cdf_put_attr_gentry_,
               cdf_put_attr_gentry,
               CDF_PUT_ATTR_GENTRY)
(id, attr_num, entry_num, data_type, num_elems, value, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	  	/* In: Attribute number. */
Int32	  *entry_num; 		/* In: gEntry number. */
Int32	  *data_type; 		/* In: Data type. */
Int32	  *num_elems;		/* In: Number of elements. */
void	  *value;	   	/* In: Value.
					VMS: If character data type, could
					     be passed by reference or
					     descriptor. */
Int32	   *status;		/* Out: CDF status code. */
{
  long scope;
  long attrNumT = (long) (*attr_num - 1);
  long entryNumT = (long) (*entry_num - 1);
  long dataTypeT = (long) *data_type;
  long numElemsT = (long) *num_elems;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     ATTR_, attrNumT,
			    GET_, ATTR_SCOPE_, &scope,
			    NULL_);
  if (StatusBAD(*status)) return;

  if (!GLOBALscope(scope)) {
    *status = (Int32) ILLEGAL_FOR_SCOPE;
    return;
  }

  *status = (Int32) CDFlib (SELECT_, gENTRY_, entryNumT,
			    PUT_, gENTRY_DATA_, dataTypeT, numElemsT,
#if defined(Fif_DESCR)
						BOO(STRINGdataType(dataTypeT),
						    DESCRtoREF(value),value),
#else
						value,
#endif
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_put_attr_rentry.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_put_attr_rentry__,
               cdf_put_attr_rentry_,
               cdf_put_attr_rentry,
               CDF_PUT_ATTR_RENTRY)
(id, attr_num, entry_num, data_type, num_elems, value, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	  	/* In: Attribute number. */
Int32	  *entry_num; 		/* In: rEntry number. */
Int32	  *data_type; 		/* In: Data type. */
Int32	  *num_elems;		/* In: Number of elements. */
void	  *value;	   	/* In: Value.
					VMS: If character data type, could
					     be passed by reference or
					     descriptor. */
Int32	   *status;		/* Out: CDF status code. */
{
  long scope;
  long attrNumT = (long) (*attr_num - 1);
  long entryNumT = (long) (*entry_num - 1);
  long dataTypeT = (long) *data_type;
  long numElemsT = (long) *num_elems;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     ATTR_, attrNumT,
			    GET_, ATTR_SCOPE_, &scope,
			    NULL_);
  if (StatusBAD(*status)) return;

  if (GLOBALscope(scope)) {
    *status = (Int32) ILLEGAL_FOR_SCOPE;
    return;
  }

  *status = (Int32) CDFlib (SELECT_, rENTRY_, entryNumT,
			    PUT_, rENTRY_DATA_, dataTypeT, numElemsT,
#if defined(Fif_DESCR)
						BOO(STRINGdataType(dataTypeT),
						    DESCRtoREF(value),value),
#else
						value,
#endif
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_put_attr_zentry.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_put_attr_zentry__,
               cdf_put_attr_zentry_,
               cdf_put_attr_zentry,
               CDF_PUT_ATTR_ZENTRY)
(id, attr_num, entry_num, data_type, num_elems, value, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	  	/* In: Attribute number. */
Int32	  *entry_num; 		/* In: zEntry number. */
Int32	  *data_type; 		/* In: Data type. */
Int32	  *num_elems;		/* In: Number of elements. */
void	  *value;	   	/* In: Value.
					VMS: If character data type, could
					     be passed by reference or
					     descriptor. */
Int32	   *status;		/* Out: CDF status code. */
{
  long scope;
  long attrNumT = (long) (*attr_num - 1);
  long entryNumT = (long) (*entry_num - 1);
  long dataTypeT = (long) *data_type;
  long numElemsT = (long) *num_elems;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     ATTR_, attrNumT,
			    GET_, ATTR_SCOPE_, &scope,
			    NULL_);
  if (StatusBAD(*status)) return;

  if (GLOBALscope(scope)) {
    *status = (Int32) ILLEGAL_FOR_SCOPE;
    return;
  }

  *status = (Int32) CDFlib (SELECT_, zENTRY_, entryNumT,
			    PUT_, zENTRY_DATA_, dataTypeT, numElemsT,
#if defined(Fif_DESCR)
						BOO(STRINGdataType(dataTypeT),
						    DESCRtoREF(value),value),
#else
						value,
#endif
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_attr_get.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_attr_get__, cdf_attr_get_, cdf_attr_get, CDF_ATTR_GET)
(id, attr_num, entry_num, value, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	  	/* In: Attribute number. */
Int32	  *entry_num; 		/* In: gEntry/rEntry number. */
void	  *value;	   	/* Out: Value.
					 VMS: Could be passed out by reference
					      or descriptor if character data
					      type. */
Int32	  *status;		/* Out: CDF status code. */
{
  long dataType, scope;
  long attrNumT = (long) (*attr_num - 1);
  long entryNumT = (long) (*entry_num - 1);

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     ATTR_, attrNumT,
			    GET_, ATTR_SCOPE_, &scope,
			    NULL_);
  if (StatusBAD(*status)) return;

  *status = (Int32) CDFlib (SELECT_, BOO(GLOBALscope(scope),
					 gENTRY_,rENTRY_), entryNumT,
			    GET_, BOO(GLOBALscope(scope),
				      gENTRY_DATATYPE_,
				      rENTRY_DATATYPE_), &dataType,
			    NULL_);
  if (StatusBAD(*status)) return;

  *status = (Int32) CDFlib (GET_, BOO(GLOBALscope(scope),
				      gENTRY_DATA_,rENTRY_DATA_),
#if defined(Fif_DESCR)
					        BOO(STRINGdataType(dataType),
						    DESCRtoREF(value),value),
#else
					        value,
#endif
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_get_attr_gentry.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_attr_gentry__,
               cdf_get_attr_gentry_,
               cdf_get_attr_gentry,
               CDF_get_ATTR_GENTRY)
(id, attr_num, entry_num, value, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	  	/* In: Attribute number. */
Int32	  *entry_num; 		/* In: gEntry number. */
void	  *value;	   	/* Out: Value.
					 VMS: Could be passed out by reference
					      or descriptor if character data
					      type. */
Int32	  *status;		/* Out: CDF status code. */
{
  long	dataType, scope;
  long attrNumT = (long) (*attr_num - 1);
  long entryNumT = (long) (*entry_num - 1);

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     ATTR_, attrNumT,
			    GET_, ATTR_SCOPE_, &scope,
			    NULL_);
  if (StatusBAD(*status)) return;

  if (!GLOBALscope(scope)) {
    *status = (Int32) ILLEGAL_FOR_SCOPE;
    return;
  }

  *status = (Int32) CDFlib (SELECT_, gENTRY_, entryNumT,
			    GET_, gENTRY_DATATYPE_, &dataType,
			    NULL_);
  if (StatusBAD(*status)) return;

  *status = (Int32) CDFlib (GET_, gENTRY_DATA_,
#if defined(Fif_DESCR)
			  	  BOO(STRINGdataType(dataType),
			    	      DESCRtoREF(value),value),
#else
				      value,
#endif
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_get_attr_rentry.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_attr_rentry__,
               cdf_get_attr_rentry_,
               cdf_get_attr_rentry,
               CDF_get_ATTR_RENTRY)
(id, attr_num, entry_num, value, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	  	/* In: Attribute number. */
Int32	  *entry_num; 		/* In: rEntry number. */
void	  *value;	   	/* Out: Value.
					 VMS: Could be passed out by reference
					      or descriptor if character data
					      type. */
Int32	  *status;		/* Out: CDF status code. */
{
  long	dataType, scope;
  long attrNumT = (long) (*attr_num - 1);
  long entryNumT = (long) (*entry_num - 1);

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     ATTR_, attrNumT,
			    GET_, ATTR_SCOPE_, &scope,
			    NULL_);
  if (StatusBAD(*status)) return;

  if (GLOBALscope(scope)) {
    *status = (Int32) ILLEGAL_FOR_SCOPE;
    return;
  }

  *status = (Int32) CDFlib (SELECT_, rENTRY_, entryNumT,
			    GET_, rENTRY_DATATYPE_, &dataType,
			    NULL_);
  if (StatusBAD(*status)) return;

  *status = (Int32) CDFlib (GET_, rENTRY_DATA_,
#if defined(Fif_DESCR)
			  	  BOO(STRINGdataType(dataType),
			    	      DESCRtoREF(value),value),
#else
				      value,
#endif
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_get_attr_zentry.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_attr_zentry__,
               cdf_get_attr_zentry_,
               cdf_get_attr_zentry,
               CDF_get_ATTR_ZENTRY)
(id, attr_num, entry_num, value, status)
Int32	  *id;		   	/* In: CDF identifier. */
Int32	  *attr_num;	  	/* In: Attribute number. */
Int32	  *entry_num; 		/* In: zEntry number. */
void	  *value;	   	/* Out: Value.
					 VMS: Could be passed out by reference
					      or descriptor if character data
					      type. */
Int32	  *status;		/* Out: CDF status code. */
{
  long	dataType, scope;
  long attrNumT = (long) (*attr_num - 1);
  long entryNumT = (long) (*entry_num - 1);

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     ATTR_, attrNumT,
			    GET_, ATTR_SCOPE_, &scope,
			    NULL_);
  if (StatusBAD(*status)) return;

  if (GLOBALscope(scope)) {
    *status = (Int32) ILLEGAL_FOR_SCOPE;
    return;
  }

  *status = (Int32) CDFlib (SELECT_, zENTRY_, entryNumT,
			    GET_, zENTRY_DATATYPE_, &dataType,
			    NULL_);
  if (StatusBAD(*status)) return;

  *status = (Int32) CDFlib (GET_, zENTRY_DATA_,
#if defined(Fif_DESCR)
			  	  BOO(STRINGdataType(dataType),
			    	      DESCRtoREF(value),value),
#else
				      value,
#endif
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_var_create.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_var_create__,cdf_var_create_,cdf_var_create,CDF_VAR_CREATE)
(id, var_name, data_type, num_elems, rec_variance, dim_variances,
var_num, status Fif_GHOSTARG(len))
Int32	  *id;	   		/* In: CDF identifier. */
void      *var_name;	   	/* In: rVariable name. */
Int32	  *data_type; 		/* In: Data type. */
Int32	  *num_elems;		/* In: Number of elements of data_type. */
Int32	  *rec_variance; 	/* In: Record variance. */
Int32	  dim_variances[];  	/* In: Dimension variances. */
Int32	  *var_num;	    	/* Out: rVariable number. */
Int32	  *status;		/* Out: CDF status code. */
Fif_GHOSTDEF(len)		/* Invisible length of "var_name"
				   (generated by FORTRAN compiler). */
{
  struct STRINGstruct *ssh = NULL;	/* Head of STRINGstruct linked list. */
  long varNumT, dimVarysT[CDF_MAX_DIMS], numDims;
  int dimN;
  long dataTypeT = (long) *data_type;
  long numElemsT = (long) *num_elems;
  long recVaryT = (long) *rec_variance;
  int LFS = FALSE;
  struct CDFstruct *CDF;

  CDF  = (struct CDFstruct *)Int32ToCDFid(*id);
  if (isLFS(CDF)) LFS = TRUE;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
			    GET_, rVARs_NUMDIMS_, &numDims,
			    NULL_);
  if (StatusBAD(*status)) return;

  for (dimN = 0; dimN < (int) numDims; dimN++) {
     dimVarysT[dimN] = (long) dim_variances[dimN];
  }

  *status = (Int32) CDFlib (CREATE_, rVAR_,
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
					    dataTypeT, numElemsT, recVaryT,
					    dimVarysT, &varNumT,
			    NULL_);
  FreeStrings (ssh);
  if (StatusBAD(*status)) return;

  *var_num = (Int32) (varNumT + 1);
  return;
}

/******************************************************************************
*  CDF_create_zvar.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_create_zvar__,
               cdf_create_zvar_,
               cdf_create_zvar,
               CDF_CREATE_ZVAR)
(id, var_name, data_type, num_elems, num_dims, dim_sizes, rec_variance,
dim_variances, var_num, status Fif_GHOSTARG(len))
Int32	  *id;	   		/* In: CDF identifier. */
void      *var_name;	   	/* In: zVariable name. */
Int32	  *data_type; 		/* In: Data type. */
Int32	  *num_elems;		/* In: Number of elements of data_type. */
Int32     *num_dims;            /* In: Number of dimension. */
Int32     dim_sizes[];          /* In: Dimension sizes. */
Int32	  *rec_variance; 	/* In: Record variance. */
Int32	  dim_variances[];  	/* In: Dimension variances. */
Int32	  *var_num;	    	/* Out: zVariable number. */
Int32	  *status;		/* Out: CDF status code. */
Fif_GHOSTDEF(len)		/* Invisible length of "var_name"
				   (generated by FORTRAN compiler). */
{
  struct STRINGstruct *ssh = NULL;	/* Head of STRINGstruct linked list. */
  long varNumT, dimVarysT[CDF_MAX_DIMS], dimSizesT[CDF_MAX_DIMS], numDims;
  int dimN;
  long dataTypeT = (long) *data_type;
  long numElemsT = (long) *num_elems;
  long recVaryT = (long) *rec_variance;
  int LFS = FALSE;
  struct CDFstruct *CDF;

  CDF  = (struct CDFstruct *)Int32ToCDFid(*id);
  if (isLFS(CDF)) LFS = TRUE;

  numDims = (long) *num_dims;

  for (dimN = 0; dimN < (int) numDims; dimN++) {
     dimSizesT[dimN] = (long) dim_sizes[dimN];
     dimVarysT[dimN] = (long) dim_variances[dimN];
  }

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                            CREATE_, zVAR_,
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
					    dataTypeT, numElemsT, numDims,
                                            dimSizesT, recVaryT,
					    dimVarysT, &varNumT,
			    NULL_);
  FreeStrings (ssh);
  if (StatusBAD(*status)) return;

  *var_num = (Int32) (varNumT + 1);
  return;
}

/******************************************************************************
*  CDF_var_num.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_var_num__,cdf_var_num_,cdf_var_num,CDF_VAR_NUM)
(id, var_name Fif_GHOSTARG(len))
Int32	  *id;		   	/* In: CDF identifier. */
void	  *var_name;	   	/* In: Variable name. */
Fif_GHOSTDEF(len)		/* Invisible length of "var_name"
				   (generated by FORTRAN compiler). */
{
  struct STRINGstruct *ssh = NULL;	/* Head of STRINGstruct linked list. */
  CDFstatus status;
  long varNumT;
  int LFS = FALSE;
  struct CDFstruct *CDF;

  CDF  = (struct CDFstruct *)Int32ToCDFid(*id);
  if (isLFS(CDF)) LFS = TRUE;

  status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
		           GET_, rVAR_NUMBER_,
#if defined(Fif_DESCR)
				       DESCRtoREFnul(var_name,
                                                     (LFS ?
                                                      CDF_VAR_NAME_LEN256 :
                                                      CDF_VAR_NAME_LEN),
						     &ssh),
#endif
#if defined(Fif_GHOSTLEN)
				       NULterminate(var_name,Fif_GHOSTUSE(len),
						    &ssh),
#endif
#if defined(Fif_NOLEN)
				       FindEndNUL(var_name,
                                                  (LFS ?
                                                   CDF_VAR_NAME_LEN256 :
                                                   CDF_VAR_NAME_LEN),
						  &ssh),
#endif
				       &varNumT,
		   NULL_);
  if (!StatusOK(status)) {
    status = (Int32) CDFlib (GET_, zVAR_NUMBER_,
#if defined(Fif_DESCR)
                                         DESCRtoREFnul(var_name,
                                                       (LFS ?
                                                        CDF_VAR_NAME_LEN256 :
                                                        CDF_VAR_NAME_LEN),
                                                       &ssh),
#endif
#if defined(Fif_GHOSTLEN)
                                         NULterminate(var_name,Fif_GHOSTUSE(len),
                                                      &ssh),
#endif
#if defined(Fif_NOLEN)
                                         FindEndNUL(var_name,
                                                    (LFS ?
                                                     CDF_VAR_NAME_LEN256 :
                                                     CDF_VAR_NAME_LEN),
                                                    &ssh),
#endif
                                         &varNumT,
                     NULL_);
  }

  FreeStrings (ssh);

  if (StatusOK(status))
    return ((Int32) (varNumT + 1));
  else
    return ((Int32) status);
}

/******************************************************************************
*  CDF_get_var_num.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_var_num__,
               cdf_get_var_num_,
               cdf_get_var_num,
               CDF_GET_VAR_NUM)
(id, var_name Fif_GHOSTARG(len))
Int32	  *id;		   	/* In: CDF identifier. */
void	  *var_name;	   	/* In: Variable name. */
Fif_GHOSTDEF(len)		/* Invisible length of "var_name"
				   (generated by FORTRAN compiler). */
{
  struct STRINGstruct *ssh = NULL;	/* Head of STRINGstruct linked list. */
  CDFstatus status;
  long varNumT;
  int LFS = FALSE;
  struct CDFstruct *CDF;

  CDF  = (struct CDFstruct *)Int32ToCDFid(*id);
  if (isLFS(CDF)) LFS = TRUE;

  status = CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
	           GET_, rVAR_NUMBER_,
#if defined(Fif_DESCR)
			       DESCRtoREFnul(var_name,
                                             (LFS ?
                                              CDF_VAR_NAME_LEN256 :
                                              CDF_VAR_NAME_LEN),
					     &ssh),
#endif
#if defined(Fif_GHOSTLEN)
			       NULterminate(var_name,Fif_GHOSTUSE(len),
					    &ssh),
#endif
#if defined(Fif_NOLEN)
			       FindEndNUL(var_name,
                                          (LFS ?
                                           CDF_VAR_NAME_LEN256 :
                                           CDF_VAR_NAME_LEN),
					  &ssh),
#endif
			       &varNumT,
		   NULL_);
  if (!StatusOK(status)) {
    status = CDFlib (GET_, zVAR_NUMBER_,
#if defined(Fif_DESCR)
                                 DESCRtoREFnul(var_name,
                                               (LFS ?
                                                CDF_VAR_NAME_LEN256 :
                                                CDF_VAR_NAME_LEN),
                                               &ssh),
#endif
#if defined(Fif_GHOSTLEN)
                                 NULterminate(var_name,Fif_GHOSTUSE(len),
                                              &ssh),
#endif
#if defined(Fif_NOLEN)
                                 FindEndNUL(var_name,
                                            (LFS ?
                                             CDF_VAR_NAME_LEN256 :
                                             CDF_VAR_NAME_LEN),
                                            &ssh),
#endif
                                 &varNumT,
                     NULL_);
  }

  FreeStrings (ssh);

  if (StatusOK(status))
    return (Int32) (varNumT + 1);
  else
    return ((Int32) status);
}

/******************************************************************************
*  CDF_var_rename.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_var_rename__,cdf_var_rename_,cdf_var_rename,CDF_VAR_RENAME)
(id, var_num, var_name, status Fif_GHOSTARG(len))
Int32	  *id;			/* In: CDF identifier. */
Int32	  *var_num;	   	/* In: rVariable number. */
void	  *var_name;	   	/* In: New variable name. */
Int32	  *status;		/* Out: CDF status code. */
Fif_GHOSTDEF(len)		/* Invisible length of "var_name"
				   (generated by FORTRAN compiler). */
{
  struct STRINGstruct *ssh = NULL;	/* Head of STRINGstruct linked list. */
  long varNumT = (long) (*var_num - 1);
  int LFS = FALSE;
  struct CDFstruct *CDF;

  CDF  = (struct CDFstruct *)Int32ToCDFid(*id);
  if (isLFS(CDF)) LFS = TRUE;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     rVAR_, varNumT,
			    PUT_, rVAR_NAME_,
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
			    NULL_);
  FreeStrings (ssh);
  return;
}

/******************************************************************************
*  CDF_rename_zvar.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_rename_zvar__,
               cdf_rename_zvar_,
               cdf_rename_zvar,
               CDF_RENAME_ZVAR)
(id, var_num, var_name, status Fif_GHOSTARG(len))
Int32	  *id;			/* In: CDF identifier. */
Int32	  *var_num;	   	/* In: zVariable number. */
void	  *var_name;	   	/* In: New variable name. */
Int32	  *status;		/* Out: CDF status code. */
Fif_GHOSTDEF(len)		/* Invisible length of "var_name"
				   (generated by FORTRAN compiler). */
{
  struct STRINGstruct *ssh = NULL;	/* Head of STRINGstruct linked list. */
  long varNumT = (long) (*var_num - 1);
  int LFS = FALSE;
  struct CDFstruct *CDF;

  CDF  = (struct CDFstruct *)Int32ToCDFid(*id);
  if (isLFS(CDF)) LFS = TRUE;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     zVAR_, varNumT,
			    PUT_, zVAR_NAME_,
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
			    NULL_);
  FreeStrings (ssh);
  return;
}

/******************************************************************************
*  CDF_var_inquire.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_var_inquire__,
	       cdf_var_inquire_,
	       cdf_var_inquire,
	       CDF_VAR_INQUIRE)
(id, var_num, var_name, data_type, num_elements, rec_variance,
dim_variances, status Fif_GHOSTARG(len))
Int32	  *id;		    	/* In: CDF identifier. */
Int32	  *var_num;	   	/* In: rVariable number. */
void	  *var_name;	    	/* Out: rVariable name. */
Int32	  *data_type;	    	/* Out: Data type. */
Int32	  *num_elements;	/* Out: Number of elements. */
Int32	  *rec_variance; 	/* Out: Record variance. */
Int32	  dim_variances[]; 	/* Out: Dimension variances. */
Int32	  *status;		/* Out: CDF status code. */
Fif_GHOSTDEF(len)		/* Invisible length of "var_name"
				   (generated by FORTRAN compiler). */
{
  char varNameT[CDF_VAR_NAME_LEN256];
  long dataTypeT, numElemsT, recVaryT, dimVarysT[CDF_MAX_DIMS], numDims;
  int dimN;
  long varNumT = (long) (*var_num - 1);

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
			    GET_, rVARs_NUMDIMS_, &numDims,
			    NULL_);
  if (StatusBAD(*status)) {
    return;
  }

  *status = (Int32) CDFlib (SELECT_, rVAR_, varNumT,
			    GET_, rVAR_NAME_, varNameT,
				  rVAR_DATATYPE_, &dataTypeT,
				  rVAR_NUMELEMS_, &numElemsT,
				  rVAR_RECVARY_, &recVaryT,
				  rVAR_DIMVARYS_, dimVarysT,
			    NULL_);
  if (StatusBAD(*status)) {
    return;
  }

#if defined(Fif_GHOSTLEN)
  CtoFORTstring (varNameT, var_name, Fif_GHOSTUSE(len));
#else
  CtoFORTstring (varNameT, var_name, CDF_VAR_NAME_LEN256);
#endif
  *data_type = (Int32) dataTypeT;
  *num_elements = (Int32) numElemsT;
  *rec_variance = (Int32) recVaryT;
  for (dimN = 0; dimN < (int) numDims; dimN++) {
     dim_variances[dimN] = (Int32) dimVarysT[dimN];
  }
  return;
}

/******************************************************************************
*  CDF_inquire_zvar.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_inquire_zvar__,
               cdf_inquire_zvar_,
               cdf_inquire_zvar,
               CDF_INQUIRE_ZVAR)
(id, var_num, var_name, data_type, num_elements, num_dims, dim_sizes,
rec_variance, dim_variances, status Fif_GHOSTARG(len))
Int32	  *id;		    	/* In: CDF identifier. */
Int32	  *var_num;	   	/* In: zVariable number. */
void	  *var_name;	    	/* Out: zVariable name. */
Int32	  *data_type;	    	/* Out: Data type. */
Int32	  *num_elements;        /* Out: Number of elements. */
Int32     *num_dims;            /* Out: Number of dimension. */
Int32     dim_sizes[];          /* Out: Dimension sizes. */
Int32     *rec_variance;        /* Out: Record variance. */
Int32	  dim_variances[]; 	/* Out: Dimension variances. */
Int32	  *status;		/* Out: CDF status code. */
Fif_GHOSTDEF(len)		/* Invisible length of "var_name"
				   (generated by FORTRAN compiler). */
{
  char varNameT[CDF_VAR_NAME_LEN256];
  long dataTypeT, numElemsT, recVaryT, dimVarysT[CDF_MAX_DIMS];
  int dimN;
  long numDimsT, dimSizesT[CDF_MAX_DIMS];
  long varNumT = (long) (*var_num - 1);

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                                     zVAR_, varNumT,
			    GET_, zVAR_NAME_, varNameT,
				  zVAR_DATATYPE_, &dataTypeT,
				  zVAR_NUMELEMS_, &numElemsT,
                                  zVAR_NUMDIMS_, &numDimsT,
                                  zVAR_DIMSIZES_, dimSizesT,
				  zVAR_RECVARY_, &recVaryT,
				  zVAR_DIMVARYS_, dimVarysT,
			    NULL_);
  if (StatusBAD(*status)) return;

#if defined(Fif_GHOSTLEN)
  CtoFORTstring (varNameT, var_name, Fif_GHOSTUSE(len));
#else
  CtoFORTstring (varNameT, var_name, CDF_VAR_NAME_LEN256);
#endif
  *data_type = (Int32) dataTypeT;
  *num_elements = (Int32) numElemsT;
  *num_dims = (Int32) numDimsT;
  *rec_variance = (Int32) recVaryT;
  for (dimN = 0; dimN < (int) numDimsT; dimN++) {
     dim_variances[dimN] = (Int32) dimVarysT[dimN];
     dim_sizes[dimN] = (Int32) dimSizesT[dimN];
  }
  return;
}

/******************************************************************************
*  CDF_var_put.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_var_put__,cdf_var_put_,cdf_var_put,CDF_VAR_PUT)
(id, var_num, record_num, indices, value, status)
Int32	  *id;		    	/* In: CDF identifier. */
Int32	  *var_num;	   	/* In: rVariable number. */
Int32	  *record_num;		/* In: Record number. */
Int32	  indices[];	    	/* In: Dimension indices. */
void	  *value;	    	/* In: Value.
					VMS: If character data type, could
					     be passed by reference or by
					     descriptor. */
Int32	  *status;		/* Out: CDF status code. */
{
  int dimN;
  long indicesT[CDF_MAX_DIMS], numDims, dataType;
  long varNumT = (long) (*var_num - 1);
  long recNumT = (long) (*record_num - 1);

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     rVAR_, varNumT,
			    GET_, rVARs_NUMDIMS_, &numDims,
				  rVAR_DATATYPE_, &dataType,
			    NULL_);
  if (StatusBAD(*status)) return;

  for (dimN = 0; dimN < (int) numDims; dimN++) {
     indicesT[dimN] = (long) (indices[dimN] - 1);
  }

  *status = (Int32) CDFlib (SELECT_, rVARs_RECNUMBER_, recNumT,
				     rVARs_DIMINDICES_, indicesT,
			    PUT_, rVAR_DATA_,
#if defined(Fif_DESCR)
					      BOO(STRINGdataType(dataType),
						  DESCRtoREF(value),value),
#else
					      value,
#endif
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_put_zvar_data.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_put_zvar_data__,
               cdf_put_zvar_data_,
               cdf_put_zvar_data,
               CDF_PUT_ZVAR_DATA)
(id, var_num, record_num, indices, value, status)
Int32	  *id;		    	/* In: CDF identifier. */
Int32	  *var_num;	   	/* In: zVariable number. */
Int32	  *record_num;		/* In: Record number. */
Int32	  indices[];	    	/* In: Dimension indices. */
void	  *value;	    	/* In: Value.
					VMS: If character data type, could
					     be passed by reference or by
					     descriptor. */
Int32	  *status;		/* Out: CDF status code. */
{
  int dimN;
  long indicesT[CDF_MAX_DIMS], numDims, dataType;
  long varNumT = (long) (*var_num - 1);
  long recNumT = (long) (*record_num - 1);

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     zVAR_, varNumT,
			    GET_, zVAR_NUMDIMS_, &numDims,
				  zVAR_DATATYPE_, &dataType,
			    NULL_);
  if (StatusBAD(*status)) return;

  for (dimN = 0; dimN < (int) numDims; dimN++) {
     indicesT[dimN] = (long) (indices[dimN] - 1);
  }

  *status = (Int32) CDFlib (SELECT_, zVAR_RECNUMBER_, recNumT,
				     zVAR_DIMINDICES_, indicesT,
			    PUT_, zVAR_DATA_,
#if defined(Fif_DESCR)
					      BOO(STRINGdataType(dataType),
						  DESCRtoREF(value),value),
#else
					      value,
#endif
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_var_get.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_var_get__,cdf_var_get_,cdf_var_get,CDF_VAR_GET)
(id, var_num, record_num, indices, value, status)
Int32	  *id;		    	/* In: CDF identifier. */
Int32	  *var_num;  	 	/* In: rVariable number. */
Int32	  *record_num;		/* In: Record number. */
Int32	  indices[];	    	/* In: Dimension indices. */
void	  *value;	    	/* In: Value.
					VMS: If character data type, could
					     be passed by reference or by
					     descriptor. */
Int32	  *status;		/* Out: CDF status code. */
{
  long indicesT[CDF_MAX_DIMS], numDims, dataType;
  int dimN;
  long varNumT = (long) (*var_num - 1);
  long recNumT = (long) (*record_num - 1);

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     rVAR_, varNumT,
			    GET_, rVARs_NUMDIMS_, &numDims,
				  rVAR_DATATYPE_, &dataType,
			    NULL_);
  if (StatusBAD(*status)) return;

  for (dimN = 0; dimN < (int) numDims; dimN++) {
     indicesT[dimN] = (long) (indices[dimN] - 1);
  }

  *status = (Int32) CDFlib (SELECT_, rVARs_RECNUMBER_, recNumT,
				     rVARs_DIMINDICES_, indicesT,
			    GET_, rVAR_DATA_,
#if defined(Fif_DESCR)
					      BOO(STRINGdataType(dataType),
						  DESCRtoREF(value),value),
#else
					      value,
#endif
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_get_zvar_data.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvar_data__,
               cdf_get_zvar_data_,
               cdf_get_zvar_data,
               CDF_get_ZVAR_DATA)
(id, var_num, record_num, indices, value, status)
Int32	  *id;		    	/* In: CDF identifier. */
Int32	  *var_num;  	 	/* In: zVariable number. */
Int32	  *record_num;		/* In: Record number. */
Int32	  indices[];	    	/* In: Dimension indices. */
void	  *value;	    	/* In: Value.
					VMS: If character data type, could
					     be passed by reference or by
					     descriptor. */
Int32	  *status;		/* Out: CDF status code. */
{
  long indicesT[CDF_MAX_DIMS], numDims, dataType;
  int dimN;
  long varNumT = (long) (*var_num - 1);
  long recNumT = (long) (*record_num - 1);

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     zVAR_, varNumT,
			    GET_, zVAR_NUMDIMS_, &numDims,
				  zVAR_DATATYPE_, &dataType,
			    NULL_);
  if (StatusBAD(*status)) return;

  for (dimN = 0; dimN < (int) numDims; dimN++) {
     indicesT[dimN] = (long) (indices[dimN] - 1);
  }

  *status = (Int32) CDFlib (SELECT_, zVAR_RECNUMBER_, recNumT,
				     zVAR_DIMINDICES_, indicesT,
			    GET_, zVAR_DATA_,
#if defined(Fif_DESCR)
					      BOO(STRINGdataType(dataType),
						  DESCRtoREF(value),value),
#else
					      value,
#endif
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_var_hyper_put.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_var_hyper_put__,
	       cdf_var_hyper_put_,
	       cdf_var_hyper_put,
	       CDF_VAR_HYPER_PUT)
(id, var_num, rec_start, rec_count, rec_int, indices, counts,
intervals, buffer, status)
Int32	  *id;	  	  	/* In: CDF identifier. */
Int32	  *var_num;   		/* In: rVariable number. */
Int32	  *rec_start;		/* In: Starting record number. */
Int32	  *rec_count;		/* In: Record count. */
Int32	  *rec_int;		/* In: Record interval. */
Int32	  indices[];	    	/* In: Dimension indices. */
Int32	  counts[];	    	/* In: Dimension counts. */
Int32	  intervals[];	    	/* In: Dimension intervals. */
void	  *buffer;	    	/* In: Values.
					VMS: If character data type, could
					     be passed by reference or
					     descriptor. */
Int32	  *status;		/* Out: CDF status code. */
{
  int dimN;
  long indicesT[CDF_MAX_DIMS], countsT[CDF_MAX_DIMS], intervalsT[CDF_MAX_DIMS],
       numDims, dataType;
  long varNumT = (long) (*var_num - 1);
  long recStartT = (long) (*rec_start - 1);
  long recCountT = (long) *rec_count;
  long recIntervalT = (long) *rec_int;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     rVAR_, varNumT,
			    GET_, rVARs_NUMDIMS_, &numDims,
				  rVAR_DATATYPE_, &dataType,
			    NULL_);
  if (StatusBAD(*status)) return;

  for (dimN = 0; dimN < (int) numDims; dimN++) {
     indicesT[dimN] = (long) (indices[dimN] - 1);
     countsT[dimN] = (long) counts[dimN];
     intervalsT[dimN] = (long) intervals[dimN];
  }

  *status = (Int32) CDFlib (SELECT_, rVARs_RECNUMBER_, recStartT,
				     rVARs_RECCOUNT_, recCountT,
				     rVARs_RECINTERVAL_, recIntervalT,
				     rVARs_DIMINDICES_, indicesT,
				     rVARs_DIMCOUNTS_, countsT,
				     rVARs_DIMINTERVALS_, intervalsT,
			    PUT_, rVAR_HYPERDATA_,
#if defined(Fif_DESCR)
						BOO(STRINGdataType(dataType),
						    DESCRtoREF(buffer),buffer),
#else
						buffer,
#endif
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_hyper_put_zvar_data.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_hyper_put_zvar_data__,
	       cdf_hyper_put_zvar_data_,
	       cdf_hyper_put_zvar_data,
	       CDF_HYPER_PUT_ZVAR_DATA)
(id, var_num, rec_start, rec_count, rec_int, indices, counts,
intervals, buffer, status)
Int32	  *id;	  	  	/* In: CDF identifier. */
Int32	  *var_num;   		/* In: zVariable number. */
Int32	  *rec_start;		/* In: Starting record number. */
Int32	  *rec_count;		/* In: Record count. */
Int32	  *rec_int;		/* In: Record interval. */
Int32	  indices[];	    	/* In: Dimension indices. */
Int32	  counts[];	    	/* In: Dimension counts. */
Int32	  intervals[];	    	/* In: Dimension intervals. */
void	  *buffer;	    	/* In: Values.
					VMS: If character data type, could
					     be passed by reference or
					     descriptor. */
Int32	  *status;		/* Out: CDF status code. */
{
  int dimN;
  long indicesT[CDF_MAX_DIMS], countsT[CDF_MAX_DIMS], intervalsT[CDF_MAX_DIMS],
       numDims, dataType;
  long varNumT = (long) (*var_num - 1);
  long recStartT = (long) (*rec_start - 1);
  long recCountT = (long) *rec_count;
  long recIntervalT = (long) *rec_int;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     zVAR_, varNumT,
			    GET_, zVAR_NUMDIMS_, &numDims,
				  zVAR_DATATYPE_, &dataType,
			    NULL_);
  if (StatusBAD(*status)) return;

  for (dimN = 0; dimN < (int) numDims; dimN++) {
     indicesT[dimN] = (long) (indices[dimN] - 1);
     countsT[dimN] = (long) counts[dimN];
     intervalsT[dimN] = (long) intervals[dimN];
  }

  *status = (Int32) CDFlib (SELECT_, zVAR_RECNUMBER_, recStartT,
				     zVAR_RECCOUNT_, recCountT,
				     zVAR_RECINTERVAL_, recIntervalT,
				     zVAR_DIMINDICES_, indicesT,
				     zVAR_DIMCOUNTS_, countsT,
				     zVAR_DIMINTERVALS_, intervalsT,
			    PUT_, zVAR_HYPERDATA_,
#if defined(Fif_DESCR)
						BOO(STRINGdataType(dataType),
						    DESCRtoREF(buffer),buffer),
#else
						buffer,
#endif
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_var_hyper_get.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_var_hyper_get__,
	       cdf_var_hyper_get_,
	       cdf_var_hyper_get,
	       CDF_VAR_HYPER_GET)
(id, var_num, rec_start, rec_count, rec_int, indices, counts,
intervals, buffer, status)
Int32	  *id;		    	/* In: CDF identifier. */
Int32	  *var_num;	   	/* In: rVariable number. */
Int32	  *rec_start; 		/* In: Starting record number. */
Int32	  *rec_count; 		/* In: Record count. */
Int32	  *rec_int; 		/* In: Record interval. */
Int32	  indices[];	    	/* In: Dimension indices. */
Int32	  counts[];	    	/* In: Dimension counts. */
Int32	  intervals[];	    	/* In: Dimension intervals. */
void	  *buffer;	    	/* In: Values.
					VMS: If character data type, could
					     be passed by reference or
					     descriptor. */
Int32	  *status;		/* Out: CDF status code. */
{
  int dimN;
  long indicesT[CDF_MAX_DIMS], countsT[CDF_MAX_DIMS], intervalsT[CDF_MAX_DIMS],
       numDims, dataType;
  long varNumT = (long) (*var_num - 1);
  long recStartT = (long) (*rec_start - 1);
  long recCountT = (long) *rec_count;
  long recIntervalT = (long) *rec_int;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     rVAR_, varNumT,
			    GET_, rVARs_NUMDIMS_, &numDims,
				  rVAR_DATATYPE_, &dataType,
			    NULL_);
  if (StatusBAD(*status)) return;

  for (dimN = 0; dimN < (int) numDims; dimN++) {
     indicesT[dimN] = (long) (indices[dimN] - 1);
     countsT[dimN] = (long) counts[dimN];
     intervalsT[dimN] = (long) intervals[dimN];
  }

  *status = (Int32) CDFlib (SELECT_, rVARs_RECNUMBER_, recStartT,
				     rVARs_RECCOUNT_, recCountT,
				     rVARs_RECINTERVAL_, recIntervalT,
				     rVARs_DIMINDICES_, indicesT,
				     rVARs_DIMCOUNTS_, countsT,
				     rVARs_DIMINTERVALS_, intervalsT,
			    GET_, rVAR_HYPERDATA_,
#if defined(Fif_DESCR)
						BOO(STRINGdataType(dataType),
						    DESCRtoREF(buffer),buffer),
#else
						buffer,
#endif
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_hyper_get_zvar_data.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_hyper_get_zvar_data__,
	       cdf_hyper_get_zvar_data_,
	       cdf_hyper_get_zvar_data,
	       CDF_HYPER_GET_ZVAR_DATA)
(id, var_num, rec_start, rec_count, rec_int, indices, counts,
intervals, buffer, status)
Int32	  *id;		    	/* In: CDF identifier. */
Int32	  *var_num;	   	/* In: zVariable number. */
Int32	  *rec_start; 		/* In: Starting record number. */
Int32	  *rec_count; 		/* In: Record count. */
Int32	  *rec_int; 		/* In: Record interval. */
Int32	  indices[];	    	/* In: Dimension indices. */
Int32	  counts[];	    	/* In: Dimension counts. */
Int32	  intervals[];	    	/* In: Dimension intervals. */
void	  *buffer;	    	/* In: Values.
					VMS: If character data type, could
					     be passed by reference or
					     descriptor. */
Int32	  *status;		/* Out: CDF status code. */
{
  int dimN;
  long indicesT[CDF_MAX_DIMS], countsT[CDF_MAX_DIMS], intervalsT[CDF_MAX_DIMS],
       numDims, dataType;
  long varNumT = (long) (*var_num - 1);
  long recStartT = (long) (*rec_start - 1);
  long recCountT = (long) *rec_count;
  long recIntervalT = (long) *rec_int;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     zVAR_, varNumT,
			    GET_, zVAR_NUMDIMS_, &numDims,
				  zVAR_DATATYPE_, &dataType,
			    NULL_);
  if (StatusBAD(*status)) return;

  for (dimN = 0; dimN < (int) numDims; dimN++) {
     indicesT[dimN] = (long) (indices[dimN] - 1);
     countsT[dimN] = (long) counts[dimN];
     intervalsT[dimN] = (long) intervals[dimN];
  }

  *status = (Int32) CDFlib (SELECT_, zVAR_RECNUMBER_, recStartT,
				     zVAR_RECCOUNT_, recCountT,
				     zVAR_RECINTERVAL_, recIntervalT,
				     zVAR_DIMINDICES_, indicesT,
				     zVAR_DIMCOUNTS_, countsT,
				     zVAR_DIMINTERVALS_, intervalsT,
			    GET_, zVAR_HYPERDATA_,
#if defined(Fif_DESCR)
						BOO(STRINGdataType(dataType),
						    DESCRtoREF(buffer),buffer),
#else
						buffer,
#endif
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_var_close.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_var_close__,cdf_var_close_,cdf_var_close,CDF_VAR_CLOSE)
(id, var_num, status)
Int32	  *id;	  	  	/* In: CDF identifier. */
Int32	  *var_num;   		/* In: rVariable number. */
Int32	  *status;		/* Out: CDF status code. */
{
  long varNumT = (long) (*var_num - 1);
  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     rVAR_, varNumT,
			    CLOSE_, rVAR_,
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_close_zvar.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_close_zvar__,cdf_close_zvar_,cdf_close_zvar,CDF_CLOSE_ZVAR)
(id, var_num, status)
Int32	  *id;	  	  	/* In: CDF identifier. */
Int32	  *var_num;   		/* In: zVariable number. */
Int32	  *status;		/* Out: CDF status code. */
{
  long varNumT = (long) (*var_num - 1);
  CDFstatus statusT;

  statusT = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
				     zVAR_, varNumT,
			    CLOSE_, zVAR_,
			    NULL_);
  return;
}

/******************************************************************************
*  CDF_error.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_error__,cdf_error_,cdf_error,CDF_ERROR)
(statusI, text, statusO Fif_GHOSTARG(text_len))
Int32	  *statusI;		/* In: CDF status code. */
void	  *text;		/* Out: Character string to receive
					explanation. */
Int32     *statusO;             /* Out: CDF operation status. */
Fif_GHOSTDEF(text_len)		/* Invisible length of "text"
				   (generated by FORTRAN compiler). */
{
  char statusTextT[CDF_ERRTEXT_LEN+1];
  *statusO = (Int32) CDFlib (SELECT_, CDF_STATUS_, (CDFstatus) *statusI,
		             GET_, STATUS_TEXT_, statusTextT,
		             NULL_);
  if (StatusOK(*statusO))
#if defined(Fif_GHOSTLEN)
    CtoFORTstring (statusTextT, text, Fif_GHOSTUSE(text_len));
#else
    CtoFORTstring (statusTextT, text, CDF_STATUSTEXT_LEN);
#endif
  else
#if defined(Fif_GHOSTLEN)
    CtoFORTstring ("Unknown CDF status code", text, Fif_GHOSTUSE(text_len));
#else
    CtoFORTstring ("Unknown CDF status code", text, CDF_STATUSTEXT_LEN);
#endif
  return;
}

/******************************************************************************
*  CDF_get_status_text.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_status_text__,
               cdf_get_status_text_,
               cdf_get_status_text,
               CDF_GET_STATUS_TEXT)
(statusI, text, statusO Fif_GHOSTARG(text_len))
Int32	  *statusI;		/* In: CDF status code. */
void	  *text;		/* Out: Character string to receive
					explanation. */
Int32     *statusO;             /* Out: CDF operation status. */
Fif_GHOSTDEF(text_len)		/* Invisible length of "text"
				   (generated by FORTRAN compiler). */
{
  char statusTextT[CDF_ERRTEXT_LEN+1];
  *statusO = (Int32) CDFlib (SELECT_, CDF_STATUS_, (CDFstatus) *statusI,
		             GET_, STATUS_TEXT_, statusTextT,
		             NULL_);
  if (StatusOK(*statusO))
#if defined(Fif_GHOSTLEN)
    CtoFORTstring (statusTextT, text, Fif_GHOSTUSE(text_len));
#else
    CtoFORTstring (statusTextT, text, CDF_STATUSTEXT_LEN);
#endif
  else
#if defined(Fif_GHOSTLEN)
    CtoFORTstring ("Unknown CDF status code", text, Fif_GHOSTUSE(text_len));
#else
    CtoFORTstring ("Unknown CDF status code", text, CDF_STATUSTEXT_LEN);
#endif
  return;
}

/******************************************************************************
*  CDF_getrVarsRecordData.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_getrvarsrecorddata__,
               cdf_getrvarsrecorddata_,
               cdf_getrvarsrecorddata,
               CDF_GETRVARSRECORDDATA)
(id, num_vars, varNums, rec_num, buffptr, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *num_vars;            /* In: Number of rVariables. */
Int32     varNums[];            /* In: rVariable numbers. */
Int32     *rec_num;             /* In: Record number. */
void      *buffptr;             /* In: Pointer for input data */
Int32     *status;              /* Out: CDF status code. */
{
  long num_varsT = (long) *num_vars;
  long recNumT = (long) (*rec_num - 1);
  long *varNumsT;
  int i;

  if (*num_vars < 1) {
    *status = (Int32) CDF_OK;
    return;
  }

  varNumsT = cdf_AllocateMemory ((size_t)*num_vars*sizeof(long), NULL);
  if (varNumsT == NULL) {
    *status = BAD_MALLOC;
    return;
  }

  for (i = 0; i < *num_vars; i++) varNumsT[i] = (long) varNums[i] - 1;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                            NULL_);
  if (StatusBAD(*status)) return;

  *status = (Int32) CDFlib (SELECT_, rVARs_RECNUMBER_, recNumT,
                            GET_, rVARs_RECDATA_, num_varsT, varNumsT, buffptr,
                            NULL_);
  cdf_FreeMemory (varNumsT, NULL);
  return;

}

/******************************************************************************
*  CDF_getzVarsRecordData.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_getzvarsrecorddata__,
               cdf_getzvarsrecorddata_,
               cdf_getzvarsrecorddata,
               CDF_GETZVARSRECORDDATA)
(id, num_vars, varNums, rec_num, buffptr, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *num_vars;            /* In: Number of zVariables. */
Int32     varNums[];            /* In: zVariable numbers. */
Int32     *rec_num;             /* In: Record number. */
void      *buffptr;             /* In: Pointer for input data */
Int32     *status;              /* Out: CDF status code. */
{
  long num_varsT = (long) *num_vars;
  long recNumT = (long) (*rec_num - 1);
  long *varNumsT;
  int i;

  if (*num_vars < 1) {
    *status = (Int32) CDF_OK;
    return;
  }

  varNumsT = cdf_AllocateMemory ((size_t)*num_vars*sizeof(long), NULL);
  if (varNumsT == NULL) {
    *status = BAD_MALLOC;
    return;
  }

  for (i = 0; i < *num_vars; i++) varNumsT[i] = (long) varNums[i] - 1;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                            NULL_);
  if (StatusBAD(*status)) return;

  *status = (Int32) CDFlib (SELECT_, zVARs_RECNUMBER_, recNumT,
                            GET_, zVARs_RECDATA_, num_varsT, varNumsT, buffptr,
                            NULL_);
  cdf_FreeMemory (varNumsT, NULL);

  return;

}

/******************************************************************************
*  CDF_putrVarsRecordData.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_putrvarsrecorddata__,
               cdf_putrvarsrecorddata_,
               cdf_putrvarsrecorddata,
               CDF_PUTRVARSRECORDDATA)
(id, num_vars, varNums, rec_num, buffptr, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *num_vars;            /* In: Number of rVariables. */
Int32     varNums[];            /* In: rVariable numbers. */
Int32     *rec_num;             /* In: Record number. */
void      *buffptr;             /* In: Pointer for output data */
Int32     *status;              /* Out: CDF status code. */
{
  long num_varsT = (long) *num_vars;
  long recNumT = (long) (*rec_num - 1);
  long *varNumsT;
  int i;

  if (*num_vars < 1) {
    *status = (Int32) CDF_OK;
    return;
  }

  varNumsT = cdf_AllocateMemory ((size_t)*num_vars*sizeof(long), NULL);
  if (varNumsT == NULL) {
    *status = BAD_MALLOC;
    return;
  }

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                            NULL_);
  if (StatusBAD(*status)) return;

  for (i = 0; i < *num_vars; i++) varNumsT[i] = (long) varNums[i] - 1;

  *status = (Int32) CDFlib (SELECT_, rVARs_RECNUMBER_, recNumT,
                            PUT_, rVARs_RECDATA_, num_varsT, varNumsT, buffptr,
                            NULL_);
  cdf_FreeMemory (varNumsT, NULL);
  return;
}

/******************************************************************************
*  CDF_putzVarsRecordData.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_putzvarsrecorddata__,
               cdf_putzvarsrecorddata_,
               cdf_putzvarsrecorddata,
               CDF_PUTZVARSRECORDDATA)
(id, num_vars, varNums, rec_num, buffptr, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *num_vars;            /* In: Number of zVariables. */
Int32     varNums[];            /* In: zVariable numbers. */
Int32     *rec_num;             /* In: Record number. */
void      *buffptr;             /* In: Pointer for output data */
Int32     *status;              /* Out: CDF status code. */
{
  long num_varsT = (long) *num_vars;
  long recNumT = (long) (*rec_num - 1);
  long *varNumsT;
  int i;

  if (*num_vars < 1) {
    *status = (Int32) CDF_OK;
    return;
  }

  varNumsT = cdf_AllocateMemory ((size_t)*num_vars*sizeof(long), NULL);
  if (varNumsT == NULL) {
    *status = BAD_MALLOC;
    return;
  }

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                            NULL_);
  if (StatusBAD(*status)) return;

  for (i = 0; i < *num_vars; i++) varNumsT[i] = (long) varNums[i] - 1;

  *status = (Int32) CDFlib (SELECT_, zVARs_RECNUMBER_, recNumT,
                            PUT_, zVARs_RECDATA_, num_varsT, varNumsT, buffptr,
                            NULL_);
  cdf_FreeMemory (varNumsT, NULL);
  return;
}

/******************************************************************************
*  CDF_get_zVars_RecordData.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_zvars_recorddata__,
               cdf_get_zvars_recorddata_,
               cdf_get_zvars_recorddata,
               CDF_GET_ZVARS_RECORDDATA)
(id, num_vars, varNums, rec_num, buffptr, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *num_vars;            /* In: Number of zVariables. */
Int32     varNums[];            /* In: zVariable numbers. */
Int32     *rec_num;             /* In: Record number. */
void      *buffptr;             /* In: Pointer for input data */
Int32     *status;              /* Out: CDF status code. */
{
  long num_varsT = (long) *num_vars;
  long recNumT = (long) (*rec_num - 1);
  long *varNumsT;
  int i;

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                            NULL_);
  if (StatusBAD(*status)) return;

  varNumsT = cdf_AllocateMemory ((size_t)*num_vars*sizeof(long), NULL);
  if (varNumsT == NULL) {
    *status = BAD_MALLOC;
    return;
  }

  for (i = 0; i < *num_vars; i++) varNumsT[i] = (long) varNums[i] - 1;

  *status = (Int32) CDFlib (SELECT_, zVARs_RECNUMBER_, recNumT,
                            GET_, zVARs_RECDATA_, num_varsT, varNumsT, buffptr,
                            NULL_);
  cdf_FreeMemory (varNumsT, NULL);
  return;

}

/******************************************************************************
*  CDF_put_zVars_RecordData.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_put_zvars_recorddata__,
               cdf_put_zvars_recorddata_,
               cdf_put_zvars_recorddata,
               CDF_PUT_ZVARS_RECORDDATA)
(id, num_vars, varNums, rec_num, buffptr, status)
Int32     *id;                  /* In: CDF identifier. */
Int32     *num_vars;            /* In: Number of zVariables. */
Int32     varNums[];            /* In: zVariable numbers. */
Int32     *rec_num;             /* In: Record number. */
void      *buffptr;             /* In: Pointer for output data */
Int32     *status;              /* Out: CDF status code. */
{
  long num_varsT = (long) *num_vars;
  long recNumT = (long) (*rec_num - 1);
  long *varNumsT;
  int i;

  if (*num_vars < 1) {
    *status = (Int32) CDF_OK;
    return;
  }

  *status = (Int32) CDFlib (SELECT_, CDF_, Int32ToCDFid(*id),
                            NULL_);
  if (StatusBAD(*status)) return;

  varNumsT = cdf_AllocateMemory ((size_t)*num_vars*sizeof(long), NULL);
  if (varNumsT == NULL) {
    *status = BAD_MALLOC;
    return;
  }

  for (i = 0; i < *num_vars; i++) varNumsT[i] = (long) varNums[i] - 1;

  *status = (Int32) CDFlib (SELECT_, zVARs_RECNUMBER_, recNumT,
                            PUT_, zVARs_RECDATA_, num_varsT, varNumsT, buffptr,
                            NULL_);
  cdf_FreeMemory (varNumsT, NULL);
  return;
}

/******************************************************************************
*  CDF_set_FileBackward.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_set_filebackward__,cdf_set_filebackward_,
               cdf_set_filebackward, CDF_SET_FILEBACKWARD)
(flag)
Int32     *flag;                /* In: Flag to set the file to old version. */
{
  int fg = (int) *flag;
  CDFsetFileBackward(fg);
  return;
}

/******************************************************************************
*  CDF_get_FileBackward.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_filebackward__,cdf_get_filebackward_,
               cdf_get_filebackward, CDF_GET_FILEBACKWARD)
()
{

  int flag;
  flag = CDFgetFileBackward();
  if (flag == 0) return (Int32) 0;
  else return (Int32) 1;
}

/******************************************************************************
*  CDF_set_Validate.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
void
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_set_validate__,cdf_set_validate_,
               cdf_set_validate, CDF_SET_VALIDATE)
(flag)
Int32     *flag;                /* In: Flag to set the file to old version. */
{
  int fg = (int) *flag;
  CDFsetValidate(fg);
  return;
}

/******************************************************************************
*  CDF_get_validate.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_get_validate__,cdf_get_validate_,
               cdf_get_validate, CDF_GET_VALIDATE)
()
{

  int flag;
  flag = CDFgetValidate();
  if (flag == 0) return (Int32) 0;
  else return (Int32) 1;
}

