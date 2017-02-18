/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF				      CDF Internal Interface/FORTRAN.
*
*  Version 1.8b, 21-Feb-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  30-Jan-91, J Love     Original version (for CDF V2.1).
*   V1.1   6-Aug-91, J Love     Use 'CDFlib'.  If variable is a character data
*                               type, check for %DESCR (if VMS).
*   V1.2  24-Oct-91, J Love     Modified for IBM-PC and IBM-RS6000 ports.
*   V1.3  20-May-92, J Love     CDF V2.2.
*   V1.4  16-Sep-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V1.5  19-Jan-94, J Love     CDF V2.4.
*   V1.5a  4-Feb-94, J Love	DEC Alpha/OpenVMS port.
*   V1.5b 22-Feb-94, J Love	Spelling lesson.
*   V1.5c 29-Mar-94, J Love	Solaris using GNU C compiler.
*   V1.6  21-Dec-94, J Love	CDF V2.5.
*   V1.7  19-Jan-95, J Love	IRIX 6.0 (64-bit).
*   V1.7a 30-Jan-95, J Love	`int sC' -> `Int32 sC'.
*   V1.7b 13-Jun-95, J Love	Linux.
*   V1.8   5-Aug-96, J Love	CDF V2.6 (renamed - previously `cdf_i_if.c').
*   V1.8a  2-Dec-96, J Love	Fixed declaration of `i' (now `int').
*   V1.8b 21-Feb-97, J Love	Removed RICE.
*
******************************************************************************/

/******************************************************************************
*
* Notes:
*
*   For the FORTRAN interfaces everything is indexed from one (1) while for
* the C interfaces everything is indexed from zero (0) [eg. variable and
* attribute numbers, record numbers, indices, entry numbers].  For this
* reason one (1) is subtracted from arguments passed in while one (1) is
* added to arguments passed out (where appropriate).
*
*   The FORTRAN interfaces use INTEGER*4 as the data type for all numerical
* values passed in/out while the C interfaces use `long'.  These are the same
* on all supported platforms except the DEC Alpha (where `long' is 8 bytes).
* For this reason temporary variables/arrays of size `long' are used in the
* calls to the CDF library (the direct C interface).  This causes a small
* performance loss on the other platforms mainly when reading/writing single
* values repeatly (but then the hyper reads/writes should have been used
* instead).
*
******************************************************************************/

/******************************************************************************
*
* Notes for VMS:
*
*      To make the user's life a little easier, all names and attribute
* values (for attributes of data types CDF_CHAR and CDF_UCHAR) may be passed
* in and out by either reference or descriptor.  The default passing mode
* for an embedded character string (e.g., CALL subr (..., 'string', ...)) or
* a CHARACTER variable symbol (e.g., CALL subr (..., ATTR_NAME, ...) where
* ATTR_NAME is defined as CHARACTER*8) is by descriptor when passing from
* FORTRAN to C in VMS.
*
*      An embedded character string could be enclosed in %REF() to force
* passing by reference since the FORTRAN compiler puts a NUL character at
* the end of these strings as expected by the CDF V2.0 library (written in
* C).  Enclosing a CHARACTER variable symbol in %REF() will result in
* an error, however, because a NUL character is not placed at the ends of
* these type strings by the FORTRAN compiler.  The user would have to
* supply the terminating NUL.  By letting the passing mode default to by-
* descriptor, this interface will supply the terminating NUL.
*
*      The main difference here from CDF Version 1 is that the %REF() is
* not needed when passing out names and attribute values.  Also, %REF()
* is not necessary when passing the value of a variable that is a character
* data type.
*
******************************************************************************/

/******************************************************************************
*
* Notes for UNIX:
*
*   All passing between FORTRAN and C on UNIX systems is done by reference.
* When character strings are passed between FORTRAN and C, extra arguments
* are added to the argument list containing the lengths of those character
* strings (EXCEPT on NeXT machines - FORTRAN applications must NUL-terminate
* passed character strings).
*
*   Entry points have been made lowercase because the FORTRAN compiler
* converts all uppercase characters to lowercase in entry points.  This way
* the linker will find everything.  Unix FORTRAN compilers and linkers also
* seem to like trailing '_'s (except on the IBM-RS6000/AIX, HP9000/HP-UX,
* and NeXT/Mach).
*
******************************************************************************/

/******************************************************************************
*
* Notes for Macintosh (MPW C/Fortran):
*
*   In order to get the Internal Interface to work with MPW Fortran a separate
* entry point has been created for each possible number of arguments.  They
* are called CDF_lib_1, CDF_lib_2,... CDF_lib_n.  This is because MPW Fortran
* will not allow varying numbers of arguments to the same subroutine/function.
* An application must use CDF_lib_x when there are `x' arguments in the call.
*
******************************************************************************/

#include "cdflib.h"

/******************************************************************************
* Local function prototypes.
******************************************************************************/

static size_t PickMaxLen PROTOARGs((long requiredArgument, ...));

VISIBLE_PREFIX
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib__,cdf_lib_,cdf_lib,CDF_LIB)
VARPROTOARGs((Int32 *requiredFnc, ...));

#define CDF_LIBx Fif_ENTRYPOINT(cdf_lib__,cdf_lib_,cdf_lib,CDF_LIB)
static CDFid currentCDFid = NULL;
/******************************************************************************
* CDF_lib.  Note that CDFstatus is returned both as an argument (the last
* argument) and as the value of the function.
******************************************************************************/

VISIBLE_PREFIX
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib__,cdf_lib_,cdf_lib,CDF_LIB)
#if defined(STDARG)
(Int32 *requiredFnc, ...)
#else
(va_alist)
va_dcl
#endif
{
CDFstatus pStatus = CDF_OK;     /* Pending status. */
CDFstatus tStatus;              /* Temporary status. */
Int32 *status;                  /* Address of `status' variable. */
struct VAstruct Va;
struct STRINGstruct *ssh = NULL; /* Head of STRINGstruct linked list. */

#if !defined(STDARG)
Int32 *fncP;			/* Pointer to required function. */
#endif

#if defined(Fif_GHOSTLEN)
int i;
Int32 sC = 0;		/* String count.  Number of (possible) strings
			   passed in. */
Int32 *sCp;		/* Pointer to string count. */
Int32 *sLs;		/* Lengths of (possible) character strings passed
			   in - some arguments may or may not be character
			   strings (eg. variable/entry data). */
#endif

#if defined(Fif_GHOSTLEN)
/******************************************************************************
* Scan argument list counting number of (possible) strings passed in.
******************************************************************************/

#if defined(STDARG)
va_start (Va.ap, requiredFnc);
if (*requiredFnc == 0) {
  sCp = requiredFnc;
  Va.fnc = (long) *(va_arg (Va.ap, Int32 *));
}
else {
  sCp = &sC;
  Va.fnc = (long) *requiredFnc;
}
#else
VA_START (Va.ap);
fncP = va_arg (Va.ap, Int32 *);
if (*fncP == 0) {
  sCp = fncP;
  Va.fnc = (long) *(va_arg (Va.ap, Int32 *));
}
else {
  sCp = &sC;
  Va.fnc = (long) *fncP;
}
#endif

while (Va.fnc != NULL_) {
  switch (Va.fnc) {
    /**************************************************************************
    * CREATE_,<item>
    **************************************************************************/
    case CREATE_:
      for (;;) {
	 Va.item = (long) *(va_arg (Va.ap, Int32 *));
	 switch (Va.item) {
	   /*******************************************************************
	   * CREATE_,CDF_
	   *******************************************************************/
	   case CDF_: {
	     (void) va_arg (Va.ap, void *);
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     (*sCp)++;
	     break;
	   }
	   /*******************************************************************
	   * CREATE_,rVAR_
	   *******************************************************************/
	   case rVAR_: {
	     (void) va_arg (Va.ap, void *);
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     (*sCp)++;
	     break;
	   }
	   /*******************************************************************
	   * CREATE_,zVAR_
	   *******************************************************************/
	   case zVAR_: {
	     (void) va_arg (Va.ap, void *);
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     (*sCp)++;
	     break;
	   }
	   /*******************************************************************
	   * CREATE_,ATTR_
	   *******************************************************************/
	   case ATTR_: {
	     (void) va_arg (Va.ap, void *);
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     (*sCp)++;
	     break;
	   }
	   /*******************************************************************
	   * Unknown - hopefully the next operation.
	   *******************************************************************/
	   default: {
	     Va.fnc = Va.item;
	     break;
	   }
	 }
	 if (Va.fnc == Va.item) break;
      }
      break;
    /**************************************************************************
    * OPEN_,<item>
    **************************************************************************/
    case OPEN_:
      for (;;) {
	 Va.item = (long) *(va_arg (Va.ap, Int32 *));
	 switch (Va.item) {
	   /*******************************************************************
	   * OPEN_,CDF_
	   *******************************************************************/
	   case CDF_: {
	     (void) va_arg (Va.ap, void *);
	     (void) va_arg (Va.ap, Int32 *);
	     (*sCp)++;
	     break;
	   }
	   /*******************************************************************
	   * Unknown - hopefully the next operation.
	   *******************************************************************/
	   default: {
	     Va.fnc = Va.item;
	     break;
	   }
	 }
	 if (Va.fnc == Va.item) break;
      }
      break; 
    /**************************************************************************
    * DELETE_,<item>
    **************************************************************************/
    case DELETE_:
      for (;;) {
	 Va.item = (long) *(va_arg (Va.ap, Int32 *));
	 switch (Va.item) {
	   /*******************************************************************
	   * DELETE_,<object>
	   *******************************************************************/
	   case CDF_:
	   case zVAR_:
	   case rVAR_:
	   case ATTR_:
	   case gENTRY_:
	   case zENTRY_:
	   case rENTRY_:
	     break;
	   /*******************************************************************
	   * DELETE_,r|zVAR_RECORDS_
	   *******************************************************************/
	   case rVAR_RECORDS_:
	   case zVAR_RECORDS_: {
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     break;
	   }
	   /*******************************************************************
	   * Unknown - hopefully the next operation.
	   *******************************************************************/
	   default: {
	     Va.fnc = Va.item;
	     break;
	   }
	 }
	 if (Va.fnc == Va.item) break;
      }
      break;
    /**************************************************************************
    * CLOSE_,<item>
    **************************************************************************/
    case CLOSE_:
      for (;;) {
	 Va.item = (long) *(va_arg (Va.ap, Int32 *));
	 switch (Va.item) {
	   /*******************************************************************
	   * CLOSE_,CDF_
	   *******************************************************************/
	   case CDF_: {
	     break;
	   }
	   /*******************************************************************
	   * CLOSE_,r/zVAR_
	   *******************************************************************/
	   case rVAR_:
	   case zVAR_: {
	     break;
	   }
	   /*******************************************************************
	   * Unknown - hopefully the next operation.
	   *******************************************************************/
	   default: {
	     Va.fnc = Va.item;
	     break;
	   }
	 }
	 if (Va.fnc == Va.item) break;
      }
      break;
    /**************************************************************************
    * SELECT_,<item>
    **************************************************************************/
    case SELECT_:
      for (;;) {
	 Va.item = (long) *(va_arg (Va.ap, Int32 *));
	 switch (Va.item) {
	   /*******************************************************************
	   * SELECT_,<Int32 * or Int32 []>
	   *******************************************************************/
	   case CDF_:
	   case CDF_STATUS_:
	   case CDF_READONLY_MODE_:
	   case CDF_zMODE_:
	   case CDF_NEGtoPOSfp0_MODE_:
	   case CDF_DECODING_:
	   case CDF_CACHESIZE_:
	   case STAGE_CACHESIZE_:
	   case COMPRESS_CACHESIZE_:
	   case rVARs_CACHESIZE_:
	   case zVARs_CACHESIZE_:
	   case rVAR_CACHESIZE_:
	   case zVAR_CACHESIZE_:
	   case rVAR_:
	   case zVAR_:
	   case rVARs_RECNUMBER_:
	   case zVARs_RECNUMBER_:
	   case zVAR_RECNUMBER_:
	   case rVARs_RECCOUNT_:
	   case zVAR_RECCOUNT_:
	   case rVARs_RECINTERVAL_:
	   case zVAR_RECINTERVAL_:
	   case rVARs_DIMINDICES_:
	   case zVAR_DIMINDICES_:
	   case rVARs_DIMCOUNTS_:
	   case zVAR_DIMCOUNTS_:
	   case rVARs_DIMINTERVALS_:
	   case zVAR_DIMINTERVALS_:
	   case rVAR_RESERVEPERCENT_:
	   case zVAR_RESERVEPERCENT_:
	   case ATTR_:
	   case gENTRY_:
	   case rENTRY_:
	   case zENTRY_: {
	     (void) va_arg (Va.ap, Int32 *);
	     break;
	   }
	   /*******************************************************************
	   * SELECT_,<Int32 *, Int32 *>
	   *******************************************************************/
	   case rVAR_SEQPOS_:
	   case zVAR_SEQPOS_: {
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     break;
	   }
	   /*******************************************************************
	   * SELECT_,<void * (string)>
	   *******************************************************************/
	   case CDF_SCRATCHDIR_:
	   case rVAR_NAME_:
	   case zVAR_NAME_:
	   case ATTR_NAME_:
	   case rENTRY_NAME_:
	   case zENTRY_NAME_: {
	     (void) va_arg (Va.ap, void *);
	     (*sCp)++;
	     break;
	   }
	   /*******************************************************************
	   * Unknown item - hopefully the next operation.
	   *******************************************************************/
	   default: {
	     Va.fnc = Va.item;
	     break;
	   }
	 }
	 if (Va.fnc == Va.item) break;
      }
      break;
    /**************************************************************************
    * CONFIRM_,<item>
    **************************************************************************/
    case CONFIRM_:
      for (;;) {
	 Va.item = (long) *(va_arg (Va.ap, Int32 *));
	 switch (Va.item) {
	   /*******************************************************************
	   * CONFIRM_,<no arguments>
	   *******************************************************************/
	   case CURgENTRY_EXISTENCE_:
	   case CURrENTRY_EXISTENCE_:
	   case CURzENTRY_EXISTENCE_:
	   case CDF_CHECKSUM_: 
	     break;
	   /*******************************************************************
	   * CONFIRM_,<Int32 * or Int32 []>
	   *******************************************************************/
	   case CDF_READONLY_MODE_:
	   case CDF_zMODE_:
	   case CDF_NEGtoPOSfp0_MODE_:
	   case CDF_DECODING_:
	   case CDF_CACHESIZE_:
	   case STAGE_CACHESIZE_:
	   case COMPRESS_CACHESIZE_:
	   case rVAR_CACHESIZE_:
	   case zVAR_CACHESIZE_:
	   case rVAR_:
	   case zVAR_:
	   case rVARs_RECNUMBER_:
	   case zVAR_RECNUMBER_:
	   case rVARs_RECCOUNT_:
	   case zVAR_RECCOUNT_:
	   case rVARs_RECINTERVAL_:
	   case zVAR_RECINTERVAL_:
	   case rVARs_DIMINDICES_:
	   case zVAR_DIMINDICES_:
	   case rVARs_DIMCOUNTS_:
	   case zVAR_DIMCOUNTS_:
	   case rVARs_DIMINTERVALS_:
	   case zVAR_DIMINTERVALS_:
	   case rVAR_RESERVEPERCENT_:
	   case zVAR_RESERVEPERCENT_:
	   case ATTR_:
	   case gENTRY_:
	   case rENTRY_:
	   case zENTRY_:
	   case gENTRY_EXISTENCE_:
	   case rENTRY_EXISTENCE_:
	   case zENTRY_EXISTENCE_: {
	     (void) va_arg (Va.ap, Int32 *);
	     break;
	   }
	   /*******************************************************************
	   * CONFIRM_,<Int32 *, Int32 *>
	   *******************************************************************/
	   case rVAR_SEQPOS_:
	   case zVAR_SEQPOS_: {
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     break;
	   }
	   /*******************************************************************
	   * CONFIRM_,<void * (string)>
	   *******************************************************************/
	   case CDF_NAME_:
	   case rVAR_EXISTENCE_:
	   case zVAR_EXISTENCE_:
	   case ATTR_EXISTENCE_: {
	     (void) va_arg (Va.ap, void *);
	     (*sCp)++;
	     break;
	   }
	   /*******************************************************************
	   * Unknown - hopefully the next operation.
	   *******************************************************************/
	   default: {
	     Va.fnc = Va.item;
	     break;
	   }
	 }
	 if (Va.fnc == Va.item) break;
      }
      break;
    /**************************************************************************
    * GET_,<item>
    **************************************************************************/
    case GET_:
      for (;;) {
	 Va.item = (long) *(va_arg (Va.ap, Int32 *));
	 switch (Va.item) {
	   /*******************************************************************
	   * GET_,<Int32 *>
	   *******************************************************************/
	   case rVARs_NUMDIMS_:
	   case zVAR_NUMDIMS_:
	   case rVARs_DIMSIZES_:
	   case zVAR_DIMSIZES_:
	   case CDF_ENCODING_:
	   case CDF_MAJORITY_:
	   case CDF_FORMAT_:
	   case CDF_LEAPSECONDLASTUPDATED_:
	   case CDF_NUMrVARS_:
	   case CDF_NUMzVARS_:
	   case CDF_NUMATTRS_:
	   case CDF_NUMgATTRS_:
	   case CDF_NUMvATTRS_:
	   case rVARs_MAXREC_:
	   case zVARs_MAXREC_:
	   case CDF_VERSION_:
	   case CDF_RELEASE_:
	   case CDF_INCREMENT_:
	   case LIB_VERSION_:
	   case LIB_RELEASE_:
	   case LIB_INCREMENT_:
	   case rVAR_DATATYPE_:
	   case zVAR_DATATYPE_:
	   case rVAR_NUMELEMS_:
	   case zVAR_NUMELEMS_:
	   case rVAR_RECVARY_:
	   case zVAR_RECVARY_:
	   case rVAR_DIMVARYS_:
	   case zVAR_DIMVARYS_:
	   case rVAR_MAXREC_:
	   case zVAR_MAXREC_:
	   case rVAR_MAXallocREC_:
	   case zVAR_MAXallocREC_:
	   case rVAR_NUMRECS_:
	   case zVAR_NUMRECS_:
	   case rVAR_NUMallocRECS_:
	   case zVAR_NUMallocRECS_:
	   case rVAR_BLOCKINGFACTOR_:
	   case zVAR_BLOCKINGFACTOR_:
	   case rVAR_nINDEXRECORDS_:
	   case zVAR_nINDEXRECORDS_:
	   case rVAR_nINDEXENTRIES_:
	   case zVAR_nINDEXENTRIES_:
	   case rVAR_nINDEXLEVELS_:
	   case zVAR_nINDEXLEVELS_:
	   case rVAR_SPARSERECORDS_:
	   case zVAR_SPARSERECORDS_:
	   case ATTR_SCOPE_:
	   case ATTR_MAXgENTRY_:
	   case ATTR_MAXrENTRY_:
	   case ATTR_MAXzENTRY_:
	   case ATTR_NUMgENTRIES_:
	   case ATTR_NUMrENTRIES_:
	   case ATTR_NUMzENTRIES_:
	   case gENTRY_DATATYPE_:
	   case rENTRY_DATATYPE_:
	   case zENTRY_DATATYPE_:
	   case gENTRY_NUMELEMS_:
	   case rENTRY_NUMELEMS_:
	   case zENTRY_NUMELEMS_: 
	   case CDF_CHECKSUM_: {
	     (void) va_arg (Va.ap, Int32 *);
	     break;
	   }
	   /*******************************************************************
	   * GET_,<Int32 *, Int32 *>
	   *******************************************************************/
	   case rVAR_ALLOCATEDFROM_:
	   case zVAR_ALLOCATEDFROM_:
	   case rVAR_ALLOCATEDTO_:
	   case zVAR_ALLOCATEDTO_:
	   case DATATYPE_SIZE_: {
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     break;
	   }
	   /*******************************************************************
	   * GET_,<Int32 *, Int32 *, Int32 *>
	   *******************************************************************/
	   case CDF_COMPRESSION_: 
	   case rVAR_COMPRESSION_: 
	   case zVAR_COMPRESSION_:
	   case rVAR_SPARSEARRAYS_:
	   case zVAR_SPARSEARRAYS_: {
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     break;
	   }
	   /*******************************************************************
	   * GET_,<void * (string)>
	   *******************************************************************/
	   case CDF_COPYRIGHT_:
	   case LIB_COPYRIGHT_:
	   case LIB_subINCREMENT_:
	   case rVAR_NAME_:
	   case zVAR_NAME_:
	   case ATTR_NAME_:
	   case STATUS_TEXT_: {
	     (void) va_arg (Va.ap, void *);
	     (*sCp)++;
	     break;
	   }
	   /*******************************************************************
	   * GET_,<void * (string), Int32 *>
	   *******************************************************************/
	   case rVAR_NUMBER_:
	   case zVAR_NUMBER_:
	   case ATTR_NUMBER_: {
	     (void) va_arg (Va.ap, void *);
	     (void) va_arg (Va.ap, Int32 *);
	     (*sCp)++;
	     break;
	   }
	   /*******************************************************************
	   * GET_,<void * (string), Int32 *, Int32 *, Int32 *, Int32 *>
	   *******************************************************************/
	   case CDF_INFO_: {
	     (void) va_arg (Va.ap, void *);
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, void *);
	     (void) va_arg (Va.ap, void *);
	     (*sCp)++;
	     break;
	   }
	   /*******************************************************************
	   * GET_,<void * (possible string)>
	   *******************************************************************/
	   case rVAR_PADVALUE_:
	   case zVAR_PADVALUE_:
	   case rVAR_DATA_:
	   case zVAR_DATA_:
	   case rVAR_HYPERDATA_:
	   case zVAR_HYPERDATA_:
	   case rVAR_SEQDATA_:
	   case zVAR_SEQDATA_:
	   case gENTRY_DATA_:
	   case rENTRY_DATA_:
	   case zENTRY_DATA_: {
	     (void) va_arg (Va.ap, void *);
	     (*sCp)++;
	     break;
	   }
	   /*******************************************************************
	   * GET_,<void * (cannot be string)>
	   *******************************************************************/
	   case rVARs_RECDATA_:
	   case zVARs_RECDATA_: {
	     Int32 *nVars = va_arg (Va.ap, Int32 *);
	     Int32 *varNs = va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, void *);
	     break;
	   }
	   /*******************************************************************
	   * Unknown - hopefully the next operation.
	   *******************************************************************/
	   default: {
	     Va.fnc = Va.item;
	     break;
	   }
	 }
	 if (Va.fnc == Va.item) break;
      }
      break;
    /**************************************************************************
    * PUT_,<item>
    **************************************************************************/
    case PUT_:
      for (;;) {
	 Va.item = (long) *(va_arg (Va.ap, Int32 *));
	 switch (Va.item) {
	   /*******************************************************************
	   * PUT_,<Int32 *>
	   *******************************************************************/
	   case CDF_ENCODING_:
	   case CDF_MAJORITY_:
	   case CDF_FORMAT_:
	   case CDF_LEAPSECONDLASTUPDATED_:
	   case rVAR_RECVARY_:
	   case zVAR_RECVARY_:
	   case rVAR_DIMVARYS_:
	   case zVAR_DIMVARYS_:
	   case rVAR_ALLOCATERECS_:
	   case zVAR_ALLOCATERECS_:
	   case rVAR_INITIALRECS_:
	   case zVAR_INITIALRECS_:
	   case rVAR_BLOCKINGFACTOR_:
	   case zVAR_BLOCKINGFACTOR_:
	   case rVAR_SPARSERECORDS_:
	   case zVAR_SPARSERECORDS_:
	   case ATTR_SCOPE_: 
	   case CDF_CHECKSUM_: {
	     (void) va_arg (Va.ap, Int32 *);
	     break;
	   }
	   /*******************************************************************
	   * PUT_,<Int32 *, Int32 *>
	   *******************************************************************/
	   case CDF_COMPRESSION_:
	   case rVAR_DATASPEC_:
	   case zVAR_DATASPEC_:
	   case rVAR_COMPRESSION_:
	   case zVAR_COMPRESSION_:
	   case rVAR_SPARSEARRAYS_:
	   case zVAR_SPARSEARRAYS_:
	   case rVAR_ALLOCATEBLOCK_:
	   case zVAR_ALLOCATEBLOCK_:
	   case gENTRY_DATASPEC_:
	   case rENTRY_DATASPEC_:
	   case zENTRY_DATASPEC_: {
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     break;
	   }
	   /*******************************************************************
	   * PUT_,<void * (string)>
	   *******************************************************************/
	   case rVAR_NAME_:
	   case zVAR_NAME_:
	   case ATTR_NAME_: {
	     (void) va_arg (Va.ap, void *);
	     (*sCp)++;
	     break;
	   }
	   /*******************************************************************
	   * PUT_,<void * (possible string)>
	   *******************************************************************/
	   case rVAR_PADVALUE_:
	   case zVAR_PADVALUE_:
	   case rVAR_DATA_:
	   case zVAR_DATA_:
	   case rVAR_HYPERDATA_:
	   case zVAR_HYPERDATA_:
	   case rVAR_SEQDATA_:
	   case zVAR_SEQDATA_: {
	     (void) va_arg (Va.ap, void *);
	     (*sCp)++;
	     break;
	   }
	   /*******************************************************************
	   * PUT_,<void * (cannot be string)>
	   *******************************************************************/
	   case rVARs_RECDATA_:
	   case zVARs_RECDATA_: {
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, void *);
	     break;
	   }
	   /*******************************************************************
	   * PUT_,<Int32 *, Int32 *, void * (possible string)>
	   *******************************************************************/
	   case gENTRY_DATA_:
	   case rENTRY_DATA_:
	   case zENTRY_DATA_: {
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, Int32 *);
	     (void) va_arg (Va.ap, void *);
	     (*sCp)++;
	     break;
	   }
	   /*******************************************************************
	   * Unknown - hopefully the next operation.
	   *******************************************************************/
	   default: {
	     Va.fnc = Va.item;
	     break;
	   }
	 }
	 if (Va.fnc == Va.item) break;
      }
      break;
    /**************************************************************************
    * Unknown - this is bad.  We're lost.
    **************************************************************************/
    default:
      va_end (Va.ap);
      return ((Int32) BAD_FNC_OR_ITEM);
  }
}

status = va_arg (Va.ap, Int32 *);

/******************************************************************************
* Return if only counting possible character strings.
******************************************************************************/

if (sCp != &sC) {
  va_end (Va.ap);
  return CDF_OK;
}

/******************************************************************************
* Store character string lengths.
******************************************************************************/

if (sC > 0) {
  sLs = (Int32 *) cdf_AllocateMemory (sC * sizeof(Int32), NULL);
  if (sLs == NULL) {
    *status = (Int32) BAD_MALLOC;
    return ((Int32) BAD_MALLOC);
  }
  for (i = 0; i < sC; i++) sLs[i] = Fif_GHOSTFETCH (Va.ap);
}
else
  sLs = NULL;
va_end (Va.ap);
#endif

/******************************************************************************
* Scan argument list performing functions.
******************************************************************************/

#if defined(Fif_GHOSTLEN)
sC = 0;            /* start at beginning of list of string lengths */
#endif

#if defined(STDARG)
va_start (Va.ap, requiredFnc);
Va.fnc = (long) *requiredFnc;
#else
VA_START (Va.ap);
Va.fnc = (long) *(va_arg (Va.ap, Int32 *));
#endif

while (Va.fnc != NULL_)
  switch (Va.fnc) {
    /**************************************************************************
    * CREATE_
    **************************************************************************/
    case CREATE_:
      for (;;) {
	 Va.item = (long) *(va_arg (Va.ap, Int32 *));
	 switch (Va.item) {
	   /*******************************************************************
	   * CREATE_,CDF_
	   *******************************************************************/
	   case CDF_: {
	     void *CDFname = va_arg (Va.ap, void *);
	     Int32 numDims = *(va_arg (Va.ap, Int32 *));
	     Int32 *dimSizes = va_arg (Va.ap, Int32 *);
	     Int32 *id = va_arg (Va.ap, Int32 *); 
	     int dimN; long dimSizesT[CDF_MAX_DIMS]; CDFid idT;
	     if (StatusBAD(pStatus)) break;
	     if (numDims < 0 || numDims > CDF_MAX_DIMS) {
	       if (!sX(BAD_NUM_DIMS,&pStatus)) break;
	     }
	     for (dimN = 0; dimN < numDims; dimN++) {
		dimSizesT[dimN] = (long) dimSizes[dimN];
	     }
	     tStatus = CDFlib (CREATE_, CDF_,
#if defined(Fif_DESCR)
					      DESCRtoREFnul(CDFname,
							    CDF_PATHNAME_LEN,
							    &ssh),
#endif
#if defined(Fif_GHOSTLEN)
					      NULterminate(CDFname,sLs[sC],
							   &ssh),
#endif
#if defined(Fif_NOLEN)
					      FindEndNUL(CDFname,
							 CDF_PATHNAME_LEN,
							 &ssh),
#endif
					      (long) numDims, dimSizesT, &idT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;

#if defined(Fif_GHOSTLEN)
	     sC++;
#endif
	     *id = CDFidToInt32 (idT);
	     currentCDFid = idT;
	     break;
	   }
	   /*******************************************************************
	   * CREATE_,rVAR_
	   *******************************************************************/
	   case rVAR_: {
	     void *varName = va_arg (Va.ap, void *);
	     Int32 dataType = *(va_arg (Va.ap, Int32 *));
	     Int32 numElements = *(va_arg (Va.ap, Int32 *));
	     Int32 recVariance = *(va_arg (Va.ap, Int32 *));
	     Int32 *dimVariances = va_arg (Va.ap, Int32 *);
	     Int32 *var_num = va_arg (Va.ap, Int32 *);
	     int dimN;
	     long numDims, dimVarysT[CDF_MAX_DIMS], varNumT;
	     int LFS = FALSE;
	     if (currentCDFid != NULL) {
	       struct CDFstruct *CDF = (struct CDFstruct *)currentCDFid;
	       if (isLFS(CDF)) LFS = TRUE;
	     }

	     if (StatusBAD(pStatus)) break;

	     tStatus = CDFlib (GET_, rVARs_NUMDIMS_, &numDims,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;

	     for (dimN = 0; dimN < numDims; dimN++) {
		dimVarysT[dimN] = (long) dimVariances[dimN];
	     }

	     tStatus = CDFlib (CREATE_, rVAR_,
#if defined(Fif_DESCR)
					       DESCRtoREFnul(varName,
							     (LFS ?
							  CDF_VAR_NAME_LEN256 : 
							  CDF_VAR_NAME_LEN),
							     &ssh),
#endif
#if defined(Fif_GHOSTLEN)
					       NULterminate(varName,
							    sLs[sC],&ssh),
#endif
#if defined(Fif_NOLEN)
					       FindEndNUL(varName,
							  (LFS ? 
							 CDF_VAR_NAME_LEN256 :
							 CDF_VAR_NAME_LEN),
							  &ssh),
#endif
					       (long) dataType,
					       (long) numElements,
					       (long) recVariance,
					       dimVarysT, &varNumT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;

#if defined(Fif_GHOSTLEN)
	     sC++;
#endif
	     *var_num = (Int32) (varNumT + 1);
	     break;
	   }
	   /*******************************************************************
	   * CREATE_,zVAR_
	   *******************************************************************/
	   case zVAR_: {
	     void *varName = va_arg (Va.ap, void *);
	     Int32 dataType = *(va_arg (Va.ap, Int32 *));
	     Int32 numElements = *(va_arg (Va.ap, Int32 *));
	     Int32 numDims = *(va_arg (Va.ap, Int32 *));
	     Int32 *dimSizes = va_arg (Va.ap, Int32 *);
	     Int32 recVariance = *(va_arg (Va.ap, Int32 *));
	     Int32 *dimVariances = va_arg (Va.ap, Int32 *);
	     Int32 *var_num = va_arg (Va.ap, Int32 *);
	     int dimN;
	     long varNumT, dimSizesT[CDF_MAX_DIMS], dimVarysT[CDF_MAX_DIMS];
             int LFS = FALSE;
             if (currentCDFid != NULL) { 
               struct CDFstruct *CDF = (struct CDFstruct *)currentCDFid;
               if (isLFS(CDF)) LFS = TRUE;
             }

	     if (StatusBAD(pStatus)) break;

	     if (numDims < 0 || numDims > CDF_MAX_DIMS)
	       if (!sX(BAD_NUM_DIMS,&pStatus)) break;

	     for (dimN = 0; dimN < numDims; dimN++) {
		dimSizesT[dimN] = (long) dimSizes[dimN];
		dimVarysT[dimN] = (long) dimVariances[dimN];
	     }

	     tStatus = CDFlib (CREATE_, zVAR_,
#if defined(Fif_DESCR)
					       DESCRtoREFnul(varName,
                                                             (LFS ? 
                                                        CDF_VAR_NAME_LEN256 :
                                                        CDF_VAR_NAME_LEN),
							     &ssh),
#endif
#if defined(Fif_GHOSTLEN)
					       NULterminate(varName,sLs[sC],
						            &ssh),
#endif
#if defined(Fif_NOLEN)
					       FindEndNUL(varName,
                                                          (LFS ? 
                                                         CDF_VAR_NAME_LEN256 :
                                                         CDF_VAR_NAME_LEN),
							  &ssh),
#endif
					       (long) dataType,
					       (long) numElements,
					       (long) numDims, dimSizesT,
					       (long) recVariance, dimVarysT,
					       &varNumT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;

#if defined(Fif_GHOSTLEN)
	     sC++;
#endif
	     *var_num = (Int32) (varNumT + 1);
	     break;
	   }
	   /*******************************************************************
	   * CREATE_,ATTR_
	   *******************************************************************/
	   case ATTR_: {
	     void *attrName = va_arg (Va.ap, void *);
	     Int32 scope = *(va_arg (Va.ap, Int32 *));
	     Int32 *attr_num = va_arg (Va.ap, Int32 *);
	     long attrNumT;
             int LFS = FALSE;
             if (currentCDFid != NULL) { 
               struct CDFstruct *CDF = (struct CDFstruct *)currentCDFid;
               if (isLFS(CDF)) LFS = TRUE;
             }

	     if (StatusBAD(pStatus)) break;

	     tStatus = CDFlib (CREATE_, ATTR_,
#if defined(Fif_DESCR)
					       DESCRtoREFnul(attrName,
                                                          (LFS ? 
                                                         CDF_ATTR_NAME_LEN256 :
                                                         CDF_ATTR_NAME_LEN),
							     &ssh),
#endif
#if defined(Fif_GHOSTLEN)
					       NULterminate(attrName,sLs[sC],
							    &ssh),
#endif
#if defined(Fif_NOLEN)
					       FindEndNUL(attrName,
                                                          (LFS ? 
                                                         CDF_ATTR_NAME_LEN256 :
                                                         CDF_ATTR_NAME_LEN),
							  &ssh),
#endif
					       (long) scope, &attrNumT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;

#if defined(Fif_GHOSTLEN)
	     sC++;
#endif
	     *attr_num = (Int32) (attrNumT + 1);
	     break;
	   }
	   /*******************************************************************
	   * Unknown item - hopefully the next function.
	   *******************************************************************/
	   default: {
	     Va.fnc = Va.item;
	     break;
	   }
	 }
	 if (Va.fnc == Va.item) break;
      }
      break;

    /**************************************************************************
    * OPEN_
    **************************************************************************/
    case OPEN_:
      for (;;) {
	 Va.item = (long) *(va_arg (Va.ap, Int32 *));
	 switch (Va.item) {
	   /*******************************************************************
	   * OPEN_,CDF_
	   *******************************************************************/
	   case CDF_: {
	     void *CDFname = va_arg (Va.ap, void *);
	     Int32 *id = va_arg (Va.ap, Int32 *);
	     CDFid idT;

	     if (StatusBAD(pStatus)) break;

	     tStatus = CDFlib (OPEN_, CDF_,
#if defined(Fif_DESCR)
					    DESCRtoREFnul(CDFname,
							  CDF_PATHNAME_LEN,
							  &ssh),
#endif
#if defined(Fif_GHOSTLEN)
					    NULterminate(CDFname,sLs[sC],
						         &ssh),
#endif
#if defined(Fif_NOLEN)
					    FindEndNUL(CDFname,
						       CDF_PATHNAME_LEN,&ssh),
#endif
					    &idT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;

#if defined(Fif_GHOSTLEN)
	     sC++;
#endif
	     *id = CDFidToInt32 (idT);
	     break;
	   }
	   /*******************************************************************
	   * Unknown item - hopefully the next function.
	   *******************************************************************/
	   default: {
	     Va.fnc = Va.item;
	     break;
	   }
	 }
	 if (Va.fnc == Va.item) break;
      }
      break;

    /**************************************************************************
    * DELETE_
    **************************************************************************/
    case DELETE_:
      for (;;) {
	 Va.item = (long) *(va_arg (Va.ap, Int32 *));
	 switch (Va.item) {
	   /*******************************************************************
	   * DELETE_,<object>
	   *******************************************************************/
	   case CDF_:
	   case zVAR_:
	   case rVAR_:
	   case ATTR_:
	   case gENTRY_:
	   case zENTRY_:
	   case rENTRY_:
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (DELETE_, Va.item,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     if (Va.item == CDF_) currentCDFid = NULL; 
	     break;
	   /*******************************************************************
	   * DELETE_,r|zVAR_RECORDS_
	   *******************************************************************/
	   case rVAR_RECORDS_:
	   case zVAR_RECORDS_: {
	     Int32 *ptr1 = va_arg (Va.ap, Int32 *);
	     Int32 *ptr2 = va_arg (Va.ap, Int32 *);
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (DELETE_, Va.item, (long) (*ptr1 - 1),
						 (long) (*ptr2 - 1),
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     break;
	   }
	   /*******************************************************************
	   * Unknown item - hopefully the next function.
	   *******************************************************************/
	   default: {
	     Va.fnc = Va.item;
	     break;
	   }
	 }
	 if (Va.fnc == Va.item) break;
      }
      break;

    /**************************************************************************
    * CLOSE_
    **************************************************************************/
    case CLOSE_:
      for (;;) {
	 Va.item = (long) *(va_arg (Va.ap, Int32 *));
	 switch (Va.item) {
	   /*******************************************************************
	   * CLOSE_,CDF_
	   *******************************************************************/
	   case CDF_: {
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (CLOSE_, CDF_,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     currentCDFid = NULL;
	     break;
	   }
	   /*******************************************************************
	   * CLOSE_,rVAR_/zVAR_
	   *******************************************************************/
	   case rVAR_:
	   case zVAR_: {
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (CLOSE_, Va.item,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     break;
	   }
	   /*******************************************************************
	   * Unknown item - hopefully the next function.
	   *******************************************************************/
	   default: {
	     Va.fnc = Va.item;
	     break;
	   }
	 }
	 if (Va.fnc == Va.item) break;
      }
      break;

    /**************************************************************************
    * SELECT_
    **************************************************************************/
    case SELECT_:
      for (;;) {
	 Va.item = (long) *(va_arg (Va.ap, Int32 *));
	 switch (Va.item) {
	   /*******************************************************************
	   * SELECT_,<Int32 * (pass by value)>
	   *******************************************************************/
	   case CDF_READONLY_MODE_:
	   case CDF_zMODE_:
	   case CDF_NEGtoPOSfp0_MODE_:
	   case CDF_DECODING_:
	   case CDF_CACHESIZE_:
	   case STAGE_CACHESIZE_:
	   case COMPRESS_CACHESIZE_:
	   case rVARs_CACHESIZE_:
	   case zVARs_CACHESIZE_:
	   case rVAR_CACHESIZE_:
	   case zVAR_CACHESIZE_:
	   case rVARs_RECCOUNT_:
	   case zVAR_RECCOUNT_:
	   case rVARs_RECINTERVAL_:
	   case zVAR_RECINTERVAL_:
	   case rVAR_RESERVEPERCENT_:
	   case zVAR_RESERVEPERCENT_: {
	     Int32 value = *(va_arg (Va.ap, Int32 *));
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (SELECT_, Va.item, (long) value,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     break;
	   }
	   /*******************************************************************
	   * SELECT_,<Int32 [] (pass by reference) - CDF_MAX_DIMS maximum>
	   *******************************************************************/
	   case rVARs_DIMCOUNTS_:
	   case zVAR_DIMCOUNTS_:
	   case rVARs_DIMINTERVALS_:
	   case zVAR_DIMINTERVALS_: {
	     Int32 *ptr = va_arg (Va.ap, Int32 *);
	     Logical Z = (Va.item == zVAR_DIMCOUNTS_ ||
			  Va.item == zVAR_DIMINTERVALS_);
	     long numDims, valuesT[CDF_MAX_DIMS];
	     int dimN;

	     if (StatusBAD(pStatus)) break;

	     tStatus = CDFlib (GET_, BOO(Z,zVAR_NUMDIMS_,
					   rVARs_NUMDIMS_), &numDims,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;

	     for (dimN = 0; dimN < numDims; dimN++) {
		valuesT[dimN] = (long) ptr[dimN];
	     }

	     tStatus = CDFlib (SELECT_, Va.item, valuesT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     break;
	   }
	   /*******************************************************************
	   * SELECT_,<(Int32 *) - 1 [pass by value]>
	   *******************************************************************/
	   case rVAR_:
	   case zVAR_:
	   case rVARs_RECNUMBER_:
	   case zVARs_RECNUMBER_:
	   case zVAR_RECNUMBER_:
	   case ATTR_:
	   case gENTRY_:
	   case rENTRY_:
	   case zENTRY_: {
	     Int32 value = *(va_arg (Va.ap, Int32 *));
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (SELECT_, Va.item, (long) (value - 1),
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     break;
	   }
	   /*******************************************************************
	   * SELECT_,<(Int32 [] - 1) [pass by reference] - CDF_MAX_DIMS
	   * maximum>
	   *******************************************************************/
	   case rVARs_DIMINDICES_:
	   case zVAR_DIMINDICES_: {
	     Int32 *indices = va_arg (Va.ap, Int32 *);
	     Logical Z = (Va.item == zVAR_DIMINDICES_);
	     long numDims, indicesT[CDF_MAX_DIMS];
	     int dimN;

	     if (StatusBAD(pStatus)) break;

	     tStatus = CDFlib (GET_, BOO(Z,zVAR_NUMDIMS_,
					   rVARs_NUMDIMS_), &numDims,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;

	     for (dimN = 0; dimN < numDims; dimN++) {
		indicesT[dimN] = (long) (indices[dimN] - 1);
	     }

	     tStatus = CDFlib (SELECT_, Va.item, indicesT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     break;
	   }
	   /*******************************************************************
	   * SELECT_,<void * (character string)>
	   *******************************************************************/
	   case CDF_SCRATCHDIR_:
	   case rVAR_NAME_:
	   case zVAR_NAME_:
	   case ATTR_NAME_:
	   case rENTRY_NAME_:
	   case zENTRY_NAME_: {
	     void *ptr = va_arg (Va.ap, void *);
             int LFS = FALSE;
             size_t maxLen;

             if (currentCDFid != NULL) { 
               struct CDFstruct *CDF = (struct CDFstruct *)currentCDFid;
               if (isLFS(CDF)) LFS = TRUE;
             }

#if !defined(Fif_GHOSTLEN)
	     maxLen = PickMaxLen (Va.item, 4,
				  CDF_SCRATCHDIR_, (size_t) DU_MAX_DIR_LEN,
				  rVAR_NAME_, (LFS ? 
					(size_t) CDF_VAR_NAME_LEN256 :
					(size_t) CDF_VAR_NAME_LEN),
				  zVAR_NAME_, (LFS ? 
                                        (size_t) CDF_VAR_NAME_LEN256 :
                                        (size_t) CDF_VAR_NAME_LEN),
				  ATTR_NAME_, (LFS ? 
                                        (size_t) CDF_ATTR_NAME_LEN256 :
                                        (size_t) CDF_ATTR_NAME_LEN));
#endif

	     if (StatusBAD(pStatus)) break;

	     tStatus = CDFlib (SELECT_, Va.item,
#if defined(Fif_DESCR)
						DESCRtoREFnul(ptr,maxLen,&ssh),
#endif
#if defined(Fif_GHOSTLEN)
						NULterminate(ptr,sLs[sC],&ssh),
#endif
#if defined(Fif_NOLEN)
						FindEndNUL(ptr,maxLen,&ssh),
#endif
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;

#if defined(Fif_GHOSTLEN)
	     sC++;
#endif
	     break;
	   }
	   /*******************************************************************
	   * SELECT_,CDF_
	   *******************************************************************/
	   case CDF_: {
	     Int32 *id = va_arg (Va.ap, Int32 *);
	     if (StatusBAD(pStatus)) break;

	     tStatus = CDFlib (SELECT_, CDF_, Int32ToCDFid(*id), 
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     currentCDFid = Int32ToCDFid(*id);
	     break;
	   }
	   /*******************************************************************
	   * SELECT_,CDF_STATUS_
	   *******************************************************************/
	   case CDF_STATUS_: {
	     Int32 newStatus = *(va_arg (Va.ap, Int32 *));
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (SELECT_, CDF_STATUS_, (CDFstatus) newStatus,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     break;
	   }
	   /*******************************************************************
	   * SELECT_,rVAR_SEQPOS_/zVAR_SEQPOS_
	   *******************************************************************/
	   case rVAR_SEQPOS_:
	   case zVAR_SEQPOS_: {
	     Int32 rec_num = *(va_arg (Va.ap, Int32 *));
	     Int32 *indices = va_arg (Va.ap, Int32 *);
	     Logical Z = (Va.item == zVAR_SEQPOS_);
	     long numDims, indicesT[CDF_MAX_DIMS];
	     int dimN;

	     if (StatusBAD(pStatus)) break;

	     tStatus = CDFlib (GET_, BOO(Z,zVAR_NUMDIMS_,
					   rVARs_NUMDIMS_), &numDims,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;

	     for (dimN = 0; dimN < numDims; dimN++) {
		indicesT[dimN] = (long) (indices[dimN] - 1);
	     }

	     tStatus = CDFlib (SELECT_, Va.item, (long) (rec_num-1), indicesT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     break;
	   }
	   /*******************************************************************
	   * Unknown item - hopefully the next function.
	   *******************************************************************/
	   default: {
	     Va.fnc = Va.item;
	     break;
	   }
	 }
	 if (Va.fnc == Va.item) break;
      }
      break;

    /**************************************************************************
    * CONFIRM_
    **************************************************************************/
    case CONFIRM_:
      for (;;) {
	 Va.item = (long) *(va_arg (Va.ap, Int32 *));
	 switch (Va.item) {
	   /*******************************************************************
	   * CONFIRM_,<no arguments>
	   *******************************************************************/
	   case CURgENTRY_EXISTENCE_:
	   case CURrENTRY_EXISTENCE_:
	   case CURzENTRY_EXISTENCE_: 
	   case CDF_CHECKSUM_: {
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (CONFIRM_, Va.item,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     break;
	   }
	   /*******************************************************************
	   * CONFIRM_,<Int32 * [output]>
	   *******************************************************************/
	   case CDF_READONLY_MODE_:
	   case CDF_zMODE_:
	   case CDF_NEGtoPOSfp0_MODE_:
	   case CDF_DECODING_:
	   case CDF_CACHESIZE_:
	   case STAGE_CACHESIZE_:
	   case COMPRESS_CACHESIZE_:
	   case rVAR_CACHESIZE_:
	   case zVAR_CACHESIZE_:
	   case rVARs_RECCOUNT_:
	   case zVAR_RECCOUNT_:
	   case rVARs_RECINTERVAL_:
	   case zVAR_RECINTERVAL_:
	   case rVAR_RESERVEPERCENT_:
	   case zVAR_RESERVEPERCENT_: {
	     Int32 *ptr = va_arg (Va.ap, Int32 *);
	     long valueT;
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (CONFIRM_, Va.item, &valueT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     *ptr = (Int32) valueT;
	     break;
	   }
	   /*******************************************************************
	   * CONFIRM_,<(Int32 *) + 1 [output]>
	   *******************************************************************/
	   case rVAR_:
	   case zVAR_:
	   case rVARs_RECNUMBER_:
	   case zVAR_RECNUMBER_:
	   case ATTR_:
	   case gENTRY_:
	   case rENTRY_:
	   case zENTRY_: {
	     Int32 *ptr = va_arg (Va.ap, Int32 *);
	     long valueT;
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (CONFIRM_, Va.item, &valueT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     *ptr = (Int32) (valueT + 1);
	     break;
	   }
	   /*******************************************************************
	   * CONFIRM_,<(Int32 []) [output] - CDF_MAX_DIMS maximum>
	   *******************************************************************/
	   case rVARs_DIMCOUNTS_:
	   case zVAR_DIMCOUNTS_:
	   case rVARs_DIMINTERVALS_:
	   case zVAR_DIMINTERVALS_: {
	     Int32 *ptr = va_arg (Va.ap, Int32 *);
	     Logical Z = (Va.item == zVAR_DIMCOUNTS_ ||
			  Va.item == zVAR_DIMINTERVALS_);
	     long numDims, valuesT[CDF_MAX_DIMS];
	     int dimN;

	     if (StatusBAD(pStatus)) break;

	     tStatus = CDFlib (GET_, BOO(Z,zVAR_NUMDIMS_,
					   rVARs_NUMDIMS_), &numDims,
			       CONFIRM_, Va.item, valuesT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;

	     for (dimN = 0; dimN < numDims; dimN++) {
		ptr[dimN] = (Int32) valuesT[dimN];
	     }
	     break;
	   }
	   /*******************************************************************
	   * CONFIRM_,<(Int32 [] + 1) [output] - CDF_MAX_DIMS maximum>
	   *******************************************************************/
	   case rVARs_DIMINDICES_:
	   case zVAR_DIMINDICES_: {
	     Int32 *values = va_arg (Va.ap, Int32 *);
	     Logical Z = (Va.item == zVAR_DIMINDICES_);
	     long numDims, valuesT[CDF_MAX_DIMS];
	     int dimN;

	     if (StatusBAD(pStatus)) break;

	     tStatus = CDFlib (GET_, BOO(Z,zVAR_NUMDIMS_,
					   rVARs_NUMDIMS_), &numDims,
			       CONFIRM_, Va.item, valuesT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;

	     for (dimN = 0; dimN < numDims; dimN++) {
		values[dimN] = (Int32) (valuesT[dimN] + 1);
	     }
	     break;
	   }
	   /*******************************************************************
	   * CONFIRM_,<(Int32 *) - 1 [input]>
	   *******************************************************************/
	   case gENTRY_EXISTENCE_:
	   case rENTRY_EXISTENCE_:
	   case zENTRY_EXISTENCE_: {
	     Int32 entryN = *(va_arg (Va.ap, Int32 *));
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (CONFIRM_, Va.item, (long) (entryN - 1),
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     break;
	   }
	   /*******************************************************************
	   * CONFIRM_,<void * (character string) [input]>
	   *******************************************************************/
	   case rVAR_EXISTENCE_:
	   case zVAR_EXISTENCE_:
	   case ATTR_EXISTENCE_: {
	     void *varName = va_arg (Va.ap, void *);
             int LFS = FALSE;
             size_t maxLen;

             if (currentCDFid != NULL) { 
               struct CDFstruct *CDF = (struct CDFstruct *)currentCDFid;
               if (isLFS(CDF)) LFS = TRUE;
             }

#if !defined(Fif_GHOSTLEN)
	     maxLen = PickMaxLen (Va.item, 3,
				  rVAR_EXISTENCE_, (LFS ?
                                        (size_t) CDF_VAR_NAME_LEN256 :
                                        (size_t) CDF_VAR_NAME_LEN),
				  zVAR_EXISTENCE_, (LFS ?
                                        (size_t) CDF_VAR_NAME_LEN256 :
                                        (size_t) CDF_VAR_NAME_LEN),
				  ATTR_EXISTENCE_, (LFS ?
                                        (size_t) CDF_ATTR_NAME_LEN256:
                                        (size_t) CDF_ATTR_NAME_LEN));
#endif

	     if (StatusBAD(pStatus)) break;

	     tStatus = CDFlib (CONFIRM_, Va.item,
#if defined(Fif_DESCR)
						  DESCRtoREFnul(varName,
								maxLen,&ssh),
#endif
#if defined(Fif_GHOSTLEN)
						  NULterminate(varName,sLs[sC],
							       &ssh),
#endif
#if defined(Fif_NOLEN)
						  FindEndNUL(varName,maxLen,
							     &ssh),
#endif
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
#if defined(Fif_GHOSTLEN)
	     sC++;
#endif
	     break;
	   }
	   /*******************************************************************
	   * CONFIRM_,CDF_NAME_
	   *******************************************************************/
	   case CDF_NAME_: {
	     void *name = va_arg (Va.ap, void *);
	     char nameT[CDF_PATHNAME_LEN+1];
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (CONFIRM_, CDF_NAME_, nameT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
#if defined(Fif_GHOSTLEN)
	     CtoFORTstring (nameT, name, sLs[sC]);
	     sC++;
#else
	     CtoFORTstring (nameT, name, CDF_PATHNAME_LEN);
#endif
	     break;
	   }
	   /*******************************************************************
	   * CONFIRM_,rVAR_SEQPOS_/zVAR_SEQPOS_
	   *******************************************************************/
	   case rVAR_SEQPOS_:
	   case zVAR_SEQPOS_: {
	     Int32 *rec_num = va_arg (Va.ap, Int32 *);
	     Int32 *indices = va_arg (Va.ap, Int32 *);
	     Logical Z = (Va.item == zVAR_SEQPOS_);
	     long numDims, indicesT[CDF_MAX_DIMS], recNumT;
	     int dimN;

	     if (StatusBAD(pStatus)) break;

	     tStatus = CDFlib (GET_, BOO(Z,zVAR_NUMDIMS_,
					   rVARs_NUMDIMS_), &numDims,
			       CONFIRM_, Va.item, &recNumT, indicesT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;

	     *rec_num = (Int32) (recNumT + 1);
	     for (dimN = 0; dimN < numDims; dimN++) {
		indices[dimN] = (Int32) (indicesT[dimN] + 1);
	     }
	     break;
	   }
	   /*******************************************************************
	   * Unknown item - hopefully the next function.
	   *******************************************************************/
	   default: {
	     Va.fnc = Va.item;
	     break;
	   }
	 }
	 if (Va.fnc == Va.item) break;
      }
      break;

    /**************************************************************************
    * GET_
    **************************************************************************/
    case GET_:
      for (;;) {
	 Va.item = (long) *(va_arg (Va.ap, Int32 *));
	 switch (Va.item) {
	   /*******************************************************************
	   * GET_,<Int32 * (passed by reference)>.
	   *******************************************************************/
	   case rVARs_NUMDIMS_:
	   case zVAR_NUMDIMS_:
	   case CDF_ENCODING_:
	   case CDF_MAJORITY_:
	   case CDF_FORMAT_:
	   case CDF_LEAPSECONDLASTUPDATED_:
	   case CDF_NUMrVARS_:
	   case CDF_NUMzVARS_:
	   case CDF_NUMATTRS_:
	   case CDF_NUMgATTRS_:
	   case CDF_NUMvATTRS_:
	   case CDF_VERSION_:
	   case CDF_RELEASE_:
	   case CDF_INCREMENT_:
	   case rVAR_DATATYPE_:
	   case zVAR_DATATYPE_:
	   case rVAR_NUMELEMS_:
	   case zVAR_NUMELEMS_:
	   case rVAR_RECVARY_:
	   case zVAR_RECVARY_:
	   case rVAR_BLOCKINGFACTOR_:
	   case zVAR_BLOCKINGFACTOR_:
	   case rVAR_nINDEXRECORDS_:
	   case zVAR_nINDEXRECORDS_:
	   case rVAR_nINDEXENTRIES_:
	   case zVAR_nINDEXENTRIES_:
	   case rVAR_nINDEXLEVELS_:
	   case zVAR_nINDEXLEVELS_:
	   case rVAR_NUMRECS_:
	   case zVAR_NUMRECS_:
	   case rVAR_NUMallocRECS_:
	   case zVAR_NUMallocRECS_:
	   case rVAR_SPARSERECORDS_:
	   case zVAR_SPARSERECORDS_:
	   case ATTR_SCOPE_:
	   case ATTR_NUMgENTRIES_:
	   case ATTR_NUMrENTRIES_:
	   case ATTR_NUMzENTRIES_:
	   case gENTRY_DATATYPE_:
	   case rENTRY_DATATYPE_:
	   case zENTRY_DATATYPE_:
	   case gENTRY_NUMELEMS_:
	   case rENTRY_NUMELEMS_:
	   case zENTRY_NUMELEMS_:
	   case LIB_VERSION_:
	   case LIB_RELEASE_:
	   case LIB_INCREMENT_: 
	   case CDF_CHECKSUM_: {
	     Int32 *ptr = va_arg (Va.ap, Int32 *);
	     long valueT;
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (GET_, Va.item, &valueT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     *ptr = (Int32) valueT;
	     break;
	   }
	   /*******************************************************************
	   * GET_,<Int32 * In, Int32 * Out>.
	   *******************************************************************/
	   case DATATYPE_SIZE_: {
	     Int32 *ptr1 = va_arg (Va.ap, Int32 *);
	     Int32 *ptr2 = va_arg (Va.ap, Int32 *);
	     long valueT;
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (GET_, Va.item, (long) *ptr1, &valueT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     *ptr2 = (Int32) valueT;
	     break;
	   }
	   /*******************************************************************
	   * GET_,<(Int32 * In) - 1, (Int32 * Out) + 1>.
	   *******************************************************************/
	   case rVAR_ALLOCATEDTO_:
	   case zVAR_ALLOCATEDTO_:
	   case rVAR_ALLOCATEDFROM_:
	   case zVAR_ALLOCATEDFROM_: {
	     Int32 *ptr1 = va_arg (Va.ap, Int32 *);
	     Int32 *ptr2 = va_arg (Va.ap, Int32 *);
	     long valueT;
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (GET_, Va.item, (long) (*ptr1 - 1), &valueT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     *ptr2 = (Int32) (valueT + 1);
	     break;
	   }
	   /*******************************************************************
	   * GET_,<(Int32 *) + 1 (passed by reference)>.
	   *******************************************************************/
	   case rVARs_MAXREC_:
	   case zVARs_MAXREC_:
	   case rVAR_MAXREC_:
	   case zVAR_MAXREC_:
	   case rVAR_MAXallocREC_:
	   case zVAR_MAXallocREC_:
	   case ATTR_MAXgENTRY_:
	   case ATTR_MAXrENTRY_:
	   case ATTR_MAXzENTRY_: {
	     Int32 *ptr = va_arg (Va.ap, Int32 *);
	     long valueT;
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (GET_, Va.item, &valueT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     *ptr = (Int32) (valueT + 1);
	     break;
	   }
	   /*******************************************************************
	   * GET_,<Int32 [] (passed by reference) - CDF_MAX_DIMS maximum>
	   *******************************************************************/
	   case rVARs_DIMSIZES_:
	   case zVAR_DIMSIZES_:
	   case rVAR_DIMVARYS_:
	   case zVAR_DIMVARYS_: {
	     Int32 *ptr = va_arg (Va.ap, Int32 *);
	     Logical Z = (Va.item == zVAR_DIMSIZES_ ||
			  Va.item == zVAR_DIMVARYS_);
	     long valuesT[CDF_MAX_DIMS], numDims;
	     int dimN;
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (GET_, BOO(Z,zVAR_NUMDIMS_,
					   rVARs_NUMDIMS_), &numDims,
				     Va.item, valuesT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     for (dimN = 0; dimN < numDims; dimN++) {
		ptr[dimN] = (Int32) valuesT[dimN];
	     }
	     break;
	   }
	   /*******************************************************************
	   * GET_,<void * (character string)>.
	   *******************************************************************/
	   case LIB_subINCREMENT_:
	   case LIB_COPYRIGHT_:
	   case CDF_COPYRIGHT_:
	   case rVAR_NAME_:
	   case zVAR_NAME_:
	   case ATTR_NAME_: {
	     void *ptr = va_arg (Va.ap, void *);
	     char *ptrT;
             int LFS = FALSE;
             size_t maxLen;

             if (currentCDFid != NULL) { 
               struct CDFstruct *CDF = (struct CDFstruct *)currentCDFid;
               if (isLFS(CDF)) LFS = TRUE;
             }

	     if (StatusBAD(pStatus)) break;
	     maxLen = PickMaxLen (Va.item, 6,
				  LIB_subINCREMENT_, (size_t) 1,
				  LIB_COPYRIGHT_, (size_t) CDF_DOCUMENT_LEN,
				  CDF_COPYRIGHT_, (size_t) CDF_DOCUMENT_LEN,
				  rVAR_NAME_, (LFS ?
                                               (size_t) CDF_VAR_NAME_LEN256 :
                                               (size_t) CDF_VAR_NAME_LEN),
				  zVAR_NAME_, (LFS ?
                                               (size_t) CDF_VAR_NAME_LEN256 :
                                               (size_t) CDF_VAR_NAME_LEN),
				  ATTR_NAME_, (LFS ?
                                               (size_t) CDF_ATTR_NAME_LEN256 :
                                               (size_t) CDF_ATTR_NAME_LEN));
	     ptrT = (char *) cdf_AllocateMemory (maxLen + 1, NULL);
	     if (ptrT == NULL) {
	       if (!sX(BAD_MALLOC,&pStatus)) break;
	     }
	     tStatus = CDFlib (GET_, Va.item, ptrT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) {
	       cdf_FreeMemory (ptrT, NULL);
	       break;
	     }
#if defined(Fif_GHOSTLEN)
	     CtoFORTstring (ptrT, ptr, sLs[sC]);
	     sC++;
#else
	     CtoFORTstring (ptrT, ptr, (int) maxLen);
#endif
	     cdf_FreeMemory (ptrT, NULL);
	     break;
	   }
	   /*******************************************************************
	   * GET_,<in: void * (character string), out: (Int32 *) + 1>
	   *******************************************************************/
	   case rVAR_NUMBER_:
	   case zVAR_NUMBER_:
	   case ATTR_NUMBER_: {
	     void *ptr1 = va_arg (Va.ap, void *);
	     Int32 *ptr2 = va_arg (Va.ap, Int32 *);
	     long valueT;
             int LFS = FALSE;
             size_t maxLen;

             if (currentCDFid != NULL) { 
               struct CDFstruct *CDF = (struct CDFstruct *)currentCDFid;
               if (isLFS(CDF)) LFS = TRUE;
             }

#if !defined(Fif_GHOSTLEN)
	     maxLen = PickMaxLen (Va.item, 3,
				  rVAR_NUMBER_, (LFS ?
                                       (size_t) CDF_VAR_NAME_LEN256 :
                                       (size_t) CDF_VAR_NAME_LEN),
				  zVAR_NUMBER_, (LFS ?
                                       (size_t) CDF_VAR_NAME_LEN256 :
                                       (size_t) CDF_VAR_NAME_LEN),
				  ATTR_NUMBER_, (LFS ?
                                       (size_t) CDF_ATTR_NAME_LEN256 :
                                       (size_t) CDF_ATTR_NAME_LEN));
#endif

	     if (StatusBAD(pStatus)) break;

	     tStatus = CDFlib (GET_, Va.item,
#if defined(Fif_DESCR)
					      DESCRtoREFnul(ptr1,maxLen,&ssh),
#endif
#if defined(Fif_GHOSTLEN)
					      NULterminate(ptr1,sLs[sC],&ssh),
#endif
#if defined(Fif_NOLEN)
					      FindEndNUL(ptr1,maxLen,&ssh),
#endif
					      &valueT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;

#if defined(Fif_GHOSTLEN)
	     sC++;
#endif
	     *ptr2 = (Int32) (valueT + 1);
	     break;
	   }
	   /*******************************************************************
	   * GET_,CDF_INFO_:
	   *******************************************************************/
	   case CDF_INFO_: {
	     void *ptr1 = va_arg (Va.ap, void *);
	     Int32 *ptr2 = va_arg (Va.ap, Int32 *);
	     Int32 *ptr3 = va_arg (Va.ap, Int32 *);
	     void *ptr4 = va_arg (Va.ap, void *);
	     void *ptr5 = va_arg (Va.ap, void *);
	     long cType, cParms[CDF_MAX_PARMS]; /* cSize, uSize;*/ int p;
	     void *cSize, *uSize;
             int LFS = FALSE;
             if (currentCDFid != NULL) { 
               struct CDFstruct *CDF = (struct CDFstruct *)currentCDFid;
               if (isLFS(CDF)) LFS = TRUE;
             }
	     if (LFS) {
		cSize = (OFF_T *) cdf_AllocateMemory(sizeof(OFF_T), NULL);
		uSize = (OFF_T *) cdf_AllocateMemory(sizeof(OFF_T), NULL);
	     } else {
		cSize = (long *) cdf_AllocateMemory(sizeof(long), NULL);
		uSize = (long *) cdf_AllocateMemory(sizeof(long), NULL);
	     }
	     tStatus = CDFlib (GET_, Va.item,
#if defined(Fif_DESCR)
					      DESCRtoREFnul(ptr1,
							    CDF_PATHNAME_LEN,
							    &ssh),
#endif
#if defined(Fif_GHOSTLEN)
					      NULterminate(ptr1,sLs[sC],&ssh),
#endif
#if defined(Fif_NOLEN)
					      FindEndNUL(ptr1,
							 CDF_PATHNAME_LEN,
							 &ssh),
#endif
					      &cType, cParms, cSize, uSize,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
#if defined(Fif_GHOSTLEN)
	     sC++;
#endif
	     *ptr2 = (Int32) cType;
	     for (p = 0; p < CompressionParmsCount((Int32)cType); p++) {
		ptr3[p] = (Int32) cParms[p];
	     }
	     memcpy(ptr4, cSize, sizeof(cSize));
	     memcpy(ptr5, uSize, sizeof(uSize));
	     cdf_FreeMemory (cSize, NULL);
	     cdf_FreeMemory (uSize, NULL);
	     break;
	   }
	   /*******************************************************************
	   * GET_,CDF_COMPRESSION_
	   * GET_,rVAR_COMPRESSION_
	   * GET_,zVAR_COMPRESSION_
	   * GET_,rVAR_SPARSEARRAYS_
	   * GET_,zVAR_SPARSEARRAYS_
	   *******************************************************************/
	   case CDF_COMPRESSION_:
	   case rVAR_COMPRESSION_:
	   case zVAR_COMPRESSION_:
	   case rVAR_SPARSEARRAYS_:
	   case zVAR_SPARSEARRAYS_: {
	     Logical sparse = (Va.item == rVAR_SPARSEARRAYS_ ||
			       Va.item == zVAR_SPARSEARRAYS_);
	     Int32 *ptr1 = va_arg (Va.ap, Int32 *);
	     Int32 *ptr2 = va_arg (Va.ap, Int32 *);
	     Int32 *ptr3 = va_arg (Va.ap, Int32 *);
	     long type, parms[CDF_MAX_PARMS], pct; int pCount, p;
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (GET_, Va.item, &type, parms, &pct,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     *ptr1 = (Int32) type;
	     pCount = BOO(sparse,SparsenessParmsCount((Int32)type),
				 CompressionParmsCount((Int32)type));
	     for (p = 0; p < pCount; p++) ptr2[p] = (Int32) parms[p];
	     *ptr3 = (Int32) pct;
	     break;
	   }
	   /*******************************************************************
	   * GET_,<void * (depends on data type) - variable value(s)>
	   *******************************************************************/
	   case rVAR_PADVALUE_:
	   case zVAR_PADVALUE_:
	   case rVAR_DATA_:
	   case zVAR_DATA_:
	   case rVAR_HYPERDATA_:
	   case zVAR_HYPERDATA_:
	   case rVAR_SEQDATA_:
	   case zVAR_SEQDATA_: {
	     void *ptr = va_arg (Va.ap, void *);
	     Logical Z = (Va.item == zVAR_PADVALUE_ ||
			  Va.item == zVAR_DATA_ ||
			  Va.item == zVAR_HYPERDATA_ ||
			  Va.item == zVAR_SEQDATA_);
	     long dataType;

	     if (StatusBAD(pStatus)) break;

	     tStatus = CDFlib (GET_, BOO(Z,zVAR_DATATYPE_,
					   rVAR_DATATYPE_), &dataType,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;

	     tStatus = CDFlib (GET_, Va.item,
#if defined(Fif_DESCR)
					      STRINGdataType(dataType) ?
					      DESCRtoREF(ptr) : ptr,
#else
					      ptr,
#endif
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;

#if defined(Fif_GHOSTLEN)
	     if (STRINGdataType(dataType)) sC++;
#endif
	     break;
	   }
	   /*******************************************************************
	   * GET_,r/zVARs_RECDATA_
	   *******************************************************************/
	   case rVARs_RECDATA_:
	   case zVARs_RECDATA_: {
	     Int32 *nVars = va_arg (Va.ap, Int32 *);
	     Int32 *varNs = va_arg (Va.ap, Int32 *);
	     void *ptr = va_arg (Va.ap, void *);
	     long *tVarNs; int varX;
	     if (StatusBAD(pStatus)) break;
	     if (*nVars < 1) {
	       if (!sX(BAD_NUM_VARS,&pStatus)) break;
	     }
	     tVarNs = (long *) cdf_AllocateMemory ((size_t)(*nVars*sizeof(long)),
					       NULL);
	     if (tVarNs == NULL) {
	       if (!sX(BAD_MALLOC,&pStatus)) break;
	     }
	     for (varX = 0; varX < *nVars; varX++) {
		tVarNs[varX] = (long) (varNs[varX] - 1);
	     }
	     tStatus = CDFlib (GET_, Va.item, (long) *nVars, tVarNs, ptr,
			       NULL_);
	     cdf_FreeMemory (tVarNs, NULL);		/*Before checking status.*/
	     if (!sX(tStatus,&pStatus)) break;
	     break;
	   }
	   /*******************************************************************
	   * GET_,gENTRY_DATA_/rENTRY_DATA_/zENTRY_DATA_
	   *******************************************************************/
	   case gENTRY_DATA_:
	   case rENTRY_DATA_:
	   case zENTRY_DATA_: {
	     void *value = va_arg (Va.ap, void *);
 	     int Et = E3p(Va.item,gENTRY_DATA_,rENTRY_DATA_,zENTRY_DATA_);
	     long dataType;

	     if (StatusBAD(pStatus)) break;

	     tStatus = CDFlib (GET_, E3(Et,gENTRY_DATATYPE_,
					   rENTRY_DATATYPE_,
					   zENTRY_DATATYPE_), &dataType,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;

	     tStatus = CDFlib (GET_, Va.item,
#if defined(Fif_DESCR)
					      STRINGdataType(dataType) ?
					      DESCRtoREF(value) : value,
#else
					    value,
#endif
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;

#if defined(Fif_GHOSTLEN)
	     if (STRINGdataType(dataType)) sC++;
#endif
	     break;
	   }
	   /*******************************************************************
	   * GET_,STATUS_TEXT_
	   *******************************************************************/
	   case STATUS_TEXT_: {
	     void *textPtr = va_arg (Va.ap, void *);
	     char textT[CDF_STATUSTEXT_LEN+1];

	     if (StatusBAD(pStatus)) break;

	     tStatus = CDFlib (GET_, STATUS_TEXT_, textT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;

#if defined(Fif_GHOSTLEN)
	     CtoFORTstring (textT, textPtr, sLs[sC]);
	     sC++;
#else
	     CtoFORTstring (textT, textPtr, CDF_STATUSTEXT_LEN);
#endif
	     break;
	   }
	   /*******************************************************************
	   * Unknown item - hopefully the next function.
	   *******************************************************************/
	   default: {
	     Va.fnc = Va.item;
	     break;
	   }
	 }
	 if (Va.fnc == Va.item) break;
      }
      break;

    /**************************************************************************
    * PUT_
    **************************************************************************/
    case PUT_:
      for (;;) {
	 Va.item = (long) *(va_arg (Va.ap, Int32 *));
	 switch (Va.item) {
	   /*******************************************************************
	   * PUT_,<Int32 *>
	   *******************************************************************/
	   case CDF_ENCODING_:
	   case CDF_MAJORITY_:
	   case CDF_FORMAT_:
	   case CDF_LEAPSECONDLASTUPDATED_:
	   case rVAR_RECVARY_:
	   case zVAR_RECVARY_:
	   case rVAR_ALLOCATERECS_:
	   case zVAR_ALLOCATERECS_:
	   case rVAR_INITIALRECS_:
	   case zVAR_INITIALRECS_:
	   case rVAR_BLOCKINGFACTOR_:
	   case zVAR_BLOCKINGFACTOR_:
	   case rVAR_SPARSERECORDS_:
	   case zVAR_SPARSERECORDS_:
	   case ATTR_SCOPE_: 
	   case CDF_CHECKSUM_: {
	     Int32 *ptr = va_arg (Va.ap, Int32 *);
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (PUT_, Va.item, (long) *ptr,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     break;
	   }
	   /*******************************************************************
	   * PUT_,<Int32 []>
	   *******************************************************************/
	   case rVAR_DIMVARYS_:
	   case zVAR_DIMVARYS_: {
	     Int32 *ptr = va_arg (Va.ap, Int32 *);
	     Logical Z = (Va.item == zVAR_DIMVARYS_);
	     long valuesT[CDF_MAX_DIMS], numDims;
	     int dimN;
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (GET_, BOO(Z,zVAR_NUMDIMS_,
					   rVARs_NUMDIMS_), &numDims,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     for (dimN = 0; dimN < numDims; dimN++) {
		valuesT[dimN] = (long) ptr[dimN];
	     }
	     tStatus = CDFlib (PUT_, Va.item, valuesT,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     break;
	   }
	   /*******************************************************************
	   * PUT_,CDF_COMPRESSION_
	   * PUT_,rVAR_COMPRESSION_
	   * PUT_,zVAR_COMPRESSION_
	   * PUT_,rVAR_SPARSEARRAYS_
	   * PUT_,zVAR_SPARSEARRAYS_
	   *******************************************************************/
	   case CDF_COMPRESSION_:
	   case rVAR_COMPRESSION_:
	   case zVAR_COMPRESSION_:
	   case rVAR_SPARSEARRAYS_:
	   case zVAR_SPARSEARRAYS_: {
	     Logical sparse = (Va.item == rVAR_SPARSEARRAYS_ ||
			       Va.item == zVAR_SPARSEARRAYS_);
	     Int32 *ptr1 = va_arg (Va.ap, Int32 *);
	     Int32 *ptr2 = va_arg (Va.ap, Int32 *);
	     int p, pCount = BOO(sparse,SparsenessParmsCount(*ptr1),
				        CompressionParmsCount(*ptr1));
	     long parms[CDF_MAX_PARMS];
	     if (StatusBAD(pStatus)) break;
	     for (p = 0; p < pCount; p++) parms[p] = (long) ptr2[p];
	     tStatus = CDFlib (PUT_, Va.item, (long) *ptr1, parms,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     break;
	   }
	   /*******************************************************************
	   * PUT_,<Int32 *, Int32 *>
	   *******************************************************************/
	   case rVAR_DATASPEC_:
	   case zVAR_DATASPEC_:
	   case gENTRY_DATASPEC_:
	   case rENTRY_DATASPEC_:
	   case zENTRY_DATASPEC_: {
	     Int32 value1 = *(va_arg (Va.ap, Int32 *));
	     Int32 value2 = *(va_arg (Va.ap, Int32 *));
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (PUT_, Va.item, (long) value1, (long) value2,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     break;
	   }
	   /*******************************************************************
	   * PUT_,<(Int32 *) - 1, (Int32 *) - 1>
	   *******************************************************************/
	   case rVAR_ALLOCATEBLOCK_:
	   case zVAR_ALLOCATEBLOCK_: {
	     Int32 value1 = *(va_arg (Va.ap, Int32 *)) - 1;
	     Int32 value2 = *(va_arg (Va.ap, Int32 *)) - 1;
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (PUT_, Va.item, (long) value1, (long) value2,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     break;
	   }
	   /*******************************************************************
	   * PUT_,<void * (character string)>
	   *******************************************************************/
	   case rVAR_NAME_:
	   case zVAR_NAME_:
	   case ATTR_NAME_: {
	     void *ptr = va_arg (Va.ap, void *);
             int LFS = FALSE;
             size_t maxLen;

             if (currentCDFid != NULL) { 
               struct CDFstruct *CDF = (struct CDFstruct *)currentCDFid;
               if (isLFS(CDF)) LFS = TRUE;
             }

#if !defined(Fif_GHOSTLEN)
	     maxLen = PickMaxLen (Va.item, 3,
				  rVAR_NAME_,(LFS ?
                                          (size_t) CDF_VAR_NAME_LEN256 :
                                          (size_t) CDF_VAR_NAME_LEN),
				  zVAR_NAME_,(LFS?
                                          (size_t) CDF_VAR_NAME_LEN256 :
                                          (size_t) CDF_VAR_NAME_LEN),
				  ATTR_NAME_,(LFS?
                                          (size_t) CDF_ATTR_NAME_LEN256 :
                                          (size_t) CDF_ATTR_NAME_LEN));
#endif
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (PUT_, Va.item,
#if defined(Fif_DESCR)
					      DESCRtoREFnul(ptr,maxLen,&ssh),
#endif
#if defined(Fif_GHOSTLEN)
					      NULterminate(ptr,sLs[sC],&ssh),
#endif
#if defined(Fif_NOLEN)
					      FindEndNUL(ptr,maxLen,&ssh),
#endif
				 NULL_);
	     if (!sX(tStatus,&pStatus)) break;

#if defined(Fif_GHOSTLEN)
	     sC++;
#endif
	     break;
	   }
	   /*******************************************************************
	   * PUT_,<void * (depends on data type) - variable value(s)>
	   *******************************************************************/
	   case rVAR_PADVALUE_:
	   case zVAR_PADVALUE_:
	   case rVAR_DATA_:
	   case zVAR_DATA_:
	   case rVAR_HYPERDATA_:
	   case zVAR_HYPERDATA_:
	   case rVAR_SEQDATA_:
	   case zVAR_SEQDATA_: {
	     void *ptr = va_arg (Va.ap, void *);
	     Logical Z = (Va.item == zVAR_PADVALUE_ ||
			  Va.item == zVAR_DATA_ ||
			  Va.item == zVAR_HYPERDATA_ ||
			  Va.item == zVAR_SEQDATA_);
	     long dataType;
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (GET_, BOO(Z,zVAR_DATATYPE_,
					   rVAR_DATATYPE_), &dataType,
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
	     tStatus = CDFlib (PUT_, Va.item,
#if defined(Fif_DESCR)
					      STRINGdataType(dataType) ?
					      DESCRtoREF(ptr) : ptr,
#else
					      ptr,
#endif
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
#if defined(Fif_GHOSTLEN)
	     if (STRINGdataType(dataType)) sC++;
#endif
	     break;
	   }
	   /*******************************************************************
	   * PUT_,r/zVARs_RECDATA_
	   *******************************************************************/
	   case rVARs_RECDATA_:
	   case zVARs_RECDATA_: {
	     Int32 *nVars = va_arg (Va.ap, Int32 *);
	     Int32 *varNs = va_arg (Va.ap, Int32 *);
	     void *ptr = va_arg (Va.ap, void *);
	     long *tVarNs; int varX;
	     if (StatusBAD(pStatus)) break;
	     if (*nVars < 1) {
	       if (!sX(BAD_NUM_VARS,&pStatus)) break;
	     }
	     tVarNs = (long *) cdf_AllocateMemory ((size_t)(*nVars*sizeof(long)),
					       NULL);
	     if (tVarNs == NULL) {
	       if (!sX(BAD_MALLOC,&pStatus)) break;
	     }
	     for (varX = 0; varX < *nVars; varX++) {
		tVarNs[varX] = (long) (varNs[varX] - 1);
	     }
	     tStatus = CDFlib (PUT_, Va.item, (long) *nVars, tVarNs, ptr,
			       NULL_);
	     cdf_FreeMemory (tVarNs, NULL);		/*Before checking status.*/
	     if (!sX(tStatus,&pStatus)) break;
	     break;
	   }
	   /*******************************************************************
	   * PUT_,gENTRY_DATA_/rENTRY_DATA_/zENTRY_DATA_
	   *******************************************************************/
	   case gENTRY_DATA_:
	   case rENTRY_DATA_:
	   case zENTRY_DATA_: {
	     Int32 dataType = *(va_arg (Va.ap, Int32 *));
	     Int32 numElements = *(va_arg (Va.ap, Int32 *));
	     void *value = va_arg (Va.ap, void *);
	     if (StatusBAD(pStatus)) break;
	     tStatus = CDFlib (PUT_, Va.item, (long) dataType,
					      (long) numElements,
#if defined(Fif_DESCR)
					      STRINGdataType(dataType) ?
					      DESCRtoREF(value) : value,
#else
					      value,
#endif
			       NULL_);
	     if (!sX(tStatus,&pStatus)) break;
#if defined(Fif_GHOSTLEN)
	     if (STRINGdataType(dataType)) sC++;
#endif
	     break;
	   }
	   /*******************************************************************
	   * Unknown item - hopefully the next function.
	   *******************************************************************/
	   default: {
	     Va.fnc = Va.item;
	     break;
	   }
	 }
	 if (Va.fnc == Va.item) break;
      }
      break;
    /**************************************************************************
    * Unknown function/item.  This is bad - we're lost.
    **************************************************************************/
    default: {
#if defined(Fif_GHOSTLEN)
      if (sLs != NULL) cdf_FreeMemory (sLs, NULL);
#endif
      FreeStrings (ssh);
      va_end (Va.ap);
      return ((Int32) BAD_FNC_OR_ITEM);
    }
  }

#if defined(Fif_GHOSTLEN)
if (sLs != NULL) cdf_FreeMemory (sLs, NULL);
#endif

status = va_arg (Va.ap, Int32 *);
*status = (Int32) pStatus;

va_end (Va.ap);

FreeStrings (ssh);
return ((Int32) pStatus);
}

/******************************************************************************
* PickMaxLen.
*   Syntax: len = PickMaxLen (targetItem, nListed,
*			      listItem1, listLen1,
*			      listItem2, listLen2,
*			      .
*			      .
*			      .
*			      listItemN, listLenN);
******************************************************************************/

static size_t PickMaxLen
#if defined(STDARG)
(long requiredArgument, ...)
#else
(va_alist)
va_dcl
#endif
{
  va_list ap;
  long targetItem;		/* The item being sought. */
  int nListed;			/* The number of listed items (the list
				   being searched for the target item). */
  int listN;			/* Index into list of item/length pairs. */
#if defined(STDARG)
  va_start (ap, requiredArgument);
  targetItem = requiredArgument;
#else
  VA_START (ap);
  targetItem = va_arg (ap, long);
#endif
  nListed = va_arg (ap, int);
  for (listN = 0; listN < nListed; listN++) {
     long listItem = va_arg (ap, long);
     size_t listLen = va_arg (ap, size_t);
     if (listItem == targetItem) {
       va_end (ap);
       return listLen;
     }
  }
  va_end (ap);
  return (size_t) 0;
}

/******************************************************************************
* CDF_lib_4.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_4__,cdf_lib_4_,cdf_lib_4,CDF_LIB_4)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[4]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a3);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, l[0], l[1], l[2], l[3]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[4]; int i; void *a[4];
  va_list ap; CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 4; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], l[0], l[1], l[2], l[3]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3);
#else
  void *a[4]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 4; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_5.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_5__,cdf_lib_5_,cdf_lib_5,CDF_LIB_5)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[5]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a4);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4,
		   l[0], l[1], l[2], l[3], l[4]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[5]; int i; void *a[5];
  va_list ap; CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 5; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4],
		   l[0], l[1], l[2], l[3], l[4]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4);
#else
  void *a[5]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 5; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_6.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_6__,cdf_lib_6_,cdf_lib_6,CDF_LIB_6)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, void *a5, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[6]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4, a5);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a5);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4, a5,
		   l[0], l[1], l[2], l[3], l[4], l[5]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[6]; int i; void *a[6];
  va_list ap; CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 6; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4], a[5]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5],
		   l[0], l[1], l[2], l[3], l[4], l[5]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4, a5);
#else
  void *a[6]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 6; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_7.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_7__,cdf_lib_7_,cdf_lib_7,CDF_LIB_7)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, void *a5, void *a6, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[7]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4, a5, a6);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a6);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6,
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[7]; int i; void *a[7];
  va_list ap; CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 7; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6],
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6);
#else
  void *a[7]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 7; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_8.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_8__,cdf_lib_8_,cdf_lib_8,CDF_LIB_8)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
 void *a7, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[8]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4, a5, a6, a7);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a7);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7,
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[8]; int i; void *a[8];
  va_list ap; CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 8; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7);
#else
  void *a[8]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 8; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_9.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_9__,cdf_lib_9_,cdf_lib_9,CDF_LIB_9)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
 void *a7, void *a8, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[9]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4, a5, a6, a7, a8);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a8);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8,
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[9]; int i; void *a[9];
  va_list ap; CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 9; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
		      a[8]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8],
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8);
#else
  void *a[9]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 9; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_10.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_10__,cdf_lib_10_,cdf_lib_10,CDF_LIB_10)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
 void *a7, void *a8, void *a9, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[10]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a9);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9,
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[10]; int i; void *a[10]; va_list ap;
  CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 10; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
		      a[8], a[9]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9);
#else
  void *a[10]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 10; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_11.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_11__,cdf_lib_11_,cdf_lib_11,CDF_LIB_11)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
 void *a7, void *a8, void *a9, void *a10, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[11]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a10);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10,
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[11]; int i; void *a[11]; va_list ap;
  CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 11; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
		      a[8], a[9], a[10]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10],
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
#else
  void *a[11]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 11; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_12.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_12__,cdf_lib_12_,cdf_lib_12,CDF_LIB_12)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
 void *a7, void *a8, void *a9, void *a10, void *a11, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[12]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a11);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[12]; int i; void *a[12]; va_list ap;
  CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 12; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
		      a[8], a[9], a[10], a[11]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11],
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
#else
  void *a[12]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 12; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_13.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_13__,cdf_lib_13_,cdf_lib_13,CDF_LIB_13)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
 void *a7, void *a8, void *a9, void *a10, void *a11, void *a12, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[13]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
		      a12);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a12);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12,
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[13]; int i; void *a[13]; va_list ap;
  CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 13; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
		      a[8], a[9], a[10], a[11], a[12]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12],
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12);
#else
  void *a[13]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 13; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_14.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_14__,cdf_lib_14_,cdf_lib_14,CDF_LIB_14)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
 void *a7, void *a8, void *a9, void *a10, void *a11, void *a12, void *a13, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[14]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
		      a12, a13);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a13);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[14]; int i; void *a[14]; va_list ap;
  CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 14; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
		      a[8], a[9], a[10], a[11], a[12], a[13]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13],
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13);
#else
  void *a[14]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 14; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_15.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_15__,cdf_lib_15_,cdf_lib_15,CDF_LIB_15)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
 void *a7, void *a8, void *a9, void *a10, void *a11, void *a12, void *a13,
 void *a14, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[15]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
		      a12, a13, a14);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a14);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14,
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[15]; int i; void *a[15]; va_list ap;
  CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 15; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
		      a[8], a[9], a[10], a[11], a[12], a[13], a[14]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14],
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14);
#else
  void *a[15]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 15; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_16.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_16__,cdf_lib_16_,cdf_lib_16,CDF_LIB_16)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
 void *a7, void *a8, void *a9, void *a10, void *a11, void *a12, void *a13,
 void *a14, void *a15, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[16]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
		      a12, a13, a14, a15);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a15);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14, a15,
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14], l[15]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[16]; int i; void *a[16]; va_list ap;
  CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 16; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
		      a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14], a[15],
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14], l[15]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14, a15);
#else
  void *a[16]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 16; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14], a[15]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_17.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_17__,cdf_lib_17_,cdf_lib_17,CDF_LIB_17)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
 void *a7, void *a8, void *a9, void *a10, void *a11, void *a12, void *a13,
 void *a14, void *a15, void *a16, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[17]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
		      a12, a13, a14, a15, a16);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a16);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14, a15, a16,
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14], l[15], l[16]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[17]; int i; void *a[17]; va_list ap;
  CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 17; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
		      a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15],
		      a[16]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14], a[15], a[16],
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14], l[15], l[16]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14, a15, a16);
#else
  void *a[17]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 17; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14], a[15], a[16]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_18.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_18__,cdf_lib_18_,cdf_lib_18,CDF_LIB_18)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
 void *a7, void *a8, void *a9, void *a10, void *a11, void *a12, void *a13,
 void *a14, void *a15, void *a16, void *a17, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[18]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
		      a12, a13, a14, a15, a16, a17);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a17);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14, a15, a16, a17, l[0], l[1], l[2], l[3], l[4], l[5],
		   l[6], l[7], l[8], l[9], l[10], l[11], l[12], l[13], l[14],
		   l[15], l[16], l[17]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[18]; int i; void *a[18]; va_list ap;
  CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 18; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
		      a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15],
		      a[16], a[17]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17],
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14], l[15], l[16], l[17]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14, a15, a16, a17);
#else
  void *a[18]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 18; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_19.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_19__,cdf_lib_19_,cdf_lib_19,CDF_LIB_19)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
 void *a7, void *a8, void *a9, void *a10, void *a11, void *a12, void *a13,
 void *a14, void *a15, void *a16, void *a17, void *a18, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[19]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
		      a12, a13, a14, a15, a16, a17, a18);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a18);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14, a15, a16, a17, a18,
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14], l[15], l[16], l[17],
		   l[18]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[19]; int i; void *a[19]; va_list ap;
  CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 19; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
		      a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15],
		      a[16], a[17], a[18]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17],
		   a[18],
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14], l[15], l[16], l[17],
		   l[18]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14, a15, a16, a17, a18);
#else
  void *a[19]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 19; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17],
		   a[18]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_20.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_20__,cdf_lib_20_,cdf_lib_20,CDF_LIB_20)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
 void *a7, void *a8, void *a9, void *a10, void *a11, void *a12, void *a13,
 void *a14, void *a15, void *a16, void *a17, void *a18, void *a19, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[20]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
		      a12, a13, a14, a15, a16, a17, a18, a19);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a19);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14, a15, a16, a17, a18, a19,
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14], l[15], l[16], l[17],
		   l[18], l[19]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[20]; int i; void *a[20]; va_list ap;
  CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 20; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
		      a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15],
		      a[16], a[17], a[18], a[19]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17],
		   a[18], a[19],
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14], l[15], l[16], l[17],
		   l[18], l[19]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14, a15, a16, a17, a18, a19);
#else
  void *a[20]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 20; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17],
		   a[18], a[19]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_21.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_21__,cdf_lib_21_,cdf_lib_21,CDF_LIB_21)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
 void *a7, void *a8, void *a9, void *a10, void *a11, void *a12, void *a13,
 void *a14, void *a15, void *a16, void *a17, void *a18, void *a19, void *a20,
 ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[21]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
		      a12, a13, a14, a15, a16, a17, a18, a19, a20);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a20);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14, a15, a16, a17, a18, a19, a20,
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14], l[15], l[16], l[17],
		   l[18], l[19], l[20]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[21]; int i; void *a[21]; va_list ap;
  CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 21; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
		      a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15],
		      a[16], a[17], a[18], a[19], a[20]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17],
		   a[18], a[19], a[20],
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14], l[15], l[16], l[17],
		   l[18], l[19], l[20]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14, a15, a16, a17, a18, a19, a20);
#else
  void *a[21]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 21; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17],
		   a[18], a[19], a[20]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_22.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_22__,cdf_lib_22_,cdf_lib_22,CDF_LIB_22)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
 void *a7, void *a8, void *a9, void *a10, void *a11, void *a12, void *a13,
 void *a14, void *a15, void *a16, void *a17, void *a18, void *a19, void *a20,
 void *a21, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[22]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
		      a12, a13, a14, a15, a16, a17, a18, a19, a20, a21);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a21);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14, a15, a16, a17, a18, a19, a20, a21,
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14], l[15], l[16], l[17],
		   l[18], l[19], l[20], l[21]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[22]; int i; void *a[22]; va_list ap;
  CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 22; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
		      a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15],
		      a[16], a[17], a[18], a[19], a[20], a[21]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17],
		   a[18], a[19], a[20], a[21],
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14], l[15], l[16], l[17],
		   l[18], l[19], l[20], l[21]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14, a15, a16, a17, a18, a19, a20, a21);
#else
  void *a[22]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 22; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17],
		   a[18], a[19], a[20], a[21]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_23.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_23__,cdf_lib_23_,cdf_lib_23,CDF_LIB_23)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
 void *a7, void *a8, void *a9, void *a10, void *a11, void *a12, void *a13,
 void *a14, void *a15, void *a16, void *a17, void *a18, void *a19, void *a20,
 void *a21, void *a22, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[23]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
		      a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a22);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14, a15, a16, a17, a18, a19, a20, a21, a22,
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14], l[15], l[16], l[17],
		   l[18], l[19], l[20], l[21], l[22]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[23]; int i; void *a[23]; va_list ap;
  CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 23; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
		      a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15],
		      a[16], a[17], a[18], a[19], a[20], a[21], a[22]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17],
		   a[18], a[19], a[20], a[21], a[22],
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14], l[15], l[16], l[17],
		   l[18], l[19], l[20], l[21], l[22]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14, a15, a16, a17, a18, a19, a20, a21, a22);
#else
  void *a[23]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 23; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17],
		   a[18], a[19], a[20], a[21], a[22]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_24.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_24__,cdf_lib_24_,cdf_lib_24,CDF_LIB_24)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
 void *a7, void *a8, void *a9, void *a10, void *a11, void *a12, void *a13,
 void *a14, void *a15, void *a16, void *a17, void *a18, void *a19, void *a20,
 void *a21, void *a22, void *a23, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[24]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
		      a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22,
		      a23);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a23);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14, a15, a16, a17, a18, a19, a20, a21, a22, a23,
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14], l[15], l[16], l[17],
		   l[18], l[19], l[20], l[21], l[22], l[23]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[24]; int i; void *a[24]; va_list ap;
  CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 24; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
		      a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15],
		      a[16], a[17], a[18], a[19], a[20], a[21], a[22], a[23]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17],
		   a[18], a[19], a[20], a[21], a[22], a[23],
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14], l[15], l[16], l[17],
		   l[18], l[19], l[20], l[21], l[22], l[23]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14, a15, a16, a17, a18, a19, a20, a21, a22, a23);
#else
  void *a[24]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 24; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17],
		   a[18], a[19], a[20], a[21], a[22], a[23]);
#endif
#endif
}

/******************************************************************************
* CDF_lib_25.
******************************************************************************/

VISIBLE_PREFIX
Fif_PREFIXa
Int32
Fif_PREFIXb
Fif_ENTRYPOINT(cdf_lib_25__,cdf_lib_25_,cdf_lib_25,CDF_LIB_25)
#if defined(STDARG)
(void *a0, void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
 void *a7, void *a8, void *a9, void *a10, void *a11, void *a12, void *a13,
 void *a14, void *a15, void *a16, void *a17, void *a18, void *a19, void *a20,
 void *a21, void *a22, void *a23, void *a24, ...)
#else
(va_alist)
va_dcl
#endif
{
#if defined(Fif_GHOSTLEN)
#if defined(STDARG)
  Int32 sC = 0; Fif_GHOSTTYPE l[25]; int i; va_list ap; CDFstatus tStatus;
  tStatus = CDF_LIBx (&sC, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
		      a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22,
		      a23, a24);
  if (StatusBAD(tStatus)) return tStatus;
  va_start (ap, a24);
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24,
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14], l[15], l[16], l[17],
		   l[18], l[19], l[20], l[21], l[22], l[23], l[24]);
#else
  Int32 sC = 0; Fif_GHOSTTYPE l[25]; int i; void *a[25]; va_list ap;
  CDFstatus tStatus;
  VA_START (ap);
  for (i = 0; i < 25; i++) a[i] = va_arg (ap, void *);
  tStatus = CDF_LIBx (&sC, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7],
		      a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15],
		      a[16], a[17], a[18], a[19], a[20], a[21], a[22], a[23],
		      a[24]);
  if (StatusBAD(tStatus)) return tStatus;
  for (i = 0; i < sC; i++) l[i] = va_arg (ap, Fif_GHOSTTYPE);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17],
		   a[18], a[19], a[20], a[21], a[22], a[23], a[24],
		   l[0], l[1], l[2], l[3], l[4], l[5], l[6], l[7], l[8], l[9],
		   l[10], l[11], l[12], l[13], l[14], l[15], l[16], l[17],
		   l[18], l[19], l[20], l[21], l[22], l[23], l[24]);
#endif
#else
#if defined(STDARG)
  return CDF_LIBx (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13,
		   a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24);
#else
  void *a[25]; int i; va_list ap;
  VA_START (ap);
  for (i = 0; i < 25; i++) a[i] = va_arg (ap, void *);
  va_end (ap);
  return CDF_LIBx (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
		   a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17],
		   a[18], a[19], a[20], a[21], a[22], a[23], a[24]);
#endif
#endif
}
