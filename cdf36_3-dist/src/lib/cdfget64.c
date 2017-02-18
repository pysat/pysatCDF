/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                       CDF `get' operations.
*
*  Version 1.4, 9-Sep-96, Hughes STX.
*
*  Modification history:
*
*   V1.0  20-May-92, J Love     Original version (was part of `cdflib.c').
*   V1.1  16-Sep-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V1.2  13-Dec-93, J Love     CDF V2.4.
*   V1.3  15-Dec-94, J Love     CDF V2.5.
*   V1.3a  9-Jan-95, J Love     Encode/decode changes.  More cache-residency.
*   V1.3b 24-Feb-95, J Love     Solaris 2.3 IDL i/f.
*   V1.3c  4-Aug-95, J Love     CDFexport-related changes.
*   V1.4   9-Sep-96, J Love     CDF V2.6.
*   V2.0  08-Apr-04, M Liu      Removed call to LocateCurrentVar function as
*                               its offset becomes avilable when it is
*                               selected.
*   V2.1  21-Jun-04, M Liu      Modified the error message for NOT_A_CDF.
*   V2.2  29-Jun-04, M Liu      Added LFS (Large File Support > 2G).
*   V3.2  16-Oct-07, D Han      The length of the pad value for a CDF_CHAR
*                               variable is the length of the variable (i.e.
*                               CHAR/162, etc.).  If the length of the pad
*                               value is greater than 16, it causes
*                               segmentation fault for the IDL CDF interface
*                               routine.
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"
#include "cdfrev.h"

/******************************************************************************
* CDFget64.
******************************************************************************/

STATICforIDL CDFstatus CDFget64 (Va, Cur)
struct VAstruct *Va;
struct CurStruct *Cur;
{
CDFstatus tStatus, pStatus = CDF_OK;

switch (Va->item) {
  /****************************************************************************
  * CDF_INFO_
  ****************************************************************************/
  case CDF_INFO_: {
    char CDFpathX[DU_MAX_PATH_LEN+1], CDFpathT[CDF_PATHNAME_LEN+1], *CDFpathP;
    vFILE *dotFp; Logical upper_case_ext, version_numbers, no_append;
    Int32 magicNumber1, magicNumber2; int i;
    long *cType, *cParms; 
    void *cFileSize, *uFileSize;
    long cSize2, uSize2;
    OFF_T cSize3, uSize3;
    int whichVersion = 3;
    CDFpathP = va_arg (Va->ap, char *);
    cType = va_arg (Va->ap, long *);
    cParms = va_arg (Va->ap, long *);
    cFileSize = va_arg (Va->ap, void *);
    uFileSize = va_arg (Va->ap, void *);
    if (strlen(CDFpathP) > (size_t) CDF_PATHNAME_LEN) {
      if (!sX(CDF_NAME_TRUNC,&pStatus)) return pStatus;
    }
    strcpyX (CDFpathT, CDFpathP, CDF_PATHNAME_LEN);
#if STRIP_TRAILING_BLANKS_FROM_CDFPATH
    StripTrailingBlanks (CDFpathT);
#endif
#if defined(vms) || defined(dos)
    MakeUpperString (CDFpathT);
#endif
/*  if (!ValidCDFname(CDFpathT)) return BAD_CDF_NAME; */
    if (!sX(FindCDF(CDFpathT,&no_append,
		    &upper_case_ext,
		    &version_numbers),&pStatus)) return pStatus;
    BuildFilePath (CDFt, CDFpathT, no_append, upper_case_ext, version_numbers,
		   INT32_ZERO, CDFpathX);
    dotFp = V_open64 (CDFpathX, READ_ONLY_a_mode);
    if (dotFp == NULL) return CDF_OPEN_ERROR;
    if (!Read32_64(dotFp,&magicNumber1)) {
      V_close64 (dotFp, NULL, NULL);
      return CDF_READ_ERROR;
    }
    if (!Read32_64(dotFp,&magicNumber2)) {
      V_close64 (dotFp, NULL, NULL);
      return CDF_READ_ERROR;
    }
    if (magicNumber1 != V3magicNUMBER_1) { /* Not V3.0. */
      V_close64 (dotFp, NULL, NULL); /* Close it and then open it with V2.0. */
      dotFp = V_open (CDFpathX, READ_ONLY_a_mode);
      if (dotFp == NULL) return CDF_OPEN_ERROR;
      if (!Read32(dotFp,&magicNumber1)) {
        V_close (dotFp, NULL, NULL);
        return CDF_READ_ERROR;
      }
      if (!Read32(dotFp,&magicNumber2)) {
        V_close (dotFp, NULL, NULL);
        return CDF_READ_ERROR;
      }
      whichVersion = 2;
    }

    switch (magicNumber1) {
      case V1magicNUMBER_flip:
	V_close (dotFp, NULL, NULL);
	return ILLEGAL_ON_V1_CDF;
      case V2magicNUMBER_1pre:
	*cType = NO_COMPRESSION;
	cSize2 = 0;
	if (!SEEKv(dotFp,(OFF_T) 0,vSEEK_END)) {
	  V_close (dotFp, NULL, NULL);
	  return CDF_READ_ERROR;
	}
	uSize2 = V_tell (dotFp);
	V_close (dotFp, NULL, NULL);
	break;
      case V2magicNUMBER_1:
        switch (magicNumber2) {
          case V2magicNUMBER_2u:
            *cType = NO_COMPRESSION;
            cSize2 = 0;
            if (!SEEKv(dotFp,0L,vSEEK_END)) {
              V_close (dotFp, NULL, NULL);
              return CDF_READ_ERROR;
            }
            uSize2 = V_tell (dotFp);
	    V_close (dotFp, NULL, NULL);
            break;
          case V2magicNUMBER_2c: {
            struct CCRstruct CCR; struct CPRstruct CPR;
            if (!sX(ReadCCR(dotFp,V2_CCR_OFFSET,
                            CCR_RECORD,&CCR,
                            CCR_NULL),&pStatus)) {
              V_close (dotFp, NULL, NULL);
              return pStatus;
            }
            if (CCR.uSize == 0) {
              V_close (dotFp, NULL, NULL);
              return EMPTY_COMPRESSED_CDF;
            }
            if (!sX(ReadCPR(dotFp,CCR.CPRoffset,
                            CPR_RECORD,&CPR,
                            CPR_NULL),&pStatus)) {
              V_close (dotFp, NULL, NULL);
              return pStatus;
            }
            *cType = (long) CPR.cType;
            if (CPR.pCount > CDF_MAX_PARMS) {
              V_close (dotFp, NULL, NULL);
              return TOO_MANY_PARMS;
            }
            for (i = 0; i < CPR.pCount; i++) cParms[i] = (long) CPR.cParms[i];
            if (!SEEKv(dotFp,0L,vSEEK_END)) {
              V_close (dotFp, NULL, NULL);
              return CDF_READ_ERROR;
            }
            cSize2 = V_tell (dotFp);
            uSize2 = (long) CCR.uSize + MAGIC_NUMBERS_SIZE;
	    V_close (dotFp, NULL, NULL);
	    break;
          }
        }
	break;
      case V3magicNUMBER_1:
	switch (magicNumber2) {
	  case V3magicNUMBER_2u:
	    *cType = NO_COMPRESSION;
	    cSize3 = 0;
	    if (!SEEKv64(dotFp,(OFF_T) 0,vSEEK_END)) {
	      V_close64 (dotFp, NULL, NULL);
	      return CDF_READ_ERROR;
	    }
	    uSize3 = V_tell64 (dotFp);
	    V_close64 (dotFp, NULL, NULL);
	    break;
	  case V3magicNUMBER_2c: {
	    struct CCRstruct64 CCR; struct CPRstruct64 CPR;
	    if (!sX(ReadCCR64(dotFp,V3_CCR_OFFSET64,
			      CCR_RECORD,&CCR,
			      CCR_NULL),&pStatus)) {
	      V_close64 (dotFp, NULL, NULL);
	      return pStatus;
	    }
	    if (CCR.uSize == 0) {
	      V_close64 (dotFp, NULL, NULL);
	      return EMPTY_COMPRESSED_CDF;
	    }
	    if (!sX(ReadCPR64(dotFp,CCR.CPRoffset,
			      CPR_RECORD,&CPR,
			      CPR_NULL),&pStatus)) {
	      V_close64 (dotFp, NULL, NULL);
	      return pStatus;
	    }
	    *cType = (long) CPR.cType;
	    if (CPR.pCount > CDF_MAX_PARMS) {
	      V_close64 (dotFp, NULL, NULL);
	      return TOO_MANY_PARMS;
	    }
	    for (i = 0; i < CPR.pCount; i++) cParms[i] = (long) CPR.cParms[i];
	    if (!SEEKv64(dotFp,(OFF_T) 0,vSEEK_END)) {
	      V_close64 (dotFp, NULL, NULL);
	      return CDF_READ_ERROR;
	    }
	    cSize3 = V_tell64 (dotFp);
	    uSize3 = (OFF_T) CCR.uSize + MAGIC_NUMBERS_SIZE;
	    V_close64 (dotFp, NULL, NULL);
	    break;
	  }
	  default:
	    V_close64 (dotFp, NULL, NULL);
	    return NOT_A_CDF;
	}
	break;
      default:
	if (whichVersion == 3)
	  V_close64 (dotFp, NULL, NULL);
	else
	  V_close (dotFp, NULL, NULL);
	return NOT_A_CDF_OR_NOT_SUPPORTED;
    }
    if (whichVersion == 2) {
      memcpy((long *)cFileSize, &cSize2, sizeof(long));
      memcpy((long *)uFileSize, &uSize2, sizeof(long));
    } else {
      memcpy((OFF_T *)cFileSize, &cSize3, sizeof(OFF_T));
      memcpy((OFF_T *)uFileSize, &uSize3, sizeof(OFF_T));
    }
    break;
  }
  /****************************************************************************
  * STATUS_TEXT_, 
  ****************************************************************************/
  case STATUS_TEXT_: {
    char *textPtr;
    textPtr = va_arg (Va->ap, char *);
    CDFstatusText (Cur->status, textPtr);
    break;
  }
  /****************************************************************************
  * DATATYPE_SIZE_, 
  ****************************************************************************/
  case DATATYPE_SIZE_: {
    long dataType = va_arg (Va->ap, long);
    long *numBytes = va_arg (Va->ap, long *);
    if (!ValidDataType((Int32)dataType)) return BAD_DATA_TYPE;
    *numBytes = (long) CDFelemSize (dataType);
    break;
  }
  /****************************************************************************
  * rVARs_NUMDIMS_/zVAR_NUMDIMS_
  *    Note that inquiring the number of rVariable dimensions is allowed while
  * in zMode.
  ****************************************************************************/
  case rVARs_NUMDIMS_: {
    struct CDFstruct *CDF;
    long *numDims = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    *numDims = CDF->rNumDims;
    break;
  }
  case zVAR_NUMDIMS_: {
    struct CDFstruct *CDF;
    long *numDimsP = va_arg (Va->ap, long *);
    Logical zVar; Int32 numDims;
    OFF_T offset;
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTvarSELECTED(CDF,TRUE)) return NO_VAR_SELECTED;

    zVar = CurrentVarMode(CDF,TRUE);
    if (zModeON(CDF) || zVar) offset = CDF->CURzVarOffset64;
    else offset = CDF->CURrVarOffset64;
    
    if (!sX(CalcDimParms64(CDF,offset,zVar,&numDims,NULL,NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    ASSIGNnotNULL (numDimsP, numDims)
    break;
  }
  /****************************************************************************
  * rVARs_DIMSIZES_/zVAR_DIMSIZES_
  *    Note that inquiring the rVariable dimension sizes is allowed while in
  * zMode.
  ****************************************************************************/
  case rVARs_DIMSIZES_: {
    struct CDFstruct *CDF;
    int dimN;
    long *dimsize = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    for (dimN = 0; dimN < CDF->rNumDims; dimN++) {
       dimsize[dimN] = CDF->rDimSizes[dimN];
    }
    break;
  }
  case zVAR_DIMSIZES_: {
    struct CDFstruct *CDF; Logical zVar;
    Int32 numDims, dimSizes[CDF_MAX_DIMS];
    OFF_T offset;
    long *dimSizesP = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTvarSELECTED(CDF,TRUE)) return NO_VAR_SELECTED;

    zVar = CurrentVarMode(CDF,TRUE);
    if (zModeON(CDF) || zVar) offset = CDF->CURzVarOffset64;
    else offset = CDF->CURrVarOffset64;

    if (!sX(CalcDimParms64(CDF,offset,zVar,&numDims,dimSizes,NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    ASSIGNnotNULLarray (dimSizesP, numDims, dimSizes)
    break;
  }
  /****************************************************************************
  * CDF_ENCODING_, 
  ****************************************************************************/
  case CDF_ENCODING_: {
    struct CDFstruct *CDF;
    long *encoding = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    *encoding = (long) CDF->encoding;
    break;
  }
  /****************************************************************************
  * CDF_MAJORITY_, 
  ****************************************************************************/
  case CDF_MAJORITY_: {
    struct CDFstruct *CDF;
    long *majority = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    *majority = BOO(CDF->rowMajor,ROW_MAJOR,COLUMN_MAJOR);
    break;
  }
  /****************************************************************************
  * CDF_FORMAT_, 
  ****************************************************************************/
  case CDF_FORMAT_: {
    struct CDFstruct *CDF;
    long *format = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    *format = BOO(CDF->singleFile,SINGLE_FILE,MULTI_FILE);
    break;
  }
  /****************************************************************************
  * CDF_CHECKSUM_,
  ****************************************************************************/
  case CDF_CHECKSUM_: {
    struct CDFstruct *CDF;
    long *checksum = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    *checksum = CDF->checksum;
    break;
  }
  /****************************************************************************
  * CDF_COMPRESSION_
  ****************************************************************************/
  case CDF_COMPRESSION_: {
    long *cType = va_arg (Va->ap, long *);      /* Compression type. */
    long *cParms = va_arg (Va->ap, long *);     /* Compression parameters. */
    long *cPct = va_arg (Va->ap, long *);	/* Compression percentage. */
    struct CDFstruct *CDF; struct CCRstruct64 CCR; struct CPRstruct64 CPR;
    int i; OFF_T cTotal, uTotal;
    SelectCDF (Cur->cdf, CDF)
    /**************************************************************************
    * If multi-file or uncompressed...
    **************************************************************************/
    if (!CDF->singleFile || CDF->uDotFp == NULL) {
      *cType = NO_COMPRESSION;
      *cPct = 100;
      break;
    }
    /**************************************************************************
    * ...otherwise, read the CCR and CPR.
    **************************************************************************/
    if (!sX(ReadCCR64(CDF->dotFp,V3_CCR_OFFSET64,
		      CCR_RECORD,&CCR,
		      CCR_NULL),&pStatus)) return pStatus;
    if (!sX(ReadCPR64(CDF->dotFp,CCR.CPRoffset,
		      CPR_RECORD,&CPR,
		      CPR_NULL),&pStatus)) return pStatus;
    *cType = CPR.cType;
    if (CPR.pCount > CDF_MAX_PARMS) return TOO_MANY_PARMS;
    for (i = 0; i < CPR.pCount; i++) cParms[i] = CPR.cParms[i];
    /**************************************************************************
    * Calculate the percentage.  If the `uSize' field of the CCR is zero,
    * then the compressed CDF is empty (and the percentage is unknown).
    **************************************************************************/
    if (CCR.uSize == 0)
      *cPct = 0;
    else {
      cTotal = MAGIC_NUMBERS_SIZE + CCR.RecordSize + CPR.RecordSize;
      uTotal = MAGIC_NUMBERS_SIZE + CCR.uSize;
      *cPct = (long) (((100.0 * cTotal) / uTotal) + 0.5);
    }
    break;
  }
  /****************************************************************************
  * CDF_COPYRIGHT_
  ****************************************************************************/
  case CDF_COPYRIGHT_: {
    struct CDFstruct *CDF;
    char *copyRight = va_arg (Va->ap,  char *);
    SelectCDF (Cur->cdf, CDF)
    if (!sX(ReadCDR64(CDF->fp,CDF->CDRoffset64,
		      CDR_COPYRIGHT,copyRight,
		      CDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    copyRight[CDF_COPYRIGHT_LEN] = NUL;
    break;
  }
  /****************************************************************************
  * LIB_COPYRIGHT_
  ****************************************************************************/
  case LIB_COPYRIGHT_: {
    char *copyRight = va_arg (Va->ap, char *);
    CDFcopyRight (copyRight);
    break;
  }
  /****************************************************************************
  * CDF_NUMrVARS_/CDF_NUMzVARS_
  *    Inquire number of r/z variables.  When in zMode, the number of
  * rVariables is always zero (0).  (Inquiring the number of rVariables
  * while in zMode is one of the few legal rVariable operations).
  ****************************************************************************/
  case CDF_NUMrVARS_:
  case CDF_NUMzVARS_: {
    Logical zOp = (Va->item == CDF_NUMzVARS_);
    struct CDFstruct *CDF;
    long *numVars = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (zModeON(CDF))
      *numVars = BOO(zOp,CDF->NrVars + CDF->NzVars,0);
    else
      *numVars = BOO(zOp,CDF->NzVars,CDF->NrVars);
    break;
  }
  /****************************************************************************
  * CDF_NUMATTRS_, 
  ****************************************************************************/
  case CDF_NUMATTRS_: {
    struct CDFstruct *CDF;
    long *numAttrs = va_arg (Va->ap, long *);
    Int32 tNumAttrs;
    SelectCDF (Cur->cdf, CDF)
    if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
		      GDR_NUMATTR,&tNumAttrs,
		      GDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    *numAttrs = tNumAttrs;
    break;
  }
  /****************************************************************************
  * CDF_NUMgATTRS_/CDF_NUMvATTRS_
  ****************************************************************************/
  case CDF_NUMvATTRS_:
  case CDF_NUMgATTRS_: {
    Logical gOp = (Va->item == CDF_NUMgATTRS_);
    struct CDFstruct *CDF;
    long *numAttrs = va_arg (Va->ap, long *);
    Int32 totalAttrs, scope;
    OFF_T offset;
    int attrX;
    SelectCDF (Cur->cdf, CDF)
    *numAttrs = 0;
    if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
		      GDR_NUMATTR,&totalAttrs,
		      GDR_ADRHEAD,&offset,
		      GDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    for (attrX = 0; attrX < totalAttrs; attrX++) {
       CDF->fp->CurADRIndex = attrX;
       if (!sX(ReadADR64(CDF->fp,offset,
		         ADR_SCOPE,&scope,
		         ADR_NULL),&pStatus)) {
	 AbortAccess64 (CDF, UPDATE, noDELETE);
	 return pStatus;
       }
       if ((gOp && GLOBALscope(scope)) ||
	   (!gOp && VARIABLEscope(scope))) (*numAttrs)++;
       if (!sX(ReadADR64(CDF->fp,offset,
		         ADR_ADRNEXT,&offset,
		         ADR_NULL),&pStatus)) {
	 AbortAccess64 (CDF, UPDATE, noDELETE);
	 return pStatus;
       }
    }
    break;
  }
  /****************************************************************************
  * rVARs_MAXREC_/zVARs_MAXREC_
  *    Maximum record number of all of the rVariables/zVariables.  Note that
  * inquiring the maximum rVariable record number is allowed while in zMode.
  ****************************************************************************/
  case rVARs_MAXREC_: {
    struct CDFstruct *CDF;
    long *maxRec = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (zModeON(CDF))
      *maxRec = NO_RECORD;
    else
      *maxRec = CDF->rMaxRec;
    break;
  }
  case zVARs_MAXREC_: {
    struct CDFstruct *CDF;
    long *maxRec = va_arg (Va->ap, long *);
    Int32 tMaxRec;
    OFF_T offset;
    SelectCDF (Cur->cdf, CDF)
    /**************************************************************************
    * If zMode is on, consider both rVariables and zVariables.  If zMode is
    * not on, only consider the zVariables.  Because zVariables did not exist
    * in CDF V2.0, the problem with the `VDRnext' offset in the last VDR does
    * not have to be considered here.
    **************************************************************************/
    *maxRec = BOO(zModeON(CDF),CDF->rMaxRec,NO_RECORD);
    if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
		      GDR_zVDRHEAD,&offset,
		      GDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    while (offset != (OFF_T) ZERO_OFFSET64) {
      if (!sX(ReadVDR64(CDF,CDF->fp,offset,TRUE,
		        VDR_MAXREC,&tMaxRec,
		        VDR_VDRNEXT,&offset,
		        VDR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      *maxRec = MAXIMUM (*maxRec, tMaxRec);
    }
    break;
  }
  /****************************************************************************
  * CDF_VERSION_, 
  ****************************************************************************/
  case CDF_VERSION_: {
    struct CDFstruct *CDF;
    long *version = va_arg (Va->ap, long *);
    Int32 tVersion;
    SelectCDF (Cur->cdf, CDF)
    if (!sX(ReadCDR64(CDF->fp,CDF->CDRoffset64,
		      CDR_VERSION,&tVersion,
		      CDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    *version = tVersion;
    break;
  }
  /****************************************************************************
  * CDF_RELEASE_, 
  ****************************************************************************/
  case CDF_RELEASE_: {
    struct CDFstruct *CDF;
    long *release = va_arg (Va->ap, long *);
    Int32 tRelease;
    SelectCDF (Cur->cdf, CDF)
    if (!sX(ReadCDR64(CDF->fp,CDF->CDRoffset64,
		      CDR_RELEASE,&tRelease,
		      CDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    *release = tRelease;
    break;
  }
  /****************************************************************************
  * CDF_INCREMENT_, 
  ****************************************************************************/
  case CDF_INCREMENT_: {
    struct CDFstruct *CDF;
    long *increment = va_arg (Va->ap, long *);
    Int32 tIncrement;
    SelectCDF (Cur->cdf, CDF)
    if (!sX(ReadCDR64(CDF->fp,CDF->CDRoffset64,
		      CDR_INCREMENT,&tIncrement,
		      CDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    *increment = tIncrement;
    break;
  }
  /****************************************************************************
  * CDF_LEAPSECONDLASTUPDATED_, 
  ****************************************************************************/
  case CDF_LEAPSECONDLASTUPDATED_: {
    struct CDFstruct *CDF;
    long *lastUpdated = va_arg (Va->ap, long *);
    Int32 tLastUpdated;
    SelectCDF (Cur->cdf, CDF)
    if (!sX(ReadGDR64(CDF->fp,CDF->GDRoffset64,
		      GDR_LEAPSECONDLASTUPDATED,&tLastUpdated,
		      GDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    *lastUpdated = tLastUpdated;
    break;
  }
  /****************************************************************************
  * LIB_VERSION_, 
  ****************************************************************************/
  case LIB_VERSION_: {
    long *version = va_arg (Va->ap, long *);
    *version = CDF_LIBRARY_VERSION;
    break;
  }
  /****************************************************************************
  * LIB_RELEASE_, 
  ****************************************************************************/
  case LIB_RELEASE_: {
    long *release = va_arg (Va->ap, long *);
    *release = CDF_LIBRARY_RELEASE;
    break;
  }
  /****************************************************************************
  * LIB_INCREMENT_, 
  ****************************************************************************/
  case LIB_INCREMENT_: {
    long *increment = va_arg (Va->ap, long *);
    *increment = CDF_LIBRARY_INCREMENT;
    break;
  }
  /****************************************************************************
  * LIB_subINCREMENT_, 
  ****************************************************************************/
  case LIB_subINCREMENT_: {
    char *subincrement = va_arg (Va->ap, char *);
    *subincrement = CDF_LIBRARY_subINCREMENT;
    break;
  }
  /****************************************************************************
  * rVAR_NAME_/zVAR_NAME_
  *    Note that a temporary variable is used when reading the variable name.
  * This is because the caller may have only allocated enough memory for the
  * size name they expect (ie., less than CDF_VAR_NAME_LEN characters).  Since
  * the variable name is NUL-terminated in the CDF, only the actual characters
  * of the name will be copied to the caller's buffer.
  ****************************************************************************/
  case rVAR_NAME_:
  case zVAR_NAME_: {
    Logical zOp = (Va->item == zVAR_NAME_), zVar;
    struct CDFstruct *CDF;
    char *varName = va_arg (Va->ap,  char *), tName[CDF_VAR_NAME_LEN256+1];
    OFF_T offset;
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) offset = CDF->CURzVarOffset64;
    else offset = CDF->CURrVarOffset64;

    if (!sX(ReadVDR64(CDF,CDF->fp,offset,zVar,
		      VDR_NAME,tName,
		      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    strcpyX (varName, tName, CDF_VAR_NAME_LEN256);
    break;
  }
  /****************************************************************************
  * rVAR_DATATYPE_/zVAR_DATATYPE_
  *    If this for an rVarible named "EPOCH" in a CDF prior to CDF V2.1.1,
  * then return the CDF_EPOCH data type if the actual data type is CDF_REAL8
  * or CDF_DOUBLE.  (The CDF_EPOCH data type was not introduced until CDF
  * V2.1.1).  Note that only rVariables were supported prior to CDF V2.3.
  ****************************************************************************/
  case rVAR_DATATYPE_:
  case zVAR_DATATYPE_: {
    Logical zOp = (Va->item == zVAR_DATATYPE_), zVar;
    struct CDFstruct *CDF;
    long *dataType = va_arg (Va->ap, long *);
    Int32 tDataType;
    OFF_T offset;
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) offset = CDF->CURzVarOffset64;
    else offset = CDF->CURrVarOffset64;

    if (!sX(ReadVDR64(CDF,CDF->fp,offset,zVar,
		      VDR_DATATYPE,&tDataType,
		      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (!zVar && CDF->fakeEPOCH) {
      char varName[CDF_VAR_NAME_LEN256+1];
      if (!sX(ReadVDR64(CDF,CDF->fp,offset,zVar,
		        VDR_NAME,varName,
		        VDR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (!strcmpITB(varName,"EPOCH") && FLOAT8dataType(tDataType)) {
	tDataType = CDF_EPOCH;
      } else if (!strcmpITB(varName,"EPOCH") && FLOAT16dataType(tDataType)) {
        tDataType = CDF_EPOCH16;
      }
    }
    *dataType = tDataType;
    break;
  }
  /****************************************************************************
  * rVAR_NUMELEMS_/zVAR_NUMELEMS_, 
  ****************************************************************************/
  case rVAR_NUMELEMS_:
  case zVAR_NUMELEMS_: {
    Logical zOp = (Va->item == zVAR_NUMELEMS_), zVar;
    struct CDFstruct *CDF;
    long *numElements = va_arg (Va->ap, long *);
    Int32 tNumElems;
    OFF_T offset;
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) offset = CDF->CURzVarOffset64;
    else offset = CDF->CURrVarOffset64;

    if (!sX(ReadVDR64(CDF,CDF->fp,offset,zVar,
		      VDR_NUMELEMS,&tNumElems,
		      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    *numElements = tNumElems;
    break;
  }
  /****************************************************************************
  * rVAR_RECVARY_/zVAR_RECVARY_, 
  ****************************************************************************/
  case rVAR_RECVARY_:
  case zVAR_RECVARY_: {
    Logical zOp = (Va->item == zVAR_RECVARY_), zVar;
    struct CDFstruct *CDF;
    long *recVary = va_arg (Va->ap, long *);
    Int32 flags;
    OFF_T offset;
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) offset = CDF->CURzVarOffset64;
    else offset = CDF->CURrVarOffset64;

    if (!sX(ReadVDR64(CDF,CDF->fp,offset,zVar,
		      VDR_FLAGS,&flags,
		      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    *recVary = BOO(RECvaryBITset(flags),VARY,NOVARY);
    break;
  }
  /****************************************************************************
  * rVAR_DIMVARYS_/zVAR_DIMVARYS_, 
  ****************************************************************************/
  case rVAR_DIMVARYS_:
  case zVAR_DIMVARYS_: {
    Logical zOp = (Va->item == zVAR_DIMVARYS_), zVar;
    struct CDFstruct *CDF; Int32 numDims, dimVarys[CDF_MAX_DIMS];
    OFF_T offset;
    long *dimVarysP = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) offset = CDF->CURzVarOffset64;
    else offset = CDF->CURrVarOffset64;
    if (!sX(CalcDimParms64(CDF,offset,zVar,&numDims,NULL,dimVarys),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    ASSIGNnotNULLarray (dimVarysP, numDims, dimVarys)
    break;
  }
  /****************************************************************************
  * rVAR_MAXREC_/zVAR_MAXREC_, 
  ****************************************************************************/
  case rVAR_MAXREC_:
  case zVAR_MAXREC_: {
    Logical zOp = (Va->item == zVAR_MAXREC_), zVar;
    struct CDFstruct *CDF; Int32 tMaxRec; OFF_T offset;
    long *maxRec = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) offset = CDF->CURzVarOffset64;
    else offset = CDF->CURrVarOffset64;

    if (!sX(ReadVDR64(CDF,CDF->fp,offset,zVar,
		      VDR_MAXREC,&tMaxRec,
		      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    *maxRec = (long) tMaxRec;
    break;
  }
  /****************************************************************************
  * rVAR_MAXallocREC_/zVAR_MAXallocREC_, 
  ****************************************************************************/
  case rVAR_MAXallocREC_:
  case zVAR_MAXallocREC_: {
    Logical zOp = (Va->item == zVAR_MAXallocREC_), zVar;
    struct CDFstruct *CDF; struct VarStruct *Var;
    Int32 maxRec, lastAllocatedRecN; OFF_T offset; int vType;
    long *maxAllocated = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) offset = CDF->CURzVarOffset64;
    else offset = CDF->CURrVarOffset64;
    if (zModeON(CDF))
      if (CDF->CURzVarNum < CDF->NrVars)
        Var = CDF->rVars[(int)CDF->CURzVarNum];
      else
        Var = CDF->zVars[(int)(CDF->CURzVarNum - CDF->NrVars)];
    else
      Var = BOO(zOp,CDF->zVars[(int)CDF->CURzVarNum],
                    CDF->rVars[(int)CDF->CURrVarNum]);

    if (!sX(ReadVDR64(CDF,CDF->fp,offset,zVar,
                      VDR_MAXREC,&maxRec,
                      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (!sX(VariableType64(CDF,offset,zVar,&vType),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }    
    /**************************************************************************
    * If a single-file CDF, pass back the maximum record number allocated.
    * If a multi-file CDF, pass back the maximum record number written (which
    * will always be the maximum record allocated).
    **************************************************************************/
    if (CDF->singleFile) {
      if (!sX(LastRecord64(CDF,offset,zVar,&lastAllocatedRecN),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (vType == SPARSE_RECORDS_) {
          if (Var != NULL) {
            if (Var->stage.areaOffset64 != (OFF_T) NO_OFFSET64 &&
                Var->stage.firstRec != NO_RECORD) {
              Int32 tmp = (Var->stage.lastRec > maxRec ? maxRec :
                           Var->stage.lastRec);
              lastAllocatedRecN += (tmp - Var->stage.firstRec + 1);
            }
          }
      } else if (vType == COMPRESSED_ ||
                 vType == SPARSE_COMPRESSED_RECORDS_) {
        if (Var != NULL) {
          if (Var->stage.areaOffset64 != (OFF_T) NO_OFFSET64 &&
              Var->stage.firstRec != NO_RECORD &&
              Var->stage.dotOffset64 == (OFF_T) NO_OFFSET64) {
            Int32 tmp = (Var->stage.lastRec > maxRec ? maxRec :
                         Var->stage.lastRec);
            lastAllocatedRecN += (tmp - Var->stage.firstRec + 1);
          }
        }
      }
      *maxAllocated = lastAllocatedRecN;
    }
    else
      *maxAllocated = (long) maxRec;
    break;
  }
  /****************************************************************************
  * rVAR_NUMRECS_/zVAR_NUMRECS_, 
  ****************************************************************************/
  case rVAR_NUMRECS_:
  case zVAR_NUMRECS_: {
    Logical zOp = (Va->item == zVAR_NUMRECS_), zVar; struct VarStruct *Var;
    struct CDFstruct *CDF; Int32 nRecords; int vType; OFF_T vdrOffset;
    Int32 tMaxRec;
    long *nRecordsP = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) vdrOffset = CDF->CURzVarOffset64;
    else vdrOffset = CDF->CURrVarOffset64;

    if (zModeON(CDF))
      if (CDF->CURzVarNum < CDF->NrVars)
        Var = CDF->rVars[(int)CDF->CURzVarNum];
      else
        Var = CDF->zVars[(int)(CDF->CURzVarNum - CDF->NrVars)];
    else
      Var = BOO(zOp,CDF->zVars[(int)CDF->CURzVarNum],
                    CDF->rVars[(int)CDF->CURrVarNum]);

    if (!sX(ReadVDR64(CDF,CDF->fp,vdrOffset,zVar,
		      VDR_MAXREC,&tMaxRec,
		      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (!sX(VariableType64(CDF,vdrOffset,zVar,&vType),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Depending on the variable type...
    **************************************************************************/
    switch (vType) {
      case SPARSE_RECORDS_:
	if (!sX(IndexingStatistics64(CDF,vdrOffset,zVar,
				     NULL,NULL,NULL,
				     &nRecords,NULL),&pStatus)) return pStatus;
	if (Var != NULL) {
	  if (Var->stage.areaOffset64 != (OFF_T) NO_OFFSET64 &&
	      Var->stage.firstRec != NO_RECORD) {
	    Int32 tmp = (Var->stage.lastRec > tMaxRec ? tMaxRec : Var->stage.lastRec);
	    nRecords += (tmp - Var->stage.firstRec + 1);
	  }
	}
	break;
      case COMPRESSED_:
      case SPARSE_COMPRESSED_RECORDS_:
	if (!sX(IndexingStatistics64(CDF,vdrOffset,zVar,
				     NULL,NULL,NULL,
				     &nRecords,NULL),&pStatus)) return pStatus;
	if (Var != NULL) {
	  if (Var->stage.areaOffset64 != (OFF_T) NO_OFFSET64 &&
	      Var->stage.firstRec != NO_RECORD &&
	      Var->stage.dotOffset64 == (OFF_T) NO_OFFSET64) {
            Int32 tmp = (Var->stage.lastRec > tMaxRec ? tMaxRec : Var->stage.lastRec);
	    nRecords += (tmp - Var->stage.firstRec + 1);
	  }
	}
	break;
      case SPARSE_ARRAYS_:
      case SPARSE_RECORDS_AND_ARRAYS_:
	return CDF_INTERNAL_ERROR;
      case STANDARD_:
      case IN_MULTI_: {
	Int32 maxRec;
	if (!sX(ReadVDR64(CDF,CDF->fp,vdrOffset,zVar,
			  VDR_MAXREC,&maxRec,
			  VDR_NULL),&pStatus)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
	nRecords = maxRec + 1;
	break;
      }
    }
    ASSIGNnotNULL (nRecordsP, nRecords)
    break;
  }
  /****************************************************************************
  * rVAR_NUMallocRECS_/zVAR_NUMallocRECS_, 
  ****************************************************************************/
  case rVAR_NUMallocRECS_:
  case zVAR_NUMallocRECS_: {
    Logical zOp = (Va->item == zVAR_NUMallocRECS_), zVar;
    struct CDFstruct *CDF; Int32 nAllocated; int vType; OFF_T offset;
    Int32 tMaxRec; struct VarStruct *Var;
    long *nAllocatedP = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) offset = CDF->CURzVarOffset64;
    else offset = CDF->CURrVarOffset64;
    if (zModeON(CDF))
      if (CDF->CURzVarNum < CDF->NrVars)
        Var = CDF->rVars[(int)CDF->CURzVarNum];
      else
        Var = CDF->zVars[(int)(CDF->CURzVarNum - CDF->NrVars)];
    else
      Var = BOO(zOp,CDF->zVars[(int)CDF->CURzVarNum],
                    CDF->rVars[(int)CDF->CURrVarNum]);

    if (!sX(ReadVDR64(CDF,CDF->fp,offset,zVar,
		      VDR_MAXREC,&tMaxRec,
		      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (!sX(VariableType64(CDF,offset,zVar,&vType),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * Depending on the variable type...
    **************************************************************************/
    switch (vType) {
      case STANDARD_:
      case SPARSE_RECORDS_:
      case COMPRESSED_:
      case SPARSE_COMPRESSED_RECORDS_:
      case SPARSE_ARRAYS_:
      case SPARSE_RECORDS_AND_ARRAYS_: {
	if (!sX(IndexingStatistics64(CDF,offset,
				     zVar,NULL,NULL,
				     &nAllocated,
				     NULL,NULL),&pStatus)) return pStatus;
        if (vType == SPARSE_RECORDS_) {
          if (Var != NULL) {
            if (Var->stage.areaOffset64 != (OFF_T) NO_OFFSET64 &&
                Var->stage.firstRec != NO_RECORD) {
              Int32 tmp = (Var->stage.lastRec > tMaxRec ? tMaxRec :
                           Var->stage.lastRec);
              nAllocated += (tmp - Var->stage.firstRec + 1);
            }
          }
        } else if (vType == COMPRESSED_ ||
                   vType == SPARSE_COMPRESSED_RECORDS_) {
          if (Var != NULL) {
            if (Var->stage.areaOffset64 != (OFF_T) NO_OFFSET64 &&
                Var->stage.firstRec != NO_RECORD &&
                Var->stage.dotOffset64 == (OFF_T) NO_OFFSET64) {
              Int32 tmp = (Var->stage.lastRec > tMaxRec ? tMaxRec :
                           Var->stage.lastRec);              
              nAllocated += (tmp - Var->stage.firstRec + 1);
            }
          }
        }
	break;
      }
      case IN_MULTI_: {
	Int32 maxRec;
	if (!sX(ReadVDR64(CDF,CDF->fp,offset,zVar,
			  VDR_MAXREC,&maxRec,
			  VDR_NULL),&pStatus)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return pStatus;
	}
	nAllocated = maxRec + 1;
	break;
      }
    }
    ASSIGNnotNULL (nAllocatedP, nAllocated)
    break;
  }
  /****************************************************************************
  * rVAR_ALLOCATEDFROM_/zVAR_ALLOCATEDFROM_
  * Determines the next allocated record AT or AFTER `startRec' (meaning that
  * `startRec' could be the record number returned).
  ****************************************************************************/
  case rVAR_ALLOCATEDFROM_:
  case zVAR_ALLOCATEDFROM_: {
    Logical zOp = (Va->item == zVAR_ALLOCATEDFROM_), zVar, found;
    struct CDFstruct *CDF; Int32 nextRec; OFF_T offset;
    Int32 startRec = (Int32) va_arg (Va->ap, long);
    long *fromRec = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    if (startRec <= NO_RECORD) return BAD_REC_NUM;

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) offset = CDF->CURzVarOffset64;
    else offset = CDF->CURrVarOffset64;

    if (!sX(NextRecord64(CDF,offset,zVar,startRec,&nextRec,&found),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (!found) return NO_SUCH_RECORD;
    *fromRec = (long) nextRec;
    break;
  }
  /****************************************************************************
  * rVAR_ALLOCATEDTO_/zVAR_ALLOCATEDTO_
  * Determines the last record (before the next unallocated record) allocated
  * AT or AFTER `startRec'.  This can span non-contiguous blocks of records.
  ****************************************************************************/
  case rVAR_ALLOCATEDTO_:
  case zVAR_ALLOCATEDTO_: {
    Logical zOp = (Va->item == zVAR_ALLOCATEDTO_), zVar, found;
    struct CDFstruct *CDF; Int32 lastRec; OFF_T vdrOffset;
    Int32 startRec = (Int32) va_arg (Va->ap, long);
    long *toRec = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    if (startRec <= NO_RECORD) return BAD_REC_NUM;

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) vdrOffset = CDF->CURzVarOffset64;
    else vdrOffset = CDF->CURrVarOffset64;

    /**************************************************************************
    * Determine the last record in the block containing the starting record.
    **************************************************************************/
    if (!sX(SearchForRecord64(CDF,vdrOffset,zVar,startRec,
			      NULL,&lastRec,NULL,&found),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (!found) return NO_SUCH_RECORD;
    /**************************************************************************
    * Keep searching until an unallocated record is encountered.
    **************************************************************************/
    while (found) {
      Int32 nextRec = (Int32) (lastRec + 1);
      if (!sX(SearchForRecord64(CDF,vdrOffset,zVar,nextRec,
				NULL,&lastRec,NULL,&found),&pStatus)) {
        AbortAccess64 (CDF, UPDATE, noDELETE);
        return pStatus;
      }
    }
    /**************************************************************************
    * Return the last allocated record detected.
    **************************************************************************/
    *toRec = (long) lastRec;
    break;
  }
  /****************************************************************************
  * rVAR_COMPRESSION_/zVAR_COMPRESSION_
  ****************************************************************************/
  case rVAR_COMPRESSION_:
  case zVAR_COMPRESSION_: {
    Logical zOp = (Va->item == zVAR_COMPRESSION_), zVar;
    struct CDFstruct *CDF; /* struct VarStruct *Var; */ struct CPRstruct64 CPR;
    Int32 flags; int parmN; OFF_T VDRoffset, PRoffset;
    long *cType = va_arg (Va->ap, long *);      /* Compression type. */
    long *cParms = va_arg (Va->ap, long *);     /* Compression parameters. */
    long *cPct = va_arg (Va->ap, long *);	/* Compression percentage. */
    /**************************************************************************
    * Select/validate/locate CDF and variable.
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) VDRoffset = CDF->CURzVarOffset64;
    else VDRoffset = CDF->CURrVarOffset64;

    /**************************************************************************
    * Read fields from VDR.
    **************************************************************************/
    if (!sX(ReadVDR64(CDF,CDF->fp,VDRoffset,zVar,
		      VDR_FLAGS,&flags,
		      VDR_CPRorSPR,&PRoffset,
		      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * If the compression bit is set, read the CPR.
    **************************************************************************/
    if (VARcompressionBITset(flags)) {
      if (!sX(ReadCPR64(CDF->fp,PRoffset,
		        CPR_RECORD,&CPR,
		        CPR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      *cType = CPR.cType;
      for (parmN = 0; parmN < CPR.pCount; parmN++) {
	 cParms[parmN] = CPR.cParms[parmN];
      }
      if (!sX(CalcCompressionPct64(CDF,VDRoffset,zVar,cPct),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    else {
      *cType = NO_COMPRESSION;
      *cPct = 100;
    }
    break;
  }
  /****************************************************************************
  * rVAR_SPARSEARRAYS_/zVAR_SPARSEARRAYS_
  ****************************************************************************/
  case rVAR_SPARSEARRAYS_:
  case zVAR_SPARSEARRAYS_: {
    Logical zOp = (Va->item == zVAR_SPARSEARRAYS_), zVar;
    struct CDFstruct *CDF; /* struct VarStruct *Var; */ struct SPRstruct64 SPR;
    Int32 flags; int parmN; OFF_T VDRoffset, PRoffset;
    long *sArraysType = va_arg (Va->ap, long *);   /* Sparseness type. */
    long *sArraysParms = va_arg (Va->ap, long *);  /* Sparseness parameters. */
    long *sArraysPct = va_arg (Va->ap, long *);    /* Sparseness percentage. */
    /**************************************************************************
    * Select/validate/locate CDF and variable.
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) VDRoffset = CDF->CURzVarOffset64;
    else VDRoffset = CDF->CURrVarOffset64;

    /**************************************************************************
    * Read fields from VDR.
    **************************************************************************/
    if (!sX(ReadVDR64(CDF,CDF->fp,VDRoffset,zVar,
		      VDR_FLAGS,&flags,
		      VDR_CPRorSPR,&PRoffset,
		      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    /**************************************************************************
    * If the sparse arrays bit is set, read the SPR.
    **************************************************************************/
    if (SPARSEarraysBITset(flags)) {
      if (!sX(ReadSPR64(CDF->fp,PRoffset,
		        SPR_RECORD,&SPR,
		        SPR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      *sArraysType = SPR.sArraysType;
      for (parmN = 0; parmN < SPR.pCount; parmN++) {
	 sArraysParms[parmN] = SPR.sArraysParms[parmN];
      }
      *sArraysPct = 100;
    }
    else {
      *sArraysType = NO_SPARSEARRAYS;
      *sArraysPct = 100;
    }
    break;
  }
  /****************************************************************************
  * rVAR_SPARSERECORDS_/zVAR_SPARSERECORDS_
  ****************************************************************************/
  case rVAR_SPARSERECORDS_:
  case zVAR_SPARSERECORDS_: {
    Logical zOp = (Va->item == zVAR_SPARSERECORDS_), zVar;
    struct CDFstruct *CDF; /* struct VarStruct *Var; */
    Int32 sRecords; OFF_T VDRoffset;
    long *sRecordsType = va_arg (Va->ap, long *);
    /**************************************************************************
    * Select/validate/locate CDF and variable.
    **************************************************************************/
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) VDRoffset = CDF->CURzVarOffset64;
    else VDRoffset = CDF->CURrVarOffset64;

    /**************************************************************************
    * Read sparse records field from VDR.
    **************************************************************************/
    if (!sX(ReadVDR64(CDF,CDF->fp,VDRoffset,zVar,
		      VDR_sRECORDS,&sRecords,
		      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    *sRecordsType = sRecords;
    break;
  }
  /****************************************************************************
  * rVAR_NUMBER_/zVAR_NUMBER_
  *    Determines the variable number for a specified variable name.
  ****************************************************************************/
  case rVAR_NUMBER_:
  case zVAR_NUMBER_: {
    Logical zOp = (Va->item == zVAR_NUMBER_), zVar;
    struct CDFstruct *CDF;
    char *varName = va_arg (Va->ap, char *);
    long *varNum = va_arg (Va->ap,  long *);
    Int32 varN; OFF_T offset;
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    tStatus = FindVarByName64 (CDF, varName, &offset, &zVar, NULL);
    switch (tStatus) {
      case CDF_OK:
	break;
      case NO_SUCH_VAR:
	return tStatus;
      default:
	if (!sX(tStatus,&pStatus)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return tStatus;
	}
    }
    if (!sX(ReadVDR64(CDF,CDF->fp,offset,zVar,
		      VDR_NUM,&varN,
		      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (zModeON(CDF))
      *varNum = BOO(zVar,CDF->NrVars,0) + varN;
    else
      if (zOp)
	if (zVar)
	  *varNum = varN;
	else
	  return NO_SUCH_VAR;                   /* Wrong type of variable. */
      else
	if (zVar)
	  return NO_SUCH_VAR;                   /* Wrong type of variable. */
	else
	  *varNum = varN;
    break;
  }
  /****************************************************************************
  * rVAR_BLOCKINGFACTOR_/zVAR_BLOCKINGFACTOR_, 
  ****************************************************************************/
  case rVAR_BLOCKINGFACTOR_:
  case zVAR_BLOCKINGFACTOR_: {
    Logical zOp = (Va->item == zVAR_BLOCKINGFACTOR_), zVar;
    struct CDFstruct *CDF;
    long *nExtendRecs = va_arg (Va->ap, long *);
    Int32 tRecs; OFF_T offset;
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) offset = CDF->CURzVarOffset64;
    else offset = CDF->CURrVarOffset64;

    if (!sX(ReadVDR64(CDF,CDF->fp,offset,zVar,
		      VDR_BLOCKING,&tRecs,
		      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    *nExtendRecs = tRecs;
    break;
  }
  /****************************************************************************
  * rVAR_nINDEXRECORDS_/zVAR_nINDEXRECORDS_,
  * rVAR_nINDEXENTRIES_/zVAR_nINDEXENTRIES_,
  * rVAR_nINDEXLEVELS_/zVAR_nINDEXLEVELS_,
  ****************************************************************************/
  case rVAR_nINDEXRECORDS_:
  case zVAR_nINDEXRECORDS_:
  case rVAR_nINDEXENTRIES_:
  case zVAR_nINDEXENTRIES_:
  case rVAR_nINDEXLEVELS_:
  case zVAR_nINDEXLEVELS_: {
    Logical zOp = (Va->item == zVAR_nINDEXRECORDS_ ||
		   Va->item == zVAR_nINDEXENTRIES_ ||
		   Va->item == zVAR_nINDEXLEVELS_), zVar;
    struct CDFstruct *CDF; Int32 count; OFF_T offset;
    long *countP = va_arg (Va->ap, long *);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;

    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) offset = CDF->CURzVarOffset64;
    else offset = CDF->CURrVarOffset64;

    /**************************************************************************
    * If a multi-file CDF, pass back a count of zero (0) and an info/warning
    * status code.  If a single-file CDF, scan through the linked list of VXRs
    * counting the parameter requested.
    **************************************************************************/
    if (!CDF->singleFile) {
      count = 0;
      sX (MULTI_FILE_FORMAT, &pStatus);
    }
    else {
      switch (Va->item) {
	case rVAR_nINDEXRECORDS_:
	case zVAR_nINDEXRECORDS_:
	  if (!sX(IndexingStatistics64(CDF,offset,zVar,
				       &count,NULL,NULL,
				       NULL,NULL),&pStatus)) return pStatus;
	  break;
	case rVAR_nINDEXENTRIES_:
	case zVAR_nINDEXENTRIES_:
	  if (!sX(IndexingStatistics64(CDF,offset,zVar,
				       NULL,&count,NULL,
				       NULL,NULL),&pStatus)) return pStatus;
	  break;
	case rVAR_nINDEXLEVELS_:
	case zVAR_nINDEXLEVELS_:
	  if (!sX(IndexingStatistics64(CDF,offset,zVar,
				       NULL,NULL,NULL,
				       NULL,&count),&pStatus)) return pStatus;
	  break;
      }
    }
    ASSIGNnotNULL (countP, count)
    break;
  }
  /****************************************************************************
  * rVAR_PADVALUE_/zVAR_PADVALUE_
  ****************************************************************************/
  case rVAR_PADVALUE_:
  case zVAR_PADVALUE_: {
    Logical zOp = (Va->item == zVAR_PADVALUE_), zVar;
    struct CDFstruct *CDF;
    void *padValue = va_arg (Va->ap, void *);
    Int32 dataType, numElems, flags; OFF_T offset;
    Int32 version, release;
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    zVar = CurrentVarMode(CDF,zOp);
    if (zModeON(CDF) || zVar) offset = CDF->CURzVarOffset64;
    else offset = CDF->CURrVarOffset64;
    if (!sX(ReadVDR64(CDF,CDF->fp,offset,zVar,
		      VDR_FLAGS,&flags,
		      VDR_DATATYPE,&dataType,
		      VDR_NUMELEMS,&numElems,
		      VDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }

    /**********************************************************************
      The length of the pad value for a CDF_CHAR variable is the
      length of the variable (i.e. CHAR/162, etc.).  If the length
      of the pad value is greater than 16, it causes segmentation fault
      for the IDL CDF interface routine.
    ***********************************************************************/
    if (STRINGdataType(dataType)) 
          numElems = 1;

    if (PADvalueBITset(flags)) {
      if (!sX(ReadVDR64(CDF,CDF->fp,offset,zVar,
	    	        VDR_PADVALUE,padValue,
		        VDR_NULL),&pStatus)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return pStatus;
      }
      if (!sX(ConvertBuffer(CDF->encoding,CDF->decoding,
			    CDF->negToPosFp0,dataType,
			    numElems,padValue,padValue),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    else {
      if (!sX(ReadCDR64(CDF->fp,CDF->CDRoffset64,
                        CDR_VERSION,&version,
                        CDR_RELEASE,&release,
                        CDR_NULL),&pStatus)) return pStatus;
      if (version*100+release < 305)
        DefaultPadValuePre350 (dataType, numElems, padValue);
      else
        DefaultPadValue (dataType, numElems, padValue);
      if (!sX(ConvertBuffer(HostEncoding(),CDF->decoding,
			    CDF->negToPosFp0,dataType,
			    numElems,padValue,padValue),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      sX (NO_PADVALUE_SPECIFIED, &pStatus);
    }
    break;
  }
  /****************************************************************************
  * rVAR_DATA_/zVAR_DATA_, 
  ****************************************************************************/
  case rVAR_DATA_:
  case zVAR_DATA_: {
    Logical zOp = (Va->item == zVAR_DATA_);
    struct CDFstruct *CDF; struct VarStruct *Var;
    Int32 phyRecNum; struct rdSTRUCT *rd; Int32 offset;
    char *value = va_arg (Va->ap, char *);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    if (!sX(InitCurrentVar64(CDF,zOp,&Var),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    rd = BOO(zModeON(CDF),&(Var->zRD),BOO(zOp,&(Var->zRD),&(CDF->rRD)));
    if (!CDF->singleFile && Var->fp == NULL) {
      if (!sX(OpenVar64(CDF,Var),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    phyRecNum = BOO(Var->recVary,rd->recNumber,0);
    offset = IndicesValueOffset(Var->numDims,
				rd->dimIndices,
				Var->dimVarys,
				Var->nPhyDimValues) * Var->NvalueBytes;
    if (!sX(ReadVarValues64(CDF,Var,phyRecNum,
			    offset,INT32_ONE,value),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    Var->accessed_at = CDF->pseudo_clock++;
    break;
  }
  /****************************************************************************
  * rVAR_HYPERDATA_/zVAR_HYPERDATA_, 
  ****************************************************************************/
  case rVAR_HYPERDATA_:
  case zVAR_HYPERDATA_: {
    Logical zOp = (Va->item == zVAR_HYPERDATA_);
    struct CDFstruct *CDF;
    struct VarStruct *Var;
    int dimN;
    struct rdSTRUCT *rd;
#if LIMITof64K
    long Nvalues, Nbytes;
#endif
    char *buffer = va_arg (Va->ap, char *);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;

    if (!sX(InitCurrentVar64(CDF,zOp,&Var),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    rd = BOO(zModeON(CDF),&(Var->zRD),BOO(zOp,&(Var->zRD),&(CDF->rRD)));
    for (dimN = 0; dimN < Var->numDims; dimN++) {
       long maxIndex = rd->dimIndices[dimN] +
		       ((rd->dimCounts[dimN] - 1) * rd->dimIntervals[dimN]);
       if (maxIndex >= Var->dimSizes[dimN]) return BAD_DIM_INDEX;
    }
#if LIMITof64K
    Nvalues = rd->recCount;
    for (dimN = 0; dimN < Var->numDims; dimN++) Nvalues *= rd->dimCounts[dimN];
    Nbytes = Nvalues * Var->NvalueBytes;
    if (TOObigIBMpc(Nbytes)) return IBM_PC_OVERFLOW;
#endif
    if (!CDF->singleFile && Var->fp == NULL) {
      if (!sX(OpenVar64(CDF,Var),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    if (!sX(HyperRead64(CDF,Var,rd,buffer),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    Var->accessed_at = CDF->pseudo_clock++;
    break;
  }
  /****************************************************************************
  * rVAR_SEQDATA_/zVAR_SEQDATA_, 
  ****************************************************************************/
  case rVAR_SEQDATA_:
  case zVAR_SEQDATA_: {
    Logical zOp = (Va->item == zVAR_SEQDATA_);
    struct CDFstruct *CDF; struct VarStruct *Var; Int32 recNum; Int32 offset;
    void *value = va_arg (Va->ap, char *);
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;
    if (!sX(InitCurrentVar64(CDF,zOp,&Var),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (!CDF->singleFile && Var->fp == NULL) {
      if (!sX(OpenVar64(CDF,Var),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
    }
    recNum = (Int32) (Var->seqValueOffset64 / (OFF_T) Var->NphyRecValues);
    if (recNum > Var->maxRec) return END_OF_VAR;
    offset = (Int32) (Var->seqValueOffset64 % (OFF_T) Var->NphyRecValues) * 
		     Var->NvalueBytes;
    if (!sX(ReadVarValues64(CDF,Var,recNum,offset,INT32_ONE,value),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    Var->seqValueOffset64++;
    Var->accessed_at = CDF->pseudo_clock++;
    break;
  }
  /****************************************************************************
  * rVARs_RECDATA_/zVARs_RECDATA_
  *    Read data records for up to all of the rVariables/zVariables.
  ****************************************************************************/
  case rVARs_RECDATA_:
  case zVARs_RECDATA_: {
    Logical zOp = (Va->item == zVARs_RECDATA_), zVar;
    struct VarStruct *Var; struct CDFstruct *CDF;
    Int32 recNum, varNt; Byte1 *tBuffer; int varX;
    long nVars = va_arg (Va->ap, long);
    long *varNs = va_arg (Va->ap, long *);
    void *buffer = va_arg (Va->ap, char *);
#if LIMITof64K
    long nBytes;
#endif
    SelectCDF (Cur->cdf, CDF)
    if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
    if (nVars < 1) return BAD_NUM_VARS;
    for (varX = 0; varX < nVars; varX++) {
       if (!sX(VarIdentity(CDF,(Int32)varNs[varX],
			   zOp,&varNt,&zVar,NULL),&pStatus)) {
	 return pStatus;
       }
       if (!sX(InitVar64(CDF,varNt,zVar,NULL),&pStatus)) {
	 AbortAccess64 (CDF, UPDATE, noDELETE);
	 return pStatus;
       }
    }
#if LIMITof64K
    for (varX = 0, nBytes = 0; varX < nVars; varX++) {
       if (!sX(VarIdentity(CDF,varNs[varX],zOp,NULL,NULL,&Var),&pStatus)) {
	 return pStatus;
       }
       nBytes += Var->NphyRecBytes;
    }
    if (TOObigIBMpc(nBytes)) return IBM_PC_OVERFLOW;
#endif
    for (varX = 0, tBuffer = buffer; varX < nVars; varX++) {
       if (!sX(VarIdentity(CDF,(Int32)varNs[varX],
			   zOp,NULL,NULL,&Var),&pStatus)) {
	 return pStatus;
       }
       if (!CDF->singleFile && Var->fp == NULL) {
	 if (!sX(OpenVar64(CDF,Var),&pStatus)) {
	   AbortAccess64 (CDF, UPDATE, noDELETE);
	   return pStatus;
	 }
       }
       recNum = BOO(Var->recVary,BOO(zOp,Var->zRD.recNumber,
					 CDF->rRD.recNumber),0);
       if (!sX(ReadVarValues64(CDF,Var,recNum,INT32_ZERO,
			       Var->NphyRecValues,tBuffer),&pStatus)) {
	 AbortAccess64 (CDF, UPDATE, noDELETE);
	 return pStatus;
       }
       tBuffer += (size_t) Var->NphyRecBytes;
       Var->accessed_at = CDF->pseudo_clock++;
    }

    break;
  }
  /****************************************************************************
  * ATTR_NAME_, 
  *    Note that a temporary variable is used when reading the attribute name.
  * This is because the caller may have only allocated enough memory for the
  * size name they expect (ie., less than CDF_ATTR_NAME_LEN characters).  Since
  * the attribute name is NUL-terminated in the CDF, only the actual characters
  * of the name will be copied to the caller's buffer.
  ****************************************************************************/
  case ATTR_NAME_: {
    struct CDFstruct *CDF;
    char *attrName = va_arg (Va->ap,  char *), tName[CDF_ATTR_NAME_LEN256+1];
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTattrSELECTED64(CDF)) return NO_ATTR_SELECTED;
    if (!sX(ReadADR64(CDF->fp,CDF->CURattrOffset64,
		      ADR_NAME,tName,
		      ADR_NULL),&pStatus)){
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    strcpyX (attrName, tName, CDF_ATTR_NAME_LEN256);
    break;
  }
  /****************************************************************************
  * ATTR_NUMBER_, 
  ****************************************************************************/
  case ATTR_NUMBER_: {
    struct CDFstruct *CDF;
    char *attrName = va_arg (Va->ap, char *);
    long *attrNum = va_arg (Va->ap,  long *);
    Int32 attrNumT; OFF_T offset;
    SelectCDF (Cur->cdf, CDF)
    tStatus = FindAttrByName64 (CDF, attrName, &offset);
    switch (tStatus) {
      case CDF_OK:
	break;
      case NO_SUCH_ATTR:
	return tStatus;
      default:
	if (!sX(tStatus,&pStatus)) {
	  AbortAccess64 (CDF, UPDATE, noDELETE);
	  return tStatus;
	}
    }
    if (!sX(ReadADR64(CDF->fp,offset,
		      ADR_NUM,&attrNumT,
		      ADR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    *attrNum = attrNumT;
    break;
  }
  /****************************************************************************
  * ATTR_SCOPE_, 
  ****************************************************************************/
  case ATTR_SCOPE_: {
    struct CDFstruct *CDF;
    long *scope = va_arg (Va->ap, long *);
    Int32 tScope;
    SelectCDF (Cur->cdf, CDF);
    if (!CURRENTattrSELECTED64(CDF)) return NO_ATTR_SELECTED;
    if (!sX(ReadADR64(CDF->fp,CDF->CURattrOffset64,
		      ADR_SCOPE,&tScope,
		      ADR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    *scope = DEFINITEscope(tScope);
    break;
  }
  /****************************************************************************
  * ATTR_MAXgENTRY_/ATTR_MAXrENTRY_/ATTR_MAXzENTRY_
  * ATTR_NUMgENTRIES_/ATTR_NUMrENTRIES_/ATTR_NUMzENTRIES_
  ****************************************************************************/
  case ATTR_MAXgENTRY_:
  case ATTR_NUMgENTRIES_:
  case ATTR_MAXrENTRY_:
  case ATTR_NUMrENTRIES_:
  case ATTR_MAXzENTRY_:
  case ATTR_NUMzENTRIES_: {
    Logical maxOp = ONEof3(Va->item,ATTR_MAXgENTRY_,
				    ATTR_MAXrENTRY_,
				    ATTR_MAXzENTRY_);
    int entryType = BOO(maxOp,E3p(Va->item,ATTR_MAXgENTRY_,
					   ATTR_MAXrENTRY_,
					   ATTR_MAXzENTRY_),
			      E3p(Va->item,ATTR_NUMgENTRIES_,
					   ATTR_NUMrENTRIES_,
					   ATTR_NUMzENTRIES_));
    struct CDFstruct *CDF;
    long *value = va_arg (Va->ap, long *);
    Int32 scope, gr, z;
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTattrSELECTED64(CDF)) return NO_ATTR_SELECTED;
    if (!sX(ReadADR64(CDF->fp,CDF->CURattrOffset64,
		      ADR_SCOPE,&scope,
		      ADR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (GLOBALscope(scope)) {
      if (entryType != gENTRYt) return ILLEGAL_FOR_SCOPE;
    }
    else {
      if (entryType == gENTRYt) return ILLEGAL_FOR_SCOPE;
    }
    if (!sX(ReadADR64(CDF->fp,CDF->CURattrOffset64,
		      BOO(maxOp,ADR_MAXgrENTRY,ADR_NgrENTRIES),&gr,
		      BOO(maxOp,ADR_MAXzENTRY,ADR_NzENTRIES),&z,
		      ADR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (GLOBALscope(scope))
      *value = (long) gr;
    else
      if (zModeON(CDF))
	if (entryType == rENTRYt)
	  *value = (long) BOO(maxOp,NO_ENTRY,0);
					/* Never any rEntries in zMode. */
	else
	  *value = (long) BOO(maxOp,BOO(z > NO_ENTRY,CDF->NrVars+z,gr),gr+z);
      else
	*value = (long) E3(entryType,BOO(maxOp,NO_ENTRY,0),gr,z);
    break;
  }
  /****************************************************************************
  * gENTRY_DATATYPE_/rENTRY_DATATYPE_/zENTRY_DATATYPE_
  * gENTRY_NUMELEMS_/rENTRY_NUMELEMS_/zENTRY_NUMELEMS_
  *    If this CDF is prior to CDF V2.1.1, the current attribute is named
  * "VALIDMIN", "VALIDMAX", "SCALEMIN", or "SCALEMAX", this is a true rEntry,
  * and the rEntry corresponds to an rVariable named "EPOCH", then return
  * the CDF_EPOCH data type if the actual data type is CDF_REAL8 or CDF_DOUBLE.
  * (The CDF_EPOCH data type was not introduced until CDF V2.1.1).  Note that
  * only rVariables were supported prior to CDF V2.3.
  ****************************************************************************/
  case gENTRY_DATATYPE_:
  case gENTRY_NUMELEMS_:
  case rENTRY_DATATYPE_:
  case rENTRY_NUMELEMS_:
  case zENTRY_DATATYPE_:
  case zENTRY_NUMELEMS_: {
    Logical dataOp = ONEof3(Va->item,gENTRY_DATATYPE_,
				     rENTRY_DATATYPE_,
				     zENTRY_DATATYPE_);
    int entryType = BOO(dataOp,E3p(Va->item,gENTRY_DATATYPE_,
					    rENTRY_DATATYPE_,
					    zENTRY_DATATYPE_),
			       E3p(Va->item,gENTRY_NUMELEMS_,
					    rENTRY_NUMELEMS_,
					    zENTRY_NUMELEMS_));
    struct CDFstruct *CDF;
    long *value = va_arg (Va->ap, long *);
    Int32 tValue;
    OFF_T eOffset;
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTattrSELECTED64(CDF)) return NO_ATTR_SELECTED;
    if (E3(entryType,
	   CDF->CURgrEntryNum,
	   CDF->CURgrEntryNum,
	   CDF->CURzEntryNum) == RESERVED_ENTRYNUM) return NO_ENTRY_SELECTED;
    if (!sX(CheckEntryOp64(CDF,entryType),&pStatus)) return pStatus;
    eOffset = E3(entryType,CDF->CURgrEntryOffset64,
			   CDF->CURgrEntryOffset64,
			   CDF->CURzEntryOffset64);
    if (eOffset == (OFF_T) RESERVED_ENTRYOFFSET64) return NO_SUCH_ENTRY;
    if (!sX(ReadAEDR64(CDF->fp,eOffset,
		       BOO(dataOp,AEDR_DATATYPE,AEDR_NUMELEMS),&tValue,
		       AEDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (dataOp && CDF->fakeEPOCH) {
      char aName[CDF_ATTR_NAME_LEN256+1];
      if (!sX(ReadADR64(CDF->fp,CDF->CURattrOffset64,
		        ADR_NAME,aName,
		        ADR_NULL),&pStatus)) {
	AbortAccess64 (CDF, UPDATE, noDELETE);
	return pStatus;
      }
      if (!strcmpITB(aName,"VALIDMIN") || !strcmpITB(aName,"VALIDMAX") ||
	  !strcmpITB(aName,"SCALEMIN") || !strcmpITB(aName,"SCALEMAX")) {
	char vName[CDF_VAR_NAME_LEN256+1];
	OFF_T vOffset;
	tStatus = FindVarByNumber64 (CDF, E3(entryType, CDF->CURgrEntryNum,
							CDF->CURgrEntryNum,
							CDF->CURzEntryNum),
				     FALSE, &vOffset);
	switch (tStatus) {
	  case NO_SUCH_VAR:
	    break;
	  default:
	    if (!sX(tStatus,&pStatus)) {
	      AbortAccess64 (CDF, UPDATE, noDELETE);
	      return tStatus;
	    }
	    if (!sX(ReadVDR64(CDF,CDF->fp,vOffset,FALSE,
			      VDR_NAME,vName,
			      VDR_NULL),&pStatus)) {
	      AbortAccess64 (CDF, UPDATE, noDELETE);
	      return pStatus;
	    }
	    if (!strcmpITB(vName,"EPOCH") &&
		FLOAT8dataType(tValue)) tValue = CDF_EPOCH;
	    if (!strcmpITB(vName,"EPOCH") &&
                FLOAT16dataType(tValue)) tValue = CDF_EPOCH16;
	    break;
	}
      }
    }
    *value = tValue;
    break;
  }
  /****************************************************************************
  * gENTRY_DATA_/rENTRY_DATA_/zENTRY_DATA_, 
  ****************************************************************************/
  case gENTRY_DATA_:
  case rENTRY_DATA_:
  case zENTRY_DATA_: {
    int entryType = E3p(Va->item,gENTRY_DATA_,rENTRY_DATA_,zENTRY_DATA_);
    struct CDFstruct *CDF;
    Int32 dataType, numElems;
    OFF_T offset;
    void *value = va_arg (Va->ap, void *);
    SelectCDF (Cur->cdf, CDF)
    if (!CURRENTattrSELECTED64(CDF)) return NO_ATTR_SELECTED;
    if (E3(entryType,
	   CDF->CURgrEntryNum,
	   CDF->CURgrEntryNum,
	   CDF->CURzEntryNum) == RESERVED_ENTRYNUM) return NO_ENTRY_SELECTED;
    if (!sX(CheckEntryOp64(CDF,entryType),&pStatus)) return pStatus;
    offset = E3(entryType,CDF->CURgrEntryOffset64,
			  CDF->CURgrEntryOffset64,
			  CDF->CURzEntryOffset64);
    if (offset == (OFF_T) RESERVED_ENTRYOFFSET64) return NO_SUCH_ENTRY;
    if (!sX(ReadAEDR64(CDF->fp,offset,
		       AEDR_DATATYPE,&dataType,
		       AEDR_NUMELEMS,&numElems,
		       AEDR_VALUE,value,
		       AEDR_NULL),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    if (!sX(ConvertBuffer(CDF->encoding,CDF->decoding,
			  CDF->negToPosFp0,dataType,
			  numElems,value,value),&pStatus)) {
      AbortAccess64 (CDF, UPDATE, noDELETE);
      return pStatus;
    }
    break;
  }
  /****************************************************************************
  * Unknown item, must be the next function.
  ****************************************************************************/
  default: {
    Va->fnc = Va->item;
    break;
  }
}
return pStatus;
}
