/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                           CDF `close' operations.
*
*  Version 1.5a, 21-Feb-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  20-May-92, J Love     Original version (was part of `cdflib.c').
*   V1.1  21-Aug-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V1.2   7-Dec-93, J Love     CDF V2.4.  Added zMode.
*   V1.3  15-Nov-94, J Love     CDF V2.5.
*   V1.3a 24-Feb-95, J Love	Solaris 2.3 IDL i/f.
*   V1.4  20-Jul-95, J Love	CDFexport-related changes.
*   V1.5   8-Aug-96, J Love	CDF V2.6.
*   V1.4a 21-Feb-97, J Love	Removed RICE.
*   V2.0  08-Apr-04, M Liu      Replaced VSTREAM.STATS with VSTREAM_STATS.
*                               Removed call to LocateCurrentVar when r/zVAR_
*                               is closed as it does more than needed.
*   V2.1  29-Jun-04, M Liu      Added LFS (Large File Support > 2G).
*
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"

/******************************************************************************
* CDFclo64.
******************************************************************************/

STATICforIDL CDFstatus CDFclo64 (Va, Cur)
struct VAstruct *Va;
struct CurStruct *Cur;
{
  CDFstatus pStatus = CDF_OK;
  switch (Va->item) {
    /**************************************************************************
    * CDF_
    *   Close a CDF with or without statistics for the dotCDF file.
    **************************************************************************/
    case CDF_:
    case CDFwithSTATS_: {
      struct CDFstruct *CDF; vSTATS vStats;
      Logical statistics = (Va->item == CDFwithSTATS_);
      vSTATS *vStatsDotP = BOO(statistics,va_arg(Va->ap,vSTATS *),NULL);
      vSTATS *vStatsStageP = BOO(statistics,va_arg(Va->ap,vSTATS *),NULL);
      vSTATS *vStatsCompressP = BOO(statistics,va_arg(Va->ap,vSTATS *),NULL);
      /************************************************************************
      * Initialize the Vstream statistics.
      ************************************************************************/
      AddTOvStats (vStatsDotP, NULL);
      AddTOvStats (vStatsStageP, NULL);
      AddTOvStats (vStatsCompressP, NULL);
      /************************************************************************
      * Validate the current CDF.  Don't use `SelectCDF' here because we don't
      * want to return NO_MORE_ACCESS if that is the case.
      ************************************************************************/
      if (Cur->cdf == NULL)
	return NO_CDF_SELECTED;
      else
	CDF = Cur->cdf;
      /************************************************************************
      * If this is a CDF for which access was aborted, simply free the CDF's
      * memory.
      ************************************************************************/
      if (CDF->magic == ABORTEDid_MAGIC_NUMBER) {
	KillAbortedCDF (CDF, Cur);
	break;
      }
      /************************************************************************
      * Get the Vstream statistics for the dotCDF file(s) so far.
      ************************************************************************/
      AddTOvStats (vStatsDotP, &CDF->dotCDFvStats);
      AddTOvStats (vStatsDotP, &CDF->uDotCDFvStats);
      /************************************************************************
      * If the CDF is open read/write, update the dotCDF file to flush out
      * the (compressed) data in the staging area..
      ************************************************************************/
      if (CDF->status == READ_WRITE) {
	if (!sX(UpdateDotCDF64(CDF),&pStatus)) {
	  AbortAccess64 (CDF, noUPDATE, noDELETE);
	  KillAbortedCDF (CDF, Cur);
	  return pStatus;
	}
      }
      /************************************************************************
      * If the CDF is compressed...
      ************************************************************************/
      if (CDF->uDotFp != NULL) {
	/**********************************************************************
	* If the CDF is open read/write...
	**********************************************************************/
	if (CDF->status == READ_WRITE) {
	  char pathName[DU_MAX_PATH_LEN+1];
	  OFF_T CPRoffset; struct CPRstruct64 CPR;
	  /********************************************************************
	  * First flush the uncompressed dotCDF file.
	  ********************************************************************/
	  if (!FLUSHv64(CDF->uDotFp)) {
	    AbortAccess64 (CDF, noUPDATE, noDELETE);
	    KillAbortedCDF (CDF, Cur);
	    return CDF_WRITE_ERROR;
	  }
	  /********************************************************************
	  * Read the compression parameters from the compressed dotCDF file.
	  ********************************************************************/
	  if (!sX(ReadCCR64(CDF->dotFp,V3_CCR_OFFSET64,
			    CCR_CPROFFSET,&CPRoffset,
			    CCR_NULL),&pStatus)) {
	    AbortAccess64 (CDF, noUPDATE, noDELETE);
	    KillAbortedCDF (CDF, Cur);
	    return pStatus;
	  }
	  if (!sX(ReadCPR64(CDF->dotFp,CPRoffset,
			    CPR_RECORD,&CPR,
			    CPR_NULL),&pStatus)) {
	    AbortAccess64 (CDF, noUPDATE, noDELETE);
	    KillAbortedCDF (CDF, Cur);
	    return pStatus;
	  }
	  /********************************************************************
	  * Delete the compressed dotCDF file.
	  ********************************************************************/
	  if (!DELETEv64(CDF->dotFp,&vStats)) {
	    CDF->dotFp = NULL;
	    AbortAccess64 (CDF, noUPDATE, noDELETE);
	    KillAbortedCDF (CDF, Cur);
	    return CDF_CLOSE_ERROR;
	  }
	  CDF->dotFp = NULL;
	  AddTOvStats (vStatsDotP, &vStats);
#if defined(DEBUG)
	  DisplayVs (getenv("VSTREAM_STATS"), "DotCDF..", &vStats);
#endif
	  /********************************************************************
	  * Recreate the compressed dotCDF file.
	  ********************************************************************/
	  BuildFilePath (CDFt, CDF->CDFname, CDF->no_append,
			 CDF->upper_case_ext, CDF->version_numbers,
			 INT32_ZERO, pathName);
	  CDF->dotFp = V_open64 (pathName, WRITE_PLUS_a_mode);
	  if (CDF->dotFp == NULL) {
	    AbortAccess64 (CDF, noUPDATE, noDELETE);
	    KillAbortedCDF (CDF, Cur);
	    return CDF_CREATE_ERROR;
	  }
	  /********************************************************************
	  * Write the compressed dotCDF file.
	  ********************************************************************/
	  if (!sX(WriteCompressedCDF64(CDF,&CPR,notEMPTY),&pStatus)) {
	    AbortAccess64 (CDF, noUPDATE, noDELETE);
	    KillAbortedCDF (CDF, Cur);
	    return pStatus;
	  }
	  /********************************************************************
	  * Close the compressed dotCDF file.
	  ********************************************************************/
	  if (!CLOSEv64(CDF->dotFp,CDF,&vStats)) {
	    CDF->dotFp = NULL;
	    AbortAccess64 (CDF, noUPDATE, noDELETE);
	    KillAbortedCDF (CDF, Cur);
	    return CDF_CLOSE_ERROR;
	  }
	  CDF->dotFp = NULL;
	  AddTOvStats (vStatsDotP, &vStats);
#if defined(DEBUG)
	  DisplayVs (getenv("VSTREAM_STATS"), "DotCDF..", &vStats);
#endif
	  /********************************************************************
	  * Delete the uncompressed dotCDF file.
	  ********************************************************************/
	  if (!DELETEv64(CDF->uDotFp,&vStats)) {
	    CDF->uDotFp = NULL;
	    AbortAccess64 (CDF, noUPDATE, noDELETE);
	    KillAbortedCDF (CDF, Cur);
	    return CDF_CLOSE_ERROR;
	  }
	  CDF->uDotFp = NULL;
	  AddTOvStats (vStatsDotP, &vStats);
#if defined(DEBUG)
	  DisplayVs (getenv("VSTREAM_STATS"), "uDotCDF.", &vStats);
#endif
	}
	else {
	  /********************************************************************
	  * The CDF is open read/only.  First close the compressed dotCDF file
	  * (which has not been changed).
	  ********************************************************************/
          if (CDF->readOnly) ResetReadOnlyState64(CDF);
	  if (!CLOSEv64(CDF->dotFp,CDF,&vStats)) {
	    CDF->dotFp = NULL;
	    AbortAccess64 (CDF, noUPDATE, noDELETE);
	    KillAbortedCDF (CDF, Cur);
	    return CDF_CLOSE_ERROR;
	  }
	  CDF->dotFp = NULL;
	  AddTOvStats (vStatsDotP, &vStats);
#if defined(DEBUG)
	  DisplayVs (getenv("VSTREAM_STATS"), "DotCDF..", &vStats);
#endif
	  /********************************************************************
	  * Delete the uncompressed dotCDF file.
	  ********************************************************************/
	  if (!DELETEv64(CDF->uDotFp,&vStats)) {
	    CDF->uDotFp = NULL;
	    AbortAccess64 (CDF, noUPDATE, noDELETE);
	    KillAbortedCDF (CDF, Cur);
	    return CDF_CLOSE_ERROR;
	  }
	  CDF->uDotFp = NULL;
	  AddTOvStats (vStatsDotP, &vStats);
#if defined(DEBUG)
	  DisplayVs (getenv("VSTREAM_STATS"), "uDotCDF.", &vStats);
#endif
	}
      }
      else {
        /**********************************************************************
	* Not compressed - first close the dotCDF file.
	**********************************************************************/
        if (CDF->readOnly) ResetReadOnlyState64(CDF);
	if (!CLOSEv64(CDF->dotFp,CDF,&vStats)) {
	  CDF->dotFp = NULL;
	  AbortAccess64 (CDF, noUPDATE, noDELETE);
	  KillAbortedCDF (CDF, Cur);
	  return CDF_CLOSE_ERROR;
	}
	CDF->dotFp = NULL;
	AddTOvStats (vStatsDotP, &vStats);
#if defined(DEBUG)
	DisplayVs (getenv("VSTREAM_STATS"), "DotCDF..", &vStats);
#endif
	/**********************************************************************
	* Close the variable files (if a multi-file CDF).
	**********************************************************************/
	if (!sX(CloseVarFiles64(CDF),&pStatus)) {
	  AbortAccess64 (CDF, noUPDATE, noDELETE);
	  KillAbortedCDF (CDF, Cur);
	  return pStatus;
	}
      }
      /************************************************************************
      * Delete staging scratch file.
      ************************************************************************/
      if (CDF->stage.fp != NULL) {
	if (!DELETEv64(CDF->stage.fp,&vStats)) {
	  CDF->stage.fp = NULL;
	  AbortAccess64 (CDF, noUPDATE, noDELETE);
	  KillAbortedCDF (CDF, Cur);
	  return SCRATCH_DELETE_ERROR;
	}
	CDF->stage.fp = NULL;
	AddTOvStats (vStatsStageP, &vStats);
#if defined(DEBUG)
	DisplayVs (getenv("VSTREAM_STATS"), "Stage...", &vStats);
#endif
      }
      /************************************************************************
      * Delete compression scratch file.
      ************************************************************************/
      if (CDF->compressFp != NULL) {
	if (!DELETEv64(CDF->compressFp,&vStats)) {
	  CDF->compressFp = NULL;
	  AbortAccess64 (CDF, noUPDATE, noDELETE);
	  KillAbortedCDF (CDF, Cur);
	  return SCRATCH_DELETE_ERROR;
	}
	CDF->compressFp = NULL;
	AddTOvStats (vStatsCompressP, &vStats);
#if defined(DEBUG)
	DisplayVs (getenv("VSTREAM_STATS"), "Compress", &vStats);
#endif
      }
      /************************************************************************
      * Free memory used and clear current CDF.
      ************************************************************************/
      FreeCDFid (CDF, FALSE);
      Cur->cdf = NULL;
      ReduceOpenCDFsCount();
      break;
    }
    /**************************************************************************
    * rVAR_/zVAR_
    *    Close the current r/zVariable file.
    **************************************************************************/
    case rVAR_:
    case zVAR_: {
      Logical zOp = (Va->item == zVAR_);
      struct CDFstruct *CDF; struct VarStruct *Var;
      SelectCDF (Cur->cdf, CDF)
      if (BADzOP(CDF,!zOp)) return ILLEGAL_IN_zMODE;
      if (!CURRENTvarSELECTED(CDF,zOp)) return NO_VAR_SELECTED;

      if (zModeON(CDF)) {
        if (CDF->CURzVarNum < CDF->NrVars)
          Var = CDF->rVars[(int)CDF->CURzVarNum];
        else
          Var = CDF->zVars[(int)(CDF->CURzVarNum - CDF->NrVars)];
      } else
        Var = BOO(zOp,CDF->zVars[(int)CDF->CURzVarNum],
                      CDF->rVars[(int)CDF->CURrVarNum]);

      if (CDF->singleFile) {
	sX (SINGLE_FILE_FORMAT, &pStatus);
	break;
      }
      if (Var != NULL) {
	if (Var->fp != NULL) {
	  if (!CLOSEv64(Var->fp,NULL,NULL)) {
	    Var->fp = NULL;
	    AbortAccess64 (CDF, UPDATE, noDELETE);
	    return VAR_CLOSE_ERROR;
	  }
	  Var->fp = NULL;
	}
	else {
	  if (!sX(VAR_ALREADY_CLOSED,&pStatus)) return pStatus;
	}
      }
      else {
	if (!sX(VAR_ALREADY_CLOSED,&pStatus)) return pStatus;
      }
      break;
    }
    /**************************************************************************
    * Unknown item, must be the next function.
    **************************************************************************/
    default: {
      Va->fnc = Va->item;
      break;
    }
  }
  return pStatus;
}
