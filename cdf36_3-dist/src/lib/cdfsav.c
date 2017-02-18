/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                           CDF `save' operations.
*
*  Version 1.0, 31-Jan-06, Hughes STX.
*
*  Modification history:
*
*   V1.0  31-Jan-06, M Liu     Original version 
*
******************************************************************************/

#include "cdflib.h"
#include "cdflib64.h"

/******************************************************************************
* CDFsav.
******************************************************************************/

STATICforIDL CDFstatus CDFsav (Va, Cur)
struct VAstruct *Va;
struct CurStruct *Cur;
{
  CDFstatus pStatus = CDF_OK;
  int ix;

  switch (Va->item) {
    /**************************************************************************
    * CDF_
    *   Save a CDF for the dotCDF file.
    **************************************************************************/
    case CDF_: {
      struct CDFstruct *CDF; 
      struct VarStruct *Var;
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
      * If the CDF is open read/write, update the dotCDF file.
      ************************************************************************/
      if (CDF->status == READ_WRITE) {
        if (!CDF->largeFile) {
	  if (!sX(UpdateDotCDF(CDF),&pStatus)) {
	    AbortAccess (CDF, noUPDATE, noDELETE);
	    KillAbortedCDF (CDF, Cur);
	    return pStatus;
          }
        } else {
          if (!sX(UpdateDotCDF64(CDF),&pStatus)) {
            AbortAccess64 (CDF, noUPDATE, noDELETE);
            KillAbortedCDF (CDF, Cur);
            return pStatus;
          }
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
          Int32 CPRoffset; struct CPRstruct CPR;
          OFF_T CPRoffset64; struct CPRstruct64 CPR64;

          /********************************************************************
          * Write cache buffers.  
          ********************************************************************/
          if (!CDF->largeFile) {
            if (!FlushCache(CDF->uDotFp,CDF->uDotFp->cacheHead)) 
	      return CDF_WRITE_ERROR;
          } else {
            if (!FlushCache64(CDF->uDotFp,CDF->uDotFp->cacheHead))
              return CDF_WRITE_ERROR;
          }
	  /********************************************************************
	  * Flush the uncompressed dotCDF file.
	  ********************************************************************/
          if (!CDF->largeFile) {
	    if (!FLUSHv(CDF->uDotFp)) {
	      AbortAccess (CDF, noUPDATE, noDELETE);
	      KillAbortedCDF (CDF, Cur);
	      return CDF_WRITE_ERROR;
            }
          } else {
            if (!FLUSHv64(CDF->uDotFp)) {
              AbortAccess64 (CDF, noUPDATE, noDELETE);
              KillAbortedCDF (CDF, Cur);
              return CDF_WRITE_ERROR;
            } 
	  }
          /********************************************************************
          * Read the compression parameters from the compressed dotCDF file.
          ********************************************************************/
          if (!CDF->largeFile) {
            if (!sX(ReadCCR(CDF->dotFp,V2_CCR_OFFSET,
                            CCR_CPROFFSET,&CPRoffset,
                            CCR_NULL),&pStatus)) {
              AbortAccess (CDF, noUPDATE, noDELETE);
              KillAbortedCDF (CDF, Cur);
              return pStatus;
            }
            if (!sX(ReadCPR(CDF->dotFp,CPRoffset,
                            CPR_RECORD,&CPR,
                            CPR_NULL),&pStatus)) {
              AbortAccess (CDF, noUPDATE, noDELETE);
              KillAbortedCDF (CDF, Cur);
              return pStatus;
            }
          } else {
            if (!sX(ReadCCR64(CDF->dotFp,V3_CCR_OFFSET64,
                              CCR_CPROFFSET,&CPRoffset64,
                              CCR_NULL),&pStatus)) {
              AbortAccess64 (CDF, noUPDATE, noDELETE);
              KillAbortedCDF (CDF, Cur);
              return pStatus;
            }
            if (!sX(ReadCPR64(CDF->dotFp,CPRoffset64,
                              CPR_RECORD,&CPR64,
                              CPR_NULL),&pStatus)) {
              AbortAccess64 (CDF, noUPDATE, noDELETE);
              KillAbortedCDF (CDF, Cur);
              return pStatus;
            }
          }
          /********************************************************************
          * Delete the compressed dotCDF file.
          ********************************************************************/
          if (!CDF->largeFile) {
            if (!DELETEv(CDF->dotFp,NULL)) {
              CDF->dotFp = NULL;
              AbortAccess (CDF, noUPDATE, noDELETE);
              KillAbortedCDF (CDF, Cur);
              return CDF_CLOSE_ERROR;
            }
          } else {
            if (!DELETEv64(CDF->dotFp,NULL)) {
              CDF->dotFp = NULL;
              AbortAccess64 (CDF, noUPDATE, noDELETE);
              KillAbortedCDF (CDF, Cur);
              return CDF_CLOSE_ERROR;
            }
          }
          CDF->dotFp = NULL;
          /********************************************************************
          * Recreate the compressed dotCDF file.
          ********************************************************************/
          BuildFilePath (CDFt, CDF->CDFname, CDF->no_append,
                         CDF->upper_case_ext, CDF->version_numbers,
                         INT32_ZERO, pathName);
          if (!CDF->largeFile) {
            CDF->dotFp = V_open (pathName, WRITE_PLUS_a_mode);
            if (CDF->dotFp == NULL) {
              AbortAccess (CDF, noUPDATE, noDELETE);
              KillAbortedCDF (CDF, Cur);
              return CDF_CREATE_ERROR;
            }
          } else {
            CDF->dotFp = V_open64 (pathName, WRITE_PLUS_a_mode);
            if (CDF->dotFp == NULL) {
              AbortAccess64 (CDF, noUPDATE, noDELETE);
              KillAbortedCDF (CDF, Cur);
              return CDF_CREATE_ERROR;
            }
          } 
          /********************************************************************
          * Write the compressed dotCDF file.
          ********************************************************************/
          if (!CDF->largeFile) {
            if (!sX(WriteCompressedCDF(CDF,&CPR,notEMPTY),&pStatus)) {
              AbortAccess (CDF, noUPDATE, noDELETE);
              KillAbortedCDF (CDF, Cur);
              return pStatus;
            }
            if (!FlushCache(CDF->dotFp,CDF->dotFp->cacheHead))
              return CDF_WRITE_ERROR;
            if (!FLUSHv(CDF->dotFp)) {
              AbortAccess (CDF, noUPDATE, noDELETE);
              KillAbortedCDF (CDF, Cur);
              return CDF_WRITE_ERROR;
            }
          } else {
            if (!sX(WriteCompressedCDF64(CDF,&CPR64,notEMPTY),&pStatus)) {
              AbortAccess64 (CDF, noUPDATE, noDELETE);
              KillAbortedCDF (CDF, Cur);
              return pStatus;
            }
            if (!FlushCache64(CDF->dotFp,CDF->dotFp->cacheHead))
              return CDF_WRITE_ERROR;
            if (!FLUSHv64(CDF->dotFp)) {
              AbortAccess64 (CDF, noUPDATE, noDELETE);
              KillAbortedCDF (CDF, Cur);
              return CDF_WRITE_ERROR;
            }
          }
	}
      }
      else {
        /**********************************************************************
	* CDF - Not compressed.
	**********************************************************************/
        if (CDF->status == READ_WRITE) {
	/**********************************************************************
	* Write cache buffers.  
	**********************************************************************/
        if (!CDF->largeFile) {
	  if (!FlushCache(CDF->dotFp,CDF->dotFp->cacheHead)) 
	      return CDF_WRITE_ERROR;
            if (!FLUSHv(CDF->dotFp)) {
              AbortAccess (CDF, noUPDATE, noDELETE);
              KillAbortedCDF (CDF, Cur);
              return CDF_WRITE_ERROR;
            }
          } else {
            if (!FlushCache64(CDF->dotFp,CDF->dotFp->cacheHead))
              return CDF_WRITE_ERROR;
            if (!FLUSHv64(CDF->dotFp)) {
              AbortAccess64 (CDF, noUPDATE, noDELETE);
              KillAbortedCDF (CDF, Cur);
              return CDF_WRITE_ERROR;
            }
          }
        }
        /**********************************************************************
        * Multi-format file....
        * If the CDF is open read/write, update the dotCDF file.
        **********************************************************************/
        if (!CDF->singleFile) {
          if (CDF->status == READ_WRITE) {
            for (ix = 0; ix < CDF->NrVars; ix++) {
              Var = CDF->rVars[ix];
              if (Var != NULL) {
                if (Var->fp != NULL) {
                  if (!CDF->largeFile) {
                    if (!FLUSHv(Var->fp)) {
                      AbortAccess (CDF, UPDATE, noDELETE);
                      return VAR_WRITE_ERROR;
                    }
                  } else {
                    if (!FLUSHv64(Var->fp)) {
                      AbortAccess64 (CDF, UPDATE, noDELETE);
                      return VAR_WRITE_ERROR;
                    }
                  }
                }
              }
            }
            for (ix = 0; ix < CDF->NzVars; ix++) {
              Var = CDF->zVars[ix];
              if (Var != NULL) {
                if (Var->fp != NULL) {
                  if (!CDF->largeFile) {
                    if (!FLUSHv(Var->fp)) {
                      AbortAccess (CDF, UPDATE, noDELETE);
                      return VAR_WRITE_ERROR;
                    }
                  } else {
                    if (!FLUSHv64(Var->fp)) {
                      AbortAccess64 (CDF, UPDATE, noDELETE);
                      return VAR_WRITE_ERROR;
                  }
                } 
              }
            }
          }
        }
        }
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
