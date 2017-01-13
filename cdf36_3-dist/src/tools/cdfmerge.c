/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                 AddAttributes - Add variable attributes.
*
*  Version 1.0, 9-Jul-06, Raytheon.
*
*  Modification history:
*
*   V1.0   09-Jul-06, M Liu      Original version.
*   V1.1   08-Mar-07, D Han      Fixed the bug with "cdfmerge -java" option.
*                                Fixed the memory free problem (prefixes)
*   V1.2   05-Aug-08, M Liu      Modified to handle variable "Epoch" 
*                                differently from "Epoch_RANGE". "Epoch" is 
*                                handled just like other variables.
*   V3.3   10-Jan-09, M Liu      Validate a file before it is used.
*   V3.5   10-Sep-12, M Liu      Changed global variable: FileSource to ISTP
*                                defined "Parent".
*   V3.5.1 01-Jul-14, M Liu      Added TT2000.
*
******************************************************************************/

#include "cdfmerge.h"
static CDFid idm, id, ido;
static int sp1gAttrs = 1, sp2gAttrs = 3, sp3gAttrs = 6, p1vAttrs = 25,
           sp2vAttrs = 11;
static char *special1gAttrs[] = {"Parent"};
/*
static char *special2gAttrs[] = {"Data_type", "Data_version",
                                 "Logical_source", "Logical_file_id",
                                 "Generation_date"};
*/
static char *special2gAttrs[] = {"Logical_source", "Logical_file_id",
                                 "Generation_date"};
static char *special3gAttrs[] = {"ADID_ref", "LINK_TEXT", "TEXT",
                                 "LINK_TITLE", "HTTP_LINK", "Rules_of_use"};
/*
static char *special1vAttrs[] = {"FIELDNAM", "VALIDMIN", "VALIDMAX", "SCALEMIN",
                                 "SCALEMAX", "LABLAXIS", "UNITS",    "FORMAT",
                                 "MONOTON",  "VAR_TYPE", "DICT_KEY", "FILLVAL",
                                 "LABL_PTR_", "UNIT_PTR", "FORM_PTR", "DEPEND_",
                                 "CATDESC",  "DELTA_PLUS_VAR",
                                 "DELTA_MINUS_VAR",      "AVG_TYPE",
                                 "DISPLAY_TYPE",         "VAR_NOTES",
                                 "SCAL_PTR", "DERIVN",   "V_PARENT"};
*/
static char *special2vAttrs[] = {
       "LABL_PTR_", "FORM_PTR", "UNIT_PTR", "DEPEND_",
       "SCAL_PTR",  "DERIVN",   "V_PARENT", "COMPONENT_",
       "OFFSET_",   "EPOCH",    "RANGE_EPOCH"
};
static char *validQuals[] = {
  "prefixes", "noprefix", "about", "log", "nolog", "file", "dataonly",
  "nodataonly", "cdaweb", "nocdaweb", "master", "nomaster", "augment_label", 
  "noaugment_label", NULL
};
static char *dspType = "DISPLAY_TYPE";
static int optRequired[] = {
  TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, 
  FALSE, FALSE, FALSE, 0
};

static CDFstatus status, status1;
static long numDims, dimSizes[CDF_MAX_DIMS], dimVarys[CDF_MAX_DIMS];
static long majority, encoding, format,
       numzVars, numAttrs, numgAttrs, numvAttrs, numgEntries, numigEntries,
       numzEntries, major, reservePct;
static long varNum, attrNum, dataType, numElems, scope, recVary, dataTypeX, numElemsX,
       dataTypeY, numElemsY, numVars;
static long cType, cParms[CDF_MAX_DIMS], cPct;
static long maxRec, varMaxRec, sp, bf, maxAllocRec, fromRec, toRec, fromRec2, toRec2,
       nRecords, hyperStep;
static long nValuesPerRec, maxGentry = 0;
static char **CDFpaths, **varNames;
static char outputCDF[DU_MAX_PATH_LEN+1], fileList[DU_MAX_PATH_LEN+1],
       line[DU_MAX_PATH_LEN+1], sourceCDF[DU_MAX_PATH_LEN+1],
       masterFile[DU_MAX_PATH_LEN+1], mytmp[DU_MAX_PATH_LEN+1];
static char varName[CDF_VAR_NAME_LEN256+1], attrName[CDF_ATTR_NAME_LEN256+1];
static char newVarName[CDF_VAR_NAME_LEN256+1], newAttrName[CDF_ATTR_NAME_LEN256+1],
       newVarName2[CDF_VAR_NAME_LEN256+1];
static char prefix[1025], entryValue[257], *newValue;
static char epochName[CDF_VAR_NAME_LEN256+1];
static void *value;
static char *value1 = NULL;
static size_t nBytes;
static Logical mLog, pad, dataOnly, usePrefix = TRUE, changed, first = TRUE;
static Logical cdaweb, master = FALSE, zVar;
static Logical augmentlabel, augmentlabelExist = FALSE;
static Logical srcRowMajor, switchMajority;
static Logical masterCDF;
static int  i, ii, j, k, ir, icount, icountx, numCDFs, ix, iy, dimN, recNum;
static int numOpt, numTokens = 0, count;
static Logical qopError = FALSE;
static QOP *qop;
static Byte1 **handles[2], *buffer1, *buffer2;
static size_t nValueBytes[2];
static long nHypers, nValues, hyperN;
static struct GroupStruct groups;
static struct HyperStruct hyper;
static Logical ifound;
static int whichEpoch2 = 0;
static double minmaxsEpoch[2], minsEpoch = 0.0, maxsEpoch;
static double minmaxdEpoch[2][2], mindEpoch[2] = {0.0, 0.0}, maxdEpoch[2];
static long long minmaxTT2000[2], minTT2000 = FILLED_TT2000_VALUE, maxTT2000;
static FILE *srcFp;
static char **attrs, **entries;
static int  *inentries;
static int lineAttrs = 0, lineEntries = 0;
static time_t bintim;
static char generationDate[11];
static int errorP = 0;
static long numx;

/******************************************************************************
* Main.
******************************************************************************/

#if !defined(win32) || (defined(win32) && defined(ALONE))
MAIN {
  Logical success = TRUE;
  strcpyX (pgmName, "CDFmerge", MAX_PROGRAM_NAME_LEN);
#if defined(mac)
  MacExecuteSO (MergeCDFs, InquireQOPs);
#else
  success = MergeCDFs (argc, argv);
#endif
#if defined(DEBUG)
  if (cdf_FreeMemory (NULL,FatalError) > 0) DisplayWarning ("Abandoned buffers.");
#else
  cdf_FreeMemory (NULL, FatalError);
#endif
  return BOO(success,EXIT_SUCCESS_,EXIT_FAILURE_);
}
#endif

/******************************************************************************
* MergeCDFs.
******************************************************************************/

Logical MergeCDFs (argC, argV)
int argC;
char *argV[];
{

Logical status2;
time (&bintim);
strftime (generationDate, 11, "%Y-%m-%d",gmtime(&bintim));

/* CDFsetValidate (VALIDATEFILEon); */

/******************************************************************************
* Parse qualifiers/options/parameters (QOP).
******************************************************************************/

switch (argC) {
  case 1:
    PageOLH ("cdfmerge.olh", argV[0]);
    return TRUE;
  case 2:
    if (strcmp(argV[1],"-java") == 0) {
        pagingOn = FALSE;
        PageOLH ("cdfmergej.olh", argV[0]);
        return TRUE;
    }
  default:
    qop = Qop (argC, argV, validQuals, optRequired);
    if (qop == NULL) return FALSE;
    /************************************************************************
    * Check for `about' qualifier.
    ************************************************************************/
    if (qop->qualEntered[ABOUTqual]) {
      DisplayIdentification (pgmName);
      cdf_FreeMemory (qop, FatalError);
      return TRUE;
    }
    /************************************************************************
    * Check for `log' qualifier.
    ************************************************************************/
    qopError = qopError | !TFqualifier (qop,&mLog,LOGqual,NOLOGqual,
                                        DEFAULTlogMERGE,"log"); 
    /************************************************************************
    * Check for `dataonly' qualifier. 
    ************************************************************************/
    qopError = qopError | !TFqualifier (qop,&dataOnly,DATAONLYqual,
                                        NODATAONLYqual,
                                        DEFAULTdataonlyMERGE,"dataonly");
    /************************************************************************
    * Check for `cdaweb' qualifier.
    ************************************************************************/
    qopError = qopError | !TFqualifier (qop,&cdaweb,CDAWEBqual,
                                        NOCDAWEBqual,
                                        DEFAULTcdawebMERGE,"cdaweb");
    /************************************************************************
    * Check for `augment_label' qualifier.
    ************************************************************************/
    qopError = qopError | !TFqualifier (qop,&augmentlabel,AUGMENTLABELqual,
                                        NOAUGMENTLABELqual,
                                        DEFAULTaugmentlabelMERGE,"augment_label");
    /************************************************************************
    * Check for `master' qualifier.
    ************************************************************************/
    if (qop->qualEntered[MASTERqual]) {
      if (!ValidateQual(qop->qualOpt[MASTERqual], validQuals)) {
        master = TRUE;
        dataOnly = TRUE;
        strcpyX (masterFile, qop->qualOpt[MASTERqual], DU_MAX_PATH_LEN);
      } else {
        DisplayError ("Invalid \"master\" qualifier.");
        qopError = TRUE;
      }
    }
    /************************************************************************
    * Check for `file' qualifier.
    ************************************************************************/
    if (qop->qualEntered[FILEqual]) {
      strcpyX (fileList, qop->qualOpt[FILEqual], DU_MAX_PATH_LEN);
#if defined(__CYGWIN__)
      srcFp = fopen (fileList, "rt");
#else
      srcFp = fopen (fileList, "r");
#endif
      if (srcFp == NULL) {
        printf ("*** Error! Unable to open the source file: %s.\n", fileList);
        qopError = TRUE;
      }
      numCDFs = 0;
      while ( fgets(line,DU_MAX_PATH_LEN,srcFp) != NULL) {
        numCDFs++;
        if (numCDFs == 1) 
          numOpt = sscanf(line, "%s %s",sourceCDF, outputCDF);
      }
/*
      if (numCDFs < 3) {
        DisplayError ("Less than 3 files specified in the input file.");
        qopError = TRUE;
        fclose (srcFp);
      }
*/
      rewind (srcFp);
/*    numCDFs--; */
      CDFpaths = (char **) cdf_AllocateMemory ((size_t)sizeof(char *) * numCDFs, 
                                               FatalError);
      if (numOpt == 2) {
        numTokens = numCDFs;
      }
      prefixes = (char **) cdf_AllocateMemory ((size_t)sizeof(char *) * numCDFs,
                                               FatalError);
      icountx = -1;
      for (i = 0; i < numCDFs; i++) {
        CDFpaths[i] = (char *) cdf_AllocateMemory ((size_t)DU_MAX_PATH_LEN+1, 
                                                   FatalError);
        prefixes[i] = (char *) cdf_AllocateMemory ((size_t)PREFIXMAXLEN+1,
                                                   FatalError);
        fgets (line, DU_MAX_PATH_LEN, srcFp);
        if (sscanf(line, "%s %s", CDFpaths[i], prefixes[i]) != numOpt) {
          DisplayError ("Missing prefix in some entries in the input file.");
          qopError = TRUE;
          fclose (srcFp);
          for (j = 0; j <= i; j++) { 
             if (prefixes[j] != NULL) cdf_FreeMemory (prefixes[j], 
                                                       FatalError);
             if (CDFpaths[j] != NULL) cdf_FreeMemory (CDFpaths[j], 
                                                       FatalError);
          }
          if (prefixes != NULL) cdf_FreeMemory (prefixes, FatalError);
          if (CDFpaths != NULL) cdf_FreeMemory (CDFpaths, FatalError);
        }
      }
      strcpyX (outputCDF, CDFpaths[numCDFs-1], strlen(line) - 1);
      fclose (srcFp);
      if (numOpt == 1) {
        for (i = 0; i < numCDFs; i++) 
           if (prefixes[i] != NULL) cdf_FreeMemory (prefixes[i],
                                                    FatalError);
        if (prefixes != NULL) cdf_FreeMemory (prefixes, FatalError);
      }
    } else
      MakeNUL (fileList);
/*
else {
       DisplayError ("Invalid \"file\" qualifier.");
       qopError = TRUE;
    }
*/
    /**************************************************************************
    * Get CDF pathnames if `file' qualifier is not entered.
    **************************************************************************/
    if (NULstring(fileList)) {
/*
      if (qop->Nparms < 3) {
        DisplayError ("Missing parameter(s)... Need at least 3 files (2 inputs 1 output).");
        qopError = TRUE;
      }
      else { 
*/
        numCDFs = qop->Nparms;
        CDFpaths = (char **) cdf_AllocateMemory ((size_t)sizeof(char *) * numCDFs, 
                                                 FatalError);
        for (i = 0; i < numCDFs; i++) {
          CDFpaths[i] = (char *) cdf_AllocateMemory ((size_t)DU_MAX_PATH_LEN+1, 
                                                      FatalError);
          strcpyX (CDFpaths[i], qop->parms[i], DU_MAX_PATH_LEN);
        }
        strcpyX (outputCDF, CDFpaths[numCDFs-1], DU_MAX_PATH_LEN);
    }
    /**************************************************************************
    * Check which prefix values should be used was specified. Only check
    * if there is no "file" qualifier entered or no prefixes specified in 
    * that file.
    **************************************************************************/
    if (NULstring(fileList) || numTokens == 0) {
      count = 0;
      if (qop->qualEntered[NOPREFIXqual] || 
          qop->qualEntered[PREFIXESqual]) count++;
      switch (count) {
        case 0:
          usePrefix = TRUE;
          MakeNUL (prefix);
          break;
        case 1:
          if (qop->qualEntered[NOPREFIXqual]) {
            usePrefix = FALSE;
            MakeNUL (prefix);
            break;
          }
          if (qop->qualEntered[PREFIXESqual]) {
            usePrefix = TRUE;
            strcpyX (prefix, qop->qualOpt[PREFIXESqual], 1024);
            numTokens = GetNumTokens ((int)',', prefix);
            if (!master && (numTokens != (numCDFs-1))) {
              DisplayError ("Numbers of prefixes and source CDFs do not match.");
              qopError = TRUE;
            } else {
              prefixes = (char **) cdf_AllocateMemory ((size_t)sizeof(char *) * numTokens,
                                                       FatalError);
              for (i = 0; i < numTokens; i++)
                prefixes[i] = (char *) cdf_AllocateMemory ((size_t)DU_MAX_PATH_LEN+1,
                                                           FatalError);
              ParseStringForTokens ((int)',', prefix, prefixes);
            }
            break;
          }
        default:
          DisplayError ("Specify only one prefix option.");
          qopError = TRUE;
      }
    }
    /************************************************************************
    * Free QOP memory and check for an error.
    ************************************************************************/
    cdf_FreeMemory (qop, FatalError);
    if (qopError) {
      for (j = 0; j <= i; j++) {
         if (prefixes[j] != NULL) cdf_FreeMemory (prefixes[j],
                                                  FatalError);
         if (CDFpaths[j] != NULL) cdf_FreeMemory (CDFpaths[j],
                                                  FatalError);
      }
      if (prefixes != NULL) cdf_FreeMemory (prefixes, FatalError);
      if (CDFpaths != NULL) cdf_FreeMemory (CDFpaths, FatalError);
      return FALSE;
    }
    break;
  }

  /*****************************************
  * Check the master file for information.
  *****************************************/
  if (master) {
    status = CDFlib (OPEN_, CDF_, masterFile, &idm,
                     SELECT_, CDF_zMODE_, zMODEon2,
                              CDF_READONLY_MODE_, READONLYon,
                     GET_, CDF_MAJORITY_, &major,
                           CDF_ENCODING_, &encoding,
                           CDF_FORMAT_, &format,
                           CDF_COMPRESSION_, &cType, cParms, &cPct,
                           CDF_NUMATTRS_, &numAttrs,
                           CDF_NUMgATTRS_, &numgAttrs,
                     NULL_);
    if (status == CDF_OK) 
      masterCDF = TRUE;
    else {
      masterCDF = FALSE;
      /* A master text file. */
      if (!LoadMasterTextFile (masterFile)) {
        printf("*** Error! Unable to open the Master file (%s)...\n",
               masterFile);
        return FALSE;
      }
    }
    MakeNUL (mytmp);
    if (masterCDF)
      ConstructOutputFileName (mytmp);
    else 
      ConstructOutputFileName2 (mytmp);

    if (!NULstring (mytmp)) {
      strcpyX (outputCDF, mytmp, 0);
    } else {
      printf("*** Not enough information is available to construct the \n");
      printf("    output file name from the master %s (%s)...\n",
             BOO(masterCDF,"CDF","text"), masterFile);
      if (errorP == 1 || errorP == 11 || errorP == 101 || errorP == 111) 
        printf("    Missing: attribute Source_name or its entry...\n");
      if (errorP == 10 || errorP == 11 || errorP == 110 || errorP == 111) 
          printf("    Missing: attribute Descriptor or its entry...\n");
      if (errorP == 100 || errorP == 101 || errorP == 110 || errorP == 111) 
        printf("    Missing: attribute Data_type or its entry...\n");
      return FALSE;
    }
  } else 
    numCDFs--;

  /***************************************************************************
  * Display converting message.
  ***************************************************************************/
  printf ("Merging cdf files: \"");
  for (icount = 0; icount < numCDFs; icount++) {
    if (icount != 0) printf ("        \"");
    printf ("%s", CDFpaths[icount]);
    if (!EndsWithIgCase (CDFpaths[icount], ".cdf"))
      if (mLog) 
        printf (".cdf\" (%d) \n", (icount+1));
      else
        printf (".cdf\" \n");
    else
      if (mLog)
        printf ("\"  (%d)\n", (icount+1));
      else
        printf ("\" \n");
  }
  printf ("     into \"");
  printf ("%s", outputCDF);
  if (!EndsWithIgCase (outputCDF, ".cdf"))
    printf (".cdf");
  printf ("\"\n");

  /***********************************
  * Loop through the source CDFs.   
  ***********************************/
  icountx = 1;
  for (icount = 0; icount < numCDFs; icount++) {
    if (!master || !masterCDF) 
      status = CDFlib (OPEN_, CDF_, CDFpaths[icount], &id,
                       SELECT_, CDF_zMODE_, zMODEon2,
                                CDF_READONLY_MODE_, READONLYon,
                       GET_, CDF_MAJORITY_, &major,
                             CDF_ENCODING_, &encoding,
                             CDF_FORMAT_, &format,
                             CDF_NUMzVARS_, &numzVars,
                             CDF_NUMATTRS_, &numAttrs,
                             CDF_NUMgATTRS_, &numgAttrs,
                             CDF_NUMvATTRS_, &numvAttrs,
                             CDF_COMPRESSION_, &cType, cParms, &cPct,
                     NULL_);
    else 
      status = CDFlib (OPEN_, CDF_, CDFpaths[icount], &id,
                       SELECT_, CDF_zMODE_, zMODEon2,
                                CDF_READONLY_MODE_, READONLYon,
                       GET_, CDF_NUMzVARS_, &numzVars,
                             CDF_NUMATTRS_, &numAttrs,
                             CDF_NUMvATTRS_, &numvAttrs,
                     NULL_);
    
    if (status < CDF_OK) return QuitCDF ("1.0", status, CDFpaths[icount]);

    /***************************************
    * Create the output CDF.              
    ***************************************/

    if (icount == 0) {  
      majority = major;
      dimSizes[0] = 0L;
      status = CDFlib (CREATE_, CDF_, outputCDF, 0L, dimSizes, &ido,
                       PUT_, CDF_MAJORITY_, majority,
                             CDF_ENCODING_, encoding,
                             CDF_FORMAT_, format,
                             CDF_COMPRESSION_, cType, cParms,
                       CREATE_, ATTR_, "Parent", GLOBAL_SCOPE, &attrNum,
                       NULL_);
      if (status < CDF_OK) return QuitCDF ("2.0", status, outputCDF);

      if (mLog) printf ("  Merged file created...\n");

    }

    status = CDFlib (SELECT_, CDF_, ido,
                              ATTR_NAME_, "Parent",
                     GET_, ATTR_NUMgENTRIES_, &numgEntries,
                     NULL_);
    if (status < CDF_OK) return QuitCDF ("3.0", status, "Parent");

    if (numTokens > 0)
      snprintf (entryValue, (size_t) sizeof(entryValue),
                "%s: ", prefixes[icount]);
    else
      snprintf (entryValue, (size_t) sizeof(entryValue),
                "file%ld: ", (numgEntries+1));
    strcatX (entryValue, CDFpaths[icount], 0);
    if (!EndsWithIgCase (entryValue, ".cdf"))
      strcatX (entryValue, ".cdf", 0);
    status = CDFlib (SELECT_, CDF_, id,
                              ATTR_NAME_, "Parent",
                     GET_, ATTR_NUMgENTRIES_, &numigEntries, 
                     NULL_);
    if (status < CDF_OK || numigEntries < 1) {
      status = CDFlib (SELECT_, CDF_, id,
                                ATTR_NAME_, "FileSource",
                       GET_, ATTR_NUMgENTRIES_, &numigEntries,
                       NULL_);
      if (status < CDF_OK || numigEntries < 1) {
        status = CDFlib (SELECT_, CDF_, ido,
                                  ATTR_NAME_, "Parent",
                                  gENTRY_, (long) numgEntries,
                         PUT_, gENTRY_DATA_, CDF_CHAR,
                               (long) strlen (entryValue), entryValue,
                         NULL_);
        if (status < CDF_OK) return QuitCDF ("9.0", status, "Parent");
        ++numgEntries;
      }
    }
    /********************************************
    * Load the Global Attributes.              
    ********************************************/

    if (master) {
      if (icount == 0) {
        if (mLog) printf ("  Adding global attributes (%ld) from file: %s...\n",
                          BOO(masterCDF, numgAttrs, lineAttrs),
                          masterFile);
        if (masterCDF) 
          status2 = LoadgAttributes (idm, icountx);
        else
          status2 = LoadgAttributes2 ();
	if (!status2) return status2;
        if (mLog) printf ("  Global attributes from source file: %s added...\n",
                          masterFile);
      }
    } else {
      if (numgAttrs > 0) {
        if (icount == 0 || (icount != 0 && !dataOnly)) {
          if (mLog) printf ("  Adding global attributes (%ld) from file: %s...\n",
                            numgAttrs, CDFpaths[icount]);
          status2 = LoadgAttributes (id, icountx);
	  if (!status2) return status2;
          if (mLog) printf ("  Global attributes from source file: %d added...\n",
                            (icount+1));
        }
      }
    }

    /******************************************************
    * Load the variables (all are zVariables).           
    ******************************************************/
    if (numzVars > 0) {

      if (mLog) printf ("  Adding zVariables (%ld) from file: %d...\n",
                        numzVars, (icount+1));

      varNames = (char **) cdf_AllocateMemory ((size_t)sizeof(char *) * 
                                               (int) numzVars, FatalError);
      for (i = 0; i < numzVars; i++) {
        status = CDFlib (SELECT_, CDF_, id,
                                  zVAR_, (long) i,
                         GET_, zVAR_NAME_, varName,
                               zVAR_DATATYPE_, &dataType,
                               zVAR_NUMELEMS_, &numElems,
                               zVAR_NUMDIMS_, &numDims,
                               zVAR_DIMSIZES_, dimSizes,
                               zVAR_DIMVARYS_, dimVarys,
                               zVAR_RECVARY_, &recVary,
                               zVAR_COMPRESSION_, &cType, cParms, &cPct,
                               zVAR_BLOCKINGFACTOR_, &bf,
                               zVAR_SPARSERECORDS_, &sp,
                        NULL_);
        if (status < CDF_OK) return QuitCDF ("10.0", status, varName);

        varNames[i] = (char *) cdf_AllocateMemory ((size_t)strlen(varName)+1,
                                                   FatalError);
        strcpyX (varNames[i], varName, strlen(varName));

/*
        if ((strcmpIgCase (varName, "epoch") == 1) ||
            (strcmpIgCase (varName, "range_epoch") == 1)) {
*/
        if (strcmpIgCase (varName, "range_epoch") == 1) {
          if (first) {
            /*******************************************************
            * Create variable "Epoch" or "RANGE_EPOCH". -- old    
            * Create variable "RANGE_EPOCH".                      
            *******************************************************/
            dimSizes[0] = 0L;
            dimVarys[0] = 0L;
            strcpyX (epochName, varName, 0);
            status = CDFlib (SELECT_, CDF_, ido,
                             CREATE_, zVAR_, varName, dataType, 1L, 0L,
                                             dimSizes, VARY, dimVarys, &varNum,
                             NULL_);

            if (status < CDF_OK)
		 return QuitCDF ("10.2", status, "Epoch or RANGE_EPOCH");

/*
            status = CDFlib (SELECT_, CDF_, ido,
                             CREATE_, zVAR_, "VirtualEpoch", dataType, 1L, 0L,
                                             dimSizes, VARY, dimVarys, &varNum,
                             NULL_);
            if (status < CDF_OK) return QuitCDF ("10.4", status, "VirtualEpoch");
*/
            if (dataType == CDF_EPOCH) whichEpoch2 = 1;
            else if (dataType == CDF_EPOCH16) whichEpoch2 = 2;
            else whichEpoch2 = 3;
            first = FALSE;
          }
          continue;
        }

        if (cType != NO_COMPRESSION) {
          status = CDFlib (CONFIRM_, zVAR_RESERVEPERCENT_, &reservePct,
                           NULL_);
          if (status < CDF_OK) return QuitCDF ("11.0", status, NULL);
        }

        nBytes = (size_t) (CDFelemSize(dataType) * numElems);
        value = cdf_AllocateMemory ((size_t) nBytes, FatalError);
        status = CDFlib (GET_, zVAR_PADVALUE_, value,
                         NULL_);
        if (status != NO_PADVALUE_SPECIFIED)
          pad = TRUE;
        else
          pad = FALSE;

        if (usePrefix && !dataOnly)
            ModifyName (varName, numTokens, prefixes, icountx, newVarName);
        else
          strcpyX (newVarName, varName, 0);
        status = CDFlib (SELECT_, CDF_, ido,
                         CREATE_, zVAR_, newVarName, dataType, numElems, numDims,
                                         dimSizes, recVary, dimVarys, &varNum,
                         PUT_, zVAR_COMPRESSION_, cType, cParms,
                               zVAR_BLOCKINGFACTOR_, bf,
                               zVAR_SPARSERECORDS_, sp,
                         NULL_);

        if (status < CDF_OK) {
          if (status == VAR_EXISTS) {
            if (dataOnly || (cdaweb && 
                             ((strcmpIgCase (newVarName, "EPOCH") == 1) ||
                              (strcmpIgCase (newVarName, "RANGE_EPOCH") == 1))))
              ;
            else
              return QuitCDF ("12.0", status, newVarName);
          } else
            return QuitCDF ("12.5", status, newVarName);
        } else {
          if (cType != NO_COMPRESSION) {
            status1 = CDFlib (SELECT_, zVAR_RESERVEPERCENT_, reservePct,
                              NULL_);
            if (status1 < CDF_OK) return QuitCDF ("13.0", status, newVarName);
          }
          if (pad) {
            status1 = CDFlib (PUT_, zVAR_PADVALUE_, value,
                              NULL_);
            if (status1 < CDF_OK) return QuitCDF ("14.0", status, newVarName);
          }
        }
        cdf_FreeMemory (value, FatalError);
      }
      if (mLog) printf ("  Variables from source file: %d added...\n",
                        (icount+1));
    }

    /*********************************************
    * Load the Variable Attributes.             
    *********************************************/
    if (numvAttrs > 0) {

      if (mLog) printf ("  Adding variable attributes (%ld) from source file: %d...\n",
                        numvAttrs, (icount+1));

      for (i = 0; i < (int) numAttrs; i++) {
        status = CDFlib (SELECT_, CDF_, id,
                                  ATTR_, (long) i,
                         GET_, ATTR_SCOPE_, &scope,
                         NULL_);
        if (status < CDF_OK) return QuitCDF ("15.0", status, NULL);

        if (scope == GLOBAL_SCOPE) continue;

        status = CDFlib (GET_, ATTR_NAME_, attrName,
                               ATTR_NUMzENTRIES_, &numzEntries,
                         NULL_);
        if (status < CDF_OK) return QuitCDF ("16.0", status, attrName);
 
        if (augmentlabel) {
          if ((strcmpIgCase (attrName, "augment_labl") == 1)) 
            augmentlabelExist = TRUE;
        }
          
        status = CDFlib (SELECT_, CDF_, ido,
                         CREATE_, ATTR_, attrName, scope, &attrNum,
                         NULL_);
        if (status < CDF_OK) {
          if (status == ATTR_EXISTS) {
            status = CDFlib (SELECT_, CDF_, ido,
                             GET_, ATTR_NUMBER_, attrName, &attrNum,
                             NULL_);
            if (status < CDF_OK) return QuitCDF ("17.0", status, attrName);
          } else
            return QuitCDF ("17.5", status, attrName);
        }

        j = k = 0;
        if (numzEntries > 0) {
          do {
            status = CDFlib (SELECT_, CDF_, id,
                                      ATTR_, (long) i,
                                      zENTRY_, (long) j,
                             GET_, zENTRY_DATATYPE_, &dataTypeX,
                                   zENTRY_NUMELEMS_, &numElemsX,
                             NULL_);
            if (status < CDF_OK) {
              j++;
              continue;
            } else {
              nBytes = (size_t) (CDFelemSize(dataTypeX) * (numElemsX+1));
              value = cdf_AllocateMemory ((size_t) nBytes, FatalError);
              status = CDFlib (SELECT_, CDF_, id,
                               GET_, zENTRY_DATA_, value,
                               SELECT_, zVAR_, (long) j,
                               GET_, zVAR_NAME_, varName, 
                               NULL_);
              if (status < CDF_OK) return QuitCDF ("18.0", status, NULL);

              if (STRINGdataType(dataTypeX)) {
                *(((char *)value)+(int)numElemsX) = NUL;
              }
/*
              if ((strcmpIgCase (varName, "epoch") == 1) ||
                  (strcmpIgCase (varName, "range_epoch") == 1)) {
*/
              if (strcmpIgCase (varName, "range_epoch") == 1) {
	          strcpyX (newVarName, varName, 0);
	      } else {
                if (usePrefix && !dataOnly)
                  ModifyName (varName, numTokens, prefixes, (int) icountx, newVarName);
                else
                  strcpyX (newVarName, varName, 0);
              }

              status = CDFlib (SELECT_, CDF_, ido,
                                        ATTR_NAME_, attrName,
                                        zENTRY_NAME_, newVarName,
                               GET_, zENTRY_DATATYPE_, &dataTypeY,
                                     zENTRY_NUMELEMS_, &numElemsY,
                               NULL_);
              /***************************************************************
              * If already exists, do nothing.                              
              * Otherwise, write it.                                       
              ***************************************************************/
              if (status == CDF_OK) {
                if (!usePrefix) {
                  cdf_FreeMemory (value, FatalError);
                  k++;    
                  j++;                              
                  continue;
/*                  return QuitCDF ("18.5", VAR_EXISTS, newVarName); */
                }
              } else if (status < CDF_OK) {
                /**************************************************************
                * Check for special attributes that have the entry data of a
                * variable name. For this case, the entry needs to be changed
                * to reflect the prefixed variable name in the merged file.
                **************************************************************/
                if (usePrefix && !dataOnly) {
                  if (strcmpIgCase (attrName, dspType) == 1) {
                    changed = FALSE;
                    newValue = (char *) cdf_AllocateMemory ((size_t)strlen(value)+150,
                                                            FatalError);
                    /**********************************************************
                    * Parse the entry for DISPLAY_TYPE for variable name(s).
                    * Have to change it as it will have a prefix.
                    **********************************************************/
                    ParseStringForVariables (numTokens, prefixes, icountx,
                                             (int) numzVars, varNames,
                                             value, newValue, &changed);
                    if (changed)
                      status = CDFlib (SELECT_, CDF_, ido,
                                                ATTR_NAME_, attrName,
                                                zENTRY_NAME_, newVarName,
                                       PUT_, zENTRY_DATA_, dataTypeX,
                                                           (long) strlen(newValue),
                                                           newValue,
                                       NULL_);
                    else
                      status = CDFlib (SELECT_, CDF_, ido,
                                                ATTR_NAME_, attrName,
                                                zENTRY_NAME_, newVarName,
                                       PUT_, zENTRY_DATA_, dataTypeX, numElemsX,
                                                           value,
                                       NULL_);
                    if (status < CDF_OK) return QuitCDF ("19.0", status, newVarName);
                    cdf_FreeMemory (newValue, FatalError);
                  } else {
                    ifound = FALSE;
                    for (ii = 0; ii < sp2vAttrs; ii++) {
                      if (strcmpIgCase (attrName, special2vAttrs[ii]) == 1) {
                        ifound = TRUE;
                        break;
                      }
                    }
                    if (ifound) {
                      if (STRINGdataType(dataTypeX)) {
                        *(((char *)value)+(int)numElemsX) = NUL;
                      }
                      ModifyName ((char *)value, numTokens, prefixes, icountx, 
                                  newVarName2);
                      numElemsX = (long) strlen (newVarName2);
                    }
                    status = CDFlib (SELECT_, CDF_, ido,
                                              ATTR_NAME_, attrName,
                                              zENTRY_NAME_, newVarName,
                                     PUT_, zENTRY_DATA_, dataTypeX, numElemsX,
                                                 BOO(ifound,newVarName2,value), 
                                     NULL_);
                    if (status < CDF_OK) return QuitCDF ("19.5", status, newVarName);
                  }
                } else {
                  status = CDFlib (SELECT_, CDF_, ido,
                                            ATTR_NAME_, attrName,
                                            zENTRY_NAME_, newVarName,
                                   PUT_, zENTRY_DATA_, dataTypeX, numElemsX,
                                                       value,
                                   NULL_);
                  if (status < CDF_OK) {
/*
                    if ((strcmpIgCase (varName, "epoch") == 1) &&
                        (strcmpIgCase (varName, "range_epoch") == 1)) 
*/
                    if (strcmpIgCase (varName, "range_epoch") == 1) 
                      return QuitCDF ("19.7", status, newVarName);
                  }
                }
              }
              k++;
              j++;
              cdf_FreeMemory (value, FatalError);
            }
          } while (k < numzEntries);
        }
      }
      
      if (mLog) printf ("  Variable attributes from source file: %d added...\n",
                        (icount+1));
    }

    if (numzVars > 0)  {
      for (i = 0; i < (int) numzVars; i++)
         cdf_FreeMemory (varNames[i], FatalError);

      cdf_FreeMemory (varNames, FatalError);
    }

    status = CDFlib (SELECT_, CDF_, id,
                              ATTR_NAME_, "Parent",
                     GET_,    ATTR_NUMgENTRIES_, &numx,
                     NULL_);
    if (status == NO_SUCH_ATTR) {
      status = CDFlib (SELECT_, CDF_, id,
                                ATTR_NAME_, "FileSource",
                       GET_,    ATTR_NUMgENTRIES_, &numx,
                       NULL_);
      if (status == NO_SUCH_ATTR) ++icountx;
      else icountx += (int) numx;
    } else
      icountx += (int) numx;

    status = CDFlib (SELECT_, CDF_, id,
                     CLOSE_, CDF_,
                     NULL_);
    if (status < CDF_OK) return QuitCDF ("20.0", status, NULL);

  }

  /*********************************************
  * Add the AUGMENT_LABL attribute if needed. 
  *********************************************/
  if (augmentlabel) {
    if (!augmentlabelExist) {
      status = CDFlib (SELECT_, CDF_, ido,
                       CREATE_, ATTR_, "AUGMENT_LABL", VARIABLE_SCOPE, &attrNum,
                       NULL_);
      if (status < CDF_OK) 
        return QuitCDF ("20.2", status, "AUGMENT_LABL");
    } else {
      status = CDFlib (SELECT_, CDF_, ido,
                       GET_, ATTR_NUMBER_, "AUGMENT_LABL", &attrNum,
                       NULL_);
      if (status < CDF_OK) 
        return QuitCDF ("20.3", status, "AUGMENT_LABL");
    }
    status = CDFlib (SELECT_, CDF_, ido,
                     GET_, CDF_NUMzVARS_, &numVars,
                     NULL_);
    if (status < CDF_OK) 
      return QuitCDF ("20.4", status, "AUGMENT_LABL");
    for (i = 0; i < (int) numVars; i++) {
      status = CDFlib (SELECT_, CDF_, ido,
                                ATTR_, attrNum,
                                zENTRY_, (long) i,
                       PUT_, zENTRY_DATA_, CDF_CHAR, 4L, "TRUE",
                       NULL_);
      if (status < CDF_OK) 
        return QuitCDF ("20.6", status, "AUGMENT_LABL");
    }
  }

  /**************************************** 
  * Load the Variable data.               
  ****************************************/
  icountx = 1;
  for (icount = 0; icount < numCDFs; icount++) {
    if (mLog) printf ("  Adding variable data from source file: %d...\n",
                      (icount+1));

    status = CDFlib (OPEN_, CDF_, CDFpaths[icount], &id,
                     SELECT_, CDF_zMODE_, zMODEon2,
                              CDF_READONLY_MODE_, READONLYon,
                     GET_, CDF_MAJORITY_, &major,
                           CDF_NUMzVARS_, &numzVars,
                     NULL_);   
    if (status < CDF_OK) return QuitCDF ("21.0", status, CDFpaths[icount]);

    srcRowMajor = ROWmajor(major);
    for (ir = 0; ir < (int) numzVars; ir++) {

      status = CDFlib (SELECT_, CDF_, id,
                                zVAR_, (long) ir,
                       GET_, zVAR_NAME_, varName,
                             zVAR_RECVARY_, &recVary,
                             zVAR_DATATYPE_, &dataType,
                             zVAR_NUMELEMS_, &numElems,
                             zVAR_NUMDIMS_, &numDims,
                             zVAR_DIMSIZES_, dimSizes,
                             zVAR_MAXREC_, &maxRec,
                             zVAR_COMPRESSION_, &cType, cParms, &cPct,
                       NULL_);
      if (status < CDF_OK) return QuitCDF ("22.0", status, varName);

      if (maxRec == NO_RECORD) continue;

      /************************************************************************
      * Acquire the min/max epoch time for Epoch or RANGE_EPOCH variable. -old
      * Acquire the min/max epoch time for RANGE_EPOCH variable, only.
      ************************************************************************/
/*
      if ((strcmpIgCase (varName, "epoch") == 1) ||
          (strcmpIgCase (varName, "range_epoch") == 1)) {
*/
      if (strcmpIgCase (varName, "range_epoch") == 1) {
        
        status = CDFlib (SELECT_, zVAR_RECNUMBER_, 0L,
                                  zVAR_RECCOUNT_, 2L,
                                  zVAR_RECINTERVAL_, 1L,
                                  zVAR_DIMINDICES_, dimSizes,
                                  zVAR_DIMCOUNTS_, dimSizes,
                                  zVAR_DIMINTERVALS_, dimSizes,
                         NULL_);
        if (status < CDF_OK) return QuitCDF ("22.3", status, "Epoch or RANGE_EPOCH");

        if (whichEpoch2 == 1) 
          status = CDFlib (GET_, zVAR_HYPERDATA_, minmaxsEpoch,
                           NULL_);
        else if (whichEpoch2 == 2)
          status = CDFlib (GET_, zVAR_HYPERDATA_, minmaxdEpoch,
                           NULL_);
        else
          status = CDFlib (GET_, zVAR_HYPERDATA_, minmaxTT2000,
                           NULL_);
        if (status < CDF_OK) return QuitCDF ("22.5", status, "Epoch or RANGE_EPOCH");

        if (whichEpoch2 == 1) {
          if (minsEpoch == 0.0) {
            minsEpoch = minmaxsEpoch[0];
            maxsEpoch = minmaxsEpoch[1];
          } else {
            if (minsEpoch > minmaxsEpoch[0]) minsEpoch = minmaxsEpoch[0];
            if (maxsEpoch < minmaxsEpoch[1]) maxsEpoch = minmaxsEpoch[1];
          }
        } else if (whichEpoch2 == 2) {
          if (mindEpoch[0] == 0.0) {
            mindEpoch[0] = minmaxdEpoch[0][0];
            mindEpoch[1] = minmaxdEpoch[0][1];
            maxdEpoch[0] = minmaxdEpoch[1][0];
            maxdEpoch[1] = minmaxdEpoch[1][1];
          } else {
            if ((mindEpoch[0] > minmaxdEpoch[0][0]) || 
                ((mindEpoch[0] == minmaxdEpoch[0][0]) &&
                 (mindEpoch[1] > minmaxdEpoch[0][1]))) {
              mindEpoch[0] = minmaxdEpoch[0][0];
              mindEpoch[1] = minmaxdEpoch[0][1];
            }
            if ((maxdEpoch[0] < minmaxdEpoch[1][0]) ||
                ((maxdEpoch[0] == minmaxdEpoch[1][0]) &&
                 (maxdEpoch[1] < minmaxdEpoch[1][1]))) {
               maxdEpoch[0] = minmaxdEpoch[1][0];
               maxdEpoch[1] = minmaxdEpoch[1][1];
            }
          }
        } else {
          if (minTT2000 == FILLED_TT2000_VALUE) {
            minTT2000 = minmaxTT2000[0];
            maxTT2000 = minmaxTT2000[1];
          } else {
            if (minTT2000 > minmaxTT2000[0]) minTT2000 = minmaxTT2000[0];
            if (maxTT2000 < minmaxTT2000[1]) maxTT2000 = minmaxTT2000[1];
          }
        }
        continue;
      } 

      /************************************************************************
      * Read/write values using hyper groups...
      ************************************************************************/
      for (dimN = 0, nValuesPerRec = 1; dimN < numDims; dimN++) {
         if (dimVarys[dimN])
           nValuesPerRec *= dimSizes[dimN];
         else
           dimSizes[dimN] = 1;
      }

      if (usePrefix && !dataOnly) 
        ModifyName (varName, numTokens, prefixes, icountx, newVarName);
      else
        strcpyX (newVarName, varName, 0);

      status = CDFlib (SELECT_, CDF_, ido,
                       GET_, zVAR_NUMBER_, newVarName, &varNum,
                       NULL_);
      if (status < CDF_OK) return QuitCDF ("23.0", status, newVarName);

      status = CDFlib (SELECT_, CDF_, ido,
                                zVAR_, varNum,
                       GET_, zVAR_MAXREC_, &varMaxRec,
                       NULL_);
      if (status < CDF_OK) return QuitCDF ("23.5", status, newVarName);

      if (icount == 0) 
        varMaxRec = 0;
      else
        varMaxRec++;

      nBytes = (size_t) (CDFelemSize(dataType) * numElems);
      handles[0] = &buffer1;
      handles[1] = &buffer2;
      nValueBytes[0] = (size_t) nBytes;
      nValueBytes[1] = (size_t) nBytes;
      switchMajority = (numDims > 1 && majority != major);

/*      if (icount != 0 && dataOnly && !recVary) continue;  */

      for (recNum = 0; recNum <= (int) maxRec; recNum = toRec + 1) {
        /**********************************************************************
        * Determine the next allocated record.
        **********************************************************************/
        status = CDFlib (SELECT_, CDF_, id,
                                  zVAR_, (long) ir,
                         GET_, zVAR_ALLOCATEDFROM_, (long) recNum, &fromRec,
                         NULL_);
        if (status < CDF_OK) return QuitCDF ("24.0", status, newVarName);
        /**********************************************************************
        * Determine the last allocated record (before the next unallocated one).
        * Do not let this exceed the maximum record written to the variable.
        **********************************************************************/
        status = CDFlib (SELECT_, CDF_, id,
                                  zVAR_, (long) ir,
                         GET_, zVAR_ALLOCATEDTO_, fromRec, &toRec,
                         NULL_);
        if (status < CDF_OK) return QuitCDF ("25.0", status, newVarName);
        toRec = MINIMUM(toRec,maxRec);

        fromRec2 = fromRec + varMaxRec;
        toRec2 = toRec + varMaxRec;

        /**********************************************************************
        * Allocate the records unless the variable is compressed or has sparse
        * arrays.
        **********************************************************************/
        if (cType == NO_COMPRESSION) {
          status = CDFlib (SELECT_, CDF_, ido,
                                    zVAR_, varNum,
                           PUT_, zVAR_ALLOCATEBLOCK_, fromRec, toRec,
                           NULL_);
          if (status < CDF_OK) return QuitCDF ("26.0", status, newVarName);
        }
        /**********************************************************************
        * Calculate the number of records in this group.
        **********************************************************************/
        nRecords = toRec - fromRec + 1;
        /**********************************************************************
        * If the majority is being switched...
        **********************************************************************/
        if (switchMajority) { 
          AllocateBuffers (nRecords, numDims, dimSizes, &groups, 0, 2, handles,
                           nValueBytes, srcRowMajor, 5, FatalError);
          if (HyperFullRecord(&groups,numDims)) {
            long nBytesPerRecord = nValuesPerRec * nBytes, recX;
            InitHyperParms (&hyper, &groups, numDims, &nHypers, &nValues);
            hyper.recNumber = fromRec;
            for (hyperN = 0; hyperN < nHypers; hyperN++) {
              status = CDFlib (SELECT_, CDF_, id,
                                        zVAR_, (long) ir,
                               NULL_);
              if (status < CDF_OK) return QuitCDF ("27.0", status, newVarName);
              status = HYPER_READ (id, TRUE, hyper, buffer1);
              if (status < CDF_OK) return QuitCDF ("28.0", status, newVarName);
              hyperStep = hyper.recNumber;
              for (recX = 0; recX < hyper.recCount; recX++) {
                size_t offset = (size_t) (recX * nBytesPerRecord);
                if (srcRowMajor)
                  ROWtoCOL (buffer1 + offset, buffer2 + offset, numDims,
                            dimSizes, nBytes);
                else
                  COLtoROW (buffer1 + offset, buffer2 + offset, numDims,
                            dimSizes, nBytes);
              }
              status = CDFlib (SELECT_, CDF_, ido,
                                        zVAR_, varNum,
                               NULL_);
              if (status < CDF_OK) return QuitCDF ("29.0", status, newVarName);
              hyper.recNumber = fromRec2;
              status = HYPER_WRITE (ido, TRUE, hyper, buffer2);
              if (status < CDF_OK) return QuitCDF ("30.0", status, newVarName);
              hyper.recNumber = hyperStep;
              fromRec2 += hyper.recCount;
              IncrHyperParms (&hyper, &groups, numDims, srcRowMajor, &nValues);
            }
            cdf_FreeMemory (buffer1, FatalError);
            cdf_FreeMemory (buffer2, FatalError);
          } else {
            cdf_FreeMemory (buffer2, FatalError);
            InitHyperParms (&hyper, &groups, numDims, &nHypers, &nValues);
            hyper.recNumber = fromRec;
            for (hyperN = 0; hyperN < nHypers; hyperN++) {
              long indices[CDF_MAX_DIMS]; Byte1 *value = buffer1; long valueN;
              status = CDFlib (SELECT_, CDF_, ido,
                                        zVAR_, (long) ir,
                               NULL_);
              if (status < CDF_OK) return QuitCDF ("31.0", status, newVarName);

              status = HYPER_READ (id, TRUE, hyper, buffer1);
              if (status < CDF_OK) return QuitCDF ("32.0", status, newVarName);
              hyperStep = hyper.recNumber;
              hyper.recNumber = fromRec2;
              status = CDFlib (SELECT_, CDF_, ido,
                                        zVAR_, varNum,
                                        zVAR_RECNUMBER_, hyper.recNumber,
                               NULL_);
              if (status < CDF_OK) return QuitCDF ("33.0", status, newVarName);
              for (dimN = 0; dimN < numDims; dimN++) 
                indices[dimN] = hyper.dimIndices[dimN];
              for (valueN = 0; valueN < nValues; valueN++) {
                status = CDFlib (SELECT_, zVAR_DIMINDICES_, indices,
                                 PUT_, zVAR_DATA_, value,
                                 NULL_);
                if (status < CDF_OK) return QuitCDF ("34.0", status, newVarName);
                if (srcRowMajor)
                  INCRindicesROW (numDims, dimSizes, indices);
                else
                  INCRindicesCOL (numDims, dimSizes, indices);
                value += (size_t) nBytes;
              }
              hyper.recNumber = hyperStep;
              fromRec2 += hyper.recCount;
              IncrHyperParms (&hyper, &groups, numDims, srcRowMajor, &nValues);
            }    
            cdf_FreeMemory (buffer1, FatalError);
          }
        } else { 
          AllocateBuffers (nRecords, numDims, dimSizes, &groups, 0, 1, handles,
                           nValueBytes, srcRowMajor, 5, FatalError);
          InitHyperParms (&hyper, &groups, numDims, &nHypers, &nValues);
          hyper.recNumber = fromRec;
          for (hyperN = 0; hyperN < nHypers; hyperN++) {
            status = CDFlib (SELECT_, CDF_, id,
                                      zVAR_, (long) ir,
                             NULL_);
            if (status < CDF_OK) return QuitCDF ("35.0", status, newVarName);
            status = HYPER_READ (id, TRUE, hyper, buffer1);
            if (status < CDF_OK) return QuitCDF ("36.0", status, newVarName);
            hyperStep = hyper.recNumber;
            status = CDFlib (SELECT_, CDF_, ido,
                                      zVAR_, varNum,
                             NULL_);
            if (status < CDF_OK) return QuitCDF ("37.0", status, newVarName);
            hyper.recNumber = fromRec2;
            status = HYPER_WRITE (ido, TRUE, hyper, buffer1);
            if (status < CDF_OK) return QuitCDF ("38.0", status, newVarName);
            hyper.recNumber = hyperStep;
            fromRec2 += hyper.recCount;
            IncrHyperParms (&hyper, &groups, numDims, srcRowMajor, &nValues);
          }      
          cdf_FreeMemory (buffer1, FatalError);
        } /* switchmajority */

      } /* recNum */

    }  /* zVars */

    status = CDFlib (SELECT_, CDF_, id,
                              ATTR_NAME_, "Parent",
                     GET_, ATTR_NUMgENTRIES_, &numx,
                     NULL_);
    if (status == NO_SUCH_ATTR) {
      status = CDFlib (SELECT_, CDF_, id,
                                ATTR_NAME_, "FileSource",
                       GET_, ATTR_NUMgENTRIES_, &numx,
                       NULL_);
      if (status == NO_SUCH_ATTR) ++icountx;
      else icountx += (int) numx;
    } else icountx += (int) numx;
 
    status = CDFlib (SELECT_, CDF_, id,
                     CLOSE_, CDF_,
                     NULL_);
    if (status < CDF_OK) return QuitCDF ("39.0", status, NULL);

    if (mLog) printf ("  Variable data from source file: %d added...\n", 
                      (icount+1));

  } /* files */

  /**********************************************************************
  * Update variable Epoch or RANGE_EPOCH for its min/max time.
  **********************************************************************/
  if (whichEpoch2 != 0) {
    if (whichEpoch2 == 1) {
      minmaxsEpoch[0] = minsEpoch;
      minmaxsEpoch[1] = maxsEpoch;
    } else if (whichEpoch2 == 2) {
      minmaxdEpoch[0][0] = mindEpoch[0];
      minmaxdEpoch[0][1] = mindEpoch[1];
      minmaxdEpoch[1][0] = maxdEpoch[0];
      minmaxdEpoch[1][1] = maxdEpoch[1];
    } else {
      minmaxTT2000[0] = minTT2000;
      minmaxTT2000[1] = maxTT2000;
    }
    status = CDFlib (SELECT_, CDF_, ido,
                              zVAR_NAME_, epochName,
                              zVAR_RECNUMBER_, 0L,
                              zVAR_RECCOUNT_, 2L,
                              zVAR_RECINTERVAL_, 1L,
                              zVAR_DIMINDICES_, dimSizes,
                              zVAR_DIMCOUNTS_, dimSizes,
                              zVAR_DIMINTERVALS_, dimSizes,
                     NULL_);
    if (status < CDF_OK) return QuitCDF ("39.3", status, "Epoch or RANGE_EPOCH");

    if (whichEpoch2 == 1) 
      status = CDFlib (PUT_, zVAR_HYPERDATA_, minmaxsEpoch,
                       NULL_);
    else if (whichEpoch2 == 2)
      status = CDFlib (PUT_, zVAR_HYPERDATA_, minmaxdEpoch,
                       NULL_);
    else
      status = CDFlib (PUT_, zVAR_HYPERDATA_, minmaxTT2000,
                       NULL_);
    if (status < CDF_OK) return QuitCDF ("39.5", status, "Epoch or RANGE_EPOCH");
  }

  status = CDFlib (SELECT_, CDF_, ido,
                   CLOSE_, CDF_, 
                   NULL_);
  if (status < CDF_OK) return QuitCDF ("40.0", status, NULL);

  if (mLog) printf ("  Done!\n");

  if (!master) ii = 1;
  else ii = 0;

  for (i = 0; i < (numCDFs+ii); i++) 
     if (CDFpaths[i] != NULL) cdf_FreeMemory (CDFpaths[i], FatalError);
  
  if (CDFpaths != NULL) cdf_FreeMemory (CDFpaths, FatalError);

  if (numTokens != 0) {
    for (i = 0; i < numCDFs; i++) 
      if (prefixes[i] != NULL) cdf_FreeMemory (prefixes[i], FatalError);
    if (prefixes != NULL) cdf_FreeMemory (prefixes, FatalError); 
  }

  if (lineAttrs > 0) {
    for (i = 0; i < lineAttrs; i++)
      cdf_FreeMemory (attrs[i], FatalError);
    for (i = 0; i < lineEntries; i++)
      cdf_FreeMemory (entries[i], FatalError);
    cdf_FreeMemory (attrs, FatalError);
    cdf_FreeMemory (entries, FatalError);
    cdf_FreeMemory (inentries, FatalError);
  }

  return TRUE;

}

/******************************************************************************
* ModifyName.
******************************************************************************/

void ModifyName (inName, numTokens, prefixes, index, outName)
char *inName;
int  numTokens;
char *prefixes[];
int   index;
char *outName;
{
  char temp[CDF_VAR_NAME_LEN256+1];
  int ix, iz;
  char *iy;
  MakeNUL (temp);
  if (numTokens != 0) {
    snprintf (temp, (size_t) CDF_VAR_NAME_LEN256, "%s_", prefixes[index]);
    snprintf (EofS(temp), (size_t) CDF_VAR_NAME_LEN256-strlen(temp), "%s", 
 	      inName);
  } else {
      if (strstr(inName, "file") != NULL) {
        iz = sscanf (inName+4, "%d_", &ix);
        if (iz == 1) {
          iy = strstr (inName, "_");
          snprintf (temp, (size_t) CDF_VAR_NAME_LEN256, "file%d_", (index+ix-1));
	  snprintf (EofS(temp), (size_t) CDF_VAR_NAME_LEN256-strlen(temp), "%s", 
	            iy+1);
        } else {
          snprintf (temp, (size_t) CDF_VAR_NAME_LEN256, "file%d_", index);
          snprintf (EofS(temp), (size_t) CDF_VAR_NAME_LEN256-strlen(temp), "%s",
                    inName);
        }
      } else {
        snprintf (temp, (size_t) CDF_VAR_NAME_LEN256, "file%d_", index);
        snprintf (EofS(temp), (size_t) CDF_VAR_NAME_LEN256-strlen(temp), "%s",
                  inName);
      }
  }
  strcpyX (outName, temp, 0);
}

/******************************************************************************
* ParseStringForVariables.
******************************************************************************/

void ParseStringForVariables (numTokens, prefixes, index, numVars, varNames, 
                              string1, newValue, changed)
int  numTokens;
char *prefixes[];
int  index;
int  numVars;
char *varNames[];
char *string1;
char *newValue;
Logical *changed;
{
  int  i; 
  char *ptr1, *ptr2, terminator;
  char *tmp, *tmp2, *outName;
  Logical first, found;

  *changed = FALSE;
  /****************************************************************************
  * Check that the entered string is not a NUL string, that there are
  * some possible tokens to match, etc.
  ****************************************************************************/
  if (NULstring(string1)) {
    return;     /* Obviously, no character were found. */
  }    
  /****************************************************************************
  * Scan the entered string of tokens searching for matches with the token.
  * First determine the starting character position and the * ending delimiter.
  ****************************************************************************/
  terminator = NUL;
  ptr1 = string1;
  first = TRUE;
  for (;;) {
     /*************************************************************************
     * Find beginning of the next token in list (skipping past any leading
     * blanks).  If the end of the list is reached instead, then there are
     * no more tokens.
     *************************************************************************/
     ptr2 = strstr (ptr1, "=");
     if (ptr2 == NULL) break;
     if (first) {
       first = FALSE;
       strcpyX (newValue, ptr1, (ptr2-ptr1+1));
     } else
       strcpyX (EofS(newValue), ptr1, (ptr2-ptr1+1));
     ptr1 = ptr2;
     /*************************************************************************
     * Assume ',', '(', ' ' or end of line is the terminator for the field
     * value.
     *************************************************************************/
     while (*ptr1 != ',' && *ptr1 != '(' && *ptr1 != ' ' && *ptr1 != terminator) 
       ptr1++;
     tmp = (char *) cdf_AllocateMemory ((size_t)(ptr1-ptr2), FatalError);
     strcpyX (tmp, ptr2+1, (ptr1-ptr2-1));
     found = FALSE;
     for (i = 0; i < numVars; i++) {
       if (!strcmp(tmp, varNames[i])) {
         found = TRUE;
         break;
       }
     }
     if (found) {
       outName = (char *) cdf_AllocateMemory ((size_t)strlen(tmp)+50, FatalError);
       ModifyName (tmp, numTokens, prefixes, index, outName);
       strcpyX (EofS(newValue), outName, strlen(outName));
       cdf_FreeMemory (outName, FatalError);
       *changed = TRUE;
     } else 
       strcpyX (EofS(newValue), tmp, strlen(tmp));
     cdf_FreeMemory (tmp, FatalError);
     if (*ptr1 == terminator) break;
  }
  if (*ptr1 != terminator) {
    if (first)
      strcpyX (newValue, ptr1, strlen(ptr1));
    else
      strcpyX (EofS(newValue), ptr1, strlen(ptr1));
  }
}

/******************************************************************************
* QuitCDF.
******************************************************************************/

Logical QuitCDF (where, status, msg)
char *where;
CDFstatus status;
char *msg;
{
  char text[CDF_STATUSTEXT_LEN+1];
  printf ("Program failed at %s...\n", where);
  if (status < CDF_OK) {
    CDFlib (SELECT_, CDF_STATUS_, status,
            GET_, STATUS_TEXT_, text,
            NULL_);
    printf ("%s ", text);
    if (msg != NULL) printf ("(%s)\n", msg);
    else printf("\n");
  }
  if (id != 0) 
    CDFlib (SELECT_, CDF_, id,
            CLOSE_, CDF_,
            NULL_);
  if (ido != 0)
    CDFlib (SELECT_, CDF_, ido,
            CLOSE_, CDF_,
            NULL_);

  printf ("\n");
  return FALSE;
}

/******************************************************************************
* ConstructOutputFileName.
******************************************************************************/
  
void ConstructOutputFileName (name)
char *name;
{ 
  char fileName[DU_MAX_PATH_LEN+1];
  char fileNaming[DU_MAX_PATH_LEN+1];
  char dataVersion[21];
  char **fields;
  char *ptr;
  int ix, iy, iz, counts, loc, loc2;
  long numElems, scope;
  char attrName[CDF_ATTR_NAME_LEN256], delimiter;
  char timeStamp[8+1];
  Logical found;

  MakeNUL (name);
  if (numAttrs == 0 || numgAttrs == 0) return;
  
  status =  CDFlib (SELECT_, CDF_, idm,
                    NULL_);

  attrs = (char **) cdf_AllocateMemory (sizeof(char *) * (int) numgAttrs, NULL);
  for (ix = 0, iy = 0; ix < (int) numAttrs; ix++) {
    status =  CDFlib (SELECT_, ATTR_, (long) ix,
                      GET_, ATTR_NAME_, attrName,
                            ATTR_SCOPE_, &scope,
                      NULL_);
    if (status != CDF_OK) return;
    if (scope != GLOBAL_SCOPE) continue;
    attrs[iy] = (char *) cdf_AllocateMemory ((size_t) strlen(attrName)+1, NULL);
    strcpyX(attrs[iy] , attrName, 0);
    iy++;
  }

  found = FALSE;
  for (ix = 0; ix < numgAttrs; ix++) {
    if (strcmpIgCase (attrs[ix], "file_naming_convention") == 1) {
      found = TRUE;
      iy = ix;
      break;
    }
  }
  if (found) {
    status =  CDFlib (SELECT_, ATTR_, (long) iy,
                               gENTRY_, 0L,
                      GET_, gENTRY_DATA_, fileNaming,
                            gENTRY_NUMELEMS_, &numElems,
                      NULL_);
    if (status != CDF_OK) {
      GetOutputFileName (name);
      return;
    }

    fileNaming[(int)numElems] = '\0';
    counts = 0;
    for (ix = 0; ix < numElems; ix++) {
       if (fileNaming[ix] == '_' || fileNaming[ix] == '-' ||
           fileNaming[ix] == '.') counts++;
       if (counts == 1) {
         if (fileNaming[ix] == '_') delimiter = '_';
         else if (fileNaming[ix] == '-') delimiter = '-';
         else if (fileNaming[ix] == '.') delimiter = '.';
       }
    }
    if (counts > 0) {
      fields = (char **) cdf_AllocateMemory (sizeof(char *) * counts, FatalError);
      BreakFields (fileNaming, (int) delimiter, fields, counts);
      loc2 = 0;
      for (ix = 0; ix <= counts; ix++) {
        found = FALSE;
        for (iz = 0; iz < numgAttrs; iz++) {
          char *tmp, *ptr;
          tmp = cdf_AllocateMemory (strlen(attrs[iz]), FatalError);
          ptr = strchr (attrs[iz], delimiter);
          if (ptr == NULL) 
            strcpyX (tmp, attrs[iz], 0);
          else {
            int locx = ptr-attrs[iz];
            strcpyX (tmp, attrs[iz], locx);
            strcatX (tmp+locx, attrs[iz]+locx+1, 0);
          }
          if (strncmpIgCasePattern (tmp, fields[ix], strlen(fields[ix])) == 0) {
            found = TRUE;
            iy = iz;
            break;
          }
          cdf_FreeMemory (tmp, FatalError);
        }
        if (found) {
          status =  CDFlib (SELECT_, ATTR_NAME_, attrs[iy],
                                     gENTRY_, 0L,
                            GET_, gENTRY_DATA_, fileNaming,
                                  gENTRY_NUMELEMS_, &numElems,
                            NULL_);
          if (status != CDF_OK) {
            GetOutputFileName (name);
            return;
          }
          fileNaming[(int)numElems] = '\0';
          ptr = strchr(fileNaming, '>');
          if (ptr != NULL) loc = (int) (ptr - fileNaming);
          else loc = (int) numElems;
          if (ix == 0)
            strcpyX(fileName, fileNaming, loc);
          else
            strcatX(fileName, fileNaming, loc+loc2);
          fileName[loc+loc2] = delimiter;
          loc2 = loc + loc2 + 1;
          fileName[loc2] = '\0';
        }
      }
      MakeNUL(timeStamp);
      GetDataTimeStamp (timeStamp);
      strcatX(fileName, timeStamp, loc2+8);  
      fileName[loc2+8] = delimiter;
      fileName[loc2+8+1] = '\0';
      loc2 = loc2 + 8 + 1;
      status =  CDFlib (SELECT_, ATTR_NAME_, "Data_version",
                                 gENTRY_, 0L,
                        GET_, gENTRY_DATA_, dataVersion,
                        NULL_);
      if (status == CDF_OK) {
        strcatX (fileName, "_V", 0);
        if (strlen(dataVersion) == 1) 
          strcatX (fileName, "0", 0);
        strcatX (fileName, dataVersion, 0);
      } else
        strcatX (fileName, "V01", 0);
      MakeLowerString (fileName);
      strcpyX (name, fileName, 0);
    } else {
      GetOutputFileName (name);
    }
  } else {
    GetOutputFileName (name);
  }
}

/******************************************************************************
* ConstructOutputFileName2.
******************************************************************************/
  
Logical ConstructOutputFileName2 (name)
char *name;
{ 
  char fileName[DU_MAX_PATH_LEN+1];
  char fileNaming[DU_MAX_PATH_LEN+1];
  char **fields;
  char *ptr;
  int ix, iy, iz, counts, loc, loc2;
  char delimiter;
  char timeStamp[8+1];
  Logical found;

  if (lineAttrs == 0 || lineEntries == 0) return TRUE;
  
  found = FALSE;
  for (ix = 0; ix < lineAttrs; ix++) {
    if (strcmpIgCase (attrs[ix], "file_naming_convention") == 1) {
      found = TRUE;
      iy = ix;
      break;
    }
  }
  if (found) {
    if (inentries[iy] == -1) {
      return GetOutputFileName2 (name);
    }
    strcpyX (fileNaming, entries[inentries[iy]], 0);
    counts = 0;
    for (ix = 0; ix < (int) strlen(fileNaming); ix++) {
       if (fileNaming[ix] == '_' || fileNaming[ix] == '-' ||
           fileNaming[ix] == '.') counts++;
       if (counts == 1) {
         if (fileNaming[ix] == '_') delimiter = '_';
         else if (fileNaming[ix] == '-') delimiter = '-';
         else if (fileNaming[ix] == '.') delimiter = '.';
       }
    }
    if (counts > 0) {
      fields = (char **) cdf_AllocateMemory (sizeof(char *) * counts, FatalError);
      BreakFields (fileNaming, (int) delimiter, fields, counts);
      loc2 = 0;
      for (ix = 0; ix <= counts; ix++) {
        found = FALSE;
        for (iz = 0; iz < lineAttrs; iz++) {
          char *tmp, *ptr;
          tmp = cdf_AllocateMemory (strlen(attrs[iz]), FatalError);
          ptr = strchr (attrs[iz], delimiter);
          if (ptr == NULL) 
            strcpyX (tmp, attrs[iz], 0);
          else {
            int locx = ptr-attrs[iz];
            strcpyX (tmp, attrs[iz], locx);
            strcatX (tmp+locx, attrs[iz]+locx+1, 0);
          }
          if (strncmpIgCasePattern (tmp, fields[ix], strlen(fields[ix])) == 0) {
            found = TRUE;
            iy = iz;
            cdf_FreeMemory (tmp, FatalError);
            break;
          }
          cdf_FreeMemory (tmp, FatalError);
        }
        if (found) {
          strcpyX (fileNaming, entries[inentries[iy]], 0);
          ptr = strchr(fileNaming, '>');
          if (ptr != NULL) loc = (int) (ptr - fileNaming);
          else loc = (int) strlen(fileNaming);
          if (ix == 0)
            strcpyX(fileName, fileNaming, loc);
          else
            strcatX(fileName, fileNaming, loc+loc2);
          fileName[loc+loc2] = delimiter;
          loc2 = loc + loc2 + 1;
          fileName[loc2] = '\0';
        } else {
          return GetOutputFileName2 (name);
        }
      }
      MakeNUL(timeStamp);
      GetDataTimeStamp (timeStamp);
      strcatX(fileName, timeStamp, loc2+8);  
      fileName[loc2+8] = '\0';
      loc2 = loc2 + 8;

      found = FALSE;
      for (ix = 0; ix < lineAttrs; ix++) {
        if (strcmpIgCase (attrs[ix], "data_version") == 1) {
          found = TRUE;
          iy = ix;
          break;
        }
      }
      if (found) {
        strcatX (fileName, "_V", 0);
        if (strlen(entries[inentries[iy]]) == 1) 
          strcatX (fileName, "0", 0);
        strcatX (fileName, entries[inentries[iy]], 0);
      } else
        strcatX (fileName, "V01", 0);
      MakeLowerString (fileName);
      strcpyX (name, fileName, 0);
    } else {
      return GetOutputFileName2 (name);
    }
  } else {
    return GetOutputFileName2 (name);
  }
  return TRUE;
}

/******************************************************************************
* GetOutputFileName.
******************************************************************************/
  
Logical GetOutputFileName (name)
char *name;
{ 
  char fileName[DU_MAX_PATH_LEN+1];
  char sourceName[21], dataType[21], descriptor[21], dataVersion[21];
  long srcLen, typLen, dspLen, verLen;
  char timeStamp[9];
  char *ptr;
  int  loc;
  Logical status2;

  status =  CDFlib (SELECT_, CDF_, idm,
                             ATTR_NAME_, "Logical_file_id",
                             gENTRY_, 0L,
                    GET_, gENTRY_DATA_, fileName,
                    NULL_);
  if (status == CDF_OK) {
    MakeLowerString (fileName);
    strcpyX (name, fileName, 0);
    return TRUE;
  }
  MakeNUL (sourceName);
  MakeNUL (dataType);
  MakeNUL (descriptor);
  MakeNUL (dataVersion);
  MakeNUL (name);

  status =  CDFlib (SELECT_, CDF_, idm,
                             ATTR_NAME_, "Source_name",
                             gENTRY_, 0L,
                    GET_, gENTRY_DATA_, sourceName,
                          gENTRY_NUMELEMS_, &srcLen,
                    NULL_);
  if (status != CDF_OK) errorP++;
 
  status =  CDFlib (SELECT_, ATTR_NAME_, "Data_type",
                    GET_, gENTRY_DATA_, dataType,
                          gENTRY_NUMELEMS_, &typLen,
                    NULL_);
  if (status != CDF_OK) errorP = errorP + 100;

  status =  CDFlib (SELECT_, ATTR_NAME_, "Descriptor",
                    GET_, gENTRY_DATA_, descriptor,
                          gENTRY_NUMELEMS_, &dspLen,
                    NULL_);
  if (status != CDF_OK) errorP = errorP + 10;
  if (errorP != 0) return TRUE;

  status =  CDFlib (SELECT_, ATTR_NAME_, "Data_version",
                    GET_, gENTRY_DATA_, dataVersion,
                          gENTRY_NUMELEMS_, &verLen,
                    NULL_);

  if (!NULstring(sourceName)) {
    sourceName[(int)srcLen] = '\0';
    ptr = strchr(sourceName, '>');
    if (ptr != NULL) loc = (int) (ptr - sourceName);
    else loc = (int) strlen(sourceName);
    strcpyX (fileName, sourceName, loc);
    strcatX (fileName, "_", 0);
  }
  if (!NULstring(descriptor)) {
    descriptor[(int)dspLen] = '\0';
    ptr = strchr(descriptor, '>');
    if (ptr != NULL) loc = (int) (ptr - descriptor);
    else loc = (int) strlen(descriptor);
    strcatX (fileName, descriptor, strlen(fileName)+loc);
    strcatX (fileName, "_", 0);
  }
  if (!NULstring(dataType)) {
    dataType[(int)typLen] = '\0';
    strcatX (fileName, dataType, 0);
    strcatX (fileName, "_", 0);
  }

  MakeNUL(timeStamp);
  status2 = GetDataTimeStamp (timeStamp);
  if (!status2) return status2;
  strcatX(fileName, timeStamp, 0);

  if (!NULstring(dataVersion)) {
    dataVersion[(int)verLen] = '\0';
    strcatX (fileName, "_V", 0);
    if (strlen(dataVersion) == 1)
      strcatX (fileName, "0", 0);
    strcatX (fileName, dataVersion, 0);
  } else
    strcatX (fileName, "_V01", 0);

  MakeLowerString (fileName);
  strcpyX (name, fileName, 0);
  return TRUE;
}    

/******************************************************************************
* GetOutputFileName2.
******************************************************************************/
  
Logical GetOutputFileName2 (name)
char *name;
{ 
  char fileName[DU_MAX_PATH_LEN+1];
  char timeStamp[9];
  int ix, iy;
  Logical found, status2;
  char *ptr;
  int loc;

  found = FALSE;
  MakeNUL (name);

  for (ix = 0; ix < lineAttrs; ix++) {
    if (strcmpIgCase (attrs[ix], "Logical_file_id") == 1) {
      found = TRUE;
      iy = ix;
      break;
    }
  }
  if (found && inentries[iy] != -1) {
    strcpyX (name, entries[inentries[iy]], 0);
    MakeLowerString (name);
    return TRUE;
  }

  found = FALSE;
  for (ix = 0; ix < lineAttrs; ix++) {
    if (strcmpIgCase (attrs[ix], "Source_name") == 1) {
      found = TRUE;
      iy = ix; 
      break;
    }   
  }   

  if (found && inentries[iy] != -1) {
    ptr = strchr(entries[inentries[iy]], '>');
    if (ptr != NULL) loc = (int) (ptr - entries[inentries[iy]]);
    else loc = (int) strlen(entries[inentries[iy]]);
    strcpyX (fileName, entries[inentries[iy]], loc);
  } else {
    errorP++;
  }

  found = FALSE;
  for (ix = 0; ix < lineAttrs; ix++) {
    if (strcmpIgCase (attrs[ix], "Descriptor") == 1) {
      found = TRUE;
      iy = ix;
      break;
    }
  }

  if (found && inentries[iy] != -1) {
    ptr = strchr(entries[inentries[iy]], '>');
    if (ptr != NULL) loc = (int) (ptr - entries[inentries[iy]]);
    else loc = (int) strlen(entries[inentries[iy]]);
    strcatX (fileName, "_", 0);
    strcatX (fileName, entries[inentries[iy]], strlen(fileName)+loc);
    
  } else {
    errorP = errorP + 10;
  }

  found = FALSE;
  for (ix = 0; ix < lineAttrs; ix++) {
    if (strcmpIgCase (attrs[ix], "Data_type") == 1) {
      found = TRUE;
      iy = ix;
      break;
    }
  }

  if (found && inentries[iy] != -1) {
    ptr = strchr(entries[inentries[iy]], '>');
    if (ptr != NULL) loc = (int) (ptr - entries[inentries[iy]]);
    else loc = (int) strlen(entries[inentries[iy]]);
    strcatX (fileName, "_", 0);
    strcatX (fileName, entries[inentries[iy]], strlen(fileName)+loc);
    strcatX (fileName, "_", 0);
  } else {
    errorP = errorP + 100;
  }

  if (errorP > 0) return TRUE;
  MakeNUL(timeStamp);
  status2 = GetDataTimeStamp (timeStamp);
  if (!status2) return status2;
  strcatX(fileName, timeStamp, 0);

  found = FALSE;
  for (ix = 0; ix < lineAttrs; ix++) {
    if (strcmpIgCase (attrs[ix], "data_version") == 1) {
      found = TRUE;
      iy = ix;
      break;
    }
  }
  if (found) {
    strcatX (fileName, "_V", 0);
    if (strlen(entries[inentries[iy]]) == 1)
      strcatX (fileName, "0", 0);
    strcatX (fileName, entries[inentries[iy]], 0);
  } else
    strcatX (fileName, "_V01", 0);

  MakeLowerString (fileName);
  strcpyX (name, fileName, 0);
  return TRUE;
}    

/******************************************************************************
* GetDataTimeStamp.
******************************************************************************/
  
Logical GetDataTimeStamp (timeStamp)
char *timeStamp;
{ 
  char **varNames;
  long numVars, numrVars, numzVars, dataType;
  double epochMin = -1.0E31, epoch16Min[2] = {-1.0E31, -1.0E31};
  double epoch, epoch16[2];
  long long tt2000, tt2000Min = FILLED_TT2000_VALUE;
  long dimSizes[1] = {0}, dimCounts[1] = {1}, dimIntervals[1] = {1};
  char epString[EPOCH16_1_STRING_LEN+1];
  CDFid idi;
  int ix, iy, iz;
  Logical found, zVar;

  for (ix = 0; ix < numCDFs; ix++) {
    varNames = NULL;
    status =  CDFlib (OPEN_, CDF_, CDFpaths[ix], &idi,
                      NULL_);
    if (status != CDF_OK)
	 return QuitCDF ("GetDataTimeStamp", status, CDFpaths[ix]);

    status =  CDFlib (SELECT_, CDF_, idi,
                               CDF_READONLY_MODE_, READONLYon,
                      GET_, CDF_NUMrVARS_, &numrVars,
                            CDF_NUMzVARS_, &numzVars,
                      NULL_);
    if (status != CDF_OK)
	 return QuitCDF ("GetDataTimeStamp", status, "getting numbers of varibales");
    numVars = numrVars + numzVars;
    if (numVars == 0) continue;

    varNames = (char **) cdf_AllocateMemory (sizeof(char *)*(int)numVars, 
                                             FatalError);
    for (iz = 0; iz < numrVars; iz++) {
      status =  CDFlib (SELECT_, rVAR_, (long) iz,
                        GET_, rVAR_NAME_, varName,
                        NULL_);
      if (status != CDF_OK)
	 return QuitCDF ("GetDataTimeStamp", status, "var_name");
      varNames[iz] = cdf_AllocateMemory (strlen(varName)+1, FatalError);
      strcpyX (varNames[iz], varName, 0);
    }

    for (iz = 0, iy = numrVars; iz < numzVars; iz++, iy++) {
      status =  CDFlib (SELECT_, zVAR_, (long) iz,
                        GET_, zVAR_NAME_, varName,
                        NULL_);
      if (status != CDF_OK)
	 return QuitCDF ("GetDataTimeStamp", status, "var_name");
      varNames[iy] = cdf_AllocateMemory (strlen(varName)+1, FatalError);
      strcpyX (varNames[iy], varName, 0);
    }

    found = FALSE;
    for (iz = 0; iz < (int) numVars; iz++) {
/*
      if ((strcmpIgCase (varNames[iz], "epoch") == 1) ||
          (strcmpIgCase (varNames[iz], "range_epoch") == 1)) {
*/
      if (strcmpIgCase (varNames[iz], "range_epoch") == 1) {
        iy = iz;
        found = TRUE;
        break;
      }
    }
    if (found) {
      zVar = (iy >= numrVars);
      if (zVar) iy = iy - numrVars;
      status =  CDFlib (SELECT_, BOO(zVar,zVAR_,rVAR_), (long) iy,
                                 BOO(zVar,zVAR_RECNUMBER_,rVARs_RECNUMBER_), 0L,
                                 BOO(zVar,zVAR_RECCOUNT_,rVARs_RECCOUNT_), 1L,
                                 BOO(zVar,zVAR_RECINTERVAL_,rVARs_RECINTERVAL_), 1L,
                                 BOO(zVar,zVAR_DIMINDICES_,rVARs_DIMINDICES_), dimSizes,
                                 BOO(zVar,zVAR_DIMCOUNTS_,rVARs_DIMCOUNTS_), dimCounts,
                                 BOO(zVar,zVAR_DIMINTERVALS_,rVARs_DIMINTERVALS_), dimIntervals,
                        GET_, BOO(zVar,zVAR_DATATYPE_,rVAR_DATATYPE_), &dataType,
                        NULL_);
      if (status == CDF_OK) {
        if (dataType == CDF_EPOCH) {
          status =  CDFlib (GET_, BOO(zVar,zVAR_HYPERDATA_,rVAR_HYPERDATA_), &epoch,
                            NULL_);
          if (status != CDF_OK) continue;
          if (epochMin == -1.0E31 || epoch < epochMin) epochMin = epoch;
        } else if (dataType == CDF_EPOCH16) {
          status =  CDFlib (GET_, BOO(zVar,zVAR_HYPERDATA_,rVAR_HYPERDATA_), &epoch16,
                            NULL_);
          if (status != CDF_OK) continue;
          if (epoch16Min[0] == -1.0E31 || epoch16[0] < epoch16Min[0] || 
              (epoch16[0] == epoch16Min[0] && epoch16[1] < epoch16Min[1])) {
            epoch16Min[0] = epoch16[0];
            epoch16Min[1] = epoch16[1];
          }
        } else {
          status =  CDFlib (GET_, BOO(zVar,zVAR_HYPERDATA_,rVAR_HYPERDATA_), &tt2000,
                            NULL_);
          if (status != CDF_OK) continue;
          if (tt2000Min == FILLED_TT2000_VALUE || tt2000 < tt2000Min) tt2000Min = tt2000;
	}
      }
    }
    status =  CDFlib (CLOSE_, CDF_, 
                      NULL_);
    if (status != CDF_OK)
	 return QuitCDF ("GetDataTimeStamp", status, "closing CDF");

    if (numVars > 0) {
      for (iy = 0; iy < numVars; iy++)
        cdf_FreeMemory (varNames[iy], FatalError);
      cdf_FreeMemory (varNames, FatalError);
    }
  }
  if (dataType == CDF_EPOCH) {
    if (epochMin != -1.0E31) {
      encodeEPOCH1 (epochMin, epString);
      strcpyX (timeStamp, epString, 8);
    } else
      strcpyX (timeStamp, "00000000", 8);
  } else if (dataType == CDF_EPOCH16) {
    if (epoch16Min[0] != -1.0E31) {
      encodeEPOCH16_1 (epoch16Min, epString);
      strcpyX (timeStamp, epString, 8);
    } else
      strcpyX (timeStamp, "00000000", 8);
  } else {
    if (tt2000Min != FILLED_TT2000_VALUE) {
      encodeTT2000 (tt2000Min, epString, 3);
      strcpyX (timeStamp, epString, 8);
    } else
      strcpyX (timeStamp, "00000000", 8);
  }
  return CDF_OK;
}    

/******************************************************************************
* LoadgAttributes.
******************************************************************************/
                                    
Logical LoadgAttributes (ids, numEntries)
CDFid ids;
int numEntries;
{         

  int i, j, k, ii;
  Logical fnc = FALSE, lfi = FALSE, lsi = FALSE;
  long numgEntries;

  j = 0;
  for (i = 0, ii = 0; ii < (int) numgAttrs; i++) {
    status = CDFlib (SELECT_, CDF_, ids,
                              ATTR_, (long) i,
                     GET_, ATTR_SCOPE_, &scope,
                     NULL_);
    if (status < CDF_OK) continue; 
    if (scope != GLOBAL_SCOPE) continue;
    ++ii;
    status = CDFlib (GET_, ATTR_NAME_, attrName,
                           ATTR_NUMgENTRIES_, &numgEntries,
                     NULL_);
    if (status < CDF_OK) return QuitCDF ("5.0", status, attrName);
    if (strcmpIgCase (attrName, "File_naming_convention") == 1 &&
        numgEntries == 0)
      fnc = TRUE;
    if (strcmpIgCase (attrName, "Logical_file_id") == 1 && 
        numgEntries == 0)
      lfi = TRUE;
    if (strcmpIgCase (attrName, "Logical_source") == 1 &&
        numgEntries == 0)
      lsi = TRUE;

    if (dataOnly) 
      strcpyX (newAttrName, attrName, 0);
    else {
      /****************************************************
      * Check for special attributes. Its                 
      * ISTP-standard name can't be changed.             
      ****************************************************/
      ifound = FALSE;
      if (usePrefix) {
        for (j = 0; j < sp2gAttrs; j++) {
          if (strcmpIgCase (attrName, special2gAttrs[j]) == 1) {
            ifound = TRUE;
            break;
          }
        }
      }
      if (usePrefix && !ifound && (strcmp(attrName, "Parent") != 0 &&
                                   strcmp(attrName, "FileSource") != 0))
        ModifyName (attrName, numTokens, prefixes, icountx, 
                    newAttrName);
      else
        strcpyX (newAttrName, attrName, 0);
    }
    if (strcmp(newAttrName, "FileSource") != 0) {
      status = CDFlib (SELECT_, CDF_, ido,
                       CREATE_, ATTR_, newAttrName, scope, &attrNum,
                       NULL_);
      if (status == ATTR_EXISTS) {
        status = CDFlib (SELECT_, CDF_, ido,
                                  ATTR_NAME_, newAttrName, 
                         GET_,    ATTR_MAXgENTRY_, &maxGentry,
                         NULL_);
        if (status < CDF_OK) return QuitCDF ("6.0", status, newAttrName);
      }
    } else {
      status = CDFlib (SELECT_, CDF_, ido,
                                ATTR_NAME_, "Parent",
                       GET_,    ATTR_MAXgENTRY_, &maxGentry,
                       NULL_);
      if (status < CDF_OK) return QuitCDF ("6.0", status, newAttrName);
      maxGentry++;
    }
    j = k = 0;
    if (numgEntries > 0) {
      do {
        status = CDFlib (SELECT_, CDF_, ids,
                                  ATTR_, (long) i,
                                  gENTRY_, (long) j,
                         GET_, gENTRY_DATATYPE_, &dataTypeX,
                               gENTRY_NUMELEMS_, &numElemsX,
                         NULL_);
        if (status < CDF_OK) {
          j++;
          continue;
        } else {
          nBytes = (size_t) (CDFelemSize(dataTypeX) * numElemsX);
          value = cdf_AllocateMemory ((size_t) nBytes + 1, FatalError);
          status = CDFlib (SELECT_, CDF_, ids,
                                    ATTR_, (long) i,
                                    gENTRY_, (long) j,
                           GET_, gENTRY_DATA_, value,
                           NULL_);
          if (status < CDF_OK) return QuitCDF ("7.0", status, NULL);
          if (STRINGdataType(dataTypeX)) {
            *(((char *)value)+(int)numElemsX) = '\0';
          }
          ifound = FALSE;
          for (ir = 0; ir < sp2gAttrs; ir++) {
            if (strcmpIgCase (newAttrName, special2gAttrs[ir]) == 1) {
              ifound = TRUE;
              break;
            }
          }
          if (!ifound) {
            for (ir = 0; ir < sp3gAttrs; ir++) {
              if (strcmpIgCase (newAttrName, special3gAttrs[ir]) == 1) {
                ifound = TRUE;
                break;
              }
            }
            if (!ifound) {
              if (strcmp(newAttrName, "Parent") == 0 ||
                  strcmp(newAttrName, "FileSource") == 0) {
                char *ptr;
                value1 = cdf_AllocateMemory ((size_t) nBytes + 2, FatalError);
                strcpy(value1, "file");
                snprintf(value1+4, nBytes+1, "%ld: ", (long) (j+icountx));
                ptr = strstr (value, " ");
                snprintf (EofS(value1), nBytes+1, "%s", ptr+1); 
                numElemsX = strlen(value1);
                strcpy (newAttrName, "Parent");
              }
              status = CDFlib (SELECT_, CDF_, ido,
                                        ATTR_NAME_, newAttrName,
                                        gENTRY_, 
                                          ((strcmp(newAttrName,"Parent")==0)? 
                                           (long) (j+icountx-1):(long)j),
                               PUT_, gENTRY_DATA_, dataTypeX, numElemsX,
                                     ((strcmp(newAttrName,"Parent") == 0) ?
                                      value1 : value),
                               NULL_);
              if ((strcmp(newAttrName, "Parent") == 0) && value1 != NULL)
                cdf_FreeMemory(value1, FatalError);
            } else {
              void *valueY;
              long dataTypeY, numElemsY;
              status = CDFlib (SELECT_, CDF_, ido,
                                        ATTR_NAME_, newAttrName,
                                        gENTRY_, (long) (j+maxGentry-1),
                               GET_, gENTRY_DATATYPE_, &dataTypeY,
                                     gENTRY_NUMELEMS_, &numElemsY,
                               NULL_);
              if (status < CDF_OK) {
                ++j; ++k;
		cdf_FreeMemory (value, FatalError);
                continue;
              }
              valueY = cdf_AllocateMemory ((size_t) CDFelemSize(dataTypeY)
                                           * numElemsY + 1, FatalError);
              status = CDFlib (GET_, gENTRY_DATA_, valueY,
                               NULL_);
              if (status < CDF_OK) return QuitCDF ("7.2", status, NULL);
              NULterminateMAX ((char *)value, (size_t) numElemsX);
              NULterminateMAX ((char *)valueY, (size_t) numElemsY);
              if ((dataTypeX != dataTypeY) || (numElemsX != numElemsY) ||
                  (strncmpIgCase (value, valueY, numElemsX) == 0))
                status = CDFlib (SELECT_, CDF_, ido,
                                          ATTR_NAME_, newAttrName,
                                          gENTRY_, (long) (j+maxGentry),
                                 PUT_, gENTRY_DATA_, dataTypeX, numElemsX, value,
                                 NULL_);
              cdf_FreeMemory (valueY, FatalError);
            }
          } else {
            long numE;
            status = CDFlib (SELECT_, CDF_, ido,
                                      ATTR_NAME_, newAttrName,
                                      gENTRY_, (long) 0,
                             NULL_);
            if (status < CDF_OK) return QuitCDF ("7.5", status, newAttrName);

            if (strcmpIgCase (newAttrName, "Logical_source") == 1) {
              int aa, bb;
              aa = 0;
              for (bb = (int) strlen (outputCDF)-1; bb >= 0; bb--) {
                if (outputCDF[bb] == '_') {
                  aa++;
                  if (aa == 2) break;
                }
              } 
              if (bb == -1) {
                if (EndsWithIgCase (outputCDF, ".cdf"))
                  numE = (long) strlen (outputCDF) - 4;
                else
                  numE = (long) strlen (outputCDF);
              } else 
                numE = bb;
              status = CDFlib (PUT_, gENTRY_DATA_, CDF_CHAR, numE, outputCDF,
                               NULL_);
            } else if (strcmpIgCase (newAttrName, "Logical_file_id") == 1) {
              if (EndsWithIgCase (outputCDF, ".cdf"))
                numE = (long) strlen (outputCDF) - 4;
              else
                numE = (long) strlen (outputCDF);
              status = CDFlib (PUT_, gENTRY_DATA_, CDF_CHAR, numE, outputCDF,
                               NULL_);
              printf("  Global attribute: Logical_file_id has been reset to: %*.*s\n",
                                                                (int)numE,
                                                                (int)numE,
                                                                outputCDF);
            } else { /* Generation_date */
              status = CDFlib (PUT_, gENTRY_DATA_, CDF_CHAR, 10L, generationDate,
                               NULL_);
            } 
          }
          if (status < CDF_OK) return QuitCDF ("8.0", status, newAttrName);
          k++;
          j++;
          cdf_FreeMemory (value, FatalError);
        }
      } while (k < (int) numgEntries);
    }
  }

  if (fnc) {
    status = CDFlib (SELECT_, CDF_, ido,
                              ATTR_NAME_, "File_naming_convention",
                              gENTRY_, 0L,
                     PUT_, gENTRY_DATA_, CDF_CHAR, 26L,
                           "source_descriptor_datatype",
                     NULL_);
    if (status != CDF_OK)
	 return QuitCDF ("8.1", status, "File_naming_convention");
  }
  if (lfi) {
    long numE;                    
    if (EndsWithIgCase (outputCDF, ".cdf"))
      numE = (long) strlen (outputCDF) - 4;
    else                                     
      numE = (long) strlen (outputCDF);
    status = CDFlib (SELECT_, CDF_, ido,
                              ATTR_NAME_, "Logical_file_id",
                              gENTRY_, 0L,
                     PUT_, gENTRY_DATA_, CDF_CHAR, numE,
                           outputCDF,
                     NULL_);
    if (status != CDF_OK) return QuitCDF ("8.1a", status, "Logical_file_id");
    printf("  Global attribute: Logical_file_id has been reset to: %*.*s\n",
                                                                  (int)numE,
                                                                  (int)numE,
                                                                  outputCDF);
  }
  if (lsi) {
    int aa, bb;
    long numE;
    aa = 0;
    for (bb = (int) strlen (outputCDF)-1; bb >= 0; bb--) {
      if (outputCDF[bb] == '_') {
        aa++;
        if (aa == 2) break;
      }
    }
    if (bb == -1) {
      if (EndsWithIgCase (outputCDF, ".cdf"))
        numE = (long) strlen (outputCDF) - 4;
      else
        numE = (long) strlen (outputCDF);
    } else
      numE = bb;
    status = CDFlib (SELECT_, CDF_, ido,
                              ATTR_NAME_, "Logical_source",
                              gENTRY_, 0L,
                     PUT_, gENTRY_DATA_, CDF_CHAR, numE,
                           outputCDF,
                     NULL_);
    if (status != CDF_OK) return QuitCDF ("8.1b", status, "Logical_source");
  }
  return TRUE;
}

/******************************************************************************
* LoadgAttributes2.
******************************************************************************/
                                    
Logical LoadgAttributes2 ()
{         

  int i, j, k, l, ii;
  Logical fnc = FALSE, lfi = FALSE, lsi = FALSE;

  for (i = 0; i < lineAttrs; i++) {
    if (strcmpIgCase (attrs[i], "File_naming_convention") == 1) {
      if (inentries[i] == -1)
        fnc = TRUE;
    } else if (strcmpIgCase (attrs[i], "Logical_file_id") == 1) {
      if (inentries[i] == -1)
        lfi = TRUE;
    } else if (strcmpIgCase (attrs[i], "Logical_source") == 1) {
      if (inentries[i] == -1)                                 
        lsi = TRUE;                                           
    }
    status = CDFlib (SELECT_, CDF_, ido,
                     CREATE_, ATTR_, attrs[i], GLOBAL_SCOPE, &attrNum,
                     NULL_);
    if (status == ATTR_EXISTS) continue;
    j = inentries[i];
    if (j == -1) continue;
    k = BOO(i==(lineAttrs-1),lineEntries-1,inentries[i+1]-1); 
    if (k < 0) k = lineEntries-1;
    ii = k - j + 1;
    for (l = 0; l < ii; l++) {
      ifound = FALSE;
      for (ir = 0; ir < sp2gAttrs; ir++) {
        if (strcmpIgCase (attrs[i], special2gAttrs[ir]) == 1) {
          ifound = TRUE;
          break;
        }
      }
      if (!ifound) {
        status = CDFlib (SELECT_, CDF_, ido,
                                  ATTR_, attrNum,
                                  gENTRY_, (long) l,
                         PUT_, gENTRY_DATA_, CDF_CHAR, (long) strlen(entries[j+l]),
                                             entries[j+l],
                         NULL_);
      } else {
        long numE;
        status = CDFlib (SELECT_, CDF_, ido,
                                  ATTR_, attrNum,
                                  gENTRY_, (long) l,
                         NULL_);

        if (strcmpIgCase (attrs[i], "Logical_source") == 1) {
          int aa, bb;
          aa = 0;
          for (bb = (int) strlen (outputCDF)-1; bb >= 0; bb--) {
            if (outputCDF[bb] == '_') {
              aa++;
              if (aa == 2) break;
            }
          } 
          numE = bb;
          status = CDFlib (PUT_, gENTRY_DATA_, CDF_CHAR, numE,                 
                                 outputCDF,
                           NULL_);
        } else if (strcmpIgCase (attrs[i], "Logical_file_id") == 1) {
          if (EndsWithIgCase (outputCDF, ".cdf"))
            numE = (long) strlen (outputCDF) - 4;
          else
            numE = (long) strlen (outputCDF);
          status = CDFlib (PUT_, gENTRY_DATA_, CDF_CHAR, numE,
                                 outputCDF,
                           NULL_);
          printf("  Global attribute: Logical_file_id has been reset to: %*.*s\n",
                                                              (int)numE,
                                                              (int)numE,
                                                              outputCDF);
        } else { /* Generation_date */
          status = CDFlib (PUT_, gENTRY_DATA_, CDF_CHAR, 10L,
                                 generationDate,
                           NULL_); 
        }
      }
      if (status < CDF_OK) return QuitCDF ("8.0a", status, attrs[i]);
    }
  }
  if (fnc) {
    status = CDFlib (SELECT_, CDF_, ido,
                              ATTR_NAME_, "File_naming_convention",
                              gENTRY_, 0L,
                     PUT_, gENTRY_DATA_, CDF_CHAR, 26L,
                           "source_descriptor_datatype",
                     NULL_);
    if (status != CDF_OK) return QuitCDF ("8.2", status, "File_naming_convention");
  }

  if (lfi) {
    long numE;
    if (EndsWithIgCase (outputCDF, ".cdf"))
      numE = (long) strlen (outputCDF) - 4;
    else
      numE = (long) strlen (outputCDF);
    status = CDFlib (SELECT_, CDF_, ido,
                              ATTR_NAME_, "Logical_file_id",
                              gENTRY_, 0L,
                     PUT_, gENTRY_DATA_, CDF_CHAR, numE,
                           outputCDF,
                     NULL_);
    if (status != CDF_OK) return QuitCDF ("8.2b", status, "Logical_file_id");
  }
  
  if (lsi) {
    int aa, bb;
    long numE;
    aa = 0;
    for (bb = (int) strlen (outputCDF)-1; bb >= 0; bb--) {
      if (outputCDF[bb] == '_') {
        aa++;
        if (aa == 2) break;
      }
    }
    numE = bb;
    status = CDFlib (SELECT_, CDF_, ido,
                              ATTR_NAME_, "Logical_source",
                              gENTRY_, 0L,
                     PUT_, gENTRY_DATA_, CDF_CHAR, numE,
                           outputCDF,
                     NULL_);
    if (status != CDF_OK) return QuitCDF ("8.2c", status, "Logical_source");
  }
  return TRUE;
}

/******************************************************************************
* BreakFields.
******************************************************************************/
                                    
void BreakFields (naming, delimiter, fields, counts)          
char *naming;
int  delimiter;
char **fields;
int counts;
{                                
                         
  char *ptr1;
  int loc1, loc2;
  int ii;
  loc1 = 0;
  for (ii = 0; ii <= counts; ii++) {
    ptr1 = strchr(naming+loc1, (char) delimiter);
    if (ptr1 == NULL) {
      fields[ii] = (char *) cdf_AllocateMemory ((int) strlen(naming+loc1)+1, 
                                                FatalError);
      strcpyX (fields[ii], naming+loc1, 0);
    } else {
      loc2 = (int) (ptr1 - (naming+loc1));
      fields[ii] = (char *) cdf_AllocateMemory (loc2+1, FatalError);
      strcpyX (fields[ii], naming+loc1, loc2);
      loc1 = loc1 + loc2 + 1;
    }
  }
}

/******************************************************************************
* LoadMasterTextFile.
******************************************************************************/

Logical LoadMasterTextFile (masterFile)
char *masterFile;
{
  FILE *masterfp;
  int  i, maxAttr, maxEntry;
  char lineX[DU_MAX_PATH_LEN+1+1], item[DU_MAX_PATH_LEN+1];
  char *ptr, *ptr2;
  char **tmp;
  int  *tmp2;
  Logical ib;

  masterfp = fopen (masterFile, "r");
  if (masterfp == NULL) return FALSE;

  lineAttrs = lineEntries = 0;
  maxAttr = maxEntry = 50;
  attrs = (char **) cdf_AllocateMemory (sizeof(char *)*maxAttr, FatalError);
  inentries = (int *) cdf_AllocateMemory (sizeof(int)*maxAttr, FatalError);
  entries = (char **) cdf_AllocateMemory (sizeof(char *)*maxEntry, FatalError);
  ib = FALSE;
  while (fgets(lineX,DU_MAX_PATH_LEN+1,masterfp) != NULL) {
    if (sscanf(lineX, "%s\n", item) != 1) continue;
    for (i = 0; lineX[i] != NUL; i++)
       if (lineX[i] == Nl) lineX[i] = '\0'; 
    RemoveLeadingBlanks (lineX);
    RemoveTrailingBlanks (lineX);
    ptr = strchr (lineX, ':');
    ptr2 = strchr (lineX, ' ');
    if (ptr != NULL && ptr2 == NULL && lineX[(int)strlen(lineX)-1] == ':') { 
      if (ib == TRUE) {
      /* No entry entered for the previous attribute... */
        inentries[lineAttrs-1] = -1;
      }
      if (lineAttrs == maxAttr) {
        tmp = (char **) cdf_AllocateMemory (sizeof(char *)*(maxAttr+50), 
                                            FatalError);
        for (i = 0; i < maxAttr; i++) {
          tmp[i] = (char *) cdf_AllocateMemory (strlen(attrs[i])+1, FatalError);
          strcpyX (tmp[i], attrs[i], 0);
          cdf_FreeMemory (attrs[i], FatalError);
        }
        cdf_FreeMemory (attrs, FatalError);         
        attrs = tmp;

        tmp2 = (int *) cdf_AllocateMemory (sizeof(int)*(maxAttr+50), 
                                           FatalError);
        for (i = 0; i < maxAttr; i++)
          tmp2[i] = inentries[i];
        cdf_FreeMemory (inentries, FatalError);
        inentries = tmp2;
        maxAttr = maxAttr + 50;
      }
      attrs[lineAttrs] = (char *) cdf_AllocateMemory (strlen(lineX)+1, FatalError);
      strcpyX (attrs[lineAttrs], lineX, strlen(lineX)-1);
      lineAttrs++;
      ib = TRUE;
    } else {
      if (lineEntries == maxEntry) {
        tmp = (char **) cdf_AllocateMemory (sizeof(char *)*(maxEntry+50),
                                            FatalError);
        for (i = 0; i < maxEntry; i++) {
          tmp[i] = (char *) cdf_AllocateMemory (strlen(entries[i])+1, FatalError);
          strcpyX (tmp[i], entries[i], 0);
          cdf_FreeMemory (entries[i], FatalError);
        }
        cdf_FreeMemory (entries, FatalError);
        entries = tmp;
        maxEntry = maxEntry + 50;
      }
      entries[lineEntries] = (char *) cdf_AllocateMemory (strlen(lineX)+1, FatalError);
      strcpyX (entries[lineEntries], lineX, 0);
      if (ib) {
        inentries[lineAttrs-1] = lineEntries;
        ib = FALSE;
      }
      lineEntries++;
    }
  }
  if (ib == TRUE) inentries[lineAttrs-1] = -1;

  fclose (masterfp);
  return TRUE;
}
