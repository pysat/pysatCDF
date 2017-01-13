/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                         CDFedit, part 0 (main).
*
*  Version 1.3b, 16-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  25-Jan-94, J Love     Original version.
*   V1.0a  4-Feb-94, J Love     DEC Alpha/OpenVMS port.
*   V1.1  15-Dec-94, J Love     CDF V2.5.
*   V1.1a 10-Jan-95, J Love	Uppercase file extensions on the Macintosh.
*   V1.1b 23-Jan-95, J Love	IRIX 6.x (64-bit).
*   V1.1c 28-Feb-95, J Love	Pass `char' as `int'.
*   V1.2  11-Apr-95, J Love	POSIX.
*   V1.2a 18-Apr-95, J Love	More POSIX.
*   V1.2b  6-Sep-95, J Love	CDFexport-related changes.  FSI key
*				definitions.
*   V1.2c 28-Sep-95, J Love	Macintosh dialog filtering.  Outline default
*				button.
*   V1.3  22-Aug-96, J Love	CDF V2.6.
*   V1.3a 21-Feb-97, J Love	Removed RICE.
*   V1.4b 16-Nov-97, J Love	Windows NT/Visual C++.
*   V1.5  11-Jul-05, M Liu      Added MingW port for PC.
*   V1.6  03-May-06, M Liu      Added checksum option for the files.
*   V1.7  13-Oct-06, M Liu      Changed to allow upper and lower case CDF  
*                               name to be used on win32.
*   V3.3  10-Jan-09, M Liu      Validate a file before it is used.
*
******************************************************************************/

#define CDFEDIT
#include "cdfedit.h"

#if defined(vms) || defined(unix) || defined(posixSHELL)
#define BROWSEaware 1
#else
#define BROWSEaware 0
#endif

/******************************************************************************
* Increased stack size and overlay buffer for Borland C on IBM PC.
******************************************************************************/

#if defined(BORLANDC)
extern unsigned _stklen = BORLANDC_STACK_SIZE;
extern unsigned _ovrbuffer = BORLANDC_OVERLAY_SIZE;
#endif

/******************************************************************************
* Global variables local to this source file.
******************************************************************************/

AOSs2A (openingLines, "Opening CDF...", "")
AOSs2B (closingLines, "Closing CDF...", "")
AOSs2C (delLines, "Deleting CDF...", "")

/******************************************************************************
* Main.
******************************************************************************/

#if !defined(win32)
MAIN {
  Logical success = TRUE;
  strcpyX (pgmName, "CDFedit", MAX_PROGRAM_NAME_LEN);
#if defined(mac)
  MacExecuteFSI (EditCDFs, EditQOPs);
#else
  success = EditCDFs (argc, argv);
#endif
#if defined(DEBUG)
  if (cdf_FreeMemory(NULL,FatalError) > 0) DisplayWarning ("Abandoned buffers.");
#else
  cdf_FreeMemory (NULL, FatalError);
#endif
  return BOO(success,EXIT_SUCCESS_,EXIT_FAILURE_);
}
#endif

/******************************************************************************
* EditCDFs.
******************************************************************************/

Logical EditCDFs (argC, argV)
int argC;
char *argV[];
{
   QOP *qop;
   static char *validQuals[] = {
     "browse", "nobrowse", "zmode", "format", "noformat", "prompt",
     "noprompt", "report", "neg2posfp0", "noneg2posfp0", "cache",
     "statistics", "nostatistics", "gwithentries", "nogwithentries",
     "vwithentries", "novwithentries", "about", NULL
   };
   static int optRequired[] = {
     FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE,
     FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 0
   };
   static char *reportTokens[] = { "errors", "warnings", "informationals" };
   char CDFspec[DU_MAX_PATH_LEN+1];
   Logical useFormat, promptForSpec, negToPosFp0, status;
   long zMode, workingCache, stageCache, compressCache;
   Logical qopError = FALSE;
   /***************************************************************************
   * Check qualifiers/options/parameters.
   ***************************************************************************/
   switch (argC) {
     case 1:
       InitializeScreen ();
       OnlineHelpWindow (olhFile, OLHhelpID);
       CleanupScreen ();
       return TRUE;
     default:
       qop = Qop (argC, argV, validQuals, optRequired);
       if (qop == NULL) return FALSE;
       /***********************************************************************
       * Check for `about' qualifier.
       ***********************************************************************/
       if (qop->qualEntered[ABOUTqual]) {
	 DisplayIdentification (pgmName);
	 cdf_FreeMemory (qop, FatalError);
	 return TRUE;
       }
       /***********************************************************************
       * Get CDF path parameter.
       ***********************************************************************/
       switch (qop->Nparms) {
	 case 0:
	   CDFspec[0] = NUL;
	   break;
	 case 1:
	   strcpyX (CDFspec, qop->parms[CDFSPECparm], DU_MAX_PATH_LEN);
#if defined(vms) || defined(dos)
	   MakeUpperString (CDFspec);
#endif
	   break;
	 default:
	   DisplayError ("Too many parameters.");
	   qopError = TRUE;
	   break;
       }
       /***********************************************************************
       * Check for `browse', `format', `prompt', `neg2posfp0', and `statistics'
       * qualfiers.
       ***********************************************************************/
       qopError = qopError | !TFqualifier(qop,&browseOnly,BROWSEqual,
					  NOBROWSEqual,DEFAULTbrowseEDIT,
					  "browse");
       qopError = qopError | !TFqualifier(qop,&useFormat,FORMATqual,
					  NOFORMATqual,DEFAULTformatEDIT,
					  "format");
       qopError = qopError | !TFqualifier(qop,&promptForSpec,PROMPTqual,
					  NOPROMPTqual,DEFAULTpromptEDIT,
					  "prompt");
       qopError = qopError | !TFqualifier(qop,&negToPosFp0,NEG2POSFP0qual,
					  NONEG2POSFP0qual,DEFAULT_NEGtoPOSfp0,
					  "neg2posfp0");
       qopError = qopError | !TFqualifier(qop,&dumpStatistics,STATSqual,
					  NOSTATSqual,DEFAULTstatsEDIT,
					  "statistics");
       qopError = qopError | !TFqualifier(qop,&gAttrsAndEntries,gWITHqual,
					  NOgWITHqual,DEFAULTgWithEDIT,
					  "gwithentries");
       qopError = qopError | !TFqualifier(qop,&vAttrsAndEntries,vWITHqual,
					  NOvWITHqual,DEFAULTvWithEDIT,
					  "vwithentries");
       /***********************************************************************
       * Check for zMode qualifier.
       ***********************************************************************/
       if (qop->qualEntered[ZMODEqual]) {
	 switch (qop->qualOpt[ZMODEqual][0]) {
	   case '0': zMode = zMODEoff; break;
	   case '1': zMode = zMODEon1; break;
	   case '2': zMode = zMODEon2; break;
	   default: {
	     char tempS[MAX_MESSAGE_TEXT_LEN+1];
	     snprintf (tempS, (size_t) sizeof(tempS),
		       "Illegal zMode (%s).", qop->qualOpt[ZMODEqual]);
	     DisplayError (tempS);
	     qopError = TRUE;
	   }
	 }
       }
       else
	 zMode = DEFAULTzModeEDIT;
       /***********************************************************************
       * Check for `cache' qualifier.
       ***********************************************************************/
       if (qop->qualEntered[CACHEqual]) {
	 if (!ParseCacheSizes(qop->qualOpt[CACHEqual],
			      &workingCache,&stageCache,&compressCache)) {
	   DisplayError ("Illegal cache size/type.");
	   qopError = TRUE;
	 }
       }
       else {
	 workingCache = useDEFAULTcacheSIZE;
	 stageCache = useDEFAULTcacheSIZE;
	 compressCache = useDEFAULTcacheSIZE;
       }
       /***********************************************************************
       * Check for `report' qualifier.  If absent, use defaults.
       ***********************************************************************/
       if (qop->qualEntered[REPORTqual]) {
	 if (!ParseOptionList(3,reportTokens,
			      qop->qualOpt[REPORTqual],report)) {
	   DisplayError ("Illegal list of `report' options.");
	   qopError = TRUE;
	 }
       }
       else {
	 report[ERRORs] = REPORTerrorsDEFAULT;
	 report[WARNs] = REPORTwarningsDEFAULT;
	 report[INFOs] = REPORTinfosDEFAULT;
       }
       /***********************************************************************
       * Check for missing/conflicting parameters/qualifiers.
       ***********************************************************************/
       if (qop->Nparms < 1 && (!promptForSpec)) {
	 DisplayError ("Enter CDF(s) specification or `prompt' qualifier.");
	 qopError = TRUE;
       }
       /***********************************************************************
       * Free QOP memory and check for an error.
       ***********************************************************************/
       cdf_FreeMemory (qop, FatalError);
       if (qopError) return FALSE;
       break;
   }
   /***************************************************************************
   * Initialize.
   ***************************************************************************/
   if (browseOnly) {
     strcpyX (pgmName, "CDFbrowse", MAX_PROGRAM_NAME_LEN);
     strcpyX (ilhFile, "cdfbrow.ilh", MAX_ILHFILE_LEN);
   }
   else
     strcpyX (ilhFile, "cdfedit.ilh", MAX_ILHFILE_LEN);
#if defined(linux)
   /* Set it to VT100 to eleminate the strange behaviour by BS & DELA key on
      long value/tring, e.g., an INT8, on xterm|xterm-256color terminal type */
   setenv ("TERM", "vt100", 1);
#endif
   InitializeScreen ();
   /***************************************************************************
   * Edit CDF(s).
   ***************************************************************************/
/*   CDFsetValidate (VALIDATEFILEon); */
   if (promptForSpec)
     status = SpecificationPrompt (CDFspec, useFormat, negToPosFp0, zMode,
				   workingCache, stageCache, compressCache);
   else
     if (IsDir(CDFspec) || IsWild(CDFspec))
       status = EditCDFsMenu (CDFspec, negToPosFp0, useFormat, zMode,
			      workingCache, stageCache, compressCache);
     else
       status = TryToEditCDF (CDFspec, negToPosFp0, useFormat, zMode,
			      workingCache, stageCache, compressCache);
   /***************************************************************************
   * Clean up screen and return.
   ***************************************************************************/
   CleanupScreen ();
   return status;
}

/******************************************************************************
* SpecificationPrompt.
******************************************************************************/

Logical SpecificationPrompt (CDFspec, useFormat, negToPosFp0, zMode,
			     workingCache, stageCache, compressCache)
char CDFspec[DU_MAX_PATH_LEN+1];
Logical useFormat;
Logical negToPosFp0;
long zMode;
long workingCache;
long stageCache;
long compressCache;
{
  for (;;) {
    if (NULstring(CDFspec)) strcpyX (CDFspec,CURRENTDIRECTORY,DU_MAX_PATH_LEN);
    if (!PromptForSpec(CDFspec)) break;
#if defined(vms) || defined(dos)
    MakeUpperString (CDFspec);
#endif
    if (IsDir(CDFspec) || IsWild(CDFspec)) {
      /************************************************************************
      * Directory/wildcard (more than one CDF).
      ************************************************************************/
      if (!EditCDFsMenu(CDFspec,negToPosFp0,useFormat,
			zMode,workingCache,stageCache,
			compressCache)) return FALSE;
    }
    else {
      /************************************************************************
      * Assumed to be a complete specification of a single CDF.
      ************************************************************************/
      TryToEditCDF (CDFspec, negToPosFp0, useFormat, zMode, workingCache,
		    stageCache, compressCache);
    }
  }
  return TRUE;
}

/******************************************************************************
* EditCDFsMenu.
******************************************************************************/

Logical EditCDFsMenu (CDFspec, negToPosFp0, useFormat, zMode, workingCache,
		      stageCache, compressCache)
char *CDFspec;
Logical negToPosFp0;
Logical useFormat;
long zMode;
long workingCache;
long stageCache;
long compressCache;
{
  CDFstatus status; int nCDFs;
  char **dirS, **CDFs;
  char CDFdir[DU_MAX_DIR_LEN+1], CDFname[DU_MAX_NAME_LEN+1];
  char CDFpathT[DU_MAX_PATH_LEN+1];
  char CDFfullPath[DU_MAX_PATH_LEN+1];
  static char label[16+DU_MAX_PATH_LEN+2+1];
  static Logical first = TRUE;
  AOSs1 (header, BLANKs78)
  AOSs2 (trailerEdit,
	 "Edit:   ________   Exit:   ________   Help: ________",
	 "Create: ________   Delete: ________   Info: ________")
  AOSs1A (trailerBrowse,
	  "Browse: ________  Exit: ________  Info: ________  Help: ________")
  static int exitCharsEdit[] = {
    ENTERkey_FSI, EXITkey_FSI, CREATECDFkey_EDIT, DELETECDFkey_EDIT,
    HELPkey_FSI, INFOkey_EDIT, NUL
  };
  static int exitCharsBrowse[] = {
    ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, INFOkey_EDIT, NUL
  };
  static struct ItemWindowStruct IW = {
     0, 0, 80, label, 1, header, 0, NULL, 0, NULL, NULL, NULL, 0, 0, NULL,
     NULL, REFRESHkey_FSI, TRUE, NSkey_FSI, PSkey_FSI
  };
  AOSs6 (infoLines, BLANKs78, BLANKs78, BLANKs78,
	 " ", "Enter any key to continue...", "")
  static char noneMsg[] = {
    "No CDF selected (no CDFs exist for this specification)."
  };
  /****************************************************************************
  * First time...
  ****************************************************************************/
  if (first) {
    EncodeKeyDefinitions (1, trailerBrowse, ENTERkey_FSI, EXITkey_FSI,
			    INFOkey_EDIT, HELPkey_FSI);
    EncodeKeyDefinitions (2, trailerEdit, ENTERkey_FSI, EXITkey_FSI,
			    HELPkey_FSI, CREATECDFkey_EDIT, DELETECDFkey_EDIT,
			    INFOkey_EDIT);
    first = FALSE;
  }
  if (browseOnly) {
    IW.NiRows = 15;
    IW.NtLines = 1;
    IW.tLines = trailerBrowse;
    IW.exitChars = exitCharsBrowse;
  }
  else {
    IW.NiRows = 14;
    IW.NtLines = 2;
    IW.tLines = trailerEdit;
    IW.exitChars = exitCharsEdit;
  }

  ParsePath (CDFspec, CDFdir, CDFname);
  BuildCDFsMenu (CDFspec, &nCDFs, &dirS, &CDFs, &IW);
  ItemWindow (NEWiw, &IW, 0);
  for (;;) {
     ItemWindow (READiw, &IW);
     switch (IW.key) {
       /***********************************************************************
       * Edit currently selected CDF.  The CDFs menu is rebuild/updated in
       * case the CDF was deleted.  When the CDFs menu is updated, the new
       * current item is set to the old current item (or the last item in
       * case more than one CDF was deleted [possibly by another user]).
       * If the CDF was deleted, the new current item should be the CDF
       * that followed it in the list (unless it was the last CDF in which
       * case the new current item will be the last CDF in the new list).
       ***********************************************************************/
       case ENTERkey_FSI:
	 if (IW.nItems == 0) {
	   ProblemWindow (noneMsg, FALSE);
	 }
	 else {
	   CDFid id; int lastItemN;
	   Logical isLFS;
	   strcpyX (CDFpathT, dirS[IW.itemN], DU_MAX_PATH_LEN);
	   AppendToDir (CDFpathT, CDFs[IW.itemN]);
	   status = CheckLFS (CDFpathT, &isLFS, CDFfullPath);
	   compressed = CompressedCDF (CDFfullPath);
	   if (compressed) {
	     MessageWindow (openingLines, NULL, LogicalFALSE);
	     zzzzz (1.0);
	   }
	   status = CDFlib (OPEN_, CDF_, CDFfullPath, &id,
			    NULL_);
	   if (compressed) MessageWindow (NULL);
	   ReportStatus (status, FALSE);
	   if (StatusBAD(status)) break;
	   ItemWindow (UNDISPLAYiw, &IW);
	   EditCDF (CDFs[IW.itemN], useFormat, workingCache, stageCache,
		    compressCache, zMode, negToPosFp0);
	   FreeCDFsMenu (dirS, CDFs, &IW);
	   BuildCDFsMenu (CDFspec, &nCDFs, &dirS, &CDFs, &IW);
	   lastItemN = IW.nItems - 1;
	   ItemWindow (UPDATEiw, &IW, MINIMUM(IW.itemN,lastItemN));
	   ItemWindow (REDISPLAYiw, &IW);
	 }
	 break;
       /***********************************************************************
       * Display `info' for currently selected CDF.
       ***********************************************************************/
       case INFOkey_EDIT:
	 if (IW.nItems == 0) {
	   ProblemWindow (noneMsg, FALSE);
	 }
	 else {
	   long cType, cParms[CDF_MAX_PARMS], cFileSize1, uFileSize1;
	   OFF_T cFileSize2, uFileSize2;
	   Logical isLFS;
	   strcpyX (CDFpathT, dirS[IW.itemN], DU_MAX_PATH_LEN);
	   AppendToDir (CDFpathT, CDFs[IW.itemN]);
	   status = CheckLFS(CDFpathT, &isLFS, CDFfullPath);
	   if (isLFS)
	     status = CDFlib (GET_, CDF_INFO_, CDFfullPath, &cType, cParms,
					       &cFileSize1, &uFileSize1,
			      NULL_);
	   else
             status = CDFlib (GET_, CDF_INFO_, CDFfullPath, &cType, cParms,
                                               &cFileSize2, &uFileSize2,
                              NULL_);
	   ReportStatus (status, FALSE);
	   if (StatusOK(status)) {
	     char label[DU_MAX_NAME_LEN+13+1];
	     snprintf (label, (size_t) sizeof(label),
		       " Info for `%s' ", CDFs[IW.itemN]);
	     snprintf (infoLines[0], (size_t) sizeof(BLANKs78),
		       "Compression:  %s", CompressionToken(cType,cParms));
	     if (cType == NO_COMPRESSION) {
	       if (isLFS)
#if !defined(win32) && !defined(__MINGW32__)
	         snprintf (infoLines[1], (size_t) sizeof(BLANKs78),
			   "Uncompressed: %lld bytes", (long long) uFileSize2);
#else
                 snprintf (infoLines[1], (size_t) sizeof(BLANKs78),
			   "Uncompressed: %I64d bytes", (long long) uFileSize2);
#endif
	       else
		 snprintf (infoLines[1], (size_t) sizeof(BLANKs78),
			   "Uncompressed: %ld bytes", uFileSize1);
	       MakeNUL (infoLines[2]);
	     }
	     else {
	       if (isLFS) {
                 snprintf (EofS(infoLines[0]), 
			   (size_t) sizeof(BLANKs78)-strlen(infoLines[0]),
			   " (%ld%%)",
                           (long) (((OFF_T)100 * cFileSize2) / uFileSize2));
#if !defined(win32) && !defined(__MINGW32__)
	         snprintf (infoLines[1], (size_t) sizeof(BLANKs78),
			   "Compressed:   %lld bytes", (long long) cFileSize2);
	         snprintf (infoLines[2], (size_t) sizeof(BLANKs78),
			   "Uncompressed: %lld bytes", (long long) uFileSize2);
#else
                 snprintf (infoLines[1], (size_t) sizeof(BLANKs78),
			   "Compressed:   %I64d bytes", (long long) cFileSize2);
                 snprintf (infoLines[2], (size_t) sizeof(BLANKs78),
			   "Uncompressed: %I64d bytes", (long long) uFileSize2);
#endif
	       } else {
		 snprintf (EofS(infoLines[0]), 
			   (size_t) sizeof(BLANKs78)-strlen(infoLines[0]),
			   " (%ld%%)",
			   ((100L * cFileSize1) / uFileSize1));
		 snprintf (infoLines[1], (size_t) sizeof(BLANKs78),
			   "Compressed:   %ld bytes", cFileSize1);
		 snprintf (infoLines[2], (size_t) sizeof(BLANKs78),
			   "Uncompressed: %ld bytes", uFileSize1);
	       }
	     }
	     MessageWindow (infoLines, label, LogicalTRUE);
	     MessageWindow (NULL);
	   }
	 }
	 break;
       /***********************************************************************
       * Create a CDF.
       * Note that `CreateCDF' will undisplay this window (if necessary).
       ***********************************************************************/
       case CREATECDFkey_EDIT: {
	 char CDFnameT[DU_MAX_NAME_LEN+1]; int itemN;
	 strcpyX (CDFnameT, "", DU_MAX_NAME_LEN);
	 CreateCDF (CDFdir, CDFnameT, zMode, useFormat, negToPosFp0,
		    workingCache, stageCache, compressCache, &IW);
	 FreeCDFsMenu (dirS, CDFs, &IW);
	 BuildCDFsMenu (CDFspec, &nCDFs, &dirS, &CDFs, &IW);
	 itemN = IndexInList (CDFnameT, nCDFs, CDFs);
	 ItemWindow (UPDATEiw, &IW, BOO(itemN < 0,0,itemN));
	 ItemWindow (REDISPLAYiw, &IW);
	 break;
       }
       /***********************************************************************
       * Delete currently selected CDF.  When the CDFs menu is updated, the
       * new current item is set to the old current item (or the last item
       * in case more than one CDF was deleted [possibly by another user]).
       * The new current item should be the CDF (in the list) that followed
       * the CDF that was deleted (unless it was the last CDF in which case
       * the new current item will be the last CDF in the new list).
       ***********************************************************************/
       case DELETECDFkey_EDIT:
	 if (IW.nItems == 0) {
	   ProblemWindow (noneMsg, FALSE);
	 }
	 else {
	   char delim, question[DU_MAX_NAME_LEN+15+1];
	   delim = PickDelimiter (CDFs[IW.itemN],strlen(CDFs[IW.itemN]));
	   snprintf (question, (size_t) sizeof(question),
		     "Delete CDF %c%s%c ?", delim, CDFs[IW.itemN], delim);
	   if (ConfirmWindow(3,78,question,NULL,FALSE,DELETECDFhelpID)) {
	     CDFid id; CDFstatus status; int lastItemN;
	     Logical isLFS;
	     strcpyX (CDFpathT, dirS[IW.itemN], DU_MAX_PATH_LEN);
	     AppendToDir (CDFpathT, CDFs[IW.itemN]);
             status = CheckLFS (CDFpathT, &isLFS, CDFfullPath);
             compressed = CompressedCDF (CDFfullPath);
	     if (compressed) {
	       MessageWindow (delLines, NULL, LogicalFALSE);
	       zzzzz (1.0);
	     }
	     status = CDFlib (OPEN_, CDF_, CDFfullPath, &id,
			      DELETE_, CDF_,
			      NULL_);
	     if (compressed) MessageWindow (NULL);
	     ReportStatus (status, FALSE);
	     FreeCDFsMenu (dirS, CDFs, &IW);
	     BuildCDFsMenu (CDFspec, &nCDFs, &dirS, &CDFs, &IW);
	     lastItemN = IW.nItems - 1;
	     ItemWindow (UPDATEiw, &IW, MINIMUM(IW.itemN,lastItemN));
	   }
	 }
	 break;
       /***********************************************************************
       * Display online help.
       ***********************************************************************/
       case HELPkey_FSI:
	 OnlineHelpWindow (ilhFile, CDFShelpID);
	 break;
       /***********************************************************************
       * Exit.
       ***********************************************************************/
       case EXITkey_FSI:
	 ItemWindow (DELETEiw, &IW);
	 FreeCDFsMenu (dirS, CDFs, &IW);
	 return TRUE;
     }
  }
}

/******************************************************************************
* BuildCDFsMenu.
******************************************************************************/

void BuildCDFsMenu (CDFspec, nCDFs, dirS, CDFs, IW)
char *CDFspec;
int *nCDFs;
char ***dirS;
char ***CDFs;
struct ItemWindowStruct *IW;
{
  int CDFn;
  snprintf (IW->label, (size_t) 16+DU_MAX_PATH_LEN+2+1,
	    " Specification \"%s\" ", CDFspec);
  *nCDFs = CDFdirList (CDFspec, dirS, CDFs);
  snprintf (IW->hLines[0], (size_t) sizeof(BLANKs78),
	    "%d CDF%s", *nCDFs, (*nCDFs <= 1 ? "" : "s"));
  if (*nCDFs > 0) {
    AllocIW (IW, *nCDFs, *nCDFs, CDFs_NAME_LEN, FatalError);
    for (CDFn = 0; CDFn < *nCDFs; CDFn++) {
       size_t nameLen = strlen((*CDFs)[CDFn]);
       strcpyX (IW->iLines[CDFn], (*CDFs)[CDFn], CDFs_NAME_LEN);
       IW->iLineNs[CDFn] = CDFn;
       IW->iCols[CDFn] = 0;
       IW->iLens[CDFn] = (int) MINIMUM(CDFs_NAME_LEN,nameLen);
    }
  }
  else {
    IW->NiLines = 0;
    IW->nItems = 0;
  }
  return;
}

/******************************************************************************
* FreeCDFsMenu.
******************************************************************************/

void FreeCDFsMenu (dirS, CDFs, IW)
char **dirS;
char **CDFs;
struct ItemWindowStruct *IW;
{
  if (dirS != NULL) cdf_FreeMemory (dirS, FatalError);
  if (CDFs != NULL) cdf_FreeMemory (CDFs, FatalError);
  FreeIW (IW, FatalError);
  return;
}

/******************************************************************************
* TryToEditCDF.
******************************************************************************/

Logical TryToEditCDF (CDFspec, negToPosFp0, useFormat, zMode, workingCache,
		      stageCache, compressCache)
char *CDFspec;
Logical negToPosFp0;
Logical useFormat;
long zMode;
long workingCache;
long stageCache;
long compressCache;
{
  CDFstatus status; CDFid id;
  char CDFdir[DU_MAX_DIR_LEN+1], CDFname[DU_MAX_NAME_LEN+1];
  char CDFfullName[DU_MAX_NAME_LEN+1];
  Logical isLFS;
  ParsePath (CDFspec, CDFdir, CDFname);
  status = CheckLFS (CDFspec, &isLFS, CDFfullName);
  compressed = FALSE;
  if (status == CDF_OK) {
    compressed = CompressedCDF (CDFfullName);
    if (compressed) {
      MessageWindow (openingLines, NULL, LogicalFALSE);
      zzzzz (1.0);
    }
  }
  status = CDFlib (OPEN_, CDF_, CDFfullName, &id,
		   NULL_);
  if (compressed) MessageWindow (NULL);
  switch (status) {
    case NO_SUCH_CDF:
      if (!browseOnly) {
	char delim, question[DU_MAX_PATH_LEN+29+1];
	int helpID;
	delim = PickDelimiter(CDFfullName, strlen(CDFfullName));
	if (CDFgetFileBackwardEnvVar() == 1) {
	  snprintf (question, (size_t) sizeof(question),
	            "CDF %c%s%c does not exist.  Create?", delim,
	            CDFfullName, delim);
	} else {
	  snprintf (question, (size_t) sizeof(question),
	            "CDF %c%s%c does not exist.  Create(V3 or V2.7)?", delim,
		    CDFfullName, delim);
	  helpID = CREATECDFV23helpID;
	}
	if (ConfirmWindow2(3,78,question,NULL,TRUE,helpID)) {
	  if (!CreateCDF(CDFdir,CDFname,zMode,
			 useFormat,negToPosFp0,
			 workingCache, stageCache,
			 compressCache,NULL)) return FALSE;
	}
      }
      else
	ProblemWindow ("CDF does not exist.", TRUE);
      break;
    default:
      ReportStatus (status, TRUE);
      if (StatusBAD(status)) return FALSE;
      if (!EditCDF(CDFname,useFormat,workingCache,stageCache,
		   compressCache,zMode,negToPosFp0)) return FALSE;
      break;
  }
  return TRUE;
}

/******************************************************************************
* CreateCDF.
******************************************************************************/

Logical CreateCDF (CDFdir, CDFname, zMode, useFormat, negToPosFp0,
		   workingCache, stageCache, compressCache, IWcdfs)
char *CDFdir;           /* Directory in which to create CDF. */
char *CDFname;          /* If null-string (""), then prompt for name.  Returns
			   name entered here. */
long zMode;             /* The zMode to select after CDF is created. */
Logical useFormat;      /* TRUE if FORMAT attribute should be used. */
Logical negToPosFp0;    /* TRUE if conversion of -0.0 to +0.0 should be
			   selected after CDF is created. */
long workingCache;      /* Number of cache buffers for the "working" dotCDF
			   file. */
long stageCache;	/* Number of cache buffers for the staging file. */
long compressCache;	/* Number of cache buffers for the compression scratch
			   file. */
struct ItemWindowStruct *IWcdfs;
			/* Pointer to "CDFs Menu" widget.  If NULL, then this
			   routine was not called from the "CDFs Menu". */
{
  CDFstatus status; CDFid id; static Logical first = TRUE;
  long numDims, dimSizes[CDF_MAX_DIMS];
  char CDFpathT[DU_MAX_PATH_LEN+1], delim;
  AOSs1 (trailer, "Enter: ________   Exit: ________   Help: ________")
  static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL };
  static char *headerDim[] = {
    "Enter rVariable dimensionality...",
    "Syntax: <numDims>:[<dimSize1>,<dimSize2>,...,<dimSizeN>]",
    "Examples: 0:[], 1:[5], 2:[100,100], 3:[10,20,30]"
  };
  static char valueDim[MAX_DIMENSIONALITY_LEN+1];
  static char labelDim[8+DU_MAX_NAME_LEN+1];
  static struct PromptWindowStruct PWdim = {
    labelDim, 3, 1, 78, 3, headerDim, MAX_DIMENSIONALITY_LEN, valueDim,
    1, trailer, exitChars, REFRESHkey_FSI, SOLkey_FSI, EOLkey_FSI,
    INSERTorOVERkey_FSI
  };
  if (first) {
    EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
			  HELPkey_FSI);
    first = FALSE;
  }
  if (NULstring(CDFname)) {
    static char *header[] = {
      "Enter CDF name (without delimiters)...",
      "Syntax: <char1><char2>...<charN>",
      "Example: D104B"
    };
    static char label[13+1+DU_MAX_DIR_LEN+1+1+1];
    static struct PromptWindowStruct PW = {
      label, 3, 1, 78, 3, header, DU_MAX_NAME_LEN, NULL, 1, trailer,
      exitChars, REFRESHkey_FSI, SOLkey_FSI, EOLkey_FSI,
      INSERTorOVERkey_FSI
    };
    char dirT[DU_MAX_DIR_LEN+1];
    strcpyX (dirT, (NULstring(CDFdir) ? CURRENTDIRECTORY : CDFdir),
	     DU_MAX_DIR_LEN);
    delim = PickDelimiter (dirT, strlen(dirT));
    snprintf (PW.label, (size_t) 13+1+DU_MAX_DIR_LEN+1+1+1,
	      " Creating in %c%s%c ", delim, dirT, delim);
    PW.value = CDFname;
    PromptWindow (NEWpw, &PW, 0, LogicalTRUE);
    for (;;) {
       if (EnterPW(&PW,CDFNAMEhelpID))
	 if (strlen(CDFname) > (size_t) 0) {
#if defined(vms) || defined(dos)
	   MakeUpperString (CDFname);
#endif
	   begin_pasteboard_update ();		/* Used to prevent the flicker
						   between this prompt and the
						   next one. */
	   PromptWindow (DELETEpw, &PW);
	   break;
	 }
	 else
	   ProblemWindow ("Illegal CDF name.", FALSE);
       else {
	 PromptWindow (DELETEpw, &PW);
	 return TRUE;
       }
    }
  }
  strcpyX (CDFpathT, CDFdir, DU_MAX_PATH_LEN);
  AppendToDir (CDFpathT, CDFname);
  strcpyX (PWdim.value, "0:[]", MAX_DIMENSIONALITY_LEN);
  delim = PickDelimiter (CDFname, strlen(CDFname));
  snprintf (labelDim, (size_t) sizeof(labelDim),
	    " CDF %c%s%c ", delim, CDFname, delim);
  PromptWindow (NEWpw, &PWdim, (int) strlen(PWdim.value), LogicalTRUE);
  end_pasteboard_update ();	/* It is OK to call this in the case where
				   the `begin_pasteboard_update' above was
				   not called (because the CDF name was not
				   prompted for. */
  for (;;) {
     if (EnterPW(&PWdim,RDIMhelpID)) {
       if (DecodeDimensionality(PWdim.value,&numDims,dimSizes)) {
	 PromptWindow (DELETEpw, &PWdim);
	 break;
       }
       else
	 ProblemWindow ("Illegal dimensionality.", FALSE);
     }
     else {
       PromptWindow (DELETEpw, &PWdim);
       return TRUE;
     }
  }
  status = CDFlib (CREATE_, CDF_, CDFpathT, numDims, dimSizes, &id,
		   NULL_);
  ReportStatus (status, FALSE);
  if (StatusBAD(status)) return FALSE;
  compressed = FALSE;
  if (IWcdfs != NULL) ItemWindow (UNDISPLAYiw, IWcdfs);
  return EditCDF(CDFname,useFormat,workingCache,stageCache,compressCache,
		 zMode,negToPosFp0);
}

/******************************************************************************
* EditCDF.
******************************************************************************/

Logical EditCDF (CDFname, useFormat, workingCache, stageCache, compressCache,
		 zMode, negToPosFp0)
char *CDFname;
Logical useFormat;
long workingCache;
long stageCache;
long compressCache;
long zMode;
Logical negToPosFp0;
{
  Logical status, closed = FALSE;
  /****************************************************************************
  * Edit the CDF.
  ****************************************************************************/
  status = EditCDFx(CDFname,useFormat,workingCache,stageCache,compressCache,
		    zMode,negToPosFp0,&closed);
  if (!closed) CDFlib (CLOSE_, CDF_,
		       NULL_);
  /****************************************************************************
  * Return the conversion status.
  ****************************************************************************/
  return status;
}

/******************************************************************************
* EditCDFx.
******************************************************************************/

Logical EditCDFx (CDFname, useFormat, workingCache, stageCache, compressCache,
		  zMode, negToPosFp0, closed)
char *CDFname;
Logical useFormat;
long workingCache;
long stageCache;
long compressCache;
long zMode;
Logical negToPosFp0;
Logical *closed;
{
   CDFstatus status;
   long readonlyMode = BOO(browseOnly,READONLYon,READONLYoff);
   long negToPosFp0Mode = BOO(negToPosFp0,NEGtoPOSfp0on,NEGtoPOSfp0off);
   AOSs3 (hLines, BLANKs78, BLANKs78, BLANKs78)
   static Logical first = TRUE;
   static char *iLinesBrowse[] = {
     "<Browse zVariables>",
     "<Browse rVariables>",
     "<Browse gAttributes>",
     "<Browse vAttributes>"
   };
   static char *iLinesEdit[] = {
     "<Edit zVariables>   <Change compression>   <Change checksum>",
     "<Edit rVariables>   <Change encoding>",
     "<Edit gAttributes>  <Change majority>",
     "<Edit vAttributes>  <Change format>"
   };
   AOSs1A (tLinesBrowse,
   "Select: ________   Exit: ________   Help: ________")
   AOSs1B (tLinesEdit,
   "Select: ________   Delete: ________   Exit: ________   Help: ________")
   static int iLineNsBrowse[] = { 0, 1, 2, 3 };
   static int iColsBrowse[] = { 0, 0, 0, 0 };
   static int iLensBrowse[] = { 19, 19, 20, 20 };
   static int iLineNsEdit[] = { 0, 0, 0, 1, 1, 2, 2, 3, 3 };
   static int iColsEdit[] = { 0, 20, 43, 0, 20, 0, 20, 0, 20 };
   static int iLensEdit[] = { 17, 20, 17, 17, 17, 18, 17, 18, 15 };
   static int exitCharsBrowse[] = {
     ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, zMODE0key_EDIT, zMODE1key_EDIT,
     zMODE2key_EDIT, NUL
   };
   static int exitCharsEdit[] = {
     ENTERkey_FSI, EXITkey_FSI, DELETECDFkey_EDIT, HELPkey_FSI,
     zMODE0key_EDIT, zMODE1key_EDIT, zMODE2key_EDIT, NUL };
   static char label[6+DU_MAX_NAME_LEN+2+1];
   static struct ItemWindowStruct IW = {
      0, 0, 80, label, 3, hLines, 0, NULL, 0, NULL, NULL, NULL, 4,
      1, NULL, NULL, REFRESHkey_FSI, FALSE, NUL, NUL
   };
   /***************************************************************************
   * First time...
   ***************************************************************************/
   if (first) {
     EncodeKeyDefinitions (1, tLinesBrowse, ENTERkey_FSI, EXITkey_FSI,
			     HELPkey_FSI);
     EncodeKeyDefinitions (1, tLinesEdit, ENTERkey_FSI, DELETECDFkey_EDIT,
			     EXITkey_FSI, HELPkey_FSI);
     first = FALSE;
   }
   if (browseOnly) {
     IW.NiLines = 4;
     IW.iLines = iLinesBrowse;
     IW.nItems = 4;
     IW.iLineNs = iLineNsBrowse;
     IW.iCols = iColsBrowse;
     IW.iLens = iLensBrowse;
     IW.tLines = tLinesBrowse;
     IW.exitChars = exitCharsBrowse;
   }
   else {
     IW.NiLines = 4;
     IW.iLines = iLinesEdit;
     IW.nItems = 9;
     IW.iLineNs = iLineNsEdit;
     IW.iCols = iColsEdit;
     IW.iLens = iLensEdit;
     IW.tLines = tLinesEdit;
     IW.exitChars = exitCharsEdit;
   }
   /***************************************************************************
   * Try to set number of cache buffers for dotCDF file, read-only mode, zMode,
   * and -0.0 mode.
   ***************************************************************************/
   status = CDFlib (SELECT_, CDF_CACHESIZE_, workingCache,
			     STAGE_CACHESIZE_, stageCache,
			     COMPRESS_CACHESIZE_, compressCache,
			     CDF_READONLY_MODE_, readonlyMode,
			     CDF_zMODE_, zMode,
			     CDF_NEGtoPOSfp0_MODE_, negToPosFp0Mode,
		    NULL_);
   if (!ReportStatus(status,FALSE)) return FALSE;
   /***************************************************************************
   * Build main menu.
   ***************************************************************************/
   if (!BuildCDFmenu(CDFname,&IW)) return FALSE;
   /***************************************************************************
   * Display menu/process keystrokes.
   ***************************************************************************/
   ItemWindow (NEWiw, &IW, 0);
   for (;;) {
     ItemWindow (READiw, &IW);
     switch (IW.key) {
       /***********************************************************************
       * Perform selected function.
       ***********************************************************************/
       case ENTERkey_FSI: {
	 int itemNt = IW.itemN + BOO(browseOnly,BROWSEinOFFSET,0);
	 switch (itemNt) {
	   /*******************************************************************
	   * Edit rVariables.
	   *******************************************************************/
	   case EDITrVarsIN:
	   case BROWSErVarsIN:
	   case EDITzVarsIN:
	   case BROWSEzVarsIN: {
	     Logical zOp = (itemNt == EDITzVarsIN || itemNt == BROWSEzVarsIN);
	     ItemWindow (UNDISPLAYiw, &IW);
	     if (!EditVars(zOp,CDFname,useFormat)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     if (!BuildCDFmenu(CDFname,&IW)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     ItemWindow (UPDATEiw, &IW, IW.itemN);
	     ItemWindow (REDISPLAYiw, &IW);
	     break;
	   }
	   /*******************************************************************
	   * Edit gAttributes.
	   *******************************************************************/
	   case EDITgAttrsIN:
	   case BROWSEgAttrsIN:
	     ItemWindow (UNDISPLAYiw, &IW);
	     if (gAttrsAndEntries) {
	       if (!EditAttrs(TRUE,CDFname)) {
		 ItemWindow (DELETEiw, &IW);
		 return FALSE;
	       }
	     }
	     else {
	       if (!EditAttrsX(TRUE,CDFname)) {
		 ItemWindow (DELETEiw, &IW);
		 return FALSE;
	       }
	     }
	     if (!BuildCDFmenu(CDFname,&IW)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     ItemWindow (UPDATEiw, &IW, IW.itemN);
	     ItemWindow (REDISPLAYiw, &IW);
	     break;
	   /*******************************************************************
	   * Edit vAttributes.
	   *******************************************************************/
	   case EDITvAttrsIN:
	   case BROWSEvAttrsIN:
	     ItemWindow (UNDISPLAYiw, &IW);
	     if (vAttrsAndEntries) {
	       if (!EditAttrs(FALSE,CDFname)) {
		 ItemWindow (DELETEiw, &IW);
		 return FALSE;
	       }
	     }
	     else {
	       if (!EditAttrsX(FALSE,CDFname)) {
		 ItemWindow (DELETEiw, &IW);
		 return FALSE;
	       }
	     }
	     if (!BuildCDFmenu(CDFname,&IW)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     ItemWindow (UPDATEiw, &IW, IW.itemN);
	     ItemWindow (REDISPLAYiw, &IW);
	     break;
	   /*******************************************************************
	   * Change format:
	   *******************************************************************/
	   case CHANGEformatIN: {
	     if (!SelectFormat()) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     if (!BuildCDFmenu(CDFname,&IW)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     ItemWindow (UPDATEiw, &IW, IW.itemN);
	     break;
	   }
	   /*******************************************************************
	   * Change encoding:
	   *******************************************************************/
	   case CHANGEencodingIN: {
	     if (!SelectEncoding()) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     if (!BuildCDFmenu(CDFname,&IW)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     ItemWindow (UPDATEiw, &IW, IW.itemN);
	     break;
	   }
	   /*******************************************************************
	   * Change majority:
	   *******************************************************************/
	   case CHANGEmajorityIN: {
	     if (!SelectMajority()) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     if (!BuildCDFmenu(CDFname,&IW)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     ItemWindow (UPDATEiw, &IW, IW.itemN);
	     break;
	   }
	   /*******************************************************************
	   * Select CDF compression:
	   *******************************************************************/
	   case CHANGEcompressionIN:
	     if (!SelectCompression(FORCDF,12,NULL)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     if (!BuildCDFmenu(CDFname,&IW)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     ItemWindow (UPDATEiw, &IW, IW.itemN);
	     break;
           /*******************************************************************
           * Change checksum:
           *******************************************************************/
           case CHANGEchecksumIN: {
             if (!SelectChecksum()) {
               ItemWindow (DELETEiw, &IW);
               return FALSE;
             }
             if (!BuildCDFmenu(CDFname,&IW)) {
               ItemWindow (DELETEiw, &IW);
               return FALSE;
             }
             ItemWindow (UPDATEiw, &IW, IW.itemN);
             break;
           }
	 }
	 break;
       }
       /***********************************************************************
       * Delete CDF.
       ***********************************************************************/
       case DELETECDFkey_EDIT:
		 if (!browseOnly) {
			char delim, question[DU_MAX_NAME_LEN+15+1];
			delim = PickDelimiter (CDFname, strlen(CDFname));
	   		snprintf (question, (size_t) sizeof(question),
				  "Delete CDF %c%s%c ?", delim, CDFname, delim);
	   		if (ConfirmWindow(12,80,question,NULL,FALSE,DELETECDFhelpID)) {
	     	  status = CDFlib (DELETE_, CDF_,
			  			       NULL_);
	     	  if (ReportStatus(status,FALSE)) {
				ItemWindow (DELETEiw, &IW);
	       		*closed = TRUE;
	       		return TRUE;
	     	  }
	     	  if (NoMoreAccess(NULL)) {
	       		ItemWindow (DELETEiw, &IW);
	       		return FALSE;
	     	  }
	   		}
	 	  }
	 	else
	   	  ProblemWindow ("Deleting a CDF not allowed while browsing.", FALSE);
	 	break;
       /***********************************************************************
       * zMode selection keys.
       ***********************************************************************/
       case zMODE0key_EDIT:
       case zMODE1key_EDIT:
       case zMODE2key_EDIT: {
	 	 long zMode;
	 	 switch (IW.key) {
	   		case zMODE0key_EDIT: zMode = zMODEoff; break;
	   		case zMODE1key_EDIT: zMode = zMODEon1; break;
	   		case zMODE2key_EDIT: zMode = zMODEon2; break;
	 	 }
	 	 status = CDFlib (SELECT_, CDF_zMODE_, zMode,
						  NULL_);
	 	 if (!ReportStatus(status,FALSE)) {
	   	   ItemWindow (DELETEiw, &IW);
	   	   return FALSE;
	 	 }
	 	 if (!BuildCDFmenu(CDFname,&IW)) {
	 	   ItemWindow (DELETEiw, &IW);
	 	   return FALSE;
	 	 }
	 	 ItemWindow (UPDATEiw, &IW, IW.itemN);
	 	 break;
       }
       /***********************************************************************
       * Display online help.
       ***********************************************************************/
       case HELPkey_FSI:
		 OnlineHelpWindow (ilhFile, CDFhelpID);
	 	 break;
       /***********************************************************************
       * Exit menu (closing CDF).
       ***********************************************************************/
       case EXITkey_FSI: {
	 	 if (dumpStatistics) {
	 	   vSTATS vStatsDotCDF, vStatsStage, vStatsCompress;
	   	   char temp1[MAX_SCREENLINE_LEN+1],
				temp2[MAX_SCREENLINE_LEN+1],
				temp3[MAX_SCREENLINE_LEN+1];
	   	   if (compressed) {
	     	 MessageWindow (closingLines, NULL, LogicalFALSE);
	    	 zzzzz (1.0);
	  	   }
	   	   status = CDFlib (CLOSE_, CDFwithSTATS_, &vStatsDotCDF,
							&vStatsStage,
							&vStatsCompress,
						    NULL_);
	   		if (compressed) MessageWindow (NULL);
	   		*closed = TRUE;
	   		if (!ReportStatus(status,FALSE)) {
	     	  ItemWindow (DELETEiw, &IW);
	     	  return FALSE;
	   		}
	   		ItemWindow (DELETEiw, &IW);
	   		if (vStatsDotCDF.maxBuffers > 0) {
	     	  BuildStatistics ("DotCDF file", &vStatsDotCDF,
			  			       temp1, temp2, temp3);
	     	  InfoWindow (temp1, temp2, temp3, TRUE, FALSE, 0);
	   		}
	   		if (vStatsStage.maxBuffers > 0) {
	     	  BuildStatistics ("staging file", &vStatsStage,
			  				    temp1, temp2, temp3);
	     	  InfoWindow (temp1, temp2, temp3, TRUE, FALSE, 0);
	   		}
	   		if (vStatsCompress.maxBuffers > 0) {
	    	  BuildStatistics ("compression scratch file", &vStatsCompress,
			 				     temp1, temp2, temp3);
	    	  InfoWindow (temp1, temp2, temp3, TRUE, FALSE, 0);
	   		}
	 	  }
	 	  else {
	   		if (compressed) {
	     		MessageWindow (closingLines, NULL, LogicalFALSE);
	     		zzzzz (1.0);
	   		}
	   		status = CDFlib (CLOSE_, CDF_,
							 NULL_);
	   		if (compressed) MessageWindow (NULL);
	   		*closed = TRUE;
	   		if (!ReportStatus(status,FALSE)) {
	   		  ItemWindow (DELETEiw, &IW);
	   		  return FALSE;
	   		}
	 		ItemWindow (DELETEiw, &IW);
	 	  }
	 	  return TRUE;
       }
     }
   }
}

/******************************************************************************
* BuildCDFmenu.
******************************************************************************/

Logical BuildCDFmenu (CDFname, IW)
char *CDFname;
struct ItemWindowStruct *IW;
{
  CDFstatus status;
  long format, encoding, majority, checksum;
  long nAttrs, NgAttrs, NvAttrs, NrVars, NzVars;
  long version, release, increment;
  long cType, cParms[CDF_MAX_PARMS], cPct;
  int pad;
  long TT2000updated = -1L;
  /****************************************************************************
  * Encode label with name of CDF.
  ****************************************************************************/
  snprintf (IW->label, (size_t) 6+DU_MAX_NAME_LEN+2+1,
	    " CDF \"%s\" ", CDFname);
  /****************************************************************************
  * Inquire CDF.
  ****************************************************************************/
  status = CDFlib (GET_, CDF_FORMAT_, &format,
			 CDF_ENCODING_, &encoding,
			 CDF_MAJORITY_, &majority,
			 CDF_NUMATTRS_, &nAttrs,
			 CDF_NUMgATTRS_, &NgAttrs,
			 CDF_NUMvATTRS_, &NvAttrs,
			 CDF_NUMrVARS_, &NrVars,
			 CDF_NUMzVARS_, &NzVars,
			 CDF_VERSION_, &version,
			 CDF_RELEASE_, &release,
			 CDF_INCREMENT_, &increment,
			 CDF_COMPRESSION_, &cType, cParms, &cPct,
			 CDF_CHECKSUM_, &checksum,
		   NULL_);
  if (!ReportStatus(status,FALSE)) return FALSE;
  if (!PriorTo ("3.5.0", version, release, increment)) {
    status = CDFlib (GET_, CDF_LEAPSECONDLASTUPDATED_, &TT2000updated,
		     NULL_);
    if (!ReportStatus(status,FALSE)) return FALSE;
  }
  /****************************************************************************
  * Encode first line of header.
  ****************************************************************************/
  snprintf (IW->hLines[0], (size_t) sizeof(BLANKs78),
	    "Format:   %s", FormatToken(format));
  pad = MAIN_MENU_1st_THIRD_LEN - strlen(IW->hLines[0]);
  if (pad > 0) CatNcharacters (IW->hLines[0], pad, (int) ' ');
  snprintf (EofS(IW->hLines[0]),
	    (size_t) sizeof(BLANKs78)-strlen(IW->hLines[0]),
	    "Attributes: %ld (%ldg/%ldv)", nAttrs, NgAttrs, NvAttrs);
  pad = MAIN_MENU_2nd_THIRD_LEN - strlen(IW->hLines[0]);
  if (pad > 0) CatNcharacters (IW->hLines[0], pad, (int) ' ');
  snprintf (EofS(IW->hLines[0]),
	    (size_t) sizeof(BLANKs78)-strlen(IW->hLines[0]),
	    "Compression:    %s", CompressionToken(cType,cParms));
#if 0
  if (cType != NO_COMPRESSION && cPct > 0) {
    snprintf (EofS(IW->hLines[0]),
	      (size_t) sizeof(BLANKs78)-strlen(IW->hLines[0]),
	      " (%ld%%)", cPct);
  }
#endif
  /****************************************************************************
  * Encode second line of header.
  ****************************************************************************/
  snprintf (IW->hLines[1], (size_t) sizeof(BLANKs78),
	    "Majority: %s", MajorityToken(majority));
  pad = MAIN_MENU_1st_THIRD_LEN - strlen(IW->hLines[1]);
  if (pad > 0) CatNcharacters (IW->hLines[1], pad, (int) ' ');
  snprintf (EofS(IW->hLines[1]),
	    (size_t) sizeof(BLANKs78)-strlen(IW->hLines[1]),
	    "Variables:  %ld (%ldr/%ldz)", NrVars + NzVars, NrVars, NzVars);
  pad = MAIN_MENU_2nd_THIRD_LEN - strlen(IW->hLines[1]);
  if (pad > 0) CatNcharacters (IW->hLines[1], pad, (int) ' ');
  snprintf (EofS(IW->hLines[1]),
	    (size_t) sizeof(BLANKs78)-strlen(IW->hLines[1]),
	    "Encoding:       %s", EncodingToken(encoding));
  /****************************************************************************
  * Encode third line of header.
  ****************************************************************************/
  snprintf (IW->hLines[2], (size_t) sizeof(BLANKs78),
	    "Version:  %ld.%ld.%ld", version, release, increment);
  pad = MAIN_MENU_1st_THIRD_LEN - strlen(IW->hLines[2]);
  if (pad > 0) CatNcharacters (IW->hLines[2], pad, (int) ' ');
  snprintf (EofS(IW->hLines[2]),
	    (size_t) sizeof(BLANKs78)-strlen(IW->hLines[2]),
	    "Checksum:   %s", ChecksumToken(checksum));
  if (!PriorTo ("3.6.0", version, release, increment)) {
    pad = MAIN_MENU_2nd_THIRD_LEN - strlen(IW->hLines[2]);
    if (pad > 0) CatNcharacters (IW->hLines[2], pad, (int) ' ');
    snprintf (EofS(IW->hLines[2]),
              (size_t) sizeof(BLANKs78)-strlen(IW->hLines[2]),
              "LastLeapSecond: %d", (int) TT2000updated);
  }
  return TRUE;
}

/******************************************************************************
* PromptForSpec.
*     Prompt for the CDF(s) specification.  Returns TRUE if a valid CDF(s)
* specification was entered; FALSE otherwise.
******************************************************************************/

Logical PromptForSpec (CDFspec)
char CDFspec[DU_MAX_PATH_LEN+1];
{
  static char *header[] = {
    "Enter CDF(s) specification (without delimiters)...",
    "Syntax: <char1><char2>...<charN>",
#if defined(vms)
    "Examples: RAIN2, RAIN*, [-.SAMPLES], USER5:[CDF1]IJ01A"
#endif
#if defined(unix) || defined(posixSHELL)
    "Examples: rain2, rain*, ../samples, ~/CDFs/ij01a"
#endif
#if defined(dos)
    "Examples: RAIN2, RAIN*, ..\\SAMPLES, B:\\CDF1\\IJ01A"
#endif
#if defined(win32)
    "Examples: rain2, rain*, ..\\samples, b:\\cdf1\\ij01a"
#endif
#if defined(mac)
    "Examples: cacsst1, cac*, Boot:CDFs:, :samples"
#endif
  };
#if BROWSEaware
  AOSs1 (trailer, "Enter: ________  Exit: ________  Help: ________  More help: ________")
  static int exitChars[] = {
    ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, MOREHELPkey_EDIT, NUL
  };
#else
  AOSs1 (trailer, "Enter: ________  Exit: ________  Help: ________")
  static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL };
#endif
  static char value[DU_MAX_PATH_LEN+1];
  static struct PromptWindowStruct PW = {
    " Specification Prompt ", 5, 5, 70, 3, header, DU_MAX_PATH_LEN, value,
    1, trailer, exitChars, REFRESHkey_FSI, SOLkey_FSI, EOLkey_FSI,
    INSERTorOVERkey_FSI
  };
#if BROWSEaware
  EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI,
			MOREHELPkey_EDIT);
#else
  EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI);
#endif
  strcpyX (PW.value, CDFspec, DU_MAX_PATH_LEN);
  PromptWindow (NEWpw, &PW, (int) strlen(PW.value), LogicalTRUE);
  for (;;) {
     PromptWindow (READiw, &PW);
     switch (PW.key) {
       case ENTERkey_FSI:
	 if (strlen(PW.value) > (size_t) 0) {
	   strcpyX (CDFspec, PW.value, DU_MAX_PATH_LEN);
	   PromptWindow (DELETEpw, &PW);
	   return TRUE;
         }
	 else
	   ProblemWindow ("Illegal CDF(s) specification.", FALSE);
	 break;
       case EXITkey_FSI:
	 PromptWindow (DELETEpw, &PW);
	 return FALSE;
       case HELPkey_FSI:
	 OnlineHelpWindow (ilhFile, SPEChelpID);
	 break;
#if BROWSEaware
       case MOREHELPkey_EDIT:
	 OnlineHelpWindow (ilhFile, MOREHELPhelpID);
	 break;
#endif
     }
  }
}

/******************************************************************************
* EditQOPs.
*    Returns TRUE if execution should continue.
******************************************************************************/

#if defined(mac)
Logical EditQOPs (argC, argV)
int *argC;
char **argV[];
{
  DialogPtr dialogP;
  DialogRecord dRecord;
  WindowPtr behind = (WindowPtr) -1;
  ControlHandle controlHs[MAXIMUMin+1];
  Rect iRect;
  short iType, i;
#ifndef __MWERKS__
  short itemN;
#else
  SInt16 itemN;
  ModalFilterUPP FilterDialogQOPfsiUPP;
  FileFilterUPP FilterForCDFsUPP;
  UserItemUPP OutlineDefaultButtonUPP;
#endif
  static Logical browseOnly = DEFAULTbrowseEDIT;
  static Logical useFormat = DEFAULTformatEDIT;
  static Logical negToPos = DEFAULT_NEGtoPOSfp0;
  static Logical promptFor = DEFAULTpromptEDIT;
  static Logical reportInfos = REPORTinfosDEFAULT;
  static Logical reportWarns = REPORTwarningsDEFAULT;
  static Logical reportErrors = REPORTerrorsDEFAULT;
  static Logical dispStats = DEFAULTstatsEDIT;
  static Logical gWith = DEFAULTgWithEDIT;
  static Logical vWith = DEFAULTgWithEDIT;
  static int zMode = DEFAULTzModeEDIT;
  static Str255 cacheText = "\p";
  static Str255 CDFtext = "\p";
  /****************************************************************************
  * Create the dialog and get the control handles.
  ****************************************************************************/
  dialogP = GetNewDialog (QOPri, &dRecord, behind);
  for (itemN = 1; itemN <= MAXIMUMin; itemN++) {
     GetDItem (dialogP, itemN, &iType, (Handle *) &controlHs[itemN], &iRect);
  }
  /****************************************************************************
  * Set the control values.
  ****************************************************************************/
  SetIText ((Handle) controlHs[CDFTEXTin], CDFtext);
  SetIText ((Handle) controlHs[CACHEin], cacheText);

  if (browseOnly) SetCtlValue (controlHs[BROWSEin], 1);
  if (useFormat) SetCtlValue (controlHs[FORMATin], 1);
  if (negToPos) SetCtlValue (controlHs[NEGZin], 1);
  if (promptFor) SetCtlValue (controlHs[PROMPTin], 1);
  if (reportInfos) SetCtlValue (controlHs[INFOin], 1);
  if (reportWarns) SetCtlValue (controlHs[WARNin], 1);
  if (reportErrors) SetCtlValue (controlHs[ERRORin], 1);
  if (dispStats) SetCtlValue (controlHs[STATSin], 1);
  if (gWith) SetCtlValue (controlHs[gWITHin], 1);
  if (vWith) SetCtlValue (controlHs[vWITHin], 1);

  SetCtlValue (controlHs[ZMODEinBASE+zMode], 1);

#ifndef __MWERKS__
  SetDItem (dialogP, (short) ODBin, (short) userItem,
	    (Handle) OutlineDefaultButton, &iRect);
#else
  OutlineDefaultButtonUPP = NewUserItemProc (OutlineDefaultButton);
  SetDItem (dialogP, (short) ODBin, (short) userItem,
	    (Handle) OutlineDefaultButtonUPP, &iRect);
#endif
  /****************************************************************************
  * Display the dialog and wait for user actions.
  ****************************************************************************/
  ShowWindow ((WindowPtr) dialogP);
  SetCursor (ARROW_CURSOR);
#ifdef __MWERKS__
  FilterDialogQOPfsiUPP = NewModalFilterProc(FilterDialogQOPfsi);
#endif

  for (;;) {
#ifndef __MWERKS__
    ModalDialog (FilterDialogQOPfsi, &itemN);
#else
    ModalDialog (FilterDialogQOPfsiUPP, &itemN);
#endif
    switch (itemN) {
      /************************************************************************
      * Ok.
      ************************************************************************/
    case OKin: {
		int n;
		char tempS1[1+1];
		/**********************************************************************
		* Get the value of each control.
		**********************************************************************/
		GetIText ((Handle) controlHs[CDFTEXTin], CDFtext);
		GetIText ((Handle) controlHs[CACHEin], cacheText);
		browseOnly = GetCtlValue (controlHs[BROWSEin]);
		useFormat = GetCtlValue (controlHs[FORMATin]);
		negToPos = GetCtlValue (controlHs[NEGZin]);
		promptFor = GetCtlValue (controlHs[PROMPTin]);
		reportInfos = GetCtlValue (controlHs[INFOin]);
		reportWarns = GetCtlValue (controlHs[WARNin]);
		reportErrors = GetCtlValue (controlHs[ERRORin]);
		dispStats = GetCtlValue (controlHs[STATSin]);
		gWith = GetCtlValue (controlHs[gWITHin]);
		vWith = GetCtlValue (controlHs[vWITHin]);
		for (zMode = 0; zMode < 3; zMode++) {
		   if (GetCtlValue(controlHs[ZMODEinBASE+zMode])) break;
		}
		/**********************************************************************
		* Build argc/argv.
		**********************************************************************/
		*argC = 12 + BOO(NULpString(CDFtext),0,1) +
			     BOO(NULpString(cacheText),0,2);
		*argV = (char **) cdf_AllocateMemory (*argC * sizeof(char *), FatalError);
		n = 0;
		MAKEstrARGv (argV, n, pgmName)
		MAKEbooARGv (argV, n, browseOnly, "-browse", "-nobrowse")
		MAKEbooARGv (argV, n, useFormat, "-format", "-noformat")
		MAKEbooARGv (argV, n, negToPos, "-neg2posfp0", "-noneg2posfp0")
		MAKEbooARGv (argV, n, promptFor, "-prompt", "-noprompt")
		MAKEbooARGv (argV, n, dispStats, "-statistics", "-nostatistics")
		MAKEbooARGv (argV, n, gWith, "-gwithentries", "-nogwithentries")
		MAKEbooARGv (argV, n, vWith, "-vwithentries", "-novwithentries")
		MAKEstrARGv (argV, n, "-zmode")
		snprintf (tempS1, (size_t) sizeof(tempS1), "%d", zMode);
		MAKEstrARGv (argV, n, tempS1)
		MAKEstrARGv (argV, n, "-report")
		MAKEstrARGv (argV, n, StatusCodeReportOptions(reportErrors,
						      reportWarns,
						      reportInfos))
		if (!NULpString(cacheText)) {
	  	  MAKEstrARGv (argV, n, "-cache")
	  	  PtoCstr (cacheText);
	  	  MAKEstrARGv (argV, n, (char *) cacheText)
	  	  CtoPstr ((char *) cacheText);
		}
		if (!NULpString(CDFtext)) {
		  PtoCstr (CDFtext);
		  MAKEstrARGv (argV, n, (char *) CDFtext)
		  CtoPstr ((char *) CDFtext);
		}
		/**********************************************************************
		* Close the dialog and return.
		**********************************************************************/
#ifdef __MWERKS__
		DisposeRoutineDescriptor(FilterDialogQOPfsiUPP);
		DisposeRoutineDescriptor(OutlineDefaultButtonUPP);
#endif
		CloseDialog (dialogP);
		return TRUE;
    }
    /************************************************************************
    * Help.
    ************************************************************************/
    case HELPin: {
		int n;
		*argC = 1;
		*argV = (char **) cdf_AllocateMemory (*argC * sizeof(char *), FatalError);
		n = 0;
		MAKEstrARGv (argV, n, pgmName)
#ifdef __MWERKS__
		DisposeRoutineDescriptor(FilterDialogQOPfsiUPP);
		DisposeRoutineDescriptor(OutlineDefaultButtonUPP);
#endif
		CloseDialog (dialogP);
		return TRUE;
    }
    /************************************************************************
    * Cancel.
    ************************************************************************/
    case CANCELin:
#ifdef __MWERKS__
    	DisposeRoutineDescriptor(FilterDialogQOPfsiUPP);
    	DisposeRoutineDescriptor(OutlineDefaultButtonUPP);
#endif
		CloseDialog (dialogP);
		return FALSE;
    /************************************************************************
    * Select CDF specification (existing CDF).
    ************************************************************************/
    case CDFSELECTin: {
		StandardFileReply CDFreply;
		char CDFpath[DU_MAX_PATH_LEN+1];
#ifndef __MWERKS__
		StandardGetFile (FilterForCDFs, -1, NULL, &CDFreply);
#else
		FilterForCDFsUPP = NewFileFilterProc(FilterForCDFs);
		StandardGetFile (FilterForCDFsUPP, -1, NULL, &CDFreply);
		DisposeRoutineDescriptor(FilterForCDFsUPP);
#endif
		if (CDFreply.sfGood && !CDFreply.sfIsFolder && !CDFreply.sfIsVolume) {
	  	  BuildMacPath (&CDFreply.sfFile, CDFpath, TRUE);
	  	  CDFtext[0] = strlen (CDFpath);
	  	  strcpyX ((char *) &CDFtext[1], CDFpath, 255);
	  	  SetIText ((Handle) controlHs[CDFTEXTin], CDFtext);
		}
		break;
    }
    /************************************************************************
    * Select CDF specification (new CDF).
    * The cursor is set because `StandardPutFile' leaves the cursor as
    * an iBeam (instead of returning it to what it was).
    ************************************************************************/
    case CDFNEWin: {
		StandardFileReply CDFreply;
		char CDFpath[DU_MAX_PATH_LEN+1];
		char prompt[] = "Enter CDF name:";
		StandardPutFile (CtoPstr(prompt), CtoPstr(""), &CDFreply);
		if (CDFreply.sfGood && !CDFreply.sfIsFolder && !CDFreply.sfIsVolume) {
	  	  BuildMacPath (&CDFreply.sfFile, CDFpath, TRUE);
	  	  CDFtext[0] = strlen (CDFpath);
	  	  strcpyX ((char *) &CDFtext[1], CDFpath, 255);
	  	  SetIText ((Handle) controlHs[CDFTEXTin], CDFtext);
		}
		SetCursor (&(qd.arrow));
		break;
    }
    /************************************************************************
    * Check boxes.
    ************************************************************************/
    case BROWSEin:
    case FORMATin:
    case INFOin:
    case WARNin:
    case ERRORin:
    case NEGZin:
    case PROMPTin:
    case STATSin:
    case gWITHin:
    case vWITHin:
		SetCtlValue (controlHs[itemN], BOO(GetCtlValue(controlHs[itemN]),0,1));
		break;
    /************************************************************************
    * Radio buttons.
    ************************************************************************/
    case ZMODEinBASE+0:
    case ZMODEinBASE+1:
    case ZMODEinBASE+2:
		for (i = 0; i < 3; i++) SetCtlValue (controlHs[ZMODEinBASE+i], 0);
		  SetCtlValue (controlHs[itemN], 1);
		break;
    	}
  }
}
#endif
