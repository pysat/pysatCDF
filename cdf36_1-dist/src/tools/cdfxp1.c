/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                                    CDFexport/1.
*
*  Version 1.2b, 21-Feb-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  15-Sep-95, J Love     Original version.
*   V1.0a 18-Sep-95, J Love	Macintosh event handling.
*   V1.1  26-Aug-96, J Love	CDF V2.6.
*   V1.2  15-Nov-96, J Love	Added `simple' mode.
*   V1.2a 15-Jan-97, J Love	Added attribute entries to beginning of text
*				file listings.
*   V1.2b 21-Feb-97, J Love	Removed RICE.
*   V2.0  20-Dec-10, M Liu      Added "#" at each text heading if i
*                               "poundedheading" is set in cdfxp.c.
*   V3.3  04-Apr-11, M Liu      Modified to handle TT2000 epoch style string.
*
******************************************************************************/

#include "cdfxp.h"

/******************************************************************************
* Error messages.
******************************************************************************/

static char listOpenError[] = "Error opening listing file.";
static char listWriteError[] = "Error writing to listing file.";
static char listCloseError[] = "Error closing listing file.";

static long *firstRecs, *lastRecs;
static void **epochValues;
static int **qaFlags;
static int numEpochGroups;
/******************************************************************************
* Function prototypes.
******************************************************************************/

char *StandardFormat PROTOARGs((long dataType));
int StandardWidth PROTOARGs((long dataType, long numElems));
Logical AdjustFirstLastRecords PROTOARGs((long *firstRec, long *lastRec));
Logical AdjustFirstsLastsRecords PROTOARGs((long *firstRecs, long *lastRecs,
                                            int *index));
void FreeEpochs PROTOARGs(());

/******************************************************************************
* ToScreenHori.
* Output to ASCII/screen, horizontal listing.
* Returns FALSE if a fatal CDF error occurred (meaning NO_MORE_ACCESS).
******************************************************************************/

Logical ToScreenHori () {
  AOSs1 (header, BLANKs78)
  static char screenLines[SCREENtextMAX+1];
  static char line[] = { BLANKs78 };
  AOSs1A (keyDefsPrompt, "More: ________   Exit: ________   Help: ________   Continuous: ________")
  AOSs1B (keyDefsEnd, "End of listing.  Hit ________ to continue.")
  AOSs1C (keyDefsLoading, "Loading lines.  Use ________ to abort.")
  AOSs1D (keyDefsCruise, "Continuous.  Use ________ to stop.")
  static char *keyDefsAborting[1] = { "Aborting at user's request." };
  static char *keyDefsInitial[1] = { "Loading..." };
  static int exitKeys[] = { NUL };
  static struct EditWindowStruct EWscr = {
    " Screen Listing ", 0, 0, SCREEN_WIDTH, 1, header, screenLines,
    NUMscreenLINES, 1, NULL, FALSE, TRUE, exitKeys, REFRESHkey_FSI, NUL, NUL,
    NUL, NUL, NUL, NUL, NUL, NUL, NUL
  };
  int lineN, totalWidth = 0, noRoom = 0, filterStatus, addL, key;
  int lineL = SCREEN_WIDTH - 2; long firstRec, lastRec, recN, nRecs;
  int ix;
  static Logical first = TRUE; Logical prompt = TRUE;
  double timeMark; CDFstatus status; struct ItemStruct *Item, *exportHead;
  /****************************************************************************
  * Encode key definitions first time.
  ****************************************************************************/
  if (first) {
    EncodeKeyDefinitions (1, keyDefsPrompt, ENTERkey_FSI, EXITkey_FSI,
			  HELPkey_FSI, CRUISEkey_EXPORT);
    EncodeKeyDefinitions (1, keyDefsEnd, ENTERkey_FSI);
    EncodeKeyDefinitions (1, keyDefsCruise, ABORTkey_EXPORT);
    EncodeKeyDefinitions (1, keyDefsLoading, ABORTkey_EXPORT);
    first = FALSE;
  }
  /****************************************************************************
  * Build export list.
  ****************************************************************************/
  BuildExportList (&exportHead, LogicalFALSE);
  /****************************************************************************
  * Validate `Record' and `Indices' selections.
  ****************************************************************************/
  ValidateRecordIndices (OUTPUTtoSCREENh, FALSE, 0L, exportHead);
  /****************************************************************************
  * Check for enough room for each exported item and calculate the maximum
  * record number.
  ****************************************************************************/
  for (Item = exportHead; Item != NULL; Item = Item->nextExport) {
     if (Item->output) {
       /***********************************************************************
       * Determine if enough room.
       ***********************************************************************/
       addL = BOO(totalWidth == 0,0,opt.spacing);
       switch (Item->type) {
	 case RECORDt:
	   addL += Item->width;
	   break;
	 case VARIABLEt:
	   addL += (int) ((Item->Var->nRecordValues * Item->width) +
			  (opt.spacing * (Item->Var->nRecordValues - 1)));
	   break;
       }
       if (totalWidth + addL <= lineL) {
	 totalWidth += addL;
	 continue;
       }
       /***********************************************************************
       * Not enough room.
       ***********************************************************************/
       noRoom++;
       Item->output = FALSE;
     }
  }
  /****************************************************************************
  * Display message if some items could not be listed.
  ****************************************************************************/
  if (noRoom > 0) {
    char msg[SCREEN_WIDTH+1];
    snprintf (msg, (size_t) sizeof(msg), "Not enough room for %d item%s.",
	      noRoom, BOO(noRoom <= 1,"","s"));
    DisplayMessage (msg, NOBEEPWAIT1);
  }
  /****************************************************************************
  * Check if a valid listing.
  ****************************************************************************/
  if (totalWidth == 0) {
    DisplayMessage ("Nothing selected for listing.", BEEPWAIT1);
    return TRUE;
  }
  /****************************************************************************
  * Remove items from the export list that are not being output or filtered.
  ****************************************************************************/
  RemoveExportItems (&exportHead);
  /****************************************************************************
  * Determine first/last records.
  ****************************************************************************/
  if (numEpochs <= 1 || (range == 0L || range == 2L)) {
    switch (FirstLastRecord(&firstRec,&lastRec,FALSE,exportHead)) {
      case PASSED: break;
      case FAILED: return TRUE;
      case FATAL: return FALSE;
    }
    if (range == 1L) {
      if (!AdjustFirstLastRecords(&firstRec, &lastRec)) return FALSE; 
      if (firstRec == -1) {
        DisplayMessage ("No records found for listing.", BEEPWAIT1);
        return TRUE;
      }
    } else if (range == 2L) {
      if (firstRec < recordStart) firstRec = recordStart;
      if (lastRec > recordEnd) lastRec = recordEnd;
    }
  } else {
    Logical found = TRUE;
    epochIndx = (int *) calloc(1, sizeof(int)*numEpochs);
    epochMaxs = (long *) calloc(1, sizeof(long)*numEpochs);
    numEpochGroups = FindUsedEpochs (&exportHead, epochIndx);
    firstRecs = (long *) malloc(sizeof(long)*numEpochs);
    lastRecs = (long *) malloc(sizeof(long)*numEpochs);
    epochValues = (void **) malloc(sizeof(void **)*numEpochs);
    LoadVirtualEpochs (epochValues);
    if (!AdjustFirstsLastsRecords(firstRecs, lastRecs, epochIndx)) return FALSE;
      for (ix = 0; ix < numEpochs; ++ix) {
        if (epochIndx[ix] == 1 && firstRecs[ix] != -1) {
          found = TRUE;
          break;
        }
      }
      if (!found) {
        DisplayMessage ("No records found for listing.", BEEPWAIT1);
        return TRUE;
      }
  }
  /****************************************************************************
  * Initialize header.
  ****************************************************************************/
  MakeNUL (header[0]);
  for (Item = exportHead; Item != NULL; Item = Item->nextExport) {
     if (Item->output) {
       if (!NULstring(header[0])) CatNcharacters (header[0],opt.spacing,' ');
       switch (Item->type) {
	 case RECORDt:
	   CatToString (header[0], "Record", Item->width, LEFT_JUSTIFY, "*");
	   break;
	 case VARIABLEt: {
	   int varWidth = (int) ((Item->Var->nRecordValues * Item->width) +
				 (opt.spacing * (Item->Var->nRecordValues-1)));
	   CatToString (header[0], Item->Var->name, varWidth, CENTER_JUSTIFY,
			"*");
	   break;
	 }
       }
     }
  }
  /****************************************************************************
  * Initialize listing lines.
  ****************************************************************************/
  MakeNUL (screenLines);
  lineN = 0;
  /****************************************************************************
  * Display listing window.
  ****************************************************************************/
  EWscr.tLines = keyDefsInitial;
  EditWindow (NEWew, &EWscr, LogicalTRUE);
  nRecs = lastRec - firstRec + 1;
  timeMark = SystemClock ();
  /****************************************************************************
  * Read values until end of listing or user requests an early exit.
  ****************************************************************************/
  if (numEpochs <= 1 || numEpochGroups <= 1) {
    for (recN = firstRec; recN <= lastRec; recN++) {
     /*************************************************************************
     * Check if the abort key has been entered.
     *************************************************************************/
     if (read_input(
#if defined(CURSESui)
		    EWscr.wid,
#endif
			      &key,PASSTHRUri,FALSE)) {
       if (key == ABORTkey_EXPORT) {
	 if (prompt) {
	   EWscr.tLines = keyDefsAborting;
	   EditWindow (UPDATEew, &EWscr, LogicalTRUE);
	   zzzzz (1.0);
	   EditWindow (DELETEew, &EWscr, TRUE);
	   return TRUE;
	 }
	 else
	   prompt = TRUE;
       }
       else
	 EditWindow (BEEPew, &EWscr);
     }
     /*************************************************************************
     * Should `loading' message be displayed?
     *************************************************************************/
     if (SystemClock() - timeMark > 1.0) {
       UpdateToScreen (&EWscr, keyDefsLoading[0], recN, nRecs);
       timeMark = SystemClock ();
     }
     /*************************************************************************
     * Encode line.
     *************************************************************************/
     status = EncodeLineHori(line,recN,&filterStatus,exportHead,FALSE,
			     (size_t) sizeof(BLANKs78));
     if (StatusBAD(status)) {
       char text[CDF_STATUSTEXT_LEN+1];
       CDFlib (SELECT_, CDF_STATUS_, status,
	       GET_, STATUS_TEXT_, text,
	       NULL_);
       InfoWindow ("An error occurred while reading the CDF.", text, NULL,
		   TRUE, TRUE, 0);
       EditWindow (DELETEew, &EWscr, TRUE);
       return FALSE;
     }
     /*************************************************************************
     * Check if line should be added to display.
     *************************************************************************/
     if (SHOWline(filterStatus)) {
       if (lineN == NUMscreenLINES) {
	 Logical loop = TRUE;
	 UpdateToScreen (&EWscr, BOO(prompt,keyDefsPrompt[0],keyDefsCruise[0]),
			 recN, nRecs);
	 if (prompt)
	   EditWindow (READew, &EWscr);
	 else
	   EWscr.key = ENTERkey_FSI;
	 while (loop) {
	   switch (EWscr.key) {
	     case CRUISEkey_EXPORT:
	       prompt = FALSE;          /* No `break' is intentional. */
	     case ENTERkey_FSI:
	       MakeNUL (screenLines);
	       lineN = 0;
	       loop = FALSE;
	       break;
	     case HELPkey_FSI:
	       OnlineHelpWindow ("cdfxp.ilh", SCREENhelpID);
	       EditWindow (READew, &EWscr);
	       break;
	     case EXITkey_FSI:
	       EditWindow (DELETEew, &EWscr, TRUE);
	       return TRUE;
	     default:
	       EditWindow (BEEPew, &EWscr);
	       EditWindow (READew, &EWscr);
	       break;
	   }
	 }
	 timeMark = SystemClock ();
       }
       strcatX (screenLines, line, SCREENtextMAX);
       strcatX (screenLines, "\n", SCREENtextMAX);
       lineN++;
     }
    }
  } else {
    for (ix = 0; ix< numEpochs; ++ix) {
    }
  }
  /****************************************************************************
  * End of listing.
  ****************************************************************************/
  EWscr.tLines = keyDefsEnd;
  EditWindow (UPDATEew, &EWscr, LogicalTRUE);
  EditWindow (READew, &EWscr);
  EditWindow (DELETEew, &EWscr, TRUE);
  FreeEpochs ();
  return TRUE;
}

/******************************************************************************
* ToFileHori.
* Output to ASCII/file, horizontal listing.
* Returns FALSE if a fatal CDF error occurred (meaning NO_MORE_ACCESS).
******************************************************************************/

Logical ToFileHori () {
  long addL, lineL = 0; int filterStatus;
  char *line; long firstRec, lastRec, recN, nRecs;
  long *firstRecs, *lastRecs;
  struct ItemStruct *Item, *exportHead; FILE *oFp;
  CDFstatus status; Logical first = TRUE, cdfFatal;
  static char pctMsg[] = "Listing started...";
  static char keyDefsBlank[] = "\n\n";
  static char keyDefsAbort[] = "Abort: ________\n\n";
  int iptf;
  /****************************************************************************
  * Encode key definitions first time.
  ****************************************************************************/
  if (first) {
    char *p1 = keyDefsAbort;
    EncodeKeyDefinitions (1, &p1, ABORTkey_EXPORT);
    first = FALSE;
  }
  /****************************************************************************
  * Build export list.
  ****************************************************************************/
  BuildExportList (&exportHead, LogicalFALSE);
  /****************************************************************************
  * Validate `Record' and `Indices' selections.
  ****************************************************************************/
  ValidateRecordIndices (OUTPUTtoFILEh, FALSE, 0L, exportHead);
  /****************************************************************************
  * Calculate the width of each line and the maximum record number.
  ****************************************************************************/
  for (Item = exportHead; Item != NULL; Item = Item->nextExport) {
     if (Item->output) {
       /***********************************************************************
       * Add to total width.
       ***********************************************************************/
       addL = BOO(lineL == 0,0,opt.spacing);
       switch (Item->type) {
	 case RECORDt:
	   addL += Item->width;
	   break;
	 case VARIABLEt: {
	   int width = BOO(simpleMode,
			   StandardWidth(Item->Var->dataType,
					 Item->Var->numElems),Item->width);
	   addL += (Item->Var->nRecordValues * width) +
		   (opt.spacing * (Item->Var->nRecordValues - 1));
	   break;
	 }
       }
       lineL += addL;
     }
  }
  /****************************************************************************
  * Check if a valid listing.
  ****************************************************************************/
  if (lineL == 0) {
    DisplayMessage ("Nothing selected for listing.", BEEPWAIT1);
    return TRUE;
  }
  /****************************************************************************
  * Remove items from the export list that are not being output or filtered.
  ****************************************************************************/
  RemoveExportItems (&exportHead);
  /****************************************************************************
  * Determine first/last records.
  ****************************************************************************/
  switch (FirstLastRecord(&firstRec,&lastRec,FALSE,exportHead)) {
    case PASSED: break;
    case FAILED: return TRUE;
    case FATAL: return FALSE;
  }
  if (range == 1L) {
    if (!AdjustFirstLastRecords(&firstRec, &lastRec)) return FALSE;
    if (firstRec == -1) {
      DisplayMessage ("No records found for listing.", BEEPWAIT1);
      return TRUE;
    }
  } else if (range == 2L) {
    if (firstRec < recordStart) firstRec = recordStart;
    if (lastRec > recordEnd) lastRec = recordEnd;
  }
  /****************************************************************************
  * Allocate buffer for line.
  ****************************************************************************/
#if defined(dos)
  if (TOObigIBMpc(lineL+1))
    line = NULL;
  else
#endif
    line = (char *) cdf_AllocateMemory ((size_t) (lineL+1), NULL);
  if (line == NULL) {
    DisplayMessage ("Not enough memory.", BEEPWAIT);
    return TRUE;
  }
  /****************************************************************************
  * Prompt for listing file path (unless in batch mode) and open file.
  ****************************************************************************/
  if (!BATCH(batchMode)) {
    if (!PromptFor(outputText,DU_MAX_PATH_LEN,strlen(outputText),
		   "Enter path for listing file...",oFILEhelpID)) {
      cdf_FreeMemory (line, FatalError);
      return TRUE;
    }
  }
  oFp = fopen (outputText, "w");
  if (oFp == NULL) {
    DisplayMessage (listOpenError, BEEPWAIT);
    cdf_FreeMemory (line, FatalError);
    return TRUE;
  }
  /****************************************************************************
  * Output header line(s).
  ****************************************************************************/
  if (opt.textHeading) {
    if (!ListAttributes(oFp,&cdfFatal)) {
      if (fclose(oFp) == EOF) DisplayMessage (listCloseError, BEEPWAIT);
      cdf_FreeMemory (line, FatalError);
      return (!cdfFatal);
    }
    MakeNUL (line);
    for (Item = exportHead; Item != NULL; Item = Item->nextExport) {
       if (Item->output) {
         if (!NULstring(line)) CatNcharacters (line, opt.spacing, ' ');
         switch (Item->type) {
	   case RECORDt:
	     CatToString (line, "Record", Item->width, LEFT_JUSTIFY, "*");
	     break;
	   case VARIABLEt: {
	     int standardWidth = StandardWidth(Item->Var->dataType,
					       Item->Var->numElems);
	     int valueWidth = BOO(simpleMode,standardWidth,Item->width);
	     int varWidth = (int) ((Item->Var->nRecordValues*valueWidth) +
				   (opt.spacing*(Item->Var->nRecordValues-1)));
	     CatToString (line, Item->Var->name, varWidth,
			  CENTER_JUSTIFY, "*");
	     break;
	   }
         }
       }
    }
    iptf = fprintf(oFp,BOO(poundedheading==1,"\n# Variables:\n\n%s\n",
                           "\nVariables:\n\n%s\n"),line);
    if (iptf < 0) {
      DisplayMessage (listWriteError, BEEPWAIT);
      if (fclose(oFp) == EOF) DisplayMessage (listCloseError, BEEPWAIT);
        cdf_FreeMemory (line, FatalError);
      return TRUE;
    }
  }
  /****************************************************************************
  * Read/output values until end of listing.
  ****************************************************************************/
  nRecs = lastRec - firstRec + 1;
  DisplayPctComplete (NO_PCT, pctMsg);
  NEWkeyDEFS (EWkey, keyDefsAbort, batchMode)
  for (recN = firstRec; recN <= lastRec; recN++) {
     if (AbortListing(oFp,line)) {
       NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
       return TRUE;
     }
     status = EncodeLineHori(line,recN,&filterStatus,exportHead,simpleMode,
			     (size_t) (lineL+1));
     DisplayStatus (status, readingCDF);
     if (StatusBAD(status)) {
       if (fprintf(oFp,"Incomplete listing.\n") < 0) {
	 DisplayMessage (listWriteError, BEEPWAIT);
       }
       if (fclose(oFp) == EOF) DisplayMessage (listCloseError, BEEPWAIT);
       cdf_FreeMemory (line, FatalError);
       return FALSE;
     }
     if (SHOWline(filterStatus)) {
       if (fprintf(oFp,"%s\n",line) < 0) {
	 DisplayMessage (listWriteError, BEEPWAIT);
	 if (fclose(oFp) == EOF) DisplayMessage (listCloseError, BEEPWAIT);
	 cdf_FreeMemory (line, FatalError);
	 return TRUE;
       }
     }
     DisplayPctComplete (PCT(recN,nRecs,1,1), pctMsg);
  }
  /****************************************************************************
  * End of listing.
  ****************************************************************************/
  NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
  if (fclose(oFp) == EOF) {
    DisplayMessage (listCloseError, BEEPWAIT);
    cdf_FreeMemory (line, FatalError);
    return TRUE;
  }
  cdf_FreeMemory (line, FatalError);
  if (!BATCH(batchMode)) DisplayMessage ("Complete.", NOBEEPWAIT1);
  return TRUE;
}

/******************************************************************************
* ToFileCDAweb.
* Output to ASCII/file, horizontal listing, similar to what CDAWEB produces.
* Returns FALSE if a fatal CDF error occurred (meaning NO_MORE_ACCESS).
******************************************************************************/

Logical ToFileCDAweb () {
  long addL, lineL = 0; int filterStatus;
  char *line, *line2; long firstRec, lastRec, recN, nRecs;
  long *firstRecs, *lastRecs;
  struct ItemStruct *Item, *exportHead; FILE *oFp;
  CDFstatus status; Logical first = TRUE, cdfFatal;
  static char pctMsg[] = "Listing started...";
  static char keyDefsBlank[] = "\n\n";
  static char keyDefsAbort[] = "Abort: ________\n\n";
  int ia, ib, ix, iy;
  int iptf;
  /****************************************************************************
  * Encode key definitions first time.
  ****************************************************************************/
  if (first) {
    char *p1 = keyDefsAbort;
    EncodeKeyDefinitions (1, &p1, ABORTkey_EXPORT);
    first = FALSE;
  }
  /****************************************************************************
  * Build export list.
  ****************************************************************************/
  BuildExportList (&exportHead, LogicalFALSE);
  /****************************************************************************
  * Validate `Record' and `Indices' selections.
  ****************************************************************************/
  ValidateRecordIndices (OUTPUTtoFILEcdaweb, FALSE, 0L, exportHead); 
  /****************************************************************************
  * Remove items from the export list that are not being output or filtered.
  ****************************************************************************/
  RemoveExportItems (&exportHead);
  /****************************************************************************
  * Determine first/last records.
  ****************************************************************************/
  switch (FirstLastRecord(&firstRec,&lastRec,FALSE,exportHead)) {
    case PASSED: break;
    case FAILED: return TRUE;
    case FATAL: return FALSE;
  }
  if (numEpochs <= 1) {
    if (range == 1L) {
      if (!AdjustFirstLastRecords(&firstRec, &lastRec)) return FALSE;
      if (firstRec == -1) {
        DisplayMessage ("No records found for listing.", BEEPWAIT1);
        return TRUE;
      }
    } else if (range == 2L) {
      if (firstRec < recordStart) firstRec = recordStart;
      if (lastRec > recordEnd) lastRec = recordEnd;
    }
    /***************************************************************************
    * Reorder the output items by moving the epoch to the first.
    ***************************************************************************/
    ReorderExportItems (&exportHead);
  } else {
    Logical found = TRUE;
    char ep1String[EPOCH_STRING_LEN+1], ep2String[EPOCH_STRING_LEN+1];
    epochIndx = (int *) calloc(1, sizeof(int)*numEpochs);
    epochMaxs = (long *) calloc(1, sizeof(long)*numEpochs);
    numEpochGroups = FindUsedEpochs (&exportHead, epochIndx);
    firstRecs = (long *) malloc(sizeof(long)*numEpochs);
    for (ia = 0; ia < numEpochs; ++ia)
      firstRecs[ia] = -1L;
    lastRecs = (long *) malloc(sizeof(long)*numEpochs);
    epochValues = (void **) malloc(sizeof(void **)*numEpochs);
    LoadVirtualEpochs (epochValues);
    /************************************************************************** 
    * Reorder the output items by moving the epoch to the first.
    **************************************************************************/
    if (range == 0L) {
      firstRecs[ia] = firstRec;
      lastRecs[ia] = lastRec;
    } else if (range == 1L) {
     if (!AdjustFirstsLastsRecords(firstRecs, lastRecs, epochIndx)) 
	return FALSE;
    } else {
      for (ia = 0; ia < numEpochs; ++ia) {
        firstRecs[ia] = recordStart;
        lastRecs[ia] = recordEnd;
      }
    }
    for (ia = 0; ia < numEpochs; ++ia) {
      if (epochIndx[ia] == 1 && firstRecs[ia] != -1) {
        found = TRUE;
      }
    }
    if (!found) {
      DisplayMessage ("No records found for listing.", BEEPWAIT1);
      return TRUE;
    }
  }
  for (Item = exportHead; Item != NULL; Item = Item->nextExport) { 
     if (Item->output) {
       /***********************************************************************
       * Add to total width.
       ***********************************************************************/
       addL = BOO(lineL == 0,0,opt.spacing);
       if (Item->type == VARIABLEt) {
           int width = BOO(simpleMode,
                           StandardWidth(Item->Var->dataType,
                                         Item->Var->numElems),Item->width);
           width = MAXIMUM (width, strlen(Item->Var->name));
           if (Item->Var->dataType == CDF_EPOCH ||
               Item->Var->dataType == CDF_EPOCH16 ||
	       Item->Var->dataType == CDF_TIME_TT2000)
             --width;
           addL += (Item->Var->nRecordValues * width) +
                   (opt.spacing * (Item->Var->nRecordValues - 1));
       }
       lineL += addL;
     }
  }
  /****************************************************************************
  * Check if a valid listing.
  ****************************************************************************/
  if (lineL == 0) {
    DisplayMessage ("Nothing selected for listing.", BEEPWAIT1);
    return TRUE;
  }
  oFp = fopen (outputText, "w");
  if (oFp == NULL) {
    DisplayMessage (listOpenError, BEEPWAIT);
    return TRUE;
  }
  /****************************************************************************
  * Output header line(s).
  ****************************************************************************/
  if (opt.textHeading) {
    if (!ListAttributes(oFp,&cdfFatal)) {
      if (fclose(oFp) == EOF) DisplayMessage (listCloseError, BEEPWAIT);
      return (!cdfFatal);
    }
  }
  ib = 0;
  for (ix = 0; ix < numEpochGroups; ++ix) {
    for (; ib < numEpochs; ++ib)
      if (epochIndx[ib] == 1) break;
    /**************************************************************************
    * Calculate the width of each line and the maximum record number.
    **************************************************************************/
    for (iy = 0; iy < numVarsInGroup[ix]; ++iy) {
      Item = (struct ItemStruct *) varPtrsInGroup[ix][iy];
      /***********************************************************************
      * Add to total width.
      ***********************************************************************/
      addL = BOO(lineL == 0,0,opt.spacing);
      if (Item->type == VARIABLEt) {
        int width = BOO(simpleMode,
                        StandardWidth(Item->Var->dataType,
                                      Item->Var->numElems),Item->width);
        width = MAXIMUM (width, strlen(Item->Var->name));
        if (Item->Var->dataType == CDF_EPOCH ||
            Item->Var->dataType == CDF_EPOCH16 ||
	    Item->Var->dataType == CDF_TIME_TT2000)
          --width;
        addL += (Item->Var->nRecordValues * width) +
                (opt.spacing * (Item->Var->nRecordValues - 1));
      }
      lineL += addL;
    }
    /**************************************************************************
    * Allocate buffer for line.
    **************************************************************************/
#if defined(dos)
    if (TOObigIBMpc(lineL+1)) {
      line = NULL;
    } else 
#endif
      line = (char *) cdf_AllocateMemory ((size_t) (lineL+1), NULL);
    if (line == NULL) {
      DisplayMessage ("Not enough memory.", BEEPWAIT);
      return TRUE;
    }
#if defined(dos)
    if (TOObigIBMpc(lineL+1)) {
      line2 = NULL:
    } else 
#endif
      line2 = (char *) cdf_AllocateMemory ((size_t) (lineL+1), NULL);
    if (line2 == NULL) {
      DisplayMessage ("Not enough memory.", BEEPWAIT);
      return TRUE;
    }
    MakeNUL (line);
    MakeNUL (line2);
    for (iy = 0; iy < numVarsInGroup[ix]; ++iy) {
      Item = varPtrsInGroup[ix][iy];
      if (Item->output) {
        if (!NULstring(line)) CatNcharacters (line, opt.spacing, ' ');
        if (!NULstring(line2)) CatNcharacters (line2, opt.spacing, ' ');
        if (Item->type == VARIABLEt) {
	  int standardWidth;
	  int valueWidth;
	  int varWidth;
	  standardWidth = StandardWidth(Item->Var->dataType,
					Item->Var->numElems);
	  valueWidth = BOO(simpleMode,standardWidth,Item->width);
          valueWidth = MAXIMUM(valueWidth, strlen(Item->Var->name));
	  if (Item->Var->dataType == CDF_EPOCH || 
	      Item->Var->dataType == CDF_EPOCH16 ||
	      Item->Var->dataType == CDF_TIME_TT2000)
	    --valueWidth; 
	  varWidth = (int) ((Item->Var->nRecordValues*valueWidth) +
			    (opt.spacing*(Item->Var->nRecordValues-1)));
          if (Item->Var->dataType == CDF_EPOCH) {
            for (ia = 0; ia < (int) Item->Var->nRecordValues; ++ia) {
              CatToString (line, Item->Var->name, valueWidth,
                           CENTER_JUSTIFY, "*");
              CatToString (line2, "dd-mm-yyyy hh:mm:ss.ms ", valueWidth,
                           CENTER_JUSTIFY, "*");
              if (ia < (Item->Var->nRecordValues-1))
		CatToString (line2, " ", 1, RIGHT_JUSTIFY, "*");
            }
          } else if (Item->Var->dataType == CDF_EPOCH16) {
            for (ia = 0; ia < (int) Item->Var->nRecordValues; ++ia) {
              CatToString (line, Item->Var->name, valueWidth,
                           CENTER_JUSTIFY, "*");
              CatToString (line2, "dd-mm-yyyy hh:mm:ss.mmm.uuu.nnn.ppp",
                           valueWidth, RIGHT_JUSTIFY, "*");
              if (ia < (Item->Var->nRecordValues-1))
		CatToString (line2, " ", 1, RIGHT_JUSTIFY, "*");
	    }
	  } else if (Item->Var->dataType == CDF_TIME_TT2000) {
            for (ia = 0; ia < (int) Item->Var->nRecordValues; ++ia) {
              CatToString (line, Item->Var->name, valueWidth,
                           CENTER_JUSTIFY, "*");
              CatToString (line2, "yyyy-mm-ddThh:mm:ss.mmmuuunnn ", valueWidth,
                           CENTER_JUSTIFY, "*");
              if (ia < (Item->Var->nRecordValues-1))
		CatToString (line2, " ", 1, RIGHT_JUSTIFY, "*");
            }
	  } else {                      
            char *tmp;
            long counts[CDF_MAX_DIMS];
            long indices[CDF_MAX_DIMS];
            if ((int) Item->Var->numDims < 1) {
              CatToString (line, Item->Var->name, varWidth,
                           RIGHT_JUSTIFY, "*");
              CatNcharacters (line2, valueWidth, ' ');
            } else {
              for (ia = 0; ia < (int) Item->Var->numDims; ++ia) { 
                indices[ia] = 0L;
                counts[ia] = Item->Var->dimSizes[ia];
              }
	      tmp = (char *) cdf_AllocateMemory ((size_t)valueWidth+1, 
						 NULL);
              for (ia = 0; ia < (int) Item->Var->nRecordValues; ++ia) {
                CatToString (line, Item->Var->name, valueWidth,
                             RIGHT_JUSTIFY, "*");
                if (ia < (Item->Var->nRecordValues-1))
                  CatToString (line, " ", 1, RIGHT_JUSTIFY, "*");
                EncodeIndicesJustify (tmp, Item->Var->numDims, indices,
                                      valueWidth, (size_t) valueWidth+1);
                CatToString (line2, tmp, valueWidth, RIGHT_JUSTIFY, "*");
		if (ia < (Item->Var->nRecordValues-1)) 
		  CatToString (line2, " ", 1, RIGHT_JUSTIFY, "*");
                if (ia < ((int) Item->Var->nRecordValues - 1)) {
                  if (ROWmajor(inMajority))
                    INCRindicesROW (Item->Var->numDims, counts, indices);
                  else
                    INCRindicesCOL (Item->Var->numDims, counts, indices);
                }
              }
	      cdf_FreeMemory (tmp, FatalError);
            } 
          }
        }              
      }
    }
    iptf = fprintf(oFp,BOO(poundedheading==1,"\n# Variables:\n\n%s\n",
                           "\nVariables:\n\n%s\n"),line);
    if (iptf < 0) {
      DisplayMessage (listWriteError, BEEPWAIT);
      if (fclose(oFp) == EOF) DisplayMessage (listCloseError, BEEPWAIT);
      cdf_FreeMemory (line, FatalError);
      cdf_FreeMemory (line2, FatalError);
      return TRUE;
    }
    if (fprintf(oFp,"%s\n",line2) < 0) {
      DisplayMessage (listWriteError, BEEPWAIT);
      if (fclose(oFp) == EOF) DisplayMessage (listCloseError, BEEPWAIT);
      cdf_FreeMemory (line, FatalError);
      cdf_FreeMemory (line2, FatalError);
      return TRUE;
    }
    if (fprintf(oFp,"%s\n",line2) < 0) {
      DisplayMessage (listWriteError, BEEPWAIT);
      if (fclose(oFp) == EOF) DisplayMessage (listCloseError, BEEPWAIT);
      cdf_FreeMemory (line, FatalError);
      cdf_FreeMemory (line2, FatalError);
      return TRUE;
    }
    /**************************************************************************
    * Read/output values until end of listing.
    **************************************************************************/
    if (numEpochGroups > 1) {
      lastRec = lastRecs[ib];
      firstRec = firstRecs[ib];
      qaFlags = (int **) malloc(sizeof(int **) * numVarsInGroup[ix]);
      for (iy = 0; iy < numVarsInGroup[ix]; ++iy) {
        Item = varPtrsInGroup[ix][iy];
        if (EndsWithIgCase(Item->Var->name, "epoch")) {
          qaFlags[iy] = NULL;
          continue;
        }
        qaFlags[iy] = malloc (4*(Item->Var->maxRec+1));
        ReadVirtualVariableApplyQaflag (Item->Var->zVar, Item->Var->name, 
					qaFlags[iy]);
        break;
      }
    }
    nRecs = lastRec - firstRec + 1;
    DisplayPctComplete (NO_PCT, pctMsg);
    NEWkeyDEFS (EWkey, keyDefsAbort, batchMode)
    for (recN = firstRec; recN <= lastRec; recN++) {
      if (AbortListing(oFp,line)) {
         NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
         return TRUE;
      }
      if (numEpochGroups == 1)
        status = EncodeLineHori(line,recN,&filterStatus,exportHead,simpleMode,
				(size_t) lineL+1);
      else
        status = EncodeLineHoriN(line,recN,&filterStatus,ix,ib,qaFlags,
				 simpleMode, (size_t) lineL+1);
      DisplayStatus (status, readingCDF);
      if (StatusBAD(status)) {
        if (fprintf(oFp,"Incomplete listing.\n") < 0) {
 	  DisplayMessage (listWriteError, BEEPWAIT);
        }
        if (fclose(oFp) == EOF) DisplayMessage (listCloseError, BEEPWAIT);
        cdf_FreeMemory (line, FatalError);
        cdf_FreeMemory (line2, FatalError);
        return FALSE;
      }
      if (SHOWline(filterStatus)) {
        if (fprintf(oFp,"%s\n",line) < 0) {
	  DisplayMessage (listWriteError, BEEPWAIT);
	  if (fclose(oFp) == EOF) DisplayMessage (listCloseError, BEEPWAIT);
	  cdf_FreeMemory (line, FatalError);
	  cdf_FreeMemory (line2, FatalError);
	  return TRUE;
        }
      }
      DisplayPctComplete (PCT(recN,nRecs,1,1), pctMsg);
    }
    ++ib;
    for (iy = 0; iy < numVarsInGroup[ix]; ++iy) {
      if (qaFlags[iy] != NULL) free(qaFlags[iy]);
    }
    free(qaFlags);
  }
  /****************************************************************************
  * End of listing.
  ****************************************************************************/
  NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
  if (fclose(oFp) == EOF) {
    DisplayMessage (listCloseError, BEEPWAIT);
    cdf_FreeMemory (line, FatalError);
    cdf_FreeMemory (line2, FatalError);
    return TRUE;
  }
  cdf_FreeMemory (line, FatalError);
  cdf_FreeMemory (line2, FatalError);
  if (!BATCH(batchMode)) DisplayMessage ("Complete.", NOBEEPWAIT1);
  return TRUE;
}

/******************************************************************************
* EncodeLineHori.
******************************************************************************/

CDFstatus EncodeLineHori (line, recN, filterStatus, exportHead, standard,
			  widthW)
char *line;
long recN;
int *filterStatus;
struct ItemStruct *exportHead;
Logical standard;
size_t widthW;
{
  struct ItemStruct *Item; Logical failedFilter; CDFstatus pStatus = CDF_OK;
  Logical outRowMajor = ROWmajor(MAJORITYtoOUT(opt.majority,inMajority));
  /****************************************************************************
  * Initialize line and filter status.
  ****************************************************************************/
  MakeNUL (line);
  *filterStatus = PASSes;
  /****************************************************************************
  * Encode each exported item.
  ****************************************************************************/
  for (Item = exportHead; Item != NULL; Item = Item->nextExport) {
     switch (Item->type) {
       /***********************************************************************
       * Indices.
       ***********************************************************************/
       case INDICESt:
         break;
       /***********************************************************************
       * Record number.
       ***********************************************************************/
       case RECORDt: {
	 if (cdaweb) break;
	 /*********************************************************************
	 * Check to see if record number passes filter.  If the record number
	 * failed the filter...
	 *********************************************************************/
	 failedFilter = RECORDfailedFILTER(Item,recN);
	 if (failedFilter) {
	   if (opt.showFiltered)
	     *filterStatus = SHOWit;
	   else {
	     *filterStatus = FAILrecord;
	     return pStatus;
	   }
	 }
	 /*********************************************************************
	 * Output record number.
	 *********************************************************************/
	 if (Item->output) {
	   if (!NULstring(line)) CatNcharacters (line, opt.spacing, ' ');
	   if (failedFilter)
	     CatNcharacters (line, Item->width, '-');
	   else
	     EncodeRecordJustify (EofS(line), recN, -(Item->width),
				  widthW-strlen(line));
	 }
	 break;
       }
       /***********************************************************************
       * Variable.
       ***********************************************************************/
       case VARIABLEt: {
	 CDFstatus status; int dimN; int style;
         if (TT2000dataType(Item->Var->dataType)) style = TT2000_3_STYLE;
         else style = opt.epochStyle;
	 /*********************************************************************
	 * Select variable, record number, and indices and read value.
	 *********************************************************************/
	 status = CDFlib (SELECT_, BOO(Item->Var->zVar,
				       zVAR_,rVAR_), Item->Var->varN,
				   BOO(Item->Var->zVar,
				       zVAR_RECNUMBER_,
				       rVARs_RECNUMBER_), recN,
			  NULL_);
	 if (!sX(status,&pStatus)) return pStatus;
	 /*********************************************************************
	 * Process each value in the variable record (array).
	 *********************************************************************/
	 for (dimN = 0; dimN < Item->Var->numDims; dimN++) {
	    Item->Var->indices[dimN] = 0;
	 }
	 for (Item->Var->valueN = 0;
	      Item->Var->valueN < Item->Var->nRecordValues;
	      Item->Var->valueN++) {
	    /******************************************************************
	    * Read the next value.
	    ******************************************************************/
	    status = CDFlib (SELECT_, BOO(Item->Var->zVar,
					  zVAR_DIMINDICES_,
					  rVARs_DIMINDICES_),
							Item->Var->indices,
			     GET_, VAR_DATA(Item->Var->zVar), Item->Var->value,
			     NULL_);
	    if (!sX(status,&pStatus)) return pStatus;
	    /******************************************************************
	    * Filter value.  If a scalar value was filtered out...
	    ******************************************************************/
	    failedFilter = VARfailedFILTER(Item,Item->Var->value);
	    if (failedFilter && Item->Var->scalar) {
	      if (opt.showFiltered)
		*filterStatus = SHOWit;
	      else {
		*filterStatus = FAILrecord;
		return pStatus;
	      }
	    }
	    /******************************************************************
	    * Encode value.
	    ******************************************************************/
	    if (Item->output) {
	      if (!NULstring(line)) CatNcharacters (line, opt.spacing, ' ');
	      if (failedFilter)
		CatNcharacters (line, Item->width, '-');
	      else {
		int standardWidth = StandardWidth(Item->Var->dataType,
					          Item->Var->numElems);
		int width = BOO(standard,standardWidth,Item->width);
		char *format = BOO(standard,
				   StandardFormat(Item->Var->dataType),
				   Item->Var->format);
		if (cdaweb)
		  width = MAXIMUM(width, strlen(Item->Var->name));
		if (!cdaweb || 
		    (Item->Var->dataType != CDF_EPOCH && 
		     Item->Var->dataType != CDF_EPOCH16 &&
                     Item->Var->dataType != CDF_TIME_TT2000)) {
                  if ((Item->Var->dataType == CDF_FLOAT) ||
                      (Item->Var->dataType == CDF_REAL4)) {
                    if (!isnan((double)*(float *)Item->Var->value) &&
                        *(float *)Item->Var->value <= DEFAULT_FLOAT_PADVALUE) {
                      EncodeValue (Item->Var->dataType, Item->Var->value,
                                   EofS(line), style,
                                   (size_t) widthW-strlen(line));
                    } else
                      EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems
,
                                          Item->Var->value, EofS(line),
                                          format, width, width, style,
                                          widthW-strlen(line));

                  } else if ((Item->Var->dataType == CDF_DOUBLE) ||
                             (Item->Var->dataType == CDF_REAL8)) {
                    if (!isnan(*(double *)Item->Var->value) &&
                        *(double *)Item->Var->value <= DEFAULT_DOUBLE_PADVALUE) {
                      EncodeValue (Item->Var->dataType, Item->Var->value,
                                   EofS(line), style,
                                   (size_t) widthW-strlen(line));
                    } else
                      EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems
,
                                          Item->Var->value, EofS(line),
                                          format, width, width, style,
                                          widthW-strlen(line));
                  } else    
		    EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems,
				        Item->Var->value, EofS(line),
				        format, width, width, style,
				        widthW-strlen(line));
		} else {
		  if (Item->Var->dataType == CDF_EPOCH) {
		    char epString[EPOCH_STRING_LEN+1];
		    encodeEPOCHx (*(double *)Item->Var->value,
		                  "<dom.02>-<mm.02>-<year.04> <hour>:<min>:<sec>.<fos>",
		                  epString);
		    CatToString (EofS(line), epString, EPOCH_STRING_LEN-1, 
				 LEFT_JUSTIFY, "*");
		  } else if (Item->Var->dataType == CDF_EPOCH16) {
		    char ep16String1[EPOCH16_STRING_LEN+1], ep16String2[12+1];
		    double time1, time2[2];
		    long mmm, uuu, nnn, ppp;
		    time2[0] = *((double *) Item->Var->value);
		    time2[1] = *(((double *) Item->Var->value)+1);
		    time1 = time2[0] * 1000.0 + time2[1] / pow(10.0, 9.0);
		    encodeEPOCHx (time1, 
		                  "<dom.02>-<mm.02>-<year.04> <hour>:<min>:<sec>.<fos>",
		                  ep16String1);
		    mmm = (long) (time2[1] / pow(10.0, 9.0));
		    time2[1] = time2[1] - mmm * pow(10.0, 9.0);
		    uuu = (long) (time2[1] / pow(10.0, 6.0));
		    time2[1] = time2[1] - uuu * pow(10.0, 6.0);
		    nnn = (long) (time2[1] / pow(10.0, 3.0));
		    ppp = (long) (time2[1] - nnn * pow(10.0, 3.0));
		    snprintf(ep16String2, (size_t) sizeof(ep16String2), 
			     ".%3.3ld.%3.3ld.%3.3ld", uuu, nnn, ppp);
		    strcatX (ep16String1, ep16String2, 0);
		    CatToString (EofS(line), ep16String1, 
				 EPOCH16_STRING_LEN-1, LEFT_JUSTIFY, "*");
		  } else if (Item->Var->dataType == CDF_TIME_TT2000) {
		    char epString[TT2000_3_STRING_LEN+1];
		    encodeTT2000 (*(long long *)Item->Var->value,
		                  epString, 3);
		    CatToString (EofS(line), epString, TT2000_3_STRING_LEN-1, 
				 LEFT_JUSTIFY, "*");
		  } else {
                    char epString[TT2000_3_STRING_LEN+1];
                    CDF_TT2000_to_UTC_string (*(long *)Item->Var->value,
                                              epString, 3);
                    CatToString (EofS(line), epString, TT2000_3_STRING_LEN,
                                 LEFT_JUSTIFY, "*");
                  }
		}
	      }
	    }
	    /******************************************************************
	    * Increment indices.
	    ******************************************************************/
	    if (Item->Var->valueN < (Item->Var->nRecordValues - 1)) {
	      if (outRowMajor)
	        INCRindicesROW (Item->Var->numDims, Item->Var->dimSizes,
		  	        Item->Var->indices);
	      else
	        INCRindicesCOL (Item->Var->numDims, Item->Var->dimSizes,
			        Item->Var->indices);
	    }
	 }
	 break;
       }
     }
  }
  return pStatus;
}

/******************************************************************************
* EncodeLineHoriN.
******************************************************************************/

CDFstatus EncodeLineHoriN (line, recN, filterStatus, idx, ide, qaFlags,
			   standard, widthW)
char *line;
long recN;
int *filterStatus;
int idx;
int ide;
int **qaFlags;
Logical standard;
size_t widthW;
{
  struct ItemStruct *Item; Logical failedFilter; CDFstatus pStatus = CDF_OK;
int i;
  Logical outRowMajor = ROWmajor(MAJORITYtoOUT(opt.majority,inMajority));
  /****************************************************************************
  * Initialize line and filter status.
  ****************************************************************************/
  MakeNUL (line);
  *filterStatus = PASSes;
  /****************************************************************************
  * Encode each exported item.
  ****************************************************************************/
  for (i = 0; i < numVarsInGroup[idx]; ++i) {
   Item = (struct ItemStruct *) varPtrsInGroup[idx][i];
     switch (Item->type) {
       /***********************************************************************
       * Indices.
       ***********************************************************************/
       case INDICESt:
         break;
       /***********************************************************************
       * Record number.
       ***********************************************************************/
       case RECORDt: {
	 if (cdaweb) break;
	 /*********************************************************************
	 * Check to see if record number passes filter.  If the record number
	 * failed the filter...
	 *********************************************************************/
	 failedFilter = RECORDfailedFILTER(Item,recN);
	 if (failedFilter) {
	   if (opt.showFiltered)
	     *filterStatus = SHOWit;
	   else {
	     *filterStatus = FAILrecord;
	     return pStatus;
	   }
	 }
	 /*********************************************************************
	 * Output record number.
	 *********************************************************************/
	 if (Item->output) {
	   if (!NULstring(line)) CatNcharacters (line, opt.spacing, ' ');
	   if (failedFilter)
	     CatNcharacters (line, Item->width, '-');
	   else
	     EncodeRecordJustify (EofS(line), recN, -(Item->width),
				  widthW-strlen(line));
	 }
	 break;
       }
       /***********************************************************************
       * Variable.
       ***********************************************************************/
       case VARIABLEt: {
	 CDFstatus status; int dimN; int style;
         if (TT2000dataType(Item->Var->dataType)) style = TT2000_3_STYLE;
         else style = opt.epochStyle;
	 /*********************************************************************
	 * Select variable, record number, and indices and read value.
	 *********************************************************************/
         if (Item->Var->dataType != CDF_EPOCH &&
             Item->Var->dataType != CDF_EPOCH16 &&
             Item->Var->dataType != CDF_TIME_TT2000) {
           if (qaFlags[idx] != NULL && qaFlags[idx][recN] > 0) {
                int standardWidth = StandardWidth(Item->Var->dataType,
                                                  Item->Var->numElems);
                int width = BOO(standard,standardWidth,Item->width);
                char *format = BOO(standard,
                                   StandardFormat(Item->Var->dataType),
                                   Item->Var->format);
                width = MAXIMUM(width, strlen(Item->Var->name));
                if ((Item->Var->dataType == CDF_FLOAT) ||
                    (Item->Var->dataType == CDF_REAL4)) {
                  if (!isnan((double)*(float *)Item->Var->fill) &&
                      *(float *)Item->Var->fill <= DEFAULT_FLOAT_PADVALUE) {
                    EncodeValue (Item->Var->dataType, Item->Var->fill,
                                 EofS(line), style,
                                 (size_t) widthW-strlen(line));
                  } else
                    EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems,
                                        Item->Var->fill, EofS(line),
                                        format, width, width, style,
                                        widthW-strlen(line));
                } else if ((Item->Var->dataType == CDF_DOUBLE) ||
                           (Item->Var->dataType == CDF_REAL8)) {
                  if (!isnan(*(double *)Item->Var->fill) &&
                      *(double *)Item->Var->fill <= DEFAULT_DOUBLE_PADVALUE) {
                    EncodeValue (Item->Var->dataType, Item->Var->fill,
                                 EofS(line), style,
                                 (size_t) widthW-strlen(line));
                  } else
                    EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems,
                                        Item->Var->fill, EofS(line),
                                        format, width, width, style,
                                        widthW-strlen(line));
                } else
                  EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems,
                                      Item->Var->fill, EofS(line),
                                      format, width, width, style,
			  	      widthW-strlen(line));
             continue;
           }
	   status = CDFlib (SELECT_, BOO(Item->Var->zVar,
	 			         zVAR_,rVAR_), Item->Var->varN,
				     BOO(Item->Var->zVar,
				         zVAR_RECNUMBER_,
				         rVARs_RECNUMBER_), recN,
			    NULL_);
	   if (!sX(status,&pStatus)) return pStatus;
	   /*******************************************************************
	   * Process each value in the variable record (array).
	   *******************************************************************/
	   for (dimN = 0; dimN < Item->Var->numDims; dimN++)
	     Item->Var->indices[dimN] = 0;
	   for (Item->Var->valueN = 0;
	     Item->Var->valueN < Item->Var->nRecordValues;
	     Item->Var->valueN++) {
	     /*****************************************************************
	     * Read the next value.
	     *****************************************************************/
	     status = CDFlib (SELECT_, BOO(Item->Var->zVar,
					   zVAR_DIMINDICES_,
					   rVARs_DIMINDICES_),
							Item->Var->indices,
			      GET_, VAR_DATA(Item->Var->zVar), Item->Var->value,
			      NULL_);
	     if (!sX(status,&pStatus)) return pStatus;
	     /****************************************************************
	     * Filter value.  If a scalar value was filtered out...
	     ****************************************************************/
	     failedFilter = VARfailedFILTER(Item,Item->Var->value);
	     if (failedFilter && Item->Var->scalar) {
	       if (opt.showFiltered)
		 *filterStatus = SHOWit;
	       else {
		 *filterStatus = FAILrecord;
		 return pStatus;
	       }
	     }
	     /*****************************************************************
	     * Encode value.
	     *****************************************************************/
	     if (Item->output) {
	       if (!NULstring(line)) CatNcharacters (line, opt.spacing, ' ');
	       if (failedFilter)
		 CatNcharacters (line, Item->width, '-');
	       else {
		 int standardWidth = StandardWidth(Item->Var->dataType,
			 	 	           Item->Var->numElems);
		 int width = BOO(standard,standardWidth,Item->width);
		 char *format = BOO(standard,
				    StandardFormat(Item->Var->dataType),
				    Item->Var->format);
		 if (cdaweb)
		   width = MAXIMUM(width, strlen(Item->Var->name));
                 if ((Item->Var->dataType == CDF_FLOAT) ||
                     (Item->Var->dataType == CDF_REAL4)) {
                   if (!isnan((double)*(float *)Item->Var->value) &&
                       *(float *)Item->Var->value <= DEFAULT_FLOAT_PADVALUE) {
                     EncodeValue (Item->Var->dataType, Item->Var->value,
                                  EofS(line), style,
                                  (size_t) widthW-strlen(line));
                 } else
                   EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems,
                                       Item->Var->value, EofS(line),
                                       format, width, width, style,
                                       widthW-strlen(line));

                 } else if ((Item->Var->dataType == CDF_DOUBLE) ||
                            (Item->Var->dataType == CDF_REAL8)) {
                   if (!isnan(*(double *)Item->Var->value) &&
                       *(double *)Item->Var->value <= DEFAULT_DOUBLE_PADVALUE) {
                     EncodeValue (Item->Var->dataType, Item->Var->value,
                                  EofS(line), style,
                                  (size_t) widthW-strlen(line));
                 } else
                   EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems,
                                       Item->Var->value, EofS(line),
                                       format, width, width, style,
                                       widthW-strlen(line));

                 } else
		   EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems,
				       Item->Var->value, EofS(line),
				       format, width, width, style,
				       widthW-strlen(line));
               }
	     }
             /****************************************************************
             * Increment indices.
             ****************************************************************/
             if (Item->Var->valueN < (Item->Var->nRecordValues - 1)) {
               if (outRowMajor)
                 INCRindicesROW (Item->Var->numDims, Item->Var->dimSizes,
                                 Item->Var->indices);
               else
                 INCRindicesCOL (Item->Var->numDims, Item->Var->dimSizes,
                                 Item->Var->indices);
             }
	   }
	 } else {
	   if (Item->Var->dataType == CDF_EPOCH) {
	     char epString[EPOCH_STRING_LEN];
	     encodeEPOCHx (((double **) epochValues)[ide][recN],
		          "<dom.02>-<mm.02>-<year.04> <hour>:<min>:<sec>.<fos>",
		           epString);
	     CatToString (EofS(line), epString, EPOCH_STRING_LEN-1, 
			  LEFT_JUSTIFY, "*");
	   } else if (Item->Var->dataType == CDF_EPOCH16) {
	     char ep16String1[EPOCH16_STRING_LEN], ep16String2[12+1];
	     double time1, time2[2];
	     long mmm, uuu, nnn, ppp;
	     time2[0] = ((double **) epochValues)[ide][2*recN];
	     time2[1] = ((double **) epochValues)[ide][2*recN+1];
	     time1 = time2[0] * 1000.0 + time2[1] / pow(10.0, 9.0);
	     encodeEPOCHx (time1, 
	                  "<dom.02>-<mm.02>-<year.04> <hour>:<min>:<sec>.<fos>",
	                   ep16String1);
	     mmm = (long) (time2[1] / pow(10.0, 9.0));
	     time2[1] = time2[1] - mmm * pow(10.0, 9.0);
	     uuu = (long) (time2[1] / pow(10.0, 6.0));
	     time2[1] = time2[1] - uuu * pow(10.0, 6.0);
	     nnn = (long) (time2[1] / pow(10.0, 3.0));
	     ppp = (long) (time2[1] - nnn * pow(10.0, 3.0));
	     snprintf(ep16String2, (size_t) sizeof(ep16String2),
		      ".%3.3ld.%3.3ld.%3.3ld", uuu, nnn, ppp);
	     strcatX (ep16String1, ep16String2, 0);
	     CatToString (EofS(line), ep16String1, 
			  EPOCH16_STRING_LEN-1, LEFT_JUSTIFY, "*");
	  } else {
             char epString[TT2000_3_STRING_LEN+1];
             CDF_TT2000_to_UTC_string (((long long **) epochValues)[ide][recN],
                                       epString, 3);
             CatToString (EofS(line), epString, TT2000_3_STRING_LEN,
                          LEFT_JUSTIFY, "*");
          }
	}
      }
      break;
    }
  }
  FreeEpochs ();
  return pStatus;
}

/******************************************************************************
* ToScreenVert.
* Output to ASCII/screen, vertical listing.
* Returns FALSE if a fatal CDF error occurred (meaning NO_MORE_ACCESS).
******************************************************************************/

Logical ToScreenVert () {
  AOSs1 (header, BLANKs78)
  static char screenLines[SCREENtextMAX+1];
  static char line[] = { BLANKs78 };
  AOSs1A (keyDefsPrompt, "More: ________   Exit: ________   Help: ________   Continuous: ________")
  AOSs1B (keyDefsEnd, "End of listing.  Hit ________ to continue.")
  AOSs1C (keyDefsLoading, "Loading lines.  Use ________ to abort.")
  AOSs1D (keyDefsCruise, "Continuous.  Use ________ to stop.")
  static char *keyDefsAborting[1] = { "Aborting at user's request." };
  static char *keyDefsInitial[1] = { "Loading..." };
  static int exitKeys[] = { NUL };
  static struct EditWindowStruct EWscr = {
    " Screen Listing ", 0, 0, SCREEN_WIDTH, 1, header, screenLines,
    NUMscreenLINES, 1, NULL, FALSE, TRUE, exitKeys, REFRESHkey_FSI, NUL, NUL,
    NUL, NUL, NUL, NUL, NUL, NUL, NUL
  };
  int lineN, totalWidth = 0, filterStatus, noRoom = 0, addL, key;
  int lineL = SCREEN_WIDTH - 2; double timeMark;
  struct ItemStruct *Item, *exportHead; CDFstatus status;
  long firstRec, lastRec, recN, nValues = 0, valueN, numDims, atLine;
  long dimSizes[CDF_MAX_DIMS], indices[CDF_MAX_DIMS], nLinesTotal;
  long firstIndices[CDF_MAX_DIMS], lastIndices[CDF_MAX_DIMS];
  Logical prompt = TRUE, same, sameGt0; static Logical first = TRUE;
  Logical outRowMajor = ROWmajor(MAJORITYtoOUT(opt.majority,inMajority));
  /****************************************************************************
  * Encode key definitions first time.
  ****************************************************************************/
  if (first) {
    EncodeKeyDefinitions (1, keyDefsPrompt, ENTERkey_FSI, EXITkey_FSI,
			  HELPkey_FSI, CRUISEkey_EXPORT);
    EncodeKeyDefinitions (1, keyDefsEnd, ENTERkey_FSI);
    EncodeKeyDefinitions (1, keyDefsCruise, ABORTkey_EXPORT);
    EncodeKeyDefinitions (1, keyDefsLoading, ABORTkey_EXPORT);
    first = FALSE;
  }
  /****************************************************************************
  * Build export list.
  ****************************************************************************/
  BuildExportList (&exportHead, LogicalFALSE);
  /****************************************************************************
  * Determine if the dimensionalities are all the same.
  ****************************************************************************/
  same = SameDimensionalities (&numDims, dimSizes, exportHead);
  sameGt0 = (same && numDims > 0);
  /****************************************************************************
  * Validate `Record' and `Indices' selections.
  ****************************************************************************/
  ValidateRecordIndices (OUTPUTtoSCREENv, same, numDims, exportHead);
  /****************************************************************************
  * Check for enough room for each exported item and calculate the maximum
  * record number and maximum number of values per record (array).
  ****************************************************************************/
  for (Item = exportHead; Item != NULL; Item = Item->nextExport) {
     if (Item->output) {
       /***********************************************************************
       * Determine if enough room.
       ***********************************************************************/
       addL = BOO(totalWidth == 0,0,opt.spacing) + Item->width;
       if (totalWidth + addL <= lineL) {
	 totalWidth += addL;
	 if (Item->type == VARIABLEt) {
	   nValues = MAXIMUM(nValues,Item->Var->nRecordValues);
	 }
	 continue;
       }
       /***********************************************************************
       * Not enough room.
       ***********************************************************************/
       noRoom++;
       Item->output = FALSE;
     }
  }
  /****************************************************************************
  * Display message if some items could not be listed.
  ****************************************************************************/
  if (noRoom > 0) {
    char msg[SCREEN_WIDTH+1];
    snprintf (msg, (size_t) sizeof(msg), "Not enough room for %d item%s.",
	      noRoom, BOO(noRoom <= 1,"","s"));
    DisplayMessage (msg, NOBEEPWAIT1);
  }
  /****************************************************************************
  * Check if a valid listing.
  ****************************************************************************/
  if (totalWidth == 0) {
    DisplayMessage ("Nothing selected for listing.", BEEPWAIT1);
    return TRUE;
  }
  /****************************************************************************
  * Remove items from the export list that are not being output or filtered.
  ****************************************************************************/
  RemoveExportItems (&exportHead);
  /****************************************************************************
  * Determine first/last record/indices.
  ****************************************************************************/
  switch (FirstLastRecord(&firstRec,&lastRec,FALSE,exportHead)) {
    case PASSED: break;
    case FAILED: return TRUE;
    case FATAL: return FALSE;
  }
  if (range == 1L) {
    if (!AdjustFirstLastRecords(&firstRec, &lastRec)) return FALSE;
    if (firstRec == -1) {
      DisplayMessage ("No records found for listing.", BEEPWAIT1);
      return TRUE;
    }
  } else if (range == 2L) {
    if (firstRec < recordStart) firstRec = recordStart;
    if (lastRec > recordEnd) lastRec = recordEnd;

  }
  if (sameGt0) {
    switch (FirstLastIndices(numDims,dimSizes,firstIndices,
			     lastIndices,&nValues,FALSE,exportHead)) {
      case PASSED: break;
      case FAILED: return TRUE;
      case FATAL: return FALSE;
    }
  }
  /****************************************************************************
  * Initialize header.
  ****************************************************************************/
  MakeNUL (header[0]);
  for (Item = exportHead; Item != NULL; Item = Item->nextExport) {
     if (Item->output) {
       if (!NULstring(header[0])) CatNcharacters (header[0],opt.spacing,' ');
       switch (Item->type) {
	 case RECORDt:
	   CatToString (header[0], "Record", Item->width, LEFT_JUSTIFY, "*");
	   break;
	 case INDICESt:
	   CatToString (header[0], "Indices", Item->width, LEFT_JUSTIFY, "*");
	   break;
	 case VARIABLEt:
	   CatToString (header[0], Item->Var->name, Item->width,
			CENTER_JUSTIFY, "*");
	   break;
       }
     }
  }
  /****************************************************************************
  * Initialize listing lines and display listing window.
  ****************************************************************************/
  MakeNUL (screenLines);
  lineN = 0;
  EWscr.tLines = keyDefsInitial;
  EditWindow (NEWew, &EWscr, LogicalTRUE);
  nLinesTotal = (lastRec - firstRec + 1) * nValues;
  timeMark = SystemClock ();
  /****************************************************************************
  * Read/list values until end of listing or user requests an early exit.
  ****************************************************************************/
  for (recN = firstRec, atLine = 0; recN <= lastRec; recN++) {
     /*************************************************************************
     * If same dimensionalities, initialize indices.
     *************************************************************************/
     if (sameGt0) {
       ARRAYtoARRAY (indices, firstIndices, numDims)
     }
     /*************************************************************************
     * For each value...
     *************************************************************************/
     for (valueN = 0; valueN < nValues; valueN++) {
	/**********************************************************************
	* Check if the abort key has been entered.
	**********************************************************************/
	if (read_input(
#if defined(CURSESui)
		       EWscr.wid,
#endif
				 &key,PASSTHRUri,FALSE)) {
	  if (key == ABORTkey_EXPORT) {
	    if (prompt) {
	      EWscr.tLines = keyDefsAborting;
	      EditWindow (UPDATEew, &EWscr, LogicalTRUE);
	      zzzzz (1.0);
	      EditWindow (DELETEew, &EWscr, TRUE);
	      return TRUE;
	    }
	    else
	      prompt = TRUE;
	  }
	  else
	    EditWindow (BEEPew, &EWscr);
	}
	/**********************************************************************
	* Should `loading' message be displayed?
	**********************************************************************/
	if (SystemClock() - timeMark > 1.0) {
	  UpdateToScreen (&EWscr, keyDefsLoading[0], atLine, nLinesTotal);
	  timeMark = SystemClock ();
	}
	/**********************************************************************
	* Encode line.
	**********************************************************************/
	status = EncodeLineVert(line,recN,valueN,numDims,indices,
				same,&filterStatus,exportHead,
				outRowMajor,FALSE,(size_t)sizeof(BLANKs78));
	if (StatusBAD(status)) {
	  char text[CDF_STATUSTEXT_LEN+1];
	  CDFlib (SELECT_, CDF_STATUS_, status,
		  GET_, STATUS_TEXT_, text,
		  NULL_);
	  InfoWindow ("An error occurred while reading the CDF.", text, NULL,
		      TRUE, TRUE, 0);
	  EditWindow (DELETEew, &EWscr, TRUE);
	  return FALSE;
	}
	/**********************************************************************
	* Check filter status to see if the line should be added to the screen.
	**********************************************************************/
	if (SHOWline(filterStatus)) {
	  if (lineN == NUMscreenLINES) {
	    Logical loop = TRUE;
	    UpdateToScreen (&EWscr, BOO(prompt,keyDefsPrompt[0],
					       keyDefsCruise[0]),
			    atLine, nLinesTotal);
	    if (prompt)
	      EditWindow (READew, &EWscr);
	    else
	      EWscr.key = ENTERkey_FSI;
	    while (loop) {
	      switch (EWscr.key) {
		case CRUISEkey_EXPORT:
		  prompt = FALSE;       /* No `break' is intentional. */
		case ENTERkey_FSI:
		  MakeNUL (screenLines);
		  lineN = 0;
		  loop = FALSE;
		  break;
		case HELPkey_FSI:
		  OnlineHelpWindow ("cdfxp.ilh", SCREENhelpID);
		  EditWindow (READew, &EWscr);
		  break;
		case EXITkey_FSI:
		  EditWindow (DELETEew, &EWscr, TRUE);
		  return TRUE;
		default:
		  EditWindow (BEEPew, &EWscr);
		  EditWindow (READew, &EWscr);
		  break;
	      }
	    }
	    timeMark = SystemClock ();
	  }
	  strcatX (screenLines, line, SCREENtextMAX);
	  strcatX (screenLines, "\n", SCREENtextMAX);
	  lineN++;
	}
	/**********************************************************************
	* Check filter status to see if this record should be aborted.
	**********************************************************************/
	if (filterStatus == FAILrecord) {
	  atLine += nValues;
	  break;
	}
	/**********************************************************************
	* If same dimensionalities, increment indices.
	**********************************************************************/
	if (sameGt0) {
	  if (outRowMajor)
	    IncrIndicesFirstLastRow (numDims, firstIndices,
				     lastIndices, indices);
	  else
	    IncrIndicesFirstLastCol (numDims, firstIndices,
				     lastIndices, indices);
	}
	/**********************************************************************
	* Increment percentage counter.
	**********************************************************************/
	atLine++;
     }
  }
  /****************************************************************************
  * End of listing.
  ****************************************************************************/
  EWscr.tLines = keyDefsEnd;
  EditWindow (UPDATEew, &EWscr, LogicalTRUE);
  EditWindow (READew, &EWscr);
  EditWindow (DELETEew, &EWscr, TRUE);
  return TRUE;
}

/******************************************************************************
* ToFileVert.
* Output to ASCII/file, vertical listing.
* Returns FALSE if a fatal CDF error occurred (meaning NO_MORE_ACCESS).
******************************************************************************/

Logical ToFileVert () {
  int lineL = 0, filterStatus;
  struct ItemStruct *Item, *exportHead; FILE *oFp; char *line;
  long firstRec, lastRec, recN, maxRecordValues = 0, valueN, numDims;
  long dimSizes[CDF_MAX_DIMS], nLinesTotal, atLine;
  long indices[CDF_MAX_DIMS], firstIndices[CDF_MAX_DIMS];
  long lastIndices[CDF_MAX_DIMS]; Logical same, sameGt0;
  Logical outRowMajor = ROWmajor(MAJORITYtoOUT(opt.majority,inMajority));
  CDFstatus status; Logical first = TRUE, cdfFatal;
  static char pctMsg[] = "Listing started...";
  static char keyDefsBlank[] = "\n\n";
  static char keyDefsAbort[] = "Abort: ________\n\n";
  int iptf;
  /****************************************************************************
  * Encode key definitions first time.
  ****************************************************************************/
  if (first) {
    char *p1 = keyDefsAbort;
    EncodeKeyDefinitions (1, &p1, ABORTkey_EXPORT);
    first = FALSE;
  }
  /****************************************************************************
  * Build export list.
  ****************************************************************************/
  BuildExportList (&exportHead, LogicalFALSE);
  /****************************************************************************
  * Determine if the dimensionalities are all the same.
  ****************************************************************************/
  same = SameDimensionalities (&numDims, dimSizes, exportHead);
  sameGt0 = (same && numDims > 0);
  /****************************************************************************
  * Validate `Record' and `Indices' selections.
  ****************************************************************************/
  ValidateRecordIndices (OUTPUTtoFILEv, same, numDims, exportHead);
  /****************************************************************************
  * Calculate the width of each line and the maximum record number and
  * maximum number of values per record (array).
  ****************************************************************************/
  for (Item = exportHead; Item != NULL; Item = Item->nextExport) {
     if (Item->output) {
       switch (Item->type) {
	 case RECORDt:
	 case INDICESt:
	   lineL += BOO(lineL == 0,0,opt.spacing) + Item->width;
	   break;
	 case VARIABLEt: {
	   int width = BOO(simpleMode,
			   StandardWidth(Item->Var->dataType,
					 Item->Var->numElems),Item->width);
	   int addL = BOO(lineL == 0,0,opt.spacing) + width;
	   lineL += addL;
	   maxRecordValues = MAXIMUM(maxRecordValues,Item->Var->nRecordValues);
	   break;
	 }
       }
     }
  }
  /****************************************************************************
  * Check if a valid listing.
  ****************************************************************************/
  if (lineL == 0) {
    DisplayMessage ("Nothing selected for listing.", BEEPWAIT1);
    return TRUE;
  }
  /****************************************************************************
  * Remove items from the export list that are not being output or filtered.
  ****************************************************************************/
  RemoveExportItems (&exportHead);
  /****************************************************************************
  * Determine first/last records/indices.
  ****************************************************************************/
  switch (FirstLastRecord(&firstRec,&lastRec,FALSE,exportHead)) {
    case PASSED: break;
    case FAILED: return TRUE;
    case FATAL: return FALSE;
  }
  if (range == 1L) {
    if (!AdjustFirstLastRecords(&firstRec, &lastRec)) return FALSE;
    if (firstRec == -1) {
      DisplayMessage ("No records found for listing.", BEEPWAIT1);
      return TRUE;
    }
  } else if (range == 2L) {
    if (firstRec < recordStart) firstRec = recordStart;
    if (lastRec > recordEnd) lastRec = recordEnd;

  }
  if (sameGt0) {
    switch (FirstLastIndices(numDims,dimSizes,firstIndices,lastIndices,
			     &maxRecordValues,FALSE,exportHead)) {
      case PASSED: break;
      case FAILED: return TRUE;
      case FATAL: return FALSE;
    }
  }
  /****************************************************************************
  * Allocate buffer for line.
  ****************************************************************************/
  line = (char *) cdf_AllocateMemory ((size_t) (lineL + 1), NULL);
  if (line == NULL) {
    DisplayMessage ("Not enough memory.", BEEPWAIT);
    return TRUE;
  }
  /****************************************************************************
  * Prompt for listing file path (unless in batch mode) and open file.
  ****************************************************************************/
  if (!BATCH(batchMode)) {
    if (!PromptFor(outputText,DU_MAX_PATH_LEN,strlen(outputText),
		   "Enter path for listing file...",oFILEhelpID)) {
      cdf_FreeMemory (line, FatalError);
      return TRUE;
    }
  }
  oFp = fopen (outputText, "w");
  if (oFp == NULL) {
    DisplayMessage (listOpenError, BEEPWAIT);
    cdf_FreeMemory (line, FatalError);
    return TRUE;
  }
  /****************************************************************************
  * Output header line(s).
  ****************************************************************************/
  if (opt.textHeading) {
    if (!ListAttributes(oFp,&cdfFatal)) {
      if (fclose(oFp) == EOF) DisplayMessage (listCloseError, BEEPWAIT);
      cdf_FreeMemory (line, FatalError);
      return (!cdfFatal);
    }
    MakeNUL (line);
    for (Item = exportHead; Item != NULL; Item = Item->nextExport) {
       if (Item->output) {
         if (!NULstring(line)) CatNcharacters (line, opt.spacing, ' ');
         switch (Item->type) {
	   case RECORDt:
	     CatToString (line, "Record", Item->width, LEFT_JUSTIFY, "*");
	     break;
	   case INDICESt:
	     CatToString (line, "Indices", Item->width, LEFT_JUSTIFY, "*");
	     break;
	   case VARIABLEt: {
	     int standardWidth = StandardWidth(Item->Var->dataType,
					       Item->Var->numElems);
	     int valueWidth = BOO(simpleMode,standardWidth,Item->width);
	     CatToString (line, Item->Var->name, valueWidth,
			  CENTER_JUSTIFY, "*");
	     break;
	   }
         }
       }
    }
    iptf = fprintf(oFp,BOO(poundedheading==1,"\n# Variables:\n\n%s\n",
                           "\nVariables:\n\n%s\n"),line);
    if (iptf < 0) {
      DisplayMessage (listWriteError, BEEPWAIT);
      if (fclose(oFp) == EOF) DisplayMessage (listCloseError, BEEPWAIT);
      cdf_FreeMemory (line, FatalError);
      return TRUE;
    }
  }
  /****************************************************************************
  * If same dimensionalities, initialize indices.
  ****************************************************************************/
  if (sameGt0) {
    ARRAYtoARRAY (indices, firstIndices, numDims)
  }
  /****************************************************************************
  * Read/output values until end of listing.
  ****************************************************************************/
  nLinesTotal = (lastRec - firstRec + 1) * maxRecordValues;
  DisplayPctComplete (NO_PCT, pctMsg);
  NEWkeyDEFS (EWkey, keyDefsAbort, batchMode)
  for (recN = firstRec, atLine = 0; recN <= lastRec; recN++) {
     for (valueN = 0; valueN < maxRecordValues; valueN++) {
	/**********************************************************************
	* Check for abort key.
	**********************************************************************/
	if (AbortListing(oFp,line)) {
	  NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
	  return TRUE;
	}
	/**********************************************************************
	* Encode line.
	**********************************************************************/
	status = EncodeLineVert(line,recN,valueN,numDims,indices,
				same,&filterStatus,exportHead,
				outRowMajor,simpleMode,(size_t)(lineL+1));
	DisplayStatus (status, readingCDF);
	if (StatusBAD(status)) {
	  if (fprintf(oFp,"Incomplete listing.\n") < 0) {
	    DisplayMessage (listWriteError, BEEPWAIT);
	  }
	  if (fclose(oFp) == EOF) DisplayMessage (listCloseError, BEEPWAIT);
	  cdf_FreeMemory (line, FatalError);
	  return FALSE;
	}
	/**********************************************************************
	* Check filter status to see if the line should be output.
	**********************************************************************/
	if (SHOWline(filterStatus)) {
	  if (fprintf(oFp,"%s\n",line) < 0) {
	    DisplayMessage (listWriteError, BEEPWAIT);
	    if (fclose(oFp) == EOF) DisplayMessage (listCloseError, BEEPWAIT);
	    cdf_FreeMemory (line, FatalError);
	    return TRUE;
	  }
	}
	/**********************************************************************
	* Check filter status to see if this record should be aborted.
	**********************************************************************/
	if (filterStatus == FAILrecord) {
	  while (valueN < maxRecordValues) {
	    DisplayPctComplete (PCT(atLine,nLinesTotal,1,1), pctMsg);
	    valueN++;
	    atLine++;
	  }
	  break;
	}
	/**********************************************************************
	* If same dimensionalities, increment indices.
	**********************************************************************/
	if (sameGt0) {
	  if (outRowMajor)
	    IncrIndicesFirstLastRow (numDims, firstIndices,
				     lastIndices, indices);
	  else
	    IncrIndicesFirstLastCol (numDims, firstIndices,
				     lastIndices, indices);
	}
	/**********************************************************************
	* Update percentage complete message.
	**********************************************************************/
	DisplayPctComplete (PCT(atLine,nLinesTotal,1,1), pctMsg);
	atLine++;
     }
  }
  /****************************************************************************
  * End of listing.
  ****************************************************************************/
  NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
  if (fclose(oFp) == EOF) {
    DisplayMessage (listCloseError, BEEPWAIT);
    cdf_FreeMemory (line, FatalError);
    return TRUE;
  }
  cdf_FreeMemory (line, FatalError);
  if (!BATCH(batchMode)) DisplayMessage ("Complete.", NOBEEPWAIT1);
  return TRUE;
}

/******************************************************************************
* EncodeLineVert.
******************************************************************************/

CDFstatus EncodeLineVert (line, recN, valueN, numDims, indices, same,
			  filterStatus, exportHead, outRowMajor, standard,
			  widthW)
char *line;
long recN;
long valueN;                    /* Ignored if same dimensionalities. */
long numDims;                   /* Ignored if different dimensionalities. */
long indices[CDF_MAX_DIMS];     /* Ignored if different dimensionalities. */
Logical same;
int *filterStatus;
struct ItemStruct *exportHead;
Logical outRowMajor;
Logical standard;
size_t widthW;
{
  struct ItemStruct *Item; CDFstatus pStatus = CDF_OK; Logical failedFilter;
  /****************************************************************************
  * Initialize line and filter status.
  ****************************************************************************/
  MakeNUL (line);
  *filterStatus = PASSes;
  /****************************************************************************
  * Encode each exported item.
  ****************************************************************************/
  for (Item = exportHead; Item != NULL; Item = Item->nextExport) {
     switch (Item->type) {
       /***********************************************************************
       * Record number.
       ***********************************************************************/
       case RECORDt: {
	 failedFilter = RECORDfailedFILTER(Item,recN);
	 if (failedFilter) {
	   if (opt.showFiltered)
	     *filterStatus = SHOWit;
	   else {
	     *filterStatus = FAILrecord;
	     return pStatus;
	   }
	 }
	 if (Item->output) {
	   if (!NULstring(line)) CatNcharacters (line, opt.spacing, ' ');
	   if (failedFilter)
	     CatNcharacters (line, Item->width, '-');
	   else
	     EncodeRecordJustify (EofS(line), recN, -(Item->width),
				  widthW-strlen(line));
	 }
	 break;
       }
       /***********************************************************************
       * Indices.
       *   This case will not occur if different dimensionalities.
       ***********************************************************************/
       case INDICESt: {
	 /*********************************************************************
	 * Filter indices.
	 *********************************************************************/
	 failedFilter = INDICESfailedFILTER(Item,indices);
	 if (failedFilter) {
	   if (opt.showFiltered)
	     *filterStatus = SHOWit;
	   else {
	     *filterStatus = FAILline;
	     return pStatus;
	   }
	 }
	 /*********************************************************************
	 * Output indices (if requested).
	 *********************************************************************/
	 if (Item->output) {
	   if (!NULstring(line)) CatNcharacters (line, opt.spacing, ' ');
	   if (failedFilter)
	     CatNcharacters (line, Item->width, '-');
	   else
	     EncodeIndicesJustify (EofS(line), numDims, indices,
				   -(Item->width),
				   widthW-strlen(line));
	 }
	 break;
       }
       /***********************************************************************
       * Variable.
       ***********************************************************************/
       case VARIABLEt: {
	 CDFstatus status; int dimN; int style;
	 int standardWidth = StandardWidth(Item->Var->dataType,
					   Item->Var->numElems);
	 int width = BOO(standard,standardWidth,Item->width);
	 char *format = BOO(standard,
			    StandardFormat(Item->Var->dataType),
			    Item->Var->format);
         if (TT2000dataType(Item->Var->dataType)) style = TT2000_3_STYLE;
         else style = opt.epochStyle;
	 /*********************************************************************
	 * If different dimensionalities and first value in a record,
	 * initialize the indices and value number for the variable.
	 *********************************************************************/
	 if (!same) {
	   if (valueN == 0) {
	     for (dimN = 0; dimN < Item->Var->numDims; dimN++) {
		Item->Var->indices[dimN] = 0;
	     }
	     Item->Var->valueN = 0;
	   }
	 }
	 /*********************************************************************
	 * If different dimensionalities, check if no more values for this
	 * variable.
	 *********************************************************************/
	 if (!same) {
	   if (Item->Var->valueN == Item->Var->nRecordValues) {
	     if (Item->output) {
	       if (!NULstring(line)) CatNcharacters (line, opt.spacing, ' ');
	       CatNcharacters (line, width, ' ');
	     }
	     break;
	   }
	 }
	 /*********************************************************************
	 * Select variable, record number, and indices and then read value.
	 *********************************************************************/
	 status = CDFlib (SELECT_, BOO(Item->Var->zVar,
				       zVAR_,rVAR_), Item->Var->varN,
				   BOO(Item->Var->zVar,
				       zVAR_RECNUMBER_,rVARs_RECNUMBER_), recN,
				   BOO(Item->Var->zVar,
				       zVAR_DIMINDICES_,
				       rVARs_DIMINDICES_),
					  BOO(same,indices,Item->Var->indices),
			  GET_, BOO(Item->Var->zVar,
				    zVAR_DATA_,rVAR_DATA_), Item->Var->value,
			  NULL_);
	 if (!sX(status,&pStatus)) return pStatus;
	 /*********************************************************************
	 * Filter value.  If the value failed the filter...
	 *********************************************************************/
	 failedFilter = VARfailedFILTER(Item,Item->Var->value);
	 if (failedFilter) {
	   if (opt.showFiltered)
	     *filterStatus = SHOWit;
	   else {
	     if (same) {
	       if (Item->Var->scalar)
		 *filterStatus = FAILrecord;
	       else
		 *filterStatus = FAILline;
	       return pStatus;
	     }
	     else {
	       if (Item->Var->nRecordValues == 1) {
		 *filterStatus = FAILrecord;
		 return pStatus;
	       }
	     }
	   }
	 }
	 /*********************************************************************
	 * Encode value.
	 *********************************************************************/
	 if (Item->output) {
	   if (!NULstring(line)) CatNcharacters (line, opt.spacing, ' ');
	   if (failedFilter)
	     CatNcharacters (line, width, '-');
	   else {
             if ((Item->Var->dataType == CDF_FLOAT) ||
                 (Item->Var->dataType == CDF_REAL4)) {
               if (!isnan((double)*(float *)Item->Var->value) &&
                   *(float *)Item->Var->value <= DEFAULT_FLOAT_PADVALUE) {
                 EncodeValue (Item->Var->dataType, Item->Var->value,
                              EofS(line), style,
                              (size_t) widthW-strlen(line));
             } else
	       EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems,
				   Item->Var->value, EofS(line),
				   format, width, width, style,
				   widthW-strlen(line));
             } else if ((Item->Var->dataType == CDF_DOUBLE) ||
                      (Item->Var->dataType == CDF_REAL8)) {
               if (!isnan(*(double *)Item->Var->value) &&
                   *(double *)Item->Var->value <= DEFAULT_DOUBLE_PADVALUE) {
                 EncodeValue (Item->Var->dataType, Item->Var->value,
                              EofS(line), style,
                              (size_t) widthW-strlen(line));
               } else
	         EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems,
				     Item->Var->value, EofS(line),
				     format, width, width, style,
				     widthW-strlen(line));
             } else
	       EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems,
				   Item->Var->value, EofS(line),
				   format, width, width, style,
				   widthW-strlen(line));
	   }
	 }
	 /*********************************************************************
	 * If different dimensionalities, increment to next value/indices.
	 *********************************************************************/
	 if (!same) {
	   if (outRowMajor)
	     INCRindicesROW (Item->Var->numDims, Item->Var->dimSizes,
			     Item->Var->indices);
	   else
	     INCRindicesCOL (Item->Var->numDims, Item->Var->dimSizes,
			     Item->Var->indices);
	   Item->Var->valueN++;
	 }
	 break;
       }
     }
  }
  return pStatus;
}

/******************************************************************************
* ListAttributes.
* Returns FALSE if an error occurred.
******************************************************************************/

Logical ListAttributes (oFp, cdfFatal)
FILE *oFp;
Logical *cdfFatal;	/* Set TRUE if a fatal CDF error occurred. */
{
  CDFstatus status; long attrN, nAttrs;
  int isize;
  /****************************************************************************
  * Display heading.
  ****************************************************************************/
  if (fprintf(oFp,BOO(poundedheading==1,"# Global attributes:\n#",
                      "Global attributes:\n")) < 0) {
    *cdfFatal = FALSE;
    return FALSE;
  }
  /****************************************************************************
  * Inquire the number of attributes.
  ****************************************************************************/
  status = CDFlib (GET_, CDF_NUMATTRS_, &nAttrs,
		   NULL_);
  DisplayStatus (status, readingCDF);
  if (StatusBAD(status)) {
    *cdfFatal = TRUE;
    return FALSE;
  }
  /****************************************************************************
  * For each attribute...
  ****************************************************************************/
  for (attrN = 0; attrN < nAttrs; attrN++) {
     long scope;
     /*************************************************************************
     * Inquire the attribute scope.
     *************************************************************************/
     status = CDFlib (SELECT_, ATTR_, attrN,
		      GET_, ATTR_SCOPE_, &scope,
		      NULL_);
     DisplayStatus (status, readingCDF);
     if (StatusBAD(status)) {
       *cdfFatal = TRUE;
       return FALSE;
     }
     /*************************************************************************
     * If a gAttribute...
     *************************************************************************/
     if (scope == GLOBAL_SCOPE) {
       long maxEntry, entryN; char attrName[CDF_ATTR_NAME_LEN+1];
       /***********************************************************************
       * Inquire the attribute's name and maximum gEntry number.
       ***********************************************************************/
       status = CDFlib (GET_, ATTR_NAME_, attrName,
			ATTR_MAXgENTRY_, &maxEntry,
			NULL_);
       DisplayStatus (status, readingCDF);
       if (StatusBAD(status)) {
	 *cdfFatal = TRUE;
	 return FALSE;
       }
       RemoveTrailingBlanks (attrName);
       if (!cdaweb) {
         if (fprintf(oFp,BOO(poundedheading==1,"\n#   %s\n#",
                             "\n  %s\n"),attrName) < 0) {
           *cdfFatal = FALSE;
           return FALSE;
         }
       } else {
         if (fprintf(oFp,BOO(poundedheading==1,"#   %s",
                             " %s"),attrName) < 0) {
           *cdfFatal = FALSE;
           return FALSE;
         }
       }
       /***********************************************************************
       * For each gEntry...
       ***********************************************************************/
       for (entryN = 0; entryN <= maxEntry; entryN++) {
	  long dataType, numElems, nBytes; void *value;
	  char encoded[MAXgENTRYencodedLEN+1];
	  char *encoded2 = NULL;
          int style;
	  /********************************************************************
	  * If this entry might exist...
	  ********************************************************************/
	  status = CDFlib (SELECT_, gENTRY_, entryN,
			   CONFIRM_, CURgENTRY_EXISTENCE_,
			   NULL_);
	  if (status != NO_SUCH_ENTRY) {
	    /******************************************************************
	    * Check if some other error occurred.
	    ******************************************************************/
	    DisplayStatus (status, readingCDF);
	    if (StatusBAD(status)) {
	      *cdfFatal = TRUE;
	      return FALSE;
	    }
	    /******************************************************************
	    * Inquire the data specification.
	    ******************************************************************/
	    status = CDFlib (GET_, gENTRY_DATATYPE_, &dataType,
				   gENTRY_NUMELEMS_, &numElems,
			     NULL_);
	    DisplayStatus (status, readingCDF);
	    if (StatusBAD(status)) {
	      *cdfFatal = TRUE;
	      return FALSE;
	    }
            if (TT2000dataType(dataType)) style = TT2000_3_STYLE;
            else style = opt.epochStyle;
	    /******************************************************************
	    * Inquire the value.
	    ******************************************************************/
	    nBytes = CDFelemSize(dataType) * numElems;
	    value = cdf_AllocateMemory ((size_t) nBytes, FatalError);
	    if (cdaweb) {
	      if (STRINGdataType(dataType))
	        isize = nBytes + 3;
	      else
	        isize = MAXgENTRYencodedLEN + 3;
	      encoded2 = (char *) cdf_AllocateMemory ((size_t) isize,
						      FatalError);
	    }
	    status = CDFlib (GET_, gENTRY_DATA_, value,
			     NULL_);
	    DisplayStatus (status, readingCDF);
	    if (StatusBAD(status)) {
	      *cdfFatal = TRUE;
	      if (encoded2 != NULL) cdf_FreeMemory (encoded2, FatalError);
	      return FALSE;
	    }
	    /******************************************************************
	    * List the value.
	    ******************************************************************/
	    if (!cdaweb)
	      EncodeValuesFormat (dataType, numElems, value, encoded, NULL,
				  0, MAXgENTRYencodedLEN, style,
				  (size_t) sizeof(encoded));
	    else
	      EncodeValuesFormat (dataType, numElems, value, encoded2, NULL,
				  0, isize, style,
				  (size_t) isize);
            if (!cdaweb) {
              if (fprintf(oFp,BOO(poundedheading==1,"    %s\n#",
                                  "    %s\n"),encoded) < 0) {
                *cdfFatal = FALSE;
                return FALSE;
              }
	    } else {
	      int pos, count, len = strlen (encoded2);
	      char tmp[83];
	      char *loc;
	      pos = 0;
	      do {
	        if (strlen(attrName) < 6) fprintf(oFp,"\t\t\t"); 
	        else if (strlen(attrName) < 14) fprintf(oFp,"\t\t");
	        else fprintf(oFp,"\t");
	        strcpyX (tmp, ((char *)encoded2)+pos, 82);
	        count = strlen(tmp);
	        if (count == 82) {
	          loc = strrchr (tmp, ' ');
	          if (loc != NULL)
	            count = loc - (char *) tmp;
	          strcpyX (tmp, ((char *)encoded2)+pos, count);
	        }
                if (fprintf(oFp,BOO(poundedheading==1,"# %s\n#",
                                    "%s\n"),tmp) < 0) {
	          *cdfFatal = FALSE;
                  cdf_FreeMemory (encoded2, FatalError);
                  return FALSE;
                }
	        pos += count;
	      } while (pos < len);
	    }
            cdf_FreeMemory (value, FatalError);
            if (cdaweb) cdf_FreeMemory (encoded2, FatalError);
          }
       }
     }
  }
  return TRUE;
}

/******************************************************************************
* ToCDF.
* Output to CDF.
* Returns FALSE if a fatal error occurred on the input CDF only.
* This routine must ensure that the input CDF is made the current CDF
* before returning.  It is assumed that the input CDF is also the current
* CDF when this routine is called.
******************************************************************************/

Logical ToCDF (inID)
CDFid inID;
{
  int dimN; CDFid outID; Logical same, sameGt0;
  struct ItemStruct *Item; CDFstatus status;
  struct ItemStruct *exportHead;
  long outMajority = MAJORITYtoOUT(opt.majority,inMajority);
  long firstRec, lastRec, nAttrs;
  long numDims, dimSizes[CDF_MAX_DIMS];
  long firstIndices[CDF_MAX_DIMS], lastIndices[CDF_MAX_DIMS];
  static char keyDefsBlank[] = "\n\n";
  long rNumDims, rDimSizes[CDF_MAX_DIMS];
  /****************************************************************************
  * Build export list.
  ****************************************************************************/
  BuildExportList (&exportHead, LogicalFALSE);
  /****************************************************************************
  * Determine if the variables being output/filtered have the same
  * dimensionalities.
  ****************************************************************************/
  same = SameDimensionalities (&numDims, dimSizes, exportHead);
  sameGt0 = (same && numDims > 0);
  /****************************************************************************
  * Validate `Record' and `Indices' selections.
  ****************************************************************************/
  ValidateRecordIndices (OUTPUTtoCDF, same, numDims, exportHead);
  /****************************************************************************
  * Check the number of variables being output.  Note that at this point if
  * an item is being output then it must be a variable.
  ****************************************************************************/
  for (Item = exportHead; Item != NULL; Item = Item->nextExport) {
     if (Item->output) break;
  }
  if (Item == NULL) {
    DisplayMessage ("No variables selected for output.", BEEPWAIT1);
    return TRUE;
  }
  /****************************************************************************
  * Remove items from the export list that are not being output or filtered.
  ****************************************************************************/
  RemoveExportItems (&exportHead);
  /****************************************************************************
  * Calculate the first/last record and indices.
  ****************************************************************************/
  switch (FirstLastRecord(&firstRec,&lastRec,TRUE,exportHead)) {
    case PASSED: break;
    case FAILED: return TRUE;
    case FATAL: return FALSE;
  }
  if (range == 1L) {
    if (!AdjustFirstLastRecords(&firstRec, &lastRec)) return FALSE;
    if (firstRec == -1) {
      DisplayMessage ("No records found for listing.", BEEPWAIT1);
      return TRUE;
    }
  } else if (range == 2L) {
    if (firstRec < recordStart) firstRec = recordStart;
    if (lastRec > recordEnd) lastRec = recordEnd;
  }

  if (sameGt0) {
    switch (FirstLastIndices(numDims,dimSizes,firstIndices,
			     lastIndices,NULL,TRUE,exportHead)) {
      case PASSED: break;
      case FAILED: return TRUE;
      case FATAL: return FALSE;
    }
  }
  /****************************************************************************
  * Prompt for new CDF's path unless in batch mode.
  ****************************************************************************/
  if (!BATCH(batchMode)) {
    if (!PromptFor(outputCDF,CDF_PATHNAME_LEN,strlen(outputCDF),
		   "Enter path for output CDF...",oCDFhelpID)) return TRUE;
  }
  /****************************************************************************
  * If same dimensionalities, calculate rDimensionality for output CDF.  If
  * different dimensionalities, inquire rDimensionality of input CDF.
  ****************************************************************************/
  if (sameGt0) {
    for (dimN = 0; dimN < numDims; dimN++) {
       dimSizes[dimN] = lastIndices[dimN] - firstIndices[dimN] + 1;
    }
  }
  else {
    status = CDFlib (GET_, rVARs_NUMDIMS_, &numDims,
			   rVARs_DIMSIZES_, dimSizes,
		     NULL_);
    DisplayStatus (status, readingCDF);
    if (StatusBAD(status)) return FALSE;
  }
  /****************************************************************************
  * Delete existing CDF with same name?
  ****************************************************************************/
  if (opt.deleteExisting) {
    if (IsCDF(outputCDF)) {
      if (!BATCH(batchMode)) {
	DisplayMessage ("Deleting existing output CDF...", NOWAIT);
      }
      status = CDFlib (OPEN_, CDF_, outputCDF, &outID,
		       NULL_);
      DisplayStatus (status, "opening existing CDF");
      if (StatusBAD(status)) {
	SELECTcdf (inID);
	return TRUE;
      }
      status = CDFlib (DELETE_, CDF_,
		       NULL_);
      DisplayStatus (status, "deleting existing CDF");
      if (StatusBAD(status)) {
	CDFclose (outID);
	SELECTcdf (inID);
	return TRUE;
      }
      if (!BATCH(batchMode)) DisplayMessage ("", NOWAIT);
    }
  }
  /****************************************************************************
  * Create output CDF.
  ****************************************************************************/
  if (!BATCH(batchMode)) DisplayMessage ("Creating output CDF...", NOWAIT);
  status = CDFlib (GET_, rVARs_NUMDIMS_, &rNumDims,
			 rVARs_DIMSIZES_, rDimSizes,
		   NULL_);
  DisplayStatus (status, readingCDF);
  status = CDFlib (CREATE_, CDF_, outputCDF, rNumDims, rDimSizes, &outID,
		   NULL_);
  DisplayStatus (status, "creating output CDF");
  if (StatusBAD(status)) {
    SELECTcdf (inID);
    return TRUE;
  }
  status = CDFlib (SELECT_, CDF_CACHESIZE_, workingCache,
			    STAGE_CACHESIZE_, stageCache,
			    COMPRESS_CACHESIZE_, compressCache,
		   PUT_, CDF_FORMAT_, BOO(opt.singleFile,SINGLE_FILE,
							 MULTI_FILE),
			 CDF_ENCODING_, opt.encoding,
			 CDF_MAJORITY_, outMajority,
			 CDF_COMPRESSION_, CDFcType, CDFcParms,
			 CDF_CHECKSUM_, CDFchecksum,
		   NULL_);
  DisplayStatus (status, "configuring output CDF");
  if (StatusBAD(status)) {
    CDFclose (outID);
    SELECTcdf (inID);
    return TRUE;
  }
  /****************************************************************************
  * Create attributes/entries and variables in output CDF.
  ****************************************************************************/
  switch (CopyAttributesANDgEntries(inID,outID,&nAttrs)) {
    case SUCCESS:
      break;
    case FATALin:
      CDFclose (outID);
      SELECTcdf (inID);
      return FALSE;
    case FATALout:
      CDFclose (outID);
      SELECTcdf (inID);
      return TRUE;
  }
  switch (CopyVariablesANDrzEntries(inID,outID,nAttrs,same,
				    numDims,dimSizes,exportHead)) {
    case SUCCESS:
      break;
    case FATALin:
      CDFclose (outID);
      SELECTcdf (inID);
      return FALSE;
    case FATALout:
      CDFclose (outID);
      SELECTcdf (inID);
      return TRUE;
  }
  /****************************************************************************
  * Write to variables in output CDF.
  ****************************************************************************/
  switch (BOO(sameGt0,ToCDFsameGt0(inID,outID,firstRec,lastRec,
				   numDims,dimSizes,firstIndices,
				   outMajority,exportHead),
		      ToCDFdiffOrZero(inID,outID,firstRec,
				      lastRec,outMajority,exportHead))) {
    case SUCCESS:
      break;
    case FATALin:
      CDFclose (outID);
      SELECTcdf (inID);
      return FALSE;
    case FATALout:
      CDFclose (outID);
      SELECTcdf (inID);
      return TRUE;
  }
  /****************************************************************************
  * Close output CDF.
  ****************************************************************************/
  NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
  if (!BATCH(batchMode)) DisplayMessage ("Closing CDF...", NOWAIT);
  if (dumpStats) {
    vSTATS vStatsDotCDF, vStatsStage, vStatsCompress;
    char temp1[MAX_SCREENLINE_LEN+1],
	 temp2[MAX_SCREENLINE_LEN+1],
	 temp3[MAX_SCREENLINE_LEN+1];
    if (!BATCH(batchMode)) DisplayMessage ("", NOWAIT);
    status = CDFlib (SELECT_, CDF_, outID,
		     CLOSE_, CDFwithSTATS_, &vStatsDotCDF,
					    &vStatsStage,
					    &vStatsCompress,
		     NULL_);
    DisplayStatus (status, "closing output CDF");
    if (StatusOK(status)) {
      if (vStatsDotCDF.maxBuffers > 0) {
        BuildStatistics ("output CDF DotCDF file", &vStatsDotCDF,
		         temp1, temp2, temp3);
	if (BATCH(batchMode))
	  printf ("%s\n%s\n%s\n", temp1, temp2, temp3);
	else
	  InfoWindow (temp1, temp2, temp3, FALSE, FALSE, 0);
      }
      if (vStatsStage.maxBuffers > 0) {
        BuildStatistics ("output CDF staging file", &vStatsStage,
		         temp1, temp2, temp3);
	if (BATCH(batchMode))
	  printf ("%s\n%s\n%s\n", temp1, temp2, temp3);
	else
	  InfoWindow (temp1, temp2, temp3, FALSE, FALSE, 0);
      }
      if (vStatsCompress.maxBuffers > 0) {
        BuildStatistics ("output CDF compression scratch file",
			 &vStatsCompress, temp1, temp2, temp3);
	if (BATCH(batchMode))
	  printf ("%s\n%s\n%s\n", temp1, temp2, temp3);
	else
	  InfoWindow (temp1, temp2, temp3, FALSE, FALSE, 0);
      }
    }
  }
  else {
    status = CDFclose (outID);
    DisplayStatus (status, "closing output CDF");
    if (StatusOK(status)) {
      if (!BATCH(batchMode)) DisplayMessage ("", NOBEEPWAIT1);
    }
  }
  SELECTcdf (inID);
  return TRUE;
}

/******************************************************************************
* ToCDFsameGt0.
* Same dimensionalities and the number of dimensions is greater than zero.
* Returns SUCCESS, FATALin, or FATALout;
******************************************************************************/

int ToCDFsameGt0 (inID, outID, firstRec, lastRec, numDims, dimSizes,
		  firstIndices, outMajority, exportHead)
CDFid inID;
CDFid outID;
long firstRec;
long lastRec;
long numDims;           /* For the output CDF. */
long dimSizes[];        /* For the output CDF. */
long firstIndices[];    /* From the input CDF. */
long outMajority;
struct ItemStruct *exportHead;
{
  long nValues, nRecordValues, indicesI[CDF_MAX_DIMS], nHypers, hyperN;
  long recF, recL; static Logical first = TRUE;
  int dimN, i; Byte1 ***handles; size_t *nValueBytes;
  int scalarCount, hyperCount, varCount, scalarX, hyperX;
  struct ItemStruct *Item, *prevItem, *scalarHead, *scalarTail;
  struct ItemStruct *hyperHead, *hyperTail;
  struct HyperStruct hyper; struct GroupStruct groups;
  Logical filteringScalars, filteringHypers; CDFstatus status;
  static char rvMsg[] = "Reading/writing variable values...";
  static char keyDefsAbort[] = "Abort: ________\n\n";
  /****************************************************************************
  * Encode key definitions first time.
  ****************************************************************************/
  if (first) {
    char *p1 = keyDefsAbort;
    EncodeKeyDefinitions (1, &p1, ABORTkey_EXPORT);
    first = FALSE;
  }
  /****************************************************************************
  * Read/write first (and only) record for NRV variables.  Note that 
  * if an item is being output it must be a variable.
  ****************************************************************************/
  switch (OutputNRVvalues(inID,outID,exportHead,TRUE,
			  dimSizes,firstIndices,outMajority)) {
    case SUCCESS: break;
    case FATALin: return FATALin;
    case FATALout: return FATALout;
  }
  /****************************************************************************
  * Remove NRV variables from export list if they are not being filtered.
  ****************************************************************************/
  for (Item = exportHead, prevItem = NULL;
       Item != NULL; Item = Item->nextExport) {
    if (!Item->Var->recVary && !Item->filter) {
      if (prevItem == NULL)
	exportHead = Item->nextExport;
      else
	prevItem->nextExport = Item->nextExport;
    }
    else
      prevItem = Item;
  }
  /****************************************************************************
  * Determine scalars and hypers.
  ****************************************************************************/
  for (Item = exportHead, scalarCount = 0, hyperCount = 0;
       Item != NULL; Item = Item->nextExport) {
     if (Item->Var->scalar)
       scalarCount++;
     else
       hyperCount++;
  }
  varCount = scalarCount + hyperCount;
  if (varCount == 0) return SUCCESS;
  handles = (Byte1 ***) cdf_AllocateMemory ((size_t)varCount * sizeof(Byte1 **),
				       FatalError);
  nValueBytes = (size_t *) cdf_AllocateMemory ((size_t)varCount * sizeof(size_t),
					   FatalError);
  for (Item = exportHead, scalarX = 0, hyperX = scalarCount,
       scalarHead = scalarTail = NULL, hyperHead = hyperTail = NULL,
       filteringScalars = FALSE, filteringHypers = FALSE;
       Item != NULL; Item = Item->nextExport) {
     if (Item->Var->scalar) {
       handles[scalarX] = &(Item->Var->buffer);
       nValueBytes[scalarX++] = Item->Var->nValueBytes;
       if (scalarHead == NULL)
	 scalarHead = scalarTail = Item;
       else
	 scalarTail = scalarTail->nextScalar = Item;
       Item->nextScalar = NULL;
       if (Item->filter) filteringScalars = TRUE;
     }
     else {
       handles[hyperX] = &(Item->Var->buffer);
       nValueBytes[hyperX++] = Item->Var->nValueBytes;
       if (hyperHead == NULL)
	 hyperHead = hyperTail = Item;
       else
	 hyperTail = hyperTail->nextHyper = Item;
       Item->nextHyper = NULL;
       if (Item->filter) filteringHypers = TRUE;
     }
  }
  /****************************************************************************
  * Preallocate variable records.
  ****************************************************************************/
  if (opt.preAllocate) {
    switch (PreAllocateRecords(inID,outID,scalarHead,
			       hyperHead,&firstRec,&lastRec)) {
      case SUCCESS: break;
      case FATALin: return FATALin;
      case FATALout: return FATALout;
    }
  }
  /****************************************************************************
  * Allocate buffers.
  ****************************************************************************/
  DisplayPctComplete (NO_PCT, rvMsg);
  NEWkeyDEFS (EWkey, keyDefsAbort, batchMode)
  AllocateBuffers (lastRec - firstRec + 1, numDims, dimSizes, &groups,
		   scalarCount, hyperCount, handles, nValueBytes,
		   ROWmajor(inMajority), 5, FatalError);
  cdf_FreeMemory (handles, FatalError);
  cdf_FreeMemory (nValueBytes, FatalError);
  /****************************************************************************
  * Count number of values per record.
  ****************************************************************************/
  for (dimN = 0, nRecordValues = 1; dimN < numDims; dimN++) {
     nRecordValues *= dimSizes[dimN];
  }
  /****************************************************************************
  * Read/write each record for RV variables.  Note that NRV variables are
  * still read for filtering.
  ****************************************************************************/
  if (HyperFullRecord(&groups,numDims)) {
    /**************************************************************************
    * Each hyper read/write consists of one or more hyper records.
    **************************************************************************/
    long recNo = 0, recX, firstX, lastX, thisCount;
    InitHyperParms (&hyper, &groups, numDims, &nHypers, &nValues);
    for (hyperN = 0; hyperN < nHypers; hyperN++) {
       /***********************************************************************
       * Hyper read scalar input variables.
       ***********************************************************************/
       for (Item = scalarHead; Item != NULL; Item = Item->nextScalar) {
	  status = HYPERget (inID, Item->Var->varN, Item->Var->zVar,
			     firstRec + hyper.recNumber, hyper.recCount,
			     dimIndices_0, dimCounts_1, Item->Var->buffer);
	  DisplayStatus (status, readingCDF);
	  if (StatusBAD(status)) {
	    FreeExportBuffers (exportHead);
	    return FATALin;
	  }
       }
       DisplayPctComplete (PCT(hyperN,nHypers,1,2), rvMsg);
       if (AbortCDF(exportHead)) return SUCCESS;
       /***********************************************************************
       * Until no more records...
       ***********************************************************************/
       for (recX = 0;;) {
	  /********************************************************************
	  * Determine first record to be output.
	  ********************************************************************/
	  firstX = FindFirstRecord (recX, scalarHead, filteringScalars,
				    hyper.recCount);
	  if (firstX == NO_RECORD) break;
	  /********************************************************************
	  * Determine last record to be output.
	  ********************************************************************/
	  lastX = FindLastRecord (firstX, scalarHead, filteringScalars,
			          hyper.recCount);
	  thisCount = lastX - firstX + 1;
	  recF = firstRec + hyper.recNumber + firstX;
	  recL = firstRec + hyper.recNumber + lastX;
	  /********************************************************************
	  * Hyper write to scalar output variables (being output).
	  ********************************************************************/
	  for (Item = scalarHead; Item != NULL; Item = Item->nextScalar) {
	     if (Item->output) {
	       if (recF <= Item->Var->maxRec) {
		 long count = MINIMUM(recL,Item->Var->maxRec) - recF + 1;
		 Byte1 *buffer = Item->Var->buffer +
				(size_t) (Item->Var->nValueBytes * firstX);
		 status = HYPERput (outID, Item->Var->varNo, Item->Var->zVar,
				    recNo, count, dimIndices_0, dimCounts_1,
				    buffer);
		 DisplayStatus (status, writingCDF);
		 if (StatusBAD(status)) {
		   FreeExportBuffers (exportHead);
		   return FATALout;
		 }
	       }
	     }
	  }
	  if (AbortCDF(exportHead)) return SUCCESS;
	  /********************************************************************
	  * Read hyper input variables.
	  ********************************************************************/
	  for (Item = hyperHead; Item != NULL; Item = Item->nextHyper) {
	     status = HYPERget (inID, Item->Var->varN, Item->Var->zVar,
				recF, thisCount, firstIndices, dimSizes,
				Item->Var->buffer);
	     DisplayStatus (status, readingCDF);
	     if (StatusBAD(status)) {
	       FreeExportBuffers (exportHead);
	       return FATALin;
	     }
	     if (AbortCDF(exportHead)) return SUCCESS;
	  }
	  /********************************************************************
	  * Filter hyper variables.
	  ********************************************************************/
	  if (filteringHypers) FilterHypers (hyperHead, nValues);
	  /********************************************************************
	  * Write to hyper output variables (being output).
	  ********************************************************************/
	  for (Item = hyperHead; Item != NULL; Item = Item->nextHyper) {
	     if (Item->output) {
	       if (AbortCDF(exportHead)) return SUCCESS;
	       if (recF <= Item->Var->maxRec) {
		 long count = MINIMUM(recL,Item->Var->maxRec) - recF + 1;
		 if (!OutputHyperBuffer(outID,Item->Var->varNo,
					Item->Var->zVar,outMajority,
					recNo,count,dimIndices_0,dimSizes,
					numDims,dimSizes,Item->Var->buffer,
					nValues,TRUE,Item->Var->nValueBytes,
					nRecordValues)) {
		   FreeExportBuffers (exportHead);
		   return FATALout;
	         }
	       }
	     }
	  }
	  /********************************************************************
	  * Increment to next...
	  ********************************************************************/
	  recNo += thisCount;
	  recX = lastX + 1;
	  if (recX == hyper.recCount) break;
       }
       DisplayPctComplete (PCT(hyperN,nHypers,2,2), rvMsg);
       /***********************************************************************
       * Increment to next hyper group.
       ***********************************************************************/
       IncrHyperParms (&hyper, &groups, numDims, ROWmajor(inMajority),
		       &nValues);
    }
  }
  else {
    /**************************************************************************
    * Each hyper read/write consists of less than a full record.
    **************************************************************************/
    long nHypersPerRecord = HypersPerRecord (&groups, numDims);
    long recNi, recNo = -1;
    InitHyperParms (&hyper, &groups, numDims, &nHypers, &nValues);
    for (hyperN = 0; hyperN < nHypers; hyperN++) {
       recNi = firstRec + hyper.recNumber;
       /***********************************************************************
       * If at the start of a record...
       ***********************************************************************/
       if (HyperStartOfRecord(&hyper,numDims)) {
	 Logical skipRecord = FALSE;
	 /*********************************************************************
	 * Read/filter values for scalar input variables.
	 *********************************************************************/
	 for (Item = scalarHead; Item != NULL; Item = Item->nextScalar) {
	    status = SINGLEget (inID, Item->Var->varN, Item->Var->zVar,
				recNi, dimIndices_0, Item->Var->buffer);
	    DisplayStatus (status, readingCDF);
	    if (StatusBAD(status)) {
	      FreeExportBuffers (exportHead);
	      return FATALin;
	    }
	    if (VARfailedFILTER(Item,Item->Var->buffer)) {
	      skipRecord = TRUE;
	      break;
	    }
	 }
	 /*********************************************************************
	 * If this record is to be skipped, increment the hyper counter by
	 * one less than the number of hypers per record.  This is because
	 * the `continue' still causes the increment part of the `for' loop
	 * to be executed.
	 *********************************************************************/
	 if (skipRecord) {
	   for (i = 0; i < nHypersPerRecord; i++) {
	      IncrHyperParms (&hyper,&groups,numDims,
			      ROWmajor(inMajority),&nValues);
	   }
	   hyperN += (nHypersPerRecord - 1);
	   continue;
	 }
	 else
	   recNo++;
	 /*********************************************************************
	 * Single write to scalar output variables (being output).
	 *********************************************************************/
	 for (Item = scalarHead; Item != NULL; Item = Item->nextScalar) {
	    if (Item->output && recNi <= Item->Var->maxRec) {
	      status = SINGLEput (outID, Item->Var->varNo, Item->Var->zVar,
				  recNo, dimIndices_0, Item->Var->buffer);
	      DisplayStatus (status, readingCDF);
	      if (StatusBAD(status)) {
		FreeExportBuffers (exportHead);
		return FATALout;
	      }
	    }
	 }
       }
       DisplayPctComplete (PCT(hyperN,nHypers,1,4), rvMsg);
       if (AbortCDF(exportHead)) return SUCCESS;
       /***********************************************************************
       * Read from hyper input variables.
       ***********************************************************************/
       for (Item = hyperHead; Item != NULL; Item = Item->nextHyper) {
	  for (dimN = 0; dimN < numDims; dimN++) {
	     indicesI[dimN] = firstIndices[dimN] + hyper.dimIndices[dimN];
	  }
	  status = HYPERget (inID, Item->Var->varN, Item->Var->zVar, recNi,
			     1L, indicesI, hyper.dimCounts, Item->Var->buffer);
	  DisplayStatus (status, readingCDF);
	  if (StatusBAD(status)) {
	    FreeExportBuffers (exportHead);
	    return FATALin;
	  }
	  if (AbortCDF(exportHead)) return SUCCESS;
       }
       DisplayPctComplete (PCT(hyperN,nHypers,2,4), rvMsg);
       /***********************************************************************
       * Filter values in hyper buffers.
       ***********************************************************************/
       if (filteringHypers) FilterHypers (hyperHead, nValues);
       DisplayPctComplete (PCT(hyperN,nHypers,3,4), rvMsg);
       /***********************************************************************
       * Hyper/single write to hyper output variables (being output).
       ***********************************************************************/
       for (Item = hyperHead; Item != NULL; Item = Item->nextHyper) {
	  if (Item->output && recNi <= Item->Var->maxRec) {
	    if (!OutputHyperBuffer(outID,Item->Var->varNo,Item->Var->zVar,
				   outMajority,recNo,1L,hyper.dimIndices,
				   hyper.dimCounts,numDims,dimSizes,
				   Item->Var->buffer,nValues,FALSE,
				   Item->Var->nValueBytes,nRecordValues)) {
	      FreeExportBuffers (exportHead);
	      return FATALout;
	    }
	    if (AbortCDF(exportHead)) return SUCCESS;
	  }
       }
       DisplayPctComplete (PCT(hyperN,nHypers,4,4), rvMsg);
       /***********************************************************************
       * Increment to next hyper group.
       ***********************************************************************/
       IncrHyperParms (&hyper,&groups,numDims,ROWmajor(inMajority),&nValues);
     }
  }
  FreeExportBuffers (exportHead);
  return SUCCESS;
}

/******************************************************************************
* ToCDFdiffOrZero.
* Different dimensionalities or all with zero dimensions.
* Returns SUCCESS, FATALin, or FATALout;
******************************************************************************/

int ToCDFdiffOrZero (inID, outID, firstRec, lastRec, outMajority, exportHead)
CDFid inID;
CDFid outID;
long firstRec;
long lastRec;
long outMajority;
struct ItemStruct *exportHead;
{
  int scalarCount, hyperCount, scalarX; size_t *nValueBytes;
  long recNo, nHypers, hyperN, nValues, recX, firstX, lastX, thisCount;
  long recF, recL; Byte1 ***handles; CDFstatus status;
  Logical filteringScalars; static Logical first = TRUE;
  struct ItemStruct *Item, *prevItem, *scalarHead, *scalarTail, *hyperHead;
  struct ItemStruct *hyperTail; struct GroupStruct groups;
  struct HyperStruct hyper;
  static char rvMsg[] = "Reading/writing variable values...";
  static char keyDefsAbort[] = "Abort: ________\n\n";
  /****************************************************************************
  * Encode key definitions first time.
  ****************************************************************************/
  if (first) {
    char *p1 = keyDefsAbort;
    EncodeKeyDefinitions (1, &p1, ABORTkey_EXPORT);
    first = FALSE;
  }
  /****************************************************************************
  * Read/write first (and only) record for NRV variables.  Note that 
  * if an item is being output it must be a variable.
  ****************************************************************************/
  switch (OutputNRVvalues(inID,outID,exportHead,FALSE,NULL,NULL,outMajority)) {
    case SUCCESS: break;
    case FATALin: return FATALin;
    case FATALout: return FATALout;
  }
  /****************************************************************************
  * Remove NRV variables and hyper variables not being output from export list.
  ****************************************************************************/
  for (Item = exportHead, prevItem = NULL;
       Item != NULL; Item = Item->nextExport) {
    if (!Item->Var->recVary || (!Item->Var->scalar && !Item->output)) {
      if (prevItem == NULL)
	exportHead = Item->nextExport;
      else
	prevItem->nextExport = Item->nextExport;
    }
    else
      prevItem = Item;
  }
  /****************************************************************************
  * Count scalars/hypers and allocate buffers for scalars.
  ****************************************************************************/
  for (Item = exportHead, scalarCount = 0, hyperCount = 0;
       Item != NULL; Item = Item->nextExport) {
     if (Item->Var->scalar)
       scalarCount++;
     else
       hyperCount++;
  }
  if (scalarCount == 0) return SUCCESS;
  handles = (Byte1 ***) cdf_AllocateMemory ((size_t)scalarCount * sizeof(Byte1 **),
				       FatalError);
  nValueBytes = (size_t *) cdf_AllocateMemory ((size_t)scalarCount * sizeof(size_t),
					   FatalError);
  for (Item = exportHead, scalarX = 0, scalarHead = scalarTail = NULL,
       hyperHead = hyperTail = NULL, filteringScalars = FALSE;
       Item != NULL; Item = Item->nextExport) {
     if (Item->Var->scalar) {
       handles[scalarX] = &(Item->Var->buffer);
       nValueBytes[scalarX++] = Item->Var->nValueBytes;
       if (scalarHead == NULL)
	 scalarHead = scalarTail = Item;
       else
	 scalarTail = scalarTail->nextScalar = Item;
       Item->nextScalar = NULL;
       if (Item->filter) filteringScalars = TRUE;
     }
     else {
       if (hyperHead == NULL)
	 hyperHead = hyperTail = Item;
       else
	 hyperTail = hyperTail->nextHyper = Item;
       Item->nextHyper = NULL;
       Item->Var->buffer = NULL;
     }
  }
  /****************************************************************************
  * Preallocate variable records.
  ****************************************************************************/
  if (opt.preAllocate) {
    switch (PreAllocateRecords(inID,outID,scalarHead,
			       hyperHead,&firstRec,&lastRec)) {
      case SUCCESS:
	break;
      case FATALin:
	if (handles != NULL) cdf_FreeMemory (handles, FatalError);
	if (nValueBytes != NULL) cdf_FreeMemory (nValueBytes, FatalError);
	return FATALin;
      case FATALout:
	if (handles != NULL) cdf_FreeMemory (handles, FatalError);
	if (nValueBytes != NULL) cdf_FreeMemory (nValueBytes, FatalError);
	return FATALout;
    }
  }
  /****************************************************************************
  * Allocate buffers for scalar variables.  Note that if there were no scalar
  * variables the handles and value sizes would be NULL and one hyper would be
  * specified covering the entire range of input records.
  ****************************************************************************/
  DisplayPctComplete (NO_PCT, rvMsg);
  NEWkeyDEFS (EWkey, keyDefsAbort, batchMode)
  AllocateBuffers(lastRec - firstRec + 1, 0L, NULL, &groups, scalarCount, 0,
		  handles, nValueBytes, ROWmajor(inMajority), 5, FatalError);
  if (handles != NULL) cdf_FreeMemory (handles, FatalError);
  if (nValueBytes != NULL) cdf_FreeMemory (nValueBytes, FatalError);
  /****************************************************************************
  * For each hyper...
  ****************************************************************************/
  InitHyperParms (&hyper, &groups, 0L, &nHypers, &nValues);
  for (hyperN = 0, recNo = 0; hyperN < nHypers; hyperN++) {
     /*************************************************************************
     * Hyper read scalar input variables.
     *************************************************************************/
     for (Item = scalarHead; Item != NULL; Item = Item->nextScalar) {
	status = HYPERget (inID, Item->Var->varN, Item->Var->zVar,
			   firstRec + hyper.recNumber, hyper.recCount,
			   dimIndices_0, dimCounts_1, Item->Var->buffer);
	DisplayStatus (status, readingCDF);
	if (StatusBAD(status)) {
	  FreeExportBuffers (exportHead);
	  return FATALin;
	}
     }
     DisplayPctComplete (PCT(hyperN,nHypers,1,2), rvMsg);
     if (AbortCDF(exportHead)) return SUCCESS;
     /**********************************************************************
     * Until no more records...
     **********************************************************************/
     for (recX = 0;;) {
	/**********************************************************************
	* Determine first record to be output.
	**********************************************************************/
	firstX = FindFirstRecord (recX, scalarHead, filteringScalars,
			          hyper.recCount);
	if (firstX == NO_RECORD) break;
	/**********************************************************************
	* Determine last record to be output.
	**********************************************************************/
	lastX = FindLastRecord (firstX, scalarHead, filteringScalars,
			        hyper.recCount);
	thisCount = lastX - firstX + 1;
	recF = firstRec + hyper.recNumber + firstX;
	recL = firstRec + hyper.recNumber + lastX;    /* lastRec -> firstRec */
	/**********************************************************************
	* Hyper write to scalar output variables (being output).
	**********************************************************************/
	for (Item = scalarHead; Item != NULL; Item = Item->nextScalar) {
	   if (Item->output) {
	     if (recF <= Item->Var->maxRec) {
	       long count = MINIMUM(recL,Item->Var->maxRec) - recF + 1;
	       Byte1 *buffer = Item->Var->buffer +
			      (size_t) (Item->Var->nValueBytes * firstX);
	       status = HYPERput (outID, Item->Var->varNo, Item->Var->zVar,
				  recNo, count, dimIndices_0, dimCounts_1,
				  buffer);
	       DisplayStatus (status, writingCDF);
	       if (StatusBAD(status)) {
	         FreeExportBuffers (exportHead);
	         return FATALout;
	       }
	     }
	   }
	}
	if (AbortCDF(exportHead)) return SUCCESS;
	/**********************************************************************
	* Hyper read/filter/write each hyper variable being output.
	**********************************************************************/
	for (Item = hyperHead; Item != NULL; Item = Item->nextHyper) {
	   if (recF <= Item->Var->maxRec) {
	     long count = MINIMUM(recL,Item->Var->maxRec) - recF + 1;
	     struct GroupStruct groups1; Logical fullRecord1;
	     Byte1 *buffer1, **handles1 = &buffer1; struct HyperStruct hyper1;
	     long nHypers1, hyperN1, nValues1;
	     size_t nValueBytes1 = Item->Var->nValueBytes;
	     AllocateBuffers (count, Item->Var->numDims, Item->Var->dimSizes,
			      &groups1, 0, 1, &handles1, &nValueBytes1,
			      ROWmajor(inMajority), 5, FatalError);
	     fullRecord1 = HyperFullRecord (&groups1, Item->Var->numDims);
	     InitHyperParms (&hyper1, &groups1, Item->Var->numDims, &nHypers1,
			     &nValues1);
	     for (hyperN1 = 0; hyperN1 < nHypers1; hyperN1++) {
	        status = HYPERget (inID, Item->Var->varN, Item->Var->zVar,
				   recF + hyper1.recNumber, hyper1.recCount,
				   hyper1.dimIndices, hyper1.dimCounts,
				   buffer1);
	        DisplayStatus (status, readingCDF);
	        if (StatusBAD(status)) {
		  FreeExportBuffers (exportHead);
		  return FATALin;
	        }
	        if (Item->filter) FilterBuffer (Item, buffer1, nValues1);
	        if (!OutputHyperBuffer(outID,Item->Var->varNo,Item->Var->zVar,
				       outMajority,recNo + hyper1.recNumber,
				       hyper1.recCount,hyper1.dimIndices,
				       hyper1.dimCounts,Item->Var->numDims,
				       Item->Var->dimSizes,buffer1,nValues1,
				       fullRecord1,Item->Var->nValueBytes,
				       Item->Var->nRecordValues)) {
		  FreeExportBuffers (exportHead);
		  return FATALout;
	        }
	        IncrHyperParms (&hyper1, &groups1, Item->Var->numDims,
			        ROWmajor(inMajority), &nValues1);
	        if (AbortCDF(exportHead)) return SUCCESS;
	     }
	     cdf_FreeMemory (buffer1, FatalError);
	   }
	}
	/*******************************************************************
	* Increment to next...
	*******************************************************************/
	recNo += thisCount;
	recX = lastX + 1;
	if (recX == hyper.recCount) break;
     }
     DisplayPctComplete (PCT(hyperN,nHypers,2,2), rvMsg);
     /**********************************************************************
     * Increment to next hyper group.
     **********************************************************************/
     IncrHyperParms (&hyper, &groups, 0L, ROWmajor(inMajority), &nValues);
  }
  FreeExportBuffers (exportHead);
  return SUCCESS;
}

/******************************************************************************
* OutputNRVvalues.
* Returns SUCCESS, FATALin, or FATALout;
******************************************************************************/

Logical OutputNRVvalues (inID, outID, exportHead, same, dimSizes,
			 firstIndices, outMajority)
CDFid inID;
CDFid outID;
struct ItemStruct *exportHead;
Logical same;                   /* Same dimensionalities? */
long dimSizes[];                /* N/a if different dimensionalities. */
long firstIndices[];            /* N/a if different dimensionalities. */
long outMajority;
{
  long nrvCount, nrvAt, nHypers, hyperN, nRecordValues; size_t *nValueBytes;
  long phyDimSizes[CDF_MAX_DIMS], indicesI[CDF_MAX_DIMS], nValues;
  struct HyperStruct hyper; struct GroupStruct groups; struct ItemStruct *Item;
  Byte1 ***handles, *buffer; CDFstatus status; int dimN; Logical fullRecord;
  static char nrvMsg[] = "Reading/writing NRV variable values...";
  /****************************************************************************
  * Count number of NRV variables being output.
  ****************************************************************************/
  for (Item = exportHead, nrvCount=0; Item != NULL; Item = Item->nextExport) {
     if (NRVtoOUTPUT(Item) && Item->Var->maxRec == 0) nrvCount++;
  }
  DisplayPctComplete (NO_PCT, nrvMsg);
  /****************************************************************************
  * Output values for each NRV variable.
  ****************************************************************************/
  for (Item = exportHead, nrvAt = 0; Item != NULL; Item = Item->nextExport) {
     if (NRVtoOUTPUT(Item) && Item->Var->maxRec == 0) {
       /***********************************************************************
       * Allocate hyper buffer.
       ***********************************************************************/
       for (dimN = 0, nRecordValues = 1; dimN < Item->Var->numDims; dimN++) {
	  if (Item->Var->dimVarys[dimN])
	    phyDimSizes[dimN] = BOO(same,dimSizes[dimN],
					 Item->Var->dimSizes[dimN]);
	  else
	    phyDimSizes[dimN] = 1;
	  nRecordValues *= phyDimSizes[dimN];
       }
       handles = (Byte1 ***) cdf_AllocateMemory ((size_t)sizeof(Byte1 **), FatalError);
       nValueBytes = (size_t *) cdf_AllocateMemory ((size_t)sizeof(size_t), FatalError);
       handles[0] = &buffer;
       nValueBytes[0] = Item->Var->nValueBytes;
       AllocateBuffers (1L, Item->Var->numDims, phyDimSizes, &groups,
			0, 1, handles, nValueBytes, ROWmajor(inMajority),
			1, FatalError);
       cdf_FreeMemory (handles, FatalError);
       cdf_FreeMemory (nValueBytes, FatalError);
       /***********************************************************************
       * Perform each hyper read/filter/write(s).
       ***********************************************************************/
       fullRecord = HyperFullRecord (&groups, Item->Var->numDims);
       InitHyperParms (&hyper, &groups, Item->Var->numDims, &nHypers,
		       &nValues);
       for (hyperN = 0; hyperN < nHypers; hyperN++) {
	  /********************************************************************
	  * Set indices for input CDF.
	  ********************************************************************/
	  for (dimN = 0; dimN < Item->Var->numDims; dimN++) {
	     indicesI[dimN] = hyper.dimIndices[dimN] +
			      BOO(same,firstIndices[dimN],0);
	  }
	  /********************************************************************
	  * Read the buffer from the input CDF.
	  ********************************************************************/
	  status = HYPERget (inID, Item->Var->varN, Item->Var->zVar, 0L, 1L,
			     indicesI, hyper.dimCounts, buffer);
	  DisplayStatus (status, readingCDF);
	  if (StatusBAD(status)) {
	    cdf_FreeMemory (buffer, FatalError);
	    return FATALin;
	  }
	  /********************************************************************
	  * If this variable is being filtered, check each value in the
	  * buffer.  If a value fails the filter, replace it with the fill
	  * value (if one is specified) or the pad value (if a fill value
	  * is not specified).
	  ********************************************************************/
	  if (Item->filter) {
	    FilterBuffer (Item, buffer, nValues);
	  }
	  /********************************************************************
	  * Write the buffer to the output CDF.
	  ********************************************************************/
	  if (!OutputHyperBuffer(outID,Item->Var->varNo,Item->Var->zVar,
				 outMajority,0L,1L,hyper.dimIndices,
				 hyper.dimCounts,Item->Var->numDims,
				 phyDimSizes,buffer,nValues,fullRecord,
				 Item->Var->nValueBytes,nRecordValues)) {
	    cdf_FreeMemory (buffer, FatalError);
	    return FATALout;
	  }
	  /********************************************************************
	  * Increment to the next hyper parameters.
	  ********************************************************************/
	  IncrHyperParms (&hyper, &groups, Item->Var->numDims,
			  ROWmajor(inMajority), &nValues);
       }
       /***********************************************************************
       * Free hyper buffer.
       ***********************************************************************/
       cdf_FreeMemory (buffer, FatalError);
       /***********************************************************************
       * Update percent complete message.
       ***********************************************************************/
       DisplayPctComplete (PCT(nrvAt,nrvCount,1,1), nrvMsg);
       nrvAt++;
     }
  }
  return SUCCESS;
}

/******************************************************************************
* OutputHyperBuffer.
* Returns FALSE if an error occurred.
******************************************************************************/

Logical OutputHyperBuffer (outID, varNo, zVar, outMajority, recNumber,
			   recCount, dimIndices, dimCounts, numDims, dimSizes,
			   buffer, nValues, fullRecord, nValueBytes,
			   nRecordValues)
CDFid outID;
long varNo;
Logical zVar;
long outMajority;
long recNumber;
long recCount;
long dimIndices[];
long dimCounts[];
long numDims;
long dimSizes[];
Byte1 *buffer;
long nValues;
Logical fullRecord;
size_t nValueBytes;
long nRecordValues;
{
  CDFstatus status; long indicesO[CDF_MAX_DIMS], recX; Byte1 *value;
  /****************************************************************************
  * If majorities are the same use one hyper write.
  ****************************************************************************/
  if (outMajority == inMajority) {
    status = HYPERput (outID, varNo, zVar, recNumber, recCount, dimIndices,
		       dimCounts, buffer);
    DisplayStatus (status, writingCDF);
    if (StatusBAD(status)) return FALSE;
    return TRUE;
  }
  /****************************************************************************
  * If majority can be switched use one hyper write.
  ****************************************************************************/
  if (fullRecord) {
    if (SwitchMajority(buffer,ROWmajor(inMajority),numDims,
		       dimSizes,recCount,nValueBytes)) {
      status = HYPERput (outID, varNo, zVar, recNumber, recCount, dimIndices,
			 dimCounts, buffer);
      DisplayStatus (status, writingCDF);
      if (StatusBAD(status)) return FALSE;
      return TRUE;
    }
  }
  /****************************************************************************
  * Otherwise use single writes.
  ****************************************************************************/
  ARRAYtoARRAY (indicesO, dimIndices, numDims)
  for (recX = 0, value = buffer; recX < recCount; recX++) {
     long nValuesX = BOO(fullRecord,nRecordValues,nValues), valueN;
     for (valueN = 0; valueN < nValuesX; valueN++, value += nValueBytes) {
	status = SINGLEput (outID, varNo, zVar, recNumber + recX, indicesO,
			    value);
	DisplayStatus (status, writingCDF);
	if (StatusBAD(status)) return FALSE;
	if (ROWmajor(inMajority))
	  INCRindicesROW (numDims, dimSizes, indicesO);
	else
	  INCRindicesCOL (numDims, dimSizes, indicesO);
     }
  }
  return TRUE;
}

/******************************************************************************
* CopyAttributesANDgEntries.
* Returns SUCCESS, FATALin, or FATALout;
******************************************************************************/

int CopyAttributesANDgEntries (inID, outID, nAttrs)
CDFid inID;
CDFid outID;
long *nAttrs;
{
  long attrN, scope, aNum; CDFstatus status;
  char attrName[CDF_ATTR_NAME_LEN+1];
  AOSs1A (pctMsg,"Creating attributes/global entries...")
  /****************************************************************************
  * Inquire number of attributes in input CDF.
  ****************************************************************************/
  DisplayPctComplete (NO_PCT, pctMsg[0]);
  status = CDFlib (SELECT_, CDF_, inID,
		   GET_, CDF_NUMATTRS_, nAttrs,
		   NULL_);
  DisplayStatus (status, readingCDF);
  if (StatusBAD(status)) return FATALin;
  /****************************************************************************
  * For each attribute in the input CDF create a corresponding attribute in
  * the output CDF.
  ****************************************************************************/
  for (attrN = 0; attrN < *nAttrs; attrN++) {
     /*************************************************************************
     * Read attribute information from input CDF.
     *************************************************************************/
     status = CDFlib (SELECT_, CDF_, inID,
			       ATTR_, attrN,
		      GET_, ATTR_NAME_, attrName,
			    ATTR_SCOPE_, &scope,
		      NULL_);
     DisplayStatus (status, readingCDF);
     if (StatusBAD(status)) return FATALin;
     /*************************************************************************
     * Create attribute in output CDF.
     *************************************************************************/
     status = CDFlib (SELECT_, CDF_, outID,
		      CREATE_, ATTR_, attrName, scope, &aNum,
		      NULL_);
     DisplayStatus (status, writingCDF);
     if (StatusBAD(status)) return FATALout;
     /*************************************************************************
     * If global-scope, also copy entries from the input CDF to the output CDF.
     *************************************************************************/
     if (scope == GLOBAL_SCOPE) {
       long maxEntry, entryN;
       /***********************************************************************
       * Inquire maximum gEntry number.
       ***********************************************************************/
       status = CDFlib (SELECT_, CDF_, inID,
			GET_, ATTR_MAXgENTRY_, &maxEntry,
			NULL_);
       DisplayStatus (status, readingCDF);
       if (StatusBAD(status)) return FATALin;
       /***********************************************************************
       * Copy each entry.
       ***********************************************************************/
       for (entryN = 0; entryN <= maxEntry; entryN++) {
	  /********************************************************************
	  * Confirm that this gEntry exists.  If so, write the entry to the
	  * output CDF.
	  ********************************************************************/
	  status = CDFlib (SELECT_, CDF_, inID,
			   CONFIRM_, gENTRY_EXISTENCE_, entryN,
			   NULL_);
	  switch (status) {
	    case NO_SUCH_ENTRY:
	      break;
	    default: {
	      long dataType, numElems; void *buffer; size_t nBytes;
	      DisplayStatus (status, readingCDF);
	      if (StatusBAD(status)) return FATALin;
	      /****************************************************************
	      * Inquire the data type/number of elements from the input CDF.
	      ****************************************************************/
	      status = CDFlib (SELECT_, CDF_, inID,
					gENTRY_, entryN,
			       GET_, gENTRY_DATATYPE_, &dataType,
				     gENTRY_NUMELEMS_, &numElems,
			       NULL_);
	      DisplayStatus (status, readingCDF);
	      if (StatusBAD(status)) return FATALin;
	      /****************************************************************
	      * Allocate a buffer for the entry value.
	      ****************************************************************/
	      nBytes = (size_t) (CDFelemSize(dataType) * numElems);
	      buffer = cdf_AllocateMemory (nBytes, FatalError);
	      /****************************************************************
	      * Read the entry value from the input CDF.
	      ****************************************************************/
	      status = CDFlib (SELECT_, CDF_, inID,
			       GET_, gENTRY_DATA_, buffer,
			       NULL_);
	      DisplayStatus (status, readingCDF);
	      if (StatusBAD(status)) {
		cdf_FreeMemory (buffer, FatalError);
		return FATALin;
	      }
	      /****************************************************************
	      * Write the entry to the output CDF.
	      ****************************************************************/
	      status = CDFlib (SELECT_, CDF_, outID,
					gENTRY_, entryN,
			       PUT_, gENTRY_DATA_, dataType, numElems, buffer,
			       NULL_);
	      DisplayStatus (status, writingCDF);
	      if (StatusBAD(status)) {
		cdf_FreeMemory (buffer, FatalError);
		return FATALout;
	      }
	      /****************************************************************
	      * Free the buffer.
	      ****************************************************************/
	      cdf_FreeMemory (buffer, FatalError);
	      break;
	    }
	  }
       }
     }
     /*************************************************************************
     * Update percentage complete message.
     *************************************************************************/
     DisplayPctComplete (PCT(attrN,*nAttrs,1,1), pctMsg[0]);
  }
  return SUCCESS;
}

/******************************************************************************
* CopyVariablesANDrzEntries.
* Returns SUCCESS, FATALin, or FATALout;
******************************************************************************/

int CopyVariablesANDrzEntries (inID, outID, nAttrs, same, numDims, dimSizes,
			       exportHead)
CDFid inID;
CDFid outID;
long nAttrs;
Logical same;
long numDims;
long dimSizes[];
struct ItemStruct *exportHead;
{
  struct ItemStruct *Item; CDFstatus status; int nVars, atVar;
  AOSs1A (pctMsg,"Creating variables/entries...")
  /****************************************************************************
  * Count number of variables being output.
  ****************************************************************************/
  DisplayPctComplete (NO_PCT, pctMsg[0]);
  for (Item = exportHead, nVars = 0; Item != NULL; Item = Item->nextExport) {
     if (Item->output) nVars++;
  }
  /****************************************************************************
  * Scan list of exported items for variables being output.
  ****************************************************************************/
  for (Item = exportHead, atVar = 0; Item != NULL; Item = Item->nextExport) {
     if (Item->output) {
       long attrN;
       /***********************************************************************
       * Create output variable.
       ***********************************************************************/

       if (Item->Var->zVar)
	 status = CDFlib (SELECT_, CDF_, outID,
			  CREATE_, zVAR_, Item->Var->name,
					  Item->Var->dataType,
					  Item->Var->numElems,
					  BOO(same,numDims,Item->Var->numDims),
					  BOO(same,dimSizes,
					      Item->Var->dimSizes),
					  Item->Var->recVary,
					  Item->Var->dimVarys,
					  &(Item->Var->varNo),
			  NULL_);
       else
	 status = CDFlib (SELECT_, CDF_, outID,
			  CREATE_, rVAR_, Item->Var->name, Item->Var->dataType,
					  Item->Var->numElems,
					  Item->Var->recVary,
					  Item->Var->dimVarys,
					  &(Item->Var->varNo),
			  NULL_);
       DisplayStatus (status, writingCDF);
       if (StatusBAD(status)) return FATALout;
       /***********************************************************************
       * If a pad value exists for the input variable, write it to the output
       * variable.  Note that `Item->Var->pad' will always be non-NULL.  It
       * points to either the variable's default (based on data type) or
       * explicit pad value.
       ***********************************************************************/
       status = CDFlib (SELECT_, CDF_, inID,
				 BOO(Item->Var->zVar,
				     zVAR_,rVAR_), Item->Var->varN,
			CONFIRM_, BOO(Item->Var->zVar,
				      zVAR_PADVALUE_,rVAR_PADVALUE_),
			NULL_);
       switch (status) {
	 case NO_PADVALUE_SPECIFIED:
	   break;
	 default:
	   DisplayStatus (status, readingCDF);
	   if (StatusBAD(status)) return FATALin;
	   status = CDFlib (SELECT_, CDF_, outID,
			    PUT_, BOO(Item->Var->zVar,
				      zVAR_PADVALUE_,
				      rVAR_PADVALUE_), Item->Var->pad,
			    NULL_);
	   DisplayStatus (status, writingCDF);
	   if (StatusBAD(status)) return FATALout;
	   break;
       }
       /***********************************************************************
       * Specify sparseness, compression, and blocking factor for output
       * variable.
       ***********************************************************************/
       status = CDFlib (SELECT_, CDF_, outID,
			PUT_, BOO(Item->Var->zVar,
				  zVAR_SPARSERECORDS_,
				  rVAR_SPARSERECORDS_),
						Item->Var->sRecordsType,
			      BOO(Item->Var->zVar,
				  zVAR_SPARSEARRAYS_,
				  rVAR_SPARSEARRAYS_), Item->Var->sArraysType,
						       Item->Var->sArraysParms,
			      BOO(Item->Var->zVar,
				  zVAR_COMPRESSION_,
				  rVAR_COMPRESSION_), Item->Var->cType,
						      Item->Var->cParms,
			      BOO(Item->Var->zVar,
				  zVAR_BLOCKINGFACTOR_,
				  rVAR_BLOCKINGFACTOR_), Item->Var->blocking,
			NULL_);
       DisplayStatus (status, writingCDF);
       if (StatusBAD(status)) return FATALout;
       /***********************************************************************
       * Specify reserve percentage for output variable.
       ***********************************************************************/
       if (Item->Var->reserve != NA_RESERVE) {
	 status = CDFlib (SELECT_, BOO(Item->Var->zVar,
				       zVAR_RESERVEPERCENT_,
				       rVAR_RESERVEPERCENT_),
							Item->Var->reserve,
			  NULL_);
	 DisplayStatus (status, writingCDF);
	 if (StatusBAD(status)) return FATALout;
       }
       /***********************************************************************
       * Copy corresponding r/zEntries from the input CDF to the output CDF.
       ***********************************************************************/
       for (attrN = 0; attrN < nAttrs; attrN++) {
	  long scope;
	  /********************************************************************
	  * Determine attribute name and scope.
	  ********************************************************************/
	  status = CDFlib (SELECT_, CDF_, inID,
				    ATTR_, attrN,
			   GET_, ATTR_SCOPE_, &scope,
			   NULL_);
	  DisplayStatus (status, readingCDF);
	  if (StatusBAD(status)) return FATALin;
	  /********************************************************************
	  * If variable-scope, copy r/zEntry if it exists.
	  ********************************************************************/
	  if (scope == VARIABLE_SCOPE) {
	    status = CDFlib (SELECT_, CDF_, inID,
			     CONFIRM_, BOO(Item->Var->zVar,
					   zENTRY_EXISTENCE_,
					   rENTRY_EXISTENCE_), Item->Var->varN,
			     NULL_);
	    switch (status) {
	      case NO_SUCH_ENTRY:
		break;
	      default: {
		long dataType, numElems; void *buffer; size_t nBytes;
		DisplayStatus (status, readingCDF);
		if (StatusBAD(status)) return FATALin;
		/**************************************************************
		* Inquire the data type/number of elements from the input CDF.
		**************************************************************/
		status = CDFlib (SELECT_, CDF_, inID,
					  BOO(Item->Var->zVar,
					      zENTRY_,rENTRY_),Item->Var->varN,
				 GET_, BOO(Item->Var->zVar,
					   zENTRY_DATATYPE_,
					   rENTRY_DATATYPE_), &dataType,
				       BOO(Item->Var->zVar,
					   zENTRY_NUMELEMS_,
					   rENTRY_NUMELEMS_), &numElems,
				 NULL_);
		DisplayStatus (status, readingCDF);
		if (StatusBAD(status)) return FATALin;
		/**************************************************************
		* Allocate a buffer for the entry value.
		**************************************************************/
		nBytes = (size_t) (CDFelemSize(dataType) * numElems);
		buffer = cdf_AllocateMemory (nBytes, FatalError);
		/**************************************************************
		* Read the entry value from the input CDF.
		**************************************************************/
		status = CDFlib (SELECT_, CDF_, inID,
				 GET_, BOO(Item->Var->zVar,
					   zENTRY_DATA_,rENTRY_DATA_), buffer,
				 NULL_);
		DisplayStatus (status, readingCDF);
		if (StatusBAD(status)) {
		  cdf_FreeMemory (buffer, FatalError);
		  return FATALin;
		}
		/**************************************************************
		* Write the entry to the output CDF.
		**************************************************************/
		status = CDFlib (SELECT_, CDF_, outID,
					  ATTR_, attrN,
					  BOO(Item->Var->zVar,
					      zENTRY_,
					      rENTRY_), Item->Var->varNo,
				 PUT_, BOO(Item->Var->zVar,
					   zENTRY_DATA_,
					   rENTRY_DATA_), dataType, numElems,
							  buffer,
				 NULL_);
		DisplayStatus (status, writingCDF);
		if (StatusBAD(status)) {
		  cdf_FreeMemory (buffer, FatalError);
		  return FATALout;
		}
		/**************************************************************
		* Free the buffer.
		**************************************************************/
		cdf_FreeMemory (buffer, FatalError);
		break;
	      }
	    }
	  }
       }
       /***********************************************************************
       * Update percentage complete message.
       ***********************************************************************/
       DisplayPctComplete (PCT(atVar,nVars,1,1), pctMsg[0]);
       atVar++;
     }
  }
  return SUCCESS;
}

/******************************************************************************
* FirstLastRecord.
* Returns: FATAL if a fatal error occurred,
*	   FAILED if the output/listing should not continue, or
*	   PASSED if the output/listing should continue.
* The existence of at least one record in the CDF is assumed.
******************************************************************************/

int FirstLastRecord (firstRec, lastRec, toCDF, exportHead)
long *firstRec;
long *lastRec;
Logical toCDF;
struct ItemStruct *exportHead;
{
  CDFstatus status; long rMaxRec, zMaxRec; struct ItemStruct *Item;
  static char noRecordsMsg[] = "No records in valid range.";
  /****************************************************************************
  * Inquire maximum r/zRecord.
  ****************************************************************************/
  status = CDFlib (GET_, rVARs_MAXREC_, &rMaxRec,
			 zVARs_MAXREC_, &zMaxRec,
		   NULL_);
  DisplayStatus (status, readingCDF);
  if (StatusBAD(status)) return FATAL;
  /****************************************************************************
  * Set first and last records based on number of records in CDF.
  ****************************************************************************/
  *firstRec = 0;
  *lastRec = MAXIMUM(rMaxRec,zMaxRec);
  /****************************************************************************
  * If filters are disabled or filtered lines are to be shown (and output is
  * not to a CDF), return now.
  ****************************************************************************/
  if (!opt.overallFilter) return PASSED;
  if (opt.showFiltered && !toCDF) return PASSED;
  /****************************************************************************
  * Scan list of exported items.
  ****************************************************************************/
  for (Item = exportHead; Item != NULL; Item = Item->nextExport) {
     switch (Item->type) {
       case RECORDt:
	 if (Item->filter) {
	   if (Item->Record->min != NOminMax) {
	     /*****************************************************************
	     * Check if last record passes minimum filter value.
	     *****************************************************************/
	     if (!RecordPassesMin(Item,*lastRec)) {
	       DisplayMessage (noRecordsMsg, BEEPWAIT1);
	       return FAILED;
	     }
	     /*****************************************************************
	     * Reset first record based on minimum filter value.
	     *****************************************************************/
	     *firstRec = MaxLong(*firstRec,BOO(Item->inclusive,
					       Item->Record->min,
					       Item->Record->min + 1));
	   }
	   if (Item->Record->max != NOminMax) {
	     /*****************************************************************
	     * Check if first record passes maximum filter value.
	     *****************************************************************/
	     if (!RecordPassesMax(Item,*firstRec)) {
	       DisplayMessage (noRecordsMsg, BEEPWAIT1);
	       return FAILED;
	     }
	     /*****************************************************************
	     * Reset last record based on maximum filter value.
	     *****************************************************************/
	     *lastRec = MinLong(*lastRec,BOO(Item->inclusive,
					     Item->Record->max,
					     Item->Record->max - 1));
	   }
	   Item->filter = FALSE;
	 }
	 break;
       case VARIABLEt:
	 if (Item->filter) {
	   if (Item->Var->scalar) {
	     if (Item->Var->monotonic == UNKNOWNmono) {
	       if (!SetVarMonotonicity(Item->Var)) return FATAL;
	     }
	     switch (Item->Var->monotonic) {
	       case INCREASEmono:
		 /*************************************************************
		 * Check if a minimum filter value exists.
		 *************************************************************/
		 if (Item->Var->min != NULL) {
		   /***********************************************************
		   * Check if scalar value at last record passes minimum filter
		   * value.
		   ***********************************************************/
		   if (!ReadScalarValue(Item->Var,*lastRec)) return FATAL;
		   if (!VarPassesMin(Item,Item->Var->value)) {
		     DisplayMessage (noRecordsMsg, BEEPWAIT1);
		     return FAILED;
		   }
		   /***********************************************************
		   * Increment first record until it either reaches the last
		   * record or passes the minimum filter value.
		   ***********************************************************/
		   while (*firstRec < *lastRec) {
		     if (!ReadScalarValue(Item->Var,*firstRec)) return FATAL;
		     if (VarPassesMin(Item,Item->Var->value)) break;
		     (*firstRec)++;
		   }
		 }
		 /*************************************************************
		 * Check if a maximum filter value exists.
		 *************************************************************/
		 if (Item->Var->max != NULL) {
		   /***********************************************************
		   * Check if scalar value at first record passes maximum
		   * filter value.
		   ***********************************************************/
		   if (!ReadScalarValue(Item->Var,*firstRec)) return FATAL;
		   if (!VarPassesMax(Item,Item->Var->value)) {
		     DisplayMessage (noRecordsMsg, BEEPWAIT1);
		     return FAILED;
		   }
		   /***********************************************************
		   * Decrement last record until it either reaches the first
		   * record or passes the maximum filter value.
		   ***********************************************************/
		   while (*lastRec > *firstRec) {
		     if (!ReadScalarValue(Item->Var,*lastRec)) return FATAL;
		     if (VarPassesMax(Item,Item->Var->value)) break;
		     (*lastRec)--;
		   }
		 }
		 Item->filter = FALSE;
		 break;
	       case DECREASEmono:
		 if (Item->Var->min != NULL) {
		   /***********************************************************
		   * Check if scalar value at first record passes minimum
		   * filter value.
		   ***********************************************************/
		   if (!ReadScalarValue(Item->Var,*firstRec)) return FATAL;
		   if (!VarPassesMin(Item,Item->Var->value)) {
		     DisplayMessage (noRecordsMsg, BEEPWAIT1);
		     return FAILED;
		   }
		   /***********************************************************
		   * Decrement last record until it either reaches the first
		   * record or passes the minimum filter value.
		   ***********************************************************/
		   while (*lastRec > *firstRec) {
		     if (!ReadScalarValue(Item->Var,*lastRec)) return FATAL;
		     if (VarPassesMin(Item,Item->Var->value)) break;
		     (*lastRec)--;
		   }
		 }
		 /*************************************************************
		 * Check if a maximum filter value exists.
		 *************************************************************/
		 if (Item->Var->max != NULL) {
		   /***********************************************************
		   * Check if scalar value at last record passes maximum filter
		   * value.
		   ***********************************************************/
		   if (!ReadScalarValue(Item->Var,*lastRec)) return FATAL;
		   if (!VarPassesMax(Item,Item->Var->value)) {
		     DisplayMessage (noRecordsMsg, BEEPWAIT1);
		     return FAILED;
		   }
		   /***********************************************************
		   * Increment first record until it either reaches the last
		   * record or passes the maximum filter value.
		   ***********************************************************/
		   while (*firstRec < *lastRec) {
		     if (!ReadScalarValue(Item->Var,*firstRec)) return FATAL;
		     if (VarPassesMax(Item,Item->Var->value)) break;
		     (*firstRec)++;
		   }
		 }
		 Item->filter = FALSE;
		 break;
	     }
	   }
	 }
	 break;
     }
  }
  return PASSED;
}

/******************************************************************************
* FirstLastIndices.
* Returns: FATAL if a fatal error occurred,
*	   FAILED if the output/listing should not continue, or
*	   PASSED if the output/listing should continue.
* The existence of at least one record in the CDF is assumed.
******************************************************************************/

int FirstLastIndices (numDims, dimSizes, firstIndices, lastIndices, nValues,
		      toCDF, exportHead)
long numDims;
long dimSizes[];
long firstIndices[];
long lastIndices[];
long *nValues;          /* NULL if not requested. */
Logical toCDF;
struct ItemStruct *exportHead;
{
  int dimN; struct ItemStruct *Item;
  static char noIndicesMsg[] = "No indices in valid range.";
  /****************************************************************************
  * Set first and last indices based on dimensionality.
  ****************************************************************************/
  for (dimN = 0; dimN < numDims; dimN++) {
     firstIndices[dimN] = 0;
     lastIndices[dimN] = dimSizes[dimN] - 1;
  }
  /****************************************************************************
  * If filters are disabled or filtered lines are to be shown (and output is
  * not to a CDF), return now.
  ****************************************************************************/
  if (!opt.overallFilter) return PASSED;
  if (opt.showFiltered && !toCDF) return PASSED;
  /****************************************************************************
  * Scan list of exported items.
  ****************************************************************************/
  for (Item = exportHead; Item != NULL; Item = Item->nextExport) {
     switch (Item->type) {
       case INDICESt: {
	 if (Item->filter) {
	   if (Item->Indices->minNumDims != NOminMax) {
	     /*****************************************************************
	     * Check if last indices pass minimum filter value.
	     *****************************************************************/
	     if (!IndicesPassMin(Item,lastIndices)) {
	       DisplayMessage (noIndicesMsg, BEEPWAIT1);
	       return FAILED;
	     }
	     /*****************************************************************
	     * Reset first indices based on minimum filter value.
	     *****************************************************************/
	     for (dimN = 0; dimN < numDims; dimN++) {
		firstIndices[dimN] = MaxLong(firstIndices[dimN],
				     BOO(Item->inclusive,
					 Item->Indices->minIndices[dimN],
					 Item->Indices->minIndices[dimN] + 1));
	     }
	   }
	   if (Item->Indices->maxNumDims != NOminMax) {
	     /*****************************************************************
	     * Check if first indices pass maximum filter value.
	     *****************************************************************/
	     if (!IndicesPassMax(Item,firstIndices)) {
	       DisplayMessage (noIndicesMsg, BEEPWAIT1);
	       return FAILED;
	     }
	     /*****************************************************************
	     * Reset last indices based on maximum filter value.
	     *****************************************************************/
	     for (dimN = 0; dimN < numDims; dimN++) {
		lastIndices[dimN] = MinLong(lastIndices[dimN],
				    BOO(Item->inclusive,
					Item->Indices->maxIndices[dimN],
					Item->Indices->maxIndices[dimN]-1));
	     }
	   }
	   Item->filter = FALSE;
	 }
	 break;
       }
       case VARIABLEt:
	 if (Item->filter) {
	   if (DimensionalVariable(Item->Var,&dimN)) {
	     if (Item->Var->monotonic == UNKNOWNmono) {
	       if (!SetVarMonotonicity(Item->Var)) return FATAL;
	     }
	     switch (Item->Var->monotonic) {
	       case INCREASEmono:
		 /*************************************************************
		 * Check if a minimum filter value exists.
		 *************************************************************/
		 if (Item->Var->min != NULL) {
		   /***********************************************************
		   * Check if value at last index passes minimum filter value.
		   ***********************************************************/
		   if (!ReadDimensionalValue(Item->Var,
					     lastIndices)) return FATAL;
		   if (!VarPassesMin(Item,Item->Var->value)) {
		     DisplayMessage (noIndicesMsg, BEEPWAIT1);
		     return FAILED;
		   }
		   /***********************************************************
		   * Increment first index until it either reaches the last
		   * index or passes the minimum filter value.
		   ***********************************************************/
		   while (firstIndices[dimN] < lastIndices[dimN]) {
		     if (!ReadDimensionalValue(Item->Var,
					       firstIndices)) return FATAL;
		     if (VarPassesMin(Item,Item->Var->value)) break;
		     firstIndices[dimN]++;
		   }
		 }
		 /*************************************************************
		 * Check if a maximum filter value exists.
		 *************************************************************/
		 if (Item->Var->max != NULL) {
		   /***********************************************************
		   * Check if value at first index passes maximum filter value.
		   ***********************************************************/
		   if (!ReadDimensionalValue(Item->Var,
					     firstIndices)) return FATAL;
		   if (!VarPassesMax(Item,Item->Var->value)) {
		     DisplayMessage (noIndicesMsg, BEEPWAIT1);
		     return FAILED;
		   }
		   /***********************************************************
		   * Decrement last index until it either reaches the first
		   * index or passes the maximum filter value.
		   ***********************************************************/
		   while (lastIndices[dimN] > firstIndices[dimN]) {
		     if (!ReadDimensionalValue(Item->Var,
					       lastIndices)) return FATAL;
		     if (VarPassesMax(Item,Item->Var->value)) break;
		     lastIndices[dimN]--;
		   }
		 }
		 Item->filter = FALSE;
		 break;
	       case DECREASEmono:
		 if (Item->Var->min != NULL) {
		   /***********************************************************
		   * Check if value at first index passes minimum filter value.
		   ***********************************************************/
		   if (!ReadDimensionalValue(Item->Var,
					     firstIndices)) return FATAL;
		   if (!VarPassesMin(Item,Item->Var->value)) {
		     DisplayMessage (noIndicesMsg, BEEPWAIT1);
		     return FAILED;
		   }
		   /***********************************************************
		   * Decrement last index until it either reaches the first
		   * index or passes the minimum filter value.
		   ***********************************************************/
		   while (lastIndices[dimN] > firstIndices[dimN]) {
		     if (!ReadDimensionalValue(Item->Var,
					       lastIndices)) return FATAL;
		     if (VarPassesMin(Item,Item->Var->value)) break;
		     lastIndices[dimN]--;
		   }
		 }
		 /*************************************************************
		 * Check if a maximum filter value exists.
		 *************************************************************/
		 if (Item->Var->max != NULL) {
		   /***********************************************************
		   * Check if value at last index passes maximum filter value.
		   ***********************************************************/
		   if (!ReadDimensionalValue(Item->Var,
					     lastIndices)) return FATAL;
		   if (!VarPassesMax(Item,Item->Var->value)) {
		     DisplayMessage (noIndicesMsg, BEEPWAIT1);
		     return FAILED;
		   }
		   /***********************************************************
		   * Increment first index until it either reaches the last
		   * index or passes the maximum filter value.
		   ***********************************************************/
		   while (firstIndices[dimN] < lastIndices[dimN]) {
		     if (!ReadDimensionalValue(Item->Var,
					       firstIndices)) return FATAL;
		     if (VarPassesMax(Item,Item->Var->value)) break;
		     firstIndices[dimN]++;
		   }
		 }
		 Item->filter = FALSE;
		 break;
	     }
	   }
	 }
	 break;
     }
  }
  /****************************************************************************
  * Recalculate number of values.
  ****************************************************************************/
  if (nValues != NULL) {
    for (*nValues = 1, dimN = 0; dimN < numDims; dimN++) {
       *nValues *= (lastIndices[dimN] - firstIndices[dimN] + 1);
    }
  }
  return PASSED;
}

/******************************************************************************
* ScalarVariable.
******************************************************************************/

Logical ScalarVariable (Var)
struct VarStruct *Var;
{
  switch (Var->numDims) {
    case 0:
      return TRUE;
    default: {
      int dimN;
      for (dimN = 0; dimN < Var->numDims; dimN++) {
	 if (Var->dimVarys[dimN] && Var->dimSizes[dimN] > 1) return FALSE;
      }
      return TRUE;
    }
  }
}

/******************************************************************************
* DimensionalVariable.
******************************************************************************/

Logical DimensionalVariable (Var, dimN)
struct VarStruct *Var;
int *dimN;
{
  int dimNt, count;
  if (Var->recVary) return FALSE;
  if (Var->numDims == 0) return FALSE;
  for (count = 0, dimNt = 0; dimNt < Var->numDims; dimNt++) {
     if (Var->dimVarys[dimNt]) {
       *dimN = dimNt;
       count++;
     }
  }
  return (count == 1);
}

/******************************************************************************
* OneDimensionVaries.
******************************************************************************/

Logical OneDimensionVaries (Var)
struct VarStruct *Var;
{
  int dimN, count = 0;
  if (Var->recVary) count++;
  for (dimN = 0; dimN < Var->numDims; dimN++) {
     if (Var->dimVarys[dimN]) count++;
  }
  return (count == 1);
}

/******************************************************************************
* ReadScalarValue.
******************************************************************************/

Logical ReadScalarValue (Var, recN)
struct VarStruct *Var;
long recN;
{
  static long indices[CDF_MAX_DIMS] = { 0,0,0,0,0,0,0,0,0,0 };
  CDFstatus status;
  status = CDFlib (SELECT_, BOO(Var->zVar,zVAR_,rVAR_), Var->varN,
			    BOO(Var->zVar,zVAR_RECNUMBER_,
					  rVARs_RECNUMBER_), recN,
			    BOO(Var->zVar,zVAR_DIMINDICES_,
					  rVARs_DIMINDICES_), indices,
		   GET_, BOO(Var->zVar,zVAR_DATA_,rVAR_DATA_), Var->value,
		   NULL_);
  DisplayStatus (status, readingCDF);
  if (StatusBAD(status)) return FALSE;
  return TRUE;
}

/******************************************************************************
* ReadDimensionalValue.
******************************************************************************/

Logical ReadDimensionalValue (Var, indices)
struct VarStruct *Var;
long indices[];
{
  CDFstatus status;
  status = CDFlib (SELECT_, BOO(Var->zVar,zVAR_,rVAR_), Var->varN,
			    BOO(Var->zVar,zVAR_RECNUMBER_,
					  rVARs_RECNUMBER_), 0L,
			    BOO(Var->zVar,zVAR_DIMINDICES_,
					  rVARs_DIMINDICES_), indices,
		   GET_, BOO(Var->zVar,zVAR_DATA_,rVAR_DATA_), Var->value,
		   NULL_);
  DisplayStatus (status, readingCDF);
  if (StatusBAD(status)) return FALSE;
  return TRUE;
}

/******************************************************************************
* ValidFormat.
******************************************************************************/

Logical ValidFormat (format)
char *format;
{
  if (NULstring(format)) return FALSE;
  return TRUE;
}

/******************************************************************************
* SameDimensionalities.
******************************************************************************/

Logical SameDimensionalities (numDims, dimSizes, exportHead)
long *numDims;
long dimSizes[];
struct ItemStruct *exportHead;
{
  struct ItemStruct *Item; Logical first = TRUE; int n;
  for (Item = exportHead; Item != NULL; Item = Item->nextExport) {
     if (Item->type == VARIABLEt && (Item->output || Item->filter)) {
       if (first) {
	 *numDims = Item->Var->numDims;
	 for (n = 0; n < *numDims; n++) dimSizes[n] = Item->Var->dimSizes[n];
	 first = FALSE;
       }
       else {
	 if (Item->Var->numDims != *numDims) return FALSE;
	 for (n = 0; n < *numDims; n++) {
	    if (Item->Var->dimSizes[n] != dimSizes[n]) return FALSE;
	 }
       }
     }
  }
  return TRUE;
}

/******************************************************************************
* ValidateRecordIndices.
******************************************************************************/

void ValidateRecordIndices (type, same, numDims, exportHead)
int type;                       /* Output type. */
Logical same;                   /* Ignored if horizontal mode. */
long numDims;                   /* Ignored if horizontal mode. */
struct ItemStruct *exportHead;
{
  struct ItemStruct *Item; int dimN;
  static char recordMsgOUT[] = {
    "`Record' not allowed for output."
  };
  static char indicesMsgODD[] = {
    "`Indices' not allowed for output (different dimensionalities)."
  };
  static char indicesMsgFDD[] = {
    "`Indices' not allowed for filtering (different dimensionalities)."
  };
  static char indicesMsgMIN[] = {
    "Wrong number of dimensions for minimum `Indices' filter."
  };
  static char indicesMsgMAX[] = {
    "Wrong number of dimensions for maximum `Indices' filter."
  };
  static char indicesMsgGT[] = {
    "Minimum `Indices' filter index is greater than maximum index."
  };
  static char indicesMsgOHM[] = {
    "`Indices' not allowed for output (horizontal mode)."
  };
  static char indicesMsgFHM[] = {
    "`Indices' not allowed for filtering (horizontal mode)."
  };
  static char indicesMsgOZD[] = {
    "`Indices' not allowed for output (zero dimensions)."
  };
  static char indicesMsgFZD[] = {
    "`Indices' not allowed for filtering (zero dimensions)."
  };
  static char indicesMsgOUT[] = {
    "`Indices' not allowed for output."
  };
  /****************************************************************************
  * Search list of exported items for `Record'.
  ****************************************************************************/
  for (Item = exportHead; Item != NULL; Item = Item->nextExport) {
     if (Item->type == RECORDt) {
       if (Item->output) {
	 if (type == OUTPUTtoCDF || type == OUTPUTtoFILEcdaweb) {
/*
	   if (type == OUTPUTtoCDF)
	     DisplayMessage (recordMsgOUT, NOBEEPWAIT1);
*/
	   Item->output = FALSE;
         }
       }
       break;
     }
  }
  /****************************************************************************
  * Search list of exported items for `Indices'.
  ****************************************************************************/
  for (Item = exportHead; Item != NULL; Item = Item->nextExport) {
     if (Item->type == INDICESt) {
       switch (type) {
	case OUTPUTtoSCREENh:
	case OUTPUTtoFILEh:
	case OUTPUTtoFILEcdaweb:
	 /*********************************************************************
	 * Output not allowed in horizontal mode.
	 *********************************************************************/
	 if (Item->output) {
	   if (type != OUTPUTtoFILEcdaweb)
	     DisplayMessage (indicesMsgOHM, NOBEEPWAIT1);
	   Item->output = FALSE;
	 }
	 /*********************************************************************
	 * Filtering not allowed in horizontal mode.
	 *********************************************************************/
	 if (Item->filter) {
	   DisplayMessage (indicesMsgFHM, NOBEEPWAIT1);
	   Item->filter = FALSE;
	 }
	 break;
	case OUTPUTtoSCREENv:
	case OUTPUTtoFILEv:
	 if (same) {
	   if (Item->output) {
	     /*****************************************************************
	     * Output not allowed if zero dimensions.
	     *****************************************************************/
	     if (numDims == 0) {
	       DisplayMessage (indicesMsgOZD, NOBEEPWAIT1);
	       Item->output = FALSE;
	     }
	   }
	   if (Item->filter) {
	     Logical checkGT = TRUE;
	     /*****************************************************************
	     * Filtering not allowed if zero dimensions.
	     *****************************************************************/
	     if (numDims == 0) {
	       DisplayMessage (indicesMsgFZD, NOBEEPWAIT1);
	       Item->filter = FALSE;
	     }
	     /*****************************************************************
	     * Filtering not allowed if wrong number of indices (minimum).
	     *****************************************************************/
	     if (Item->Indices->minNumDims != NOminMax) {
	       if (Item->Indices->minNumDims != numDims) {
		 DisplayMessage (indicesMsgMIN, NOBEEPWAIT1);
		 Item->filter = FALSE;
		 checkGT = FALSE;
	       }
	     }
	     /*****************************************************************
	     * Filtering not allowed if wrong number of indices (maximum).
	     *****************************************************************/
	     if (Item->Indices->maxNumDims != NOminMax) {
	       if (Item->Indices->maxNumDims != numDims) {
		 DisplayMessage (indicesMsgMAX, NOBEEPWAIT1);
		 Item->filter = FALSE;
		 checkGT = FALSE;
	       }
	     }
	     /*****************************************************************
	     * Filtering not allowed if minimum greater than maximum.
	     *****************************************************************/
	     if (Item->Indices->minNumDims != NOminMax &&
		 Item->Indices->maxNumDims != NOminMax && checkGT) {
	       for (dimN = 0; dimN < numDims; dimN++) {
		  if (Item->Indices->minIndices[dimN] >
		      Item->Indices->maxIndices[dimN]) {
		    DisplayMessage (indicesMsgGT, NOBEEPWAIT1);
		    Item->filter = FALSE;
		  }
	       }
	     }
	   }
	 }
	 else {
	   /*******************************************************************
	   * Output not allowed if different dimensionalities.
	   *******************************************************************/
	   if (Item->output) {
	     DisplayMessage (indicesMsgODD, NOBEEPWAIT1);
	     Item->output = FALSE;
	   }
	   /*******************************************************************
	   * Filtering not allowed if different dimensionalities.
	   *******************************************************************/
	   if (Item->filter) {
	     DisplayMessage (indicesMsgFDD, NOBEEPWAIT1);
	     Item->filter = FALSE;
	   }
	 }
	 break;
	case OUTPUTtoCDF:
	 /*********************************************************************
	 * Output not allowed.
	 *********************************************************************/
	 if (Item->output) {
/*	   DisplayMessage (indicesMsgOUT, NOBEEPWAIT1); */
	   Item->output = FALSE;
	 }
	 if (same) {
	   if (Item->filter) {
	     Logical checkGT = TRUE;
	     /*****************************************************************
	     * Filtering not allowed if zero dimensions.
	     *****************************************************************/
	     if (numDims == 0) {
	       DisplayMessage (indicesMsgFZD, NOBEEPWAIT1);
	       Item->filter = FALSE;
	     }
	     /*****************************************************************
	     * Filtering not allowed if wrong number of indices (minimum).
	     *****************************************************************/
	     if (Item->Indices->minNumDims != NOminMax) {
	       if (Item->Indices->minNumDims != numDims) {
		 DisplayMessage (indicesMsgMIN, NOBEEPWAIT1);
		 Item->filter = FALSE;
		 checkGT = FALSE;
	       }
	     }
	     /*****************************************************************
	     * Filtering not allowed if wrong number of indices (maximum).
	     *****************************************************************/
	     if (Item->Indices->maxNumDims != NOminMax) {
	       if (Item->Indices->maxNumDims != numDims) {
		 DisplayMessage (indicesMsgMAX, NOBEEPWAIT1);
		 Item->filter = FALSE;
		 checkGT = FALSE;
	       }
	     }
	     /*****************************************************************
	     * Filtering not allowed if minimum greater than maximum.
	     *****************************************************************/
	     if (Item->Indices->minNumDims != NOminMax &&
		 Item->Indices->maxNumDims != NOminMax && checkGT) {
	       for (dimN = 0; dimN < numDims; dimN++) {
		  if (Item->Indices->minIndices[dimN] >
		      Item->Indices->maxIndices[dimN]) {
		    DisplayMessage (indicesMsgGT, NOBEEPWAIT1);
		    Item->filter = FALSE;
		  }
	       }
	     }
	   }
	 }
	 else {
	   /*******************************************************************
	   * Filtering not allowed if different dimensionalities.
	   *******************************************************************/
	   if (Item->filter) {
	     DisplayMessage (indicesMsgFDD, NOBEEPWAIT1);
	     Item->filter = FALSE;
	   }
	 }
	 break;
       }
       break;
     }
  }
  return;
}

/******************************************************************************
* RecordPassesMin.
*   It is assumed that overall filtered and filtering for the `Record' item
* are both enabled.
******************************************************************************/

Logical RecordPassesMin (Item, recN)
struct ItemStruct *Item;
long recN;
{
  if (Item->Record->min == NOminMax) return TRUE;
  if (BOO(Item->inclusive,
	  recN >= Item->Record->min,
	  recN > Item->Record->min)) return TRUE;
  return FALSE;
}

/******************************************************************************
* RecordPassesMax.
*   It is assumed that overall filtered and filtering for the `Record' item
* are both enabled.
******************************************************************************/

Logical RecordPassesMax (Item, recN)
struct ItemStruct *Item;
long recN;
{
  if (Item->Record->max == NOminMax) return TRUE;
  if (BOO(Item->inclusive,
	  recN <= Item->Record->max,
	  recN < Item->Record->max)) return TRUE;
  return FALSE;
}

/******************************************************************************
* IndicesPassMin.
*   It is assumed that overall filtered and filtering for the `Indices' item
* are both enabled.
******************************************************************************/

Logical IndicesPassMin (Item, indices)
struct ItemStruct *Item;
long indices[];
{
  int dimN;
  if (Item->Indices->minNumDims == NOminMax) return TRUE;
  for (dimN = 0; dimN < Item->Indices->minNumDims; dimN++) {
     if (BOO(Item->inclusive,
	     indices[dimN] < Item->Indices->minIndices[dimN],
	     indices[dimN] <= Item->Indices->minIndices[dimN])) return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* IndicesPassMax.
*   It is assumed that overall filtered and filtering for the `Indices' item
* are both enabled.
******************************************************************************/

Logical IndicesPassMax (Item, indices)
struct ItemStruct *Item;
long indices[];
{
  int dimN;
  if (Item->Indices->maxNumDims == NOminMax) return TRUE;
  for (dimN = 0; dimN < Item->Indices->maxNumDims; dimN++) {
     if (BOO(Item->inclusive,
	     indices[dimN] > Item->Indices->maxIndices[dimN],
	     indices[dimN] >= Item->Indices->maxIndices[dimN])) return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* VarPassesMin.
*   It is assumed that overall filtered and filtering for this variable are
* both enabled.
******************************************************************************/

Logical VarPassesMin (Item, value)
struct ItemStruct *Item;
void *value;
{
  if (Item->Var->min == NULL) return TRUE;
  if (BOO(Item->inclusive,
	  GEx(value,Item->Var->min,Item->Var->dataType,Item->Var->numElems),
	  GTx(value,Item->Var->min,Item->Var->dataType,Item->Var->numElems))) {
    return TRUE;
  }
  if (USEFILL(Item->Var,opt)) {
    if (EQx(value,Item->Var->fill,Item->Var->dataType,Item->Var->numElems)) {
      return TRUE;
    }
  }
  return FALSE;
}

/******************************************************************************
* VarPassesMax.
*   It is assumed that overall filtered and filtering for this variable are
* both enabled.
******************************************************************************/

Logical VarPassesMax (Item, value)
struct ItemStruct *Item;
void *value;
{
  if (Item->Var->max == NULL) return TRUE;
  if (BOO(Item->inclusive,
	  LEx(value,Item->Var->max,Item->Var->dataType,Item->Var->numElems),
	  LTx(value,Item->Var->max,Item->Var->dataType,Item->Var->numElems))) {
    return TRUE;
  }
  if (USEFILL(Item->Var,opt)) {
    if (EQx(value,Item->Var->fill,Item->Var->dataType,Item->Var->numElems)) {
      return TRUE;
    }
  }
  return FALSE;
}

/******************************************************************************
* DisplayPctComplete.
* Does nothing if in batch mode.
******************************************************************************/

void DisplayPctComplete (pct, msg)
int pct;
char *msg;
{
  static int lastPct;
  if (BATCH(batchMode)) return;
  if (pct == NO_PCT) {
    lastPct = NO_PCT;
    DisplayMessage (msg, NOWAIT);
    return;
  }
  if (pct > lastPct) {
    char text[SCREEN_WIDTH+1], pctText[8+1]; int pad;
    strcpyX (text, msg, SCREEN_WIDTH - 2);
    snprintf (pctText, (size_t) sizeof(pctText), "%d%%", pct);
    pad = (SCREEN_WIDTH - 2) - strlen(text) - strlen(pctText);
    CatNcharacters (text, pad, ' ');
    strcatX (text, pctText, SCREEN_WIDTH - 2);
    DisplayMessage (text, NOWAIT);
    lastPct = pct;
  }
  return;
}

/******************************************************************************
* UpdateToScreen.
******************************************************************************/

void UpdateToScreen (EWscr, trailerMsg, at, total)
struct EditWindowStruct *EWscr;
char *trailerMsg;
long at;
long total;
{
  int pad; char pct[8+1];
  AOSs1 (trailer, BLANKs78)
  strcpyX (trailer[0], trailerMsg, SCREEN_WIDTH - 2);
  snprintf (pct, (size_t) sizeof(pct), "%ld%%", (100 * at) / total);
  pad = (SCREEN_WIDTH - 2) - strlen(trailer[0]) - strlen(pct);
  CatNcharacters (trailer[0], pad, ' ');
  strcatX (trailer[0], pct, SCREEN_WIDTH - 2);
  EWscr->tLines = trailer;
  EditWindow (UPDATEew, EWscr, LogicalTRUE);
  return;
}

/******************************************************************************
* StandardFormat.
******************************************************************************/

char *StandardFormat (dataType)
long dataType;
{
  switch (dataType) {
    case CDF_CHAR:
    case CDF_UCHAR: return NULL;
    case CDF_BYTE:
    case CDF_INT1: return "%4d";
    case CDF_UINT1: return "%3u";
    case CDF_INT2: return "%6d";
    case CDF_UINT2: return "%5u";
    case CDF_INT4: return Int32FORMATstandard;
    case CDF_INT8: return Int64FORMATstandard;
    case CDF_UINT4: return Int32uFORMATstandard;
    case CDF_REAL4:
    case CDF_FLOAT: return "%16.9e";
    case CDF_REAL8:
    case CDF_DOUBLE: return "%25.17e";
    case CDF_EPOCH:
      switch (opt.epochStyle) {
        case EPOCH0_STYLE:
        case EPOCH1_STYLE:
        case EPOCH2_STYLE:
        case EPOCH3_STYLE:
        case EPOCH4_STYLE: return NULL;
        case EPOCHf_STYLE: return StandardFormat(CDF_REAL8);
        case EPOCHx_STYLE: return EPOCHx_FORMAT_STANDARD;
      }
    case CDF_EPOCH16:	/* Based on EPOCH, it only shows 1 of 2 doubles.*/
      switch (opt.epochStyle) {
        case EPOCH0_STYLE:
        case EPOCH1_STYLE:
        case EPOCH2_STYLE:
        case EPOCH3_STYLE:
        case EPOCH4_STYLE: return NULL;
        case EPOCHf_STYLE: return StandardFormat(CDF_REAL8);
        case EPOCHx_STYLE: return EPOCHx_FORMAT_STANDARD;
      }
    case CDF_TIME_TT2000:
      switch (opt.epochStyle) {
        case EPOCH0_STYLE:
        case EPOCH1_STYLE:
        case EPOCH2_STYLE:
        case EPOCH3_STYLE:
        case EPOCH4_STYLE: return NULL;
      }
  }
  return NULL;
}

/******************************************************************************
* StandardWidth.
******************************************************************************/

int StandardWidth (dataType, numElems)
long dataType;
long numElems;
{
  switch (dataType) {
    case CDF_CHAR:
    case CDF_UCHAR: return (int) (1 + numElems + 1);
    case CDF_BYTE:
    case CDF_INT1:
    case CDF_UINT1:
    case CDF_INT2:
    case CDF_UINT2:
    case CDF_INT4:
    case CDF_INT8:
    case CDF_UINT4:
    case CDF_REAL4:
    case CDF_FLOAT:
    case CDF_REAL8:
    case CDF_DOUBLE: return FormatWidth(StandardFormat(dataType));
    case CDF_EPOCH:
      switch (opt.epochStyle) {
        case EPOCH0_STYLE: return EPOCH_STRING_LEN;
        case EPOCH1_STYLE: return EPOCH1_STRING_LEN;
        case EPOCH2_STYLE: return EPOCH2_STRING_LEN;
        case EPOCH3_STYLE: return EPOCH3_STRING_LEN;
        case EPOCH4_STYLE: return EPOCH4_STRING_LEN; /* ISO 8601 */
        case EPOCHf_STYLE: return FormatWidth(StandardFormat(CDF_REAL8));
        case EPOCHx_STYLE: return (int) strlen(EPOCHx_FORMAT_STANDARD);
      }
    case CDF_EPOCH16:	/* Based on EPOCH, it only shows 1 of 2 doubles.*/
      switch (opt.epochStyle) {
        case EPOCH0_STYLE: return EPOCH16_STRING_LEN;
        case EPOCH1_STYLE: return EPOCH16_1_STRING_LEN;
        case EPOCH2_STYLE: return EPOCH16_2_STRING_LEN;
        case EPOCH3_STYLE: return EPOCH16_3_STRING_LEN;
        case EPOCH4_STYLE: return EPOCH16_4_STRING_LEN; /* ISO 8601 */
        case EPOCHf_STYLE: return FormatWidth(StandardFormat(CDF_REAL8));
        case EPOCHx_STYLE: return (int) strlen(EPOCHx_FORMAT_STANDARD);
      }
    case CDF_TIME_TT2000:
      switch (opt.epochStyle) {
        case EPOCH0_STYLE:
        case EPOCH1_STYLE:
        case EPOCH2_STYLE:
        case EPOCH3_STYLE:
        case EPOCH4_STYLE: return TT2000_3_STRING_LEN; /* ISO 8601 */
      }
  }
  return 0;
}

/******************************************************************************
* AdjustFirstLastRecords.
******************************************************************************/

Logical AdjustFirstLastRecords (firstRec, lastRec)
long *firstRec;
long *lastRec;
{
  struct ItemStruct *Item, *itemTail = NULL;
  for (Item = itemHead; Item != NULL; Item = Item->nextItem) {
    if (Item->type != VARIABLEt) continue;
    if (Item->Var->dataType == CDF_EPOCH) {
      long ix, is = -1, ie = -1;
      if (!ReadScalarValue(Item->Var,*firstRec)) return FALSE;
      if (*(double *)(Item->Var->value) > epochEnd) {
        *firstRec = *lastRec = -1;
        return TRUE;
      }
      if (!ReadScalarValue(Item->Var,*lastRec)) return FALSE;
      if (*(double *)(Item->Var->value) < epochStart) {
        *firstRec = *lastRec = -1;
        return TRUE;
      }
      for (ix = *firstRec; ix < *lastRec; ++ix) {
        if (!ReadScalarValue(Item->Var,ix)) return FALSE;
        if (*(double *)(Item->Var->value) < epochStart) is = ix + 1;
        if (*(double *)(Item->Var->value) > epochEnd) {
          ie = ix - 1;
          break;
        }
      }
      if (is != -1) *firstRec = is;
      if (ie != -1) *lastRec = ie;
      return TRUE;
    } else if (Item->Var->dataType == CDF_EPOCH16) {
      long ix, is = -1, ie = -1;
      double time1, time2[2];
      if (!ReadScalarValue(Item->Var,*firstRec)) return FALSE;
      time2[0] = *((double *) Item->Var->value);
      time2[1] = *(((double *) Item->Var->value)+1);
      time1 = time2[0] * 1000.0 + time2[1] / pow(10.0, 9.0);
      if (time1 > epochEnd) {
        *firstRec = *lastRec = -1;
        return TRUE;
      }
      if (!ReadScalarValue(Item->Var,*lastRec)) return FALSE;
      time2[0] = *((double *) Item->Var->value);
      time2[1] = *(((double *) Item->Var->value)+1);
      time1 = time2[0] * 1000.0 + time2[1] / pow(10.0, 9.0);
      if (time1 < epochStart) {
        *firstRec = *lastRec = -1;
        return TRUE;
      }
      for (ix = *firstRec; ix < *lastRec; ++ix) {
        if (!ReadScalarValue(Item->Var,ix)) return FALSE;
        time2[0] = *((double *) Item->Var->value);
        time2[1] = *(((double *) Item->Var->value)+1);
        time1 = time2[0] * 1000.0 + time2[1] / pow(10.0, 9.0);
        if (time1 < epochStart) is = ix + 1;
        if (time1 > epochEnd) {
          ie = ix - 1;
          break;
        }
      }
      if (is != -1) *firstRec = is;
      if (ie != -1) *lastRec = ie;
      return TRUE;
    } else if (Item->Var->dataType == CDF_TIME_TT2000) {
      long ix, is = -1, ie = -1;
      if (!ReadScalarValue(Item->Var,*firstRec)) return FALSE;
      if (*(long long *)(Item->Var->value) > tt2000End) {
        *firstRec = *lastRec = -1;
        return TRUE;
      }
      if (!ReadScalarValue(Item->Var,*lastRec)) return FALSE;
      if (*(long long *)(Item->Var->value) < tt2000Start) {
        *firstRec = *lastRec = -1;
        return TRUE;
      }
      for (ix = *firstRec; ix < *lastRec; ++ix) {
        if (!ReadScalarValue(Item->Var,ix)) return FALSE;
        if (*(long long *)(Item->Var->value) < tt2000Start) is = ix + 1;
        if (*(long long *)(Item->Var->value) > tt2000End) {
          ie = ix - 1;
          break;
        }
      }
      if (is != -1) *firstRec = is;
      if (ie != -1) *lastRec = ie;
      return TRUE;
    }
  }
  return TRUE;
}

/******************************************************************************
* AdjustFirstsLastsRecords.
******************************************************************************/

Logical AdjustFirstsLastsRecords (firstRecs, lastRecs, index)
long firstRecs[];
long lastRecs[];
int index[];
{
  int ix, iy, is, ie;
  for (iy = 0; iy < numEpochs; ++iy) {
    if (index[iy] != 1) continue;
    if (epochTypes[iy] == 1) {
      is = ie = -1;
      if (*(double *)(epochValues[iy]) > epochEnd) {
        firstRecs[iy] = lastRecs[iy] = -1;
        continue;
      }
      if (*(((double *)(epochValues[iy]))+(int)epochMaxs[iy]) < epochStart) {
        firstRecs[iy] = lastRecs[iy] = -1;
        continue;
      }
      for (ix = 0; ix < (int) epochMaxs[iy]; ++ix) {
        if (is == -1 && *((double *)(epochValues[iy])+ix) >= epochStart) is = ix;
        if (ie == -1 && *((double *)(epochValues[iy])+ix) > epochEnd) {
          ie = ix - 1;
          break;
        }
      }
      if (is != -1) firstRecs[iy] = (long) is;
      if (ie != -1) lastRecs[iy] = (long) ie;
    } else if (epochTypes[iy] == 2) {
      double time1, time2[2];
      is = ie = -1;
      time2[0] = *(double *) (epochValues[iy]);
      time2[1] = *((double *) (epochValues[iy])+1);
      time1 = time2[0] * 1000.0 + time2[1] / pow(10.0, 9.0);
      if (time1 > epochEnd) {
        firstRecs[iy] = lastRecs[iy] = -1;
        continue;
      }
      time2[0] = *((double *)(epochValues[iy])+(int)2*epochMaxs[iy]);
      time2[1] = *((double *)(epochValues[iy])+(int)2*epochMaxs[iy]+1);
      time1 = time2[0] * 1000.0 + time2[1] / pow(10.0, 9.0);
      if (time1 < epochStart) {
        firstRecs[iy] = lastRecs[iy] = -1;
        continue;
      }   
      for (ix = 0; ix < (int) epochMaxs[iy]; ++ix) {
        time2[0] = *((double *)(epochValues[iy])+2*ix);
        time2[1] = *((double *)(epochValues[iy])+2*ix+1);
        time1 = time2[0] * 1000.0 + time2[1] / pow(10.0, 9.0);
        if (is == -1 && time1 >= epochStart) is = ix;
        if (ie == -1 && time1 > epochEnd) {
          ie = ix - 1;
          break;
        }
      }
      if (is != -1) firstRecs[iy] = (long) is;
      if (ie != -1) lastRecs[iy] = (long) ie;
    } else { 
      is = ie = -1;
      if (*(long long *)(epochValues[iy]) > tt2000End) {
        firstRecs[iy] = lastRecs[iy] = -1;
        continue;
      }
      if (*(((long long *)(epochValues[iy]))+(int)epochMaxs[iy]) < tt2000Start) {
        firstRecs[iy] = lastRecs[iy] = -1;
        continue;
      }
      for (ix = 0; ix < (int) epochMaxs[iy]; ++ix) {
        if (is == -1 && *((long long *)(epochValues[iy])+ix) >= tt2000Start) is = ix;
        if (ie == -1 && *((long long *)(epochValues[iy])+ix) > tt2000End) {
          ie = ix - 1;
          break;
        }
      }
      if (is != -1) firstRecs[iy] = (long) is;
      if (ie != -1) lastRecs[iy] = (long) ie;
    }
  }
  return TRUE;
}

/******************************************************************************
* FreeEpochs.
******************************************************************************/

void FreeEpochs ()
{
  int i;
  if (numEpochs > 0) {
    if (epochIndx != NULL) free (epochIndx);
    if (epochMaxs != NULL) free (epochMaxs);
    if (firstRecs != NULL) free (firstRecs);
    if (lastRecs != NULL) free (lastRecs);
    for (i = 0; i < numEpochs; ++i)
      if (epochValues[i] != NULL) free (epochValues[i]);
    if (epochValues != NULL) free (epochValues);
  }
  if (numEpochGroups > 1) {
    if (qaFlags != NULL) free (qaFlags);
  }
}

