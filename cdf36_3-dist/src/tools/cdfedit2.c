/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                    CDFedit, part 2 (variables).
*
*  Version 1.3a, 16-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  24-Jan-94, J Love     Original version.
*   V1.0a  8-Feb-94, J Love     DEC Alpha/OpenVMS port.
*   V1.1  15-Dec-94, J Love     CDF V2.5.
*   V1.1a  9-Jan-95, J Love     Window positioning.
*   V1.1b 23-Jan-95, J Love     IRIX 6.x (64-bit).
*   V1.1c 28-Feb-95, J Love     Pass `char' as `int'.
*   V1.2  11-Apr-95, J Love     POSIX.
*   V1.2a 11-May-95, J Love     EPOCH styles.
*   V1.2b  9-Jun-95, J Love     catchrX.
*   V1.2c  6-Sep-95, J Love     CDFexport-related changes.  FSI key
*                               definitions.
*   V1.3  27-Aug-96, J Love     CDF V2.6.
*   V1.3a 16-Nov-97, J Love	Windows NT/Visual C++.
*   V1.3b 02-Jun-98, M Liu      Minor correction at EditVarValues.
*   V3.3  10-Apr-09, M Liu      Filled blank(s) for default pad value of
*                               CDF_CHAR type.
*   V3.3  04-Apr-11, M Liu      Modified to handle TT2000 epoch style string.
*   V3.5  27-May-14, M Liu      Broke "Delete records" into two options.
*
******************************************************************************/

#include "cdfedit.h"

/******************************************************************************
* Local function prototypes.
******************************************************************************/

static Logical ModPadValue PROTOARGs((
  Logical zVar, long varN, Logical useFormat, Logical *changed
));
static Logical RegularVar PROTOARGs((
  Logical zVar, long varN
));

/******************************************************************************
* EditVars.  This function handles both rVariables and zVariables.
******************************************************************************/

Logical EditVars (Z, CDFname, useFormat)
Logical Z;
char *CDFname;
Logical useFormat;
{
   CDFstatus status;
   long varN;
   AOSs2 (header, BLANKs78, BLANKs78)
   AOSs1 (trailerBrowse, "Select: ________   Exit: ________   Help: ________")
   AOSs2A (trailerEdit,
	   "Select: ________   Create variable: ________   Help: ________",
	   "Exit:   ________   Delete variable: ________")
   static int exitCharsBrowse[] = {
     ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL
   };
   static int exitCharsEdit[] = {
     ENTERkey_FSI, EXITkey_FSI, CREATEVARkey_EDIT, HELPkey_FSI,
     DELETEVARkey_EDIT, NUL
   };
   static char label[6+DU_MAX_NAME_LEN+13+1];
   static struct ItemWindowStruct IW = {
      0, 0, 80, label, 2, header, 0, NULL, 0, NULL, NULL, NULL, 0, 0, NULL,
      NULL, REFRESHkey_FSI, TRUE, NSkey_FSI, PSkey_FSI
   };
   int varJump = BOO(Z,7,6);
   static Logical first = TRUE;
   /***************************************************************************
   * First time...
   ***************************************************************************/
   if (first) {
       EncodeKeyDefinitions (1, trailerBrowse, ENTERkey_FSI, EXITkey_FSI,
			     HELPkey_FSI);
       EncodeKeyDefinitions (2, trailerEdit, ENTERkey_FSI, CREATEVARkey_EDIT,
			     HELPkey_FSI, EXITkey_FSI, DELETEVARkey_EDIT);
     first = FALSE;
   }
     if (browseOnly) {
       IW.NiRows = 14;
       IW.NtLines = 1;
       IW.tLines = trailerBrowse;
       IW.exitChars = exitCharsBrowse;
     }
     else {
       IW.NiRows = 13;
       IW.NtLines = 2;
       IW.tLines = trailerEdit;
       IW.exitChars = exitCharsEdit;
     }
   /***************************************************************************
   * Build variable lines.
   ***************************************************************************/
   if (!BuildVarMenu(Z,CDFname,&IW)) return FALSE;
   /***************************************************************************
   * Display window.
   ***************************************************************************/
   ItemWindow (NEWiw, &IW, 0);
   /***************************************************************************
   * Read/process keystrokes until request to exit menu.
   ***************************************************************************/
   for (;;) {
     ItemWindow (READiw, &IW);
     switch (IW.key) {
       /***********************************************************************
       * Item selected.
       ***********************************************************************/
       case ENTERkey_FSI: {
	 int fieldN;
	 /*********************************************************************
	 * Check if any variables exist.
	 *********************************************************************/
	 if (IW.nItems == 0) {
	   ProblemWindow (BOO(Z,"Nothing selected (no zVariables exist).",
			        "Nothing selected (no rVariables exist)."),
			  FALSE);
	   break;
	 }
	 /*********************************************************************
	 * Calculate variable number and field number.
	 *********************************************************************/
	 fieldN = IW.itemN % varJump;
	 varN = IW.itemN / varJump;
	 /*********************************************************************
	 * Modify/show variable name.
	 *********************************************************************/
	 if (fieldN == 0) {
	   char varName[CDF_VAR_NAME_LEN256+1];
	   status = CDFlib (SELECT_, VAR(Z), varN,
			    GET_, VAR_NAME(Z), varName,
			    NULL_);
	   if (!ReportStatus(status,FALSE)) {
	     ItemWindow (DELETEiw, &IW);
	     return FALSE;
	   }
	   if (browseOnly)
	     ShowFullName (varName, FALSE);
	   else {
	     static char *header[] = {
	       "Enter new name (with delimiters)...",
	       "Syntax: <delim><char1><char2>...<charN><delim>",
	       "Example: \"SeaSurfaceTemperature\""
	     };
	     AOSs1 (trailer,"Enter: ________   Exit: ________  Help: ________")
	     static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI,
					HELPkey_FSI, NUL };
	     static char value[1+CDF_VAR_NAME_LEN256+1+1]; char delim;
	     static char label[14+CDF_VAR_NAME_LEN256+1];
	     static struct PromptWindowStruct PW = {
		label, 4, 1, 78, 3, header, CDF_VAR_NAME_LEN256+2, value,
		1, trailer, exitChars, REFRESHkey_FSI, SOLkey_FSI, EOLkey_FSI,
		INSERTorOVERkey_FSI
	     };
	     EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
				   HELPkey_FSI);
	     delim = PickDelimiter(varName, strlen(varName));
	     snprintf (value, (size_t) sizeof(value),
		       "%c%s%c", delim, varName, delim);
	     snprintf (label, (size_t) sizeof(label),
		       " %cVariable %c%s%c ", BOO(Z,'z','r'),
		       delim, varName, delim);
	     PromptWindow (NEWpw, &PW, (int) (strlen(PW.value) - 1),
			   LogicalTRUE);
	     for (;;) {
		if (EnterPW(&PW,VARRENAMEhelpID)) {
		  char varName[CDF_VAR_NAME_LEN256+1];
		  if (DecodeDelimitedString(PW.value,varName)) {
		    status = CDFlib (PUT_, VAR_NAME(Z), varName,
				     NULL_);
		    if (ReportStatus(status,FALSE)) {
		      FreeIW (&IW, FatalError);
		      if (!BuildVarMenu(Z,CDFname,&IW)) {
			PromptWindow (DELETEpw, &PW);
			ItemWindow (DELETEiw, &IW);
			return FALSE;
		      }
		      ItemWindow (UPDATEiw, &IW, IW.itemN);
		      break;
		    }
		    if (NoMoreAccess(NULL)) {
		      PromptWindow (DELETEpw, &PW);
		      ItemWindow (DELETEiw, &IW);
		      return FALSE;
		    }
		  }
		  else
		    ProblemWindow ("Illegal name (check delimiters).", FALSE);
		}
		else
		  break;
	     }
	     PromptWindow (DELETEpw, &PW);
	   }
	   break;
	 }
	 /*********************************************************************
	 * Modify data specification (data type and number of elements).
	 *********************************************************************/
	 if (fieldN == 1) {
	   if (browseOnly)
	     ItemWindow (BEEPiw, &IW);
	   else {
	     long numElems, dataType; static char label[14+CDF_VAR_NAME_LEN256+1];
	     char varName[CDF_VAR_NAME_LEN256+1], delim;
	     status = CDFlib (SELECT_, VAR(Z), varN,
			      GET_, VAR_NAME(Z), varName,
				    VAR_DATATYPE(Z), &dataType,
			      NULL_);
	     if (!ReportStatus(status,FALSE)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     delim = PickDelimiter(varName, strlen(varName));
	     snprintf (label, (size_t) sizeof(label),
		       " %cVariable %c%s%c ", BOO(Z,'z','r'),
		       delim, varName, delim);
	     if (SelectDataSpec(&dataType,&numElems,label)) {
	       status = CDFlib (PUT_, VAR_DATASPEC(Z), dataType, numElems,
				NULL_);
	       if (ReportStatus(status,FALSE)) {
		 FreeIW (&IW, FatalError);
		 if (!BuildVarMenu(Z,CDFname,&IW)) {
		   ItemWindow (DELETEiw, &IW);
		   return FALSE;
		 }
		 ItemWindow (UPDATEiw, &IW, IW.itemN);
	       }
	       else {
		 if (NoMoreAccess(NULL)) {
		   ItemWindow (DELETEiw, &IW);
		   return FALSE;
		 }
	       }
	     }
	   }
	   break;
	 }
	 /*********************************************************************
	 * Dimensionality field.
	 *********************************************************************/
	 if (Z && fieldN == 2) {
	   ItemWindow (BEEPiw, &IW);
	   break;
	 }
	 /*********************************************************************
	 * Modify variances.
	 *********************************************************************/
	 if (fieldN == BOO(Z,3,2)) {
	   if (browseOnly)
	     ItemWindow (BEEPiw, &IW);
	   else {
	     long numDims;
	     long recVary, dimVarys[CDF_MAX_DIMS];
	     static char *header[] = {
	       "Enter variances...",
	       "Syntax: <recVary>/<dimVary1><dimVary2>...<dimVaryN>",
	       "Examples: T/, T/F, F/TF, T/TTT"
	     };
	     AOSs1 (trailer,"Enter: ________   Exit: ________  Help: ________")
	     static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI,
					HELPkey_FSI, NUL };
	     static char value[2+CDF_MAX_DIMS+1];
	     static char label[14+CDF_VAR_NAME_LEN256+1];
	     static struct PromptWindowStruct PW = {
	       label, 4, 1, 78, 3, header, 0, value, 1, trailer, exitChars,
	       REFRESHkey_FSI, SOLkey_FSI, EOLkey_FSI,
	       INSERTorOVERkey_FSI
	     };
	     char varName[CDF_VAR_NAME_LEN256+1], delim;
	     EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
				   HELPkey_FSI);
	     status = CDFlib (SELECT_, VAR(Z), varN,
			      GET_, BOO(Z,zVAR_NUMDIMS_,
					  rVARs_NUMDIMS_), &numDims,
				    VAR_RECVARY(Z), &recVary,
				    VAR_DIMVARYS(Z), dimVarys,
				    VAR_NAME(Z), varName,
			      NULL_);
	     if (!ReportStatus(status,FALSE)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     PW.maxChars = (int) (2 + numDims);
	     EncodeVariances (PW.value, recVary, numDims, dimVarys);
	     delim = PickDelimiter(varName, strlen(varName));
	     snprintf (label, (size_t) sizeof(label),
		       " %cVariable %c%s%c ", BOO(Z,'z','r'),
		       delim, varName, delim);
	     PromptWindow (NEWpw, &PW, (int) strlen(PW.value), LogicalTRUE);
	     for (;;) {
		if (EnterPW(&PW,VARVARYShelpID)) {
		  if (DecodeVariances(PW.value,numDims,&recVary,dimVarys)) {
		    status = CDFlib (PUT_, VAR_RECVARY(Z), recVary,
					   VAR_DIMVARYS(Z), dimVarys,
				     NULL_);
		    if (ReportStatus(status,FALSE)) {
		      FreeIW (&IW, FatalError);
		      if (!BuildVarMenu(Z,CDFname,&IW)) {
			PromptWindow (DELETEpw, &PW);
			ItemWindow (DELETEiw, &IW);
			return FALSE;
		      }
		      ItemWindow (UPDATEiw, &IW, IW.itemN);
		      break;
		    }
		    if (NoMoreAccess(NULL)) {
		      PromptWindow (DELETEpw, &PW);
		      ItemWindow (DELETEiw, &IW);
		      return FALSE;
		    }
		  }
		  else
		    ProblemWindow ("Illegal variances.", FALSE);
		}
		else
		  break;
	     }
	     PromptWindow (DELETEpw, &PW);
	   }
	   break;
	 }
	 /*********************************************************************
	 * View/modify attribute entries.
	 *********************************************************************/
	 if (fieldN == BOO(Z,4,3)) {
	   ItemWindow (UNDISPLAYiw, &IW);
	   if (!EditVarEntries(Z,varN,(long)IW.NiLines)) {
	     ItemWindow (DELETEiw, &IW);
	     return FALSE;
	   }
	   ItemWindow (REDISPLAYiw, &IW);
	   break;
	 }
	 /*********************************************************************
	 * View/modify values.
	 *********************************************************************/
	 if (fieldN == BOO(Z,5,4)) {
	   Logical changed = FALSE;
	   ItemWindow (UNDISPLAYiw, &IW);
	   if (!EditVarValues(Z,varN,(long)IW.NiLines,useFormat,&changed)) {
	     ItemWindow (DELETEiw, &IW);
	     return FALSE;
	   }
	   if (changed) {
	     FreeIW (&IW, FatalError);
	     if (!BuildVarMenu(Z,CDFname,&IW)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     ItemWindow (UPDATEiw, &IW, IW.itemN);
	   }
	   ItemWindow (REDISPLAYiw, &IW);
	   break;
	 }
	 /*********************************************************************
	 * More...
	 *********************************************************************/
	 if (fieldN == BOO(Z,6,5)) {
	   Logical changed = FALSE;
	   long nVars = IW.nItems / varJump;
	   ItemWindow (UNDISPLAYiw, &IW);
	   if (!EditVarMore(Z,varN,useFormat,&changed,nVars)) {
	     ItemWindow (DELETEiw, &IW);
	     return FALSE;
	   }
	   if (changed) {
	     FreeIW (&IW, FatalError);
	     if (!BuildVarMenu(Z,CDFname,&IW)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     ItemWindow (UPDATEiw, &IW, IW.itemN);
	   }
	   ItemWindow (REDISPLAYiw, &IW);
	   break;
	 }
	 break;
       }
       /***********************************************************************
       * Create variable.  If variable is successfully created, the current
       * item is set to its name field.
       ***********************************************************************/
       case CREATEVARkey_EDIT: {
	 Logical created;
	 if (!CreateVar(Z,&created)) {
	   ItemWindow (DELETEiw, &IW);
	   return FALSE;
	 }
	 if (created) {
	   FreeIW (&IW, FatalError);
	   if (!BuildVarMenu(Z,CDFname,&IW)) {
	     ItemWindow (DELETEiw, &IW);
	     return FALSE;
	   }
	   ItemWindow (UPDATEiw, &IW, IW.nItems - varJump);
	 }
	 break;
       }
       /***********************************************************************
       * Delete variable.
       * If variable is successfully deleted, the current item is set to...
       ***********************************************************************/
       case DELETEVARkey_EDIT: {
	 char varName[CDF_VAR_NAME_LEN256+1], delim,
	      question[21+CDF_VAR_NAME_LEN256+1];
	 if (IW.nItems == 0) {
	   ProblemWindow (BOO(Z,"Nothing selected (no zVariables exist).",
			        "Nothing selected (no rVariables exist)."),
			  FALSE);
	   break;
	 }
	 varN = IW.itemN / varJump;
	 status = CDFlib (SELECT_, BOO(Z,zVAR_,rVAR_), varN,
			  GET_, BOO(Z,zVAR_NAME_,rVAR_NAME_), varName,
			  NULL_);
	 if (!ReportStatus(status,FALSE)) {
	   ItemWindow (DELETEiw, &IW);
	   return FALSE;
	 }
	 delim = PickDelimiter (varName, strlen(varName));
	 snprintf (question, (size_t) sizeof(question),
		   "Delete %sVariable %c%s%c ?",
		   BOO(Z,"z","r"), delim, varName, delim);
	 if (ConfirmWindow(4,78,question,NULL,FALSE,DELETEVARhelpID)) {
	   status = CDFlib (DELETE_, BOO(Z,zVAR_,rVAR_),
			    NULL_);
	   if (ReportStatus(status,FALSE)) {
	     FreeIW (&IW, FatalError);
	     if (!BuildVarMenu(Z,CDFname,&IW)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     ItemWindow (UPDATEiw, &IW, 0);
	   }
	   else {
	     if (NoMoreAccess(NULL)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	   }
	 }
	 break;
       }
       /***********************************************************************
       * Display online help.
       ***********************************************************************/
       case HELPkey_FSI:
	 OnlineHelpWindow (ilhFile, BOO(Z,ZVARShelpID,RVARShelpID));
	 break;
       /***********************************************************************
       * Exit menu.
       ***********************************************************************/
       case EXITkey_FSI:
	 ItemWindow (DELETEiw, &IW);
	 FreeIW (&IW, FatalError);
	 return TRUE;
     }
   }
}

/******************************************************************************
* BuildVarMenu.
******************************************************************************/

Logical BuildVarMenu (Z, CDFname, IW)
Logical Z;
char *CDFname;
struct ItemWindowStruct *IW;
{
   CDFstatus status;
   long nVars, varN, dataType, numElems, maxRec, nRecords;
   long recVary, dimVarys[CDF_MAX_DIMS];
   long numDims, dimSizes[CDF_MAX_DIMS];
   int curCol, dimN, curItem;
   char varName[CDF_VAR_NAME_LEN256+1], field[80+1];
   AOSs2 (loadingLines, "Loading menu...", "")
   /***************************************************************************
   * Determine number of variables.
   ***************************************************************************/
   status = CDFlib (GET_, BOO(Z,CDF_NUMzVARS_,CDF_NUMrVARS_), &nVars,
		    NULL_);
   if (!ReportStatus(status,FALSE)) return FALSE;
   if (nVars > MANY_VARs) MessageWindow (loadingLines, NULL, LogicalFALSE);
   /***************************************************************************
   * Encode label/header.
   ***************************************************************************/
   status = CDFlib (GET_, VARs_MAXREC(Z), &maxRec,
		    NULL_);
   if (!ReportStatus(status,FALSE)) return FALSE;
   nRecords = maxRec + 1;
   snprintf (IW->label, (size_t) 6+DU_MAX_NAME_LEN+13+1,
	     " CDF \"%s\" %cVariables ", CDFname, BOO(Z,'z','r'));
   if (Z) {
     snprintf (IW->hLines[0], (size_t) sizeof(BLANKs78),
	       "%ld zVariable%s, ", nVars, (nVars <= 1 ? "" : "s"));
     snprintf (&(IW->hLines[0][strlen(IW->hLines[0])]), 
	       (size_t) sizeof(BLANKs78)-strlen(IW->hLines[0]),
	       "(maximum of %ld zRecord%s)", nRecords,
	       (nRecords <= 1 ? "" : "s"));
     strcpyX (IW->hLines[1],
	      "Name             DataSpec  Dimensions       Varys", 0);
   }
   else {
     status = CDFlib (GET_, rVARs_NUMDIMS_, &numDims,
			    rVARs_DIMSIZES_, dimSizes,
		      NULL_);
     if (!ReportStatus(status,FALSE)) return FALSE;
     snprintf (IW->hLines[0], (size_t) sizeof(BLANKs78),
	       "%ld rVariable%s, ", nVars, (nVars <= 1 ? "" : "s"));
     EncodeDimensionality (&(IW->hLines[0][strlen(IW->hLines[0])]), numDims,
			   dimSizes,
			   (size_t) sizeof(BLANKs78)-strlen(IW->hLines[0]));
     strcatX (IW->hLines[0], " dimensionality, ", 0);
     snprintf (&(IW->hLines[0][strlen(IW->hLines[0])]), 
	       (size_t) sizeof(BLANKs78)-strlen(IW->hLines[0]),
	       "%ld rRecord%s", nRecords, (nRecords <= 1 ? "" : "s"));
     strcpyX (IW->hLines[1], "Name             DataSpec  Varys", 0);
   }
   /***************************************************************************
   * Allocate item section control/lines.
   ***************************************************************************/
   AllocIW (IW, (int) (nVars * BOO(Z,7,6)), (int) nVars, 80, FatalError);
   /***************************************************************************
   * Build each variable line.
   ***************************************************************************/
   curItem = 0;
   for (varN = 0; varN < nVars; varN++) {
      /************************************************************************
      * Inquire variable.
      ************************************************************************/
      status = CDFlib (SELECT_, VAR(Z), varN,
		       GET_, VAR_NAME(Z), varName,
			     VAR_DATATYPE(Z), &dataType,
			     VAR_NUMELEMS(Z), &numElems,
			     VAR_RECVARY(Z), &recVary,
			     VAR_DIMVARYS(Z), dimVarys,
		       NULL_);
      if (!ReportStatus(status,FALSE)) return FALSE;
      if (Z) {
		status = CDFlib (GET_, zVAR_NUMDIMS_, &numDims,
				       zVAR_DIMSIZES_, dimSizes,
			 NULL_);
		if (!ReportStatus(status,FALSE)) return FALSE;
      }
      /************************************************************************
      * Setup variable name.
      ************************************************************************/
      curCol = 0;
      EncodeString (strlen(varName), varName, field, 0, VARNAME_FIELD_LEN);
      strcpyX (IW->iLines[(int)varN], field, 0);
      if (strlen(field) < (size_t) VARNAME_FIELD_LEN) {
		CatNcharacters (IW->iLines[(int)varN],
						VARNAME_FIELD_LEN - strlen(field), (int) ' ');
      }
      CatNcharacters (IW->iLines[(int)varN], VARNAME_FIELD_BLANKS, (int) ' ');
      IW->iLineNs[curItem] = (int) varN;
      IW->iCols[curItem] = curCol;
      IW->iLens[curItem] = strlen(field);
      curCol += VARNAME_FIELD_LEN + VARNAME_FIELD_BLANKS;
      curItem++;
      /************************************************************************
      * Setup data type & number of elements.
      ************************************************************************/
      strcpyX (field, DataTypeToken(dataType), 0);
      catchrX (field, (int) '/', 0);
      snprintf (&field[strlen(field)], (size_t) sizeof(field)-strlen(field),
	        "%ld", numElems);
      if (strlen(field) > (size_t) DATASPEC_FIELD_LEN) {
	strcpyX (field + DATASPEC_FIELD_LEN - 3, "...", 0);
      }
      strcatX (IW->iLines[(int)varN], field, 0);
      if (strlen(field) < (size_t) DATASPEC_FIELD_LEN) {
	CatNcharacters (IW->iLines[(int)varN],
			DATASPEC_FIELD_LEN - strlen(field), (int) ' ');
      }
      CatNcharacters (IW->iLines[(int)varN], DATASPEC_FIELD_BLANKS, (int) ' ');
      IW->iLineNs[curItem] = (int) varN;
      IW->iCols[curItem] = curCol;
      IW->iLens[curItem] = strlen(field);
      curCol += DATASPEC_FIELD_LEN + DATASPEC_FIELD_BLANKS;
      curItem++;
      /************************************************************************
      * If Z variable, setup dimensionality.
      ************************************************************************/
      if (Z) {
	snprintf (field, (size_t) sizeof(field), "%ld:[", numDims);
	for (dimN = 0; dimN < numDims; dimN++) {
	   snprintf (&field[strlen(field)],
		     (size_t) sizeof(field)-strlen(field),
		     "%s%ld", (dimN == 0 ? "" : ","), dimSizes[dimN]);
	}
	catchrX (field, (int) ']', 0);
	if (strlen(field) > (size_t) DIMENSIONS_FIELD_LEN) {
	  strcpyX (field + DIMENSIONS_FIELD_LEN - 3, "...", 0);
	}
	strcatX (IW->iLines[(int)varN], field, 0);
	if (strlen(field) < (size_t) DIMENSIONS_FIELD_LEN) {
	  CatNcharacters (IW->iLines[(int)varN],
			  DIMENSIONS_FIELD_LEN - strlen(field), (int) ' ');
	}
	CatNcharacters (IW->iLines[(int)varN], DIMENSIONS_FIELD_BLANKS,
			(int) ' ');
	IW->iLineNs[curItem] = (int) varN;
	IW->iCols[curItem] = curCol;
	IW->iLens[curItem] = strlen (field);
	curCol += DIMENSIONS_FIELD_LEN + DIMENSIONS_FIELD_BLANKS;
	curItem++;
      }
      /************************************************************************
      * Setup variances.
      ************************************************************************/
      snprintf (field, (size_t) sizeof(field), "%s/", TFvarianceToken(recVary));
      for (dimN = 0; dimN < numDims; dimN++) {
	 strcatX (field, TFvarianceToken(dimVarys[dimN]), 0);
      }
      if (strlen(field) > (size_t) VARIANCES_FIELD_LEN) {
	strcpyX (field + VARIANCES_FIELD_LEN - 3, "...", 0);
      }
      strcatX (IW->iLines[(int)varN], field, 0);
      if (strlen(field) < (size_t) VARIANCES_FIELD_LEN) {
	CatNcharacters (IW->iLines[(int)varN],
			VARIANCES_FIELD_LEN - strlen(field), (int) ' ');
      }
      CatNcharacters (IW->iLines[(int)varN], VARIANCES_FIELD_BLANKS, (int)' ');
      IW->iLineNs[curItem] = (int) varN;
      IW->iCols[curItem] = curCol;
      IW->iLens[curItem] = strlen (field);
      curCol += VARIANCES_FIELD_LEN + VARIANCES_FIELD_BLANKS;
      curItem++;
      /************************************************************************
      * Setup "entries" item.
      ************************************************************************/
      if (!Z) {
	CatNcharacters (IW->iLines[(int)varN],
			DIMENSIONS_FIELD_LEN + DIMENSIONS_FIELD_BLANKS,
			(int) ' ');
	curCol += DIMENSIONS_FIELD_LEN + DIMENSIONS_FIELD_BLANKS;
      }
      strcpyX (field, (Z ? "<zEntries>" : "<rEntries>"), 0);
      strcatX (IW->iLines[(int)varN], field, 0);
      CatNcharacters (IW->iLines[(int)varN], 1, (int) ' ');
      IW->iLineNs[curItem] = (int) varN;
      IW->iCols[curItem] = curCol;
      IW->iLens[curItem] = strlen(field);
      curCol += strlen(field) + 1;
      curItem++;
      /************************************************************************
      * Setup "values" item.
      ************************************************************************/
      strcpyX (field, "<values>", 0);
      strcatX (IW->iLines[(int)varN], field, 0);
      CatNcharacters (IW->iLines[(int)varN], 1, (int) ' ');
      IW->iLineNs[curItem] = (int) varN;
      IW->iCols[curItem] = curCol;
      IW->iLens[curItem] = strlen (field);
      curCol += strlen(field) + 1;
      curItem++;
      /************************************************************************
      * Setup "more" item.
      ************************************************************************/
      strcpyX (field, "<more>", 0);
      strcatX (IW->iLines[(int)varN], field, 0);
      IW->iLineNs[curItem] = (int) varN;
      IW->iCols[curItem] = curCol;
      IW->iLens[curItem] = strlen(field);
      curItem++;
   }
   if (nVars > MANY_VARs) MessageWindow (NULL);
   return TRUE;
}

/******************************************************************************
* EditVarValues.
******************************************************************************/

Logical EditVarValues (Z, varNum, nVars, useFormat, changed)
Logical Z;
long varNum;
long nVars;
Logical useFormat;
Logical *changed;	/* Assumed to be initialized to FALSE by caller. */
{
  CDFstatus status; size_t nBytes; Logical overflow, done;
  long dataType, numElems, numDims, dimSizes[CDF_MAX_DIMS], recVary;
  long dimVarys[CDF_MAX_DIMS], indices[CDF_MAX_DIMS], recNum, maxRec;
  long cType, cParms[CDF_MAX_PARMS], cPct;
  int dimN; void *binary; char delim, *format, varName[CDF_VAR_NAME_LEN256+1];
  static Logical first = TRUE;
  static int vvCols[1+CDF_MAX_DIMS+1];
  static int vvLens[1+CDF_MAX_DIMS+1];
  static int iLineNs[1+CDF_MAX_DIMS+1] = { 0,0,0,0,0,0,0,0,0,0,0,0 };
  static char *vString;
  static char vLine[MAX_SCREENLINE_LEN+1];
  static char label[12+CDF_VAR_NAME_LEN256+9+1];
  static char *iLines[] = { vLine };
  static char *header[] = {
    "Syntax: <recNum>:[<index1>,<index2>,...,<indexN>] = <value>"
  };
  AOSs3 (trailer,
    "Select:    ________       Exit:      ________       Help: ________",
    "Next item: ____________   Increment: ____________   Next variable: ________",
    "Prev item: ____________   Decrement: ____________")
  static int exitChars[] = {
    ENTERkey_FSI, EXITkey_FSI, INCREMENTkey_EDIT, DECREMENTkey_EDIT,
    HELPkey_FSI, NEXTVARkey_EDIT, NUL
  };
  static struct ItemWindowStruct IW = {
    0, 0, 80, label, 1, header, 1, iLines, 0, iLineNs, vvCols, vvLens, 1,
    3, trailer, exitChars, REFRESHkey_FSI, FALSE, NUL, NUL
  };
  /***************************************************************************
  * First time...
  ***************************************************************************/
  if (first) {
    EncodeKeyDefinitions (3, trailer, ENTERkey_FSI, EXITkey_FSI,
			  HELPkey_FSI, NEXTFIELDkey_EDIT, INCREMENTkey_EDIT,
			  NEXTVARkey_EDIT, PREVFIELDkey_EDIT,
			  DECREMENTkey_EDIT);
    first = FALSE;
  }
  /***************************************************************************
  * Initialize.
  ***************************************************************************/
  MakeNUL (label);
  MakeNUL (vLine);
  IW.nItems = 0;
  /***************************************************************************
  * Display the window and until an exit is requested...
  ***************************************************************************/
  ItemWindow (NEWiw, &IW, 0);
  for (;;) {
     /*************************************************************************
     * Inquire the variable.
     *************************************************************************/
     status = CDFlib (SELECT_, VAR(Z), varNum,
		      GET_, VAR_NAME(Z), varName,
			    BOO(Z,zVAR_NUMDIMS_,rVARs_NUMDIMS_), &numDims,
			    BOO(Z,zVAR_DIMSIZES_,rVARs_DIMSIZES_), dimSizes,
			    VAR_DATATYPE(Z), &dataType,
			    VAR_NUMELEMS(Z), &numElems,
			    VAR_RECVARY(Z), &recVary,
			    VAR_DIMVARYS(Z), dimVarys,
			    VAR_COMPRESSION(Z), &cType, cParms, &cPct,
		      CONFIRM_, BOO(Z,zVAR_RECNUMBER_,
				      rVARs_RECNUMBER_), &recNum,
				BOO(Z,zVAR_DIMINDICES_,
				      rVARs_DIMINDICES_), indices,
		      NULL_);
     if (!ReportStatus(status,FALSE)) {
       ItemWindow (DELETEiw, &IW);
       return FALSE;
     }
     /*************************************************************************
     * Allocate needed memory buffers.
     *************************************************************************/
     nBytes = (size_t) (numElems * CDFelemSize(dataType));
     binary = cdf_AllocateMemory (nBytes, FatalError);
     nBytes = (size_t) BOO(STRINGdataType(dataType),
			   numElems+3,MAX_nonSTRING_VALUE_LEN);
     vString = (char *) cdf_AllocateMemory (nBytes, FatalError);
     /*************************************************************************
     * Encode the label.
     *************************************************************************/
     delim = PickDelimiter (varName, strlen(varName));
     snprintf (IW.label, (size_t) sizeof(label),
	       " %cVariable %c%s%c Values ", BOO(Z,'z','r'), delim,
	       varName, delim);
     /*************************************************************************
     * Get the FORMAT attribute entry.
     *************************************************************************/
     if (useFormat) {
       status = GetFormatEntry (Z, varNum, &format);
       if (!ReportStatus(status,FALSE)) {
	 ItemWindow (DELETEiw, &IW);
	 return FALSE;
       }
     }
     else
       format = NULL;
     /*************************************************************************
     * If the variable is compressed and the values actually compressed, put
     * a "loading" message where the record/indices/value will be displayed.
     *************************************************************************/
     if (cType != NO_COMPRESSION && cPct < 100)
       strcpyX (vLine, "Loading values...", MAX_SCREENLINE_LEN);
     else
       MakeNUL (vLine);
     ItemWindow (UPDATEiw, &IW, 0);
     /*************************************************************************
     * Encode/display the initial record/indices/value.
     *************************************************************************/
     if (!BuildVarValueLine(Z,recNum,recVary,numDims,indices,
			    dimVarys,dataType,numElems,binary,
			    vString,vLine,&(IW.nItems),vvCols,
			    vvLens,IW.nColsTotal-2,format,&overflow,nBytes)) {
       ItemWindow (DELETEiw, &IW);
       return FALSE;
     }
     ItemWindow (UPDATEiw, &IW, 0);
     /*************************************************************************
     * Until done with this variable...
     *************************************************************************/
     done = FALSE;
     while (!done) {
       ItemWindow (READiw, &IW);
       switch (IW.key) {
	 /*********************************************************************
         * Up or down arrow.
         *********************************************************************/
         case INCREMENTkey_EDIT:
		 case DECREMENTkey_EDIT: {
		   Logical increment = (IW.key == INCREMENTkey_EDIT);
		   /*****************************************************************
		   * Check for the record number.  If the record variance is FALSE,
		   * incrementing and/or decrementing is illegal.
		   *****************************************************************/
		   if (IW.itemN == 0) {
		     if (recVary) {
		       status = CDFlib (GET_, VAR_MAXREC(Z), &maxRec,
						        NULL_);
		       if (!ReportStatus(status,FALSE)) {
				 ItemWindow (DELETEiw, &IW);
				 return FALSE;
	   		   }
		       if (maxRec == -1) maxRec = 0;
		   	   if (increment)
#if MAXREC_LIMIT
		         recNum = BOO(recNum < maxRec,recNum + 1,0);
#else
				 recNum++;
#endif
		       else
#if MAXREC_LIMIT
		         recNum = BOO(recNum == 0,maxRec,
						      BOO(recNum > maxRec,maxRec,recNum - 1));
#else
		         recNum = BOO(recNum == 0,maxRec,recNum - 1);
#endif
		       if (!BuildVarValueLine(Z,recNum,recVary,numDims,indices,
				      dimVarys,dataType,numElems,binary,
				      vString,vLine,&(IW.nItems),vvCols,vvLens,
				      IW.nColsTotal-2,format,&overflow,nBytes)) {
				 ItemWindow (DELETEiw, &IW);
				 return FALSE;
		       }
		       ItemWindow (UPDATEiw, &IW, IW.itemN);
		     }
		     else
		       ItemWindow (BEEPiw, &IW);
		     break;
		   }
		   /*****************************************************************
		   * Check for the value (which is an error - just ring bell).  The
		   * value is always the last item.
		   *****************************************************************/
		   if (IW.itemN == IW.nItems - 1) {
		     ItemWindow (BEEPiw, &IW);
		     break;
		   }
		   /*****************************************************************
		   * Must be a dimension index.  Calculate which dimension was
		   * selected.  There must be at least one dimension for this
		   * point to have been reached.
		   *****************************************************************/
		   dimN = IW.itemN - 1;
		   if (dimVarys[dimN]) {
		     if (increment)
		       indices[dimN] = (indices[dimN] + 1) % dimSizes[dimN];
		     else
		       indices[dimN] = (indices[dimN] - 1 + dimSizes[dimN]) %
				       dimSizes[dimN];
		     if (!BuildVarValueLine(Z,recNum,recVary,numDims,indices,dimVarys,
					    dataType,numElems,binary,vString,vLine,
					    &(IW.nItems),vvCols,vvLens,
					    IW.nColsTotal-2,format,&overflow,nBytes)) {
		       ItemWindow (DELETEiw, &IW);
		       return FALSE;
		     }
		     ItemWindow (UPDATEiw, &IW, IW.itemN);
		   }
		   else
		     ItemWindow (BEEPiw, &IW);
		   break;
		 }
		 /*********************************************************************
         * Modify key.
         *********************************************************************/
         case ENTERkey_FSI:
		   /*******************************************************************
		   * Check for the record number.  If the record variance is FALSE, a
		   * modification is illegal.
		   *******************************************************************/
		   if (IW.itemN == 0) {
		     if (recVary) {
		       static char *header[] = { "Enter record number..." };
		       AOSs1 (trailer,
			      "Enter: ________   Exit: ________  Help: ________")
		       static char value[MAX_RECORD_NUMBER_LEN+1];
		       static int exitChars[] = {
				 ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL
		       };
		       static struct PromptWindowStruct PW = {
		         NULL, 9, 0, 80, 1, header, MAX_RECORD_NUMBER_LEN, value,
		         1, trailer, exitChars, REFRESHkey_FSI, SOLkey_FSI,
		         EOLkey_FSI, INSERTorOVERkey_FSI
		       };
		       EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
								     HELPkey_FSI);
		       snprintf (PW.value, (size_t) sizeof(value),
				 "%ld", recNum + 1);
		       PromptWindow (NEWpw, &PW, (int) strlen(PW.value), LogicalTRUE);
		       for (;;) {
				  if (EnterPW(&PW,RECNUMBERhelpID)) {
				    long newRecNum;
				    if (sscanf(PW.value,"%ld",&newRecNum) == 1) {
				      if (newRecNum > 0) {
				        recNum = newRecNum - 1;
				        if (!BuildVarValueLine(Z,recNum,recVary,numDims,
						      indices,dimVarys,dataType,
						      numElems,binary,vString,vLine,
						      &(IW.nItems),vvCols,vvLens,
						      IW.nColsTotal-2,
						      format,&overflow,nBytes)) {
						  PromptWindow (DELETEpw, &PW);
						  ItemWindow (DELETEiw, &IW);
						  return FALSE;
						}
				        ItemWindow (UPDATEiw, &IW, IW.itemN);
				        break;
				      }
				      else
				        ProblemWindow ("Record number out of valid range.",
								       FALSE);
				    }
			    else
		      ProblemWindow ("Error decoding record number.", FALSE);
		  }
		  else
		    break;
	       }
	       PromptWindow (DELETEpw, &PW);
	       }
	     else
	       ItemWindow (BEEPiw, &IW);
	     break;
	   }
	   /*****************************************************************
	   * Check for the value.  If so, prompt for new value (unless
	   * browsing).  The value is always the last item.
	   *****************************************************************/
	   if (IW.itemN == IW.nItems - 1) {
	     if (browseOnly && !overflow) {
	       ItemWindow (BEEPiw, &IW);
	     }
	     else {
	       static char *headerBrowse[] = { "Full value..." };
	       static char *headerNoCHAR[] = { "Enter new value..." };
	       static char *headerCHAR[] = {
		 "Enter new value (delimiters required)..."
	       };
	       AOSs1A (trailerBrowse, "Exit: ________  Help: ________")
	       AOSs1B (trailerEdit,
		      "Enter: ________   Exit: ________  Help: ________")
	       static int exitCharsBrowse[] = { EXITkey_FSI, HELPkey_FSI, NUL};
	       static int exitCharsEdit[] = {
		 ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL
	       };
	       static struct PromptWindowStruct PW = {
		  NULL, 9, 0, 80, 1, NULL, 0, NULL, 1, NULL, NULL,
		  REFRESHkey_FSI, SOLkey_FSI, EOLkey_FSI, INSERTorOVERkey_FSI
	       };
	       if (browseOnly) {
		 EncodeKeyDefinitions (1, trailerBrowse, EXITkey_FSI,
				       HELPkey_FSI);
		 PW.hLines = headerBrowse;
		 PW.tLines = trailerBrowse;
		 PW.exitChars = exitCharsBrowse;
	       }
	       else {
		 EncodeKeyDefinitions (1, trailerEdit, ENTERkey_FSI,
				       EXITkey_FSI, HELPkey_FSI);
		 PW.hLines = BOO(STRINGdataType(dataType),
				 headerCHAR,headerNoCHAR);
		 PW.tLines = trailerEdit;
		 PW.exitChars = exitCharsEdit;
	       }
	       PW.maxChars = (int) BOO(STRINGdataType(dataType),
				       numElems+3,MAX_nonSTRING_VALUE_LEN);
	       nBytes = (size_t) (PW.maxChars + 1);
	       PW.value = (char *) cdf_AllocateMemory (nBytes, FatalError);
	       strcpyX (PW.value, vString, PW.maxChars);
	       PromptWindow (NEWpw, &PW,
			     (int) BOO(STRINGdataType(dataType),
				       strlen(PW.value)-1,
				       strlen(PW.value)), LogicalTRUE);
	       for (;;) {
		  if (EnterPW(&PW,VARVALUEhelpID)) {
		    long nElemsEntered; void *newBinary;
                    int style;
                    if (TT2000dataType(dataType)) style = TT2000_3_STYLE;
                    else style = EPOCH0_STYLE;
		    if (DecodeValues(PW.value,dataType,
				     &nElemsEntered,&newBinary,style)) {
		      if (nElemsEntered == numElems) {
			status = CDFlib (PUT_, VAR_DATA(Z), newBinary,
					 NULL_);
			cdf_FreeMemory (newBinary, FatalError);
			if (ReportStatus(status,FALSE)) {
			  if (!BuildVarValueLine(Z,recNum,recVary,
						 numDims,indices,
						 dimVarys,dataType,
						 numElems,binary,
						 vString,vLine,
						 &(IW.nItems),
						 vvCols, vvLens,
						 IW.nColsTotal-2,
						 format,&overflow,nBytes)) {
			    PromptWindow (DELETEpw, &PW);
			    ItemWindow (DELETEiw, &IW);
			    return FALSE;
			  }
			  ItemWindow (UPDATEiw, &IW, IW.itemN);
			  *changed = TRUE;
			  break;
			}
			if (NoMoreAccess(NULL)) {
			  PromptWindow (DELETEpw, &PW);
			  ItemWindow (DELETEiw, &IW);
			  return FALSE;
			}
		      }
		      else {
			char tempS[MAX_MESSAGE_TEXT_LEN+1];
			strcpyX (tempS, "Illegal number of elements ",
				 MAX_MESSAGE_TEXT_LEN);
			if (nElemsEntered > numElems)
			  strcatX (tempS, "(too many).", MAX_MESSAGE_TEXT_LEN);
			else
			  strcatX (tempS, "(too few).", MAX_MESSAGE_TEXT_LEN);
			ProblemWindow (tempS, FALSE);
			cdf_FreeMemory (newBinary, FatalError);
		      }
		    }
		    else
		      ProblemWindow ("Illegal value.", FALSE);
		  }
		  else
		    break;
	       }
	       cdf_FreeMemory (PW.value, FatalError);
	       PromptWindow (DELETEpw, &PW);
	     }
	     break;
	   }
	   /*****************************************************************
	   * Must be a dimension index.  Calculate which dimension was
	   * selected.  There must be at least one dimension for this point
	   * to have been reached.
	   *****************************************************************/
	   {
	     static char *header[] = { "Enter dimension index..." };
	     AOSs1 (trailer,
		    "Enter: ________   Exit: ________  Help: ________")
	     static char value[MAX_DIMENSION_INDEX_LEN+1];
	     static int exitChars[] = {
	       ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL
	     };
	     static struct PromptWindowStruct PW = {
	       NULL, 9, 0, 80, 1, header, MAX_DIMENSION_INDEX_LEN, value,
	       1, trailer, exitChars, REFRESHkey_FSI, SOLkey_FSI,
	       EOLkey_FSI, INSERTorOVERkey_FSI
	     };
	     EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
				   HELPkey_FSI);
	     dimN = IW.itemN - 1;
	     if (dimVarys[dimN]) {
	       snprintf (PW.value, (size_t) sizeof(value),
			 "%ld", indices[dimN]+1);
	       PromptWindow (NEWpw, &PW, (int) strlen(PW.value), LogicalTRUE);
	       for (;;) {
		  if (EnterPW(&PW,DIMINDEXhelpID)) {
		    long newIndex;
		    if (sscanf(PW.value,"%ld",&newIndex) == 1) {
		      if (newIndex > 0 && newIndex <= dimSizes[dimN]) {
		        indices[dimN] = newIndex - 1;
		        if (!BuildVarValueLine(Z,recNum,recVary,numDims,
					       indices,dimVarys,dataType,
					       numElems,binary,vString,vLine,
					       &(IW.nItems),vvCols,vvLens,
					       IW.nColsTotal-2,
					       format,&overflow,nBytes)) {
			  PromptWindow (DELETEpw, &PW);
			  ItemWindow (DELETEiw, &IW);
			  return FALSE;
			}
		        ItemWindow (UPDATEiw, &IW, IW.itemN);
		        break;
		      }
		      else
		        ProblemWindow ("Dimension index out of range", FALSE);
		    }
		    else
		      ProblemWindow ("Error decoding dimension index.", FALSE);
		  }
		  else
		    break;
	       }
	       PromptWindow (DELETEpw, &PW);
	     }
	     else
	       ItemWindow (BEEPiw, &IW);
	     break;
	   }
	 /*********************************************************************
         * Display online help.
         *********************************************************************/
         case HELPkey_FSI:
	   OnlineHelpWindow (ilhFile, VARVALUEShelpID);
	   break;
         /*********************************************************************
         * Next variable.
         *********************************************************************/
         case NEXTVARkey_EDIT:
	   cdf_FreeMemory (vString, FatalError);
	   cdf_FreeMemory (binary, FatalError);
	   if (format != NULL) cdf_FreeMemory (format, FatalError);
	   varNum = (varNum + 1) % nVars;
	   done = TRUE;
	   break;
         /*********************************************************************
         * Exit.
         *********************************************************************/
         case EXITkey_FSI:
	   cdf_FreeMemory (vString, FatalError);
	   cdf_FreeMemory (binary, FatalError);
	   if (format != NULL) cdf_FreeMemory (format, FatalError);
	   ItemWindow (DELETEiw, &IW);
	   return TRUE;
         /*********************************************************************
         * Unknown key.
         *********************************************************************/
         default:
	   ItemWindow (BEEPiw, &IW);
       }
     }
  }
}

/******************************************************************************
* BuildVarValueLine.
******************************************************************************/

Logical BuildVarValueLine (Z, recNum, recVary, numDims, indices, dimVarys,
			   dataType, numElems, binary, vString, vLine, nItems,
			   vvCols, vvLens, nCols, format, overflow, width)
Logical Z;
long recNum;
long recVary;
long numDims;
long *indices;
long *dimVarys;
long dataType;
long numElems;
void *binary;
char *vString;
char *vLine;
int *nItems;
int *vvCols;
int *vvLens;
int nCols;
char *format;
Logical *overflow;
size_t width;
{
   CDFstatus status; int dimN; Logical virtual;
   static char virtualT[] = " <virtual>";
   int itemN = 0, availCols;
   int style;
   /***************************************************************************
   * Encode record number.
   ***************************************************************************/
   vvCols[itemN] = 0;
   if (recVary)
     snprintf (vLine, (size_t) MAX_SCREENLINE_LEN+1,
	       "%ld", (long) (recNum + 1));
   else
     strcpyX (vLine, "*", 0);
   vvLens[itemN] = strlen(vLine);
   itemN++;
   strcatX (vLine, ":[", 0);
   /***************************************************************************
   * Encode dimension indices.
   ***************************************************************************/
   for (dimN = 0; dimN < numDims; dimN++) {
      if (dimN != 0) strcatX (vLine, ",", 0);
      vvCols[itemN] = strlen(vLine);
      if (dimVarys[dimN])
	snprintf (EofS(vLine), (size_t) MAX_SCREENLINE_LEN+1-strlen(vLine),
	          "%ld", (long) (indices[dimN] + 1));
      else
	strcpyX (EofS(vLine), "*", 0);
      vvLens[itemN] = strlen (&vLine[vvCols[itemN]]);
      itemN++;
   }
   strcatX (vLine, "] = ", 0);
   /***************************************************************************
   * Encode value.
   ***************************************************************************/
   status = CDFlib (SELECT_, BOO(Z,zVAR_RECNUMBER_,rVARs_RECNUMBER_), recNum,
			     BOO(Z,zVAR_DIMINDICES_,
				   rVARs_DIMINDICES_), indices,
                    GET_, VAR_DATA(Z), binary,
		    NULL_);
   if (!ReportStatus(status,FALSE)) return FALSE;
   vvCols[itemN] = strlen(vLine);
   virtual = (status == VIRTUAL_RECORD_DATA);
   availCols = nCols - strlen(vLine) - BOO(virtual,strlen(virtualT),0);
   *overflow = BOO(STRINGdataType(dataType),
		   availCols < numElems + 3,
		   availCols < MAX_nonSTRING_VALUE_LEN);
   if (TT2000dataType(dataType)) style = TT2000_3_STYLE;
   else style = EPOCH0_STYLE;
   if ((dataType == CDF_FLOAT) || (dataType == CDF_REAL4)) {
     if (!isnan((double)*(float *)binary) && 
         !isinf((double)*(float *)binary) &&
         *(float *)binary <= DEFAULT_FLOAT_PADVALUE) {
       EncodeValuesFormat (dataType, numElems, binary, EofS(vLine), NULL,
                           -BOO(virtual,availCols,0), availCols, style,
                           (size_t) MAX_SCREENLINE_LEN+1-strlen(vLine));
     } else
       EncodeValuesFormat (dataType, numElems, binary, EofS(vLine), format,
                           -BOO(virtual,availCols,0), availCols, style,
                           (size_t) MAX_SCREENLINE_LEN+1-strlen(vLine));
   } else if ((dataType == CDF_DOUBLE) || (dataType == CDF_REAL8)) {
     if (!isnan(*(double *)binary) && !isinf(*(double *)binary) &&
         *(double *)binary <= DEFAULT_DOUBLE_PADVALUE) {
       EncodeValuesFormat (dataType, numElems, binary, EofS(vLine), NULL,
                           -BOO(virtual,availCols,0), availCols, style,
                           (size_t) MAX_SCREENLINE_LEN+1-strlen(vLine));
     } else
       EncodeValuesFormat (dataType, numElems, binary, EofS(vLine), format,
                           -BOO(virtual,availCols,0), availCols, style,
                           (size_t) MAX_SCREENLINE_LEN+1-strlen(vLine));
   } else
     EncodeValuesFormat (dataType, numElems, binary, EofS(vLine), format,
		         -BOO(virtual,availCols,0), availCols, style,
		         (size_t) MAX_SCREENLINE_LEN+1-strlen(vLine));
   if (virtual) strcatX (vLine, virtualT, 0);
   vvLens[itemN] = strlen(&vLine[vvCols[itemN]]);
   *nItems = itemN + 1;
   if ((dataType == CDF_FLOAT) || (dataType == CDF_REAL4)) {
     if (!isnan((double)*(float *)binary) &&
         !isinf((double)*(float *)binary) &&
         *(float *)binary <= DEFAULT_FLOAT_PADVALUE) {
       EncodeValuesFormat (dataType, numElems, binary, vString, NULL, 0,
                           BOO(STRINGdataType(dataType),
                               (int)(numElems + 3),MAX_nonSTRING_VALUE_LEN),
                           style, width);
     } else
       EncodeValuesFormat (dataType, numElems, binary, vString, format, 0,
                           BOO(STRINGdataType(dataType),
                               (int)(numElems + 3),MAX_nonSTRING_VALUE_LEN),
                           style, width);
   } else if ((dataType == CDF_DOUBLE) || (dataType == CDF_REAL8)) {
     if (!isnan(*(double *)binary) && !isinf(*(double *)binary) &&
         *(double *)binary <= DEFAULT_DOUBLE_PADVALUE) {
       EncodeValuesFormat (dataType, numElems, binary, vString, NULL, 0,
                           BOO(STRINGdataType(dataType),
                               (int)(numElems + 3),MAX_nonSTRING_VALUE_LEN),
                           style, width);
     } else
       EncodeValuesFormat (dataType, numElems, binary, vString, format, 0,
                           BOO(STRINGdataType(dataType),
                               (int)(numElems + 3),MAX_nonSTRING_VALUE_LEN),
                           style, width);
   } else           
     EncodeValuesFormat (dataType, numElems, binary, vString, format, 0,
		         BOO(STRINGdataType(dataType),
			     (int)(numElems + 3),MAX_nonSTRING_VALUE_LEN),
		         style, width);
   return TRUE;
}

/******************************************************************************
* EditVarMore.
******************************************************************************/

Logical EditVarMore (Z, varN, useFormat, changed, nVars)
Logical Z;
long varN;
Logical useFormat;
Logical *changed;        /* Only report changes that affect the caller.
			   Assumed to be initialized to FALSE by caller. */
long nVars;
{
   CDFstatus status; static Logical first = TRUE;
   AOSs6 (header, BLANKs78, BLANKs78, BLANKs78, BLANKs78, BLANKs78, BLANKs78)
   AOSs1 (trailer, "Select: ________   Exit: ________   Help: ________   Next variable: ________")
   static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI,
			      NEXTVARkey_EDIT, NUL };
   static char label[12+CDF_VAR_NAME_LEN256+9+1];
   static char *itemsBrowse[] = {
     "<View pad value>",
     "",
     ""
   };
   static char *itemsEdit[] = {
     "<Modify pad value> <Modify blocking factor> <Modify sparseness>",
     "<Allocate records> <Write initial records> <Modify compression>",
     "<Delete records>"
   };
   static char *itemsEdit2[] = {
     "<Modify pad value> <Modify blocking factor> <Modify sparseness>",
     "<Allocate records> <Write initial records> <Modify compression>",
     "<Delete records opt1> <Delete records opt2>"
   };
   static int iLineNsBrowse[] = { 0 };
   static int iColsBrowse[] = { 0 };
   static int iLensBrowse[] = { 16 };
   static int iLineNsEdit[] = { 0,0,0,1,1,1,2 };
   static int iLineNsEdit2[] = { 0,0,0,1,1,1,2,2 };
   static int iColsEdit[] = { 0,19,44,0,19,43,0 };
   static int iColsEdit2[] = { 0,19,44,0,19,43,0,22 };
   static int iLensEdit[] = { 18,24,19,18,23,20,16 };
   static int iLensEdit2[] = { 18,24,19,18,23,20,21,21 };
   static struct ItemWindowStruct IW = {
      0, 0, 80, label, 6, header, 3, NULL, 0, NULL, NULL, NULL, 3, 1, trailer,
      exitChars, REFRESHkey_FSI, FALSE, NUL, NUL
   };
   /***************************************************************************
   * First time...
   ***************************************************************************/
   if (first) {
     EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
			   HELPkey_FSI, NEXTVARkey_EDIT);
     first = FALSE;
   }
     if (browseOnly) {
       IW.iLines = itemsBrowse;
       IW.nItems = 1;
       IW.iLineNs = iLineNsBrowse;
       IW.iCols = iColsBrowse;
       IW.iLens = iLensBrowse;
     }
     else {
       if (RegularVar(Z, varN)) {
         IW.iLines = itemsEdit;
         IW.nItems = 7;
         IW.iLineNs = iLineNsEdit;
         IW.iCols = iColsEdit;
         IW.iLens = iLensEdit;
       } else {
         IW.iLines = itemsEdit2;
         IW.nItems = 8;
         IW.iLineNs = iLineNsEdit2;
         IW.iCols = iColsEdit2;
         IW.iLens = iLensEdit2;
       }
     }
   /***************************************************************************
   * Build variable `more' menu and display window.
   ***************************************************************************/
   if (!BuildVarMoreMenu(Z,varN,&IW,useFormat,TRUE)) return FALSE;
   ItemWindow (NEWiw, &IW, 0);
   /***************************************************************************
   * Read/process keystrokes until request to exit menu.
   ***************************************************************************/
   for (;;) {
     ItemWindow (READiw, &IW);
     switch (IW.key) {
       /***********************************************************************
       * Item selected.
       ***********************************************************************/
       case ENTERkey_FSI: {
	 switch (IW.itemN) {
	   /*******************************************************************
	   * View/modify pad value.
	   *******************************************************************/
	   case VIEWorMODIFYpadIN: {
	     if (!ModPadValue(Z,varN,useFormat,changed)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     if (*changed) {
	       if (!BuildVarMoreMenu(Z,varN,&IW,useFormat,FALSE)) {
		 ItemWindow (DELETEiw, &IW);
		 return FALSE;
	       }
	       ItemWindow (UPDATEiw, &IW, IW.itemN);
	     }
	     break;
	   }
	   /*******************************************************************
	   * Blocking factor.
	   *******************************************************************/
	   case MODIFYblockingIN: {
	     static char *header[] = { "Enter blocking factor (records)..." };
	     AOSs1 (trailer,
		    "Enter: ________   Exit: ________  Help: ________")
	     static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI,
					HELPkey_FSI, NUL };
	     static char value[MAX_nonSTRING_VALUE_LEN+1];
	     static struct PromptWindowStruct PWt = {
		NULL, 14, 0, 80, 1, header, MAX_nonSTRING_VALUE_LEN, value,
		1, trailer, exitChars, REFRESHkey_FSI, SOLkey_FSI,
		EOLkey_FSI, INSERTorOVERkey_FSI
	     };
	     EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
				   HELPkey_FSI);
	     strcpyX (value, "", MAX_nonSTRING_VALUE_LEN);
	     PromptWindow (NEWpw, &PWt, 0, LogicalTRUE);
	     for (;;) {
		if (EnterPW(&PWt,EXTENDRECShelpID)) {
		  long nRecords;
		  if (sscanf(PWt.value,"%ld",&nRecords) == 1) {
		    status = CDFlib (SELECT_, VAR(Z), varN,
				     PUT_, VAR_BLOCKINGFACTOR(Z), nRecords,
				     NULL_);
		    if (ReportStatus(status,FALSE)) {
		      if (!BuildVarMoreMenu(Z,varN,&IW,useFormat,FALSE)) {
			PromptWindow (DELETEpw, &PWt);
			ItemWindow (DELETEiw, &IW);
			return FALSE;
		      }
		      ItemWindow (UPDATEiw, &IW, IW.itemN);
		      break;
		    }
		    if (NoMoreAccess(NULL)) {
		      PromptWindow (DELETEpw, &PWt);
		      ItemWindow (DELETEiw, &IW);
		      return FALSE;
		    }
		  }
		  else
		    ProblemWindow ("Error decoding number of records.", FALSE);
		}
		else
		  break;
	     }
	     PromptWindow (DELETEpw, &PWt);
	     break;
	   }
	   /*******************************************************************
	   * Allocate records.
	   *******************************************************************/
	   case ALLOCATErecordsIN: {
	     static char *header[] = {
	       "Enter first/last records to allocate.  Examples: 1/100, 10/10"
	     };
	     AOSs1 (trailer,
		    "Enter: ________   Exit: ________  Help: ________")
	     static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI,
					HELPkey_FSI, NUL };
	     static char value[MAX_nonSTRING_VALUE_LEN+1];
	     static struct PromptWindowStruct PWt = {
		NULL, 14, 0, 80, 1, header, MAX_nonSTRING_VALUE_LEN, value,
		1, trailer, exitChars, REFRESHkey_FSI, SOLkey_FSI,
		EOLkey_FSI, INSERTorOVERkey_FSI
	     };
	     EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
				   HELPkey_FSI);
	     strcpyX (value, "", MAX_nonSTRING_VALUE_LEN);
	     PromptWindow (NEWpw, &PWt, 0, LogicalTRUE);
	     for (;;) {
		if (EnterPW(&PWt,ALLOCATERECShelpID)) {
		  long firstRecN, lastRecN;
		  if (sscanf(PWt.value,"%ld/%ld",&firstRecN,&lastRecN) == 2) {
		    firstRecN -= 1;
		    lastRecN -= 1;
		    status = CDFlib (SELECT_, VAR(Z), varN,
				     PUT_, VAR_ALLOCATEBLOCK(Z), firstRecN,
								 lastRecN,
				     NULL_);
		    if (ReportStatus(status,FALSE)) {
		      if (!BuildVarMoreMenu(Z,varN,&IW,useFormat,FALSE)) {
			PromptWindow (DELETEpw, &PWt);
			ItemWindow (DELETEiw, &IW);
			return FALSE;
		      }
		      ItemWindow (UPDATEiw, &IW, IW.itemN);
		      break;
		    }
		    if (NoMoreAccess(NULL)) {
		      PromptWindow (DELETEpw, &PWt);
		      ItemWindow (DELETEiw, &IW);
		      return FALSE;
		    }
		  }
		  else
		    ProblemWindow ("Error decoding first/last records.",FALSE);
		}
		else
		  break;
	     }
	     PromptWindow (DELETEpw, &PWt);
	     break;
	   }
	   /*******************************************************************
	   * Write initial records.
	   *******************************************************************/
	   case INITIALrecordsIN: {
	     static char *header[] = { "Enter number of initial records..." };
	     AOSs1 (trailer,
		    "Enter: ________   Exit: ________  Help: ________")
	     static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI,
					HELPkey_FSI, NUL };
	     static char value[MAX_nonSTRING_VALUE_LEN+1];
	     static struct PromptWindowStruct PWt = {
		NULL, 14, 0, 80, 1, header, MAX_nonSTRING_VALUE_LEN, value,
		1, trailer, exitChars, REFRESHkey_FSI, SOLkey_FSI,
		EOLkey_FSI, INSERTorOVERkey_FSI
	     };
	     EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
				   HELPkey_FSI);
	     strcpyX (value, "", MAX_nonSTRING_VALUE_LEN);
	     PromptWindow (NEWpw, &PWt, 0, LogicalTRUE);
	     for (;;) {
		if (EnterPW(&PWt,INITIALRECShelpID)) {
		  long nRecords;
		  if (sscanf(PWt.value,"%ld",&nRecords) == 1) {
		    status = CDFlib (SELECT_, VAR(Z), varN,
				     PUT_, VAR_INITIALRECS(Z), nRecords,
				     NULL_);
		    if (ReportStatus(status,FALSE)) {
		      if (!BuildVarMoreMenu(Z,varN,&IW,useFormat,FALSE)) {
		        PromptWindow (DELETEpw, &PWt);
		        ItemWindow (DELETEiw, &IW);
			return FALSE;
		      }
		      ItemWindow (UPDATEiw, &IW, IW.itemN);
		      *changed = TRUE;
		      break;
		    }
		    if (NoMoreAccess(NULL)) {
		      PromptWindow (DELETEpw, &PWt);
		      ItemWindow (DELETEiw, &IW);
		      return FALSE;
		    }
		  }
		  else
		    ProblemWindow ("Error decoding number of records.", FALSE);
		}
		else
		  break;
	     }
	     PromptWindow (DELETEpw, &PWt);
	     break;
	   }
	   /*******************************************************************
	   * Variable compression.
	   *******************************************************************/
	   case MODIFYcompressionIN: {
	     Logical changed = FALSE;
	     if (!SelectCompression(BOO(Z,FORzVAR,FORrVAR),14,&changed)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     if (changed) {
	       if (!BuildVarMoreMenu(Z,varN,&IW,useFormat,FALSE)) {
	         ItemWindow (DELETEiw, &IW);
	         return FALSE;
	       }
	       ItemWindow (UPDATEiw, &IW, IW.itemN);
	     }
	     break;
	   }
	   /*******************************************************************
	   * Variable sparseness.
	   *******************************************************************/
	   case MODIFYsparsenessIN: {
	     Logical changed = FALSE;
	     if (!SelectSparseness(Z,&changed)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     if (changed) {
	       if (!BuildVarMoreMenu(Z,varN,&IW,useFormat,FALSE)) {
	         ItemWindow (DELETEiw, &IW);
	         return FALSE;
	       }
	       ItemWindow (UPDATEiw, &IW, IW.itemN);
	     }
	     break;
	   }
	   /*******************************************************************
	   * Delete records.
	   *******************************************************************/
	   case DELETErecords1IN:
	   case DELETErecords2IN: {
	     static char *header1[] = {
	       "Enter first/last records to delete.  Example: 1/100",
               "No record renumbering after the deletion for sparse variable."
	     };
	     static char *header2[] = {
	       "Enter first/last records to delete.  Example: 1/100",
               "Record is renumbered after the deletion for sparse variable."
	     };
	     AOSs1 (trailer,"Enter: ________   Exit: ________  Help: ________")
	     static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI,
					HELPkey_FSI, NUL };
	     static char value[MAX_nonSTRING_VALUE_LEN+1];
	     static struct PromptWindowStruct PWt = {
		NULL, 14, 0, 80, 2, NULL, MAX_nonSTRING_VALUE_LEN, value,
		1, trailer, exitChars, REFRESHkey_FSI, SOLkey_FSI,
		EOLkey_FSI, INSERTorOVERkey_FSI
	     };
             if (IW.itemN == DELETErecords1IN)
               PWt.hLines = header1;
             else
               PWt.hLines = header2;
	     EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
				   HELPkey_FSI);
	     strcpyX (value, "", MAX_nonSTRING_VALUE_LEN);
	     PromptWindow (NEWpw, &PWt, 0, LogicalTRUE);
	     for (;;) {
		if (EnterPW(&PWt,DELETERECShelpID)) {
		  long firstRecN, lastRecN;
		  if (sscanf(PWt.value,"%ld/%ld",&firstRecN,&lastRecN) == 2) {
		    firstRecN -= 1;
		    lastRecN -= 1;
		    status = CDFlib (SELECT_, VAR(Z), varN,
				     DELETE_, (IW.itemN==DELETErecords1IN?
					       VAR_RECORDS(Z):
					       VAR_RECORDS_RENUMBER(Z)),
					      firstRecN, lastRecN,
				     NULL_);
		    if (ReportStatus(status,FALSE)) {
		      if (!BuildVarMoreMenu(Z,varN,&IW,useFormat,FALSE)) {
		        PromptWindow (DELETEpw, &PWt);
		        ItemWindow (DELETEiw, &IW);
			return FALSE;
		      }
		      ItemWindow (UPDATEiw, &IW, IW.itemN);
		      break;
		    }
		    if (NoMoreAccess(NULL)) {
		      PromptWindow (DELETEpw, &PWt);
		      ItemWindow (DELETEiw, &IW);
		      return FALSE;
		    }
		  }
		  else
		    ProblemWindow ("Error decoding first/last records.",FALSE);
		}
		else
		  break;
	     }
	     PromptWindow (DELETEpw, &PWt);
	     break;
	   }
	 }
	 break;
       }
       /***********************************************************************
       * Online help.
       ***********************************************************************/
       case HELPkey_FSI:
	 OnlineHelpWindow (ilhFile, VARMOREhelpID);
	 break;
       /***********************************************************************
       * Next variable.
       ***********************************************************************/
       case NEXTVARkey_EDIT:
	 varN = (varN + 1) % nVars;
	 if (!BuildVarMoreMenu(Z,varN,&IW,useFormat,FALSE)) {
	   ItemWindow (DELETEiw, &IW);
	   return FALSE;
	 }
	 ItemWindow (UPDATEiw, &IW, IW.itemN);
	 break;
       /***********************************************************************
       * Exit menu.
       ***********************************************************************/
       case EXITkey_FSI:
	 ItemWindow (DELETEiw, &IW);
	 return TRUE;
     }
   }
}

/******************************************************************************
* BuildVarMoreMenu.
******************************************************************************/

Logical BuildVarMoreMenu (Z, varN, IW, useFormat, center)
Logical Z;
long varN;
struct ItemWindowStruct *IW;
Logical useFormat;
Logical center;		/* Center messages displayed? */
{
  CDFstatus status; char varName[CDF_VAR_NAME_LEN256+1];
  long dataType, numElems, maxRec, maxAllocRec, nAllocRecs, blockingFactor;
  long nIndexRecords, nIndexEntries, CDFformat, nRecords, nIndexLevels;
  long numDims, dimSizes[CDF_MAX_DIMS], recVary, dimVarys[CDF_MAX_DIMS];
  long cType, cParms[CDF_MAX_PARMS], cPct;
  long sRecordsType, sArraysType, sArraysParms[CDF_MAX_PARMS], sArraysPct;
  void *padBinary; int pad, maxWidth; char *format, delim;
  int style;
  /****************************************************************************
  * Inquire variable.
  ****************************************************************************/
  status = CDFlib (SELECT_, VAR(Z), varN,
		   GET_, CDF_FORMAT_, &CDFformat,
			 VAR_NAME(Z), varName,
			 VAR_DATATYPE(Z), &dataType,
			 VAR_NUMELEMS(Z), &numElems,
			 BOO(Z,zVAR_NUMDIMS_,rVARs_NUMDIMS_), &numDims,
			 BOO(Z,zVAR_DIMSIZES_,rVARs_DIMSIZES_), dimSizes,
			 VAR_RECVARY(Z), &recVary,
			 VAR_DIMVARYS(Z), dimVarys,
		   NULL_);
  if (!ReportStatus(status,center)) return FALSE;
  status = CDFlib (GET_, VAR_MAXallocREC(Z), &maxAllocRec,
			 VAR_NUMallocRECS(Z), &nAllocRecs,
			 VAR_MAXREC(Z), &maxRec,
			 VAR_NUMRECS(Z), &nRecords,
			 VAR_BLOCKINGFACTOR(Z), &blockingFactor,
			 VAR_COMPRESSION(Z), &cType, cParms, &cPct,
			 VAR_SPARSERECORDS(Z), &sRecordsType,
			 VAR_SPARSEARRAYS(Z), &sArraysType,
					      sArraysParms,
					      &sArraysPct,
		   NULL_);
  if (!ReportStatus(status,center)) return FALSE;
  if (CDFformat == SINGLE_FILE) {
    status = CDFlib (GET_, VAR_nINDEXRECORDS(Z), &nIndexRecords,
			   VAR_nINDEXENTRIES(Z), &nIndexEntries,
			   VAR_nINDEXLEVELS(Z), &nIndexLevels,
		     NULL_);
    if (!ReportStatus(status,center)) return FALSE;
  }
  /****************************************************************************
  * Encode label.
  ****************************************************************************/
  delim = PickDelimiter (varName, strlen(varName));
  snprintf (IW->label, (size_t) 12+CDF_VAR_NAME_LEN256+9+1,
	    " %cVariable %c%s%c `More' ", BOO(Z,'z','r'), delim,
	    varName, delim);
  /****************************************************************************
  * Encode 1st header line.
  ****************************************************************************/
  snprintf (IW->hLines[0], (size_t) sizeof(BLANKs78),
	    "  DataSpec: %s/%ld", DataTypeToken(dataType),
	    numElems);
  pad = VAR_MORE_MENU_1st_HALF_LEN - strlen(IW->hLines[0]);
  if (pad > 0) CatNcharacters (IW->hLines[0], pad, (int) ' ');
  strcatX (IW->hLines[0], "Dimensionality: ", 0);
  EncodeDimensionality (EofS(IW->hLines[0]), numDims, dimSizes,
		        (size_t) sizeof(BLANKs78)-strlen(IW->hLines[0]));
  strcatX (IW->hLines[0], BOO(Z," (z)"," (r)"), 0);
  /****************************************************************************
  * Encode 2nd header line.
  ****************************************************************************/
  snprintf (IW->hLines[1], (size_t) sizeof(BLANKs78),
	    "   Records: %ldn/%ldx", nRecords, maxRec + 1);
  pad = VAR_MORE_MENU_1st_HALF_LEN - strlen(IW->hLines[1]);
  if (pad > 0) CatNcharacters (IW->hLines[1], pad, (int) ' ');
  strcatX (IW->hLines[1], "     Variances: ", 0);
  EncodeVariances (EofS(IW->hLines[1]), recVary, numDims, dimVarys);
  /****************************************************************************
  * Encode 3rd header line.
  ****************************************************************************/
  snprintf (IW->hLines[2], (size_t) sizeof(BLANKs78),
	    "  Blocking: %ld (record%s)", blockingFactor,
	    (blockingFactor <= 1 ? "" : "s"));
  pad = VAR_MORE_MENU_1st_HALF_LEN - strlen(IW->hLines[2]);
  if (pad > 0) CatNcharacters (IW->hLines[2], pad, (int) ' ');
  snprintf (EofS(IW->hLines[2]),
	    (size_t) sizeof(BLANKs78)-strlen(IW->hLines[2]),
	    "     Allocated: %ldn/%ldx (records)",
	    nAllocRecs, maxAllocRec + 1);
  /****************************************************************************
  * Encode 4th header line.
  ****************************************************************************/
  snprintf (IW->hLines[3], (size_t) sizeof(BLANKs78),
	    "Sparseness: %s",
	    SparsenessToken(sRecordsType,sArraysType,sArraysParms));
  pad = VAR_MORE_MENU_1st_HALF_LEN - strlen(IW->hLines[3]);
  if (pad > 0) CatNcharacters (IW->hLines[3], pad, (int) ' ');
  snprintf (EofS(IW->hLines[3]),
	    (size_t) sizeof(BLANKs78)-strlen(IW->hLines[3]),
	    "   Compression: %s", CompressionToken(cType,cParms));
  if (cPct == 0) cPct = 1;
  if (cType != NO_COMPRESSION && cPct > 0) {
    snprintf (EofS(IW->hLines[3]), (size_t) sizeof(BLANKs78), " (saving: %d%%)",
              (int)(cPct<=100?(100-cPct):0));
  }
  /****************************************************************************
  * Encode 5th header line.
  ****************************************************************************/
  if (useFormat) {
    status = GetFormatEntry (Z, varN, &format);
    if (!ReportStatus(status,center)) return FALSE;
  }
  else
    format = NULL;
  strcpyX (IW->hLines[4], " Pad value: ", 0);
  padBinary = cdf_AllocateMemory ((size_t) (numElems * CDFelemSize(dataType)),
				  FatalError);
  status = CDFlib (GET_, VAR_PADVALUE(Z), padBinary,
		   NULL_);
  if (TT2000dataType(dataType)) style = TT2000_3_STYLE;
  else style = EPOCH0_STYLE;
  switch (status) {
    case NO_PADVALUE_SPECIFIED: {
      static char defaultT[] = " (default)";
      maxWidth = (IW->nColsTotal - 2) - strlen(IW->hLines[4]) -
		  strlen(defaultT);
      if (STRINGdataType(dataType)) {
        int ix;
        for (ix = 0; ix < (int) numElems; ++ix)
           *(((char *) padBinary)+ix) = (char) ' ';
      }
      EncodeValuesFormat (dataType, numElems, padBinary, EofS(IW->hLines[4]),
			  NULL, 0, maxWidth, style,
		          (size_t) sizeof(BLANKs78)-strlen(IW->hLines[4]));
      strcatX (IW->hLines[4], defaultT, 0);
      break;
    }
    default:
      if (!ReportStatus(status,center)) return FALSE;
      maxWidth = (IW->nColsTotal - 2) - strlen(IW->hLines[4]);
      EncodeValuesFormat (dataType, numElems, padBinary, EofS(IW->hLines[4]),
			  NULL, 0, maxWidth, style,
		          (size_t) sizeof(BLANKs78)-strlen(IW->hLines[4]));
      break;
  }
  cdf_FreeMemory (padBinary, FatalError);
  if (format != NULL) cdf_FreeMemory (format, FatalError);
  /****************************************************************************
  * Encode 6th header line.
  ****************************************************************************/
  if (CDFformat == SINGLE_FILE) {
    snprintf (IW->hLines[5], (size_t) sizeof(BLANKs78),
	      "  Indexing: %ld record%s, %ld entr%s, %ld level%s",
	      nIndexRecords, BOO(nIndexRecords <= 1,"","s"),
	      nIndexEntries, BOO(nIndexEntries <= 1,"y","ies"),
	      nIndexLevels, BOO(nIndexLevels <= 1,"","s"));
  }
  else
    snprintf (IW->hLines[5], (size_t) sizeof(BLANKs78), " Indexing: n/a");
  return TRUE;
}

/******************************************************************************
* CreateVar.
*     Returns FALSE is a fatal error is encountered - TRUE otherwise.  The
* variable not being created correctly is NOT considered a fatal error (eg.,
* an illegal name could have been specified).
******************************************************************************/

Logical CreateVar (Z, created)
Logical Z;
Logical *created;       /* Set to TRUE if a variable is created. */
{
  CDFstatus status;
  char varName[CDF_VAR_NAME_LEN256+1];
  long recVary, dimVarys[CDF_MAX_DIMS];
  long numDims, dimSizes[CDF_MAX_DIMS];
  long dataType, numElems;
  long varNum;
  static char labelOthers[19+CDF_VAR_NAME_LEN256+1], delim;
  AOSs1 (trailerEnter1, "Enter: ________   Exit: ________  Help: ________")
  AOSs2A (trailerEnter2, "Enter: ________   Exit: ________", "Help: ________")
  AOSs2 (trailerSelect2, "Select: ________   Exit: ________", "Help: ________")
  static int exitCharsEnter[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI,
				  NUL };
  static char *headerName[] = {
    "Enter name (with delimiters)...",
    "Syntax: <delim><char1><char2>...<charN><delim>",
    "Example: \"EPOCH\""
  };
  static char valueName[DELIMed_VAR_NAME_LEN+1];
  static char labelName[15+1];
  static struct PromptWindowStruct PWname = {
    labelName, 4, 1, 78, 3, headerName, 1+CDF_VAR_NAME_LEN256+1, valueName,
    1, trailerEnter1, exitCharsEnter, REFRESHkey_FSI, SOLkey_FSI,
    EOLkey_FSI, INSERTorOVERkey_FSI
  };
  static char *headerDim[] = {
    "Enter dimensionality...",
    "Syntax: <numDims>:[<dimSize1>,<dimSize2>,...,<dimSizeN>]",
    "Examples: 0:[], 1:[5], 2:[100,200], 3:[10,20,30]"
  };
  static char valueDim[MAX_DIMENSIONALITY_LEN+1];
  static struct PromptWindowStruct PWdim = {
    labelOthers, 4, 1, 78, 3, headerDim, MAX_DIMENSIONALITY_LEN, valueDim,
    1, trailerEnter1, exitCharsEnter, REFRESHkey_FSI, SOLkey_FSI,
    EOLkey_FSI, INSERTorOVERkey_FSI
  };
  static char *headerVary[] = {
    "Enter variances...",
    "Syntax: <recVary>/<dimVary1><dimVary2>...<dimVaryN>",
    "Examples: T/, T/F, F/TF, T/TTT"
  };
  static char valueVary[MAX_VARIANCES_LEN+1];
  static struct PromptWindowStruct PWvary = {
    labelOthers, 4, 1, 78, 3, headerVary, 0, valueVary, 1, trailerEnter1,
    exitCharsEnter, REFRESHkey_FSI, SOLkey_FSI, EOLkey_FSI,
    INSERTorOVERkey_FSI
  };
  /****************************************************************************
  * Set up.
  ****************************************************************************/
  *created = FALSE;
  EncodeKeyDefinitions (1, trailerEnter1,
			ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI);
  EncodeKeyDefinitions (2, trailerEnter2,
			ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI);
  EncodeKeyDefinitions (2, trailerSelect2,
			ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI);
  /****************************************************************************
  * Prompt for name.
  ****************************************************************************/
  strcpyX (PWname.value, "\"\"", DELIMed_VAR_NAME_LEN);
  snprintf (labelName, (size_t) sizeof(labelName),
	    " New %cVariable ", BOO(Z,'z','r'));
  PromptWindow (NEWpw, &PWname, 1, LogicalTRUE);
  for (;;) {
     if (EnterPW(&PWname,VARNAMEhelpID)) {
       if (DecodeDelimitedString(PWname.value,varName))
	 break;
       else
	 ProblemWindow ("Illegal name (check delimiters).", FALSE);
     }
     else {
       strcpyX (varName, "", CDF_VAR_NAME_LEN256);
       break;
     }
  }
  PromptWindow (DELETEpw, &PWname);
  if (strlen(varName) == 0) return TRUE;
  delim = PickDelimiter (varName, strlen(varName));
  snprintf (labelOthers, (size_t) sizeof(labelOthers),
	    " New %cVariable %c%s%c ", BOO(Z,'z','r'),
	    delim, varName, delim);
  /****************************************************************************
  * Prompt for data type/number of elements.
  ****************************************************************************/
  dataType = NO_DATATYPE;
  if (!SelectDataSpec(&dataType,&numElems,labelOthers)) return TRUE;
  /****************************************************************************
  * If zVariable, prompt for number/sizes of dimensions.  If rVariable,
  * determine number of dimensions.
  ****************************************************************************/
  if (Z) {
    strcpyX (PWdim.value, "", MAX_DIMENSIONALITY_LEN);
    PromptWindow (NEWpw, &PWdim, 0, LogicalTRUE);
    for (;;) {
       if (EnterPW(&PWdim,ZDIMhelpID)) {
	 if (DecodeDimensionality(PWdim.value,&numDims,dimSizes))
	   break;
	 else
	   ProblemWindow ("Illegal dimensionality.", FALSE);
       }
       else {
	 numDims = -1;
	 break;
       }
    }
    PromptWindow (DELETEpw, &PWdim);
    if (numDims == -1) return TRUE;
  }
  else {
    status = CDFlib (GET_, rVARs_NUMDIMS_, &numDims,
			   rVARs_DIMSIZES_, dimSizes,
		     NULL_);
    if (!ReportStatus(status,FALSE)) return FALSE;
  }
  /****************************************************************************
  * Prompt for variances.
  ****************************************************************************/
  PWvary.maxChars = (int) (1 + 1 + numDims);
  strcpyX (PWvary.value, "", MAX_VARIANCES_LEN);
  PromptWindow (NEWpw, &PWvary, 0, LogicalTRUE);
  for (;;) {
     if (EnterPW(&PWvary,VARVARYShelpID)) {
       if (DecodeVariances(PWvary.value,numDims,&recVary,dimVarys))
	 break;
       else 
	 ProblemWindow ("Illegal variances.", FALSE);
     }
     else {
       recVary = -2;
       break;
     }
  }
  PromptWindow (DELETEpw, &PWvary);
  if (recVary == -2) return TRUE;
  /****************************************************************************
  * Create variable.
  ****************************************************************************/
  if (Z)
    status = CDFlib (CREATE_, zVAR_, varName, dataType, numElems, numDims,
				     dimSizes, recVary, dimVarys, &varNum,
		     NULL_);
  else
    status = CDFlib (CREATE_, rVAR_, varName, dataType, numElems, recVary,
				     dimVarys, &varNum,
		     NULL_);
  if (ReportStatus(status,FALSE)) {
    *created = TRUE;
    return TRUE;
  }
  if (NoMoreAccess(NULL)) return FALSE;
  return TRUE;
}

/******************************************************************************
* ModPadValue.
******************************************************************************/

static Logical ModPadValue (zVar, varN, useFormat, changed)
Logical zVar;
long varN;
Logical useFormat;
Logical *changed;	/* Assumed to be initialized to FALSE by caller. */
{
  long dataType, numElems; size_t nBytes; int cursorAt;
  void *binary; char *format; CDFstatus status;
  static char *headerBrowse[] = { "Full pad value..." };
  static char *headerNoCHAR[] = { "Enter new pad value..." };
  static char *headerCHAR[] = {"Enter new pad value (delimiters required)..."};
  AOSs1A (trailerBrowse, "Exit: ________  Help: ________")
  AOSs1B (trailerEdit, "Enter: ________   Exit: ________  Help: ________")
  static int exitCharsBrowse[] = { EXITkey_FSI, HELPkey_FSI, NUL };
  static int exitCharsEdit[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL };
  static struct PromptWindowStruct PWt = {
    NULL, 14, 0, 80, 1, NULL, 0, NULL, 1, NULL, NULL, REFRESHkey_FSI,
    SOLkey_FSI, EOLkey_FSI, INSERTorOVERkey_FSI
  };
  int style;
  status = CDFlib (SELECT_, VAR(zVar), varN,
		   GET_, VAR_DATATYPE(zVar), &dataType,
			 VAR_NUMELEMS(zVar), &numElems,
		   NULL_);
  if (!ReportStatus(status,FALSE)) return FALSE;
  nBytes = (size_t) (CDFelemSize(dataType) * numElems);
  binary = cdf_AllocateMemory (nBytes, FatalError);
  status = CDFlib (GET_, VAR_PADVALUE(zVar), binary,
		   NULL_);
  switch (status) {
    case NO_PADVALUE_SPECIFIED:
      if (STRINGdataType(dataType)) {
        int ix;
        for (ix = 1; ix < numElems; ++ix) *(((char *)binary)+ix) = ' ';
      }
      break;
    default:
      if (!ReportStatus(status,FALSE)) return FALSE;
      break;
  }
  if (browseOnly) {
    EncodeKeyDefinitions (1, trailerBrowse, EXITkey_FSI, HELPkey_FSI);
    PWt.hLines = headerBrowse;
    PWt.tLines = trailerBrowse;
    PWt.exitChars = exitCharsBrowse;
  }
  else {
    EncodeKeyDefinitions (1, trailerEdit, ENTERkey_FSI, EXITkey_FSI,
			  HELPkey_FSI);
    PWt.hLines = BOO(STRINGdataType(dataType),headerCHAR,headerNoCHAR);
    PWt.tLines = trailerEdit;
    PWt.exitChars = exitCharsEdit;
  }
  PWt.maxChars = BOO(STRINGdataType(dataType),
		     (int)(numElems+3),MAX_nonSTRING_VALUE_LEN);
  nBytes = (size_t) (PWt.maxChars + 1);
  PWt.value = (char *) cdf_AllocateMemory (nBytes, FatalError);
  if (useFormat) {
    status = GetFormatEntry (zVar, varN, &format);
    if (!ReportStatus(status,FALSE)) return FALSE;
  }
  else
    format = NULL;
  if (TT2000dataType(dataType)) style = TT2000_3_STYLE;
  else style = EPOCH0_STYLE;
  if ((dataType == CDF_FLOAT) || (dataType == CDF_REAL4)) {
    if (!isnan((double)*(float *)binary) &&
        !isinf((double)*(float *)binary) &&
        *(float *)binary <= DEFAULT_FLOAT_PADVALUE) {
      EncodeValuesFormat (dataType, numElems, binary, PWt.value, NULL, 0,
                          (int) nBytes-1, style, (size_t) nBytes);
    } else
      EncodeValuesFormat (dataType, numElems, binary, PWt.value, format, 0,
                          (int) nBytes-1, style, (size_t) nBytes);
  } else if ((dataType == CDF_DOUBLE) || (dataType == CDF_REAL8)) {
    if (!isnan(*(double *)binary) && !isinf(*(double *)binary) &&
        *(double *)binary <= DEFAULT_DOUBLE_PADVALUE) {
      EncodeValuesFormat (dataType, numElems, binary, PWt.value, NULL, 0,
                          (int) nBytes-1, style, (size_t) nBytes);
    } else
      EncodeValuesFormat (dataType, numElems, binary, PWt.value, format, 0,
                          (int) nBytes-1, style, (size_t) nBytes);
  } else
    EncodeValuesFormat (dataType, numElems, binary, PWt.value, format, 0, 
		        (int) nBytes-1, style, (size_t) nBytes);
  cdf_FreeMemory (binary, FatalError);
  if (format != NULL) cdf_FreeMemory (format, FatalError);
  cursorAt = BOO(STRINGdataType(dataType),
		 strlen(PWt.value) - 1,strlen(PWt.value));
  PromptWindow (NEWpw, &PWt, cursorAt, LogicalTRUE);
  for (;;) {
     if (EnterPW(&PWt,PADVALUEhelpID)) {
       long nElemsEntered; void *newBinary;
       if (DecodeValues(PWt.value,dataType,
			&nElemsEntered,&newBinary,style)) {
	 if (nElemsEntered == numElems) {
	   status = CDFlib (PUT_, VAR_PADVALUE(zVar), newBinary,
			    NULL_);
	   if (ReportStatus(status,FALSE)) {
	     PromptWindow (DELETEpw, &PWt);
	     cdf_FreeMemory (newBinary, FatalError);
	     cdf_FreeMemory (PWt.value, FatalError);
	     *changed = TRUE;
	     return TRUE;
	   }
	   if (NoMoreAccess(NULL)) {
	     PromptWindow (DELETEpw, &PWt);
	     return FALSE;
	   }
	   cdf_FreeMemory (newBinary, FatalError);
	 }
	 else {
	   static char tooMany[] = "Illegal number of elements (too many).";
	   static char tooFew[] = "Illegal number of elements (too few).";
	   ProblemWindow (BOO(nElemsEntered > numElems,tooMany,tooFew), FALSE);
	   cdf_FreeMemory (newBinary, FatalError);
	 }
       }
       else
	 ProblemWindow ("Illegal value.", FALSE);
     }
     else {
       PromptWindow (DELETEpw, &PWt);
       cdf_FreeMemory (PWt.value, FatalError);
       return TRUE;
     }
  }
}

/******************************************************************************
* ModPadValue.
******************************************************************************/

static Logical RegularVar (zVar, varN)
Logical zVar;
long varN;
{
  long sr;
  CDFstatus status;
  status = CDFlib (SELECT_, VAR(zVar), varN,
		   GET_, VAR_SPARSERECORDS(zVar), &sr,
		   NULL_);
  if (!ReportStatus(status,FALSE)) return FALSE;
  if (sr == NO_SPARSERECORDS) return TRUE;
  else return FALSE;
}
