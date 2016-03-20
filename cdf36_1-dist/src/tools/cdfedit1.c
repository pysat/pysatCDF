/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                            CDFedit, part 1 (attributes/entries).
*
*  Version 1.3c, 16-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  24-Jan-94, J Love     Original version.
*   V1.0a  4-Feb-94, J Love     DEC Alpha/OpenVMS port.
*   V1.0b 22-Feb-94, J Love     Spelling lesson.
*   V1.1  15-Dec-94, J Love     CDF V2.5.
*   V1.1a 23-Jan-95, J Love	IRIX 6.x (64-bit).
*   V1.1b 28-Feb-95, J Love	Solaris 2.3.  Pass `char' as `int'.
*   V1.2  11-Apr-95, J Love	POSIX.
*   V1.2a 11-May-95, J Love	EPOCH styles.
*   V1.2b  9-Jun-95, J Love	catchrX.
*   V1.2c  6-Sep-95, J Love	CDFexport-related changes.  FSI key
*				definitions.
*   V1.3  30-Sep-96, J Love	CDF V2.6.
*   V1.3a 19-Dec-96, J Love	Display error if attempt to browse/edit a
*				vAttribute's entries in "text mode" is made.
*   V1.3b  2-Sep-97, J Love	Fixed attribute name declaration.
*   V1.3c 16-Nov-97, J Love	Windows NT/Visual C++.
*   V3.3  04-Apr-11, M Liu      Modified to handle TT2000 epoch style string.
*
******************************************************************************/

#include "cdfedit.h"

/******************************************************************************
* EditAttrs.
******************************************************************************/

Logical EditAttrs (G, CDFname)
Logical G;                      /* If TRUE, global scoped attributes. */
char *CDFname;                  /* Name of the CDF being edited. */
{
   CDFstatus status; int attrX, entryX, entryType;
   long attrN, entryN, fieldN, *nEntries, *attrNs, **entryNs;
   long nAttrs;                 /* Number of attributes of the proper scope. */
   AOSs2 (header, BLANKs78, BLANKs78)
   AOSs1 (trailerBrowse,
	  "Select: ________   Exit: ________   Help: ________")
   AOSs3 (trailerEdit,
"Select: ________ Create attribute: ________ Delete attribute/entry: ________",
"Help:   ________ Create entry:     ________",
"Exit:   ________ Toggle scope:     ________")
   static int exitCharsBrowse[] = {
     ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL
   };
   static int exitCharsEdit[] = {
     ENTERkey_FSI, CREATEATTRkey_EDIT, EXITkey_FSI, HELPkey_FSI,
     CREATEENTRYkey_EDIT, TOGGLESCOPEkey_EDIT, DELETEATTRorENTRYkey_EDIT, NUL
   };
   static char label[6+DU_MAX_NAME_LEN+14+1];
   static struct ItemWindowStruct IW = {
      0, 0, 80, label, 2, header, 0, NULL, 0, NULL, NULL, NULL, 0, 0, NULL,
      NULL, REFRESHkey_FSI, TRUE, NSkey_FSI, PSkey_FSI
   };
   static Logical first = TRUE;
   /***************************************************************************
   * First time...
   ***************************************************************************/
   if (first) {
     EncodeKeyDefinitions (1, trailerBrowse, ENTERkey_FSI, EXITkey_FSI,
			     HELPkey_FSI);
     EncodeKeyDefinitions (3, trailerEdit, ENTERkey_FSI,
			     CREATEATTRkey_EDIT, DELETEATTRorENTRYkey_EDIT,
			     HELPkey_FSI, CREATEENTRYkey_EDIT, EXITkey_FSI,
			     TOGGLESCOPEkey_EDIT);
     first = FALSE;
   }
	 if (browseOnly) {
       IW.NiRows = 14;
       IW.NtLines = 1;
       IW.tLines = trailerBrowse;
       IW.exitChars = exitCharsBrowse;
     }
     else {
       IW.NiRows = 12;
       IW.NtLines = 3;
       IW.tLines = trailerEdit;
       IW.exitChars = exitCharsEdit;
     }
   /***************************************************************************
   * Build attribute/entry lines and display menu.
   ***************************************************************************/
   if (!BuildAttrMenu(G,CDFname,&nAttrs,&attrNs,
		      &nEntries,&entryNs,&IW)) return FALSE;
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
	 int nameItemN;
	 /*********************************************************************
	 * Check if any attributes/entries exist.
	 *********************************************************************/
	 if (IW.nItems == 0) {
	   ProblemWindow (BOO(G,"Nothing selected (no gAttributes exist).",
			        "Nothing selected (no vAttributes exist)."),
			  FALSE);
	   break;
	 }
	 /*********************************************************************
	 * Calculate attribute, entry, and field numbers.
	 *********************************************************************/
	 for (attrX = 0, nameItemN = 0; attrX < nAttrs; attrX++) {
	    int nItems = (int) (1 + (4 * nEntries[attrX]));
	    if (IW.itemN < nameItemN + nItems) {
	      int itemNt = IW.itemN - nameItemN;
	      attrN = attrNs[attrX];
	      if (itemNt == 0)
		fieldN = 0;
	      else {
		fieldN = ((itemNt - 1) % 4) + 1;
		entryX = (itemNt - 1) / 4;
		entryN = entryNs[attrX][entryX];
		if (G)
		  entryType = gENTRYt;
		else {
		  long NrEntries;
		  status = CDFlib (SELECT_, ATTR_, attrN,
				   GET_, ATTR_NUMrENTRIES_, &NrEntries,
				   NULL_);
		  if (!ReportStatus(status,FALSE)) {
		    ItemWindow (DELETEiw, &IW);
		    return FALSE;
		  }
		  entryType = (entryX < NrEntries ? rENTRYt : zENTRYt);
		}
	      }
	      break;
	    }
	    nameItemN += nItems;
	 }
	 /*********************************************************************
	 * Perform desired function (based on field number).
	 *********************************************************************/
	 switch (fieldN) {
	   /*******************************************************************
	   * Modify attribute name.
	   *******************************************************************/
	   case 0: {
	     char attrName[CDF_ATTR_NAME_LEN256+1];
	     status = CDFlib (SELECT_, ATTR_, attrN,
			      GET_, ATTR_NAME_, attrName,
			      NULL_);
	     if (!ReportStatus(status,FALSE)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     if (browseOnly)
	       ShowFullName (attrName, TRUE);
	     else {
	       static char *header[] = {
	         "Enter new name (with delimiters)...",
	         "Syntax: <delim><char1><char2>...<charN><delim>",
	         "Example: \"FILTERs\""
	       };
	       AOSs1 (trailer,
		      "Enter: ________   Exit: ________  Help: ________")
	       static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI,
					  HELPkey_FSI, NUL };
	       static char value[1+CDF_ATTR_NAME_LEN256+1+1];
	       static char label[15+CDF_ATTR_NAME_LEN256+1];
	       static struct PromptWindowStruct PWt = {
		  label, 4, 1, 78, 3, header, CDF_ATTR_NAME_LEN256+2, value,
		  1, trailer, exitChars, REFRESHkey_FSI, SOLkey_FSI,
		  EOLkey_FSI, INSERTorOVERkey_FSI
	       };
	       char delim;
	       EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
				     HELPkey_FSI);
	       delim = PickDelimiter (attrName, strlen(attrName));
	       snprintf (value, (size_t) sizeof(value), 
			 "%c%s%c", delim, attrName, delim);
	       snprintf (label, (size_t) sizeof(label),
			 " %cAttribute %c%s%c ", BOO(G,'g','v'),
		         delim, attrName, delim);
	       PromptWindow (NEWpw, &PWt, (int) (strlen(PWt.value) - 1),
			     LogicalTRUE);
	       for (;;) {
		  if (EnterPW(&PWt,ATTRRENAMEhelpID)) {
		    char attrName[CDF_ATTR_NAME_LEN256+1];
		    if (DecodeDelimitedString(PWt.value,attrName)) {
		      status = CDFlib (PUT_, ATTR_NAME_, attrName,
				       NULL_);
		      if (ReportStatus(status,FALSE)) {
		        FreeAttrMenu (nAttrs, attrNs, nEntries, entryNs, &IW);
		        if (!BuildAttrMenu(G,CDFname,&nAttrs,&attrNs,
					   &nEntries,&entryNs,&IW)) {
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
		      ProblemWindow ("Illegal name (check delimiters).",
				     FALSE);
		  }
		  else
		    break;
	       }
	       PromptWindow (DELETEpw, &PWt);
	     }
	     break;
	   }
	   /*******************************************************************
	   * Modify variable name/global entry number.
	   *******************************************************************/
	   case 1: {
	     if (G)
	       ItemWindow (BEEPiw, &IW);
	     else {
	       char varName[CDF_VAR_NAME_LEN256+1];
	       Logical Z = (entryType == zENTRYt); long nVars;
	       status = CDFlib (GET_, BOO(Z,CDF_NUMzVARS_,
					    CDF_NUMrVARS_), &nVars,
				NULL_);
	       if (!ReportStatus(status,FALSE)) {
		 ItemWindow (DELETEiw, &IW);
		 return FALSE;
	       }
	       if (nVars <= entryN) {
		 ProblemWindow (BOO(Z,"No corresponding zVariable.",
				      "No corresponding rVariable."), FALSE);
		 break;
	       }
	       status = CDFlib (SELECT_, VAR(Z), entryN,
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
		   "Example: \"Humidity\""
	         };
	         AOSs1 (trailer,
		        "Enter: ________   Exit: ________  Help: ________")
	         static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI,
					    HELPkey_FSI, NUL };
	         static char value[1+CDF_VAR_NAME_LEN256+1+1];
	         static char label[14+CDF_VAR_NAME_LEN256+1];
	         static struct PromptWindowStruct PWt = {
		    label, 4, 1, 78, 3, header, CDF_VAR_NAME_LEN256+2, value,
		    1, trailer, exitChars, REFRESHkey_FSI, SOLkey_FSI,
		    EOLkey_FSI, INSERTorOVERkey_FSI
	         };
		 char delim;
	         EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
				       HELPkey_FSI);
	         delim = PickDelimiter (varName, strlen(varName));
	         snprintf (value, (size_t) sizeof(value), 
			   "%c%s%c", delim, varName, delim);
	         snprintf (label, (size_t) sizeof(label), 
			   " %cVariable %c%s%c ", BOO(Z,'z','r'),
			   delim, varName, delim);
	         PromptWindow (NEWpw, &PWt, (int) (strlen(PWt.value) - 1),
			       LogicalTRUE);
	         for (;;) {
		    if (EnterPW(&PWt,VARRENAMEhelpID)) {
		      char varName[CDF_VAR_NAME_LEN256+1];
		      if (DecodeDelimitedString(PWt.value,varName)) {
		        status = CDFlib (PUT_, VAR_NAME(Z), varName,
				         NULL_);
		        if (ReportStatus(status,FALSE)) {
			  FreeAttrMenu (nAttrs, attrNs, nEntries, entryNs,
					&IW);
			  if (!BuildAttrMenu(G,CDFname,&nAttrs,&attrNs,
					     &nEntries,&entryNs,&IW)) {
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
		        ProblemWindow ("Illegal name (check delimiters).",
				       FALSE);
		    }
		    else
		      break;
	         }
	         PromptWindow (DELETEpw, &PWt);
	       }
	     }
	     break;
	   }
	   /*******************************************************************
	   * Modify data specification.
	   *******************************************************************/
	   case 2: {
	     if (browseOnly)
	       ItemWindow (BEEPiw, &IW);
	     else {
	       long dataType, numElems;
	       if (G)
	         snprintf (label, (size_t) sizeof(label), 
			   " gEntry %ld ", entryN + 1);
	       else {
	         Logical Z = (entryType == zENTRYt);
	         char varName[CDF_VAR_NAME_LEN256+1], delim;
	         status = CDFlib (SELECT_, VAR(Z), entryN,
				  GET_, VAR_NAME(Z), varName,
				  NULL_);
	         switch (status) {
		   case NO_SUCH_VAR:
		     snprintf (label, (size_t) sizeof(label),
			       " %cEntry %ld ",BOO(Z,'z','r'),entryN+1);
		     break;
		   default:
		     if (!ReportStatus(status,FALSE)) {
		       ItemWindow (DELETEiw, &IW);
		       return FALSE;
		     }
		     delim = PickDelimiter (varName, strlen(varName));
		     snprintf (label, (size_t) sizeof(label),
			       " %cEntry for %c%s%c ", BOO(Z,'z','r'),
			       delim, varName, delim);
		     break;
	         }
	       }
	       status = CDFlib (SELECT_, ATTR_, attrN,
				         ENTRY(entryType), entryN,
			        GET_, ENTRY_DATATYPE(entryType), &dataType,
			        NULL_);
	       if (!ReportStatus(status,FALSE)) {
		 ItemWindow (DELETEiw, &IW);
		 return FALSE;
	       }
	       if (SelectDataSpec(&dataType,NULL,label)) {
		 status = CDFlib (GET_, ENTRY_NUMELEMS(entryType), &numElems,
				  NULL_);
		 if (!ReportStatus(status,FALSE)) {
		   ItemWindow (DELETEiw, &IW);
		   return FALSE;
		 }
		 status = CDFlib (PUT_, ENTRY_DATASPEC(entryType), dataType,
								   numElems,
				  NULL_);
		 if (ReportStatus(status,FALSE)) {
		   FreeAttrMenu (nAttrs, attrNs, nEntries, entryNs, &IW);
		   if (!BuildAttrMenu(G,CDFname,&nAttrs,&attrNs,
				      &nEntries,&entryNs,&IW)) {
		     ItemWindow (DELETEiw, &IW);
		     return FALSE;
		   }
		   ItemWindow (UPDATEiw, &IW, IW.itemN);
		   break;
		 }
		 if (NoMoreAccess(NULL)) {
		   ItemWindow (DELETEiw, &IW);
		   return FALSE;
		 }
	       }
	     }
	     break;
	   }
	   /*******************************************************************
	   * Modify data specification/value(s).
	   *******************************************************************/
	   case 3: {
	     if (browseOnly)
	       ItemWindow (BEEPiw, &IW);
	     else {
	       Logical changed = FALSE;
	       if (!EditEntry(attrN,entryN,entryType,TRUE,&changed)) {
		 ItemWindow (DELETEiw, &IW);
		 return FALSE;
	       }
	       if (changed) {
	         FreeAttrMenu (nAttrs, attrNs, nEntries, entryNs, &IW);
	         if (!BuildAttrMenu(G,CDFname,&nAttrs,&attrNs,
				    &nEntries,&entryNs,&IW)) {
		   ItemWindow (DELETEiw, &IW);
		   return FALSE;
		 }
	         ItemWindow (UPDATEiw, &IW, IW.itemN);
	       }
	     }
	     break;
	   }
	   /*******************************************************************
	   * Modify value(s).
	   *******************************************************************/
	   case 4: {
	     Logical changed = FALSE;
	     if (!EditEntry(attrN,entryN,entryType,FALSE,&changed)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     if (changed) {
	       FreeAttrMenu (nAttrs, attrNs, nEntries, entryNs, &IW);
	       if (!BuildAttrMenu(G,CDFname,&nAttrs,&attrNs,
				  &nEntries,&entryNs,&IW)) {
		 ItemWindow (DELETEiw, &IW);
		 return FALSE;
	       }
	       ItemWindow (UPDATEiw, &IW, IW.itemN);
	     }
	     break;
	   }
	 }

	 break;
       }
       /***********************************************************************
       * Create new attribute.
       ***********************************************************************/
       case CREATEATTRkey_EDIT: {
	 static char *header[] = {
	   "Enter name (with delimiters)...",
	   "Syntax: <delim><char1><char2>...<charN><delim>",
	   "Example: \"Resolution\""
	 };
	 AOSs1 (trailer, "Enter: ________   Exit: ________  Help: ________")
	 static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI,
				    NUL };
	 static char value[DELIMed_ATTR_NAME_LEN+1];
	 static char label[16+1];
	 static struct PromptWindowStruct PWt = {
	    label, 4, 1, 78, 3, header, CDF_ATTR_NAME_LEN256+2, value, 1,
	    trailer, exitChars, REFRESHkey_FSI, SOLkey_FSI, EOLkey_FSI,
	    INSERTorOVERkey_FSI
	 };
	 EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
			       HELPkey_FSI);
	 strcpyX (value, "\"\"", DELIMed_ATTR_NAME_LEN);
	 snprintf (label, (size_t) sizeof(label), 
		   " New %cAttribute ", BOO(G,'g','v'));
	 PromptWindow (NEWpw, &PWt, 1, LogicalTRUE);
	 for (;;) {
	    if (EnterPW(&PWt,ATTRNAMEhelpID)) {
	      char attrName[CDF_ATTR_NAME_LEN256+1];
	      long attrNum;
	      if (DecodeDelimitedString(PWt.value,attrName)) {
		status = CDFlib (CREATE_, ATTR_, attrName,
						 BOO(G,GLOBAL_SCOPE,
						       VARIABLE_SCOPE),
						 &attrNum,
				 NULL_);
		if (ReportStatus(status,FALSE)) {
		  FreeAttrMenu (nAttrs, attrNs, nEntries, entryNs, &IW);
		  if (!BuildAttrMenu(G,CDFname,&nAttrs,&attrNs,
				     &nEntries,&entryNs,&IW)) {
		    ItemWindow (DELETEiw, &IW);
		    return FALSE;
		  }
		  ItemWindow (UPDATEiw, &IW, IW.nItems - 1);
		  break;
		}
		if (NoMoreAccess(NULL)) {
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
	 PromptWindow (DELETEpw, &PWt);
	 break;
       }
       /***********************************************************************
       * Create new entry.
       ***********************************************************************/
       case CREATEENTRYkey_EDIT: {
	 int nameItemN; Logical changed = FALSE;
	 if (IW.nItems == 0) {
	   ProblemWindow(BOO(G,"A gAttribute isn't selected (none exist).",
			       "A vAttribute isn't selected (none exist)."),
			 FALSE);
	   break;
	 }
	 for (attrX = 0, nameItemN = 0; attrX < nAttrs; attrX++) {
	    int nItems = (int) (1 + (4 * nEntries[attrX]));
	    if (IW.itemN < nameItemN + nItems) {
	      attrN = attrNs[attrX];
	      break;
	    }
	    nameItemN += nItems;
	 }
	 if (!CreateEntry(G,attrN,&changed)) {
	   ItemWindow (DELETEiw, &IW);
	   return FALSE;
	 }
	 if (changed) {
	   FreeAttrMenu (nAttrs, attrNs, nEntries, entryNs, &IW);
	   if (!BuildAttrMenu(G,CDFname,&nAttrs,&attrNs,
			      &nEntries,&entryNs,&IW)) {
	     ItemWindow (DELETEiw, &IW);
	     return FALSE;
	   }
	   ItemWindow (UPDATEiw, &IW, nameItemN);
	 }
	 break;
       }
       /***********************************************************************
       * Delete attribute/entry.
       ***********************************************************************/
       case DELETEATTRorENTRYkey_EDIT: {
	 int nameItemN;
	 Logical deleteAttr;
	 if (IW.nItems == 0) {
	   static char noG[] = {
	     "A gAttribute/gEntry isn't selected (none exist)."
	   };
	   static char noV[] = {
	     "A vAttribute/entry isn't selected (none exist)."
	   };
	   ProblemWindow (BOO(G,noG,noV), FALSE);
	   break;
	 }
	 for (attrX = 0, nameItemN = 0; attrX < nAttrs; attrX++) {
	    int nItems = (int) (1 + (4 * nEntries[attrX]));
	    if (IW.itemN < nameItemN + nItems) {
	      int itemNt = IW.itemN - nameItemN;
	      attrN = attrNs[attrX];
	      if (itemNt == 0)
		deleteAttr = TRUE;
	      else {
		deleteAttr = FALSE;
		entryX = (itemNt - 1) / 4;
		entryN = entryNs[attrX][entryX];
		if (G)
		  entryType = gENTRYt;
		else {
		  long NrEntries;
		  status = CDFlib (SELECT_, ATTR_, attrN,
				   GET_, ATTR_NUMrENTRIES_, &NrEntries,
				   NULL_);
		  if (!ReportStatus(status,FALSE)) {
		    ItemWindow (DELETEiw, &IW);
		    return FALSE;
		  }
		  entryType = (entryX < NrEntries ? rENTRYt : zENTRYt);
		}
	      }
	      break;
	    }
	    nameItemN += nItems;
	 }
	 if (deleteAttr) {
	   char attrName[CDF_ATTR_NAME_LEN256+1],
		question[22+CDF_ATTR_NAME_LEN256+1], delim;
	   status = CDFlib (SELECT_, ATTR_, attrN,
			    GET_, ATTR_NAME_, attrName,
			    NULL_);
	   if (!ReportStatus(status,FALSE)) {
	     ItemWindow (DELETEiw, &IW);
	     return FALSE;
	   }
	   delim = PickDelimiter (attrName, strlen(attrName));
	   snprintf (question, (size_t) sizeof(question), 
		     "Delete %sAttribute %c%s%c ?",
		     BOO(G,"g","v"), delim, attrName, delim);
	   if (ConfirmWindow(4,78,question,NULL,FALSE,DELETEATTRhelpID)) {
	     status = CDFlib (DELETE_, ATTR_,
			      NULL_);
	     if (ReportStatus(status,FALSE)) {
	       FreeAttrMenu (nAttrs, attrNs, nEntries, entryNs, &IW);
	       if (!BuildAttrMenu(G,CDFname,&nAttrs,&attrNs,
				  &nEntries,&entryNs,&IW)) {
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
	 }
	 else {
	   char attrName[CDF_ATTR_NAME_LEN256+1], delim = ' ',
		question1[40+ENTRYNUM_FIELD_LEN+CDF_ATTR_NAME_LEN256+1],
		question2[31+CDF_VAR_NAME_LEN256+1];
	   status = CDFlib (SELECT_, ATTR_, attrN,
			    GET_, ATTR_NAME_, attrName,
			    NULL_);
	   if (!ReportStatus(status,FALSE)) {
	     ItemWindow (DELETEiw, &IW);
	     return FALSE;
	   }
	   if (entryType == gENTRYt) {
	     delim = PickDelimiter (attrName, strlen(attrName));
	     snprintf (question1, (size_t) sizeof(question1), 
		       "Delete gEntry number %ld of gAttribute %c%s%c ?",
		       entryN + 1, delim, attrName, delim);
	     question2[0] = NUL;
	   }
	   else {
	     char varName[CDF_VAR_NAME_LEN256+1];
	     status = CDFlib (SELECT_, BOO(entryType==zENTRYt,zVAR_,
							      rVAR_), entryN,
			      GET_, BOO(entryType==zENTRYt,
					zVAR_NAME_,rVAR_NAME_), varName,
			      NULL_);
	     switch (status) {
	       case NO_SUCH_VAR:
		 snprintf (question1, (size_t) sizeof(question1), 
			   "Delete %sEntry number %ld of vAttribute %c%s%c ?",
			   BOO(entryType==zENTRYt,"z","r"), entryN + 1, delim,
			   attrName, delim);
		 question2[0] = NUL;
		 break;
	       default:
		 if (!ReportStatus(status,FALSE)) {
		   ItemWindow (DELETEiw, &IW);
		   return FALSE;
		 }
		 delim = PickDelimiter (attrName, strlen(attrName));
		 snprintf (question1, (size_t) sizeof(question1),
			   "Delete %sEntry of vAttribute %c%s%c",
			   BOO(entryType==zENTRYt,"z","r"), delim, attrName,
			   delim);
		 delim = PickDelimiter (varName, strlen(varName));
		 snprintf (question2, (size_t) sizeof(question2),
			   "corresponding to %sVariable %c%s%c ?",
			   BOO(entryType==zENTRYt,"z","r"), delim, varName,
			   delim);
		 break;
	     }
	   }
	   if (ConfirmWindow(4,78,question1,
			     BOO(NULstring(question2),NULL,question2),
			     FALSE,DELETEENTRYhelpID)) {
	     status = CDFlib (SELECT_, ENTRY(entryType), entryN,
			      DELETE_, ENTRY(entryType),
			      NULL_);
	     if (ReportStatus(status,FALSE)) {
	       FreeAttrMenu (nAttrs, attrNs, nEntries, entryNs, &IW);
	       if (!BuildAttrMenu(G,CDFname,&nAttrs,&attrNs,
				  &nEntries,&entryNs,&IW)) {
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
	 }
	 break;
       }
       /***********************************************************************
       * Toggle scope.
       ***********************************************************************/
       case TOGGLESCOPEkey_EDIT: {
	 int nameItemN;
	 if (IW.nItems == 0) {
	   ProblemWindow(BOO(G,"A gAttribute isn't selected (none exist).",
			       "A vAttribute isn't selected (none exist)."),
			 FALSE);
	   break;
	 }
	 for (attrX = 0, nameItemN = 0; attrX < nAttrs; attrX++) {
	    int nItems = (int) (1 + (4 * nEntries[attrX]));
	    if (IW.itemN < nameItemN + nItems) {
	      attrN = attrNs[attrX];
	      break;
	    }
	    nameItemN += nItems;
	 }
	 status = CDFlib (SELECT_,ATTR_,attrN,
			  PUT_,ATTR_SCOPE_,BOO(G,VARIABLE_SCOPE,GLOBAL_SCOPE),
			  NULL_);
	 if (ReportStatus(status,FALSE)) {
	   FreeAttrMenu (nAttrs, attrNs, nEntries, entryNs, &IW);
	   if (!BuildAttrMenu(G,CDFname,&nAttrs,&attrNs,
			      &nEntries,&entryNs,&IW)) {
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
	 break;
       }
       /***********************************************************************
       * Display online help.
       ***********************************************************************/
       case HELPkey_FSI:
	 OnlineHelpWindow (ilhFile, BOO(G,GATTRShelpID,VATTRShelpID));
	 break;
       /***********************************************************************
       * Exit menu.
       ***********************************************************************/
       case EXITkey_FSI:
	 ItemWindow (DELETEiw, &IW);
	 FreeAttrMenu (nAttrs, attrNs, nEntries, entryNs, &IW);
	 return TRUE;
     }
   }
}

/******************************************************************************
* BuildAttrMenu.
******************************************************************************/

Logical BuildAttrMenu (G, CDFname, nAttrs, attrNs, nEntries, entryNs, IW)
Logical G;
char *CDFname;
long *nAttrs;
long **attrNs;
long **nEntries;
long ***entryNs;
struct ItemWindowStruct *IW;
{
   CDFstatus status;
   int attrX, entryX;
   long dataType, numElems;
   long NgEntries, NrEntries, NzEntries;
   char attrName[CDF_ATTR_NAME_LEN256+1];
   int curItem;
   int curCol;
   int lineN;
   int n;
   int dataSpecCol;
   void *binary;
   char field[80+1];
   long TnAttrs;                /* Total number of attributes (global and
				   variable scoped). */
   int entryType;
   int entryXt;
   int nLines, nItems;
   long attrN;
   long estLines;               /* Estimated number of lines for the menu. */
   int style;
   AOSs2 (loadingLines, "Loading menu...", "")
   /***************************************************************************
   * Determine number of attributes (total and of proper scope).
   ***************************************************************************/
   status = CDFlib (GET_, CDF_NUMATTRS_, &TnAttrs,
			  BOO(G,CDF_NUMgATTRS_,CDF_NUMvATTRS_), nAttrs,
		    NULL_);
   if (!ReportStatus(status,FALSE)) return FALSE;
   if (!G) {
     long NrVars, NzVars;
     status = CDFlib (GET_, CDF_NUMrVARS_, &NrVars,
			    CDF_NUMzVARS_, &NzVars,
		      NULL_);
     if (!ReportStatus(status,FALSE)) return FALSE;
     estLines = (*nAttrs) * (NrVars + NzVars);
     if (estLines > MANY_ATTRs_AND_ENTRYs) {
       MessageWindow (loadingLines, NULL, LogicalFALSE);
     }
   }
   /***************************************************************************
   * Count/save attribute and entry numbers.  Determine attribute name item
   * numbers.
   ***************************************************************************/
   *nEntries = (long *) cdf_AllocateMemory ((size_t) (*nAttrs * sizeof(long)),
					FatalError);
   *attrNs = (long *) cdf_AllocateMemory ((size_t) (*nAttrs * sizeof(long)),
				      FatalError);
   *entryNs = (long **) cdf_AllocateMemory ((size_t) (*nAttrs * sizeof(long *)),
					FatalError);
   for (attrN=0, attrX=0, nItems=0, nLines=0; attrN < TnAttrs; attrN++) {
      long scope;
      /************************************************************************
      * Determine scope.
      ************************************************************************/
      status = CDFlib (SELECT_, ATTR_, attrN,
		       GET_, ATTR_SCOPE_, &scope,
		       NULL_);
      if (!ReportStatus(status,FALSE)) return FALSE;
      /************************************************************************
      * If proper scope, save attribute number and determine which entries
      * exist.
      ************************************************************************/
      if ((G && (scope == GLOBAL_SCOPE)) ||
	  (!G && (scope == VARIABLE_SCOPE))) {
	int i;
	long entryN;
	/**********************************************************************
	* Tally an attribute of the proper scope.
	**********************************************************************/
	(*attrNs)[attrX] = attrN;
	nItems++;
	nLines++;
	/**********************************************************************
	* Determine number of entries.
	**********************************************************************/
	if (G) {
	  status = CDFlib (GET_, ATTR_NUMgENTRIES_, &NgEntries,
			   NULL_);
	  if (!ReportStatus(status,FALSE)) return FALSE;
	  (*nEntries)[attrX] = NgEntries;
	}
	else {
	  status = CDFlib (GET_, ATTR_NUMrENTRIES_, &NrEntries,
				 ATTR_NUMzENTRIES_, &NzEntries,
			   NULL_);
	  if (!ReportStatus(status,FALSE)) return FALSE;
	  (*nEntries)[attrX] = NrEntries + NzEntries;
	}
	(*entryNs)[attrX] = (long *)
			    cdf_AllocateMemory ((size_t) ((*nEntries)[attrX] *
						       sizeof(long)),
					    FatalError);
	for (i = 0, entryX = 0, entryType = BOO(G,gENTRYt,rENTRYt);
	     i < BOO(G,1,2); i++, entryType = zENTRYt) {
	  long nEntries = E3(entryType,NgEntries,NrEntries,NzEntries);
	  for (entryN = 0, entryXt = 0; entryXt < nEntries; entryN++) {
	     status = CDFlib (SELECT_, ENTRY(entryType), entryN,
			      CONFIRM_, CURENTRY_EXISTENCE(entryType),
			      NULL_);
	     switch (status) {
	       case NO_SUCH_ENTRY:
		 break;
	       default:
		 if (!ReportStatus(status,FALSE)) return FALSE;
		 nItems += 4;
		 if (entryX > 0) nLines++;
		 (*entryNs)[attrX][entryX] = entryN;
		 entryX++;
		 entryXt++;
		 break;
	     }
	  }
	}
	attrX++;
      }
   }
   /***************************************************************************
   * Allocate item section control/lines.
   ***************************************************************************/
   AllocIW (IW, nItems, nLines, 80, FatalError);
   /***************************************************************************
   * Encode label/header.
   ***************************************************************************/
   snprintf (IW->label, (size_t) 6+DU_MAX_NAME_LEN+14+1,
	     " CDF \"%s\" %cAttributes ", CDFname, BOO(G,'g','v'));
   if (G) {
     snprintf (IW->hLines[0], (size_t) sizeof(BLANKs78),
	       "%ld gAttribute%s", *nAttrs, (*nAttrs <= 1 ? "" : "s"));
     strcpyX (IW->hLines[1],
	      "AttrName         Entry  DataSpec  Value(s)", 0);
   }
   else {
     snprintf (IW->hLines[0], (size_t) sizeof(BLANKs78),
	       "%ld vAttribute%s", *nAttrs, (*nAttrs <= 1 ? "" : "s"));
     strcpyX (IW->hLines[1],
	      "AttrName         VarName          DataSpec  Value(s)", 0);
   }
   /***************************************************************************
   * Build each attribute/entry line.
   ***************************************************************************/
   lineN = 0;
   curItem = 0;
   for (attrX = 0; attrX < *nAttrs; attrX++) {
      /************************************************************************
      * Inquire attribute.
      ************************************************************************/
      status = CDFlib (SELECT_, ATTR_, (*attrNs)[attrX],
		       GET_, ATTR_NAME_, attrName,
		       NULL_);
      if (!ReportStatus(status,FALSE)) return FALSE;
      /************************************************************************
      * Setup attribute name.
      ************************************************************************/
      curCol = 0;
      EncodeString (strlen(attrName), attrName, field, 0, ATTRNAME_FIELD_LEN);
      strcpyX (IW->iLines[lineN], field, 0);
      if (strlen(field) < (size_t) ATTRNAME_FIELD_LEN) {
	CatNcharacters (IW->iLines[lineN], ATTRNAME_FIELD_LEN - strlen(field),
			(int) ' ');
      }
      CatNcharacters (IW->iLines[lineN], ATTRNAME_FIELD_BLANKS, (int) ' ');
      IW->iLineNs[curItem] = lineN;
      IW->iCols[curItem] = curCol;
      IW->iLens[curItem] = strlen(field);
      curCol += ATTRNAME_FIELD_LEN + ATTRNAME_FIELD_BLANKS;
      curItem++;
      /************************************************************************
      * Setup each entry.
      ************************************************************************/
      if (!G) {
	status = CDFlib (GET_, ATTR_NUMrENTRIES_, &NrEntries,
			 NULL_);
	if (!ReportStatus(status,FALSE)) return FALSE;
      }
      if ((*nEntries)[attrX] > 0) {
	for (entryX = 0; entryX < (*nEntries)[attrX]; entryX++) {
	   int entryType = BOO(G,gENTRYt,
			       BOO(entryX < NrEntries,rENTRYt,zENTRYt));
	   /*******************************************************************
	   * Inquire entry.
	   *******************************************************************/
	   status = CDFlib (SELECT_,ENTRY(entryType),(*entryNs)[attrX][entryX],
			    GET_, ENTRY_DATATYPE(entryType), &dataType,
				  ENTRY_NUMELEMS(entryType), &numElems,
			    NULL_);
	   if (!ReportStatus(status,FALSE)) return FALSE;
	   /*******************************************************************
	   * Pad with blanks the attribute name field if this isn't the first
	   * entry.
	   *******************************************************************/
	   if (entryX > 0) {
	     strcpyX (IW->iLines[lineN], "", 0);
	     CatNcharacters (IW->iLines[lineN], ATTRNAME_FIELD_LEN, (int) ' ');
	     CatNcharacters (IW->iLines[lineN], ATTRNAME_FIELD_BLANKS,
			     (int) ' ');
	     curCol = ATTRNAME_FIELD_LEN + ATTRNAME_FIELD_BLANKS;
	   }
	   /*******************************************************************
	   * Setup entry number field if global scope, variable name if
	   * variable scope.
	   *******************************************************************/
	   if (G) {
	     snprintf (field, (size_t) sizeof(field),
		       "%ld", (*entryNs)[attrX][entryX] + 1);
	     if (strlen(field) > (size_t) ENTRYNUM_FIELD_LEN)
	       strcpyX (field + ENTRYNUM_FIELD_LEN - 3, "...", 0);
	   }
	   else {
	     char varName[CDF_VAR_NAME_LEN256+1];
	     status = CDFlib (SELECT_, E3(entryType,0,rVAR_,
					  zVAR_), (*entryNs)[attrX][entryX],
			      GET_, E3(entryType,0,
				       rVAR_NAME_,zVAR_NAME_), varName,
			      NULL_);
	     switch (status) {
	       case NO_SUCH_VAR: {
		 Logical Z = E3(entryType,FALSE,FALSE,TRUE);
		 snprintf (field, (size_t) sizeof(field),
			   "<%sEntry.%ld>", BOO(Z,"z","r"),
			   (*entryNs)[attrX][entryX] + 1);
		 break;
	       }
	       default:
		 if (!ReportStatus(status,FALSE)) return FALSE;
		 EncodeString (strlen(varName), varName, field, 0,
			       VARNAME_FIELD_LEN);
		 break;
	     }
	   }
	   strcatX (IW->iLines[lineN], field, 0);
	   if (strlen(field) < (size_t) BOO(G,ENTRYNUM_FIELD_LEN,
					      VARNAME_FIELD_LEN)) {
	     CatNcharacters (IW->iLines[lineN],
			     BOO(G,ENTRYNUM_FIELD_LEN,
				   VARNAME_FIELD_LEN) - strlen(field),
			     (int) ' ');
	   }
	   CatNcharacters (IW->iLines[lineN],
			   BOO(G,ENTRYNUM_FIELD_BLANKS,VARNAME_FIELD_BLANKS),
			   (int) ' ');
	   IW->iLineNs[curItem] = lineN;
	   IW->iCols[curItem] = curCol;
	   IW->iLens[curItem] = strlen(field);
	   curCol += BOO(G,ENTRYNUM_FIELD_LEN,VARNAME_FIELD_LEN) +
		     BOO(G,ENTRYNUM_FIELD_BLANKS,VARNAME_FIELD_BLANKS);
	   curItem++;
	   /*******************************************************************
	   * Setup data type & number of elements.
	   *******************************************************************/
	   strcpyX (field, DataTypeToken(dataType), 0);
	   catchrX (field, (int) '/', 0);
	   snprintf (&field[strlen(field)],
		     (size_t) sizeof(field)-strlen(field),
		     "%ld", numElems);
	   if (strlen(field) > (size_t) DATASPEC_FIELD_LEN) {
	     strcpyX (field + DATASPEC_FIELD_LEN - 3, "...", 0);
	   }
	   strcatX (IW->iLines[lineN], field, 0);
	   if (strlen(field) < (size_t) DATASPEC_FIELD_LEN) {
	     CatNcharacters (IW->iLines[lineN],
			     DATASPEC_FIELD_LEN - strlen(field), (int) ' ');
	   }
	   CatNcharacters (IW->iLines[lineN], DATASPEC_FIELD_BLANKS, (int)' ');
	   IW->iLineNs[curItem] = lineN;
	   IW->iCols[curItem] = curCol;
	   IW->iLens[curItem] = strlen(field);
	   dataSpecCol = curCol;
	   curCol += DATASPEC_FIELD_LEN + DATASPEC_FIELD_BLANKS;
	   curItem++;
	   /*******************************************************************
	   * Setup value(s) field.
	   *******************************************************************/
	   binary = cdf_AllocateMemory ((size_t) (numElems *
					      CDFelemSize(dataType)),
				    FatalError);
	   status = CDFlib (GET_, ENTRY_DATA(entryType), binary,
			    NULL_);
	   if (!ReportStatus(status,FALSE)) return FALSE;
           if (TT2000dataType(dataType)) style = TT2000_3_STYLE;
           else style = EPOCH0_STYLE;
	   n = EncodeValuesFormat (dataType, numElems, binary,
				   &(IW->iLines[lineN][curCol]), NULL, 0,
				   BOO(G,gAttrENTRYVALUE_FIELD_LEN,
					 vAttrENTRYVALUE_FIELD_LEN),
				   style,
				   (size_t) 80-strlen(IW->iLines[lineN]));
	   cdf_FreeMemory (binary, FatalError);
	   /*******************************************************************
	   * Set 'dataSpec/value' item control information.
	   *******************************************************************/
	   IW->iLineNs[curItem] = lineN;
	   IW->iCols[curItem] = dataSpecCol;
	   IW->iLens[curItem] = strlen(&(IW->iLines[lineN][dataSpecCol]));
	   curItem++;
	   /*******************************************************************
	   * Set `value' item control information.
	   *******************************************************************/
	   IW->iLineNs[curItem] = lineN;
	   IW->iCols[curItem] = curCol;
	   IW->iLens[curItem] = n;
	   curItem++;
	   lineN++;
	}
      }
      else {
	lineN++;        /* No entries, just increment to next line. */
      }
   }
   if (!G) {
     if (estLines > MANY_ATTRs_AND_ENTRYs) MessageWindow (NULL);
   }
   return TRUE;
}

/******************************************************************************
* FreeAttrMenu.
******************************************************************************/

void FreeAttrMenu (nAttrs, attrNs, nEntries, entryNs, IW)
long nAttrs;
long *attrNs;
long *nEntries;
long **entryNs;
struct ItemWindowStruct *IW;
{
  int attrX;
  FreeIW (IW, FatalError);
  for (attrX = (int) (nAttrs - 1); attrX >= 0; attrX--) {
     if (entryNs[attrX] != NULL) cdf_FreeMemory (entryNs[attrX],
					     FatalError);
  }
  if (entryNs != NULL) cdf_FreeMemory (entryNs, FatalError);
  if (attrNs != NULL) cdf_FreeMemory (attrNs, FatalError);
  if (nEntries != NULL) cdf_FreeMemory (nEntries, FatalError);
  return;
}

/******************************************************************************
* EditEntry.
* Returns FALSE if a fatal CDF error occurred.
******************************************************************************/

Logical EditEntry (attrN, entryN, entryType, newDataSpec, changed)
long attrN;
long entryN;
int entryType;
Logical newDataSpec;
Logical *changed;	/* Assumed to be initialized to FALSE by the caller. */
{
  long dataType, numElems; int cursorAtThisChar; CDFstatus status;
  static char value[MAX_ENTRYSTRING_LEN+1];
  static char label[15+CDF_VAR_NAME_LEN256+1];
  static char *headerBrowse[] = {
    "Full entry value..."
  };
  static char *headerChar[] = {
    "Enter character string (with delimiters)...",
    "Syntax: <delim><char1><char2>...<charN><delim>"
  };
  static char *headerNoChar[] = {
    "Enter value(s)...",
    "Syntax: <value1>,<value2>,...,<valueN>"
  };
  AOSs1A (trailerBrowse, "Exit: ________   Help: ________")
  AOSs1B (trailerEdit, "Enter: ________   Exit: ________   Help: ________")
  static int exitCharsBrowse[] = { EXITkey_FSI, HELPkey_FSI, NUL };
  static int exitCharsEdit[] = {
    ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL
  };
  static struct PromptWindowStruct PW = {
    label, 4, 1, 78, 0, NULL, MAX_ENTRYSTRING_LEN, value, 1, NULL,
    NULL, REFRESHkey_FSI, SOLkey_FSI, EOLkey_FSI, INSERTorOVERkey_FSI
  };
  /****************************************************************************
  * Encode label.
  ****************************************************************************/
  switch (entryType) {
    case gENTRYt:
      snprintf (label, (size_t) sizeof(label), " gEntry %ld ", entryN + 1);
      break;
    case rENTRYt:
    case zENTRYt: {
      Logical Z = (entryType == zENTRYt);
      char varName[CDF_VAR_NAME_LEN256+1], delim;
      status = CDFlib (SELECT_, VAR(Z), entryN,
		       GET_, VAR_NAME(Z), varName,
		       NULL_);
      switch (status) {
	case NO_SUCH_VAR:
	  snprintf (label, (size_t) sizeof(label),
		    " %cEntry %ld ", BOO(Z,'z','r'), entryN + 1);
	  break;
	default:
	  if (!ReportStatus(status,FALSE)) return FALSE;
	  delim = PickDelimiter (varName, strlen(varName));
	  snprintf (label, (size_t) sizeof(label),
		    " %cEntry for %c%s%c ", BOO(Z,'z','r'),
		    delim, varName, delim);
	  break;
      }
      break;
    }
  }
  /****************************************************************************
  * Either prompt for a new data type (the number of elements will be implied
  * by the value entered) or read the current data specification and value.
  * Note that it should never be necessary to prompt for a new data type when
  * browsing.
  ****************************************************************************/
  if (!browseOnly && newDataSpec) {
    dataType = NO_DATATYPE;
    if (!SelectDataSpec(&dataType,NULL,label)) return TRUE;
    strcpyX (value, BOO(STRINGdataType(dataType),"\"\"",""),
	     MAX_ENTRYSTRING_LEN);
  }
  else {
    void *binary;
    int style;
    status = CDFlib (SELECT_, ATTR_, attrN,
			      ENTRY(entryType), entryN,
		     GET_, ENTRY_DATATYPE(entryType), &dataType,
			   ENTRY_NUMELEMS(entryType), &numElems,
		     NULL_);
    if (!ReportStatus(status,FALSE)) return FALSE;
    binary = cdf_AllocateMemory ((size_t) (numElems * CDFelemSize(dataType)),
			     FatalError);
    status = CDFlib (GET_, ENTRY_DATA(entryType), binary,
		     NULL_);
    if (!ReportStatus(status,FALSE)) return FALSE;
    if (TT2000dataType(dataType)) style = TT2000_3_STYLE;
    else style = EPOCH0_STYLE;
    EncodeValuesFormat (dataType, numElems, binary, value, NULL, 0,
			MAX_ENTRYSTRING_LEN, style,
			(size_t) sizeof(value));
    cdf_FreeMemory (binary, FatalError);
  }
  /****************************************************************************
  * Prompt for the new value.
  ****************************************************************************/
  if (browseOnly) {
    EncodeKeyDefinitions (1, trailerBrowse, EXITkey_FSI, HELPkey_FSI);
    PW.NhLines = 1;
    PW.hLines = headerBrowse;
    PW.tLines = trailerBrowse;
    PW.exitChars = exitCharsBrowse;
  }
  else {
    EncodeKeyDefinitions (1, trailerEdit, ENTERkey_FSI, EXITkey_FSI,
			  HELPkey_FSI);
    PW.NhLines = 2;
    PW.hLines = BOO(STRINGdataType(dataType),headerChar,headerNoChar);
    PW.tLines = trailerEdit;
    PW.exitChars = exitCharsEdit;
  }
  cursorAtThisChar = BOO(browseOnly,0,strlen(PW.value) -
				      BOO(STRINGdataType(dataType),1,0));
  PromptWindow (NEWpw, &PW, cursorAtThisChar, LogicalTRUE);
  for (;;) {
     if (EnterPW(&PW,ENTRYVALUEhelpID)) {
       void *newBinary;
       long newNumElems;
       int style;
       if (TT2000dataType(dataType)) style = TT2000_3_STYLE;
       else style = EPOCH0_STYLE;
       if (DecodeValues(PW.value,dataType,&newNumElems,
			&newBinary,style)) {
	 status = CDFlib (SELECT_, ATTR_, attrN,
				   ENTRY(entryType), entryN,
			  PUT_, ENTRY_DATA(entryType), dataType,
						       newNumElems,
						       newBinary,
			  NULL_);
	 cdf_FreeMemory (newBinary, FatalError);
	 if (ReportStatus(status,FALSE)) {
	   *changed = TRUE;
	   break;
	 }
	 if (NoMoreAccess(NULL)) {
	   PromptWindow (DELETEpw, &PW);
	   return FALSE;
	 }
       }
       else {
         if (dataType == CDF_EPOCH)
	   ProblemWindow ("Error decoding values. Use 'dd-mmm-yyyy hh:mm:ss.sss' epoch format.", FALSE);
         else if (dataType == CDF_EPOCH16)
	   ProblemWindow ("Error decoding values. Use 'dd-mmm-yyyy hh:mm:ss.mmm.uuu.nnn.ppp' epoch format.", FALSE);
         else if (dataType == CDF_TIME_TT2000)
	   ProblemWindow ("Error decoding values. Use 'yyyy-mm-ddThh:mm:ss.mmmuuunnn' epoch format.", FALSE);
         else
	   ProblemWindow ("Error decoding values.", FALSE);
       }
     }
     else
       break;
  }
  PromptWindow (DELETEpw, &PW);
  return TRUE;
}

/******************************************************************************
* EditVarEntries.
******************************************************************************/

Logical EditVarEntries (Z, varN, nVars)
Logical Z;
long varN;
long nVars;
{
   CDFstatus status;
   long nAttrs;                 /* Number of variable scoped attributes. */
   long attrN;
   AOSs2 (header, BLANKs78, BLANKs78)
   AOSs1 (trailerBrowse,
     "Select: ________   Exit: ________   Help: ________   Next variable: ________")
   AOSs3 (trailerEdit,
     "Select: ________   Create attribute: ________   Next variable: ________",
     "Exit:   ________   Create entry:     ________   Delete entry:  ________",
     "Help:   ________")
   static int exitCharsBrowse[] = {
     ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NEXTVARkey_EDIT, NUL
   };
   static int exitCharsEdit[] = {
     ENTERkey_FSI, EXITkey_FSI, CREATEATTRkey_EDIT, HELPkey_FSI,
     CREATEENTRYkey_EDIT, DELETEATTRorENTRYkey_EDIT, NEXTVARkey_EDIT, NUL
   };
   static char label[12+CDF_VAR_NAME_LEN256+11+1];
   static struct ItemWindowStruct IW = {
      0, 0, 80, label, 2, header, 0, NULL, 0, NULL, NULL, NULL, 0, 0, NULL,
      NULL, REFRESHkey_FSI, TRUE, NSkey_FSI, PSkey_FSI
   };
   long *attrNs;                /* Attribute numbers. */
   Logical *entryIs;            /* TRUE if there is an entry. */
   int attrX;                   /* Attribute index (which variable scoped
				   attribute). */
   static Logical first = TRUE;
   /***************************************************************************
   * First time...
   ***************************************************************************/
   if (first) {
       EncodeKeyDefinitions (1, trailerBrowse, ENTERkey_FSI, EXITkey_FSI,
			     HELPkey_FSI, NEXTVARkey_EDIT);
       EncodeKeyDefinitions (3, trailerEdit, ENTERkey_FSI,
			     CREATEATTRkey_EDIT, NEXTVARkey_EDIT, EXITkey_FSI,
			     CREATEENTRYkey_EDIT, DELETEATTRorENTRYkey_EDIT,
			     HELPkey_FSI);
     first = FALSE;
   }
     if (browseOnly) {
       IW.NiRows = 14;
       IW.NtLines = 1;
       IW.tLines = trailerBrowse;
       IW.exitChars = exitCharsBrowse;
     }
     else {
       IW.NiRows = 12;
       IW.NtLines = 3;
       IW.tLines = trailerEdit;
       IW.exitChars = exitCharsEdit;
     }
   /***************************************************************************
   * Build attribute/entry lines and display window.
   ***************************************************************************/
   if (!BuildVarEntryMenu(Z,varN,&nAttrs,&attrNs,&entryIs,&IW)) return FALSE;
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
	 int fieldN, nameItemN;
	 /*********************************************************************
	 * Do any items exist?
	 *********************************************************************/
	 if (IW.nItems == 0) {
	   ProblemWindow ("Nothing selected (no vAttributes exist).", FALSE);
	   break;
	 }
	 /*********************************************************************
	 * Calculate attribute and field numbers.
	 *********************************************************************/
	 for (attrX = 0, nameItemN = 0; attrX < nAttrs; attrX++) {
	    int nItems = (int) (entryIs[attrX] ? 4 : 1);
	    if (IW.itemN < nameItemN + nItems) {
	      attrN = attrNs[attrX];
	      fieldN = IW.itemN - nameItemN;
	      break;
	    }
	    nameItemN += nItems;
	 }
	 /*********************************************************************
	 * Perform desired function (based on field number).
	 *********************************************************************/
	 switch (fieldN) {
	   /*******************************************************************
	   * Modify attribute name.
	   *******************************************************************/
	   case 0: {
	     char attrName[CDF_ATTR_NAME_LEN256+1];
	     status = CDFlib (SELECT_, ATTR_, attrN,
			      GET_, ATTR_NAME_, attrName,
			      NULL_);
	     if (!ReportStatus(status,FALSE)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     if (browseOnly)
	       ShowFullName (attrName, TRUE);
	     else {
	       static char *header[] = {
		  "Enter new name (with delimiters)...",
		  "Syntax: <delim><char1><char2>...<charN><delim>",
		  "Example: \"RANGE\""
	       };
	       AOSs1 (trailer,
		      "Enter: ________   Exit: ________  Help: ________")
	       static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI,
					  HELPkey_FSI, NUL };
	       static char value[1+CDF_ATTR_NAME_LEN256+1+1];
	       static char label[15+CDF_ATTR_NAME_LEN256+1];
	       static struct PromptWindowStruct PW = {
		  label, 4, 1, 78, 3, header, CDF_ATTR_NAME_LEN256+2, value, 1,
		  trailer, exitChars, REFRESHkey_FSI, SOLkey_FSI, EOLkey_FSI,
		  INSERTorOVERkey_FSI
	       };
	       char delim;
	       EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
				     HELPkey_FSI);
	       delim = PickDelimiter (attrName, strlen(attrName));
	       snprintf (value, (size_t) sizeof(value),
			 "%c%s%c", delim, attrName, delim);
	       snprintf (label, (size_t) sizeof(label),
			 " vAttribute %c%s%c ", delim, attrName, delim);
	       PromptWindow (NEWpw, &PW, (int) (strlen(PW.value) - 1),
			     LogicalTRUE);
	       for (;;) {
		  if (EnterPW(&PW,ATTRRENAMEhelpID)) {
		    char attrName[CDF_ATTR_NAME_LEN256+1];
		    if (DecodeDelimitedString(PW.value,attrName)) {
		      status = CDFlib (PUT_, ATTR_NAME_, attrName,
				       NULL_);
		      if (ReportStatus(status,FALSE)) {
		        FreeVarEntryMenu (attrNs, entryIs, &IW);
		        if (!BuildVarEntryMenu(Z,varN,&nAttrs,
					       &attrNs,&entryIs,&IW)) {
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
		      ProblemWindow ("Illegal name (check delimiters).",
				     FALSE);
		  }
		  else
		    break;
	       }
	       PromptWindow (DELETEpw, &PW);
	     }
	     break;
	   }
	   /*******************************************************************
	   * Modify data specification.
	   *******************************************************************/
	   case 1: {
	     if (browseOnly)
	       ItemWindow (BEEPiw, &IW);
	     else {
	       long dataType, numElems;
	       char varName[CDF_VAR_NAME_LEN256+1], delim;
	       status = CDFlib (SELECT_, VAR(Z), varN,
				         ATTR_, attrN,
				         BOO(Z,zENTRY_,rENTRY_), varN,
			        GET_, VAR_NAME(Z), varName,
				      BOO(Z,zENTRY_DATATYPE_,
					    rENTRY_DATATYPE_), &dataType,
				      BOO(Z,zENTRY_NUMELEMS_,
					    rENTRY_NUMELEMS_), &numElems,
			        NULL_);
	       if (!ReportStatus(status,FALSE)) {
		 ItemWindow (DELETEiw, &IW);
		 return FALSE;
	       }
	       delim = PickDelimiter (varName, strlen(varName));
	       snprintf (label, (size_t) sizeof(label),
			 " %cEntry for %c%s%c ", BOO(Z,'z','r'),
		         delim, varName, delim);
	       if (SelectDataSpec(&dataType,NULL,label)) {
		 status = CDFlib (PUT_, BOO(Z,zENTRY_DATASPEC_,
					      rENTRY_DATASPEC_), dataType,
								 numElems,
				  NULL_);
		 if (ReportStatus(status,FALSE)) {
		   FreeVarEntryMenu (attrNs, entryIs, &IW);
		   if (!BuildVarEntryMenu(Z,varN,&nAttrs,
					  &attrNs,&entryIs,&IW)) {
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
	   /*******************************************************************
	   * Modify data specification/value(s).
	   *******************************************************************/
	   case 2: {
	     if (browseOnly)
	       ItemWindow (BEEPiw, &IW);
	     else {
	       Logical changed = FALSE;
	       if (!EditEntry(attrN,varN,
			      BOO(Z,zENTRYt,rENTRYt),
			      TRUE,&changed)) {
		 ItemWindow (DELETEiw, &IW);
		 return FALSE;
	       }
	       if (changed) {
	         FreeVarEntryMenu (attrNs, entryIs, &IW);
	         if (!BuildVarEntryMenu(Z,varN,&nAttrs,&attrNs,&entryIs,&IW)) {
		   ItemWindow (DELETEiw, &IW);
		   return FALSE;
		 }
	         ItemWindow (UPDATEiw, &IW, IW.itemN);
	       }
	     }
	     break;
	   }
	   /*******************************************************************
	   * Modify value(s).
	   *******************************************************************/
	   case 3: {
	     Logical changed = FALSE;
	     if (!EditEntry(attrN,varN,
			    BOO(Z,zENTRYt,rENTRYt),
			    FALSE,&changed)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     if (changed) {
	       FreeVarEntryMenu (attrNs, entryIs, &IW);
	       if (!BuildVarEntryMenu(Z,varN,&nAttrs,
				      &attrNs,&entryIs,&IW)) {
		 ItemWindow (DELETEiw, &IW);
		 return FALSE;
	       }
	       ItemWindow (UPDATEiw, &IW, IW.itemN);
	     }
	     break;
	   }
	 }
	 break;
       }
       /***********************************************************************
       * Create attribute.
       ***********************************************************************/
       case CREATEATTRkey_EDIT: {
	 static char *header[] = {
	   "Enter name (with delimiters)...",
	   "Syntax: <delim><char1><char2>...<charN><delim>",
	   "Example: \"MetricUnits\""
	 };
	 AOSs1 (trailer, "Enter: ________   Exit: ________  Help: _______")
	 static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI,
				    NUL };
	 static char value[DELIMed_ATTR_NAME_LEN+1];
	 static struct PromptWindowStruct PWt = {
	    " New vAttribute ", 4, 1, 78, 3, header, CDF_ATTR_NAME_LEN256+2,
	    value, 1, trailer, exitChars, REFRESHkey_FSI, SOLkey_FSI,
	    EOLkey_FSI, INSERTorOVERkey_FSI
	 };
	 EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
			       HELPkey_FSI);
	 strcpyX (PWt.value, "\"\"", DELIMed_ATTR_NAME_LEN);
	 PromptWindow (NEWpw, &PWt, 1, LogicalTRUE);
	 for (;;) {
	    if (EnterPW(&PWt,ATTRNAMEhelpID)) {
	      char attrName[CDF_ATTR_NAME_LEN256+1]; long attrNum;
	      if (DecodeDelimitedString(PWt.value,attrName)) {
		status = CDFlib (CREATE_, ATTR_, attrName, VARIABLE_SCOPE,
						 &attrNum,
				 NULL_);
		if (ReportStatus(status,FALSE)) {
		  FreeVarEntryMenu (attrNs, entryIs, &IW);
		  if (!BuildVarEntryMenu(Z,varN,&nAttrs,
					 &attrNs,&entryIs,&IW)) {
		    PromptWindow (DELETEpw, &PWt);
		    ItemWindow (DELETEiw, &IW);
		    return FALSE;
		  }
		  ItemWindow (UPDATEiw, &IW, IW.nItems - 1);
		  break;
		}
		if (NoMoreAccess(NULL)) {
		  PromptWindow (DELETEpw, &PWt);
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
	 PromptWindow (DELETEpw, &PWt);
	 break;
       }
       /***********************************************************************
       * Create entry.
       ***********************************************************************/
       case CREATEENTRYkey_EDIT: {
	 int nameItemN;
	 /*********************************************************************
	 * Do any items exist?
	 *********************************************************************/
	 if (IW.nItems == 0) {
	   ProblemWindow ("A vAttribute isn't selected (none exist).",
			  FALSE);
	   break;
	 }
	 /*********************************************************************
	 * Calculate attribute number.
	 *********************************************************************/
	 for (attrX = 0, nameItemN = 0; attrX < nAttrs; attrX++) {
	    int nItems = (int) (entryIs[attrX] ? 4 : 1);
	    if (IW.itemN < nameItemN + nItems) {
	      attrN = attrNs[attrX];
	      break;
	    }
	    nameItemN += nItems;
	 }
	 /*********************************************************************
	 * Create entry (unless it already exists).
	 *********************************************************************/
         if (entryIs[attrX])
           ProblemWindow ("An entry already exists.", FALSE);
         else {
	   Logical changed = FALSE;
           if (!EditEntry(attrN,varN,BOO(Z,zENTRYt,rENTRYt),TRUE,&changed)) {
	     ItemWindow (DELETEiw, &IW);
	     return FALSE;
	   }
	   if (changed) {
             FreeVarEntryMenu (attrNs, entryIs, &IW);
             if (!BuildVarEntryMenu(Z,varN,&nAttrs,&attrNs,&entryIs,&IW)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
             ItemWindow (UPDATEiw, &IW, nameItemN);
           }
	 }
	 break;
       }
       /***********************************************************************
       * Delete entry.
       ***********************************************************************/
       case DELETEATTRorENTRYkey_EDIT: {
	 int nameItemN;
	 /*********************************************************************
	 * Do any items exist?
	 *********************************************************************/
	 if (IW.nItems == 0) {
	   ProblemWindow ("A vAttribute isn't selected (none exist).", FALSE);
	   break;
	 }
	 /*********************************************************************
	 * Calculate attribute number.
	 *********************************************************************/
	 for (attrX = 0, nameItemN = 0; attrX < nAttrs; attrX++) {
	    int nItems = (int) (entryIs[attrX] ? 4 : 1);
	    if (IW.itemN < nameItemN + nItems) {
	      attrN = attrNs[attrX];
	      break;
	    }
	    nameItemN += nItems;
	 }
	 /*********************************************************************
	 * Delete entry (unless it doesn't exist).
	 *********************************************************************/
	 if (entryIs[attrX]) {
	   char attrName[CDF_ATTR_NAME_LEN256+1], varName[CDF_VAR_NAME_LEN256+1],
		question1[28+1+CDF_ATTR_NAME_LEN256+1+1],
		question2[29+1+CDF_VAR_NAME_LEN256+1+1], delim;
	   status = CDFlib (SELECT_, ATTR_, attrN,
			    GET_, ATTR_NAME_, attrName,
			    SELECT_, BOO(Z,zVAR_,rVAR_), varN,
			    GET_, BOO(Z,zVAR_NAME_,rVAR_NAME_), varName,
			    NULL_);
	   if (!ReportStatus(status,FALSE)) {
	     ItemWindow (DELETEiw, &IW);
	     return FALSE;
	   }
	   delim = PickDelimiter (attrName, strlen(attrName));
	   snprintf (question1, (size_t) sizeof(question1),
		     "Delete %sEntry of vAttribute %c%s%c",
		     BOO(Z,"z","r"), delim, attrName, delim);
	   delim = PickDelimiter (varName, strlen(varName));
	   snprintf (question2, (size_t) sizeof(question2),
		     "corresponding to %sVariable %c%s%c ?",
		     BOO(Z,"z","r"), delim, varName, delim);
	   if (ConfirmWindow(4,78,question1,question2,
			     FALSE,DELETEENTRYhelpID)) {
	     status = CDFlib (SELECT_, BOO(Z,zENTRY_,rENTRY_), varN,
			      DELETE_, BOO(Z,zENTRY_,rENTRY_),
			      NULL_);
	     if (ReportStatus(status,FALSE)) {
	       FreeVarEntryMenu (attrNs, entryIs, &IW);
	       if (!BuildVarEntryMenu(Z,varN,&nAttrs,
				      &attrNs,&entryIs,&IW)) {
		 ItemWindow (DELETEiw, &IW);
		 return FALSE;
	       }
	       ItemWindow (UPDATEiw, &IW, nameItemN);
	     }
	     else {
	       if (NoMoreAccess(NULL)) {
		 ItemWindow (DELETEiw, &IW);
		 return FALSE;
	       }
	     }
	   }
	 }
	 else
	   ProblemWindow ("An entry does not exist.", FALSE);
	 break;
       }
       /***********************************************************************
       * Next variable.
       ***********************************************************************/
       case NEXTVARkey_EDIT:
	 varN = (varN + 1) % nVars;
	 FreeVarEntryMenu (attrNs, entryIs, &IW);
	 if (!BuildVarEntryMenu(Z,varN,&nAttrs,&attrNs,&entryIs,&IW)) {
	   ItemWindow (DELETEiw, &IW);
	   return FALSE;
	 }
	 ItemWindow (UPDATEiw, &IW, 0);
	 break;
       /***********************************************************************
       * Display online help.
       ***********************************************************************/
       case HELPkey_FSI:
	 OnlineHelpWindow (ilhFile, ENTRIEShelpID);
	 break;
       /***********************************************************************
       * Exit menu.
       ***********************************************************************/
       case EXITkey_FSI:
	 ItemWindow (DELETEiw, &IW);
	 FreeVarEntryMenu (attrNs, entryIs, &IW);
	 return TRUE;
     }
   }
}

/******************************************************************************
* BuildVarEntryMenu.
******************************************************************************/

Logical BuildVarEntryMenu (Z, varN, nAttrs, attrNs, entryIs, IW)
Logical Z;
long varN;
long *nAttrs;
long **attrNs;
Logical **entryIs;
struct ItemWindowStruct *IW;
{
   CDFstatus status;
   int attrX, curItem, curCol, lineN, n, nItems, dataSpecCol;
   long dataType, numElems, attrN;
   char attrName[CDF_ATTR_NAME_LEN256+1], varName[CDF_VAR_NAME_LEN256+1], delim,
	field[80+1];
   void *binary;
   /***************************************************************************
   * Determine number of attributes (of variable scope).
   ***************************************************************************/
   status = CDFlib (GET_, CDF_NUMvATTRS_, nAttrs,
		    NULL_);
   if (!ReportStatus(status,FALSE)) return FALSE;
   /***************************************************************************
   * Count/save attribute numbers.
   ***************************************************************************/
   *attrNs = (long *) cdf_AllocateMemory ((size_t) (*nAttrs * sizeof(long)),
				      FatalError);
   *entryIs = (Logical *) cdf_AllocateMemory ((size_t) (*nAttrs * sizeof(Logical)),
					  FatalError);
   for (attrN = 0, attrX = 0, nItems = 0; attrX < *nAttrs; attrN++) {
      long scope;
      /************************************************************************
      * Determine scope.
      ************************************************************************/
      status = CDFlib (SELECT_, ATTR_, attrN,
		       GET_, ATTR_SCOPE_, &scope,
		       NULL_);
      if (!ReportStatus(status,FALSE)) return FALSE;
      /************************************************************************
      * If proper scope, save attribute number and determine if entry exists.
      ************************************************************************/
      if (scope == VARIABLE_SCOPE) {
	(*attrNs)[attrX] = attrN;
	status = CDFlib (SELECT_, BOO(Z,zENTRY_,rENTRY_), varN,
			 GET_, BOO(Z,zENTRY_DATATYPE_,
				     rENTRY_DATATYPE_), &dataType,
			 NULL_);
	switch (status) {
	  case NO_SUCH_ENTRY:
	    (*entryIs)[attrX] = FALSE;
	    nItems += 1;
	    break;
	  default:
	    if (!ReportStatus(status,FALSE)) return FALSE;
	    (*entryIs)[attrX] = TRUE;
	    nItems += 4;
	    break;
	}
	attrX++;
      }
   }
   /***************************************************************************
   * Allocate item section control/lines.
   ***************************************************************************/
   AllocIW (IW, nItems, (int) *nAttrs, 80, FatalError);
   /***************************************************************************
   * Encode label/header.
   ***************************************************************************/
   status = CDFlib (SELECT_, VAR(Z), varN,
		    GET_, VAR_NAME(Z), varName,
		    NULL_);
   if (!ReportStatus(status,FALSE)) return FALSE;
   delim = PickDelimiter (varName, strlen(varName));
   snprintf (IW->label, (size_t) 12+CDF_VAR_NAME_LEN256+11+1,
	     " %cVariable %c%s%c %cEntries ", BOO(Z,'z','r'), delim,
	     varName, delim, BOO(Z,'z','r'));

   snprintf (IW->hLines[0], (size_t) sizeof(BLANKs78),
	     "%ld vAttributes", *nAttrs);
   strcpyX (IW->hLines[1], "AttrName         DataSpec  Value(s)", 0);
   /***************************************************************************
   * Build each attribute/entry line.
   ***************************************************************************/
   lineN = 0;
   curItem = 0;
   for (attrX = 0; attrX < *nAttrs; attrX++) {
      /************************************************************************
      * Inquire attribute/entry.
      ************************************************************************/
      status = CDFlib (SELECT_, ATTR_, (*attrNs)[attrX],
		       GET_, ATTR_NAME_, attrName,
		       NULL_);
      if (!ReportStatus(status,FALSE)) return FALSE;
      /************************************************************************
      * Setup attribute name.
      ************************************************************************/
      curCol = 0;
      EncodeString (strlen(attrName), attrName, field, 0, ATTRNAME_FIELD_LEN);
      strcpyX (IW->iLines[lineN], field, 0);
      if (strlen(field) < (size_t) ATTRNAME_FIELD_LEN) {
	CatNcharacters (IW->iLines[lineN], ATTRNAME_FIELD_LEN - strlen(field),
			(int) ' ');
      }
      CatNcharacters (IW->iLines[lineN], ATTRNAME_FIELD_BLANKS, (int) ' ');
      IW->iLineNs[curItem] = lineN;
      IW->iCols[curItem] = curCol;
      IW->iLens[curItem] = strlen(field);
      curCol += ATTRNAME_FIELD_LEN + ATTRNAME_FIELD_BLANKS;
      curItem++;
      /************************************************************************
      * Setup entry data specification/value(s).
      ************************************************************************/
      if ((*entryIs)[attrX]) {
        int style;
	status = CDFlib (SELECT_, BOO(Z,zENTRY_,rENTRY_), varN,
			 GET_, BOO(Z,zENTRY_DATATYPE_,
				     rENTRY_DATATYPE_), &dataType,
			       BOO(Z,zENTRY_NUMELEMS_,
				     rENTRY_NUMELEMS_), &numElems,
			 NULL_);
	if (!ReportStatus(status,FALSE)) return FALSE;
	strcpyX (field, DataTypeToken(dataType), 0);
	catchrX (field, (int) '/', 0);
	snprintf (&field[strlen(field)], (size_t) sizeof(field)-strlen(field),
			 "%ld", numElems);
	if (strlen(field) > (size_t) DATASPEC_FIELD_LEN) {
	  strcpyX (field + DATASPEC_FIELD_LEN - 3, "...", 0);
	}
	strcatX (IW->iLines[lineN], field, 0);
	if (strlen(field) < (size_t) DATASPEC_FIELD_LEN) {
	  CatNcharacters (IW->iLines[lineN],
			  DATASPEC_FIELD_LEN - strlen(field), (int) ' ');
	}
	CatNcharacters (IW->iLines[lineN], DATASPEC_FIELD_BLANKS, (int) ' ');
	IW->iLineNs[curItem] = lineN;
	IW->iCols[curItem] = curCol;
	IW->iLens[curItem] = strlen(field);
	dataSpecCol = curCol;
	curCol += DATASPEC_FIELD_LEN + DATASPEC_FIELD_BLANKS;
	curItem++;
	binary = cdf_AllocateMemory ((size_t) (numElems * CDFelemSize(dataType)),
				 FatalError);
	status = CDFlib (GET_, BOO(Z,zENTRY_DATA_,rENTRY_DATA_), binary,
			 NULL_);
	if (!ReportStatus(status,FALSE)) return FALSE;
        if (TT2000dataType(dataType)) style = TT2000_3_STYLE;
        else style = EPOCH0_STYLE;
	n = EncodeValuesFormat (dataType, numElems, binary,
				&(IW->iLines[lineN][curCol]), NULL, 0,
				varENTRYVALUE_FIELD_LEN, style,
				(size_t) 80-strlen(IW->iLines[lineN]));
	cdf_FreeMemory (binary, FatalError);
	IW->iLineNs[curItem] = lineN;
	IW->iCols[curItem] = dataSpecCol;
	IW->iLens[curItem] = strlen(&(IW->iLines[lineN][dataSpecCol]));
	curItem++;
	IW->iLineNs[curItem] = lineN;
	IW->iCols[curItem] = curCol;
	IW->iLens[curItem] = n;
	curItem++;
      }
      lineN++;
   }
   return TRUE;
}

/******************************************************************************
* FreeVarEntryMenu.
******************************************************************************/

void FreeVarEntryMenu (attrNs, entryIs, IW)
long *attrNs;
Logical *entryIs;
struct ItemWindowStruct *IW;
{
  FreeIW (IW, FatalError);
  if (attrNs != NULL) cdf_FreeMemory (attrNs, FatalError);
  if (entryIs != NULL) cdf_FreeMemory (entryIs, FatalError);
  return;
}

/******************************************************************************
* CreateEntry.
******************************************************************************/

Logical CreateEntry (G, attrN, changed)
Logical G;
long attrN;
Logical *changed;	/* Assumed to be initialized to FALSE by caller. */
{
  long entryN;
  int entryType;
  char attrName[CDF_ATTR_NAME_LEN256+1], delim;
  CDFstatus status;
  /****************************************************************************
  * Inquire attribute name and delimiter to be used.
  ****************************************************************************/
  status = CDFlib (SELECT_, ATTR_, attrN,
		   GET_, ATTR_NAME_, attrName,
		   NULL_);
  if (!ReportStatus(status,FALSE)) return FALSE;
  delim = PickDelimiter (attrName, strlen(attrName));
  if (G) {
    /**************************************************************************
    * Global scope attribute, prompt for entry number.
    **************************************************************************/
    static char *header[] = { "Enter gEntry number..." };
    AOSs2 (trailer, "Enter: ________   Exit: ________", "Help:  ________")
    static char value[MAX_ENTRYNUM_LEN+1];
    static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI,
			       NUL };
    static char label[15+CDF_ATTR_NAME_LEN256+1];
    static struct PromptWindowStruct PW = {
      label, 4, 23, 34, 1, header, MAX_ENTRYNUM_LEN, value, 2, trailer,
      exitChars, REFRESHkey_FSI, SOLkey_FSI, EOLkey_FSI,
      INSERTorOVERkey_FSI
    };
    entryType = gENTRYt;
    EncodeKeyDefinitions (2, trailer, ENTERkey_FSI, EXITkey_FSI,
			  HELPkey_FSI);
    strcpyX (PW.value, "", MAX_ENTRYNUM_LEN);
    snprintf (label, (size_t) sizeof(label),
	      " gAttribute %c%s%c ", delim, attrName, delim);
    PromptWindow (NEWpw, &PW, 0, LogicalTRUE);
    for (;;) {
       if (EnterPW(&PW,ENTRYNUMBERhelpID)) {
	 long entryNt; Logical exists;
	 if (sscanf(PW.value,"%ld",&entryNt) == 1) {
	   if (entryNt > 0) {
	     if (!EntryExists(attrN,entryNt-1,entryType,&exists)) {
	       PromptWindow (DELETEpw, &PW);
	       return FALSE;
	     }
	     if (!exists) {
	       entryN = entryNt - 1;
	       break;
	     }
	     else
	       ProblemWindow (E3(entryType,"That gEntry already exists.",
					   "That rEntry already exists.",
					   "That zEntry already exists."),
			      FALSE);
	   }
	   else
	     ProblemWindow ("Illegal gEntry number.", FALSE);
	 }
	 else
	   ProblemWindow ("Error decoding gEntry number.", FALSE);
       }
       else {
	 entryN = -1;
	 break;
       }
    }
    PromptWindow (DELETEpw, &PW);
  }
  else {
    /**************************************************************************
    * Variable scope attribute, select corresponding variable (or enter entry
    * number).
    **************************************************************************/
    static char *header[] =  { "Select the corresponding variable..." };
    AOSs3 (trailer, "Select: ________   Numbered rEntry: ________",
		    "Exit:   ________   Numbered zEntry: ________",
		    "Help:   ________")
    static int exitChars[] = { ENTERkey_FSI, rENTRYbyNUMBERkey_EDIT,
			       EXITkey_FSI, zENTRYbyNUMBERkey_EDIT,
			       HELPkey_FSI, NUL };
    static char label[15+CDF_ATTR_NAME_LEN256+1];
    static struct ItemWindowStruct IW = {
	4, 16, 48, label, 1, header, 0, NULL, 0, NULL, NULL, NULL, 7,
	3, trailer, exitChars, REFRESHkey_FSI, TRUE, NSkey_FSI, PSkey_FSI
    };
    CDFstatus status; long NrVars, NzVars; int nVars, varX;
    EncodeKeyDefinitions (3, trailer, ENTERkey_FSI, rENTRYbyNUMBERkey_EDIT,
			  EXITkey_FSI, zENTRYbyNUMBERkey_EDIT, HELPkey_FSI);
    snprintf (label, (size_t) sizeof(label),
	      " vAttribute %c%s%c ", delim, attrName, delim);
    status = CDFlib (GET_, CDF_NUMrVARS_, &NrVars,
			   CDF_NUMzVARS_, &NzVars,
		     NULL_);
    if (!ReportStatus(status,FALSE)) return FALSE;
    nVars = (int) (NrVars + NzVars);
    AllocIW (&IW, nVars, nVars, 1+CDF_VAR_NAME_LEN256+1+1, FatalError);
    for (varX = 0; varX < nVars; varX++) {
       char varName[CDF_VAR_NAME_LEN256+1], delim; size_t lengthX;
       Logical Z = (varX < NrVars ? FALSE : TRUE);
       long varN = BOO(Z,varX-NrVars,varX);
       status = CDFlib (SELECT_, VAR(Z), varN,
			GET_, VAR_NAME(Z), varName,
			NULL_);
       if (!ReportStatus(status,FALSE)) return FALSE;
       delim = PickDelimiter (varName, strlen(varName));
       snprintf (IW.iLines[varX], (size_t) 1+CDF_VAR_NAME_LEN256+1+1+1,
		 "%c%s%c", delim, varName, delim);
       IW.iLineNs[varX] = varX;
       IW.iCols[varX] = 0;
       lengthX = strlen(IW.iLines[varX]);
       IW.iLens[varX] = (int) MINIMUM(40,lengthX);
    }
    ItemWindow (NEWiw, &IW, 0);
    for (entryN = -2; entryN == -2; ) {
       ItemWindow (READiw, &IW);
       switch (IW.key) {
	 case ENTERkey_FSI:
	   if (IW.nItems == 0)
	     ProblemWindow ("No variables exist.", FALSE);
	   else {
	     Logical Z = BOO(IW.itemN < NrVars,FALSE,TRUE), exists;
	     long entryNt = BOO(Z,IW.itemN-NrVars,IW.itemN);
	     entryType = BOO(Z,zENTRYt,rENTRYt);
	     if (!EntryExists(attrN,entryNt,entryType,&exists)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     if (!exists)
	       entryN = entryNt;
	     else
	       ProblemWindow (E3(entryType,"That gEntry already exists.",
					   "That rEntry already exists.",
					   "That zEntry already exists."),
			      FALSE);
	   }
	   break;
	 case zENTRYbyNUMBERkey_EDIT:
	 case rENTRYbyNUMBERkey_EDIT: {
	   static char *rHeader[] = { "Enter rEntry number..." };
	   static char *zHeader[] = { "Enter zEntry number..." };
	   AOSs2 (trailer, "Enter: ________   Exit: ________",
			   "Help:  ________")
	   static char value[MAX_ENTRYNUM_LEN+1];
	   static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI,
				      HELPkey_FSI, NUL };
	   static char label[15+CDF_ATTR_NAME_LEN256+1];
	   static struct PromptWindowStruct PW = {
	     label, 7, 23, 34, 1, NULL, MAX_ENTRYNUM_LEN, value, 2, trailer,
	     exitChars, REFRESHkey_FSI, SOLkey_FSI, EOLkey_FSI,
	     INSERTorOVERkey_FSI
	   };
	   Logical Z = (IW.key == zENTRYbyNUMBERkey_EDIT);
	   entryType = BOO(Z,zENTRYt,rENTRYt);
	   EncodeKeyDefinitions (2, trailer, ENTERkey_FSI, EXITkey_FSI,
				 HELPkey_FSI);
	   PW.hLines = BOO(Z,zHeader,rHeader);
	   strcpyX (PW.value, "", MAX_ENTRYNUM_LEN);
	   snprintf (label, (size_t) sizeof(label),
		     " vAttribute %c%s%c ", delim, attrName, delim);
	   PromptWindow (NEWpw, &PW, 0, LogicalTRUE);
	   for (;;) {
	      if (EnterPW(&PW,ENTRYNUMBERhelpID)) {
		long entryNt; Logical exists;
		if (sscanf(PW.value,"%ld",&entryNt) == 1) {
		  if (entryNt > 0) {
		    if (!EntryExists(attrN,entryNt-1,entryType,&exists)) {
		      PromptWindow (DELETEpw, &PW);
		      return FALSE;
		    }
		    if (!exists) {
		      entryN = entryNt - 1;
		      break;
		    }
		    else
		      ProblemWindow (E3(entryType,
					"That gEntry already exists.",
					"That rEntry already exists.",
					"That zEntry already exists."), FALSE);
		  }
		  else
		    ProblemWindow (BOO(Z,"Illegal zEntry number.",
					 "Illegal rEntry number."), FALSE);
		}
		else
		  ProblemWindow (BOO(Z,"Error decoding zEntry number.",
				       "Error decoding rEntry number."),FALSE);
	      }
	      else {
		entryN = -1;
		break;
	      }
	   }
	   PromptWindow (DELETEpw, &PW);
	   break;
	 }
	 case HELPkey_FSI:
	   OnlineHelpWindow (ilhFile, VARSELECThelpID);
	   break;
	 case EXITkey_FSI:
	   entryN = -1;
	   break;
       }
    }
    ItemWindow (DELETEiw, &IW);
    FreeIW (&IW, FatalError);
  }
  /****************************************************************************
  * Return if an entry number wasn't selected/entered.
  ****************************************************************************/
  if (entryN == -1) return TRUE;
  /****************************************************************************
  * Create the entry.
  ****************************************************************************/
  return EditEntry(attrN,entryN,entryType,TRUE,changed);
}

/******************************************************************************
* EntryExists.
******************************************************************************/

Logical EntryExists (attrN, entryN, entryType, exists)
long attrN;
long entryN;
int entryType;
Logical *exists;
{
  CDFstatus status = CDFlib (SELECT_, ATTR_, attrN,
			     CONFIRM_, ENTRY_EXISTENCE(entryType), entryN,
			     NULL_);
  switch (status) {
    case NO_SUCH_ENTRY:
      *exists = FALSE;
      break;
    default:
      ReportStatus (status, FALSE);
      if (StatusBAD(status)) return FALSE;
      *exists = StatusOK(status);
      break;
  }
  return TRUE;
}

/******************************************************************************
* EditAttrsX.
******************************************************************************/

Logical EditAttrsX (G, CDFname)
Logical G;                      /* If TRUE, global scoped attributes. */
char *CDFname;                  /* Name of the CDF being edited. */
{
   CDFstatus status;
   long nAttrs;			/* Total number of attributes. */
   long nAttrsOfScope;		/* Number of attributes of the proper scope. */
   long attrN;
   long fieldN;
   long *attrNs;
   AOSs2 (header, BLANKs78, BLANKs78)
   AOSs1 (trailerBrowse,
    "Select: ________   Exit: ________   Help: ________")
   AOSs2A (trailerEdit,
    "Select: ________  Create attribute: ________  Toggle scope:     ________",
    "Exit:   ________  Help:             ________  Delete attribute: ________")
   static int exitCharsBrowse[] = {
     ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL
   };
   static int exitCharsEdit[] = {
     ENTERkey_FSI, EXITkey_FSI, CREATEATTRkey_EDIT, HELPkey_FSI,
     TOGGLESCOPEkey_EDIT, DELETEATTRorENTRYkey_EDIT, NUL
   };
   static char label[6+DU_MAX_NAME_LEN+14+1];
   static struct ItemWindowStruct IW = {
      0, 0, 80, label, 2, header, 0, NULL, 0, NULL, NULL, NULL, 0, 0, NULL,
      NULL, REFRESHkey_FSI, TRUE, NSkey_FSI, PSkey_FSI
   };
   static Logical first = TRUE;
   /***************************************************************************
   * First time...
   ***************************************************************************/
   if (first) {
       EncodeKeyDefinitions (1, trailerBrowse, ENTERkey_FSI, EXITkey_FSI,
			     HELPkey_FSI);
       EncodeKeyDefinitions (2, trailerEdit, ENTERkey_FSI,
			     CREATEATTRkey_EDIT, TOGGLESCOPEkey_EDIT,
			     EXITkey_FSI, HELPkey_FSI,
			     DELETEATTRorENTRYkey_EDIT);
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
   * Build attribute lines and display menu.
   ***************************************************************************/
   if (!BuildAttrMenuX(G,CDFname,&nAttrs,
		       &nAttrsOfScope,&attrNs,&IW)) return FALSE;
   ItemWindow (NEWiw, &IW, 0);
   /***************************************************************************
   * Read/process keystrokes until request to exit menu.
   ***************************************************************************/
   for (;;) {
     ItemWindow (READiw, &IW);
     if (IW.nItems != 0) {
       fieldN = IW.itemN % 3;
       attrN = attrNs[IW.itemN/3];
     }
     switch (IW.key) {
       /***********************************************************************
       * Item selected.
       ***********************************************************************/
       case ENTERkey_FSI: {
	 /*********************************************************************
	 * Check if any attributes/entries exist.
	 *********************************************************************/
	 if (IW.nItems == 0) {
	   ProblemWindow (BOO(G,"Nothing selected (no gAttributes exist).",
			        "Nothing selected (no vAttributes exist)."),
			  FALSE);
	   break;
	 }
	 /*********************************************************************
	 * Perform desired function (based on field number).
	 *********************************************************************/
	 switch (fieldN) {
	   /*******************************************************************
	   * Modify attribute name.
	   *******************************************************************/
	   case 0: {
	     char attrName[CDF_ATTR_NAME_LEN256+1];
	     status = CDFlib (SELECT_, ATTR_, attrN,
			      GET_, ATTR_NAME_, attrName,
			      NULL_);
	     if (!ReportStatus(status,FALSE)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     if (browseOnly) {
	       ShowFullName (attrName, TRUE);
	     }
	     else {
	       static char *header[] = {
	         "Enter new name (with delimiters)...",
	         "Syntax: <delim><char1><char2>...<charN><delim>",
	         "Example: \"FILTERs\""
	       };
	       AOSs1 (trailer,
		      "Enter: ________   Exit: ________  Help: ________")
	       static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI,
					  HELPkey_FSI, NUL };
	       static char value[1+CDF_ATTR_NAME_LEN256+1+1];
	       static char label[15+CDF_ATTR_NAME_LEN256+1];
	       static struct PromptWindowStruct PWt = {
		  label, 4, 1, 78, 3, header, CDF_ATTR_NAME_LEN256+2, value,
		  1, trailer, exitChars, REFRESHkey_FSI, SOLkey_FSI,
		  EOLkey_FSI, INSERTorOVERkey_FSI
	       };
	       char delim;
	       EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
				     HELPkey_FSI);
	       delim = PickDelimiter (attrName, strlen(attrName));
	       snprintf (value, (size_t) sizeof(value),
			 "%c%s%c", delim, attrName, delim);
	       snprintf (label, (size_t) sizeof(label),
			 " %cAttribute %c%s%c ", BOO(G,'g','v'),
		        delim, attrName, delim);
	       PromptWindow (NEWpw, &PWt, (int)(strlen(PWt.value) - 1),
			     LogicalTRUE);
	       for (;;) {
		  if (EnterPW(&PWt,ATTRRENAMEhelpID)) {
		    char attrName[CDF_ATTR_NAME_LEN256+1];
		    if (DecodeDelimitedString(PWt.value,attrName)) {
		      status = CDFlib (PUT_, ATTR_NAME_, attrName,
				       NULL_);
		      if (ReportStatus(status,FALSE)) {
		        FreeAttrMenuX (attrNs, &IW);
		        if (!BuildAttrMenuX(G,CDFname,&nAttrs,
					    &nAttrsOfScope,&attrNs,&IW)) {
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
		      ProblemWindow ("Illegal name (check delimiters).",
				     FALSE);
		  }
		  else
		    break;
	       }
	       PromptWindow (DELETEpw, &PWt);
	     }
	     break;
	   }
	   /*******************************************************************
	   * Edit entries.
	   *******************************************************************/
	   case 1: {
	     ItemWindow (UNDISPLAYiw, &IW);
	     if (!EditAttrEntriesX(G,attrN,nAttrs)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     ItemWindow (REDISPLAYiw, &IW);
	     break;
	   }
	   /*******************************************************************
	   * TextEdit entries.
	   *******************************************************************/
	   case 2: {
	     if (G) {
	       if (!TextEditEntries(attrN)) {
		 ItemWindow (DELETEiw, &IW);
		 return FALSE;
	       }
	     }
	     else
	       ProblemWindow ("Not supported for vAttributes.", FALSE);
	     break;
	   }
	 }
	 break;
       }
       /***********************************************************************
       * Create new attribute.
       ***********************************************************************/
       case CREATEATTRkey_EDIT: {
	 static char *header[] = {
	   "Enter name (with delimiters)...",
	   "Syntax: <delim><char1><char2>...<charN><delim>",
	   "Example: \"Resolution\""
	 };
	 AOSs1 (trailer, "Enter: ________   Exit: ________  Help: ________")
	 static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI,
				    NUL };
	 static char value[DELIMed_ATTR_NAME_LEN+1];
	 static char label[16+1];
	 static struct PromptWindowStruct PWt = {
	   label, 4, 1, 78, 3, header, CDF_ATTR_NAME_LEN256+2, value, 1, trailer,
	   exitChars, REFRESHkey_FSI, SOLkey_FSI, EOLkey_FSI,
	   INSERTorOVERkey_FSI
	 };
	 EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
			       HELPkey_FSI);
	 strcpyX (value, "\"\"", DELIMed_ATTR_NAME_LEN);
	 snprintf (label, (size_t) sizeof(label),
		   " New %cAttribute ", BOO(G,'g','v'));
	 PromptWindow (NEWpw, &PWt, 1, LogicalTRUE);
	 for (;;) {
	    if (EnterPW(&PWt,ATTRNAMEhelpID)) {
	      char attrName[CDF_ATTR_NAME_LEN256+1];
	      long attrNum;
	      if (DecodeDelimitedString(PWt.value,attrName)) {
		status = CDFlib (CREATE_, ATTR_, attrName,
						 BOO(G,GLOBAL_SCOPE,
						       VARIABLE_SCOPE),
						 &attrNum,
				 NULL_);
		if (ReportStatus(status,FALSE)) {
		  FreeAttrMenuX (attrNs, &IW);
		  if (!BuildAttrMenuX(G,CDFname,&nAttrs,
				      &nAttrsOfScope,&attrNs,&IW)) {
		    PromptWindow (DELETEpw, &PWt);
		    ItemWindow (DELETEiw, &IW);
		    return FALSE;
		  }
		  ItemWindow (UPDATEiw, &IW, IW.nItems - 3);
		  break;
		}
		if (NoMoreAccess(NULL)) {
		  PromptWindow (DELETEpw, &PWt);
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
	 PromptWindow (DELETEpw, &PWt);
	 break;
       }
       /***********************************************************************
       * Delete attribute.
       ***********************************************************************/
       case DELETEATTRorENTRYkey_EDIT: {
	 char attrName[CDF_ATTR_NAME_LEN256+1],
	      question[22+CDF_ATTR_NAME_LEN256+1], delim;
	 if (IW.nItems == 0) {
	   ProblemWindow
	       (BOO(G,"A gAttribute isn't selected (none exist).",
		      "A vAttribute isn't selected (none exist)."), FALSE);
	   break;
	 }
	 status = CDFlib (SELECT_, ATTR_, attrN,
			  GET_, ATTR_NAME_, attrName,
			  NULL_);
	 if (!ReportStatus(status,FALSE)) {
	   ItemWindow (DELETEiw, &IW);
	   return FALSE;
	 }
	 delim = PickDelimiter (attrName, strlen(attrName));
	 snprintf (question, (size_t) sizeof(question),
		   "Delete %sAttribute %c%s%c ?",
		   BOO(G,"g","v"), delim, attrName, delim);
	 if (ConfirmWindow(4,78,question,NULL,FALSE,DELETEATTRhelpID)) {
	   status = CDFlib (DELETE_, ATTR_,
			    NULL_);
	   if (ReportStatus(status,FALSE)) {
	     int itemNt = IW.itemN - (IW.itemN % 3);
	     FreeAttrMenuX (attrNs, &IW);
	     if (!BuildAttrMenuX(G,CDFname,&nAttrs,
				 &nAttrsOfScope,&attrNs,&IW)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     if (itemNt == IW.nItems) itemNt = MaxInt (0, itemNt - 3);
	     ItemWindow (UPDATEiw, &IW, itemNt);
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
       * Toggle scope.
       ***********************************************************************/
       case TOGGLESCOPEkey_EDIT: {
	 if (IW.nItems == 0) {
	   ProblemWindow(BOO(G,"A gAttribute isn't selected (none exist).",
			       "A vAttribute isn't selected (none exist)."),
			 FALSE);
	   break;
	 }
	 status = CDFlib (SELECT_,ATTR_,attrN,
			  PUT_,ATTR_SCOPE_,BOO(G,VARIABLE_SCOPE,GLOBAL_SCOPE),
			  NULL_);
	 if (ReportStatus(status,FALSE)) {
	   int itemNt = IW.itemN - (IW.itemN % 3);
	   FreeAttrMenuX (attrNs, &IW);
	   if (!BuildAttrMenuX(G,CDFname,&nAttrs,
			       &nAttrsOfScope,&attrNs,&IW)) {
	     ItemWindow (DELETEiw, &IW);
	     return FALSE;
	   }
	   if (itemNt == IW.nItems) itemNt = MaxInt (0, itemNt - 3);
	   ItemWindow (UPDATEiw, &IW, itemNt);
	 }
	 else {
	   if (NoMoreAccess(NULL)) {
	     ItemWindow (DELETEiw, &IW);
	     return FALSE;
	   }
	 }
	 break;
       }
       /***********************************************************************
       * Display online help.
       ***********************************************************************/
       case HELPkey_FSI:
	 OnlineHelpWindow (ilhFile, BOO(G,GATTRShelpIDx,VATTRShelpIDx));
	 break;
       /***********************************************************************
       * Exit menu.
       ***********************************************************************/
       case EXITkey_FSI:
	 ItemWindow (DELETEiw, &IW);
	 FreeAttrMenuX (attrNs, &IW);
	 return TRUE;
     }
   }
}

/******************************************************************************
* BuildAttrMenuX.
******************************************************************************/

Logical BuildAttrMenuX (G, CDFname, nAttrs, nAttrsOfScope, attrNs, IW)
Logical G;
char *CDFname;
long *nAttrs;
long *nAttrsOfScope;
long **attrNs;
struct ItemWindowStruct *IW;
{
   CDFstatus status; long attrN; int lineN; size_t nBytes;
   int attrNameFieldLen = BOO(browseOnly,ATTRNAME_bFIELD_LEN,
					 ATTRNAME_eFIELD_LEN);
   /***************************************************************************
   * Determine number of attributes (total and of proper scope).
   ***************************************************************************/
   status = CDFlib (GET_, CDF_NUMATTRS_, nAttrs,
			  BOO(G,CDF_NUMgATTRS_,CDF_NUMvATTRS_), nAttrsOfScope,
		    NULL_);
   if (!ReportStatus(status,FALSE)) return FALSE;
   /***************************************************************************
   * Allocate item section control/lines.
   ***************************************************************************/
   nBytes = (size_t) (*nAttrsOfScope * sizeof(long));
   *attrNs = (long *) cdf_AllocateMemory (nBytes, FatalError);
   AllocIW (IW, (int) (3 * (*nAttrsOfScope)), (int) *nAttrsOfScope,
	    80, FatalError);
   /***************************************************************************
   * Encode label/header.
   ***************************************************************************/
   snprintf (IW->label, (size_t) 6+DU_MAX_NAME_LEN+14+1,
	     " CDF \"%s\" %cAttributes ", CDFname, BOO(G,'g','v'));
   if (G)
     snprintf (IW->hLines[0], (size_t) sizeof(BLANKs78),
	       "%ld gAttribute%s", *nAttrsOfScope,
	       (*nAttrs <= 1 ? "" : "s"));
   else
     snprintf (IW->hLines[0], (size_t) sizeof(BLANKs78),
	       "%ld vAttribute%s", *nAttrsOfScope,
	       (*nAttrs <= 1 ? "" : "s"));
   strcpyX (IW->hLines[1], "AttrName", 0);
   /***************************************************************************
   * Build each attribute/entry line.
   ***************************************************************************/
   for (attrN = 0, lineN = 0; attrN < *nAttrs; attrN++) {
      long scope; int itemN, nameLen;
      char attrName[CDF_ATTR_NAME_LEN256+1];
      /************************************************************************
      * Inquire attribute.
      ************************************************************************/
      status = CDFlib (SELECT_, ATTR_, attrN,
		       GET_, ATTR_NAME_, attrName,
			     ATTR_SCOPE_, &scope,
		       NULL_);
      if (!ReportStatus(status,FALSE)) return FALSE;
      /************************************************************************
      * If of the proper scope...
      ************************************************************************/
      if ((G && scope == GLOBAL_SCOPE) ||
	  (!G && scope == VARIABLE_SCOPE)) {
	/**********************************************************************
	* Save attribute number.
	**********************************************************************/
	(*attrNs)[lineN] = attrN;
	/**********************************************************************
	* Setup attribute name.
	**********************************************************************/
        nameLen = (int) strlen(attrName);
	EncodeString ((long) nameLen, attrName, IW->iLines[lineN],
		      -attrNameFieldLen, attrNameFieldLen);
	strcatX (IW->iLines[lineN], " ", 0);
	itemN = 3 * lineN;
	IW->iLineNs[itemN] = lineN;
	IW->iCols[itemN] = 0;
	IW->iLens[itemN] = MinInt(1 + nameLen + 1,attrNameFieldLen);
	/**********************************************************************
	* Setup <Edit/Browse entries> field.
	**********************************************************************/
	IW->iCols[itemN+1] = (int) strlen(IW->iLines[lineN]);
	strcatX (IW->iLines[lineN],
		 BOO(browseOnly,"<Browse entries> ","<Edit entries> "), 0);
	IW->iLineNs[itemN+1] = lineN;
	IW->iLens[itemN+1] = BOO(browseOnly,16,14);
	/**********************************************************************
	* Setup <TextEdit/TextBrowse entries> field.
	**********************************************************************/
	IW->iCols[itemN+2] = (int) strlen(IW->iLines[lineN]);
	strcatX (IW->iLines[lineN],
		 BOO(browseOnly,"<TextBrowse entries>","<TextEdit entries>"),
		 0);
	IW->iLineNs[itemN+2] = lineN;
	IW->iLens[itemN+2] = BOO(browseOnly,20,18);
	/**********************************************************************
	* Increment to next line.
	**********************************************************************/
	lineN++;
     }
   }
   return TRUE;
}

/******************************************************************************
* FreeAttrMenuX.
******************************************************************************/

void FreeAttrMenuX (attrNs, IW)
long *attrNs;
struct ItemWindowStruct *IW;
{
  if (attrNs != NULL) cdf_FreeMemory (attrNs, FatalError);
  FreeIW (IW, FatalError);
  return;
}

/******************************************************************************
* TextEditEntries.
******************************************************************************/

Logical TextEditEntries (attrN)
long attrN;		/* Attribute number being edited. */
{
   AOSs1A (header,
	   "Is your favorite editing key missing?  If so, contact CDFsupport.")
   AOSs1B (trailerBrowse, "Exit: ________   Help: ________")
   AOSs1C (trailerEdit, "Update: ________   Exit: ________   Help: ________")
   static int exitCharsBrowse[] = { EXITkey_FSI, HELPkey_FSI, NUL };
   static int exitCharsEdit[] = { UPDATEkey_EDIT, EXITkey_FSI, HELPkey_FSI };
   static char label[12+1+CDF_ATTR_NAME_LEN256+1+13+1];
   static struct EditWindowStruct EW = {
      label, 0, 0, 80, 1, header, NULL, 15, 1, NULL, TRUE, TRUE,
      NULL, REFRESHkey_FSI, NUL, NUL, NUL, NUL, NSkey_FSI, PSkey_FSI,
      NUL, NUL, INSERTorOVERkey_FSI
   };
   static Logical first = TRUE; Logical allCharacter;
   /***************************************************************************
   * First time...
   ***************************************************************************/
   if (first) {
       EncodeKeyDefinitions (1, trailerBrowse, EXITkey_FSI, HELPkey_FSI);
       EncodeKeyDefinitions (1, trailerEdit, UPDATEkey_EDIT, EXITkey_FSI,
			     HELPkey_FSI);
     first = FALSE;
   }
     if (browseOnly) {
       EW.tLines = trailerBrowse;
       EW.exitChars = exitCharsBrowse;
       EW.readOnly = TRUE;
     }
     else {
       EW.tLines = trailerEdit;
       EW.exitChars = exitCharsEdit;
       EW.readOnly = FALSE;
     }
   /***************************************************************************
   * Build attribute/entry lines and display menu.
   ***************************************************************************/
   if (!BuildTextEditEntriesMenu(attrN,&EW,&allCharacter)) return FALSE;
   if (!allCharacter) {
     ProblemWindow ("Not all entries have a character data type.", FALSE);
     return TRUE;
   }
   EditWindow (NEWiw, &EW, TRUE);
   /***************************************************************************
   * Read/process keystrokes until request to exit menu.
   ***************************************************************************/
   for (;;) {
     EditWindow (READew, &EW);
     switch (EW.key) {
       case UPDATEkey_EDIT: {
	 UpdateTextEditEntries (attrN, &EW);
	 EditWindow (DELETEew, &EW);
	 cdf_FreeMemory (EW.eText, FatalError);
	 return (!NoMoreAccess(NULL));
       }
       /***********************************************************************
       * Display online help.
       ***********************************************************************/
       case HELPkey_FSI:
	 OnlineHelpWindow (ilhFile, TEXTENTRIEShelpID);
	 break;
       /***********************************************************************
       * Exit menu.
       ***********************************************************************/
       case EXITkey_FSI:
	 EditWindow (DELETEew, &EW);
	 cdf_FreeMemory (EW.eText, FatalError);
	 return TRUE;
     }
   }
}

/******************************************************************************
* BuildTextEditEntriesMenu.
* Returns FALSE if a fatal error detected.
******************************************************************************/

Logical BuildTextEditEntriesMenu (attrN, EW, allCharacter)
long attrN;			/* gAttribute number. */
struct EditWindowStruct *EW;	/* EditWindow structure pointer. */
Logical *allCharacter;		/* All character data type entries? */
{
  CDFstatus status; char *line, attrName[CDF_ATTR_NAME_LEN256+1]; int i;
  long totalChars = 0, maxEntry, entryN, dataType, numElems, totalBytes;
  long numBytes;
  /****************************************************************************
  * Encode label.
  ****************************************************************************/
  status = CDFlib (SELECT_, ATTR_, attrN,
		   GET_, ATTR_NAME_, attrName,
		   NULL_);
  if (!ReportStatus(status,FALSE)) return FALSE;
  snprintf (EW->label, (size_t) 12+1+CDF_ATTR_NAME_LEN256+1+13+1,
	    " gAttribute \"%s\" TextEntries ", attrName);
  /****************************************************************************
  * Verify that all entries have a character data type and count the total
  * number of characters.
  ****************************************************************************/
  status = CDFlib (GET_, ATTR_MAXgENTRY_, &maxEntry,
		   NULL_);
  if (!ReportStatus(status,FALSE)) return FALSE;
  for (entryN = 0; entryN <= maxEntry; entryN++) {
     status = CDFlib (SELECT_, gENTRY_, entryN,
		      GET_, gENTRY_DATATYPE_, &dataType,
			    gENTRY_NUMELEMS_, &numElems,
		      NULL_);
     switch (status) {
       case NO_SUCH_ENTRY:
	 break;
       default:
	 if (!ReportStatus(status,FALSE)) return FALSE;
	 if (!STRINGdataType(dataType)) {
	   *allCharacter = FALSE;
	   return TRUE;
	 }
	 totalChars += numElems;
	 break;
     }
  }
  /****************************************************************************
  * Build text.
  ****************************************************************************/
  totalBytes = (maxEntry + 1) + totalChars + 1;
  EW->eText = (char *) cdf_AllocateMemory ((size_t) totalBytes, FatalError);
  MakeNUL (EW->eText);
  for (entryN = 0; entryN <= maxEntry; entryN++) {
     status = CDFlib (SELECT_, gENTRY_, entryN,
		      GET_, gENTRY_NUMELEMS_, &numElems,
		      NULL_);
     switch (status) {
       case NO_SUCH_ENTRY:
	 strcatX (EW->eText, "\n", (size_t) totalBytes);
	 break;
       default:
	 if (!ReportStatus(status,FALSE)) return FALSE;
	 line = (char *) cdf_AllocateMemory ((size_t) (numElems + 1),
					 FatalError);
	 status = CDFlib (GET_, gENTRY_DATA_, line,
			  NULL_);
	 if (!ReportStatus(status,FALSE)) return FALSE;
	 numBytes = (long) UTF8StrLength ((unsigned char *)line);
	 if (numBytes == numElems) {
	   for (i = 0; i < numElems; i++) {
	      if (!Printable(line[i])) line[i] = '.';
	   }
	 }
	 line[(int)numElems] = NUL;
	 strcatX (EW->eText, line, (size_t) totalBytes);
	 strcatX (EW->eText, "\n", (size_t) totalBytes);
	 cdf_FreeMemory (line, FatalError);
	 break;
     }
  }
  *allCharacter = TRUE;
  return TRUE;
}

/******************************************************************************
* UpdateTextEditEntries.
******************************************************************************/

Logical UpdateTextEditEntries (attrN, EW)
long attrN;			/* gAttribute number. */
struct EditWindowStruct *EW;	/* EditWindow structure pointer. */
{
  CDFstatus status; long entryN, maxEntryN; char *ptr1, *ptr2; size_t length;
  /****************************************************************************
  * Select attribute.
  ****************************************************************************/
  status = CDFlib (SELECT_, ATTR_, attrN,
		   NULL_);
  if (!ReportStatus(status,FALSE)) return FALSE;
  /****************************************************************************
  * Scan text...
  ****************************************************************************/
  entryN = NO_ENTRY;
  ptr1 = EW->eText;
  for (;;) {
     /*************************************************************************
     * Look for next newline character.
     *************************************************************************/
     ptr2 = strchr (ptr1, Nl);
     /*************************************************************************
     * If a newline character wasn't found, check if one or more characters
     * exist before the terminating NUL.  If not, we're done scanning the
     * text.
     *************************************************************************/
     if (ptr2 == NULL) {
       length = strlen (ptr1);
       if (length == 0) break;
       ptr2 = ptr1 + length;
     }
     else
       length = (size_t) (ptr2 - ptr1);
     /*************************************************************************
     * Rewrite or delete the entry depending on the length of this line of
     * text.
     *************************************************************************/
     entryN++;
     if (length > 0) {
       status = CDFlib (SELECT_, gENTRY_, entryN,
			PUT_, gENTRY_DATA_, CDF_CHAR, (long) length, ptr1,
			NULL_);
       if (!ReportStatus(status,FALSE)) return FALSE;
     }
     else {
       status = CDFlib (SELECT_, gENTRY_, entryN,
			DELETE_, gENTRY_,
			NULL_);
       switch (status) {
	 case NO_SUCH_ENTRY:
	   break;
	 default:
	   if (!ReportStatus(status,FALSE)) return FALSE;
	   break;
       }
     }
     /*************************************************************************
     * If this line of text ended on the NUL, then we're done scanning the
     * text.  Otherwise, continue at the next character.
     *************************************************************************/
     if (*ptr2 == NUL) break;
     ptr1 = ptr2 + 1;
  }
  /****************************************************************************
  * Determine maximum gEntry number.
  ****************************************************************************/
  status = CDFlib (GET_, ATTR_MAXgENTRY_, &maxEntryN,
		   NULL_);
  if (!ReportStatus(status,FALSE)) return FALSE;
  /****************************************************************************
  * Delete remaining entries.
  ****************************************************************************/
  entryN++;
  while (entryN <= maxEntryN) {
    status = CDFlib (SELECT_, gENTRY_, entryN,
		     DELETE_, gENTRY_,
		     NULL_);
    switch (status) {
      case NO_SUCH_ENTRY:
	break;
      default:
	if (!ReportStatus(status,FALSE)) return FALSE;
	break;
    }
    entryN++;
  }
  return TRUE;
}

/******************************************************************************
* EditAttrEntriesX.
******************************************************************************/

Logical EditAttrEntriesX (G, attrN, nAttrs)
Logical G;	/* If TRUE, global scoped attribute. */
long attrN;     /* Attribute number being edited. */
long nAttrs;	/* Number of attributes with this scope. */
{
   CDFstatus status;
   long entryN, fieldN, scope;
   AOSs2 (header, BLANKs78, BLANKs78)
   AOSs1 (trailerBrowse,
	  "Select: ________   Exit: ________   Help: ________   Next attribute: ________")
   AOSs2A (trailerEdit,
	  "Modify: ________   Create entry: ________   Delete entry:   ________",
	  "Exit:   ________   Help:         ________   Next attribute: ________")
   static int exitCharsBrowse[] = {
     ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NEXTATTRkey_EDIT, NUL
   };
   static int exitCharsEdit[] = {
     ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, CREATEENTRYkey_EDIT,
     DELETEATTRorENTRYkey_EDIT, NEXTATTRkey_EDIT, NUL
   };
   static char label[12+1+CDF_ATTR_NAME_LEN256+1+9+1];
   static struct ItemWindowStruct IW = {
      0, 0, 80, label, 2, header, 0, NULL, 0, NULL, NULL, NULL, 0, 0, NULL,
      NULL, REFRESHkey_FSI, TRUE, NSkey_FSI, PSkey_FSI
   };
   long nEntries; long *entryNs; int *entryTypes, entryType;
   static Logical first = TRUE;
   /***************************************************************************
   * First time...
   ***************************************************************************/
   if (first) {
       EncodeKeyDefinitions (1, trailerBrowse, ENTERkey_FSI, EXITkey_FSI,
			     HELPkey_FSI, NEXTATTRkey_EDIT);
       EncodeKeyDefinitions (2, trailerEdit, ENTERkey_FSI,
			     CREATEENTRYkey_EDIT, DELETEATTRorENTRYkey_EDIT,
			     EXITkey_FSI, HELPkey_FSI, NEXTATTRkey_EDIT);
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
   * Build attribute/entry lines and display menu.
   ***************************************************************************/
   if (!BuildAttrEntriesMenuX(G,attrN,&nEntries,
			      &entryNs,&entryTypes,&IW)) return FALSE;
   ItemWindow (NEWiw, &IW, 0);
   /***************************************************************************
   * Read/process keystrokes until request to exit menu.
   ***************************************************************************/
   for (;;) {
     ItemWindow (READiw, &IW);
     /*********************************************************************
     * Calculate entry and field numbers and entry type.
     *********************************************************************/
     if (IW.nItems != 0) {
       fieldN = IW.itemN % 4;
       entryN = entryNs[IW.itemN/4];
       entryType = entryTypes[IW.itemN/4];
     }
     switch (IW.key) {
       /***********************************************************************
       * Item selected.
       ***********************************************************************/
       case ENTERkey_FSI: {
	 /*********************************************************************
	 * Check if any attributes/entries exist.
	 *********************************************************************/
	 if (IW.nItems == 0) {
	   ProblemWindow (BOO(G,"Nothing selected (no gEntries exist).",
			        "Nothing selected (no entries exist)."),FALSE);
	   break;
	 }
	 /*********************************************************************
	 * Perform desired function (based on field number).
	 *********************************************************************/
	 switch (fieldN) {
	   /*******************************************************************
	   * Modify variable name/global entry number.
	   *******************************************************************/
	   case 0: {
	     if (G)
	       ItemWindow (BEEPiw, &IW);
	     else {
	       char varName[CDF_VAR_NAME_LEN256+1]; long nVars;
	       Logical Z = (entryType == zENTRYt);
	       status = CDFlib (GET_, BOO(Z,CDF_NUMzVARS_,
					    CDF_NUMrVARS_), &nVars,
				NULL_);
	       if (!ReportStatus(status,FALSE)) {
		 ItemWindow (DELETEiw, &IW);
		 return FALSE;
	       }
	       if (entryN >= nVars) {
		 ProblemWindow (BOO(Z,"No corresponding zVariable.",
				      "No corresponding rVariable."), FALSE);
		 break;
	       }
	       status = CDFlib (SELECT_, VAR(Z), entryN,
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
		   "Example: \"Humidity\""
	         };
	         AOSs1 (trailer,
		        "Enter: ________   Exit: ________  Help: ________")
	         static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI,
					    HELPkey_FSI, NUL };
	         static char value[1+CDF_VAR_NAME_LEN256+1+1];
	         static char label[14+CDF_VAR_NAME_LEN256+1];
	         static struct PromptWindowStruct PWt = {
		    label, 4, 1, 78, 3, header, CDF_VAR_NAME_LEN256+2, value,
		    1, trailer, exitChars, REFRESHkey_FSI, SOLkey_FSI,
		    EOLkey_FSI, INSERTorOVERkey_FSI
	         };
	         char delim;
	         EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
				       HELPkey_FSI);
	         delim = PickDelimiter (varName, strlen(varName));
	         snprintf (value, (size_t) sizeof(value),
			   "%c%s%c", delim, varName, delim);
	         snprintf (label, (size_t) sizeof(label),
			   " %cVariable %c%s%c ", BOO(Z,'z','r'),
			   delim, varName, delim);
	         PromptWindow (NEWpw, &PWt, (int) (strlen(PWt.value) - 1),
			       LogicalTRUE);
	         for (;;) {
		    if (EnterPW(&PWt,VARRENAMEhelpID)) {
		      char varName[CDF_VAR_NAME_LEN256+1];
		      if (DecodeDelimitedString(PWt.value,varName)) {
		        status = CDFlib (PUT_, VAR_NAME(Z), varName,
				         NULL_);
		        if (ReportStatus(status,FALSE)) {
			  FreeAttrEntriesMenuX (entryNs, entryTypes, &IW);
			  if (!BuildAttrEntriesMenuX(G,attrN,&nEntries,
						     &entryNs,&entryTypes,
						     &IW)) {
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
		        ProblemWindow ("Illegal name (check delimiters).",
				       FALSE);
		    }
		    else
		      break;
	         }
	         PromptWindow (DELETEpw, &PWt);
	       }
	     }
	     break;
	   }
	   /*******************************************************************
	   * Modify data specification.
	   *******************************************************************/
	   case 1: {
	     if (browseOnly)
	       ItemWindow (BEEPiw, &IW);
	     else {
	       long dataType, numElems;
	       static char label[15+CDF_VAR_NAME_LEN256+1];
	       if (G)
	         snprintf (label, (size_t) sizeof(label),
			   " gEntry %ld ", entryN + 1);
	       else {
	         Logical Z = (entryType == zENTRYt);
	         char varName[CDF_VAR_NAME_LEN256+1], delim;
	         status = CDFlib (SELECT_, VAR(Z), entryN,
				  GET_, VAR_NAME(Z), varName,
				  NULL_);
	         switch (status) {
		   case NO_SUCH_VAR:
		     snprintf (label, (size_t) sizeof(label),
			       " %cEntry %ld ",BOO(Z,'z','r'),entryN + 1);
		     break;
		   default:
		     if (!ReportStatus(status,FALSE)) {
		       ItemWindow (DELETEiw, &IW);
		       return FALSE;
		     }
		     delim = PickDelimiter (varName, strlen(varName));
		     snprintf (label, (size_t) sizeof(label),
			       " %cEntry for %c%s%c ", BOO(Z,'z','r'),
			       delim, varName, delim);
		     break;
	         }
	       }
	       status = CDFlib (SELECT_, ATTR_, attrN,
				         ENTRY(entryType), entryN,
			        GET_, ENTRY_DATATYPE(entryType), &dataType,
				      ENTRY_NUMELEMS(entryType), &numElems,
			        NULL_);
	       if (!ReportStatus(status,FALSE)) {
		 ItemWindow (DELETEiw, &IW);
		 return FALSE;
	       }
	       if (SelectDataSpec(&dataType,NULL,label)) {
		 status = CDFlib (PUT_, ENTRY_DATASPEC(entryType), dataType,
								   numElems,
				  NULL_);
		 if (ReportStatus(status,FALSE)) {
		   FreeAttrEntriesMenuX (entryNs, entryTypes, &IW);
		   if (!BuildAttrEntriesMenuX(G,attrN,&nEntries,&entryNs,
					      &entryTypes,&IW)) {
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
	   /*******************************************************************
	   * Modify data specification/value(s).
	   *******************************************************************/
	   case 2: {
	     if (browseOnly)
	       ItemWindow (BEEPiw, &IW);
	     else {
	       Logical changed = FALSE;
	       if (!EditEntry(attrN,entryN,entryType,TRUE,&changed)) {
		 ItemWindow (DELETEiw, &IW);
		 return FALSE;
	       }
	       if (changed) {
	         FreeAttrEntriesMenuX (entryNs, entryTypes, &IW);
	         if (!BuildAttrEntriesMenuX(G,attrN,&nEntries,
					    &entryNs,&entryTypes,&IW)) {
		   ItemWindow (DELETEiw, &IW);
		   return FALSE;
		 }
	         ItemWindow (UPDATEiw, &IW, IW.itemN);
	       }
	     }
	     break;
	   }
	   /*******************************************************************
	   * Modify value(s).
	   *******************************************************************/
	   case 3: {
	     Logical changed = FALSE;
	     if (!EditEntry(attrN,entryN,entryType,FALSE,&changed)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     if (changed) {
	       FreeAttrEntriesMenuX (entryNs, entryTypes, &IW);
	       if (!BuildAttrEntriesMenuX(G,attrN,&nEntries,
					  &entryNs,&entryTypes,&IW)) {
		 ItemWindow (DELETEiw, &IW);
		 return FALSE;
	       }
	       ItemWindow (UPDATEiw, &IW, IW.itemN);
	     }
	     break;
	   }
	 }
	 break;
       }
       /***********************************************************************
       * Create new entry.
       ***********************************************************************/
       case CREATEENTRYkey_EDIT: {
	 Logical changed = FALSE;
	 if (!CreateEntry(G,attrN,&changed)) {
	   ItemWindow (DELETEiw, &IW);
	   return FALSE;
	 }
	 if (changed) {
	   FreeAttrEntriesMenuX (entryNs, entryTypes, &IW);
	   if (!BuildAttrEntriesMenuX(G,attrN,&nEntries,
				      &entryNs,&entryTypes,&IW)) {
	     ItemWindow (DELETEiw, &IW);
	     return FALSE;
	   }
	   ItemWindow (UPDATEiw, &IW, 0);
	 }
	 break;
       }
       /***********************************************************************
       * Delete attribute/entry.
       ***********************************************************************/
       case DELETEATTRorENTRYkey_EDIT: {
	 char attrName[CDF_ATTR_NAME_LEN256+1], delim,
	      question1[40+ENTRYNUM_FIELD_LEN+CDF_ATTR_NAME_LEN256+1],
	      question2[31+CDF_VAR_NAME_LEN256+1];
	 if (IW.nItems == 0) {
	   ProblemWindow (BOO(G,"A gEntry isn't selected (none exist).",
			        "An entry isn't selected (none exist)."),
			  FALSE);
	   break;
	 }
	 status = CDFlib (SELECT_, ATTR_, attrN,
			  GET_, ATTR_NAME_, attrName,
			  NULL_);
	 if (!ReportStatus(status,FALSE)) {
	   ItemWindow (DELETEiw, &IW);
	   return FALSE;
	 }
	 delim = PickDelimiter (attrName, strlen(attrName));
	 if (entryType == gENTRYt) {
	   snprintf (question1, (size_t) sizeof(question1), 
		     "Delete gEntry number %ld of gAttribute %c%s%c ?",
		     entryN + 1, delim, attrName, delim);
	   question2[0] = NUL;
	 }
	 else {
	   char varName[CDF_VAR_NAME_LEN256+1], attrName[CDF_ATTR_NAME_LEN256+1];
	   status = CDFlib (SELECT_, ATTR_, attrN,
			    GET_, ATTR_NAME_, attrName,
			    SELECT_, BOO(entryType == zENTRYt,zVAR_,
							      rVAR_), entryN,
			    GET_, BOO(entryType == zENTRYt,
				      zVAR_NAME_,rVAR_NAME_), varName,
			    NULL_);
	   switch (status) {
	     case NO_SUCH_VAR:
	       snprintf (question1, (size_t) sizeof(question1),
			 "Delete %sEntry number %ld of vAttribute %c%s%c ?",
			 BOO(entryType==zENTRYt,"z","r"), entryN + 1, delim,
			 attrName, delim);
	       question2[0] = NUL;
	       break;
	     default:
	       if (!ReportStatus(status,FALSE)) {
		 ItemWindow (DELETEiw, &IW);
		 return FALSE;
	       }
	       delim = PickDelimiter (attrName, strlen(attrName));
	       snprintf (question1, (size_t) sizeof(question1),
			 "Delete %sEntry of vAttribute %c%s%c",
			 BOO(entryType==zENTRYt,"z","r"), delim, attrName,
			 delim);
	       delim = PickDelimiter (varName, strlen(varName));
	       snprintf (question2, (size_t) sizeof(question2),
			 "corresponding to %sVariable %c%s%c ?",
			 BOO(entryType==zENTRYt,"z","r"), delim, varName,delim);
	       break;
	   }
	 }
	 if (ConfirmWindow(4,78,question1,
			   BOO(NULstring(question2),NULL,question2),
			   FALSE,DELETEENTRYhelpID)) {
	   status = CDFlib (SELECT_, ENTRY(entryType), entryN,
			    DELETE_, ENTRY(entryType),
			    NULL_);
	   if (ReportStatus(status,FALSE)) {
	     int itemNt = IW.itemN - (IW.itemN % 4);
	     FreeAttrEntriesMenuX (entryNs, entryTypes, &IW);
	     if (!BuildAttrEntriesMenuX(G,attrN,&nEntries,
					&entryNs,&entryTypes,&IW)) {
	       ItemWindow (DELETEiw, &IW);
	       return FALSE;
	     }
	     if (itemNt == IW.nItems) itemNt = MaxInt (0, itemNt - 4);
	     ItemWindow (UPDATEiw, &IW, itemNt);
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
       * Next attribute (of the same scope).
       ***********************************************************************/
       case NEXTATTRkey_EDIT:
	 for (;;) {
	    attrN = (attrN + 1) % nAttrs;
	    status = CDFlib (SELECT_, ATTR_, attrN,
			     GET_, ATTR_SCOPE_, &scope,
			     NULL_);
	    if (!ReportStatus(status,FALSE)) {
	      ItemWindow (DELETEiw, &IW);
	      return FALSE;
	    }
	    if ((G && scope == GLOBAL_SCOPE) ||
		(!G && scope == VARIABLE_SCOPE)) break;
	 }
	 FreeAttrEntriesMenuX (entryNs, entryTypes, &IW);
	 if (!BuildAttrEntriesMenuX(G,attrN,&nEntries,
				    &entryNs,&entryTypes,&IW)) {
	   ItemWindow (DELETEiw, &IW);
	   return FALSE;
	 }
	 ItemWindow (UPDATEiw, &IW, 0);
	 break;
       /***********************************************************************
       * Display online help.
       ***********************************************************************/
       case HELPkey_FSI:
	 OnlineHelpWindow (ilhFile, BOO(G,GATTRENTRIEShelpIDx,
					  VATTRENTRIEShelpIDx));
	 break;
       /***********************************************************************
       * Exit menu.
       ***********************************************************************/
       case EXITkey_FSI:
	 ItemWindow (DELETEiw, &IW);
	 FreeAttrEntriesMenuX (entryNs, entryTypes, &IW);
	 return TRUE;
     }
   }
}

/******************************************************************************
* BuildAttrEntriesMenuX.
******************************************************************************/

Logical BuildAttrEntriesMenuX (G, attrN, nEntries, entryNs, entryTypes, IW)
Logical G;
long attrN;
long *nEntries;
long **entryNs;
int **entryTypes;
struct ItemWindowStruct *IW;
{
   CDFstatus status; void *binary;
   long dataType, numElems, entryN, NgEntries, NrEntries, NzEntries;
   char field[80+1], attrName[CDF_ATTR_NAME_LEN256+1];
   int entryX, curCol, n, dataSpecCol, entryType, itemN;
   int style;
   AOSs2 (loadingLines, "Loading menu...", "")
   /***************************************************************************
   * Make attribute current and get name.
   ***************************************************************************/
   status = CDFlib (SELECT_, ATTR_, attrN,
		    GET_, ATTR_NAME_, attrName,
		    NULL_);
   if (!ReportStatus(status,FALSE)) return FALSE;
   /***************************************************************************
   * Determine number of entries.
   ***************************************************************************/
   if (G) {
     status = CDFlib (GET_, ATTR_NUMgENTRIES_, &NgEntries,
		      NULL_);
     if (!ReportStatus(status,FALSE)) return FALSE;
     *nEntries = NgEntries;
   }
   else {
     status = CDFlib (GET_, ATTR_NUMrENTRIES_, &NrEntries,
		      ATTR_NUMzENTRIES_, &NzEntries,
		      NULL_);
     if (!ReportStatus(status,FALSE)) return FALSE;
     *nEntries = NrEntries + NzEntries;
   }
   if (*nEntries > MANY_ENTRYs) MessageWindow (loadingLines,NULL,LogicalFALSE);
   *entryNs = (long *) cdf_AllocateMemory ((size_t) ((*nEntries) * sizeof(long)),
				       FatalError);
   *entryTypes = (int *) cdf_AllocateMemory ((size_t) ((*nEntries) * sizeof(int)),
					 FatalError);
   if (G) {
     for (entryN = 0, entryX = 0; entryX < NgEntries; entryN++) {
	status = CDFlib (SELECT_, gENTRY_, entryN,
			 CONFIRM_, CURgENTRY_EXISTENCE_,
			 NULL_);
	switch (status) {
	  case NO_SUCH_ENTRY:
	    break;
	  default:
	    if (!ReportStatus(status,FALSE)) return FALSE;
	    (*entryNs)[entryX] = entryN;
	    (*entryTypes)[entryX] = gENTRYt;
	    entryX++;
	    break;
	}
     }
   }
   else {
     for (entryN = 0, entryX = 0; entryX < NrEntries; entryN++) {
	status = CDFlib (SELECT_, rENTRY_, entryN,
			 CONFIRM_, CURrENTRY_EXISTENCE_,
			 NULL_);
	switch (status) {
	  case NO_SUCH_ENTRY:
	    break;
	  default:
	    if (!ReportStatus(status,FALSE)) return FALSE;
	    (*entryNs)[entryX] = entryN;
	    (*entryTypes)[entryX] = rENTRYt;
	    entryX++;
	    break;
	}
     }
     for (entryN = 0; entryX < *nEntries; entryN++) {
	status = CDFlib (SELECT_, zENTRY_, entryN,
			 CONFIRM_, CURzENTRY_EXISTENCE_,
			 NULL_);
	switch (status) {
	  case NO_SUCH_ENTRY:
	    break;
	  default:
	    if (!ReportStatus(status,FALSE)) return FALSE;
	    (*entryNs)[entryX] = entryN;
	    (*entryTypes)[entryX] = zENTRYt;
	    entryX++;
	    break;
	}
     }
   }
   /***************************************************************************
   * Allocate item section control/lines.
   ***************************************************************************/
   AllocIW (IW, (int) (4 * (*nEntries)), (int) *nEntries, 80, FatalError);
   /***************************************************************************
   * Encode label/header.
   ***************************************************************************/
   snprintf (IW->label, (size_t) 12+1+CDF_ATTR_NAME_LEN256+1+9+1,
	     " %cAttribute \"%s\" Entries ",BOO(G,'g','v'),attrName);
   if (G) {
     snprintf (IW->hLines[0], (size_t)  sizeof(BLANKs78),
	       "%ld gEntr%s", NgEntries, BOO(NgEntries == 1,"y","ies"));
     strcpyX (IW->hLines[1],
	      "Entry  DataSpec  Value(s)", 0);
   }
   else {
     snprintf (IW->hLines[0], (size_t) sizeof(BLANKs78), 
	       "%ld rEntr%s, %ld zEntr%s",
	       NrEntries, BOO(NrEntries == 1,"y","ies"),
	       NzEntries, BOO(NzEntries == 1,"y","ies"));
     strcpyX (IW->hLines[1],
	      "VarName          DataSpec  Value(s)", 0);
   }
   /***************************************************************************
   * Build each attribute/entry line.
   ***************************************************************************/
   for (entryX = 0, curCol = 0; entryX < *nEntries; entryX++, curCol = 0) {
      entryType = (*entryTypes)[entryX];
      status = CDFlib (SELECT_, ENTRY(entryType), (*entryNs)[entryX],
		       GET_, ENTRY_DATATYPE(entryType), &dataType,
			     ENTRY_NUMELEMS(entryType), &numElems,
		       NULL_);
      if (!ReportStatus(status,FALSE)) return FALSE;
      /*******************************************************************
      * Setup entry number field if global scope, variable name if
      * variable scope.
      *******************************************************************/
      if (G) {
	snprintf (field, (size_t) sizeof(field),
		  "%ld", (*entryNs)[entryX] + 1);
	if (strlen(field) > (size_t) ENTRYNUM_FIELD_LENx) {
	  strcpyX (field + ENTRYNUM_FIELD_LENx - 3, "...", 0);
	}
      }
      else {
	char varName[CDF_VAR_NAME_LEN256+1];
	status = CDFlib (SELECT_, E3(entryType,
				     0,rVAR_,zVAR_), (*entryNs)[entryX],
			 GET_, E3(entryType,0,rVAR_NAME_,zVAR_NAME_), varName,
			 NULL_);
	switch (status) {
	  case NO_SUCH_VAR: {
	     Logical Z = E3(entryType,FALSE,FALSE,TRUE);
	     snprintf (field, (size_t) sizeof(field),
		       "<%sEntry.%ld>",
		       BOO(Z,"z","r"), (*entryNs)[entryX] + 1);
	     break;
	  }
	  default:
	    if (!ReportStatus(status,FALSE)) return FALSE;
	    EncodeString (strlen(varName), varName, field, 0,
			  VARNAME_FIELD_LENx);
	    break;
	}
      }
      strcpyX (IW->iLines[entryX], field, 0);
      if (strlen(field) < (size_t) BOO(G,ENTRYNUM_FIELD_LENx,
					 VARNAME_FIELD_LENx)) {
	CatNcharacters (IW->iLines[entryX],
			BOO(G,ENTRYNUM_FIELD_LENx,
			      VARNAME_FIELD_LENx) - strlen(field), (int) ' ');
      }
      CatNcharacters (IW->iLines[entryX],
		      BOO(G,ENTRYNUM_FIELD_BLANKSx,VARNAME_FIELD_BLANKSx),
		      (int) ' ');
      itemN = 4 * entryX;
      IW->iLineNs[itemN] = entryX;
      IW->iCols[itemN] = curCol;
      IW->iLens[itemN] = strlen(field);
      curCol += BOO(G,ENTRYNUM_FIELD_LENx,VARNAME_FIELD_LENx) +
		BOO(G,ENTRYNUM_FIELD_BLANKSx,VARNAME_FIELD_BLANKSx);
      /*******************************************************************
      * Setup data type & number of elements.
      *******************************************************************/
      strcpyX (field, DataTypeToken(dataType), 0);
      catchrX (field, (int) '/', 0);
      snprintf (&field[strlen(field)], (size_t) sizeof(field)-strlen(field),
		"%ld", numElems);
      if (strlen(field) > (size_t) DATASPEC_FIELD_LENx) {
	strcpyX (field + DATASPEC_FIELD_LENx - 3, "...", 0);
      }
      strcatX (IW->iLines[entryX], field, 0);
      if (strlen(field) < (size_t) DATASPEC_FIELD_LENx) {
	CatNcharacters (IW->iLines[entryX],
			DATASPEC_FIELD_LENx - strlen(field), (int) ' ');
      }
      CatNcharacters (IW->iLines[entryX], DATASPEC_FIELD_BLANKSx, (int) ' ');
      itemN = (4 * entryX) + 1;
      IW->iLineNs[itemN] = entryX;
      IW->iCols[itemN] = curCol;
      IW->iLens[itemN] = strlen(field);
      dataSpecCol = curCol;
      curCol += DATASPEC_FIELD_LENx + DATASPEC_FIELD_BLANKSx;
      /*******************************************************************
      * Setup value(s) field.
      *******************************************************************/
      binary = cdf_AllocateMemory ((size_t) (numElems * CDFelemSize(dataType)),
			       FatalError);
      status = CDFlib (GET_, ENTRY_DATA(entryType), binary,
		       NULL_);
      if (!ReportStatus(status,FALSE)) return FALSE;
      if (TT2000dataType(dataType)) style = TT2000_3_STYLE;
      else style = EPOCH0_STYLE;
      n = EncodeValuesFormat (dataType, numElems, binary,
			      &(IW->iLines[entryX][curCol]), NULL, 0,
			      BOO(G,gAttrENTRYVALUE_FIELD_LENx,
				    vAttrENTRYVALUE_FIELD_LENx),
			      style,
			      (size_t) 80-strlen(IW->iLines[entryX]));
      cdf_FreeMemory (binary, FatalError);
      /*******************************************************************
      * Set 'dataSpec/value' item control information.
      *******************************************************************/
      itemN = (4 * entryX) + 2;
      IW->iLineNs[itemN] = entryX;
      IW->iCols[itemN] = dataSpecCol;
      IW->iLens[itemN] = strlen(&(IW->iLines[entryX][dataSpecCol]));
      /*******************************************************************
      * Set `value' item control information.
      *******************************************************************/
      itemN = (4 * entryX) + 3;
      IW->iLineNs[itemN] = entryX;
      IW->iCols[itemN] = curCol;
      IW->iLens[itemN] = n;
   }
   if (*nEntries > MANY_ENTRYs) MessageWindow (NULL);
   return TRUE;
}

/******************************************************************************
* FreeAttrEntriesMenuX.
******************************************************************************/

void FreeAttrEntriesMenuX (entryNs, entryTypes, IW)
long *entryNs;
int *entryTypes;
struct ItemWindowStruct *IW;
{
  FreeIW (IW, FatalError);
  if (entryTypes != NULL) cdf_FreeMemory (entryTypes, FatalError);
  if (entryNs != NULL) cdf_FreeMemory (entryNs, FatalError);
  return;
}
