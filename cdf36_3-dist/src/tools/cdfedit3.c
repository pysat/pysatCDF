/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                    CDFedit, auxiliary routines.
*
*  Version 1.4b, 14-Dec-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  19-Nov-93, J Love     Original version.
*   V1.0a  4-Feb-94, J Love     DEC Alpha/OpenVMS port.
*   V1.1  15-Dec-94, J Love     CDF V2.5.
*   V1.1a 23-Jan-95, J Love     IRIX 6.x (64-bit).
*   V1.1b 28-Feb-95, J Love     Pass `char' as `int'.
*   V1.2  21-Mar-95, J Love     POSIX.
*   V1.2a 18-Apr-95, J Love     More POSIX.
*   V1.3   6-Sep-95, J Love     CDFexport-related changes.  FSI key
*                               definitions.
*   V1.4  26-Aug-96, J Love     CDF V2.6.
*   V1.4a 21-Feb-97, J Love	Removed RICE.
*   V1.4b 14-Dec-97, J Love	Added ALPHAVMSi encoding.
*   V3.3  10-Apr-09, M Liu      Changed "MAC" to "PPC" for encoding.
*
******************************************************************************/

#include "cdfedit.h"

/******************************************************************************
* InitializeScreen.
******************************************************************************/

void InitializeScreen () {
  create_pasteboard ();
  set_cursor_mode (CURSORoff);
  return;
}

/******************************************************************************
* CleanupScreen.
******************************************************************************/

void CleanupScreen () {
  delete_pasteboard (ERASE);
  set_cursor_mode (CURSORon);
  return;
}

/******************************************************************************
* ReportStatus.
******************************************************************************/

Logical ReportStatus (status, center)
CDFstatus status;
Logical center;
{
  if ((StatusINFO(status) && report[INFOs]) ||
      (StatusWARN(status) && report[WARNs]) ||
      (StatusERROR(status) && report[ERRORs])) CDFstatusWindow (status,center);
  return StatusOK(status);
}

/******************************************************************************
* CDFstatusWindow.
******************************************************************************/

void CDFstatusWindow (status, center)
CDFstatus status;
Logical center;
{
   static char text[CDF_STATUSTEXT_LEN+1];
   CDFlib (SELECT_, CDF_STATUS_, status,
	   GET_, STATUS_TEXT_, text,
	   NULL_);
   ProblemWindow (text, center);
   return;
}

/******************************************************************************
* ProblemWindow.
******************************************************************************/

void ProblemWindow (message, center)
char *message;
Logical center;
{
   static int exitChars[] = { NUL };
   static struct EditWindowStruct EW = {
     " Enter any key to acknowledge... ", 0, 0, 0, 0, NULL, NULL, 1, 0,
     NULL, FALSE, TRUE, exitChars, REFRESHkey_FSI, NUL, NUL, NUL, NUL, NUL,
     NUL, NUL, NUL, NUL
   };
   if (center) {
     EW.nColsTotal = MaxInt ((int)(strlen(EW.label) + 4),
			     (int)(strlen(message) + 2));
     EW.ULrow = (SCREEN_HEIGHT - 3) / 2;
     EW.ULcol = (SCREEN_WIDTH - EW.nColsTotal) / 2;
   }
   else {
     EW.nColsTotal = SCREEN_WIDTH;
     EW.ULrow = SCREEN_HEIGHT - 3;
     EW.ULcol = 0;
   }
   EW.eText = message;
   EditWindow (NEWew, &EW, LogicalTRUE);
   EditWindow (BEEPew, &EW);
   EditWindow (READew, &EW);
   EditWindow (DELETEew, &EW);
   return;
}

/******************************************************************************
* MessageWindow.
* 
*   void MessageWindow (char **lineS,   Terminate with a NUL-string ("").
*                       char *label,
*                       Logical block);
*
******************************************************************************/

#if defined(STDARG)
void MessageWindow (char **lineS, ...)
#else
void MessageWindow (va_alist)
va_dcl
#endif
{
#if !defined(STDARG)
  char **lineS;
#endif
  static int exitChars[] = { NUL };
  static struct EditWindowStruct EW = {
    NULL, 0, 0, 0, 0, NULL, NULL, 0, 0, NULL, FALSE, TRUE, exitChars,
    REFRESHkey_FSI, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL
  };
  va_list ap;
#if defined(STDARG)
  va_start (ap, lineS);
#else
  VA_START (ap);
  lineS = va_arg (ap, char **);
#endif
  if (lineS == NULL) {
    EditWindow (DELETEew, &EW);
    cdf_FreeMemory (EW.eText, FatalError);
  }
  else {
    int lineN; size_t textLength;
    char *label = va_arg (ap, char *);
    Logical block = va_arg (ap, Logical);
    EW.nColsTotal = BOO(label == NULL,0,strlen(label)+4);
    for (textLength = 0, lineN = 0; !NULstring(lineS[lineN]); lineN++) {
       size_t length = strlen (lineS[lineN]);
       EW.nColsTotal = MaxInt (EW.nColsTotal, (int) (length + 2));
       textLength += length + 1;
    }
    EW.eText = (char *) cdf_AllocateMemory ((size_t)textLength + 1, FatalError);
    MakeNUL (EW.eText);
    for (lineN = 0; !NULstring(lineS[lineN]); lineN++) {
       strcatX (EW.eText, lineS[lineN], textLength);
       strcatX (EW.eText, "\n", textLength);
    }
    EW.NeRows = lineN;
    EW.label = label;
    EW.ULrow = 5;
    EW.ULcol = (SCREEN_WIDTH - EW.nColsTotal) / 2;
    EditWindow (NEWew, &EW, LogicalTRUE);
    if (block) EditWindow (READew, &EW);
  }
  va_end (ap);
  return;
}

/******************************************************************************
* ConfirmWindow.
******************************************************************************/

Logical ConfirmWindow (rowN, width, question1, question2, yesInitial, helpId)
int rowN;
int width;
char *question1;
char *question2;        /* If NULL, a one line question. */
Logical yesInitial;     /* If TRUE, <YES> is initial current item. */
int helpId;
{
  static char label[] = " Confirmation ";
  static char *header[] = { NULL, NULL };
  AOSs1 (trailer, "Select: ________   Exit: ________   Help: ________")
  static char *items[] = {
    "",
    "                  <Yes>                              <No>",
    ""
  };
  static int iLineNs[] = { 1, 1 };
  static int iCols[] = { 18, 53 };
  static int iLens[] = { 5, 4 };
  static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL };
  static struct ItemWindowStruct IWt = {
    0, 0, 0, label, 0, header, 3, items, 2, iLineNs, iCols, iLens, 3,
    1, trailer, exitChars, REFRESHkey_FSI, FALSE
  };
  Logical answer;
  EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
                        HELPkey_FSI);
  IWt.hLines[0] = question1;
  IWt.NhLines = 1; 
  if (question2 != NULL) {
    header[1] = question2;
    IWt.NhLines = 2;
  }
  IWt.ULrow = rowN;
  IWt.ULcol = (SCREEN_WIDTH - width) / 2;
  IWt.nColsTotal = width;
  ItemWindow (NEWiw, &IWt, BOO(yesInitial,0,1));
  if (EnterIW(&IWt,helpId))
    switch (IWt.itemN) {
      case 0:
        answer = TRUE;
        break;
      case 1:
        answer = FALSE;
        break;
    }
  else
    answer = FALSE;
    ItemWindow (DELETEiw, &IWt);
  return answer;
}

/******************************************************************************
* ConfirmWindow2.
******************************************************************************/

Logical ConfirmWindow2 (rowN, width, question1, question2, yesInitial, helpId)
int rowN;
int width;
char *question1;
char *question2;        /* If NULL, a one line question. */
Logical yesInitial;     /* If TRUE, <YES> is initial current item. */
int helpId;
{
  static char label[] = " Confirmation ";
  static char *header[] = { NULL, NULL };
  AOSs1 (trailer, "Select: ________   Exit: ________   Help: ________")
  static char *items1[] = {
    "",
    "                  <Yes>                              <No>",
    ""
  };
  static char *items2[] = {
    "",
    "           <Yes to V3>        <Yes to V2>            <No>",
    ""
  };
  static int iLineNs1[] = { 1, 1 };
  static int iLineNs2[] = { 1, 1, 1 };
  static int iCols1[] = { 18, 53 };
  static int iLens1[] = { 5, 4 };
  static int iCols2[] = { 11, 30, 53 };
  static int iLens2[] = { 11, 11, 4 };
  static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL };
  static struct ItemWindowStruct IWt;
  static struct ItemWindowStruct IWt1 = {
    0, 0, 0, label, 0, header, 3, items1, 2, iLineNs1, iCols1, iLens1, 3,
    1, trailer, exitChars, REFRESHkey_FSI, FALSE
  };
  static struct ItemWindowStruct IWt2 = {
    0, 0, 0, label, 0, header, 3, items2, 3, iLineNs2, iCols2, iLens2, 3,
    1, trailer, exitChars, REFRESHkey_FSI, FALSE
  };
  Logical answer;
  EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
			HELPkey_FSI);
  if (CDFgetFileBackwardEnvVar() == 1) 
    IWt = IWt1;
  else
    IWt = IWt2;
  
  IWt.hLines[0] = question1;
  IWt.NhLines = 1;
  if (question2 != NULL) {
    header[1] = question2;
    IWt.NhLines = 2;
  }
  IWt.ULrow = rowN;
  IWt.ULcol = (SCREEN_WIDTH - width) / 2;
  IWt.nColsTotal = width;
  ItemWindow (NEWiw, &IWt, 0);
  if (EnterIW(&IWt,helpId)) {
    if (CDFgetFileBackwardEnvVar() == 1) {
      switch (IWt.itemN) {
        case 0:
	  answer = TRUE;
	  break;
        case 1:
	  answer = FALSE;
	  break;
      }
    } else {
      switch (IWt.itemN) {
        case 0:
          answer = TRUE;
          CDFsetFileBackward(BACKWARDFILEoff);
          break;
        case 1:
          answer = TRUE;
          CDFsetFileBackward(BACKWARDFILEon);
          break;
        case 2:
          answer = FALSE;
          break;
      }
    }
  } else
    answer = FALSE;
  ItemWindow (DELETEiw, &IWt);
  return answer;
}

/******************************************************************************
* EnterIW.
******************************************************************************/

Logical EnterIW (IW, helpId)
struct ItemWindowStruct *IW;
int helpId;
{
  for (;;) {
     ItemWindow (READiw, IW);
     switch (IW->key) {
       case EXITkey_FSI:
	 return FALSE;
       case ENTERkey_FSI:
	 return TRUE;
       case HELPkey_FSI:
	 OnlineHelpWindow (ilhFile, helpId);
	 break;
     }
  }
}

/******************************************************************************
* EnterPW.
******************************************************************************/

Logical EnterPW (PW, helpId)
struct PromptWindowStruct *PW;
int helpId;
{
  for (;;) {
     PromptWindow (READiw, PW);
     switch (PW->key) {
       case EXITkey_FSI:
	 return FALSE;
       case ENTERkey_FSI:
	 return TRUE;
       case HELPkey_FSI:
	 OnlineHelpWindow (ilhFile, helpId);
	 break;
     }
  }
}

/******************************************************************************
* IndexInList.
******************************************************************************/

int IndexInList (match, nItems, items)
char *match;
int nItems;
char **items;
{
  int i;
  for (i = 0; i < nItems; i++) if (!strcmp(match,items[i])) return i;
  return -1;
}

/******************************************************************************
* DataTypeItemN.
******************************************************************************/

int DataTypeItemN (dataType)
long dataType;
{
  static char *symbols[] = { DTiwSYMBOLS };
  static int colums[] = { DTiwCOLs };
  static int lineNs[] = { DTiwLINEnS };
  static int lengths[] = { DTiwLENs };
  char token[11+1];     /* Assumes `CDF_EPOCH16' is longest data type token. */
  int itemN; char *symbol;
  for (itemN = 0; itemN < 15; itemN++) {
     strcpyX (token, "CDF_", 11);
     strcatX (token, DataTypeToken(dataType), 11);
     symbol = &(symbols[lineNs[itemN]][colums[itemN]]);
     if (!strncmp(token,symbol,lengths[itemN])) return itemN;
  }
  return 0;
}

/******************************************************************************
* ShowFullName.
******************************************************************************/

void ShowFullName (name, attr)
char *name;
Logical attr;   /* TRUE: attribute, FALSE: variable. */
{
  AOSs6 (lineS, " ",BLANKs78," ","  Enter any key to continue...  "," ","")
  size_t length = strlen (name);
  char delim = PickDelimiter (name, length);
  snprintf (lineS[1], (size_t) sizeof(BLANKs78),
	    "  %c%s%c  ", delim, name, delim);
  MessageWindow (lineS, BOO(attr,"Full attribute name...",
				 "Full variable name..."), LogicalTRUE);
  MessageWindow (NULL);
  return;
}

/******************************************************************************
* SelectMajority.
******************************************************************************/

Logical SelectMajority () {
  long majority; int itemN; CDFstatus status;
  static char *header[] = { "Select new majority..." };
  AOSs1 (trailer, "Select: ________   Exit: ________   Help:   ________")
  static char *items[] = { "ROW", "COLUMN" };
  static int iLineNs[] = { 0, 1 };
  static int iCols[] = { 0, 0 };
  static int iLens[] = { 3, 6 };
  static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL };
  static struct ItemWindowStruct IWt = {
    12, 0, 80, NULL, 1, header, 2, items, 2, iLineNs, iCols, iLens, 3, 1,
    trailer, exitChars, REFRESHkey_FSI, FALSE
  };
  EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI);
  status = CDFlib (GET_, CDF_MAJORITY_, &majority,
		   NULL_);
  if (!ReportStatus(status,FALSE)) return FALSE;
  for (itemN = 0; itemN < 2; itemN++) {
     if (!strcmp(MajorityToken(majority),IWt.iLines[itemN])) break;
  }
  ItemWindow (NEWiw, &IWt, itemN);
  for (;;) {
     if (EnterIW(&IWt,MAJORITYhelpID)) {
       long majority = WhichMajority(items[IWt.itemN]);
       status = CDFlib (PUT_, CDF_MAJORITY_, majority,
			NULL_);
       if (ReportStatus(status,FALSE)) {
	 ItemWindow (DELETEiw, &IWt);
	 return TRUE;
       }
       if (NoMoreAccess(NULL)) {
	 ItemWindow (DELETEiw, &IWt);
	 return FALSE;
       }
     }
     else
       break;
  }
  ItemWindow (DELETEiw, &IWt);
  return TRUE;
}

/******************************************************************************
* SelectFormat.
******************************************************************************/

Logical SelectFormat () {
  long format; int itemN; CDFstatus status;
  static char *header[] = { "Select new format..." };
  AOSs1 (trailer, "Select: ________   Exit: ________   Help:   ________")
  static char *items[] = { "SINGLE", "MULTI" };
  static int iLineNs[] = { 0, 1 };
  static int iCols[] = { 0, 0 };
  static int iLens[] = { 6, 5 };
  static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL };
  static struct ItemWindowStruct IWt = {
    12, 0, 80, NULL, 1, header, 2, items, 2, iLineNs, iCols, iLens, 3, 1,
    trailer, exitChars, REFRESHkey_FSI, FALSE
  };
  EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI);
  status = CDFlib (GET_, CDF_FORMAT_, &format,
		   NULL_);
  if (!ReportStatus(status,FALSE)) return FALSE;
  for (itemN = 0; itemN < 2; itemN++) {
     if (!strcmp(FormatToken(format),IWt.iLines[itemN])) break;
  }
  ItemWindow (NEWiw, &IWt, itemN);
  for (;;) {
     if (EnterIW(&IWt,FORMAThelpID)) {
       format = WhichFormat(items[IWt.itemN]);
       status = CDFlib (PUT_, CDF_FORMAT_, format,
			NULL_);
       if (ReportStatus(status,FALSE)) {
	 ItemWindow (DELETEiw, &IWt);
	 return TRUE;
       }
       if (NoMoreAccess(NULL)) {
	 ItemWindow (DELETEiw, &IWt);
	 return FALSE;
       }
     }
     else
       break;
  }
  ItemWindow (DELETEiw, &IWt);
  return TRUE;
}

/******************************************************************************
* SelectEncoding.
******************************************************************************/

Logical SelectEncoding () {
  long encoding; int itemN; CDFstatus status;
  static char *header[] = { "Select new encoding..." };
  AOSs1 (trailer, "Select: ________   Exit: ________   Help:   ________")
  static char *items[] = {
    "HOST      SUN   IBMPC   SGi   HP     ALPHAVMSg   ALPHAVMSi   DECSTATION",
    "NETWORK   VAX   IBMRS   PPC   NeXT   ALPHAVMSd   ALPHAOSF1"
  };
  static int iLineNs[] = { 0,0,0,0,0,0,0,0,1,1,1,1,1,1,1 };
  static int iCols[] = { 0,10,16,24,30,37,49,61,0,10,16,24,30,37,49 };
  static int iLens[] = { 4,3,5,3,2,9,9,10,7,3,5,3,4,9,9 };
  static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL };
  static struct ItemWindowStruct IWt = {
    12, 0, 80, NULL, 1, header, 2, items, 15, iLineNs, iCols, iLens, 3, 1,
    trailer, exitChars, REFRESHkey_FSI, FALSE
  };
  char selectedEncoding[10+1], delim = ' ', *selected = NULL, *tmpStr;

  EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI);
  status = CDFlib (GET_, CDF_ENCODING_, &encoding,
		   NULL_);
  if (!ReportStatus(status,FALSE)) return FALSE;
  for (itemN = 0; itemN < 15; itemN++) {
     if (!strncmp(EncodingToken(encoding),
		  &(IWt.iLines[IWt.iLineNs[itemN]][IWt.iCols[itemN]]),
		  IWt.iLens[itemN])) break;
  }
  ItemWindow (NEWiw, &IWt, itemN);
  for (;;) {
     if (EnterIW(&IWt,ENCODINGhelpID)) {
       tmpStr = &items[iLineNs[IWt.itemN]][iCols[IWt.itemN]];
       selected = strchr(tmpStr, delim);   
       if (selected != NULL) 
         strcpyX (selectedEncoding, tmpStr, selected - tmpStr);
       else
         strcpyX (selectedEncoding, tmpStr, strlen(tmpStr));
       encoding = WhichEncoding(selectedEncoding);
       status = CDFlib (PUT_, CDF_ENCODING_, encoding,
			NULL_);
       if (ReportStatus(status,FALSE)) {
	 ItemWindow (DELETEiw, &IWt);
	 return TRUE;
       }
       if (NoMoreAccess(NULL)) {
	 ItemWindow (DELETEiw, &IWt);
	 return FALSE;
       }
     }
     else
       break;
  }
  ItemWindow (DELETEiw, &IWt);
  return TRUE;
}

/******************************************************************************
* SelectSparseness.
******************************************************************************/

Logical SelectSparseness (zVar, changed)
Logical zVar;
Logical *changed;	/* Assumed initialized to FALSE by caller. */
{
  long sRecordsType; CDFstatus status; static Logical first = TRUE;
  static char *header[] = { "Select sparseness..." };
  AOSs1 (trailer, "Select: ________   Exit: ________   Help: ________")
  static char *items[] = {
    "sRecords.NO     sRecords.PAD     sRecords.PREV"
  };
  static int iLineNs[] = { 0,0,0 };
  static int iCols[] = { 0,16,33 };
  static int iLens[] = { 11,12,13 };
  static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL };
  static struct ItemWindowStruct IWt = {
    14, 0, 80, NULL, 1, header, 1, items, 3, iLineNs, iCols, iLens, 1, 1,
    trailer, exitChars, REFRESHkey_FSI, TRUE
  };
  static long sRecordsTypes[] = {
    NO_SPARSERECORDS, PAD_SPARSERECORDS, PREV_SPARSERECORDS
  };
  if (first) {
    EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI);
    first = FALSE;
  }
  ItemWindow (NEWiw, &IWt, 0);
  for (;;) {
     if (EnterIW(&IWt,SPARSENESShelpID)) {
       sRecordsType = sRecordsTypes[IWt.itemN];
       status = CDFlib (PUT_, BOO(zVar,zVAR_SPARSERECORDS_,
				       rVAR_SPARSERECORDS_), sRecordsType,
		        NULL_);
       if (ReportStatus(status,FALSE)) {
	 ItemWindow (DELETEiw, &IWt);
	 ASSIGNnotNULL (changed, TRUE)
	 return TRUE;
       }
       if (NoMoreAccess(NULL)) {
	 ItemWindow (DELETEiw, &IWt);
	 return FALSE;
       }
     }
     else {
       ItemWindow (DELETEiw, &IWt);
       return TRUE;
     }
  }
}

/******************************************************************************
* SelectCompression.
******************************************************************************/

Logical SelectCompression (forWhat, row, changed)
int forWhat;
int row;
Logical *changed;	/* Assumed initialized to FALSE by caller. */
{
  long cType, cParms[CDF_MAX_PARMS], compressionItem;
  CDFstatus status; static Logical first = TRUE;
  static char *header[] = { "Select compression..." };
  AOSs1 (trailer, "Select: ________   Exit: ________   Help: ________")
  static char *items[] = {
    "None     RLE.0     HUFF.0     AHUFF.0     GZIP..."
  };
  static int iLineNs[] = { 0,0,0,0,0 };
  static int iCols[] = { 0,9,19,30,42 };
  static int iLens[] = { 4,5,6,7,7 };
  static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL };
  static struct ItemWindowStruct IW = {
    0, 0, 80, NULL, 1, header, 1, items, nCOMPRESSIONs, iLineNs, iCols,
    iLens, 1, 1, trailer, exitChars, REFRESHkey_FSI, TRUE
  };
  if (first) {
    EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI);
    first = FALSE;
  }
  switch (forWhat) {
    case FORCDF: compressionItem = CDF_COMPRESSION_; break;
    case FORzVAR: compressionItem = zVAR_COMPRESSION_; break;
    case FORrVAR: compressionItem = rVAR_COMPRESSION_; break;
  }
  IW.ULrow = row;
  ItemWindow (NEWiw, &IW, 0);
  for (;;) {
     if (EnterIW(&IW,BOO(forWhat == FORCDF,
			 CDFCOMPRESSIONhelpID,
			 VARCOMPRESSIONhelpID))) {
       switch (IW.itemN) {
	 case NOcompressionIN:
	   cType = NO_COMPRESSION;
	   break;
	 case RLE0compressionIN:
	   cType = RLE_COMPRESSION;
	   cParms[0] = RLE_OF_ZEROs;
	   break;
	 case HUFF0compressionIN:
	   cType = HUFF_COMPRESSION;
	   cParms[0] = OPTIMAL_ENCODING_TREES;
	   break;
	 case AHUFF0compressionIN:
	   cType = AHUFF_COMPRESSION;
	   cParms[0] = OPTIMAL_ENCODING_TREES;
	   break;
	 case GZIPcompressionIN: {
	   static char *gzipHeader[] = {
	     "Syntax: GZIP.<1|2|3|4|5|6|7|8|9>"
	   };
	   AOSs1A (gzipTrailer,
		   "Enter: ________   Exit: ________   Help: ________")
	   static char token[MAX_GZIP_TOKEN_LEN+1];
	   static Logical gzipFirst = TRUE;
	   static int gzipExit[] = {
	     ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL
	   };
	   static struct PromptWindowStruct PW = {
	     "Enter GZIP compression parameters...", 0, 0, 80, 1, gzipHeader,
	     MAX_GZIP_TOKEN_LEN, token, 1, gzipTrailer, gzipExit,
	     REFRESHkey_FSI, SOLkey_FSI, EOLkey_FSI, INSERTorOVERkey_FSI
	   };
	   if (gzipFirst) {
	     EncodeKeyDefinitions (1, gzipTrailer,
				   ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI);
	     strcpyX (PW.value, "GZIP.", MAX_GZIP_TOKEN_LEN);
	     gzipFirst = FALSE;
	   }
	   PW.ULrow = row;
	   PromptWindow (NEWpw, &PW, (int) strlen(PW.value), LogicalTRUE);
	   for (;;) {
	      if (EnterPW(&PW,GZIPhelpID)) {
		int level;
	        if (sscanf(PW.value,"GZIP.%d",&level) == 1) {
		  if (INCLUSIVE(1,level,9)) {
		    cType = GZIP_COMPRESSION;
		    cParms[0] = level;
		    PromptWindow (DELETEpw, &PW);
		    break;
		  }
		  else
		    ProblemWindow ("Illegal GZIP level.", FALSE);
		}
		else
		  ProblemWindow ("Illegal GZIP parameter.", FALSE);
	      }
	      else {
		PromptWindow (DELETEpw, &PW);
		ItemWindow (DELETEiw, &IW);
		return TRUE;
	      }
	   }
	   break;
	 }
       }
       ItemWindow (UNDISPLAYiw, &IW);
       if (forWhat == FORCDF) {
	 AOSs2 (compressingLines, "Changing compression...", "")
	 MessageWindow (compressingLines, NULL, LogicalFALSE);
	 zzzzz (1.0);
       }
       status = CDFlib (PUT_, compressionItem, cType, cParms,
			NULL_);
       if (forWhat == FORCDF) MessageWindow (NULL);
       if (ReportStatus(status,FALSE)) {
	 ItemWindow (DELETEiw, &IW);
	 if (forWhat == FORCDF) compressed = (cType != NO_COMPRESSION);
	 ASSIGNnotNULL (changed, TRUE)
	 return TRUE;
       }
       if (NoMoreAccess(NULL)) {
	 ItemWindow (DELETEiw, &IW);
	 return FALSE;
       }
       ItemWindow (REDISPLAYiw, &IW);
     }
     else {
       ItemWindow (DELETEiw, &IW);
       return TRUE;
     }
  }
}

/******************************************************************************
* SelectDataSpec.
******************************************************************************/

Logical SelectDataSpec (dataType, numElems, label)
long *dataType;         /* If NO_DATATYPE, start at item number 0. */
long *numElems;         /* If NULL, don't prompt for number of elements. */
char *label;            /* If NULL, no label. */
{
  static char *header[] = { "Select data type..." };
  static char *items[] = { DTiwSYMBOLS };
  AOSs1 (trailer, "Select: ________    Exit: ________    Help: ________")
  static int iLineNs[] = { DTiwLINEnS };
  static int iCols[] = { DTiwCOLs };
  static int iLens[] = { DTiwLENs };
  static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL };
  static struct ItemWindowStruct IWt = {
    4, 1, 78, NULL, 1, header, 3, items, 17, iLineNs, iCols, iLens, 3, 1,
    trailer, exitChars, REFRESHkey_FSI, FALSE
  };
  static Logical first = TRUE; int itemN;
  char selectedDataType[15+1], delim = ' ', *selected = NULL, *tmpStr;
  if (first) {
    EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
			  HELPkey_FSI);
    first = FALSE;
  }
  IWt.label = label;
  itemN = BOO(*dataType == NO_DATATYPE,0,DataTypeItemN(*dataType));
  ItemWindow (NEWiw, &IWt, itemN);
  if (EnterIW(&IWt,VARDATATYPEhelpID)) {
    tmpStr = &items[iLineNs[IWt.itemN]][iCols[IWt.itemN]];
    selected = strchr(tmpStr, delim);
    if (selected != NULL) 
      strcpyX (selectedDataType, tmpStr, selected - tmpStr);
    else
      strcpyX (selectedDataType, tmpStr, strlen(tmpStr));
    *dataType = WhichDataType(selectedDataType);
    ItemWindow (DELETEiw, &IWt);
    if (numElems == NULL) return TRUE;
    if (STRINGdataType(*dataType)) {
      return SelectNumElems(*dataType,numElems,label);
    }
    *numElems = 1;
    return TRUE;
  }
  ItemWindow (DELETEiw, &IWt);
  return FALSE;
}

/******************************************************************************
* SelectNumElems.
******************************************************************************/

Logical SelectNumElems (dataType, numElems, label)
long dataType;
long *numElems;
char *label;
{
  static char *headerCHAR[] = {
    "Enter number of elements (characters) for CDF_CHAR data type..."
  };
  static char *headerUCHAR[] = {
    "Enter number of elements (characters) for CDF_UCHAR data type..."
  };
  AOSs1 (trailer, "Enter: ________    Exit: ________    Help: ________")
  static char value[MAX_NUMELEMS_LEN+1];
  static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL };
  static struct PromptWindowStruct PWt = {
    NULL, 4, 1, 78, 1, NULL, MAX_NUMELEMS_LEN, value, 1, trailer, exitChars,
    REFRESHkey_FSI, SOLkey_FSI, EOLkey_FSI, INSERTorOVERkey_FSI
  };
  static Logical first = TRUE; long tNumElems;
  if (first) {
    EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI,
			  HELPkey_FSI);
    first = FALSE;
  }
  strcpyX (PWt.value, "", MAX_NUMELEMS_LEN);
  PWt.hLines = BOO(dataType == CDF_CHAR,headerCHAR,headerUCHAR);
  PWt.label = label;
  PromptWindow (NEWpw, &PWt, 0, LogicalTRUE);
  for (;;) {
     if (EnterPW(&PWt,VARNUMELEMShelpID)) {
       if (sscanf(PWt.value,"%ld",&tNumElems) == 1) {
	 if (tNumElems > 0) {
	   PromptWindow (DELETEpw, &PWt);
	   *numElems = tNumElems;
	   return TRUE;
	 }
	 else
	   ProblemWindow ("Illegal number of elements.", FALSE);
       }
       else
	 ProblemWindow ("Error decoding number of elements.", FALSE);
     }
     else {
       PromptWindow (DELETEpw, &PWt);
       return FALSE;
     }
  }
}

/******************************************************************************
* SelectChecksum.
******************************************************************************/

Logical SelectChecksum () {
  long checksum; int itemN; CDFstatus status;
  static char *header[] = { "Select new checksum method..." };
  AOSs1 (trailer, "Select: ________   Exit: ________   Help:   ________")
  static char *items[] = { "None", "MD5" };
  static int iLineNs[] = { 0, 1 };
  static int iCols[] = { 0, 0 };
  static int iLens[] = { 4, 3 };
  static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL };
  static struct ItemWindowStruct IWt = {
    12, 0, 80, NULL, 1, header, 2, items, 2, iLineNs, iCols, iLens, 3, 1,
    trailer, exitChars, REFRESHkey_FSI, FALSE
  };
  EncodeKeyDefinitions (1, trailer, ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI);
  status = CDFlib (GET_, CDF_CHECKSUM_, &checksum,
		   NULL_);
  if (!ReportStatus(status,FALSE)) return FALSE;
  for (itemN = 0; itemN < 2; itemN++) {
     if (!strcmp(ChecksumToken(checksum),IWt.iLines[itemN])) break;
  }
  ItemWindow (NEWiw, &IWt, itemN);
  for (;;) {
     if (EnterIW(&IWt,CHECKSUMhelpID)) {
       checksum = WhichChecksum(items[IWt.itemN]);
       status = CDFlib (PUT_, CDF_CHECKSUM_, checksum,
			NULL_);
       if (ReportStatus(status,FALSE)) {
	 ItemWindow (DELETEiw, &IWt);
	 return TRUE;
       }
       if (NoMoreAccess(NULL)) {
	 ItemWindow (DELETEiw, &IWt);
	 return FALSE;
       }
     }
     else
       break;
  }
  ItemWindow (DELETEiw, &IWt);
  return TRUE;
}

