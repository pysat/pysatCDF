/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                 Toolbox of routines for CDF Toolkit (Macintosh).
*
*  Version 1.2a, 15-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  26-Oct-94, J Love	Original version.
*   V1.1  19-Sep-95, J Love	Macintosh event handling.
*   V1.1a 29-Sep-95, J Love	Macintosh dialog filtering.  Outline default
*				button.  The CDF cursor.
*   V1.2  15-Aug-95, J Love	CDF V2.6.
*   V1.2a 15-Nov-97, J Love	Windows NT (renamed functions).
*
******************************************************************************/

#define TOOLBOX2
#include "windoz.h"

#if defined(mac)
#include "cdfdist.rh"
#include "so.rh"
#endif

/******************************************************************************
* Macros.
******************************************************************************/

#define ToBottom(textH,atPoint) \
(((*textH)->nLines * (*textH)->lineHeight) - atPoint.v)

#define MAX_TE_TEXT_LEN         30000   /* 32767 doesn't seem to work. */
#define MAX_TE_TEXT_LEN_wNL     (MAX_TE_TEXT_LEN - NUMsoCOLS)

/******************************************************************************
* Local function prototypes.
******************************************************************************/

static void FlashButton PROTOARGs((DialogPtr dialog, int itemN));
static void delay PROTOARGs((double seconds));
static void UpdateCursorSO PROTOARGs((EventRecord *event));
static void SaveStatusLine PROTOARGs((char *text));

/******************************************************************************
* MacExecuteSO.
******************************************************************************/

void MacExecuteSO (exeFnc, qopFnc)
Logical (*exeFnc) PROTOARGs((int argC, char *argV[]));
Logical (*qopFnc) PROTOARGs((int *argC, char **argV[]));
{
  int argC; char **argV; EventRecord event; WindowPtr whichWindow;
  InitMacUI ();
  InitMacMenusSO ();
  if (!(*qopFnc)(&argC,&argV)) return;
  InitMacSO ();
  MacExecuteTimer (exeFnc, argC, argV);
  UpdateSOscrollBars ();
  FreeMacQOPs (argC, argV);
  for (;;) {
     SystemTask ();
     TEIdle (soTextH);
     GetNextEvent (everyEvent, &event);
       switch (event.what) {
         /*********************************************************************
         * Check if a null event;
         *********************************************************************/
         case nullEvent:
           UpdateCursorSO (&event);
           break;
         /*********************************************************************
         * Check if a mouse down event.
         *********************************************************************/
         case mouseDown: {
	   switch (FindWindow(event.where,&whichWindow)) {
             /*****************************************************************
             * Check if mouse down in menu bar.
             *****************************************************************/
             case inMenuBar: {
               long tempL = MenuSelect (event.where);
               short menuId = HighSHORTinLONG (tempL);
               short itemN = LowSHORTinLONG (tempL);
               switch (menuId) {
	         case APPLEmi:
	           switch (itemN) {
	             case ABOUTin:
	               DisplayAbout ();
	               break;
	             default: {
	               Str255 name;
	               GetItem (appleMenuHso, itemN, name);
	               OpenDeskAcc (name);
	               SetPort (soWindowP);
	               break;
	             }
	           }
	           break;
                 case FILEmi:
                   switch (itemN) {
                     case EXECUTEin:
		       if ((*qopFnc)(&argC,&argV)) {
		         MacExecuteTimer (exeFnc, argC, argV);
		         UpdateSOscrollBars ();
		         FreeMacQOPs (argC, argV);
		       }
                       break;
                     case SAVEinFILE:
                       SaveSO (FALSE);
                       break;
                     case SAVEASinFILE:
                       SaveSO (TRUE);
                       break;
                     case CLEARinFILE:
                       ResetSO (TRUE);
                       break;
                     case QUITin:
                       return;
                   }
                   break;
               }
               HiliteMenu (0);
               break;
             }
             /*****************************************************************
             * Check for mouse down in drag region of a window.
             *****************************************************************/
             case inDrag: {
               RectPtr screen = &qd.screenBits.bounds;
               Rect dragRect;
               dragRect.top = screen->top + 40;
               dragRect.left = screen->left + 40;
               dragRect.bottom = screen->bottom - 40;
               dragRect.right = screen->right - 40;
               DragWindow (whichWindow, event.where, &dragRect);
               break;
             }
             /*****************************************************************
             * Check for mouse down in system window.
             *****************************************************************/
             case inSysWindow:
               SystemClick (&event, whichWindow);
               break;
             /*****************************************************************
             * Check for mouse down in body of a window.
             *****************************************************************/
             case inContent: {
               Point tPoint = event.where; ControlHandle controlHandle;
               short partCode;
               GlobalToLocal (&tPoint);
               partCode = FindControl (tPoint, whichWindow, &controlHandle);
               if (partCode != 0) {
                 float pct;
                 short delta, toBottom, value, newAtPointV;
                 long ref = GetCRefCon (controlHandle);
                 switch (ref) {
                   case VSCROLLsoREFCON:
		     switch (partCode) {
		       case inUpButton:
			 if (soAtPoint.v > 0) {
			   delta = MINIMUM (soLineHeight, soAtPoint.v);
                           TEScroll ((short) 0, delta, soTextH);
			   soAtPoint.v -= delta;
			 }
			 break;
		       case inDownButton:
		         toBottom = ToBottom (soTextH, soAtPoint);
			 if (toBottom > 0) {
			   delta = MINIMUM (soLineHeight, toBottom);
                           TEScroll ((short) 0, -delta, soTextH);
			   soAtPoint.v += delta;
			 }
			 break;
		       case inPageUp:
		         if (soAtPoint.v > 0) {
			   delta = MINIMUM (soViewHeight, soAtPoint.v);
		           TEScroll ((short) 0, delta, soTextH);
			   soAtPoint.v -= delta;
			 }
			 break;
		       case inPageDown:
		         toBottom = ToBottom (soTextH, soAtPoint);
			 if (toBottom > 0) {
		           delta = MINIMUM (soViewHeight, toBottom);
			   TEScroll ((short) 0, -delta, soTextH);
			   soAtPoint.v += delta;
			 }
			 break;
		       case inThumb:
			 if (TrackControl(controlHandle,tPoint,NULL) != 0) {
                           if (soDestHeight > soViewHeight) {
			     value = GetCtlValue (controlHandle);
			     pct = ((float) value) /
				   ((float) (SCROLLsoMAX - SCROLLsoMIN));
			     newAtPointV = soDestHeight * pct;
			     newAtPointV -= (newAtPointV % soLineHeight);
			     delta = newAtPointV - soAtPoint.v;
			     TEScroll ((short) 0, -delta, soTextH);
			     soAtPoint.v = newAtPointV;
			   }
			 }
		         break;
		     }
		     UpdateSOscrollBars ();
                     break;
                 }
               }
               break;
             }
           }
           break;
	 }
         /*********************************************************************
         * Check if a key down event.
         *********************************************************************/
         case keyDown:
         case autoKey: {
           char keyCode = (char) ((event.message & keyCodeMask) >> 8);
           short delta, toBottom;
           switch (keyCode) {
             case PAGEup_KEYCODE:
	       if (soAtPoint.v > 0) {
		 delta = MINIMUM (soViewHeight, soAtPoint.v);
		 TEScroll ((short) 0, delta, soTextH);
	         soAtPoint.v -= delta;
	       }
               break;
             case PAGEdown_KEYCODE:
	       toBottom = ToBottom (soTextH, soAtPoint);
	       if (toBottom > 0) {
		 delta = MINIMUM (soViewHeight, toBottom);
		 TEScroll ((short) 0, -delta, soTextH);
		 soAtPoint.v += delta;
	       }
               break;
             case R_KEYCODE:
               if ((event.modifiers & cmdKey) != 0) {
	         if ((*qopFnc)(&argC,&argV)) {
	           MacExecuteTimer (exeFnc, argC, argV);
	           UpdateSOscrollBars ();
	           FreeMacQOPs (argC, argV);
	         }
               }
               break;
             case S_KEYCODE:
               if ((*soTextH)->teLength > 0) SaveSO (FALSE);
               break;
             case Q_KEYCODE:
               if ((event.modifiers & cmdKey) != 0) return;
               break;
             default:
               break;
           }
           UpdateSOscrollBars ();
           break;
         }
         /*********************************************************************
         * Check for an activation event (activate or deactivate).
         *********************************************************************/
         case activateEvt:
           if ((event.modifiers & activeFlag) != 0) {
             TEActivate (soTextH);
             ShowControl (soVscrollH);
             SetCursor (CDF_CURSOR);
           }
           else {
             TEDeactivate (soTextH);
             HideControl (soVscrollH);
           }
           break;
         /*********************************************************************
         * Check for an update event for a window.
         *********************************************************************/
         case updateEvt: {
           BeginUpdate ((WindowPtr) event.message);
           EraseRect (&soTextRect);
           TEUpdate (&soTextRect, soTextH);
           DrawControls (soWindowP);
           DrawStatusLine (NULL);
           EndUpdate ((WindowPtr) event.message);
           break;
         }
       }
  }

}

/******************************************************************************
* MacExecuteTimer.
******************************************************************************/

void MacExecuteTimer (exeFnc, argC, argV)
Logical (*exeFnc) PROTOARGs((int argC, char *argV[]));
int argC;
char *argV[];
{
  char text[SO_STATUS_LINE_LEN+1]; clock_t endClock, totalClock;
  int hour, minute, second, hundredths, i;
  ResetSO (FALSE);
  if ((*soTextH)->teLength > 0) {
    TEScroll ((short) 0, -soLineHeight, soTextH);
    soAtPoint.v += soLineHeight;
    for (i = 0; i < NUMsoCOLS - 1; i++) WriteOut (stdout, "-");
    WriteOut (stdout, "\n");
  }
  strcpyX (text, "Executing...", 0);
  CatNcharacters (text, 46, (int) ' ');
  strcatX (text, "Elapsed time: 00:00:00.00", 0);
  DrawStatusLine (text);
  startClock = clock ();
  lastSecond = 0;
  (*exeFnc) (argC, argV);
  endClock = clock ();
  totalClock = endClock - startClock;
  hour = (int) ((totalClock / CLOCKS_PER_SEC) / 3600);
  minute = (int) ((totalClock / CLOCKS_PER_SEC) / 60);
  second = (int) ((totalClock / CLOCKS_PER_SEC) % 60);
  hundredths = (int) (((totalClock % CLOCKS_PER_SEC) * 100) / CLOCKS_PER_SEC);
  strcpyX (text, "Executing...complete.", 0);
  CatNcharacters (text, 37, (int) ' ');
  snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	    "Elapsed time: %02d:%02d:%02d.%02d",
	    hour, minute, second, hundredths);
  DrawStatusLine (text);
  return;
}

/******************************************************************************
* InitMacUI.
*   Initialize the Macintosh User Interface.
******************************************************************************/

void InitMacUI () {
  InitGraf (&qd.thePort);
  InitFonts ();
  FlushEvents ((short) everyEvent, (short) 0);
  InitWindows ();
  TEInit ();
  InitDialogs (NULL);
  InitCursor ();
  return;
}

/******************************************************************************
* InitMacSO.
*   Initialize the Macintosh standard output window.
******************************************************************************/

void InitMacSO () {
  static WindowRecord wRecord;
  WindowPtr behindWindow = (WindowPtr) -1;   /* On top of all other windows. */
  soWindowP = GetNewWindow (SOri, &wRecord, behindWindow);
  ShowWindow (soWindowP);
  SetPort (soWindowP);
  TextFont (monaco);
  soTextRect.top = MARGINsoSIZE;
  soTextRect.left = MARGINsoSIZE;
  soTextRect.bottom = MARGINsoSIZE + (NUMsoROWS * FONTsoHEIGHT);
  soTextRect.right = MARGINsoSIZE + (NUMsoCOLS * FONTsoWIDTH);
  soTextH = TENew (&soTextRect, &soTextRect);
  soViewHeight = soTextRect.bottom - soTextRect.top;
  soLineHeight = (*soTextH)->lineHeight;
  soNviewLines = soViewHeight / soLineHeight;
  soDestHeight = 0;  
  soAtPoint.v = 0;
  soVscrollH = GetNewControl (SOSCROLLri, soWindowP);
  soStatusRect.top = WINDOWsoHEIGHT - STATUSareaHEIGHT;
  soStatusRect.left = 0;
  soStatusRect.bottom = WINDOWsoHEIGHT;
  soStatusRect.right = WINDOWsoWIDTH;
  DrawStatusLine (NULL);
  return;
}

/******************************************************************************
* InitMacMenusSO.
*   Initialize the Macintosh menus for the standard output window.
******************************************************************************/

void InitMacMenusSO () {
  appleMenuHso = GetMenu (APPLEri);
  AddResMenu (appleMenuHso, *((long *) "DRVR"));
  InsertMenu (appleMenuHso, 0);
  fileMenuHso = GetMenu (FILEri);
  InsertMenu (fileMenuHso, 0);
  DrawMenuBar ();
  DisableItem (fileMenuHso, SAVEinFILE);
  DisableItem (fileMenuHso, SAVEASinFILE);
  DisableItem (fileMenuHso, CLEARinFILE);
  return;
}

/******************************************************************************
* UpdateCursorSO.
******************************************************************************/

static void UpdateCursorSO (event)
EventRecord *event;
{
  WindowPtr whichWindow;
  switch (FindWindow(event->where,&whichWindow)) {
    case inContent: {
      Point tPoint = event->where; ControlHandle controlHandle;
      GlobalToLocal (&tPoint);
      if (FindControl(tPoint,whichWindow,&controlHandle) == 0)
        SetCursor (CDF_CURSOR);
      else
        SetCursor (ARROW_CURSOR);
      break;
    }
    default:
      SetCursor (ARROW_CURSOR);
      break;
  }
  return;
}

/******************************************************************************
* MacMessageDialog.
******************************************************************************/

void MacMessageDialog (severityText, messageText)
char *severityText;
char *messageText;
{
  DialogRecord dRecord; DialogPtr dialogP; static Rect rect0s = { 0,0,0,0 };
  WindowPtr behind = (WindowPtr) -1;
#ifndef __MWERKS__
  short itemN;
#else
  SInt16 itemN;
  UserItemUPP OutlineDefaultButtonUPP;
  OutlineDefaultButtonUPP = NewUserItemProc (OutlineDefaultButton);
#endif
  ParamText (CtoPstr(severityText), CtoPstr(messageText),
	     CtoPstr(""), CtoPstr(""));
  PtoCstr ((uChar *) severityText);
  PtoCstr ((uChar *) messageText);
  dialogP = GetNewDialog (MESSAGEri, &dRecord, behind);
#ifndef __MWERKS__
  SetDItem (dialogP, (short) ODBinMD, (short) userItem,
	    	(Handle) OutlineDefaultButton, &rect0s);
#else
  SetDItem (dialogP, (short) ODBinMD, (short) userItem,
	    	(Handle) OutlineDefaultButtonUPP, &rect0s);
#endif
  if (soWindowP != NULL) {				/* Only if SO. */
    HideControl (soVscrollH);
    TEDeactivate (soTextH);
  }
  ShowWindow ((WindowPtr) dialogP);
  ModalDialog (NULL, &itemN);
  CloseDialog (dialogP);
  if (soWindowP != NULL) {				/* Only if SO. */
    ShowControl (soVscrollH);
    TEActivate (soTextH);
  }
  return;
}

/******************************************************************************
* DrawStatusLine.
* Note that `soStatusLine' is blank padded just in case it got shorter.
******************************************************************************/

void DrawStatusLine (text)
char *text;
{
  int i;
  MoveTo ((short) soStatusRect.left, (short) soStatusRect.top);
  LineTo ((short) soStatusRect.right, (short) soStatusRect.top);
  MoveTo ((short) (soStatusRect.left + 2), (short) (soStatusRect.bottom - 4));
  TextFont (monaco);
  TextMode (srcCopy);
  if (text != NULL) strcpyX (soStatusLine, text, SO_STATUS_LINE_LEN);
  for (i = strlen(soStatusLine); i < SO_STATUS_LINE_LEN; i++) {
     soStatusLine[i] = ' ';
  }
  CtoPstr (soStatusLine);
  DrawString ((uChar *) soStatusLine);
  PtoCstr ((uChar *) soStatusLine);
  return;
}

/******************************************************************************
* SaveStatusLine.
******************************************************************************/

static void SaveStatusLine (text)
char *text;
{
  strcpyX (text, soStatusLine, SO_STATUS_LINE_LEN);
  return;
}

/******************************************************************************
* FreeMacQOPs.
******************************************************************************/

void FreeMacQOPs (argC, argV)
int argC;
char *argV[];
{
  int argN;
  for (argN = 0; argN < argC; argN++) cdf_FreeMemory (argV[argN], FatalError);
  cdf_FreeMemory (argV, FatalError);
  return;
}

/******************************************************************************
* CalcBounds.
******************************************************************************/

void CalcBounds (rect, width, height, topPct, leftPct)
Rect *rect;
int width;
int height;
double topPct;
double leftPct;
{
  Rect screenRect = qd.screenBits.bounds;
  short screenWidth = screenRect.right - screenRect.left;
  short screenHeight = screenRect.bottom - screenRect.top;
  rect->left = (screenWidth - width) * leftPct;
  rect->right = rect->left + width;
  rect->top = ((screenHeight -
  		MENUbarHEIGHT -
  	        TITLEbarHEIGHT -
  	        height) * topPct) + MENUbarHEIGHT + TITLEbarHEIGHT;
  rect->bottom = rect->top + height;
  return;
}

/******************************************************************************
* ResetSO.
******************************************************************************/

void ResetSO (clear)
Logical clear;
{
  if (clear) {
    TEScroll ((short) 0, soAtPoint.v, soTextH);
    soAtPoint.v = 0;
    TESetSelect (0L, (long) (*soTextH)->teLength, soTextH);
    TEDelete (soTextH);
    TESetSelect (0L, 0L, soTextH);
    soDestHeight = 0;
    DisableItem (fileMenuHso, SAVEinFILE);
    DisableItem (fileMenuHso, SAVEASinFILE);
    DisableItem (fileMenuHso, CLEARinFILE);
  }
  else {
    short toBottom = ToBottom (soTextH, soAtPoint);
    if (toBottom > 0) {
      TEScroll ((short) 0, -toBottom, soTextH);
      soAtPoint.v += toBottom;
    }
  }
  UpdateSOscrollBars ();
  soLineCount = 0;
  return;
}

/******************************************************************************
* UpdateSOscrollBars.
* Currently only a vertical scroll bar is used.
******************************************************************************/

void UpdateSOscrollBars (void) {
  float pct = BOO(soDestHeight == 0,0.0,
		  ((float) soAtPoint.v) / ((float) soDestHeight));
  short value = SCROLLsoMIN + (pct * (SCROLLsoMAX - SCROLLsoMIN));
  SetCtlValue (soVscrollH, value);
  return;
}

/******************************************************************************
* SaveSO.
* Returns TRUE if the standard output was successfully saved.
******************************************************************************/

Logical SaveSO (as)
Logical as;		/* If TRUE, prompt for file in which to save output. */
{
  Logical overWrite = FALSE;
  char errorMsg[] = "Unable to save output.";
  char fileName[DU_MAX_PATH_LEN+1], a_mode[1+1];
  FILE *fp;
  /****************************************************************************
  * If necessary, prompt for file.
  ****************************************************************************/
  if (as) {
    StandardFileReply reply;
    char prompt[] = "Enter output file:";
    char defaultName[] = "";
    StandardPutFile (CtoPstr(prompt), CtoPstr(defaultName), &reply);
    if (reply.sfGood) {
      BuildMacPath (&reply.sfFile, fileName, FALSE);
      if (reply.sfReplacing) overWrite = TRUE;
    }
    else
      return FALSE;
  }
  else {
    strcpyX (fileName, pgmName, DU_MAX_NAME_LEN);
    strcatX (fileName, ".so", DU_MAX_NAME_LEN);
    MakeLowerString (fileName);
  }
  /****************************************************************************
  * If the file exists, prompt for what to do.
  ****************************************************************************/
  if (IsReg(fileName)) {
    if (!overWrite) {
      DialogPtr dialog; DialogRecord dRecord;
      static Rect rect0s = { 0,0,0,0 }; WindowPtr behind = (WindowPtr) -1;
#ifndef __MWERKS__
	  short itemN;
#else
	  SInt16 itemN;
	  UserItemUPP OutlineDefaultButtonUPP;
	  OutlineDefaultButtonUPP = NewUserItemProc (OutlineDefaultButton);
#endif
      ParamText (CtoPstr(fileName),CtoPstr(""),CtoPstr(""),CtoPstr(""));
      PtoCstr ((uChar *) fileName);
      dialog = GetNewDialog (OFEXISTSri, &dRecord, behind);
#ifndef __MWERKS__
      SetDItem (dialog, (short) ODBinEXISTS, (short) userItem,
		        (Handle) OutlineDefaultButton, &rect0s);
#else
      SetDItem (dialog, (short) ODBinEXISTS, (short) userItem,
				(Handle) OutlineDefaultButtonUPP, &rect0s);
#endif
      HideControl (soVscrollH);
      TEDeactivate (soTextH);
      ShowWindow ((WindowPtr) dialog);
      SetCursor (ARROW_CURSOR);
      ModalDialog (NULL, &itemN);
      CloseDialog (dialog);
      TEActivate (soTextH);
      ShowControl (soVscrollH);
      switch (itemN) {
        case OVERinEXISTS:
          strcpyX (a_mode, "w", 0);
          break;
        case APPENDinEXISTS:
          strcpyX (a_mode, "a", 0);
          break;
        case CANCELinEXISTS:
          return FALSE;
      }
    }
    else
      strcpyX (a_mode, "w", 0);
  }
  else
    strcpyX (a_mode, "w", 0);
  /****************************************************************************
  * Open file.
  ****************************************************************************/
  fp = fopen (fileName, a_mode);
  if (fp == NULL) {
    DisplayError (errorMsg);
    return FALSE;
  }
  /****************************************************************************
  * Write to the file.
  ****************************************************************************/
  if ((*soTextH)->teLength > 0) {
    if (fwrite (*((*soTextH)->hText),(*soTextH)->teLength,1,fp) != 1) {
      DisplayError (errorMsg);
      fclose (fp);
      return FALSE;
    }
  }
  /****************************************************************************
  * Close file.
  ****************************************************************************/
  if (fclose(fp) == EOF) {
    DisplayError (errorMsg);
    return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* BuildMacPath.
******************************************************************************/

#define ROOT_dirID	2

Logical BuildMacPath (spec, path, stripExt)
FSSpec *spec;			/* In: File specification from File Manager. */
char path[DU_MAX_PATH_LEN+1];	/* Out: Full/relative path. */
Logical stripExt;		/* In: If TRUE, strip off file extension. */
{
  long parID = spec->parID,	/* Parent directory identifier. */
       curID;			/* Current directory identifier. */
  CInfoPBRec pb;
  char dir[DU_MAX_DIR_LEN+1];
  HGetVol (NULL, NULL, &curID);
  PstrcpyX (path, (char *) spec->name, DU_MAX_PATH_LEN);
  if (stripExt) {
    char *dot = strchr (path, '.');
    if (dot != NULL) *dot = NUL;
  }
  if (parID == curID) return TRUE;
  pb.dirInfo.ioCompletion = NULL;
  pb.dirInfo.ioNamePtr = (StringPtr) dir;
  pb.dirInfo.ioVRefNum = spec->vRefNum;
  pb.dirInfo.ioFDirIndex = -1;
  while (parID != ROOT_dirID) {
    pb.dirInfo.ioDrDirID = parID;
    if (PBGetCatInfo(&pb,FALSE) != noErr) return FALSE;
    prependX (path, ":", DU_MAX_PATH_LEN);
    PprependX (path, (char *) pb.dirInfo.ioNamePtr, DU_MAX_PATH_LEN);
    parID = pb.dirInfo.ioDrParID;
    if (parID == curID) {
      prependX (path, ":", DU_MAX_PATH_LEN);
      return TRUE;
    }
  }
  pb.dirInfo.ioDrDirID = parID;
  if (PBGetCatInfo(&pb,FALSE) != noErr) return FALSE;
  prependX (path, ":", DU_MAX_PATH_LEN);
  PprependX (path, (char *) pb.dirInfo.ioNamePtr, DU_MAX_PATH_LEN);
  return TRUE;
}

/******************************************************************************
* DisplayAbout.
******************************************************************************/

void DisplayAbout () {
  DialogRecord dRecord;
  DialogPtr dialogP;
  WindowPtr behind = (WindowPtr) -1;
  char tempS[15+1], subIncrement;
  long version, release, increment;
#ifndef __MWERKS__
  short itemN;
#else
  SInt16 itemN;
#endif
  CDFlib (GET_, LIB_VERSION_, &version,
	        LIB_RELEASE_, &release,
	        LIB_INCREMENT_, &increment,
	        LIB_subINCREMENT_, &subIncrement,
	  NULL_);
  if (subIncrement != NULL && subIncrement != " ")
    snprintf (tempS, (size_t) sizeof(tempS), "V%ld.%ld.%ld_%c",
	      version, release, increment, subIncrement);
  else
    snprintf (tempS, (size_t) sizeof(tempS), "V%ld.%ld.%ld",
              version, release, increment);
  ParamText (CtoPstr(tempS), CtoPstr(""), CtoPstr(""), CtoPstr(""));
  dialogP = GetNewDialog (ABOUTri, &dRecord, behind);
  ShowWindow ((WindowPtr) dialogP);
  ModalDialog (NULL, &itemN);
  CloseDialog (dialogP);
  return;
}

/******************************************************************************
* WriteOutMacSO.
******************************************************************************/

void WriteOutMacSO (text)
char *text;
{
  char *ptr = text, *ptrNL; size_t len;
  /**************************************************************************
  * If paging is on, first prompt for `more...' if the end of the standard
  * output screen has been reached.
  **************************************************************************/
  for (;;) {
     /***********************************************************************
     * If the maximum number of lines have been written that will fit on
     * the screen, then if paging prompt for RETURN before resetting the SO
     * window, otherwise automatically reset the SO window.
     ***********************************************************************/
     if (soLineCount == MAX_LINES_WHEN_PAGING) {
       if (pagingOn) {
	 	char key = NUL; char savedStatusLine[SO_STATUS_LINE_LEN+1];
	 	SaveStatusLine (savedStatusLine);
	 	DrawStatusLine ("Enter RETURN for more...");
	 	MacReadKeySO (&key);
	 	if (key != '\r' && key != '\n') pagingOn = FALSE;
	 	ResetSO (FALSE);
	 	DrawStatusLine (savedStatusLine);
	 	soLineCount = 0;
       }
       else
	 	ResetSO (FALSE);
     }
     /***********************************************************************
     * Write the string up to the next newline character.  If there aren't
     * any(more) newline characters, write the rest of the string.
     ***********************************************************************/
     ptrNL = strchr (ptr, Nl);
     if (ptrNL == NULL) {
       len = strlen (ptr);
       if (len + (*soTextH)->teLength > MAX_TE_TEXT_LEN) MacSOoverFlow ();
       WriteStringSO (ptr, len);
       break;
     }
     else {
       len = (int) (ptrNL - ptr + 1);
       if (len + (*soTextH)->teLength > MAX_TE_TEXT_LEN) MacSOoverFlow ();
       WriteStringSO (ptr, len);
       if ((*soTextH)->teLength > MAX_TE_TEXT_LEN_wNL)
	 	 MacSOoverFlow ();
       else
	 	 soLineCount++;
       ptr = ptrNL + 1;
       if (*ptr == NUL) break;
     }
  }
  return;
}

/******************************************************************************
* WriteStringSO.
* `TEKey' is used if the string contains one or more backspace characters
* because `TEInsert' doesn't handle them properly.  Also, all newline
* characters must first be converted to carriage return characters (for
* TextEdit).  Note that because MPW C has reversed the values normally
* associated with '\r' and '\n', the actual numeric values must be used
* ('\015' and '\012', respectively).
******************************************************************************/

void WriteStringSO (string, length)
char *string;
size_t length;
{
  int i; char *ptr;
  if (length > 0 && (*soTextH)->teLength == 0) {
    EnableItem (fileMenuHso, SAVEinFILE);
    EnableItem (fileMenuHso, SAVEASinFILE);
    EnableItem (fileMenuHso, CLEARinFILE);
  }
  ptr = (char *) cdf_AllocateMemory (length + 1, FatalError);
  strcpyX (ptr, string, length);
  for (i = 0; ptr[i] != NUL; i++) if (ptr[i] == '\012') ptr[i] = '\015';
  if (strchr(ptr,Bs) != NULL) {
    for (i = 0; ptr[i] != NUL; i++) TEKey (ptr[i], soTextH);
  }
  else
    TEInsert (ptr, (long) length, soTextH);
  cdf_FreeMemory (ptr, FatalError);
  soDestHeight = (*soTextH)->nLines * soLineHeight;
  return;
}

/******************************************************************************
* MacSOoverFlow.
******************************************************************************/

void MacSOoverFlow () {
  DialogPtr dialog; DialogRecord dRecord;
  WindowPtr behind = (WindowPtr) -1; static Rect rect0s = { 0,0,0,0 };
  char savedStatusLine[SO_STATUS_LINE_LEN+1];
#ifndef __MWERKS__
  short itemN;
#else
  SInt16 itemN;
  UserItemUPP OutlineDefaultButtonUPP;
  OutlineDefaultButtonUPP = NewUserItemProc (OutlineDefaultButton);
#endif
  SaveStatusLine (savedStatusLine);
  DrawStatusLine ("");
  dialog = GetNewDialog (SOOFLOWri, &dRecord, behind);
#ifndef __MWERKS__
  SetDItem (dialog, (short) ODBinOFLOW, (short) userItem,
		    (Handle) OutlineDefaultButton, &rect0s);
#else
  SetDItem (dialog, (short) ODBinOFLOW, (short) userItem,
		    (Handle) OutlineDefaultButtonUPP, &rect0s);
#endif
  HideControl (soVscrollH);
  TEDeactivate (soTextH);
  ShowWindow ((WindowPtr) dialog);
  SetCursor (ARROW_CURSOR);
  ModalDialog (NULL, &itemN);
  CloseDialog (dialog);
  TEActivate (soTextH);
  ShowControl (soVscrollH);
  switch (itemN) {
    case SAVEinOFLOW:
      SaveSO (FALSE);
      break;
    case SAVEASinOFLOW:
      SaveSO (TRUE);
      break;
    case CLEARinOFLOW:
      break;
    case ABORTinOFLOW:
      Exit;
  }
  ResetSO (TRUE);
  DrawStatusLine (savedStatusLine);
}

/******************************************************************************
* MacReadKeySO.
*   Read a character from the keyboard.  This routine should only be used in
* `standard output' programs.
******************************************************************************/

void MacReadKeySO (key)
char *key;
{
  EventRecord event;
  for (;;) {
     SystemTask ();
     TEIdle (soTextH);
     GetNextEvent(everyEvent,&event);
     switch (event.what) {
       /*********************************************************************
       * Null event.
       *********************************************************************/
       case nullEvent:
         UpdateCursorSO (&event);
		 break;
       /*********************************************************************
       * Key down event.
       *********************************************************************/
       case keyDown: {
		 if (ABORTkeyMAC(event)) {
	   		cdf_FreeMemory (NULL, FatalError);
	   		Exit;
	 	 }
         *key = (char) (event.message & charCodeMask);
	 	 return;
       }
       /*********************************************************************
       * Check for an activation event (activate or deactivate).
       *********************************************************************/
       case activateEvt:
		 if ((event.modifiers & activeFlag) != 0) {
		   TEActivate (soTextH);
		   ShowControl (soVscrollH);
	 	   SetCursor (CDF_CURSOR);
		 }
	 	 else {
	 	   TEDeactivate (soTextH);
	 	   HideControl (soVscrollH);
		 }
	 	 break;
       /*********************************************************************
       * Check for an update event for a window.
       *********************************************************************/
       case updateEvt: {
		 BeginUpdate ((WindowPtr) event.message);
		 EraseRect (&soTextRect);
		 TEUpdate (&soTextRect, soTextH);
		 DrawControls (soWindowP);
	  	 DrawStatusLine (NULL);
	 	 EndUpdate ((WindowPtr) event.message);
	 	 break;
       }
     }
  }
}

/******************************************************************************
* CheckForAbortSOmac.
******************************************************************************/

void CheckForAbortSOmac () {
  EventRecord event; clock_t currentClock, totalClock;
  int hour, minute, second; char text[SO_STATUS_LINE_LEN+1];
  SystemTask ();
  TEIdle (soTextH);
  GetNextEvent (everyEvent, &event);
  switch (event.what) {
    case nullEvent:
      break;
    case mouseDown: {
      WindowPtr whichWindow;
      switch (FindWindow(event.where,&whichWindow)) {
        case inMenuBar:
	  break;
        case inDrag: {
          RectPtr screen = &qd.screenBits.bounds; Rect dragRect;
          dragRect.top = screen->top + 40;
          dragRect.left = screen->left + 40;
          dragRect.bottom = screen->bottom - 40;
          dragRect.right = screen->right - 40;
          DragWindow (whichWindow, event.where, &dragRect);
          break;
		}
        case inSysWindow:
          SystemClick (&event, whichWindow);
          break;
        case inContent:
	  break;
      }
    }
    case keyDown:
      if (ABORTkeyMAC(event)) {
		cdf_FreeMemory (NULL, FatalError);
		Exit;
      }
      break;
    case activateEvt:
      if ((event.modifiers & activeFlag) != 0) {
        TEActivate (soTextH);
        ShowControl (soVscrollH);
		SetCursor (CDF_CURSOR);
      }
      else {
        TEDeactivate (soTextH);
        HideControl (soVscrollH);
      }
      break;
    case updateEvt:
      BeginUpdate ((WindowPtr) event.message);
      EraseRect (&soTextRect);
      TEUpdate (&soTextRect, soTextH);
      DrawControls (soWindowP);
      DrawStatusLine (NULL);
      EndUpdate ((WindowPtr) event.message);
      break;
  }
  currentClock = clock ();
  totalClock = currentClock - startClock;
  second = (totalClock / CLOCKS_PER_SEC) % 60;
  if (second != lastSecond) {
    hour = (totalClock / CLOCKS_PER_SEC) / 3600;
    minute = (totalClock / CLOCKS_PER_SEC) / 60;
    strcpyX (text, "Executing...", 0);
    CatNcharacters (text, 46, (int) ' ');
    snprintf (EofS(text), (size_t) sizeof(text)-strlen(text),
	      "Elapsed time: %02d:%02d:%02d.00",
	      hour, minute, second);
    DrawStatusLine (text);
    lastSecond = second;
    UpdateCursorSO (&event);
  }
  return;
}

/******************************************************************************
* OutlineDefaultButton.
******************************************************************************/
#ifndef __MWERKS__
pascal void OutlineDefaultButton (window, itemN)
DialogPtr window;
short itemN;
#else
pascal void OutlineDefaultButton (DialogPtr window, SInt16 itemN)
#endif
{
  short itemType; Handle handle; Rect rect;
  itemN = 1;	/* Set to default button's item number. */
  GetDItem ((DialogPtr) window, itemN, &itemType, &handle, &rect);
  PenSize ((short) 3, (short) 3);
  InsetRect (&rect, (short) -4, (short) -4);
  FrameRoundRect (&rect, (short) 22, (short) 22);
  PenNormal ();
  return;
}

/******************************************************************************
* FilterForSKTs.
******************************************************************************/

pascal Boolean FilterForSKTs (pb)
ParmBlkPtr pb;
{
  char *name = PtoCstr (pb->fileParam.ioNamePtr);
  Boolean filterOut = (!Trailer(name,".skt") && !Trailer(name,".SKT") &&
		       !Trailer(name,".skt;1") && !Trailer(name,".SKT;1"));
  CtoPstr (name);
  return filterOut;
}

/******************************************************************************
* FilterForCDFs.
******************************************************************************/

pascal Boolean FilterForCDFs (pb)
ParmBlkPtr pb;
{
  char *name = PtoCstr (pb->fileParam.ioNamePtr);
  Boolean filterOut = (!Trailer(name,".cdf") && !Trailer(name,".CDF") &&
		       !Trailer(name,".cdf;1") && !Trailer(name,".CDF;1"));
  CtoPstr (name);
  return filterOut;
}

/******************************************************************************
* FilterDialogQOPfsi.
******************************************************************************/
pascal Boolean FilterDialogQOPfsi (dialog, event, itemN)
DialogPtr dialog;
EventRecord *event;
#ifndef __MWERKS__
short *itemN;
#else
SInt16 *itemN;
#endif
{
  switch (event->what) {
    case keyDown: {
      char chr = (char) (event->message & charCodeMask);
      if ((event->modifiers & cmdKey) != 0) {
        if ('a' <= chr && chr <= 'z') chr ^= 0x60; 
      }
      if (chr == (char) KB_RETURN) {
        FlashButton (dialog, OKin);
        *itemN = OKin;
        return TRUE;
      }
      if (chr == (char) EXITkey_FSI) {
        FlashButton (dialog, CANCELin);
	 	*itemN = CANCELin;
		return TRUE;
      }
      break;
    }
  }
   return FALSE; 
}

/******************************************************************************
* FilterDialogQOPso.
******************************************************************************/

pascal Boolean FilterDialogQOPso (dialog, event, itemN)
DialogPtr dialog;
EventRecord *event;
#ifndef __MWERKS__
short *itemN;
#else
SInt16 *itemN;
#endif
{
  switch (event->what) {
    case keyDown: {
      char key = (char) ((event->message & keyCodeMask) >> 8);
      if (key == RETURN_KEYCODE) {
        FlashButton (dialog, OKin);
        *itemN = OKin;
        return TRUE;
      }
      if ((event->modifiers & cmdKey) != 0 && key == Q_KEYCODE) {
        FlashButton (dialog, CANCELin);
		*itemN = CANCELin;
		return TRUE;
      }
      break;
    }
  }
  return FALSE;
}

/******************************************************************************
* FlashButton.
******************************************************************************/

static void FlashButton (dialog, itemN)
DialogPtr dialog;
int itemN;
{
  short itemType, ovalWH = 15; Handle handle; Rect rect; GrafPtr currentPort;
  GetDItem (dialog, (short) itemN, &itemType, &handle, &rect);
  GetPort (&currentPort);
  SetPort ((GrafPtr) dialog);
  InvertRoundRect (&rect, ovalWH, ovalWH);
  FrameRoundRect (&rect, ovalWH, ovalWH);
  delay (0.1);  
  InvertRoundRect (&rect, ovalWH, ovalWH);
  FrameRoundRect (&rect, ovalWH, ovalWH);
  delay (0.1);
  SetPort (currentPort);
  return;
}

/******************************************************************************
* delay.
******************************************************************************/

static void delay (seconds)
double seconds;
{
  EventRecord event; long untilTick; short nullEventMask = 0;
  EventAvail (nullEventMask, &event);
  untilTick = event.when + (long) (seconds * 60);
  for (;;) {
     EventAvail (nullEventMask, &event);
     if (event.when > untilTick) break;
  }
  return;
}
