/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                            Windowing widgets.
*
*  Version 1.3b, 13-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  19-Nov-93, J Love     Original version.
*   V1.0a 22-Feb-94, J Love     Spelling lesson.
*   V1.0b 28-Mar-94, J Love     Solaris using GNU C compiler.
*   V1.1  13-Dec-94, J Love     CDF V2.5.
*   V1.1a 23-Jan-95, J Love     Primary/alternate "current" item movement
*                               keys for ItemWindow.
*   V1.1b  7-Mar-95, J Love     Added FieldWindow.
*   V1.2  11-Apr-95, J Love     POSIX/CDFexport.
*   V1.2a 10-May-95, J Love     More CDFexport.
*   V1.2b 13-Jun-95, J Love     catchrX.  Linux.
*   V1.2c 30-Aug-95, J Love     CDFexport-related changes.  Moved online help
*                               widget to this file.  FSI key definitions.
*   V1.2d 18-Sep-95, J Love     Macintosh event handling.
*   V1.3  26-Aug-96, J Love     CDF V2.6.
*   V1.3a 31-Mar-97, J Love     Allowed fields in a FieldWindow to be longer
*                               than their on-screen width.
*   V1.3b 13-Nov-97, J Love	Windows NT/Visual C++.
*   V1.4  11-Jul-05, M Liu      Added MingW port for PC.
*
******************************************************************************/

#include "widgets.h"

/******************************************************************************
* Local macros.
******************************************************************************/

#define MAX_PCT_LEN     5

#define DIFF(a,b)       BOO((a < b),(b - a),(a - b))

/******************************************************************************
* Local function prototypes.
******************************************************************************/

static void CalcItemDirections PROTOARGs((
  int nItems, int NiLines, int *iLineNs, int *iCols, int *iLens,
  int *iDownTo, int *iUpTo, int *iLeftTo, int *iRightTo
));
static void DownArrow PROTOARGs((
  int *iLineNs, int *iDownTo, int iRowT, int iRowB, Logical iScroll,
  int *iRowN, int *itemN
));
static void UpArrow PROTOARGs((
  int NiLines, int *iLineNs, int *iUpTo, int iRowT, int iRowB, Logical iScroll,
  int *iRowN, int *itemN
));
static void NextScreen PROTOARGs((
  int NiRows, int NiLines, int *iLineNs, int *iDownTo, int iRowB, int iRowT,
  Logical iScroll, int *itemN, int *iRowN
));
static void PrevScreen PROTOARGs((
  int NiRows, int *iLineNs, int *iUpTo, int iRowT, Logical iScroll, int *iRowN,
  int *itemN
));
static void UpdatePctString PROTOARGs((WINDOWid, int, int, int, int, int));
static void DrawSection PROTOARGs((
  WINDOWid, int, int, int, int, char **, int
));
static void DrawField PROTOARGs((struct FieldWindowStruct *FW));
static void DrawFieldsSection PROTOARGs((struct FieldWindowStruct *FW));
static int IWtopRowLineN PROTOARGs((struct ItemWindowStruct *IW));
static int FWtopRowLineN PROTOARGs((struct FieldWindowStruct *FW));
static void DrawEditSection PROTOARGs((struct EditWindowStruct *EW));
static void SetEditCursor PROTOARGs((struct EditWindowStruct *EW));

/******************************************************************************
* ItemWindow.
*
*       Header section.
*       Items section (scrollable).
*       Trailer section.
*
******************************************************************************/

#if defined(STDARG)
int ItemWindow (int opT, ...)
#else
int ItemWindow (va_alist)
va_dcl
#endif
{
   int op;                      /* Operation to perform (eg. create new menu,
				   delete menu, etc. */
   struct ItemWindowStruct *IW; /* Menu configuration. */
   va_list ap;
   /***************************************************************************
   * Start variable-length argument list scanning.
   ***************************************************************************/
#if defined(STDARG)
   va_start (ap, opT);
   op = opT;
#else
   VA_START (ap);
   op = va_arg (ap, int);
#endif
   IW = va_arg (ap, struct ItemWindowStruct *);
   /***************************************************************************
   * Perform desired operation.  Some operations wait for user input (by
   * falling through the `switch' statement).
   ***************************************************************************/
   switch (op) {
     /*************************************************************************
     * Check for new/modified menu.
     *************************************************************************/
     case NEWiw:
     case UPDATEiw: {
       int nSections;           /* Number of sections on window. */
       int nHorizLines;         /* Number of horizontal lines needed.  At
				   most 2 horizontal lines will be needed
				   to separate the 3 sections. */
       int horizRows[2];        /* Rows at which to draw the horizontal
				   lines. */
       int horizLineN;          /* Horizontal line number. */
       int xRow;                /* Used when determining starting rows for
				   the various sections. */
       Logical highlightItem = FALSE;           /* If TRUE, hightlight the
						   current item. */
       /***********************************************************************
       * Get remaining arguments.
       ***********************************************************************/
       IW->itemN = va_arg (ap, int);            /* Initial item number (if any
						   items exist). */
       va_end (ap);
       /***********************************************************************
       * Calculate positions.
       ***********************************************************************/
       if (op == NEWiw) {
	 IW->nColS = IW->nColsTotal - 2;
	 nSections = 0;
	 nHorizLines = 0;
	 /********************************************************************\
	 | Start at row 1 (row 0 is top border line).
	 \********************************************************************/
	 xRow = 1;
	 if (IW->NhLines > 0) {
	   nSections++;
	   IW->hRowT = xRow;
	   IW->hRowB = IW->hRowT + IW->NhLines - 1;
	   xRow += IW->NhLines + 1;
	 }
	 if (IW->NiRows > 0) {
	   nSections++;
	   IW->iRowT = xRow;
	   IW->iRowB = IW->iRowT + IW->NiRows - 1;
	   xRow += IW->NiRows + 1;
	   if (nSections > 1) horizRows[nHorizLines++] = IW->iRowT - 1;
	 }
	 else
	   return FALSE;                /* An item section must exist. */
	 if (IW->NtLines > 0) {
	   nSections++;
	   IW->tRowT = xRow;
	   IW->tRowB = IW->tRowT + IW->NtLines - 1;
	   xRow += IW->NtLines + 1;
	   if (nSections > 1) horizRows[nHorizLines++] = IW->tRowT - 1;
	 }
	 IW->nRowsTotal = xRow;
       }
       /***********************************************************************
       * Turn off cursor.
       ***********************************************************************/
       set_cursor_mode (CURSORoff);
       /***********************************************************************
       * Create window?
       ***********************************************************************/
       begin_pasteboard_update ();
       if (op == NEWiw) {
	 create_virtual_display (IW->nRowsTotal, IW->nColsTotal, &(IW->wid),
				 BORDER, NORMAL);
	 paste_virtual_display (IW->wid, IW->ULrow, IW->ULcol);
       }
       /***********************************************************************
       * If specified, write label to window.  If this is an update operation,
       * first overwrite the existing label.
       ***********************************************************************/
       if (op == UPDATEiw) draw_horizontal_line (IW->wid, 0, 1, IW->nColS,
						 NORMAL, FALSE);
       if (IW->label != NULL) {
	 int len = (int) strlen(IW->label);
	 if (len <= IW->nColS) {
	   int nRemainChars = IW->nColS - len;
	   int nCharsBefore = nRemainChars / 2;
	   put_chars (IW->wid, IW->label, len, 0, nCharsBefore + 1, FALSE,
		      REVERSE1);
	 }
       }
       /***********************************************************************
       * If necessary, draw horizontal lines on window.
       ***********************************************************************/
       if (op == NEWiw) {
	 for (horizLineN = 0; horizLineN < nHorizLines; horizLineN++) {
	    draw_horizontal_line (IW->wid, horizRows[horizLineN], 0,
				  IW->nColsTotal-1, NORMAL, TRUE);
	 }
       }
       /***********************************************************************
       * If specified, write header section to window.  If an update operation,
       * it is assumed that the number of lines has not changed.
       ***********************************************************************/
       if (IW->NhLines > 0) DrawSection (IW->wid, IW->hRowT, IW->hRowB, 0,
					 IW->NhLines, IW->hLines, IW->nColS);
       /***********************************************************************
       * Write items section to window and `highlight' current item.  The
       * current item is set to zero initially for a new menu (if items exist,
       * otherwise it is set to -1).  The item section may contain lines but
       * no items.  If an update operation, note that the number of lines may
       * have changed.
       ***********************************************************************/
       if (op == UPDATEiw) {
	 if (IW->iUpTo != NULL) cdf_FreeMemory (IW->iUpTo,
					    FatalError);
	 if (IW->iDownTo != NULL) cdf_FreeMemory (IW->iDownTo,
					      FatalError);
	 if (IW->iLeftTo != NULL) cdf_FreeMemory (IW->iLeftTo,
					      FatalError);
	 if (IW->iRightTo != NULL) cdf_FreeMemory (IW->iRightTo,
					       FatalError);
       }
       IW->iUpTo = (int *) cdf_AllocateMemory ((size_t)IW->nItems * sizeof(int),
					   FatalError);
       IW->iDownTo = (int *) cdf_AllocateMemory ((size_t)IW->nItems * sizeof(int),
					     FatalError);
       IW->iLeftTo = (int *) cdf_AllocateMemory ((size_t)IW->nItems * sizeof(int),
					     FatalError);
       IW->iRightTo = (int *) cdf_AllocateMemory ((size_t)IW->nItems * sizeof(int),
					      FatalError);
       if (IW->NiLines > 0) {
	 /*******************************************************************
	 * One or more lines in the item section.
	 *******************************************************************/
	 if (IW->nItems > 0) {
	   /*****************************************************************
	   * One or more items in the item section.
	   *****************************************************************/
	   CalcItemDirections (IW->nItems, IW->NiLines, IW->iLineNs,
			       IW->iCols, IW->iLens, IW->iDownTo, IW->iUpTo,
			       IW->iLeftTo, IW->iRightTo);
	   if (IW->NiLines > IW->NiRows)
	     IW->iScroll = TRUE;
	   else
	     IW->iScroll = FALSE;
	   /*******************************************************************
	   * Determine row number for current item.  If this is an UPDATEiw
	   * operation, try to keep the row number the same as it was (or as
	   * close as possible).
	   *******************************************************************/
	   if (op == NEWiw)
	     IW->iRowN = MinInt (IW->iRowT +
				 IW->iLineNs[IW->itemN], IW->iRowB);
	   else
	     if (IW->iScroll) {
	       int nLinesFromTop = IW->iLineNs[IW->itemN];
	       int nRowsFromTop = IW->iRowN - IW->iRowT;
	       if (nLinesFromTop < nRowsFromTop)
		 IW->iRowN -= nRowsFromTop - nLinesFromTop;
	       else {
		 int nLinesFromBot = (IW->NiLines-1) - IW->iLineNs[IW->itemN];
		 int nRowsFromBot = IW->iRowB - IW->iRowN;
		 if (nLinesFromBot < nRowsFromBot)
		   IW->iRowN += nRowsFromBot - nLinesFromBot;
	       }
	     }
	     else
	       IW->iRowN = IW->iRowT + IW->iLineNs[IW->itemN];
	   DrawSection (IW->wid, IW->iRowT, IW->iRowB, IWtopRowLineN(IW),
			IW->NiLines, IW->iLines, IW->nColS);
	   highlightItem = TRUE;
	 }
	 else {
	   /*****************************************************************
	   * No items in the item section (but there are some lines).  Just
	   * redraw lines.
	   *****************************************************************/
	   IW->iRowN = IW->iRowT;
	   DrawSection (IW->wid, IW->iRowT, IW->iRowB, IWtopRowLineN(IW),
			IW->NiLines, IW->iLines, IW->nColS);
	 }
       }
       else {
	 /*******************************************************************
	 * No lines in the item section.  Erase lines in case there had
	 * previously been lines.
	 *******************************************************************/
	 IW->iRowN = IW->iRowT;
	 erase_display (IW->wid, IW->iRowT, 1, IW->iRowB, IW->nColS);
       }
       /*********************************************************************
       * Setup/display initial percentage indicator.
       *********************************************************************/
       if (IW->iPct) {
	 int lineNt = IWtopRowLineN (IW);
	 if (op == NEWiw) {
	   IW->iPctRowN = IW->iRowB + 1;
	   IW->iPctColN = IW->nColsTotal - 7;
	 }
	 UpdatePctString (IW->wid, IW->NiLines, IW->NiRows, lineNt,
			  IW->iPctRowN, IW->iPctColN);
       }
       /***********************************************************************
       * If specified, write trailer section to window.  If an update
       * operation, it is assumed that the number of lines has not changed.
       ***********************************************************************/
       if (IW->NtLines > 0) DrawSection (IW->wid, IW->tRowT, IW->tRowB, 0,
					 IW->NtLines, IW->tLines, IW->nColS);
       /***********************************************************************
       * Update screen and then highlight the current item (if there is one).
       * The current item is highlighted after the screen is updated for those
       * situations where the cursor could not be turned off.  Highlighting
       * the current item last will put the (possible blinking) cursor after
       * the current item which isn't as distracting as when the cursor is at
       * the end of the last line of the trailer section.
       ***********************************************************************/
       end_pasteboard_update ();
       if (highlightItem) change_rendition (IW->wid, IW->iRowN,
					    IW->iCols[IW->itemN] + 1, 1,
					    IW->iLens[IW->itemN], REVERSE2);
       return TRUE;
     }
     /*************************************************************************
     * Check for a beep request.
     *************************************************************************/
     case BEEPiw:
       ring_bell ();
       va_end (ap);
       return TRUE;
     /*************************************************************************
     * Check if menu should be deleted.
     *************************************************************************/
     case DELETEiw:
       if (IW->iUpTo != NULL) cdf_FreeMemory (IW->iUpTo, FatalError);
       if (IW->iDownTo != NULL) cdf_FreeMemory (IW->iDownTo, FatalError);
       if (IW->iLeftTo != NULL) cdf_FreeMemory (IW->iLeftTo, FatalError);
       if (IW->iRightTo != NULL) cdf_FreeMemory (IW->iRightTo, FatalError);
       delete_virtual_display (IW->wid);
       va_end (ap);
       return TRUE;
     /*************************************************************************
     * Check if menu should be erased (but remain in existence).
     *************************************************************************/
     case UNDISPLAYiw:
       unpaste_virtual_display (IW->wid);
       va_end (ap);
       return TRUE;
     /*************************************************************************
     * Check if menu should be redisplayed (but don't wait for input - a call
     * with `op = READiw' should be made next).
     *************************************************************************/
     case REDISPLAYiw:
       set_cursor_mode (CURSORoff);
       paste_virtual_display (IW->wid, IW->ULrow, IW->ULcol);
       va_end (ap);
       return TRUE;
     /*************************************************************************
     * Get next input.  The cursor is positioned and turned off first.  The
     * cursor is positioned for those cases where the cursor cannot be turned
     * off.  A (possibly) blinking cursor at the end of the highlighted item
     * is less distracting than where it might be.
     *************************************************************************/
     case READiw:
       for (;;) {
	  if (IW->nItems > 0) {
	    set_cursor_abs (IW->wid, IW->iRowN,
			    IW->iCols[IW->itemN] + IW->iLens[IW->itemN] + 1);
	  }
	  set_cursor_mode (CURSORoff);
	  read_input (
#if defined(CURSESui)
		      IW->wid,
#endif
			       &(IW->key), PASSTHRUri, TRUE);
	  /********************************************************************
	  * Check for an exit key.  If no exit keys were specified, any key
	  * causes an exit.
	  ********************************************************************/
	  if (IW->exitChars[0] == NUL) {
	    va_end (ap);
	    return TRUE;
	  }
	  else {
	    int charN;
	    for (charN = 0; IW->exitChars[charN] != NUL; charN++)
	       if (IW->key == IW->exitChars[charN]) {
		 va_end (ap);
		 return TRUE;
	       }
	  }
	  /********************************************************************
	  * Check for next-screen key.
	  ********************************************************************/
	  if (IW->key == IW->NSkey) {
	    begin_pasteboard_update ();
	    if (IW->nItems > 0) {
	      change_rendition (IW->wid, IW->iRowN, IW->iCols[IW->itemN] + 1,
				1, IW->iLens[IW->itemN], NORMAL);
	      NextScreen (IW->NiRows, IW->NiLines, IW->iLineNs, IW->iDownTo,
			  IW->iRowB, IW->iRowT, IW->iScroll, &(IW->itemN),
			  &(IW->iRowN));
	      DrawSection (IW->wid, IW->iRowT, IW->iRowB, IWtopRowLineN(IW),
			   IW->NiLines, IW->iLines, IW->nColS);
	      if (IW->iPct) UpdatePctString (IW->wid, IW->NiLines, IW->NiRows,
					     IWtopRowLineN(IW), IW->iPctRowN,
					     IW->iPctColN);
	      change_rendition (IW->wid, IW->iRowN, IW->iCols[IW->itemN] + 1,
				1, IW->iLens[IW->itemN], REVERSE2);
	    }
	    else
	      ring_bell ();
	    end_pasteboard_update ();
	    continue;
	  }
	  /********************************************************************
	  * Check for prev-screen key.
	  ********************************************************************/
	  if (IW->key == IW->PSkey) {
	    begin_pasteboard_update ();
	    if (IW->nItems > 0) {
	      change_rendition (IW->wid, IW->iRowN, IW->iCols[IW->itemN] + 1,
				1, IW->iLens[IW->itemN], NORMAL);
	      PrevScreen (IW->NiRows, IW->iLineNs, IW->iUpTo, IW->iRowT,
			  IW->iScroll, &(IW->iRowN), &(IW->itemN));
	      DrawSection (IW->wid, IW->iRowT, IW->iRowB, IWtopRowLineN(IW),
			   IW->NiLines, IW->iLines, IW->nColS);
	      if (IW->iPct) UpdatePctString (IW->wid, IW->NiLines, IW->NiRows,
					     IWtopRowLineN(IW), IW->iPctRowN,
					     IW->iPctColN);
	      change_rendition (IW->wid, IW->iRowN, IW->iCols[IW->itemN] + 1,
				1, IW->iLens[IW->itemN], REVERSE2);
	    }
	    else
	      ring_bell ();
	    end_pasteboard_update ();
	    continue;
	  }
	  /********************************************************************
	  * Check for down arrow key.
	  ********************************************************************/
	  if (IW->key == IW_DOWN || IW->key == IW_DOWNx) {
	    begin_pasteboard_update ();
	    if (IW->nItems > 0) {
	      change_rendition (IW->wid, IW->iRowN, IW->iCols[IW->itemN] + 1,
				1, IW->iLens[IW->itemN], NORMAL);
	      DownArrow (IW->iLineNs, IW->iDownTo, IW->iRowT, IW->iRowB,
			 IW->iScroll, &(IW->iRowN), &(IW->itemN));
	      DrawSection (IW->wid, IW->iRowT, IW->iRowB, IWtopRowLineN(IW),
			   IW->NiLines, IW->iLines, IW->nColS);
	      if (IW->iPct) UpdatePctString (IW->wid, IW->NiLines, IW->NiRows,
					     IWtopRowLineN(IW), IW->iPctRowN,
					     IW->iPctColN);
	      change_rendition (IW->wid, IW->iRowN, IW->iCols[IW->itemN] + 1,
				1, IW->iLens[IW->itemN], REVERSE2);
	    }
	    else
	      ring_bell ();
	    end_pasteboard_update ();
	    continue;
	  }
	  /********************************************************************
	  * Check for up arrow key.
	  ********************************************************************/
	  if (IW->key == IW_UP || IW->key == IW_UPx) {
	    begin_pasteboard_update ();
	    if (IW->nItems > 0) {
	      change_rendition (IW->wid, IW->iRowN, IW->iCols[IW->itemN] + 1,
				1, IW->iLens[IW->itemN], NORMAL);
	      UpArrow (IW->NiLines, IW->iLineNs, IW->iUpTo, IW->iRowT,
		       IW->iRowB, IW->iScroll, &(IW->iRowN), &(IW->itemN));
	      DrawSection (IW->wid, IW->iRowT, IW->iRowB, IWtopRowLineN(IW),
			   IW->NiLines, IW->iLines, IW->nColS);
	      if (IW->iPct) UpdatePctString (IW->wid, IW->NiLines, IW->NiRows,
					     IWtopRowLineN(IW), IW->iPctRowN,
					     IW->iPctColN);
	      change_rendition (IW->wid, IW->iRowN, IW->iCols[IW->itemN] + 1,
				1, IW->iLens[IW->itemN], REVERSE2);
	    }
	    else
	      ring_bell ();
	    end_pasteboard_update ();
	    continue;
	  }
	  /********************************************************************
	  * Check for left arrow key.
	  ********************************************************************/
	  if (IW->key == IW_LEFT || IW->key == IW_LEFTx) {
	    begin_pasteboard_update ();
	    if (IW->nItems > 0) {
	      change_rendition (IW->wid, IW->iRowN, IW->iCols[IW->itemN] + 1,
				1, IW->iLens[IW->itemN], NORMAL);
	      IW->itemN = IW->iLeftTo[IW->itemN];
	      change_rendition (IW->wid, IW->iRowN, IW->iCols[IW->itemN] + 1,
				1, IW->iLens[IW->itemN], REVERSE2);
	    }
	    else
	      ring_bell ();
	    end_pasteboard_update ();
	    continue;
	  }
	  /********************************************************************
	  * Check for right arrow key.
	  ********************************************************************/
	  if (IW->key == IW_RIGHT || IW->key == IW_RIGHTx) {
	    begin_pasteboard_update ();
	    if (IW->nItems > 0) {
	      change_rendition (IW->wid, IW->iRowN, IW->iCols[IW->itemN] + 1,
				1, IW->iLens[IW->itemN], NORMAL);
	      IW->itemN = IW->iRightTo[IW->itemN];
	      change_rendition (IW->wid, IW->iRowN, IW->iCols[IW->itemN] + 1,
				1, IW->iLens[IW->itemN], REVERSE2);
	    }
	    else
	      ring_bell ();
	    end_pasteboard_update ();
	    continue;
	  }
	  /********************************************************************
	  * Check for the `refresh' key.
	  ********************************************************************/
	  if (IW->key == IW->refreshChar) {
	    repaint_screen ();
	    continue;
	  }
	  /********************************************************************
	  * Illegal key, ring the bell.
	  ********************************************************************/
	  ring_bell ();
       }
   }
   va_end (ap);
   return FALSE;                                      /* Illegal operation. */
}

/******************************************************************************
* PromptWindow.
******************************************************************************/

#if defined(STDARG)
int PromptWindow (int opT, ...)
#else
int PromptWindow (va_alist)
va_dcl
#endif
{
   int op;                      /* Operation to perform (eg. create new
				   prompt window, delete window, etc.). */
   struct PromptWindowStruct *PW;
				/* Prompt window configuration. */
   va_list ap;
   /***************************************************************************
   * Start variable-length argument list scanning.
   ***************************************************************************/
#if defined(STDARG)
   va_start (ap, opT);
   op = opT;
#else
   VA_START (ap);
   op = va_arg (ap, int);
#endif
   PW = va_arg (ap, struct PromptWindowStruct *);
   /***************************************************************************
   * Perform desired operation.  Some operations wait for user input (by
   * falling through the `switch' statement).
   ***************************************************************************/
   switch (op) {
     /*************************************************************************
     * Check for new menu or menu to be reset.
     *************************************************************************/
     case NEWpw:
     case RESETpw: {
       /***********************************************************************
       * Get remaining arguments.
       ***********************************************************************/
       PW->curChar = va_arg (ap, int);
       PW->insertMode = va_arg (ap, Logical);
       va_end (ap);
       /***********************************************************************
       * Calculate positions.
       ***********************************************************************/
       if (op == NEWpw) {
	 PW->nColS = PW->nColsTotal - 2;
	 PW->nRowsTotal = (PW->NhLines > 0 ? 1 + PW->NhLines : 0) +
			  3 +
			  (PW->NtLines > 0 ? PW->NtLines + 1 : 0);
	 PW->NvCols = PW->nColsTotal - 6;
	 PW->vRow = (PW->NhLines > 0 ? PW->NhLines + 2 : 1);
	 PW->vLcol = 3;
	 PW->vRcol = PW->vLcol + PW->NvCols - 1;
	 PW->mLcol = 1;
	 PW->mRcol = PW->nColsTotal - 2;
	 if (PW->NhLines > 0) {
	   PW->hRowT = 1;
	   PW->hRowB = PW->hRowT + PW->NhLines - 1;
	 }
	 if (PW->NtLines > 0) {
	   PW->tRowT = (PW->NhLines > 0 ? 1 + PW->NhLines : 0) + 3;
	   PW->tRowB = PW->tRowT + PW->NtLines - 1;
	 }
       }
       /***********************************************************************
       * Create window?
       ***********************************************************************/
       begin_pasteboard_update ();
       if (op == NEWpw) {
	 create_virtual_display (PW->nRowsTotal, PW->nColsTotal, &(PW->wid),
				 BORDER, NORMAL);
	 paste_virtual_display (PW->wid, PW->ULrow, PW->ULcol);
       }
       /***********************************************************************
       * If specified, write label to window.
       ***********************************************************************/
       if (PW->label != NULL) {
	 int len = (int) strlen(PW->label);
	 if (len <= PW->nColS) {
	   int nRemainChars = PW->nColS - len;
	   int nCharsBefore = nRemainChars / 2;
	   put_chars (PW->wid, PW->label, len, 0, nCharsBefore + 1, FALSE,
		      REVERSE1);
	 }
       }
       /***********************************************************************
       * Draw necessary horizontal and vertical lines.
       ***********************************************************************/
       if (op == NEWpw) {
	 if (PW->NhLines > 0) draw_horizontal_line (PW->wid, PW->vRow-1, 0,
						    PW->nColsTotal-1, NORMAL,
						    TRUE);
	 if (PW->NtLines > 0) draw_horizontal_line (PW->wid, PW->vRow+1, 0,
						    PW->nColsTotal-1, NORMAL,
						    TRUE);
	 draw_vertical_line (PW->wid, PW->vRow - 1, PW->vRow + 1,
			     PW->vLcol - 1, NORMAL, TRUE);
	 draw_vertical_line (PW->wid, PW->vRow - 1, PW->vRow + 1,
			     PW->vRcol + 1, NORMAL, TRUE);
       }
       /***********************************************************************
       * If specified, draw header.
       ***********************************************************************/
       if (PW->NhLines > 0) DrawSection (PW->wid, PW->hRowT, PW->hRowB, 0,
					 PW->NhLines, PW->hLines, PW->nColS);
       /***********************************************************************
       * Draw initial value (if one exists) and place cursor.  If a RESETpw
       * operation, first erase existing value.
       ***********************************************************************/
       if (op == RESETpw) {
	 put_chars (PW->wid, " ", 1, PW->vRow, PW->mLcol, FALSE, NORMAL);
	 erase_display (PW->wid, PW->vRow, PW->vLcol, PW->vRow, PW->vRcol);
	 put_chars (PW->wid, " ", 1, PW->vRow, PW->mRcol, FALSE, NORMAL);
       }
       PW->curLen = (int) strlen (PW->value);
       if (PW->curChar < PW->NvCols) {
	 if (PW->curLen > PW->NvCols) {
	   put_chars (PW->wid, PW->value, PW->NvCols, PW->vRow, PW->vLcol,
		      FALSE, NORMAL);
	   put_chars (PW->wid, ">", 1, PW->vRow, PW->mRcol, FALSE, NORMAL);
	 }
	 else
	   put_chars (PW->wid, PW->value, PW->curLen, PW->vRow, PW->vLcol,
		      FALSE, NORMAL);
	 PW->curCol = PW->vLcol + PW->curChar;
       }
       else {
	 put_chars (PW->wid, "<", 1, PW->vRow, PW->mLcol, FALSE, NORMAL);
	 if (PW->curChar < PW->curLen) {
	   put_chars (PW->wid, &(PW->value[PW->curChar - PW->NvCols + 1]),
		      PW->NvCols, PW->vRow, PW->vLcol, FALSE, NORMAL);
	   if (PW->curChar != PW->curLen - 1) put_chars (PW->wid, ">", 1,
							 PW->vRow, PW->mRcol,
							 FALSE, NORMAL);
	 }
	 else
	   put_chars (PW->wid, &(PW->value[PW->curChar - PW->NvCols + 1]),
		      PW->NvCols - 1, PW->vRow, PW->vLcol, FALSE, NORMAL);
	 PW->curCol = PW->vRcol;
       }
       /***********************************************************************
       * If specified, draw trailer.
       ***********************************************************************/
       if (PW->NtLines > 0) DrawSection (PW->wid, PW->tRowT, PW->tRowB, 0,
					 PW->NtLines, PW->tLines, PW->nColS);
       /***********************************************************************
       * Update screen.
       ***********************************************************************/
       end_pasteboard_update ();
       /***********************************************************************
       * Position cursor (which must occur after the pasteboard batching is
       * ended).
       ***********************************************************************/
       set_cursor_abs (PW->wid, PW->vRow, PW->curCol);
       set_cursor_mode (CURSORon);
       return TRUE;
     }
     /*************************************************************************
     * Check for a beep request.
     *************************************************************************/
     case BEEPpw:
       ring_bell ();
       va_end (ap);
       return TRUE;
     /*************************************************************************
     * Check if window should be erased (but remain in existence).
     *************************************************************************/
     case UNDISPLAYpw:
       unpaste_virtual_display (PW->wid);
       va_end (ap);
       return TRUE;
     /*************************************************************************
     * Check if window should be redisplayed (don't wait for input - a call
     * with `op = READpw' should be made next).
     *************************************************************************/
     case REDISPLAYpw:
       paste_virtual_display (PW->wid, PW->ULrow, PW->ULcol);
       set_cursor_abs (PW->wid, PW->vRow, PW->curCol);
       set_cursor_mode (CURSORon);
       va_end (ap);
       return TRUE;
     /*************************************************************************
     * Check if window should be deleted.
     *************************************************************************/
     case DELETEpw:
       delete_virtual_display (PW->wid);
       va_end (ap);
       return TRUE;
     /*************************************************************************
     * Read keystrokes until `exit' key entered.
     *************************************************************************/
     case READpw: {
       int colsToEnd;           /* Number of columns from cursor to the right-
				   most column of the value field (including
				   the cursor position). */
       int offRight;            /* Number of characters to the right of the
				   last column in the value field (the right
				   `more' indicator should be ON).  This can
				   be zero or negative. */
       int offLeft;             /* Number of characters to the left of the
				   first column in the value field (the left
				   `more' indicator should be ON).  This can
				   be zero or negative. */
       int charsToEnd;          /* Number of characters from the cursor to the
				   end of the value (including at the cursor
				   position). */
       int leftColChar;         /* Character number at left column of value
				   field. */
       int rightColChar;        /* Character number at right column of value
				   field. */
       int charN;               /* Character number. */
       /***********************************************************************
       * Set cursor mode/position.
       ***********************************************************************/
       set_cursor_abs (PW->wid, PW->vRow, PW->curCol);
       set_cursor_mode (CURSORon);
       /***********************************************************************
       * Read/process keys until an `exit' key...
       ***********************************************************************/
       for (;;) {
	  read_input (
#if defined(CURSESui)
		      PW->wid,
#endif
			       &(PW->key), PASSTHRUri, TRUE);
	  /********************************************************************
	  * Check for an `exit' key.
	  ********************************************************************/
	  for (charN = 0; PW->exitChars[charN] != NUL; charN++) {
	     if (PW->key == PW->exitChars[charN]) {
	       va_end (ap);
	       return TRUE;
	     }
	  }
	  /********************************************************************
	  * Check for left arrow.
	  ********************************************************************/
	  if (PW->key == KB_LEFTARROW) {
	    begin_pasteboard_update ();
	    if (PW->curCol > PW->vLcol) {
	      PW->curChar--;
	      PW->curCol--;
	    }
	    else
	      if (PW->curChar > 0) {
		PW->curChar--;
		charsToEnd = PW->curLen - PW->curChar;
		put_chars (PW->wid, &(PW->value[PW->curChar]),
			   MINIMUM(charsToEnd,PW->NvCols), PW->vRow, PW->vLcol,
			   FALSE, NORMAL);
		if (PW->curChar == 0) put_chars (PW->wid, " ", 1, PW->vRow,
						 PW->mLcol, FALSE, NORMAL);
		offRight = charsToEnd - PW->NvCols;
		if (offRight == 1) put_chars (PW->wid, ">", 1, PW->vRow,
					      PW->mRcol, FALSE, NORMAL);
	      }
	      else
		ring_bell ();
	    end_pasteboard_update ();
	    set_cursor_abs (PW->wid, PW->vRow, PW->curCol);
	    continue;
	  }
	  /********************************************************************
	  * Check for right arrow.
	  ********************************************************************/
	  if (PW->key == KB_RIGHTARROW) {
	    begin_pasteboard_update ();
	    if (PW->curCol < PW->vRcol) {
	      if (PW->curChar < PW->curLen) {
		PW->curChar++;
		PW->curCol++;
	      }
	      else
		ring_bell ();
	    }
	    else
	      if (PW->curChar < PW->curLen) {
		PW->curChar++;
		leftColChar = PW->curChar - PW->NvCols + 1;
		if (PW->curChar == PW->curLen) {
		  put_chars (PW->wid, &(PW->value[leftColChar]),
			     PW->NvCols - 1, PW->vRow, PW->vLcol, FALSE,
			     NORMAL);
		  put_chars (PW->wid, " ", 1, PW->vRow, PW->vRcol, FALSE,
			     NORMAL);
		}
		else
		  put_chars (PW->wid, &(PW->value[leftColChar]), PW->NvCols,
			     PW->vRow, PW->vLcol, FALSE, NORMAL);
		put_chars (PW->wid, "<", 1, PW->vRow, PW->mLcol, FALSE,
			   NORMAL);
		charsToEnd = PW->curLen - PW->curChar;
		colsToEnd = PW->vRcol - PW->curCol + 1;
		offRight = charsToEnd - colsToEnd;
		if (offRight == 0) put_chars (PW->wid, " ", 1, PW->vRow,
					      PW->mRcol, FALSE, NORMAL);
	      }
	      else
		ring_bell ();
	    end_pasteboard_update ();
	    set_cursor_abs (PW->wid, PW->vRow, PW->curCol);
	    continue;
	  }
	  /********************************************************************
	  * Check for delete key.
	  ********************************************************************/
	  if (PW->key == KB_DELETE) {
	    begin_pasteboard_update ();
	    if (PW->curCol == PW->vLcol) {
	      if (PW->curChar > 0) {
		charsToEnd = PW->curLen - PW->curChar;
		memmove (&PW->value[PW->curChar-1], &PW->value[PW->curChar],
			 charsToEnd + 1);
		PW->curChar--;
		PW->curLen--;
		if (PW->curChar == 0) put_chars (PW->wid, " ", 1, PW->vRow,
						 PW->mLcol, FALSE, NORMAL);
	      }
	      else
		ring_bell ();
	    }
	    else {
	      charsToEnd = PW->curLen - PW->curChar;
	      colsToEnd = PW->vRcol - PW->curCol + 1;
	      offRight = charsToEnd - colsToEnd;
	      if (offRight > 0) {
		memmove (&(PW->value[PW->curChar-1]),
			 &(PW->value[PW->curChar]), charsToEnd + 1);
		PW->curChar--;
		PW->curCol--;
		PW->curLen--;
		colsToEnd++;
		put_chars (PW->wid, &(PW->value[PW->curChar]), colsToEnd,
			   PW->vRow, PW->curCol, FALSE, NORMAL);
		offRight--;
		if (offRight == 0) put_chars (PW->wid, " ", 1, PW->vRow,
					      PW->mRcol, FALSE, NORMAL);
	      }
	      else
		if (charsToEnd > 0) {
		  memmove (&(PW->value[PW->curChar-1]),
			   &(PW->value[PW->curChar]), charsToEnd + 1);
		  PW->curChar--;
		  PW->curCol--;
		  PW->curLen--;
		  put_chars (PW->wid, &(PW->value[PW->curChar]), charsToEnd,
			     PW->vRow, PW->curCol, FALSE, NORMAL);
		  put_chars (PW->wid, " ", 1, PW->vRow,
			     PW->curCol + charsToEnd, FALSE, NORMAL);
		}
		else {
		  PW->curChar--;
		  PW->value[PW->curChar] = NUL;
		  PW->curLen--;
		  PW->curCol--;
		  put_chars (PW->wid, " ", 1, PW->vRow, PW->curCol, FALSE,
			     NORMAL);
		}
	    }
	    end_pasteboard_update ();
	    set_cursor_abs (PW->wid, PW->vRow, PW->curCol);
	    continue;
	  }
	  /********************************************************************
	  * Check for the `refresh' key.
	  ********************************************************************/
	  if (PW->key == PW->refreshChar) {
	    repaint_screen ();
	    continue;
	  }
	  /********************************************************************
	  * Check for the SOL key.  If the current length of the value is
	  * greater than the number of available columns, the right `more'
	  * indicator is drawn (even though it may already be drawn).
	  ********************************************************************/
	  if (PW->key == PW->SOLchar) {
	    begin_pasteboard_update ();
	    leftColChar = PW->curChar - (PW->curCol - PW->vLcol);
	    if (leftColChar > 0) {
	      put_chars (PW->wid, " ", 1, PW->vRow, PW->mLcol, FALSE, NORMAL);
	      put_chars (PW->wid, PW->value, PW->NvCols, PW->vRow, PW->vLcol,
			 FALSE, NORMAL);
	      if (PW->curLen > PW->NvCols) put_chars (PW->wid, ">", 1,
						      PW->vRow, PW->mRcol,
						      FALSE, NORMAL);
	    }
	    PW->curCol = PW->vLcol;
	    PW->curChar = 0;
	    end_pasteboard_update ();
	    set_cursor_abs (PW->wid, PW->vRow, PW->curCol);
	    continue;
	  }
	  /********************************************************************
	  * Check for the EOL key.  The `right column character' is used to
	  * determine if moving the cursor to the end of the value will cause
	  * characters to be off the left end of the value field.  The left
	  * and right `more' indicators are updated regardless of how they may
	  * have already been.
	  ********************************************************************/
	  if (PW->key == PW->EOLchar) {
	    begin_pasteboard_update ();
	    rightColChar = PW->curChar + (PW->vRcol - PW->curCol);
	    if (rightColChar < PW->curLen) {
	      put_chars (PW->wid, "<", 1, PW->vRow, PW->mLcol, FALSE, NORMAL);
	      put_chars (PW->wid, &(PW->value[PW->curLen - PW->NvCols + 1]),
			 PW->NvCols - 1, PW->vRow, PW->vLcol, FALSE, NORMAL);
	      put_chars (PW->wid, " ", 1, PW->vRow, PW->vRcol, FALSE, NORMAL);
	      put_chars (PW->wid, " ", 1, PW->vRow, PW->mRcol, FALSE, NORMAL);
	    }
	    PW->curCol = MinInt (PW->vLcol + PW->curLen, PW->vRcol);
	    PW->curChar = PW->curLen;
	    end_pasteboard_update ();
	    set_cursor_abs (PW->wid, PW->vRow, PW->curCol);
	    continue;
	  }
	  /********************************************************************
	  * Check for the toggle insert/overstrike mode key.
	  ********************************************************************/
	  if (PW->key == PW->TOGGLEchar) {
	    PW->insertMode = (PW->insertMode ? FALSE : TRUE);
	    continue;
	  }
	  /********************************************************************
	  * Check for a printable character to insert/overstrike.
	  ********************************************************************/
	  if (Printable(PW->key)) {
	    begin_pasteboard_update ();
	    charsToEnd = PW->curLen - PW->curChar;
	    colsToEnd = PW->vRcol - PW->curCol + 1;
	    if (charsToEnd == 0) {
	      /****************************************************************
	      * The cursor is at the end of the value.
	      ****************************************************************/
	      if (PW->curLen < PW->maxChars) {
		catchrX (PW->value, PW->key, PW->maxChars);
		PW->curLen++;
		PW->curChar++;
		if (colsToEnd > 1) {
		  put_chars (PW->wid, &(PW->value[PW->curChar-1]), 1, PW->vRow,
			     PW->curCol, FALSE, NORMAL);
		  PW->curCol++;
		}
		else {
		  leftColChar = PW->curChar - PW->NvCols + 1;
		  put_chars (PW->wid, &(PW->value[leftColChar]), PW->NvCols-1,
			     PW->vRow, PW->vLcol, FALSE, NORMAL);
		  offLeft = PW->curLen - PW->NvCols + 1;
		  if (offLeft == 1) put_chars (PW->wid, "<", 1, PW->vRow,
					       PW->mLcol, FALSE, NORMAL);
		}
	      }
	      else
		ring_bell ();
	    }
	    else {
	      /****************************************************************
	      * The cursor is somewhere in the middle of the value.
	      ****************************************************************/
	      if (!PW->insertMode ||
		  (PW->insertMode && PW->curLen < PW->maxChars)) {
		if (PW->insertMode) {
		  /************************************************************
		  * Insert mode.
		  ************************************************************/
		  memmove (&(PW->value[PW->curChar+1]),
			   &(PW->value[PW->curChar]), charsToEnd + 1);
		  PW->value[PW->curChar] = PW->key;
		  PW->curLen++;
		  if (colsToEnd > 1) {
		    /**********************************************************
		    * The cursor is not in the last column of the value field.
		    **********************************************************/
		    put_chars (PW->wid, &(PW->value[PW->curChar]),
			       MINIMUM(colsToEnd,charsToEnd + 1),
			       PW->vRow, PW->curCol, FALSE, NORMAL);
		    PW->curChar++;
		    PW->curCol++;
		    charsToEnd = PW->curLen - PW->curChar;
		    colsToEnd = PW->vRcol - PW->curCol + 1;
		    offRight = charsToEnd - colsToEnd;
		    if (offRight == 1) put_chars (PW->wid, ">", 1, PW->vRow,
						  PW->mRcol, FALSE, NORMAL);
		  }
		  else {
		    /**********************************************************
		    * The cursor is in the last column of the value field.
		    **********************************************************/
		    PW->curChar++;
		    leftColChar = PW->curChar - PW->NvCols + 1;
		    put_chars (PW->wid, &(PW->value[leftColChar]), PW->NvCols,
			       PW->vRow, PW->vLcol, FALSE, NORMAL);
		    if (leftColChar == 1) put_chars (PW->wid, "<", 1, PW->vRow,
						     PW->mLcol, FALSE, NORMAL);
		  }
		}
		else {
		  /************************************************************
		  * Overstrike mode.
		  ************************************************************/
		  PW->value[PW->curChar] = PW->key;
		  if (colsToEnd > 1) {
		    /**********************************************************
		    * The cursor is not in the last column of the value field.
		    **********************************************************/
		    put_chars (PW->wid, &(PW->value[PW->curChar]), 1, PW->vRow,
			       PW->curCol, FALSE, NORMAL);
		    PW->curChar++;
		    PW->curCol++;
		  }
		  else {
		    /**********************************************************
		    * The cursor is in the last column of the value field.
		    **********************************************************/
		    offRight = charsToEnd - 1;
		    if (offRight > 0) {
		      /********************************************************
		      * One or more character off the right end.
		      ********************************************************/
		      PW->curChar++;
		      leftColChar = PW->curChar - PW->NvCols + 1;
		      put_chars (PW->wid, &(PW->value[leftColChar]),
				 PW->NvCols, PW->vRow, PW->vLcol, FALSE,
				 NORMAL);
		      if (leftColChar == 1) put_chars (PW->wid, "<", 1,
						       PW->vRow, PW->mLcol,
						       FALSE, NORMAL);
		      offRight--;
		      if (offRight == 0) put_chars (PW->wid, " ", 1, PW->vRow,
						    PW->mRcol, FALSE, NORMAL);
		    }
		    else {
		      /********************************************************
		      * Cursor is on the last character.
		      ********************************************************/
		      PW->curChar++;
		      leftColChar = PW->curChar - PW->NvCols + 1;
		      put_chars (PW->wid, &(PW->value[leftColChar]),
				 PW->NvCols - 1, PW->vRow, PW->vLcol, FALSE,
				 NORMAL);
		      put_chars (PW->wid, " ", 1, PW->vRow, PW->vRcol, FALSE,
				 NORMAL);
		      if (leftColChar == 1) put_chars (PW->wid, "<", 1,
						       PW->vRow, PW->mLcol,
						       FALSE, NORMAL);
		    }
		  }
		}
	      }
	      else
		ring_bell ();
	    }
	    end_pasteboard_update ();
	    set_cursor_abs (PW->wid, PW->vRow, PW->curCol);
	    continue;
	  }
	  /********************************************************************
	  * None of the above, illegal character.
	  ********************************************************************/
	  ring_bell ();
       }
     }
   }
   va_end (ap);
   return FALSE;                                      /* Illegal operation. */
}

/******************************************************************************
* EditWindow.
******************************************************************************/

#if defined(STDARG)
int EditWindow (int opT, ...)
#else
int EditWindow (va_alist)
va_dcl
#endif
{
   int op;                       /* Operation to perform (eg. create new
				    edit window, delete window, etc.). */
   struct EditWindowStruct *EW;  /* Edit window configuration. */
   va_list ap;
   /***************************************************************************
   * Start variable-length argument list scanning.
   ***************************************************************************/
#if defined(STDARG)
   va_start (ap, opT);
   op = opT;
#else
   VA_START (ap);
   op = va_arg (ap, int);
#endif
   EW = va_arg (ap, struct EditWindowStruct *);
   /***************************************************************************
   * Perform desired operation.  Some operations wait for user input (by
   * falling through the `switch' statement).
   ***************************************************************************/
   switch (op) {
     /*************************************************************************
     * Check for new menu.
     *************************************************************************/
     case NEWew:
     case UPDATEew: {
       /***********************************************************************
       * Get remaining arguments.
       ***********************************************************************/
       EW->insertMode = va_arg (ap, Logical);
       va_end (ap);
       /***********************************************************************
       * Create window?
       ***********************************************************************/
       begin_pasteboard_update ();
       if (op == NEWew) {
	 EW->nRowsTotal = BOO(EW->NhLines > 0,1,0) + EW->NhLines +
			  BOO(EW->NeRows > 0,1,0) + EW->NeRows +
			  BOO(EW->NtLines > 0,1,0) + EW->NtLines + 1;
	 create_virtual_display (EW->nRowsTotal, EW->nColsTotal, &(EW->wid),
				 BORDER, NORMAL);
	 paste_virtual_display (EW->wid, EW->ULrow, EW->ULcol);
	 EW->nColS = EW->nColsTotal - 2;
       }
       /***********************************************************************
       * If specified, write label to window.  If this is an update operation,
       * first overwrite the existing label.
       ***********************************************************************/
       if (op == UPDATEew) draw_horizontal_line (EW->wid, 0, 1, EW->nColS,
						 NORMAL, FALSE);
       if (EW->label != NULL) {
	 int len = (int) strlen(EW->label);
	 if (len <= EW->nColS) {
	   int nRemainChars = EW->nColS - len;
	   int nCharsBefore = nRemainChars / 2;
	   put_chars (EW->wid, EW->label, len, 0, nCharsBefore + 1, FALSE,
		      REVERSE1);
	 }
       }
       /***********************************************************************
       * If necessary, calculate positions and draw horizontal lines on window.
       ***********************************************************************/
       if (op == NEWew) {
	 int nSections = 0;    /* Number of sections in the window. */
	 int xRow = 1;         /* Start at row 1 (row 0 is top border line). */
	 if (EW->NhLines > 0) {
	   nSections++;
	   EW->hRowT = xRow;
	   EW->hRowB = EW->hRowT + EW->NhLines - 1;
	   xRow += EW->NhLines + 1;
	 }
	 if (EW->NeRows > 0) {
	   nSections++;
	   EW->eRowT = xRow;
	   EW->eRowB = EW->eRowT + EW->NeRows - 1;
	   xRow += EW->NeRows + 1;
	   if (nSections > 1) draw_horizontal_line (EW->wid, EW->eRowT - 1, 0,
						    EW->nColsTotal - 1, NORMAL,
						    TRUE);
	 }
	 if (EW->NtLines > 0) {
	   nSections++;
	   EW->tRowT = xRow;
	   EW->tRowB = EW->tRowT + EW->NtLines - 1;
	   xRow += EW->NtLines + 1;
	   if (nSections > 1) draw_horizontal_line (EW->wid, EW->tRowT - 1, 0,
						    EW->nColsTotal - 1, NORMAL,
						    TRUE);
	 }
       }
       /***********************************************************************
       * If specified, write header section to window.  If an update operation,
       * it is assumed that the number of lines has not changed.
       ***********************************************************************/
       if (EW->NhLines > 0) DrawSection (EW->wid, EW->hRowT, EW->hRowB, 0,
					 EW->NhLines, EW->hLines, EW->nColS);
       /***********************************************************************
       * Write edit section to window.  If an update operation, note that the
       * text may have changed (to more or less lines) but not the number of
       * rows.
       ***********************************************************************/
       EW->nChars = (int) strlen(EW->eText);
       EW->xChars = EW->nChars;
       EW->cursorRow = EW->eRowT;
       EW->cursorCol = 1;
       EW->firstChar = 0;
       EW->cursorChar = 0;
       DrawEditSection (EW);
       /*********************************************************************
       * Setup/display initial percentage indicator.
       *********************************************************************/
       if (EW->ePct) {
	 if (op == NEWew) {
	   EW->ePctRowN = EW->eRowB + 1;
	   EW->ePctColN = EW->nColsTotal - 7;
	 }
	 UpdatePctString (EW->wid, EW->nChars + 1, 1, EW->cursorChar,
			  EW->ePctRowN, EW->ePctColN);
       }
       /***********************************************************************
       * If specified, write trailer section to window.  If an update
       * operation, it is assumed that the number of lines has not changed.
       ***********************************************************************/
       if (EW->NtLines > 0) DrawSection (EW->wid, EW->tRowT, EW->tRowB, 0,
					 EW->NtLines, EW->tLines, EW->nColS);
       /***********************************************************************
       * Update screen.
       ***********************************************************************/
       end_pasteboard_update ();
       /***********************************************************************
       * Position cursor (which must occur after the pasteboard batching is
       * ended).
       ***********************************************************************/
       set_cursor_abs (EW->wid, EW->cursorRow, EW->cursorCol);
       set_cursor_mode (CURSORon);
       return TRUE;
     }
     /*************************************************************************
     * Check for a beep request.
     *************************************************************************/
     case BEEPew:
       ring_bell ();
       va_end (ap);
       return TRUE;
     /*************************************************************************
     * Check if menu should be deleted.
     *************************************************************************/
     case DELETEew:
       delete_virtual_display (EW->wid);
       va_end (ap);
       return TRUE;
     /*************************************************************************
     * Get next input (exit key).
     *************************************************************************/
     case READew: {
       int length, i, count;
       set_cursor_abs (EW->wid, EW->cursorRow, EW->cursorCol);
       set_cursor_mode (CURSORon);
       for (;;) {
	  read_input (
#if defined(CURSESui)
		      EW->wid,
#endif
			       &(EW->key), PASSTHRUri, TRUE);
	  /********************************************************************
	  * Check for an exit key.  If no exit keys were specified, any key
	  * causes an exit.
	  ********************************************************************/
	  if (EW->exitChars[0] == NUL) {
	    va_end (ap);
	    return TRUE;
	  }
	  else {
	    int charN;
	    for (charN = 0; EW->exitChars[charN] != NUL; charN++) {
	       if (EW->key == EW->exitChars[charN]) {
		 va_end (ap);
		 return TRUE;
	       }
	    }
	  }
	  /********************************************************************
	  * Check for start-of-line key.
	  ********************************************************************/
	  if (EW->key == EW->SOLkey) {
	    ring_bell ();
	    continue;
	  }
	  /********************************************************************
	  * Check for end-of-line key.
	  ********************************************************************/
	  if (EW->key == EW->EOLkey) {
	    ring_bell ();
	    continue;
	  }
	  /********************************************************************
	  * Check for start-of-text key.
	  ********************************************************************/
	  if (EW->key == EW->SOTkey) {
	    ring_bell ();
	    continue;
	  }
	  /********************************************************************
	  * Check for end-of-text key.
	  ********************************************************************/
	  if (EW->key == EW->EOTkey) {
	    ring_bell ();
	    continue;
	  }
	  /********************************************************************
	  * Check for next-word key.
	  ********************************************************************/
	  if (EW->key == EW->NWkey) {
	    ring_bell ();
	    continue;
	  }
	  /********************************************************************
	  * Check for delete-line key.
	  ********************************************************************/
	  if (EW->key == EW->DLkey) {
	    if (EW->readOnly)
	      ring_bell ();
	    else {
	      ring_bell ();
	    }
	    continue;
	  }
	  /********************************************************************
	  * Check for next-screen key.
	  ********************************************************************/
	  if (EW->key == EW->NSkey) {
	    if (EW->eText[EW->cursorChar] == NUL)
	      ring_bell ();
	    else {
	      int count;       /* Number of lines that cursor is moved down. */
	      /****************************************************************
	      * Move the cursor to the beginning of the line.
	      ****************************************************************/
	      while (EW->cursorChar > 0) {
		if (EW->eText[EW->cursorChar-1] == Nl) break;
		EW->cursorChar--;
	      }
	      /****************************************************************
	      * Move the cursor down the number of rows displayed (or less).
	      ****************************************************************/
	      for (count = 0;
		   EW->eText[EW->cursorChar] != NUL;
		   EW->cursorChar++) {
		 if (EW->eText[EW->cursorChar] == Nl) {
		   count++;
		   if (count == EW->NeRows) {
		     EW->cursorChar++;
		     break;
		   }
		 }
	      }
	      /****************************************************************
	      * Move the `first character displayed' down the same number of
	      * rows (if necessary).
	      ****************************************************************/
	      if (EW->cursorRow + count > EW->eRowB) {
		int count1;    /* Number of lines after the first displayed. */
		int charN;
		for (count1 = 0, charN = EW->firstChar;
		     EW->eText[charN] != NUL; charN++) {
		   if (EW->eText[charN] == Nl) count1++;
		}
		count1 -= (EW->NeRows - 1);
		count1 = MINIMUM(count1,count);
		while (count1 > 0) {
		  if (EW->eText[EW->firstChar] == Nl) count1--;
		  EW->firstChar++;
		}
		DrawEditSection (EW);
	      }
	      /****************************************************************
	      * Update the percentage and cursor position.
	      ****************************************************************/
	      if (EW->ePct) UpdatePctString (EW->wid, EW->nChars + 1, 1,
					     EW->cursorChar, EW->ePctRowN,
					     EW->ePctColN);
	      SetEditCursor (EW);
	    }
	    continue;
	  }
	  /********************************************************************
	  * Check for prev-screen key.
	  ********************************************************************/
	  if (EW->key == EW->PSkey) {
	    if (EW->cursorChar == 0)
	      ring_bell ();
	    else {
	      int count;       /* Number of lines that cursor is moved up. */
	      /****************************************************************
	      * Move the cursor to the beginning of the line.
	      ****************************************************************/
	      while (EW->cursorChar > 0) {
		if (EW->eText[EW->cursorChar-1] == Nl) break;
		EW->cursorChar--;
	      }
	      /****************************************************************
	      * Move the cursor up the number of rows displayed (or less).
	      ****************************************************************/
	      for (count = 0; EW->cursorChar > 0; EW->cursorChar--) {
		 if (EW->eText[EW->cursorChar] == Nl) {
		   count++;
		   if (count == EW->NeRows) {
		     if (EW->cursorChar > 0) {
		       EW->cursorChar--;
		       while (EW->cursorChar > 0) {
			 if (EW->eText[EW->cursorChar-1] == Nl) break;
			 EW->cursorChar--;
		       }
		     }
		     break;
		   }
		 }
	      }
	      /****************************************************************
	      * Move the `first character displayed' up the same number of
	      * rows (if necessary).
	      ****************************************************************/
	      if (EW->cursorRow - count < EW->eRowT) {
		while (EW->firstChar > 0) {
		  if (EW->eText[EW->firstChar] == Nl) {
		    count--;
		    if (count == 0) {
		      if (EW->firstChar > 0) {
			EW->firstChar--;
			while (EW->firstChar > 0) {
			  if (EW->eText[EW->firstChar-1] == Nl) break;
			  EW->firstChar--;
			}
		      }
		      break;
		    }
		  }
		  EW->firstChar--;
		}
		DrawEditSection (EW);
	      }
	      /****************************************************************
	      * Update the percentage and cursor position.
	      ****************************************************************/
	      if (EW->ePct) UpdatePctString (EW->wid, EW->nChars + 1, 1,
					     EW->cursorChar, EW->ePctRowN,
					     EW->ePctColN);
	      SetEditCursor (EW);
	    }
	    continue;
	  }
	  /********************************************************************
	  * Check for down arrow key.
	  ********************************************************************/
	  if (EW->key == KB_DOWNARROW) {
	    char *nextNL = strchr (&(EW->eText[EW->cursorChar]), Nl);
	    if (nextNL == NULL)
	      ring_bell ();
	    else {
	      int toNL = (int) (nextNL - &(EW->eText[EW->cursorChar]));
	      EW->cursorChar += (toNL + 1);
	      for (i = 1; i < EW->cursorCol; i++) {
		 if (EW->eText[EW->cursorChar] == Nl) break;
		 if (EW->eText[EW->cursorChar] == NUL) break;
		 EW->cursorChar++;
	      }
	      if (EW->cursorRow == EW->eRowB) {
		while (EW->eText[EW->firstChar] != Nl) EW->firstChar++;
		EW->firstChar++;
		DrawEditSection (EW);
	      }
	      if (EW->ePct) UpdatePctString (EW->wid, EW->nChars + 1, 1,
					     EW->cursorChar, EW->ePctRowN,
					     EW->ePctColN);
	      SetEditCursor (EW);
	    }
	    continue;
	  }
	  /********************************************************************
	  * Check for up arrow key.
	  ********************************************************************/
	  if (EW->key == KB_UPARROW) {
	    if (EW->firstChar == 0 && EW->cursorRow == EW->eRowT)
	      ring_bell ();
	    else {
	      /****************************************************************
	      * If the cursor is on the top line then scroll up one line.
	      ****************************************************************/
	      if (EW->cursorRow == EW->eRowT) {
		for (EW->firstChar--; EW->firstChar > 0; EW->firstChar--) {
		   if (EW->eText[EW->firstChar-1] == Nl) break;
		}
		DrawEditSection (EW);
	      }
	      /****************************************************************
	      * Set the cursor character as follows...
	      * 1. Move backward to the newline character of the preceeding
	      *    line while counting the number of characters moved.
	      * 2. Move backward to the first character of the preceeding
	      *    line.
	      * 3. Move forward to position the cursor character at the same
	      *    column or at the newline character (which ever occurs
	      *    first).
	      ****************************************************************/
	      for (count = 1, EW->cursorChar--;
		   EW->eText[EW->cursorChar] != Nl;
		   EW->cursorChar--) count++;
	      while (EW->cursorChar > 0) {
		if (EW->eText[EW->cursorChar-1] == Nl) break;
		EW->cursorChar--;
	      }
	      for (i = 1; i < count; i++) {
		 if (EW->eText[EW->cursorChar] == Nl) break;
		 EW->cursorChar++;
	      }
	      /****************************************************************
	      * Update the percentage and cursor position.
	      ****************************************************************/
	      if (EW->ePct) UpdatePctString (EW->wid, EW->nChars + 1, 1,
					     EW->cursorChar, EW->ePctRowN,
					     EW->ePctColN);
	      SetEditCursor (EW);
	    }
	    continue;
	  }
	  /********************************************************************
	  * Check for left arrow key.
	  ********************************************************************/
	  if (EW->key == KB_LEFTARROW) {
	    if (EW->cursorChar == 0)
	      ring_bell ();
	    else {
	      if (EW->cursorChar == EW->firstChar) {
		for (EW->firstChar--; EW->firstChar > 0; EW->firstChar--) {
		   if (EW->eText[EW->firstChar-1] == Nl) break;
		}
		DrawEditSection (EW);
	      }
	      EW->cursorChar--;
	      if (EW->ePct) UpdatePctString (EW->wid, EW->nChars + 1, 1,
					     EW->cursorChar, EW->ePctRowN,
					     EW->ePctColN);
	      SetEditCursor (EW);
	    }
	    continue;
	  }
	  /********************************************************************
	  * Check for right arrow key.
	  ********************************************************************/
	  if (EW->key == KB_RIGHTARROW) {
	    if (EW->eText[EW->cursorChar] == NUL)
	      ring_bell ();
	    else {
	      if (EW->cursorRow == EW->eRowB &&
		  EW->eText[EW->cursorChar] == Nl) {
		while (EW->eText[EW->firstChar] != Nl) EW->firstChar++;
		EW->firstChar++;
		DrawEditSection (EW);
	      }
	      EW->cursorChar++;
	      if (EW->ePct) UpdatePctString (EW->wid, EW->nChars + 1, 1,
					     EW->cursorChar, EW->ePctRowN,
					     EW->ePctColN);
	      SetEditCursor (EW);
	    }
	    continue;
	  }
	  /********************************************************************
	  * Check for the RETURN key.
	  ********************************************************************/
	  if (EW->key == KB_RETURN) {
	    if (EW->readOnly)
	      ring_bell ();
	    else {
	      if (EW->nChars == EW->xChars) {
		EW->xChars += EW->nColS;
		EW->eText = cdf_ReallocateMemory (EW->eText,
					      (size_t) (EW->xChars + 1),
					      FatalError);
	      }
	      length = (int) strlen (&(EW->eText[EW->cursorChar]));
	      memmove (&(EW->eText[EW->cursorChar+1]),
		       &(EW->eText[EW->cursorChar]), (size_t) (length + 1));
	      EW->eText[EW->cursorChar] = Nl;
	      EW->nChars++;
	      EW->cursorChar++;
	      if (EW->cursorRow == EW->eRowB) {
		while (EW->eText[EW->firstChar] != Nl) EW->firstChar++;
		EW->firstChar++;
	      }
	      DrawEditSection (EW);
	      if (EW->ePct) UpdatePctString (EW->wid, EW->nChars + 1, 1,
					     EW->cursorChar, EW->ePctRowN,
					     EW->ePctColN);
	      SetEditCursor (EW);
	    }
	    continue;
	  }
	  /********************************************************************
	  * Check for the DELETE key.
	  ********************************************************************/
	  if (EW->key == KB_DELETE) {
	    if (EW->readOnly)
	      ring_bell ();
	    else {
	      if (EW->cursorChar == 0)
		ring_bell ();
	      else {
		length = (int) strlen (&(EW->eText[EW->cursorChar]));
		memmove (&(EW->eText[EW->cursorChar-1]),
			 &(EW->eText[EW->cursorChar]), (size_t) (length + 1));
		EW->nChars--;
		if (EW->cursorChar == EW->firstChar) {
		  for (EW->firstChar--; EW->firstChar > 0; EW->firstChar--) {
		     if (EW->eText[EW->firstChar-1] == Nl) break;
		  }
		}
		EW->cursorChar--;
		DrawEditSection (EW);
		if (EW->ePct) UpdatePctString (EW->wid, EW->nChars + 1, 1,
					       EW->cursorChar, EW->ePctRowN,
					       EW->ePctColN);
		SetEditCursor (EW);
	      }
	    }
	    continue;
	  }
	  /********************************************************************
	  * Check for the toggle insert/overstrike mode key.
	  ********************************************************************/
	  if (EW->key == EW->TOGGLEkey) {
	    if (EW->readOnly)
	      ring_bell ();
	    else
	      EW->insertMode = BOO(EW->insertMode,FALSE,TRUE);
	    continue;
	  }
	  /********************************************************************
	  * Check for the `refresh' key.
	  ********************************************************************/
	  if (EW->key == EW->REFRESHkey) {
	    repaint_screen ();
	    continue;
	  }
	  /********************************************************************
	  * Check for a printable character.
	  ********************************************************************/
	  if (Printable(EW->key)) {
	    if (EW->readOnly)
	      ring_bell ();
	    else {
	      if (EW->insertMode ||
		  EW->eText[EW->cursorChar] == Nl ||    /* If over- */
		  EW->eText[EW->cursorChar] == NUL) {   /* strike mode. */
		if (EW->nChars == EW->xChars) {
		  EW->xChars += EW->nColS;
		  EW->eText = cdf_ReallocateMemory (EW->eText,
					       (size_t) (EW->xChars + 1),
					       FatalError);
		}
		length = (int) strlen (&(EW->eText[EW->cursorChar]));
		memmove (&(EW->eText[EW->cursorChar+1]),
			 &(EW->eText[EW->cursorChar]), (size_t) (length + 1));
		EW->eText[EW->cursorChar] = (char) EW->key;
		EW->nChars++;
	      }
	      else
		EW->eText[EW->cursorChar] = (char) EW->key;
	      DrawEditSection (EW);
	      if (EW->cursorCol < EW->nColS) EW->cursorChar++;
	      if (EW->ePct) UpdatePctString (EW->wid, EW->nChars + 1, 1,
					     EW->cursorChar, EW->ePctRowN,
					     EW->ePctColN);
	      SetEditCursor (EW);
	    }
	    continue;
	  }
	  /********************************************************************
	  * Illegal key, ring the bell.
	  ********************************************************************/
	  ring_bell ();
       }
     }
   }
   va_end (ap);
   return FALSE;                                      /* Illegal operation. */
}

/******************************************************************************
* DrawEditSection.
******************************************************************************/

static void DrawEditSection (EW)
struct EditWindowStruct *EW;
{
  char *charPtr = &(EW->eText[EW->firstChar]);
  int rowN = EW->eRowT, length;
  for (;;) {
     char *nextNL = strchr (charPtr, Nl);
     if (nextNL == NULL) {
       length = (int) strlen (charPtr);
       if (length > 0) {
	 put_chars (EW->wid, charPtr, MINIMUM(length,EW->nColS), rowN, 1,
		    FALSE, NORMAL);
	 if (length < EW->nColS) erase_display (EW->wid, rowN, length + 1,
						rowN, EW->nColS);
	 rowN++;
       }
       break;
     }
     length = (int) (nextNL - charPtr);
     put_chars (EW->wid, charPtr, MINIMUM(length,EW->nColS), rowN, 1, FALSE,
		NORMAL);
     if (length < EW->nColS) erase_display (EW->wid, rowN, length + 1,
					    rowN, EW->nColS);
     rowN++;
     if (rowN > EW->eRowB) break;
     charPtr = nextNL + 1;
  }
  if (rowN <= EW->eRowB) erase_display (EW->wid, rowN, 1,
					EW->eRowB, EW->nColS);
  return;
}

/******************************************************************************
* SetEditCursor.
******************************************************************************/

static void SetEditCursor (EW)
struct EditWindowStruct *EW;
{
  int charN;
  EW->cursorRow = EW->eRowT;
  EW->cursorCol = 0;
  for (charN = EW->firstChar; charN <= EW->cursorChar; charN++) {
     EW->cursorCol++;
     if (EW->eText[charN] == Nl && charN < EW->cursorChar) {
       EW->cursorCol = 0;
       EW->cursorRow++;
     }
  }
  set_cursor_abs (EW->wid, EW->cursorRow, EW->cursorCol);
  return;
}

/******************************************************************************
* FieldWindow.
*
*       Header section (optional).
*       Fields section (scrollable) containing one or more fields.
*       Trailer section (optional).
*
******************************************************************************/

#if defined(STDARG)
int FieldWindow (int opT, ...)
#else
int FieldWindow (va_alist)
va_dcl
#endif
{
   int op;              /* Operation to perform (eg. create new menu). */
   struct FieldWindowStruct *FW;
			/* Menu configuration. */
   va_list ap;
   /***************************************************************************
   * Start variable-length argument list scanning.
   ***************************************************************************/
#if defined(STDARG)
   va_start (ap, opT);
   op = opT;
#else
   VA_START (ap);
   op = va_arg (ap, int);
#endif
   FW = va_arg (ap, struct FieldWindowStruct *);
   /***************************************************************************
   * Perform desired operation.  Some operations wait for user input (by
   * falling through the `switch' statement).
   ***************************************************************************/
   switch (op) {
     /*************************************************************************
     * Check for new/modified menu.
     *************************************************************************/
     case NEWfw:
     case UPDATEfw: {
       int nSections;           /* Number of sections on window. */
       int nHorizLines;         /* Number of horizontal lines needed.  At
				   most 2 horizontal lines will be needed
				   to separate the 3 sections. */
       int horizRows[2];        /* Rows at which to draw the horizontal
				   lines. */
       int horizLineN;          /* Horizontal line number. */
       int xRow;                /* Used when determining starting rows for
				   the various sections. */
       /***********************************************************************
       * Get remaining arguments.
       ***********************************************************************/
       FW->fieldN = va_arg (ap, int);
       FW->insert = va_arg (ap, Logical);
       va_end (ap);
       /***********************************************************************
       * Validate menu.
       ***********************************************************************/
       if (FW->NfRows < 1) return FALSE;
       /***********************************************************************
       * Turn on cursor.
       ***********************************************************************/
       set_cursor_mode (CURSORon);
       /***********************************************************************
       * Calculate positions.
       ***********************************************************************/
       if (op == NEWfw) {
	 FW->nColS = FW->nColsTotal - 2;
	 nSections = 0;
	 nHorizLines = 0;
	 /*********************************************************************
	 * Start at row 1 (row 0 is top border line).
	 *********************************************************************/
	 xRow = 1;
	 if (FW->NhLines > 0) {
	   nSections++;
	   FW->hRowT = xRow;
	   FW->hRowB = FW->hRowT + FW->NhLines - 1;
	   xRow += FW->NhLines + 1;
	 }
	 nSections++;
	 FW->fRowT = xRow;
	 FW->fRowB = FW->fRowT + FW->NfRows - 1;
	 xRow += FW->NfRows + 1;
	 if (nSections > 1) horizRows[nHorizLines++] = FW->fRowT - 1;
	 if (FW->NtLines > 0) {
	   nSections++;
	   FW->tRowT = xRow;
	   FW->tRowB = FW->tRowT + FW->NtLines - 1;
	   xRow += FW->NtLines + 1;
	   if (nSections > 1) horizRows[nHorizLines++] = FW->tRowT - 1;
	 }
	 FW->nRowsTotal = xRow;
       }
       /***********************************************************************
       * Create window?
       ***********************************************************************/
       begin_pasteboard_update ();
       if (op == NEWfw) {
	 create_virtual_display (FW->nRowsTotal, FW->nColsTotal, &(FW->wid),
				 BORDER, NORMAL);
	 paste_virtual_display (FW->wid, FW->ULrow, FW->ULcol);
       }
       /***********************************************************************
       * If specified, write label to window.  If this is an update operation,
       * first overwrite the existing label.
       ***********************************************************************/
       if (op == UPDATEfw) draw_horizontal_line (FW->wid, 0, 1, FW->nColS,
						 NORMAL, FALSE);
       if (FW->label != NULL) {
	 int len = (int) strlen(FW->label);
	 if (len <= FW->nColS) {
	   int nRemainChars = FW->nColS - len;
	   int nCharsBefore = nRemainChars / 2;
	   put_chars (FW->wid, FW->label, len, 0, nCharsBefore + 1, FALSE,
		      REVERSE1);
	 }
       }
       /***********************************************************************
       * If necessary, draw horizontal lines on window.
       ***********************************************************************/
       if (op == NEWfw) {
	 for (horizLineN = 0; horizLineN < nHorizLines; horizLineN++) {
	    draw_horizontal_line (FW->wid, horizRows[horizLineN], 0,
				  FW->nColsTotal-1, NORMAL, TRUE);
	 }
       }
       /***********************************************************************
       * If specified, write header section to window.  If an update operation,
       * it is assumed that the number of lines has not changed.
       ***********************************************************************/
       if (FW->NhLines > 0) DrawSection (FW->wid, FW->hRowT, FW->hRowB, 0,
					 FW->NhLines, FW->hLines, FW->nColS);
       /***********************************************************************
       * Write fields section to window.  If an update operation, note that
       * the number of lines may have changed.
       ***********************************************************************/
       if (op == UPDATEfw) {
	 if (FW->fUpTo != NULL) cdf_FreeMemory (FW->fUpTo, FatalError);
	 if (FW->fDownTo != NULL) cdf_FreeMemory (FW->fDownTo, FatalError);
	 if (FW->fLeftTo != NULL) cdf_FreeMemory (FW->fLeftTo, FatalError);
	 if (FW->fRightTo != NULL) cdf_FreeMemory (FW->fRightTo, FatalError);
       }
       if (FW->nFields > 0) {
	 size_t nBytes = FW->nFields * sizeof(int);
	 FW->fUpTo = (int *) cdf_AllocateMemory (nBytes, FatalError);
	 FW->fDownTo = (int *) cdf_AllocateMemory (nBytes, FatalError);
	 FW->fLeftTo = (int *) cdf_AllocateMemory (nBytes, FatalError);
	 FW->fRightTo = (int *) cdf_AllocateMemory (nBytes, FatalError);
	 CalcItemDirections (FW->nFields, FW->NfLines, FW->fLineNs, FW->fCols,
			     FW->fLens, FW->fDownTo, FW->fUpTo, FW->fLeftTo,
			     FW->fRightTo);
       }
       else {
	 FW->fUpTo = NULL;
	 FW->fDownTo = NULL;
	 FW->fLeftTo = NULL;
	 FW->fRightTo = NULL;
       }
       if (FW->NfLines > FW->NfRows)
	 FW->fScroll = TRUE;
       else
	 FW->fScroll = FALSE;
       /***********************************************************************
       * Determine row number for current field.  If this is an UPDATEfw
       * operation, try to keep the row number the same as it was (or as
       * close as possible).
       ***********************************************************************/
       if (FW->nFields > 0) {
	 if (op == NEWfw)
	   FW->fRowN = MinInt (FW->fRowT + FW->fLineNs[FW->fieldN], FW->fRowB);
	 else {
	   if (FW->fScroll) {
	     int nLinesFromTop = FW->fLineNs[FW->fieldN];
	     int nRowsFromTop = FW->fRowN - FW->fRowT;
	     if (nLinesFromTop < nRowsFromTop)
	       FW->fRowN -= nRowsFromTop - nLinesFromTop;
	     else {
	       int nLinesFromBot = (FW->NfLines-1) - FW->fLineNs[FW->fieldN];
	       int nRowsFromBot = FW->fRowB - FW->fRowN;
	       if (nLinesFromBot < nRowsFromBot)
		 FW->fRowN += nRowsFromBot - nLinesFromBot;
	     }
	   }
	   else
	     FW->fRowN = FW->fRowT + FW->fLineNs[FW->fieldN];
	 }
       }
       else
	 FW->fRowN = FW->fRowT;
       /*********************************************************************
       * Draw fields section.
       *********************************************************************/
       DrawFieldsSection (FW);
       /*********************************************************************
       * Setup/display initial percentage indicator.
       *********************************************************************/
       if (FW->fPct) {
	 int lineNt = FWtopRowLineN (FW);
	 if (op == NEWfw) {
	   FW->fPctRowN = FW->fRowB + 1;
	   FW->fPctColN = FW->nColsTotal - 7;
	 }
	 UpdatePctString (FW->wid, FW->NfLines, FW->NfRows, lineNt,
			  FW->fPctRowN, FW->fPctColN);
       }
       /***********************************************************************
       * If specified, write trailer section to window.  If an update
       * operation, it is assumed that the number of lines has not changed.
       ***********************************************************************/
       if (FW->NtLines > 0) DrawSection (FW->wid, FW->tRowT, FW->tRowB, 0,
					 FW->NtLines, FW->tLines, FW->nColS);
       /***********************************************************************
       * Update screen and place the cursor in the current field.
       ***********************************************************************/
       end_pasteboard_update ();
       FW->charN = 0;
       FW->leftCharN = 0;
       if (FW->nFields > 0) {
	 set_cursor_abs (FW->wid, FW->fRowN, FW->fCols[FW->fieldN] + 1);
       }
       return TRUE;
     }
     /*************************************************************************
     * Check for a beep request.
     *************************************************************************/
     case BEEPfw:
       ring_bell ();
       va_end (ap);
       return TRUE;
     /*************************************************************************
     * Check if menu should be deleted.
     *************************************************************************/
     case DELETEfw:
       if (FW->fUpTo != NULL) cdf_FreeMemory (FW->fUpTo, FatalError);
       if (FW->fDownTo != NULL) cdf_FreeMemory (FW->fDownTo, FatalError);
       if (FW->fLeftTo != NULL) cdf_FreeMemory (FW->fLeftTo, FatalError);
       if (FW->fRightTo != NULL) cdf_FreeMemory (FW->fRightTo, FatalError);
       delete_virtual_display (FW->wid);
       va_end (ap);
       return TRUE;
     /*************************************************************************
     * Check if menu should be erased (but remain in existence).
     *************************************************************************/
     case UNDISPLAYfw:
       unpaste_virtual_display (FW->wid);
       va_end (ap);
       return TRUE;
     /*************************************************************************
     * Check if menu should be redisplayed (but don't wait for input - a call
     * with `op = READfw' should be made next).
     *************************************************************************/
     case REDISPLAYfw:
       set_cursor_mode (CURSORon);
       paste_virtual_display (FW->wid, FW->ULrow, FW->ULcol);
       va_end (ap);
       return TRUE;
     /*************************************************************************
     * Get next input.  The cursor is positioned first in case it had been
     * moved.
     *************************************************************************/
     case READfw: {
       int colN;
       if (FW->nFields < 1) return FALSE;
       set_cursor_mode (CURSORon);
       colN = FW->fCols[FW->fieldN] + (FW->charN - FW->leftCharN) + 1;
       set_cursor_abs (FW->wid, FW->fRowN, colN);
       for (;;) {
	  read_input (
#if defined(CURSESui)
		      FW->wid,
#endif
			       &(FW->key), PASSTHRUri, TRUE);
	  /********************************************************************
	  * Check for an exit key.  If no exit keys were specified, any key
	  * causes an exit.
	  ********************************************************************/
	  if (FW->exitChars[0] == NUL) {
	    va_end (ap);
	    return TRUE;
	  }
	  else {
	    int charN;
	    for (charN = 0; FW->exitChars[charN] != NUL; charN++)
	       if (FW->key == FW->exitChars[charN]) {
		 va_end (ap);
		 return TRUE;
	       }
	  }
	  /********************************************************************
	  * Check for next-screen key.
	  ********************************************************************/
	  if (FW->key == FW->NSkey) {
	    begin_pasteboard_update ();
	    NextScreen (FW->NfRows, FW->NfLines, FW->fLineNs, FW->fDownTo,
			FW->fRowB, FW->fRowT, FW->fScroll, &(FW->fieldN),
			&(FW->fRowN));
	    DrawFieldsSection (FW);
	    if (FW->fPct) UpdatePctString (FW->wid, FW->NfLines, FW->NfRows,
					   FWtopRowLineN(FW), FW->fPctRowN,
					   FW->fPctColN);
	    end_pasteboard_update ();
	    FW->charN = 0;
	    FW->leftCharN = 0;
	    set_cursor_abs (FW->wid, FW->fRowN, FW->fCols[FW->fieldN] + 1);
	    continue;
	  }
	  /********************************************************************
	  * Check for prev-screen key.
	  ********************************************************************/
	  if (FW->key == FW->PSkey) {
	    begin_pasteboard_update ();
	    PrevScreen (FW->NfRows, FW->fLineNs, FW->fUpTo, FW->fRowT,
			FW->fScroll, &(FW->fRowN), &(FW->fieldN));
	    DrawFieldsSection (FW);
	    if (FW->fPct) UpdatePctString (FW->wid, FW->NfLines, FW->NfRows,
					   FWtopRowLineN(FW), FW->fPctRowN,
					   FW->fPctColN);
	    end_pasteboard_update ();
	    FW->charN = 0;
	    FW->leftCharN = 0;
	    set_cursor_abs (FW->wid, FW->fRowN, FW->fCols[FW->fieldN] + 1);
	    continue;
	  }
	  /********************************************************************
	  * Check for down field key.
	  ********************************************************************/
	  if (FW->key == FW_DOWN_FIELD || FW->key == FW_DOWN_FIELDx) {
	    begin_pasteboard_update ();
	    DownArrow (FW->fLineNs, FW->fDownTo, FW->fRowT, FW->fRowB,
		       FW->fScroll, &(FW->fRowN), &(FW->fieldN));
	    DrawFieldsSection (FW);
	    if (FW->fPct) UpdatePctString (FW->wid, FW->NfLines, FW->NfRows,
					   FWtopRowLineN(FW), FW->fPctRowN,
					   FW->fPctColN);
	    end_pasteboard_update ();
	    FW->charN = 0;
	    FW->leftCharN = 0;
	    set_cursor_abs (FW->wid, FW->fRowN, FW->fCols[FW->fieldN] + 1);
	    continue;
	  }
	  /********************************************************************
	  * Check for up field key.
	  ********************************************************************/
	  if (FW->key == FW_UP_FIELD || FW->key == FW_UP_FIELDx) {
	    begin_pasteboard_update ();
	    UpArrow (FW->NfLines, FW->fLineNs, FW->fUpTo, FW->fRowT,
		     FW->fRowB, FW->fScroll, &(FW->fRowN), &(FW->fieldN));
	    DrawFieldsSection (FW);
	    if (FW->fPct) UpdatePctString (FW->wid, FW->NfLines, FW->NfRows,
					   FWtopRowLineN(FW), FW->fPctRowN,
					   FW->fPctColN);
	    end_pasteboard_update ();
	    FW->charN = 0;
	    FW->leftCharN = 0;
	    set_cursor_abs (FW->wid, FW->fRowN, FW->fCols[FW->fieldN] + 1);
	    continue;
	  }
	  /********************************************************************
	  * Check for left field key.
	  ********************************************************************/
	  if (FW->key == FW_LEFT_FIELD || FW->key == FW_LEFT_FIELDx) {
	    FW->fieldN = FW->fLeftTo[FW->fieldN];
	    FW->charN = 0;
	    FW->leftCharN = 0;
	    set_cursor_abs (FW->wid, FW->fRowN, FW->fCols[FW->fieldN] + 1);
	    continue;
	  }
	  /********************************************************************
	  * Check for right field key.
	  ********************************************************************/
	  if (FW->key == FW_RIGHT_FIELD || FW->key == FW_RIGHT_FIELDx) {
	    FW->fieldN = FW->fRightTo[FW->fieldN];
	    FW->charN = 0;
	    FW->leftCharN = 0;
	    set_cursor_abs (FW->wid, FW->fRowN, FW->fCols[FW->fieldN] + 1);
	    continue;
	  }
	  /********************************************************************
	  * Check for left character key.
	  ********************************************************************/
	  if (FW->key == FW_LEFT_CHAR || FW->key == FW_LEFT_CHARx) {
	    if (FW->charN > 0) {
	      int colN, len; char *ptr;
	      if (FW->charN == FW->leftCharN) {
		FW->leftCharN--;
		ptr = &(FW->fields[FW->fieldN][FW->leftCharN]);
		len = (int) strlen(ptr);
		put_chars (FW->wid, ptr, MINIMUM(len,FW->fLens[FW->fieldN]),
			   FW->fRowN, FW->fCols[FW->fieldN] + 1, FALSE, NORMAL);
	      }
	      FW->charN--;
	      colN = FW->fCols[FW->fieldN] + (FW->charN - FW->leftCharN);
	      set_cursor_abs (FW->wid, FW->fRowN, colN + 1);
	    }
	    else
	      ring_bell ();
	    continue;
	  }
	  /********************************************************************
	  * Check for right character key.
	  ********************************************************************/
	  if (FW->key == FW_RIGHT_CHAR || FW->key == FW_RIGHT_CHARx) {
	    int fieldLen = (int) strlen(FW->fields[FW->fieldN]);
	    if (FW->charN < fieldLen) {
	      int colN, cursorPos = FW->charN - FW->leftCharN + 1;
	      if (cursorPos == FW->fLens[FW->fieldN]) {
		char *ptr = &(FW->fields[FW->fieldN][FW->leftCharN+1]);
		if (FW->charN == fieldLen - 1) {
		  int count = FW->fLens[FW->fieldN] - 1;
		  put_chars (FW->wid, ptr, count, FW->fRowN,
			     FW->fCols[FW->fieldN] + 1, FALSE, NORMAL);
		  colN = FW->fCols[FW->fieldN] + count;
		  put_chars (FW->wid, " ", 1, FW->fRowN, colN + 1,
			     FALSE, NORMAL);
		}
		else {
		  put_chars (FW->wid, ptr, FW->fLens[FW->fieldN], FW->fRowN,
			     FW->fCols[FW->fieldN] + 1, FALSE, NORMAL);
		}
		FW->leftCharN++;
	      }
	      FW->charN++;
	      colN = FW->fCols[FW->fieldN] + (FW->charN - FW->leftCharN);
	      set_cursor_abs (FW->wid, FW->fRowN, colN + 1);
	    }
	    else
	      ring_bell ();
	    continue;
	  }
	  /********************************************************************
	  * Check for delete key.
	  ********************************************************************/
	  if (FW->key == KB_DELETE) {
	    if (FW->charN > 0) {
	      if (FW->charN == FW->leftCharN) {
		char *ptr = &(FW->fields[FW->fieldN][FW->charN]);
		memmove (ptr - 1, ptr, strlen(ptr) + 1);
		FW->charN--;
		FW->leftCharN--;
	      }
	      else {
		char *ptr;
		ptr = &(FW->fields[FW->fieldN][FW->charN]);
		memmove (ptr - 1, ptr, strlen(ptr) + 1);
		FW->charN--;
		DrawField (FW);
	      }
	    }
	    else
	      ring_bell ();
	    continue;
	  }
	  /********************************************************************
	  * Check for the `refresh' key.
	  ********************************************************************/
	  if (FW->key == FW->refreshChar) {
	    repaint_screen ();
	    continue;
	  }
	  /********************************************************************
	  * Check for the toggle insert/overstrike key.
	  ********************************************************************/
	  if (FW->key == FW->toggleKey) {
	    FW->insert = BOO(FW->insert,FALSE,TRUE);
	    continue;
	  }
	  /********************************************************************
	  * Character to insert/overstrike.
	  ********************************************************************/
	  if (Printable(FW->key)) {
	    if (FW->insert) {
	      int len = (int) strlen(FW->fields[FW->fieldN]);
	      if (len < FW->fMaxs[FW->fieldN]) {
		char *ptr = &(FW->fields[FW->fieldN][FW->charN]);
		int len = (int) strlen(ptr);
		memmove (ptr + 1, ptr, len + 1);
		*ptr = (char) FW->key;
		if (FW->charN - FW->leftCharN == FW->fLens[FW->fieldN] - 1) {
		  FW->leftCharN++;
		}
		FW->charN++;
		DrawField (FW);
	      }
	      else
		ring_bell ();
	    }
	    else {
	      if (FW->charN < FW->fMaxs[FW->fieldN]) {
		int len = (int) strlen(FW->fields[FW->fieldN]);
		char *ptr = &(FW->fields[FW->fieldN][FW->charN]);
		*ptr = (char) FW->key;
		if (FW->charN == len) *(ptr+1) = NUL;
		if (FW->charN - FW->leftCharN == FW->fLens[FW->fieldN] - 1) {
		  FW->leftCharN++;
		}
		FW->charN++;
		DrawField (FW);
	      }
	      else
		ring_bell ();
	    }
	    continue;
	  }
	  /********************************************************************
	  * Illegal key, ring the bell.
	  ********************************************************************/
	  ring_bell ();
       }
     }
   }
   va_end (ap);
   return FALSE;                                      /* Illegal operation. */
}

/******************************************************************************
* DrawField.
******************************************************************************/

static void DrawField (FW)
struct FieldWindowStruct *FW;
{
  char *ptr = &(FW->fields[FW->fieldN][FW->leftCharN]);
  int i, colN, len = (int) strlen(ptr);
  put_chars (FW->wid, ptr, MINIMUM(len,FW->fLens[FW->fieldN]),
	     FW->fRowN, FW->fCols[FW->fieldN] + 1, FALSE, NORMAL);
  for (i = len; i < FW->fLens[FW->fieldN]; i++) {
     put_chars (FW->wid, " ", 1, FW->fRowN,
		FW->fCols[FW->fieldN] + i + 1, FALSE, NORMAL);
  }
  colN = FW->fCols[FW->fieldN] + (FW->charN - FW->leftCharN);
  set_cursor_abs (FW->wid, FW->fRowN, colN + 1);
  return;
}

/******************************************************************************
* DrawFieldsSection.
******************************************************************************/

static void DrawFieldsSection (FW)
struct FieldWindowStruct *FW; /* Pointer to FieldWindow structure. */
{
  size_t len; int i, rowN, lineN, fieldN; char line[SCREEN_WIDTH+1];
  for (rowN = FW->fRowT, lineN = FWtopRowLineN(FW);
       rowN <= FW->fRowB && lineN < FW->NfLines; rowN++, lineN++) {
     strcpyX (line, FW->fLines[lineN], SCREEN_WIDTH);
     for (fieldN = 0; fieldN < FW->nFields; fieldN++) {
	if (FW->fLineNs[fieldN] == lineN) {
	  len = (int) strlen(FW->fields[fieldN]);
	  for (i = 0; i < FW->fLens[fieldN]; i++) {
	     if (i < (int) len)
	       line[FW->fCols[fieldN]+i] = FW->fields[fieldN][i];
	     else
	       line[FW->fCols[fieldN]+i] = ' ';
	  }
	}
     }
     len = (int) strlen(line);
     put_chars (FW->wid, line, MINIMUM(FW->nColS,(int)len), rowN, 1,
		FALSE, NORMAL);
     if ((int)len < FW->nColS)
	erase_display (FW->wid, rowN, (int)len + 1, rowN, FW->nColS);
  }
  if (rowN <= FW->fRowB) erase_display (FW->wid, rowN, 1, FW->fRowB, FW->nColS);
  return;
}

/******************************************************************************
* CalcItemDirections.
******************************************************************************/

static void CalcItemDirections (nItems, NiLines, iLineNs, iCols, iLens,
				iDownTo, iUpTo, iLeftTo, iRightTo)
int nItems;
int NiLines;
int *iLineNs;
int *iCols;
int *iLens;
int *iDownTo;
int *iUpTo;
int *iLeftTo;
int *iRightTo;
{
   int *center, i;
   /***************************************************************************
   * Allocate and calculate center point for each item.
   ***************************************************************************/
   center = (int *) cdf_AllocateMemory ((size_t)nItems * sizeof(int),
				    FatalError);
   for (i = 0; i < nItems; i++) {
      center[i] = iCols[i] + (iLens[i] / 2);
   }
   /***************************************************************************
   * Calculate directions for each item.
   ***************************************************************************/
   for (i = 0; i < nItems; i++) {
      /************************************************************************
      * Calculate down direction.
      ************************************************************************/
      iDownTo[i] = i;
      if (NiLines > 1 && nItems > 1) {
	int toLineN, mostOff, j;
	int fromLineN = iLineNs[i];
	for (j = (i+1) % nItems, toLineN = -1; j != i; j = (j+1) % nItems) {
	   int lineNt = iLineNs[j];
	   if (lineNt != fromLineN) {
	     if (toLineN == -1) {
	       iDownTo[i] = j;
	       toLineN = lineNt;
	       mostOff = DIFF(center[i],center[j]);
	     } else {
	       if (lineNt == toLineN) {
		 int offBy = DIFF(center[i],center[j]);
		 if (offBy < mostOff) {
		   iDownTo[i] = j;
		   mostOff = offBy;
		 }
	       }
	       else
		 break;         /* No more items on `toLineN'. */
	     }
	   }
	}
      }
      /************************************************************************
      * Calculate up direction.
      ************************************************************************/
      iUpTo[i] = i;
      if (NiLines > 1 && nItems > 1) {
	int toLineN, mostOff, j;
	int fromLineN = iLineNs[i];
	for (j = (i == 0 ? nItems-1 : i-1), toLineN = -1;
	     j != i; j = (j == 0 ? nItems-1 : j-1)) {
	   int lineNt = iLineNs[j];
	   if (lineNt != fromLineN) {
	     if (toLineN == -1) {
	       iUpTo[i] = j;
	       toLineN = lineNt;
	       mostOff = DIFF(center[i],center[j]);
	     } else {
	       if (lineNt == toLineN) {
		 int offBy = DIFF(center[i],center[j]);
		 if (offBy < mostOff) {
		   iUpTo[i] = j;
		   mostOff = offBy;
		 }
	       }
	       else
		 break;         /* No more items on `toLineN'. */
	     }
	   }
	}
      }
      /************************************************************************
      * Calculate left direction.  First check for the nearest item to the
      * left.  If none found, check for the farthest item to the right (if
      * none found, going left stays at the same item).
      ************************************************************************/
      iLeftTo[i] = i;
      if (nItems > 1) {
	int toLeftLineN = (i > 0 ? iLineNs[i-1] : -1), j;
	if (toLeftLineN == iLineNs[i])
	  iLeftTo[i] = i - 1;
	else
	  for (j = i+1; j < nItems; j++)
	     if (iLineNs[j] == iLineNs[i])
	       iLeftTo[i] = j;
	     else
	       break;
      }
      /************************************************************************
      * Calculate right direction.  First check for the nearest item to the
      * right.  If none found, check for the farthest item to the left (if
      * none found, going right stays at the same item).
      ************************************************************************/
      iRightTo[i] = i;
      if (nItems > 1) {
	int toRightLineN = (i < nItems-1 ? iLineNs[i+1] : -1), j;
	if (toRightLineN == iLineNs[i])
	  iRightTo[i] = i + 1;
	else
	  for (j = i-1; j >= 0; j--)
	     if (iLineNs[j] == iLineNs[i])
	       iRightTo[i] = j;
	     else
	       break;
      }
   }
   cdf_FreeMemory (center, FatalError);
   return;
}

/******************************************************************************
* UpdatePctString.
******************************************************************************/

static void UpdatePctString (wid, nLines, nRows, topRowLineN, rowN, colN)
WINDOWid wid;
int nLines;
int nRows;
int topRowLineN;
int rowN;
int colN;
{
  char pct[MAX_PCT_LEN+1];
  if (nLines <= nRows)
    strcpyX (pct, " All ", MAX_PCT_LEN);
  else
    if (topRowLineN == 0)
      strcpyX (pct, " Top ", MAX_PCT_LEN);
    else
      if (topRowLineN == nLines - nRows)
	strcpyX (pct, " End ", MAX_PCT_LEN);
      else {
	snprintf (pct, (size_t) sizeof(pct), "%3d%% ", (int)
		  ((100.0 * (((float) topRowLineN) / (nLines - nRows))) + 0.5));
      }
  put_chars (wid, pct, (int) strlen(pct), rowN, colN, FALSE, NORMAL);
  return;
}

/******************************************************************************
* DrawSection.
******************************************************************************/

static void DrawSection (wid, rowT, rowB, topRowLineN, nLines, lineS, nColS)
WINDOWid wid;
int rowT;               /* Top row number. */
int rowB;               /* Bottom row number. */
int topRowLineN;        /* Line number to be displayed in top row. */
int nLines;             /* Number of lines (may be greater than the number of
			   available rows). */
char **lineS;           /* Lines to be displayed.  Capital `S' because of the
			   IBM RS6000. */
int nColS;              /* Number of available columns (first character of a
			   line is displayed in column one [1]). */
{
  size_t len;
  int rowN, lineN;
  for (rowN = rowT, lineN = topRowLineN;
       rowN <= rowB && lineN < nLines; rowN++, lineN++) {
     len = strlen(lineS[lineN]);
     put_chars (wid, lineS[lineN], MINIMUM(nColS,(int)len), rowN, 1,
		FALSE, NORMAL);
     if ((int)len < nColS)
	erase_display (wid, rowN, (int)len + 1, rowN, nColS);
  }
  if (rowN <= rowB) erase_display (wid, rowN, 1, rowB, nColS);
  return;
}

/******************************************************************************
* NextScreen.
******************************************************************************/

static void NextScreen (NiRows, NiLines, iLineNs, iDownTo, iRowB, iRowT,
			iScroll, itemN, iRowN)
int NiRows;
int NiLines;
int *iLineNs;
int *iDownTo;
int iRowB;
int iRowT;
Logical iScroll;
int *itemN;
int *iRowN;
{
  int oldLineN = iLineNs[*itemN];
  int maxLineN = MinInt (oldLineN + NiRows, NiLines - 1);
  for (;;) {
     int itemNt = iDownTo[*itemN];
     int lineNt = iLineNs[itemNt];
     if (lineNt <= maxLineN && lineNt > oldLineN)
       *itemN = itemNt;
     else
       break;
  }
  if (iScroll) {
    int linesToEnd = (NiLines - 1) - iLineNs[*itemN];
    if (*iRowN + linesToEnd < iRowB) *iRowN = iRowB - linesToEnd;
  }
  else
    *iRowN = iRowT + iLineNs[*itemN];
  return;
}

/******************************************************************************
* PrevScreen.
******************************************************************************/

static void PrevScreen (NiRows, iLineNs, iUpTo, iRowT, iScroll, iRowN, itemN)
int NiRows;
int *iLineNs;
int *iUpTo;
int iRowT;
Logical iScroll;
int *iRowN;
int *itemN;
{
  int oldLineN = iLineNs[*itemN];
  int minLineN = MaxInt (oldLineN - NiRows, 0);
  for (;;) {
     int itemNt = iUpTo[*itemN];
     int lineNt = iLineNs[itemNt];
     if (lineNt >= minLineN && lineNt < oldLineN)
       *itemN = itemNt;
     else
       break;
  }
  if (iScroll) {
    int linesToBeg = iLineNs[*itemN];
    if (*iRowN - linesToBeg > iRowT) *iRowN = iRowT + linesToBeg;
  }
  else
    *iRowN = iRowT + iLineNs[*itemN];
  return;
}

/******************************************************************************
* DownArrow.
******************************************************************************/

static void DownArrow (iLineNs, iDownTo, iRowT, iRowB, iScroll, iRowN, itemN)
int *iLineNs;
int *iDownTo;
int iRowT;
int iRowB;
Logical iScroll;
int *iRowN;
int *itemN;
{
  int oldLineN = iLineNs[*itemN];
  *itemN = iDownTo[*itemN];
  if (iScroll) {
    int nLinesDown = iLineNs[*itemN] - oldLineN;
    if (nLinesDown < 0)
      *iRowN = iRowT + iLineNs[*itemN];
    else
      *iRowN = MINIMUM (*iRowN + nLinesDown, iRowB);
  }
  else
    *iRowN = iRowT + iLineNs[*itemN];
  return;
}

/******************************************************************************
* UpArrow.
******************************************************************************/

static void UpArrow (NiLines, iLineNs, iUpTo, iRowT, iRowB, iScroll, iRowN,
		     itemN)
int NiLines;
int *iLineNs;
int *iUpTo;
int iRowT;
int iRowB;
Logical iScroll;
int *iRowN;
int *itemN;
{
  int oldLineN = iLineNs[*itemN];
  *itemN = iUpTo[*itemN];
  if (iScroll) {
    int nLinesUp = oldLineN - iLineNs[*itemN];
    if (nLinesUp < 0) {
      int nLinesFromBot = (NiLines - 1) - iLineNs[*itemN];
      *iRowN = iRowB - nLinesFromBot;
    }
    else
      *iRowN = MaxInt (*iRowN - nLinesUp, iRowT);
  }
  else
    *iRowN = iRowT + iLineNs[*itemN];
  return;
}

/******************************************************************************
* IWtopRowLineN.
******************************************************************************/

static int IWtopRowLineN (IW)
struct ItemWindowStruct *IW;
{
  return BOO(IW->nItems > 0,
	     IW->iLineNs[IW->itemN] - (IW->iRowN - IW->iRowT), 0);
}

/******************************************************************************
* FWtopRowLineN.
******************************************************************************/

static int FWtopRowLineN (FW)
struct FieldWindowStruct *FW;
{
  return BOO(FW->nFields > 0,
	     FW->fLineNs[FW->fieldN] - (FW->fRowN - FW->fRowT), 0);
}

/******************************************************************************
* AllocIW.
******************************************************************************/

void AllocIW (IW, nItems, NiLines, iLineNchars, fatalFnc)
struct ItemWindowStruct *IW;
int nItems;
int NiLines;
int iLineNchars;
void (*fatalFnc) PROTOARGs((char *msg));
{
   IW->nItems = nItems;
   if (IW->nItems > 0) {
     IW->iLineNs = (int *) cdf_AllocateMemory ((size_t)IW->nItems * sizeof(int),
					   fatalFnc);
     IW->iCols = (int *) cdf_AllocateMemory ((size_t)IW->nItems * sizeof(int),
					 fatalFnc);
     IW->iLens = (int *) cdf_AllocateMemory ((size_t)IW->nItems * sizeof(int),
					 fatalFnc);
   }
   IW->NiLines = NiLines;
   if (IW->NiLines > 0) {
     int lineN, i;
     IW->iLines = (char **) cdf_AllocateMemory ((size_t)IW->NiLines * sizeof(char *),
					    fatalFnc);
     for (lineN = 0; lineN < IW->NiLines; lineN++) {
	IW->iLines[lineN] = (char *) cdf_AllocateMemory ((size_t)iLineNchars+1,
						     fatalFnc);
	for (i = 0; i < iLineNchars; i++) IW->iLines[lineN][i] = ' ';
	IW->iLines[lineN][iLineNchars] = NUL;
     }
   }
   return;
}

/******************************************************************************
* AllocFW.
******************************************************************************/

void AllocFW (FW, nFields, NfLines, fLineNchars, fatalFnc)
struct FieldWindowStruct *FW;
int nFields;
int NfLines;
int fLineNchars;
void (*fatalFnc) PROTOARGs((char *msg));
{
   FW->nFields = nFields;
   if (FW->nFields > 0) {
     FW->fLineNs = (int *) cdf_AllocateMemory ((size_t)FW->nFields * sizeof(int),
					   fatalFnc);
     FW->fCols = (int *) cdf_AllocateMemory ((size_t)FW->nFields * sizeof(int),
					 fatalFnc);
     FW->fLens = (int *) cdf_AllocateMemory ((size_t)FW->nFields * sizeof(int),
					 fatalFnc);
   }
   FW->NfLines = NfLines;
   if (FW->NfLines > 0) {
     int lineN, i;
     FW->fLines = (char **) cdf_AllocateMemory ((size_t)FW->NfLines * sizeof(char *),
					    fatalFnc);
     for (lineN = 0; lineN < FW->NfLines; lineN++) {
	FW->fLines[lineN] = (char *) cdf_AllocateMemory ((size_t)fLineNchars+1,
						     fatalFnc);
	for (i = 0; i < fLineNchars; i++) FW->fLines[lineN][i] = ' ';
	FW->fLines[lineN][fLineNchars] = NUL;
     }
   }
   return;
}

/******************************************************************************
* FreeIW.
******************************************************************************/

void FreeIW (IW, fatalFnc)
struct ItemWindowStruct *IW;
void (*fatalFnc) PROTOARGs((char *msg));
{
   if (IW->NiLines > 0) {
     int lineN;
     for (lineN = IW->NiLines - 1; lineN >= 0; lineN--) {
	cdf_FreeMemory (IW->iLines[lineN], fatalFnc);
     }
     cdf_FreeMemory (IW->iLines, fatalFnc);
   }
   if (IW->nItems > 0) {
     cdf_FreeMemory (IW->iLens, fatalFnc);
     cdf_FreeMemory (IW->iCols, fatalFnc);
     cdf_FreeMemory (IW->iLineNs, fatalFnc);
   }
   IW->NiLines = 0;
   IW->nItems = 0;
   IW->iLines = NULL;
   IW->iLineNs = NULL;
   IW->iCols = NULL;
   IW->iLens = NULL;
   return;
}

/******************************************************************************
* FreeFW.
******************************************************************************/

void FreeFW (FW, fatalFnc)
struct FieldWindowStruct *FW;
void (*fatalFnc) PROTOARGs((char *msg));
{
   if (FW->NfLines > 0) {
     int lineN;
     for (lineN = FW->NfLines - 1; lineN >= 0; lineN--) {
	cdf_FreeMemory (FW->fLines[lineN], fatalFnc);
     }
     cdf_FreeMemory (FW->fLines, fatalFnc);
   }
   if (FW->nFields > 0) {
     cdf_FreeMemory (FW->fLens, fatalFnc);
     cdf_FreeMemory (FW->fCols, fatalFnc);
     cdf_FreeMemory (FW->fLineNs, fatalFnc);
   }
   FW->NfLines = 0;
   FW->nFields = 0;
   FW->fLines = NULL;
   FW->fLineNs = NULL;
   FW->fCols = NULL;
   FW->fLens = NULL;
   return;
}


/******************************************************************************
* OnlineHelpWindow.
******************************************************************************/

Logical OnlineHelpWindow (ilhFile, helpId)
char *ilhFile;
int helpId;
{
  AOSs1A (header, BLANKs78)
  AOSs1B (trailer,
	  "Exit: ________  NextScreen: ________  PrevScreen: ________")
  static char errorLines[] = "Online help not available.\n";
  static int exitChars[] = { EXITkey_FSI, NUL };
  static char label[] = { BLANKs78 };
  static Logical first = TRUE;
  static struct EditWindowStruct EW = {
    label, 0, 0, SCREEN_WIDTH, 1, header, NULL, 18, 1, trailer, TRUE, TRUE,
    exitChars, REFRESHkey_FSI, NUL, NUL, NUL, NUL, NSkey_FSI, PSkey_FSI, NUL,
    NUL, NUL
  };
  /****************************************************************************
  * Encode label and key definitions the first time.
  ****************************************************************************/
  if (first) {
    snprintf (EW.label, sizeof(BLANKs78), " %s Online Help ", pgmName);
    EncodeKeyDefinitions (1, EW.tLines, EXITkey_FSI, NSkey_FSI, PSkey_FSI);
    first = FALSE;
  }
  /****************************************************************************
  * Load online help.
  ****************************************************************************/
  if (!LoadOnlineHelp(ilhFile,helpId,EW.hLines[0],&EW.eText)) {
    cdf_FreeMemory (EW.eText, FatalError);
    strcpyX (EW.hLines[0], "Error!", 0);
    EW.eText = errorLines;
    EditWindow (NEWew, &EW, TRUE);
    EditWindow (READew, &EW);
    EditWindow (DELETEew, &EW);
    return FALSE;
  }
  /****************************************************************************
  * Display help window/wait for exit key.
  ****************************************************************************/
  EditWindow (NEWew, &EW, TRUE);
  EditWindow (READew, &EW);
  EditWindow (DELETEew, &EW);
  /****************************************************************************
  * Cleanup and return.
  ****************************************************************************/
  cdf_FreeMemory (EW.eText, FatalError);
  return TRUE;
}

/******************************************************************************
* LoadOnlineHelp.
*     You'll notice that the buffers used to hold lines of online help are
* allocated as (SCREEN_WIDTH-2)+1+1.  This is for the number of characters,
* the newline (as returned by `fgets'), and the terminating NUL character.
* When `fgets' is called, the number of characters to read is specified as
* (SCREEN_WIDTH-2)+1+1.  This is for the actual characters plus the newline
* plus one more since `fgets' subtracts one from this value and uses that as
* the maximum number of characters to read (which includes the newline).
******************************************************************************/

Logical LoadOnlineHelp (ilhFile, helpId, header, eText)
char *ilhFile;
int helpId;
char *header;
char **eText;
{
  FILE *fp; int i; size_t length;
  char beginStr[15+1], line[(SCREEN_WIDTH-2)+1+1], helpIdStr[15+1];
  enum { ITEMw, PROMPTw, EDITw } windowType;
  int nBlanks;
  int nestLevel = 0;    /* Depth into `#ifos's. */
  int osMask = 0;       /* When 0, display line.  Note that bit 0 is not used
			   (eg. nesting depth of 1 uses bit 1, etc.). */
#if defined(vms)
  static char thisOS[] = "vms";
#endif
#if defined(unix) || defined(posixSHELL)
  static char thisOS[] = "unix";
#endif
#if defined(dos)
  static char thisOS[] = "dos";
#endif
#if defined(mac)
  static char thisOS[] = "mac";
#endif
#if defined(win32)
  static char thisOS[] = "win";
#endif
  /****************************************************************************
  * Initialize.
  ****************************************************************************/
  *eText = cdf_AllocateMemory ((size_t) 1, FatalError);
  MakeNUL (*eText);
  /****************************************************************************
  * Open help file.
  ****************************************************************************/
  fp = OnlineHelpFP (ilhFile, NULL);
  if (fp == NULL) return FALSE;
  /****************************************************************************
  * Read through help file looking for proper help section.
  ****************************************************************************/
  snprintf (beginStr, (size_t) sizeof(beginStr), "#section %d", helpId);
  while (fgets(line,(SCREEN_WIDTH-2)+1+1,fp) != NULL) {
    /**************************************************************************
    * Strip trailing newline character and check to see if the help section
    * has been found.
    **************************************************************************/
    line[strlen(line)-1] = NUL;
    if (!strcmp(line,beginStr)) {
      /************************************************************************
      * Determine window type.
      ************************************************************************/
      if (fgets(line,(SCREEN_WIDTH-2)+1+1,fp) == NULL) {
	fclose (fp);
	return FALSE;
      }
      line[strlen(line)-1] = NUL;
      if (!strcmp(line,"#item"))
	windowType = ITEMw;
      else
	if (!strcmp(line,"#prompt"))
	  windowType = PROMPTw;
	else
	  if (!strcmp(line,"#edit"))
	    windowType = EDITw;
	  else {
	    fclose (fp);
	    return FALSE;
	  }
      /************************************************************************
      * Build header.
      ************************************************************************/
      if (fgets(line,(SCREEN_WIDTH-2)+1+1,fp) == NULL) {
	fclose (fp);
	return FALSE;
      }
      line[strlen(line)-1] = NUL;
      if (strncmp(line,"#title ",7) != 0) {
	fclose (fp);
	return FALSE;
      }
      strcpyX (header, "Help for ", 0);
      strcatX (header, &line[7], 0);
      snprintf (helpIdStr, (size_t) sizeof(helpIdStr), "[%d%c]", helpId,
	        (windowType == ITEMw ? 'i' : (windowType == PROMPTw ? 'p' :
		 (windowType == EDITw ? 'e' : '?'))));
      nBlanks = (SCREEN_WIDTH-2) - strlen(header) - strlen(helpIdStr);
      CatNcharacters (header, nBlanks, (int) ' ');
      strcatX (header, helpIdStr, 0);
      /************************************************************************
      * Read and save lines until "#endsection" is found.
      ************************************************************************/
      while (fgets(line,(SCREEN_WIDTH-2)+1+1,fp) != NULL) {
	line[strlen(line)-1] = NUL;
	/**********************************************************************
	* Check if at end of this section of help.  If so, delete trailing
	* newline characters, encode key definitions based on window type,
	* close help file, and return.
	**********************************************************************/
	if (!strcmp(line,"#endsection")) {
	  if (nestLevel != 0) {
	    fclose (fp);
	    return FALSE;
	  }
	  length = strlen (*eText);
	  if (length > 0) {
	    for (i = length - 1; i >= 0; i--) {
	       if ((*eText)[i] != Nl) break;
	       (*eText)[i] = NUL;
	    }
	  }
	  switch (windowType) {
	    case ITEMw:
	      EncodeKeyDefinitions (1, eText, NSkey_FSI, PSkey_FSI);
	      break;
	    case PROMPTw:
	      EncodeKeyDefinitions (1, eText, SOLkey_FSI, EOLkey_FSI,
				    INSERTorOVERkey_FSI);
	      break;
	    case EDITw:
	      EncodeKeyDefinitions (1, eText, NSkey_FSI, PSkey_FSI);
	      break;
	  }
	  fclose (fp);
	  return TRUE;
	}
	/**********************************************************************
	* Not at end yet.  Check if an operating system directive.  If not,
	* include the line if all of the bits in the operating system mask
	* are clear.
	**********************************************************************/
	if (line[0] == '#') {
	  /********************************************************************
	  * Check for an `#ifos' directive.  If this operating system is not
	  * specified, then set the bit in the operating system mask for this
	  * nesting level.
	  ********************************************************************/
	  if (!strncmp(line,"#ifos",5)) {
	    nestLevel++;
	    if (strstr(line,thisOS) == NULL) SETBIT (osMask, nestLevel);
	    continue;
	  }
	  /********************************************************************
	  * Check for an `#else' directive.  Simply flip the bit in the
	  * operating system mask for this nesting level.
	  ********************************************************************/
	  if (!strcmp(line,"#else")) {
	    if (nestLevel < 1) {
	      fclose (fp);
	      return FALSE;
	    }
	    FLPBIT (osMask, nestLevel);
	    continue;
	  }
	  /********************************************************************
	  * Check for an `#endos' directive.  Clear the bit in the operating
	  * system mask for this nesting level and decrement the nesting level.
	  ********************************************************************/
	  if (!strcmp(line,"#endos")) {
	    if (nestLevel < 1) {
	      fclose (fp);
	      return FALSE;
	    }
	    CLRBIT (osMask, nestLevel);
	    nestLevel--;
	    continue;
	  }
	  /********************************************************************
	  * An unknown directive has been encountered.
	  ********************************************************************/
	  fclose (fp);
	  return FALSE;
	}
	else {
	  if (osMask == 0) {
	    size_t length, newLength;
#if defined(dos)
	    int tabCount, i;
#endif
	    length = strlen (line);
#if defined(dos)
	    for (tabCount = 0, i = 0; i < length; i++) {
	       if (line[i] == Ht) tabCount++;
	    }
	    length += (7 * tabCount);
#endif
	    newLength = strlen(*eText) + (length + 1) + 1;
	    *eText = cdf_ReallocateMemory (*eText, newLength,
				       FatalError);
#if defined(dos)
	    for (i = 0; i < tabCount; i++) {
	       strcatX (*eText, "        ", 0);
	    }
	    strcatX (*eText, &line[i], 0);
#else
	    strcatX (*eText, line, 0);
#endif
	    strcatX (*eText, "\n", 0);
	  }
	}
      }
      /************************************************************************
      * `#endsection' not found - error return.
      ************************************************************************/
      fclose (fp);
      return FALSE;
    }
  }
  /****************************************************************************
  * `#section x' not found - error return.
  ****************************************************************************/
  fclose (fp);
  return FALSE;
}

/******************************************************************************
* InfoWindow.
******************************************************************************/

void InfoWindow (message1, message2, message3, center, beep, wait)
char *message1;         /* This message line must exist. */
char *message2;         /* This message line is optional (NULL if absent). */
char *message3;         /* This message line is optional (NULL if absent).
			   If `message2' is NULL, this must also be NULL. */
Logical center;         /* TRUE if window should be in center of screen. */
Logical beep;           /* TRUE if window should beep after being displayed. */
int wait;               /* 0: Read a key before deleting window.
			   >0: Wait `wait' seconds and then delete window. */
{
   static int exitChars[] = { NUL };
   static char eText[INFOtextMAX+1];
   static char ackLabel[] = " Enter any key to acknowledge. ";
   static struct EditWindowStruct EW = {
     NULL, 0, 0, 0, 0, NULL, eText, 0, 0, NULL, FALSE, TRUE, exitChars,
     REFRESHkey_FSI, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL, NUL
   };
   EW.label = BOO(wait > 0,NULL,ackLabel);
   if (center) {
     EW.nColsTotal = BOO(EW.label == NULL,0,(int)(strlen(EW.label) + 4));
     EW.nColsTotal = MaxInt (EW.nColsTotal, (int) (strlen(message1) + 2));
     strcpyX (EW.eText, message1, INFOtextMAX);
     strcatX (EW.eText, "\n", INFOtextMAX);
     EW.ULrow = (SCREEN_HEIGHT - 3) / 2;
     EW.NeRows = 1;
     if (message2 != NULL) {
       EW.nColsTotal = MaxInt (EW.nColsTotal, (int) (strlen(message2) + 2));
       strcatX (EW.eText, message2, INFOtextMAX);
       strcatX (EW.eText, "\n", INFOtextMAX);
       EW.ULrow = (SCREEN_HEIGHT - 4) / 2;
       EW.NeRows = 2;
     }
     if (message3 != NULL) {
       EW.nColsTotal = MaxInt (EW.nColsTotal, (int) (strlen(message3) + 2));
       strcatX (EW.eText, message3, INFOtextMAX);
       strcatX (EW.eText, "\n", INFOtextMAX);
       EW.ULrow = (SCREEN_HEIGHT - 5) / 2;
       EW.NeRows = 3;
     }
     EW.ULcol = (SCREEN_WIDTH - EW.nColsTotal) / 2;
   }
   else {
     strcpyX (EW.eText, message1, INFOtextMAX);
     strcatX (EW.eText, "\n", INFOtextMAX);
     EW.ULrow = SCREEN_HEIGHT - 3;
     EW.NeRows = 1;
     if (message2 != NULL) {
       strcatX (EW.eText, message2, INFOtextMAX);
       strcatX (EW.eText, "\n", INFOtextMAX);
       EW.ULrow = SCREEN_HEIGHT - 4;
       EW.NeRows = 2;
     }
     if (message3 != NULL) {
       strcatX (EW.eText, message3, INFOtextMAX);
       strcatX (EW.eText, "\n", INFOtextMAX);
       EW.ULrow = SCREEN_HEIGHT - 5;
       EW.NeRows = 3;
     }
     EW.nColsTotal = SCREEN_WIDTH;
     EW.ULcol = 0;
   }
   EditWindow (NEWew, &EW, TRUE);
   if (beep) EditWindow (BEEPew, &EW);
   if (wait > 0)
     zzzzz ((double) wait);
   else
     EditWindow (READew, &EW);
   EditWindow (DELETEew, &EW);
   return;
}
