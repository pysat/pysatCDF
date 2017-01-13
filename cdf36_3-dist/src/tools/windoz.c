/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                            Windowing functions.
*
*  Version 4.0a, 15-Dec-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  29-Jan-91, H Leckner  Original version (for CDF V2.0).
*   V1.1   4-Aug-91, J Love     TRUE/FALSE.  Minor change to borders (well
*                    H Leckner  maybe).
*   V1.2  13-Aug-91, H Leckner  Fixed border labeling.
*   V1.3   8-Oct-91, J Love     Modified for IRIX 4.0 port, etc.
*                    H Leckner
*   V2.0  30-Apr-92, H Leckner  CDF V2.2.  IBM PC port.  Added `input_field'
*                    J Love     and `get_input'.
*   V2.1  20-Jul-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V3.0   9-Dec-93, J Love     CDF V2.4.  Generalized for all platforms.
*   V3.1  21-Dec-94, J Love	CDF V2.5.
*   V3.2  23-Jan-95, J Love	IRIX 6.x (64-bit).
*   V3.2a 31-Jan-95, J Love	Fixed `WindowSize' for VMS.
*   V3.2b  7-Feb-95, J Love	Made `WindowLocation' and `WindowSize'
*				function prototypes `static'.
*   V3.2c  3-Mar-95, J Love	Moved `EncodeKeyDefinitions', etc. to this
*				file.
*   V3.3   4-Apr-95, J Love	POSIX.
*   V3.3a 18-Apr-95, J Love	More POSIX.
*   V3.3b 13-Jun-95, J Love	Linux.
*   V3.4  16-Jun-95, J Love	`key_waiting'.
*   V3.4a 11-Jul-95, J Love	`key_waiting' for UNIX.
*   V3.4b  7-Sep-95, J Love	CDFexport-related changes.
*   V3.4c 19-Sep-95, J Love	Macintosh event handling.
*   V3.4d 28-Sep-95, J Love	Increased precision of `zzzzz' on Macintosh.
*				The CDF cursor.
*   V3.5   3-Oct-96, J Love	CDF V2.6.
*   V3.5a  2-Sep-97, J Love	Special keys for AIX.
*   V4.0  14-Nov-97, J Love	Windows NT/Visual C++.
*   V4.0a 15-Dec-97, J Love	IEEE floating-point on Alpha/OpenVMS.
*   V4.1   2-May-01, M Liu      Special keys for CYGWIN.
*   V4.2  11-Jul-05, M Liu      Added MingW port for PC.
*
*******************************************************************************
* NOTES:
*    1. All functions return TRUE if successful, FALSE if an error occurred.
*    2. The borders provided by SMG are not used.  SMG$DRAW_RECTANGLE is used
*       if a border is requested.  This is to allow the `draw_horizontal_line'
*       and 'draw_vertical_line' functions the ability to draw a line all the
*       way to the border (where SMG "tees" the line).
*    3. On the IBM PC...
*       a) It doesn't seem to be possible to OR in attributes with a
*          character.  Currently using `wattrset' before writing characters.
*       b) The cursor cannot be positioned on a character position that has
*          not been written to.
*    4. On the VAX (SMG), using SMG$SET_CURSOR_ABS doesn't seem to work if
*	the pasteboard is being batched (SMG$BEGIN/END_PASTEBOARD_UPDATE).
*	The cursor is always placed at (1,1) when the screen is updated.  So
*	don't do that.
******************************************************************************/

/******************************************************************************
* Include files.
******************************************************************************/

#include "windoz.h"

#if defined(win32)
#include "windows.h"
#endif

#if defined(mac)
#include "fsi.rh"
#endif

/******************************************************************************
* Macros.
******************************************************************************/

#define INTERPRET_ESC_SEQUENCES	1
#define ESC			27

/******************************************************************************
* Global variables.
******************************************************************************/

#if defined(SMGui)
static long pbid;
static long kbid;
static int keyWaiting;
#endif

#if defined(CURSESui)
static int batchCount;			/* When zero (0), updates to screen
					   (pasteboard) are applied
					   immediately. */
#endif

#if defined(COWui)
#if defined(mac)
static WindowPtr fsiWindowP;		/* Full screen interface window
					   pointer. */
static MenuHandle appleMenuHfsi;	/* Menu handle for apple. */
#endif
static WINDOWid cursorWIND;		/* Window in which the cursor was
					   last set. */
static int cursorRow;			/* Row at which cursor was last set. */
static int cursorCol;			/* Column at which cursor was last
					   set. */
static Logical cursorVisible;		/* TRUE if the cursor is currently
					   visible. */
static int batchCount;			/* When zero (0), updates to screen
					   (pasteboard) are applied
					   immediately. */
static char *pbNewChars;		/* New characters for the
					   pasteboard. */
static char *pbNewAttrs;		/* New attributes for the
					   pasteboard. */
static char *pbCurrentChars;		/* Current characters on the
					   pasteboard. */
static char *pbCurrentAttrs;		/* Current attributes on the
					   pasteboard. */
static char *pbNullChars;		/* Pasteboard of null characters. */
static char *pbBlankChars;		/* Pasteboard of blank characters. */
static char *pbNormalAttrs;		/* Normal pasteboard of attributes. */
#endif

static WINDOWid WINDhead;		/* Head of window linked list.  1st
					   window on list is most occluded.
					   Last window on list is on top of
					   all other windows (most recently
					   pasted). */
static Logical cursorOn;		/* TRUE if the cursor has been set to
					   be visible (when not occluded). */

/******************************************************************************
* Local function prototypes.
******************************************************************************/

#if defined(SMGui)
static void KeyWaitingAST PROTOARGs((void));
#endif

#if defined(CURSESui)
static void WindowLocation PROTOARGs((LocalId id, int *row, int *col));
#endif

#if defined(CURSESui) || defined(SMGui)
static void WindowSize PROTOARGs((LocalId id, int *rows, int *cols));
#endif

static int RenditionMapping PROTOARGs((int));
static WINDOWid AddWIND PROTOARGs((LocalId, Logical, Logical));
static Logical MoveWINDtoEnd PROTOARGs((WINDOWid));
static Logical DeleteWIND PROTOARGs((WINDOWid));
static Logical DeleteWINDs PROTOARGs((void));

#if defined(CURSESui) || defined(COWui)
static Logical RefreshWINDs PROTOARGs((int level));
#endif

#if defined(CURSESui)
static int EraseWindow PROTOARGs((LocalId));
#endif

#if defined(COWui)
#if defined(mac)
static void InvertCursor PROTOARGs((WINDOWid wid, int rowN, int colN));
#endif
static Logical CharOccluded PROTOARGs((WINDOWid wid, int rowN, int colN));
#endif

static char *KeyToken PROTOARGs((int, int));
static char *AlphaKey PROTOARGs((int, int));
static char *ControlKey PROTOARGs((int, int));

/******************************************************************************
* begin_pasteboard_update.
******************************************************************************/

int begin_pasteboard_update () {
#if defined(SMGui)
  if (StatusBad(smg$begin_pasteboard_update(&pbid))) return FALSE;
  return TRUE;
#endif
#if defined(CURSESui)
  batchCount++;
  return TRUE;
#endif
#if defined(COWui)
  batchCount++;
  return TRUE;
#endif
}

/******************************************************************************
* change_rendition.
******************************************************************************/

int change_rendition (wid, row, col, num_rows, num_cols, rendition)
WINDOWid wid;
int row;
int col;
int num_rows;
int num_cols;
int rendition;
{
#if defined(SMGui)
  long rowN = row + 1;
  long colN = col + 1;
  uLongx rend = RenditionMapping (rendition);
  if (StatusBad(smg$change_rendition(&(wid->id),&rowN,&colN,
				     &num_rows,&num_cols,&rend))) return FALSE;
  return TRUE;
#endif
#if defined(CURSESui)
  int i,j;
  wattrset (wid->id, RenditionMapping(rendition));
  for (i = 0; i < num_rows; i++)
     for (j = 0; j < num_cols; j++)
	mvwaddch (wid->id, row+i, col+j,
		  mvwinch(wid->id,row+i,col+j) & A_CHARTEXT);
  if (!RefreshWINDs(UPDATE_)) return FALSE;
  return TRUE;
#endif
#if defined(COWui)
  char rend = RenditionMapping (rendition);
  char *attrs = wid->id->attrs;
  int i, j;
  for (i = 0; i < num_rows; i++)
     for (j = 0; j < num_cols; j++) {
	int charN = (wid->id->nCols*(row+i)) + (col+j);
	attrs[charN] = rend;
     }
  if (!RefreshWINDs(UPDATE_)) return FALSE;
  return TRUE;
#endif
}

/******************************************************************************
* create_pasteboard.
******************************************************************************/

int create_pasteboard () {
#if defined(SMGui)
  if (StatusBad(smg$create_pasteboard(&pbid,NULL,NULL,NULL,NULL,NULL))) {
    return FALSE;
  }
  if (StatusBad(smg$create_virtual_keyboard(&kbid,NULL,NULL,NULL,NULL))) {
    return FALSE;
  }
  if (StatusBad(smg$enable_unsolicited_input(&pbid,KeyWaitingAST,NULL))) {
    return FALSE;
  }
  keyWaiting = 0;
  WINDhead = NULL;
  set_cursor_mode (CURSORon);
  return TRUE;
#endif
#if defined(CURSESui)
  initscr ();
  keypad (stdscr, TRUE);
  noecho ();
  nonl ();
  raw ();
  refresh ();			/* Clears screen. */
  batchCount = 0;
  WINDhead = NULL;
  set_cursor_mode (CURSORon);
  return TRUE;
#endif
#if defined(COWui)
  int charN, nChars = NUMfsiROWS * NUMfsiCOLS;
#if defined(mac)
  Rect eraseRect = { 0, 0, WINDOWfsiHEIGHT, WINDOWfsiWIDTH };
#endif
  batchCount = 0;
  WINDhead = NULL;
  cursorWIND = NULL;
  cursorVisible = FALSE;
  cursorOn = FALSE;
  pbCurrentChars = (char *) cdf_AllocateMemory ((size_t)nChars, FatalError);
  pbCurrentAttrs = (char *) cdf_AllocateMemory ((size_t)nChars, FatalError);
  pbNewChars = (char *) cdf_AllocateMemory ((size_t)nChars, FatalError);
  pbNewAttrs = (char *) cdf_AllocateMemory ((size_t)nChars, FatalError);
  pbNullChars = (char *) cdf_AllocateMemory ((size_t)nChars, FatalError);
  pbBlankChars = (char *) cdf_AllocateMemory ((size_t)nChars, FatalError);
  pbNormalAttrs = (char *) cdf_AllocateMemory ((size_t)nChars, FatalError);
  for (charN = 0; charN < nChars; charN++) {
     pbNullChars[charN] = 0;
     pbBlankChars[charN] = ' ';
     pbNormalAttrs[charN] = 0;
  }
  memmove (pbCurrentChars, pbBlankChars, nChars);
  memmove (pbCurrentAttrs, pbNormalAttrs, nChars);
  set_cursor_mode (CURSORon);
#if defined(mac)
  EraseRect (&eraseRect);
  ShowWindow (fsiWindowP);
#endif
#if defined(win32)
   /* Jeff...maybe nothing needs to be done. */
#endif
  return TRUE;
#endif
}

/******************************************************************************
* create_virtual_display.
* The window is not visibile until the function `paste_virtual_display' is
* used.
******************************************************************************/

int create_virtual_display (num_rows, num_cols, wid, borderMode, rendition)
int num_rows;
int num_cols;
WINDOWid *wid;
int borderMode;			/* If BORDER, draw border around display. */
int rendition;			/* Rendition of border (if bordered). */
{
#if defined(SMGui)
  LocalId id;
  uLongx attrs = 0;
  uLongx rend = RenditionMapping (NORMAL);   /* Default for display. */
  long nRows = num_rows;
  long nCols = num_cols;
  if (StatusBad(smg$create_virtual_display(&nRows,&nCols,&id,
					   &attrs,&rend,NULL))) return FALSE;
  *wid = AddWIND (id, FALSE, (borderMode == BORDER));
  if (borderMode == BORDER) draw_rectangle (*wid, 0, 0, num_rows - 1,
					    num_cols - 1, rendition);
  return TRUE;
#endif
#if defined(CURSESui)
  LocalId id = newwin (num_rows, num_cols, 0, 0);
  if (id == NULL) return FALSE;
  keypad (id, TRUE);
  *wid = AddWIND (id, FALSE, (borderMode == BORDER));
  erase_display (*wid, 0, 0, num_rows - 1, num_cols - 1);
  if (borderMode == BORDER) draw_rectangle (*wid, 0, 0, num_rows - 1,
					    num_cols - 1, rendition);
  return TRUE;
#endif
#if defined(COWui)
  LocalId id;
  int nChars = num_rows * num_cols;
  if (num_rows < 1 || num_rows > NUMfsiROWS) return FALSE;
  if (num_cols < 1 || num_cols > NUMfsiCOLS) return FALSE;
  id = (LocalId) cdf_AllocateMemory ((size_t)sizeof(COWvd), FatalError);
  id->nRows = num_rows;
  id->nCols = num_cols;
  id->atRowN = 0;
  id->atColN = 0;
  id->chars = (char *) cdf_AllocateMemory ((size_t)nChars, FatalError);
  id->attrs = (char *) cdf_AllocateMemory ((size_t)nChars, FatalError);
  *wid = AddWIND (id, FALSE, (borderMode == BORDER));
  memmove (id->chars, pbBlankChars, nChars);
  memmove (id->attrs, pbNormalAttrs, nChars);
  if (borderMode == BORDER) draw_rectangle (*wid, 0, 0, num_rows - 1,
					    num_cols - 1, rendition);
  return TRUE;
#endif
}

/******************************************************************************
* delete_pasteboard.
******************************************************************************/

int delete_pasteboard (eraseMode)
int eraseMode;
{
#if defined(SMGui)
  uLongx flags = (eraseMode == ERASE ? SMG$M_ERASE_PBD : 0);
  Logical status = DeleteWINDs();
  if (StatusBad(smg$delete_pasteboard(&pbid,&flags))) return FALSE;
  return status;
#endif
#if defined(CURSESui)
  Logical status = DeleteWINDs();
  if (eraseMode == ERASE) {     /* Don't change default attributes first. */
    EraseWindow (stdscr);
    wrefresh (stdscr);
  }
  endwin ();
  return status;
#endif
#if defined(COWui)
  Logical status = DeleteWINDs();
#if defined(mac)
  Rect eraseRect = { 0, 0, WINDOWfsiHEIGHT, WINDOWfsiWIDTH };
#endif
#if defined(mac)
  if (eraseMode == ERASE) EraseRect (&eraseRect);
#endif
  cdf_FreeMemory (pbCurrentChars, FatalError);
  cdf_FreeMemory (pbCurrentAttrs, FatalError);
  cdf_FreeMemory (pbNewChars, FatalError);
  cdf_FreeMemory (pbNewAttrs, FatalError);
  cdf_FreeMemory (pbNullChars, FatalError);
  cdf_FreeMemory (pbBlankChars, FatalError);
  cdf_FreeMemory (pbNormalAttrs, FatalError);
#if defined(mac)
  HideWindow (fsiWindowP);
#endif
  return status;
#endif
}

/******************************************************************************
* delete_virtual_display.
******************************************************************************/

int delete_virtual_display (wid)
WINDOWid wid;
{
  if (!DeleteWIND(wid)) return FALSE;
  return TRUE;
}

/******************************************************************************
* draw_horizontal_line.
******************************************************************************/

int draw_horizontal_line (wid, rowN, LcolN, RcolN, rendition, teeEnds)
WINDOWid wid;           /* Window id. */
int rowN;               /* Row number. */
int LcolN;              /* Left column number (start) -- numbered from zero. */
int RcolN;              /* Right column number (stop) -- numbered from zero. */
int rendition;          /* Rendition to use. */
Logical teeEnds;	/* If TRUE, draw ACS_LTEE and ACS_RTEE at the ends of
			   the line (ACS_HLINE is drawn at all of the other
			   positions).  If FALSE, draw ACS_HLINE at all of the
			   positions.  On SMG systems (VAX/VMS), this option
			   is ignored (SMG decides whether or not to tee the
			   ends based on the existing characters at those
			   positions). */
{
#if defined(SMGui)
  long startRowN = rowN + 1;
  long endRowN = startRowN;
  long startColN = LcolN + 1;
  long endColN = RcolN + 1;
  uLongx rend = RenditionMapping(rendition);
  if (StatusBad(smg$draw_line(&(wid->id),&startRowN,&startColN,
			      &endRowN,&endColN,&rend,NULL))) return FALSE;
#endif
#if defined(CURSESui)
   int colN;            /* Column number -- numbered from zero. */
   wattrset (wid->id, RenditionMapping(rendition));
   mvwaddch (wid->id, rowN, LcolN, (teeEnds ? ACS_LTEE : ACS_HLINE));
   for (colN = LcolN + 1; colN < RcolN; colN++)
      mvwaddch (wid->id, rowN, colN, ACS_HLINE);
   mvwaddch (wid->id, rowN, RcolN, (teeEnds ? ACS_RTEE : ACS_HLINE));
   if (!RefreshWINDs(UPDATE_)) return FALSE;
   return TRUE;
#endif
#if defined(COWui)
   int colN,            /* Column number -- numbered from zero. */
       charN;		/* Character number -- numbered from zero. */
   char rend = RenditionMapping (rendition);
   short nCols = wid->id->nCols;
   char *chars = wid->id->chars,
	*attrs = wid->id->attrs;
   charN = (rowN * nCols) + LcolN;
   chars[charN] = (teeEnds ? ACS_LTEE : ACS_HLINE);
   attrs[charN] = rend;
   for (colN = LcolN + 1; colN < RcolN; colN++) {
      charN = (rowN * nCols) + colN;
      chars[charN] = ACS_HLINE;
      attrs[charN] = rend;
   }
   charN = (rowN * nCols) + RcolN;
   chars[charN] = (teeEnds ? ACS_RTEE : ACS_HLINE);
   attrs[charN] = rend;
   if (!RefreshWINDs(UPDATE_)) return FALSE;
   return TRUE;
#endif
}

/******************************************************************************
* draw_vertical_line.
******************************************************************************/

int draw_vertical_line (wid, TrowN, BrowN, colN, rendition, teeEnds)
WINDOWid wid;           /* Window id. */
int TrowN;              /* Top row number (start) -- numbered from zero. */
int BrowN;              /* Bottom row number (stop) -- numbered from zero. */
int colN;               /* Column number -- numbered from zero. */
int rendition;          /* Rendition to use. */
Logical teeEnds;	/* If TRUE, draw ACS_TTEE and ACS_BTEE at the ends of
			   the line (ACS_VLINE is drawn at all of the other
			   positions).  If FALSE, draw ACS_VLINE at all of the
			   positions.  On SMG systems (VAX/VMS), this option
			   is ignored (SMG decides whether or not to tee the
			   ends based on the existing characters at those
			   positions). */
{
#if defined(SMGui)
  long startRowN = TrowN + 1;
  long endRowN = BrowN + 1;
  long startColN = colN + 1;
  long endColN = startColN;
  uLongx rend = RenditionMapping(rendition);
  if (StatusBad(smg$draw_line(&(wid->id),&startRowN,&startColN,
			      &endRowN,&endColN,&rend,NULL))) return FALSE;
#endif
#if defined(CURSESui)
   int rowN;            /* Row number -- numbered from zero. */
   wattrset (wid->id, RenditionMapping(rendition));
   mvwaddch (wid->id, TrowN, colN, (teeEnds ? ACS_TTEE : ACS_VLINE));
   for (rowN = TrowN + 1; rowN < BrowN; rowN++)
      mvwaddch (wid->id, rowN, colN, ACS_VLINE);
   mvwaddch (wid->id, BrowN, colN, (teeEnds ? ACS_BTEE : ACS_VLINE));
   if (!RefreshWINDs(UPDATE_)) return FALSE;
   return TRUE;
#endif
#if defined(COWui)
   int rowN,            /* Row number -- numbered from zero. */
       charN;		/* Character number -- numbered from zero. */
   char rend = RenditionMapping (rendition);
   short nCols = wid->id->nCols;
   char *chars = wid->id->chars,
	*attrs = wid->id->attrs;
   charN = (TrowN * nCols) + colN;
   chars[charN] = (teeEnds ? ACS_TTEE : ACS_VLINE);
   attrs[charN] = rend;
   for (rowN = TrowN + 1; rowN < BrowN; rowN++) {
      charN = (rowN * nCols) + colN;
      chars[charN] = ACS_VLINE;
      attrs[charN] = rend;
   }
   charN = (BrowN * nCols) + colN;
   chars[charN] = (teeEnds ? ACS_BTEE : ACS_VLINE);
   attrs[charN] = rend;
   if (!RefreshWINDs(UPDATE_)) return FALSE;
   return TRUE;
#endif
}

/******************************************************************************
* draw_rectangle.
******************************************************************************/

int draw_rectangle (wid, Trow, Lcol, Brow, Rcol, rendition)
WINDOWid wid;
int Trow;		/* Top row number. */
int Lcol;		/* Left column number. */
int Brow;		/* Bottom row number. */
int Rcol;		/* Right column number. */
int rendition;		/* Video attributes. */
{
#if defined(SMGui)
  long startRow = Trow + 1;
  long startCol = Lcol + 1;
  long endRow = Brow + 1;
  long endCol = Rcol + 1;
  uLongx rend = RenditionMapping (rendition);
  if (StatusBad(smg$draw_rectangle(&(wid->id),&startRow,&startCol,
				   &endRow,&endCol,&rend,NULL))) return FALSE;
  return TRUE;
#endif
#if defined(CURSESui)
  int x, y;
  wattrset (wid->id, RenditionMapping(rendition));
  mvwaddch (wid->id, Trow, Lcol, ACS_ULCORNER);
  for (x = Lcol+1; x <= Rcol-1; x++) mvwaddch (wid->id, Trow, x, ACS_HLINE);
  mvwaddch (wid->id, Trow, Rcol, ACS_URCORNER);
  for (y = Trow+1; y <= Brow-1; y++) mvwaddch (wid->id, y, Rcol, ACS_VLINE);
  mvwaddch (wid->id, Brow, Rcol, ACS_LRCORNER);
  for (x = Rcol-1; x >= Lcol+1; x--) mvwaddch (wid->id, Brow, x, ACS_HLINE);
  mvwaddch (wid->id, Brow, Lcol, ACS_LLCORNER);
  for (y = Brow-1; y >= Trow+1; y--) mvwaddch (wid->id, y, Lcol, ACS_VLINE);
  if (!RefreshWINDs(UPDATE_)) return FALSE;
  return TRUE;
#endif
#if defined(COWui)
  char rend = RenditionMapping (rendition);
  short nCols = wid->id->nCols;
  char *chars = wid->id->chars,
       *attrs = wid->id->attrs;
  int charN, x, y;
  charN = (Trow * nCols) + Lcol;
  chars[charN] = ACS_ULCORNER;
  attrs[charN] = rend;
  for (x = Lcol + 1; x <= Rcol - 1; x++) {
     charN = (Trow * nCols) + x;
     chars[charN] = ACS_HLINE;
     attrs[charN] = rend;
  }
  charN = (Trow * nCols) + Rcol;
  chars[charN] = ACS_URCORNER;
  attrs[charN] = rend;
  for (y = Trow + 1; y <= Brow - 1; y++) {
     charN = (y * nCols) + Rcol;
     chars[charN] = ACS_VLINE;
     attrs[charN] = rend;
  }
  charN = (Brow * nCols) + Rcol;
  chars[charN] = ACS_LRCORNER;
  attrs[charN] = rend;
  for (x = Rcol - 1; x >= Lcol + 1; x--) {
     charN = (Brow * nCols) + x;
     chars[charN] = ACS_HLINE;
     attrs[charN] = rend;
  }
  charN = (Brow * nCols) + Lcol;
  chars[charN] = ACS_LLCORNER;
  attrs[charN] = rend;
  for (y = Brow - 1; y >= Trow + 1; y--) {
     charN = (y * nCols) + Lcol;
     chars[charN] = ACS_VLINE;
     attrs[charN] = rend;
  }
  if (!RefreshWINDs(UPDATE_)) return FALSE;
  return TRUE;
#endif
}

/******************************************************************************
* end_pasteboard_update.
******************************************************************************/

int end_pasteboard_update ()
{
#if defined(SMGui)
  if (StatusBad(smg$end_pasteboard_update(&pbid))) return FALSE;
  return TRUE;
#endif
#if defined(CURSESui)
  batchCount = MaxInt (0, batchCount - 1);
  if (!RefreshWINDs(UPDATE_)) return FALSE;
  return TRUE;
#endif
#if defined(COWui)
  batchCount = MaxInt (0, batchCount - 1);
  if (!RefreshWINDs(UPDATE_)) return FALSE;
  return TRUE;
#endif
}

/******************************************************************************
* erase_display.
* This routine erases the BOX specified by the starting and ending character
* positions (which is different from what SMG$ERASE_DISPLAY does on a VAX).
******************************************************************************/

int erase_display (wid, startRow, startCol, endRow, endCol)
WINDOWid wid;
int startRow;
int startCol;
int endRow;
int endCol;
{
#if defined(SMGui)
  long startRowN = startRow + 1;
  long endRowN = endRow + 1;
  long startColN = startCol + 1;
  long nCols = endCol - startCol + 1;
  long rowN;
  for (rowN = startRowN; rowN <= endRowN; rowN++)
     if (StatusBad(smg$erase_chars(&(wid->id),&nCols,
				   &rowN,&startColN))) return FALSE;
  return TRUE;
#endif
#if defined(CURSESui)
  int i, j;
  wattrset (wid->id, RenditionMapping(NORMAL));
  for (i = startRow; i <= endRow; i++)
     for (j = startCol; j <= endCol; j++) mvwaddch (wid->id, i, j, ' ');
  if (!RefreshWINDs(UPDATE_)) return FALSE;
  return TRUE;
#endif
#if defined(COWui)
  int i, j, charN;
  char rend = RenditionMapping (NORMAL);
  char *chars = wid->id->chars,
       *attrs = wid->id->attrs;
  short nCols = wid->id->nCols;
  for (i = startRow; i <= endRow; i++)
     for (j = startCol; j <= endCol; j++) {
	charN = (i * nCols) + j;
	chars[charN] = ' ';
	attrs[charN] = rend;
     }
  if (!RefreshWINDs(UPDATE_)) return FALSE;
  return TRUE;
#endif
}

/******************************************************************************
* input_field.
* The caller must turn the cursor on/off before calling this routine.
* This is only used by CDFlist and CDFwalk (ie. expendable).
******************************************************************************/

int input_field (wid, field, fieldRow, fieldCol, fieldLen, exitKeys, exitKey,
		 toggleInsertModeKey, moveToSOLkey, moveToEOLkey,
		 deleteToSOLkey, deleteToEOLkey, refreshKey)
WINDOWid wid;
char *field;                    /* The field may contain an initial value. */
int fieldRow;
int fieldCol;
int fieldLen;                   /* Maximum length of field. */
int *exitKeys;                  /* Keys which cause input field to be
				   exited (NUL-terminated). */
int *exitKey;			/* Key which actually caused exit. */
int toggleInsertModeKey;        /* Key causing insert mode to be toggled
				   (between insert and overstrike). */
int moveToSOLkey;		/* Key causing cursor to move to the start
				   of the field. */
int moveToEOLkey;		/* Key causing cursor to move to the end of
				   the field. */
int deleteToSOLkey;		/* Key causing the characters in front of
				   the cursor to be deleted (and then the
				   cursor/remaining characters are moved up. */
int deleteToEOLkey;		/* Key causing the characters starting at the
				   cursor to be deleted. */
int refreshKey;			/* Key causing the screen to be refreshed. */
{
  int curLen = (int) strlen(field);
  int insertMode = FALSE;	/* Initially in insert mode. */
  int firstCol = fieldCol;
  int lastCol = fieldCol + fieldLen - 1;
  int curChar, colN, key, i;
  /****************************************************************************
  * Write current field contents and position cursor.
  ****************************************************************************/
  if (!put_chars(wid,field,curLen,
		 fieldRow,fieldCol,FALSE,NORMAL)) return FALSE;
  for (colN = firstCol + curLen; colN <= lastCol; colN++)
     if (!put_chars(wid," ",1,fieldRow,colN,FALSE,NORMAL)) return FALSE;
  curChar = 0;
  if (!set_cursor_abs(wid,fieldRow,firstCol+curChar)) return FALSE;
  /****************************************************************************
  * Read keystrokes until return/exit.
  ****************************************************************************/
  for (;;) {
     if (!read_input(
#if defined(CURSESui)
		     wid,
#endif
			 &key,PASSTHRUri,TRUE)) return FALSE;
     /*************************************************************************
     * Check for an exit key.
     *************************************************************************/
     for (i = 0; exitKeys[i] != NUL; i++)
	if (key == exitKeys[i]) {
	  *exitKey = key;
	  return TRUE;
	}
     /*************************************************************************
     * Left arrow key.
     *************************************************************************/
     if (key == KB_LEFTARROW) {
       if (curChar > 0) {
	 curChar--;
	 if (!set_cursor_abs(wid,fieldRow,firstCol+curChar)) return FALSE;
       }
       else
	 ring_bell ();
       continue;
     }
     /*************************************************************************
     * Right arrow key.
     *************************************************************************/
     if (key == KB_RIGHTARROW) {
       if (curChar < curLen && curChar < fieldLen - 1) {
	 curChar++;
	 if (!set_cursor_abs(wid,fieldRow,firstCol+curChar)) return FALSE;
       }
       else
	 ring_bell ();
       continue;
     }
     /*************************************************************************
     * Delete key.
     *************************************************************************/
     if (key == KB_DELETE) {
       if (curChar > 0) {
	 int charsToEnd = curLen - curChar;
	 memmove (&field[curChar-1], &field[curChar], charsToEnd + 1);
	 curLen--;
	 curChar--;
	 if (charsToEnd > 0)
	   if (!put_chars(wid,&field[curChar],charsToEnd,fieldRow,
			  firstCol+curChar,FALSE,NORMAL)) return FALSE;
	 if (!put_chars(wid," ",1,fieldRow,
			firstCol+curLen,FALSE,NORMAL)) return FALSE;
	 if (!set_cursor_abs(wid,fieldRow,firstCol+curChar)) return FALSE;
       }
       else
	 ring_bell ();
       continue;
     }
     /*************************************************************************
     * Move to start-of-line (SOL) key.
     *************************************************************************/
     if (key == moveToSOLkey) {
       if (curChar > 0) {
	 curChar = 0;
	 if (!set_cursor_abs(wid,fieldRow,firstCol+curChar)) return FALSE;
       }
       else
	 ring_bell ();
       continue;
     }
     /*************************************************************************
     * Move to end-of-line (EOL) key.
     *************************************************************************/
     if (key == moveToEOLkey) {
       if (curLen < fieldLen && curChar < curLen) {
	 curChar = curLen;
	 if (!set_cursor_abs(wid,fieldRow,firstCol+curChar)) return FALSE;
	 continue;
       }
       if (curLen == fieldLen && curChar < curLen - 1) {
	 curChar = curLen - 1;
	 if (!set_cursor_abs(wid,fieldRow,firstCol+curChar)) return FALSE;
	 continue;
       }
       ring_bell ();
       continue;
     }
     /*************************************************************************
     * Delete to start-of-line (SOL) key.
     *************************************************************************/
     if (key == deleteToSOLkey) {
       if (curChar > 0) {
	 int charsToEnd = curLen - curChar;
	 int charsToStart = curChar + 1;
	 int colN, i;
	 memmove (field, &field[curChar], charsToEnd + 1);
	 curLen = charsToEnd;
	 curChar = 0;
	 if (!put_chars(wid,field,curLen,
			fieldRow,firstCol,FALSE,NORMAL)) return FALSE;
	 for (colN = firstCol+curLen, i = 0; i < charsToStart-1; colN++, i++)
	    if (!put_chars(wid," ",1,fieldRow,colN,FALSE,NORMAL)) return FALSE;
	 if (!set_cursor_abs(wid,fieldRow,firstCol+curChar)) return FALSE;
       }
       else
	 ring_bell ();
       continue;
     }
     /*************************************************************************
     * Delete to end-of-line (EOL) key.
     *************************************************************************/
     if (key == deleteToEOLkey) {
       if (curChar < curLen) {
	 int charsToEnd = curLen - curChar;
	 int colN, i;
	 field[curChar] = NUL;
	 curLen = curChar;
	 for (colN = firstCol+curChar, i = 0; i < charsToEnd; colN++, i++)
	    if (!put_chars(wid," ",1,fieldRow,colN,FALSE,NORMAL)) return FALSE;
	 if (!set_cursor_abs(wid,fieldRow,firstCol+curChar)) return FALSE;
       }
       else
	 ring_bell ();
       continue;
     }
     /*************************************************************************
     * Toggle insert mode key.
     *************************************************************************/
     if (key == toggleInsertModeKey) {
       insertMode = (insertMode ? FALSE : TRUE);
       continue;
     }
     /*************************************************************************
     * Refresh key.
     *************************************************************************/
     if (key == refreshKey) {
       repaint_screen ();
       continue;
     }
     /*************************************************************************
     * All the other keys (ignored if not printable).
     *************************************************************************/
     if (Printable(key)) {
       if (insertMode) {
	 if (curLen < fieldLen) {
	   int charsToEnd = curLen - curChar;
	   memmove (&field[curChar+1], &field[curChar], charsToEnd + 1);
	   field[curChar] = key;
	   curLen++;
	   if (!put_chars(wid,&field[curChar],charsToEnd+1,fieldRow,
			  firstCol+curChar,FALSE,NORMAL)) return FALSE;
	   if (curChar < fieldLen - 1) curChar++;
	 }
	 else
	   ring_bell ();
       }
       else {
	 field[curChar] = key;
	 if (curChar == curLen) {
	   curLen++;
	   field[curLen] = NUL;
	 }
	 if (!put_chars(wid,&field[curChar],1,fieldRow,
			firstCol+curChar,FALSE,NORMAL)) return FALSE;
	 if (curChar < fieldLen - 1) curChar++;
       }
       if (!set_cursor_abs(wid,fieldRow,firstCol+curChar)) return FALSE;
     }
     else
       ring_bell ();
  }
}

/******************************************************************************
* inq_cursor_mode.
******************************************************************************/

int inq_cursor_mode (cursorMode)
int *cursorMode;
{
  *cursorMode = (cursorOn ? CURSORon : CURSORoff);
  return TRUE;
}

/******************************************************************************
* label_border.
******************************************************************************/

int label_border(wid, label, rendition)
WINDOWid wid;
char *label;
int rendition;
{
#if defined(SMGui)
  int len = (int) strlen(label);
  static struct dsc$descriptor_s descr = {0,DSC$K_DTYPE_T,DSC$K_CLASS_S,NULL};
  uLongx position = SMG$K_TOP;
  uLongx rend = RenditionMapping(rendition);
  int nRows, nCols;
  long startRow = 1;
  long startCol;
  WindowSize (wid->id, &nRows, &nCols);
  if (!wid->bordered) return FALSE;
  if (len > nCols - 2) return FALSE;
  startCol = (nCols - len) / 2 + 1;
  descr.dsc$w_length = len;
  descr.dsc$a_pointer = label;
  if (StatusBad(smg$put_chars(&(wid->id),&descr,&startRow,&startCol,
			      NULL,&rend,NULL,NULL))) return FALSE;
  return TRUE;
#endif
#if defined(CURSESui)
  int len = (int) strlen (label);
  int nRows, nCols, i, startCol;
  WindowSize (wid->id, &nRows, &nCols);
  if (!wid->bordered) return FALSE;
  if (len > nCols - 2) return FALSE;
  wattrset (wid->id, RenditionMapping(rendition));
  startCol = (nCols - len) / 2;
  for (i = 0; i < len; i++) mvwaddch (wid->id, 0, startCol + i, label[i]);
  if (!RefreshWINDs(UPDATE_)) return FALSE;
  return TRUE;
#endif
#if defined(COWui)
  int len = (int) strlen (label);
  int nCols = wid->id->nCols;
  char rend = RenditionMapping (rendition);
  char *chars = wid->id->chars,
       *attrs = wid->id->attrs;
  int i, startCol;
  if (!wid->bordered) return FALSE;
  if (len > nCols - 2) return FALSE;
  startCol = (nCols - len) / 2;
  for (i = 0; i < len; i++) {
     chars[startCol+i] = label[i];
     attrs[startCol+i] = rend;
  }
  if (!RefreshWINDs(UPDATE_)) return FALSE;
  return TRUE;
#endif
}

/******************************************************************************
* paste_virtual_display.
* It is illegal to paste a virtual display that is already pasted (use
* `repaste_virtual_display').  It is also illegal to paste a virtual display
* that is being batched.
******************************************************************************/

int paste_virtual_display (wid, row, col)
WINDOWid wid;
int row;
int col;
{
#if defined(SMGui)
  long rowN = row + 1;
  long colN = col + 1;
  if (wid->pasted) return FALSE;
  if (StatusBad(smg$paste_virtual_display(&(wid->id),&pbid,
					  &rowN,&colN,NULL))) return FALSE;
  wid->pasted = TRUE;
  if (!MoveWINDtoEnd(wid)) return FALSE;
  return TRUE;
#endif
#if defined(CURSESui)
  if (wid->pasted) return FALSE;
  mvwin (wid->id, row, col);
  wid->pasted = TRUE;
  if (!MoveWINDtoEnd(wid)) return FALSE;
  if (!RefreshWINDs(UPDATE_)) return FALSE;
  return TRUE;
#endif
#if defined(COWui)
  if (wid->pasted) return FALSE;
  wid->id->atRowN = row;
  wid->id->atColN = col;
  wid->pasted = TRUE;
  if (!MoveWINDtoEnd(wid)) return FALSE;
  if (!RefreshWINDs(UPDATE_)) return FALSE;
  return TRUE;
#endif
}

/******************************************************************************
* put_chars.
******************************************************************************/

int put_chars (wid, string, len, row, col, eraseMode, rendition)
WINDOWid wid;
char *string;
int len;
int row;
int col;
int eraseMode;
int rendition;
{
#if defined(SMGui)
  static struct dsc$descriptor_s descr = {0,DSC$K_DTYPE_T,DSC$K_CLASS_S,NULL};
  uLongx rend = RenditionMapping (rendition);
  long rowN = row + 1;
  long colN = col + 1;
  if (eraseMode == ERASE) {
    int nRows, nCols;
    long nChars, startCol;
    WindowSize (wid->id, &nRows, &nCols);
    nChars = (wid->bordered ? nCols - 2 : nCols);
    startCol = (wid->bordered ? 2 : 1);
    if (StatusBad(smg$erase_chars(&(wid->id),&nChars,
				  &rowN,&startCol))) return FALSE;
  }
  descr.dsc$w_length = len;
  descr.dsc$a_pointer = string;
  if (StatusBad(smg$put_chars(&(wid->id),&descr,&rowN,
			      &colN,NULL,&rend,NULL,NULL))) return FALSE;
  return TRUE;
#endif
#if defined(CURSESui)
  int nRows, nCols, colN, i;
  WindowSize (wid->id, &nRows, &nCols);
  if (eraseMode == ERASE) {
    wattrset (wid->id, RenditionMapping(NORMAL));
    for (colN = 1; colN <= nCols - 2; colN++) mvwaddch (wid->id,row,colN,' ');
  }
  wattrset (wid->id, RenditionMapping(rendition));
  for (i = 0; i < len; i++) mvwaddch (wid->id, row, col + i, string[i]);
  if (!RefreshWINDs(UPDATE_)) return FALSE;
  return TRUE;
#endif
#if defined(COWui)
  short nCols = wid->id->nCols;
  char rend;
  char *chars = wid->id->chars;
  char *attrs = wid->id->attrs;
  int colN, i, charN;
  if (eraseMode == ERASE) {
    rend = RenditionMapping (NORMAL);
    for (colN = 1; colN <= nCols - 2; colN++) {
       charN = (row * nCols) + colN;
       chars[charN] = ' ';
       attrs[charN] = rend;
    }
  }
  rend = RenditionMapping (rendition);
  for (i = 0; i < len; i++) {
     charN = (row * nCols) + (col + i);
     chars[charN] = string[i];
     attrs[charN] = rend;
  }
  if (!RefreshWINDs(UPDATE_)) return FALSE;
  return TRUE;
#endif
}

/******************************************************************************
* read_display.
* Use only by CDFlist and CDFwalk (ie. expendable).
******************************************************************************/

int read_display (wid, row, string)
WINDOWid wid;
int row;
char *string;
{
#if defined(SMGui)
  static struct dsc$descriptor_s descr = {0,DSC$K_DTYPE_T,DSC$K_CLASS_S,NULL};
  int nRows, nCols; long rowN = row + 1; char *stringT;
  WindowSize (wid->id, &nRows, &nCols);
  stringT = (char *) cdf_AllocateMemory ((size_t)nCols, FatalError);
  descr.dsc$w_length = nCols;
  descr.dsc$a_pointer = stringT;
  if (StatusBad(smg$read_from_display(&(wid->id),&descr,NULL,&rowN))) {
    cdf_FreeMemory (stringT, FatalError);
    return FALSE;
  }
  if (wid->bordered) {
    memmove (string, &stringT[1], nCols-2);
    string[nCols-2] = NUL;
  }
  else {
    memmove (string, stringT, nCols);
    string[nCols] = NUL;
  }
  cdf_FreeMemory (stringT, FatalError);
  return TRUE;
#endif
#if defined(CURSESui)
  int colN, charN, nRows, nCols, nChars, startCol;
  WindowSize (wid->id, &nRows, &nCols);
  nChars = (wid->bordered ? nCols - 2 : nCols);
  startCol = (wid->bordered ? 1 : 0);
  for (colN = startCol, charN = 0; charN < nChars; colN++, charN++)
     string[charN] = (char) mvwinch (wid->id, row, colN);
  string[nChars] = NUL;
  return TRUE;
#endif
#if defined(COWui)
  int charN, i;
  int nCols = wid->id->nCols;
  int nChars = (wid->bordered ? nCols - 2 : nCols);
  int startCol = (wid->bordered ? 1 : 0);
  char *chars = wid->id->chars;
  for (i = 0; i < nChars; i++) {
     charN = (row * nCols) + (startCol + i);
     string[i] = chars[charN];
  }
  string[nChars] = NUL;
  return TRUE;
#endif
}

/******************************************************************************
* read_input.
*
* TRUE is returned if a key was read.  FALSE is returned if blocking is
* disabled and a key is not available or if an error occurred.
*
* The `wid' parameter is only used on CURSES-based systems.  This is because
* `getch' on POSIXshell systems (VAX and DEC Alpha) would move the cursor to
* `0,0' (which isn't where it should be).  `wgetch' is used instead because
* the cursor will be positioned in the proper place in the window.  If
* `wgetch' (on POSIXshell systems) moves the cursor at least it will move to
* the right place (which is probably where it already is) and not to `0,0'.
******************************************************************************/

int read_input (
#if defined(CURSESui)
		wid,
#endif
		     tcode, mode, block)
#if defined(CURSESui)
WINDOWid wid;
#endif
int *tcode;		/* Out: key entered. */
int mode;		/* Case conversion mode. */
Logical block;		/* Block until key available? */
{
#if defined(SMGui)
  /****************************************************************************
  * SMG.
  ****************************************************************************/
  uShort terminator_code;
  if (!block) {
    if (keyWaiting == 0) return FALSE;
  }
  if (StatusBad(smg$read_keystroke(&kbid,&terminator_code,
				   NULL,NULL,NULL,NULL,NULL))) return FALSE;
  *tcode = terminator_code;
  if (keyWaiting > 0) keyWaiting--;
  if (ABORTkey(*tcode)) {
    set_cursor_mode (CURSORon);
    delete_pasteboard (ERASE);
    cdf_FreeMemory (NULL, FatalError);
    Exit;
  }
  switch (mode) {
    case PASSTHRUri: break;
    case TOUPPERri: *tcode = MakeUpper (*tcode); break;
    case TOLOWERri: *tcode = MakeLower (*tcode); break;
  }
  return TRUE;
#endif
  /****************************************************************************
  * Curses.
  ****************************************************************************/
#if defined(CURSESui)
  if (!block) {
#if defined(unix) && !defined(__MINGW32__)
    fd_set rset;
    static struct timeval tv = { 0L, 0L };
    FD_ZERO (&rset);
    FD_SET (STDIN_FILENO, &rset);
    if (select(STDIN_FILENO+1,
#if defined(hpux)
			      (int *)	/* HP-UX has this as an `int *'. */
#endif
				     &rset,NULL,NULL,&tv) == 0) return FALSE;
#endif
#if defined(dos) || defined(__MINGW32__)
    if (!kbhit()) return FALSE;
#endif
  }
  *tcode = wgetch (wid->id);
  if (ABORTkey(*tcode)) {
    set_cursor_mode (CURSORon);
    delete_pasteboard (ERASE);
    cdf_FreeMemory (NULL, FatalError);
    Exit;
  }
  switch (*tcode) {
#if defined(sgi)
    case SGI_CONSOLE_RETURN: *tcode = KB_RETURN; break;
#endif
#if defined(posixSHELL)
    case POSIX_SHELL_DELETE: *tcode = KB_DELETE; break;
#endif
#if defined(hpux)
    case HPUX_DELETE: *tcode = KB_DELETE; break;
#endif
#if defined(AIX)
    case AIX_RETURN: *tcode = KB_RETURN; break;
    case AIX_DELETE: *tcode = KB_DELETE; break;
#endif
#if defined(__CYGWIN__)
    case CYGWIN_DELETE: *tcode = KB_DELETE; break;
#endif
#if defined(linux)
    case LINUX_DELETE: *tcode = KB_DELETE; break;
#endif
#if defined(__MINGW32__)
    case MINGW32_DELETE: *tcode = KB_DELETE; break;
#endif
    default: break;
  }
  switch (mode) {
    case PASSTHRUri: break;
    case TOUPPERri: *tcode = MakeUpper (*tcode); break;
    case TOLOWERri: *tcode = MakeLower (*tcode); break;
  }
#if INTERPRET_ESC_SEQUENCES
  if (*tcode == ESC) {
    int key1 = wgetch (wid->id);
    int key2 = wgetch (wid->id);
    switch (key1) {
      case 'O':
	switch (key2) {
	  case 'A': *tcode = KEY_UP; break;
	  case 'B': *tcode = KEY_DOWN; break;
	  case 'C': *tcode = KEY_RIGHT; break;
	  case 'D': *tcode = KEY_LEFT; break;
	  default: break;
	}
	break;
      default:
	break;
    }
  }
#endif
  return TRUE;
#endif
  /****************************************************************************
  * COW.
  ****************************************************************************/
#if defined(COWui)
#if defined(win32)
  MSG msg;
  for (;;) {
	  if (!PeekMessage(&msg,NULL,0,0,PM_REMOVE)) {
		  if (block) continue;
		  return FALSE;
	  }
	  if (msg.message == WM_KEYDOWN) {
	    int scanCode = (msg.lParam & 0x00FF0000) >> 16;
	    switch (scanCode) {
	      case 0x4B:
		*tcode = KB_LEFTARROW;
		return TRUE;
	      case 0x4D:
		*tcode = KB_RIGHTARROW;
		return TRUE;
	      case 0x48:
		*tcode = KB_UPARROW;
		return TRUE;
	      case 0x50:
		*tcode = KB_DOWNARROW;
		return TRUE;
	    }
	  }
	  if (msg.message == WM_CHAR) {
		  *tcode = (int) msg.wParam;
		 switch (mode) {
		   case PASSTHRUri: break;
		   case TOUPPERri: *tcode = MakeUpper (*tcode); break;
		   case TOLOWERri: *tcode = MakeLower (*tcode); break;
		 }
		  return TRUE;
	  }
	  if (!TranslateMessage(&msg)) DispatchMessage (&msg);
  }
#endif
#if defined(mac)
  EventRecord event;
  for (;;) {
     SystemTask ();
     GetNextEvent(everyEvent,&event);
     switch (event.what) {
       /*********************************************************************
       * Null event.
       *********************************************************************/
       case nullEvent: {
	 WindowPtr whichWindow;
	 switch (FindWindow(event.where,&whichWindow)) {
	   case inContent:
	     SetCursor (CDF_CURSOR);
	     break;
	   default:
             SetCursor (ARROW_CURSOR);
             break;
         }
         break;
       }
       /*********************************************************************
       * Mouse down event.
       *********************************************************************/
       case mouseDown: {
	 WindowPtr whichWindow;
	 switch (FindWindow(event.where,&whichWindow)) {
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
	             GetItem (appleMenuHfsi, itemN, name);
	             OpenDeskAcc (name);
	             SetPort (fsiWindowP);
	             break;
	           }
	         }
	         break;
	     }
	     HiliteMenu (0);
	     break;
	   }
	   case inDrag: {
             RectPtr screen = &qd.screenBits.bounds; Rect dRect;
             dRect.top = screen->top + 40;
             dRect.left = screen->left + 40;
             dRect.bottom = screen->bottom - 40;
             dRect.right = screen->right - 40;
             DragWindow (whichWindow, event.where, &dRect);
             break;
	   }
	   case inSysWindow:
	     SystemClick (&event, whichWindow);
	     break;
	 }
	 break;
       }
       /*********************************************************************
       * Key down event.
       *********************************************************************/
       case keyDown:
       case autoKey: {
	 *tcode = (int) (event.message & charCodeMask);
	 if (ABORTkeyMAC(event)) {
	   delete_pasteboard (ERASE);
	   cdf_FreeMemory (NULL, FatalError);
	   Exit;
	 }
	 /*******************************************************************
	 * If the command key was held down, convert the character to the
	 * corresponding control character.
	 *******************************************************************/
	 if ((event.modifiers & cmdKey) != 0 &&
	     'a' <= *tcode && *tcode <= 'z') *tcode = *tcode & 0x1F;
	 /*******************************************************************
	 * Convert to uppercase/lowercase if requested.
	 *******************************************************************/
	 switch (mode) {
	   case PASSTHRUri: break;
	   case TOUPPERri: *tcode = MakeUpper (*tcode); break;
	   case TOLOWERri: *tcode = MakeLower (*tcode); break;
	 }
	 return TRUE;
       }
       /*********************************************************************
       * Active event.
       *********************************************************************/
       case activateEvt:
         if ((event.modifiers & activeFlag) != 0) {	/* Activate. */
           SetCursor (CDF_CURSOR);
         }
         else {						/* Deactivate. */
	   /* Nothing to be done. */
         }
         break;
       /*********************************************************************
       * Update event.
       *********************************************************************/
       case updateEvt:
	 BeginUpdate ((WindowPtr) event.message);
	 if (!RefreshWINDs(SOFT_)) return FALSE;
	 EndUpdate ((WindowPtr) event.message);
         break;
     }
     if (!block) return FALSE;
  }
#endif
#endif
}

/******************************************************************************
* repaint_screen.
******************************************************************************/

int repaint_screen () {
#if defined(SMGui)
  if (StatusBad(smg$repaint_screen(&pbid))) return FALSE;
  return TRUE;
#endif
#if defined(CURSESui)
#if defined(posixSHELL)
  wrefresh (stdscr);
#else
  wrefresh (curscr);
#endif
  return TRUE;
#endif
#if defined(COWui)
  if (!RefreshWINDs(HARD_)) return FALSE;
  return TRUE;
#endif
}

/******************************************************************************
* repaste_virtual_display.
******************************************************************************/

int repaste_virtual_display (wid, row, col)
WINDOWid wid;
int row;
int col;
{
#if defined(SMGui)
  long rowN = row + 1;
  long colN = col + 1;
  if (StatusBad(smg$repaste_virtual_display(&(wid->id),&pbid,
					    &rowN,&colN,NULL))) return FALSE;
  if (!MoveWINDtoEnd(wid)) return FALSE;
  return TRUE;
#endif
#if defined(CURSESui)
  if (!wid->pasted) return FALSE;
  mvwin (wid->id, row, col);
  if (!MoveWINDtoEnd(wid)) return FALSE;
  if (!RefreshWINDs(UPDATE_)) return FALSE;
  return TRUE;
#endif
#if defined(COWui)
  if (!wid->pasted) return FALSE;
  wid->id->atRowN = row;
  wid->id->atColN = col;
  if (!MoveWINDtoEnd(wid)) return FALSE;
  if (!RefreshWINDs(UPDATE_)) return FALSE;
  return TRUE;
#endif
}

/******************************************************************************
* ring_bell.
******************************************************************************/

int ring_bell () {
#if defined(SMGui)
  long nRows = 1;
  long nCols = 1;
  long rowN = 1;
  long colN = 1;
  long cursorRow, cursorCol;
  LocalId id, cursorId;
  if (StatusBad(smg$find_cursor_display(&pbid,&cursorId,
					NULL,NULL))) return FALSE;
  if (cursorId != 0) cursorRow = smg$cursor_row (&cursorId);
  if (cursorId != 0) cursorCol = smg$cursor_column (&cursorId);
  if (StatusBad(smg$begin_pasteboard_update(&pbid))) return FALSE;
  if (StatusBad(smg$create_virtual_display(&nRows,&nCols,&id,
					   NULL,NULL,NULL))) return FALSE;
  if (StatusBad(smg$paste_virtual_display(&id,&pbid,
					  &rowN,&colN,NULL))) return FALSE;
  if (StatusBad(smg$ring_bell(&id))) return FALSE;
  if (StatusBad(smg$delete_virtual_display(&id))) return FALSE;
  if (StatusBad(smg$end_pasteboard_update(&pbid))) return FALSE;
  if (cursorId != 0)
    if (StatusBad(smg$set_cursor_abs(&cursorId,
				     &cursorRow,
				     &cursorCol))) return FALSE;
  return TRUE;
#endif
#if defined(CURSESui)
  beep ();
  return TRUE;
#endif
#if defined(COWui)
#if defined(win32)
   Beep ((DWORD) 800, (DWORD) 150);
#endif
#if defined(mac)
  short ticks = 1;
  SysBeep (ticks);
#endif
  return TRUE;
#endif
}

/******************************************************************************
* set_cursor_abs.
*
* CURSES:
* After setting the cursor position in the specified window, only refresh that
* window.  Refreshing all windows (using `RefreshWINDs') will put the physical
* screen cursor where the `top' window's virtual cursor is.
******************************************************************************/

int set_cursor_abs (wid, row, col)
WINDOWid wid;
int row;
int col;
{
#if defined(SMGui)
  long rowN = row + 1;
  long colN = col + 1;
  if (StatusBad(smg$set_cursor_abs(&(wid->id),&rowN,&colN))) return FALSE;
  return TRUE;
#endif
#if defined(CURSESui)
  wmove (wid->id, row, col);
  wrefresh (wid->id);
  return TRUE;
#endif
#if defined(COWui)
  /****************************************************************************
  * Undisplay current cursor (if it exists and is visible).
  ****************************************************************************/
  if (cursorOn && cursorWIND != NULL && cursorWIND->pasted &&
      !CharOccluded(cursorWIND,cursorRow,cursorCol)) {
#if defined(mac)
    InvertCursor (cursorWIND, cursorRow, cursorCol);
#endif
#if defined(win32)
    SetCursorPosition (-1, -1);
#endif
  }
  /****************************************************************************
  * Set/display new cursor (if it is visible).
  ****************************************************************************/
  cursorWIND = wid;
  cursorRow = row;
  cursorCol = col;
  if (cursorOn && cursorWIND != NULL && cursorWIND->pasted &&
      !CharOccluded(cursorWIND,cursorRow,cursorCol)) {
#if defined(mac)
    InvertCursor (cursorWIND, cursorRow, cursorCol);
#endif
#if defined(win32)
    SetCursorPosition (cursorWIND->id->atRowN + cursorRow,
		       cursorWIND->id->atColN + cursorCol);
#endif
  }
  return TRUE;
#endif
}

/******************************************************************************
* set_cursor_mode.
******************************************************************************/

int set_cursor_mode (cursorMode)
int cursorMode;                         /* CURSORon/CURSORoff */
{
#if defined(SMGui)
  uLongx mode = (cursorMode == CURSORon ? SMG$M_CURSOR_ON :
						 SMG$M_CURSOR_OFF);
  if (StatusBad(smg$set_cursor_mode(&pbid,&mode))) return FALSE;
  cursorOn = (cursorMode == CURSORon);
  return TRUE;
#endif
#if defined(CURSESui)
#if defined(unix) || defined(posixSHELL)
#if CURS_SETavail
  int visibility = (cursorMode == CURSORon ? 1 : 0);
  curs_set (visibility);
#endif
  cursorOn = (cursorMode == CURSORon);
  return TRUE;
#endif
#if defined(dos)
  if (cursorMode == CURSORon)
    curson ();
  else
    cursoff ();
  cursorOn = (cursorMode == CURSORon);
  return TRUE;
#endif
#endif
#if defined(COWui)
  /****************************************************************************
  * If the new mode is different than the old mode, set the new mode and
  * invert the current cursor (if one exists and is visible).
  ****************************************************************************/
  if (cursorMode != BOO(cursorOn,CURSORon,CURSORoff)) {
    cursorOn = (cursorMode == CURSORon);
    if (cursorWIND != NULL && cursorWIND->pasted &&
        !CharOccluded(cursorWIND,cursorRow,cursorCol)) {
#if defined(mac)
      InvertCursor (cursorWIND, cursorRow, cursorCol);
#endif
#if defined(win32)
      SetCursorPosition (BOO(cursorOn,cursorWIND->id->atRowN + cursorRow,-1),
			 BOO(cursorOn,cursorWIND->id->atColN + cursorCol,-1));
#endif
    }
  }
  return TRUE;
#endif
}

/******************************************************************************
* unpaste_virtual_display.
******************************************************************************/

int unpaste_virtual_display (wid)
WINDOWid wid;
{
#if defined(SMGui)
   if (StatusBad(smg$unpaste_virtual_display(&(wid->id),&pbid))) return FALSE;
   wid->pasted = FALSE;
   return TRUE;
#else
#  if defined(CURSESui)
     LocalId tempId;
     int nRows, nCols, row, col;
     WindowSize (wid->id, &nRows, &nCols);
     WindowLocation (wid->id, &row, &col);
     tempId = newwin (nRows, nCols, row, col);
     if (tempId == NULL) return FALSE;
     wattrset (tempId, RenditionMapping(BLACK));
     EraseWindow (tempId);
     wnoutrefresh (tempId);
     delwin (tempId);
     wid->pasted = FALSE;
     if (!RefreshWINDs(UPDATE_)) return FALSE;
     return TRUE;
#  else
#    if defined(COWui)
       wid->pasted = FALSE;
       if (!RefreshWINDs(UPDATE_)) return FALSE;
       return TRUE;
#    else
       return TRUE;
#    endif
#  endif
#endif
}

/******************************************************************************
* zzzzz.
* Sleep, get it?
******************************************************************************/

int zzzzz (seconds)
double seconds;
{
#if defined(SMGui)					/* ie. VMS */
#if defined(alphavmsI)
  time_t start_time, current_time;
  time (&start_time);
  time (&current_time);
  while ((current_time - start_time) < (seconds + 1.0)) time (&current_time);
#else
  float seconds4 = seconds;
  if (StatusBad(lib$wait(&seconds4))) return FALSE;
#endif
  return TRUE;
#endif
#if defined(CURSESui)
#if defined(unix)
  int ms = (int) seconds;
#if !defined(__QNX__)		/* Due to a `ncurses' bug under QNX. */
  ms *= 1000;
#endif
  napms (ms);
  return TRUE;
#endif
#if defined(MICROSOFTC) || defined(posixSHELL)
  time_t start_time, current_time;
  time (&start_time);
  time (&current_time);
  while ((current_time - start_time) < (seconds + 1.0)) time (&current_time);
  return TRUE;
#endif
#if defined(BORLANDC)
  sleep ((uInt) seconds + 0.5);
  return TRUE;
#endif
#endif
#if defined(COWui)
#if defined(win32)
  MSG msg;
  UINT timerId = SetTimer (NULL, 0, (UINT) (1000 * seconds), NULL);
  for (;;) {
	  GetMessage (&msg, NULL, 0, 0);
	  if (msg.message == WM_TIMER) {
		  KillTimer (NULL, timerId);
		  break;
	  }
	  DispatchMessage (&msg);
  }
#endif
#if defined(mac)
  EventRecord event; long untilTick; short nullEventMask = 0;
  EventAvail (nullEventMask, &event);
  untilTick = event.when + (long) (seconds * 60);
  for (;;) {
     EventAvail (nullEventMask, &event);
     if (event.when > untilTick) break;
  }
#endif
  return TRUE;
#endif
}

/******************************************************************************
* AddWIND.  Add a WIND to the linked list (at the end).
******************************************************************************/

static WINDOWid AddWIND (id, pasted, bordered)
LocalId id;
Logical pasted;
Logical bordered;
{
  WINDOWid newWIND, tW;
  newWIND = (WINDOWid) cdf_AllocateMemory ((size_t)sizeof(WIND), FatalError);
  if (WINDhead == NULL)
    WINDhead = newWIND;
  else {
    tW = WINDhead;
    while (tW->next != NULL) tW = tW->next;
    tW->next = newWIND;
  }
  newWIND->id = id;
  newWIND->pasted = pasted;
  newWIND->bordered = bordered;
  newWIND->next = NULL;
  return newWIND;
}

/******************************************************************************
* MoveWINDtoEnd.
* Move a WIND to the end of the linked list.  The case where WIND is already
* at the end of the linked list is handled (but not as a special case).
******************************************************************************/

static Logical MoveWINDtoEnd (wid)
WINDOWid wid;
{
  WINDOWid tW = WINDhead;
  WINDOWid prevW = NULL;
  while (tW != NULL) {
    if (tW == wid) {
      if (prevW == NULL)
	WINDhead = tW->next;
      else
	prevW->next = tW->next;
      if (WINDhead == NULL)
	WINDhead = tW;
      else {
	WINDOWid sW = WINDhead;
	while (sW->next != NULL) sW = sW->next;
	sW->next = tW;
	tW->next = NULL;
      }
      return TRUE;
    }
    prevW = tW;
    tW = tW->next;
  }
  return FALSE;			/* Wasn't found. */
}

/******************************************************************************
* DeleteWIND.  Delete a WIND from the linked list.
******************************************************************************/

static Logical DeleteWIND (wid)
WINDOWid wid;
{
  WINDOWid tW = WINDhead;
  WINDOWid prevW = NULL;
  while (tW != NULL) {
    if (tW == wid) {
#if defined(SMGui)
      if (StatusBad(smg$delete_virtual_display(&(tW->id)))) return FALSE;
#endif
#if defined(CURSESui)
      if (tW->pasted) {
	wattrset (tW->id, RenditionMapping(BLACK));
	EraseWindow (tW->id);
	wnoutrefresh (tW->id);
      }
      delwin (tW->id);
#endif
#if defined(COWui)
      if (tW == cursorWIND) cursorWIND = NULL;
#endif
      if (prevW == NULL)
	WINDhead = tW->next;
      else
	prevW->next = tW->next;
#if defined(COWui)
      cdf_FreeMemory (tW->id->chars, FatalError);
      cdf_FreeMemory (tW->id->attrs, FatalError);
      cdf_FreeMemory (tW->id, FatalError);
#endif
      cdf_FreeMemory (tW, FatalError);
#if defined(CURSESui) || defined(COWui)
      if (!RefreshWINDs(UPDATE_)) return FALSE;
#endif
      return TRUE;
    }
    prevW = tW;
    tW = tW->next;
  }
  return FALSE;			/* Wasn't found. */
}

/******************************************************************************
* DeleteWINDs.  Delete all of the WINDs.
******************************************************************************/

static Logical DeleteWINDs () {
  while (WINDhead != NULL) {
    if (!DeleteWIND(WINDhead)) return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* RefreshWINDs.
* Refresh all windows (that are pasted).  Note that this has no affect if
* updates are being batched (with begin/end_pasteboard_update).
******************************************************************************/

#if defined(CURSESui)
static Logical RefreshWINDs (level)
int level;
{
/*  if (batchCount < 1) { */
    WINDOWid tW;
    switch (level) {
      case HARD_:
      case SOFT_:
      case UPDATE_:
	/* Do nothing if CURSES. */
	break;
    }
    tW = WINDhead;
    while (tW != NULL) {
      if (tW->pasted) {
	touchwin (tW->id);
	wnoutrefresh (tW->id);
      }
      tW = tW->next;
    }
    doupdate ();
/*  } */
  return TRUE;
}
#endif

#if defined(COWui)
static Logical RefreshWINDs (level)
int level;
{
  WINDOWid tW;
  char *savePtr;
  int nChars = NUMfsiROWS * NUMfsiCOLS, rowN, windCharN, pbCharN;
  /****************************************************************************
  * If updates are being batched, do nothing.
  ****************************************************************************/
  if (batchCount > 0) return TRUE;
  /****************************************************************************
  * Check level of refresh.
  *  Hard:   Erases window and force all non-blank/non-normal character
  *	     positions to be redrawn.  This will causes the characters in
  *	     the window to "flash" (off then on).  In this case the cursor
  *          does not need to be turned off.
  *  Soft:   Force all character positions to be redrawn.  The characters
  *          should not "flash".  In this case the cursor does not need to
  *          be turned off (it will be overwritten by a character).
  *  Update: In this case the cursor does need to be turned off if it is
  *          actually visible (not if it should be visible).  This is
  *	     because the operations before this refresh could have occluded
  *	     the cursor.
  ****************************************************************************/
  switch (level) {
    case HARD_: {
      int nChars = NUMfsiROWS * NUMfsiCOLS;
#if defined(win32)
      /* Jeff...maybe nothing needs to be done. */
#endif
#if defined(mac)
      Rect eraseRect = { 0, 0, WINDOWfsiHEIGHT, WINDOWfsiWIDTH };
      EraseRect (&eraseRect);
#endif
      memmove (pbCurrentChars, pbNullChars, nChars);
      memmove (pbCurrentAttrs, pbNormalAttrs, nChars);
      break;
    }
    case SOFT_: {
      int nChars = NUMfsiROWS * NUMfsiCOLS;
      memmove (pbCurrentChars, pbNullChars, nChars);
      memmove (pbCurrentAttrs, pbNormalAttrs, nChars);
      break;
    }
    case UPDATE_:
#if defined(mac)
      if (cursorVisible) InvertCursor (cursorWIND, cursorRow, cursorCol);
#endif
#if defined(win32)
      SetCursorPosition (-1, -1);
#endif
      break;
  }
  /****************************************************************************
  * Reset new characters/attributes...
  ****************************************************************************/
  memmove (pbNewChars, pbBlankChars, nChars);
  memmove (pbNewAttrs, pbNormalAttrs, nChars);
  /****************************************************************************
  * ...and then generate what the window should look like.
  ****************************************************************************/
  tW = WINDhead;
  while (tW != NULL) {
    if (tW->pasted) {
      for (rowN = 0; rowN < tW->id->nRows; rowN++) {
	 windCharN = rowN * tW->id->nCols;
	 pbCharN = ((tW->id->atRowN + rowN) * NUMfsiCOLS) + tW->id->atColN;
	 memmove (&pbNewChars[pbCharN], &(tW->id->chars[windCharN]),
		  tW->id->nCols);
	 memmove (&pbNewAttrs[pbCharN], &(tW->id->attrs[windCharN]),
		  tW->id->nCols);
      }
    }
    tW = tW->next;
  }
  /****************************************************************************
  * Redraw the characters/attributes which have changed.
  ****************************************************************************/
#if defined(win32)
  TransferTextAttrs (pbNewChars, pbNewAttrs);
#endif
#if defined(mac)
  for (rowN = 0; rowN < NUMfsiROWS; rowN++) {
     rowCharN = rowN * NUMfsiCOLS;
     if (memcmp(&pbNewChars[rowCharN],
		&pbCurrentChars[rowCharN],NUMfsiCOLS) != 0 ||
         memcmp(&pbNewAttrs[rowCharN],
		&pbCurrentAttrs[rowCharN],NUMfsiCOLS) != 0) {
       startN = rowCharN;
       while (pbNewChars[startN] == pbCurrentChars[startN] &&
	      pbNewAttrs[startN] == pbCurrentAttrs[startN]) startN++;
       endN = rowCharN + NUMfsiCOLS - 1;
       while (pbNewChars[startN] == pbCurrentChars[startN] &&
	      pbNewAttrs[startN] == pbCurrentAttrs[startN]) endN--;
       for (;;) {
	  upToN = startN;
	  thisAttr = pbNewAttrs[startN];
	  while (upToN+1 <= endN && pbNewAttrs[upToN+1] == thisAttr &&
		 !LINEdrawingCHAR(pbNewChars[upToN]) &&
	         !LINEdrawingCHAR(pbNewChars[upToN+1])) upToN++;
	  if (BITSET(thisAttr,REVERSEbit))
	    TextMode (notSrcCopy);
	  else
	    TextMode (srcCopy);
	  if (BITSET(thisAttr,BOLDbit))
	    TextFace (bold);
	  else
	    TextFace (0);
	  /* BLINKINGbit is not yet supported. */
	  startX = (FONTfsiWIDTH * (startN % NUMfsiCOLS)) + MARGINfsiSIZE;
	  startY = (FONTfsiHEIGHT * rowN) + MARGINfsiSIZE;
	  MoveTo (startX, startY);
	  if (LINEdrawingCHAR(pbNewChars[startN])) {
	    /******************************************************************
	    * A line drawing character.  First erase the entire character
	    * cell (including the leading line).
	    ******************************************************************/
	    Rect eraseRect;
	    eraseRect.top = startY;
	    eraseRect.bottom = startY + FONTfsiHEIGHT;
	    eraseRect.left = startX;
	    eraseRect.right = startX + FONTfsiWIDTH;
	    EraseRect (&eraseRect);
	    switch (pbNewChars[startN]) {
	      case ACS_PLUS:
	        Move (FONTfsiHALFwidth, 0);
	        Line (0, FONTfsiHEIGHT-1);
	        Move (-FONTfsiHALFwidth, -(FONTfsiHALFheight-1));
	        Line (FONTfsiWIDTH-1, 0);
	        break;
	      case ACS_TTEE:
	        Move (0, FONTfsiHALFheight);
	        Line (FONTfsiWIDTH-1, 0);
	        Move (-FONTfsiHALFwidth, 0);
	        Line (0, FONTfsiHALFheight-1);
	        break;
	      case ACS_BTEE:
		Move (0, FONTfsiHALFheight);
		Line (FONTfsiWIDTH-1, 0);
		Move (-FONTfsiHALFwidth, 0);
		Line (0, -FONTfsiHALFheight);
	        break;
	      case ACS_LTEE:
		Move (FONTfsiHALFwidth, 0);
		Line (0, FONTfsiHEIGHT-1);
		Move (0, -(FONTfsiHALFheight-1));
		Line (FONTfsiHALFwidth, 0);
	        break;
	      case ACS_RTEE:
		Move (FONTfsiHALFwidth, 0);
		Line (0, FONTfsiHEIGHT-1);
		Move (0, -(FONTfsiHALFheight-1));
		Line (-FONTfsiHALFwidth, 0);
	        break;
	      case ACS_HLINE:
		Move (0, FONTfsiHALFheight);
		Line (FONTfsiWIDTH-1, 0);
	        break;
	      case ACS_VLINE:
		Move (FONTfsiHALFwidth, 0);
		Line (0, FONTfsiHEIGHT-1);
	        break;
	      case ACS_ULCORNER:
		Move (FONTfsiHALFwidth, FONTfsiHEIGHT-1);
		Line (0, -(FONTfsiHALFheight-1));
		Line (FONTfsiHALFwidth, 0);
	        break;
	      case ACS_URCORNER:
		Move (0, FONTfsiHALFheight);
		Line (FONTfsiHALFwidth, 0);
		Line (0, FONTfsiHALFheight-1);
	        break;
	      case ACS_LRCORNER:
		Move (FONTfsiHALFwidth, 0);
		Line (0, FONTfsiHALFheight);
		Line (-FONTfsiHALFwidth, 0);
	        break;
	      case ACS_LLCORNER:
		Move (FONTfsiHALFwidth, 0);
		Line (0, FONTfsiHALFheight);
		Line (FONTfsiHALFwidth, 0);
	        break;
	    }
	  }
	  else {
	    /******************************************************************
	    * Drawing text characters.  Since one or more line drawing
	    * characters (or the cursor) may be written over, erase the
	    * leading line first (which is used by the line drawing characters
	    * but not the text characters).
	    ******************************************************************/
	    Rect eRect;
	    short nChars = upToN - startN + 1;
	    eRect.top = startY + FONTfsiHEIGHT - 1;
	    eRect.bottom = startY + FONTfsiHEIGHT;
	    eRect.left = startX;
	    eRect.right = startX + (nChars * FONTfsiWIDTH);
	    EraseRect (&eRect);
	    Move (0, FONTfsiASCENT);
	    DrawText (&pbNewChars[startN], 0, nChars);
	  }
	  if (upToN == endN) break;
	  startN = upToN + 1;
       }
     }
  }
#endif
  /****************************************************************************
  * Make the new characters/attributes the current characters/attributes.
  ****************************************************************************/
  savePtr = pbCurrentChars;
  pbCurrentChars = pbNewChars;
  pbNewChars = savePtr;
  savePtr = pbCurrentAttrs;
  pbCurrentAttrs = pbNewAttrs;
  pbNewAttrs = savePtr;
  /****************************************************************************
  * Redisplay cursor (if appropriate).
  ****************************************************************************/
  if (cursorOn && cursorWIND != NULL && cursorWIND->pasted &&
      !CharOccluded(cursorWIND,cursorRow,cursorCol)) {
#if defined(mac)
    InvertCursor (cursorWIND, cursorRow, cursorCol);
#endif
#if defined(win32)
    SetCursorPosition (cursorWIND->id->atRowN + cursorRow,
		       cursorWIND->id->atColN + cursorCol);
#endif
  }
  return TRUE;
}
#endif

/******************************************************************************
* CharOccluded.
******************************************************************************/

#if defined(COWui)
static Logical CharOccluded (wid, rowN, colN)
WINDOWid wid;
int rowN;
int colN;
{
  WINDOWid tW = wid->next;
  int pbRowN = wid->id->atRowN + rowN,
      pbColN = wid->id->atColN + colN;
  while (tW != NULL) {
    if (tW->pasted &&
	tW->id->atRowN <= pbRowN &&
        pbRowN <= (tW->id->atRowN + tW->id->nRows - 1) &&
        tW->id->atColN <= pbColN &&
        pbColN <= (tW->id->atColN + tW->id->nCols - 1)) return TRUE;
    tW = tW->next;
  }
  return FALSE;
}
#endif

/******************************************************************************
* InvertCursor.
* This is used to turn a cursor on or off.
******************************************************************************/

#if defined(COWui)
#if defined(mac)
static void InvertCursor (wid, rowN, colN)
WINDOWid wid;
int rowN;
int colN;
{
  Rect iRect;
  int startX = (FONTfsiWIDTH * (wid->id->atColN + colN)) + MARGINfsiSIZE,
      startY = (FONTfsiHEIGHT * (wid->id->atRowN + rowN)) + MARGINfsiSIZE;
  iRect.top = startY;
  iRect.bottom = startY + FONTfsiHEIGHT - 1;	/* -1 for leading line. */
  iRect.left = startX;
  iRect.right = startX + FONTfsiWIDTH;
  InvertRect (&iRect);
  cursorVisible = BOO(cursorVisible,FALSE,TRUE);
  return;
}
#endif
#endif

/******************************************************************************
* WindowLocation.
* Passes back row/column at which window is pasted.
******************************************************************************/

#if defined(CURSESui)
static void WindowLocation (id, row, col)
LocalId id;
int *row;
int *col;
{
#if defined(unix) || defined(posixSHELL)
#if GETBEGavail
  getbegyx (id, *row, *col);			/* `getbegyx' is a macro. */
#else
  *row = id->_begy;
  *col = id->_begx;
#endif
#endif
#if defined(dos)
  *row = id->_begy;
  *col = id->_begx;
#endif
  return;
}
#endif

/******************************************************************************
* WindowSize.
* Passes back number of rows/columns in window.
******************************************************************************/

#if defined(CURSESui) || defined(SMGui)
static void WindowSize (id, rows, cols)
LocalId id;
int *rows;
int *cols;
{
#if defined(SMGui)
  long height = -1, width = -1;
  smg$get_display_attr (&id, &height, &width, NULL, NULL, NULL, NULL);
  *rows = height;
  *cols = width;
#endif
#if defined(CURSESui)
#if defined(unix) || defined(posixSHELL)
#if GETMAXavail
  getmaxyx (id, *rows, *cols);			/* `getmaxyx' is a macro. */
#else
  *rows = id->_maxy;
  *cols = id->_maxx;
#endif
#endif
#if defined(dos)
  *rows = id->_maxy;
  *cols = id->_maxx;
#endif
#endif
  return;
}
#endif

/******************************************************************************
* RenditionMapping.
* The BLACK rendition is ignored on non-IBM PC machines (it is the same as
* NORMAL).  On IBM PCs it overrides all other renditions.
******************************************************************************/

static int RenditionMapping (rendition)
int rendition;
{
  int mapped;
#if defined(SMGui)
  mapped = 0;
  if ((rendition & BOLD) != 0) mapped = mapped | SMG$M_BOLD;
  if ((rendition & REVERSE) != 0 ||
      (rendition & REVERSE1) != 0 ||
      (rendition & REVERSE2) != 0) mapped = mapped | SMG$M_REVERSE;
  if ((rendition & BLINKING) != 0) mapped = mapped | SMG$M_BLINK;
#endif
#if defined(CURSESui)
#if defined(unix) || defined(posixSHELL)
  mapped = A_NORMAL;
  if ((rendition & BOLD) != 0) mapped = mapped | A_BOLD;
  if ((rendition & REVERSE) != 0 ||
      (rendition & REVERSE1) != 0 ||
      (rendition & REVERSE2) != 0) mapped = mapped | A_REVERSE;
  if ((rendition & BLINKING) != 0) mapped = mapped | A_BLINK;
#endif
#if defined(dos)
  if ((rendition & BLACK) != 0)
    mapped = A_NORMAL | F_BLACK | B_BLACK;
  else {
    if ((rendition & REVERSE) != 0 ||
	(rendition & REVERSE1) != 0 ||
	(rendition & REVERSE2) != 0) {
      if ((rendition & REVERSE) != 0)
	mapped = A_REVERSE | F_GRAY | B_BLUE;
      else
	if ((rendition & REVERSE1) != 0)
	  mapped = A_BOLD | F_GRAY | B_RED;
	else /*REVERSE2*/
	  mapped = A_BOLD | F_GRAY | B_CYAN;
    }
    else
      mapped = A_NORMAL | F_GRAY | B_BLUE;
    if ((rendition & BOLD) != 0) mapped = mapped | A_BOLD;
    if ((rendition & BLINKING) != 0) mapped = mapped | A_BLINK;
  }
#endif
#endif
#if defined(COWui)
  mapped = 0;
  if ((rendition & BOLD) != 0) mapped = mapped | BOLD;
  if ((rendition & REVERSE) != 0 ||
      (rendition & REVERSE1) != 0 ||
      (rendition & REVERSE2) != 0) mapped = mapped | REVERSE;
  if ((rendition & BLINKING) != 0) mapped = mapped | BLINKING;
#endif
  return mapped;
}

/******************************************************************************
* EraseWindow.
* On DECstations, DEC Alphas running OSF/1, and IBM RS6000s, `werase' doesn't
* work - no idea why not.  Also, putting blanks to each position in the window
* doesn't work either.  A non-blank character must first be put and then a
* blank.  Again I'm baffled.  Anyway, this function is used in several places
* to erase the contents of a window.
******************************************************************************/

#if defined(CURSESui)
static int EraseWindow (id)
LocalId id;
{
#if WERASEworks
  werase (id);
#else
  int nRows, nCols, rowN, colN;
  WindowSize (id, &nRows, &nCols);
  for (rowN = 0; rowN < nRows; rowN++) {
     for (colN = 0; colN < nCols; colN++) {
	mvwaddch (id, rowN, colN, '*');
	mvwaddch (id, rowN, colN, ' ');
     }
  }
#endif
  return TRUE;
}
#endif

/******************************************************************************
* MacExecuteFSI.
******************************************************************************/

#if defined(mac)
void MacExecuteFSI (exeFnc, qopFnc)
Logical (*exeFnc) PROTOARGs((int argC, char *argV[]));
Logical (*qopFnc) PROTOARGs((int *argC, char **argV[]));
{
  int argC;
  char **argV;
  InitMacUI ();
  InitMacMenusFSI ();
  InitMacFSI ();
  for (;;) {
     if ((*qopFnc)(&argC,&argV)) {
       (*exeFnc) (argC, argV);
       FreeMacQOPs (argC, argV);
     }
     else
       return;
  }
}
#endif

/******************************************************************************
* InitMacMenusFSI.
* Initialize the Macintosh menus for the full screen interface window.
******************************************************************************/

#if defined(mac)
void InitMacMenusFSI () {
  appleMenuHfsi = GetMenu (APPLEri);
  AddResMenu (appleMenuHfsi, *((long *) "DRVR"));
  InsertMenu (appleMenuHfsi, 0);
  DrawMenuBar ();
  return;
}
#endif

/******************************************************************************
* InitMacFSI.
* Initialize the Macintosh full screen interface window (pasteboard).
******************************************************************************/

#if defined(mac)
void InitMacFSI () {
  static WindowRecord wRecord;
  WindowPtr behindWindow = (WindowPtr) -1;
  fsiWindowP = GetNewWindow (FSIri, &wRecord, behindWindow);
  SetPort (fsiWindowP);
  TextFont (monaco);
  return;
}
#endif

/******************************************************************************
* EncodeKeyDefinitions.
******************************************************************************/

#if defined(STDARG)
void EncodeKeyDefinitions (int nLines, char **lineS, ...)
#else
void EncodeKeyDefinitions (va_alist)
va_dcl
#endif
{
#if !defined(STDARG)
  int nLines;
  char **lineS;		/* Capital `S' because of the IBM RS6000. */
#endif
  int lineN;		/* Line number. */
  va_list ap;
  /****************************************************************************
  * Start variable-length argument list scanning.
  ****************************************************************************/
#if defined(STDARG)
  va_start (ap, lineS);
#else
  VA_START (ap);
  nLines = va_arg (ap, int);
  lineS = va_arg (ap, char **);
#endif
  /****************************************************************************
  * Scan lines replacing ______'s with corresponding key tokens.
  ****************************************************************************/
  for (lineN = 0; lineN < nLines; lineN++) {
     char *ptr1 = strchr(lineS[lineN],'_');
     while (ptr1 != NULL) {
       char *ptr2 = ptr1, *ptr = ptr1, token[MAX_KEY_TOKEN_LEN+1];
       int i, lenT, lenL, count, pad, key = va_arg(ap,int);
       while (*(ptr2+1) == '_') ptr2++;
       lenL = (int) (ptr2 - ptr1 + 1);
       strcpyX (token, KeyToken(key,lenL), MAX_KEY_TOKEN_LEN);
       lenT = (int) strlen (token);
       count = MINIMUM(lenT,lenL);
       pad = (lenT < lenL ? lenL - lenT : 0);
       for (i = 0; i < count; i++) *ptr++ = token[i];
       for (i = 0; i < pad; i++) *ptr++ = ' ';
       ptr1 = strchr (ptr2 + 1, '_');
     }
  }
  va_end (ap);
  return;
}

/******************************************************************************
* KeyToken.
* Return address of character string for key.
******************************************************************************/

static char *KeyToken(key,maxL)
int key;
int maxL;
{
  switch (key) {
    case KB_a: return AlphaKey('a',maxL);
    case KB_b: return AlphaKey('b',maxL);
    case KB_c: return AlphaKey('c',maxL);
    case KB_d: return AlphaKey('d',maxL);
    case KB_e: return AlphaKey('e',maxL);
    case KB_f: return AlphaKey('f',maxL);
    case KB_g: return AlphaKey('g',maxL);
    case KB_h: return AlphaKey('h',maxL);
    case KB_i: return AlphaKey('i',maxL);
    case KB_j: return AlphaKey('j',maxL);
    case KB_k: return AlphaKey('k',maxL);
    case KB_l: return AlphaKey('l',maxL);
    case KB_m: return AlphaKey('m',maxL);
    case KB_n: return AlphaKey('n',maxL);
    case KB_o: return AlphaKey('o',maxL);
    case KB_p: return AlphaKey('p',maxL);
    case KB_q: return AlphaKey('q',maxL);
    case KB_r: return AlphaKey('r',maxL);
    case KB_s: return AlphaKey('s',maxL);
    case KB_t: return AlphaKey('t',maxL);
    case KB_u: return AlphaKey('u',maxL);
    case KB_v: return AlphaKey('v',maxL);
    case KB_w: return AlphaKey('w',maxL);
    case KB_x: return AlphaKey('x',maxL);
    case KB_y: return AlphaKey('y',maxL);
    case KB_z: return AlphaKey('z',maxL);
    case KB_A: return AlphaKey('A',maxL);
    case KB_B: return AlphaKey('B',maxL);
    case KB_C: return AlphaKey('C',maxL);
    case KB_D: return AlphaKey('D',maxL);
    case KB_E: return AlphaKey('E',maxL);
    case KB_F: return AlphaKey('F',maxL);
    case KB_G: return AlphaKey('G',maxL);
    case KB_H: return AlphaKey('H',maxL);
    case KB_I: return AlphaKey('I',maxL);
    case KB_J: return AlphaKey('J',maxL);
    case KB_K: return AlphaKey('K',maxL);
    case KB_L: return AlphaKey('L',maxL);
    case KB_M: return AlphaKey('M',maxL);
    case KB_N: return AlphaKey('N',maxL);
    case KB_O: return AlphaKey('O',maxL);
    case KB_P: return AlphaKey('P',maxL);
    case KB_Q: return AlphaKey('Q',maxL);
    case KB_R: return AlphaKey('R',maxL);
    case KB_S: return AlphaKey('S',maxL);
    case KB_T: return AlphaKey('T',maxL);
    case KB_U: return AlphaKey('U',maxL);
    case KB_V: return AlphaKey('V',maxL);
    case KB_W: return AlphaKey('W',maxL);
    case KB_X: return AlphaKey('X',maxL);
    case KB_Y: return AlphaKey('Y',maxL);
    case KB_Z: return AlphaKey('Z',maxL);
    case KB_PLUS: return AlphaKey('+',maxL);
    case KB_MINUS: return AlphaKey('-',maxL);
    case KB_CTRL_at: return ControlKey('@',maxL);
    case KB_CTRL_A: return ControlKey('A',maxL);
    case KB_CTRL_B: return ControlKey('B',maxL);
    case KB_CTRL_C: return ControlKey('C',maxL);
    case KB_CTRL_D: return ControlKey('D',maxL);
    case KB_CTRL_E: return ControlKey('E',maxL);
    case KB_CTRL_F: return ControlKey('F',maxL);
    case KB_CTRL_G: return ControlKey('G',maxL);
#if !defined(dos) && !defined(mac) && !defined(win32) && !defined(__MINGW32__)
    /**************************************************************************
    * Same as DELETE on IBM PC and Macintosh.
    **************************************************************************/
    case KB_CTRL_H: return ControlKey('H',maxL);
#endif
    case KB_TAB:
      switch (maxL) {
	case 3: case 4:
	  return "TAB";
	default:
	  return "<Tab>";
      }
    case KB_CTRL_J: return ControlKey('J',maxL);
    case KB_CTRL_K: return ControlKey('K',maxL);
    case KB_CTRL_L: return ControlKey('L',maxL);
    case KB_RETURN:
      switch (maxL) {
	case 3: case 4:
	  return "RET";
	case 5: case 6: case 7:
	  return "<RET>";
	default:
	  return "<Return>";
      }
    case KB_CTRL_N: return ControlKey('N',maxL);
    case KB_CTRL_O: return ControlKey('O',maxL);
    case KB_CTRL_P: return ControlKey('P',maxL);
    case KB_CTRL_Q: return ControlKey('Q',maxL);
    case KB_CTRL_R: return ControlKey('R',maxL);
    case KB_CTRL_S: return ControlKey('S',maxL);
    case KB_CTRL_T: return ControlKey('T',maxL);
    case KB_CTRL_U: return ControlKey('U',maxL);
    case KB_CTRL_V: return ControlKey('V',maxL);
    case KB_CTRL_W: return ControlKey('W',maxL);
    case KB_CTRL_X: return ControlKey('X',maxL);
    case KB_CTRL_Y: return ControlKey('Y',maxL);
    case KB_CTRL_Z: return ControlKey('Z',maxL);
    case KB_ESCAPE:
      switch (maxL) {
	case 3: case 4:
	  return "ESC";
	default:
	  return "<Esc>";
      }
    case KB_DELETE:
      switch (maxL) {
	case 3: case 4:
	  return "DEL";
	case 5: case 6: case 7:
	  return "<DEL>";
	default:
	  return "<Delete>";
      }
    case KB_UPARROW:
      switch (maxL) {
	case 2: case 3:
	  return "UP";
	case 4: case 5: case 6:
	  return "<UP>";
	case 7: case 8:
	  return "UpArrow";
	default:
	  return "<UpArrow>";
      }
    case KB_DOWNARROW:
      switch (maxL) {
	case 3:
	  return "DWN";
	case 4: case 5:
	  return "DOWN";
	case 6: case 7: case 8:
	  return "<DOWN>";
	case 9: case 10:
	  return "DownArrow";
	default:
	  return "<DownArrow>";
      }
    case KB_LEFTARROW:
      switch (maxL) {
	case 3:
	  return "LFT";
	case 4: case 5:
	  return "LEFT";
	case 6: case 7: case 8:
	  return "<LEFT>";
	case 9: case 10:
	  return "LeftArrow";
	default:
	  return "<LeftArrow>";
      }
    case KB_RIGHTARROW:
      switch (maxL) {
	case 3:
	  return "RHT";
	case 5: case 6:
	  return "RIGHT";
	case 7: case 8: case 9:
	  return "<RIGHT>";
	case 10: case 11:
	  return "RightArrow";
	default:
	  return "<RightArrow>";
      }
  }
  return "?";
}

/******************************************************************************
* AlphaKey.
******************************************************************************/

static char *AlphaKey (key, maxL)
int key;
int maxL;
{
  static char token[MAX_KEY_TOKEN_LEN+1];
  switch (maxL) {
    case 1: case 2:
      snprintf (token, (size_t) sizeof(token), "%c", key);
      break;
    default:
      snprintf (token, (size_t) sizeof(token), "<%c>", key);
      break;
  }
  return token;
}

/******************************************************************************
* ControlKey.
******************************************************************************/

static char *ControlKey (key, maxL)
int key;
int maxL;
{
  static char token[MAX_KEY_TOKEN_LEN+1];
  switch (maxL) {
    case 3: case 4:
      snprintf (token, (size_t) sizeof(token), "C-%c", key);
      break;
    case 5:
      snprintf (token, (size_t) sizeof(token), "CTL-%c", key);
      break;
    case 6: case 7:
      snprintf (token, (size_t) sizeof(token), "Ctrl-%c", key);
      break;
    default:
      snprintf (token, (size_t) sizeof(token), "<Ctrl-%c>", key);
      break;
  }
  return token;
}

/******************************************************************************
* KeyWaitingAST.
******************************************************************************/

#if defined(SMGui)
void KeyWaitingAST () {
  keyWaiting++;
  return;
}
#endif
