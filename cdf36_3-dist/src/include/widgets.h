/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                    Header file for `widgets'.
*
*  Version 1.3a, 30-Mar-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  26-Nov-93, J Love     Original version.
*   V1.0a  4-Feb-94, J Love     DEC Alpha/OpenVMS port.
*   V1.0b  6-Apr-94, J Love     Solaris using GNU C compiler.
*   V1.1  13-Dec-94, J Love     CDF V2.5.
*   V1.1a 23-Jan-95, J Love     Primary/alternate "current" item movement keys
*                               for ItemWindow.
*   V1.1b  7-Mar-95, J Love     Added FieldWindow.
*   V1.2  30-Mar-95, J Love     POSIX.
*   V1.2a  6-Sep-95, J Love     CDFexport-related changes.
*   V1.3  18-Jul-96, J Love     CDF V2.6.
*   V1.3a 30-Mar-97, J Love     Allow FieldWindow fields to grow larger than
*                               their on-screen width.
*
******************************************************************************/

#if !defined(WIDGETSh_INCLUDEd__)
#define WIDGETSh_INCLUDEd__

/******************************************************************************
* Include files.
******************************************************************************/

#include "windoz.h"

/******************************************************************************
* Constants.
******************************************************************************/

#define MAX_INFO_WINDOW_eLINES  3
#define INFOtextMAX             (MAX_INFO_WINDOW_eLINES*(SCREEN_WIDTH-2+1))

/******************************************************************************
* ItemWindow operations.
*
*  ItemWindow (NEWiw, &IWstruct, ItemNumber);
*  ItemWindow (UPDATEiw, &IWstruct, ItemNumber);
*  ItemWindow (READiw, &IWstruct);
*  ItemWindow (DELETEiw, &IWstruct);
*  ItemWindow (UNDISPLAYiw, &IWstruct);
*  ItemWindow (REDISPLAYiw, &IWstruct);
*  ItemWindow (BEEPiw, &IWstruct);
*
*  where,
*
*    <op>iw......................the operation to perform.
*    IWstruct....................a pointer to an `ItemWindowStruct' structure.
*    ItemNumber(int).............the initial current item number.
*
******************************************************************************/

#define NEWiw           1       /* Create new menu. */
#define UPDATEiw        2       /* Update sections. */
#define READiw          3       /* Read keystrokes until `exit' key entered. */
#define DELETEiw        4       /* Delete menu. */
#define UNDISPLAYiw     5       /* Erase menu (it still exists though). */
#define REDISPLAYiw     6       /* Redisplay menu. */
#define BEEPiw          7       /* Beep. */

/******************************************************************************
* ItemWindow structure.
******************************************************************************/

struct ItemWindowStruct {
  /***************************************************************************
  * These must be initialized before calling `ItemWindow' the first time.
  ***************************************************************************/
  int ULrow;
  int ULcol;
  int nColsTotal;               /* Includes left and right borders. */
  char *label;
  int NhLines;
  char **hLines;
  int NiLines;
  char **iLines;
  int nItems;
  int *iLineNs;
  int *iCols;
  int *iLens;
  int NiRows;
  int NtLines;
  char **tLines;
  int *exitChars;               /* The characters which cause an exit from
				   the prompt window.  Terminated by NUL. */
  int refreshChar;              /* Causes entire screen to be refreshed. */
  Logical iPct;                 /* TRUE if a percentage indicator should be
				   displayed in the item section. */
  int NSkey;                    /* Next-screen key. */
  int PSkey;                    /* Prev-screen key. */
  /***************************************************************************
  * These are initialized/calculated/returned by `ItemWindow'.
  ***************************************************************************/
  int itemN;                    /* Current item number. */
  int key;                      /* Key entered causing exit from menu. */
  /***************************************************************************
  * These are for internal use by `ItemWindow'.
  ***************************************************************************/
  int nRowsTotal;               /* Total number of rows (including top and
				   bottom border rows). */
  int nColS;                    /* Doesn't include left and right borders. */
  int *iUpTo;
  int *iDownTo;
  int *iLeftTo;
  int *iRightTo;
  int hRowT, iRowT, tRowT;      /* Top row for section. */
  int hRowB, iRowB, tRowB;      /* Bottom row for section.  For the item
				   section, it is the last row actually
				   used. */
  Logical iScroll;              /* TRUE if item section needs to be scrolled
				   (more lines than rows). */
  int iRowN;                    /* Row number in item section containing
				   current item. */
  int iPctRowN;                 /* Row number of item section percentage
				   indicator. */
  int iPctColN;                 /* Column number of item section percentage
				   indicator. */
  WINDOWid wid;                 /* Window identifier. */
};

/******************************************************************************
* ItemWindow "current" item primary/alternate movement keys.
******************************************************************************/

#define IW_DOWN         KB_DOWNARROW
#define IW_UP           KB_UPARROW
#define IW_LEFT         KB_LEFTARROW
#define IW_RIGHT        KB_RIGHTARROW

#define IW_DOWNx        KB_d
#define IW_UPx          KB_u
#define IW_LEFTx        KB_l
#define IW_RIGHTx       KB_r

/******************************************************************************
* PromptWindow operations.
*
*  PromptWindow (NEWpw, &PWstruct, CursorAtThisChar, InsertMode);
*  PromptWindow (RESETpw, &PWstruct, CursorAtThisChar, InsertMode);
*  PromptWindow (READpw, &PWstruct);
*  PromptWindow (UNDISPLAYpw, &PWstruct);
*  PromptWindow (REDISPLAYpw, &PWstruct);
*  PromptWindow (DELETEpw, &PWstruct);
*  PromptWindow (BEEPpw, &PWstruct);
*
*  where,
*    <op>pw...................the operation to perform.
*    PWstruct.................a pointer to a `PromptWindowStruct' structure.
*    CursorAtThisChar(int)....character position at which to start the cursor.
*    InsertMode(Logical)......initial insert mode.  TRUE means insert, FALSE
*                             means overstrike.  `TOGGLEchar' toggles this
*                             mode after it is initially set.
*
******************************************************************************/

#define NEWpw           1       /* Create new window. */
#define RESETpw         2       /* Reset window. */
#define READpw          3       /* Read keystrokes until `exit' key entered. */
#define DELETEpw        4       /* Delete window. */
#define BEEPpw          5       /* Beep. */
#define UNDISPLAYpw     6       /* Erase window (it still exists though). */
#define REDISPLAYpw     7       /* Redisplay window. */

/******************************************************************************
* PromptWindow structure.
******************************************************************************/

struct PromptWindowStruct {
  /***************************************************************************
  * These must be initialized before calling `PromptWindow' the first time.
  ***************************************************************************/
  char *label;
  int ULrow;
  int ULcol;
  int nColsTotal;               /* Includes left and right borders. */
  int NhLines;
  char **hLines;
  int maxChars;
  char *value;
  int NtLines;
  char **tLines;
  int *exitChars;               /* The characters which cause an exit from
				   the prompt window.  Terminated by NUL. */
  int refreshChar;              /* Causes entire screen to be refreshed. */
  int SOLchar;                  /* Moves cursor to start-of-line. */
  int EOLchar;                  /* Moves cursor to end-of-line. */
  int TOGGLEchar;               /* Toggles between insert mode and overstrike
				   mode. */
  /***************************************************************************
  * These are initialized/calculated/returned by `PromptWindow'.
  ***************************************************************************/
  int key;                      /* Key entered causing exit from menu. */
  /***************************************************************************
  * These are for internal use by `PromptWindow'.
  ***************************************************************************/
  Logical insertMode;           /* If TRUE, insert mode.  If FALSE, overstrike
				   mode. */
  int nRowsTotal;               /* Total number of rows (including top and
				   bottom border rows). */
  int nColS;                    /* Doesn't include left and right borders. */
  int curCol;                   /* Current column cursor is on (numbered from
				   zero (0). */
  int curChar;                  /* Current character cursor is on (numbered
				   from zero (0). */
  int curLen;                   /* Current length of value. */
  int NvCols;                   /* Number of columns in value section. */
  int vRow;                     /* Row number value is on. */
  int vLcol;                    /* Left-most column of value. */
  int vRcol;                    /* Right-most column of value. */
  int mLcol;                    /* Column left `more' indicator is in. */
  int mRcol;                    /* Column right `more' indicator is in. */
  int hRowT, tRowT;             /* Top row for section. */
  int hRowB, tRowB;             /* Bottom row for section.  */
  WINDOWid wid;                 /* Window id. */
};

/******************************************************************************
* EditWindow operations.
*
*  EditWindow (NEWew, &EWstruct, InsertMode);
*  EditWindow (UPDATEew, &EWstruct, InsertMode);
*  EditWindow (READew, &EWstruct);
*  EditWindow (DELETEew, &EWstruct);
*  EditWindow (BEEPew, &EWstruct);
*
*  where,
*    <op>ew...................the operation to perform.
*    EWstruct.................a pointer to a `EditWindowStruct' structure.
*    InsertMode(Logical)......initial insert mode.  TRUE means insert, FALSE
*                             means overstrike.  Ctrl-A toggles this mode after
*                             it is initially set.
*
******************************************************************************/

#define NEWew           1       /* Create edit window. */
#define UPDATEew        2       /* Update edit window. */
#define READew          3       /* Read keystrokes until `exit' key entered. */
#define DELETEew        4       /* Delete window. */
#define BEEPew          5       /* Beep. */

/******************************************************************************
* EditWindow structure.
******************************************************************************/

struct EditWindowStruct {
  /***************************************************************************
  * These must be initialized before calling `EditWindow' the first time.
  ***************************************************************************/
  char *label;                  /* Label for top of window (on border). */
  int ULrow;                    /* Row on the screen at which top line of
				   window should be placed. */
  int ULcol;                    /* Column on the screen at which the left-most
				   column of the window should be placed. */
  int nColsTotal;               /* Number of columns that window should have
				   (this includes the left and right border
				   columns). */
  int NhLines;                  /* Number of header lines. */
  char **hLines;                /* Header lines. */
  char *eText;                  /* Text to be edited. */
  int NeRows;                   /* Number of rows in the edit section.  This
				   can be less than the number of lines. */
  int NtLines;                  /* Number of trailer lines. */
  char **tLines;                /* Trailer lines. */
  Logical ePct;                 /* TRUE if a percentage indicator should be
				   displayed in the edit section. */
  Logical readOnly;             /* TRUE if the edit section may not be
				   modified. */
  int *exitChars;               /* The characters which cause an exit from
				   the edit window.  Terminated by NUL. */
  int REFRESHkey;               /* Causes entire screen to be refreshed. */
  int SOLkey;                   /* Moves cursor to start-of-line. */
  int EOLkey;                   /* Moves cursor to end-of-line. */
  int SOTkey;                   /* Moves cursor to start-of-text. */
  int EOTkey;                   /* Moves cursor to end-of-text. */
  int NSkey;                    /* Moves cursor to next-screen. */
  int PSkey;                    /* Moves cursor to prev-screen. */
  int DLkey;                    /* Delete line that cursor is on. */
  int NWkey;                    /* Moves cursor to end of current/next word. */
  int TOGGLEkey;                /* Toggles between insert mode and overstrike
				   mode. */
  /***************************************************************************
  * These are initialized/calculated/returned by `EditWindow'.
  ***************************************************************************/
  int key;                      /* Key entered causing exit from menu. */
  /***************************************************************************
  * These are for internal use by `EditWindow'.
  ***************************************************************************/
  int nRowsTotal;               /* Total number of rows (including top and
				   bottom border rows). */
  int nColS;                    /* Doesn't include left and right borders. */
  int hRowT, eRowT, tRowT;      /* Top row for section. */
  int hRowB, eRowB, tRowB;      /* Bottom row for section. */
  int nChars;                   /* Number of characters currently in `text'. */
  int xChars;                   /* Maximum number of characters that `text'
				   can currently contain. */
  int cursorRow;                /* Row containing cursor. */
  int cursorCol;                /* Column containing cursor. */
  int firstChar;                /* Character in text at the upper-left
				   position of the EditSection. */
  int cursorChar;               /* Character in text on which the cursor is
				   sitting. */
  Logical insertMode;           /* If TRUE, insert mode.  If FALSE, overstrike
				   mode. */
  int ePctRowN;                 /* Row number of edit section percentage
				   indicator. */
  int ePctColN;                 /* Column number of edit section percentage
				   indicator. */
  WINDOWid wid;                 /* Window id. */
};

/******************************************************************************
* FieldWindow operations.
*
*  FieldWindow (NEWfw, &FWstruct, fieldNumber, insertMode);
*  FieldWindow (UPDATEfw, &FWstruct, fieldNumber, insertMode);
*  FieldWindow (READfw, &FWstruct);
*  FieldWindow (DELETEfw, &FWstruct);
*  FieldWindow (UNDISPLAYfw, &FWstruct);
*  FieldWindow (REDISPLAYfw, &FWstruct);
*  FieldWindow (BEEPfw, &FWstruct);
*
*  where,
*
*    <op>fw..................is the operation to perform.
*    FWstruct................is a pointer to an `FieldWindowStruct' structure.
*    fieldNumber(int)........is the initial current field number.
*    insertMode(Logical).....is the initial insert mode.  TRUE means insert,
*                            FALSE means overstrike.  This mode is toggled by
*                            TOGGLEkey.
*
******************************************************************************/

#define NEWfw           1       /* Create new menu. */
#define UPDATEfw        2       /* Update sections. */
#define READfw          3       /* Read keystrokes until `exit' key entered. */
#define DELETEfw        4       /* Delete menu. */
#define UNDISPLAYfw     5       /* Erase menu (it still exists though). */
#define REDISPLAYfw     6       /* Redisplay menu. */
#define BEEPfw          7       /* Beep. */

/******************************************************************************
* FieldWindow structure.
******************************************************************************/

struct FieldWindowStruct {
  /***************************************************************************
  * These must be initialized before calling `FieldWindow' the first time.
  ***************************************************************************/
  int ULrow;                    /* Upper-left row number. */
  int ULcol;                    /* Upper-left column number. */
  int nColsTotal;               /* Includes left and right borders. */
  char *label;                  /* Label for window. */
  int NhLines;                  /* Number of header lines (zero or more). */
  char **hLines;                /* Header lines. */
  int NfLines;                  /* Number of field lines (one or more). */
  char **fLines;                /* Field lines. */
  char **fields;                /* Fields. */
  int nFields;                  /* Number of fields (one or more). */
  int *fLineNs;                 /* Line number for each field. */
  int *fCols;                   /* Starting column number for each field. */
  int *fLens;                   /* Length (width) of each field. */
  int *fMaxs;                   /* Maximum length of a field.  This can be
				   greater than the on-screen width. */
  int NfRows;                   /* Number of rows in fields section. */
  int NtLines;                  /* Number of trailer lines (zero or more). */
  char **tLines;                /* Trailer lines. */
  int *exitChars;               /* The characters which cause an exit from
				   the prompt window.  Terminated by NUL. */
  int refreshChar;              /* Causes entire screen to be refreshed. */
  Logical fPct;                 /* TRUE if a percentage indicator should be
				   displayed in the fields section. */
  int NSkey;                    /* Next-screen key. */
  int PSkey;                    /* Prev-screen key. */
  int toggleKey;                /* Toggles between insert and overstrike. */
  /***************************************************************************
  * These are initialized/calculated/returned by `FieldWindow'.
  ***************************************************************************/
  int fieldN;                   /* Current field number. */
  int key;                      /* Key entered causing exit from menu. */
  /***************************************************************************
  * These are for internal use by `FieldWindow'.
  ***************************************************************************/
  int charN;                    /* Cursor position within current field. */
  int leftCharN;                /* Character within current field that is at
				   the left (first) on-screen position. */
  int nRowsTotal;               /* Total number of rows (including top and
				   bottom border rows). */
  int nColS;                    /* Doesn't include left and right borders. */
  int *fUpTo;                   /* Field number above. */
  int *fDownTo;                 /* Field number below. */
  int *fLeftTo;                 /* Field number to the left. */
  int *fRightTo;                /* Field number to the right. */
  int hRowT, fRowT, tRowT;      /* Top row for header, fields, and trailer
				   sections. */
  int hRowB, fRowB, tRowB;      /* Bottom row for each section.  For the fields
				   section, it is the last row actually used.*/
  Logical fScroll;              /* TRUE if fields section needs to be scrolled
				   (more lines than rows). */
  int fRowN;                    /* Row number in fields section containing
				   current field. */
  int fPctRowN;                 /* Row number of fields section percentage
				   indicator. */
  int fPctColN;                 /* Column number of fields section percentage
				   indicator. */
  Logical insert;               /* If TRUE, insert.  If FALSE, overstrike. */
  WINDOWid wid;                 /* Window identifier. */
};

/******************************************************************************
* FieldWindow "current" field primary/alternate movement keys.
******************************************************************************/

#define FW_DOWN_FIELD   KB_DOWNARROW
#define FW_UP_FIELD     KB_UPARROW
#define FW_LEFT_FIELD   NUL
#define FW_RIGHT_FIELD  KB_TAB
#define FW_LEFT_CHAR    KB_LEFTARROW
#define FW_RIGHT_CHAR   KB_RIGHTARROW

#define FW_DOWN_FIELDx  NUL
#define FW_UP_FIELDx    NUL
#define FW_LEFT_FIELDx  NUL
#define FW_RIGHT_FIELDx NUL
#define FW_LEFT_CHARx   NUL
#define FW_RIGHT_CHARx  NUL

/******************************************************************************
* Function prototypes.
******************************************************************************/

int ItemWindow VARPROTOARGs((int, ...));
int PromptWindow VARPROTOARGs((int, ...));
int EditWindow VARPROTOARGs((int, ...));
int FieldWindow VARPROTOARGs((int, ...));
void AllocIW PROTOARGs((
  struct ItemWindowStruct *IW, int nItems, int NiLines, int iLineNchars,
  void (*fatalFnc) PROTOARGs((char *msg))
));
void FreeIW PROTOARGs((
  struct ItemWindowStruct *IW, void (*fatalFnc) PROTOARGs((char *msg))
));
void AllocFW PROTOARGs((
  struct FieldWindowStruct *FW, int nFields, int NfLines, int fLineNchars,
  void (*fatalFnc) PROTOARGs((char *msg))
));
void FreeFW PROTOARGs((
  struct FieldWindowStruct *FW, void (*fatalFnc) PROTOARGs((char *msg))
));
Logical OnlineHelpWindow PROTOARGs((char *ilhFile, int helpId));
Logical LoadOnlineHelp PROTOARGs((
  char *ilhFile, int helpId, char *header, char **eText
));
void InfoWindow PROTOARGs((
  char *line1, char *line2, char *line3, Logical center, Logical beep, int wait
));

/*****************************************************************************/

#endif
