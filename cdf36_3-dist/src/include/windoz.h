/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                    Header file for `windows'.
*
*  Version 4.0, 14-Nov-97, Hughes STX.
*
*  Modification history
*
*   V1.0  29-Jan-91, H Leckner	Original vesion (for CDF V2.0).
*   V1.1   1-Oct-91, J Love	Modified for IRIX 4.0 port.
*   V1.2  15-Oct-91, H Leckner  Changed  for IBM-RS6000 (AIX) port
*   V2.0  30-Apr-92, H Leckner  IBM PC port.  CDF V2.2.
*   V3.0  30-Nov-93, J Love     CDF V2.4.  Generalized for all platforms.
*                               Included `kb_def.h'.
*   V3.0a  6-Apr-94, J Love	Solaris using Gnu C compiler.
*   V3.1  25-Oct-94, J Love	CDF V2.5.
*   V3.1a 23-Jan-95, J Love	IRIX 6.x (64-bit).
*   V3.1b 16-Mar-95, J Love	`EncodeKeyDefinitions' prototype moved here.
*   V3.2   6-Apr-95, J Love	POSIX.
*   V3.2a 18-Apr-95, J Love	More POSIX.
*   V3.2b 13-Jun-95, J Love	Linux.
*   V3.3  15-Jun-95, J Love	`key_waiting'.
*   V3.3a 31-Aug-95, J Love	CDFexport-related changes.
*   V3.3b 18-Sep-95, J Love	Macintosh event handling.
*   V3.4   3-Oct-96, J Love	CDF V2.6.
*   V3.4a  2-Sep-97, J Love	Special keys for AIX.
*   V4.0  14-Nov-97, J Love	Windows NT/Visual C++.
*   V4.1   2-May-01, M Liu      Special keys for CYGWIN.
*   V4.2  11-Jul-05, M Liu      Added MingW port for PC.
*
******************************************************************************/

#if !defined(WINDOZh_INCLUDEd__)
#define WINDOZh_INCLUDEd__

/******************************************************************************
* Include files.
******************************************************************************/

#include "cdftools.h"

/******************************************************************************
* Which type of screen management is being used?
******************************************************************************/

#if defined(vms)
#  define SMGui
#endif

#if defined(unix) || defined(dos) || defined(posixSHELL)
#  define CURSESui
#endif

#if defined(mac) || defined(win32)
#  define COWui				/* Curses On Windows... */
#endif

/******************************************************************************
* Include files.  Because some `curses.h' files define TRUE and FALSE without
* first checking to see if they are already defined and because some compilers
* complain when that happens, we will save those definitions, undefine them,
* include `curses.h', and then restore the definitions if `curses.h' did not
* define them.
******************************************************************************/


#if defined(CURSESui)
#  define TRUEx TRUE
#  undef TRUE
#  define FALSEx FALSE
#  undef FALSE
#  if defined(ultrix)
#    include <cursesX.h>
#  else
#    if defined(linux) || defined(__QNX__)
#      include <ncurses.h>
#    else
#      include <curses.h>
#    endif
#  endif
#  if !defined(TRUE)
#    define TRUE TRUEx
#  endif
#  if !defined(FALSE)
#    define FALSE FALSEx
#  endif
#endif

#if defined(SMGui)
#  include <smg$routines.h>
#endif

/******************************************************************************
* `typedef's.
******************************************************************************/

#if defined(dos)
typedef int chtype;
#endif

/******************************************************************************
* Windows NT.
******************************************************************************/

#define NUMfsiROWS	24
#define NUMfsiCOLS	80

/******************************************************************************
* For some reason the `curses.h' file on some Solaris 2.2 machines does not
* seem to know that it is a System V implementation.  For that reason, `strchr'
* and `strrchr' are defined to `index' and `rindex', respectively (for what
* the maintainers of Curses say are portability reasons).  This is bad since
* `index' and `rindex' do not exist in the C run-time library on them there
* machines.  To be safe, we'll undefine `strchr' and `strrchr' everywhere since
* we always want to reference those names.
******************************************************************************/

#if defined(strchr)
#  undef strchr
#endif

#if defined(strrchr)
#  undef strrchr
#endif

/******************************************************************************
* Curses function availability/reliability.
*  1. `werase' doesn't seem to work on Ultrix, OSF/1, and AIX.
*  2. `getmaxyx', `getbegyx', and `curs_set' aren't available on Ultrix,
*     OSF/1, AIX, or HP-UX.
*  3. `curs_set' seems to be available under the POSIX Shell but there is
*     no function prototype.
******************************************************************************/

#if defined(CURSESui)
#  if defined(ultrix) || defined(alphaosf) || defined(AIX)
#    define WERASEworks	0
#  else
#    define WERASEworks	1
#  endif
#  if defined(ultrix) || defined(alphaosf) || defined(AIX) || defined(hpux)
#    define GETMAXavail	0
#    define GETBEGavail	0
#  else
#    define GETMAXavail	1
#    define GETBEGavail	1
#  endif
#  if defined(ultrix) || defined(alphaosf) || defined(AIX) || defined(hpux) || defined(posixSHELL)
#    define CURS_SETavail	0
#  else
#    define CURS_SETavail	1
#  endif
#endif

/******************************************************************************
* Line drawing characters (for those machines on which they are not already
* defined or don't seem to work).
******************************************************************************/

#if defined(CURSESui)
#  if defined(posixSHELL)
#    define ACS_LLCORNER    ACS_BLCORNER
#    define ACS_LRCORNER    ACS_BRCORNER
#  endif
#  if defined(ultrix) || defined(AIX) || defined(hpux)
#    define problemACS
#  endif
#  if defined(alphaosf) || defined(IRIX64bit) || defined(_LP64) || \
      defined(posixSHELL) || defined(__QNX__) || \
      defined(__CYGWIN__) || defined(__MINGW32__)
#    define problemACS
#    undef ACS_VLINE
#    undef ACS_HLINE
#    undef ACS_TTEE
#    undef ACS_BTEE
#    undef ACS_LTEE
#    undef ACS_RTEE
#    undef ACS_ULCORNER
#    undef ACS_URCORNER
#    undef ACS_LLCORNER
#    undef ACS_LRCORNER
#    undef ACS_PLUS
#  endif
#  if defined(problemACS)
#    if defined(ACS_VLINE)
#       undef ACS_VLINE
#    endif
#    define ACS_VLINE   '|'
#    if defined(ACS_HLINE)
#       undef ACS_HLINE
#    endif
#    define ACS_HLINE   '-'
#    if defined(ACS_TTEE)
#       undef ACS_TTEE
#    endif
#    define ACS_TTEE    '+'
#    if defined(ACS_BTEE)
#       undef ACS_BTEE
#    endif
#    define ACS_BTEE    '+'
#    if defined(ACS_LTEE)
#       undef ACS_LTEE
#    endif
#    define ACS_LTEE    '+'
#    if defined(ACS_RTEE)
#       undef ACS_RTEE
#    endif
#    define ACS_RTEE    '+'
#    if defined(ACS_ULCORNER)
#       undef ACS_ULCORNER
#    endif
#    define ACS_ULCORNER        '+'
#    if defined(ACS_URCORNER)
#       undef ACS_URCORNER
#    endif
#    define ACS_URCORNER        '+'
#    if defined(ACS_LLCORNER)
#       undef ACS_LLCORNER
#    endif
#    define ACS_LLCORNER        '+'
#    if defined(ACS_LRCORNER)
#       undef ACS_LRCORNER
#    endif
#    define ACS_LRCORNER        '+'
#    if defined(ACS_PLUS)
#       undef ACS_PLUS
#    endif
#    define ACS_PLUS    '+'
#    undef problemACS
#  endif
#  if defined(dos)
#    define ACS_PLUS        0305
#    define ACS_TTEE        0302
#    define ACS_BTEE        0301
#    define ACS_LTEE        0303
#    define ACS_RTEE        0264
#    define ACS_HLINE       0304
#    define ACS_VLINE       0263
#    define ACS_ULCORNER    0332
#    define ACS_URCORNER    0277
#    define ACS_LRCORNER    0331
#    define ACS_LLCORNER    0300
#  endif
#endif

#if defined(COWui)
#  if defined(mac)
#    define ACS_PLUS        ((char) 0xF0)
#    define ACS_TTEE        ((char) 0xF1)
#    define ACS_BTEE        ((char) 0xF2)
#    define ACS_LTEE        ((char) 0xF3)
#    define ACS_RTEE        ((char) 0xF4)
#    define ACS_HLINE       ((char) 0xF5)
#    define ACS_VLINE       ((char) 0xF6)
#    define ACS_ULCORNER    ((char) 0xF7)
#    define ACS_URCORNER    ((char) 0xF8)
#    define ACS_LRCORNER    ((char) 0xF9)
#    define ACS_LLCORNER    ((char) 0xFA)
#  endif
#  if defined(win32)
#    define ACS_PLUS        ((char) 0xF0)
#    define ACS_TTEE        ((char) 0xF1)
#    define ACS_BTEE        ((char) 0xF2)
#    define ACS_LTEE        ((char) 0xF3)
#    define ACS_RTEE        ((char) 0xF4)
#    define ACS_HLINE       ((char) 0xF5)
#    define ACS_VLINE       ((char) 0xF6)
#    define ACS_ULCORNER    ((char) 0xF7)
#    define ACS_URCORNER    ((char) 0xF8)
#    define ACS_LRCORNER    ((char) 0xF9)
#    define ACS_LLCORNER    ((char) 0xFA)
#  endif
#endif

/******************************************************************************
* Special keys which have to be mapped to other keys.
******************************************************************************/

#if defined(sgi)
#  define SGI_CONSOLE_RETURN	343
#endif

#if defined(posixSHELL)
#  define POSIX_SHELL_DELETE	263
#endif

#if defined(hpux)
#  define HPUX_DELETE		263
#endif

#if defined(AIX)
#  define AIX_RETURN		KEY_ENTER
#  define AIX_DELETE		KEY_BACKSPACE
#endif

#if defined(__CYGWIN__)
#  define CYGWIN_DELETE         263
#endif

#if defined(linux)
#  define LINUX_DELETE          263
#endif

#if defined(__MINGW32__)
#  define MINGW32_DELETE        263
#endif

/******************************************************************************
* LINEdrawingCHAR.
* If the ACS_* definitions are changed, this macro may also have to be changed.
******************************************************************************/

#if defined(COWui)
#  define LINEdrawingCHAR(chr) \
((uChar) ACS_PLUS <= (uChar) chr && (uChar) chr <= (uChar) ACS_LLCORNER)
#endif

/******************************************************************************
* Attributes.
******************************************************************************/

#define REVERSEbit		1
#define BOLDbit			0
#define BLINKINGbit		4

/******************************************************************************
* Reset levels.
******************************************************************************/

#define HARD_			2
#define SOFT_			1
#define UPDATE_			0

/******************************************************************************
* StatusOk (macro) [and StatusBad].
******************************************************************************/

#if defined(SMGui)
#  define StatusOk(status) ((status & 1 == 1) ? TRUE : FALSE)
#endif

#if defined(CURSESui)
#  define StatusOk(status) ((status != ERR) ? TRUE : FALSE)
#endif

#define StatusBad !StatusOk

/******************************************************************************
* Windowing structures.
******************************************************************************/

#if defined(COWui)
typedef struct COWvdStruct {	/* COW "virtual display" structure. */
  short nRows;			/* Number of rows in this window. */
  short nCols;			/* Number of columns in this window. */
  short atRowN;			/* Row number on pasteboard at which upper
				   left character position of this window
				   is located. */
  short atColN;			/* Column number on pasteboard at which upper
				   left character position of this window
				   is located. */
  char *chars;			/* Characters currently in this window.  The
				   upper left character is first and the lower
				   right character is last. */
  char *attrs;			/* Attributes for each corresponding
				   character. */
} COWvd;
#endif

#if defined(SMGui)
  typedef uLongx LocalId;		/* SMG virtual display identifier. */
#endif
#if defined(CURSESui)
  typedef WINDOW *LocalId;		/* Pointer to Curses window. */
#endif
#if defined(COWui)
  typedef COWvd *LocalId;		/* Pointer to COW "virtual display"
					   structure. */
#endif

typedef struct WINDstruct {
  LocalId id;			/* "Window" identifier for windowing system
				    being used. */
  Logical pasted;		/* TRUE: window is pasted (visible). */
  Logical bordered;		/* TRUE: border around window. */
  struct WINDstruct *next;	/* Next window on linked list. */
} WIND;

/******************************************************************************
* WINDOWid.
* Window identifier.  Called a window in CURSES and on COW system (Macintosh
* and Windows).  Called a virtual display in SMG.
******************************************************************************/

typedef WIND *WINDOWid;

/******************************************************************************
* Keys.
******************************************************************************/

#define KB_0		'0'
#define KB_1		'1'
#define KB_2		'2'
#define KB_3		'3'
#define KB_4		'4'
#define KB_5		'5'
#define KB_6		'6'
#define KB_7		'7'
#define KB_8		'8'
#define KB_9		'9'

#define KB_a		'a'
#define KB_b		'b'
#define KB_c		'c'
#define KB_d		'd'
#define KB_e		'e'
#define KB_f		'f'
#define KB_g		'g'
#define KB_h		'h'
#define KB_i		'i'
#define KB_j		'j'
#define KB_k		'k'
#define KB_l		'l'
#define KB_m		'm'
#define KB_n		'n'
#define KB_o		'o'
#define KB_p		'p'
#define KB_q		'q'
#define KB_r		'r'
#define KB_s		's'
#define KB_t		't'
#define KB_u		'u'
#define KB_v		'v'
#define KB_w		'w'
#define KB_x		'x'
#define KB_y		'y'
#define KB_z		'z'

#define KB_A		'A'
#define KB_B		'B'
#define KB_C		'C'
#define KB_D		'D'
#define KB_E		'E'
#define KB_F		'F'
#define KB_G		'G'
#define KB_H		'H'
#define KB_I		'I'
#define KB_J		'J'
#define KB_K		'K'
#define KB_L		'L'
#define KB_M		'M'
#define KB_N		'N'
#define KB_O		'O'
#define KB_P		'P'
#define KB_Q		'Q'
#define KB_R		'R'
#define KB_S		'S'
#define KB_T		'T'
#define KB_U		'U'
#define KB_V		'V'
#define KB_W		'W'
#define KB_X		'X'
#define KB_Y		'Y'
#define KB_Z		'Z'

#define KB_PLUS		'+'
#define KB_MINUS	'-'

#define KB_CTRL_at	0		/* At sign (@).  The NUL character. */
#define KB_CTRL_A	1
#define KB_CTRL_B	2
#define KB_CTRL_C	3
#define KB_CTRL_D	4
#define KB_CTRL_E	5
#define KB_CTRL_F	6
#define KB_CTRL_G	7
#define KB_CTRL_H	8
#define KB_CTRL_I	9
#define KB_CTRL_J	10
#define KB_CTRL_K	11
#define KB_CTRL_L	12
#define KB_CTRL_M	13
#define KB_CTRL_N	14
#define KB_CTRL_O	15
#define KB_CTRL_P	16
#define KB_CTRL_Q	17
#define KB_CTRL_R	18
#define KB_CTRL_S	19
#define KB_CTRL_T	20
#define KB_CTRL_U	21
#define KB_CTRL_V	22
#define KB_CTRL_W	23
#define KB_CTRL_X	24
#define KB_CTRL_Y	25
#define KB_CTRL_Z	26
#define KB_ESCAPE	27		/* Left bracket ([). */
#define KB_CTRL_28	28		/* Back slash (\). */
#define KB_CTRL_29	29		/* Right bracket (]). */
#define KB_CTRL_30	30		/* Circumflex accent (^). */
#define KB_CTRL_31	31		/* Underscore (_). */

#define KB_TAB		KB_CTRL_I
#define KB_RETURN	KB_CTRL_M

#if defined(dos) || defined(mac) || defined(win32) || defined(__MINGW32__)
#  define KB_DELETE	KB_CTRL_H
#else
#  define KB_DELETE	127
#endif

#if defined(CURSESui)
#  define KB_UPARROW      KEY_UP
#  define KB_DOWNARROW    KEY_DOWN
#  define KB_LEFTARROW    KEY_LEFT
#  define KB_RIGHTARROW   KEY_RIGHT
#endif

#if defined(SMGui)
#  define KB_UPARROW      0422
#  define KB_DOWNARROW    0423
#  define KB_LEFTARROW    0424
#  define KB_RIGHTARROW   0425
#endif

#if defined(COWui)
#  define KB_UPARROW	KB_CTRL_30
#  define KB_DOWNARROW	KB_CTRL_31
#  define KB_LEFTARROW	KB_CTRL_28
#  define KB_RIGHTARROW	KB_CTRL_29
#endif

/******************************************************************************
* Renditions (video attributes), cursor modes, border modes, etc.
******************************************************************************/

#define NORMAL          0
#define BOLD            1
#define REVERSE         2
#define REVERSE1        4       /* Different from REVERSE only on IBM PC. */
#define REVERSE2        8       /* Different from REVERSE only on IBM PC. */
#define BLINKING        16
#define BLACK           32      /* For erasing windows on IBM PC. */

#define BORDER          TRUE
#define NOBORDER        FALSE

#define CURSORon        TRUE
#define CURSORoff       FALSE

#define ERASE           TRUE
#define NOERASE         FALSE

#define PASSTHRUri	1
#define TOUPPERri	2
#define TOLOWERri	3

/******************************************************************************
* Function prototypes.
******************************************************************************/

int begin_pasteboard_update PROTOARGs((void));
int change_rendition PROTOARGs((WINDOWid, int, int, int, int, int));
int create_pasteboard PROTOARGs((void));
int create_virtual_display PROTOARGs((int, int, WINDOWid *, int, int));
int delete_pasteboard PROTOARGs((int));
int delete_virtual_display PROTOARGs((WINDOWid));
int draw_horizontal_line PROTOARGs((WINDOWid, int, int, int, int, Logical));
int draw_vertical_line PROTOARGs((WINDOWid, int, int, int, int, Logical));
int draw_rectangle PROTOARGs((WINDOWid, int, int, int, int, int));
int end_pasteboard_update PROTOARGs((void));
int erase_display PROTOARGs((WINDOWid, int, int, int, int));
int input_field PROTOARGs((WINDOWid, char *, int, int, int, int *, int *,
			   int, int, int, int, int, int));
int inq_cursor_mode PROTOARGs((int *));
int label_border PROTOARGs((WINDOWid, char *, int));
int paste_virtual_display PROTOARGs((WINDOWid, int, int));
int put_chars PROTOARGs((WINDOWid, char *, int, int, int, int, int));
int read_display PROTOARGs((WINDOWid, int, char *));
#if defined(CURSESui)
  int read_input PROTOARGs((WINDOWid, int *, int, Logical));
#else
  int read_input PROTOARGs((int *, int, Logical));
#endif
int repaint_screen PROTOARGs((void));
int repaste_virtual_display PROTOARGs((WINDOWid, int, int));
int ring_bell PROTOARGs((void));
int set_cursor_abs PROTOARGs((WINDOWid, int, int));
int set_cursor_mode PROTOARGs((int));
int unpaste_virtual_display PROTOARGs((WINDOWid));
int zzzzz PROTOARGs((double));

void EncodeKeyDefinitions VARPROTOARGs((int nLines, char **lineS, ...));

/*****************************************************************************/

#endif
