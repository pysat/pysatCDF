/******************************************************************************
* Copyright 1996-2013 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                   Toolbox of routines for CDF Toolkit (Win32).
*
*  Version 1.0, 13-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  13-Nov-97, J Love	Original version.
*
******************************************************************************/
#if !defined(ALONE)

#define TOOLBOX4
#include "cdftools.h"

#include "stdafx.h"

#if defined(SO)
#include "CDFxyzDoc.h"
extern CCDFxyzDoc *pTheDoc;
#endif

#if defined(FSI)
#include "CDFfsiDoc.h"
extern CCDFfsiDoc *pTheDoc;
#endif

/******************************************************************************
* CheckForAbortSOwin32.
******************************************************************************/

#if defined(SO)
void CheckForAbortSOwin32 () {
  for (;;) {
     MSG msg;
	 if (!PeekMessage(&msg,NULL,0,0,PM_REMOVE)) break;
	 DispatchMessage (&msg);
  }
  return;
}
#endif

/******************************************************************************
* WriteOutWin32.
******************************************************************************/

#if defined(SO)
void WriteOutWin32 (char *text) {
  pTheDoc->AddText (text);
  CheckForAbortSOwin32 ();
  return;
}
#endif

/******************************************************************************
* TransferTextAttrs.
******************************************************************************/

#if defined(FSI)
void TransferTextAttrs (char *text, char *attrs) {
  pTheDoc->XferTextAttrs (text, attrs);
  return;
}
#endif

/******************************************************************************
* SetCursorPosition.
******************************************************************************/

#if defined(FSI)
void SetCursorPosition (int row, int col) {
  pTheDoc->CursorPos (row, col);
  return;
}
#endif

/******************************************************************************
* WinMessageDialog.
******************************************************************************/

void WinMessageDialog (char *severity, char *msg) {
  CString text(severity);
  text += "\n";
  text += msg;
  AfxMessageBox (text, MB_OK);
  return;
}

/******************************************************************************
* DebugMessage.
******************************************************************************/

void DebugMessage (char *text) {
  AfxMessageBox (text, MB_OK);
  return;
}

/******************************************************************************
* BuildArgcArgv.
******************************************************************************/

#if 0
void BuildArgcArgv (char *name, const char *qop, int *argC, char ***argV) {
  const char *ptr1; const char *ptr2; int len;
  *argC = 1;
  *argV = (char **) malloc (*argC * sizeof(char *));
  (*argV)[*argC-1] = (char *) malloc (strlen(name) + 1);
  strcpy ((*argV)[*argC-1], name);
  ptr1 = qop;
  for (;;) {
     ptr1 += strspn (ptr1, " ");
     if (*ptr1 == NUL) break;
     switch (*ptr1) {
       case '"':
       case '\'':
	 ptr2 = strchr(ptr1+1,*ptr1);
	 if (ptr2 == NULL) return;		/* Error */
	 (*argC)++;
	 (*argV) = (char **) realloc (*argV, *argC * sizeof(char *));
	 len = (ptr2 - 1) - (ptr1 + 1) + 1;
	 (*argV)[*argC-1] = (char *) malloc (len + 1);
	 strncpy ((*argV)[*argC-1], ptr1 + 1, len);
	 (*argV)[*argC-1][len] = NUL;
	 ptr1 = ptr2 + 1;
	 break;
       default:
	 ptr2 = strchr(ptr1,' ');
	 if (ptr2 == NULL) ptr2 = ptr1 + strlen(ptr1);
	 (*argC)++;
	 (*argV) = (char **) realloc (*argV, *argC * sizeof(char *));
	 len = (ptr2 - 1) - ptr1 + 1;
	 (*argV)[*argC-1] = (char *) malloc (len + 1);
	 strncpy ((*argV)[*argC-1], ptr1, len);
	 (*argV)[*argC-1][len] = NUL;
	 ptr1 = ptr2;
	 break;
     }
  }
  return;
}
#endif

#endif
