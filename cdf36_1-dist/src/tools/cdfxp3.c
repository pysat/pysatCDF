/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF						CDFexport - Macintosh.
*
*  Version 1.0, 15-Jan-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  15-Jan-97, J Love     Original version.
*
******************************************************************************/

#include "cdfxp.h"

/******************************************************************************
* ExportQOPs.
* Returns TRUE if execution should continue.
******************************************************************************/

#if defined(mac)
Logical ExportQOPs (argC, argV)
int *argC;
char **argV[];
{
  DialogPtr dialogP;
  DialogRecord dRecord;
  WindowPtr behind = (WindowPtr) -1;
  ControlHandle controlHs[MAXIMUMin+1];
  Rect iRect;
#ifdef __MWERKS__
  ModalFilterUPP FilterDialogQOPfsiUPP;
  FileFilterUPP FilterForCDFsUPP;
  UserItemUPP OutlineDefaultButtonUPP;
#endif
  short itemN, iType, i;

  static Logical simpleMode = DEFAULTsimpleEXPORT;
  static Logical negToPos = DEFAULT_NEGtoPOSfp0;
  static Logical showStatistics = DEFAULTstatsEXPORT;
  static Logical useFilters;
  static Logical useFills;
  static Logical useFormat;
  static Logical useFillval;
  static Logical useValidmin;
  static Logical useValidmax;
  static Logical useMonoton;
  static Logical showRecord;
  static Logical showIndices;
  static Logical exclusiveFilters;
  static Logical outputItems;
  static Logical deleteExisting;
  static Logical preAllocate;
  static Logical showHeadings;
  static int zMode;
  static int newFormat;
  static int newEncoding;
  static int majority;
  static int orientation;
  static int epochStyle;
  static Str255 cacheText = "\p";
  static Str255 CDFtext = "\p";

  /****************************************************************************
  * Depending on simple mode...
  ****************************************************************************/

  if (simpleMode) {
    useFilters = FORCEeachFilterSIMPLE;
    useFills = FORCEfillsSIMPLE;
    useFormat = DEFAULTformatSIMPLE;
    useFillval = FORCEfillvalSIMPLE;
    useValidmin = FORCEvalidminSIMPLE;
    useValidmax = FORCEvalidmaxSIMPLE;
    useMonoton = FORCEmonotonSIMPLE;
    showRecord = DEFAULTrecordSIMPLE;
    showIndices = DEFAULTindicesSIMPLE;
    exclusiveFilters = NAexclusiveSIMPLE;
    showHeadings = DEFAULTheadingSIMPLE;
    outputItems = DEFAULToutputSIMPLE;
    deleteExisting = NAdeleteSIMPLE;
    preAllocate = NApreAllocateSIMPLE;
    zMode = DEFAULTzModeSIMPLE;
    newFormat = BOO(NAsingleSIMPLE,0,1);
    newEncoding = BOO(NAnetworkSIMPLE,1,0);
    majority = (int) DEFAULTmajoritySIMPLE;
    orientation = BOO(DEFAULThorizontalSIMPLE,0,1);
    epochStyle = DEFAULTepochSIMPLE;
  }
  else {
    useFilters = DEFAULTeachFilterEXPORT;
    useFills = DEFAULTfillsEXPORT;
    useFormat = DEFAULTformatEXPORT;
    useFillval = DEFAULTfillvalEXPORT;
    useValidmin = DEFAULTvalidminEXPORT;
    useValidmax = DEFAULTvalidmaxEXPORT;
    useMonoton = DEFAULTmonotonEXPORT;
    showRecord = DEFAULTrecordEXPORT;
    showIndices = DEFAULTindicesEXPORT;
    exclusiveFilters = DEFAULTexclusiveEXPORT;
    showHeadings = DEFAULTheadingEXPORT;
    outputItems = DEFAULToutputEXPORT;
    deleteExisting = DEFAULTdeleteEXPORT;
    preAllocate = DEFAULTpreAllocateEXPORT;
    zMode = DEFAULTzModeEXPORT;
    newFormat = BOO(DEFAULTsingleEXPORT,0,1);
    newEncoding = BOO(DEFAULTnetworkEXPORT,1,0);
    majority = (int) DEFAULTmajorityEXPORT;
    orientation = BOO(DEFAULThorizontalEXPORT,0,1);
    epochStyle = DEFAULTepochEXPORT;
  }

  /****************************************************************************
  * Create the dialog and get the control handles.
  ****************************************************************************/

  dialogP = GetNewDialog (QOPri, &dRecord, behind);
  
  for (itemN = 1; itemN <= MAXIMUMin; itemN++) {
     GetDItem (dialogP, itemN, &iType, (Handle *) &controlHs[itemN], &iRect);
  }

  /****************************************************************************
  * Set the control values.
  ****************************************************************************/

  if (simpleMode) SetCtlValue (controlHs[SIMPLEin], 1);
  if (negToPos) SetCtlValue (controlHs[NEGZin], 1);
  if (useFilters) SetCtlValue (controlHs[FILTERin], 1);
  if (useFills) SetCtlValue (controlHs[FILLSin], 1);
  if (useFormat) SetCtlValue (controlHs[FORMATin], 1);
  if (useFillval) SetCtlValue (controlHs[FILLVALin], 1);
  if (useValidmin) SetCtlValue (controlHs[VALIDMINin], 1);
  if (useValidmax) SetCtlValue (controlHs[VALIDMAXin], 1);
  if (useMonoton) SetCtlValue (controlHs[MONOTONin], 1);
  if (showRecord) SetCtlValue (controlHs[RECORDin], 1);
  if (showIndices) SetCtlValue (controlHs[INDICESin], 1);
  if (exclusiveFilters) SetCtlValue (controlHs[EXCLUSIVEin], 1);
  if (showStatistics) SetCtlValue (controlHs[STATSin], 1);
  if (showHeadings) SetCtlValue (controlHs[HEADSin], 1);
  if (outputItems) SetCtlValue (controlHs[OUTPUTin], 1);
  if (deleteExisting) SetCtlValue (controlHs[DELETEin], 1);
  if (preAllocate) SetCtlValue (controlHs[PREALLOCATEin], 1);
  SetCtlValue (controlHs[zMODEinBASE+zMode], 1);
  SetCtlValue (controlHs[FORMATinBASE+newFormat], 1);
  SetCtlValue (controlHs[ENCODINGinBASE+newEncoding], 1);
  SetCtlValue (controlHs[MAJORITYinBASE+majority], 1);
  SetCtlValue (controlHs[EPOCHinBASE+epochStyle], 1);
  SetCtlValue (controlHs[ORIENTinBASE+orientation], 1);
  SetIText ((Handle) controlHs[CDFTEXTin], CDFtext);
  SetIText ((Handle) controlHs[CACHEin], cacheText);

#ifndef __MWERKS__
  SetDItem (dialogP, (short) ODBin, (short) userItem,
	    (Handle) OutlineDefaultButton, &iRect);
#else
  OutlineDefaultButtonUPP = NewUserItemProc (OutlineDefaultButton);
  SetDItem (dialogP, (short) ODBin, (short) userItem,
	    (Handle) OutlineDefaultButtonUPP, &iRect);
#endif

  /****************************************************************************
  * Display the dialog and wait for user actions.
  ****************************************************************************/
    
  ShowWindow ((WindowPtr) dialogP);
  SetCursor (ARROW_CURSOR);
#ifdef __MWERKS__
  FilterDialogQOPfsiUPP = NewModalFilterProc (FilterDialogQOPfsi);
#endif

  for (;;) {
#ifndef __MWERKS__
    ModalDialog (FilterDialogQOPfsi, &itemN);
#else
    ModalDialog (FilterDialogQOPfsiUPP, &itemN);
#endif
    switch (itemN) {
      /************************************************************************
      * Ok.
      ************************************************************************/
      case OKin: {
		int n;
		char tempS[8+8+9+10+11+11+10+9+10+12+9+9+14+7+8+7+7+11+1];
		char *zModesMAC[3] = { "0", "1", "2" };
		char *formatsMAC[2] = { ",single", ",multi" };
		char *encodingsMAC[2] = { ",host", ",network" };
		char *majoritiesMAC[3] = { "", ",row", ",column" };
		char *orientationsMAC[2] = { ",horizontal", ",vertical" };
		char *epochStylesMAC[7] = {
		  ",epoch", ",epoch1", ",epoch2", ",epoch3", ",epochf", ",epochx",
                  ",iso8601"
		};
		/**********************************************************************
		* Get the value of each control.
		**********************************************************************/

		simpleMode = GetCtlValue (controlHs[SIMPLEin]);
		negToPos = GetCtlValue (controlHs[NEGZin]);
		useFilters = GetCtlValue (controlHs[FILTERin]);
		useFills = GetCtlValue (controlHs[FILLSin]);
		useFormat = GetCtlValue (controlHs[FORMATin]);
		useFillval = GetCtlValue (controlHs[FILLVALin]);
		useValidmin = GetCtlValue (controlHs[VALIDMINin]);
		useValidmax = GetCtlValue (controlHs[VALIDMAXin]);
		useMonoton = GetCtlValue (controlHs[MONOTONin]);
		showRecord = GetCtlValue (controlHs[RECORDin]);
		showIndices = GetCtlValue (controlHs[INDICESin]);
		exclusiveFilters = GetCtlValue (controlHs[EXCLUSIVEin]);
		showStatistics = GetCtlValue (controlHs[STATSin]);
		showHeadings = GetCtlValue (controlHs[HEADSin]);
		outputItems = GetCtlValue (controlHs[OUTPUTin]);
		deleteExisting = GetCtlValue (controlHs[DELETEin]);
		preAllocate = GetCtlValue (controlHs[PREALLOCATEin]);

		for (zMode = 0; zMode < 3; zMode++) {
		   if (GetCtlValue(controlHs[zMODEinBASE+zMode])) break;
		}
		for (newFormat = 0; newFormat < 2; newFormat++) {
		   if (GetCtlValue(controlHs[FORMATinBASE+newFormat])) break;
		}
		for (newEncoding = 0; newEncoding < 2; newEncoding++) {
		   if (GetCtlValue(controlHs[ENCODINGinBASE+newEncoding])) break;
		}
		for (majority = 0; majority < 3; majority++) {
		   if (GetCtlValue(controlHs[MAJORITYinBASE+majority])) break;
		}
		for (epochStyle = 0; epochStyle < 5; epochStyle++) {
		   if (GetCtlValue(controlHs[EPOCHinBASE+epochStyle])) break;
		}
		for (orientation = 0; orientation < 2; orientation++) {
		   if (GetCtlValue(controlHs[ORIENTinBASE+orientation])) break;
		}

		GetIText ((Handle) controlHs[CDFTEXTin], CDFtext);
		GetIText ((Handle) controlHs[CACHEin], cacheText);

		/**********************************************************************
		* Build argc/argv.
		**********************************************************************/

		*argC = 8 + BOO(NULpString(CDFtext),0,1) +
				    BOO(NULpString(cacheText),0,2);
		*argV = (char **) cdf_AllocateMemory ((size_t)*argC * sizeof(char *),
										  FatalError);
	
		n = 0;
		MAKEstrARGv (argV, n, pgmName)

		if (!NULpString(CDFtext)) {
		  PtoCstr (CDFtext);
		  MAKEstrARGv (argV, n, (char *) CDFtext)
		  CtoPstr ((char *) CDFtext);
		}

		MAKEstrARGv (argV, n, "-initial");
		strcpyX (tempS, BOO(outputItems,"output","nooutput"), 0);
		strcatX (tempS, BOO(useFormat,",format",",noformat"), 0);
		strcatX (tempS, BOO(showRecord,",record",",norecord"), 0);
		strcatX (tempS, BOO(showIndices,",indices",",noindices"), 0);
		strcatX (tempS, BOO(showHeadings,",heading",",noheading"), 0);
		strcatX (tempS, majoritiesMAC[majority], 0);
		strcatX (tempS, epochStylesMAC[epochStyle], 0);
		strcatX (tempS, orientationsMAC[orientation], 0);
		if (!simpleMode) {
		  strcatX (tempS, BOO(useFilters,",filter",",nofilter"), 0);
		  strcatX (tempS, BOO(useFills,",fills",",nofills"), 0);
		  strcatX (tempS, BOO(useFillval,",fillval",",nofillval"), 0);
		  strcatX (tempS, BOO(useValidmin,",validmin",",novalidmin"), 0);
		  strcatX (tempS, BOO(useValidmax,",validmax",",novalidmax"), 0);
		  strcatX (tempS, BOO(useMonoton,",monoton",",nomonoton"), 0);
		  strcatX (tempS, BOO(exclusiveFilters,",exclusive",",noexclusive"),0);
		  strcatX (tempS, BOO(deleteExisting,",delete",",nodelete"), 0);
		  strcatX (tempS, BOO(preAllocate,",preallocate",",nopreallocate"), 0);
		  strcatX (tempS, formatsMAC[newFormat], 0);
		  strcatX (tempS, encodingsMAC[newEncoding], 0);
		}
		MAKEstrARGv (argV, n, tempS);

		MAKEstrARGv (argV, n, BOO(simpleMode,"-simple","-nosimple"));
		MAKEstrARGv (argV, n, BOO(negToPos,"-neg2posfp0","-noneg2posfp0"));
		MAKEstrARGv (argV, n, BOO(showStatistics,"-statistics",
								 "-nostatistics"));

		MAKEstrARGv (argV, n, "-zmode");
		MAKEstrARGv (argV, n, zModesMAC[zMode]);

		if (!NULpString(cacheText)) {
		  MAKEstrARGv (argV, n, "-cache")
		  PtoCstr (cacheText);
		  MAKEstrARGv (argV, n, (char *) cacheText)
		  CtoPstr ((char *) cacheText);
		}

		/**********************************************************************
		* Close the dialog and return.
		**********************************************************************/
		CloseDialog (dialogP);
#ifdef __MWERKS__
        DisposeRoutineDescriptor (FilterDialogQOPfsiUPP);
		DisposeRoutineDescriptor (OutlineDefaultButtonUPP);
#endif
		return TRUE;
      }
      /************************************************************************
      * Help.
      ************************************************************************/
      case HELPin: {
		int n;
		*argC = 2;
		*argV = (char **) cdf_AllocateMemory ((size_t)*argC * sizeof(char *), FatalError);
		n = 0;
		MAKEstrARGv (argV, n, pgmName)
		MAKEstrARGv (argV, n, "-help")
		CloseDialog (dialogP);
#ifdef __MWERKS__
        DisposeRoutineDescriptor (FilterDialogQOPfsiUPP);
		DisposeRoutineDescriptor (OutlineDefaultButtonUPP);
#endif
		return TRUE;
      }
      /************************************************************************
      * Cancel.
      ************************************************************************/
      case CANCELin:
		CloseDialog (dialogP);
#ifdef __MWERKS__
        DisposeRoutineDescriptor (FilterDialogQOPfsiUPP);
		DisposeRoutineDescriptor (OutlineDefaultButtonUPP);
#endif
		return FALSE;
      /************************************************************************
      * Select CDF specification.
      ************************************************************************/
      case CDFSELECTin: {
		StandardFileReply CDFreply;
		char CDFpath[DU_MAX_PATH_LEN+1];
#ifndef __MWERKS__
		StandardGetFile (FilterForCDFs, -1, NULL, &CDFreply);
#else
		FilterForCDFsUPP = NewFileFilterProc((ProcPtr) FilterForCDFs);
		StandardGetFile (FilterForCDFsUPP, -1, NULL, &CDFreply);
		DisposeRoutineDescriptor (FilterForCDFsUPP);
#endif
		if (CDFreply.sfGood && !CDFreply.sfIsFolder && !CDFreply.sfIsVolume) {
		  BuildMacPath (&CDFreply.sfFile, CDFpath, TRUE);
		  CDFtext[0] = strlen (CDFpath);
		  strcpyX ((char *) &CDFtext[1], CDFpath, 255);
		  SetIText ((Handle) controlHs[CDFTEXTin], CDFtext);
		}
		break;
      }
      /************************************************************************
      * Check boxes.
      ************************************************************************/
      case SIMPLEin:
      case NEGZin:
      case FILTERin:
      case FILLSin:
      case FORMATin:
      case FILLVALin:
      case VALIDMINin:
      case VALIDMAXin:
      case MONOTONin:
      case RECORDin:
      case INDICESin:
      case EXCLUSIVEin:
      case STATSin:
      case HEADSin:
      case OUTPUTin:
      case DELETEin:
      case PREALLOCATEin:
		SetCtlValue (controlHs[itemN], BOO(GetCtlValue(controlHs[itemN]),0,1));
		break;
      /************************************************************************
      * Radio buttons.
      ************************************************************************/
      case zMODEinBASE+0:
      case zMODEinBASE+1:
      case zMODEinBASE+2:
		for (i = 0; i < 3; i++) SetCtlValue (controlHs[zMODEinBASE+i], 0);
		SetCtlValue (controlHs[itemN], 1);
		break;
      case FORMATinBASE+0:
      case FORMATinBASE+1:
		for (i = 0; i < 2; i++) SetCtlValue (controlHs[FORMATinBASE+i], 0);
		SetCtlValue (controlHs[itemN], 1);
		break;
      case ENCODINGinBASE+0:
      case ENCODINGinBASE+1:
		for (i = 0; i < 2; i++) SetCtlValue (controlHs[ENCODINGinBASE+i], 0);
		SetCtlValue (controlHs[itemN], 1);
		break;
      case MAJORITYinBASE+0:
      case MAJORITYinBASE+1:
      case MAJORITYinBASE+2:
		for (i = 0; i < 3; i++) SetCtlValue (controlHs[MAJORITYinBASE+i], 0);
		SetCtlValue (controlHs[itemN], 1);
		break;
      case EPOCHinBASE+0:
      case EPOCHinBASE+1:
      case EPOCHinBASE+2:
      case EPOCHinBASE+3:
      case EPOCHinBASE+4:
      case EPOCHinBASE+5:
		for (i = 0; i < 6; i++) SetCtlValue (controlHs[EPOCHinBASE+i], 0);
		SetCtlValue (controlHs[itemN], 1);
		break;
      case ORIENTinBASE+0:
      case ORIENTinBASE+1:
		for (i = 0; i < 2; i++) SetCtlValue (controlHs[ORIENTinBASE+i], 0);
		SetCtlValue (controlHs[itemN], 1);
		break;
    }
  }
}
#endif
