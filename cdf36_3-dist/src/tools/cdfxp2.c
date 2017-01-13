/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                                    CDFexport/2.
*
*  Version 1.2b, 2-Sep-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  15-Sep-95, J Love     Original version.
*   V1.0a 18-Sep-95, J Love	Macintosh event handling.
*   V1.0b 22-Sep-95, J Love	INITIALRECS/ALLOCATERECS in `AllocateRecords'.
*   V1.1   9-Sep-96, J Love	CDF V2.6.
*   V1.2  15-Nov-96, J Love	Added `simple' mode and batch mode.
*   V1.2a  8-Jan-96, J Love	Changed settings file messages.
*   V1.2b  2-Sep-97, J Love	Fixed `inclusive' filtering.
*   V3.3  10-Jan-09, M Liu      Added the missing "GZIP" compression.
*   V3.3  04-Apr-11, M Liu      Ignored the char case in EPOCH_STYLE field.
*
******************************************************************************/

#include "cdfxp.h"

Logical StrStrIgCaseX (char *string, char *chkstring);

/******************************************************************************
* Local macros.
******************************************************************************/

#define SETTINGS_DELIM		'"'

#define COUNTx(max,first,last) \
BOO(max < first,0,BOO(max < last,max-first+1,last-first+1));

/******************************************************************************
* Local function prototypes.
******************************************************************************/

static Logical SetFilter PROTOARGs((char *field, struct ItemStruct *Item));
static Logical SetOutput PROTOARGs((char *field, struct ItemStruct *Item));
static Logical SetWidth PROTOARGs((char *field, struct ItemStruct *Item));
static Logical SetBlocking PROTOARGs((char *field, struct VarStruct *Var));
static Logical SetMonotonic PROTOARGs((char *field, struct VarStruct *Var));
static Logical SetSparseness PROTOARGs((char *field, struct VarStruct *Var));
static Logical SetCompression PROTOARGs((char *field, struct VarStruct *Var));
static Logical ScanValueField PROTOARGs((
  char *fieldPtr, long dataType, long numElems, char **commaPtr, void **value
));
static void WriteDelimitedString PROTOARGs((
  FILE *fp, int delim, char *string
));
static char *EndingDelimiter PROTOARGs((char *string));
static void RemoveDelimiters PROTOARGs((char *string));
static Logical AllocateRecords PROTOARGs((
  CDFid outID, struct ItemStruct *scalarHead, struct ItemStruct *hyperHead
));
static CDFstatus BuildWalkScreen PROTOARGs((
  struct ItemStruct *exportHead, long recordN, long numDims, long indices[],
  char *lineS[], int lineNs[], int colS[], int lenS[], struct VarStruct *Vars[]
));
static Logical PromptForRecordNumber PROTOARGs((long *recordN, long maxRec));
static Logical PromptForDimensionIndex PROTOARGs((long *index, long dimSize));
static Logical PromptForVariableSearch PROTOARGs((
  struct VarStruct *Var, int *searchType, void **searchValue
));
static CDFstatus SearchForVariableValue PROTOARGs((
  struct VarStruct *Var, long numDims, long dimSizes[], long *recordAt,
  long indicesAt[], long maxRec, int searchType, void *searchValue
));
static Logical AbortSearch PROTOARGs((void));

/******************************************************************************
* SaveSettings.
******************************************************************************/

void SaveSettings () {
  FILE *fp; long version, release, increment; char subIncr;
  struct ItemStruct *Item; CDFstatus status; int style;
  status = CDFlib (GET_, LIB_VERSION_, &version,
			 LIB_RELEASE_, &release,
			 LIB_INCREMENT_, &increment,
			 LIB_subINCREMENT_, &subIncr,
		   NULL_);
  DisplayStatus (status, "inquiring CDF");
  if (StatusBAD(status)) return;
  fp = fopen (settingsFile, "w");
  if (fp == NULL) {
    DisplayMessage ("Error opening settings file.", BEEPWAIT1);
    return;
  }
  if (subIncr != ' ')
    fprintf (fp, "CDF V%ld.%ld.%ld_%c%s\n", version, release, increment,
             subIncr, BOO(simpleMode," (SimpleMode)",""));
  else
    fprintf (fp, "CDF V%ld.%ld.%ld%s\n", version, release, increment,
             BOO(simpleMode," (SimpleMode)",""));
  for (Item = itemHead; Item != NULL; Item = Item->nextItem) {
     switch (Item->type) {
       case RECORDt:
	 fprintf (fp, "ITEM=Record,");
	 fprintf (fp, "%s,", BOO(Item->outputSetting,"YES","no"));
	 if (!simpleMode) {
	   if (Item->Record->min == NOminMax)
	     fprintf (fp, ",");
	   else
	     fprintf (fp, "%ld,", Item->Record->min + 1);
	   if (Item->Record->max == NOminMax)
	     fprintf (fp, ",");
	   else
	     fprintf (fp, "%ld,", Item->Record->max + 1);
	   fprintf (fp, "%s,", BOO(Item->filterSetting,
				   BOO(opt.exclusive,
				       BOO(Item->inclusive,"YeS","yEs"),
				       "YES"),
				   "no"));
	   fprintf (fp, "%d,", Item->width);
	 }
	 fprintf (fp, "\n");
	 break;
       case INDICESt:
	 fprintf (fp, "ITEM=Indices,");
	 fprintf (fp, "%s,", BOO(Item->outputSetting,"YES","no"));
	 if (!simpleMode) {
	   if (Item->Indices->minNumDims == NOminMax)
	     fprintf (fp, ",");
	   else {
	     char indices[MAXitemFieldLEN+1];
	     EncodeIndicesJustify (indices, Item->Indices->minNumDims,
				   Item->Indices->minIndices, 0,
				   (size_t) sizeof(indices));
	     fprintf (fp, "%s,", indices);
	   }
	   if (Item->Indices->maxNumDims == NOminMax)
	     fprintf (fp, ",");
	   else {
	     char indices[MAXitemFieldLEN+1];
	     EncodeIndicesJustify (indices, Item->Indices->maxNumDims,
				   Item->Indices->maxIndices, 0,
				   (size_t) sizeof(indices));
	     fprintf (fp, "%s,", indices);
	   }
	   fprintf (fp, "%s,", BOO(Item->filterSetting,
				   BOO(opt.exclusive,
				       BOO(Item->inclusive,"YeS","yEs"),
				       "YES"),
				   "no"));
	   fprintf (fp, "%d,", Item->width);
	 }
	 fprintf (fp, "\n");
	 break;
       case VARIABLEt:
         if (TT2000dataType(Item->Var->dataType)) style = TT2000_3_STYLE;
         else style = opt.epochStyle;
	 fprintf (fp, "ITEM=Variable,");
	 WriteDelimitedString (fp, (int) SETTINGS_DELIM, Item->Var->name);
	 fprintf (fp, ",");
	 fprintf (fp, "%s,", BOO(Item->outputSetting,"YES","no"));
	 if (!simpleMode) {
	   if (Item->Var->min != NULL) {
	     if (STRINGdataType(Item->Var->dataType))
	       WriteDelimitedString (fp, (int) SETTINGS_DELIM,
				     (char *) Item->Var->min);
	     else {
	       char temp[MAX_nonSTRING_VALUE_LEN+1];
               if ((Item->Var->dataType == CDF_FLOAT) ||
		   (Item->Var->dataType == CDF_REAL4)) {
                 if (!isnan((double)*(float *)Item->Var->min) &&
                     *(float *)Item->Var->min <= DEFAULT_FLOAT_PADVALUE) {
                   EncodeValueFormat (Item->Var->dataType, Item->Var->min, temp,
                                      NULL, 0, MAX_nonSTRING_VALUE_LEN, style,
                                      (size_t) sizeof(temp));
                 } else
	           EncodeValueFormat (BOO(EPOCHdataType(Item->Var->dataType),
				      CDF_REAL8,Item->Var->dataType),
				      Item->Var->min, temp,
				      BOO(EPOCHdataType(Item->Var->dataType),
				      "%.13e",NULL),
				      0, MAX_nonSTRING_VALUE_LEN, style,
				      (size_t) sizeof(temp));
               } else if ((Item->Var->dataType == CDF_DOUBLE) ||
		          (Item->Var->dataType == CDF_REAL8)) {
                 if (!isnan(*(double *)Item->Var->min) &&
                     *(double *)Item->Var->min <= DEFAULT_DOUBLE_PADVALUE) {
                   EncodeValueFormat (Item->Var->dataType, Item->Var->min, temp,
                                      NULL, 0, MAX_nonSTRING_VALUE_LEN, style,
                                      (size_t) sizeof(temp));
                 } else
	           EncodeValueFormat (BOO(EPOCHdataType(Item->Var->dataType),
				      CDF_REAL8,Item->Var->dataType),
				      Item->Var->min, temp,
				      BOO(EPOCHdataType(Item->Var->dataType),
				      "%.13e",NULL),
				      0, MAX_nonSTRING_VALUE_LEN, style,
				      (size_t) sizeof(temp));
               } else
	         EncodeValueFormat (BOO(EPOCHdataType(Item->Var->dataType),
				    CDF_REAL8,Item->Var->dataType),
				    Item->Var->min, temp,
				    BOO(EPOCHdataType(Item->Var->dataType),
				    "%.13e",NULL),
				    0, MAX_nonSTRING_VALUE_LEN, style,
				    (size_t) sizeof(temp));
	       fprintf (fp, "[%s]", temp);
	     }
	   }
	   fprintf (fp, ",");
	   if (Item->Var->max != NULL) {
	     if (STRINGdataType(Item->Var->dataType))
	       WriteDelimitedString (fp, (int) SETTINGS_DELIM,
				     (char *) Item->Var->max);
	     else {
	       char temp[MAX_nonSTRING_VALUE_LEN+1];
               if ((Item->Var->dataType == CDF_FLOAT) ||
                   (Item->Var->dataType == CDF_REAL4)) {
                 if (!isnan((double)*(float *)Item->Var->max) &&
                     *(float *)Item->Var->max <= DEFAULT_FLOAT_PADVALUE) {
                   EncodeValueFormat (Item->Var->dataType, Item->Var->max, temp,
                                      NULL, 0, MAX_nonSTRING_VALUE_LEN, style,
                                      (size_t) sizeof(temp));
                 } else
	           EncodeValueFormat (BOO(EPOCHdataType(Item->Var->dataType),
				      CDF_REAL8,Item->Var->dataType),
				      Item->Var->max, temp,
				      BOO(EPOCHdataType(Item->Var->dataType),
				      "%.13e",NULL),
				      0, MAX_nonSTRING_VALUE_LEN, style,
				      (size_t) sizeof(temp));
               } else if ((Item->Var->dataType == CDF_DOUBLE) || 
                          (Item->Var->dataType == CDF_REAL8)) {
                 if (!isnan(*(double *)Item->Var->max) &&
                     *(double *)Item->Var->max <= DEFAULT_DOUBLE_PADVALUE) {
                   EncodeValueFormat (Item->Var->dataType, Item->Var->max, temp,
                                      NULL, 0, MAX_nonSTRING_VALUE_LEN, style,
                                      (size_t) sizeof(temp));
                 } else
	           EncodeValueFormat (BOO(EPOCHdataType(Item->Var->dataType),
				      CDF_REAL8,Item->Var->dataType),
				      Item->Var->max, temp,
				      BOO(EPOCHdataType(Item->Var->dataType),
				      "%.13e",NULL),
				      0, MAX_nonSTRING_VALUE_LEN, style,
				      (size_t) sizeof(temp));
               } else
	         EncodeValueFormat (BOO(EPOCHdataType(Item->Var->dataType),
				    CDF_REAL8,Item->Var->dataType),
				    Item->Var->max, temp,
				    BOO(EPOCHdataType(Item->Var->dataType),
				    "%.13e",NULL),
				    0, MAX_nonSTRING_VALUE_LEN, style,
				    (size_t) sizeof(temp));
	       fprintf (fp, "[%s]", temp);
	     }
	   }
	   fprintf (fp, ",");
	   fprintf (fp, "%s,", BOO(Item->filterSetting,
				   BOO(opt.exclusive,
				       BOO(Item->inclusive,"YeS","yEs"),
				       "YES"),
				   "no"));
	   if (Item->Var->fill != NULL) {
	     if (STRINGdataType(Item->Var->dataType))
	       WriteDelimitedString (fp, (int) SETTINGS_DELIM,
				     (char *) Item->Var->fill);
	     else {
	       char temp[MAX_nonSTRING_VALUE_LEN+1];
               if ((Item->Var->dataType == CDF_FLOAT) ||
                   (Item->Var->dataType == CDF_REAL4)) {
                 if (!isnan((double)*(float *)Item->Var->fill) &&
                     *(float *)Item->Var->fill <= DEFAULT_FLOAT_PADVALUE) {
                   EncodeValueFormat (Item->Var->dataType, Item->Var->fill, temp,
                                      NULL, 0, MAX_nonSTRING_VALUE_LEN, style,
                                      (size_t) sizeof(temp));
                 } else
	           EncodeValueFormat (BOO(EPOCHdataType(Item->Var->dataType),
				      CDF_REAL8,Item->Var->dataType),
				      Item->Var->fill, temp,
				      BOO(EPOCHdataType(Item->Var->dataType),
				      "%.13e",NULL),
				      0, MAX_nonSTRING_VALUE_LEN, style,
				      (size_t) sizeof(temp));
               } else if ((Item->Var->dataType == CDF_DOUBLE) ||
                          (Item->Var->dataType == CDF_REAL8)) {
                 if (!isnan(*(double *)Item->Var->fill) &&
                     *(double *)Item->Var->fill <= DEFAULT_DOUBLE_PADVALUE) {
                   EncodeValueFormat (Item->Var->dataType, Item->Var->fill, temp,
                                      NULL, 0, MAX_nonSTRING_VALUE_LEN, style,
                                      (size_t) sizeof(temp));
                 } else
	           EncodeValueFormat (BOO(EPOCHdataType(Item->Var->dataType),
				      CDF_REAL8,Item->Var->dataType),
				      Item->Var->fill, temp,
				      BOO(EPOCHdataType(Item->Var->dataType),
				      "%.13e",NULL),
				      0, MAX_nonSTRING_VALUE_LEN, style,
				      (size_t) sizeof(temp));
               } else
	         EncodeValueFormat (BOO(EPOCHdataType(Item->Var->dataType),
				    CDF_REAL8,Item->Var->dataType),
				    Item->Var->fill, temp,
				    BOO(EPOCHdataType(Item->Var->dataType),
				    "%.13e",NULL),
				    0, MAX_nonSTRING_VALUE_LEN, style,
				    (size_t) sizeof(temp));
	       fprintf (fp, "[%s]", temp);
	     }
	   }
	   fprintf (fp, ",");
	   fprintf (fp, "%s,", monos[Item->Var->monotonic+1]);
	   if (Item->Var->format != NULL) {
	     WriteDelimitedString (fp, (int)SETTINGS_DELIM, Item->Var->format);
	   }
	   fprintf (fp, ",");
	   fprintf (fp, "%d,", Item->width);
	   fprintf (fp, "%s,", SparsenessToken(Item->Var->sRecordsType,
					       Item->Var->sArraysType,
					       Item->Var->sArraysParms));
	   fprintf (fp, "%s,", CompressionToken(Item->Var->cType,
					        Item->Var->cParms));
	   fprintf (fp, "%ld,", Item->Var->blocking);
	 }
	 fprintf (fp, "\n");
	 break;
     }
  }
  if (!simpleMode) {
    fprintf (fp, "USE_FILTERS=%s\n", BOO(opt.overallFilter,"YES","no"));
    fprintf (fp, "USE_FILLS=%s\n", BOO(opt.useFills,"YES","no"));
    fprintf (fp, "CDF_FORMAT=%s\n", BOO(opt.singleFile,"single","multi"));
    fprintf (fp, "CDF_ENCODING=%s\n", encodings[(int)opt.encoding]);
    if (CDFcType != NO_COMPRESSION)
      fprintf (fp, "CDF_COMPRESSION=%s\n",CompressionToken(CDFcType, CDFcParms));
    if (CDFchecksum != NO_CHECKSUM) 
      fprintf (fp, "CDF_CHECKSUM=%s\n",ChecksumToken(CDFchecksum));
    fprintf (fp, "EPOCH_STYLE=%s\n", epochStyles[opt.epochStyle]);
    fprintf (fp, "ORIENTATION=%s\n",
	     BOO(opt.horizontalMode,"horizontal","vertical"));
    fprintf (fp, "MAJORITY=%s\n", majorities[(int)opt.majority]);
    fprintf (fp, "SHOW_FILTERED=%s\n", BOO(opt.showFiltered,"YES","no"));
    fprintf (fp, "SPACING=%d\n", opt.spacing);
    fprintf (fp, "DELETE_EXISTING=%s\n", BOO(opt.deleteExisting,"YES","no"));
    fprintf (fp, "PREALLOCATE=%s\n", BOO(opt.preAllocate,"YES","no"));
    fprintf (fp, "HEADING=%s\n", BOO(opt.textHeading,(poundedheading==1?
                                     "POUNDEDHEADING":"YES"),"no"));
  }
  fclose (fp);
  return;
}

/******************************************************************************
* RestoreSettings.
******************************************************************************/

void RestoreSettings () {
  char *equalPtr, *newlinePtr, *commaPtr, *fieldPtr, *linePtr, *delimPtr;
  FILE *fp; long valueL; struct ItemStruct *Item;
  char line[MAX_SETTINGS_LEN+1+1];              /* +1+1 for newline+NUL. */
  if (!IsReg(settingsFile)) {
    DisplayMessage ("Settings file does not exist.", BEEPWAIT1);
    return;
  }
  fp = fopen (settingsFile, "r");
  if (fp == NULL) {
    DisplayMessage ("Error opening settings file.", BEEPWAIT1);
    return;
  }
  linePtr = fgets (line, MAX_SETTINGS_LEN+1+1, fp);
  if (linePtr == NULL) {
    DisplayMessage ("Error reading settings file.", BEEPWAIT1);
    return;
  }
  while ((linePtr = fgets(line,MAX_SETTINGS_LEN+1+1,fp)) != NULL) {
    newlinePtr = strchr (line, '\n');
    if (newlinePtr != NULL)
      *newlinePtr = NUL;
    else {
      /* Line was longer than MAX_SETTINGS_LEN.  What should we do? */
    }
    equalPtr = strchr (line, '=');
    if (equalPtr == NULL) break;
    *equalPtr = NUL;
    /**************************************************************************
    * Check for item/variable.
    **************************************************************************/
    if (!strcmp(line,"ITEM")) {
      fieldPtr = equalPtr + 1;
      commaPtr = strchr (fieldPtr, ',');
      if (commaPtr == NULL) break;
      *commaPtr = NUL;
      /************************************************************************
      * Check for `Record' item.
      ************************************************************************/
      if (!strcmp(fieldPtr,"Record")) {
	/**********************************************************************
	* Check if `Record' is present in SelectionWindow.
	**********************************************************************/
	for (Item = itemHead; Item != NULL; Item = Item->nextItem) {
	   if (Item->type == RECORDt) break;
	}
	if (Item == NULL) continue;
	/**********************************************************************
	* Check for `Output' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	commaPtr = strchr (fieldPtr, ',');
	if (commaPtr == NULL) break;
	*commaPtr = NUL;
	if (!SetOutput(fieldPtr,Item)) break;
        if (outputVars == 2L && Item->outputSetting) {
          vars[numVars] = (char *) cdf_AllocateMemory((size_t)6+1, FatalError);
          strcpy(vars[numVars], "DUMMYr");
          ++numVars;
        }
	/**********************************************************************
	* A settings file created in `simple' mode will end here.
	**********************************************************************/
	if (*(commaPtr+1) == NUL) continue;
	/**********************************************************************
	* Check for `Minimum' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	commaPtr = strchr (fieldPtr, ',');
	if (commaPtr == NULL) break;
	*commaPtr = NUL;
	if (NULstring(fieldPtr))
	  Item->Record->min = NOminMax;
	else
	  if (sscanf(fieldPtr,"%ld",&valueL) == 1)
	    if (valueL > 0)
	      Item->Record->min = valueL - 1;
	    else
	      break;
	  else
	    break;
	/**********************************************************************
	* Check for `Maximum' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	commaPtr = strchr (fieldPtr, ',');
	if (commaPtr == NULL) break;
	*commaPtr = NUL;
	if (NULstring(fieldPtr))
	  Item->Record->max = NOminMax;
	else
	  if (sscanf(fieldPtr,"%ld",&valueL) == 1)
	    if (valueL > 0)
	      Item->Record->max = valueL - 1;
	    else
	      break;
	  else
	    break;
	/**********************************************************************
	* Check that Minimum/Maximum fields are legal.
	**********************************************************************/
	if (Item->Record->min != NOminMax && Item->Record->max != NOminMax) {
	  if (Item->Record->min > Item->Record->max) {
	    Item->Record->min = NOminMax;
	    Item->Record->max = NOminMax;
	    break;
	  }
	}
	/**********************************************************************
	* Check for `Filter' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	commaPtr = strchr (fieldPtr, ',');
	if (commaPtr == NULL) break;
	*commaPtr = NUL;
	if (!SetFilter(fieldPtr,Item)) break;
	/**********************************************************************
	* Check for `Width' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	commaPtr = strchr (fieldPtr, ',');
	if (commaPtr == NULL) break;
	*commaPtr = NUL;
	if (!SetWidth(fieldPtr,Item)) break;
	continue;
      }
      /************************************************************************
      * Check for `Indices' item.
      ************************************************************************/
      if (!strcmp(fieldPtr,"Indices")) {
	/**********************************************************************
	* Check if `Indices' is present in SelectionWindow.
	**********************************************************************/
	for (Item = itemHead; Item != NULL; Item = Item->nextItem) {
	   if (Item->type == INDICESt) break;
	}
	if (Item == NULL) continue;
	/**********************************************************************
	* Check for `Output' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	commaPtr = strchr (fieldPtr, ',');
	if (commaPtr == NULL) break;
	*commaPtr = NUL;
	if (!SetOutput(fieldPtr,Item)) break;
        if (outputVars == 2L && Item->outputSetting) {
          vars[numVars] = (char *) cdf_AllocateMemory((size_t)6+1, FatalError);
          strcpy(vars[numVars], "DUMMYi");
          ++numVars;
        }
	/**********************************************************************
	* A settings file created in `simple' mode will end here.
	**********************************************************************/
	if (*(commaPtr+1) == NUL) continue;
	/**********************************************************************
	* Check for `Minimum' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	if (*fieldPtr == '[') {
	  long numDims, indices[CDF_MAX_DIMS]; int dimN;
	  commaPtr = strchr (fieldPtr + 1, ']');
	  if (commaPtr == NULL) break;
	  commaPtr = strchr (commaPtr + 1, ',');
	  if (commaPtr == NULL) break;
	  *commaPtr = NUL;
	  if (DecodeRecordAndIndices(fieldPtr,NULL,&numDims,indices)) {
	    Item->Indices->minNumDims = numDims;
	    for (dimN = 0; dimN < numDims; dimN++) {
	       Item->Indices->minIndices[dimN] = indices[dimN];
	    }
	  }
	  else
	    break;
	}
	else
	  if (*fieldPtr == ',') {
	    Item->Indices->minNumDims = NOminMax;
	    commaPtr = fieldPtr;
	  }
	  else
	    break;
	/**********************************************************************
	* Check for `Maximum' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	if (*fieldPtr == '[') {
	  long numDims, indices[CDF_MAX_DIMS]; int dimN;
	  commaPtr = strchr (fieldPtr + 1, ']');
	  if (commaPtr == NULL) break;
	  commaPtr = strchr (commaPtr + 1, ',');
	  if (commaPtr == NULL) break;
	  *commaPtr = NUL;
	  if (DecodeRecordAndIndices(fieldPtr,NULL,&numDims,indices)) {
	    Item->Indices->maxNumDims = numDims;
	    for (dimN = 0; dimN < numDims; dimN++) {
	       Item->Indices->maxIndices[dimN] = indices[dimN];
	    }
	  }
	  else
	    break;
	}
	else
	  if (*fieldPtr == ',') {
	    Item->Indices->maxNumDims = NOminMax;
	    commaPtr = fieldPtr;
	  }
	  else
	    break;
	/**********************************************************************
	* Check for `Filter' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	commaPtr = strchr (fieldPtr, ',');
	if (commaPtr == NULL) break;
	*commaPtr = NUL;
	if (!SetFilter(fieldPtr,Item)) break;
	/**********************************************************************
	* Check for `Width' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	commaPtr = strchr (fieldPtr, ',');
	if (commaPtr == NULL) break;
	*commaPtr = NUL;
	if (!SetWidth(fieldPtr,Item)) break;
	continue;
      }
      /************************************************************************
      * Check for `Variable' item.
      ************************************************************************/
      if (!strcmp(fieldPtr,"Variable")) {
	/**********************************************************************
	* Parse variable name.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	if (*fieldPtr != SETTINGS_DELIM) break;
	delimPtr = EndingDelimiter (fieldPtr);
	if (delimPtr == NULL) break;
	commaPtr = delimPtr + 1;
	if (*commaPtr != ',') break;
	*commaPtr = NUL;
	RemoveDelimiters (fieldPtr);
	/**********************************************************************
	* Check if the variable is in the SelectionWindow.
	**********************************************************************/
	for (Item = itemHead; Item != NULL; Item = Item->nextItem) {
	   if (Item->type == VARIABLEt) {
	     if (!strcmp(fieldPtr,Item->Var->name)) break;
	   }
	}
	if (Item == NULL) continue;
	/**********************************************************************
	* Check for `Output' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	commaPtr = strchr (fieldPtr, ',');
	if (commaPtr == NULL) break;
	*commaPtr = NUL;
	if (!SetOutput(fieldPtr,Item)) break;
        if (outputVars == 2L && Item->outputSetting) {
          vars[numVars] = (char *) cdf_AllocateMemory((size_t)CDF_VAR_NAME_LEN256+1,    
                                                      FatalError);
          strcpyX (vars[numVars], Item->Var->name, CDF_VAR_NAME_LEN256);
          ++numVars;
        }
	/**********************************************************************
	* A settings file created in `simple' mode will end here.
	**********************************************************************/
	if (*(commaPtr+1) == NUL) continue;
	/**********************************************************************
	* Check for `Minimum' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	if (!ScanValueField(fieldPtr,Item->Var->dataType,
			    Item->Var->numElems,&commaPtr,
			    &(Item->Var->min))) break;
	/**********************************************************************
	* Check for `Maximum' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	if (!ScanValueField(fieldPtr,Item->Var->dataType,
			    Item->Var->numElems,&commaPtr,
			    &(Item->Var->max))) break;
	/**********************************************************************
	* Check for `Filter' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	commaPtr = strchr (fieldPtr, ',');
	if (commaPtr == NULL) break;
	*commaPtr = NUL;
	if (!SetFilter(fieldPtr,Item)) break;
	/**********************************************************************
	* Check for `Fill' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	if (!ScanValueField(fieldPtr,Item->Var->dataType,
			    Item->Var->numElems,&commaPtr,
			    &(Item->Var->fill))) break;
	/**********************************************************************
	* Check for `Monotonicity' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	commaPtr = strchr (fieldPtr, ',');
	if (commaPtr == NULL) break;
	*commaPtr = NUL;
	if (!SetMonotonic(fieldPtr,Item->Var)) break;
	/**********************************************************************
	* Check for `Format' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	if (*fieldPtr == SETTINGS_DELIM) {
	  delimPtr = EndingDelimiter (fieldPtr);
	  if (delimPtr == NULL) break;
	  commaPtr = delimPtr + 1;
	  if (*commaPtr != ',') break;
	  *commaPtr = NUL;
	  RemoveDelimiters (fieldPtr);
	  if (Item->Var->format != NULL) {
	    cdf_FreeMemory (Item->Var->format, FatalError);
	  }
	  Item->Var->format = (char *) cdf_AllocateMemory ((size_t)strlen(fieldPtr) + 1,
						       FatalError);
	  strcpyX (Item->Var->format, fieldPtr, 0);
	}
	else {
	  commaPtr = fieldPtr;
	  if (Item->Var->format != NULL) {
	    cdf_FreeMemory (Item->Var->format, FatalError);
	    Item->Var->format = NULL;
	  }
	}
	/**********************************************************************
	* Check for `Width' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	commaPtr = strchr (fieldPtr, ',');
	if (commaPtr == NULL) break;
	*commaPtr = NUL;
	if (!SetWidth(fieldPtr,Item)) break;
	/**********************************************************************
	* Check for `Sparseness' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	commaPtr = strchr (fieldPtr, ',');
	if (commaPtr == NULL) break;
	*commaPtr = NUL;
	if (!SetSparseness(fieldPtr,Item->Var)) break;
	/**********************************************************************
	* Check for `Compression' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	commaPtr = strchr (fieldPtr, ',');
	if (commaPtr == NULL) break;
	*commaPtr = NUL;
	if (!SetCompression(fieldPtr,Item->Var)) break;
	/**********************************************************************
	* Check for `Blocking' field.
	**********************************************************************/
	fieldPtr = commaPtr + 1;
	commaPtr = strchr (fieldPtr, ',');
	if (commaPtr == NULL) break;
	*commaPtr = NUL;
	if (!SetBlocking(fieldPtr,Item->Var)) break;
	/**********************************************************************
	* Continue to next line...
	**********************************************************************/
	continue;
      }
      break;
    }
    /**************************************************************************
    * Check for `Use filters'.
    **************************************************************************/
    if (!strcmp(line,"USE_FILTERS")) {
      if (!strcmp(equalPtr+1,"YES")) {
	opt.overallFilter = TRUE;
	continue;
      }
      if (!strcmp(equalPtr+1,"no")) {
	opt.overallFilter = FALSE;
	continue;
      }
      break;
    }
    /**************************************************************************
    * Check for `Use fills'.
    **************************************************************************/
    if (!strcmp(line,"USE_FILLS")) {
      if (!strcmp(equalPtr+1,"YES")) {
	opt.useFills = TRUE;
	continue;
      }
      if (!strcmp(equalPtr+1,"no")) {
	opt.useFills = FALSE;
	continue;
      }
      break;
    }
    /**************************************************************************
    * Check for `CDF Format'.
    **************************************************************************/
    if (!strcmp(line,"CDF_FORMAT")) {
      if (!strcmp(equalPtr+1,"single")) {
	opt.singleFile = TRUE;
	continue;
      }
      if (!strcmp(equalPtr+1,"multi")) {
	opt.singleFile = FALSE;
	continue;
      }
      break;
    }
    /**************************************************************************
    * Check for `CDF Encoding'.
    **************************************************************************/
    if (!strcmp(line,"CDF_ENCODING")) {
      int encoding; Logical found;
      for (encoding = 0, found = FALSE; encoding <= MAX_ENCODING; encoding++) {
	 if (encodings[encoding] != NULL) {
	   if (!strcmp(equalPtr+1,encodings[encoding])) {
	     opt.encoding = encoding;
	     found = TRUE;
	     break;
	   }
	 }
      }
      if (found) continue;
      break;
    }
    /**************************************************************************
    * Check for `CDF Compression'.
    **************************************************************************/
    if (!strcmp(line,"CDF_COMPRESSION")) {
      long type; Logical found;
      long ct, cp[CDF_MAX_DIMS];
      type = WhichCompression(equalPtr+1, &ct, cp);
      if (type > -1) {
        CDFcType = ct;
        CDFcParms[0] = cp[0]; 
        found = TRUE;
      }
      if (found) continue;
      break;
    }
    /**************************************************************************
    * Check for `CDF Checksum'.
    **************************************************************************/
    if (!strcmp(line,"CDF_CHECKSUM")) {
      int type; Logical found;
      for (type = 0, found = FALSE; type <= MAX_CHECKSUM; type++) {
         if (checksums[type] != NULL) {
           if (strcmpIgCase(equalPtr+1,checksums[type]) == 1) {
             CDFchecksum = (long) type;
             found = TRUE;
             break;
           }
         }
      }
      if (found) continue;
      break;
    }
    /**************************************************************************
    * Check for `EPOCH Style'.
    **************************************************************************/
    if (!strcmp(line,"EPOCH_STYLE")) {
      int style; Logical found;
      for (style = EPOCH0_STYLE, found = FALSE;
	   style <= EPOCH4_STYLE; style++) {
	 if (StrStrIgCaseX(equalPtr+1,epochStyles[style])) {
	   opt.epochStyle = style;
	   found = TRUE;
	   break;
	 }
      }
      if (found) continue;
      break;
    }
    /**************************************************************************
    * Check for `Listing Orientation'.
    **************************************************************************/
    if (!strcmp(line,"ORIENTATION")) {
      if (!strcmp(equalPtr+1,"horizontal")) {
	opt.horizontalMode = TRUE;
	continue;
      }
      if (!strcmp(equalPtr+1,"vertical")) {
	opt.horizontalMode = FALSE;
	continue;
      }
      break;
    }
    /**************************************************************************
    * Check for `Listing/CDF Majority'.
    **************************************************************************/
    if (!strcmp(line,"MAJORITY")) {
      if (!strcmp(equalPtr+1,"row")) {
	opt.majority = ROW_MAJOR;
	continue;
      }
      if (!strcmp(equalPtr+1,"column")) {
	opt.majority = COLUMN_MAJOR;
	continue;
      }
      if (!strcmp(equalPtr+1,"input")) {
	opt.majority = INPUT_MAJOR;
	continue;
      }
      break;
    }
    /**************************************************************************
    * Check for `Show Filtered Lines'.
    **************************************************************************/
    if (!strcmp(line,"SHOW_FILTERED")) {
      if (!strcmp(equalPtr+1,"YES")) {
	opt.showFiltered = TRUE;
	continue;
      }
      if (!strcmp(equalPtr+1,"no")) {
	opt.showFiltered = FALSE;
	continue;
      }
      break;
    }
    /**************************************************************************
    * Check for `Listing Spacing'.
    **************************************************************************/
    if (!strcmp(line,"SPACING")) {
      int spacing;
      if (sscanf(equalPtr+1,"%d",&spacing) == 1) {
	if (0 <= spacing) {
	  opt.spacing = spacing;
	  continue;
	}
	else
	  break;
      }
      else
	break;
    }
    /**************************************************************************
    * Check for `Delete Existing CDF'.
    **************************************************************************/
    if (!strcmp(line,"DELETE_EXISTING")) {
      if (!strcmp(equalPtr+1,"YES")) {
	opt.deleteExisting = TRUE;
	continue;
      }
      if (!strcmp(equalPtr+1,"no")) {
	opt.deleteExisting = FALSE;
	continue;
      }
      break;
    }
    /**************************************************************************
    * Check for `Preallocate Variable Records'.
    **************************************************************************/
    if (!strcmp(line,"PREALLOCATE")) {
      if (!strcmp(equalPtr+1,"YES")) {
	opt.preAllocate = TRUE;
	continue;
      }
      if (!strcmp(equalPtr+1,"no")) {
	opt.preAllocate = FALSE;
	continue;
      }
      break;
    }
    /**************************************************************************
    * Check for `HEADING'.
    **************************************************************************/
    if (!strcmp(line,"HEADING")) {
      int style; Logical found;
      char *headings[] = { "no", "YES", "POUNDEDHEADING" };
      Logical ll[] = { FALSE, TRUE, TRUE };
      poundedheading = 0;
      for (style = 0, found = FALSE;
           style < 3; style++) {
         if (!strcmp(equalPtr+1,headings[style])) {
           opt.textHeading = ll[style];
           if (style == 2) poundedheading = 1;
           found = TRUE;
           break;
         }
      }
      if (found) continue;
      break;
    }
    break;
  }
  fclose (fp);
  if (linePtr != NULL) {
    DisplayMessage ("Error parsing settings file.", BEEPWAIT1);
    return;
  }
  return;
}

/******************************************************************************
* SetFilter.
******************************************************************************/

static Logical SetFilter (field, Item)
char *field;
struct ItemStruct *Item;
{
  if (!strcmp(field,"YES")) {
    Item->filterSetting = TRUE;
    Item->inclusive = TRUE;
    return TRUE;
  }
  if (!strcmp(field,"YeS")) {
    Item->filterSetting = TRUE;
    Item->inclusive = TRUE;
    return TRUE;
  }
  if (!strcmp(field,"yEs")) {
    Item->filterSetting = TRUE;
    Item->inclusive = FALSE;
    return TRUE;
  }
  if (!strcmp(field,"no")) {
    Item->filterSetting = FALSE;
    Item->inclusive = TRUE;
    return TRUE;
  }
  return FALSE;
}

/******************************************************************************
* SetOutput.
******************************************************************************/

static Logical SetOutput (field, Item)
char *field;
struct ItemStruct *Item;
{
  if (!strcmp(field,"YES")) {
    Item->outputSetting = TRUE;
    return TRUE;
  }
  if (!strcmp(field,"no")) {
    Item->outputSetting = FALSE;
    return TRUE;
  }
  return FALSE;
}

/******************************************************************************
* SetWidth.
******************************************************************************/

static Logical SetWidth (field, Item)
char *field;
struct ItemStruct *Item;
{
  int width;
  if (sscanf(field,"%d",&width) != 1) return FALSE;
  if (width < 1) return FALSE;
  Item->width = width;
  return TRUE;
}

/******************************************************************************
* SetBlocking.
******************************************************************************/

static Logical SetBlocking (field, Var)
char *field;
struct VarStruct *Var;
{
  long blocking;
  if (sscanf(field,"%ld",&blocking) != 1) return FALSE;
  if (blocking < 0) return FALSE;
  Var->blocking = blocking;
  return TRUE;
}

/******************************************************************************
* SetMonotonic.
******************************************************************************/

static Logical SetMonotonic (field, Var)
char *field;
struct VarStruct *Var;
{
  if (Var->monotonic != NAmono) {
    if (!strcmp(field,"Increase")) {
      Var->monotonic = INCREASEmono;
      return TRUE;
    }
    if (!strcmp(field,"Decrease")) {
      Var->monotonic = DECREASEmono;
      return TRUE;
    }
    if (!strcmp(field,"False")) {
      Var->monotonic = FALSEmono;
      return TRUE;
    }
    if (!strcmp(field,"Unknown")) {
      Var->monotonic = UNKNOWNmono;
      return TRUE;
    }
    return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* SetSparseness.
******************************************************************************/

static Logical SetSparseness (field, Var)
char *field;
struct VarStruct *Var;
{
  Var->sRecordsType = NO_SPARSERECORDS;
  if (strstr(field,"sRecords.PAD") != NULL) {
    Var->sRecordsType = PAD_SPARSERECORDS;
    return TRUE;
  }
  if (strstr(field,"sRecords.PREV") != NULL) {
    Var->sRecordsType = PREV_SPARSERECORDS;
    return TRUE;
  }
  return TRUE;
}

/******************************************************************************
* SetCompression.
******************************************************************************/

static Logical SetCompression (field, Var)
char *field;
struct VarStruct *Var;
{
  if (!strcmp(field,"None")) {
    Var->cType = NO_COMPRESSION;
    return TRUE;
  }
  if (!strncmp(field,"GZIP", 4)) {
    long cl;
    Var->cType = GZIP_COMPRESSION;
    if (sscanf(field+5, "%ld", &cl) == 1) {
      if (cl > 0L && cl < 10L)
        Var->cParms[0] = cl;
      else
        Var->cParms[0] = 6L;
    } else
      Var->cParms[0] = 6L;
    return TRUE;
  }
  if (!strcmp(field,"RLE.0")) {
    Var->cType = RLE_COMPRESSION;
    Var->cParms[0] = RLE_OF_ZEROs;
    return TRUE;
  }
  if (!strcmp(field,"HUFF.0")) {
    Var->cType = HUFF_COMPRESSION;
    Var->cParms[0] = OPTIMAL_ENCODING_TREES;
    return TRUE;
  }
  if (!strcmp(field,"AHUFF.0")) {
    Var->cType = AHUFF_COMPRESSION;
    Var->cParms[0] = OPTIMAL_ENCODING_TREES;
    return TRUE;
  }
  return TRUE;
}

/******************************************************************************
* ScanValueField.
******************************************************************************/

static Logical ScanValueField (fieldPtr, dataType, numElems, commaPtr, value)
char *fieldPtr;         /* In: Pointer to beginning of field. */
long dataType;          /* In: Data type of variable. */
long numElems;          /* In: Number of elements of variable. */
char **commaPtr;        /* Out: Pointer (to pointer) to ending comma. */
void **value;           /* Out: Pointer (to pointer) to value. */
{
  if (*fieldPtr == ',') {
    if (*value != NULL) {
      cdf_FreeMemory (*value, FatalError);
      *value = NULL;
    }
    *commaPtr = fieldPtr;
    return TRUE;
  }
  if (*fieldPtr == SETTINGS_DELIM) {
    char *delimPtr = EndingDelimiter (fieldPtr);
    if (delimPtr == NULL) return FALSE;
    *commaPtr = delimPtr + 1;
    if (**commaPtr != ',') return FALSE;
    **commaPtr = NUL;
    if (STRINGdataType(dataType)) {
      RemoveDelimiters (fieldPtr);
      if ((int) strlen(fieldPtr) == (int) numElems) {
	if (*value == NULL) {
	  size_t nBytes = (size_t) (numElems + 1);
	  *value = (char *) cdf_AllocateMemory (nBytes, FatalError);
	}
	strcpyX (*value, fieldPtr, 0);
      }
    }
    return TRUE;
  }
  if (*fieldPtr == '[') {
    void *newValue; long newNumElems;
    char *endPtr = strchr (fieldPtr + 1, ']');
    if (endPtr == NULL) return FALSE;
    *commaPtr = endPtr + 1;
    if (**commaPtr != ',') return FALSE;
    fieldPtr++;
    *endPtr = NUL;
    if (!STRINGdataType(dataType)) {
      if (!DecodeValues(fieldPtr,BOO(EPOCHdataType(dataType),
				     CDF_REAL8,dataType),
			&newNumElems,&newValue,0)) return FALSE;
      if (newNumElems != numElems) {
	cdf_FreeMemory (newValue, FatalError);
	return FALSE;
      }
      if (*value != NULL) cdf_FreeMemory (*value, FatalError);
      *value = newValue;
    }
    return TRUE;
  }
  return FALSE;
}

/******************************************************************************
* WriteDelimitedString.
******************************************************************************/

static void WriteDelimitedString (fp, delim, string)
FILE *fp;
int delim;
char *string;
{
  int i;
  fprintf (fp, "%c", (char) delim);
  for (i = 0; string[i] != NUL; i++) {
     if (string[i] == (char) delim) fprintf (fp, "%c", (char) delim);
     fprintf (fp, "%c", string[i]);
  }
  fprintf (fp, "%c", (char) delim);
  return;
}

/******************************************************************************
* EndingDelimiter.
******************************************************************************/

static char *EndingDelimiter (string)
char *string;
{
  int i; char delim;
  for (i = 1, delim = string[0]; string[i] != NUL; i++) {
     if (string[i] == delim) {
       if (string[i+1] != delim)
	 return &(string[i]);
       else
	 i++;
     }
  }
  return NULL;
}

/******************************************************************************
* RemoveDelimiters.
******************************************************************************/

static void RemoveDelimiters (string)
char *string;
{
  int to = 0, from = 1; char delim = string[0];
  for (;;) {
     if (string[from] == NUL) {
       string[to] = NUL;
       return;
     }
     if (string[from] == delim)
       if (string[from+1] == delim) {
	 string[to++] = delim;
	 from += 2;
       }
       else
	 from++;
     else
       string[to++] = string[from++];
  }
}

/******************************************************************************
* SetItemMonotonicities.
* Returns FALSE if a fatal error occurred.
******************************************************************************/

Logical SetItemMonotonicities () {
  struct ItemStruct *Item;
  DisplayMessage ("Setting variable monotonicities.", NOBEEPWAIT1);
  for (Item = itemHead; Item != NULL; Item = Item->nextItem) {
     switch (Item->type) {
       case RECORDt:
       case INDICESt:
	 break;
       case VARIABLEt:
	 if (Item->Var->monotonic != NAmono) {
	   if (!SetVarMonotonicity(Item->Var)) return FALSE;
	 }
	 break;
     }
  }
  DisplayMessage ("", NOWAIT);
  return TRUE;
}

/******************************************************************************
* SetVarMonotonicity.
* Returns FALSE if a fatal error occurred.
******************************************************************************/

Logical SetVarMonotonicity (Var)
struct VarStruct *Var;
{
  static long indices[CDF_MAX_DIMS] = { 0,0,0,0,0,0,0,0,0,0 };
  void *prevValue = NULL; CDFstatus status;
  Var->monotonic = UNKNOWNmono;
  status = CDFlib (SELECT_, BOO(Var->zVar,zVAR_,rVAR_), Var->varN,
			    BOO(Var->zVar,zVAR_SEQPOS_,
					  rVAR_SEQPOS_), 0L, indices,
		   NULL_);
  DisplayStatus (status, "setting variable monotonicity");
  if (StatusBAD(status)) return FALSE;
  status = CDFlib (GET_, BOO(Var->zVar,zVAR_SEQDATA_,
				       rVAR_SEQDATA_), Var->value,
		   NULL_);
  while (StatusOK(status)) {
    if (prevValue == NULL) {
      prevValue = cdf_AllocateMemory ((size_t)Var->nValueBytes, FatalError);
      ASSIGNx (prevValue, Var->value, Var->dataType, Var->numElems);
    }
    else {
      switch (Var->monotonic) {
	case UNKNOWNmono:
	  if (GTx(Var->value,prevValue,Var->dataType,Var->numElems)) {
	    Var->monotonic = INCREASEmono;
	    break;
	  }
	  if (LTx(Var->value,prevValue,Var->dataType,Var->numElems)) {
	    Var->monotonic = DECREASEmono;
	    break;
	  }
	  Var->monotonic = FALSEmono;
	  cdf_FreeMemory (prevValue, FatalError);
	  return TRUE;
	case INCREASEmono:
	  if (LTx(Var->value,prevValue,Var->dataType,Var->numElems)) {
	    Var->monotonic = FALSEmono;
	    cdf_FreeMemory (prevValue, FatalError);
	    return TRUE;
	  }
	  break;
	case DECREASEmono:
	  if (GTx(Var->value,prevValue,Var->dataType,Var->numElems)) {
	    Var->monotonic = FALSEmono;
	    cdf_FreeMemory (prevValue, FatalError);
	    return TRUE;
	  }
	  break;
      }
      ASSIGNx (prevValue, Var->value, Var->dataType, Var->numElems);
    }
    status = CDFlib (GET_, BOO(Var->zVar,zVAR_SEQDATA_,
					 rVAR_SEQDATA_), Var->value,
		     NULL_);
  }
  if (prevValue != NULL) cdf_FreeMemory (prevValue, FatalError);
  if (status != END_OF_VAR) {
    DisplayStatus (status, "setting variable monotonicity");
    if (StatusBAD(status)) return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* BuildExportList.
******************************************************************************/

void BuildExportList (exportHead, walking)
struct ItemStruct **exportHead;
Logical walking;
{
  struct ItemStruct *Item, *exportTail = NULL;
  int jj;
  Logical found;

  *exportHead = NULL;
  if (includeVars > 0L && numVarsO == 0) return;
  if (outputVars != 2L) { /* A non-sequencesettings option */
    for (Item = itemHead; Item != NULL; Item = Item->nextItem) {
       if ((walking && Item->outputSetting && Item->type == VARIABLEt) ||
    	   (!walking && (Item->outputSetting ||
		         (opt.overallFilter && Item->filterSetting)))) {
         if (includeVars > 0L && Item->type == VARIABLEt) {
           found = FALSE;
           for (jj = 0; jj < numVars; ++jj) {
             if (strcmp(Item->Var->name, vars[jj]) == 0) {
               found = TRUE;
               break;
             }
           }
           if (includeVars == 1L && !found) continue;
           if (includeVars == 2L && found) continue;
         }
         if (*exportHead == NULL)
	   *exportHead = exportTail = Item;
         else
	   exportTail = exportTail->nextExport = Item;
         Item->nextExport = NULL;
         Item->output = Item->outputSetting;
         Item->filter = BOO(opt.overallFilter,Item->filterSetting,FALSE);
       }
    }
  } else { /* Export by the variable sequence in the settings file. */
    for (jj = 0; jj < numVars; ++jj) {
      for (Item = itemHead; Item != NULL; Item = Item->nextItem) {
        if (!Item->outputSetting) continue;
        if ((Item->type == RECORDt && strcmp(vars[jj], "DUMMYr") == 0) || 
            (Item->type == INDICESt && strcmp(vars[jj], "DUMMYi") == 0) ||
            (Item->type == VARIABLEt && 
             strcmp(vars[jj], Item->Var->name) == 0)) { 
          if (*exportHead == NULL)
            *exportHead = exportTail = Item;
          else
            exportTail = exportTail->nextExport = Item;
          Item->nextExport = NULL;
          Item->output = Item->outputSetting;
          Item->filter = BOO(opt.overallFilter,Item->filterSetting,FALSE);
          break;
        }
      }
    }
  }
  return;
}

/******************************************************************************
* RemoveExportItems.
******************************************************************************/

void RemoveExportItems (exportHead)
struct ItemStruct **exportHead;
{
  struct ItemStruct *Item = *exportHead, *prevItem = NULL;
  while (Item != NULL) {
    if (!Item->output && !Item->filter) {
      if (prevItem == NULL)
	*exportHead = Item->nextExport;
      else
	prevItem->nextExport = Item->nextExport;
    }
    else
      prevItem = Item;
    Item = Item->nextExport;
  }
  return;
}

/******************************************************************************
* FindUsedEpochs.
******************************************************************************/

int FindUsedEpochs (exportHead, indx)
struct ItemStruct **exportHead;
int *indx;
{
  int ix, iy, iz, ia, i1, numVars, num;
  int *tmpx, *varInGroup;
  char **names, **t1;
  struct ItemStruct *Item = *exportHead;
  struct ItemStruct **t2;
  numVars = 0;
  for (Item = *exportHead; Item != NULL; Item = Item->nextExport) {
    if (Item->type == VARIABLEt) ++numVars;
  }
  names = (char **) malloc(sizeof(char **) * numVars);
  varInGroup = (int *) calloc(1, sizeof(int) * numEpochs);
  for (ix = 0; ix < numVars; ++ix) 
    names[ix] = malloc(CDF_VAR_NAME_LEN256+1);
  num = 0;
  Item = *exportHead;
  while (Item != NULL) {
    if (Item->type == VARIABLEt) {
      strcpyX (names[num], Item->Var->name, 0);
      for (ix = 0; ix < numEpochs; ++ix) {
        if (!strcmp(Item->Var->name, epochs[ix]) ||
            !strncmp(Item->Var->name, epochs[ix], (int)strlen(epochs[ix])-6)) {
          indx[ix] = 1;
          varInGroup[num] = ix;
          break;
        }
      }
      ++num;
    }
    Item = Item->nextExport;
  }
  numGroups = 0;
  for (ix = 0; ix < numEpochs; ++ix) {
    if (indx[ix] == 1) ++numGroups;
  }
  tmpx = (int *) calloc(1, sizeof(int) * numEpochs);
  numVarsInGroup = (int *) calloc(1, sizeof(int) * numGroups);
  for (ix = 0; ix < num; ++ix) {
    ++tmpx[varInGroup[ix]];
  }
  for (ix = 0,iy = 0; ix < numEpochs; ++ix) {
    if (tmpx[ix] > 0) {
      numVarsInGroup[iy] = tmpx[ix];
      ++iy;
    }
  }
  varsInGroup = (char ***) malloc(sizeof(char ***) * numGroups);
  varPtrsInGroup = (struct ItemStruct ***)
                    malloc(sizeof(struct ItemStruct ***) * numGroups);
  for (ix = 0, ia = 0; ix < numGroups; ++ix) {
    t1 = (char **) malloc(sizeof(char **) * numVarsInGroup[ix]);
    t2 = (struct ItemStruct **) 
                   malloc(sizeof(struct ItemStruct **) * numVarsInGroup[ix]);
    varsInGroup[ix] = (char **) malloc(sizeof(char **) * numVarsInGroup[ix]);
    varPtrsInGroup[ix] = (struct ItemStruct **) 
                     malloc(sizeof(struct ItemStruct **) * numVarsInGroup[ix]);
    for (iy = 0; iy < numVarsInGroup[ix]; ++iy) {
      varsInGroup[ix][iy] = (char *) malloc(CDF_VAR_NAME_LEN256+1);
      t1[iy] = (char *) malloc(CDF_VAR_NAME_LEN256+1);
      strcpyX (t1[iy], names[ia], 0);
      Item = *exportHead;
      while (Item != NULL) {
        if (Item->type == VARIABLEt) {
          if (!strcmp(Item->Var->name, names[ia])) {
            t2[iy] = Item;
            break;
          }
        }
        Item = Item->nextExport;
      }
      ++ia;
    }
    i1 = -1;
    /* Reorder the variables. */
    for (iy = 0; iy < numVarsInGroup[ix]; ++iy) {
      if (strstrIgCase(t1[iy], "epoch") != NULL) {
        i1 = iy;
        break;
      }
    }
    if (i1 == -1) {
      for (iy = 0; iy < numVarsInGroup[ix]; ++iy) {
        strcpyX (varsInGroup[ix][iy], t1[iy], 0);
        varPtrsInGroup[ix][iy] = t2[iy];
      }
    } else {
      strcpyX (varsInGroup[ix][0], t1[i1], 0);
      varPtrsInGroup[ix][0] = t2[i1];
      for (iz = 0, iy = 1; iy < numVarsInGroup[ix]; ++iz) {
        if (iz == i1) continue;
        strcpyX (varsInGroup[ix][iy], t1[iz], 0);
        varPtrsInGroup[ix][iy++] = t2[iz];
      }
    }
    free (t1);
    free (t2);
  }
  free (names); 
  free (varInGroup);
  free (tmpx);
  return numGroups;
}

/******************************************************************************
* ReorderExportItems.
******************************************************************************/

void ReorderExportItems (exportHead)
struct ItemStruct **exportHead;
{
  Logical cont, reorder;
  struct ItemStruct *Item = *exportHead, *prevItem = NULL, *head = NULL;
  cont = TRUE;
  reorder = FALSE;
  do {
    if (Item != NULL) {
      if (Item->type == VARIABLEt &&
          (Item->Var->dataType == CDF_EPOCH ||
           Item->Var->dataType == CDF_EPOCH16 ||
	   Item->Var->dataType == CDF_TIME_TT2000)) {
        if (prevItem == NULL) {
          cont = FALSE;
          reorder = FALSE;
        } else {
          head = Item;
          prevItem->nextExport = Item->nextExport;
          reorder = TRUE;
          cont = FALSE;
        }
      } else
        prevItem = Item;
      Item = Item->nextExport;
    } else
      cont = FALSE;
  } while (cont);
  if (reorder) {
    head->nextExport = *exportHead;
    *exportHead = head;
  }
  return;
}

/******************************************************************************
* ReorderGroupItems.
******************************************************************************/

void ReorderGroupItems (exportHead)
struct ItemStruct **exportHead;
{
  Logical cont, reorder;
  struct ItemStruct *Item = *exportHead, *prevItem = NULL, *head = NULL;
  cont = TRUE;
  reorder = FALSE;
  do {
    if (Item != NULL) {
      if (Item->type == VARIABLEt &&
          (Item->Var->dataType == CDF_EPOCH ||
           Item->Var->dataType == CDF_EPOCH16 ||
	   Item->Var->dataType == CDF_TIME_TT2000)) {
        if (prevItem == NULL) {
          cont = FALSE;
          reorder = FALSE;
        } else {
          head = Item;
          prevItem->nextExport = Item->nextExport;
          reorder = TRUE;
          cont = FALSE;
        }
      } else
        prevItem = Item;
      Item = Item->nextExport;
    } else
      cont = FALSE;
  } while (cont);
  if (reorder) {
    head->nextExport = *exportHead;
    *exportHead = head;
  }
  return;
}

/******************************************************************************
* FreeExportBuffers.
******************************************************************************/

void FreeExportBuffers (exportHead)
struct ItemStruct *exportHead;
{
  struct ItemStruct *Item;
  for (Item = exportHead; Item != NULL; Item = Item->nextExport) {
     if (Item->Var->buffer != NULL) cdf_FreeMemory (Item->Var->buffer,
						FatalError);
  }
  return;
}

/******************************************************************************
* FindFirstRecord.
******************************************************************************/

long FindFirstRecord (recX, scalarHead, filteringScalars, recCount)
long recX;
struct ItemStruct *scalarHead;
Logical filteringScalars;
long recCount;
{
  struct ItemStruct *Item; long firstX;
  if (!filteringScalars) return recX;
  for (firstX = recX; firstX < recCount; firstX++) {
     Logical passes = TRUE;
     for (Item = scalarHead; Item != NULL; Item = Item->nextScalar) {
	if (Item->filter) {
	  Byte1 *value = Item->Var->buffer +
		        (size_t) (firstX * Item->Var->nValueBytes);
	  if (!VarPassesMin(Item,value) || !VarPassesMax(Item,value)) {
	    passes = FALSE;
	    break;
	  }
	}
     }
     if (passes) return firstX;
  }
  return NO_RECORD;
}

/******************************************************************************
* FindLastRecord.
******************************************************************************/

long FindLastRecord (firstX, scalarHead, filteringScalars, recCount)
long firstX;
struct ItemStruct *scalarHead;
Logical filteringScalars;
long recCount;
{
  struct ItemStruct *Item; long recX;
  if (!filteringScalars) return (recCount - 1);
  for (recX = firstX + 1; recX < recCount; recX++) {
     for (Item = scalarHead; Item != NULL; Item = Item->nextScalar) {
	if (Item->filter) {
	  Byte1 *value = Item->Var->buffer +
		        (size_t) (recX * Item->Var->nValueBytes);
	  if (!VarPassesMin(Item,value) ||
	      !VarPassesMax(Item,value)) return (recX - 1);
	}
     }
  }
  return (recCount - 1);
}

/******************************************************************************
* FilterBuffer.
******************************************************************************/

void FilterBuffer (Item, buffer, nValues)
struct ItemStruct *Item;
Byte1 *buffer;
long nValues;
{
  long valueN; Byte1 *value;
  for (valueN = 0, value = buffer; valueN < nValues;
       valueN++, value += Item->Var->nValueBytes) {
     if (!VarPassesMin(Item,value) || !VarPassesMax(Item,value)) {
       if (USEFILL(Item->Var,opt))
	 ASSIGNx (value, Item->Var->fill,
		  Item->Var->dataType, Item->Var->numElems);
       else
	 ASSIGNx (value, Item->Var->pad,
		  Item->Var->dataType, Item->Var->numElems);
     }
  }
  return;
}

/******************************************************************************
* FilterHypers.
******************************************************************************/

void FilterHypers (hyperHead, nValues)
struct ItemStruct *hyperHead;
long nValues;
{
  long valueN; Byte1 *value; struct ItemStruct *Item;
  for (valueN = 0; valueN < nValues; valueN++) {
     for (Item = hyperHead; Item != NULL; Item = Item->nextHyper) {
	if (Item->filter) {
	  value = VALUEinBUFFER (Item->Var, valueN);
	  if (!VarPassesMin(Item,value) || !VarPassesMax(Item,value)) {
	    for (Item = hyperHead; Item != NULL; Item = Item->nextHyper) {
	       if (Item->output) {
		 value = VALUEinBUFFER (Item->Var, valueN);
		 if (USEFILL(Item->Var,opt))
		   ASSIGNx (value, Item->Var->fill, Item->Var->dataType,
			    Item->Var->numElems);
		 else
		   ASSIGNx (value, Item->Var->pad, Item->Var->dataType,
			    Item->Var->numElems);
	       }
	    }
	    break;
	  }
	}
     }
  }
  return;
}

/******************************************************************************
* AbortListing.
* Does nothing if in batch mode.
******************************************************************************/

Logical AbortListing (oFp, line)
FILE *oFp;
char *line;
{
  int key;
  static char keyDefsBlank[] = "\n\n";
  if (BATCH(batchMode)) return FALSE;
  if (read_input(
#if defined(CURSESui)
		 EWmsg->wid,
#endif
			    &key,PASSTHRUri,FALSE)) {
    if (key == ABORTkey_EXPORT) {
      NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
      DisplayMessage ("Aborting at user's request.", NOBEEPWAIT1);
      DisplayMessage ("Listing file is not complete.", BEEPWAIT);
      if (fclose(oFp) == EOF) {
	DisplayMessage ("Error closing listing file.", BEEPWAIT);
      }
      cdf_FreeMemory (line, FatalError);
      return TRUE;
    }
    else
      EditWindow (BEEPew, EWmsg);
  }
  return FALSE;
}

/******************************************************************************
* AbortCDF.
* Does nothing if in batch mode.
******************************************************************************/

Logical AbortCDF (exportHead)
struct ItemStruct *exportHead;
{
  int key;
  static char keyDefsBlank[] = "\n\n";
  if (BATCH(batchMode)) return FALSE;
  if (read_input(
#if defined(CURSESui)
		 EWmsg->wid,
#endif
			    &key,PASSTHRUri,FALSE)) {
    if (key == ABORTkey_EXPORT) {
      NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
      DisplayMessage ("Aborting at user's request.", NOBEEPWAIT1);
      DisplayMessage ("CDF is not complete.", BEEPWAIT);
      FreeExportBuffers (exportHead);
      return TRUE;
    }
    else
      EditWindow (BEEPew, EWmsg);
  }
  return FALSE;
}

/******************************************************************************
* PreAllocateRecords.
******************************************************************************/

Logical PreAllocateRecords (inID, outID, scalarHead, hyperHead, firstRec,
			    lastRec)
CDFid inID;
CDFid outID;
struct ItemStruct *scalarHead;
struct ItemStruct *hyperHead;
long *firstRec;
long *lastRec;
{
  long nRecords = *lastRec - *firstRec + 1, newFirstRec, newLastRec, nHypers;
  long nValues, hyperN, recX, firstX, lastX, recF, recL;
  CDFstatus status; struct ItemStruct *Item; struct HyperStruct hyper;
  size_t nBytes, *nValueBytes; Byte1 ***handles; struct GroupStruct groups;
  int filterCount, filterX;
  AOSs1A (allocMsg,"Allocating variable records...")
  /****************************************************************************
  * Count number of scalars still being filtered.  Note that monotonic
  * scalars will have already been considered when the first/last record was
  * determined (and filtering turned off for them).
  ****************************************************************************/
  DisplayPctComplete (NO_PCT, allocMsg[0]);
  for (Item = scalarHead, filterCount = 0;
       Item != NULL; Item = Item->nextScalar) if (Item->filter) filterCount++;
  /****************************************************************************
  * If no scalars are being filtered, allocate records based on the current
  * first/last records.
  ****************************************************************************/
  if (filterCount == 0) {
    for (Item = scalarHead; Item != NULL; Item = Item->nextScalar) {
       if (Item->output) {
	 Item->Var->oRecords = COUNTx(Item->Var->maxRec,*firstRec,*lastRec);
       }
    }
    for (Item = hyperHead; Item != NULL; Item = Item->nextHyper) {
       if (Item->output) {
	 Item->Var->oRecords = COUNTx(Item->Var->maxRec,*firstRec,*lastRec);
       }
    }
    if (!AllocateRecords(outID,scalarHead,hyperHead)) return FALSE;
    return TRUE;
  }
  /****************************************************************************
  * Allocate buffers for the scalars.
  ****************************************************************************/
  nBytes = (size_t) filterCount * sizeof(Byte1 **);
  handles = (Byte1 ***) cdf_AllocateMemory (nBytes, FatalError);
  nBytes = (size_t) filterCount * sizeof(size_t);
  nValueBytes = (size_t *) cdf_AllocateMemory (nBytes, FatalError);
  for (Item = scalarHead, filterX = 0;
       Item != NULL; Item = Item->nextScalar) {
     if (Item->filter) {
       handles[filterX] = &(Item->Var->buffer);
       nValueBytes[filterX++] = Item->Var->nValueBytes;
     }
  }
  AllocateBuffers (nRecords, 0L, NULL, &groups, filterCount, 0, handles,
		   nValueBytes, ROWmajor(inMajority), 1, FatalError);
  cdf_FreeMemory (handles, FatalError);
  cdf_FreeMemory (nValueBytes, FatalError);
  /****************************************************************************
  * Initialize new first/last records and output record counts (for those
  * variables being output).
  ****************************************************************************/
  newFirstRec = *lastRec + 1;
  newLastRec = *firstRec - 1;
  for (Item = scalarHead; Item != NULL; Item = Item->nextScalar) {
     if (Item->output) Item->Var->oRecords = 0;
  }
  for (Item = hyperHead; Item != NULL; Item = Item->nextHyper) {
     if (Item->output) Item->Var->oRecords = 0;
  }
  /****************************************************************************
  * Read values for scalars being filtered and count the number of records
  * that pass the filters.  Also update the first/last records if needed.
  ****************************************************************************/
  InitHyperParms (&hyper, &groups, 0L, &nHypers, &nValues);
  for (hyperN = 0; hyperN < nHypers; hyperN++) {
     /*************************************************************************
     * Hyper read scalar values.
     *************************************************************************/
     for (Item = scalarHead; Item != NULL; Item = Item->nextScalar) {
	if (Item->filter) {
	  status = HYPERget (inID, Item->Var->varN, Item->Var->zVar,
			     *firstRec + hyper.recNumber, hyper.recCount,
			     dimIndices_0, dimCounts_1, Item->Var->buffer);
	  DisplayStatus (status, readingCDF);
	  if (StatusBAD(status)) {
	    for (Item = scalarHead; Item != NULL; Item = Item->nextScalar) {
	       cdf_FreeMemory (Item->Var->buffer, FatalError);
	    }
	    return FALSE;
	  }
	}
     }
     /*************************************************************************
     * Until no more records...
     *************************************************************************/
     for (recX = 0;;) {
	/**********************************************************************
	* Determine next range of records in this hyper read that pass the
	* filters.
	**********************************************************************/
	firstX = FindFirstRecord (recX, scalarHead, TRUE, hyper.recCount);
	if (firstX == NO_RECORD) break;
	lastX = FindLastRecord (firstX, scalarHead, TRUE, hyper.recCount);
	/**********************************************************************
	* Calculate the absolute record number of this range and set the new
	* first/last records accordingly.
	**********************************************************************/
	recF = *firstRec + hyper.recNumber + firstX;
	recL = *firstRec + hyper.recNumber + lastX;
	newFirstRec = MINIMUM(newFirstRec,recF);
	newLastRec = MAXIMUM(newLastRec,recL);
	/**********************************************************************
	* Add to the output record count of those variables being output.
	**********************************************************************/
	for (Item = scalarHead; Item != NULL; Item = Item->nextScalar) {
	   if (Item->output) {
	     Item->Var->oRecords += COUNTx(Item->Var->maxRec,recF,recL);
	   }
	}
	for (Item = hyperHead; Item != NULL; Item = Item->nextHyper) {
	   if (Item->output) {
	     Item->Var->oRecords += COUNTx(Item->Var->maxRec,recF,recL);
	   }
	}
	/**********************************************************************
	* Increment to check for the next range of records.
	**********************************************************************/
	recX = lastX + 1;
	if (recX == hyper.recCount) break;
     }
     /*************************************************************************
     * Increment to next hyper.
     *************************************************************************/
     IncrHyperParms (&hyper, &groups, 0L, ROWmajor(inMajority), &nValues);
  }
  /****************************************************************************
  * Free memory used.
  ****************************************************************************/
  for (Item = scalarHead; Item != NULL; Item = Item->nextScalar) {
     if (Item->Var->buffer != NULL) 
       cdf_FreeMemory (Item->Var->buffer, FatalError);
  }
  /****************************************************************************
  * Check/set the new first/last records.
  ****************************************************************************/
  if (newLastRec < newFirstRec) {
    DisplayMessage ("No records in valid range.", BEEPWAIT1);
    return FALSE;
  }
  *firstRec = newFirstRec;
  *lastRec = newLastRec;
  /****************************************************************************
  * Allocate the records.
  ****************************************************************************/
  if (!AllocateRecords(outID,scalarHead,hyperHead)) return FALSE;
  return TRUE;
}

/******************************************************************************
* AllocateRecords.
******************************************************************************/

static Logical AllocateRecords (outID, scalarHead, hyperHead)
CDFid outID;
struct ItemStruct *scalarHead;
struct ItemStruct *hyperHead;
{
  CDFstatus status; struct ItemStruct *Item; int varCount = 0, varNum = 0;
  AOSs1A (allocMsg,"Allocating variable records.")
#if defined(vms)
  long rItem = rVAR_INITIALRECS_;
  long zItem = zVAR_INITIALRECS_;
#else
  long rItem = rVAR_ALLOCATERECS_;
  long zItem = zVAR_ALLOCATERECS_;
#endif
  /****************************************************************************
  * Count number of variables being allocated.
  ****************************************************************************/
  for (Item = scalarHead; Item != NULL; Item = Item->nextScalar) {
     if (Item->output && Item->Var->oRecords > 0) varCount++;
  }
  for (Item = hyperHead; Item != NULL; Item = Item->nextHyper) {
     if (Item->output && Item->Var->oRecords > 0) varCount++;
  }
  /****************************************************************************
  * Select output CDF.
  ****************************************************************************/
  status = CDFlib (SELECT_, CDF_, outID,
		   NULL_);
  DisplayStatus (status, "selecting output CDF");
  if (StatusBAD(status)) return FALSE;
  /****************************************************************************
  * Allocate records for scalar variables being output.  Also check that one
  * or more records will be written to the variable.
  ****************************************************************************/
  for (Item = scalarHead; Item != NULL; Item = Item->nextScalar) {
     if (Item->output && Item->Var->oRecords > 0) {
       struct VarStruct *Var = Item->Var;
       if (Var->sRecordsType == NO_SPARSERECORDS &&
	   Var->sArraysType == NO_SPARSEARRAYS &&
	   Var->cType == NO_COMPRESSION) {
         status = CDFlib (SELECT_, BOO(Var->zVar,zVAR_,rVAR_), Var->varNo,
			  PUT_, BOO(Var->zVar,zItem,rItem), Var->oRecords,
			  NULL_);
         DisplayStatus (status, "allocating output records");
         if (StatusBAD(status)) return FALSE;
       }
       DisplayPctComplete (PCT(varNum,varCount,1,1), allocMsg[0]);
       varNum++;
     }
  }
  /****************************************************************************
  * Allocate records for hyper variables being output.  Also check that one
  * or more records will be written to the variable.
  ****************************************************************************/
  for (Item = hyperHead; Item != NULL; Item = Item->nextHyper) {
     if (Item->output && Item->Var->oRecords > 0) {
       struct VarStruct *Var = Item->Var;
       if (Var->sRecordsType == NO_SPARSERECORDS &&
	   Var->sArraysType == NO_SPARSEARRAYS &&
	   Var->cType == NO_COMPRESSION) {
         status = CDFlib (SELECT_, BOO(Var->zVar,zVAR_,rVAR_), Var->varNo,
			  PUT_, BOO(Var->zVar,zItem,rItem), Var->oRecords,
			  NULL_);
         DisplayStatus (status, "allocating output records");
         if (StatusBAD(status)) return FALSE;
       }
       DisplayPctComplete (PCT(varNum,varCount,1,1), allocMsg[0]);
       varNum++;
     }
  }
  return TRUE;
}

/******************************************************************************
* ToWalk.
* Returns FALSE if a fatal error occurred.
******************************************************************************/

Logical ToWalk () {
  CDFstatus status; struct ItemStruct *exportHead, *Item;
  long numDims, dimSizes[CDF_MAX_DIMS], indices[CDF_MAX_DIMS], recordN = 0;
  int varCount = 0, itemN = 0, varX, searchType; long maxRec = NO_RECORD;
  static Logical first = TRUE; void *searchValue;
  struct VarStruct *Vars[MAXwalkingVARs];
  static int lineNs[1+CDF_MAX_DIMS+MAXwalkingVARs];
  static int colS[1+CDF_MAX_DIMS+MAXwalkingVARs];
  static int lenS[1+CDF_MAX_DIMS+MAXwalkingVARs];
  AOSs15 (lineS, BLANKs78, BLANKs78, BLANKs78, BLANKs78, BLANKs78, BLANKs78,
	  BLANKs78, BLANKs78, BLANKs78, BLANKs78, BLANKs78, BLANKs78, BLANKs78,
	  BLANKs78, BLANKs78)
  static int exitChars[] = {
    EXITkey_FSI, INCREMENTkey_EDIT, DECREMENTkey_EDIT, HELPkey_FSI,
    NEXTFIELDkey_EXPORT, PREVFIELDkey_EXPORT, SWITCHkey_EXPORT,
    OPTIONSkey_EXPORT, ENTERkey_FSI, NUL
  };
  static struct ItemWindowStruct IW = {
    0, 0, SCREEN_WIDTH, " Walking... ", 0, NULL, 0, lineS, 0, lineNs, colS,
    lenS, 2 + MAXwalkingVARs, 0, NULL, exitChars, REFRESHkey_FSI, FALSE, NUL,
    NUL
  };
  static char keyDefsBlank[] = "\n\n";
  static char keyDefsAbort[] = "Abort: ________\n\n";
  static char keyDefsRI[] = "Enter:  ________  Next field: __________  Increment: _______    Help: ________\nVariables: _____  Prev field: __________  Decrement: _________  Exit: ________\n";
  static char keyDefsVAR[] = "Search: ________       Next variable: ___________  Help: ________\nRecord/indices: _____  Prev variable: ___________  Exit: ________\n";
  /***************************************************************************
  * First time...
  ***************************************************************************/
  if (first) {
    char *p1 = keyDefsRI;
    EncodeKeyDefinitions (1, &p1, ENTERkey_FSI, NEXTFIELDkey_EXPORT,
			  INCREMENTkey_EXPORT, HELPkey_FSI, SWITCHkey_EXPORT,
			  PREVFIELDkey_EXPORT, DECREMENTkey_EXPORT,
			  EXITkey_FSI);
    p1 = keyDefsVAR;
    EncodeKeyDefinitions (1, &p1, ENTERkey_FSI, DECREMENTkey_EXPORT,
			  HELPkey_FSI, SWITCHkey_EXPORT, INCREMENTkey_EXPORT,
			  EXITkey_FSI);
    p1 = keyDefsAbort;
    EncodeKeyDefinitions (1, &p1, ABORTkey_EXPORT);
    first = FALSE;
  }
  /****************************************************************************
  * Build export list.  This should contain only variables being output.
  ****************************************************************************/
  BuildExportList (&exportHead, LogicalTRUE);
  /****************************************************************************
  * Check that the dimensionalities are the same.
  ****************************************************************************/
  if (!SameDimensionalities(&numDims,dimSizes,exportHead)) {
    static char msg[] = {
      "Walking allowed only if the variable dimensionalities are the same."
    };
    DisplayMessage (msg, BEEPWAIT2);
    return TRUE;
  }    
  /****************************************************************************
  * Check if enough room for each variable.
  ****************************************************************************/
  for (Item = exportHead; Item != NULL; Item = Item->nextExport) {
     varCount++;
     maxRec = MAXIMUM (maxRec, Item->Var->maxRec);
     if (varCount == MAXwalkingVARs && Item->nextExport != NULL) {
       Item->nextExport = NULL;
       DisplayMessage ("Not enough room for all the variables.", NOBEEPWAIT1);
       break;
     }
  }
  /****************************************************************************
  * Check if valid selections.
  ****************************************************************************/
  if (varCount == 0) {
    DisplayMessage ("No variables selected for walking.", BEEPWAIT2);
    return TRUE;
  }
  if (maxRec == NO_RECORD) {
    DisplayMessage ("No variable records exist.", BEEPWAIT2);
    return TRUE;
  }
  /****************************************************************************
  * Display a "loading" walking window.
  ****************************************************************************/
  strcpyX (lineS[0], "Loading...", 0);
  IW.NiLines = 1;
  ItemWindow (NEWiw, &IW, 0);
  NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
  /****************************************************************************
  * Display variable values and "walk" until exit requested.
  ****************************************************************************/
  IW.nItems = (int) (1 + numDims + varCount);
  ARRAYtoVALUE (indices, 0, numDims)
  IW.NiLines = varCount + 2;
  for (;;) {
     /*************************************************************************
     * Update with variable values...
     *************************************************************************/
     status = BuildWalkScreen (exportHead, recordN, numDims, indices,
			       lineS, lineNs, colS, lenS, Vars);
     if (StatusBAD(status)) {
       NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
       DisplayStatus (status, readingCDF);
       ItemWindow (DELETEiw, &IW);
       return (!NoMoreAccess(NULL));
     }
     ItemWindow (UPDATEiw, &IW, itemN);
     if (INCLUSIVE(0,IW.itemN,numDims)) {
       NEWkeyDEFS (EWkey, keyDefsRI, batchMode)
     }
     else {
       NEWkeyDEFS (EWkey, keyDefsVAR, batchMode)
     }
     /*************************************************************************
     * ...and do as the user commands.
     *************************************************************************/
     ItemWindow (READiw, &IW);
     switch (IW.key) {
       /***********************************************************************
       * Enter...
       ***********************************************************************/
       case ENTERkey_FSI:
	 /*********************************************************************
	 * ...the record number.
	 *********************************************************************/
	 if (IW.itemN == 0) {
	   PromptForRecordNumber (&recordN, maxRec);
	   NEWkeyDEFS (EWkey, keyDefsRI, batchMode)
	   break;
	 }
	 /*********************************************************************
	 * ...a dimension index.
	 *********************************************************************/
	 if (INCLUSIVE(1,IW.itemN,numDims)) {
	   int dimN = IW.itemN - 1;
	   PromptForDimensionIndex (&indices[dimN], dimSizes[dimN]);
	   NEWkeyDEFS (EWkey, keyDefsRI, batchMode)
	   break;
	 }
	 /*********************************************************************
	 * ...a variable value.
	 *********************************************************************/
	 varX = (int) (IW.itemN - 1 - numDims);
	 if (PromptForVariableSearch(Vars[varX],&searchType,&searchValue)) {
	   double timeMark = SystemClock ();
	   DisplayMessage ("Searching...", NOWAIT);
	   NEWkeyDEFS (EWkey, keyDefsAbort, batchMode)
	   status = SearchForVariableValue (Vars[varX], numDims, dimSizes,
					    &recordN, indices, maxRec,
					    searchType, searchValue);
	   if (StatusBAD(status)) {
	     NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
	     DisplayStatus (status, readingCDF);
	     cdf_FreeMemory (searchValue, FatalError);
	     ItemWindow (DELETEiw, &IW);
	     return (!NoMoreAccess(NULL));
	   }
	   cdf_FreeMemory (searchValue, FatalError);
	   for (;;) if (1.0 <= SystemClock() - timeMark) break;
	   DisplayMessage ("", NOWAIT);
	 }
	 NEWkeyDEFS (EWkey, keyDefsVAR, batchMode)
	 break;
       /***********************************************************************
       * Switch between record/indices and variables...
       ***********************************************************************/
       case SWITCHkey_EXPORT:
	 itemN = BOO(INCLUSIVE(0,IW.itemN,numDims),(int)(numDims + 1),0);
	 break;
       /***********************************************************************
       * Next/previous field...
       ***********************************************************************/
       case NEXTFIELDkey_EXPORT:
       case PREVFIELDkey_EXPORT: {
	 if (INCLUSIVE(0,IW.itemN,numDims))
	   itemN = BOO(IW.key == NEXTFIELDkey_EXPORT,
		       BOO(IW.itemN < numDims,IW.itemN + 1,0),
		       BOO(IW.itemN > 0,IW.itemN - 1,(int)numDims));
	 else
	   ItemWindow (BEEPiw, &IW);
	 break;
       }
       /***********************************************************************
       * Increment/decrement...
       ***********************************************************************/
       case INCREMENTkey_EXPORT:
       case DECREMENTkey_EXPORT: {
	 Logical increase = (IW.key == INCREMENTkey_EXPORT);
	 /*********************************************************************
	 * ...the record number.
	 *********************************************************************/
	 if (IW.itemN == 0) {
	   recordN = BOO(increase,
			 BOO(recordN == maxRec,0,recordN + 1),
			 BOO(recordN == 0,maxRec,recordN - 1));
	   break;
	 }
	 /*********************************************************************
	 * ...a dimension index.
	 *********************************************************************/
	 if (INCLUSIVE(1,IW.itemN,numDims)) {
	   int dimN = IW.itemN - 1;
	   indices[dimN] = BOO(increase,
			       BOO(indices[dimN] == dimSizes[dimN] - 1,
				   0,indices[dimN] + 1),
			       BOO(indices[dimN] == 0,
				   dimSizes[dimN] - 1,indices[dimN] - 1));
	   break;
	 }
	 /*********************************************************************
	 * ...to the next/previous variable.
	 *********************************************************************/
	 itemN = BOO(IW.key == DECREMENTkey_EXPORT,
		     BOO(IW.itemN < IW.nItems - 1,
			 IW.itemN + 1,(int)(1 + numDims)),
		     BOO(IW.itemN > 1 + numDims,
			 IW.itemN - 1,IW.nItems - 1));
	 break;
       }
       case OPTIONSkey_EXPORT:
	 OptionMenu ();
	 break;
       case EXITkey_FSI:
	 NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
	 ItemWindow (DELETEiw, &IW);
	 return TRUE;
       case HELPkey_FSI:
	 OnlineHelpWindow ("cdfxp.ilh", BOO(INCLUSIVE(0,IW.itemN,numDims),
					    WALKriHelpID,WALKvarHelpID));
	 break;
     }
  }
}

/******************************************************************************
* BuildWalkScreen.
******************************************************************************/

static CDFstatus BuildWalkScreen (exportHead, recordN, numDims, indices,
				  lineS, lineNs, colS, lenS, Vars)
struct ItemStruct *exportHead;
long recordN;
long numDims;
long indices[];
char *lineS[];
int lineNs[];
int colS[];
int lenS[];
struct VarStruct *Vars[];
{
  CDFstatus status, pStatus = CDF_OK; int dimN, lineN, itemN = 0, varX;
  struct ItemStruct *Item; char *rightField;
  int style;
  /****************************************************************************
  * Encode record/indices line.
  ****************************************************************************/
  MakeNUL (lineS[0]);
  CatToString (lineS[0], "Record/Indices.......", WALKleftLEN, LEFT_JUSTIFY,
	       dots);
  strcatX (lineS[0], " ", 0);
  colS[itemN] = strlen(lineS[0]);
  snprintf (EofS(lineS[0]), (size_t) sizeof(BLANKs78)-strlen(lineS[0]),
            "%ld", (long) (recordN + 1));
  lenS[itemN] = strlen(&(lineS[0][colS[itemN]]));
  lineNs[itemN++] = 0;
  strcatX (lineS[0], ":[", 0);
  for (dimN = 0; dimN < numDims; dimN++) {
     if (dimN > 0) strcatX (lineS[0], ",", 0);
     colS[itemN] = strlen(lineS[0]);
     snprintf (EofS(lineS[0]), (size_t) sizeof(BLANKs78)-strlen(lineS[0]),
               "%ld", (long) (indices[dimN] + 1));
     lenS[itemN] = strlen(&(lineS[0][colS[itemN]]));
     lineNs[itemN++] = 0;
  }
  strcatX (lineS[0], "]", 0);
  /****************************************************************************
  * A blank line is next.
  ****************************************************************************/
  MakeNUL (lineS[1]);
  /****************************************************************************
  * Encode the variable lines.
  ****************************************************************************/
  for (Item = exportHead, lineN = 2, varX = 0;
       Item != NULL; Item = Item->nextExport, lineN++, varX++) {
     EncodeString ((long) strlen(Item->Var->name), Item->Var->name,
		   lineS[lineN], -WALKleftLEN, WALKleftLEN);
     strcatX (lineS[lineN], " ", 0);
     status = CDFlib (SELECT_, VAR(Item->Var->zVar), Item->Var->varN,
			       BOO(Item->Var->zVar,zVAR_RECNUMBER_,
						   rVARs_RECNUMBER_), recordN,
			       BOO(Item->Var->zVar,zVAR_DIMINDICES_,
						   rVARs_DIMINDICES_), indices,
		      GET_, VAR_DATA(Item->Var->zVar), Item->Var->value,
		      NULL_);
     if (!sX(status,&pStatus)) return pStatus;
     colS[itemN] = strlen(lineS[lineN]);
     rightField = EofS(lineS[lineN]);
     if (TT2000dataType(Item->Var->dataType)) style = TT2000_3_STYLE;
     else style = opt.epochStyle;
     if ((Item->Var->dataType == CDF_FLOAT) ||
         (Item->Var->dataType == CDF_REAL4)) {
       if (!isnan((double)*(float *)Item->Var->value) &&
           *(float *)Item->Var->value <= DEFAULT_FLOAT_PADVALUE) {
         EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems,
			     Item->Var->value, rightField, NULL, 0,
			     WALKrightLEN, style,
			     (size_t) sizeof(BLANKs78)-strlen(lineS[lineN]));
       } else
         EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems,
			     Item->Var->value, rightField, Item->Var->format, 0,
			     WALKrightLEN, style,
			     (size_t) sizeof(BLANKs78)-strlen(lineS[lineN]));
     } else if ((Item->Var->dataType == CDF_DOUBLE) ||
                (Item->Var->dataType == CDF_REAL8)) {
       if (!isnan(*(double *)Item->Var->value) &&
           *(double *)Item->Var->value <= DEFAULT_DOUBLE_PADVALUE) {
         EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems,
			     Item->Var->value, rightField, NULL, 0,
			     WALKrightLEN, style,
			     (size_t) sizeof(BLANKs78)-strlen(lineS[lineN]));
       } else
         EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems,
			     Item->Var->value, rightField, Item->Var->format, 0,
			     WALKrightLEN, style,
			     (size_t) sizeof(BLANKs78)-strlen(lineS[lineN]));
     } else
       EncodeValuesFormat (Item->Var->dataType, Item->Var->numElems,
			   Item->Var->value, rightField, Item->Var->format, 0,
			   WALKrightLEN, style,
			   (size_t) sizeof(BLANKs78)-strlen(lineS[lineN]));
     RemoveLeadingBlanks (rightField);
     lenS[itemN] = strlen(rightField);
     lineNs[itemN++] = lineN;
     Vars[varX] = Item->Var;
  }
  return pStatus;
}

/******************************************************************************
* PromptForRecordNumber.
******************************************************************************/

static Logical PromptForRecordNumber (recordN, maxRec)
long *recordN;
long maxRec;
{
  static char value[MAXrecordNumberLEN+1]; long newRecordN;
  snprintf (value, (size_t) sizeof(value), "%ld", (long) (*recordN + 1));
  if (!PromptFor(value,MAXrecordNumberLEN,strlen(value),
		 "Enter the record number...",RECORDhelpID)) return FALSE;
  if (NULstring(value)) return FALSE;
  if (sscanf(value,"%ld",&newRecordN) != 1) {
    DisplayMessage ("Error decoding record number.", BEEPWAIT1);
    return FALSE;
  }
  if (!INCLUSIVE(1,newRecordN,maxRec+1)) {
    DisplayMessage ("Record number out of range.", BEEPWAIT1);
    return FALSE;
  }
  *recordN = newRecordN - 1L;
  return TRUE;
}

/******************************************************************************
* PromptForDimensionIndex.
******************************************************************************/

static Logical PromptForDimensionIndex (index, dimSize)
long *index;
long dimSize;
{
  static char value[MAXdimensionIndexLEN+1]; long newIndex;
  snprintf (value, (size_t) sizeof(value), "%ld", (long) (*index + 1));
  if (!PromptFor(value,MAXdimensionIndexLEN,strlen(value),
		 "Enter the dimension index...",INDEXhelpID)) return FALSE;
  if (NULstring(value)) return FALSE;
  if (sscanf(value,"%ld",&newIndex) != 1) {
    DisplayMessage ("Error decoding dimension index.", BEEPWAIT1);
    return FALSE;
  }
  if (!INCLUSIVE(1,newIndex,dimSize)) {
    DisplayMessage ("Dimension index out of range.", BEEPWAIT1);
    return FALSE;
  }
  *index = newIndex - 1L;
  return TRUE;
}

/******************************************************************************
* PromptForVariableSearch.
******************************************************************************/

static Logical PromptForVariableSearch (Var, searchType, searchValue)
struct VarStruct *Var;
int *searchType;
void **searchValue;
{
  static char value[MAXvalueLEN+1]; long numElems; int iniChar;
  static Logical first = TRUE; Logical done = FALSE;
  static int exitChars[] = { ENTERkey_FSI, EXITkey_FSI, HELPkey_FSI, NUL };
  AOSs1 (iLines, "EQUAL   LESS   LESS/EQUAL   GREATER   GREATER/EQUAL")
  static int iLineNs[] = { 0,0,0,0,0 };
  static int iCols[] = { 0,8,15,28,38 };
  static int iLens[] = { 5,4,10,7,13 };
  static struct ItemWindowStruct IW = {
    21, 0, 80, " Select search type... ", 0, NULL, 1, iLines, 5, iLineNs,
    iCols, iLens, 1, 0, NULL, exitChars, REFRESHkey_FSI, FALSE, NUL, NUL
  };
  static char keyDefs[] = {
    "Enter: ________    Help: ________    Exit: ________\n\n"
  };
  static char keyDefsBlank[] = "";
  int style;
  /****************************************************************************
  * First time...
  ****************************************************************************/
  if (first) {
    char *p1 = keyDefs;
    EncodeKeyDefinitions (1, &p1, ENTERkey_FSI, HELPkey_FSI, EXITkey_FSI);
    first = FALSE;
  }
  /****************************************************************************
  * Prompt for type of search.
  ****************************************************************************/
  NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
  ItemWindow (NEWiw, &IW, 0);
  NEWkeyDEFS (EWkey, keyDefs, batchMode)
  while (!done) {
     ItemWindow (READiw, &IW);
     switch (IW.key) {
       case ENTERkey_FSI:
	 switch (IW.itemN) {
	   case 0: *searchType = EQsearch; break;
	   case 1: *searchType = LTsearch; break;
	   case 2: *searchType = LEsearch; break;
	   case 3: *searchType = GTsearch; break;
	   case 4: *searchType = GEsearch; break;
	 }
	 NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
	 ItemWindow (DELETEiw, &IW);
	 done = TRUE;
	 break;
       case HELPkey_FSI:
	 OnlineHelpWindow ("cdfxp.ilh", SEARCHTYPEhelpID);
	 break;
       case EXITkey_FSI:
	 NEWkeyDEFS (EWkey, keyDefsBlank, batchMode)
	 ItemWindow (DELETEiw, &IW);
	 return FALSE;
     }
  }
  /****************************************************************************
  * Prompt for value.
  ****************************************************************************/
  if (TT2000dataType(Var->dataType)) style = TT2000_3_STYLE;
  else style = opt.epochStyle;
  if ((Var->dataType == CDF_FLOAT) || (Var->dataType == CDF_REAL4)) {
    if (!isnan((double)*(float *)Var->value) &&
        *(float *)Var->value <= DEFAULT_FLOAT_PADVALUE) {
      EncodeValuesFormat (Var->dataType, Var->numElems, Var->value, value,
		          NULL, 0, MAXvalueLEN, style,
		          (size_t) sizeof(value));
    } else
      EncodeValuesFormat (Var->dataType, Var->numElems, Var->value, value,
		          Var->format, 0, MAXvalueLEN, style,
		          (size_t) sizeof(value));
  } else if ((Var->dataType == CDF_DOUBLE) || (Var->dataType == CDF_REAL8)) {
    if (!isnan(*(double *)Var->value) &&
        *(double *)Var->value <= DEFAULT_DOUBLE_PADVALUE) {
      EncodeValuesFormat (Var->dataType, Var->numElems, Var->value, value,
		          NULL, 0, MAXvalueLEN, style,
		          (size_t) sizeof(value));
    } else
      EncodeValuesFormat (Var->dataType, Var->numElems, Var->value, value,
		          Var->format, 0, MAXvalueLEN, style,
		          (size_t) sizeof(value));
  } else
    EncodeValuesFormat (Var->dataType, Var->numElems, Var->value, value,
		        Var->format, 0, MAXvalueLEN, style,
		        (size_t) sizeof(value));
  RemoveLeadingBlanks (value);
  iniChar = strlen(value) - BOO(STRINGdataType(Var->dataType),1,0);
  if (!PromptFor(value,MAXvalueLEN,iniChar,
		 "Enter the search value...",VALUEhelpID)) return FALSE;
  if (NULstring(value)) return FALSE;
  if (!DecodeValues(value,Var->dataType,&numElems,
		    searchValue,opt.epochStyle)) {
    DisplayMessage ("Error decoding value.", BEEPWAIT1);
    return FALSE;
  }
  if (numElems != Var->numElems) {
    DisplayMessage ("Wrong number of elements.", BEEPWAIT1);
    cdf_FreeMemory (*searchValue, FatalError);
    return FALSE;
  }
  return TRUE;
}

/******************************************************************************
* SearchForVariableValue.
******************************************************************************/

static CDFstatus SearchForVariableValue (Var, numDims, dimSizes, recordAt,
				         indicesAt, maxRec, searchType,
					 searchValue)
struct VarStruct *Var;
long numDims;
long dimSizes[];
long *recordAt;
long indicesAt[];
long maxRec;
int searchType;
void *searchValue;
{
  CDFstatus status, pStatus = CDF_OK;
  long recordN, indices[CDF_MAX_DIMS], indicesFirst[CDF_MAX_DIMS], pct;
  long indicesLast[CDF_MAX_DIMS], nValuesPerRecord, dimCounts[CDF_MAX_DIMS];
  long nValues, valueN, nDimValues[CDF_MAX_DIMS], valueOffset, lastPct = -1L;
  size_t nBytes; void *value; int dimN;
  Logical outRowMajor = ROWmajor(MAJORITYtoOUT(opt.majority,inMajority));
  static Logical (*entryPoints[])PROTOARGs((void *, void *, long, long)) = {
    EQx, LTx, LEx, GTx, GEx
  };
  /****************************************************************************
  * Initialize...
  ****************************************************************************/
  recordN = BOO(Var->recVary,*recordAt,maxRec);
  for (dimN = 0; dimN < numDims; dimN++) {
     indices[dimN] = BOO(Var->dimVarys[dimN],indicesAt[dimN],0);
     indicesFirst[dimN] = 0;
     indicesLast[dimN] = BOO(Var->dimVarys[dimN],dimSizes[dimN] - 1,0);
     dimCounts[dimN] = indicesLast[dimN] - indicesFirst[dimN] + 1;
  }
  /****************************************************************************
  * Calculate the number of values from the current record/indices to the end
  * of the variable's values.
  ****************************************************************************/
  for (nValuesPerRecord = 1, dimN = 0; dimN < numDims; dimN++) {
     nValuesPerRecord *= dimCounts[dimN];
  }
  if (numDims > 0) {
    if (outRowMajor) {
      nDimValues[(int)(numDims-1)] = 1;
      for (dimN = (int) (numDims - 2); dimN >= 0; dimN--) {
	 nDimValues[dimN] = dimCounts[dimN+1] * nDimValues[dimN+1];
      }
    }
    else {
      nDimValues[0] = 1;
      for (dimN = 1; dimN < numDims; dimN++) {
	 nDimValues[dimN] = dimCounts[dimN-1] * nDimValues[dimN-1];
      }
    }
  }
  for (valueOffset = 0, dimN = 0; dimN < numDims; dimN++) {
     valueOffset += (indices[dimN] * nDimValues[dimN]);
  }
  nValues = nValuesPerRecord - valueOffset;
  nValues += (nValuesPerRecord * (maxRec - recordN));
  /****************************************************************************
  * Increment past the current record/indices.  Return if there is only one
  * value to search.
  ****************************************************************************/
  if (nValues > 1) {
     IncrRecordIndicesFirstLast (outRowMajor, &recordN, numDims,
				 indicesFirst, indicesLast, indices);
     nValues--;
  }
  else
    return pStatus;
  /****************************************************************************
  * Select the variable.
  ****************************************************************************/
  status = CDFlib (SELECT_, VAR(Var->zVar), Var->varN,
		   NULL_);
  if (!sX(status,&pStatus)) return pStatus;
  /****************************************************************************
  * Allocate memory to hold the value.
  ****************************************************************************/
  nBytes = (size_t) (Var->numElems * CDFelemSize(Var->dataType));
  value = (void *) cdf_AllocateMemory (nBytes, FatalError);
  /****************************************************************************
  * For each value to be searched...
  ****************************************************************************/
  for (valueN = 0; valueN < nValues; valueN++) {
     /*************************************************************************
     * Read the value.
     *************************************************************************/
     status = CDFlib (SELECT_, BOO(Var->zVar,zVAR_RECNUMBER_,
					     rVARs_RECNUMBER_), recordN,
		      NULL_);
     if (!sX(status,&pStatus)) {
       cdf_FreeMemory (value, FatalError);
       return pStatus;
     }
     if (numDims > 0) {
       status = CDFlib (SELECT_, BOO(Var->zVar,zVAR_DIMINDICES_,
					       rVARs_DIMINDICES_), indices,
		        NULL_);
       if (!sX(status,&pStatus)) {
         cdf_FreeMemory (value, FatalError);
         return pStatus;
       }
     }
     status = CDFlib (GET_, VAR_DATA(Var->zVar), value,
		      NULL_);
     if (!sX(status,&pStatus)) {
       cdf_FreeMemory (value, FatalError);
       return pStatus;
     }
     /*************************************************************************
     * If equal, set the record/indices and return.
     *************************************************************************/
     if ((entryPoints[searchType])(value,searchValue,
				   Var->dataType,Var->numElems)) {
       if (Var->recVary) *recordAt = recordN;
       for (dimN = 0; dimN < numDims; dimN++) {
	  if (Var->dimVarys[dimN]) indicesAt[dimN] = indices[dimN];
       }
       cdf_FreeMemory (value, FatalError);
       return pStatus;
     }
     /*************************************************************************
     * Update percentage complete.
     *************************************************************************/
     pct = (100L * valueN) / nValues;
     if (pct > lastPct) {
       static char msg[] = "Searching...                                                                  ";
       snprintf (&msg[75], (size_t) sizeof(msg)-75, "%2ld%%", pct);
       DisplayMessage (msg, NOWAIT);
       lastPct = pct;
     }
     /*************************************************************************
     * Check if search should be aborted.
     *************************************************************************/
     if (AbortSearch()) {
       cdf_FreeMemory (value, FatalError);
       return pStatus;
     }
     /*************************************************************************
     * Increment to the next record/indices based on the majority selected.
     *************************************************************************/
     IncrRecordIndicesFirstLast (outRowMajor, &recordN, numDims,
				 indicesFirst, indicesLast, indices);
  }
  /****************************************************************************
  * The value was not found...
  ****************************************************************************/
  DisplayMessage ("Matching value not found.", BEEPWAIT1);
  cdf_FreeMemory (value, FatalError);
  return pStatus;
}

/******************************************************************************
* AbortSearch.
******************************************************************************/

static Logical AbortSearch () {
  int key;
  if (read_input(
#if defined(CURSESui)
		 EWmsg->wid,
#endif
			    &key,PASSTHRUri,FALSE)) {
    if (key == ABORTkey_EXPORT) {
      DisplayMessage ("Aborting at user's request.", NOBEEPWAIT1);
      return TRUE;
    }
    else
      EditWindow (BEEPew, EWmsg);
  }
  return FALSE;
}


/******************************************************************************
* IsaVirtualVariable.
******************************************************************************/
Logical IsaVirtualVariable(Logical zVar, char *varName) {

  CDFstatus status;
  long      maxRec;

  /* Get the variable info. */
  maxRec = -1;
  status = CDFlib(SELECT_, ATTR_NAME_, "VIRTUAL",
                           BOO(zVar,zENTRY_NAME_,rENTRY_NAME_), varName,
                  SELECT_, BOO(zVar,zVAR_NAME_,rVAR_NAME_), varName,
                  GET_, BOO(zVar,zVAR_MAXREC_,rVAR_MAXREC_), &maxRec,
                  NULL_);

  if (status != CDF_OK || maxRec > -1)
    return FALSE;
  else
    return TRUE;
}

/******************************************************************************
* GetVirtualVariableFunc.
******************************************************************************/
CDFstatus GetVirtualVariableFunc(Logical zVar, char *varName, char *funcName) {
  CDFstatus status;
  long numElms;
  /* Get the variable info. */
  status = CDFlib(SELECT_, ATTR_NAME_, "FUNCT",
                           BOO(zVar,zENTRY_NAME_,rENTRY_NAME_), varName,
                  GET_, BOO(zVar,zENTRY_DATA_,rENTRY_DATA_), funcName,
                        BOO(zVar,zENTRY_NUMELEMS_,rENTRY_NUMELEMS_), &numElms,
                  NULL_);
  if (status != CDF_OK)
    funcName = NULL;
  else
    *(funcName+(int)numElms) = (char) 0;
  return status;
}

/******************************************************************************
* ReadRealorVirtualVariableEpoch.
******************************************************************************/
CDFstatus ReadRealorVirtualVariableEpoch (zVar, varName, ind, out) 
Logical zVar;
char *varName;
int ind;
void **out;
{
  CDFstatus status;
  long      dataType, numElements, dimensionality, dimSizes[CDF_MAX_DIMS],
            maxRec;
  char      component0[CDF_VAR_NAME_LEN256+1], 
            component1[CDF_VAR_NAME_LEN256+1];
  long dimIndices[CDF_MAX_DIMS], dimIntervals[CDF_MAX_DIMS];
  long numBytes, numElms1, numElms2;
  int ix, iy, isize;
  void *baseTime, *offTimes;
  double epoch[2];
  long long tt2000[2];
  int recNo;
  if (!IsaVirtualVariable(zVar, varName)) {
    status = CDFlib(SELECT_, BOO(zVar,zVAR_NAME_,rVAR_NAME_), varName,
                    GET_, BOO(zVar,zVAR_MAXREC_,rVAR_MAXREC_), &maxRec,
                    NULL_);
    if (status != CDF_OK) return status;
    status = CDFlib(GET_, BOO(zVar,zVAR_NUMDIMS_,rVARs_NUMDIMS_), &dimensionality,
                          BOO(zVar,zVAR_DIMSIZES_,rVARs_DIMSIZES_), dimSizes,
                    NULL_);
    if (status != CDF_OK) return status;
    isize = 1;
    if (dimensionality > 0) {
      for (ix = 0; ix < (int) dimensionality; ++ix) {
         dimIndices[ix] = 0L;
         dimIntervals[ix] = 1L;
         isize = isize * (int) dimSizes[ix];
      }
    }
    if (epochTypes[ind] != 3)
      out[ind] = (void *) malloc ((epochTypes[ind] == 2 ? 2 : 1) * isize *
                                    sizeof(double) * (maxRec + 1));
    else
      out[ind] = (void *) malloc (isize * sizeof(long long) * (maxRec + 1));

    status = CDFlib(SELECT_, BOO(zVar,zVAR_RECNUMBER_,zVAR_RECNUMBER_), 0L, 
                             BOO(zVar,zVAR_RECINTERVAL_,rVARs_RECINTERVAL_), 1L,
                             BOO(zVar,zVAR_RECCOUNT_,rVARs_RECCOUNT_), maxRec+1,
                             BOO(zVar,zVAR_DIMINDICES_,rVARs_DIMINDICES_), dimIndices,
                             BOO(zVar,zVAR_DIMINTERVALS_,rVARs_DIMINTERVALS_), dimIntervals,
                             BOO(zVar,zVAR_DIMCOUNTS_,rVARs_DIMCOUNTS_), dimSizes,
                   GET_, BOO(zVar,zVAR_HYPERDATA_,rVAR_HYPERDATA_), out[ind],
                   NULL_);

    if (status != CDF_OK) {
      free (out[ind]);
      return status;
    }
  } else {
    /* Get the component info. */
    status = CDFlib(SELECT_, ATTR_NAME_, "COMPONENT_0",
                             zENTRY_NAME_, varName,
                    GET_, zENTRY_DATA_, component0,
                          zENTRY_NUMELEMS_, &numElms1,
                    SELECT_, ATTR_NAME_, "COMPONENT_1",
                             zENTRY_NAME_, varName,
                    GET_, zENTRY_DATA_, component1,
                          zENTRY_NUMELEMS_, &numElms2,
                    NULL_);
    if (status != CDF_OK) {
      return status;
    } 
    component0[(int)numElms1] = (char) 0;
    component1[(int)numElms2] = (char) 0;
    /*
     Get the base time and offset times
     */
    status = CDFlib(SELECT_, BOO(zVar,zVAR_NAME_,rVAR_NAME_), component0,
                    GET_, BOO(zVar,zVAR_DATATYPE_,rVAR_DATATYPE_), &dataType,
                          BOO(zVar,zVAR_NUMELEMS_,rVAR_NUMELEMS_), &numElements,
                          BOO(zVar,zVAR_NUMDIMS_,rVARs_NUMDIMS_), &dimensionality,
                          BOO(zVar,zVAR_DIMSIZES_,rVARs_DIMSIZES_), dimSizes,
                          BOO(zVar,zVAR_MAXREC_,rVAR_MAXREC_), &maxRec,
                    NULL_);
    if (status != CDF_OK) 
      return status;
    if (maxRec < 0) {
      return status;
    }
    isize = 1;
    if (dimensionality > 0) {
      for (ix = 0; ix < (int) dimensionality; ++ix) {
         dimIndices[ix] = 0L;
         dimIntervals[ix] = 1L;
         isize = isize * (int) dimSizes[ix];
      }
    }
    CDFgetDataTypeSize(dataType, &numBytes);
    baseTime = (void *) malloc ((size_t) (numBytes * isize * (maxRec+1)));
    status = CDFlib(SELECT_, BOO(zVar,zVAR_RECNUMBER_,rVARs_RECNUMBER_), 0L,
                             BOO(zVar,zVAR_RECINTERVAL_,rVARs_RECINTERVAL_), 1L,
                             BOO(zVar,zVAR_RECCOUNT_,rVARs_RECCOUNT_), maxRec+1,
                             BOO(zVar,zVAR_DIMINDICES_,rVARs_DIMINDICES_), dimIndices,
                             BOO(zVar,zVAR_DIMINTERVALS_,rVARs_DIMINTERVALS_), dimIntervals,
                             BOO(zVar,zVAR_DIMCOUNTS_,rVARs_DIMCOUNTS_), dimSizes,
                    GET_, BOO(zVar,zVAR_HYPERDATA_,rVAR_HYPERDATA_), baseTime,
                    NULL_);

    if (status != CDF_OK) {
      free (baseTime);
      return status;
    }
    status = CDFlib(SELECT_, BOO(zVar,zVAR_NAME_,rVAR_NAME_), component1,
                    GET_, BOO(zVar,zVAR_DATATYPE_,rVAR_DATATYPE_), &dataType,
                          BOO(zVar,zVAR_NUMELEMS_,rVAR_NUMELEMS_), &numElements,
                          BOO(zVar,zVAR_NUMDIMS_,rVARs_NUMDIMS_), &dimensionality,
                          BOO(zVar,zVAR_DIMSIZES_,rVARs_DIMSIZES_), dimSizes,
                          BOO(zVar,zVAR_MAXREC_,rVAR_MAXREC_), &maxRec,
                    NULL_);
    if (status != CDF_OK) {
      free (baseTime);
      return status;
    }
    if (maxRec < 0) {
     free (baseTime);
      return status;
    }
    isize = 1;
    if (dimensionality > 0) {
      for (ix = 0; ix < (int) dimensionality; ++ix) {
         dimIndices[ix] = 0L;
         dimIntervals[ix] = 1L;
         isize = isize * (int) dimSizes[ix];
      }
    }
    CDFgetDataTypeSize(dataType, &numBytes);
    offTimes = (void *) malloc ((size_t) (numBytes * isize * (maxRec+1)));
    status = CDFlib(SELECT_, BOO(zVar,zVAR_RECNUMBER_,rVARs_RECNUMBER_), 0L,
                             BOO(zVar,zVAR_RECINTERVAL_,rVARs_RECINTERVAL_), 1L,
                             BOO(zVar,zVAR_RECCOUNT_,rVARs_RECCOUNT_), maxRec+1,
                             BOO(zVar,zVAR_DIMINDICES_,rVARs_DIMINDICES_), dimIndices,
                             BOO(zVar,zVAR_DIMINTERVALS_,rVARs_DIMINTERVALS_), dimIntervals,
                             BOO(zVar,zVAR_DIMCOUNTS_,rVARs_DIMCOUNTS_), dimSizes,
                    GET_, BOO(zVar,zVAR_HYPERDATA_,rVAR_HYPERDATA_), offTimes,
                    NULL_);
    if (status != CDF_OK) {
      free (baseTime);
      free (offTimes);
      return status;
    }
    status = CDFlib(SELECT_, BOO(zVar,zVAR_NAME_,rVAR_NAME_), varName,
                    GET_, BOO(zVar,zVAR_DATATYPE_,rVAR_DATATYPE_), &dataType,
                    NULL_);
    if (status != CDF_OK) {
      free (baseTime);
      free (offTimes);
      return status;
    }
    if (epochTypes[ind] != 3) 
      out[ind] = (void *) malloc ((epochTypes[ind] == 2 ? 2 : 1) * isize *
                                    sizeof(double) * (maxRec + 1 ));
    else
      out[ind] = (void *) malloc (isize * sizeof(long long) * (maxRec + 1));
    epochMaxs[ind] = maxRec;
    recNo = 0;
    if (dataType == CDF_EPOCH) {
      for (ix = 0; ix < (int) maxRec+1; ix++) {
         for (iy = 0; iy < isize; iy++) {
           if (*(((double *)offTimes)+ix*isize+iy) > 0.0)
             epoch[0] =  *(double *)baseTime +
                         *(((double *)offTimes)+ix*isize+iy) * 1000.0;   
           else
             epoch[0] = -1.0e+31;
           memcpy(((double *)(out[ind]))+recNo*isize+iy, epoch, 8);
         }
         recNo++;
      }
    }
    else if (dataType == CDF_EPOCH16) {
      for (ix = 0; ix < (int) maxRec+1; ix++) {
         for (iy = 0; iy < isize; iy++) {
           if (*(((double *)offTimes)+ix*isize+iy) > 0.0) {
             epoch[0] = *(double *)baseTime / 1000.0 + 
                        (long) *(((double *)offTimes)+ix*isize+iy);
             epoch[1] = 1.0e12 * (*(((double *)offTimes)+ix*isize+iy) -
                        (long)*(((double *)offTimes)+ix*isize+iy));
           } else {
             epoch[0] =  -1.0e+31;
             epoch[1] =  0.0;
           }
           memcpy(((double *)(out[ind]))+(recNo*isize)+iy, epoch, 16);
         } 
         recNo++;
      }
    }
    else {
      for (ix = 0; ix < (int) maxRec+1; ix++) {
         for (iy = 0; iy < isize; iy++) {
           if (*(((long long *)offTimes)+ix*isize+iy) > 0.0)
             tt2000[0] =  *(long long *)baseTime +
                          *(((long long *)offTimes)+ix*isize+iy) * 1000.0;   
           else
             tt2000[0] = FILLED_TT2000_VALUE;
           memcpy(((long long *)(out[ind]))+recNo*isize+iy, tt2000, 8);
         }
         recNo++;
      }
    }
    free (baseTime); 
    free (offTimes);
  }
  return CDF_OK;
}

/******************************************************************************
* ReadVirtualVariableApplyQaflag.
******************************************************************************/
CDFstatus ReadVirtualVariableApplyQaflag(zVar, varName, out)
Logical zVar;
char *varName;
int *out;
{
  CDFstatus status;
  long dataType, numElements, dimensionality, dimSizes[CDF_MAX_DIMS],
       maxRec;
  char component0[CDF_VAR_NAME_LEN256+1], 
       component1[CDF_VAR_NAME_LEN256+1],
       qaName[CDF_VAR_NAME_LEN256+1];
  long dimIndices[CDF_MAX_DIMS], dimIntervals[CDF_MAX_DIMS];
  long numElms1, numElms2;
  int  ix, isize1;

  /* Get the quality variable name. */
  strcpyX (qaName, varName, 0);
  snprintf(EofS(qaName), (size_t) sizeof(qaName)-strlen(qaName), "q");
  status = CDFlib(SELECT_, BOO(zVar,zVAR_NAME_,rVAR_NAME_), qaName,
                  NULL_);
  if (status != CDF_OK) {
    strcpyX (qaName, varName, 0); 
    snprintf(EofS(qaName), (size_t) sizeof(qaName)-strlen(qaName), "Q");
    status = CDFlib(SELECT_, BOO(zVar,zVAR_NAME_,rVAR_NAME_), qaName,
                    NULL_);
    if (status != CDF_OK) {
      return status;
    }
  }
  /* Get the component info. */
  status = CDFlib(SELECT_, ATTR_NAME_, "COMPONENT_0",
                           BOO(zVar,zENTRY_NAME_,rENTRY_NAME_), qaName,
                  GET_, BOO(zVar,zENTRY_DATA_,rENTRY_DATA_), component0,
                        BOO(zVar,zENTRY_NUMELEMS_,rENTRY_NUMELEMS_), &numElms1,
                  SELECT_, ATTR_NAME_, "COMPONENT_1",
                           BOO(zVar,zENTRY_NAME_,rENTRY_NAME_), qaName,
                  GET_, BOO(zVar,zENTRY_DATA_,rENTRY_DATA_), component1,
                        BOO(zVar,zENTRY_NUMELEMS_,rENTRY_NUMELEMS_), &numElms2,
                  NULL_);
  if (status != CDF_OK) {
    return status;
  } 
  component0[(int)numElms1] = (char ) 0;
  component1[(int)numElms2] = (char ) 0;
  if (strcmp(varName, component0)) {
    return status;
  }
  /*
   Get the data quality flags
   */
  status = CDFlib(SELECT_, BOO(zVar,zVAR_NAME_,rVAR_NAME_), component1,
                  GET_, BOO(zVar,zVAR_DATATYPE_,rVAR_DATATYPE_), &dataType,
                        BOO(zVar,zVAR_NUMELEMS_,rVAR_NUMELEMS_), &numElements,
                        BOO(zVar,zVAR_NUMDIMS_,rVARs_NUMDIMS_), &dimensionality,
                        BOO(zVar,zVAR_DIMSIZES_,rVARs_DIMSIZES_), dimSizes,
                        BOO(zVar,zVAR_MAXREC_,rVAR_MAXREC_), &maxRec,
                  NULL_);
  if (status != CDF_OK || maxRec < 0) {
    out = NULL;
    return status;
  }
  isize1 = 1;
  if (dimensionality > 0) {
    for (ix = 0; ix < (int) dimensionality; ++ix) {
       dimIndices[ix] = 0L;
       dimIntervals[ix] = 1L;
       isize1 = isize1 * (int) dimSizes[ix];
    }
  } else {
    dimIndices[0] = 0L;
    dimIntervals[0] = 1L;
  }

  status = CDFlib(SELECT_, BOO(zVar,zVAR_RECNUMBER_,rVARs_RECNUMBER_), 0L,
                           BOO(zVar,zVAR_RECINTERVAL_,rVARs_RECINTERVAL_), 1L,
                           BOO(zVar,zVAR_RECCOUNT_,rVARs_RECCOUNT_), maxRec+1,
                           BOO(zVar,zVAR_DIMINDICES_,rVARs_DIMINDICES_), dimIndices,
                           BOO(zVar,zVAR_DIMINTERVALS_,rVARs_DIMINTERVALS_), dimIntervals,
                           BOO(zVar,zVAR_DIMCOUNTS_,rVARs_DIMCOUNTS_), dimSizes,
                  GET_, BOO(zVar,zVAR_HYPERDATA_,rVAR_HYPERDATA_), out,
                  NULL_);

  if (status != CDF_OK) {
    return status;
  }
  return CDF_OK;
}

/******************************************************************************
* LoadVirtualEpochs.
******************************************************************************/
CDFstatus LoadVirtualEpochs(epochValues) 
void **epochValues;
{
  CDFstatus status;
  struct ItemStruct *Item;
  int ix;
  Logical zVar;
  for (ix = 0; ix < numEpochs; ++ix) {
    if (epochIndx[ix] != 1) continue;
    Item = itemHead;
    while (Item != NULL) {
      if (Item->type == VARIABLEt) {
        if (!strcmp(Item->Var->name, epochs[ix])) {
          zVar = Item->Var->zVar;
          break;
        }
      }
      Item = Item->nextItem;
    }
    status = ReadRealorVirtualVariableEpoch (zVar, epochs[ix], ix, epochValues);
  }
  return status;
}


