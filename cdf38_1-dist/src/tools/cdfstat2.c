/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF						Display statistics.
*
*  Version 1.7, 1-Jul-96, Hughes STX.
*
*  Modification history:
*
*   V1.0  29-Aug-91, J Love	Original version (for CDF V2.1).
*   V1.1  15-Nov-91, J Love	Changes for port to IBM-RS6000 (AIX)
*				involving signed vs. unsigned 'char', etc.
*   V1.2  19-Mar-92, J Love	IBM PC port.  Added filter fills option.
*   V1.3  12-Jun-92, J Love	CDF V2.3 (shareable/NeXT/zVar).
*   V1.4  20-Oct-93, J Love	CDF V2.4.
*   V1.5  12-Dec-94, J Love	CDF V2.5.
*   V1.5a  9-May-95, J Love	EPOCH styles.
*   V1.6  27-Jul-95, J Love	Hyper groups.
*   V1.7   1-Jul-96, J Love	CDF V2.6.
*   V3.3  04-Apr-11, M Liu 	Modified to handle TT2000 epoch style string.
*
******************************************************************************/

#include "cdfstats.h"

/******************************************************************************
* DISPstat.
******************************************************************************/

void DISPstat (Var, virtual)
struct VarStructST *Var;
int virtual;
{
  char line[MAX_SCREENLINE_LEN+1];

  if (!epochMonotonic) {
    DisplayMin (Var);
    DisplayMax (Var);
    DisplayFill (Var);
  }
  strcpyX (line, "       monotonic: ", MAX_SCREENLINE_LEN);

  if (Var->checkMonotonicVar)
    if (Var->monoInited)
      switch (Var->monoState) {
        case _Init:
          strcatX (line, "Steady (one value)", MAX_SCREENLINE_LEN);
          break;
        case _Steady:
          strcatX (line, "Steady (all values the same)", MAX_SCREENLINE_LEN);
          break;
        case _Increase:
          strcatX (line, "Increase", MAX_SCREENLINE_LEN);
          break;
        case _Decrease:
          strcatX (line, "Decrease", MAX_SCREENLINE_LEN);
          break;
        case _noIncrease:
          strcatX (line, "noIncrease (some values the same)",
		   MAX_SCREENLINE_LEN);
          break;
        case _noDecrease:
          strcatX (line, "noDecrease (some values the same)",
		   MAX_SCREENLINE_LEN);
          break;
        case _False:
          strcatX (line, "False", MAX_SCREENLINE_LEN);
          break;
      }
    else
      strcatX (line, (virtual==1?"n/a (0 or 1 record)":"n/a (all fill values)"),
	       MAX_SCREENLINE_LEN);
  else
    strcatX (line, "n/a", MAX_SCREENLINE_LEN);
  strcatX (line, "\n\n", MAX_SCREENLINE_LEN);
  WriteOut (OUTfp, line);

  return;
}

/******************************************************************************
* DisplayMin.
******************************************************************************/

void DisplayMin (Var)
struct VarStructST *Var;
{ 
  char line[MAX_SCREENLINE_LEN+1];
  int style;

  strcpyX (line, "         minimum: ", MAX_SCREENLINE_LEN);
  if (TT2000dataType(Var->dataTypeV)) {
    if (!ziso8601) style = TT2000_3_STYLE;
    else style = TT2000_4_STYLE;
  } else {
    if (!ziso8601) style = DEFAULTstyleSTATS;
    else style = EPOCH3_STYLE;
  }
  if (Var->minmaxInited) {
    if (FLOAT4dataType(Var->dataTypeV)) {
      if (!isnan((double)*(float *)Var->min) &&
          *(float *)Var->min <= DEFAULT_FLOAT_PADVALUE) {
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->min,
		            &line[strlen(line)], NULL, 0,
		            MAX_SCREENLINE_LEN - strlen(line), style,
		    	    (size_t) sizeof(line)-strlen(line));
      } else
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->min,
                            &line[strlen(line)], Var->format, -1,
                            MAX_SCREENLINE_LEN - strlen(line), style,
                            (size_t) sizeof(line)-strlen(line));
    } else if (DOUBLEdataType(Var->dataTypeV)) {
      if (!isnan(*(double *)Var->min) &&
          *(double *)Var->min <= DEFAULT_DOUBLE_PADVALUE) {
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->min,
		            &line[strlen(line)], NULL, 0,
		            MAX_SCREENLINE_LEN - strlen(line), style,
		    	    (size_t) sizeof(line)-strlen(line));
      } else
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->min,
                            &line[strlen(line)], Var->format, -1,
                            MAX_SCREENLINE_LEN - strlen(line), style,
                            (size_t) sizeof(line)-strlen(line));
    } else if (Var->dataTypeV == CDF_EPOCH) {
      if (!isnan(*(double *)Var->min) &&
          *(double *)Var->min <= DEFAULT_EPOCH_PADVALUE) {
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->min,
		            &line[strlen(line)], Var->format, 0, /* or NULL, 0 */
		            MAX_SCREENLINE_LEN - strlen(line), style,
		    	    (size_t) sizeof(line)-strlen(line));
      } else
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->min,
                            &line[strlen(line)], Var->format, 0,
                            MAX_SCREENLINE_LEN - strlen(line), style,
                            (size_t) sizeof(line)-strlen(line));
    } else if (Var->dataTypeV == CDF_EPOCH16) {
      if (!isnan(*(double *)Var->min) &&
          !isnan(*(((double *)Var->min)+1)) &&
          *(double *)Var->min <= DEFAULT_EPOCH16_PADVALUE &&
          *(((double *)Var->min)+1) <= DEFAULT_EPOCH16_PADVALUE) {
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->min,
		            &line[strlen(line)], Var->format, 0, /* or NULL, 0 */
		            MAX_SCREENLINE_LEN - strlen(line), style,
		    	    (size_t) sizeof(line)-strlen(line));
      } else
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->min,
                            &line[strlen(line)], Var->format, 0,
                            MAX_SCREENLINE_LEN - strlen(line), style,
                            (size_t) sizeof(line)-strlen(line));
    } else 
      EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->min,
		          &line[strlen(line)], Var->format, -1,
		          MAX_SCREENLINE_LEN - strlen(line), style,
		    	  (size_t) sizeof(line)-strlen(line));
  } else
    strcatX (line, "n/a (all fill values)", MAX_SCREENLINE_LEN);
  strcatX (line, "\n", MAX_SCREENLINE_LEN);
  WriteOut (OUTfp, line);

  if (Var->rangeCheckVar) {
    strcpyX (line, "    min in range: ", MAX_SCREENLINE_LEN);
    if (Var->minmaxInited)
      if (Var->oneINrange) {
        if (FLOAT4dataType(Var->dataTypeV)) {
          if (!isnan((double)*(float *)Var->minINrange) &&
              *(float *)Var->minINrange <= DEFAULT_FLOAT_PADVALUE) {
            EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->minINrange,
			        &line[strlen(line)], NULL, 0,
			        MAX_SCREENLINE_LEN - strlen(line), style,
			        (size_t) sizeof(line)-strlen(line));
          } else
            EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->minINrange,
                                &line[strlen(line)], Var->format, -1,
                                MAX_SCREENLINE_LEN - strlen(line), style,
                                (size_t) sizeof(line)-strlen(line));
        } else if (DOUBLEdataType(Var->dataTypeV)) {
          if (!isnan(*(double *)Var->minINrange) &&
              *(double *)Var->minINrange <= DEFAULT_DOUBLE_PADVALUE) {
            EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->minINrange,
			        &line[strlen(line)], NULL, 0,
			        MAX_SCREENLINE_LEN - strlen(line), style,
			        (size_t) sizeof(line)-strlen(line));
          } else
            EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->minINrange,
                                &line[strlen(line)], Var->format, -1,
                                MAX_SCREENLINE_LEN - strlen(line), style,
                                (size_t) sizeof(line)-strlen(line));
        } else if (Var->dataTypeV == CDF_EPOCH) {
          if (!isnan(*(double *)Var->minINrange) &&
              *(double *)Var->minINrange <= DEFAULT_EPOCH_PADVALUE) {
            EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->minINrange,
			        &line[strlen(line)], NULL, 0,
			        MAX_SCREENLINE_LEN - strlen(line), style,
			        (size_t) sizeof(line)-strlen(line));
          } else
            EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->minINrange,
                                &line[strlen(line)], Var->format, 0,
                                MAX_SCREENLINE_LEN - strlen(line), style,
                                (size_t) sizeof(line)-strlen(line));
        } else if (Var->dataTypeV == CDF_EPOCH16) {
          if (!isnan(*(double *)Var->minINrange) &&
              !isnan(*(((double *)Var->minINrange)+1)) &&
              *(double *)Var->minINrange <= DEFAULT_EPOCH16_PADVALUE &&
              *(((double *)Var->minINrange)+1) <= DEFAULT_EPOCH16_PADVALUE) {
            EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->minINrange,
			        &line[strlen(line)], NULL, 0,
			        MAX_SCREENLINE_LEN - strlen(line), style,
			        (size_t) sizeof(line)-strlen(line));
          } else
            EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->minINrange,
                                &line[strlen(line)], Var->format, 0,
                                MAX_SCREENLINE_LEN - strlen(line), style,
                                (size_t) sizeof(line)-strlen(line));
        } else
          EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->minINrange,
			      &line[strlen(line)], Var->format, -1,
			      MAX_SCREENLINE_LEN - strlen(line), style,
			      (size_t) sizeof(line)-strlen(line));
      } else
        strcatX (line, "(none)", MAX_SCREENLINE_LEN);
    else
      strcatX (line, "n/a (all fill values)", MAX_SCREENLINE_LEN);
    strcatX (line, "\n", MAX_SCREENLINE_LEN);
    WriteOut (OUTfp, line);

    strcpyX (line, "       valid min: ", MAX_SCREENLINE_LEN);
    if (FLOAT4dataType(Var->dataTypeV)) {
      if (!isnan((double)*(float *)Var->validmin) &&
          *(float *)Var->validmin <= DEFAULT_FLOAT_PADVALUE) {
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->validmin,
		            &line[strlen(line)], NULL, 0,
		            MAX_SCREENLINE_LEN - strlen(line), style,
		  	    (size_t) sizeof(line)-strlen(line));
      } else
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->validmin,
                            &line[strlen(line)], Var->format, -1,
                            MAX_SCREENLINE_LEN - strlen(line), style,
                            (size_t) sizeof(line)-strlen(line));
    } else if (DOUBLEdataType(Var->dataTypeV)) {
      if (!isnan(*(double *)Var->validmin) &&
          *(double *)Var->validmin <= DEFAULT_DOUBLE_PADVALUE) {
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->validmin,
		            &line[strlen(line)], NULL, 0,
		            MAX_SCREENLINE_LEN - strlen(line), style,
		  	    (size_t) sizeof(line)-strlen(line));
      } else
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->validmin,
                            &line[strlen(line)], Var->format, -1,
                            MAX_SCREENLINE_LEN - strlen(line), style,
                            (size_t) sizeof(line)-strlen(line));
    } else if (Var->dataTypeV == CDF_EPOCH) {
      if (!isnan(*(double *)Var->validmin) &&
          *(double *)Var->validmin <= DEFAULT_EPOCH_PADVALUE) {
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->validmin,
		            &line[strlen(line)], NULL, 0,
		            MAX_SCREENLINE_LEN - strlen(line), style,
		  	    (size_t) sizeof(line)-strlen(line));
      } else
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->validmin,
                            &line[strlen(line)], Var->format, 0,
                            MAX_SCREENLINE_LEN - strlen(line), style,
                            (size_t) sizeof(line)-strlen(line));
    } else if (Var->dataTypeV == CDF_EPOCH16) {
      if (!isnan(*(double *)Var->validmin) &&
          !isnan(*(((double *)Var->validmin)+1)) &&
          *(double *)Var->validmin <= DEFAULT_EPOCH16_PADVALUE &&
          *(((double *)Var->validmin)+1) <= DEFAULT_EPOCH16_PADVALUE) {
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->validmin,
		            &line[strlen(line)], NULL, 0,
		            MAX_SCREENLINE_LEN - strlen(line), style,
		  	    (size_t) sizeof(line)-strlen(line));
      } else
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->validmin,
                            &line[strlen(line)], Var->format, 0,
                            MAX_SCREENLINE_LEN - strlen(line), style,
                            (size_t) sizeof(line)-strlen(line));
    } else
      EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->validmin,
		          &line[strlen(line)], Var->format, -1,
		          MAX_SCREENLINE_LEN - strlen(line), style,
			  (size_t) sizeof(line)-strlen(line));
    if (Var->minmaxInited) 
      snprintf (&line[strlen(line)], (size_t) sizeof(line)-strlen(line),
	        ", %ld low value%s", Var->low, (Var->low <= 1 ? "" : "s"));
    strcatX (line, "\n\n", MAX_SCREENLINE_LEN);
    WriteOut (OUTfp, line);
  }
  return;
}

/******************************************************************************
* DisplayMax.
******************************************************************************/

void DisplayMax (Var)
struct VarStructST *Var;
{ 
  char line[MAX_SCREENLINE_LEN+1];
  int style;

  strcpyX (line, "         maximum: ", MAX_SCREENLINE_LEN);
  if (TT2000dataType(Var->dataTypeV)) {
    if (!ziso8601) style = TT2000_3_STYLE;
    else style = TT2000_4_STYLE;
  } else {
    if (!ziso8601) style = DEFAULTstyleSTATS;
    else style = EPOCH3_STYLE;
  }
  if (Var->minmaxInited) {
    if (FLOAT4dataType(Var->dataTypeV)) {
     if (!isnan((double)*(float *)Var->max) &&
         *(float *)Var->max <= DEFAULT_FLOAT_PADVALUE) {
       EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->max,
		           &line[strlen(line)], NULL, 0,
		           MAX_SCREENLINE_LEN - strlen(line), style,
			   (size_t) sizeof(line)-strlen(line));
      } else
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->max,
                            &line[strlen(line)], Var->format, -1,
                            MAX_SCREENLINE_LEN - strlen(line), style,
                            (size_t) sizeof(line)-strlen(line));
    } else if (DOUBLEdataType(Var->dataTypeV)) {
      if (!isnan(*(double *)Var->max) &&
          *(double *)Var->max <= DEFAULT_DOUBLE_PADVALUE) {
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->max,
		            &line[strlen(line)], NULL, 0,
		            MAX_SCREENLINE_LEN - strlen(line), style,
			    (size_t) sizeof(line)-strlen(line));
      } else
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->max,
                            &line[strlen(line)], Var->format, -1,
                            MAX_SCREENLINE_LEN - strlen(line), style,
                            (size_t) sizeof(line)-strlen(line));
    } else if (Var->dataTypeV == CDF_EPOCH) {
      if (!isnan(*(double *)Var->max) &&
          *(double *)Var->max <= DEFAULT_EPOCH_PADVALUE) {
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->max,
		            &line[strlen(line)], NULL, 0,
		            MAX_SCREENLINE_LEN - strlen(line), style,
			    (size_t) sizeof(line)-strlen(line));
      } else
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->max,
                            &line[strlen(line)], Var->format, 0,
                            MAX_SCREENLINE_LEN - strlen(line), style,
                            (size_t) sizeof(line)-strlen(line));
    } else if (Var->dataTypeV == CDF_EPOCH16) {
      if (!isnan(*(double *)Var->max) &&
          !isnan(*(((double *)Var->max)+1)) &&
          *(double *)Var->max <= DEFAULT_EPOCH16_PADVALUE &&
          *(((double *)Var->max)+1) <= DEFAULT_EPOCH16_PADVALUE) {
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->max,
		            &line[strlen(line)], NULL, 0,
		            MAX_SCREENLINE_LEN - strlen(line), style,
			    (size_t) sizeof(line)-strlen(line));
      } else
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->max,
                            &line[strlen(line)], Var->format, 0,
                            MAX_SCREENLINE_LEN - strlen(line), style,
                            (size_t) sizeof(line)-strlen(line));
    } else
      EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->max,
		          &line[strlen(line)], Var->format, -1,
		          MAX_SCREENLINE_LEN - strlen(line), style,
			  (size_t) sizeof(line)-strlen(line));
  } else
    strcatX (line, "n/a (all fill values)", MAX_SCREENLINE_LEN);
  strcatX (line, "\n", MAX_SCREENLINE_LEN);
  WriteOut (OUTfp, line);

  if (Var->rangeCheckVar) {
    strcpyX (line, "    max in range: ", MAX_SCREENLINE_LEN);
    if (Var->minmaxInited)
      if (Var->oneINrange) {
        if (FLOAT4dataType(Var->dataTypeV)) {
          if (!isnan((double)*(float *)Var->maxINrange) &&
              *(float *)Var->maxINrange <= DEFAULT_FLOAT_PADVALUE) {
            EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->maxINrange,
			        &line[strlen(line)], NULL, 0,
			        MAX_SCREENLINE_LEN - strlen(line), style,
			        (size_t) sizeof(line)-strlen(line));
          } else
            EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->maxINrange,
                              &line[strlen(line)], Var->format, -1,
                              MAX_SCREENLINE_LEN - strlen(line), style,
                              (size_t) sizeof(line)-strlen(line));
        } else if (DOUBLEdataType(Var->dataTypeV)) {
          if (!isnan(*(double *)Var->maxINrange) &&
              *(double *)Var->maxINrange <= DEFAULT_DOUBLE_PADVALUE) {
            EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->maxINrange,
			        &line[strlen(line)], NULL, 0,
			        MAX_SCREENLINE_LEN - strlen(line), style,
			        (size_t) sizeof(line)-strlen(line));
          } else
            EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->maxINrange,
                              &line[strlen(line)], Var->format, -1,
                              MAX_SCREENLINE_LEN - strlen(line), style,
                              (size_t) sizeof(line)-strlen(line));
        } else if (Var->dataTypeV == CDF_EPOCH) {
          if (!isnan(*(double *)Var->maxINrange) &&
              *(double *)Var->maxINrange <= DEFAULT_EPOCH_PADVALUE) {
            EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->maxINrange,
			        &line[strlen(line)], NULL, 0,
			        MAX_SCREENLINE_LEN - strlen(line), style,
			        (size_t) sizeof(line)-strlen(line));
          } else
            EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->maxINrange,
                              &line[strlen(line)], Var->format, 0,
                              MAX_SCREENLINE_LEN - strlen(line), style,
                              (size_t) sizeof(line)-strlen(line));
        } else if (Var->dataTypeV == CDF_EPOCH16) {
          if (!isnan(*(double *)Var->maxINrange) &&
              !isnan(*(((double *)Var->maxINrange)+1)) &&
              *(double *)Var->maxINrange <= DEFAULT_EPOCH16_PADVALUE &&
              *(((double *)Var->maxINrange)+1) <= DEFAULT_EPOCH16_PADVALUE) {
            EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->maxINrange,
			        &line[strlen(line)], NULL, 0,
			        MAX_SCREENLINE_LEN - strlen(line), style,
			        (size_t) sizeof(line)-strlen(line));
          } else
            EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->maxINrange,
                              &line[strlen(line)], Var->format, 0,
                              MAX_SCREENLINE_LEN - strlen(line), style,
                              (size_t) sizeof(line)-strlen(line));
	} else
          EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->maxINrange,
			      &line[strlen(line)], Var->format, -1,
			      MAX_SCREENLINE_LEN - strlen(line), style,
			      (size_t) sizeof(line)-strlen(line));
      } else
        strcatX (line, "(none)", MAX_SCREENLINE_LEN);
    else
      strcatX (line, "n/a (all fill values)", MAX_SCREENLINE_LEN);
    strcatX (line, "\n", MAX_SCREENLINE_LEN);
    WriteOut (OUTfp, line);

    strcpyX (line, "       valid max: ", MAX_SCREENLINE_LEN);
    if (FLOAT4dataType(Var->dataTypeV)) {
      if (!isnan((double)*(float *)Var->validmax) &&
          *(float *)Var->validmax <= DEFAULT_FLOAT_PADVALUE) {
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->validmax,
		            &line[strlen(line)], NULL, 0,
		            MAX_SCREENLINE_LEN - strlen(line), style,
			    (size_t) sizeof(line)-strlen(line));
      } else
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->validmax,
                            &line[strlen(line)], Var->format, -1,
                            MAX_SCREENLINE_LEN - strlen(line), style,
                            (size_t) sizeof(line)-strlen(line));
    } else if (DOUBLEdataType(Var->dataTypeV)) {
      if (!isnan(*(double *)Var->validmax) &&
          *(double *)Var->validmax <= DEFAULT_DOUBLE_PADVALUE) {
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->validmax,
		            &line[strlen(line)], NULL, 0,
		            MAX_SCREENLINE_LEN - strlen(line), style,
		  	    (size_t) sizeof(line)-strlen(line));
      } else
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->validmax,
                            &line[strlen(line)], Var->format, -1,
                            MAX_SCREENLINE_LEN - strlen(line), style,
                            (size_t) sizeof(line)-strlen(line));
    } else if (Var->dataTypeV == CDF_EPOCH) {
      if (!isnan(*(double *)Var->validmax) &&
          *(double *)Var->validmax <= DEFAULT_EPOCH_PADVALUE) {
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->validmax,
		            &line[strlen(line)], NULL, 0,
		            MAX_SCREENLINE_LEN - strlen(line), style,
		  	    (size_t) sizeof(line)-strlen(line));
      } else
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->validmax,
                            &line[strlen(line)], Var->format, 0,
                            MAX_SCREENLINE_LEN - strlen(line), style,
                            (size_t) sizeof(line)-strlen(line));
    } else if (Var->dataTypeV == CDF_EPOCH16) {
      if (!isnan(*(double *)Var->validmax) &&
          !isnan(*(((double *)Var->validmax)+1)) &&
          *(double *)Var->validmax <= DEFAULT_EPOCH16_PADVALUE &&
          *(((double *)Var->validmax)+1) <= DEFAULT_EPOCH16_PADVALUE) {
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->validmax,
		            &line[strlen(line)], NULL, 0,
		            MAX_SCREENLINE_LEN - strlen(line), style,
		  	    (size_t) sizeof(line)-strlen(line));
      } else
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->validmax,
                            &line[strlen(line)], Var->format, 0,
                            MAX_SCREENLINE_LEN - strlen(line), style,
                            (size_t) sizeof(line)-strlen(line));
    } else
      EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->validmax,
		          &line[strlen(line)], Var->format, -1,
		          MAX_SCREENLINE_LEN - strlen(line), style,
			  (size_t) sizeof(line)-strlen(line));
    if (Var->minmaxInited) 
      snprintf (&line[strlen(line)], (size_t) sizeof(line)-strlen(line),
	        ", %ld high value%s", Var->high, (Var->high <= 1 ? "" : "s"));
    strcatX (line, "\n\n", MAX_SCREENLINE_LEN);
    WriteOut (OUTfp, line);
  }
  return;
}

/******************************************************************************
* DisplayFill.
******************************************************************************/

void DisplayFill (Var)
struct VarStructST *Var;
{
  char line[MAX_SCREENLINE_LEN+1];
  int style;

  if (Var->ignoreFillsVar) {
    strcpyX (line, "      fill value: ", MAX_SCREENLINE_LEN);
    if (TT2000dataType(Var->dataTypeV)) {
      if (!ziso8601) style = TT2000_3_STYLE;
      else style = TT2000_4_STYLE;
    } else {
      if (!ziso8601) style = DEFAULTstyleSTATS;
      else style = EPOCH3_STYLE;
    }
    if (FLOAT4dataType(Var->dataTypeV)) {
      if (!isnan((double)*(float *)Var->fillval) &&
          *(float *)Var->fillval <= DEFAULT_FLOAT_PADVALUE) {
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->fillval,
		            &line[strlen(line)], NULL, 0,
		            MAX_SCREENLINE_LEN - strlen(line), style,
		  	    (size_t) sizeof(line)-strlen(line));
      } else
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->fillval,
                            &line[strlen(line)], Var->format, -1,
                            MAX_SCREENLINE_LEN - strlen(line), style,
                            (size_t) sizeof(line)-strlen(line));
    } else if (DOUBLEdataType(Var->dataTypeV)) {
      if (!isnan(*(double *)Var->fillval) &&
          *(double *)Var->fillval <= DEFAULT_DOUBLE_PADVALUE) {
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->fillval,
		            &line[strlen(line)], NULL, 0,
		            MAX_SCREENLINE_LEN - strlen(line), style,
		    	    (size_t) sizeof(line)-strlen(line));
      } else
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->fillval,
                            &line[strlen(line)], Var->format, -1,
                            MAX_SCREENLINE_LEN - strlen(line), style,
                            (size_t) sizeof(line)-strlen(line));
    } else if (Var->dataTypeV == CDF_EPOCH) {
      if (!isnan(*(double *)Var->fillval) &&
          *(double *)Var->fillval <= DEFAULT_EPOCH_PADVALUE) {
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->fillval,
		            &line[strlen(line)], NULL, 0,
		            MAX_SCREENLINE_LEN - strlen(line), style,
		    	    (size_t) sizeof(line)-strlen(line));
      } else
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->fillval,
                            &line[strlen(line)], Var->format, 0,
                            MAX_SCREENLINE_LEN - strlen(line), style,
                            (size_t) sizeof(line)-strlen(line));
    } else if (Var->dataTypeV == CDF_EPOCH16) {
      if (!isnan(*(double *)Var->fillval) &&
          !isnan(*(((double *)Var->fillval)+1)) &&
          *(double *)Var->fillval <= DEFAULT_EPOCH16_PADVALUE && 
          *(((double *)Var->fillval)+1) <= DEFAULT_EPOCH16_PADVALUE) {
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->fillval,
		            &line[strlen(line)], NULL, 0,
		            MAX_SCREENLINE_LEN - strlen(line), style,
		    	    (size_t) sizeof(line)-strlen(line));
      } else
        EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->fillval,
                            &line[strlen(line)], Var->format, 0,
                            MAX_SCREENLINE_LEN - strlen(line), style,
                            (size_t) sizeof(line)-strlen(line));
    } else    
      EncodeValuesFormat (Var->dataTypeV, Var->numElemsV, Var->fillval,
		          &line[strlen(line)], Var->format, -1,
		          MAX_SCREENLINE_LEN - strlen(line), style,
		  	  (size_t) sizeof(line)-strlen(line));
    snprintf (&line[strlen(line)], (size_t) sizeof(line)-strlen(line),
	      ", %ld fill value%s", Var->fills, (Var->fills <= 1 ? "" : "s"));
    strcatX (line, "\n\n", MAX_SCREENLINE_LEN);
    WriteOut (OUTfp, line);
  }
  return;
}
