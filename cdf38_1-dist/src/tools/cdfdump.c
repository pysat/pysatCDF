/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                 CDFdump - dump data contents in a CDF file.
*
*  Version 1.0, 9-Jul-06, Raytheon.
*
*  Modification history:
*
*   V1.0  09-Jul-06, M Liu      Original version.
*   V3.3  10-Jan-09, M Liu      Validate a file before it is used.
*   V3.5  10-Sep-13, M Liu      Added "-col2row" option.
*
******************************************************************************/

#define CDFDUMP
#include "cdfdump.h"

/******************************************************************************
* Increased stack size for Borland C on IBM PC.
******************************************************************************/

#if defined(BORLANDC)
extern unsigned _stklen = BORLANDC_STACK_SIZE;
#endif

double EpochScale (long, char *);
void *HandleVariable (long, Logical, long *);
void *HandleReal (long, Logical, long *);
void *HandleVirtual (long, Logical, long *);
Logical IsVirtualVar (long, Logical);
long GetVirtualVarMaxRecord (long, Logical);
void *GetVirtualVarRecords (long, Logical);

/******************************************************************************
* Global variables.
******************************************************************************/
#ifdef WIN32
  static unsigned int oldCodePage = 0;
#endif

FILE *fp;
/******************************************************************************
* Main.
******************************************************************************/

#if !defined(win32) || (defined(win32) && defined(ALONE))
MAIN {
  Logical success = TRUE;
  strcpyX (pgmName, "CDFdump", MAX_PROGRAM_NAME_LEN);
  success = DumpCDF (argc, argv);
#ifdef WIN32
  if (oldCodePage != 0) SetConsoleOutputCP(oldCodePage);
#endif
  return BOO(success,EXIT_SUCCESS_,EXIT_FAILURE_);
}
#endif

/******************************************************************************
* DumpCDF.
******************************************************************************/

Logical DumpCDF (argC, argV)
int  argC;
char *argV[];
{

CDFstatus status;
long numDims, dimSizes[CDF_MAX_DIMS], dimIntervals[CDF_MAX_DIMS], 
     dimIndices[CDF_MAX_DIMS], dimCounts[CDF_MAX_DIMS];
long version, release, increment, format, majority, encoding, numrVars,
     numzVars, numAttrs, numgAttrs, numvAttrs, numgEntries, numrEntries, 
     numzEntries, checksum, lastUpdated;
long dataType, numElems, scope, recVary, dataTypeX, numElemsX;
long dimVarys[CDF_MAX_DIMS];
long compression, compressParms[CDF_MAX_DIMS], cPct;
long numRecs, numAllocRecs, maxRec, sp, bf, maxAllocRec;
long nHypers, hyperN, nValuesPerRec, nLogValuesPerRec, 
     nValuesPerDim[CDF_MAX_DIMS], nValues;
char CDFpath[CDF_PATHNAME_LEN+1], outputFile[CDF_PATHNAME_LEN+1];
char CDFname[DU_MAX_NAME_LEN+1], dir[DU_MAX_DIR_LEN+1];
char varName[CDF_VAR_NAME_LEN256+1], attrName[CDF_ATTR_NAME_LEN256+1];
char copyRight[CDF_COPYRIGHT_LEN+1];
char oDir[DU_MAX_DIR_LEN+1], oName[DU_MAX_NAME_LEN+1];
char tText[256];
char shortStr1[20], shortStr2[20], *prtFormat = NULL;
void *value, *working = NULL;
void *value2 = NULL;
unsigned char *checkPtr = NULL;
Logical  toCol2Row = FALSE;
Byte1 **handles[1], *buffer = NULL;
char **vars = NULL;
int  numVars = 0;
size_t nValueBytes[1];
size_t offset, nBytes;
struct HyperStruct hyperSt;
struct GroupStruct groups;
Logical Z, pad, ifound;
int  varNum, i, ii, j, k, ir, imore, iskip, icount = 0, ij, precision;
long tmpLong[1] = {0};
Logical qopError = FALSE;
Logical detect = FALSE;
QOP *qop;
Logical virtual = FALSE;
static char *validQuals[] = {
  "dump", "output", "format", "noformat", "significant", "vars",
  "about", "header", "noheader", "recordrange", "col2row", "detect",
  "epoch_virtual", "ziso8601", NULL 
};
static int optRequired[] = {
  TRUE, TRUE, FALSE, FALSE, TRUE, TRUE,
  FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE,
  0
};
int numValidDumps = 6;
static char *validDumps[] = {
  "all", "data", "nodata", "metadata", "global", "variable" };
char *locale;

/******************************************************************************
* Parse qualifiers/options/parameters (QOP).
******************************************************************************/

switch (argC) {
  case 1:
    PageOLH ("cdfdump.olh", argV[0]);
    return TRUE;
  case 2:
    if (strcmp(argV[1],"-java") == 0) {
      pagingOn = FALSE;
      PageOLH ("cdfdumpj.olh", argV[0]);
      return TRUE;
    }
  default:
    qop = Qop (argC, argV, validQuals, optRequired);
    if (qop == NULL) return FALSE;
    /************************************************************************
    * Check for the `dump' qualifier.
    ************************************************************************/
    if (qop->qualEntered[DUMPqual]) {
      strcpyX (dumpOption, qop->qualOpt[DUMPqual], 20);
      Z = FALSE;
      for (i = 0; i < numValidDumps; i++) {
        if (strcmpIgCase(validDumps[i], dumpOption) == 1) {
          dump = i;
          Z = TRUE;
          break;
        }
      }
      if (!Z) {
        if (strcmpIgCase(dumpOption, "meta") == 1) {
          dump = 3;
          Z = TRUE;
	}
        if (!Z) {
          DisplayError ("Invalid dump option.");
          qopError = TRUE;
          return TRUE;
        }
      }
    } else
      MakeNUL(dumpOption);
    /************************************************************************
    * Check for the `format' qualifier.
    ************************************************************************/
    qopError = qopError |! TFqualifier(qop,&useFormat,FORMATqual,NOFORMATqual,
                                       DEFAULTformatDUMP, "format");
    /************************************************************************
    * Check for the `detect' qualifier.
    ************************************************************************/
    if (qop->qualEntered[DETECTqual]) {
      detect = TRUE;
    }
    /**************************************************************************
    * Check for `about' qualifier.
    **************************************************************************/
    if (qop->qualEntered[ABOUTqual]) {
      DisplayIdentification (pgmName);
      cdf_FreeMemory (qop, FatalError);
      return TRUE;
    }
    /**************************************************************************
    * Check for `precision' qualifier.
    **************************************************************************/
    if (qop->qualEntered[PRECISIONqual]) {
      if (sscanf(qop->qualOpt[PRECISIONqual],"%d,",&precision) != 1) {
        DisplayError ("Invalid precision option.");
        qopError = TRUE;
      } else {
        if (precision < 1) {
          DisplayError ("Invalid precision option.");
          qopError = TRUE;
        }
      }
    } else
      precision = 0;
    /**************************************************************************
    * Check for `header' qualifier.
    **************************************************************************/
    qopError = qopError |! TFqualifier(qop,&header,HEADERqual,NOHEADERqual,
                                       DEFAULTheaderDUMP, "header");
    /**************************************************************************
    * Check for `col2row' qualifier.
    **************************************************************************/
    if (qop->qualEntered[COL2ROWqual]) {
      col2row = TRUE;
    }
    /************************************************************************
    * Check for `output' qualifier.
    ************************************************************************/
    if (qop->qualEntered[OUTPUTqual]) {
      if (!ValidateQual(qop->qualOpt[OUTPUTqual], validQuals))
        strcpyX (outputFile, qop->qualOpt[OUTPUTqual], CDF_PATHNAME_LEN);
      else {
        DisplayError ("Invalid output option.");
        qopError = TRUE;
      }
    } else
      MakeNUL(outputFile);
    /************************************************************************
    * Check for `vars' qualifier.
    ************************************************************************/
    if (qop->qualEntered[VARSqual]) {
      strcpyX (varsOption, qop->qualOpt[VARSqual], 512);
      numVars = GetNumTokens (',', varsOption);
      if (numVars > 0) {
        vars = (char **) cdf_AllocateMemory ((size_t)sizeof(char *) * numVars,
                                             FatalError);
        for (i = 0; i < numVars; i++) 
          vars[i] = (char *) cdf_AllocateMemory ((size_t)CDF_VAR_NAME_LEN256+1,
                                                 FatalError);
        ParseStringForTokens (',', varsOption, vars);
      }   
    } else
      MakeNUL(varsOption);
    /*********************************************************************
    * Check for `recordrange' qualifier.
    *********************************************************************/
    if (qop->qualEntered[RECORDRANGEqual]) {
      if (!ParseRecordRange(qop->qualOpt[RECORDRANGEqual],
                            &startingRec,&endingRec)) {
        DisplayError ("Illegal record range.");
        qopError = TRUE;
      }
    }
    else {
      startingRec = useDEFAULTrecordNUM;
      endingRec = useDEFAULTrecordNUM;
    }
    /************************************************************************
    * Check for the `epoch_virtual' qualifier.
    ************************************************************************/
    if (qop->qualEntered[EPOCHVVqual]) {
      epochVV = TRUE;
    } else
      epochVV = FALSE;
    /**************************************************************************
    * Check for /ZISO8601,-ziso8601 qualifier.
    **************************************************************************/
    if (qop->qualEntered[ZISO8601qual]) {
      ziso8601 = TRUE;
    } else
      ziso8601 = FALSE;
    /**************************************************************************
    * Get CDF pathname.
    **************************************************************************/
    if (qop->Nparms < 1) {
      DisplayError ("Missing parameter.");
      qopError = TRUE;
    }
    else { 
      strcpyX (CDFpath, qop->parms[CDFPATHparm], DU_MAX_PATH_LEN);
      ParsePath (CDFpath, dir, CDFname);
    }
    if (!NULstring(outputFile)) {
      if (strncmpIgCase(outputFile, "source", 6) != 0) {
        int loc;
        char mytmp[CDF_PATHNAME_LEN+1];
        strcpyX (mytmp, CDFpath, CDF_PATHNAME_LEN);
        loc = StrlaststrIgCase(mytmp, ".cdf");
        if (loc == -1) strcatX (EofS(mytmp), ".txt", 0);
        else strcpyX (mytmp+loc, ".txt", 0);
        strcpyX (outputFile, mytmp, 0);
      } else {
        ParsePath (outputFile, oDir, oName);
        if (strchr(oName,'.') == NULL) 
          strcatX (outputFile, ".txt", DU_MAX_PATH_LEN);
      }
    }
    if (!strcmp(outputFile, CDFpath)) {
      DisplayError ("Output file name is identical to source CDF file.");
      qopError = TRUE;
    }
    /**************************************************************************
    * Free QOP memory and check for an error.
    **************************************************************************/
    cdf_FreeMemory (qop, FatalError);
    if (qopError) return FALSE;
    break;
}

  fp = BOO(!NULstring(outputFile),FOPEN(outputFile,"w"),stdout);

  /****************************************************************************
  * Display information.
  ****************************************************************************/
  if (!detect && header) {
    fprintf (fp, "Dumping cdf from \"");
    fprintf (fp, "%s", CDFpath);
    if (!EndsWithIgCase(CDFpath, ".cdf"))
      fprintf (fp, ".cdf");
    if (!NULstring(outputFile)) { 
      fprintf (fp, "\"  to  \"");
      fprintf (fp, "%s", outputFile);
    }
    fprintf (fp, "\"...\n");
  }

  if (detect) CDFsetValidate (VALIDATEFILEon);
  /****************************************************************************
  * Open/inquire CDF.
  ****************************************************************************/

  status = CDFlib (OPEN_, CDF_, CDFpath, &id,
                   NULL_);
  if (status < CDF_OK) return QuitCDF (CDFpath, "1.0", status, CDFpath);

  status = CDFlib (SELECT_, CDF_, id,
                   GET_, CDF_VERSION_, &version,
                         CDF_RELEASE_, &release,
                         CDF_INCREMENT_, &increment,
                         CDF_FORMAT_, &format,
                         CDF_MAJORITY_, &majority,
                         CDF_ENCODING_, &encoding,
                         CDF_COMPRESSION_, &compression, compressParms, &cPct,
                         CDF_NUMrVARS_, &numrVars,
                         CDF_NUMzVARS_, &numzVars,
                         CDF_NUMATTRS_, &numAttrs,
                         CDF_NUMgATTRS_, &numgAttrs,
                         CDF_NUMvATTRS_, &numvAttrs,
                         CDF_COPYRIGHT_, copyRight,
                         CDF_CHECKSUM_, &checksum,
                   NULL_);
  if (status < CDF_OK) return QuitCDF (CDFpath, "2.0", status, CDFpath);

  /**********************************/
  /* Print out the file information */
  /**********************************/

  if (!detect) {
    fprintf (fp, "File Info\n=========================================\n");
    fprintf (fp, "CDF File:     %s\n",CDFname);
    fprintf (fp, "Version:      %ld.%ld.%ld\n",version,release,
							  increment);
    fprintf (fp, "%s\n",copyRight);
    fprintf (fp, "Format:       %s\n",FormatToken(format));
    fprintf (fp, "Encoding:     %s\n",EncodingToken(encoding));
    fprintf (fp, "Majority:     %s\n",MajorityToken(majority));
    fprintf (fp, "NumrVars:     %ld\n",numrVars);
    fprintf (fp, "NumzVars:     %ld\n",numzVars);
    fprintf (fp, "NumAttrs:     %ld (%ld global, %ld variable)\n",
  		 numAttrs, numgAttrs, numvAttrs);
    fprintf (fp, "Compression:  %s\n",CompressionToken(compression,
                                      compressParms));
    if (!PriorTo ("3.2.0", (Int32) version, (Int32) release,
		  (Int32) increment)) 
      fprintf (fp, "Checksum:     %s\n", ChecksumToken(checksum));
    if (!PriorTo ("3.5.0", (Int32) version, (Int32) release,
                  (Int32) increment)) {
      status = CDFlib (GET_, CDF_LEAPSECONDLASTUPDATED_, &lastUpdated,
                       NULL_);
      if (status < CDF_OK) return QuitCDF (CDFpath, "2.1", status, CDFpath);
      fprintf (fp, "LeapSecondLastUpdated:     %ld\n", lastUpdated);
    }
  }
  
#if !defined(WIN32)
  setlocale(LC_CTYPE, "en_US.UTF-8");
#else
  setlocale(LC_CTYPE, ".UTF8");
  oldCodePage = GetConsoleOutputCP();
  SetConsoleOutputCP(65001); 
#endif

  /**********************************************/
  /* Print out the Global Attribute information */
  /**********************************************/

  if (dump != DATADUMP && dump != VARIABLEDUMP) {
    if (numgAttrs > 0) {
      if (numgAttrs > 1) {
        if (!detect) fprintf (fp, "\nGlobal Attributes (%ld attributes)\n",
			      numgAttrs);
      } else
        if (!detect) fprintf (fp, "\nGlobal Attribute (%ld attribute)\n",
			      numgAttrs);
      if (!detect) fprintf (fp, "=========================================\n");

      j = 0;
      for (i = 0; i < (int) numAttrs; i++) {
        status = CDFlib (SELECT_, ATTR_, (long) i,
                         GET_, ATTR_SCOPE_, &scope,
                         NULL_);
        if (status < CDF_OK) return QuitCDF (CDFpath, "3.0", status, NULL);

        if (scope != GLOBAL_SCOPE) continue;

        status = CDFlib (GET_, ATTR_NAME_, attrName,
                               ATTR_NUMgENTRIES_, &numgEntries,
                         NULL_);
        if (status < CDF_OK) return QuitCDF (CDFpath, "4.0", status, attrName);

        if (numgEntries <= 1) {
          if (!detect) fprintf (fp, "%s (%ld entry):\n", attrName, numgEntries);
        } else
          if (!detect)
            fprintf (fp, "%s (%ld entries):\n", attrName, numgEntries);
        j = k = 0;
        if (numgEntries > 0) {
          do {
            status = CDFlib (SELECT_, gENTRY_, (long) j,
                             GET_, gENTRY_DATATYPE_, &dataTypeX,
                                   gENTRY_NUMELEMS_, &numElemsX,
                             NULL_);
            if (status < CDF_OK) {
              j++;
              continue;
            } else {
              if (STRINGdataType(dataTypeX)) checkPtr = NULL;
              if (STRINGdataType(dataTypeX)) imore = 1;
              else imore = 0;
              nBytes = (size_t) (CDFelemSize(dataTypeX) * (numElemsX + imore));
              value = cdf_AllocateMemory ((size_t) nBytes, FatalError);
              status = CDFlib (GET_, gENTRY_DATA_, value,
                               NULL_);
              if (status < CDF_OK)
                return QuitCDF (CDFpath, "5.0", status, attrName);
              if (STRINGdataType(dataTypeX)) {
		int cnt;
		*((char *)value+numElemsX) = '\0';
		checkPtr = UTF8Check ((unsigned char *)value, (int)numElemsX,
			              &cnt);
	      }
	      if (detect) {
		if (STRINGdataType(dataTypeX)) {
	          if (checkPtr != NULL) 
	            printf("** Attr:%s entry:%d failed UTF-8 check...\n",
                           attrName, j);
	        }
		++k; ++j;
                cdf_FreeMemory (value, FatalError);
		continue;
	      }
              fprintf (fp, "\t%d (CDF_%s/%ld): \t",
                       j,DataTypeToken(dataTypeX),numElemsX);
              if (STRINGdataType(dataTypeX)) {
                fprintf (fp, "\"%s\"\n", (char *)value);
              } else {
                int ix, style;
                char tmp[256];
                if (TT2000dataType(dataTypeX)) {
                  if (!ziso8601) style = TT2000_3_STYLE;
                  else style = TT2000_4_STYLE;
                } else {
                  if (!ziso8601) style = DEFAULTstyleDUMP;
                  else style = EPOCH3_STYLE;
                }
                for (ix = 0; ix < numElemsX; ++ix) {
                  EncodeValue (dataTypeX,
                               (char *)value+ix*CDFelemSize(dataTypeX),
                               tmp, style, (size_t) sizeof(tmp));
                  if (ix == 0) strcpyX (tText, tmp, 0);
                  else strcatX (tText, tmp, 255);
                  if (numElemsX > 1 && ix < (numElemsX-1))
                    strcatX (tText, ", ", 255);
                }
                fprintf (fp, "%s\n", tText);
              }
	      ++k; ++j;
              cdf_FreeMemory (value, FatalError);
            }
          } while (k < numgEntries);
        }
      }
    }
  }
  
  if (dump == GLOBALDUMP) {
    status = CDFlib (CLOSE_, CDF_,
                     NULL_);
    Exit;
  }

  if (dump != DATADUMP) {
    if (numvAttrs > 0) {
      if (numvAttrs > 1) {
        if (!detect) fprintf (fp, "\nVariable Attributes (%ld attributes)\n",
			      numvAttrs);
      } else
        if (!detect) fprintf (fp, "\nVariable Attribute (%ld attribute)\n",
			      numvAttrs);
      if (!detect) fprintf (fp, "=========================================\n");

      for (i = 0; i < (int) numAttrs; i++) {
        status = CDFlib (SELECT_, ATTR_, (long) i,
                         GET_, ATTR_SCOPE_, &scope,
                         NULL_);
        if (status < CDF_OK) return QuitCDF (CDFpath, "6.0", status, NULL);

        if (scope == GLOBAL_SCOPE) continue;

        status = CDFlib (GET_, ATTR_NAME_, attrName,
                               ATTR_NUMrENTRIES_, &numrEntries,
                               ATTR_NUMzENTRIES_, &numzEntries,
                         NULL_);
        if (status < CDF_OK) return QuitCDF (CDFpath, "7.0", status, attrName);

        if (!detect) fprintf (fp, "%s\n", attrName);
      }
    }
  }

  /**************************************/
  /* Print out the Variable information */ 
  /**************************************/

  if (!detect) {
    fprintf (fp, "\nVariable Information ");
    if (numVars == 0) {
      if (numrVars <= 1) fprintf (fp, "(%ld rVariable, ", numrVars);
      else fprintf (fp, "(%ld rVariables, ", numrVars);
      if (numzVars <= 1) fprintf (fp, "%ld zVariable)\n", numzVars);
      else fprintf (fp, "%ld zVariables)\n", numzVars); 
    } else
    fprintf (fp, "\n");
    fprintf (fp, "===========================================================\n");
  }

  if (numrVars > 0) {
    status = CDFlib (GET_, rVARs_NUMDIMS_, &numDims,
                           rVARs_DIMSIZES_, dimSizes,
                     NULL_);
    if (status < CDF_OK) return QuitCDF (CDFpath, "8.0", status, NULL);

    for (i = 0; i < numrVars; i++) {
       status = CDFlib (SELECT_, rVAR_, (long) i,
                        GET_, rVAR_NAME_, varName,
                              rVAR_DATATYPE_, &dataType,
                              rVAR_NUMELEMS_, &numElems,
                              rVAR_DIMVARYS_, dimVarys,
                              rVAR_RECVARY_, &recVary,
                              rVAR_SPARSERECORDS_, &sp,
                        NULL_);
      if (status < CDF_OK) return QuitCDF (CDFpath, "9.0", status, varName);
      if (detect) continue;
      if (numVars > 0) {
        ifound = FALSE;
        for (ii = 0; ii < numVars; ii++) {
           if (!strcmp(varName, vars[ii])) {
             ifound = TRUE;
             break;
           }
        }
        if (!ifound) continue;
      }

      fprintf (fp, "%s", varName);
      j = (int) strlen(varName);
      if (j < 22) Ncharacters (fp, (22-j), ' ');
      else fprintf (fp, "%s", " ");
      if (numDims == 0)
        fprintf (fp, "CDF_%s/%ld\t0:[]\t%s/  ", 
                 DataTypeToken(dataType), numElems, BOO(recVary,"T","F"));
      else if (numDims == 1)
        fprintf (fp, "CDF_%s/%ld\t%ld:[%ld]\t%s/%s", 
                 DataTypeToken(dataType), numElems, numDims, dimSizes[0], 
                 BOO(recVary,"T","F"), BOO(dimVarys[0],"T","F"));
      else {
        for (j = 0; j < numDims; j++) {
          if (j == 0) {
            snprintf (shortStr1, (size_t) sizeof(shortStr1),
		      "%ld", dimSizes[j]);
            snprintf (shortStr2, (size_t) sizeof(shortStr2), 
		      "%s", BOO(dimVarys[j],"T","F"));
          } else {
            strcat(shortStr1, ",");
            snprintf (EofS(shortStr1),
		      (size_t) sizeof(shortStr1)-strlen(shortStr1), "%ld", 
		      dimSizes[j]);
            snprintf (EofS(shortStr2),
		      (size_t) sizeof(shortStr2)-strlen(shortStr2), "%s", 
		      BOO(dimVarys[j],"T","F"));
          }
        }
        fprintf (fp, "CDF_%s/%ld\t%ld:[%s]\t%s/%s", 
                 DataTypeToken(dataType), numElems, numDims, shortStr1,
                 BOO(recVary,"T","F"), shortStr2);
      }
      if (sp != NO_SPARSERECORDS)
        fprintf (fp, " %s", SparsenessToken(sp, 0L, tmpLong));
      fprintf (fp, "\n");
    }
  }

  if (numzVars > 0) {
    for (i = 0; i < numzVars; i++) {
       status = CDFlib (SELECT_, zVAR_, (long) i,
                        GET_, zVAR_NAME_, varName,
                              zVAR_DATATYPE_, &dataType,
                              zVAR_NUMELEMS_, &numElems,
                              zVAR_NUMDIMS_, &numDims,
                              zVAR_DIMSIZES_, dimSizes,
                              zVAR_DIMVARYS_, dimVarys,
                              zVAR_RECVARY_, &recVary,
                              zVAR_SPARSERECORDS_, &sp,
                        NULL_);
      if (status < CDF_OK) return QuitCDF (CDFpath, "10.0", status, varName);
      if (detect) continue;
      if (numVars > 0) {
        ifound = FALSE;
        for (ii = 0; ii < numVars; ii++) {
           if (!strcmp(varName, vars[ii])) {
             ifound = TRUE;
             break;
           }
        }
        if (!ifound) continue;
      }

      fprintf (fp, "%s", varName);
      j = (int) strlen(varName);
      if (j < 22) Ncharacters (fp, (22-j), ' ');
      else fprintf (fp, "%s", " ");
      if (numDims == 0)
        fprintf (fp, "CDF_%s/%ld\t0:[]\t%s/  ", 
                 DataTypeToken(dataType), numElems, BOO(recVary,"T","F"));
      else if (numDims == 1)
        fprintf (fp, "CDF_%s/%ld\t%ld:[%ld]\t%s/%s", 
                 DataTypeToken(dataType), numElems, numDims, dimSizes[0],
                 BOO(recVary,"T","F"), BOO(dimVarys[0],"T","F"));
      else {
        for (j = 0; j < numDims; j++) {
          if (j == 0) {
            snprintf (shortStr1, (size_t) sizeof(shortStr1), "%ld",
		      dimSizes[j]);
            snprintf (shortStr2, (size_t) sizeof(shortStr2), "%s",
		      BOO(dimVarys[j],"T","F"));
          } else {
            strcat(shortStr1, ",");
            snprintf (EofS(shortStr1),
		      (size_t) sizeof(shortStr1)-strlen(shortStr1), "%ld", 
		      dimSizes[j]);
            snprintf (EofS(shortStr2),
		      (size_t) sizeof(shortStr2)-strlen(shortStr2), "%s", 
		      BOO(dimVarys[j],"T","F"));
          }
        }
        fprintf (fp, "CDF_%s/%ld\t%ld:[%s]\t%s/%s", 
                 DataTypeToken(dataType), numElems, numDims, shortStr1,
                 BOO(recVary,"T","F"), shortStr2);
      }
      if (sp != NO_SPARSERECORDS)
        fprintf (fp, " %s", SparsenessToken(sp, 0L, tmpLong));
      fprintf (fp, "\n");
    }
  }

  /**********************************************************/
  /* Print out the Variable data.                           */ 
  /**********************************************************/
  if (!detect) {
    if (numVars == 0) {
      if ((numrVars+numzVars) > 1)
        fprintf (fp, "\n\nVariable (%ld variables)\n", (numrVars+numzVars));
      else
        fprintf (fp, "\n\nVariable (%ld variable)\n", (numrVars+numzVars));
      fprintf (fp, "=========================================\n\n");
    } else
      fprintf (fp, "\n\n");
  }
  for (varNum = 0; varNum < (int) (numrVars+numzVars); varNum++) {
    if (varNum < (int) numrVars) Z = FALSE;
    else Z = TRUE;
    ii = varNum;
    working = NULL;
    if (ii >= (int) numrVars) ii = ii - (int) numrVars;
    status = CDFlib (SELECT_, BOO(Z,zVAR_,rVAR_), (long) ii,
                     GET_, BOO(Z,zVAR_NAME_,rVAR_NAME_), varName,
                           BOO(Z,zVAR_COMPRESSION_,rVAR_COMPRESSION_),
                               &compression, compressParms, &cPct,
			   BOO(Z,zVAR_MAXREC_,rVAR_MAXREC_), &maxRec,
			   BOO(Z,zVAR_DATATYPE_,rVAR_DATATYPE_), &dataType,
                     NULL_);
    if (status < CDF_OK) return QuitCDF (CDFpath, "11.0", status, varName);
        
    if (numVars > 0) {
      ifound = FALSE;
      for (ij = 0; ij < numVars; ij++) {
         if (!strcmp(varName, vars[ij])) {
           ifound = TRUE;
           break;
         } 
      } 
      if (!ifound) continue;
    }

    if (!detect) {
      if (CDFepochDataType(dataType) && epochVV) {
        virtual = IsVirtualVar ((long)ii, Z);
      } else
        virtual = FALSE;
      if (virtual) {
        maxRec = GetVirtualVarMaxRecord ((long)ii, Z);
        fprintf (fp, "%s (Virtual) (No: %d) (Recs: %ld)", varName, ii,
                                                          (maxRec+1));
      } else
        fprintf (fp, "%s (No: %d) (Recs: %ld)", varName, ii, (maxRec+1));
      if (METADATADUMP && compression != NO_COMPRESSION) {
        fprintf (fp, " (Compression: %s)\n",
                 CompressionToken(compression, compressParms));
        Ncharacters (fp, (int) strlen(varName), '-');
       } else {
        fprintf (fp, "\n");
        Ncharacters (fp, (int) strlen(varName), '-');
      }
      fprintf (fp, "\n");
    }
    if (useFormat) {
      status = GetFORMATEntry (id, Z, (long) ii, &prtFormat);
      if (status == CDF_OK) RemoveLeadingBlanks (prtFormat);
      else prtFormat = NULL;
    } else
      prtFormat = NULL;

    status = CDFlib (SELECT_, BOO(Z,zVAR_,rVAR_), (long) ii,
                     GET_, BOO(Z,zVAR_DATATYPE_,rVAR_DATATYPE_), &dataType,
                           BOO(Z,zVAR_NUMELEMS_,rVAR_NUMELEMS_), &numElems,
                           BOO(Z,zVAR_NUMDIMS_,rVARs_NUMDIMS_), &numDims,
                           BOO(Z,zVAR_DIMSIZES_,rVARs_DIMSIZES_), dimSizes,
                           BOO(Z,zVAR_DIMVARYS_,rVAR_DIMVARYS_), dimVarys,
                           BOO(Z,zVAR_RECVARY_,rVAR_RECVARY_), &recVary,
                           BOO(Z,zVAR_NUMRECS_,rVAR_NUMRECS_), &numRecs,
                           BOO(Z,zVAR_MAXallocREC_,rVAR_MAXallocREC_), 
                               &maxAllocRec,
                           BOO(Z,zVAR_NUMallocRECS_,rVAR_NUMallocRECS_), 
                               &numAllocRecs,
                           BOO(Z,zVAR_BLOCKINGFACTOR_,rVAR_BLOCKINGFACTOR_), 
                               &bf,
                           BOO(Z,zVAR_SPARSERECORDS_,rVAR_SPARSERECORDS_), 
                               &sp,
                     NULL_);
    if (status < CDF_OK) return QuitCDF (CDFpath, "12.0", status, varName);

    if ((dump != METADATADUMP && dump != VARIABLEDUMP) || detect) {
      if (STRINGdataType(dataType)) imore = 1;
      else imore = 0;
      nBytes = (size_t) (CDFelemSize(dataType) * (numElems + imore));
      value = cdf_AllocateMemory ((size_t) nBytes, FatalError);
      status = CDFlib (GET_, BOO(Z,zVAR_PADVALUE_,rVAR_PADVALUE_), value,
                       NULL_);
      if (status != NO_PADVALUE_SPECIFIED) {
        pad = TRUE;
        if (STRINGdataType(dataType)) {
          ((char *)value)[numElems] = '\0';
          strcpyX (tText, value, numElems);
        } else {
          int style, toUse;
          if (TT2000dataType(dataType)) {
            if (!ziso8601) style = TT2000_3_STYLE;
            else style = TT2000_4_STYLE;
          } else {
            if (!ziso8601) style = DEFAULTstyleDUMP;
            else style = EPOCH3_STYLE;
          }
          if (prtFormat != NULL) {
            if (IsHexFormat(prtFormat))
              toUse = 0;
            else
              toUse = 1;
          } else
            toUse = 0;
          EncodeValueFormat (dataType, value, tText, (toUse==1?prtFormat:NULL),
                             -1, 0, style, (size_t) sizeof(tText));
        }
      } else
        pad = FALSE;
      cdf_FreeMemory (value, FatalError);
      if (!detect) {
        fprintf (fp, "Data Type:           CDF_%s", DataTypeToken(dataType));
        if (STRINGdataType(dataType)) fprintf (fp, "/%ld\n", numElems);
        else fprintf (fp, "\n");
        fprintf (fp, "Dimensionality:      ");
        if (numDims == 0) fprintf (fp, "0:[]\t(%s/)  ", BOO(recVary,"T","F"));
        else if (numDims == 1)
          fprintf (fp, "%ld:[%ld]\t(%s/%s)", numDims, dimSizes[0], 
                   BOO(recVary,"T","F"), BOO(dimVarys[0],"T","F"));
        else {
          for (j = 0; j < numDims; j++) {
            if (j == 0) {
              snprintf (shortStr1, (size_t) sizeof(shortStr1), "%ld",
	  	        dimSizes[j]);
              snprintf (shortStr2, (size_t) sizeof(shortStr2), "%s",
		        BOO(dimVarys[j],"T","F"));
            } else {
              strcat(shortStr1, ",");
              snprintf (EofS(shortStr1),
	  	        (size_t) sizeof(shortStr1)-strlen(shortStr1), "%ld", 
		        dimSizes[j]);
              snprintf (EofS(shortStr2),
		        (size_t) sizeof(shortStr2)-strlen(shortStr2), "%s", 
		        BOO(dimVarys[j],"T","F"));
            }
          }
          fprintf (fp, "%ld:[%s]\t(%s/%s)", numDims, shortStr1,
                   BOO(recVary,"T","F"), shortStr2);
        }
        fprintf (fp, "\n");
      
        if (!ROWmajor(majority) && col2row && numDims > 1) toCol2Row = TRUE;
        else toCol2Row = FALSE;
        if (useFormat && prtFormat != NULL)
          fprintf (fp, "Format:              %s\n", prtFormat);
        if (dump != DATADUMP)
          if (compression != NO_COMPRESSION) {
            fprintf (fp, "Compression:         %s\n", 
                     CompressionToken(compression, compressParms));
          }
        if (pad) {
          if (STRINGdataType(dataType)) {
            char delimiter;
            if (strchr(tText, '"') == NULL) delimiter = '"';
            else if (strchr(tText, '\'') == NULL) delimiter = '\'';
            else delimiter = '%';
            fprintf (fp, "Pad value:           %c", delimiter);
            fprintf (fp, "%s", tText);
            fprintf (fp, "%c\n", delimiter);
          } else 
            if (!detect) fprintf (fp, "Pad value:           %s\n", tText);
        }
        fprintf (fp, "Written Records:     %ld/%ld(max)\n", numRecs, (maxRec+1));
        if (dump != DATADUMP) {
          fprintf (fp, "Allocated Records:   %ld/%ld(max)\n", numAllocRecs, 
                                                          (maxAllocRec+1));
          fprintf (fp, "Blocking Factor:     %ld (records)\n", bf);
        }
        if (sp != NO_SPARSERECORDS)
          fprintf (fp, "Sparseness:          %s\n", 
                   SparsenessToken(sp, 0L, tmpLong));
      }
    }

    if (dump != DATADUMP) {
      if (numvAttrs > 0) {
        if (dump != VARIABLEDUMP)
          if (!detect) fprintf (fp, "Attribute Entries:\n");
        for (ij = 0; ij < (int) numAttrs; ij++) {
          status = CDFlib (SELECT_, ATTR_, (long) ij,
                           GET_, ATTR_SCOPE_, &scope,
                           NULL_);
          if (status < CDF_OK) return QuitCDF (CDFpath, "14.0", status, NULL);

          if (scope == GLOBAL_SCOPE) continue;

          status = CDFlib (SELECT_, BOO(Z,zENTRY_,rENTRY_), (long) ii,
                           GET_, BOO(Z,zENTRY_DATATYPE_,rENTRY_DATATYPE_), 
                                 &dataTypeX,
                                 BOO(Z,zENTRY_NUMELEMS_,rENTRY_NUMELEMS_), 
                                 &numElemsX,
                           NULL_);
          if (status < CDF_OK) continue;
	  if (detect) continue;
          if (STRINGdataType(dataTypeX)) imore = 1;
          else imore = 0;
          nBytes = (size_t) (CDFelemSize(dataTypeX) * (numElemsX + imore));
          value = cdf_AllocateMemory ((size_t) nBytes, FatalError);
          status = CDFlib (GET_, ATTR_NAME_, attrName,
                                 BOO(Z,zENTRY_DATA_,rENTRY_DATA_), value,
                           NULL_);
          if (status < CDF_OK)
            return QuitCDF (CDFpath, "15.0", status, attrName);
          if (STRINGdataType(dataTypeX)) *((char *)value+numElemsX) = '\0';
	  if (STRINGdataType(dataTypeX)) {
	    int cnt;
	    checkPtr = UTF8Check ((unsigned char *)value, (int)numElemsX, &cnt);
	    if (checkPtr != NULL) 
	      printf("** Attr:%s entry:%d failed UTF-8 check...\n", attrName,
                     ii);
	  }
          fprintf (fp, "     %s", attrName);
          ir = (int) strlen(attrName);
          if (ir < 16) Ncharacters (fp, (16-ir), ' ');
          else fprintf (fp, "%s", " ");
          fprintf (fp, "(CDF_%s/%ld): ", DataTypeToken(dataTypeX),numElemsX);
          if (STRINGdataType(dataTypeX)) {
            long numStrings;
            status = CDFlib (GET_, BOO(Z,zENTRY_NUMSTRINGS_,rENTRY_NUMSTRINGS_),
                                   &numStrings,
                             NULL_);
            if (status < CDF_OK)
              return QuitCDF (CDFpath, "15.1", status, attrName);
            if (numStrings < 2) {
              ((char *)value)[numElemsX] = '\0';
              if (checkPtr == NULL)
                fprintf (fp, "\"%s\"\n", (char *)value);
              else {
                char *badStr = malloc((int)numElemsX + 1);
                memset (badStr, '.', (size_t)numElemsX);
                badStr[(int)numElemsX] = '\0';
                fprintf (fp, "\"%s\"   (<= non-UTF8)\n", badStr);
                free (badStr);
                badStr = NULL;
              }
            } else {
              char **strings; int ij, cnt;
              status = CDFlib (GET_, BOO(Z,zENTRY_STRINGSDATA_,
                                           rENTRY_STRINGSDATA_),
                                     &numStrings, &strings,
                               NULL_);
              if (status < CDF_OK)
                return QuitCDF (CDFpath, "15.3", status, attrName);
              fprintf (fp, "(strings:%ld)\n", numStrings);
              for (ij =0; ij < numStrings; ++ij) {
                checkPtr = UTF8Check ((unsigned char *)strings[ij],
			              (int)strlen(strings[ij]), &cnt);
                if (checkPtr == NULL)
                  fprintf (fp, "\"%s\"\n", strings[ij]);
                else {
                  size_t len = strlen(strings[ij]);
                  char *badStr;
                  badStr = malloc(len + 1);
                  memset (badStr, '.', len);
                  badStr[(int)len] = '\0';
                  fprintf (fp, "\"%s\"   (<= non-UTF8)\n", badStr);
                  free (badStr);
                  badStr = NULL;
                }
              }
              CDF_Free_String (numStrings, strings);
            }
          } else {
            int style;
            if (TT2000dataType(dataTypeX)) {
              if (!ziso8601) style = TT2000_3_STYLE;
              else style = TT2000_4_STYLE;
            } else {
              if (!ziso8601) style = DEFAULTstyleDUMP;
              else style = EPOCH3_STYLE;
            }
            if (numElemsX > 1) {
              int ip;
              int ix = CDFelemSize(dataTypeX);
              for (ip = 0; ip < numElemsX; ++ip) {
                EncodeValueFormat (dataTypeX, ((char *) value)+ix*ip, tText,
                                   prtFormat, -1, 0, style,
                                   (size_t) sizeof(tText));
                fprintf (fp, "%s", tText);
                if (ip != (numElemsX -1)) fprintf (fp, ", ");
              }
              fprintf (fp, "\n");
            } else {
              if (dataTypeX == CDF_EPOCH || dataTypeX == CDF_EPOCH16 ||
                  dataTypeX == CDF_TIME_TT2000)
                EncodeValue (dataTypeX, value, tText, style,
                             (size_t) sizeof(tText));
              else
                EncodeValueFormat (dataTypeX, value, tText, prtFormat, -1, 0,
                                   style, (size_t) sizeof(tText));
              fprintf (fp, "%s\n", tText);
            }
          }
          cdf_FreeMemory (value, FatalError);
        }
      }
    } 
    if (maxRec == -1) {
      if (prtFormat != NULL) cdf_FreeMemory (prtFormat, FatalError);
      if (!detect) fprintf (fp, "\n");
      continue;
    }
    if ((dump != METADATADUMP && dump != VARIABLEDUMP && dump != NODATADUMP) ||
        detect) {
      if (!detect) fprintf (fp, "Variable Data:\n");
      for (i = 0, nValuesPerRec = 1, nLogValuesPerRec = 1; i < numDims; i++) {
        if (dimVarys[i]) nValuesPerRec *= dimSizes[i];
        nLogValuesPerRec *= dimSizes[i];
      }
      iskip = (int) nLogValuesPerRec / nValuesPerRec;

      for (i = 0; i < numDims; i++) {
        nValuesPerDim[i] = 1;
        if (ROWmajor(majority)) {
          for (j = i + 1; j < numDims; j++) 
              nValuesPerDim[i] *= dimSizes[j];
        } else {
          for (j = i - 1; j >= 0; j--) 
            nValuesPerDim[i] *= dimSizes[j];
        }
      }
      nValueBytes[0] = (size_t) numElems * CDFelemSize(dataType);
      if (CDFepochDataType(dataType) && epochVV && virtual && !detect) {
        int style;
        if (TT2000dataType(dataType)) {
          if (!ziso8601) style = TT2000_3_STYLE;
          else style = TT2000_4_STYLE;
        } else {
          if (!ziso8601) style = DEFAULTstyleDUMP;
          else style = EPOCH3_STYLE;
        }
        buffer = (Byte1 *) HandleVirtual((long)ii, Z, &maxRec);
        if (buffer != NULL) {
          offset = 0;
          for (k = 0, value = buffer; k <= maxRec;
               k += 1, value = (char *) value + CDFelemSize(dataType)) {
            EncodeValueFormat (dataType, value, tText, prtFormat, -1, 0, style,
	                       (size_t) sizeof(tText));
            fprintf (fp, "  Record # %d: %s\n", (k+1), tText);
          }
        }
      } else {
	long toRecs, ist, ien;
        ist = startingRec - 1;
        ien = endingRec - 1;
        if (ist == -1) ist = 0;
        if (ien == -1) ien = maxRec;
        toRecs = ien - ist + 1;
        if (ist > maxRec && detect) continue;
        if (sp == NO_SPARSERECORDS) {
          handles[0] = &buffer;
          AllocateBuffers (toRecs, numDims, dimSizes, &groups, 0, 1, handles,
                           nValueBytes, ROWmajor(majority), 1, FatalError);
          InitHyperParmsFromAnywhere (&hyperSt, &groups, numDims, &nHypers, 
                                      &nValues, ist);
          if (STRINGdataType(dataType)) imore = 1;
          else imore = 0;
          nBytes = (size_t) (CDFelemSize(dataType) * (numElems + imore));
          working = cdf_AllocateMemory ((size_t) nBytes, FatalError);
          value2 = NULL;
          if (toCol2Row)
            value2 = malloc((size_t)nLogValuesPerRec*nValueBytes[0]);
          for (hyperN = 0; hyperN < nHypers; hyperN++) {
            for (j = 0; j < numDims; ++j)
              if (!dimVarys[j]) hyperSt.dimCounts[j] = 1;
            status = HYPER_READ (id, Z, hyperSt, buffer);
            if (status < CDF_OK)
              return QuitCDF(CDFpath, "16.0", status, varName);
            if (!detect) {
              offset = 0;
              for (k = 0, value = buffer; k < nValues;
                   k += iskip, offset += (size_t) nValueBytes[0]) {
                i = hyperSt.recNumber + k / nLogValuesPerRec; 
                j = k - k / nLogValuesPerRec * nLogValuesPerRec;
                if (j == 0) {
                  icount = 0; 
                  if (!detect) fprintf (fp, "  Record # %d: ",(i+1));
                  if (nValuesPerRec > 1) if (!detect) fprintf (fp, "%s", "[");
                  if (toCol2Row) {
                    COLtoROW((char *)value+offset, (char *) value2, numDims,
                              dimSizes, nValueBytes[0]);
                    memcpy ((char *)value+offset, (char *)value2,
                            (size_t)nLogValuesPerRec*nValueBytes[0]); 
                  }
                }
                memcpy (working, (char *) value+offset, nBytes);
                if (STRINGdataType(dataType)) {
                  ((char *)working)[numElems] = '\0';
                  fprintf (fp, "\"%s\"", (char *)working);
                } else {
                  int style;
                  if (TT2000dataType(dataType)) {
                    if (!ziso8601) style = TT2000_3_STYLE;
                    else style = TT2000_4_STYLE;
                  } else {
                    if (!ziso8601) style = DEFAULTstyleDUMP;
                    else style = EPOCH3_STYLE;
                  }
                  if (prtFormat != NULL) {
	      	    if (FLOAT4dataType(dataType)) {
                      if (!isnan((double)*(float *)working) &&
                          *(float *)working <= DEFAULT_FLOAT_PADVALUE) {
                        EncodeValueFormat (dataType, working, tText, NULL, 0, 0,
                                           style, (size_t) sizeof(tText));
                      } else
                        EncodeValueFormat (dataType, working, tText, prtFormat,
                                           -1, 0, style,
                                           (size_t) sizeof(tText));
		    } else if (DOUBLEdataType(dataType)) {
                      if (!isnan(*(double *)working) &&
                          *(double *)working <= DEFAULT_DOUBLE_PADVALUE) {
                        EncodeValueFormat (dataType, working, tText, NULL, 0, 0,
                                           style, (size_t) sizeof(tText));
                      } else
                        EncodeValueFormat (dataType, working, tText, prtFormat, 
                                           -1, 0, style,
                                           (size_t) sizeof(tText));
		    } else if (dataType == CDF_EPOCH) {
                      if (!isnan(*(double *)working) &&
                          *(double *)working <= DEFAULT_EPOCH_PADVALUE) {
                        EncodeValueFormat (dataType, working, tText, NULL, 0, 0,
                                           style, (size_t) sizeof(tText));
                      } else
                        EncodeValueFormat (dataType, working, tText, prtFormat, 
                                           0, 0, style, (size_t) sizeof(tText));
		    } else if (dataType == CDF_EPOCH16) {
                      if ((!isnan(*(double *)working)) &&
                          (!isnan(*((double *)working)+1)) &&
                          *(double *)working <= DEFAULT_EPOCH16_PADVALUE &&
                          *(((double *)working)+1) <= DEFAULT_EPOCH16_PADVALUE){
                        EncodeValueFormat (dataType, working, tText, NULL, 0, 0,
                                           style, (size_t) sizeof(tText));
                      } else
                        EncodeValueFormat (dataType, working, tText, prtFormat, 
                                           0, 0, style, (size_t) sizeof(tText));
		    } else
                      EncodeValueFormat (dataType, working, tText, prtFormat,
                                         -1, 0, style, (size_t) sizeof(tText));
                  } else {
                    if (useFormat && (precision > 0) && 
                        (FLOAT4dataType(dataType) ||
                         DOUBLEdataType(dataType))) {
                      char newFormat[20+1];
                      snprintf (newFormat, 20, "%%.%dg", precision);
                      EncodeValueFormat (dataType, working, tText, newFormat,
                                         -1, 0, style, (size_t) sizeof(tText));
                    } else
		      EncodeValue (dataType, working, tText, style,
		 	           (size_t) sizeof(tText));
                  }
                  fprintf (fp, "%s", tText);
                }
                if (nValuesPerRec == 1) {
                  if ((recVary && (status == VIRTUAL_RECORD_DATA &&
                                   i > maxRec)) ||
                      (!recVary && i > maxRec)) 
                    fprintf (fp, "\t\t<== Virtual");
                  fprintf (fp, "\n");
                  continue;
                }
                icount++;
                if (icount == nValuesPerRec) {
                  fprintf (fp, "]");
                  if ((recVary && (status == VIRTUAL_RECORD_DATA &&
                                   i > maxRec)) ||
                      (!recVary && i > maxRec)) 
                    fprintf (fp, "\t\t<== Virtual");
                  fprintf (fp, "\n");
                } else fprintf (fp, ",");
              }
	    }
            IncrHyperParms (&hyperSt, &groups, numDims, ROWmajor(majority),
                            &nValues);
          }
        } else {
          buffer = cdf_AllocateMemory ((size_t) nLogValuesPerRec * 
                                       nValueBytes[0], FatalError);
          if (STRINGdataType(dataType)) imore = 1;
          else imore = 0;
          nBytes = (size_t) (CDFelemSize(dataType) * (numElems + imore));
          working = cdf_AllocateMemory ((size_t) nBytes, FatalError);
          for (k = 0; k < numDims; k++) {
            dimIndices[k] = 0L;
            dimIntervals[k] = 1L;
            if (dimVarys[k]) dimCounts[k] = dimSizes[k];
            else dimCounts[k] = 1;
          }
          status = CDFlib(SELECT_, BOO(Z,zVAR_RECCOUNT_,rVARs_RECCOUNT_), 1L,
                                   BOO(Z,zVAR_RECINTERVAL_,rVARs_RECINTERVAL_), 
                                   1L,
                                   BOO(Z,zVAR_DIMINDICES_,rVARs_DIMINDICES_), 
                                   dimIndices,
                                   BOO(Z,zVAR_DIMINTERVALS_,
					 rVARs_DIMINTERVALS_), 
                                   dimIntervals,
                                   BOO(Z,zVAR_DIMCOUNTS_,rVARs_DIMCOUNTS_), 
                                   dimCounts,
                          NULL_);
          if (status < CDF_OK) return QuitCDF(CDFpath, "17.0", status, varName);
          if (toCol2Row)
            value2 = malloc((size_t)nLogValuesPerRec*nValueBytes[0]);
          for (k = (int) ist; k <= (int) ien; k++) {
            status = CDFlib(SELECT_, BOO(Z,zVAR_RECNUMBER_,rVARs_RECNUMBER_), 
                                     (long) k,
                            GET_, BOO(Z,zVAR_HYPERDATA_,rVAR_HYPERDATA_), buffer,
                            NULL_);
            if (status < CDF_OK)
              return QuitCDF(CDFpath, "18.0", status, varName);
	    if (!detect) {
              offset = 0;
              for (ir = 0, value = buffer; ir < nLogValuesPerRec;
                   ir += iskip, offset += (size_t) nValueBytes[0]) {
                if (ir == 0) {
                  icount = 0;
                  fprintf (fp, "  Record # %d: ",(k+1));
                  if (nValuesPerRec > 1) if (!detect) fprintf (fp, "%s", "[");
                  if (toCol2Row) {
                    COLtoROW(buffer, value2, numDims, dimSizes,
                             nValueBytes[0]);
                    memcpy ((char *)buffer, (char *)value2,
                            (size_t)nLogValuesPerRec*nValueBytes[0]);
                  }
                } 
                memcpy (working, (char *)value+offset, nBytes);
                if (STRINGdataType(dataType)) {
                    ((char *)working)[numElems] = '\0';
                  fprintf (fp, "\"%s\"", (char *)working);
                } else {
                  int style;
                  if (TT2000dataType(dataType)) {
                    if (!ziso8601) style = TT2000_3_STYLE;
                    else style = TT2000_4_STYLE;
                  } else {
                    if (!ziso8601) style = DEFAULTstyleDUMP;
                    else style = EPOCH3_STYLE;
                  }
                  if (prtFormat != NULL) {
                    if (FLOAT4dataType(dataType)) {
                      if (!isnan((double)*(float *)working) &&
                          *(float *)working <= DEFAULT_FLOAT_PADVALUE) {
                        EncodeValueFormat (dataType, working, tText, NULL, 0, 0,
                                           style, (size_t) sizeof(tText));
                      } else
                        EncodeValueFormat (dataType, working, tText, prtFormat,
                                           -1, 0, style,
                                           (size_t) sizeof(tText));
                    } else if (DOUBLEdataType(dataType)) {
                      if (!isnan(*(double *)working) &&
                          *(double *)working <= DEFAULT_DOUBLE_PADVALUE) {
                        EncodeValueFormat (dataType, working, tText, NULL, 0, 0,
                                           style, (size_t) sizeof(tText));
                      } else
                        EncodeValueFormat (dataType, working, tText, prtFormat,
                                           -1, 0, style,
                                           (size_t) sizeof(tText));
                    } else if (dataType == CDF_EPOCH) {
                      if (!isnan(*(double *)working) &&
                          *(double *)working <= DEFAULT_EPOCH_PADVALUE) {
                        EncodeValueFormat (dataType, working, tText, NULL, 0, 0,
                                           style, (size_t) sizeof(tText));
                      } else
                        EncodeValueFormat (dataType, working, tText, prtFormat,
                                           0, 0, style, (size_t) sizeof(tText));
                    } else if (dataType == CDF_EPOCH16) {
                      if (!isnan(*(double *)working) &&
                          !isnan(*(((double *)working)+1)) &&
                          *(double *)working <= DEFAULT_EPOCH16_PADVALUE &&
                          *(((double *)working)+1) <= DEFAULT_EPOCH16_PADVALUE){
                        EncodeValueFormat (dataType, working, tText, NULL, 0, 0,
                                           style, (size_t) sizeof(tText));
                      } else
                        EncodeValueFormat (dataType, working, tText, prtFormat,
                                           0, 0, style, (size_t) sizeof(tText));
                    } else
                      EncodeValueFormat (dataType, working, tText, prtFormat,
                                         -1, 0, style, (size_t) sizeof(tText));
                  } else {
                    if (useFormat && (precision > 0) && 
                        (FLOAT4dataType(dataType) ||
                         DOUBLEdataType(dataType))) {
                      char newFormat[20+1];
                      snprintf (newFormat, 20, "%%.%dg", precision);
                      EncodeValueFormat (dataType, working, tText, newFormat,
                                         -1, 0, style, (size_t) sizeof(tText));
                    } else
		      EncodeValue (dataType, working, tText, style,
		  	           (size_t) sizeof(tText));
                  }
                  fprintf (fp, "%s", tText);
                }
                if (nValuesPerRec == 1) {
                  if (status == VIRTUAL_RECORD_DATA)
	            fprintf (fp, "\t\t<== Virtual");
                  fprintf (fp, "\n");
                  continue;
                }
                icount++;
                if (icount == nValuesPerRec) {
	          fprintf (fp, "]");
	          if (status == VIRTUAL_RECORD_DATA)
	            fprintf (fp, "\t\t<== Virtual");
                  fprintf (fp, "\n");
                } else fprintf (fp, ",");
              }
            }
  	  }
        } 
      }
      if (toCol2Row && value2 != NULL) {
        free (value2);
        value2 = NULL;
      }
      if (!detect) fprintf (fp, "\n");
      if (buffer != NULL) cdf_FreeMemory (buffer, FatalError);
      if (working != NULL) cdf_FreeMemory (working, FatalError);
    } else
      if (!detect) fprintf (fp, "\n");
    if (prtFormat != NULL) cdf_FreeMemory (prtFormat, FatalError);
  }

  status = CDFlib (CLOSE_, CDF_, 
                   NULL_);

  if (fp != stdout) fclose(fp);

  if (numVars > 0) {
    for (k = 0; k < numVars; ++k) 
       cdf_FreeMemory (vars[k], FatalError);
    cdf_FreeMemory (vars, FatalError);
  }
  Exit;

}

/******************************************************************************
* QuitCDF.
******************************************************************************/

Logical QuitCDF (file, where, status, msg)
char *file;
char *where;
CDFstatus status;
char *msg;
{
  char text[CDF_STATUSTEXT_LEN+1];
  fprintf (fp, "Program failed for file: %s at %s...\n", file, where);
  if (status < CDF_OK) {
    CDFlib (SELECT_, CDF_STATUS_, status,
            GET_, STATUS_TEXT_, text,
            NULL_);
    fprintf (fp, "ERROR> %s ", text);
    if (msg != NULL) fprintf (fp, "(%s)\n", msg);
    else fprintf(fp, "\n");
  }
  CDFlib (CLOSE_, CDF_,
          NULL_);
  fprintf (fp, "\n");
  if (fp != stdout) fclose(fp);
  ExitBAD;
}

/******************************************************************************
* IsVirtualVar.
******************************************************************************/

Logical IsVirtualVar (varNum, Z)
long varNum;
Logical Z;
{
  CDFstatus status;
  long numElems;

  status = CDFlib (SELECT_, ATTR_NAME_, "VIRTUAL",
			    BOO(Z,zENTRY_,rENTRY_), varNum,
		   GET_, BOO(Z,zENTRY_NUMELEMS_,
			       rENTRY_NUMELEMS_), &numElems,
		   NULL_);
  if (status == NO_SUCH_ENTRY) return FALSE;
  return TRUE;
}

/******************************************************************************
* HandleVariable.
******************************************************************************/

void *HandleVariable (varNum, Z, maxRec)
long varNum;
Logical Z;
long *maxRec;
{
  CDFstatus status;
  long numElems;

  status = CDFlib (SELECT_, ATTR_NAME_, "VIRTUAL",
			    BOO(Z,zENTRY_,rENTRY_), varNum,
		   GET_, BOO(Z,zENTRY_NUMELEMS_,
			       rENTRY_NUMELEMS_), &numElems,
		   NULL_);
  if (status == NO_SUCH_ENTRY) return HandleReal (varNum, Z, maxRec);
  else return HandleVirtual (varNum, Z, maxRec);
}

/******************************************************************************
* HandleReal.
******************************************************************************/

void *HandleReal (varNum, Z, maxRec)
long varNum;
Logical Z;
long *maxRec;
{
  CDFstatus status;
  long numElems, dataType, varMaxRec;
  void *temp;
  long dimIndices[CDF_MAX_DIMS], dimSizes[CDF_MAX_DIMS],
       dimIntervals[CDF_MAX_DIMS];
  long recVary, dimVarys[CDF_MAX_DIMS];
  long numDims;
  int  i;

  status = CDFlib (SELECT_, BOO(Z,zVAR_,rVAR_), varNum,
		   GET_, BOO(Z,zVAR_NUMDIMS_,
			       rVARs_NUMDIMS_), &numDims,
		         BOO(Z,zVAR_DIMSIZES_,
			       rVARs_DIMSIZES_), dimSizes,
		         BOO(Z,zVAR_RECVARY_,
			       rVAR_RECVARY_), &recVary,
		         BOO(Z,zVAR_DIMVARYS_,
			       rVAR_DIMVARYS_), dimVarys,
		         BOO(Z,zVAR_MAXREC_,
			       rVAR_MAXREC_), &varMaxRec,
		         BOO(Z,zVAR_NUMELEMS_,
			       rVAR_NUMELEMS_), &numElems,
		         BOO(Z,zVAR_DATATYPE_,
			       rVAR_DATATYPE_), &dataType,
		   NULL_);
  if (status != CDF_OK) return NULL;
  temp = cdf_AllocateMemory ((size_t) (CDFelemSize(dataType) * numElems *
				       (varMaxRec+1)), FatalError);
  for (i = 0; i < (int)numDims; ++i) {
    dimIndices[i] = 0L;
    dimIntervals[i] = 1L;
  }
  status = CDFlib (SELECT_, BOO(Z,zVAR_RECNUMBER_,
				  rVARs_RECNUMBER_), 0L,
                            BOO(Z,zVAR_RECCOUNT_,
				  rVARs_RECCOUNT_), (varMaxRec+1),
                            BOO(Z,zVAR_RECINTERVAL_,
				   rVARs_RECINTERVAL_), 1L,
                            BOO(Z,zVAR_DIMINDICES_,
				  rVARs_DIMINDICES_), dimIndices,
                            BOO(Z,zVAR_DIMCOUNTS_,
				  rVARs_DIMCOUNTS_), dimSizes,
                            BOO(Z,zVAR_DIMINTERVALS_,
				  rVARs_DIMINTERVALS_), dimIntervals,
		   GET_, BOO(Z,zVAR_HYPERDATA_,rVAR_HYPERDATA_), temp,
		   NULL_);
  if (status != CDF_OK) {
    cdf_FreeMemory (temp, FatalError);
    return NULL;
  }
  *maxRec = varMaxRec;
  return temp;
}

/******************************************************************************
* GetVirtualVarMaxRecord.
******************************************************************************/

long GetVirtualVarMaxRecord (varNum, Z)
long varNum;
Logical Z;
{
  CDFstatus status;
  long numElems1, numElems2, varN1, varN2, varMaxRec1, varMaxRec2;
  char varName1[CDF_VAR_NAME_LEN256+1], varName2[CDF_VAR_NAME_LEN256+1];

  /****************************************************************************
  * Find out the real variables that in combined have the epoch time values. 
  ****************************************************************************/
  status = CDFlib (SELECT_, ATTR_NAME_, "COMPONENT_0",
			    BOO(Z,zENTRY_,rENTRY_), varNum,
		   GET_, BOO(Z,zENTRY_NUMELEMS_,
			       rENTRY_NUMELEMS_), &numElems1,
		         BOO(Z,zENTRY_DATA_,
			       rENTRY_DATA_), varName1,
		   NULL_);
  if (status == NO_SUCH_ENTRY) return -1L;
  varName1[(int)numElems1] = '\0';
  status = CDFlib (GET_, BOO(Z,zVAR_NUMBER_,
			       rVAR_NUMBER_), varName1, &varN1,
		   NULL_);
  if (status != CDF_OK) return -1L;
  status = CDFlib (SELECT_, BOO(Z,zVAR_,rVAR_), varN1,
		   GET_, BOO(Z,zVAR_MAXREC_,
			       rVAR_MAXREC_), &varMaxRec1,
		   NULL_);
  if (status != CDF_OK) return -1L;
  status = CDFlib (SELECT_, ATTR_NAME_, "COMPONENT_1",
			    BOO(Z,zENTRY_,rENTRY_), varNum,
		   GET_, BOO(Z,zENTRY_NUMELEMS_,
			       rENTRY_NUMELEMS_), &numElems2,
		         BOO(Z,zENTRY_DATA_,
			       rENTRY_DATA_), varName2,
		   NULL_);
  if (status != CDF_OK) return -1L;
  varName2[(int)numElems2] = '\0';
  status = CDFlib (GET_, BOO(Z,zVAR_NUMBER_,
			       rVAR_NUMBER_), varName2, &varN2,
		   NULL_);
  if (status != CDF_OK) return -1L;
  status = CDFlib (SELECT_, BOO(Z,zVAR_,rVAR_), varN2,
		   GET_, BOO(Z,zVAR_MAXREC_,
			       rVAR_MAXREC_), &varMaxRec2,
		   NULL_);
  if (status != CDF_OK) return -1L;
  if (varMaxRec1 == -1L || varMaxRec2 == -1L) return -1L;
  if (varMaxRec1 <= varMaxRec2) return varMaxRec2;
  else return varMaxRec1;
}

/******************************************************************************
* HandleVirtual.
******************************************************************************/

void *HandleVirtual (varNum, Z, maxRec)
long varNum;
Logical Z;
long *maxRec;
{
  CDFstatus status;
  long numElems1, numElems2;
  long dataType, varN1, dataType1, varN2, dataType2, varMaxRec1, varMaxRec2;
  void *temp = NULL, *temp1 = NULL, *temp2 = NULL;
  long dimIndices1[CDF_MAX_DIMS], dimSizes1[CDF_MAX_DIMS],
       dimIntervals1[CDF_MAX_DIMS];
  long dimIndices2[CDF_MAX_DIMS], dimSizes2[CDF_MAX_DIMS],
       dimIntervals2[CDF_MAX_DIMS];
  char varName1[CDF_VAR_NAME_LEN256+1], varName2[CDF_VAR_NAME_LEN256+1];
  long recVary1, recVary2, dimVarys1[CDF_MAX_DIMS], dimVarys2[CDF_MAX_DIMS];
  long numDims1, numDims2;
  char units1[21], units2[21];
  double scale;
  int  i;

  status = CDFlib (SELECT_, BOO(Z,zVAR_,rVAR_), varNum,
		   GET_, BOO(Z,zVAR_DATATYPE_,
			       rVAR_DATATYPE_), &dataType,
		   NULL_);
  if (status != CDF_OK) return NULL;
  /****************************************************************************
  * Find out the real variables that in combined have the epoch time values. 
  ****************************************************************************/
  status = CDFlib (SELECT_, ATTR_NAME_, "COMPONENT_0",
			    BOO(Z,zENTRY_,rENTRY_), varNum,
		   GET_, BOO(Z,zENTRY_NUMELEMS_,
			       rENTRY_NUMELEMS_), &numElems1,
		         BOO(Z,zENTRY_DATA_,
			       rENTRY_DATA_), varName1,
		   NULL_);
  if (status == NO_SUCH_ENTRY) return NULL;
  varName1[(int)numElems1] = '\0';
  status = CDFlib (GET_, BOO(Z,zVAR_NUMBER_,
			       rVAR_NUMBER_), varName1, &varN1,
		   NULL_);
  if (status != CDF_OK) return NULL;
  status = CDFlib (SELECT_, ATTR_NAME_, "UNITS",
			    BOO(Z,zENTRY_,rENTRY_), varN1,
		   GET_, BOO(Z,zENTRY_NUMELEMS_,
			       rENTRY_NUMELEMS_), &numElems1,
		   NULL_);
  if (status != CDF_OK) return NULL;
  status = CDFlib (GET_, BOO(Z,zENTRY_DATA_,
			       rENTRY_DATA_), units1,
		   NULL_);
  if (status != CDF_OK) return NULL;
  units1[(int)numElems1] ='\0';
  status = CDFlib (SELECT_, BOO(Z,zVAR_,rVAR_), varN1,
		   GET_, BOO(Z,zVAR_MAXREC_,
			       rVAR_MAXREC_), &varMaxRec1,
		         BOO(Z,zVAR_DATATYPE_,
			       rVAR_DATATYPE_), &dataType1,
		   NULL_);
  if (status != CDF_OK) return NULL;
  status = CDFlib (SELECT_, ATTR_NAME_, "COMPONENT_1",
			    BOO(Z,zENTRY_,rENTRY_), varNum,
		   GET_, BOO(Z,zENTRY_NUMELEMS_,
			       rENTRY_NUMELEMS_), &numElems2,
		         BOO(Z,zENTRY_DATA_,
			       rENTRY_DATA_), varName2,
		   NULL_);
  if (status != CDF_OK) return NULL;
  varName2[(int)numElems2] = '\0';
  status = CDFlib (GET_, BOO(Z,zVAR_NUMBER_,
			       rVAR_NUMBER_), varName2, &varN2,
		   NULL_);
  if (status != CDF_OK) return NULL;
  status = CDFlib (SELECT_, ATTR_NAME_, "UNITS",
			    BOO(Z,zENTRY_,rENTRY_), varN2,
		   GET_, BOO(Z,zENTRY_NUMELEMS_,
			       rENTRY_NUMELEMS_), &numElems2,
		   NULL_);
  if (status != CDF_OK) return NULL;
  status = CDFlib (GET_, BOO(Z,zENTRY_DATA_,
			       rENTRY_DATA_), units2,
		   NULL_);
  if (status != CDF_OK) return NULL;
  units2[(int)numElems2] ='\0';
  status = CDFlib (SELECT_, BOO(Z,zVAR_,rVAR_), varN2,
		   GET_, BOO(Z,zVAR_MAXREC_,
			       rVAR_MAXREC_), &varMaxRec2,
		         BOO(Z,zVAR_DATATYPE_,
			       rVAR_DATATYPE_), &dataType2,
		   NULL_);
  if (status != CDF_OK) return NULL;
  if (varMaxRec1 < 0 && varMaxRec2 < 0) return NULL;
  status = CDFlib (SELECT_, BOO(Z,zVAR_,rVAR_), varN1,
		   GET_, BOO(Z,zVAR_NUMDIMS_,
			       rVARs_NUMDIMS_), &numDims1,
		         BOO(Z,zVAR_DIMSIZES_,
			       rVARs_DIMSIZES_), dimSizes1,
		         BOO(Z,zVAR_RECVARY_,
			       rVAR_RECVARY_), &recVary1,
		         BOO(Z,zVAR_DIMVARYS_,
			       rVAR_DIMVARYS_), dimVarys1,
		         BOO(Z,zVAR_NUMELEMS_,
			       rVAR_NUMELEMS_), &numElems1,
		   NULL_);
  if (status != CDF_OK) return NULL;
  temp1 = cdf_AllocateMemory ((size_t) (CDFelemSize(dataType1) * numElems1 *
					(varMaxRec1+1)), FatalError);
  for (i = 0; i < (int)numDims1; ++i) {
    dimIndices1[i] = 0L;
    dimIntervals1[i] = 1L;
  }
  status = CDFlib (SELECT_, BOO(Z,zVAR_RECNUMBER_,
				  rVARs_RECNUMBER_), 0L,
                            BOO(Z,zVAR_RECCOUNT_,
				  rVARs_RECCOUNT_), (varMaxRec1+1),
                            BOO(Z,zVAR_RECINTERVAL_,
				   rVARs_RECINTERVAL_), 1L,
                            BOO(Z,zVAR_DIMINDICES_,
				  rVARs_DIMINDICES_), dimIndices1,
                            BOO(Z,zVAR_DIMCOUNTS_,
				  rVARs_DIMCOUNTS_), dimSizes1,
                            BOO(Z,zVAR_DIMINTERVALS_,
				  rVARs_DIMINTERVALS_), dimIntervals1,
		   GET_, BOO(Z,zVAR_HYPERDATA_,rVAR_HYPERDATA_), temp1,
		   NULL_);
  if (status != CDF_OK) {
    cdf_FreeMemory (temp1, FatalError);
    return NULL;
  }
  status = CDFlib (SELECT_, BOO(Z,zVAR_,rVAR_), varN2,
		   GET_, BOO(Z,zVAR_NUMDIMS_,
			       rVARs_NUMDIMS_), &numDims2,
		         BOO(Z,zVAR_DIMSIZES_,
			       rVARs_DIMSIZES_), dimSizes2,
		         BOO(Z,zVAR_RECVARY_,
			       rVAR_RECVARY_), &recVary2,
		         BOO(Z,zVAR_DIMVARYS_,
			       rVAR_DIMVARYS_), dimVarys2,
		         BOO(Z,zVAR_NUMELEMS_,
			       rVAR_NUMELEMS_), &numElems2,
		   NULL_);
  if (status != CDF_OK) return NULL;
  temp2 = cdf_AllocateMemory ((size_t) (CDFelemSize(dataType2) * numElems2 *
				       (varMaxRec2+1)), FatalError);
  if (dataType != dataType1)
    temp = cdf_AllocateMemory ((size_t) (CDFelemSize(dataType) * numElems2 *
					 (varMaxRec2+1)), FatalError);
  for (i = 0; i < (int)numDims2; ++i) {
    dimIndices2[i] = 0L;
    dimIntervals2[i] = 1L;
  }
  status = CDFlib (SELECT_, BOO(Z,zVAR_RECNUMBER_,
				  rVARs_RECNUMBER_), 0L,
                            BOO(Z,zVAR_RECCOUNT_,
				  rVARs_RECCOUNT_), (varMaxRec2+1),
                            BOO(Z,zVAR_RECINTERVAL_,
				   rVARs_RECINTERVAL_), 1L,
                            BOO(Z,zVAR_DIMINDICES_,
				  rVARs_DIMINDICES_), dimIndices2,
                            BOO(Z,zVAR_DIMCOUNTS_,
				  rVARs_DIMCOUNTS_), dimSizes2,
                            BOO(Z,zVAR_DIMINTERVALS_,
				  rVARs_DIMINTERVALS_), dimIntervals2,
		   GET_, BOO(Z,zVAR_HYPERDATA_,rVAR_HYPERDATA_), temp2,
		   NULL_);
  if (status != CDF_OK) {
    if (temp1 != NULL) cdf_FreeMemory (temp1, FatalError);
    if (temp2 != NULL) cdf_FreeMemory (temp2, FatalError);
    if (temp != NULL) cdf_FreeMemory (temp, FatalError);
    return NULL;
  }
  scale = EpochScale (dataType1, units2);
  if (varMaxRec1 == 0) {
    for (i = 0; i <= varMaxRec2; ++i) {
      if (dataType1 == CDF_EPOCH) {
	*((double *)temp2 + i) = *(double *)temp1 +
                                 *((double *)temp2 + i)*scale;
        if (dataType == CDF_EPOCH16) {
          long x1; double d2[2];
          x1=(long) (*((double *)temp2 + i)*1.E-3);
          d2[0] = (double) x1;
          d2[1] = (*((double *)temp2+i)-x1*1.E3)*1.E9;
          memcpy ((double *)temp+2*i, d2, 16);
        } else if (dataType == CDF_TIME_TT2000) {
          long yy,mm,dd,hh,mn,ss,ms; long long ll;
          EPOCHbreakdown(*((double *)temp2 + i),&yy,&mm,&dd,&hh,&mn,&ss,&ms);
          ll = computeTT2000((double)yy,(double)mm,(double)dd,(double)hh,
                             (double)mn,(double)ss,(double)ms,(double)0,
                             (double)0);
          memcpy ((long long *)temp+i, &ll, 8);
	} else {}
      } else if (dataType1 == CDF_EPOCH16) {
	*((double *)temp2 + 2*i) = *(double *)temp1 +
                                   *((double *)temp2 + 2*i)*scale;
	*((double *)temp2 + 2*i+1) = *((double *)temp1+1) + *((double *)temp2 +
                                                              2*i+1)*scale;
        if (dataType == CDF_EPOCH) {
          double d2;
          d2=*((double *)temp2 + 2*i)*1.E3+*((double *)temp2 + 2*i+1)*1.E-9;
          memcpy ((double *)temp+i, &d2, 8);
        } else if (dataType == CDF_TIME_TT2000) {
          long yy,mm,dd,hh,mn,ss,ms,us,ns,ps; long long ll;
          EPOCH16breakdown((double *)temp2 + 2*i,&yy,&mm,&dd,&hh,&mn,&ss,&ms,
                                                 &us,&ns,&ps);
          ll = computeTT2000((double)yy,(double)mm,(double)dd,(double)hh,
                             (double)mn,(double)ss,(double)ms,(double)us,
                             (double)ns);
          memcpy ((long long *)temp+i, &ll, 8);
	} else {}
      } else { /* if (dataType1 == CDF_TIME_TT2000) */
	*((long long *)temp2 + i) = *(long long *)temp1 +
                                    *((long long *)temp2 + i)*scale;
        if (dataType == CDF_EPOCH) {
          double yy,mm,dd,hh,mn,ss,ms,us,ns; double d2;
          TT2000breakdown(*((long long *)temp2 + i),&yy,&mm,&dd,&hh,&mn,&ss,
                                                    &ms,&us,&ns);
          d2=computeEPOCH((long)yy,(long)mm,(long)dd,(long)hh,(long)mn,
                          (long)ss,(long)ms);
          memcpy ((double *)temp+i, &d2, 8);
        } else if (dataType == CDF_EPOCH16) {
          double yy,mm,dd,hh,mn,ss,ms,us,ns; double d2[2];
          TT2000breakdown(*((long long *)temp2 + i),&yy,&mm,&dd,&hh,&mn,&ss,
                                                    &ms,&us,&ns);
          computeEPOCH16((long)yy,(long)mm,(long)dd,(long)hh,(long)mn,(long)ss,
                         (long)ms,(long)us,(long)ns,(long)0,d2);
          memcpy ((double *)temp+2*i, d2, 16);
	} else {}
      }
    }
  }
  /****************************************************************************
  * Return - note that memory allocated will be freed by caller.
  ****************************************************************************/
  *maxRec = varMaxRec2;
  if (dataType != dataType1) {
    if (temp1 != NULL) cdf_FreeMemory (temp1, FatalError);
    if (temp2 != NULL) cdf_FreeMemory (temp2, FatalError);
    return temp;
  } else {
    if (temp1 != NULL) cdf_FreeMemory (temp1, FatalError);
    return temp2;
  }
}

/******************************************************************************
* EpochScale. It returns the scale of units between the base and offset times.
******************************************************************************/

double EpochScale(long dataType1, char *units) {
  /* msec - milliseconds */
  if (dataType1==CDF_EPOCH && strcmpIgCase(units, "sec")) return 1.E3;
  if (dataType1==CDF_EPOCH && strcmpIgCase(units, "msec")) return 1.0;
  /* nsec - nanoseconds */
  if (dataType1==CDF_TIME_TT2000 && strcmpIgCase(units, "sec")) return 1.E9;
  if (dataType1==CDF_TIME_TT2000 && strcmpIgCase(units, "msec")) return 1.E6;
  if (dataType1==CDF_TIME_TT2000 && strcmpIgCase(units, "nsec")) return 1.0;
  /* psec - picoseconds */
  if (dataType1==CDF_EPOCH16 && strcmpIgCase(units, "sec")) return 1.E12;
  if (dataType1==CDF_EPOCH16 && strcmpIgCase(units, "msec")) return 1.E9;
  if (dataType1==CDF_EPOCH16 && strcmpIgCase(units, "psec")) return 1.0;
  return 1.0;
}

