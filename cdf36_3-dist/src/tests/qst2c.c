/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                  Quick Start Test Program (STANDARD interface/C).
*
*  Version 1.11, 13-Jun-96, Hughes STX.
*
*  Modification history:
*
*   V1.0  27-May-91, J Love     Original version (for CDF V2.1).
*   V1.1  25-Jun-91, J Love     Renamed CDF for portability.
*   V1.2   2-Aug-91, J Love     Use 'Exit'/'ExitBAD'.  Use 'CDFlib'.
*   V1.3  24-Oct-91, J Love     Modified for IBM-PC port.
*   V1.4  21-Apr-92, J Love     CDF V2.2.
*   V1.5   2-Sep-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V1.6  13-Nov-92, J Love     Borland C 3.0 (partial bracketing).
*   V1.7  14-Jan-94, J Love     CDF V2.4.
*   V1.8  20-Dec-94, J Love     CDF V2.5.
*   V1.8a  9-Jan-95, J Love     Indentation on UNIX machines.
*   V1.8b  8-Mar-95, J Love     Fixed wrong data type for rEntry.
*   V1.9   6-Apr-95, J Love     POSIX.
*   V1.9a 18-Apr-95, J Love     More POSIX.
*   V1.10  2-Jan-95, J Love     Shortened argument lists for Think C.
*   V1.11 13-Jun-96, J Love     CDF V2.6.
*   V2.0  21-Apr-05, M Liu      New version that includes new standard
*                               interfaces that handle both r/zVariables and
*                               gr/zRetry.
*   V2.1  27-May-05, M Liu      CDF V3.1.
*   V2.2  27-Oct-16, M Liu      CDF V3.6.3.
*
******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cdf.h"

#if defined(vms)
#include <ssdef>
#define EXIT_SUCCESS_   SS$_NORMAL
#define EXIT_FAILURE_   SS$_ABORT
#else
#define EXIT_SUCCESS_   0
#define EXIT_FAILURE_   1
#endif

/******************************************************************************
* Increased stack size for Borland C on IBM PC.
******************************************************************************/

#if defined(BORLANDC)
extern unsigned _stklen = 12000u;
#endif

/******************************************************************************
* Macros/prototypes.
******************************************************************************/

#define N_DIMS          2
#define DIM_0_SIZE      2
#define DIM_1_SIZE      3

#define zN_DIMSa        1
#define zDIM_0_SIZEa    5
#define zNUM_ELEMSa     8

void QuitCDF PROTOARGs((char *where, CDFstatus status));

CDFid id;
/******************************************************************************
* Main.
******************************************************************************/

int main () {
/* CDFid id; */
CDFstatus status;
int dim_n;
static long encoding = IBMPC_ENCODING;
static long actual_encoding = IBMPC_ENCODING;
static long majority = ROW_MAJOR;
static long numDims = N_DIMS;
static long dimSizes[N_DIMS] = { DIM_0_SIZE, DIM_1_SIZE };
static long zNumDimsA = zN_DIMSa;
static long zDimSizesA[zN_DIMSa] = { zDIM_0_SIZEa };
static long var1DataType = { CDF_INT2 };
static long var1DataTypeNew = { CDF_UINT2 };
static long var2DataType = { CDF_REAL4 };
static long var3DataType = { CDF_CHAR };
static long var3DataTypeNew = { CDF_UCHAR };
long var1DataType_out, var2DataType_out, var3DataType_out;
static long var1NumElements = { 1 };
static long var1NumElementsNew = { 1 };
static long var2NumElements = { 1 };
static long var3NumElements = { zNUM_ELEMSa };
static long var3NumElementsNew = { zNUM_ELEMSa };
long var1NumElements_out, var2NumElements_out, var3NumElements_out;
long var1Num_out, var2Num_out, var3Num_out, varNum_out1, varNum_out2,
     varNum_out3;
static short var1Values[DIM_0_SIZE][DIM_1_SIZE] = {{1,2,3},{4,5,6}};
static float var2Values[DIM_0_SIZE][DIM_1_SIZE] = {{1.,2.,3.},{4.,5.,6.}};
static char var3Values[zDIM_0_SIZEa][zNUM_ELEMSa] = {
  {'1','1','1','1','1','1','1','1'},
  {'2','2','2','2','2','2','2','2'},
  {'3','3','3','3','3','3','3','3'},
  {'4','4','4','4','4','4','4','4'},
  {'5','5','5','5','5','5','5','5'}
};
short var1Value_out;
float var2Value_out;
static char var3Value_out[zNUM_ELEMSa];
static long recNum = { 0 };
static long recStart = { 2 };
static long recCount = { 1 };
static long recInterval = { 1 };
long indices[N_DIMS];
static long counts[N_DIMS] = { DIM_0_SIZE, DIM_1_SIZE };
static long intervals[N_DIMS] = { 1, 1 };
long zIndicesA[zN_DIMSa];
static long zCounts[zN_DIMSa] = { zDIM_0_SIZEa };
static long zIntervals[N_DIMS] = { 1 };
short var1Buffer_out[DIM_0_SIZE][DIM_1_SIZE]; 
long numRecs1, numRecs2, numRecs3, recVary1, recVary2, recVary3,
     numDims1, numDims2, numDims3;
long dimSizes1[CDF_MAX_DIMS], dimSizes2[CDF_MAX_DIMS], dimSizes3[CDF_MAX_DIMS];
long dimVarys1[CDF_MAX_DIMS], dimVarys2[CDF_MAX_DIMS], dimVarys3[CDF_MAX_DIMS];
CDFdata var1xBuffer_out, var1yBuffer_out;
float var2Buffer_out[DIM_0_SIZE][DIM_1_SIZE]; 
CDFdata var2xBuffer_out, var2yBuffer_out;
char var3Buffer_out[zDIM_0_SIZEa][zNUM_ELEMSa]; 
CDFdata var3xBuffer_out, var3yBuffer_out;
long attrNum_out;
static long entryNum = { 2 };
long maxEntry_out;
static long attrScope = { GLOBAL_SCOPE };
static long attrScope2 = { VARIABLE_SCOPE };
static long attrScope3 = { VARIABLE_SCOPE };
long attrScope_out;
static long entryDataType = { CDF_INT2 };
static long entryDataTypeNew = { CDF_UINT2 };
long entryDataType_out;
static long entryNumElems = { 1 };
long entryNumElems_out;
static short entryValue = { 1 };
short entryValue_out; CDFdata entryValuex_out;
long encoding_out;
long majority_out;
long numDims_out;
long dimSizes_out[N_DIMS];
long zNumDimsA_out;
long zDimSizesA_out[zN_DIMSa];
long maxRec_out;
long numAttrs_out;
long version_out;
long release_out;
long increment_out;
char subincrement_out;
long numDimsX,dimSizesX[N_DIMS],encodingX,majorityX,maxrRecX,nrVarsX,nAttrsX,
     maxzRecX;
long numDimsY,dimSizesY[N_DIMS],encodingY,majorityY,maxrRecY,nrVarsY,nzVarsY,
     nAttrsY, maxzRecY;
int i, x0, x1, x;
static long var1RecVariance = { VARY };
static long var1RecVarianceNew = { NOVARY };
static long var2RecVariance = { VARY };
static long var3RecVariance = { VARY };
static long var3RecVarianceNew = { NOVARY };
long var1RecVariance_out, var2RecVariance_out, var3RecVariance_out;
static long var1DimVariances[N_DIMS] = { VARY, VARY };
static long var1DimVariancesNew[N_DIMS] = { NOVARY, NOVARY };
static long var2DimVariances[N_DIMS] = { VARY, VARY };
static long var3DimVariances[zN_DIMSa] = { VARY };
static long var3DimVariancesNew[zN_DIMSa] = { NOVARY };
long var1DimVariances_out[N_DIMS],
     var2DimVariances_out[N_DIMS],
     var3DimVariances_out[zN_DIMSa];
static char var1Name[] = "VAR1a";
static char var2Name[] = "VAR2a";
static char var3Name[] = "zVARa1";
static char new_var1Name[] = "VAR1b";
static char new_var2Name[] = "VAR2b";
static char new_var3Name[] = "zVARa2";
char var1Name_out[CDF_VAR_NAME_LEN256+1],
     var2Name_out[CDF_VAR_NAME_LEN256+1],
     var3Name_out[CDF_VAR_NAME_LEN256+1];
static char attrName[] = "ATTR1";
static char attrName2[] = "ATTR2";
static char attrName3[] = "ATTR3";
static char new_attrName[] = "ATTR1a";
char attrName_out[CDF_ATTR_NAME_LEN256];
char CopyrightText[CDF_COPYRIGHT_LEN+1];
char errorText[CDF_STATUSTEXT_LEN+1];
static char zEntryValue1 = { 4 };
char zEntryValue1Out; CDFdata zEntryValue1xOut;
static double zEntryValue2 = { 4.0 };
double zEntryValue2Out; CDFdata zEntryValue2xOut;
long numZvars, maxGentry, numGentries, 
     maxZentry, numZentries, numGattrs, numVattrs;
long cacheOut1, cacheOut2, cacheOut3;
static short pad1 = { -999 };
static float pad2 = { -8.0 };
static char pad3[zNUM_ELEMSa+1] = { "********" };
short pad1out; CDFdata pad1xout;
float pad2out; CDFdata pad2xout;
static char pad3out[zNUM_ELEMSa+1] = { "        " }; CDFdata pad3xout;
static long blockingfactor1 = 3;
static long blockingfactor2 = 4;
static long blockingfactor3 = 5;
long blockingfactorOut1, blockingfactorOut2, blockingfactorOut3;
long recStartOut, recCountOut, recIntervalOut, recNumOut;
long indicesOut[CDF_MAX_DIMS],
     countsOut[CDF_MAX_DIMS],
     intervalsOut[CDF_MAX_DIMS];
int dimN;
long entryNumOut1, entryNumOut2, entryNumOut3;
long formatOut;
long maxAllocOut1, maxAllocOut2, maxAllocOut3;
long maxRecOut1, maxRecOut2, maxRecOut3, maxRecOut;
static long allocRecs1 = { 10 };
static long allocRecs2 = { 15 };
static long allocRecs3 = { 8 };
static long varNs1[1] = { 0 }, varNs2[1] = { 2 };
static char varsRecBuffer1[DIM_0_SIZE][DIM_1_SIZE][6] = {
  {{0,0,0,0,0,0},{0,0,0,0,0,0},{0,0,0,0,0,0}},
  {{0,0,0,0,0,0},{0,0,0,0,0,0},{0,0,0,0,0,0}}
};
static char varsRecBuffer1Out[DIM_0_SIZE][DIM_1_SIZE][6];
static long nVars = { 1 };
static char varsRecBuffer2[zDIM_0_SIZEa][zNUM_ELEMSa] = {
  {'%','%','%','%','%','%','%','%'},
  {'%','%','%','%','%','%','%','%'},
  {'%','%','%','%','%','%','%','%'},
  {'%','%','%','%','%','%','%','%'},
  {'%','%','%','%','%','%','%','%'}
};
static char varsRecBuffer2Out[zDIM_0_SIZEa][zNUM_ELEMSa];
long dataType0, dataType1, dataType2, numElms0, numElms1, numElms2;

/******************************************************************************
* Display title.
******************************************************************************/

printf ("Testing Standard /C interface...\n");

/******************************************************************************
* Create CDF.
******************************************************************************/

status = CDFcreate ("TEST", numDims, dimSizes, encoding, majority, &id);

if (status < CDF_OK) {
  if (status == CDF_EXISTS) {
    status = CDFopen ("TEST", &id);
    if (status < CDF_OK) QuitCDF ("1.0", status);

    status = CDFdeleteCDF (id);
    if (status < CDF_OK) QuitCDF ("1.1", status);

    status = CDFcreate ("TEST", numDims, dimSizes, encoding, majority, &id);
    if (status < CDF_OK) QuitCDF ("1.2", status);
    status = CDFsetFormat (id, MULTI_FILE);
    if (status < CDF_OK) QuitCDF ("1.3", status);
  }
  else
    QuitCDF ("1.4", status);
}

status = CDFsetFormat (id, MULTI_FILE);
if (status < CDF_OK) QuitCDF ("1.5", status);

/******************************************************************************
* Create variables and set/confirm cache sizes, etc.
******************************************************************************/

status = CDFcreatezVar (id, var1Name, var1DataType, var1NumElements, numDims, 
			dimSizes, var1RecVariance, var1DimVariances, &var1Num_out);
if (status < CDF_OK) QuitCDF ("2.0", status);

status = CDFsetzVarPadValue (id, var1Num_out, &pad1);
if (status < CDF_OK) QuitCDF ("2.1", status);

status = CDFcreatezVar (id, var2Name, var2DataType, var2NumElements, numDims, 
			dimSizes, var2RecVariance, var2DimVariances, &var2Num_out);
if (status < CDF_OK) QuitCDF ("2.2", status);

status = CDFsetzVarPadValue (id, var2Num_out, &pad2);
if (status < CDF_OK) QuitCDF ("2.3", status);

status = CDFcreatezVar (id, var3Name, var3DataType, var3NumElements,
			zNumDimsA, zDimSizesA, var3RecVariance,
			var3DimVariances, &var3Num_out);
if (status < CDF_OK) QuitCDF ("2.4", status);

status = CDFsetzVarPadValue (id, var3Num_out, pad3);
if (status < CDF_OK) QuitCDF ("2.5", status);

status = CDFsetzVarsCacheSize (id, 5L);
if (status < CDF_OK) QuitCDF ("2.6", status);

status = CDFgetzVarCacheSize(id, 0L, &cacheOut1);
if (status < CDF_OK) QuitCDF ("2.8", status);

status = CDFgetzVarPadValue(id, 0L, &pad1out);
if (status < CDF_OK) QuitCDF ("2.9", status);

status = CDFreadzVarPadValue(id, 0L, &dataType0, &numElms0, &pad1xout);
if (status < CDF_OK) QuitCDF ("2.99", status);

status = CDFgetzVarCacheSize(id, 1L, &cacheOut2); 
if (status < CDF_OK) QuitCDF ("2.10", status);

status = CDFgetzVarPadValue(id, 1L, &pad2out); 
if (status < CDF_OK) QuitCDF ("2.11", status);

status = CDFreadzVarPadValue(id, 1L, &dataType1, &numElms1, &pad2xout); 
if (status < CDF_OK) QuitCDF ("2.111", status);

status = CDFgetzVarCacheSize(id, 2L, &cacheOut3); 
if (status < CDF_OK) QuitCDF ("2.12", status);

status = CDFgetzVarPadValue(id, 2L, &pad3out);
if (status < CDF_OK) QuitCDF ("2.13", status);

status = CDFreadzVarPadValue(id, 2L, &dataType2, &numElms2, &pad3xout);
if (status < CDF_OK) QuitCDF ("2.133", status);

if (cacheOut1 != 5) QuitCDF ("2.14", status);
if (cacheOut2 != 5) QuitCDF ("2.15", status);
if (cacheOut3 != 5) QuitCDF ("2.16", status);
if (pad1out != pad1) QuitCDF ("2.17", status);
if (memcmp((void *)pad1xout,&pad1,(size_t)CDFelemSize(dataType0)))
  QuitCDF ("2.177", status);
if (pad2out != pad2) QuitCDF ("2.18", status);
if (memcmp((void *)pad2xout,&pad2,(size_t)CDFelemSize(dataType1)))
  QuitCDF ("2.188", status);
if (strcmp(pad3out,pad3)) QuitCDF ("2.19", status);
if (strcmp((char *)pad3xout,pad3)) QuitCDF ("2.199", status);
CDFdataFree (pad1xout);
CDFdataFree (pad2xout);
CDFdataFree (pad3xout);

status = CDFsetzVarCacheSize(id, 0L, 4L);
if (status < CDF_OK) QuitCDF ("2.20", status);

status = CDFsetzVarCacheSize(id, 2L, 8L);
if (status < CDF_OK) QuitCDF ("2.21", status);

status = CDFgetzVarCacheSize(id, 0L, &cacheOut1);
if (status < CDF_OK) QuitCDF ("2.22", status);

status = CDFgetzVarCacheSize(id, 1L, &cacheOut2);
if (status < CDF_OK) QuitCDF ("2.23", status);

status = CDFgetzVarCacheSize(id, 2L, &cacheOut3);
if (status < CDF_OK) QuitCDF ("2.24", status);

if (cacheOut1 != 4) QuitCDF ("2.25", status);
if (cacheOut2 != 5) QuitCDF ("2.26", status);
if (cacheOut3 != 8) QuitCDF ("2.27", status);

/******************************************************************************
* Modify variables.
******************************************************************************/

status = CDFsetzVarDataSpec (id, 0L, var1DataTypeNew);
if (status < CDF_OK) QuitCDF ("3.0", status);

status = CDFsetzVarRecVariance (id, 0L, var1RecVarianceNew);
if (status < CDF_OK) QuitCDF ("3.1", status);

status = CDFsetzVarDimVariances (id, 0L, var1DimVariancesNew);
if (status < CDF_OK) QuitCDF ("3.2", status);

status = CDFsetzVarInitialRecs (id, 0L, 1L);
if (status < CDF_OK) QuitCDF ("3.3", status);

status = CDFsetzVarDataSpec (id, 2L, var3DataTypeNew);
if (status < CDF_OK) QuitCDF ("3.4", status);

status = CDFsetzVarRecVariance (id, 2L, var3RecVarianceNew);
if (status < CDF_OK) QuitCDF ("3.5", status);

status = CDFsetzVarDimVariances (id, 2L, var3DimVariancesNew);
if (status < CDF_OK) QuitCDF ("3.6", status);

status = CDFsetzVarInitialRecs (id, 2L, 1L);
if (status < CDF_OK) QuitCDF ("3.7", status);

/******************************************************************************
* Close CDF.
******************************************************************************/

status = CDFcloseCDF (id);
if (status < CDF_OK) QuitCDF ("4.0", status);

/******************************************************************************
* Reopen CDF.
******************************************************************************/

status = CDFopen ("TEST", &id);
if (status < CDF_OK) QuitCDF ("5.0", status);

status = CDFsetDecoding (id, HOST_DECODING);
if (status < CDF_OK) QuitCDF ("5.1", status);

/******************************************************************************
* Delete CDF.
******************************************************************************/

status = CDFdeleteCDF (id);
if (status < CDF_OK) QuitCDF ("6.0", status);

/******************************************************************************
* Create CDF again (previous delete will allow this).
******************************************************************************/

status = CDFcreateCDF("TEST", &id);
if (status < CDF_OK) QuitCDF ("7.0", status);

status = CDFsetEncoding (id, IBMPC_ENCODING);
if (status < CDF_OK) QuitCDF ("7.1", status);

status = CDFsetMajority (id, ROW_MAJOR);
if (status < CDF_OK) QuitCDF ("7.2", status);

status = CDFsetFormat (id, SINGLE_FILE);
if (status < CDF_OK) QuitCDF ("7.3", status);

status = CDFsetDecoding (id, HOST_DECODING);
if (status < CDF_OK) QuitCDF ("7.4", status);

/******************************************************************************
* Create variables.
******************************************************************************/

status = CDFcreatezVar (id, var1Name, var1DataType, var1NumElements, numDims, 
                        dimSizes, var1RecVariance, var1DimVariances, &var1Num_out);
if (status < CDF_OK) QuitCDF ("8.0", status);

status = CDFsetzVarAllocRecords (id, var1Num_out, allocRecs1);
if (status < CDF_OK) QuitCDF ("8.1", status);

status = CDFsetzVarBlockingFactor  (id, var1Num_out, blockingfactor1);
if (status < CDF_OK) QuitCDF ("8.2", status);

status = CDFcreatezVar (id, var2Name, var2DataType, var2NumElements, numDims, 
                        dimSizes, var2RecVariance, var2DimVariances, &var2Num_out);
if (status < CDF_OK) QuitCDF ("8.3", status);

status = CDFsetzVarAllocRecords (id, var2Num_out, allocRecs2);
if (status < CDF_OK) QuitCDF ("8.4", status);

status = CDFsetzVarBlockingFactor  (id, var2Num_out, blockingfactor2);
if (status < CDF_OK) QuitCDF ("8.5", status);

status = CDFcreatezVar (id, var3Name, var3DataType, var3NumElements,
                        zNumDimsA, zDimSizesA, var3RecVariance,
                        var3DimVariances, &var3Num_out);
if (status < CDF_OK) QuitCDF ("8.6", status);

status = CDFsetzVarAllocRecords (id, var3Num_out, allocRecs3);
if (status < CDF_OK) QuitCDF ("8.7", status);

status = CDFsetzVarBlockingFactor  (id, var3Num_out, blockingfactor3);
if (status < CDF_OK) QuitCDF ("8.8", status);

/******************************************************************************
* PUT to variables.
******************************************************************************/

for (x0 = 0; x0 < DIM_0_SIZE; x0++)
   for (x1 = 0; x1 < DIM_1_SIZE; x1++) {
      indices[0] = x0;
      indices[1] = x1;
      status = CDFputzVarData (id, var1Num_out, recNum, indices, 
                               &var1Values[x0][x1]);
      if (status < CDF_OK) QuitCDF ("9.0", status);

      status = CDFputzVarData (id, var2Num_out, recNum, indices, 
                               &var2Values[x0][x1]);
      if (status < CDF_OK) QuitCDF ("9.1", status);
   }

for (x0 = 0; x0 < zDIM_0_SIZEa; x0++) {
   zIndicesA[0] = x0;
   status = CDFputzVarData (id, var3Num_out, recNum, zIndicesA,
                            var3Values[x0]);
   if (status < CDF_OK) QuitCDF ("9.2", status);
}

/******************************************************************************
* GET from the variables.
******************************************************************************/

for (x0 = 0; x0 < DIM_0_SIZE; x0++)
   for (x1 = 0; x1 < DIM_1_SIZE; x1++) {
      indices[0] = x0;
      indices[1] = x1;
      status = CDFgetzVarData (id, var1Num_out, recNum, indices, 
                               &var1Value_out);
      if (status < CDF_OK) QuitCDF ("10.0", status);

      status = CDFgetzVarData (id, var2Num_out, recNum, indices, 
                               &var2Value_out);
      if (status < CDF_OK) QuitCDF ("10.1", status);

      if (var1Value_out != var1Values[x0][x1]) QuitCDF ("10.2", status);
      if (var2Value_out != var2Values[x0][x1]) QuitCDF ("10.3", status);
   }

for (x0 = 0; x0 < zDIM_0_SIZEa; x0++) {
   zIndicesA[0] = x0;
   status = CDFgetzVarData (id, var3Num_out, recNum, zIndicesA, 
                            var3Value_out);
   if (status < CDF_OK) QuitCDF ("10.4", status);

   for (i = 0; i < zNUM_ELEMSa; i++) {
      if (var3Value_out[i] != var3Values[x0][i]) QuitCDF ("10.5", status);
   }
}

/******************************************************************************
* HyperPUT to the variables.
******************************************************************************/

for (x0 = 0; x0 < DIM_0_SIZE; x0++)
   for (x1 = 0; x1 < DIM_1_SIZE; x1++) {
      var1Values[x0][x1] = -var1Values[x0][x1];
      var2Values[x0][x1] = -var2Values[x0][x1];
   }

indices[0] = 0;
indices[1] = 0;

status = CDFhyperPutzVarData (id, var1Num_out, recStart, recCount, recInterval,
                              indices, counts, intervals, var1Values); 
if (status < CDF_OK) QuitCDF ("11.0", status);

status = CDFhyperPutzVarData (id, var2Num_out, recStart, recCount, recInterval,
                              indices, counts, intervals, var2Values); 
if (status < CDF_OK) QuitCDF ("11.1", status);

for (x0 = 0; x0 < zDIM_0_SIZEa; x0++)
   for (i = 0; i < zNUM_ELEMSa; i++) {
      var3Values[x0][i]++;
   }

zIndicesA[0] = 0;

status = CDFhyperPutzVarData (id, var3Num_out, recStart, recCount, 
                              recInterval, zIndicesA, zCounts, zIntervals, 
                              var3Values); 
if (status < CDF_OK) QuitCDF ("11.2", status);

/******************************************************************************
* HyperGET from variables.
******************************************************************************/

status = CDFhyperGetzVarData (id, var1Num_out, recStart, recCount, recInterval,
                              indices, counts, intervals, var1Buffer_out);
if (status < CDF_OK) QuitCDF ("12.0", status);

status = CDFhyperGetzVarData (id, var2Num_out, recStart, recCount, recInterval,
                              indices, counts, intervals, var2Buffer_out);
if (status < CDF_OK) QuitCDF ("12.1", status);

for (x0 = 0; x0 < DIM_0_SIZE; x0++)
   for (x1 = 0; x1 < DIM_1_SIZE; x1++) {
      if (var1Buffer_out[x0][x1] != var1Values[x0][x1])
	QuitCDF ("12.2", status);
      if (var2Buffer_out[x0][x1] != var2Values[x0][x1])
	QuitCDF ("12.3", status);
   }

status = CDFreadzVarAllByVarID (id, var1Num_out, &numRecs1, &dataType1,
                                &numElms1, &numDims1, dimSizes1,
                                &recVary1, dimVarys1, &var1xBuffer_out);
if (status < CDF_OK) QuitCDF ("12.31", status);

if (memcmp((void *)var1Values,
           (char *)var1xBuffer_out+CDFelemSize(dataType1)*DIM_0_SIZE*
                                   DIM_1_SIZE*recStart,
           (size_t)CDFelemSize(dataType1)*DIM_0_SIZE*DIM_1_SIZE))
  QuitCDF ("12.32", status);

CDFdataFree (var1xBuffer_out);

status = CDFreadzVarRangeDataByVarID (id, var1Num_out, recStart, recStart,
                                      &var1yBuffer_out);
if (status < CDF_OK) QuitCDF ("12.33", status);

if (memcmp((void *)var1Values,(void *)var1yBuffer_out,
           (size_t)CDFelemSize(dataType1)*DIM_0_SIZE*DIM_1_SIZE))
  QuitCDF ("12.34", status);

CDFdataFree (var1yBuffer_out);

status = CDFreadzVarAllByVarID (id, var2Num_out, &numRecs2, &dataType2,
                                &numElms2, &numDims2, dimSizes2,
                                &recVary2, dimVarys2, &var2xBuffer_out);
if (status < CDF_OK) QuitCDF ("12.35", status);

if (memcmp((void *)var2Values,
           (char *)var2xBuffer_out+CDFelemSize(dataType2)*DIM_0_SIZE*
                                   DIM_1_SIZE*recStart,
           (size_t)CDFelemSize(dataType2)*DIM_0_SIZE*DIM_1_SIZE))
  QuitCDF ("12.36", status);

CDFdataFree (var2xBuffer_out);

status = CDFreadzVarRangeDataByVarID (id, var2Num_out, recStart, recStart,
                                      &var2yBuffer_out);
if (status < CDF_OK) QuitCDF ("12.37", status);

if (memcmp((void *)var2Values,(void *)var2yBuffer_out,
           (size_t)CDFelemSize(dataType2)*DIM_0_SIZE*DIM_1_SIZE))
  QuitCDF ("12.38", status);

CDFdataFree (var2yBuffer_out);

status = CDFhyperGetzVarData (id, var3Num_out, recStart, recCount,
                              recInterval, zIndicesA, zCounts, zIntervals,
                              var3Buffer_out);
if (status < CDF_OK) QuitCDF ("12.4", status);

for (x0 = 0; x0 < zDIM_0_SIZEa; x0++)
   for (i = 0; i < zNUM_ELEMSa; i++) {
      if (var3Buffer_out[x0][i] != var3Values[x0][i])
	QuitCDF ("12.5", status);
   }

status = CDFreadzVarAllByVarID (id, var3Num_out, &numRecs3, &dataType0,
                                &numElms0, &numDims3, dimSizes3,
                                &recVary3, dimVarys3, &var3xBuffer_out);
if (status < CDF_OK) QuitCDF ("12.44", status);

if (memcmp((void *)var3Values,
           (char *)var3xBuffer_out+CDFelemSize(dataType0)*zDIM_0_SIZEa*
                                   zNUM_ELEMSa*recStart,
           (size_t)CDFelemSize(dataType0)*zDIM_0_SIZEa*zNUM_ELEMSa))
  QuitCDF ("12.51", status);

CDFdataFree (var3xBuffer_out);

status = CDFreadzVarRangeDataByVarID (id, var3Num_out, recStart, recStart,
                                      &var3yBuffer_out);
if (status < CDF_OK) QuitCDF ("12.45", status);

if (memcmp((void *)var3Values,(void *)var3yBuffer_out,
           (size_t)CDFelemSize(dataType0)*zDIM_0_SIZEa*zNUM_ELEMSa))
  QuitCDF ("12.52", status);

CDFdataFree (var3yBuffer_out);

/******************************************************************************
* Set/confirm sequential access position for a zVariable (and read/write a
* value).
******************************************************************************/

status = CDFsetzVarSeqPos (id, var3Num_out, recStart, zIndicesA);
if (status < CDF_OK) QuitCDF ("14.0", status);

status = CDFgetzVarSeqData (id, var3Num_out, var3Value_out);
if (status < CDF_OK) QuitCDF ("14.1", status);

status = CDFputzVarSeqData (id, var3Num_out, var3Value_out);
if (status < CDF_OK) QuitCDF ("14.2", status);

status = CDFgetzVarSeqPos (id, var3Num_out, &recNumOut, indicesOut);
if (status < CDF_OK) QuitCDF ("14.3", status);

if (recNumOut != recStart) QuitCDF ("14.4", status);
if (indicesOut[0] != zIndicesA[0] + 2) QuitCDF ("14.5", status);

/******************************************************************************
* Create attributes.
******************************************************************************/

status = CDFcreateAttr (id, attrName, attrScope, &attrNum_out);
if (status < CDF_OK) QuitCDF ("16.0", status);

status = CDFcreateAttr (id, attrName2, attrScope2, &attrNum_out);
if (status < CDF_OK) QuitCDF ("16.1", status);

status = CDFcreateAttr (id, attrName3, attrScope3, &attrNum_out);
if (status < CDF_OK) QuitCDF ("16.2", status);

/******************************************************************************
* PUT to attributes.
******************************************************************************/

status = CDFputAttrgEntry (id, 0L, entryNum, entryDataType, entryNumElems,
                           &entryValue);
if (status < CDF_OK) QuitCDF ("17.0", status);

status = CDFputAttrzEntry (id, 1L, CDFgetVarNum(id, var1Name), CDF_BYTE, 1L, 
                           &zEntryValue1);
if (status < CDF_OK) QuitCDF ("17.1", status);

status = CDFputAttrzEntry (id, 2L, CDFgetVarNum(id, var2Name), CDF_REAL8, 1L,
                           &zEntryValue2);
if (status < CDF_OK) QuitCDF ("17.2", status);

/******************************************************************************
* GET from attributes.
******************************************************************************/

status = CDFgetAttrgEntry (id, 0L, entryNum, &entryValue_out);
if (status < CDF_OK) QuitCDF ("19.1", status);

status = CDFreadgAttrEntry (id, 0L, entryNum, &dataType0, &numElms0, 
                            &entryValuex_out);
if (status < CDF_OK) QuitCDF ("19.11", status);

status = CDFgetAttrzEntry (id, 1L, CDFgetVarNum(id, var1Name), &zEntryValue1Out);
if (status < CDF_OK) QuitCDF ("19.3", status);

status = CDFreadzAttrEntry (id, 1L, CDFgetVarNum(id, var1Name), &dataType1,
                            &numElms1, &zEntryValue1xOut);
if (status < CDF_OK) QuitCDF ("19.33", status);

status = CDFgetAttrzEntry (id, 2L, CDFgetVarNum(id, var2Name), &zEntryValue2Out);
if (status < CDF_OK) QuitCDF ("19.5", status);

status = CDFreadzAttrEntry (id, 2L, CDFgetVarNum(id, var2Name), &dataType2,
                            &numElms2, &zEntryValue2xOut);
if (status < CDF_OK) QuitCDF ("19.55", status);

if (entryValue_out != entryValue) QuitCDF ("19.6", status);
if (memcmp(&entryValue_out,(void *)entryValuex_out,(size_t)numElms0))
  QuitCDF ("19.66", status);
if (zEntryValue1 != zEntryValue1Out) QuitCDF ("19.7", status);
if (memcmp(&zEntryValue1,(void *)zEntryValue1xOut,(size_t)numElms1))
  QuitCDF ("19.77", status);
if (zEntryValue2 != zEntryValue2Out) QuitCDF ("19.8", status);
if (memcmp(&zEntryValue2,(void *)zEntryValue2xOut,(size_t)numElms2))
  QuitCDF ("19.88", status);

/******************************************************************************
* Confirm existence of variables/attributes/entries.
******************************************************************************/

status = CDFconfirmzVarExistence (id, var3Name);
if (status < CDF_OK) QuitCDF ("20.0", status);

status = CDFconfirmzVarExistence (id, var1Name);
if (status < CDF_OK) QuitCDF ("20.1", status);

status = CDFconfirmAttrExistence (id, attrName3);
if (status < CDF_OK) QuitCDF ("20.2", status);

status = CDFconfirmgEntryExistence (id, 0L, entryNum);
if (status < CDF_OK) QuitCDF ("20.3", status);

status = CDFconfirmzEntryExistence (id, 1L, CDFgetVarNum(id, var1Name));
if (status < CDF_OK) QuitCDF ("20.5", status);

/******************************************************************************
* Get CDF documentation.
******************************************************************************/

status = CDFgetLibraryVersion (&version_out, &release_out, &increment_out,
                               &subincrement_out);
if (status < CDF_OK) QuitCDF ("21.0", status);

status = CDFgetLibraryCopyright (CopyrightText);
if (status < CDF_OK) QuitCDF ("21.1", status);

/******************************************************************************
* Inquire CDF.
******************************************************************************/

status = CDFgetFormat (id, &formatOut);
if (status < CDF_OK) QuitCDF ("22.0", status);

status = CDFgetEncoding (id, &encoding_out);
if (status < CDF_OK) QuitCDF ("22.3", status);

status = CDFgetMajority (id, &majority_out);
if (status < CDF_OK) QuitCDF ("22.4", status);

status = CDFgetzVarsMaxWrittenRecNum (id, &maxRec_out);
if (status < CDF_OK) QuitCDF ("22.5", status);

status = CDFgetNumzVars (id, &numZvars);
if (status < CDF_OK) QuitCDF ("22.7", status);

status = CDFgetNumAttributes (id, &numAttrs_out);
if (status < CDF_OK) QuitCDF ("22.8", status);

if (formatOut != SINGLE_FILE) QuitCDF ("22.9", status);
if (encoding_out != actual_encoding) QuitCDF ("22.12", status);
if (majority_out != majority) QuitCDF ("22.13", status);
if (maxRec_out != 2) QuitCDF ("22.14", status);
if (numZvars != 3) QuitCDF ("22.16", status);
if (numAttrs_out != 3) QuitCDF ("22.17", status);

/******************************************************************************
* Inquire numbers.
******************************************************************************/

attrNum_out = CDFgetAttrNum (id, attrName3);
if (status < CDF_OK) QuitCDF ("23.0", status);

varNum_out1 = CDFgetVarNum (id, var2Name);
if (status < CDF_OK) QuitCDF ("23.1", status);

varNum_out2 = CDFgetVarNum (id, var3Name);
if (status < CDF_OK) QuitCDF ("23.2", status);

if (attrNum_out != 2) QuitCDF ("23.3", status);
if (varNum_out1 != 1) QuitCDF ("23.4", status);
if (varNum_out2 != 2) QuitCDF ("23.5", status);

/******************************************************************************
* Rename variables.
******************************************************************************/

status = CDFrenamezVar (id, CDFgetVarNum (id, var1Name), new_var1Name);
if (status < CDF_OK) QuitCDF ("24.0", status);

status = CDFrenamezVar (id, CDFgetVarNum (id, var2Name), new_var2Name);
if (status < CDF_OK) QuitCDF ("24.1", status);

status = CDFrenamezVar (id, CDFgetVarNum (id, var3Name), new_var3Name);
if (status < CDF_OK) QuitCDF ("24.2", status);

/******************************************************************************
* Read/write multiple variable data.
******************************************************************************/

status = CDFputzVarsRecordDatabyNumbers (id, nVars, varNs1, 2L, 
                                         varsRecBuffer1);
if (status < CDF_OK) QuitCDF ("25.0", status);

status = CDFputzVarsRecordDatabyNumbers (id, nVars, varNs2, 2L,
                                         varsRecBuffer2);
if (status < CDF_OK) QuitCDF ("25.1", status);

status = CDFgetzVarsRecordDatabyNumbers (id, nVars, varNs1, 2L,
                                         varsRecBuffer1Out);
if (status < CDF_OK) QuitCDF ("25.2", status);

status = CDFgetzVarsRecordDatabyNumbers (id, nVars, varNs2, 2L,
                                         varsRecBuffer2Out);
if (status < CDF_OK) QuitCDF ("25.3", status);

if (memcmp(varsRecBuffer1Out,varsRecBuffer1,
	   sizeof(varsRecBuffer1))) QuitCDF ("25.4", status);
if (memcmp(varsRecBuffer2Out,varsRecBuffer2,
	   sizeof(varsRecBuffer2))) QuitCDF ("25.5", status);

/******************************************************************************
* Inquire variables.
******************************************************************************/

status = CDFgetzVarName (id, var1Num_out, var1Name_out);
if (status < CDF_OK) QuitCDF ("26.0", status);

status = CDFgetzVarDataType (id, var1Num_out, &var1DataType_out);
if (status < CDF_OK) QuitCDF ("26.1", status);

status = CDFgetzVarNumElements (id, var1Num_out, &var1NumElements_out);
if (status < CDF_OK) QuitCDF ("26.2", status);

status = CDFgetzVarRecVariance (id, var1Num_out, &var1RecVariance_out);
if (status < CDF_OK) QuitCDF ("26.3", status);

status = CDFgetzVarDimVariances (id, var1Num_out, var1DimVariances_out);
if (status < CDF_OK) QuitCDF ("26.4", status);

status = CDFgetzVarBlockingFactor (id, var1Num_out, &blockingfactorOut1);
if (status < CDF_OK) QuitCDF ("26.5", status);

status = CDFgetzVarMaxAllocRecNum (id, var1Num_out, &maxAllocOut1);
if (status < CDF_OK) QuitCDF ("26.6", status);

status = CDFgetzVarMaxWrittenRecNum (id, var1Num_out, &maxRecOut1);
if (status < CDF_OK) QuitCDF ("26.7", status);

if (strcmp(var1Name_out,new_var1Name) != 0) QuitCDF ("26.11", status);
if (var1DataType_out != var1DataType) QuitCDF ("26.12", status);
if (var1NumElements_out != var1NumElements) QuitCDF ("26.13", status);
if (var1RecVariance_out != var1RecVariance) QuitCDF ("26.14", status);
if (var1Num_out != 0L) QuitCDF ("26.15", status);
if (blockingfactorOut1 != blockingfactor1) QuitCDF ("26.16", status);
if (maxAllocOut1 + 1 != allocRecs1) QuitCDF ("26.17", status);
if (maxRecOut1 != 2L) QuitCDF ("26.18", status);

for (dim_n = 0; dim_n < numDims; dim_n++) {
   if (var1DimVariances_out[dim_n] != var1DimVariances[dim_n]) {
     QuitCDF ("26.19", status);
   }
}

status = CDFgetzVarName (id, var2Num_out, var2Name_out);
if (status < CDF_OK) QuitCDF ("26.20", status);

status = CDFgetzVarDataType (id, var2Num_out, &var2DataType_out);
if (status < CDF_OK) QuitCDF ("26.21", status);

status = CDFgetzVarNumElements (id, var2Num_out, &var2NumElements_out);
if (status < CDF_OK) QuitCDF ("26.22", status);

status = CDFgetzVarRecVariance (id, var2Num_out, &var2RecVariance_out);
if (status < CDF_OK) QuitCDF ("26.23", status);

status = CDFgetzVarDimVariances (id, var2Num_out, var2DimVariances_out);
if (status < CDF_OK) QuitCDF ("26.24", status);

status = CDFgetzVarBlockingFactor (id, var2Num_out, &blockingfactorOut2);
if (status < CDF_OK) QuitCDF ("26.25", status);

status = CDFgetzVarMaxAllocRecNum (id, var2Num_out, &maxAllocOut2);
if (status < CDF_OK) QuitCDF ("26.26", status);

status = CDFgetzVarMaxWrittenRecNum (id, var2Num_out, &maxRecOut2);
if (status < CDF_OK) QuitCDF ("26.27", status);

if (strcmp(var2Name_out,new_var2Name) != 0) QuitCDF ("26.31", status);
if (var2DataType_out != var2DataType) QuitCDF ("26.32", status);
if (var2NumElements_out != var2NumElements) QuitCDF ("26.33", status);
if (var2RecVariance_out != var2RecVariance) QuitCDF ("26.34", status);
if (var2Num_out != 1L) QuitCDF ("26.35", status);
if (blockingfactorOut2 != blockingfactor2) QuitCDF ("26.36", status);
if (maxAllocOut2 + 1 != allocRecs2) QuitCDF ("26.37", status);
if (maxRecOut2 != 2L) QuitCDF ("26.38", status);


for (dim_n = 0; dim_n < numDims; dim_n++) {
   if (var2DimVariances_out[dim_n] != var2DimVariances[dim_n]) {
     QuitCDF ("26.39", status);
   }
}

status = CDFgetzVarName (id, var3Num_out, var3Name_out);
if (status < CDF_OK) QuitCDF ("26.40", status);

status = CDFgetzVarDataType (id, var3Num_out, &var3DataType_out);
if (status < CDF_OK) QuitCDF ("26.41", status);

status = CDFgetzVarNumElements (id, var3Num_out, &var3NumElements_out);
if (status < CDF_OK) QuitCDF ("26.42", status);

status = CDFgetzVarRecVariance (id, var3Num_out, &var3RecVariance_out);
if (status < CDF_OK) QuitCDF ("26.43", status);

status = CDFgetzVarDimVariances (id, var3Num_out, var3DimVariances_out);
if (status < CDF_OK) QuitCDF ("26.44", status);

status = CDFgetzVarNumDims (id, var3Num_out, &zNumDimsA_out);
if (status < CDF_OK) QuitCDF ("26.45", status);

status = CDFgetzVarDimSizes (id, var3Num_out, zDimSizesA_out);
if (status < CDF_OK) QuitCDF ("26.46", status);

status = CDFgetzVarBlockingFactor (id, var3Num_out, &blockingfactorOut3);
if (status < CDF_OK) QuitCDF ("26.47", status);

status = CDFgetzVarMaxAllocRecNum (id, var3Num_out, &maxAllocOut3);
if (status < CDF_OK) QuitCDF ("26.48", status);

status = CDFgetzVarMaxWrittenRecNum (id, var3Num_out, &maxRecOut3);
if (status < CDF_OK) QuitCDF ("26.49", status);

if (strcmp(var3Name_out,new_var3Name) != 0) QuitCDF ("26.53", status);
if (var3DataType_out != var3DataType) QuitCDF ("26.54", status);
if (var3NumElements_out != var3NumElements) QuitCDF ("26.55", status);
if (var3RecVariance_out != var3RecVariance) QuitCDF ("26.56", status);
if (zNumDimsA_out != zNumDimsA) QuitCDF ("26.57", status);
if (var3Num_out != 2L) QuitCDF ("26.58", status);
if (blockingfactorOut3 != blockingfactor3) QuitCDF ("26.59", status);
if (maxAllocOut3 + 1 != allocRecs3) QuitCDF ("26.60", status);
if (maxRecOut3 != 2L) QuitCDF ("26.61", status);

for (dim_n = 0; dim_n < zNumDimsA; dim_n++) {
   if (zDimSizesA_out[dim_n] != zDimSizesA[dim_n]) {
     QuitCDF ("26.62", status);
   }
   if (var3DimVariances_out[dim_n] != var3DimVariances[dim_n]) {
     QuitCDF ("26.63", status);
   }
}

/******************************************************************************
* Rename attribute.
******************************************************************************/

status = CDFrenameAttr (id, CDFgetAttrNum (id, attrName), new_attrName);
if (status < CDF_OK) QuitCDF ("27.0", status);

/******************************************************************************
* Inquire attribute.
******************************************************************************/

status = CDFgetAttrName (id, CDFgetAttrNum (id, new_attrName), attrName_out);
if (status < CDF_OK) QuitCDF ("28.0", status);

status = CDFgetAttrScope (id, CDFgetAttrNum (id, new_attrName), 
                          &attrScope_out);
if (status < CDF_OK) QuitCDF ("28.1", status);

status = CDFgetAttrMaxgEntry (id, CDFgetAttrNum (id, new_attrName), 
                              &maxEntry_out);
if (status < CDF_OK) QuitCDF ("28.2", status);

if (strcmp(attrName_out,new_attrName) != 0) QuitCDF ("28.4", status);
if (attrScope_out != attrScope) QuitCDF ("28.5", status);
if (maxEntry_out != entryNum) QuitCDF ("28.6", status);

/******************************************************************************
* Inquire attribute entries.
******************************************************************************/

status = CDFgetAttrgEntryDataType (id, 0L, entryNum, &entryDataType_out);
if (status < CDF_OK) QuitCDF ("29.0", status);

status = CDFgetAttrgEntryNumElements (id, 0L, entryNum, &entryNumElems_out);
if (status < CDF_OK) QuitCDF ("29.1", status);

if (entryDataType_out != entryDataType) QuitCDF ("29.2", status);
if (entryNumElems_out != entryNumElems) QuitCDF ("29.3", status);

status = CDFgetAttrzEntryDataType (id, 1L, 0L, &entryDataType_out);
if (status < CDF_OK) QuitCDF ("29.4", status);

status = CDFgetAttrzEntryNumElements (id, 1L, 0L, &entryNumElems_out);
if (status < CDF_OK) QuitCDF ("29.5", status);

if (entryDataType_out != CDF_BYTE) QuitCDF ("29.6", status);
if (entryNumElems_out != 1L) QuitCDF ("29.7", status);

status = CDFgetAttrzEntryDataType (id, 2L, 1L, &entryDataType_out);
if (status < CDF_OK) QuitCDF ("29.8", status);

status = CDFgetAttrzEntryNumElements (id, 2L, 1L, &entryNumElems_out);
if (status < CDF_OK) QuitCDF ("29.9", status);

if (entryDataType_out != CDF_REAL8) QuitCDF ("29.10", status);
if (entryNumElems_out != 1L) QuitCDF ("29.11", status);

/******************************************************************************
* Get error text.
******************************************************************************/

status = CDFgetStatusText (CDF_OK, errorText);
if (status < CDF_OK) QuitCDF ("30.0", status);

/******************************************************************************
* Select zMode and inquire CDF.
******************************************************************************/

status = CDFsetzMode (id, zMODEon2);
if (status < CDF_OK) QuitCDF ("31.0", status);

status = CDFgetNumgAttributes (id, &numGattrs);
if (status < CDF_OK) QuitCDF ("31.1", status);

status = CDFgetNumvAttributes (id, &numVattrs);
if (status < CDF_OK) QuitCDF ("31.2", status);

status = CDFgetNumzVars (id, &numZvars);
if (status < CDF_OK) QuitCDF ("31.4", status);

status = CDFgetAttrMaxgEntry (id, 0L, &maxGentry);
if (status < CDF_OK) QuitCDF ("31.5", status);

status = CDFgetNumAttrgEntries (id, 0L, &numGentries);
if (status < CDF_OK) QuitCDF ("31.6", status);

status = CDFgetzVarsMaxWrittenRecNum (id, &maxRecOut);
if (status < CDF_OK) QuitCDF ("31.7", status);

status = CDFgetAttrMaxzEntry (id, 1L, &maxZentry);
if (status < CDF_OK) QuitCDF ("31.10", status);

status = CDFgetNumAttrzEntries (id, 1L, &numZentries);
if (status < CDF_OK) QuitCDF ("31.11", status);

status = CDFsetzMode (id, zMODEoff);
if (status < CDF_OK) QuitCDF ("31.12", status);

status = CDFinquire (id,&numDimsX,dimSizesX,&encodingX,&majorityX,&maxrRecX,
                     &nrVarsX,&nAttrsX);
if (status < CDF_OK) QuitCDF ("31.12a", status);

status = CDFinquireCDF (id,&numDimsY,dimSizesY,&encodingY,&majorityY,&maxrRecY,
                        &nrVarsY,&maxzRecY,&nzVarsY,&nAttrsY);
if (status < CDF_OK) QuitCDF ("31.12b", status);

if (numGattrs != 1) QuitCDF ("31.13", status);
if (numVattrs != 2) QuitCDF ("31.14", status);
if (numZvars != 3) QuitCDF ("31.16", status);
if (maxGentry != entryNum) QuitCDF ("31.17", status);
if (numGentries != 1) QuitCDF ("31.18", status);
if (maxZentry != 0) QuitCDF ("31.21", status);
if (numZentries != 1) QuitCDF ("31.22", status);
if (maxRecOut != 2L) QuitCDF ("31.23", status);
if (numDimsX != numDimsY || numDimsX != 0) QuitCDF ("31.24", status);
if (encodingX != encodingY || encodingX != IBMPC_ENCODING) QuitCDF ("31.25", status);
if (majorityX != majorityY || majorityX != ROW_MAJOR) QuitCDF ("31.26", status);
/* if (maxrRecX != maxrRecY || maxrRecX != 0) QuitCDF ("31.27", status); */
if (nrVarsX != nrVarsY || nrVarsX != 0) QuitCDF ("31.28", status);
if (nAttrsX != nAttrsY || nAttrsX != 3) QuitCDF ("31.29", status);
if (nzVarsY != 3) QuitCDF ("31.30", status);
if (maxzRecY != 2) QuitCDF ("31.31", status);

/******************************************************************************
* Attempt to close variables.
******************************************************************************/

status = CDFclosezVar (id, 0L);
if (status != SINGLE_FILE_FORMAT) QuitCDF ("32.0", status);

status = CDFclosezVar (id, 2L);
if (status != SINGLE_FILE_FORMAT) QuitCDF ("32.1", status);

/******************************************************************************
* Modify entries/attribute.
******************************************************************************/

status = CDFsetAttrgEntryDataSpec (id, 0L, entryNum, entryDataTypeNew);
if (status < CDF_OK) QuitCDF ("33.0", status);

status = CDFsetAttrzEntryDataSpec (id, 1L, CDFgetVarNum(id, new_var1Name), 
                                   CDF_UINT1);
if (status < CDF_OK) QuitCDF ("33.1", status);

status = CDFsetAttrzEntryDataSpec (id, 2L, CDFgetVarNum(id, new_var2Name), 
                                   CDF_EPOCH);
if (status < CDF_OK) QuitCDF ("33.2", status);

status = CDFsetAttrScope (id, 0L, VARIABLE_SCOPE);
if (status < CDF_OK) QuitCDF ("33.3", status);

status = CDFsetAttrScope (id, 0L, GLOBAL_SCOPE);
if (status < CDF_OK) QuitCDF ("33.4", status);

/******************************************************************************
* Delete entries/attribute/variables and variable record.
******************************************************************************/

status = CDFdeleteAttrgEntry (id, 0L, entryNum);
if (status < CDF_OK) QuitCDF ("34.0", status);

status = CDFdeleteAttrzEntry (id, 1L, 0L);
if (status < CDF_OK) QuitCDF ("34.1", status);

status = CDFdeleteAttrzEntry (id, 2L, 1L);
if (status < CDF_OK) QuitCDF ("34.2", status);

status = CDFdeleteAttr (id, CDFgetAttrNum(id, new_attrName));
if (status < CDF_OK) QuitCDF ("34.3", status);

/* status = CDFdeletezVar (id, CDFgetVarNum(id, new_var1Name)); */
status = CDFdeletezVarRecords(id,CDFgetVarNum(id, new_var1Name), 0L, 0L);
if (status < CDF_OK) QuitCDF ("34.4", status);

/* status = CDFdeletezVar (id, CDFgetVarNum(id, new_var3Name)); */
status = CDFdeletezVarRecords(id,CDFgetVarNum(id, new_var3Name), 1L, 1L);
if (status < CDF_OK) QuitCDF ("34.5", status);

status = CDFdeletezVarRecords(id,CDFgetVarNum(id, new_var2Name), 2L, 2L);
if (status < CDF_OK) QuitCDF ("34.6", status);

/******************************************************************************
* Close CDF.
******************************************************************************/

status = CDFcloseCDF (id);  
if (status < CDF_OK) QuitCDF ("35.0", status);

/******************************************************************************
* Reopen and close CDF to validate the file.
******************************************************************************/

status = CDFopenCDF ("TEST", &id);
if (status < CDF_OK) QuitCDF ("36.0", status);

status = CDFcloseCDF (id);
if (status < CDF_OK) QuitCDF ("37.0", status);

/******************************************************************************
* Successful completion.
******************************************************************************/

return EXIT_SUCCESS_;
}


/******************************************************************************
* QuitCDF.
******************************************************************************/

void QuitCDF (where, status)
char *where;
CDFstatus status;
{
  char text[CDF_STATUSTEXT_LEN+1];
  printf ("Aborting at %s...\n", where);
  if (status < CDF_OK) {
    status = CDFgetStatusText (status, text); 
    printf ("%s\n", text);
  }
  status = CDFcloseCDF (id);
  printf ("...test aborted.\n");
  exit (EXIT_FAILURE_);
}
