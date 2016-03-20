/******************************************************************************
*
*  SPDF/CDF		Test program for operating variables in a CDF.
*
*  Version 1.0, 09-Dec-05
*
*  Modification history:
*
*  V1.0  09-Dec-05, M Liu       Original version (for CDF V3.1).
*                               A simple program to create zVariables, the
*                               preferred variables, to a CDF. It also
*                               shows how to respecify and verify the 
*                               variable's specification. Data values are
*                               written to and read from the variables in 
*                               differenet ways. 
******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cdf.h"

#define EXIT_SUCCESS_   0
#define EXIT_FAILURE_   1

/******************************************************************************
* Macros/prototypes.
******************************************************************************/

#define N_DIMS1         2
#define DIM_0_SIZE1     2
#define DIM_1_SIZE1     3

#define N_DIMS2        1
#define DIM_0_SIZE2    5
#define NUM_ELEMS2     8

void QuitCDF PROTOARGs((char *where, CDFstatus status));

/******************************************************************************
* Main.
******************************************************************************/

int main () {
CDFid id;
CDFstatus status;
int dim_n;
static long numDims1 = N_DIMS1;
static long numDims2 = N_DIMS2;
static long dimSizes[1] = { 0 };
static long dimSizes1[N_DIMS1] = { DIM_0_SIZE1, DIM_1_SIZE1 };
static long dimSizes2[N_DIMS2] = { DIM_0_SIZE2 };
static long var1DataType = { CDF_INT2 };
static long var1DataTypeNew = { CDF_UINT2 };
static long var2DataType = { CDF_CHAR };
static long var2DataTypeNew = { CDF_UCHAR };
long var1DataType_out, var2DataType_out;
static long var1NumElements = { 1 };
static long var1NumElementsNew = { 1 };
static long var2NumElements = { NUM_ELEMS2 };
static long var2NumElementsNew = { NUM_ELEMS2 };
long var1NumElements_out, var2NumElements_out;
long var1Num_out, var2Num_out, varNum_out1, varNum_out2;
static short var1Values[DIM_0_SIZE1][DIM_1_SIZE1] = {{1,2,3},{4,5,6}};
static char var2Values[DIM_0_SIZE2][NUM_ELEMS2] = {
  {'1','1','1','1','1','1','1','1'},
  {'2','2','2','2','2','2','2','2'},
  {'3','3','3','3','3','3','3','3'},
  {'4','4','4','4','4','4','4','4'},
  {'5','5','5','5','5','5','5','5'}
};
short var1Value_out;
static char var2Value_out[NUM_ELEMS2];
static long recNum = { 0 };
static long recStart = { 0 };
static long recCount = { 1 };
static long recInterval = { 1 };
long indices[N_DIMS1];
static long counts[N_DIMS1] = { DIM_0_SIZE1, DIM_1_SIZE1 };
static long intervals[N_DIMS1] = { 1, 1 };
static long zRecNum = { 0 };
static long zRecStart = { 0 };
static long zRecCount = { 1 };
static long zRecInterval = { 1 };
long zIndicesA[N_DIMS2];
static long zCounts[N_DIMS2] = { DIM_0_SIZE2 };
static long zIntervals[N_DIMS2] = { 1 };
short var1Buffer_out[DIM_0_SIZE1][DIM_1_SIZE1];
char var2Buffer_out[DIM_0_SIZE2][NUM_ELEMS2];
long numDims1_out;
long dimSizes1_out[N_DIMS1];
long numDims2_out;
long dimSizes2_out[N_DIMS2];
long maxRec_out;
long numAttrs_out;
int i, x0, x1, x;
static long var1RecVariance = { VARY };
static long var1RecVarianceNew = { NOVARY };
static long var2RecVariance = { VARY };
static long var2RecVarianceNew = { NOVARY };
long var1RecVariance_out, var2RecVariance_out;
static long var1DimVariances[N_DIMS1] = { VARY, VARY };
static long var1DimVariancesNew[N_DIMS1] = { NOVARY, NOVARY };
static long var2DimVariances[N_DIMS2] = { VARY };
static long var2DimVariancesNew[N_DIMS2] = { NOVARY };
long var1DimVariances_out[N_DIMS1],
     var2DimVariances_out[N_DIMS2];
static char var1Name[] = "VAR1a";
static char var2Name[] = "zVARa1";
static char new_var1Name[] = "VAR1b";
static char new_var2Name[] = "zVARa2";
char var1Name_out[CDF_VAR_NAME_LEN256+1],
     var2Name_out[CDF_VAR_NAME_LEN256+1];
char CopyRightText[CDF_COPYRIGHT_LEN+1];
char errorText[CDF_STATUSTEXT_LEN+1];
long numRvars, numZvars;
static short pad1 = { -999 };
static char pad2[NUM_ELEMS2+1] = { "********" };
short pad1out;
static char pad2out[NUM_ELEMS2+1] = { "        " };
static long blockingfactor1 = 3;
static long blockingfactor2 = 4;
long blockingfactorOut1, blockingfactorOut2;
long recStartOut, recCountOut, recIntervalOut, recNumOut;
long indicesOut[CDF_MAX_DIMS],
     countsOut[CDF_MAX_DIMS],
     intervalsOut[CDF_MAX_DIMS];
int dimN;
long formatOut;
long maxAllocOut1, maxAllocOut2;
long maxRecOut1, maxRecOut2, maxRecOut;
long nIndexRecsOut1, nIndexRecsOut2;
long nIndexEntriesOut1, nIndexEntriesOut2;
static long allocRecs1 = { 10 };
static long allocRecs2 = { 8 };
static long nZvars1 = { 1 };
static long zVarNs1[2] = { 0 };
static char zVarsRecBuffer1[DIM_0_SIZE1][DIM_1_SIZE1][6] = {
  {{0,0,0,0,0,0},{0,0,0,0,0,0},{0,0,0,0,0,0}},
  {{0,0,0,0,0,0},{0,0,0,0,0,0},{0,0,0,0,0,0}}
};
static char zVarsRecBufferOut1[DIM_0_SIZE1][DIM_1_SIZE1][6];
static long nZvars2 = { 1 };
static long zVarNs2[1] = { 1 };
static char zVarsRecBuffer2[DIM_0_SIZE2][NUM_ELEMS2] = {
  {'%','%','%','%','%','%','%','%'},
  {'%','%','%','%','%','%','%','%'},
  {'%','%','%','%','%','%','%','%'},
  {'%','%','%','%','%','%','%','%'},
  {'%','%','%','%','%','%','%','%'}
};
static char zVarsRecBufferOut2[DIM_0_SIZE2][NUM_ELEMS2];

/******************************************************************************
* Create CDF.
******************************************************************************/
/* CDFsetFileBackwardFlag(BACKWARDFILEon); */
status = CDFlib (CREATE_, CDF_, "TEST", 0L, dimSizes, &id,
		 NULL_); 

if (status < CDF_OK) {
  if (status == CDF_EXISTS) {
    status = CDFlib (OPEN_, CDF_, "TEST", &id,
                     NULL_);
    if (status < CDF_OK) QuitCDF ("1.0", status);
 
    status = CDFlib (DELETE_, CDF_,
                     NULL_);
    if (status < CDF_OK) QuitCDF ("1.1", status);
    
    status = CDFlib (CREATE_, CDF_, "TEST", 0L, dimSizes, &id,
                     NULL_);
    if (status < CDF_OK) QuitCDF ("1.2", status);
  }
  else
    QuitCDF ("1.3", status);
}

/******************************************************************************
* Create variables.
******************************************************************************/

status = CDFlib (CREATE_, zVAR_, var1Name, var1DataType, var1NumElements,
		                 numDims1, dimSizes1,
				 var1RecVariance, var1DimVariances,
				 &var1Num_out,
		 NULL_);
if (status < CDF_OK) QuitCDF ("2.0", status);

status = CDFlib (CREATE_, zVAR_, var2Name, var2DataType, var2NumElements,
		                 numDims2, dimSizes2,
		                 var2RecVariance, var2DimVariances,
				 &var2Num_out,
		 PUT_, zVAR_ALLOCATERECS_, allocRecs2,
                       zVAR_BLOCKINGFACTOR_, blockingfactor2,
		 NULL_);
if (status < CDF_OK) QuitCDF ("2.1", status);

/******************************************************************************
* Set/confirm pad values.
******************************************************************************/

status = CDFlib (SELECT_, CDF_, id,
                          zVAR_, var1Num_out,
		 PUT_, zVAR_PADVALUE_, &pad1,
		 SELECT_, zVAR_, var2Num_out,
                 PUT_, zVAR_PADVALUE_, &pad2,
		 NULL_);
if (status < CDF_OK) QuitCDF ("3.0", status);

status = CDFlib (SELECT_, zVAR_, var1Num_out,
		 GET_, zVAR_PADVALUE_, &pad1out,
		 SELECT_, zVAR_, var2Num_out,
		 GET_, zVAR_PADVALUE_, &pad2out,
                 NULL_);
if (status < CDF_OK) QuitCDF ("3.1", status);

if (pad1out != pad1) QuitCDF ("3.2", status);
if (strcmp(pad2out,pad2)) QuitCDF ("3.3", status);

/******************************************************************************
* Write to variables - one value at a time.
******************************************************************************/

status = CDFlib (SELECT_, zVAR_, var1Num_out,
		          zVAR_RECNUMBER_, recNum,
		 NULL_);
if (status < CDF_OK) QuitCDF ("4.0", status);

for (x0 = 0; x0 < DIM_0_SIZE1; x0++)
   for (x1 = 0; x1 < DIM_1_SIZE1; x1++) {
      indices[0] = x0;
      indices[1] = x1;
      status = CDFlib (SELECT_, zVAR_DIMINDICES_, indices,
                       PUT_, zVAR_DATA_, &var1Values[x0][x1],
                       NULL_);
     if (status < CDF_OK) QuitCDF ("4.1", status);
}

status = CDFlib (SELECT_, zVAR_, var2Num_out,
			  zVAR_RECNUMBER_, zRecNum,
		 NULL_);
if (status < CDF_OK) QuitCDF ("4.2", status);

for (x0 = 0; x0 < DIM_0_SIZE2; x0++) {
   zIndicesA[0] = x0;
   status = CDFlib (SELECT_, zVAR_DIMINDICES_, zIndicesA,
		    PUT_, zVAR_DATA_, var2Values[x0],
		    NULL_);
   if (status < CDF_OK) QuitCDF ("4.3", status);
}

/******************************************************************************
* Read/verify for the variables - one value at a time.
******************************************************************************/

status = CDFlib (SELECT_, zVAR_, var1Num_out,
                          zVAR_RECNUMBER_, recNum,
                 NULL_);
if (status < CDF_OK) QuitCDF ("5.0", status);

for (x0 = 0; x0 < DIM_0_SIZE1; x0++)
   for (x1 = 0; x1 < DIM_1_SIZE1; x1++) {
      indices[0] = x0;
      indices[1] = x1;
      status = CDFlib (SELECT_, zVAR_DIMINDICES_, indices,
		       GET_, zVAR_DATA_, &var1Value_out,
		       NULL_);
      if (status < CDF_OK) QuitCDF ("5.1", status);

      if (var1Value_out != var1Values[x0][x1]) QuitCDF ("5.2", status);
   }

status = CDFlib (SELECT_, zVAR_, var2Num_out,
                          zVAR_RECNUMBER_, zRecNum,
                 NULL_);
if (status < CDF_OK) QuitCDF ("5.3", status);

for (x0 = 0; x0 < DIM_0_SIZE2; x0++) {
   zIndicesA[0] = x0;
   status = CDFlib (SELECT_, zVAR_DIMINDICES_, zIndicesA,
		    GET_, zVAR_DATA_, var2Value_out,
		    NULL_);
   if (status < CDF_OK) QuitCDF ("5.4", status);

   for (i = 0; i < NUM_ELEMS2; i++) {
      if (var2Value_out[i] != var2Values[x0][i]) QuitCDF ("5.5", status);
   }
}

/******************************************************************************
* HyperPUT to the variables - one full record at a time.
******************************************************************************/

for (x0 = 0; x0 < DIM_0_SIZE1; x0++)
   for (x1 = 0; x1 < DIM_1_SIZE1; x1++) {
      var1Values[x0][x1] = -var1Values[x0][x1];
   }

indices[0] = 0;
indices[1] = 0;

status = CDFlib (SELECT_, zVAR_, var1Num_out,
		          zVAR_RECNUMBER_, recStart,
			  zVAR_RECCOUNT_, recCount,
			  zVAR_RECINTERVAL_, recInterval,
			  zVAR_DIMINDICES_, indices,
			  zVAR_DIMCOUNTS_, counts,
			  zVAR_DIMINTERVALS_, intervals,
			  zVAR_, var1Num_out,
		 PUT_, zVAR_HYPERDATA_, var1Values,
		 NULL_);
if (status < CDF_OK) QuitCDF ("6.0", status);

for (x0 = 0; x0 < DIM_0_SIZE2; x0++)
   for (i = 0; i < NUM_ELEMS2; i++) {
      var2Values[x0][i]++;
   }

zIndicesA[0] = 0;

status = CDFlib (SELECT_, zVAR_, var2Num_out,
		          zVAR_RECNUMBER_, zRecStart,
			  zVAR_RECCOUNT_, zRecCount,
			  zVAR_RECINTERVAL_, zRecInterval,
			  zVAR_DIMINDICES_, zIndicesA,
			  zVAR_DIMCOUNTS_, zCounts,
			  zVAR_DIMINTERVALS_, zIntervals,
		 PUT_, zVAR_HYPERDATA_, var2Values,
		 NULL_);
if (status < CDF_OK) QuitCDF ("6.1", status);

/******************************************************************************
* HyperGET/verify for variables - one record at a time.
******************************************************************************/

status = CDFlib (SELECT_, zVAR_, var1Num_out,
		          zVAR_RECNUMBER_, recStart,
			  zVAR_RECCOUNT_, recCount,
			  zVAR_RECINTERVAL_, recInterval,
			  zVAR_DIMINDICES_, indices,
			  zVAR_DIMCOUNTS_, counts,
			  zVAR_DIMINTERVALS_, intervals,
			  zVAR_, var1Num_out,
		 GET_, zVAR_HYPERDATA_, var1Buffer_out,
		 NULL_);
if (status < CDF_OK) QuitCDF ("7.0", status);

for (x0 = 0; x0 < DIM_0_SIZE1; x0++)
   for (x1 = 0; x1 < DIM_1_SIZE1; x1++) {
      if (var1Buffer_out[x0][x1] != var1Values[x0][x1])
	QuitCDF ("7.1", status);
   }

status = CDFlib (SELECT_, zVAR_, var2Num_out,
                          zVAR_RECNUMBER_, zRecStart,
                          zVAR_RECCOUNT_, zRecCount,
                          zVAR_RECINTERVAL_, zRecInterval,
                          zVAR_DIMINDICES_, zIndicesA,
                          zVAR_DIMCOUNTS_, zCounts,
                          zVAR_DIMINTERVALS_, zIntervals,
	         GET_, zVAR_HYPERDATA_, var2Buffer_out,
		 NULL_);
if (status < CDF_OK) QuitCDF ("7.2", status);

for (x0 = 0; x0 < DIM_0_SIZE2; x0++)
   for (i = 0; i < NUM_ELEMS2; i++) {
      if (var2Buffer_out[x0][i] != var2Values[x0][i])
	QuitCDF ("7.3", status);
   }

/******************************************************************************
* Confirm hyper parameters for a zVariable.
******************************************************************************/

status = CDFlib (CONFIRM_, zVAR_RECNUMBER_, &recStartOut,
			   zVAR_RECCOUNT_, &recCountOut,
			   zVAR_RECINTERVAL_, &recIntervalOut,
			   zVAR_DIMINDICES_, indicesOut,
			   zVAR_DIMCOUNTS_, countsOut,
			   zVAR_DIMINTERVALS_, intervalsOut,
		 NULL_);
if (status < CDF_OK) QuitCDF ("8.0", status);

if (recStartOut != zRecStart) QuitCDF ("8.1", status);
if (recCountOut != zRecCount) QuitCDF ("8.2", status);
if (recIntervalOut != zRecInterval) QuitCDF ("8.3", status);
for (dimN = 0; dimN < N_DIMS2; dimN++) {
   if (indicesOut[dimN] != zIndicesA[dimN]) QuitCDF ("8.4", status);
   if (countsOut[dimN] != zCounts[dimN]) QuitCDF ("8.5", status);
   if (intervalsOut[dimN] != zIntervals[dimN]) QuitCDF ("8.6", status);
}

/******************************************************************************
* Set/confirm sequential access position for a zVariable (and read/write a
* value).
******************************************************************************/

status = CDFlib (SELECT_, zVAR_SEQPOS_, zRecStart, zIndicesA,
		 GET_, zVAR_SEQDATA_, var2Value_out,
		 PUT_, zVAR_SEQDATA_, var2Value_out,
		 CONFIRM_, zVAR_SEQPOS_, &recNumOut, indicesOut,
		 NULL_);
if (status < CDF_OK) QuitCDF ("9.0", status);

if (recNumOut != zRecStart) QuitCDF ("9.1", status);
if (indicesOut[0] != zIndicesA[0] + 2) QuitCDF ("9.2", status);


/******************************************************************************
* Confirm existence of variables.
******************************************************************************/

status = CDFlib (CONFIRM_, zVAR_EXISTENCE_, var1Name,
			   zVAR_EXISTENCE_, var2Name,
		 NULL_);
if (status < CDF_OK) QuitCDF ("10.0", status);

/******************************************************************************
* Inquire variable numbers.
******************************************************************************/

status = CDFlib (GET_, zVAR_NUMBER_, var1Name, &varNum_out1,
		       zVAR_NUMBER_, var2Name, &varNum_out2,
		 NULL_);
if (status < CDF_OK) QuitCDF ("11.0", status);

if (varNum_out1 != 0) QuitCDF ("11.2", status);
if (varNum_out2 != 1) QuitCDF ("11.3", status);

/******************************************************************************
* Rename variables.
******************************************************************************/

status = CDFlib (SELECT_, zVAR_NAME_, var1Name,
		 PUT_, zVAR_NAME_, new_var1Name,
		 NULL_);
if (status < CDF_OK) QuitCDF ("12.0", status);

status = CDFlib (SELECT_, zVAR_NAME_, var2Name,
		 PUT_, zVAR_NAME_, new_var2Name,
		 NULL_);
if (status < CDF_OK) QuitCDF ("12.1", status);

/******************************************************************************
* Read/write multiple variable data - one record for each variable involved.
******************************************************************************/

status = CDFlib (SELECT_, zVARs_RECNUMBER_, 2L,
		 PUT_, zVARs_RECDATA_, nZvars1, zVarNs1, zVarsRecBuffer1,
		 SELECT_, zVARs_RECNUMBER_, 2L,
		 PUT_, zVARs_RECDATA_, nZvars2, zVarNs2, zVarsRecBuffer2,
		 NULL_);
if (status < CDF_OK) QuitCDF ("13.0", status);

status = CDFlib (GET_, zVARs_RECDATA_, nZvars1, zVarNs1, zVarsRecBufferOut1,
		 GET_, zVARs_RECDATA_, nZvars2, zVarNs2, zVarsRecBufferOut2,
		 NULL_);
if (status < CDF_OK) QuitCDF ("13.1", status);

if (memcmp(zVarsRecBufferOut1,zVarsRecBuffer1,
	   sizeof(zVarsRecBuffer1))) QuitCDF ("13.3", status);
if (memcmp(zVarsRecBufferOut2,zVarsRecBuffer2,
	   sizeof(zVarsRecBuffer2))) QuitCDF ("13.4", status);

/******************************************************************************
* Inquire variables.
******************************************************************************/

status = CDFlib (SELECT_, zVAR_, var1Num_out,
		 GET_, zVAR_NAME_, var1Name_out,
		       zVAR_DATATYPE_, &var1DataType_out,
		       zVAR_NUMELEMS_, &var1NumElements_out,
		       zVAR_BLOCKINGFACTOR_, &blockingfactorOut1,
		       zVAR_MAXallocREC_, &maxAllocOut1,
		       zVAR_MAXREC_, &maxRecOut1,
		       zVAR_nINDEXRECORDS_, &nIndexRecsOut1,
		       zVAR_nINDEXENTRIES_, &nIndexEntriesOut1,
		 CONFIRM_, zVAR_, &var1Num_out,
		 NULL_);
if (status < CDF_OK) QuitCDF ("14.0", status);

if (strcmp(var1Name_out,new_var1Name) != 0) QuitCDF ("14.1", status);
if (var1DataType_out != var1DataType) QuitCDF ("14.2", status);
if (var1NumElements_out != var1NumElements) QuitCDF ("14.3", status);
if (var1Num_out != 0L) QuitCDF ("14.4", status);
if (blockingfactorOut1 != blockingfactor1) QuitCDF ("14.5", status);
if (maxAllocOut1 + 1 != allocRecs1) QuitCDF ("14.6", status);
if (maxRecOut1 != 2L) QuitCDF ("14.7", status);

status = CDFlib (SELECT_, zVAR_, var2Num_out,
		 GET_, zVAR_NAME_, var2Name_out,
		       zVAR_DATATYPE_, &var2DataType_out,
		       zVAR_NUMELEMS_, &var2NumElements_out,
		       zVAR_BLOCKINGFACTOR_, &blockingfactorOut2,
		       zVAR_MAXallocREC_, &maxAllocOut2,
		       zVAR_MAXREC_, &maxRecOut2,
		       zVAR_nINDEXRECORDS_, &nIndexRecsOut2,
		       zVAR_nINDEXENTRIES_, &nIndexEntriesOut2,
		 CONFIRM_, zVAR_, &var2Num_out,
		 NULL_);
if (status < CDF_OK) QuitCDF ("14.8", status);

if (strcmp(var2Name_out,new_var2Name) != 0) QuitCDF ("14.9", status);
if (var2DataType_out != var2DataType) QuitCDF ("14.10", status);
if (var2NumElements_out != var2NumElements) QuitCDF ("14.11", status);
if (var2Num_out != 1L) QuitCDF ("14.12", status);
if (blockingfactorOut2 != blockingfactor2) QuitCDF ("14.13", status);
if (maxAllocOut2 + 1 != allocRecs2) QuitCDF ("14.14", status);
if (maxRecOut2 != 2L) QuitCDF ("14.15", status);

/******************************************************************************
* Close CDF.
******************************************************************************/

status = CDFlib (CLOSE_, CDF_,
		 NULL_);
if (status < CDF_OK) QuitCDF ("15.0", status);

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
    CDFlib (SELECT_, CDF_STATUS_, status,
	    GET_, STATUS_TEXT_, text,
	    NULL_);
    printf ("%s\n", text);
  }
  CDFlib (CLOSE_, CDF_,
	  NULL_);
  printf ("...test aborted.\n");
  exit (EXIT_FAILURE_);
}
