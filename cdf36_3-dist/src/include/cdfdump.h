/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                                    Header file for CDFdump.
*
*  Version 1.6a, 17-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  23-Sep-91, J Love     Original version (for CDF V2.1).
*   V1.1  17-May-92, J Love     IBM PC port.
*   V1.2  28-Aug-92, J Love     CDF V2.3 (shareable/NeXT/zVar).
*   V1.3  26-Nov-93, J Love     CDF V2.4.
*   V1.4  27-Oct-94, J Love     CDF V2.5.
*   V1.4a 27-Feb-95, J Love     `value' qualifier.
*   V1.5  28-Mar-95, J Love     POSIX.
*   V1.5a 14-Sep-95, J Love     Hyper groups.
*   V1.6   2-Jul-96, J Love	CDF V2.6.
*   V1.6a 17-Nov-97, J Love	Windows NT/Visual C++.
*   V1.7  19-Jun-02, M Liu      Added the default float/double tolerance
*                               values.
*
******************************************************************************/

#if !defined(CDFDUMPh_INCLUDEd__)
#define CDFDUMPh_INCLUDEd__

/******************************************************************************
* Include files.
******************************************************************************/

#include "cdflib.h"
#include "cdftools.h"

/******************************************************************************
* Parameter/qualifier constants.
******************************************************************************/

#define CDFPATHparm     0

#define DUMPqual        0
#define OUTPUTqual      1
#define FORMATqual      2
#define NOFORMATqual    3
#define PRECISIONqual   4
#define VARSqual        5
#define ABOUTqual	6
#define HEADERqual	7
#define NOHEADERqual	8
#define RECORDRANGEqual 9
#define COL2ROWqual    10
#define DETECTqual     11
#define EPOCHVVqual    12

#define ALLDUMP         0
#define DATADUMP        1
#define METADATADUMP    2
#define GLOBALDUMP      3
#define VARIABLEDUMP    4

#define DEFAULToptionDUMP ALLDUMP
/******************************************************************************
* Global variables.
******************************************************************************/
#if defined(CDFDUMP)
int dump = DEFAULToptionDUMP;
Logical useFormat = DEFAULTformatDUMP;
char outputCDF[CDF_PATHNAME_LEN+1] = "default";
char dumpOption[20];
char varsOption[513];
Logical header = DEFAULTheaderDUMP;
Logical col2row = DEFAULTcol2rowDUMP;
long startingRec, endingRec;
Logical epochVV = DEFAULTepochVVDUMP;
#else
extern char outputCDF[CDF_PATHNAME_LEN+1];
extern int dump;
extern Logical useFormat;
extern char dumpOption[20];
extern char varsOption[513];
extern Logical header;
extern Logical col2row;
extern long startingRec, endingRec;
extern Logical epochVV;
#endif
/******************************************************************************
* Function Prototypes.
******************************************************************************/

Logical DumpCDF PROTOARGs((int argC, char *argV[]));
Logical QuitCDF PROTOARGs((char *file, char *text, CDFstatus status, char *msg));

/*****************************************************************************/

#endif
