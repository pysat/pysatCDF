/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF					Header file for SkeletonCDF.
*
*  Version 1.6a, 18-Nov-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  13-May-92, J Love	Original version.
*   V1.1   3-Jun-92, J Love	Increased MAX_STREAM_LEN.
*   V1.2  22-Jul-92, J Love	CDF V2.3 (shareable/NeXT/zVar).
*   V1.3  26-Nov-93, J Love	CDF V2.4.
*   V1.3a  6-Feb-94, J Love	DEC Alpha/OpenVMS port.
*   V1.3b  8-Apr-94, J Love	Solaris using Gnu C compiler.
*   V1.4  13-Dec-94, J Love	CDF V2.5.
*   V1.4a 28-Feb-95, J Love	Pass `char' as `int'.
*   V1.5  28-Mar-95, J Love	POSIX.
*   V1.6   6-Aug-96, J Love	CDF V2.6.
*   V1.6a 18-Nov-97, J Love	Windows NT/Visual C++.
*
******************************************************************************/

#if !defined(SKT2CDFh_INCLUDEd__)
#define SKT2CDFh_INCLUDEd__

/******************************************************************************
* Include files.
******************************************************************************/

#include "cdftools.h"
#include "cdflib.h" 

#if defined(mac)
#  include "skt2cdf.rh"
#endif

/******************************************************************************
* Parameter/qualifier constants.
******************************************************************************/

#define SKTPATHparm	0

#define CDFPATHqual	0
#define LOGqual		1
#define NOLOGqual	2
#define DELqual		3
#define NODELqual	4
#define FILLVALqual	5
#define NOFILLVALqual	6
#define REPORTqual	7
#define NEG2POSqual	8
#define NONEG2POSqual	9
#define ABOUTqual	10
#define ZMODEqual	11
#define STATSqual	12
#define NOSTATSqual	13
#define CACHEqual	14
#define WARNINGqual	15
#define NOWARNINGqual	16
#define BACKWARDqual	17

/******************************************************************************
* Lengths/sizes.
******************************************************************************/

#define MAX_SKTLINE_LEN		132
#define MAX_MSG_LEN		132
#define BASE_STREAM_SIZE	132
#define BASE_TOKEN_SIZE		50
#define BASE_STRING_SIZE	132
#define BASE_LOCATION_SIZE	20
#define BASE_EVALUE_SIZE	132

/******************************************************************************
* Enumerated types.
******************************************************************************/

enum searchForENUM { TOKEN, DELIMSTRING, BRACEDchar, BRACEDnonChar,
		     VALLOCATION, VAREPOCH, VAREPOCH16, VARTT2000, VARCHAR };

enum nextItemENUM { HEADERmark,
		    CDForOUTPUTmark,
		    FILEmark,
		    NAMEmark,
		    NAMECOLONmark,
		    CDFPATHfield,
		    DATAmark,
		    ENCODINGmark,
		    ENCODINGCOLONmark,
		    ENCODINGfield,
		    MAJORITYmark,
		    MAJORITYCOLONmark,
		    MAJORITYfield,
		    FORMATmarkOrNUMVARSfield,
		    FORMATCOLONmark,
		    FORMATfield,
		    NUMVARSfield,
		    NUMGATTRSfield,
		    NUMVATTRSfield,
		    NUMRECSfield,
		    NUMDIMSfield,
		    DIMSIZEfield,
		    CDFCOMPRESSIONmark,
		    CDFCOMPRESSIONCOLONmark,
		    CDFCOMPRESSIONfield,
		    CDFCHECKSUMmark,
		    CDFCHECKSUMCOLONmark,
		    CDFCHECKSUMfield,
		    CDFLEAPSECONDLASTUPDATEDmark,
		    CDFLEAPSECONDLASTUPDATEDCOLONmark,
		    CDFLEAPSECONDLASTUPDATEDfield,
		    GATTRmark,
		    GaNAMEfieldOrVATTRmark,
		    GaNAMEfield,
		    PERIODmarkOrGa1stENTRYNUMfield,
		    GaENTRYNUMfield,
		    GaDATATYPEfield,
		    GaVALUEfield,
		    PERIODmarkOrGaENTRYNUMfield,
		    GaDATATYPEfieldOrBRACEmark,
		    VaNAMEfieldOrVARmark,
		    VaNAMEfield,
		    VARNAMEfieldORzVARmarkOrENDmark,
		    VARNAMEfieldOrENDmark,
		    VARNAMEfield,
		    VARDATATYPEfield,
		    VARNUMELEMSfield,
		    zVARNUMDIMSfield,
		    zVARDIMSIZEfield,
		    VARRECVARYfield,
		    VARDIMVARYfield,
		    VaNAMEfieldOrPERIODmark,
		    VaeNAMEfield,
		    VaeDATATYPEfield,
		    VaeVALUEfield,
		    LOCfieldOrVARNAMEfieldOrENDmark,
		    LOCfieldOrVARNAMEfieldORzVARmarkOrENDmark,
		    LOCfield,
		    EQUALmark,
		    VARCOMPRESSIONmark,
		    VARCOMPRESSIONCOLONmark,
		    VARCOMPRESSIONfield,
		    VARSPARSEmark,
		    VARSPARSECOLONmark,
		    VARSPARSEfield,
		    VARBLOCKINGmark,
		    VARBLOCKINGCOLONmark,
		    VARBLOCKINGfield,
		    VARPADVALUEmark,
		    VARPADVALUECOLONmark,
		    VARPADVALUEfield,
		    VARVALUEfield };

/******************************************************************************
* TEMPcharSTRING.
* Note: This only works if this is between the last variable declaration and
* the first executable statement.
******************************************************************************/

#define TEMPcharSTRING(chr,str) \
char str[1+1]; \
str[0] = (char) chr; \
str[1] = NUL;

/******************************************************************************
* Function Prototypes.
******************************************************************************/

Logical CreateSkeletonCDF PROTOARGs((int argC, char *argV[]));
Logical CreateSkeletonQOPs PROTOARGs((int *argC, char **argV[]));
Logical CharStream PROTOARGs((int chr));
Logical ItemStream PROTOARGs((char *item));
void ParseError PROTOARGs((char *msg));
void NextSearchItem PROTOARGs((enum searchForENUM type,
			                   enum nextItemENUM item));
void ItemError1 PROTOARGs((char *expected, char *found));
void ItemError2 PROTOARGs((char *expected1, char *expected2, char *found));
void ItemError3 PROTOARGs((char *expected1, char *expected2, char *expected3,
			               char *found));
long WhichDataType PROTOARGs((char *item));
void CATchr PROTOARGs((char **str, int chr, size_t *size, int base));
Logical RecurCharStream PROTOARGs((char *stream, enum searchForENUM type,
			                       enum nextItemENUM next));
void AllocateGrowingStrings PROTOARGs((void));
void FreeGrowingStrings PROTOARGs((void));
Logical StatusHandlerS2C PROTOARGs((CDFstatus, char *));

/*****************************************************************************/

#endif
