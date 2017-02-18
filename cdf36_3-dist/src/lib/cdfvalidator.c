/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                      Validate internal records in a CDF V2.* file
*
*  Version 1.0, 15-Jul-08, Peort Systems.
*
*  Modification history:
*
*   V1.0  15-Jul-08, M Liu      Original version.
*   V1.1  20-Aug-09, M Liu      Modified ValidateCDF to handle V2.0 files that
*                               have no filesize field. Also, don't look for
*                               any VDRs when the last one is read. 
*   V1.2  12-Aug-10, M Liu      Ensured the debugging message will not overrun
*                               the defined string in QuitCDF.
*
******************************************************************************/

#include "cdflib.h"

/******************************************************************************
* Local macro definitions.
******************************************************************************/

#define CRE CDF_READ_ERROR
#define CV2C CORRUPTED_V2_CDF

/******************************************************************************
* Local function prototypes.
******************************************************************************/

static CDFstatus QuitCDF PROTOARGs((char *why, Int32 offset, int size, int num,
void *value1, void *value2, Logical debug));
static CDFstatus ValidateCCR PROTOARGs((vFILE *fp, Int32 offset, Logical debug));
static CDFstatus ValidateCDR PROTOARGs((vFILE *fp, Int32 offset, Logical debug));
static CDFstatus ValidateGDR PROTOARGs((vFILE *fp, Int32 offset, Logical debug));
static CDFstatus ValidateVDR PROTOARGs((struct CDFstruct *CDF, vFILE *fp, Int32 offset, Logical zVar, Logical debug));
static CDFstatus ValidateVXR PROTOARGs((vFILE *fp, Int32 offset, Int32 lastRec, Logical debug));
static CDFstatus ValidateVVR PROTOARGs((vFILE *fp, Int32 offset, Logical debug));
static CDFstatus ValidateADR PROTOARGs((struct CDFstruct *CDF, vFILE *fp, Int32 offset, Logical debug));
static CDFstatus ValidateAEDR PROTOARGs((struct CDFstruct *CDF, vFILE *fp, Int32 offset, Int32 num, Int32 maxEntry, Logical zEntry, Logical debug));
static CDFstatus ValidateCPR PROTOARGs((vFILE *fp, Int32 offset, Logical debug));
static CDFstatus ValidateSPR PROTOARGs((vFILE *fp, Int32 offset, Logical debug));
static CDFstatus ValidateCVVR PROTOARGs((vFILE *fp, Int32 offset, Logical debug));
static CDFstatus ValidateUIR PROTOARGs((vFILE *fp, Int32 offset, Logical debug));
static CDFstatus ValidateAttributeLinks PROTOARGs((struct CDFstruct *CDF, vFILE *fp, Logical debug));
static CDFstatus ValidateAttributeEntryLink PROTOARGs((struct CDFstruct *CDF, vFILE *fp, Int32 num, Logical zEntry, Int32 AEDRhead, Int32 numEntries, Int32 maxEntry, Logical debug));
static CDFstatus ValidateVariableLinks PROTOARGs((struct CDFstruct *CDF, vFILE *fp, Logical zVar, Logical debug));
static CDFstatus ValidateVariableValueLinks PROTOARGs((struct CDFstruct *CDF, vFILE *fp, Int32 lastRec, Int32 headVXR, Logical debug));
static struct CDRstruct CDR;
static struct GDRstruct GDR;
static int rVars, zVars;
static int numAttrs;

/******************************************************************************
* QuitCDF.
******************************************************************************/

static CDFstatus QuitCDF (char *why, Int32 offset, int size, int num,
	                  void *value1, void *value2, Logical debug)
{
  
  if (debug) {
    char text[151], texta[41];
    text[0] = NUL;
    strcpyX (text, why, 150);
    if (offset >= 0) {
      snprintf (texta, (size_t) sizeof(texta),
                " (@%ld) ", (long) offset);
      strcatX (text, texta, 150);
    }
    if (strlen(why) < 150) {
      if (num == 2) { /* Two argument values to show. */
        if (size == 4) { /* Each one is a 4-byte int type. */
                         /* Each int is up to 10 digits + sign. */
          char text2[2 * 11 + 6 + 1];
          text2[0] = NUL;
          snprintf (text2, (size_t) sizeof(text2),
		    "(%ld vs %ld)", (long) *(int *)value1,
                    (long) *(int *)value2);
          strcatX (text, text2, 150);
        } else { /* Both are strings. */
          char *text2;
          size_t len = strlen ((char *) value1) + strlen ((char *) value2) + 
                       6 + 1;
          text2 = (char *) cdf_AllocateMemory (len, NULL);
          if (text2 != NULL) {
            text2[0] = NUL;
            snprintf (text2, len, "(%s vs %s)", (char *)value1, (char *)value2);
            strcatX (text, text2, 150);
            cdf_FreeMemory (text2, NULL);
          }
        }
      } else { /* Only one argument value to show. */
        if (size == 4) { /* Each int is up to 10 digits + sign. */
          char text2[11 + 2 + 1];
          text2[0] = NUL;
          snprintf (text2, (size_t) sizeof(text2),
		    "(%ld)", (long) *(int *)value1);
          strcatX (text, text2, 150);
        } else {
          char *text2;
          size_t len = strlen ((char *) value1) + 2 + 1;
          text2 = (char *) cdf_AllocateMemory (len, NULL);
          if (text2 != NULL) {
            text2[0] = NUL;
            snprintf (text2, len, "(%s)", (char *)value1);
            strcatX (text, text2, 150);
            cdf_FreeMemory (text2, NULL);
          }
        }
      }
    }
    printf ("ERROR...%s\n", text);
  }
  return CV2C;
}

/******************************************************************************
* ValidateCDF.
*   Start to validate each internal record in a CDF file.
******************************************************************************/

STATICforIDL CDFstatus ValidateCDF (struct CDFstruct *CDF, vFILE *CDFfp, 
                                    Int32 offset, Int32 fileSize,
                                    Logical debug)
{
  Int32 recordSize;
  Int32 recordType;
  CDFstatus status;

  status = CDF_OK;
  /****************************************************************************
  * Read internal records from the beginning of the file until EOF (or illegal
  * record) reached.
  ****************************************************************************/
  if (!SEEKv(CDFfp,offset,vSEEK_SET)) return CRE;
  for (;;) {
     /*************************************************************************
     * Read record size.
     *************************************************************************/
     offset = V_tell (CDFfp);
     if (!CDF->badEOF && offset == fileSize) break;
     if (!Read32(CDFfp,&recordSize)) {
       if (!CDF->badEOF) return CRE;
       else break;
     }
     if (recordSize < 1 || (!CDF->badEOF && recordSize > fileSize)) 
       return QuitCDF ("CDF: an invalid internal record size ", offset,
                       4, 1, &recordSize, 0, debug);
     /*************************************************************************
     * Read record type.
     *************************************************************************/
     if (!Read32(CDFfp,&recordType)) return CRE;
     /*************************************************************************
     * Based on the record type...
     *************************************************************************/
     switch ((int)recordType) {
        /**********************************************************************
        * Compressed CDF Record (CCR).
        **********************************************************************/
        case CCR_: {
/*
          status = ValidateCCR (CDFfp, offset, debug);
          if (status != CDF_OK) return status;
*/
          offset = offset + recordSize;
          if (!SEEKv(CDFfp,offset,vSEEK_SET)) return CRE;
	  break;
        }
        /**********************************************************************
        * CDF Descriptor Record (CDR).
        **********************************************************************/
        case CDR_: {
          numAttrs = rVars = zVars = 0;
          status = ValidateCDR (CDFfp, offset, debug);
          if (status != CDF_OK) return status;
	  status = ValidateGDR (CDFfp, CDR.GDRoffset, debug);
	  if (status != CDF_OK) return status;
          offset = offset + recordSize;
          if (!SEEKv(CDFfp,offset,vSEEK_SET)) return CRE;
	  break;
        }
        /**********************************************************************
        * Global Descriptor Record (GDR).
        **********************************************************************/
        case GDR_: {
          offset = offset + recordSize;
          if (!SEEKv(CDFfp,offset,vSEEK_SET)) return CRE;
	  break;
        }
        /**********************************************************************
        * Attribute Descriptor Record (ADR).
        **********************************************************************/
        case ADR_: {
          ++numAttrs;
          offset = offset + recordSize;
          if (!SEEKv(CDFfp,offset,vSEEK_SET)) return CRE;
	  break;
        }
        /**********************************************************************
        * Attribute Entry Descriptor Record (AgrEDR or AzEDR).
        **********************************************************************/
        case AgrEDR_:
        case AzEDR_: {
          offset = offset + recordSize;
          if (!SEEKv(CDFfp,offset,vSEEK_SET)) return CRE;
	  break;
        }
        /**********************************************************************
        * Variable Descriptor Record (rVDR or zVDR).
        **********************************************************************/
        case rVDR_:
        case zVDR_: {
          Logical zVar = (recordType == zVDR_);
          if (zVar)
            ++zVars;
          else
            ++rVars;
          offset = offset + recordSize;
          if (!SEEKv(CDFfp,offset,vSEEK_SET)) return CRE;
	  break;
        }
        /**********************************************************************
        * Variable indeX Record (VXR).
        **********************************************************************/
        case VXR_: {
          offset = offset + recordSize;
          if (!SEEKv(CDFfp,offset,vSEEK_SET)) return CRE;
	  break;
        }
        /**********************************************************************
        * Variable Values Record (VVR).
        **********************************************************************/
        case VVR_: {
          status = ValidateVVR (CDFfp, offset, debug);
          if (recordSize < VVR_BASE_SIZE) 
            return QuitCDF ("CDF(VVR): record size is invalid ", offset,
                            4, 1, &recordSize, 0, debug);
          offset = offset + recordSize;
          if (!SEEKv(CDFfp,offset,vSEEK_SET)) return CRE;
	  break;
        }
        /**********************************************************************
        * Compressed Variable Values Record (CVVR).
        **********************************************************************/
        case CVVR_: {
          status = ValidateCVVR (CDFfp, offset, debug);
          if (status != CDF_OK) return status;
          offset = offset + recordSize;
          if (!SEEKv(CDFfp,offset,vSEEK_SET)) return CRE;
	  break;
        }
        /**********************************************************************
        * Compressed Parameters Record (CPR).
        **********************************************************************/
        case CPR_: {
          offset = offset + recordSize;
          if (!SEEKv(CDFfp,offset,vSEEK_SET)) return CRE;
	  break;
        }
        /**********************************************************************
        * Sparseness Parameters Record (SPR).
        **********************************************************************/
        case SPR_: {
/*
          status = ValidateSPR (CDFfp, offset, debug);
          if (status != CDF_OK) return status;
*/
          offset = offset + recordSize;
          if (!SEEKv(CDFfp,offset,vSEEK_SET)) return CRE;
	  break;
        }
        /**********************************************************************
        * Unused internal record
        **********************************************************************/
        case UIR_: {
          offset = offset + recordSize;
          if (!SEEKv(CDFfp,offset,vSEEK_SET)) return CRE;
	  break;
        }
        /**********************************************************************
        * Illegal record type.
        **********************************************************************/
        default: {
	    return QuitCDF ("CDF: an invalid internal record type", offset,
                            4, 1, &recordType, 0, debug);
        }
     }
  }
  /****************************************************************************
  * Check attribute links and their attribute entries.
  ****************************************************************************/
  if (GDR.NumAttr != numAttrs)
    return QuitCDF ("CDF: number of attributes does not match: ", (Int32) -1,
                    4, 2, &GDR.NumAttr, &numAttrs, debug);
  if (numAttrs > 0) {
    status = ValidateAttributeLinks(CDF, CDFfp, debug);
    if (status != CDF_OK) return status;
  }
  /****************************************************************************
  * Check rVariable links and their data.
  ****************************************************************************/
  if (GDR.NrVars != rVars)
    return QuitCDF ("CDF: number of rVariables does not match: ", (Int32) -1,
                    4, 2, &GDR.NrVars, &rVars, debug);
  if (rVars > 0) {
    status = ValidateVariableLinks(CDF, CDFfp, FALSE, debug);
    if (status != CDF_OK) return status;
  }
  /****************************************************************************
  * Check zVariable links and their data.
  ****************************************************************************/
  if (GDR.NzVars != zVars)
    return QuitCDF ("CDF: number of zVariables does not match: ", (Int32) -1,
                    4, 2, &GDR.NzVars, &zVars, debug);
  if (zVars > 0) {
    status = ValidateVariableLinks(CDF, CDFfp, TRUE, debug);
    if (status != CDF_OK) return status;
  }
  return CDF_OK;
}

/******************************************************************************
* ValidateCDR.
******************************************************************************/

static CDFstatus ValidateCDR (vFILE *fp, Int32 offset, Logical debug)
{
  Int32 actualEncoding;
/*
  char copyRight[CDF_COPYRIGHT_LEN+1];
*/
  CDFstatus status;
 
  if (debug)
    printf("  Checking CDR...@%d\n", (int) offset);
  status = ReadCDR (fp, offset, 
                    CDR_RECORD, &CDR, NULL,
                    CDR_NULL);
  if (status != CDF_OK) return status;
  if (CDR.RecordType != CDR_)
    return QuitCDF ("CDF(CDR): record type is invalid ", offset,
                    4, 1, &(CDR.RecordType), 0, debug);
  if (CDR.RecordSize < CDR_BASE_SIZE ||
      (!PriorTo("2.5",CDR.Version,CDR.Release,CDR.Increment) && 
       CDR.RecordSize > CDR_MAX_SIZE))
    return QuitCDF ("CDF(CDR): record size is invalid ", offset,
                    4, 1, &(CDR.RecordSize), 0, debug);
  if (CDR.GDRoffset < 1) 
    return QuitCDF ("CDF(CDR): offset to GDR is invalid ", offset,
                    4, 1, &(CDR.GDRoffset), 0, debug);
  if (CDR.Version != 2)
    return QuitCDF ("CDF: version number is invalid ", offset,
                    4, 1, &(CDR.Version), 0, debug);
  if (CDR.Release < 0)
    return QuitCDF ("CDF: release number is invalid ", offset,
                    4, 1, &(CDR.Release), 0, debug);
  if (CDR.Increment < 0)
    return QuitCDF ("CDF: increment number is invalid ", offset,
                    4, 1, &(CDR.Increment), 0, debug);
  if (!ValidEncoding(CDR.Encoding,&actualEncoding)) 
    return QuitCDF ("CDF(CDR): encoding is invalid ", offset,
                    4, 1, &(CDR.Encoding), 0, debug);
/*
  for (i = 0; i < strlen(copyRight); ++i) 
    if (!Printable(copyRight[i])) 
      return QuitCDF ("CDF(CDR): copyright is invalid ",  offset,
                      0, copyRight), 0, debug);
*/
  return CDF_OK;
}

/******************************************************************************
* ValidateGDR.
******************************************************************************/

static CDFstatus ValidateGDR (vFILE *fp, Int32 offset, Logical debug)
{
  int i;
  CDFstatus status;

  if (debug)
    printf("  Checking GDR...@%d\n", (int) offset);
  status = ReadGDR (fp, offset, 
                    GDR_RECORD, &GDR,
                    GDR_NULL);
  if (status != CDF_OK) return status;
  if (GDR.RecordType != GDR_)
    return QuitCDF ("CDF(GDR): record type is invalid ", offset,
                    4, 1, &(GDR.RecordType), 0, debug);
  if (GDR.RecordSize < GDR_BASE_SIZE ||
      GDR.RecordSize > GDR_MAX_SIZE) 
    return QuitCDF ("CDF(GDR): record size is invalid ", offset,
                    4, 1, &(GDR.RecordSize), 0, debug);
  if (GDR.NrVars < 0 || (GDR.NrVars > 0 && GDR.rVDRhead < 1)) 
    return QuitCDF ("CDF(GDR): NrVars or their link is invalid ", offset,
                    4, 1, &(GDR.NrVars), 0, debug);
  if (GDR.NumAttr < 0 || (GDR.NumAttr > 0 && GDR.ADRhead < 1)) 
    return QuitCDF ("CDF(GDR): attribute numbers or their link is invalid ", offset,
                    4, 1, &(GDR.NumAttr), 0, debug);
  if (GDR.rMaxRec < -1) 
    return QuitCDF ("CDF(GDR): max rVars record is invalid ", offset,
                    4, 1, &(GDR.rMaxRec), 0, debug);
  if (GDR.rNumDims < 0 ||
      GDR.rNumDims > CDF_MAX_DIMS) 
    return QuitCDF ("CDF(GDR): number of dimensions for rVars is invalid ", offset,
                    4, 1, &(GDR.rNumDims), 0, debug);
  if (GDR.NzVars < 0 || (GDR.NzVars > 0 && GDR.zVDRhead < 1)) 
    return QuitCDF ("CDF(GDR): NzVars or their link is invalid ", offset,
                    4, 1, &(GDR.NzVars), 0, debug);
  if (GDR.UIRhead < 0) 
    return QuitCDF ("CDF(GDR): offset to UIR is invalid ", offset,
                    4, 1, &(GDR.UIRhead), 0, debug);
  for (i = 0; i < (int)GDR.rNumDims; ++i)
    if (GDR.rDimSizes[i] < 1) 
    return QuitCDF ("CDF(GDR): dimensional size is invalid ", offset,
                    4, 1, &(GDR.rDimSizes[i]), 0, debug);
  return CDF_OK;
}

/******************************************************************************
* ValidateADR.
******************************************************************************/

static CDFstatus ValidateADR (struct CDFstruct *CDF, vFILE *fp, Int32 offset, 
                              Logical debug)
{
  struct ADRstruct ADR;
  CDFstatus status;

  if (debug)
    printf("  Checking ADR...@%d\n", (int) offset);
  status = ReadADR (fp, offset, 
                    ADR_RECORD, &ADR,
                    ADR_NULL);
  if (status != CDF_OK) return status;
  if (ADR.RecordType != ADR_)
    return QuitCDF ("CDF(ADR): record type is invalid ", offset,
                    4, 1, &(ADR.RecordType), 0, debug);

  if (ADR.RecordSize < ADR_BASE_SIZE ||
      ADR.RecordSize > ADR_MAX_SIZE) 
    return QuitCDF ("CDF(ADR): record size is invalid ", offset,
                    4, 1, &(ADR.RecordSize), 0, debug);
  if (ADR.ADRnext < 0 || ((ADR.Num < (GDR.NumAttr-1))  && ADR.ADRnext == 0)) 
    return QuitCDF ("CDF(ADR): offset to next ADR is invalid ", offset,
                    4, 1, &(ADR.ADRnext), 0, debug);
  if (ADR.AgrEDRhead < 0 || (ADR.NgrEntries > 0 && ADR.AgrEDRhead == 0)) 
    return QuitCDF ("CDF(ADR): record size is invalid ", offset,
                    4, 1, &(ADR.AgrEDRhead), 0, debug);
/*
  if ((ADR.Scope == GLOBAL_SCOPE) && (ADR.NzEntries > 0))
    return QuitCDF ("CDF(ADR): attribute zEntries exist for a global attribute ", offset,
                    4, 1, &(ADR.NzEntries), 0, debug);
*/
  if (!ValidAttrScope(ADR.Scope)) 
    return QuitCDF ("CDF(ADR): scope is invalid ", offset,
                    4, 1, &(ADR.Scope), 0, debug);
  if (ADR.Num < 0 || ADR.Num > GDR.NumAttr) 
    return QuitCDF ("CDF(ADR): attribute number is invalid ", offset,
                    4, 2, &(ADR.Num), &(GDR.NumAttr), debug);
  if (ADR.NgrEntries < 0) 
    return QuitCDF ("CDF(ADR): number of g/rEntries is invalid ", offset,
                    4, 1, &(ADR.NgrEntries), 0, debug);
/*
  if ((ADR.Scope == VARIABLE_SCOPE) && (ADR.NgrEntries > CDF->NrVars)) 
    return QuitCDF ("CDF(ADR): number of rEntries is invalid ", offset,
                    4, 1, &(ADR.NgrEntries), 0, debug);
*/
  if (ADR.MAXgrEntry < -1) 
    return QuitCDF ("CDF(ADR): max g/rEntry is invalid ", offset,
                    4, 1, &(ADR.MAXgrEntry), 0, debug);
  if ((ADR.Scope == VARIABLE_SCOPE) && (ADR.MAXgrEntry < (ADR.NgrEntries-1)))
    return QuitCDF ("CDF(ADR): max rEntry is invalid ", offset,
                    4, 2, &(ADR.MAXgrEntry), &(ADR.NgrEntries), debug);
  if (ADR.AzEDRhead < 0 || (ADR.NzEntries > 0 && ADR.AzEDRhead == 0)) 
    return QuitCDF ("CDF(ADR): offset to next AzEDR is invalid ", offset,
                    4, 1, &(ADR.AzEDRhead), 0, debug);
/*
  if (ADR.NzEntries < 0 || ((ADR.Scope == VARIABLE_SCOPE) && 
                            (ADR.NzEntries > CDF->NzVars)))
    return QuitCDF ("CDF(ADR): number of zEntries is invalid ", offset,
                    4, 2, &(ADR.NzEntries), &(CDF->NzVars), debug);
  if (ADR.MAXzEntry < -1 || ((ADR.Scope == VARIABLE_SCOPE) &&
                             (ADR.MAXzEntry > CDF->NzVars)))
    return QuitCDF ("CDF(ADR): max zEntry is invalid ", offset,
                    4, 2, &(ADR.MAXzEntry), &(CDF->NzVars), debug);
*/
  if ((ADR.Scope == VARIABLE_SCOPE) && (ADR.MAXzEntry < (ADR.NzEntries-1)))
    return QuitCDF ("CDF(ADR): max zEntry is invalid ", offset,
                    4, 2, &(ADR.MAXzEntry), &(ADR.NzEntries), debug);
  if (!ValidAttrName(ADR.Name)) 
    return QuitCDF ("CDF(ADR): attribute name is invalid ",  offset,
                    0, 1, ADR.Name, 0, debug);
  return CDF_OK;
}

/******************************************************************************
* ValidateAEDR/AzEDR.
******************************************************************************/

static CDFstatus ValidateAEDR (struct CDFstruct *CDF, vFILE *fp, Int32 offset,
                               Int32 num, Int32 maxEntry, Logical zEntry, 
                               Logical debug)
{
  struct AEDRstruct AEDR;
  size_t nBytes;
  CDFstatus status;

  if (debug)
    printf("  Checking AEDR...@%d\n", (int) offset);
  status = ReadAEDR (fp, offset, 
                     AEDR_RECORD, &AEDR, NULL,
                     AEDR_NULL);
  if (status != CDF_OK) return status;
  if ((!zEntry && AEDR.RecordType != AgrEDR_) ||
      (zEntry && AEDR.RecordType != AzEDR_))
    return QuitCDF ("CDF(AEDR): record type is invalid ", offset,
                    4, 1, &(AEDR.RecordType), 0, debug);
  if (AEDR.RecordSize < AEDR_BASE_SIZE) 
    return QuitCDF ("CDF(AEDR): record size is invalid ", offset,
                    4, 1, &(AEDR.RecordSize), 0, debug);
  if (AEDR.AEDRnext < 0) 
    return QuitCDF ("CDF(AEDR): offset to next AEDR is invalid ", offset,
                    4, 1, &(AEDR.AEDRnext), 0, debug);
/*  Check for "AEDR.AttrNum != num" is not included as some CDFs fail this
    check. Don't know why.... */ 
/*  if (AEDR.AttrNum < 0 || AEDR.AttrNum > GDR.NumAttr || AEDR.AttrNum != num)  */
/*
  if (AEDR.AttrNum < 0 || AEDR.AttrNum > GDR.NumAttr) 
    return QuitCDF ("CDF(AEDR): attribute number is invalid ", offset,
                    4, 2, &(AEDR.AttrNum), &(GDR.NumAttr), debug);
*/
  if (!ValidDataType(AEDR.DataType) || InValidDataType(AEDR.DataType))
    return QuitCDF ("CDF(AEDR): data type is invalid ", offset,
                    4, 1, &(AEDR.DataType), 0, debug);
  if (AEDR.Num < 0 || AEDR.Num > maxEntry)
    return QuitCDF ("CDF(AEDR): entry number is invalid ", offset,
                    4, 2, &(AEDR.Num), &maxEntry, debug);
  if (AEDR.AEDRnext < 0)
    return QuitCDF ("CDF(AEDR): next AEDR offset is invalid : ", offset,
                    4, 1, &(AEDR.AEDRnext), 0, debug);
/*  
  if (zEntry && AEDR.EntryNum > GDR->NzVars)
    return QuitCDF ("CDF(AEDR): entry number is invalid ", offset,
                    4, 2, &(AEDR.Num), &(GDR->NzVars), debug);
*/
  if (AEDR.NumElems < 1 || 
      (STRINGdataType(AEDR.DataType) && 
       AEDR.NumElems > (AEDR.RecordSize - AEDR_VALUE_OFFSET))) 
    return QuitCDF ("CDF(AEDR): number of elements is invalid ", offset,
                    4, 1, &(AEDR.NumElems), 0, debug);
  nBytes = (size_t) (CDFelemSize(AEDR.DataType) * AEDR.NumElems);
  if (nBytes < 1 || 
      nBytes > (AEDR.RecordSize - AEDR_VALUE_OFFSET)) 
    return QuitCDF ("CDF(AEDR): entry value size is invalid ", offset,
                    4, 1, &nBytes, 0, debug);	
  if (AEDR.RecordSize > (AEDR_MAX_SIZE + nBytes))
    return QuitCDF ("CDF(AEDR): record size is invalid ", offset,
                    4, 1, &(AEDR.RecordSize), 0, debug);
  return CDF_OK;
}

/******************************************************************************
* ValidateVDR/zVDR.
******************************************************************************/

static CDFstatus ValidateVDR (struct CDFstruct *CDF, vFILE *fp, Int32 offset, 
		              Logical zVar, Logical debug)
{
  struct VDRstruct VDR;
  Int32 numVars;
  int ix;
  CDFstatus status;
  size_t nBytes;
  
  if (debug)
    printf("  Checking VDR...@%d\n", (int) offset);
  status = ReadVDR (CDF, fp, offset, zVar, 
                    VDR_RECORD, &VDR, NULL,
                    VDR_NULL);
  if (status != CDF_OK) return status;
  if ((!zVar && VDR.RecordType != rVDR_) || (zVar && VDR.RecordType != zVDR_))
    return QuitCDF ("CDF(VDR): record type is invalid ", offset,
                    4, 1, &(VDR.RecordType), 0, debug);
  if (VDR.RecordSize < (zVar ? zVDR_BASE_SIZE : rVDR_BASE_SIZE)) 
    return QuitCDF ("CDF(VDR): record size is invalid ", offset,
                    4, 1, &(VDR.RecordSize), 0, debug);
  if (!ValidDataType(VDR.DataType) || InValidDataType(VDR.DataType))
    return QuitCDF ("CDF(VDR): data type is invalid ", offset,
                    4, 1, &(VDR.DataType), 0, debug);
  if (VDR.MaxRec < -1) 
    return QuitCDF ("CDF(VDR): max record is invalid ", offset,
                    4, 1, &(VDR.MaxRec), 0, debug);
  if (VDR.NumElems < 1 || (!STRINGdataType(VDR.DataType) && VDR.NumElems != 1))
    return QuitCDF ("CDF(VDR): number of elements is invalid ", offset,
                    4, 1, &(VDR.NumElems), 0, debug);
  if (zVar) numVars = CDF->NzVars;
  else numVars = CDF->NrVars;
  if (VDR.Num < 0 || VDR.Num > numVars)
    return QuitCDF ("CDF(VDR): variable number is invalid ", offset,
                    4, 2, &(VDR.Num), &numVars, debug);
  if ((VDR.Num < (numVars-1)) && VDR.VDRnext < 1) 
    return QuitCDF ("CDF(VDR): offset to next VDR is invalid ", offset,
                    4, 1, &(VDR.VDRnext), 0, debug);
  if (VARcompressionBITset(VDR.Flags) && VDR.CPRorSPRoffset <= (int) NO_OFFSET) 
    return QuitCDF ("CDF(VDR): offset to CPRorSPR is invalid ", offset,
                    4, 1, &(VDR.CPRorSPRoffset), 0, debug);
  if (VDR.blockingFactor < 0) 
    return QuitCDF ("CDF(VDR): blocking factor is invalid ", offset,
                    4, 1, &(VDR.blockingFactor), 0, debug);
  if (!ValidVarName(VDR.Name)) 
    return QuitCDF ("CDF(VDR): variable name is invalid ",  offset,
                    0, 1, VDR.Name, 0, debug);
  if (zVar) {
    if (VDR.zNumDims < 0 || VDR.zNumDims > CDF_MAX_DIMS) 
    return QuitCDF ("CDF(VDR): number of dimensions is invalid ", offset,
                    4, 1, &(VDR.zNumDims), 0, debug);
    for (ix = 0; ix < (int)VDR.zNumDims; ++ix)
      if (VDR.zDimSizes[ix] < 1) 
    return QuitCDF ("CDF(VDR): dimensional size is invalid ", offset,
                    4, 1, &(VDR.zDimSizes[ix]), 0, debug);
  }
  nBytes = (size_t) (CDFelemSize(VDR.DataType)*VDR.NumElems);
  if (nBytes < 1)
    return QuitCDF ("CDF(VDR): pad value size is invalid ", offset,
                    4, 1, &nBytes, 0, debug);
  if (VDR.RecordSize > ((zVar ? zVDR_MAX_SIZE : rVDR_MAX_SIZE) + nBytes +
                         (CDF->wastedSpace ? VDR_WASTED_SIZE : 0)))
    return QuitCDF ("CDF(VDR): record size is invalid ", offset,
                    4, 1, &(VDR.RecordSize), 0, debug);
  return CDF_OK;
}

/******************************************************************************
* ValidateVXR.
******************************************************************************/

static CDFstatus ValidateVXR (vFILE *fp, Int32 offset, Int32 lastRec,
	                      Logical debug)
{
  struct VXRstruct VXR;
  int i;
  Int32 irType;
  CDFstatus status;

  if (debug)
    printf("  Checking VXR...@%d\n", (int) offset);
  status = ReadVXR (fp, offset, 
                    VXR_RECORD, &VXR,
                    VXR_NULL);
  if (status != CDF_OK) return status;
  if (VXR.RecordType != VXR_)
    return QuitCDF ("CDF(VXR): record type is invalid ", offset,
                    4, 1, &(VXR.RecordType), 0, debug);
  if (VXR.RecordSize != (VXR_FIRSTREC_OFFSET + 12 * VXR.Nentries))
    return QuitCDF ("CDF(VXR): record size is invalid ", offset,
                    4, 1, &(VXR.RecordSize), 0, debug);
  if (VXR.Nentries < 0 || VXR.Nentries > MAX_VXR_ENTRIES) 
    return QuitCDF ("CDF(VXR): number of entries is invalid ", offset,
                    4, 1, &(VXR.Nentries), 0, debug); 
  if (VXR.NusedEntries < 0 || VXR.NusedEntries > VXR.Nentries) 
    return QuitCDF ("CDF(VXR): number of used entries is invalid ", offset,
                    4, 2, &(VXR.NusedEntries), &(VXR.Nentries), debug);
  if (VXR.VXRnext > 0) {
    if (VXR.Last[VXR.NusedEntries-1] > lastRec)
      return QuitCDF ("CDF(VXR): a variable last record does not match in a Variable Index Record: ", offset,
                      4, 2, &(VXR.Last[VXR.NusedEntries-1]), &lastRec, debug);
    status = ValidateVXR (fp, VXR.VXRnext, lastRec, debug); 
    if (status != CDF_OK) return status;
  } else if (VXR.VXRnext < 0) {
      return QuitCDF ("CDF(VXR): a link offset to next record is negative in a Variable Index Record: ", offset,
                      4, 1, &(VXR.VXRnext), 0, debug);
  }

  for (i = 0; i < VXR.NusedEntries; ++i) {
    if (VXR.First[i] < 0 || VXR.Last[i] < 0 || VXR.First[i] > VXR.Last[i])
      return QuitCDF ("CDF(VXR): entry value for first/last is invalid ", offset,
                      4, 2, &(VXR.First[i]), &(VXR.Last[i]), debug);
    if (VXR.Offset[i] < 1) 
      return QuitCDF ("CDF(VXR): entry value for offset is invalid ", offset,
                      4, 1, &(VXR.Offset[i]), 0, debug);

    status = ReadIrType (fp, VXR.Offset[i], &irType);
    if (status != CDF_OK) return status;
    if (irType != VXR_ && irType != VVR_ && irType != CVVR_)
      return QuitCDF ("CDF(VXR): entry value for offset is invalid ", offset,
                      4, 1, &(VXR.Offset[i]), 0, debug);
    if (irType == VXR_ && VXR.Offset[i] != offset) {
      status = ValidateVXR (fp, VXR.Offset[i], lastRec, debug);
      if (status != CDF_OK) return status;
    } 
  }
  return CDF_OK;
}

/******************************************************************************
* ValidateVVR.
******************************************************************************/

static CDFstatus ValidateVVR (vFILE *fp, Int32 offset, Logical debug)
{
  struct VVRstruct VVR;
  CDFstatus status;

  if (debug)
    printf("  Checking VVR...@%d\n", (int) offset);
  status = ReadVVR (fp, offset, 
                    VVR_RECORDx, &VVR,
                    VVR_NULL);
  if (status != CDF_OK) return status;
  if (VVR.RecordType != VVR_)
    return QuitCDF ("CDF(VVR): record type is invalid ", offset,
                    4, 1, &(VVR.RecordType), 0, debug);
  if (VVR.RecordSize < VVR_BASE_SIZE) 
    return QuitCDF ("CDF(VVR): record size is invalid for Variable Value Record", offset,
                    4, 1, &(VVR.RecordSize), 0, debug);
  return CDF_OK;
}

/******************************************************************************
* ValidateUIR.
******************************************************************************/

static CDFstatus ValidateUIR (vFILE *fp, Int32 offset, Logical debug)
{
  struct UIRstruct UIR;
  CDFstatus status;

  if (debug)
    printf("  Checking UIR...@%d\n", (int) offset);
  status = ReadUIR (fp, offset, 
                    UIR_RECORD, &UIR,
                    UIR_NULL);  
  if (status != CDF_OK) return status;
  if (UIR.RecordType != UIR_)
    return QuitCDF ("CDF(UIR): record type is invalid ", offset,
                    4, 1, &(UIR.RecordType), 0, debug);
  if (UIR.RecordSize < UIR_BASE_SIZE) 
    return QuitCDF ("CDF(UIR): record size is invalid for Unused Internal Record", offset,
                    4, 1, &(UIR.RecordSize), 0, debug);
  if (UIR.NextUIR < 0) 
    return QuitCDF ("CDF(UIR): offset to next UIR is invalid for Unused Internal Record", offset,
                    4, 1, &(UIR.NextUIR), 0, debug);
  if (UIR.PrevUIR < 0) 
    return QuitCDF ("CDF(UIR): offset to previous UIR is invalid for Unused Internal Record", offset,
                    4, 1, &(UIR.PrevUIR), 0, debug);
  return CDF_OK;
}

/******************************************************************************
* ValidateCCR.
******************************************************************************/

static CDFstatus ValidateCCR (vFILE *fp, Int32 offset, Logical debug)
{
  struct CCRstruct CCR;
  CDFstatus status;

  if (debug)
    printf("  Checking CCR...@%d\n", (int) offset);
  status = ReadCCR (fp, offset, 
                    CCR_RECORD, &CCR,
                    CCR_NULL);
  if (status != CDF_OK) return status;
  if (CCR.RecordType != CCR_)
    return QuitCDF ("CDF(CCR): record type is invalid ", offset,
                    4, 1, &(CCR.RecordType), 0, debug);
  if (CCR.RecordSize < CCR_BASE_SIZE) 
    return QuitCDF ("CDF(CCR): record size is invalid ", offset,
                    4, 1, &(CCR.RecordSize), 0, debug);
  if (CCR.uSize < 0) 
    return QuitCDF ("CDF(CCR): uncompressed file size is invalid ", offset,
                    4, 1, &(CCR.uSize), 0, debug);
  if (CCR.CPRoffset < 0) 
    return QuitCDF ("CDF(CCR): offset to CPR is invalid ", offset,
                    4, 1, &(CCR.CPRoffset), 0, debug);
  return CDF_OK;
}

/******************************************************************************
* ValidateCPR.
******************************************************************************/

static CDFstatus ValidateCPR (vFILE *fp, Int32 offset, Logical debug)
{
  struct CPRstruct CPR;
  CDFstatus status;
  long cType, cParms[1];

  if (debug)
    printf("  Checking CPR...@%d\n", (int) offset);
  status = ReadCPR (fp, offset, 
                    CPR_RECORD, &CPR,
                    CPR_NULL);
  if (status != CDF_OK) return status;
  if (CPR.RecordType != CPR_)
    return QuitCDF ("CDF(CPR): record type is invalid ", offset,
                    4, 1, &(CPR.RecordType), 0, debug);
  if (CPR.RecordSize != (CPR_BASE_SIZE + CPR.pCount * sizeof(Int32)))
    return QuitCDF ("CDF(CPR): reocrd size is invalid ", offset,
                    4, 1, &(CPR.RecordSize), 0, debug);
  if (CPR.pCount != 1) 
    return QuitCDF ("CDF(CPR): compression parameter count is invalid ", offset,
                    4, 1, &(CPR.pCount), 0, debug);
  cType = (long) CPR.cType;
  cParms[0] = (long) CPR.cParms[0];
  if (ValidateCompression(cType, cParms) != CDF_OK)        
    return QuitCDF ("CDF(CPR): compression parameter is invalid ", offset,
                    4, 1, &(CPR.cType), 0, debug);
  return CDF_OK;
}

/******************************************************************************
* ValidateSPR.
******************************************************************************/

static CDFstatus ValidateSPR (vFILE *fp, Int32 offset, Logical debug)
{
  struct SPRstruct SPR;
  CDFstatus status;

  if (debug)
    printf("  Checking SPR...@%d\n", (int) offset);
  status = ReadSPR (fp, offset, 
                    SPR_RECORD, &SPR,
                    SPR_NULL);
  if (status != CDF_OK) return status;
  if (SPR.RecordType != SPR_)
    return QuitCDF ("CDF(SPR): record type is invalid ", offset,
                    4, 1, &(SPR.RecordType), 0, debug);
  if (SPR.RecordSize != (SPR_BASE_SIZE + SPR.pCount * sizeof(Int32)))
    return QuitCDF ("CDF(SPR): record size is invalid ", offset,
                    4, 1, &(SPR.RecordSize), 0, debug);
  if (SPR.pCount < 1 || SPR.pCount > CDF_MAX_PARMS) 
    return QuitCDF ("CDF(SPR): sparseness parameter count is invalid ", offset,
                    4, 1, &(SPR.pCount), 0, debug);
  return CDF_OK;
}

/******************************************************************************
* ValidateCVVR.
******************************************************************************/

static CDFstatus ValidateCVVR (vFILE *fp, Int32 offset, Logical debug)
{
  struct CVVRstruct CVVR;
  CDFstatus status;

  if (debug)
    printf("  Checking CVVR...@%d\n", (int) offset);
  status = ReadCVVR (fp, offset, 
                     CVVR_RECORDx, &CVVR,
                     CVVR_NULL);
  if (status != CDF_OK) return status;
  if (CVVR.RecordSize < CVVR_BASE_SIZE) 
    return QuitCDF ("CDF(CVVR): record size is invalid ", offset,
                    4, 1, &(CVVR.RecordSize), 0, debug);
  if (CVVR.cSize < 0) 
    return QuitCDF ("CDF(CVVR): uncompressed records size is invalid ", offset,
                    4, 1, &(CVVR.cSize), 0, debug);
  return CDF_OK;
}

/******************************************************************************
* ValidateAttributeLinks.
******************************************************************************/

static CDFstatus ValidateAttributeLinks (struct CDFstruct *CDF, vFILE *fp, 
                                         Logical debug)
{
  CDFstatus status;
  Int32 offset, nextADR, AgrEDRhead, AzEDRhead;
  Int32 num, numgrEntries, maxgrEntry, numzEntries, maxzEntry;
  int ix, *visit;

  visit = (int *) cdf_AllocateMemory(GDR.NumAttr * sizeof(Int32), NULL);
  for (ix = 0; ix < GDR.NumAttr; ++ix) visit[ix] = 0;
  offset = GDR.ADRhead;
  for (ix = 0; ix < GDR.NumAttr; ++ix) {
    status = ValidateADR (CDF, fp, offset, debug);
    if (status != CDF_OK) {
      cdf_FreeMemory (visit, NULL);
      return status;
    }
    status = ReadADR (fp, offset,
                      ADR_NUM, &num,
                      ADR_ADRNEXT, &nextADR,
                      ADR_AgrEDRHEAD, &AgrEDRhead,
                      ADR_NgrENTRIES, &numgrEntries,
                      ADR_MAXgrENTRY, &maxgrEntry,
                      ADR_AzEDRHEAD, &AzEDRhead,
                      ADR_NzENTRIES, &numzEntries,
                      ADR_MAXzENTRY, &maxzEntry,
                      ADR_NULL);
    if (numgrEntries > 0)
      status = ValidateAttributeEntryLink (CDF, fp, num, FALSE, AgrEDRhead, 
                                           numgrEntries, maxgrEntry, debug);
    if (status != CDF_OK) {
      cdf_FreeMemory (visit, NULL);
      return status;
    }
    if (numzEntries > 0)
      status = ValidateAttributeEntryLink (CDF, fp, num, TRUE, AzEDRhead, 
                                           numzEntries, maxzEntry, debug);
    if (status != CDF_OK) {
      cdf_FreeMemory (visit, NULL);
      return status;
    }
    visit[num] = 1;
    offset = nextADR;
  }
  for (ix = 0; ix < GDR.NumAttr; ++ix) {
    if (visit[ix] == 0) {
      cdf_FreeMemory (visit, NULL);
      return QuitCDF ("CDF: an attribute unreachable in the attribute link: ", (Int32) -1,
                       4, 1, &ix, 0, debug);
    }
  }
  cdf_FreeMemory (visit, NULL);
  return CDF_OK;
}

/******************************************************************************
* ValidateAttributeEntryLink.
******************************************************************************/

static CDFstatus ValidateAttributeEntryLink (struct CDFstruct *CDF, vFILE *fp, 
                                      Int32 num, Logical zEntry, Int32 EDRhead, 
                                             Int32 numEntries, Int32 maxEntry,
                                             Logical debug)
{
  CDFstatus status;
  Int32 offset, nextAEDR;
  Int32 entryNum, lastNum, *visits;
  int ix, iy;

  offset = EDRhead;
  lastNum = 0;
  visits = (Int32 *) cdf_AllocateMemory ((size_t)numEntries * sizeof(Int32), NULL);
  if (visits == NULL) return BAD_MALLOC;
  for (ix = 0; ix < numEntries; ++ix) visits[ix] = 0;
  ix = 0;
  while (offset != 0) {
    status = ValidateAEDR (CDF, fp, offset, num, maxEntry, zEntry, debug);
    if (status != CDF_OK) {
      cdf_FreeMemory (visits, NULL);
      return status;
    }
    status = ReadAEDR (fp, offset,
                       AEDR_NUM, &entryNum,
                       AEDR_AEDRNEXT, &nextAEDR,
                       AEDR_NULL);
    if (ix > 0) {
      for (iy = 0; iy < ix; ++iy) {
        if (visits[iy] == entryNum) { 
          cdf_FreeMemory (visits, NULL);
          return QuitCDF ("CDF: entry number is repeating in an attribute entry link: ", (Int32) -1,
                          4, 1, &entryNum, 0, debug);
        }
      }
    }
    if (ix == (int) numEntries) {
      cdf_FreeMemory (visits, NULL);
      return QuitCDF ("CDF: number of entries is more than maximum in an attribute entry link: ", (Int32) -1,
                      4, 1, &ix, 0, debug);
    }
    visits[ix] = entryNum;
    ++ix;
    if (lastNum < entryNum) lastNum = entryNum;
    offset = nextAEDR;
  }
  if (lastNum != maxEntry) {
    cdf_FreeMemory (visits, NULL);
    return QuitCDF ("CDF: last entry number is not the maximum entry number in  an attribute entry link: ", (Int32) -1,
                    4, 2, &lastNum, &maxEntry, debug);
  }
  cdf_FreeMemory (visits, NULL);
  return CDF_OK;
}

/******************************************************************************
* ValidateVariableLinks.
******************************************************************************/

static CDFstatus ValidateVariableLinks (struct CDFstruct *CDF, vFILE *fp, 
                                        Logical zVar, Logical debug)
{
  CDFstatus status;
  Int32 offset, headVXR, tailVXR, nextVDR, cprOffset;
  Int32 num, lastRec, flags;
  int ix, numVars, *visit;

  numVars = (zVar ? GDR.NzVars : GDR.NrVars);
  visit = (int *) cdf_AllocateMemory ((size_t)numVars * sizeof(Int32), NULL);
  if (visit == NULL) return BAD_MALLOC;
  for (ix = 0; ix < numVars; ++ix) visit[ix] = 0;
  offset = (zVar ? GDR.zVDRhead : GDR.rVDRhead);
  for (ix = 0; ix < numVars; ++ix) {
    status = ValidateVDR (CDF, fp, offset, zVar, debug);
    if (status != CDF_OK) {
      cdf_FreeMemory (visit, NULL);
      return status;
    }
    status = ReadVDR (CDF, fp, offset, zVar,
                      VDR_NUM, &num,
                      VDR_FLAGS, &flags,
                      VDR_MAXREC, &lastRec,
                      VDR_VDRNEXT, &nextVDR,
                      VDR_CPRorSPR, &cprOffset,
                      VDR_VXRHEAD, &headVXR,
                      VDR_VXRTAIL, &tailVXR,
                      VDR_NULL);
    if (CDF->singleFile && lastRec > -1) {
      status = ValidateVariableValueLinks (CDF, fp, lastRec, headVXR, debug);
      if (status != CDF_OK) {
        cdf_FreeMemory (visit, NULL);
        return status;
      }
    }
/*
    if (CDF->singleFile && (headVXR != tailVXR) && tailVXR > 0) {
      status = ValidateVariableValueLinks (CDF, fp, lastRec, tailVXR, debug);
      if (status != CDF_OK) {
        cdf_FreeMemory (visit, NULL);
        return status;
      }
    }
*/
    if (VARcompressionBITset(flags)) {
      status = ValidateCPR (fp, cprOffset, debug);
      if (status != CDF_OK) {
        cdf_FreeMemory (visit, NULL);
        return status;
      }
    }
    visit[num] = 1;
    offset = nextVDR;
  }
  for (ix = 0; ix < numVars; ++ix) {
    if (visit[ix] == 0) {
      cdf_FreeMemory (visit, NULL);
      return QuitCDF ("CDF: a variable unreachable in variable link: ", (Int32) -1,
                       4, 1, &ix, 0, debug);
    }
  }
  cdf_FreeMemory (visit, NULL);
  if (!CDF->badEOF && offset != 0) {
    status = ReadVDR (CDF, fp, offset, zVar,
                      VDR_NUM, &num,
                      VDR_VDRNEXT, &nextVDR,
                      VDR_NULL);
    if (status == CDF_OK) {
      if (num >= numVars)
        return QuitCDF ("CDF(VDR): a variable unreachable in variable link: ", offset,
                        4, 2, &num, &numVars, debug);
      else
        return QuitCDF ("CDF(VDR): a variable is repeated in variable link: ", offset,
                        4, 2, &num, &numVars, debug);
    }
  }
  return CDF_OK;
}

/******************************************************************************
* ValidateVariableValueLinks.
******************************************************************************/

static CDFstatus ValidateVariableValueLinks (struct CDFstruct *CDF, vFILE *fp, 
                                             Int32 lastRec, Int32 headVXR,
                                             Logical debug)
{
  CDFstatus status;

  status = ValidateVXR (fp, headVXR, lastRec, debug);
  if (status != CDF_OK) return status;

  return CDF_OK;
}

