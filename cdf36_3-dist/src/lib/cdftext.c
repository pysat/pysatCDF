/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF				Get CDF status code explanation text.
*
*  Version 1.3a, 2-Sep-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  21-Sep-92, J Love	Original version.
*   V1.1  12-Jan-94, J Love	CDF V2.4.
*   V1.1a  4-Feb-94, J Love	DEC Alpha/OpenVMS port.
*   V1.2  24-Oct-94, J Love	CDF V2.5.
*   V1.2a 10-Jan-95, J Love	Incremental release in 1995.
*   V1.2b 23-Jan-95, J Love	NSI/DECnet -> DECnet.
*   V1.2c 24-Feb-95, J Love	Solaris 2.3 IDL i/f.
*   V1.3   9-Sep-96, J Love	CDF V2.6.
*   V1.3a  2-Sep-97, J Love	Incremental release in 1997.
*   V1.4  21-Jun-04, M Liu      Added a new error, NOT_A_CDF_OR_NOT_SUPPORTED.
*   V1.5  24-Apr-08, M Liu      Modified the copyright.
*   V1.6  24-Apr-09, M Liu      Modified the copyright.
*
******************************************************************************/

#include "cdflib.h"

/******************************************************************************
* CDFcopyRight.
* `\012' is used to indicate a newline character because MPW C seems to map
* newline characters to carriage returns (`\015').
******************************************************************************/

STATICforIDL void CDFcopyRight (copyRight)
char *copyRight;
{
  strcpyX (copyRight,
"\012\
Common Data Format (CDF)\012\
(C) Copyright 1990-2016 NASA/GSFC\012\
Space Physics Data Facility\012\
NASA/Goddard Space Flight Center\012\
Greenbelt, Maryland 20771 USA\012\
(Internet -- GSFC-CDF-SUPPORT@LISTS.NASA.GOV)\012", CDF_COPYRIGHT_LEN);
  return;
}

/******************************************************************************
* CDFstatusText.
******************************************************************************/

STATICforIDL CDFstatus CDFstatusText (status, textPtr)
CDFstatus status;
char *textPtr;
{
#if INCLUDE_STATUS_TEXT
  switch (status) {

case ATTR_EXISTS:
strcpyX (textPtr,
"ATTR_EXISTS: Named attribute already exists.",
CDF_STATUSTEXT_LEN);
break;

case ATTR_NAME_TRUNC:
strcpyX (textPtr,
"ATTR_NAME_TRUNC: Attribute name truncated.",
CDF_STATUSTEXT_LEN);
break;

case BAD_ALLOCATE_RECS:
strcpyX (textPtr,
"BAD_ALLOCATE_RECS: Illegal number of records to allocate specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_ARGUMENT:
strcpyX (textPtr,
"BAD_ARGUMENT: Illegal/undefined argument specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_ATTR_NAME:
strcpyX (textPtr,
"BAD_ATTR_NAME: Illegal attribute name specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_ATTR_NUM:
strcpyX (textPtr,
"BAD_ATTR_NUM: Illegal attribute number specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_CACHE_SIZE:
strcpyX (textPtr,
"BAD_CACHE_SIZE: Illegal number of cache buffers specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_CDF_EXTENSION:
strcpyX (textPtr,
"BAD_CDF_EXTENSION: Illegal/missing extension for multi-file CDF.",
CDF_STATUSTEXT_LEN);
break;

case BAD_CDF_ID:
strcpyX (textPtr,
"BAD_CDF_ID: CDF identifier is unknown or invalid.",
CDF_STATUSTEXT_LEN);
break;

case BAD_CDF_NAME:
strcpyX (textPtr,
"BAD_CDF_NAME: Illegal CDF name specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_CDFSTATUS:
strcpyX (textPtr,
"BAD_CDFSTATUS: Unknown CDF status code specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_COMPRESSION_PARM:
strcpyX (textPtr,
"BAD_COMPRESSION_PARM: An illegal compression parameter was specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_DATA_TYPE:
strcpyX (textPtr,
"BAD_DATA_TYPE: Unknown data type specified or encountered.",
CDF_STATUSTEXT_LEN);
break;

case BAD_DECODING:
strcpyX (textPtr,
"BAD_DECODING: Unknown or unsupported data decoding specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_DIM_COUNT:
strcpyX (textPtr,
"BAD_DIM_COUNT: Illegal dimension count specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_DIM_INDEX:
strcpyX (textPtr,
"BAD_DIM_INDEX: Dimension index out of range.",
CDF_STATUSTEXT_LEN);
break;

case BAD_DIM_INTERVAL:
strcpyX (textPtr,
"BAD_DIM_INTERVAL: Illegal dimension interval specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_DIM_SIZE:
strcpyX (textPtr,
"BAD_DIM_SIZE: Dimension size specified as zero or less.",
CDF_STATUSTEXT_LEN);
break;

case BAD_ENCODING:
strcpyX (textPtr,
"BAD_ENCODING: Unknown or unsupported data encoding.",
CDF_STATUSTEXT_LEN);
break;

case BAD_ENTRY_NUM:
strcpyX (textPtr,
"BAD_ENTRY_NUM: Illegal attribute entry number specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_BLOCKING_FACTOR:
strcpyX (textPtr,
"BAD_BLOCKING_FACTOR: Illegal blocking factor specified (less than zero).",
CDF_STATUSTEXT_LEN);
break;

case BAD_FNC_OR_ITEM:
strcpyX (textPtr,
"BAD_FNC_OR_ITEM: The specified function or item is illegal.",
CDF_STATUSTEXT_LEN);
break;

case BAD_FORMAT:
strcpyX (textPtr,
"BAD_FORMAT: Unknown CDF format specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_INITIAL_RECS:
strcpyX (textPtr,
"BAD_INITIAL_RECS: Illegal number of initial records.",
CDF_STATUSTEXT_LEN);
break;

case BAD_MAJORITY:
strcpyX (textPtr,
"BAD_MAJORITY: Unknown variable majority specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_MALLOC:
strcpyX (textPtr,
"BAD_MALLOC: Unable to allocate dynamic memory.",
CDF_STATUSTEXT_LEN);
break;

case BAD_NEGtoPOSfp0_MODE:
strcpyX (textPtr,
"BAD_NEGtoPOSfp0_MODE: Illegal negative to positive floating point zero mode.",
CDF_STATUSTEXT_LEN);
break;

case BAD_NUM_DIMS:
strcpyX (textPtr,
"BAD_NUM_DIMS: Illegal number of dimensions.",
CDF_STATUSTEXT_LEN);
break;

case BAD_NUM_ELEMS:
strcpyX (textPtr,
"BAD_NUM_ELEMS: Illegal number of elements (for data type).",
CDF_STATUSTEXT_LEN);
break;

case BAD_NUM_VARS:
strcpyX (textPtr,
"BAD_NUM_VARS: Illegal number of variables specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_READONLY_MODE:
strcpyX (textPtr,
"BAD_READONLY_MODE: Illegal read-only mode specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_REC_COUNT:
strcpyX (textPtr,
"BAD_REC_COUNT: Illegal record count specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_REC_INTERVAL:
strcpyX (textPtr,
"BAD_REC_INTERVAL: Illegal record interval specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_REC_NUM:
strcpyX (textPtr,
"BAD_REC_NUM: Record number is out of range.",
CDF_STATUSTEXT_LEN);
break;

case BAD_SCOPE:
strcpyX (textPtr,
"BAD_SCOPE: Unrecognized attribute scope.",
CDF_STATUSTEXT_LEN);
break;

case BAD_SPARSEARRAYS_PARM:
strcpyX (textPtr,
"BAD_SPARSEARRAYS_PARM: An illegal sparse arrays parameter was specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_SCRATCH_DIR:
strcpyX (textPtr,
"BAD_SCRATCH_DIR: An illegal scratch directory was specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_VAR_NAME:
strcpyX (textPtr,
"BAD_VAR_NAME: Illegal variable name specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_VAR_NUM:
strcpyX (textPtr,
"BAD_VAR_NUM: Illegal variable number specified.",
CDF_STATUSTEXT_LEN);
break;

case BAD_zMODE:
strcpyX (textPtr,
"BAD_zMODE: Illegal zMode specified.",
CDF_STATUSTEXT_LEN);
break;

case CANNOT_ALLOCATE_RECORDS:
strcpyX (textPtr,
"CANNOT_ALLOCATE_RECORDS: Records can't be allocated at this time.",
CDF_STATUSTEXT_LEN);
break;

case CANNOT_CHANGE:
strcpyX (textPtr,
"CANNOT_CHANGE: The parameter/value can't be set/changed.",
CDF_STATUSTEXT_LEN);
break;

case CANNOT_COMPRESS:
strcpyX (textPtr,
"CANNOT_COMPRESS: The compression for the CDF/variable can't be set/modified.",
CDF_STATUSTEXT_LEN);
break;

case CANNOT_SPARSEARRAYS:
strcpyX (textPtr,
"CANNOT_SPARSEARRAYS: Sparse arrays can't be set/modified for the variable.",
CDF_STATUSTEXT_LEN);
break;

case CANNOT_SPARSERECORDS:
strcpyX (textPtr,
"CANNOT_SPARSERECORDS: Sparse records can't be set/modified for the variable.",
CDF_STATUSTEXT_LEN);
break;

case CDF_CLOSE_ERROR:
strcpyX (textPtr,
"CDF_CLOSE_ERROR: Close failed - error from file system.",
CDF_STATUSTEXT_LEN);
break;

case CDF_CREATE_ERROR:
strcpyX (textPtr,
"CDF_CREATE_ERROR: Creation failed - error from file system.",
CDF_STATUSTEXT_LEN);
break;

case CDF_DELETE_ERROR:
strcpyX (textPtr,
"CDF_DELETE_ERROR: Deletion failed - error from file system.",
CDF_STATUSTEXT_LEN);
break;

case CDF_EXISTS:
strcpyX (textPtr,
"CDF_EXISTS: The CDF named already exists.",
CDF_STATUSTEXT_LEN);
break;

case CDF_INTERNAL_ERROR:
strcpyX (textPtr,
"CDF_INTERNAL_ERROR: An internal error has occurred in the CDF library.",
CDF_STATUSTEXT_LEN);
break;

case CDF_NAME_TRUNC:
strcpyX (textPtr,
"CDF_NAME_TRUNC: CDF pathname truncated.",
CDF_STATUSTEXT_LEN);
break;

case CDF_OK:
strcpyX (textPtr,
"CDF_OK: Function completed successfully.",
CDF_STATUSTEXT_LEN);
break;

case CDF_OPEN_ERROR:
strcpyX (textPtr,
"CDF_OPEN_ERROR: Open failed - error from file system.",
CDF_STATUSTEXT_LEN);
break;

case CDF_READ_ERROR:
strcpyX (textPtr,
"CDF_READ_ERROR: Read failed - error from file system.",
CDF_STATUSTEXT_LEN);
break;

case CDF_WRITE_ERROR:
strcpyX (textPtr,
"CDF_WRITE_ERROR: Write failed - error from file system.",
CDF_STATUSTEXT_LEN);
break;

case CORRUPTED_V2_CDF:
strcpyX (textPtr,
"CORRUPTED_V2_CDF: Version 2 CDF is corrupted.",
CDF_STATUSTEXT_LEN);
break;

case CORRUPTED_V3_CDF:
strcpyX (textPtr,
"CORRUPTED_V3_CDF: Version 3 CDF is corrupted.",
CDF_STATUSTEXT_LEN);
break;

case DECOMPRESSION_ERROR:
strcpyX (textPtr,
"DECOMPRESSION_ERROR: An error was detected in the compressed data.",
CDF_STATUSTEXT_LEN);
break;

case DID_NOT_COMPRESS:
strcpyX (textPtr,
"DID_NOT_COMPRESS: The CDF or variable values did not compress.",
CDF_STATUSTEXT_LEN);
break;

case EMPTY_COMPRESSED_CDF:
strcpyX (textPtr,
"EMPTY_COMPRESSED_CDF: No internal records in a compressed CDF.",
CDF_STATUSTEXT_LEN);
break;

case END_OF_VAR:
strcpyX (textPtr,
"END_OF_VAR: Current position is at the end of the variable.",
CDF_STATUSTEXT_LEN);
break;

case FORCED_PARAMETER:
strcpyX (textPtr,
"FORCED_PARAMETER: A specified parameter was changed to a different value.",
CDF_STATUSTEXT_LEN);
break;

case IBM_PC_OVERFLOW:
strcpyX (textPtr,
"PC_OVERFLOW: Greater than 64Kb of memory required.",
CDF_STATUSTEXT_LEN);
break;

case ILLEGAL_FOR_SCOPE:
strcpyX (textPtr,
"ILLEGAL_FOR_SCOPE: The operation is illegal for the attribute's scope.",
CDF_STATUSTEXT_LEN);
break;

case ILLEGAL_IN_zMODE:
strcpyX (textPtr,
"ILLEGAL_IN_zMODE: Operation is illegal while in zMode.",
CDF_STATUSTEXT_LEN);
break;

case ILLEGAL_ON_V1_CDF:
strcpyX (textPtr,
"ILLEGAL_ON_V1_CDF: Operation not allowed on Version 1 CDFs.",
CDF_STATUSTEXT_LEN);
break;

case MULTI_FILE_FORMAT:
strcpyX (textPtr,
"MULTI_FILE_FORMAT: Operation n/a when multi-file format.",
CDF_STATUSTEXT_LEN);
break;

case NA_FOR_VARIABLE:
strcpyX (textPtr,
"NA_FOR_VARIABLE: Operation n/a for the type of variable.",
CDF_STATUSTEXT_LEN);
break;

case NEGATIVE_FP_ZERO:
strcpyX (textPtr,
"NEGATIVE_FP_ZERO: A negative floating point zero (-0.0) was detected.",
CDF_STATUSTEXT_LEN);
break;

case NO_ATTR_SELECTED:
strcpyX (textPtr,
"NO_ATTR_SELECTED: An attribute has not been selected.",
CDF_STATUSTEXT_LEN);
break;

case NO_CDF_SELECTED:
strcpyX (textPtr,
"NO_CDF_SELECTED: A CDF has not been selected.",
CDF_STATUSTEXT_LEN);
break;

case NO_DELETE_ACCESS:
strcpyX (textPtr,
"NO_DELETE_ACCESS: Deleting is not allowed (read only).",
CDF_STATUSTEXT_LEN);
break;

case NO_ENTRY_SELECTED:
strcpyX (textPtr,
"NO_ENTRY_SELECTED: An attribute entry has not been selected.",
CDF_STATUSTEXT_LEN);
break;

case NO_MORE_ACCESS:
strcpyX (textPtr,
"NO_MORE_ACCESS: No more access to the CDF due to a severe error.",
CDF_STATUSTEXT_LEN);
break;

case NO_PADVALUE_SPECIFIED:
strcpyX (textPtr,
"NO_PADVALUE_SPECIFIED: A pad value has not been specified.",
CDF_STATUSTEXT_LEN);
break;

case NO_STATUS_SELECTED:
strcpyX (textPtr,
"NO_STATUS_SELECTED: A CDF status code has not been selected.",
CDF_STATUSTEXT_LEN);
break;

case NO_SUCH_ATTR:
strcpyX (textPtr,
"NO_SUCH_ATTR: Named attribute not found in this CDF.",
CDF_STATUSTEXT_LEN);
break;

case NO_SUCH_CDF:
strcpyX (textPtr,
"NO_SUCH_CDF: The specified CDF does not exist.",
CDF_STATUSTEXT_LEN);
break;

case NO_SUCH_ENTRY:
strcpyX (textPtr,
"NO_SUCH_ENTRY: No such entry for specified attribute.",
CDF_STATUSTEXT_LEN);
break;

case NO_SUCH_RECORD:
strcpyX (textPtr,
"NO_SUCH_RECORD: The specified record does not exist.",
CDF_STATUSTEXT_LEN);
break;

case NO_SUCH_VAR:
strcpyX (textPtr,
"NO_SUCH_VAR: Named variable not found in this CDF.",
CDF_STATUSTEXT_LEN);
break;

case NO_VAR_SELECTED:
strcpyX (textPtr,
"NO_VAR_SELECTED: A variable has not been selected.",
CDF_STATUSTEXT_LEN);
break;

case NO_VARS_IN_CDF:
strcpyX (textPtr,
"NO_VARS_IN_CDF: CDF contains no rVariables.",
CDF_STATUSTEXT_LEN);
break;

case NO_WRITE_ACCESS:
strcpyX (textPtr,
"NO_WRITE_ACCESS: Write access is not allowed on the CDF file(s).",
CDF_STATUSTEXT_LEN);
break;

case NOT_A_CDF:
strcpyX (textPtr,
"NOT_A_CDF: Named CDF is corrupted or not actually a CDF.",
CDF_STATUSTEXT_LEN);
break;

case NOT_A_CDF_OR_NOT_SUPPORTED:
strcpyX (textPtr,
"NOT_A_CDF_OR_NOT_SUPPORTED: Named CDF is corrupted or not supported by the current library version.",
CDF_STATUSTEXT_LEN);
break;

case PRECEEDING_RECORDS_ALLOCATED:
strcpyX (textPtr,
"PRECEEDING_RECORDS_ALLOCATED: The preceeding records were also allocated.",
CDF_STATUSTEXT_LEN);
break;

case READ_ONLY_DISTRIBUTION:
strcpyX (textPtr,
"READ_ONLY_DISTRIBUTION: Writing/deleting is illegal.",
CDF_STATUSTEXT_LEN);
break;

case READ_ONLY_MODE:
strcpyX (textPtr,
"READ_ONLY_MODE: CDF is in read-only mode.",
CDF_STATUSTEXT_LEN);
break;

case SCRATCH_CREATE_ERROR:
strcpyX (textPtr,
"SCRATCH_CREATE_ERROR: Scratch file creation failed - error from file system.",
CDF_STATUSTEXT_LEN);
break;

case SCRATCH_DELETE_ERROR:
strcpyX (textPtr,
"SCRATCH_DELETE_ERROR: Scratch file deletion failed - error from file system.",
CDF_STATUSTEXT_LEN);
break;

case SCRATCH_READ_ERROR:
strcpyX (textPtr,
"SCRATCH_READ_ERROR: Scratch file read failed - error from file system.",
CDF_STATUSTEXT_LEN);
break;

case SCRATCH_WRITE_ERROR:
strcpyX (textPtr,
"SCRATCH_WRITE_ERROR: Scratch file write failed - error from file system.",
CDF_STATUSTEXT_LEN);
break;

case SINGLE_FILE_FORMAT:
strcpyX (textPtr,
"SINGLE_FILE_FORMAT: Operation n/a when single-file format.",
CDF_STATUSTEXT_LEN);
break;

case SOME_ALREADY_ALLOCATED:
strcpyX (textPtr,
"SOME_ALREADY_ALLOCATED: One or more of the records were already allocated.",
CDF_STATUSTEXT_LEN);
break;

case TOO_MANY_PARMS:
strcpyX (textPtr,
"TOO_MANY_PARMS: Too many parameters were encountered.",
CDF_STATUSTEXT_LEN);
break;

case TOO_MANY_VARS:
strcpyX (textPtr,
"TOO_MANY_VARS: Only a limited number of variables may exist in this CDF.",
CDF_STATUSTEXT_LEN);
break;

case UNKNOWN_COMPRESSION:
strcpyX (textPtr,
"UNKNOWN_COMPRESSION: An unknown type of compression was encountered.",
CDF_STATUSTEXT_LEN);
break;

case UNKNOWN_SPARSENESS:
strcpyX (textPtr,
"UNKNOWN_SPARSENESS: An unknown sparseness was encountered.",
CDF_STATUSTEXT_LEN);
break;

case UNSUPPORTED_OPERATION:
strcpyX (textPtr,
"UNSUPPORTED_OPERATION: The attempted operation isn't supported at this time.",
CDF_STATUSTEXT_LEN);
break;

case VAR_ALREADY_CLOSED:
strcpyX (textPtr,
"VAR_ALREADY_CLOSED: Variable is already closed.",
CDF_STATUSTEXT_LEN);
break;

case VAR_CLOSE_ERROR:
strcpyX (textPtr,
"VAR_CLOSE_ERROR: Close failed - error from file system.",
CDF_STATUSTEXT_LEN);
break;

case VAR_CREATE_ERROR:
strcpyX (textPtr,
"VAR_CREATE_ERROR: Creation failed - error from file system.",
CDF_STATUSTEXT_LEN);
break;

case VAR_DELETE_ERROR:
strcpyX (textPtr,
"VAR_DELETE_ERROR: Deletion failed - error from file system.",
CDF_STATUSTEXT_LEN);
break;

case VAR_EXISTS:
strcpyX (textPtr,
"VAR_EXISTS: Named variable already exists.",
CDF_STATUSTEXT_LEN);
break;

case VAR_NAME_TRUNC:
strcpyX (textPtr,
"VAR_NAME_TRUNC: Variable name truncated.",
CDF_STATUSTEXT_LEN);
break;

case VAR_OPEN_ERROR:
strcpyX (textPtr,
"VAR_OPEN_ERROR: Open failed - error from file system.",
CDF_STATUSTEXT_LEN);
break;

case VAR_READ_ERROR:
strcpyX (textPtr,
"VAR_READ_ERROR: Read failed - error from file system.",
CDF_STATUSTEXT_LEN);
break;

case VAR_WRITE_ERROR:
strcpyX (textPtr,
"VAR_WRITE_ERROR: Write failed - error from file system.",
CDF_STATUSTEXT_LEN);
break;

case VIRTUAL_RECORD_DATA:
strcpyX (textPtr,
"VIRTUAL_RECORD_DATA: One or more of the records are virtual.",
CDF_STATUSTEXT_LEN);
break;

case BAD_CHECKSUM:
strcpyX (textPtr,
"BAD_CHECKSUM: The specified checksum method is not currently supported.",
CDF_STATUSTEXT_LEN);
break;

case CHECKSUM_ERROR:
strcpyX (textPtr,
"CHECKSUM_ERROR: The data integrity verification through the checksum failed.",
CDF_STATUSTEXT_LEN);
break;

case CHECKSUM_NOT_ALLOWED:
strcpyX (textPtr,
"CHECKSUM_NOT_ALLOWED: The checksum is not allowed for old versioned files.",
CDF_STATUSTEXT_LEN);
break;

case IS_A_NETCDF:
strcpyX (textPtr,
"IS_A_NETCDF: Named CDF file is actually a netCDF file.",
CDF_STATUSTEXT_LEN);
break;

case TT2000_TIME_ERROR:
strcpyX (textPtr,
"TT2000_TIME_ERROR: Error handling the date/time for TT2000 or a TT2000 epoch.",
CDF_STATUSTEXT_LEN);
break;

case UNABLE_TO_PROCESS_CDF:
strcpyX (textPtr,
"UNABLE_TO_PROCESS_CDF: One of data fields is invalid... Upgrade the library might fix the problem.",
CDF_STATUSTEXT_LEN);
break;

case ZLIB_COMPRESS_ERROR:
strcpyX (textPtr,
"ZLIB_ERROR: Error during ZLIB compression.",
CDF_STATUSTEXT_LEN);
break;

case ZLIB_UNCOMPRESS_ERROR:
strcpyX (textPtr,
"ZLIB_ERROR: Error during ZLIB decompression.",
CDF_STATUSTEXT_LEN);
break;

case ILLEGAL_EPOCH_FIELD:
strcpyX (textPtr,
"ILLEGAL_EPOCH_FIELD: One or more of the date/time fields is out of valid range.",
CDF_STATUSTEXT_LEN);
break;

case CANNOT_INSERT_RECORDS:
strcpyX (textPtr,
"CANNOT_INSERT_RECORDS: Cannot insert records to a variable with sparserecords.",
CDF_STATUSTEXT_LEN);
break;

case TT2000_CDF_MAYNEEDUPDATE:
strcpyX (textPtr,
"TT2000_CDF_MAYNEEDUPDATE: CDF version or its file-based template may need to upgrade to handle TT2000 data properly (even its value may be valid).",
CDF_STATUSTEXT_LEN);
break;

case TT2000_USED_OUTDATED_TABLE:
strcpyX (textPtr,
"TT2000_USED_OUTDATED_TABLE: A TT2000 data is either invalid (made with an oudated leap second table) or trying to use an outdated leap second table.",
CDF_STATUSTEXT_LEN);
break;

case BADDATE_LEAPSECOND_UPDATED:
strcpyX (textPtr,
"BADDATE_LEAPSECOND_UPDATED: The last leap second updated date is not valid (not in the leap second table).",
CDF_STATUSTEXT_LEN);
break;

case FUNCTION_NOT_SUPPORTED:
strcpyX (textPtr,
"FUNCTION_NOT_SUPPORTED: This function is not supported. Likely, it is trying to access an rVariable.",
CDF_STATUSTEXT_LEN);
break;

default:
snprintf (textPtr, (size_t) CDF_STATUSTEXT_LEN+1,
          "Text not found - unknown CDF status code (%ld).", status);
return BAD_CDFSTATUS;

  }
#else
  snprintf (textPtr, (size_t) CDF_STATUSTEXT_LEN+1,
	    "Explanation text not available (%ld).", (long) status);
#endif
  return CDF_OK;
}
