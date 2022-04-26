	PROGRAM QST2IF
C-----------------------------------------------------------------------
C Copyright 1996-2014 United States Government as represented by the
C Administrator of the National Aeronautics and Space Administration.
C All Rights Reserved.
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C
C  NSSDC/CDF	  Quick Start Test Program (INTERNAL interface/FORTRAN).
C
C  Version 1.12, 20-May-96, Hughes STX.
C
C  Modification history:
C
C   V1.0    6-Jun-91, J Love	Original version (for CDF V2.1).
C   V1.1   25-Jun-91, J Love	Changed field width processing.  Renamed
C				CDF for portability.
C   V1.2   22-Jul-91, J Love	Use CDF_lib.  Changed variable data type
C				to CDF_CHAR.  Added SEQ ACC testing.
C   V1.3   26-Sep-91, J Love	Modified for IBM-RS6000 port.
C   V1.4    6-Oct-91, J Love	No longer use I<width> format specifier
C				(does not seem to work on IBM-RS6000).
C   V1.5   24-Oct-91, J Love	Use <SELECT_, VAR_NAME_/ATTR_NAME_> to
C				rename variable/attribute.
C   V1.6   20-May-92, J Love	CDF V2.2.
C   V1.7   21-Sep-92, J Love	CDF V2.3 (shareable/Next/zVar).
C   V1.8   19-Jan-94, J Love	CDF V2.4.
C   V1.8a  22-Feb-94, J Love	Limited lines to 72 columns or less.
C   V1.9   22-Jun-94, J Love	Renamed CDF to `TEST'.
C   V1.10  20-Dec-94, J Love	CDF V2.5.
C   V1.11  28-Mar-95, J Love	Multi-file format.
C   V1.11a 13-Jun-95, J Love	Linux.
C   V1.12  20-May-96, J Love	CDF V2.6.
C
C-----------------------------------------------------------------------

        INCLUDE 'CDFDVF.INC'
        INCLUDE 'CDFDF.INC'
		
	INTEGER*4 ID, STATUS, ENCODING, MAJORITY, NUM_DIMS,
     .		  DIM_SIZES(2), VAR_DATA_TYPE, VAR_DATA_TYPE_OUT,
     .		  VAR_NUM_ELEMENTS, VAR_NUM_ELEMENTS_OUT, VAR_NUM_OUT,
     .		  INDICES(2), REC_NUM, REC_START, REC_COUNT,
     .		  REC_INTERVAL, COUNTS(2), INTERVALS(2), cReserve,
     .		  ATTR_NUM_OUT, ENTRY_NUM, NUM_ENTRIES_OUT, ATTRSCOPE,
     .		  ATTRSCOPE_OUT, ENTRY_DATA_TYPE, ENTRY_DATA_TYPE_OUT,
     .		  ENTRY_NUM_ELEMENTS, ENTRY_NUM_ELEMENTS_OUT,
     .		  ENTRY_VALUE, ENTRY_VALUE_OUT, ENCODING_OUT,
     .		  MAJORITY_OUT, NUM_DIMS_OUT, DIM_SIZES_OUT(2),
     .		  MAX_REC_OUT, NUM_VARS_OUT, NUM_ATTRS_OUT,
     .		  NEGtoPOSmode, READONLYmode, RELEASElib, VERSIONlib,
     .		  INCREMENTlib, RELEASEcdf, VERSIONcdf, INCREMENTcdf,
     .		  cacheSize, cacheSizeOUT, I, X1, X2, X, decodingOut,
     .		  VAR_REC_VARIANCE, VAR_REC_VARIANCE_OUT, recnum_out,
     .		  VAR_DIM_VARIANCES(2), reccount_out, cSizeOUT,
     .		  VAR_DIM_VARIANCES_OUT(2),  recinterval_out,
     .		  dimindices_out(CDF_MAX_DIMS), datatype_size,
     .		  dimcounts_out(CDF_MAX_DIMS), cType, cTypeOUT,
     .		  dimintervals_out(CDF_MAX_DIMS), cParms(CDF_MAX_PARMS)
        INTEGER*4 cParmsOUT(CDF_MAX_PARMS), cPctOUT, uSizeOUT,
     .		  sRecordsType, sArraysType, sRecordsTypeOUT,
     .		  sArraysParms(CDF_MAX_PARMS), sArraysTypeOUT,
     .		  sArraysParmsOUT(CDF_MAX_PARMS), sArraysPctOUT,
     .		  numVars, varNums(2), recBuffer(12), recBufferOUT(12),
     .		  nRecordsOUT, nAllocOUT, fromOUT, toOUT, nLevelsOUT,
     .		  cReserveOUT

	CHARACTER CDFNAME*(CDF_PATHNAME_LEN),
     .		  VARNAME*(CDF_VAR_NAME_LEN256),
     .		  NEW_VARNAME*(CDF_VAR_NAME_LEN256),
     .		  VARNAME_OUT*(CDF_VAR_NAME_LEN256),
     .		  ATTRNAME*(CDF_ATTR_NAME_LEN256),
     .		  NEW_ATTRNAME*(CDF_ATTR_NAME_LEN256),
     .		  ATTRNAME_OUT*(CDF_ATTR_NAME_LEN256),
     .		  COPYRIGHT_TEXTlib*(CDF_COPYRIGHT_LEN),
     .		  COPYRIGHT_TEXTcdf*(CDF_COPYRIGHT_LEN),
     .		  ERRORTEXT*(CDF_STATUSTEXT_LEN),
     .		  VAR_VALUES(2,3)*5, VAR_VALUE_OUT*5,
     .		  VAR_BUFFER_OUT(2,3)*5,
     .		  SUBINCREMENTlib*1, nameOut*(CDF_PATHNAME_LEN)

	DATA ENCODING/IBMPC_ENCODING/, MAJORITY/COL_MAJOR/,
     .	     NUM_DIMS/2/, DIM_SIZES/2,3/,
     .	     VAR_DATA_TYPE/CDF_CHAR/, VAR_NUM_ELEMENTS/5/,
     .	     VAR_REC_VARIANCE/VARY/, VAR_DIM_VARIANCES/VARY,VARY/,
     .	     REC_NUM/1/, REC_START/1/, REC_COUNT/1/, REC_INTERVAL/1/,
     .	     COUNTS/2,3/, INTERVALS/1,1/,
     .	     ENTRY_NUM/1/, ATTRSCOPE/GLOBAL_SCOPE/,
     .	     ENTRY_DATA_TYPE/CDF_INT4/, ENTRY_NUM_ELEMENTS/1/,
     .	     ENTRY_VALUE/1/, cacheSize/3/, numVars/2/,
     .	     VAR_VALUES/'11111','22222','33333',
     .			'44444','55555','66666'/,
     .	     CDFNAME(1:4)/'TEST'/, VARNAME(1:4)/'VAR1'/,
     .	     NEW_VARNAME(1:4)/'VAR2'/, ATTRNAME(1:5)/'ATTR1'/,
     .	     NEW_ATTRNAME(1:5)/'ATTR2'/, cType/RLE_COMPRESSION/,
     .	     cParms(1)/RLE_OF_ZEROs/, sRecordsType/PAD_SPARSERECORDS/,
     .	     sArraysType/NO_SPARSEARRAYS/, cReserve/150/,
     .	     recBuffer/1,2,3,4,5,6,7,8,9,10,11,12/

C-----------------------------------------------------------------------
C NUL-terminate character strings.
C-----------------------------------------------------------------------

	CDFNAME(5:5) = CHAR(0)
	VARNAME(5:5) = CHAR(0)
	NEW_VARNAME(5:5) = CHAR(0)
	ATTRNAME(6:6) = CHAR(0)
	NEW_ATTRNAME(6:6) = CHAR(0)

C-----------------------------------------------------------------------
C Display test title.
C-----------------------------------------------------------------------

	WRITE (6,100)
 100	FORMAT (' ','Testing Internal/FORTRAN interface...')

C-----------------------------------------------------------------------
C Create CDF.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB    (CREATE_, CDF_, CDFNAME, NUM_DIMS,
     .				            DIM_SIZES, ID,
C    .		             PUT_, CDF_ENCODING_, ENCODING,
     .			     PUT_, CDF_MAJORITY_, MAJORITY,
     .			           CDF_FORMAT_, SINGLE_FILE,
     .		             NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) THEN
	  IF (STATUS .EQ. CDF_EXISTS) THEN
            STATUS = CDF_LIB   (OPEN_, CDF_, CDFNAME, ID,
     .			        NULL_, STATUS)
	    IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '1.0')

            STATUS = CDF_LIB   (DELETE_, CDF_,
     .			        NULL_, STATUS)
	    IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '1.1')

            STATUS = CDF_LIB    (CREATE_, CDF_, CDFNAME, NUM_DIMS,
     .					        DIM_SIZES, ID,
C    .			         PUT_, CDF_ENCODING_, ENCODING,
     .				 PUT_, CDF_MAJORITY_, MAJORITY,
     .				       CDF_FORMAT_, MULTI_FILE,
     .			         NULL_, STATUS)
	    IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '1.2')
	   ELSE
	    CALL QUIT_CDF (STATUS, '1.3')
	  END IF
	END IF

C-----------------------------------------------------------------------
C Create variable.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB    (CREATE_, rVAR_, VARNAME, VAR_DATA_TYPE,
     .				             VAR_NUM_ELEMENTS,
     .				             VAR_REC_VARIANCE,
     .				             VAR_DIM_VARIANCES,
     .				             VAR_NUM_OUT,
     .		             NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '2.0')

C-----------------------------------------------------------------------
C Close CDF.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (CLOSE_, CDF_,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '3.0')

C-----------------------------------------------------------------------
C Reopen CDF.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (OPEN_, CDF_, CDFNAME, ID,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '4.0')

C-----------------------------------------------------------------------
C Delete CDF.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (DELETE_, CDF_,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '5.0')

C-----------------------------------------------------------------------
C Create CDF again (previous delete will allow this).
C-----------------------------------------------------------------------

        STATUS = CDF_LIB    (CREATE_, CDF_, CDFNAME, NUM_DIMS,
     .				            DIM_SIZES, ID,
C    .		             PUT_, CDF_ENCODING_, ENCODING,
     .			     PUT_, CDF_MAJORITY_, MAJORITY,
     .			           CDF_FORMAT_, SINGLE_FILE,
     .			           CDF_COMPRESSION_, cType, cParms,
     .			     SELECT_, CDF_CACHESIZE_, cacheSize,
     .		             NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '6.0')

C-----------------------------------------------------------------------
C Inquire CDF.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB    (GET_, rVARs_NUMDIMS_, NUM_DIMS_OUT,
     .			           rVARs_DIMSIZES_, DIM_SIZES_OUT,
     .			           CDF_ENCODING_, ENCODING_OUT,
     .			           CDF_MAJORITY_, MAJORITY_OUT,
     .			           rVARs_MAXREC_, MAX_REC_OUT,
     .			           CDF_NUMrVARS_, NUM_VARS_OUT,
     .			           CDF_NUMATTRS_, NUM_ATTRS_OUT,
     .			     CONFIRM_, CDF_CACHESIZE_, cacheSizeOUT,
     .		             NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '6a.0')

	IF (NUM_DIMS_OUT .NE. NUM_DIMS)
     .	  CALL QUIT_CDF (STATUS, '6a.1')

	DO X = 1, 2
	  IF (DIM_SIZES_OUT(X) .NE. DIM_SIZES(X))
     .	    CALL QUIT_CDF (STATUS, '6a.2')
	END DO

	IF (ENCODING_OUT .NE. ENCODING)
     .	  CALL QUIT_CDF (STATUS, '6a.3')
	IF (MAJORITY_OUT .NE. MAJORITY)
     .	  CALL QUIT_CDF (STATUS, '6a.4')
	IF (MAX_REC_OUT .NE. 0) CALL QUIT_CDF (STATUS, '6a.5')
	IF (NUM_VARS_OUT .NE. 0) CALL QUIT_CDF (STATUS, '6a.6')
	IF (NUM_ATTRS_OUT .NE. 0) CALL QUIT_CDF (STATUS, '6a.7')
	IF (cacheSizeOUT .NE. cacheSize)
     .	  CALL QUIT_CDF (STATUS, '6a.8')

C-----------------------------------------------------------------------
C Create variables.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB    (CREATE_, rVAR_, VARNAME, VAR_DATA_TYPE,
     .				             VAR_NUM_ELEMENTS,
     .				             VAR_REC_VARIANCE,
     .				             VAR_DIM_VARIANCES,
     .				             VAR_NUM_OUT,
     .			     PUT_, rVAR_COMPRESSION_, cType, cParms,
     .			           rVAR_SPARSERECORDS_, sRecordsType,
     .			           rVAR_SPARSEARRAYS_, sArraysType,
     .						       sArraysParms,
     .			     SELECT_, rVAR_RESERVEPERCENT_, cReserve,
     .		             NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '7.0')

        STATUS = CDF_LIB    (CREATE_, rVAR_, 'VAR2a', CDF_INT4, 1,
     .					     VAR_REC_VARIANCE,
     .					     VAR_DIM_VARIANCES,
     .					     varNums(1),
     .			     CREATE_, rVAR_, 'VAR2b', CDF_INT4, 1,
     .					     VAR_REC_VARIANCE,
     .					     VAR_DIM_VARIANCES,
     .					     varNums(2),
     .		             NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '7a.0')

C-----------------------------------------------------------------------
C Alocate/inquire records for a variable.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB    (SELECT_, rVAR_, varNums(1),
     .			     PUT_, rVAR_ALLOCATEBLOCK_, 1, 10,
     .			     GET_, rVAR_NUMallocRECS_, nAllocOUT,
     .			           rVAR_ALLOCATEDFROM_, 5, fromOUT,
     .			           rVAR_ALLOCATEDTO_, 5, toOUT,
     .			           rVAR_nINDEXLEVELS_, nLevelsOUT,
     .			     NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '7b.0')

	IF (nAllocOUT .NE. 10) THEN
	  CALL QUIT_CDF (STATUS, '7b.1')
	END IF
	IF (fromOUT .NE. 5) THEN
	  CALL QUIT_CDF (STATUS, '7b.2')
	END IF
	IF (toOUT .NE. 10) THEN
	  CALL QUIT_CDF (STATUS, '7b.3')
	END IF

C-----------------------------------------------------------------------
C PUT to variable.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (SELECT_, rVAR_, VAR_NUM_OUT,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '8.0')

        STATUS = CDF_LIB   (SELECT_, rVARs_RECNUMBER_, REC_NUM,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '8.0a')

	DO X2 = 1, 3
	  DO X1 = 1, 2
	    INDICES(1) = X1
	    INDICES(2) = X2
            STATUS = CDF_LIB   (SELECT_, rVARs_DIMINDICES_, INDICES,
     .			        PUT_, rVAR_DATA_, VAR_VALUES(X1,X2),
     .			        NULL_, STATUS)
	    IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '8.1')
	  END DO
	END DO

C-----------------------------------------------------------------------
C GET from the variable.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (SELECT_, rVARs_RECNUMBER_, REC_NUM,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '9.0')

	DO X2 = 1, 3
	  DO X1 = 1, 2
	    INDICES(1) = X1
	    INDICES(2) = X2
            STATUS = CDF_LIB   (SELECT_, rVARs_DIMINDICES_, INDICES,
     .			        GET_, rVAR_DATA_, VAR_VALUE_OUT,
     .			        NULL_, STATUS)
	    IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '9.1')
	    IF (VAR_VALUE_OUT .NE. VAR_VALUES(X1,X2)) THEN
	      CALL QUIT_CDF (STATUS, '9.2')
	    END IF
	  END DO
	END DO

C-----------------------------------------------------------------------
C HyperPUT to the variable.
C-----------------------------------------------------------------------

	DO X2 = 1, 3
	  DO X1 = 1, 2
	    DO I = 1, VAR_NUM_ELEMENTS
	      VAR_VALUES(X1,X2)(I:I) =
     .		CHAR(ICHAR(VAR_VALUES(X1,X2)(I:I)) + 1)
	    END DO
	  END DO
	END DO

	INDICES(1) = 1
	INDICES(2) = 1

        STATUS = CDF_LIB    (SELECT_, rVARs_RECNUMBER_, REC_START,
     .			              rVARs_RECCOUNT_, REC_COUNT,
     .			              rVARs_RECINTERVAL_, REC_INTERVAL,
     .			              rVARs_DIMINDICES_, INDICES,
     .			              rVARs_DIMCOUNTS_, COUNTS,
     .			              rVARs_DIMINTERVALS_, INTERVALS,
     .		             NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '10.0a')

        STATUS = CDF_LIB   (PUT_, rVAR_HYPERDATA_, VAR_VALUES,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '10.0b')

C-----------------------------------------------------------------------
C HyperGET from variable.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (GET_, rVAR_HYPERDATA_, VAR_BUFFER_OUT,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '11.0')

	DO X2 = 1, 3
	  DO X1 = 1, 2
	    IF (VAR_BUFFER_OUT(X1,X2) .NE. VAR_VALUES(X1,X2))
     .	      CALL QUIT_CDF (STATUS, '11.1')
	  END DO
	END DO

C-----------------------------------------------------------------------
C Confirm indices, counts, etc.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB    (CONFIRM_, rVARs_RECNUMBER_, recnum_out,
     .			               rVARs_RECCOUNT_, reccount_out,
     .			               rVARs_RECINTERVAL_, recinterval_out,
     .			               rVARs_DIMINDICES_, dimindices_out,
     .			               rVARs_DIMCOUNTS_, dimcounts_out,
     .			               rVARs_DIMINTERVALS_,dimintervals_out,
     .		             NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '10a.0')

	IF (recnum_out .NE. REC_START)
     .	  CALL QUIT_CDF (STATUS, '10a.1')
	IF (reccount_out .NE. REC_COUNT)
     .	  CALL QUIT_CDF (STATUS, '10a.2')
	IF (recinterval_out .NE. REC_INTERVAL)
     .	  CALL QUIT_CDF (STATUS, '10a.3')

	DO I = 1, 2
	  IF (dimindices_out(I) .NE. INDICES(I))
     .	    CALL QUIT_CDF (STATUS, '10a.4')
	  IF (dimcounts_out(I) .NE. COUNTS(I))
     .	    CALL QUIT_CDF (STATUS, '10a.5')
	  IF (dimintervals_out(I) .NE. INTERVALS(I))
     .	    CALL QUIT_CDF (STATUS, '10a.6')
	END DO

C-----------------------------------------------------------------------
C SEQuential ACCess PUT to variable.
C-----------------------------------------------------------------------

	DO X2 = 1, 3
	  DO X1 = 1, 2
	    DO I = 1, VAR_NUM_ELEMENTS
	      VAR_VALUES(X1,X2)(I:I) =
     .		CHAR(ICHAR(VAR_VALUES(X1,X2)(I:I)) + 1)
	    END DO
	  END DO
	END DO

	REC_NUM = 1
	INDICES(1) = 1
	INDICES(2) = 1

        STATUS = CDF_LIB   (SELECT_, rVAR_SEQPOS_, REC_NUM, INDICES,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '11a.0')

	DO X2 = 1, 3
	  DO X1 = 1, 2
            STATUS = CDF_LIB   (PUT_, rVAR_SEQDATA_, VAR_VALUES(X1,X2),
     .			        NULL_, STATUS)
	    IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '11a.1')
	  END DO
	END DO

        STATUS = CDF_LIB   (CONFIRM_, rVAR_SEQPOS_, REC_NUM, INDICES,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '11c.0')

	IF (REC_NUM .NE. 2) CALL QUIT_CDF (STATUS, '11c.1')
	IF (INDICES(1) .NE. 1) CALL QUIT_CDF (STATUS, '11c.2')
	IF (INDICES(2) .NE. 1) CALL QUIT_CDF (STATUS, '11c.3')

C-----------------------------------------------------------------------
C SEQuential ACCess GET from variable.
C-----------------------------------------------------------------------

	REC_NUM = 1
	INDICES(1) = 1
	INDICES(2) = 1

        STATUS = CDF_LIB   (SELECT_, rVAR_SEQPOS_, REC_NUM, INDICES,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '11b.0')

	DO X2 = 1, 3
	  DO X1 = 1, 2
            STATUS = CDF_LIB   (GET_, rVAR_SEQDATA_, VAR_VALUE_OUT,
     .			        NULL_, STATUS)
	    IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '11b.1')
	    IF (VAR_VALUE_OUT .NE. VAR_VALUES(X1,X2))
     .	      CALL QUIT_CDF (STATUS, '11b.2')
	  END DO
	END DO

C-----------------------------------------------------------------------
C Record access.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB    (SELECT_, rVARs_RECNUMBER_, 10,
     .			     PUT_, rVARs_RECDATA_, numVars,
     .						   varNums,
     .					           recBuffer,
     .		             NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, 'RA.1')

        STATUS = CDF_LIB   (GET_, rVARs_RECDATA_, numVars,
     .						  varNums,
     .					          recBufferOUT,
     .		        NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, 'RA.2')

	DO I = 1, 12
	  IF (recBufferOUT(I) .NE. recBuffer(I)) THEN
	    CALL QUIT_CDF (STATUS, 'RA.3')
	  ENDIF
	END DO

C-----------------------------------------------------------------------
C Create attribute.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (CREATE_, ATTR_, ATTRNAME,
     .					    ATTRSCOPE,
     .				            ATTR_NUM_OUT,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '12.0')

C-----------------------------------------------------------------------
C PUT to attribute.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB    (SELECT_, gENTRY_, ENTRY_NUM,
     .		             PUT_, gENTRY_DATA_, ENTRY_DATA_TYPE,
     .				                 ENTRY_NUM_ELEMENTS,
     .				                 ENTRY_VALUE,
     .		             NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '13.0')

C-----------------------------------------------------------------------
C GET from attribute.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (GET_, gENTRY_DATA_, ENTRY_VALUE_OUT,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '14.0')

	IF (ENTRY_VALUE_OUT .NE. ENTRY_VALUE) THEN
	  CALL QUIT_CDF (STATUS, '14.1')
	ENDIF

C-----------------------------------------------------------------------
C Get CDF documentation.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB    (GET_, LIB_VERSION_, VERSIONlib,
     .			           LIB_RELEASE_, RELEASElib,
     .			           LIB_INCREMENT_, INCREMENTlib,
     .			           LIB_subINCREMENT_, SUBINCREMENTlib,
     .		                   LIB_COPYRIGHT_, COPYRIGHT_TEXTlib,
     .		             NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '15.0')

        STATUS = CDF_LIB    (GET_, CDF_VERSION_, VERSIONcdf,
     .			           CDF_RELEASE_, RELEASEcdf,
     .			           CDF_INCREMENT_, INCREMENTcdf,
     .		                   CDF_COPYRIGHT_, COPYRIGHT_TEXTcdf,
     .		             NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '15.1')

	IF (VERSIONcdf .NE. VERSIONlib)
     .	  CALL QUIT_CDF (STATUS, '15.2')
	IF (RELEASEcdf .NE. RELEASElib)
     .	  CALL QUIT_CDF (STATUS, '15.3')
	IF (INCREMENTcdf .NE. INCREMENTlib)
     .	  CALL QUIT_CDF (STATUS, '15.4')
	IF (COPYRIGHT_TEXTcdf .NE. COPYRIGHT_TEXTlib)
     .	  CALL QUIT_CDF (STATUS, '15.4')

C-----------------------------------------------------------------------
C Inquire CDF.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB    (GET_, rVARs_NUMDIMS_, NUM_DIMS_OUT,
     .			           rVARs_DIMSIZES_, DIM_SIZES_OUT,
     .			           CDF_ENCODING_, ENCODING_OUT,
     .			           CDF_MAJORITY_, MAJORITY_OUT,
     .			           rVARs_MAXREC_, MAX_REC_OUT,
     .			           CDF_NUMrVARS_, NUM_VARS_OUT,
     .			           CDF_NUMATTRS_, NUM_ATTRS_OUT,
     .			           CDF_COMPRESSION_, cTypeOUT,
     .						     cParmsOUT,
     .						     cPctOUT,
     .		             NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '16.0')

	IF (NUM_DIMS_OUT .NE. NUM_DIMS)
     .	  CALL QUIT_CDF (STATUS, '16.1')

	DO X = 1, 2
	  IF (DIM_SIZES_OUT(X) .NE. DIM_SIZES(X)) THEN
	    CALL QUIT_CDF (STATUS, '16.2')
	  ENDIF
	END DO

	IF (ENCODING_OUT .NE. ENCODING) THEN
	  CALL QUIT_CDF (STATUS, '16.3')
	ENDIF
	IF (MAJORITY_OUT .NE. MAJORITY) THEN
	  CALL QUIT_CDF (STATUS, '16.4')
	ENDIF
	IF (MAX_REC_OUT .NE. 10) CALL QUIT_CDF (STATUS, '16.5')
	IF (NUM_VARS_OUT .NE. 3) CALL QUIT_CDF (STATUS, '16.6')
	IF (NUM_ATTRS_OUT .NE. 1) CALL QUIT_CDF (STATUS, '16.7')
	IF (cTypeOUT .NE. cType) CALL QUIT_CDF (STATUS, '16.8')

C-----------------------------------------------------------------------
C Rename variable.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (SELECT_, rVAR_NAME_, VARNAME,
     .		            PUT_, rVAR_NAME_, NEW_VARNAME,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '17.0')

C-----------------------------------------------------------------------
C Inquire variable.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB    (GET_, rVAR_NAME_, VARNAME_OUT,
     .			           rVAR_DATATYPE_, VAR_DATA_TYPE_OUT,
     .			           rVAR_NUMELEMS_, VAR_NUM_ELEMENTS_OUT,
     .			           rVAR_COMPRESSION_, cTypeOUT,
     .						      cParmsOUT,
     .						      cPctOUT,
     .			     CONFIRM_, rVAR_RESERVEPERCENT_,cReserveOUT,
     .			     GET_, rVAR_SPARSERECORDS_, sRecordsTypeOUT,
     .			           rVAR_SPARSEARRAYS_, sArraysTypeOUT,
     .						       sArraysParmsOUT,
     .						       sArraysPctOUT,
     .			           rVAR_NUMRECS_, nRecordsOUT,
     .		             NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '18.0')

        STATUS = CDF_LIB   (GET_, rVAR_RECVARY_, VAR_REC_VARIANCE_OUT,
     .			          rVAR_DIMVARYS_, VAR_DIM_VARIANCES_OUT,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '18.0')

	IF (VARNAME_OUT .NE. NEW_VARNAME(1:4)) THEN
	  CALL QUIT_CDF (STATUS, '18.1')
	ENDIF
	IF (VAR_DATA_TYPE_OUT .NE. VAR_DATA_TYPE) THEN
	  CALL QUIT_CDF (STATUS, '18.2')
	ENDIF
	IF (VAR_NUM_ELEMENTS_OUT .NE. VAR_NUM_ELEMENTS) THEN
	  CALL QUIT_CDF (STATUS, '18.3')
	ENDIF
	IF (VAR_REC_VARIANCE_OUT .NE. VAR_REC_VARIANCE) THEN
	  CALL QUIT_CDF (STATUS, '18.4')
	ENDIF
	DO X = 1, 2
	  IF (VAR_DIM_VARIANCES_OUT(X) .NE. VAR_DIM_VARIANCES(X)) THEN
	    CALL QUIT_CDF (STATUS, '18.5')
	  ENDIF
	END DO
	IF (cTypeOUT .NE. cType) CALL QUIT_CDF (STATUS, '18.5')
	IF (sRecordsTypeOUT .NE. sRecordsType) THEN
	  CALL QUIT_CDF (STATUS, '18.6')
	ENDIF
	IF (sArraysTypeOUT .NE. sArraysType) THEN
	  CALL QUIT_CDF (STATUS, '18.7')
	ENDIF
	IF (nRecordsOUT .NE. 1) THEN
	  CALL QUIT_CDF (STATUS, '18.8')
	ENDIF
	IF (cReserveOUT .NE. cReserve) THEN
	  CALL QUIT_CDF (STATUS, '18.9')
	ENDIF

C-----------------------------------------------------------------------
C Rename attribute.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (SELECT_, ATTR_NAME_, ATTRNAME,
     .		            PUT_, ATTR_NAME_, NEW_ATTRNAME,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '20.0')

C-----------------------------------------------------------------------
C Inquire attribute.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (GET_, ATTR_NAME_, ATTRNAME_OUT,
     .			          ATTR_SCOPE_, ATTRSCOPE_OUT,
     .			          ATTR_NUMgENTRIES_, NUM_ENTRIES_OUT,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '22.0')

	IF (ATTRNAME_OUT .NE. NEW_ATTRNAME(1:5))
     .	  CALL QUIT_CDF (STATUS, '22.1')
	IF (ATTRSCOPE_OUT .NE. ATTRSCOPE)
     .	  CALL QUIT_CDF (STATUS, '22.2')
	IF (NUM_ENTRIES_OUT .NE. 1) CALL QUIT_CDF (STATUS, '22.3')

C-----------------------------------------------------------------------
C Inquire attribute entry.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (GET_, gENTRY_DATATYPE_, ENTRY_DATA_TYPE_OUT,
     .			          gENTRY_NUMELEMS_, ENTRY_NUM_ELEMENTS_OUT,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '23.0')

	IF (ENTRY_DATA_TYPE_OUT .NE. ENTRY_DATA_TYPE)
     .	  CALL QUIT_CDF (STATUS, '23.1')

	IF (ENTRY_NUM_ELEMENTS_OUT .NE. ENTRY_NUM_ELEMENTS)
     .	  CALL QUIT_CDF (STATUS, '23.2')

C-----------------------------------------------------------------------
C Get error text.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (SELECT_, CDF_STATUS_, CDF_OK,
     .		            GET_, STATUS_TEXT_, ERRORTEXT,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '24.0')

C-----------------------------------------------------------------------
C Set and confirm CDF modes, decoding, etc.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (SELECT_, CDF_NEGtoPOSfp0_MODE_,
     .							NEGtoPOSfp0on,
     .				     CDF_READONLY_MODE_, READONLYon,
     .				     CDF_DECODING_, MAC_DECODING,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '25.0')

        STATUS = CDF_LIB    (CONFIRM_, CDF_NEGtoPOSfp0_MODE_,
     .							NEGtoPOSmode,
     .				       CDF_READONLY_MODE_, READONLYmode,
     .				       CDF_DECODING_, decodingOut,
     .				       CDF_NAME_, nameOut,
     .		             NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '26.0')

	IF (NEGtoPOSmode .NE. NEGtoPOSfp0on)
     .    CALL QUIT_CDF (STATUS, '26.1')
	IF (READONLYmode .NE. READONLYon)
     .    CALL QUIT_CDF (STATUS, '26.2')
	IF (decodingOut .NE. MAC_DECODING)
     .	  CALL QUIT_CDF (STATUS, '26.3')
	IF (nameOut(1:4) .NE. CDFNAME(1:4))
     .	  CALL QUIT_CDF (STATUS, '26.4')

C-----------------------------------------------------------------------
C Inquire size of a data type.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (GET_, DATATYPE_SIZE_, CDF_EPOCH,
     .						  datatype_size,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '27.0')

	IF (datatype_size .ne. 8) CALL QUIT_CDF (STATUS, '27.1')

C-----------------------------------------------------------------------
C Close CDF.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (CLOSE_, CDF_,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '28.0')

C-----------------------------------------------------------------------
C Get CDF information.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (GET_, CDF_INFO_, CDFNAME, cTypeOUT,
     .					     cParmsOUT, cSizeOUT,
     .					     uSizeOUT,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '29.0')

	IF (cTypeOUT .NE. cType) CALL QUIT_CDF (STATUS, '29.1')

C-----------------------------------------------------------------------

	END

C-----------------------------------------------------------------------
C QUIT_CDF.  Abort test early due to CDF error.
C-----------------------------------------------------------------------

	SUBROUTINE QUIT_CDF (STATUS, WHERE)
	INTEGER*4 STATUS
	CHARACTER WHERE*(*)
	WRITE (6,401) WHERE
 401	FORMAT (' ', 'Aborting at ', A, '...')
	IF (STATUS .LT. 0) THEN
	  WRITE (6,501) STATUS
 501	  FORMAT (' ', 'CDF status code: ', I5)
	ENDIF
	WRITE (6,404)
 404	FORMAT (' ','...test aborted')
	STOP
	END
