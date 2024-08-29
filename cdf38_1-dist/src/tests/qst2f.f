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
C   V2.1   27-May-05, M Liu     CDF V3.1.
C
C-----------------------------------------------------------------------

        INCLUDE 'CDF.INC'
		
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
     .		  MAX_RREC_OUT, NUM_RVARS_OUT, NUM_ATTRS_OUT,
     .            MAX_ZREC_OUT, NUM_ZVARS_OUT,
     .		  NEGtoPOSmode, READONLYmode, RELEASElib, VERSIONlib,
     .		  INCREMENTlib, RELEASEcdf, VERSIONcdf, INCREMENTcdf,
     .		  cacheSize, cacheSizeOUT, I, J, X1, X2, X, decodingOut,
     .		  VAR_REC_VARIANCE, VAR_REC_VARIANCE_OUT, recnum_out,
     .		  VAR_DIM_VARIANCES(2), reccount_out,
     .		  VAR_DIM_VARIANCES_OUT(2),  recinterval_out,
     .		  dimindices_out(CDF_MAX_DIMS), datatype_size,
     .		  dimcounts_out(CDF_MAX_DIMS), cType, cTypeOUT,
     .		  dimintervals_out(CDF_MAX_DIMS), cParms(CDF_MAX_PARMS)
	INTEGER*8 cSizeOUT, uSizeOUT
        INTEGER*4 cParmsOUT(CDF_MAX_PARMS), cPctOUT, 
     .		  sRecordsType, sArraysType, sRecordsTypeOUT,
     .		  sArraysParms(CDF_MAX_PARMS), sArraysTypeOUT,
     .		  sArraysParmsOUT(CDF_MAX_PARMS), sArraysPctOUT,
     .		  numVars, varNums(2), 
     .		  nRecordsOUT, nAllocOUT, fromOUT, toOUT, nLevelsOUT,
     .		  cReserveOUT
        INTEGER*4 recBuffer1(2,3), recBuffer2(2,3), recBuffer1OUT(2,3), 
     .		  recBuffer2OUT(2,3)
        INTEGER*4 recBuffer1x(3), recBuffer1xOUT(3)

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

        COMMON /BLK1/recBuffer1,recBuffer2
        COMMON /BLK2/recBuffer1OUT,recBuffer2OUT

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
     .	     recBuffer1/1,2,3,4,5,6/,
     .       recBuffer2/7,8,9,10,11,12/,
     .       recBuffer1x/13,14,15/

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
 100	FORMAT (' ','Testing Standard/FORTRAN interface...')

C-----------------------------------------------------------------------
C Create CDF.
C-----------------------------------------------------------------------

        CALL CDF_create_cdf (CDFNAME, ID, STATUS)
	IF (STATUS .LT. CDF_OK) THEN
	  IF (STATUS .EQ. CDF_EXISTS) THEN
            CALL CDF_open_cdf ("TEST", ID, STATUS)
	    IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '1.0')

            CALL CDF_delete_cdf (ID, STATUS)
	    IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '1.1')

            CALL CDF_create_cdf (CDFNAME, ID, STATUS)
	    IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '1.2')
	   ELSE
	    CALL QUIT_CDF (STATUS, '1.3')
	  END IF
	END IF

C-----------------------------------------------------------------------
C Create variable.
C-----------------------------------------------------------------------

        CALL CDF_create_zvar (ID, VARNAME, VAR_DATA_TYPE, 
     .			      VAR_NUM_ELEMENTS, NUM_DIMS,
     .                        DIM_SIZES, VAR_REC_VARIANCE, 
     .			      VAR_DIM_VARIANCES, VAR_NUM_OUT, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '2.0')

C-----------------------------------------------------------------------
C Close CDF.
C-----------------------------------------------------------------------

        CALL CDF_close (ID, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '3.0')

C-----------------------------------------------------------------------
C Reopen CDF.
C-----------------------------------------------------------------------

        CALL CDF_open ("TEST", ID, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '4.0')

C-----------------------------------------------------------------------
C Delete CDF.
C-----------------------------------------------------------------------

        CALL CDF_delete (ID, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '5.0')

C-----------------------------------------------------------------------
C Create CDF again (previous delete will allow this).
C-----------------------------------------------------------------------

        CALL CDF_create_cdf (CDFNAME, ID, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '6.0')

C       CALL CDF_set_encoding (ID, ENCODING, STATUS)
C       IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '6.1')

        CALL CDF_set_majority (ID, MAJORITY, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '6.2')

        CALL CDF_set_format (ID, SINGLE_FILE, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '6.3')

        CALL CDF_set_compression (ID, cType, cParms, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '6.4')

        CALL CDF_set_cachesize (ID, cacheSize, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '6.5')

C-----------------------------------------------------------------------
C Inquire CDF.
C-----------------------------------------------------------------------

        CALL CDF_get_encoding (ID, ENCODING_OUT, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '7.2')

        CALL CDF_get_majority (ID, MAJORITY_OUT, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '7.3')

        CALL CDF_get_num_attrs (ID, NUM_ATTRS_OUT, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '7.6')

        CALL CDF_get_cachesize (ID, cacheSizeOUT, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '7.7')

C	IF (ENCODING_OUT .NE. ENCODING) CALL QUIT_CDF (STATUS, '7.10')
	IF (MAJORITY_OUT .NE. MAJORITY) CALL QUIT_CDF (STATUS, '7.11')
	IF (NUM_ATTRS_OUT .NE. 0) CALL QUIT_CDF (STATUS, '7.14')
	IF (cacheSizeOUT .NE. cacheSize) CALL QUIT_CDF (STATUS, '7.15')

C-----------------------------------------------------------------------
C Create variables.
C-----------------------------------------------------------------------

        CALL CDF_create_zvar (ID, VARNAME, VAR_DATA_TYPE,
     .			      VAR_NUM_ELEMENTS, NUM_DIMS,
     .                        DIM_SIZES, VAR_REC_VARIANCE,
     .			      VAR_DIM_VARIANCES, VAR_NUM_OUT, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '8.0')

        CALL CDF_set_zvar_compression (ID, VAR_NUM_OUT, cType, cParms, 
     .                                 STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '8.1')

        CALL CDF_set_zvar_sparserecords (ID, VAR_NUM_OUT, sRecordsType, 
     .                                   STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '8.2')

        CALL CDF_set_zvar_reservepercent (ID, VAR_NUM_OUT, cReserve, 
     .                                    STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '8.4')

        CALL CDF_create_zvar (ID, 'VAR2a', CDF_INT4, 1, NUM_DIMS,
     .                        DIM_SIZES, 
     .			      VAR_REC_VARIANCE, VAR_DIM_VARIANCES, 
     .                        varNums(1), STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '8.5')

        CALL CDF_create_zvar (ID, 'VAR2b', CDF_INT4, 1, NUM_DIMS,
     .                        DIM_SIZES, 
     .			      VAR_REC_VARIANCE, VAR_DIM_VARIANCES,
     .			      varNums(2), STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '8.6')

C-----------------------------------------------------------------------
C Alocate/inquire records for a variable.
C-----------------------------------------------------------------------

        CALL CDF_set_zvar_allocblockrecs (ID, varNums(1), 1, 10,
     .                                    STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '9.0')


        CALL CDF_get_zvar_allocrecs (ID, varNums(1), nAllocOUT, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '9.1')

	IF (nAllocOUT .NE. 10) CALL QUIT_CDF (STATUS, '9.5')

C-----------------------------------------------------------------------
C PUT to variable.
C-----------------------------------------------------------------------

	DO X2 = 1, 3
	  DO X1 = 1, 2
	    INDICES(1) = X1
	    INDICES(2) = X2
            CALL CDF_put_zvar_data (ID, CDF_var_num (ID, VARNAME), 
     .                              REC_NUM, INDICES,
     .                              VAR_VALUES(X1,X2), STATUS)
	    IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '10.0')
	  END DO
	END DO

C-----------------------------------------------------------------------
C GET from the variable.
C-----------------------------------------------------------------------

	DO X2 = 1, 3
	  DO X1 = 1, 2
	    INDICES(1) = X1
	    INDICES(2) = X2
            CALL CDF_get_zvar_data (ID, VAR_NUM_OUT, REC_NUM, INDICES,
     .			            VAR_VALUE_OUT, STATUS)
	    IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '11.0')
	    IF (VAR_VALUE_OUT .NE. VAR_VALUES(X1,X2)) THEN
	      CALL QUIT_CDF (STATUS, '11.1')
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

        CALL CDF_hyper_put_zvar_data (ID, VAR_NUM_OUT, REC_START, 
     .                                REC_COUNT, REC_INTERVAL, INDICES,
     .			              COUNTS,INTERVALS, VAR_VALUES,
     .		                      STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '12.0')

C-----------------------------------------------------------------------
C HyperGET from variable.
C-----------------------------------------------------------------------

        CALL CDF_hyper_get_zvar_data (ID, VAR_NUM_OUT, REC_START, 
     .                                REC_COUNT, REC_INTERVAL, INDICES,
     .                                COUNTS,INTERVALS, VAR_BUFFER_OUT,
     .                                STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '13.0')

	DO X2 = 1, 3
	  DO X1 = 1, 2
	    IF (VAR_BUFFER_OUT(X1,X2) .NE. VAR_VALUES(X1,X2))
     .	      CALL QUIT_CDF (STATUS, '13.1')
	  END DO
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

        CALL CDF_set_zvar_seqpos (ID, VAR_NUM_OUT, REC_NUM, INDICES,
     .		                  STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '15.0')

	DO X2 = 1, 3
	  DO X1 = 1, 2
            CALL CDF_put_zvar_seqdata (ID, VAR_NUM_OUT, 
     .                                 VAR_VALUES(X1,X2), STATUS)
	    IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '15.1')
	  END DO
	END DO

        CALL CDF_get_zvar_seqpos (ID, VAR_NUM_OUT, REC_NUM, INDICES,
     .		                  STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '15.2')

	IF (REC_NUM .NE. 2) CALL QUIT_CDF (STATUS, '15.3')
	IF (INDICES(1) .NE. 1) CALL QUIT_CDF (STATUS, '15.4')
	IF (INDICES(2) .NE. 1) CALL QUIT_CDF (STATUS, '15.5')

C-----------------------------------------------------------------------
C SEQuential ACCess GET from variable.
C-----------------------------------------------------------------------

	REC_NUM = 1
	INDICES(1) = 1
	INDICES(2) = 1

        CALL CDF_set_zvar_seqpos (ID, VAR_NUM_OUT, REC_NUM, INDICES, 
     .                            STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '16.0')

	DO X2 = 1, 3
	  DO X1 = 1, 2
            CALL CDF_get_zvar_seqdata (ID, VAR_NUM_OUT, VAR_VALUE_OUT, 
     .                                 STATUS)
	    IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '16.1')
	    IF (VAR_VALUE_OUT .NE. VAR_VALUES(X1,X2))
     .	      CALL QUIT_CDF (STATUS, '16.2')
	  END DO
	END DO

C-----------------------------------------------------------------------
C Record access.
C-----------------------------------------------------------------------

        CALL CDF_put_zvars_recorddata (ID, numVars, varNums, 
     .                                 10, recBuffer1, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '17.1')

        CALL CDF_get_zvars_recorddata (ID, numVars, varNums, 
     .                                 10, recBuffer1OUT, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '17.2')

	DO I = 1, 2
	  DO J = 1, 3
	    IF (recBuffer1OUT(I,j) .NE. recBuffer1(I,j)) THEN
	      CALL QUIT_CDF (STATUS, '17.3')
	    ENDIF
            IF (recBuffer2OUT(I,j) .NE. recBuffer2(I,j)) THEN
              CALL QUIT_CDF (STATUS, '17.4')
            ENDIF
	  END DO
	END DO

        CALL CDF_put_zvar_recorddata (ID, varNums(1),
     .                                11, recBuffer1x, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '17.5')

        CALL CDF_get_zvar_recorddata (ID, varNums(1),
     .                                11, recBuffer1xOUT, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '17.6')

        DO J = 1, 3
          IF (recBuffer1xOUT(j) .NE. recBuffer1x(j)) THEN
            CALL QUIT_CDF (STATUS, '17.7')
          ENDIF
        END DO

C-----------------------------------------------------------------------
C Create attribute.
C-----------------------------------------------------------------------

        CALL CDF_create_attr (ID, ATTRNAME, ATTRSCOPE, ATTR_NUM_OUT,
     .		              STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '18.0')

C-----------------------------------------------------------------------
C PUT to attribute.
C-----------------------------------------------------------------------

        CALL CDF_put_attr_gentry (ID, ATTR_NUM_OUT, ENTRY_NUM,
     .		                  ENTRY_DATA_TYPE,
     .				  ENTRY_NUM_ELEMENTS,
     .				  ENTRY_VALUE, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '19.0')

C-----------------------------------------------------------------------
C GET from attribute.
C-----------------------------------------------------------------------

        CALL CDF_get_attr_gentry (ID, ATTR_NUM_OUT, ENTRY_NUM, 
     .                            ENTRY_VALUE_OUT, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '20.0')

	IF (ENTRY_VALUE_OUT .NE. ENTRY_VALUE) THEN
	  CALL QUIT_CDF (STATUS, '20.1')
	ENDIF

C-----------------------------------------------------------------------
C Get CDF documentation.
C-----------------------------------------------------------------------

        CALL CDF_get_lib_version (VERSIONlib, RELEASElib, 
     .                            INCREMENTlib, SUBINCREMENTlib, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '21.0')

        CALL CDF_get_lib_copyright (COPYRIGHT_TEXTlib, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '21.1')

        CALL CDF_get_version (ID, VERSIONcdf, RELEASEcdf, INCREMENTcdf,
     .                        STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '21.2')

        CALL CDF_get_copyright (ID, COPYRIGHT_TEXTcdf, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '21.3')

	IF (VERSIONcdf .NE. VERSIONlib) CALL QUIT_CDF (STATUS, '21.4')
	IF (RELEASEcdf .NE. RELEASElib) CALL QUIT_CDF (STATUS, '21.5')
	IF (INCREMENTcdf .NE. INCREMENTlib)
     .	  CALL QUIT_CDF (STATUS, '21.6')
	IF (COPYRIGHT_TEXTcdf .NE. COPYRIGHT_TEXTlib)
     .	  CALL QUIT_CDF (STATUS, '21.7')

C-----------------------------------------------------------------------
C Inquire CDF.
C-----------------------------------------------------------------------

        CALL CDF_inquire_cdf (ID, NUM_DIMS_OUT, DIM_SIZES_OUT, 
     .                        ENCODING_OUT,
     .			      MAJORITY_OUT, MAX_RREC_OUT, NUM_RVARS_OUT,
     .                        MAX_ZREC_OUT, NUM_ZVARS_OUT,
     .			      NUM_ATTRS_OUT, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '22.0')

        CALL CDF_get_compression (ID, cTypeOUT, cParmsOUT, cPctOUT,
     .		                  STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '22.1')

	IF (NUM_DIMS_OUT .NE. 0)
     .	  CALL QUIT_CDF (STATUS, '22.2')

	IF (NUM_DIMS_OUT .GT. 0) THEN
	  DO X = 1, NUM_DIMS_OUT
	    IF (DIM_SIZES_OUT(X) .NE. DIM_SIZES(X)) THEN
	      CALL QUIT_CDF (STATUS, '22.3')
	    ENDIF
	  END DO
	END IF

C	IF (ENCODING_OUT .NE. ENCODING) CALL QUIT_CDF (STATUS, '22.4')
	IF (MAJORITY_OUT .NE. MAJORITY) CALL QUIT_CDF (STATUS, '22.5')
	IF (MAX_ZREC_OUT .NE. 11) CALL QUIT_CDF (STATUS, '22.6')
	IF (NUM_ZVARS_OUT .NE. 3) CALL QUIT_CDF (STATUS, '22.7')
	IF (NUM_ATTRS_OUT .NE. 1) CALL QUIT_CDF (STATUS, '22.8')
	IF (cTypeOUT .NE. cType) CALL QUIT_CDF (STATUS, '22.9')

C-----------------------------------------------------------------------
C Rename variable.
C-----------------------------------------------------------------------

        VAR_NUM_OUT = CDF_get_var_num (ID, VARNAME)
        IF (VAR_NUM_OUT .LT. CDF_OK) CALL QUIT_CDF (STATUS, '23.0')

        CALL CDF_rename_zvar (ID, VAR_NUM_OUT, NEW_VARNAME, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '23.1')

C-----------------------------------------------------------------------
C Inquire variable.
C-----------------------------------------------------------------------

        CALL CDF_get_zvar_name (ID, VAR_NUM_OUT, VARNAME_OUT, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '24.0')
        
        CALL CDF_get_zvar_datatype (ID, VAR_NUM_OUT, VAR_DATA_TYPE_OUT,
     .                              STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '24.1')

        CALL CDF_get_zvar_numelems (ID, VAR_NUM_OUT, 
     .                              VAR_NUM_ELEMENTS_OUT, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '24.2')

        CALL CDF_get_zvar_compression (ID, VAR_NUM_OUT, cTypeOUT, 
     .                                 cParmsOUT, cPctOUT, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '24.3')

        CALL CDF_get_zvar_reservepercent (ID, VAR_NUM_OUT, cReserveOUT,
     .                                    STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '24.4')

        CALL CDF_get_zvar_sparserecords (ID, VAR_NUM_OUT, 
     .                                   sRecordsTypeOUT, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '24.5')

        CALL CDF_get_zvar_numrecs_written (ID, VAR_NUM_OUT, nRecordsOUT, 
     .                                     STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '24.7')

        CALL CDF_get_zvar_recvariance (ID, VAR_NUM_OUT, 
     .                                 VAR_REC_VARIANCE_OUT, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '24.8')

        CALL CDF_get_zvar_dimvariances (ID, VAR_NUM_OUT,
     .			                VAR_DIM_VARIANCES_OUT, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '24.9')

	IF (VARNAME_OUT .NE. NEW_VARNAME(1:4)) THEN
	  CALL QUIT_CDF (STATUS, '24.10')
	ENDIF
	IF (VAR_DATA_TYPE_OUT .NE. VAR_DATA_TYPE) THEN
	  CALL QUIT_CDF (STATUS, '24.11')
	ENDIF
	IF (VAR_NUM_ELEMENTS_OUT .NE. VAR_NUM_ELEMENTS) THEN
	  CALL QUIT_CDF (STATUS, '24.12')
	ENDIF
	IF (VAR_REC_VARIANCE_OUT .NE. VAR_REC_VARIANCE) THEN
	  CALL QUIT_CDF (STATUS, '24.13')
	ENDIF
	DO X = 1, 2
	  IF (VAR_DIM_VARIANCES_OUT(X) .NE. VAR_DIM_VARIANCES(X)) THEN
	    CALL QUIT_CDF (STATUS, '24.14')
	  ENDIF
	END DO
	IF (cTypeOUT .NE. cType) CALL QUIT_CDF (STATUS, '24.15')
	IF (sRecordsTypeOUT .NE. sRecordsType) THEN
	  CALL QUIT_CDF (STATUS, '24.16')
	ENDIF
	IF (nRecordsOUT .NE. 1) CALL QUIT_CDF (STATUS, '24.18')
	IF (cReserveOUT .NE. cReserve) CALL QUIT_CDF (STATUS, '24.19')

C-----------------------------------------------------------------------
C Rename attribute.
C-----------------------------------------------------------------------

        ATTR_NUM_OUT = CDF_get_attr_num (ID, ATTRNAME)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '25.0')

        CALL CDF_rename_attr (ID, ATTR_NUM_OUT, NEW_ATTRNAME, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '25.1')

C-----------------------------------------------------------------------
C Inquire attribute.
C-----------------------------------------------------------------------

        CALL CDF_get_attr_name (ID, ATTR_NUM_OUT, ATTRNAME_OUT, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '26.0')

        CALL CDF_get_attr_scope (ID, ATTR_NUM_OUT, ATTRSCOPE_OUT, 
     .                           STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '26.1')
        CALL CDF_get_attr_num_gentries (ID, ATTR_NUM_OUT, 
     .                                  NUM_ENTRIES_OUT, STATUS) 
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '26.2')

	IF (ATTRNAME_OUT .NE. NEW_ATTRNAME(1:5))
     .	  CALL QUIT_CDF (STATUS, '26.3')
	IF (ATTRSCOPE_OUT .NE. ATTRSCOPE) CALL QUIT_CDF (STATUS, '26.4')
	IF (NUM_ENTRIES_OUT .NE. 1) CALL QUIT_CDF (STATUS, '26.5')

C-----------------------------------------------------------------------
C Inquire attribute entry.
C-----------------------------------------------------------------------

        CALL CDF_get_attr_gentry_datatype (ID, ATTR_NUM_OUT, ENTRY_NUM,
     .                                     ENTRY_DATA_TYPE_OUT, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '27.0')

        CALL CDF_get_attr_gentry_numelems (ID, ATTR_NUM_OUT, ENTRY_NUM,
     .			                   ENTRY_NUM_ELEMENTS_OUT, 
     .                                     STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '27.1')

	IF (ENTRY_DATA_TYPE_OUT .NE. ENTRY_DATA_TYPE)
     .	  CALL QUIT_CDF (STATUS, '27.2')

	IF (ENTRY_NUM_ELEMENTS_OUT .NE. ENTRY_NUM_ELEMENTS)
     .	  CALL QUIT_CDF (STATUS, '27.3')

C-----------------------------------------------------------------------
C Get error text.
C-----------------------------------------------------------------------

        CALL CDF_get_status_text (CDF_OK, ERRORTEXT, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '28.0')

C-----------------------------------------------------------------------
C Set and confirm CDF modes, decoding, etc.
C-----------------------------------------------------------------------

        CALL CDF_set_negtoposfp0_mode (ID, NEGtoPOSfp0on, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '29.0')

        CALL CDF_set_readonly_mode (ID, READONLYon, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '29.1')

        CALL CDF_set_decoding (ID, MAC_DECODING, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '29.2')

        CALL CDF_get_negtoposfp0_mode (ID, NEGtoPOSmode, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '29.3')

        CALL CDF_get_readonly_mode (ID, READONLYmode, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '29.4')

        CALL CDF_get_decoding (ID, decodingOut, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '29.5')

        CALL CDF_get_name (ID, nameOut, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '29.6')

	IF (NEGtoPOSmode .NE. NEGtoPOSfp0on)
     .    CALL QUIT_CDF (STATUS, '29.7')
C	IF (READONLYmode .NE. READONLYon) CALL QUIT_CDF (STATUS, '29.8')
	IF (decodingOut .NE. MAC_DECODING)
     .	  CALL QUIT_CDF (STATUS, '29.9')
	IF (nameOut(1:4) .NE. CDFNAME(1:4))
     .	  CALL QUIT_CDF (STATUS, '29.10')

C-----------------------------------------------------------------------
C Inquire size of a data type.
C-----------------------------------------------------------------------

        CALL CDF_get_datatype_size (CDF_EPOCH, datatype_size, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '30.0')

	IF (datatype_size .ne. 8) CALL QUIT_CDF (STATUS, '30.1')

C-----------------------------------------------------------------------
C Close CDF.
C-----------------------------------------------------------------------

        CALL CDF_close (ID, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '31.0')

C-----------------------------------------------------------------------
C Get CDF information.
C-----------------------------------------------------------------------

        CALL CDF_get_compression_info (CDFNAME, cTypeOUT, cParmsOUT, 
     .			               cSizeOUT, uSizeOUT, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '32.0')

	IF (cTypeOUT .NE. cType) CALL QUIT_CDF (STATUS, '32.1')


C-----------------------------------------------------------------------

	END

C-----------------------------------------------------------------------
C QUIT_CDF.  Abort test early due to CDF error.
C-----------------------------------------------------------------------

	SUBROUTINE QUIT_CDF (STATUS, WHERE)
	INCLUDE 'CDF.INC'
	INTEGER*4 STATUS
	CHARACTER WHERE*(*)
	CHARACTER TEXT*(CDF_ERRTEXT_LEN)
	INTEGER*4 STATUS2
	WRITE (6,401) WHERE
 401	FORMAT (' ', 'Aborting at ', A, '...')
	IF (STATUS .LT. 0) THEN
	  CALL CDF_ERROR (STATUS, TEXT, STATUS2)
	  WRITE (6,501) TEXT
 501	  FORMAT (' ', 'ERROR: ', A)
	ENDIF
	WRITE (6,404)
 404	FORMAT (' ','...test aborted')
	STOP
	END
