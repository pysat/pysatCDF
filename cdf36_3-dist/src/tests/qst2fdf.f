	PROGRAM QST2F
C-----------------------------------------------------------------------
C Copyright 1996-2014 United States Government as represented by the
C Administrator of the National Aeronautics and Space Administration.
C All Rights Reserved.
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C
C  NSSDC/CDF		  Quick Start Test Program (FORTRAN interface).
C
C  Version 1.11, 10-Sep-96, Hughes STX.
C
C  Modification history:
C
C   V1.0  24-Jan-91, J Love	Original version (for CDF V2.0).
C   V1.1  11-Jan-91, J Love	Fixed problem with "max_rec" from
C				CDF_INQUIRE and added CDF_INQUIRE
C				call after CDF_CREATE.
C   V1.2   7-Mar-91, J Love	Modified output display.
C   V1.3  27-May-91, J Love	Changed for CDF V2.1 enhancements.
C   V1.4  25-Jun-91, J Love	Renamed CDF for portability.
C   V1.5  16-Jul-91, J Love	Declaration of CDF_var_num and
C				CDF_attr_num now in 'cdf.inc'.
C   V1.6  26-Sep-91, J Love	Modified for IBM-RS6000 port.
C   V1.7  20-May-92, J Love	CDF V2.2.
C   V1.8   9-Aug-93, J Love	CDF V2.4.
C   V1.8a 22-Feb-94, J Love	Limited lines to 72 columns or less.
C   V1.9  22-Jun-94, J Love	Renamed CDF to `TEST'.
C   V1.10 20-Dec-94, J Love	CDF V2.5.
C   V1.11 10-Sep-96, J Love	CDF V2.6.
C   V1.12 27-May-05, J Liu	CDF V3.1.
C
C-----------------------------------------------------------------------
        INCLUDE 'CDFDF.INC'
        INCLUDE 'CDFDVF.INC'
        INCLUDE 'CDFDVF2.INC'
        INCLUDE 'CDFDVF3.INC'

	INTEGER*4 N_DIMS
	PARAMETER (N_DIMS = 2)

	INTEGER*4 DIM_1_SIZE
	PARAMETER (DIM_1_SIZE = 2)
	INTEGER*4 DIM_2_SIZE
	PARAMETER (DIM_2_SIZE = 3)

	INTEGER*4 CDF_ID
	INTEGER*4 STATUS
	INTEGER*4 ENCODING
	INTEGER*4 MAJORITY
	INTEGER*4 NUM_DIMS
	INTEGER*4 DIM_SIZES(N_DIMS)
	INTEGER*4 VAR_DATA_TYPE
	INTEGER*4 VAR_DATA_TYPE_OUT
	INTEGER*4 VAR_NUM_ELEMENTS
	INTEGER*4 VAR_NUM_ELEMENTS_OUT
	INTEGER*4 VAR_NUM_OUT
	INTEGER*4 VAR_VALUES(DIM_1_SIZE,DIM_2_SIZE)
	INTEGER*4 INDICES(N_DIMS)
	INTEGER*4 REC_NUM
	INTEGER*4 VAR_VALUE_OUT
	INTEGER*4 REC_START
	INTEGER*4 REC_COUNT
	INTEGER*4 REC_INTERVAL
	INTEGER*4 COUNTS(N_DIMS)
	INTEGER*4 INTERVALS(N_DIMS)
	INTEGER*4 VAR_BUFFER_OUT(DIM_1_SIZE,DIM_2_SIZE)
	INTEGER*4 ATTR_NUM_OUT
	INTEGER*4 ENTRY_NUM
	INTEGER*4 NUM_ENTRIES_OUT
	INTEGER*4 ATTRSCOPE
	INTEGER*4 ATTRSCOPE_OUT
	INTEGER*4 ENTRY_DATA_TYPE
	INTEGER*4 ENTRY_DATA_TYPE_OUT
	INTEGER*4 ENTRY_NUM_ELEMENTS
	INTEGER*4 ENTRY_NUM_ELEMENTS_OUT
	INTEGER*4 ENTRY_VALUE
	INTEGER*4 ENTRY_VALUE_OUT
	INTEGER*4 ENCODING_OUT
	INTEGER*4 MAJORITY_OUT
	INTEGER*4 NUM_DIMS_OUT
	INTEGER*4 DIM_SIZES_OUT(N_DIMS)
	INTEGER*4 MAX_REC_OUT
	INTEGER*4 NUM_VARS_OUT
	INTEGER*4 NUM_ATTRS_OUT
	INTEGER*4 RELEASE
	INTEGER*4 VERSION
	INTEGER*4 START
	INTEGER*4 I
	INTEGER*4 LAST_CHAR
C					! last character in "copyright"
C					! (before padding blanks begin)

	INTEGER*4 X1, X2, X

	INTEGER*4 VAR_REC_VARIANCE
	INTEGER*4 VAR_REC_VARIANCE_OUT
	INTEGER*4 VAR_DIM_VARIANCES(N_DIMS)
	INTEGER*4 VAR_DIM_VARIANCES_OUT(N_DIMS)

	CHARACTER VARNAME*(CDF_VAR_NAME_LEN)
	CHARACTER NEW_VARNAME*(CDF_VAR_NAME_LEN)
	CHARACTER VARNAME_OUT*(CDF_VAR_NAME_LEN)
	CHARACTER ATTRNAME*(CDF_ATTR_NAME_LEN)
	CHARACTER NEW_ATTRNAME*(CDF_ATTR_NAME_LEN)
	CHARACTER ATTRNAME_OUT*(CDF_ATTR_NAME_LEN)
	CHARACTER COPYRIGHT_TEXT*(CDF_COPYRIGHT_LEN)
	CHARACTER ERRORTEXT*(CDF_STATUSTEXT_LEN)
	CHARACTER CDFNAME*(CDF_PATHNAME_LEN)

	CHARACTER LF*1

	INTEGER*4 YEAR, MONTH, DAY, HOUR, MINUTE, SECOND, MSEC
	INTEGER*4 YEAR_OUT, MONTH_OUT, DAY_OUT,
     .		  HOUR_OUT, MINUTE_OUT, SECOND_OUT, MSEC_OUT
	REAL*8    EPOCH, EPOCH_OUT
	CHARACTER EPSTRING*(EPOCH_STRING_LEN),
     .		  EPSTRING1*(EPOCH1_STRING_LEN),
     .		  EPSTRING2*(EPOCH2_STRING_LEN),
     .		  EPSTRING3*(EPOCH3_STRING_LEN)
	CHARACTER EPSTRING_TRUE*(EPOCH_STRING_LEN),
     .		  EPSTRING1_TRUE*(EPOCH1_STRING_LEN),
     .		  EPSTRING2_TRUE*(EPOCH2_STRING_LEN),
     .		  EPSTRING3_TRUE*(EPOCH3_STRING_LEN)

	DATA ENCODING/IBMPC_ENCODING/
	DATA MAJORITY/COL_MAJOR/
	DATA NUM_DIMS/N_DIMS/
	DATA DIM_SIZES/DIM_1_SIZE,DIM_2_SIZE/
	DATA VAR_DATA_TYPE/CDF_INT4/
	DATA VAR_NUM_ELEMENTS/1/
	DATA VAR_REC_VARIANCE/VARY/
	DATA VAR_DIM_VARIANCES/N_DIMS * VARY/
	DATA REC_NUM/1/
	DATA VAR_VALUES/1,2,3,4,5,6/
	DATA REC_START/1/
	DATA REC_COUNT/1/
	DATA REC_INTERVAL/1/
	DATA COUNTS/DIM_1_SIZE,DIM_2_SIZE/
	DATA INTERVALS/N_DIMS * 1/
	DATA ENTRY_NUM/1/
	DATA ATTRSCOPE/GLOBAL_SCOPE/
	DATA ENTRY_DATA_TYPE/CDF_INT4/
	DATA ENTRY_NUM_ELEMENTS/1/
	DATA ENTRY_VALUE/1/

	DATA CDFNAME(1:4)/'TEST'/
	DATA VARNAME(1:4)/'VAR1'/
	DATA NEW_VARNAME(1:4)/'VAR2'/
	DATA ATTRNAME(1:5)/'ATTR1'/
	DATA NEW_ATTRNAME(1:5)/'ATTR2'/

	DATA YEAR/1994/, MONTH/10/, DAY/13/,
     .	     HOUR/12/, MINUTE/0/, SECOND/0/, MSEC/0/
	DATA EPSTRING_TRUE/'13-Oct-1994 12:00:00.000'/,
     .	     EPSTRING1_TRUE/'19941013.5000000'/,
     .	     EPSTRING2_TRUE/'19941013120000'/,
     .	     EPSTRING3_TRUE/'1994-10-13T12:00:00.000Z'/

C-----------------------------------------------------------------------
C  NUL-terminate character strings.
C-----------------------------------------------------------------------

	CDFNAME(5:5) = CHAR(0)
	VARNAME(5:5) = CHAR(0)
	NEW_VARNAME(5:5) = CHAR(0)
	ATTRNAME(6:6) = CHAR(0)
	NEW_ATTRNAME(6:6) = CHAR(0)

C-----------------------------------------------------------------------
C  Display test title.
C-----------------------------------------------------------------------

	WRITE (6,100)
 100	FORMAT (' ','Testing Standard/FORTRAN interface...')

C-----------------------------------------------------------------------
C  Create CDF.
C-----------------------------------------------------------------------

        CALL CDF_CREATE (CDFNAME, NUM_DIMS, DIM_SIZES, ENCODING,
     .			 MAJORITY, CDF_ID, STATUS)

	IF (STATUS .LT. CDF_OK) THEN
	  IF (STATUS .EQ. CDF_EXISTS) THEN
            CALL CDF_OPEN (CDFNAME, CDF_ID, STATUS)
	    IF (STATUS .LT. CDF_OK)
     .	      CALL QUIT_CDF (STATUS, '1.0')

            CALL CDF_DELETE (CDF_ID, STATUS)
	    IF (STATUS .LT. CDF_OK)
     .	      CALL QUIT_CDF (STATUS, '1.1')

            CALL CDF_CREATE (CDFNAME, NUM_DIMS, DIM_SIZES,
     .			     ENCODING, MAJORITY, CDF_ID,
     .			     STATUS)
	    IF (STATUS .LT. CDF_OK)
     .	      CALL QUIT_CDF (STATUS, '1.2')
	   ELSE
	    CALL QUIT_CDF (STATUS, '1.3')
	  END IF
	END IF

C-----------------------------------------------------------------------
C  Create variable.
C-----------------------------------------------------------------------
        CALL CDF_VAR_CREATE (CDF_ID, VARNAME, VAR_DATA_TYPE,
     .			     VAR_NUM_ELEMENTS, VAR_REC_VARIANCE,
     .			     VAR_DIM_VARIANCES, VAR_NUM_OUT,
     .			     STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '2.0')

C-----------------------------------------------------------------------
C  Close CDF.
C-----------------------------------------------------------------------

        CALL CDF_CLOSE (CDF_ID, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '3.0')

C-----------------------------------------------------------------------
C  Reopen CDF.
C-----------------------------------------------------------------------

        CALL CDF_OPEN (CDFNAME, CDF_ID, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '4.0')

C-----------------------------------------------------------------------
C  Delete CDF.
C-----------------------------------------------------------------------

        CALL CDF_DELETE (CDF_ID, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '5.0')

C-----------------------------------------------------------------------
C  Create CDF again (previous delete will allow this).
C-----------------------------------------------------------------------

        CALL CDF_CREATE (CDFNAME, NUM_DIMS, DIM_SIZES, ENCODING,
     .			 MAJORITY, CDF_ID, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '6.0')

C-----------------------------------------------------------------------
C  Inquire CDF (added for V1.1).
C-----------------------------------------------------------------------

        CALL CDF_INQUIRE (CDF_ID, NUM_DIMS_OUT, DIM_SIZES_OUT,
     .			  ENCODING_OUT, MAJORITY_OUT,
     .			  MAX_REC_OUT, NUM_VARS_OUT,
     .			  NUM_ATTRS_OUT, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '6a.0')

	IF (NUM_DIMS_OUT .NE. NUM_DIMS)
     .	  CALL QUIT_CDF (STATUS, '6a.1')

	DO X = 1, N_DIMS
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

C-----------------------------------------------------------------------
C  Create variable.
C-----------------------------------------------------------------------

        CALL CDF_VAR_CREATE (CDF_ID, VARNAME, VAR_DATA_TYPE,
     .			     VAR_NUM_ELEMENTS, VAR_REC_VARIANCE,
     .			     VAR_DIM_VARIANCES, VAR_NUM_OUT,
     .			     STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '7.0')

C-----------------------------------------------------------------------
C  PUT to variable.
C-----------------------------------------------------------------------

	DO X1 = 1, DIM_1_SIZE
	  DO X2 = 1, DIM_2_SIZE
	    INDICES(1) = X1
	    INDICES(2) = X2
            CALL CDF_VAR_PUT (CDF_ID, CDF_VAR_NUM(CDF_ID,VARNAME),
     .			      REC_NUM, INDICES, VAR_VALUES(X1,X2),
     .			      STATUS)
	    IF (STATUS .LT. CDF_OK)
     .	      CALL QUIT_CDF (STATUS, '8.0')
	  END DO
	END DO

C-----------------------------------------------------------------------
C  GET from the variable.
C-----------------------------------------------------------------------

	DO X1 = 1, DIM_1_SIZE
	  DO X2 = 1, DIM_2_SIZE
	    INDICES(1) = X1
	    INDICES(2) = X2
            CALL CDF_VAR_GET (CDF_ID, CDF_VAR_NUM(CDF_ID,VARNAME),
     .			      REC_NUM, INDICES, VAR_VALUE_OUT,
     .			      STATUS)
	    IF (STATUS .LT. CDF_OK)
     .	      CALL QUIT_CDF (STATUS, '9.0')

	    IF (VAR_VALUE_OUT .NE. VAR_VALUES(X1,X2))
     .	      CALL QUIT_CDF (STATUS, '9.1')
	  END DO
	END DO

C-----------------------------------------------------------------------
C  HyperPUT to the variable.
C-----------------------------------------------------------------------

	DO X1 = 1, DIM_1_SIZE
	  DO X2 = 1, DIM_2_SIZE
	    VAR_VALUES(X1,X2) = -VAR_VALUES(X1,X2)
	  END DO
	END DO

	INDICES(1) = 1
	INDICES(2) = 1

        CALL CDF_VAR_HYPER_PUT (CDF_ID, 
     .                          CDF_VAR_NUM(CDF_ID,VARNAME),
     .				REC_START, REC_COUNT, REC_INTERVAL,
     .				INDICES, COUNTS, INTERVALS,
     .				VAR_VALUES, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '10.0')

C-----------------------------------------------------------------------
C  HyperGET from variable.
C-----------------------------------------------------------------------

        CALL CDF_VAR_HYPER_GET (CDF_ID, 
     .                          CDF_VAR_NUM(CDF_ID,VARNAME),
     .				REC_START, REC_COUNT, REC_INTERVAL,
     .				INDICES, COUNTS, INTERVALS,
     .				VAR_BUFFER_OUT, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '11.0')

	DO X1 = 1, DIM_1_SIZE
	  DO X2 = 1, DIM_2_SIZE
	    IF (VAR_BUFFER_OUT(X1,X2) .NE. VAR_VALUES(X1,X2))
     .	      CALL QUIT_CDF (STATUS, '11.1')
	  END DO
	END DO

C-----------------------------------------------------------------------
C  Create attribute.
C-----------------------------------------------------------------------

        CALL CDF_ATTR_CREATE (CDF_ID, ATTRNAME, ATTRSCOPE,
     .			      ATTR_NUM_OUT, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '12.0')

C-----------------------------------------------------------------------
C  PUT to attribute.
C-----------------------------------------------------------------------

        CALL CDF_ATTR_PUT (CDF_ID, CDF_ATTR_NUM(CDF_ID,ATTRNAME),
     .			   ENTRY_NUM, ENTRY_DATA_TYPE,
     .			   ENTRY_NUM_ELEMENTS, ENTRY_VALUE,
     .			   STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '13.0')

C-----------------------------------------------------------------------
C  GET from attribute.
C-----------------------------------------------------------------------

        CALL CDF_ATTR_GET (CDF_ID, CDF_ATTR_NUM(CDF_ID,ATTRNAME),
     .			   ENTRY_NUM, ENTRY_VALUE_OUT,
     .			   STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '14.0')

	IF (ENTRY_VALUE_OUT .NE. ENTRY_VALUE)
     .	  CALL QUIT_CDF (STATUS, '14.1')

C-----------------------------------------------------------------------
C  Get CDF documentation.
C-----------------------------------------------------------------------

        CALL CDF_DOC (CDF_ID, VERSION, RELEASE,
     .		      COPYRIGHT_TEXT, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '15.0')

C	WRITE (6,101) VERSION, RELEASE
C 101	FORMAT (' ','CDF V',I1,'.',I1)
C							! V1.2
	LAST_CHAR = CDF_COPYRIGHT_LEN
	DO WHILE (COPYRIGHT_TEXT(LAST_CHAR:LAST_CHAR) .EQ. ' ')
	  LAST_CHAR = LAST_CHAR - 1
	END DO

	LF = CHAR(10)

	START = 1
	DO I = 1, LAST_CHAR
	  IF (COPYRIGHT_TEXT(I:I) .EQ. LF) THEN
C	    WRITE (6,301) COPYRIGHT_TEXT(START:I-1)
C 301	    FORMAT (' ',A)
	    START = I + 1
	  END IF
	END DO

C-----------------------------------------------------------------------
C  Inquire CDF.
C-----------------------------------------------------------------------

        CALL CDF_INQUIRE (CDF_ID, NUM_DIMS_OUT, DIM_SIZES_OUT,
     .			  ENCODING_OUT, MAJORITY_OUT,
     .			  MAX_REC_OUT, NUM_VARS_OUT,
     .			  NUM_ATTRS_OUT, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '16.0')

	IF (NUM_DIMS_OUT .NE. NUM_DIMS)
     .	  CALL QUIT_CDF (STATUS, '16.1')

	DO X = 1, N_DIMS
	  IF (DIM_SIZES_OUT(X) .NE. DIM_SIZES(X))
     .	    CALL QUIT_CDF (STATUS, '16.2')
	END DO

	IF (ENCODING_OUT .NE. ENCODING)
     .	  CALL QUIT_CDF (STATUS, '16.3')
	IF (MAJORITY_OUT .NE. MAJORITY)
     .	  CALL QUIT_CDF (STATUS, '16.4')
	IF (MAX_REC_OUT .NE. 1) CALL QUIT_CDF (STATUS, '16.5')
C								! V1.1
	IF (NUM_VARS_OUT .NE. 1) CALL QUIT_CDF (STATUS, '16.6')
	IF (NUM_ATTRS_OUT .NE. 1) CALL QUIT_CDF (STATUS, '16.7')

C-----------------------------------------------------------------------
C  Rename variable.
C-----------------------------------------------------------------------

        CALL CDF_VAR_RENAME (CDF_ID, CDF_VAR_NUM(CDF_ID,VARNAME),
     .			     NEW_VARNAME, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '17.0')

C-----------------------------------------------------------------------
C  Inquire variable.
C-----------------------------------------------------------------------

        CALL CDF_VAR_INQUIRE (CDF_ID, 
     .                        CDF_VAR_NUM(CDF_ID,NEW_VARNAME),
     .			      VARNAME_OUT, VAR_DATA_TYPE_OUT,
     .			      VAR_NUM_ELEMENTS_OUT,
     .			      VAR_REC_VARIANCE_OUT,
     .			      VAR_DIM_VARIANCES_OUT, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '18.0')

	IF (VARNAME_OUT .NE. NEW_VARNAME(1:4))
     .	  CALL QUIT_CDF (STATUS, '18.1')
	IF (VAR_DATA_TYPE_OUT .NE. VAR_DATA_TYPE)
     .	  CALL QUIT_CDF (STATUS, '18.2')
	IF (VAR_NUM_ELEMENTS_OUT .NE. VAR_NUM_ELEMENTS)
     .	  CALL QUIT_CDF (STATUS, '18.3')
	IF (VAR_REC_VARIANCE_OUT .NE. VAR_REC_VARIANCE)
     .	  CALL QUIT_CDF (STATUS, '18.4')

	DO X = 1, N_DIMS
	  IF (VAR_DIM_VARIANCES_OUT(X) .NE. VAR_DIM_VARIANCES(X))
     .	    CALL QUIT_CDF (STATUS, '18.5')
	END DO

C-----------------------------------------------------------------------
C  Close variable.
C-----------------------------------------------------------------------

        CALL CDF_VAR_CLOSE (CDF_ID, 
     .                      CDF_VAR_NUM(CDF_ID,NEW_VARNAME),
     .			    STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '19.0')

C-----------------------------------------------------------------------
C  Rename attribute.
C-----------------------------------------------------------------------

        CALL CDF_ATTR_RENAME (CDF_ID, 
     .                        CDF_ATTR_NUM(CDF_ID,ATTRNAME),
     .			      NEW_ATTRNAME, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '20.0')

C-----------------------------------------------------------------------
C  Inquire attribute.
C-----------------------------------------------------------------------

        CALL CDF_ATTR_INQUIRE (CDF_ID,
     .                         CDF_ATTR_NUM(CDF_ID,NEW_ATTRNAME),
     .			       ATTRNAME_OUT, ATTRSCOPE_OUT,
     .			       NUM_ENTRIES_OUT, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '22.0')

	IF (ATTRNAME_OUT .NE. NEW_ATTRNAME(1:5))
     .	  CALL QUIT_CDF (STATUS, '22.1')
	IF (ATTRSCOPE_OUT .NE. ATTRSCOPE)
     .	  CALL QUIT_CDF (STATUS, '22.2')
	IF (NUM_ENTRIES_OUT .NE. 1)
     .	  CALL QUIT_CDF (STATUS, '22.3')

C-----------------------------------------------------------------------
C  Inquire attribute entry.
C-----------------------------------------------------------------------

        CALL CDF_ATTR_ENTRY_INQUIRE (CDF_ID,
     .                              CDF_ATTR_NUM(CDF_ID,NEW_ATTRNAME),
     .				     ENTRY_NUM,
     .				     ENTRY_DATA_TYPE_OUT,
     .				     ENTRY_NUM_ELEMENTS_OUT,
     .				     STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '23.0')

	IF (ENTRY_DATA_TYPE_OUT .NE. ENTRY_DATA_TYPE)
     .	  CALL QUIT_CDF (STATUS, '23.1')
	IF (ENTRY_NUM_ELEMENTS_OUT .NE. ENTRY_NUM_ELEMENTS)
     .	  CALL QUIT_CDF (STATUS, '23.2')

C-----------------------------------------------------------------------
C  Get error text.
C-----------------------------------------------------------------------

        CALL CDF_ERROR (CDF_OK, ERRORTEXT, STATUS)

	LAST_CHAR = CDF_ERRTEXT_LEN
	DO WHILE (ERRORTEXT(LAST_CHAR:LAST_CHAR) .EQ. ' ')
	  LAST_CHAR = LAST_CHAR - 1
	END DO

C	WRITE (6,103) ERRORTEXT(1:LAST_CHAR)
C 103	FORMAT (/,' ',A,/)

C-----------------------------------------------------------------------
C  Close CDF.
C-----------------------------------------------------------------------

        CALL CDF_CLOSE (CDF_ID, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '24.0')

C-----------------------------------------------------------------------
C  Test EPOCH routines.
C-----------------------------------------------------------------------

	CALL COMPUTE_EPOCH (YEAR, MONTH, DAY, HOUR, MINUTE, SECOND,
     .			    MSEC, EPOCH)

	CALL ENCODE_EPOCH (EPOCH, EPSTRING)
	IF (EPSTRING .NE. EPSTRING_TRUE) CALL QUIT_EPOCH ('30.0')

	CALL PARSE_EPOCH (EPSTRING, EPOCH_OUT)
	IF (EPOCH_OUT .NE. EPOCH) CALL QUIT_EPOCH ('30.1')

	CALL ENCODE_EPOCH1 (EPOCH, EPSTRING1)
	IF (EPSTRING1 .NE. EPSTRING1_TRUE) CALL QUIT_EPOCH ('30.2')

	CALL PARSE_EPOCH1 (EPSTRING1, EPOCH_OUT)
	IF (EPOCH_OUT .NE. EPOCH) CALL QUIT_EPOCH ('30.3')

	CALL ENCODE_EPOCH2 (EPOCH, EPSTRING2)
	IF (EPSTRING2 .NE. EPSTRING2_TRUE) CALL QUIT_EPOCH ('30.4')

	CALL PARSE_EPOCH2 (EPSTRING2, EPOCH_OUT)
	IF (EPOCH_OUT .NE. EPOCH) CALL QUIT_EPOCH ('30.5')

	CALL ENCODE_EPOCH3 (EPOCH, EPSTRING3)
	IF (EPSTRING3 .NE. EPSTRING3_TRUE) CALL QUIT_EPOCH ('30.6')

	CALL PARSE_EPOCH3 (EPSTRING3, EPOCH_OUT)
	IF (EPOCH_OUT .NE. EPOCH) CALL QUIT_EPOCH ('30.7')

	CALL EPOCH_BREAKDOWN (EPOCH, YEAR_OUT, MONTH_OUT, DAY_OUT,
     .			      HOUR_OUT, MINUTE_OUT, SECOND_OUT,
     .			      MSEC_OUT)
	IF (YEAR_OUT .NE. YEAR) CALL QUIT_EPOCH ('32.1')
	IF (MONTH_OUT .NE. MONTH) CALL QUIT_EPOCH ('32.2')
	IF (DAY_OUT .NE. DAY) CALL QUIT_EPOCH ('32.3')
	IF (HOUR_OUT .NE. HOUR) CALL QUIT_EPOCH ('32.4')
	IF (MINUTE_OUT .NE. MINUTE) CALL QUIT_EPOCH ('32.5')
	IF (SECOND_OUT .NE. SECOND) CALL QUIT_EPOCH ('32.6')
	IF (MSEC_OUT .NE. MSEC) CALL QUIT_EPOCH ('32.7')

C-----------------------------------------------------------------------

	END

C-----------------------------------------------------------------------
C  QUIT_CDF.  Abort test early due to CDF error.
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

C-----------------------------------------------------------------------
C  QUIT_EPOCH.  Abort test early due to EPOCH error.
C-----------------------------------------------------------------------

	SUBROUTINE QUIT_EPOCH (WHERE)
	CHARACTER WHERE*(*)
	WRITE (6,402) WHERE
 402	FORMAT (' ', 'Aborting at ', A, '...test aborted')
	STOP
	END
