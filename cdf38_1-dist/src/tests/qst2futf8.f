	PROGRAM QST2UTF8
C
C  Test writing three UTF8 strings into a global attribute.
C

      INCLUDE 'CDF.INC'
		
      INTEGER*4 ID, STATUS, ATTR_NUM, dimsizes(1), len1, len2, len3

      data dimsizes/0/

      character(len = 50)::spdf1
      character(len = 50)::spdf2
      character(len = 50)::spdf3
C  This character statements are for 2003 Fortran
C     character(len = :), allocatable ::spdf1o
C     character(len = :), allocatable ::spdf2o
C     character(len = :), allocatable ::spdf3o
C  This character statements are for older Fortran
      character(len = 14) ::spdf1o
      character(len = 28) ::spdf2o
      character(len = 15) ::spdf3o

      WRITE (6,100)
100   FORMAT (' ','Testing UTF8/FORTRAN...')

      spdf1 = 'ASCII: ABCDEFG'
      spdf2 = 'Latin1: ©æêü÷Æ¼®¢¥'
      spdf3 = 'Chinese: 社安'

      STATUS = CDF_LIB (CREATE_, CDF_, 'TUTF8', 0, dimsizes, ID, 
     .		        NULL_, STATUS)
      IF (STATUS .LT. CDF_OK) THEN
        IF (STATUS .EQ. CDF_EXISTS) THEN
          STATUS = CDF_LIB (OPEN_, CDF_, "TUTF8", ID,
     .                      NULL_, STATUS)
          IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '1.0')

          STATUS = CDF_LIB (DELETE_, CDF_,
     .                      NULL_, STATUS)
          IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '1.1')

          STATUS = CDF_LIB (CREATE_, CDF_, "TUTF8", 0, dimsizes, ID,
     .                      NULL_, STATUS)
          IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '1.2')
        ELSE
          CALL QUIT_CDF (STATUS, '1.3')
        END IF
      END IF

      STATUS = CDF_LIB (CREATE_, ATTR_, "UTF8", GLOBAL_SCOPE, ATTR_NUM,
     .		        NULL_, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '12.0')

      len1 = len(trim(spdf1))
      len2 = len(trim(spdf2))
      len3 = len(trim(spdf3))

      spdf1o = trim(spdf1)
      spdf2o = trim(spdf2)
      spdf3o = trim(spdf3)

      WRITE(6,200) 'spdf1=', '"', trim(spdf1), '"', 'byte_len=', len1
      WRITE(6,200) 'spdf2=', '"', trim(spdf2), '"', 'byte_len=', len2
      WRITE(6,200) 'spdf3=', '"', trim(spdf3), '"', 'byte_len=', len3
200   FORMAT(2X,A,A,A,A,3x,A,I2)

      STATUS = CDF_LIB (SELECT_, ATTR_, ATTR_NUM,
     .                           gENTRY_, 1,
     .		        PUT_, gENTRY_DATA_, CDF_CHAR, len1, spdf1,
     .		        NULL_, STATUS)
      IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '13.0')

      STATUS = CDF_LIB (SELECT_, gENTRY_, 2,
     .		        PUT_, gENTRY_DATA_, CDF_CHAR, len2, spdf2,
     .		        NULL_, STATUS)
      IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '14.0')
 
      STATUS = CDF_LIB (SELECT_, gENTRY_, 3,
     .		        PUT_, gENTRY_DATA_, CDF_CHAR, len3, spdf3,
     .		        NULL_, STATUS)
      IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '15.0')
 
      STATUS = CDF_LIB (CLOSE_, CDF_,
     .		        NULL_, STATUS)
      IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '16.0')

      STATUS = CDF_LIB (OPEN_, CDF_, 'TUTF8', ID, 
     .		        NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '1.5')

      STATUS = CDF_LIB (SELECT_, ATTR_NAME_, "UTF8",
     .		        NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '17.0')

      STATUS = CDF_LIB (SELECT_, gENTRY_, 1,
     .		        GET_, gENTRY_DATA_, spdf1o,
     .		        NULL_, STATUS)
      IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '18.0')
      IF (SPDF1 /= SPDF1O) CALL QUIT_CDF (STATUS, "18.1")

      STATUS = CDF_LIB (SELECT_, gENTRY_, 2,
     .		        GET_, gENTRY_DATA_, spdf2o,
     .		        NULL_, STATUS)
      IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '19.0')
      IF (SPDF2 /= SPDF2O) CALL QUIT_CDF (STATUS, '19.1')

412   STATUS = CDF_LIB (SELECT_, gENTRY_, 3,
     .		        GET_, gENTRY_DATA_, spdf3o,
     .		        NULL_, STATUS)
      IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '20.0')
      IF (SPDF3 /= SPDF3O) CALL QUIT_CDF (STATUS, '20.1')

413   STATUS = CDF_LIB (CLOSE_, CDF_,
     .		        NULL_, STATUS)
      IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '21.0')

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
	  STATUS2 = CDF_LIB (SELECT_, CDF_STATUS_, STATUS,
     .                       GET_, STATUS_TEXT_, TEXT,
     .                       NULL_, STATUS2)
	  WRITE (6,501) TEXT
 501	  FORMAT (' ', 'ERROR: ', A)
	ENDIF
	WRITE (6,404)
 404	FORMAT (' ','...test aborted')
	STOP
	END
