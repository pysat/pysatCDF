	PROGRAM QST2IF2
C
C  Test writing a couple of UTF8 strings into a global attribute.
C

        INCLUDE 'CDF.INC'
		
	INTEGER*4 ID, STATUS, ATTR_NUM, dimsizes(1)

      data dimsizes/0/

      character(len = 25)::spdf
      character(len = 25)::spdf2

      spdf = 'SPDF©SPDF'
      spdf2 = 'Glück©Glück社i安'

      STATUS = CDF_LIB    (CREATE_, CDF_, 'TUTF8', 0, dimsizes, ID, 
     .		           NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '1.3')

      STATUS = CDF_LIB   (CREATE_, ATTR_, "Global1", GLOBAL_SCOPE,
     .				            ATTR_NUM,
     .		          NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '12.0')

      STATUS = CDF_LIB    (SELECT_, gENTRY_, 1,
     .		           PUT_, gENTRY_DATA_, CDF_CHAR,
     .				               len(trim(spdf)), spdf,
     .		           NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '13.0')

      STATUS = CDF_LIB    (SELECT_, gENTRY_, 2,
     .		           PUT_, gENTRY_DATA_, CDF_CHAR,
     .				               len(trim(spdf2)), spdf2,
     .		           NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '14.0')

        STATUS = CDF_LIB   (CLOSE_, CDF_,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '28.0')

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
