	PROGRAM QST2IFEPOCH16
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

        INCLUDE 'CDF.INC'
		
	INTEGER*4 ID, STATUS, ENCODING, MAJORITY, NUM_DIMS,
     .		  DIM_SIZES(2), VAR_DATA_TYPE, 
     .		  VAR_NUM_ELEMENTS, VAR_NUM_DIMS, VAR_DIM_SIZES(2),
     .            VAR_NUM_OUT,
     .		  INDICES(2), REC_NUM, REC_START, REC_COUNT,
     .		  REC_INTERVAL, COUNTS(2), INTERVALS(2), 
     .		  I, X1, 
     .		  VAR_REC_VARIANCE, 
     .		  VAR_DIM_VARIANCES(2)
        INTEGER*4 year, month, day, hour, minute, second, msec, usec, 
     .            nsec, 
     .            psec, yearOut, monthOut, dayOut, hourOut, minuteOut, 
     .            secondOut, msecOut, usecOut, nsecOut, psecOut
        INTEGER*4 yearHyper(4), monthHyper(4), dayHyper(4), 
     .            hourHyper(4),
     .            minuteHyper(4), secondHyper(4), msecHyper(4), 
     .            usecHyper(4),
     .            nsecHyper(4), psecHyper(4), yearHyperOut(4), 
     .            monthHyperOut(4),
     .            dayHyperOut(4), hourHyperOut(4), minuteHyperOut(4), 
     .            secondHyperOut(4),
     .            msecHyperOut(4), usecHyperOut(4), nsecHyperOut(4), 
     .            psecHyperOut(4)
        REAL*8    epoch(2), epochOut(2), 
     .            EPOCHHyper(2,4), epochHyperOut(2,4), MMM
	CHARACTER CDFNAME*(CDF_PATHNAME_LEN),
     .		  VARNAME*(CDF_VAR_NAME_LEN),
     .		  VAR_VALUES(2,3)*5, VAR_VALUE_OUT*5,
     .		  VAR_BUFFER_OUT(2,3)*5

	DATA ENCODING/IBMPC_ENCODING/, MAJORITY/COL_MAJOR/,
     .	     NUM_DIMS/0/, DIM_SIZES/0,0/,
     .	     VAR_DATA_TYPE/CDF_EPOCH16/, VAR_NUM_ELEMENTS/1/,
     .       VAR_NUM_DIMS/0/, VAR_DIM_SIZES/0,0/,
     .	     VAR_REC_VARIANCE/VARY/, VAR_DIM_VARIANCES/VARY,VARY/,
     .	     REC_NUM/1/, REC_START/1/, REC_COUNT/1/, REC_INTERVAL/1/,
     .	     COUNTS/0,0/, INTERVALS/1,1/,
     .	     CDFNAME(1:12)/'TEST'/, VARNAME(1:7)/'myEPOCH'/

C-----------------------------------------------------------------------
C NUL-terminate character strings.
C-----------------------------------------------------------------------

	CDFNAME(5:5) = CHAR(0)
	VARNAME(8:8) = CHAR(0)

C-----------------------------------------------------------------------
C Display test title.
C-----------------------------------------------------------------------

	WRITE (6,100)
 100	FORMAT (' Testing Internal/FORTRAN interface for CDF_EPOCH16..')

C-----------------------------------------------------------------------
C Create CDF.
C-----------------------------------------------------------------------
        
        STATUS = CDF_LIB    (CREATE_, CDF_, CDFNAME, 0,
     .                                      DIM_SIZES, ID,
C    .                       PUT_, CDF_ENCODING_, ENCODING,
     .                       PUT_, CDF_MAJORITY_, MAJORITY,
     .                             CDF_FORMAT_, SINGLE_FILE,
     .                       NULL_, STATUS)
        IF (STATUS .LT. CDF_OK) THEN
          IF (STATUS .EQ. CDF_EXISTS) THEN
            STATUS = CDF_LIB   (OPEN_, CDF_, CDFNAME, ID,
     .                          NULL_, STATUS)
            IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '1.0')
            
            STATUS = CDF_LIB   (DELETE_, CDF_,
     .                          NULL_, STATUS)
            IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '1.1')

            STATUS = CDF_LIB    (CREATE_, CDF_, CDFNAME, 0,
     .                                          DIM_SIZES, ID,
C    .                           PUT_, CDF_ENCODING_, ENCODING,
     .                           PUT_, CDF_MAJORITY_, MAJORITY,
     .                                 CDF_FORMAT_, SINGLE_FILE,
     .                           SELECT_, CDF_DECODING_, HOST_DECODING,
     .                           NULL_, STATUS)
            IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '1.2')
           ELSE
            CALL QUIT_CDF (STATUS, '1.3')
          END IF
        END IF

C-----------------------------------------------------------------------
C Create variables.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB (CREATE_, zVAR_, VARNAME, VAR_DATA_TYPE,
     .				          VAR_NUM_ELEMENTS,
     .                                    VAR_NUM_DIMS, VAR_DIM_SIZES,
     .				          VAR_REC_VARIANCE,
     .				          VAR_DIM_VARIANCES,
     .				          VAR_NUM_OUT,
     .		          NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '7.0')

C-----------------------------------------------------------------------
C Alocate/inquire records for a variable.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB    (SELECT_, zVAR_, VAR_NUM_OUT,
     .			     PUT_, zVAR_ALLOCATEBLOCK_, 1, 8,
     .			     NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '7b.0')

C-----------------------------------------------------------------------
C PUT to variable.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (SELECT_, zVAR_, VAR_NUM_OUT,
     .                               zVAR_RECNUMBER_, 1,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '8.0')
        year=2001
        month=10
        day=1
        hour=12
        minute=10
        second=20
        msec=100
        usec=200
        nsec=300
        psec=400

        CALL compute_EPOCH16 (year, month, day, hour, minute, 
     .                             second, msec, usec, nsec,psec, epoch)

        STATUS = CDF_LIB   (PUT_, zVAR_DATA_, epoch,
     .			    NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '8.1')

C-----------------------------------------------------------------------
C GET from the variable.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (SELECT_, zVAR_RECNUMBER_, 1,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '9.0')

        STATUS = CDF_LIB   (GET_, zVAR_DATA_, epochOut,
     .			    NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '9.1')

        CALL EPOCH16_breakdown (epochOut, yearOut, monthOut, 
     .                          dayOut, hourOut, minuteOut, 
     .                          secondOut, msecOut, usecOut,
     .                          nsecOut, psecOut)

        if (year .NE. yearOut)
     .     CALL QUIT_CDF(STATUS, "9.2a")
        if (month .NE. monthOut)
     .     CALL QUIT_CDF(STATUS, "9.2b")
        if (day .NE. dayOut)
     .     CALL QUIT_CDF(STATUS, "9.2c")
        if (hour .NE. hourOut)
     .     CALL QUIT_CDF(STATUS, "9.2d")                           
        if (minute .NE. minuteOut)
     .     CALL QUIT_CDF(STATUS, "9.2e")                       
        if (second .NE. secondOut)
     .     CALL QUIT_CDF(STATUS, "9.2f")                       
        if (msec .NE. msecOut)
     .     CALL QUIT_CDF(STATUS, "9.2g")                           
        if (usec .NE. usecOut)
     .     CALL QUIT_CDF(STATUS, "9.2h")                           
        if (nsec .NE. nsecOut)
     .     CALL QUIT_CDF(STATUS, "9.2i")                          
        if (psec .NE. psecOut)
     .     CALL QUIT_CDF(STATUS, "9.2j")                          

C-----------------------------------------------------------------------
C HyperPUT to the variable.
C-----------------------------------------------------------------------

	INDICES(1) = 0
	INDICES(2) = 0

        DO X1 = 1, 4  
          year = year + 1
          hour = hour + 1
          second = second +1
          nsec = nsec + 100
          yearHyper(x1) = year
          monthHyper(x1) = month
          dayHyper(x1) = day
          hourHyper(x1) = hour
          minuteHyper(x1) = minute
          secondHyper(x1) = second
          msecHyper(x1) = msec
          usecHyper(x1) = usec
          nsecHyper(x1) = nsec
          psecHyper(x1) = psec
          CALL compute_EPOCH16 (year, month, day, hour,
     .                          minute, second, msec, usec, nsec,
     .                          psec, epoch)
          epochHyper(1,x1) = epoch(1)
          epochHyper(2,x1) = epoch(2)
        END DO

        STATUS = CDF_LIB    (SELECT_, zVAR_RECNUMBER_, 2,
     .			              zVAR_RECCOUNT_, 4,
     .			              zVAR_RECINTERVAL_, 1,
     .			              zVAR_DIMINDICES_, INDICES,
     .			              zVAR_DIMCOUNTS_, COUNTS,
     .			              zVAR_DIMINTERVALS_, INTERVALS,
     .		             NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '10.0a')

        STATUS = CDF_LIB   (PUT_, zVAR_HYPERDATA_, epochHyper,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '10.0b')

C-----------------------------------------------------------------------
C HyperGET from variable.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (GET_, zVAR_HYPERDATA_, epochHyperOut,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '11.0')

        DO x1 = 1, 4
          epoch(1) = epochHyperOut(1, x1)
          epoch(2) = epochHyperOut(2, x1)
          CALL EPOCH16_breakdown (epoch, yearOut, monthOut, dayOut,
     .                            hourOut, minuteOut, secondOut,
     .                            msecOut, usecOut, nsecOut, psecOut)
          yearHyperOut(x1) = yearOut
          monthHyperOut(x1) = monthOut
          dayHyperOut(x1) = dayOut
          hourHyperOut(x1) = hourOut
          minuteHyperOut(x1) = minuteOut
          secondHyperOut(x1) = secondOut
          msecHyperOut(x1) = msecOut
          usecHyperOut(x1) = usecOut
          nsecHyperOut(x1) = nsecOut
          psecHyperOut(x1) = psecOut
        END DO

        DO x1 = 1, 4
           if (yearHyper(x1) .NE. yearHyperOut(x1)) 
     .        CALL QUIT_CDF(STATUS, "11.2a")
           if (monthHyper(x1) .NE. monthHyperOut(x1)) 
     .        CALL QUIT_CDF(STATUS, "11.2b")
           if (dayHyper(x1) .NE. dayHyperOut(x1)) 
     .        CALL QUIT_CDF(STATUS, "11.2c")
           if (hourHyper(x1) .NE. hourHyperOut(x1)) 
     .        CALL QUIT_CDF(STATUS, "11.2d")
           if (minuteHyper(x1) .NE. minuteHyperOut(x1))
     .        CALL QUIT_CDF(STATUS, "11.2e")
           if (secondHyper(x1) .NE. secondHyperOut(x1))
     .        CALL QUIT_CDF(STATUS, "11.2f")
           if (msecHyper(x1) .NE. msecHyperOut(x1)) 
     .        CALL QUIT_CDF(STATUS, "11.2g")
           if (usecHyper(x1) .NE. usecHyperOut(x1)) 
     .        CALL QUIT_CDF(STATUS, "11.2h")
           if (nsecHyper(x1) .NE. nsecHyperOut(x1)) 
     .        CALL QUIT_CDF(STATUS, "11.2i")
           if (psecHyper(x1) .NE. psecHyperOut(x1)) 
     .        CALL QUIT_CDF(STATUS, "11.2j")
        END DO                                                                          

C-----------------------------------------------------------------------
C Close CDF.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (CLOSE_, CDF_,
     .		            NULL_, STATUS)
	IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '28.0')

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
