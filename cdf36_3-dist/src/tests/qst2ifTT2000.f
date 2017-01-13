        PROGRAM QST2IFTT2000
C-----------------------------------------------------------------------
C Copyright 1996-2014 United States Government as represented by the
C Administrator of the National Aeronautics and Space Administration.
C All Rights Reserved.
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C
C  SPDF/CDF         Quick Start Test Program (CDF_TIME_TT2000 /FORTRAN).
C
C  Version 1.0, 16-Mar-11, ADNET systems.
C
C  Modification history:
C
C   V1.0   16-Mar-11, M Liu         Original version (for CDF V3.3.2).
C
C-----------------------------------------------------------------------

        INCLUDE 'CDF.INC'

        INTEGER (KIND=KIND_INT8) TT2000(8), MMM(8), DINT8(2,3),
     .                           OUT8(2,3), NN
        INTEGER*4 ID, STATUS, 
     .            VAR1_DIM_VARIANCES(1), VAR2_DIM_VARIANCES(1),
     .            VAR1_DIM_SIZES(1),VAR2_DIM_SIZES(1),
     .            INDICES(1),COUNTS(1),INTERVALS(1),
     .            IX, VAR1_NUM_OUT, VAR2_NUM_OUT
        INTEGER*4 year, month, day, hour, minute, second, msec, usec, 
     .            nsec, year1, month1, day1, hour1, minute1, second1,
     .            msec1, usec1, nsec1, yearOut, monthOut, dayOut,
     .            hourOut, minuteOut, secondOut, msecOut, usecOut,
     .            asecOut

       CHARACTER  CDFNAME*(CDF_PATHNAME_LEN),
     .            VAR1NAME*(CDF_VAR_NAME_LEN256),
     .            VAR2NAME*(CDF_VAR_NAME_LEN256),
     .            STRINGDATA*(TT2000_3_STRING_LEN)

        DATA VAR1_DIM_SIZES/0/, VAR2_DIM_SIZES/2/,
     .       VAR1_DIM_VARIANCES/VARY/, VAR2_DIM_VARIANCES/VARY/,
     .       COUNTS/1/, INTERVALS/1/, INDICES/0/,
     .       CDFNAME(1:6)/'TESTF'/, VAR1NAME(1:9)/'myTT2000'/,
     .       VAR2NAME(1:7)/'myINT8'/

C-----------------------------------------------------------------------
C NUL-terminate character strings.
C-----------------------------------------------------------------------

        CDFNAME(6:6) = CHAR(0)
        VAR1NAME(9:9) = CHAR(0)
        VAR2NAME(7:7) = CHAR(0)

C-----------------------------------------------------------------------
C Display test title.
C-----------------------------------------------------------------------

        WRITE (6,100)
 100    FORMAT (' Testing FORTRAN interface for CDF_TIME_TT2000..')

C-----------------------------------------------------------------------
C Create CDF.
C-----------------------------------------------------------------------
        
        STATUS = CDF_LIB    (CREATE_, CDF_, CDFNAME, 0,
     .                                      VAR1_DIM_SIZES, ID,
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
     .                                          VAR1_DIM_SIZES, ID,
     .                           NULL_, STATUS)
            IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '1.2')
           ELSE
            CALL QUIT_CDF (STATUS, '1.3')
          END IF
        END IF

C-----------------------------------------------------------------------
C Create variables.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB (CREATE_, zVAR_, VAR1NAME, CDF_TIME_TT2000,
     .                                    1, 0,
     .                                    VAR1_DIM_SIZES,
     .                                    VARY,
     .                                    VAR1_DIM_VARIANCES,
     .                                    VAR1_NUM_OUT,
     .                    NULL_, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '6.0')

        STATUS = CDF_LIB (CREATE_, zVAR_, VAR2NAME, CDF_INT8,
     .                                    1, 1,
     .                                    VAR2_DIM_SIZES,
     .                                    VARY,
     .                                    VAR2_DIM_VARIANCES,
     .                                    VAR2_NUM_OUT,
     .                    NULL_, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '7.0')

        year=2016
        month=12
        day=31
        hour=23
        minute=59
        second=57
        msec=100
        usec=200
        nsec=300
        year1=2016
        month1=12
        day1=31
        hour1=23
        minute1=59
        second1=57
        msec1=100
        usec1=200
        nsec1=300

        call COMPUTE_TT2000 (year, month, day, hour,
     .                    minute, second, msec, usec, nsec, tt2000(1))
        if (tt2000(1) .EQ. ILLEGAL_TT2000_VALUE) 
     .    CALL QUIT_CDF (TT2000_TIME_ERROR, '8.0')
        DO IX = 2, 8
          tt2000(ix) = tt2000(1) + (ix-1) * 1000000000_KIND_INT8
        END DO

C-----------------------------------------------------------------------
C HyperPUT to the variable.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB    (SELECT_, zVAR_, VAR1_NUM_OUT,
     .                                zVAR_RECNUMBER_, 1,
     .                                zVAR_RECCOUNT_, 8,
     .                                zVAR_RECINTERVAL_, 1,
     .                                zVAR_DIMINDICES_, INDICES,
     .                                zVAR_DIMCOUNTS_, COUNTS,
     .                                zVAR_DIMINTERVALS_, INTERVALS,
     .                       NULL_, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '10.0a')

        STATUS = CDF_LIB   (PUT_, zVAR_HYPERDATA_, tt2000,
     .                      NULL_, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '10.0b')

C-----------------------------------------------------------------------
C HyperGET from variable.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (GET_, zVAR_HYPERDATA_, MMM,
     .                      NULL_, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '11.0')

        DO IX = 1, 8
          CALL ENCODE_TT2000 (MMM(IX), 3, STRINGDATA)
          WRITE (6, 200) IX, STRINGDATA
200       FORMAT(' ',I1,': ',A)
          CALL BREAKDOWN_TT2000 (MMM(IX), yearOut,
     .                     monthOut, dayOut, hourOut, minuteOut,
     .                     secondOut, msecOut, usecOut, asecOut)
           if (year1 .NE. yearOut) 
     .        CALL QUIT_CDF(STATUS, "11.2a")
           if (month1 .NE. monthOut) 
     .        CALL QUIT_CDF(STATUS, "11.2b")
           if (day1 .NE. dayOut) 
     .        CALL QUIT_CDF(STATUS, "11.2c")
           if (hour1 .NE. hourOut) 
     .        CALL QUIT_CDF(STATUS, "11.2d")
           if (minute1 .NE. minuteOut)
     .        CALL QUIT_CDF(STATUS, "11.2e")
           if (second1 .NE. secondOut)
     .        CALL QUIT_CDF(STATUS, "11.2f")
           if (msec1 .NE. msecOut) 
     .        CALL QUIT_CDF(STATUS, "11.2g")
           if (usec1 .NE. usecOut) 
     .        CALL QUIT_CDF(STATUS, "11.2h")
           if (nsec1 .NE. asecOut) 
     .        CALL QUIT_CDF(STATUS, "11.2i")
           second1 = second1 + 1.0
           if (second1 > 60.0) then
             year1 = year1 + 1
             month1 = 1
             day1 = 1
             hour1 = 0
             minute1 = 0
             second1 = 0
           endif
        END DO

        NN = 8888888888_KIND_INT8
        DO IX = 1, 3
           DINT8(1,IX) = NN
           DINT8(2,IX) = NN + 1
           NN = NN + 1000000000_KIND_INT8
        END DO

        INDICES(1) = 1
        COUNTS(1) = 2
        INTERVALS(1) = 1
C-----------------------------------------------------------------------
C HyperPUT to the variable.
C-----------------------------------------------------------------------
        
        STATUS = CDF_LIB    (SELECT_, zVAR_, VAR2_NUM_OUT,
     .                                zVAR_RECNUMBER_, 1,
     .                                zVAR_RECCOUNT_, 3,
     .                                zVAR_RECINTERVAL_, 1,
     .                                zVAR_DIMINDICES_, INDICES,
     .                                zVAR_DIMCOUNTS_, COUNTS,
     .                                zVAR_DIMINTERVALS_, INTERVALS,
     .                       NULL_, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '13.1')
        
        STATUS = CDF_LIB   (PUT_, zVAR_HYPERDATA_, DINT8,
     .                      NULL_, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '13.2')

C-----------------------------------------------------------------------
C HyperGET from variable.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (GET_, zVAR_HYPERDATA_, OUT8,
     .                      NULL_, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '14.0')

        NN = 8888888888_KIND_INT8
        DO IX = 1, 3
          IF (OUT8(1,IX) .NE. NN) CALL QUIT_CDF (STATUS, '15.0')
          IF (OUT8(2,IX) .NE. (NN+1)) CALL QUIT_CDF (STATUS, '15.0')
          NN = NN + 1000000000_KIND_INT8
        END DO 
C-----------------------------------------------------------------------
C Close CDF.
C-----------------------------------------------------------------------

        STATUS = CDF_LIB   (CLOSE_, CDF_,
     .                      NULL_, STATUS)
        IF (STATUS .LT. CDF_OK) CALL QUIT_CDF (STATUS, '28.0')

        END

C-----------------------------------------------------------------------
C QUIT_CDF.  Abort test early due to CDF error.
C-----------------------------------------------------------------------

        SUBROUTINE QUIT_CDF (STATUS, WHERE)
        INTEGER*4 STATUS
        CHARACTER WHERE*(*)
        WRITE (6,401) WHERE
 401    FORMAT (' ', 'Aborting at ', A, '...')
        IF (STATUS .LT. 0) THEN
          WRITE (6,501) STATUS
 501      FORMAT (' ', 'CDF status code: ', I5)
        ENDIF
        WRITE (6,404)
 404    FORMAT (' ','...test aborted')
        STOP
        END
