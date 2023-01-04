        PROGRAM QQQ
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

        INTEGER (KIND=KIND_INT8) TT2000(8), MMM(8), XTT2000, YTT2000
        INTEGER*4 ID, STATUS, IX
        REAL*8    unixTime(8)
        INTEGER*4 year, month, day, hour, minute, second, msec, usec, 
     .            nsec, year1, month1, day1, hour1, minute1, second1,
     .            msec1, usec1, nsec1, yearOut, monthOut, dayOut,
     .            hourOut, minuteOut, secondOut, msecOut, usecOut,
     .            asecOut

       CHARACTER STRINGDATA*(TT2000_4_STRING_LEN),
     .           STRINGDATA2*(TT2000_4_STRING_LEN)

C-----------------------------------------------------------------------
C Display test title.
C-----------------------------------------------------------------------
        STATUS = -1
        WRITE (6,100)
 100    FORMAT (' Testing FORTRAN interface for TT2000 & UnixTime..')

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
     .               minute, second, msec, usec, nsec, tt2000(1))
        if (tt2000(1) .EQ. ILLEGAL_TT2000_VALUE) 
     .    CALL QUIT_CDF (TT2000_TIME_ERROR, '8.0')
        DO IX = 2, 8
          tt2000(ix) = tt2000(1) + (ix-1) * 1000000000_KIND_INT8
        END DO

        DO IX = 1, 8
          CALL TOENCODE_TT2000 (tt2000(IX), 4, STRINGDATA)
          CALL TOPARSE_TT2000 (STRINGDATA, mmm(IX))
          if (tt2000(IX) .NE. MMM(IX))
     .       CALL QUIT_CDF(STATUS, '10.2a')
          CALL BREAKDOWN_TT2000 (MMM(IX), yearOut,
     .                       monthOut, dayOut, hourOut, minuteOut,
     .                       secondOut, msecOut, usecOut, asecOut)
           if (year1 .NE. yearOut) 
     .        CALL QUIT_CDF(STATUS, '11.2a')
           if (month1 .NE. monthOut) 
     .        CALL QUIT_CDF(STATUS, '11.2b')
           if (day1 .NE. dayOut) 
     .        CALL QUIT_CDF(STATUS, '11.2c')
           if (hour1 .NE. hourOut) 
     .        CALL QUIT_CDF(STATUS, '11.2d')
           if (minute1 .NE. minuteOut)
     .        CALL QUIT_CDF(STATUS, '11.2e')
           if (second1 .NE. secondOut)
     .        CALL QUIT_CDF(STATUS, '11.2f')
           if (msec1 .NE. msecOut) 
     .        CALL QUIT_CDF(STATUS, '11.2g')
           if (usec1 .NE. usecOut) 
     .        CALL QUIT_CDF(STATUS, '11.2h')
           if (nsec1 .NE. asecOut) 
     .        CALL QUIT_CDF(STATUS, '11.2i')
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
        CALL TT2000_TO_UNIXTIME (tt2000, unixTime, 8)
        CALL UNIXTIME_TO_TT2000 (unixTime, mmm, 8)
        DO IX = 1, 8
          CALL TOENCODE_TT2000 (tt2000(IX), 4, STRINGDATA)
          CALL TOENCODE_TT2000 (mmm(IX), 4, STRINGDATA2)
          WRITE (6, 224) IX, tt2000(IX), STRINGDATA, UNIXTIME(IX),
     .                   mmm(IX), STRINGDATA2
224       FORMAT(' ',I1,': ',I18,':(',A,') => ',F20.6,' => ',I18,
     .           ':(',A,')')
        END DO
        END

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
