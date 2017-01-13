        PROGRAM QSTLEAPSECONDS
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

        INTEGER*4 IX, STATUS, year, month, day, IY, ROWS, dump
        REAL*8    table(6,100)
        INTEGER*4 narg
        CHARACTER  ENVVAR*(CDF_PATHNAME_LEN)
        CHARACTER  myarg*(CDF_PATHNAME_LEN)

        dump = 0
        narg = IARGC ()
        if (narg > 0) then
          call getarg (1, myarg)
          if ((trim(myarg) .eq. "-dump") .OR.
     x        (trim(myarg) .eq. "-DUMP")) then
            dump = 1
          end if
        end if
        WRITE (*,*) 'Info for CDF leap second table...'
        call CDF_getLeapSecondsTableEnvVar(ENVVAR)
        status = CDF_getLeapSecondsTableStatus()
        if (ENVVAR(1:1).eq.char(0)) then
          WRITE (*,*) 'Environment Variable: CDF_LEAPSECONDSTABLE',
     x                ' is NOT defined....'
          WRITE (*,*) 'Thus, the hard-coded table is used.'
        else
          if (status.eq.0) then 
            WRITE (*,*) 'Environment Variable: CDF_LEAPSECONDSTABLE',
     x                  ' is defined as: ', trim(ENVVAR)
            WRITE (*,*) '                      but the file is',
     x                  ' invalid....'
            WRITE (*,*) 'Thus, the hard-coded table is used.'
          else
            WRITE (*,*) 'CDF leap seconds table is based on the',
     x                  ' file: ', trim(ENVVAR)
          end if
        end if
        call CDF_getLastDateinLeapSecondsTable (year, month, day)
        WRITE (*, 50) year, month, day
 50     FORMAT (' The last date a leap second was added to the',
     x          ' table is: ', i4, '-', i2.2, '-', i2.2)
        if (dump == 1) then
          call CDF_fillLeapSecondsTable(table)
          rows = CDF_getRowsinLeapSecondsTable()
          WRITE (*, 60)
 60       FORMAT ('   ',T7,' ',T14,' ',T21,'Leap',T35,'Drift',T49,
     x            'Drift')
          WRITE (*, 65)
 65       FORMAT (' Year',T7,'Month',T14,' Day',T21,'Seconds',T35,
     x            '    1',T49,'    2')
          do 100 ix = 1, rows
            if (ix < 6) then
              if (table(2,ix) < 10.0) then
                WRITE (*, 70) table(1,ix), table(2,ix), table(3,ix),
     x                        table(4,ix), table(5,ix), table(6,ix)
 70             FORMAT (' ',G4.4,T7,'  ',G3.1,T14,'   ',G1.1,T21,F8.6,
     x                  T35,' ', F6.0,T49,' ',F9.7)
              else
                WRITE (*, 72) table(1,ix), table(2,ix), table(3,ix),
     x                        table(4,ix), table(5,ix), table(6,ix)
 72             FORMAT (' ',G4.4,T7,' ',F4.0,T14,'   ',G1.1,T21,F8.6,
     x                  T35,' ', F6.0,T49,' ',F9.7)
              end if
            else 
              if (ix < 15) then
                WRITE (*, 75) table(1,ix), table(2,ix), table(3,ix),
     x                      table(4,ix), table(5,ix), table(6,ix)
 75             FORMAT (' ',G4.4,T7,'  ',G3.1,T14,'   ',G1.1,T21,F7.5,
     x                  T35,' ', F6.0,T49,' ',F9.7)
              else
                WRITE (*, 80) table(1,ix), table(2,ix), table(3,ix),
     x                        table(4,ix), table(5,ix), table(6,ix)
 80             FORMAT (' ',G4.4,T7,'  ',G3.1,T14,'   ',G1.1,T21,G2.2,
     x                  T35,' ', G5.5,T49,' ',G5.5)
              end if
            end if
 100      continue
        end if
        STOP
        END
