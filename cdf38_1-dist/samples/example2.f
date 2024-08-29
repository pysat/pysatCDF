C----------------------------------------------------------------------
C
C  NSSDC/CDF                    Create an example CDF (using skeleton 
C                                                      table).
C
C  Version 1.0, 13-Nov-91, Hughes STX
C
C  Modification history:
C
C   V1.0  13-Nov-91, J Love    Original version 
C    1.1  12-Jan-06, M Liu     Used the new Standard Interface 
C                              routines
C
C---------------------------------------------------------------------

        INCLUDE '../include/cdf.inc'

        INTEGER*4 id            ! CDF identifier.
        INTEGER*4 status        ! CDF completion status.

        INTEGER*4 lun           ! Logical unit number for input data 
                                ! file.

        INTEGER*4 indices(2)    ! Dimension indices.
        INTEGER*4 rec_num       ! Record number.

        INTEGER*4 time_var_num  ! 'Time' zVariable number.
        INTEGER*4 tmp_var_num   ! 'Temperature' zVariable number.

        INTEGER*4 time          ! 'Time' zVariable value.
        REAL*4    lat           ! 'Latitude' zVariable value.
        REAL*4    lon           ! 'Longitude' zVariable value.
        REAL*4    tmp           ! 'Temperature' zVariable value.

	INTEGER*4 x1, x2	! Indices

        DATA lun/1/

C----------------------------------------------------------------------
C Open the CDF.
C----------------------------------------------------------------------

        CALL CDF_open_cdf ('example2', id, status)
        IF (status .NE. CDF_OK) CALL StatusHandler (status)

C----------------------------------------------------------------------
C Determine zVariable numbers.
C----------------------------------------------------------------------

        time_var_num = CDF_get_var_num (id, 'Time')
        IF (time_var_num .LT. CDF_OK) CALL StatusHandler (status)

        tmp_var_num = CDF_get_var_num (id, 'Temperature')
        IF (tmp_var_num .LT. CDF_OK) CALL StatusHandler (status)

C----------------------------------------------------------------------
C Read input values for zVariables and write them to the CDF.  Not
C every value must be written to the CDF - many of the values are 
C redundant. The 'Time' value only has to be written once per CDF 
C record (every 4 input records).  The 'Longitude' and 'Latitude' 
C values are not written at all because they had been specified in the 
C skeleton table.  Each 'Temperature' value read is written to the CDF.
C----------------------------------------------------------------------

        OPEN (UNIT=lun, FILE='example.dat', STATUS='OLD', ERR=99)

        DO rec_num = 1, 24
          DO x1 = 1, 2
            DO x2 = 1, 2
              indices(1) = x1
              indices(2) = x2

              READ (lun, *, ERR=99) time, lon, lat, tmp
              IF (indices(1) .EQ. 1 .AND. indices(2) .EQ. 1) THEN
                CALL CDF_put_zvar_data (id, time_var_num, rec_num, 
     .                                  indices, time, status)
                IF (status .NE. CDF_OK) CALL StatusHandler (status)
              END IF
              CALL CDF_put_zvar_data (id, tmp_var_num, rec_num, 
     .                                indices, tmp, status)
              IF (status .NE. CDF_OK) CALL StatusHandler (status)
            END DO
          END DO
        END DO

        CLOSE (lun, ERR=99)

C----------------------------------------------------------------------
C Close CDF.
C----------------------------------------------------------------------

        CALL CDF_close_cdf (id, status)
        IF (status .NE. CDF_OK) CALL StatusHandler (status)

        STOP

C----------------------------------------------------------------------
C Input file error handler.
C----------------------------------------------------------------------

 99     WRITE (6,101)
 101    FORMAT (' ','Error reading input file')
        STOP

        END



C----------------------------------------------------------------------------
C Status handler.
C----------------------------------------------------------------------------

        SUBROUTINE StatusHandler (status)
        INTEGER*4 status

        INCLUDE '../include/cdf.inc'

        CHARACTER message*(CDF_ERRTEXT_LEN)
	INTEGER*4 statuso

        IF (status .LT. CDF_WARN) THEN
          WRITE (6,10)
10        FORMAT (' ','Error (halting)...')
          CALL CDF_get_status_text (status, message, statuso)
          IF (statuso .EQ. CDF_OK) WRITE (6,11) message
11        FORMAT (' ',A)
          STOP
        ELSE
          IF (status .LT. CDF_OK) THEN
            WRITE (6,12)
12          FORMAT (' ','Warning...')
            CALL CDF_get_status_text (status, message, statuso)
            IF (statuso .EQ. CDF_OK) WRITE (6,13) message
13          FORMAT (' ',A)
          ELSE
            IF (status .GT. CDF_OK) THEN
              WRITE (6,14)
14            FORMAT (' ','Be advised that...')
              CALL CDF_get_status_text (status, message, statuso)
              IF (statuso .EQ. CDF_OK) WRITE (6,15) message
15            FORMAT (' ',A)
            END IF
          END IF
        END IF

        RETURN
        END
