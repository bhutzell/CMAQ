c...fake_ioapi.f

     
      SUBROUTINE M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      
      IMPLICIT NONE
      CHARACTER*(*) PNAME
      CHARACTER*(*) XMSG
      INTEGER JDATE, JTIME, XSTAT1
      
      IF(XSTAT1 .EQ. 0)THEN
         PRINT *, ' *******PROGRAM ',PNAME,' TERMINATED NORMALLY********'
      ELSEIF(XSTAT1 .EQ. 1)THEN
         PRINT*,  ' IO ERROR IN ',PNAME
      ELSEIF(XSTAT1 .GE. 2)THEN
         PRINT*, ' PROGRAMING ERROR IN ',PNAME
      ENDIF
      PRINT *, ' AT DATE/TIME ', JDATE, JTIME
      PRINT *, TRIM( XMSG )
      STOP
      END SUBROUTINE M3EXIT
 
     
      SUBROUTINE M3WARN ( PNAME, JDATE, JTIME, XMSG )
      
      IMPLICIT NONE
      CHARACTER*(*) PNAME
      CHARACTER*(*) XMSG
      INTEGER JDATE, JTIME
      
      PRINT *, ' !!!WARNING IN PROGRAM ',PNAME,' !!!!!!!!!!'
      PRINT *, ' AT DATE/TIME ', JDATE, JTIME
      PRINT *, TRIM( XMSG )
      RETURN
      END SUBROUTINE M3WARN
      

      SUBROUTINE PM3WARN ( PNAME, JDATE, JTIME, XMSG )
      
      IMPLICIT NONE
      CHARACTER*16 PNAME
      CHARACTER*(*) XMSG
      INTEGER JDATE, JTIME
      
      PRINT *, ' !!!WARNING IN PROGRAM ',PNAME,' !!!!!!!!!!'
      PRINT *, ' AT DATE/TIME ', JDATE, JTIME
      PRINT *, TRIM( XMSG )
      RETURN
      END SUBROUTINE PM3WARN




C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/index1.f,v 1.2 2000/11/28 21:22:49 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

      INTEGER FUNCTION INDEX1 (NAME, N, NLIST)

C***********************************************************************
C  subroutine body starts at line 46
C
C  FUNCTION:
C
C    Searches for NAME in list NLIST and returns the subscript
C    (1...N) at which it is found, or returns 0 when NAME not
C    found in NLIST
C
C  PRECONDITIONS REQUIRED:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION HISTORY:
C
C    5/88   Modified for ROMNET
C    9/94   Modified for Models-3 by CJC
C
C***********************************************************************

      IMPLICIT NONE
 
C.......   Arguments and their descriptions:

      CHARACTER*(*), INTENT( IN ) :: NAME        !  Character string being searched for
      INTEGER, INTENT( IN )       :: N           !  Length of array to be searched
      CHARACTER*(*), INTENT( IN ) :: NLIST(*)    !  array to be searched

C.......   Local variable:

      INTEGER       I   !  loop counter

C.....................................................................
C.......   begin body of INDEX1()

      DO 100 I = 1, N

          IF ( TRIM( NAME ) .EQ. TRIM( NLIST( I ) )  ) THEN    ! Found NAME in NLIST
              INDEX1 = I
              RETURN
          ENDIF

100   CONTINUE

      INDEX1 = 0        !  not found
      RETURN

      END FUNCTION INDEX1

C @(#)time2sec.f        1.1 /pub/storage/edss/aqm/src/ioapi/util/SCCS/s.time2sec.f 08 Jun 1994 11:04:32
        INTEGER   FUNCTION  TIME2SEC ( TIME )

C***********************************************************************
C  function body starts at line
C
C  FUNCTION:  convert time difference format HHMMSS to integer seconds 
C
C  RETURN VALUE:  seconds
C
C  REVISION  HISTORY:  prototype 5/92 by CJC
C
C
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:
        INTEGER(4)       TIME
C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        INTEGER(4)       ABST
***********************************************************************
C   begin body of function  TIME2SEC

        IF ( TIME .GE. 0 ) THEN         !  div-mod arithmetic OK
            TIME2SEC = MOD( TIME, 100 )  +
     &                   60 * ( MOD( TIME / 100, 100 )  +
     &                   60 * ( TIME / 10000 ) )
        ELSE                            !  work with absolute values:
            ABST     = - TIME
            TIME2SEC = - ( MOD( ABST, 100 )  +
     &                       60 * ( MOD( ABST / 100, 100 )  +
     &                       60 * ( ABST / 10000 ) ) )
        END IF

        RETURN

        END FUNCTION  TIME2SEC


        SUBROUTINE  UPCASE ( BUFFER )
C***********************************************************************

C  subroutine body starts at line  41
C
C  FUNCTION:  upcase the text in BUFFER
C
C  PRECONDITIONS REQUIRED:  text is ASCII
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:  prototype 1/91 by CJC
C
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:
        CHARACTER*(*)   BUFFER
C...........   PARAMETER:  ASCII for 'a', 'z', 'A'
        INTEGER, PARAMETER :: IA = 97, IZ = 122, AADIF =32
!        PARAMETER   ( IA    = 97,
!     &                IZ    = 122,
!     &                AADIF = 32 )
C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        INTEGER       I, L
        INTEGER       C
C***********************************************************************
C   begin body of subroutine  UPCASE

        L  =  LEN ( BUFFER )
        DO  I = 1 , L
          C = ICHAR ( BUFFER ( I:I ) )
          IF ( C .GE. IA  .AND.  C .LE. IZ )BUFFER ( I:I ) = CHAR ( C - AADIF )
        END DO        !  end loop on I

        RETURN

        END SUBROUTINE  UPCASE
C @(#)hhmmss.f	1.1 /pub/storage/edss/aqm/src/ioapi/util/SCCS/s.hhmmss.f 08 Jun 1994 11:04:25
        CHARACTER(10) FUNCTION  HHMMSS ( JTIME )

C***********************************************************************
C  function body starts at line  52
C
C  FUNCTION:  format and return the time as a character string 
C             "HH:MM:SS"
C
C
C  PRECONDITIONS REQUIRED:  valid time HHMMSS with hours component
C             at most 9999
C
C  RETURN VALUE:  time, as "HH:MM:SS"
C
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C
C  REVISION  HISTORY:  prototype 10/90 byu CJC
C
C
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT( IN ) ::  JTIME   !  Julian time, coded YYYYDDD


C...........   SAVED LOCAL VARIABLES and their descriptions:
C...........   NOTE:  the ANSI standard requires the use of SAVE statements
C...........   for variables which must retain their values from call to call.
        CHARACTER( 1 ), PARAMETER :: DIGITS( 0:9 ) = 
     &                             (/ '0','1','2','3','4','5','6','7','8','9' /)
!        DATA    	DIGITS
!     &  


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER       HOUR
        INTEGER       MINS
        INTEGER       SECS
        INTEGER       J , K

        CHARACTER(10)    CHRBUF


C***********************************************************************
C   begin body of function  HHMMSS

        CHRBUF = '          '

        HOUR = JTIME
        SECS = MOD ( HOUR , 100 )
        HOUR = HOUR / 100
        MINS = MOD ( HOUR , 100 )
        HOUR = HOUR / 100

        J = 1
        K = HOUR / 1000
        IF ( K .GT. 9 ) THEN
            STOP 'JTIME out of range for HHMMSS'
        ELSE IF ( K .NE. 0 ) THEN
            CHRBUF( J:J ) = DIGITS( K )
            J = J + 1
        END IF

        K = MOD( HOUR / 100 , 10 )
        IF ( K .NE. 0 ) THEN
            CHRBUF( J:J ) = DIGITS( K )
            J = J + 1
        END IF

        K = MOD( HOUR / 10 , 10 )
        IF ( K .NE. 0 ) THEN
            CHRBUF( J:J ) = DIGITS( K )
            J = J + 1
        END IF

        CHRBUF( J:J ) = DIGITS( MOD( HOUR, 10 ) )
        J = J + 1
        CHRBUF( J:J ) = ':'
        J = J + 1

        CHRBUF( J:J ) = DIGITS( MINS / 10 )
        J = J + 1
        CHRBUF( J:J ) = DIGITS( MOD( MINS, 10 ) )
        J = J + 1
        CHRBUF( J:J ) = ':'
        J = J + 1

        CHRBUF( J:J ) = DIGITS( SECS / 10 )
        J = J + 1
        CHRBUF( J:J ) = DIGITS( MOD( SECS, 10 ) )

        HHMMSS = CHRBUF

        RETURN

        END FUNCTION  HHMMSS


      INTEGER FUNCTION  JUNIT()
C***********************************************************************
C  FUNCTION:
C    Routine returns next available FORTRAN unit number
C  ARGUMENT LIST DESCRIPTION: None
C  RETURN VALUE:  JUNIT        Unit number selected
C  LOCAL VARIABLE DESCRIPTION:
C    IUNIT    state variable:  counts through available units
C    BOT      parameter:  first POSIX-approved unit number for FORTRAN I/O
C    TOP      parameter:  last   ...
C***********************************************************************
      IMPLICIT NONE
C...........   PARAMETERS and their descriptions:
      INTEGER, PARAMETER :: BOT = 10 ! 1 less than initial unit number
      INTEGER, PARAMETER :: TOP = 99 ! final unit number
C...........   SAVED LOCAL VARIABLES and their descriptions:
      INTEGER      IUNIT, J
      LOGICAL      FLAG

       DO  J = BOT, TOP
           INQUIRE( UNIT=J, OPENED=FLAG )
           IF ( .NOT. FLAG ) THEN
              JUNIT = J
              RETURN
           END IF 
11     END DO
       WRITE (*,91001) BOT, TOP, IUNIT
       STOP
       RETURN

91001 FORMAT (///, 1X, '*** ERROR ABORT IN ROUTINE JUNIT ***',
     &          /, 5X, 'NO MORE UNIT NUMBERS AVAILABLE FOR I/O',
     &          /, 5X, 'First POSIX-approved unit:', I4 ,
     &          /, 5X, 'Last  POSIX-approved unit:', I4 ,
     &          /, 5X, 'Current unit:             ', I4 ,
     &          //)
      END FUNCTION JUNIT

        INTEGER  FUNCTION  SECSDIFF ( ADATE, ATIME, ZDATE, ZTIME )

C***********************************************************************
C  FUNCTION:  returns the time interval (seconds) between ADATE:ATIME
C             and ZDATE:ZTIME
C  PRECONDITION:  normalized dates and times (0 <= SS <= 59, etc.)
C                 stored in format YYYYDDD:HHMMSS.
C***********************************************************************
      IMPLICIT NONE
C...........   ARGUMENTS and their descriptions:
        INTEGER, INTENT( IN ) :: ADATE, ATIME
        INTEGER, INTENT( IN ) :: ZDATE, ZTIME
C...........   Local Variables
        INTEGER         AYEAR,  ZYEAR, DAYS, HOURS, MINS, SECS
        INTEGER         TOTAL
C.......   Start with day, hour, min, sec differences:
        DAYS  = MOD( ZDATE,     1000 ) -  MOD( ADATE, 1000 )
        HOURS =      ZTIME / 10000     -       ATIME / 10000 
        MINS  = MOD( ZTIME/100, 100 )  -  MOD( ATIME/100, 100 )
        SECS  = MOD( ZTIME,     100 )  -  MOD( ATIME,     100 )

        TOTAL = 60 * ( 60 * ( 24 * DAYS + HOURS ) + MINS ) + SECS
C.......   Now add corrections for differences in years:
        AYEAR = ADATE / 1000
        ZYEAR = ZDATE / 1000
        DO
            IF ( AYEAR .GE. ZYEAR )  THEN
                EXIT
            ELSE IF ( MOD( AYEAR, 4   ) .NE. 0 ) THEN   !  nonleap
                TOTAL = TOTAL + 365 * 86400
            ELSE IF ( MOD( AYEAR, 100 ) .NE. 0 ) THEN   !  leap noncentury
                TOTAL = TOTAL + 366 * 86400
            ELSE IF ( MOD( AYEAR, 400 ) .NE. 0 ) THEN   !  nonleap century
                TOTAL = TOTAL + 365 * 86400
            ELSE                                        !  leap century
                TOTAL = TOTAL + 366 * 86400
            END IF
            AYEAR = AYEAR + 1
        END DO

        DO
            IF ( ZYEAR .GE. AYEAR )  THEN
                EXIT
            ELSE IF ( MOD( ZYEAR, 4   ) .NE. 0 ) THEN   !  nonleap
                TOTAL = TOTAL - 365 * 86400
            ELSE IF ( MOD( ZYEAR, 100 ) .NE. 0 ) THEN   !  leap noncentury
                TOTAL = TOTAL - 366 * 86400
            ELSE IF ( MOD( ZYEAR, 400 ) .NE. 0 ) THEN   !  nonleap century
                TOTAL = TOTAL - 365 * 86400
            ELSE                                        !  leap century
                TOTAL = TOTAL - 366 * 86400     
            END IF
            ZYEAR = ZYEAR + 1
        END DO

        SECSDIFF = TOTAL
        RETURN
        END FUNCTION SECSDIFF


      SUBROUTINE NEXTIME  ( JDATE , JTIME, DTIME )
C.....................................................................
C  FUNCTION:
C       Subroutine to add  DTIME  to the current time, and then
C       update the clock  JTIME  and calendar  JDATE  accordingly.  
C       Output is fully normalized.
C   JDATE is stored in the form   YYYYDDD = YEAR*1000  +  DAY
C   JTIME is stored in the form   HHMMSS  = HOUR*10000 +  MINS*100  +  SECS
C   DTIME is stored in the form   HHMMSS  = HOUR*10000 +  MINS*100  +  SECS
C  ARGUMENT LIST DESCRIPTION:   see below
C  LOCAL VARIABLE DESCRIPTION:  see below
C  CALLS:  none
C........................................................................
        IMPLICIT  NONE
C.......   ARGUMENTS:
        INTEGER         JTIME           !  time (encoded HHMMSS)
        INTEGER         JDATE           !  date (encoded DDDYY)
        INTEGER         DTIME           !  time increment (encoded HHMMSS)
C.......   LOCAL VARIABLES:  day and year components of date
        INTEGER         YEAR            !  year-component    of JDATE
        INTEGER         DAYS            !  day-component     of JDATE
        INTEGER         HOUR            !  hours-component   of JTIME
        INTEGER         MINS            !  minutes-component of JTIME
        INTEGER         SECS            !  seconds-component of JTIME
        INTEGER         SCR             !  scratch accumulator
        INTEGER         DIR             !  sign of DTIME
        INTEGER         ATIME           !  absolute value of DTIME
C.......   Increment seconds part of JTIME by DTIME (secs), and
C.......   re-normalize minute, hour, day part of JDATE:JTIME

        IF ( DTIME .GE. 0 ) THEN
            ATIME = DTIME
            DIR   = 1
        ELSE
            ATIME = - DTIME
            DIR   = - 1
        END IF

        SECS  =  MOD ( JTIME , 100 )  +  DIR * MOD ( ATIME , 100 )
        IF ( SECS .GE. 0 ) THEN
            SCR = SECS / 60
        ELSE
            SCR = - ( 60 - SECS ) / 60
        END IF

        SECS = SECS  -  SCR * 60
        MINS = SCR  +  MOD ( JTIME / 100 , 100 )
     &              +  DIR * MOD ( ATIME / 100 , 100 )

        IF ( MINS .GE. 0 ) THEN
            SCR = MINS / 60
        ELSE
            SCR = - ( 60 - MINS ) / 60
        END IF

        MINS  =  MINS  -  SCR * 60
        HOUR  =  JTIME / 10000  +  DIR * ( ATIME / 10000 )  +  SCR

        DAYS  =  MOD ( JDATE , 1000 )

        IF  ( HOUR .LT. 0 )  THEN

            SCR   =  ( 24 - HOUR ) / 24
            HOUR  =  HOUR  +  SCR * 24
            DAYS  =  DAYS  -  SCR

        ELSE IF  ( HOUR .GT. 23 )  THEN

            SCR   =  HOUR / 24
            HOUR  =  HOUR  -  SCR * 24
            DAYS  =  DAYS  +  SCR

        END IF

        JTIME  =  10000 * HOUR  +  100 * MINS  +  SECS


C...........   Update JDATE:
C...........   Note that each year must be treated individually

        YEAR  =  JDATE / 1000

100     CONTINUE        !  loop normalizing negative day numbers

            IF ( DAYS .LT. 0 ) THEN

                IF (           ( MOD (YEAR,4)   .NE. 1 )        !  nonleap year
     &             .OR. (      ( MOD (YEAR,100) .EQ. 1 )
     &                   .AND. ( MOD (YEAR,400) .NE. 1 ) ) ) THEN

                    DAYS  =  366   +  DAYS
                    YEAR  =  YEAR  -  1

                ELSE            !  leap-year case

                    DAYS  =  367   +  DAYS
                    YEAR  =  YEAR  -  1

                END IF

                GO TO  100

            END IF


200     CONTINUE        !  loop normalizing day numbers > 365,366

            IF ( DAYS .GE. 366 ) THEN

                IF (           ( MOD (YEAR,4)   .NE. 0 )        !  nonleap year
     &              .OR. (     ( MOD (YEAR,100) .EQ. 0 )
     &                   .AND. ( MOD (YEAR,400) .NE. 0 ) ) ) THEN

                    DAYS  =  DAYS   - 365
                    YEAR  =  YEAR  +   1

                    GO TO  200

                ELSE IF ( DAYS .GE. 367 ) THEN           !  leap year case

                    DAYS  =  DAYS   - 366
                    YEAR  =  YEAR  +   1

                    GO TO  200

                END IF
            END IF      !  end DAYS > 365,366 date-normalization loop

        JDATE  =   1000 * YEAR  +  DAYS


        RETURN
        END SUBROUTINE NEXTIME

        INTEGER  FUNCTION  SEC2TIME( SECS )

C***********************************************************************
C  function body starts at line  31
C
C  FUNCTION:  convert integer seconds to time difference format HHMMSS
C
C  RETURN VALUE:  integer HHMMSS formatted secs
C
C  REVISION  HISTORY:  
C       Prototype  5/92 by CJC
C       Version    3/93 by CJC for CRAY, etc.
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER    	SECS


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER        	ABSS


C***********************************************************************
C   begin body of function  SEC2TIME

        IF ( SECS .GE. 0 ) THEN		!  div-mod arithmetic OK
            SEC2TIME = MOD( SECS, 60 )  +  
     &                   100 * ( MOD( SECS / 60, 60 )  +  
     &                   100 * ( SECS / 3600 ) )
        ELSE				!  work with absolute values:
            ABSS     = - SECS
            SEC2TIME = - ( MOD( ABSS, 60 )  +  
     &                       100 * ( MOD( ABSS / 60, 60 )  +  
     &                       100 * ( ABSS / 3600 ) ) )
        END IF

        RETURN

        END FUNCTION  SEC2TIME
C
        CHARACTER(2) FUNCTION CRLF()

C***********************************************************************
C  subroutine body starts at line  39
C
C  FUNCTION:  Return character string of character return and line feed
C
C  PRECONDITIONS REQUIRED:
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       prototype 1/97 by M Houyoux
C       Modified 5/2003 by Carlie J. Coats, Jr., BAMS,  so that it
C       works correctly for "normal" UNIX systems.
C****************************************************************************

        IMPLICIT NONE

        CHARACTER*2 VALUE
        SAVE        VALUE

        LOGICAL     FIRSTIME
        DATA        FIRSTIME / .TRUE. /
        SAVE        FIRSTIME

C***********************************************************************
C   begin body of subroutine CRLF

        IF( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
#ifdef _WIN32
            VALUE( 1:1 ) = CHAR( 13 )
            VALUE( 2:2 ) = CHAR( 10 )
#endif
#ifndef _WIN32
            VALUE( 1:1 ) = ' '
            VALUE( 2:2 ) = CHAR( 10 )
#endif
        ENDIF

        CRLF = VALUE

        RETURN

        END FUNCTION CRLF

         REAL FUNCTION ENVREAL( LNAME, DESC, DEFAULT, STAT )

            USE GET_ENV_VARS, ONLY: GET_ENV_REAL

            IMPLICIT NONE

            CHARACTER*(*), INTENT(IN   ) :: LNAME
            CHARACTER*(*), INTENT(IN   ) :: DESC
            REAL         , INTENT(IN   ) :: DEFAULT
            INTEGER      , INTENT(  OUT) :: STAT

C             forces routine to use the default value
            ENVREAL = GET_ENV_REAL( LNAME, DESC, DEFAULT, STAT )
C           STAT    = -2
C           ENVREAL = -2
            RETURN

         END FUNCTION ENVREAL
	
         LOGICAL FUNCTION ENVYN( LNAME, DESC, DEFAULT, STATUS )
           IMPLICIT NONE
!arguments
           CHARACTER*(*), INTENT(  IN ) :: LNAME
           CHARACTER*(*), INTENT(  IN ) :: DESC
           LOGICAL,       INTENT(  IN ) :: DEFAULT
           INTEGER,       INTENT( OUT ) :: STATUS 
!local
           CHARACTER( 120 ) :: MSG         ! Message text
           CHARACTER(  1  ) :: VAR_VALUE
           CHARACTER(  5  ) :: REPLY

           INTEGER, PARAMETER :: LOGDEV = 6

           CALL GET_ENVIRONMENT_VARIABLE( NAME=LNAME, VALUE= VAR_VALUE, STATUS=STATUS, TRIM_NAME=.TRUE.)

           IF( STATUS .LT. 0 ) THEN
              MSG = 'ERROR in environment value for ' // TRIM( LNAME )
              WRITE(LOGDEV,'(a)')TRIM( MSG )
              STATUS = 1
           ELSE IF( STATUS .EQ. 1 )THEN
              STATUS  = -1
              ENVYN = DEFAULT
           ELSE IF( STATUS .GT. 1 )THEN
              STATUS  = 1
              MSG = 'ERROR in environment value for ' // TRIM( LNAME )
              WRITE(LOGDEV,'(a)')TRIM( MSG )
              MSG = 'ERROR Processor does not support environment variables '
              WRITE(LOGDEV,'(a)')
           ELSE
              STATUS = 0 
              IF( VAR_VALUE(1:1) .EQ. 'T' .OR. VAR_VALUE(1:1) .EQ. 'Y' )THEN
                  ENVYN = .TRUE.
              ELSE IF( VAR_VALUE(1:1) .EQ. 't' .OR. VAR_VALUE(1:1) .EQ. 'y' )THEN
                  ENVYN = .TRUE.
              ELSE IF(  VAR_VALUE(1:1) .EQ. 'F' .OR. VAR_VALUE(1:1) .EQ. 'N' )THEN
                  ENVYN = .FALSE.
              ELSE IF(  VAR_VALUE(1:1) .EQ. 'f' .OR. VAR_VALUE(1:1) .EQ. 'n' )THEN
                  ENVYN = .FALSE.
              ELSE 
                  WRITE(LOGDEV,' (A)')'Environment Variable WRITE_CGRID_DATA set to '
     &            // TRIM( LNAME ) // ' and must equal T, Y, F, or N.'
     &            // ' Using default value of F'
                  STATUS = 1
              END IF
           END IF
           IF( STATUS .EQ. 0 )THEN
              IF( ENVYN )THEN
                  REPLY = 'TRUE '
              ELSE
                  REPLY = 'FALSE'
              END IF
              WRITE(MSG,'(a,L1,a)')'     Value for ' // TRIM( LNAME ) // ': ',ENVYN,
     &        ' returning ' // REPLY
              WRITE(LOGDEV,'(a)')TRIM( MSG )
           END IF 

         END FUNCTION ENVYN
	

      integer function getefile( FNAME, LOGA, LOGB, PNAME)

        USE DRIVER_INPUTS
        IMPLICIT NONE
        CHARACTER*(*), INTENT(IN   ) :: FNAME      !  logical file name
        LOGICAL,       INTENT(IN   ) :: LOGA
        LOGICAL,       INTENT(IN   ) :: LOGB
        CHARACTER*(*), INTENT(IN   ) :: PNAME      !  logical file name
        INTEGER, EXTERNAL            :: JUNIT
        INTEGER                      :: IOST
        CHARACTER( 256 )             :: FILE_NAME
        CHARACTER( 256 )             :: FILE_LINE

       INTERFACE
         SUBROUTINE ENVSTR( VAR_NAME, VAR_DESC, VAR_DEFAULT, VAR_VALUE, STATUS ) 
!arguments
           CHARACTER*(*), INTENT(  IN ) :: VAR_NAME
           CHARACTER*(*), INTENT(  IN ) :: VAR_DESC
           CHARACTER*(*), INTENT(  IN ) :: VAR_DEFAULT
           CHARACTER*(*), INTENT( OUT ) :: VAR_VALUE
           INTEGER,       INTENT( OUT ) :: STATUS 
         END SUBROUTINE ENVSTR
         SUBROUTINE NAMEVAL( VAR_NAME, VAR_VALUE )
!arguments
           CHARACTER*(*), INTENT(  IN ) :: VAR_NAME
           CHARACTER*(*), INTENT( OUT ) :: VAR_VALUE
         END SUBROUTINE NAMEVAL
        END INTERFACE

        IF( TRIM( FNAME ) .EQ. 'XJ_DATA' )THEN
            GETEFILE = JUNIT()
!            print*, 'XJ_DATA = ', TRIM( FNAME )
            OPEN ( UNIT = GETEFILE, FILE = XJ_DATA, IOSTAT = IOST, ERR = 100 )
!        ELSE IF( TRIM( FNAME ) .EQ. 'OPTICS_DATA' )THEN
!            GETEFILE = JUNIT()
!            print*, 'OPTICS_DATA = ', TRIM( FNAME )
!            OPEN ( UNIT = GETEFILE, FILE = FNAME, IOSTAT = IOST )
!        ELSE IF( TRIM( FNAME ) .EQ. 'CSQY_DATA' )THEN
!            GETEFILE = JUNIT()
!            print*, 'CSQY_DATA = ', TRIM( FNAME )
!            OPEN ( UNIT = GETEFILE, FILE = FNAME, IOSTAT = IOST )
!        ELSE
!             GETEFILE = -1
        ELSE 
             CALL NAMEVAL( FNAME, FILE_NAME )
             GETEFILE = JUNIT()
             OPEN ( UNIT = GETEFILE, FILE = FILE_NAME, IOSTAT = IOST, ERR = 100 )
        END IF
 
        RETURN
        PRINT*,'getefile: UNABLE to open file, ',TRIM( FILE_NAME )
100     GETEFILE = -1
        
      end function getefile

         SUBROUTINE ENVSTR( VAR_NAME, VAR_DESC, VAR_DEFAULT, VAR_VALUE, STATUS )
           IMPLICIT NONE
!arguments
           CHARACTER*(*), INTENT(  IN ) :: VAR_NAME
           CHARACTER*(*), INTENT(  IN ) :: VAR_DESC
           CHARACTER*(*), INTENT(  IN ) :: VAR_DEFAULT
           CHARACTER*(*), INTENT( OUT ) :: VAR_VALUE
           INTEGER,       INTENT( OUT ) :: STATUS 
!local
           CHARACTER( 1000 )    :: MSG         ! Message text
           INTEGER, PARAMETER  :: LOGDEV = 6

           CALL GET_ENVIRONMENT_VARIABLE( NAME=VAR_NAME, VALUE= VAR_VALUE, STATUS=STATUS, TRIM_NAME=.TRUE.)

           IF( STATUS .LT. 0 ) THEN
              MSG = 'ERROR in environment value for ' // TRIM( VAR_NAME )
              WRITE(LOGDEV,'(a)')TRIM( MSG )
           ELSE IF( STATUS .EQ. 1 )THEN
!              STATUS  = 0
              VAR_VALUE = TRIM( VAR_DEFAULT ) 
              MSG = 'ERROR in environment value ' // TRIM( VAR_NAME )
     &           // ' not set so using default value. '
              WRITE(LOGDEV,'(a)')TRIM( MSG )
           ELSE IF( STATUS .GT. 1 )THEN
              STATUS  = 1
              MSG = 'ERROR in environment value for ' // TRIM( VAR_NAME )
              WRITE(LOGDEV,'(a)')TRIM( MSG )
              MSG = 'ERROR Processor does not support environment variables '
              WRITE(LOGDEV,'(a)')
           END IF
           IF( STATUS .EQ. 0 .OR. STATUS .EQ. 1 )THEN
               MSG = 'ENVSTR: Value for ' // TRIM( VAR_NAME ) // ': ' // TRIM( VAR_VALUE )
               WRITE(LOGDEV,'(a)')TRIM( MSG )
           END IF

         END SUBROUTINE ENVSTR
         SUBROUTINE NAMEVAL( VAR_NAME, VAR_VALUE )
           IMPLICIT NONE
!arguments
           CHARACTER*(*), INTENT(  IN ) :: VAR_NAME
           CHARACTER*(*), INTENT( OUT ) :: VAR_VALUE
!local
           CHARACTER( 1500 )  :: MSG         ! Message text
           INTEGER            :: STATUS 
           INTEGER, PARAMETER :: LOGDEV = 6

           CALL GET_ENVIRONMENT_VARIABLE( NAME=VAR_NAME, VALUE= VAR_VALUE, STATUS=STATUS, TRIM_NAME=.TRUE.)

           IF( STATUS .LT. 0 ) THEN
              MSG = 'ERROR in environment value for ' // TRIM( VAR_NAME )
              WRITE(LOGDEV,'(a)')TRIM( MSG )
           ELSE IF( STATUS .EQ. 1 )THEN
!              STATUS  = 0
              MSG = 'ERROR in environment value ' // TRIM( VAR_NAME )
     &           // ' not set. '
              WRITE(LOGDEV,'(a)')TRIM( MSG )
              VAR_VALUE = TRIM( VAR_NAME ) 
           ELSE IF( STATUS .GT. 1 )THEN
!              STATUS  = 1
              MSG = 'ERROR in environment value for ' // TRIM( VAR_NAME )
              WRITE(LOGDEV,'(a)')TRIM( MSG )
              MSG = 'ERROR Processor does not support environment variables '
              WRITE(LOGDEV,'(a)')
           END IF
           IF( STATUS .EQ. 0 .OR. STATUS .EQ. 1 )THEN
               MSG = 'NAMEVAL: Value for ' // TRIM( VAR_NAME ) // ': ' // TRIM( VAR_VALUE )
               WRITE(LOGDEV,'(a)')TRIM( MSG )
           END IF

         END SUBROUTINE NAMEVAL


