      MODULE M3UTILIO

       IMPLICIT NONE
         INCLUDE 'PARMS3.EXT'        !  I/O API PARAMETERs
         INCLUDE 'FDESC3.EXT'        !  I/O API file headers
         INCLUDE 'IODECL3.EXT'       !  I/O API function declarations

         INTEGER, PARAMETER :: MAX_IO_STRLEN = 10000
   
       INTERFACE XTRACT3
          MODULE PROCEDURE XTRACT3_0D,
     &                     XTRACT3_1D,
     &                     XTRACT3_2D,
     &                     XTRACT3_2DI,
     &                     XTRACT3_3D,
     &                     XTRACT3_4D
       END INTERFACE
       INTERFACE WRITE3
          MODULE PROCEDURE WRITE3R2D,
     &                     WRITE3I,
     &                     WRITE3R3D,
     &                     WRITE3R4D
       END INTERFACE


       INTERFACE
         SUBROUTINE FETCH_CHR ( FILDEV, INBUF , LPOS , EOL , CHR )
           IMPLICIT NONE
           CHARACTER*1  CHR     ! Last character read
           CHARACTER*(*) INBUF  ! Input buffer -- 1 line
           INTEGER EOL          ! Last non-blank character on line
           INTEGER FILDEV       ! Unit number for ASCII file
           INTEGER LPOS         ! Read position on input line
         END SUBROUTINE FETCH_CHR
       END INTERFACE
       INTERFACE
         SUBROUTINE SKIP_COMMENT (FILDEV, INBUF , LPOS , EOL , LINNUM ,CHR)
           IMPLICIT NONE
           CHARACTER*1  CHR          ! Last character read
           CHARACTER*256 INBUF  ! Input buffer -- 1 line
           INTEGER EOL          ! Last non-blank character on line
           INTEGER FILDEV       ! Unit number for ASCII file
           INTEGER LINNUM       ! Current line number read in ASCII file
           INTEGER LPOS         ! Read position on input line
         END SUBROUTINE SKIP_COMMENT
       END INTERFACE
       
       INTERFACE
         SUBROUTINE GETREAL ( FILDEV , INBUF , LPOS, EOL , LINNUM, CHR,
     &                     NUMBER )
           CHARACTER*1  CHR          ! Last character read
           CHARACTER*100 INBUF       ! Input buffer -- 1 line
           INTEGER EOL          ! Last non-blank character on line
           INTEGER FILDEV       ! Unit number for ASCII file
           INTEGER LINNUM       ! Current line number read in ASCII file
           INTEGER LPOS         ! Read position on input line
           REAL   NUMBER        ! Number found
         END SUBROUTINE GETREAL
       END INTERFACE
       
            INTERFACE
                LOGICAL FUNCTION ENVYN( LNAME, DESC, DEFAULT, STAT )
                CHARACTER*(*), INTENT(IN   ) :: LNAME
                CHARACTER*(*), INTENT(IN   ) :: DESC
                LOGICAL      , INTENT(IN   ) :: DEFAULT
                INTEGER      , INTENT(  OUT) :: STAT
                END FUNCTION ENVYN
            END INTERFACE


!      INTERFACE
!        LOGICAL FUNCTION  CLOSE3( FNAME )
!           IMPLICIT NONE
!           CHARACTER*(*), INTENT(IN   ) :: FNAME   !  logical name of file Cto be opened
!        END FUNCTION  CLOSE3
!       END INTERFACE
       
       INTERFACE
         SUBROUTINE FETCH_WORD ( FILDEV , INBUF , LPOS, EOL , LINNUM, CHR ,
     &                     WORD )
           IMPLICIT NONE
           CHARACTER*1   CHR         ! Last character read
           CHARACTER*(*) INBUF       ! Input buffer -- 1 line
           CHARACTER*(*) WORD        ! Last word read
           INTEGER EOL          ! Last non-blank character on line
           INTEGER FILDEV       ! Unit number for ASCII file
           INTEGER LINNUM       ! Current line number read in ASCII file
           INTEGER LPOS         ! Read position on input line
         END SUBROUTINE FETCH_WORD
       END INTERFACE
       INTERFACE
         CHARACTER(2) FUNCTION CRLF()
           IMPLICIT NONE
         END FUNCTION CRLF
       END INTERFACE
!      INTERFACE
!        LOGICAL FUNCTION INTERPX ( FNAME, VNAME, CALLER,
!    &                    COL0, COL1, ROW0, ROW1, LAY0, LAY1,
!    &                    JDATE, JTIME, BUFFER )
!         IMPLICIT NONE
!          INCLUDE SUBST_SCENDATA
!         CHARACTER*(*)   FNAME   !  logical name of file to be "opened"
!         CHARACTER*(*)   CALLER  !  Calling program name
!         CHARACTER*(*)   VNAME   !  variable name
!         INTEGER   COL0        !  COLUMN for value
!         INTEGER   COL1        !  COLUMN for value
!         INTEGER   ROW0        !  ROW for value
!         INTEGER   ROW1        !  ROW for value
!         INTEGER   LAY0        !  LAYER for value
!         INTEGER   LAY1        !  LAYER for value
!         INTEGER   JDATE       !  Date for value
!         INTEGER   JTIME       !  Time for value
!         REAL   BUFFER ( : ) !  interpolation-output buffer array
!        END FUNCTION INTERPX                               
!      END INTERFACE
!      INTERFACE
!        LOGICAL FUNCTION  OPEN3 ( FNAME , FSTATUS , PNAME)
!          IMPLICIT NONE
!          CHARACTER*(*)   FNAME   !  logical name of file to be "opened"
!          CHARACTER*(*)   PNAME   !  Calling program name
!          INTEGER   FSTATUS       !  Not used
!        END FUNCTION OPEN3         
!        LOGICAL FUNCTION  DESC3 ( FNAME )
!          IMPLICIT NONE
!          CHARACTER*(*)   FNAME   !  logical name of file to be described.
!        END FUNCTION DESC3
!      END INTERFACE
!      INTERFACE
!        LOGICAL FUNCTION  CHECK3 ( FNAME , VNAME , JDATE , JTIME )
!          IMPLICIT NONE
!          CHARACTER*(*)   FNAME   !  logical name of file to be "opened"
!          CHARACTER*(*)   VNAME   !  Variable to check
!          INTEGER   JDATE       !  Model date
!          INTEGER   JTIME       !  Model time
!        END FUNCTION CHECK3
!      END INTERFACE
       INTERFACE
      subroutine m3exit ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
      implicit none
      character*(*) pname
      character*(*) xmsg
      integer jdate, jtime, xstat1
      end subroutine m3exit
       END INTERFACE
       INTERFACE
      subroutine m3warn ( PNAME, JDATE, JTIME, XMSG )
      implicit none
      character*(*) pname
      character*(*) xmsg
      integer jdate, jtime
      end subroutine m3warn
       END INTERFACE
       INTERFACE
      subroutine pm3warn ( PNAME, JDATE, JTIME, XMSG )
        implicit none
        character*16 pname
        character*(*) xmsg
        integer jdate, jtime
      end subroutine pm3warn
       END INTERFACE
       INTERFACE
       END INTERFACE
       INTERFACE
         INTEGER FUNCTION INDEX1 (NAME, N, NLIST)
           IMPLICIT NONE
           CHARACTER*(*), INTENT ( IN ) :: NAME        !  Character string being searched for
           INTEGER,       INTENT ( IN ) :: N           !  Length of array to be searched
           CHARACTER*(*), INTENT ( IN ) :: NLIST(*)    !  array to be searched
         END FUNCTION INDEX1
       END INTERFACE
       INTERFACE
        INTEGER   FUNCTION  TIME2SEC ( TIME )
          IMPLICIT NONE
          INTEGER*4       TIME
        END FUNCTION TIME2SEC
       END INTERFACE
       INTERFACE
        SUBROUTINE  UPCASE ( BUFFER )
          IMPLICIT NONE
          CHARACTER*(*)   BUFFER
        END SUBROUTINE UPCASE
       END INTERFACE
       INTERFACE
        CHARACTER(10) FUNCTION  HHMMSS ( JTIME )
          IMPLICIT NONE
          INTEGER*4       JTIME   !  Julian time, coded YYYYDDD
        END FUNCTION HHMMSS
       END INTERFACE
       INTERFACE
         INTEGER FUNCTION  JUNIT()
         IMPLICIT NONE
         END FUNCTION JUNIT
       END INTERFACE
       INTERFACE
        INTEGER  FUNCTION  SECSDIFF ( ADATE, ATIME, ZDATE, ZTIME )
          IMPLICIT NONE
          INTEGER         ADATE, ATIME
          INTEGER         ZDATE, ZTIME
        END FUNCTION  SECSDIFF
       END INTERFACE

       INTERFACE
           integer function getefile( FNAME, LOGA, LOGB, PNAME)
           IMPLICIT NONE
           CHARACTER*(*), INTENT(IN   ) :: FNAME      !  logical file name
           LOGICAL,       INTENT(IN   ) :: LOGA
           LOGICAL,       INTENT(IN   ) :: LOGB
           CHARACTER*(*), INTENT(IN   ) :: PNAME      !  logical file name
         end function getefile
       END INTERFACE

       INTERFACE
         SUBROUTINE NEXTIME  ( JDATE , JTIME, DTIME )
           IMPLICIT  NONE
           INTEGER         JTIME           !  time (encoded HHMMSS)
           INTEGER         JDATE           !  date (encoded DDDYY)
           INTEGER         DTIME           !  time increment (encoded HHMMSS)
         END SUBROUTINE NEXTIME
       END INTERFACE
       INTERFACE
         INTEGER  FUNCTION  SEC2TIME( SECS )
           IMPLICIT NONE
           INTEGER SECS
         END FUNCTION  SEC2TIME
       END INTERFACE
       INTERFACE
         REAL FUNCTION ENVREAL( LNAME, DESC, DEFAULT, STAT )
         CHARACTER*(*), INTENT(IN   ) :: LNAME
         CHARACTER*(*), INTENT(IN   ) :: DESC
         REAL         , INTENT(IN   ) :: DEFAULT
         INTEGER      , INTENT(  OUT) :: STAT
         END FUNCTION ENVREAL
       END INTERFACE
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
     
       INTEGER, PARAMETER, PRIVATE :: LOGDEV = 6
       private :: quicksort

       INTERFACE
        LOGICAL FUNCTION INTERPX ( FNAME, VNAME, CALLER,
     &                    COL0, COL1, ROW0, ROW1, LAY0, LAY1,
     &                    JDATE, JTIME, BUFFER )
         CHARACTER(LEN=*), INTENT(IN   ) :: FNAME           !  logical file name
         CHARACTER(LEN=*), INTENT(IN   ) :: VNAME           !  variable name, or 'ALL'
         CHARACTER(LEN=*), INTENT(IN   ) :: CALLER          !  name of caller
         INTEGER,          INTENT(IN   ) :: LAY0            !  lower layer bound for XTRACT3
         INTEGER,          INTENT(IN   ) :: LAY1            !  upper layer bound for XTRACT3
         INTEGER,          INTENT(IN   ) :: ROW0            !  lower row   bound for XTRACT3
         INTEGER,          INTENT(IN   ) :: ROW1            !  upper row   bound for XTRACT3
         INTEGER,          INTENT(IN   ) :: COL0            !  lower col   bound for XTRACT3
         INTEGER,          INTENT(IN   ) :: COL1            !  upper col   bound for XTRACT3
         INTEGER,          INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
         INTEGER,          INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
         REAL,             INTENT(  OUT) :: BUFFER( * )     !  interpolation-output buffer array
        END FUNCTION INTERPX
!        LOGICAL FUNCTION XTRACT3( FNAME, VNAME,
!     &                            LAY0, LAY1, ROW0, ROW1, COL0, COL1,
!     &                            JDATE, JTIME, BUFFER )
!         CHARACTER(LEN=*), INTENT(IN   ) :: FNAME           !  logical file name
!         CHARACTER(LEN=*), INTENT(IN   ) :: VNAME           !  variable name, or 'ALL'
!         INTEGER,          INTENT(IN   ) :: LAY0            !  lower layer bound for XTRACT3
!         INTEGER,          INTENT(IN   ) :: LAY1            !  upper layer bound for XTRACT3
!         INTEGER,          INTENT(IN   ) :: ROW0            !  lower row   bound for XTRACT3
!         INTEGER,          INTENT(IN   ) :: ROW1            !  upper row   bound for XTRACT3
!         INTEGER,          INTENT(IN   ) :: COL0            !  lower col   bound for XTRACT3
!         INTEGER,          INTENT(IN   ) :: COL1            !  upper col   bound for XTRACT3
!         INTEGER,          INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
!         INTEGER,          INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
!         REAL,             INTENT(  OUT) :: BUFFER( * )     !  interpolation-output buffer array
!        END FUNCTION XTRACT3
        LOGICAL   FUNCTION  CHECK3 ( FNAME , VNAME , JDATE , JTIME )
         CHARACTER(LEN=*), INTENT( IN ) :: FNAME   !  logical name of file to be "opened"
         CHARACTER(LEN=*), INTENT( IN ) :: VNAME   !  Variable to check
         INTEGER,          INTENT( IN ) :: JDATE       !  Model date
         INTEGER,          INTENT( IN ) :: JTIME       !  Model time
        END FUNCTION CHECK3
        LOGICAL   FUNCTION  OPEN3 ( FNAME , FSTATUS , PNAME)
         CHARACTER(LEN=*), INTENT( IN ) :: FNAME   !  logical name of file to be "opened"
         CHARACTER(LEN=*), INTENT( IN ) :: PNAME   !  Calling program name
         INTEGER,          INTENT( IN ) :: FSTATUS       !  Not used
        END FUNCTION OPEN3     
        LOGICAL FUNCTION  DESC3 ( FNAME )
         CHARACTER(LEN=*), INTENT( IN ) ::  FNAME   !  logical name of file to be described.
        END FUNCTION DESC3
        LOGICAL FUNCTION  SHUT3 ( )
        END FUNCTION SHUT3
        LOGICAL FUNCTION  CLOSE3 ( FNAME )
         CHARACTER(LEN=*), INTENT( IN ) ::  FNAME   !  logical name of file to be described.
        END FUNCTION CLOSE3
        LOGICAL FUNCTION READ3 ( FNAME, VNAME, LAY1, JDATE, JTIME, BUFFER )
         CHARACTER(LEN=*), INTENT(IN   ) :: FNAME           !  logical file name
         CHARACTER(LEN=*), INTENT(IN   ) :: VNAME           !  variable name, or 'ALL'
         INTEGER         , INTENT(IN   ) :: LAY1            !  upper layer bound for READ3
         INTEGER         , INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
         INTEGER         , INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
         REAL            , INTENT(  OUT) :: BUFFER( * )     !  interpolation-output buffer array
        END FUNCTION READ3 
       END INTERFACE

       INTERFACE
        LOGICAL FUNCTION FLUSH3 ( FNAME )
         CHARACTER(LEN=*), INTENT(IN   ) :: FNAME   !  logical name of file to be closed
        END FUNCTION FLUSH3
       END INTERFACE 

       PUBLIC XTRACT3

      TYPE OUTPUT_FILE
         CHARACTER(LEN=NAMLEN3) :: FILENAME = ''
         INTEGER                :: NVARS    = 0
         INTEGER                :: IO_UNIT  = -1
         INTEGER                :: NDIM     = -1
         INTEGER                :: JDATE    = -999999
         INTEGER                :: JTIME    = 0
         INTEGER                :: NSTEPS   = 0
         INTEGER                :: FILLED   = 0
         LOGICAL                :: FLUSHED  = .TRUE.
         LOGICAL                :: HEADER   = .TRUE.
         CHARACTER(LEN=16), ALLOCATABLE :: VARNAMES(:)
         CHARACTER(LEN=16), ALLOCATABLE :: UNITS(:)
         REAL,              ALLOCATABLE :: VALUES(:)
      END TYPE OUTPUT_FILE

      INTEGER :: N_OUTPUT_FILES = 0
      TYPE( OUTPUT_FILE )  :: OUTPUT_FILES( MXFILE3 )

      CONTAINS

        LOGICAL FUNCTION SETUP_OUTPUT_FILE( FILENAME,NVARS,UNITS,VARNAMES,NDIM,IO_UNIT,JDATE,JTIME,NFILE )

          IMPLICIT NONE

          CHARACTER(LEN=*), INTENT( IN ) :: FILENAME
          INTEGER,          INTENT( IN ) :: NVARS
          CHARACTER(LEN=*), INTENT( IN ) :: VARNAMES(:)
          CHARACTER(LEN=*), INTENT( IN ) :: UNITS(:)
          INTEGER,          INTENT( IN ) :: IO_UNIT
          INTEGER,          INTENT( IN ) :: NDIM
          INTEGER,          INTENT( IN ) :: JDATE
          INTEGER,          INTENT( IN ) :: JTIME
          INTEGER,          INTENT( IN ) :: NFILE

          INTEGER :: ALSTAT, NVAR

          SETUP_OUTPUT_FILE = .TRUE.

          OUTPUT_FILES(NFILE)%FILENAME = TRIM(FILENAME)
          OUTPUT_FILES(NFILE)%NVARS = NVARS
          OUTPUT_FILES(NFILE)%NDIM  = NDIM
          OUTPUT_FILES(NFILE)%JDATE = JDATE
          OUTPUT_FILES(NFILE)%JTIME = JTIME
          OUTPUT_FILES(NFILE)%IO_UNIT = IO_UNIT

          ALLOCATE( OUTPUT_FILES(NFILE)%VARNAMES( NVARS ),
     &              OUTPUT_FILES(NFILE)%UNITS( NVARS ),
     &              OUTPUT_FILES(NFILE)%VALUES( NVARS ),
     &              STAT =  ALSTAT)
          IF( ALSTAT .NE. 0 )THEN
            WRITE(6,'(A)')"ALLOCATION ERROR in M3UTILIO FUNCTION: SETUP_OUTPUT_FILE"
            SETUP_OUTPUT_FILE = .FALSE.
            RETURN
          ELSE
            WRITE(6,'(A)')"SETUP_OUTPUT_FILE: Set file," // TRIM(OUTPUT_FILES(NFILE)%FILENAME)
          END IF

          DO NVAR = 1,NVARS
             OUTPUT_FILES(NFILE)%VARNAMES( NVAR ) = VARNAMES( NVAR )
             OUTPUT_FILES(NFILE)%UNITS( NVAR )    = UNITS( NVAR )
             OUTPUT_FILES(NFILE)%VALUES( NVAR )   = AMISS3
          END DO
          
        END FUNCTION SETUP_OUTPUT_FILE
      INTEGER FUNCTION LBLANK( STRING )

C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C    function body starts at line 42
C
C  FUNCTION:
C
C    Returns the number of leading blanks in STRING
C
C  REVISION HISTORY:
C
C       Adapted  09/1995 from ROM utility routine LEN2() by CJC
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C.......   Arguments and their descriptions:

      CHARACTER*(*), INTENT(IN   ) :: STRING    !  Character string to search

C.......   PARAMETERs:

      CHARACTER*1, PARAMETER :: BLANK = ' '


C.......   Local variable:  loop counter

      INTEGER       I, L

C........................................................................
C.......   begin body:  Scan from left to right until non blank character

      L = LEN( STRING )
      DO  100  I = 1 , L

          IF ( ( STRING( I:I ) .NE. BLANK ) .AND.
     &         ( STRING( I:I ) .NE. CHAR( 9 ) ) ) THEN
              LBLANK = I - 1
              RETURN
          END IF

100   CONTINUE

      LBLANK = L
      RETURN

      END FUNCTION LBLANK
        CHARACTER*24 FUNCTION  DT2STR ( JDATE , JTIME )

C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  60
C
C  FUNCTION:  format and return the date and time as a character string
C             "HH:MM:SS  M+ D+, YYYY"
C
C
C  PRECONDITIONS REQUIRED:  valid Julian date YYYYDDD, time HHMMSS
C
C
C  RETURN VALUE:  date & time, as "HH:MM:SS  MMM DD, YYYY"
C
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C
C  REVISION  HISTORY:  
C       prototype 10/90 by CJC
C
C       Version    2/93 by CJC for CRAY, etc.
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: JDATE   !  Julian date, coded YYYYDDD
        INTEGER, INTENT(IN   ) :: JTIME   !  time, coded HHMMSS


C...........  EXTERNAL FUNCTIONS:

        CHARACTER*10, EXTERNAL :: HHMMSS

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER       J, T

        CHARACTER(10)    TIMBUF
        CHARACTER(24)    DATBUF


C***********************************************************************
C   begin body of function  DT2STR

        J = JDATE
        T = JTIME
        CALL NEXTIME( J, T, 0 )
        TIMBUF = HHMMSS( T )
        DATBUF = MMDDYY( J )
        DT2STR = TIMBUF // DATBUF

        RETURN

        END FUNCTION  DT2STR


        LOGICAL FUNCTION ISDSTIME( JDATE )

C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  70
C
C  RETURNS:  
C       TRUE iff Daylight Savings Time is in effect for JDATE,
C       calculated using Zeller's Congruence to find the 
C       starting and ending dates for Daylight Savings Time:
C       pre-2007:  first Sunday in April and the last Sunday in October
C       post-2007: secnd Sunday in March and the first Sunday in November
C
C  PRECONDITIONS REQUIRED:
C       JDATE represents a date YYYYDDD according to Models-3 conventions
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       None
C
C  REVISION  HISTORY:
C       Prototype  8/1995 by Carlie J. Coats, Jr., MCNC Environmental Programs
C       
C       Unification 2/2002 by CJC with global-climate ISDST, which
C       uses a 360-day "year"
C
C       Bugfix     7/2002 by CJC
C
C       Version 1/2007 by CJC:  update for 2007 Daylight Saving Time changes;
C       handle negative JDATE arguments correctly
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT( IN ) :: JDATE   !  Julian date, coded YYYYDDD


C...........   PARAMETERS and their descriptions:

        INTEGER, PARAMETER :: APR1  =  91
        INTEGER, PARAMETER :: OCT31 = 304


C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        
        INTEGER         YEAR, IDAY
        INTEGER         DAY0, DAY1
        INTEGER         K, IBIAS
        

C***********************************************************************
C   begin body of function  ISDSTIME

        IF ( JDATE .GE. 1000 ) THEN
            YEAR = JDATE / 1000
            IDAY = MOD ( JDATE , 1000 )
        ELSE
            YEAR  = -JDATE
            YEAR  = YEAR / 1000 + 2800  !  leap-year/day-of-week cycle 2800
            IBIAS = 2800000 * ( YEAR / 2800 )
            YEAR =     ( JDATE + IBIAS ) / 1000
            IDAY = MOD ( JDATE + IBIAS   , 1000 )
        END IF


C.......   Uses Zeller's Congruence calculation of day of wk for APR1, OCT31:
C.......   wkday = 1 + mod( k + day, 7 )

        DAY0  = APR1        !  get first Sunday in April,
        DAY1  = OCT31       !  last Sunday in October

        IF (          ( MOD (YEAR,4)   .EQ. 0 )        !  leap year adjustment
     &     .AND. (    ( MOD (YEAR,100) .NE. 0 )        !  month >= FEB
     &           .OR. ( MOD (YEAR,400) .EQ. 0 ) ) ) THEN

            DAY0 = DAY0 + 1
            DAY1 = DAY1 + 1

        END IF

        K     = YEAR - 1
        K     = K * 365  +  K / 4  -  K / 100  +  K / 400  -  1
        IF ( YEAR .LT. 2007 ) THEN
            DAY0  = DAY0 + 6 - MOD( K + DAY0, 7 ) !  first Sun. in Apr.
            K     = 1 + MOD( K + DAY1, 7 )        !  day-number 1...7 for OCT31
            DAY1  = DAY1     - MOD( K, 7 )        !  last  Sun. in Oct.
        ELSE
            DAY0  = DAY0 - 31                      !  March 1
            DAY0  = DAY0 + 13 - MOD( K + DAY0, 7 ) !  second Sunday in March,
            K     = 1 + MOD( K + DAY1, 7 )         !  day-number 1...7 for OCT31
            DAY1  = DAY1 +  6 - MOD( K, 7 )        !  first Sunday in November
        END IF

        ISDSTIME = ( ( IDAY .GE. DAY0 ) .AND. ( IDAY .LT. DAY1 ) )

        RETURN

        END FUNCTION ISDSTIME

        SUBROUTINE GETDTTIME( IDATE, ITIME )

           IMPLICIT NONE

            INTEGER, INTENT( INOUT ) :: IDATE
            INTEGER, INTENT( INOUT ) :: ITIME

               IDATE = 0
               ITIME = 0

        END SUBROUTINE GETDTTIME

         SUBROUTINE DAYMON(JDATE,MONTH,DAY)

            IMPLICIT NONE

c     return month, day
c     input julian date

           integer, intent( in  ) :: jdate
           integer, intent( out ) :: month
           integer, intent( out ) :: day

           integer   :: year, leap,m4,m100,m400
           integer   :: i
           integer   :: jday, test_day


           integer, save ::  days_per_month(13,2)
           data days_per_month / 0,31,28,31,30,31,30,31,31,30,31,30,31,
     +                      0,31,29,31,30,31,30,31,31,30,31,30,31 /

           integer, save ::  jdays_per_year(13,2)

           logical, save :: initialize = .true.

           if ( initialize ) then
                jdays_per_year = 0
                jdays_per_year(1,1) = days_per_month(2,1)
                jdays_per_year(1,2) = days_per_month(2,2)
                do i = 2,13
                   do leap = 1,2
                      jdays_per_year(i,leap) = jdays_per_year(i-1,leap)
     &                                       + days_per_month(i,leap)
                   end do
                end do
                initialize = .false.
           endif
           year =   int( jdate/1000 )
           jday =   mod( jdate,1000 )
           m4       = mod(year,4  )
           m100     = mod(year,100)
           m400     = mod(year,400)
           if(((m4.eq.0).and.(m100.ne.0)).or.(m400.eq.0))then
            leap = 2
           else
            leap = 1
           endif

           jday = min( jdays_per_year(13,leap),max( jday,1 ) )
           do month = 1,12
              day = jday - jdays_per_year(month,leap) - 1
              if ( day .le. 0 ) then
                 if ( day .eq. 0 ) then
                    day = day + jdays_per_year(month,leap) + 1
                 end if
                 exit
              endif
           end do
           return

        END SUBROUTINE DAYMON
        LOGICAL FUNCTION  DSCGRID( GNAME, CNAME,
     &                     CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT,
     &                     XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, NTHIK )

           IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

           CHARACTER*(*), INTENT(INOUT) :: GNAME   !  grid  sys name
           CHARACTER*(*), INTENT(  OUT) :: CNAME   !  coord sys name
           INTEGER,       INTENT(  OUT) :: CTYPE   !  coord sys type
           REAL*8 ,       INTENT(  OUT) :: P_ALP   !  first, second, third map
           REAL*8 ,       INTENT(  OUT) :: P_BET   !  projection descriptive
           REAL*8 ,       INTENT(  OUT) :: P_GAM   !  parameters
           REAL*8 ,       INTENT(  OUT) :: XCENT   !  lon for coord-system X=0
           REAL*8 ,       INTENT(  OUT) :: YCENT   !  lat for coord-system Y=0
           REAL*8 ,       INTENT(  OUT) :: XORIG   !  X-coordinate origin of grid (map units)
           REAL*8 ,       INTENT(  OUT) :: YORIG   !  Y-coordinate origin of grid
           REAL*8 ,       INTENT(  OUT) :: XCELL   !  X-coordinate cell dimension
           REAL*8 ,       INTENT(  OUT) :: YCELL   !  Y-coordinate cell dimension
           INTEGER,       INTENT(  OUT) :: NCOLS   !  number of grid columns
           INTEGER,       INTENT(  OUT) :: NROWS   !  number of grid rows
           INTEGER,       INTENT(  OUT) :: NTHIK   !  BOUNDARY:  perimeter thickness (cells)

           GNAME = "M3BOX"
           CNAME = "M3BOX"

           CTYPE  = 1

           P_ALP  = 0.0D0
           P_BET  = 0.0D0
           P_GAM  = 0.0D0
           
           XCELL = 1.0D0
           YCELL = 1.0D0

           XCENT = 0.0D0
           YCENT = 0.0D0

           XORIG = 0.0D0
           YORIG = 0.0D0

           XCELL = 5.0D0
           YCELL = 5.0D0

           NROWS = 1
           NCOLS = 1
           NTHIK = 1

           DSCGRID = .TRUE.

        END FUNCTION DSCGRID
! -----------------------------------------------------------
          logical function leap_year (year)

! determine whether a given year is a leap year or not

            integer :: year

            if (mod(year, 4) .ne. 0) then
               leap_year = .false.
            else if (mod(year, 400) .eq. 0) then
               leap_year = .true.
            else if (mod(year, 100) .eq. 0) then
               leap_year = .false.
            else
               leap_year = .true.
            endif

          end function leap_year

! -------------------------------------------------------------------------
          subroutine date_time_minus_sec (date, time, minus_sec)

! subtract a Julian jdate and jtime in hhmmss format with minus_sec seconds

            integer, intent(inout) :: date        ! date in julian date format
            integer, intent(inout) :: time        ! time in hhmmss format
            integer, intent(in   ) :: minus_sec

            integer :: loc_year, loc_date, loc_sec, subtract_hr, subtract_min,
     &                 subtract_sec, temp, hr, min, sec, num_days

            loc_year = date / 1000
            loc_date = mod(date, 1000)

            num_days = minus_sec / 86400
            loc_sec = mod(minus_sec, 86400)
            loc_date = loc_date - num_days

            do while (loc_date <= 0)
               loc_year = loc_year - 1
               if (leap_year(loc_year)) then
                  loc_date = loc_date + 366
               else
                  loc_date = loc_date + 365
               end if
            end do

            subtract_hr  = loc_sec / 3600
            temp         = mod(loc_sec, 3600)
            subtract_sec = mod(temp, 60)
            subtract_min = temp / 60

            hr = time / 10000
            sec = mod(time, 100)
            min = mod(time/100, 100)

            sec = sec - subtract_sec

            if (sec .lt. 0) then
               min = min - 1 - subtract_min
               sec = sec + 60
            else
               min = min - subtract_min
            end if

            if (min .lt. 0) then
               hr = hr - 1 - subtract_hr
               min = min + 60
            else
               hr = hr - subtract_hr
            end if

            if (hr .lt. 0) then
               date = date - 1
               hr = hr + 24
               if (loc_date == 0) then
                  loc_year = loc_year - 1
                  if (leap_year(loc_year)) then
                     loc_date = 366
                  else
                     loc_date = 365
                  end if
               end if
            end if

            date = loc_year * 1000 + loc_date
            time = hr * 10000 + min * 100 + sec

          end subroutine date_time_minus_sec

! ------------------------------------------------------------------------------
          logical function currstep (jdate, jtime,
     &                               sdate, stime, tstep,
     &                               cdate, ctime )

! compute cdate and ctime from starting date and time, sdate and stime w.r.t.
! time step, tstep such that it is closest to and smaller than jdate and jtime

            integer, intent(in)  :: jdate, jtime, sdate, stime, tstep
            integer, intent(out) :: cdate, ctime

            integer :: sec_diff, tsec, mod_sec

            if ((jdate .lt. sdate) .or.
     &          ((jdate .eq. sdate) .and. (jtime .lt. stime))) then
               currstep = .false.
               cdate = 0
               ctime = 0
            else
               currstep = .true.
               sec_diff = secsdiff (sdate, stime, jdate, jtime)
               tsec = time2sec(tstep)

               mod_sec = mod(sec_diff, tsec)
               cdate = jdate
               ctime = jtime
               call date_time_minus_sec (cdate, ctime, mod_sec)
            end if

          end function currstep

! ------------------------------------------------------------------------------
          integer function findc (key, n, list)

! find the location of a key in a sorted list

            character*(*), intent(in) :: key
            integer, intent(in)       :: n
            character*(*), intent(in) :: list(n)

            integer :: top, bot, mid
            logical :: done

            findc = -1
            top = 1
            bot = n
            done = .false.
            do while (.not. done)
               mid = (top + bot) / 2
               if (top .le. bot) then
                  if (key == list(mid)) then
                     done = .true.
                     findc = mid
                  else if (key .lt. list(mid)) then
                     bot = mid - 1
                  else
                     top = mid + 1
                  end if
               else
                  done = .true.
               end if
            end do

          end function findc

! -----------------------------------------------------------
          integer function wkday ( jdate )

! determine the day of a week for a given Julian date
! based on Zeller's Rule, Monday = 1, Tuesday = 2, ... Sunday = 7
! TO DO: add optional argument for climatological 365-day year.
!        make some assumption regarding 'standard' year.

            integer, intent(in) :: jdate

            integer :: f, c, d, year, day

            year = jdate / 1000
            day  = mod(jdate, 1000)

! compute with respect to the first day of a year
            d = mod(year-1, 100)
            c = (year-1) / 100

! add day - 1 to calculate w.r.t. the actual day
!           f = 1 + ((13 * 11 - 1) / 5) + d + d / 4 + c / 4 - 2 * c + day - 1
            f = 28 + d + d / 4 + c / 4 - 2 * c + day

! to adjust the situation that Sunday = 7
            f = mod(f, 7)
            if (f .eq. 0) then
               f = 7
            end if

! to adjust the situation when f is negative
            if (f .lt. 0) then
               f = 1 + mod(f+6, 7)
            end if

            wkday = f

          end function wkday
! ------------------------------------------------------------------------------
          integer function promptffile (prompt, rdonly, fmtted, default, caller)

! prompt user to provide file characteristics and open the file accordingly

            character*(*), intent(in) :: prompt         !  prompt for user
            logical      , intent(in) :: rdonly         !  TRUE iff file is input-only
            logical      , intent(in) :: fmtted         !  TRUE iff file should be formatted
            character*(*), intent(in) :: default        !  default logical file name
            character*(*), intent(in) :: caller         !  caller-name for logging messages

            integer :: fnum

            fnum = getefile ( default, rdonly, fmtted, caller )

            promptffile = fnum

          end function promptffile
! ------------------------------------------------------------------------------
          real function str2real( string )

! convert a given string to real number

            character(*), intent(in) :: string

            read (string, *) str2real

          end function str2real
        CHARACTER(16) FUNCTION PROMPTMFILE( PROMPT, FMODE,
     &                                     DEFAULT, CALLER )

           IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:

             CHARACTER*(*), INTENT(IN   ) :: PROMPT         !  prompt for user
             INTEGER      , INTENT(IN   ) :: FMODE          !  file opening-mode
             CHARACTER*(*), INTENT(IN   ) :: DEFAULT        !  default logical file name
             CHARACTER*(*), INTENT(IN   ) :: CALLER         !  caller-name for logging messages


C...........   PARAMETERS:

             CHARACTER(16), PARAMETER :: BLANK16 = ' '
             CHARACTER(16), PARAMETER :: NONE16  = 'NONE'

             PROMPTMFILE = DEFAULT

         END FUNCTION PROMPTMFILE
        LOGICAL FUNCTION GRDCHK3( FNAME,
     &                            P_ALP, P_BET, P_GAM, XCENT, YCENT,
     &                            XORIG, YORIG, XCELL, YCELL,
     &                            NLAYS, VGTYP, VGTOP, VGLEV )

        IMPLICIT NONE


        !!........  Arguments:

        CHARACTER*(*), INTENT(IN   ) :: FNAME
        REAL*8       , INTENT(IN   ) :: P_ALP      ! first, second, third map
        REAL*8       , INTENT(IN   ) :: P_BET      ! projection descriptive
        REAL*8       , INTENT(IN   ) :: P_GAM      ! parameters.
        REAL*8       , INTENT(IN   ) :: XCENT      ! lon for coord-system X=0
        REAL*8       , INTENT(IN   ) :: YCENT      ! lat for coord-system Y=0
        REAL*8       , INTENT(IN   ) :: XORIG      ! X-coordinate origin of grid (map units)
        REAL*8       , INTENT(IN   ) :: YORIG      ! Y-coordinate origin of grid
        REAL*8       , INTENT(IN   ) :: XCELL      ! X-coordinate cell dimension
        REAL*8       , INTENT(IN   ) :: YCELL      ! Y-coordinate cell dimension
        INTEGER      , INTENT(IN   ) :: NLAYS      ! number of layers
        INTEGER      , INTENT(IN   ) :: VGTYP      ! vertical coordinate type
        REAL         , INTENT(IN   ) :: VGTOP
        REAL         , INTENT(IN   ) :: VGLEV( * )

           GRDCHK3 = .TRUE.
        END FUNCTION GRDCHK3
! ------------------------------------------------------------------------------
          integer function indexint1 (name, n, list)

! perform same function as in index1 but for integer

            integer, intent(in) :: name
            integer, intent(in) :: list(*)
            integer, intent(in) :: n

            integer :: i
            logical :: found

            i = 0
            found = .false.
            do while ((.not. found) .and. (i < n))
               i = i + 1
               if (name == list(i)) then
                  found = .true.
               end if
            end do

            if (.not. found) then
               indexint1 = 0
            else
               indexint1 = i
            end if

          end function indexint1
! ----------------------------------------------------------
          integer function julian (year, month, day)

! convert year month day to Julian date (without year) format

! TO DO: handle climatological 365-day years.
!        Maybe rather than optional argument, use a module variable for calendar type
!        What should ERRVAL be set to?

            implicit none

            integer, intent(in) :: year, month, day

            integer, parameter :: numday(12) =
     &                            (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
            integer :: i, temp_julian

            integer,parameter  :: ERRVAL = -1
            if ( month .le. 0 .or. month .gt. 12 .or. day .le. 0 ) then
               julian = ERRVAL
               return
            else if ( day .gt. numday(month) ) then
               julian = ERRVAL
               return
            end if

            i = 1
            temp_julian= 0
            do while (i < month)
              temp_julian= temp_julian+ numday(i)
              i = i + 1
            end do
            temp_julian= temp_julian+ day

            if (leap_year(year) .and. (month > 2)) then
               temp_julian= temp_julian+ 1
            end if

            julian = temp_julian

          end function julian
! ------------------------------------------------------------------------------
          character*14 function mmddyy (jdate)

! convert a Julian date to mmddyy format
! Modified to handle some error conditions.

            integer, intent(in) :: jdate

            integer, parameter :: numdays (12) = (/ 31, 28 ,31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
            character (len = 4) :: month_name (12)  = (/ 'Jan.', 'Feb.', 'Mar.', 'Apr.', 'May ', 'Jun.',
     &                                                 'Jul.', 'Aug.', 'Sep.', 'Oct.', 'Nov.', 'Dec.' /)
            character (len = 14) :: errstr = "<DATE ERROR>"

            integer :: year, day, numjdays(12), i, j
            logical :: done

            year = jdate / 1000
            if ( year .le. 0 ) then
               mmddyy = errstr
               return
            end if
            day  = mod(jdate, 1000)

! compute number of jdays
            numjdays(1) = 1
            numjdays(2) = 32
            if (leap_year(year)) then
               if (day .le. 0 .or. day .gt. 366) then
                  mmddyy = errstr
                  return
               end if
               numjdays(3) = numjdays(2) + 29
            else
               if (day .le. 0 .or. day .gt. 365) then
                  mmddyy = errstr
                  return
               end if
               numjdays(3) = numjdays(2) + 28
            end if
            do i = 4, 12
               numjdays(i) = numjdays(i-1) + numdays(i-1)
            end do

            i = 1
            done = .false.
            do while ((i < 13) .and. (.not. done))
               if (numjdays(i) .le. day) then
                  i = i + 1
               else
                  done = .true.
               end if
            end do

            if (i .gt. 1) then
               j = i - 1
            end if

            day = day + 1 - numjdays(j)

            if (day .lt. 10) then
               write (mmddyy, '(a4, 1x, i1, a1, 1x, i4)') month_name(j), day, ',', year
            else
               write (mmddyy, '(a4, 1x, i2, a1, 1x, i4)') month_name(j), day, ',', year
            end if

          end function mmddyy
! ------------------------------------------------------------------------------
          real function poly (pt, x, fx, n)

! Performs arbitrary-degree polynomial interpolation for pt on curve determined 
! by x and fx using Newton divided-differences. (from IOAPI)

            implicit none

            real, intent(in)    :: pt, x(n+1), fx(n+1)
            integer, intent(in) :: n

            real, allocatable :: fd(:,:)
            real :: prod, result
            integer :: stat, i, j

            allocate (fd(n, n), stat=stat)

            do i = 1, n
               fd(i,1) = (fx(i+1) - fx(i)) / (x(i+1) - x(i))
            end do

            do j = 2, n
               do i = 1, n-j+1
                  fd(i,j) = (fd(i+1,j-1) - fd(i,j-1)) / (x(j+i) - x(i))
               end do
            end do

            result = fx(1)
            prod = 1
            do j = 1, n
               prod = prod * (pt - x(j))
               result = result + prod * fd(1,j)
            end do

            poly = result
            deallocate (fd)

          end function poly
! ------------------------------------------------------------------------------
          subroutine sortic (n, index, name)

! sort an integer index w.r.t. a string list

            character (16), intent(in) :: name(:)
            integer, intent(in)        :: n
            integer, intent(inout)     :: index(:)

            call quicksort(n, name, index)

          end subroutine sortic

! -------------------------------------------------------------------------
          integer function quicksort_partition (n, name, index)

! quicksort operation

            character (16), intent(in) :: name(:)
            integer, intent(in) :: n
            integer, intent(inout) :: index(:)

            character (16) :: pivot_value
            integer :: p, q, open, temp

            pivot_value = name(index(1))
            temp = index(1)
            p = 1
            q = n
            open = 1
            do while (p .lt. q)
               if (open .eq. p) then
                  if (name(index(q)) .le. pivot_value) then
                     index(open) = index(q)
                     open = q
                     p = p + 1
                  else
                     q = q - 1
                  end if
               else
                  if (name(index(p)) .gt. pivot_value) then
                     index(open) = index(p)
                     open = p
                     q = q - 1
                  else
                     p = p + 1
                  end if
               end if
            end do

            index(p) = temp
            quicksort_partition = p

          end function quicksort_partition
! -------------------------------------------------------------------------
          recursive subroutine quicksort (n, name, index)

! recursive quicksort algorithm

            implicit none

            character (16), intent(in) :: name(:)
            integer, intent(in) :: n
            integer, intent(inout) :: index(:)

            integer :: pindex

            if (n .gt. 1) then
               pindex = quicksort_partition (n, name, index)

               call quicksort (pindex-1, name, index(1:pindex-1))
               call quicksort (n-pindex, name, index(min(pindex+1,n):n))
            end if

          end subroutine quicksort
! ------------------------------------------------------------------------------
          real function yr2day (year)

! compute the reciprocal of number of days in a given year
! TO DO: add optional argument for climatological 365-day calendar
            integer, intent(in) :: year

            if (leap_year(year)) then
               yr2day = 1.0/366.0
            else
               yr2day = 1.0/365.0
            end if

          end function yr2day
! ------------------------------------------------------------------------------
          LOGICAL FUNCTION FILCHK3( FNAME,
     &                              FTYPE, NCOLS, NROWS, NLAYS, NTHIK )
     
            IMPLICIT NONE

            CHARACTER(LEN=*), INTENT( IN ) :: FNAME !  logical file name
            INTEGER,          INTENT( IN ) :: FTYPE !  user's queried file type
            INTEGER,          INTENT( IN ) :: NCOLS !  user's queried col-dimension
            INTEGER,          INTENT( IN ) :: NROWS !  user's queried row-dimension
            INTEGER,          INTENT( IN ) :: NLAYS !  user's queried lay-dimension
            INTEGER,          INTENT( IN ) :: NTHIK !  user's queried bdy-dimension

            FILCHK3 = .TRUE.

          END FUNCTION FILCHK3

          SUBROUTINE M3MSG2 ( MSG )
            IMPLICIT NONE
             CHARACTER*(*), INTENT ( IN ) :: MSG
             WRITE(6,*)TRIM( MSG )
             FLUSH( 6 )
             RETURN
          END SUBROUTINE M3MSG2
          SUBROUTINE M3MESG ( MSG )
            IMPLICIT NONE
              CHARACTER*(*), INTENT( IN ) :: MSG
              PRINT *, TRIM( MSG )
              RETURN
          END SUBROUTINE M3MESG
         INTEGER FUNCTION INIT3 ( )
           IMPLICIT NONE
             INIT3 = 6
             RETURN
         END FUNCTION INIT3
         INTEGER  FUNCTION  TRIMLEN ( STRING )
C***********************************************************************
C  FUNCTION:  return the effective length of argument CHARACTER*(*) STRING,
C             after trailing blanks have been trimmed.
C
C  PRECONDITIONS REQUIRED:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C***********************************************************************
        IMPLICIT NONE
C...........   ARGUMENTS and their descriptions:
          CHARACTER*(*), INTENT ( IN ) :: STRING
C   begin body of function  TRIMLEN
          TRIMLEN = LEN_TRIM( STRING )
          RETURN
         END FUNCTION TRIMLEN
        integer function setenvvar ( env_name, env_value )

        !------------------------------------------------------------------------------!
        ! description:                                                                 !
        !                                                                              !
        ! wrapper function around c function setenvvarc to set a shell             !
        ! environmental variable from within program                                   !
        !                                                                              !
        ! inputs: env_name  : shell environmental variable                             !
        !         env_value : value that is env_name is set to                         !
        !                                                                              !
        ! example:                                                                     !
        !          you want to setenv foo bar                                          !
        !          iout = mio_setenvvar ( "foo", "bar" )                                !
        !                                                                              !
        ! external functions called:                                                   !
        !          c function mio_setenvvarc                                           !
        !                                                                              !
        ! revision history:                                                            !
        !     2024: prototype adapted from d.wong/CJC setenvvar f.sidi usepa           !
        !                                                                              !
        !------------------------------------------------------------------------------!
            implicit none

            ! function arguments
            character(*), intent(in) :: env_name    ! logical env. name to be set
            character(*), intent(in) :: env_value   ! value that env_name is set to


            ! scratch variables
            integer :: env_name_len                     ! length of env_name string
            integer :: env_value_len                    ! length of env_value string
            character(len=len(env_value)) :: test_value ! used to get set variable
            ! exteranal functions

            integer, external :: setenvvarc

            ! find length of strings

            env_name_len  = len_trim ( env_name  )
            env_value_len = len_trim ( env_value )

            ! check to make sure no blank string is passed & call c function
            ! mio_setenvvarc
            if ( ( env_name_len .eq. 0 ) .or. (env_value_len .eq. 0 ) ) then
              setenvvar = -1
              return
            else
              setenvvar = setenvvarc ( env_name, env_name_len,
     &                                        env_value, env_value_len )
            endif
            if( setenvvar .le. 0 )then
              print*,'Error: setenvvar fails to set ',trim(env_name)
              stop
            else
              call nameval(env_name,test_value)
              print*,'Success: setenvvar sets ',trim(env_name),' = ',
     &        trim(test_value)
            end if

        end function setenvvar

        LOGICAL FUNCTION XTRACT3_2D( FNAME, VNAME,
     &                            LAY0, LAY1, ROW0, ROW1, COL0, COL1,
     &                            JDATE, JTIME, BUFFER )

C***********************************************************************
C
C  FUNCTION:  Mimics IO/API function DESC3 by putting selected
C             file description data in commons.
C
C  RETURN VALUE:  TRUE iff successful
C
C  PRECONDITIONS REQUIRED: None
C
C  SUBROUTINES AND FUNCTIONS CALLED: None
C
C  REVISION  HISTORY: Prototype created by Golam Sarwar - March, 2004
C  REVISION  HISTORY: This file was Created from XTRACT3
C  REVISION  HISTORY: XTRACT3 was created Prototype created by Jerry Gipson IN JULY 1997	
C
C***********************************************************************
      USE DRIVER_INPUTS
      USE SCENE_DATA

      IMPLICIT NONE

C...........   INCLUDES:

C...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(IN   ) :: FNAME           !  logical file name
        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME           !  variable name, or 'ALL'
        INTEGER,          INTENT(IN   ) :: LAY0            !  lower layer bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: LAY1            !  upper layer bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: ROW0            !  lower row   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: ROW1            !  upper row   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: COL0            !  lower col   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: COL1            !  upper col   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
        INTEGER,          INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
        REAL,             INTENT(  OUT) :: BUFFER( :,: )   !  interpolation-output buffer array

        INTEGER  L, M

        CHARACTER(LEN=LEN(VNAME)) :: VARIABLE
C.............................................................................
C   begin body of subroutine  OPEN3

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  MET_CRO_2D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   
            L = LEN_TRIM(VNAME)
            M = LEN(VNAME)

            VARIABLE( 1:L )   = VNAME( 1:L )
            VARIABLE( L+1:M ) = ' '
              
       
            IF ( INDEX(TRIM(VNAME),'PRES') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = CELL_PRES
                  XTRACT3_2D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
         
            IF ( INDEX(TRIM(VNAME),'WBAR') .GT. 0 ) THEN
               BUFFER  = BXM_WBAR
               XTRACT3_2D = .TRUE.
               RETURN
            ENDIF
                                
            IF ( INDEX(TRIM(VNAME),'SEAICE') .GT. 0 ) THEN
                 BUFFER  = BXM_SEAICE
                 XTRACT3_2D = .TRUE.
                 RETURN
            ENDIF

            IF ( INDEX(TRIM(VNAME),'CLDT') .GT. 0 ) THEN
               BUFFER  = BXM_CLDT
               XTRACT3_2D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'CLDB') .GT. 0 ) THEN
               BUFFER  = BXM_CLDB
               XTRACT3_2D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'CFRAC') .GT. 0 ) THEN
               BUFFER  = BXM_CFRAC
               XTRACT3_2D = .TRUE.
               RETURN
            ENDIF

            IF ( INDEX(TRIM(VNAME),'SLTYP') .GT. 0 ) THEN
               BUFFER  = 5.0 ! set soil type to loam
               XTRACT3_2D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'TEMPG') .GT. 0 ) THEN
                  BUFFER  = CELL_TEMP
                  XTRACT3_2D = .TRUE.
                  RETURN
            ENDIF
   
   
            IF ( INDEX(TRIM(VNAME),'RCA') .GT. 0 ) THEN
               BUFFER  = -1.0
               XTRACT3_2D = .TRUE.
               RETURN
            ENDIF
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  MET_CRO_3D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   
            IF ( INDEX(TRIM(VNAME),'TA') .GT. 0 ) THEN
                  BUFFER  = CELL_TEMP
                  XTRACT3_2D = .TRUE.
                  RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'PRES') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = CELL_PRES
                  XTRACT3_2D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'QV') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = QV
                  XTRACT3_2D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'DENS') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = DENS
                  XTRACT3_2D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'DENSA_J') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = DENS_J
                  XTRACT3_2D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'ZH') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = 50.0
                  XTRACT3_2D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  OCEAN_1 variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            IF ( INDEX(TRIM(VNAME),'OPEN') .GT. 0 ) THEN
               BUFFER  = BXM_OPEN
               XTRACT3_2D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'SURF') .GT. 0 ) THEN
               BUFFER  = BXM_SURF
               XTRACT3_2D = .TRUE.
               RETURN
            ENDIF


            IF ( INDEX(TRIM(VNAME),'CHLO') .GT. 0 ) THEN
               BUFFER  = 0.0 
               XTRACT3_2D = .TRUE.
               RETURN
            ENDIF



            IF ( INDEX(TRIM(VNAME),'DMS') .GT. 0 ) THEN
               BUFFER  = 0.0
               XTRACT3_2D = .TRUE.
               RETURN
            ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  GRID_CRO_2D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            IF ( INDEX(TRIM(VNAME),'LAT') .GT. 0 ) THEN
               BUFFER  = BXM_LAT
               XTRACT3_2D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'LON') .GT. 0 ) THEN
               BUFFER  = BXM_LON
               XTRACT3_2D = .TRUE.
               RETURN
            ENDIF
                                 
            IF ( INDEX(TRIM(VNAME),'OPEN') .GT. 0 ) THEN
               BUFFER  = BXM_OPEN
               XTRACT3_2D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'SURF') .GT. 0 ) THEN
               BUFFER  = BXM_SURF
               XTRACT3_2D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'MSFX2') .GT. 0 ) THEN
               BUFFER  = 1.0
               XTRACT3_2D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'HT') .GT. 0 ) THEN
               BUFFER  = BXM_HT
               XTRACT3_2D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'LUFRAC_') .GT. 0 ) THEN
              IF ( INDEX(TRIM(VNAME),'LUFRAC_04') .GT. 0 ) THEN
                 BUFFER = 1.0
              ELSE
                 BUFFER = 0.0
              END IF
              XTRACT3_2D = .TRUE.
              RETURN
            ENDIF

            BUFFER  = 1.0E-30
            PRINT*,"XTRACT3_2D: Unknown file and Variable, ",TRIM(FNAME)," and ",TRIM(VNAME)
            PRINT*,"Setting ", TRIM(VNAME),' to 1.0E-30'
            XTRACT3_2D = .TRUE.
      
      RETURN

      END FUNCTION XTRACT3_2D
        LOGICAL FUNCTION XTRACT3_2DI( FNAME, VNAME,
     &                            LAY0, LAY1, ROW0, ROW1, COL0, COL1,
     &                            JDATE, JTIME, BUFFER )

C***********************************************************************
C
C  FUNCTION:  Mimics IO/API function DESC3 by putting selected
C             file description data in commons.
C
C  RETURN VALUE:  TRUE iff successful
C
C  PRECONDITIONS REQUIRED: None
C
C  SUBROUTINES AND FUNCTIONS CALLED: None
C
C  REVISION  HISTORY: Prototype created by Golam Sarwar - March, 2004
C  REVISION  HISTORY: This file was Created from XTRACT3
C  REVISION  HISTORY: XTRACT3 was created Prototype created by Jerry Gipson IN JULY 1997	
C
C***********************************************************************
      USE DRIVER_INPUTS
      USE SCENE_DATA

      IMPLICIT NONE

C...........   INCLUDES:

C...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(IN   ) :: FNAME           !  logical file name
        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME           !  variable name, or 'ALL'
        INTEGER,          INTENT(IN   ) :: LAY0            !  lower layer bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: LAY1            !  upper layer bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: ROW0            !  lower row   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: ROW1            !  upper row   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: COL0            !  lower col   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: COL1            !  upper col   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
        INTEGER,          INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
        INTEGER,          INTENT(  OUT) :: BUFFER( :,: )   !  interpolation-output buffer array

        INTEGER  L, M

        CHARACTER(LEN=LEN(VNAME)) :: VARIABLE
C.............................................................................
C   begin body of subroutine  OPEN3

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  MET_CRO_2D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   
            L = LEN_TRIM(VNAME)
            M = LEN(VNAME)

            VARIABLE( 1:L )   = VNAME( 1:L )
            VARIABLE( L+1:M ) = ' '
              
       
            IF ( INDEX(TRIM(VNAME),'PRES') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = CELL_PRES
                  XTRACT3_2DI = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
         
            IF ( INDEX(TRIM(VNAME),'WBAR') .GT. 0 ) THEN
               BUFFER  = BXM_WBAR
               XTRACT3_2DI = .TRUE.
               RETURN
            ENDIF
                                
            IF ( INDEX(TRIM(VNAME),'SEAICE') .GT. 0 ) THEN
                 BUFFER  = BXM_SEAICE
                 XTRACT3_2DI = .TRUE.
                 RETURN
            ENDIF

            IF ( INDEX(TRIM(VNAME),'CLDT') .GT. 0 ) THEN
               BUFFER  = BXM_CLDT
               XTRACT3_2DI = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'CLDB') .GT. 0 ) THEN
               BUFFER  = BXM_CLDB
               XTRACT3_2DI = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'CFRAC') .GT. 0 ) THEN
               BUFFER  = BXM_CFRAC
               XTRACT3_2DI = .TRUE.
               RETURN
            ENDIF

            IF ( INDEX(TRIM(VNAME),'SLTYP') .GT. 0 ) THEN
               BUFFER  = 5.0 ! set soil type to loam
               XTRACT3_2DI = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'TEMPG') .GT. 0 ) THEN
                  BUFFER  = CELL_TEMP
                  XTRACT3_2DI = .TRUE.
                  RETURN
            ENDIF
   
   
            IF ( INDEX(TRIM(VNAME),'RCA') .GT. 0 ) THEN
               BUFFER  = -1.0
               XTRACT3_2DI = .TRUE.
               RETURN
            ENDIF
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  MET_CRO_3D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   
            IF ( INDEX(TRIM(VNAME),'TA') .GT. 0 ) THEN
                  BUFFER  = CELL_TEMP
                  XTRACT3_2DI = .TRUE.
                  RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'PRES') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = CELL_PRES
                  XTRACT3_2DI = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'QV') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = QV
                  XTRACT3_2DI = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'DENS') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = DENS
                  XTRACT3_2DI = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'DENSA_J') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = DENS_J
                  XTRACT3_2DI = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'ZH') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = 50.0
                  XTRACT3_2DI = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  OCEAN_1 variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            IF ( INDEX(TRIM(VNAME),'OPEN') .GT. 0 ) THEN
               BUFFER  = BXM_OPEN
               XTRACT3_2DI = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'SURF') .GT. 0 ) THEN
               BUFFER  = BXM_SURF
               XTRACT3_2DI = .TRUE.
               RETURN
            ENDIF


            IF ( INDEX(TRIM(VNAME),'CHLO') .GT. 0 ) THEN
               BUFFER  = 0.0 
               XTRACT3_2DI = .TRUE.
               RETURN
            ENDIF



            IF ( INDEX(TRIM(VNAME),'DMS') .GT. 0 ) THEN
               BUFFER  = 0.0
               XTRACT3_2DI = .TRUE.
               RETURN
            ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  GRID_CRO_2D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            IF ( INDEX(TRIM(VNAME),'LAT') .GT. 0 ) THEN
               BUFFER  = BXM_LAT
               XTRACT3_2DI = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'LON') .GT. 0 ) THEN
               BUFFER  = BXM_LON
               XTRACT3_2DI = .TRUE.
               RETURN
            ENDIF
                                 
            IF ( INDEX(TRIM(VNAME),'OPEN') .GT. 0 ) THEN
               BUFFER  = BXM_OPEN
               XTRACT3_2DI = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'SURF') .GT. 0 ) THEN
               BUFFER  = BXM_SURF
               XTRACT3_2DI = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'MSFX2') .GT. 0 ) THEN
               BUFFER  = 1.0
               XTRACT3_2DI = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'HT') .GT. 0 ) THEN
               BUFFER  = BXM_HT
               XTRACT3_2DI = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'LUFRAC_') .GT. 0 ) THEN
              IF ( INDEX(TRIM(VNAME),'LUFRAC_04') .GT. 0 ) THEN
                 BUFFER = 1.0
              ELSE
                 BUFFER = 0.0
              END IF
              XTRACT3_2DI = .TRUE.
              RETURN
            ENDIF

            BUFFER  = 1.0E-30
            PRINT*,"XTRACT3_2DI: Unknown file and Variable, ",TRIM(FNAME)," and ",TRIM(VNAME)
            PRINT*,"Setting ", TRIM(VNAME),' to 1.0E-30'
            XTRACT3_2DI = .TRUE.
      
      RETURN

      END FUNCTION XTRACT3_2DI
        LOGICAL FUNCTION XTRACT3_3D( FNAME, VNAME,
     &                            LAY0, LAY1, ROW0, ROW1, COL0, COL1,
     &                            JDATE, JTIME, BUFFER )

C***********************************************************************
C
C  FUNCTION:  Mimics IO/API function XTRACT3 by putting selected
C             file description data in commons.
C
C  RETURN VALUE:  TRUE if successful
C
C  PRECONDITIONS REQUIRED: None
C
C  SUBROUTINES AND FUNCTIONS CALLED: None
C
C  REVISION  HISTORY: Prototype created by Golam Sarwar - March, 2004
C  REVISION  HISTORY: This file was Created from XTRACT3
C  REVISION  HISTORY: XTRACT3 was created Prototype created by Jerry Gipson IN JULY 1997	
C
C***********************************************************************
      USE DRIVER_INPUTS
      USE SCENE_DATA

      IMPLICIT NONE

C...........   INCLUDES:

C...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(IN   ) :: FNAME           !  logical file name
        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME           !  variable name, or 'ALL'
        INTEGER,          INTENT(IN   ) :: LAY0            !  lower layer bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: LAY1            !  upper layer bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: ROW0            !  lower row   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: ROW1            !  upper row   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: COL0            !  lower col   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: COL1            !  upper col   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
        INTEGER,          INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
        REAL,             INTENT(  OUT) :: BUFFER( :,:,: )   !  interpolation-output buffer array

        INTEGER  L
C.............................................................................
C   begin body of subroutine  OPEN3

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  MET_CRO_3D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   
            IF ( INDEX(TRIM(VNAME),'PRES') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = CELL_PRES
                  XTRACT3_3D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
         
            IF ( INDEX(TRIM(VNAME),'WBAR') .GT. 0 ) THEN
               BUFFER  = BXM_WBAR
               XTRACT3_3D = .TRUE.
               RETURN
            ENDIF
                                
            IF ( INDEX(TRIM(VNAME),'SEAICE') .GT. 0 ) THEN
                 BUFFER  = BXM_SEAICE
                 XTRACT3_3D = .TRUE.
                 RETURN
            ENDIF

            IF ( INDEX(TRIM(VNAME),'CLDT') .GT. 0 ) THEN
               BUFFER  = BXM_CLDT
               XTRACT3_3D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'CLDB') .GT. 0 ) THEN
               BUFFER  = BXM_CLDB
               XTRACT3_3D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'CFRAC') .GT. 0 ) THEN
               BUFFER  = BXM_CFRAC
               XTRACT3_3D = .TRUE.
               RETURN
            ENDIF

            IF ( INDEX(TRIM(VNAME),'SLTYP') .GT. 0 ) THEN
               BUFFER  = 5.0 ! set soil type to loam
               XTRACT3_3D = .TRUE.
               RETURN
            ENDIF
   
   
            IF ( INDEX(TRIM(VNAME),'RCA') .GT. 0 ) THEN
               BUFFER  = -1.0
               XTRACT3_3D = .TRUE.
               RETURN
            ENDIF
   
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  MET_CRO_3D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   
            IF ( INDEX(TRIM(VNAME),'TA') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = CELL_TEMP
                  XTRACT3_3D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'PRES') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = CELL_PRES
                  XTRACT3_3D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'QV') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = QV
                  XTRACT3_3D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'DENS') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = DENS
                  XTRACT3_3D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'DENSA_J') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = DENS_J
                  XTRACT3_3D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'ZH') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = 50.0
                  XTRACT3_3D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  OCEAN_1 variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            IF ( INDEX(TRIM(VNAME),'OPEN') .GT. 0 ) THEN
               BUFFER  = BXM_OPEN
               XTRACT3_3D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'SURF') .GT. 0 ) THEN
               BUFFER  = BXM_SURF
               XTRACT3_3D = .TRUE.
               RETURN
            ENDIF


            IF ( INDEX(TRIM(VNAME),'CHLO') .GT. 0 ) THEN
               BUFFER  = 0.0 
               XTRACT3_3D = .TRUE.
               RETURN
            ENDIF



            IF ( INDEX(TRIM(VNAME),'DMS') .GT. 0 ) THEN
               BUFFER  = 0.0
               XTRACT3_3D = .TRUE.
               RETURN
            ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  GRID_CRO_3D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            IF ( INDEX(TRIM(VNAME),'LAT') .GT. 0 ) THEN
               BUFFER  = BXM_LAT
               XTRACT3_3D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'LON') .GT. 0 ) THEN
               BUFFER  = BXM_LON
               XTRACT3_3D = .TRUE.
               RETURN
            ENDIF
                                 
            IF ( INDEX(TRIM(VNAME),'OPEN') .GT. 0 ) THEN
               BUFFER  = BXM_OPEN
               XTRACT3_3D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'SURF') .GT. 0 ) THEN
               BUFFER  = BXM_SURF
               XTRACT3_3D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'MSFX2') .GT. 0 ) THEN
               BUFFER  = 1.0
               XTRACT3_3D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'HT') .GT. 0 ) THEN
               BUFFER  = BXM_HT
               XTRACT3_3D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'LUFRAC_') .GT. 0 ) THEN
              IF ( INDEX(TRIM(VNAME),'LUFRAC_04') .GT. 0 ) THEN
                 BUFFER = 1.0
              ELSE
                 BUFFER = 0.0
              END IF
              XTRACT3_3D = .TRUE.
              RETURN
            END IF 

            BUFFER  = 1.0E-30
            PRINT*,"XTRACT3_3D: Unknown file and Variable, ",TRIM(FNAME)," and ",TRIM(VNAME)
            PRINT*,"Setting ", TRIM(VNAME),' to 1.0E-30'
            XTRACT3_3D = .TRUE.
         
      
      RETURN

      END FUNCTION XTRACT3_3D
        LOGICAL FUNCTION XTRACT3_1D( FNAME, VNAME,
     &                            LAY0, LAY1, ROW0, ROW1, COL0, COL1,
     &                            JDATE, JTIME, BUFFER )

C***********************************************************************
C
C  FUNCTION:  Mimics IO/API function XTRACT3 by putting selected
C             file description data in commons.
C
C  RETURN VALUE:  TRUE if successful
C
C  PRECONDITIONS REQUIRED: None
C
C  SUBROUTINES AND FUNCTIONS CALLED: None
C
C  REVISION  HISTORY: Prototype created by Golam Sarwar - March, 2004
C  REVISION  HISTORY: This file was Created from XTRACT3
C  REVISION  HISTORY: XTRACT3 was created Prototype created by Jerry Gipson IN JULY 1997	
C
C***********************************************************************
      USE DRIVER_INPUTS
      USE SCENE_DATA

      IMPLICIT NONE

C...........   INCLUDES:

C...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(IN   ) :: FNAME           !  logical file name
        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME           !  variable name, or 'ALL'
        INTEGER,          INTENT(IN   ) :: LAY0            !  lower layer bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: LAY1            !  upper layer bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: ROW0            !  lower row   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: ROW1            !  upper row   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: COL0            !  lower col   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: COL1            !  upper col   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
        INTEGER,          INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
        REAL,             INTENT(  OUT) :: BUFFER( : )   !  interpolation-output buffer array

        INTEGER  L, M

        CHARACTER(LEN=LEN(VNAME)) :: VARIABLE

            L = LEN_TRIM(VNAME)
            M = LEN(VNAME)

            VARIABLE( 1:L )   = VNAME( 1:L )
            VARIABLE( L+1:M ) = ' '


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  MET_CRO_2D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   
            IF ( INDEX(VNAME,'PRSFC') ) THEN
                  BUFFER = BXM_PRES
                  XTRACT3_1D = .TRUE.
                  RETURN
            ENDIF

            IF ( INDEX(TRIM(VNAME),'TEMPG') .GT. 0 ) THEN
                  BUFFER  = CELL_TEMP
                  XTRACT3_1D = .TRUE.
                  RETURN
            ENDIF

            IF ( INDEX(TRIM(VNAME),'TSEASFC') .GT. 0 ) THEN
                  BUFFER  = CELL_TEMP
                  XTRACT3_1D = .TRUE.
                  RETURN
            ENDIF

            IF ( INDEX(TRIM(VNAME),'TEMP2') .GT. 0 ) THEN
                  BUFFER  = CELL_TEMP
                  XTRACT3_1D = .TRUE.
                  RETURN
            ENDIF

           IF ( INDEX(TRIM( VARIABLE ),'PRES') .GT. 0  ) THEN
               BUFFER  = CELL_PRES
               XTRACT3_1D = .TRUE.
               RETURN
            ENDIF
         
            IF ( INDEX(TRIM( VARIABLE ),'WBAR' ) .GT. 0 ) THEN
               BUFFER  = BXM_WBAR
               XTRACT3_1D = .TRUE.
               RETURN
            ENDIF
                                
            IF ( INDEX(TRIM( VARIABLE ),'SEAICE' ) .GT. 0 ) THEN
                 BUFFER  = BXM_SEAICE
                 XTRACT3_1D = .TRUE.
                 RETURN
            ENDIF

            IF ( INDEX(TRIM( VARIABLE ),'CLDT' ) .GT. 0 ) THEN
               BUFFER  = BXM_CLDT
               XTRACT3_1D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM( VARIABLE ),'CLDB' ) .GT. 0 ) THEN
               BUFFER  = BXM_CLDB
               XTRACT3_1D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM( VARIABLE ),'CFRAC' ) .GT. 0 ) THEN
               BUFFER  = BXM_CFRAC
               XTRACT3_1D = .TRUE.
               RETURN
            ENDIF

            IF ( INDEX(TRIM( VARIABLE ),'SLTYP' ) .GT. 0 ) THEN
               BUFFER  = 5.0 ! set soil type to loam
               XTRACT3_1D = .TRUE.
               RETURN
            ENDIF
   
   
            IF ( INDEX(TRIM(VNAME),'RCA') .GT. 0 ) THEN
               BUFFER  = -1.0
               XTRACT3_1D = .TRUE.
               RETURN
            ENDIF
   
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  MET_CRO_3D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   
            IF ( INDEX(TRIM( VARIABLE ),'TA' ) .GT. 0 ) THEN
                  BUFFER  = CELL_TEMP
                  XTRACT3_1D = .TRUE.
                  RETURN
            ENDIF
   
            IF ( INDEX(TRIM( VARIABLE ),'PRES' ) .GT. 0 ) THEN
                  BUFFER  = CELL_PRES
                  XTRACT3_1D = .TRUE.
                  RETURN
            ENDIF
   
            IF ( INDEX(TRIM( VARIABLE ),'QV' ) .GT. 0 ) THEN
                  BUFFER  = QV
                  XTRACT3_1D = .TRUE.
                  RETURN
            ENDIF
   
            IF ( INDEX(TRIM( VARIABLE ),'DENS' ) .GT. 0 ) THEN
                  BUFFER  = DENS
                  XTRACT3_1D = .TRUE.
                  RETURN
            ENDIF
   
            IF ( INDEX(TRIM( VARIABLE ),'DENSA_J' ) .GT. 0 ) THEN
                  BUFFER  = DENS_J
                  XTRACT3_1D = .TRUE.
                  RETURN
            ENDIF
   
            IF ( INDEX(TRIM( VARIABLE ),'ZH') .GT. 0  ) THEN
                  BUFFER  = 50.0
                  XTRACT3_1D = .TRUE.
                  RETURN
            ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  OCEAN_1 variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            IF ( INDEX(TRIM( VARIABLE ),'OPEN') .GT. 0  ) THEN
               BUFFER  = BXM_OPEN
               XTRACT3_1D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM( VARIABLE ),'SURF') .GT. 0  ) THEN
               BUFFER  = BXM_SURF
               XTRACT3_1D = .TRUE.
               RETURN
            ENDIF


            IF ( INDEX(TRIM( VARIABLE ),'CHLO') .GT. 0  ) THEN
               BUFFER  = 0.0 
               XTRACT3_1D = .TRUE.
               RETURN
            ENDIF



            IF ( INDEX(TRIM( VARIABLE ),'DMS') .GT. 0  ) THEN
               BUFFER  = 0.0
               XTRACT3_1D = .TRUE.
               RETURN
            ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  GRID_CRO_3D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            IF ( INDEX(TRIM( VARIABLE ),'LAT') .GT. 0  ) THEN
               BUFFER  = BXM_LAT
               XTRACT3_1D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM( VARIABLE ),'LON') .GT. 0  ) THEN
               BUFFER  = BXM_LON
               XTRACT3_1D = .TRUE.
               RETURN
            ENDIF
                                 
            IF ( INDEX(TRIM( VARIABLE ),'OPEN') .GT. 0 ) THEN
               BUFFER  = BXM_OPEN
               XTRACT3_1D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM( VARIABLE ),'SURF') .GT. 0 ) THEN
               BUFFER  = BXM_SURF
               XTRACT3_1D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM( VARIABLE ),'MSFX2') .GT. 0 ) THEN
               BUFFER  = 1.0
               XTRACT3_1D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM( VARIABLE ),'HT') .GT. 0 ) THEN
               BUFFER  = BXM_HT
               XTRACT3_1D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VARIABLE),'LUFRAC_') .GT. 0 ) THEN
              IF ( INDEX(TRIM(VARIABLE),'LUFRAC_04') .GT. 0 ) THEN
                 BUFFER = 1.0
              ELSE
                 BUFFER = 0.0
              END IF
              XTRACT3_1D = .TRUE.
              RETURN
            END IF

            BUFFER  = 1.0E-30
            PRINT*,"XTRACT3_1D: Unknown file and Variable, ",TRIM(FNAME)," and ",TRIM(VARIABLE)
            PRINT*,"Setting ", TRIM(VARIABLE),' to 1.0E-30'
            XTRACT3_1D = .TRUE.
         
      
      RETURN

      END FUNCTION XTRACT3_1D

        LOGICAL FUNCTION XTRACT3_0D( FNAME, VNAME,
     &                            LAY0, LAY1, ROW0, ROW1, COL0, COL1,
     &                            JDATE, JTIME, BUFFER )

C***********************************************************************
C
C  FUNCTION:  Mimics IO/API function XTRACT3 by putting selected
C             file description data in commons.
C
C  RETURN VALUE:  TRUE if successful
C
C  PRECONDITIONS REQUIRED: None
C
C  SUBROUTINES AND FUNCTIONS CALLED: None
C
C  REVISION  HISTORY: Prototype created by Golam Sarwar - March, 2004
C  REVISION  HISTORY: This file was Created from XTRACT3
C  REVISION  HISTORY: XTRACT3 was created Prototype created by Jerry Gipson IN JULY 1997	
C
C***********************************************************************
      USE DRIVER_INPUTS
      USE SCENE_DATA

      IMPLICIT NONE

C...........   INCLUDES:

C...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(IN   ) :: FNAME           !  logical file name
        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME           !  variable name, or 'ALL'
        INTEGER,          INTENT(IN   ) :: LAY0            !  lower layer bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: LAY1            !  upper layer bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: ROW0            !  lower row   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: ROW1            !  upper row   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: COL0            !  lower col   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: COL1            !  upper col   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
        INTEGER,          INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
        REAL,             INTENT(  OUT) :: BUFFER          !  interpolation-output buffer 

        INTEGER  L
C.............................................................................
C   begin body of subroutine  OPEN3

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  MET_CRO_3D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   
            IF ( INDEX(TRIM(VNAME),'PRES') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = CELL_PRES
                  XTRACT3_0D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
         
            IF ( INDEX(TRIM(VNAME),'WBAR') .GT. 0 ) THEN
               BUFFER  = BXM_WBAR
               XTRACT3_0D = .TRUE.
               RETURN
            ENDIF
                                
            IF ( INDEX(TRIM(VNAME),'SEAICE') .GT. 0 ) THEN
                 BUFFER  = BXM_SEAICE
                 XTRACT3_0D = .TRUE.
                 RETURN
            ENDIF

            IF ( INDEX(TRIM(VNAME),'CLDT') .GT. 0 ) THEN
               BUFFER  = BXM_CLDT
               XTRACT3_0D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'CLDB') .GT. 0 ) THEN
               BUFFER  = BXM_CLDB
               XTRACT3_0D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'CFRAC') .GT. 0 ) THEN
               BUFFER  = BXM_CFRAC
               XTRACT3_0D = .TRUE.
               RETURN
            ENDIF

            IF ( INDEX(TRIM(VNAME),'SLTYP') .GT. 0 ) THEN
               BUFFER  = 5.0 ! set soil type to loam
               XTRACT3_0D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'RCA') .GT. 0 ) THEN
               BUFFER  = -1.0
               XTRACT3_0D = .TRUE.
               RETURN
            ENDIF
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  MET_CRO_3D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   
            IF ( INDEX(TRIM(VNAME),'TA') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = CELL_TEMP
                  XTRACT3_0D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'PRES') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = CELL_PRES
                  XTRACT3_0D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'QV') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = QV
                  XTRACT3_0D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'DENS') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = DENS
                  XTRACT3_0D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'DENSA_J') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = DENS_J
                  XTRACT3_0D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'ZH') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = 50.0
                  XTRACT3_0D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  OCEAN_1 variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            IF ( INDEX(TRIM(VNAME),'OPEN') .GT. 0 ) THEN
               BUFFER  = BXM_OPEN
               XTRACT3_0D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'SURF') .GT. 0 ) THEN
               BUFFER  = BXM_SURF
               XTRACT3_0D = .TRUE.
               RETURN
            ENDIF


            IF ( INDEX(TRIM(VNAME),'CHLO') .GT. 0 ) THEN
               BUFFER  = 0.0 
               XTRACT3_0D = .TRUE.
               RETURN
            ENDIF



            IF ( INDEX(TRIM(VNAME),'DMS') .GT. 0 ) THEN
               BUFFER  = 0.0
               XTRACT3_0D = .TRUE.
               RETURN
            ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  GRID_CRO_3D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            IF ( INDEX(TRIM(VNAME),'LAT') .GT. 0 ) THEN
               BUFFER  = BXM_LAT
               XTRACT3_0D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'LON') .GT. 0 ) THEN
               BUFFER  = BXM_LON
               XTRACT3_0D = .TRUE.
               RETURN
            ENDIF
                                 
            IF ( INDEX(TRIM(VNAME),'OPEN') .GT. 0 ) THEN
               BUFFER  = BXM_OPEN
               XTRACT3_0D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'SURF') .GT. 0 ) THEN
               BUFFER  = BXM_SURF
               XTRACT3_0D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'MSFX2') .GT. 0 ) THEN
               BUFFER  = 1.0
               XTRACT3_0D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'HT') .GT. 0 ) THEN
               BUFFER  = BXM_HT
               XTRACT3_0D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'LUFRAC_') .GT. 0 ) THEN
              IF ( INDEX(TRIM(VNAME),'LUFRAC_04') .GT. 0 ) THEN
                 BUFFER = 1.0
              ELSE
                 BUFFER = 0.0
              END IF
              XTRACT3_0D = .TRUE.
              RETURN
            ENDIF

            BUFFER  = 1.0E-30
            PRINT*,"XTRACT3_0D: Unknown file and Variable, ",TRIM(FNAME)," and ",TRIM(VNAME)
            PRINT*,"Setting ", TRIM(VNAME),' to 1.0E-30'
            XTRACT3_0D = .TRUE.
         
      
      RETURN

      END FUNCTION XTRACT3_0D

        LOGICAL FUNCTION XTRACT3_4D( FNAME, VNAME,
     &                            LAY0, LAY1, ROW0, ROW1, COL0, COL1,
     &                            JDATE, JTIME, BUFFER )

C***********************************************************************
C
C  FUNCTION:  Mimics IO/API function XTRACT3 by putting selected
C             file description data in commons.
C
C  RETURN VALUE:  TRUE if successful
C
C  PRECONDITIONS REQUIRED: None
C
C  SUBROUTINES AND FUNCTIONS CALLED: None
C
C  REVISION  HISTORY: Prototype created by Golam Sarwar - March, 2004
C  REVISION  HISTORY: This file was Created from XTRACT3
C  REVISION  HISTORY: XTRACT3 was created Prototype created by Jerry Gipson IN JULY 1997	
C
C***********************************************************************
      USE DRIVER_INPUTS
      USE SCENE_DATA

      IMPLICIT NONE

C...........   INCLUDES:

C...........   ARGUMENTS and their descriptions:

        CHARACTER(LEN=*), INTENT(IN   ) :: FNAME             !  logical file name
        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME             !  variable name, or 'ALL'
        INTEGER,          INTENT(IN   ) :: LAY0              !  lower layer bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: LAY1              !  upper layer bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: ROW0              !  lower row   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: ROW1              !  upper row   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: COL0              !  lower col   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: COL1              !  upper col   bound for XTRACT3
        INTEGER,          INTENT(IN   ) :: JDATE             !  date, formatted YYYYDDD
        INTEGER,          INTENT(IN   ) :: JTIME             !  time, formatted HHMMSS
        REAL,             INTENT(  OUT) :: BUFFER( :,:,:,: ) !  interpolation-output buffer 

        INTEGER  L
C.............................................................................
C   begin body of subroutine  OPEN3

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  MET_CRO_2D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   
            IF ( INDEX(TRIM(VNAME),'PRES') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = CELL_PRES
                  XTRACT3_4D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
         
            IF ( INDEX(TRIM(VNAME),'WBAR') .GT. 0 ) THEN
               BUFFER  = BXM_WBAR
               XTRACT3_4D = .TRUE.
               RETURN
            ENDIF
                                
            IF ( INDEX(TRIM(VNAME),'SEAICE') .GT. 0 ) THEN
                 BUFFER  = BXM_SEAICE
                 XTRACT3_4D = .TRUE.
                 RETURN
            ENDIF

            IF ( INDEX(TRIM(VNAME),'CLDT') .GT. 0 ) THEN
               BUFFER  = BXM_CLDT
               XTRACT3_4D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'CLDB') .GT. 0 ) THEN
               BUFFER  = BXM_CLDB
               XTRACT3_4D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'CFRAC') .GT. 0 ) THEN
               BUFFER  = BXM_CFRAC
               XTRACT3_4D = .TRUE.
               RETURN
            ENDIF

            IF ( INDEX(TRIM(VNAME),'RCA') .GT. 0 ) THEN
               BUFFER  = -1.0
               XTRACT3_4D = .TRUE.
               RETURN
            ENDIF

            IF ( INDEX(TRIM(VNAME),'SLTYP') .GT. 0 ) THEN
               BUFFER  = 5.0 ! set soil type to loam
               XTRACT3_4D = .TRUE.
               RETURN
            ENDIF
   
   
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  MET_CRO_3D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   
            IF ( INDEX(TRIM(VNAME),'TA') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = CELL_TEMP
                  XTRACT3_4D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'PRES') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = CELL_PRES
                  XTRACT3_4D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'QV') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = QV
                  XTRACT3_4D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'DENS') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = DENS
                  XTRACT3_4D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'DENSA_J') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = DENS_J
                  XTRACT3_4D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'ZH') .GT. 0 ) THEN
               IF ( LDEFAULT ) THEN
                  BUFFER  = 50.0
                  XTRACT3_4D = .TRUE.
                  RETURN
               ELSE
               ENDIF
            ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  OCEAN_1 variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            IF ( INDEX(TRIM(VNAME),'OPEN') .GT. 0 ) THEN
               BUFFER  = BXM_OPEN
               XTRACT3_4D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'SURF') .GT. 0 ) THEN
               BUFFER  = BXM_SURF
               XTRACT3_4D = .TRUE.
               RETURN
            ENDIF


            IF ( INDEX(TRIM(VNAME),'CHLO') .GT. 0 ) THEN
               BUFFER  = 0.0 
               XTRACT3_4D = .TRUE.
               RETURN
            ENDIF



            IF ( INDEX(TRIM(VNAME),'DMS') .GT. 0 ) THEN
               BUFFER  = 0.0
               XTRACT3_4D = .TRUE.
               RETURN
            ENDIF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  GRID_CRO_3D variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            IF ( INDEX(TRIM(VNAME),'LAT') .GT. 0 ) THEN
               BUFFER  = BXM_LAT
               XTRACT3_4D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'LON') .GT. 0 ) THEN
               BUFFER  = BXM_LON
               XTRACT3_4D = .TRUE.
               RETURN
            ENDIF
                                 
            IF ( INDEX(TRIM(VNAME),'OPEN') .GT. 0 ) THEN
               BUFFER  = BXM_OPEN
               XTRACT3_4D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'SURF') .GT. 0 ) THEN
               BUFFER  = BXM_SURF
               XTRACT3_4D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'MSFX2') .GT. 0 ) THEN
               BUFFER  = 1.0
               XTRACT3_4D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'HT') .GT. 0 ) THEN
               BUFFER  = BXM_HT
               XTRACT3_4D = .TRUE.
               RETURN
            ENDIF
   
            IF ( INDEX(TRIM(VNAME),'LUFRAC_') .GT. 0 ) THEN
              IF ( INDEX(TRIM(VNAME),'LUFRAC_04') .GT. 0 ) THEN
                 BUFFER = 1.0
              ELSE
                 BUFFER = 0.0
              END IF
              XTRACT3_4D = .TRUE.
              RETURN
            ENDIF

            BUFFER  = 1.0E-30
            PRINT*,"XTRACT3_4D: Unknown file and Variable, ",TRIM(FNAME)," and ",TRIM(VNAME)
            PRINT*,"Setting ", TRIM(VNAME),' to 1.0E-30'
            XTRACT3_4D = .TRUE.
         
      
      RETURN

      END FUNCTION XTRACT3_4D

        LOGICAL FUNCTION WRITE3R( FNAME, VNAME, JDATE, JTIME, BUFFER )

!          USE M3UTILIO, ONLY: OUTPUT_FILE,N_OUTPUT_FILES,OUTPUT_FILES,SETUP_OUTPUT_FILE

           IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:
           CHARACTER*(*), INTENT(IN   ) :: FNAME      !  logical file name
           CHARACTER*(*), INTENT(IN   ) :: VNAME      !  logical file name
           INTEGER      , INTENT(IN   ) :: JDATE      !  date, formatted YYYYDDD
           INTEGER      , INTENT(IN   ) :: JTIME      !  time, formatted HHMMSS
           REAL         , INTENT(IN   ) :: BUFFER(*)  !  output buffer array

           INTEGER :: FILE_ID
           INTEGER :: VAR_ID
           INTEGER :: NVAR
           INTEGER :: IO_UNIT
           INTEGER :: IOS
           INTEGER :: LDATE, LTIME
  
           LOGICAL :: WRITE_ALL
           LOGICAL :: NEW_MOMENT = .TRUE.

!          WRITE3 = .TRUE.
!          RETURN

           FILE_ID = 0
           DO NVAR = 1,N_OUTPUT_FILES
!              write(6,'(6(A,1X))')'WRITE3: FNAME,OUTPUT_FILES(NVAR)%FILENAME = ',
!     &        TRIM(FNAME),TRIM(OUTPUT_FILES(NVAR)%FILENAME)
              IF ( INDEX(TRIM(OUTPUT_FILES(NVAR)%FILENAME),TRIM(FNAME)) .GT. 0 ) THEN
                FILE_ID = NVAR
                EXIT
              END IF
           END DO
           IF( FILE_ID .LT. 1 )THEN
             WRITE(6,'(A)')'WRITE3 ERROR: ' // TRIM(FNAME) // ' is not opened.'
             WRITE3R = .FALSE.
             RETURN
           END IF

           LDATE = OUTPUT_FILES(FILE_ID)%JDATE
           LTIME = OUTPUT_FILES(FILE_ID)%JTIME
           
           IF ( LDATE .NE. JDATE .OR. LTIME .NE. JTIME 
     &          .OR.  LDATE .NE. JDATE .AND. LTIME .NE. JTIME ) THEN
              OUTPUT_FILES(FILE_ID)%JDATE  = JDATE
              OUTPUT_FILES(FILE_ID)%JTIME  = JTIME
              NEW_MOMENT = .TRUE.
           ELSE
              NEW_MOMENT = .FALSE.
           END IF
           IF ( OUTPUT_FILES(FILE_ID)%HEADER ) THEN
              NEW_MOMENT = .TRUE.
              OUTPUT_FILES(FILE_ID)%HEADER = .FALSE.
           END IF
           
           IO_UNIT = OUTPUT_FILES(FILE_ID)%IO_UNIT

           IF( TRIM(VNAME) .EQ. ALLVAR3 )THEN 
               WRITE_ALL = .TRUE.
               NVAR = OUTPUT_FILES(FILE_ID)%NVARS
               OUTPUT_FILES(FILE_ID)%FILLED = NVAR
               OUTPUT_FILES(FILE_ID)%VALUES(1:NVAR) = BUFFER(1:NVAR)
           ELSE
               VAR_ID = 0
               DO NVAR = 1,OUTPUT_FILES(FILE_ID)%NVARS
!                  IF ( INDEX(TRIM(OUTPUT_FILES(FILE_ID)%VARNAMES(NVAR)),TRIM(VNAME)) .GT. 0 ) THEN
                  IF ( OUTPUT_FILES(FILE_ID)%VARNAMES(NVAR) .EQ. VNAME ) THEN
                    VAR_ID = NVAR
                    EXIT
                  END IF
               END DO
               IF( VAR_ID .LT. 1 )THEN
                 WRITE(6,'(A)')'WRITE3 ERROR: ' // TRIM(FNAME) // ' does not have variable, '
     &           // TRIM( VNAME )
                 WRITE3R = .FALSE.
                 RETURN
               END IF
               OUTPUT_FILES(FILE_ID)%FILLED = OUTPUT_FILES(FILE_ID)%FILLED + 1
               IF( OUTPUT_FILES(FILE_ID)%FILLED .EQ. OUTPUT_FILES(FILE_ID)%NVARS )THEN
                   WRITE_ALL = .TRUE.
               ELSE
                   WRITE_ALL = .FALSE.
               END IF
               OUTPUT_FILES(FILE_ID)%VALUES(VAR_ID) = BUFFER(1)
           END IF
           IF ( NEW_MOMENT ) THEN
             OUTPUT_FILES(FILE_ID)%NSTEPS = OUTPUT_FILES(FILE_ID)%NSTEPS + 1
             OUTPUT_FILES(FILE_ID)%FLUSHED = .TRUE.
           ELSE
             BACKSPACE(UNIT = IO_UNIT, ERR = 1000, IOSTAT = IOS)
             OUTPUT_FILES(FILE_ID)%FLUSHED = .FALSE.
           END IF
           WRITE(IO_UNIT,'(2(I7,","),4000(18X,ES16.4,","))')JDATE,JTIME,
     &     (OUTPUT_FILES(FILE_ID)%VALUES(NVAR),NVAR=1,OUTPUT_FILES(FILE_ID)%NVARS)
           IF ( WRITE_ALL ) THEN
!             OUTPUT_FILES(FILE_ID)%VALUES = AMISS3
           END IF

           WRITE3R = .TRUE.
           RETURN

1000       WRITE(6,'(A)')'WRITE3R ERROR: Failed to rewind ' // TRIM(FNAME) 
     &     // ' one lines.'
           WRITE3R = .FALSE.

        END FUNCTION WRITE3R
        LOGICAL FUNCTION WRITE3I( FNAME, VNAME, JDATE, JTIME, BUFFER )

!          USE M3UTILIO, ONLY: OUTPUT_FILE,N_OUTPUT_FILES,OUTPUT_FILES,SETUP_OUTPUT_FILE

           IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:
           CHARACTER*(*), INTENT(IN   ) :: FNAME       !  logical file name
           CHARACTER*(*), INTENT(IN   ) :: VNAME       !  logical file name
           INTEGER      , INTENT(IN   ) :: JDATE       !  date, formatted YYYYDDD
           INTEGER      , INTENT(IN   ) :: JTIME       !  time, formatted HHMMSS
           INTEGER      , INTENT(IN   ) :: BUFFER(:,:) !  output buffer array

           INTEGER :: FILE_ID
           INTEGER :: VAR_ID
           INTEGER :: NVAR
           INTEGER :: IO_UNIT
           INTEGER :: IOS
           INTEGER :: LDATE, LTIME
  
           LOGICAL :: WRITE_ALL
           LOGICAL :: NEW_MOMENT = .TRUE.

!          WRITE3I = .TRUE.
!          RETURN

           FILE_ID = 0
           DO NVAR = 1,N_OUTPUT_FILES
!              write(6,'(6(A,1X))')'WRITE3: FNAME,OUTPUT_FILES(NVAR)%FILENAME = ',
!     &        TRIM(FNAME),TRIM(OUTPUT_FILES(NVAR)%FILENAME)
              IF ( INDEX(TRIM(OUTPUT_FILES(NVAR)%FILENAME),TRIM(FNAME)) .GT. 0 ) THEN
                FILE_ID = NVAR
                EXIT
              END IF
           END DO
           IF( FILE_ID .LT. 1 )THEN
             WRITE(6,'(A)')'WRITE3 ERROR: ' // TRIM(FNAME) // ' is not opened.'
             WRITE3I = .FALSE.
             RETURN
           END IF

           LDATE = OUTPUT_FILES(FILE_ID)%JDATE
           LTIME = OUTPUT_FILES(FILE_ID)%JTIME
           
           IF ( LDATE .NE. JDATE .OR. LTIME .NE. JTIME 
     &          .OR.  LDATE .NE. JDATE .AND. LTIME .NE. JTIME ) THEN
              OUTPUT_FILES(FILE_ID)%JDATE  = JDATE
              OUTPUT_FILES(FILE_ID)%JTIME  = JTIME
              NEW_MOMENT = .TRUE.
           ELSE
              NEW_MOMENT = .FALSE.
           END IF
           IF ( OUTPUT_FILES(FILE_ID)%HEADER ) THEN
              NEW_MOMENT = .TRUE.
              OUTPUT_FILES(FILE_ID)%HEADER = .FALSE.
           END IF
           
           IO_UNIT = OUTPUT_FILES(FILE_ID)%IO_UNIT

           IF( TRIM(VNAME) .EQ. ALLVAR3 )THEN 
               WRITE_ALL = .TRUE.
               NVAR = OUTPUT_FILES(FILE_ID)%NVARS
               OUTPUT_FILES(FILE_ID)%FILLED = NVAR
               OUTPUT_FILES(FILE_ID)%VALUES(1:NVAR) = BUFFER(1,1:NVAR)
           ELSE
               VAR_ID = 0
               DO NVAR = 1,OUTPUT_FILES(FILE_ID)%NVARS
!                  IF ( INDEX(TRIM(OUTPUT_FILES(FILE_ID)%VARNAMES(NVAR)),TRIM(VNAME)) .GT. 0 ) THEN
                  IF ( OUTPUT_FILES(FILE_ID)%VARNAMES(NVAR) .EQ. VNAME ) THEN
                    VAR_ID = NVAR
                    EXIT
                  END IF
               END DO
               IF( VAR_ID .LT. 1 )THEN
                 WRITE(6,'(A)')'WRITE3 ERROR: ' // TRIM(FNAME) // ' does not have variable, '
     &           // TRIM( VNAME )
                 WRITE3I = .FALSE.
                 RETURN
               END IF
               OUTPUT_FILES(FILE_ID)%FILLED = OUTPUT_FILES(FILE_ID)%FILLED + 1
               IF( OUTPUT_FILES(FILE_ID)%FILLED .EQ. OUTPUT_FILES(FILE_ID)%NVARS )THEN
                   WRITE_ALL = .TRUE.
               ELSE
                   WRITE_ALL = .FALSE.
               END IF
               OUTPUT_FILES(FILE_ID)%VALUES(VAR_ID) = BUFFER(1,1)
           END IF
           IF ( NEW_MOMENT ) THEN
             OUTPUT_FILES(FILE_ID)%NSTEPS = OUTPUT_FILES(FILE_ID)%NSTEPS + 1
             OUTPUT_FILES(FILE_ID)%FLUSHED = .TRUE.
           ELSE
             BACKSPACE(UNIT = IO_UNIT, ERR = 1000, IOSTAT = IOS)
             OUTPUT_FILES(FILE_ID)%FLUSHED = .FALSE.
           END IF
           WRITE(IO_UNIT,'(2(I7,","),4000(26X,I8,","))')JDATE,JTIME,
     &     (OUTPUT_FILES(FILE_ID)%VALUES(NVAR),NVAR=1,OUTPUT_FILES(FILE_ID)%NVARS)
           IF ( WRITE_ALL ) THEN
!             OUTPUT_FILES(FILE_ID)%VALUES = IMISS3
           END IF

           WRITE3I = .TRUE.
           RETURN

1000       WRITE(6,'(A)')'WRITE3I ERROR: Failed to rewind ' // TRIM(FNAME) 
     &     // ' one lines.'
           WRITE3I = .FALSE.

        END FUNCTION WRITE3I
        LOGICAL FUNCTION WRITE3R2D( FNAME, VNAME, JDATE, JTIME, BUFFER )

!          USE M3UTILIO, ONLY: OUTPUT_FILE,N_OUTPUT_FILES,OUTPUT_FILES,SETUP_OUTPUT_FILE

           IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:
           CHARACTER*(*), INTENT(IN   ) :: FNAME       !  logical file name
           CHARACTER*(*), INTENT(IN   ) :: VNAME       !  logical file name
           INTEGER      , INTENT(IN   ) :: JDATE       !  date, formatted YYYYDDD
           INTEGER      , INTENT(IN   ) :: JTIME       !  time, formatted HHMMSS
           REAL         , INTENT(IN   ) :: BUFFER(:,:) !  output buffer array

           INTEGER :: FILE_ID
           INTEGER :: VAR_ID
           INTEGER :: NVAR
           INTEGER :: IO_UNIT
           INTEGER :: IOS
           INTEGER :: LDATE, LTIME
           LOGICAL :: NEW_MOMENT = .TRUE.
  
           LOGICAL :: WRITE_ALL

!          WRITE3 = .TRUE.
!          RETURN

           FILE_ID = 0
           DO NVAR = 1,N_OUTPUT_FILES
!              write(6,'(6(A,1X))')'WRITE3: FNAME,OUTPUT_FILES(NVAR)%FILENAME = ',
!     &        TRIM(FNAME),TRIM(OUTPUT_FILES(NVAR)%FILENAME)
              IF ( INDEX(TRIM(OUTPUT_FILES(NVAR)%FILENAME),TRIM(FNAME)) .GT. 0 ) THEN
                FILE_ID = NVAR
                EXIT
              END IF
           END DO
           IF( FILE_ID .LT. 1 )THEN
             WRITE(6,'(A)')'WRITE3 ERROR: ' // TRIM(FNAME) // ' is not opened.'
             WRITE3R2D = .FALSE.
             RETURN
           END IF

           LDATE = OUTPUT_FILES(FILE_ID)%JDATE
           LTIME = OUTPUT_FILES(FILE_ID)%JTIME
           
           IF ( LDATE .NE. JDATE .OR. LTIME .NE. JTIME 
     &          .OR.  LDATE .NE. JDATE .AND. LTIME .NE. JTIME ) THEN
              OUTPUT_FILES(FILE_ID)%JDATE  = JDATE
              OUTPUT_FILES(FILE_ID)%JTIME  = JTIME
              NEW_MOMENT = .TRUE.
           ELSE
              NEW_MOMENT = .FALSE.
           END IF
           IF ( OUTPUT_FILES(FILE_ID)%HEADER ) THEN
              NEW_MOMENT = .TRUE.
              OUTPUT_FILES(FILE_ID)%HEADER = .FALSE.
           END IF

                      
           IO_UNIT = OUTPUT_FILES(FILE_ID)%IO_UNIT

           IF( TRIM(VNAME) .EQ. ALLVAR3 )THEN 
               WRITE_ALL = .TRUE.
               NVAR = OUTPUT_FILES(FILE_ID)%NVARS
               OUTPUT_FILES(FILE_ID)%FILLED = NVAR
               OUTPUT_FILES(FILE_ID)%VALUES(1:NVAR) = BUFFER(1,1:NVAR)
           ELSE
               VAR_ID = 0
               DO NVAR = 1,OUTPUT_FILES(FILE_ID)%NVARS
!                  IF ( INDEX(TRIM(OUTPUT_FILES(FILE_ID)%VARNAMES(NVAR)),TRIM(VNAME)) .GT. 0 ) THEN
                  IF ( OUTPUT_FILES(FILE_ID)%VARNAMES(NVAR) .EQ. VNAME ) THEN
                    VAR_ID = NVAR
                    EXIT
                  END IF
               END DO
               IF( VAR_ID .LT. 1 )THEN
                 WRITE(6,'(A)')'WRITE3 ERROR: ' // TRIM(FNAME) // ' does not have variable, '
     &           // TRIM( VNAME )
                 WRITE3R2D = .FALSE.
                 RETURN
               END IF
               OUTPUT_FILES(FILE_ID)%FILLED = OUTPUT_FILES(FILE_ID)%FILLED + 1
               IF( OUTPUT_FILES(FILE_ID)%FILLED .EQ. OUTPUT_FILES(FILE_ID)%NVARS )THEN
                   WRITE_ALL = .TRUE.
               ELSE
                   WRITE_ALL = .FALSE.
               END IF
               OUTPUT_FILES(FILE_ID)%VALUES(VAR_ID) = BUFFER(1,1)
           END IF
           IF ( NEW_MOMENT ) THEN
             OUTPUT_FILES(FILE_ID)%NSTEPS = OUTPUT_FILES(FILE_ID)%NSTEPS + 1
             OUTPUT_FILES(FILE_ID)%FLUSHED = .TRUE.
           ELSE
             BACKSPACE(UNIT = IO_UNIT, ERR = 1000, IOSTAT = IOS)
             OUTPUT_FILES(FILE_ID)%FLUSHED = .FALSE.
           END IF
           WRITE(IO_UNIT,'(2(I7,","),4000(18X,ES16.4,","))')JDATE,JTIME,
     &     (OUTPUT_FILES(FILE_ID)%VALUES(NVAR),NVAR=1,OUTPUT_FILES(FILE_ID)%NVARS)
           IF ( WRITE_ALL ) THEN
!             OUTPUT_FILES(FILE_ID)%VALUES = AMISS3
           END IF

           WRITE3R2D = .TRUE.
           RETURN

1000       WRITE(6,'(A)')'WRITE3R2D ERROR: Failed to rewind ' // TRIM(FNAME) 
     &     // ' one lines.'
           WRITE3R2D = .FALSE.
        END FUNCTION WRITE3R2D
        LOGICAL FUNCTION WRITE3R3D( FNAME, VNAME, JDATE, JTIME, BUFFER )

!          USE M3UTILIO, ONLY: OUTPUT_FILE,N_OUTPUT_FILES,OUTPUT_FILES,SETUP_OUTPUT_FILE

           IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:
           CHARACTER*(*), INTENT(IN   ) :: FNAME         !  logical file name
           CHARACTER*(*), INTENT(IN   ) :: VNAME         !  logical file name
           INTEGER      , INTENT(IN   ) :: JDATE         !  date, formatted YYYYDDD
           INTEGER      , INTENT(IN   ) :: JTIME         !  time, formatted HHMMSS
           REAL         , INTENT(IN   ) :: BUFFER(:,:,:) !  output buffer array

           INTEGER :: FILE_ID
           INTEGER :: VAR_ID
           INTEGER :: NVAR
           INTEGER :: IO_UNIT
           INTEGER :: IOS
           INTEGER :: LDATE, LTIME
  
           LOGICAL :: WRITE_ALL
           LOGICAL :: NEW_MOMENT = .TRUE.

!          WRITE3 = .TRUE.
!          RETURN

           FILE_ID = 0
           DO NVAR = 1,N_OUTPUT_FILES
!              write(6,'(6(A,1X))')'WRITE3: FNAME,OUTPUT_FILES(NVAR)%FILENAME = ',
!     &        TRIM(FNAME),TRIM(OUTPUT_FILES(NVAR)%FILENAME)
              IF ( INDEX(TRIM(OUTPUT_FILES(NVAR)%FILENAME),TRIM(FNAME)) .GT. 0 ) THEN
                FILE_ID = NVAR
                EXIT
              END IF
           END DO
           IF( FILE_ID .LT. 1 )THEN
             WRITE(6,'(A)')'WRITE3 ERROR: ' // TRIM(FNAME) // ' is not opened.'
             WRITE3R3D = .FALSE.
             RETURN
           END IF

           LDATE = OUTPUT_FILES(FILE_ID)%JDATE
           LTIME = OUTPUT_FILES(FILE_ID)%JTIME
           
           IF ( LDATE .NE. JDATE .OR. LTIME .NE. JTIME 
     &          .OR.  LDATE .NE. JDATE .AND. LTIME .NE. JTIME ) THEN
              OUTPUT_FILES(FILE_ID)%JDATE  = JDATE
              OUTPUT_FILES(FILE_ID)%JTIME  = JTIME
              NEW_MOMENT = .TRUE.
           ELSE
              NEW_MOMENT = .FALSE.
           END IF
           IF ( OUTPUT_FILES(FILE_ID)%HEADER ) THEN
              NEW_MOMENT = .TRUE.
              OUTPUT_FILES(FILE_ID)%HEADER = .FALSE.
           END IF
           
           IO_UNIT = OUTPUT_FILES(FILE_ID)%IO_UNIT

           IF( TRIM(VNAME) .EQ. ALLVAR3 )THEN 
               WRITE_ALL = .TRUE.
               NVAR = OUTPUT_FILES(FILE_ID)%NVARS
               OUTPUT_FILES(FILE_ID)%FILLED = NVAR
               OUTPUT_FILES(FILE_ID)%VALUES(1:NVAR) = BUFFER(1,1,1:NVAR)
           ELSE
               VAR_ID = 0
               DO NVAR = 1,OUTPUT_FILES(FILE_ID)%NVARS
!                  IF ( INDEX(TRIM(OUTPUT_FILES(FILE_ID)%VARNAMES(NVAR)),TRIM(VNAME)) .GT. 0 ) THEN
                  IF ( OUTPUT_FILES(FILE_ID)%VARNAMES(NVAR) .EQ. VNAME ) THEN
                    VAR_ID = NVAR
                    EXIT
                  END IF
               END DO
               IF( VAR_ID .LT. 1 )THEN
                 WRITE(6,'(A)')'WRITE3 ERROR: ' // TRIM(FNAME) // ' does not have variable, '
     &           // TRIM( VNAME )
                 WRITE3R3D = .FALSE.
                 RETURN
               END IF
               OUTPUT_FILES(FILE_ID)%FILLED = OUTPUT_FILES(FILE_ID)%FILLED + 1
               IF( OUTPUT_FILES(FILE_ID)%FILLED .EQ. OUTPUT_FILES(FILE_ID)%NVARS )THEN
                   WRITE_ALL = .TRUE.
               ELSE
                   WRITE_ALL = .FALSE.
               END IF
               OUTPUT_FILES(FILE_ID)%VALUES(VAR_ID) = BUFFER(1,1,1)
           END IF
           IF ( NEW_MOMENT ) THEN
             OUTPUT_FILES(FILE_ID)%NSTEPS = OUTPUT_FILES(FILE_ID)%NSTEPS + 1
             OUTPUT_FILES(FILE_ID)%FLUSHED = .TRUE.
           ELSE
             BACKSPACE(UNIT = IO_UNIT, ERR = 1000, IOSTAT = IOS)
             OUTPUT_FILES(FILE_ID)%FLUSHED = .FALSE.
           END IF
           WRITE(IO_UNIT,'(2(I7,","),4000(18X,ES16.4,","))')JDATE,JTIME,
     &     (OUTPUT_FILES(FILE_ID)%VALUES(NVAR),NVAR=1,OUTPUT_FILES(FILE_ID)%NVARS)
           IF ( WRITE_ALL ) THEN
!             OUTPUT_FILES(FILE_ID)%VALUES = AMISS3
           END IF

           WRITE3R3D = .TRUE.
           RETURN

1000       WRITE(6,'(A)')'WRITE3R3D ERROR: Failed to rewind ' // TRIM(FNAME) 
     &     // ' one lines.'
           WRITE3R3D = .FALSE.

        END FUNCTION WRITE3R3D

        LOGICAL FUNCTION WRITE3R4D( FNAME, VNAME, JDATE, JTIME, BUFFER )

!          USE M3UTILIO, ONLY: OUTPUT_FILE,N_OUTPUT_FILES,OUTPUT_FILES,SETUP_OUTPUT_FILE

           IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:
           CHARACTER*(*), INTENT(IN   ) :: FNAME           !  logical file name
           CHARACTER*(*), INTENT(IN   ) :: VNAME           !  logical file name
           INTEGER      , INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
           INTEGER      , INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
           REAL         , INTENT(IN   ) :: BUFFER(:,:,:,:) !  output buffer array

           INTEGER :: FILE_ID
           INTEGER :: VAR_ID
           INTEGER :: NVAR
           INTEGER :: IO_UNIT
           INTEGER :: IOS
           INTEGER :: LDATE, LTIME
  
           LOGICAL :: WRITE_ALL
           LOGICAL :: NEW_MOMENT = .TRUE.

!          WRITE3 = .TRUE.
!          RETURN

           FILE_ID = 0
           DO NVAR = 1,N_OUTPUT_FILES
!              write(6,'(6(A,1X))')'WRITE3: FNAME,OUTPUT_FILES(NVAR)%FILENAME = ',
!     &        TRIM(FNAME),TRIM(OUTPUT_FILES(NVAR)%FILENAME)
              IF ( INDEX(TRIM(OUTPUT_FILES(NVAR)%FILENAME),TRIM(FNAME)) .GT. 0 ) THEN
                FILE_ID = NVAR
                EXIT
              END IF
           END DO
           IF( FILE_ID .LT. 1 )THEN
             WRITE(6,'(A)')'WRITE3 ERROR: ' // TRIM(FNAME) // ' is not opened.'
             WRITE3R4D = .FALSE.
             RETURN
           END IF

           LDATE = OUTPUT_FILES(FILE_ID)%JDATE
           LTIME = OUTPUT_FILES(FILE_ID)%JTIME
           
           IF ( LDATE .NE. JDATE .OR. LTIME .NE. JTIME 
     &          .OR.  LDATE .NE. JDATE .AND. LTIME .NE. JTIME ) THEN
              OUTPUT_FILES(FILE_ID)%JDATE  = JDATE
              OUTPUT_FILES(FILE_ID)%JTIME  = JTIME
              NEW_MOMENT = .TRUE.
           ELSE
              NEW_MOMENT = .FALSE.
           END IF
           IF ( OUTPUT_FILES(FILE_ID)%HEADER ) THEN
              NEW_MOMENT = .TRUE.
              OUTPUT_FILES(FILE_ID)%HEADER = .FALSE.
           END IF
           
           IO_UNIT = OUTPUT_FILES(FILE_ID)%IO_UNIT

           IF( TRIM(VNAME) .EQ. ALLVAR3 )THEN 
               WRITE_ALL = .TRUE.
               NVAR = OUTPUT_FILES(FILE_ID)%NVARS
               OUTPUT_FILES(FILE_ID)%FILLED = NVAR
               OUTPUT_FILES(FILE_ID)%VALUES(1:NVAR) = BUFFER(1,1,1,1:NVAR)
           ELSE
               VAR_ID = 0
               DO NVAR = 1,OUTPUT_FILES(FILE_ID)%NVARS
!                  IF ( INDEX(TRIM(OUTPUT_FILES(FILE_ID)%VARNAMES(NVAR)),TRIM(VNAME)) .GT. 0 ) THEN
                  IF ( OUTPUT_FILES(FILE_ID)%VARNAMES(NVAR) .EQ. VNAME ) THEN
                    VAR_ID = NVAR
                    EXIT
                  END IF
               END DO
               IF( VAR_ID .LT. 1 )THEN
                 WRITE(6,'(A)')'WRITE3 ERROR: ' // TRIM(FNAME) // ' does not have variable, '
     &           // TRIM( VNAME )
                 WRITE3R4D = .FALSE.
                 RETURN
               END IF
               OUTPUT_FILES(FILE_ID)%FILLED = OUTPUT_FILES(FILE_ID)%FILLED + 1
               IF( OUTPUT_FILES(FILE_ID)%FILLED .EQ. OUTPUT_FILES(FILE_ID)%NVARS )THEN
                   WRITE_ALL = .TRUE.
               ELSE
                   WRITE_ALL = .FALSE.
               END IF
               OUTPUT_FILES(FILE_ID)%VALUES(VAR_ID) = BUFFER(1,1,1,1)
           END IF
           IF ( NEW_MOMENT ) THEN
             OUTPUT_FILES(FILE_ID)%NSTEPS = OUTPUT_FILES(FILE_ID)%NSTEPS + 1
             OUTPUT_FILES(FILE_ID)%FLUSHED = .TRUE.
           ELSE
             BACKSPACE(UNIT = IO_UNIT, ERR = 1000, IOSTAT = IOS)
             OUTPUT_FILES(FILE_ID)%FLUSHED = .FALSE.
           END IF
           WRITE(IO_UNIT,'(2(I7,","),4000(18X,ES16.4,","))')JDATE,JTIME,
     &     (OUTPUT_FILES(FILE_ID)%VALUES(NVAR),NVAR=1,OUTPUT_FILES(FILE_ID)%NVARS)
           IF ( WRITE_ALL ) THEN
!             OUTPUT_FILES(FILE_ID)%VALUES = AMISS3
           END IF

           WRITE3R4D = .TRUE.
           RETURN

1000       WRITE(6,'(A)')'WRITE3R4D ERROR: Failed to rewind ' // TRIM(FNAME) 
     &     // ' one lines.'
           WRITE3R4D = .FALSE.

        END FUNCTION WRITE3R4D

       END MODULE M3UTILIO
  
