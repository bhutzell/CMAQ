        module logdev_mod
         


        IMPLICIT NONE

        INTEGER :: LOGDEV = -1   ! File Unit for Standard Output
        INTEGER :: ERROR
        INTEGER :: LOG_LINE_LENGTH = 80  ! Cut the log offs at this character if possible
        INTEGER, PARAMETER :: MAXLEN_CCTM_APPL_loc     = 200    ! Length of Logfile Names
        INTEGER :: LOG_MAJOR_TAB   = 5   ! Left tab for all text including headings
        INTEGER :: LOG_MINOR_TAB   = 2   ! Tab for indenting subsequent lines of text in
                                       ! a paragraph for instance.

        CHARACTER( 3 ) :: CMYPE  = ""  ! Processor Number
        INTEGER :: OUTDEV = 6   ! File Unit for Standard Output
        INTEGER :: MYPE   = -1  ! Processor Number
        CHARACTER( 200 ) :: APPL_NAME = 'APPL' ! Logfile Names



        contains
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SETUP_LOGDEV (APP_NAME_IN) 

C Purpose
C   Establish ancillary run time log files for each parallel processor
C   Effectively no operation, if serial

C Revision History:
C   ???      J.Young - initial
C   26 Dec, 01 David Wong
C      -- removed MPI_INIT call since the same function has been called in
C         subroutine PAR_INIT
C   11 Jun 02 J.Young - minor mods
C   23 Feb 11 J.Young - SETENVVAR is a function, not a subroutine
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN;
C                        removed deprecated TRIMLEN
C   12 Aug 15 D.Wong:   Replaced SUBST_MPI with mpif.h

      !USE RUNTIME_VARS
      USE UTILIO_DEFN

      IMPLICIT NONE

#ifdef parallel      
      include 'mpif.h'
#endif
      INTEGER, PARAMETER :: MAXLEN_CTM_LOG   = MAXLEN_CCTM_APPL_loc + 11


      CHARACTER( * ), INTENT( IN )    :: APP_NAME_IN

      INTEGER :: TOTPE  = 1   ! Number of Total Processors
      CHARACTER(  8 ) :: PRESTR  = 'CTM_LOG_'
      CHARACTER( MAXLEN_CTM_LOG ) :: IOLOGEQ

      INTEGER :: IOST, IHEAD
      INTEGER :: NIOAPI_HEAD, NCMAQ_HEAD
      INTEGER, PARAMETER :: NHEAD = 200
      CHARACTER( 90 ) :: CMAQ_HEADER( NHEAD )
      CHARACTER( 90 ) :: IOAPI_HEADER( NHEAD )

      integer :: i
      logical :: found, opened
 
      APPL_NAME = APP_NAME_IN

#ifdef parallel
      CALL MPI_COMM_RANK ( MPI_COMM_WORLD, MYPE, ERROR )
      CALL MPI_COMM_SIZE ( MPI_COMM_WORLD, TOTPE, ERROR )
#else
      MYPE = 0
      TOTPE = 0
#endif
      ! Fetch CMAQ Header
      CALL LOAD_HEADER( CMAQ_HEADER, NCMAQ_HEAD )


#ifdef mpas
      write ( cmype, '(I3.3)' ) mype
      iologeq = 'CTM_LOG_' // cmype

      i = 99
      found = .false.
      do while ((.not. found) .and. (i .le. 150))
         inquire (unit=i, opened=opened)
         if (.not. opened) then
            found = .true.
         else
            i = i + 1
         end if
      end do

      logdev = i

      open ( unit    =  logdev,
     &       iostat  =  iost,
     &       file    =  iologeq,
     &       status  =  'new',
     &       access  =  'sequential')

#else

      ! Create Log Filename using the processor rank and initialize
      ! I/O-API and Logs
      WRITE ( CMYPE, '(I3.3)' ) MYPE
      IOLOGEQ = PRESTR // CMYPE // '.' // TRIM( APPL_NAME )
      IF ( .NOT. SETENVVAR ( 'LOGFILE', IOLOGEQ ) ) THEN
         WRITE( *,* ) '*** Could not set environment variable for ' // IOLOGEQ
         CALL M3EXIT( 'SETUP_LOGDEV', 0, 0, '', 2 )
      END IF

#ifndef twoway
      ! Write CMAQ Header to Screen
      IF ( MYPE .EQ. 0 ) THEN
         DO IHEAD = 1,NCMAQ_HEAD
            WRITE( OUTDEV, '(A)' ) CMAQ_HEADER( IHEAD )
         END DO
      END IF
#endif

      ! Redirect Standard Output
      IF ( MYPE .NE. 0 ) OPEN( UNIT = 6, FILE = "/dev/null", STATUS = "OLD" )

      LOGDEV = INIT3() 

      ! Put Standard Output Back to File Unit 6 By Closing File Unit 6
      IF ( MYPE .NE. 0 ) CLOSE( 6 ) 

      ! Fetch I/O-API Header
#ifndef twoway
      REWIND( LOGDEV )
      DO IHEAD = 1,NHEAD
          READ( LOGDEV, '(A)', IOSTAT=IOST ), IOAPI_HEADER( IHEAD )
          ! Check for End of File
          IF ( IOST .LT. 0 ) THEN
             NIOAPI_HEAD = IHEAD - 1
             EXIT
          END IF
      END DO
      REWIND( LOGDEV )
#endif
#endif

      ! Write CMAQ Header and then I/O-API Header
      DO IHEAD = 1,NCMAQ_HEAD
          WRITE( LOGDEV, '(A)' ) CMAQ_HEADER( IHEAD )
      END DO
      WRITE( LOGDEV, * )
#ifndef mpas
#ifndef twoway
      DO IHEAD = 1,NIOAPI_HEAD
          WRITE( LOGDEV, '(A)' ) IOAPI_HEADER( IHEAD )
      END DO
#endif
#endif

      END SUBROUTINE SETUP_LOGDEV 

!.........................................................................
      SUBROUTINE LOG_MESSAGE( FUNIT, CMSG_IN )

!  Formats and writes a user-supplied message to a specific log file.
!  This approach is intended to standardize the log files that are
!  created by CMAQ.
!.........................................................................

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: FUNIT
      CHARACTER( * ), INTENT( IN ) :: CMSG_IN
      CHARACTER( len=: ), ALLOCATABLE :: CMSG
      CHARACTER( 20 )       :: FMT

      CHARACTER( LOG_LINE_LENGTH ) :: CTEMP
      INTEGER :: MSG_SIZE
      INTEGER :: NLINE1, NLINE2, NLINE, NTAB, LAST_SPACE

      CMSG = CMSG_IN

      ! Write the message to the log file, while trimming to 80
      ! characters (while accounting for tab-length)
      NLINE1 = LOG_LINE_LENGTH - LOG_MAJOR_TAB
      NLINE2 = LOG_LINE_LENGTH - LOG_MAJOR_TAB - LOG_MINOR_TAB
      NLINE  = NLINE1
      NTAB   = LOG_MAJOR_TAB
      
      ! Determine Length of Total Message
      MSG_SIZE = LEN_TRIM( CMSG )
      
      DO WHILE ( MSG_SIZE .GT. LOG_LINE_LENGTH )
         ! Isolate One Line of Text
         LAST_SPACE = INDEX( CMSG( 1:NLINE+1 ), " ", BACK=.TRUE. )
     
         CTEMP = CMSG( 1:LAST_SPACE-1 )

         WRITE( FMT, '("(", I0, "x,A)")' ) NTAB
         WRITE( FUNIT, FMT ), CTEMP

         CMSG = CMSG( LAST_SPACE+1:LEN( CMSG ) )
         MSG_SIZE = LEN_TRIM( CMSG )

         IF ( NTAB .EQ. LOG_MAJOR_TAB ) NTAB = NTAB + LOG_MINOR_TAB
         IF ( NLINE.EQ. NLINE1 ) NLINE = NLINE2
      END DO

      ! Write Last Line
      WRITE( FMT, '("(", I0, "x,A)")' ) NTAB
      WRITE( FUNIT, FMT ), TRIM( CMSG )


      END SUBROUTINE LOG_MESSAGE
!.........................................................................
      SUBROUTINE LOG_WARNING( PNAME, FUNIT, CMSG_IN )
!
!  Formats and writes a user-supplied warning to a specific log file.
!  This approach is intended to standardize the log files that are
!  created by CMAQ.
!.........................................................................

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: FUNIT
      CHARACTER( * ), INTENT( IN ) :: PNAME, CMSG_IN
      CHARACTER( len=: ), ALLOCATABLE :: CMSG

      CHARACTER( 20 )       :: FMT
      CHARACTER( 50 )       :: BANNER

      CHARACTER( LOG_LINE_LENGTH ) :: CTEMP
      INTEGER :: MSG_SIZE
      INTEGER :: NLINE1, NLINE2, NLINE, NTAB, LAST_SPACE

      CMSG = CMSG_IN


      ! Write a banner indicating that this is a WARNING and
      ! identifying the program causing it
      BANNER = TRIM('!! WARNING !! in ' // PNAME)
      WRITE( FUNIT, '( 5X, A )' )  BANNER
 

      ! Write the message to the log file, while trimming to 80
      ! characters (while accounting for tab-length)
      NLINE1 = LOG_LINE_LENGTH - LOG_MAJOR_TAB
      NLINE2 = LOG_LINE_LENGTH - LOG_MAJOR_TAB - LOG_MINOR_TAB
      NLINE  = NLINE1
      NTAB   = LOG_MAJOR_TAB
      
      ! Determine Length of Total Message
      MSG_SIZE = LEN_TRIM( CMSG )
      
      DO WHILE ( MSG_SIZE .GT. LOG_LINE_LENGTH )
         ! Isolate One Line of Text
         LAST_SPACE = INDEX( CMSG( 1:NLINE+1 ), " ", BACK=.TRUE. )
     
         CTEMP = CMSG( 1:LAST_SPACE-1 )

         WRITE( FMT, '("(", I0, "x,A)")' ) NTAB
         WRITE( FUNIT, FMT ), CTEMP

         CMSG = CMSG( LAST_SPACE+1:LEN( CMSG ) )
         MSG_SIZE = LEN_TRIM( CMSG )

         IF ( NTAB .EQ. LOG_MAJOR_TAB ) NTAB = NTAB + LOG_MINOR_TAB
         IF ( NLINE.EQ. NLINE1 ) NLINE = NLINE2
      END DO

      ! Write Last Line
      WRITE( FMT, '("(", I0, "x,A)")' ) NTAB
      WRITE( FUNIT, FMT ), TRIM( CMSG )


      END SUBROUTINE LOG_WARNING

        end module logdev_mod
