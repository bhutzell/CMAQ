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
      INTEGER :: NCMAQ_HEAD
      INTEGER, PARAMETER :: NHEAD = 200
      CHARACTER( 90 ) :: CMAQ_HEADER( NHEAD )
      character( 16 ) :: pname = 'SETUP_LOGDEV'
      character(200)  :: xmsg

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

      WRITE ( CMYPE, '(I3.3)' ) MYPE

#ifdef mpas
      IOLOGEQ = PRESTR // CMYPE
     
! MPAS-CMAQ should be able to use same algorithm as in the JUNIT replacement
! (or JUNIT should use the algorithm here) 
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

      ! Create Log Filename using the processor rank 
      IOLOGEQ = PRESTR // CMYPE // '.' // TRIM( APPL_NAME )

#ifndef twoway
      ! Write CMAQ Header to Screen
      IF ( MYPE .EQ. 0 ) THEN
         DO IHEAD = 1,NCMAQ_HEAD
            WRITE( OUTDEV, '(A)' ) CMAQ_HEADER( IHEAD )
         END DO
      END IF
#endif

      LOGDEV = JUNIT()
      OPEN( UNIT=LOGDEV, FILE=IOLOGEQ, status='new', access='sequential', iostat=iost )
      if ( iost .ne. 0 ) then
         xmsg = 'Could not open logfile ' // IOLOGEQ
         call m3exit( pname, 0, 0, xmsg, 1 )
      end if
#endif

      ! Write CMAQ Header to log file
      DO IHEAD = 1,NCMAQ_HEAD
          WRITE( LOGDEV, '(A)' ) CMAQ_HEADER( IHEAD )
      END DO
      WRITE( LOGDEV, * )

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

!.........................................................................
      SUBROUTINE LOG_HEADING( FUNIT, CHEAD_IN )

!  Formats and writes a user-supplied heading to a specific log file.
!  This approach is intended to standardize the log files that are
!  created by CMAQ. The length of the input array is set at 80 because
!  we would like to try limiting lines to 80 characters and a heading
!  should probably just be one line.
!.........................................................................

#ifdef mpas
      use util_module
#else
      use replacement_util_module, only: UPCASE
#endif

      IMPLICIT NONE

      INTEGER, INTENT( IN )           :: FUNIT
      CHARACTER( * ), INTENT( IN )    :: CHEAD_IN
      CHARACTER( len=: ), ALLOCATABLE :: CHEAD
      CHARACTER( 20 )                 :: FMT
      CHARACTER( 20 )                 :: FMT2
      INTEGER                         :: LDASH

      ! Capitalize the heading
      CHEAD = CHEAD_IN
      CALL UPCASE( CHEAD )

      ! Write the heading to the log file
      WRITE( FUNIT, * )
      WRITE( FMT, '("(", I0, "x,A,A,A)")' ) LOG_MAJOR_TAB
      WRITE( FMT2,'("(", I0, "x,A,)")' ) LOG_MAJOR_TAB

      LDASH = 2*8 + LEN_TRIM( CHEAD )
      WRITE( FUNIT, FMT2 ), REPEAT( '=', LDASH )
      WRITE( FUNIT, FMT ),
     &       '|>---   ',TRIM( CHEAD ),'   ---<|'
      WRITE( FUNIT, FMT2 ), REPEAT( '=', LDASH )

      END SUBROUTINE LOG_HEADING
      
!.........................................................................
      SUBROUTINE LOG_SUBHEADING( FUNIT, CHEAD )

!  Formats and writes a user-supplied sub-heading to a specific log file.
!  This approach is intended to standardize the log files that are
!  created by CMAQ. The length of the input array is set at 80 because
!  we would like to try limiting lines to 80 characters and a sub-heading
!  should probably just be one line.
!.........................................................................

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: FUNIT
      CHARACTER( * ), INTENT( IN ) :: CHEAD
      CHARACTER( 20 )       :: FMT
      INTEGER                         :: LDASH

      ! Write the sub-heading to the log file
      WRITE( FMT, '("(/,", I0, "x,A,A,A)")' ) LOG_MAJOR_TAB
      WRITE( FUNIT, FMT ),'|> ',TRIM( CHEAD ),':'
      LDASH = 2*3 - 1 + LEN_TRIM( CHEAD )

      WRITE( FMT, '("(", I0, "x,A,A)")' ) LOG_MAJOR_TAB
      WRITE( FUNIT, FMT ) '+',REPEAT( '=', LDASH )

      END SUBROUTINE LOG_SUBHEADING
      
!.........................................................................
      REAL( 8 ) FUNCTION SET_CPU_TIMING()

! Function set cpu time
      
       IMPLICIT NONE

#ifdef parallel
      INCLUDE 'mpif.h'
#endif
      
 ! local:      
      REAL( 8 )  :: CPU_TIME_FINISH
#ifndef parallel
      REAL       :: REAL_TIME
#endif
       
#ifdef parallel
      SET_CPU_TIMING = MPI_WTIME()
#else
      CALL CPU_TIME( REAL_TIME )
      SET_CPU_TIMING = REAL( REAL_TIME,8 )
#endif
          
      END FUNCTION SET_CPU_TIMING
!.........................................................................
      SUBROUTINE TIMING_DELTA( CPU_TIME_START, CPU_TIME_DELTA )
      
         IMPLICIT NONE

#ifdef parallel
      INCLUDE 'mpif.h'
#endif
        REAL( 8 ), INTENT( INOUT ) :: CPU_TIME_START
        REAL( 8 ), INTENT( INOUT ) :: CPU_TIME_DELTA
      
 ! local:      
      REAL( 8 )  :: CPU_TIME_FINISH
#ifndef parallel
      REAL       :: REAL_TIME
#endif

!  This subroutine calculates and returns elapsed time from CPU_TIME_START
!  as an update to CPU_TIME_DELTA so total time expense can be determine
!  for code block in a cyclng loop.
!  It also resets CPU_TIME_START to current CPU time.

#ifdef parallel
      CPU_TIME_FINISH = MPI_WTIME()
#else
      CALL CPU_TIME( REAL_TIME )
      CPU_TIME_FINISH = REAL( REAL_TIME,8 )
#endif
      CPU_TIME_DELTA = CPU_TIME_DELTA + (CPU_TIME_FINISH-CPU_TIME_START)

#ifdef parallel      
      CPU_TIME_START = MPI_WTIME()
#else
      CALL CPU_TIME( REAL_TIME )
      CPU_TIME_START = REAL( REAL_TIME,8 )
#endif
      
      END SUBROUTINE TIMING_DELTA

!.........................................................................
      SUBROUTINE TIMING_SPLIT( CPU_TIME_START, IMSG, CPROCIN )

!  This subroutine provides a split for the MPI timing functions and
!  then prints out a message for how much time has passed using a
!  character string input for customizing that message. 
!.........................................................................
 
      IMPLICIT NONE

#ifdef parallel
      INCLUDE 'mpif.h'
#endif
      REAL( 8 ), INTENT( INOUT ) :: CPU_TIME_START
      INTEGER,   INTENT( IN   )  :: IMSG   ! What kind of checkpoint this is
      CHARACTER( * ), INTENT(IN), OPTIONAL :: CPROCIN
      
! local:      
      REAL( 8 )        :: CPU_TIME_FINISH
                                 !  1 = 'PROCESS'
                                 !  2 = 'MASTER TIME STEP'
                                 !  3 = 'OUTPUT'
      CHARACTER( len=: ), ALLOCATABLE :: CPROC
      CHARACTER( 250 ) :: XMSG

#ifndef parallel
      REAL :: REAL_TIME
#endif


      IF ( PRESENT( CPROCIN ) ) THEN
         CPROC = CPROCIN
      ELSE
         CPROC = ' '
      END IF
 
      ! Record Time at this Checkpoint
#ifdef parallel
      CPU_TIME_FINISH = MPI_WTIME()
#else
      CALL CPU_TIME( REAL_TIME )
      CPU_TIME_FINISH = REAL( REAL_TIME,8 )
#endif


      ! Assemble the statement requested by the calling program
      SELECT CASE ( IMSG )
      CASE ( 1 )
          ! Write Out The Time to Complete Each Sub-Process
          WRITE( XMSG, 1002 ),TRIM( CPROC ), CPU_TIME_FINISH-CPU_TIME_START
1002      FORMAT ( 2x, A15, ' completed... ', F12.4, ' seconds' )

      CASE ( 2 ) 
          ! Write out the time to complete the entire master time step
          WRITE( XMSG, '(7x,A16)' ),'Master Time Step'
          CALL LOG_MESSAGE( LOGDEV, XMSG )
#ifndef twoway
          IF ( MYPE .EQ. 0 ) CALL LOG_MESSAGE( OUTDEV, XMSG )
#endif
          WRITE( XMSG, '(7x,A24,F12.4,A8)' ),'Processing completed... ',
     &                     (CPU_TIME_FINISH-CPU_TIME_START),' seconds'
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          WRITE( LOGDEV, * )

      CASE ( 3 ) 
          ! Write out the time to complete the output procedure
          WRITE( XMSG, '(1x,A32,F10.4,A)' ), '=--> Data Output completed...   ',
     &                     (CPU_TIME_FINISH-CPU_TIME_START),' seconds'
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          WRITE( LOGDEV, * )
#ifndef twoway
          IF ( MYPE .EQ. 0 ) WRITE( OUTDEV, * )
          
#endif
      CASE( 4 )
 
      END SELECT 

      ! Write out the timing statement
#ifndef twoway
      IF ( MYPE .EQ. 0 ) CALL LOG_MESSAGE( OUTDEV, XMSG )
#endif
#ifdef parallel      
      CPU_TIME_START = MPI_WTIME()
#else
      CALL CPU_TIME( REAL_TIME )
      CPU_TIME_START = REAL( REAL_TIME,8 )
#endif
      END SUBROUTINE TIMING_SPLIT    
 
      subroutine m3exit( pname, jdate, jtime, xmsg, xstat )
         implicit none
         character(*), intent(in) :: pname, xmsg
         integer, intent(in)      :: jdate, jtime, xstat

         if ( xstat .ne. 0 ) then
            call log_warning( pname, logdev, 'ERROR ABORT' )
         end if
         call log_message( logdev, xmsg )
         call mio_shutdown

      end subroutine m3exit

      end module logdev_mod
