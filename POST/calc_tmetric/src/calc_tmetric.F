!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!

C****************************************************************************************
       PROGRAM CALC_TMETRIC
c
C*****************************************************************************************
      USE M3FILES
      USE SPECIES
      USE GRID_DATA
      USE TIME_STEP
      USE GET_ENV_VARS

      USE M3UTILIO

      IMPLICIT NONE

C external functions
      character*(16) int2Str
      character*(16) real2Str
      character*(16) date2Str
      character*(16) time2Str

C local variables
      integer status
      logical lstatus
      integer logdev
      INTEGER NVARS_IN, KSWIT
      character*(3)  c_proc
      character*(16)  field
      character*(256)  infile
      character*(256)  outfile
      character*(256)  MSG
      character*(16)  VNAME_IN(MXVARS3)
      character*(16)  VUNITS_IN(MXVARS3)
      character*(16) ::  PNAME = 'CALC_TMETRIC'
      character*(16) :: VERSION = '1.0'
      real, allocatable :: in_data(:,:,:) 
      real, allocatable :: sum_data(:,:,:) 
      real, allocatable :: MIN_PARALLEL(:,:,:,:) 
      real, allocatable :: proc_parallel(:,:,:,:) 
      real, allocatable :: min_serial(:,:,:) 
      real, allocatable :: proc_serial(:,:,:) 
      CHARACTER*(16), ALLOCATABLE :: OUT_VARNAME( : )
      CHARACTER*(16), ALLOCATABLE :: OUT_UNITS( : )
      integer lfn
      integer i, s, t, idx, K, N, NAVG
      integer col, row, lay
      real x, y, lon, lat
      
      INTEGER   NLOOPS      
      INTEGER   AVG_TIME
      INTEGER   N_RETREIVE   ! Number of hour to read for files per metric
      INTEGER   N_SAMPLE     ! NUMBER OF HOURS INCLUDED TO THE AVERAGE   
      INTEGER   N_PULL       ! ARRAY index
      INTEGER   OUTPUT_FREQ  ! 
      INTEGER   HOURS, MINUTES, SECONDS, START_SECONDS
      INTEGER   PULL_DATE, PULL_TIME
      INTEGER   NCOLS, NROWS, NLAYS
      INTEGER   :: OPERATION_FLAG = -1 
      
      CHARACTER(LEN = M3_FILE_LEN) :: PULL_FLNAME

      LOGICAL :: COMPUTE_AVG = .FALSE.
      LOGICAL :: PARALLEL_PROCESS = .TRUE.


C**********************************************************************

C... write program name and version number
      WRITE( *, '(a)' )
     &    '>>---->  Program ' // TRIM(PNAME) // ' (Version ' // TRIM(Version) // ')  <----<<' 

      lfn = 10

C... start program
      logdev = init3 ()

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the Models-3 file(s) to process and the other environment   
c  variables
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      CALL OPEN_M3FILES

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check the file(s) for consistency and make sure the requested   
c  species is on the file(s)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL CK_M3FLS()

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the grid definition and the tsteps from the M3 files
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL GET_M3GRID

      CALL GET_TSTEPS


C... get operation (SUM or AVG)
!     CALL ENVSTR( 'OPERATION', 'Operation to be peformed - SUM or AVG', 'SUM', c_proc, status)
      CALL GET_ENV_STRING( 'OPERATION', 'Operation to be peformed - SUM or AVG', 'SUM', C_PROC, STATUS )

!      IF ( ( C_PROC .NE. 'SUM') .AND. ( C_PROC .NE. 'AVG') )THEN
!        WRITE(6,'("OPERATIONS set to ",A)')TRIM( C_PROC )
!        CALL M3ERR(PNAME, 0, 0, 'Must set OPERATION to SUM or AVG', .TRUE.)      
!      ENDIF
!      
!      IF( C_PROC .EQ. 'AVG' )COMPUTE_AVG = .TRUE.

      SELECT CASE ( C_PROC )
        CASE( 'AVG' )
            OPERATION_FLAG = -1
            COMPUTE_AVG    = .TRUE.
        CASE( 'SUM' )
            OPERATION_FLAG = 0
        CASE( 'MIN' )
            OPERATION_FLAG = 1
        CASE( 'MAX' )
            OPERATION_FLAG = 2
        CASE( 'RNG' )  ! range
            OPERATION_FLAG = 3
        CASE DEFAULT
            WRITE(6,'("OPERATIONS set to ",A)')TRIM( C_PROC )
            CALL M3ERR(PNAME, 0, 0, 
     &      'Must set OPERATION to MAX, MIN, RNG (Range_, SUM or AVG', .TRUE.)      
      END SELECT
      WRITE(6,'("Operational Flag, COMPUTE_AVG = ",I3,", ",L4)')OPERATION_FLAG,
     & COMPUTE_AVG      
      
C... get species definitions from system variables
      Call loadSpecies()
      write(*,'(i5,'' species defined'')') NSPECVAR
      IF ( NSPECVAR .LE. 0 ) THEN
          WRITE(6,'("ERROR:: No species defined. Check Environment Varaible SPECIES")')
!         CALL NF90_CLOSE
          STOP
      END IF

C... get name of output file 
C      CALL ENVSTR( 'OUTFILE', 'Name of output file', 'OUTFILE', outfile, status)

C... set up metadata for output file 

      IF( .NOT. DESC3 ( M3_FLNAME( 1 ) ) ) THEN
         MSG = 'Could not read DESC of  ' // M3_FLNAME( 1 ) 
     &         // ' file'
         CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )
      ENDIF
      
      NVARS_IN = NVARS3D
      DO S = 1, NVARS_IN !obtain units for all species from first input file
       VNAME_IN(S)  = VNAME3D(S)
       VUNITS_IN(S) = UNITS3D(S)
      ENDDO !S

      TSTEP3D = 0  
      AVG_TIME = 3 ! MIN( 3,NSTEPS )
      AVG_TIME = GET_ENV_INT( "SAMPLE_PERIOD", "Sampling Period for output metric",
     &                         AVG_TIME, STATUS ) 
      IF( AVG_TIME .GT. NSTEPS )THEN
          WRITE( MSG,'(A)')'Error total timestep for input file(s) '
     &    // ' less than requested sample size or averaging period.'
          WRITE( 6,'(5X,A)')TRIM( MSG )
          WRITE( 6,'(5X,"SAMPLE SIZE = ",I5)')AVG_TIME
          WRITE( 6,'(5X,"Number TimeSteps from Input File(s) = ",I5)')NSTEPS
          MSG = 'Correct the above Error by changing value of AVG_TIME option.'
          CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )
      ELSE IF ( AVG_TIME .LE. 0 ) THEN
          WRITE( MSG,'(A)')'Requested sample size or averaging period <= 0.'
          WRITE( MSG,'(A)')'Setting to number of timesteps over input files.'
          WRITE( 6,'(5X,A)')TRIM( MSG )
          AVG_TIME = NSTEPS
          WRITE( 6,'(5X,"SAMPLE SIZE = ",I5)')AVG_TIME
      END IF
      IF( MOD(NSTEPS,AVG_TIME) .GT. 0 )THEN
          WRITE( 6,'(A)')'Sample size or averaging period does not '
     &    // 'divide evenly into total timesteps from input file(s). '
          WRITE( 6,'(5X,"No Output based on Last ",I4," timestep(s).")')
     &    MOD( NSTEPS,AVG_TIME)
      END IF
      OUTPUT_FREQ = AVG_TIME
      NLOOPS      = MAX( NSTEPS - ( AVG_TIME - 1 ),1 )

! based on output frequency compute output file timestep
      HOURS   = INT( TIMESTEP/10000 )
      SECONDS = MOD( TIMESTEP,100 )
      MINUTES = MAX( (INT(TIMESTEP/100)-100*HOURS),0 )
! value of input timestep in seconds 
      SECONDS  = OUTPUT_FREQ * ( 60*( 60*HOURS + MINUTES ) + SECONDS )
! assemble output timestep in HHMMSS format
      HOURS   = INT( SECONDS/3600 )
      MINUTES = MAX( ( INT(SECONDS/60)-3600*HOURS ),0  )
      SECONDS = MOD(SECONDS,60) 
      TSTEP3D = 100*( 100*HOURS + MINUTES ) + SECONDS
! start date and time of output file
      N_PULL  = MAX( AVG_TIME-1,1 )
      N_PULL  = MAX( AVG_TIME,1 )
      SDATE3D = STEP_DATE( N_PULL )
      STIME3D = STEP_TIME( N_PULL )
      
      NVARS3D = NSPECVAR    !number of output species from module_spec 
      ALLOCATE( OUT_VARNAME( NSPECVAR ),
     &           OUT_UNITS(  NSPECVAR ), stat=status )
      IF ( status .NE. 0 ) THEN
         MSG = 'Failure allocating OUT_VARNAME or OUT_UNITS'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
      END IF

      DO S = 1, NSPECVAR
       VNAME3D(S) = TRIM(SPECVARS(S))//"_"//C_PROC      
       VTYPE3D(S) = M3REAL      
       VDESC3D(S) = C_PROC//" OF "//TRIM(SPECVARS(S))
       UNITS3D(S) = VUNITS_IN(INDEX1(SPECVARS(S),NVARS_IN,VNAME_IN))
       OUT_VARNAME(S) = VNAME3D(S)
       OUT_UNITS(S)   = UNITS3D(S)
      ENDDO !S

C... open output file as new 
      if( .NOT. OPEN3( 'OUTFILE', FSCREA3, TRIM(PNAME) ) ) THEN
        WRITE( *, '(''**ERROR** while openning OUTFILE'')' )
        stop 
      endif 

      NROWS = NROWS3D
      NCOLS = NCOLS3D
      NLAYS = NLAYS3D

C... start process loop

      ! allocate data array for 1 species
!     allocate( in_data(NCOLS, NROWS, NLAYS), stat=status )
      allocate( in_data(NCOLS, NROWS, NLAYS), 
     &          sum_data(NCOLS, NROWS, NLAYS), stat=status )
      IF ( status .NE. 0 ) THEN
         MSG = 'Failure allocating in_data or sum_data'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
      END IF

      PARALLEL_PROCESS = .FALSE.
      IF( PARALLEL_PROCESS )THEN
      IF ( OPERATION_FLAG .GT. 2 ) THEN
         ALLOCATE(  PROC_PARALLEL(NSPECVAR, NCOLS, NROWS, NLAYS),
     &              MIN_PARALLEL(NSPECVAR, NCOLS, NROWS, NLAYS), stat=status )
         MSG = 'Failure allocating PROC_PARALLEL or MIN_PARALLEL.'
      ELSE
         ALLOCATE( PROC_PARALLEL(NSPECVAR, NCOLS, NROWS, NLAYS), stat=status )
         MSG = 'Failure allocating PROC_PARALLEL.'
      END IF
      END IF
      
      IF( STATUS .NE. 0 )THEN
          MSG = TRIM( MSG )
          WRITE(6,'(A)')TRIM( MSG ) // ' Attempting slower serial processing.'
          PARALLEL_PROCESS = .FALSE.
      END IF 
      
      PARALLEL_PROCESS = .FALSE.

      IF( PARALLEL_PROCESS )THEN
          CALL PROCESS_PARALLEL
      ELSE
          IF ( OPERATION_FLAG .GT. 2 ) THEN
             ALLOCATE(  PROC_SERIAL( NCOLS, NROWS, NLAYS),
     &                  MIN_SERIAL( NCOLS, NROWS, NLAYS), stat=status )
             MSG = 'Failure allocating PROC_PARALLEL or MIN_PARALLEL'
          ELSE
             ALLOCATE( PROC_SERIAL( NCOLS, NROWS, NLAYS), stat=status )
             MSG = 'Failure allocating PROC_PARALLEL'
          END IF
          IF( STATUS .NE. 0 )CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
          CALL PROCESS_SEARIAL
      END IF
      
      
!      DO N = 1, NLOOPS, OUTPUT_FREQ ! loop for each block average
!         
!          N_RETREIVE = MIN( N+AVG_TIME-1,NSTEPS)
!          IF( N+AVG_TIME-1 .GT. NSTEPS ) EXIT ! Do not compute for incomplete sample
!          N_SAMPLE  = 0
!          
!!  initial data array(s) based on wanted operation
!          IF ( OPERATION_FLAG .EQ. 1 ) THEN       
!            PROC_PARALLEL =  1.0E36
!          ELSE IF ( OPERATION_FLAG .EQ. 2 ) THEN
!            PROC_PARALLEL  = -1.0E36
!          ELSE IF ( OPERATION_FLAG .EQ. 3 ) THEN
!            PROC_PARALLEL  = -1.0E36
!            MIN_PARALLEL   =  1.0E36
!          ELSE
!            PROC_PARALLEL = 0.0
!          END IF
!          DO NAVG = N, N_RETREIVE             
!! have to determine if reading one timestep too many
!              N_PULL = MIN( NAVG,NSTEPS ) 
!              N_SAMPLE = N_SAMPLE + 1
!              PULL_FLNAME = M3_FLNAME(STEP_FILE(N_PULL))
!              PULL_DATE   = STEP_DATE(N_PULL)
!              PULL_TIME   = STEP_TIME(N_PULL)
!          
!              DO S = 1, NSPECVAR
!
!                  IF( .NOT. READ3(PULL_FLNAME,SPECVARS(S),ALLAYS3,PULL_DATE,PULL_TIME,IN_DATA) )THEN
!                      MSG = 'Could not read input Models-3 file ' // PULL_FLNAME
!                      CALL M3ERR( PNAME, PULL_DATE, PULL_TIME, MSG, .FALSE. )             
!                  END IF
!
!                  SELECT CASE ( OPERATION_FLAG )
!                    CASE( -1 )
!                       PROC_PARALLEL(S,:,:,:) = PROC_PARALLEL(S,:,:,:) + IN_DATA 
!                    CASE( 0 )
!                       PROC_PARALLEL(S,:,:,:) = PROC_PARALLEL(S,:,:,:) + IN_DATA 
!                    CASE( 1 )
!                       WHERE( PROC_PARALLEL(S,:,:,:) .GT. IN_DATA ) PROC_PARALLEL(S,:,:,:) = IN_DATA                   
!                    CASE( 2 )
!                       WHERE( PROC_PARALLEL(S,:,:,:) .LT. IN_DATA ) PROC_PARALLEL(S,:,:,:) = IN_DATA 
!                    CASE( 3 )
!                       WHERE( PROC_PARALLEL(S,:,:,:) .LT. IN_DATA ) PROC_PARALLEL(S,:,:,:) = IN_DATA 
!                       WHERE( MIN_PARALLEL(S,:,:,:) .GT. IN_DATA ) MIN_PARALLEL(S,:,:,:) = IN_DATA                   
!                  END SELECT                   
!#ifdef verbose
!                  IF ( NAVG .EQ. N_RETREIVE )THEN
!                      write(*,'(''Done Processing species:'',a)') TRIM(SPECVARS(S))
!                  END IF
!#endif
!              END DO ! NSPECVAR
!#ifdef verbose
!             WRITE(6,'(3(A),2(I8,1X))')'Reading ',TRIM(SPECVARS(S)),' at ', 
!     &       PULL_DATE, PULL_TIME
!#endif
!          END DO ! NAVG
!!          Write(*,'(''Done reading for species '',a,I4,a)') VNAME3D(S),N_SAMPLE,
!!     &    ' timestep(s) read.'
!          Write(*,'(''Done reading for species '',I4,'' timestep(s) read.'')') N_SAMPLE
!c         compute average if desired, write out the results
!          IF( OPERATION_FLAG .GT. 2 ) THEN
!             PROC_PARALLEL = PROC_PARALLEL - MIN_PARALLEL
!          ELSE IF ( OPERATION_FLAG .LT. 0 ) THEN
!             PROC_PARALLEL = PROC_PARALLEL / REAL(N_SAMPLE,4)     
!          END IF        
!          write(6,'("writing for JDATE, JTIME = ",2(I8,1X))')PULL_DATE,PULL_TIME
!          DO S = 1, NSPECVAR
!              IF ( .NOT. WRITE3('OUTFILE', OUT_VARNAME(S), PULL_DATE,
!     &                            PULL_TIME, PROC_PARALLEL(S,:,:,:)) ) THEN
!                  MSG = ' Cannot write to OUTFILE for species ' // VNAME3D(S)
!                  CALL M3EXIT( PNAME, STEP_DATE(N_PULL), STEP_TIME(N_PULL), MSG, 1 ) 
!              ENDIF
!          END DO ! NSPECVAR
!      END DO ! NLOOP    
               
      WRITE(6,'(''finished output values'')')

      DEALLOCATE(IN_DATA)
      IF( PARALLEL_PROCESS )THEN
         DEALLOCATE(PROC_PARALLEL)
         IF( OPERATION_FLAG .GT. 2 )DEALLOCATE(MIN_PARALLEL)
      ELSE
         DEALLOCATE(PROC_SERIAL)
         IF( OPERATION_FLAG .GT. 2 )DEALLOCATE(MIN_SERIAL)
      END IF

C... shut down ioapi
      KSWIT = SHUT3()
      WRITE(*,'('' IOAPI Shut down'')')
      STOP

      CONTAINS
         SUBROUTINE PROCESS_PARALLEL()
           IMPLICIT NONE

           DO N = 1, NLOOPS, OUTPUT_FREQ ! loop for each block average
      
               N_RETREIVE = MIN( N+AVG_TIME-1,NSTEPS)
               IF( N+AVG_TIME-1 .GT. NSTEPS ) EXIT ! Do not compute for incomplete sample
               N_SAMPLE  = 0
!  initial data array(s) based on wanted operation
               IF ( OPERATION_FLAG .EQ. 1 ) THEN
                 PROC_PARALLEL =  1.0E36
               ELSE IF ( OPERATION_FLAG .EQ. 2 ) THEN
                 PROC_PARALLEL  = -1.0E36
               ELSE IF ( OPERATION_FLAG .EQ. 3 ) THEN
                 PROC_PARALLEL  = -1.0E36
                 MIN_PARALLEL   =  1.0E36
               ELSE
                 PROC_PARALLEL = 0.0
               END IF
               DO NAVG = N, N_RETREIVE
! have to determine if reading one timestep too many
                  N_PULL = MIN( NAVG,NSTEPS )
                  N_SAMPLE = N_SAMPLE + 1
                  PULL_FLNAME = M3_FLNAME(STEP_FILE(N_PULL))
                  PULL_DATE   = STEP_DATE(N_PULL)
                  PULL_TIME   = STEP_TIME(N_PULL)
    
                  DO S = 1, NSPECVAR
    
                      IF( .NOT. READ3(PULL_FLNAME,SPECVARS(S),ALLAYS3,PULL_DATE,PULL_TIME,IN_DATA) )THEN
                          MSG = 'Could not read input Models-3 file ' // PULL_FLNAME
                          CALL M3ERR( PNAME, PULL_DATE, PULL_TIME, MSG, .FALSE. )
                      END IF
    
                      SELECT CASE ( OPERATION_FLAG )
                        CASE( -1 )
                           PROC_PARALLEL(S,:,:,:) = PROC_PARALLEL(S,:,:,:) + IN_DATA
                        CASE( 0 )
                           PROC_PARALLEL(S,:,:,:) = PROC_PARALLEL(S,:,:,:) + IN_DATA
                        CASE( 1 )
                           WHERE( PROC_PARALLEL(S,:,:,:) .GT. IN_DATA ) PROC_PARALLEL(S,:,:,:) = IN_DATA
                        CASE( 2 )
                           WHERE( PROC_PARALLEL(S,:,:,:) .LT. IN_DATA ) PROC_PARALLEL(S,:,:,:) = IN_DATA
                        CASE( 3 )
                           WHERE( PROC_PARALLEL(S,:,:,:) .LT. IN_DATA ) PROC_PARALLEL(S,:,:,:) = IN_DATA
                           WHERE( MIN_PARALLEL(S,:,:,:) .GT. IN_DATA ) MIN_PARALLEL(S,:,:,:) = IN_DATA
                      END SELECT
#ifdef verbose
                      IF ( NAVG .EQ. N_RETREIVE )THEN
                          write(*,'(''Done Processing species:'',a)') TRIM(SPECVARS(S))
                      END IF
#endif
                  END DO ! NSPECVAR
#ifdef verbose
                  WRITE(6,'(3(A),2(I8,1X))')'Reading ',TRIM(SPECVARS(S)),' at ',
     &            PULL_DATE, PULL_TIME
#endif
               END DO ! NAVG
               WRITE(*,'(''Done reading for species '',I4,'' timestep(s) read.'')') N_SAMPLE
c         compute average if desired, write out the results
               IF( OPERATION_FLAG .GT. 2 ) THEN
                  PROC_PARALLEL = PROC_PARALLEL - MIN_PARALLEL
               ELSE IF ( OPERATION_FLAG .LT. 0 ) THEN
                  PROC_PARALLEL = PROC_PARALLEL / REAL(N_SAMPLE,4)
               END IF
               WRITE(6,'("writing for JDATE, JTIME = ",2(I8,1X))')PULL_DATE,PULL_TIME
               DO S = 1, NSPECVAR
                   IF ( .NOT. WRITE3('OUTFILE', OUT_VARNAME(S), PULL_DATE,
     &                                 PULL_TIME, PROC_PARALLEL(S,:,:,:)) ) THEN
                       MSG = ' Cannot write to OUTFILE for species ' // VNAME3D(S)
                       CALL M3EXIT( PNAME, STEP_DATE(N_PULL), STEP_TIME(N_PULL), MSG, 1 )
                   ENDIF
               END DO ! NSPECVAR
           END DO ! NLOOP

         END  SUBROUTINE PROCESS_PARALLEL
         SUBROUTINE PROCESS_SEARIAL()
           IMPLICIT NONE
           
           
           DO S = 1, NSPECVAR
              DO N = 1, NLOOPS, OUTPUT_FREQ ! loop for each block average
      
                  N_RETREIVE = MIN( N+AVG_TIME-1,NSTEPS)
                  IF( N+AVG_TIME-1 .GT. NSTEPS ) EXIT ! Do not compute for incomplete sample
                  N_SAMPLE  = 0
!  initial data array(s) based on wanted operation
                  IF ( OPERATION_FLAG .EQ. 1 ) THEN
                    PROC_SERIAL =  1.0E36
                  ELSE IF ( OPERATION_FLAG .EQ. 2 ) THEN
                    PROC_SERIAL  = -1.0E36
                  ELSE IF ( OPERATION_FLAG .EQ. 3 ) THEN
                    PROC_SERIAL  = -1.0E36
                    MIN_SERIAL   =  1.0E36
                  ELSE
                    PROC_SERIAL = 0.0
                  END IF
                  DO NAVG = N, N_RETREIVE
! have to determine if reading one timestep too many
                     N_PULL = MIN( NAVG,NSTEPS )
                     N_SAMPLE = N_SAMPLE + 1
                     PULL_FLNAME = M3_FLNAME(STEP_FILE(N_PULL))
                     PULL_DATE   = STEP_DATE(N_PULL)
                     PULL_TIME   = STEP_TIME(N_PULL)
                 
                      IF( .NOT. READ3(PULL_FLNAME,SPECVARS(S),ALLAYS3,PULL_DATE,PULL_TIME,IN_DATA) )THEN
                          MSG = 'Could not read input Models-3 file ' // PULL_FLNAME
                          CALL M3ERR( PNAME, PULL_DATE, PULL_TIME, MSG, .FALSE. )
                      END IF
                 
                      SELECT CASE ( OPERATION_FLAG )
                        CASE( -1 )
                           PROC_SERIAL = PROC_SERIAL + IN_DATA
                        CASE( 0 )
                           PROC_SERIAL = PROC_SERIAL + IN_DATA
                        CASE( 1 )
                           WHERE( PROC_SERIAL .GT. IN_DATA ) PROC_SERIAL = IN_DATA
                        CASE( 2 )
                           WHERE( PROC_SERIAL .LT. IN_DATA ) PROC_SERIAL = IN_DATA
                        CASE( 3 )
                           WHERE( PROC_SERIAL .LT. IN_DATA ) PROC_SERIAL = IN_DATA
                           WHERE( MIN_SERIAL  .GT. IN_DATA ) MIN_SERIAL = IN_DATA
                      END SELECT
                   END DO ! NAVG
                   Write(*,'(''Done reading for species '',I4,'' timestep(s) read.'')') N_SAMPLE
c         compute average if desired, write out the results
                   IF( OPERATION_FLAG .GT. 2 ) THEN
                      PROC_PARALLEL = PROC_PARALLEL - MIN_PARALLEL
                   ELSE IF ( OPERATION_FLAG .LT. 0 ) THEN
                      PROC_PARALLEL = PROC_PARALLEL / REAL(N_SAMPLE,4)
                   END IF
                   write(6,'("writing for JDATE, JTIME = ",2(I8,1X))')PULL_DATE,PULL_TIME
                   
                   IF ( .NOT. WRITE3('OUTFILE', OUT_VARNAME(S), PULL_DATE,PULL_TIME, PROC_SERIAL) ) THEN
                      MSG = ' Cannot write to OUTFILE for species ' // VNAME3D(S)
                      CALL M3EXIT( PNAME, STEP_DATE(N_PULL), STEP_TIME(N_PULL), MSG, 1 )
                   ENDIF
              END DO ! NLOOP
           END DO ! NSPECVAR

         END  SUBROUTINE PROCESS_SEARIAL
      END PROGRAM CALC_TMETRIC
C****************************************************************************
C  routine to convert integer to string
C****************************************************************************
      Character*16 Function int2Str( value, fmt ) result(intStr)

      IMPLICIT NONE

      ! argument
      Integer value
      Character*(*) fmt

      Write(intStr,fmt) value
      Call LeftTrim(intStr)
      return
      End Function int2Str   


C****************************************************************************
C  routine to convert real to string
C****************************************************************************
      Character*16 Function real2Str( value, fmt ) result(realStr)

      IMPLICIT NONE

      ! argument
      Real value
      Character*(*) fmt

      Write(realStr,fmt) value
      Call LeftTrim(realStr)
      return
      End Function real2Str    
 

C****************************************************************************
C  routine to convert date and time to string as "yyyy-mm-dd"
C****************************************************************************
      Character*16 Function date2Str( date ) result(dateStr)
 
      Implicit None

      Integer date
 
C..  local variables
      Integer month, day, year
 
      call DayMon( date, month, day )
      year = date/1000
 
      write(dateStr,'(i4.4,''-'',i2.2,''-'',i2.2)') year, month, day

      return
      End Function date2Str


C****************************************************************************
C  routine to convert time to string as "HH:MM:SS"         
C****************************************************************************
      Character*16 Function time2Str( time ) result(timeStr)
                                                              
      Implicit None

      Integer time                                      
                                                              
C..  local variables                                          
      Integer hour, minutes, secs
                                                              
      hour = time/10000                                       
      minutes = (time - 10000*hour)/100                       
      secs = mod(time,100)                       
                                                              
      write(timeStr,'(i2.2,'':'',i2.2,'':'',i2.2)')   
     &      hour,minutes,secs
      return                                                  
      End Function time2Str                                   
  
