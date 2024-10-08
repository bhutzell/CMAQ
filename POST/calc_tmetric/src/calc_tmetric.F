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
c      PROGRAM timeagg 
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
      character*(16)  PNAME
      character*(16)  VERSION
      real, allocatable :: in_data(:,:,:) 
      real, allocatable :: proc_data(:,:,:) 
      integer lfn
      integer i, s, t, idx, K, N, NAVG
      integer column, row
      real x, y, lon, lat
      
      INTEGER   NLOOPS       
      INTEGER   AVG_TIME
      INTEGER   N_RETREIVE   ! Number of hour to read for files per metric
      INTEGER   N_SAMPLE     ! NUMBER OF HOURS INCLUDED TO THE AVERAGE   
      INTEGER   N_PULL       ! ARRAY index
      INTEGER   OUTPUT_FREQ  ! 
      INTEGER   HOURS, MINUTES, SECONDS, START_SECONDS


C**********************************************************************
      DATA  PNAME       / 'CALC_TMETRIC'  /
      DATA  Version     / '1.0'  /

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

      IF ( ( C_PROC .NE. 'SUM') .AND. ( C_PROC .NE. 'AVG') )THEN
        WRITE(6,'("OPERATIONS set to ",A)')TRIM( C_PROC )
        CALL M3ERR(PNAME, 0, 0, 'Must set OPERATION to SUM or AVG', .TRUE.)      
      ENDIF

C... get species definitions from system variables
      Call loadSpecies()
      write(*,'(i5,'' species defined'')') NSPECVAR



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
      DO S = 1, NSPECVAR
       VNAME3D(S) = TRIM(SPECVARS(S))//"_"//C_PROC      
       VTYPE3D(S) = M3REAL      
       VDESC3D(S) = C_PROC//" OF "//TRIM(SPECVARS(S))
       UNITS3D(S) = VUNITS_IN(INDEX1(SPECVARS(S),NVARS_IN,VNAME_IN))
      ENDDO !S


C... open output file as new 
      if( .NOT. OPEN3( 'OUTFILE', FSCREA3, TRIM(PNAME) ) ) THEN
        WRITE( *, '(''**ERROR** while openning OUTFILE'')' )
        stop 
      endif 


C... start process loop

      ! allocate data array for 1 species
      allocate( in_data(NCOLS3D, NROWS3D, M3GRID % NLAYS), stat=status )
      IF ( status .NE. 0 ) THEN
         MSG = 'Failure allocating in_data'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
      END IF

      allocate( proc_data(NCOLS3D, NROWS3D, M3GRID % NLAYS), stat=status )
      IF ( status .NE. 0 ) THEN
         MSG = 'Failure allocating proc_data'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
      END IF


      DO S = 1, NSPECVAR
       write(*,'(''Processing species:'',a)') TRIM(SPECVARS(s))

!        DO N = 1, NSTEPS !number of time steps across all model files from module_tstep
        DO N = 1, NLOOPS, OUTPUT_FREQ ! loop for each block average
         
          N_RETREIVE = MIN( N+AVG_TIME-1,NSTEPS)

          IF( N+AVG_TIME-1 .GT. NSTEPS ) EXIT ! Do not compute for incomplete sample
          N_SAMPLE  = 0
          PROC_DATA = 0.0
       
          DO NAVG = N, N_RETREIVE             
! have to determine if reading one timestep too many
              N_PULL = MIN( NAVG,NSTEPS ) 
              N_SAMPLE = N_SAMPLE + 1

              IF( .NOT. READ3( M3_FLNAME(STEP_FILE(N_PULL)), SPECVARS(S), 
     &                     ALLAYS3, STEP_DATE(N_PULL), 
     &                     STEP_TIME(N_PULL), IN_DATA ) ) THEN
                MSG = 'Could not read input Models-3 file ' // M3_FLNAME(STEP_FILE(N_PULL))
                CALL M3ERR( PNAME, STEP_DATE(N_PULL), STEP_TIME(N_PULL), MSG, .FALSE. )             
             ELSE             
                PROC_DATA = PROC_DATA + IN_DATA                    
             ENDIF
#ifdef verbose
             WRITE(6,'(3(A),2(I8,1X))')'Reading ',TRIM(SPECVARS(S)),' at ', 
     &       STEP_DATE(N_PULL), STEP_TIME(N_PULL)             
#endif
          END DO ! NAVG

          Write(*,'(''Done reading for species '',a,I4,a)') VNAME3D(S),N_SAMPLE,
     &    ' timestep(s) read.'
c         compute average if desired, write out the result for this variable
          
          IF ( C_PROC .EQ. 'AVG' ) PROC_DATA = PROC_DATA 
     &                                       / REAL(N_SAMPLE,4) 
            
          IF ( .NOT. WRITE3('OUTFILE', VNAME3D(S), STEP_DATE(N_PULL),
     &                      STEP_TIME(N_PULL), PROC_DATA) ) THEN
              MSG = ' Cannot write to OUTFILE for species ' // VNAME3D(S)
              CALL M3EXIT( PNAME, STEP_DATE(N_PULL), STEP_TIME(N_PULL), MSG, 1 ) 
          ENDIF

        enddo !N
      ENDDO !S
               
      Write(*,'(''finished reading model values'')')

      DEALLOCATE(IN_DATA)
      DEALLOCATE(PROC_DATA)

C... shut down ioapi
      KSWIT = SHUT3()
      WRITE(*,'('' IOAPI Shut down'')')
      STOP
      END




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
  
