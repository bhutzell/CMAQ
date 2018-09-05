
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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE CMAQ_DRIVER ( MODEL_STDATE, MODEL_STTIME, MODEL_TSTEP, 
     $                         MODEL_JDATE, MODEL_JTIME, LAST_STEP,
     $                         COUPLE_TSTEP, NCOLS_IN, NLAYS_IN)

C-----------------------------------------------------------------------
C Function:
C    CMAQ CTM driver
 
C Preconditions:
C    Initialized file CONCFILE for output; completed
C    files HISTORY containing initial conditions, SPCCONST for
C    conversion of concentration field from computational units
C    to output units.
 
C Subroutines and functions called:
C    INITSCEN, ADVSTEP, M3EXIT, WRITE3
C    science processes SCIPROC, PA_OUTPUT
 
C Revision History:
C    prototype 6/92 by CJC for proof-of-concept
C    Revised   2/93 by CJC for initial LCM Prototype Alpha
 
C    31 August, 1995 by M. Talat Odman at NCSC: special version for one 
C    single grid
 
C    16 April 1995 by M. Talat Odman at NCSC: write (or rewrite if restart)
C    initial conditions to the output file

C    Jeff
C    18 June 98 - put load of mechanism common here because of ping/ping_noop,
C    chem/chem_noop options

C    2 October, 1998 by Al Bourgeois at LM: parallel implementation
C    Jeff - Dec 00 - move CGRID_MAP into f90 module, re-order cols/rows in
C    call to PAR_INIT
C    Jeff - Jul 01 - enable integral average conc data
C    Sep 01  J. Young        Dyn Alloc - Use HGRD_DEFN

C    3 Sep 01 David wong
C      -- removed M3IO SHUT3 call which is done in PAR_TERM
C      -- removed SET_CTMFILE call

C   23 Jun 03 J.Young: for layer dependent advection tstep
C   18 Aug 03 J. Pleim - move vdiff before advection
C   07 Dec 04 J.Young: for layer dyn alloc - Use VGRD_DEFN
C   30 May 05 J.Young: mass-conserving advection (yamo)
C   20 Jan 06 J.Young: add circular buffer CGRID state file
C   24 May 06 J.Young: par_init/pio_init col/row order check
C    6 Sep 06 J.Young: one-write cgrid file; SGRID in module
C   27 May 09 J.Young: re-do parallel processing initialization
C   21 Jun 10 J.Young: convert for Namelist redesign
C   20 Jul 10 J.Young: re-do serial processing termination (eliminate par_noop)
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C   11 May 11 D.Wong: incorporated twoway model implementation
C   24 Aug 11 D.Wong: eliminated data and geo orientation in se_init call
C    8 Jan 13 C.Nolte: fixed load AGRID bug if TSTEP(1) .ne. 010000 hhmmss
C   24 Sep 13 D.Wong:  Computed AGRID at the output mark of the model
C                      indicated in TSTEP(1) for twoway model
C   21 Apr 14 D.Wong:  Removed M3EXIT call in SHUT3 block
C   07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module
C   10 Aug 15 D.Wong:  Replaced MYPE with IO_PE_INCLUSIVE for parallel
C                      I/O implementation
C   10 Dec 15 D.Wong:  Moved the code which determines which processors are involved
C                      in I/O processing in front of routine PIO_RE_INIT and passed
C                      that information into PIO_RE_INIT
C   26 Jan 16 J.Young: Consolidate PIO_INIT, use keywords for optional arguments
C   28 Jan 16 D.Wong:  Add SAVE attribute to TSTEP for the two-way model implementation
C   16 Sep 16 J.Young: update for inline procan (IRR)
C   29 Nov 17 D. Wong: removed all SWAP routines and replaced with SE_COMM
C   30 Aug 18 D. Wong: setup this file as a subroutine so it works with coupled model 
C                      with MPAS and WRF as well as offlined CMAQ
C-----------------------------------------------------------------------

      USE RXNS_DATA             ! chemical mechanism data
#ifdef mpas
      USE CGRID_SPCS            ! CGRID mechanism species
      USE HGRD_DEFN, ONLY: MYPE
      use coupler_module
      use VGRD_DEFN, ONLY : NLAYS
      use HGRD_DEFN, ONLY : NCOLS, NROWS

      use mydata_module
      use get_env_module
      use lus_defn
      USE AERO_DATA

      use mio_module
#else
      USE PCGRID_DEFN           ! inherits GRID_CONF
      USE CGRID_SPCS, Only: CGRID_SPCS_INIT  ! CGRID mechanism species
      USE STD_CONC              ! standard CONC
      USE AVG_CONC              ! integral average CONC
      USE WVEL_DEFN             ! derived vertical velocity component
      USE PA_DEFN, Only: LIPR, LIRR  ! Process Anaylsis control and data variables
      USE PAGRD_DEFN            ! Process Anaylsis horiz domain specs
      USE UTILIO_DEFN
      USE CENTRAL_IO_MODULE

#ifdef twoway
      USE SD_TIME_SERIES_MODULE
#endif

#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_INIT_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_INIT_MODULE)
#endif
#endif

      IMPLICIT NONE

      INTEGER, INTENT( IN )  :: MODEL_STDATE, MODEL_STTIME, MODEL_TSTEP
      INTEGER, INTENT( OUT ) :: MODEL_JDATE, MODEL_JTIME
      LOGICAL, INTENT( IN )  :: LAST_STEP
      INTEGER, INTENT( IN ), OPTIONAL :: COUPLE_TSTEP
      INTEGER, INTENT( IN ), OPTIONAL :: NCOLS_IN, NLAYS_IN

#ifdef mpas
      real, parameter :: cmin = 1.0E-30

      integer :: ncols_gl
#endif

C Include Files:
      INCLUDE SUBST_FILES_ID    ! I/O definitions and declarations

#ifdef parallel
!     INCLUDE SUBST_MPI         ! MPI definitions and parameters
      INCLUDE 'mpif.h'
#endif

C External Functions:
      INTEGER, EXTERNAL :: SETUP_LOGDEV

C Local variables:

      INTEGER, SAVE :: LOGDEV   ! FORTRAN unit number for log file
      INTEGER      STDATE       ! starting date,    format YYYYDDD
      INTEGER      STTIME       ! starting time,    format HHMMSS
      INTEGER, SAVE :: TSTEP( 3 ) ! time step vector (HHMMSS)
                                  ! TSTEP(1) = local output step
                                  ! TSTEP(2) = sciproc sync. step (chem)
                                  ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                  !            step and wrf/cmaq call frequency

      INTEGER, ALLOCATABLE, SAVE :: ASTEP( : )
      INTEGER, SAVE :: NREPS    ! number of model time steps per output step
      INTEGER          ISTEP    ! current output time step number
      INTEGER          IREP     ! model step number within this output step
      INTEGER, SAVE :: JDATE    ! current model date, coded YYYYDDD
      INTEGER, SAVE :: JTIME    ! current model time, coded HHMMSS
      INTEGER          C, R, L, K, S, V     ! loop induction variables
      INTEGER          ALLOCSTAT

      CHARACTER(  2 ) :: COLROW = 'CR'  ! col/row arg list order
      CHARACTER( 16 ) :: PNAME = 'DRIVER'
      CHARACTER( 96 ) :: XMSG = ' '

      REAL, SAVE, POINTER     :: CGRID( :,:,:,: )
!     REAL, ALLOCATABLE, SAVE :: CGRID( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: AGRID( :,:,:,: )
      REAL    DIVFAC      ! trapezoidal average factor
      INTEGER A_NLYS

      INTEGER NPROCS      ! number of processors
#ifdef parallel
!     INTEGER, PARAMETER :: GEO_ORI = 0   ! stenex array order: 0 (Cartesian), 1 (Matrix)
      INTEGER PAR_ERR     ! Error code from parallel initialization
      REAL    BEGTIME     ! Wall-clock time (sec) at MPI initialization
      REAL    ENDTIME     ! Wall-clock time (sec) at MPI finish
#endif
      INTEGER ELAPTIME    ! ENDTIME-BEGTIME

      LOGICAL, SAVE :: FIRST_RUN = .TRUE.  ! used for twoway model
      LOGICAL       :: WFLG                ! turn on write subdmap in pio_init

#ifdef twoway
      LOGICAL, SAVE :: SD_TIME_SERIES
      LOGICAL       :: CMAQ_WRF_FEEDBACK
#endif
      INTEGER, SAVE :: myNREPS = 0
      INTEGER       :: STATUS

#ifdef mpas
      INTEGER       :: TIME2SEC, SEC2TIME

      integer :: io_mode
      logical :: ncd_64bit_offset

      CHARACTER( 16 ), SAVE :: CTM_PMDIAG    = 'CTM_PMDIAG'
      LOGICAL, SAVE :: PMDIAG

      CHARACTER( 16 ), SAVE :: CTM_DEPV_FILE = 'CTM_DEPV_FILE'
      LOGICAL, SAVE :: DEPV_DIAG

      LOGICAL, SAVE :: PHOTDIAG           ! Flag for PHOTDIAG file
      CHARACTER(  16 ), SAVE :: CTM_PHOTDIAG = 'CTM_PHOTDIAG'
 
      LOGICAL, SAVE :: CLD_DIAG           ! Flag for CLD_DIAG file
 
      INTEGER      SPC_STRT, SPC_FINI, J
      LOGICAL      LSTAT
      INTEGER      STAT                       ! Status reported by Aerosol Dist Checker

      INTEGER      LMODE    !Identifies the problematic mode from
                                !the BC Check routine
      REAL         AER_PAR( 2, N_MODE,5 )  !Modal parameter after the BC 
                                           !check (N, dg, sg)
                                           !      (N, M2, M3) -
                                           !      Before
                                           !      (N, M2, M3) -
                                           !      After
      REAL,ALLOCATABLE :: AECON( : )
      LOGICAL,SAVE :: NEW_START
#endif
 
      INTERFACE
         SUBROUTINE INITSCEN ( CGRID, STDATE, STTIME, TSTEP )
            REAL, POINTER            :: CGRID( :,:,:,: )
!           REAL, INTENT(INOUT)      :: CGRID( :,:,:,: )
            INTEGER, INTENT( OUT )   :: STDATE, STTIME, TSTEP( 3 )
         END SUBROUTINE INITSCEN
#ifndef mpas
         SUBROUTINE ADVSTEP ( JDATE, JTIME, TSTEP, ASTEP, NREPS )
            INTEGER, INTENT( IN )    :: JDATE, JTIME
            INTEGER, INTENT( INOUT ) :: TSTEP( 3 )
            INTEGER, INTENT( OUT )   :: ASTEP( : )
            INTEGER, INTENT( OUT )   :: NREPS
         END SUBROUTINE ADVSTEP
         SUBROUTINE CKSUMMER ( PRNAME, CGRID, JDATE, JTIME )
            CHARACTER( * ), INTENT( IN ) :: PRNAME
            REAL, POINTER            :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )    :: JDATE, JTIME
         END SUBROUTINE CKSUMMER
         SUBROUTINE PA_INIT ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER            :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )    :: JDATE, JTIME, TSTEP( 3 )
         END SUBROUTINE PA_INIT
#endif
         SUBROUTINE SCIPROC ( CGRID, JDATE, JTIME, TSTEP, ASTEP )
            REAL, POINTER            :: CGRID( :,:,:,: )
!           REAL, INTENT(INOUT)      :: CGRID( :,:,:,: )
            INTEGER, INTENT( INOUT ) :: JDATE, JTIME
            INTEGER, INTENT( IN )    :: TSTEP( 3 ), ASTEP( : )
         END SUBROUTINE SCIPROC
#ifndef mpas
         SUBROUTINE WR_ACONC ( AGRID, JDATE, JTIME, TSTEP )
            REAL,    INTENT( IN )    :: AGRID( :,:,:,: )
            INTEGER, INTENT( IN )    :: JDATE, JTIME, TSTEP
         END SUBROUTINE WR_ACONC
         SUBROUTINE WR_CGRID ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER            :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )    :: JDATE, JTIME, TSTEP
         END SUBROUTINE WR_CGRID
         SUBROUTINE PA_OUTPUT ( CGRID, JDATE, JTIME )
            REAL, POINTER            :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )    :: JDATE, JTIME
         END SUBROUTINE PA_OUTPUT
#endif

         SUBROUTINE LOAD_CGRID ( FNAME, STDATE, STTIME, SPC_CAT, CMIN, CGRID )
            CHARACTER( 16 ), INTENT( IN ) :: FNAME
            INTEGER, INTENT( IN )         :: STDATE, STTIME
            CHARACTER(  2 ), INTENT( IN ) :: SPC_CAT
            REAL,    INTENT( IN )         :: CMIN
            REAL, INTENT(INOUT)           :: CGRID( :,:,:,: )
         END SUBROUTINE LOAD_CGRID

         SUBROUTINE UNLOAD_CGRID ( CGRID )
            REAL, INTENT(IN)  :: CGRID( :,:,:,: )
         END SUBROUTINE UNLOAD_CGRID
      END INTERFACE

C-----------------------------------------------------------------------

      IF ( FIRST_RUN ) THEN

         TSTEP = 0
         STDATE     = MODEL_STDATE
         STTIME     = MODEL_STTIME
         TSTEP( 1 ) = MODEL_TSTEP

         IF (PRESENT (COUPLE_TSTEP)) THEN
            TSTEP( 3 ) = SEC2TIME( COUPLE_TSTEP )
         END IF

         IF (PRESENT(NCOLS_IN)) THEN
            NCOLS = NCOLS_IN
            NROWS = 1
            NLAYS = NLAYS_IN
            TSTEP( 2 ) = SEC2TIME( COUPLE_TSTEP )
         END IF

#ifdef parallel
C Start up processor communication
         CALL MPCOMM_INIT( NPROCS, MYPE, BEGTIME, PAR_ERR )
#else
         NPROCS = 1
         MYPE = 0
#endif

         JDATE = 0
         JTIME = 0

#ifdef mpas
         call get_env( cell_num, 'cell_num', 1)

         call mpi_allreduce (ncols, ncols_gl, 1, mpi_int, mpi_sum, mpi_comm_world, status)

         call finit(NPROCS, 1, ncols_gl, nrows)
#endif


C Start I/O-API and set up log file(s)
         LOGDEV = SETUP_LOGDEV()

#ifndef mpas
C Set up horizontal domain, calculate processor-to-subdomain maps
C and define vertical layer structure (in module GRID_CONF)
         IF ( .NOT. GRID_INIT ( NPROCS, MYPE ) ) THEN
            XMSG = '*** Failure defining domain configuration'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

#ifdef verbose_driver
         write( logdev,* ) ' MYPE -> NPROCS:   ', mype, nprocs
         write( logdev,* ) ' MYPE -> NPCOL:    ', mype, npcol
         write( logdev,* ) ' MYPE -> NPROW:    ', mype, nprow
         write( logdev,* ) ' MYPE -> GL_NCOLS: ', mype, gl_ncols
         write( logdev,* ) ' MYPE -> GL_NROWS: ', mype, gl_nrows
         write( logdev,* ) ' MYPE -> NLAYS:    ', mype, nlays
#endif

C Set CGRID mechanism
         IF ( .NOT. CGRID_SPCS_INIT() ) THEN
            XMSG = 'EXIT: Error in CGRID_SPCS:CGRID_SPCS_INIT'
            print *, trim(xmsg); stop
         END IF
#ifdef verbose_driver
         write( logdev,* ) ' MYPE -> NSPCS:    ', mype, nspcsd
#endif

#ifdef parallel_io
           IF ( MOD( MYPE, NPCOL ) .EQ. 0 ) THEN
#else
           IF ( MYPE .EQ. 0 ) THEN
#endif
              IO_PE_INCLUSIVE = .TRUE.
           ELSE
              IO_PE_INCLUSIVE = .FALSE.
           END IF

#ifdef parallel
C Initialize PARIO
         IF ( .NOT. PIO_INIT( COLROW, GL_NCOLS, GL_NROWS, NLAYS, NTHIK,
     &                        NCOLS, NROWS, NPCOL, NPROW, NPROCS, MYPE,
     &                        wflg = WFLG, io_pe_inclusive = IO_PE_INCLUSIVE ) ) THEN
            XMSG = 'Failed to initialize parallel I/O library.'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
#endif

C Initialize stencil exchange
         CALL SE_INIT( NPROCS, NPCOL, NPROW, GL_NCOLS, GL_NROWS, NLAYS,
     &                 NSPCSD, MYPE, MNDIS, MEDIS, MSDIS, MWDIS )
#ifdef verbose_driver
         write( logdev,* ) ' MYPE -> MNDIS:    ', mype, mndis
         write( logdev,* ) ' MYPE -> MEDIS:    ', mype, medis
         write( logdev,* ) ' MYPE -> MSDIS:    ', mype, msdis
         write( logdev,* ) ' MYPE -> MWDIS:    ', mype, mwdis
#endif

C Generate the process analysis data: load PA_DEFN module
         CALL PA_DATAGEN( )

C Set up horizontal domain and calculate processor-to-subdomain maps for
C process analysis, if required
         IF ( LIPR .OR. LIRR ) THEN
            IF ( .NOT. PAGRD_INIT( NPROCS, MYPE ) ) THEN
               XMSG = '*** Failure defining PA domain configuration'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END IF

         IF ( NSPCSD .GT. MXVARS3 ) THEN
            WRITE( XMSG,'(5X, A, I5, A)' ) 'The number of variables,', NSPCSD,
     &      ' to be written to the State CGRID File'
            WRITE( LOGDEV, '( A )' ) XMSG
            WRITE( XMSG,'(5X, A, I5)' ) 'exceeds the I/O-API limit:', MXVARS3
            WRITE( LOGDEV, '( A )' ) XMSG
            XMSG = 'Recompile with an I/O-API lib having a larger MXVARS3'
            WRITE( LOGDEV, '( 5X, A )' ) XMSG
            CALL M3EXIT( PNAME, JDATE, JTIME, ' ', XSTAT1 )
         END IF

C Initialize PCGRID
         IF ( .NOT. PCGRID_INIT () ) THEN
            XMSG = 'Failure defining horizontal domain'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2  )
         END IF
      
         CGRID => PCGRID( 1:NCOLS,1:NROWS,:,: )   ! required for PinG
#else
         ALLOCATE (CGRID ( NCOLS,NROWS,NLAYS,NSPCSD ), STAT = ALLOCSTAT )
#endif

#ifndef mpas
C Initalize CONC definitions (in STD_CONC F90 module)
         CALL CONC_DEFN ()

C Get avg CONC definitions, species and layer pointers (in AVG_CONC F90 module)
         CALL A_CONC_DEFN ()
         A_NLYS = ACONC_ELEV - ACONC_BLEV + 1

C Initialize optional derived vertical velocity writes to conc file
         IF ( .NOT. WVEL_INIT () ) THEN
            XMSG = 'Failure initializing derived vertical velocity writes'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2  )
         END IF
#endif

C Initialize conc field: Copy IC's to CONC file as step 0
C Convention: the input file concentration units are always ppmV.

         CALL INITSCEN ( CGRID, STDATE, STTIME, TSTEP )
         JDATE = STDATE; JTIME = STTIME

#ifndef mpas
         CALL CKSUMMER ( 'INITSCEN', CGRID, JDATE, JTIME )

         IF ( LIPR .OR. LIRR ) CALL PA_INIT ( CGRID, JDATE, JTIME, TSTEP )

C Verify input file header consistency and run duration
         CALL FLCHECK ( JDATE, JTIME, TSTEP( 1 ) )

         ALLOCATE ( AGRID( NCOLS,NROWS,A_NLYS,N_ASPCS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'AGRID memory allocation failed'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

!        AGRID = 0.0

         ALLOCATE ( ASTEP( NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'EXIT: ASTEP memory allocation failed'
            print *, trim(XMSG); stop
         END IF

         FIRST_RUN = .FALSE.

#ifdef twoway
         CMAQ_WRF_FEEDBACK = ENVYN( 'CMAQ_WRF_FEEDBACK', ' ', .FALSE., STATUS )
         IF ( STATUS .EQ. 1 ) THEN
            PRINT *, 'Environment variable improperly formatted'
            STOP
         ELSE IF ( STATUS .EQ. -1 ) THEN
            PRINT *, 'Environment variable set, but empty ... Using default:'
         ELSE IF ( STATUS .EQ. -2 ) THEN
            PRINT *, 'Environment variable not set ... Using default:'
         END IF

         IF ( CMAQ_WRF_FEEDBACK ) THEN
            CALL FEEDBACK_SETUP ( JDATE, JTIME, TSTEP( 3 ) )
         END IF

         SD_TIME_SERIES = ENVYN( "SD_TIME_SERIES", ' ', .FALSE., STATUS )
         IF ( SD_TIME_SERIES ) THEN
            CALL SD_TIME_SERIES_INIT ( LOGDEV, TSTEP( 3 ) )
            CALL OUTPUT_SD_TIME_SERIES ( CGRID, JDATE, JTIME )
         END IF
#endif

#else
        if (.not. lus_init (mminlu_mpas, lufrac_data(:,:,1)) ) then
           print *, ' Error: Cannot initialize Land Use category'
           stop
        end if

        call get_env (ncd_64bit_offset, 'ncd_64bit_offset', .false.)

        if (ncd_64bit_offset) then
            io_mode = ior (nf90_noclobber, nf90_64bit_offset)
         else
            io_mode = nf90_noclobber
         end if

        call setfile (EMIS_1)

        call get_env (pmdiag, ctm_pmdiag, .false.)
        if (pmdiag) then
           call fcreate (CTM_PMDIAG_1, io_mode)
        end if

        call fcreate (CTM_DRY_DEP_1, io_mode)
        call fcreate (CTM_WET_DEP_1, io_mode)

        call get_env (depv_diag, ctm_depv_file, .false.)
        if ( depv_diag ) then
            call fcreate (CTM_DEPV_DIAG, io_mode)
        end if

        call get_env (cld_diag, 'CLD_DIAG', .false.)
        if ( cld_diag ) then
            call fcreate (CTM_WET_DEP_2, io_mode)
        end if

        call get_env (photdiag, ctm_photdiag, .false.)
        if (photdiag) then
           call fcreate (CTM_RJ_1, io_mode)
           call fcreate (CTM_RJ_2, io_mode)
        end if

#endif

      END IF ! first_run

      MODEL_JDATE = JDATE
      MODEL_JTIME = JTIME

#ifdef mpas

      mpas_cmaq_last_step = LAST_STEP

      v = size(cmaq_species, 4)

      DO L = 1, NLAYS
         DO C = 1, NCOLS
            CGRID( C,1,L,1:V ) = cmaq_species( c,1,l,1:v )
         END DO
      END DO

      ! If this run is not a restart, then check the initial conditions
      ! to make sure they are physically realistic. Check Aerosol Size 
      ! Distributions and Warn the User if They Are Not Robust.
      call get_env (NEW_START, 'NEW_START', .false.)
      IF ( NEW_START ) THEN
         ALLOCATE( AECON( N_AE_SPC ) )
         SPC_STRT = AE_STRT
         SPC_FINI = AE_STRT + N_AE_SPC - 1
         LSTAT    = .FALSE.
         DO L = 1, NLAYS
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  AECON( 1:N_AE_SPC ) = CGRID( C,R,L,SPC_STRT:SPC_FINI )
                  CALL CHECK_AERO_ICBC( AECON, .TRUE., STAT, AER_PAR, LMODE )
                  CGRID( C,R,L,SPC_STRT:SPC_FINI ) = AECON( 1:N_AE_SPC )
                  IF ( STAT .GT. 0 ) THEN
                     LSTAT = .TRUE.
                  ENDIF
               END DO
            END DO
         END DO
    
         !Print warning if any aerosol ICs violated the size
         !distribution parameters
         IF ( LSTAT ) THEN
            WRITE( XMSG, '(A,A)' ),
     &         'Applying fix to aerosol Initial Conditions for aerosol',
     &         ' modes.'
!           CALL M3WARN( 'Read ICs', JDATE, JTIME, XMSG )
            print *, trim(xmsg)
         END IF
      END IF
 
      NREPS = 1
#else
C Get synchronization and advection time steps, TSTEP(2), ASTEP(L) and NREPS
      CALL ADVSTEP ( JDATE, JTIME, TSTEP, ASTEP, NREPS )

      IF ( MOD( TIME2SEC( JTIME ), TIME2SEC( TSTEP( 1 ) ) ) .EQ. 0 ) THEN
         DO V = 1, N_ASPCS
            S = AVG_CONC_MAP( V )
            L = 0
            DO K = ACONC_BLEV, ACONC_ELEV
               L = L + 1
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     AGRID( C,R,L,V ) = CGRID( C,R,K,S )
                  END DO
               END DO
            END DO
         END DO
      END IF
#endif

C science process sequence:

      myNREPS = myNREPS + NREPS

      DO IREP = 1, NREPS

         CALL SCIPROC ( CGRID, JDATE, JTIME, TSTEP, ASTEP )

#ifndef mpas
         DO V = 1, N_ASPCS
            S = AVG_CONC_MAP( V )
            L = 0
            DO K = ACONC_BLEV, ACONC_ELEV
               L = L + 1
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     AGRID( C,R,L,V ) = AGRID( C,R,L,V )
     &                                + 2.0 * CGRID( C,R,K,S )
                  END DO
               END DO
            END DO
         END DO
#endif

      END DO

#ifndef mpas
      IF ( MOD( TIME2SEC( JTIME ), TIME2SEC( TSTEP( 1 ) ) ) .EQ. 0 ) THEN
         DIVFAC = 0.5 / FLOAT( myNREPS )
         myNREPS = 0
         DO V = 1, N_ASPCS
            S = AVG_CONC_MAP( V )
            L = 0
            DO K = ACONC_BLEV, ACONC_ELEV
               L = L + 1
               DO R = 1, NROWS
                  DO C = 1, NCOLS
                     AGRID( C,R,L,V ) = DIVFAC * ( AGRID( C,R,L,V )
     &                                -            CGRID( C,R,K,S ) )
                  END DO
               END DO
            END DO
         END DO
      END IF

      DO V = 1, N_CSPCS
         S = CONC_MAP( V )
         L = 0
         DO K = CONC_BLEV, CONC_ELEV
            L = L + 1
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  SGRID( C,R,L,V ) = CGRID( C,R,K,S )
               END DO
            END DO
         END DO
      END DO

C write conc fields

      IF ( MOD( TIME2SEC( JTIME ), TIME2SEC( TSTEP( 1 ) ) ) .EQ. 0 ) THEN
         CALL WR_CONC ( JDATE, JTIME, TSTEP( 1 ) )
         CALL WR_ACONC ( AGRID, JDATE, JTIME, TSTEP( 1 ) )
         IF ( LIPR .OR. LIRR ) CALL PA_OUTPUT ( CGRID, JDATE, JTIME )
      END IF

#ifdef twoway
      IF ( SD_TIME_SERIES ) THEN
         CALL OUTPUT_SD_TIME_SERIES ( CGRID, JDATE, JTIME )
      END IF
#endif

#else
      CALL UNLOAD_CGRID (CGRID)
#endif

      IF ( LAST_STEP ) THEN
#ifdef mpas
         call shutdown()
#else
C write CGRID state file for subsequent runs
         CALL WR_CGRID ( CGRID, JDATE, JTIME, TSTEP( 1 ) )

C Shut down IOAPI
         IF ( SHUT3() ) THEN
            XMSG = '>>---->  Program completed successfully  <----<<'
            WRITE ( LOGDEV,'( //5X, A, / )' ) XMSG
            WRITE( LOGDEV,2015 ) DT2STR( JDATE, JTIME ), JDATE, JTIME
         ELSE
            XMSG = ' *** FATAL ERROR shutting down Models-3 I/O *** '
            WRITE ( LOGDEV,'( //5X, A, / )' ) XMSG
         END IF

#ifdef parallel
C Shut down MPI
         ENDTIME = REAL(MPI_WTIME())   ! get final wall-clock time
         ELAPTIME = NINT ( ENDTIME - BEGTIME )
         IF ( MYPE .EQ. 0 ) WRITE( *,2021 ) ELAPTIME
         CALL MPI_FINALIZE ( PAR_ERR )
         IF ( PAR_ERR .NE. 0 ) THEN
            STOP 'Error terminating MPI'
         END IF
#endif
#endif
      END IF

2015  FORMAT( 5X , 'Date and time ', A, ' (', I7, ':', I6.6, ')' )
2021  FORMAT( //5X, 'The elapsed time for this job was', I7.6, ' seconds.' / )

      END SUBROUTINE CMAQ_DRIVER
