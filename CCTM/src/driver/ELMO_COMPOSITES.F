
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

      MODULE ELMO_COMPOSITES

      USE ELMO_DATA
      USE RUNTIME_VARS, ONLY : LOGDEV, CMAQ_CHEM_CTRL, 
     &                         LOG_MESSAGE, LOG_SUBHEADING

      IMPLICIT NONE
 
      INTEGER, SAVE ::  N_COMPOSITES
      INTEGER, SAVE ::  N_MAX_COMPONENTS
      INTEGER, PARAMETER ::  N_MAX_FORMULA = 1024

      TYPE COMPOSITE_NML
          CHARACTER(LEN = 16) :: NAME
          CHARACTER(LEN = 100) :: DESC
          CHARACTER(LEN = 16) :: CUNIT
          CHARACTER(LEN = 16) :: CMPST_TYPE
          CHARACTER(LEN = N_MAX_FORMULA) :: FORMULA
      END TYPE COMPOSITE_NML

      TYPE( COMPOSITE_NML ), ALLOCATABLE, SAVE :: CMPST( : )
 
      CONTAINS

!-------------------------------------------------------------------------
      SUBROUTINE ELMO_READ_COMPOSITES()
!     This subroutine reads the mechanism-specific namelist file and
!     processes the composite variables defined there. The information
!     provided by the namelist file includes the name of the variable,
!     its units, a variable type (CONC or DEP), a long description, and
!     an formula for the variable based on other available variables.
!     These data are checked for basic errors and compiled into a list
!     so that they may be added to the ELMO_TABLE.
!-------------------------------------------------------------------------

      USE AERO_DATA, ONLY : AEROSPC, N_AEROSPC, N_MODE, AERO_MISSING
      USE CGRID_SPCS, ONLY : WDEP_SPC, N_SPC_WDEP, CGRID_NAME,
     &                       N_CGRID_SPC, N_SPC_DDEP
      USE VDIFF_MAP, ONLY : DDEP_SPC

      IMPLICIT NONE

      CHARACTER( 300 ) :: XMSG

      INTEGER FUNIT
      INTEGER IOST, IW, ID
      CHARACTER( 200 ) :: TMPLINE1, TMPLINE2, TMPLINE3
      CHARACTER( 40 )  :: PNAME = 'ELMO_READ_COMPOSITES'
      INTEGER IAERO, IAER, ICOMP, IM, INDX
      INTEGER N_MAX_COMPOSITES

      ! Define Namelist Input from Mechanism-Specific Control File
      Namelist / COMPOSITE_INIT / N_MAX_COMPOSITES, N_MAX_COMPONENTS
      Namelist / ELMO_COMPOSITES / CMPST

      CALL LOG_MESSAGE( LOGDEV, ' ' )
      CALL LOG_SUBHEADING( LOGDEV, 'Reading ELMO Composites '//
     &                             'from Mechanism-Specific Namelist')

      ! Retrieve the Name of the Mechanism-Specific Control File
      IF ( CMAQ_CHEM_CTRL .EQ. "CMAQ_CH_CTRL_NML" ) THEN
          XMSG = 'You have chosen not to indicate the location of a ' //
     &           'mechanism-specific control namelist file for ELMO '//
     &           'and DESID. You must give a value for the CMAQ_CH_CTRL_NML '//
     &           'variable in the CMAQ runscript.'
          CALL M3EXIT( PNAME,0,0,XMSG,1 )
      END IF

      ! Open Mechanism-Specific Control File
      FUNIT = JUNIT()
      OPEN( FILE = CMAQ_CHEM_CTRL, UNIT = FUNIT, STATUS = 'OLD',
     &      POSITION = 'REWIND', FORM='FORMATTED', IOSTAT = IOST )

      ! Check for Error in File Open Process
      IF ( IOST .NE. 0 ) THEN
          WRITE( XMSG, '(A,A,A)' ),'ERROR: Could not read ',
     &           'CMAQ CHemical Control namelist file: ',TRIM( CMAQ_CHEM_CTRL )
          CALL M3EXIT( PNAME,0,0,XMSG,1 )
      END IF

      ! Read the number of ELMO Composite Variables
      N_MAX_COMPOSITES = 0
      N_MAX_COMPONENTS = 1
      REWIND( FUNIT )
      READ( NML = COMPOSITE_INIT, UNIT = FUNIT, IOSTAT=IOST )
      IF ( IOST .EQ. -1 ) THEN
          ! The Number of Composites was completely missing
          XMSG = 'WARNING: Number of ELMO Composites was not specified. '//
     &           'If you intended to specify composite variables for ELMO, '//
     &           'check the CMAQ Chemical Control Namelist file.'
          CALL LOG_MESSAGE( LOGDEV, ' ')
          CALL LOG_MESSAGE( LOGDEV, XMSG )

      ELSE IF ( IOST .NE. 0 ) THEN
          ! Read Error detected for COMPOSITES_INIT
          backspace( FUNIT )
          read( FUNIT, fmt='(A)' ) tmpline1
          XMSG = 'ERROR: There was a syntax error reading the number of '//
     &           'composite variables for use by the ELMO module. Please '//
     &           'check the format of each line for syntax errors. The '//
     &           'invalid line was likely: '
          CALL LOG_MESSAGE( LOGDEV, ' ')
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          WRITE( LOGDEV, '(8x,A)' ) TMPLINE1
          CALL M3EXIT ( PNAME, 0, 0, 'CMAQ must Crash until you '//
     &                  'fix the specification.', 1 )
      END IF

      ! Allocate and Initialize Namelist Variables
      ALLOCATE( CMPST( N_MAX_COMPOSITES + N_AEROSPC*3 ),
     &          STAT=IOST )
      CALL CHECKMEM( IOST, 'CMPST',PNAME)

      ! Read the Composite Definition Section
      CMPST%NAME       = ''
      CMPST%CUNIT      = ''
      CMPST%CMPST_TYPE = ''
      CMPST%DESC       = ''
      CMPST%FORMULA    = ''

      REWIND( FUNIT )
      READ( NML = ELMO_COMPOSITES, UNIT = FUNIT, IOSTAT=IOST )
      IF ( IOST .EQ. -1 ) THEN
          ! The ELMO Composites Section was completely missing
          XMSG = 'WARNING: There were no valid composite variables specified '//
     &           'for use by the ELMO module. If you intended to specify '//
     &           'diagnostic output in the emission control interface, check the '//
     &           'file you have provided for CMAQ_CH_CTRL_NML.'
          CALL LOG_MESSAGE( LOGDEV, ' ')
          CALL LOG_MESSAGE( LOGDEV, XMSG )
      ELSE IF ( IOST .NE. 0 ) THEN
          ! Read Error for Composite Variables
          backspace( FUNIT )
          backspace( FUNIT )
          backspace( FUNIT )
          read( FUNIT, fmt='(A)' ) tmpline1
          read( FUNIT, fmt='(A)' ) tmpline2
          read( FUNIT, fmt='(A)' ) tmpline3
          XMSG = 'ERROR: There was a syntax error reading Composite '//
     &           'Variables for output by the ELMO module. Please check the format of '//
     &           'each line for syntax errors. The invalid line was likely the last '//
     &           'of this selection: '
          CALL LOG_MESSAGE( LOGDEV, ' ')
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          WRITE( LOGDEV, '(8x,A)' ) TMPLINE1
          WRITE( LOGDEV, '(8x,A)' ) TMPLINE2
          WRITE( LOGDEV, '(8x,A)' ) TMPLINE3
          CALL M3EXIT ( 'DESID_READ_NAMELIST', 0, 0, 'CMAQ must Crash until you '//
     &                  'fix the Composite Variable Specification.', 1 )
      END IF

      ! Check that all user-defined Composites are populated
      N_COMPOSITES = 0
      DO ICOMP = 1,N_MAX_COMPOSITES
         IF ( CMPST(ICOMP)%NAME .NE. '' ) THEN
            N_COMPOSITES = N_COMPOSITES + 1
         END IF
      END DO

      CALL LOG_MESSAGE( LOGDEV, 'Completed Reading ELMO Composites '//
     &                             'from Mechanism-Spcific Namelist')

      ! Check that No Composite names are equal to any CGRID name
      DO ICOMP = 1,N_COMPOSITES
         INDX = INDEX1( CMPST(ICOMP)%NAME, N_CGRID_SPC, CGRID_NAME )
         IF ( INDX .NE. 0 ) THEN
            XMSG = 'The name of a Composite: ' // TRIM( CMPST(ICOMP)%NAME ) //
     &             ' is the same as a gas or particle species in CGRID. This is ' //
     &             'not allowed. Either delete this Composite or rename it.'
            CALL M3EXIT ( 'DESID_READ_NAMELIST', 0, 0, XMSG, 1 ) 
         END IF
      END DO

      ! Add Bulk Aerosols to Composite List
      DO IAERO = 1,N_AEROSPC
      
         ! Add this Aerosol Concentration to the Composite List
         IAER = INDEX1( AEROSPC(IAERO)%BULKNAME, N_COMPOSITES, CMPST%NAME )
         IF ( IAER .EQ. 0 ) THEN
            N_COMPOSITES = N_COMPOSITES + 1
            CMPST( N_COMPOSITES )%NAME = AEROSPC(IAERO)%BULKNAME
            CMPST( N_COMPOSITES )%CUNIT = 'ug m-3'
            CMPST( N_COMPOSITES )%CMPST_TYPE = 'TSP'
            CMPST( N_COMPOSITES )%DESC = 'Total ' // TRIM(AEROSPC(IAERO)%BULKNAME) //
     &           ' across all modes.'
            DO IM = 1,N_MODE
              IF ( .NOT. AERO_MISSING( IAERO, IM ) .AND. 
     &             AEROSPC( IAERO )%NAME(IM) .NE. AEROSPC(IAERO)%BULKNAME ) THEN
                CMPST( N_COMPOSITES )%FORMULA = 
     &                 TRIM( CMPST( N_COMPOSITES )%FORMULA ) //
     &                 ' + ' // TRIM(AEROSPC( IAERO )%NAME( IM ))
              END IF
            END DO
            IF ( CMPST( N_COMPOSITES )%FORMULA .EQ. '' ) N_COMPOSITES = N_COMPOSITES - 1
         END IF
   
         ! Add this Aerosol Wet Dep to the Composite List
         IAER = INDEX1( 'WD_'//AEROSPC(IAERO)%BULKNAME, N_COMPOSITES, CMPST%NAME )
         IF ( IAER .EQ. 0 ) THEN
            N_COMPOSITES = N_COMPOSITES + 1
            CMPST( N_COMPOSITES )%NAME = 'WD_' // AEROSPC(IAERO)%BULKNAME
            CMPST( N_COMPOSITES )%CUNIT = 'kg ha-1'
            CMPST( N_COMPOSITES )%CMPST_TYPE = 'DEP'
            CMPST( N_COMPOSITES )%DESC = 'Total ' // TRIM(AEROSPC(IAERO)%BULKNAME) //
     &           ' wet deposition across all modes.'
            DO IM = 1,N_MODE
              IF ( .NOT. AERO_MISSING( IAERO, IM ) .AND.
     &             AEROSPC( IAERO )%NAME(IM) .NE. AEROSPC(IAERO)%BULKNAME ) THEN
                IW = INDEX1( TRIM(AEROSPC( IAERO )%NAME( IM )), N_SPC_WDEP, WDEP_SPC )
                IF ( IW .NE. 0 ) 
     &               CMPST( N_COMPOSITES )%FORMULA = 
     &                    TRIM( CMPST( N_COMPOSITES )%FORMULA ) //
     &                    ' + ' // 'WD_' // TRIM(AEROSPC( IAERO )%NAME( IM ))
              END IF
            END DO
            IF ( CMPST( N_COMPOSITES )%FORMULA .EQ. '' ) N_COMPOSITES = N_COMPOSITES - 1
         END IF
         
         ! Add this Aerosol Dry Deposition to the Composite List
         IAER = INDEX1( 'DD_'//AEROSPC(IAERO)%BULKNAME, N_COMPOSITES, CMPST%NAME )
         IF ( IAER .EQ. 0 ) THEN
            N_COMPOSITES = N_COMPOSITES + 1
            CMPST( N_COMPOSITES )%NAME = 'DD_' // AEROSPC(IAERO)%BULKNAME
            CMPST( N_COMPOSITES )%CUNIT = 'kg ha-1'
            CMPST( N_COMPOSITES )%CMPST_TYPE = 'DEP'
            CMPST( N_COMPOSITES )%DESC = 'Total ' // TRIM(AEROSPC(IAERO)%BULKNAME) //
     &           ' dry deposition across all modes.'
            DO IM = 1,N_MODE
              IF ( .NOT. AERO_MISSING( IAERO, IM ) .AND.
     &             AEROSPC( IAERO )%NAME(IM) .NE. AEROSPC(IAERO)%BULKNAME ) THEN
                ID = INDEX1( TRIM(AEROSPC( IAERO )%NAME( IM )), N_SPC_DDEP+1, DDEP_SPC )
                IF ( ID .NE. 0 ) 
     &               CMPST( N_COMPOSITES )%FORMULA = 
     &                    TRIM( CMPST( N_COMPOSITES )%FORMULA ) //
     &                    ' + ' // 'DD_' // TRIM(AEROSPC( IAERO )%NAME( IM ))
              END IF
            END DO
            IF ( CMPST( N_COMPOSITES )%FORMULA .EQ. '' ) N_COMPOSITES = N_COMPOSITES - 1
         END IF
      END DO

      CLOSE( UNIT = FUNIT )

      ! Reduce the size of CMPST from N_MAX_COMPOSITES to N_COMPOSITES
      CMPST = CMPST( 1:N_COMPOSITES )

      END SUBROUTINE ELMO_READ_COMPOSITES

!-------------------------------------------------------------------------
      SUBROUTINE ELMO_MAP_COMPOSITES()
!     This subroutine maps the ELMO variables that the user has
!       requested to the entries in the ELMO_DATA table.
!-------------------------------------------------------------------------
      
      IMPLICIT NONE

      INTEGER N_VARS, J, ID, IVAR
      CHARACTER( 32 ) :: COMPS ( N_MAX_COMPONENTS )
      REAL            :: COEFFS( N_MAX_COMPONENTS )

      ! Loop through all available ELMO_TABLE Variables
      DO J = 1,N_ELMO_TABLE
          ID = ELMO_TABLE( J )%ID

          IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ET_CMPST .OR.
     &         ELMO_TABLE( J )%VAR_TYPE .EQ. ET_ISAM_CMPST .OR.
     &         ELMO_TABLE( J )%VAR_TYPE .EQ. ET_DDM_CMPST .OR.
     &         ELMO_TABLE( J )%VAR_TYPE .EQ. ET_CMPST_DEP .OR.
     &         ELMO_TABLE( J )%VAR_TYPE .EQ. ET_ISAM_CMPST_DEP .OR.
     &         ELMO_TABLE( J )%VAR_TYPE .EQ. ET_DDM_CMPST_DEP ) THEN
            
            CALL LOG_MESSAGE( LOGDEV, '   Reading Formula for Composite: '//
     &           TRIM( ELMO_TABLE( J )%OUT_NAME ) )


            ! Parse Formula from CMAQ Chemical Control Namelist
            CALL ELMO_PARSE_FORMULA( ID, N_VARS, COMPS, COEFFS )

            ! Account for Bulk Aerosol Names being used. Expand them if
            ! they are detected.
            CALL ELMO_EXPAND_AERO( N_VARS, COMPS, COEFFS )

            ! Map Composite
            CALL ELMO_INIT_COMPOSITE( J, N_VARS, CMPST( ID )%CMPST_TYPE )
            DO IVAR = 1,N_VARS
              CALL ELMO_SET_COMPONENTS( J, N_VARS, IVAR, COMPS( IVAR ), COEFFS( IVAR ) )
            END DO

         END IF
      END DO

      END SUBROUTINE ELMO_MAP_COMPOSITES

!-------------------------------------------------------------------------
      SUBROUTINE ELMO_INIT_COMPOSITE( J, N_VARS, AEROTYPE_TMP )
!     This subroutine allocates and initalizes all of the elements of an 
!        index IDG within the ELMO_COEFFS structure
!
!     IDG - the index of this ELMO variable in the space defined
!           by the integer index variables like ID_PMF_POC, etc.
!     N_VARS - Number of variables used to create this aggregate variable. 
!           Does not include the number of modes. For example, ASO4I+ASO4J 
!           counts as 1 variable (ASO4). It will be expanded to modes later.
!     AEROTYPE - Aerosol size range or mode to be calculated. This defines 
!           the modes considered and the inlet type if any. Options
!           include:
!             ALL, NONE, ULTRAFINE, FINE, COARSE, NUCLEATION, AITKEN,
!             ACCUMULATION, PM01, PM1, PM25, PM10, PM25to10 and PMAMS
!     
!-------------------------------------------------------------------------

      USE AERO_DATA, ONLY : N_MODE, AEROMODE
#ifdef isam
      USE SA_DEFN, ONLY : NTAG_SA
#elif sens
      USE DDM3D_DEFN, ONLY : NPMAX
#endif

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: J 
      INTEGER, INTENT( IN ) :: N_VARS
      CHARACTER( * ), INTENT( IN ) :: AEROTYPE_TMP
      CHARACTER( 20 ) AEROTYPE
      INTEGER VAR_TYPE, ISRC

      CHARACTER( 200 ) XMSG
      CHARACTER(16), SAVE :: PNAME = 'INIT_ELMO_COEF'
      INTEGER ALLOCSTAT

      ! Initialize All Components of ELMO_COEFFS
      ELMO_TABLE( J )%L_COMP = .TRUE.
      ELMO_COMP( J )%N_VARS = N_VARS
      ALLOCATE( ELMO_COMP( J )%L_MODE  ( N_MODE ),
#ifdef isam    
     &          ELMO_COMP( J )%L_SOURCE( NTAG_SA+1 ),
#elif sens
     &          ELMO_COMP( J )%L_SOURCE( NPMAX ),
#else
     &          ELMO_COMP( J )%L_SOURCE( 1 ),
#endif
     &          ELMO_COMP( J )%MW      ( N_VARS ),
     &          ELMO_COMP( J )%SPC_UNIT( N_VARS ),
     &          ELMO_COMP( J )%ID      ( N_VARS ),
     &          ELMO_COMP( J )%TABID   ( N_VARS ),
     &          ELMO_COMP( J )%VAR_TYPE( N_VARS ),
     &          ELMO_COMP( J )%COEFF   ( N_VARS ),
     &          STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_COMP' 
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_COMP( J )%L_MODE  ( : )  = .FALSE.
      ELMO_COMP( J )%L_SOURCE( : )  = .FALSE.
      ELMO_COMP( J )%MW      ( : )  = 0.
      ELMO_COMP( J )%SPC_UNIT( : )  = 0
      ELMO_COMP( J )%ID      ( : )  = 0
      ELMO_COMP( J )%TABID   ( : )  = 0
      ELMO_COMP( J )%VAR_TYPE( : )  = 0
      ELMO_COMP( J )%COEFF   ( : )  = 0.
      ELMO_COMP( J )%INLET          = 0
      ELMO_COMP( J )%WETNESS        = IWET

      ! Specify the Aerosol Modes Needed and Inlet Type based on User
      ! Input through the AEROTYPE variable.
      AEROTYPE = AEROTYPE_TMP
      CALL UPCASE( AEROTYPE )
      SELECT CASE ( AEROTYPE )
         CASE ('ULTRAFINE' )
             ELMO_COMP( J )%L_MODE( : ) = AEROMODE(:)%ULTRAFINE_MASK
         CASE ('FINE' )
             ELMO_COMP( J )%L_MODE( : ) = AEROMODE(:)%FINE_MASK
         CASE ('COARSE' )
             ELMO_COMP( J )%L_MODE( : ) = AEROMODE(:)%COARSE_MASK
         CASE ('NUCLEATION' )
             ELMO_COMP( J )%L_MODE( : ) = AEROMODE(:)%NUC_MASK
         CASE ('AITKEN' )
             ELMO_COMP( J )%L_MODE( : ) = AEROMODE(:)%AITKEN_MASK
         CASE ('ACCUMULATION' )
             ELMO_COMP( J )%L_MODE( : ) = AEROMODE(:)%ACCUM_MASK
         CASE ('PM01' )
             ELMO_COMP( J )%L_MODE( : ) = .TRUE.
             ELMO_COMP( J )%INLET = IPM01
             ELMO_COMP( J )%WETNESS = IWET
         CASE ('PM1' )
             ELMO_COMP( J )%L_MODE( : ) = .TRUE.
             ELMO_COMP( J )%INLET = IPM1
             ELMO_COMP( J )%WETNESS = IWET
         CASE ('PM25' )
             ELMO_COMP( J )%L_MODE( : ) = .TRUE.
             ELMO_COMP( J )%INLET = IPM25
             ELMO_COMP( J )%WETNESS = IWET
         CASE ('PM10' )
             ELMO_COMP( J )%L_MODE( : ) = .TRUE.
             ELMO_COMP( J )%INLET = IPM10
             ELMO_COMP( J )%WETNESS = IWET
         CASE ('PM25TO10' )
             ELMO_COMP( J )%L_MODE( : ) = .TRUE.
             ELMO_COMP( J )%INLET = IPM25to10
             ELMO_COMP( J )%WETNESS = IWET
         CASE ('PMAMS' )
             ELMO_COMP( J )%L_MODE( : ) = .TRUE.
             ELMO_COMP( J )%INLET = IPMAMS
             ELMO_COMP( J )%WETNESS = IWET
         CASE ('ALL' )
             ! This is a bulk quantity. All modes are selected.
             ELMO_COMP( J )%L_MODE( : ) = .TRUE.
         CASE ('TSP' )
             ! This is a bulk quantity. All modes are selected.
             ELMO_COMP( J )%L_MODE( : ) = .TRUE.
         CASE ('GAS')
             ! No Aerosol Parameters need to be set since
             ! this aggregate will be all gas.
         CASE ('DEP')
             ! Deposition will consist of particles from 
             ! the entire size range
             ELMO_COMP( J )%L_MODE( : ) = .TRUE.
         CASE ('DEP_FINE')
             ! Deposition will consist of particles from 
             ! the fine mode range
             ELMO_COMP( J )%L_MODE( : ) = AEROMODE(:)%FINE_MASK
         CASE ('DEP_COARSE')
             ! Deposition will consist of particles from 
             ! the coarse mode range
             ELMO_COMP( J )%L_MODE( : ) = AEROMODE(:)%COARSE_MASK
         CASE ('DEP_AITKEN')
             ! Deposition will consist of particles from 
             ! the Aitken mode range
             ELMO_COMP( J )%L_MODE( : ) = AEROMODE(:)%AITKEN_MASK
         CASE ('DEP_ACCUMULATION')
             ! Deposition will consist of particles from 
             ! the accumulation mode range
             ELMO_COMP( J )%L_MODE( : ) = AEROMODE(:)%ACCUM_MASK
         CASE DEFAULT
             XMSG = 'An aerosol descriptor (' // trim(AEROTYPE) // ') has been given ' //
     &              'for the mapping of an ELMO composite variable. It is not recognized.'
             CALL M3EXIT( 'ELMO_INIT_COMPOSITE', 0, 0, XMSG, XSTAT1 )
      END SELECT

      ! Determine the Variable Type
      VAR_TYPE = ELMO_TABLE( J )%VAR_TYPE
      
      ! If this variable is an ISAM or DDM variable, inherit the Source 
      ! for this Component from the Composite
      IF ( VAR_TYPE .EQ. ET_ISAM_CMPST    .OR. VAR_TYPE .EQ. ET_ISAM_CONC 
     & .OR. VAR_TYPE .EQ. ET_ISAM_CMPST_DEP
     & .OR.VAR_TYPE .EQ. ET_ISAM_WDEP     .OR. VAR_TYPE .EQ. ET_ISAM_DDEP
     & .OR.VAR_TYPE .EQ. ET_DDM_CMPST     .OR. VAR_TYPE .EQ. ET_DDM_CONC
     & .OR. VAR_TYPE .EQ. ET_DDM_CMPST_DEP 
     & .OR.VAR_TYPE .EQ. ET_DDM_WDEP      .OR. VAR_TYPE .EQ. ET_DDM_DDEP 
     &        ) THEN
            
         ! The only options are one source or all sources summed
         ! together (SOURCE from the ELMO Table variable = 0). 
         ! Future versions may expand the feature to allow subsets 
         ! of sources to be summed together.
         ISRC = ELMO_TABLE( J )%SOURCE
         IF ( ISRC .EQ. 0 ) THEN
            ELMO_COMP( J )%L_SOURCE(:) = .TRUE.
         ELSE
            ELMO_COMP( J )%L_SOURCE( ISRC ) = .TRUE.
         END IF
      END IF                                                                                                
      END SUBROUTINE ELMO_INIT_COMPOSITE 

!-------------------------------------------------------------------------
      SUBROUTINE ELMO_SET_COMPONENTS( J, N_VARS, IVAR, CSPEC_IN, COEFF )
!     This subroutine maps the variables in CSPEC to the ELMO Table and
!     stores IDs, variable properties, and coefficients for calculation
!     of ELMO variables during runtime.
!-------------------------------------------------------------------------

      USE PRECURSOR_DATA, ONLY : N_PRECURSOR, PRECURSOR
      USE AERO_DATA, ONLY : N_AEROSPC, AEROSPC
      IMPLICIT NONE

      INTEGER, INTENT( IN ) ::  J, IVAR
      INTEGER, INTENT( INOUT ) :: N_VARS
      CHARACTER( 32 ), INTENT( IN ) :: CSPEC_IN
      CHARACTER( 32 ) CSPEC
      REAL, INTENT( IN ) :: COEFF
      INTEGER  ISPEC, JSPEC, IAERO, ICGRID, IELMO
      INTEGER  I0, I1, I2, VAR_TYPE, ISRC, K
      CHARACTER( 1000 ) :: XMSG
      CHARACTER( 30 ),PARAMETER :: PNAME = 'ELMO_SET_COMPONENTS'

      ! Initialize Index for this COMPOSITE Component
      ELMO_COMP( J )%TABID( IVAR ) = 0

      ! Details in the processing of the components of variable J rely
      ! on knowing what type of variable it is
      VAR_TYPE = ELMO_TABLE( J )%VAR_TYPE

      CSPEC = CSPEC_IN
      
      ! Find this component in the ELMO_TABLE
      I1 = INDEX1( CSPEC, N_ELMO_TABLE, ELMO_TABLE(:)%OUT_NAME )
      !I2 = INDEX1( CSPEC, N_ELMO_TABLE, ELMO_TABLE(:)%NAME )

      ! For ISAM ELMO Variables, we need to make sure the components
      ! match the source number, or apply to all sources (i.e. ISAM
      ! Bulk)
      IF ( VAR_TYPE .EQ. ET_ISAM_CMPST .OR. 
     &     VAR_TYPE .EQ. ET_ISAM_CMPST_DEP ) THEN
#ifdef isam      
         CSPEC = TRIM(CSPEC) // '_ISAM'
         ISRC = ELMO_TABLE( J )%SOURCE
         DO K = 1,N_ELMO_TABLE
           IF ( ELMO_TABLE( K )%NAME .EQ. CSPEC .AND.
     &          ELMO_TABLE( K )%SOURCE .EQ. ISRC     ) THEN
              ! This component matches an ELMO Table Output Name
              ELMO_COMP( J )%TABID( IVAR )    = K
              ELMO_COMP( J )%ID( IVAR )       = ELMO_TABLE( K )%ID
              ELMO_COMP( J )%VAR_TYPE( IVAR ) = VAR_TYPE
              ELMO_COMP( J )%MW( IVAR )       = ELMO_TABLE( K )%MW
              CALL MAP_ELMO_UNITS( ELMO_TABLE( K )%CUNIT,
     &                       ELMO_COMP( J )%SPC_UNIT( IVAR ),
     &                       ELMO_TABLE( K )%NAME,
     &                       ELMO_TABLE( K )%VAR_TYPE )
              ELMO_COMP( J )%COEFF( IVAR )    = COEFF
           END IF
         END DO
#endif

      ELSE IF  ( VAR_TYPE .EQ. ET_DDM_CMPST .OR. 
     &           VAR_TYPE .EQ. ET_DDM_CMPST_DEP ) THEN
#ifdef sens
         CSPEC = TRIM(CSPEC) // '_DDM'
         ISRC = ELMO_TABLE( J )%SOURCE
         DO K = 1,N_ELMO_TABLE
           IF ( ELMO_TABLE( K )%NAME .EQ. CSPEC .AND.
     &          ELMO_TABLE( K )%SOURCE .EQ. ISRC     ) THEN
              ! This component matches an ELMO Table Output Name
              ELMO_COMP( J )%TABID( IVAR )    = K
              ELMO_COMP( J )%ID( IVAR )       = ELMO_TABLE( K )%ID
              ELMO_COMP( J )%VAR_TYPE( IVAR ) = VAR_TYPE
              ELMO_COMP( J )%MW( IVAR )       = ELMO_TABLE( K )%MW
              CALL MAP_ELMO_UNITS( ELMO_TABLE( K )%CUNIT,
     &                       ELMO_COMP( J )%SPC_UNIT( IVAR ),
     &                       ELMO_TABLE( K )%NAME,
     &                       ELMO_TABLE( K )%VAR_TYPE )
              ELMO_COMP( J )%COEFF( IVAR )    = COEFF
           END IF
         END DO 
#endif

      ELSE IF ( I1 .GT. 0 ) THEN
        ! This component matches an ELMO Table Output Name
        ELMO_COMP( J )%TABID( IVAR )    = I1
        ELMO_COMP( J )%ID( IVAR )       = ELMO_TABLE( I1 )%ID
        ELMO_COMP( J )%VAR_TYPE( IVAR ) = ELMO_TABLE( I1 )%VAR_TYPE
        ELMO_COMP( J )%MW( IVAR )       = ELMO_TABLE( I1 )%MW
        CALL MAP_ELMO_UNITS( ELMO_TABLE( I1 )%CUNIT,
     &                       ELMO_COMP( J )%SPC_UNIT( IVAR ),
     &                       ELMO_TABLE( I1 )%NAME,
     &                       ELMO_TABLE( I1 )%VAR_TYPE )
        ELMO_COMP( J )%COEFF( IVAR )    = COEFF
        
       !ELSE IF ( I2 .GT. 0 ) THEN
       ! ! This component matches an ELMO Table Name  
       ! ELMO_COMP( J )%TABID( IVAR )    = I2
       ! ELMO_COMP( J )%ID( IVAR )       = ELMO_TABLE( I2 )%ID
       ! ELMO_COMP( J )%VAR_TYPE( IVAR ) = ELMO_TABLE( I2 )%VAR_TYPE
       ! ELMO_COMP( J )%MW( IVAR )       = ELMO_TABLE( I2 )%MW
       ! CALL MAP_ELMO_UNITS( ELMO_TABLE( I2 )%CUNIT,
     & !                      ELMO_COMP( J )%SPC_UNIT( IVAR ),
     & !                      ELMO_TABLE( I2 )%NAME,
     & !                      ELMO_TABLE( I2 )%VAR_TYPE )
       ! ELMO_COMP( J )%COEFF( IVAR )    = COEFF
        
      ELSE
        ! Component Not found on the ELMO_TABLE. Crash CMAQ gracefully.
        XMSG = 'Component '//TRIM( CSPEC )//' of composite ' //
     &         TRIM( ELMO_TABLE(J)%OUT_NAME )//' is not valid. '//
     &         'Please correct the formula in the CMAQ Chemical '//
     &         'Control File.'
          CALL M3EXIT( PNAME,0,0,XMSG,1 )
      END IF
 



      !CALL LOG_MESSAGE( LOGDEV, '      Component: '//CSPEC )

      END SUBROUTINE ELMO_SET_COMPONENTS

!-------------------------------------------------------------------------
      SUBROUTINE ELMO_PARSE_FORMULA( ICMP, N_VARS, COMPS, COEFFS )
!     This subroutine interprets the formulae specifying the ELMO
!     composite variables in the CMAQ Chemical Control File.
!-------------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER ICMP        ! Index of Composite
      INTEGER N_VARS      ! Number of Components in formula
      CHARACTER( 32 )  :: COMPS( N_MAX_COMPONENTS )
      REAL             :: COEFFS( N_MAX_COMPONENTS )
      CHARACTER( N_MAX_FORMULA ) :: FORMULA
      INTEGER          :: IX, IY, LENGTH, ICHR, ICHR2
      LOGICAL          :: LNEG, LEND, LCOEFF
      CHARACTER( 1 )   :: CHR, CHR2
      CHARACTER( 1000 ) :: XMSG
      CHARACTER( 30 ),PARAMETER :: PNAME = 'ELMO_PARSE_FORMULA'

      FORMULA = CMPST( ICMP )%FORMULA

      !WRITE( LOGDEV, '(6x,A)' )'Formula: '//FORMULA

      COMPS( 1:N_MAX_COMPONENTS ) = ''
      COEFFS( 1:N_MAX_COMPONENTS ) = 1.0
      N_VARS = 0
      LNEG = .FALSE.
      LCOEFF = .FALSE.
      IX = 1
      DO WHILE ( IX .LT. N_MAX_FORMULA )
         CHR = FORMULA( IX:IX ) 
         ICHR = ICHAR( CHR )

         IF ( CHR .EQ. ' ' .OR. CHR .EQ. '+' .or. CHR .EQ. '*' ) THEN
            ! This is a space, plus, or multiplication
            ! so proceed to the next character

         ELSE IF ( CHR .EQ. '-' ) THEN
            ! The next variable should be negative 
            LNEG = .TRUE.
         
         ELSE IF ( ICHR .GE. 48 .AND. ICHR .LE. 57 ) THEN
            ! This is a number. Find out what it is.
            ! Determine the coefficient of this component.
            N_VARS = N_VARS + 1
            LCOEFF = .TRUE.
            CALL ELMO_GETREAL( FORMULA, N_MAX_FORMULA, IX, LNEG,
     &                         COEFFS( N_VARS ) )
         ELSE
            ! This is a letter. Determine the component name
            IF ( LCOEFF ) THEN
               LCOEFF = .FALSE.
            ELSE
               N_VARS = N_VARS + 1
               IF ( LNEG ) THEN
                  COEFFS( N_VARS ) = -1.0
                  LNEG = .FALSE.
               END IF
            END IF

            LENGTH = 0
            LEND = .FALSE.
            IY = IX
            DO WHILE ( IY .LT. N_MAX_FORMULA .AND. .NOT.LEND )
               CHR2 = FORMULA( IY:IY )
               ICHR2 = ICHAR( CHR2 )

               ! Check if this character is not a space or a
               ! special character
               IF ( ( ICHR2 .GE. 65 .AND. ICHR2 .LE. 90 ) .OR. 
     &              ( ICHR2 .GE. 97 .AND. ICHR2 .LE. 122 ) .OR.
     &              ( ICHR2 .GE. 48 .AND. ICHR2 .LE. 57 ) .OR.
     &              ( ICHR2 .EQ. 95 ) ) THEN
                  LENGTH = LENGTH + 1
                  COMPS( N_VARS )( LENGTH:LENGTH ) = CHR2
               ELSE
                  LEND = .TRUE.
               END IF
               IY = IY + 1
            END DO
            IX = IY - 2
            
         END IF

         ! Go to the next character
         IX = IX + 1

      END DO    

      IF ( N_VARS .EQ. 0 ) THEN
         ! No variables found in this formula
         XMSG = 'A formula for an ELMO composite has an error or has '//
     &          'exceeded the allowed length specified by N_MAX_FORMULA. '//
     &          'The formula is: '//TRIM(FORMULA)
         CALL M3EXIT( PNAME,0,0,XMSG,XSTAT1 ) 
      END IF

      END SUBROUTINE ELMO_PARSE_FORMULA
 
!-------------------------------------------------------------------------
      SUBROUTINE ELMO_GETREAL( F, MX, IX, LNEG, COEF )
!     This subroutine parse ELMO composite definitions from the CMAQ
!     Chemical Control namelist and determines what the Real-type
!     coefficient is of a particular component.
!-------------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER MX, IX
      CHARACTER( LEN=MX ) :: F
      LOGICAL LNEG
      REAL COEF
      CHARACTER( 16 ), SAVE :: PNAME = 'ELMO_GETREAL'
      CHARACTER( 300 ) XMSG
            
      INTEGER ICHR, LENGTH, NOSIGNS
      LOGICAL LDECIMAL, LEXP
      CHARACTER( 20 ) :: NUMSTRING
      CHARACTER(  1 ) :: CHR
      LOGICAL LSIGN

      LENGTH    = 0
      LDECIMAL  = .FALSE.
      LEXP      = .FALSE.
      NUMSTRING = ''
      LSIGN = .FALSE.

      DO WHILE ( IX .LT. MX )
        CHR = F( IX:IX )
        ICHR = ICHAR( CHR )

        ! Check if this character is a number
        IF ( ICHR .GE. 48 .AND. ICHR .LE. 57 ) THEN
           LENGTH = LENGTH + 1 
           NUMSTRING( LENGTH:LENGTH ) = CHR

        ! Check if this character is a decimal
        ELSE IF ( CHR .EQ. '.' ) THEN
           IF ( LDECIMAL ) THEN
              ! Too many decimals
              XMSG = 'Too many decimals have been used to specify '//
     &               'a coefficient in an ELMO Composite formula. '//
     &               'The formaula is: '//TRIM(F)
              CALL M3EXIT( 'ELMO_GETREAL',0,0,XMSG,XSTAT1 )
           ELSE
              LDECIMAL = .TRUE.
              LENGTH = LENGTH + 1 
              NUMSTRING( LENGTH:LENGTH ) = '.'
           END IF
              
        ! Check if this is an exponent
        ELSE IF ( CHR .EQ. 'E' .OR. CHR .EQ. 'e' ) THEN
           IF ( LEXP ) THEN
              ! Too many exponents
              XMSG = 'Too many exponents have been used to specify '//
     &               'a coefficient in an ELMO Composite formula. '//
     &               'The formula is: '//TRIM(F)
              CALL M3EXIT( PNAME,0,0,XMSG,XSTAT1 )
           ELSE
              LEXP = .TRUE.
              LENGTH = LENGTH + 1 
              NUMSTRING( LENGTH:LENGTH ) = 'e'
           END IF

        ! Check for sign
        ELSE IF ( CHR .EQ. '+' .OR. CHR .EQ. '-' ) THEN
           IF ( LSIGN ) THEN
              ! Too many signs
              XMSG = 'Too many signs have been used to specify '//
     &               'a coefficient in an ELMO Composite formula. '//
     &               'The formula is: '//TRIM(F)
              CALL M3EXIT( PNAME,0,0,XMSG,XSTAT1 )
           ELSE
              LSIGN = .TRUE.
              LENGTH = LENGTH + 1 
              NUMSTRING( LENGTH:LENGTH ) = CHR
           END IF

        ! If None of these, then we have reached the end of the
        ! coefficient
        ELSE
           IF ( ( .NOT. LEXP ) .AND. ( .NOT. LDECIMAL ) ) THEN
              LENGTH = LENGTH + 1
              NUMSTRING( LENGTH:LENGTH ) = '.'
           END IF
        
           ! Convert number string to real and resolve possible negative
           ! sign
           READ( NUMSTRING, '(E20.6)' ) COEF
           IF ( LNEG ) THEN
               COEF = COEF * -1.0
               LNEG = .FALSE.
           END IF

           RETURN
        END IF

        IX = IX + 1

      END DO

      ! If this point is reached, then IX exceeds the maximum of the
      ! allowed formula length. CMAQ should crash
      XMSG = 'A formula for an ELMO composite has an error or has '//
     &       'exceeded the allowed length specified by N_MAX_FORMULA. '//
     &       'The formula is: '//TRIM(F)

      CALL M3EXIT( PNAME,0,0,XMSG,XSTAT1 )

      END SUBROUTINE ELMO_GETREAL          
       
!-------------------------------------------------------------------------
      SUBROUTINE ELMO_EXPAND_AERO( N_VARS, COMPS, COEFFS )
!     This subroutine parse ELMO composite definitions from the CMAQ
!     Chemical Control namelist and determines what the Real-type
!     coefficient is of a particular component.
!-------------------------------------------------------------------------

      USE AERO_DATA, ONLY : N_AEROSPC, AEROSPC, N_MODE, AERO_MISSING,
     &                      MODESUFF

      IMPLICIT NONE
      
      INTEGER N_VARS      ! Number of Components in formula
      CHARACTER( 32 )  :: COMPS( N_MAX_COMPONENTS )
      CHARACTER( 32 )  :: COMPS_OUT( N_MAX_COMPONENTS )
      REAL             :: COEFFS( N_MAX_COMPONENTS )
      REAL             :: COEFFS_OUT( N_MAX_COMPONENTS )
      CHARACTER( 32 )  :: CSPEC
      INTEGER IVAR, I0, IM
      INTEGER O_VARS, JVAR
       
      IVAR = 1
      JVAR = 1
      O_VARS = N_VARS
      COMPS_OUT = COMPS
      COEFFS_OUT = COEFFS

      DO WHILE ( IVAR .LE. N_VARS )

         ! First Determine if this component is a bulk aerosol concentration 
         ! or deposition. If it is, then it needs to be expanded to add
         ! N_MODE variables to the components list.
         CSPEC = COMPS( IVAR )
         IF ( CSPEC(1:3) .EQ. 'DD_' .OR. CSPEC(1:3) .EQ. 'WD_' ) 
     &      CSPEC = CSPEC(4:32) // '   '
         
         I0 = INDEX1( CSPEC, N_AEROSPC, AEROSPC(:)%BULKNAME )
         IF ( I0 .NE. 0 ) THEN
            ! This is a Bulk Aerosol Species
            ! Add each available mode
            O_VARS = O_VARS - 1
            JVAR = JVAR - 1
            DO IM = 1,N_MODE
               IF ( .NOT. AERO_MISSING( I0,IM ) ) THEN
                  O_VARS = O_VARS + 1
                  JVAR = JVAR + 1
                  IF ( CSPEC .EQ. 'ASEACAT' .OR. CSPEC .EQ. 'ASOIL'
     &             .OR.CSPEC .EQ. 'ACORS'   .OR. CSPEC .EQ. 'ADE_CORS' ) THEN
                     COMPS_OUT( JVAR ) = COMPS( IVAR )
                  ELSE
                     COMPS_OUT( JVAR ) = TRIM( COMPS( IVAR ) ) // MODESUFF( IM )
                  END IF
                  COEFFS_OUT( JVAR ) = COEFFS( IVAR )
               END IF
            END DO
         ELSE 
            ! This is not a Bulk Aerosol Species.
            ! Just pass it through.
            COMPS_OUT( JVAR )  = COMPS( IVAR )
            COEFFS_OUT( JVAR ) = COEFFS( IVAR )

         END IF
         IVAR = IVAR + 1
         JVAR = JVAR + 1
      END DO

      N_VARS = O_VARS
      COMPS = COMPS_OUT
      COEFFS = COEFFS_OUT

      END SUBROUTINE ELMO_EXPAND_AERO
 
      END MODULE ELMO_COMPOSITES

