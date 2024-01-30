
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
 
!-------------------------------------------------------------------------
      SUBROUTINE ELMO_INIT
      ! This subroutine calls subroutines that read user input and build
      ! the ELMO_TABLE. It then maps output file variables to the ELMO
      ! TABLE and creates the ELMO_LOCAL vector for efficient storage of
      ! ELMO results, with maps to the calculation IDs and output file
      ! vectors.
!-------------------------------------------------------------------------
      
      IMPLICIT NONE

      INTEGER N, M, P, J, IM, ILIST, S, F
      CHARACTER( 16 ), SAVE :: PNAME = 'ELMO_INIT'
      CHARACTER( 300 ) :: XMSG
      INTEGER ALLOCSTAT, IOS

      ! Read User-Input from Namelist input file (Miscellaneous Control File)
      CALL READ_ELMO_NML()
      IF ( .NOT. L_ELMO ) RETURN
      
      ! Initialize AOD and Extinction Global Variables
      CALL ELMO_INIT_SHARED()

      ! Initialize Main Table for all possible output variables
      CALL ELMO_TABLE()

      ! Map ELMO User Input to ELMO_TABLE
      CALL ELMO_KEYWORDS

      ! Determine which ELMO_TABLE variables belong on the Instantaneous
      ! file
      L_ELMO_TABLE_INST( : ) = .FALSE.
      CALL MAP_ELMO_NML( INST_VARS_NML, L_ELMO_TABLE_INST, LERROR )
      IF ( LERROR .GT. 0 ) THEN
         ! Entry Doesn't Match any variable on the ELMO table. Crash the model.
         XMSG = 'An Instantaneous ELMO variable has been requested that does not exist.'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      N_ELMO_INST = COUNT( L_ELMO_TABLE_INST )
      ALLOCATE( ELMO_INSTtoTABLE( N_ELMO_INST ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_INSTtoTABLE',PNAME ) 

      N = 0
      DO J = 1,N_ELMO_TABLE
          IF ( L_ELMO_TABLE_INST( J ) ) THEN 
              N = N + 1
              ELMO_INSTtoTABLE( N ) = J
              ELMO_TABLEtoINST( J ) = N
              ELMO_INST_PARS( N ) = ELMO_TABLE( J )
          END IF
      END DO

      ! Determine which ELMO_TABLE variables belong on the Average file
      L_ELMO_TABLE_AVRG( : ) = .FALSE.
      CALL MAP_ELMO_NML( AVRG_VARS_NML, L_ELMO_TABLE_AVRG, LERROR )
      IF ( LERROR .GT. 0 ) THEN
         ! Entry Doesn't Match any variable on the ELMO table. Crash the model.
         XMSG = 'An Average ELMO variable has been requested that does not exist.'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      N_ELMO_AVRG = COUNT( L_ELMO_TABLE_AVRG )
      ALLOCATE( ELMO_AVRGtoTABLE( N_ELMO_AVRG ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_AVRGtoTABLE',PNAME ) 

      N = 0
      DO J = 1,N_ELMO_TABLE
          IF ( L_ELMO_TABLE_AVRG( J ) ) THEN 
              N = N + 1
              ELMO_AVRGtoTABLE( N ) = J
              ELMO_TABLEtoAVRG( J ) = N
              ELMO_AVRG_PARS( N ) = ELMO_TABLE( J )
          END IF
      END DO

      ! Map All ELMO_TABLE Variables, Identify which are Used and
      ! allocate/map ELMO_LOCAL vector
      ALLOCATE( L_ELMO_TABLE_USED( N_ELMO_TABLE ), STAT=IOS )
      CALL CHECKMEM( IOS, 'L_ELMO_TABLE_USED',PNAME ) 
      L_ELMO_TABLE_USED(:) = L_ELMO_TABLE_INST .OR. L_ELMO_TABLE_AVRG

      CALL MAP_ELMO_VARIABLES()

      N_ELMO_LOCAL = COUNT( L_ELMO_TABLE_USED )
      ALLOCATE( ELMO_LOCAL( N_ELMO_LOCAL ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_LOCAL',PNAME ) 
      ELMO_LOCAL = 0.0

      N = 0
      DO J = 1,N_ELMO_TABLE
          IF ( L_ELMO_TABLE_USED( J ) ) THEN
              N = N + 1
              ELMO_LOCALtoTABLE( N ) = J

              IF ( ELMO_TABLEtoINST( J ) .GT. 0 ) 
     &           ELMO_INSTtoLOCAL( ELMO_TABLEtoINST( J ) ) = N
              IF ( ELMO_TABLEtoAVRG( J ) .GT. 0 ) 
     &           ELMO_AVRGtoLOCAL( ELMO_TABLEtoAVRG( J ) ) = N

          END IF    
      END DO

      ALLOCATE( L_CALCULATED( N_ELMO_LOCAL ), STAT=IOS )
      CALL CHECKMEM( IOS, 'L_CALCULATED',PNAME )

      END SUBROUTINE ELMO_INIT
 
!-------------------------------------------------------------------------
      SUBROUTINE READ_ELMO_NML( )
!     This subroutine maps the ELMO variables that the user has
!       requested to the entries in the ELMO_DATA table.
!-------------------------------------------------------------------------
         
      USE RUNTIME_VARS, ONLY: MISC_CTRL, LOGDEV
      USE CGRID_SPCS, ONLY: CGRID_NAME, N_CGRID_SPC
      USE UTIL_FAMILY_MODULE, ONLY: N_Chem_Fams,ChemFamilyName,
     &                              Map_Chem_Families
      USE GRID_CONF, ONLY : MYPE
      
      IMPLICIT NONE

      INTEGER, PARAMETER :: N_NML = 1000
      INTEGER            :: INUM, J, N_END, I1, I2, I3, I4, JNUM
      INTEGER            :: FUNIT
      INTEGER            :: STAT
      LOGICAL, SAVE      :: INSTANT  =.TRUE., 
     &                      AVERAGE  =.TRUE.

      CHARACTER( 16 ), SAVE :: PNAME = 'READ_ELMO_NML'
      CHARACTER( 200 )   :: XMSG
      INTEGER IOS
      LOGICAL EXPAND_NML

      NAMELIST / elmo_activate / instant, average 
      NAMELIST / elmo_inst / inst_layer_top, 
     &                       inst_layer_bot, inst_vars_nml
      NAMELIST / elmo_avrg / avrg_layer_top, 
     &                       avrg_layer_bot, avrg_vars_nml

      ALLOCATE( INST_VARS_NML( N_NML ), STAT = IOS )
      CALL CHECKMEM( IOS, 'INST_VARS_NML',PNAME )
      ALLOCATE( AVRG_VARS_NML( N_NML ), STAT = IOS )
      CALL CHECKMEM( IOS, 'AVRG_VARS_NML',PNAME )
 

      INST_ACTIVE      = .TRUE.
      INST_LAYER_TOP   = 1
      INST_LAYER_BOT   = 1
      INST_VARS_NML(:) = ''
      AVRG_ACTIVE      = .TRUE.
      AVRG_LAYER_TOP   = 1
      AVRG_LAYER_BOT   = 1
      AVRG_VARS_NML(:) = ''

      ! Retrieve the Name of the CMAQ Miscellaneous Control File
      IF ( MISC_CTRL .EQ. "MISC_CTRL_NML" ) THEN
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A)"),
     &           'You have chosen not to indicate the location of an',
     &           'CMAQ_Control_Misc.nml namelist file. Default settings ',
     &           'will be assumed.'
          RETURN
      END IF

      ! Open Miscellaneous Control Namelist File
      FUNIT = JUNIT()
      OPEN( FILE = MISC_CTRL, UNIT = FUNIT, STATUS = 'OLD',
     &      POSITION = 'REWIND', FORM='FORMATTED', IOSTAT = STAT )

      ! Check for Error in File Open Process
      IF ( STAT .NE. 0 ) THEN
          WRITE( XMSG, '(A,A,A)' ),'ERROR: Could not read ',
     &           'CMAQ miscellaneous control namelist file: ',TRIM( MISC_CTRL )
          CALL M3EXIT( PNAME, 0, 0, XMSG, 1 )
      END IF
 
      ! Read Toggles for Turning Instantaneous and Average Files On/Off
      REWIND( FUNIT )
      READ( NML = elmo_activate, UNIT = FUNIT, IOSTAT=STAT )
      IF ( STAT .NE. 0 ) THEN
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &           'Warning! Something went wrong while reading the ',
     &           'ELMO_ACTIVATE section of the CMAQ Miscellaneous Control ',
     &           'Namelist. Default values for this section will be ',
     &           'assumed.'
      END IF
      INST_ACTIVE = INSTANT
      AVRG_ACTIVE = AVERAGE
      L_ELMO = ( INST_ACTIVE .OR. AVRG_ACTIVE )
      
      IF ( .NOT. L_ELMO ) THEN
          RETURN
      END IF

      ! Read Desired ELMO_INST variables from input namelist
      REWIND( FUNIT )
      READ( NML = elmo_inst, UNIT = FUNIT, IOSTAT=STAT )
      IF ( STAT .NE. 0 ) THEN
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &           'Warning! Something went wrong while reading the ',
     &           'ELMO_INST section of the CMAQ Miscellaneous Control ',
     &           'Namelist. Default values for this section will be ',
     &           'assumed.'
      END IF

      ! Check Top-Layer Specification
      CALL ELMO_CHECK_LAYERS( INST_LAYER_TOP, INST_LAYER_BOT, 'INST' )
      NLAY_ELMO_INST = INST_LAYER_TOP - INST_LAYER_BOT + 1

      ! Read Desired ELMO_AVRG variables from input namelist
      REWIND( FUNIT )
      READ( NML = elmo_avrg, UNIT = FUNIT, IOSTAT=STAT )
      IF ( STAT .NE. 0 ) THEN
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &           'Warning! Something went wrong while reading the ',
     &           'ELMO_AVRG section of the CMAQ Miscellaneous Control ',
     &           'Namelist. Default values for this section will be ',
     &           'assumed.'
      END IF
     
      ! Check Top-Layer Specification
      CALL ELMO_CHECK_LAYERS( AVRG_LAYER_TOP, AVRG_LAYER_BOT, 'INST' )
      NLAY_ELMO_AVRG = AVRG_LAYER_TOP - AVRG_LAYER_BOT + 1
      
      END SUBROUTINE READ_ELMO_NML

!-------------------------------------------------------------------------
      SUBROUTINE BUILD_ELMO_TABLE()
!     This subroutine creates the main table of all possible output
!     variables including ELMO variables, bulk aerosol species, CGRID
!     species, and chemical families
!-------------------------------------------------------------------------

      USE AERO_DATA, ONLY: N_MODE, AEROMODE
      USE CGRID_SPCS, ONLY : N_CGRID_SPC, CGRID_NAME, 
     &                       CGRID_UNITS, CGRID_CONC_DESC
      USE UTIL_FAMILY_MODULE, ONLY: N_Chem_Fams,ChemFamilyName
      USE GRID_CONF, ONLY : MYPE

      IMPLICIT NONE

      INTEGER       :: ILIST
      CHARACTER(32) :: PNAME = 'BUILD_ELMO_TABLE'

      N = 0
      ALLOCATE( ELMO_TABLE( 2000 ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_TABLE',PNAME ) 

      ! Load ELMO Variables into ELMO_Table
      DO ILIST = 1,N_ELMO_LIST
         N = N + 1
         ELMO_TABLE( N )%NAME = ELMO_LIST( ILIST )%NAME
         ELMO_TABLE( N )%OUT_TYPE = ELMO_LIST( ILIST )%VAR_TYPE
         ELMO_TABLE( N )%VAR_TYPE = ETYPE_ELMO
         ELMO_TABLE( N )%ID = ELMO_LIST( ILIST )%ID
         ELMO_TABLE( N )%CUNIT = ELMO_LIST( ILIST )%UNIT
         ELMO_TABLE( N )%DESC = ELMO_LIST( ILIST )%DESC
            
         IF ( ELMO_LIST( ILIST )%MODE ) THEN
           ELMO_TABLE( N )%OUT_NAME = ELMO_LIST( ILIST )%NAME//aeromode( 1 )%suff 
           DO IM = 2,N_MODE
             N = N + 1
             ELMO_TABLE( N ) = ELMO_TABLE( N-1 )
             ELMO_TABLE( N )%OUT_NAME = ELMO_LIST( ILIST )%NAME//aeromode( IM )%suff
           END DO
         END IF
      END DO

      ! Load CGRID Species into ELMO_Table
      DO ICGRID = 1,N_CGRID_SPC
         N = N + 1
         ELMO_TABLE( N )%NAME = CGRID_NAME( ICGRID )
         ELMO_TABLE( N )%OUT_NAME = CGRID_NAME( ICGRID )
         ELMO_TABLE( N )%OUT_TYPE = M3REAL
         ELMO_TABLE( N )%VAR_TYPE = ETYPE_CGRID
         ELMO_TABLE( N )%ID = ICGRID
         ELMO_TABLE( N )%CUNIT = CGRID_UNITS( ICGRID )
         ELMO_TABLE( N )%DESC = CGRID_CONC_DESC(S)
      END DO    

      ! Load Aerosol Species into ELMO_Table
      DO IAERO = 1,N_AEROSPC
         N = N + 1
         ELMO_TABLE( N )%NAME = AEROSPC( IAERO )%BULKNAME
         ELMO_TABLE( N )%OUT_NAME = AEROSPC( IAERO )%BULKNAME
         ELMO_TABLE( N )%OUT_TYPE = M3REAL
         ELMO_TABLE( N )%VAR_TYPE = ETYPE_AEROBULK
         ELMO_TABLE( N )%IDG = IAERO
         ELMO_TABLE( N )%CUNIT = 'ug m-3'
         ELMO_TABLE( N )%DESC = 'Total Aerosol Mass for '//
     &                          TRIM(AEROSPC(IAERO)%BULKNAME)
      END DO

      ! Load Chemical Families that are not Aerosol Bulk Species  into ELMO_Table
      DO IFAM = 1,N_CHEM_FAMS
         S = ( CHEMFAMILYNAME( IFAM ), N_AEROSPC, AEROSPC( : )%BULKNAME )
         IF ( S .EQ. 0 ) THEN
           N = N + 1
           ELMO_TABLE( N )%NAME = CHEMFAMILYNAME( IFAM ) 
           ELMO_TABLE( N )%OUT_NAME = CHEMFAMILYNAME( IFAM )
           ELMO_TABLE( N )%OUT_TYPE = M3REAL
           ELMO_TABLE( N )%VAR_TYPE = ETYPE_FAM
           ELMO_TABLE( N )%IDG = IFAM
           ELMO_TABLE( N )%CUNIT = CHEMFAMILYUNITS( IFAM )
           ELMO_TABLE( N )%DESC = 'Mass for Chemical Family '//
     &                            TRIM( CHEMFAMILYNAME( IFAM ))
         END IF
      END DO
 
      ! Load Deposition Variables into ELMO_Table
      DO IWDEP = 1,N_SPC_WDEP
        N = N + 1
        ELMO_TABLE( N )%NAME = 'WDEP_' // WDEP_SPC( IWDEP )
        ELMO_TABLE( N )%OUT_NAME = 'WDEP_' // WDEP_SPC( IWDEP )
        ELMO_TABLE( N )%OUT_TYPE = M3REAL
        ELMO_TABLE( N )%VAR_TYPE = ETYPE_WDEP
        ELMO_TABLE( N )%IDG = IWDEP

        ICGRID = MAP_WDEPtoCGRID( IWDEP )
        IF ( CGRID_MASK_NUM( ICGRID ) ) THEN
           ELMO_TABLE( N )%CUNIT = 'ha-1'
        ELSE IF ( CGRID_MASK_SRF( ICGRID ) ) THEN
           ELMO_TABLE( N )%CUNIT = 'm2 ha-1'
        ELSE
           ELMO_TABLE( N )%CUNIT = 'kg ha-1'
        END IF
        ELMO_TABLE( N )%DESC = 'Wet Deposition for Species '//
     &                         TRIM( WDEP_SPC( IWDEP ) )
      END DO

      DO IDDEP = 1,N_SPC_DEPV
        N = N + 1
        ELMO_TABLE( N )%NAME = 'DDEP_' // DDEP_SPC( IDDEP )
        ELMO_TABLE( N )%OUT_NAME = 'DDEP_' // DDEP_SPC( IDDEP )
        ELMO_TABLE( N )%OUT_TYPE = M3REAL
        ELMO_TABLE( N )%VAR_TYPE = ETYPE_DDEP
        ELMO_TABLE( N )%IDG = IDDEP

        ICGRID = MAP_DEPVtoCGRID( IDDEP )
        IF ( CGRID_MASK_NUM( ICGRID ) ) THEN
           ELMO_TABLE( N )%CUNIT = 'ha-1'
        ELSE IF ( CGRID_MASK_SRF( ICGRID ) ) THEN
           ELMO_TABLE( N )%CUNIT = 'm2 ha-1'
        ELSE
           ELMO_TABLE( N )%CUNIT = 'kg ha-1'
        END IF
        ELMO_TABLE( N )%DESC = 'Dry Deposition for Species '//
     &                         TRIM( DDEP_SPC( IDDEP ) )
      END DO

      ! Load ISAM Variables to ELMO Table
      DO ISPEC = 1,


      ! Load DDM Vriables to ELMO Table


      N_ELMO_TABLE = N
      N_ELMO_ID = NID

      END SUBROUTINE BUILD_ELMO_TABLE

!-------------------------------------------------------------------------
      SUBROUTINE MAP_ELMO_VARIABLES()
!     This subroutine maps the ELMO variables that the user has
!       requested to the entries in the ELMO_DATA table.
!-------------------------------------------------------------------------
      USE CGRID_SPCS, ONLY : CGRID_NAME
      USE UTIL_FAMILY_MODULE, ONLY : CHEMFAMILYNUM, CHEMFAMILYMEMBERS
      USE AERO_DATA, ONLY : ascat_na_fac, asoil_na_fac, acors_na_fac,
     &                      ascat_mg_fac, asoil_mg_fac, acors_mg_fac,
     &                      ascat_k_fac,  asoil_k_fac,  acors_k_fac,
     &                      ascat_ca_fac, asoil_ca_fac, acors_ca_fac,
     &                                    asoil_fe_fac, acors_fe_fac,
     &                                    asoil_mn_fac, acors_mn_fac

      IMPLICIT NONE

      INTEGER IDG, N_VARS, IELMO, S, F, IV

      ALLOCATE( ELMO_COEFFS( NTOT_ELMO_IDS ) )
      ELMO_COEFFS( : )%L_MAPPED = .FALSE.
      
      ! Loop through all available ELMO_LIST Variables
      DO J = 1,N_ELMO_TABLE
          IDG = ELMO_TABLE( J )%ID
          ELMO_IDGtoTABLE( IDG ) = J

          IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_ELMO ) THEN
           SELECT CASE ( IDG )
             
             !!!! Coarse-Mode Variables !!!!  

             ! Map PMC_SO4 - Coarse-Mode Sulfate
             CASE ( ID_PMC_SO4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( J, N_VARS, 'COARSE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( J, 1, 'ASO4', 1.0 )
 
             ! Map PMC_NO3 - Coarse-Mode Nitrate
             CASE ( ID_PMC_NO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( J, N_VARS, 'COARSE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( J, 1, 'ANO3', 1.0 )
 
             ! Map PMC_NH4 - Coarse-Mode Ammonium
             CASE ( ID_PMC_NH4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'COARSE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ANH4', 1.0 )
 
             ! Map PMC_CL - Coarse-Mode Chlorine
             CASE ( ID_PMC_CL ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'COARSE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ACL', 1.0 )
 
             ! Map PMC_NA - Coarse-Mode Sodium
             CASE ( ID_PMC_NA ) 
               N_VARS = 3
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'COARSE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASEACAT', real(ascat_na_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL',   real(asoil_na_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ACORS',   real(acors_na_fac) )
 
             ! Map PMC_MG - Coarse-Mode Magnesium
             CASE ( ID_PMC_MG ) 
               N_VARS = 3
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'COARSE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASEACAT', real(ascat_mg_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL',   real(asoil_mg_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ACORS',   real(acors_mg_fac) )
 
             ! Map PMC_K - Coarse-Mode Potassium
             CASE ( ID_PMC_K ) 
               N_VARS = 3
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'COARSE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASEACAT', real(ascat_k_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL',   real(asoil_k_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ACORS',   real(acors_k_fac) )

             ! Map PMC_CA - Coarse-Mode Calcium
             CASE ( ID_PMC_CA ) 
               N_VARS = 3
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'COARSE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASEACAT', real(ascat_ca_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL',   real(asoil_ca_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ACORS',   real(acors_ca_fac) )

             
             !!!! Fine-Mode Variables !!!!  

             ! Map PMF_SO4 - Fine-Mode Sulfate
             CASE ( ID_PMF_SO4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ASO4', 1.0 )
 
             ! Map PMF_NO3 - Fine-Mode Nitrate
             CASE ( ID_PMF_NO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ANO3', 1.0 )
 
             ! Map PMF_NH4 - Fine-Mode Ammonium
             CASE ( ID_PMF_NH4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS , 'FINE', 'ug m-3')
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ANH4', 1.0 )
 
             ! Map PMF_CL - Fine-Mode Chloride
             CASE ( ID_PMF_CL ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ACL', 1.0 )
 
             ! Map PMF_NA - Fine-Mode Sodium
             CASE ( ID_PMF_NA ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ANA', 1.0 )
 
             ! Map PMF_EC - Fine-Mode Elemental Carbon                                   
             CASE ( ID_PMF_EC )                                                         
               N_VARS = 1                                                               
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )                           
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AEC', 1.0 )                  
                                                                                        
             ! Map PMF_FE - Fine-Mode Iron
             CASE ( ID_PMF_FE ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AFE', 1.0 )
 
             ! Map PMF_AL - Fine-Mode Aluminum
             CASE ( ID_PMF_AL ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AAL', 1.0 )
 
             ! Map PMF_SI - Fine-Mode Silicon
             CASE ( ID_PMF_SI ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ASI', 1.0 )
 
             ! Map PMF_TI - Fine-Mode Titanium
             CASE ( ID_PMF_TI ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ATI', 1.0 )
 
             ! Map PMF_CA - Fine-Mode Calcium
             CASE ( ID_PMF_CA ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ACA', 1.0 )
 
             ! Map PMF_MG - Fine-Mode Magnesium
             CASE ( ID_PMF_MG ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AMG', 1.0 )
 
             ! Map PMF_K - Fine-Mode Potassium
             CASE ( ID_PMF_K ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AK', 1.0 )
 
             ! Map PMF_MN - Fine-Mode Manganese
             CASE ( ID_PMF_MN ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AMN', 1.0 )
 
             ! Map PMF_H2O - Fine-Mode Particle Water           
             CASE ( ID_PMF_H2O )                               
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AH2O', 1.0/19.0 )
 
             ! Map SOIL_IMPV - IMPROVE reconstruction of Soil PM
             CASE ( ID_SOIL_IMPV )
               N_VARS = 5
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'ACCUMULATION', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AAL', 2.20 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'ASI', 2.49 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'ACA', 1.63 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AFE', 2.42 )
               CALL SET_ELMO_COEFF_MAP( IDG, 5, 'ATI', 1.94 )
 
             ! Map PMF_UNSP1 - Retrieve Fine-Mode Unspeciated PM based 
             !   on reconstructed SOIL PM from IMPROVE method and including 
             !   NCOM in the unspeciated portion
             CASE ( ID_PMF_UNSP1 )
               N_VARS = 9
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AGG',  'PMF_MASS', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'ASO4', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'ANO3', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'ANH4', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 5, 'ACL', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 6, 'ANA', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 7, 'AEC', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 8, 'PMF_OC', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 9, 'PMF_SOILIMPV', -1.0 )          
 
             ! Map PMF_UNSP2 - Retrieve Fine-Mode Unspeciated PM based 
             !   on reconstructed SOIL PM from IMPROVE method and EXCLUDING 
             !   NCOM in the unspeciated portion
             CASE ( ID_PMF_UNSP2 )
               N_VARS = 9
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AGG',  'PMF_MASS', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'ASO4', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'ANO3', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'ANH4', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 5, 'ACL', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 6, 'ANA', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 7, 'AEC', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 8, 'PMF_OA', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 9, 'PMF_SOILIMPV', -1.0 )          

             ! Map PMF_HP - Fine-Mode Hydronium Ion
             CASE ( ID_PMF_HP ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AH3OP', 1.0/19.0 )
 
             ! Map PMF_NCOM - Fine-Mode Non-Carbon Organic Mass in POA
             CASE ( ID_PMF_NCOM ) 
               N_VARS = 2
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'PMF_OA', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'PMF_OC',-1.0 )
 
             ! Map PMF_CLDGLY - Fine-Mode SOA from glyoxal and methylglyoxal
             CASE ( ID_PMF_CLDGLY ) 
               N_VARS = 2
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AORGC', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AGLY', 1.0 )
 
             ! Map PMF_ISOP - Fine-Mode Isoprene SOA excluding IEPOX SOA
             CASE ( ID_PMF_ISOP ) 
               N_VARS = 2
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AISO1', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AISO2', 1.0 )
 
             ! Map PMF_IEPOX - Fine-Mode IEPOX SOA
             CASE ( ID_PMF_IEPOX ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AISO3', 1.0 )
 
             ! Map PMF_MTNSOA - Fine-Mode Monoterpene Nitrate SOA
             CASE ( ID_PMF_MTNSOA ) 
               N_VARS = 2
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AMTNO3', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AMTHYD', 1.0 )
 
             ! Map PMF_MTSOA - Fine-Mode Monoterpene SOA excluding
             ! Nitrates
             CASE ( ID_PMF_MTSOA ) 
               N_VARS = 6
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AMT1', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AMT2', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AMT3', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AMT4', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 5, 'AMT5', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 6, 'AMT6', 1.0 )
 
             ! Map PMF_BENAPY - Fine-Mode Benzo-a-Pyrene
             CASE ( ID_PMF_BENAPY ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ABENAPY', 1.0 )
 
             ! Map GAS_BENAPY - Gas-Phase Benzo-a-Pyrene
             CASE ( ID_GAS_BENAPY ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'ALL', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'BENAPY',1.0 ) 
             
             ! Map GAS_NH3 
             CASE ( ID_GAS_NH3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'ALL', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'NH3',1.0 ) 
 
             ! Map GAS_HNO3
             CASE ( ID_GAS_HNO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'ALL', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'HNO3',1.0 ) 
 
             
             !!!! AMS Variables !!!!  

             ! Map AMS_SO4 - AMS-Measure Sulfate
             CASE ( ID_AMS_SO4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PMAMS', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ASO4', 1.0 )
             
             ! Map AMS_NO3 - AMS-Measured Nitrate
             CASE ( ID_AMS_NO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PMAMS', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ANO3', 1.0 )
 
             ! Map AMS_NH4 - AMS-Measured Ammonium
             CASE ( ID_AMS_NH4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PMAMS', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ANH4', 1.0 )
 
             ! Map AMS_CL - AMS-Measured Chloride
             CASE ( ID_AMS_CL ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PMAMS', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ACL', 1.0 )
 
             
             !!!! PM1.0 Variables !!!!  

             ! Map PM1_SO4 - PM1 Sulfate
             CASE ( ID_PM1_SO4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ASO4', 1.0 )
 
             ! Map PM1_NO3 - PM1 Nitrate
             CASE ( ID_PM1_NO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ANO3', 1.0 )
 
             ! Map PM1_NH4 - PM1 Ammonium
             CASE ( ID_PM1_NH4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ANH4', 1.0 )
 
             ! Map PM1_CL - PM1 Chloride
             CASE ( ID_PM1_CL ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ACL', 1.0 )
 
             ! Map PM1_NA - PM1 Sodium
             CASE ( ID_PM1_NA ) 
               N_VARS = 4
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANA', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASEACAT', real(ascat_na_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ASOIL',   real(asoil_na_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ACORS',   real(acors_na_fac) )
 
             ! Map PM1_EC - PM1 Elemental Carbon
             CASE ( ID_PM1_EC ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AEC', 1.0 )
 
             ! Map PM1_MG - PM1 Magnesium
             CASE ( ID_PM1_MG ) 
               N_VARS = 4
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AMG', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASEACAT', real(ascat_mg_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ASOIL',   real(asoil_mg_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ACORS',   real(acors_mg_fac) )
 
             ! Map PM1_K - PM1 Potassium
             CASE ( ID_PM1_K ) 
               N_VARS = 4
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AK', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASEACAT', real(ascat_k_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ASOIL',   real(asoil_k_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ACORS',   real(acors_k_fac) )
 
             ! Map PM1_CA - PM1 Calcium
             CASE ( ID_PM1_CA ) 
               N_VARS = 4
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ACA', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASEACAT', real(ascat_ca_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ASOIL',   real(asoil_ca_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ACORS',   real(acors_ca_fac ))
 
             ! Map PM1_Other - PM1 Other 
             CASE ( ID_PM1_OT ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AOTHR', 1.0 )
 
             ! Map PM1_FE - PM1 Iron
             CASE ( ID_PM1_FE ) 
               N_VARS = 3
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AFE', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL',   real(asoil_fe_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ACORS',   real(acors_fe_fac ))
 
             ! Map PM1_SI - PM1 Silicon
             CASE ( ID_PM1_SI ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ASI', 1.0 )
 
             ! Map PM1_TI - PM1 Titanium
             CASE ( ID_PM1_TI ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ATI', 1.0 )
 
             ! Map PM1_MN - PM1 Manganese
             CASE ( ID_PM1_MN ) 
               N_VARS = 3
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AMN', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL',   real(asoil_mn_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ACORS',   real(acors_mn_fac ))
 
             ! Map PM1_AL - PM1 Aluminum
             CASE ( ID_PM1_AL ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AAL', 1.0 )
  
             ! Map PM1_Soil - PM1.0 Reconstructed Soil for the fine
             ! mode + explicit soil for the coarse mode
             CASE ( ID_PM1_SOIL ) 
               N_VARS = 6
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ASOIL', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AAL', 2.20 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'ASI', 2.49 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'ACA', 1.63 )
               CALL SET_ELMO_COEFF_MAP( IDG, 5, 'AFE', 2.42 )
               CALL SET_ELMO_COEFF_MAP( IDG, 6, 'ATI', 1.94 )
   
             ! Map PM1.0 Unspeciated PM based on reconstructed
             ! SOIL PM from IMPROVE method and including NCOM in the
             ! unspeciated portion
             CASE ( ID_PM1_UNSP1 )
               N_VARS = 9
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'PM1', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'ASO4', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'ANO3', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'ANH4', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 5, 'ACL', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 6, 'ANA', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 7, 'AEC', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 8, 'PM1_OC', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 9, 'PM1_SOIL', -1.0 )          
  
             ! Map PM1_UN - PM1.0 Unspeciated Coarse Mass
             CASE ( ID_PM1_UN ) 
               N_VARS = 3
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ASEACAT', 0.0 ) ! All Seaspray Cation mass has been 
                                                                           ! apportined to Na, Mg, K, and Ca
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'ASOIL', 0.8124 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'ACORS', 0.9207 )
              
             ! Map PM1_HP - PM1.0 Hydronium Ion
             CASE ( ID_PM1_HP ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AH3OP', 1.0/19.0 ) 
             
             !!!! PM2.5 Variables !!!!  

             ! Map PM25_SO4 - PM2.5 Sulfate
             CASE ( ID_PM25_SO4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ASO4', 1.0 )
 
             ! Map PM25_NO3 - PM2.5 Nitrate
             CASE ( ID_PM25_NO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ANO3', 1.0 )
 
             ! Map PM25_NH4 - PM2.5 Ammonium
             CASE ( ID_PM25_NH4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ANH4', 1.0 )
 
             ! Map PM25_CL - PM2.5 Chloride
             CASE ( ID_PM25_CL ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ACL', 1.0 )
 
             ! Map PM25_NA - PM2.5 Sodium
             CASE ( ID_PM25_NA ) 
               N_VARS = 4
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANA', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASEACAT', real(ascat_na_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ASOIL',   real(asoil_na_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ACORS',   real(acors_na_fac ))
 
             ! Map PM25_EC - PM2.5 Elemental Carbon
             CASE ( ID_PM25_EC ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AEC', 1.0 )
 
             ! Map PM25_MG - PM2.5 Magnesium
             CASE ( ID_PM25_MG ) 
               N_VARS = 4
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AMG', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASEACAT', real(ascat_mg_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ASOIL',   real(asoil_mg_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ACORS',   real(acors_mg_fac ))
 
             ! Map PM25_K - PM2.5 Potassium
             CASE ( ID_PM25_K ) 
               N_VARS = 4
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AK', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASEACAT', real(ascat_k_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ASOIL',   real(asoil_k_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ACORS',   real(acors_k_fac ))
 
             ! Map PM25_CA - PM2.5 Calcium
             CASE ( ID_PM25_CA ) 
               N_VARS = 4
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ACA', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASEACAT', real(ascat_ca_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ASOIL',   real(asoil_ca_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ACORS',   real(acors_ca_fac ))
 
             ! Map PM25_Other - PM25 Other 
             CASE ( ID_PM25_OT ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AOTHR', 1.0 )
 
             ! Map PM25_FE - PM2.5 Iron
             CASE ( ID_PM25_FE ) 
               N_VARS = 3
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AFE', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL',   real(asoil_fe_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ACORS',   real(acors_fe_fac ))
 
             ! Map PM25_SI - PM2.5 Silicon
             CASE ( ID_PM25_SI ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ASI', 1.0 )
 
             ! Map PM25_TI - PM2.5 Titanium
             CASE ( ID_PM25_TI ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ATI', 1.0 )
 
             ! Map PM25_MN - PM2.5 Manganese
             CASE ( ID_PM25_MN ) 
               N_VARS = 3
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AMN', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL',   real(asoil_mn_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ACORS',   real(acors_mn_fac ))
 
             ! Map PM25_AL - PM2.5 Aluminum
             CASE ( ID_PM25_AL ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AAL', 1.0 )
 
             ! Map PM25_Soil - PM2.5 Reconstructed Soil for the fine
             ! mode + explicit soil for the coarse mode
             CASE ( ID_PM25_SOIL ) 
               N_VARS = 6
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ASOIL', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AAL', 2.20 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'ASI', 2.49 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'ACA', 1.63 )
               CALL SET_ELMO_COEFF_MAP( IDG, 5, 'AFE', 2.42 )
               CALL SET_ELMO_COEFF_MAP( IDG, 6, 'ATI', 1.94 )
   
             ! Map PM2.5 Unspeciated PM based on reconstructed
             ! SOIL PM from IMPROVE method and including NCOM in the
             ! unspeciated portion
             CASE ( ID_PM25_UNSP1 )
               N_VARS = 9
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'PM25', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'ASO4', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'ANO3', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'ANH4', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 5, 'ACL', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 6, 'ANA', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 7, 'AEC', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 8, 'PM25_OC', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 9, 'PM25_SOIL', -1.0 )          
 
             ! Map PM25_UN - PM2.5 Unspeciated Coarse Mass
             CASE ( ID_PM25_UN ) 
               N_VARS = 3
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASEACAT', 1.0 - real( ascat_na_fac + ascat_mg_fac +  
     &                                                                         ascat_k_fac  + ascat_ca_fac  )) 
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL',   1.0 - real( asoil_na_fac + asoil_mg_fac +
     &                                                                         asoil_k_fac  + asoil_ca_fac +
     &                                                                         asoil_mn_fac + asoil_fe_fac  ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ACORS',   1.0 - real( acors_na_fac + acors_mg_fac +
     &                                                                         acors_k_fac  + acors_ca_fac +
     &                                                                         acors_mn_fac + acors_fe_fac  ))
 
             ! Map PM25_HP - PM2.5 Hydronium Ion
             CASE ( ID_PM25_HP ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AH3OP', 1.0/19.0 )
 
             
             !!!! PM2.5 - 10.0 Variables !!!!  

             ! Map PM25to10_SO4 - PM2.5-10.0 Sulfate
             CASE ( ID_PM25to10_SO4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25TO10', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ASO4', 1.0 )
 
             ! Map PM25to10_NO3 - PM2.5-10.0 Nitrate
             CASE ( ID_PM25to10_NO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25TO10', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ANO3', 1.0 )
 
             ! Map PM25to10_NH4 - PM2.5-10.0 Ammonium
             CASE ( ID_PM25to10_NH4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25TO10', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ANH4', 1.0 )
 
             ! Map PM25to10_CL - PM2.5-10.0 Chloride
             CASE ( ID_PM25to10_CL ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25TO10' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ACL', 1.0 )
 
             ! Map PM25to10_NA - PM2.5-10.0 Sodium
             CASE ( ID_PM25to10_NA ) 
               N_VARS = 4
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25TO10' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANA', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASEACAT', real(ascat_na_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ASOIL',   real(asoil_na_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ACORS',   real(acors_na_fac ))
 
             !!!! Toxics !!!!  
 
             ! Map PM25_HDSL - HAP inventory PM2.5 diesel mass
             CASE ( ID_PM25_HDSL ) 
               N_VARS = 6
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ADE_OTHR', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'ADE_EC', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'ADE_OC', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'ADE_SO4', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 5, 'ADE_NO3', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 6, 'ADE_CORS', 1.0 )
 
             ! Map PM25_HBE - HAP inventory Beryllium
             CASE ( ID_PM25_HBE ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ABE', 1.0 )
 
             ! Map PM25_HCD - HAP inventory Cadmium
             CASE ( ID_PM25_HCD ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ACD', 1.0 )
 
             ! Map PM25_HCR3 - HAP inventory Chromium III
             CASE ( ID_PM25_HCR3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ACR_III', 1.0 )
 
             ! Map PM25_HCR6 - HAP inventory Chromium VI
             CASE ( ID_PM25_HCR6 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ACR_VI', 1.0 )
 
             ! Map PM25_HCR - HAP inventory Total Chromium
             CASE ( ID_PM25_HCR ) 
               N_VARS = 2
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'PM25_HCR3', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'PM25_HCR6', 1.0 )
 
             ! Map PM25_HPB - HAP inventory Lead
             CASE ( ID_PM25_HPB ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'APB', 1.0 )
 
             ! Map PM25_HMN - HAP inventory Manganese
             CASE ( ID_PM25_HMN ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AMN_HAPS', 1.0 )
 
             ! Map PM25_HNI - HAP inventory Nickel
             CASE ( ID_PM25_HNI ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ANI', 1.0 )
 
             ! Map PM25_HAS - HAP inventory Arsenic
             CASE ( ID_PM25_HAS ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AAS', 1.0 )
 
             ! Map PM25_HG - Mercury
             CASE ( ID_PM25_HG ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'APHG', 1.0 )
 
             ! Map PM25_BENAPY - Benzo-a-Pyrene
             CASE ( ID_PM25_BENAPY ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ABENAPY', 1.0 )
 
             ! Map PM10_HDSL - HAP inventory PM2.5 diesel mass
             CASE ( ID_PM10_HDSL ) 
               N_VARS = 6
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ADE_OTHR', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'ADE_EC', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'ADE_OC', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'ADE_SO4', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 5, 'ADE_NO3', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 6, 'ADE_CORS', 1.0 )
 
             ! Map PM10_HBE - HAP inventory Beryllium
             CASE ( ID_PM10_HBE ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ABE', 1.0 )
 
             ! Map PM10_HCD - HAP inventory Cadmium
             CASE ( ID_PM10_HCD ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ACD', 1.0 )
 
             ! Map PM10_HCR3 - HAP inventory Chromium III
             CASE ( ID_PM10_HCR3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ACR_III', 1.0 )
 
             ! Map PM10_HCR6 - HAP inventory Chromium VI
             CASE ( ID_PM10_HCR6 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ACR_VI', 1.0 )
 
             ! Map PM10_HCR - HAP inventory Total Chromium
             CASE ( ID_PM10_HCR ) 
               N_VARS = 2
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'PM10_HCR3', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'PM10_HCR6', 1.0 )
 
             ! Map PM10_HPB - HAP inventory Lead
             CASE ( ID_PM10_HPB ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'APB', 1.0 )
 
             ! Map PM10_HMN - HAP inventory Manganese
             CASE ( ID_PM10_HMN ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AMN_HAPS', 1.0 )
 
             ! Map PM10_HNI - HAP inventory Nickel
             CASE ( ID_PM10_HNI ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ANI', 1.0 )
 
             ! Map PM10_HAS - HAP inventory Arsenic
             CASE ( ID_PM10_HAS ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AAS', 1.0 )
 
             ! Map PM10_HG - Mercury
             CASE ( ID_PM10_HG ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'APHG', 1.0 )
 
             ! Map PM10_BENAPY - Benzo-a-Pyrene
             CASE ( ID_PM10_BENAPY ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ABENAPY', 1.0 )
 
             !!!! Other Variables !!!!  

             ! Map TNO3 - Total Inorganic Nitrate
             CASE ( ID_TNO3 ) 
               N_VARS = 2
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'ALL', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ANO3', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'HNO3', 1.0 ) 
 
             ! Map TNO3TOT - Total Inorganic+Organic Nitrate
             CASE ( ID_TNO3TOT ) 
               N_VARS = 4
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'ALL', 'ug m-3' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'ANO3', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'HNO3', 1.0 ) 
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AISOPNN', 1.0 ) ! Convert to just NO3
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AMTNO3', 1.0 )      ! Convert to just NO3
 
          END SELECT

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_CGRID ) THEN
           ELMO_TABLE( J )%N_VARS = 1            
           CALL SET_ELMO_COEFF_MAP( J, 1, CGRID_NAME( IDG ), 1.0 )

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_AEROBULK ) THEN
           ELMO_TABLE( J )%N_VARS = N_MODE
           DO IM = 1,N_MODE
             CALL SET_ELMO_COEFF_MAP( J, IM, AEROSPC( ID )%NAME( IM ), 1.0 )
           END DO

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_FAM ) THEN
           N_VARS = CHEMFAMILYNUM( ID )
           CALL INIT_ELMO_COEFFS( J, N_VARS, 'ALL', CHEMFAMILYUNITS( ID ) )
           DO K = 1,N_VARS
             CALL SET_ELMO_COEFF_MAP( J, K, CHEMFAMILYMEMBERS(ID,K), 1.0 )
           END DO

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_WDEP ) THEN
           ELMO_TABLE( J )%N_VARS = 1            
           CALL SET_ELMO_COEFF_MAP( J, 1, ETYPE_WDEP, WDEP_SPC( ID ), 1.0 )

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_DDEP ) THEN
           ELMO_TABLE( J )%N_VARS = 1            
           CALL SET_ELMO_COEFF_MAP( J, 1, ETYPE_DDEP, DDEP_SPC( ID ), 1.0 )

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_ISAM ) THEN

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_ISAM_WDEP ) THEN

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_ISAM_DDEP ) THEN

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_DDM ) THEN

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_DDM_WDEP ) THEN

         ELSE IF ( ELMO_TABLE( J )%VAR_TYPE .EQ. ETYPE_DDM_DDEP ) THEN

         END IF
      END DO
      END SUBROUTINE MAP_ELMO_VARIABLES
 
!-------------------------------------------------------------------------
      SUBROUTINE INIT_ELMO_COEFFS( J, N_VARS, AEROTYPE_TMP, UNIT_TMP )
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

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: J 
      INTEGER, INTENT( IN ) :: N_VARS
      CHARACTER( * ), INTENT( IN ) ::  AEROTYPE_TMP
      CHARACTER( * ), INTENT( IN ) ::  UNIT_TMP     ! Options are PPMV and UGM3
      CHARACTER( 20 ) AEROTYPE
      CHARACTER( 200 ) XMSG
      CHARACTER(16), SAVE :: PNAME = 'INIT_ELMO_COEF'
      INTEGER ALLOCSTAT

      ! Initialize All Components of ELMO_COEFFS
      ELMO_COEFFS( J )%L_MAPPED = .TRUE.
      ELMO_COEFFS( J )%N_VARS = N_VARS
      ALLOCATE( ELMO_COEFFS( J )%L_MODE( N_MODE ),
     &          ELMO_COEFFS( IDG )%L_CGRID( N_VARS ),
     &          ELMO_COEFFS( IDG )%L_AERO( N_VARS ),
     &          ELMO_COEFFS( IDG )%L_AGG ( N_VARS ),
     &          ELMO_COEFFS( IDG )%MW    ( N_VARS ),
     &          ELMO_COEFFS( IDG )%UNIT_SP( N_VARS ),
     &          ELMO_COEFFS( IDG )%I_SPEC( N_VARS ),
     &          ELMO_COEFFS( IDG )%COEFF ( N_VARS ),
     &          STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_COEFFS' 
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_COEFFS( IDG )%L_CGRID( : )   = .FALSE.
      ELMO_COEFFS( IDG )%L_AERO ( : )   = .FALSE.
      ELMO_COEFFS( IDG )%L_AGG  ( : )   = .FALSE.
      ELMO_COEFFS( IDG )%MW     ( : )   = 0.
      ELMO_COEFFS( IDG )%UNIT_SP( : )   = 0
      ELMO_COEFFS( IDG )%I_SPEC ( : )   = 0
      ELMO_COEFFS( IDG )%L_MODE ( : )   = .FALSE.
      ELMO_COEFFS( IDG )%COEFF  ( : )   = 0.
      ELMO_COEFFS( IDG )%INLET          = 0
      ELMO_COEFFS( IDG )%WETNESS        = IWET
      ELMO_COEFFS( IDG )%UNIT           = 0       ! 0 = ug m-3

      ! Specify the Aerosol Modes Needed and Inlet Type based on User
      ! Input through the AEROTYPE variable.
      AEROTYPE = AEROTYPE_TMP
      CALL UPCASE( AEROTYPE )
      SELECT CASE ( AEROTYPE )
         CASE ( 'ALL' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = .TRUE.
         CASE ('NONE' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = .FALSE.
         CASE ('ULTRAFINE' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = AEROMODE(:)%ULTRAFINE_MASK
         CASE ('FINE' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = AEROMODE(:)%FINE_MASK
         CASE ('COARSE' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = AEROMODE(:)%COARSE_MASK
         CASE ('NUCLEATION' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = AEROMODE(:)%NUC_MASK
         CASE ('AITKEN' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = AEROMODE(:)%AITKEN_MASK
         CASE ('ACCUMULATION' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = AEROMODE(:)%ACCUM_MASK
         CASE ('PM01' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = .TRUE.
             ELMO_COEFFS( IDG )%INLET = IPM01
             ELMO_COEFFS( IDG )%WETNESS = IWET
         CASE ('PM1' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = .TRUE.
             ELMO_COEFFS( IDG )%INLET = IPM1
             ELMO_COEFFS( IDG )%WETNESS = IWET
         CASE ('PM25' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = .TRUE.
             ELMO_COEFFS( IDG )%INLET = IPM25
             ELMO_COEFFS( IDG )%WETNESS = IWET
         CASE ('PM10' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = .TRUE.
             ELMO_COEFFS( IDG )%INLET = IPM10
             ELMO_COEFFS( IDG )%WETNESS = IWET
         CASE ('PM25TO10' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = .TRUE.
             ELMO_COEFFS( IDG )%INLET = IPM25to10
             ELMO_COEFFS( IDG )%WETNESS = IWET
         CASE ('PMAMS' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = .TRUE.
             ELMO_COEFFS( IDG )%INLET = IPMAMS
             ELMO_COEFFS( IDG )%WETNESS = IWET
         CASE ('GAS')
             ! No Aerosol Parameters need to be set since
             ! this aggregate will be all gas.
         CASE DEFAULT
             XMSG = 'An aerosol descriptor (' // trim(AEROTYPE) // ') has been given ' //
     &              'for the mapping of an ELMO aggregate variable. It is not recognized.'
             CALL M3EXIT( 'INIT_ELMO_COEFFS', 0, 0, XMSG, XSTAT1 )
      END SELECT

      CALl MAP_ELMO_UNITS( UNIT_TMP,  IUNIT )
      ELMO_COEFFS( IDG )%UNIT = IUNIT 

      END SUBROUTINE INIT_ELMO_COEFFS

!-------------------------------------------------------------------------
      SUBROUTINE SET_ELMO_COEFF_MAP( IDG, IVAR, VAR_TYPE, CSPEC, COEFF )
!     This subroutine maps the modes designated by the variable CMODE to
!        to the indices available in L_MODE.
!-------------------------------------------------------------------------

      USE PRECURSOR_DATA, ONLY : N_PRECURSOR, PRECURSOR
      USE CGRID_SPCS, ONLY : N_CGRID_SPC, CGRID_NAME
      USE AERO_DATA, ONLY : N_AEROSPC, AEROSPC
      USE RUNTIME_VARS
      IMPLICIT NONE

      INTEGER, INTENT( IN ) ::  IDG, IVAR, VAR_TYPE
      CHARACTER( * ), INTENT( IN ) :: CSPEC
      REAL, INTENT( IN ) :: COEFF
      INTEGER  ISPEC, IUSED, JSPEC, IAERO, ICGRID, IELMO
      LOGICAL  USED
      CHARACTER( 1000 ) :: XMSG

      ! Populate ELMO parameter structure depending on nature of
      ! variable 
      IF ( VAR_TYPE .EQ. ETYPE_AEROBULK )  ! Aerosol
         ! Found the Index. Save it!
         ELMO_COEFFS( IDG )%I_SPEC( IVAR ) = IAERO
         ELMO_COEFFS( IDG )%L_AERO( IVAR ) = .TRUE.
         ELMO_COEFFS( IDG )%MW( IVAR )     = AEROSPC_MW( IAERO )
         
         CALL MAP_ELMO_UNITS( 'ug m-3', 
     &                        ELMO_COEFFS( IDG )%UNIT_SP( IVAR ) )

      ELSE IF ( VAR_TYPE .EQ. ETYPE_CGRID ) THEN   ! CGRID Species
         ! Found the Index. Save it!
         ELMO_COEFFS( IDG )%I_SPEC( IVAR )  = ICGRID
         ELMO_COEFFS( IDG )%L_CGRID( IVAR ) = .TRUE.
         ELMO_COEFFS( IDG )%MW( IVAR )      = CGRID_MW( ICGRID )

         CALL MAP_ELMO_UNITS( CGRID_UNITS( ICGRID ), 
     &                        ELMO_COEFFS( IDG )%UNIT_SP( IVAR ) )

      ELSE IF ( VAR_TYPE .EQ. ETYPE_ELMO ) THEN    ! ELMO Aggregate Variable
         ! Map to Index on ELMO_LIST, not the ID column
         ELMO_COEFFS( IDG )%I_SPEC( IVAR ) = ELMO_LIST( IELMO )%ID
         ELMO_COEFFS( IDG )%L_AGG( IVAR )  = .TRUE.

         ! MW will not be needed because aggregate ELMO variables 
         ! will not be able to change units.
         ELMO_COEFFS( IDG )%MW( IVAR )     = 0.0

         CALL MAP_ELMO_UNITS( ELMO_COEFFS( ELMO_LIST( IELMO )%ID )%UNIT, 
     &                        ELMO_COEFFS( IDG )%UNIT_SP( IVAR ) )
 
         IF ( ELMO_COEFFS( IDG )%UNIT_SP( IVAR ) .NE. ELMO_COEFFS( IDG )%UNIT ) THEN
            XMSG = "ERROR: An ELMO variable relies " //
     &             "on another ELMO variable " // TRIM(CSPEC) // ". The variable "//
     &             "and family have differing Units. "//
     &             "Please check that the recipes in MAP_ELMO_COEFFS are "// 
     &             "compatible with the mechanism you have selected in ELMO_PROC."
            CALL LOG_MESSAGE( LOGDEV, XMSG )
            CALL M3EXIT( 'SET_ELMO_COEFF_MAP', 0, 0, '', XSTAT1 )
         END IF          
 
             
         ! Check if it is used. If it is not used, then add it to the USED_ELMO_ID
         USED = .FALSE.
         DO JSPEC = 1,N_ELMO
            IF ( ELMO_LIST( IELMO )%ID .EQ. USED_ELMO_ID( JSPEC ) ) USED = .TRUE.
         END DO
         IF ( .NOT. USED ) THEN
            N_ELMO = N_ELMO + 1
            USED_ELMO_ID( N_ELMO ) = ELMO_LIST( IELMO )%ID
         END IF

      ELSE IF ( VAR_TYPE .EQ. ETYPE_FAM ) THEN   ! Family Variable
         ! Map Index to Family Name
         ELMO_COEFFS( IDG )%I_SPEC( IVAR ) = IFAM
         ELMO_COEFFS( IDG )%L_FAM( IVAR )  = .TRUE.

         ! MW will not be needed because family variables 
         ! will not be able to change units.
         ELMO_COEFFS( IDG )%MW( IVAR )     = 0.0
         CALL MAP_ELMO_UNITS( CHEMFAMILYUNITS( IFAM )%UNIT, 
     &                        ELMO_COEFFS( IDG )%UNIT_SP( IVAR ) )

         IF ( ELMO_COEFFS( IDG )%UNIT_SP( IVAR ) .NE. ELMO_COEFFS( IDG )%UNIT ) THEN
            XMSG = "ERROR: An ELMO variable relies " //
     &             "on chemical family " // TRIM(CSPEC) // ". The variable "//
     &             "and family have differing Units. "//
     &             "Please check that the recipes in MAP_ELMO_COEFFS are "// 
     &             "compatible with the mechanism you have selected in ELMO_PROC."
            CALL LOG_MESSAGE( LOGDEV, XMSG )
            CALL M3EXIT( 'SET_ELMO_COEFF_MAP', 0, 0, '', XSTAT1 )
         END IF          

      ELSE    ! Error and Crash
         XMSG = "ERROR: An ELMO variable has been requested that relies " //
     &           "on a variable named " // TRIM(CSPEC) // ". This variable "//
     &           "has not been incorporated in CGRID, the Aerosol module, "//
     &           "the ELMO List, or a chemical family. Please check that the "//
     &           "recipes in MAP_ELMO_COEFFS are compatible with the mechanism "//
     &           "you have selected in ELMO_PROC."
             CALL LOG_MESSAGE( LOGDEV, XMSG )
             CALL M3EXIT( 'SET_ELMO_COEFF_MAP', 0, 0, '', XSTAT1 )
      END IF          
  
      ELMO_COEFFS( IDG )%COEFF( IVAR ) = COEFF

      END SUBROUTINE SET_ELMO_COEFF_MAP

!-------------------------------------------------------------------------
      SUBROUTINE OPEN_ELMO ( JDATE, JTIME, TSTEP )
! Revision history
!   ??? Frank Binkowski
!   8 Sep 01 J.Young: dyn alloc - Use HGRD_DEFN
!   03 Sep 01 David Wong: for new pario
!   13 May 04 P.Bhave: added RH to species list; removed M0 and dry M2
!                      conc's from species list
!   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
!                      domain specifications in one module
!   18 Jul 05 P.Bhave: Added mass fractions of each mode < 2.5um to output list
!   06 Apr 06 P.Bhave: Added GAMMA_N2O5 to output list
!   25 May 06 P.Bhave: Changed units of all DG variables from m to um, as 
!                      suggested by Dr. Bill Hutzell
!   11 Apr 08 J.Kelly: Added STDEVCOR and variables to account for dry & wet
!                      DGCOR, M2COR_WET, and M3COR_DRY
!   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
!   26 Sep 14 H. Pye: Added GAMMA_IEPOX and replaced blank units with na
!   11 May 16 D. Wong: - Modified the code to retreive ELMO information from
!                        construct ELMO_SPC_RECORD stored in module cgrid_spcs
!                        to provide flexibility to handle AE6 and AE61
!                      - renamed the procedure name from OPDIAM to OPELMO
!-------------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID  ! file name parameters

      !...Arguments:

        INTEGER, INTENT( IN ) :: JDATE      ! current model date, coded YYYYDDD
        INTEGER, INTENT( IN ) :: JTIME      ! current model time, coded HHMMSS
        INTEGER, INTENT( IN ) :: TSTEP      ! output time step

      !...Local variables:

        CHARACTER( 16 ), SAVE :: PNAME = 'OPEN_ELMO'
        CHARACTER( 96 ) :: XMSG = ' '

        INTEGER :: L          ! loop induction variables
        INTEGER :: MDATE, MTIME

      ! Define General Output File Parameters
      FTYPE3D = GRDDED3
      GDNAM3D = GRID_NAME  ! from HGRD_DEFN
      TSTEP3D = TSTEP
      NCOLS3D = GL_NCOLS
      NROWS3D = GL_NROWS
      GDTYP3D = GDTYP_GD
      P_ALP3D = P_ALP_GD
      P_BET3D = P_BET_GD 
      P_GAM3D = P_GAM_GD
      XORIG3D = XORIG_GD
      YORIG3D = YORIG_GD
      XCENT3D = XCENT_GD
      YCENT3D = YCENT_GD
      XCELL3D = XCELL_GD
      YCELL3D = YCELL_GD
      VGTYP3D = VGTYP_GD
      VGTOP3D = VGTOP_GD
      NTHIK3D =     1

      ! Open Instantaneous PM Diagnostic File
      IF ( INST_ACTIVE ) THEN
        
         ! Try to open existing file for update
         IF ( .NOT. OPEN3( CTM_ELMO_1, FSRDWR3, PNAME ) ) THEN

            ! Set output file characteristics based on COORD.EXT and open the aerosol
            ! diagnostic file
            SDATE3D = JDATE
            STIME3D = JTIME
            !CALL NEXTIME ( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour
         
            NLAYS3D = INST_LAYER_TOP - INST_LAYER_BOT + 1
            DO L = INST_LAYER_BOT, INST_LAYER_TOP + 1
               VGLVS3D( L ) = VGLVS_GD( L )
            END DO
         
            NVARS3D = N_ELMO_INST_OUT 
            VTYPE3D(1:NVARS3D) = INST_TYPE_OUT( 1:NVARS3D )
            VNAME3D(1:NVARS3D) = INST_PARS_OUT( 1:NVARS3D )
            UNITS3D(1:NVARS3D) = INST_UNIT_OUT( 1:NVARS3D )
            VDESC3D(1:NVARS3D) = INST_DESC_OUT( 1:NVARS3D )
         
            FDESC3D( 1 ) = 'Variables Relevant for PM Formation and Distribution'
            DO L = 2, MXDESC3
               FDESC3D( L ) = ' '
            END DO
         
            ! Open the Instantaneous aerosol diagnostic file
            IF ( .NOT. OPEN3( CTM_ELMO_1, FSNEW3, PNAME ) ) THEN
               XMSG = 'Could not create '// CTM_ELMO_1 // ' file'
               CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
            END IF

         END IF
      END IF
 

      ! Open Average Output PM Diagnostic File
      IF ( AVRG_ACTIVE ) THEN
        
         ! Try to open existing file for update
         IF ( .NOT. OPEN3( CTM_AELMO_1, FSRDWR3, PNAME ) ) THEN

            ! Get end time information from environment variable AVG_FILE_ENDTIME
            MDATE = JDATE; MTIME = JTIME
            IF ( END_TIME ) CALL NEXTIME ( MDATE, MTIME, TSTEP )

            ! Set output file characteristics based on COORD.EXT and open the aerosol
            ! diagnostic file
            SDATE3D = MDATE
            STIME3D = MTIME
         
            NLAYS3D = AVRG_LAYER_TOP - AVRG_LAYER_BOT + 1
            DO L = AVRG_LAYER_BOT, AVRG_LAYER_TOP + 1
               VGLVS3D( L ) = VGLVS_GD( L )
            END DO
         
            NVARS3D = N_ELMO_AVRG_OUT 
            VTYPE3D(1:NVARS3D) = AVRG_TYPE_OUT( 1:NVARS3D )
            VNAME3D(1:NVARS3D) = AVRG_PARS_OUT( 1:NVARS3D )
            UNITS3D(1:NVARS3D) = AVRG_UNIT_OUT( 1:NVARS3D )
            VDESC3D(1:NVARS3D) = AVRG_DESC_OUT( 1:NVARS3D )
         
            FDESC3D( 1 ) = 'Variables Relevant for PM Formation and Distribution'
            DO L = 2, MXDESC3
               FDESC3D( L ) = ' '
            END DO
         
            ! Open the Instantaneous aerosol diagnostic file
            IF ( .NOT. OPEN3( CTM_AELMO_1, FSNEW3, PNAME ) ) THEN
               XMSG = 'Could not create '// CTM_AELMO_1 // ' file'
               CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
            END IF
 
         END IF
      END IF
 
      END SUBROUTINE OPEN_ELMO 

