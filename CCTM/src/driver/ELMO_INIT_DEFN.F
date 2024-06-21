
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
      MODULE ELMO_INIT_DEFN

      USE ELMO_DATA
      
      CONTAINS
!-------------------------------------------------------------------------
      SUBROUTINE ELMO_INIT
      ! This subroutine calls subroutines that read user input and build
      ! the ELMO_TABLE. It then maps output file variables to the ELMO
      ! TABLE and creates the ELMO_LOCAL vector for efficient storage of
      ! ELMO results, with maps to the calculation IDs and output file
      ! vectors.
!-------------------------------------------------------------------------
      
      USE GRID_CONF, ONLY : NCOLS, NROWS
      USE ELMO_COMPONENTS, ONLY : ELMO_MAP_COMPONENTS

      IMPLICIT NONE

      INTEGER N, M, P, J, IM, ILIST, S, F
      INTEGER ID, IV, ISRC, IT, IS, NM, NS
      INTEGER N1, N2, N3
      CHARACTER( 16 ), SAVE :: PNAME = 'ELMO_INIT'
      CHARACTER( 300 ) :: XMSG
      INTEGER ALLOCSTAT, IOS, IERROR
      INTEGER :: N_MAP_MODE_SOURCE, N_MAP_MODE, N_MAP_SOURCE

      ! Read User-Input from Namelist input file (Miscellaneous Control File)
      CALL READ_ELMO_NML()
      IF ( .NOT. L_ELMO ) RETURN
      
      ! Initialize Shared Output Variables like AOD, Extinction, and
      ! other Global Variables
      CALL ELMO_INIT_SHARED()

      ! Initialize Main Table for all possible output variables
      CALL BUILD_ELMO_TABLE()

      ! Map All Table Variables to their components, if applicable
      CALL ELMO_MAP_COMPONENTS()

      ! Map ELMO User Input to ELMO_TABLE
      CALL ELMO_KEYWORDS

      ! INSTANTANEOUS FILE INITIALIZATION
      ! Determine which ELMO_TABLE variables belong on the Instantaneous
      ! file
      ALLOCATE( L_ELMO_TABLE_INST( N_ELMO_TABLE ),
     &          MAP_TABLEtoINST( N_ELMO_TABLE ), STAT=IOS )
      CALL CHECKMEM( IOS, 'L_ELMO_TABLE_INST',PNAME ) 
      L_ELMO_TABLE_INST( : ) = .FALSE.
      MAP_TABLEtoINST = 0

      IF ( INST_ACTIVE ) THEN
        CALL MAP_ELMO_NML( INST_VARS_NML, L_ELMO_TABLE_INST, 'Instantaneous', IERROR )
        IF ( IERROR .GT. 0 ) THEN
           ! Entry Doesn't Match any variable on the ELMO table. Crash the model.
           XMSG = 'An Instantaneous ELMO variable has been requested that does not exist.'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
        END IF

        N_ELMO_INST = COUNT( L_ELMO_TABLE_INST )
        ALLOCATE( MAP_INSTtoTABLE( N_ELMO_INST ),
     &            MAP_INSTtoUSED( N_ELMO_INST ),
     &            ELMO_INST_PARS( N_ELMO_INST ), STAT=IOS )
        CALL CHECKMEM( IOS, 'MAP_INSTtoTABLE',PNAME ) 
        MAP_INSTtoTABLE = 0
        MAP_INSTtoUSED  = 0

        N = 0
        DO J = 1,N_ELMO_TABLE
          IF ( L_ELMO_TABLE_INST( J ) ) THEN 
              N = N + 1
              MAP_INSTtoTABLE( N ) = J
              MAP_TABLEtoINST( J ) = N
              ELMO_INST_PARS( N ) = ELMO_TABLE( J )
          END IF
        END DO
        
        ALLOCATE( ELMO_INST( NCOLS,NROWS,NLAY_ELMO_INST,
     &                       N_ELMO_INST ), STAT=IOS )
        CALL CHECKMEM( IOS, 'ELMO_INST', PNAME )
        ELMO_INST = 0.0
      END IF

      ! AVERAGE FILE INITIALIZATION
      ! Determine which ELMO_TABLE variables belong on the Average file
      ALLOCATE( L_ELMO_TABLE_AVRG( N_ELMO_TABLE ),
     &          MAP_TABLEtoAVRG( N_ELMO_TABLE ), STAT=IOS )
      CALL CHECKMEM( IOS, 'L_ELMO_TABLE_AVRG',PNAME ) 
      L_ELMO_TABLE_AVRG( : ) = .FALSE.
      MAP_TABLEtoAVRG = 0

      IF ( AVRG_ACTIVE ) THEN
        CALL MAP_ELMO_NML( AVRG_VARS_NML, L_ELMO_TABLE_AVRG, 'Average', IERROR )
        IF ( IERROR .GT. 0 ) THEN
           ! Entry Doesn't Match any variable on the ELMO table. Crash the model.
           XMSG = 'An Average ELMO variable has been requested that does not exist.'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
        END IF
     
        N_ELMO_AVRG = COUNT( L_ELMO_TABLE_AVRG )
        ALLOCATE( MAP_AVRGtoTABLE( N_ELMO_AVRG ),
     &            MAP_AVRGtoUSED( N_ELMO_AVRG ),
     &            ELMO_AVRG_PARS( N_ELMO_AVRG ), STAT=IOS )
        CALL CHECKMEM( IOS, 'ELMO_AVRGtoTABLE',PNAME ) 
        MAP_AVRGtoTABLE = 0
        MAP_AVRGtoUSED  = 0
     
        N = 0
        DO J = 1,N_ELMO_TABLE
            IF ( L_ELMO_TABLE_AVRG( J ) ) THEN 
                N = N + 1
                MAP_AVRGtoTABLE( N ) = J
                MAP_TABLEtoAVRG( J ) = N
                ELMO_AVRG_PARS( N ) = ELMO_TABLE( J )
            END IF
        END DO

        ALLOCATE( ELMO_AVRG( NCOLS,NROWS,NLAY_ELMO_AVRG,
     &                       N_ELMO_AVRG ), STAT=IOS )
        CALL CHECKMEM( IOS, 'ELMO_AVRG', PNAME )
        ELMO_AVRG = 0.0
      END IF

      ! Identify which ELMO_TABLE variables are Used and
      ! allocate/map ELMO_USED vector
      ALLOCATE( L_ELMO_TABLE_USED( N_ELMO_TABLE ), STAT=IOS )
      CALL CHECKMEM( IOS, 'L_ELMO_TABLE_USED',PNAME ) 
      L_ELMO_TABLE_USED(:) = L_ELMO_TABLE_INST .OR. L_ELMO_TABLE_AVRG

      N_ELMO_USED = COUNT( L_ELMO_TABLE_USED )
      ALLOCATE( ELMO_USED( N_ELMO_USED ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_USED',PNAME ) 
      ELMO_USED = 0.0
      ALLOCATE( ELMO_USED_ID( N_ELMO_USED ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_USED_ID',PNAME ) 
      ELMO_USED_ID = 0
      ALLOCATE( ELMO_USED_IM( N_ELMO_USED ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_USED_IM',PNAME ) 
      ELMO_USED_IM = 0
      ALLOCATE( ELMO_USED_IV( N_ELMO_USED ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_USED_IV',PNAME ) 
      ELMO_USED_IV = 0
      ALLOCATE( ELMO_USED_ISRC( N_ELMO_USED ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_USED_ISRC',PNAME ) 
      ELMO_USED_ISRC = 0
      ALLOCATE( MAP_USEDtoTABLE( N_ELMO_USED ), STAT=IOS )
      CALL CHECKMEM( IOS, 'MAP_USEDtoTABLE',PNAME ) 
      MAP_USEDtoTABLE = 0
      ALLOCATE( MAP_TABLEtoUSED( N_ELMO_TABLE ), STAT=IOS )
      CALL CHECKMEM( IOS, 'MAP_TABLEtoUSED',PNAME ) 
      MAP_TABLEtoUSED = 0

      ALLOCATE( MAP_ELMOTABLE( MAXVAL( ELMO_TABLE(:)%VAR_TYPE),
     &                         MAXVAL( ELMO_TABLE(:)%ID) ), STAT=IOS )
      CALL CHECKMEM( IOS, 'MAP_ELMOTABLE',PNAME ) 
      MAP_ELMOTABLE = 0
      ALLOCATE( N_ELMOTABLE_MODE( MAXVAL( ELMO_TABLE(:)%VAR_TYPE),
     &                            MAXVAL( ELMO_TABLE(:)%ID) ), STAT=IOS )
      CALL CHECKMEM( IOS, 'N_ELMOTABLE_MODE',PNAME ) 
      N_ELMOTABLE_MODE = 0
      ALLOCATE( N_ELMOTABLE_SOURCE( MAXVAL( ELMO_TABLE(:)%VAR_TYPE),
     &                              MAXVAL( ELMO_TABLE(:)%ID) ), STAT=IOS )
      CALL CHECKMEM( IOS, 'N_ELMOTABLE_SOURCE',PNAME ) 
      N_ELMOTABLE_SOURCE = 0


      N = 0
      DO J = 1,N_ELMO_TABLE
          IF ( L_ELMO_TABLE_USED( J ) ) THEN
              N = N + 1
              MAP_TABLEtoUSED( J ) = N
              MAP_USEDtoTABLE( N ) = J

              ID   = ELMO_TABLE(J)%ID
              IM   = ELMO_TABLE(J)%IM
              IV   = ELMO_TABLE(J)%VAR_TYPE
              ISRC = ELMO_TABLE(J)%SOURCE

              ELMO_USED_ID( N )   = ID
              ELMO_USED_IM( N )   = IM
              ELMO_USED_IV( N )   = IV
              ELMO_USED_ISRC( N ) = ISRC

              IF ( MAP_TABLEtoINST( J ) .GT. 0 ) 
     &           MAP_INSTtoUSED( MAP_TABLEtoINST( J ) ) = N
              IF ( MAP_TABLEtoAVRG( J ) .GT. 0 ) 
     &           MAP_AVRGtoUSED( MAP_TABLEtoAVRG( J ) ) = N

              ! Count number of modes for variables with multiple modes
              IF ( IM .GT. 0 ) THEN
                N_ELMOTABLE_MODE( IV, ID ) = N_ELMOTABLE_MODE( IV, ID ) + 1
              END IF

              ! Count number of sources for variables with multiple
              ! sources
              IF ( ISRC .GT. 0 ) THEN
                N_ELMOTABLE_SOURCE( IV, ID ) = N_ELMOTABLE_SOURCE( IV, ID ) + 1
              END IF

          END IF    
      END DO

      ! Allocate Look Up table to get from calculation ID and variable
      ! type to ELMO Table ID.
      N_MAP_MODE_SOURCE = COUNT( N_ELMOTABLE_MODE(:,:).GT.0 .AND.
     &                           N_ELMOTABLE_SOURCE(:,:).GT.0 )
      N_MAP_MODE = COUNT( N_ELMOTABLE_MODE(:,:).GT.0 ) - N_MAP_MODE_SOURCE
      N_MAP_SOURCE = COUNT( N_ELMOTABLE_SOURCE(:,:).GT.0 ) - N_MAP_MODE_SOURCE
      ALLOCATE( MAP_ELMO_IDtoTABLE_MODE_SOURCE( N_MAP_MODE_SOURCE ) )
      ALLOCATE( MAP_ELMO_IDtoTABLE_MODE( N_MAP_MODE ) )
      ALLOCATE( MAP_ELMO_IDtoTABLE_SOURCE( N_MAP_SOURCE ) )

      ! Loop through all used variables and create maps from variable
      ! type, calculation ID, number of modes and sources back to the
      ! ELMO Table ID.
      N1 = 0
      N2 = 0
      N3 = 0

      DO N = 1,N_ELMO_USED
        IT = MAP_USEDtoTABLE( N )

        ID = ELMO_USED_ID(N)
        IV = ELMO_USED_IV(N)
        IM =  ELMO_USED_IM(N)
        IS =  ELMO_USED_ISRC(N)

        NM = N_ELMOTABLE_MODE( IV,ID )
        NS = N_ELMOTABLE_SOURCE( IV,ID )
        
        IF ( NM .GT. 0 .AND. NS .gt. 0    ) THEN
          ! Multiple Modes and Sources
          IF ( MAP_ELMOTABLE( IV,ID ).EQ.0 ) THEN
             N1 = N1 + 1
             MAP_ELMOTABLE( IV,ID ) = N1
             ALLOCATE( MAP_ELMO_IDtoTABLE_MODE_SOURCE( N1 )%IND(NM,NS) )
             MAP_ELMO_IDtoTABLE_MODE_SOURCE( N1 )%IND(:,:) = 0
          END IF  
          MAP_ELMO_IDtoTABLE_MODE_SOURCE( MAP_ELMOTABLE(IV,ID) )%IND(IM,IS) = IT

        ELSE IF ( NM .GT. 0 ) THEN
          ! Multiple Modes
          IF ( MAP_ELMOTABLE( IV,ID ).EQ.0 ) THEN
             N2 = N2 + 1
             MAP_ELMOTABLE( IV,ID ) = N2
             ALLOCATE( MAP_ELMO_IDtoTABLE_MODE( N2 )%IND(NM) )
             MAP_ELMO_IDtoTABLE_MODE( N2 )%IND(:) = 0
          END IF
          MAP_ELMO_IDtoTABLE_MODE( MAP_ELMOTABLE(IV,ID) )%IND(IM) = IT

        ELSE IF ( NS .GT. 0 ) THEN
          ! Multiple Sources
          IF ( MAP_ELMOTABLE( IV,ID ).EQ.0 ) THEN
             N3 = N3 + 1
             MAP_ELMOTABLE( IV,ID ) = N3
             ALLOCATE( MAP_ELMO_IDtoTABLE_SOURCE( N3 )%IND(NS) )
             MAP_ELMO_IDtoTABLE_SOURCE( N3 )%IND(:) = 0
          END IF
          MAP_ELMO_IDtoTABLE_SOURCE( MAP_ELMOTABLE(IV,ID) )%IND(IS) = IT

        ELSE 
          ! Assign Table ID for variables that do not have multiple
          ! modes or sources
          MAP_ELMOTABLE( IV,ID ) = IT

        END IF
      END DO


      ! Allocate array for tracking whether or not a variable has been
      ! calculated each time step.
      ALLOCATE( L_CALCULATED( N_ELMO_USED ), STAT=IOS )
      CALL CHECKMEM( IOS, 'L_CALCULATED',PNAME )
      L_CALCULATED = .FALSE.


      ! Write Output Variable Information to Log File
      CALL ELMO_WRITE_LOG()

      END SUBROUTINE ELMO_INIT
 
!-------------------------------------------------------------------------
      SUBROUTINE READ_ELMO_NML( )
!     This subroutine maps the ELMO variables that the user has
!       requested to the entries in the ELMO_DATA table.
!-------------------------------------------------------------------------
      
      USE RUNTIME_VARS, ONLY: MISC_CTRL, LOGDEV
      
      IMPLICIT NONE

      INTEGER, PARAMETER :: N_NML = 1000
      INTEGER            :: FUNIT
      INTEGER            :: STAT
      LOGICAL, SAVE      :: INSTANT  =.TRUE., 
     &                      AVERAGE  =.TRUE.

      CHARACTER( 16 ), SAVE :: PNAME = 'READ_ELMO_NML'
      CHARACTER( 200 )   :: XMSG
      INTEGER IOS

      NAMELIST / elmo_activate / instant, average 
      NAMELIST / elmo_inst_nml / inst_layer_top, 
     &                           inst_layer_bot, inst_vars_nml
      NAMELIST / elmo_avrg_nml / avrg_layer_top, 
     &                           avrg_layer_bot, avrg_vars_nml

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
#ifdef mpas
      L_ELMO = .false.
#endif
      
      IF ( .NOT. L_ELMO ) THEN
          RETURN
      END IF

      ! Read Desired ELMO_INST variables from input namelist
      REWIND( FUNIT )
      READ( NML = elmo_inst_nml, UNIT = FUNIT, IOSTAT=STAT )
      IF ( STAT .NE. 0 ) THEN
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &           'Warning! Something went wrong while reading the ',
     &           'ELMO_INST_NML section of the CMAQ Miscellaneous Control ',
     &           'Namelist. Default values for this section will be ',
     &           'assumed.'
      END IF

      ! Check Top-Layer Specification
      CALL ELMO_CHECK_LAYERS( INST_LAYER_TOP, INST_LAYER_BOT, 'INST' )
      NLAY_ELMO_INST = INST_LAYER_TOP - INST_LAYER_BOT + 1

      ! Read Desired ELMO_AVRG variables from input namelist
      REWIND( FUNIT )
      READ( NML = elmo_avrg_nml, UNIT = FUNIT, IOSTAT=STAT )
      IF ( STAT .NE. 0 ) THEN
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &           'Warning! Something went wrong while reading the ',
     &           'ELMO_AVRG_NML section of the CMAQ Miscellaneous Control ',
     &           'Namelist. Default values for this section will be ',
     &           'assumed.'
      END IF
     
      ! Check Top-Layer Specification
      CALL ELMO_CHECK_LAYERS( AVRG_LAYER_TOP, AVRG_LAYER_BOT, 'AVRG' )
      NLAY_ELMO_AVRG = AVRG_LAYER_TOP - AVRG_LAYER_BOT + 1
      
      END SUBROUTINE READ_ELMO_NML

!-------------------------------------------------------------------------
      SUBROUTINE BUILD_ELMO_TABLE()
!     This subroutine creates the main table of all possible output
!     variables including ELMO variables, bulk aerosol species, CGRID
!     species, and chemical families
!-------------------------------------------------------------------------

      USE AERO_DATA, ONLY: N_MODE, AEROMODE, N_AEROSPC, AEROSPC,
     &                     AEROSPC_MW
      USE CGRID_SPCS, ONLY : N_CGRID_SPC, CGRID_NAME, CGRID_MW,
     &                       CGRID_UNITS, CGRID_CONC_DESC,
     &                       CGRID_MASK_NUM, CGRID_MASK_SRF,
     &                       CGRID_MASK_AERO,
     &                       WDEP_SPC, N_SPC_WDEP, MAP_WDEPtoCGRID,
     &                       N_SPC_DEPV, MAP_DEPVtoCGRID
      USE VDIFF_MAP, ONLY : DDEP_SPC
      USE UTIL_FAMILY_MODULE, ONLY: N_Chem_Fams,ChemFamilyName,
     &                              ChemFamilyUnits
      USE GRID_CONF, ONLY : MYPE
      USE UTILIO_DEFN

#ifdef isam
      USE SA_DEFN, ONLY : TAGNAME, NSPC_SA, NTAG_SA, ISAM_SPEC,
     &                    MAP_SAtoCGR
#endif

#ifdef sens
      USE DDM3D_DEFN, ONLY : NPMAX, SEN_PAR, N_ASENS_VARS
#endif

      IMPLICIT NONE

      INTEGER       :: ILIST, IUNIT, IFAM, S, IM, ICGRID,
     &                 IWDEP, IDDEP
      INTEGER       :: VAR_TYPE
      INTEGER       :: N, IAERO, IOS, ITAG, ISPEC, NP
      CHARACTER(32) :: PNAME = 'BUILD_ELMO_TABLE'

      N = 0
#ifdef isam | sens
      ALLOCATE( ELMO_TABLE( 10000 ), STAT=IOS )
#else
      ALLOCATE( ELMO_TABLE( 2000 ), STAT=IOS )
#endif
      CALL CHECKMEM( IOS, 'ELMO_TABLE',PNAME ) 
       
      ! Load Bulk Aerosol Species into ELMO_Table
      ! Because the family names are loaded first in the ELMO Table, any
      ! request for a chemical family name that matches an ELMO variable
      ! name will default to outputting the chemical family.
      DO IAERO = 1,N_AEROSPC
         N = N + 1
         ELMO_TABLE( N )%NAME = AEROSPC( IAERO )%BULKNAME
         ELMO_TABLE( N )%OUT_NAME = AEROSPC( IAERO )%BULKNAME
         ELMO_TABLE( N )%OUT_TYPE = M3REAL
         ELMO_TABLE( N )%VAR_TYPE = ETYPE_AEROBULK
         ELMO_TABLE( N )%ID = IAERO
         ELMO_TABLE( N )%CUNIT = 'ug m-3'
         CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                        ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
         ELMO_TABLE( N )%IUNIT = IUNIT
         ELMO_TABLE( N )%DESC = 'Total Aerosol Mass for '//
     &                          TRIM(AEROSPC(IAERO)%BULKNAME)
         ELMO_TABLE( N )%MW   = AEROSPC_MW( IAERO )
      END DO
 
      ! Load Chemical Families that are not Aerosol Bulk Species into ELMO_Table
      DO IFAM = 1,N_CHEM_FAMS
         S = INDEX1( CHEMFAMILYNAME( IFAM ), N_AEROSPC, AEROSPC( : )%BULKNAME )
         IF ( S .EQ. 0 ) THEN
           N = N + 1
           ELMO_TABLE( N )%NAME = CHEMFAMILYNAME( IFAM ) 
           ELMO_TABLE( N )%OUT_NAME = CHEMFAMILYNAME( IFAM )
           ELMO_TABLE( N )%OUT_TYPE = M3REAL
           ELMO_TABLE( N )%VAR_TYPE = ETYPE_FAM
           ELMO_TABLE( N )%ID = IFAM
           ELMO_TABLE( N )%CUNIT = CHEMFAMILYUNITS( IFAM )
           CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                          ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
           ELMO_TABLE( N )%IUNIT = IUNIT
           ELMO_TABLE( N )%DESC = 'Mass for Chemical Family '//
     &                            TRIM( CHEMFAMILYNAME( IFAM ))
           ELMO_TABLE( N )%MW   = 0.0
         END IF
      END DO
 
#ifdef isam
      ! Load ISAM Bulk Aerosols
      DO IAERO = 1,N_AEROSPC
         N = N + 1
         ELMO_TABLE( N )%NAME = TRIM( AEROSPC( IAERO )%BULKNAME) // '_ISAM'
         ELMO_TABLE( N )%OUT_NAME = TRIM(AEROSPC( IAERO )%BULKNAME) //'_TAGS'

         ELMO_TABLE( N )%OUT_TYPE = M3REAL
         ELMO_TABLE( N )%VAR_TYPE = ETYPE_ISAM_AEROBULK
         ELMO_TABLE( N )%ID = IAERO
         ELMO_TABLE( N )%SOURCE = 0
         ELMO_TABLE( N )%CUNIT = 'ug m-3'
         CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                        ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
         ELMO_TABLE( N )%IUNIT = IUNIT
         ELMO_TABLE( N )%DESC = 'Total Aerosol Mass for '//
     &        TRIM(AEROSPC(IAERO)%BULKNAME)//' across all sources.'
         ELMO_TABLE( N )%MW   = AEROSPC_MW( IAERO )
 
         DO ITAG = 1,NTAG_SA
           N = N + 1
           ELMO_TABLE( N ) = ELMO_TABLE( N-1 )
           ELMO_TABLE( N )%NAME = TRIM(AEROSPC( IAERO )%BULKNAME) //'_ISAM' 
           ELMO_TABLE( N )%OUT_NAME = TRIM(AEROSPC( IAERO )%BULKNAME)
     &                            // '_' // TRIM(TAGNAME( ITAG )) 
           ELMO_TABLE( N )%SOURCE = ITAG
           ELMO_TABLE( N )%DESC = 'ISAM Concentration of Species '//
     &                     TRIM( AEROSPC( IAERO )%BULKNAME ) //
     &                     ' from source ' // TRIM(TAGNAME( ITAG ))
        END DO  
      END DO

      ! Load ISAM Families
      DO IFAM = 1,N_CHEM_FAMS
         S = INDEX1( CHEMFAMILYNAME( IFAM ), N_AEROSPC, AEROSPC( : )%BULKNAME )
         IF ( S .EQ. 0 ) THEN
           N = N + 1
           ELMO_TABLE( N )%NAME = TRIM( CHEMFAMILYNAME( IFAM )) // '_ISAM'
           ELMO_TABLE( N )%OUT_NAME = TRIM(CHEMFAMILYNAME(IFAM)) //'_TAGS'
          
           ELMO_TABLE( N )%OUT_TYPE = M3REAL
           ELMO_TABLE( N )%VAR_TYPE = ETYPE_ISAM_FAM
           ELMO_TABLE( N )%ID = IFAM
           ELMO_TABLE( N )%SOURCE = 0
           ELMO_TABLE( N )%CUNIT = CHEMFAMILYUNITS( IFAM )
           CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                          ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
           ELMO_TABLE( N )%IUNIT = IUNIT
           ELMO_TABLE( N )%DESC = 'Concentration of Chemical Family '//
     &                            TRIM( CHEMFAMILYNAME( IFAM )) //
     &                            ' across all sources.'
           ELMO_TABLE( N )%MW   = 0.0
          
           DO ITAG = 1,NTAG_SA
             N = N + 1
             ELMO_TABLE( N ) = ELMO_TABLE( N-1 )
             ELMO_TABLE( N )%NAME = TRIM(CHEMFAMILYNAME( IFAM ))
     &                              // '_ISAM'
             ELMO_TABLE( N )%OUT_NAME = TRIM(CHEMFAMILYNAME( IFAM ))
     &                              // '_' // TRIM(TAGNAME( ITAG )) 
             ELMO_TABLE( N )%SOURCE = ITAG
             ELMO_TABLE( N )%DESC = 'Concentration of Chemical Family '//
     &                       TRIM( CHEMFAMILYNAME( IFAM ) ) //
     &                       ' from source ' // TRIM(TAGNAME( ITAG )) 
           END DO
        END IF
      END DO
      
#endif

#ifdef sens
      ! Load DDM Bulk Aerosols
      DO IAERO = 1,N_AEROSPC
         DO NP = 1,NPMAX
            N = N + 1
            ELMO_TABLE( N )%NAME = TRIM(AEROSPC( IAERO )%BULKNAME)
     &                               // '_DDM' 
            ELMO_TABLE( N )%OUT_NAME = TRIM(AEROSPC( IAERO )%BULKNAME)
     &                               // '_' // TRIM(SEN_PAR( NP )) 
          
            ELMO_TABLE( N )%OUT_TYPE = M3REAL
            ELMO_TABLE( N )%VAR_TYPE = ETYPE_DDM_AEROBULK
            ELMO_TABLE( N )%ID = IAERO
            ELMO_TABLE( N )%SOURCE = NP
            ELMO_TABLE( N )%CUNIT = 'ug m-3'
            CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                           ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
            ELMO_TABLE( N )%IUNIT = IUNIT
            ELMO_TABLE( N )%MW   = AEROSPC_MW( IAERO )
            ELMO_TABLE( N )%DESC = 'ISAM Concentration of Species '//
     &                      TRIM( AEROSPC( IAERO )%BULKNAME ) //
     &                      ' from source ' // TRIM(SEN_PAR( NP ))
        END DO  
      END DO
 
      ! Load DDM Families
      DO IFAM = 1,N_CHEM_FAMS
         S = INDEX1( CHEMFAMILYNAME( IFAM ), N_AEROSPC, AEROSPC( : )%BULKNAME )
         IF ( S .EQ. 0 ) THEN
           DO NP = 1,NPMAX
              N = N + 1
              ELMO_TABLE( N )%NAME = TRIM( CHEMFAMILYNAME( IFAM ))
     &                               // '_DDM'
              ELMO_TABLE( N )%OUT_NAME = TRIM(CHEMFAMILYNAME( IFAM ))
     &                               // '_' // TRIM(SEN_PAR( NP )) 
              
              ELMO_TABLE( N )%OUT_TYPE = M3REAL
              ELMO_TABLE( N )%VAR_TYPE = ETYPE_DDM_FAM
              ELMO_TABLE( N )%ID = IFAM
              ELMO_TABLE( N )%SOURCE = NP
              ELMO_TABLE( N )%CUNIT = CHEMFAMILYUNITS( IFAM )
              CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                             ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
              ELMO_TABLE( N )%IUNIT = IUNIT
              ELMO_TABLE( N )%MW   = 0.0
              ELMO_TABLE( N )%DESC = 'Concentration of Chemical Family '//
     &                        TRIM( CHEMFAMILYNAME( IFAM ) ) //
     &                        ' from source ' // TRIM(SEN_PAR( NP )) 
           END DO
         END IF
      END DO
 
#endif

      ! Load ELMO Variables into ELMO_Table
      DO ILIST = 1,N_ELMO_LIST
         N = N + 1
         ELMO_TABLE( N )%NAME = ELMO_LIST( ILIST )%NAME
         ELMO_TABLE( N )%OUT_NAME = ELMO_LIST( ILIST )%NAME
         ELMO_TABLE( N )%OUT_TYPE = ELMO_LIST( ILIST )%OUT_TYPE
         ELMO_TABLE( N )%VAR_TYPE = ETYPE_ELMO
         ELMO_TABLE( N )%ID = ELMO_LIST( ILIST )%ID
         ELMO_TABLE( N )%CUNIT = ELMO_LIST( ILIST )%UNIT
         CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                        ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
         ELMO_TABLE( N )%IUNIT = IUNIT
         ELMO_TABLE( N )%DESC = ELMO_LIST( ILIST )%DESC
         ELMO_TABLE( N )%MW   = 0.0
            
         IF ( ELMO_LIST( ILIST )%L_MODE ) THEN
           ! Set Mode-Dependent Properties for this variables first mode
           ELMO_TABLE( N )%IM = 1
           ELMO_TABLE( N )%OUT_NAME = TRIM(ELMO_LIST( ILIST)%NAME)//'_'//aeromode( 1 )%suff 
           ELMO_TABLE( N )%DESC = TRIM(ELMO_LIST( ILIST )%DESC) // ' for mode ' //
     &                            aeromode(1)%suff
           
           ! Define ELMO Variables for all remaining modes
           DO IM = 2,N_MODE
             N = N + 1
             ELMO_TABLE( N ) = ELMO_TABLE( N-1 )
             ELMO_TABLE( N )%IM = IM
             ELMO_TABLE( N )%OUT_NAME = TRIM(ELMO_LIST( ILIST )%NAME)//
     &                                      '_' // aeromode( IM )%suff
             ELMO_TABLE( N )%VAR_TYPE = ETYPE_ELMO
             ELMO_TABLE( N )%DESC = TRIM(ELMO_LIST( ILIST )%DESC) // ' for mode ' //
     &                              aeromode(IM)%suff
           END DO
         END IF

#ifdef isam
         IF ( ELMO_LIST( ILIST )%L_ISAM ) THEN
           ! Set Source-Dependent Properties for the first ISAM source
           N = N + 1
           ELMO_TABLE( N ) = ELMO_TABLE( N-1 )
           ELMO_TABLE( N )%NAME = TRIM(ELMO_LIST( ILIST )%NAME) // '_ISAM'
           ELMO_TABLE( N )%OUT_NAME = TRIM(ELMO_LIST( ILIST )%NAME) // '_TAGS'
           ELMO_TABLE( N )%OUT_TYPE = M3REAL
           ELMO_TABLE( N )%VAR_TYPE = ETYPE_ISAM_ELMO
           ELMO_TABLE( N )%ID = ELMO_LIST( ILIST )%ID
           ELMO_TABLE( N )%SOURCE = 0
           ELMO_TABLE( N )%DESC = ELMO_LIST( ILIST )%DESC // 
     &           ' for the sum of all sources.'
           ELMO_TABLE( N )%CUNIT =  ELMO_LIST( ILIST )%UNIT
           CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                          ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
           ELMO_TABLE( N )%IUNIT = IUNIT
           
           ! Define ELMO Variables for all remaining ISAM sources
           DO ITAG = 2,NTAG_SA
             N = N + 1
             ELMO_TABLE( N ) = ELMO_TABLE( N-1 )
             ELMO_TABLE( N )%NAME = TRIM(ELMO_LIST( ILIST )%NAME)
     &                              // '_ISAM'
             ELMO_TABLE( N )%OUT_NAME = TRIM(ELMO_LIST( ILIST )%NAME) 
     &                              // '_' // TRIM(TAGNAME( ITAG ))
             ELMO_TABLE( N )%SOURCE = ITAG
             ELMO_TABLE( N )%DESC = ELMO_LIST( ILIST )%DESC // 
     &             ' for source ' // TRIM(TAGNAME( ITAG ))
           END DO
         END IF
#endif         

#ifdef sens
         IF ( ELMO_LIST( ILIST )%L_DDM3D ) THEN
           
           ! Define ELMO Variables for all remaining DDM3D sources
           DO NP = 1,NPMAX
             N = N + 1
             ELMO_TABLE( N ) = ELMO_TABLE( N-1 )
             ELMO_TABLE( N )%SOURCE = NP
             ELMO_TABLE( N )%NAME = TRIM(ELMO_LIST( ILIST )%NAME)
     &                              // '_DDM'
             ELMO_TABLE( N )%OUT_NAME = TRIm(ELMO_LIST( ILIST )%NAME)
     &                              // '_' // TRIM( SEN_PAR( NP ) )
             ELMO_TABLE( N )%OUT_TYPE = M3REAL
             ELMO_TABLE( N )%VAR_TYPE = ETYPE_DDM_ELMO
             ELMO_TABLE( N )%ID = ELMO_LIST( ILIST )%ID
             ELMO_TABLE( N )%DESC = 'Sensitivity of ' // ELMO_LIST( ILIST )%DESC // 
     &             ' to ' // TRIM( SEN_PAR( NP ) )
             ELMO_TABLE( N )%CUNIT =  ELMO_LIST( ILIST )%UNIT
             CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                            ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
             ELMO_TABLE( N )%IUNIT = IUNIT
           END DO
         END IF
#endif         
 
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
         CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                        ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
         ELMO_TABLE( N )%IUNIT = IUNIT
         ELMO_TABLE( N )%DESC = CGRID_CONC_DESC(S)
         ELMO_TABLE( N )%MW   = CGRID_MW( ICGRID )
      END DO    

      ! Load Deposition Variables into ELMO_Table
      ! Load Wet Deposition Fluxes
      IF ( L_ELMO_WDEP ) THEN
      DO IWDEP = 1,N_SPC_WDEP
        N = N + 1
        ELMO_TABLE( N )%NAME = 'WD_' // WDEP_SPC( IWDEP )
        ELMO_TABLE( N )%OUT_NAME = 'WD_' // WDEP_SPC( IWDEP )
        ELMO_TABLE( N )%OUT_TYPE = M3REAL
        ELMO_TABLE( N )%VAR_TYPE = ETYPE_WDEP
        ELMO_TABLE( N )%ID = IWDEP

        ICGRID = MAP_WDEPtoCGRID( IWDEP )
        ELMO_TABLE( N )%MW  = CGRID_MW( ICGRID )
        IF ( CGRID_MASK_NUM( ICGRID ) ) THEN
           ELMO_TABLE( N )%CUNIT = 'ha-1'
        ELSE IF ( CGRID_MASK_SRF( ICGRID ) ) THEN
           ELMO_TABLE( N )%CUNIT = 'm2 ha-1'
        ELSE
           ELMO_TABLE( N )%CUNIT = 'kg ha-1'
        END IF
        CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                       ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
        ELMO_TABLE( N )%IUNIT = IUNIT
        ELMO_TABLE( N )%DESC = 'Wet Deposition for Species '//
     &                         TRIM( WDEP_SPC( IWDEP ) )
      END DO
      END IF

      ! Load Dry Deposition Fluxes
      IF ( L_ELMO_DDEP ) THEN
      DO IDDEP = 1,N_SPC_DEPV
        N = N + 1
        ELMO_TABLE( N )%NAME = 'DD_' // DDEP_SPC( IDDEP )
        ELMO_TABLE( N )%OUT_NAME = 'DD_' // DDEP_SPC( IDDEP )
        ELMO_TABLE( N )%OUT_TYPE = M3REAL
        ELMO_TABLE( N )%VAR_TYPE = ETYPE_DDEP
        ELMO_TABLE( N )%ID = IDDEP

        ICGRID = MAP_DEPVtoCGRID( IDDEP )
        ELMO_TABLE( N )%MW  = CGRID_MW( ICGRID )
        IF ( CGRID_MASK_NUM( ICGRID ) ) THEN
           ELMO_TABLE( N )%CUNIT = 'ha-1'
        ELSE IF ( CGRID_MASK_SRF( ICGRID ) ) THEN
           ELMO_TABLE( N )%CUNIT = 'm2 ha-1'
        ELSE
           ELMO_TABLE( N )%CUNIT = 'kg ha-1'
        END IF
        CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                       ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
        ELMO_TABLE( N )%IUNIT = IUNIT
        ELMO_TABLE( N )%DESC = 'Dry Deposition Flux for Species '//
     &                         TRIM( DDEP_SPC( IDDEP ) )
      END DO
      END IF

#ifdef isam
      ! Load ISAM CGRID Variables to ELMO Table
      DO ISPEC = 1,NSPC_SA
        N = N + 1
        ELMO_TABLE( N )%NAME = TRIM(ISAM_SPEC( ISPEC,1 )) // '_ISAM'
        ELMO_TABLE( N )%OUT_NAME = TRIM(ISAM_SPEC( ISPEC,1 )) //'_TAGS'
        ELMO_TABLE( N )%OUT_TYPE = M3REAL
        ELMO_TABLE( N )%VAR_TYPE = ETYPE_ISAM
        ELMO_TABLE( N )%ID = ISPEC
        ELMO_TABLE( N )%SOURCE = 0

        ICGRID = MAP_SAtoCGR( ISPEC )
        ELMO_TABLE( N )%MW  = CGRID_MW( ICGRID )
        IF ( CGRID_MASK_AERO( ICGRID ) ) THEN
           ELMO_TABLE( N )%CUNIT = 'ug m-3'
        ELSE
           ELMO_TABLE( N )%CUNIT = 'ppmV'
        END IF
        CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                       ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
        ELMO_TABLE( N )%IUNIT = IUNIT
        ELMO_TABLE( N )%DESC = 'ISAM Concentration of Species '//
     &                  TRIM( ISAM_SPEC( ISPEC,1 ) ) //
     &                  ' from the sum of sources.'

        DO ITAG = 1,NTAG_SA
           N = N + 1
           ELMO_TABLE( N ) = ELMO_TABLE( N-1 )
           ELMO_TABLE( N )%NAME = TRIM(ISAM_SPEC( ISPEC,ITAG ))
     &                            // '_ISAM'
           ELMO_TABLE( N )%OUT_NAME = TRIM(ISAM_SPEC( ISPEC,ITAG ))
     &                            // '_' // TRIM(TAGNAME( ITAG )) 
           ELMO_TABLE( N )%SOURCE = ITAG
           ELMO_TABLE( N )%DESC = 'ISAM Concentration of Species '//
     &                     TRIM( ISAM_SPEC( ISPEC,ITAG ) ) //
     &                     ' from source ' // TRIM(TAGNAME( ITAG ))
        END DO
      END DO


      ! Load ISAM Wet Deposition Fluxes into ELMO Table
      IF ( L_ELMO_WDEP ) THEN
      DO IWDEP = 1,N_SPC_WDEP
        N = N + 1
        ELMO_TABLE( N )%NAME = 'WD_' //TRIM( WDEP_SPC( IWDEP ) ) // '_ISAM'
        ELMO_TABLE( N )%OUT_NAME = 'WD_' // TRIM( WDEP_SPC( IWDEP ) ) // '_TAGS'
        ELMO_TABLE( N )%OUT_TYPE = M3REAL
        ELMO_TABLE( N )%VAR_TYPE = ETYPE_ISAM_WDEP
        ELMO_TABLE( N )%ID = IWDEP
        ELMO_TABLE( N )%SOURCE = 0

        ICGRID = MAP_WDEPtoCGRID( IWDEP )
        ELMO_TABLE( N )%MW  = CGRID_MW( ICGRID )
        IF ( CGRID_MASK_NUM( ICGRID ) ) THEN
           ELMO_TABLE( N )%CUNIT = 'ha-1'
        ELSE IF ( CGRID_MASK_SRF( ICGRID ) ) THEN
           ELMO_TABLE( N )%CUNIT = 'm2 ha-1'
        ELSE
           ELMO_TABLE( N )%CUNIT = 'kg ha-1'
        END IF
        CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                       ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
        ELMO_TABLE( N )%IUNIT = IUNIT
        ELMO_TABLE( N )%DESC = 'Tagged Wet Deposition Flux of '//
     &     TRIM( WDEP_SPC( IWDEP ) ) //' from all sources.'

        DO ITAG = 1,NTAG_SA
           N = N + 1
           ELMO_TABLE( N ) = ELMO_TABLE( N-1 )
           ELMO_TABLE( N )%NAME = 'WD_' //TRIM( WDEP_SPC( IWDEP ) )
     &                            // '_ISAM'
           ELMO_TABLE( N )%OUT_NAME = 'WD_' // TRIM( WDEP_SPC( IWDEP ) )
     &                            // '_' // TRIM(TAGNAME( ITAG )) 
           ELMO_TABLE( N )%SOURCE = ITAG
           ELMO_TABLE( N )%DESC = 'Tagged Wet Deposition Flux of '//
     &        TRIM( WDEP_SPC( IWDEP ) ) //' from '// TRIM( TAGNAME( ITAG ) ) 
        END DO
      END DO
      END IF

      ! Load ISAM Dry Deposition Fluxes into ELMO Table
      IF ( L_ELMO_DDEP ) THEN
      DO IDDEP = 1,N_SPC_DEPV
        N = N + 1
        ELMO_TABLE( N )%NAME = 'DD_' //TRIM(DDEP_SPC( IDDEP )) // '_ISAM'
        ELMO_TABLE( N )%OUT_NAME = 'DD_' // TRIM(DDEP_SPC( IDDEP ))//'_TAGS'
        ELMO_TABLE( N )%OUT_TYPE = M3REAL
        ELMO_TABLE( N )%VAR_TYPE = ETYPE_ISAM_DDEP
        ELMO_TABLE( N )%ID = IDDEP
        ELMO_TABLE( N )%SOURCE = 0

        ICGRID = MAP_DEPVtoCGRID( IDDEP )
        ELMO_TABLE( N )%MW  = CGRID_MW( ICGRID )
        IF ( CGRID_MASK_NUM( ICGRID ) ) THEN
           ELMO_TABLE( N )%CUNIT = 'ha-1'
        ELSE IF ( CGRID_MASK_SRF( ICGRID ) ) THEN
           ELMO_TABLE( N )%CUNIT = 'm2 ha-1'
        ELSE
           ELMO_TABLE( N )%CUNIT = 'kg ha-1'
        END IF
        CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                       ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
        ELMO_TABLE( N )%IUNIT = IUNIT
        ELMO_TABLE( N )%DESC = 'Tagged Dry Deposition Flux of '//
     &     TRIM( DDEP_SPC( IDDEP ) ) //' from all sources.'

        DO ITAG = 1,NTAG_SA
           N = N + 1
           ELMO_TABLE( N ) = ELMO_TABLE( N-1 )
           ELMO_TABLE( N )%NAME = 'DD_' //TRIM(DDEP_SPC( IDDEP ))
     &                            // '_ISAM'
           ELMO_TABLE( N )%OUT_NAME = 'DD_' // TRIM(DDEP_SPC( IDDEP ))
     &                            // '_' // TRIM(TAGNAME( ITAG )) 
           ELMO_TABLE( N )%SOURCE = ITAG
           ELMO_TABLE( N )%DESC = 'Tagged Dry Deposition Flux of '//
     &        TRIM( DDEP_SPC( IDDEP ) ) //' from '//
     &        TRIM( TAGNAME( ITAG ) ) 
        END DO
      END DO
      END IF
#endif 

#ifdef sens
      ! Load DDM Variables to ELMO Table
      DO ICGRID = 1,N_CGRID_SPC
        DO NP = 1,NPMAX
           N = N + 1
           ELMO_TABLE( N )%NAME = TRIM( CGRID_NAME(ICGRID) ) 
     &                                // '_DDM'
           ELMO_TABLE( N )%OUT_NAME = TRIM( CGRID_NAME(ICGRID) ) 
     &                                // '_' // TRIM( SEN_PAR(NP) )
           ELMO_TABLE( N )%OUT_TYPE = M3REAL
           ELMO_TABLE( N )%VAR_TYPE = ETYPE_DDM
           ELMO_TABLE( N )%ID = ICGRID
           ELMO_TABLE( N )%SOURCE = NP
           ELMO_TABLE( N )%MW  = CGRID_MW( ICGRID )

           IF ( CGRID_MASK_NUM( ICGRID ) ) THEN
              ELMO_TABLE( N )%CUNIT = 'm-3'
           ELSE IF ( CGRID_MASK_SRF( ICGRID ) ) THEN
              ELMO_TABLE( N )%CUNIT = 'm2 m-3'
           ELSE IF ( CGRID_MASK_AERO( ICGRID ) ) THEN
              ELMO_TABLE( N )%CUNIT = 'ug m-3'
           ELSE
              ELMO_TABLE( N )%CUNIT = 'ppmV'
           END IF
           CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                          ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
           ELMO_TABLE( N )%IUNIT = IUNIT
           ELMO_TABLE( N )%DESC = 'Sensitivity of '//
     &        TRIM( CGRID_NAME( ICGRID ) ) //' to '//
     &        TRIM( SEN_PAR(NP) )

        END DO
      END DO

      ! Load DDM3D Wet Deposition Fluxes into ELMO Table
      IF ( L_ELMO_WDEP ) THEN
      DO IWDEP = 1,N_SPC_WDEP
        DO NP = 1,NPMAX
           N = N + 1
           ELMO_TABLE( N )%NAME = 'WD_' // TRIM( WDEP_SPC( IWDEP ) )
     &                     // '_DDM' 
           ELMO_TABLE( N )%OUT_NAME = 'WD_' // TRIM(WDEP_SPC( IWDEP ))
     &                     // '_' // TRIM( SEN_PAR(NP) )
           ELMO_TABLE( N )%OUT_TYPE = M3REAL
           ELMO_TABLE( N )%VAR_TYPE = ETYPE_DDM_WDEP
           ELMO_TABLE( N )%ID = IWDEP
           ELMO_TABLE( N )%SOURCE = NP

           ICGRID = MAP_WDEPtoCGRID( IWDEP )
           ELMO_TABLE( N )%MW  = CGRID_MW( ICGRID )
           IF ( CGRID_MASK_NUM( ICGRID ) ) THEN
              ELMO_TABLE( N )%CUNIT = 'ha-1'
           ELSE IF ( CGRID_MASK_SRF( ICGRID ) ) THEN
              ELMO_TABLE( N )%CUNIT = 'm2 ha-1'
           ELSE
              ELMO_TABLE( N )%CUNIT = 'kg ha-1'
           END IF
           CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                          ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
           ELMO_TABLE( N )%IUNIT = IUNIT
           ELMO_TABLE( N )%DESC = 'Sensitivity of '//
     &        TRIM( WDEP_SPC( IWDEP ) ) //' wet deposition to '//
     &        TRIM( SEN_PAR(NP) ) // ' from all cloud types'
        END DO
      END DO
      END IF

      ! Load DDM3D Dry Deposition Fluxes into ELMO Table
      IF ( L_ELMO_DDEP ) THEN
      DO IDDEP = 1,N_SPC_DEPV
        DO NP = 1,NPMAX
           N = N + 1
           ELMO_TABLE( N )%NAME = 'DD_' // TRIM(DDEP_SPC( IDDEP ))
     &                                // '_DDM'
           ELMO_TABLE( N )%OUT_NAME = 'DD_' // TRIM(DDEP_SPC( IDDEP ))
     &                                // '_' // TRIM( SEN_PAR(NP) )
           ELMO_TABLE( N )%OUT_TYPE = M3REAL
           ELMO_TABLE( N )%VAR_TYPE = ETYPE_DDM_DDEP
           ELMO_TABLE( N )%ID = IDDEP
           ELMO_TABLE( N )%SOURCE = NP

           ICGRID = MAP_DEPVtoCGRID( IDDEP )
           ELMO_TABLE( N )%MW  = CGRID_MW( ICGRID )
           IF ( CGRID_MASK_NUM( ICGRID ) ) THEN
              ELMO_TABLE( N )%CUNIT = 'ha-1'
           ELSE IF ( CGRID_MASK_SRF( ICGRID ) ) THEN
              ELMO_TABLE( N )%CUNIT = 'm2 ha-1'
           ELSE
              ELMO_TABLE( N )%CUNIT = 'kg ha-1'
           END IF
           CALL MAP_ELMO_UNITS( ELMO_TABLE(N)%CUNIT, IUNIT, 
     &                          ELMO_TABLE( N )%NAME, ELMO_TABLE( N )%VAR_TYPE )
           ELMO_TABLE( N )%IUNIT = IUNIT
           ELMO_TABLE( N )%DESC = 'Sensitivity of '//
     &        TRIM( DDEP_SPC( IDDEP ) ) //' dry deposition to '//
     &        TRIM( SEN_PAR(NP) ) // ' from all cloud types'
        END DO
      END DO
      END IF
#endif

      ! Summarize Total number of ELMO Table entries and subset Table to
      ! just the populated rows.
      N_ELMO_TABLE = N
      ELMO_TABLE = ELMO_TABLE( 1:N_ELMO_TABLE ) 

      ! Initialize all ELMO Variables as Detailed calculations (i.e.
      ! L_COMP = FALSE). If they appear in ELMO_MAP_VARIABLES, then
      ! their L_COMP attribute will be set to True.
      ELMO_TABLE(:)%L_COMP = .FALSE.
      ALLOCATE( ELMO_COMP( N_ELMO_TABLE ) )

      END SUBROUTINE BUILD_ELMO_TABLE

!-------------------------------------------------------------------------
      SUBROUTINE OPEN_ELMO ( JDATE, JTIME, TSTEP )
!-------------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE MIO_ASCII
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
        INTEGER :: MDATE, MTIME, IINST, IAVRG, J, ICOMP

#ifndef mpas
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
#endif
      ! Open Instantaneous ELMO File
      IF ( INST_ACTIVE ) THEN

#ifndef mpas
         ! Try to open existing file for update
         IF ( .NOT. OPEN3( CTM_ELMO_1, FSRDWR3, PNAME ) ) THEN

            ! Set output file characteristics based on COORD.EXT and 
            ! open the ELMO file
            SDATE3D = JDATE
            STIME3D = JTIME
            !CALL NEXTIME ( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour
         
            NLAYS3D = INST_LAYER_TOP - INST_LAYER_BOT + 1
            DO L = INST_LAYER_BOT, INST_LAYER_TOP + 1
               VGLVS3D( L ) = VGLVS_GD( L )
            END DO
         
            NVARS3D = N_ELMO_INST 
            VTYPE3D(1:NVARS3D) = ELMO_INST_PARS( 1:NVARS3D )%OUT_TYPE
            VNAME3D(1:NVARS3D) = ELMO_INST_PARS( 1:NVARS3D )%OUT_NAME
            UNITS3D(1:NVARS3D) = ELMO_INST_PARS( 1:NVARS3D )%CUNIT
            VDESC3D(1:NVARS3D) = ELMO_INST_PARS( 1:NVARS3D )%DESC
         
            FDESC3D( 1 ) = 'Variables Relevant for PM Formation and Distribution'
            DO L = 2, MXDESC3
               FDESC3D( L ) = ' '
            END DO
 
            ! Store MIO Metadata
            NDIMS3D( 1:NVARS3D ) = 4
            L_TSTEP( 1:NVARS3D ) = .True.
            L_LAY  ( 1:NVARS3D ) = .True.
            L_COL  ( 1:NVARS3D ) = .True.
            L_ROW  ( 1:NVARS3D ) = .True.
            L_VEXT ( 1:NVARS3D ) = .False.
           
            CALL LOAD_MIO_FILE ( 'CTM_ELMO_1', NLAYS3D, INST_LAYER_BOT, INST_LAYER_TOP,
     &           VNAME3D(1:NVARS3D), VTYPE3D(1:NVARS3D), UNITS3D(1:NVARS3D),
     &           VDESC3D(1:NVARS3D), NDIMS3D(1:NVARS3D), L_TSTEP(1:NVARS3D),
     &           L_LAY(1:NVARS3D),   L_COL(1:NVARS3D), L_ROW(1:NVARS3D),
     &           L_VEXT(1:NVARS3D) )
         
            ! Open the Instantaneous ELMO File
            IF ( .NOT. OPEN3( CTM_ELMO_1, FSNEW3, PNAME ) ) THEN
               XMSG = 'Could not create '// CTM_ELMO_1 // ' file'
               CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
            END IF

         END IF 
#endif
      END IF
 

      ! Open Average ELMO File
      IF ( AVRG_ACTIVE ) THEN
      

#ifndef mpas
         ! Try to open existing file for update
         IF ( .NOT. OPEN3( CTM_AELMO_1, FSRDWR3, PNAME ) ) THEN

            ! Get end time information from environment variable AVG_FILE_ENDTIME
            MDATE = JDATE; MTIME = JTIME
            IF ( END_TIME ) CALL NEXTIME ( MDATE, MTIME, TSTEP )

            ! Set output file characteristics based on COORD.EXT and 
            ! open the Average ELMO File
            SDATE3D = MDATE
            STIME3D = MTIME
         
            NLAYS3D = AVRG_LAYER_TOP - AVRG_LAYER_BOT + 1
            DO L = AVRG_LAYER_BOT, AVRG_LAYER_TOP + 1
               VGLVS3D( L ) = VGLVS_GD( L )
            END DO
         
            NVARS3D = N_ELMO_AVRG
            VTYPE3D(1:NVARS3D) = ELMO_AVRG_PARS( 1:NVARS3D )%OUT_TYPE
            VNAME3D(1:NVARS3D) = ELMO_AVRG_PARS( 1:NVARS3D )%OUT_NAME
            UNITS3D(1:NVARS3D) = ELMO_AVRG_PARS( 1:NVARS3D )%CUNIT
            VDESC3D(1:NVARS3D) = ELMO_AVRG_PARS( 1:NVARS3D )%DESC
         
            FDESC3D( 1 ) = 'Variables Relevant for PM Formation and Distribution'
            DO L = 2, MXDESC3
               FDESC3D( L ) = ' '
            END DO
 
            ! Store MIO Metadata
            NDIMS3D( 1:NVARS3D ) = 4
            L_TSTEP( 1:NVARS3D ) = .True.
            L_LAY  ( 1:NVARS3D ) = .True.
            L_COL  ( 1:NVARS3D ) = .True.
            L_ROW  ( 1:NVARS3D ) = .True.
            L_VEXT ( 1:NVARS3D ) = .False.
           
            CALL LOAD_MIO_FILE ( 'CTM_AELMO_1', NLAYS3D, AVRG_LAYER_BOT, AVRG_LAYER_TOP,
     &           VNAME3D(1:NVARS3D), VTYPE3D(1:NVARS3D), UNITS3D(1:NVARS3D),
     &           VDESC3D(1:NVARS3D), NDIMS3D(1:NVARS3D), L_TSTEP(1:NVARS3D),
     &           L_LAY(1:NVARS3D),   L_COL(1:NVARS3D), L_ROW(1:NVARS3D),
     &           L_VEXT(1:NVARS3D) )
         
            ! Open the Average ELMO File
            IF ( .NOT. OPEN3( CTM_AELMO_1, FSNEW3, PNAME ) ) THEN
               XMSG = 'Could not create '// CTM_AELMO_1 // ' file'
               CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
            END IF
 
         END IF
#endif
      END IF
 
      END SUBROUTINE OPEN_ELMO 

!-------------------------------------------------------------------------
      SUBROUTINE ELMO_WRITE_LOG ( )
!-------------------------------------------------------------------------
    
      USE RUNTIME_VARS, ONLY : LOGDEV

      IMPLICIT NONE

      INTEGER IINST, IAVRG, ICOMP, J, TABID, IFILE

      ! Write out all ELMO variables organized by output file
      WRITE( LOGDEV, '(/,/,5x,A)' ),'|> ELMO OUTPUT VARIABLE REPORT:'
      WRITE( LOGDEV, '(5x,A)'   ),'+=================================='
      WRITE( LOGDEV,'(7x,A,8(/,13x,A))' ),
     &                              'Note: these are populated using rules from the',
     &                              'Miscellaneous Control File supplied by the User. ',
     &                              'When variables are indented, they are components ',
     &                              'of the most recent non-indented variable. Becuase ',
     &                              'their output units should either match the output ',
     &                              'variable or be converted to its units, the third ',
     &                              'column reports any coefficients used when applying ',
     &                              'these components to the sum, excluding molecular ',
     &                              'weight, air density, etc.'


      ifile = 0

      !-------------------------------------------------------------------------
      ! Instantaneous File
      !--------------------------------------------------------------------------
      ! Header and File Metadata
      IF ( INST_ACTIVE ) THEN
         ifile = ifile + 1
         WRITE( LOGDEV, '(/,5x,A,A)'   ),'>',REPEAT('-',80 )
         WRITE( LOGDEV,'(5x,A,I2,A,A,/,5x,A,A,/,8x,A,I2,A,I2)' ),
     &       'Output File Num: "',ifile,'" | Temporality: ','Instantaneous',
     &       'Output Filename: ','CTM_ELMO_1',
     &       ' Bottom Layer: ',INST_LAYER_BOT,
     &       ' Top Layer: ',INST_LAYER_TOP 

         ! Print all variables on the file and their contents if they have
         ! components 
         WRITE( LOGDEV, '(/,8x,A,12x,A,12x,A,8x,A,/,10x,A,39x,A)' ),
     &       'Variable','Type','Unit','Description','Component','Coeff'
         WRITE( LOGDEV, '(8x,A,3x,A,2x,A,4x,A)' ),
     &       '-----------------','--------------','--------','--------------'

         DO IINST = 1,N_ELMO_INST 
            WRITE ( LOGDEV,'(8x,A16,4x,A14,2x,A8,4x,A)'),
     &           ELMO_INST_PARS( IINST )%OUT_NAME,
     &           CETYPE(ELMO_INST_PARS( IINST )%VAR_TYPE),
     &           ELMO_INST_PARS( IINST )%CUNIT,
     &           TRIM(ELMO_INST_PARS( IINST )%DESC)

            IF ( ELMO_INST_PARS( IINST )%L_COMP ) THEN
               J = MAP_INSTtoTABLE( IINST )
               DO ICOMP = 1,ELMO_COMP( J )%N_VARS
                  TABID = ELMO_COMP( J )%TABID( ICOMP )

                  WRITE( LOGDEV, '(8x,A2,A16,4x,A14,2x,ES9.2,4x,A)'),
     &                   '->',ELMO_TABLE( TABID )%OUT_NAME,
     &                   CETYPE(ELMO_TABLE( TABID )%VAR_TYPE),
     &                   ELMO_COMP( J )%COEFF( ICOMP ),
     &                   TRIM(ELMO_TABLE( TABID )%DESC)
               END DO
            END IF
            WRITE( LOGDEV, '')
         END DO
      END IF

      !-------------------------------------------------------------------------
      ! Average File
      !--------------------------------------------------------------------------
      ! Header and File Metadata
      IF ( AVRG_ACTIVE ) THEN
         IFILE = IFILE + 1
         WRITE( LOGDEV, '(/,5x,A,A)'   ),'>',REPEAT('-',80 )
         WRITE( LOGDEV,'(5x,A,I2,A,A,/,5x,A,A,/,8x,A,I2,A,I2)' ),
     &       'Output File Num: "',IFILE,'" | Temporality: ','Average',
     &       'Output Filename: ','CTM_AELMO_1',
     &       ' Bottom Layer: ',AVRG_LAYER_BOT,
     &       ' Top Layer: ',AVRG_LAYER_TOP 

         ! Print all variables on the file and their contents if they have
         ! components 
         WRITE( LOGDEV, '(/,8x,A,12x,A,12x,A,8x,A,/,10x,A,39x,A)' ),
     &       'Variable','Type','Unit','Description','Component','Coeff'
         WRITE( LOGDEV, '(8x,A,3x,A,2x,A,4x,A)' ),
     &       '-----------------','--------------','--------','--------------'

         DO IAVRG = 1,N_ELMO_AVRG 
            WRITE ( LOGDEV,'(8x,A16,4x,A14,2x,A8,4x,A)'),
     &              ELMO_AVRG_PARS( IAVRG )%OUT_NAME,
     &              CETYPE(ELMO_AVRG_PARS( IAVRG )%VAR_TYPE),
     &              ELMO_AVRG_PARS( IAVRG )%CUNIT,
     &              TRIM(ELMO_AVRG_PARS( IAVRG )%DESC)

            IF ( ELMO_AVRG_PARS( IAVRG )%L_COMP ) THEN
               J = MAP_AVRGtoTABLE( IAVRG )
               DO ICOMP = 1,ELMO_COMP( J )%N_VARS
                  TABID = ELMO_COMP( J )%TABID( ICOMP )

                  WRITE( LOGDEV, '(8x,A2,A16,4x,A14,2x,ES9.2,4x,A)'),
     &                   '->',ELMO_TABLE( TABID )%OUT_NAME,
     &                   CETYPE(ELMO_TABLE( TABID )%VAR_TYPE),
     &                   ELMO_COMP( J )%COEFF( ICOMP ),
     &                   TRIM(ELMO_TABLE( TABID )%DESC)
               END DO
            END IF
            WRITE( LOGDEV, '')
         END DO
      END IF

      END SUBROUTINE ELMO_WRITE_LOG

      END MODULE ELMO_INIT_DEFN
