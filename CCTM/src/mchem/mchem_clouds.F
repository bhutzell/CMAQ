
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


      MODULE MCHEM_CLOUDS

      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
      USE mchem_Precision, ONLY: dp

      IMPLICIT NONE

      INTEGER, PARAMETER  :: SPC_DIM = 1000
      INTEGER, SAVE                         :: N_CL_SPC               ! Number of cloud species per cloud
      INTEGER, SAVE                         :: N_CL_SPC_CHARGED       ! Number of charged cloud species per cloud
      INTEGER, PARAMETER                    :: N_IC = 2               ! Number of possible surrogate species.
                                                                      !   Currently this is limited to two species.
      INTEGER, PARAMETER                    :: CL_TYPES = 2           ! Number of cloud types
      CHARACTER( 2 ), DIMENSION( CL_TYPES ), SAVE :: CL_NAMES = ['RS','CV'] ! Name of cloud type (RS=Resolved, CV=Convective)
      INTEGER, PARAMETER                    :: IRS = 1                ! Index for resolved clouds
      INTEGER, PARAMETER                    :: ICV = 2                ! Index for convective clouds
      REAL(dp),PARAMETER                    :: LWC_LIMIT = 0.00001    ! LWC limit used for cloud chemistry

      TYPE SPECIES_DATA_CLOUDS
          CHARACTER( 16 ) :: SPECIES_NAME  ! CMAQ Species Name
          REAL            :: MOLWT         ! Molar weight of species
          REAL            :: CHARGE        ! Charge of species
          CHARACTER( 16 ) :: IC1_SURR      ! IC1 Surrogate
          REAL            :: IC1_FAC       ! IC1 Scale Factor
          CHARACTER( 16 ) :: IC1_RESET     ! IC1 reset needed?
          CHARACTER( 16 ) :: IC2_SURR      ! IC2 Surrogate
          REAL            :: IC2_FAC       ! IC2 Scale Factor
          CHARACTER( 16 ) :: IC2_RESET     ! IC2 reset needed?
          CHARACTER( 16 ) :: EV_SURR       ! EV Surrogate
          REAL            :: EV_FAC        ! EV Scale Factor
          CHARACTER( 16 ) :: WETSUB        ! If coarse mode aerosols will be affected by scavenging only
      END TYPE SPECIES_DATA_CLOUDS

      TYPE (SPECIES_DATA_CLOUDS), DIMENSION(SPC_DIM), SAVE :: CL_SPECIES_DATA

      TYPE MECH_SPECIES_CLOUDS
          CHARACTER( 19 ), DIMENSION( CL_TYPES ) :: NAME      ! MECH Species Name
          INTEGER        , DIMENSION( CL_TYPES ) :: KPP_INDEX ! MECH Species Index
          REAL                                   :: MOLWT     ! Molar weight of species
          REAL                                   :: CHARGE    ! Charge of species
          CHARACTER( 16 ), DIMENSION( N_IC )     :: IC_NAME   ! IC Surrogate Name
          INTEGER        , DIMENSION( N_IC )     :: IC_CGRID_INDEX  ! IC Surrogate Index
          REAL           , DIMENSION( N_IC )     :: IC_FAC    ! IC Scale Factor
          LOGICAL        , DIMENSION( N_IC )     :: IC_RESET  ! IC reset needed?
          REAL           , DIMENSION( N_IC )     :: IC_MW     ! IC molar weight
          CHARACTER( 2 )                         :: EV_TYPE   ! Evaporation species type
          CHARACTER( 16 )                        :: EV_NAME   ! EV Surrogate Name
          INTEGER                                :: EV_CGRID_INDEX  ! EV Surrogate Index
          REAL                                   :: EV_FAC    ! EV Scale Factor
          REAL                                   :: EV_MW     ! EV molar weight
          CHARACTER( 16 )                        :: WETSUB    ! If coarse mode aerosols will be affected by scavenging only
          INTEGER                                :: WETSUB_CGRID_INDEX  ! WETSUB Surrogate Index
      END TYPE MECH_SPECIES_CLOUDS

      TYPE (MECH_SPECIES_CLOUDS), ALLOCATABLE, SAVE :: MECH_SPC_CL(:)

      TYPE CHARGED_SPECIES_CLOUDS
          CHARACTER( 19 ), DIMENSION( CL_TYPES ) :: NAME      ! MECH Species Name
          INTEGER        , DIMENSION( CL_TYPES ) :: KPP_INDEX ! MECH Species Index
          REAL                                   :: CHARGE    ! Charge of species
      END TYPE CHARGED_SPECIES_CLOUDS

      TYPE (CHARGED_SPECIES_CLOUDS), ALLOCATABLE, SAVE :: CHARGED_SPC_CL(:)

C...Variables related to CGRID conversion
      INTEGER, ALLOCATABLE, SAVE :: QAE( : ) ! CGRID pointer to ug/m3 species
      INTEGER, ALLOCATABLE, SAVE :: NAE( : ) ! CGRID pointer to #/m3 species
      INTEGER, ALLOCATABLE, SAVE :: SAE( : ) ! CGRID pointer to m2/m3 species
      INTEGER, SAVE :: NNAE          ! number of #/m3 species
      INTEGER, SAVE :: NQAE          ! number of ug/m3 species
      INTEGER, SAVE :: NSAE          ! number of m2/m3 species

C...Variables related to scavenging
      INTEGER, ALLOCATABLE, SAVE :: WDEP_MAP( : ) ! wet deposition map to CGRID
      INTEGER, SAVE :: N_SPC_WDEP                 ! # of wet deposition species
      INTEGER, SAVE :: N_CGRID_SCAV               ! # of species scavenged
      REAL( 8 ), ALLOCATABLE   :: WCBAR_RS  ( :,:,: )   ! liq water content of resolved cloud (kg/m3)
      REAL( 8 ), ALLOCATABLE   :: WETFAC_RS ( :,:,: )   ! wet deposition rate coefficient resolved cloud
      REAL( 8 ), ALLOCATABLE   :: ALFA0_RS  ( :,:,: )   ! aitken mode number scavenging coef resolved cloud
      REAL( 8 ), ALLOCATABLE   :: ALFA3_RS  ( :,:,: )   ! aitken mode mass scavenging coef resolved cloud
      REAL( 8 ), ALLOCATABLE   :: PRATE1_RS ( :,:,: )   ! precipitation rate (mm/hr) resolved cloud
      REAL( 8 ), ALLOCATABLE   :: RSCAV_RS  ( :,:,:,: ) ! scavenging coefficient(1/s) resolved cloud
      REAL( 8 ), ALLOCATABLE   :: WCBAR_CV  ( :,:,: )   ! liq water content of convective cloud (kg/m3)
      REAL( 8 ), ALLOCATABLE   :: WETFAC_CV ( :,:,: )   ! wet deposition rate coefficient convective cloud
      REAL( 8 ), ALLOCATABLE   :: ALFA0_CV  ( :,:,: )   ! aitken mode number scavenging coef convective cloud
      REAL( 8 ), ALLOCATABLE   :: ALFA3_CV  ( :,:,: )   ! aitken mode mass scavenging coef convective cloud
      REAL( 8 ), ALLOCATABLE   :: PRATE1_CV ( :,:,: )   ! precipitation rate (mm/hr) convective cloud
      REAL( 8 ), ALLOCATABLE   :: RSCAV_CV  ( :,:,:,: ) ! scavenging coefficient(1/s) convective cloud
      REAL( 8 ), ALLOCATABLE   :: FRAC_CV   ( :,:,: )   ! convective cloud grid box fraction
      REAL( 8 ), ALLOCATABLE   :: WET_DEP   ( :,:,:,: ) ! wet deposition of current time step
      REAL( 8 ), ALLOCATABLE   :: AIRMASS   ( :,:,: )   ! total airmass (mol/m2) in cloudy air
      REAL( 8 ), ALLOCATABLE   :: WDEP_Hp   ( :,:,: )   ! wet deposition of H+
      REAL,    ALLOCATABLE, SAVE :: TWET_DEP( :,:,: )   ! total wet deposition for output
      LOGICAL, ALLOCATABLE, SAVE :: KPP_SCAV( : )       ! true if scavenging is performed in KPP
      LOGICAL, ALLOCATABLE, SAVE :: LSCAV( : )          ! scavenging logical for all CGRID species
      INTEGER, ALLOCATABLE, SAVE :: I_KPP_SCAV( : )     ! index for scavenging species
      LOGICAL, SAVE              :: LHp = .TRUE.        ! if H+ is in mechanism

C.. Cloud properties
      REAL, ALLOCATABLE, SAVE  :: RESTRANS( :,: )   ! resolved cloud transmissivity
      REAL, ALLOCATABLE, SAVE  :: SUBTRANS( :,:,: ) ! subgrid cloud transmissivity

C.. Related to metals
      REAL( 8 ), SAVE          :: SOIL_FE_FAC           ! Fe molar fraction of ASOIL
      REAL( 8 ), SAVE          :: CORS_FE_FAC           ! Fe molar fraction of ACORS
      REAL( 8 ), SAVE          :: SOIL_MN_FAC           ! Mn molar fraction of ASOIL
      REAL( 8 ), SAVE          :: CORS_MN_FAC           ! Fe molar fraction of ACORS
      REAL( 8 ), SAVE          :: SEAS_NA_FAC           ! Na molar fraction of ASEACAT
      REAL( 8 ), SAVE          :: SOIL_NA_FAC           ! Fe molar fraction of ASOIL
      REAL( 8 ), SAVE          :: CORS_NA_FAC           ! Fe molar fraction of ACORS
      REAL( 8 ), SAVE          :: SEAS_MG_FAC           ! Na molar fraction of ASEACAT
      REAL( 8 ), SAVE          :: SOIL_MG_FAC           ! Fe molar fraction of ASOIL
      REAL( 8 ), SAVE          :: CORS_MG_FAC           ! Fe molar fraction of ACORS
      REAL( 8 ), SAVE          :: SEAS_CA_FAC           ! Na molar fraction of ASEACAT
      REAL( 8 ), SAVE          :: SOIL_CA_FAC           ! Fe molar fraction of ASOIL
      REAL( 8 ), SAVE          :: CORS_CA_FAC           ! Fe molar fraction of ACORS
      REAL( 8 ), SAVE          :: SEAS_K_FAC            ! Na molar fraction of ASEACAT
      REAL( 8 ), SAVE          :: SOIL_K_FAC            ! Fe molar fraction of ASOIL
      REAL( 8 ), SAVE          :: CORS_K_FAC            ! Fe molar fraction of ACORS
      INTEGER, PARAMETER       :: INDEX_FE         = 1  ! Index FE
      INTEGER, PARAMETER       :: INDEX_MN         = 2  ! Index MN
      INTEGER, PARAMETER       :: INDEX_NA         = 3  ! Index NA
      INTEGER, PARAMETER       :: INDEX_MG         = 4  ! Index MG
      INTEGER, PARAMETER       :: INDEX_CA         = 5  ! Index CA
      INTEGER, PARAMETER       :: INDEX_K          = 6  ! Index K
      INTEGER, PARAMETER       :: INDEX_MAX_METALS = 6  ! Maximum number of indices for metals
      INTEGER, PARAMETER       :: METALS_NUM_IC    = 4  ! Maximum number metal IC surrogates

      TYPE MECH_SPECIES_METALS
          CHARACTER( 19 )                             :: NAME      ! MECH Species Name
          CHARACTER( 11 ), DIMENSION( CL_TYPES )      :: KPP_NAME  ! MECH Species Name
          INTEGER        , DIMENSION( CL_TYPES )      :: KPP_INDEX ! MECH Species Index
          CHARACTER( 16 ), DIMENSION( METALS_NUM_IC ) :: IC_NAME   ! IC Surrogate Name
          INTEGER        , DIMENSION( METALS_NUM_IC ) :: IC_CGRID_INDEX  ! IC Surrogate Index
          REAL( 8 )      , DIMENSION( METALS_NUM_IC ) :: IC_FAC    ! IC Scale Factor
      END TYPE MECH_SPECIES_METALS

      TYPE (MECH_SPECIES_METALS), SAVE :: MECH_METALS( INDEX_MAX_METALS )

      REAL( dp )               :: FE_III          ! Fe(3+) fraction of Fe total
      REAL( dp ), PARAMETER    :: MN_II  = 1.0D0  ! Mn(2+) fraction of Mn total
      REAL( dp ), PARAMETER    :: FE_SOL = 0.1D0  ! Fraction of soluble Fe
      REAL( dp ), PARAMETER    :: MN_SOL = 0.5D0  ! Fraction of soluble Mn

      CONTAINS

      SUBROUTINE ALLOC_METAL_DATA ( )

        USE AERO_DATA

        IMPLICIT NONE

!... Local variables
        INTEGER             :: I, J
        CHARACTER( 200 )    :: FMT      ! Format for log-file output

!... set MW ratios and speciation factors for molar concentrations of coarse
!... soluble aerosols
        SOIL_FE_FAC = ASOIL_FE_FAC * REAL( AEROSPC_MW( ASOIL_IDX ), 8 )
     &                / REAL( AEROSPC_MW( AFE_IDX ), 8 ) / ASOIL_RENORM
        CORS_FE_FAC = ACORS_FE_FAC * REAL( AEROSPC_MW( ACORS_IDX ), 8 )
     &                / REAL( AEROSPC_MW( AFE_IDX ), 8 ) / ACORSEM_RENORM

        SOIL_MN_FAC = ASOIL_MN_FAC * REAL( AEROSPC_MW( ASOIL_IDX ), 8 )
     &                / REAL( AEROSPC_MW( AMN_IDX ), 8 ) / ASOIL_RENORM
        CORS_MN_FAC = ACORS_MN_FAC * REAL( AEROSPC_MW( ACORS_IDX ), 8 )
     &                / REAL( AEROSPC_MW( AMN_IDX ), 8 ) / ACORSEM_RENORM

        SEAS_NA_FAC = ASCAT_NA_FAC * REAL( AEROSPC_MW( ASEACAT_IDX ), 8 )
     &                / REAL( AEROSPC_MW( ANA_IDX ), 8 )
        SOIL_NA_FAC = ASOIL_NA_FAC * REAL( AEROSPC_MW( ASOIL_IDX ), 8 )
     &                / REAL( AEROSPC_MW( ANA_IDX ), 8 ) / ASOIL_RENORM
        CORS_NA_FAC = ACORS_NA_FAC * REAL( AEROSPC_MW( ACORS_IDX ), 8 )
     &                / REAL( AEROSPC_MW( ANA_IDX ), 8 ) / ACORSEM_RENORM

        SEAS_MG_FAC = ASCAT_MG_FAC * REAL( AEROSPC_MW( ASEACAT_IDX ), 8 )
     &                / REAL( AEROSPC_MW( AMG_IDX ), 8 )
        SOIL_MG_FAC = ASOIL_MG_FAC * REAL( AEROSPC_MW( ASOIL_IDX ), 8 )
     &                / REAL( AEROSPC_MW( AMG_IDX ), 8 ) / ASOIL_RENORM
        CORS_MG_FAC = ACORS_MG_FAC * REAL( AEROSPC_MW( ACORS_IDX ), 8 )
     &                / REAL( AEROSPC_MW( AMG_IDX ), 8 ) / ACORSEM_RENORM

        SEAS_CA_FAC = ASCAT_CA_FAC * REAL( AEROSPC_MW( ASEACAT_IDX ), 8 )
     &                / REAL( AEROSPC_MW( ACA_IDX ), 8 )
        SOIL_CA_FAC = ASOIL_CA_FAC * REAL( AEROSPC_MW( ASOIL_IDX ), 8 )
     &                / REAL( AEROSPC_MW( ACA_IDX ), 8 ) / ASOIL_RENORM
        CORS_CA_FAC = ACORS_CA_FAC * REAL( AEROSPC_MW( ACORS_IDX ), 8 )
     &                / REAL( AEROSPC_MW( ACA_IDX ), 8 ) / ACORSEM_RENORM

        SEAS_K_FAC = ASCAT_K_FAC * REAL( AEROSPC_MW( ASEACAT_IDX ), 8 )
     &                / REAL( AEROSPC_MW( AK_IDX ), 8 )
        SOIL_K_FAC = ASOIL_K_FAC * REAL( AEROSPC_MW( ASOIL_IDX ), 8 )
     &                / REAL( AEROSPC_MW( AK_IDX ), 8 ) / ASOIL_RENORM
        CORS_K_FAC = ACORS_K_FAC * REAL( AEROSPC_MW( ACORS_IDX ), 8 )
     &                / REAL( AEROSPC_MW( AK_IDX ), 8 ) / ACORSEM_RENORM

C... Allocate metal info
        DO I = 1, INDEX_MAX_METALS
          MECH_METALS( I )%NAME      = ''
          DO J = 1, CL_TYPES
            MECH_METALS( I )%KPP_NAME( J )  = ''
            MECH_METALS( I )%KPP_INDEX( J ) = 0
          END DO
          DO J = 1, METALS_NUM_IC
            MECH_METALS( I )%IC_NAME( J )        = ''
            MECH_METALS( I )%IC_CGRID_INDEX( J ) = 0
            MECH_METALS( I )%IC_FAC( J )         = 1.
          END DO
        END DO

C... Allocate FE details
        I = INDEX_FE
        MECH_METALS( I )%NAME         = 'FEppp'
        MECH_METALS( I )%IC_NAME( 1 ) = 'AFEJ'
        MECH_METALS( I )%IC_NAME( 2 ) = 'ASOIL'
        MECH_METALS( I )%IC_FAC ( 2 ) = SOIL_FE_FAC
        MECH_METALS( I )%IC_NAME( 3 ) = 'ACORS'
        MECH_METALS( I )%IC_FAC ( 3 ) = CORS_FE_FAC

C... Allocate MN details
        I = INDEX_MN
        MECH_METALS( I )%NAME         = 'MNpp'
        MECH_METALS( I )%IC_NAME( 1 ) = 'AMNJ'
        MECH_METALS( I )%IC_NAME( 2 ) = 'ASOIL'
        MECH_METALS( I )%IC_FAC ( 2 ) = SOIL_MN_FAC
        MECH_METALS( I )%IC_NAME( 3 ) = 'ACORS'
        MECH_METALS( I )%IC_FAC ( 3 ) = CORS_MN_FAC

C... Allocate NA details
        I = INDEX_NA
        MECH_METALS( I )%NAME         = 'NAp'
        MECH_METALS( I )%IC_NAME( 1 ) = 'ANAJ'
        MECH_METALS( I )%IC_NAME( 2 ) = 'ASEACAT'
        MECH_METALS( I )%IC_FAC ( 2 ) = SEAS_NA_FAC
        MECH_METALS( I )%IC_NAME( 3 ) = 'ASOIL'
        MECH_METALS( I )%IC_FAC ( 3 ) = SOIL_NA_FAC
        MECH_METALS( I )%IC_NAME( 4 ) = 'ACORS'
        MECH_METALS( I )%IC_FAC ( 4 ) = CORS_NA_FAC

C... Allocate MG details
        I = INDEX_MG
        MECH_METALS( I )%NAME         = 'MGpp'
        MECH_METALS( I )%IC_NAME( 1 ) = 'AMGJ'
        MECH_METALS( I )%IC_NAME( 2 ) = 'ASEACAT'
        MECH_METALS( I )%IC_FAC ( 2 ) = SEAS_MG_FAC
        MECH_METALS( I )%IC_NAME( 3 ) = 'ASOIL'
        MECH_METALS( I )%IC_FAC ( 3 ) = SOIL_MG_FAC
        MECH_METALS( I )%IC_NAME( 4 ) = 'ACORS'
        MECH_METALS( I )%IC_FAC ( 4 ) = CORS_MG_FAC

C... Allocate CA details
        I = INDEX_CA
        MECH_METALS( I )%NAME         = 'CApp'
        MECH_METALS( I )%IC_NAME( 1 ) = 'ACAJ'
        MECH_METALS( I )%IC_NAME( 2 ) = 'ASEACAT'
        MECH_METALS( I )%IC_FAC ( 2 ) = SEAS_CA_FAC
        MECH_METALS( I )%IC_NAME( 3 ) = 'ASOIL'
        MECH_METALS( I )%IC_FAC ( 3 ) = SOIL_CA_FAC
        MECH_METALS( I )%IC_NAME( 4 ) = 'ACORS'
        MECH_METALS( I )%IC_FAC ( 4 ) = CORS_CA_FAC

C... Allocate K details
        I = INDEX_K
        MECH_METALS( I )%NAME         = 'Kp'
        MECH_METALS( I )%IC_NAME( 1 ) = 'AKJ'
        MECH_METALS( I )%IC_NAME( 2 ) = 'ASEACAT'
        MECH_METALS( I )%IC_FAC ( 2 ) = SEAS_K_FAC
        MECH_METALS( I )%IC_NAME( 3 ) = 'ASOIL'
        MECH_METALS( I )%IC_FAC ( 3 ) = SOIL_K_FAC
        MECH_METALS( I )%IC_NAME( 4 ) = 'ACORS'
        MECH_METALS( I )%IC_FAC ( 4 ) = CORS_K_FAC

C... Get KPP and CGRID index
        DO I = 1, INDEX_MAX_METALS
          DO J = 1, CL_TYPES
            MECH_METALS( I )%KPP_NAME( J )  = TRIM( MECH_METALS( I )%NAME ) // '_' // CL_NAMES( J )
            MECH_METALS( I )%KPP_INDEX( J ) = INDEX_KPP( MECH_METALS( I )%KPP_NAME( J ) )
          END DO
          DO J = 1, METALS_NUM_IC
            MECH_METALS( I )%IC_CGRID_INDEX( J ) = INDEX_CGRID( MECH_METALS( I )%IC_NAME( J ) )
          END DO
        END DO

C..   Report metal information to log-file
        WRITE(LOGDEV,"(A)") 'Metal species used in MCHEM:'
        FMT = "(A16,A11,X,A12,X,A11,X,A12,X,A8,X,A9,X,A7,X,A8,X,A9,X,A7,X,A8,X,A9,X,A7,X,A8,X,A9,X,A7)"
        WRITE(LOGDEV,FMT)   'NAME            ','KPP_NAME RS','KPP_INDEX RS','KPP_NAME CV','KPP_INDEX CV',
     &                      'IC1_NAME','IC1_INDEX','IC1_FAC',
     &                      'IC2_NAME','IC2_INDEX','IC2_FAC',
     &                      'IC3_NAME','IC3_INDEX','IC3_FAC',
     &                      'IC4_NAME','IC4_INDEX','IC4_FAC'
        FMT = "(A16,A11,X,I12,X,A11,X,I12,X,A8,X,I9,X,F7.4,X,A8,X,I9,X,F7.4,X,A8,X,I9,X,F7.4,X,A8,X,I9,X,F7.4)"
        DO I = 1, INDEX_MAX_METALS
          WRITE(LOGDEV,FMT) MECH_METALS( I )%NAME,
     &        MECH_METALS( I )%KPP_NAME( 1 ), MECH_METALS( I )%KPP_INDEX( 1 ),
     &        MECH_METALS( I )%KPP_NAME( 2 ), MECH_METALS( I )%KPP_INDEX( 2 ),
     &        MECH_METALS( I )%IC_NAME( 1 ), MECH_METALS( I )%IC_CGRID_INDEX( 1 ), MECH_METALS( I )%IC_FAC( 1 ),
     &        MECH_METALS( I )%IC_NAME( 2 ), MECH_METALS( I )%IC_CGRID_INDEX( 2 ), MECH_METALS( I )%IC_FAC( 2 ),
     &        MECH_METALS( I )%IC_NAME( 3 ), MECH_METALS( I )%IC_CGRID_INDEX( 3 ), MECH_METALS( I )%IC_FAC( 3 ),
     &        MECH_METALS( I )%IC_NAME( 4 ), MECH_METALS( I )%IC_CGRID_INDEX( 4 ), MECH_METALS( I )%IC_FAC( 4 )
        END DO

      END SUBROUTINE ALLOC_METAL_DATA

      SUBROUTINE ALLOC_AE_DATA ( )

        IMPLICIT NONE

        INTEGER       VAR              ! variable subscript indices

        IF ( N_AE_SPC .GT. 0 ) THEN
C...create aerosol species pointers to distinguish micro-grams / m**3
C...  # / m**3 (number density), and m**2 / m**3 (surface area) units

          ALLOCATE ( QAE( N_AE_SPC ),
     &               NAE( N_AE_SPC ),
     &               SAE( N_AE_SPC ) )

          NQAE = 0       ! number of ug/m3 species
          NNAE = 0       ! number of #/m3 species
          NSAE = 0       ! number of m2/m3 species

          DO VAR = 1, N_AE_SPC
            IF ( AE_SPC( VAR )( 1:3 ) .EQ. 'NUM' ) THEN
              NNAE = NNAE + 1
              NAE( NNAE ) = AE_STRT - 1 + VAR
            ELSE IF ( AE_SPC( VAR )( 1:3 ) .EQ. 'SRF' ) THEN
              NSAE = NSAE + 1
              SAE( NSAE ) = AE_STRT - 1 + VAR
            ELSE
              NQAE = NQAE + 1
              QAE( NQAE ) = AE_STRT - 1 + VAR
            END IF
          END DO

        END IF

      END SUBROUTINE ALLOC_AE_DATA

      SUBROUTINE ALLOC_WDEP_DATA ( JDATE, JTIME )

        USE GRID_CONF             ! horizontal & vertical domain specifications

        IMPLICIT NONE

        INTEGER, INTENT( IN )    :: JDATE            ! current model date, coded YYYYDDD
        INTEGER, INTENT( IN )    :: JTIME            ! current model time, coded HHMMSS

        INTEGER ALLOCSTAT       ! Allocate status code
        INTEGER       VAR       ! variable subscript indices
        INTEGER       SPC       ! species subscript indices
        INTEGER      STRT       ! starting position
        INTEGER      FINI       ! ending position
        INTEGER     INDXC       ! index cgrid
        INTEGER     INDXV       ! index var
        INTEGER     SHIFT

        CHARACTER(  16 ) :: PNAME = 'MCHEM_WDEP' ! Procedure name
        CHARACTER( 144 ) :: XMSG                 ! Message text
        CHARACTER( 200 ) :: FMT                  ! Format for log-file output


C...first check to make sure that some species in CGRID were specified
C...for output in the wet deposition array, otherwise notify the user
C...and return

        N_SPC_WDEP = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + N_TR_WDEP
        ALLOCATE ( WDEP_MAP( N_SPC_WDEP ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating WDEP_MAP'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
        IF ( N_SPC_WDEP .LE. 0 ) THEN

          XMSG = 'No species were specified for wet deposition ' //
     &           'tracking'
          CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )

          XMSG = 'ONLY CLOUD DIAGNOSTICS WILL BE WRITTEN TO THE ' //
     &           'WETDEP OUTPUT FILE!'
          CALL M3MESG ( XMSG )

        END IF

C...check to see if user wants to output extra diagnostic files

        SPC = 0
        STRT = 1
        FINI = N_GC_WDEP
        DO VAR = STRT, FINI
          SPC = SPC + 1
          WDEP_MAP( VAR ) = GC_STRT - 1 + GC_WDEP_MAP( SPC )
        END DO

        SPC = 0
        STRT = N_GC_WDEP + 1
        FINI = N_GC_WDEP + N_AE_WDEP
        DO VAR = STRT, FINI
          SPC = SPC + 1
          WDEP_MAP( VAR ) = AE_STRT - 1 + AE_WDEP_MAP( SPC )
        END DO

        SPC = 0
        STRT = N_GC_WDEP + N_AE_WDEP + 1
        FINI = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP
        DO VAR = STRT, FINI
          SPC = SPC + 1
          WDEP_MAP( VAR ) = NR_STRT - 1 + NR_WDEP_MAP( SPC )
        END DO

        SPC = 0
        STRT = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + 1
        FINI = N_GC_WDEP + N_AE_WDEP + N_NR_WDEP + N_TR_WDEP
        DO VAR = STRT, FINI
          SPC = SPC + 1
          WDEP_MAP( VAR ) = TR_STRT - 1 + TR_WDEP_MAP( SPC )
        END DO

C.. Allocate wet deposition arrays
        N_CGRID_SCAV = N_GC_SCAV + N_AE_SCAV + N_NR_SCAV + N_TR_SCAV

        ALLOCATE ( WCBAR_RS ( NCOLS, NROWS, NLAYS ),
     &             WETFAC_RS( NCOLS, NROWS, NLAYS ),
     &             ALFA0_RS ( NCOLS, NROWS, NLAYS ),
     &             ALFA3_RS ( NCOLS, NROWS, NLAYS ),
     &             PRATE1_RS( NCOLS, NROWS, NLAYS ),
     &             RSCAV_RS ( NCOLS, NROWS, NLAYS, N_CGRID_SPC ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = '*** Error allocating WCBAR_RS, WETFAC_RS, ALFA0_RS, ALFA3_RS, PRATE1_RS, and RSCAV_RS'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE ( WCBAR_CV ( NCOLS, NROWS, NLAYS ),
     &             WETFAC_CV( NCOLS, NROWS, NLAYS ),
     &             ALFA0_CV ( NCOLS, NROWS, NLAYS ),
     &             ALFA3_CV ( NCOLS, NROWS, NLAYS ),
     &             PRATE1_CV( NCOLS, NROWS, NLAYS ),
     &             RSCAV_CV ( NCOLS, NROWS, NLAYS, N_CGRID_SPC ),
     &             FRAC_CV  ( NCOLS, NROWS, NLAYS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = '*** Error allocating WCBAR_CV, WETFAC_CV, ALFA0_CV, ALFA3_CV, PRATE1_CV, RSCAV_CV, and FRAC_CV'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE ( KPP_SCAV( N_CGRID_SPC ),
     &             LSCAV   ( N_CGRID_SPC ),
     &             I_KPP_SCAV( N_CGRID_SPC ),
     &             WET_DEP ( NCOLS, NROWS, NLAYS, N_CGRID_SPC ),
     &             WDEP_Hp ( NCOLS, NROWS, NLAYS ),
     &             TWET_DEP( NCOLS, NROWS, N_SPC_WDEP + 1 ),
     &             AIRMASS ( NCOLS, NROWS, NLAYS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = '*** Error allocating KPP_SCAV, LSCAV, I_SCAV_KPP, WET_DEP, WDEP_Hp, TWET_DEP, and AIRMASS'
          CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

C...  initialize arrays
        KPP_SCAV   = .FALSE.
        LSCAV      = .FALSE.
        I_KPP_SCAV = 0
        TWET_DEP   = 0.0

        WRITE(LOGDEV,"(A)") 'Cloud species scavenging used in MCHEM:'
        FMT = "(A16,I12,X,L5,X,L8,X,I12)"
        WRITE(LOGDEV,"(A)") 'NAME             CGRID_INDEX LSCAV KPP_SCAV WD-KPP_INDEX'

C...  load the CGRID to scavenged species pointers for the gases
        SPC = 0
        SHIFT = 0
        DO VAR=1, N_GC_SCAV
          SPC = SPC + 1
          INDXV = GC_SCAV_MAP( VAR )
          INDXC = INDXV + SHIFT
          LSCAV( INDXC ) = .TRUE.
          IF ( INDEX_KPP ( GC_SPC( INDXV ) ) .NE. 0 ) THEN
            KPP_SCAV( INDXC )   = .TRUE.
            I_KPP_SCAV( INDXC ) = INDEX_KPP ( 'WD_'//TRIM( GC_SPC( INDXV ) ) )
          END IF
          WRITE(LOGDEV,FMT) GC_SPC( INDXV ), INDXC, LSCAV( INDXC ), KPP_SCAV( INDXC ), I_KPP_SCAV( INDXC )
        END DO

C...  load the CGRID to scavenged species pointers for the aerosols
        SHIFT = N_GC_SPCD
        DO VAR=1, N_AE_SCAV
          SPC = SPC + 1
          INDXV = AE_SCAV_MAP( VAR )
          INDXC = INDXV + SHIFT
          LSCAV( INDXC ) = .TRUE.
          IF ( INDEX_KPP ( AE_SPC( INDXV ) ) .NE. 0 ) THEN
            KPP_SCAV( INDXC )   = .TRUE.
            I_KPP_SCAV( INDXC ) = INDEX_KPP ( 'WD_'//TRIM( AE_SPC( INDXV ) ) )
          END IF
          WRITE(LOGDEV,FMT) AE_SPC( INDXV ), INDXC, LSCAV( INDXC ), KPP_SCAV( INDXC ), I_KPP_SCAV( INDXC )
        END DO

C...  load the CGRID to scavenged species pointers for the non-reactives
        SHIFT = N_GC_SPCD + N_AE_SPC
        DO VAR=1, N_NR_SCAV
          SPC = SPC + 1
          INDXV = NR_SCAV_MAP( VAR )
          INDXC = INDXV + SHIFT
          LSCAV( INDXC ) = .TRUE.
          IF ( INDEX_KPP ( NR_SPC( INDXV ) ) .NE. 0 ) THEN
            KPP_SCAV( INDXC )   = .TRUE.
            I_KPP_SCAV( INDXC ) = INDEX_KPP ( 'WD_'//TRIM( NR_SPC( INDXV ) ) )
          END IF
          WRITE(LOGDEV,FMT) NR_SPC( INDXV ), INDXC, LSCAV( INDXC ), KPP_SCAV( INDXC ), I_KPP_SCAV( INDXC )
        END DO

C...  load the CGRID to scavenged species pointers for the tracers
        SHIFT = N_GC_SPCD + N_AE_SPC + N_NR_SPC
        DO VAR=1, N_TR_SCAV
          SPC = SPC + 1
          INDXV = TR_SCAV_MAP( VAR )
          INDXC = INDXV + SHIFT
          LSCAV( INDXC ) = .TRUE.
          IF ( INDEX_KPP ( TR_SPC( INDXV ) ) .NE. 0 ) THEN
            KPP_SCAV( INDXC )   = .TRUE.
            I_KPP_SCAV( INDXC ) = INDEX_KPP ( 'WD_'//TRIM( TR_SPC( INDXV ) ) )
          END IF
          WRITE(LOGDEV,FMT) TR_SPC( INDXV ), INDXC, LSCAV( INDXC ), KPP_SCAV( INDXC ), I_KPP_SCAV( INDXC )
        END DO

C...  check if H+ is part of the mechanism
         IF ( ( INDEX_KPP ( 'WD_Hp' ) == 0 ) .AND. ( INDEX_KPP ( 'WD_HP' ) == 0 ) ) LHp = .FALSE.
         IF ( ( INDEX_KPP ( 'Hp_RS' ) == 0 ) .AND. ( INDEX_KPP ( 'HP_RS' ) == 0 ) ) LHp = .FALSE.
         IF ( ( INDEX_KPP ( 'Hp_CV' ) == 0 ) .AND. ( INDEX_KPP ( 'HP_CV' ) == 0 ) ) LHp = .FALSE.

         IF ( .NOT. LHp ) THEN
           XMSG = 'H+ is not part of the mechanism - ' //
     &            'scavenging rates will not be reported'
           CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
         END IF

C..Allocate cloud properties
         ALLOCATE ( RESTRANS ( NCOLS,NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating RESTRANS'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         RESTRANS = 1.0   ! array assignment

         ALLOCATE ( SUBTRANS ( NCOLS,NROWS,2 ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating SUBTRANS'
           CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         SUBTRANS = 1.0   ! array assignment

      END SUBROUTINE ALLOC_WDEP_DATA

      SUBROUTINE CONVERT_WDEP ( REMOV, REMOV_Hp, AIRMASS )

        USE GRID_CONF             ! horizontal & vertical domain specifications

        IMPLICIT NONE
        REAL(8), INTENT(INOUT)  :: REMOV( : ) ! wet deposition array
        REAL(8), INTENT(INOUT)  :: REMOV_Hp   ! wet deposition of H+
        REAL(8), INTENT(IN)     :: AIRMASS    ! air mass

        INTEGER       VAR       ! variable subscript indices
        INTEGER       SPC       ! species subscript indices
        INTEGER      STRT       ! starting position
        INTEGER      FINI       ! ending position

        REAL(8), PARAMETER :: GPKG = 1.0E+03  ! g/kg
        REAL(8), PARAMETER :: M2PHA = 1.0E+04 ! 1 hectare = 1.0e4 m**2
        REAL(8), PARAMETER :: M2PHA_OVER_GPKG = M2PHA / GPKG

C...convert from moles/m**2 to kg/m**2 and kg/m**2 to kg/hectare
C...  for gases
         SPC = 0
         STRT = GC_STRT
         FINI = GC_STRT - 1 + N_GC_SPC
         DO VAR = STRT, FINI
            SPC = SPC + 1
            REMOV( VAR ) = REMOV( VAR ) * GC_MOLWT( SPC )
     &                   * M2PHA_OVER_GPKG
     &                   * AIRMASS
         END DO

C...  for aerosols
         SPC = 0
         STRT = AE_STRT
         FINI = AE_STRT - 1 + N_AE_SPC
         DO VAR = STRT, FINI
            SPC = SPC + 1
            IF (( INDEX( AE_SPC( SPC ), 'NUM' ) .EQ. 0 ) .AND.
     &          ( INDEX( AE_SPC( SPC ), 'SRF' ) .EQ. 0 )) THEN
               REMOV( VAR ) = REMOV( VAR ) * AE_MOLWT( SPC )
     &                      * M2PHA_OVER_GPKG
     &                      * AIRMASS
            ELSE
               REMOV( VAR ) = REMOV( VAR ) * M2PHA
     &                      * AIRMASS
            END IF
         END DO

C...  for non-reactives
         SPC = 0
         STRT = NR_STRT
         FINI = NR_STRT - 1 + N_NR_SPC
         DO VAR = STRT, FINI
            SPC = SPC + 1
            REMOV( VAR ) = REMOV( VAR ) * NR_MOLWT( SPC )
     &                   * M2PHA_OVER_GPKG
     &                   * AIRMASS
         END DO

C...  for tracers
         SPC = 0
         STRT = TR_STRT
         FINI = TR_STRT - 1 + N_TR_SPC
         DO VAR = STRT, FINI
            SPC = SPC + 1
            REMOV( VAR ) = REMOV( VAR ) * TR_MOLWT( SPC )
     &                   * M2PHA_OVER_GPKG
     &                   * AIRMASS
         END DO

C...  for H+
         REMOV_Hp = REMOV_Hp * 1.0_dp * M2PHA_OVER_GPKG * AIRMASS

      END SUBROUTINE CONVERT_WDEP

      SUBROUTINE CL_SPCS_DEF ( )

        IMPLICIT NONE

        INTEGER, SAVE       :: DEV_CL_NML
        INTEGER             :: ICL, ICLT, ICLC             ! Loop indices
        CHARACTER( 200 )    :: FMT                         ! Format for log-file output
        CHARACTER( 16 )     :: CL_NML_NAME = 'CL_spec.nml' ! Name of cloud-nml file

        CL_SPECIES_DATA%SPECIES_NAME = ''
        CL_SPECIES_DATA%MOLWT        = 0.
        CL_SPECIES_DATA%CHARGE       = 0.
        CL_SPECIES_DATA%IC1_SURR     = ''
        CL_SPECIES_DATA%IC1_FAC      = 1.
        CL_SPECIES_DATA%IC1_RESET    = ''
        CL_SPECIES_DATA%IC2_SURR     = ''
        CL_SPECIES_DATA%IC2_FAC      = 1.
        CL_SPECIES_DATA%IC2_RESET    = ''
        CL_SPECIES_DATA%EV_SURR      = ''
        CL_SPECIES_DATA%EV_FAC       = 1.
        CL_SPECIES_DATA%WETSUB       = ''

        NAMELIST / CL_NML / CL_SPECIES_DATA

        DEV_CL_NML = JUNIT()
        OPEN( FILE = TRIM( CL_NML_NAME ), UNIT = DEV_CL_NML,
     &        STATUS = 'old', POSITION = 'rewind' )
        READ( NML = CL_NML, UNIT = DEV_CL_NML )

        ! Determine Number of Cloud Species
        N_CL_SPC = 0
        DO ICL = 1,SPC_DIM
           IF ( CL_SPECIES_DATA( ICL )%SPECIES_NAME .EQ. '' ) EXIT
           N_CL_SPC = ICL
        END DO

        ALLOCATE( MECH_SPC_CL( N_CL_SPC ) )
        DO ICL = 1,N_CL_SPC
           MECH_SPC_CL( ICL )%CHARGE  = 0.
           MECH_SPC_CL( ICL )%MOLWT   = 0.
           MECH_SPC_CL( ICL )%EV_TYPE = ''
           MECH_SPC_CL( ICL )%EV_NAME = ''
           MECH_SPC_CL( ICL )%EV_CGRID_INDEX = 0
           MECH_SPC_CL( ICL )%EV_FAC  = 0.
           MECH_SPC_CL( ICL )%EV_MW   = 0.
           MECH_SPC_CL( ICL )%WETSUB  = ''
           MECH_SPC_CL( ICL )%WETSUB_CGRID_INDEX = 0
           DO ICLT = 1,CL_TYPES
              MECH_SPC_CL( ICL )%NAME( ICLT )       = ''
              MECH_SPC_CL( ICL )%KPP_INDEX( ICLT )  = 0
           END DO
           DO ICLT = 1,N_IC
              MECH_SPC_CL( ICL )%IC_NAME( ICLT )  = ''
              MECH_SPC_CL( ICL )%IC_CGRID_INDEX( ICLT ) = 0
              MECH_SPC_CL( ICL )%IC_FAC( ICLT )   = 0.
              MECH_SPC_CL( ICL )%IC_RESET( ICLT ) = .FALSE.
              MECH_SPC_CL( ICL )%IC_MW( ICLT )    = 0.
           END DO
        END DO

        DO ICL = 1,N_CL_SPC
           DO ICLT = 1,CL_TYPES
              MECH_SPC_CL( ICL )%NAME( ICLT )    = TRIM(CL_SPECIES_DATA( ICL )%SPECIES_NAME) // '_' // CL_NAMES( ICLT )
              MECH_SPC_CL( ICL )%KPP_INDEX( ICLT ) = INDEX_KPP( MECH_SPC_CL( ICL )%NAME( ICLT ) )
           END DO
           MECH_SPC_CL( ICL )%CHARGE = CL_SPECIES_DATA( ICL )%CHARGE
           MECH_SPC_CL( ICL )%MOLWT  = CL_SPECIES_DATA( ICL )%MOLWT
           IF ( TRIM(CL_SPECIES_DATA( ICL )%IC1_SURR) .NE. '' ) THEN
              MECH_SPC_CL( ICL )%IC_NAME( 1 )         = CL_SPECIES_DATA( ICL )%IC1_SURR
              MECH_SPC_CL( ICL )%IC_FAC ( 1 )         = CL_SPECIES_DATA( ICL )%IC1_FAC
              MECH_SPC_CL( ICL )%IC_CGRID_INDEX ( 1 ) = INDEX_CGRID( MECH_SPC_CL( ICL )%IC_NAME( 1 ) )
              MECH_SPC_CL( ICL )%IC_RESET ( 1 )       = GET_LOGICAL_NML( CL_SPECIES_DATA( ICL )%IC1_RESET )
              MECH_SPC_CL( ICL )%IC_MW  ( 1 )         = CGRID_MW( MECH_SPC_CL( ICL )%IC_CGRID_INDEX( 1 ) )
           END IF
           IF ( TRIM(CL_SPECIES_DATA( ICL )%IC2_SURR) .NE. '' ) THEN
              MECH_SPC_CL( ICL )%IC_NAME( 2 )         = CL_SPECIES_DATA( ICL )%IC2_SURR
              MECH_SPC_CL( ICL )%IC_FAC ( 2 )         = CL_SPECIES_DATA( ICL )%IC2_FAC
              MECH_SPC_CL( ICL )%IC_CGRID_INDEX ( 2 ) = INDEX_CGRID( MECH_SPC_CL( ICL )%IC_NAME( 2 ) )
              MECH_SPC_CL( ICL )%IC_RESET ( 2 )       = GET_LOGICAL_NML( CL_SPECIES_DATA( ICL )%IC2_RESET )
              MECH_SPC_CL( ICL )%IC_MW  ( 2 )         = CGRID_MW( MECH_SPC_CL( ICL )%IC_CGRID_INDEX( 2 ) )
           END IF
           IF ( TRIM(CL_SPECIES_DATA( ICL )%EV_SURR) .NE. '' ) THEN
              MECH_SPC_CL( ICL )%EV_NAME        = CL_SPECIES_DATA( ICL )%EV_SURR
              MECH_SPC_CL( ICL )%EV_CGRID_INDEX = INDEX_CGRID( MECH_SPC_CL( ICL )%EV_NAME )
              MECH_SPC_CL( ICL )%EV_TYPE        = SPC_TYPE( MECH_SPC_CL( ICL )%EV_CGRID_INDEX )
              MECH_SPC_CL( ICL )%EV_FAC         = CL_SPECIES_DATA( ICL )%EV_FAC
              MECH_SPC_CL( ICL )%EV_MW          = CGRID_MW( MECH_SPC_CL( ICL )%EV_CGRID_INDEX )
           END IF
           IF ( TRIM(CL_SPECIES_DATA( ICL )%WETSUB) .NE. '' ) THEN
              MECH_SPC_CL( ICL )%WETSUB             = CL_SPECIES_DATA( ICL )%WETSUB
              MECH_SPC_CL( ICL )%WETSUB_CGRID_INDEX = INDEX_CGRID( MECH_SPC_CL( ICL )%WETSUB )
           END IF
        END DO

C..   Report cloud species definitions to log-file
        WRITE(LOGDEV,"(A)") 'Cloud species definition used in MCHEM:'
        FMT = "(A16,A6,X,A6,X,A9,X,A8,X,A9,X,A7,X,A9,X,A8,X,A9,X,A7,X,A9,X,A8,X,A8,X,A7,X,A6,X,A8,X,A12)"
        WRITE(LOGDEV,FMT)   'NAME            ','MOLWT','CHARGE','KPP_INDEX',
     &                      'IC1_NAME','IC1_INDEX','IC1_FAC','IC1_RESET',
     &                      'IC2_NAME','IC2_INDEX','IC2_FAC','IC2_RESET',
     &                      'EV_NAME ','EV_INDEX','EV_TYPE','EV_FAC',
     &                      'WETSUB  ','WETSUB_INDEX'
        FMT = "(A16,F6.1,X,F6.1,X,I9,X,A8,X,I9,X,F7.1,X,L9,X,A8,X,I9,X,F7.1,X,L9,X,A8,X,I8,X,A7,X,F6.1,X,A8,X,I12)"
        DO ICL = 1,N_CL_SPC
           DO ICLT = 1,CL_TYPES
              WRITE(LOGDEV,FMT) MECH_SPC_CL( ICL )%NAME( ICLT ), MECH_SPC_CL( ICL )%MOLWT,
     &                          MECH_SPC_CL( ICL )%CHARGE, MECH_SPC_CL( ICL )%KPP_INDEX( ICLT ),
     &                          MECH_SPC_CL( ICL )%IC_NAME( 1 ), MECH_SPC_CL( ICL )%IC_CGRID_INDEX ( 1 ),
     &                          MECH_SPC_CL( ICL )%IC_FAC ( 1 ), MECH_SPC_CL( ICL )%IC_RESET ( 1 ),
     &                          MECH_SPC_CL( ICL )%IC_NAME( 2 ), MECH_SPC_CL( ICL )%IC_CGRID_INDEX ( 2 ),
     &                          MECH_SPC_CL( ICL )%IC_FAC ( 2 ), MECH_SPC_CL( ICL )%IC_RESET ( 2 ),
     &                          MECH_SPC_CL( ICL )%EV_NAME, MECH_SPC_CL( ICL )%EV_CGRID_INDEX,
     &                          MECH_SPC_CL( ICL )%EV_TYPE, MECH_SPC_CL( ICL )%EV_FAC,
     &                          MECH_SPC_CL( ICL )%WETSUB, MECH_SPC_CL( ICL )%WETSUB_CGRID_INDEX
           END DO
        END DO

C..   Find all charged species and allocate array
        N_CL_SPC_CHARGED = 0
        DO ICL = 1,N_CL_SPC
           IF ( MECH_SPC_CL( ICL )%CHARGE .NE. 0. ) THEN
             N_CL_SPC_CHARGED = N_CL_SPC_CHARGED + 1
           END IF
        END DO
        ALLOCATE( CHARGED_SPC_CL( N_CL_SPC_CHARGED ) )

C..   Fill charged species array
        ICLC = 0
        DO ICL = 1,N_CL_SPC
           IF ( MECH_SPC_CL( ICL )%CHARGE .NE. 0. ) THEN
              ICLC = ICLC + 1
              DO ICLT = 1,CL_TYPES
                 CHARGED_SPC_CL( ICLC )%NAME( ICLT )      = MECH_SPC_CL( ICL )%NAME( ICLT )
                 CHARGED_SPC_CL( ICLC )%KPP_INDEX( ICLT ) = MECH_SPC_CL( ICL )%KPP_INDEX( ICLT )
              END DO
              CHARGED_SPC_CL( ICLC )%CHARGE = MECH_SPC_CL( ICL )%CHARGE
           END IF
        END DO

      END SUBROUTINE CL_SPCS_DEF

      SUBROUTINE INIT_CLOUDS ( CONC, C, LWC_RS, LWC_CV, PHI2_RS, PHI2_CV, FRAC_CV_IN )

        USE mchem_Parameters, ONLY: IND_Hp_RS, IND_OHm_RS, IND_O2_RS,
     &                              IND_Hp_CV, IND_OHm_CV, IND_O2_CV,
     &                              IND_FEppp_RS, IND_FEppp_CV,
     &                              IND_MNpp_RS, IND_MNpp_CV
        USE mchem_Global,     ONLY: TEMP, PRESS, LSUNLIGHT

        IMPLICIT NONE
        REAL,     INTENT ( INOUT ) :: CONC( N_CGRID_SPC )    !  species concentration in cell
        REAL(dp), INTENT ( INOUT ) :: C( N_CGRID_SPC )       !  KPP species concentration in cell
        REAL(dp), INTENT( IN ) :: LWC_RS, LWC_CV, PHI2_RS, PHI2_CV, FRAC_CV_IN
        INTEGER                :: ICL, ICLT, ICGRID, IKPP_RS, IKPP_CV
        INTEGER                :: I_METAL, I_IC_METAL
        REAL(dp)               :: FRAC_RS, FRAC_CV, LWC_SUM, SCAL, SMW, TCLC
        REAL, PARAMETER        :: ZERO = 1.0E-30

        REAL(dp)               :: SUM_CHARGE, DELINVT, Kw, CHGBAL
        INTEGER, DIMENSION( CL_TYPES )  :: IHp  = [IND_Hp_RS,IND_Hp_CV]
        INTEGER, DIMENSION( CL_TYPES )  :: IOHm = [IND_OHm_RS,IND_OHm_CV]
        REAL(dp), DIMENSION( CL_TYPES ) :: PHI2
        INTEGER                :: IKPP

C.. Initialise cloud tracer based on convective fraction
        FRAC_RS = 0.0D0
        FRAC_CV = 0.0D0
        IF ( LWC_CV .GT. LWC_LIMIT ) FRAC_CV = FRAC_CV_IN
        IF ( LWC_RS .GT. LWC_LIMIT ) FRAC_RS = 1.0D0 - FRAC_CV

        DO ICL = 1,N_CL_SPC
           IF ( ( TRIM(MECH_SPC_CL( ICL )%IC_NAME( 1 )) .NE. '' ) .OR.
     &          ( TRIM(MECH_SPC_CL( ICL )%IC_NAME( 2 )) .NE. '' ) ) THEN
              IKPP_RS = MECH_SPC_CL( ICL )%KPP_INDEX( IRS )
              IKPP_CV = MECH_SPC_CL( ICL )%KPP_INDEX( ICV )
              DO ICLT = 1,N_IC
                 ICGRID  = MECH_SPC_CL( ICL )%IC_CGRID_INDEX( ICLT )
                 IF ( ICGRID .NE. 0 ) THEN
                    SCAL = REAL( MECH_SPC_CL( ICL )%IC_FAC( ICLT ), dp )                           ! Scale by IC fraction
                    SMW  = REAL( MECH_SPC_CL( ICL )%IC_MW( ICLT ) / MECH_SPC_CL( ICL )%MOLWT, dp ) ! Scale by molar weight
                    TCLC = REAL( CONC( ICGRID ), dp ) * SCAL                                       ! Total concentration that can partition into clouds
                    IF ( FRAC_RS .GT. 0.0 ) C( IKPP_RS ) = C( IKPP_RS ) + TCLC * FRAC_RS * SMW
                    IF ( FRAC_CV .GT. 0.0 ) C( IKPP_CV ) = C( IKPP_CV ) + TCLC * FRAC_CV * SMW
                    IF ( MECH_SPC_CL( ICL )%IC_RESET ( ICLT ) )
     &                   CONC( ICGRID ) = MAX( CONC( ICGRID ) -
     &                   REAL( TCLC * ( FRAC_RS  + FRAC_CV ), 4 ), ZERO ) ! In this scaling molar weight does not need to be taken into account
                 END IF
              END DO
           END IF
        END DO

C.. Initialise metals
        DO I_METAL = 1, INDEX_MAX_METALS
          DO ICL  = 1, CL_TYPES
            IKPP = MECH_METALS( I_METAL )%KPP_INDEX( ICL )
            IF ( ICL .EQ. 1 ) SCAL = FRAC_RS
            IF ( ICL .EQ. 2 ) SCAL = FRAC_CV
            IF ( SCAL .NE. 0. ) THEN
              DO I_IC_METAL = 1, METALS_NUM_IC
                IF ( MECH_METALS( I_METAL )%IC_CGRID_INDEX( I_IC_METAL ) .NE. 0 ) THEN
                  ICGRID = MECH_METALS( I_METAL )%IC_CGRID_INDEX( I_IC_METAL )
                  SMW    = REAL( MECH_METALS( I_METAL )%IC_FAC( I_IC_METAL ), dp )
                  C( IKPP ) = C( IKPP ) + REAL( CONC( ICGRID ) , dp )
     &                                  * SMW
     &                                  * SCAL
                END IF
              END DO
            END IF
          END DO
        END DO
C.. Scale metals (FE & MN)
        IF ( LSUNLIGHT ) THEN
          FE_III = 0.1D0  ! Day time, GS 01July2011
        ELSE
          FE_III = 0.9D0  ! Night time, GS 01July2011
        END IF
        C( IND_FEppp_RS ) = C( IND_FEppp_RS ) * FE_SOL * FE_III
        C( IND_FEppp_CV ) = C( IND_FEppp_CV ) * FE_SOL * FE_III
        C( IND_MNpp_RS )  = C( IND_MNpp_RS )  * MN_SOL * MN_II
        C( IND_MNpp_CV )  = C( IND_MNpp_CV )  * MN_SOL * MN_II

C.. Initialise Hp and OHm
        PHI2( 1 ) = PHI2_RS
        PHI2( 2 ) = PHI2_CV
        DO ICLT = 1,CL_TYPES
           IF ( PHI2( ICLT ) .GT. 0. ) THEN
           SUM_CHARGE = 0.
           DO ICL = 1,N_CL_SPC
              IF ( MECH_SPC_CL( ICL )%CHARGE .NE. 0.0 ) THEN
                 IKPP = MECH_SPC_CL( ICL )%KPP_INDEX( ICLT )
                 SUM_CHARGE = SUM_CHARGE + C( IKPP ) * REAL( MECH_SPC_CL( ICL )%CHARGE, dp )
              END IF
           END DO ! N_CL_SPC
           DELINVT = ( 298.D0 - TEMP ) / ( 298.D0 * TEMP )
           Kw = 1.00D-14 * EXP( -6.955D+03 * DELINVT )  ! includes conc of H2O
           CHGBAL = SUM_CHARGE * PHI2( ICLT )
           C( IOHm( ICLT ) ) = ( CHGBAL + SQRT( CHGBAL * CHGBAL + 4.D0 * Kw ) ) * 0.5D0
           C( IHp ( ICLT ) ) = Kw / C( IOHm( ICLT ) )
           IF ( -LOG10( C( IHp ( ICLT ) ) ) .LT. .0 ) WRITE(*,*) -LOG10( C( IHp ( ICLT ) ) )
           C( IOHm( ICLT ) ) = MAX( C( IOHm( ICLT ) ) / PHI2( ICLT ), REAL( ZERO, dp ) ) !Convert to molec/cm3
           C( IHp ( ICLT ) ) = MAX( C( IHp ( ICLT ) ) / PHI2( ICLT ), REAL( ZERO, dp ) ) !Convert to molec/cm3
           C( IND_O2_RS ) = ( 0.21 * PRESS * 1.3D-3 * EXP( 1500.D0 * DELINVT ) ) / PHI2( ICLT )
           C( IND_O2_CV ) = ( 0.21 * PRESS * 1.3D-3 * EXP( 1500.D0 * DELINVT ) ) / PHI2( ICLT )
           END IF
        END DO ! CL_TYPES

      END SUBROUTINE INIT_CLOUDS

      SUBROUTINE EVAP_CLOUDS ( CONC, C, DEP, WFAC_RS, WFAC_CV, TSTEP )

        IMPLICIT NONE
        REAL, INTENT ( INOUT )     :: CONC( N_CGRID_SPC )    !  species concentration in cell
        REAL(dp), INTENT ( INOUT ) :: C( : )                 !  KPP species concentration in cell
        REAL(8),  INTENT ( INOUT ) :: DEP( N_CGRID_SPC )     !  wet deposition in cell
        REAL(dp), INTENT( IN ) :: WFAC_RS, WFAC_CV, TSTEP
        INTEGER                :: ICL, ICLW, ICGRID, IKPP_RS, IKPP_CV
        REAL, PARAMETER        :: ZERO = 1.0E-30
        REAL(dp)               :: SCAL, CONC_TEMP, EXPWDEP
        INTRINSIC DEXP

C.. Evaporate cloud tracer
        DO ICL = 1,N_CL_SPC
           IF ( TRIM(MECH_SPC_CL( ICL )%EV_NAME) .NE. '' ) THEN
              IKPP_RS = MECH_SPC_CL( ICL )%KPP_INDEX( IRS )
              IKPP_CV = MECH_SPC_CL( ICL )%KPP_INDEX( ICV )
              ICGRID  = MECH_SPC_CL( ICL )%EV_CGRID_INDEX
              SCAL = REAL( MECH_SPC_CL( ICL )%EV_FAC, dp )
              SCAL = SCAL * REAL( ( MECH_SPC_CL( ICL )%MOLWT / MECH_SPC_CL( ICL )%EV_MW ), dp ) ! Scale by molar weight
              CONC_TEMP = ( C( IKPP_RS ) + C( IKPP_CV ) ) * SCAL
              CONC( ICGRID ) = MAX( CONC( ICGRID ) +
     &                 ( REAL( CONC_TEMP, 4 ) ), ZERO )
              C( IKPP_RS ) = REAL( ZERO, dp ) ! reseting these fields should not be necessary
              C( IKPP_CV ) = REAL( ZERO, dp ) ! reseting these fields should not be necessary
           END IF
        END DO

        DO ICL = 1,N_CL_SPC
C.. Coarse species are only affected by scavenging. Thus subtract coarse aerosol for now
C.. This code needs modifications once we take scavenging into account
              IF ( TRIM(MECH_SPC_CL( ICL )%WETSUB) .NE. '' ) THEN
                 ICLW = MECH_SPC_CL( ICL )%WETSUB_CGRID_INDEX
                 ICGRID  = MECH_SPC_CL( ICL )%EV_CGRID_INDEX
                 EXPWDEP = DEXP( - ( WFAC_RS + WFAC_CV ) * TSTEP )
                 DEP ( ICLW ) = REAL( CONC( ICLW ) * ( 1.0 - EXPWDEP ), 8 )
                 CONC( ICLW ) = CONC( ICLW ) - REAL( DEP( ICLW ), 4 )
                 CONC( ICGRID ) = MAX( CONC( ICGRID ) - CONC( ICLW ), ZERO )
              END IF
C.. The code above needs modifications once we take scavenging into account
        END DO

      END SUBROUTINE EVAP_CLOUDS

      SUBROUTINE CONVERT_FORWARD ( CGRID, DENS )

        USE GRID_CONF             ! horizontal & vertical domain specifications

        IMPLICIT NONE
        REAL,  POINTER         :: CGRID( :,:,:,: ) ! concentrations
        REAL,  INTENT( IN )    :: DENS( :,:,: )    ! density

        INTEGER       LAY              ! layer subscript indices
        INTEGER       COL              ! column subscript indices
        INTEGER       ROW              ! row subscript indices
        INTEGER       SPC              ! species subscript indices
        INTEGER       STRT             ! starting position
        INTEGER       FINI             ! ending position
        INTEGER       VAR              ! variable subscript indices
        REAL FAC                       ! temp conversion factor
        REAL CCMIN
        REAL, PARAMETER :: MWAIR = 28.9628
        REAL, PARAMETER :: CNV1 = MWAIR * 1.0E-9
        REAL, PARAMETER :: CNV1I = 1.0 / CNV1
        REAL, PARAMETER :: CNV2 = MWAIR * 1.0E-3
        REAL, PARAMETER :: CNV2I = 1.0 / CNV2
        REAL, PARAMETER :: CNV3 = CNV2                  ! -> #/mol
        REAL, PARAMETER :: CNV3I = 1.0 / CNV3

C...convert units on cgrid to molar mixing ratio (by volume)
C...  determine no. of moles of gas per moles of air by volume

        STRT = GC_STRT
        FINI = GC_STRT + N_GC_SPC - 1
        DO SPC = STRT, FINI
          DO LAY = 1, NLAYS
            DO ROW = 1, NROWS
              DO COL = 1, NCOLS
                CGRID( COL, ROW, LAY, SPC ) = MAX( CGRID( COL, ROW, LAY, SPC )
     &                                      * 1.0E-6,  1.0E-36 )
              END DO
            END DO
          END DO
        END DO

C...for aerosol mass concentration
C...  convert to moles of aerosol per mole of air by volume

        STRT = 1
        FINI = NQAE
        DO VAR = STRT, FINI
          SPC = QAE( VAR )
          DO LAY = 1, NLAYS
            DO ROW = 1, NROWS
              DO COL = 1, NCOLS
                FAC = CNV1 / DENS( COL, ROW, LAY ) / AE_MOLWT( SPC - AE_STRT + 1 )
                CCMIN = MAX( CGRID( COL, ROW, LAY, SPC ), 1.0E-30/FAC )
                CGRID( COL, ROW, LAY, SPC ) = FAC * CCMIN
              END DO
            END DO
          END DO
        END DO

C...for aerosol number concentration
C...    convert to # per mole of air by volume

        STRT = 1
        FINI = NNAE
        DO VAR = STRT, FINI
          SPC = NAE( VAR )
          DO LAY = 1, NLAYS
            DO ROW = 1, NROWS
              DO COL = 1, NCOLS
                FAC = CNV3 / DENS( COL, ROW, LAY )
                CCMIN = MAX( CGRID( COL, ROW, LAY, SPC ), 1.0E-30/FAC )
                CGRID( COL, ROW, LAY, SPC ) = FAC * CCMIN
              END DO
            END DO
          END DO
        END DO

C...for aerosol surface area
C...    convert to m2 per mole of air by volume

        STRT = 1
        FINI = NSAE
        DO VAR = STRT, FINI
          SPC = SAE( VAR )
          DO LAY = 1, NLAYS
            DO ROW = 1, NROWS
              DO COL = 1, NCOLS
                FAC = CNV2 / DENS( COL, ROW, LAY )
                CCMIN = MAX( CGRID( COL, ROW, LAY, SPC ), 1.0E-30/FAC )
                CGRID( COL, ROW, LAY, SPC ) = FAC * CCMIN
              END DO
            END DO
          END DO
        END DO

C...determine no. of moles of non-reactive gas per moles of air by volume

        STRT = NR_STRT
        FINI = NR_STRT + N_NR_SPC - 1
        DO SPC = STRT, FINI
          DO LAY = 1, NLAYS
            DO ROW = 1, NROWS
              DO COL = 1, NCOLS
                CGRID( COL, ROW, LAY, SPC ) = MAX( CGRID( COL, ROW, LAY, SPC )
     &                                      * 1.0E-6, 1.0E-36 )
              END DO
            END DO
          END DO
        END DO

C...determine no. of moles of tracer gas per moles of air by volume

        STRT = TR_STRT
        FINI = TR_STRT + N_TR_SPC - 1
        DO SPC = STRT, FINI
          DO LAY = 1, NLAYS
            DO ROW = 1, NROWS
              DO COL = 1, NCOLS
                CGRID( COL, ROW, LAY, SPC ) = MAX( CGRID( COL, ROW, LAY, SPC )
     &                                      * 1.0E-6, 1.0E-36 )
              END DO
            END DO
          END DO
        END DO

      END SUBROUTINE CONVERT_FORWARD

      SUBROUTINE CONVERT_BACKWARD ( CGRID, DENS )

        USE GRID_CONF             ! horizontal & vertical domain specifications

        IMPLICIT NONE
        REAL,  POINTER         :: CGRID( :,:,:,: ) ! concentrations
        REAL,  INTENT( IN )    :: DENS( :,:,: )    ! density

        INTEGER       LAY              ! layer subscript indices
        INTEGER       COL              ! column subscript indices
        INTEGER       ROW              ! row subscript indices
        INTEGER       SPC              ! species subscript indices
        INTEGER       STRT             ! starting position
        INTEGER       FINI             ! ending position
        INTEGER       VAR              ! variable subscript indices
        REAL FAC                       ! temp conversion factor
        REAL CCMIN
        REAL, PARAMETER :: MWAIR = 28.9628
        REAL, PARAMETER :: CNV1 = MWAIR * 1.0E-9
        REAL, PARAMETER :: CNV1I = 1.0 / CNV1
        REAL, PARAMETER :: CNV2 = MWAIR * 1.0E-3
        REAL, PARAMETER :: CNV2I = 1.0 / CNV2
        REAL, PARAMETER :: CNV3 = CNV2                  ! -> #/mol
        REAL, PARAMETER :: CNV3I = 1.0 / CNV3

C...convert units on cgrid back to original units
C...  convert to ppmV gas

        STRT = GC_STRT
        FINI = GC_STRT + N_GC_SPC - 1
        DO SPC = STRT, FINI
          DO LAY = 1, NLAYS
            DO ROW = 1, NROWS
              DO COL = 1, NCOLS
                CGRID( COL, ROW, LAY, SPC ) = CGRID( COL, ROW, LAY, SPC )
     &                                      * 1.0E+6
              END DO
            END DO
          END DO
        END DO

C...convert to ug/m3 of aerosol mass

        STRT = 1
        FINI = NQAE
        DO VAR = STRT, FINI
          SPC = QAE( VAR )
          DO LAY = 1, NLAYS
            DO ROW = 1, NROWS
              DO COL = 1, NCOLS
                FAC = CNV1I * DENS( COL, ROW, LAY )
                CGRID( COL, ROW, LAY, SPC ) = FAC * CGRID( COL, ROW, LAY, SPC )
     &                                      * AE_MOLWT( SPC - AE_STRT + 1 )
              END DO
            END DO
          END DO
        END DO

C...convert to #/m3 of aerosol number

        STRT = 1
        FINI = NNAE
        DO VAR = STRT, FINI
          SPC = NAE( VAR )
          DO LAY = 1, NLAYS
            DO ROW = 1, NROWS
              DO COL = 1, NCOLS
                FAC = CNV3I * DENS( COL, ROW, LAY )
                CGRID( COL, ROW, LAY, SPC ) = FAC * CGRID( COL, ROW, LAY, SPC )
              END DO
            END DO
          END DO
        END DO

C...convert to m2/m3 of aerosol surface area

        STRT = 1
        FINI = NSAE
        DO VAR = STRT, FINI
          SPC = SAE( VAR )
          DO LAY = 1, NLAYS
            DO ROW = 1, NROWS
              DO COL = 1, NCOLS
                FAC = CNV2I * DENS( COL, ROW, LAY )
                CGRID( COL, ROW, LAY, SPC ) = FAC * CGRID( COL, ROW, LAY, SPC )
              END DO
            END DO
          END DO
        END DO

C...convert to ppmV non-reactive gas

        STRT = NR_STRT
        FINI = NR_STRT + N_NR_SPC - 1
        DO SPC = STRT, FINI
          DO LAY = 1, NLAYS
            DO ROW = 1, NROWS
              DO COL = 1, NCOLS
                CGRID( COL, ROW, LAY, SPC ) = CGRID( COL, ROW, LAY, SPC )
     &                                      * 1.0E+6
              END DO
            END DO
          END DO
        END DO

C...convert to ppmV tracer gas

        STRT = TR_STRT
        FINI = TR_STRT + N_TR_SPC - 1
        DO SPC = STRT, FINI
          DO LAY = 1, NLAYS
            DO ROW = 1, NROWS
              DO COL = 1, NCOLS
                CGRID( COL, ROW, LAY, SPC ) = CGRID( COL, ROW, LAY, SPC )
     &                                      * 1.0E+6
              END DO
            END DO
          END DO
        END DO

      END SUBROUTINE CONVERT_BACKWARD

      Subroutine set_aeconcmin( aeconcmin )
C-----------------------------------------------------------------------
C
C  Function:
C       set minimum concentrations for aerosol species
C       routine called from convcld_acm
C
C  Preconditions:
C       Dates and times represented YYYYDDD:HHMMSS.
C
C  Revision History:
C       5/05 copied code from aero_depv to begin subroutine
C       4/08 J.Kelly: added code to set min for coarse surface area
C       21 Jun 10 J.Young: convert for Namelist redesign
C-----------------------------------------------------------------------

      Use cgrid_spcs            ! CGRID mechanism species
      Use aero_data
      Use utilio_defn

      Implicit None

C..Includes:
      Include SUBST_CONST               ! constants

C..Functions:

C..Parameters:

C..Arguments:
      Real, Intent( Out ) :: aeconcmin( : ) ! array of minimum concentrations

C..Local Variables:
      !Character( 16 ) :: pname = 'SET_AECONCMIN'   ! program name

      Integer m        ! mode loop counter
      Integer n        ! aerospc loop counter
      Integer spc      ! species loop counter

      ! compute minimums for numbers and surfaces
      Do m = 1, n_mode
         spc = index1( aeromode( m )%num_name, n_ae_spc, ae_spc )
         aeconcmin( spc ) = aeromode_minNum( m ) * mwair * 1.0E-3

         spc = index1( aeromode( m )%srf_name, n_ae_spc, ae_spc )
         aeconcmin( spc ) = aeromode_minM2( m ) * pi * mwair * 1.0E-3
      End Do

      ! compute minimums for aerosol species
      Do m = 1, n_mode
         Do n = 1, n_aerospc
            If ( .NOT. aero_missing(n,m) ) Then
               spc = index1( aerospc( n )%name( m ), n_ae_spc, ae_spc )
               If ( spc .Ne. 0 ) Then
                  aeconcmin( spc ) = aerospc( n )%min_conc( m ) * mwair * 1.0E-9 / ae_molwt( spc )
               End If
            End If
         End Do
      End Do

      Return
      End Subroutine set_aeconcmin

      INTEGER FUNCTION INDEX_KPP( NAME )

        USE MCHEM_PARAMETERS, ONLY: NSPEC
        USE MCHEM_MONITOR,    ONLY: SPC_NAMES

        IMPLICIT NONE

        CHARACTER( * ), INTENT( IN ) :: NAME
        INTEGER :: SPC

        INDEX_KPP = 0

        DO SPC = 1, NSPEC
           IF ( TRIM(NAME) == TRIM(SPC_NAMES(SPC)) ) THEN
              INDEX_KPP = SPC
           END IF
        END DO

      END FUNCTION INDEX_KPP

      INTEGER FUNCTION INDEX_CGRID( NAME )

        IMPLICIT NONE

        CHARACTER( 16 ), INTENT( IN ) :: NAME
        INTEGER :: SPC

        INDEX_CGRID = 0

        DO SPC = 1, N_CGRID_SPC
           IF ( TRIM(NAME) == TRIM(CGRID_NAME(SPC)) ) THEN
              INDEX_CGRID = SPC
           END IF
        END DO

      END FUNCTION INDEX_CGRID

      CHARACTER( 2 ) FUNCTION SPC_TYPE( IND )

        IMPLICIT NONE

        INTEGER, INTENT( IN ) :: IND
        INTEGER               :: IGC, IAE, INR, ITR

        IGC = N_GC_SPCD
        IAE = N_GC_SPCD + N_AE_SPC
        INR = N_GC_SPCD + N_AE_SPC + N_NR_SPC
        ITR = N_GC_SPCD + N_AE_SPC + N_NR_SPC + N_TR_SPC

        IF ( IND .LE. IGC ) SPC_TYPE = 'GC'
        IF ( ( IND .GT. IGC ) .AND. ( IND .LE. IAE ) ) SPC_TYPE = 'AE'
        IF ( ( IND .GT. IAE ) .AND. ( IND .LE. INR ) ) SPC_TYPE = 'NR'
        IF ( ( IND .GT. INR ) .AND. ( IND .LE. ITR ) ) SPC_TYPE = 'TR'

      END FUNCTION SPC_TYPE

      SUBROUTINE UPDATE_GM1_GM2 ( GM1,GM2,PHI2,VAR,CL_TYPE )

        IMPLICIT NONE

        REAL( dp ), INTENT( INOUT ) :: GM1
        REAL( dp ), INTENT( INOUT ) :: GM2
        REAL( dp ), INTENT( IN )    :: PHI2
        REAL( dp ), INTENT( IN )    :: VAR( : )
        INTEGER,    INTENT( IN )    :: CL_TYPE

        INTEGER                     :: ICL
        REAL( dp )                  :: STION
        REAL( dp )                  :: GM1LOG
        REAL( dp )                  :: GM2LOG

        STION = 0.0D0
        DO ICL = 1,N_CL_SPC_CHARGED
          STION = STION + ( ( REAL( CHARGED_SPC_CL( ICL )%CHARGE, dp ) ) ** 2.0D0 *
     &            VAR( CHARGED_SPC_CL( ICL )%KPP_INDEX( CL_TYPE ) ) )
        END DO
        STION  = 0.5D0 * STION * PHI2

        GM1LOG = -0.509D0 * ( SQRT( STION ) / ( 1.0D0 + SQRT( STION ) ) - 0.2D0 * STION )
        GM2LOG = GM1LOG * 4.0D0
        GM1    = 10.0D0**GM1LOG
        GM2    = MAX( 10.0D0**GM2LOG, 1.0D-30 )

      END SUBROUTINE UPDATE_GM1_GM2

      END MODULE MCHEM_CLOUDS
