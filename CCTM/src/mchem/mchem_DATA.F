
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
      MODULE MCHEM_DATA

      USE CGRID_SPCS            ! CGRID mechanism species
      Use RUNTIME_VARS,     ONLY: LOGDEV
C..MCHEM related species data
      USE MCHEM_PARAMETERS, ONLY: NSPEC_MCHEM => NSPEC
      USE MCHEM_MONITOR,    ONLY: SPC_NAMES_MCHEM => SPC_NAMES
C..GAS related species data
      USE GAS_PARAMETERS,   ONLY: NSPEC_GAS => NSPEC
      USE GAS_MONITOR,      ONLY: SPC_NAMES_GAS => SPC_NAMES
C..SPC type
      USE MCHEM_CLOUDS,     ONLY: SPC_TYPE

      IMPLICIT NONE

      TYPE :: KPP_SPC
        CHARACTER(16) :: NAME         = ''
        CHARACTER(2)  :: SPC_TYPE     = ''
        INTEGER       :: CGRID_INDEX  = 0        ! Index used in CGRID
        INTEGER       :: CONC_INDEX   = 0        ! Index used in KPP arrays
        LOGICAL       :: CONVERT      = .FALSE.
        REAL(8)       :: FORWARD_CONV = 1.0      ! Conversion factor cgrid to conc
        REAL(8)       :: REVERSE_CONV = 1.0      ! Conversion factor conc to cgrid
        REAL(8)       :: MOLWT        = 0.0      ! Molecular Weight of species (gm/mole)
      END TYPE
      TYPE(KPP_SPC), DIMENSION(NSPEC_MCHEM), SAVE :: MECH_SPC
      TYPE(KPP_SPC), DIMENSION(NSPEC_GAS),   SAVE :: MECH_SPC_GAS

      CHARACTER( 200 )        :: FMT                  ! Format for log-file output

      CONTAINS
 
C------------------------------------------------------------------------
      SUBROUTINE ALLOC_KPP_SPC( )

      USE UTILIO_DEFN

      IMPLICIT NONE

C..Includes:
      INCLUDE SUBST_CONST       ! CMAQ constants

      INTEGER ICGRID          ! Index used in CGRID
      INTEGER SPC             ! Species loop index
      INTEGER CSPC            ! CGRID Species loop index

C.. Set up KPP species information
         DO SPC = 1, NSPEC_MCHEM
            MECH_SPC(SPC)%NAME       = SPC_NAMES_MCHEM(SPC)
            MECH_SPC(SPC)%CONC_INDEX = SPC
            ! Get CGRID index
            DO CSPC = 1, N_CGRID_SPC
               IF ( TRIM(MECH_SPC(SPC)%NAME) == TRIM(CGRID_NAME(CSPC)) ) THEN
                  MECH_SPC(SPC)%CGRID_INDEX = CSPC
               END IF
            END DO
            ! Get additional information if species is in CGRID
            IF ( MECH_SPC(SPC)%CGRID_INDEX /= 0 ) THEN
               ICGRID = MECH_SPC(SPC)%CGRID_INDEX
               MECH_SPC(SPC)%SPC_TYPE     = SPC_TYPE( ICGRID )
               IF ( ( CGRID_MASK_AERO( ICGRID ) ) .AND.
     &              ( .NOT. CGRID_MASK_NUM( ICGRID ) ) .AND.
     &              ( .NOT. CGRID_MASK_SRF( ICGRID ) ) ) THEN
                  MECH_SPC(SPC)%CONVERT   = .TRUE.
               END IF
               MECH_SPC(SPC)%MOLWT        = REAL( CGRID_MW( ICGRID ), 8 )
               IF ( ( MECH_SPC(SPC)%MOLWT > 0.0 ) .AND. ( MECH_SPC(SPC)%CONVERT ) ) THEN
                  MECH_SPC(SPC)%FORWARD_CONV = REAL( 1.0E-3 * MWAIR / MECH_SPC(SPC)%MOLWT, 8 )
                  MECH_SPC(SPC)%REVERSE_CONV = REAL( 1.0E+3 / MWAIR * MECH_SPC(SPC)%MOLWT, 8 )
               END IF
            END IF
         END DO
C.. Report species setup
         WRITE(LOGDEV,"(A)") 'CGRID gas- and aerosol-species references in MCHEM mechanism:'
         FMT = "(A16,A6,  X,A4,X,A11,X,A9,X,A7,X,A12,   X,A12)"
         WRITE(LOGDEV,FMT) 'NAME            ','MOLWT','TYPE','CGRID_INDEX',
     &                     'KPP_INDEX','CONVERT','FORWARD_CONV','REVERSE_CONV'
         FMT = "(A16,F6.1,X,A4,X,I11,X,I9,X,L7,X,ES12.2,X,ES12.2)"
         DO SPC = 1, NSPEC_MCHEM
            IF ( MECH_SPC(SPC)%CGRID_INDEX /= 0 ) THEN
               WRITE(LOGDEV,FMT) MECH_SPC(SPC)%NAME,MECH_SPC(SPC)%MOLWT,
     &                           MECH_SPC(SPC)%SPC_TYPE,MECH_SPC(SPC)%CGRID_INDEX,
     &                           MECH_SPC(SPC)%CONC_INDEX,MECH_SPC(SPC)%CONVERT,
     &                           MECH_SPC(SPC)%FORWARD_CONV,MECH_SPC(SPC)%REVERSE_CONV
            END IF
         END DO

C.. Set up KPP species information
         DO SPC = 1, NSPEC_GAS
            MECH_SPC_GAS(SPC)%NAME       = SPC_NAMES_GAS(SPC)
            MECH_SPC_GAS(SPC)%CONC_INDEX = SPC
            ! Get CGRID index
            DO CSPC = 1, N_CGRID_SPC
               IF ( TRIM(MECH_SPC_GAS(SPC)%NAME) == TRIM(CGRID_NAME(CSPC)) ) THEN
                  MECH_SPC_GAS(SPC)%CGRID_INDEX = CSPC
               END IF
            END DO
            ! Get additional information if species is in CGRID
            IF ( MECH_SPC_GAS(SPC)%CGRID_INDEX /= 0 ) THEN
               ICGRID = MECH_SPC_GAS(SPC)%CGRID_INDEX
               MECH_SPC_GAS(SPC)%SPC_TYPE     = SPC_TYPE( ICGRID )
               IF ( ( CGRID_MASK_AERO( ICGRID ) ) .AND.
     &              ( .NOT. CGRID_MASK_NUM( ICGRID ) ) .AND.
     &              ( .NOT. CGRID_MASK_SRF( ICGRID ) ) ) THEN
                  MECH_SPC_GAS(SPC)%CONVERT   = .TRUE.
               END IF
               MECH_SPC_GAS(SPC)%MOLWT        = REAL( CGRID_MW( ICGRID ), 8 )
               IF ( ( MECH_SPC_GAS(SPC)%MOLWT > 0.0 ) .AND. ( MECH_SPC_GAS(SPC)%CONVERT ) ) THEN
                  MECH_SPC_GAS(SPC)%FORWARD_CONV = REAL( 1.0E-3 * MWAIR / MECH_SPC_GAS(SPC)%MOLWT, 8 )
                  MECH_SPC_GAS(SPC)%REVERSE_CONV = REAL( 1.0E+3 / MWAIR * MECH_SPC_GAS(SPC)%MOLWT, 8 )
               END IF
            END IF
         END DO
C.. Report species setup
         WRITE(LOGDEV,"(A)") 'CGRID gas- and aerosol-species references in GAS/HETERO mechanism:'
         FMT = "(A16,A6,  X,A4,X,A11,X,A9,X,A7,X,A12,   X,A12)"
         WRITE(LOGDEV,FMT) 'NAME            ','MOLWT','TYPE','CGRID_INDEX',
     &                     'KPP_INDEX','CONVERT','FORWARD_CONV','REVERSE_CONV'
         FMT = "(A16,F6.1,X,A4,X,I11,X,I9,X,L7,X,ES12.2,X,ES12.2)"
         DO SPC = 1, NSPEC_GAS
            IF ( MECH_SPC_GAS(SPC)%CGRID_INDEX /= 0 ) THEN
               WRITE(LOGDEV,FMT) MECH_SPC_GAS(SPC)%NAME,MECH_SPC_GAS(SPC)%MOLWT,
     &                           MECH_SPC_GAS(SPC)%SPC_TYPE,MECH_SPC_GAS(SPC)%CGRID_INDEX,
     &                           MECH_SPC_GAS(SPC)%CONC_INDEX,MECH_SPC_GAS(SPC)%CONVERT,
     &                           MECH_SPC_GAS(SPC)%FORWARD_CONV,MECH_SPC_GAS(SPC)%REVERSE_CONV
            END IF
         END DO

      END SUBROUTINE ALLOC_KPP_SPC

      END MODULE MCHEM_DATA
