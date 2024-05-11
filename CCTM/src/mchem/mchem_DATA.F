
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

      USE RXNS_DATA
      USE MCHEM_PARAMETERS, ONLY: NSPEC
      USE MCHEM_MONITOR,    ONLY: SPC_NAMES
      Use RUNTIME_VARS,     ONLY: LOGDEV

      IMPLICIT NONE

      TYPE :: KPP_SPC
        CHARACTER(16) :: NAME         = ''
        CHARACTER(2)  :: SPC_TYPE     = ''
        INTEGER       :: RXNS_INDEX   = 0        ! Index used in RXNS data arrays
        INTEGER       :: CGRID_INDEX  = 0        ! Index used in CGRID
        INTEGER       :: CONC_INDEX   = 0        ! Index used in KPP arrays
        LOGICAL       :: CONVERT      = .FALSE.
        REAL(8)       :: FORWARD_CONV = 1.0      ! Conversion factor cgrid to conc
        REAL(8)       :: REVERSE_CONV = 1.0      ! Conversion factor conc to cgrid
        REAL(8)       :: MOLWT        = 0.0      ! Molecular Weight of species (gm/mole)
      END TYPE
      TYPE(KPP_SPC), DIMENSION(NSPEC), SAVE :: MECH_SPC

      CHARACTER( 200 )        :: FMT                  ! Format for log-file output

      CONTAINS
 
C------------------------------------------------------------------------
      SUBROUTINE ALLOC_KPP_SPC( )

      USE UTILIO_DEFN

      IMPLICIT NONE

C..Includes:
      INCLUDE SUBST_CONST       ! CMAQ constants

      INTEGER IRXNS           ! Index used in RXNS data
      INTEGER SPC             ! Species loop index
      INTEGER CSPC            ! RXNS Species loop index

C.. Set up KPP species information
         DO SPC = 1, NSPEC
            MECH_SPC(SPC)%NAME       = SPC_NAMES(SPC)
            MECH_SPC(SPC)%CONC_INDEX = SPC
            ! Get RXNS index
            DO CSPC = 1, NUMB_MECH_SPC
               IF ( TRIM(MECH_SPC(SPC)%NAME) == TRIM(CHEMISTRY_SPC(CSPC)) ) THEN
                  MECH_SPC(SPC)%RXNS_INDEX = CSPC
               END IF
            END DO
            ! Get additional information if species is in CGRID
            IF ( MECH_SPC(SPC)%RXNS_INDEX /= 0 ) THEN
               IRXNS = MECH_SPC(SPC)%RXNS_INDEX
               MECH_SPC(SPC)%SPC_TYPE     = SPECIES_TYPE(  IRXNS )
               MECH_SPC(SPC)%CGRID_INDEX  = CGRID_INDEX(   IRXNS )
               MECH_SPC(SPC)%CONVERT      = CONVERT_CONC(  IRXNS )
               MECH_SPC(SPC)%MOLWT        = REAL( SPECIES_MOLWT( IRXNS ), 8 )
               IF ( ( MECH_SPC(SPC)%MOLWT > 0.0 ) .AND. ( MECH_SPC(SPC)%CONVERT ) ) THEN
                  MECH_SPC(SPC)%FORWARD_CONV = REAL( 1.0E-3 * MWAIR / MECH_SPC(SPC)%MOLWT, 8 )
                  MECH_SPC(SPC)%REVERSE_CONV = REAL( 1.0E+3 / MWAIR * MECH_SPC(SPC)%MOLWT, 8 )
               END IF
            END IF
         END DO
C.. Report species setup
         WRITE(LOGDEV,"(A)") 'CGRID gas- and aerosol-species references in MCHEM:'
         FMT = "(A16,A6,  X,A4,X,A10,X,A11,X,A9,X,A7)"
         WRITE(LOGDEV,FMT) 'NAME            ','MOLWT','TYPE','RXNS_INDEX','CGRID_INDEX',
     &                     'KPP_INDEX','CONVERT'
         FMT = "(A16,F6.1,X,A4,X,I10,X,I11,X,I9,X,L7)"
         DO SPC = 1, NSPEC
            IF ( MECH_SPC(SPC)%RXNS_INDEX /= 0 ) THEN
               WRITE(LOGDEV,FMT) MECH_SPC(SPC)%NAME,MECH_SPC(SPC)%MOLWT,MECH_SPC(SPC)%SPC_TYPE,
     &                           MECH_SPC(SPC)%RXNS_INDEX,MECH_SPC(SPC)%CGRID_INDEX,
     &                           MECH_SPC(SPC)%CONC_INDEX,MECH_SPC(SPC)%CONVERT
            END IF
         END DO

      END SUBROUTINE ALLOC_KPP_SPC

      END MODULE MCHEM_DATA
