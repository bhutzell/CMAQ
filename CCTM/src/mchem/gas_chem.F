
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
      SUBROUTINE GAS_CHEM ( CGRID, TA, PRES, DENSI, OPEN_WATER_IN,
     &                      SEAWATER_IN, CAIR_IN, QV,
     &                      CHEMSTEP, LEV, COL, ROW )

C-----------------------------------------------------------------------
C  FUNCTION: Resolved-scale CLOUD processor Models-3 science process:
C
C  This function only obtains the cloud properties necessary for MCHEM.
C
C  Calls the following subroutines:  SCAVWDEP
C-----------------------------------------------------------------------

      USE gas_Model
      USE gas_Initialize,   ONLY: Initialize
      USE mchem_DATA,       ONLY: MECH_SPC_GAS, NSTEPS, NFAILS,
     &                            pH_RS, pH_CV
      USE PHOT_MOD,         ONLY: RJ     ! photolysis rate
      USE AEROSOL_CHEMISTRY,ONLY: KHETERO

      IMPLICIT NONE

C...........Includes:
      INCLUDE SUBST_CONST       ! CMAQ constants

C...........Arguments:
      REAL,    POINTER         :: CGRID( :,:,:,: )    ! concentrations
      REAL(8), INTENT( IN )    :: TA                  ! temperature (K)
      REAL(8), INTENT( IN )    :: PRES                ! pressure (atm)
      REAL(8), INTENT( IN )    :: DENSI
      LOGICAL, INTENT( IN )    :: OPEN_WATER_IN
      REAL(8), INTENT( IN )    :: SEAWATER_IN
      REAL(8), INTENT( IN )    :: CAIR_IN
      REAL,    INTENT( IN )    :: QV
      REAL(8), INTENT( IN )    :: CHEMSTEP
      INTEGER, INTENT( IN )    :: LEV
      INTEGER, INTENT( IN )    :: COL
      INTEGER, INTENT( IN )    :: ROW

C...........Local Variables:

      LOGICAL, SAVE            :: FIRSTIME = .TRUE.   ! flag for first pass thru

      REAL,    PARAMETER :: CONCMIN   = 1.0E-30         ! Minimum conc
      REAL,    PARAMETER :: CONCOFM   = 1.0E+06         ! conc. of M = 1E+06 ppm
      REAL,    PARAMETER :: PA2ATM    = 1.0 / STDATMPA  ! Pascal to atm conv fac
      REAL,    PARAMETER :: MAOMV     = MWAIR / MWWAT   ! Mol Wt of air over Mol Wt of water
      REAL,    PARAMETER :: QV_TO_PPM = CONCOFM * MAOMV ! factor to convert water wapor into ppm

      REAL(kind=dp)           :: RSTATE(20)           ! KPP integrator variables
      INTEGER, SAVE           :: ICNTRL(20) = 0       ! KPP integrator variables
      INTEGER                 :: IERR                 ! KPP integrator variables
      INTEGER                 :: ISTATUS(20)          ! KPP integrator variables

      INTEGER SPC             ! Species loop index
      INTEGER ICGSPC          ! Index of species in CGRID
      INTEGER ICSPC           ! Index of species in CONC
      INTEGER NPH             ! Index for number of photolysis reaction in PHOT

C-----------------------------------------------------------------------

C...Initialization

      IF ( FIRSTIME ) THEN

        FIRSTIME = .FALSE.

C.. Set KPP specific values
        DO SPC = 1, NVAR
           ATOL(SPC) = 1.0D1
           RTOL(SPC) = 1.0D-2
        END DO

C.. Set KPP options
        ICNTRL(1) = 1 ! 0 - non-autonomous, 1 - autonomous
        ICNTRL(3) = 4 ! Rodas3
C.. Alternative KPP options
        !ICNTRL(3) = 2 ! Ros3

      END IF  ! FIRSTIME

      CALL Initialize()

      TEMP       = REAL( TA, dp )
      PRESS      = REAL( PRES, dp )
      SEAWATER   = REAL( SEAWATER_IN, dp )
      OPEN_WATER = OPEN_WATER_IN
      CAIR       = REAL( CAIR_IN, dp )
      TFAC      = ( 298.0D0 - TEMP) / ( 298.0D0 * TEMP );

      ! Fill CONC gas and aerosols
      C(:) = 0.0D0
      DO SPC = 1, NSPEC
         IF ( MECH_SPC_GAS(SPC)%CGRID_INDEX /= 0 ) THEN
            ICGSPC = MECH_SPC_GAS(SPC)%CGRID_INDEX
            ICSPC  = MECH_SPC_GAS(SPC)%CONC_INDEX
            C( ICSPC ) = REAL( MAX( CGRID( COL,ROW,LEV,ICGSPC ), CONCMIN), dp )
         END IF
      END DO

      ! Set constant species concentrations
      C(IND_M)   = 1.0E+06    * CAIR * 1.D-6
      C(IND_H2)  = 0.56       * CAIR * 1.D-6
      C(IND_O2)  = 0.2095E+06 * CAIR * 1.D-6
      C(IND_CH4) = 1.85       * CAIR * 1.D-6

      ! Fill water
      C(IND_H2O) = REAL( MAX( REAL( QV_TO_PPM * QV, dp ) *CAIR*1.D-6, 0.0 ), dp)

      COEFF_FALLOFF = C(IND_M)

      ! Fill photolysis & heterogeneous rates
      ! provided in 1/min. here converted to 1/s.
      DO NPH = 1, KPP_NHETERO
         KHETCELL( NPH ) = REAL( KHETERO( NPH, COL, ROW, LEV ) , dp ) / 60.0_dp
      END DO
      DO NPH = 1, KPP_NPHOTAB
         RJCELL( NPH )   = REAL( RJ( COL,ROW,LEV,NPH ), dp ) / 60.0_dp
         IF ( RJCELL( NPH ) .GT. 0.0D0 ) LSUNLIGHT = .TRUE.
      END DO                         

      ! Perform integration
      !~~~> Time loop
      TSTART = 0.0_dp
      DT     = REAL( CHEMSTEP, dp )
      TEND   = TSTART + DT

      CALL Update_RCONST()

      CALL INTEGRATE( TIN = TSTART, TOUT = TSTART+DT, RSTATUS_U = RSTATE,
     &                ICNTRL_U = ICNTRL, IERR_U = IERR, ISTATUS_U = ISTATUS )

      !~~~> End Time loop
      !C(:) = MAX(C(:),CONCMIN)
      ! END integration

C...save KPP diagnostics
      NSTEPS( COL,ROW,LEV ) = REAL( ISTATUS( 3 ), 8 )
      NFAILS( COL,ROW,LEV ) = REAL( ISTATUS( 5 ), 8 )

      ! Fill CGRID
      DO SPC = 1, NSPEC
         IF ( MECH_SPC_GAS(SPC)%CGRID_INDEX /= 0 ) THEN
            ICGSPC = MECH_SPC_GAS(SPC)%CGRID_INDEX
            ICSPC  = MECH_SPC_GAS(SPC)%CONC_INDEX
            CGRID( COL,ROW,LEV,ICGSPC ) = MAX( REAL( C( ICSPC ), 4 ), CONCMIN )
         END IF
      END DO

      RETURN

      END
