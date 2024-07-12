
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
      SUBROUTINE MCHEM_CHEM ( CGRID, TA, PRES, DENSI, OPEN_WATER_IN,
     &                        SEAWATER_IN, CAIR_IN, QV,
     &                        CHEMSTEP, LEV, COL, ROW )

C-----------------------------------------------------------------------
C  FUNCTION: Resolved-scale CLOUD processor Models-3 science process:
C
C  This function only obtains the cloud properties necessary for MCHEM.
C
C  Calls the following subroutines:  SCAVWDEP
C-----------------------------------------------------------------------

      USE mchem_Model
      USE mchem_Initialize, ONLY: Initialize
      USE mchem_clouds
      USE mchem_DATA,       ONLY: MECH_SPC, NSTEPS, NFAILS,
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
      LOGICAL, PARAMETER      :: LCL_TOL = .TRUE.     ! flag if alternative tolerances should
                                                      ! be used for cloud species

      INTEGER SPC             ! Species loop index
      INTEGER ICGSPC          ! Index of species in CGRID
      INTEGER ICSPC           ! Index of species in CONC
      INTEGER ICLT            ! Index of cloud type
      INTEGER NPH             ! Index for number of photolysis reaction in PHOT
      REAL(8)                 :: ALFA                 ! total (RS + CV) scavenging rate coefficient (1/s)

C-----------------------------------------------------------------------

C...Initialization

      IF ( FIRSTIME ) THEN

        FIRSTIME = .FALSE.

C.. Set KPP tolerances
        DO SPC = 1, NVAR
           ATOL(SPC) = 1.0D1
           RTOL(SPC) = 1.0D-2
        END DO

C.. Set different KPP tolerances for cloud species if requested by user
        IF ( LCL_TOL ) THEN
           DO SPC = 1, N_CL_SPC
              DO ICLT = 1, CL_TYPES
                 ICSPC = MECH_SPC_CL( SPC )%KPP_INDEX( ICLT )
                 ATOL(ICSPC) = 1.0D0
                 RTOL(ICSPC) = 1.0D-3
              END DO
           END DO
        END IF

C.. Set KPP options
        ICNTRL(1) = 1 ! 0 - non-autonomous, 1 - autonomous
        ICNTRL(3) = 4 ! Rodas3
C.. Alternative KPP options
        !ICNTRL(3) = 2 ! Ros3

      END IF  ! FIRSTIME

      TEMP       = REAL( TA, dp )
      PRESS      = REAL( PRES, dp )
      SEAWATER   = REAL( SEAWATER_IN, dp )
      OPEN_WATER = OPEN_WATER_IN
      CAIR       = REAL( CAIR_IN, dp )

      ! Fill aqueous related data
      LWC_RS = REAL( WCBAR_RS( COL,ROW,LEV ), dp )
      LWC_CV = REAL( WCBAR_CV( COL,ROW,LEV ), dp )
      LCL_RS = .FALSE.
      LCL_CV = .FALSE.
      LCL    = .FALSE.
      IF ( LWC_RS .GT. LWC_LIMIT ) LCL_RS = .TRUE.
      IF ( LWC_CV .GT. LWC_LIMIT ) LCL_CV = .TRUE.
      IF ( LCL_RS .OR. LCL_CV )    LCL    = .TRUE.
      IF ( LCL_RS ) THEN
         XCL_RS  = 1.0D0
         GM1_RS  = 1.0D0
         GM2_RS  = 1.0D0
         PHI2_RS = 1000.0D0 / 6.022d23 / (LWC_RS * 1.D-3)
         WFAC_RS = REAL( WETFAC_RS( COL,ROW,LEV ), dp )
         KPP_ALFA3_RS = REAL( ALFA3_RS( COL,ROW,LEV ), dp )
      ELSE
         XCL_RS  = 0.0D0
         GM1_RS  = 0.0D0
         GM2_RS  = 0.0D0
         PHI2_RS = 0.0D0
         WFAC_RS = 0.0D0
         LWC_RS  = 0.0D0 ! Reset lwc if below limit
         KPP_ALFA3_RS = 0.0D0
      END IF
      IF ( LCL_CV ) THEN
         XCL_CV  = 1.0D0
         GM1_CV  = 1.0D0
         GM2_CV  = 1.0D0
         PHI2_CV = 1000.0D0 / 6.022d23 / (LWC_CV * 1.D-3)
         WFAC_CV = REAL( WETFAC_CV( COL,ROW,LEV ), dp )
         KPP_ALFA3_CV = REAL( ALFA3_CV( COL,ROW,LEV ), dp )
         CFRAC_CV = REAL( FRAC_CV( COL,ROW,LEV ), dp )
      ELSE
         XCL_CV  = 0.0D0
         GM1_CV  = 0.0D0
         GM2_CV  = 0.0D0
         PHI2_CV = 0.0D0
         WFAC_CV = 0.0D0
         LWC_CV  = 0.0D0 ! Reset lwc if below limit
         KPP_ALFA3_CV = 0.0D0
         CFRAC_CV = 0.0D0
      END IF
      TFAC      = ( 298.0D0 - TEMP) / ( 298.0D0 * TEMP );
      ! Calculate Henry's law coefficients
      H_SO2  = 1.4D+00  * EXP( 2.9D+03 * TFAC );
      H_HNO3 = 2.1D+05  * EXP( 8.7D+03 * TFAC );
      H_CO2  = 3.6D-02  * EXP( 2.2D+03 * TFAC );
      H_NH3  = 6.1D+01  * EXP( 4.2D+03 * TFAC );
      H_H2O2 = 8.3D+04  * EXP( 7.4D+03 * TFAC );
      H_O3   = 1.14D-02 * EXP( 2.3D+03 * TFAC );
      H_FACD = 8.9D+03  * EXP( 6.1D+03 * TFAC );
      H_HCL  = 1.9D+01  * EXP( 6.0D+02 * TFAC );
      H_GLY  = 3.6D+05  * EXP( 0.0D+00 * TFAC );
      H_MGLY = 3.2D+04  * EXP( 0.0D+00 * TFAC );
      H_OH   = 3.0D+01  * EXP( 4.5D+03 * TFAC );
      H_HO2  = 4.0D+03  * EXP( 5.9D+03 * TFAC );
      H_NO2  = 1.2D-02  * EXP( 2.5D+03 * TFAC );
      H_HONO = 5.0D+01  * EXP( 4.9D+03 * TFAC );
      H_HNO4 = 1.2D+04  * EXP( 6.9D+03 * TFAC );
      H_GLYD = 4.1D+04  * EXP( 4.6D+03 * TFAC );
      H_AACD = 4.1D+03  * EXP( 6.3D+03 * TFAC );
      H_FORM = 2.5D0;
      H_NO3  = 0.6D0;
      H_CH3O2 = 2.7D0   * EXP( 2.03D+03 * TFAC );
      H_IEPOX = 3.0D7;
      H_MHP  = 3.1D+02  * EXP( 5.2D+03 * TFAC );
      H_PAA  = 8.4D+02  * EXP( 5.3D+03 * TFAC );
      ! IEPOX for AE6I and AE7I
      H_IMAE  = 1.2E+05 * EXP( 0.0E+00 * TFAC )
      H_IHMML = H_IMAE
      ! End filling aqueous related data

      ! Fill photolysis & heterogeneous rates
      ! provided in 1/min. here converted to 1/s.
      DO NPH = 1, KPP_NHETERO
         KHETCELL( NPH ) = REAL( KHETERO( NPH, COL, ROW, LEV ) , dp ) / 60.0_dp
      END DO
      LSUNLIGHT = .FALSE.
      DO NPH = 1, KPP_NPHOTAB
         RJCELL( NPH )   = REAL( RJ( COL,ROW,LEV,NPH ), dp ) / 60.0_dp
         IF ( RJCELL( NPH ) .GT. 0.0D0 ) LSUNLIGHT = .TRUE.
      END DO

      ! Initialize before filling C array
      CALL Initialize()

      ! Fill CONC gas and aerosols
      C(:) = 0.0D0
      ! Init cloud concentrations first, since it impacts aerosol and gas phase concentrations
      IF ( LCL ) CALL INIT_CLOUDS ( CGRID( COL,ROW,LEV,: ), C(:), LWC_RS, LWC_CV, PHI2_RS, PHI2_CV, CFRAC_CV )
      DO SPC = 1, NSPEC
         IF ( MECH_SPC(SPC)%CGRID_INDEX /= 0 ) THEN
            ICGSPC = MECH_SPC(SPC)%CGRID_INDEX
            ICSPC  = MECH_SPC(SPC)%CONC_INDEX
            C( ICSPC ) = REAL( MAX( CGRID( COL,ROW,LEV,ICGSPC ), CONCMIN), dp )
         END IF
      END DO

      ! Set constant species concentrations
      C(IND_M)   = 1.0E+06    * CAIR * 1.D-6
      C(IND_H2)  = 0.56       * CAIR * 1.D-6
      C(IND_O2)  = 0.2095E+06 * CAIR * 1.D-6
      C(IND_CH4) = 1.85       * CAIR * 1.D-6
      C(IND_CO2) = 350.0D0    * CAIR * 1.D-6

      ! Set cloud water
      IF ( LCL_RS ) C(IND_H2O_RS) = LWC_RS / REAL( DENSI * QV_TO_PPM, dp ) * CAIR * 1.D-6
      IF ( LCL_CV ) C(IND_H2O_CV) = LWC_CV / REAL( DENSI * QV_TO_PPM, dp ) * CAIR * 1.D-6

      ! Fill water
      C(IND_H2O) = REAL( MAX( REAL( QV_TO_PPM * QV, dp ) *CAIR*1.D-6, 0.0 ), dp)

      COEFF_FALLOFF = C(IND_M)

      ! Fill scavenging rates
      DO SPC = 1, NSPEC
         IF ( MECH_SPC(SPC)%CGRID_INDEX /= 0 ) THEN
            ICGSPC = MECH_SPC(SPC)%CGRID_INDEX
            ICSPC  = MECH_SPC(SPC)%CONC_INDEX
            IF ( LSCAV( ICGSPC ) .AND. KPP_SCAV( ICGSPC ) ) THEN
               ALFA = RSCAV_RS( COL, ROW, LEV, ICGSPC ) +
     &                RSCAV_CV( COL, ROW, LEV, ICGSPC )
               ALFA = MAX( ALFA, 0.0 )
            ELSE
               ALFA = 0.0
            END IF
            KPP_RSCAV( ICSPC ) = REAL( ALFA, dp )
         END IF
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

C...calculate cloud pH as diagnostic
      IF ( ( LCL_RS ) .AND. ( C( ind_Hp_RS ) .GT. 0. ) )
     &     pH_RS( COL,ROW,LEV ) = - LOG10( C( ind_Hp_RS ) * PHI2_RS )
      IF ( ( LCL_CV ) .AND. ( C( ind_Hp_CV ) .GT. 0. ) )
     &     pH_CV( COL,ROW,LEV ) = - LOG10( C( ind_Hp_CV ) * PHI2_CV )

      ! Fill CGRID
      DO SPC = 1, NSPEC
         IF ( MECH_SPC(SPC)%CGRID_INDEX /= 0 ) THEN
            ICGSPC = MECH_SPC(SPC)%CGRID_INDEX
            ICSPC  = MECH_SPC(SPC)%CONC_INDEX
            CGRID( COL,ROW,LEV,ICGSPC ) = MAX( REAL( C( ICSPC ), 4 ), CONCMIN )
         END IF
      END DO

C...transfer scavenging information from KPP to scavenging array
      DO SPC = 1, NSPCSD
         IF ( LSCAV( SPC ) .AND. KPP_SCAV( SPC ) ) THEN
            WET_DEP( COL, ROW, LEV, SPC ) = REAL( C( I_KPP_SCAV ( SPC ) ), 8 )
         END IF
      END DO

      IF ( LHp ) WDEP_Hp( COL, ROW, LEV ) = REAL( C( IND_WD_HP ), 8 )

C...perform scavenging if tracer is not part of KPP
      DO SPC = 1, NSPCSD
         IF ( LSCAV( SPC ) .AND.
     &      ( .NOT. KPP_SCAV( SPC ) ) .AND.
     &      ( WET_DEP( COL, ROW, LEV, SPC ) == 0.0 ) ) THEN
            ALFA = RSCAV_RS( COL, ROW, LEV, SPC ) +
     &             RSCAV_CV( COL, ROW, LEV, SPC )
            WET_DEP( COL, ROW, LEV, SPC ) = REAL( CGRID( COL, ROW, LEV, SPC ), 8 ) *
     &                                      ( 1.0 - EXP( -ALFA * CHEMSTEP ) )
            CGRID( COL, ROW, LEV, SPC )   = CGRID( COL, ROW, LEV, SPC ) -
     &                                      REAL( WET_DEP( COL, ROW, LEV, SPC ), 4 )
         END IF
      END DO

C...evaporate clouds back to gas/aerosol phase
      IF ( LCL ) CALL EVAP_CLOUDS ( CGRID( COL,ROW,LEV,: ), C(:), WET_DEP( COL,ROW,LEV,: ),
     &                              WFAC_RS, WFAC_CV, DT )

      RETURN

      END
