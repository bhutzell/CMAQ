
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

       SUBROUTINE CHEM( CGRID, JDATE, JTIME, TSTEP )


C**********************************************************************
C
C  Function: To control multi-phase chemistry calculations performed by
C            the Rosenbrock solver
C
C  Preconditions: None
C
C**********************************************************************

      USE RXNS_DATA
      USE RXNS_FUNCTION
      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
      USE PHOT_MOD, Only: INIT_PHOT_SHARED, RJ     ! photolysis rate, in-line module
      USE AEROSOL_CHEMISTRY
      USE CENTRALIZED_IO_MODULE, ONLY : INTERPOLATE_VAR, OCEAN, SZONE

C..   KPP related
      USE mchem_Model
      USE mchem_Initialize, ONLY: Initialize

C.. Cloud data
      USE MCHEM_CLOUDS
      USE AQ_DATA, Only: WCBAR_RS, WCBAR_CV, WETFAC_RS, WETFAC_CV,
     &                   ALFA0_RS, ALFA0_CV, ALFA3_RS, ALFA3_CV

      IMPLICIT NONE

C..Includes:

      INCLUDE SUBST_FILES_ID    ! CMAQ files
      INCLUDE SUBST_CONST       ! CMAQ constants
 
C..Arguments:

      REAL, POINTER :: CGRID( :,:,:,: )  ! Concentrations

      INTEGER JDATE                     ! Current date (YYYYDDD)
      INTEGER JTIME                     ! Current time (HHMMSS)
      INTEGER TSTEP( 3 )                ! Time step vector (HHMMSS)

C..Parameters:

      INTEGER, PARAMETER :: IZERO = 0                ! Integer zero

      REAL,    PARAMETER :: CONCMIN   = 1.0E-30         ! Minimum conc
      REAL,    PARAMETER :: CONCOFM   = 1.0E+06         ! conc. of M = 1E+06 ppm
      REAL,    PARAMETER :: PA2ATM    = 1.0 / STDATMPA  ! Pascal to atm conv fac
      REAL,    PARAMETER :: MAOMV     = MWAIR / MWWAT   ! Mol Wt of air over Mol Wt of water
      REAL,    PARAMETER :: QV_TO_PPM = CONCOFM * MAOMV ! factor to convert water wapor into ppm
C..External Functions:

C..Local Variables:
      LOGICAL, SAVE :: LFIRST    = .TRUE. ! Flag for first call to this subroutine

      INTEGER, SAVE :: NOXYZ           ! Total number of grid cells

      REAL,    SAVE :: AIRFC           ! Factor to convert gms air to ppm

      REAL( 8 )     :: CHEMSTEP   ! Chem integration interval (s)

      CHARACTER(  16 ) :: PNAME = 'MCHEM_DRIVER' ! Procedure name
      CHARACTER(  16 ) :: VNAME                  ! Name of I/O API data variable
      CHARACTER( 144 ) :: XMSG                   ! Message text

      INTEGER ALLOCSTAT       ! Allocate status code
      INTEGER NCSP            ! Mech no: 1=gas/day 2=gas/night
      INTEGER IPAR            ! Pointer for cell sort routine
      INTEGER IRVAL           ! Pointer for cell sort routine
      INTEGER IRXN            ! Reaction number
      INTEGER ISP             ! Species index
      INTEGER ISPOLD          ! Species number in original order
      INTEGER ISPNEW          ! Species number in new sorted order 
      INTEGER ITMSTEP         ! Chemistry integration interval (sec)
      INTEGER JPAR            ! Pointer for cell sort routine
      INTEGER JREORD          ! Index holder for sort routine
      INTEGER LVAL            ! Pointer for cell sort routine
      INTEGER MIDDATE         ! Date at time step midpoint
      INTEGER MIDTIME         ! Time at time step midpoint
      INTEGER NCELL           ! Index for number of cells
      INTEGER NPH             ! Index for number of phot. rxns in PHOT
      INTEGER NRX             ! Index for number of reactions
      INTEGER SPC             ! Species loop index
      INTEGER CSPC            ! RXNS Species loop index

      INTEGER COL             ! Column index
      INTEGER LEV             ! Layer index
      INTEGER ROW             ! Row index
      INTEGER IRXNS           ! Index used in RXNS data
      INTEGER ICGSPC          ! Index of species in CGRID
      INTEGER ICSPC           ! Index of species in CONC

      REAL, ALLOCATABLE, SAVE :: SEAICE ( :, : )         ! fractional seaice cover, [-]

      REAL, ALLOCATABLE, SAVE :: DENS   ( :, :, : )      ! Cell density (Kg/m**3)
      REAL, ALLOCATABLE, SAVE :: PRES   ( :, :, : )      ! Cell pressure (Atm)
      REAL, ALLOCATABLE, SAVE :: QV     ( :, :, : )      ! Cell water vapor (Kg/Kg air)
      REAL, ALLOCATABLE, SAVE :: TA     ( :, :, : )      ! Cell temperature (K)

      REAL, ALLOCATABLE, SAVE :: SEAWATER_ZONE   ( :,: )          ! fractional area of OPEN+SURF

C.. Cloud chemistry related
      REAL( 8 ),    PARAMETER :: COEF1 = 7.33981D+15  ! Molec/cc to ppm conv factor 
      REAL( 8 )               :: VOL                  ! Cell air specific volume, m^3/kg
      REAL( 8 )               :: DENSI                ! Cell ambient MASS DENSITY in Kg/m^3
      REAL( 8 )               :: NA = 6.02214076D23   ! Avogadro constant [mol^-1]

      REAL(kind=dp)           :: RSTATE(20)           ! KPP integrator variables
      INTEGER                 :: ICNTRL(20) = 0       ! KPP integrator variables
      INTEGER                 :: IERR                 ! KPP integrator variables
      INTEGER                 :: ISTATUS(20)          ! KPP integrator variables
      CHARACTER( 200 )        :: FMT                  ! Format for log-file output
      REAL                    :: CONVERT_VALUE
      LOGICAL                 :: LCL_RS, LCL_CV, LCL
      REAL                    :: CONC( NCOLS, NROWS, NLAYS, N_CGRID_SPC )  ! Concentrations [moelc/cm3]

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

#ifdef mchem_stats
      CHARACTER( 16 ), SAVE              :: CTM_MCHEMSTATS_1 = 'CTM_MCHEMSTATS_1'
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: VSTATS( : )

      INTEGER, SAVE                      :: WSTEP = 0
      INTEGER  EDATE, ETIME
#endif

      INTERFACE
         SUBROUTINE HETCHEM_UPDATE_AERO( CGRID )
           REAL, POINTER :: CGRID( :,:,:,: )    !  species concentration in cell
         END SUBROUTINE HETCHEM_UPDATE_AERO      
      END INTERFACE

C**********************************************************************


#ifdef isam
      MSG = 'ERROR: Rosenbrock Chemistry Solver does not perform source apportionment.'
      WRITE(LOGDEV,'(A)')TRIM( MSG )
      MSG = 'Must use the EBI solver for the chemical mechanism'
      CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
#endif


      IF ( NUMB_MECH_SPC .EQ. 0 ) THEN
         CALL M3MESG( '*** WARNING: Gas-Chemistry processing bypassed!' )
         RETURN
      END IF

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  On first call, call routines to set-up for Gear solver and 
c  set-up to do emissions here if that option is invoked
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LFIRST ) THEN
         LFIRST   = .FALSE.

         NOXYZ = NCOLS * NROWS * NLAYS

C.. Get fractional seawater coverage from the OCEAN file.
         ALLOCATE( SEAWATER_ZONE( NCOLS, NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'ERROR allocating SEAWATER_ZONE'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
         END IF

         DO ROW = 1, NROWS
            DO COL = 1, NCOLS
                 SEAWATER_ZONE( COL,ROW ) =  OCEAN( COL,ROW ) + SZONE( COL,ROW )
             END DO
         END DO

         ALLOCATE( DENS( NCOLS, NROWS, NLAYS ), PRES( NCOLS, NROWS, NLAYS ),
     &             QV  ( NCOLS, NROWS, NLAYS ), TA  ( NCOLS, NROWS, NLAYS ),
     &             SEAICE( NCOLS, NROWS ) )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'ERROR allocating DENS, PRES, QV, TA, SEAICE'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
         END IF

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

        CALL CL_SPCS_DEF()

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

C..Initialize shared photolysis data
         CALL INIT_PHOT_SHARED()

c..Open file for mchem stats if requested
#ifdef mchem_stats
         ALLOCATE( VSTATS( 2 ) )
         VSTATS( 1 ) = 'LWC_RS'
         VSTATS( 2 ) = 'LWC_CV'

         IF ( MYPE .EQ. 0 ) THEN

            EDATE = JDATE
            ETIME = JTIME
            CALL NEXTIME( EDATE, ETIME, TSTEP( 1 ) )

            SDATE3D = EDATE
            STIME3D = ETIME
            NVARS3D = 2
            NCOLS3D = GL_NCOLS
            NROWS3D = GL_NROWS
            NLAYS3D = NLAYS
            VNAME3D( 1 ) = 'LWC_RS'
            VNAME3D( 2 ) = 'LWC_CV'
            VDESC3D( 1 ) = 'Liquid water content for resolved clouds'
            VDESC3D( 2 ) = 'Liquid water content for convective clouds'
            UNITS3D( 1 ) = 'kg/m3'
            UNITS3D( 2 ) = 'kg/m3'
            VTYPE3D( 1 ) = M3REAL
            VTYPE3D( 2 ) = M3REAL
            IF ( .NOT. OPEN3( CTM_MCHEMSTATS_1, FSNEW3, PNAME ) ) THEN
               XMSG = 'Could not create '// TRIM( CTM_MCHEMSTATS_1 ) // ' file'
               CALL M3EXIT( PNAME, SDATE3D, STIME3D, XMSG, XSTAT2 )
            END IF

         END IF
#endif

      END IF      ! First call

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Set date and time to center of time step, get necessary physical
C  data, and get photolysis rates
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      MIDDATE = JDATE
      MIDTIME = JTIME
      ITMSTEP = TIME2SEC( TSTEP( 2 ) )
      CHEMSTEP = REAL( ITMSTEP, 8 )
      CALL NEXTIME( MIDDATE, MIDTIME, SEC2TIME( ITMSTEP / 2 ) )

C.. Get fractional seaice coverage from the METCRO2D file.
      CALL INTERPOLATE_VAR ('SEAICE', MIDDATE, MIDTIME, SEAICE)

C.. Get ambient temperature in K
      CALL INTERPOLATE_VAR ('TA', MIDDATE, MIDTIME, TA)

C.. Get specific humidity in Kg H2O / Kg air
      CALL INTERPOLATE_VAR ('QV', MIDDATE, MIDTIME, QV)

C.. Get ambient MASS DENSITY in Kg/m^3
      CALL INTERPOLATE_VAR ('DENS', MIDDATE, MIDTIME, DENS)

C.. Get pressure in Pascals
      CALL INTERPOLATE_VAR ('PRES', MIDDATE, MIDTIME, PRES)

C.. Get Heterogeneous Rates using Aerosol Surface Area. Also Store
C   a snapshot of the aerosol surface area so that it can be
C   appropriately updated after the solver finds a solution.

      CALL HETCHEM_RATES( TA, PRES, QV, CGRID, DENS )

      DO LEV = 1, NLAYS
         DO COL = 1, NCOLS
            DO ROW = 1, NROWS

               CALL Initialize()

               TEMP     = REAL( TA( COL,ROW,LEV ), dp )
               PRESS    = REAL( PA2ATM * PRES( COL,ROW,LEV ), dp )
               DENSI    = REAL( DENS( COL,ROW,LEV ), dp )
               VOL      = REAL( 1.0 / DENSI, dp )
               OPEN_WATER = .FALSE.
               SEAWATER = REAL(MAX ( 0.0, ( SEAWATER_ZONE( COL,ROW ) - SEAICE (COL,ROW) ) ), dp )
               IF ( SEAWATER .GT. 0.001D0 ) OPEN_WATER = .TRUE.
               CAIR     = ( PRESS/PA2ATM ) / ( TEMP * 8.314510 ) * 6.0221413D17

C.. Fill aqueous related data
               LWC_RS = REAL( WCBAR_RS ( COL,ROW,LEV ), dp)
               LWC_CV = REAL( WCBAR_CV ( COL,ROW,LEV ), dp)
               LCL_RS = .FALSE.
               LCL_CV = .FALSE.
               LCL    = .FALSE.
               IF ( LWC_RS .GT. LWC_LIMIT ) LCL_RS = .TRUE.
               IF ( LWC_CV .GT. LWC_LIMIT ) LCL_CV = .TRUE.
               IF ( LCL_RS .OR. LCL_CV )    LCL    = .TRUE.
               IF ( LCL_RS ) THEN
                  XCL_RS = 1.0D0
                  GM1_RS = 1.0D0
                  GM2_RS = 1.0D0
                  PHI2_RS = 1000.0D0 / 6.022d23 / (LWC_RS * 1.D-3)
                  WFAC_RS = WETFAC_RS ( COL,ROW,LEV )
               ELSE
                  XCL_RS = 0.0D0
                  GM1_RS = 0.0D0
                  GM2_RS = 0.0D0
                  PHI2_RS = 0.0D0
                  WFAC_RS = 0.0D0
                  LWC_RS = 0.0D0 ! Reset lwc if below limit
               END IF
               IF ( LCL_CV ) THEN
                  XCL_CV = 1.0D0
                  GM1_CV = 1.0D0
                  GM2_CV = 1.0D0
                  PHI2_CV = 1000.0D0 / 6.022d23 / (LWC_CV * 1.D-3)
                  WFAC_CV = WETFAC_CV ( COL,ROW,LEV )
               ELSE
                  XCL_CV = 0.0D0
                  GM1_CV = 0.0D0
                  GM2_CV = 0.0D0
                  PHI2_CV = 0.0D0
                  WFAC_CV = 0.0D0
                  LWC_CV = 0.0D0 ! Reset lwc if below limit
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
C.. End filling aqueous related data

C.. Convert CGRID to molec/cm^3
               DO SPC = 1,N_CGRID_SPC
                  ! If tracer is aerosol -> convert from ug m-3 to ppmV
                  IF ( CGRID_MASK_AERO( SPC ) ) THEN
                     CONVERT_VALUE = 1.0E-3 * MWAIR / CGRID_MW( SPC ) * VOL             ! ug m-3 to ppmV
                     CONVERT_VALUE = CONVERT_VALUE * CAIR * 1.D-6                       ! ppmV to molecules/cm3
                     CONC( COL,ROW,LEV,SPC ) = CGRID( COL,ROW,LEV,SPC ) * CONVERT_VALUE
                  ELSE
                     CONVERT_VALUE = CAIR * 1.D-6 ! ppmV to molecules/cm3
                     CONC( COL,ROW,LEV,SPC ) = CGRID( COL,ROW,LEV,SPC ) * CONVERT_VALUE
                  END IF
               END DO

               ! Fill CONC gas and aerosols
               C(:) = 0.0D0
               ! Init cloud concentrations first, since it impacts aerosol and gas phase concentrations
               IF ( LCL ) CALL INIT_CLOUDS ( CONC( COL,ROW,LEV,: ), LWC_RS, LWC_CV, PHI2_RS, PHI2_CV )
               DO SPC = 1, NSPEC
                  IF ( MECH_SPC(SPC)%CGRID_INDEX /= 0 ) THEN
                     ICGSPC = MECH_SPC(SPC)%CGRID_INDEX
                     ICSPC  = MECH_SPC(SPC)%CONC_INDEX
                     C( ICSPC ) = REAL( MAX( CONC( COL,ROW,LEV,ICGSPC ), CONCMIN), dp )
                  END IF
               END DO

               ! Set constant species concentrations
               C(IND_M)   = 1.0E+06    * CAIR * 1.D-6
               C(IND_H2)  = 0.56       * CAIR * 1.D-6
               C(IND_O2)  = 0.2095E+06 * CAIR * 1.D-6
               C(IND_CH4) = 1.85       * CAIR * 1.D-6
               C(IND_CO2) = 350.0D0    * CAIR * 1.D-6

               ! Set cloud water
               IF ( LCL_RS ) C(IND_H2O_RS) = LWC_RS / DENSI * QV_TO_PPM * CAIR * 1.D-6
               IF ( LCL_CV ) C(IND_H2O_CV) = LWC_CV / DENSI * QV_TO_PPM * CAIR * 1.D-6

               ! Fill water
               C(IND_H2O) = REAL( MAX( QV_TO_PPM * QV( COL,ROW,LEV ) *CAIR*1.D-6, 0.0 ), dp)

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
               DT     = CHEMSTEP
               TEND   = TSTART + DT

               CALL Update_RCONST()

               CALL INTEGRATE( TIN = TSTART, TOUT = TSTART+DT, RSTATUS_U = RSTATE,
     &                         ICNTRL_U = ICNTRL, IERR_U = IERR, ISTATUS_U = ISTATUS )

               !~~~> End Time loop
               C(:) = MAX(C(:),CONCMIN)
               ! END integration

               ! Fill CGRID
               DO SPC = 1, NSPEC
                  IF ( MECH_SPC(SPC)%CGRID_INDEX /= 0 ) THEN
                     ICGSPC = MECH_SPC(SPC)%CGRID_INDEX
                     ICSPC  = MECH_SPC(SPC)%CONC_INDEX
                     CONC( COL,ROW,LEV,ICGSPC ) = REAL( C( ICSPC ), 4 )
                  END IF
               END DO

               IF ( LCL ) CALL EVAP_CLOUDS ( CONC( COL,ROW,LEV,: ), WFAC_RS, WFAC_CV, DT )

C.. Convert CGRID to ppmV (gas) and ug m-3 (aerosol)
               DO SPC = 1,N_CGRID_SPC
                  ! If tracer is aerosol -> convert to ug m-3
                  IF ( CGRID_MASK_AERO( SPC ) ) THEN
                     CONVERT_VALUE = 1.0E+3 / MWAIR * CGRID_MW( SPC ) * DENSI           ! ug m-3 to ppmV
                     CONVERT_VALUE = CONVERT_VALUE / ( CAIR * 1.D-6 )                   ! ppmV to molecules/cm3
                     CGRID( COL,ROW,LEV,SPC ) = CONC( COL,ROW,LEV,SPC ) * CONVERT_VALUE
                  ELSE
                     CONVERT_VALUE = 1.0 / ( CAIR * 1.D-6 ) ! molecules/cm3 to ppmV
                     CGRID( COL,ROW,LEV,SPC ) = CONC( COL,ROW,LEV,SPC ) * CONVERT_VALUE
                  END IF
               END DO

            END DO ! ROW
         END DO ! COL
      END DO ! LEV

      !Update Aerosol Surface Area
      CALL HETCHEM_UPDATE_AERO( CGRID )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Output statistics if required and return
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

#ifdef mchem_stats
      WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )
      EDATE = JDATE
      ETIME = JTIME
      CALL NEXTIME( EDATE, ETIME, TSTEP( 2 ) )
      IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) ) THEN

         WSTEP = 0

         IF ( .NOT. WRITE3( CTM_MCHEMSTATS_1, VSTATS( 1 ),
     &         EDATE, ETIME, REAL( WCBAR_RS, 4) ) ) THEN
            XMSG = 'Could not write ' // CTM_MCHEMSTATS_1 // ' file'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         IF ( .NOT. WRITE3( CTM_MCHEMSTATS_1, VSTATS( 2 ),
     &         EDATE, ETIME, REAL( WCBAR_CV, 4 ) ) ) THEN
            XMSG = 'Could not write ' // CTM_MCHEMSTATS_1 // ' file'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

      END IF
#endif

      END
