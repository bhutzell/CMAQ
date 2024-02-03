
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

!-----------------------------------------------------------------------
      MODULE ELMO_PROC
!-----------------------------------------------------------------------
! This module will:
!      - collect all procedures used for subsetting the ELMO variables 
!        requested by the user. 
!      - open the ELMO output files and populate with metadata
!        reflecting the user-requested ELMO variables
!      - Calculate values of ELMO variables during the CCTM runtime
!
! Revision History:
! Ben Murphy 2020 Feb: Created
!-----------------------------------------------------------------------

      USE UTILIO_DEFN
      USE ELMO_DATA
      USE GRID_CONF, ONLY: NCOLS, NROWS, NLAYS, IO_PE_INCLUSIVE

      IMPLICIT NONE

      LOGICAL, SAVE :: INST_ACTIVE, AVRG_ACTIVE
      INTEGER, SAVE :: INST_LAYER_TOP, INST_LAYER_BOT,
     &                 AVRG_LAYER_TOP, AVRG_LAYER_BOT

      CHARACTER(16), ALLOCATABLE, SAVE :: 
     &               INST_VARS_NML( : ), AVRG_VARS_NML( : ),
     &               INST_PARS( : ),     AVRG_PARS( : ),
     &               INST_PARS_OUT(:),   AVRG_PARS_OUT(:),
     &               INST_UNIT_OUT(:),   AVRG_UNIT_OUT(:)

      CHARACTER(80), ALLOCATABLE, SAVE :: 
     &               INST_DESC_OUT( : ), AVRG_DESC_OUT( : )

      INTEGER, SAVE :: N_ELMO, N_ELMO_OUT, NTOT_ELMO_IDS, 
     &                 N_ELMO_INST, N_ELMO_AVRG,
     &                 N_ELMO_INST_OUT, N_ELMO_AVRG_OUT

      INTEGER, ALLOCATABLE, SAVE :: 
     &        MAP_INST2USED( : ), MAP_AVRG2USED( : ), 
     &        USED_ELMO_ID( : ), USED_ELMO_MODE( : ),
     &        INST_TYPE_OUT( : ), AVRG_TYPE_OUT( : ),
     &        MAP_ID2USED( :,: ),
     &        USED_CGRID_ID(:), USED_FAM_ID(:)

      CONTAINS


!-------------------------------------------------------------------------
      SUBROUTINE ELMO_DRIVER( CGRID, JDATE, JTIME, TSTEP, INIT_TIME )
!     This subroutine maps the PM diagnostic variables that the user has
!       requested to the entries in the ELMO_DATA table.
!-------------------------------------------------------------------------

      USE AERO_DATA, ONLY: AEROMODE_LNSG, DRY_AERO_DIAM, WET_AERO_DIAM,
     &                     DRY_AERO_M2, WET_AERO_M2, DRY_AERO_M3, 
     &                     WET_AERO_M3, N_MODE, MOMENT0_CONC, MOMENT2_CONC,
     &                     MOMENT3_CONC, WET_AERO_DENS, AEROSPC_CONC,
     &                     AH2O_IDX, AEROSPC, CALC_AERODIST_PARAMS,
     &                     Extract_Aero
      USE SOA_DEFN, ONLY : Extract_Soa
      USE AEROSOL_CHEMISTRY, ONLY: HETCHEM_RATES
      USE AEROMET_DATA, ONLY: AIRRH, AIRTEMP, AIRQV, AIRDENS, AIRPRES,
     &                        SRFTEMP, H2OVP, H2OSATVP, MWWAT, MWAIR,
     &                        CFRAC, DZ, PV, ZH
      use centralized_io_module, only : interpolate_var, pv_avail
      use RUNTIME_VARS
      Use phot_mod, Only: init_phot_shared 

#ifdef sens
      USE DDM3D_DEFN, ONLY : SENGRID
#endif 

      IMPLICIT NONE

      REAL, POINTER         :: CGRID( :,:,:,: ) ! Master concentration grid
      INTEGER, INTENT( IN ) :: JDATE, JTIME     ! Date and time inputs to 
                                                ! determine whether to write 
                                                ! concentrations
      INTEGER, INTENT( IN ) :: TSTEP(3)
      LOGICAL, INTENT( IN ) :: INIT_TIME        ! Is this the first time step 
                                                !   of the simulation
      LOGICAL :: INIT_STEP        ! Is the time step beginning

      ! Variable to set time step for writing visibility file
      INTEGER, SAVE :: WSTEP  = 0          ! local write counter
      LOGICAL, SAVE :: WRITE_STEP =.FALSE. ! local write flag

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
    
      ! Statement Function **************
      REAL, PARAMETER :: EPSWATER = MWWAT / MWAIR
      REAL ESATL ! arithmetic statement function for vapor pressure [Pa]
      REAL TT
      ! Coefficients for the equation, ESATL defining saturation vapor pressure
      REAL, PARAMETER :: AL = 610.94
      REAL, PARAMETER :: BL = 17.625
      REAL, PARAMETER :: CL = 243.04

      INTEGER C, R, L

      REAL, ALLOCATABLE, SAVE :: PRES(:,:,:), TA(:,:,:), TEMP2(:,:),
     &                           QV(:,:,:), DENS(:,:,:), CFRACS(:,:),
     &                           ZF(:,:,:), PVS(:,:,:), ZHS(:,:,:)

      ! values of AL, BL, and CL are from:
      ! Alduchov and Eskridge, "Improved Magnus Form Approximations of
      !                       Saturation Vapor Pressure,"
      !                       Jour. of Applied Meteorology, vol. 35,
      !                       pp 601-609, April, 1996.
      ESATL( TT ) = AL * EXP( BL * ( TT - 273.15 ) / ( TT - 273.15 + CL ) )
 

      IF ( FIRSTIME ) THEN
          FIRSTIME = .FALSE.
          ! Initialize Number of Steps Used for Calculating Average
          ELMO_NSTEP = 0.

          ALLOCATE( PRES( NCOLS, NROWS, NLAYS),
     &              TA( NCOLS, NROWS, NLAYS ),
     &              QV( NCOLS, NROWS, NLAYS ),
     &              DENS( NCOLS, NROWS, NLAYS ),
     &              CFRACS( NCOLS, NROWS ),
     &              ZF( NCOLS, NROWS, NLAYS ),
     &              ZHS( NCOLS, NROWS, NLAYS ),
     &              PVS( NCOLS, NROWS, NLAYS ),
     &              TEMP2( NCOLS, NROWS ) )
      END IF

      ! Determine if this is a write step
      WRITE_STEP = .FALSE.
      INIT_STEP  = .FALSE.
      IF ( ELMO_NSTEP .LT. 1.0 ) THEN
         WSTEP = 0
         INIT_STEP = .TRUE.
      ELSE
         WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )
         IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) )
     &        WRITE_STEP = .TRUE.
      END IF
      ELMO_NSTEP = ELMO_NSTEP + 1.0

      ! Get Meteorological Variables

      ! pressure [Pa]
      call interpolate_var ('PRES', jdate, jtime, PRES)

      ! temperature [K]
      call interpolate_var ('TA', jdate, jtime, TA)       ! Grid Cell Temp
      call interpolate_var ('TEMP2',jdate, jtime, TEMP2 ) ! 2-m Temp

      ! specific humidity [g H2O/g air]
      call interpolate_var ('QV', jdate, jtime, QV)

      ! air density [kg/m3]
      call interpolate_var ('DENS', jdate, jtime, DENS)
      
      ! get cloud fraction if photolysis hasn not been called yet
      call interpolate_var ('CFRAC', jdate, jtime, CFRACS)

      ! retrieve all layer heights. This is the height of the top of the
      ! layers
      call interpolate_var ('ZF', jdate, jtime, ZF)
      
      ! retrieve potential vorticity
      if ( pv_avail ) then
           call interpolate_var ('PV', jdate, jtime, PVS)
      else
#ifdef mpas
            PVS = -9.000E36
#else
            PVS = AMISS3
#endif
      end if
      
      ! retrieve layer midpoint heights. This is a mass weighted
      ! coordinate so is not necessarily consistent with the distance 
      ! between the heights of the layer bottom and top.
      call interpolate_var ('ZH', jdate, jtime, ZHS)

      ! Calculate Heterogeneous Chemistry Rates
      CALL INIT_PHOT_SHARED()
      CALL HETCHEM_RATES( TA, PRES, QV, CGRID, DENS )

      ! Process PM Diagnostics for Base Model
      DO C = 1,NCOLS
      DO R = 1,NROWS
      DO L = 1,NLAYS
         ! Grid cell meteorological data.
         AIRTEMP  = TA   ( C,R,L )
         SRFTEMP  = TEMP2( C,R )     ! 2-meter temperature (K)
         AIRPRES  = PRES ( C,R,L )   ! Note pascals
         AIRQV    = QV   ( C,R,L )
         AIRDENS  = DENS ( C,R,L )
         H2OSATVP = ESATL( AIRTEMP )
         H2OVP    = AIRPRES * AIRQV / ( EPSWATER  + AIRQV )
         AIRRH    = MAX( 0.005, MIN( 0.99, H2OVP / H2OSATVP ) ) ! 0-1
         IF ( L .EQ. 1 ) THEN
             DZ   = ZF( C,R,L )
             CFRAC= CFRACS( C,R ) 
         ELSE
             DZ   = ZF( C,R,L ) - ZF( C,R,L-1 )
#ifdef mpas
            CFRAC = -9.000E36
#else
            CFRAC = AMISS3
#endif
         END IF
         ZH       = ZHS( C,R,L )
         PV       = PVS( C,R,L )
 
         ! Extract Aerosols
#ifdef sens
         CALL EXTRACT_AERO( CGRID( C,R,L,: ), .TRUE., SENGRID( C,R,L,:,: ), .TRUE. )
         CALL EXTRACT_SOA( CGRID( C,R,L,: ), SENGRID( C,R,L,:,: ), .TRUE. )
#else
         CALL EXTRACT_AERO( CGRID( C,R,L,: ), .TRUE. )
         CALL EXTRACT_SOA( CGRID( C,R,L,: ) )
#endif         
         ! Populate Diagnostic Arrays
         CALL LOAD_ELMO( C,R,L, CGRID(C,R,L,:), WRITE_STEP, INIT_STEP, INIT_TIME )

      END DO
      END DO
      END DO

      END SUBROUTINE ELMO_DRIVER

!-------------------------------------------------------------------------
      SUBROUTINE LOAD_ELMO( C,R,L,CONC,WRITE_STEP, INIT_STEP, INIT_TIME )
!     This subroutine maps the PM diagnostic variables that the user has
!       requested to the entries in the ELMO_DATA table.
!-------------------------------------------------------------------------

      USE AERO_DATA, ONLY: AEROMODE_LNSG, DRY_AERO_DIAM, WET_AERO_DIAM,
     &                     DRY_AERO_M2, WET_AERO_M2, DRY_AERO_M3, 
     &                     WET_AERO_M3, N_MODE, MOMENT0_CONC, MOMENT2_CONC,
     &                     MOMENT3_CONC, WET_AERO_DENS, AEROSPC_CONC, DRY_AERO_DENS,
     &                     AH2O_IDX, AEROSPC, CALC_AERODIST_PARAMS
      USE AEROMET_DATA, ONLY: AIRRH

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: C, R, L
      INTEGER IDG, IM, AL, IL, IUSED, IAVRG, IINST

      REAL, INTENT( IN ) :: CONC( : )

      LOGICAL, INTENT( IN ) :: WRITE_STEP
      LOGICAL, INTENT( IN ) :: INIT_STEP
      LOGICAL, INTENT( IN ) :: INIT_TIME

      IF ( FIRST_TIME ) THEN
         ! Allocate local image of CGRID so it can be used each time step
         ALLOCATE( CONC_VEC( N_CGRID_SPC ), STAT=IOS )
         CALL CHECKMEM( IOS, 'CONC_VEC',PNAME )
      END IF

      ! Initalize Local Parameters
      C1                = C
      R1                = R
      L1                = L
      ELMO_LOCAL( : )   = 0.0
      INLET_FRAC(:,:,:) = -1.
      L_CALCULATED      = .FALSE.
      CONC_VEC( : )     = CONC( : )

      ! Check to make sure the current layer is requested for diagnostic
      ! output.
      IF ( ( L.GE.INST_LAYER_BOT .OR. L.GE.AVRG_LAYER_BOT ) .AND.
     &     ( L.LE.INST_LAYER_TOP .OR. L.LE.AVRG_LAYER_TOP ) ) THEN
       
        ! Calculate All Aerosol Size Distribution Parameters
        CALL CALC_AERODIST_PARAMS( INIT_TIME )
      
        ! Loop Through Used Variables List and Call ELMO Functions 
        ! to fill in Local Array. ELMO_LOCAL is the same length as the
        ! ELMO_LIST and is ordered by the ELMO ID numbers so that it
        ! can be used seamlessly with the CALC_ELMO internal order.
        DO ILOC = 1,N_ELMO_LOCAL
           IDG = ELMO_LOCAL_ID( ILOC ) ! Map to ELMO Variable ID numbers
           IM  = ELMO_LOCAL_IM( ILOC ) ! Specify mode of interest for
                                       !   mode-specific variables (e.g. WET_DG)
           IV  = ELMO_LOCAL_IV( ILOC ) ! Specify Variable Type (e.g. CGRID, FAM, 
                                       !   ISAM)
           ISRC= ELMO_LOCAL_ISRC( ILOC)! Specify Source Type, for ISAM or DDM
           CALL CALC_ELMO( IDG, IM, IV, ISRC, ELMO_LOCAL( ILOC ) )
        END DO

        ! *** Aggregate Diagnostic Variables in Average Array
        !     using trapezoidal rule
        IF ( AVRG_ACTIVE .AND. 
     &       L.GE.AVRG_LAYER_BOT .AND. L.LE.AVRG_LAYER_TOP ) THEN
           AL = L - AVRG_LAYER_BOT + 1
           DO  IAVRG = 1,N_ELMO_AVRG
              IF ( ELMO_LOCAL_ID( ELMO_AVRGtoLOCAL( IAVRG )) .EQ. ID_PMF_PH
     &             .AND. ELMO_LOCAL( ELMO_AVERGtoLOCAL( IAVRG ) ) .LT. -100. ) THEN
                 ! Special Averageing for Particle pH
                 ! Add whatever the current average is right now.
                 ! Certainly this is problematic, particularly
                 ! at the beginning of an hour where ELMO_AVRG =
                 ! 0. It's best to either use the instantaneous
                 ! value or recalculate the average fine particle acidity
                 ! offline.
                 IF ( INIT_STEP ) THEN
                    ELMO_AVRG( C,R,AL,IAVRG ) = 3.5  ! Assume relatively acidic particles
                 ELSE IF ( WRITE_STEP ) THEN
                    ELMO_AVRG( C,R,AL,IAVRG ) = ELMO_AVRG( C,R,AL,IAVRG ) 
                 ELSE
                    ELMO_AVRG( C,R,AL,IAVRG ) = ELMO_AVRG( C,R,AL,IAVRG ) 
     &                      + 2.0 * ELMO_AVRG( C,R,AL,IAVRG )
                 END IF
              ELSE
                 ! For all other variables, apply trapezoidal rule for
                 ! averaging.
                 IF ( INIT_STEP ) THEN
                    ELMO_AVRG( C,R,AL,IAVRG ) = ELMO_LOCAL( MAP_AVRG2USED( IAVRG ) ) 
                 ELSE IF ( WRITE_STEP ) THEN
                    ELMO_AVRG( C,R,AL,IAVRG ) = ELMO_AVRG( C,R,AL,IAVRG ) 
     &                                        + ELMO_LOCAL( MAP_AVRG2USED( IAVRG ) )
                 ELSE
                    ELMO_AVRG( C,R,AL,IAVRG ) = ELMO_AVRG( C,R,AL,IAVRG ) 
     &                      + 2.0 * ELMO_LOCAL( MAP_AVRG2USED( IAVRG ) )
                 END IF 
              END IF
           END DO
        END IF
       
        ! *** Populate Diagnostic Variables to Instantaneous Array if this is a write step
        IF ( INST_ACTIVE .AND. ( INIT_TIME .OR. WRITE_STEP ) .AND.
     &       L.GE.INST_LAYER_BOT .AND. L.LE.INST_LAYER_TOP ) THEN
           IL = L - INST_LAYER_BOT + 1
           DO IINST = 1,N_ELMO_INST_OUT
              ELMO_INST( C,R,IL,IINST ) = ELMO_LOCAL( MAP_INST2USED( IINST ) )
           END DO
        END IF
 
      END IF
      END SUBROUTINE LOAD_ELMO
 
!-------------------------------------------------------------------------
      SUBROUTINE CALC_ELMO( IDG, VAL )
!     This subroutine sums aggregate PM diagnostic parameters using the
!         ELMO_COEFFS structure as a guide for summing aerosol
!         components.          
!-------------------------------------------------------------------------
      USE AERO_DATA, ONLY : WET_AERO_M3, WET_AERO_M2, MOMENT0_CONC,
     &                      AH2O_IDX, AEROSPC, AEROSPC_CONC,
     &                      WET_AERO_DENS, N_MODE, AERO_MISSING,
     &                      AEROMODE
      USE PRECURSOR_DATA, ONLY : PRECURSOR_CONC
      USE AEROMET_DATA, ONLY : AIRDENS
      USE RUNTIME_VARS

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: IDG
      REAL, INTENT( INOUT ) :: VAL
      REAL VAL2
      INTEGER IVAR, ILIST
      LOGICAL L_CALC( N_ELMO )
      REAl    COEFF
      INTEGER IM, WETNESS, ISPEC, INLET
      LOGICAL LMODE
      REAL    FRAC( N_MODE )

      CHARACTER( 500 ) :: XMSG

      ! For most variables the ELMO_TABLE structure will
      ! define all of the mappings and coefficients needed for
      ! a particular summation. This can therefore be used in
      ! most cases. Variables in this category are linear
      ! combinations of CMAQ species. For example, PMF_SO4 = 
      ! ASO4I + ASO4J.

      ! If this Aggregate has not been mapped already, then return an
      ! error and crash the model
      IF ( .NOT. ELMO_COEFFS( IDG )%L_MAPPED ) THEN
         ILIST = MAP_ID2LIST( IDG )  
         XMSG = 'ELMO variable ' // TRIM( ELMO_LIST( ILIST )%NAME ) //
     &          ' has not been mapped to a procedure within ELMO_PROC even ' //
     &          'though it does appear to be an acceptable variable on the ' //
     &          'ELMO_LIST. If you have added this variable yourself, make ' //
     &          'sure it is being mapped to an actual calculation in CALC_ELMO ' //
     &          'or MAP_ELMO_COEFFS. If this variable is needed for a different ' //
     &          'aggregate variable you requested, please add this variable ' //
     &          'explicitly as an ELMO requested output.'
         CALL LOG_MESSAGE( LOGDEV, XMSG )
         CALL M3EXIT( PNAME, 0, 0, '', XSTAT1 )
      END IF

      ! Initialize value of aggregate variable
      VAL = 0.0
      
      ! Retrieve size cutoff if necessary
      INLET = ELMO_TABLE( J )%INLET
      WETNESS = ELMO_TABLE( J )%WETNESS
      DO IM = 1,N_MODE
         CALL GET_AERO_INLET( INLET, IM, WETNESS, FRAC( IM ) )
      END DO
      
      ! Loop through N_VARS and add species together
      DO IVAR = 1,ELMO_COEFFS( IDG )%N_VARS
          ISPEC = ELMO_COEFFS( IDG )%I_SPEC( IVAR )
          COEFF = ELMO_COEFFS( IDG )%COEFF( IVAR )
          SPC_UNIT = ELMO_COEFFS( IDG )%UNIT_SP( IVAR )
          MW    = ELMO_COEFFS( IDG )%MW( IVAR )
      
          FAC = COEFF
          ! Calculate Unit Conversions
          SELECT CASE ( SPC_UNIT )
            CASE ( 0 ) ! ug m-3
              IF ( OUT_UNIT .EQ. 0 ) THEN
                FAC = FAC 
              ELSE IF ( OUT_UNIT .EQ. 1 ) THEN
                FAC = FAC / MW * MWAIR / AIRDENS / 1000.0 ! to ppmv
              ELSE IF ( OUT_UNIT .EQ. 2 ) THEN
                FAC = FAC * ELMO_COEFFS( IDG )%CNUM( IVAR ) * 
     &                      12.0 / MW ! to ugC m-3
              ELSE IF ( OUT_UNIT .EQ. 3 ) THEN
                FAC = FAC / MW * MWAIR / AIRDENS / 1000.0 * 
     &                      ELMO_COEFFS( IDG )%CNUM( IVAR ) ! to ppmC
              ELSE IF ( OUT_UNIT .EQ. 4 ) THEN
                FAC = FAC * ELMO_COEFFS( IDG )%NNUM( IVAR ) * 
     &                      28.02 / MW ! to ugN m-3
              ELSE IF ( OUT_UNIT .EQ. 5 ) THEN
                FAC = FAC / MW * MWAIR / AIRDENS / 1000.0 * 
     &                      ELMO_COEFFS( IDG )%NNUM( IVAR ) ! to ppmN
              END IF
            CASE ( 1 ) ! ppmv
              IF ( OUT_UNIT .EQ. 0 ) THEN
                FAC = FAC * MW / MWAIR * AIRDENS * 1000.0 ! to ug m-3
              ELSE IF ( OUT_UNIT .EQ. 1 ) THEN
                FAC = FAC ! to ppmv
              ELSE IF ( OUT_UNIT .EQ. 2 ) THEN
                FAC = FAC / MWAIR * AIRDENS * 1000.0 * 
     &                      ELMO_COEFFS( IDG )%CNUM( IVAR ) *
     &                      12.0 ! to ugC m-3
              ELSE IF ( OUT_UNIT .EQ. 3 ) THEN
                FAC = FAC * ELMO_COEFFS( IDG )%CNUM( IVAR ) ! to ppmC
              ELSE IF ( OUT_UNIT .EQ. 4 ) THEN
                FAC = FAC / MWAIR * AIRDENS * 1000.0 * 
     &                      ELMO_COEFFS( IDG )%NNUM( IVAR ) * 
     &                      28.02 ! to ugN m-3
              ELSE IF ( OUT_UNIT .EQ. 5 ) THEN
                FAC = FAC * ELMO_COEFFS( IDG )%NNUM( IVAR ) ! to ppmN
              END IF 
          END SELECT 
         
          ! Add up variable while applying coefficient, size parameter 
          ! (for aerosols) and unit conversion
          IF ( ISPEC .GT. 0 ) THEN
             IF ( ELMO_COEFFS( IDG )%L_AERO( IVAR ) ) THEN
                 DO IM = 1,N_MODE
                   ! If this mode is supposed to be added (LMODE) and the
                   ! species is not missing, then add it up
                   LMODE = ELMO_COEFFS( IDG )%L_MODE( IM )
                   IF ( LMODE .AND. ( .NOT. AERO_MISSING( ISPEC,IM ) ) ) 
     &                 VAL = VAL + AEROSPC_CONC( ISPEC, IM ) * 
     &                             FRAC( IM )* FAC
                 END DO
                 
             ELSE IF ( ELMO_COEFFS( IDG )%L_CGRID( IVAR ) ) THEN
                 VAL = VAL + CONC_VEC( ISPEC ) * FAC
                  
             ELSE IF ( ELMO_COEFFS( IDG )%L_AGG( IVAR ) ) THEN
                 CALL CALC_ELMO( ISPEC, 1, VAL2 )
                 VAL = VAL + VAL2 * FAC
         
             ELSE IF ( ELMO_COEFFS( IDG )%L_FAM( IVAR ) ) THEN
                 CALL CALC_ELMO( ISPEC, 1, VAL2 )
                 VAL = VAL + VAL2 * FAC
         
             END IF
         
          END IF
         
      END DO 
 
      END SUBROUTINE CALC_ELMO
 
 
!-------------------------------------------------------------------------
      RECURSIVE SUBROUTINE CALC_ELMO_ELAB( IDG, IM, IV, ISRC, OUTVAL )
!     This subroutine maps the PM diagnostic variables that the user has
!       requested to the entries in the ELMO_DATA table.
!-------------------------------------------------------------------------

      USE AERO_DATA, ONLY: AEROMODE_LNSG, DRY_AERO_DIAM, WET_AERO_DIAM,
     &                     DRY_AERO_M2, WET_AERO_M2, DRY_AERO_M3, 
     &                     WET_AERO_M3, N_MODE, MOMENT0_CONC, MOMENT2_CONC,
     &                     MOMENT3_CONC, WET_AERO_DENS, AEROSPC_CONC,
     &                     AH2O_IDX, AORGH2O_IDX, AEROSPC, CALC_AERODIST_PARAMS,
     &                     AEROMODE, DRY_AERO_DENS, APOC_IDX, APNCOM_IDX
      USE AEROMET_DATA, ONLY: AIRRH, PI, AIRTEMP, SRFTEMP, AIRPRES, DZ, CFRAC, PV, ZH
      USE CGRID_SPCS, ONLY : RHOJ_LOC
      USE SOA_DEFN, ONLY : N_OA, N_OA_NotTracers, OASPC, MAP_OAtoAERO
      USE RUNTIME_VARS, ONLY : PHOTDIAG
      USE ASX_DATA_MOD, ONLY : MET_DATA

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: IDG, IM, IV, ISRC
      REAL, INTENT( INOUT ) :: OUTVAL
      INTEGER ILOC, IMODE, IVAR, IOA
      REAL VAL4, VAL3, VAL2, VAL, FRAC( N_MODE )
      REAL PM01, PM1, PM25, PM10, PMAMS, PM25to10, POM, POC, OtoC
      REAL K, P1, P2, P3, a, K2, PMF_NO3, PMF_NO3_LOSS,
     &     PMF_NH4_LOSS, PMF_NH4, PMF_SO4, PMF_MASS,
     &     PM25_NH4, PM25_NO3, PM25_SO4, PM25_NO3_LOSS,
     &     PM25_NH4_LOSS

      ! Map this combination of ID number and mode to the USED
      ! variables list, which is how ELMO_LOCAL is defined.
      ILOC = ELMO_IDtoLOCAL( IDG, IM, IV, ISRC )

      ! First determine if this variable has been mapped already. If it
      ! has, send it back
      IF ( ILOC .GT. 0 ) THEN
        IF ( L_CALCULATED( ILOC ) ) THEN
           OUTVAL = ELMO_LOCAL( ILOC )
           RETURN
        END IF
      END IF

      OUTVAL = 0.0

      ! Determine which variable to calculate
      SELECT CASE( IDG )

         ! Retrieve Standard Deviation of Each Mode
         CASE ( ID_STDEV )
            OUTVAL = EXP( AEROMODE_LNSG( IM ) )

         ! Retrieve Mode Dry Diameter
         CASE ( ID_DRY_DG )
            OUTVAL = DRY_AERO_DIAM( IM )  ! dry diam.

         ! Retrieve Mode Wet Diameter
         CASE ( ID_WET_DG )
            OUTVAL = WET_AERO_DIAM( IM )  ! wet diam.

         ! Retrieve Mode Wet Second Moment
         CASE ( ID_WET_M2 )
            OUTVAL = WET_AERO_M2( IM )  ! dry M2

         ! Retrieve Dry Third Moment
         CASE ( ID_DRY_M3 )
            OUTVAL = DRY_AERO_M3( IM )  ! dry M3

         ! Retrieve Wet Third Moment
         CASE ( ID_WET_M3 )
            OUTVAL = WET_AERO_M3( IM )  ! wet M3

         ! Retrieve Total Particle Number
         CASE ( ID_PM_NUM )
            OUTVAL = SUM( MOMENT0_CONC( : ) ) ! N m-3

         ! Retrieve Ultrafine-Particle Particle Number
         CASE ( ID_PMU_NUM )
            OUTVAL = SUM( MOMENT0_CONC( : ), 
     &                          MASK=AEROMODE(:)%ULTRAFINE_MASK ) ! N m-3
         
         ! Retrieve Fine-Particle Particle Number
         CASE ( ID_PMF_NUM )
            OUTVAL = SUM( MOMENT0_CONC( : ), 
     &                          MASK=AEROMODE(:)%FINE_MASK ) ! N m-3
         
         ! Retrieve Coarse-Particle Particle Number
         CASE ( ID_PMC_NUM )
            OUTVAL = SUM( MOMENT0_CONC( : ), 
     &                          MASK=AEROMODE(:)%COARSE_MASK ) ! N m-3

         ! Retrieve N10 - Number of Particles Greater than 10 nm
         CASE ( ID_N10 )
            CALL SUM_INLET_NUM( INUM10, IWET, OUTVAL )

         ! Retrieve N20 - Number of Particles Greater than 20 nm
         CASE ( ID_N20 )
            CALL SUM_INLET_NUM( INUM20, IWET, OUTVAL )

         ! Retrieve N40 - Number of Particles Greater than 40 nm
         CASE ( ID_N40 )
            CALL SUM_INLET_NUM( INUM40, IWET, OUTVAL )

         ! Retrieve N100 - Number of Particles Greater than 100 nm
         CASE ( ID_N100 )
            CALL SUM_INLET_NUM( INUM100, IWET, OUTVAL )

         ! Retrieve Total Particle Surface Area
         CASE ( ID_PM_SRF )
            OUTVAL = PI * SUM( MOMENT2_CONC( : ) ) ! N m-3

         ! Retrieve Ultrafine Particle Surface Area
         CASE (ID_PMU_SRF )
            OUTVAL = PI * SUM( MOMENT2_CONC( : ), 
     &                          MASK=AEROMODE(:)%ULTRAFINE_MASK ) ! N m-3

         ! Retrieve Fine Particle Surface Area
         CASE ( ID_PMF_SRF )
            OUTVAL = PI * SUM( MOMENT2_CONC( : ), 
     &                          MASK=AEROMODE(:)%FINE_MASK ) ! N m-3

         ! Retrieve Coarse Particle Surface Area
         CASE ( ID_PMC_SRF )
            OUTVAL = PI * SUM( MOMENT2_CONC( : ), 
     &                          MASK=AEROMODE(:)%COARSE_MASK ) ! N m-3

         ! Retrieve Total Particle Mass
         CASE ( ID_PM_MASS )
           SELECT CASE ( IV )
            CASE ( ETYPE_CGRID )
              DO IMODE = 1,N_MODE
                 OUTVAL = OUTVAL +
     &             SUM( AEROSPC_CONC( :,IMODE ),
     &                  MASK=.NOT.AEROSPC(:)%TRACER ) ! ug m-3
     &              - AEROSPC_CONC( AH2O_IDX,IMODE )
                 IF ( AORGH2O_IDX .GT. 0 ) OUTVAL = OUTVAL
     &              - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3 
              END DO
            CASE ( ETYPE_ISAM )

            CASE ( ETYPE_DDM )

           END SELECT
             

         ! Retrieve Ultrafine Particle Mass
         CASE ( ID_PMU_MASS )
            DO IMODE = 1,N_MODE
               IF ( AEROMODE( IMODE )%ULTRAFINE_MASK ) THEN
                    OUTVAL = OUTVAL +
     &                 SUM( AEROSPC_CONC( :,IMODE ),
     &                      MASK=.NOT.AEROSPC(:)%TRACER ) ! ug m-3
     &                  - AEROSPC_CONC( AH2O_IDX,IMODE )
                    IF ( AORGH2O_IDX .GT. 0 ) OUTVAL = OUTVAL
     &                  - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3
               END IF
            END DO

         ! Retrieve Fine Particle Mass
         CASE ( ID_PMF_MASS )
            DO IMODE = 1,N_MODE
               IF ( AEROMODE( IMODE )%FINE_MASK ) THEN
                    OUTVAL = OUTVAL +
     &                 SUM( AEROSPC_CONC( :,IMODE ),
     &                      MASK=.NOT.AEROSPC(:)%TRACER )
     &                  - AEROSPC_CONC( AH2O_IDX,IMODE )
                    IF ( AORGH2O_IDX .GT. 0 ) OUTVAL = OUTVAL
     &                  - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3
               END IF
            END DO

         ! Retrieve Coarse Particle Mass
         CASE ( ID_PMC_MASS )
            DO IMODE = 1,N_MODE
               IF ( AEROMODE( IMODE )%COARSE_MASK ) THEN
                    OUTVAL = OUTVAL +
     &                 SUM( AEROSPC_CONC( :,IMODE ),
     &                      MASK=.NOT.AEROSPC(:)%TRACER ) ! ug m-3
     &                  - AEROSPC_CONC( AH2O_IDX,IMODE )
                    IF ( AORGH2O_IDX .GT. 0 ) OUTVAL = OUTVAL
     &                  - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3
               END IF
            END DO

         ! Retrieve Nucleation-Mode Particle Mass
         CASE ( ID_PMNUC_MASS )
            DO IMODE = 1,N_MODE
               IF ( AEROMODE( IMODE )%NUC_MASK ) THEN
                    OUTVAL = OUTVAL +
     &                 SUM( AEROSPC_CONC( :,IMODE ),
     &                      MASK=.NOT.AEROSPC(:)%TRACER ) ! ug m-3
     &                  - AEROSPC_CONC( AH2O_IDX,IMODE )
                    IF ( AORGH2O_IDX .GT. 0 ) OUTVAL = OUTVAL
     &                  - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3
               END IF
            END DO

         ! Retrieve Aitken-Mode Particle Mass
         CASE ( ID_PMAIT_MASS )
            DO IMODE = 1,N_MODE
               IF ( AEROMODE( IMODE )%AITKEN_MASK ) THEN
                    OUTVAL = OUTVAL +
     &                 SUM( AEROSPC_CONC( :,IMODE ),
     &                      MASK=.NOT.AEROSPC(:)%TRACER ) ! ug m-3
     &                  - AEROSPC_CONC( AH2O_IDX,IMODE )
                    IF ( AORGH2O_IDX .GT. 0 ) OUTVAL = OUTVAL
     &                  - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3
               END IF
            END DO

         ! Retrieve Accumution-Mode Particle Mass
         CASE ( ID_PMACC_MASS )
            DO IMODE = 1,N_MODE
               IF ( AEROMODE( IMODE )%ACCUM_MASK ) THEN
                    OUTVAL = OUTVAL +
     &                 SUM( AEROSPC_CONC( :,IMODE ),
     &                      MASK=.NOT.AEROSPC(:)%TRACER ) ! ug m-3
     &                  - AEROSPC_CONC( AH2O_IDX,IMODE )
                    IF ( AORGH2O_IDX .GT. 0 ) OUTVAL = OUTVAL
     &                  - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3
               END IF
            END DO

         ! Retrieve Dry Mode Density [kg m-3]
         CASE ( ID_DRY_DENS )
            OUTVAL = DRY_AERO_DENS( IM )  ! dry i-mode Density

         ! Retrieve Wet Mode Density [kg m-3]
         CASE ( ID_WET_DENS )
            OUTVAL = WET_AERO_DENS( IM )  ! wet i-mode Density

         ! Retrieve PM01 Fraction in Each Mode
         CASE ( ID_FPM01 )
            CALL GET_AERO_INLET( IPM01, IM, IWET, VAL ) 
            OUTVAL = VAL

         ! Retrieve PM1 Fraction in Each Mode
         CASE ( ID_FPM1 )
            CALL GET_AERO_INLET( IPM1, IM, IWET, VAL ) 
            OUTVAL = VAL

         ! Retrieve PM25 Fraction in Each Mode
         CASE ( ID_FPM25 )
            CALL GET_AERO_INLET( IPM25, IM, IWET, VAL ) 
            OUTVAL = VAL

         ! Retrieve PM10 Fraction in Each Mode
         CASE ( ID_FPM10 )
            CALL GET_AERO_INLET( IPM10, IM, IWET, VAL ) 
            OUTVAL = VAL

         ! Retrieve PM2.5-10 Fraction in Each Mode
         CASE ( ID_FPM25to10 )
            CALL GET_AERO_INLET( IPM25to10, IM, IWET, VAL ) 
            OUTVAL = VAL

         ! Retrieve AMS Fraction in Each Mode
         CASE ( ID_FAMS )
            CALL GET_AERO_INLET( IPMAMS, IM, IWET, VAL ) 
            OUTVAL = VAL
 
         ! Retrieve Bulk PM0.1 Concentration
         CASE ( ID_PM01 )
            CALL SUM_INLET_PM( IPM01, IWET, PM01 )
            OUTVAL = PM01

         ! Retrieve Bulk PM1.0 Concentration
         CASE ( ID_PM1 )
            CALL SUM_INLET_PM( IPM1, IWET, PM1 )
            OUTVAL = PM1

         ! Retrieve Bulk PM2.5 Concentration
         CASE ( ID_PM25 )
            CALL SUM_INLET_PM( IPM25, IWET, PM25 )
            OUTVAL = PM25 

         ! Retrieve Bulk PM10.0 Concentration
         CASE ( ID_PM10 )
            CALL SUM_INLET_PM( IPM10, IWET, PM10 )
            OUTVAL = PM10
         
         ! Retrieve Bulk PM2.5-10.0 Concentration
         CASE ( ID_PM25to10 )
            CALL SUM_INLET_PM( IPM25to10, IWET, PM25to10 )
            OUTVAL = PM25to10

         ! Retrieve Bulk PM Concentration Relevant for the AMS
         CASE ( ID_PMAMS )
            CALL SUM_INLET_PM( IPMAMS, IWET, PMAMS )
            OUTVAL = PMAMS
         
         ! Calculate AHPMOLAL - Fine Mode Concentration of H+ in
         ! Particle Water
         CASE ( ID_PMF_HPM )
            ! Retrieve AHPLUSIJ
            CALL CALC_ELMO( ID_PMF_HP, 1, VAL )
            ! Retrieve AH2OIJ
            CALL CALC_ELMO( ID_PMF_H2O, 1, VAL2 )
            ! HPMOLAL = AHPLUSIJ / AH2OIJ * 1000.0 [mol L-1]
            OUTVAL = VAL / VAL2 * 1000.0

         ! Calculate PMF_PH - Fine-Mode pH
         CASE ( ID_PMF_PH )
            ! Retrieve HPMOLAL
            CALL CALC_ELMO( ID_PMF_HPM, 1, VAL )
            ! Retrieve AH2OIJ
            CALL CALC_ELMO( ID_PMF_H2O, 1, VAL2 )

            !PMF_PH = -1*LOG10(HPMOLAL)
            OUTVAL = BADVAL3
            IF ( VAL2 .GT. 0.01 )
     &           OUTVAL = -1.0 * LOG10( VAL )

         ! Calculate POCIJ - Fine-Mode Primary Organic Carbon Mass
         CASE ( ID_PMF_POC )
            ! Sum up primary organic aerosol species normalized to
            ! just carbon mass
            VAL = 0.0
            DO IOA = 1,N_OA_NotTracers
                ! Just Select Primary Species
                IF ( OASPC( IOA )%PRIMARY ) THEN  
                   IF ( OASPC( IOA )%OMtoOC .gt. 0.0 ) 
     &                VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ),
     &                            MASK = AEROMODE(:)%FINE_MASK ) 
     &                          / OASPC( IOA )%OMtoOC
                END IF
            END DO
            OUTVAL = VAL
      
         ! Calculate SOCIJ - Fine-Mode Secondary Organic Carbon Mass
         CASE ( ID_PMF_SOC )
            ! Sum up primary organic aerosol species normalized to
            ! just carbon mass
            VAL = 0.0
            DO IOA = 1,N_OA_NotTracers
                ! Just Select Secondary Species
                IF ( .NOT. OASPC( IOA )%PRIMARY ) THEN  
                      VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ),
     &                            MASK = AEROMODE(:)%FINE_MASK ) 
     &                          / OASPC( IOA )%OMtoOC
                END IF
            END DO
            OUTVAL = VAL 
       
         ! Calculate OCIJ - Fine-Mode Organic Carbon Mass
         CASE ( ID_PMF_OC )
            ! Sum up primary organic aerosol species normalized to
            ! just carbon mass
            VAL = 0.0
            DO IOA = 1,N_OA_NotTracers
               VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ),
     &                     MASK = AEROMODE(:)%FINE_MASK ) 
     &                   / OASPC( IOA )%OMtoOC
            END DO
            OUTVAL = VAL  

         ! Calculate PMF_POA - Fine-Mode Primary Organic Aerosol Mass
         CASE ( ID_PMF_POA )
            ! Sum up primary organic aerosol species 
            VAL = 0.0
            DO IOA = 1,N_OA_NotTracers
                ! Just Select Primary Species
                IF ( OASPC( IOA )%PRIMARY ) 
     &             VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ),
     &                         MASK = AEROMODE(:)%FINE_MASK ) 
            END DO
            OUTVAL = VAL
 
         ! Calculate PMF_SOA - Fine-Mode Secondary Organic Aerosol Mass
         CASE ( ID_PMF_SOA )
            ! Sum up primary organic aerosol species 
            VAL = 0.0
            DO IOA = 1,N_OA_NotTracers
                ! Just Select Secondary Species
                IF ( .NOT.OASPC( IOA )%PRIMARY ) 
     &             VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ),
     &                         MASK = AEROMODE(:)%FINE_MASK ) 
            END DO
            OUTVAL = VAL
 
         ! Calculate PMF_OA - Fine-Mode Organic Aerosol Mass
         CASE ( ID_PMF_OA )
            ! Sum up primary organic aerosol species 
            VAL = 0.0
            DO IOA = 1,N_OA_NotTracers
                VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ),
     &                      MASK = AEROMODE(:)%FINE_MASK ) 
            END DO
            OUTVAL = VAL    
         
         ! Calculate PMF_ASOA - Fine-Mode Anthropogenic OA Mass
         CASE ( ID_PMF_ASOA )
            ! Sum up anthropogenic oa species 
            VAL = 0.0
            DO IOA = 1,N_OA_NotTracers
                ! Just Select Anthropogenic Species
                IF ( .NOT.OASPC( IOA )%PRIMARY .AND. OASPC( IOA )%ANTHRO  ) 
     &             VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ),
     &                         MASK = AEROMODE(:)%FINE_MASK ) 
            END DO
            OUTVAL = VAL
          
         ! Calculate PMF_BSOA - Fine-Mode Biogenic SOA Mass
         CASE ( ID_PMF_BSOA )
            ! Sum up biogenic soa species 
            VAL = 0.0
            DO IOA = 1,N_OA_NotTracers 
                ! Just Select Biogenic Secondary Species
                IF ( .NOT.OASPC( IOA )%PRIMARY .AND. OASPC( IOA )%BIOG  ) 
     &             VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ),
     &                         MASK = AEROMODE(:)%FINE_MASK ) 
            END DO
            OUTVAL = VAL
 
         ! Calculate PMF_OMOC - Fine-Mode OM:OC 
         CASE ( ID_PMF_OMOC )
            VAL  = 0.0
            VAL2 = 0.0
            CALL CALC_ELMO( ID_PMF_OA, 1, VAL  )
            CALL CALC_ELMO( ID_PMF_OC, 1, VAL2 )
            OUTVAL = VAL / ( VAL2 + TINY( 0.0 ) )
            
         ! Calculate PMF_OtoC - Fine-Mode O:C 
         CASE ( ID_PMF_OtoC )
            CALL CALC_ELMO( ID_PMF_OA, 1, VAL  )

            VAL2 = 0.0
            DO IOA = 1,N_OA_NotTracers
               IF ( MAP_OAtoAERO(IOA) .NE. APOC_IDX .AND.
     &              MAP_OAtoAERO(IOA) .NE. APNCOM_IDX ) THEN
                  ! Weight OA Sum by O:C
                  VAL2 = VAL2 + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ), 
     &                               MASK = AEROMODE(:)%FINE_MASK )        
     &                          * OASPC(IOA)%OtoC                    
               END IF
            END DO
            ! Add POC/PNCOM Contribution to Weighted O:C
            POC = SUM( AEROSPC_CONC(APOC_IDX,:),MASK = AEROMODE(:)%FINE_MASK )
            POM = POC + SUM( AEROSPC_CONC(APNCOM_IDX,:), MASK = AEROMODE(:)%FINE_MASK )
            OtoC = 12./15. * ( POM / ( POC + TINY(0.0) ) )- 14./15.  
            
            OUTVAL = ( VAL2 + OtoC*POM ) / ( VAL + TINY(0.0))

         ! Calculate PM1_OC - PM1.0 Organic Carbon Mass
         CASE ( ID_PM1_OC )
            ! Save PM1 Fraction for each mode
            DO IMODE = 1,N_MODE
                CALL GET_AERO_INLET( IPM1, IMODE, IWET, FRAC( IMODE ) ) 
            END DO
            ! Sum up primary organic aerosol species normalized to
            ! just carbon mass
            VAL = 0.0
            DO IOA = 1,N_OA_NotTracers
               IF ( OASPC( IOA )%OMtoOC .GE. 0.0 ) 
     &              VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ) * FRAC(:))
     &                         / OASPC( IOA )%OMtoOC
            END DO
            OUTVAL = VAL  
 
         ! Calculate PM1_OM - PM1.0 Organic Aerosol Mass
         CASE ( ID_PM1_OA )
            ! Save PM1 Fraction for each mode
            DO IMODE = 1,N_MODE
                CALL GET_AERO_INLET( IPM1, IMODE, IWET, FRAC( IMODE ) ) 
            END DO
            ! Sum up primary organic aerosol species
            VAL = 0.0
            DO IOA = 1,N_OA_NotTracers
               VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ) * FRAC(:))
            END DO
            OUTVAL = VAL  
 
         ! Calculate PM25_OC - PM2.5 Organic Carbon Mass
         CASE ( ID_PM25_OC )
            ! Save PM2.5 Fraction for each mode
            DO IMODE = 1,N_MODE
                CALL GET_AERO_INLET( IPM25, IMODE, IWET, FRAC( IMODE ) ) 
            END DO
            ! Sum up primary organic aerosol species normalized to
            ! just carbon mass
            VAL = 0.0
            DO IOA = 1,N_OA_NotTracers
               IF ( OASPC( IOA )%OMtoOC .GE. 0.0 ) 
     &              VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ) * FRAC(:) )
     &                        / OASPC( IOA )%OMtoOC
            END DO
            OUTVAL = VAL  
 
         ! Calculate PM25_OM - PM2.5 Organic Aerosol Mass
         CASE ( ID_PM25_OA )
            ! Save PM2.5 Fraction for each mode
            DO IMODE = 1,N_MODE
                CALL GET_AERO_INLET( IPM25, IMODE, IWET, FRAC( IMODE ) ) 
            END DO
            ! Sum up primary organic aerosol species
            VAL = 0.0
            DO IOA = 1,N_OA_NotTracers
               VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ) * FRAC(:) )
            END DO
            OUTVAL = VAL  
 
         ! Retrieve OA Concentration Relevant for the AMS
         CASE ( ID_AMS_OA )
            ! Save AMS Fraction for each mode
            DO IMODE = 1,N_MODE
                CALL GET_AERO_INLET( IPMAMS, IMODE, IWET, FRAC( IMODE ) ) 
            END DO
            ! Sum up OA AMS mass
            VAL = 0.0
            DO IOA = 1,N_OA_NotTracers
               VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ) * FRAC(:) )
            END DO
              
         ! Calculate O:C relevant for AMS observations
         CASE ( ID_AMS_OtoC )
            CALL CALC_ELMO( ID_AMS_OA, 1, VAL  )

            ! Save AMS Fraction for each mode
            DO IMODE = 1,N_MODE
                CALL GET_AERO_INLET( IPMAMS, IMODE, IWET, FRAC( IMODE ) ) 
            END DO

            VAL2 = 0.0
            VAL4 = 0.0
            DO IMODE = 1,N_MODE
               DO IOA = 1,N_OA_NotTracers
                  IF ( MAP_OAtoAERO(IOA) .NE. APOC_IDX .AND.
     &                 MAP_OAtoAERO(IOA) .NE. APNCOM_IDX ) THEN
                     ! Weight OA Sum by O:C
                     VAL2 = VAL2 + AEROSPC_CONC( MAP_OAtoAERO(IOA),IMODE )        
     &                         * OASPC(IOA)%OtoC * FRAC( IMODE )
                  END IF
               END DO
               ! Add POC/PNCOM Contribution to Weighted O:C
               VAL3 = ( AEROSPC_CONC(APOC_IDX,IMODE) 
     &                + AEROSPC_CONC(APNCOM_IDX,IMODE) ) * FRAC( IMODE )
               VAL3 = ( 12./15. * ( VAL3 / ( AEROSPC_CONC(APOC_IDX,IMODE)
     &                                      *FRAC(IMODE) 
     &                           + TINY(0.0) ) ) - 14./15. ) * VAL3
               VAL4 = VAL4 + VAL3
            END DO
            OUTVAL = ( VAL2 + VAL4 ) / ( VAL + TINY(0.0))
 
         ! Calculate Aerosol Fraction of Benzo-a-Pyrene
         CASE ( ID_BENAPY_FAERO )
            CALL CALC_ELMO( ID_PMF_BENAPY, 1, VAL  )
            CALL CALC_ELMO( ID_GAS_BENAPY, 1, VAL2  )
            OUTVAL = VAL / ( VAL + VAL2 + TINY( 0.0 ) )

         ! Retrieve Fine-Mode Particle Mass Collected by the Federal
         ! Reference Method compliant sampler
         CASE ( ID_PMF_FRM )
            K = EXP( 118.87 - 24084/SRFTEMP - 6.025*LOG( SRFTEMP ) )
            P1 = EXP( 8763/SRFTEMP + 19.12*LOG( SRFTEMP ) - 135.94 )
            P2 = EXP( 9969/SRFTEMP + 16.22*LOG( SRFTEMP ) - 122.65 )
            P3 = EXP( 13875/SRFTEMP + 24.46*LOG( SRFTEMP ) - 182.61 )
            a = 1 - AIRRH 
            IF ( AIRRH .LE. 0.61 ) THEN
                K2 = K ** 0.5
            ELSE
                K2 = ( K * ( P1 - P2*a + P3*a*a ) * ( a**1.75 ) ) ** 0.5
            END IF

            CALL CALC_ELMO( ID_PMF_NO3, 1, PMF_NO3 )
            PMF_NO3_LOSS = MIN( 745.7/SRFTEMP * K2, PMF_NO3 )

            PMF_NH4_LOSS = PMF_NO3_LOSS * 18.0 / 62.0

            CALL CALC_ELMO( ID_PMF_NH4, 1, PMF_NH4 )
            CALL CALC_ELMO( ID_PMF_SO4, 1, PMF_SO4 ) 
            CALL CALC_ELMO( ID_PMF_MASS,1, PMF_MASS) 

            OUTVAL = PMF_MASS - PMF_NO3_LOSS - PMF_NH4_LOSS
     &                      +0.24 * ( PMF_SO4 + PMF_NH4 - PMF_NH4_LOSS ) + 0.5
          
         ! Retrieve PM2.5 Particle Mass Collected by the Federal
         ! Reference Method compliant sampler
         CASE ( ID_PM25_FRM )
            K = EXP( 118.87 - 24084/SRFTEMP - 6.025*LOG( SRFTEMP ) )
            P1 = EXP( 8763/SRFTEMP + 19.12*LOG( SRFTEMP ) - 135.94 )
            P2 = EXP( 9969/SRFTEMP + 16.22*LOG( SRFTEMP ) - 122.65 )
            P3 = EXP( 13875/SRFTEMP + 24.46*LOG( SRFTEMP ) - 182.61 )
            a = 1 - AIRRH 
            IF ( AIRRH .LE. 0.61 ) THEN
                K2 = K ** 0.5
            ELSE
                K2 = ( K * ( P1 - P2*a + P3*a*a ) * ( a**1.75 ) ) ** 0.5
            END IF

            CALL CALC_ELMO( ID_PM25_NO3, 1, PM25_NO3 )
            PM25_NO3_LOSS = MIN( 745.7/SRFTEMP * K2, PM25_NO3 )

            PM25_NH4_LOSS = PM25_NO3_LOSS * 18.0 / 62.0

            CALL CALC_ELMO( ID_PM25_NH4, 1, PM25_NH4 )
            CALL CALC_ELMO( ID_PM25_SO4, 1, PM25_SO4 ) 
            CALL SUM_INLET_PM( IPM25, IWET, PM25 )

            OUTVAL = PM25 - PM25_NO3_LOSS - PM25_NH4_LOSS
     &                      +0.24 * ( PM25_SO4 + PM25_NH4 - PM25_NH4_LOSS ) + 0.5
          
         ! Retrieve GAMMA_N2O5IJ - fine N2O5 heterogeneous rxn probability
         CASE ( ID_GN2O5F )
            IF ( AERO_CHEM_SET ) OUTVAL = GAMMA_N2O5IJ( C1,R1,L1 )

         ! Retrieve GAMMA_N2O5K - coarse N2O5 heterogeneous rxn probability
         CASE ( ID_GN2O5C )
            IF ( AERO_CHEM_SET ) OUTVAL = GAMMA_N2O5K( C1,R1,L1 )

         ! Retrieve YCLNO2IJ - fine CLNO2 heterogeneous rxn probability
         CASE ( ID_YCLNO2F )
            IF ( AERO_CHEM_SET ) OUTVAL = YCLNO2IJ( C1,R1,L1 )

         ! Retrieve YCLNO2K - coarse CLNO2 heterogeneous rxn probability
         CASE ( ID_YCLNO2C )
            IF ( AERO_CHEM_SET ) OUTVAL = YCLNO2K( C1,R1,L1 )

         ! Retrieve GAMMA_IEPOX - heterogeneous uptake coefficient
         CASE ( ID_GIEPOX )
            IF ( AERO_CHEM_SET ) OUTVAL = GAMMA_IEPOX( C1,R1,L1 )

         ! Retrieve Particle-Phase Reaction Rate Constant for IEPOX
         CASE ( ID_KIEPOX )
            IF ( AERO_CHEM_SET ) OUTVAL = KPARTIEPOX( C1,R1,L1 )

         ! Retrieve IMAE Heterogeneous Uptake Coefficient
         CASE ( ID_GIMAE )
            IF ( AERO_CHEM_SET ) OUTVAL = GAMMA_IMAE( C1,R1,L1 )

         ! Retrieve AOD at 550 nm
         CASE ( ID_AOD550 )
             IF ( L1 .EQ. 1 ) THEN
                 OUTVAL = ELMO_AOD_550( C1,R1 )
             ELSE
                 OUTVAL = AMISS3
             END IF

         ! Retrieve Aerosol Extinction at 550 nm
         CASE ( ID_PMEXT550 )
            OUTVAL = ELMO_EXT_550( C1,R1,L1 )

         ! Retrieve Relative Humidity
         CASE ( ID_RH )
            OUTVAL = AIRRH

         ! Retrieve Air Temperature
         CASE ( ID_TEMP )
            OUTVAL = AIRTEMP
         
         ! Retrieve 2-meter Temperature
         CASE ( ID_SRFTEMP )
            OUTVAL = SRFTEMP

         ! Retrieve Air Pressure
         CASE ( ID_PRES )
            OUTVAL = AIRPRES

         ! Retrieve DZ
         CASE ( ID_DZ )
            OUTVAL = DZ

         ! Retrieve ZH
         CASE ( ID_ZH )
            OUTVAL = ZH

         ! Retrieve Cloud Fraction
         CASE ( ID_CFRAC )
            OUTVAL = CFRAC

         ! Retrieve Potential Vorticity
         CASE ( ID_PV )
            OUTVAL = PV

         ! Retrieve Air Density x Jacobian/MSFX2
         CASE ( ID_RHOJ )
            OUTVAL = CONC_VEC( RHOJ_LOC ) ! kg m-3

         ! Retrieve Air Density
         CASE ( ID_DENS )
            OUTVAL = MET_DATA%DENS( C1,R1,L1 ) ! kg m-3

         CASE DEFAULT
            ! For most variables the ELMO_TABLE structure will
            ! define all of the mappings and coefficients needed for
            ! a particular summation. This can therefore be used in
            ! most cases. Variables in this category are linear
            ! combinations of CMAQ species. For example, PMF_SO4 = 
            ! ASO4I + ASO4J.

         END SELECT 
 
         ! Save the fact that this variable has been calculated
         IF ( ILOC .GT. 0 ) THEN
           L_CALCULATED( ILOC ) = .TRUE.
           ELMO_LOCAL( ILOC ) = OUTVAL
         END IF

      END SUBROUTINE CALC_ELMO_ELAB
 
      END MODULE ELMO_PROC
