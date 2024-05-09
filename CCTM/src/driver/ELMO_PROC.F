
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
         ! Load All Raw Concentrations and Fluxes
         CONC_VEC(:) = CGRID(C,R,L,:)

         IF ( L.EQ.1 .AND. L_ELMO_WDEP )
     &        WDEP_VEC(:) = TOT_DEP(C,R,:)
         IF ( L.EQ.1 .AND. L_ELMO_DDEP )
     &        DDEP_VEC(:) = DDEP(:,C,R)  ! N_SPC_DDEP --> DD2DV

#ifdef isam
         ! Load All Raw Concentrations and Fluxes
         ISAM_VEC(:) = CGRID(C,R,L,:)

         IF ( L.EQ.1 .AND. L_ELMO_WDEP )
     &        ISAM_WDEP_VEC(:,:) = TOT_SADEP(C,R,:,:) ! N_ISAM_SPEC, NTAG_SA
         IF ( L.EQ.1 .AND. L_ELMO_DDEP )
     &        ISAM_DDEP_VEC(:) = SA_DDEP(C,R,:)   ! N_SPCTAG
#endif

#ifdef sens
         ! Load All Raw Concentrations and Fluxes
         DDM_VEC(:) = CGRID(C,R,L,:)

         IF ( L.EQ.1 .AND. L_ELMO_WDEP )
     &        DDM_WDEP_VEC(:,:) = S_TOTDEP(C,R,:,:) !NPMAX, N_SPC_WDEP
         IF ( L.EQ.1 .AND. L_ELMO_DDEP )
     &        DDM_DDEP_VEC(:) = S_DDEP(:,C,R,:)  ! N_SPC_DDEP --> DD2DV, NPMAX
#endif

         ! Populate Diagnostic Arrays
         CALL LOAD_ELMO( C,R,L, WRITE_STEP, INIT_STEP, INIT_TIME )

      END DO
      END DO
      END DO

      END SUBROUTINE ELMO_DRIVER

!-------------------------------------------------------------------------
      SUBROUTINE LOAD_ELMO( C,R,L,WRITE_STEP, INIT_STEP, INIT_TIME )
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
           CALL CALC_ELMO( OUTVAL=ELMO_LOCAL( ILOC ), J=ELMO_LOCALtoTABLE( ILOC ) )
        END DO

        ! *** Aggregate Diagnostic Variables in Average Array
        !     using trapezoidal rule
        IF ( AVRG_ACTIVE .AND. 
     &       L.GE.AVRG_LAYER_BOT .AND. L.LE.AVRG_LAYER_TOP ) THEN
           AL = L - AVRG_LAYER_BOT + 1
           DO  IAVRG = 1,N_ELMO_AVRG
              IF ( ELMO_LOCAL_ID( ELMO_AVRGtoLOCAL( IAVRG )) .EQ. ID_PMF_PH
     &             .AND. ELMO_LOCAL( ELMO_AVRGtoLOCAL( IAVRG ) ) .LT. -100. ) THEN
                 ! Special Averaging for Particle pH
                 ! Add whatever the current average is right now.
                 ! Certainly this is problematic, particularly
                 ! at the beginning of an hour where ELMO_AVRG =
                 ! 0. It's best to either use the instantaneous
                 ! value or recalculate the average fine particle acidity
                 ! offline.
                 IF ( INIT_STEP ) THEN
                    ELMO_AVRG( C,R,AL,IAVRG ) = 3.5  ! Assume relatively acidic particles
                 ELSE IF ( .NOT. WRITE_STEP ) THEN
                    ELMO_AVRG( C,R,AL,IAVRG ) = ELMO_AVRG( C,R,AL,IAVRG ) 
     &                                  + 2.0 * ELMO_AVRG( C,R,AL,IAVRG )
                 END IF
              ELSE
                 ! For all other variables, apply trapezoidal rule for averaging.
                 IF ( INIT_STEP ) THEN
                    ELMO_AVRG( C,R,AL,IAVRG ) = ELMO_LOCAL( ELMO_AVRGtoLOCAL( IAVRG ) ) 
                 ELSE IF ( WRITE_STEP ) THEN
                    ELMO_AVRG( C,R,AL,IAVRG ) = ELMO_AVRG( C,R,AL,IAVRG ) 
     &                                        + ELMO_LOCAL( ELMO_AVRGtoLOCAL( IAVRG ) )
                 ELSE
                    ELMO_AVRG( C,R,AL,IAVRG ) = ELMO_AVRG( C,R,AL,IAVRG ) 
     &                      + 2.0 * ELMO_LOCAL( ELMO_AVRGtoLOCAL( IAVRG ) )
                 END IF 
              END IF
           END DO
        END IF
       
        ! *** Populate Diagnostic Variables to Instantaneous Array if this is a write step
        IF ( INST_ACTIVE .AND. ( INIT_TIME .OR. WRITE_STEP ) .AND.
     &       L.GE.INST_LAYER_BOT .AND. L.LE.INST_LAYER_TOP ) THEN
           IL = L - INST_LAYER_BOT + 1
           DO IINST = 1,N_ELMO_INST_OUT
              ELMO_INST( C,R,IL,IINST ) = ELMO_LOCAL( ELMO_INSTtoLOCAL( IINST ) )
           END DO
        END IF
 
      END IF
      END SUBROUTINE LOAD_ELMO
 
!-------------------------------------------------------------------------
      RECURSIVE SUBROUTINE CALC_ELMO( OUTVAL, OUT_UNIT, J, IDG, IM, IV, ISRC )
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

      INTEGER, INTENT( IN ), OPTIONAL :: J, IDG, IM, IV, ISRC ! Table Index
      REAL, INTENT( INOUT ) :: OUTVAL   ! Output Value
      INTEGER, INTENT( IN ) :: OUT_UNIT ! The desired output unit
      INTEGER ID, VAR_TYPE
      REAL VAL2
      INTEGER IVAR, ILIST
      LOGICAL L_CALC( N_ELMO )
      INTEGER IM, WETNESS, ISPEC, INLET
      LOGICAL LMODE
      REAL    FRAC( N_MODE )

      CHARACTER( 500 ) :: XMSG
  
      ! If the Table Index (J) is not provided, then look it up using
      ! other identifiers like the variable ID, variable type, source
      ! number, and/or mode number
      IF ( .NOT.PRESENT( J ) ) THEN
        ! Set Defaults for Missing Input Parameters
        IF ( .NOT.PRESENT(IDG) )  IDG = 0
        IF ( .NOT.PRESENT(IM) )   IM  = 0
        IF ( .NOT.PRESENT(IV) )   IV  = 0
        IF ( .NOT.PRESENT(ISRC) ) ISRC= 0
        J = FIND_ELMO_TABLE( IDG, IM, IV, ISRC )
      END IF

      ! Determine if this variable has been mapped already. If it
      ! has, send it back
      ILOC = ELMO_TABLEtoLOCAL( J )
      IF ( ILOC .GT. 0 ) THEN
        IF ( L_CALCULATED( ILOC ) ) THEN
           OUTVAL = ELMO_LOCAL( ILOC )
           RETURN
        END IF
      END IF
 
      ! For most variables, the ELMO_TABLE and ELMO_COMP structures 
      ! will define all of the mappings and coefficients needed for
      ! a particular summation. This can therefore be used in most 
      ! cases. Variables in this category are linear combinations 
      ! of CMAQ species. For example, PMF_SO4 = ASO4I + ASO4J.
 
      ! Initialize value of aggregate variable
      OUTVAL = 0.0
      ID  = ELMO_TABLE( J )%ID             ! Map to ELMO Variable ID numbers
      VAR_TYPE  = ELMO_TABLE( J )%VAR_TYPE ! Specify Variable Type (e.g. CGRID, FAM, 
                                           !   ISAM)
      
      ! Retrieve size cutoff if necessary
      INLET = ELMO_TABLE( J )%INLET
      WETNESS = ELMO_TABLE( J )%WETNESS 
      FRAC( : ) = 1.0
      IF ( INLET .NE. 0 ) THEN
        DO IMODE = 1,N_MODE
           CALL GET_AERO_INLET( INLET, IMODE, WETNESS, FRAC( IMODE ) )
        END DO
      END IF
 
      ! Calculate conversion factor for transforming between units. For
      ! example, ppbC or kgN ha-1 can be calculated using this routine.
      ! The inputs are the current variable being calculated and the
      ! desired output units. The output units are most often equivalent
      ! to those of the variable being calculated.
      FAC = 1.0
      CALL COMP_UNIT_CONVERSION( J, OUT_UNIT, FAC )

      ! Retrieve value from corresponding array. These can come from
      ! CGRID, the Detailed ELMO variables, linear combinations of CGRID
      ! variables, deposition arrays, ISAM arrays, or DDM3D arrays.
      SELECT CASE ( VAR_TYPE ) 
         CASE ( ETYPE_CGRID )
            ! Variable is in CGRID
            OUTVAL = CONC_VEC( ID ) * FAC
         
         CASE ( ETYPE_AEROBULK )
            ! Variable is a Bulk Aerosol
            DO IMODE = 1,N_MODE
              ! If this mode is supposed to be added (LMODE) and the
              ! species is not missing, then add it up
              LMODE = ELMO_COMP( J )%L_MODE( IMODE )
              IF ( LMODE .AND. ( .NOT. AERO_MISSING( ID,IMODE ) ) ) 
     &            OUTVAL = OUTVAL + AEROSPC_CONC( ID, IMODE ) * 
     &                        FRAC( IMODE )* FAC
            END DO
                
         CASE ( ETYPE_FAM )
            ! Variable is a Chemical Family
            DO IVAR = 1,ELMO_COMP( J )%N_VARS
              CALL CALC_ELMO( OUTVAL=VAL2, OUT_UNIT=OUT_UNIT, 
     &                        J=ELMO_COMP( J )%TABID(IVAR) )
              OUTVAL = OUTVAL + VAL2
            END DO 

         CASE ( ETYPE_ELMO )   
            ! Variable is an ELMO Variable
            IF ( ELMO_TABLE( J )%L_COMP ) THEN
               ! Sum up Linear Combination Variable
               DO IVAR = 1,ELMO_COMP( J )%N_VARS
                 CALL CALC_ELMO( OUTVAL=VAL2, OUT_UNIT=OUT_UNIT, 
     &                           J=ELMO_COMP( J )%TABID(IVAR) )
                 OUTVAL = OUTVAL + VAL2 * ELMO_COMP( J )%COEFF(IVAR)
               END DO 
            ELSE
               ! Retrieve Detailed ELMO Variable
               CALL ELMO_CALC_DETAILED( VAL2, J )
               OUTVAL = OUTVAL + VAL2 * FAC
            END IF

         CASE ( ETYPE_WDEP )
            ! Variable is a Wet Deposition Flux
            OUTVAL = WDEP_VEC( ID ) * FAC
         
         CASE ( ETYPE_DDEP )
            ! Variable is a Dry Deposition Flux
            OUTVAL = DDEP_VEC( ID ) * FAC
 
         CASE ( ETYPE_ISAM )
            ! Variable is in the ISAM Array
            OUTVAL = ISAM_VEC( ID ) * FAC
         
         CASE ( ETYPE_ISAM_WDEP )
            ! Variable is in the ISAM Array
            OUTVAL = ISAM_WDEP_VEC( ID ) * FAC
         
         CASE ( ETYPE_ISAM_DDEP )
            ! Variable is in the ISAM Array
            OUTVAL = ISAM_DDEP_VEC( ID ) * FAC

         CASE ( ETYPE_DDM )
            ! Variable is in the DDM3D Array
            OUTVAL = DDM_VEC( ID ) * FAC

         CASE ( ETYPE_DDM_WDEP )
            ! Variable is in the DDM3D Array
            OUTVAL = DDM_WDEP_VEC( ID ) * FAC

         CASE ( ETYPE_DDM_DDEP )
            ! Variable is in the DDM3D Array
            OUTVAL = DDM_DDEP_VEC( ID ) * FAC

         CASE DEFAULT

            ! Invalid Variable Type
            XMSG = 'ELMO variable ' // TRIM( ELMO_TABLE( J )%NAME ) //
     &             ' has an invalid variable type. Please check that it is '//
     &             'mapped correctly.'
            CALL LOG_MESSAGE( LOGDEV, XMSG )
            CALL M3EXIT( PNAME, 0, 0, '', XSTAT1 )
         END SELECT
         
      END DO 
 
      END SUBROUTINE CALC_ELMO
 
      END MODULE ELMO_PROC
