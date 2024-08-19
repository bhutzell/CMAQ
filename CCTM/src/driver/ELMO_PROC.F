
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
      USE RUNTIME_VARS

      IMPLICIT NONE

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
     &                        SRFTEMP, H2OVP, H2OSATVP, 
     &                        CFRAC, DZ, PV, ZH
      USE CGRID_SPCS, ONLY : N_CGRID_SPC, N_SPC_WDEP, N_SPC_DEPV, NSPCSD
      use centralized_io_module, only : interpolate_var, pv_avail
      Use phot_mod, Only: init_phot_shared

#ifdef isam
      USE SA_DEFN, ONLY : NTAG_SA, ISAM, NSPC_SA
#endif

#ifdef sens
      USE DDM3D_DEFN, ONLY : SENGRID, NPMAX
#endif 
      USE CONST, ONLY : ESATL, MWWAT, MWAIR


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
      REAL TT

      INTEGER C, R, L, IOS
      CHARACTER(16) :: PNAME = 'ELMO_DRIVER'

      REAL, ALLOCATABLE, SAVE :: PRES(:,:,:), TA(:,:,:), TEMP2(:,:),
     &                           QV(:,:,:), DENS(:,:,:), CFRACS(:,:),
     &                           ZF(:,:,:), PVS(:,:,:), ZHS(:,:,:)

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
         
         ! Allocate local image of CGRID so it can be used each time step
         ALLOCATE( CONC_VEC( N_CGRID_SPC ), 
     &             WDEP_VEC( N_SPC_WDEP+1 ),
     &             DDEP_VEC( N_SPC_DEPV ),
     &             STAT=IOS )
         CALL CHECKMEM( IOS, 'ELMOS_VECTORS',PNAME )

#ifdef isam
         ALLOCATE( ISAM_VEC( NSPC_SA,NTAG_SA ),
     &             ISAM_WDEP_VEC( N_SPC_WDEP,NTAG_SA), 
     &             ISAM_DDEP_VEC( N_SPC_DEPV,NTAG_SA ),
     &             STAT=IOS )
         CALL CHECKMEM( IOS, 'ISAM_ELMO_VECTORS',PNAME )
#endif

#ifdef sens
         ALLOCATE( DDM_VEC( NPMAX,NSPCSD ),
     &             DDM_WDEP_VEC( N_SPC_WDEP+1,NPMAX ), 
     &             DDM_DDEP_VEC( N_SPC_DEPV,NPMAX ),
     &             STAT=IOS )
         CALL CHECKMEM( IOS, 'ISAM_ELMO_VECTORS',PNAME )
#endif

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
     &        WDEP_VEC(:) = ELMO_WDEP(C,R,:)
         IF ( L.EQ.1 .AND. L_ELMO_DDEP )
     &        DDEP_VEC(:) = ELMO_DDEP(C,R,:)  ! N_SPC_DDEP --> DD2DV

#ifdef isam                        
         ! Load All Raw Concentrations and Fluxes
         ISAM_VEC(:,:) = ISAM(C,R,L,:,:)

         IF ( L.EQ.1 .AND. L_ELMO_WDEP )
     &        ISAM_WDEP_VEC(:,:) = ELMO_ISAM_WDEP(C,R,:,:) ! N_SPEC_WDEP+1, NTAG_SA
         IF ( L.EQ.1 .AND. L_ELMO_DDEP )
     &        ISAM_DDEP_VEC(:,:) = ELMO_ISAM_DDEP(C,R,:,:) ! N_SPEC_DEPV, NTAG_SA
#endif

#ifdef sens
         ! Load All Raw Concentrations and Fluxes
         DDM_VEC(:,:) = SENGRID(C,R,L,:,:)  ! NPMAX, NSPCSD

         IF ( L.EQ.1 .AND. L_ELMO_WDEP )
     &        DDM_WDEP_VEC(:,:) = ELMO_DDM_WDEP(C,R,:,:) ! N_SPC_WDEP+1, NPMAX
         IF ( L.EQ.1 .AND. L_ELMO_DDEP )
     &        DDM_DDEP_VEC(:,:) = ELMO_DDM_DDEP(C,R,:,:) ! N_SPC_DEPV, NPMAX
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
      INTEGER IDG, IM, OL, IUSED, ILOC, J, IV, IVAR, IERR, IFL, U

      LOGICAL, INTENT( IN ) :: WRITE_STEP
      LOGICAL, INTENT( IN ) :: INIT_STEP
      LOGICAL, INTENT( IN ) :: INIT_TIME

      CHARACTER(250) :: XMSG
      INTEGER        :: XSTAT1

      ! Initalize Local Parameters
      C1                = C
      R1                = R
      L1                = L
      ELMO_USED( : )   = 0.0
      INLET_FRAC(:,:,:) = -1.
      L_CALCULATED      = .FALSE.
      ELMO_USED_UNIT    = 0

      ! Check to make sure the current layer is requested for diagnostic
      ! output.
      IF ( L .GE. LAY_BOT_MIN .AND. L .LE. LAY_TOP_MAX ) THEN
       
        ! Calculate All Aerosol Size Distribution Parameters
        CALL CALC_AERODIST_PARAMS( INIT_TIME )
      
        ! Loop Through Used Variables List and Call ELMO Functions 
        ! to fill in Local Array. ELMO_LOCAL is the same length as the
        ! ELMO_LIST and is ordered by the ELMO ID numbers so that it
        ! can be used seamlessly with the CALC_ELMO internal order.
        DO ILOC = 1,N_ELMO_USED
           J = MAP_USEDtoTABLE( ILOC ) 
           CALL CALC_ELMO( OUTVAL=ELMO_USED( ILOC ), J=J,
     &                     OUT_UNIT=ELMO_TABLE( J )%IUNIT,IERR=IERR )
           IF ( IERR .EQ. 1 ) THEN
              ! Invalid Variable Type
              WRITE ( XMSG, '(A,I3,A)'),'A user-requested variable for ',
     &                'ELMO cannot be found. Please check that it is ',
     &                'defined correctly. It is number ',J,' on the used ',
     &                'variable list.'
              CALL LOG_MESSAGE( LOGDEV, XMSG )
              CALL M3EXIT( 'LOAD_ELMO', 0, 0, '', XSTAT1 ) 
           END IF
        END DO

      ELSE
        ! This grid layer is not requested for output
        RETURN
      END IF

      DO IFL = 1,N_ELMO_FILES
        IF ( L .GE. ELMO_FILE( IFL )%LAY_BOT .AND.
     &       L .LE. ELMO_FILE( IFL )%LAY_TOP ) THEN

          SELECT CASE ( ELMO_FILE( IFL )%TMODE )
          CASE( 0 ) 
             ! *** Populate Diagnostic Variables to Instantaneous 
             !     Array if this is a write step
             IF ( INIT_TIME .OR. WRITE_STEP ) THEN
               OL = L - ELMO_FILE( IFL )%LAY_BOT + 1
               DO IVAR = 1,ELMO_FILE( IFL )%N_VARS
                   ELMO_CONC( IFL )%DATA( C,R,OL,IVAR ) = ELMO_USED( ELMO_FILE(IFL)%USEDID( IVAR ) )
               END DO
             END IF
 
          CASE ( 1 )
             ! *** Aggregate Diagnostic Variables in Average Array
             !     using trapezoidal rule
             OL = L - ELMO_FILE( IFL )%LAY_BOT + 1
             DO  IVAR = 1,ELMO_FILE( IFL )%N_VARS
                J   = ELMO_FILE( IFL )%TABID( IVAR )
                U   = ELMO_FILE( IFL )%USEDID( IVAR )
                IV  = ELMO_TABLE( J )%VAR_TYPE
                IDG = ELMO_TABLE( J )%ID
              
                IF ( (IV .EQ. ET_DRVD .OR. IV .EQ. ET_ISAM_DRVD 
     &                                .OR. IV .EQ. ET_DDM_DRVD ) .AND.
     &               IDG .EQ. ID_PMF_PH .AND.
     &               ELMO_USED( U ) .LT. -100. ) THEN
                   ! Special Averaging for Particle pH
                   ! Add whatever the current average is right now.
                   ! Certainly this is problematic, particularly
                   ! at the beginning of an hour where ELMO_AVRG =
                   ! 0. It's best to either use the instantaneous
                   ! value or recalculate the average fine particle acidity
                   ! offline.
                   IF ( INIT_STEP ) THEN
                      ELMO_CONC(IFL)%DATA( C,R,OL,IVAR ) = 3.5  ! Assume relatively acidic particles
                   ELSE IF ( .NOT. WRITE_STEP ) THEN
                       ELMO_CONC(IFL)%DATA( C,R,OL,IVAR ) =
     &                          ELMO_CONC(IFL)%DATA( C,R,OL,IVAR ) 
     &                        + 2.0 * ELMO_CONC(IFL)%DATA( C,R,OL,IVAR )
                   END IF
              
                ELSE IF ( IV .EQ. ET_DDEP .OR.
     &                    IV .EQ. ET_ISAM_DDEP .OR.
     &                    IV .EQ. ET_DDM_DDEP .OR.
     &                    IV .EQ. ET_WDEP .OR. 
     &                    IV .EQ. ET_ISAM_WDEP .OR. 
     &                    IV .EQ. ET_DDM_WDEP .OR.
     &                    IV .EQ. ET_CMPST_DEP .OR.
     &                    IV .EQ. ET_ISAM_CMPST_DEP .OR.
     &                    IV .EQ. ET_DDM_CMPST_DEP ) THEN
                   ! This is a deposition species so we should be
                   ! computing a cumulative sum, not an average by default
                   IF ( INIT_STEP ) THEN
                      ELMO_CONC(IFL)%DATA( C,R,OL,IVAR ) = ELMO_USED( U )
                   ELSE   
                      ELMO_CONC(IFL)%DATA( C,R,OL,IVAR ) = 
     &                     ELMO_CONC(IFL)%DATA( C,R,OL,IVAR ) + ELMO_USED( U )
                   END IF
              
                ELSE
                   ! For all other variables, apply trapezoidal rule for averaging.
                   IF ( INIT_STEP ) THEN
                      ELMO_CONC(IFL)%DATA( C,R,OL,IVAR ) = ELMO_USED( U ) 
                   ELSE IF ( WRITE_STEP ) THEN
                      ELMO_CONC(IFL)%DATA( C,R,OL,IVAR ) = 
     &                     ELMO_CONC(IFL)%DATA( C,R,OL,IVAR ) + ELMO_USED( U )
                   ELSE
                      ELMO_CONC(IFL)%DATA( C,R,OL,IVAR ) = 
     &                     ELMO_CONC(IFL)%DATA( C,R,OL,IVAR ) + 2.0 * ELMO_USED(U)
                   END IF 
                END IF
             END DO
          END SELECT
        END IF
      END DO

      END SUBROUTINE LOAD_ELMO
 
!-------------------------------------------------------------------------
      RECURSIVE SUBROUTINE CALC_ELMO( OUTVAL, OUT_UNIT, J, IERR )
!     This subroutine sums aggregate PM diagnostic parameters using the
!         ELMO_COEFFS structure as a guide for summing aerosol
!         components.          
!-------------------------------------------------------------------------
      USE AERO_DATA, ONLY : WET_AERO_M3, WET_AERO_M2, MOMENT0_CONC,
     &                      AH2O_IDX, AEROSPC, AEROSPC_CONC,
     &                      WET_AERO_DENS, N_MODE, AERO_MISSING,
     &                      AEROMODE, AEROSPC_MAP
      USE PRECURSOR_DATA, ONLY : PRECURSOR_CONC
      USE AEROMET_DATA, ONLY : AIRDENS
#ifdef isam
      USE SA_DEFN, ONLY : MAP_AEROtoSA, NTAG_SA
#endif
      USE CONST, ONLY : PI

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: J        ! Table Index
      INTEGER               :: IERR     ! Error Code
      REAL, INTENT( INOUT ) :: OUTVAL   ! Output Value
      INTEGER, INTENT( IN ) :: OUT_UNIT ! The desired output unit
      INTEGER ID, VAR_TYPE
      REAL VAL2
      INTEGER IVAR, ILIST, ILOC, IX1, IX2
      INTEGER WETNESS, ISPEC, INLET, IMODE, ISC
      INTEGER ISA, DSA
      LOGICAL LMODE
      REAL    FRAC( N_MODE ), FRAC1, FAC

      CHARACTER( 500 ) :: XMSG
  
      ! Stop CALC_ELMO and return an error code if the Table Index
      ! is equal to or less than zero.
      IERR = 0
      IF ( J .LE. 0 ) THEN
         IERR = 1
         RETURN
      END IF

      ! Determine if this variable has been mapped already. If it
      ! has, send it back
      ILOC = MAP_TABLEtoUSED( J )
      IF ( ILOC .GT. 0 ) THEN
        IF ( L_CALCULATED( ILOC ) .AND. 
     &       ELMO_USED_UNIT( ILOC ) .EQ. OUT_UNIT ) THEN
           OUTVAL = ELMO_USED( ILOC )
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
 
      ! Retrieve value from corresponding array. These can come from
      ! CGRID, the Detailed ELMO variables, linear combinations of CGRID
      ! variables, deposition arrays, ISAM arrays, or DDM3D arrays.
      SELECT CASE ( VAR_TYPE ) 
         CASE ( ET_CONC )
            ! Variable is in CGRID - look up value in CONC_VEC
            OUTVAL = ELMO_UNIT_CONV( J, OUT_UNIT, CONC_VEC( ID ) )
         
         CASE ( ET_CMPST, ET_ISAM_CMPST, ET_DDM_CMPST,
     &          ET_CMPST_DEP, ET_ISAM_CMPST_DEP, ET_DDM_CMPST_DEP )      
            ! Sum up Composite Variable
            ! Compute mode-dependent fraction from size cutoff if applicable
            FRAC = 1.0
            INLET = ELMO_COMP( J )%INLET
            WETNESS = ELMO_COMP( J )%WETNESS
            DO IMODE = 1,N_MODE
              IF ( INLET .NE. 0 ) THEN
                 CALL GET_AERO_INLET( INLET, IMODE, WETNESS, FRAC( IMODE ) )
              END IF
            END DO

            DO IVAR = 1,ELMO_COMP( J )%N_VARS
              IX1 = ELMO_COMP( J )%TABID(IVAR)
              IMODE = ELMO_TABLE( IX1 )%IM
              FRAC1 = 1.0
              LMODE = .FALSE.
              IF ( IMODE .NE. 0 ) THEN
                  FRAC1 = FRAC( IMODE )
                  LMODE = ELMO_COMP( J )%L_MODE( IMODE )
              END IF

              IF ( IMODE .EQ. 0 .OR. LMODE ) THEN

                CALL CALC_ELMO( OUTVAL=VAL2, OUT_UNIT=OUT_UNIT, J=IX1, IERR=IERR )
                IF ( IERR .EQ. 1 ) THEN
                   ! Invalid Variable Type
                   WRITE ( XMSG, '(2A,I3,4A)'),'There was an error processing ', 
     &                     'component number ',IX1,' of the composite ',
     &                     TRIM(ELMO_TABLE( J )%NAME),'. Please check ',
     &                     'its definition.'
                   CALL LOG_MESSAGE( LOGDEV, XMSG )
                   CALL M3EXIT( 'CALC_ELMO', 0, 0, '', XSTAT1 ) 
                END IF 

                OUTVAL = OUTVAL + VAL2 * 
     &                   ELMO_COMP( J )%COEFF(IVAR) * FRAC1

              END IF
            END DO

         CASE ( ET_DRVD, ET_ISAM_DRVD, ET_DDM_DRVD,
     &          ET_AEROPROP, ET_CHEM, ET_MET, ET_PHOT )
            ! Retrieve Detailed ELMO Variable
            CALL ELMO_CALC_DERIVED( VAL2, J )
            OUTVAL = OUTVAL + ELMO_UNIT_CONV( J, OUT_UNIT, VAL2 )

         CASE ( ET_WDEP )
            ! Variable is a Wet Deposition Flux
            OUTVAL = ELMO_UNIT_CONV( J, OUT_UNIT, WDEP_VEC( ID ) ) 
         
         CASE ( ET_DDEP )
            ! Variable is a Dry Deposition Flux
            OUTVAL = ELMO_UNIT_CONV( J, OUT_UNIT, DDEP_VEC( ID ) ) 

#ifdef isam
         CASE ( ET_ISAM_CONC )
            ! Variable is in the ISAM Array
            ISC = ELMO_TABLE( J )%SOURCE
            IF ( ISC .LE. NTAG_SA ) THEN
                OUTVAL = ELMO_UNIT_CONV( J, OUT_UNIT, ISAM_VEC( ID,ISC ) ) 
            ELSE
                OUTVAL = ELMO_UNIT_CONV( J, OUT_UNIT, SUM(ISAM_VEC( ID,: )) ) 
            END IF
         
         CASE ( ET_ISAM_WDEP )
            ! Variable is in the ISAM WetDep Array
            ISC = ELMO_TABLE( J )%SOURCE
            IF ( ISC .LE. NTAG_SA ) THEN
              OUTVAL = ELMO_UNIT_CONV( J, OUT_UNIT, ISAM_WDEP_VEC( ID,ISC ) ) 
            ELSE
              OUTVAL = ELMO_UNIT_CONV( J, OUT_UNIT, SUM( ISAM_WDEP_VEC( ID,: )) ) 
            END IF
         
         CASE ( ET_ISAM_DDEP )
            ! Variable is in the ISAM DryDep Array
            ISC = ELMO_TABLE( J )%SOURCE
            IF ( ISC .LE. NTAG_SA ) THEN
              OUTVAL = ELMO_UNIT_CONV( J, OUT_UNIT, ISAM_DDEP_VEC( ID,ISC ) ) 
            ELSE
              OUTVAL = ELMO_UNIT_CONV( J, OUT_UNIT, SUM( ISAM_DDEP_VEC( ID,: )) ) 
            END IF
#endif
#ifdef sens
         CASE ( ET_DDM_CONC )
            ! Variable is in the DDM3D Array
            ISC = ELMO_TABLE( J )%SOURCE
            OUTVAL = ELMO_UNIT_CONV( J, OUT_UNIT, DDM_VEC( ISC,ID ) ) 

         CASE ( ET_DDM_WDEP )
            ! Variable is in the DDM3D WetDep Array
            ISC = ELMO_TABLE( J )%SOURCE
            OUTVAL = ELMO_UNIT_CONV( J, OUT_UNIT, DDM_WDEP_VEC( ISC,ID ) ) 

         CASE ( ET_DDM_DDEP )
            ! Variable is in the DDM3D DryDep Array
            ISC = ELMO_TABLE( J )%SOURCE
            OUTVAL = ELMO_UNIT_CONV( J, OUT_UNIT, DDM_DDEP_VEC( ISC,ID ) ) 
#endif

         CASE DEFAULT

            ! Invalid Variable Type
            XMSG = 'ELMO variable ' // TRIM( ELMO_TABLE( J )%NAME ) //
     &             ' has an invalid variable type. Please check that it is '//
     &             'mapped correctly.'
            CALL LOG_MESSAGE( LOGDEV, XMSG )
            CALL M3EXIT( 'CALC_ELMO', 0, 0, '', XSTAT1 )
      END SELECT
      
      ! Save the fact that this variable has been calculated
      IF ( ILOC .GT. 0 ) THEN
        L_CALCULATED( ILOC ) = .TRUE.
        ELMO_USED_UNIT( ILOC ) = OUT_UNIT
        ELMO_USED( ILOC ) = OUTVAL
      END IF

      END SUBROUTINE CALC_ELMO
 
      END MODULE ELMO_PROC
