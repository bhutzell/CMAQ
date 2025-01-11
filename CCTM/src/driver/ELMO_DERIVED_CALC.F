
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
      
!-------------------------------------------------------------------------
      SUBROUTINE ELMO_DERIVED_CALC( OUTVAL, J )
!     This subroutine maps the PM diagnostic variables that the user has
!       requested to the entries in the ELMO_DATA table.
!-------------------------------------------------------------------------
      USE CONST
      USE ELMO_DATA
      USE ELMO_DERIVED_DEFN
      USE ELMO_PROC, ONLY : CALC_ELMO
      USE AERO_DATA, ONLY: AEROMODE_LNSG, DRY_AERO_DIAM, WET_AERO_DIAM,
     &                     DRY_AERO_M2, WET_AERO_M2, DRY_AERO_M3, 
     &                     WET_AERO_M3, N_MODE, MOMENT0_CONC, MOMENT2_CONC,
     &                     MOMENT3_CONC, WET_AERO_DENS, AEROSPC_CONC,
     &                     AH2O_IDX, AORGH2O_IDX, AEROSPC, CALC_AERODIST_PARAMS,
     &                     AEROMODE, DRY_AERO_DENS, APOC_IDX, APNCOM_IDX,
     &                     AEROSPC_MAP
      USE AEROMET_DATA, ONLY: AIRRH, AIRTEMP, SRFTEMP, AIRPRES
      USE CGRID_SPCS, ONLY : RHOJ_LOC
      USE SOA_DEFN, ONLY : N_OA, N_OA_NotTracers, OASPC, MAP_OAtoAERO
      USE RUNTIME_VARS, ONLY : PHOTDIAG
      USE ASX_DATA_MOD, ONLY : MET_DATA
      USE ELMO_DERIVED_FXNS

#ifdef isam
      USE SA_DEFN, ONLY : MAP_AEROtoSA,NTAG_SA
#endif

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: J 
      REAL, INTENT( INOUT ) :: OUTVAL
      INTEGER ILOC, IMODE, IVAR, IOA, IM, IV, ISRC, IDG, IUNIT, 
     &        IERR, N
      REAL VAL4, VAL3, VAL2, VAL, FRAC( N_MODE )
      REAL PM01, PM1, PM25, PM10, PMAMS, PM25to10, POM, POC, OtoC
      REAL K, P1, P2, P3, a, K2, PMF_NO3, PMF_NO3_LOSS,
     &     PMF_NH4_LOSS, PMF_NH4, PMF_SO4, PMF,
     &     PM25_NH4, PM25_NO3, PM25_SO4, PM25_NO3_LOSS,
     &     PM25_NH4_LOSS
      LOGICAL ORG_MASK( N_OA ), MODE_MASK( N_MODE )
      CHARACTER( 300 ) :: XMSG

      OUTVAL = 0.0

      IDG    = ELMO_TABLE( J )%ID
      IM     = ELMO_TABLE( J )%IM
      IV     = ELMO_TABLE( J )%VAR_TYPE
      ISRC   = ELMO_TABLE( J )%SOURCE
      IUNIT  = ELMO_TABLE( J )%IUNIT

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
 
         ! Calculate AHPMOLAL - Fine Mode Concentration of H+ in
         ! Particle Water
         CASE ( ID_PMF_HPM )
            ! Retrieve AHPLUSIJ (umol m-3)
            CALL CALC_ELMO( OUTVAL=VAL, OUT_UNIT=IU_UMOLpM3, 
     &                      J=J_PMF_HP( ISRC+1 ), IERR=IERR )
            IF ( IERR .EQ. 1 ) THEN
               XMSG = 'PMF_HP has not been defined in the chem control namelist '//
     &                'so PMF_HPM cannot be calculated. Please define PMF_HP.'
               CALL M3EXIT( 'ELMO_DERIVED_CALC',0,0,XMSG,1)
            END IF 

            ! Retrieve AH2OIJ (ug m-3)
            CALL CALC_ELMO( OUTVAL=VAL2, OUT_UNIT=IU_UGpM3, 
     &                      J=J_PMF_H2O( ISRC+1 ), IERR=IERR )
            IF ( IERR .EQ. 1 ) THEN
               XMSG = 'PMF_H2O has not been defined in the chem control namelist'//
     &                'so PMF_HPMOLAL cannot be calculated. Please define PMF_H2O.'
               CALL M3EXIT( 'ELMO_DERIVED_CALC',0,0,XMSG,1)
            END IF
            
            ! HPMOLAL = AHPLUSIJ / AH2OIJ * 1000.0 [mol kg-1]
            OUTVAL = VAL / VAL2 * 1000.0

         ! Calculate PMF_PH - Fine-Mode pH
         CASE ( ID_PMF_PH )
            ! Retrieve HPMOLAL (mol kg-1)
            CALL CALC_ELMO( OUTVAL=VAL, OUT_UNIT=IU_MOLpKG, 
     &                      J=J_PMF_HPMOLAL( ISRC+1 ), IERR=IERR )
            IF ( IERR .EQ. 1 ) THEN
               XMSG = 'PMF_HPMOLAL has not been defined in the chem control namelist'//
     &                'so PMF_PH cannot be calculated. Please define PMF_HPMOLAL.'
               CALL M3EXIT( 'ELMO_DERIVED_CALC',0,0,XMSG,1)
            END IF

            ! Retrieve AH2OIJ (ug m-3)
            CALL CALC_ELMO( OUTVAL=VAL2,OUT_UNIT=IU_UGpM3, 
     &                      J=J_PMF_H2O( ISRC+1 ), IERR=IERR )
            IF ( IERR .EQ. 1 ) THEN
               XMSG = 'PMF_H2O has not been defined in the chem control namelist'//
     &                'so PMF_PH cannot be calculated. Please define PMF_H2O.'
               CALL M3EXIT( 'ELMO_DERIVED_CALC',0,0,XMSG,1)
            END IF

            OUTVAL = ELMO_BLANK

            IF ( VAL2 .GT. 0.01 )
     &           OUTVAL = -1.0 * LOG10( VAL )

         ! Calculate Aerosol Fraction of Benzo-a-Pyrene
         CASE ( ID_BENAPY_FAERO )
            ! Confirm that particle and gas benzo-a-pyrene are 
            ! actually mapped composites before computing partition
            ! fraction
            IF ( J_PMF_BENAPY( ISRC+1 ) .GT. 0 .AND. 
     &           J_GAS_BENAPY( ISRC+1 ) .GT. 0 ) THEN

               CALL CALC_ELMO( OUTVAL=VAL,  OUT_UNIT=IU_UGpM3, 
     &                         J=J_PMF_BENAPY( ISRC+1 ), IERR=IERR )
               CALL CALC_ELMO( OUTVAL=VAL2, OUT_UNIT=IU_UGpM3, 
     &                         J=J_GAS_BENAPY( ISRC+1 ), IERR=IERR )
               OUTVAL = VAL / ( VAL + VAL2 + TINY( 0.0 ) )

            ELSE 
               OUTVAL = 0.0
            END IF

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

            CALL CALC_ELMO( OUTVAL=PMF_NO3, OUT_UNIT=IUNIT, 
     &                      J=J_PMF_NO3( ISRC+1 ), IERR=IERR )
            IF ( IERR .EQ. 1 ) THEN
               XMSG = 'PMF_NO3 has not been defined in the chem control namelist'//
     &                'so PMF_FRM cannot be calculated. Please define PMF_NO3.'
               CALL M3EXIT( 'ELMO_DERIVED_CALC',0,0,XMSG,1)
            END IF
            
            PMF_NO3_LOSS = MIN( 745.7/SRFTEMP * K2, PMF_NO3 )

            PMF_NH4_LOSS = PMF_NO3_LOSS * 18.0 / 62.0

            CALL CALC_ELMO( OUTVAL=PMF_NH4,  OUT_UNIT=IUNIT, 
     &                      J=J_PMF_NH4( ISRC+1 ), IERR=IERR )
            IF ( IERR .EQ. 1 ) THEN
               XMSG = 'PMF_NH4 has not been defined in the chem control namelist'//
     &                'so PMF_FRM cannot be calculated. Please define PMF_NH4.'
               CALL M3EXIT( 'ELMO_DERIVED_CALC',0,0,XMSG,1)
            END IF
            
            CALL CALC_ELMO( OUTVAL=PMF_SO4,  OUT_UNIT=IUNIT, 
     &                      J=J_PMF_SO4( ISRC+1 ), IERR=IERR ) 
            IF ( IERR .EQ. 1 ) THEN
               XMSG = 'PMF_SO4 has not been defined in the chem control namelist'//
     &                'so PMF_FRM cannot be calculated. Please define PMF_SO4.'
               CALL M3EXIT( 'ELMO_DERIVED_CALC',0,0,XMSG,1)
            END IF
            
            PMF = SUM_PM_MODES( IM_F, 0, 0, IV, ISRC ) 

            OUTVAL = PMF - PMF_NO3_LOSS - PMF_NH4_LOSS
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
            
            CALL CALC_ELMO( OUTVAL=PM25_NO3, OUT_UNIT=IUNIT, 
     &                      J=J_PM25_NO3( ISRC+1 ), IERR=IERR )
            IF ( IERR .EQ. 1 ) THEN
               XMSG = 'PM25_NO3 has not been defined in the chem control namelist'//
     &                'so PM25_FRM cannot be calculated. Please define PM25_NO3.'
               CALL M3EXIT( 'ELMO_DERIVED_CALC',0,0,XMSG,1)
            END IF
            
            PM25_NO3_LOSS = MIN( 745.7/SRFTEMP * K2, PM25_NO3 )

            PM25_NH4_LOSS = PM25_NO3_LOSS * 18.0 / 62.0

            CALL CALC_ELMO( OUTVAL=PM25_NH4, OUT_UNIT=IUNIT, 
     &                      J=J_PM25_NH4( ISRC+1 ), IERR=IERR )
            IF ( IERR .EQ. 1 ) THEN
               XMSG = 'PM25_NH4 has not been defined in the chem control namelist'//
     &                'so PM25_FRM cannot be calculated. Please define PM25_NH4.'
               CALL M3EXIT( 'ELMO_DERIVED_CALC',0,0,XMSG,1)
            END IF
            
            CALL CALC_ELMO( OUTVAL=PM25_SO4, OUT_UNIT=IUNIT, 
     &                      J=J_PM25_SO4( ISRC+1 ), IERR=IERR ) 
            IF ( IERR .EQ. 1 ) THEN
               XMSG = 'PM25_SO4 has not been defined in the chem control namelist'//
     &                'so PM25_FRM cannot be calculated. Please define PM25_SO4.'
               CALL M3EXIT( 'ELMO_DERIVED_CALC',0,0,XMSG,1)
            END IF
            
            PM25 = SUM_PM_MODES( IM_ALL, IPM25, IWET, IV, ISRC ) 

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
                 OUTVAL = ELMO_BLANK
             END IF

         ! Retrieve Aerosol Extinction at 550 nm
         CASE ( ID_PMEXT550 )
            OUTVAL = ELMO_EXT_550( C1,R1,L1 )

         ! Retrieve column density variables
         CASE ( ID_NO2COL )
             IF ( L1 .EQ. 1 ) THEN
             OUTVAL = ELMO_NO2COL( C1,R1 )
             ELSE
             OUTVAL = ELMO_BLANK
             END IF

         CASE ( ID_SO2COL )
             IF ( L1 .EQ. 1 ) THEN
             OUTVAL = ELMO_SO2COL( C1,R1 )
             ELSE
             OUTVAL = ELMO_BLANK
             END IF

         CASE ( ID_HCHOCOL )
             IF ( L1 .EQ. 1 ) THEN
             OUTVAL = ELMO_HCHOCOL( C1,R1 )
             ELSE
             OUTVAL = ELMO_BLANK
             END IF

         CASE ( ID_COCOL )
             IF ( L1 .EQ. 1 ) THEN
             OUTVAL = ELMO_COCOL( C1,R1 )
             ELSE
             OUTVAL = ELMO_BLANK
             END IF

         ! Vertical diffusivity coefficients
         CASE ( ID_KZ )
            IF ( INIT_TIME ) THEN
                OUTVAL = 0.0
            ELSE
                OUTVAL = MET_DATA%KZ( C1,R1,L1 )
            END IF
                 

         CASE ( ID_KZMIN )
            IF ( INIT_TIME ) THEN
                OUTVAL = 0.0
            ELSE
                OUTVAL = MET_DATA%KZMIN( C1,R1,L1 )
            END IF

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

         ! Retrieve PBL Height
         CASE ( ID_PBL )
            OUTVAL = MET_DATA%PBL( C1,R1 )

         ! Retrieve Solar Radiation at Ground
         CASE ( ID_RGRND )
            OUTVAL = MET_DATA%RGRND( C1,R1 )

         ! Retrieve Precipitation
         CASE ( ID_PRECIP )
            OUTVAL = MET_DATA%RN( C1,R1 )
            IF ( MET_DATA%RC( C1,R1 ) .GE. 0.0 ) 
     &           OUTVAL = OUTVAL + MET_DATA%RC( C1,R1 )

         ! Retrieve Wind Speed at 10 m
         CASE ( ID_WSPD10 )
            OUTVAL = MET_DATA%WSPD10( C1,R1 )

         ! Retrieve Wind Direction at 10 m
         CASE ( ID_WDIR10 )
            OUTVAL = WDIR10

         ! Retrieve Vertical Wind Velocity
         CASE ( ID_WVEL )
            OUTVAL = MET_DATA%WVEL( C1,R1,L1 )

         ! Retrieve DZ
         CASE ( ID_DZ )
            IF ( L1 .EQ. 1 ) THEN
               OUTVAL = MET_DATA%ZF( C1,R1,L1 )
            ELSE 
               OUTVAL = MET_DATA%ZF( C1,R1,L1 ) - MET_DATA%ZF( C1,R1,L1-1 )
            END IF

         ! Retrieve ZH
         CASE ( ID_ZH )
            OUTVAL = MET_DATA%ZH( C1,R1,L1 )

         ! Retrieve Cloud Fraction
         CASE ( ID_CFRAC )
            OUTVAL = MET_DATA%CFRAC( C1,R1 )

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
            ! This Could be a PM mass or organic aerosol derived
            ! concentration or deposition. Check the DRVD_TAB field
            N = ELMO_TABLE( J )%DRVD_TAB
            IF ( N .GT. 0 ) THEN
               IF ( ELMO_DRVD_TAB( N )%ORG_OP .EQ. 0 ) THEN
                  ! Total PM Mass across MODES for INLET
                  OUTVAL = SUM_PM_MODES( ELMO_DRVD_TAB( N )%MODE, 
     &                                   ELMO_DRVD_TAB( N )%INLET, 
     &                                   IWET, IV, ISRC ) 
               ELSE
                  ! Organic Aerosol Concentration or Deposition Metric
                  OUTVAL = SUM_ORGS( ELMO_DRVD_TAB( N )%ORG_OP,
     &                               ELMO_DRVD_TAB( N )%ORG_MASK,
     &                               ELMO_DRVD_TAB( N )%MODE, 
     &                               ELMO_DRVD_TAB( N )%INLET, 
     &                               IWET, IV, ISRC )
               END IF
            END IF
         END SELECT 
 
      END SUBROUTINE ELMO_DERIVED_CALC
 
