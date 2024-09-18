
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
      SUBROUTINE ELMO_CALC_DERIVED( OUTVAL, J )
!     This subroutine maps the PM diagnostic variables that the user has
!       requested to the entries in the ELMO_DATA table.
!-------------------------------------------------------------------------
      USE CONST
      USE ELMO_DATA
      USE ELMO_DERIVED
      USE ELMO_PROC, ONLY : CALC_ELMO
      USE AERO_DATA, ONLY: AEROMODE_LNSG, DRY_AERO_DIAM, WET_AERO_DIAM,
     &                     DRY_AERO_M2, WET_AERO_M2, DRY_AERO_M3, 
     &                     WET_AERO_M3, N_MODE, MOMENT0_CONC, MOMENT2_CONC,
     &                     MOMENT3_CONC, WET_AERO_DENS, AEROSPC_CONC,
     &                     AH2O_IDX, AORGH2O_IDX, AEROSPC, CALC_AERODIST_PARAMS,
     &                     AEROMODE, DRY_AERO_DENS, APOC_IDX, APNCOM_IDX,
     &                     AEROSPC_MAP
      USE AEROMET_DATA, ONLY: AIRRH, AIRTEMP, SRFTEMP, AIRPRES, DZ, CFRAC, PV, ZH
      USE CGRID_SPCS, ONLY : RHOJ_LOC
      USE SOA_DEFN, ONLY : N_OA, N_OA_NotTracers, OASPC, MAP_OAtoAERO
      USE RUNTIME_VARS, ONLY : PHOTDIAG
      USE ASX_DATA_MOD, ONLY : MET_DATA

#ifdef isam
      USE SA_DEFN, ONLY : MAP_AEROtoSA,NTAG_SA
#endif

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: J
      REAL, INTENT( INOUT ) :: OUTVAL
      INTEGER ILOC, IMODE, IVAR, IOA, IM, IV, ISRC, IDG, IUNIT, IERR
      REAL VAL4, VAL3, VAL2, VAL, FRAC( N_MODE )
      REAL PM01, PM1, PM25, PM10, PMAMS, PM25to10, POM, POC, OtoC
      REAL K, P1, P2, P3, a, K2, PMF_NO3, PMF_NO3_LOSS,
     &     PMF_NH4_LOSS, PMF_NH4, PMF_SO4, PMF,
     &     PM25_NH4, PM25_NO3, PM25_SO4, PM25_NO3_LOSS,
     &     PM25_NH4_LOSS
      REAL SUM_PM_MODES, SUM_ORGS
      LOGICAL ORG_MASK( N_OA )
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

         ! Retrieve Total Particle Mass
         CASE ( ID_PM_MASS )
            OUTVAL = SUM_PM_MODES( AEROMODE%ALL_MASK, 0, 0, IV, ISRC ) 

         ! Retrieve Ultrafine Particle Mass
         CASE ( ID_PMU_MASS )
            OUTVAL = SUM_PM_MODES( AEROMODE%ULTRAFINE_MASK, 0, 0, IV, ISRC ) 

         ! Retrieve Fine Particle Mass
         CASE ( ID_PMF_MASS )
            OUTVAL = SUM_PM_MODES( AEROMODE%FINE_MASK, 0, 0, IV, ISRC ) 

         ! Retrieve Coarse Particle Mass
         CASE ( ID_PMC_MASS )
            OUTVAL = SUM_PM_MODES( AEROMODE%COARSE_MASK, 0, 0, IV, ISRC ) 

         ! Retrieve Nucleation-Mode Particle Mass
         CASE ( ID_PMNUC_MASS )
            OUTVAL = SUM_PM_MODES( AEROMODE%NUC_MASK, 0, 0, IV, ISRC ) 

         ! Retrieve Aitken-Mode Particle Mass
         CASE ( ID_PMAIT_MASS )
            OUTVAL = SUM_PM_MODES( AEROMODE%AITKEN_MASK, 0, 0, IV, ISRC ) 

         ! Retrieve Accumution-Mode Particle Mass
         CASE ( ID_PMACC_MASS )
            OUTVAL = SUM_PM_MODES( AEROMODE%ACCUM_MASK, 0, 0, IV, ISRC ) 

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
            OUTVAL = SUM_PM_MODES( AEROMODE%ALL_MASK, IPM01, IWET, IV, ISRC ) 

         ! Retrieve Bulk PM1.0 Concentration
         CASE ( ID_PM1 )
            OUTVAL = SUM_PM_MODES( AEROMODE%ALL_MASK, IPM1, IWET, IV, ISRC ) 

         ! Retrieve Bulk PM2.5 Concentration
         CASE ( ID_PM25 )
            OUTVAL = SUM_PM_MODES( AEROMODE%ALL_MASK, IPM25, IWET, IV, ISRC ) 

         ! Retrieve Bulk PM10.0 Concentration
         CASE ( ID_PM10 )
            OUTVAL = SUM_PM_MODES( AEROMODE%ALL_MASK, IPM10, IWET, IV, ISRC ) 
         
         ! Retrieve Bulk PM2.5-10.0 Concentration
         CASE ( ID_PM25to10 )
            OUTVAL = SUM_PM_MODES( AEROMODE%ALL_MASK, IPM25to10, IWET, IV, ISRC ) 

         ! Retrieve Bulk PM Concentration Relevant for the AMS
         CASE ( ID_PMAMS )
            OUTVAL = SUM_PM_MODES( AEROMODE%ALL_MASK, IPMAMS, IWET, IV, ISRC ) 
         
         ! Calculate AHPMOLAL - Fine Mode Concentration of H+ in
         ! Particle Water
         CASE ( ID_PMF_HPM )
            ! Retrieve AHPLUSIJ (umol m-3)
            CALL CALC_ELMO( OUTVAL=VAL, OUT_UNIT=IU_UMOLpM3, 
     &                      J=J_PMF_HP( ISRC+1 ), IERR=IERR )
            IF ( IERR .EQ. 1 ) THEN
               XMSG = 'PMF_HP has not been defined in the chem control namelist '//
     &                'so PMF_HPM cannot be calculated. Please define PMF_HP.'
               CALL M3EXIT( 'ELMO_CALC_DERIVED',0,0,XMSG,1)
            END IF 

            ! Retrieve AH2OIJ (ug m-3)
            CALL CALC_ELMO( OUTVAL=VAL2, OUT_UNIT=IU_UGpM3, 
     &                      J=J_PMF_H2O( ISRC+1 ), IERR=IERR )
            IF ( IERR .EQ. 1 ) THEN
               XMSG = 'PMF_H2O has not been defined in the chem control namelist'//
     &                'so PMF_HPMOLAL cannot be calculated. Please define PMF_H2O.'
               CALL M3EXIT( 'ELMO_CALC_DERIVED',0,0,XMSG,1)
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
               CALL M3EXIT( 'ELMO_CALC_DERIVED',0,0,XMSG,1)
            END IF

            ! Retrieve AH2OIJ (ug m-3)
            CALL CALC_ELMO( OUTVAL=VAL2,OUT_UNIT=IU_UGpM3, 
     &                      J=J_PMF_H2O( ISRC+1 ), IERR=IERR )
            IF ( IERR .EQ. 1 ) THEN
               XMSG = 'PMF_H2O has not been defined in the chem control namelist'//
     &                'so PMF_PH cannot be calculated. Please define PMF_H2O.'
               CALL M3EXIT( 'ELMO_CALC_DERIVED',0,0,XMSG,1)
            END IF

            OUTVAL = ELMO_BLANK

            IF ( VAL2 .GT. 0.01 )
     &           OUTVAL = -1.0 * LOG10( VAL )

         ! Calculate PMF_OA - Fine-Mode Organic Aerosol Mass
         CASE ( ID_PMF_OA )
            ORG_MASK = .TRUE.
            OUTVAL = SUM_ORGS( OT_OA, ORG_MASK, 
     &                         AEROMODE(:)%FINE_MASK, 0, 0, IV, ISRC )

         ! Calculate PMF_POA - Fine-Mode Primary Organic Aerosol Mass
         CASE ( ID_PMF_POA )
            ORG_MASK = OASPC(:)%PRIMARY
            OUTVAL = SUM_ORGS( OT_OA, ORG_MASK, 
     &                         AEROMODE(:)%FINE_MASK, 0, 0, IV, ISRC )

         ! Calculate PMF_SOA - Fine-Mode Secondary Organic Aerosol Mass
         CASE ( ID_PMF_SOA )
            ORG_MASK = .NOT. OASPC(:)%PRIMARY
            OUTVAL = SUM_ORGS( OT_OA, ORG_MASK, 
     &                         AEROMODE(:)%FINE_MASK, 0, 0, IV, ISRC )

         ! Calculate Fine-Mode Organic Carbon Mass
         CASE ( ID_PMF_OC )
            ORG_MASK = OASPC( : )%OMtoOC .GT. 0.0
            OUTVAL = SUM_ORGS( OT_OC, ORG_MASK, 
     &                         AEROMODE(:)%FINE_MASK, 0, 0, IV, ISRC )

         ! Calculate Fine-Particle Primary Organic Carbon Mass
         CASE ( ID_PMF_POC )
            ORG_MASK = ( OASPC(:)%OMtoOC .GT. 0.0 ) .AND.
     &                 ( OASPC(:)%PRIMARY )
            OUTVAL = SUM_ORGS( OT_OC, ORG_MASK, 
     &                         AEROMODE(:)%FINE_MASK, 0, 0, IV, ISRC )

         ! Calculate Fine-Particle Secondary Organic Carbon Mass
         CASE ( ID_PMF_SOC )
            ORG_MASK = ( OASPC(:)%OMtoOC .GT. 0.0 ) .AND.
     &                 ( .NOT. OASPC(:)%PRIMARY )
            OUTVAL = SUM_ORGS( OT_OC, ORG_MASK, 
     &                         AEROMODE(:)%FINE_MASK, 0, 0, IV, ISRC )

         ! Calculate PMF_ASOA - Fine-Mode Anthropogenic OA Mass
         CASE ( ID_PMF_ASOA )
            ORG_MASK = ( .NOT. OASPC(:)%PRIMARY ) .AND.
     &                 ( OASPC(:)%ANTHRO )
            OUTVAL = SUM_ORGS( OT_OA, ORG_MASK, 
     &                         AEROMODE(:)%FINE_MASK, 0, 0, IV, ISRC )

         ! Calculate PMF_BSOA - Fine-Mode Biogenic SOA Mass
         CASE ( ID_PMF_BSOA )
            ORG_MASK = ( .NOT. OASPC(:)%PRIMARY ) .AND.
     &                 ( OASPC(:)%BIOG )
            OUTVAL = SUM_ORGS( OT_OA, ORG_MASK, 
     &                         AEROMODE(:)%FINE_MASK, 0, 0, IV, ISRC )

         ! Calculate PMF_OMOC - Fine-Mode OM:OC 
         CASE ( ID_PMF_OMOC )
            ORG_MASK = ( OASPC(:)%OMtoOC .GT. 0.0 ) 
            OUTVAL = SUM_ORGS( OT_OMOC, ORG_MASK, 
     &                         AEROMODE(:)%FINE_MASK, 0, 0, IV, ISRC )

         ! Calculate PMF_OtoC - Fine-Mode O:C 
         CASE ( ID_PMF_OtoC )
            ORG_MASK = .TRUE.
            OUTVAL = SUM_ORGS( OT_OTOC, ORG_MASK, 
     &                         AEROMODE(:)%FINE_MASK, 0, 0, IV, ISRC )

         ! Calculate PM1_OM - PM1.0 Organic Aerosol Mass
         CASE ( ID_PM1_OA )
            ORG_MASK = .TRUE.
            OUTVAL = SUM_ORGS( OT_OA, ORG_MASK, 
     &                         AEROMODE(:)%ALL_MASK, IPM1, IWET, IV, ISRC )

         ! Calculate PM1_OC - PM1.0 Organic Carbon Mass
         CASE ( ID_PM1_OC )
            ORG_MASK = OASPC( : )%OMtoOC .GT. 0.0
            OUTVAL = SUM_ORGS( OT_OC, ORG_MASK, 
     &                         AEROMODE(:)%ALL_MASK, IPM1, IWET, IV, ISRC )

         ! Calculate PM25_OM - PM2.5 Organic Aerosol Mass
         CASE ( ID_PM25_OA )
            ORG_MASK = .TRUE.
            OUTVAL = SUM_ORGS( OT_OA, ORG_MASK, 
     &                         AEROMODE(:)%ALL_MASK, IPM25, IWET, IV, ISRC )

         ! Calculate PM25_OC - PM2.5 Organic Carbon Mass
         CASE ( ID_PM25_OC )
            ORG_MASK = OASPC( : )%OMtoOC .GT. 0.0
            OUTVAL = SUM_ORGS( OT_OC, ORG_MASK, 
     &                         AEROMODE(:)%ALL_MASK, IPM25, IWET, IV, ISRC )

         ! Retrieve OA Concentration Relevant for the AMS
         CASE ( ID_AMS_OA )
            ORG_MASK = .TRUE.
            OUTVAL = SUM_ORGS( OT_OA, ORG_MASK, 
     &                         AEROMODE(:)%ALL_MASK, IPMAMS, IWET, IV, ISRC )

         ! Calculate O:C relevant for AMS observations
         CASE ( ID_AMS_OtoC )
            ORG_MASK = .TRUE.
            OUTVAL = SUM_ORGS( OT_OTOC, ORG_MASK, 
     &                         AEROMODE(:)%FINE_MASK, IPMAMS, IWET, IV, ISRC )

         ! Calculate Dry Deposition of Total OA
         CASE ( ID_DD_OA )
            ORG_MASK = .TRUE.
            OUTVAL = SUM_ORGS( OT_DD_OA, ORG_MASK, 
     &                         AEROMODE(:)%ALL_MASK, 0, 0, IV, ISRC )

         ! Calculate Wet Deposition of Total OA
         CASE ( ID_WD_OA )
            ORG_MASK = .TRUE.
            OUTVAL = SUM_ORGS( OT_WD_OA, ORG_MASK, 
     &                         AEROMODE(:)%ALL_MASK, 0, 0, IV, ISRC )

         ! Calculate Dry Deposition of Total OC
         CASE ( ID_DD_OC )
            ORG_MASK = .TRUE.
            OUTVAL = SUM_ORGS( OT_DD_OC, ORG_MASK, 
     &                         AEROMODE(:)%ALL_MASK, 0, 0, IV, ISRC )

         ! Calculate Wet Deposition of Total OC
         CASE ( ID_WD_OC )
            ORG_MASK = .TRUE.
            OUTVAL = SUM_ORGS( OT_WD_OC, ORG_MASK, 
     &                         AEROMODE(:)%ALL_MASK, 0, 0, IV, ISRC )

         ! Calculate Dry Deposition of Primary OA
         CASE ( ID_DD_POA )
            ORG_MASK = OASPC(:)%PRIMARY
            OUTVAL = SUM_ORGS( OT_DD_OA, ORG_MASK, 
     &                         AEROMODE(:)%ALL_MASK, 0, 0, IV, ISRC )

         ! Calculate Wet Deposition of Primary OA
         CASE ( ID_WD_POA )
            ORG_MASK = OASPC(:)%PRIMARY
            OUTVAL = SUM_ORGS( OT_WD_OA, ORG_MASK, 
     &                         AEROMODE(:)%ALL_MASK, 0, 0, IV, ISRC )

         ! Calculate Dry Deposition of Primary OC
         CASE ( ID_DD_POC )
            ORG_MASK = OASPC(:)%PRIMARY
            OUTVAL = SUM_ORGS( OT_DD_OC, ORG_MASK, 
     &                         AEROMODE(:)%ALL_MASK, 0, 0, IV, ISRC )

         ! Calculate Wet Deposition of Primary OC
         CASE ( ID_WD_POC )
            ORG_MASK = OASPC(:)%PRIMARY
            OUTVAL = SUM_ORGS( OT_WD_OC, ORG_MASK, 
     &                         AEROMODE(:)%ALL_MASK, 0, 0, IV, ISRC )

         ! Calculate Dry Deposition of Secondary OA
         CASE ( ID_DD_SOA )
            ORG_MASK = .NOT. OASPC(:)%PRIMARY
            OUTVAL = SUM_ORGS( OT_DD_OA, ORG_MASK, 
     &                         AEROMODE(:)%ALL_MASK, 0, 0, IV, ISRC )

         ! Calculate Wet Deposition of Secondary OA
         CASE ( ID_WD_SOA )
            ORG_MASK = .NOT. OASPC(:)%PRIMARY
            OUTVAL = SUM_ORGS( OT_WD_OA, ORG_MASK, 
     &                         AEROMODE(:)%ALL_MASK, 0, 0, IV, ISRC )

         ! Calculate Dry Deposition of Secondary OC
         CASE ( ID_DD_SOC )
            ORG_MASK = .NOT. OASPC(:)%PRIMARY
            OUTVAL = SUM_ORGS( OT_DD_OC, ORG_MASK, 
     &                         AEROMODE(:)%ALL_MASK, 0, 0, IV, ISRC )

         ! Calculate Wet Deposition of Secondary OC
         CASE ( ID_WD_SOC )
            ORG_MASK = .NOT. OASPC(:)%PRIMARY
            OUTVAL = SUM_ORGS( OT_WD_OC, ORG_MASK, 
     &                         AEROMODE(:)%ALL_MASK, 0, 0, IV, ISRC )

         ! Calculate Aerosol Fraction of Benzo-a-Pyrene
         CASE ( ID_BENAPY_FAERO )
            CALL CALC_ELMO( OUTVAL=VAL,  OUT_UNIT=IU_UGpM3, 
     &                      J=J_PMF_BENAPY( ISRC+1 ), IERR=IERR )
            IF ( IERR .EQ. 1 ) THEN
               XMSG = 'PMF_BENAPY has not been defined in the chem control namelist'//
     &                'so BENAPY_FAERO cannot be calculated. Please define PMF_BENAPY.'
               CALL M3EXIT( 'ELMO_CALC_DERIVED',0,0,XMSG,1)
            END IF
            
            CALL CALC_ELMO( OUTVAL=VAL2, OUT_UNIT=IU_UGpM3, 
     &                      J=J_GAS_BENAPY( ISRC+1 ), IERR=IERR )
            IF ( IERR .EQ. 1 ) THEN
               XMSG = 'GAS_BENAPY has not been defined in the chem control namelist'//
     &                'so BENAPY_FAERO cannot be calculated. Please define GAS_BENAPY.'
               CALL M3EXIT( 'ELMO_CALC_DERIVED',0,0,XMSG,1)
            END IF
            
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

            CALL CALC_ELMO( OUTVAL=PMF_NO3, OUT_UNIT=IUNIT, 
     &                      J=J_PMF_NO3( ISRC+1 ), IERR=IERR )
            IF ( IERR .EQ. 1 ) THEN
               XMSG = 'PMF_NO3 has not been defined in the chem control namelist'//
     &                'so PMF_FRM cannot be calculated. Please define PMF_NO3.'
               CALL M3EXIT( 'ELMO_CALC_DERIVED',0,0,XMSG,1)
            END IF
            
            PMF_NO3_LOSS = MIN( 745.7/SRFTEMP * K2, PMF_NO3 )

            PMF_NH4_LOSS = PMF_NO3_LOSS * 18.0 / 62.0

            CALL CALC_ELMO( OUTVAL=PMF_NH4,  OUT_UNIT=IUNIT, 
     &                      J=J_PMF_NH4( ISRC+1 ), IERR=IERR )
            IF ( IERR .EQ. 1 ) THEN
               XMSG = 'PMF_NH4 has not been defined in the chem control namelist'//
     &                'so PMF_FRM cannot be calculated. Please define PMF_NH4.'
               CALL M3EXIT( 'ELMO_CALC_DERIVED',0,0,XMSG,1)
            END IF
            
            CALL CALC_ELMO( OUTVAL=PMF_SO4,  OUT_UNIT=IUNIT, 
     &                      J=J_PMF_SO4( ISRC+1 ), IERR=IERR ) 
            IF ( IERR .EQ. 1 ) THEN
               XMSG = 'PMF_SO4 has not been defined in the chem control namelist'//
     &                'so PMF_FRM cannot be calculated. Please define PMF_SO4.'
               CALL M3EXIT( 'ELMO_CALC_DERIVED',0,0,XMSG,1)
            END IF
            
            PMF = SUM_PM_MODES( AEROMODE%FINE_MASK, 0, 0, IV, ISRC ) 

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
               CALL M3EXIT( 'ELMO_CALC_DERIVED',0,0,XMSG,1)
            END IF
            
            PM25_NO3_LOSS = MIN( 745.7/SRFTEMP * K2, PM25_NO3 )

            PM25_NH4_LOSS = PM25_NO3_LOSS * 18.0 / 62.0

            CALL CALC_ELMO( OUTVAL=PM25_NH4, OUT_UNIT=IUNIT, 
     &                      J=J_PM25_NH4( ISRC+1 ), IERR=IERR )
            IF ( IERR .EQ. 1 ) THEN
               XMSG = 'PM25_NH4 has not been defined in the chem control namelist'//
     &                'so PM25_FRM cannot be calculated. Please define PM25_NH4.'
               CALL M3EXIT( 'ELMO_CALC_DERIVED',0,0,XMSG,1)
            END IF
            
            CALL CALC_ELMO( OUTVAL=PM25_SO4, OUT_UNIT=IUNIT, 
     &                      J=J_PM25_SO4( ISRC+1 ), IERR=IERR ) 
            IF ( IERR .EQ. 1 ) THEN
               XMSG = 'PM25_SO4 has not been defined in the chem control namelist'//
     &                'so PM25_FRM cannot be calculated. Please define PM25_SO4.'
               CALL M3EXIT( 'ELMO_CALC_DERIVED',0,0,XMSG,1)
            END IF
            
            PM25 = SUM_PM_MODES( AEROMODE%ALL_MASK, IPM25, IWET, IV, ISRC ) 

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
            OUTVAL = ELMO_KZ( C1,R1,L1 )

         CASE ( ID_KZMIN )
            OUTVAL = ELMO_KZMIN( C1,R1,L1 )

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
            OUTVAL = PBL

         ! Retrieve Solar Radiation at Ground
         CASE ( ID_RGRND )
            OUTVAL = RGRND

         ! Retrieve Precipitation
         CASE ( ID_PRECIP )
            IF ( RC .GE. 0.0 ) THEN
              OUTVAL = RN + RC
            ELSE
              OUTVAL = RN
            END IF

         ! Retrieve Wind Speed at 10 m
         CASE ( ID_WSPD10 )
            OUTVAL = WSPD10

         ! Retrieve Wind Direction at 10 m
         CASE ( ID_WDIR10 )
            OUTVAL = WDIR10

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

         END SELECT 
 
      END SUBROUTINE ELMO_CALC_DERIVED
 
!----------------------------------------------------------------------
      REAL FUNCTION SUM_PM_MODES( MODE_MASK, INLET, WETNESS, IV, ISRC )
!----------------------------------------------------------------------
!     SUM_PM_MODES returns the sum of total PM Mass ignoring water,
!     organic water, and mass of all aerosol tracer species. It uses the
!     MODE_MASK input variable, which is a vector of logical variables,
!     to determine which modes to sum.
!
!     The input variable IV instructs the function to sum for ISAM or
!     DDM variables instead, and the variable ISRC identifies which
!     source to sum.          
!----------------------------------------------------------------------

      USE AERO_DATA, ONLY: N_MODE, AEROSPC_CONC, AH2O_IDX, AORGH2O_IDX, 
     &                     AEROSPC, N_AEROSPC
      USE ELMO_DATA, ONLY : ET_CMPST, ET_DRVD
#ifdef isam      
      USE ELMO_DATA, ONLY : ET_ISAM_CMPST, ET_ISAM_DRVD, ISAM_VEC
      USE SA_DEFN, ONLY : MAP_AEROtoSA, NTAG_SA
#endif
#ifdef sens
      USE ELMO_DATA, ONLY : ET_DDM_CMPST, ET_DDM_DRVD, DDM_VEC
      USE AERO_DDM3D, ONLY : S_AEROSPC_CONC
#endif


      IMPLICIT NONE

      LOGICAL :: MODE_MASK( N_MODE )  ! Mask Indicating which modes to sum
      INTEGER :: IV, ISRC             ! ELMO Variable type, Source number
      INTEGER :: INLET                ! Integer prescribing the inlet type
      INTEGER :: WETNESS              ! 1 = Wet Aerosol; 2 = Dry Aerosol
                                      !   (see ELMO_INLET table in ELMO_DATA module)
      INTEGER :: IM, IAER, ISA
      REAL    :: FRAC
      REAL    :: OUTVAL
                                        
      OUTVAL = 0.0

      ! Determine Which Type of ELMO Variable is Requested
      SELECT CASE ( IV )
         CASE ( ET_CMPST, ET_DRVD )
            ! Standard ELMO PM Sum
            DO IM = 1,N_MODE
               IF ( MODE_MASK( IM ) ) THEN
                 CALL GET_AERO_INLET( INLET, IM, WETNESS, FRAC )
                 OUTVAL = OUTVAL + FRAC * 
     &                    SUM( AEROSPC_CONC( :,IM ),
     &                         MASK=.NOT.AEROSPC(:)%TRACER ) ! ug m-3
     &                    - FRAC * AEROSPC_CONC( AH2O_IDX,IM )
                 IF ( AORGH2O_IDX .GT. 0 ) OUTVAL = OUTVAL
     &                - FRAC * AEROSPC_CONC( AORGH2O_IDX,IM ) ! ug m-3 
               END IF
            END DO

#ifdef isam            
         CASE ( ET_ISAM_CMPST, ET_ISAM_DRVD )
           ! ISAM PM Sum
           DO IM = 1,N_MODE
              IF ( MODE_MASK(IM) ) THEN
                CALL GET_AERO_INLET( INLET, IM, WETNESS, FRAC )
                DO IAER = 1,N_AEROSPC                  
                  IF ( .NOT. AEROSPC(IAER)%TRACER .AND.
     &                 IAER .NE. AH2O_IDX .AND. IAER .NE. AORGH2O_IDX ) THEN
                    ! Add up PM for this source
                    ISA = MAP_AEROtoSA( IAER, IM )
                    IF ( ISRC .LE. NTAG_SA ) THEN
                       OUTVAL = OUTVAL + FRAC * ISAM_VEC(ISA,ISRC) 
                    ELSE
                       OUTVAL = OUTVAL + FRAC * SUM(ISAM_VEC( ISA,:) ) 
                    END IF
                  END IF
                END DO
              END IF
           END DO
#endif

#ifdef sens
         CASE ( ET_DDM_CMPST, ET_DDM_DRVD )
           ! DDM PM Sum
           DO IM = 1,N_MODE
              IF ( MODE_MASK(IM) ) THEN
                CALL GET_AERO_INLET( INLET, IM, WETNESS, FRAC )
                DO IAER = 1,N_AEROSPC                  
                  IF ( .NOT. AEROSPC(IAER)%TRACER .AND.
     &                 IAER .NE. AH2O_IDX .AND. IAER .NE. AORGH2O_IDX ) THEN
                    ! Add up PM for this source
                    OUTVAL = OUTVAL + FRAC * S_AEROSPC_CONC(IAER,IM,ISRC) 
                  END IF
                END DO
              END IF
           END DO
#endif          
 

      END SELECT
 
      SUM_PM_MODES = OUTVAL

      END FUNCTION SUM_PM_MODES

!----------------------------------------------------------------------
      RECURSIVE FUNCTION SUM_ORGS( ORG_TYPE, ORG_MASK, MODE_MASK, INLET, 
     &                        WETNESS, IV, ISRC ) RESULT( OUTVAL )
!----------------------------------------------------------------------
!     SUM_ORGS returns the sum of total Organic species given a mask
!     of which species to add. This mask should be of the same length
!     as those in the OASPC table. That length of ORG_MASK is 
!     N_OA_NotTracers. The input variable ORG_TYPE specifies which 
!     organic aerosol metric to calcualte (e.g. mass, carbon, OM:OC,
!     or O:C).
!
!     Size specifications are communicated via the MODE_MASK and INLET 
!     parameters.
!
!     The input variable IV instructs the function to sum for ISAM or
!     DDM variables instead, and the variable ISRC identifies which
!     source to sum.          
!----------------------------------------------------------------------

      USE AERO_DATA, ONLY: N_MODE, AEROSPC_CONC, AH2O_IDX, AORGH2O_IDX, 
     &                     AEROSPC, N_AEROSPC, AEROSPC_MAP, 
     &                     AEROMODE, APOC_IDX, APNCOM_IDX, AERO_MISSING
      USE CGRID_SPCS, ONLY : MAP_CGRIDtoWDEP
      USE VDIFF_MAP, ONLY : DEPV_MAP
      USE SOA_DEFN, ONLY : N_OA, N_OA_NotTracers, MAP_OAtoAERO, OASPC
      USE ELMO_DATA, ONLY : ET_CMPST, ET_DRVD, OT_OA, OT_OC, OT_OMOC, 
     &                      OT_OTOC, OT_DD_OA, OT_DD_OC, OT_WD_OA, 
     &                      OT_DD_OC, OT_WD_OC, ET_DRVD_DDEP, ET_DRVD_WDEP,
     &                      ET_ISAM_DRVD, ET_ISAM_DRVD_DDEP, ET_ISAM_DRVD_WDEP,
     &                      ET_DDM_DRVD, ET_DDM_DRVD_DDEP, ET_DDM_DRVD_WDEP,
     &                      DDEP_VEC, WDEP_VEC
#ifdef isam      
      USE ELMO_DATA, ONLY : ISAM_VEC
      USE SA_DEFN, ONLY : MAP_AEROtoSA, NTAG_SA
#endif
#ifdef sens
      USE ELMO_DATA, ONLY : ET_DDM_CMPST, ET_DDM_DRVD, DDM_VEC
      USE AERO_DDM3D, ONLY : S_AEROSPC_CONC
#endif


      IMPLICIT NONE

      INTEGER :: ORG_TYPE
      LOGICAL :: ORG_MASK( N_OA )
      LOGICAL :: MODE_MASK( N_MODE )  ! Mask Indicating which modes to sum
      INTEGER :: IV, ISRC             ! ELMO Variable type, Source number
      INTEGER :: INLET                ! Integer prescribing the inlet type
      INTEGER :: WETNESS              ! 1 = Wet Aerosol; 2 = Dry Aerosol
                                      !   (see ELMO_INLET table in ELMO_DATA module)
      INTEGER :: IM, IAER, ISA, IOA, IVAR
      REAL    :: FRAC( N_MODE ), VAL, VAL2,
     &           POC, POM, OtoC
      REAL    :: OUTVAL
                                        
      ! Loop through modes and calculate inlet fraction     
      IF ( ORG_TYPE .NE. OT_OMOC ) THEN
        DO IM = 1,N_MODE
           CALL GET_AERO_INLET( INLET, IM, WETNESS, FRAC(IM) )
        END DO
      END IF

      ! Calculate Organic Aerosol Metric
      OUTVAL = 0.0
 
      SELECT CASE ( ORG_TYPE )
        CASE ( OT_OA )
          ! Sum up organic aerosol mass
          DO IOA = 1,N_OA_NotTracers
             DO IM = 1,N_MODE
                ! Just Select Species from the Org_Mask
                IF ( ORG_MASK( IOA ) .AND. MODE_MASK( IM ) ) THEN
                    
                   IF ( IV .EQ. ET_DRVD ) THEN  ! Core Model
                     OUTVAL = OUTVAL + FRAC( IM ) 
     &                        * AEROSPC_CONC( MAP_OAtoAERO(IOA),IM )

                   ELSE IF ( IV .EQ. ET_ISAM_DRVD ) THEN  ! ISAM
#ifdef isam      
                     IVAR = MAP_AEROtoSA( MAP_OAtoAERO(IOA),IMODE )
                     IF ( ISRC .LE. NTAG_SA ) THEN
                       OUTVAL = OUTVAL + FRAC( IM ) * ISAM_VEC( IVAR,ISRC ) 
                     ELSE
                       OUTVAL = OUTVAL + FRAC( IM ) * SUM(ISAM_VEC( IVAR,: )) 
                     END IF
#endif
                   ELSE IF ( IV .EQ. ET_DDM_DRVD ) THEN  ! DDM
#ifdef sens      
                     IVAR = AEROSPC_MAP( MAP_OAtoAERO(IOA),IMODE )
                     OUTVAL = OUTVAL + FRAC( IM ) * DDM_VEC( ISRC,IVAR ) 
#endif
                   END IF 
                END IF
             END DO
          END DO
 
        CASE ( OT_OC )
          ! Sum up organic carbon mass
          DO IOA = 1,N_OA_NotTracers
             DO IM = 1,N_MODE 
                ! Just Select Species from the Org_Mask
                IF ( ORG_MASK( IOA ) .AND. MODE_MASK( IM ) ) THEN

                   IF ( IV .EQ. ET_DRVD ) THEN  ! Core Model
                     OUTVAL = OUTVAL + FRAC( IM )
     &                        * AEROSPC_CONC( MAP_OAtoAERO(IOA),IM )
     &                        / OASPC( IOA )%OMtoOC

                   ELSE IF ( IV .EQ. ET_ISAM_DRVD ) THEN  ! ISAM
#ifdef isam      
                     IVAR = MAP_AEROtoSA( MAP_OAtoAERO(IOA),IMODE )
                     IF ( ISRC .LE. NTAG_SA ) THEN
                       OUTVAL = OUTVAL + FRAC( IM )
     &                          * ISAM_VEC( IVAR,ISRC ) 
     &                          / OASPC( IOA )%OMtoOC
                     ELSE
                       OUTVAL = OUTVAL + FRAC( IM )
     &                          * SUM(ISAM_VEC( IVAR,: )) 
     &                          / OASPC( IOA )%OMtoOC
                     END IF
#endif

                   ELSE IF ( IV .EQ. ET_DDM_DRVD ) THEN  ! DDM
#ifdef sens      
                     IVAR = AEROSPC_MAP( MAP_OAtoAERO(IOA),IMODE )
                     OUTVAL = OUTVAL + FRAC( IM )
     &                        * DDM_VEC( ISRC,IVAR ) 
     &                        / OASPC( IOA )%OMtoOC
#endif

                   END IF
                END IF
             END DO
          END DO

        CASE ( OT_OMOC )
          ! Calculate OM:OC of Species selected by ORG_MASK
          VAL = SUM_ORGS( OT_OA, ORG_MASK, MODE_MASK, INLET, 
     &                    WETNESS, IV, ISRC )
          VAL2= SUM_ORGS( OT_OC, ORG_MASK, MODE_MASK, INLET, 
     &                    WETNESS, IV, ISRC )

          OUTVAL = VAL / ( VAL2 + TINY( 0.0 ) )
        
        CASE ( OT_OTOC )
          ! Calculate O:C of Species selected by ORG_MASK
          ! First retrieve OM of this selection
          VAL = SUM_ORGS( OT_OA, ORG_MASK, MODE_MASK, INLET, 
     &                    WETNESS, IV, ISRC )
        
          ! Now Calculate Weighted O:C
          VAL2 = 0.0
          DO IOA = 1,N_OA_NotTracers
             DO IM = 1,N_MODE
                IF ( ORG_MASK( IOA ) .AND. MODE_MASK( IM ) .AND.
     &               MAP_OAtoAERO(IOA) .NE. APOC_IDX .AND.
     &               MAP_OAtoAERO(IOA) .NE. APNCOM_IDX ) THEN
                
                   IF ( IV .EQ. ET_DRVD ) THEN   ! Core Model
                      ! Weight OA Sum by O:C
                      VAL2 = VAL2 + FRAC( IM )
     &                       * AEROSPC_CONC( MAP_OAtoAERO(IOA),IM )
     &                       * OASPC(IOA)%OtoC

                   ELSE IF ( IV .EQ. ET_ISAM_DRVD ) THEN  ! ISAM
#ifdef isam      
                     IVAR = MAP_AEROtoSA( MAP_OAtoAERO(IOA),IM )
                     IF ( ISRC .LE. NTAG_SA ) THEN
                       VAL2 = VAL2 + FRAC( IM )
     &                        * ISAM_VEC( IVAR,ISRC ) 
     &                        * OASPC( IOA )%OtoC
                     ELSE
                       VAL2 = VAL2 + FRAC( IM )
     &                        * SUM(ISAM_VEC( IVAR,: )) 
     &                        * OASPC( IOA )%OtoC
                     END IF
#endif

                   ELSE IF ( IV .EQ. ET_DDM_DRVD ) THEN  ! DDM
#ifdef sens      
                     IVAR = AEROSPC_MAP( MAP_OAtoAERO(IOA),IM )
                     VAL2 = VAL2 + FRAC( IM )
     &                      * DDM_VEC( ISRC,IVAR ) 
     &                      * OASPC( IOA )%OtoC
#endif

                   END IF
                   
                END IF
             END DO  
          END DO

          ! Add POC/PNCOM Contribution to Weighted O:C
          POC = 0.0
          POM = 0.0
          DO IM = 1,N_MODE
            IF ( MODE_MASK( IM ) ) THEN
               
               IF ( IV .EQ. ET_DRVD ) THEN  ! Core Model
                  IF ( APOC_IDX .NE. 0 ) 
     &               POC = POC + AEROSPC_CONC(APOC_IDX,IM) * FRAC(IM)
                  IF ( APNCOM_IDX .NE. 0 ) 
     &               POM = POM + AEROSPC_CONC(APNCOM_IDX,IM) * FRAC(IM)

               ELSE IF ( IV .EQ. ET_ISAM_DRVD ) THEN  ! ISAM
#ifdef isam      
                  IF ( APOC_IDX .NE. 0 ) THEN
                     IVAR = MAP_AEROtoSA( APOC_IDX,IM )
                     IF ( ISRC .LE. NTAG_SA ) THEN
                        POC = POC + FRAC(IM) 
     &                    * ISAM_VEC( IVAR,ISRC ) 
                     ELSE
                        POC = POC + FRAC(IM) 
     &                    * SUM(ISAM_VEC( IVAR,: ))
                  END IF
                  IF ( APNCOM_IDX .NE. 0 ) THEN
                     IVAR = MAP_AEROtoSA( APNCOM_IDX,IM )
                     IF ( ISRC .LE. NTAG_SA ) THEN
                        POM = POM + FRAC(IM) 
     &                    * ISAM_VEC( IVAR,ISRC ) 
                     ELSE
                        POM = POM + FRAC(IM) 
     &                    * SUM(ISAM_VEC( IVAR,: ))
                  END IF 
#endif

               ELSE IF ( IV .EQ. ET_DDM_DRVD ) THEN  ! DDM
#ifdef sens      
                  IF ( APOC_IDX .NE. 0 ) THEN
                     IVAR = AEROSPC_MAP( APOC_IDX,IM )
                     POC = POC + FRAC(IM) * DDM_VEC( ISRC,IVAR ) 
                  END IF
                  IF ( APNCOM_IDX .NE. 0 ) THEN
                     IVAR = AEROSPC_MAP( APNCOM_IDX,IM )
                     POM = POM + FRAC(IM) * DDM_VEC( ISRC,IVAR ) 
                  END IF
#endif
               END IF
 
            END IF
          END DO
          POM = POM + POC

          ! Calculate O:C from OM:OC using the relationship from Aiken
          ! et al. ES&T (2008), Fig. 4A. The best-fit line is rearranged to
          ! solve for O:C as a function of OM:OC.
          OtoC = 12./15. * ( POM / ( POC + TINY(0.0) ) )- 14./15.  
            
          OUTVAL = ( VAL2 + OtoC*POM ) / ( VAL + TINY(0.0))
 
        CASE ( OT_WD_OA )
          ! Sum up wet deposition of organic aerosol mass
          DO IOA = 1,N_OA_NotTracers
             DO IM = 1,N_MODE
                ! Just Select Species from the Org_Mask
                IF ( ORG_MASK( IOA ) .AND. MODE_MASK( IM ) ) THEN
                   IVAR = MAP_CGRIDtoWDEP( AEROSPC_MAP( MAP_OAtoAERO(IOA),IM ))
                   IF ( IVAR .GT. 0 ) THEN
                    
                     IF ( IV .EQ. ET_DRVD_DDEP ) THEN  ! Core Model
                       OUTVAL = OUTVAL + FRAC( IM ) * WDEP_VEC( IVAR )

                     ELSE IF ( IV .EQ. ET_ISAM_DRVD_DDEP ) THEN  ! ISAM
#ifdef isam      
                       IF ( ISRC .LE. NTAG_SA ) THEN
                         ! Just one source (ISRC)
                         OUTVAL = OUTVAL + FRAC( IM ) 
     &                            * ISAM_WDEP_VEC( IVAR,ISRC ) 
                       ELSE
                         ! Sum All Sources
                         OUTVAL = OUTVAL + FRAC( IM ) 
     &                            * SUM(ISAM_WDEP_VEC( IVAR,: )) 
                       END IF
#endif

                     ELSE IF ( IV .EQ. ET_DDM_DRVD_DDEP ) THEN  ! DDM
#ifdef sens      
                       ! Just one source (ISRC)
                       OUTVAL = OUTVAL + FRAC( IM ) 
     &                          * DDM_WDEP_VEC( IVAR,ISRC ) 
#endif

                     END IF
                   END IF 
                END IF
             END DO
          END DO
 
        CASE ( OT_WD_OC )
          ! Sum up wet deposition of organic carbon mass
          DO IOA = 1,N_OA_NotTracers
             DO IM = 1,N_MODE 
                ! Just Select Species from the Org_Mask
                IF ( ORG_MASK( IOA ) .AND. MODE_MASK( IM ) ) THEN
                   IVAR = MAP_CGRIDtoWDEP( AEROSPC_MAP( MAP_OAtoAERO(IOA),IM ))
                   IF ( IVAR .GT. 0 ) THEN

                     IF ( IV .EQ. ET_DRVD_DDEP ) THEN  ! Core Model
                       OUTVAL = OUTVAL + FRAC( IM ) * WDEP_VEC( IVAR )
     &                          / OASPC( IOA )%OMtoOC
                    
                     ELSE IF ( IV .EQ. ET_ISAM_DRVD_DDEP ) THEN  ! ISAM
#ifdef isam      
                       IF ( ISRC .LE. NTAG_SA ) THEN
                         ! Just one source (ISRC)
                         OUTVAL = OUTVAL + FRAC( IM )
     &                            * ISAM_WDEP_VEC( IVAR,ISRC ) 
     &                            / OASPC( IOA )%OMtoOC
                       ELSE
                         ! Sum All Sources
                         OUTVAL = OUTVAL + FRAC( IM )
     &                            * SUM(ISAM_WDEP_VEC( IVAR,: )) 
     &                            / OASPC( IOA )%OMtoOC
                       END IF
#endif
                    
                     ELSE IF ( IV .EQ. ET_DDM_DRVD_DDEP ) THEN  ! DDM
#ifdef sens      
                       ! Just one source (ISRC)
                       OUTVAL = OUTVAL + FRAC( IM )
     &                          * DDM_WDEP_VEC( IVAR,ISRC ) 
     &                          / OASPC( IOA )%OMtoOC
#endif
                    
                     END IF
                   END IF
                END IF
             END DO
          END DO
 
        CASE ( OT_DD_OA )
          ! Sum up dry deposition of organic aerosol mass
          DO IOA = 1,N_OA_NotTracers
             DO IM = 1,N_MODE
                ! Just Select Species from the Org_Mask
                IF ( ORG_MASK( IOA ) .AND. MODE_MASK( IM ) ) THEN
                   IVAR = DEPV_MAP( AEROSPC_MAP( MAP_OAtoAERO(IOA),IM ))
                   IF ( IVAR .GT. 0 ) THEN
                    
                     IF ( IV .EQ. ET_DRVD_DDEP ) THEN  ! Core Model
                       OUTVAL = OUTVAL + FRAC( IM ) * DDEP_VEC( IVAR )

                     ELSE IF ( IV .EQ. ET_ISAM_DRVD_DDEP ) THEN  ! ISAM
#ifdef isam      
                       IF ( ISRC .LE. NTAG_SA ) THEN
                         ! Just one source (ISRC)
                         OUTVAL = OUTVAL + FRAC( IM ) 
     &                            * ISAM_DDEP_VEC( IVAR,ISRC ) 
                       ELSE
                         ! Sum All Sources
                         OUTVAL = OUTVAL + FRAC( IM ) 
     &                            * SUM(ISAM_DDEP_VEC( IVAR,: )) 
                       END IF
#endif

                     ELSE IF ( IV .EQ. ET_DDM_DRVD_DDEP ) THEN  ! DDM
#ifdef sens      
                       ! Just one source (ISRC)
                       OUTVAL = OUTVAL + FRAC( IM ) 
     &                          * DDM_DDEP_VEC( IVAR,ISRC ) 
#endif

                     END IF
                   END IF 
                END IF
             END DO
          END DO
 
        CASE ( OT_DD_OC )
          ! Sum up dry deposition of organic carbon mass
          DO IOA = 1,N_OA_NotTracers
             DO IM = 1,N_MODE 
                ! Just Select Species from the Org_Mask
                IF ( ORG_MASK( IOA ) .AND. MODE_MASK( IM ) ) THEN
                   IVAR = DEPV_MAP( AEROSPC_MAP( MAP_OAtoAERO(IOA),IM ))
                   IF ( IVAR .GT. 0 ) THEN

                     IF ( IV .EQ. ET_DRVD_DDEP ) THEN  ! Core Model
                       OUTVAL = OUTVAL + FRAC( IM ) * DDEP_VEC( IVAR )
     &                          / OASPC( IOA )%OMtoOC
                    
                     ELSE IF ( IV .EQ. ET_ISAM_DRVD_DDEP ) THEN  ! ISAM
#ifdef isam      
                       IF ( ISRC .LE. NTAG_SA ) THEN
                         ! Just one source (ISRC)
                         OUTVAL = OUTVAL + FRAC( IM )
     &                            * ISAM_DDEP_VEC( IVAR,ISRC ) 
     &                            / OASPC( IOA )%OMtoOC
                       ELSE
                         ! Sum All Sources
                         OUTVAL = OUTVAL + FRAC( IM )
     &                            * SUM(ISAM_DDEP_VEC( IVAR,: )) 
     &                            / OASPC( IOA )%OMtoOC
                       END IF
#endif
                    
                     ELSE IF ( IV .EQ. ET_DDM_DRVD_DDEP ) THEN  ! DDM
#ifdef sens      
                       ! Just one source (ISRC)
                       OUTVAL = OUTVAL + FRAC( IM )
     &                          * DDM_DDEP_VEC( IVAR,ISRC ) 
     &                          / OASPC( IOA )%OMtoOC
#endif
                    
                     END IF
                   END IF
                END IF
             END DO
          END DO
 
        
      END SELECT

      END FUNCTION SUM_ORGS
 
