
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
      SUBROUTINE CALC_ELMO_DETAILED( OUTVAL, J )
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

      INTEGER, INTENT( IN ) :: J
      REAL, INTENT( INOUT ) :: OUTVAL
      INTEGER ILOC, IMODE, IVAR, IOA, IM, IV, ISRC
      REAL VAL4, VAL3, VAL2, VAL, FRAC( N_MODE )
      REAL PM01, PM1, PM25, PM10, PMAMS, PM25to10, POM, POC, OtoC
      REAL K, P1, P2, P3, a, K2, PMF_NO3, PMF_NO3_LOSS,
     &     PMF_NH4_LOSS, PMF_NH4, PMF_SO4, PMF_MASS,
     &     PM25_NH4, PM25_NO3, PM25_SO4, PM25_NO3_LOSS,
     &     PM25_NH4_LOSS

      ! First determine if this variable has been mapped already. If it
      ! has, send it back
      ILOC = ELMO_TABLEtoLOCAL( J )
      IF ( ILOC .GT. 0 ) THEN
        IF ( L_CALCULATED( ILOC ) ) THEN
           OUTVAL = ELMO_LOCAL( ILOC )
           RETURN
        END IF
      END IF

      OUTVAL = 0.0

      IDG = ELMO_TABLE( J )%ID
      IM  = ELMO_TABLE( J )%IM
      IV  = ELMO_TABLE( J )%VAR_TYPE
      ISRC= ELMO_TABLE( J )%SOURCE

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
            CALL CALC_ELMO( OUTVAL=VAL, IDG=ID_PMF_HP,  IV=ETYPE_ELMO )
            ! Retrieve AH2OIJ
            CALL CALC_ELMO( OUTVAL=VAL2,IDG=ID_PMF_H2O, IV=ETYPE_ELMO )
            ! HPMOLAL = AHPLUSIJ / AH2OIJ * 1000.0 [mol L-1]
            OUTVAL = VAL / VAL2 * 1000.0

         ! Calculate PMF_PH - Fine-Mode pH
         CASE ( ID_PMF_PH )
            ! Retrieve HPMOLAL
            CALL CALC_ELMO( OUTVAL=VAL, IDG=ID_PMF_HPM, IV=ETYPE_ELMO )
            ! Retrieve AH2OIJ
            CALL CALC_ELMO( OUTVAL=VAL2,IDG=ID_PMF_H2O, IV=ETYPE_ELMO )

            !PMF_PH = -1*LOG10(HPMOLAL)
#ifdef mpas
            OUTVAL = -9.999E36
#else
            OUTVAL = BADVAL3
#endif
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
            CALL CALC_ELMO( OUTVAL=VAL, IDG=ID_PMF_OA, IV=ETYPE_ELMO )
            CALL CALC_ELMO( OUTVAL=VAL2,IDG=ID_PMF_OC, IV=ETYPE_ELMO )
            OUTVAL = VAL / ( VAL2 + TINY( 0.0 ) )
            
         ! Calculate PMF_OtoC - Fine-Mode O:C 
         CASE ( ID_PMF_OtoC )
            CALL CALC_ELMO( OUTVAL=VAL, IDG=ID_PMF_OA, IV=ETYPE_ELMO  )

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

            ! Calculate O:C from OM:OC using the relationship from Aiken
            ! et al. ES&T (2008), Fig. 4A. The best-fit line is rearranged to
            ! solve for O:C as a function of OM:OC.
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
            CALL CALC_ELMO( OUTVAL=VAL, IDG=ID_AMS_OA, IV=ETYPE_ELMO )

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
               ! Calculate O:C from OM:OC using the relationship from Aiken
               ! et al. ES&T (2008), Fig. 4A. The best-fit line is rearranged to
               ! solve for O:C as a function of OM:OC.
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
            CALL CALC_ELMO( OUTVAL=VAL,  IDG=ID_PMF_BENAPY, IV=ETYPE_ELMO )
            CALL CALC_ELMO( OUTVAL=VAL2, IDG=ID_GAS_BENAPY, IV=ETYPE_ELMO )
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

            CALL CALC_ELMO( OUTVAL=PMF_NO3, IDG=ID_PMF_NO3, IV=ETYPE_ELMO )
            PMF_NO3_LOSS = MIN( 745.7/SRFTEMP * K2, PMF_NO3 )

            PMF_NH4_LOSS = PMF_NO3_LOSS * 18.0 / 62.0

            CALL CALC_ELMO( OUTVAL=PMF_NH4,  IDG=ID_PMF_NH4, IV=ETYPE_ELMO )
            CALL CALC_ELMO( OUTVAL=PMF_SO4,  IDG=ID_PMF_SO4, IV=ETYPE_ELMO ) 
            CALL CALC_ELMO( OUTVAL=PMF_MASS, IDG=ID_PMF_MASS,IV=ETYPE_ELMO ) 

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

            CALL CALC_ELMO( OUTVAL=PM25_NO3, IDG=ID_PM25_NO3, IV=ETYPE_ELMO )
            PM25_NO3_LOSS = MIN( 745.7/SRFTEMP * K2, PM25_NO3 )

            PM25_NH4_LOSS = PM25_NO3_LOSS * 18.0 / 62.0

            CALL CALC_ELMO( OUTVAL=PM25_NH4, IDG=ID_PM25_NH4, IV=ETYPE_ELMO )
            CALL CALC_ELMO( OUTVAL=PM25_SO4, IDG=ID_PM25_SO4, IV=ETYPE_ELMO ) 
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
#ifdef mpas
                 OUTVAL = -9.000E36
#else
                 OUTVAL = AMISS3
#endif
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

         END SELECT 
 
         ! Save the fact that this variable has been calculated
         IF ( ILOC .GT. 0 ) THEN
           L_CALCULATED( ILOC ) = .TRUE.
           ELMO_LOCAL( ILOC ) = OUTVAL
         END IF

      END SUBROUTINE CALC_ELMO_DETAILED
 
