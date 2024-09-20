
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
      
      MODULE ELMO_DERIVED_FXNS

      CONTAINS 

!----------------------------------------------------------------------
      REAL FUNCTION SUM_PM_MODES( MODE_MASK, INLET, WETNESS, IV, ISRC )
     &              RESULT( OUTVAL )
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
 
      RETURN
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
      RETURN
      END FUNCTION SUM_ORGS
      END MODULE
