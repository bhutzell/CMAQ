
      SUBROUTINE SA_WRAP_OZ( CGRID, CBFOR, ISAM, ISAMB4, JDATE, JTIME, TSTEP )

!*************************************************************
!  As a wrapper to apportion ozone tags by taking ratio of CGRIDs
!  after and before a process. 
!
!  Jun 6, 2013 - KRT....modified from SA_WRAP_GC.F, now only deals with
!                 2-regime ozone apportionment
!
!  Feb 22, 2011 - KRT...applied to CB05, though applicable to other
!                  mechanisms and/or solvers
!  Jun 1, 2011 - KRT...SULF produced from SO2 is to be tracked and fed back
!                      to sulfate conversion process in next time step 
!
!*************************************************************
      USE GRID_CONF
      USE CGRID_SPCS
      USE SA_GRD_DEFN  ! for SA_SULF_NEW array; also inherits SA_DEFN
      USE SA_IRR_DEFN  ! for apportioned reaction rates  inherits hrdata
      USE UTILIO_DEFN
      USE OZVOC_DEFN   ! 20120928 

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID

! Arguments
      REAL, POINTER             :: CGRID( :,:,:,: )
      REAL, POINTER             :: CBFOR( :,:,:,: )
      REAL, POINTER             :: ISAM( :,:,:,:,: )
      REAL, POINTER             :: ISAMB4( :,:,:,:,: )
      INTEGER, INTENT( IN )     :: JDATE, JTIME
      INTEGER, INTENT( IN )     :: TSTEP( 3 ) ! 1 local output timestep;
                                              ! 2 sciproc sync timestep.

! Scratch
      CHARACTER( 16 ), SAVE :: PNAME = 'SA_WRAP_OZ'
      INTEGER C,R,L,S,V,K
      REAL, ALLOCATABLE, SAVE :: TTLB4( :,:,:,: )
      INTEGER  LENSTR
      CHARACTER( 96 )  :: XMSG = ' '

! NOX related
      INTEGER, SAVE :: JNO, JNO2, JNO3, JHONO
      INTEGER, SAVE :: JN2O5, JPNA, JPAN, JPANX
      INTEGER, SAVE :: JHNO3, JNTR
      REAL, ALLOCATABLE, SAVE :: CGNEW( : )
      REAL, ALLOCATABLE, SAVE :: CGOLD( : )
      REAL, ALLOCATABLE, SAVE :: CGMID( : )

! SULF related Now Removed 20130606

! Ozone related 20120908
      INTEGER, SAVE :: JO3V, JO3N
      REAL             DENOMO3 ! sum over tags for either regime of O3 in each grid cell
      REAL,    SAVE :: R_OZTAGS  ! number of ozone tags in real type

! Regime indicator related
      CHARACTER(16)    INDICNAME   ! file name to hold indicator values
      CHARACTER(7)     CHDATE
!      INTEGER, SAVE :: C_H2O2, C_HNO3, C_O3, C_FORM
!      INTEGER, SAVE :: C_NO, C_NO2, C_NO3, C_N2O5, C_PAN
      LOGICAL          COND_INDIC
      REAL             DENOM1, DENOM2
      REAL             VOTE_NOX
!      REAL             P_H2O2, P_ROOH, P_HNO3

! VOC related 20120928; voc species made explicit, 20131211
      INTEGER, ALLOCATABLE, SAVE :: JVOC( : )
      REAL             DO3, PO3  ! ozone destruction and production
      INTEGER, SAVE :: INDICTYP  ! Type of O3 regime indicator: 
      INTEGER          IOST
      REAL          :: INDICREG( N_INDIC )  ! indicators of ozone regimes
      REAL, ALLOCATABLE, SAVE ::  NUMERO( : )     ! For each tag, sum over all species each product of voc and respective mir
      REAL             DENOMINO     ! Tag-total of the product: VOC tag times weighted MIR tag
      INTEGER          JV        ! index running through VOC list from 1 to N_EVOC

! Logical
      LOGICAL, SAVE :: FIRSTIME = .TRUE.


!-------------------------------------------------------------------

      IF ( .NOT. ANY( L_OZONE ) ) RETURN
      IF ( .NOT. YES_2REGIME ) RETURN

      IF ( FIRSTIME ) THEN
        FIRSTIME = .FALSE.
      
        !krt Identify species index in ISAM array
      
        JNO   = INDEX1( 'NO' ,  NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        JNO2  = INDEX1( 'NO2',  NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        JNO3  = INDEX1( 'NO3',  NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        JHONO = INDEX1( 'HONO', NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        JN2O5 = INDEX1( 'N2O5', NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        JPNA  = INDEX1( 'PNA',  NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        JPAN  = INDEX1( 'PAN',  NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        JPANX = INDEX1( 'PANX', NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        JHNO3 = INDEX1( 'HNO3', NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        JNTR  = INDEX1( 'NTR',  NSPC_SA, SPC_NAME( :, OTHRTAG ) )

        JO3V  = INDEX1( 'O3V',  NSPC_SA, SPC_NAME( :, OTHRTAG ) )
        JO3N  = INDEX1( 'O3N',  NSPC_SA, SPC_NAME( :, OTHRTAG ) )

        IF ( .NOT. ALLOCATED( JVOC ) ) ALLOCATE( JVOC( N_EVOC ) )
        if ( mype .eq. 0 ) print*,'jv, nam_voc(jv), jvoc(jv):'
        DO JV = 1, N_EVOC
          JVOC( JV )  = INDEX1( NAM_VOC( JV ),  NSPC_SA, SPC_NAME( :, OTHRTAG ) )
          if ( mype .eq. 0 ) print*,JV, NAM_VOC( JV ), JVOC( JV )
        ENDDO ! jv

        IF ( .NOT. ALLOCATED( TTLB4 ) )
     &    ALLOCATE( TTLB4( MY_NCOLS, MY_NROWS, NLAYS, NSPC_SA ) )
      
        IF ( .NOT. ALLOCATED( CGNEW ) ) ALLOCATE( CGNEW( N_SPEC ) )
      
        IF ( .NOT. ALLOCATED( CGOLD ) ) ALLOCATE( CGOLD( N_SPEC ) )

        IF ( .NOT. ALLOCATED( CGMID ) ) ALLOCATE( CGMID( N_SPEC ) )

        ! 20131211
        IF ( .NOT. ALLOCATED( NUMERO ) ) ALLOCATE( NUMERO( NTAG_SA ) )

         ! Determine O3 regime indicator type
         INDICTYP = ENVINT( 'O3INDICATOR_TYPE','1=P(perox)/P(HNO3), etc', -999, IOST )
         IF ( IOST .NE. 0 )
     &    CALL M3EXIT( PNAME, 0, 0, 'O3 indicator type undetermined', 2 )

         ! Choose to write out the regime indicator
         YES_INDIC = ENVYN( 'WR_INDICATOR',
     &           'Want to output peroxides-to-hno3 as regime indicator?',
     &           .FALSE., IOST )
         IF ( YES_INDIC ) THEN
           CALL OP_O3INDICAT ( JDATE, JTIME, TSTEP )
         ENDIF ! yes_indic
  
         ! Count number of ozone tags in the first place
         R_OZTAGS = REAL( COUNT( L_OZONE ) )
 
    
      ENDIF ! firstime
      
      !krt Sum up pre-process tags for each species at each grid cell
      TTLB4 = 0.0
      CGNEW = 0.0
      CGOLD = 0.0
      DO S = 1, NSPC_SA
        DO K = 1, NTAG_SA
          DO L = 1, NLAYS
            DO R = 1, MY_NROWS
              DO C = 1, MY_NCOLS
                TTLB4( C,R,L,S ) = TTLB4( C,R,L,S ) + ISAMB4( C,R,L,S,K )
              ENDDO ! C
            ENDDO ! R
          ENDDO ! L
        ENDDO ! K
      ENDDO ! S

!KRT20130131 initialize ozone indicator
      IF ( YES_INDIC ) THEN
        INDICVALU = -1.0
        INDORIG = 1.E-30
      ENDIF ! yes_indic

Ckrt Apportion the conc
      DO L = 1, NLAYS
        DO R = 1, MY_NROWS
          DO C = 1, MY_NCOLS
            DO V = 1, N_SPEC
              CGNEW( V ) = CGRID( C,R,L,V )
              CGOLD( V ) = CBFOR( C,R,L,V )
            ENDDO ! loop of regular species
            CGMID = 0.5 * ( CGNEW + CGOLD )

! the call for pl_cb05 now removed  20130606

            ! Calculate production of H2O2, ROOH, and HNO3
            P_H2O2  =   RKI_INIT( C,R,L,34 ) * CGMID( HO2 ) * CGMID( HO2 )
     &            + RKI_INIT( C,R,L,35 ) * CGMID( HO2 ) * CGMID( HO2 )
     &            + RKI_INIT( C,R,L,42 ) * CGMID( OH ) * CGMID( OH )
            P_ROOH  =  RKI_INIT( C,R,L,56 ) * CGMID( XO2 ) * CGMID( HO2 )
     &            + RKI_INIT( C,R,L,57 ) * CGMID( XO2N ) * CGMID( HO2 )
            P_HNO3  =  2.000 * RKI_INIT( C,R,L, 19 ) * CGMID( N2O5 )
     &            + 2.000 * RKI_INIT( C,R,L, 20 ) * CGMID( N2O5 )
     &            +         RKI_INIT( C,R,L, 28 ) * CGMID( NO2 ) * CGMID( OH )
     &            +         RKI_INIT( C,R,L, 48 ) * CGMID( NO3 ) * CGMID( HO2 )
     &            +         RKI_INIT( C,R,L, 61 ) * CGMID( NTR ) * CGMID( OH )
     &            +         RKI_INIT( C,R,L, 77 ) * CGMID( FORM ) * CGMID( NO3 )
     &            +         RKI_INIT( C,R,L, 85 ) * CGMID( ALD2 ) * CGMID( NO3 )
     &            +         RKI_INIT( C,R,L,100 ) * CGMID( ALDX ) * CGMID( NO3 )
     &            +         RKI_INIT( C,R,L,132 ) * CGMID( CRES ) * CGMID( NO3 )
     &            +         RKI_INIT( C,R,L,136 ) * CGMID( CRON ) * CGMID( NO3 )
     &            +         RKI_INIT( C,R,L,146 ) * CGMID( OPEN ) * CGMID( NO3 )
     &            +         RKI_INIT( C,R,L,148 ) * CGMID( CAT1 ) * CGMID( NO3 )
     &            + 0.150 * RKI_INIT( C,R,L,163 ) * CGMID( NO3 ) * CGMID( ISPD )


Ckrt....20120928 Ozone
            IF ( JO3V .GT. 0 ) THEN
              ! Ozone production attributed to either regime
              DENOM1 = CBFOR(C,R,L, NO)+CBFOR(C,R,L, NO2)
              DENOM2 = CBFOR(C,R,L, NO3) + CBFOR(C,R,L, HNO3)
     &                 + CBFOR(C,R,L, N2O5) + CBFOR(C,R,L, PAN)
              INDICREG( 1 ) = ( P_H2O2 + P_ROOH ) / MAX( P_HNO3, 1.E-30 )
              INDICREG( 2 ) = P_H2O2 / MAX( P_HNO3, 1.E-30 )
              INDICREG( 3 ) = CBFOR( C,R,L, H2O2 )/ MAX( CBFOR( C,R,L, HNO3 ) , 1.E-30 )
              INDICREG( 4 ) = CBFOR ( C,R,L, H2O2 )
     &             / MAX( CBFOR(C,R,L, O3)+CBFOR(C,R,L, NO2 ) , 1.E-30 )
              INDICREG( 5 ) = DENOM1 + DENOM2
              INDICREG( 6 ) = CBFOR( C,R,L, O3 ) / MAX( DENOM1, 1.E-30 )
              INDICREG( 7 ) = CBFOR( C,R,L, O3 ) 
     &              / MAX( DENOM1 + DENOM2, 1.E-30 )
              INDICREG( 8 ) = CBFOR( C,R,L, O3 ) / MAX( DENOM2, 1.E-30 )
              INDICREG( 9 ) = CBFOR( C,R,L, FORM ) 
     &              / MAX( DENOM1 + DENOM2, 1.E-30 )
              INDICREG( 10 ) = CBFOR( C,R,L, FORM ) 
     &                / MAX( CBFOR(C,R,L, NO2), 1.E-30 )

              DO3 = CGRID( C,R,L, SPC_INDEX(JO3V,2)) - CBFOR( C,R,L, SPC_INDEX(JO3V,2))
              IF ( DO3 .LE. 0.0 ) THEN  ! overall ozone destruction in each grid cell
                ! Sum of tags based on which tags are updated 20130409
                DENOMO3 = 0.0
                DO K = 1, NTAG_SA
                  DENOMO3 = DENOMO3 + ISAMB4( C,R,L,JO3N,K ) + ISAMB4( C,R,L,JO3V,K )
                ENDDO ! loop K for sum of tags
                DO K = 1, NTAG_SA
                  IF ( CBFOR( C,R,L, SPC_INDEX(JO3V,2) ) .LE. 1.E-20 ) THEN
                    ISAM( C,R,L, JO3N, K ) = 0.0
                    ISAM( C,R,L, JO3V, K ) = 0.0
                  ELSE
                    IF ( DENOMO3 .LE. 1.E-20 ) THEN
                      ISAM( C,R,L, JO3N, K ) = 0.0
                      ISAM( C,R,L, JO3V, K ) = 0.0
                    ELSE
                      ISAM( C,R,L, JO3N, K ) = ISAMB4( C,R,L, JO3N, K ) 
     &                         + DO3  * ISAMB4( C,R,L, JO3N, K ) / DENOMO3
                      ISAM( C,R,L, JO3V, K ) = ISAMB4( C,R,L, JO3V, K ) 
     &                         + DO3  * ISAMB4( C,R,L, JO3V, K ) / DENOMO3
                      IF ( ISAM( C,R,L,JO3N,K ) .LT. 0.0 ) ISAM( C,R,L,JO3N,K ) = 1.E-30
                      IF ( ISAM( C,R,L,JO3V,K ) .LT. 0.0 ) ISAM( C,R,L,JO3V,K ) = 1.E-30
                    ENDIF ! denomo3 > 0 ?
                  ENDIF ! cbfor > 0 ?
                ENDDO ! loop K
              ELSE   ! overall ozone production in each grid cell
                IF ( YES_INDIC .OR. INDICTYP .EQ. N_INDIC + 1 ) THEN
                  VOTE_NOX = 0.0
                  DO S = 1 , N_INDIC
                   IF ( S .NE. 5 ) THEN
                    COND_INDIC = INDICREG( S ) .GE. TRANSV( S )
                   ELSE
                    COND_INDIC = INDICREG( S ) .LE. TRANSV( S )
                   ENDIF ! indictyp != 5 ?
                   ! nox-limited?
                   IF ( COND_INDIC ) VOTE_NOX = VOTE_NOX + 1  
                  ENDDO ! S 1:n_indic
                  INDICVALU( C,R,L, N_INDIC+1 ) = VOTE_NOX / REAL(N_INDIC)
                ENDIF ! yes_indic
                IF ( INDICTYP .EQ. 5 ) THEN
                  COND_INDIC = INDICREG(INDICTYP) .LE. TRANSV(INDICTYP)
                ELSEIF ( INDICTYP .EQ. N_INDIC + 1 ) THEN
                  COND_INDIC = VOTE_NOX .GT. N_INDIC / 2.0
                ELSE
                  COND_INDIC = INDICREG(INDICTYP) .GE. TRANSV(INDICTYP)
                ENDIF ! indictyp != 5 ?
                IF ( COND_INDIC ) THEN ! nox-limited in the abundance of VOC
                  DO K = 1, NTAG_SA
                    IF ( TTLB4( C,R,L,JNO ) + TTLB4( C,R,L,JNO2 ) .GT. 1.E-20 ) THEN
                      ISAM( C,R,L, JO3N, K ) = ISAMB4( C,R,L, JO3N, K )
     &                        + DO3 
     &                         * ( ISAMB4(C,R,L, JNO, K) + ISAMB4(C,R,L, JNO2, K) )
     &                         / ( TTLB4( C,R,L, JNO ) + TTLB4( C,R,L, JNO2 ) )
                    ELSE
                      ISAM( C,R,L, JO3N, K ) = ISAMB4( C,R,L, JO3N, K )
     &                                        + DO3 / REAL( NTAG_SA )
                    ENDIF ! bulk no + bulk no2 > 0 ?
                  ENDDO ! loop K
                ELSE  ! voc-limited in the abundance of NOx
                  NUMERO = 0.0
                  DENOMINO = 0.0
!20131212         if ( mype .eq. 0 .and. C .eq. 1 .and. R .eq. 1 .and. L .eq. 1 ) 
!20131212     &  print*,'ktag, jv, nam_voc(jv), jvoc(jv)'
                  DO K = 1, NTAG_SA
                    DO JV = 1, N_EVOC
!20131212             if ( mype .eq. 0 .and. C .eq. 1 .and. R .eq. 1 .and. L .eq. 1 ) 
!20131212     &   print*,K, JV, NAM_VOC(JV), JVOC( JV )
                      NUMERO( K ) = NUMERO( K ) 
     &   + ISAMB4( C,R,L,JVOC(JV), K ) * MIR( JV )
                    ENDDO ! jv
                    DENOMINO = DENOMINO + NUMERO( K )
                  ENDDO ! loop K
                  DO K = 1, NTAG_SA
                    IF ( DENOMINO .GT. 1.E-20 ) THEN
                      ISAM( C,R,L, JO3V, K ) = ISAMB4( C,R,L, JO3V, K )
     &                        + DO3 * NUMERO( K ) / DENOMINO
                    ELSE
                      ISAM( C,R,L, JO3V, K ) = ISAMB4( C,R,L, JO3V, K )
     &                        + DO3 / R_OZTAGS
                    ENDIF ! vxmir > 0 ?
                  ENDDO ! loop K
                ENDIF ! indicreg >= 0.5 ?
                ! Optional storage of all kinds of indicators
                IF ( YES_INDIC ) THEN 
                  DO S = 1, N_INDIC
                    INDICVALU( C,R,L,S ) = INDICREG( S )
                  ENDDO ! S 1:n_indic
                  !20140305 outputs indicators' originating variables
                  INDORIG( C,R,L,1 ) = P_H2O2
                  INDORIG( C,R,L,2 ) = P_ROOH
                  INDORIG( C,R,L,3 ) = P_HNO3
                  INDORIG( C,R,L,4 ) = CBFOR( C,R,L, H2O2 )
                  INDORIG( C,R,L,5 ) = CBFOR( C,R,L, HNO3 )
                  INDORIG( C,R,L,6 ) = CBFOR( C,R,L, O3 )
                  INDORIG( C,R,L,7 ) = CBFOR( C,R,L, NO )
                  INDORIG( C,R,L,8 ) = CBFOR( C,R,L, NO2 )
                  INDORIG( C,R,L,9 ) = CBFOR( C,R,L, NO3 )
                  INDORIG( C,R,L,10 ) = CBFOR( C,R,L, N2O5 )
                  INDORIG( C,R,L,11 ) = CBFOR( C,R,L, PAN )
                  INDORIG( C,R,L,12 ) = CBFOR( C,R,L, FORM )
                ENDIF ! yes_indic
              ENDIF ! cgrid - cbfor < 0 ?             

            ENDIF ! jo3v ( and hence jo3n ) > 0

          ENDDO ! C
        ENDDO ! R
      ENDDO ! L

      !Optional writeout of indicator values
      IF ( YES_INDIC ) THEN
        IF ( MOD( JTIME, TSTEP( 1 ) ) .EQ. 0 ) THEN
         DO S = 1, N_INDIC
          IF ( .NOT. WRITE3( O3INDIC_1, VNAM_INDIC(S), JDATE, JTIME,
     &              INDICVALU( :,:,1,S ) ) ) THEN
            XMSG = 'Could not write to ' // VNAM_INDIC(S)
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          ENDIF
         ENDDO ! 1 to n_indic
         IF ( .NOT. WRITE3( O3INDIC_1, 'VOTE_NOX', JDATE, JTIME,
     &              INDICVALU( :,:,1,N_INDIC+1 ) ) ) THEN
           XMSG = 'Could not write to ' // VNAME3D( N_INDIC + 1 )
           CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         ENDIF
         !20140305 outputs indicators' originating variables
         DO S = 1, N_INDORIG
          IF ( .NOT. WRITE3( O3INDIC_1, VNAM_INDORIG(S), JDATE, JTIME,
     &              INDORIG( :,:,1,S ) ) ) THEN
            XMSG = 'Could not write to ' // VNAM_INDORIG(S)
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          ENDIF
         ENDDO ! 1 to n_indic
        ENDIF ! hour loop
      ENDIF ! yes_indic

      END 

