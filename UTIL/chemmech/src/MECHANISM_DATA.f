       MODULE MECHANISM_DATA
       
         USE MECHANISM_PARMS

         IMPLICIT NONE



         CHARACTER( 120 ) :: EQUATIONS_MECHFILE
         CHARACTER(  32 ) :: MECHNAME
         
         INTEGER KUNITS, 
     &           KTYPE       ( MAXRXNUM ),
     &           IRXBITS     ( MAXRXNUM ),
     &           IORDER      ( MAXRXNUM ),
     &           KTN1, KRX1  ( MAXRXNUM ), 
     &           KTN2, KRX2  ( MAXRXNUM ), 
     &           KTN3, KRX3  ( MAXRXNUM ),
     &           KTN4, KRX4  ( MAXRXNUM ),
     &           KTN5, KRX5  ( MAXRXNUM ),
     &           KTN6, KRX6  ( MAXRXNUM ),
     &           KTN7, KRX7  ( MAXRXNUM ),
!    &           KCNV, KRXCNV( MAXRXNUM ),
     &           NFALLOFF, 
     &           IRRFALL( MAXFALLOFF ),
     &           HAL_PHOTAB( MAXRXNUM )

         INTEGER NWM,   NRXWM  ( MAX3BODIES )
         INTEGER NWW,   NRXWW  ( MAX3BODIES )
         INTEGER NWO2,  NRXWO2 ( MAX3BODIES )
         INTEGER NWN2,  NRXWN2 ( MAX3BODIES )
         INTEGER NWCH4, NRXWCH4( MAX3BODIES )
         INTEGER NWH2,  NRXWH2 ( MAX3BODIES )

         REAL( 8 ) :: RTDAT( 3,MAXRXNUM )
         REAL( 8 ) :: RFDAT( 5,MAXFALLOFF )
         REAL( 8 ) :: CONST( MAXCONSTS )        
          
         INTEGER         :: NPDERIV                    ! number nonzero PD in mechanism
         INTEGER         :: NMPHOT                     ! number of photolysis reactions
         INTEGER         :: NSUNLIGHT                  ! number of sunlight reactions
         INTEGER         :: ZERO_REACT_SUNLIGHT  = 0   ! number zero reactant reactions in sunlight reactions
         INTEGER         :: ONE_REACT_SUNLIGHT   = 0   ! number one reactant reactions in sunlight reactions
         INTEGER         :: NTHERMAL                   ! number of thermal (non-sunlight-dependent) reactions
         INTEGER         :: ONE_REACT_THERMAL    = 0   ! number one reactant reactions in thermal (non-sunlight-dependent) reactions
         INTEGER         :: ZERO_REACT_THERMAL   = 0   ! number zero reactant reactions in thermal (non-sunlight-dependent) reactions
         INTEGER         :: TWO_REACT_THERMAL    = 0   ! number second order reactions in thermal (non-sunlight-dependent) reactions
         INTEGER         :: THREE_REACT_THERMAL  = 0   ! number three reactant reactions in thermal (non-sunlight-dependent) reactions
         INTEGER         :: NSPECIAL_RXN               ! number of special rate constant reactions
         INTEGER         :: ISPECIAL( MAXSPECRXNS,2 )
         INTEGER         :: NSPECIAL                   ! number of special rate expressions
         INTEGER         :: MSPECTERMS                 ! highest number of terms in the expressions

         INTEGER         :: ZERO_REACT_REACTIONS  = 0  ! number zero reactant reactions
         INTEGER         :: ONE_REACT_REACTIONS   = 0  ! number one reactant reactions
         INTEGER         :: TWO_REACT_REACTIONS   = 0  ! number second order reactions
         INTEGER         :: THREE_REACT_REACTIONS = 0  ! number three reactant reactions

         CHARACTER( 16 ) :: SPECIAL( MAXSPECRXNS )     

         INTEGER         :: NKC_TERMS(  MAXSPECRXNS )
         CHARACTER( 16 ) :: KC_TERMS(   MAXSPECRXNS,  MAXSPECTERMS, 2)
         INTEGER         :: INDEX_KTERM( MAXSPECRXNS, MAXSPECTERMS)
         INTEGER         :: INDEX_CTERM( MAXSPECRXNS, MAXSPECTERMS)
         REAL( 8 )       :: KC_COEFFS(  MAXSPECRXNS,  MAXSPECTERMS)
         INTEGER         :: N_OPERATORS( MAXSPECRXNS )
         INTEGER         :: OPERATORS( MAXSPECRXNS, MAXSPECTERMS )
         REAL( 8 )       :: OPERATOR_COEFFS( MAXSPECRXNS, MAXSPECTERMS)
	 
	 INTEGER, ALLOCATABLE :: ORDER_SPECIAL( : )
         
         INTEGER IPH( MAXPHOTRXNS,3 )
         INTEGER NPHOTAB                          ! no. of unique photolysis rates
         CHARACTER( 16 ) :: PHOTAB( MAXPHOTRXNS ) ! photolysis rate name or label

         INTEGER IHETERO( MAXPHOTRXNS,2 )         ! mapping bewtween reaction # and unique heteorogeneous rates
         INTEGER MHETERO                          ! no. of heteorogeneous reactions
         INTEGER NHETERO                          ! no. of unique heteorogeneous rates 
         CHARACTER( 16 ) :: HETERO( MAXPHOTRXNS ) ! names of unique heteorogeneous rates

         INTEGER MXPRD                            ! max no. products

         INTEGER   :: NPRDCT( MAXRXNUM )               ! no. of products for rx j
         INTEGER   :: NREACT( MAXRXNUM )               ! no. of reactants for rx j
         INTEGER   :: IRR( MAXRXNUM,MAXPRODS+3 )
         REAL( 8 ) ::  SC ( MAXRXNUM,MAXPRODS )
         REAL( 8 ), ALLOCATABLE, SAVE :: NET_RCOEFF( :, :)

c.. Variables for steady-state species
         INTEGER         :: N_SS_SPC = 0                         ! No. of SS species
         CHARACTER( 16 ) :: SS_SPC( MAXNLIST )                   ! List of SS pecies names
         INTEGER         :: SS_RCT_COEF( MAXNLIST, MAXRXNUM )    ! Reactant coeffs for each SS species
         REAL            :: SS_PRD_COEF( MAXNLIST, MAXRXNUM )    ! Product coeffs for each SS species
         INTEGER         :: MAX_SS_LOSS = 0                      ! Max no of reactions for which 1 SS species
                                                              ! appears as a reactant
         INTEGER         :: MAX_SS_PROD = 0                      ! Max no of reactions for which 1 SS species
                                                              ! appears as a product
         INTEGER         :: N_LOSS_RXNS( MAXNLIST )              ! No. of loss rxns for each SS species
         INTEGER         :: N_PROD_RXNS( MAXNLIST )              ! No. of prod rxns for each SS species
         INTEGER         :: SS_LOSS_RXNS( MAXNLIST, MAXRXNUM )   ! List of rxns in which SS species is a reactant
         INTEGER         :: SS_PROD_RXNS( MAXNLIST, MAXRXNUM )   ! List of rxns in which SS species is a product
         INTEGER         :: SS_RCT_IND( MAXRXNUM )               ! SS spc ind that reacts w/ a non-SS spc
         REAL            :: SS_PROD_COEF( MAXNLIST, MAXRXNUM )   ! Yields for rxns producing a SS species
         

         CHARACTER( 16 ) RXLABEL( MAXRXNUM )   ! label for rx 

         CHARACTER( 120 ) :: EQNAME_SPCS
         CHARACTER( 120 ) :: EQNAME_RXDT
         CHARACTER( 120 ) :: EQNAME_RXCM
         CHARACTER( 120 ) :: FNAME_MODULE
         CHARACTER( 120 ) :: FNAME_DATA_MODULE
         CHARACTER( 120 ) :: FNAME_FUNC_MODULE
         CHARACTER( 120 ) :: OUTDIR 

         INTEGER        ::  EXUNIT_SPCS
         INTEGER        ::  EXUNIT_RXDT
         INTEGER        ::  EXUNIT_RXCM
         
         INTEGER        ::  MODULE_UNIT
         INTEGER        ::  DATA_MODULE_UNIT
         INTEGER        ::  FUNC_MODULE_UNIT
         

         LOGICAL, SAVE      :: USE_SPCS_NAMELISTS  ! species data based on CMAQ NMLS
         LOGICAL, SAVE      :: WRITE_CGRID_DATA  = .TRUE.

         INTEGER,            ALLOCATABLE ::  CGRID_INDEX  ( : )
         INTEGER,            ALLOCATABLE ::  TYPE_INDEX   ( : )
         LOGICAL, SAVE,      ALLOCATABLE ::  CONVERT_CONC ( : )
         REAL,               ALLOCATABLE ::  SPECIES_MOLWT( : )
         CHARACTER( 16),     ALLOCATABLE ::  CGRID_SPC    ( : )
         CHARACTER(LEN = 2), ALLOCATABLE ::  SPECIES_TYPE ( : )

         LOGICAL                      ::  HALOGEN_PARAMETER = .FALSE.          
         INTEGER                      ::  N_GAS_CHEM_SPC
         INTEGER                      ::  NUMB_MECH_SPCS
         INTEGER ,        ALLOCATABLE ::  MECHANISM_INDEX( : )
         CHARACTER( 16 ), ALLOCATABLE ::  MECHANISM_SPC  ( : )
         CHARACTER( 16 ), ALLOCATABLE ::  SPARSE_SPECIES ( : )

         INTEGER, PARAMETER :: NCONSTANT_SPECIES = 6

         TYPE REACTION
            CHARACTER( 16 ) :: LABEL( 2 )     ! name of reaction and if needed reference 
            INTEGER   :: IRXBITS              ! bit value for rate constant
            INTEGER   :: RATE_TYPE            ! type of rate constant
            INTEGER   :: NPRDCT               ! no. of products
            INTEGER   :: NREACT               ! no. of reactants
            INTEGER   :: ORDER                ! order of reaction
            INTEGER   :: IRR( MAXPRODS+3 )    ! reactant and product species indices
            INTEGER   :: HETEO_INDEX( 2 )     ! mechanism reaction indices if heterogeneous type
            INTEGER   :: PHOTO_INDEX( 3 )     ! mechanism reaction indices if rate constant photolysis
            INTEGER   :: FALLOFF_INDEX        ! mechanism reaction indices if falloff type
            INTEGER   :: SPECIAL_INDEX( 2 )   ! mechanism reaction indices if rate constant a special expression
            REAL( 8 ) :: SC( MAXPRODS )       ! product stiochometric coefficients
            REAL( 8 ) :: RTDAT( 3 )           ! general data for rate constant
            REAL( 8 ) :: RFDAT( 5 )           ! data of fall rate constant type
            INTEGER   :: NAIR_RCTNTS          ! # times M or air a reactant
            INTEGER   :: NH2O_RCTNTS          ! # times water a reactant
            INTEGER   :: N_O2_RCTNTS          ! # times O2 a reactant
            INTEGER   :: N_N2_RCTNTS          ! # times H2 a reactant
            INTEGER   :: N_H2_RCTNTS          ! # times N2 a reactant
            INTEGER   :: NCH4_RCTNTS          ! # times methane a reactant
         END TYPE REACTION
 
         TYPE ( REACTION ), ALLOCATABLE :: PHOTOLYSIS_REACTIONS( : )  
         TYPE ( REACTION ), ALLOCATABLE :: THERMAL_REACTIONS   ( : )  
         

c..Miscellaneous variables
         INTEGER, PARAMETER :: NCS  = 1        ! no. of chemical mechanisms
         INTEGER, PARAMETER :: NCS2 = 2 * NCS  ! accounts for day/night 


c..Sparse Matrix maximum dimensions
         INTEGER, SAVE :: MAXGL3    ! Max # of P/L terms per species
         INTEGER, SAVE :: MXARRAY   ! Max # of terms in I-hJ matrix

c..Mechanism specific variables
         INTEGER, SAVE :: N_SPEC               ! No. of species in mech
         INTEGER, SAVE :: NRXNS               ! No. of reactions in mech

         INTEGER, SAVE :: MXCOUNT1, MXCOUNT2   ! Sparse matrx pntr dimensions
         INTEGER, SAVE :: MXRR, MXRP           ! Max # of PD terms
         INTEGER, SAVE :: MXRCT                ! max no. of reactants


c..Sparse Matrix variables 
         INTEGER, SAVE :: ISCHAN          ! No. of reacting species in current mech
         INTEGER, SAVE :: ISCHANG( NCS  ) ! No. of reacting species in day & nite
         INTEGER, SAVE :: NUSERAT( NCS2 ) ! No. of active rxns in day & nite
         INTEGER, SAVE :: IARRAY(  NCS2 ) ! No. of PD terms in I-hJ matrix

C Most of the following are allocated
         INTEGER, ALLOCATABLE, SAVE :: NKUSERAT( :,: )     ! Rxn nos of active rxns
         INTEGER, ALLOCATABLE, SAVE :: NET_EFFECT( :, : )  ! reaction's net effect on species
         INTEGER, ALLOCATABLE, SAVE :: IRM2  ( :,: )       ! Species rxn array
         INTEGER, ALLOCATABLE, SAVE :: IRM2SP( :,: )       ! Species indices for special rate expressions
         INTEGER, ALLOCATABLE, SAVE :: ICOEFF( :,:,: )     ! stoich coeff indx
         
         INTEGER, ALLOCATABLE, SAVE :: JARRAYPT( :,:,: )   ! A-Matrix index
         INTEGER, ALLOCATABLE, SAVE :: JARRL( :,:,: )      ! Pntr to PD Loss term
         INTEGER, ALLOCATABLE, SAVE :: JARRP( :,:,: )      ! Pntr to PD Prod term
         INTEGER, ALLOCATABLE, SAVE :: JLIAL( :,:,: )      ! Spec # for PD loss term
         INTEGER, ALLOCATABLE, SAVE :: JPIAL( :,:,: )      ! Spec # for PD prod term 
        
         INTEGER, ALLOCATABLE, SAVE :: INEW2OLD( : )     ! Spec index xref
         INTEGER, ALLOCATABLE, SAVE :: IOLD2NEW( : )     ! Spec index xref

         INTEGER, ALLOCATABLE, SAVE :: NDERIVL( :,: )      ! # of PD loss terms
         INTEGER, ALLOCATABLE, SAVE :: NDERIVP( :,: )      ! # of PD prod terms

C descirbes the partial derivatives in each sparse Jacobian component

         INTEGER, ALLOCATABLE,   SAVE :: NDERIVN1( :, : )      ! # PD with a coefficient of -1
         INTEGER, ALLOCATABLE,   SAVE :: NDERIVP1( :, : )      ! # PD with a coefficient of  1 
         INTEGER, ALLOCATABLE,   SAVE :: NDERIVCO( :, : )      ! # PD with other coefficients
         INTEGER, ALLOCATABLE,   SAVE :: PDERIVN1( :, :, : )   ! PD index with a coefficient of -1
         INTEGER, ALLOCATABLE,   SAVE :: PDERIVP1( :, :, : )   ! PD index with a coefficient of  1 
         INTEGER, ALLOCATABLE,   SAVE :: PDERIVCO( :, :, : )   ! PD index with other coefficients
         REAL( 8 ), ALLOCATABLE, SAVE :: PD_COEFF( :, :, : )   ! PD coefficients
 

c..indices for decomposition
         INTEGER, ALLOCATABLE, SAVE :: JZLO( : )           ! # of ops in decmp loop 1
         INTEGER, ALLOCATABLE, SAVE :: IDEC1LO( :,: )      ! decomp loop 1 bound
         INTEGER, ALLOCATABLE, SAVE :: IDEC1HI( :,: )      ! decomp loop 1 bound
         
         INTEGER, ALLOCATABLE, SAVE :: IJDECA( : ) ! Pntr for ij term 1 in decomp loop 1
         INTEGER, ALLOCATABLE, SAVE :: IJDECB( : ) ! Pntr for ij term 2 in decomp loop 1
         INTEGER, ALLOCATABLE, SAVE :: IKDECA( : ) ! Pntr for ik term 1 in decomp loop 1
         INTEGER, ALLOCATABLE, SAVE :: IKDECB( : ) ! Pntr for ik term 2 in decomp loop 1
         INTEGER, ALLOCATABLE, SAVE :: KJDECA( : ) ! Pntr for kj term 1 in decomp loop 1
         INTEGER, ALLOCATABLE, SAVE :: KJDECB( : ) ! Pntr for kj term 2 in decomp loop 1
         INTEGER, ALLOCATABLE, SAVE :: JZEROA( : ) ! Pntr for j term 1 in decomp loop 2
         INTEGER, ALLOCATABLE, SAVE :: JZEROB( : ) ! Pntr for j term 2 in decomp loop 2

         INTEGER, ALLOCATABLE, SAVE :: JHIZ1( :,: )  ! # of 2-term groups in dcmp loop 2
         INTEGER, ALLOCATABLE, SAVE :: JHIZ2( :,: )  ! # of 1-term groups in dcmp loop 2

         
         INTEGER, ALLOCATABLE, SAVE :: KZLO1( :,: )  ! Start indx for 2-term bksb loop 1
         INTEGER, ALLOCATABLE, SAVE :: KZLO2( :,: )  ! Start indx for 1-term bksb loop 1
         INTEGER, ALLOCATABLE, SAVE :: KZHI0( :,: )  ! End index for 5-term bksub loop 1
         INTEGER, ALLOCATABLE, SAVE :: KZHI1( :,: )  ! End index for 2-term bksub loop 1
         INTEGER, ALLOCATABLE, SAVE :: KZERO( :,: )  ! Pointer to bksub j index
         
         INTEGER, ALLOCATABLE, SAVE :: MZHI0 ( :,: ) ! End index for 5-term bksub loop 2
         INTEGER, ALLOCATABLE, SAVE :: MZHI1 ( :,: ) ! End index for 2-term bksub loop 2
         INTEGER, ALLOCATABLE, SAVE :: MZILCH( :,: ) ! # of calcs in bksub loop 2 (U)
         INTEGER, ALLOCATABLE, SAVE :: MZLO1 ( :,: ) ! Start indx for 2-term bksb loop 2
         INTEGER, ALLOCATABLE, SAVE :: MZLO2 ( :,: ) ! Start indx for 1-term bksb loop 2
         INTEGER, ALLOCATABLE, SAVE :: KZILCH( :,: ) ! # of calcs in bksub loop 1 (L)

         LOGICAL, SAVE  :: LREORDER = .TRUE.             ! Flag to reorder or not


         CONTAINS
         
         SUBROUTINE INIT_MECH_DATA()
!   Function initialize module variables         
           IMPLICIT NONE
         
         
           INTEGER    :: ISPC, IRX    ! loop counters

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C Initialize mechanism array variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            CONST = 0.0D0

            DO 101 IRX = 1, MAXRXNUM
               DO ISPC = 1, MAXPRODS+3
                  IRR( IRX,ISPC ) = 0
               END DO
               DO ISPC = 1, MAXPRODS
                  SC( IRX,ISPC ) = 0.0
               END DO
               DO ISPC = 1, 3
                   RTDAT( ISPC,IRX ) = 0.0D0
               END DO
               KTYPE( IRX ) = 0
               IORDER( IRX )  = 0
               IRXBITS( IRX ) = 0
               KRX1( IRX ) = 0
               KRX2( IRX ) = 0
               KRX3( IRX ) = 0
               KRX4( IRX ) = 0
               KRX5( IRX ) = 0
               KRX6( IRX ) = 0
               KRX7( IRX ) = 0
101         CONTINUE
            HAL_PHOTAB = 0
            NFALLOFF   = 0

            DO 103 IRX = 1, MAXFALLOFF
               IRRFALL( IRX ) = 0   
               DO ISPC = 1, 5
                  RFDAT( ISPC,IRX ) = 0.0D0
               END DO
103         CONTINUE

            DO 105 IRX = 1, MAX3BODIES
               NRXWM( IRX )   = 0
               NRXWW( IRX )   = 0
               NRXWO2( IRX )  = 0
               NRXWN2( IRX )  = 0
               NRXWCH4( IRX ) = 0
               NRXWH2( IRX )  = 0
105         CONTINUE

            KTN1 = 0
            KTN2 = 0
            KTN3 = 0
            KTN4 = 0
            KTN5 = 0
            KTN6 = 0
            KTN7 = 0
!           KCNV = 0
            NWM  = 0
            NWW  = 0
            NWO2 = 0
            NWN2 = 0
            NWCH4 = 0
            NWH2 = 0

            NPHOTAB = 0
            NMPHOT  = 0
            NSUNLIGHT = 0
            NTHERMAL  = 0
            DO ISPC = 1, MAXPHOTRXNS
               IPH( ISPC,1 ) = 0
               IPH( ISPC,2 ) = 0
               IPH( ISPC,3 ) = 0
               PHOTAB( ISPC ) = ' '
            END DO
            
            NSPECIAL     = 0
            NSPECIAL_RXN = 0
            MSPECTERMS   = 1

            DO ISPC = 1, MAXSPECRXNS
               ISPECIAL( ISPC,1 ) = 0
               ISPECIAL( ISPC,2 ) = 0
               SPECIAL( ISPC )    = ' '
               NKC_TERMS( ISPC )  = 0
               KC_COEFFS( ISPC,  1:MAXSPECTERMS) = 0.0
               KC_TERMS(  ISPC,  1:MAXSPECTERMS, 1) = ' '
               KC_TERMS(  ISPC,  1:MAXSPECTERMS, 2) = ' '
               INDEX_KTERM(MAXSPECRXNS,  1:MAXSPECTERMS) = -1
               INDEX_CTERM(MAXSPECRXNS,  1:MAXSPECTERMS) = 0
               N_OPERATORS( ISPC )  = 0
               OPERATORS(   ISPC, 1:MAXSPECTERMS)  = 0
               OPERATOR_COEFFS( ISPC, 1:MAXSPECTERMS) = 0.0
            END DO
            
            IHETERO = 0
            NHETERO = 0
            HETERO  = '                '
            
            SS_RCT_COEF = 0                 ! Array initialization
            SS_PRD_COEF = 0.0               ! Array initialization
            SS_RCT_IND  = 0                 ! Array initialization
            MAX_SS_LOSS = 0
            MAX_SS_PROD = 0

            ALLOCATE( PHOTOLYSIS_REACTIONS( MAXPHOTRXNS ) )
            CALL INIT_REACTION_LIST( PHOTOLYSIS_REACTIONS )

            ALLOCATE( THERMAL_REACTIONS( ( MAXRXNUM - MAXPHOTRXNS ) ) )
            CALL INIT_REACTION_LIST( THERMAL_REACTIONS )

         RETURN
         END SUBROUTINE INIT_MECH_DATA
 
         SUBROUTINE INIT_REACTION_LIST( REACTION_LIST  )
           IMPLICIT NONE

           TYPE( REACTION ),  INTENT( INOUT ) :: REACTION_LIST( : )


           INTEGER :: IREACTION
 
           DO IREACTION = 1, SIZE( REACTION_LIST, 1 )
            REACTION_LIST( IREACTION )%LABEL( 1:2 )  = '>>>>>>>>>>>>>>>>'  ! name of reaction and if needed reference 
            REACTION_LIST( IREACTION )%IRXBITS    = 0           ! bit value for rate constant
            REACTION_LIST( IREACTION )%RATE_TYPE  = 0           ! type of rate constant
            REACTION_LIST( IREACTION )%NPRDCT     = 0           ! no. of products
            REACTION_LIST( IREACTION )%NREACT     = 0           ! no. of reactants
            REACTION_LIST( IREACTION )%ORDER      = 0           ! order of reaction
            REACTION_LIST( IREACTION )%IRR( 1:MAXPRODS+3 )    = 0   ! reactant and product species indices
            REACTION_LIST( IREACTION )%HETEO_INDEX( 1:2 )     = 0   ! mechanism reaction indices if heterogeneous type
            REACTION_LIST( IREACTION )%PHOTO_INDEX( 1:3 )     = 0   ! mechanism reaction indices if rate constant photolysis
            REACTION_LIST( IREACTION )%FALLOFF_INDEX          = 0   ! mechanism reaction indices if falloff type
            REACTION_LIST( IREACTION )%SPECIAL_INDEX( 1:2 )   = 0   ! mechanism reaction indices if rate constant a special expression
            REACTION_LIST( IREACTION )%SC( 1:MAXPRODS ) = 0.0D0   ! product stiochometric coefficients
            REACTION_LIST( IREACTION )%RTDAT( 1:3 )     = 0.0D0   ! general data for rate constant
            REACTION_LIST( IREACTION )%RFDAT( 1:5 )     = 0.0D0   ! data of fall rate constant type
            REACTION_LIST( IREACTION )%NAIR_RCTNTS    = 0       ! # times M or air a reactant
            REACTION_LIST( IREACTION )%NH2O_RCTNTS    = 0       ! # times water a reactant
            REACTION_LIST( IREACTION )%N_O2_RCTNTS    = 0       ! # times O2 a reactant
            REACTION_LIST( IREACTION )%N_N2_RCTNTS    = 0       ! # times H2 a reactant
            REACTION_LIST( IREACTION )%N_H2_RCTNTS    = 0       ! # times N2 a reactant
            REACTION_LIST( IREACTION )%NCH4_RCTNTS    = 0       ! # times methane a reactant
           END DO
           
         END SUBROUTINE INIT_REACTION_LIST
         SUBROUTINE SORT_REACTION_LIST( OFFSET ,NREACTIONS, REACTION_LIST )
! routine sorts the reactant based on the lowest number of reactants
             IMPLICIT NONE
             INTEGER,           INTENT( IN    ) :: OFFSET             ! in master list, #reactions before REACTION_LIST 
             INTEGER,           INTENT( IN    ) :: NREACTIONS         ! number of reactions in list
             TYPE( REACTION ),  INTENT( INOUT ) :: REACTION_LIST( : ) ! data for individual reactions
!            INTEGER,           INTENT( INOUT ) :: INEW_BUBBLE  ( : )
           
             TYPE( REACTION ) :: SWAPVALUE
             INTEGER          :: I, J
             INTEGER          :: INEW
             LOGICAL          :: SWAPPED
             
             INTEGER, ALLOCATABLE :: INEW_BUBBLE  ( : )

             ALLOCATE( INEW_BUBBLE ( NREACTIONS ) )
             INEW_BUBBLE = (/ (I, I = 1, NREACTIONS) /)
                 
             DO J =  (NREACTIONS-1), 1, -1
                SWAPPED = .FALSE.
                DO I = 1, J
                  IF ( REACTION_LIST( I )%NREACT .GT. REACTION_LIST( I+1 )%NREACT ) THEN
                    SWAPVALUE          = REACTION_LIST(I)
                    REACTION_LIST(I)   = REACTION_LIST(I+1)
                    REACTION_LIST(I+1) = SWAPVALUE
                    INEW               = INEW_BUBBLE( I )
                    INEW_BUBBLE( I )     = INEW_BUBBLE( I + 1 )
                    INEW_BUBBLE( I + 1 ) = INEW
                    SWAPPED = .TRUE.
                  END IF
                END DO
                IF (.NOT. SWAPPED) EXIT
             END DO

!            WRITE(6,'(A)')'Results from sorting REACTION_LIST by number of reactants '
             WRITE(6,99815)
             DO J = 1, NREACTIONS
                WRITE(6,99816)OFFSET+J,REACTION_LIST( J )%LABEL( 1 ),REACTION_LIST( J )%NREACT,
     &          OFFSET+INEW_BUBBLE( J )
             END DO

             DEALLOCATE( INEW_BUBBLE )
99815        FORMAT("Results from sorting REACTION_LIST by number of reactants"
     &              / "INDEX",6X,"LABEL",7X,"NREACT",1X,"OLD INDEX")
99816        FORMAT(1X,I4,1X,A16,3X,I1,3X,I4)
         END SUBROUTINE SORT_REACTION_LIST 
         SUBROUTINE REV_SORT_REACTION_LIST( OFFSET ,NREACTIONS, REACTION_LIST )
! routine sorts the reactant based on the highest number of reactants
             IMPLICIT NONE
             INTEGER,           INTENT( IN    ) :: OFFSET             ! in master list, #reactions before REACTION_LIST 
             INTEGER,           INTENT( IN    ) :: NREACTIONS         ! number of reactions in list
             TYPE( REACTION ),  INTENT( INOUT ) :: REACTION_LIST( : ) ! data for individual reactions
!            INTEGER,           INTENT( INOUT ) :: INEW_BUBBLE  ( : )
           
             TYPE( REACTION ) :: SWAPVALUE
             INTEGER          :: I, J
             INTEGER          :: INEW
             LOGICAL          :: SWAPPED
             
             INTEGER, ALLOCATABLE :: INEW_BUBBLE  ( : )

             ALLOCATE( INEW_BUBBLE ( NREACTIONS ) )
             INEW_BUBBLE = (/ (I, I = 1, NREACTIONS) /)
                 
             DO J =  (NREACTIONS-1), 1, -1
                SWAPPED = .FALSE.
                DO I = 1, J
                  IF ( REACTION_LIST( I )%NREACT .LT. REACTION_LIST( I+1 )%NREACT ) THEN
                    SWAPVALUE          = REACTION_LIST(I)
                    REACTION_LIST(I)   = REACTION_LIST(I+1)
                    REACTION_LIST(I+1) = SWAPVALUE
                    INEW               = INEW_BUBBLE( I )
                    INEW_BUBBLE( I )     = INEW_BUBBLE( I + 1 )
                    INEW_BUBBLE( I + 1 ) = INEW
                    SWAPPED = .TRUE.
                  END IF
                END DO
                IF (.NOT. SWAPPED) EXIT
             END DO

!            WRITE(6,'(A)')'Results from sorting REACTION_LIST by number of reactants '
             WRITE(6,99815)
             DO J = 1, NREACTIONS
                WRITE(6,99816)OFFSET+J,REACTION_LIST( J )%LABEL( 1 ),REACTION_LIST( J )%NREACT,
     &          OFFSET+INEW_BUBBLE( J )
             END DO

             DEALLOCATE( INEW_BUBBLE )
99815        FORMAT("Results from reverse sorting REACTION_LIST by number of reactants"
     &              / "INDEX",6X,"LABEL",7X,"NREACT",1X,"OLD INDEX")
99816        FORMAT(1X,I4,1X,A16,3X,I1,3X,I4)
         END SUBROUTINE REV_SORT_REACTION_LIST 
         SUBROUTINE LOAD_REACTION_LIST( IREACTION, JREACTION, LABELS, REACTION_LIST  )
           IMPLICIT NONE
           INTEGER, INTENT( IN )              :: IREACTION
           INTEGER, INTENT( IN )              :: JREACTION
           CHARACTER(LEN=16), INTENT( IN )    :: LABELS( :,: )
           TYPE( REACTION ),  INTENT( INOUT ) :: REACTION_LIST( : )

           INTEGER :: I
           IF( SIZE( REACTION_LIST, 1 ) .LT. IREACTION )THEN
               WRITE( 6, * )'In LOAD_REACTION_LIST: array index exceeds REACTION_LIST size'
               WRITE( 6, '(A,I4)' )'IREACTION = ',IREACTION
               WRITE( 6, '(A,I4)' )'REACTION_LIST size = ',SIZE( REACTION_LIST, 1 )
               STOP
           END IF
! set an element if a reaction list
              REACTION_LIST( IREACTION )%LABEL(1)   = LABELS( JREACTION,1 )
              REACTION_LIST( IREACTION )%LABEL(2)   = LABELS( JREACTION,2 )
              REACTION_LIST( IREACTION )%IRXBITS    = IRXBITS( JREACTION )
              REACTION_LIST( IREACTION )%RATE_TYPE  = KTYPE( JREACTION )
              REACTION_LIST( IREACTION )%NPRDCT     = NPRDCT( JREACTION )
              REACTION_LIST( IREACTION )%NREACT     = NREACT( JREACTION )
              REACTION_LIST( IREACTION )%ORDER      = IORDER( JREACTION )
              REACTION_LIST( IREACTION )%RTDAT(1:3) = RTDAT( 1:3,JREACTION )        
!              WRITE(6,'(A,7(ES12.4,1X))')'REACTION_LIST( IREACTION )%RTDAT(1:3),RTDAT( 1:3,JREACTION )',
!     &          REACTION_LIST( IREACTION )%RTDAT(1:3), RTDAT( 1:3,JREACTION )
              REACTION_LIST( IREACTION )%IRR(1:MAXPRODS+3) = IRR( JREACTION,1:MAXPRODS+3 )        
              REACTION_LIST( IREACTION )%SC(1:MAXPRODS)    = SC( JREACTION, 1:MAXPRODS )
              IF( KTYPE( JREACTION ) .EQ. 11 )THEN
                 REACTION_LIST( IREACTION )%SPECIAL_INDEX(1) = IREACTION
                 REACTION_LIST( IREACTION )%SPECIAL_INDEX(2) = ISPECIAL( NSPECIAL_RXN,2 )
              END IF
              IF( KTYPE( JREACTION ) .GT. 7 .AND. KTYPE( JREACTION ) .LE. 10 )THEN
                 REACTION_LIST( IREACTION )%FALLOFF_INDEX = IREACTION
                 REACTION_LIST( IREACTION )%RFDAT(1:5)    = RFDAT( 1:5,NFALLOFF )        
              END IF
              IF( KTYPE( JREACTION ) .EQ. 12 )THEN
                 REACTION_LIST( IREACTION )%FALLOFF_INDEX = IREACTION
                 REACTION_LIST( IREACTION )%RFDAT(1:5)    = RFDAT(1:5,NFALLOFF)        
              END IF
              IF( KTYPE( JREACTION ) .EQ. 0 )THEN
                  REACTION_LIST( IREACTION )%PHOTO_INDEX(1) = IREACTION
                  REACTION_LIST( IREACTION )%PHOTO_INDEX(2) = IPH(NMPHOT,2)
                  REACTION_LIST( IREACTION )%PHOTO_INDEX(3) = IPH(NMPHOT,3)
              END IF
              IF( KTYPE( JREACTION ) .EQ. -1 )THEN
                  REACTION_LIST( IREACTION )%HETEO_INDEX(1) = IREACTION
                  REACTION_LIST( IREACTION )%HETEO_INDEX(2) = IHETERO(MHETERO,2)
              END IF
              DO I = 1, MAX3BODIES
!                  IF( NRXWM( I ) .GT. 0 )print*,'I, NRXWM( I ),JREACTION= ',I, NRXWM( I ),JREACTION
                 IF( NRXWM( I ) .EQ. JREACTION  )THEN
               WRITE(6,'(A,3(I4,1X))')'NRXWM( I ), JREACTION,REACTION_LIST( IREACTION )%NAIR_RCTNTS = ', NRXWM( I ), JREACTION, 
     &             REACTION_LIST( IREACTION )%NAIR_RCTNTS
                   REACTION_LIST( IREACTION )%NAIR_RCTNTS = REACTION_LIST( IREACTION )%NAIR_RCTNTS  + 1
               WRITE(6,'(A,3(I4,1X))')'NRXWM( I ), JREACTION,REACTION_LIST( IREACTION )%NAIR_RCTNTS = ', NRXWM( I ), JREACTION, 
     &             REACTION_LIST( IREACTION )%NAIR_RCTNTS
                 END IF
                 IF( NRXWW( I ) .EQ. JREACTION  )THEN
                   REACTION_LIST( IREACTION )%NH2O_RCTNTS = REACTION_LIST( IREACTION )%NH2O_RCTNTS  + 1
            WRITE(6,'(A,3(I4,1X))')'NRXWW( I ), JREACTION, REACTION_LIST( IREACTION )%NH2O_RCTNTS = ', NRXWw( I ), JREACTION,
     &             REACTION_LIST( IREACTION )%NH2O_RCTNTS
                 END IF
                 IF( NRXWO2( I ) .EQ. JREACTION  )THEN
                   REACTION_LIST( IREACTION )%N_O2_RCTNTS = REACTION_LIST( IREACTION )%N_O2_RCTNTS  + 1
              WRITE(6,'(A,3(I4,1X))')'NRXWO2( I ), JREACTION, REACTION_LIST( IREACTION )%N_O2_RCTNTS = ', NRXWO2( I ), JREACTION,
     &             REACTION_LIST( IREACTION )%N_O2_RCTNTS
                 END IF
                 IF( NRXWN2( I ) .EQ. JREACTION  )THEN
                   REACTION_LIST( IREACTION )%N_N2_RCTNTS = REACTION_LIST( IREACTION )%N_N2_RCTNTS  + 1
              WRITE(6,'(A,3(I4,1X))')'NRXWN2( I ), JREACTION, REACTION_LIST( IREACTION )%N_N2_RCTNTS = ', NRXWN2( I ), JREACTION,
     &              REACTION_LIST( IREACTION )%N_N2_RCTNTS
                 END IF
                 IF( NRXWH2( I ) .EQ. JREACTION  )THEN
                   REACTION_LIST( IREACTION )%N_H2_RCTNTS = REACTION_LIST( IREACTION )%N_H2_RCTNTS  + 1
              WRITE(6,'(A,3(I4,1X))')'NRXWH2( I ), JREACTION, REACTION_LIST( IREACTION )%N_H2_RCTNTS = ', NRXWH2( I ), JREACTION,
     &              REACTION_LIST( IREACTION )%N_H2_RCTNTS
                 END IF
                 IF( NRXWCH4( I ) .EQ. JREACTION  )THEN
                   REACTION_LIST( IREACTION )%NCH4_RCTNTS = REACTION_LIST( IREACTION )%NCH4_RCTNTS  + 1
           WRITE(6,'(A,3(I4,1X))')'NRXWCH4( I ), JREACTION, REACTION_LIST( IREACTION )%NCH4_RCTNTS = ', NRXWCH4( I ), JREACTION,
     &             REACTION_LIST( IREACTION )%NCH4_RCTNTS
                 END IF
              END DO
!              REACTION_LIST( IREACTION )%NH2O_RCTNTS = COUNT( NRXWW .EQ. JREACTION  )
!              REACTION_LIST( IREACTION )%N_O2_RCTNTS = COUNT( NRXWO2 .EQ. JREACTION  )
!              REACTION_LIST( IREACTION )%N_N2_RCTNTS = COUNT( NRXWN2 .EQ. JREACTION  )
!              REACTION_LIST( IREACTION )%N_H2_RCTNTS = COUNT( NRXWH2 .EQ. JREACTION  )
!              REACTION_LIST( IREACTION )%NCH4_RCTNTS = COUNT( NRXWCH4 .EQ. JREACTION  )
!              print*,"IREACTION, REACTION_LIST( IREACTION )%LABEL = ", IREACTION, REACTION_LIST( IREACTION )%LABEL(1),
!     &        REACTION_LIST( IREACTION )%RATE_TYPE 
         END SUBROUTINE LOAD_REACTION_LIST
         SUBROUTINE PUT_PHOTRXNS_ONTOP( LABELS )
            USE KPP_DATA
            IMPLICIT NONE
            
            CHARACTER(LEN=16), INTENT( INOUT ) :: LABELS( :,: )

            INTEGER :: I, J, K
            INTEGER :: IFALLOFF
            INTEGER :: IPHOT
            INTEGER :: IHET
            INTEGER :: JSPECIAL
            INTEGER :: N_AIR_3BODY
            INTEGER :: N_H2O_3BODY
            INTEGER :: N_N2_3BODY
            INTEGER :: N_O2_3BODY
            INTEGER :: N_CH4_REACTION
            INTEGER :: N_H2_REACTION
            INTEGER :: FIXED_SPC_COUNT

            IFALLOFF = 0
            IPHOT    = 0
            IHET     = 0
            JSPECIAL = 0
            N_AIR_3BODY    = 0
            N_H2O_3BODY    = 0
            N_N2_3BODY     = 0  
            N_O2_3BODY     = 0
            N_CH4_REACTION = 0
            N_H2_REACTION  = 0
! reset varaible of KPP_DATA
            INDEX_FIXED_SPECIES = 0
            DO I = 1, NSUNLIGHT

               IF( PHOTOLYSIS_REACTIONS( I )%NREACT .EQ. 1 )THEN
                   ONE_REACT_SUNLIGHT = ONE_REACT_SUNLIGHT + 1
               ELSE IF( PHOTOLYSIS_REACTIONS( I )%NREACT .EQ. 0 )THEN
                   ZERO_REACT_SUNLIGHT = ZERO_REACT_SUNLIGHT + 1
               END IF
!redefine first part of total mechanism data                  
               FIXED_SPC_COUNT       = 0
               LABELS( I,1 )         = PHOTOLYSIS_REACTIONS( I )%LABEL(1)                     
               LABELS( I,2 )         = PHOTOLYSIS_REACTIONS( I )%LABEL(2)                     
               IRXBITS( I )          = PHOTOLYSIS_REACTIONS( I )%IRXBITS            
               KTYPE( I )            = PHOTOLYSIS_REACTIONS( I )%RATE_TYPE          
               NPRDCT( I )           = PHOTOLYSIS_REACTIONS( I )%NPRDCT             
               NREACT( I )           = PHOTOLYSIS_REACTIONS( I )%NREACT             
               IORDER( I )           = PHOTOLYSIS_REACTIONS( I )%ORDER             
               RTDAT( 1:3,I )        = PHOTOLYSIS_REACTIONS( I )%RTDAT(1:3)         
               IRR( I,1:MAXPRODS+3 ) = PHOTOLYSIS_REACTIONS( I )%IRR(1:MAXPRODS+3)  
               SC( I, 1:MAXPRODS )   = PHOTOLYSIS_REACTIONS( I )%SC(1:MAXPRODS)     
               IF( KTYPE( I ) .EQ. 12 .OR. ( KTYPE( I ) .GT. 7 .AND. KTYPE( I ) .LT. 11  ) )THEN
                 IFALLOFF = IFALLOFF + 1
                 IRRFALL( IFALLOFF )   = I ! PHOTOLYSIS_REACTIONS( I )%FALLOFF_INDEX
                 RFDAT( 1:5,IFALLOFF ) = PHOTOLYSIS_REACTIONS( I )%RFDAT(1:5)
               WRITE(6,'(A,I4,1X,(A16,1X),(I2,1X),5(ES12.4,1X))')"PHOTOLYSIS: I, LABELS( I, 1 ), IRRFALL = ", I, 
     &         LABELS( I, 1 ),IRRFALL( IFALLOFF ),RFDAT( 1:5,IFALLOFF )
               END IF
               IF( KTYPE( I ) .EQ. 0 )THEN
                 IPHOT = IPHOT + 1
                 IPH( IPHOT,1 ) = I ! PHOTOLYSIS_REACTIONS( I )%PHOTO_INDEX(1:3)
                 IPH( IPHOT,2 ) = PHOTOLYSIS_REACTIONS( I )%PHOTO_INDEX(2)
                 IPH( IPHOT,3 ) = PHOTOLYSIS_REACTIONS( I )%PHOTO_INDEX(3)
               WRITE(6,'(A,I4,1X,(A16,1X),(I3,1X),A16,1X,I3)')"PHOTOLYSIS: I, LABELS( I, 1 ), IPH = ", I, 
     &         LABELS( I, 1 ),IPH( IPHOT,1 ),PHOTAB(IPH( IPHOT,2 )),IPH( IPHOT,3 )
               END IF
               IF( KTYPE( I ) .EQ. 11 )THEN
                 JSPECIAL = JSPECIAL + 1
                 ISPECIAL( JSPECIAL,1 ) = I ! PHOTOLYSIS_REACTIONS( I )%SPECIAL_INDEX(1:2)
                 ISPECIAL( JSPECIAL,2 ) = PHOTOLYSIS_REACTIONS( I )%SPECIAL_INDEX(2)
               END IF
               IF( KTYPE( I ) .EQ. -1 )THEN
                 IHET = IHET + 1
                 IHETERO(IHET,1) = I ! PHOTOLYSIS_REACTIONS( I )%HETEO_INDEX(1:2)
                 IHETERO(IHET,2) = PHOTOLYSIS_REACTIONS( I )%HETEO_INDEX(2)
               END IF
!reset third body, CH4 and H2 reaction pointers
               DO K = 1, PHOTOLYSIS_REACTIONS( I )%NAIR_RCTNTS
                  N_AIR_3BODY = N_AIR_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWM(N_AIR_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 1
                  print*,'I, N_AIR_3BODY, NRXWM(N_AIR_3BODY) = ',I, N_AIR_3BODY, NRXWM(N_AIR_3BODY)
               END DO
               DO K = 1, PHOTOLYSIS_REACTIONS( I )%NH2O_RCTNTS
                  N_H2O_3BODY = N_H2O_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWW(N_AIR_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 2
               END DO
               DO K = 1, PHOTOLYSIS_REACTIONS( I )%N_O2_RCTNTS
                  N_O2_3BODY = N_O2_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWO2(N_O2_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 3
               END DO
               DO K = 1, PHOTOLYSIS_REACTIONS( I )%N_N2_RCTNTS
                  N_N2_3BODY = N_N2_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWN2(N_N2_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 4
               END DO
               DO K = 1, PHOTOLYSIS_REACTIONS( I )%N_H2_RCTNTS
                  N_H2_REACTION = N_H2_REACTION  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWH2(N_H2_REACTION) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 6
               END DO
               DO K = 1, PHOTOLYSIS_REACTIONS( I )%NCH4_RCTNTS
                  N_CH4_REACTION = N_CH4_REACTION  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWH2(N_CH4_REACTION) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 5
               END DO
               WRITE(6,'(A,I4,1X,(A16,1X),2(I2,1X))')"PHOTOLYSIS: I, LABELS( I, 1 ), KTYPE( I ), IORDER = ", I, 
     &         LABELS( I, 1 ),KTYPE( I ),IORDER( I )
               IF ( FIXED_SPC_COUNT .GT. 3 ) THEN
                 WRITE( *,* )'Number of Constant Species Exceeds Three for Reaction:', LABELS( I,1 )
                 STOP
               END IF
            END DO
            IF( NSUNLIGHT .NE. ONE_REACT_SUNLIGHT )THEN
               WRITE( 6,'(a)')"FATAL ERROR"
               WRITE( 6,'(a)')"Below sunlight dependent reactions not have only one reactant"
               DO I = 1, NSUNLIGHT
                  IF( PHOTOLYSIS_REACTIONS( I )%NREACT .EQ. 1 )CYCLE
                  WRITE(6,'(8X,A16)')PHOTOLYSIS_REACTIONS( I )%LABEL( 1 )
               END DO
               STOP
            END IF
!redefine second part of total mechanism data                  
            I = NSUNLIGHT
            DO J = 1, NTHERMAL
               I = I + 1
               FIXED_SPC_COUNT       = 0
               LABELS( I,1 )         = THERMAL_REACTIONS( J )%LABEL(1)                     
               LABELS( I,2 )         = THERMAL_REACTIONS( J )%LABEL(2)                     
               IRXBITS( I )          = THERMAL_REACTIONS( J )%IRXBITS            
               KTYPE( I )            = THERMAL_REACTIONS( J )%RATE_TYPE          
               NPRDCT( I )           = THERMAL_REACTIONS( J )%NPRDCT             
               NREACT( I )           = THERMAL_REACTIONS( J )%NREACT
               SELECT CASE (THERMAL_REACTIONS( J )%NREACT )
                 CASE( 0 )
                   ZERO_REACT_THERMAL = ZERO_REACT_THERMAL + 1
                 CASE( 1 )
                   ONE_REACT_THERMAL  = ONE_REACT_THERMAL + 1
                 CASE( 2 )
                   TWO_REACT_THERMAL  = TWO_REACT_THERMAL + 1
                 CASE( 3 )
                   THREE_REACT_THERMAL = THREE_REACT_THERMAL + 1
               END SELECT
               IORDER( I )           = THERMAL_REACTIONS( J )%ORDER             
               RTDAT( 1:3,I )        = THERMAL_REACTIONS( J )%RTDAT(1:3)         
!              WRITE(6,'(A,7(ES12.4,1X))')'THERMAL_REACTIONS( J )%RTDAT(1:3),RTDAT( 1:3,I )',
!     &          THERMAL_REACTIONS( J )%RTDAT(1:3), RTDAT( 1:3,I)
               IRR( I,1:MAXPRODS+3 ) = THERMAL_REACTIONS( J )%IRR(1:MAXPRODS+3)  
               SC( I, 1:MAXPRODS )   = THERMAL_REACTIONS( J )%SC(1:MAXPRODS)
               IF( KTYPE( I ) .EQ. 12 .OR. ( KTYPE( I ) .GT. 7 .AND. KTYPE( I ) .LT. 11 ) )THEN
                 IFALLOFF = IFALLOFF + 1
                 IRRFALL( IFALLOFF )   = I ! THERMAL_REACTIONS( J )%FALLOFF_INDEX
                 DO K = 1, 5
                    RFDAT( K,IFALLOFF ) = THERMAL_REACTIONS( J )%RFDAT(K)
                 END DO
              WRITE(6,'(2(I4,1x),2A,7(ES12.4,1X))')IFALLOFF, I, 'FALLOFF RXN:' // TRIM(LABELS( I,1 )), 
     &           ': THERMAL_REACTIONS( J )%RFDAT(1:3),RFDAT( 1:5,I )',
     &          THERMAL_REACTIONS( J )%RFDAT(1:5), RFDAT( 1:5,IFALLOFF)
               END IF
               IF( KTYPE( I ) .EQ. 0 )THEN
                 IPHOT = IPHOT + 1
                 IPH( IPHOT,1 ) = I ! THERMAL_REACTIONS( J )%PHOTO_INDEX(1:3)
                 IPH( IPHOT,2 ) = THERMAL_REACTIONS( J )%PHOTO_INDEX(2)
                 IPH( IPHOT,3 ) = THERMAL_REACTIONS( J )%PHOTO_INDEX(3)
               END IF
               IF( KTYPE( I ) .EQ. 11 )THEN
                 JSPECIAL = JSPECIAL + 1
                 ISPECIAL( JSPECIAL,1 ) = I ! THERMAL_REACTIONS( J )%SPECIAL_INDEX(1:2)
                 ISPECIAL( JSPECIAL,2 ) = THERMAL_REACTIONS( J )%SPECIAL_INDEX(2)
               WRITE(6,'(A,I4,1X,(A16,1X),2(I4,1X))')"THERMAL: I, LABELS( I, 1 ), ISPECIAL = ", I, 
     &         LABELS( I, 1 ),ISPECIAL( JSPECIAL,1 ),ISPECIAL( JSPECIAL,2 )
               END IF
               IF( KTYPE( I ) .EQ. -1 )THEN
                 IHET = IHET + 1
                 IHETERO(IHET,1) = I ! THERMAL_REACTIONS( J )%HETEO_INDEX(1:2)
                 IHETERO(IHET,2) = THERMAL_REACTIONS( J )%HETEO_INDEX(2)
               WRITE(6,'(A,I4,1X,(A16,1X),2(I4,1X))')"THERMAL: I, LABELS( I, 1 ), I = ", I, 
     &         LABELS( I, 1 ),IHETERO(IHET,1),IHETERO(IHET,2)
               END IF
!reset third body, CH4 and H2 reaction pointers
               DO K = 1, THERMAL_REACTIONS( J )%NAIR_RCTNTS
                  N_AIR_3BODY = N_AIR_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWM(N_AIR_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 1
         WRITE(6,'(A,4(I4,1X))')'I, N_AIR_3BODY, NRXWM(N_AIR_3BODY) = ',J,I, N_AIR_3BODY, NRXWM(N_AIR_3BODY)
               END DO
               DO K = 1, THERMAL_REACTIONS( J )%NH2O_RCTNTS
                  N_H2O_3BODY = N_H2O_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWW(N_H2O_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 2
         WRITE(6,'(A,4(I4,1X))')'J, I, N_H2O_3BODY, NRXWW(N_H2O_3BODY) = ',J, I, N_H2O_3BODY, NRXWW(N_H2O_3BODY)
               END DO
               DO K = 1, THERMAL_REACTIONS( J )%N_O2_RCTNTS
                  N_O2_3BODY = N_O2_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWO2(N_O2_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 3
               WRITE(6,'(A,4(I4,1X))')'I, N_O2_3BODY, NRXWO2(N_O2_3BODY) = ',J,I, N_O2_3BODY, NRXWO2(N_O2_3BODY)
               END DO
               DO K = 1, THERMAL_REACTIONS( J )%N_N2_RCTNTS
                  N_N2_3BODY = N_N2_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWN2(N_N2_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 4
               WRITE(6,'(A,4(I4,1X))')'I, N_N2_3BODY, NRXWN2(N_N2_3BODY) = ',J,I, N_N2_3BODY, NRXWN2(N_N2_3BODY)
               END DO
               DO K = 1, THERMAL_REACTIONS( J )%N_H2_RCTNTS
                  N_H2_REACTION = N_H2_REACTION  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWH2(N_H2_REACTION) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 6
               WRITE(6,'(A,4(I4,1X))')'I, N_H2_REACTION, NRXWH2(N_H2_REACTION) = ',J,I, N_H2_REACTION, NRXWH2(N_H2_REACTION)
               END DO
               DO K = 1, THERMAL_REACTIONS( J )%NCH4_RCTNTS
                  N_CH4_REACTION = N_CH4_REACTION  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWCH4(N_CH4_REACTION) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 5
           WRITE(6,'(A,4(I4,1X))')'I, N_CH4_REACTION, NRXWCH4(N_CH4_REACTION) = ',J,I, N_CH4_REACTION, NRXWCH4(N_CH4_REACTION)
               END DO
               IF ( FIXED_SPC_COUNT .GT. 3 ) THEN
                 WRITE( *,* )'Number of Constant Species Exceeds Three for Reaction:', LABELS( I,1 )
                 STOP
               END IF

               WRITE(6,'(A,I4,1X,(A16,1X),2(I2,1X))')"THERMAL: I, LABELS( I, 1 ), KTYPE( I ), IORDER = ", I, 
     &         LABELS( I, 1 ),KTYPE( I ),IORDER( I )               
            END DO
! rest number of third body, CH4 and H2 reactions
            NWM   = N_AIR_3BODY
            NWW   = N_H2O_3BODY
            NWO2  = N_O2_3BODY
            NWN2  = N_N2_3BODY
            NWH2  = N_H2_REACTION
            NWCH4 = N_CH4_REACTION
!set total count of reactant per reaction
            ONE_REACT_REACTIONS   = ONE_REACT_SUNLIGHT + ONE_REACT_THERMAL
            ZERO_REACT_REACTIONS  = ZERO_REACT_THERMAL 
            TWO_REACT_REACTIONS   = TWO_REACT_THERMAL 
            THREE_REACT_REACTIONS = THREE_REACT_THERMAL 
! update labels
            DO J = 1, (NMPHOT + NTHERMAL)
               RXLABEL( J ) = LABELS( J,1 )
               WRITE(6,'(A,I4,1X,3(A16,1X),I2)')"MECHANISM: J, LABELS(J,1:2),RXLABEL( J ), KTYPE( J ) = ", J,
     &         LABELS( J,1:2), RXLABEL( J ), KTYPE( J )
            END DO
         END SUBROUTINE PUT_PHOTRXNS_ONTOP
         SUBROUTINE REPLACE_REACTIONS( REACTION_LIST, NREACTIONS, ISTART, LABELS )
            USE KPP_DATA
            IMPLICIT NONE
            
           INTEGER, INTENT( IN )              :: ISTART
           INTEGER, INTENT( IN )              :: NREACTIONS
           TYPE( REACTION ),  INTENT( INOUT ) :: REACTION_LIST( : )
           CHARACTER(LEN=16), INTENT( INOUT ) :: LABELS( :,: )

            INTEGER       :: I, J, K

            INTEGER, SAVE :: IFALLOFF
            INTEGER, SAVE :: IPHOT
            INTEGER, SAVE :: IHET
            INTEGER, SAVE :: JSPECIAL
            INTEGER, SAVE :: N_AIR_3BODY
            INTEGER, SAVE :: N_H2O_3BODY
            INTEGER, SAVE :: N_N2_3BODY
            INTEGER, SAVE :: N_O2_3BODY
            INTEGER, SAVE :: N_CH4_REACTION
            INTEGER, SAVE :: N_H2_REACTION
            INTEGER, SAVE :: FIXED_SPC_COUNT
            LOGICAL, SAVE :: FIRST_CALL = .TRUE.
            
            IF( FIRST_CALL )THEN
              IFALLOFF = 0
              IPHOT    = 0
              IHET     = 0
              JSPECIAL = 0
              N_AIR_3BODY    = 0
              N_H2O_3BODY    = 0
              N_N2_3BODY     = 0  
              N_O2_3BODY     = 0
              N_CH4_REACTION = 0
              N_H2_REACTION  = 0
! reset varaible of KPP_DATA
              INDEX_FIXED_SPECIES = 0
            END IF
            
            DO I = 1, NREACTIONS   
               FIXED_SPC_COUNT       = 0
               LABELS( I,1 )         = REACTION_LIST( I )%LABEL(1)                     
               LABELS( I,2 )         = REACTION_LIST( I )%LABEL(2)                     
               IRXBITS( I )          = REACTION_LIST( I )%IRXBITS            
               KTYPE( I )            = REACTION_LIST( I )%RATE_TYPE          
               NPRDCT( I )           = REACTION_LIST( I )%NPRDCT             
               NREACT( I )           = REACTION_LIST( I )%NREACT             
               IORDER( I )           = REACTION_LIST( I )%ORDER             
               RTDAT( 1:3,I )        = REACTION_LIST( I )%RTDAT(1:3)         
               IRR( I,1:MAXPRODS+3 ) = REACTION_LIST( I )%IRR(1:MAXPRODS+3)  
               SC( I, 1:MAXPRODS )   = REACTION_LIST( I )%SC(1:MAXPRODS)     
               IF( KTYPE( I ) .EQ. 12 .OR. ( KTYPE( I ) .GT. 7 .AND. KTYPE( I ) .LT. 11  ) )THEN
                 IFALLOFF = IFALLOFF + 1
                 IRRFALL( IFALLOFF )   = I ! REACTION_LIST( I )%FALLOFF_INDEX
                 RFDAT( 1:5,IFALLOFF ) = REACTION_LIST( I )%RFDAT(1:5)
               WRITE(6,'(A,I4,1X,(A16,1X),(I2,1X),5(ES12.4,1X))')"PHOTOLYSIS: I, LABELS( I, 1 ), IRRFALL = ", I, 
     &         LABELS( I, 1 ),IRRFALL( IFALLOFF ),RFDAT( 1:5,IFALLOFF )
               END IF
               IF( KTYPE( I ) .EQ. 0 )THEN
                 IPHOT = IPHOT + 1
                 IPH( IPHOT,1 ) = I ! REACTION_LIST( I )%PHOTO_INDEX(1:3)
                 IPH( IPHOT,2 ) = REACTION_LIST( I )%PHOTO_INDEX(2)
                 IPH( IPHOT,3 ) = REACTION_LIST( I )%PHOTO_INDEX(3)
               WRITE(6,'(A,I4,1X,(A16,1X),(I3,1X),A16,1X,I3)')"PHOTOLYSIS: I, LABELS( I, 1 ), IPH = ", I, 
     &         LABELS( I, 1 ),IPH( IPHOT,1 ),PHOTAB(IPH( IPHOT,2 )),IPH( IPHOT,3 )
               END IF
               IF( KTYPE( I ) .EQ. 11 )THEN
                 JSPECIAL = JSPECIAL + 1
                 ISPECIAL( JSPECIAL,1 ) = I ! REACTION_LIST( I )%SPECIAL_INDEX(1:2)
                 ISPECIAL( JSPECIAL,2 ) = REACTION_LIST( I )%SPECIAL_INDEX(2)
               END IF
               IF( KTYPE( I ) .EQ. -1 )THEN
                 IHET = IHET + 1
                 IHETERO(IHET,1) = I ! REACTION_LIST( I )%HETEO_INDEX(1:2)
                 IHETERO(IHET,2) = REACTION_LIST( I )%HETEO_INDEX(2)
               END IF
!reset third body, CH4 and H2 reaction pointers
               DO K = 1, REACTION_LIST( I )%NAIR_RCTNTS
                  N_AIR_3BODY = N_AIR_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWM(N_AIR_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 1
                  print*,'I, N_AIR_3BODY, NRXWM(N_AIR_3BODY) = ',I, N_AIR_3BODY, NRXWM(N_AIR_3BODY)
               END DO
               DO K = 1, REACTION_LIST( I )%NH2O_RCTNTS
                  N_H2O_3BODY = N_H2O_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWW(N_AIR_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 2
               END DO
               DO K = 1, REACTION_LIST( I )%N_O2_RCTNTS
                  N_O2_3BODY = N_O2_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWO2(N_O2_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 3
               END DO
               DO K = 1, REACTION_LIST( I )%N_N2_RCTNTS
                  N_N2_3BODY = N_N2_3BODY  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWN2(N_N2_3BODY) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 4
               END DO
               DO K = 1, REACTION_LIST( I )%N_H2_RCTNTS
                  N_H2_REACTION = N_H2_REACTION  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWH2(N_H2_REACTION) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 6
               END DO
               DO K = 1, REACTION_LIST( I )%NCH4_RCTNTS
                  N_CH4_REACTION = N_CH4_REACTION  + 1
                  FIXED_SPC_COUNT = FIXED_SPC_COUNT + 1
                  NRXWH2(N_CH4_REACTION) = I
                  INDEX_FIXED_SPECIES( I, FIXED_SPC_COUNT ) = 5
               END DO
               WRITE(6,'(A,I4,1X,(A16,1X),2(I2,1X))')"PHOTOLYSIS: I, LABELS( I, 1 ), KTYPE( I ), IORDER = ", I, 
     &         LABELS( I, 1 ),KTYPE( I ),IORDER( I )
               IF ( FIXED_SPC_COUNT .GT. 3 ) THEN
                 WRITE( *,* )'Number of Constant Species Exceeds Three for Reaction:', LABELS( I,1 )
                 STOP
               END IF
            END DO         
! rest number of third body, CH4 and H2 reactions
            NWM   = N_AIR_3BODY
            NWW   = N_H2O_3BODY
            NWO2  = N_O2_3BODY
            NWN2  = N_N2_3BODY
            NWH2  = N_H2_REACTION
            NWCH4 = N_CH4_REACTION
! update labels
            DO J = 1, (NMPHOT + NTHERMAL)
               RXLABEL( J ) = LABELS( J,1 )
               WRITE(6,'(A,I4,1X,3(A16,1X),I2)')"MECHANISM: J, LABELS(J,1:2),RXLABEL( J ), KTYPE( J ) = ", J,
     &         LABELS( J,1:2), RXLABEL( J ), KTYPE( J )
            END DO
         END SUBROUTINE REPLACE_REACTIONS
       END MODULE MECHANISM_DATA
            
            
            
