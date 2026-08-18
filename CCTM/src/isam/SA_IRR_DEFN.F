
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


      MODULE SA_IRR_DEFN

C***********************************************************************
C20140428                       
C  
C  (1) Stores initial reaction rates in a C-R-L-nrxns array
C  (2) Contains subroutines SA_IRR_INIT
C                       and ACCUMRR
C
C    Aug 16, 2011: chemical integration time interval is in MINUTES
C
C***********************************************************************


      IMPLICIT NONE


      REAL, ALLOCATABLE :: RXINIT( :,:,:,: )
      REAL, ALLOCATABLE :: RKI_INIT( :,:,:,: )
      REAL, ALLOCATABLE :: YC_INIT( :,:,:,: )
      REAL, ALLOCATABLE :: RKMID ( : )

      Real, Allocatable, Save :: A( :, : )     ! One minus half timestep times Jacobain matrix
                                               ! representing chemistry ODEs
      Real,    Allocatable, Save :: A1( :, : ) ! One plus half timestep times Jacobain matrix
                                               ! representing chemistry ODEs
      ! Real 8 versions of above matricies
      Real(8), Allocatable, Save :: Real8A( :, : )
      Real(8), Allocatable, Save :: Real8A1( :, : )
      
      Real, Allocatable, Save :: PDT( :, : ) ! Jacobain matrix representing chemistry ODEs
      Real, Allocatable, Save :: PRD( : )    ! final estimate of of tag concentrations at t plus timestep 

      ! Real 8 versions of above matrix and vector
      REAL(8), ALLOCATABLE, Save :: PRD0( : )        ! initial estimate of of tag concentrations at t plus timestep
      Real(8), Allocatable, Save :: Real8PDT( :, : )
      Real(8), Allocatable, Save :: Real8PRD( : )

      Integer, Allocatable, Save :: IPVT ( : )      ! an integer vector of pivot indices for A matrix LU Decomposition
      Integer, Allocatable       :: SENS_INDEX( : ) ! CGRID_INDEX translated by JNEW2OLD
      Integer, Allocatable       :: MECH_INDEX( : ) ! CGRID_INDEX translated by JNEW2OLD

      REAL( 8 ) :: SA_TEMP( 1 )    ! cell air temperature, K
      REAL( 8 ) :: SA_DENSITY( 1 ) ! air cell air mass density, kg/m3

!20140307 Integrated Rates


      INTEGER              :: ISAM_CHEMISTRY_SPC ! number of ISAM species om photochemistry
      INTEGER, ALLOCATABLE :: ISAM_SPC_MAP( : ) ! index in ISAM species array
      INTEGER, ALLOCATABLE :: ISAM_TO_CHEM( : ) ! index is CHEMSITRY_SPC array
      LOGICAL, ALLOCATABLE :: CONVERT_ISAM( : ) ! whether to ISAM concentration for photochemsitry 
      LOGICAL, ALLOCATABLE :: BIAS_CHEMISTRY_SPC( :,: ) ! whether to favor chemistry species when a reactant
           
      REAL                   :: CONMIN_TAG               ! min tag concentration uploaded
      REAL( 8 )              :: NUMB_ISAM_CELLS = 0.0D0
      REAL( 8 )              :: DCONCMIN                 ! internal min concentration
      REAL( 8 )              :: DCONMIN_TAG              ! internal min tag concentration
      REAL( 8 )              :: UNIFORM_DCONMIN_TAG      ! min tag concentration evenly distributed

      REAL( 8 ), ALLOCATABLE :: UGM3_TO_PPM( : )      ! CGRID to CHEM Species conversion factor
      REAL,      ALLOCATABLE :: PPM_TO_UGM3( : )      ! CHEM to CGRID Species conversion factor
      REAL( 8 ), ALLOCATABLE :: SOLD( :,: )           ! local source concentrations

      
      LOGICAL, ALLOCATABLE :: ISAM_SPECIES( : )
      LOGICAL              :: ISAM_NOT_FOUND = .FALSE.

      TYPE SPECIES_BUDGET
         CHARACTER(16)        :: SPECIES_NAME = ' '
         INTEGER              :: NREACTIONS   = 0
         INTEGER, ALLOCATABLE :: IREACTION( : )
         REAL(8), ALLOCATABLE :: COEFF_NET( : )
         INTEGER              :: NRXNS_PROD   = 0
         INTEGER, ALLOCATABLE :: IRXN_PROD( : )
         REAL(8), ALLOCATABLE :: COEFF_POS( : )
         INTEGER              :: NRXNS_LOSS   = 0
         INTEGER, ALLOCATABLE :: IRXN_LOSS( : )
         REAL(8), ALLOCATABLE :: COEFF_NEG( : )
      END TYPE SPECIES_BUDGET
      
      TYPE(SPECIES_BUDGET), ALLOCATABLE :: MECHANISM_BUDGET ( : )
      TYPE(SPECIES_BUDGET), ALLOCATABLE :: OX_RADICAL_BUDGET( : )
      TYPE(SPECIES_BUDGET), ALLOCATABLE :: ISAM_SPC_BUDGET  ( : )   
      
      INTEGER :: NRXNS_PROD_H2O2      = 0              ! # reactions producing only H2O2
      INTEGER :: NRXNS_PROD_HNO3      = 0              ! # reactions producing only HNO3
      INTEGER :: NRXNS_PROD_H2O2_HNO3 = 0              ! # reactions producing H2O2 or HNO3
      INTEGER, ALLOCATABLE :: H2O2_HNO3_PROD_RXNS( : ) ! indices of H2O2 or HNO3 producing reactions

      REAL( 8 ), ALLOCATABLE :: H2O2_HNO3_YIELD( : )   ! H2O2 or HNO3 yield for a reaction subset
      REAL( 8 ) :: VOC_TO_NOX = -1.0D0
       
      INTEGER :: ISAM_LOG        ! Unit number of output log
      INTEGER :: ISAM_JDATE = 0
      INTEGER :: ISAM_JTIME = 0
      INTEGER :: ISAM_NSTEP = 0
#ifdef verbose_isam
      LOGICAL :: CHECK_ISAM  = .TRUE.
      LOGICAL :: WRITE_BUDGET_REPORT = .TRUE.
#else
      LOGICAL :: CHECK_ISAM  = .FALSE.
      LOGICAL :: WRITE_BUDGET_REPORT = .FALSE.
#endif      
      LOGICAL :: WRITE_CELL  = .FALSE.
      LOGICAL :: UPDATE_SOLD = .FALSE.
      LOGICAL :: UPDATE_PROBABILITIES = .FALSE.

      INTEGER, PARAMETER :: N_OX_RADICALS = 13
      CHARACTER( 16 )    :: OX_RADICALS( N_OX_RADICALS ) = 
     &                      (/ 'O               ',      
     &                         'O1D             ',      
     &                         'ACO3            ',      
     &                         'MEO3            ',      
     &                         'C2O3            ',      
     &                         'O3P             ',      
     &                         'NO              ',
     &                         'NO2             ',     
     &                         'NO3             ',     
     &                         'N2O5            ',     
     &                         'HO2             ',
     &                         'MEO2            ',
     &                         'MO2             ' /)
     
       LOGICAL, ALLOCATABLE :: IS_ISAM_OX_RADICAL( : )
       LOGICAL, ALLOCATABLE :: IS_CHEM_OX_RADICAL( : )
       LOGICAL, ALLOCATABLE :: IS_TAG_NONZERO( : )

       INTEGER              :: OX_RADICAL_FOUND = 0   
       INTEGER              :: OZONE_INDEX      = 0     
       INTEGER, ALLOCATABLE :: OX_INDEX ( : )
       
       LOGICAL :: ISAM_WITH_OZONE = .FALSE.

! specific species indices in chemical mechanism 
       INTEGER              :: H2O2_INDEX       = 0     
       INTEGER              :: HNO3_INDEX       = 0     
       INTEGER              :: O3_INDEX         = 0     
       LOGICAL              :: CHEMISTRY_WITH_O3 = .FALSE.
       LOGICAL              :: USE_REAL4         = .FALSE.

       REAL( 8 ), PARAMETER, PRIVATE :: ONE    = 1.0D0
       REAL( 8 ), PARAMETER, PRIVATE :: ZERO   = 0.0D0
       REAL( 8 ), PARAMETER, PRIVATE :: CUTOFF = 1.0D-23
       REAL( 8 ), PARAMETER, PRIVATE :: BULK_CUTOFF = ZERO

       INTERFACE FILTER_AND_RENORMAL
          MODULE PROCEDURE DFILTER_AND_RENORMAL, ! double precision version
     &                     SFILTER_AND_RENORMAL  ! single precision version
       END INTERFACE

      CONTAINS
        SUBROUTINE SA_IRR_INIT

          USE HGRD_DEFN
          USE VGRD_DEFN
          USE UTILIO_DEFN
          USE RXNS_DATA
          USE SA_DEFN
          USE CONST, ONLY: MWAIR
C Initialize arrays and maps that store reaction rates in each grid cell and that
C         relate ISAM species to chemistry species
C
C         Called by chemistry driver

        IMPLICIT NONE

         CHARACTER( 16 ), PARAMETER :: PNAME = 'SA_IRR_INIT'     ! Program name

         INTEGER :: I, J, RXN, IP, IL 
         INTEGER :: IOSTAT
         INTEGER :: C, L, R, S   ! Loop indices
         INTEGER :: SPC          ! array index
         INTEGER :: IOS


         CHARACTER( 132 ) :: MSG           ! Message text
! temporary arrays to set maps between isam to chemistry species
         INTEGER, ALLOCATABLE :: ISAM_SPC_IDX( : )
         INTEGER, ALLOCATABLE :: ISAM_2_CHEMI( : )
         LOGICAL, ALLOCATABLE :: NO_CHEMISTRY( : )

         CHARACTER(16), ALLOCATABLE :: FIND_IN_ISAM( : )

! temporary variables to define MECHANISM_BUDGET

         INTEGER, ALLOCATABLE :: IREACTION( : )
         INTEGER, ALLOCATABLE :: IRXN_PROD( : )
         INTEGER, ALLOCATABLE :: IRXN_LOSS( : )
         REAL(8), ALLOCATABLE :: COEFF_NET( : )
         REAL(8), ALLOCATABLE :: COEFF_POS( : )
         REAL(8), ALLOCATABLE :: COEFF_NEG( : )
         REAL(8)              :: COEFF

         TYPE(SPECIES_BUDGET) :: H2O2_BUDGET
         TYPE(SPECIES_BUDGET) :: HNO3_BUDGET
         
C=======================================================

        ISAM_LOG = INIT3( )
        


        ALLOCATE( ISAM_2_CHEMI( NSPC_SA + 1 ) )
        ALLOCATE( ISAM_SPC_IDX( NSPC_SA + 1 ) )
        ALLOCATE( NO_CHEMISTRY( NSPC_SA + 1 ) )
        ALLOCATE( FIND_IN_ISAM( NSPC_SA + 1 ) )
        ALLOCATE( BIAS_CHEMISTRY_SPC( NBIAS, NUMB_MECH_SPC ) )
        BIAS_CHEMISTRY_SPC = .FALSE.
        
        ! Identify species index in ISAM array
        ISAM_SPC_IDX = 0
        ISAM_2_CHEMI = 0
        NO_CHEMISTRY = .TRUE.
        FIND_IN_ISAM = ' '

        SPC = 0
        
        DO S = 1, NSPC_SA
           FIND_IN_ISAM( S ) = ISAM_SPEC( S,OTHRTAG )
           ISAM_SPC_IDX( S ) = S
        END DO
        SPC = NSPC_SA
! find tagged species in chemistry_spc array to set value of convert_isam
        ISAM_CHEMISTRY_SPC = 0
        DO S = 1, SPC
           R  = INDEX1( TRIM(FIND_IN_ISAM( S )), NUMB_MECH_SPC, CHEMISTRY_SPC )
           IF ( R .LE. 0 ) THEN
              MSG = 'ISAM SPECIES: ' 
     &           // TRIM( FIND_IN_ISAM( S ) ) 
     &           // ' not found in CHEMISTRY_SPC array  '
              CALL M3WARN( PNAME, 0, 0, MSG )
              CYCLE
           END IF
           ISAM_CHEMISTRY_SPC = ISAM_CHEMISTRY_SPC
     &                        + 1           
           ISAM_2_CHEMI( S )  = R
           BIAS_CHEMISTRY_SPC( :,R ) = ISAM_SPECIES_BIAS( :,S )
           NO_CHEMISTRY( S )  = .FALSE.
        END DO
        
        HNO3_INDEX = INDEX1( 'HNO3', NUMB_MECH_SPC, CHEMISTRY_SPC )
        H2O2_INDEX = INDEX1( 'H2O2', NUMB_MECH_SPC, CHEMISTRY_SPC )
        IF ( H2O2_INDEX .LT. 1 ) THEN
            H2O2_INDEX = INDEX1( 'HO2H', NUMB_MECH_SPC, CHEMISTRY_SPC )        
        END IF
        O3_INDEX = INDEX1( 'H2O2', NUMB_MECH_SPC, CHEMISTRY_SPC )
        IF ( O3_INDEX .GT. 0 ) CHEMISTRY_WITH_O3 = .TRUE.
        
        IF ( ISAM_CHEM_BIAS .EQ. 5 ) THEN
          IF ( H2O2_INDEX .LT. 1 .OR. HNO3_INDEX .LT. 1 ) THEN
             IF ( H2O2_INDEX .LT. 1 ) THEN
                 WRITE(LOGDEV,'(A)')'ISAM_O3_WEIGHTS set to 5 but hydrogen peroxide not found gas chemistry.'
             END IF
             IF ( HNO3_INDEX .LT. 1 ) THEN
                 WRITE(LOGDEV,'(A)')'ISAM_O3_WEIGHTS set to 5 but nitric acid not found gas chemistry.'
             END IF
             MSG = 'Above Error(s) not found using ISAM runtime option'
             CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
          END IF        
          IF ( O3_INDEX .LT. 1 ) THEN
             WRITE(LOGDEV,'(A)')'ISAM_O3_WEIGHTS set to 5 but gas chemistry does not have ozone.'
             WRITE(LOGDEV,'(A)')'ISAM_O3_WEIGHTS reset to 1.'
             ISAM_CHEM_BIAS = 1
          END IF
        END IF

        IF( ANY(  .NOT. NO_CHEMISTRY ) )THEN
C..Save pointer for isam species found in chemistry species
           ALLOCATE( ISAM_TO_CHEM( ISAM_CHEMISTRY_SPC ) )
           ALLOCATE( ISAM_SPC_MAP( ISAM_CHEMISTRY_SPC ) )
           WRITE(ISAM_LOG,'(/A)')'Below isam species participate in photochemistry'
           WRITE(ISAM_LOG,'("SPC     ISAM_SPC     SPC PHOTOCHEM_SPC  ")')
           L = 0 
           DO S = 1, SPC
              IF ( .NOT. NO_CHEMISTRY( S ) ) THEN
                 L = L + 1
                 C = ISAM_SPC_IDX( S )
                 R = ISAM_2_CHEMI( S )
                 ISAM_SPC_MAP( L ) = ISAM_SPC_IDX( S )
                 ISAM_TO_CHEM( L ) = ISAM_2_CHEMI( S )
                 WRITE(ISAM_LOG,'(I3,1X,A16,1x,I3,1X,A16)') 
     &           C, FIND_IN_ISAM( S ), R, CHEMISTRY_SPC( R )
              END IF
           END DO
           IF( L .NE. ISAM_CHEMISTRY_SPC )THEN
             MSG = 'ERROR mapping isam to chemistry species: inconsistent number found'
             CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
           END IF
        ELSE
           MSG = 'NO ISAM species participate in photochemistry '
           CALL M3WARN ( PNAME, 0, 0, MSG )
        END IF
        L = 0
        DO S = 1, SPC
           IF( NO_CHEMISTRY( S ) )THEN
                C = ISAM_SPC_IDX( S )
                IF( C .LE. 0 )CYCLE
                L = L + 1
                IF( L .LT. 2 )THEN
                    WRITE(ISAM_LOG,'(/A)')'Below isam species DO NOT participate in photochemistry'
                    WRITE(ISAM_LOG,'("SPC     ISAM_SPC")')
                END IF    
                WRITE(ISAM_LOG,'(I3,1X,A16,1x,I3,A16)') C, FIND_IN_ISAM( S )
           END IF
        END DO

        DEALLOCATE( ISAM_2_CHEMI )
        DEALLOCATE( ISAM_SPC_IDX )
        DEALLOCATE( NO_CHEMISTRY )

C...Allocate and set conversion factors for isam chemistry species
        ALLOCATE( CONVERT_ISAM( ISAM_CHEMISTRY_SPC ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
             MSG = 'Error allocating CONVERT_ISAM'
             CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF 
              
        ALLOCATE( UGM3_TO_PPM( ISAM_CHEMISTRY_SPC ),
     &            PPM_TO_UGM3( ISAM_CHEMISTRY_SPC ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
             MSG = 'Error allocating UGM3_TO_PPM or PPM_TO_UGM3'
             CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF 

        ALLOCATE( IS_ISAM_OX_RADICAL( ISAM_CHEMISTRY_SPC ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
             MSG = 'Error allocating IS_ISAM_A_RADICAL'
             CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF 
        IS_ISAM_OX_RADICAL = .FALSE.

        ALLOCATE( IS_CHEM_OX_RADICAL( NUMB_MECH_SPC ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
             MSG = 'Error allocating IS_ISAM_A_RADICAL'
             CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF 
        IS_CHEM_OX_RADICAL = .FALSE.

        WRITE(ISAM_LOG,'(/A)')'Final Table of ISAM chemistry species'
        WRITE(ISAM_LOG,'("SPC     ISAM_SPC     SPC PHOTOCHEM_SPC  Mol.Wei Convert Conc. Radical Spc")')
              
        ALLOCATE( IS_TAG_NONZERO( NTAG_SA ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
             MSG = 'Error allocating IS_TAG_NONZERO'
             CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF 

!       ALLOCATE( FRAC_TOTAL( NTAG_SA ), STAT = IOS )
!       IF ( IOS .NE. 0 ) THEN
!            MSG = 'Error allocating FRAC_TOTAL'
!            CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
!       END IF 

        ALLOCATE( ISAM_SPECIES( NUMB_MECH_SPC ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
           MSG = 'Error allocating ISAM_SPECIES'
           CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF
        ISAM_SPECIES = .FALSE. 

        DO S = 1, ISAM_CHEMISTRY_SPC
           C = ISAM_SPC_MAP( S )
           R = ISAM_TO_CHEM( S )
           IF( FIND_IN_ISAM( C ) .EQ. 'O3' )THEN
               ISAM_WITH_OZONE = .TRUE.
               OZONE_INDEX     =  R
               WRITE(ISAM_LOG,'(A)')'ISAM Species include ozone'
           END IF    
           DO L = 1, N_OX_RADICALS
              IF( FIND_IN_ISAM( C ) .EQ. OX_RADICALS( L ) )THEN
                  IS_ISAM_OX_RADICAL( S ) = .TRUE.
              END IF    
              IF( CHEMISTRY_SPC( R ) .EQ. OX_RADICALS( L ) )THEN
                  IS_CHEM_OX_RADICAL( R ) = .TRUE.
                  OX_RADICAL_FOUND        = OX_RADICAL_FOUND + 1
              END IF    
           END DO
           CONVERT_ISAM( S ) = CONVERT_CONC( R )
           ISAM_SPECIES( R ) = .TRUE.
           UGM3_TO_PPM ( S ) = REAL( 1.0E-3 * MWAIR / SPECIES_MOLWT( R ), 8 )
           PPM_TO_UGM3 ( S ) = 1.0E+3 / MWAIR * SPECIES_MOLWT( R )
           WRITE(ISAM_LOG,'(I3,1X,A16,1x,I3,1X,A16,2X,F7.2,3(1X,L14))') 
     &     C, FIND_IN_ISAM( C ), R, CHEMISTRY_SPC( R ), SPECIES_MOLWT( R ), 
     &     CONVERT_ISAM( S ), IS_ISAM_OX_RADICAL( S ),IS_CHEM_OX_RADICAL( R )
        END DO
        
        DEALLOCATE( FIND_IN_ISAM )

        ALLOCATE( OX_INDEX( OX_RADICAL_FOUND ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
           MSG = 'Error allocating SOLD'
           CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF 
        OX_INDEX = 0

        DCONMIN_TAG = 1.0D-40 
        DCONCMIN    = 1.0D-30 
        CONMIN_TAG  = 1.0E-30
         
        UNIFORM_DCONMIN_TAG = DCONMIN_TAG / REAL( NTAG_SA,8 ) 

        ALLOCATE( SOLD( NTAG_SA, NUMB_MECH_SPC ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
           MSG = 'Error allocating SOLD'
           CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF 
        
        ALLOCATE( MECHANISM_BUDGET( NUMB_MECH_SPC ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
           MSG = 'Error allocating MECHANISM_BUDGET'
           CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF 

        ALLOCATE( IREACTION( NRXNS ),
     &            IRXN_PROD( NRXNS ),
     &            IRXN_LOSS( NRXNS ),
     &            COEFF_POS( NRXNS ),
     &            COEFF_NEG( NRXNS ),
     &            COEFF_NET( NRXNS ),  STAT = IOS )     
        IF ( IOS .NE. 0 ) THEN
           MSG = 'Error allocating IREACTION and COEFF_NET arrays'
           CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF 

        ISAM_WITH_OZONE = ( ISAM_WITH_OZONE .AND. OX_RADICAL_FOUND .GT. 0 )
        IF( OX_RADICAL_FOUND .GT. 0  )THEN
           ALLOCATE( OX_RADICAL_BUDGET( OX_RADICAL_FOUND ), STAT = IOS )
           IF ( IOS .NE. 0 ) THEN
              MSG = 'Error allocating MECHANISM_BUDGET'
              CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
           END IF 
        END IF

! find how reactions affect all chemistry species
        L = 0
        DO SPC = 1, NUMB_MECH_SPC
           S  = 0
           IP = 0
           IL = 0
           MECHANISM_BUDGET( SPC )%SPECIES_NAME = CHEMISTRY_SPC( SPC )
           MECHANISM_BUDGET( SPC )%NREACTIONS   = 0
           MECHANISM_BUDGET( SPC )%NRXNS_PROD   = 0
           MECHANISM_BUDGET( SPC )%NRXNS_LOSS   = 0
           IREACTION = 0
           COEFF_NET = 0.0D0
           COEFF_POS = 0.0D0
           COEFF_NEG = 0.0D0           
! set indices for Ox radicals
           IF( IS_CHEM_OX_RADICAL( SPC ) )THEN
               L = L + 1
               OX_INDEX( L ) = SPC
               OX_RADICAL_BUDGET( L )%SPECIES_NAME = CHEMISTRY_SPC( SPC )
               OX_RADICAL_BUDGET( L )%NREACTIONS   = 0
               OX_RADICAL_BUDGET( L )%NRXNS_PROD   = 0
               OX_RADICAL_BUDGET( L )%NRXNS_LOSS   = 0
           END IF    
! find effect on CHEMISTRY_SPC( SPC ) and save results
           DO R = 1, NRXNS
              COEFF = EFFECT_REACTION( SPC, R, C )
              IF( ABS( COEFF ) .GT. 1.0D-8 )THEN
                 S = S + 1
                 IREACTION( S ) = R
                 COEFF_NET( S ) = COEFF
                 IF( COEFF  .GT. 0.0D0 )THEN
                   IP = IP + 1
                   IRXN_PROD( IP ) = R
                   COEFF_POS( IP ) = COEFF
                 ELSE
                   IL = IL + 1
                   IRXN_LOSS( IL ) = R
                   COEFF_NEG( IL ) = ABS( COEFF )
                 END IF
              END IF
           END DO
! set up species budget                
           IF( S .GT. 0 )THEN
               MECHANISM_BUDGET( SPC )%NREACTIONS = S
               ALLOCATE( MECHANISM_BUDGET( SPC )%IREACTION( S ),
     &                   MECHANISM_BUDGET( SPC )%COEFF_NET( S ),  STAT = IOS )
               IF ( IOS .NE. 0 ) THEN
                    MSG = 'Error allocating bulk MECHANISM_BUDGET arrays'
                   CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
               END IF 
               MECHANISM_BUDGET( SPC )%IREACTION( 1:S ) = IREACTION( 1:S )
               MECHANISM_BUDGET( SPC )%COEFF_NET( 1:S ) = COEFF_NET( 1:S )
               IF( IS_CHEM_OX_RADICAL( SPC ) )THEN
                   OX_RADICAL_BUDGET( L )%NREACTIONS = S
                   ALLOCATE( OX_RADICAL_BUDGET( L )%IREACTION( S ),
     &                       OX_RADICAL_BUDGET( L )%COEFF_NET( S ),  STAT = IOS )
                   IF ( IOS .NE. 0 ) THEN
                       MSG = 'Error allocating production OX_RADICAL_BUDGET arrays'
                       CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
                   END IF 
                   OX_RADICAL_BUDGET( L )%IREACTION( 1:S )  = IRXN_PROD( 1:S )
                   OX_RADICAL_BUDGET( L )%COEFF_NET( 1:S )  = COEFF_NET( 1:S )
               END IF
               IF( IP .GT. 0 )THEN
! define production information
                  MECHANISM_BUDGET( SPC )%NRXNS_PROD = IP
                  ALLOCATE( MECHANISM_BUDGET( SPC )%IRXN_PROD( IP ),
     &                      MECHANISM_BUDGET( SPC )%COEFF_POS( IP ),  STAT = IOS )
                  IF ( IOS .NE. 0 ) THEN
                       MSG = 'Error allocating production MECHANISM_BUDGET arrays'
                      CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
                  END IF 
                  MECHANISM_BUDGET( SPC )%IRXN_PROD( 1:IP )  = IRXN_PROD( 1:IP )
                  MECHANISM_BUDGET( SPC )%COEFF_POS( 1:IP )  = COEFF_POS( 1:IP )
! capture production information if OX radical
                  IF( IS_CHEM_OX_RADICAL( SPC ) )THEN
                     OX_RADICAL_BUDGET( L )%NRXNS_PROD = IP
                     ALLOCATE( OX_RADICAL_BUDGET( L )%IRXN_PROD( IP ),
     &                         OX_RADICAL_BUDGET( L )%COEFF_POS( IP ),  STAT = IOS )
                     IF ( IOS .NE. 0 ) THEN
                         MSG = 'Error allocating production OX_RADICAL_BUDGET arrays'
                         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
                     END IF 
                     OX_RADICAL_BUDGET( L )%IRXN_PROD( 1:IP )  = IRXN_PROD( 1:IP )
                     OX_RADICAL_BUDGET( L )%COEFF_POS( 1:IP )  = COEFF_POS( 1:IP )
                  END IF
               END IF
               
               IF( IL .GT. 0 )THEN
! define destruction information
                  MECHANISM_BUDGET( SPC )%NRXNS_LOSS = IL
                  ALLOCATE( MECHANISM_BUDGET( SPC )%IRXN_LOSS( IL ),
     &                      MECHANISM_BUDGET( SPC )%COEFF_NEG( IL ),  STAT = IOS )
                  IF ( IOS .NE. 0 ) THEN
                       MSG = 'Error allocating destruction MECHANISM_BUDGET arrays'
                      CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
                  END IF 
                  MECHANISM_BUDGET( SPC )%IRXN_LOSS( 1:IL ) = IRXN_LOSS( 1:IL )
                  MECHANISM_BUDGET( SPC )%COEFF_NEG( 1:IL ) = COEFF_NEG( 1:IL )
! capture destruction information if OX radical
                  IF( IS_CHEM_OX_RADICAL( SPC ) )THEN
                     OX_RADICAL_BUDGET( L )%NRXNS_LOSS = IL
                     ALLOCATE( OX_RADICAL_BUDGET( L )%IRXN_LOSS( IL ),
     &                         OX_RADICAL_BUDGET( L )%COEFF_NEG( IL ),  STAT = IOS )
                     IF ( IOS .NE. 0 ) THEN
                         MSG = 'Error allocating production OX_RADICAL_BUDGET arrays'
                         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
                     END IF 
                     OX_RADICAL_BUDGET( L )%IRXN_LOSS( 1:IL )  = IRXN_LOSS( 1:IL )
                     OX_RADICAL_BUDGET( L )%COEFF_NEG( 1:IL )  = COEFF_NET( 1:IL )
                  END IF
               END IF
           END IF                  
        END DO
        
        DEALLOCATE( IREACTION,
     &              IRXN_PROD,
     &              IRXN_LOSS,
     &              COEFF_POS,
     &              COEFF_NEG,
     &              COEFF_NET )     


! Set Hydrogen Peroxide budget data        
        IF ( H2O2_INDEX .GT. 0 ) THEN
           H2O2_BUDGET%SPECIES_NAME = CHEMISTRY_SPC( H2O2_INDEX )           
           H2O2_BUDGET%NREACTIONS = MECHANISM_BUDGET( H2O2_INDEX )%NREACTIONS

! set up reaction data with net hydrogen peroxide change                
           S = H2O2_BUDGET%NREACTIONS
           IF ( S .GT. 0 ) THEN
               ALLOCATE( H2O2_BUDGET%IREACTION( S ),
     &                   H2O2_BUDGET%COEFF_NET( S ),  STAT = IOS )
               IF ( IOS .NE. 0 ) THEN
                    MSG = 'Error allocating H2O2 budget array'
                   CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
               END IF
               H2O2_BUDGET%IREACTION( 1:S ) = MECHANISM_BUDGET( H2O2_INDEX )%IREACTION( 1:S )
               H2O2_BUDGET%COEFF_NET( 1:S ) = MECHANISM_BUDGET( H2O2_INDEX )%COEFF_NET( 1:S )
           END IF    

! define hydrogen peroxide production reaction information
           
           H2O2_BUDGET%NRXNS_PROD = MECHANISM_BUDGET( H2O2_INDEX )%NRXNS_PROD
           IP = H2O2_BUDGET%NRXNS_PROD
           IF ( IP .GT. 0 ) THEN
               ALLOCATE( H2O2_BUDGET%IRXN_PROD( IP ),
     &                   H2O2_BUDGET%COEFF_POS( IP ),  STAT = IOS )
               IF ( IOS .NE. 0 ) THEN
                    MSG = 'Error allocating production H2O2_BUDGET arrays'
                   CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
               END IF
               H2O2_BUDGET%IRXN_PROD( 1:IP ) = MECHANISM_BUDGET( H2O2_INDEX )%IRXN_PROD( 1:IP )
               H2O2_BUDGET%COEFF_POS( 1:IP ) = MECHANISM_BUDGET( H2O2_INDEX )%COEFF_POS( 1:IP )
           END IF

! define hydrogen peroxide production reaction information

           H2O2_BUDGET%NRXNS_LOSS = MECHANISM_BUDGET( H2O2_INDEX )%NRXNS_LOSS
           IL = H2O2_BUDGET%NRXNS_LOSS
           IF ( IL .GT. 0 ) THEN
               ALLOCATE( H2O2_BUDGET%IRXN_LOSS( IL ),
     &                   H2O2_BUDGET%COEFF_NEG( IL ),  STAT = IOS )
               IF ( IOS .NE. 0 ) THEN
                    MSG = 'Error allocating production H2O2_BUDGET arrays'
                   CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
               END IF
               H2O2_BUDGET%IRXN_LOSS( 1:IL ) = MECHANISM_BUDGET( H2O2_INDEX )%IRXN_LOSS( 1:IL )
               H2O2_BUDGET%COEFF_NEG( 1:IL ) = MECHANISM_BUDGET( H2O2_INDEX )%COEFF_NEG( 1:IL )
           END IF

        END IF

! Set Nitric Acid budget data        
        IF ( HNO3_INDEX .GT. 0 ) THEN

! set up reaction data with net nitric acid change                
           HNO3_BUDGET%SPECIES_NAME = CHEMISTRY_SPC( HNO3_INDEX )
           HNO3_BUDGET%NREACTIONS = MECHANISM_BUDGET( HNO3_INDEX )%NREACTIONS
           S = HNO3_BUDGET%NREACTIONS
           IF ( S .GT. 0 ) THEN
               ALLOCATE( HNO3_BUDGET%IREACTION( S ),
     &                   HNO3_BUDGET%COEFF_NET( S ),  STAT = IOS )
               IF ( IOS .NE. 0 ) THEN
                    MSG = 'Error allocating HNO3 budget array'
                   CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
               END IF
               HNO3_BUDGET%IREACTION( 1:S ) = MECHANISM_BUDGET( HNO3_INDEX )%IREACTION( 1:S )
               HNO3_BUDGET%COEFF_NET( 1:S ) = MECHANISM_BUDGET( HNO3_INDEX )%COEFF_NET( 1:S )
           END IF    

! define nitric acid production reaction information
           HNO3_BUDGET%NRXNS_PROD = MECHANISM_BUDGET( HNO3_INDEX )%NRXNS_PROD
           IP = HNO3_BUDGET%NRXNS_PROD
           IF ( IP .GT. 0 ) THEN
               ALLOCATE( HNO3_BUDGET%IRXN_PROD( IP ),
     &                   HNO3_BUDGET%COEFF_POS( IP ),  STAT = IOS )
               IF ( IOS .NE. 0 ) THEN
                    MSG = 'Error allocating production HNO3_BUDGET arrays'
                   CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
               END IF
               HNO3_BUDGET%IRXN_PROD( 1:IP ) = MECHANISM_BUDGET( HNO3_INDEX )%IRXN_PROD( 1:IP )
               HNO3_BUDGET%COEFF_POS( 1:IP ) = MECHANISM_BUDGET( HNO3_INDEX )%COEFF_POS( 1:IP )
           END IF
! define nitric acid destruction information
           HNO3_BUDGET%NRXNS_LOSS = MECHANISM_BUDGET( HNO3_INDEX )%NRXNS_LOSS
           IL = HNO3_BUDGET%NRXNS_LOSS
           IF ( IL .GT. 0 ) THEN
               ALLOCATE( HNO3_BUDGET%IRXN_LOSS( IL ),
     &                   HNO3_BUDGET%COEFF_NEG( IL ),  STAT = IOS )
               IF ( IOS .NE. 0 ) THEN
                    MSG = 'Error allocating production HNO3_BUDGET arrays'
                   CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
               END IF
               HNO3_BUDGET%IRXN_LOSS( 1:IL ) = MECHANISM_BUDGET( HNO3_INDEX )%IRXN_LOSS( 1:IL )
               HNO3_BUDGET%COEFF_NEG( 1:IL ) = MECHANISM_BUDGET( HNO3_INDEX )%COEFF_NEG( 1:IL )
           END IF
        END IF

        NRXNS_PROD_HNO3 = HNO3_BUDGET%NRXNS_PROD
        NRXNS_PROD_H2O2 = H2O2_BUDGET%NRXNS_PROD
        NRXNS_PROD_H2O2_HNO3 = NRXNS_PROD_HNO3 + NRXNS_PROD_H2O2
        IF ( NRXNS_PROD_HNO3 .GT. 0 .AND. NRXNS_PROD_HNO3 .GT. 0 ) THEN
           ALLOCATE( H2O2_HNO3_PROD_RXNS( NRXNS_PROD_H2O2_HNO3 ),STAT = IOS )
           IF ( IOS .NE. 0 ) THEN
                MSG = 'Error allocating production H2O2_HNO3_PROD_RXNS arrays'
               CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
           END IF
           IP = NRXNS_PROD_H2O2
           IL = NRXNS_PROD_HNO3
           H2O2_HNO3_PROD_RXNS( 1:IP )      = H2O2_BUDGET%IRXN_PROD( 1:IP ) 
           H2O2_HNO3_PROD_RXNS( (IP+1):IP+IL ) = HNO3_BUDGET%IRXN_PROD( 1:IL ) 
           ALLOCATE( H2O2_HNO3_YIELD( NRXNS_PROD_H2O2_HNO3 ),STAT = IOS )
           IF ( IOS .NE. 0 ) THEN
                MSG = 'Error allocating production H2O2_HNO3_YIELD arrays'
               CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
           END IF
           H2O2_HNO3_YIELD( 1:IP ) = H2O2_BUDGET%COEFF_POS( 1:IP ) 
           H2O2_HNO3_YIELD( (IP+1):IP+IL ) = HNO3_BUDGET%COEFF_POS( 1:IL ) 
        END IF

        ALLOCATE( ISAM_SPC_BUDGET( ISAM_CHEMISTRY_SPC ), STAT = IOS )
        IF ( IOS .NE. 0 ) THEN
             MSG = 'Error allocating ISAM_SPC_BUDGET'
             CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF 

        DO SPC = 1, ISAM_CHEMISTRY_SPC
           R = ISAM_TO_CHEM( SPC )
           ISAM_SPC_BUDGET( SPC )%SPECIES_NAME = MECHANISM_BUDGET( R )%SPECIES_NAME
           ISAM_SPC_BUDGET( SPC )%NREACTIONS   = MECHANISM_BUDGET( R )%NREACTIONS  
           ISAM_SPC_BUDGET( SPC )%NRXNS_PROD   = MECHANISM_BUDGET( R )%NRXNS_PROD  
           ISAM_SPC_BUDGET( SPC )%NRXNS_LOSS   = MECHANISM_BUDGET( R )%NRXNS_LOSS  
! define net information
           S =  ISAM_SPC_BUDGET( SPC )%NREACTIONS
           IF( S .GT. 0 )THEN
               ALLOCATE( ISAM_SPC_BUDGET( SPC )%IREACTION( S ),
     &                   ISAM_SPC_BUDGET( SPC )%COEFF_NET( S ),  STAT = IOS )
               IF ( IOS .NE. 0 ) THEN
                    MSG = 'Error allocating bulk ISAM_SPC_BUDGET arrays'
                   CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
               END IF 
               ISAM_SPC_BUDGET( SPC )%IREACTION( 1:S ) = MECHANISM_BUDGET( R )%IREACTION( 1:S )
               ISAM_SPC_BUDGET( SPC )%COEFF_NET( 1:S ) = MECHANISM_BUDGET( R )%COEFF_NET( 1:S )
           END IF          
           IP = ISAM_SPC_BUDGET( SPC )%NRXNS_PROD
           IF( IP .GT. 0 )THEN
! define production information
              ALLOCATE( ISAM_SPC_BUDGET( SPC )%IRXN_PROD( IP ),
     &                  ISAM_SPC_BUDGET( SPC )%COEFF_POS( IP ),  STAT = IOS )
              IF ( IOS .NE. 0 ) THEN
                  MSG = 'Error allocating production ISAM_SPC_BUDGET arrays'
                  CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
              END IF 
              ISAM_SPC_BUDGET( SPC )%IRXN_PROD( 1:IP )  = MECHANISM_BUDGET( R )%IRXN_PROD( 1:IP )
              ISAM_SPC_BUDGET( SPC )%COEFF_POS( 1:IP )  = MECHANISM_BUDGET( R )%COEFF_POS( 1:IP )
           END IF       
           IL = ISAM_SPC_BUDGET( SPC )%NRXNS_LOSS
           IF( IL .GT. 0 )THEN
! define destruction information
              ALLOCATE( ISAM_SPC_BUDGET( SPC )%IRXN_LOSS( IL ),
     &                  ISAM_SPC_BUDGET( SPC )%COEFF_NEG( IL ),  STAT = IOS )
              IF ( IOS .NE. 0 ) THEN
                  MSG = 'Error allocating production ISAM_SPC_BUDGET arrays'
                  CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
              END IF 
              ISAM_SPC_BUDGET( SPC )%IRXN_LOSS( 1:IL )  = MECHANISM_BUDGET( R )%IRXN_LOSS( 1:IL )
              ISAM_SPC_BUDGET( SPC )%COEFF_NEG( 1:IL )  = MECHANISM_BUDGET( R )%COEFF_NEG( 1:IL )
           END IF  
           WRITE(ISAM_LOG,*)SPC, ISAM_SPC_BUDGET( SPC )%SPECIES_NAME,IP,IL    
        END DO          
        
! report budget for mechanism species and OX radicals
        IF( WRITE_BUDGET_REPORT ) THEN
            CALL REPORT_MECH_BUDGET( ISAM_LOG )
            IF( OX_RADICAL_FOUND .GT. 0  )THEN
               CALL REPORT_OX_RADICALS( ISAM_LOG )
            ELSE
               MSG = "Note that no Oxygen Radicals were found in mechanism."   
               WRITE(ISAM_LOG,'(A)')TRIM( MSG )
            END IF
            IF( ISAM_CHEMISTRY_SPC .GT. 0 )THEN
               CALL REPORT_ISAM_BUDGET( ISAM_LOG )
            ELSE
               MSG = "Note that ISAM found in mechanism."   
               WRITE(ISAM_LOG,'(A)')TRIM( MSG )
            END IF
        END IF    
        
        END SUBROUTINE SA_IRR_INIT
      REAL(8) FUNCTION EFFECT_REACTION( NAMINDX, NRX, OCCURS )

C-----------------------------------------------------------------------
C Function: To find net effect on the number of species molecules from a reaction 
 
C Preconditions: None
  
C Key Subroutines/Functions Called: None
 
C Revision History:
C  Prototype created by Bill Hutzell, May, 2018
C-----------------------------------------------------------------------
      USE RXNS_DATA

      IMPLICIT NONE
      
C Includes: None
      
C Arguments:
      INTEGER,        INTENT(IN )   :: NAMINDX  ! Index for chemistry species 
      INTEGER,        INTENT(IN )   :: NRX      ! Reaction number
      INTEGER,        INTENT(INOUT) :: OCCURS   ! Number of products and reaction 
                                        
C Parameters: None

C External Functions: None 

C Local Variables:

      CHARACTER( 16 ) :: SPECIS    ! Species name to check

      INTEGER INDX       ! Pointer to reactant or product in CHEMISTRY_SPC array
      INTEGER IRRPNTR    ! Pointer to reactant or product in IRR array
      INTEGER N          ! Loop index over IRR array

      REAL(8) TOTAL      ! Sum of molecular production and loss coeffecients
         
C-----------------------------------------------------------------------
      OCCURS = 0
      TOTAL  = 0.0D0

      SPECIS = CHEMISTRY_SPC( NAMINDX )
c..Subtract the number of species molecules lost in this reaction
      DO N = 1, NREACT( NRX )
         INDX = IRR( NRX, N )
         IF ( INDX .EQ. NAMINDX ) THEN
             TOTAL  = TOTAL - 1.0D0
             OCCURS = OCCURS + 1
         END IF    
      END DO
      
c..Add the number of species molecules produced in this reaction
      DO N = 1, NPRDCT( NRX )
         IRRPNTR = N + 3
         INDX = IRR( NRX, IRRPNTR )
         IF ( INDX .EQ. NAMINDX ) THEN
             TOTAL  = TOTAL + REAL( SC( NRX,N ), 8)
             OCCURS = OCCURS + 1
         END IF    
      END DO

      EFFECT_REACTION = TOTAL

      RETURN

      END FUNCTION EFFECT_REACTION
      SUBROUTINE REPORT_MECH_BUDGET( OUT_UNIT )
!        purpose writes out 
         USE RXNS_DATA

         IMPLICIT NONE


!..Arguments:
         INTEGER,   INTENT( IN ) ::  OUT_UNIT  ! output unit #
         
        INTEGER SPC
        INTEGER IR, NR
         
        DO SPC = 1, NUMB_MECH_SPC
           WRITE(OUT_UNIT,95000)MECHANISM_BUDGET( SPC )%SPECIES_NAME, 
     &                          MECHANISM_BUDGET( SPC )%NREACTIONS
           DO NR = 1, MECHANISM_BUDGET( SPC )%NREACTIONS
              IR = MECHANISM_BUDGET( SPC )%IREACTION( NR )
              WRITE(OUT_UNIT,95001)RXLABEL( IR ),MECHANISM_BUDGET( SPC )%COEFF_NET( NR )
           END DO
              WRITE(OUT_UNIT,95005)MECHANISM_BUDGET( SPC )%SPECIES_NAME, 
     &                             MECHANISM_BUDGET( SPC )%NRXNS_PROD
              DO NR = 1, MECHANISM_BUDGET( SPC )%NRXNS_PROD
                 IR = MECHANISM_BUDGET( SPC )%IRXN_PROD( NR )
                 WRITE(OUT_UNIT,95001)RXLABEL( IR ),MECHANISM_BUDGET( SPC )%COEFF_POS( NR )
              END DO
        END DO
        
95000   FORMAT("Chemistry species, ",A16,", changed by the ",I4," below reactions",
     &         / 3X, "Reaction Label  ",1X,"Net Coeff."  )
95001   FORMAT(3X,A16,1X,ES12.4)
95005   FORMAT("Radical  species, ",A16,", produced by the ",I4," below reactions",
     &         / 3X, "Reaction Label  ",1X,"Net Coeff."  )

      END SUBROUTINE REPORT_MECH_BUDGET    
      SUBROUTINE REPORT_OX_RADICALS( OUT_UNIT )
!        purpose writes out production and loss reaction for each OX radical
         USE RXNS_DATA

         IMPLICIT NONE

!..Arguments:
         INTEGER,   INTENT( IN ) ::  OUT_UNIT  ! output unit #
         
        INTEGER SPC
        INTEGER IR, NR
         
        DO SPC = 1, OX_RADICAL_FOUND
           IF( OX_RADICAL_BUDGET( SPC )%NRXNS_PROD .GT. 0 )THEN
              WRITE(OUT_UNIT,95005)OX_RADICAL_BUDGET( SPC )%SPECIES_NAME, 
     &                             OX_RADICAL_BUDGET( SPC )%NRXNS_PROD
              DO NR = 1, OX_RADICAL_BUDGET( SPC )%NRXNS_PROD
                 IR = OX_RADICAL_BUDGET( SPC )%IRXN_PROD( NR )
                 WRITE(OUT_UNIT,95001)RXLABEL( IR ),OX_RADICAL_BUDGET( SPC )%COEFF_POS( NR )
              END DO
           ELSE   
              WRITE(OUT_UNIT,95003)OX_RADICAL_BUDGET( SPC )%SPECIES_NAME 
           END IF   
           IF( OX_RADICAL_BUDGET( SPC )%NRXNS_LOSS .GT. 0 )THEN
              WRITE(OUT_UNIT,95002)OX_RADICAL_BUDGET( SPC )%SPECIES_NAME, 
     &                             OX_RADICAL_BUDGET( SPC )%NRXNS_LOSS
              DO NR = 1, OX_RADICAL_BUDGET( SPC )%NRXNS_LOSS
                 IR = OX_RADICAL_BUDGET( SPC )%IRXN_LOSS( NR )
                 WRITE(OUT_UNIT,95001)RXLABEL( IR ),OX_RADICAL_BUDGET( SPC )%COEFF_NEG( NR )
              END DO
           ELSE
              WRITE(OUT_UNIT,95004)OX_RADICAL_BUDGET( SPC )%SPECIES_NAME 
           END IF   
        END DO
        
95005   FORMAT("Radical  species, ",A16,", produced by the ",I4," below reactions",
     &         / 3X, "Reaction Label  ",1X,"Net Coeff."  )
95001   FORMAT(3X,A16,1X,ES12.4)
95002   FORMAT("Radical  species, ",A16,", destoryed by the ",I4," below reactions",
     &         / 3X, "Reaction Label  ",1X,"Net Coeff."  )
95003   FORMAT(A16, " radical not produced by any reactions.")
95004   FORMAT(A16, " radical not destoryed by any reactions.")

      END SUBROUTINE REPORT_OX_RADICALS
      SUBROUTINE REPORT_ISAM_BUDGET( OUT_UNIT )
!        purpose writes out 
         USE RXNS_DATA

         IMPLICIT NONE


!..Arguments:
         INTEGER,   INTENT( IN ) ::  OUT_UNIT  ! output unit #
         
        INTEGER SPC
        INTEGER IR, NR
         
        DO SPC = 1, ISAM_CHEMISTRY_SPC
           WRITE(OUT_UNIT,95100)ISAM_SPC_BUDGET( SPC )%SPECIES_NAME, 
     &                          ISAM_SPC_BUDGET( SPC )%NREACTIONS
           DO NR = 1, ISAM_SPC_BUDGET( SPC )%NREACTIONS
              IR = ISAM_SPC_BUDGET( SPC )%IREACTION( NR )
              WRITE(OUT_UNIT,95101)RXLABEL( IR ),ISAM_SPC_BUDGET( SPC )%COEFF_NET( NR )
           END DO
              WRITE(OUT_UNIT,95105)ISAM_SPC_BUDGET( SPC )%SPECIES_NAME, 
     &                             ISAM_SPC_BUDGET( SPC )%NRXNS_PROD
              DO NR = 1, ISAM_SPC_BUDGET( SPC )%NRXNS_PROD
                 IR = ISAM_SPC_BUDGET( SPC )%IRXN_PROD( NR )
                 WRITE(OUT_UNIT,95101)RXLABEL( IR ),ISAM_SPC_BUDGET( SPC )%COEFF_POS( NR )
              END DO
        END DO
        
95100   FORMAT("   ISAM  species, ",A16,", changed by the ",I4," below reactions",
     &         / 3X, "Reaction Label  ",1X,"Net Coeff."  )
95101   FORMAT(3X,A16,1X,ES12.4)
95105   FORMAT("   ISAM  species, ",A16,", produced by the ",I4," below reactions",
     &         / 3X, "Reaction Label  ",1X,"Net Coeff."  )

      END SUBROUTINE REPORT_ISAM_BUDGET    
      SUBROUTINE SA_IRR_EXTRACT( COL, ROW, LAY, DENS, CONC )
                
          USE HGRD_DEFN
          USE VGRD_DEFN
          USE RXNS_DATA
          USE UTILIO_DEFN
          USE SA_DEFN  

         IMPLICIT NONE

!..Arguments:
         INTEGER,   INTENT( IN ) ::  COL        ! cell column index
         INTEGER,   INTENT( IN ) ::  ROW        ! cell row index 
         INTEGER,   INTENT( IN ) ::  LAY        ! cell layer index      
         REAL,      INTENT( IN ) ::  DENS       ! air mass density, kg/m3
         REAL(8),   INTENT( IN ) ::  CONC( : )  ! cgrid concentrations

!..Local:
         CHARACTER( 16 ), PARAMETER :: PNAME = 'SA_IRR_EXTRACT'     ! Program name

         REAL      :: FACTOR2
         REAL( 8 ) :: TOTAL, FACTOR1, FACTOR3
         REAL( 8 ) :: INV_DENS       ! one over air mass density, m3/kg

         INTEGER :: JSPC, KTAG
!..variables borrowed from DDM
         INTEGER :: I, J, RXN
         INTEGER :: C, L, R, S   ! Loop indices
         INTEGER :: SPC          ! array index
!        REAL(8) :: TAGS_TOTAL
         
!         INTEGER :: IOS         
!         CHARACTER( 132 )  :: MSG

          SOLD = ZERO

          INV_DENS = REAL( ONE/DENS, 8 )
          
          DO JSPC = 1, ISAM_CHEMISTRY_SPC
             S       = ISAM_TO_CHEM( JSPC )
             SPC     = ISAM_SPC_MAP( JSPC )
             IS_TAG_NONZERO = .FALSE.
             LOAD_SOLD: DO KTAG = 1, NTAG_SA
                SOLD( KTAG, S ) = MAX( ZERO,
     &                            REAL( ISAM( COL,ROW,LAY,SPC,KTAG ),8 ))
             END DO LOAD_SOLD ! ktag loop
!            TAGS_TOTAL = SUM( SOLD( 1:NTAG_SA, S ) )
!            TAGS_TOTAL = 1.0D0 / MAX( TAGS_TOTAL, DCONMIN_TAG )
             FILTER_SOLD: DO KTAG = 1, NTAG_SA
!               FRAC_TOTAL( KTAG ) = SOLD( KTAG, S ) * TAGS_TOTAL
                IF( SOLD( KTAG, S ) .GT. ZERO )THEN
                   IS_TAG_NONZERO( KTAG ) = .TRUE.
                   IF( CONVERT_ISAM( JSPC ) )THEN
                      SOLD( KTAG, S ) = SOLD( KTAG, S )
     &                                * INV_DENS * UGM3_TO_PPM( JSPC )
                   END IF
                   IF( SOLD( KTAG,S ) .LT. CUTOFF )THEN
                      SOLD( KTAG,S ) = ZERO
                   END IF
                END IF
             END DO FILTER_SOLD ! ktag loop

! Remove bangs to prevent All SOLD equaling zero               
!             IF( MAXVAL( SOLD( 1:NTAG_SA,S ) ) .LE. ZERO )THEN
!                IF( ANY( IS_TAG_NONZERO ) )THEN
!                    DO KTAG = 1, NTAG_SA                    
!                       IF( IS_TAG_NONZERO( KTAG ) )THEN
!                          SOLD( KTAG,S ) = FRAC_TOTAL( KTAG ) * DCONMIN_TAG
!                       END IF
!                    END DO
!             END IF    
          END DO ! loop jspc

#ifdef verbose_isam          
          IF( WRITE_CELL )THEN
             DO KTAG = 1, NTAG_SA
               WRITE(ISAM_LOG,*)' '
               WRITE(ISAM_LOG,'(A24,2(1X,ES12.4))')'Initial MAX Values',
     &         MAXVAL(ISAM(:,:,:,:,KTAG))
             END DO ! ktag loop
             DO JSPC = 1, NSPC_SA 
                  TOTAL = SUM( ISAM( COL,ROW,LAY,JSPC,1:NTAG_SA ) )
                  WRITE(ISAM_LOG,'(A16,11(1X,ES12.4))')ISAM_SPEC( JSPC,OTHRTAG ),
     &            (ISAM( COL,ROW,LAY,JSPC,KTAG ),KTAG = 1, NTAG_SA), TOTAL
             END DO
             WRITE(ISAM_LOG,*)'EX: Initial Totals'
             DO JSPC = 1, ISAM_CHEMISTRY_SPC
                S   = ISAM_TO_CHEM( JSPC )
                SPC = ISAM_SPC_MAP( JSPC ) 
                TOTAL = 0.0
                DO KTAG = 1, NTAG_SA
                   TOTAL = TOTAL + ISAM( COL,ROW,LAY,SPC,KTAG )
                END DO 
                WRITE(ISAM_LOG,'(2(A16,1X),5(1X,ES12.4))')ISAM_SPEC( SPC,OTHRTAG ), CHEMISTRY_SPC(S), 
     &          TOTAL, CONC( S ),TOTAL-CONC( S ), TOTAL/CONC( S )
             END DO ! loop jspc
          END IF
#endif
        END SUBROUTINE SA_IRR_EXTRACT
        SUBROUTINE SA_IRR_UPLOAD( COL, ROW, LAY, DENS, CONC )
                
          USE HGRD_DEFN
          USE VGRD_DEFN
          USE RXNS_DATA
          USE UTILIO_DEFN
          USE ELMO_DATA, ONLY : ELMO_VOC_NOX
          USE SA_DEFN    ! 20130517

         IMPLICIT NONE

!..Arguments:
         INTEGER,   INTENT( IN ) ::  COL        ! cell column index
         INTEGER,   INTENT( IN ) ::  ROW        ! cell row index 
         INTEGER,   INTENT( IN ) ::  LAY        ! cell layer index      
         REAL,      INTENT( IN ) ::  DENS       ! air mass density, kg/m3
         REAL(8),   INTENT( IN ) ::  CONC( : )  ! cgrid concentrations

!..Local:
         CHARACTER( 16 ), PARAMETER :: PNAME = 'SA_IRR_UPLOAD'     ! Program name

         REAL      :: TOTAL
         REAL( 8 ) :: INV_DENS       ! one over air mass density, m3/kg

         INTEGER :: JSPC, KTAG
         INTEGER :: I, J, RXN
         INTEGER :: C, L, R, S   ! Loop indices
         INTEGER :: SPC          ! array index
         
!         INTEGER :: IOS         
!         CHARACTER( 132 )  :: MSG

         REAL :: TEMP_VALUE

         DO JSPC = 1, ISAM_CHEMISTRY_SPC
            S   = ISAM_TO_CHEM( JSPC )
            SPC = ISAM_SPC_MAP( JSPC ) 
            LOAD_ISAM: DO KTAG = 1, NTAG_SA
               IF( SOLD( KTAG,S ) .GT. DCONCMIN )THEN
                 TEMP_VALUE = REAL( SOLD( KTAG,S ) )
                 IF( CONVERT_ISAM( JSPC ) )THEN
                     TEMP_VALUE = TEMP_VALUE
     &                          * DENS * PPM_TO_UGM3( JSPC )
                 END IF
                 ISAM( COL,ROW,LAY,SPC,KTAG ) = MAX( CONMIN_TAG, TEMP_VALUE )
               ELSE
                 ISAM( COL,ROW,LAY,SPC,KTAG ) = ZERO
               END IF    
            END DO LOAD_ISAM ! ktag loop     
         END DO   ! jspc
         ELMO_VOC_NOX( COL,ROW,LAY )  = VOC_TO_NOX
 
#ifdef verbose_isam             
          IF( WRITE_CELL )THEN
             DO KTAG = 1, NTAG_SA
               WRITE(ISAM_LOG,*)' '
               WRITE(ISAM_LOG,'(A24,2(1X,ES12.4))')'Final MAX Values',
     &         MAXVAL(ISAM(:,:,:,:,KTAG))
             END DO ! ktag loop
             DO JSPC = 1, NSPC_SA 
                  TOTAL = SUM( ISAM( COL,ROW,LAY,JSPC,1:NTAG_SA ) )
                  WRITE(ISAM_LOG,'(A16,11(1X,ES12.4))')ISAM_SPEC( JSPC,OTHRTAG ),
     &            (ISAM( COL,ROW,LAY,JSPC,KTAG ),KTAG = 1, NTAG_SA), TOTAL
             END DO
             WRITE(ISAM_LOG,*)'UP: Final Totals'
             DO JSPC = 1, ISAM_CHEMISTRY_SPC
                S   = ISAM_TO_CHEM( JSPC )
                SPC = ISAM_SPC_MAP( JSPC )
                TEMP_VALUE =  MAX( REAL(CONC( S )),1.0E-30 )
                TOTAL = 0.0
                DO KTAG = 1, NTAG_SA
                   TOTAL = TOTAL + ISAM( COL,ROW,LAY,SPC,KTAG )
                END DO 
                WRITE(ISAM_LOG,'(2(A16,1X),3(1X,ES12.4))')ISAM_SPEC( SPC,OTHRTAG ), CHEMISTRY_SPC(S), 
     &          TOTAL,TEMP_VALUE,TOTAL-TEMP_VALUE
             END DO ! loop jspc
          END IF
#endif
        END SUBROUTINE SA_IRR_UPLOAD
C----------------------------------------------------------------------
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SA_IRR_UNBLOCKED ( LSTART, RK, CONC, DELT )

C-----------------------------------------------------------------------
C  Function: Iupdate tag concentrations
 
C  Preconditions: None
 
C  Key Subroutines/Functions Called: None
 
C  Revision History:
C   Prototype created by Jerry Gipson, September, 1996
C   global BLKPRM Jeff Dec 96
C   Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C   Modified Jun, 1998 by Jerry Gipson to add reaction number error checks
C   Modified 1/19/99 by David Wong at LM:
C                      -- add four include files because of new PA_CMN.EXT
C   Modified 2/26/99 by David Wong at LM:
C                      -- remove SUBST_AE_SPC, SUBST_NR_SPC, SUBST_TR_SPC,
C                         three .EXT files
C   31 Mar 01 J.Young: Use HGRD_DEFN; eliminate BLKPRM.EXT
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   21 Jun 10 J.Young: convert for Namelist redesign
C   19 Aug 11 J.Young: Replaced I/O API include files with UTILIO_DEFN
C   07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module

C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE RXNS_DATA             ! chemical mechanism data
      USE CGRID_SPCS            ! CGRID mechanism species
      USE SA_DEFN
      USE UTILIO_DEFN

      IMPLICIT NONE 

C..Includes: None
      
C..Arguments: 
      LOGICAL, INTENT( IN ) :: LSTART   ! Flag to indicate start of chemical integration period

      REAL( 8 ),    INTENT( IN ) :: RK  ( : )    ! Reaction rate coefficients
      REAL( 8 ),    INTENT( IN ) :: CONC( : )    ! species concentrations
      REAL( 8 ),    INTENT( IN ) :: DELT         ! Chemistry integration time size

C..Parameters: None

      REAL(8), SAVE  :: NOX_VOC_SWITCH ! REAL( VOC_NOX_TRANS,8 ), switch between NOx to limiting ozone production
      
C..External Functions: None
 
      CHARACTER( 16 ) , SAVE :: PNAME = 'PA_IRR'   ! Program name
      CHARACTER( 132)        :: MSG

      LOGICAL, SAVE :: LFIRST = .TRUE.   ! Flag for first call to subroutine

C..Scratch Local Variables:
      INTEGER ISP1, ISP2, ISP3  ! Species indices
      INTEGER    S, JSPC,  SPC  ! Species indices
      INTEGER NCELL             ! Loop index for cells
      INTEGER NIRR              ! Loop index for IRR outputs
      INTEGER NOUT              ! IRR output index
      INTEGER NC,NR,NRX         ! Loop index for reactions
      INTEGER NTEMP             ! Loop index for temp IRRs
      INTEGER NTERM             ! Loop index for terms
      INTEGER ASTAT             ! allocation status
      INTEGER KTAG              ! Loop index/pointer for source
      INTEGER POSITIVE          ! count of tag concentration greater than zero

      REAL(8) TOTAL                   ! scratch term for summations
      REAL(8) TOTAL_PROD              ! scratch term for total bulk production
      REAL(8) ISAM_PROD               ! scratch term for total isam production
      REAL(8) TERM                    ! scratch term for summations
      REAL(8) TOTAL_LOSS              ! scratch term for total bulk loss
      REAL(8) ISAM_LOSS               ! scratch term for total isam loss
      REAL(8) LOSS_FACT               ! effective loss frequency
      REAL(8) DELTA_BULK              ! change in bulk concentration
      REAL(8) DELTA_H2O2              ! change in bulk H2O2 concentration
      REAL(8) DELTA_HNO3              ! change in bulk HNO3 concentration
      REAL(8) COEFF                   ! Coefficient of IRR term
      REAL(8) TOTAL_PROBABILITY       ! normalization coefficient for SOURCE_PROBABILITY
      REAL(8) ISAM_TOTAL_PROBABILITY  ! normalization coefficient for ISAM_PROBABILITY
      REAL(8) H2O2_Production         ! gross hydrogen peroxide production
      REAL(8) HNO3_Production         ! gross nitric acid production
      
      LOGICAL :: REMOVED                 = .FALSE.   ! whether initial concentrations are wiped out
      LOGICAL, SAVE :: DISTRIBUTE_TO_ALL = .TRUE.    ! distribute extra production among all source tags 
                                                     ! or just to the other source tag
                                                     
      LOGICAL, ALLOCATABLE, SAVE   :: SOURCE_ZERO( : ) ! whether source concentration greater than zero 
C..Saved Local Variables:

      REAL( 8 ), ALLOCATABLE, SAVE :: YCOLD  ( : )     ! bulk concentrations from previous time step
      REAL( 8 ), ALLOCATABLE, SAVE :: OUTSIDE( : )     ! species concentration outside isam tags
      REAL( 8 ), ALLOCATABLE, SAVE :: YCMID  ( : )     ! bulk concentrations from previous time step
      REAL( 8 ), ALLOCATABLE, SAVE :: YCRATES( : )     ! bulk concentration used to calculate reactionvrates 
      REAL( 8 ), ALLOCATABLE, SAVE :: RXRAT  ( : )     ! reactions rates   
      REAL( 8 ), ALLOCATABLE, SAVE :: INTRXN ( : )     ! Integrated reaction rates

      REAL( 8 ) :: ONE_OVER_CONC      ! reciprocal of total species concentrations
      REAL( 8 ), ALLOCATABLE, SAVE :: TOTAL_ISAM_CONC( : )      ! total concentrations from isam sources
      REAL( 8 ), ALLOCATABLE, SAVE :: NOT_OUTSIDE_ISAM( : )     ! fraction of species not from non-isam sources
      REAL( 8 ), ALLOCATABLE, SAVE :: NOT_ISAM_SOURCE( :,: )    ! fraction of species not from an isam source
      REAL( 8 ), ALLOCATABLE, SAVE :: ISAM_SOURCE( :,: )        ! fraction of species from an isam source
      REAL( 8 ), ALLOCATABLE, SAVE :: SOURCE_PROBABILITY( :,: ) ! probability or amount that a source contributes to reaction
      REAL( 8 ), ALLOCATABLE, SAVE :: SOURCE_DELTA   ( :,: )    ! change in source tag species 
      REAL( 8 ), ALLOCATABLE, SAVE :: SA_DELTA( : )
      REAL( 8 ), ALLOCATABLE, SAVE :: SA_LOSS( : )
      REAL( 8 ), ALLOCATABLE, SAVE :: ISAM_PROBABILITY  ( :,: ) ! probability or amount that a source contributes to reaction      
      LOGICAL,   ALLOCATABLE, SAVE :: ZERO_ISAM( :           )  ! whether isam species have no effect on reaction
      LOGICAL,   ALLOCATABLE, SAVE :: REBALANCE( : )            ! renormalize tag concentration when production occurs
      LOGICAL,   ALLOCATABLE, SAVE :: SOURCE_NONZERO( :,: )     ! fraction of species not from an isam source

      INTEGER,   ALLOCATABLE, SAVE :: MAP_REACTANT_CASE( : )      
      LOGICAL,   ALLOCATABLE, SAVE :: TWO_REACTANT_CASE( :,:,: )
      LOGICAL,   ALLOCATABLE, SAVE :: THREE_REACTANT_CASE( :,:,: )
      
      

C-----------------------------------------------------------------------

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  On first call, flag the reactions for which to calculate IRRS
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LFIRST ) THEN

C set value of NOX_VOC_SWITCH

        NOX_VOC_SWITCH = REAL( VOC_NOX_TRANS,8 )

C Allocate arrays:

        ALLOCATE( YCOLD( NUMB_MECH_SPC ), STAT = ASTAT )
        IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating YCOLD variable'
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
        END IF

        ALLOCATE( YCMID( NUMB_MECH_SPC ), STAT = ASTAT )
        IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating YCOLD variable'
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
        END IF
        
        ALLOCATE( YCRATES( NUMB_MECH_SPC ), STAT = ASTAT )
        IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating YCRATES variable'
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
        END IF
        
        ALLOCATE( OUTSIDE( NUMB_MECH_SPC ), STAT = ASTAT )
        IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating YCOLD variable'
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
        END IF
        
        OUTSIDE = 0.0D0
        
        ALLOCATE( RXRAT  ( NRXNS ),
     &            INTRXN ( NRXNS ), STAT = ASTAT )
        IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating SA_IRR variables'
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
        END IF

!         ALLOCATE( ONE_OVER_CONC( NUMB_MECH_SPC ), STAT = ASTAT )
!         IF ( ASTAT .NE. 0 ) THEN
!           MSG = 'ERROR allocating SA_IRR variables'
!           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
!         END IF

         ALLOCATE( TOTAL_ISAM_CONC( NUMB_MECH_SPC ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating SA_IRR variables'
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF

         ALLOCATE( NOT_ISAM_SOURCE( NTAG_SA, NUMB_MECH_SPC ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating SA_IRR variables'
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF

         ALLOCATE( ISAM_SOURCE( NTAG_SA, NUMB_MECH_SPC ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating SA_IRR variables'
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF

         ALLOCATE( SOURCE_PROBABILITY( NTAG_SA + 1, NRXNS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating SA_IRR variables'
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF

         ALLOCATE( ISAM_PROBABILITY( NTAG_SA, NRXNS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating ISAM_PROBABILITY variables'
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF

         ALLOCATE( ZERO_ISAM( NRXNS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating ZERO_ISAM '
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF

         ALLOCATE( REBALANCE( NUMB_MECH_SPC ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating REBALANCE '
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF
         REBALANCE = .FALSE.
             
         ALLOCATE( SOURCE_DELTA( NTAG_SA + 1, NUMB_MECH_SPC ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating SOURCE_DELTA '
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF

         ALLOCATE( SA_DELTA( NTAG_SA ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating SA_DELTA '
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF

         ALLOCATE( SA_LOSS( NTAG_SA ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating SA_LOSS '
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF

         ALLOCATE( SOURCE_ZERO( NTAG_SA + 1 ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating SOURCE_ZERO '
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF
         LFIRST = .FALSE.

         ALLOCATE( NOT_OUTSIDE_ISAM( NUMB_MECH_SPC ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating SA_IRR variables'
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF

         ALLOCATE( SOURCE_NONZERO( NTAG_SA, NUMB_MECH_SPC ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating SA_IRR variables'
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF

         SOURCE_NONZERO = .TRUE.

! map unimolecular, bimolecular and trimolecular reactions
         
         ALLOCATE( MAP_REACTANT_CASE( NRXNS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
           MSG = 'ERROR allocating SA_IRR variables'
           CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
         END IF
         MAP_REACTANT_CASE = -1
         
         ISP1 = 0
         ISP2 = 0
         ISP3 = 0
         DO NRX = 1, NRXNS
            SELECT CASE ( NREACT( NRX ) )
               CASE ( 1 )
                  ISP1 = ISP1 + 1                    
                  MAP_REACTANT_CASE( NRX ) = ISP1
               CASE( 2 ) 
                  ISP2 = ISP2 + 1                    
                  MAP_REACTANT_CASE( NRX ) = ISP2
               CASE( 3 )
                  ISP3 = ISP3 + 1                    
                  MAP_REACTANT_CASE( NRX ) = ISP3
            END SELECT
         END DO

         IF( ISP2 .GT. 0 )THEN
            ALLOCATE( TWO_REACTANT_CASE( NBIAS,ISP2,3 ), STAT = ASTAT )
            IF ( ASTAT .NE. 0 ) THEN
              MSG = 'ERROR allocating SA_IRR variables'
              CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
            END IF
            TWO_REACTANT_CASE = .FALSE.
         END IF
         
         IF( ISP3 .GT. 0 )THEN
            ALLOCATE( THREE_REACTANT_CASE( NBIAS,ISP3,7 ), STAT = ASTAT )
            IF ( ASTAT .NE. 0 ) THEN
              MSG = 'ERROR allocating SA_IRR variables'
              CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
            END IF
            THREE_REACTANT_CASE = .FALSE.
         END IF         

C...Calculate reactant cases based on species_bias for each reactant
         DO NC = 1, NBIAS
            DO NRX = 1, NRXNS
               NR = MAP_REACTANT_CASE( NRX )
               SELECT CASE ( NREACT( NRX ) )
                  CASE( 2 ) 
                      ISP1 = IRR( NRX,1 )
                      ISP2 = IRR( NRX,2 )
                      TWO_REACTANT_CASE( NC,NR,1 ) = (  BIAS_CHEMISTRY_SPC( NC,ISP1 ) 
     &                                                 .AND. .NOT. BIAS_CHEMISTRY_SPC( NC,ISP2 ) )
                      TWO_REACTANT_CASE( NC,NR,2 ) = ( .NOT. BIAS_CHEMISTRY_SPC( NC,ISP1 ) 
     &                                                 .AND. BIAS_CHEMISTRY_SPC( NC,ISP2 ) )
                      TWO_REACTANT_CASE( NC,NR,3 ) = ( .NOT. BIAS_CHEMISTRY_SPC( NC,ISP1 ) 
     &                                                 .AND. .NOT. BIAS_CHEMISTRY_SPC( NC,ISP2 ) )
     &                                           .OR. ( BIAS_CHEMISTRY_SPC( NC,ISP1 ) 
     &                                                 .AND. BIAS_CHEMISTRY_SPC( NC,ISP2 ) )
                  CASE( 3 )
                      ISP1 = IRR( NRX,1 )
                      ISP2 = IRR( NRX,2 )
                      ISP3 = IRR( NRX,3 )
                      THREE_REACTANT_CASE( NC,NR,1 ) = (              BIAS_CHEMISTRY_SPC( NC,ISP1 ) 
     &                                                   .AND.  .NOT. BIAS_CHEMISTRY_SPC( NC,ISP2 ) 
     &                                                   .AND.  .NOT. BIAS_CHEMISTRY_SPC( NC,ISP3 ) )
                      THREE_REACTANT_CASE( NC,NR,2 ) = (        .NOT. BIAS_CHEMISTRY_SPC( NC,ISP1 ) 
     &                                                   .AND.        BIAS_CHEMISTRY_SPC( NC,ISP2 ) 
     &                                                   .AND. .NOT.  BIAS_CHEMISTRY_SPC( NC,ISP3 ) )
                      THREE_REACTANT_CASE( NC,NR,3 ) = (       .NOT.  BIAS_CHEMISTRY_SPC( NC,ISP1 ) 
     &                                                   .AND. .NOT.  BIAS_CHEMISTRY_SPC( NC,ISP2 ) 
     &                                                   .AND.        BIAS_CHEMISTRY_SPC( NC,ISP3 ) )
                      THREE_REACTANT_CASE( NC,NR,4 ) = (              BIAS_CHEMISTRY_SPC( NC,ISP1 ) 
     &                                                   .AND.        BIAS_CHEMISTRY_SPC( NC,ISP2 ) 
     &                                                   .AND. .NOT.  BIAS_CHEMISTRY_SPC( NC,ISP3 ) )
                      THREE_REACTANT_CASE( NC,NR,5 ) = (              BIAS_CHEMISTRY_SPC( NC,ISP1 ) 
     &                                                   .AND. .NOT.  BIAS_CHEMISTRY_SPC( NC,ISP2 ) 
     &                                                   .AND.        BIAS_CHEMISTRY_SPC( NC,ISP3 ) )
                      THREE_REACTANT_CASE( NC,NR,6 ) = (       .NOT.  BIAS_CHEMISTRY_SPC( NC,ISP1 ) 
     &                                                   .AND.        BIAS_CHEMISTRY_SPC( NC,ISP2 ) 
     &                                                   .AND.        BIAS_CHEMISTRY_SPC( NC,ISP3 ) )
                      THREE_REACTANT_CASE( NC,NR,7 ) = (       .NOT.  BIAS_CHEMISTRY_SPC( NC,ISP1 ) 
     &                                                   .AND. .NOT.  BIAS_CHEMISTRY_SPC( NC,ISP2 ) 
     &                                                   .AND. .NOT.  BIAS_CHEMISTRY_SPC( NC,ISP3 ) )
     &                                             .OR. (      BIAS_CHEMISTRY_SPC( NC,ISP1 ) 
     &                                                   .AND. BIAS_CHEMISTRY_SPC( NC,ISP2 ) 
     &                                                   .AND. BIAS_CHEMISTRY_SPC( NC,ISP3 ) )      
              END SELECT
             END DO
         END DO
C...Assume that ISAM_SPECIES is fixed over the domain and simulation        
         WHERE ( ISAM_SPECIES ) 
            NOT_OUTSIDE_ISAM = 1.0D0
         ELSE WHERE
            NOT_OUTSIDE_ISAM = 0.0D0
         END WHERE            
         DO ISP2 = 1, NUMB_MECH_SPC
            IF( .NOT. ISAM_SPECIES( ISP2 ) )THEN            
              DO KTAG = 1, NTAG_SA
                NOT_ISAM_SOURCE( KTAG,ISP2 ) = 1.0D0
              END DO
            END IF
         END DO            
      END IF ! LFIRST
      UPDATE_PROBABILITIES = .TRUE.
      
      IF( LSTART )THEN
!         UPDATE_SOLD = .FALSE.
         SOURCE_DELTA = 0.0D0
         DO NIRR = 1, NUMB_MECH_SPC 
            YCOLD( NIRR ) = CONC( NIRR )
         END DO
         YCMID = YCOLD
         RETURN
      END IF
      
C...filter bulk concentrations for concentrations used for reaction rates   
      WHERE ( CONC .GE. BULK_CUTOFF )
         YCRATES = CONC
      ELSE WHERE
         YCRATES = 0.0D0
      END WHERE   

C...check for bad values
         TOTAL_ISAM_CONC = 0.0D0 ! 1.0D-40
         DO ISP2 = 1, NUMB_MECH_SPC
            DO KTAG = 1, NTAG_SA
               TOTAL_ISAM_CONC( ISP2 ) = TOTAL_ISAM_CONC( ISP2 )
     &                                 + SOLD( KTAG,ISP2 )
               IF( SOLD( KTAG,ISP2 ) .LT. 0.0D0 )
     &         WRITE(ISAM_LOG,*)TRIM(CHEMISTRY_SPC(ISP2)) // ' bad value = ',SOLD( KTAG,ISP2 )
            END DO
         END DO
C..compute species fractions from ISAM groups and the remainder group for all mechanism species
!  Note that remainder group (NTAG_SA+1) exists to handle untracked model species 
      DO ISP2 = 1, NUMB_MECH_SPC
         IF( ISAM_SPECIES( ISP2 ) )THEN          
           ONE_OVER_CONC   = 1.0D0 / MAX( TOTAL_ISAM_CONC( ISP2 ), DCONMIN_TAG )
           DO KTAG = 1, NTAG_SA
             ISAM_SOURCE( KTAG,ISP2 )     = SOLD( KTAG,ISP2 )*ONE_OVER_CONC
             NOT_ISAM_SOURCE( KTAG,ISP2 ) = MAX( 0.0D0, 1.0D0-ISAM_SOURCE( KTAG,ISP2 ))
           END DO
           OUTSIDE( ISP2 ) = 0.0D0    
           NOT_OUTSIDE_ISAM( ISP2 ) = 1.0D0
         ELSE
           ONE_OVER_CONC   = 1.0D0 / MAX( YCMID( ISP2 ), DCONMIN_TAG )
           DO KTAG = 1, NTAG_SA
             ISAM_SOURCE( KTAG,ISP2 )     = SOLD( KTAG,ISP2 )*ONE_OVER_CONC
             NOT_ISAM_SOURCE( KTAG,ISP2 ) = MAX( 0.0D0, 1.0D0-ISAM_SOURCE( KTAG,ISP2 ))
           END DO
           IF ( TOTAL_ISAM_CONC( ISP2 ) .GT. DCONMIN_TAG ) THEN
              TERM = MAX( (YCMID( ISP2 )-TOTAL_ISAM_CONC( ISP2 )), 0.0D0 )
           ELSE
              TERM =  YCMID( ISP2 )
           END IF
           OUTSIDE( ISP2 ) = TERM    
           NOT_OUTSIDE_ISAM( ISP2 ) = MAX( 1.0D0 - TERM * ONE_OVER_CONC, 0.0D0 )
         END IF
         DO KTAG = 1, NTAG_SA
           IF( SOLD( KTAG,ISP2 ) .GE. DCONMIN_TAG )THEN
              SOURCE_NONZERO(  KTAG,ISP2 ) = .TRUE.
           ELSE
              SOURCE_NONZERO(  KTAG,ISP2 ) = .FALSE.
           END IF  
        END DO
      END DO 

C...Calculate reaction rates and source probabilties
        IF ( ISAM_CHEM_BIAS .EQ. 5 ) THEN
!  for concentrations 0.35 is average of range in 
!      Sillman et al. (1998). J. Geophys. Res., 103, 22,629–22,644, 
!  but Need to add routine that HNO2 and H2O2 production based MECHANISM_BUDGET array  
!            
! determine if net production of ozone takes place
!           IF ( (CONC( O3_INDEX )-YCMID( O3_INDEX )) .GT. 0.0D0 ) THEN 
! Using net change in H2O2 and HNO3 concentrations
!              DELTA_H2O2 = CONC( H2O2_INDEX )-YCMID( H2O2_INDEX )
!              DELTA_HNO3 = CONC( HNO3_INDEX )-YCMID( HNO3_INDEX )
!              IF ( DELTA_H2O2/MAX( 1.0D-30,DELTA_NO3 ) .LT. VOC_NOX_TRANS  ) THEN
! Using H2O2 and HNO3 concentrations
!              IF ( CONC( H2O2_INDEX )/MAX( 1.0D-30,CONC( HNO3_INDEX ) ) .LT. VOC_NOX_TRANS  ) THEN
! Using gross production of H2O2 and HNO3
               VOC_TO_NOX = PH2O2_OVER_PHNO3( RK,CONC )
               IF ( VOC_TO_NOX .GT. NOX_VOC_SWITCH  ) THEN
!              IF ( PH2O2_OVER_PHNO3( RK,CONC ) .GT. REAL( VOC_NOX_TRANS,8 )  ) THEN
                  NC = ISAM_NOX_CASE
               ELSE 
                  NC = ISAM_VOC_CASE
               END IF

!           ELSE
!              NC = 3
!           END IF
            IF ( WRITE_CELL ) THEN

                WRITE(ISAM_LOG,*)'PH2O2_OVER_PHNO3, NC = ',PH2O2_OVER_PHNO3( RK,CONC ), NC

            END IF
        ELSE 
            NC = ISAM_CHEM_BIAS
            VOC_TO_NOX = NOX_VOC_SWITCH
        END IF
        DO NRX = 1, NRXNS
            NR = MAP_REACTANT_CASE( NRX )
            SELECT CASE ( NREACT( NRX ) )
               CASE( 1 )
                  ISP1 = IRR( NRX,1 )
                  RXRAT( NRX ) = RK( NRX )
     &                         * YCRATES( ISP1 )
                  DO KTAG = 1, NTAG_SA
                    SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 - NOT_ISAM_SOURCE( KTAG,ISP1 ) 
                  END DO
                  KTAG = NTAG_SA + 1
                  SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 - NOT_OUTSIDE_ISAM( ISP1 ) 
               CASE( 2 ) 
                   ISP1 = IRR( NRX,1 )
                   ISP2 = IRR( NRX,2 )
                   RXRAT( NRX ) = RK( NRX )
     &                          * YCRATES( ISP1 )
     &                          * YCRATES( ISP2 ) 
                   IF ( TWO_REACTANT_CASE( NC,NR,1 ) ) THEN
                       DO KTAG = 1, NTAG_SA
                         SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                                  -  NOT_ISAM_SOURCE( KTAG,ISP1 ) 
                       END DO
                       KTAG = NTAG_SA + 1
                       SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                                -  NOT_OUTSIDE_ISAM( ISP1 ) 
                   ELSE IF ( TWO_REACTANT_CASE( NC,NR,2 ) ) THEN
                       DO KTAG = 1, NTAG_SA
                         SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                                  -  NOT_ISAM_SOURCE( KTAG,ISP2 ) 
                       END DO
                       KTAG = NTAG_SA + 1
                       SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                                -  NOT_OUTSIDE_ISAM( ISP2 ) 
                   ELSE IF ( TWO_REACTANT_CASE( NC,NR,3 ) ) THEN
                       DO KTAG = 1, NTAG_SA
                         SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                                  -  NOT_ISAM_SOURCE( KTAG,ISP1 ) 
     &                                                  *  NOT_ISAM_SOURCE( KTAG,ISP2 ) 
                       END DO
                       KTAG = NTAG_SA + 1
                       SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                                -  NOT_OUTSIDE_ISAM( ISP1 ) 
     &                                                *  NOT_OUTSIDE_ISAM( ISP2 ) 

                   END IF
               CASE( 3 )
                   ISP1 = IRR( NRX,1 )
                   ISP2 = IRR( NRX,2 )
                   ISP3 = IRR( NRX,3 )
                   RXRAT( NRX ) = RK( NRX )
     &                          * YCRATES( ISP1 )
     &                          * YCRATES( ISP2 )
     &                          * YCRATES( ISP3 ) 
                   IF ( THREE_REACTANT_CASE( NC,NR,1 ) ) THEN
                       DO KTAG = 1, NTAG_SA
                         SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                                  -  NOT_ISAM_SOURCE( KTAG,ISP1 ) 
                       END DO
                       KTAG = NTAG_SA + 1
                       SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                                -  NOT_OUTSIDE_ISAM( ISP1 ) 
                   ELSE IF ( THREE_REACTANT_CASE( NC,NR,2 ) ) THEN
                       DO KTAG = 1, NTAG_SA
                         SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                                  -  NOT_ISAM_SOURCE( KTAG,ISP2 ) 
                       END DO
                       KTAG = NTAG_SA + 1
                       SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                                -  NOT_OUTSIDE_ISAM( ISP2 ) 
                   ELSE IF ( THREE_REACTANT_CASE( NC,NR,3 ) ) THEN
                       DO KTAG = 1, NTAG_SA
                         SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                                  -  NOT_ISAM_SOURCE( KTAG,ISP3 ) 
                       END DO
                       KTAG = NTAG_SA + 1
                       SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                                -  NOT_OUTSIDE_ISAM( ISP3 ) 
                   ELSE IF ( THREE_REACTANT_CASE( NC,NR,4 ) ) THEN
                       DO KTAG = 1, NTAG_SA
                         SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                                  -  NOT_ISAM_SOURCE( KTAG,ISP1 ) 
     &                                                  *  NOT_ISAM_SOURCE( KTAG,ISP2 ) 
                       END DO
                       KTAG = NTAG_SA + 1
                       SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                                *  NOT_OUTSIDE_ISAM( ISP1 ) 
     &                                                *  NOT_OUTSIDE_ISAM( ISP2 ) 

                   ELSE IF ( THREE_REACTANT_CASE( NC,NR,5 ) ) THEN
                       DO KTAG = 1, NTAG_SA
                         SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                                  -  NOT_ISAM_SOURCE( KTAG,ISP1 ) 
     &                                                  *  NOT_ISAM_SOURCE( KTAG,ISP3 ) 
                       END DO
                       KTAG = NTAG_SA + 1
                       SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                                *  NOT_OUTSIDE_ISAM( ISP1 ) 
     &                                                *  NOT_OUTSIDE_ISAM( ISP3 ) 
                   ELSE IF ( THREE_REACTANT_CASE( NC,NR,6 ) ) THEN
                       DO KTAG = 1, NTAG_SA
                         SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                                  -  NOT_ISAM_SOURCE( KTAG,ISP2 ) 
     &                                                  *  NOT_ISAM_SOURCE( KTAG,ISP3 ) 
                       END DO
                       KTAG = NTAG_SA + 1
                       SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                                *  NOT_OUTSIDE_ISAM( ISP1 ) 
     &                                                *  NOT_OUTSIDE_ISAM( ISP3 ) 
                   ELSE IF ( THREE_REACTANT_CASE( NC,NR,7 ) ) THEN
                        DO KTAG = 1, NTAG_SA
                          SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                                   -  NOT_ISAM_SOURCE( KTAG,ISP1 ) 
     &                                                   *  NOT_ISAM_SOURCE( KTAG,ISP2 ) 
     &                                                   *  NOT_ISAM_SOURCE( KTAG,ISP3 ) 
                        END DO
                        KTAG = NTAG_SA + 1
                        SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
     &                                                 -  NOT_OUTSIDE_ISAM( ISP1 ) 
     &                                                 *  NOT_OUTSIDE_ISAM( ISP2 ) 
     &                                                 *  NOT_OUTSIDE_ISAM( ISP3 ) 
                   END IF
               CASE( 0 ) 
                  RXRAT( NRX ) = RK( NRX )
                  DO KTAG = 1, NTAG_SA
                     SOURCE_PROBABILITY( KTAG,NRX ) = 0.0D0 
                  END DO
                  KTAG = NTAG_SA + 1
                  SOURCE_PROBABILITY( KTAG,NRX ) = 1.0D0 
            END SELECT
C..Normalize sources probabilities for reaction
            ISAM_TOTAL_PROBABILITY  =  0.0D0
            DO KTAG = 1, NTAG_SA
               SOURCE_PROBABILITY( KTAG,NRX ) = MAX( SOURCE_PROBABILITY( KTAG,NRX ), 0.0D0 )
               ISAM_PROBABILITY  ( KTAG,NRX ) = SOURCE_PROBABILITY( KTAG,NRX )
               ISAM_TOTAL_PROBABILITY = ISAM_TOTAL_PROBABILITY + ISAM_PROBABILITY( KTAG,NRX )
            END DO
            KTAG = NTAG_SA + 1
            SOURCE_PROBABILITY( KTAG,NRX ) = MAX( SOURCE_PROBABILITY( KTAG,NRX ), 0.0D0 )
            TOTAL_PROBABILITY = ISAM_TOTAL_PROBABILITY + SOURCE_PROBABILITY( KTAG,NRX )
            IF( ISAM_TOTAL_PROBABILITY .LE. 1.0D-30 )THEN
                ZERO_ISAM( NRX ) = .TRUE. 
                DO KTAG = 1, NTAG_SA
                   ISAM_PROBABILITY( KTAG,NRX ) = 0.0D0
                END DO
!               ISAM_PROBABILITY( OTHRTAG,NRX ) = 1.0D0
            ELSE
                ZERO_ISAM( NRX ) = .FALSE.
                ISAM_TOTAL_PROBABILITY = 1.0D0 / ISAM_TOTAL_PROBABILITY
                DO KTAG = 1, NTAG_SA
                   ISAM_PROBABILITY( KTAG,NRX ) = ISAM_PROBABILITY( KTAG,NRX )
     &                                          * ISAM_TOTAL_PROBABILITY
                END DO
            END IF    
            IF( TOTAL_PROBABILITY .LT. 0.0D0 )THEN
               DO JSPC = 1, ISAM_CHEMISTRY_SPC
                  S   = ISAM_TO_CHEM( JSPC )
                  SPC = ISAM_SPC_MAP( JSPC ) 
                  WRITE(ISAM_LOG,'(A16,86(1X,ES12.4))')CHEMISTRY_SPC(S),(SOLD( KTAG, S ),KTAG=1,NTAG_SA ),
     &            YCOLD( S )
               END DO ! loop jspc
               MSG = 'Fraction Results, note that last column is total isam over total species CONCentration'
               WRITE(ISAM_LOG,'(A)')TRIM(MSG)
               WRITE(ISAM_LOG,'(A16,86(1X,I12))')'Species/Tag #',(KTAG,KTAG=1,NTAG_SA+1)
               DO ISP2 = 1, NUMB_MECH_SPC
                  WRITE(ISAM_LOG,'(A16,86(1X,ES12.4))')CHEMISTRY_SPC(ISP2),
     &           (1.0D0-NOT_ISAM_SOURCE( KTAG,ISP2 ),KTAG=1,NTAG_SA),1.0D0-NOT_OUTSIDE_ISAM( ISP2 )
               END DO

               MSG = 'Unnormalized Source Probabilities, note that last column is for NonISAM CONCentrations'
               WRITE(ISAM_LOG,'(A)')TRIM(MSG)
               WRITE(ISAM_LOG,'(A16,86(1X,I12))')'Reaction/Tag #',(KTAG,KTAG=1,NTAG_SA+1)
               WRITE(ISAM_LOG,'(A16,86(1X,ES12.4))')RXLABEL( NRX ),
     &         (SOURCE_PROBABILITY( KTAG,NRX ),KTAG=1,NTAG_SA+1)

               MSG = 'TOTAL_PROBABILITY < zero from reaction label: ' 
     &             // TRIM( RXLABEL( NRX ) )
               CALL M3EXIT ( 'SA_IRR', 0, 0, MSG, XSTAT2 )
            ELSE IF( TOTAL_PROBABILITY .EQ. 0.0D0 )THEN   
               TOTAL_PROBABILITY = 1.0D0
            END IF
            TOTAL_PROBABILITY = 1.0D0 / TOTAL_PROBABILITY
            DO KTAG = 1, NTAG_SA + 1
               SOURCE_PROBABILITY( KTAG,NRX ) = SOURCE_PROBABILITY( KTAG,NRX )
     &                                        * TOTAL_PROBABILITY
            END DO
        END DO
#ifdef verbose_isam
        IF( WRITE_CELL )THEN
           MSG = 'Calculated Source Probabilities, note that last column is for NonISAM concentrations'
           WRITE(ISAM_LOG,'(A)')TRIM(MSG)
           WRITE(ISAM_LOG,'(A16,86(1X,I12))')'Reaction/Tag #',(KTAG,KTAG=1,NTAG_SA+1)
           DO NRX = 1, NRXNS
              WRITE(ISAM_LOG,'(A16,86(1X,ES12.4))')RXLABEL( NRX ),
     &        (SOURCE_PROBABILITY( KTAG,NRX ),KTAG=1,NTAG_SA+1)
           END DO
           MSG = 'Caculated ISAM Probabilities, note that last column is their sum'
           WRITE(ISAM_LOG,'(A)')TRIM(MSG)
           WRITE(ISAM_LOG,'(A16,86(1X,I12))')'Reaction/Tag #',(KTAG,KTAG=1,NTAG_SA+1)
           DO NRX = 1, NRXNS
              ISAM_TOTAL_PROBABILITY = SUM( ISAM_PROBABILITY( 1:NTAG_SA,NRX ) )
              WRITE(ISAM_LOG,'(A16,86(1X,ES12.4))')RXLABEL( NRX ),
     &       (ISAM_PROBABILITY( KTAG,NRX ),KTAG=1,NTAG_SA),ISAM_TOTAL_PROBABILITY
           END DO
         END IF ! WRITE_CELL  
#endif         

C..Compute integrated reaction rates
      DO NRX = 1, NRXNS
          INTRXN( NRX ) = DELT * RXRAT( NRX )
      END DO

c..Compute change in source concentrations for updating source concentrations
        DO JSPC = 1, NUMB_MECH_SPC
! compute species total production and isam production 
             ISAM_PROD  = 0.0D0
             TOTAL_PROD = 0.0D0
             SA_DELTA   = 0.0D0
             DO NTERM = 1, MECHANISM_BUDGET( JSPC )%NRXNS_PROD
                NRX   = MECHANISM_BUDGET( JSPC )%IRXN_PROD( NTERM )
                COEFF = MECHANISM_BUDGET( JSPC )%COEFF_POS( NTERM )
     &                * INTRXN( NRX )
                TOTAL_PROD = TOTAL_PROD + COEFF
                IF( .NOT. ZERO_ISAM( NRX ) )THEN
                  DO KTAG = 1, NTAG_SA
                     TERM      = COEFF 
     &                         * MIN( (SOURCE_PROBABILITY( KTAG,NRX ) 
     &                         +       SOURCE_PROBABILITY((NTAG_SA+1),NRX )
     &                         *       ISAM_PROBABILITY  ( KTAG,NRX )),
     &                                 1.0D0 )
                     ISAM_PROD = ISAM_PROD + TERM
                     SA_DELTA( KTAG ) = SA_DELTA( KTAG ) + TERM
                  END DO                    
                END IF
             END DO

             IF( ISAM_PROD .GT. TOTAL_PROD )TOTAL_PROD = ISAM_PROD
             IF ( WRITE_CELL ) THEN
                IF ( JSPC .EQ. H2O2_INDEX ) THEN
                   H2O2_Production = TOTAL_PROD
                   write(isam_log,*)'H2O2 Production = ',TOTAL_PROD
                END IF
                IF ( JSPC .EQ. HNO3_INDEX ) THEN
                   HNO3_Production = TOTAL_PROD
                   write(isam_log,*)'HNO3 Production = ',TOTAL_PROD
                END IF

             END IF
! compute species loss
             ISAM_LOSS  = 0.0D0
             TOTAL_LOSS = 0.0D0
             SA_LOSS    = 0.0D0
             DO NTERM = 1, MECHANISM_BUDGET( JSPC )%NRXNS_LOSS
                NRX   = MECHANISM_BUDGET( JSPC )%IRXN_LOSS( NTERM )
                COEFF = MECHANISM_BUDGET( JSPC )%COEFF_NEG( NTERM )
     &                * INTRXN( NRX )
                TOTAL_LOSS = TOTAL_LOSS + INTRXN( NRX )
             END DO
             NIRR = JSPC
             DELTA_BULK        = (CONC( JSPC )-YCMID( JSPC ))
             REMOVED           = .FALSE. 
             REBALANCE( JSPC ) = .FALSE. 
             IF( TOTAL_PROD .GT. 1.0D-30 )THEN
! estimate net change concentration inside and outside source tags based on total loss
                 IF( TOTAL_LOSS .GT. 1.0D-30 )THEN
! correct outside and isam production/concentration based on the bulk average loss frequency times timestep
                    LOSS_FACT  =  TOTAL_LOSS / CONC( JSPC )
                    IF ( LOSS_FACT .LT. 14.0D0 ) THEN ! > one millionth of initial concentration remains
!                    IF ( LOSS_FACT .LT. 34.5D0 ) THEN ! > one millionth of initial concentration remains
                       REBALANCE( JSPC ) = .TRUE.
                       TERM              = ( 1.0D0 - EXP( -LOSS_FACT ) )
                       COEFF             = TERM / LOSS_FACT
                       TOTAL_PROD        = TOTAL_PROD * COEFF                    
                       DO KTAG = 1, NTAG_SA
                          SA_LOSS ( KTAG )          = SOLD( KTAG,JSPC ) * TERM
                          SA_DELTA( KTAG )          = SA_DELTA(  KTAG ) * COEFF
                          SOURCE_DELTA( KTAG,JSPC ) = MAX( SA_DELTA( KTAG ) - SA_LOSS( KTAG ),
     &                                                    -SOLD( KTAG,JSPC ) )
                       END DO
                       ISAM_PROD  = SUM( SA_DELTA( 1:NTAG_SA ) )
! estimate changes in outside concentration based on surplus production 
                       OUTSIDE( JSPC ) = OUTSIDE( JSPC ) * ( 1.0D0 - TERM )
     &                                 + (TOTAL_PROD-ISAM_PROD) 
                       OUTSIDE( JSPC ) = MAX(  OUTSIDE( JSPC ),0.0D0 )  
                    ELSE 
! tag change removes initial value and apportions bulk concentration to the tag based on its relative production
                       REMOVED = .TRUE.
                       COEFF = CONC( JSPC ) / MAX( TOTAL_PROD,1.0D-30 )
                       DO KTAG = 1, NTAG_SA
                          SOURCE_DELTA( KTAG,JSPC ) = SA_DELTA( KTAG )*COEFF - SOLD( KTAG,JSPC )
                       END DO   
                       OUTSIDE( JSPC ) = COEFF * MAX( TOTAL_PROD-ISAM_PROD,0.0D0 ) 
                     END IF  
                 ELSE
! simply scale tag changes based their fraction of production
                    LOSS_FACT  = -1.0D0
                    COEFF      = DELTA_BULK / MAX( TOTAL_PROD,1.0D-30 )
                    DO KTAG = 1, NTAG_SA
                       SOURCE_DELTA( KTAG,JSPC ) = SA_DELTA(  KTAG ) * COEFF
                    END DO
! estimate changes in outside concentration based on surplus production and loss
                    OUTSIDE( JSPC ) = OUTSIDE( JSPC ) 
     &                              + COEFF * MAX( TOTAL_PROD-ISAM_PROD, 0.0D0 )
                 END IF     
! add surplus concentration to tags based on whether species is tracked
                 IF( ISAM_SPECIES( JSPC ) )THEN
                    IF ( ISAM_PROD .GT. 1.0D-30 ) THEN
! add outside concentration to tag based on their production relative to the isam total
                       TERM = 1.0D0 / ISAM_PROD
                       DO KTAG = 1, NTAG_SA
                          SA_DELTA( KTAG ) = TERM * SA_DELTA( KTAG )
                          SOURCE_DELTA( KTAG,JSPC ) = SOURCE_DELTA( KTAG,JSPC )
     &                                              + OUTSIDE( JSPC ) * SA_DELTA( KTAG )
                       END DO
                    ELSE  ! add to other tag
                       TERM = 1.0D0
                       SOURCE_DELTA( OTHRTAG,JSPC ) = SOURCE_DELTA( OTHRTAG,JSPC )
     &                                              + OUTSIDE( JSPC )                     
                    END IF                    
                 END IF  
!                 WRITE( LOGDEV,'(A,6(ES20.10,1X))')'Surplus Changes for ' // CHEMISTRY_SPC( JSPC ) // ' = ',
!     &           TOTAL_PROD,TOTAL_LOSS,OUTSIDE( JSPC ),COEFF,SUM( SOURCE_DELTA( :,JSPC ) ),ISAM_LOSS
#ifdef verbose_isam
                 IF( ISAM_SPECIES( JSPC ) )THEN
                    IF( REMOVED )DELTA_BULK = CONC( JSPC )
                    IF( WRITE_CELL )WRITE(ISAM_LOG,'(2A,30(1X,ES20.10))', ADVANCE = 'NO')MECHANISM_BUDGET( JSPC )%SPECIES_NAME,
     &              ' P ',-LOSS_FACT,(SOURCE_DELTA( KTAG,JSPC ),KTAG =1, NTAG_SA), OUTSIDE( JSPC ), TERM, DELTA_BULK                 
                 END IF
#endif     
                 IF( ISAM_SPECIES( JSPC ) ) OUTSIDE( JSPC ) = 0.0D0
             ELSE !!!!IF ( DELTA_BULK .LT. 0.0D0 ) THEN
! scale by relative change in bulk and fraction of species in tag             
               LOSS_FACT = MAX( -1.0D0, DELTA_BULK / MAX( YCMID( JSPC ),DCONMIN_TAG ) ) 
               DO KTAG = 1, NTAG_SA
                  SOURCE_DELTA( KTAG,JSPC ) = SOURCE_DELTA( KTAG, JSPC )
     &                                      + SOLD( KTAG,JSPC ) * LOSS_FACT
               END DO
!  have to add lines for outside change?
#ifdef verbose_isam
                 IF( ISAM_SPECIES( JSPC ) )THEN
                    IF( WRITE_CELL )WRITE(ISAM_LOG,'(2A,30(1X,ES20.10))',ADVANCE = 'NO')MECHANISM_BUDGET( JSPC )%SPECIES_NAME,
     &              ' L ',-LOSS_FACT,(SOURCE_DELTA( KTAG,JSPC ),KTAG =1, NTAG_SA),OUTSIDE( JSPC ),COEFF,
     &              DELTA_BULK            
                 END IF
#endif     
             END IF
!
#ifdef verbose_isam
             IF( ISAM_SPECIES( JSPC ) )THEN
                TOTAL = SUM( SOURCE_DELTA( 1:NTAG_SA,JSPC ))
                IF( WRITE_CELL )WRITE(ISAM_LOG,'(A,ES12.4,A,ES12.4)')
     &          ' Net ISAM Change ', TOTAL,': Change ISAM-BULK ',( TOTAL-DELTA_BULK )
             END IF
#endif
             DO KTAG = 1, NTAG_SA
                IF( SOURCE_DELTA( KTAG, JSPC ) .LT. -SOLD( KTAG, JSPC ) )THEN
                   SOURCE_DELTA( KTAG, JSPC ) = -SOLD( KTAG, JSPC )
                END IF 
             END DO   
       END DO

       IF ( WRITE_CELL ) THEN            
                   write(isam_log,'(A,2(ES12.4,1X))')
     &            'H2O2_Production/HNO3_Production, PH2O2_OVER_PHNO3  = ',
     &             H2O2_Production/max(HNO3_Production,1.0d-30) ,PH2O2_OVER_PHNO3( RK,CONC )
       END IF

C...update SOLD using SOURCE_DELTA       
        DO JSPC = 1,NUMB_MECH_SPC
           NIRR = JSPC 
           DO KTAG = 1, NTAG_SA
              SOLD( KTAG, JSPC ) = SOLD( KTAG, JSPC ) + SOURCE_DELTA( KTAG, JSPC )
               IF( SOLD( KTAG,JSPC ) .LT. DCONMIN_TAG )SOLD( KTAG,JSPC ) = 0.0D0
           END DO           
           IF( REBALANCE( JSPC ) )THEN 
!     adjust tags based on final bulk concentration
              TERM  = MAX( OUTSIDE( JSPC ) + SUM( SOLD(1:NTAG_SA,JSPC) ), DCONMIN_TAG ) 
              COEFF = CONC( JSPC ) / TERM            
              DO KTAG = 1, NTAG_SA
                 SOLD( KTAG,JSPC ) = COEFF * SOLD( KTAG,JSPC )
                 IF( SOLD( KTAG,JSPC ) .LT. DCONMIN_TAG )SOLD( KTAG,JSPC ) = 0.0D0
              END DO
              OUTSIDE( JSPC ) = COEFF * OUTSIDE ( JSPC )
           END IF
! set tags to conmin if initially nonzero
           IF( MAXVAL( SOLD( 1:NTAG_SA,JSPC ) ) .LT. DCONMIN_TAG )THEN
              DO KTAG = 1, NTAG_SA
                 IF( SOURCE_NONZERO( KTAG,JSPC ) )SOLD( KTAG,JSPC ) = DCONMIN_TAG
              END DO
! zero out ICON tag
!             SOLD( NTAG_SA,JSPC )   = 0.0D0
           END IF    
        END DO

#ifdef verbose_isam
       IF( WRITE_CELL )THEN
          WRITE(ISAM_LOG,'(A)')'Source Delta Concentration include extra delta then last two columns, Solver Change and Sum Deltas'
          WRITE(ISAM_LOG,'(A16,86(1X,I12))')'Species/Tag #',(KTAG,KTAG=1,NTAG_SA),0,NTAG_SA+1
!          DO S = 1, NUMB_MECH_SPC 
          DO JSPC = 1, ISAM_CHEMISTRY_SPC
             S = ISAM_TO_CHEM( JSPC )
             WRITE(ISAM_LOG,'(A16,86(1X,ES12.4))')CHEMISTRY_SPC(S),(SOURCE_DELTA( KTAG, JSPC ),KTAG=1,NTAG_SA ),
     &      (CONC( S )-YCMID( S )),SUM(SOURCE_DELTA( 1:(NTAG_SA), JSPC ))
          END DO
          WRITE(ISAM_LOG,*)' '
          WRITE(ISAM_LOG,'(A21,5(1X,A12))')'ISAM_CHEMISTRY_SPC,','  IRR Conc,  ',' True Conc,  correction ',
     &                                    '  IRR Change,  ','  True Change  ',' Difference '
!          DO NIRR = 1, NUMB_MECH_SPC 
          DO JSPC = 1, ISAM_CHEMISTRY_SPC
             NIRR = ISAM_TO_CHEM( JSPC )
             COEFF = 0.0D0
             TERM  = 0.0D0
             DO KTAG = 1, NTAG_SA
!                SOLD( KTAG, NIRR ) = MAX( SOLD( KTAG, NIRR ) + SOURCE_DELTA( KTAG, JSPC ), 0.0D0 )
                TERM  = TERM  + SOLD( KTAG, NIRR )
                COEFF = COEFF + SOURCE_DELTA( KTAG, JSPC )
             END DO
             DELTA_BULK =  CONC( NIRR )- YCMID( NIRR )             
             WRITE(ISAM_LOG,'(A21,5(1X,",",ES12.4))')'ISAM_' // CHEMISTRY_SPC(NIRR),
     &       MAX(0.0D0,TERM),CONC( NIRR ),COEFF,DELTA_BULK,(DELTA_BULK-COEFF)
          END DO
       END IF
#endif

       DO NIRR = 1, NUMB_MECH_SPC 
          YCMID( NIRR ) = CONC( NIRR )
       END DO
          
C..Clear source deltas
       SOURCE_DELTA = 0.0D0
C..Save concentrations
       DO NIRR = 1, NUMB_MECH_SPC 
          YCOLD( NIRR ) = CONC( NIRR )
       END DO
#ifdef verbose_isam
       IF( WRITE_CELL )THEN
          WRITE(ISAM_LOG,'(A,I7,1X,I6.6,1X,I3)')'At Date, Time, Nstep = ',ISAM_JDATE,ISAM_JTIME,ISAM_NSTEP
          WRITE(ISAM_LOG,*)'Final Source Concentration then New Total (Last Column) Concentrations'
          WRITE(ISAM_LOG,'(A16,86(1X,I12))')'Species/Tag #',(KTAG,KTAG=1,NTAG_SA),0,NTAG_SA+1
!          DO S = 1, NUMB_MECH_SPC 
          DO JSPC = 1, ISAM_CHEMISTRY_SPC
             S = ISAM_TO_CHEM( JSPC )
             WRITE(ISAM_LOG,'(A16,86(1X,ES12.4))')CHEMISTRY_SPC(S),(SOLD( KTAG, S ),KTAG=1,NTAG_SA ),
     &       CONC( S ),SUM(SOLD(1:NTAG_SA,S) )
          END DO
       END IF
#endif        
      

      RETURN
      END SUBROUTINE SA_IRR_UNBLOCKED
      REAL( 8 ) FUNCTION PH2O2_OVER_PHNO3( RKI,CONC )
      
      USE UTILIO_DEFN
      USE RXNS_DATA             ! chemical mechanism data

      IMPLICIT NONE 

C..Includes: None
      
C..Arguments: 
      REAL( 8 ),    INTENT( IN ) :: RKI  ( : )    ! Reaction rate coefficients
      REAL( 8 ),    INTENT( IN ) :: CONC ( : )    ! species concentrations

C..Parameters: None

C..Local
      INTEGER       :: IRXN, ISTART, ISTOP
      INTEGER       :: NRX
      INTEGER       :: IOS
      INTEGER       :: ISP1, ISP2, ISP3

      INTEGER, SAVE :: NRXNS_PROD_H2O2_HNO3 = 0

      REAL( 8 )     :: PROD_H2O2
      REAL( 8 )     :: PROD_HNO3
      
      LOGICAL, SAVE :: FIRSTCALL = .TRUE.
      LOGICAL, SAVE :: NO_SPECIES_PROD = .FALSE.
      
      REAL( 8 ), ALLOCATABLE, SAVE :: RXRAT( : )

      CHARACTER( 132 ) :: MSG
      IF ( FIRSTCALL ) THEN 
           
         IF ( NRXNS_PROD_HNO3 .LT. 1 .OR. NRXNS_PROD_H2O2 .LT. 1 ) THEN
            NO_SPECIES_PROD = .TRUE.
         ELSE 
            NRXNS_PROD_H2O2_HNO3 = NRXNS_PROD_HNO3 + NRXNS_PROD_H2O2
            ALLOCATE( RXRAT( NRXNS_PROD_H2O2_HNO3 ),  STAT = IOS )
            IF ( IOS .NE. 0 ) THEN
                MSG = 'Error allocating RXRAT arrays'
                CALL M3EXIT( ' PH2O2_OVER_PHNO3', 0, 0, MSG, XSTAT1 )
            END IF  
          END IF
                   
         FIRSTCALL = .FALSE.
      END IF

      IF ( NO_SPECIES_PROD ) THEN
         PH2O2_OVER_PHNO3 = CONC( H2O2_INDEX ) / MAX( 1.0D-30,CONC( HNO3_INDEX ))
         RETURN
      END IF
      
      DO IRXN = 1, NRXNS_PROD_H2O2_HNO3
            NRX = H2O2_HNO3_PROD_RXNS( IRXN )
            SELECT CASE ( NREACT( NRX ) )
               CASE( 1 )
                  ISP1 = IRR( NRX,1 )
                  RXRAT(IRXN ) = RKI( NRX )
     &                         * CONC( ISP1 )
               CASE( 2 ) 
                   ISP1 = IRR( NRX,1 )
                   ISP2 = IRR( NRX,2 )
                   RXRAT( IRXN ) = RKI( NRX )
     &                          * CONC( ISP1 )
     &                          * CONC( ISP2 ) 
               CASE( 3 )
                   ISP1 = IRR( NRX,1 )
                   ISP2 = IRR( NRX,2 )
                   ISP3 = IRR( NRX,3 )
                   RXRAT( IRXN ) = RKI( NRX )
     &                          * CONC( ISP1 )
     &                          * CONC( ISP2 )
     &                          * CONC( ISP3 ) 
               CASE( 0 ) 
                  RXRAT( IRXN ) = RKI( NRX )
            END SELECT
      END DO

! Compute hydrogen peroxide production
      PROD_H2O2 = 0.0D0
      ISTART = 1
      ISTOP  = NRXNS_PROD_H2O2
      DO IRXN = ISTART, ISTOP
         PROD_H2O2 = PROD_H2O2
     &             + H2O2_HNO3_YIELD( IRXN ) * RXRAT( IRXN )
      END DO

! Compute hydrogen peroxide production
      PROD_HNO3 = 0.0D0
      ISTART = ISTOP + 1
      ISTOP  = NRXNS_PROD_H2O2_HNO3
      DO IRXN = ISTART, ISTOP
         PROD_HNO3 = PROD_HNO3
     &             + H2O2_HNO3_YIELD( IRXN ) * RXRAT( IRXN )
      END DO
      
      PH2O2_OVER_PHNO3 = PROD_H2O2 / MAX( 1.0D-30,PROD_HNO3 )
      
      END FUNCTION PH2O2_OVER_PHNO3
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE DFILTER_AND_RENORMAL( SIZE_XARRAY, XARRAY, FILTER, MAX_REMOVAL, SUCCESS )

            IMPLICIT NONE

! function set to zero values in a one dimensional array of real(8) variables, XARRAY,
! that are less than a requested value, FILTER. Next, the XARRAY is normalized so its new
! sum of values equals the original summation. However, the process is abandomed if sum of removed
! values is greater than MAX_REMOVE percent.

            INTEGER, INTENT( IN )    :: SIZE_XARRAY ! length of XARRAY
            REAL(8), INTENT( INOUT ) :: XARRAY( : ) ! real array to be filter
            REAL(8), INTENT( IN )    :: FILTER      ! test value for setting array values to zero
            REAL(8), INTENT( IN )    :: MAX_REMOVAL ! limit on sum removed values. Process abandoned if exceeded.
            LOGICAL, INTENT( INOUT ) :: SUCCESS     ! whether process is accomplished.

! local:
            REAL(8) :: SUM_INITIAL
            REAL(8) :: SUM_FINAL
            REAL(8) :: RENORMALIZE
            INTEGER :: IX

            SUCCESS = .TRUE.
! Maximum removal is not zero
            IF ( MAX_REMOVAL .LE. 1.0D-30 ) THEN
              SUCCESS = .FALSE.
              RETURN
            END IF

            SUM_INITIAL = SUM( XARRAY )
            SUM_FINAL   = SUM( XARRAY, (XARRAY .GT. FILTER) )

            RENORMALIZE = SUM_FINAL / MAX( SUM_INITIAL,1.0D-30 )
! test limit is exceeded
            IF ( RENORMALIZE .LT. MAX_REMOVAL ) THEN
              SUCCESS = .FALSE.
              RETURN
            END IF
! filter array
            DO IX = 1,SIZE_XARRAY
              IF ( XARRAY( IX ) .LE. FILTER ) XARRAY( IX ) = 0.0D0
            END DO
! renormalize array values
            RENORMALIZE = 1.0D0 / RENORMALIZE
            XARRAY = RENORMALIZE * XARRAY
            SUCCESS = .TRUE.

            RETURN
       END SUBROUTINE DFILTER_AND_RENORMAL
       SUBROUTINE SFILTER_AND_RENORMAL( SIZE_XARRAY, XARRAY, FILTER, MAX_REMOVAL, SUCCESS )

            IMPLICIT NONE

! function set to zero values in a one dimensional array of real(8) variables, XARRAY,
! that are less than a requested value, FILTER. Next, the XARRAY is normalized so its new
! sum of values equals the original summation. However, the process is abandomed if sum of removed
! values is greater than MAX_REMOVE percent.

            INTEGER, INTENT( IN )    :: SIZE_XARRAY ! length of XARRAY
            REAL,    INTENT( INOUT ) :: XARRAY( : ) ! real array to be filter
            REAL,    INTENT( IN )    :: FILTER      ! test value for setting array values to zero
            REAL,    INTENT( IN )    :: MAX_REMOVAL ! limit on sum removed values. Process abandoned if exceeded.
            LOGICAL, INTENT( INOUT ) :: SUCCESS     ! whether process is accomplished.

! local:
            REAL    :: SUM_INITIAL
            REAL    :: SUM_FINAL
            REAL    :: RENORMALIZE
            INTEGER :: IX

! Maximum removal is not zero
            IF ( MAX_REMOVAL .LE. 1.0E-30 ) THEN
              SUCCESS = .FALSE.
              RETURN
            END IF

            SUM_INITIAL = SUM( XARRAY )
            SUM_FINAL   = SUM( XARRAY, (XARRAY .GT. FILTER) )

            RENORMALIZE = SUM_FINAL / MAX( SUM_INITIAL,1.0E-30 )
! test limit is exceeded
            IF ( RENORMALIZE .LT. MAX_REMOVAL ) THEN
              SUCCESS = .FALSE.
              RETURN
            END IF
! filter array
            DO IX = 1,SIZE_XARRAY
              IF ( XARRAY( IX ) .LE. FILTER ) XARRAY( IX ) = 0.0
            END DO
! renormalize array values
            RENORMALIZE = 1.0 / RENORMALIZE
            XARRAY = RENORMALIZE * XARRAY
            SUCCESS = .TRUE.

            RETURN
       END SUBROUTINE SFILTER_AND_RENORMAL 
       Subroutine SOLVE_ISAM_CHEM( REPLACE_SOLD, RKI, CBULK, DELT, OLD2NEW, NEW2OLD )

          USE GRID_CONF             ! horizontal & vertical domain specifications
          USE RXNS_DATA             ! chemical mechanism data
          USE MECHANISM_FUNCTIONS
          USE CGRID_SPCS            ! CGRID mechanism species
          USE SA_DEFN
          USE LU_DecompositionSolver
          USE LU_DecompositionSolver_LINPACK
          USE UTILIO_DEFN
          
          IMPLICIT NONE 
                    
          Character( 16 ), Save :: PNAME = 'SOLVE_ISAM_CHEM'
          
          LOGICAL,   Intent( In ) :: REPLACE_SOLD
          Real( 8 ), Intent( In ) :: RKI( : )
          Real( 8 ), Intent( In ) :: CBULK( : )
          Real( 8 ), Intent( In ) :: DELT
          Integer,   Optional, Intent( In ) :: OLD2NEW( :,: )
          Integer,   Optional, Intent( In ) :: NEW2OLD( :,: )
          
          Integer I,J,S,N
          Integer JROW, JCOL
          Integer INFO                            ! see s_lu.F
          
          Real(8) SUMAT
          Real(8) SUMSP
          Real(8) TOTAL
          Real(8) DIFF
          Real(8) DIFFSP
          
          Real KSTEP                              ! Holds k times timestep(in min)
          Real RXNMULT                            ! Holds product of concs of reactants
          
          Integer NRCT                            ! Counter over reactants
          Integer NPROD                           ! Counter over products
          Integer IREACT                          ! index for reaction reactant
          Integer IPROD                           ! index for reaction produce

C..Scratch Local Variables:
          INTEGER ISP1, ISP2, ISP3  ! Species indices
          INTEGER JSPC,  SPC        ! Species indices
          INTEGER NCELL             ! Loop index for cells
          INTEGER NOUT              ! IRR output index
          INTEGER NC,NR,NRX         ! Loop index for reactions
          INTEGER ASTAT             ! allocation status
          INTEGER KTAG              ! Loop index/pointer for source
          INTEGER RXN               ! Loop index/pointer for reaction
                                                     
C..Saved Local Variables:
      Integer,   Allocatable       :: SENS_INDEX( : )  ! CGRID_INDEX translated by JNEW2OLD
      Integer,   Allocatable       :: MECH_INDEX( : )  ! MECH_INDEX translated by JNEW2OLD

      REAL                         :: CHEMSTEP               ! single precision conversion of DELT
      REAL( 8 )                    :: RENORM_FACTOR          ! used to normal tag concentration wrt CBULK
      REAL( 8 ), ALLOCATABLE, SAVE :: YNEW   ( : )           ! bulk concentrations from previous time step
      REAL( 8 ), ALLOCATABLE, SAVE :: OUTSIDE( : )           ! species concentration outside isam tags
      REAL( 8 ), ALLOCATABLE, SAVE :: YCMID  ( : )           ! bulk concentrations from previous time step
      REAL( 8 ), ALLOCATABLE, SAVE :: YCRATES( : )           ! bulk concentration used to calculate reactionvrates 
      REAL,      ALLOCATABLE, SAVE :: SNEW   ( :,: )         ! change in source tag species 
      REAL,      ALLOCATABLE, SAVE :: IDENTITY_MATRIX( :,: ) ! change in source tag species 


      Logical, Save :: FIRSTIME = .TRUE.
      Logical, Save :: REORDER  = .TRUE. ! reorder CBULK
      LOGICAL       :: SUCCESS  = .TRUE. ! flag call to FILTER_AND_RENORMAL worked      

      Character( 96 ) :: XMSG = ' '

      IF ( FIRSTIME ) THEN

          FIRSTIME  = .FALSE.
!          LOGDEV    = INIT3()
         
          CALL SET_MECHANISM( ) ! determine formulas for Mechanism Jacobain and Species Rate of Change
  
C Allocate arrays:
          ALLOCATE( YNEW ( NUMB_MECH_SPC ), STAT = ASTAT )
          IF ( ASTAT .NE. 0 ) THEN
             XMSG = 'ERROR allocating YNEW variable'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
          END IF
          
          ALLOCATE( SNEW( NUMB_MECH_SPC,NTAG_SA ), STAT = ASTAT )
          IF ( ASTAT .NE. 0 ) THEN
             XMSG = 'ERROR allocating SNEW variable'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
          END IF
          
          ALLOCATE( IDENTITY_MATRIX( NUMB_MECH_SPC,NUMB_MECH_SPC ), STAT = ASTAT )
          IF ( ASTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating IDENTITY_MATRIX'
          CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
          END IF
          
          IDENTITY_MATRIX = 0.0
          DO JSPC = 1,NUMB_MECH_SPC
             IDENTITY_MATRIX( JSPC,JSPC ) = 1.0        
          END DO
          
          ALLOCATE( A( NUMB_MECH_SPC, NUMB_MECH_SPC ),
     &              A1( NUMB_MECH_SPC, NUMB_MECH_SPC ), STAT = ASTAT )
          IF ( ASTAT .NE. 0 ) THEN
             XMSG = 'Failure allocating A, A1'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
          END IF
          
          ALLOCATE( PDT( NUMB_MECH_SPC,NUMB_MECH_SPC ),
     &              IPVT ( NUMB_MECH_SPC ),
     &              PRD( NUMB_MECH_SPC ), STAT = ASTAT )
          IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating PRD, IPVT, PRD_RATE'
            CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
          END IF
          
          ALLOCATE( Real8A( NUMB_MECH_SPC, NUMB_MECH_SPC ),
     &              Real8A1( NUMB_MECH_SPC, NUMB_MECH_SPC ), STAT = ASTAT )
          IF ( ASTAT .NE. 0 ) THEN
             XMSG = 'Failure allocating Real8A, Real8A1'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
          END IF

          ALLOCATE( Real8PRD( NUMB_MECH_SPC ), STAT = ASTAT )
          IF ( ASTAT .NE. 0 ) THEN
             XMSG = 'Failure allocating Real8PRD'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
          END IF

          ALLOCATE( SENS_INDEX( NUMB_MECH_SPC ),
     &              MECH_INDEX( NUMB_MECH_SPC ), STAT = ASTAT )
          IF ( ASTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating SENS_INDEX or MECH_INDEX'
          CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
          END IF
          
          DO JSPC = 1,NUMB_MECH_SPC
             MECH_INDEX(JSPC) = CGRID_INDEX(JSPC)
             SENS_INDEX(JSPC) = CGRID_INDEX(JSPC)
          END DO

         YNEW = 0.0D0
         
         IF( PRESENT( OLD2NEW ) .AND. PRESENT( OLD2NEW ) )THEN

             REORDER = .FALSE. ! no reordering because YCDMM is already redeorderd by chemsolver
             
              PRINT*,"PRESENT( OLD2NEW ) .AND. PRESENT( OLD2NEW )"
!              DO I = 1,NUMB_MECH_SPC
!                 S  = NEW2OLD( I,1 )
!                 YNEW( S ) = CBULK( I )
!                 WRITE(LOGDEV,'(I4,A,A,ES16.4)')I,' ORIG: ' // CHEMISTRY_SPC(I),' = ',YNEW(S)
!              END DO
!              DO I = 1,NUMB_MECH_SPC
!                 S  = NEW2OLD( I,1 )
!                 WRITE(LOGDEV,'(I4,A,A,2(ES16.4,1X))')S,' SORT: ' // CHEMISTRY_SPC(S),' = ',CBULK( I ),YNEW(S)-CBULK( I )
!              END DO
!             CALL  EVALUATE_F_JAC_MECH( YNEW, SRK, PDT2 ) ! Evaluate Jacobian based on CBULK and SKR values

! overwrite reordering using chemsolver conversing maps

             JNEW2OLD(1:NUMB_MECH_SPC,1) = NEW2OLD(1:NUMB_MECH_SPC,1)
             JOLD2NEW(1:NUMB_MECH_SPC,1) = OLD2NEW(1:NUMB_MECH_SPC,1)
             RESET_JACOBIAN = .TRUE.
             DO RXN = 1, NRXNS
                DO NRCT = 1, NREACT( RXN )
                   IREACT = ISPECIES_REACTION( NRCT,RXN )
                   ISPECIES_REACTION( NRCT,RXN ) = OLD2NEW( IREACT,1 )
                END DO

                DO NPROD = 1, NPRDCT( RXN )
                   IPROD = ISPECIES_REACTION( NPROD+3,RXN )
                   ISPECIES_REACTION( NPROD+3,RXN ) = OLD2NEW( IPROD,1 )
               END DO
             END DO

!             DO S = 1, NUMB_MECH_SPC
!               MECH_INDEX(S) = CGRID_INDEX(JOLD2NEW(S,1))
!               SENS_INDEX(S) = CGRID_INDEX(JNEW2OLD(S,1))
!             END DO

         ELSE
!
!            DO S = 1, NUMB_MECH_SPC
!               MECH_INDEX(S) = CGRID_INDEX(JOLD2NEW(S,1))
!               SENS_INDEX(S) = CGRID_INDEX(JNEW2OLD(S,1))
!            END DO
!            DO I = 1,NUMB_MECH_SPC
!               S  = JOLD2NEW( I,1 )
!               YNEW( S ) = CBULK( I )
!               WRITE(LOGDEV,'(I4,1X,A,A,ES16.4)')I,' SORT: ' // CHEMISTRY_SPC(I),' = ',YNEW(S)
!            END DO
!            DO I = 1,NUMB_MECH_SPC
!               S  = JNEW2OLD( I,1 )
!               WRITE(LOGDEV,'(I4,1X,A,A,2(ES16.4,1X))')I,' ORIGINAL: ' // CHEMISTRY_SPC(S),' = ',CBULK( S ),YNEW(I)-CBULK( S )
!            END DO
!            CBULK = YNEW
         
         END IF
 
!             DO RXN = 1, NRXNS

!                WRITE(LOGDEV,'(A,1X,I4,1X,A)')'For reaction number and label,',RXN,
!     &                                         TRIM(RXLABEL(RXN))
!                WRITE(LOGDEV,*)'Reactants'
!                WRITE(LOGDEV,'(10X,40(A,", "))')
!     &         (CHEMISTRY_SPC(JNEW2OLD(ISPECIES_REACTION( NRCT,RXN ),1)),NRCT = 1, NREACT( RXN ))

!                WRITE(LOGDEV,*)'Products'
!                WRITE(LOGDEV,'(10X,40(A,", "))')
!     &         (CHEMISTRY_SPC(JNEW2OLD(ISPECIES_REACTION( NPROD+3,RXN ),1)),NPROD = 1, NPRDCT( RXN ))

!             END DO
!             WRITE(LOGDEV,*)'Original order: chemistry species, cgrid index'
!             DO S = 1, NUMB_MECH_SPC
!                WRITE(LOGDEV,'(A16,1X,I4)')CHEMISTRY_SPC(S),CGRID_INDEX(S)
!             END DO
!             WRITE(LOGDEV,*)'Sort ordered: chemistry species, cgrid index'
!             DO S = 1, NUMB_MECH_SPC
!                WRITE(LOGDEV,'(A16,1X,I4)')CHEMISTRY_SPC(JOLD2NEW(S,1)),MECH_INDEX(S)
!             END DO

      END IF
      
      IF ( REORDER ) THEN  ! reorder to speed-up LU decomposition of Jacobian

            DO I = 1,NUMB_MECH_SPC
               S  = JOLD2NEW( I,1 )
               YNEW( S ) = CBULK( I )
               IF( YNEW( S ) .LE. 1.0D-25 )YNEW( S ) = 0.0D0 
       !        WRITE(LOGDEV,'(I4,1X,A,A,2(ES16.4,1X))')I,
     & !        ' YAVE and Difference with its sorted value: ' // CHEMISTRY_SPC(S),
     & !        ' = ',CBULK( S ),YNEW(I)-CBULK( S )
            END DO
!            CBULK = YNEW
         
      ELSE
             DO I = 1,NUMB_MECH_SPC
               YNEW( I ) = CBULK( I )
       !       IF( YNEW( I ) .LE. 1.0D-25 )YNEW( I ) = 0.0D0 
       !       WRITE(LOGDEV,'(I4,1X,A,A,2(ES16.4,1X))')I,
     & !       ' YAVE for ' // CHEMISTRY_SPC(I),
     & !       ' = ',YNEW( I )
            END DO
      END IF
      
!     DO I = 1,NRXNS
!        write(6,'(I4,1X,"reaction:",A16," = ",ES12.4)')I,RXLABEL(I),RKI(I)
!     END DO

      CALL  EVALUATE_F_JAC_MECH( YNEW, RKI, PDT ) ! Evaluate Jacobian based on CBULK and SKR values

      CHEMSTEP = REAL( DELT,4 )
      DO 433 J = 1, NUMB_MECH_SPC
         DO 434 I = 1, NUMB_MECH_SPC

         ISP1 = JNEW2OLD( J,1 )
         ISP2 = JNEW2OLD( I,1 )

            A( I, J )  = 0.0
            A1( I, J ) = 0.0
            A( I, J )  = -0.5 * CHEMSTEP * PDT( I,J )
            A1( I, J ) =  0.5 * CHEMSTEP * PDT( I,J )
            IF ( I .EQ. J ) THEN
               A( I, J )  = 1.0 + A( I, J )
               A1( I, J ) = 1.0 + A1( I, J )
!          IF( I .GT. 1 .AND. I .LT. 4 )THEN      
!           write(6,'(A16," STEP: (1-dt/2*Jacobian)(",I4,",",I4,"),Jacobian(",I4,",",I4,") = ",2(ES18.6,A2))')
!     &     CHEMISTRY_SPC(ISP1),J,J,J,J,A(J,J),", ",-0.5 * CHEMSTEP * PDT(J,J)
!          END IF       
            END IF
            
!            Real8A1( I,J ) = Real( A1( I,J ),8 )
            Real8A( I,J )  = Real( A( I,J ),8 )
!           IF( TRIM( ADJUSTL(CHEMISTRY_SPC(ISP2)) ) .EQ. "O3" .AND. Real8A1( I,J ) .GT. 0.0D0 )THEN
!              write(6,'("1st:",I4,1X,A16," STEP: (1+dt/2*Jacobian)(",I4,",",I4,"),Jacobian(",I4,",",I4,") = ",2(ES18.6,A2))')
!    &         ISP1,CHEMISTRY_SPC(ISP2),I,J,I,J,A1(I,J),", ",PDT(I,J)                           
!           END IF
            
434      CONTINUE
433   CONTINUE

      !print*,"CHEMSTEP = ",CHEMSTEP
      !DO J = 1, NUMB_MECH_SPC
      !   S = JNEW2OLD( J,1 )
      !   write(6,'(A,":(1-dt/2*Jacobian)(",I4,",",I4,"),Jacobian(",I4,",",I4,") = ",2(ES18.6,A2))')
     & !   CHEMISTRY_SPC(S),J,J,J,J,A(J,J),", ",-0.5 * CHEMSTEP * PDT(J,J)
      !END DO


C Factor matrix A by Gaussian elimination
      INFO = 0
!      CALL SGEFA( A, NUMB_MECH_SPC, NUMB_MECH_SPC, IPVT, INFO )
       CALL DGEFA( Real8A, NUMB_MECH_SPC, NUMB_MECH_SPC, IPVT, INFO )
!      print*,"SGEFA: INFO = ",INFO
!       CALL  dludcmp_f90(Real8A,NUMB_MECH_SPC,IPVT,INFO)
       INFO = 0
       A = REAL( Real8A,4 )
      
       !DO J = 1, NUMB_MECH_SPC
       !  S = JNEW2OLD( J,1 )
       !  write(6,'(A,":Inverse(1-dt/2*Jacobian)(",I4,",",I4,") = ",2(ES18.6,1X))')
     & !   CHEMISTRY_SPC(S),J,J,A(J,J),Real8A(J,J)
       !END DO

!     DO J = 1, NUMB_MECH_SPC
!        S = JNEW2OLD( J,1 )
!        write(6,'(A,":Inverse(1-dt/2*Jacobian)(",I4,",",I4,") = ",(ES18.6,A2))')
!    &   CHEMISTRY_SPC(S),J,J,A(J,J)
!     END DO

      IF ( INFO .NE. 0 ) THEN
        XMSG = "Error Inverse of (Identity Matrix - 0.5*TimeStep*Jacobain has below zero(s) on diagonal. "
        WRITE(LOGDEV,'(A)')Trim(XMSG)
        DO J = 1, NUMB_MECH_SPC
          IF( A(J,J) .EQ. 0.0 .OR. Real8A(J,J) .LE. 0.0D0 )THEN
            S = JNEW2OLD( J,1 )
            write(6,'(A,":Inverse(1-dt/2*Jacobian)(",I4,",",I4,") = ",2(ES18.6,1X))')
     &      CHEMISTRY_SPC(S),J,J,A(J,J),Real8A(J,J)
          END IF
        END DO       
        XMSG = "Check Jacobian Matrix for bad values, especially the diagonal."
        WRITE(LOGDEV,'(A)')Trim(XMSG)
        XMSG = "Above Fatal Error in LU Decomposition occurred."
        CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF  

!      IF ( .NOT. REPLACE_SOLD ) THEN
      ! load SNEW for efficient computation using SOLD  
         DO JSPC = 1,NUMB_MECH_SPC
            J = JOLD2NEW( JSPC,1 )
            DO KTAG = 1, NTAG_SA
               SNEW( J,KTAG ) = REAL( SOLD( KTAG,JSPC ),4 )
!               IF( SOLD( KTAG,JSPC ) .GT. 1.0D-36 )THEN
!              IF( KTAG .EQ. 3 )THEN
!                 write(6,'(2(I4,1X),A,2(ES12.4,1X))')KTAG,JSPC,
!    &            "Initial Condition:"
!    &            // CHEMISTRY_SPC(JOLD2NEW(JSPC,1))
!    &            // ":SNEW( J,KTAG ),SOLD( KTAG,JSPC ) = ",
!    &            SNEW( J,KTAG ),SOLD( KTAG,JSPC )             
!              END IF
!               IF ( SNEW( J,KTAG ) .LT. 1.e-25 ) SNEW( J,KTAG ) = 0.0
            END DO
         END DO
!      ELSE
!         print*,".NOT. REPLACE_SOLD = FALSE"   
!      END IF
!      STOP

!      DO J = 1, NUMB_MECH_SPC
!        DO I = 1, NUMB_MECH_SPC
!            ISP2 = JNEW2OLD( I,1 )
!            IF( TRIM( ADJUSTL(CHEMISTRY_SPC(ISP2)) ) .EQ. "O3" .AND. Real8A1( J,I) .GT. 0.0D0 )THEN
!               write(6,'("2nd:",I4,1X,A16," STEP: (1+dt/2*Jacobian)(",I4,",",I4,"),Jacobian(",I4,",",I4,") = ",2(ES18.6,A2))')
!    &          ISP1,CHEMISTRY_SPC(ISP2),I,J,J,I,A1(I,J),", ",PDT(I,J)                           
!            END IF
!        END DO
!      END DO   

      ! Time Integration for SNEW at t+dt
      DO KTAG = 1,NTAG_SA
         ! Evaluate tag concentration vector times (Id+dt/2*Jac) matrix
         DO I = 1,NUMB_MECH_SPC
         ! Evaluate prd: prd = (Id+dt/2*Jac)*sold
            PRD( I ) = 0.0
!           Real8PRD( I ) = 0.0D0
            DO J = 1,NUMB_MECH_SPC
               PRD( I ) = PRD( I ) + A1( I, J ) * SNEW( J,KTAG )

               
!           ISP1 = JNEW2OLD( I,1 )
!           IF( TRIM( ADJUSTL(CHEMISTRY_SPC(ISP1)) ) .EQ. "O3" .AND. Real8A1( I,J ) .GT. 0.0D0 )THEN
!              write(6,'("2nd:",I4,1X,A16," STEP: (1+dt/2*Jacobian)(",I4,",",I4,"),Jacobian(",I4,",",I4,") = ",2(ES18.6,A2))')
!    &         ISP1,CHEMISTRY_SPC(ISP1),I,J,I,J,A1(I,J),", ",PDT(I,J)                           
!           END IF
            
            
!              IF( I .EQ. 1 .AND. A1( I, J ) .GT. 0.0 )THEN
!                 write(6,"(2(I4,1X),A,4(es12.4,1X))")I,J,
!    &            TRIM( CHEMISTRY_SPC(JOLD2NEW(I,1)) ) 
!    &            // ":PRD( I ),A1( I, J ),SNEW( I,KTAG ) = ", 
!    &            PRD( I ),A1( I,J ),SNEW( I,KTAG )              
!              END IF
            END DO
            Real8PRD( I ) =  REAL( PRD( I ),8 )
!           IF( SNEW( I,KTAG ) .GT. 1.0E-30 )THEN
!               write(6,"(2(I4,1X),A,4(es12.4,1X))")KTAG,I,
!    &         TRIM( CHEMISTRY_SPC(JOLD2NEW(I,1)) ) 
!    &         // ":PRD( I ),SNEW( I,KTAG ) = ", 
!    &            REAL8PRD( I ),SNEW( I,KTAG )
!           END IF
         END DO
!        STOP
         
         ! Solve tag concentrations at t+dt:Inverse(Id-dt/2*Jac) * prd
!         CALL SGESL( A, NUMB_MECH_SPC, NUMB_MECH_SPC, IPVT, PRD, 0 )
          CALL DGESL( Real8A, NUMB_MECH_SPC, NUMB_MECH_SPC, IPVT, Real8PRD, 0 )
!         CALL dlubksb_f90(Real8A,NUMB_MECH_SPC,IPVT,Real8PRD)
         DO I = 1,NUMB_MECH_SPC
!            SNEW( I,KTAG ) = MAX( PRD( I ),0.0 )
           SNEW( I,KTAG ) = MAX( REAL( Real8PRD( I ),4 ),0.0 )
         END DO 
      END DO  ! tag loop

      CALL FILTER_AND_RENORMAL( NTAG_SA, SNEW(1:NTAG_SA,J), 1.0E-25, 0.98, SUCCESS )
      ! Load Solution into SOLD using SNEW
      IF ( REPLACE_SOLD ) THEN
         DO JSPC = 1,NUMB_MECH_SPC
            J = JNEW2OLD( JSPC,1 )
            DO KTAG = 1, NTAG_SA
               SOLD( KTAG,J ) = REAL( SNEW( JSPC,KTAG ),8 )
               IF( SOLD( KTAG,JSPC ) .GT. 1.0D3 )THEN
                 write(6,"('Warning High Prediction:Tag Number,Species Number = ',2(I4,1X),A,ES12.4)")
     &           KTAG,J,CHEMISTRY_SPC(J) // "SOLD = ",SOLD( KTAG,J )
               END IF
        !       IF( KTAG .EQ. 3 )THEN
        !         write(6,"('Prediction:Tag Number,Species Number = ',2(I4,1X),A,2(ES12.4,1X))")
     &  !         KTAG,J,CHEMISTRY_SPC(J) // "SOLD = ",SOLD( KTAG,J ),YNEW(JSPC)
        !       END IF
            END DO           
            RENORM_FACTOR = CBULK(J)/MAX( SUM( SOLD( :,J ) ),1.0D-25 )
!            print*," RENORM_FACTOR for ",CHEMISTRY_SPC(J)," = ",RENORM_FACTOR
            SOLD( :,J ) = RENORM_FACTOR * SOLD( :,J )
!            DO KTAG = 1, NTAG_SA
!               SOLD( KTAG,J ) = RENORM_FACTOR * SOLD( KTAG,J )
!            END DO            
         END DO
      END IF
!     STOP

      Return

      End Subroutine SOLVE_ISAM_CHEM
       Subroutine REAL4_ISAM_CHEM( REPLACE_SOLD, RKI, CBULK, DELT, OLD2NEW, NEW2OLD )

          USE GRID_CONF             ! horizontal & vertical domain specifications
          USE RXNS_DATA             ! chemical mechanism data
          USE MECHANISM_FUNCTIONS
          USE CGRID_SPCS            ! CGRID mechanism species
          USE SA_DEFN
          USE LU_DecompositionSolver
          USE LU_DecompositionSolver_LINPACK
          USE UTILIO_DEFN
          
          IMPLICIT NONE 
                    
          Character( 16 ), Save :: PNAME = 'REAL4_ISAM_CHEM'
          
          LOGICAL,   Intent( In ) :: REPLACE_SOLD
          Real( 8 ), Intent( In ) :: RKI( : )
          Real( 8 ), Intent( In ) :: CBULK( : )
          Real( 8 ), Intent( In ) :: DELT
          Integer,   Optional, Intent( In ) :: OLD2NEW( :,: )
          Integer,   Optional, Intent( In ) :: NEW2OLD( :,: )
          
          Integer I,J,S,N
          Integer JROW, JCOL
          Integer INFO                            ! see s_lu.F
          
          Real(8) SUMAT
          Real(8) SUMSP
          Real(8) TOTAL
          Real(8) DIFF
          Real(8) DIFFSP
          
          Real KSTEP                              ! Holds k times timestep(in min)
          Real RXNMULT                            ! Holds product of concs of reactants
          
          Integer NRCT                            ! Counter over reactants
          Integer NPROD                           ! Counter over products
          Integer IREACT                          ! index for reaction reactant
          Integer IPROD                           ! index for reaction produce

C..Scratch Local Variables:
          INTEGER ISP1, ISP2, ISP3  ! Species indices
          INTEGER JSPC,  SPC        ! Species indices
          INTEGER NCELL             ! Loop index for cells
          INTEGER NOUT              ! IRR output index
          INTEGER NC,NR,NRX         ! Loop index for reactions
          INTEGER ASTAT             ! allocation status
          INTEGER KTAG              ! Loop index/pointer for source
          INTEGER RXN               ! Loop index/pointer for reaction
                                                     
C..Saved Local Variables:
      Integer,   Allocatable       :: SENS_INDEX( : )  ! CGRID_INDEX translated by JNEW2OLD
      Integer,   Allocatable       :: MECH_INDEX( : )  ! MECH_INDEX translated by JNEW2OLD

      REAL                         :: CHEMSTEP               ! single precision conversion of DELT
      REAL( 8 )                    :: GAMMA                  ! double precision conversion of half DELT
      REAL( 8 )                    :: RENORM_FACTOR          ! used to normal tag concentration wrt CBULK
      REAL( 8 ), ALLOCATABLE, SAVE :: YNEW   ( : )           ! bulk concentrations from previous time step
      REAL( 8 ), ALLOCATABLE, SAVE :: OUTSIDE( : )           ! species concentration outside isam tags
      REAL( 8 ), ALLOCATABLE, SAVE :: YCMID  ( : )           ! bulk concentrations from previous time step
      REAL( 8 ), ALLOCATABLE, SAVE :: YCRATES( : )           ! bulk concentration used to calculate reactionvrates 
      REAL,      ALLOCATABLE, SAVE :: SNEW   ( :,: )         ! change in source tag species 
      REAL,      ALLOCATABLE, SAVE :: IDENTITY_MATRIX( :,: ) ! change in source tag species 


      Logical, Save :: FIRSTIME = .TRUE.
      Logical, Save :: REORDER  = .TRUE. ! reorder CBULK
      LOGICAL       :: SUCCESS  = .TRUE. ! flag call to FILTER_AND_RENORMAL worked      

      Character( 96 ) :: XMSG = ' '

      IF ( FIRSTIME ) THEN

          FIRSTIME  = .FALSE.
!          LOGDEV    = INIT3()
         
          CALL SET_MECHANISM( ) ! determine formulas for Mechanism Jacobain and Species Rate of Change
  
C Allocate arrays:
          ALLOCATE( YNEW ( NUMB_MECH_SPC ), STAT = ASTAT )
          IF ( ASTAT .NE. 0 ) THEN
             XMSG = 'ERROR allocating YNEW variable'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
          END IF
          
          ALLOCATE( SNEW( NUMB_MECH_SPC,NTAG_SA ), STAT = ASTAT )
          IF ( ASTAT .NE. 0 ) THEN
             XMSG = 'ERROR allocating SNEW variable'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
          END IF
          
          ALLOCATE( IDENTITY_MATRIX( NUMB_MECH_SPC,NUMB_MECH_SPC ), STAT = ASTAT )
          IF ( ASTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating IDENTITY_MATRIX'
          CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
          END IF
          
          IDENTITY_MATRIX = 0.0
          DO JSPC = 1,NUMB_MECH_SPC
             IDENTITY_MATRIX( JSPC,JSPC ) = 1.0        
          END DO
          
          ALLOCATE( A( NUMB_MECH_SPC, NUMB_MECH_SPC ),
     &              A1( NUMB_MECH_SPC, NUMB_MECH_SPC ), STAT = ASTAT )
          IF ( ASTAT .NE. 0 ) THEN
             XMSG = 'Failure allocating A, A1'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
          END IF
          
          ALLOCATE( PDT( NUMB_MECH_SPC,NUMB_MECH_SPC ),
     &              IPVT ( NUMB_MECH_SPC ),
     &              PRD( NUMB_MECH_SPC ), STAT = ASTAT )
          IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating PRD, IPVT, PDT'
            CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
          END IF
          
          ALLOCATE( SENS_INDEX( NUMB_MECH_SPC ),
     &              MECH_INDEX( NUMB_MECH_SPC ), STAT = ASTAT )
          IF ( ASTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating SENS_INDEX or MECH_INDEX'
          CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
          END IF
          
          DO JSPC = 1,NUMB_MECH_SPC
             MECH_INDEX(JSPC) = CGRID_INDEX(JSPC)
             SENS_INDEX(JSPC) = CGRID_INDEX(JSPC)
          END DO

         YNEW = 0.0D0
         
         IF( PRESENT( OLD2NEW ) .AND. PRESENT( OLD2NEW ) )THEN

             REORDER = .FALSE. ! no reordering because YCDMM is already redeorderd by chemsolver
             
              PRINT*,"PRESENT( OLD2NEW ) .AND. PRESENT( OLD2NEW )"
!              DO I = 1,NUMB_MECH_SPC
!                 S  = NEW2OLD( I,1 )
!                 YNEW( S ) = CBULK( I )
!                 WRITE(LOGDEV,'(I4,A,A,ES16.4)')I,' ORIG: ' // CHEMISTRY_SPC(I),' = ',YNEW(S)
!              END DO
!              DO I = 1,NUMB_MECH_SPC
!                 S  = NEW2OLD( I,1 )
!                 WRITE(LOGDEV,'(I4,A,A,2(ES16.4,1X))')S,' SORT: ' // CHEMISTRY_SPC(S),' = ',CBULK( I ),YNEW(S)-CBULK( I )
!              END DO
!             CALL  EVALUATE_F_JAC_MECH( YNEW, SRK, PDT2 ) ! Evaluate Jacobian based on CBULK and SKR values

! overwrite reordering using chemsolver conversing maps

             JNEW2OLD(1:NUMB_MECH_SPC,1) = NEW2OLD(1:NUMB_MECH_SPC,1)
             JOLD2NEW(1:NUMB_MECH_SPC,1) = OLD2NEW(1:NUMB_MECH_SPC,1)
             RESET_JACOBIAN = .TRUE.
             DO RXN = 1, NRXNS
                DO NRCT = 1, NREACT( RXN )
                   IREACT = ISPECIES_REACTION( NRCT,RXN )
                   ISPECIES_REACTION( NRCT,RXN ) = OLD2NEW( IREACT,1 )
                END DO

                DO NPROD = 1, NPRDCT( RXN )
                   IPROD = ISPECIES_REACTION( NPROD+3,RXN )
                   ISPECIES_REACTION( NPROD+3,RXN ) = OLD2NEW( IPROD,1 )
               END DO
             END DO

!             DO S = 1, NUMB_MECH_SPC
!               MECH_INDEX(S) = CGRID_INDEX(JOLD2NEW(S,1))
!               SENS_INDEX(S) = CGRID_INDEX(JNEW2OLD(S,1))
!             END DO

         ELSE
!
!            DO S = 1, NUMB_MECH_SPC
!               MECH_INDEX(S) = CGRID_INDEX(JOLD2NEW(S,1))
!               SENS_INDEX(S) = CGRID_INDEX(JNEW2OLD(S,1))
!            END DO
!            DO I = 1,NUMB_MECH_SPC
!               S  = JOLD2NEW( I,1 )
!               YNEW( S ) = CBULK( I )
!               WRITE(LOGDEV,'(I4,1X,A,A,ES16.4)')I,' SORT: ' // CHEMISTRY_SPC(I),' = ',YNEW(S)
!            END DO
!            DO I = 1,NUMB_MECH_SPC
!               S  = JNEW2OLD( I,1 )
!               WRITE(LOGDEV,'(I4,1X,A,A,2(ES16.4,1X))')I,' ORIGINAL: ' // CHEMISTRY_SPC(S),' = ',CBULK( S ),YNEW(I)-CBULK( S )
!            END DO
!            CBULK = YNEW
         
         END IF
 
!             DO RXN = 1, NRXNS

!                WRITE(LOGDEV,'(A,1X,I4,1X,A)')'For reaction number and label,',RXN,
!     &                                         TRIM(RXLABEL(RXN))
!                WRITE(LOGDEV,*)'Reactants'
!                WRITE(LOGDEV,'(10X,40(A,", "))')
!     &         (CHEMISTRY_SPC(JNEW2OLD(ISPECIES_REACTION( NRCT,RXN ),1)),NRCT = 1, NREACT( RXN ))

!                WRITE(LOGDEV,*)'Products'
!                WRITE(LOGDEV,'(10X,40(A,", "))')
!     &         (CHEMISTRY_SPC(JNEW2OLD(ISPECIES_REACTION( NPROD+3,RXN ),1)),NPROD = 1, NPRDCT( RXN ))

!             END DO
!             WRITE(LOGDEV,*)'Original order: chemistry species, cgrid index'
!             DO S = 1, NUMB_MECH_SPC
!                WRITE(LOGDEV,'(A16,1X,I4)')CHEMISTRY_SPC(S),CGRID_INDEX(S)
!             END DO
!             WRITE(LOGDEV,*)'Sort ordered: chemistry species, cgrid index'
!             DO S = 1, NUMB_MECH_SPC
!                WRITE(LOGDEV,'(A16,1X,I4)')CHEMISTRY_SPC(JOLD2NEW(S,1)),MECH_INDEX(S)
!             END DO

      END IF
      
      IF ( REORDER ) THEN  ! reorder to speed-up LU decomposition of Jacobian

            DO I = 1,NUMB_MECH_SPC
               S  = JOLD2NEW( I,1 )
               YNEW( S ) = CBULK( I )
               IF( YNEW( S ) .LE. 1.0D-25 )YNEW( S ) = 0.0D0 
       !        WRITE(LOGDEV,'(I4,1X,A,A,2(ES16.4,1X))')I,
     & !        ' YAVE and Difference with its sorted value: ' // CHEMISTRY_SPC(S),
     & !        ' = ',CBULK( S ),YNEW(I)-CBULK( S )
            END DO
!            CBULK = YNEW
         
      ELSE
             DO I = 1,NUMB_MECH_SPC
               YNEW( I ) = CBULK( I )
       !       IF( YNEW( I ) .LE. 1.0D-25 )YNEW( I ) = 0.0D0 
       !       WRITE(LOGDEV,'(I4,1X,A,A,2(ES16.4,1X))')I,
     & !       ' YAVE for ' // CHEMISTRY_SPC(I),
     & !       ' = ',YNEW( I )
            END DO
      END IF
      
!     DO I = 1,NRXNS
!        write(6,'(I4,1X,"reaction:",A16," = ",ES12.4)')I,RXLABEL(I),RKI(I)
!     END DO

         CALL  EVALUATE_F_JAC_MECH( YNEW, RKI, PDT ) ! Evaluate Jacobian based on CBULK and SKR values
         
         CHEMSTEP = REAL( DELT,4 )
         DO J = 1, NUMB_MECH_SPC
             DO I = 1, NUMB_MECH_SPC
         
             ISP1 = JNEW2OLD( J,1 )
             ISP2 = JNEW2OLD( I,1 )
         
                 A( I, J )  = 0.0
                 A1( I, J ) = 0.0
                 A( I, J )  = -0.5 * CHEMSTEP * PDT( I,J )
                 A1( I, J ) =  0.5 * CHEMSTEP * PDT( I,J )
                 IF ( I .EQ. J ) THEN
                 A( I, J )  = 1.0 + A( I, J )
                 A1( I, J ) = 1.0 + A1( I, J )
!               IF( I .GT. 1 .AND. I .LT. 4 )THEN      
!                write(6,'(A16," STEP: (1-dt/2*Jacobian)(",I4,",",I4,"),Jacobian(",I4,",",I4,") = ",2(ES18.6,A2))')
!        &     CHEMISTRY_SPC(ISP1),J,J,J,J,A(J,J),", ",-0.5 * CHEMSTEP * PDT(J,J)
!               END IF       
                 END IF                 
!                IF( TRIM( ADJUSTL(CHEMISTRY_SPC(ISP2)) ) .EQ. "O3" .AND. Real8A1( I,J ) .GT. 0.0D0 )THEN
!                   write(6,'("1st:",I4,1X,A16," STEP: (1+dt/2*Jacobian)(",I4,",",I4,"),Jacobian(",I4,",",I4,") = ",2(ES18.6,A2))')
!        &         ISP1,CHEMISTRY_SPC(ISP2),I,J,I,J,A1(I,J),", ",PDT(I,J)                           
!                END IF
            END DO
         END DO
      !print*,"CHEMSTEP = ",CHEMSTEP
      !DO J = 1, NUMB_MECH_SPC
      !   S = JNEW2OLD( J,1 )
      !   write(6,'(A,":(1-dt/2*Jacobian)(",I4,",",I4,"),Jacobian(",I4,",",I4,") = ",2(ES18.6,A2))')
     & !   CHEMISTRY_SPC(S),J,J,J,J,A(J,J),", ",-0.5 * CHEMSTEP * PDT(J,J)
      !END DO


C Factor matrix A by Gaussian elimination
      INFO = 0
      CALL SGEFA( A, NUMB_MECH_SPC, NUMB_MECH_SPC, IPVT, INFO )
!      print*,"SGEFA: INFO = ",INFO
!       CALL sludcmp_f90(A,NUMB_MECH_SPC,IPVT,INFO)
       INFO = 0
      
!     DO J = 1, NUMB_MECH_SPC
!        S = JNEW2OLD( J,1 )
!        write(6,'(A,":Inverse(1-dt/2*Jacobian)(",I4,",",I4,") = ",(ES18.6,A2))')
!    &   CHEMISTRY_SPC(S),J,J,A(J,J)
!     END DO

      IF ( INFO .NE. 0 ) THEN
        XMSG = "Error Inverse of (Identity Matrix - 0.5*TimeStep*Jacobain has below zero(s) on diagonal. "
        WRITE(LOGDEV,'(A)')Trim(XMSG)
        DO J = 1, NUMB_MECH_SPC
          IF( A(J,J) .EQ. 0.0 )THEN
            S = JNEW2OLD( J,1 )
            write(6,'(A,":Inverse(1-dt/2*Jacobian)(",I4,",",I4,") = ",2(ES18.6,1X))')
     &      CHEMISTRY_SPC(S),J,J,A(J,J)
          END IF
        END DO       
        XMSG = "Check Jacobian Matrix for bad values, especially the diagonal."
        WRITE(LOGDEV,'(A)')Trim(XMSG)
        XMSG = "Above Fatal Error in LU Decomposition occurred."
        CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF  

!      IF ( .NOT. REPLACE_SOLD ) THEN
      ! load SNEW for efficient computation using SOLD  
         DO JSPC = 1,NUMB_MECH_SPC
            J = JOLD2NEW( JSPC,1 )
            DO KTAG = 1, NTAG_SA
               SNEW( J,KTAG ) = REAL( SOLD( KTAG,JSPC ),4 )
!               IF( SOLD( KTAG,JSPC ) .GT. 1.0D-36 )THEN
!              IF( KTAG .EQ. 3 )THEN
!                 write(6,'(2(I4,1X),A,2(ES12.4,1X))')KTAG,JSPC,
!    &            "Initial Condition:"
!    &            // CHEMISTRY_SPC(JOLD2NEW(JSPC,1))
!    &            // ":SNEW( J,KTAG ),SOLD( KTAG,JSPC ) = ",
!    &            SNEW( J,KTAG ),SOLD( KTAG,JSPC )             
!              END IF
!               IF ( SNEW( J,KTAG ) .LT. 1.e-25 ) SNEW( J,KTAG ) = 0.0
            END DO
         END DO
!      ELSE
!         print*,".NOT. REPLACE_SOLD = FALSE"   
!      END IF
!      STOP

!      DO J = 1, NUMB_MECH_SPC
!        DO I = 1, NUMB_MECH_SPC
!            ISP2 = JNEW2OLD( I,1 )
!            IF( TRIM( ADJUSTL(CHEMISTRY_SPC(ISP2)) ) .EQ. "O3" .AND. Real8A1( J,I) .GT. 0.0D0 )THEN
!               write(6,'("2nd:",I4,1X,A16," STEP: (1+dt/2*Jacobian)(",I4,",",I4,"),Jacobian(",I4,",",I4,") = ",2(ES18.6,A2))')
!    &          ISP1,CHEMISTRY_SPC(ISP2),I,J,J,I,A1(I,J),", ",PDT(I,J)                           
!            END IF
!        END DO
!      END DO   

      ! Time Integration for SNEW at t+dt
      DO KTAG = 1,NTAG_SA
         ! Evaluate tag concentration vector times (Id+dt/2*Jac) matrix
            DO I = 1,NUMB_MECH_SPC
            ! Evaluate prd: prd = (Id+dt/2*Jac)*sold
               PRD( I ) = 0.0
               DO J = 1,NUMB_MECH_SPC
                  PRD( I ) = PRD( I ) + A1( I, J ) * SNEW( J,KTAG )
!              ISP1 = JNEW2OLD( I,1 )
!              IF( TRIM( ADJUSTL(CHEMISTRY_SPC(ISP1)) ) .EQ. "O3" .AND. Real8A1( I,J ) .GT. 0.0D0 )THEN
!                 write(6,'("2nd:",I4,1X,A16," STEP: (1+dt/2*Jacobian)(",I4,",",I4,"),Jacobian(",I4,",",I4,") = ",2(ES18.6,A2))')
!    &            ISP1,CHEMISTRY_SPC(ISP1),I,J,I,J,A1(I,J),", ",PDT(I,J)                           
!              END IF
               
               
!                 IF( I .EQ. 1 .AND. A1( I, J ) .GT. 0.0 )THEN
!                    write(6,"(2(I4,1X),A,4(es12.4,1X))")I,J,
!    &               TRIM( CHEMISTRY_SPC(JOLD2NEW(I,1)) ) 
!    &               // ":PRD( I ),A1( I, J ),SNEW( I,KTAG ) = ", 
!    &               PRD( I ),A1( I,J ),SNEW( I,KTAG )              
!                 END IF
               END DO
!              IF( SNEW( I,KTAG ) .GT. 1.0E-30 )THEN
!                  write(6,"(2(I4,1X),A,4(es12.4,1X))")KTAG,I,
!    &            TRIM( CHEMISTRY_SPC(JOLD2NEW(I,1)) ) 
!    &            // ":PRD( I ),SNEW( I,KTAG ) = ", 
!    &                PRD( I ),SNEW( I,KTAG )
!              END IF
           END DO
!        STOP
         
         ! Solve tag concentrations at t+dt:Inverse(Id-dt/2*Jac) * prd
         CALL SGESL( A, NUMB_MECH_SPC, NUMB_MECH_SPC, IPVT, PRD, 0 )
!         CALL slubksb_f90(A,NUMB_MECH_SPC,IPVT,PRD)
         
         DO I = 1,NUMB_MECH_SPC
!            SNEW( I,KTAG ) = MAX( PRD( I ),0.0 )
                J = JNEW2OLD( I,1 )  
!!!                IF( KTAG .EQ. 3 )THEN
!!!                  write(6,"('Prediction:Tag Number,Species Number = ',2(I4,1X),A,2(ES12.4,1X))")
!!!     &            KTAG,I,CHEMISTRY_SPC(J) // "SNEW( I,KTAG ) = ",Real8PRD( I )
!!!                END IF
              SNEW( I,KTAG ) = MAX( PRD( I ),0.0 )
         END DO 
      END DO  ! tag loop

      CALL FILTER_AND_RENORMAL( NTAG_SA, SNEW(1:NTAG_SA,J), 1.0E-25, 0.98, SUCCESS )
      ! Load Solution into SOLD using SNEW
      IF ( REPLACE_SOLD ) THEN
         DO JSPC = 1,NUMB_MECH_SPC
            J = JNEW2OLD( JSPC,1 )
            DO KTAG = 1, NTAG_SA
               SOLD( KTAG,J ) = REAL( SNEW( JSPC,KTAG ),8 )
               IF( SOLD( KTAG,JSPC ) .GT. 1.0D3 )THEN
                 write(6,"('Warning High Prediction:Tag Number,Species Number = ',2(I4,1X),A,ES12.4)")
     &           KTAG,J,CHEMISTRY_SPC(J) // "SOLD = ",SOLD( KTAG,J )
               END IF
        !       IF( KTAG .EQ. 3 )THEN
        !         write(6,"('Prediction:Tag Number,Species Number = ',2(I4,1X),A,2(ES12.4,1X))")
     &  !         KTAG,J,CHEMISTRY_SPC(J) // "SOLD = ",SOLD( KTAG,J ),YNEW(JSPC)
        !       END IF
            END DO           
            RENORM_FACTOR = CBULK(J)/MAX( SUM( SOLD( :,J ) ),1.0D-25 )
!            print*," RENORM_FACTOR for ",CHEMISTRY_SPC(J)," = ",RENORM_FACTOR
            SOLD( :,J ) = RENORM_FACTOR * SOLD( :,J )
!            DO KTAG = 1, NTAG_SA
!               SOLD( KTAG,J ) = RENORM_FACTOR * SOLD( KTAG,J )
!            END DO            
         END DO
      END IF
!     STOP

      Return

      End Subroutine REAL4_ISAM_CHEM

       Subroutine REAL8_ISAM_CHEM( REPLACE_SOLD, RKI, CBULK, DELT, OLD2NEW, NEW2OLD )

          USE GRID_CONF             ! horizontal & vertical domain specifications
          USE RXNS_DATA             ! chemical mechanism data
          USE MECHANISM_FUNCTIONS
          USE CGRID_SPCS            ! CGRID mechanism species
          USE SA_DEFN
          USE LU_DecompositionSolver
          USE LU_DecompositionSolver_LINPACK
          USE UTILIO_DEFN
          
          IMPLICIT NONE 
                    
          Character( 16 ), Save :: PNAME = 'REAL8_ISAM_CHEM'
          
          LOGICAL,   Intent( In ) :: REPLACE_SOLD
          Real( 8 ), Intent( In ) :: RKI( : )
          Real( 8 ), Intent( In ) :: CBULK( : )
          Real( 8 ), Intent( In ) :: DELT
          Integer,   Optional, Intent( In ) :: OLD2NEW( :,: )
          Integer,   Optional, Intent( In ) :: NEW2OLD( :,: )
          
          Integer I,J,S,N
          Integer INFO                            ! see s_lu.F
          
          Real(8) SUMAT
          Real(8) SUMSP
          Real(8) TOTAL
          Real(8) DIFF
          Real(8) DIFFSP
          
          Real KSTEP                              ! Holds k times timestep(in min)
          Real RXNMULT                            ! Holds product of concs of reactants
          
          Integer NRCT                            ! Counter over reactants
          Integer NPROD                           ! Counter over products
          Integer IREACT                          ! index for reaction reactant
          Integer IPROD                           ! index for reaction produce

C..Scratch Local Variables:
          INTEGER ISP1, ISP2, ISP3  ! Species indices
          INTEGER JSPC,  SPC        ! Species indices
          INTEGER NCELL             ! Loop index for cells
          INTEGER NOUT              ! IRR output index
          INTEGER NC,NR,NRX         ! Loop index for reactions
          INTEGER ASTAT             ! allocation status
          INTEGER KTAG              ! Loop index/pointer for source
          INTEGER RXN               ! Loop index/pointer for reaction
                                                     
C..Saved Local Variables:
          Integer,   Allocatable       :: SENS_INDEX( : )  ! CGRID_INDEX translated by JNEW2OLD
          Integer,   Allocatable       :: MECH_INDEX( : )  ! MECH_INDEX translated by JNEW2OLD
          
          REAL( 8 )                    :: GAMMA                  ! half DELT
          REAL( 8 )                    :: RENORM_FACTOR          ! used to normal tag concentration wrt CBULK
          REAL( 8 ), ALLOCATABLE, SAVE :: YNEW   ( : )           ! bulk concentrations from previous time step
          REAL( 8 ), ALLOCATABLE, SAVE :: YCRATES( : )           ! bulk concentration used to calculate reactionvrates 
          REAL( 8 ), ALLOCATABLE, SAVE :: SNEW   ( :,: )         ! change in source tag species 
          REAL,      ALLOCATABLE, SAVE :: IDENTITY_MATRIX( :,: ) ! change in source tag species 
          
          
          Logical, Save :: FIRSTIME = .TRUE.
          Logical, Save :: REORDER  = .TRUE. ! reorder CBULK
          LOGICAL       :: SUCCESS  = .TRUE. ! flag call to FILTER_AND_RENORMAL worked      

          Character( 96 ) :: XMSG = ' '

      IF ( FIRSTIME ) THEN

          FIRSTIME  = .FALSE.
!          LOGDEV    = INIT3()
         
          CALL SET_MECHANISM( ) ! determine formulas for Mechanism Jacobain and Species Rate of Change
  
C Allocate arrays:
          ALLOCATE( YNEW ( NUMB_MECH_SPC ), STAT = ASTAT )
          IF ( ASTAT .NE. 0 ) THEN
             XMSG = 'ERROR allocating YNEW variable'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
          END IF
          
          ALLOCATE( SNEW( NUMB_MECH_SPC,NTAG_SA ), STAT = ASTAT )
          IF ( ASTAT .NE. 0 ) THEN
             XMSG = 'ERROR allocating SNEW variable'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
          END IF
          
          ALLOCATE( Real8A( NUMB_MECH_SPC, NUMB_MECH_SPC ),
     &              Real8A1( NUMB_MECH_SPC, NUMB_MECH_SPC ), STAT = ASTAT )
          IF ( ASTAT .NE. 0 ) THEN
             XMSG = 'Failure allocating A, A1'
             CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
          END IF
          
          ALLOCATE( Real8PDT( NUMB_MECH_SPC,NUMB_MECH_SPC ),
     &              IPVT ( NUMB_MECH_SPC ),
     &              Real8PRD( NUMB_MECH_SPC ), STAT = ASTAT )
          IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating PRD, IPVT, PDT'
            CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
          END IF
          
          ALLOCATE( SENS_INDEX( NUMB_MECH_SPC ),
     &              MECH_INDEX( NUMB_MECH_SPC ), STAT = ASTAT )
          IF ( ASTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating SENS_INDEX or MECH_INDEX'
          CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
          END IF
          
          DO JSPC = 1,NUMB_MECH_SPC
             MECH_INDEX(JSPC) = CGRID_INDEX(JSPC)
             SENS_INDEX(JSPC) = CGRID_INDEX(JSPC)
          END DO

         YNEW = 0.0D0
         
         IF( PRESENT( OLD2NEW ) .AND. PRESENT( OLD2NEW ) )THEN

             REORDER = .FALSE. ! no reordering because YCDMM is already redeorderd by chemsolver
             
              PRINT*,"PRESENT( OLD2NEW ) .AND. PRESENT( OLD2NEW )"
!              DO I = 1,NUMB_MECH_SPC
!                 S  = NEW2OLD( I,1 )
!                 YNEW( S ) = CBULK( I )
!                 WRITE(LOGDEV,'(I4,A,A,ES16.4)')I,' ORIG: ' // CHEMISTRY_SPC(I),' = ',YNEW(S)
!              END DO
!              DO I = 1,NUMB_MECH_SPC
!                 S  = NEW2OLD( I,1 )
!                 WRITE(LOGDEV,'(I4,A,A,2(ES16.4,1X))')S,' SORT: ' // CHEMISTRY_SPC(S),' = ',CBULK( I ),YNEW(S)-CBULK( I )
!              END DO
!             CALL  EVALUATE_F_JAC_MECH( YNEW, SRK, PDT2 ) ! Evaluate Jacobian based on CBULK and SKR values

! overwrite reordering using chemsolver conversing maps

             JNEW2OLD(1:NUMB_MECH_SPC,1) = NEW2OLD(1:NUMB_MECH_SPC,1)
             JOLD2NEW(1:NUMB_MECH_SPC,1) = OLD2NEW(1:NUMB_MECH_SPC,1)
             RESET_JACOBIAN = .TRUE.
             DO RXN = 1, NRXNS
                DO NRCT = 1, NREACT( RXN )
                   IREACT = ISPECIES_REACTION( NRCT,RXN )
                   ISPECIES_REACTION( NRCT,RXN ) = OLD2NEW( IREACT,1 )
                END DO

                DO NPROD = 1, NPRDCT( RXN )
                   IPROD = ISPECIES_REACTION( NPROD+3,RXN )
                   ISPECIES_REACTION( NPROD+3,RXN ) = OLD2NEW( IPROD,1 )
               END DO
             END DO

!             DO S = 1, NUMB_MECH_SPC
!               MECH_INDEX(S) = CGRID_INDEX(JOLD2NEW(S,1))
!               SENS_INDEX(S) = CGRID_INDEX(JNEW2OLD(S,1))
!             END DO

         ELSE
!
!            DO S = 1, NUMB_MECH_SPC
!               MECH_INDEX(S) = CGRID_INDEX(JOLD2NEW(S,1))
!               SENS_INDEX(S) = CGRID_INDEX(JNEW2OLD(S,1))
!            END DO
!            DO I = 1,NUMB_MECH_SPC
!               S  = JOLD2NEW( I,1 )
!               YNEW( S ) = CBULK( I )
!               WRITE(LOGDEV,'(I4,1X,A,A,ES16.4)')I,' SORT: ' // CHEMISTRY_SPC(I),' = ',YNEW(S)
!            END DO
!            DO I = 1,NUMB_MECH_SPC
!               S  = JNEW2OLD( I,1 )
!               WRITE(LOGDEV,'(I4,1X,A,A,2(ES16.4,1X))')I,' ORIGINAL: ' // CHEMISTRY_SPC(S),' = ',CBULK( S ),YNEW(I)-CBULK( S )
!            END DO
!            CBULK = YNEW
         
         END IF
 
!             DO RXN = 1, NRXNS

!                WRITE(LOGDEV,'(A,1X,I4,1X,A)')'For reaction number and label,',RXN,
!     &                                         TRIM(RXLABEL(RXN))
!                WRITE(LOGDEV,*)'Reactants'
!                WRITE(LOGDEV,'(10X,40(A,", "))')
!     &         (CHEMISTRY_SPC(JNEW2OLD(ISPECIES_REACTION( NRCT,RXN ),1)),NRCT = 1, NREACT( RXN ))

!                WRITE(LOGDEV,*)'Products'
!                WRITE(LOGDEV,'(10X,40(A,", "))')
!     &         (CHEMISTRY_SPC(JNEW2OLD(ISPECIES_REACTION( NPROD+3,RXN ),1)),NPROD = 1, NPRDCT( RXN ))

!             END DO
!             WRITE(LOGDEV,*)'Original order: chemistry species, cgrid index'
!             DO S = 1, NUMB_MECH_SPC
!                WRITE(LOGDEV,'(A16,1X,I4)')CHEMISTRY_SPC(S),CGRID_INDEX(S)
!             END DO
!             WRITE(LOGDEV,*)'Sort ordered: chemistry species, cgrid index'
!             DO S = 1, NUMB_MECH_SPC
!                WRITE(LOGDEV,'(A16,1X,I4)')CHEMISTRY_SPC(JOLD2NEW(S,1)),MECH_INDEX(S)
!             END DO

      END IF
      
      IF ( REORDER ) THEN  ! reorder to speed-up LU decomposition of Jacobian

            DO I = 1,NUMB_MECH_SPC
               S  = JOLD2NEW( I,1 )
               YNEW( S ) = CBULK( I )
               IF( YNEW( S ) .LE. 1.0D-25 )YNEW( S ) = 0.0D0 
       !        WRITE(LOGDEV,'(I4,1X,A,A,2(ES16.4,1X))')I,
     & !        ' YAVE and Difference with its sorted value: ' // CHEMISTRY_SPC(S),
     & !        ' = ',CBULK( S ),YNEW(I)-CBULK( S )
            END DO
!            CBULK = YNEW
         
      ELSE
             DO I = 1,NUMB_MECH_SPC
               YNEW( I ) = CBULK( I )
       !       IF( YNEW( I ) .LE. 1.0D-25 )YNEW( I ) = 0.0D0 
       !       WRITE(LOGDEV,'(I4,1X,A,A,2(ES16.4,1X))')I,
     & !       ' YAVE for ' // CHEMISTRY_SPC(I),
     & !       ' = ',YNEW( I )
            END DO
      END IF
      
!     DO I = 1,NRXNS
!        write(6,'(I4,1X,"reaction:",A16," = ",ES12.4)')I,RXLABEL(I),RKI(I)
!     END DO

         CALL  EVALUATE_F_DJAC_MECH( YNEW, RKI, Real8PDT ) ! Evaluate Jacobian based on CBULK and SKR values
         
         GAMMA = 0.5D0 * DELT
         DO J = 1, NUMB_MECH_SPC
             DO I = 1, NUMB_MECH_SPC         
!                 Real8A( I, J )  = 0.0D0
!                 Real8A1( I, J ) = 0.0D0
                 Real8A( I, J )  = -GAMMA * Real8PDT( I,J )
                 Real8A1( I, J ) =  GAMMA * Real8PDT( I,J )
                 IF ( I .EQ. J ) THEN
                    Real8A( I, J )  = 1.0 + Real8A( I, J )
                    Real8A1( I, J ) = 1.0 + Real8A1( I, J )
!               IF( I .GT. 1 .AND. I .LT. 4 )THEN      
!                write(6,'(A16," STEP: (1-dt/2*Jacobian)(",I4,",",I4,"),Jacobian(",I4,",",I4,") = ",2(ES18.6,A2))')
!        &     CHEMISTRY_SPC(ISP1),J,J,J,J,Real8A(J,J),", ",-0.5 * CHEMSTEP * PDT(J,J)
!               END IF       
                 END IF                 
!                ISP1 = JNEW2OLD( J,1 )
!                ISP2 = JNEW2OLD( I,1 )         
!                IF( TRIM( ADJUSTL(CHEMISTRY_SPC(ISP2)) ) .EQ. "O3" .AND. Real8A( I,J ) .GT. 0.0D0 )THEN
!                   write(6,'("1st:",I4,1X,A16," STEP: (1+dt/2*Jacobian)(",I4,",",I4,"),Jacobian(",I4,",",I4,") = ",2(ES18.6,A2))')
!        &         ISP1,CHEMISTRY_SPC(ISP2),I,J,I,J,Real8A(I,J),", ",PDT(I,J)                           
!                END IF
            END DO
         END DO
      !print*,"CHEMSTEP = ",CHEMSTEP
      !DO J = 1, NUMB_MECH_SPC
      !   S = JNEW2OLD( J,1 )
      !   write(6,'(A,":(1-dt/2*Jacobian)(",I4,",",I4,"),Jacobian(",I4,",",I4,") = ",2(ES18.6,A2))')
     & !   CHEMISTRY_SPC(S),J,J,J,J,Real8A(J,J),", ",-0.5 * CHEMSTEP * PDT(J,J)
      !END DO


C Factor matrix A by Gaussian elimination
      INFO = 0
      CALL DGEFA( Real8A, NUMB_MECH_SPC, NUMB_MECH_SPC, IPVT, INFO )
!      print*,"SGEFA: INFO = ",INFO
!       CALL ludcmp_f90(A,NUMB_MECH_SPC,IPVT,INFO)
       INFO = 0
      
       !DO J = 1, NUMB_MECH_SPC
       !  S = JNEW2OLD( J,1 )
       !  write(6,'(A,":Inverse(1-dt/2*Jacobian)(",I4,",",I4,") = ",2(ES18.6,1X))')
     & !   CHEMISTRY_SPC(S),J,J,Real8A(J,J)
       !END DO

!     DO J = 1, NUMB_MECH_SPC
!        S = JNEW2OLD( J,1 )
!        write(6,'(A,":Inverse(1-dt/2*Jacobian)(",I4,",",I4,") = ",(ES18.6,A2))')
!    &   CHEMISTRY_SPC(S),J,J,Real8A(J,J)
!     END DO

      IF ( INFO .NE. 0 ) THEN
        XMSG = "Error Inverse of (Identity Matrix - 0.5*TimeStep*Jacobain has below zero(s) on diagonal. "
        WRITE(LOGDEV,'(A)')Trim(XMSG)
        DO J = 1, NUMB_MECH_SPC
          IF( Real8A(J,J) .EQ. 0.0 )THEN
            S = JNEW2OLD( J,1 )
            write(6,'(A,":Inverse(1-dt/2*Jacobian)(",I4,",",I4,") = ",2(ES18.6,1X))')
     &      CHEMISTRY_SPC(S),J,J,Real8A(J,J)
          END IF
        END DO       
        XMSG = "Check Jacobian Matrix for bad values, especially the diagonal."
        WRITE(LOGDEV,'(A)')Trim(XMSG)
        XMSG = "Above Fatal Error in LU Decomposition occurred."
        CALL M3EXIT ( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF  

!      IF ( .NOT. REPLACE_SOLD ) THEN
      ! load SNEW for efficient computation using SOLD  
         DO JSPC = 1,NUMB_MECH_SPC
            J = JOLD2NEW( JSPC,1 )
            DO KTAG = 1, NTAG_SA
               SNEW( J,KTAG ) = SOLD( KTAG,JSPC )
!               IF( SOLD( KTAG,JSPC ) .GT. 1.0D-36 )THEN
!              IF( KTAG .EQ. 3 )THEN
!                 write(6,'(2(I4,1X),A,2(ES12.4,1X))')KTAG,JSPC,
!    &            "Initial Condition:"
!    &            // CHEMISTRY_SPC(JOLD2NEW(JSPC,1))
!    &            // ":SNEW( J,KTAG ),SOLD( KTAG,JSPC ) = ",
!    &            SNEW( J,KTAG ),SOLD( KTAG,JSPC )             
!              END IF
!               IF ( SNEW( J,KTAG ) .LT. 1.0d-25 ) SNEW( J,KTAG ) = 0.0D0
            END DO
         END DO
!      ELSE
!         print*,".NOT. REPLACE_SOLD = FALSE"   
!      END IF
!      STOP

!      DO J = 1, NUMB_MECH_SPC
!        DO I = 1, NUMB_MECH_SPC
!            ISP2 = JNEW2OLD( I,1 )
!            IF( TRIM( ADJUSTL(CHEMISTRY_SPC(ISP2)) ) .EQ. "O3" .AND. Real8A1( J,I) .GT. 0.0D0 )THEN
!               write(6,'("2nd:",I4,1X,A16," STEP: (1+dt/2*Jacobian)(",I4,",",I4,"),Jacobian(",I4,",",I4,") = ",2(ES18.6,A2))')
!    &          ISP1,CHEMISTRY_SPC(ISP2),I,J,J,I,Real8A1(I,J),", ",Real8PDT(I,J)                           
!            END IF
!        END DO
!      END DO   

      ! Time Integration for SNEW at t+dt
      DO KTAG = 1,NTAG_SA
         ! Evaluate tag concentration vector times (Id+dt/2*Jac) matrix
            DO I = 1,NUMB_MECH_SPC
            ! Evaluate prd: prd = (Id+dt/2*Jac)*snew
               Real8PRD( I ) = 0.0D0
               DO J = 1,NUMB_MECH_SPC
                  Real8PRD( I ) = Real8PRD( I ) + Real8A1( I, J ) * SNEW( J,KTAG )
!              ISP1 = JNEW2OLD( I,1 )
!              IF( TRIM( ADJUSTL(CHEMISTRY_SPC(ISP1)) ) .EQ. "O3" .AND. Real8A1( I,J ) .GT. 0.0D0 )THEN
!                 write(6,'("2nd:",I4,1X,A16," STEP: (1+dt/2*Jacobian)(",I4,",",I4,"),Jacobian(",I4,",",I4,") = ",2(ES18.6,A2))')
!    &            ISP1,CHEMISTRY_SPC(ISP1),I,J,I,J,Real8A1(I,J),", ",Real8PDT(I,J)
!              END IF
               
               
!                 IF( I .EQ. 1 .AND. Real8A1( I, J ) .GT. 0.0 )THEN
!                    write(6,"(2(I4,1X),A,4(es12.4,1X))")I,J,
!    &               TRIM( CHEMISTRY_SPC(JOLD2NEW(I,1)) ) 
!    &               // ":Real8PRD( I ),Real8A1( I, J ),SNEW( I,KTAG ) = ", 
!    &               Real8PRD( I ),Real8A1( I,J ),SNEW( I,KTAG )              
!                 END IF
               END DO
!              IF( SNEW( I,KTAG ) .GT. 1.0D-30 )THEN
!                  write(6,"(2(I4,1X),A,4(es12.4,1X))")KTAG,I,
!    &            TRIM( CHEMISTRY_SPC(JOLD2NEW(I,1)) ) 
!    &            // ":Real8PRD( I ),SNEW( I,KTAG ) = ", 
!    &                Real8PRD( I ),SNEW( I,KTAG )
!              END IF
           END DO
!        STOP
         
         ! Solve tag concentrations at t+dt:Inverse(Id-dt/2*Jac) * prd
         CALL DGESL( Real8A, NUMB_MECH_SPC, NUMB_MECH_SPC, IPVT, Real8PRD, 0 )
!         CALL lubksb_f90(A,NUMB_MECH_SPC,IPVT,PRD)
         
         DO I = 1,NUMB_MECH_SPC
!            SNEW( I,KTAG ) = MAX( Real8PRD( I ),0.0D0 )
                J = JNEW2OLD( I,1 )  
!!!                IF( KTAG .EQ. 3 )THEN
!!!                  write(6,"('Prediction:Tag Number,Species Number = ',2(I4,1X),A,2(ES12.4,1X))")
!!!     &            KTAG,I,CHEMISTRY_SPC(J) // "SNEW( I,KTAG ) = ",Real8PRD( I )
!!!                END IF
              SNEW( I,KTAG ) = MAX( Real8PRD( I ),0.0D0 )
         END DO 
      END DO  ! tag loop

      CALL FILTER_AND_RENORMAL( NTAG_SA, SNEW(1:NTAG_SA,J), 1.0D-25, 0.98D0, SUCCESS )
      ! Load Solution into SOLD using SNEW
      IF ( REPLACE_SOLD ) THEN
         DO JSPC = 1,NUMB_MECH_SPC
            J = JNEW2OLD( JSPC,1 )
            DO KTAG = 1, NTAG_SA
               SOLD( KTAG,J ) = SNEW( JSPC,KTAG )
               IF( SOLD( KTAG,JSPC ) .GT. 1.0D3 )THEN
                 write(6,"('Warning High Prediction:Tag Number,Species Number = ',2(I4,1X),A,ES12.4)")
     &           KTAG,J,CHEMISTRY_SPC(J) // "SOLD = ",SOLD( KTAG,J )
               END IF
        !       IF( KTAG .EQ. 3 )THEN
        !         write(6,"('Prediction:Tag Number,Species Number = ',2(I4,1X),A,2(ES12.4,1X))")
     &  !         KTAG,J,CHEMISTRY_SPC(J) // "SOLD = ",SOLD( KTAG,J ),YNEW(JSPC)
        !       END IF
            END DO           
            RENORM_FACTOR = CBULK(J)/MAX( SUM( SOLD( :,J ) ),1.0D-25 )
!            print*," RENORM_FACTOR for ",CHEMISTRY_SPC(J)," = ",RENORM_FACTOR
            SOLD( :,J ) = RENORM_FACTOR * SOLD( :,J )
!            DO KTAG = 1, NTAG_SA
!               SOLD( KTAG,J ) = RENORM_FACTOR * SOLD( KTAG,J )
!            END DO            
         END DO
      END IF
!     STOP

      Return

      End Subroutine REAL8_ISAM_CHEM
      END MODULE SA_IRR_DEFN



