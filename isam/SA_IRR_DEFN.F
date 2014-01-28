      MODULE SA_IRR_DEFN

C***********************************************************************
C  Created by Kwok, Jun 24, 2011
C  
C  (1) Stores initial reaction rates in a C-R-L-nrxns array
C  (2) Contains the following subroutine SA_PL_CB05
C
C    SA_PL_CB05 does the following:
C    (a) Apportion production rates of NOX, HNO3 and NTR, using the
C      initial reaction rates
C    (b) Update the tags with analytic solutions that require the 
C      apportioned production rates
C    (c) Correct the solutions by bulk concentrations of regular species
C
C    Note: Time interval of analytic solution is 
C             EBI_TMSTEP * N_EBI_STEPS
C
C    Aug 16, 2011: chemical integration time interval is in MINUTES
C
C***********************************************************************

      USE HGRD_DEFN
      USE VGRD_DEFN
      USE HRDATA
      USE SA_DEFN    ! 20130517

      IMPLICIT NONE

!KRT  INCLUDE SUBST_IOPARMS
!KRT  INCLUDE SUBST_IODECL
!KRT  INCLUDE SUBST_CONST
!KRT  INCLUDE SUBST_RXCMMN

      REAL, ALLOCATABLE, SAVE :: RXINIT( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: RKI_INIT( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: YC_INIT( :,:,:,: )
      REAL, ALLOCATABLE, SAVE :: PRDRATE( : )
      REAL, ALLOCATABLE, SAVE :: RKMID ( : )

      CONTAINS
        SUBROUTINE SA_IRR_INIT

        IMPLICIT NONE

C=======================================================

        IF ( .NOT. ALLOCATED( RXINIT ) ) 
     &    ALLOCATE( RXINIT( MY_NCOLS, MY_NROWS, NLAYS, N_RXNS ) )
        RXINIT = 0.0

        IF ( .NOT. ALLOCATED( RKI_INIT ) )
     &    ALLOCATE( RKI_INIT( MY_NCOLS, MY_NROWS, NLAYS, N_RXNS ) )
        RKI_INIT = 0.0

        IF ( .NOT. ALLOCATED( YC_INIT ) )
     &    ALLOCATE( YC_INIT( MY_NCOLS, MY_NROWS, NLAYS, N_SPEC ) )
        YC_INIT = 0.0

!20130517        IF ( .NOT. ALLOCATED( PRDRATE ) ) ALLOCATE( PRDRATE( N_SPEC ) )
        IF ( .NOT. ALLOCATED( PRDRATE ) ) ALLOCATE( PRDRATE( NSPC_SA ) )
        PRDRATE = 0.0

        IF ( .NOT. ALLOCATED( RKMID ) ) ALLOCATE( RKMID( N_RXNS ) )
        RKMID = 0.0

        END SUBROUTINE SA_IRR_INIT
C----------------------------------------------------------------------

! subroutine SA_PL_CB05 removed.  20130606

      END MODULE SA_IRR_DEFN
