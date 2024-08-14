
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
      MODULE ELMO_DERIVED

      USE ELMO_DATA

      IMPLICIT NONE

      PUBLIC

      SAVE

      INTEGER  :: MX_SRC

      ! Allocate variables needed for referencing specific components of
      ! DERIVED variable calculations
      INTEGER,ALLOCATABLE :: J_PMF_MASS(:)
      INTEGER,ALLOCATABLE :: J_PMF_SO4(:)
      INTEGER,ALLOCATABLE :: J_PMF_NO3(:)
      INTEGER,ALLOCATABLE :: J_PMF_NH4(:)
      INTEGER,ALLOCATABLE :: J_AMS_OA(:)
      INTEGER,ALLOCATABLE :: J_PMF_OA(:)
      INTEGER,ALLOCATABLE :: J_PMF_OC(:)
      INTEGER,ALLOCATABLE :: J_PM25_SO4(:)
      INTEGER,ALLOCATABLE :: J_PM25_NO3(:)
      INTEGER,ALLOCATABLE :: J_PM25_NH4(:)
      INTEGER,ALLOCATABLE :: J_PMF_HP(:)
      INTEGER,ALLOCATABLE :: J_PMF_HPM(:)
      INTEGER,ALLOCATABLE :: J_PMF_H2O(:)
      INTEGER,ALLOCATABLE :: J_PMF_BENAPY(:)
      INTEGER,ALLOCATABLE :: J_GAS_BENAPY(:)
      

      CONTAINS

!-------------------------------------------------------------------------
      SUBROUTINE ELMO_MAP_DERIVED( )
!     This subroutine maps the DERIVED ELMO variables.
!-------------------------------------------------------------------------

#ifdef isam
      USE SA_DEFN, ONLY : NTAG_SA
#endif
#ifdef sens
      USE DDM3D_DEFN, ONLY : NPMAX
#endif

      IMPLICIT NONE    

      INTEGER J, IOS, ISRC
      
      MX_SRC = 0
#ifdef isam
      MX_SRC = NTAG_SA + 1
#endif
#ifdef sens
      MX_SRC = NPMAX
#endif

      ! Allocate indices for saving ELMO TABLE location of DERIVED
      ! variable components
      ALLOCATE( J_PMF_MASS(MX_SRC),  J_PMF_SO4(MX_SRC),
     &          J_PMF_NO3(MX_SRC),   J_PMF_NH4(MX_SRC),
     &          J_PMF_OA(MX_SRC),    J_PMF_OC(MX_SRC),
     &          J_PM25_SO4(MX_SRC),  J_PM25_NO3(MX_SRC),
     &          J_PM25_NH4(MX_SRC),  J_PMF_HP(MX_SRC),
     &          J_PMF_HPM(MX_SRC),   J_PMF_H2O(MX_SRC),
     &          J_PMF_BENAPY(MX_SRC),J_GAS_BENAPY(MX_SRC),
     &          STAT = IOS )
      CALL CHECKMEM( IOS, 'J DERIVED COMPONENTS','ELMO_MAP_DERIVED' )

      ! Search for source-dependent indices in ELMO TABLE
      DO ISRC = 0,MX_SRC
        DO J = 1,N_ELMO_TABLE
          IF ( ELMO_TABLE( J )%SOURCE .EQ. ISRC ) THEN
            SELECT CASE ( ELMO_TABLE( J )%NAME )
              CASE ( 'PMF_MASS' )
                J_PMF_MASS( ISRC+1 ) = J
              CASE ( 'PMF_SO4' )
                J_PMF_SO4( ISRC+1 ) = J
              CASE ( 'PMF_NO3' ) 
                J_PMF_NO3( ISRC+1 ) = J
              CASE ( 'PMF_NH4' ) 
                J_PMF_NH4( ISRC+1 ) = J
              CASE ( 'AMS_OA' ) 
                J_AMS_OA( ISRC+1 ) = J
              CASE ( 'PMF_OA' ) 
                J_PMF_OA( ISRC+1 ) = J
              CASE ( 'PMF_OC' ) 
                J_PMF_OC( ISRC+1 ) = J
              CASE ( 'PM25_SO4' )
                J_PM25_SO4( ISRC+1 ) = J
              CASE ( 'PM25_NO3' )
                J_PM25_NO3( ISRC+1 ) = J
              CASE ( 'PM25_NH4' ) 
                J_PM25_NH4( ISRC+1 ) = J
              CASE ( 'PMF_HP' ) 
                J_PMF_HP( ISRC+1 ) = J
              CASE ( 'PMF_HPM' ) 
                J_PMF_HPM( ISRC+1 ) = J
              CASE ( 'PMF_H2O' )
                J_PMF_H2O( ISRC+1 ) = J
              CASE ( 'PMF_BENAPY' ) 
                J_PMF_BENAPY( ISRC+1 ) = J
              CASE ( 'GAS_BENAPY' ) 
                J_GAS_BENAPY( ISRC+1 ) = J
            END SELECT                
          END IF
        END DO
      END DO

      ! Define Formulas for DERIVED Variables


      END SUBROUTINE ELMO_MAP_DERIVED

      END MODULE ELMO_DERIVED
      
