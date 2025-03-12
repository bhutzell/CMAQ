!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !!
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

      SUBROUTINE HETCHEM_EXTRACT_AERO ( CONCVEC, C, R, L, WET_M2, WET_M3, 
     &                                  DE_WET )

c  Calculates the change in aerosol surface area due to heterogeneous
C  reactions and updates the CGRID array with this information.
  
C  History
C  May 2016: BM Created

C  Key Subroutines Called: none
C
C  Called by: Gas-Phase Chemistry Driver
      USE AERO_DATA, ONLY: EXTRACT_AERO, CALCMOMENTS, N_MODE, MOMENT2_CONC,
     &                     MOMENT3_CONC, AEROMODE_DIAM, AEROMODE_LNSG,
     &                     CHEM_M2DRY_INIT, CHEM_M3DRY_INIT, FIXED_SG
      USE CONST, ONLY: pi
      USE GRID_CONF, ONLY: NLAYS, NCOLS, NROWS
      USE PRECURSOR_DATA, ONLY: EXTRACT_PRECURSOR

#ifdef sens
      USE DDM3D_DEFN, ONLY: SENGRID

#endif
      IMPLICIT NONE
      
      REAL, INTENT( IN ) :: CONCVEC ( : )  ! pointer to model concentrations
      REAL, PARAMETER    :: TWOTHIRDS = 2.0 / 3.0
      REAL               :: M2DRY_FINAL( N_MODE ) ! Dry 2nd Aerosol Moment
      REAL               :: M3DRY_FINAL( N_MODE ) ! Dry 3rd Aerosol Moment
      INTEGER            :: C, R, L, N
      REAL( 8 ), INTENT(OUT) :: WET_M2( : ), WET_M3( : ), DE_WET( : )
      LOGICAL, SAVE      :: FIRSTIME = .TRUE.

      IF ( FIRSTIME ) THEN
          FIRSTIME = .FALSE.
          ALLOCATE( CHEM_M2DRY_INIT( NCOLS,NROWS,NLAYS,N_MODE ),
     &              CHEM_M3DRY_INIT( NCOLS,NROWS,NLAYS,N_MODE ) )

      END IF

C *** extract grid cell concentrations of aero species from CGRID
C     into aerospc_conc in aero_data module
C     also converts dry surface area to wet second moment
#ifdef sens
               CALL EXTRACT_AERO ( CONCVEC(:), .TRUE., SENGRID(NCOLS,NROWS,NLAYS,:,:), .FALSE. )
#else
               CALL EXTRACT_AERO ( CONCVEC(:), .TRUE. )
#endif
 
C *** extract in inorganic aerosol processors

#ifdef sens
               CALL EXTRACT_PRECURSOR( CONCVEC(:), SENGRID( NCOLS,NROWS,NLAYS,:,: ) )
#else
               CALL EXTRACT_PRECURSOR( CONCVEC(:) ) 
#endif

C *** Update geometric mean diameters, geometric
C     standard deviations, modal mass totals, and modal particle
C     densities, based on the concentrations of M2, M0, and speciated
C     masses.
               CALL getpar( FIXED_sg )
     
C *** set up variables needed for calculating KN2O5 and YIELD_CLNO2

               DO N = 1, N_MODE
C *** estimate the "wet third moments" from moment3_conc
C     Note: this is the H2O concentration from previous time step
                  WET_M3( N ) = REAL( MOMENT3_CONC( N ), 8 )
C *** calculate "wet second moment" assuming that H2O does not
C     affect the geometric standard deviation
                  WET_M2( N ) = REAL( MOMENT2_CONC( N ), 8 ) 
C *** The "wet" geometric mean (same as median) diameter was updated in
C     getpar. It is stored in aeromode_diam
C *** calculate effective diameter (this is actually the mean) using Eq 3 of Pleim et al (1995)
                  DE_WET( N ) = REAL( AEROMODE_DIAM( N ) * EXP( 1.5 * AEROMODE_LNSG( N ) ** 2 ), 8)
               END DO

C *** Retrieve and Save the Dry 3rd and 2nd Moments
               CALL calcmoments( .False. )
               CHEM_M2DRY_INIT( C,R,L,: ) = REAL( MOMENT2_CONC( : ), 8 )
               CHEM_M3DRY_INIT( C,R,L,: ) = REAL( MOMENT3_CONC( : ), 8 )

      END SUBROUTINE HETCHEM_EXTRACT_AERO

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

      SUBROUTINE HETCHEM_UPDATE_AERO ( CGRID )

c  Calculates the change in aerosol surface area due to heterogeneous
C  reactions and updates the CGRID array with this information.
C  
C  History
C  May 2016: BM Created

C  Key Subroutines Called: none
C
C  Called by: Gas-Phase Chemistry Driver
      USE AERO_DATA
      USE CONST, ONLY: pi
      USE GRID_CONF, ONLY: NLAYS, NROWS, NCOLS

      IMPLICIT NONE
      
      REAL, POINTER   :: CGRID ( :,:,:,: )  ! pointer to model concentrations
      REAL, PARAMETER :: TWOTHIRDS = 2.0 / 3.0
      REAL            :: M2DRY_FINAL( N_MODE ) ! Dry 2nd Aerosol Moment
      REAL            :: M3DRY_FINAL( N_MODE ) ! Dry 3rd Aerosol Moment
      INTEGER L, R, C, M 

      LOOP_LAY: DO L = 1, NLAYS
         LOOP_ROW: DO R = 1, NROWS
            LOOP_COL: DO C = 1, NCOLS   
      
               ! Extract grid cell concentrations of aero species from CGRID
               ! into aerospc_conc in aero_data module
               ! also converts dry surface area to wet second moment
#ifdef sens
               CALL EXTRACT_AERO ( CGRID( C,R,L,: ), .TRUE., CGRID(C,R,:,: ), .FALSE. )
#else
               CALL extract_aero ( CGRID( C,R,L,: ), .True. )
#endif

               ! Recalculate Dry 3rd Moment After Chemistry Processes
               ! 2nd moment hasnt been update yet.
               CALL calcmoments( .False. )
               M2DRY_FINAL( : ) = MOMENT2_CONC( : )
               M3DRY_FINAL( : ) = MOMENT3_CONC( : )

               ! Calculate new Second Moment Manually
               ! Assume standard deviation is fixed.
               M2DRY_FINAL( : ) = REAL( CHEM_M2DRY_INIT( C,R,L,: ) ) 
     &             * ( M3DRY_FINAL( : ) 
     &             / REAL( CHEM_M3DRY_INIT( C,R,L,: ) ) ) ** TWOTHIRDS

               ! Update the Aerosol Surface Area making sure to multiply
               ! the 2nd moment by Pi to convert properly
               DO M = 1, N_MODE
                 CGRID( C,R,L, aerosrf_map( M ) ) = 
     &                        PI * M2DRY_FINAL( M )
               END DO

            END DO LOOP_COL
         END DO LOOP_ROW
      END DO LOOP_LAY

      END SUBROUTINE HETCHEM_UPDATE_AERO
 
