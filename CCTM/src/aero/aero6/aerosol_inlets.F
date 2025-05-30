
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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE AERO_INLET ( DGN, XXLSG, RHOP, D_AD1, D_AD2, fPM )

C  Calculates the volume fraction of a given aerosol mode that would enter
C  a sharp-cut inlet, using equations from Jiang et al (2006). The
C  parameters D_AD1 and D_AD2 provide the lower and upper cutoff diameters
C  for the inlet size cut and the routine returns the fraction of the
C  size distribution that is transmitted through the hypothetical inlet
C  with the variable fPM. But first, those cutoffs are converted from 
C  Aerodynamic Diameter to Stokes Diameter (used internally within CMAQ).
 
C  CMAQ aerosols are represented with Stokes Diameter. There are many
C  alternate forms of diameter including the following:
C       Volume Equivalent Diameter (D_ve) - the diameter of a sphere with the
C          same volume as the particle.
C       Stokes Diameter (D_st) - the diameter of a sphere with the same
C          terminal velocity as the real particle. If the shape factor
C          is 1, then the Stokes and Volume Equivalent Diameters are
C          equal.
C       Aerodynamic Diameter (D_ad) - the diameter of a sphere with unit
C          density and the same terminal velocity as the real particle.
C          If the density is 1, then the Aerodynamic Diameter and Stokes
C          Diameter are equal.
C       Vacuum Aerodynamic Diameter (D_va) - the diameter of a sphere with very
C          high Knudsen number and thus a simplified relationship with
C          Stokes diameter that is only dependent on density and shape
C          factor, not Slip Correction. The very high Knudsen number
C          could be because the particle is very small or because the
C          system pressure is very low.
C       Electrical Mobility Diameter (D_em) - the diameter of a sphere with the
C          same migration velocity as the real particle. Requires a
C          slip-correction and a shape factor correction when converting
C          from volume equivalent diameter.
 
C  Conversions:
C       Parameters
C         rho  - Density
C         rho0 - Unit Density (1 g cm-3)
C         Cc( ) - Cunningham Slip Correction of a particular diameter
C         X   - Shape factor = (NonSpherical Drag Force)/(Spherical Drag Force)
C       Aerodynamic -> Stokes
C         D_st = D_ad * sqrt(rho0/rho) * sqrt( Cc(D_ad) / Cc(D_st) )
C       Volume Equivalent -> Stokes
C         D_st = D_ve * sqrt(1/X) * sqrt( Cc(D_ve) / Cc(D_st) )
C       Electrical Mobility -> Volume Equivalent
C         D_ve = D_em * [Cc(D_ve) / Cc(D_em)] * 1/X
C             (If X = 1 (i.e. spherical), then D_st = D_ve = D_em )
C       Vacuum Aerodynamic -> Volume Equivalent (DeCarlo et al., 2004)
C         D_ve = D_va * rho0/rho * X
C             (If X = 1 (i.e. spherical), then D_st = D_ve = D_va * rho0/rho )
 
C  Key Subroutines called: none
 
C  Revision History:
C    Coded Jul 2005 by Prakash Bhave
C          Apr 2008 J.Kelly: corrected equation for Dst25 calculation
C          Feb 2016 B.Murphy: modified to output PM1 and PM10 mode
C                             fractions in addition to PM2.5
 
C  References:
C   1. Jiang, W., Smyth, S., Giroux, E., Roth, H., Yin, D., Differences
C   between CMAQ fine mode particle and PM2.5 concentrations and their
C   impact on model performance evaluation in the Lower Fraser Valley,
C   Atmos. Environ., 40:4973-4985, 2006.
C   2. Meng, Z., Seinfeld, J.H., On the source of the submicrometer
C   droplet mode of urban and regional aerosols, Aerosol Sci. and
C   Technology, 20:253-265, 1994.
 
C-----------------------------------------------------------------------

      USE CONST
      IMPLICIT NONE

C    Input variables
      REAL, INTENT( IN ) :: DGN     ! geometric mean Stokes diameter by number [ m ]
      REAL, INTENT( IN ) :: XXLSG   ! natural log of geometric standard deviation
      REAL, INTENT( IN ) :: RHOP    ! average particle density [ kg/m**3 ]
      REAL, INTENT( IN ) :: D_AD1   ! Lower aerodynamic diameter cut point [ um ]
      REAL, INTENT( IN ) :: D_AD2   ! Upper aerodynamic diameter cut point [ um ]

C    Output variable
      REAL, INTENT( OUT ) :: fPM   ! fraction of particulate volume transmitted through 
                                   ! hypothetical inlet.

C    Internal variables
      REAL, PARAMETER :: SQRT2 = 1.4142136  !  SQRT( 2 )
      REAL, PARAMETER :: B = 0.21470 ! Cunningham slip-correction approx. param [ um ]
                                     ! This factor works well applied to the entire particle size-range
                                     ! The approximation is: Cc(Dp) = 1 + B/Dp
      REAL D_ST1, D_ST2              ! Stokes diameter equivalent of DCA..
      REAL DG                        ! DGN converted to [ um ]
      REAL ERF1, ERF2, ERFARG        ! argument of ERF, from Step#6 of Jiang et al. (2006)

C ----------------------- Begin solution -------------------------------

C *** Calculate Transmission Fractions for Inlets with Aerodynamic
C     Cutoffs
      DG = DGN  ! [um] The units need to be equivalent with the B parameter

      ! from Meng & Seinfeld (1994) and modified form of Fk(X) equation
      ! in Step#6 of Jiang et al. (2006).
      IF ( D_AD1 .EQ. 0.0 ) THEN
          ERF1 = -1.0
      ELSE
          ! Convert size cut to equivalent Stokes diameter using
          ! equation 2 of Jiang et al. (2006). Note: the equation in Step 5
          ! of this paper has a typo (i.e., eq. 2 is correct).
          D_ST1  = 0.5 * ( SQRT( B ** 2 + 4.0 * D_AD1 *
     &                        ( D_AD1 + B ) * 1.0E+03 / RHOP ) - B )
          ! Calculate mass fraction with Dca < SizeCut
          ERFARG = ( LOG( D_ST1 ) - LOG( DG ) ) / ( SQRT2 * XXLSG ) - 3.0 * XXLSG / SQRT2
          ERF1 = ERF( ERFARG )
      END IF

      ! Convert size cut to equivalent Stokes diameter using
      ! equation 2 of Jiang et al. (2006). Note: the equation in Step 5
      ! of this paper has a typo (i.e., eq. 2 is correct).
      D_ST2  = 0.5 * ( SQRT( B ** 2 + 4.0 * D_AD2 *
     &                     ( D_AD2 + B ) * 1.0E+03 / RHOP ) - B )

      ! Calculate mass fraction with Dca < SizeCut
      ERFARG = ( LOG( D_ST2 ) - LOG( DG ) ) / ( SQRT2 * XXLSG ) - 3.0 * XXLSG / SQRT2
      ERF2 = ERF( ERFARG )

      ! Calculate fraction between Lower and Upper Cut Points
      fPM = 0.5 * ( ERF2 - ERF1 )

      END SUBROUTINE AERO_INLET

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE AERO_AMS ( M3_WET, M2_WET, M0, M_H2O, RHOP, RHO_H2O, fAMS )

C  The subroutine calculates transmission factors applicable for
C  comparison with AMS measurements using the curve suggested by Jimenez
C  and coworkers  (http://cires1.colorado.edu/jimenez-group/wiki/index.php/
C  FAQ_for_AMS_Data_Users#What_is_the_size_cut_of_AMS_measurements.3F).
C  The AMS measures Vacuum Aerodynamic Diameter (D_va) because the pressure is
C  so low in the instrument. This curve (also described in Ensberg et
C  al., 2013)  includes the following (Note: before applying the curve,
C  the bounds must be converted to Stokes Diameter):
C     D_va < 0.04 um: 0% transmission (T)
C     0.04 < D_va < 0.1 um:  T% = 1666.6 * (D_va - 0.04)   (1)
C     0.1  < D_va < 0.55 um: T% = 100%                     (2)
C     0.55 < D_va < 2.0 um:  T% = 100-68.965*(D_va-0.55)   (3)
C     2.0 um < D_va: T = 0%
 
C  CMAQ aerosols are represented with Stokes Diameter. There are many
C  alternate froms of diameter including the following:
C       Volume Equivalent Diameter (D_ve) - the diameter of a sphere with the
C          same volume as the particle.
C       Stokes Diameter (D_st) - the diameter of a sphere with the same
C          terminal velocity as the real particle. If the shape factor
C          is 1, then the Stokes and Volume Equivalent Diameters are
C          equal.
C       Vacuum Aerodynamic Diameter (D_va) - the diameter of a sphere with very
C          high Knudsen number and thus a simplified relationship with
C          Stokes diameter that is only dependent on density and shape
C          factor, not Slip Correction. The very high Knudsen number
C          could be because the particle is very small or because the
C          system pressure is very low.
 
C  Conversions:
C       Parameters
C         ro  - Density
C         ro0 - Unit Density (1 g cm-3)
C         Cc( ) - Cunningham Slip Correction of a particular diameter
C         X   - Shape factor = (NonSpherical Drag Force)/(Spherical Drag Force)
C       Vacuum Aerodynamic -> Volume Equivalent (DeCarlo et al., 2004)
C         D_ve = D_va * ro0/ro * X
C             (If X = 1 (i.e. spherical), then D_st = D_ve = D_va * ro0/ro )
 
C  Revision History:
C    Coded Feb 2016 B.Murphy: Created

C  References:
C   1. DeCarlo et al., Particle Morphology and Density Characterization
C   by Combined Mobility and Aerodynamic Diameter Measurements. Part 1:
C   Theory, Aerosol Sci. and Technology, 38:1185-1205, 2004
C   2. Ensberg et al., Inorganic and black carbon aerosols in the Los
C   Angeles Basin during CalNex, Journ. Geophys. Res., 2013.

C-----------------------------------------------------------------------

      USE AERO_DATA, ONLY    : MIN_SIGMA_G, MAX_SIGMA_G
      USE CONST
!     USE AEROMET_DATA, ONLY : F6PI, PI6

      IMPLICIT NONE

C    Input variables
      REAL, INTENT( IN ) :: M3_WET  ! Third Moment of Wet Distribution (m3/m3)
      REAL, INTENT( IN ) :: M2_WET  ! Second Moment of Wet Distribution (m2/m3)
      REAL, INTENT( IN ) :: M0      ! Number of Particles in Distribution (N/m3)
      REAL, INTENT( IN ) :: M_H2O   ! Mass Conc. of Water in Particles (ug/m3)
      REAL, INTENT( IN ) :: RHOP    ! average particle density [ kg/m**3 ]
      REAL, INTENT( IN ) :: RHO_H2O ! Water Density [ kg/m3 ]

C    Output variable
      REAL, INTENT(OUT) :: fAMS   ! fraction of particulate volume transmitted through AMS Inlet

C    Internal variables
      REAL, PARAMETER :: SQRT2 = 1.4142136    !  SQRT( 2 )
      REAL, PARAMETER :: DGMIN = 1.0E-9       !  min(Dp) in [m]
      REAL, PARAMETER :: ONETHIRD = 1.0 / 3.0
      REAL, PARAMETER :: TWOTHIRDS = 2.0 * ONETHIRD

      REAL DG                        ! DGN converted to [ um ]
      REAL DGv                       ! Volume Median Diameter
      REAL XXLSG                     ! ln(StndDev) for current mode
      REAL M3_DRY, M2_DRY, M3SUBT, M_WET, M_DRY, DRY_DENS, DENSFAC
      REAL XFSUM, LXFM2, L2SG, ES36
      REAL DBlo_st, DBhi_st, LOG_HILO, LOG_LOHI, LOG_HI, LOG_LO
      REAL SQRT2LSG, TERM1, TERM2, TERM3, TERM4, TERM5

C ----------------------- Begin solution -------------------------------

C *** First Calculate Parameters of Dry Distribution since this is more
C     applicable in general to AMS measurements.
      M3SUBT = ( 1.0E-9 * F6PI / RHO_H2O ) * M_H2O        ! m3 m-3
      M3_DRY = Max(M3_WET - M3SUBT, 0.0) + TINY(0.0)   ! m3 m-3 
      M2_DRY = M2_WET * ( M3_DRY / M3_WET ) ** TWOTHIRDS  ! m2 m-3

      M_WET = M3_WET * 1.0E+9 * PI6 * RHOP ! ug m-3
      M_DRY = Max(M_WET - M_H2O, 0.0) + TINY(0.0) ! m3 m-3 

      DRY_DENS = M_DRY / M3_DRY * F6PI * 1.0E-9   ! kg m-3

      XFSUM = ONETHIRD * Log( M0 ) + TWOTHIRDS * Log( M3_DRY )
      LXFM2 = Log( M2_DRY )
      L2SG = XFSUM - LXFM2   ! ( ln(sigma) )^2

      L2SG = Min( Max( L2SG, LOG( MIN_SIGMA_G ) ** 2 ), LOG( MAX_SIGMA_G ) ** 2 )
      LXFM2 = XFSUM - L2SG
      ES36 = Exp( 4.5 * L2SG )

      DG = Max( DGMIN, ( M3_DRY / ( M0 * ES36 ) ) ** ONETHIRD ) * 1.0E+06 ![um] Units should correspond to D
      XXLSG = Sqrt( L2SG )  ! ln(sigma)

      DGv = EXP( LOG( DG ) + 3.0 * L2SG )

C *** Calculate Transmission Fractions for AMS with Vacuum Aerodynamic
C     Cutoffs. This approximation is split into a piecewise function (see
C     Appendix B in Ensberg et al., 2013).
      fAMS = 0.0
      DENSFAC = 1.0E+03 / DRY_DENS  ! Density Correction Factor
                                    !   = rho0 / rho
                                    !   = 1000.0 / rho
      SQRT2LSG = SQRT2 * XXLSG

      ! First Piece of Function [T(%) = 1666.6 * (Dva - 0.04) ]
      DBlo_st = 0.040 * DENSFAC  ! Stokes Lower Bound of Piece [um]
      DBhi_st = 0.100 * DENSFAC  ! Stokes Upper Bound of Piece [um]

      LOG_HILO = LOG( 100.0/40.0 )
      LOG_HI   = LOG( DBhi_st/DGv )
      LOG_LO   = LOG( DBlo_st/DGv )

      TERM1 = LOG( DGv/DBlo_st ) / LOG_HILO
      TERM2 =  ERF( LOG_HI / SQRT2LSG )
     &        -ERF( LOG_LO / SQRT2LSG )
      TERM3 = XXLSG / ( LOG_HILO * (pi/2.0) ** 0.5 )
      TERM4 = EXP( -1.0 * ( LOG_LO / SQRT2LSG ) ** 2 )
      TERM5 = EXP( -1.0 * ( LOG_HI / SQRT2LSG ) ** 2 )

      fAMS = ( TERM1*TERM2  +  TERM3*(TERM4-TERM5) )

      ! Second Piece of Function [T(%) = 100]
      DBlo_st = DBhi_st         ! Stokes Lower Bound [um]
      DBhi_st = 0.55 * DENSFAC  ! Stokes Upper Bound [um]

      TERM1 = ERF( LOG( DBhi_st/DGv ) / SQRT2LSG )
      TERM2 = ERF( LOG( DBlo_st/DGv ) / SQRT2LSG )
      fAMS = fAMS +  (TERM1 - TERM2)

      ! Third Piece of Function [T(%) = 1.0 - 0.6805 * (Dva - 0.55) ]
      DBlo_st = DBhi_st        ! Stokes Lower Bound [um]
      DBhi_st = 2.0 * DENSFAC  ! Stokes Upper Bound [um]

      LOG_LOHI = LOG( 550.0/2000.0 )
      LOG_HI   = LOG( DBhi_st/DGv )
      LOG_LO   = LOG( DBlo_st/DGv )

      TERM1 = LOG( DGv/DBhi_st ) / LOG_LOHI
      TERM2 =  ERF( LOG_HI / SQRT2LSG )
     &        -ERF( LOG_LO / SQRT2LSG )
      TERM3 = XXLSG / ( LOG_LOHI * (pi/2.0) ** 0.5 )
      TERM4 = EXP( -1.0 * ( LOG_LO / SQRT2LSG ) ** 2 )
      TERM5 = EXP( -1.0 * ( LOG_HI / SQRT2LSG ) ** 2 )

      fAMS = fAMS + TERM1 * TERM2  +  TERM3 * (TERM4-TERM5)

      ! Apply the Factor of 0.5 Consistent with Appendix B in Ensberg et
      ! al., 2013. Omit the total mass quantity since these are fractions
      ! we want.
      fAMS = 0.5 * fAMS

      END SUBROUTINE AERO_AMS

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SUM_INLET_PM ( INLET, WETNESS, PM )

!     Calculates the total PM mass below a user-defined cut point D_AD,
!     or Aerodynamic Diameter in um.
!-----------------------------------------------------------------------

      USE AERO_DATA, ONLY : AEROSPC_CONC, AEROSPC, N_MODE, 
     &                      AH2O_IDX, AORGH2O_IDX
      USE ELMO_DATA

      IMPLICIT NONE

      ! Input variables
      INTEGER, INTENT( INOUT ) :: INLET ! Integer prescribing the inlet type
                                        !   (see ELMO_INLET table in ELMO_DATA module)
      INTEGER, INTENT( INOUT ) :: WETNESS ! 1 = Wet Aerosol; 2 = Dry Aerosol

      ! Output variable
      REAL, INTENT( OUT ) :: PM   ! PM Mass below cut point

      INTEGER IM
      REAL    VAL

C ----------------------- Begin solution -------------------------------
      PM = 0.0
      
      DO IM = 1,N_MODE
         CALL GET_AERO_INLET( INLET, IM, WETNESS, VAL )
         PM = PM + VAL * ( SUM( AEROSPC_CONC( :,IM ), 
     &                  MASK = .NOT.AEROSPC(:)%TRACER )
     &                  - AEROSPC_CONC( AH2O_IDX,IM ) )
                    IF ( AORGH2O_IDX .GT. 0 ) PM = PM
     &                  - VAL * AEROSPC_CONC( AORGH2O_IDX,IM ) ! ug m-3
      END DO
 
      END SUBROUTINE SUM_INLET_PM

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE GET_AERO_INLET ( INLET, IMODE, WETNESS, FRAC )

!     Calls the AERO_INLET routine for calculating the fraction of each 
!       mode collected by inlet prescribed by INLET. This subroutine is
!       only for PM0.1, PM1, PM2.5, PM10 and PM2.5to10. It is not for
!       AMS inlet calculation. If the user expands the ELMO_INLET
!       table to consider more cutoff ranges, this subroutine can be
!       used to calculate them as well.
!-----------------------------------------------------------------------

      USE ELMO_DATA
      USE AERO_DATA, ONLY : WET_AERO_DIAM, AEROMODE_LNSG, WET_AERO_DENS,
     &                      DRY_AERO_DIAM, DRY_AERO_DENS, AH2O_IDX,
     &                      MOMENT0_CONC, AEROSPC, AEROSPC_CONC,
     &                      WET_AERO_M3, WET_AERO_M2

      IMPLICIT NONE

      ! Input variables
      INTEGER, INTENT( INOUT ) :: INLET ! Integer prescribing the inlet type
                                        !   (see ELMO_INLET table in ELMO_DATA module)
      INTEGER, INTENT( INOUT ) ::  IMODE   ! Aerosol Mode of interest
      INTEGER, INTENT( INOUT ) ::  WETNESS ! 1 = Wet Aerosol; 2 = Dry Aerosol

      ! Output variable
      REAL, INTENT( OUT ) :: FRAC ! Fraction of aerosol mode IM
                                  !   collected by inlet described by INLET

C ----------------------- Begin solution -------------------------------
      
      IF ( INLET .EQ. 0 ) THEN
          FRAC = 1.0
          RETURN
      END IF

      IF ( INLET_FRAC( INLET,IMODE,WETNESS ) .LT. 0. ) THEN
         IF ( INLET .EQ. IPMAMS ) THEN
           CALL AERO_AMS( WET_AERO_M3( IMODE ), WET_AERO_M2( IMODE ),
     &                    MOMENT0_CONC( IMODE ), AEROSPC_CONC( AH2O_IDX,IMODE ),
     &                    WET_AERO_DENS( IMODE ), AEROSPC(AH2O_IDX)%DENSITY,
     &                    FRAC )

         ELSE IF ( WETNESS .EQ. IWET ) THEN
           CALL AERO_INLET( WET_AERO_DIAM( IMODE ), AEROMODE_LNSG( IMODE ),
     &                      WET_AERO_DENS( IMODE ), 
     &                      ELMO_INLET( INLET )%DIAM_LO, 
     &                      ELMO_INLET( INLET )%DIAM_HI, 
     &                      FRAC )
         ELSE
           CALL AERO_INLET( DRY_AERO_DIAM( IMODE ), AEROMODE_LNSG( IMODE ),
     &                      DRY_AERO_DENS( IMODE ), 
     &                      ELMO_INLET( INLET )%DIAM_LO, 
     &                      ELMO_INLET( INLET )%DIAM_HI, 
     &                      FRAC )
         END IF
         
         INLET_FRAC( INLET,IMODE,WETNESS ) = FRAC 
      ELSE
         FRAC = INLET_FRAC( INLET,IMODE,WETNESS )
      END IF
 
      END SUBROUTINE GET_AERO_INLET
 
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SUM_INLET_NUM ( INLET, WETNESS, NUM )

!     Calculates the total particle number within a user-defined range
!     for each aerosol mode and then sums them up.
!-----------------------------------------------------------------------

      USE AERO_DATA, ONLY : DRY_AERO_DIAM, WET_AERO_DIAM,
     &                      AEROMODE_LNSG, MOMENT0_CONC
      USE ELMO_DATA

      IMPLICIT NONE

      ! Input variables
      INTEGER, INTENT( INOUT ) :: INLET ! Integer prescribing the inlet type
                                        !   (see ELMO_INLET table in ELMO_DATA module)
      INTEGER, INTENT( INOUT ) ::  WETNESS ! 1 = Wet Aerosol; 2 = Dry Aerosol

      ! Output variable
      REAL, INTENT( OUT ) :: NUM   ! PM Mass below cut point

      INTEGER IM
      REAL    FRAC

C ----------------------- Begin solution -------------------------------
      NUM = 0.0
      
      DO IM = 1,N_MODE
         IF ( WETNESS .EQ. IWET ) THEN
           CALL NUM_INLET( WET_AERO_DIAM( IM ), AEROMODE_LNSG( IM ),
     &                      ELMO_INLET( INLET )%DIAM_LO, 
     &                      ELMO_INLET( INLET )%DIAM_HI, 
     &                      FRAC )
         ELSE
           CALL NUM_INLET( DRY_AERO_DIAM( IM ), AEROMODE_LNSG( IM ),
     &                      ELMO_INLET( INLET )%DIAM_LO, 
     &                      ELMO_INLET( INLET )%DIAM_HI, 
     &                      FRAC )
         END IF
 
         NUM = NUM + MOMENT0_CONC( IM ) * FRAC
      END DO
 
      END SUBROUTINE SUM_INLET_NUM

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE NUM_INLET ( DGN, XXLSG, D_ST1, D_ST2, fPM )

C  Calculates the number fraction of a given aerosol mode that is between
C  cutoff diamters D_ST1 and D_ST2. The Stokes diameter used by CMAQ is 
C  assumed to be equivalent to the electrical mobility diameter, reported 
C  by typical SMPS datasets. This is true for spherical particles.
C 
C  CMAQ aerosols are represented with Stokes Diameter. There are many
C  alternate forms of diameter including the following:
C       Volume Equivalent Diameter (D_ve) - the diameter of a sphere with the
C          same volume as the particle.
C       Stokes Diameter (D_st) - the diameter of a sphere with the same
C          terminal velocity as the real particle. If the shape factor
C          is 1, then the Stokes and Volume Equivalent Diameters are
C          equal.
C       Aerodynamic Diameter (D_ad) - the diameter of a sphere with unit
C          density and the same terminal velocity as the real particle.
C          If the density is 1, then the Aerodynamic Diameter and Stokes
C          Diameter are equal.
C       Vacuum Aerodynamic Diameter (D_va) - the diameter of a sphere with very
C          high Knudsen number and thus a simplified relationship with
C          Stokes diameter that is only dependent on density and shape
C          factor, not Slip Correction. The very high Knudsen number
C          could be because the particle is very small or because the
C          system pressure is very low.
C       Electrical Mobility Diameter (D_em) - the diameter of a sphere with the
C          same migration velocity as the real particle. Requires a
C          slip-correction and a shape factor correction when converting
C          from volume equivalent diameter.
 
C  Conversions:
C       Parameters
C         rho  - Density
C         rho0 - Unit Density (1 g cm-3)
C         Cc( ) - Cunningham Slip Correction of a particular diameter
C         X   - Shape factor = (NonSpherical Drag Force)/(Spherical Drag Force)
C       Aerodynamic -> Stokes
C         D_st = D_ad * sqrt(rho0/rho) * sqrt( Cc(D_ad) / Cc(D_st) )
C       Volume Equivalent -> Stokes
C         D_st = D_ve * sqrt(1/X) * sqrt( Cc(D_ve) / Cc(D_st) )
C       Electrical Mobility -> Volume Equivalent
C         D_ve = D_em * [Cc(D_ve) / Cc(D_em)] * 1/X
C             (If X = 1 (i.e. spherical), then D_st = D_ve = D_em )
C       Vacuum Aerodynamic -> Volume Equivalent (DeCarlo et al., 2004)
C         D_ve = D_va * rho0/rho * X
C             (If X = 1 (i.e. spherical), then D_st = D_ve = D_va * rho0/rho )
 
C  Key Subroutines called: none
 
C  Revision History:
C    Coded Jul 2005 by Prakash Bhave
C          Apr 2008 J.Kelly: corrected equation for Dst25 calculation
C          Feb 2016 B.Murphy: modified to output PM1 and PM10 mode
C                             fractions in addition to PM2.5
C          Feb 2020 B.Murphy: Created this number synthesizer
C 
C-----------------------------------------------------------------------

      USE CONST
      IMPLICIT NONE

C    Input variables
      REAL, INTENT( IN ) :: DGN     ! geometric mean Stokes diameter by NUMBER [ m ]
                                    !  Remeber that geometric mean and median are
                                    !  identical for log-normal distributions.
      REAL, INTENT( IN ) :: XXLSG   ! natural log of geometric standard deviation
      REAL, INTENT( IN ) :: D_ST1   ! Lower stokes/electricla mobility diameter cut point [ um ]
      REAL, INTENT( IN ) :: D_ST2   ! Upper stokes/electrical mobility diameter cut point [ um ]

C    Output variable
      REAL, INTENT( OUT ) :: fPM   ! fraction of particulate number transmitted 
                                   ! through size window between D_ST1 and D_ST2

C    Internal variables
      REAL, PARAMETER :: SQRT2 = 1.4142136  !  SQRT( 2 )
      REAL ERF1, ERF2 ! argument of ERF, from Step#6 of Jiang et al. (2006)
      REAL ERFARG
      REAL DG         ! Internal units for median diameter

C ----------------------- Begin solution -------------------------------

C *** Calculate Transmission Fractions for Inlets with Aerodynamic
C     Cutoffs
      DG = DGN  ! [um] The units need to be equivalent with the B parameter

      ! from Meng & Seinfeld (1994) and modified form of Fk(X) equation
      ! in Step#6 of Jiang et al. (2006).
      IF ( D_ST1 .EQ. 0.0 ) THEN
          ERF1 = -1.0
      ELSE
          ! Calculate mass fraction with Dca < SizeCut
          ERFARG = ( LOG( D_ST1 ) - LOG( DG ) ) / ( SQRT2 * XXLSG ) 
          ERF1 = ERF( ERFARG )
      END IF

      ! Calculate mass fraction with Dca < SizeCut
      ERFARG = ( LOG( D_ST2 ) - LOG( DG ) ) / ( SQRT2 * XXLSG ) 
      ERF2 = ERF( ERFARG )

      ! Calculate fraction between Lower and Upper Cut Points
      fPM = 0.5 * ( ERF2 - ERF1 )

      END SUBROUTINE NUM_INLET
 
