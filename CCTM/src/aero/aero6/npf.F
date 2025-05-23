
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

      SUBROUTINE NEWPART3 ( RH, T, XH2SO4, SO4RATE, DNDT, DMDT_so4, DM2DT )

      USE AERO_DATA
      USE CONST
      USE PRECURSOR_DATA, ONLY: PRECURSOR_MW, SULF_IDX
      USE UTILIO_DEFN
      
      IMPLICIT NONE

C   REVISION HISTORY:
C     Replacement of Kulmala et al., 1998 nucleation scheme with scheme 
C     of Vehkamaki et al. (2002) by G. Sarwar and K. Fahey - 03/2014

C.. References:
C     Vehkamaki, H., Kulmala, M, Napari, I., Lehtinen, K.E.J., Timmreck, C.,
C     Noppel, M., and A. Laaksonen. (2002) An improved parameterization for 
C     sulfuric acid-water nucleation rates for tropospheric and stratospheric
C     conditions.  JGR, v107(D22).    
   
C..Inputs: 
      REAL, INTENT(IN ) :: RH              ! fractional relative humidity      
      REAL, INTENT(IN)  :: T               ! Ambient temperature [ K ]
      REAL, INTENT(IN)  :: XH2SO4          ! sulfuric acid concentration [ ug/m**3 ]
      REAL, INTENT(IN)  :: SO4RATE         ! gas-phase H2SO4 production rate [ ug/m**3 s ]

C.. Outputs:
      REAL( 8 ), INTENT( OUT ) :: DNDT     ! particle number production rate [ m^-3/s ]
      REAL( 8 ), INTENT( OUT ) :: DMDT_so4 ! SO4 mass production rate [ ug/m**3 s ]
      REAL( 8 ), INTENT( OUT ) :: DM2DT    ! second moment production rate [ m**2/m**3 s ]

C.. Parameters
      CHARACTER( 16 ), PARAMETER :: PNAME = 'NEWPART'                 

C.. Particle size parameters:
      REAL, PARAMETER :: d20 = 2.0E-07                ! diameter of a new particle [cm]
      REAL, PARAMETER :: d20sq = d20 * d20            ! new-particle diameter squared [cm**2]
      REAL, PARAMETER :: m2_20 = 1.0E-4 * d20sq       ! new-particle diameter squared [m**2]
      REAL, PARAMETER :: v20 = PI * d20 * d20sq /6.0  ! volume of a new particle [cm**3]
      
      REAL( 8 )       :: sulfmass                     ! mass of a new particle [ug]
      REAL( 8 )       :: sulfmass1                    ! inverse of sulfmass [ug**-1]

C.. Set constants and local variables for Vehkamaki et al. (2002) scheme
      REAL, PARAMETER :: C1 = 0.740997
      REAL, PARAMETER :: C2 = -0.00266379
      REAL, PARAMETER :: C3 = -0.00349998
      REAL, PARAMETER :: C4 = 0.0000504022
      REAL, PARAMETER :: C5 = 0.00201048
      REAL, PARAMETER :: C6 = -0.000183289
      REAL, PARAMETER :: C7 = 0.00157407
      REAL, PARAMETER :: C8 = -0.0000179059
      REAL, PARAMETER :: C9 = 0.000184403
      REAL, PARAMETER :: C10 = -1.50345E-6

      REAL :: XSTAR           ! mole fraction of sulfuric acid in the critical cluster
      REAL :: NA              ! total gas phase concentration of H2SO4  [ #/cm**3 ]
      REAL :: TEMP            ! ambient temperature
      REAL :: LNRH, LNNA      ! LN(RH), LN(Na)
      REAL :: LNRH2, LNNA2    ! LN(RH)**2, LN(Na)**2
      REAL :: LNRH3, LNNA3    ! LN(RH)**3, LN(Na)**3  
      REAL :: TEMP2, TEMP3    ! TEMP**2, TEMP**3
      REAL( 8 ) :: XFAC       ! exponential term for the nucleation rate  
      REAL( 8 ) :: Jnuc       ! nucleation rate [ #/cm**3 s ]
      
      REAL :: A, B, C, D, E, F, G, H, I, J

      REAL :: MW_H2SO4        ! MW of H2SO4 in [ g / mole ]        
      REAL :: DENSITY_H2SO4   ! DENSITY of H2SO4 in [ kg / m**3 ]

      REAL, PARAMETER :: SCALEFAC = 1.0E-06           ! for [ 1 / m**3 ] ->  [ 1 / cm**3 ]
      REAL, PARAMETER :: MUG2G = 1.0E-6               ! [ ug  ] -> [ g ]                 

C.. Initialize variables
      DNDT     = 0.0D0
      DMDT_so4 = 0.0D0
      DM2DT    = 0.0D0

C.. Calculate molecular weight of H2SO4 [ g / mole ] 
      MW_H2SO4 = REAL( PRECURSOR_MW( SULF_IDX ),4 )
         
C.. Calculate density of sulfuric acid [ kg / m**3 ] 
      DENSITY_H2SO4 = AEROSPC( ASO4_IDX )%DENSITY           

C.. Calculate mass of a new particle [ug]
      sulfmass = 1.0D+3 * DENSITY_H2SO4 * v20

C.. Calculate inverse of sulfmass [ug**-1]      
      sulfmass1 = 1.0D0 / sulfmass   
     
C.. Calculate sulfuric acid concentration in molecules/cm3 
      NA = XH2SO4 * MUG2G * AVO * SCALEFAC / MW_H2SO4     

C.. The parameterization is valid at sulfuric acid concentrations of 1.0E4 - 1.0E11 molecules cm-3
      NA  = MAX (NA, 1.0E4)
      NA  = MIN (1.0E11, NA)

C.. The parameterization is valid at temperatures of 190.00-305.15 K
      TEMP = MAX (T, 190.00)
      TEMP = MIN (305.15, TEMP)

C.. The parameterization is valid at RH of 0.0001-1.0
C   aero_driver.f limits RH to 0.005-0.99; thus no additional constraint is needed

C.. Define convenient constants
      TEMP2 = TEMP * TEMP
      TEMP3 = TEMP * TEMP2
      
      LNRH = LOG( RH )  
      LNNA = LOG( NA )
      LNRH2 = LNRH * LNRH
      LNRH3 = LNRH * LNRH2
      LNNA2 = LNNA * LNNA
      LNNA3 = LNNA * LNNA2

C.. Calculate mole fraction of sulfuric acid in the critical cluster
      XSTAR = C1 + C2 * TEMP + C3 * LNNA + C4 * TEMP * LNNA + 
     &      C5 * LNRH + C6 * TEMP * LNRH +
     &      C7 * LNRH2 + C8 * TEMP * LNRH2 + C9 * LNRH3 + 
     &      C10 * TEMP * LNRH3

C.. Calculate coefficients needed for the nucleation rate [Eq-12 - Vehkamaki et al., 2002]           
      A = 0.14309 + 2.21956 * TEMP - 0.0273911 * TEMP2 + 
     &      0.0000722811 * TEMP3 + 5.91822 / XSTAR
     
      B = 0.117489 + 0.462532 * TEMP - 0.0118059 * TEMP2 + 
     &      0.0000404196 * TEMP3 + 15.7963 / XSTAR
     
      C = -0.215554 - 0.0810269 * TEMP + 0.00143581 * TEMP2 - 
     &      4.7758E-6 * TEMP3 - 2.91297 / XSTAR
     
      D = -3.58856 + 0.049508 * TEMP - 0.00021382 * TEMP2 + 
     &      3.10801E-7 * TEMP3 - 0.0293333 / XSTAR
     
      E = 1.14598 - 0.600796 * TEMP + 0.00864245 * TEMP2 - 
     &      0.0000228947 * TEMP3 - 8.44985 / XSTAR
     
      F = 2.15855 + 0.0808121 * TEMP - 0.000407382 * TEMP2 - 
     &      4.01957E-7 * TEMP3 + 0.721326 / XSTAR

      G = 1.6241 - 0.0160106 * TEMP + 0.0000377124 * TEMP2 + 
     &      3.21794E-8 * TEMP3 - 0.0113255 / XSTAR
      
      H = 9.71682 - 0.115048 * TEMP + 0.000157098 * TEMP2 + 
     &      4.00914E-7 * TEMP3 + 0.71186 / XSTAR
     
      I = -1.05611 + 0.00903378 * TEMP - 0.0000198417 * TEMP2 +
     &      2.46048E-8 * TEMP3 - 0.0579087 / XSTAR

      J = -0.148712 + 0.00283508 * TEMP - 9.24619E-6 * TEMP2 + 
     &      5.00427E-9 * TEMP3 - 0.0127081 / XSTAR

C.. Calculate the exponential term for the nucleation rate [Eq-12 - Vehkamaki et al., 2002] 
      XFAC = A + B * LNRH + C * LNRH2 + D * LNRH3 + 
     &      E * LNNA + F * LNRH * LNNA + G * LNRH2 * LNNA +     
     &      H * LNNA2 + I * LNRH * LNNA2 + J * LNNA3

C.. Calculate particle nucleation rate: unit [ 1 / cm**3 s] [Eq-12 - Vehkamaki et al., 2002] 
      Jnuc = EXP(XFAC)  
      
C.. The parameterization is valid for nucleation rates of 1.0E-7-1.0E10 [ 1 / cm**3 s]
      Jnuc = MAX (Jnuc, 1.0D-7)
      Jnuc = MIN (1.0D10, Jnuc)

C.. Convert the unit of particle nucleation rate into [ 1 / m**3 s] by multiplying it by 1.0E6  
       DNDT = Jnuc * 1.0E06  ! (1/(m**3 s))

C.. Calculate mass production rate [ ug / (m**3 s) ] analogous to
C   Equation 6a of Binkowski & Roselle (2003). Set the upper limit
C   of the mass production rate as the gas-phase production rate of
C   H2SO4, and adjust the number production rate accordingly.
      DMDT_so4 = sulfmass * DNDT 

      IF ( DMDT_so4 .GT. SO4RATE ) THEN
         DMDT_so4 = SO4RATE
         DNDT = DMDT_SO4 * sulfmass1
      END IF

C.. Calculate the production rate of 2nd moment [ m**2 / (m**3 s) ]
C   This is similar to Equation 6c of Binkowski & Roselle (2003),
C   except the factor of PI is removed and the assumed particle
C   diameter is different.
      DM2DT = DNDT * m2_20

      RETURN    
      END SUBROUTINE NEWPART3 

